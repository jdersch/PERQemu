//
// PERQSystem.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
// See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Threading;

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.Processor;
using PERQemu.Memory;
using PERQemu.IO;
using PERQemu.UI;

namespace PERQemu
{

    /// <summary>
    /// PERQSystem encapsulates basic operation of a complete virtual machine.
    /// It builds the configuration as described by the user in a Configuration
    /// object and manages the creation of a thread to run the PERQ.  It starts
    /// up the SDL Display when the system is "powered on" and accepts commands
    /// from the Execution Controller to run, stop, single step, or shutdown.
    /// </summary>
    public sealed class PERQSystem
    {
        public PERQSystem(Configuration conf)
        {
            //
            // Everything we need to know to build the virtual PERQ
            //
            _conf = conf;

            _userInterrupt = false;

            //
            // TODO: There are some ugly dependencies here...
            //      CPU needs Memory;
            //      Scheduler "needs" CPU;
            //      Display, Video need Memory, CPU, and (eventually IOB);
            //      IOBus needs IOB, OIO;
            //      new Z80 needs IOB?
            //

            // Memory board configures itself and the VideoController
            _mem = new MemoryBoard(this);

            // Instantiate the CPU board
            _cpu = new CPUBoard(this);

            // Fire up the IO board (which will set up the Z80).  Once the
            // board type is selected, load the appropriate CPU boot ROMs!
            switch (_conf.IOBoard)
            {
                case IOBoardType.IOB:
                    _iob = new IOB(this);
                    _cpu.LoadBootROM("boot.bin");      // 4K or 16K, old Z80
                    break;

                case IOBoardType.CIO:
                    _iob = new CIO(this);
                    _cpu.LoadBootROM("cioboot.bin");   // 4K or 16K, new Z80
                    break;

                case IOBoardType.EIO:
                case IOBoardType.NIO:
                    //  _iob = new EIO(this);
                    //  if (_conf.CPU == CPUType.PERQ24)
                    //  {
                    //    _cpu.LoadROM("eio24boot.bin");    // 16K 24-bit, new Z80
                    //  }
                    //  else
                    //  {
                    //    _cpu.LoadROM("eioboot.bin");      // 16K, new Z80
                    //  }
                    //  break;
                    throw new UnimplementedHardwareException(
                        string.Format("IO board type {0} is not implemented.", _conf.IOBoard));

                default:
                    throw new InvalidConfigurationException(
                        string.Format("No such IO board type '{0}'", _conf.IOBoard));
            }

            // Assume async mode if the implementation supports it.
            // Might want to select sync mode on uniprocessor systems?  What is
            // a "uniprocessor"?  Is that like a "land line" or a "glacier"?
            _mode = (_iob.SupportsAsync && _cpu.SupportsAsync) ? ExecutionMode.Asynchronous : ExecutionMode.Synchronous;

            // If any IO options are defined, instantiate the board
            // which will contain them (and set them up)
            switch (conf.IOOptionBoard)
            {
                case OptionBoardType.None:
                    _oio = null;
                    break;

                case OptionBoardType.OIO:
                    _oio = new OIO(this);
                    break;

                case OptionBoardType.Ether3:
                case OptionBoardType.MLO:
                    throw new UnimplementedHardwareException(
                        string.Format("IO Option board type {0} is not implemented.", _conf.IOOptionBoard));

                default:
                    throw new InvalidConfigurationException(
                        string.Format("No such IO Option board type '{0}'", _conf.IOOptionBoard));
            }

            // The Display will initialize itself (lazily)
            _display = new Display(this);

            // Set up the IO Bus and check in the boards
            _ioBus = new IOBus(this);

            // Set our initial state; instantiated but not yet initialized...
            _state = RunState.Off;

            // Compute how many cycles between runs of the GUI's event loop (in
            // this case, the SDL2 message loop).  This is gross.
            _uiEventInterval = (uint)((16.6667 * Conversion.MsecToNsec) / CPU.MicroCycleTime);
        }

        public Configuration Config => _conf;

        public CPU CPU => _cpu.Processor;
        public Scheduler Scheduler => _cpu.Scheduler;

        public MemoryBoard Memory => _mem;
        public VideoController VideoController => _mem.Video;
        public Display Display => _display;

        public IOBoard IOB => _iob;
        public OptionBoard OIO => _oio;
        public IOBus IOBus => _ioBus;


        public ExecutionMode Mode
        {
            get { return _mode; }
            set { _mode = value; }
        }

        public RunState State
        {
            get { return _state; }
            set { _state = value; }
        }

        /// <summary>
        /// User- or emulator-initiated pause in execution.
        /// </summary>
        public void Break()
        {
            _userInterrupt = true;
            _state = RunState.Paused;
        }

        /// <summary>
        /// Transition to and run the PERQ in a given state, in the current
        /// execution mode.
        /// </summary>
        public void Run(RunState s)
        {
            bool running = true;

            _state = s;

            while (running)
            {
                Trace.Log(LogType.EmuState, "[PERQsystem {0} in {1} mode]", _state, _mode);
                Console.WriteLine("[PERQsystem {0} in {1} mode]", _state, _mode);

                switch (_state)
                {
                    case RunState.WarmingUp:
                        _display.InitializeSDL();
                        _state = RunState.Reset;
                        break;

                    case RunState.Running:
                        if (_mode == ExecutionMode.Asynchronous)
                        {
                            // Start up the background threads
                            _iob.RunAsync();
                            _cpu.RunAsync();

                            // Run the event loop until State changes
                            _display.SDLMessageLoop();

                            // Stop the threads and loop for State transition
                            _cpu.Stop();
                            _iob.Stop();
                        }
                        else
                        {
                            // Run the PERQ CPU and Z80 CPU in lockstep until manually stopped
                            RunGuarded(() =>
                                {
                                    uint count = 0;

                                    while (_state == RunState.Running)
                                    {
                                        // Run the IOB for one Z80 instruction, then run the PERQ CPU for
                                        // the number of microinstructions equivalent to that wall-clock time.
                                        var clocks = _iob.Clock();

                                        // Calculate the fudge factor
                                        clocks = (uint)(clocks * (IOBoard.Z80CycleTime / CPU.MicroCycleTime));
                                        count += clocks;

                                        // Run the main CPU.  Rate limiting?  Hmm.
                                        _cpu.Run((int)clocks);

                                        // Run the SDL loop.  There has to be a better way.
                                        if (count > _uiEventInterval)
                                        {
                                            _display.SDLMessageLoop();
                                            count = 0;
                                        }
                                    }
                                });
                        }
                        break;

                    case RunState.SingleStep:
                    case RunState.RunZ80Inst:
                        // For now:   Run the IOB for one Z80 instruction, then
                        // run the PERQ CPU for one instruction.  Timing-wise
                        // this is very inaccurate.  It would be nice to allow
                        // single-stepping either processor and have the timings
                        // be more correct.
                        RunGuarded(() =>
                            {
                                _iob.Clock();
                                _cpu.Run();
                            });

                        _state = RunState.Paused;
                        break;

                    case RunState.RunInst:
                        // Run a single QCode.  As above, except we execute
                        // PERQ CPU instructions until the start of the next QCode.
                        RunGuarded(() =>
                            {
                                do
                                {
                                    _iob.Clock();
                                    _cpu.Run();

                                } while (!CPU.IncrementBPC);
                            });

                        _state = RunState.Paused;
                        break;

                    case RunState.Reset:
                        Reset();

                        _state = (_userInterrupt || Settings.PauseOnReset) ? RunState.Paused : RunState.Running;
                        
                        _userInterrupt = false;
                        break;

                    case RunState.Off:
                    case RunState.Paused:
                    case RunState.Halted:
                        running = false;
                        break;

                    case RunState.ShuttingDown:
                        _display.ShutdownSDL();
                        _state = RunState.Off;
                        break;
                }
            }
            PrintStatus();  // fixme move to command prompt?
        }

        /// <summary>
        /// Wait for the background threads to sync up at a given RunState.
        /// </summary>
        //public RunState WaitForSync(RunState s)
        //{
        //    bool sync = false;

        //    Console.WriteLine("[Waiting for thread sync in {0} state]", s);

        //    do
        //    {
        //        // fixme this is rudimentary; can't just loop forever...
        //        // should/may be able to check for error conditions, or
        //        // timeout if something is stuck?
        //        var cpuState = _cpu.State;
        //        var z80State = _iob.Z80System.IsRunning ? _iob.State : s;   // hack

        //        if (cpuState == s && z80State == s)
        //        {
        //            sync = true;
        //        }
        //        else if (cpuState == RunState.Halted || z80State == RunState.Halted)
        //        {
        //            // fixme this is an error condition... throw?
        //            // but what about when the z80 is "turned off"?
        //            // in async mode it should just spin in Paused mode... fmuta
        //            s = RunState.Halted;
        //            sync = true;
        //        }
        //        else Thread.Sleep(1);

        //    } while (!sync);

        //    return s;
        //}

        /// <summary>
        /// Reset the virtual machine.
        /// </summary>
        /// <remarks>
        /// This method is transitioned to by the state machine, not called
        /// asynchronously or directly.
        /// </remarks>
        private void Reset()
        {
            _cpu.Reset();
            _mem.Reset();
            _ioBus.Reset();
        }

        /// <summary>
        /// Executes the specified emulation delegate inside a try/catch block that
        /// properly handles PowerDown and other exceptions to return to debug state.
        /// </summary>
        private void RunGuarded(RunDelegate execute)
        {
            try
            {
                execute();
            }
            catch (Exception e)
            {
                // The emulation has hit a serious error.  Enter the debugger...
                _state = RunState.Halted;

                Console.WriteLine("\nBreak due to internal emulation error: {0}.\nSource={1}\nSystem state may be inconsistent.", e.Message, e.Source);
#if DEBUG
                Console.WriteLine(Environment.StackTrace);
#endif
            }
        }

        /// <summary>
        /// Prints the status of both processors at the current instruction.
        /// Should probably be made more configurable (some debug flag to turn
        /// on or off one or both disassemblies?).  (A graphical debugger would
        /// update itself based on the runstate/machinestate change events.)
        /// </summary>
        private void PrintStatus()
        {
            _iob.Z80System.ShowZ80State();
            CPU.ShowPC();

            Console.WriteLine("ucode {0}",
                        Disassembler.Disassemble(CPU.PC, CPU.GetInstruction(CPU.PC)));

            Console.WriteLine("inst  {0:x2}-{1} (@BPC {2})", CPU.OpFile[CPU.BPC],
                        QCodeHelper.GetQCodeFromOpCode(CPU.OpFile[CPU.BPC]).Mnemonic, CPU.BPC);
        }

        /// <summary>
        /// Initial call to load all of the defined media files from the Config.
        /// We call this after the PERQSystem is successfully instantiated so that
        /// a bad filename doesn't cause the constructor to bail; with the system
        /// in a Halted state the user can fix the bad pathname and continue.
        /// </summary>
        public void LoadAllMedia()
        {
            // todo: deal with multiple units once the disk controllers can handle that
            // route qic tape, 9track and smd to the option board, gpib devs someday...
            foreach (var drive in _conf.Drives)
            {
                if (!string.IsNullOrEmpty(drive.MediaPath))
                {
                    switch (drive.Device)
                    {
                        case DriveType.Floppy:
                            _iob.Z80System.LoadFloppyDisk(drive.MediaPath);
                            break;

                        case DriveType.Disk14Inch:
                            _iob.DiskController.LoadImage(drive.MediaPath);
                            break;

                        default:
                            throw new UnimplementedHardwareException(
                                string.Format("Support for drive type {0} is not implemented.", drive.Device));
                    }
                }
            }
        }

        /// <summary>
        /// If the user has specified an alternate boot character, kick off
        /// a workitem to automagically press it (up until the point in the
        /// standard boot microcode where the key is read).
        /// </summary>
        public void PressBootKey()
        {
            if (PERQemu.Controller.BootChar != 0)
            {
                Scheduler.Schedule(1 * Conversion.MsecToNsec, BootCharCallback);
            }
        }

        /// <summary>
        /// Simulate holding down a key if the user has selected an alternate
        /// boot char.  To be effective, only sends the character from the start
        /// of SYSB (when the DDS reaches 150) to when the Z80 reads and returns
        /// it to the microcode (by DDS 151).  Deschedules itself after that.
        /// </summary>
        /// <remarks>
        /// Most versions of SYSB start by immediately turning off the Z80,
        /// restarting it, enabling the keyboard to read the boot character,
        /// then turning everything off again while it continues the bootstrap.
        /// The microcode waits up to 4.2M cycles (around .7 seconds) to get a
        /// key from the keyboard before defaulting to 'a' boot.
        /// </remarks>
        private void BootCharCallback(ulong skewNsec, object context)
        {
            if (CPU.DDS < 152)
            {
                // Send the key:
                _iob.Z80System.Keyboard.QueueInput(PERQemu.Controller.BootChar);

                // And do it again.
                _cpu.Scheduler.Schedule(100 * Conversion.MsecToNsec, BootCharCallback);
            }
        }


        // The PERQ
        private Configuration _conf;
        private MemoryBoard _mem;
        private CPUBoard _cpu;
        private IOBoard _iob;
        private OptionBoard _oio;
        private Display _display;
        private IOBus _ioBus;

        // Controlly bits
        private volatile RunState _state;
        private ExecutionMode _mode;
        private uint _uiEventInterval;

        private bool _userInterrupt;

        private delegate void RunDelegate();
    }
}
