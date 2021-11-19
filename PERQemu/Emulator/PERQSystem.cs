
// perqsystem.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
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
            switch (_conf.CPU)
            {
                case CPUType.PERQ1:
                    _cpu = new PERQ1(this);
                    break;

                case CPUType.PERQ1A:
                    _cpu = new PERQ1A(this);
                    break;

                case CPUType.PERQ24:
                    _cpu = new PERQ24(this);
                    break;

                case CPUType.PERQ24A:
                    throw new UnimplementedHardwareException("Sorry, PERQ24A CPU is not implemented.");

                default:
                    throw new InvalidConfigurationException(
                        string.Format("No such CPU board type '{0}'", _conf.CPU));
            }

            // Create the CPU's scheduler
            _scheduler = new Scheduler(CPU.MicroCycleTime);


            // Fire up the IO board (which will set up the Z80).  Once the
            // board type is selected, load the appropriate CPU boot ROMs!
            switch (_conf.IOBoard)
            {
                case IOBoardType.IOB:
                    _iob = new IOB(this);
                    _cpu.LoadROM(Paths.BuildPROMPath("boot.bin"));  // 4K or 16K, old Z80
                    break;

                case IOBoardType.CIO:
                _iob = new CIO(this);
                _cpu.LoadROM("cioboot.bin");          // 4K or 16K, new Z80
                break;

                //case IOBoardType.EIO:
                //case IOBoardType.NIO:
                //_iob = new EIO(this);
                //if (_conf.CPU == CPUType.PERQ24)
                //{
                //    _cpu.LoadROM("eio24boot.bin");    // 16K 24-bit, new Z80
                //}
                //else
                //{
                //    _cpu.LoadROM("eioboot.bin");      // 16K, new Z80
                //}
                //break;

                case IOBoardType.EIO:
                case IOBoardType.NIO:
                    throw new UnimplementedHardwareException(
                        string.Format("IO board type {0} is not implemented.", _conf.IOBoard));

                default:
                    throw new InvalidConfigurationException(
                        string.Format("No such IO board type '{0}'", _conf.IOBoard));
            }

            // Assume async mode if the IO Board implementation supports it.
            // Might want to select sync mode on uniprocessor systems?  What is
            // a "uniprocessor"?  Is that like a "land line" or a "glacier"?
            _z80ExecutionMode = _iob.SupportsAsync ? ExecutionMode.Asynchronous : ExecutionMode.Synchronous;

            // If any IO options are defined, instantiate the board
            // which will contain them (and set them up)
            switch (conf.IOOptionBoard)
            {
                case OptionBoardType.None:
                    _oio = null;
                    break;

                case OptionBoardType.OIO:
                    _oio = new OIO(); // this
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

            // Set up our IO bus; devices will check in at Reset
            _ioBus = new IOBus(this);

            // Load media!!
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

            // Create a lock to protect state transitions
            _stLock = new object();

        }


        public Configuration Config => _conf;
        public Scheduler Scheduler => _scheduler;

        public CPU CPU => _cpu;
        public MemoryBoard Memory => _mem;
        public VideoController VideoController => _mem.Video;
        public Display Display => _display;
        public IOBoard IOB => _iob;
        public OIO OIO => _oio;              // todo ioboard (or optionioboard?)
        public IOBus IOBus => _ioBus;


        public ExecutionMode Z80ExecutionMode
        {
            get { return _z80ExecutionMode; }
            set { _z80ExecutionMode = value; }
        }

        public RunState State
        {
            get { return _state; }
            set
            {
                lock (_stLock)
                {
                    _state = value;
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
                _scheduler.Schedule(10 * Conversion.MsecToNsec, BootCharCallback);
            }
        }

        /// <summary>
        /// User- or emulator-initiated pause in execution.
        /// </summary>
        public void Break()
        {
            _userInterrupt = true;
            State = RunState.Paused;
        }

        private void PrintStatus()
        {
            _iob.Z80System.ShowZ80State();
            _cpu.ShowPC();

            Console.WriteLine("ucode {0}",
                        Disassembler.Disassemble(_cpu.PC, _cpu.GetInstruction(_cpu.PC)));

            Console.WriteLine("inst  {0:x2}-{1} (@BPC {2})", _cpu.OpFile[_cpu.BPC],
                        QCodeHelper.GetQCodeFromOpCode(_cpu.OpFile[_cpu.BPC]).Mnemonic, _cpu.BPC);
        }


        // Todo: make this our asynch loop and fire off the background thread
        // when we "power on" the machine.  exit when we "power off".
        public void Execute()
        {
            bool running = true;


            while (running)
            {
                Console.WriteLine("Executing state " + State);
                switch (State)
                {
                    case RunState.WarmingUp:
                        _display.InitializeSDL();
                        State = RunState.Reset;
                        break;

                    case RunState.Running:
                        if (_z80ExecutionMode == ExecutionMode.Asynchronous)
                        {
                            // Let the IOB run in its own thread.
                            _iob.RunAsync();

                            // Run the PERQ CPU until manually stopped
                            RunGuarded(() =>
                            {
                                while (State == RunState.Running)
                                {
                                    _scheduler.Clock();
                                    _cpu.Execute();
                                }
                            });

                            // Stop the IOB before continuing.
                            _iob.Stop();
                        }
                        else
                        {
                            // Run the PERQ CPU and Z80 CPU in lockstep until manually stopped
                            RunGuarded(() =>
                            {
                                while (State == RunState.Running)
                                {
                                    // Run the IOB for one Z80 instruction, then run the PERQ CPU for
                                    // the number of microinstructions equivalent to that wall-clock time.
                                    uint clocks = _iob.Clock();

                                    // TODO: Fudge the PERQ ratio here, need to work out the actual math.
                                    clocks = (uint)(clocks * 2.4);

                                    //RunState nextState = State;
                                    do
                                    {
                                        _scheduler.Clock();
                                        _cpu.Execute();
                                        clocks--;
                                    } while (clocks > 0);
                                }
                            });
                        }
                        break;

                    case RunState.SingleStep:
                    case RunState.RunZ80Inst:
                        // For now:
                        // Run the IOB for one Z80 instruction, then run the PERQ CPU for
                        // one instruction.
                        // Timing-wise this is very inaccurate.  It would be nice to allow single-stepping either
                        // processor and have the timings be correct.
                        //
                        RunGuarded(() =>
                        {
                            _iob.Clock();
                            _scheduler.Clock();
                            _cpu.Execute();
                        });

                        State = RunState.Paused;
                        break;

                    case RunState.RunInst:  // Run a single QCode
                                            // For now:
                                            // As above, except we execute PERQ CPU instructions until the start of the next QCode.
                        RunGuarded(() =>
                        {
                            do
                            {
                                _iob.Clock();
                                _scheduler.Clock();
                                _cpu.Execute();

                            } while (!_cpu.IncrementBPC);
                        });

                        State = RunState.Paused;
                        break;

                    case RunState.Reset:
                        Reset();

                        if (_userInterrupt || Settings.PauseOnReset)
                        {
                            State = RunState.Paused;
                            _userInterrupt = false;
                        }
                        else
                        {
                            State = RunState.Running;
                        }
                        break;

                    case RunState.Off:
                    case RunState.Paused:
                    case RunState.Halted:
                        running = false;
                        break;

                    case RunState.ShuttingDown:
                        _display.ShutdownSDL();
                        State = RunState.Off;
                        break;
                }
            }
            PrintStatus();  // fixme move to command prompt?
        }

        private delegate void RunDelegate();

        /// <summary>
        /// Reset the virtual machine.
        /// </summary>
        /// <remarks>
        /// This method is transitioned to by the state machine, not called
        /// asynchronously or directly.
        /// </remarks>
        private void Reset()
        {
            _scheduler.Reset();
            _cpu.Reset();
            _mem.Reset();
            _ioBus.Reset();
            _display.Reset();
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
            catch (PowerOffException)
            {
                // This is thrown when the microcode tells the PERQ to power down.
                // Catch here and tell the user.  Go back to debug state.
                State = RunState.ShuttingDown;
                //_debugMessage = "The PERQ has powered itself off.  Use the 'reset' command to restart the PERQ.";
                Console.WriteLine("The PERQ has powered itself off.  Use the 'reset' command to restart the PERQ.");
            }
            catch (Exception e)
            {
                // The emulation has hit a serious error.
                // Enter the debugger.
                State = RunState.Halted;
                //_debugMessage = string.Format("Break due to internal emulation error: {0}.  System state may be inconsistent.", e.Message);
                Console.WriteLine("\nBreak due to internal emulation error: {0}.\nSource={1}\nSystem state may be inconsistent.", e.Message, e.Source);
#if DEBUG
                Console.WriteLine(Environment.StackTrace);
#endif
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
        /// With only 2-3 retries at approximately 11.9ms of delay between each
        /// one, there's a very short window to sneak the boot key in there!
        /// </remarks>
        private void BootCharCallback(ulong skewNsec, object context)
        {
            if (_cpu.DDS < 152)
            {
                // Send the key:
                _iob.Z80System.Keyboard.QueueInput(PERQemu.Controller.BootChar);

                // And do it again.
                _scheduler.Schedule(10 * Conversion.MsecToNsec, BootCharCallback);
            }
        }

        private RunState _state;
        private bool _userInterrupt;
        private ExecutionMode _z80ExecutionMode;
        //private string _debugMessage;

        private Configuration _conf;
        private Scheduler _scheduler;
        private MemoryBoard _mem;
        private CPU _cpu;
        private IOBoard _iob;
        private IOBus _ioBus;
        private OIO _oio;
        private Display _display;

        private object _stLock;
    }
}
