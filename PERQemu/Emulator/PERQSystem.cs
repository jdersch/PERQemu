//
// PERQSystem.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.Processor;
using PERQemu.Memory;
using PERQemu.IO;
using PERQemu.IO.DiskDevices;
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
            // Everything we need to know to build the virtual PERQ
            _conf = conf;

            //
            // NB: There are still some dependencies here which require these
            // components to be created in order.  Memory must be set up first,
            // then the processor!  After the rest of the system objects are
            // instantiated, ROM images loaded, boards checked in, etc. the
            // machine must be Reset before it is fully initialized and ready
            // to run (ExecutionController sees to this).
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
                    //    _cpu.LoadBootROM("eio24boot.bin");    // 16K 24-bit, new Z80
                    //  }
                    //  else
                    //  {
                    //    _cpu.LoadBootROM("eioboot.bin");      // 16K, new Z80
                    //  }
                    //  break;
                    throw new UnimplementedHardwareException(
                        string.Format("IO board type {0} is not implemented.", _conf.IOBoard));

                default:
                    throw new InvalidConfigurationException(
                        string.Format("No such IO board type '{0}'", _conf.IOBoard));
            }

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
            _ioBus = new IOBus();

            // Attach devices
            _ioBus.AddDevice(_mem.Video);
            _ioBus.AddDevice(_iob);
            _ioBus.AddDevice(_oio);

            // Set our initial state; instantiated but not yet initialized...
            // shouldn't this just be... "On"?  Sigh.
            _state = RunState.Off;

            SetMode();

            // Compute how many cycles between runs of the GUI's event loop (in
            // this case, the SDL2 message loop).  In Synchronous mode we explicitly
            // call on the Display to run the loop; in Asynch mode it just sits in a
            // wait rather than poll.  FIXME.  This is gross.
            _uiEventInterval = (uint)((16.6667 * Conversion.MsecToNsec) / CPU.MicroCycleTime);

            // Okay!  We have a PERQ!  Now listen for state change events
            // from the Controller (or Debugger)
            PERQemu.Controller.RunStateChanged += OnRunStateChange;
        }

        public Configuration Config => _conf;

        public CPU CPU => _cpu.Processor;
        public Scheduler Scheduler => _cpu.Scheduler;

        public MemoryBoard Memory => _mem;
        public VideoController VideoController => _mem.Video;
        public Display Display => _display;

        public IOBus IOBus => _ioBus;
        public IOBoard IOB => _iob;
        public OptionBoard OIO => _oio;

        public ExecutionMode Mode => _mode;
        public RunState State => _state;

        /// <summary>
        /// Set the user's preferred run mode.  We assume async mode if the
        /// implementation supports it, but might want to select sync mode
        /// for debugging, or on uniprocessor systems.  (A "uniprocessor"?
        /// Is that like a "land line" or a "glacier"?)
        /// </summary>
        private void SetMode()
        {
            _mode = PERQemu.Controller.Mode;

            if (_mode == ExecutionMode.Asynchronous && !(_iob.SupportsAsync && _cpu.SupportsAsync))
            {
                _mode = ExecutionMode.Synchronous;
                Log.Info(Category.Emulator, "Asynchronous execution not supported; falling back to Synchronous mode.");
            }
        }

        private void OnRunStateChange(RunStateChangeEventArgs s)
        {
            _state = s.State;

            Log.Debug(Category.Emulator, "PERQSystem state changing to {0} ({1} mode)", _state, _mode);

            switch (_state)
            {
                case RunState.WarmingUp:
                    _display.InitializeSDL();
                    break;

                case RunState.Running:
                    Run();
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
                            _state = RunState.Paused;
                        });
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
                            }
                            while (!CPU.IncrementBPC);

                            _state = RunState.Paused;
                        });
                    break;

                case RunState.ShuttingDown:
                    _display.ShutdownSDL();
                    _state = RunState.Off;
                    break;

                case RunState.Reset:
                    Reset();
                    _state = RunState.Paused;
                    break;

                case RunState.Paused:
                case RunState.Halted:
                    break;

                case RunState.Off:
                    break;
            }
            Log.Debug(Category.Emulator, "PERQSystem transitioned to {0}", _state);
        }

        /// <summary>
        /// Run the machine until the state changes.  For now, calls the SDL message
        /// loop directly, which is horrible awful no good very bad and I am deeply
        /// ashamed by this.
        /// </summary>
        public void Run()
        {
            // Get the current run mode from the Controller
            SetMode();

            if (_mode == ExecutionMode.Asynchronous)
            {
                // Start up the background threads
                _iob.RunAsync();
                _cpu.RunAsync();

                // Run the event loop until State changes
                _display.SDLMessageLoop(_mode);
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
                                _display.SDLMessageLoop(_mode);
                                count = 0;
                            }
                        }
                    });
            }
        }

        /// <summary>
        /// Reset the virtual machine.
        /// </summary>
        /// <remarks>
        /// This method is transitioned to by the state machine, not called
        /// asynchronously or directly.
        /// </remarks>
        private void Reset()
        {
            Console.WriteLine("ExecutionController Reset called.");

            _cpu.Reset();
            _mem.Reset();
            _ioBus.Reset();
        }

        /// <summary>
        /// Provide a hook to catch errors and halt execution.
        /// </summary>
        public void Halt(Exception e)
        {
            // The emulation has hit a serious error.  Enter the debugger...
            Log.Error(Category.All, "\nBreak due to internal emulation error: {0}.\nSource={1}\nSystem state may be inconsistent.", e.Message, e.Source);
#if DEBUG
            Log.Write(Environment.StackTrace);
#endif
            PERQemu.Controller.Halt();
        }

        public void Shutdown()
        {
            // Detach
            PERQemu.Controller.RunStateChanged -= OnRunStateChange;
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
                Halt(e);
            }
        }

        /// <summary>
        /// Prints the status of both processors at the current instruction.
        /// Should probably be made more configurable (some debug flag to turn
        /// on or off one or both disassemblies?).  (A graphical debugger would
        /// update itself based on the runstate/machinestate change events.)
        /// </summary>
        public void PrintStatus()
        {
            _iob.Z80System.ShowZ80State();
            _cpu.Processor.ShowPC();

            Console.WriteLine("ucode {0}",
                        Disassembler.Disassemble(CPU.PC, CPU.GetInstruction(CPU.PC)));

            Console.WriteLine("inst  {0:x2}-{1} (@BPC {2})", CPU.OpFile[CPU.BPC],
                        QCodeHelper.GetQCodeFromOpCode(CPU.OpFile[CPU.BPC]).Mnemonic, CPU.BPC);
        }

        /// <summary>
        /// Initial call to load all of the defined media files from the Config.
        /// </summary>
        /// <remarks>
        /// We call this after the PERQSystem is successfully instantiated so
        /// that a bad filename doesn't cause the constructor to bail; with the
        /// system in a Halted state the user can fix the bad pathname and
        /// reset.  All drives are loaded and then assigned to controllers by
        /// device type!
        /// </remarks>
        public bool LoadAllMedia()
        {
            for (var unit = 0; unit < _conf.Drives.Length; unit++)
            {
                var drive = _conf.Drives[unit];

                if (!string.IsNullOrEmpty(drive.MediaPath))
                {
                    if (!LoadMedia(drive.Type, drive.MediaPath, unit))
                        return false;
                }
            }

            return true;
        }

        /// <summary>
        /// Load a media file (floppy, hard disk or tape).
        /// </summary>
        public bool LoadMedia(DeviceType type, string path, int unit)
        {
            try
            {
                // As PERQmedia to see if it's loadable
                var dev = FileUtilities.GetDeviceTypeFromFile(path);

                // Is it what we're expecting?
                if (dev != type)
                {
                    Console.WriteLine("File loaded is not the correct media type for this drive!");
                    Console.WriteLine($"  Unit {unit}  Expected: {type}  Loaded: {dev}");
                    return false;
                }

                // Hand it off to the appropriate controller
                switch (dev)
                {
                    // case DriveType.Disk5Inch:
                    // case DriveType.Disk8Inch:
                    case DeviceType.Disk14Inch:
                    case DeviceType.Floppy:
                        _iob.LoadDisk(type, path, unit);
                        break;

                    // case DriveType.DiskSMD:
                    //  _oio.LoadDisk();

                    // case DriveType.Tape*:
                    //  _oio.LoadTape();

                    default:
                        throw new UnimplementedHardwareException($"Support for drive type {type} is not implemented.");
                }
                return true;
            }
            catch (Exception e)
            {
                Console.WriteLine("Failed to load '{0}' (unit {1}): {2}", path, unit, e.Message);
                return false;
            }
        }

        /// <summary>
        /// Tells all the loaded storage devices to save themselves, depending
        /// on user preferences.  Saves to original format and filename.
        /// </summary>
        public void SaveAllMedia()
        {
            // Should we bother?
            if (Settings.SaveDiskOnShutdown == Ask.No) return;

            Console.WriteLine("Checking for unsaved media...");
            for (var unit = 0; unit < Config.Drives.Length; unit++)
            {
                var drive = _conf.Drives[unit];
                if (!string.IsNullOrEmpty(drive.MediaPath))
                {
                    SaveMedia(drive.Type, drive.MediaPath, unit);
                }
            }
        }

        public void SaveMedia(DeviceType type, string path, int unit = 0)
        {
            switch (type)
            {
                // case DriveType.Disk5Inch:
                // case DriveType.Disk8Inch:
                case DeviceType.Disk14Inch:
                case DeviceType.Floppy:
                    _iob.SaveDisk(unit);
                    break;

                // case DriveType.DiskSMD:
                //  _oio.LoadDisk();

                // case DriveType.Tape*:
                //  _oio.LoadTape();

                default:
                    throw new UnimplementedHardwareException($"Support for drive type {type} is not implemented.");
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
            if (CPU.DDS < 151)
            {
                // Send the key:    fixme: iob and eio interfaces slightly different :-/
                _iob.Z80System.Keyboard.QueueInput(PERQemu.Controller.BootChar);

                // And do it again.
                Scheduler.Schedule(100 * Conversion.MsecToNsec, BootCharCallback);
            }
        }


        // The PERQ
        private Configuration _conf;
        private CPUBoard _cpu;
        private MemoryBoard _mem;
        private Display _display;
        private IOBus _ioBus;
        private IOBoard _iob;
        private OptionBoard _oio;

        // Controlly bits
        private ExecutionMode _mode;
        private volatile RunState _state;

        // This is a hack, and should move elsewhere...?
        private uint _uiEventInterval;

        private delegate void RunDelegate();
    }
}
