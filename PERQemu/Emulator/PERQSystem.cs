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
using System.Collections.Generic;

using PERQmedia;

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.Processor;
using PERQemu.Memory;
using PERQemu.IO;
using PERQemu.UI;

namespace PERQemu
{
    /// <summary>
    /// PERQSystem encapsulates a complete virtual machine as described by a
    /// Configuration object.  It creates all of the necessary components,
    /// attaches a Display and a Debugger, loads the media files, and listens
    /// for commands from the ExecutionController to run, stop, single step, or
    /// shutdown.
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
                    throw new UnimplementedHardwareException($"IO board type {_conf.IOBoard}");

                default:
                    throw new InvalidConfigurationException($"No such IO board type '{_conf.IOBoard}'");
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
                    throw new UnimplementedHardwareException($"IO Option board type {_conf.IOOptionBoard}");

                default:
                    throw new InvalidConfigurationException($"No such IO Option board type '{_conf.IOOptionBoard}'");
            }

            // The Display will initialize itself (lazily)
            _display = new Display(this);

            // Interface to the host keyboard & mouse
            _inputs = new InputDevices(this);

            // Set up the IO Bus and check in the boards
            _ioBus = new IOBus();

            // Attach devices
            _ioBus.AddDevice(_mem.Video);
            _ioBus.AddDevice(_iob);
            _ioBus.AddDevice(_oio);

            // Allocate handles for storage devices to be mounted
            _volumes = new StorageDevice[_conf.Drives.Length];

            // Attach a debugger!
            _debugger = new PERQDebugger(new List<object>() { _cpu.Processor });

            // Set our initial state; instantiated but not yet initialized...
            // shouldn't this just be... "On"?  Sigh.
            _state = RunState.Off;

            // Not sure that RunModes make sense anymore
            SetMode();

            // Okay!  We have a PERQ!  Now listen for state change events
            // from the Controller (or Debugger)
            PERQemu.Controller.RunStateChanged += OnRunStateChange;
        }

        public Configuration Config => _conf;

        public StorageDevice[] Volumes => _volumes;

        public CPU CPU => _cpu.Processor;
        public Scheduler Scheduler => _cpu.Scheduler;

        public MemoryBoard Memory => _mem;
        public VideoController VideoController => _mem.Video;
        public Display Display => _display;
        public InputDevices Mouse => _inputs;

        public IOBus IOBus => _ioBus;
        public IOBoard IOB => _iob;
        public OptionBoard OIO => _oio;

        public ExecutionMode Mode => _mode;
        public RunState State => _state;

        public PERQDebugger Debugger => _debugger;


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
                Log.Info(Category.Emulator,
                         "Asynchronous execution not supported; falling back to Synchronous mode.");
            }
        }

        private void OnRunStateChange(RunStateChangeEventArgs s)
        {
            _state = s.State;

            Log.Debug(Category.Emulator, "PERQSystem state changing to {0} ({1} mode)", _state, _mode);

            switch (_state)
            {
                case RunState.WarmingUp:
                    _display.Initialize();
                    _inputs.Initialize();
                    break;

                case RunState.Running:
                    Run();  // todo: just inline this if our simplified approach works out
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
                            _cpu.Run();
                            _iob.Run();
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
                                _cpu.Run();
                                _iob.Run();
                            }
                            while (!CPU.IncrementBPC);

                            _state = RunState.Paused;
                        });
                    break;

                case RunState.ShuttingDown:
                    _inputs.Shutdown();
                    _display.Shutdown();
                    _state = RunState.Off;
                    break;

                case RunState.Reset:
                    Reset();
                    _state = RunState.Paused;
                    break;

                case RunState.Paused:
                case RunState.Halted:
                    if (Mode == ExecutionMode.Asynchronous)
                    {
                        _cpu.Stop();
                        _iob.Stop();
                    }
                    break;

                case RunState.Off:
                    break;
            }
            Log.Debug(Category.Emulator, "PERQSystem transitioned to {0}", _state);
        }

        /// <summary>
        /// Run the machine until the state changes.
        /// </summary>
        public void Run()
        {
            // Get the current run mode from the Controller
            SetMode();

            if (_mode == ExecutionMode.Asynchronous)
            {
                // Start up the background threads
                _cpu.RunAsync();
                _iob.RunAsync();
            }
            else
            {
                // Run the PERQ CPU and Z80 CPU in lockstep until manually stopped
                RunGuarded(() =>
                    {
                        while (_state == RunState.Running)
                        {
                            _cpu.Run();
                            _iob.Run();
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
            Log.Info(Category.Emulator, "PERQSystem Reset called");

            _cpu.Reset();
            _mem.Reset();
            _ioBus.Reset();
        }

        /// <summary>
        /// Provide a hook to catch errors and halt execution.
        /// </summary>
        public void Halt(Exception e)
        {
            // The emulation has hit a serious error.  Return to the CLI.
            Log.Error(Category.All, "\nBreak due to internal emulation error: {0}.", e.Message);
            Log.Error(Category.All, "System state may be inconsistent.\n");
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

                if (!string.IsNullOrEmpty(drive.MediaPath) || drive.Type == DeviceType.Floppy)
                {
                    if (!LoadMedia(drive))
                    {
                        return false;
                    }
                }
            }

            return true;
        }

        /// <summary>
        /// Load (or reload) a media file (floppy, hard disk or tape).  Returns
        /// true and updates the _volumes[] with a reference to the actual loaded
        /// device upon successful load of a supported storage device.
        /// </summary>
        /// <remarks>
        /// Since we moved the error checking and media type/path verification
        /// into the UI, assume that the given Drive entry is valid and don't
        /// waste time duplicating those checks here.
        /// 
        /// If a drive is already loaded and is not marked as Removable, don't
        /// allow a reload -- the PERQ doesn't expect normal fixed hard disks to
        /// just appear or disappear while the machine is running.
        /// </remarks>
        public bool LoadMedia(Drive drive)
        {
            // Hand it off to the IO or Option IO board
            switch (drive.Type)
            {
                // case DriveType.Disk5Inch:
                // case DriveType.Disk8Inch:
                case DeviceType.Disk14Inch:
                    // The UI shouldn't actually allow this but check anyway
                    if (_volumes[drive.Unit] != null)
                        throw new InvalidOperationException($"Drive {drive.Unit} is already loaded");

                    // Spin 'er up
                    _volumes[drive.Unit] = _iob.LoadDisk(drive);

                    // Load indicates success!
                    return _volumes[drive.Unit].IsLoaded;

                case DeviceType.Floppy:
                    if (_volumes[drive.Unit] == null)
                    {
                        // Add the drive (with or without media)
                        _volumes[drive.Unit] = _iob.LoadDisk(drive);

                        // Okay if there was no media
                        return _volumes[drive.Unit] != null;
                    }

                    // Place a new diskette in the drive!  Since we're
                    // reloading, assume the pathname changed
                    _volumes[drive.Unit].LoadFrom(drive.MediaPath);

                    // Fail if the file given couldn't be loaded
                    return (!string.IsNullOrEmpty(drive.MediaPath) && _volumes[drive.Unit].IsLoaded);

                // case DriveType.DiskSMD:
                //  _oio.LoadDisk();

                // case DriveType.Tape*:
                //  _oio.LoadTape();

                default:
                    throw new UnimplementedHardwareException($"Drive type {drive.Type}");
            }
        }


        /// <summary>
        /// Unload a device and remove it from the Volumes list.
        /// </summary>
        public void UnloadMedia(int unit)
        {
            if (_volumes[unit] == null)
                throw new InvalidOperationException($"Drive {unit} is not loaded");

            // Tell the device to unload
            _volumes[unit].Unload();

            if (!_volumes[unit].Info.IsRemovable)
            {
                // Non-removable drives get released.  Buh bye, now
                _volumes[unit] = null;
            }
        }


        /// <summary>
        /// Report the status of loaded storage devices.  Basic info for now.
        /// </summary>
        public void CheckMedia()
        {
            for (var unit = 0; unit < _volumes.Length; unit++)
            {
                var dev = _volumes[unit];

                if (dev != null)
                {
                    Console.Write($"Drive {unit} ({dev.Info.Type}) is online");
                    if (dev.IsLoaded) Console.Write($", is loaded ({dev.Filename})");
                    if (dev.IsModified) Console.Write(", is modified");
                    if (dev.Info.IsWritable) Console.Write(", is writable");
                    Console.WriteLine();
                }
            }
        }

        /// <summary>
        /// Tells all the loaded storage devices to save themselves, depending
        /// on user preferences.  Called at shutdown, saves each device to its
        /// original format and filename.
        /// </summary>
        public void SaveAllMedia()
        {
            for (var unit = 0; unit < _volumes.Length; unit++)
            {
                if (_volumes[unit] != null)
                {
                    SaveMedia(unit);
                }
            }
        }

        /// <summary>
        /// Save a modified media file to disk.
        /// </summary>
        public bool SaveMedia(int unit)
        {
            // Sanity checks
            if (_volumes[unit] == null)
            {
                throw new InvalidOperationException($"Drive {unit} is not loaded");
            }

            if (!_volumes[unit].IsLoaded)
            {
                Log.Info(Category.Emulator, "Drive {0} is not loaded, cannot save", unit);
                return false;
            }

            // Did the user update the Configuration record?
            if (_conf.Drives[unit].MediaPath != _volumes[unit].Filename)
            {
                // Name changed!  Save it and set the Modified flag
                _volumes[unit].Filename = _conf.Drives[unit].MediaPath;
                _volumes[unit].IsModified = true;
            }

            if (!_volumes[unit].IsModified)
            {
                Log.Info(Category.Emulator, "Drive {0} media does not require saving", unit);
                return false;
            }

            // todo: here's where we should decide to pause the emulator if
            // running, do the save, then continue.  probably not necessary
            // for floppies, but for a large hard disk we don't want to take
            // a snapshot that might be modified as we're writing it out!

            _volumes[unit].Save();

            // A successful save clears the Modified flag
            return !_volumes[unit].IsModified;
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
                // Send the key:
                _iob.Z80System.Keyboard.QueueInput(PERQemu.Controller.BootChar);

                // And do it again
                Scheduler.Schedule(100 * Conversion.MsecToNsec, BootCharCallback);
            }
        }


        // The PERQ
        private Configuration _conf;
        private CPUBoard _cpu;
        private MemoryBoard _mem;
        private IOBus _ioBus;
        private IOBoard _iob;
        private OptionBoard _oio;

        // Drives attached to this PERQ
        private StorageDevice[] _volumes;

        // User interface hooks (SDL)
        private Display _display;
        private InputDevices _inputs;

        // Debugger
        private PERQDebugger _debugger;

        // Controlly bits
        private ExecutionMode _mode;
        private volatile RunState _state;

        private delegate void RunDelegate();
    }
}
