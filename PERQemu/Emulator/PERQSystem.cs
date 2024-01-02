//
// PERQSystem.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

using SDL2;
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
                //
                // IOB boot ROMs:
                //      "PB" - IOB, 4K or 16K, old Z80          boot.rom
                //
                case IOBoardType.IOB:
                    _iob = new IOB(this);
                    _cpu.LoadBootROM("boot.rom");
                    break;

                //
                // CIO boot ROMs:  Shugart/Micropolis(?)
                //      "SD" - CIO, 4K CPU, new Z80             cioboot.rom
                //      "SC" - CIO, 16K CPU, new Z80            cioboot.rom
                //      Copied boot partition backups           ciomicboot.rom
                //
                case IOBoardType.CIO:
                    _iob = new CIO(this);
                    _cpu.LoadBootROM("cioboot.rom");
                    break;

                //
                // EIO boot ROMs:
                //      "MD" - EIO, 4K, new Z80, 8" disk        eio4kboot.rom [missing]
                //      "MC" - EIO, 16K, new Z80, 8" disk       eioboot.rom
                //      "TC" - EIO, 16K, new Z80, 5.25" disk    eio5boot.rom
                //
                case IOBoardType.EIO:
                case IOBoardType.NIO:
                    _iob = new EIO(this);

                    if (_conf.Chassis == ChassisType.PERQ2Tx)
                    {
                        // PERQ-2/T2 or /T4 (5.25")
                        _cpu.LoadBootROM("eio5boot.rom");
                    }
                    else
                    {
                        // Original PERQ-2 or /T1 model (8")
                        _cpu.LoadBootROM("eioboot.rom");
                    }
                    break;

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
            _debugger = new PERQDebugger(new List<object> { _cpu.Processor });

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

        public event MachineStateChangeEventHandler DDSChanged;
        public event MachineStateChangeEventHandler FloppyActivity;
        public event MachineStateChangeEventHandler StreamerActivity;
        public event MachineStateChangeEventHandler PowerDownRequested;


        /// <summary>
        /// Set the user's preferred run mode.  We assume async mode if the
        /// implementation supports it, but might want to select sync mode
        /// for debugging, or on uniprocessor systems.  (A "uniprocessor"?
        /// Is that like a "land line" or a "glacier"?)
        /// </summary>
        void SetMode()
        {
            _mode = PERQemu.Controller.Mode;

            if (_mode == ExecutionMode.Asynchronous && !(_iob.SupportsAsync && _cpu.SupportsAsync))
            {
                _mode = ExecutionMode.Synchronous;
                Log.Info(Category.Emulator,
                         "Asynchronous execution not supported; falling back to Synchronous mode.");
            }
        }

        void OnRunStateChange(RunStateChangeEventArgs s)
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
                    // Get the current run mode from the Controller
                    SetMode();

                    // Bring up the window if it's minimized
                    _display.Restore();

                    // Start up the CPU thread (both modes)
                    _cpu.RunAsync();

                    if (_mode == ExecutionMode.Asynchronous)
                    {
                        // Start up the separate Z80 thread
                        _iob.RunAsync();
                    }
                    break;

                case RunState.SingleStep:
                    //
                    // Run a single microinstruction.  Due to the discrepancy in
                    // execution rates, the Z80 may or may not execute an opcode!
                    //
                    RunGuarded(() =>
                        {
                            _cpu.Run();
                            _iob.Run();
                            _state = RunState.Paused;
                        });
                    break;

                case RunState.RunInst:
                    //
                    // Run a single QCode.  As above, except we execute PERQ
                    // microinstructions until the start of the next QCode.
                    //
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

                case RunState.RunZ80Inst:
                    //
                    // The Z80 won't execute if it has leapfrogged the main CPU;
                    // single step the PERQ until the main system clock catches
                    // up and the Z80 runs its next opcode.
                    //
                    RunGuarded(() =>
                        {
                            var startTime = _iob.Z80System.Clocks;
                            do
                            {
                                _cpu.Run();
                                _iob.Run();
                            }
                            while (_iob.Z80System.Clocks == startTime);

                            _state = RunState.Paused;
                        });
                    break;

                case RunState.ShuttingDown:
                    _state = RunState.Off;
                    Shutdown();
                    break;

                case RunState.Reset:
                    _cpu.Reset();
                    _mem.Reset();
                    _ioBus.Reset();
                    _state = RunState.Paused;
                    break;

                case RunState.Paused:
                case RunState.Halted:
                    _cpu.Stop();
                    _iob.Stop();
#if DEBUG
                    // Ah!  If we've paused on a breakpoint and there's a script
                    // to run, do it now;  Otherwise this is a no-op.  Kewl.
                    _debugger.RunDeferredActions();
#endif
                    break;
            }
            Log.Debug(Category.Emulator, "PERQSystem transitioned to {0}", _state);
        }

        /// <summary>
        /// Executes the specified emulation delegate inside a try/catch block that
        /// properly handles PowerDown and other exceptions to return to debug state.
        /// </summary>
        void RunGuarded(RunDelegate execute)
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
        /// Provide a hook to catch errors and halt execution.
        /// </summary>
        public void Halt(Exception e)
        {
            // The emulation has hit a serious error.  Return to the CLI.
            Log.Error(Category.All, "\nBreak due to internal emulation error: {0}", e.Message);
            Log.Error(Category.All, "System state may be inconsistent.\n");
#if DEBUG
            Log.Write(e.StackTrace);
#endif
            // Make sure both threads stop
            PERQemu.Controller.TransitionTo(RunState.Halted);
        }

        /// <summary>
        /// Completely shut down this instance.
        /// </summary>
        public void Shutdown()
        { 
            // Detach events
            PERQemu.Controller.RunStateChanged -= OnRunStateChange;

            // Now go away or I shall taunt you some more
            _inputs.Shutdown();
            _display.Shutdown();

            if (_oio != null) _oio.Shutdown();

            _iob.Shutdown();
            _cpu.Shutdown();
            _ioBus = null;
            Log.Detail(Category.Emulator, "PERQSystem shutdown.");
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
            _cpu.Processor.ShowCPUState();

            Console.WriteLine("ucode {0}",
                              Disassembler.Disassemble(CPU.PC, CPU.GetInstruction(CPU.PC)));

            // Print the current Q-code in 1- or 2-byte form
            if (CPU.LastOpcode > 255)
            {
                Console.WriteLine("inst  {0:x4}+{1} (@BPC {2})", CPU.LastOpcode,
                                  QCodeHelper.GetExtendedOpCode(CPU.LastOpcode).Mnemonic, CPU.BPC);
            }
            else
            {
                Console.WriteLine("inst  {0:x2}-{1} (@BPC {2})", CPU.LastOpcode,
                                  QCodeHelper.GetQCodeFromOpCode((byte)CPU.LastOpcode).Mnemonic, CPU.BPC);
            }
        }


        #region The white zone is for media loading and unloading only

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

                // If we have a path OR the drive is a removable type, load it
                if (!string.IsNullOrEmpty(drive.MediaPath) ||
                    drive.Type == DeviceType.Floppy ||
                    drive.Type == DeviceType.TapeQIC)
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
                case DeviceType.Disk8Inch:
                case DeviceType.DCIOMicrop:
                case DeviceType.Disk14Inch:
                case DeviceType.DCIOShugart:
                    // The UI shouldn't actually allow this but check anyway
                    if (_volumes[drive.Unit] != null)
                        throw new InvalidOperationException($"Drive {drive.Unit} is already loaded");
                    
                    // Spin 'er up
                    _volumes[drive.Unit] = _iob.LoadDisk(drive);

                    // Load indicates success!
                    return _volumes[drive.Unit].IsLoaded;
                                               
                case DeviceType.Floppy:
                    // Add the drive?
                    if (_volumes[drive.Unit] == null)
                    {
                        _volumes[drive.Unit] = _iob.LoadDisk(drive);

                        // Okay if there was no media
                        return _volumes[drive.Unit] != null;
                    }

                    // Place a new diskette in the existing drive!  Since we're
                    // reloading, assume the pathname changed
                    _volumes[drive.Unit].LoadFrom(drive.MediaPath);

                    // Fail if the file given couldn't be loaded
                    return (!string.IsNullOrEmpty(drive.MediaPath) && _volumes[drive.Unit].IsLoaded);

                case DeviceType.TapeQIC:
                    // Add the drive?
                    if (_volumes[drive.Unit] == null)
                    {
                        _volumes[drive.Unit] = _oio.LoadTape(drive);

                        return _volumes[drive.Unit] != null;
                    }

                    // Mount a cartridge in the drive
                    _volumes[drive.Unit].LoadFrom(drive.MediaPath);

                    // Return true only if the volume successfully loaded
                    return (!string.IsNullOrEmpty(drive.MediaPath) && _volumes[drive.Unit].IsLoaded);

                default:
                    throw new UnimplementedHardwareException($"Unhandled drive type {drive.Type}");
            }
        }

        /// <summary>
        /// Unload a device and remove it from the Volumes list.  If modified
        /// and not saved, gives the user a chance to save first (depending on
        /// autosave Settings).
        /// </summary>
        public void UnloadMedia(int unit)
        {
            if (_volumes[unit] == null)
                throw new InvalidOperationException($"Drive {unit} is not loaded");

            if (!_volumes[unit].IsLoaded) return;

            // We get ONE shot at this...
            if (_volumes[unit].IsModified && SaveRequested(unit))
            {
                if (SaveMedia(unit))
                {
                    Console.WriteLine($"Drive {unit} saved.");
                }
            }

            // Tell the device to unload
            _volumes[unit].Unload();

            if (!_volumes[unit].Info.IsRemovable)
            {
                // Non-removable drives get released.  Buh bye, now
                _volumes[unit] = null;
            }
        }

        /// <summary>
        /// Called at shutdown, tells all the loaded storage devices to unload
        /// and (optionally) save themselves.
        /// </summary>
        public void SaveAllMedia()
        {
            for (var unit = 0; unit < _volumes.Length; unit++)
            {
                if (_volumes[unit] != null)
                {
                    UnloadMedia(unit);
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
        /// Painstakingly assemble an SDL2 MessageBoxData structure one bloody
        /// manual step at a time.  Because why even bother with constructors
        /// when this is so much more fun?  <jams_dull_spoon_into_brain>
        /// </summary>
        SDL.SDL_MessageBoxData MakeMessageBox(StorageDevice dev, int unit)
        {
            var yesBtn = new SDL.SDL_MessageBoxButtonData();
            yesBtn.buttonid = 0;
            yesBtn.text = "Yep!";
            yesBtn.flags = SDL.SDL_MessageBoxButtonFlags.SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT;

            var noBtn = new SDL.SDL_MessageBoxButtonData();
            noBtn.buttonid = 1;
            noBtn.text = "Nope";
            noBtn.flags = SDL.SDL_MessageBoxButtonFlags.SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT;

            var mbData = new SDL.SDL_MessageBoxData();
            mbData.flags = SDL.SDL_MessageBoxFlags.SDL_MESSAGEBOX_INFORMATION;
            mbData.title = "Save modified media";
            mbData.message = $"Drive {unit} ({dev.Info.Name}) is modified.\n" +
                             $"Save the file '{dev.Filename}' before unloading?";

            mbData.numbuttons = 2;
            mbData.buttons = new SDL.SDL_MessageBoxButtonData[] { yesBtn, noBtn };

            mbData.colorScheme = null;                  // Use default colors
            mbData.window = PERQemu.GUI.DisplayWindow;  // Set parent window

            return mbData;
        }

        /// <summary>
        /// For a modified device, consults the user's preferences to determine
        /// if it should be saved or not.  For Yes or No does the obvious thing;
        /// for Maybe it pops up a dialog and lets the user decide.
        /// </summary>
        bool SaveRequested(int unit)
        {
            var dev = _volumes[unit];
            bool doit = false;
            bool running = (_state == RunState.Running);

            switch (dev.Info.Type)
            {
                case DeviceType.Floppy:
                case DeviceType.TapeQIC:
                    //
                    // The code is basically the same for ejectable media:
                    //
                    if ((dev.Info.Type == DeviceType.Floppy && Settings.SaveFloppyOnEject == Ask.Maybe) ||
                        (dev.Info.Type == DeviceType.TapeQIC && Settings.SaveTapeOnUnload == Ask.Maybe))
                    {
                        if (running)
                        {
                            // Pause emulation so SDL display events don't stack up
                            PERQemu.Controller.TransitionTo(RunState.Paused);

                            // Refresh the FPS to show we're paused!
                            _display.RefreshFPS(new HRTimerElapsedEventArgs(1d));
                            Thread.Sleep(15);
                        }

                        // Put up the modal dialog box
                        int btn = 0;
                        var mbData = MakeMessageBox(dev, unit);
                        var ret = SDL.SDL_ShowMessageBox(ref mbData, out btn);

                        // Assume a failure to mean don't save...
                        doit = (ret == 0 && btn == 0);

                        // Resume the emulator
                        if (running)
                        {
                            PERQemu.Controller.TransitionTo(RunState.Running);
                        }
                    }
                    else
                    {
                        if (dev.Info.Type == DeviceType.Floppy)
                            doit = (Settings.SaveFloppyOnEject == Ask.Yes);
                        else
                            doit = (Settings.SaveTapeOnUnload == Ask.Yes);
                    }
                    return doit;

                case DeviceType.Disk14Inch:
                case DeviceType.Disk8Inch:
                case DeviceType.Disk5Inch:
                    if (Settings.SaveDiskOnShutdown == Ask.Maybe)
                    {
                        if (running)
                        {
                            PERQemu.Controller.TransitionTo(RunState.Paused);
                            _display.RefreshFPS(new HRTimerElapsedEventArgs(1d));
                            Thread.Sleep(15);
                        }

                        int btn = 0;
                        var mbData = MakeMessageBox(dev, unit);
                        var ret = SDL.SDL_ShowMessageBox(ref mbData, out btn);

                        doit = (ret == 0 && btn == 0);

                        if (running)
                        {
                            PERQemu.Controller.TransitionTo(RunState.Running);
                        }
                    }
                    else
                    {
                        doit = (Settings.SaveDiskOnShutdown == Ask.Yes);
                    }
                    return doit;

                case DeviceType.DiskSMD:
                case DeviceType.Tape9Track:
                    // Not supported yet
                    return false;

                default:
                    throw new InvalidConfigurationException($"Device type {dev} is not supported, cannot save");
            }
        }

        #endregion

        /// <summary>
        /// Deal with MachineStateChange events.  For now (with no GUI, especially
        /// not a full-blown debugger) there aren't handlers/subscribers for some
        /// events, so they're dealt with locally.  This is kind of terrible.
        /// </summary>
        public void MachineStateChange(WhatChanged w, params object[] args)
        {
            MachineStateChangeEventHandler handler = null;

            switch (w)
            {
                case WhatChanged.DDSChanged:
                    handler = DDSChanged;
                    break;

                case WhatChanged.FloppyActivity:
                    handler = FloppyActivity;
                    break;

                case WhatChanged.StreamerActivity:
                    handler = StreamerActivity;
                    break;

                case WhatChanged.Z80RunState:
                    // For now, just a debug message; in future, Z80 debugger/GUI update
                    Log.Write(Category.Emulator, "Z80 power state changed: running={0}", (bool)args[0]);
                    return;

                case WhatChanged.HaltedInLoop:
                    // We don't actually fire an event; just call Halt()
                    Log.Write("The CPU has halted in a loop at PC {0:x4}", (ushort)args[0]);
                    PERQemu.Controller.TransitionTo(RunState.Halted);
                    return;

                case WhatChanged.PowerDown:
                    // Could probably just deal with this here, firing an SDL_QUIT directly...
                    handler = PowerDownRequested;
                    break;

                default:
                    // This can't/shouldn't happen
                    Log.Warn(Category.Emulator, "Unhandled MachineStateChange type {0}", w);
                    return;
            }

            if (handler != null)
            {
                Log.Debug(Category.Emulator, "MachineStateChange {0} firing", w);
                handler(new MachineStateChangeEventArgs(w, args));
            }
        }


        // The PERQ
        Configuration _conf;
        CPUBoard _cpu;
        MemoryBoard _mem;
        IOBus _ioBus;
        IOBoard _iob;
        OptionBoard _oio;

        // Drives attached to this PERQ
        StorageDevice[] _volumes;

        // User interface hooks (SDL)
        Display _display;
        InputDevices _inputs;

        // Debugger
        PERQDebugger _debugger;

        // Controlly bits
        ExecutionMode _mode;
        volatile RunState _state;

        delegate void RunDelegate();
    }
}
