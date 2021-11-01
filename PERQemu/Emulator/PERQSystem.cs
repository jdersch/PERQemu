
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
using PERQemu.Config;
using PERQemu.Processor;
using PERQemu.Memory;
using PERQemu.Debugger;
using PERQemu.IO.SerialDevices;
using PERQemu.IO;
using PERQemu.UI;

namespace PERQemu
{

    public enum RunState
    {
        Run = 0,
        SingleStep,
        RunInst,
        RunZ80Inst,
        Debug,
        DebugScript,
        Reset,
        Exit,
    }

    /// <summary>
    /// PERQSystem encapsulates basic operation (normal execution, debugging, etc) of the emulated
    /// PERQ system.
    /// </summary>
    public sealed class PERQSystem
    {
        public PERQSystem(Configuration conf)
        {
            //
            // TODO: We now take in a configuration record, but make no attempt
            // yet to make sure it has been validated (since the Configurator
            // hasn't been wired in.  Assume we're passing in a known-good default
            // configuration (a PERQ-1A with 1-2MB and no funny business).
            //
            _conf = conf;

            _mem = new MemoryBoard(this);

            //
            // TODO: There are some ugly dependencies here...
            //      CPU needs Memory;
            //      Scheduler "needs" CPU;
            //      Display, Video need Memory, CPU, and (eventually IOB);
            //      IOBus needs IOB, OIO;
            //      new Z80 needs IOB?
            //
            _perqCPU = new PERQ1A(this);

            // Just have to do this once.  Will be CPU-specific. TODO: this moves to the CPUBoard
            _perqCPU.LoadROM(Paths.BuildPROMPath("boot.bin"));

            _scheduler = new Scheduler(CPU.MicroCycleTime);

            _display = new Display(this);
            _iob = new IOB(this);
            _oio = new OIO(); // this
            _ioBus = new IOBus(this);

            // Start off debugging
            _state = RunState.Debug;
            _debugger = new Debugger.Debugger(this);

            // Assume async mode if the IOB implementation supports it.
            // Might want to select sync mode on uniprocessor systems?
            _z80ExecutionMode = _iob.SupportsAsync ? ExecutionMode.Asynchronous : ExecutionMode.Synchronous;

            // Now issue a reset
            Reset();

            // And kick off the display...
            _display.InitializeSDL();
        }

        public void Execute(string[] args)
        {
            //
            // Evaluate commandline args.  For now, we support only one argument -- a script to execute.
            //
            if (args.Length > 0)
            {
                if (args.Length == 1)
                {
                    _state = RunState.DebugScript;
                }
                else
                {
                    Console.WriteLine("usage: PERQemu.exe <scriptFile>");
                }
            }

            bool running = true;
            while (running)
            {
                switch (_state)
                {
                    case RunState.Run:
                        if (_z80ExecutionMode == ExecutionMode.Asynchronous)
                        {
                            // Let the IOB run in its own thread.
                            _iob.RunAsync();

                            // Run the PERQ CPU until manually stopped
                            RunGuarded(() =>
                            {
                                while (_state == RunState.Run)
                                {
                                    _scheduler.Clock();
                                    _perqCPU.Execute();
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
                                while (_state == RunState.Run)
                                {
                                    // Run the IOB for one Z80 instruction, then run the PERQ CPU for
                                    // the number of microinstructions equivalent to that wall-clock time.
                                    uint clocks = _iob.Clock();

                                    // TODO: Fudge the PERQ ratio here, need to work out the actual math.
                                    clocks = (uint)(clocks * 2.4);

                                    RunState nextState = _state;
                                    do
                                    {
                                        _scheduler.Clock();
                                        _perqCPU.Execute();
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
                            _perqCPU.Execute();

                            // Drop back into debugging mode after running a single step.
                            _state = RunState.Debug;
                        });

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
                                _perqCPU.Execute();

                            } while (!_perqCPU.IncrementBPC);

                            // Drop back into debugging mode now.
                            _state = RunState.Debug;
                        });
                        break;

                    case RunState.Debug:
                        // Enter the debugger.  On return from debugger, switch to the specified state.
                        _state = _debugger.Enter(_debugMessage);
                        break;

                    case RunState.DebugScript:
                        _state = _debugger.RunScript(args[0]);
                        break;

                    case RunState.Reset:
                        Reset();
                        _state = RunState.Debug;
                        _debugMessage = "";
                        break;

                    case RunState.Exit:
                        Shutdown();
                        running = false;
                        break;
                }
            }
        }

        public void Break()
        {
            // User break into debugger
            _state = RunState.Debug;
        }

        public void Shutdown()
        {
            _display.ShutdownSDL();
        }

        /// <summary>
        /// Allows overriding the default OS boot character.
        /// (This is a key that, when held down at boot time will cause the PERQ
        /// microcode to select a different OS to boot.)
        /// </summary>
        public byte BootChar
        {
            get { return _bootChar; }
            set { _bootChar = value; }
        }

        public Configuration Config => _conf;
        public Scheduler Scheduler => _scheduler;

        public CPU CPU => _perqCPU;
        public MemoryBoard Memory => _mem;
        public VideoController VideoController => _mem.Video;
        public Display Display => _display;
        public IOB IOB =>  _iob;
        public OIO OIO => _oio;
        public IOBus IOBus => _ioBus;

        public Debugger.Debugger Debugger => _debugger;
        

        private delegate void RunDelegate();

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
                _state = RunState.Debug;
                _debugMessage = "The PERQ has powered itself off.  Use the 'reset' command to restart the PERQ.";
            }
            catch (Exception e)
            {
                // The emulation has hit a serious error.
                // Enter the debugger.
                _state = RunState.Debug;
                _debugMessage = String.Format("Break due to internal emulation error: {0}.  System state may be inconsistent.", e.Message);
#if DEBUG
                Console.WriteLine(Environment.StackTrace);
#endif
            }
        }


        #region Debugger Commands

        [DebugFunction("dump scheduler queue", "")]
        private void DumpScheduler()
        {
            _display.Status();
            VideoController.Status();
            _scheduler.DumpEvents();
        }

        [DebugFunction("set rs232", "Configures the emulated serial port to use the specified device")]
        private void SetSerialPort(string devName)
        {
            ISerialDevice dev = null;

            try
            {
                if (!PERQemu.HostIsUnix && devName.StartsWith("com", StringComparison.InvariantCultureIgnoreCase))
                {
                    // COM port.  Try to instantiate & assign to system.
                    dev = new PhysicalPort(devName);
                }
                else if (PERQemu.HostIsUnix && devName.StartsWith("/dev", StringComparison.InvariantCultureIgnoreCase))
                {
                    // Unix device path.  Try to instantiate & assign to system.
                    dev = new PhysicalPort(devName);
                }
                else if (devName.Equals("rsx:", StringComparison.InvariantCultureIgnoreCase))
                {
                    // RSX device.
                    dev = new RSXFilePort();
                }
                else
                {
                    throw new ArgumentOutOfRangeException("Invalid device name. Expected /dev/path, COMn: or RSX:");
                }

                _iob.Z80System.SetSerialPort(dev);
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to set rs232 port - {0}", e.Message);
            }
        }

        [DebugFunction("show rs232", "Displays the current rs232 device")]
        private void ShowSerialPort()
        {
            Console.WriteLine("RS232 port is set to {0}", _iob.Z80System.GetSerialPort());
        }

        [DebugFunction("create floppy", "Creates and mounts a new, unformatted floppy disk image")]
        private void CreateFloppyDisk()
        {
            _iob.Z80System.LoadFloppyDisk(null);
            Console.WriteLine("Created.");
        }

        [DebugFunction("load floppy", "Mounts a floppy disk image")]
        private void LoadFloppy(string imagePath)
        {
            try
            {
                _iob.Z80System.LoadFloppyDisk(imagePath);
                Console.WriteLine("Loaded.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("unload floppy", "Unmounts a floppy disk image")]
        private void UnloadFloppy()
        {
            _iob.Z80System.UnloadFloppyDisk();
        }

        [DebugFunction("save floppy", "Saves the current in memory floppy disk to an image file")]
        private void SaveFloppy(string imagePath)
        {
            try
            {
                _iob.Z80System.SaveFloppyDisk(imagePath);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("create harddisk", "Creates and mounts a new, unformatted hard disk image")]
        private void CreateHardDisk()
        {
            _iob.ShugartDiskController.LoadImage(null);
            Console.WriteLine("Created.");
        }

        [DebugFunction("load harddisk", "Mounts an existing hard disk image")]
        private void LoadHardDisk(string imagePath)
        {
            try
            {
                _iob.ShugartDiskController.LoadImage(imagePath);
                Console.WriteLine("Loaded.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("save harddisk", "Saves the current hard disk to an image file")]
        private void SaveHardDisk(string imagePath)
        {
            try
            {
                _iob.ShugartDiskController.SaveImage(imagePath);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("save screenshot", "Saves a screenshot of the current PERQ display in jpg format")]
        private void SaveScreenshot(string filePath)
        {
            string outputPath = filePath + ".jpg";
            try
            {
                _display.SaveScreenshot(outputPath);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error saving screenshot to {0} - Error: {1}", outputPath, e.Message);
            }
        }

        [DebugFunction("set bootchar", "Sets the boot character (which selects the OS to boot)")]
        private void SetBootChar(char bootChar)
        {
            BootChar = (byte)bootChar;
        }

        [DebugFunction("show bootchar", "Shows the boot character.")]
        private void ShowBootChar()
        {
            Console.Write("Bootchar is ");
            if (BootChar != 0)
            {
                Console.WriteLine((char)BootChar);
            }
            else
            {
                Console.WriteLine("unset");
            }
        }

        [DebugFunction("set z80 execution mode", "Sets the execution mode for the Z80 coprocessor on the IO board.")]
        private void SetZ80ExecutionMode(ExecutionMode mode)
        {
            if (mode == ExecutionMode.Asynchronous &&
                !_iob.Z80System.SupportsAsync)
            {
                Console.WriteLine("The current implementation does not support asynchronous execution.");
                _z80ExecutionMode = ExecutionMode.Synchronous;
            }
            else
            {
                _z80ExecutionMode = mode;
            }
        }

        [DebugFunction("show z80 execution mode", "Shows the currently set execution mode for the Z80 coprocessor on the IO board.")]
        private void ShowZ80ExecutionMode()
        {
            Console.WriteLine(_z80ExecutionMode);
        }

        #endregion

        private void Reset()
        {
            _scheduler.Reset();
            _perqCPU.Reset();
            _mem.Reset();
            _ioBus.Reset();

            //
            // If the user has specified an alternate boot character, kick off this workitem to
            // automagically press it up until DDS 154.
            //
            if (BootChar != 0)
            {
                _scheduler.Schedule((ulong)(250.0 * Conversion.MsecToNsec), BootCharCallback);
            }
        }

        private void BootCharCallback(ulong skewNsec, object context)
        {
            if (BootChar != 0 && CPU.DDS < 154)
            {
                // Send the key:
                _iob.Z80System.Keyboard.QueueInput(BootChar);

                // And do it again.
                _scheduler.Schedule((ulong)(250.0 * Conversion.MsecToNsec), BootCharCallback);
            }
        }

        private RunState _state;
        private ExecutionMode _z80ExecutionMode;
        private string _debugMessage;
        private static byte _bootChar;

        private Configuration _conf;
        private Scheduler _scheduler;
        private CPU _perqCPU;
        private MemoryBoard _mem;
        private IOB _iob;
        private IOBus _ioBus;
        private OIO _oio;
        private Display _display;
        private Debugger.Debugger _debugger;
    }
}

