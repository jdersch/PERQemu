
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
            _cpu = new PERQ1A(this);

            // Just have to do this once.  Will be CPU-specific. TODO: this moves to the CPUBoard
            _cpu.LoadROM(Paths.BuildPROMPath("boot.bin"));

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
        }


        //      =============================
        //
        //      MAJOR REORGANIZATION UNDERWAY
        //  "Strange things are afoot at the Circle-K"
        //
        //      =============================
        //
        // PERQSystem is undergoing major reconstructive surgery.  Things here
        // in the experiments branch will be changing rapidly in the coming
        // days/weeks.  Things will be broken or weird for a while.
        //      -- S. Boondoggle
        //

        public Configuration Config => _conf;
        public Scheduler Scheduler => _scheduler;

        public CPU CPU => _cpu;
        public MemoryBoard Memory => _mem;
        public VideoController VideoController => _mem.Video;
        public Display Display => _display;
        public IOB IOB => _iob;                 // todo ioboard
        public OIO OIO => _oio;                 // todo ioboard (or optionioboard?)
        public IOBus IOBus => _ioBus;

        public Debugger.Debugger Debugger => _debugger; // fixme soon to go

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

        public ExecutionMode Z80ExecutionMode
        {
            get { return _z80ExecutionMode; }
            set { _z80ExecutionMode = value; }
        }

        private delegate void RunDelegate();

        private void Reset()
        {
            _scheduler.Reset();
            _cpu.Reset();
            _mem.Reset();
            _ioBus.Reset();
        }

        public void Execute()
        {

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
                                _cpu.Execute();

                            } while (!_cpu.IncrementBPC);

                            // Drop back into debugging mode now.
                            _state = RunState.Debug;
                        });
                        break;

                    case RunState.Debug:
                        // Enter the debugger.  On return from debugger, switch to the specified state.
                        _state = _debugger.Enter(_debugMessage);
                        break;

                    case RunState.Reset:
                        Reset();
                        _state = RunState.Debug;
                        _debugMessage = "";
                        break;

                    case RunState.Exit:
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
                _debugMessage = string.Format("Break due to internal emulation error: {0}.  System state may be inconsistent.", e.Message);
#if DEBUG
                Console.WriteLine(Environment.StackTrace);
#endif
            }
        }

        /// <summary>
        /// If the user has specified an alternate boot character, kick off
        /// a workitem to automagically press it (up until the point in the
        /// standard boot microcode where the key is read).
        /// </summary>
        public void PressBootKey()
        {
            if (BootChar != 0)
            {
                _scheduler.Schedule((ulong)(10.0 * Conversion.MsecToNsec), BootCharCallback);
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
        /// one, there's actually a very short window to sneak the boot key in
        /// there!  Adding a new run state is a little cheesy but should be
        /// more accurate (and reliable) than before.
        /// </remarks>
        private void BootCharCallback(ulong skewNsec, object context)
        {
            if (_cpu.DDS < 152)
            {
                // Send the key:
                _iob.Z80System.Keyboard.QueueInput(BootChar);

                // And do it again.
                _scheduler.Schedule((ulong)(10.0 * Conversion.MsecToNsec), BootCharCallback);
            }
        }

        private RunState _state;
        private ExecutionMode _z80ExecutionMode;
        private string _debugMessage;
        private static byte _bootChar;

        private Configuration _conf;
        private Scheduler _scheduler;
        private CPU _cpu;
        private MemoryBoard _mem;
        private IOB _iob;
        private IOBus _ioBus;
        private OIO _oio;
        private Display _display;
        private Debugger.Debugger _debugger;
    }
}

