//
// ExecutionController.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Config;

namespace PERQemu
{
    public enum ExecutionMode
    {
        Synchronous,
        Asynchronous
    }

    public sealed class SMKey : Tuple<RunState, RunState>
    {
        public SMKey(RunState from, RunState to) : base(from, to) { }

        public RunState From => Item1;
        public RunState To => Item2;
    }

    public delegate void TransitionDelegate();

    public sealed class Transition
    {
        public Transition(TransitionDelegate thingToDo, RunState expectedResult, bool blocking = true)
        {
            DoTheThing = thingToDo;
            ExpectedResult = expectedResult;
            WaitForIt = blocking;
        }

        public TransitionDelegate DoTheThing;
        public RunState ExpectedResult;
        public bool WaitForIt;
    }


    /// <summary>
    /// Top-level control for managing the execution of a configured PERQ.
	/// It knows how to start, pause and stop the machine, as well as the
    /// debug modes(step, inst, auto), shifting gears through the RunStates
    /// in response to user input (or uncaught Emulator exceptions).
    /// </summary>
    public sealed class ExecutionController
    {

        public ExecutionController()
        {
            _system = null;
            _bootChar = 0;
            _bootKeyArmed = true;
            _mode = Settings.RunMode;

            InitStateMachine();
        }

        public PERQSystem System
        {
            get { return _system; }
        }

        public ExecutionMode Mode
        {
            get { return _mode; }
            set { _mode = value; }
        }

        public RunState State
        {
            get { return (_system == null ? RunState.Off : _system.State); }
        }

        public byte BootChar
        {
            get { return _bootChar; }
            set { _bootChar = value; }
        }

        public event RunStateChangeEventHandler RunStateChanged;

        /// <summary>
        /// Instantiate a new PERQSystem from the given Configuration.
        /// </summary>
        public bool Initialize(Configuration conf)
        {
            // Quick sanity check
            if (!PERQemu.Config.Validate())
            {
                Console.WriteLine("The system as configured is invalid and cannot start.");
                Console.WriteLine("Please check the configuration and try again.");
                return false;
            }

            try
            {
                _system = new PERQSystem(conf);

                // Reset upon successful build
                PERQemu.Config.Changed = false;

                Console.WriteLine("PERQSystem initialized.");
                return true;
            }
            catch (Exception e)
            {
                Console.WriteLine("The system could not be initialized:");
                Console.WriteLine(e.Message);
                Console.WriteLine("Please check the configuration and try again.");
                _system = null;
                return false;
            }
        }

        /// <summary>
        /// Powers on the PERQSystem, initializing a new machine if the config has
        /// changed, and places it in "warming up" mode.  At this point, the SDL
        /// Display window is created and the machine is made ready to run.
        /// </summary>
        public void PowerOn()
        {
            // Out with the old
            if (_system != null)
            {
                throw new InvalidOperationException("Power on when system already exists?");
            }

            Console.WriteLine("Power on requested.");

            // Restart the SDL machinery
            PERQemu.GUI.InitializeSDL();

            // In with the new?
            if (!Initialize(PERQemu.Config.Current))
            {
                Console.WriteLine("System initialization failed.");
                // No worky Perqy
                _system = null;
                return;
            }

            // We have a system!  Listen for events
            PERQemu.Sys.DDSChanged += PressBootKey;
            PERQemu.Sys.PowerDownRequested += SoftPowerOff;

            // Load the configured storage devices
            if (!_system.LoadAllMedia())
            {
                Console.WriteLine("Storage initialization failed.");
                // Give an opportunity to try again
                SetState(RunState.Halted);
                return;
            }

            // Set the initial state
            SetState(RunState.WarmingUp);

            // Save as the default config for next time we launch
            if (PERQemu.Config.Current.Name != "default")
            {
                Settings.Changed = true;
            }
        }

        /// <summary>
        /// Resets the PERQ.
        /// </summary>
        /// <remarks>
        /// If PauseOnReset is set, leaves the machine in Paused and waits for the
        /// user to issue run (or step) commands.  If not, acts like the hardware
        /// boot/reset button and starts the machine running again.
        /// </remarks>
        public void Reset()
        {
            _bootKeyArmed = true;

            // If no system, quietly no-op
            if (State > RunState.Off)
            {
                TransitionTo(RunState.Reset);
                Console.WriteLine("PERQ system reset.");

                if (State == RunState.Paused && !Settings.PauseOnReset)
                {
                    TransitionTo(RunState.Running);
                }
            }
        }

        /// <summary>
        /// Pauses the PERQ, if it's running.
        /// </summary>
        public void Break()
        {
            if (State > RunState.WarmingUp && State < RunState.Halted)
            {
                // Break out of the Running state
                TransitionTo(RunState.Paused);
                _system.PrintStatus();
            }
        }

        /// <summary>
        /// Shuts down the virtual machine, asks or saves changed media (depending
        /// on user preferences), then deconfigures the PERQSystem.  The machine
        /// may only be restarted by a subsequent PowerOn.
        /// </summary>
        public void PowerOff(bool save = true)
        {
            if (State == RunState.Off) return;

            Console.WriteLine();
            Console.WriteLine("Power off requested.");

            // Force the machine to pause if in any other state
            if (State > RunState.WarmingUp && State < RunState.Halted)
            {
                TransitionTo(RunState.Paused);
            }

            // Give opportunity to save disks
            if (save)
            {
                _system.SaveAllMedia();
            }

            // Now unhook and close down
            PERQemu.Sys.DDSChanged -= PressBootKey;
            PERQemu.Sys.PowerDownRequested -= SoftPowerOff;

            TransitionTo(RunState.Off);

            // The only way to actually release resources? :-/
            PERQemu.GUI.ShutdownSDL();

            // Farewell, sweet PERQ
            _system = null;
        }

        /// <summary>
        /// Catch PERQ-1 "soft" power down events.  Queues up an SDL "quit"
        /// event so that the current instruction can finish and the processor
        /// transition to idle before initiating the PowerOff sequence -- else
        /// the PERQsystem object gets clobbered and we throw an exception and
        /// blow up.  This is just... a convoluted way of avoiding a catch {}.
        /// </summary>
        public void SoftPowerOff(MachineStateChangeEventArgs a)
        {
            Log.Info(Category.Emulator, "The PERQ has powered itself off.");

            var e = new SDL.SDL_Event();
            e.type = SDL.SDL_EventType.SDL_QUIT;
            SDL.SDL_PushEvent(ref e);
        }

        /// <summary>
        /// If the user has specified an alternate boot character, kick off
        /// a workitem to automagically press it (up until the point in the
        /// standard boot microcode where the key is read).
        /// </summary>
        public void PressBootKey(MachineStateChangeEventArgs a)
        {
            var dds = (int)a.Args[0];

            if (dds == 149 && _bootChar != 0 && _bootKeyArmed)
            {
                Log.Write("Selecting '{0}' boot...", (char)_bootChar);
                var count = 25;
                BootCharCallback(0, count);
            }
        }

        /// <summary>
        /// Simulate holding down a key if the user has selected an alternate
        /// boot char.  To be effective, only sends the character from the start
        /// of SYSB (when the DDS reaches 150) to when the Z80 reads and returns
        /// it to the microcode (by DDS 151).  Deschedules itself after that.
        /// </summary>
        /// <remarks>
        /// SYSB starts by immediately turning off the Z80, restarting it,
        /// enabling the keyboard to read the boot character, then turning the
        /// Z80 off again while it continues the bootstrap.  The microcode waits
        /// up to 4.2M cycles (around .7 seconds) to get a key from the keyboard
        /// before defaulting to 'a' boot.  The Z80 startup sequence always does
        /// at least one dummy read to clear the keyboard register so we really
        /// only need to jam the key in there 2-3 times... but the timing _must_
        /// be such that a keyboard interrupt is generated within the window so
        /// that the keystroke is actually sent to the PERQ in a message -- it
        /// isn't enough to just have the boot character in the keyboard buffer!
        /// 
        /// NB: For the PERQ-2 keyboard, the data is sent inverted!
        /// </remarks>
        void BootCharCallback(ulong skewNsec, object context)
        {
            var count = (int)context - 1;
            var keycode = (byte)(PERQemu.Sys.Config.Chassis == ChassisType.PERQ1 ? _bootChar : ~_bootChar);

            if (PERQemu.Sys.CPU.DDS < 152 && count > 0)
            {
                // Send the key:
                PERQemu.Sys.IOB.Z80System.QueueKeyboardInput(keycode);

                // And do it again
                PERQemu.Sys.Scheduler.Schedule(10 * Conversion.MsecToNsec, BootCharCallback, count);

                Log.Detail(Category.Emulator, "Pressing the bootchar again in 10msec, retry {0}", count);
            }
            else
            {
                // Disarm until next reset; POS G loops the DDS around so we
                // don't want to send extraneous keystrokes!  Oy.
                _bootKeyArmed = false;
            }
        }


        /// <summary>
        /// Change the state of the virtual machine.  For now, will block on all
        /// state transitions (even when running in Asynchronous mode).
        /// </summary>
        /// <remarks>
        /// See Docs/Controller.txt for more details.
        /// </remarks>
        public void TransitionTo(RunState nextState)
        {
            var current = State;

            if (current == RunState.Off)
            {
                Console.WriteLine("No PERQ defined; must 'power on' first.");
                return;
            }

            // Bail if already in the requested state
            if (current == nextState) return;

            Log.Debug(Category.Controller, "Requesting transition from {0} to {1}", current, nextState);

            // Now, how do we get there?  Let's consult the runes...
            var key = new SMKey(current, nextState);

            if (_SMDict.ContainsKey(key))
            {
                List<Transition> steps;

                if (_SMDict.TryGetValue(key, out steps))
                {
                    foreach (var step in steps)
                    {
                        // All steps execute Synchronously except the Running
                        // step, if Asynch mode is selected.  So we will block
                        // here until the delegate completes...
                        step.DoTheThing();

                        do
                        {
                            current = State;

                            // If we got the expected RunState (or an error, in
                            // which case we halt) then continue to the next step
                            if ((current == step.ExpectedResult) ||
                                (current == RunState.Halted) ||
                                (current == RunState.Off))
                            {
                                break;
                            }

                            Log.Debug(Category.Controller, "Waiting for {0}, got {1}...",
                                                            step.ExpectedResult, current);

                            // Pause and try again.  There should probably be
                            // a limit to our patience...
                            Thread.Sleep(10);

                        } while (step.WaitForIt);

                        // Abandon the rest of our steps
                        if (current == RunState.Off || current == RunState.Halted)
                        {
                            break;
                        }
                    }
                }
                else
                {
                    // fixme: this shouldn't happen if our state machine is complete... 
                    // so make this an error or just remove it once finally debugged
                    Log.Error(Category.Controller, "What, bad steps!? Key {0} yeilded no value!", key);
                }
            }
            else
            {
                // throw new InvalidOperationException()
                Console.WriteLine("Sorry, ya cahnt get theyah from heeyah.");
            }

            Log.Debug(Category.Controller, "Transition result is {0}", State);
        }

        /// <summary>
        /// Initiate a state change in the current PERQsystem.
        /// </summary>
        void SetState(RunState s)
        {
            RunStateChangeEventHandler handler = RunStateChanged;

            if (handler != null)
            {
                Log.Debug(Category.Controller, "SetState to {0} on {1}", s, Thread.CurrentThread.ManagedThreadId);
                handler(new RunStateChangeEventArgs(s));
            }
        }

        /// <summary>
        /// Set up table of legal transitions and the steps needed to get there.
        /// This is either brilliant, or insane.  ¯\_(ツ)_/¯
        /// </summary>
        void InitStateMachine()
        {
            _SMDict = new Dictionary<SMKey, List<Transition>> {
                {
                    new SMKey(RunState.WarmingUp, RunState.Reset), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Paused), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.RunInst), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunInst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.RunZ80Inst), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunZ80Inst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.SingleStep), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.SingleStep); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Running), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.Running); }, RunState.Running) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Halted), new List<Transition> {
                        new Transition(() => { SetState(RunState.Halted); }, RunState.Halted) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Off), new List<Transition> {
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.Paused, RunState.Reset), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.RunInst), new List<Transition> {
                        new Transition(() => { SetState(RunState.RunInst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.RunZ80Inst), new List<Transition> {
                        new Transition(() => { SetState(RunState.RunZ80Inst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.SingleStep), new List<Transition> {
                        new Transition(() => { SetState(RunState.SingleStep); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.Running), new List<Transition> {
                        new Transition(() => { SetState(RunState.Running); }, RunState.Running) }
                },
                {
                    new SMKey(RunState.Paused, RunState.Off), new List<Transition> {
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.Running, RunState.Paused), new List<Transition> {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.Reset), new List<Transition> {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.RunInst), new List<Transition> {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunInst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.RunZ80Inst), new List<Transition> {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunZ80Inst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.SingleStep), new List<Transition> {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.SingleStep); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.Off), new List<Transition> {
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.Halted, RunState.Reset), new List<Transition> {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Halted, RunState.Off), new List<Transition> {
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                }
            };
        }

        // To encode our state machine transitions
        Dictionary<SMKey, List<Transition>> _SMDict;

        ExecutionMode _mode;

        static byte _bootChar;
        bool _bootKeyArmed;

        PERQSystem _system;
    }
}
