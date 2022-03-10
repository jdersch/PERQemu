//
// ExecutionController.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
            get { return (_system == null ? RunState.Unavailable : _system.State); }
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
            Console.WriteLine("Power ON requested.");

            if (!PERQemu.Config.Current.IsValid)
            {
                Console.WriteLine("The system as configured is invalid and cannot start.");
                Console.WriteLine("Please check the configuration and try again.");
                return;
            }

            // Just always blow it away now.  Bloody hell I have to straighten out this mess
            if (_system != null || PERQemu.Config.Changed)
            {
                // Out with the old
                _system?.Shutdown();
                _system = null;
            }

                // In with the new?
                if (!Initialize(PERQemu.Config.Current))
                {
                    Console.WriteLine("System initialization failed.");
                    // No worky Perqy
                    _system = null;
                    return;
                }

                // Load the configured storage devices
                if (!_system.LoadAllMedia())
                {
                    Console.WriteLine("Storage initialization failed.");
                    // Give an opportunity to try again
                    Halt();
                    return;
                }


            // (Re-)Start the machine
            SetState(RunState.WarmingUp);
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
            // If no system, quietly no-op
            if (State > RunState.Off)
            {
                TransitionTo(RunState.Reset);

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
            if (State > RunState.Off && State != RunState.Halted)
            {
                // Break out of the Running state
                SetState(RunState.Paused);
            }

#if DEBUG
            HighResolutionTimer.DumpTimers();
            PERQemu.Sys.Scheduler.DumpEvents("CPU");
            PERQemu.Sys.IOB.Z80System.Scheduler.DumpEvents("Z80");
#endif
        }

        // sigh.
        public void Halt()
        {
            SetState(RunState.Halted);
        }

        /// <summary>
        /// Shuts down the virtual machine, asks or saves changed media (depending
        /// on user preferences), then deconfigures the PERQSystem.  The machine
        /// may only be restarted by a subsequent PowerOn.
        /// </summary>
        public void PowerOff(bool save = true)
        {
            if (State <= RunState.Off) return;

            Console.WriteLine("Power OFF requested on {0}.", Thread.CurrentThread.ManagedThreadId);

            // Force the machine to pause if in any other state
            Break();

            // Give opportunity to save disks
            if (save)
            {
                _system.SaveAllMedia();
            }

            // Now clobber the Display and close down
            SetState(RunState.ShuttingDown);

            // Farewell, sweet PERQ
            _system = null;
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

            if (current == RunState.Unavailable)
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
                                (current == RunState.Unavailable))
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
                        if (current == RunState.Unavailable || current == RunState.Halted)
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
        private void SetState(RunState a)
        {
            Log.Debug(Category.Controller, "SetState on {0} to call {1}", Thread.CurrentThread.ManagedThreadId, a);
            RunStateChangeEventHandler handler = RunStateChanged;
            handler?.Invoke(new RunStateChangeEventArgs(a));
        }

        /// <summary>
        /// Set up table of legal transitions and the steps needed to get there.
        /// This is either brilliant, or insane.  ¯\_(ツ)_/¯
        /// </summary>
        private void InitStateMachine()
        {
            _SMDict = new Dictionary<SMKey, List<Transition>>() {
                {
                    new SMKey(RunState.Off, RunState.Paused), new List<Transition>() {
                        new Transition(() => { PowerOn(); }, RunState.WarmingUp),
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Off, RunState.Running), new List<Transition>() {
                        new Transition(() => { PowerOn(); }, RunState.WarmingUp),
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.Running); }, RunState.Paused, false) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Paused), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.RunZ80Inst), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunZ80Inst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Off), new List<Transition>() {
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.WarmingUp, RunState.Running), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.Running); }, RunState.Paused, false) }
                },
                {
                    new SMKey(RunState.Paused, RunState.Off), new List<Transition>() {
                        new Transition(() => { PowerOff(); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.Paused, RunState.Reset), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.RunInst), new List<Transition>() {
                        new Transition(() => { SetState(RunState.RunInst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.RunZ80Inst), new List<Transition>() {
                        new Transition(() => { SetState(RunState.RunZ80Inst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.SingleStep), new List<Transition>() {
                        new Transition(() => { SetState(RunState.SingleStep); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Paused, RunState.Running), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Running); }, RunState.Paused, false) }
                },
                {
                    new SMKey(RunState.Running, RunState.Paused), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.Reset), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.RunInst), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunInst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.RunZ80Inst), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.RunZ80Inst); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.SingleStep), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.SingleStep); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Running, RunState.Off), new List<Transition>() {
                        new Transition(() => { PowerOff(); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.Halted, RunState.Reset), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Halted, RunState.Off), new List<Transition>() {
                        new Transition(() => { PowerOff(); }, RunState.Off) }
                }
            };
        }

        // To encode our state machine transitions
        private Dictionary<SMKey, List<Transition>> _SMDict;

        private ExecutionMode _mode;
        private static byte _bootChar;
        private PERQSystem _system;
    }
}
