//
// ExecutionController.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
    /// <summary>
    /// Defines the run state of the virtual PERQ.
    /// </summary>
    public enum RunState
    {
        Off = 0,        // Power is off or no PERQ configured
        WarmingUp,      // Power is on and the GUI is warming up :-)
        Reset,          // Transitional reset state
        Paused,         // User- or program-requested Pause
        Running,        // Run, run like the wind
        SingleStep,     // Debugger is single stepping execution
        RunInst,        // Debugger is running one opcode
        RunZ80Inst,     // Debugger is running one Z80 opcode
        Halted,         // Exception or grievous error stopped execution
        ShuttingDown    // Program is shutting down, PERQ deconfiguring
    }

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

            InitStateMachine();
        }

        public PERQSystem System
        {
            get { return _system; }
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

        public void PowerOn()
        {
            Console.WriteLine("Power ON requested.");

            if (!PERQemu.Config.Current.IsValid)
            {
                Console.WriteLine("The system as configured is invalid and cannot start.");
                Console.WriteLine("Please check the configuration and try again.");
                return;
            }

            if (_system == null || PERQemu.Config.Changed)
            {
                // Out with the old
                _system = null;

                // In with the new?
                if (!Initialize(PERQemu.Config.Current))
                {
                    Console.WriteLine("Initialization failed.");
                    return;     // Fail.
                }

                // Load the configured storage devices
                _system.LoadAllMedia();
            }

            // (Re-)Start the machine
            _system.Run(RunState.WarmingUp);
        }

        public void Reset()
        {
            Console.WriteLine("ExecutionController Reset called.");

            // If no system, quietly no-op.
            if (State != RunState.Off)
            {
                TransitionTo(RunState.Reset);

                if (State == RunState.Paused && !Settings.PauseOnReset)
                {
                    TransitionTo(RunState.Running);
                }
            }
        }

        public void Break()
        {
            if (State != RunState.Off && State != RunState.Halted)
            {
                // Break out of the Running state
                TransitionTo(RunState.Paused);

                // No, really, I mean it
                SetState(RunState.Paused);
            }
        }

        public void PowerOff()
        {
            if (State == RunState.Off) return;

            Console.WriteLine("Power OFF requested.");

            // todo: get the sequence right!
            //      stop if running
            //      save disks? -- graphical dialog!?  consult Settings.*
            //      proceed to shutdown (stops SDL loop, closes display)
            //      then properly shut down and release the PERQsystem and
            //      ALL of its timers, handles, media, etc. -- so that a new
            //      machine can be configured OR the current one reinstantiated
            TransitionTo(RunState.Paused);
            _system.SaveAllMedia();
            TransitionTo(RunState.Off);
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
            var key = new SMKey(current, nextState);

            // Bail if already in the requested state
            if (current == nextState) return;

			Trace.Log(LogType.EmuState, "Controller requesting transition from {0} to {1}", current, nextState);

            // Now, how do we get there?  Let's consult the runes...
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
                                (current == RunState.Halted))
                            {
                                break;
                            }

                            Trace.Log(LogType.EmuState, "Waiting for {0}, got {1}...",
                                                        step.ExpectedResult, current);

                            // Pause and try again.  There should probably be
                            // a limit to our patience...
                            Thread.Sleep(10);

                        } while (step.WaitForIt);
                    }
                }
                else
                {
                    // fixme: this shouldn't happen if our state machine is complete... 
                    // so make this an error or just remove it once finally debugged
                    Console.WriteLine("What, bad steps!? Key {0} yeilded no value!", key);
                }
            }
            else
            {
                // throw new InvalidOperationException()
                Console.WriteLine("Sorry, ya cahnt get theyah from heeyah.");
            }

            Trace.Log(LogType.EmuState, "Controller transition result is {0}", State);
       }

        /// <summary>
        /// Initiate a state change in the current PERQsystem.
        /// </summary>
        private void SetState(RunState s)
        {
            // or do the Event-based way if/when the GUI and Debuggers need to be informed too
            _system?.Run(s);
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
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
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
                        new Transition(() => { SetState(RunState.Paused); }, RunState.Paused),
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                },
                {
                    new SMKey(RunState.Halted, RunState.Reset), new List<Transition>() {
                        new Transition(() => { SetState(RunState.Reset); }, RunState.Paused) }
                },
                {
                    new SMKey(RunState.Halted, RunState.Off), new List<Transition>() {
                        new Transition(() => { SetState(RunState.ShuttingDown); }, RunState.Off) }
                }
            };
        }

        // To encode our state machine transitions
        private Dictionary<SMKey, List<Transition>> _SMDict;

        private static byte _bootChar;
        private PERQSystem _system;
    }
}
