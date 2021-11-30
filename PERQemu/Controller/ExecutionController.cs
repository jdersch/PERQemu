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

    /// <summary>
    /// Top-level control for managing the execution of a configured PERQ.
	/// It knows how to start, pause and stop the machine, as well as the
    /// debug modes(step, inst, auto).
    /// </summary>
    public sealed class ExecutionController
    {

        public ExecutionController()
        {
            _system = null;
            _bootChar = 0;
        }

        public PERQSystem System
        {
            get { return _system; }
        }

        public RunState State
        {
            get { return (_system == null ? RunState.Off : _system.State); }
            set { if (_system != null) _system.State = value; }
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
                return true;
            }
            catch (Exception e)
            {
                Console.WriteLine("The system could not be initialized:");
                Console.WriteLine(e.Message);
                Console.WriteLine("Please check the configuration and try again.");
                return false;
            }
        }

        public void PowerOn()
        {
            if (State != RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered on.");
                return;
            }

            if (!PERQemu.Config.Current.IsValid)
            {
                Console.WriteLine("The system as configured is invalid and cannot start.");
                Console.WriteLine("Please check the configuration and try again.");
                return;
            }

            Console.WriteLine("Power ON requested.");

            // FIXME: it seems stupid to initialize the "default" perq (or the
            // last saved config, then immediately throw it out and recreate it
            // (as in the case of a startup script that loads and goes).  trust
            // the "changed" or "modified" flag in the configurator and/or add
            // a method that can cleanly free up resources rather than just let
            // the runtime take care of it...?  oof.
            // Out with the old
            _system = null;

            // In with the new
            if (Initialize(PERQemu.Config.Current))
            {
                // Load the configured storage devices
                _system.LoadAllMedia();
                _system.Run(RunState.WarmingUp);
            }
        }

        public void Reset()
        {
            // If no system, quietly no-op (since we always pass
            // through Reset when bringing the system up)
            if (State != RunState.Off)
            {
                State = RunState.Reset;

                if (!Settings.PauseOnReset)
                {
                    State = RunState.Running;
                }
            }
        }

        public void PowerOff()
        {
            if (State == RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered off.");
                return;
            }

            // todo:
            //      stop if running
            //      save disks? -- graphical dialog!?  consult Settings.*
            //      proceed to shutdown (stops SDL loop, closes display)

            Console.WriteLine("Power OFF requested.");
            State = RunState.ShuttingDown;
            // fixme wait for sync here!!
        }

        /// <summary>
        /// Change the state of the virtual machine.  If the RunMode is
        /// synchronous, blocks until the execution stops (pause, fault,
        /// or shutdown).  In asynchronous mode, returns the state when
        /// all threads have registered the new state.
        /// </summary>
        /// <remarks>
        /// If the system is not powered on, TransitionTo will first execute
        /// the PowerOn sequence; similarly, a PowerOff request will initiate
        /// a PowerOff.  Any exceptions thrown will result in a Halted state,
        /// or an Off state if the machine cannot be instantiated.  See the
        /// file Docs/Controller.txt for more details.
        /// </remarks>
        public RunState TransitionTo(RunState nextState)
        {
            var current = State;

            // todo Trace.Log()
            Console.WriteLine("Transition request from {0} to {1}", current, nextState);

            // Are we already in the requested state?
            if (current == nextState) return current;

            // Now, how do we get theyah from heeyah?
            // this gets real stupid real fast
            if (current == RunState.Off)
                PowerOn();

            State = nextState;

            return State;
        }

        private static byte _bootChar;
        private PERQSystem _system;
    }
}
