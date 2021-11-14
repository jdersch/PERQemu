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
        Reset,          // Held-in-reset state if PauseOnReset
        Paused,         // User- or program-requested Pause
        Running,        // Run, run like the wind
        SingleStep,     // Debugger is single stepping execution
        RunInst,        // Debugger is running one opcode
        RunZ80Inst,     // Debugger is running one Z80 opcode
        Halted,         // Power off exception or grievous error
        ShuttingDown    // Program is shutting down, PERQ deconfiguring
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

        public RunState State
        {
            get { return (_system == null ? RunState.Off : _system.State); }
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

        public PERQSystem System
        {
            get { return _system; }
        }

        public void Initialize(Configuration conf)
        {
            _system = new PERQSystem(conf);
        }

        public void PowerOn()
        {
            if (_system != null && _system.State != RunState.Off)
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

            if (PERQemu.Config.Current.IsModified)
            {
                _system = null;     // release?
                Initialize(PERQemu.Config.Current);
            }

            _system.State = RunState.WarmingUp;
            _system.Execute();
        }

        public void Reset()
        {
            // If no system, quietly no-op (since we always pass
            // through Reset when bringing the system up)
            if (_system != null && _system.State != RunState.Off)
            {
                _system.State = RunState.Reset;
                _system.Execute();
            }
        }

        public void PowerOff()
        {
           if (_system == null || _system.State == RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered off.");
                return;
            }

            // todo:
            //      stop if running
            //      save disks? -- graphical dialog!?  consult Settings.*
            //      proceed to shutdown (stops SDL loop, closes display)

            Console.WriteLine("Power OFF requested.");
            _system.State = RunState.ShuttingDown;
            _system.Execute();
        }

        public RunState TransitionTo(RunState nextState)
        {
            var current = _system.State;

            Console.WriteLine("Transition request from {0} to {1}", current, nextState);

            // Are we already in the requested state?
            if (current == nextState) return current;

            // Now, how do we get theyah from heeyah?
            // this gets real stupid real fast
            if (current == RunState.Off)
                PowerOn();

            _system.State = nextState;
            _system.Execute();

            return PERQemu.Controller.State;
        }

        private static byte _bootChar;

        private PERQSystem _system;
    }
}
