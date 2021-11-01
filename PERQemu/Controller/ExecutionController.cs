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
    /// Defines the run state of the virtual PERQ.  This hasn't been fully worked out.
    /// </summary>
    public enum RunMode
    {
        Off = 0,                // Power is off or no PERQ configured
        WarmingUp,              // Power is on and the GUI is warming up :-)
        Reset,                  // Held-in-reset state if PauseOnReset
        Run,                    // Run, run like the wind
        Pause,                  // User- or program-requested Pause
        Step,                   // Debugger is single stepping execution
        Inst,                   // Debugger is running one opcode
        Auto,                   // Debugger is updating on every cycle (slow)
        Halt,                   // Power off exception or grievous error
        CatchFire               // Program is shutting down, PERQ deconfiguring
    }

    /// <summary>
    /// Top-level control for managing the execution of a configured PERQ.
	/// It knows how to start, pause and stop the machine, as well as the
    /// debug modes(step, inst, auto).
    /// </summary>
    /// <design>
    /// Runs on the main thread, and simply manages several WaitHandles to
    /// block or unblock the running machine.  Calls may be made from the
    /// CLI or the GUI in response to typed commands or clicks from the
    /// Front Panel or Debugger (so this control is thread aware).
    /// 
    /// We only instantiate one controller, and attach/detach the configured
    /// PERQ to it.
    /// </design>
    public sealed class ExecutionController
    {

        public ExecutionController()
        {
            _system = null;
            _mode = RunMode.Off;
        }

        public void Initialize(Configuration conf)
        {
            //_system = new PERQSystem(conf);
            //Log.Write("ExecutionController: {0} initialized.", conf.Name);

            _system = new PERQSystem(conf);
        }

        public RunMode State
        {
            get { return _mode; }
            set { _mode = value; }      // this is wrong; need a method to sanity check things
        }

        public PERQSystem System
        {
            get { return _system; }
        }

        public void PowerOn()
        {
            if (_system == null)
            {
                Console.WriteLine("No PERQ defined!  Cannot power on.");
                return;
            }

            //if (!_system.Config.IsValid)
            //{
            //    Console.WriteLine("The system as configured is invalid and cannot start.");
            //    Console.WriteLine("Please check the configuration and try again.");
            //    return;
            //}

            if (_mode != RunMode.Off)
            {
                Console.WriteLine("The PERQ is already powered on.");
                return;
            }

            //Log.Write("ExecutionCtrl: Power ON requested.");

            
            //_system.Run(_mode);
        }

        public void Reset()
        {
            //Log.Write("ExecutionCtrl: System Reset requested.");
            _mode = RunMode.Reset;

            //PERQemu.Sys.Reset();

            // if (!Settings.PauseOnReset) { change back to Run mode; }
        }

        public void PowerOff()
        {
            //Log.Write("ExecutionCtrl: Power OFF requested.");
            //DetachSystem();
            _mode = RunMode.Off;
        }


        private RunMode _mode;
        private PERQSystem _system;

    }
}
