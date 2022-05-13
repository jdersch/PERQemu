//
// Events.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu
{
    /// <summary>
    /// Defines the run state of the virtual PERQ.
    /// </summary>
    public enum RunState
    {
        Unavailable = -1,   // No PERQ is configured
        Off = 0,            // Machine defined; power is off
        WarmingUp = 1,      // Power is on and the GUI is warming up :-)
        Reset,              // Transitional reset state
        Paused,             // User- or program-requested Pause
        Running,            // Run, run like the wind
        SingleStep,         // Debugger is single stepping execution
        RunInst,            // Debugger is running one opcode
        RunZ80Inst,         // Debugger is running one Z80 opcode
        Halted,             // Exception or grievous error stopped execution
        ShuttingDown        // The PERQ is shutting down, will power off
    }

    public class RunStateChangeEventArgs : EventArgs
    {
        public RunStateChangeEventArgs(RunState s)
        {
            State = s;
        }

        public RunState State;
    }

    /// <summary>
    /// Enumerate the types of machine state changes we can provide.  Most of
    /// these seem irrelevant unless/until a GUI is built... 
    /// </summary>
    public enum WhatChanged
    {
        Nothing = 0,
        DDSChanged,         // For UI update, boot key
        HaltedInLoop,       // Microcode halted in loop
        HardDiskActivity,   // For blinkenlights
        FloppyActivity,     // Sure, why not
        PowerDown,          // PERQ 1 software power off
        Other
    }

    public class MachineStateChangeEventArgs : EventArgs
    {
        public MachineStateChangeEventArgs(WhatChanged w, params object[] args)
        {
            Changed = w;
            Args = args;
        }

        public WhatChanged Changed;
        public object[] Args;
    }

    public delegate void RunStateChangeEventHandler(RunStateChangeEventArgs a);
    public delegate void MachineStateChangeEventHandler(MachineStateChangeEventArgs a);
}
