//
// Events.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

    public class RunStateChangeEventArgs : EventArgs
    {
        public RunStateChangeEventArgs(RunState s)
        {
            _state = s;
        }

        public RunState State
        {
            get { return _state; }
        }

        private RunState _state;
    }

    // Make Breakpoints an event of their own, don't nest or complicate further?
    public enum BreakpointType
    {
        None = 0,
        WatchedIOPort,      // IOA for PERQ, port for Z80
        WatchedOpCode,      // Qcode for PERQ, opcode for Z80
        WatchedAddress,     // microaddr for PERQ, Z80 RAM
        WatchedMemoryLoc,   // main memory address for PERQ (or DMA)
        WatchedInterrupt,   // break on interrupt raise (PERQ or Z80)
        AndSoForth
    }

    public enum WhatChanged
    {
        Nothing = 0,
        DDSChanged,         // For UI update, boot key
        BreakpointReached,  // triggers a run state change...
        PowerOffRequested,  // Perq 1 only
        UncaughtException,  // Kaboom
        Other
    }

    public class MachineStateChangeEventArgs : EventArgs
    {
        public MachineStateChangeEventArgs(WhatChanged w, params object[] args)
        {
        }
    }

    public delegate void RunStateChangeEventHandler(RunStateChangeEventArgs a);
    public delegate void MachineStateChangeEventHandler(MachineStateChangeEventArgs a);
}
