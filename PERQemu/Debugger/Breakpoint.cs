//
// Breakpoint.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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
using System.Collections.Generic;

using PERQemu.Processor;

namespace PERQemu.Debugger
{
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

    public class BreakpointEventArgs : EventArgs
    {
        public BreakpointEventArgs(BreakpointType bp, params object[] args)
        {
            Type = bp;
            Args = args;
        }

        public BreakpointType Type;
        public object[] Args;
    }

    public delegate void BreakpointEventHandler(BreakpointEventArgs a);


    public partial class PERQDebugger
    {
        //
        // Breakpoint support
        //

        /// <summary>
        /// Fired when an IO Address is referenced (system bus).
        /// </summary>
        public event BreakpointEventHandler IOPortBreakpoint;

        /// <summary>
        /// Fired when a microinstruction address is referenced.
        /// </summary>
        public event BreakpointEventHandler InstAddrBreakpoint;

        /// <summary>
        /// Fired when a microinstruction address is referenced.
        /// </summary>
        public event BreakpointEventHandler MemAddrBreakpoint;

        /// <summary>
        /// Fired when a system interrupt is raised or cleared.
        /// </summary>
        public event BreakpointEventHandler IRQBreakpoint;


        public void InitBreakpoints()
        {
            _ioWatchList = new Dictionary<byte, DebuggerAction>();
            _irqWatchList = new Dictionary<int, DebuggerAction>(); //bool[(int)InterruptSource.Parity + 1];
            _memWatchList = new Dictionary<uint, DebuggerAction>();
            _uinstWatchList = new Dictionary<ushort, DebuggerAction>();
        }


        public bool WatchedIRQ(InterruptSource i)
        {
            return _irqWatchList.ContainsKey((int)i);
        }

        public DebuggerAction GetActionForAddress(ushort addr)
        {
            return _uinstWatchList[addr];
        }

        public bool WatchedAddress(ushort addr)
        {
            return _uinstWatchList.ContainsKey(addr);
        }

        public void WatchAddress(ushort addr, DebuggerAction action)
        {
            _uinstWatchList[addr] = action;
        }

        public void UnwatchAddress(ushort addr)
        {
            if (_uinstWatchList.ContainsKey(addr))
            {
                _uinstWatchList.Remove(addr);
            }
        }


        public void BreakpointReached(BreakpointType bp, params object[] args)
        {
            BreakpointEventHandler handler = null;

            switch (bp)
            {
                case BreakpointType.WatchedInterrupt:
                    handler = IRQBreakpoint;
                    break;

                case BreakpointType.WatchedAddress:
                    handler = InstAddrBreakpoint;
                    break;

                default:
                    Log.Debug(Category.Emulator, "Breakpoint type {0} not yet implemented", bp);
                    return;
            }

            handler?.Invoke(new BreakpointEventArgs(bp, args));
        }


        private Dictionary<int, DebuggerAction> _irqWatchList;
        private Dictionary<byte, DebuggerAction> _ioWatchList;
        private Dictionary<uint, DebuggerAction> _memWatchList;
        private Dictionary<ushort, DebuggerAction> _uinstWatchList;
    }
}

/*
    Notes:

    This is way harder than it seems.  Breakpoints can't fuck up the normal
    execution flow; there has to be a way to trigger one then precisely resume
    the current instruction without blowing up the internal state.  Enabling a
    breakpoint may have to force synchronous execution?  And the "event" may
    not actually fire, but really just dispatch and return like a subroutine
    call?

    Would be nice to have a "fire once" vs. "fire every time", which would be
    very handy in the "press the boot key" scenario given how the DDS jumps
    all over the place (loops around) during the POS G boot (ugh).

    debugcommands:
        ui for setting up the list of requests:
            adds/removes/modifies breakpoints
            enables/disables them individually/en masse

    debugger:
        breakpoints actually execute here
        coordinates with controller to shift execution into/out of synch mode?
        
    cpu/z80/devices:
        watches for and triggers breakpoints
        queries the debugger for trigger info (fire = askDebugger(someVal))
        can't interrupt cycle; takes back breakpointaction.pause boolean?
        returns it from Execute() to set state
        minimizes disruption to flow since the instruction completes?
        
    perqsystem/z80system:
        if cpu.execute() signals a breakpoint has fired, set state to paused;
        the z80 will automatically pause regardless of mode (setstate will catch
        the cpu n cycles later when z80 breakpoints implemented?)
        
        move _back_ to the dedicated loop/idle mode; power on actually starts
        up the async thread, ALL messages dispatched through events, only
        exits on power off...

        async mode we run the execution loops directly; sync mode we have the
        main thread schedule both?

    SET UP THE BLOODY PC SO YOU CAN TEST THIS ON WINDOWS AND LINUX, YA BOOB.



    cpu/z80 have to be able to quickly check against a list of enabled triggers
        if (_debugger.QuicklyCheckCondition(thing))
            stopOrNot = _debugger.TriggerCondition(type, thing)

    so the fast method needs to be specific to what we're checking
    firing the actual trigger is slower, obviously... 

    checking the condition has to match the particular thing (irq #, uinst addr,
    mem loc, etc) against the debugger action?  don't trigger if not enabled,
    or if set to fire once and already has fired...

    checkIRQ = _irqlist.containskey(thing)
    
*/