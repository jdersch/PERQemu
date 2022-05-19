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
    /// <summary>
    /// General types of breakpoints that both the PERQ and Z80 debuggers may
    /// implement.  (The Z80dotNet hooks aren't really useful to us and that
    /// whole thing may be replaced.)
    /// </summary>
    public enum BreakpointType
    {
        None = 0,
        IOPort,         // IOA for PERQ, port for Z80
        OpCode,         // Qcode for PERQ, opcode for Z80
        uAddress,       // Microaddr for PERQ, Z80 RAM
        MemoryLoc,      // Main memory address for PERQ (or DMA)
        Interrupt,      // Break on interrupt (PERQ or Z80)
        All             // For CLI convenience
    }

    /// <summary>
    /// Breakpoint events always include a type and the value that triggered it;
    /// other optional args may convey additional state.
    /// </summary>
    public class BreakpointEventArgs : EventArgs
    {
        public BreakpointEventArgs(BreakpointType bp, int val, params object[] args)
        {
            Type = bp;
            Value = val;
            Args = args;
        }

        public BreakpointType Type;
        public int Value;
        public object[] Args;
    }

    public delegate void BreakpointEventCallback(BreakpointEventArgs a);

    /// <summary>
    /// Manage a list of breakpoints of a particular type and associated debugger
    /// actions when they fire.  Also provides a "master switch" that lets them
    /// be enabled or disabled all at once.
    /// </summary>
    /// <remarks>
    /// Rather than use generics (which are cool, but in this case are overkill
    /// and needlessly add complexity) just pass in a limit to let the debugger
    /// do basic range checking.  Note that the limit isn't enforced here; the
    /// command parser should handle that, but if a breakpoint is set on a value
    /// that's never reached it just won't ever fire.
    /// </remarks>  
    public class BreakpointList
    {
        public BreakpointList(BreakpointType type, string name, int limit)
        {
            _type = type;
            _name = name;
            _limit = limit;
            _list = new Dictionary<int, BreakpointAction>();

            Log.Debug(Category.Debugger, "Initialized {0} breakpoints (lim {1})", name, limit);
        }

        public BreakpointType Type => _type;
        public string Name => _name;
        public int Range => _limit;
        public int Count => _list.Count;

        public bool IsWatched(int val)
        {
            return _list.ContainsKey(val);
        }

        public BreakpointAction GetActionFor(int val)
        {
            if (_list.ContainsKey(val))
                return _list[val];

            // Better than returning null?  Meh.
            return new BreakpointAction();
        }

        public void Watch(int val, BreakpointAction action)
        {
            if (_list.ContainsKey(val))
                _list[val] = action;
            else
                _list.Add(val, action);
        }

        public void Unwatch(int val)
        {
            if (_list.ContainsKey(val))
                _list.Remove(val);
        }

        /// <summary>
        /// Process a breakpoint according to the defined actions specified by
        /// the user.  NB: Only tracks invocations and fires the events; does
        /// not pause the emulator or run scripts -- that's up to whomever is
        /// catching the events!
        /// </summary>
        /// <remarks>
        /// val is always the watched address/port that we triggered on; other
        /// parameters are optional.
        /// </remarks>
        public bool BreakpointReached(int val, params object[] args)
        {
            if (!PERQemu.Sys.Debugger.BreakpointsEnabled) return false;

            BreakpointAction action;

            if (!_list.ContainsKey(val))
            {
                Log.Warn(Category.Debugger, "No action for breakpoint {0}, value {1}", _type, val);
                return false;
            }

            action = _list[val];

            // Should we fire?
            if (action.Enabled && (action.Count == 0 || action.Retriggerable))
            {
                action.Count++;
                Log.Detail(Category.Debugger, "Fired action count now {0}", action.Count);

                // Ugh!  We don't actually want to fire this yet; if pausing the
                // emulator we need to complete the current cycle, change run state,
                // fire the handler, then (if a script is defined) let that execute.
                // The Debugger should queue up "pending" breakpoints and provide a
                // callback to invoke them?
                if (action.Callback != null)
                {
                    action.Callback(new BreakpointEventArgs(_type, val, args));
                }

                return action.PauseEmulation;
            }

            // If we didn't fire, don't pause...
            return false;
        }

        public string FmtVal(int val)
        {
            // I'll have mine with extra cheese
            if (_type == BreakpointType.Interrupt)
            {
                return string.Format("{0} ({1})", val, (InterruptSource)val);
            }

            return string.Format("0x{0:x} ({1})", val, val);
        }

        public void ShowActions()
        {
            if (_list.Count == 0) return;

            Console.WriteLine("  Watched value    Enabled  Pause  OneShot  Count         Script");
            //               ("12345678901234567  1234567  12345  1234567  123456789012  ...");

            foreach (var key in _list.Keys)
            {
                Console.WriteLine("{0,-17}  {1,-7}  {2,-5}  {3,-7}  {4,-12}  {5}", FmtVal(key),
                                  _list[key].Enabled,
                                  _list[key].PauseEmulation,
                                  _list[key].Retriggerable,
                                  $"{_list[key].Count} times",
                                  string.IsNullOrEmpty(_list[key].Script) ? "<none>" : _list[key].Script);
            }
        }

        private BreakpointType _type;
        private string _name;
        private int _limit;
        private Dictionary<int, BreakpointAction> _list;
    }

    /// <summary>
    /// Describes the actions the Debugger should take when a breakpoint fires
    /// or certain machine state changes occur.  May include a callback or not;
    /// if none provided the default is to simply log that the breakpoint was
    /// triggered.
    /// </summary>
    public class BreakpointAction
    {
        /// <summary>
        /// Default action: don't pause emulation, retrigger, no script, no callback.
        /// </summary>
        public BreakpointAction()
        {
            Count = 0;
            Enabled = true;
            PauseEmulation = false;
            Retriggerable = true;
            Script = string.Empty;
            Callback = null;
        }

        public BreakpointAction(bool pause = true, bool once = false, string script = "") : this()
        {
            PauseEmulation = pause;
            Retriggerable = once;
            Script = script;
        }

        public BreakpointAction(BreakpointEventCallback cb) : this()
        {
            Callback = cb;
        }

        public BreakpointEventCallback Callback;
        public bool Enabled;
        public bool PauseEmulation;
        public bool Retriggerable;
        public string Script;
        public int Count;
    }


    //
    // Breakpoint support
    //
    public partial class PERQDebugger
    {
        /// <summary>
        /// Set up separate watch lists for breakpoints.  This lets clients subscribe
        /// to just the ones they want.
        /// </summary>
        public void InitBreakpoints()
        {
            _ioWatchList = new BreakpointList(BreakpointType.IOPort, "IO Port", 255);
            _irqWatchList = new BreakpointList(BreakpointType.Interrupt, "CPU Interrupt", (int)InterruptSource.Parity);
            _memWatchList = new BreakpointList(BreakpointType.MemoryLoc, "Memory Address", PERQemu.Config.Current.MemorySizeInBytes / 2);
            _uinstWatchList = new BreakpointList(BreakpointType.uAddress, "Microaddress", CPU.WCSSize);
        }

        public void EnableBreakpoints(bool enab)
        {
            _masterEnable = enab;

            Console.WriteLine("Breakpoints {0}.", enab ? "enabled" : "disabled");
        }

        public bool BreakpointsEnabled => _masterEnable;

        public BreakpointList WatchedInterrupts => _irqWatchList;
        public BreakpointList WatchedIOPorts => _ioWatchList;
        public BreakpointList WatchedMemoryAddress => _memWatchList;
        public BreakpointList WatchedMicroaddress => _uinstWatchList;


        private bool _masterEnable;

        private BreakpointList _irqWatchList;
        private BreakpointList _ioWatchList;
        private BreakpointList _memWatchList;
        private BreakpointList _uinstWatchList;
    }
}
