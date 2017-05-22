// queuecontroller.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;

using PERQemu.CPU;

namespace PERQemu.Memory
{
    public enum MemoryState
    {
        Idle = 0,
        WaitForT3,
        WaitForT2,
        Running
    }

    /// <summary>
    /// Represents a request to the memory subsystem.
    /// </summary>
    public class MemoryRequest
    {
        public MemoryRequest(MemoryCycle type, int addr, long id)
        {
            CycleType = type;
            StartAddress = addr;
            RequestID = id;
            Recognized = false;
            Complete = false;
        }

        public override string ToString()
        {
            return String.Format("ID={0} addr={1:x5} cycle={2} entered={3} {4}",
                RequestID, StartAddress, CycleType, Recognized, Complete ? "Done" : "");
        }

        public MemoryCycle CycleType;
        public int StartAddress;
        public long RequestID;
        public bool Recognized;
        public bool Complete;
    }

    /// <summary>
    /// Represents one element in the memory fetch/store queues.  Each instruction
    /// is scheduled to execute in a particular T-state.
    /// </summary>
    public class MemoryInstruction
    {
        public MemoryInstruction(MemoryRequest req, int addr, int index, int when, bool valid, bool done)
        {
            Requester = req;
            Address = addr;
            WordIndex = index;
            ExecuteTime = when;
            Valid = valid;
            Complete = done;
        }

        public override string ToString()
        {
            return String.Format("ID={0} addr={1:x5} index={2} time=T{3} valid={4} {5}",
                Requester.RequestID, Address, WordIndex, ExecuteTime, Valid, Complete ? "Done" : "");
        }

        public MemoryRequest Requester;     // MemoryRequest that issued this instruction
        public int Address;                 // Actual address for this word (pre-computed)
        public int WordIndex;               // Which word in a multi-word cycle (simplifies LoadOp)
        public int ExecuteTime;             // Tstate to actually execute this fetch/store
        public bool Valid;                  // Is this slot just a placeholder?
        public bool Complete;               // True for last word (so we can retire the request)
    }

    /// <summary>
    /// Object to keep track of the state of both Fetch and Store queues,
    /// generating CPU Wait (and IO Hold -- eventually) states and managing
    /// the memory instruction pipelines.
    /// </summary>
    public class QueueController
    {
        public QueueController(string name)
        {
            _reqQueue = new Queue<MemoryRequest>(4);
            _instQueue = new Queue<MemoryInstruction>(12);
            _name = name;

            Reset();
        }

        public void Reset()
        {
            _reqQueue.Clear();
            _instQueue.Clear();

            _state = _nextState = MemoryState.Idle;
            _cycle = MemoryCycle.None;
            _wait = false;
            _valid = false;
            _address = 0;
            _index = 0;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "{0} queue: Reset.", _name);
#endif
        }

        public bool Wait
        {
            get { return _wait; }
        }

        public bool Valid
        {
            get { return _valid; }
        }

        public MemoryState CurrentState
        {
            get { return _state; }
        }

        public MemoryCycle CurrentCycle
        {
            get { return _cycle; }
        }

        public int Address
        {
            get { return _address; }
        }

        public int WordIndex
        {
            get { return _index; }
        }

        public int Count
        {
            get { return _reqQueue.Count; }
        }

        private int Tstate
        {
            get { return MemoryBoard.Instance.TState; }     // too lazy to type this over and over
        }

        private int NextTstate(int t)
        {
            return (t + 1) & 0x3;
        }

        public void Request(MemoryCycle cycleType, int startAddr, long id)
        {
            _cycle = cycleType;
            _nextState = (cycleType == MemoryCycle.Store ? MemoryState.WaitForT2 : MemoryState.WaitForT3);

            MemoryRequest r = new MemoryRequest(cycleType, startAddr, id);
            _reqQueue.Enqueue(r);

            Recognize();
        }

        /// <summary>
        /// Clocks the memory queue state machine. Retires completed requests, schedules new
        /// ones, and runs the next instruction in the pipeline.
        /// </summary>
        public void Clock()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MemoryState,
                         "{0} queue: Clock T{1} state={2} next={3} reqQ={4} instQ={5}",
                         _name, Tstate, _state, _nextState, _reqQueue.Count, _instQueue.Count);
#endif
            // Retire any requests completed in the previous cycle
            Retire();

            // Update state
            RunStateMachine();

            // Check for any new requests that may be ready to run
            Recognize();

            // Run the instruction queue
            RunInstQueue();
        }

        /// <summary>
        /// Update the current state of the controller.
        /// </summary>
        private void RunStateMachine()
        {
            _state = _nextState;

            switch (_state)
            {
                case MemoryState.Idle:
                case MemoryState.Running:
                    _wait = false;
                    break;

                case MemoryState.WaitForT3:
                    _wait = true;
                    break;

                case MemoryState.WaitForT2:
                    if (MemoryBoard.Instance.IsFetch(_cycle))
                    {
                        //
                        // If this queue is processing Fetches, we have to advance to the Running
                        // state here and release Wait.  Recognize() drops us into T2 wait when
                        // new Fetches are recognized; it will also invalidate outstanding Fetch
                        // T0/T1 cycles in the case of overlaps (so no check here anymore).
                        //
                        if (Tstate == 2) _nextState = MemoryState.Running;
                        _wait = false;
                    }
                    else
                    {
                        //
                        // If we're waiting on a Store, really wait.  If it's any other type,
                        // that's not supposed to happen... should throw an exception?
                        //
                        if (_cycle != MemoryCycle.Store)
                            Console.WriteLine("\t** How did a {0} cycle end up in WaitForT2!?", _cycle);
                        _wait = true;
                    }
                    break;
            }
        }

        /// <summary>
        /// Runs the next instruction in the pipeline, if any.  Otherwise, no-op.
        /// </summary>
        private void RunInstQueue()
        {
            _valid = false;

            // If this is the correct cycle, run the instruction
            if (_instQueue.Count > 0)
            {
                MemoryInstruction m = _instQueue.Peek();
                if (m.ExecuteTime == Tstate)
                {
                    // Yup, go for it
                    m = _instQueue.Dequeue();
                    _address = m.Address;
                    _index = m.WordIndex;
                    _valid = m.Valid;

                    // Last word?  Mark request as complete
                    if (m.Complete)
                    {
                        m.Requester.Complete = true;
                    }
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "{0} queue: Executed {1}", _name, m);
#endif
                }
            }
        }

        /// <summary>
        /// Scans request queue for a new request that's ready to run, and schedules
        /// instructions to fulfill it.  If appropriate, sets the next memory state.
        /// </summary>
        private void Recognize()
        {
            MemoryInstruction m;
            int i, a, t;

            // If the request queue is empty, don't bother setting up the loop
            if (_reqQueue.Count > 0)
            {
                Queue<MemoryRequest>.Enumerator e = _reqQueue.GetEnumerator();

                while (e.MoveNext())
                {
                    MemoryRequest req = e.Current;

#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "{0} queue: Scanning {1}", _name, req);
#endif
                    // If this req is not yet "recognized," see if it's the correct cycle
                    if (!req.Recognized)
                    {
                        switch (req.CycleType)
                        {
                            case MemoryCycle.Fetch:
                                //
                                // All Fetch types all start in T3, providing the first result word in
                                // the following T2.  The single-word Fetch is slightly special; it provides
                                // a valid MDI for four full cycles, not just one.
                                //
                                if (Tstate == 3)
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.WaitForT2;

                                    InvalidatePreviousFetch();

                                    // Queue up the next read cycle
                                    for (i = 0, t = 1; i <= 3; i++, t++)
                                    {
                                        // convert mi to struct, then inline these? avoid constructor overhead?
                                        m = new MemoryInstruction(req, req.StartAddress, 0, NextTstate(t), true, i == 3 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Fetch2:
                                //
                                // Fetch2 provides valid MDI for two cycles.  Addresses are double-word
                                // aligned.  Results are "undefined" if an access to MDI/MDX is made after
                                // the first two words are read.
                                //
                                if (Tstate == 3)
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.WaitForT2;

                                    InvalidatePreviousFetch();

                                    for (i = 0, a = req.StartAddress & 0xffffe, t = 1; i <= 3; i++, a++, t++)
                                    {
                                        m = new MemoryInstruction(req, a, i, NextTstate(t),
                                                                          i <= 1 ? true : false,
                                                                          i == 3 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Fetch4:
                                //
                                // Fetch4 starts in T3 like any other Fetch type; addresses are quad-word aligned,
                                // MDI is valid for four cycles, starting in T2.  May overlap during RasterOp
                                // with Store4.
                                //
                                if (Tstate == 3)
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.WaitForT2;

                                    InvalidatePreviousFetch();

                                    for (i = 0, a = req.StartAddress & 0xffffc, t = 1; i <= 3; i++, a++, t++)
                                    {
                                        m = new MemoryInstruction(req, a, i, NextTstate(t), true, i == 3 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Fetch4R:
                                //
                                // Fetch4R is the same as Fetch4, but transports the words in reverse
                                // order.  May overlap during RasterOps with a Store4R.
                                //
                                if (Tstate == 3)
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.WaitForT2;

                                    InvalidatePreviousFetch();

                                    for (i = 3, a = (req.StartAddress & 0xffffc) + 3, t = 1; i >= 0; i--, a--, t++)
                                    {
                                        m = new MemoryInstruction(req, a, i, NextTstate(t), true, i == 0 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Store:
                                //
                                // Single-word Store is a special case: starts in T2, not T3.  MDO required
                                // for one cycle.
                                //
                                if (Tstate == 2)
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.Running;

                                    for (i = 0, t = Tstate; i <= 3; i++, t++)
                                    {
                                        // MDO needed for one cycle; rest ignored
                                        m = new MemoryInstruction(req, req.StartAddress, i, NextTstate(t),
                                                                                         i == 0 ? true : false,
                                                                                         i == 3 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Store2:
                                //
                                // Store2 starts in T3.  Addresses are double-word aligned.  MDO required
                                // for two cycles.
                                //
                                if (Tstate == 3)
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.Running;

                                    for (i = 0, a = req.StartAddress & 0xffffe, t = Tstate; i <= 3; i++, a++, t++)
                                    {
                                        m = new MemoryInstruction(req, a, i, NextTstate(t),
                                                                          i <= 1 ? true : false,
                                                                          i == 1 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Store4:
                                //
                                // Store4 normally happens at T3, but if RasterOp is enabled, can execute
                                // in T0 and may overlap with a Fetch4.  We'll set these up without holds in
                                // either T3 or T0, and assume no waits (though it's confusing in the docs).
                                //
                                // Addresses are quad-word aligned.  MDO required for four cycles.
                                //
                                if (Tstate == 3 || (RasterOp.Instance.Enabled && Tstate == 0))
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.Running;

                                    for (i = 0, a = req.StartAddress & 0xffffc, t = Tstate; i <= 3; i++, a++, t++)
                                    {
                                        m = new MemoryInstruction(req, a, i, NextTstate(t), true, i == 3 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            case MemoryCycle.Store4R:
                                //
                                // Store4R follows the same rules as Store4, but transfers the results in
                                // reverse order.  May overlap during RasterOp with Fetch4R.
                                //
                                if (Tstate == 3 || (RasterOp.Instance.Enabled && Tstate == 0))
                                {
                                    req.Recognized = true;
                                    _nextState = MemoryState.Running;

                                    for (i = 3, a = (req.StartAddress & 0xffffc) + 3, t = Tstate; i >= 0; i--, a--, t++)
                                    {
                                        m = new MemoryInstruction(req, a, i, NextTstate(t), true, i == 0 ? true : false);
                                        _instQueue.Enqueue(m);
#if TRACING_ENABLED
                                        if (Trace.TraceOn)
                                            Trace.Log(LogType.MemoryState, "{0} queue: Scheduled {1}", _name, m);
#endif
                                    }
                                }
                                break;

                            default:
                                throw new CPU.UnimplementedInstructionException(
                                    String.Format("Unimplemented memory cycle type {0}", _cycle));
                        }
                    }
                }

                // Clean up enumerator
                e.Dispose();
            }
        }

        /// <summary>
        /// Marks the next T0/T1 slot in the currently executing Fetch as invalid, or
        /// a no-op if no cycle in progress.  This is to handle the hugely annoying
        /// case of an overlapped Fetch request where we have to release the CPU wait
        /// but want to avoid giving back stale data (from the previous Fetch).  Sigh.
        /// </summary>
        private void InvalidatePreviousFetch()
        {
            if (_reqQueue.Count > 1)
            {
                MemoryRequest r = _reqQueue.Peek();

                // Probably being overly cautious here, but make sure the current
                // op is a recognized Fetch, and zap its T0/T1...
                if (r.CycleType == MemoryCycle.Fetch && _instQueue.Count > 0)
                {
                    MemoryInstruction[] a = _instQueue.ToArray();

                    for (int i = 0; i < _instQueue.Count; i++)
                    {
                        if ((a[i].ExecuteTime == 0 || a[i].ExecuteTime == 1) && a[i].Requester == r)
                        {
                            a[i].Valid = false;
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.MemoryFetch, "{0} queue: Invalidated: {1}", _name, a[i]);
                        }
                        else
                        {
                            if (Trace.TraceOn) Trace.Log(LogType.MemoryFetch, "{0} queue: Still valid: {1}", _name, a[i]);
#endif
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Removes completed memory requests from the queue.
        /// </summary>
        private void Retire()
        {
            bool done = false;

            //
            // Pull completed entries from the front of the FIFO.  In some cases (i.e., RasterOp)
            // the requests might finish out-of-order; no harm in waiting a few cycles (the queue
            // is never more than 2-3 requests long, typically).  If we retire the last request,
            // set our state to Idle.
            //
            while (_reqQueue.Count > 0 && !done)
            {
                MemoryRequest r = _reqQueue.Peek();
                if (r.Complete)
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "{0} queue: Retiring {1}", _name, r);
#endif
                    _reqQueue.Dequeue();
                }
                else
                {
                    done = true;
                }
            }

            // If we've retired the last active op, idle the state machine
            if (_reqQueue.Count == 0)
            {
                _nextState = MemoryState.Idle;
                _cycle = MemoryCycle.None;
            }
        }

        private string _name;
        private Queue<MemoryRequest> _reqQueue;
        private Queue<MemoryInstruction> _instQueue;
        private MemoryState _state;
        private MemoryState _nextState;
        private MemoryCycle _cycle;
        private bool _wait;
        private bool _valid;
        private int _address;
        private int _index;
    }
}

// ---- notes ----

#region Hairy memory rules
//
// The real PERQ memory rules are seriously hairy.  To make matters much worse, the
// wording in the Microprogrammers' Guide is terribly confusing.  So I'm not sure what
// will break if I try to implement them faithfully... but here goes:
//
// 1. For any Fetch executed in T3, any memory reference in T0 or T1 is ignored, EXCEPT:
//  - a Store in T2 will start immediately          [Uh, we're talking about T0/T1 here??]
//  - a Store4 or Store4R can be specified in T0    [Will stall until T2? Or need MDO in T1?]
// All others will abort until the correct cycle.   [But.. you said "ignored" above. What?]

// 2. After a Store in T2, any memory reference in T3 or T0 is ignored, but
// refs started in T1 are aborted until the correct cycle.

// 3. After a Store2/4/4R in T3, any reference in the next 4 cycles is ignored.
// But references started in T0 are aborted until the correct cycle.  [Uh, "ignored"?  You
// keep using that word. I do not think it means what you think it means...]

// Hold must be asserted in T2 - does PERQemu care?  We never worry about IO contention...

// After a Fetch, MDI is valid from T2 to the following T1; all other Fetches supply
// one word for a single microcycle.

// cur state    cur op  rqst        tcycle      ignore      next state      _wait
//    idle      none    fetch*      t0..2       no          waitfort3       yes
//    idle      none    fetch*      t3          no          busy            no
//    idle      none    store       !t2         no          waitfort2       yes
//    idle      none    store       t2          no          busy            no
//    idle      none    store<n>    !t3         no          waitfort3       yes
//    idle      none    store<n>    t3          no          busy            no
//   !idle      fetch*  any         t0, t1      yes         no change       same    (rule 1)
//   !idle      fetch*  store       t2          no          no change!?     uh...   (rule 1a)
//   !idle      fetch*  store4/4r   t0          no          overlap         no?     (rule 1b)
//   !idle      store   any         t3, t0      yes         no change       same    (rule 2)
//   !idle      store   any         t1          no          wait            yes?    (rule 2a)
//   !idle      store<n> any        t1..3       yes         no change       same    (rule 3)
//   !idle      store<n> any        t0          no          wait            wait?   (rule 3a)

//// If we're already doing something see if a new request will violate the timing rules.
//if (_state != MemoryState.Idle)
//{
//    if (IsFetch(_cycle))
//    {
//        // If current cycle is any Fetch type, ignore all references in T0 or T1, EXCEPT:
//        //      A Store4 or Store4R in T0 is allowed IF
//        //      the current memory transfer is a Fetch4/4R
//        //      in the same direction (RasterOp depends on this)
//        ignore = (_Tstate == 1 ||
//                 (_Tstate == 0 &&
//                 ((_cycle == MemoryCycle.Fetch4 && request != MemoryCycle.Store4) ||
//                 (_cycle == MemoryCycle.Fetch4R && request != MemoryCycle.Store4R))));    // Oooh, Lispy!
//    }
//    else
//    {
//        // If current cycle is a Store, ignore all memory references for four cycles, EXCEPT:
//        //      References started in T1 (will wait for appropriate cycle);
//        // If current cycle is a multi-word Store type, ignore all references EXCEPT:
//        //      Any multi-word Store is allowed in T0 (will wait for T3)
//        ignore = ((_cycle == MemoryCycle.Store && (_Tstate == 3 || _Tstate == 0) ||
//                  (_cycle != MemoryCycle.Store && _Tstate != 0)));
//    }
//}
//
//if (ignore)
//{
//#if TRACING_ENABLED
//      if (Trace.TraceOn)
//          Trace.Log(LogType.MemoryState, "Memory: IGNORED! T{0} state={1} cycle={2} request={3}", _Tstate, _state, _cycle, request);
//#endif
//}
#endregion
