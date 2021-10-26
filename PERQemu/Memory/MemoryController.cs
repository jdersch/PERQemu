// memorycontroller.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.IO;
using System.Runtime.CompilerServices;
using PERQemu.Processor;

namespace PERQemu.Memory
{

    /// <summary>
    /// Represents a request to the memory subsystem.
    /// </summary>
    public class MemoryRequest
    {
        public MemoryRequest()
        {
            Clear();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Clear()
        {
#if DEBUG
            _reqID = -1;
#endif
            _startAddr = -1;
            _cycleType = MemoryCycle.None;
            _bookmark = 0;
            _active = false;
        }

        public override string ToString()
        {
#if DEBUG
            return String.Format("ID={0} addr={1:x6} cycle={2} bookmark={3} active={4}",
                                 _reqID, _startAddr, _cycleType, _bookmark, _active);
#else
            return String.Format("Addr={0:x6} cycle={1} bookmark={2} active={3}",
                                _startAddr, _cycleType, _bookmark, _active);
#endif
        }

#if DEBUG
        public long RequestID
        {
            get { return _reqID; }
            set { _reqID = value; }
        }
#endif

        public int StartAddress
        {
            get { return _startAddr; }
            set { _startAddr = value; }
        }

        public MemoryCycle CycleType
        {
            get { return _cycleType; }
            set { _cycleType = value; }
        }

        public int Bookmark
        {
            get { return _bookmark; }
            set { _bookmark = value; }
        }

        public bool Active
        {
            get { return _active; }
            set { _active = value; }
        }

#if DEBUG
        private long _reqID;
#endif
        private int _startAddr;
        private MemoryCycle _cycleType;
        private int _bookmark;
        private bool _active;
    }


    public enum MemoryState
    {
        Idle = 0,
        WaitForT3,
        WaitForT2,
        Running
    }

    /// <summary>
    /// A bookmark value comprises a set of flags, a word index, and next state value.
    /// These are read from the BKM16 ROM and are consulted each cycle to drive the
    /// memory state machine.
    /// </summary>
    public class BookmarkEntry
    {
        public BookmarkEntry(byte result)
        {
            Abort = ((result & 0x80) != 0);
            Recognize = ((result & 0x40) != 0);
            Complete = ((result & 0x20) != 0);
            Valid = ((result & 0x10) != 0);
            Index = (int)(result & 0x0c) >> 2;
            NextState = (MemoryState)(result & 0x03);
        }

        public override string ToString()
        {
            return string.Format("PA={0} ST={1} CO={2} VA={3} idx={4} next={5}",
                                Abort, Recognize, Complete, Valid, Index, NextState);
        }

        public bool Abort;
        public bool Recognize;
        public bool Complete;
        public bool Valid;
        public int Index;
        public MemoryState NextState;
    }

    /// <summary>
    /// Object to keep track of the Memory board's input and output queues.
    /// Instantiating one each for Stores and Fetches dramatically simplifies
    /// the overlapping quad-word read/write cycles used by RasterOp.  This
    /// class handles all of the arcane timing requirements, generates CPU Wait
    /// (and IO Hold eventually?) states when needed, and calculates the address
    /// for the appropriate word in the quad as each request executes.
    /// </summary>
    public sealed class MemoryController
    {
        static MemoryController()
        {
            _bkmTable = new BookmarkEntry[256];
            LoadBookmarkROM();
        }

        public MemoryController(MemoryBoard mem, string name)
        {
            _mem = mem;
            _name = name;
            _current = new MemoryRequest();
            _pending = new MemoryRequest();

            _quadWordMask = _mem.MemSizeMask & 0xfffffc;     // nip two LSBs
            _doubleWordMask = _mem.MemSizeMask & 0xfffffe;   // nip LSB
        }

        public void Reset()
        {
            _state = _nextState = MemoryState.Idle;
            _bookmark = _nextBookmark = 0;
            _address = -1;
            _index = 0;
            _wait = false;
            _valid = false;
            _current.Clear();
            _pending.Clear();

            Trace.Log(LogType.MemoryState, "{0} queue: Reset.", _name);
        }

        public bool Wait
        {
            get { return _wait; }
        }

        public bool Valid
        {
            get { return _valid; }
        }

        public int Address
        {
            get { return _address; }
        }

        public int WordIndex
        {
            get { return _index; }
        }

        public MemoryCycle Cycle
        {
            get { return _current.CycleType; }
        }


        /// <summary>
        /// Clocks this memory queue's state machine, setting flags appropriately for
        /// the current running request, or setting up for the next one.  Called from
        /// Memory.Tick(), this executes at the top of the microcycle, so it may abort 
        /// the current instruction if a new request is issued at the wrong time.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Clock(MemoryCycle nextCycle)
        {
            Trace.Log(LogType.MemoryState, "{0} queue  IN: Clock T{1} cycle={2} bkm={3} next={4} state={5} next={6}",
                     _name, _mem.TState, _current.CycleType, _bookmark, nextCycle, _state, _nextState);

            // Update the current op
            Recognize();

            // Update state and set flags for this cycle
            RunStateMachine();

            // Update bookmarks for the next cycle
            UpdateBookmarks(nextCycle);

            Trace.Log(LogType.MemoryState, "{0} queue OUT: Clock T{1} cycle={2} bkm={3} next={4} state={5} next={6}",
                     _name, _mem.TState, _current.CycleType, _bookmark, nextCycle, _state, _nextState);
        }

#if DEBUG
        /// <summary>
        /// Accept a new memory request (at the bottom of the CPU cycle, after R is
        /// computed).  Because the CPU now aborts until the correct cycle when issuing
        /// new memory operations, we simply latch the new request and let the state
        /// machine mechanism do all the right magic at the next Clock().
        /// </summary>
        public void Request(long id, int startAddr, MemoryCycle cycleType)
        {
            if (_pending.Active)
                Console.WriteLine("{0} queue: ** new Request() when _pending already Active?", _name);

            _pending.RequestID = id;
            _pending.StartAddress = startAddr & _mem.MemSizeMask;
            _pending.CycleType = cycleType;

            _pending.Active = true;
            _pending.Bookmark = _nextBookmark;
        }
#else
            /// <summary>
            /// Accept a new memory request (at the bottom of the CPU cycle, after R is
            /// computed).  Because the CPU now aborts until the correct cycle when issuing
            /// new memory operations, we simply latch the new request and let the state
            /// machine mechanism do all the right magic at the next Clock().
            /// </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public void Request(int startAddr, MemoryCycle cycleType)
            {
                _pending.StartAddress = startAddr;
                _pending.CycleType = cycleType;

                _pending.Active = true;
                _pending.Bookmark = _nextBookmark;
            }
#endif

        /// <summary>
        /// If the current op is complete and a pending op is ready, promote it.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void Recognize()
        {
            if (_pending.Active && !_current.Active)
            {
                // Swap current with pending.
                MemoryRequest old = _current;
                _current = _pending;
                _pending = old;
                _bookmark = _current.Bookmark;

                Trace.Log(LogType.MemoryState, "{0} queue: Recognized {1}", _name, _current);

                _pending.Clear();
            }
        }

        /// <summary>
        /// Update the current state of the controller, and computes the address and
        /// index of the current op if applicable.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void RunStateMachine()
        {
            // Bump the state
            _state = _nextState;

            // Get the flags for the current bookmark
            BookmarkEntry flags = GetBookmarkEntry(_bookmark, _state);

            // Set the wait and next state based on the current flags
            _wait = flags.Abort;
            _nextState = flags.NextState;

            // If reading or writing this cycle, compute the address for this word
            _valid = flags.Valid;

            if (_valid)
            {
                _index = flags.Index;

                switch (_current.CycleType)
                {
                    case MemoryCycle.Fetch4R:
                    case MemoryCycle.Fetch4:
                    case MemoryCycle.Store4R:
                    case MemoryCycle.Store4:
                        _address = (_current.StartAddress & _quadWordMask) + _index;

                        break;

                    case MemoryCycle.Fetch2:
                    case MemoryCycle.Store2:
                        _address = (_current.StartAddress & _doubleWordMask) + _index;
                        break;

                    default:
                        _address = _current.StartAddress;
                        break;
                }
            }

            // If this is the last word in a cycle, retire the current op
            if (flags.Complete)
            {
                Trace.Log(LogType.MemoryState, "{0} queue: Retired {1}", _name, _current);

                _current.Clear();
                _bookmark = 0;
            }
        }

        /// <summary>
        /// Sets bookmarks for the next cycle, and modifies the current one if necessary.
        /// WARNING: THIS IS WHERE THE SAUSAGE IS MADE.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void UpdateBookmarks(MemoryCycle nextCycle)
        {
            if (nextCycle == MemoryCycle.None)
            {
                // If no active or pending op, reset our bookmark
                if (!_current.Active && !_pending.Active)
                {
                    _bookmark = 0;
                }
            }
            else
            {
                // This microinstruction specifies a new memory request: initialize
                // the next bookmark value based on the request type
                int book = (int)nextCycle;

                // 
                // Special cases for RasterOp
                //
                if (_mem.RopEnabled)
                {
                    if (_mem.TState == 0)
                    {
                        // First: we're allowed to issue Store4/4R in T0, ahead
                        // of the usual T3.  So we tweak the cycle type to index
                        // the bookmark ROM with the modified timings.
                        if (nextCycle == MemoryCycle.Store4R)
                        {
                            book = 0x2;         // "RopStore4R"
                        }
                        else if (nextCycle == MemoryCycle.Store4)
                        {
                            book = 0x4;         // "RopStore4"
                        }
                    }
                    else if (_mem.TState == 3)
                    {
                        //
                        // Second: Fetch4/4Rs are issued back-to-back (in the
                        // correct t3) but must NOT introduce the possible CPU
                        // abort of a WaitT2 state;  MDI must remain valid AND
                        // the index values must count down correctly for the
                        // operation in progress, so after the t0,t1 complete
                        // the next op's four words arrive in the four subsequent
                        // Tstates.  This introduces two additional fake cycle
                        // types, as with the case above.  Ugh..
                        //
                        if (_current.CycleType == MemoryCycle.Fetch4R &&
                                     nextCycle == MemoryCycle.Fetch4R)
                        {
                            _bookmark = book = 0x1;     // "RopFetch4R"
                        }
                        else if (_current.CycleType == MemoryCycle.Fetch4 &&
                                          nextCycle == MemoryCycle.Fetch4)
                        {
                            _bookmark = book = 0x3;     // "RopFetch4"
                        }
                    }
                }

                // For RasterOp special cases, use modified bookmark for entire cycle
                _nextBookmark = book;

                //
                // Special cases for indirect or overlapped Fetches (non-RasterOp)
                //
                if (_mem.IsFetch(nextCycle))
                {
                    //
                    // Back-to-back Fetch or Fetch2 requests present unique timing
                    // challenges.  To accommodate this with as little embarrassment
                    // as possible, we use a transitional bookmark value to cover
                    // the overlap. For a Fetch, this may terminate the op early,
                    // invalidating one or more time slots where MDI is valid (and
                    // forcing a CPU wait so that incorrect data is not returned).
                    // In other cases we have to let the current op retire normally
                    // but drop immediately into a WaitT2 (rather than WaitT3) for
                    // the new op.  There's no pretty way to deal with this...
                    //
                    // Gory details in the comments below.  Look away now for
                    // "plausible deniability".
                    //
                    if (_current.CycleType == MemoryCycle.Fetch ||
                        _current.CycleType == MemoryCycle.Fetch2)
                    {
                        book = 0x6;                     // "IndFetch" covers the overlap...
                        _bookmark = book;               // ...force immediate switch for the (t2,t3)...
                        _nextBookmark = (int)nextCycle; // ...but switch back to the real cycle type in Request()
                    }
                    else if (_current.CycleType == MemoryCycle.Fetch4 && !_mem.RopEnabled)
                    {
                        book = 0x7;                     // "IndFetch4" is for the specific case of a RefillOp
                        _bookmark = book;               // followed immediately by another Fetch; can't clobber the
                        _nextBookmark = (int)nextCycle; // last index word, or the last two OpFile bytes are screwed
                    }
                }

                // Get a new set of flags -- these may modify the current cycle!
                BookmarkEntry flags = GetBookmarkEntry(book, _nextState);

#if DEBUG
                // If the Recognize flag is not set, we're really out in left field...
                // ... but all of this can go away entirely once things are fully debugged.
                if (!flags.Recognize)
                {
                    Console.WriteLine("-->\t{0} queue: Recognize not set for new {1} request in T{2}!", _name, nextCycle, _mem.TState);

                    // If the Abort flag isn't set either, our BKM16 ROM is buggy; force an
                    // abort and just hope for the best?
                    if (!flags.Abort)
                    {
                        Console.WriteLine("-->\tForced abort in T{0} due to new request in wrong cycle.", _mem.TState);
                        Console.WriteLine("\tFlags: {0}", flags);
                        DumpQueue();
                        flags.Abort = true;
                    }
                }
#endif

                // If the done flag is set, retire the current op (may be early,
                // if a Fetch is overlapped)
                if (flags.Complete)
                {
                    Trace.Log(LogType.MemoryState, "{0} queue: Terminated {1}", _name, _current);
                    _current.Clear();
                }

                // Set the wait and next state based on the new flags
                _wait = flags.Abort;
                _nextState = flags.NextState;
            }
        }

        /// <summary>
        /// Gets the bookmark for a particular cycle type.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private BookmarkEntry GetBookmarkEntry(int book, MemoryState state)
        {
            //
            // Index into the "bookmark" table:
            //		bits	value
            //		7:4		bookmark (cycle type)
            //		3:2		current state
            //		1:0		current Tstate
            //
            int lookup = (((int)book & 0x0f) << 4) | ((int)state << 2) | _mem.TState;

            Trace.Log(LogType.MemoryState, "{0} Bookmark[{1:x3}]: {2}", _name, lookup, _bkmTable[lookup]);
            return _bkmTable[lookup];
        }

        /// <summary>
        /// Load the BKM16 ROM image from disk.
        /// </summary>
        private static void LoadBookmarkROM()
        {
            // BKM is a lookup table with an 8-bit index, returning an 8-bit value
            FileStream fs = new FileStream(Paths.BuildPROMPath("bkm16emu.rom"), FileMode.Open);

            for (int i = 0; i < 256; i++)
            {
                // Split result byte into fields once, rather than on lookup
                _bkmTable[i] = new BookmarkEntry((byte)fs.ReadByte());
            }
            fs.Close();

            Trace.Log(LogType.EmuState, "Initialized BKM ROM lookup table.");
        }

#if DEBUG
        /// <summary>
        /// Dumps the current controller state and request slots. Quick and dirty debugging aid.
        /// </summary>
        public void DumpQueue()
        {
            Console.WriteLine("{0} queue:\tstate: wait={1} valid={2} index={3} addr={4:x6}",
                              _name, _wait, _valid, _index, _address);
            Console.WriteLine("\t\tcurrent: {0}", _current);
            Console.WriteLine("\t\tpending: {0}", _pending);
        }
#endif

        private string _name;
        private MemoryState _state;
        private MemoryState _nextState;

        private MemoryRequest _current;
        private MemoryRequest _pending;

        private int _quadWordMask;
        private int _doubleWordMask;

        private int _address;
        private int _index;
        private bool _wait;
        private bool _valid;

        private int _bookmark;
        private int _nextBookmark;

        private static BookmarkEntry[] _bkmTable;

        private MemoryBoard _mem;   // parent
    }
}


#region Hairy memory rules
/*
    [ This belongs in a doc file somewhere ]
    
The real PERQ memory rules are seriously hairy.  To make matters much worse, the
wording in the Microprogrammers' Guide is terribly confusing:

1. For any Fetch executed in T3, any memory reference in T0 or T1 is ignored, EXCEPT:
- a Store in T2 will start immediately          [Uh, we're talking about T0/T1 here??]
- a Store4 or Store4R can be specified in T0    [Will stall until T2? Or need MDO in T1?]
All others will abort until the correct cycle.   [But.. you said "ignored" above. What?]

2. After a Store in T2, any memory reference in T3 or T0 is ignored, but
refs started in T1 are aborted until the correct cycle.

3. After a Store2/4/4R in T3, any reference in the next 4 cycles is ignored.
But references started in T0 are aborted until the correct cycle.  [Uh, "ignored"?  You
keep using that word. I do not think it means what you think it means...]

Hold must be asserted in T2 to be effective; PERQemu doesn't worry about IO contention...

After a Fetch, MDI is valid from T2 to the following T1; all other Fetches supply
one word for a single microcycle.

	[Previous implementation-specific notes removed.]

Notes on the "indirect fetches" special case:

Because MDI is valid for four cycles after a one-word Fetch, a running Fetch can provide the address
for a subsequent Fetch (of any variety) issued in the immediate T2 -- first valid MDI cycle, when we
transition from WaitT2 to Running: 
	MA := addr1, Fetch;		(t3)
	(explicit inst or Nop)	(t0)
	(explicit inst or Nop)	(t1)
	MA := MDI, Fetch<*>;	(t2,t3)
			..				(t0,t1)
	Rnn := MDI;				(t2)

Page 3-36 of the uProgrammer's Guide (15-Jan-1984) illustrates this, and states explicitly that the
second Fetch* instruction is in fact stalled by one cycle.

In the previous queue-based implementation, the second Fetch would execute in t2, then stall in t3
waiting for Recognize() to invalidate the MemoryInstructions in the t0, t1 slots, then queue up the
new instructions to execute starting as usual in the next t2.  It was a hack, but it worked because
we kept a RequestID that tracked each instruction word and could Retire() instructions in any order.
The MDI access happened in t2, so the second Fetch latched the correct starting address.

The new implementation now snoops the microinstruction and aborts the processor until the correct
cycle when a memory op is selected:  the uOp.MemoryCycle is passed through Memory.Tick() ->
MemoryController.Clock()), which consults the runes, tea leaves and other cosmic sources to set the
Wait flag at the _top_ of the microcycle, before it executes.  This means we can single-step through
all phases of the request, including aborts, and it means that when the instruction executes we're
already in the correct cycle.  It does away with all of the queues entirely (and the associated GC
overhead) and replaces all the mechanics with a simple table lookup.  This is based loosely on the
hardware's "bookmark" ROM (BKM16.2), which now determines the timing of state machine transitions,
and provides a small set of flags and the Index value for multi-word fetches and stores.

The first whack at this was based on setting an initial bookmark value based on the cycle type,
memory state machine state, time val, RasterOp enabled flag, and next cycle type (to handle the
overlap cases below).  This would be looked up in a separate table, producing a single n-bit
bookmark which would essentially be the next address in the table.  A 13-bit index seemed excessive,
and editing an 8,192-line input file extremely tedious.  By stripping off 5 bits and special-casing
the weird ones, it's a faaaar more manageable 256 entries, some of which are unused.  The tradeoff
is a bit of complexity in UpdateBookmark().

Those tricky cases that require special attention are two:
1.	Store4 or Store4R when RasterOp is enabled can execute in t0, rather than the usual t3;
	this means the timings shift by one cycle, and the simplest way to accommodate that is to
	fake up a separate bookmark value for those two cycles.  Because we're using 4 bits to
	represent the 8 fetch/store types plus a "none", it seemed simpler to use two empty slots
	rather than add another bit ("RopEnabled") to the index, doubling the table size.  When a
	Store4/4R is issued in t0 and the RasterOp unit is enabled, we set the bookmark type to
	the appropriate fake one for the duration.

2.	The overlapped Fetch cases, detailed above.  This turns out to be a little trickier, but
	the solution was very similar: rather than add four more bits to the index ("curOp" and
	"nextOp"), ballooning up the table, or splitting it into two (like the "GMV" PROM in the
	hardware) I created another fake cycle type: "IndFetch".  The dark bit of magic here is
	that we use this bookmark value only for the t2,t3 where the overlap occurs -- specifying
	that the MDI word is still valid -- but then setting the "real" bookmark value to the
	correct fetch type to track the second cycle (with its usual timings).  That is, we have
	to use modified flags in the _top_ of the overlapped t3 -- allow the CPU to execute with
	MDI from the first Fetch -- then accept the new Fetch<> in the bottom half of the cycle
	with the correct bookmark type.  Because the overlapped request executes in t3 as usual,
	we transition back to WaitT2 in the next cycle -- invalidating MDI in the t0,t1 slots as
	we did in the queue implementation, but essentially "for free."

TL;DR: the new mechanism is a balance between keeping the lookup table as compact as possible
while accepting only a couple of oddball cases.  The "pure" approaches would have required more
input bits and a much larger index (and a LOT more work to create :-) so this is hopefully a
good compromise -- and a decent performance benefit!

*/
#endregion
