//
// Scheduler.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.CompilerServices;

namespace PERQemu
{
    /// <summary>
    /// The SchedulerEventCallback describes a delegate that is invoked whenever
    /// a scheduled event has reached its due-date and is fired.
    /// </summary>    
    /// <param name="skewNsec">The delta between the requested exec time and the actual exec time (in nsec)</param>
    /// <param name="context">An object containing context useful to the scheduler of the event</param>
    public delegate void SchedulerEventCallback(ulong skewNsec, object context);

    /// <summary>
    /// A SchedulerEvent encapsulates a callback and associated context that is
    /// scheduled for a future timestamp.
    /// </summary>
    public class SchedulerEvent
    {
        public SchedulerEvent(ulong timestampNsec, object context, SchedulerEventCallback callback)
        {
            _timestampNsec = timestampNsec;
            _context = context;
            _callback = callback;
        }

        /// <summary>
        /// The absolute time (in nsec) to raise the event.
        /// </summary>
        public ulong TimestampNsec
        {
            get { return _timestampNsec; }
            set { _timestampNsec = value; }
        }

        /// <summary>
        /// An object containing context to be passed to the
        /// event callback.
        /// </summary>
        public object Context
        {
            get { return _context; }
            set { _context = value; }
        }

        /// <summary>
        /// A delegate to be executed when the callback fires.
        /// </summary>
        public SchedulerEventCallback EventCallback
        {
            get { return _callback; }
        }

        private ulong _timestampNsec;
        private object _context;
        private SchedulerEventCallback _callback;
    }

    /// <summary>
    /// The Scheduler class provides infrastructure for scheduling hardware
    /// events inside the virtual machine, clocked at the CPU's execution rate.
    /// </summary>
    /// <remarks>
    /// Each Scheduler can run on its own timebase inside its own thread, and
    /// fires its callbacks on the thread that advances the Clock.  There are
    /// some circumstances where cross-thread calls are difficult to avoid;
    /// for example, the Z80's hard disk seek control has to interact with the
    /// hard disk on the main CPU thread.  We provide as low-cost a method as
    /// we can to synchronize around the non-thread-safe LinkedList...
    /// </remarks>
    public class Scheduler
    {
        public Scheduler(ulong timeStepNsec)
        {
            _timeStepNsec = timeStepNsec;
            _schedule = new SchedulerQueue();
            _queueLock = 0;
        }

        public ulong CurrentTimeNsec => _currentTimeNsec;

        public void Reset()
        {
            // Hit it, with the rock!
            QuickRelease();

            if (_currentTimeNsec > 0)
            {
                _currentTimeNsec = 0;
                _schedule.Clear();
                Log.Debug(Category.Scheduler, "Reset ({0})", _timeStepNsec);
            }
        }

        /// <summary>
        /// Advance the system clock and fire off events that are ready.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Clock(int steps = 1)
        {

            while (steps > 0)
            {
                // Move one clock step forward in time
                _currentTimeNsec += _timeStepNsec;

                // See if we have any events waiting to fire at this timestep
                while (true)
                {
                    if (_currentTimeNsec < _schedule.Top.TimestampNsec)
                    {
                        // No more ready events this trip
                        break;
                    }

                    QuickLock();
                    SchedulerEvent e = _schedule.Pop();
                    QuickRelease();

                    // Fire the callback
                    e.EventCallback(_currentTimeNsec - e.TimestampNsec, e.Context);
                }

                steps--;
            }
        }

        /// <summary>
        /// Add a new event to the schedule.
        /// </summary>
        public SchedulerEvent Schedule(ulong timestampNsec, SchedulerEventCallback callback, object context = null)
        {
            SchedulerEvent e = new SchedulerEvent(timestampNsec + _currentTimeNsec, context, callback);
            QuickLock();
            _schedule.Push(e);
            QuickRelease();

            return e;
        }

        /// <summary>
        /// Update an existing event with a new timestamp.
        /// </summary>
        public SchedulerEvent ReSchedule(SchedulerEvent old, ulong timestampNsec)
        {
            SchedulerEvent e = new SchedulerEvent(timestampNsec + _currentTimeNsec, old.Context, old.EventCallback);
            QuickLock();
            _schedule.Push(e);
            _schedule.Remove(old);
            QuickRelease();

            return e;
        }

        /// <summary>
        /// Remove an existing event.
        /// </summary>
        public void Cancel(SchedulerEvent e)
        {
            if (e != null)
            {
                QuickLock();
                _schedule.Remove(e);
                QuickRelease();
            }
        }

        // DEBUG
        public void DumpEvents(string name)
        {
            Console.WriteLine("Time is {0}.  {1} Queue:", _currentTimeNsec, name);
            _schedule.Dump();
        }

        /// <summary>
        /// Crude fast lock around the queue.  I can't imagine this is faster
        /// than lock() (Monitor.Enter) but let's suck it and see
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void QuickLock()
        {
            long spins = 0;
            SpinWait spin = new SpinWait();
            var me = Thread.CurrentThread.ManagedThreadId;

            // This is supposed to be many times faster than lock{}
            // Impact is around +1fps, if that?  Woo.
            while (Interlocked.CompareExchange(ref _queueLock, me, 0) != me)
            {
                spins++;
                spin.SpinOnce();
            };

#if DEBUG
            if (_queueLock != me)
                Console.WriteLine($"thread {me} doesn't actually have the lock?!?");

            if (spins > 1)
                Console.WriteLine($"acquiring thread {me} took {spins} spins");
#endif
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void QuickRelease()
        {
#if DEBUG
            var me = Thread.CurrentThread.ManagedThreadId;
            if (_queueLock > 0 && _queueLock != me)
                Console.WriteLine($"thread {me} releasing a lock held by {_queueLock}");
#endif
            Interlocked.Exchange(ref _queueLock, 0);
        }

        public ulong TimeStepNsec => _timeStepNsec;

        // The time-base for the scheduler
        private ulong _timeStepNsec;
        private ulong _currentTimeNsec;

        private volatile int _queueLock;
        private SchedulerQueue _schedule;
    }

    /// <summary>
    /// Provides an "ordered" queue based on timestamp -- the top of the queue
    /// is always the next event to be fired; a "push" places a new event in
    /// order on the current queue.
    /// </summary>
    /// <remarks>
    /// The queue always contains a sentinel node a so that Top is guaranteed
    /// not to be null.  This simplifies things a lot.
    /// </remarks>
    internal class SchedulerQueue
    {
        public SchedulerQueue()
        {
            _queue = new LinkedList<SchedulerEvent>();
            Clear();
        }

        /// <summary>
        /// The Top of the queue (null if queue is empty).
        /// </summary>
        public SchedulerEvent Top
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get
            {
                return _top;
            }
        }

        /// <summary>
        /// Clear this queue and reset Top by adding in the sentinel value.
        /// </summary>
        public void Clear()
        {
            _top = null;
            _queue.Clear();

            // If this fires in 584 years I'll be very impressed
            Push(new SchedulerEvent(ulong.MaxValue, null, null));
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Push(SchedulerEvent e)
        {
            //
            // Do a linear search to find the place to put this in.  Since we
            // maintain a sorted list with every insertion we only need to find
            // the first entr that the new entry is earlier (or equal) to. This
            // will likely be adequate as the queue should never get incredibly
            // deep; a binary search may be more performant if this isn't the case.
            //
            LinkedListNode<SchedulerEvent> current = _queue.First;

            // Empty list?  Add the first event
            if (current == null)
            {
                _queue.AddFirst(e);
                _top = _queue.First.Value;
                return;
            }

            // Insert in chronological order
            while (current != null)
            {
                if (e.TimestampNsec < current.Value.TimestampNsec)
                {
                    // This might be our new first element, so reset Top
                    _queue.AddBefore(current, e);
                    _top = _queue.First.Value;
                    return;
                }

                current = current.Next;
            }

            // Add at end
            _queue.AddLast(e);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public SchedulerEvent Pop()
        {
            SchedulerEvent e = _top;

            _queue.RemoveFirst();
            _top = _queue.First != null ? _queue.First.Value : null;

            return e;
        }

        public void Remove(SchedulerEvent e)
        {
            if (_queue.Contains(e))
            {
                if (_top == e)
                {
                    _queue.RemoveFirst();
                    _top = _queue.First != null ? _queue.First.Value : null;
                }
                else
                {
                    _queue.Remove(e);
                }
            }
        }

        // DEBUG
        public void Dump()
        {
            if (_queue.Count > 1)   // ignore the sentinel value :-)
            {
                foreach (var e in _queue)
                {
                    if (e.TimestampNsec < uint.MaxValue)
                    {
                        Console.WriteLine("event {0} at {1}",
                                          e.EventCallback.Method.Name,
                                          e.TimestampNsec);
                    }
                }
            }
            else
            {
                Console.WriteLine("<queue is empty>");
            }
        }

        private LinkedList<SchedulerEvent> _queue;
        private SchedulerEvent _top;
    }
}
