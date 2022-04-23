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
        }

        public ulong CurrentTimeNsec => _currentTimeNsec;

        /// <summary>
        /// Reset this scheduler.
        /// </summary>
        /// <remarks>
        /// NB: This is a no-op if time has not advanced.  We don't want to blow
        /// away the initial events registered prior to starting the machine.
        /// This is something of a hack, but given that the Z80 can be "turned
        /// off" and restarted it's necessary to prevent greater ugliness. :-|
        /// 
        /// Such as: the Z80/IO scheduler can pass in the initial time upon reset.
        /// This supports the new cooperative model where the Z80 synchronizes
        /// itself to the main CPU for speed regulation.
        /// </remarks>
        public void Reset(ulong startTime = 0)
        {
            if (_currentTimeNsec > 0 || startTime > 0)
            {
                _currentTimeNsec = startTime;
                _schedule.Clear();
                Log.Debug(Category.Scheduler, "Reset (step {0}ns, time {1})", _timeStepNsec, _currentTimeNsec);
            }
        }

        /// <summary>
        /// Advance the system clock and fire off events that are ready.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Clock(int steps = 1)
        {
            SchedulerEvent e;

            while (steps > 0)
            {
                // Move one clock step forward in time
                _currentTimeNsec += _timeStepNsec;

                // See if we have any events waiting to fire at this timestep
                while (_schedule.Pop(_currentTimeNsec, out e))
                {
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
#if DEBUG
            if (callback == null)
                throw new InvalidOperationException("Null callback in Scheduler");
#endif
            SchedulerEvent e = new SchedulerEvent(timestampNsec + _currentTimeNsec, context, callback);
            _schedule.Push(e);

            return e;
        }

        /// <summary>
        /// Update an existing event with a new timestamp.
        /// </summary>
        public SchedulerEvent ReSchedule(SchedulerEvent old, ulong timestampNsec)
        {
            SchedulerEvent e = new SchedulerEvent(timestampNsec + _currentTimeNsec, old.Context, old.EventCallback);
            _schedule.Remove(old);
            _schedule.Push(e);

            return e;
        }

        /// <summary>
        /// Remove an existing event.
        /// </summary>
        public void Cancel(SchedulerEvent e)
        {
            if (e != null)
            {
                _schedule.Remove(e);
            }
        }

        // DEBUG
        public void DumpEvents(string name)
        {
            Console.WriteLine("Time is {0}.  {1} Queue:", _currentTimeNsec, name);
            _schedule.Dump();
        }


        public ulong TimeStepNsec => _timeStepNsec;

        // The time-base for the scheduler
        private ulong _timeStepNsec;
        private ulong _currentTimeNsec;

        private SchedulerQueue _schedule;
    }


    /// <summary>
    /// Provides an "ordered" queue based on timestamp -- the top of the queue
    /// is always the next event to be fired; a "push" places a new event in
    /// order on the current queue.
    /// </summary>
    /// <remarks>
    /// The queue always contains a sentinel node a so that _top is guaranteed
    /// not to be null.  This simplifies things a lot.
    /// </remarks>
    internal class SchedulerQueue
    {
        public SchedulerQueue()
        {
            _queue = new LinkedList<SchedulerEvent>();
            _queueLock = new object();

            Clear();
        }

        /// <summary>
        /// Clear this queue and reset Top by adding in the sentinel value.
        /// </summary>
        public void Clear()
        {
            _last = null;
            _queue.Clear();

            // If this fires in 584 years I'll be very impressed
            _last = new SchedulerEvent(ulong.MaxValue, null, null);
            Push(_last);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Push(SchedulerEvent e)
        {
            lock (_queueLock)
            {
                //
                // Do a linear search to find the place to put this in.  Since we
                // maintain a sorted list with every insertion we only need to find
                // the first entry that the new entry is earlier (or equal) to. This
                // will likely be adequate as the queue should never get incredibly
                // deep; a binary search may be more performant if this isn't the case.
                //
                LinkedListNode<SchedulerEvent> current = _queue.First;

                // Empty list?  Add the first event
                if (current == null)
                {
                    _queue.AddFirst(e);
                    _next = e.TimestampNsec;
                    return;
                }

                // Insert in chronological order
                while (current != null)
                {
                    if (e.TimestampNsec < current.Value.TimestampNsec)
                    {
                        // Found our spot, slip it in
                        _queue.AddBefore(current, e);
                        _next = _queue.First.Value.TimestampNsec;
                        return;
                    }

                    current = current.Next;
                }

                // Add at end
                _queue.AddLast(e);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool Pop(ulong timeToFire, out SchedulerEvent e)
        {
            // Do a fast rough check (outside the lock); it's okay if we
            // "blow a rev" if the event gets cancelled or rescheduled since
            // we'll just pick it up next time around
            if (timeToFire < _next)
            {
                e = null;
                return false;
            }
#if DEBUG
            // Should never attempt to pop the sentinel!  Either the list
            // was not initialized properly or there's a bug in Clock()...
            if (_queue.Count <= 1)
                throw new InvalidOperationException($"Pop from schedule with {_queue.Count} elements");
#endif
            lock (_queueLock)
            {
                // Are we *reeeally* ready to fire?  Since we've incurred the
                // cost of acquiring the lock, double check just in case :-/
                e = _queue.First.Value;

                if (timeToFire < e.TimestampNsec)
                {
                    // Aw, crap
                    Console.WriteLine($"False alarm: list head time {e.TimestampNsec} moved back to {_next}");
                    return false;
                }

                // We're good; pop the event
                _queue.RemoveFirst();

                // Reset
                _next = _queue.First.Value.TimestampNsec;
                return true;
            }
        }

        public void Remove(SchedulerEvent e)
        {
            lock (_queueLock)
            {
                if (_queue.Contains(e))
                {
#if DEBUG
                    if (e == _last)
                        throw new InvalidOperationException("Attempt to pop sentinel from schedule");
#endif           
                    _queue.Remove(e);
                    _next = _queue.First.Value.TimestampNsec;
                }
            }
        }

        // DEBUG
        public void Dump()
        {
            lock (_queueLock)
            {
                if (_queue.Count > 1)   // ignore the sentinel value :-)
                {
                    foreach (var e in _queue)
                    {
                        if (e != _last)
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
        }

        private LinkedList<SchedulerEvent> _queue;
        private object _queueLock;

        private ulong _next;
        private SchedulerEvent _last;
    }
}
