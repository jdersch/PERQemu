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
    /// The SchedulerEventCallback describes a delegate that is invoked whenever a scheduled event has
    /// reached its due-date and is fired.
    /// </summary>    
    /// <param name="skewNsec">The delta between the requested exec time and the actual exec time (in nsec)</param>
    /// <param name="context">An object containing context useful to the scheduler of the event</param>
    public delegate void SchedulerEventCallback(ulong skewNsec, object context);

    /// <summary>
    /// An Event encapsulates a callback and associated context that is scheduled for a future timestamp.
    /// </summary>
    public class Event
    {
        public Event(ulong timestampNsec, object context, SchedulerEventCallback callback)
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
    /// The Scheduler class provides infrastructure for scheduling time-based hardware events.
    /// 
    /// Note that the Scheduler is not thread-safe and must only be used from the emulation thread,
    /// or else things will break.  This is not optimal -- having a thread-safe scheduler would make
    /// it easier/cleaner to deal with asynchronous things like ethernet packets and scripting events
    /// but doing so incurs about a 10% performance penalty so it's been avoided.
    /// </summary>
    public class Scheduler
    {
        public Scheduler(ulong timeStepNsec)
        {
            _timeStepNsec = timeStepNsec;
            _schedule = new SchedulerQueue();
        }

        public ulong CurrentTimeNsec => _currentTimeNsec;

        public void Reset()
        {
            if (_currentTimeNsec > 0)
            {
                _currentTimeNsec = 0;
                _schedule.Clear();
                Log.Debug(Category.Scheduler, "Reset ({0})", _timeStepNsec);
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Clock(int steps = 1)
        {
            while (steps > 0)
            {
                // Move one clock step forward in time
                _currentTimeNsec += _timeStepNsec;

                // See if we have any events waiting to fire at this timestep
                while (_schedule.Top != null)
                {
                    if (_currentTimeNsec < _schedule.Top.TimestampNsec)
                    {
                        // No more ready events this trip
                        break;
                    }

                    // Pop the top event and fire the callback
                    Event e = _schedule.Pop();
                    e.EventCallback(_currentTimeNsec - e.TimestampNsec, e.Context);
                }

                steps--;
            }
        }

        /// <summary>
        /// Add a new event to the schedule.
        /// </summary>
        public Event Schedule(ulong timestampNsec, object context, SchedulerEventCallback callback)
        {
            Event e = new Event(timestampNsec + _currentTimeNsec, context, callback);
            _schedule.Push(e);

            return e;
        }

        public Event Schedule(ulong timestampNsec, SchedulerEventCallback callback)
        {
            Event e = new Event(timestampNsec + _currentTimeNsec, null, callback);
            _schedule.Push(e);

            return e;
        }

        public void Cancel(Event e)
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
    /// Provides an "ordered" queue based on timestamp -- the top of the queue is always the 
    /// next event to be fired; a "push" places a new event in order on the current queue.
    /// </summary>
    internal class SchedulerQueue
    {
        public SchedulerQueue()
        {
            _queue = new LinkedList<Event>();
        }

        /// <summary>
        /// The Top of the queue (null if queue is empty).
        /// </summary>
        public Event Top
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get
            {
                return _top;
            }
        }

        public void Clear()
        {
            _queue.Clear();
            _top = null;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Push(Event e)
        {
            //
            // Do a linear search to find the place to put this in.  Since we
            // maintain a sorted list with every insertion we only need to find
            // the first entr that the new entry is earlier (or equal) to. This
            // will likely be adequate as the queue should never get incredibly
            // deep; a binary search may be more performant if this isn't the case.
            //
            LinkedListNode<Event> current = _queue.First;

            // Empty list?  Add the first event
            if (current == null)
            {
                _queue.AddFirst(e);
                _top = e;
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
        public Event Pop()
        {
            Event e = _top;

            _queue.RemoveFirst();
            _top = _queue.First != null ? _queue.First.Value : null;

            return e;
        }

        public void Remove(Event e)
        {
            if (_queue.Contains(e))
            {
                _queue.Remove(e);
                _top = _queue.First != null ? _queue.First.Value : null;
            }
        }

        // DEBUG
        public void Dump()
        {
            if (_queue.Count > 0)
            {
                foreach (var e in _queue)
                {
                    Console.WriteLine("event {0} at {1}",
                                      e.EventCallback.Method.Name,
                                      e.TimestampNsec);
                }
            }
            else
            {
                Console.WriteLine("<queue is empty>");
            }
        }

        private LinkedList<Event> _queue;
        private Event _top;
    }
}
