
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PERQemu
{
    /// <summary>
    /// The SchedulerEventCallback describes a delegate that is invoked whenever a scheduled event has
    /// reached its due-date and is fired.
    /// </summary>    
    /// <param name="skew">The delta between the requested exec time and the actual exec time (in nsec)</param>
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
            Reset();
        }

        public ulong CurrentTimeNsec
        {
            get { return _currentTimeNsec; }
        }

        public void Reset()
        {
            _schedule = new SchedulerQueue();
            _currentTimeNsec = 0;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Clock()
        {
            //
            // Move one system clock forward in time.
            //
            _currentTimeNsec += _timeStepNsec;

            //
            // See if we have any events waiting to fire at this timestep.
            //
            while (_schedule.Top != null && _currentTimeNsec >= _schedule.Top.TimestampNsec)
            {
                // Pop the top event and fire the callback.
                Event e = _schedule.Pop();
                e.EventCallback(_currentTimeNsec - e.TimestampNsec, e.Context);
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

        public void DumpEvents()
        {
            System.Console.WriteLine("Time is {0}.  Queue:", _currentTimeNsec);
            _schedule.Dump();
        }

        public ulong TimeStepNsec => _timeStepNsec;

        private ulong _currentTimeNsec;

        private SchedulerQueue _schedule;

        // The time-base for the scheduler.
        private ulong _timeStepNsec;
    }

    /// <summary>
    /// Provides an "ordered" queue based on timestamp -- the top of the queue is always the 
    /// next event to be fired; a "push" places a new event in order on the current queue.
    /// </summary>
    public class SchedulerQueue
    {
        public SchedulerQueue()
        {
            _queue = new LinkedList<Event>();
        }

        public Event Top
        {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get
            {
                return _top;
            }
        }

        public bool Contains(Event e)
        {
            return _queue.Contains(e);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Push(Event e)
        {
            // Degenerate case:  list is empty or new entry is earlier than the head of the list.
            if (_queue.Count == 0 || _top.TimestampNsec >= e.TimestampNsec)
            {
                _queue.AddFirst(e);
                _top = e;
                return;
            }

            //
            // Do a linear search to find the place to put this in.
            // Since we maintain a sorted list with every insertion we only needy
            // to find the first entr that the new entry is earlier (or equal) to.
            // This will likely be adequate as the queue should never get incredibly
            // deep; a binary search may be more performant if this is not the case.
            //
            LinkedListNode<Event> current = _queue.First;
            while (current != null)
            {
                if (current.Value.TimestampNsec >= e.TimestampNsec)
                {
                    _queue.AddBefore(current, e);
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

        // debugging
        public void Dump()
        {
            foreach (var e in _queue)
            {
                System.Console.WriteLine("event {0} at {1}",
                                         e.EventCallback.Method.Name,
                                         e.TimestampNsec);
            }
        }

        private LinkedList<Event> _queue;

        /// <summary>
        /// The Top of the queue (null if queue is empty).
        /// </summary>
        private Event _top;
    }
}
