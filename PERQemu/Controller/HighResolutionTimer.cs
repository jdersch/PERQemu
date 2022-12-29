//
// HighResolutionTimer.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Diagnostics;
using System.Threading;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PERQemu
{

    public class HRTimerElapsedEventArgs : EventArgs
    {
        public double Delay { get; }

        internal HRTimerElapsedEventArgs(double delay)
        {
            Delay = delay;
        }
    }

    public delegate void HRTimerElapsedCallback(HRTimerElapsedEventArgs a);

    /// <summary>
    /// Internal class to track our timer clients (multiple intervals for
    /// one stopwatch).
    /// </summary>
    internal class TimerThing
    {
        public TimerThing()
        {
            Interval = 0;
            NextTrigger = 0;
            Callback = null;
            Enabled = false;
            Free = true;
        }

        public TimerThing(double interval, HRTimerElapsedCallback handler)
        {
            Interval = interval;
            NextTrigger = interval;
            Callback = handler;
            Enabled = false;
            Free = false;
        }

        public HRTimerElapsedCallback Callback { get; set; }
        public double NextTrigger { get; set; }
        public double Interval { get; set; }
        public bool Enabled { get; set; }
        public bool Free { get; set; }

        public override string ToString()
        {
            return string.Format("NextTrigger={0}, Interval={1:N3}, Enabled={2}, Free={3}",
                                 NextTrigger, Interval, Enabled, Free);
        }
    }

    /// <summary>
    /// Shared high-ish precision timer that supports multiple subscribers with
    /// separate intervals.  Runs a single Stopwatch on the current thread and
    /// doesn't rely on platform-specific code.
    /// </summary>
    /// <remarks>
    /// Based initially on a mash-up of anonymous code snippets found on-line,
    /// but hacked to run a dedicated thread.  It was way too intensive for our
    /// needs and all that silliness has been undone; it now forms the basis of
    /// the CLI's "multiplexed" Run loop (SDL, timers and Console input!).  Now
    /// that it runs on the main thread, Timer callbacks aren't coming from a
    /// random background thread so SDL events can be scheduled directly.
    /// 
    /// To register or unregister a timer client, the timer should probably
    /// be stopped since I'm not locking it or using enumerators (too slow).
    /// This is still more a proof-of-concept than a final implementation...
    /// </remarks>
    public static class HighResolutionTimer
    {
        /// <summary>
        /// Creates a timer and an empty list of requesters.
        /// </summary>
        static HighResolutionTimer()
        {
            _stopwatch = new Stopwatch();
            _requesters = new List<TimerThing>();

            _runTimers = false;
            _throttle = new AutoResetEvent(true);
        }

        /// <summary>
        /// "Close enough" value for coalescing or firing timers.
        /// </summary>
        public static double Tolerance = 0.01d;

        /// <summary>
        /// Tick frequency of the underlying mechanism.
        /// </summary>
        public static readonly double Frequency = Stopwatch.Frequency;

        /// <summary>
        /// Tick time length in milliseconds.
        /// </summary>
        public static readonly double TickLength = 1000d / Frequency;

        /// <summary>
        /// True if the system/operating system supports HighResolution timer.
        /// </summary>
        public static bool IsHighResolution = Stopwatch.IsHighResolution;

        /// <summary>
        /// True when our Stopwatch is running.
        /// </summary>
        public static bool IsRunning => _runTimers;

        /// <summary>
        /// Return how many milliseconds have elapsed on the stopwatch.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static double ElapsedHiRes()
        {
            return _stopwatch.ElapsedTicks * TickLength;
        }

        /// <summary>
        /// Register as a timer client to receive events at the specified
        /// interval and event handler.  Similar to SDL_Timer, returns an
        /// ID so the specific timer can be referred to later.
        /// </summary>
        public static int Register(double interval, HRTimerElapsedCallback cb)
        {
            int tag = 0;
            double next = interval;

            Log.Detail(Category.Timer, "Register called, requesters length = " + _requesters.Count);

            // Loop to see if we have an existing subscriber with the same
            // period; if so, adjust our new request to fire at the same time,
            // in effect coalescing the two and slightly improving efficiency :-)
            for (int i = 0; i < _requesters.Count; i++)
            {
                if (!_requesters[i].Free && Math.Abs(_requesters[i].Interval - interval) < Tolerance)
                {
                    next = _requesters[i].NextTrigger;
                    Log.Detail(Category.Timer, "Coalesced new timer at " + next);
                    break;
                }
            }

            // Now find an empty slot for the new request and set it
            for (tag = 0; tag < _requesters.Count; tag++)
            {
                if (_requesters[tag].Free)
                {
                    _requesters[tag].Enabled = false;
                    _requesters[tag].Interval = interval;
                    _requesters[tag].NextTrigger = next;
                    _requesters[tag].Callback = cb;
                    _requesters[tag].Free = false;

                    Log.Debug(Category.Timer,
                              "Registered timer {0}, interval {1:N3}, next trigger {2:N3}",
                              tag, interval, next);

                    return tag;
                }
            }

            // None free?  Extend...
            _requesters.Add(new TimerThing(interval, cb));

            Log.Debug(Category.Timer,
                      "Added new timer {0}, interval {1:N3}, next trigger {2:N3}",
                      tag, interval, next);

            return tag;
        }

        /// <summary>
        /// Enable events for a particular subscriber.  If the thread isn't yet
        /// running, start it up.  This might be problematic, if we start firing
        /// events for the CPU before the IO starts back up, or whatever.  We'll
        /// plummet off that bridge when we come to it.
        /// </summary>
        public static void Enable(int tag, bool doit)
        {
            try
            {
                _requesters[tag].Enabled = doit;

                // Enabling a timer starts the thread if it isn't already running
                if (doit && !_runTimers)
                {
                    Start();
                }
            }
            catch
            {
                Log.Error(Category.Timer, "Failed to set enable for tag {0}", tag);
            }
        }

        /// <summary>
        /// Adjust the interval for a timer.
        /// </summary>
        /// <remarks>
        /// Should disable, adjust then reenable to be "safe"?  But might get
        /// away with adjustments on the fly even if there's a little bump.
        /// This is still deemed less risky than freeing up a timer then re-
        /// registering.  It's mostly a hack so the CLI doesn't spin at a high
        /// rate if the Display isn't active; unless/until we have a GUI that
        /// needs attention, there aren't any SDL events to process until the
        /// emulator starts up.
        /// </remarks>
        public static void Adjust(int tag, double interval)
        {
            try
            {
                _requesters[tag].Interval = interval;
                Log.Debug(Category.Timer, "Interval for timer {0} now {1}", tag, interval);
            }
            catch
            {
                Log.Error(Category.Timer, "Failed to adjust interval for tag {0}", tag);
            }
        }

        /// <summary>
        /// Unregister the specified client and mark the slot as free.
        /// </summary>
        public static void Unregister(int tag)
        {
            try
            {
                if (_requesters[tag].Free)
                {
                    Log.Error(Category.Timer, "Request to unregister alread freed timer {0}", tag);
                }
                else
                {
                    // Be sure to shut it down first
                    _requesters[tag].Enabled = false;
                    _requesters[tag].Callback = null;
                    _requesters[tag].Free = true;
                }
            }
            catch
            {
                Log.Error(Category.Timer, "Bad call to unregister timer {0}!", tag);
                // do proper exception handling here...
            }
        }

        /// <summary>
        /// Enables the firing of events.
        /// </summary>
        public static void Start()
        {
            if (_runTimers) return;

            ResetIntervals();
            _stopwatch.Restart();

            _runTimers = true;
            //_throttle.Set();        // In case we're hanging in WaitOne()
        }

        /// <summary>
        /// Stops the timer loop and disable the firing of events.
        /// </summary>
        public static void Stop()
        {
            if (!_runTimers) return;

            _runTimers = false;
            _stopwatch.Stop();
        }

        /// <summary>
        /// Shutdown the timer and clear the list of requesters.
        /// </summary>
        public static void Shutdown()
        {
            Stop();
            _throttle.Set();        // If we're in Wait(), release the hold

#if DEBUG
            // For posterity
            Log.Debug(Category.Timer, "Stopwatch stopped, HRT thread exiting");
            Log.Debug(Category.Timer, "--> SpinWaits short={0} long={1}", shortSpin, longSpin);
            Log.Debug(Category.Timer, "--> Sleeps    short={0} long={1}", shortSleep, longSleep);
#endif
        }

        /// <summary>
        /// Executes the timer loop.
        /// </summary>
        /// <remarks>
        /// Exits if _runTimers is false or a null delegate is fired.  This
        /// provides a cheesy way for the CLI to periodically check the console
        /// for input before jumping back into the timer loop.
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Run()
        {
            double now, next, diff, skew;
            now = next = ElapsedHiRes();

#if DEBUG
            // Gather some efficiency stats for tuning, curiosity
            shortSpin = longSpin = shortSleep = longSleep = 0;
#endif
            var looping = true;

            while (looping)
            {
                // Set our next target
                next = NextInterval(now);

                // Loop until we reach it
                while (_runTimers)
                {
                    diff = next - ElapsedHiRes();
                    if (diff <= Tolerance)
                        break;

                    if (diff < 1d)
                    {
#if DEBUG
                        shortSpin++;
#endif
                        Thread.SpinWait(10);
                    }
                    else if (diff < 5d)
                    {
#if DEBUG
                        longSpin++;
#endif
                        Thread.SpinWait(100);
                    }
                    else if (diff < 15d)
                    {
#if DEBUG
                        shortSleep++;
#endif
                        _throttle.WaitOne(1);
                    }
                    else
                    {
#if DEBUG
                        longSleep++;
#endif
                        _throttle.WaitOne(10);
                    }
                }

                // Snapshot the elapsed time
                now = ElapsedHiRes();

                // Time to fire, unless we've been stopped...
                if (_runTimers)
                {
                    // Fire off expired callbacks, then reschedule them
                    for (int i = 0; i < _requesters.Count; i++)
                    {
                        if (_requesters[i].Enabled && (_requesters[i].NextTrigger <= (now + Tolerance)))
                        {
                            skew = ElapsedHiRes() - next;
                            _requesters[i].NextTrigger += _requesters[i].Interval;

                            // Bit of a hack: if the delegate is null, return.
                            if (_requesters[i].Callback == null)
                                looping = false;
                            else
                                _requesters[i].Callback.Invoke(new HRTimerElapsedEventArgs(skew));
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Run through the list of active timers and select the next one to
        /// fire.  If there are no defined or active timers, return "now" and
        /// tell the loop to exit?
        /// </summary>
        /// <remarks>
        /// This is terribly inefficient, but the list of timers is short.  We
        /// also don't lock anything here, which is dangerous, but again we get
        /// away with it because usage is fairly static (a couple of UI timers
        /// and a couple of CPU rate timers, all of which are set up once and
        /// not modified while the VM is running).
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static double NextInterval(double now)
        {
            double next = 0d;
            bool found = false;

            for (int i = 0; i < _requesters.Count; i++)
            {
                if (_requesters[i].Enabled)
                {
                    if (!found || _requesters[i].NextTrigger < next)
                    {
                        next = _requesters[i].NextTrigger;
                        found = true;
                    }
                }
            }

            if (found)
            {
                return next;
            }

            // hack so we don't just loop crazily
            Thread.Yield();

            _runTimers = false;
            return now;
        }

        /// <summary>
        /// Resets the intervals to zero.  Must be called when the Stopwatch
        /// is reset, otherwise the first interval can be a long, long wait. ;-)
        /// </summary>
        private static void ResetIntervals()
        {
            for (int i = 0; i < _requesters.Count; i++)
            {
                _requesters[i].NextTrigger = _requesters[i].Interval;
            }
            Log.Debug(Category.Timer, "Intervals reset");
        }

        // [Conditional("DEBUG")]
        public static void DumpTimers()
        {
            Console.WriteLine("Event loop is " + (_runTimers ? "running" : "not running"));
            Console.WriteLine("Stopwatch is " + (_stopwatch.IsRunning ? "running" : "not running"));

            Console.WriteLine("Registered timer clients:");
            for (int i = 0; i < _requesters.Count; i++)
            {
                Console.WriteLine("\t" + _requesters[i]);
            }
        }

#if DEBUG
        private static long shortSpin, longSpin, shortSleep, longSleep;
#endif

        /// <summary>
        /// The timer is running and firing events
        /// </summary>
        private static volatile bool _runTimers;

        private static Stopwatch _stopwatch;
        private static AutoResetEvent _throttle;
        private static List<TimerThing> _requesters;
    }
}
