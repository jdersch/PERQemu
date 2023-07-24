//
// SystemTimer.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using System.Threading;

namespace PERQemu
{
    /// <summary>
    /// Provides a heartbeat timer with a high-resolution interval.  Used to
    /// gate real-time CPU execution.
    /// </summary>
    public class SystemTimer
    {
        public SystemTimer(double ival, ulong cycleTime)
        {
            _interval = ival;
            _period = (uint)(ival / (cycleTime * Conversion.NsecToMsec));
            _callback = new HRTimerElapsedCallback(OnElapsed);
            _handle = HighResolutionTimer.Register(_interval, _callback);
            _sync = new ManualResetEventSlim(false);
            _isEnabled = false;

            Log.Debug(Category.Timer,
                      "SystemTimer {0} created, interval {1}ms, period {2} cycles",
                      _handle, _interval, _period);
        }

        /// <summary>
        /// Period between heartbeats, in clock cycles.
        /// </summary>
        public uint Period => _period;

        /// <summary>
        /// Timer interval, in ticks.
        /// </summary>
        public double Interval => _interval;

        /// <summary>
        /// Is we is, or is we isn't?
        /// </summary>
        public bool IsEnabled => _isEnabled;

        /// <summary>
        /// Resets (disables) this heartbeat timer.
        /// </summary>
        public void Reset()
        {
            Enable(false);
        }

        /// <summary>
        /// Enable or disable the heartbeat timer.  Clears the wait handle (freeing
        /// the thread if it was blocked).  This may be slighty counterintuitive.
        /// </summary>
        public void Enable(bool enabled)
        {
            if (enabled != _isEnabled)
            {
                _isEnabled = enabled;
                HighResolutionTimer.Enable(_handle, enabled);
                Log.Debug(Category.Timer, "Heartbeat {0} {1}", _handle, (enabled ? "started" : "stopped"));
            }

            // Always clear regardless of transition
            _sync.Set();
        }

        public void Shutdown()
        {
            try
            {
                // Free up our callback in case we reconfigure and reinstantiate
                HighResolutionTimer.Unregister(_handle);
            }
            catch
            {
                Log.Error(Category.Timer, "Barfed trying to unregister SystemTimer {0}", _handle);
            }
        }

        public void WaitForHeartbeat()
        {
            // If we're running fast, block; if too slow, blow through
            if (!_sync.IsSet)
            {
                _sync.Wait();
            }

            _sync.Reset();
        }

        void OnElapsed(HRTimerElapsedEventArgs e)
        {
            _sync.Set();
        }

        int _handle;
        bool _isEnabled;
        uint _period;
        double _interval;
        HRTimerElapsedCallback _callback;
        ManualResetEventSlim _sync;
    }
}
