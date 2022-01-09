//
// SystemTimer.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
    /// gate real-time CPU execution (both the PERQ and Z80 processors).
    /// </summary>
    public class SystemTimer
    {
        public SystemTimer(float ival)
        {
            _interval = ival;
            _callback = new HRTimerElapsedCallback(OnElapsed);
            _handle = HighResolutionTimer.Register(_interval, _callback);
            _sync = new ManualResetEventSlim(false);

            Trace.Log(LogType.Timer, "SystemTimer constructed, HR timer handle is {0}", _handle);
        }

        ~SystemTimer()
        {
            try
            {
                // Free up our callback in case we reconfigure and reinstantiate
                HighResolutionTimer.Unregister(_handle);
            }
            catch
            {
                Trace.Log(LogType.Timer, "Barfed trying to unregister SystemTimer " + _handle);
            }
        }

        public float Interval
        {
            get { return _interval; }
            set { _interval = value; }
        }

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
            HighResolutionTimer.Enable(_handle, enabled);
            _sync.Set();
            Trace.Log(LogType.Timer, "Heartbeat {0} {1}", _handle, (enabled ? "started" : "stopped"));
        }

        public void WaitForHeartbeat()
        {
            // If we're running fast, block; if too slow, blow through
            if (!_sync.IsSet)
                _sync.Wait();
            
            _sync.Reset();
        }

        void OnElapsed(HRTimerElapsedEventArgs e)
        {
            _sync.Set();
        }

        private int _handle;
        private float _interval;
        private HRTimerElapsedCallback _callback;
        private ManualResetEventSlim _sync;
    }
}
