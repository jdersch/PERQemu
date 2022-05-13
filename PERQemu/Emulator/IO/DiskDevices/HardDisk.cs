//
// HardDisk.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;

namespace PERQemu.IO.DiskDevices
{
    /// <summary>
    /// Emulates the mechanical operation of a hard disk drive, handling the
    /// timing for seek operations, index pulses, and other basic operations.
    /// Exposes the block API from StorageDevice through Get/SetSector for data
    /// access, and provides status infomation useful to a disk controller.  
    /// </summary>
    /// <remarks>
    /// Status flags are true/false; the associated Controller presents these
    /// to the PERQ in whichever active low/high state is appropriate.
    /// </remarks>
    public class HardDisk : StorageDevice
    {
        public HardDisk(Scheduler sched, string filename) : base(filename)
        {
            _scheduler = sched;

            _ready = false;
            _fault = false;
            _index = false;
            _seekComplete = false;

            _seekCallback = null;
            _seekEvent = null;
            _indexEvent = null;
            _startupEvent = null;

            _cyl = 0;
            _lastStep = 0;
            _discRotationTimeNsec = 0;
            _indexPulseDurationNsec = 0;
        }

        // Status bits
        public virtual bool Ready => _ready;
        public virtual bool Fault => _fault;
        public virtual bool Index => _index;
        public virtual bool Track0 => (_cyl == 0);
        public virtual bool SeekComplete => _seekComplete;

        // Debugging/sanity check
        public virtual ushort CurCylinder => _cyl;
        public virtual byte CurHead => _head;

        /// <summary>
        /// Does a hardware reset on the device.
        /// </summary>
        public virtual void Reset(bool soft = false)
        {
            if (!IsLoaded)
            {
                // Not really relevant until we support removable pack hard drives?
                Log.Warn(Category.HardDisk, "Reset called but no disk loaded!");
                _fault = true;
                return;
            }

            // A "soft" reset from the controller just clears the fault status
            // and should probably clear the seek state... but what if a seek
            // is in progress?  Abort or wait for motion to stop?  todo: check this?

            FaultClear();
            StopSeek();

            if (soft) return;

            // A "hard" reset is from a power-on or reboot
            _ready = false;
            _index = false;
            _seekComplete = false;

            if (_startupEvent == null)
            {
                // Cold start: schedule a ready event so our drive can
                // come up to speed.  todo: play the spin-up audio! :-)
                MotorStart();
            }

            if (_indexEvent != null)
            {
                _scheduler.Cancel(_indexEvent);
            }

            IndexPulseStart(0, null);
        }

        /// <summary>
        /// Gets a sector from the current cylinder/track.
        /// </summary>
        public virtual Sector GetSector(ushort sector)
        {
            // We'll ignore rotational delays... for now.
            return Read(_cyl, _head, sector);
        }

        /// <summary>
        /// Get a sector of data immediately.
        /// </summary>
        public virtual Sector GetSector(ushort cylinder, byte head, ushort sector)
        {
            return Read(cylinder, head, sector);
        }

        /// <summary>
        /// Writes a sector.  Embrace your inner address!
        /// </summary>
        public virtual void SetSector(Sector sec)
        {
            Write(sec);
        }

        /// <summary>
        /// Register a callback to fire when a seek operation completes.
        /// </summary>
        public virtual void SetSeekCompleteCallback(SchedulerEventCallback cb)
        {
            _seekCallback = cb;
        }

        /// <summary>
        /// Set the current head from the Head Select lines.
        /// </summary>
        public virtual void HeadSelect(byte head)
        {
            _head = head;
        }

        /// <summary>
        /// Initiates or continues a Seek by pulsing the Disk Step line.
        /// Direction is >0 for positive steps, or 0 or a negative steps.
        /// </summary>
        public virtual void SeekStep(int direction)
        {
            // Compute and check our new cylinder
            if (direction > 0)
            {
                _cyl = (ushort)Math.Min(_cyl + 1, (Geometry.Cylinders - 1));
            }
            else
            {
                _cyl = Math.Min((ushort)(_cyl - 1), _cyl);      // Don't underflow yer ushorts!
            }

            ulong start = _scheduler.CurrentTimeNsec;
            ulong delay = (ulong)Specs.MinimumSeek;

            // Schedule the time delay based on drive specifications
            if (_seekEvent == null)
            {
                // Starting a new seek
                _lastStep = start;
                _stepCount = 1;

                Log.Detail(Category.HardDisk, "Initial step to cyl {0}, seek {1}ms", _cyl, delay);

                _seekEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, SeekCompletion, start);
            }
            else
            {
                // Seek in progress, so buffer the step by extending the delay
                _stepCount++;

                // How long since the last step?  Technically there are tight specs
                // for the duration of and time between pulses but we can only assume
                // that the microcode/controller will adhere to them...
                var interval = (_scheduler.CurrentTimeNsec - _lastStep) * Conversion.NsecToMsec;
                _lastStep = _scheduler.CurrentTimeNsec;

                // Get our start time from the event context...
                start = (ulong)_seekEvent.Context;

                // Extend our completion time based on the number of steps received
                // minus the time elapsed so far.  This is quick and crude and should
                // be a nice smooth ramp function but for now just clamped based on
                // drive specs for full-stroke seek times (expressed in milliseconds
                // or months-of-sundays, take your pick)
                int tmp = Math.Min(_stepCount * Specs.MinimumSeek, Specs.MaximumSeek);
                delay = ((ulong)tmp * Conversion.MsecToNsec) - (_lastStep - start);

                Log.Detail(Category.HardDisk,
                          "Buffered step to cyl {0}, total seek now {1}ms (step interval={2:n}ms)",
                          _cyl, delay * Conversion.NsecToMsec, interval);

                // Update the seek event with the new delay time
                _seekEvent = _scheduler.ReSchedule(_seekEvent, delay);
            }
        }

        /// <summary>
        /// Finish a seek and inform any registered client.  Here's where we
        /// apply the "head settling" time if the device requires it.
        /// </summary>
        public virtual void SeekCompletion(ulong skewNsec, object context)
        {
            _seekComplete = true;
            _seekEvent = null;

            // Schedule a callback to the registered client, if any
            if (_seekCallback != null)
            {
                ulong settle = 1 * Conversion.UsecToNsec;

                // If faithfully emulating the slow ass disk drives of the mid-
                // 1980s, then add the head settling time to cap off our seek
                // odyssey.  Otherwise a default 1us delay is reasonable.
                if (Settings.Performance.HasFlag(RateLimit.AccurateDiskSpeedEmulation) && Specs.HeadSettling > 0)
                {
                    settle = (ulong)Specs.HeadSettling * Conversion.MsecToNsec;
                }

                Log.Debug(Category.HardDisk, "Seek complete [settling callback in {0:n}ms]",
                                              settle * Conversion.NsecToMsec);

                _scheduler.Schedule(settle, _seekCallback);
            }
            else
            {
                Log.Debug(Category.HardDisk, "Seek complete");
            }
        }

        /// <summary>
        /// Stops a seek in progress and resets.  It's not clear that the microcode
        /// would ever do this or how the drive would react (presumably the hardware
        /// completes the seek operation based on how many steps have been buffered;
        /// check the manual...)
        /// </summary>
        public virtual void StopSeek()
        {
            if (_seekEvent != null)
            {
                _scheduler.Cancel(_seekEvent);
                _seekEvent = null;
            }

            _seekComplete = false;
            _stepCount = 0;
        }

        /// <summary>
        /// Requests that the drive clear its Fault status.
        /// </summary>
        public virtual void FaultClear()
        {
            if (_fault)
            {
                Log.Debug(Category.HardDisk, "Fault cleared!");
                _fault = false;
            }
        }

        /// <summary>
        /// Spin up the virtual drive.  This schedules an event to raise the
        /// ready signal (which should cause an interrupt) after hardware reset.
        /// </summary>
        private void MotorStart()
        {
            ulong delay = 100;

            if (Settings.Performance.HasFlag(RateLimit.AccurateStartupDelays))
            {
                // This makes sense if we do the whole "watch the screen warm up
                // and play audio of the fans & disks whirring" for the total
                // multimedia experience.  If we just rewrote this as a fully
                // immersive 60fps 3D game, texture mapping the screen output
                // to the display and simulating actual keyboard presses through
                // data glove/VR or 3D first person shooter style, then you'd
                // really appreciate this attention to detail.  Otherwise it's
                // basically just insane.

                // Introduce a little variation, from 50-95% of max startup time
                var rand = new Random();
                delay = (ulong)(Specs.StartupDelay / 100 * rand.Next(50, 95));
            }

            _startupEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, DriveReady);

            Log.Debug(Category.HardDisk, "Drive {0} motor start ({1} sec delay)",
                      Info.Name, delay * Conversion.MsecToSec);
        }

        /// <summary>
        /// Signal that the drive is online.
        /// </summary>
        private void DriveReady(ulong skew, object context)
        {
            _ready = true;
            _startupEvent = null;

            Log.Info(Category.HardDisk, "{0} is online: {1}", Info.Description, Geometry);
        }

        /// <summary>
        /// Raises the Index signal for the drive's specified duration.
        /// </summary>
        private void IndexPulseStart(ulong skew, object context)
        {
            _index = true;
            _indexEvent = _scheduler.Schedule(_indexPulseDurationNsec, IndexPulseEnd);
        }

        /// <summary>
        /// Clears the Index pulse and schedules the next one.
        /// </summary>
        private void IndexPulseEnd(ulong skew, object context)
        {
            var next = _discRotationTimeNsec - _indexPulseDurationNsec;

            _index = false;
            _indexEvent = _scheduler.Schedule(next, IndexPulseStart);
        }

        /// <summary>
        /// Initialize some tings when we loaded, mon.
        /// </summary>
        public override void OnLoad()
        {
            // Compute the index pulse duration and gap
            _discRotationTimeNsec = (ulong)(1 / (Specs.RPM / 60.0) * Conversion.MsecToNsec);
            _indexPulseDurationNsec = (ulong)Specs.IndexPulse;

            Log.Info(Category.HardDisk, "{0} drive loaded!  Index is {1:n}us every {2:n}ms",
                     Info.Name, _indexPulseDurationNsec / 1000.0, _discRotationTimeNsec / 1000.0);

            base.OnLoad();
        }

        // Access to a scheduler
        private Scheduler _scheduler;

        // Status that the drive keeps track of
        private bool _ready;
        private bool _fault;
        private bool _index;
        private bool _seekComplete;

        // Where my heads at
        private ushort _cyl;
        private byte _head;

        // Index timing
        private ulong _discRotationTimeNsec;
        private ulong _indexPulseDurationNsec;
        private SchedulerEvent _indexEvent;

        // Seek timing
        private int _stepCount;
        private ulong _lastStep;
        private SchedulerEvent _seekEvent;

        private SchedulerEventCallback _seekCallback;

        // Startup delay
        private SchedulerEvent _startupEvent;
    }
}
