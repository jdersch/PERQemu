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

        /// <summary>
        /// Does a hardware reset on the device.
        /// </summary>
        public virtual void Reset(bool soft = false)
        {
            if (!IsLoaded)
            {
                // Should this just be a debug thing?
                Log.Warn(Category.HardDisk, "Reset called but no disk loaded!");
                _fault = true;
                return;
            }

            // A "soft" reset from the controller just clears the fault status
            // and should probably clear the seek state... todo/fixme check this
            if (soft)
            {
                _fault = false;
                return;
            }

            // A "hard" reset is from a power-on or reboot
            _ready = false;
            _fault = false;
            _index = false;

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
            return Sectors[_cyl, _head, sector];
        }

        /// <summary>
        /// Get a sector of data immediately.
        /// </summary>
        public virtual Sector GetSector(ushort cylinder, byte head, ushort sector)
        {
            return Sectors[cylinder, head, sector];
        }

        /// <summary>
        /// Writes a sector.  Embrace your inner address!
        /// </summary>
        public virtual void SetSector(Sector sec)
        {
            Sectors[sec.CylinderID, sec.HeadID, sec.SectorID] = sec;
        }

        /// <summary>
        /// Write a sector to the drive, with error checking.  Why?  Why do this?
        /// </summary>
        public virtual void SetSector(Sector sec, ushort cylinder, byte head, ushort sector)
        {
            if (sec.CylinderID != _cyl || sec.HeadID != _head)
            {
                // "Address error" :-)
                _fault = true;
            }

            Sectors[sec.CylinderID, sec.HeadID, sec.SectorID] = sec;
        }

        /// <summary>
        /// Initiates or continues a Seek by pulsing the Disk Step line.
        /// Direction is >0 for positive steps, or 0 for a negative steps.
        /// </summary>
        public virtual void SeekStep(int direction)
        {
            // Take a step, but within limits
            if (direction > 0)
            {
                _cyl = Math.Min(_cyl++, Geometry.Cylinders);
            }
            else
            {
                _cyl = Math.Min(_cyl--, _cyl);  // Heh.  Avoid wrap.
            }
            // Want to see if the buffered seeks are actually coming through
            // or if I have to fix the CTC to actually dtrt; i think this has
            // never actually worked right; we just deliver the data as requested
            // and treat every seek as a single step (which, in reality, is really
            // slow since there's 20ms head settling time on every seek op)
            var interval = (_scheduler.CurrentTimeNsec - _lastStep) * Conversion.NsecToMsec;
            Log.Debug(Category.HardDisk, "Step pulse! cyl={0}, interval={1:n}", _cyl, interval);
            _lastStep = _scheduler.CurrentTimeNsec;

            // schedule it:
            //      if not seeking, we start (with the "settling" time if any)
            //      if the timer is already running, we're buffering; extend
            //      eventually the pulses stop and we allow the seek to complete
            // easy peasy lemon squeezy

            //Event foo = _scheduler.Schedule((ulong)interval, (skewNsec, context) => { } );
            //foo.TimestampNsec += 9;

            // For now:
            _seekComplete = true;
        }


        /// <summary>
        /// Requests that the drive clear its Fault status.
        /// </summary>
        public virtual void FaultClear()
        {
            if (_fault)
            {
                Log.Debug(Category.HardDisk, "Fault cleared.");
                _fault = false;
            }
            else
            {
                Log.Debug(Category.HardDisk, "Fault clear requested.");
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
                delay = (ulong)(Specs.StartupDelay / 100 * rand.Next(50, 95) / 10); // DEBUG
            }

            _startupEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, DriveReady);

            Log.Debug(Category.HardDisk, "Drive {0} motor start ({1} sec delay).",
                      Info.Name, delay * Conversion.MsecToSec);
        }

        /// <summary>
        /// Signal that the drive is online.
        /// </summary>
        private void DriveReady(ulong skew, object context)
        {
            _ready = true;
            _startupEvent = null;

            Log.Debug(Category.HardDisk, "Hard drive is online: {0}.", Geometry);
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

            Log.Debug(Category.HardDisk,
                      "{0} drive loaded!  Index is {1:n}us every {2:n}ms ",
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
        private Event _indexEvent;

        // Oh yeah, seek stuff
        private ulong _lastStep;
        private Event _seekEvent;

        // Startup delay
        private Event _startupEvent;
    }
}
