//
// FloppyDisk.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;

namespace PERQemu.IO.DiskDevices
{
    /// <summary>
    /// Emulates the mechanical operation of a Shugart SA851 8" floppy drive
    /// and access to the floppy's data (if loaded).
    /// </summary>
    public class FloppyDisk : StorageDevice
    {
        public FloppyDisk(Scheduler sched, string filename) : base(filename)
        {
            _scheduler = sched;

            // If instantiated without a diskette loaded!
            if (string.IsNullOrEmpty(filename))
            {
                Geometry = DeviceGeometry.NoMedia;
            }

            // The only device type we support
            Info = DeviceInfo.SA851;

            _ready = false;
            _fault = false;
            _diskChange = false;
            _driveSelect = false;
            _isSingleSided = true;
            _isDoubleDensity = false;

            _cylinder = 1;      // Force a Step Out to find track 0 on startup
            _head = 0;

            _loadDelayEvent = null;
            _seekDelayEvent = null;
        }

        /// <summary>
        /// Reset the device.
        /// </summary>
        public void Reset(bool soft = false)
        {
            // Stop any seek in progress
            if (_seekDelayEvent != null)
            {
                _scheduler.Cancel(_seekDelayEvent);
                _seekDelayEvent = null;
            }

            // Anything else?
            Log.Debug(Category.FloppyDisk, "Drive reset");
        }

        public ushort Cylinder => _cylinder;

        public bool Fault => _fault;
        public bool Ready => (IsLoaded && _ready);
        public bool Track0 => (_cylinder == 0);
        public bool IsSingleSided => _isSingleSided;
        public bool IsDoubleDensity => _isDoubleDensity;

        public bool DiskChange
        {
            get { return _diskChange; }
            set { _diskChange = value; }
        }

        public bool DriveSelect
        {
            get { return _driveSelect; }
            set
            {
                _driveSelect = value;

                // The Disk Change signal is reset when Drive Select goes low
                _diskChange &= _driveSelect;
            }
        }

        public byte HeadSelect
        {
            get { return _head; }
            set
            {
                _head = value;

                if (IsLoaded && IsSingleSided && _head > 0)
                {
                    // SA851 denies Ready if selecting head 1 on a single
                    // sided diskette. We assume the FDC checks the RDY pin...
                    Log.Debug(Category.FloppyDisk, "Bad head select, clearing RDY");
                    _ready = false;
                    _head = 0;
                }
                else
                {
                    // Reset the Ready bit if they reselect the proper head
                    _ready |= (IsLoaded && _loadDelayEvent == null);
                }
            }
        }

        public void SeekTo(ushort track, SchedulerEventCallback cb)
        {
            // Clip cylinder count into range and compute seek delay
            var cyls = Math.Abs(track - _cylinder);
            var delay = Math.Min(Specs.MinimumSeek * (cyls + 1), Specs.MaximumSeek);

            if (!IsLoaded)
            {
                // Trying to execute a seek or recalibrate with no media loaded
                // should result in an EquipCheck fault and an abnormal termination
                _fault = true;
                track = _cylinder;  // don't move the heads
            }

            // Schedule the callback (to fire on the FDC) to signal seek complete
            _seekDelayEvent = _scheduler.Schedule((ulong)delay * Conversion.MsecToNsec, cb);

            // Make sure we don't fly off the end
            _cylinder = Math.Min(track, Geometry.Cylinders);

            Log.Debug(Category.FloppyDisk, "Drive seek to cyl {0} in {1}ms", _cylinder, delay);
        }

        /// <summary>
        /// Returns sector data for the given address, but map the (annoying)
        /// floppy sector numbering (1..26) to the underlying device (0..25).
        /// </summary>
        public override Sector Read(ushort cyl, byte head, ushort sec)
        {
            // Read from 1..26 -> 0..25
            Sector fudge = base.Read(cyl, head, (ushort)(sec - 1));

            // But fudge the returned id from 0..25 -> 1..26
            fudge.SectorID++;

            Log.Detail(Category.FloppyDisk, "Read {0} (actual {1})", sec - 1, fudge.SectorID);
            return fudge;
        }

        /// <summary>
        /// Writes a sector to the given address, accounting for offset.
        /// </summary>
        public override void Write(Sector sec)
        {
            // Map from 1..26 -> 0..25
            sec.SectorID--;

            Log.Detail(Category.FloppyDisk, "Write {0} (actual {1})", sec.SectorID, sec.SectorID + 1);
            base.Write(sec);
        }

        /// <summary>
        /// Validate a write, accounting for sector offset and IsWritable.
        /// </summary>
        public override bool WriteCheck(Sector sec)
        {
            return Info.IsWritable && Validate(sec.CylinderID, sec.HeadID, (ushort)(sec.SectorID - 1));
        }

        /// <summary>
        /// On load, check and set some flags 'n stuff.
        /// </summary>
        public override void OnLoad()
        {
            _ready = false;
            _fault = false;
            _driveSelect = false;
            _diskChange = true;
            _isSingleSided = (Geometry.Heads == 1);
            _isDoubleDensity = (Geometry.SectorSize == 256);

            // SA851 manual says that Ready comes true after two index holes are
            // sensed, or _three_ revolutions for a double density floppy!
            var startup = (_isDoubleDensity ? 3 : 2) * (1 / (Specs.RPM / 60.0));

            Log.Info(Category.FloppyDisk, "Drive will come ready in {0:n} seconds", startup);

            _loadDelayEvent = _scheduler.Schedule((ulong)startup * Conversion.MsecToNsec, (skewNsec, context) =>
            {
                _loadDelayEvent = null;
                _cylinder = 1;
                _head = 0;
                _ready = true;
                Log.Info(Category.FloppyDisk, "{0} online: {1}", Info.Description, Geometry);
            });

            base.OnLoad();
        }

        public override void Unload()
        {
            Log.Info(Category.FloppyDisk, "Floppy is about to eject...");

            // DiskChange will signal the FDC at the next Poll that the floppy
            // was ejected.  On the cable interface most of the drive signals
            // are pulled up, so we set some relevant ones here accordingly
            _ready = false;
            _fault = false;
            _driveSelect = false;
            _diskChange = true;
            _isSingleSided = true;
            _isDoubleDensity = false;

            // Reset to show unloaded status
            Info = DeviceInfo.SA851;
            Geometry = DeviceGeometry.NoMedia;

            base.Unload();
        }

        private bool _ready;
        private bool _fault;
        private bool _diskChange;
        private bool _driveSelect;
        private bool _isSingleSided;
        private bool _isDoubleDensity;

        private ushort _cylinder;
        private byte _head;

        private SchedulerEvent _seekDelayEvent;
        private SchedulerEvent _loadDelayEvent;
        private Scheduler _scheduler;
    }
}
