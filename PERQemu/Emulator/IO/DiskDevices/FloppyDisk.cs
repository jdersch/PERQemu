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
using System.IO;
using System.Text;

using PERQmedia;
using PERQemu;
using PERQemu.IO;

namespace PERQemu.IO.DiskDevices
{
    /// <summary>
    /// Emulates the mechanical operation of a Shugart SA851 8" floppy drive
    /// and access to the floppy's data (if loaded).  Provides a Track 
    /// abstraction that the FDC finds useful for some operations.
    /// </summary>
    public class FloppyDisk : StorageDevice
    {
        public FloppyDisk(Scheduler sched, string filename) : base(filename)
        {
            _scheduler = sched;
            _tracks = new Track[2, 77];     // temp HACK do this more elegantly

            _ready = false;
            _diskChange = false;
            _driveSelect = false;
            _isSingleSided = false;
            _isDoubleDensity = false;

            _cylinder = 1;      // Gently force a Step Out to find track 0 on startup
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
            Log.Debug(Category.FloppyDisk, "Drive reset.");
        }

        public ushort Cylinder => _cylinder;

        public bool Fault => false;     // For now ("Equipment Check")
        public bool Ready => (IsLoaded && _ready);
        public bool Track0 => (_cylinder == 0);
        public bool IsSingleSided => _isSingleSided;
        public bool IsDoubleDensity => _isDoubleDensity;
        public bool DiskChange => _diskChange;

        public bool DriveSelect
        {
            get { return _driveSelect; }
            set
            {
                _driveSelect = value;

                // The Disk Change signal is reset when Drive Select goes low
                if (!_driveSelect)
                {
                    _diskChange = false;
                }
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
                    if (IsLoaded && _loadDelayEvent == null)
                    {
                        _ready = true;
                    }
                }
            }
        }

        public void SeekTo(ushort track, SchedulerEventCallback cb)
        {
            // Clip cylinder count into range and compute seek delay
            var cyls = Math.Abs(track - _cylinder);
            var delay = Math.Min(Specs.MinimumSeek * (cyls + 1), Specs.MaximumSeek);

            // Schedule the callback (to fire on the FDC) to signal seek complete
            _seekDelayEvent = _scheduler.Schedule((ulong)delay * Conversion.MsecToNsec, cb);

            // Make sure we don't fly off the end
            _cylinder = Math.Min(track, Geometry.Cylinders);
            Log.Debug(Category.FloppyDisk, "[Drive seek to cylinder {0} in {1}ms]", _cylinder, delay);
        }

        /// <summary>
        /// Returns sector data for the given address.
        /// </summary>
        public Sector GetSector(ushort cylinder, byte head, ushort sector)
        {
            return Sectors[cylinder, head, sector];
        }

        public Track GetTrack(ushort cylinder, byte head)
        {
            return _tracks[head, cylinder];
        }

        public void SetTrack(ushort cylinder, byte head, Track track)
        {
            _tracks[head, cylinder] = track;
        }

        /// <summary>
        /// Formats the given track with the specified sector count, size, and format.
        /// </summary>
        public void FormatTrack(ushort cylinder, byte head, ushort sectorCount, ushort sectorSize)
        {
            _tracks[head, cylinder] = new Track(cylinder, head, sectorCount, sectorSize);
        }

        /// <summary>
        /// On load, check and set some flags 'n stuff.
        /// </summary>
        public override void OnLoad()
        {
            _ready = false;
            _diskChange = true;
            _isSingleSided = (Geometry.Heads == 1);
            _isDoubleDensity = (Geometry.SectorSize == 256);

            // SA851 manual says that Ready comes true after two index holes are sensed
            // (it's actually _three_ revolutions for a double density floppy!)
            var startup = 2 * (1 / (Specs.RPM / 60.0)) * Conversion.MsecToNsec;

            Log.Debug(Category.FloppyDisk, "Floppy will come ready in {0:n} seconds", startup / 10e6);

            _loadDelayEvent = _scheduler.Schedule((ulong)startup, (skewNsec, context) =>
            {
                Log.Debug(Category.FloppyDisk, "Floppy online: {0}", Geometry.ToString());
                _loadDelayEvent = null;
                _ready = true;
            });

            base.OnLoad();
        }

        private bool _ready;
        private bool _diskChange;
        private bool _driveSelect;
        private bool _isSingleSided;
        private bool _isDoubleDensity;

        private ushort _cylinder;
        private byte _head;

        private Track[,] _tracks;

        private Event _seekDelayEvent;
        private Event _loadDelayEvent;
        private Scheduler _scheduler;
    }


    /// <summary>
    /// Represents a single track's worth of sectors.  Overlays the underlying
    /// StorageDevice which provides the actual data storage.
    /// </summary>
    public class Track
    {
        /// <summary>
        /// Create a new, unformatted track with the specified format, sector
        /// size and sector count.  Used when formatting a track.
        /// </summary>
        public Track(ushort cylinder, byte head, ushort sectorCount, ushort sectorSize)
        {
            _cylinder = cylinder;
            _head = head;
            _sectorCount = sectorCount;
            _sectorSize = sectorSize;

            // Create new sector dictionary and sector ordering map, but do not
            // populate; this will be done as sectors are added via AddSector.
            _sectors = new Dictionary<int, Sector>();
            _sectorOrdering = new List<int>(_sectorCount);
        }

        public void AddSector(ushort sector, Sector newSector)
        {
            if (_sectorOrdering.Count == _sectorCount)
            {
                throw new InvalidOperationException("Track full.");
            }

            _sectorOrdering.Add(sector);
            _sectors[sector] = newSector;
        }

        public Sector ReadSector(ushort sector)
        {
            Sector s;
            _sectors.TryGetValue(sector, out s);
            return s;
        }

        private ushort _cylinder;
        private byte _head;
        private ushort _sectorCount;
        private ushort _sectorSize;

        private List<int> _sectorOrdering;
        private Dictionary<int, Sector> _sectors;
    }
}
