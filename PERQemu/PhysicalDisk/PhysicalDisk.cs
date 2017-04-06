// physicaldisk.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace PERQemu.PhysicalDisk
{
    /// <summary>
    /// Represents the physical structure of a given disk (Shugart/Micropolis/floppy, etc...)
    /// </summary>
    [Serializable]
    public struct DiskGeometry
    {
        public DiskGeometry(uint cyls, uint tracks, uint secs, uint sectorSize, bool hasBlockHeader)
        {
            if (cyls < 0 || tracks < 0 || secs < 0)
            {
                throw new ArgumentOutOfRangeException("Invalid CHS specification, must be greater than zero.");
            }

            _cylinders = cyls;
            _tracks = tracks;
            _sectors = secs;
            _sectorSize = sectorSize;
            _hasBlockHeader = hasBlockHeader;
        }

        public uint Cylinders
        {
            get { return _cylinders; }
        }

        public uint Tracks
        {
            get { return _tracks; }
        }

        public uint Sectors
        {
            get { return _sectors; }
        }

        public uint SectorSize
        {
            get { return _sectorSize; }
        }

        public bool HasBlockHeader
        {
            get { return _hasBlockHeader; }
        }

        private uint _cylinders;
        private uint _tracks;
        private uint _sectors;
        private uint _sectorSize;
        private bool _hasBlockHeader;

        public static DiskGeometry Shugart12 = new DiskGeometry(202, 4, 30, 512, true);
        public static DiskGeometry Shugart24 = new DiskGeometry(202, 8, 30, 512, true);
        public static DiskGeometry Micropolis = new DiskGeometry(580, 5, 24, 512, true);
        public static DiskGeometry FloppySSSD = new DiskGeometry(77, 1, 26, 128, false);
        public static DiskGeometry FloppyDSSD = new DiskGeometry(77, 2, 26, 128, false);
        public static DiskGeometry FloppySSDD = new DiskGeometry(77, 1, 26, 256, false);
        public static DiskGeometry FloppyDSDD = new DiskGeometry(77, 2, 26, 256, false);
    }

    /// <summary>
    /// Represents a physical disk drive, which contains one or more Cylinders.
    /// This is the primary interface for interaction with the disk image, and allows
    /// reading sectors of data.
    /// </summary>
    [Serializable]
    public abstract class PhysicalDisk
    {
        /// <summary>
        /// Constructs a new PhysicalDisk, given the number of Cylinders on the drive,
        /// Tracks per Cylinder and Sectors per Track.
        /// </summary>
        /// <param name="cylinders"></param>
        /// <param name="tracks"></param>
        /// <param name="sectors"></param>
        public PhysicalDisk()
        {
            _loaded = false;
            _isWriteProtected = false;
        }

        public bool Loaded
        {
            get { return _loaded; }
        }

        /// <summary>
        /// Loads the disk image from the supplied filestream.
        /// </summary>
        /// <param name="fs"></param>
        public virtual void Load(FileStream fs)
        {
            ReadHeader(fs);

            for (int cyl = 0; cyl < _diskType.Cylinders; cyl++)
            {
                for (int track = 0; track < _diskType.Tracks; track++)
                {
                    for (int sector = 0; sector < _diskType.Sectors; sector++)
                    {
                        _sectors[cyl, track, sector].Load(fs);
                    }
                }
            }

            _loaded = true;
        }

        public virtual void Save(FileStream fs)
        {
            WriteHeader(fs);

            for (int cyl = 0; cyl < _diskType.Cylinders; cyl++)
            {
                for (int track = 0; track < _diskType.Tracks; track++)
                {
                    for (int sector = 0; sector < _diskType.Sectors; sector++)
                    {
                        _sectors[cyl, track, sector].Save(fs);
                    }
                }
            }
        }

        public virtual Sector GetSector(int cylinder, int track, int sector)
        {
            ValidateCHS(cylinder, track, sector);
            return _sectors[cylinder, track, sector];
        }

        public virtual void SetSector(Sector sec, int cylinder, int track, int sector)
        {
            ValidateCHS(cylinder, track, sector);
            _sectors[cylinder, track, sector] = sec;
        }

        public abstract void ReadHeader(FileStream fs);

        public abstract void WriteHeader(FileStream fs);

        /// <summary>
        /// Indicates the geometry of the drive
        /// </summary>
        public DiskGeometry DiskGeometry
        {
            get { return _diskType; }
        }

        public uint Cylinders
        {
            get { return _diskType.Cylinders; }
        }

        public uint Tracks
        {
            get { return _diskType.Tracks; }
        }

        public uint Sectors
        {
            get { return _diskType.Sectors; }
        }

        public bool IsWriteProtected
        {
            get { return _isWriteProtected; }
            set { _isWriteProtected = value; }
        }

        protected void CreateSectors()
        {
            _sectors = new Sector[Cylinders, Tracks, Sectors];

            for (int cyl = 0; cyl < Cylinders; cyl++)
            {
                for (int track = 0; track < Tracks; track++)
                {
                    for (int sector = 0; sector < Sectors; sector++)
                    {
                        _sectors[cyl, track, sector] = new Sector(cyl, track, sector, DiskGeometry);
                    }
                }
            }
        }

        private void ValidateCHS(int cylinder, int track, int sector)
        {
            if (cylinder < 0 || cylinder >= _diskType.Cylinders)
            {
                throw new ArgumentOutOfRangeException("cylinder");
            }

            if (track < 0 || track >= _diskType.Tracks)
            {
                throw new ArgumentOutOfRangeException("track");
            }

            if (sector < 0 || sector >= _diskType.Sectors)
            {
                throw new ArgumentOutOfRangeException("sector");
            }
        }

        protected Sector[, ,] _sectors;
        protected DiskGeometry _diskType;
        protected bool _isWriteProtected;
        private bool _loaded;
    }
}

