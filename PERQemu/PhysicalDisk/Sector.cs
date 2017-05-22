// sector.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.IO;

namespace PERQemu.PhysicalDisk
{
    /// <summary>
    /// Represents a physical disk sector, which is where the actual
    /// data lives.  Woo!
    /// </summary>
    public sealed class Sector
    {
        public Sector(int cylID, int trackID, int sectorID, DiskGeometry geometry)
        {
            if (sectorID < 0)
            {
                throw new ArgumentOutOfRangeException("sectorID");
            }

            _cylinderID = cylID;
            _trackID = trackID;
            _sectorID = sectorID;

            _geometry = geometry;

            _sectorBad = false;
            _rawHeader = new byte[16];
            _sectorData = new byte[_geometry.SectorSize];
        }

        public byte[] Data
        {
            get { return _sectorData; }
        }

        public byte[] Header
        {
            get { return _rawHeader; }
        }

        public bool IsBad
        {
            get { return _sectorBad; }
        }

        /// <summary>
        /// Loads this sector's data from disk.
        /// This assumes that the incoming FileStream is currently
        /// pointing to the data for this sector.
        /// </summary>
        /// <param name="fs"></param>
        public void Load(FileStream fs)
        {
            if (_geometry.HasBlockHeader)
            {
                // Read the sector status (good/bad)
                _sectorBad = fs.ReadByte() == 1;

                // Read the header data
                if (fs.Read(_rawHeader, 0, 16) != 16)
                {
                    throw new InvalidOperationException("Out of data when reading sector header.");
                }
            }

            // Read the sector data
            if (fs.Read(_sectorData, 0, (int)_geometry.SectorSize) != _geometry.SectorSize)
            {
                throw new InvalidOperationException("Out of data when reading sector data.");
            }
        }

        /// <summary>
        /// Saves this sector's data to disk.
        /// This assumes that the incoming FileStream is currently
        /// pointing to the data for this sector.
        /// </summary>
        /// <param name="fs"></param>
        public void Save(FileStream fs)
        {
            if (_geometry.HasBlockHeader)
            {
                // Write the bad sector flag
                fs.WriteByte(_sectorBad ? (byte)1 : (byte)0);

                // Write the header data
                fs.Write(_rawHeader, 0, 16);
            }

            // Write the sector data
            fs.Write(_sectorData, 0, (int)_geometry.SectorSize);
        }

        public byte ReadByte(int offset)
        {
            if (offset < 0 || offset >= _geometry.SectorSize)
            {
                throw new ArgumentOutOfRangeException("offset");
            }

            return _sectorData[offset];
        }

        public bool IsFloppySector
        {
            get
            {
                return !_geometry.HasBlockHeader; // HACK!
            }
        }

        private bool _sectorBad;
        private byte[] _rawHeader;
        private byte[] _sectorData;
        private int _cylinderID;
        private int _trackID;
        private int _sectorID;
        private DiskGeometry _geometry;
    }
}

