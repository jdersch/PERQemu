//
//  Sector.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022-2024, Boondoggle Heavy Industries, Ltd.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Runtime.CompilerServices;

namespace PERQmedia
{
    /// <summary>
    /// Represents a physical disk sector, which is where the actual data lives.
    /// Each record contains its own physical address and "knows" if it has been
    /// marked as bad (such as from a media defect during archiving).
    /// </summary>
    public struct Sector
    {
        public Sector(ushort cylID, byte headID, ushort secID, ushort secSize, byte hdrSize = 0, bool defect = false)
        {
            _cylinderID = cylID;
            _headID = headID;
            _sectorID = secID;
            _sectorBad = defect;
            _sectorData = new byte[secSize];
            _rawHeader = new byte[hdrSize];
        }

        public ushort CylinderID
        {
            get { return _cylinderID; }
            set { _cylinderID = value; }
        }

        public byte HeadID
        {
            get { return _headID; }
            set { _headID = value; }
        }

        public ushort SectorID
        {
            get { return _sectorID; }
            set { _sectorID = value; }
        }

        public bool IsBad
        {
            get { return _sectorBad; }
            set { _sectorBad = value; }
        }

        public byte[] Data => _sectorData;
        public byte[] Header => _rawHeader;

        public override string ToString()
        {
            return string.Format("[Sector: CylID={0}, HeadID={1}, SecID={2}, Data={3}, Header={4}, Bad={5}]",
                                 CylinderID, HeadID, SectorID, Data.Length, Header.Length, IsBad);
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte ReadByte(uint offset)
        {
            try
            {
                return _sectorData[offset];
            }
            catch
            {
                throw new ArgumentOutOfRangeException(nameof(offset));
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteByte(uint offset, byte value)
        {
            try
            {
                _sectorData[offset] = value;
            }
            catch
            {
                throw new ArgumentOutOfRangeException(nameof(offset));
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public byte ReadHeaderByte(uint offset)
        {
            try
            {
                return _rawHeader[offset];
            }
            catch
            {
                throw new ArgumentOutOfRangeException(nameof(offset));
            }
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void WriteHeaderByte(uint offset, byte value)
        {
            try
            {
                _rawHeader[offset] = value;
            }
            catch
            {
                throw new ArgumentOutOfRangeException(nameof(offset));
            }
        }

        // Account for the header and flag bytes when
        // computing the size of a sector.  This is a hack.
        public static readonly int SECTOR_OVERHEAD = 6;

        ushort _cylinderID;
        byte _headID;
        ushort _sectorID;
        byte[] _rawHeader;
        byte[] _sectorData;
        bool _sectorBad;
    }
}
