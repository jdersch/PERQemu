// shugartdisk.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.PhysicalDisk
{
    //
    // Represents the contents of a 12 or 24MB Shugart SA4000-series hard disk.
    //
    public sealed class ShugartDisk : HardDisk
    {
        public ShugartDisk(bool is24mb) : base()
        {
            _diskType = is24mb ? DiskGeometry.Shugart24 : DiskGeometry.Shugart12;

            CreateSectors();
        }

        //
        // Header format for PERQemu hard disk images is:
        //
        // 4 bytes - "PERQ"
        // 1 byte  - sector header present (1 = present, 0 = absent)
        // 2 bytes - cylinders (MSB first)
        // 1 bytes - sectors
        // 1 bytes - heads
        //
        public override void ReadHeader(System.IO.FileStream fs)
        {
            // Ensure stream begins with cookie
            byte[] perq = new byte[4];

            fs.Read(perq, 0, 4);

            for (int i = 0; i < 4; i++)
            {
                if (perq[i] != _cookie[i])
                {
                    throw new InvalidOperationException("This is not a valid PERQemu hard disk image.");
                }
            }

            // Ensure sector header data is present
            if (fs.ReadByte() != 1)
            {
                throw new InvalidOperationException("Sector header data must be present for a Shugart disk image.");
            }

            // Ensure cyl/sec/tracks matches
            int cylinders = (fs.ReadByte() << 8) | fs.ReadByte();
            int sectors = fs.ReadByte();
            int tracks = fs.ReadByte();

            if (cylinders != _diskType.Cylinders ||
                sectors != _diskType.Sectors ||
                tracks != _diskType.Tracks)
            {
                throw new InvalidOperationException(
                            String.Format("Disk geometry (CHS {0}/{1}/{2}) is incorrect for a Shugart disk image.",
                                           cylinders, sectors, tracks));
            }
        }

        public override void WriteHeader(System.IO.FileStream fs)
        {
            fs.Write(_cookie, 0, 4);

            fs.WriteByte(1);    // Sector header present on Shugart disks

            fs.WriteByte((byte)((_diskType.Cylinders & 0xff00) >> 8));
            fs.WriteByte((byte)(_diskType.Cylinders & 0xff));

            fs.WriteByte((byte)_diskType.Sectors);
            fs.WriteByte((byte)_diskType.Tracks);
        }

        private static byte[] _cookie = { (byte)'P', (byte)'E', (byte)'R', (byte)'Q' };
    }
}

