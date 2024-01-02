//
//  DeviceGeometry.cs
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

namespace PERQmedia
{
    /// <summary>
    /// Describes the basic geometry of a storage device:  cylinders, heads (aka
    /// tracks), sectors (per track) and the size (in bytes) of each sector.  For
    /// hard drives, the extra filesystem metadata ("logical header") is set too.
    /// </summary>
    /// <remarks>
    /// PERQ hard drives are formatted with non-standard 528-byte sectors.  The
    /// hardware supports a typical 512-byte data sector with an extra 16-byte
    /// "logical header" containing filesystem metadata, used by Scavenger to
    /// recover damaged partitions.  PERQ filesystem floppies simulate this
    /// header by allocating one block per track to hold all the headers for
    /// that track's sectors.
    /// 
    /// Tape drives are described here as long, skinny disk drives. :-)  The
    /// on-tape format of a tape block includes gap bytes, sync mark, address
    /// and CRC fields totaling 528.5 bytes per block!  However, the drive's
    /// controller hides all that and just transfers fixed-size 512 byte data
    /// blocks.  We use a 4-byte header to capture file marks, bad blocks, gaps,
    /// or other metadata for interop with .TAP files.
    /// </remarks>
    public struct DeviceGeometry
    {
        public DeviceGeometry(ushort cyls, byte heads, ushort secs, ushort secSize, byte hdrSize = 0)
        {
            Cylinders = cyls;
            Heads = heads;
            Sectors = secs;
            SectorSize = secSize;
            HeaderSize = hdrSize;
        }

        public override string ToString()
        {
            return $"[Cylinders {Cylinders}, Heads {Heads}, Sectors {Sectors} @ {SectorSize} + {HeaderSize} bytes]";
        }

        //
        // These sizes reflect the 16-bit nature of the PERQ-era hardware.
        //
        public readonly ushort Cylinders;
        public readonly byte Heads;
        public readonly ushort Sectors;
        public readonly ushort SectorSize;
        public readonly byte HeaderSize;

        // Convenience
        public int TotalBlocks => Cylinders * Heads * Sectors;
        public int TotalBytes => TotalBlocks * (SectorSize + HeaderSize + Sector.SECTOR_OVERHEAD);

        //
        // An "empty" geometry to support drives with removable media.
        //
        public static DeviceGeometry NoMedia = new DeviceGeometry(0, 1, 0, 0);

        //
        // The PERQ only officially supported 8" floppies, so pre-define the
        // standard floppy geometries.  (Note:  SS/DD was not commonly used
        // but the Floppy program doesn't disallow formatting a DSDD floppy
        // with just one side...)
        //
        public static DeviceGeometry SSSD = new DeviceGeometry(77, 1, 26, 128);
        public static DeviceGeometry DSSD = new DeviceGeometry(77, 2, 26, 128);
        public static DeviceGeometry SSDD = new DeviceGeometry(77, 1, 26, 256);
        public static DeviceGeometry DSDD = new DeviceGeometry(77, 2, 26, 256);

        //
        // All PERQ-1s came with Shugart SA4000-series drives in either the
        // 12MB (SA4004) or 24MB (SA4008) variety.
        //
        public static DeviceGeometry Shugart12 = new DeviceGeometry(202, 4, 30, 512, 16);
        public static DeviceGeometry Shugart24 = new DeviceGeometry(202, 8, 30, 512, 16);

        //
        // All of the standard 8", 5.25" and even SMD drives are loaded or
        // defined at run-time.
        //

        //
        // PERQ only "officially" supported the Archive Sidewinder streaming
        // tape drive; standard geometry is the 450' DC-300XL (20MB) cartridge.
        // 
        public static DeviceGeometry QIC20 = new DeviceGeometry(1, 4, 10240, 512, 4);
    }
}
