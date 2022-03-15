//
//  DeviceGeometry.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022, Boondoggle Heavy Industries, Ltd.
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
    /// Tape drives may be described here as long, skinny disk drives. :-)  For
    /// a DC-300XL QIC tape, one cylinder with four heads and ~10000 sectors
    /// yields ~20MB total capacity.  Curiously, if we simulated the gap, sync
    /// mark, address and CRC fields accurately, we'd use 528.5 bytes per block!
    /// The Streamer isn't implemented in PERQemu yet, however.  Reel-to-reel
    /// 9-track drives attached to a Multibus card are a long, long ways off...
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
            return string.Format($"[Cylinders {Cylinders}, Heads {Heads}, Sectors {Sectors} @ {SectorSize} + {HeaderSize} bytes]");
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
        // To support drives with removable media, define an "empty" geometry
        //
        public static DeviceGeometry NoMedia = new DeviceGeometry(0, 1, 0, 0);

        //
        // As the PERQ only officially supported 8" floppies, we can pre-define
        // the standard floppy geometries 3RCC used.  (Note that SS/DD was not
        // really a thing but I don't see anything in the Floppy program that
        // disallows formatting a DSDD floppy with just one side...)
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
    }
}

/*
 * Notes - Additional interesting drive possibilities
 *
 * Look into the M224xB series -- 5.25" half-height drives from 32-86MB.  The
 * "B" suffix indicates an SA4000 interface!!  The M2243T is straight MFM (and
 * those have popped up on Ebay now and again).  That'd be wild if a non-Shugart
 * SA4000 drive could be plugged into a PERQ-1!!
 * 
 * The Fujitsu M2301B and M2302B look like 8" equivalents of the SA4004 and
 * SA4008.  Two more to keep an eye out for!  The M2301-2BE are slightly newer?
 * And the M2303BE is a 48MB SA4104 clone!  Dammit, 3RCC, these were available
 * in 1982.  They were interface-compatible but faster and smaller, drawing only
 * 60-70W.  Sigh.  More SA4000 options: M223XB and BH series (in 6-26MB range
 * but 5.25" half-high).
 * 
 * There are some 3.5" ST-506/MFM options too.  The Toshiba MK134 (above) is the
 * highest capacity; the Fujitsu M222X series is 25-51MB with typical MFM specs.
 * But with the smaller form factor it'd be fun to build a 4-drive PERQ-2!
 * 
 * Fujitsu's 8" SMD lineup ranged from the M232X, M233X and M234X in capacities
 * ranging from 85MB to 690MB.  The M237X and M238X 8" SMD / ESMD drives round
 * out the high end if we want to get completely crazy.  458-1000MB and 3MB/sec.
 * Not likely we'll ever get SMD support working, but maybe for PERQ-2.0? :-)
 * 
*/
