//
//  DeviceInfo.cs
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

using System;
using System.Collections.Generic;

namespace PERQmedia
{
    /// <summary>
    /// Device type codes identify which controller a storage device must attach
    /// to (in the emulator).  They mirror the codes used by the hardware and
    /// POS I/O software/microcode!
    /// </summary>
    /// <remarks>
    /// See: DiskDefs module, March 1984 Operating System doc, pg 49.  The EIO
    /// hardware actually uses types 0..3 (set by hardware jumper) to influence
    /// the disk state machine's operation.  Codes 4..7 are also defined by the
    /// module, though 8..15 are left as "reserved."  I've added the Streamer
    /// and MLO 9-track tape for PERQemu use, while leaving open more fanciful
    /// options such as GPIB or SCSI disk/tape and other blue-sky possibilities.
    /// </remarks>
    public enum DeviceType
    {
        D5Inch = 0,         // Various 5.25" -- EIO5 only
        Unused = 1,
        D14Inch = 2,        // Shugart hard drive -- IOB
        D8Inch = 3,         // Micropolis 8" -- CIO or EIO
        DSMD = 4,           // SMD -- MLO board
        DFloppy = 5,        // Shugart floppy drive -- All IO boards
        DCIOShugart = 6,    // The CIO board was an IOB with updated
        DCIOMicrop = 7,     // Z80 ROMs; also required a hardware mod?
        Reserved7 = 8,
        Reserved6 = 9,
        Reserved5 = 10,
        TStreamer = 11,     // QIC Streamer - OIO board
        T9Track = 12,       // 9-Track - MLO board (someday!?)
        Reserved2 = 13,
        Reserved1 = 14,
        Reserved0 = 15
    }

    /// <summary>
    /// Common device information.
    /// </summary>
    public struct DeviceInfo
    {
        public DeviceInfo(DeviceType type, string name, string desc = "",
                          bool canWrite = true, bool canBoot = false, bool canRemove = false)
        {
            Type = type;
            Name = name;
            Description = desc;

            IsWritable = canWrite;
            IsBootable = canBoot;
            IsRemovable = canRemove;
        }

        public string Name;
        public string Description;
        public DeviceType Type;

        public bool IsWritable;
        public bool IsBootable;
        public bool IsRemovable;
    }
}
