//
// ConfigTypes.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.Config
{
    //
    // See Notes/Configuration.txt for details.
    //

    public enum ChassisType
    {
        PERQ1 = 1,
        PERQ2,
        PERQ2T2
    }

    public enum CPUType
    {
        PERQ1 = 0,
        PERQ1A,
        PERQ24,
        PERQ24A
    }

    public enum IOBoardType
    {
        IOB = 0,
        CIO,
        NIO,
        EIO
    }

    public enum OptionBoardType
    {
        None = 0,
        Ether3,
        OIO,
        MLO
    }

    [Flags]
    public enum IOOptionType
    {
        None = 0x00,
        Link = 0x01,
        Ether = 0x02,
        Canon = 0x04,
        EthCan = 0x06,      // Shortcut for popular option
        Tape = 0x08,
        LinkTape = 0x09,    // "Universal Streamer board" (CPU Option slot)
        CanTape = 0x0c,     // Another shortcut, useful on PERQ-2s w/EIO
        EthCanTape = 0x0e,  // Rare, but not unusual
        SMD = 0x10          // EIO/PERQ2 only (on MLO board)
    }

    public enum DisplayType
    {
        Portrait,
        Landscape
    }

    [Flags]
    public enum TabletType
    {
        None = 0x0,
        Kriz = 0x1,
        BitPad = 0x2,
        Both = 0x3
    }

    public enum DriveType
    {
        None = 0,
        Floppy,
        Disk14Inch,
        Disk8Inch,
        Disk5Inch,
        DiskSMD,
        TapeQIC,
        Tape9Track
    }

    /// <summary>
    /// Holds a record of a media file containing a mountable storage device image.
    /// This isn't real well thought out yet.
    /// </summary>
    public struct StorageConfiguration
    {
        public StorageConfiguration(DriveType t, int unit, string path)
        {
            Device = t;
            Unit = unit;
            MediaPath = path;
        }

        public int Unit { get; set; }
        public DriveType Device { get; set; }
        public string MediaPath { get; set; }
    }
}
