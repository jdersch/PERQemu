//
// ConfigTypes.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
        PERQ2Tx
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
        Link = 0x01,        // Universal
        Ether = 0x02,       // OIO only
        Canon = 0x04,       // OIO or MLO
        Tape = 0x08,        // OIO or MLO Streamer or MLO 9-track
        SMD = 0x10          // MLO only
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
}
