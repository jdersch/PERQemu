//
// IOB.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.Z80;
using PERQemu.IO.DiskDevices;

namespace PERQemu.IO
{
    /// <summary>
    /// Represents an IOB card in a PERQ1 system.  This contains hardware for
    /// the Shugart disk controller and a Z80 for controlling low-speed devices.
    /// </summary>
    public sealed class IOB : IOBoard
    {
        static IOB()
        {
            _name = "IOB";
            _desc = "PERQ-1 I/O Board, old Z80, Shugart";

            _z80CycleTime = 407;    // 2.4576Mhz

            _z80RamSize = 0x400;    // 1K of RAM
            _z80RamAddr = 0x2c00;
            _z80RomSize = 0x2000;   // 8K of ROM
            _z80RomAddr = 0x0;
        }

        public IOB(PERQSystem system) : base(system)
        {
            _hardDiskController = new ShugartDiskController(_sys);

            _z80System = new CIOZ80(_sys);
            _z80System.LoadZ80ROM("oioz80.bin");        // "old" Z80 ROM

            // Set up the mapping of IO port -> DMA register for IOB
            _dmaRegisters.Assign(0xd0, 0xd8, 0xd1, 0xd9);
            _dmaRegisters.Assign(0xd2, 0xda, 0xd3, 0xdb);
            _dmaRegisters.Assign(0xd4, 0xdc, 0xd5, 0xdd);
            _dmaRegisters.Assign(0xd6, 0xde, 0xd7, 0xdf);

            RegisterPorts(_handledPorts);
        }

        /// <summary>
        /// Reads a word from the given I/O port.
        /// </summary>
        public override int IORead(byte port)
        {
            switch (port)
            {
                case 0x40:    // Read disk status
                    return _hardDiskController.ReadStatus();

                case 0x46:    // Read Z80 data
                    return _z80System.ReadData();

                default:
                    Log.Warn(Category.IO, "Unhandled IOB Read from port {0:x2}", port);
                    return 0xffff;
            }
        }

        /// <summary>
        /// Writes a word to the given I/O port.
        /// </summary>
        public override void IOWrite(byte port, int value)
        {
            switch (port)
            {
                case 0xc1:      // Shugart command/control register & Z80 status register
                    _hardDiskController.LoadRegister(port, value & 0x7f);
                    _z80System.WriteStatus(value & 0x80);
                    break;

                case 0xc7:      // Z80 data port
                    _z80System.WriteData(value);
                    break;

                case 0xc2:      // Shugart Head register
                case 0xc8:      // Shugart Cylinder/Sector register
                case 0xc9:      // Shugart File SN Low Register
                case 0xca:      // Shugart File SN High register
                case 0xcb:      // Shugart Block Number register
                    _hardDiskController.LoadRegister(port, value);
                    break;

                case 0xd0:      // DMA Registers
                case 0xd1:
                case 0xd8:
                case 0xd9:
                    _dmaRegisters.LoadRegister(ChannelName.HardDisk, port, value);
                    break;

                case 0xd2:
                case 0xd3:
                case 0xda:
                case 0xdb:
                    _dmaRegisters.LoadRegister(ChannelName.Network, port, value);
                    break;

                case 0xd4:
                case 0xd5:
                case 0xdc:
                case 0xdd:
                    _dmaRegisters.LoadRegister(ChannelName.ExtB, port, value);
                    break;

                case 0xd6:
                case 0xd7:
                case 0xde:
                case 0xdf:
                    _dmaRegisters.LoadRegister(ChannelName.ExtA, port, value);
                    break;

                default:
                    Log.Warn(Category.IO, "Unhandled IOB Write to port {0:x2}, data {1:x4}", port, value);
                    break;
            }
        }

        /// <summary>
        /// Ports handled by the IOB.
        /// </summary>
        byte[] _handledPorts =
        {
            // Z80
            0x46,       // 106 CioZ80In: read Z80 input
            0xc1,       // 301 CioZ80Start: load Z80 control register
            0xc7,       // 307 CioZ80Out: load Z80 output

            // Hard disk
            0x40,       // 100 DskStat: read disk status register
            0xc2,       // 302 DskHead: load Shugart head register
            0xc8,       // 310 DskCylSec: load Shugart cylinder/sector register
            0xc9,       // 311 DskFSNlo: (FileL) load Shugart file serial # low bits
            0xca,       // 312 DskFSNhi: (FileH)   "     "      "     "    high bits
            0xcb,       // 313 DskLBN: (Block) load Shugart logical block #

            // DMA
            0xd0,       // 320 Disk data addr, high bits (Shugart)
            0xd1,       // 321 Disk header addr, high
            0xd2,       // 322 Net data addr, high (unused)
            0xd3,       // 323 Net header addr, high
            0xd4,       // 324 ExtB data addr, high (Canon)
            0xd5,       // 325 ExtB header addr, high
            0xd6,       // 326 ExtA data addr, high (Ether3Mbit or OIO Ether10Mbit)
            0xd7,       // 327 ExtA header addr, high
            0xd8,       // 330 Disk data addr, low bits
            0xd9,       // 331 Disk header addr, low
            0xda,       // 332 Net data addr, low (unused)
            0xdb,       // 333 Net header addr, low
            0xdc,       // 334 ExtB data addr, low (Canon)
            0xdd,       // 335 ExtB header addr, low
            0xde,       // 336 ExtA data addr, low (Ether3Mbit or OIO Ether10Mbit)
            0xdf        // 337 ExtA header addr, low
        };
    }
}
