//
// IOB.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.HardDisk;
using PERQemu.IO.Z80;

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
            _hardDiskController = new ShugartDiskController(system);

            _z80System = new Z80System(system);
            _z80System.LoadZ80ROM("pz80.bin");      // "old" Z80 ROM

            RegisterPorts(_handledPorts);
        }

        /// <summary>
        /// Reads a word from the given I/O port.
        /// </summary>
        public override int IORead(byte port)
        {
            switch (port)
            {
                case 0x40:  // Read disk status
                    return _hardDiskController.ReadStatus();

                case 0x46:  // Read Z80 data
                    return _z80System.ReadData();

                default:
                    Log.Warn(Category.IO, "Unhandled IOB Read from port {0:x2}", port);
                    return 0xff;
            }
        }

        /// <summary>
        /// Writes a word to the given I/O port.
        /// </summary>
        public override void IOWrite(byte port, int value)
        {
            switch (port)
            {
                case 0xc1:  // Shugart command/control register & Z80 status register
                    _hardDiskController.LoadCommandRegister(value & 0x3f);  // Low 6 bits
                    _z80System.WriteStatus(value & 0xc0);                   // Hi 2 bits?
                    break;

                case 0xc2:  // Shugart Head register
                    _hardDiskController.LoadHeadRegister(value);
                    break;

                case 0xc7:  // Z80 data port
                    _z80System.WriteData(value);
                    break;

                case 0xc8:  // Shugart Cylinder/Sector register
                    _hardDiskController.LoadCylSecRegister(value);
                    break;

                case 0xc9:  // Shugart File SN Low Register
                    _hardDiskController.LoadSerialLowRegister(value);
                    break;

                case 0xca:  // Shugart File SN High register
                    _hardDiskController.LoadSerialHighRegister(value);
                    break;

                case 0xcb:  // Shugart Block Number register
                    _hardDiskController.LoadBlockRegister(value);
                    break;

                case 0xd0:  // Shugart Data Buffer Address High register
                    _hardDiskController.LoadDataBufferAddrHighRegister(value);
                    break;

                case 0xd1:  // Shugart Header Address High register
                    _hardDiskController.LoadHeaderAddrHighRegister(value);
                    break;

                // 0xd4,d5,dc,dd: load DMA registers -- Canon, Streamer interfaces?

                case 0xd8:  // Shugart Data Buffer Address Low register
                    _hardDiskController.LoadDataBufferAddrLowRegister(value);
                    break;

                case 0xd9:  // Shugart Header Address low register
                    _hardDiskController.LoadHeaderAddrLowRegister(value);
                    break;

                default:
                    Log.Warn(Category.IO, "Unhandled IOB Write to port {0:x2}, data {1:x4}", port, value);
                    break;
            }
        }

        /// <summary>
        /// Ports handled by the IOB.
        /// </summary>
        private byte[] _handledPorts =
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
            0xd0,       // 320 DatAdrH: load Shugart data buffer address high bits
            0xd1,       // 321 CWAdrH:    "     "    header  "      "      "    "
            0xd8,       // 330 DatAdrL: load Shugart data buffer address low bits
            0xd9,       // 331 CWAdrL:    "     "    header  "      "     "    "

            // DMA
            0xd4,       // 324 DMAhi: load DMA data buffer address high bits (Only used by Canon?)
            0xd5,       // 325 HDRhi:   "   "  header  "      "      "    "           "
            0xdc,       // 334 DMAlo: load DMA data buffer address low bits (Only used by Canon?)
            0xdd        // 335 HDRlo:   "   "  header  "      "     "    "           "
        };
    }
}
