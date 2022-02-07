//
// EIO.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
    /// Represents an EIO or NIO card in a PERQ2 system.  This contains hardware
    /// for a Micropolis or MFM disk controller, an Ethernet controller (not
    /// present on the NIO) and a Z80 for controlling low-speed devices.
    /// </summary>
    public sealed class EIO : IOBoard
    {
        static EIO()
        {
            _name = "EIO";
            _desc = "PERQ-2 I/O Board, new Z80, MFM, Ethernet";

            _z80CycleTime = 250;    // 4Mhz!

            //
            // The EIO schematic very clearly shows an 8Kx8 ROM (2764) chip
            // and 8 x 16Kx1 SRAMs (2167s) but the ROM that's built seems to
            // limit the Z80 assembler's view to 4K ROM and several other docs
            // show that only 4K is used.  Since we're (eventually/ultimately)
            // going to load from a actual ROM dumps, the ROM loader wants to
            // match the RomSize to the length of the actual file...
            //
            _z80RamSize = 0x4000;   // 16K of RAM
            _z80RamAddr = 0x4000;
            _z80RomSize = 0x1000;   // 4K of ROM
            _z80RomAddr = 0x0;

            // TODO: load the correct Z80 ROM (eio vs. eio24?)
            // TODO: identify the ports, memory config, addresses
            // TODO: write the Micropolis and MFM controllers
            // TODO: determine if 24-bit version or NIO version
            //       warrants a separate class entirely... like
            //       the "EIO5" for T2/T4 systems with MFM, and
            //       plain EIO for Microp in T0/T1 configs
        }

        public EIO(PERQSystem system) : base(system)
        {
            _hardDiskController = new ShugartDiskController(system);

            // _ethernetController = new EthernetController(system);

            _z80System = new Z80System(system);
            _z80System.LoadZ80ROM("eioz80.bin");      // "new" Z80 ROM

            RegisterPorts(_handledPorts);
        }

        /// <summary>
        /// Reads a word from the given I/O port.
        /// </summary>
        public override int IORead(byte port)
        {
            switch (port)
            {
                // TODO: update all of this for the EIO!

                //case 0x40:  // Read disk status
                //    return _hardDiskController.ReadStatus();

                //case 0x46:  // Read Z80 data
                //    return _z80System.ReadData();

                //case 0x55:  // Read Z80 status -- not used in IOB/CIO -- remove
                //    return _z80System.ReadStatus();

                default:
                    Log.Warn(Category.IO, "Unhandled EIO Read from port {0:x2}", port);
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
                // TODO: update all of this for the EIO!

                //case 0xc1:  // Shugart command/control register & Z80 status register
                //    _hardDiskController.LoadCommandRegister(value);
                //    _z80System.WriteStatus(value);
                //    break;

                //case 0xc2:  // Shugart Head register
                //    _hardDiskController.LoadHeadRegister(value);
                //    break;

                //case 0xc7:  // Z80 data port
                //    _z80System.WriteData(value);
                //    break;

                //case 0xc8:  // Shugart Cylinder/Sector register
                //    _hardDiskController.LoadCylSecRegister(value);
                //    break;

                //case 0xc9:  // Shugart File SN Low Register
                //    _hardDiskController.LoadSerialLowRegister(value);
                //    break;

                //case 0xca:  // Shugart File SN High register
                //    _hardDiskController.LoadSerialHighRegister(value);
                //    break;

                //case 0xcb:  // Shugart Block Number register
                //    _hardDiskController.LoadBlockRegister(value);
                //    break;

                //case 0xd0:  // Shugart Data Buffer Address High register
                //    _hardDiskController.LoadDataBufferAddrHighRegister(value);
                //    break;

                //case 0xd1:  // Shugart Header Address High register
                //    _hardDiskController.LoadHeaderAddrHighRegister(value);
                //    break;

                //// 0xd4,d5,dc,dd: load DMA registers -- Canon, Streamer interfaces?

                //case 0xd8:  // Shugart Data Buffer Address Low register
                //    _hardDiskController.LoadDataBufferAddrLowRegister(value);
                //    break;

                //case 0xd9:  // Shugart Header Address low register
                //    _hardDiskController.LoadHeaderAddrLowRegister(value);
                //    break;

                default:
                    Log.Warn(Category.IO, "Unhandled EIO Write to port {0:x2}, data {1:x4}", port, value);
                    break;
            }
        }

        /// <summary>
        /// Ports handled by the EIO.  (But no Ethernet on NIO.)
        /// </summary>
        private byte[] _handledPorts =
        {
            // Z80
            0x54,       // 124 EioZ80In: dismiss Z80 interrupt
            0x55,       // 125 EioZ80Stat: read Z80 interface status
            0xc4,       // 304 EioZ80Out: PERQ->Z80 send byte
            0xc5,       // 305 EioZ80Ctrl: Z80 control register

            // DMA
            0xc0,       // 300 ChanSel (E10EWrDMAChn): DMA Ch# Selector
            0xd4,       // 324 DmaDbLo (E10EWrBufLo): load buf addr lo
            0xd5,       // 325 DmaDbHi (E10EWrBufHi): load buf addr hi
            0xd6,       // 326 DmaLhLo (E10EWrHdrLo): load hdr addr lo
            0xd7,       // 327 DmaLhHi (E10EWrHdrHi): load hdr addr hi

            // Hard disk
            0x53,       // 123 DskStat (SMStat): read disk status reg
            0xd0,       // 320 ConstPtr: constant register selector
            0xd1,       // 321 RamFile: constreg load and incr selector
            0xd2,       // 322 SmCtl: state machine control register
            0xd3,       // 323 DskCtl: disk control register

            // Ethernet
            0x52,       // 122 E10ERdNetSR: read net status register
            0x5a,       // 132 E10ERdBCLow: read bit count low byte
            0x5b,       // 133 E10ERdBCHgh: read bit count high byte
            0xc2,       // 302 E10EWrNetCR: load net control register
            0xc3,       // 303 E10EWrIntEnb: load net interrupt enable reg
            0xc8,       // 310 E10EWrNA0: load net addr low word byte 6
            0xc9,       // 311 E10EWrNA1: load net addr low word byte 5
            0xca,       // 312 E10EWrMCCmd: load multicast command byte
            0xcb,       // 313 E10EWrMC0: load multcast reg grp 0
            0xcc,       // 314 E10EWrMC1: load multcast reg grp 1
            0xcd,       // 315 E10EWrMC2: load multcast reg grp 2
            0xce,       // 316 E10EWrMC3: load multcast reg grp 3
            0xcf,       // 317 E10EWrMC4: load multcast reg grp 4
            0xd8,       // 330 E10OWrBCCR: load bit count control reg
            0xd9,       // 331 E10OWrBCHgh: load bit count high byte
            0xda,       // 332 E10OWrBCLow: load bit count low byte 
            0xdc,       // 334 E10EWrUSCR: load usec clock control
            0xdd,       // 335 E10EWrUSHgh: load usec clock high byte
            0xde        // 336 E10EWrUSLow: load usec clock low byte
        };

        /// <remarks>
        /// These EIO registers are not implemented:
        /// 120     50      FPSat           read floating point status
        /// 121     51      FPResult        read floating point result
        /// 234     9C      AdrReg*         enable state machine addr reg
        /// 235     9D      WrtRam*         load ethernet test mumble
        /// 247     A7      DMATest         load DMA test reg
        /// 301     C1      FPInst          load floating point data
        /// 346     E6      *               "put ones on the iob"
        /// They are for the FPU (never shipped) and hardware diagnostics,
        /// so it's not likely they'll ever be needed except when poked at
        /// by very obscure bits of test microcode.  See NewIOPorts.txt.
        /// </remarks>
    }
}
