//
// EIO.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;
using PERQemu.Config;
using PERQemu.IO.Z80;
using PERQemu.IO.Network;
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
            _desc = "PERQ-2 I/O Board, new Z80, Microp/MFM, Ethernet";

            _z80CycleTime = 250;    // 4Mhz!

            //
            // The EIO schematic very clearly shows that the board contains 8x
            // 16Kx1 SRAMs (2167s), but the EIO sources have very strange values
            // for the base address and length.  It must be that they reserve
            // around 10K for loading from the Zboot file at startup, and only
            // use 6K for data?  Why not use 16K ROM and avoid that whole mess?
            // 
            // Similarly, there's an 8Kx8 ROM (2764) chip on the board but the
            // ROM that's built seems to limit the Z80 assembler's view to 4K
            // and several other docs show that only 4K is used.  Since we're
            // loading from actual ROM dumps, the ROM loader wants to match the
            // RomSize to the length of the actual file.
            //
            _z80RamSize = 0x1800;   // 6K (of 16K) RAM
            _z80RamAddr = 0x6800;
            _z80RomSize = 0x1000;   // 4K (of 8K) ROM
            _z80RomAddr = 0x0;
        }

        public EIO(PERQSystem system) : base(system)
        {
            // Set up the Z80 with the new firmware
            _z80System = new EIOZ80(system);
            _z80System.LoadZ80ROM("eioz80.bin");

            RegisterPorts(_handledPorts);

            // What flavor of PERQ 2 are we?
            if ((system.Config.Chassis == ChassisType.PERQ2 ||
                 system.Config.Chassis == ChassisType.PERQ2Tx) &&
                 system.Config.GetDrivesOfType(DeviceType.Disk8Inch).Length > 0)
            {
                // A PERQ-2 or 2/T1
                _hardDiskController = new MicropolisDiskController(system);
            }
            else if (system.Config.Chassis == ChassisType.PERQ2Tx &&
                     system.Config.GetDrivesOfType(DeviceType.Disk5Inch).Length > 0)
            {
                // A PERQ-2/T2 or 2/T4
                // _hardDiskController = new MFMDiskController(system);
                throw new UnimplementedHardwareException("MFMDiskController not yet implemented");
            }
            else
            {
                throw new InvalidOperationException("EIO does not support this disk/chassis configuration");
            }

            // Set up the on-board Ethernet
            if (system.Config.IOBoard == IOBoardType.NIO ||
                string.IsNullOrEmpty(Settings.EtherDevice) ||
                Settings.EtherDevice == "null")
            {
                // A minimal interface to let Accent boot properly
                _ethernetController = new NullEthernet(system);
            }
            else
            {
                try
                {
                    _ethernetController = new Ether10MbitController(system);
                }
                catch (UnimplementedHardwareException e)
                {
                    // Failed to open - bad device, or no permissions?
                    Log.Warn(Category.All, "{0}; no Ethernet available.", e.Message);

                    // Fall back to the fake one and continue
                    _ethernetController = new NullEthernet(system);
                }
            }
            RegisterPorts(_etherPorts);
        }

        public INetworkController Ether => _ethernetController;

        /// <summary>
        /// Reads a word from the given I/O port.
        /// </summary>
        public override int IORead(byte port)
        {
            switch (port)
            {
                case 0x53:      // DskStat (SMStat): read disk status reg
                    return _hardDiskController.ReadStatus();

                case 0x54:       // 124 EioZ80In: dismiss Z80 interrupt
                    return _z80System.ReadData();

                case 0x55:       // 125 EioZ80Stat: read Z80 interface status
                    return _z80System.ReadStatus();

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
                // Z80 data port
                case 0xc4:
                    _z80System.WriteData(value);
                    break;

                // Z80 control register
                case 0xc5:
                    // Todo: if bit 3 set, inform the PDMA
                    _z80System.WriteStatus(value);
                    break;

                // Load disk registers
                case 0xd0:
                case 0xd1:
                case 0xd2:
                case 0xd3:
                    _hardDiskController.LoadRegister(port, value);
                    break;

                // Load DMA registers
                case 0xc0:
                case 0xd4:
                case 0xd5:
                case 0xd6:
                case 0xd7:
                    throw new InvalidOperationException($"EIO DMA not yet implemented (0x{port:x2})");

                // Load Ethernet registers
                case 0xc2:
                case 0xc3:
                case 0xc8:
                case 0xc9:
                case 0xca:
                case 0xcb:
                case 0xcd:
                case 0xce:
                case 0xcf:
                case 0x93:
                case 0xd8:
                case 0xd9:
                case 0xda:
                case 0xdc:
                case 0xdd:
                case 0xde:
                    if (_ethernetController != null)
                    {
                        _ethernetController.LoadRegister(port, value);
                    }
                    break;

                default:
                    Log.Warn(Category.IO, "Unhandled EIO Write to port {0:x2}, data {1:x4}", port, value);
                    break;
            }
        }

        /// <summary>
        /// Shutdown local devices that need extra attention.
        /// </summary>
        public override void Shutdown()
        {
            // Make sure our Ethernet (if configured) is properly shut down!
            if (_ethernetController != null && _ethernetController is Ether10MbitController)
            {
                _ethernetController.Shutdown();
            }

            // todo: Save the RTC offset? :-)

            base.Shutdown();
        }

        /// <summary>
        /// Ports handled by the EIO.  (But no Ethernet on NIO.)
        /// </summary>
        byte[] _handledPorts =
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
            0xd3        // 323 DskCtl: disk control register
        };

        byte[] _etherPorts =
        {
            // Ethernet ports
            0x52,       // 122 E10ERdNetSR: read net status register
            0x5a,       // 132 E10ERdBCLow: read bit count low byte
            0x5b,       // 133 E10ERdBCHgh: read bit count high byte
            0xc2,       // 302 E10EWrNetCR: load net control register
            0xc3,       // 303 E10EWrIntEnb: load net interrupt enable reg
            0xc8,       // 310 E10EWrNA0: load net addr low word byte 6
            0xc9,       // 311 E10EWrNA1: load net addr low word byte 5
            0xca,       // 312 E10EWrMCCmd: load multicast command byte
            0xcb,       // 313 E10EWrMC0: load multicast reg grp 1
            0xcc,       // 314 E10EWrMC1: load multicast reg grp 2
            0xcd,       // 315 E10EWrMC2: load multicast reg grp 3
            0xce,       // 316 E10EWrMC3: load multicast reg grp 4
            0xcf,       // 317 E10EWrMC4: load multicast reg grp 5
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

        INetworkController _ethernetController;

    }
}

/*
    Notes:

    This write register needs some special attention:
    
           305  Control register  bit 3  - Disable Ext A address
                                           (see PERQ DMA)
                                  bit 2  - Z80 reset when clear
                                  bit 1  - Enable write channel;
                                           interrupt when set
                                  bit 0  - Enable read channel;
                                           interrupt when set

    ACK!  This is very confusing and finicky:

    PERQ TO the Z80 is the "Write FIFO", or Z80 IO BUS INPUT (schematics pg 10):
    
        writes data to port 304 (LD_UPROC_DATA_L) trigger PERQ_INT on the Z80
        and set UPROC_RDY if the FIFO OR signal is true (i.e., not empty)

        control register (port 305) bit 1 is the UPROC_RDY_ENB flag; this enables
        the UPROC_RDY_INT_L interrupt (Z80DataOut) to be sent TO the PERQ when
        there is NO data in the FIFO AND the UPROC_RDY_ENB is set (meaning, "I'm
        ready for more data")

        bit 0 is the UPROC_ENB flag, which enables read-side (Z80DataIn) interrupts
        (see below)

        data is read from the FIFO on port 124 (0x54) RD_UPROC_DATA_L

        the PERQ UPROC_INT_L is asserted when IOD_OUT_RDY is asserted by the Z80,
        the FIFO OR is asserted (i.e., not empty) and the UPROC_ENB bit is set.

        reads from the status register (125, 0x55, RD_UPROC_STAT_L) return the
        IOD_OUT_RDY (output ready) bit as set by the Z80 directly) and UPROC_RDY
        (meaning the Z80->PERQ FIFO is empty)
        

    Z80 to the PERQ is the "Read FIFO", mislabeled Z80 IO BUS INPUT (schematics
    pg 9) when it is clearly output from BUF_D<> -> IOB<>

        IOD_OUT_RDY is the one-bit register written to by the Z80 when it has
        data to send TO the PERQ; it is latched in the '259 by writes to the
        control register at port 170Q (0x78)

        data bytes are queued by writes to port 161Q (0x71), SEL_IOD_WR_L
*/
