// PERQ24A.cs - Copyright 2021 Skeezics Boondoggle (skeezicsb@gmail.com)
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

namespace PERQemu.Processor
{
    /// <summary>
    /// This class implements the fire-breathing 24-bit, 64K CPU used in
    /// the "PERQ 2.0" lineup of workstations and servers from the famed
    /// Retrocomputing Division of Boondoggle Heavy Industries, Ltd.
    /// </summary>
    public class PERQ24A : CPU
    {
        static PERQ24A()
        {
            _name = "PERQ24A";
            _desc = "PERQ24A 64K CPU (24-bit)"
          //_cycleTime = 160;   // 6.25Mhz development board
            _cycleTime = 125;   // 8MHz production board

            _bits = 24;
            _mask = 0xffffff;

            _wcsBits = 16;
            _wcsSize = 65536;
            _wcsMask = 0xffff;
        }

        public PERQ24A(PERQSystem system) : base(system)
        {
        }

        /// <summary>
        /// Reads the microstate registers.
        /// </summary>
        /// <remarks>
        /// Like the original 24-bit PERQ, PERQ24A uses the H (Hold) bit of
        /// the microinstruction to select between the usual Microstate reg
        /// and "Upper", which gives the top 8 bits of the BMux input (so the
        /// upper byte of a 24-bit register can be read).  When H=0, the usual
        /// uState value is returned, but the format is redefined for PERQ24A:
        /// 
        ///  15      12               8    7        0
        /// +-------+-------------------+------------+
        /// | flags | uu | CO | CF | SE | OE |  BPC  |
        /// +-------+-------------------+------------+
        /// 
        /// where:
        ///     uu - unused         CO - CacheOn    CF - Call stack Full
        ///     SE - Estack empty   OE - OpEmpty    BPC - 7 bit op address
        /// 
        /// See the "PERQ 2.0 Microprogramming Guide" for more info. :-)
        /// </remarks>
        public override int ReadMicrostateRegister(byte h)
        {
            if (h == 1)
            {
                return  ((~_lastBmux) >> 16) & 0xff;
            }
            else
            {
                return  BPC |
                        (OpFileEmpty ? 0x0080 : 0x0) |
                        (_estack.StackPointer == 0  ? 0x0100 : 0x0) |
                        (_usequencer.StackFull == 0 ? 0x0200 : 0x0) | // active low
                        (_cpuOption.CacheOn ? 0x0400 : 0x0) |
                        (_alu.Flags.Lss ? 0x1000 : 0x0) |
                        (_alu.Flags.Eql ? 0x2000 : 0x0) |
                        (_alu.Flags.Cry ? 0x4000 : 0x0) |
                        (_alu.Flags.Ovf ? 0x8000 : 0x0);
            }
        }

        ///<remarks>
        /// This (mostly!) fictional device is a fever dream, KiCAD schematic,
        /// and collection of documents describing a brand new retrocomputer
        /// based on the original PERQ, using 1980s technologies, but designed
        /// to incorporate some of the features of the "TKUP" machine and
        /// address some of the weaknesses of the original PERQ.  It features
        /// a full 16-bit microsequencer that's 100% code compatible with the
        /// Am2910 (eliminating the two-bit kluge), an 8-bit BPC and provision
        /// for an off-board cache, an extended RBase (10 bits) to expand the
        /// XY register file to 1,024 (!!) general-purpose registers (16 banks
        /// of 64, switched into the low bank), a writable Upper register and
        /// a completely revamped set of Special Function decodes, new boot ROMs
        /// with graphical console and network booting support, and numerous
        /// other little improvements/corrections to the various documented
        /// "quirks and oddities" of the beloved PERQ.
        /// 
        /// Paired with this new CPU is a dramatically updated Z80/EIO board
        /// with a redesigned DMA architecture and unified hard disk/Ethernet
        /// cache, serial console support, audio input and output, support for
        /// dual floppy drives and up to four MFM, ESDI or SMD hard disks.
        /// 
        /// Naturally this also requires a radical new memory board and video
        /// architecture.  To fully utilize the 24-bit address space, a new
        /// 7-slot backplane accepts _two_ 8MW boards, each with an independent
        /// frame buffer, making "dual-headed" configurations possible.
        /// 
        /// While we will collectively experience the heat death of the universe
        /// long before this "PERQ 2.0" architecture actually comes into 
        /// existence, the dream behind it helped motivate the refactoring of
        /// PERQemu, so that the software would have the flexibility to allow
        /// for these crazy new designs and at least provide a simulation of
        /// what could be/could have been, and a way to develop and test out
        /// new microcode and firmware for the theoretical new platform. :-)
        /// </remarks>
    }
}