//
// MachineAddress.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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

using System.Net.NetworkInformation;

using PERQemu.Config;

namespace PERQemu.IO.Network
{
    /// <summary>
    /// The PERQ's 48-bit Ethernet address.
    /// </summary>
    /// <remarks>
    /// The hardware's fixed address is burned into the NET PROMs on the board.
    /// For reasons lost to history, you don't just read the bytes directly; the
    /// six octets are written via DMA into the header buffer in response to a
    /// "special receive" command.  The first four bytes are fixed (three octets
    /// 02:1c:7c assigned by Xerox, plus the board ID set by 3RCC) while the last
    /// two are machine-specific, burned into the PROMs during manufacturing.
    /// These aren't just returned as two bytes, though; they are given as four
    /// nibbles, bit reversed, and must be flipped and reconstructed in software.
    /// </remarks>
    public struct MachineAddress
    {
        public MachineAddress(Configuration conf)
        {
            _mac = new byte[6];

            // First three octets allocated by Xerox 
            _mac[0] = 0x02;
            _mac[1] = 0x1c;
            _mac[2] = 0x7c;

            // Fourth octet coded by 3RCC:  0 = OIO, 1 = EIO, 2 = 24-bit EIO!
            _mac[3] = (byte)(conf.CPU == CPUType.PERQ24 ? 2 :
                             conf.IOBoard == IOBoardType.EIO ? 1 : 0);

            // Read from the NET PROMs (or configured by the user)
            _mac[4] = 0;
            _mac[5] = 0;

            // Store it in this format too
            _physAddr = new PhysicalAddress(_mac);
        }

        // For convenience
        public PhysicalAddress PA => _physAddr;

        // These are fixed
        public ushort High => (ushort)((_mac[0] << 8) | _mac[1]);
        public ushort Mid => (ushort)((_mac[2] << 8) | _mac[3]);

        // This one can be tweaked
        public ushort Low
        {
            get
            {
                return (ushort)((_mac[4] << 8) | _mac[5]);
            }

            set
            {
                _mac[4] = (byte)(value >> 8);
                _mac[5] = (byte)(value & 0xff);

                // Update for compares
                _physAddr = new PhysicalAddress(_mac);
            }
        }

        // How the hardware returns the low word
        public byte Hn => Mirror(_mac[4], 4);
        public byte MHn => Mirror(_mac[4], 0);
        public byte MLn => Mirror(_mac[5], 4);
        public byte Ln => Mirror(_mac[5], 0);

        // Return a bit-swapped nibble (OIO only)
        byte Mirror(byte nibble, int offset)
        {
            var mirrored = 0;
            nibble >>= offset;

            // On OIO bits are stored in network order, so we have to flip them
            for (var i = 0; i < 4; i++)
            {
                mirrored = (mirrored << 1) | (nibble & 0x1);
                nibble >>= 1;
            }

            return (byte)(mirrored & 0xf);
        }

        public override string ToString()
        {
            return $"[MAC: {_mac[0]:x2}:{_mac[1]:x2}:{_mac[2]:x2}:{_mac[3]:x2}:{_mac[4]:x2}:{_mac[5]:x2}]";
        }

        public string ToPERQFormat()
        {
            return $"[{High},{Mid},{Low}]";
        }

        byte[] _mac;
        PhysicalAddress _physAddr;
    }
}
