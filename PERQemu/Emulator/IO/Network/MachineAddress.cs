//
// MachineAddress.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using System;
using System.Collections.Generic;
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
        }

        // Just the bytes, ma'am
        public byte[] MAC => _mac;

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
    }


    /// <summary>
    /// Store an Ethernet address translation so that virtual PERQs can be easily
    /// mapped to their hosts.  Largely for stats/debugging?
    /// </summary>
    public class NATEntry
    {
        public NATEntry(PhysicalAddress host, PhysicalAddress perq)
        {
            Host = host;
            Perq = perq;
            FirstSeen = DateTime.Now;
            LastReceived = DateTime.Now;
            Sent = Received = 0;
        }

        public override string ToString()
        {
            return $"[Host: {Host}  Perq: {Perq}]";
        }

        public PhysicalAddress Host;        // Host MAC
        public PhysicalAddress Perq;        // PERQ's MAC
        public DateTime FirstSeen;          // Date/time mapping established
        public DateTime LastReceived;       // Date/time last packet/update received
        public ulong Received;              // Count Host->Perq mappings
        public ulong Sent;                  // Count Perq->Host mappings
    }

    /// <summary>
    /// A (very) simple Network Address Translation table, mapping a given host
    /// MAC address to an emulated PERQ address.
    /// </summary>
    public class NATTable
    {
        public NATTable()
        {
            _entries = new Dictionary<PhysicalAddress, NATEntry>();
        }

        public void Reset()
        {
        }

        /// <summary>
        /// Flush the entire table.
        /// </summary>
        public void Flush()
        {
            _entries.Clear();
            Log.Info(Category.Ethernet, "NAT table flushed");
        }

        /// <summary>
        /// Add a new mapping.  We leave initialization up to the caller.
        /// </summary>
        public void Add(NATEntry ent)
        {
            // Make sure it isn't already there...
            if (_entries.ContainsValue(ent))
            {
                Log.Debug(Category.Ethernet, "Can't add duplicate NAT entry, ignored {0}", ent);
            }

            _entries.Add(ent.Host, ent);
            Log.Debug(Category.Ethernet, "NAT entry added {0}", ent);
        }

        /// <summary>
        /// Search the table to see if we've seen PERQ traffic from a given source
        /// MAC address before.  If so, return the NAT entry; otherwise, null.
        /// </summary>
        public NATEntry LookupHost(PhysicalAddress host)
        {
            if (_entries.ContainsKey(host))
            {
                return _entries[host];
            }

            return null;
        }

        /// <summary>
        /// Look up another PERQ (real or virtual) in the inverse map and return
        /// the host entry for it.
        /// </summary>
        public NATEntry LookupPerq(PhysicalAddress perq)
        {
            return LookupHost(_perqToHost[perq]);
        }

        /// <summary>
        /// Bump the access/packet count and receive time for the given host.
        /// Quietly no-op if not in table?
        /// </summary>
        public void Update(PhysicalAddress host)
        {
            if (_entries.ContainsKey(host))
            {
                _entries[host].LastReceived = DateTime.Now;
                _entries[host].Received++;
            }
        }

        Dictionary<PhysicalAddress, NATEntry> _entries;
        Dictionary<PhysicalAddress, PhysicalAddress> _perqToHost;
    }
}
