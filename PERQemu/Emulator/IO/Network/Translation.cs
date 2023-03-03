//
// Translation.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Network
{
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
            _perqToHost = new Dictionary<PhysicalAddress, PhysicalAddress>();
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
            _perqToHost.Clear();
            Log.Info(Category.Ethernet, "NAT table flushed");
        }

        /// <summary>
        /// Add a new mapping.  We leave initialization up to the caller.
        /// </summary>
        public void Add(NATEntry ent)
        {
            // Make sure it isn't already there...
            if (_entries.ContainsKey(ent.Host))
            {
                Log.Write(Category.Ethernet, "Can't add duplicate NAT entry, ignored {0}", ent);
                return;
            }
            _entries.Add(ent.Host, ent);

            // Do the inverse index too
            if (_perqToHost.ContainsKey(ent.Perq))
            {
                Log.Write(Category.Ethernet, "PERQ {0} already in index at different host?", ent.Perq);
                return;
            }
            _perqToHost.Add(ent.Perq, ent.Host);

            Log.Write(Category.Ethernet, "NAT entry added {0}", ent);
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
            if (_perqToHost.ContainsKey(perq))
            {
                return LookupHost(_perqToHost[perq]);
            }

            return null;
        }

        // Debugging
        public void DumpTable()
        {
            if (_entries.Count == 0)
            {
                Console.WriteLine("  NAT table is empty.");
                return;
            }

            // Todo: messy, gotta format this up all sweetly
            Console.WriteLine("\nNAT table:");
            Console.WriteLine("\nHost\t\tPerq\t\tFirst seen\tLast seen\t\tSent / Rcvd");

            foreach (var e in _entries.Values)
            {
                //var seconds = e.LastReceived - e.FirstSeen;
                Console.WriteLine($"{e.Host}  {e.Perq}  {e.FirstSeen}  {e.LastReceived}  {e.Sent}  {e.Received}");
            }
        }

        Dictionary<PhysicalAddress, NATEntry> _entries;
        Dictionary<PhysicalAddress, PhysicalAddress> _perqToHost;
    }
}
