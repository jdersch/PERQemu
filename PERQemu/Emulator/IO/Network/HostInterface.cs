//
// HostInterface.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using SharpPcap;
using PacketDotNet;

namespace PERQemu.IO.Network
{
    /// <summary>
    /// Encapsulate a host Ethernet interface for sending and receiving PERQ
    /// packets on a real network.
    /// </summary>
    public class HostInterface
    {
        public HostInterface()
        {
        }

        // Weed out the non-Ethernet interfaces.  On Mac/Mono everything shows
        // up as plain Ethernet (and most of these will never appear) but let's
        // be complete.  I find it vaguely hilarious that MS includes 3Mbit as
        // an enumeration.  That port of Windows to Alto, PDP-11 or PERQ coming
        // along any day now?
        public static bool IsEthernet(NetworkInterfaceType t)
        {
            return (t == NetworkInterfaceType.Ethernet ||
                    t == NetworkInterfaceType.Ethernet3Megabit ||
                    t == NetworkInterfaceType.FastEthernetFx ||
                    t == NetworkInterfaceType.FastEthernetT ||
                    t == NetworkInterfaceType.GigabitEthernet);
        }


        // Debugging
        public static void ShowInterfaceSummary()
        {
            var interfaces = NetworkInterface.GetAllNetworkInterfaces();

            foreach (NetworkInterface adapter in interfaces)
            {
                if (!IsEthernet(adapter.NetworkInterfaceType)) continue;

                Console.WriteLine("Name: {0}", adapter.Name);
                Console.WriteLine(adapter.Description);
                Console.WriteLine(string.Empty.PadLeft(adapter.Description.Length, '='));
                Console.WriteLine("  Interface type ......... : {0}", adapter.NetworkInterfaceType);
                Console.WriteLine("  Operational status ..... : {0}", adapter.OperationalStatus);
                Console.WriteLine("  Hardware address ....... : {0}", adapter.GetPhysicalAddress());

                // Create a display string for the supported IP versions
                //string versions = "";

                //if (adapter.Supports(NetworkInterfaceComponent.IPv4))
                //{
                //    versions = "IPv4";
                //}
                //if (adapter.Supports(NetworkInterfaceComponent.IPv6))
                //{
                //    if (versions.Length > 0)
                //    {
                //        versions += " ";
                //    }
                //    versions += "IPv6";
                //}
                //Console.WriteLine("  IP version ............. : {0}", versions);
                Console.WriteLine();
            }
            Console.WriteLine();
        }
    }
}

/*
    Notes:

    Constructor takes the name (string) to locate, initialize the interface?

    How can we do NAT for the PERQs to see each other on a busy network?
    Proxy ARP?  On Mac/Linux can probably rename or alias the MAC addr with
    relative ease (mac: "sudo ifconfig en1 ether 02:1c:7c:00:ba:be") but that
    gets a little annoying if you want to switch from OIO to EIO or change
    the last word...

       PERQemu  <-->    HW MAC        <==>     HW MAC   <-->   PERQemu
    2:1c:7c:0:a:b   0:1D:4F:46:F8:2D        0:8:0:20:f:c    2:1c:7c:1:d:e
    
    We have to learn the address of the other hosts!  Build our own ARP/NAT
    table?!  Hmm.  That's ugly.  Or impossible without a custom protocol!?
    No!  Regular ARP would work?

    Sender:     Sends packet addressed to 2:1c:7c:1:d:e
                NAT translates local 2:1c:7c:0:a:b outbound -> 0:1d:4f:46:f8:2d

    Receiver:   Host 0:8:0:20:f:c (in promiscuous mode) sees dest 2:1c:7c:1:d:e
                That's me!  Replies (translated) 1:d:e is at 0:8:0:20etc

    Sender:     Host 0:1d:4f:46:f8:2d (promisc) sees reply and translates response
                Sees reply from 1:d:e (8:0:20 is rewritten)

    Do we need to maintain a table of mappings?  Just for informational purposes?

 */