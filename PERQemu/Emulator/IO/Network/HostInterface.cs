﻿//
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
using SharpPcap.LibPcap;
using PacketDotNet;
using PacketDotNet.Utils;

namespace PERQemu.IO.Network
{
    /// <summary>
    /// Encapsulate a host Ethernet interface for sending and receiving PERQ
    /// packets on a real network.
    /// </summary>
    public class HostInterface
    {
        public HostInterface(INetworkController controller, string devName)
        {
            _nat = new NATTable();
            _controller = controller;
            _adapter = GetAdapter(devName);

            if (_adapter == null)
            {
                Log.Warn(Category.Ethernet, "Adapter is not present; no Ethernet available.");
            }
            else
            {
                // Open the device and register our receive callback
                // Todo: catch in case the open fails?
                _adapter.Open(DeviceMode.Promiscuous, 0);
                _adapter.OnPacketArrival += OnPacketArrival;
            }
        }

        public string Name => _adapter.Name;
        public string Description => _adapter.Description;
        public PhysicalAddress Address => (_adapter == null ? PhysicalAddress.None : _adapter.MacAddress);
        public bool Running => (_adapter != null && _adapter.Started);

        /// <summary>
        /// There's nothing to reset, really; we just use this to lazily start
        /// packet capture once the rest of the virtual PERQ is set up.
        /// </summary>
        public void Reset()
        {
            if (!Running)
            {
                _adapter.StartCapture();

                Log.Write(Category.Ethernet, "Adapter reset (packet capture started)");
            }

            // Say hello to my little friend
            SendGreeting();
        }

        /// <summary>
        /// Send a RARP request with our emulated address to let other PERQemu
        /// (or real PERQ) nodes know we're here.  We don't expect a reply, but
        /// issue these periodically just to check in.  Someday this might be
        /// formalized into a way to allow PERQs to rendezvous over the Interwebs!
        /// </summary>
        public void SendGreeting()
        {
            try
            {
                var greeting = new ARPPacket(ARPOperation.InARPRequest,
                                             Broadcast,
                                             new System.Net.IPAddress(new byte[] { 0, 0, 0, 0 }),
                                             _controller.MACAddress,
                                             new System.Net.IPAddress(new byte[] { 0, 0, 0, 0 }));

                Console.WriteLine(greeting.PrintHex());
                _adapter.SendPacket(greeting);

                Log.Info(Category.Ethernet, "Sent RARP request from {0}", _controller.MACAddress);
            }
            catch (PcapException ex)
            {
                Log.Write(Category.Ethernet, "Failed to send greeting packet: {0}", ex.Message);
            }
        }

        /// <summary>
        /// Send a raw Ethernet packet straight from the PERQ, baybee!!
        /// </summary>
        public bool SendPacket(byte[] packet)
        {
            try
            {
                var raw = new EthernetPacket(new ByteArraySegment(packet));
                if (raw == null)
                    Console.WriteLine($"RAW NULL ON SEND");
                else
                    Console.WriteLine(raw.PrintHex());

                // Are we sending to another PERQ?
                var dest = _nat.LookupPerq(raw.DestinationHwAddress);
                if (dest != null)
                {
                    // Yes!  Swap the destination with the host's addr
                    raw.DestinationHwAddress = dest.Host;

                    // Basic stats
                    dest.Sent++;

                    Log.Write(Category.Ethernet, "NAT send to Perq {0} => Host {1}", dest.Perq, dest.Host);
                }

                _adapter.SendPacket(raw);
                return true;
            }
            catch (PcapException ex)
            {
                Log.Write(Category.Ethernet, "Failed to send packet: {0}", ex.Message);
                return false;
            }
        }

        /// <summary>
        /// Callback for incoming packets.
        /// </summary>
        void OnPacketArrival(object s, CaptureEventArgs e)
        {
            // See what SharpPcap dumps out
            Console.WriteLine(e.Packet);

            // Start with the raw Ethernet frame
            if (e.Packet.LinkLayerType == LinkLayers.Ethernet)
            {
                EthernetPacket raw = null;

                try
                {
                    raw = (EthernetPacket)Packet.ParsePacket(e.Packet.LinkLayerType, e.Packet.Data);
                    Console.WriteLine(raw.PrintHex());

                    // If this is addressed to us specifically, NAT it!
                    if (raw.DestinationHwAddress == _adapter.MacAddress)
                    {
                        raw.DestinationHwAddress = _controller.MACAddress;

                        // Is it from a PERQ that we've seen before?
                        var src = _nat.LookupHost(raw.SourceHwAddress);

                        if (src != null)
                        {
                            // Yes!  Update the stats to show they're still active
                            src.LastReceived = DateTime.Now;
                            src.Received++;
                        }
                    }
                }
                catch (PcapException ex)
                {
                    Log.Warn(Category.Ethernet, "Failed to parse packet: {0}", ex.Message);
                    raw = null;
                }

                //
                // Look for RARPs, which are pretty rare these days and will
                // almost certainly be PERQemu (or maybe QEMU :-) emulated hosts
                // broadcasting a greeting
                //
                ARPPacket rarp = null;

                try
                {
                    rarp = (ARPPacket)raw.Extract(typeof(ARPPacket));

                    if (rarp != null)
                    {
                        Console.WriteLine(rarp.PrintHex());

                        var seen = _nat.LookupPerq(rarp.SenderHardwareAddress);
                        if (seen == null && IsPerqPrefix(rarp.SenderHardwareAddress.GetAddressBytes()))
                        {
                            // Woo!  Another Perqy came out to play!
                            seen = new NATEntry(raw.SourceHwAddress, rarp.SenderHardwareAddress);
                            _nat.Add(seen);
                        }
                    }
                }
                catch (PcapException ex)
                {
                    Log.Info(Category.Ethernet, "Not a RARP packet?  {0}", ex.Message);
                    // No biggie, just continue
                }

                // Ask the controller if it wants the packet and has room for it
                if (_controller.WantReceive(raw.DestinationHwAddress))
                {
                    // Yep!  Queue it up and return
                    _controller.DoReceive(raw.Bytes);
                }
                // else: discard it
            }
            // else: log that it was something we couldn't read?
        }

        /// <summary>
        /// Stop packet capture, detach the callback and shutdown this instance.
        /// </summary>
        public void Shutdown()
        {
            try
            {
                _adapter.StopCapture();
            }
            catch (Exception e)
            {
                // Log, but throw away exceptions since we're shutting down...
                Log.Info(Category.Ethernet, "Exception on shutdown (ignored): {0}", e.Message);
            }
            finally
            {
                _adapter.OnPacketArrival -= OnPacketArrival;
                _adapter.Close();

                Log.Write(Category.Ethernet, "Adapter shutdown");
            }
        }

        /// <summary>
        /// Return true if a given address is in the official 3RCC address block.
        /// </summary>
        public static bool IsPerqPrefix(byte[] a)
        {
            return (a[0] == 0x02 &&
                    a[1] == 0x1c &&
                    a[2] == 0x7c &&
                    a[3] >= 0 && a[3] <= 2);
        }

        /// <summary>
        /// Weed out the non-Ethernet interfaces.  On Mac/Mono everything shows
        /// up as plain Ethernet (and most of these will never appear) but let's
        /// be complete.  I find it vaguely hilarious that MS includes 3Mbit as
        /// an enumeration.  That port of Windows to Alto, PDP-11 or PERQ coming
        /// along any day now?
        /// 
        /// This will probably go away since I'll probably have to use the #Pcap
        /// names to make it simpler to store/match names.  Ugh.
        /// </summary>
        public static bool IsEthernet(NetworkInterfaceType t)
        {
            return (t == NetworkInterfaceType.Ethernet ||
                    t == NetworkInterfaceType.Ethernet3Megabit ||
                    t == NetworkInterfaceType.FastEthernetFx ||
                    t == NetworkInterfaceType.FastEthernetT ||
                    t == NetworkInterfaceType.GigabitEthernet);
        }

        /// <summary>
        /// Find the adapter that matches the interface name.  OF COURSE the C#
        /// runtime has a completely different way of doing this from SharpPcap,
        /// so I have no idea how the hell we're supposed to store this value in
        /// a sane way.  Windows gives back a stupid GUID-long-ass-path-thing
        /// rather than just "en0" or "eth1" or even a Windowsy "NET0:".  UGH.
        /// </summary>
        public static ICaptureDevice GetAdapter(string name)
        {
            var devices = CaptureDeviceList.Instance;

            // Run through the list and try to match exactly...
            if (devices.Count > 0)
            {
                foreach (var dev in devices)
                {
                    if (dev.Name.ToLowerInvariant() == name.ToLowerInvariant())
                        return dev;
                }
            }

            Log.Write("Could not find a match for Ethernet adapter '{0}'", name);
            return null;
        }

        // Debugging
        public static void ShowInterfaceSummary()
        {
            // Show the C# runtime's view
            var interfaces = NetworkInterface.GetAllNetworkInterfaces();

            foreach (NetworkInterface adapter in interfaces)
            {
                if (!IsEthernet(adapter.NetworkInterfaceType)) continue;

                Console.WriteLine("ID: {0}  Name: {1}", adapter.Id, adapter.Name);
                Console.WriteLine(adapter.Description);
                Console.WriteLine(string.Empty.PadLeft(adapter.Description.Length, '='));
                Console.WriteLine("  Interface type ......... : {0}", adapter.NetworkInterfaceType);
                Console.WriteLine("  Operational status ..... : {0}", adapter.OperationalStatus);
                Console.WriteLine("  Hardware address ....... : {0}", adapter.GetPhysicalAddress());
                Console.WriteLine();
            }
            Console.WriteLine();

            // Let's see what Pcap gives us...
            var ver = SharpPcap.Version.VersionString;
            Console.WriteLine("SharpPcap {0} devices:", ver);

            // Retrieve the device list
            var devices = CaptureDeviceList.Instance;

            // If no devices were found print an error
            if (devices.Count < 1)
            {
                Console.WriteLine("No Ethernet adapters were found on this machine");
                return;
            }

            int i = 0;

            // Print out the devices
            foreach (var dev in devices)
            {
                Console.WriteLine("{0}) {1} - {2}", i, dev.Name, dev.Description);
                i++;
            }
        }

        public void DumpStatus()
        {
            Console.WriteLine($"\nHost adapter status:");
            Console.WriteLine($"  NIC {Name} - {Description}");
            Console.WriteLine($"  Address: {Address}\tRunning: {Running}");

            _nat.DumpTable();
        }

        ICaptureDevice _adapter;
        INetworkController _controller;

        NATTable _nat;

        // All 1's broadcast
        public static PhysicalAddress Broadcast = new PhysicalAddress(new byte[] { 255, 255, 255, 255, 255, 255 });
    }
}
