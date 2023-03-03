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
using System.Collections.Concurrent;
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
            _pending = new ConcurrentQueue<EthernetPacket>();

            _controller = controller;
            _adapter = GetAdapter(devName);

            if (_adapter == null)
            {
                Log.Warn(Category.NetAdapter, "Adapter is not present; no Ethernet available.");
            }
            else
            {
                // Open the device and register our receive callback
                // Todo: catch in case the open fails?
                _adapter.Open(DeviceMode.Promiscuous);
                _adapter.OnPacketArrival += OnPacketArrival;

                // Does this avoid the spurious exception on shutdown?
                _adapter.StopCaptureTimeout = new TimeSpan(100000000);
            }
        }

        public string Name => _adapter.Name;
        public string Description => _adapter.Description;
        public PhysicalAddress Address => (_adapter == null ? PhysicalAddress.None : _adapter.MacAddress);
        public bool Running => (_adapter != null && _adapter.Started);

        /// <summary>
        /// There's nothing to reset, really; we just use this to lazily start
        /// packet capture once the rest of the virtual PERQ is set up.  Any
        /// subsequent calls will send out a "greeting", periodically.
        /// </summary>
        public void Reset()
        {
            if (!Running)
            {
                _adapter.StartCapture();

                Log.Write(Category.NetAdapter, "Adapter reset (packet capture started)");
            }
            else
            {
                // Flush the backlog?
                while (!_pending.IsEmpty)
                {
                    EthernetPacket tossIt;
                    _pending.TryDequeue(out tossIt);
                }

                // Announce our presence with authori-tie
                SendGreeting();
            }
        }

        /// <summary>
        /// Send a RARP request with our emulated address to let other PERQemu
        /// (or real PERQ) nodes know we're here.  We don't expect a reply, but
        /// issue these periodically just to check in.  Someday this might be
        /// formalized into a way to allow PERQs to rendezvous over the Interwebs!
        /// </summary>
        public void SendGreeting()
        {
            // Don't spam the network with broadcasts!
            TimeSpan ts = DateTime.Now - _lastGreeting;
            if (ts.Seconds < GreetingInterval) return;

            try
            {
                // Broadcast our request
                var packet = new EthernetPacket(_adapter.MacAddress,
                                                Broadcast,
                                                EthernetPacketType.ReverseArp);
                // Well hello there!
                var greeting = new ARPPacket(ARPOperation.RequestReverse,
                                             _controller.MACAddress,
                                             System.Net.IPAddress.None,
                                             _adapter.MacAddress,
                                             System.Net.IPAddress.None);

                packet.PayloadPacket = greeting;
                _adapter.SendPacket(packet);
                _lastGreeting = DateTime.Now;

                Log.Info(Category.NetAdapter, "Sent RARP request from {0}", _controller.MACAddress);
            }
            catch (PcapException ex)
            {
                Log.Write(Category.NetAdapter, "Failed to send greeting packet: {0}", ex.Message);
            }
        }

        /// <summary>
        /// Send a RARP reply when we catch one from another PERQ.  We're kinda
        /// breaking the paradigm a little bit; PERQemu uses the "request" to
        /// tell other PERQs out there about us, while the "reply" here is to
        /// say "gotcha, here's my info in return".  That way when a new instance
        /// comes online it quickly gathers up data for the others without doing
        /// periodic broadcasts.  This is all kinda cheesy. :-)
        /// </summary>
        void SendReply(ARPPacket greeting)
        {
            try
            {
                // Return to sender
                var packet = new EthernetPacket(_adapter.MacAddress,
                                                greeting.SenderHardwareAddress,
                                                EthernetPacketType.ReverseArp);
                // Send back our data
                var salutation = new ARPPacket(ARPOperation.ReplyReverse,
                                              _controller.MACAddress,
                                               System.Net.IPAddress.None,
                                              _adapter.MacAddress,
                                               System.Net.IPAddress.None);

                packet.PayloadPacket = salutation;
                _adapter.SendPacket(packet);

                Log.Info(Category.NetAdapter, "Sent RARP reply to {0}", greeting.TargetHardwareAddress);
            }
            catch (PcapException ex)
            {
                Log.Write(Category.NetAdapter, "Failed to send greeting reply: {0}", ex.Message);
            }
        }

        /// <summary>
        /// Send a raw Ethernet packet straight from the PERQ, baybee!!
        /// </summary>
        public bool SendPacket(byte[] raw)
        {
            try
            {
                // Turn raw bytes from the PERQ provided into a packet
                var packet = new EthernetPacket(new ByteArraySegment(raw));
                if (packet == null)
                {
                    Log.Error(Category.NetAdapter, "Could not format packet on send");
                    // dump more data (if this ever happens); drop it like it's hot
                    return false;
                }

                Log.Write(Category.NetAdapter, "Sending from {0} to {1} (type {2})",
                          packet.SourceHwAddress, packet.DestinationHwAddress, packet.Type);
                Log.Write(Category.NetAdapter, "SIZES: packet {0}, header {1}, payload {2}",
                          packet.Bytes.Length, packet.Header.Length, packet.PayloadData?.Length);

                // Always remap our source address to the host adapter
                packet.SourceHwAddress = _adapter.MacAddress;

                // Are we (potentially) sending to another PERQ?
                if (IsPerqPrefix(packet.DestinationHwAddress) || packet.DestinationHwAddress.Equals(Broadcast))
                {
                    if (!packet.DestinationHwAddress.Equals(Broadcast))
                    {
                        // Look up the PERQ's host address
                        var map = _nat.LookupPerq(packet.DestinationHwAddress);

                        if (map != null)
                        {
                            // Translate it too
                            packet.DestinationHwAddress = map.Host;

                            // Basic stats
                            map.Sent++;

                            Log.Write(Category.NetAdapter, "NAT send to Perq {0} via Host {1}", map.Perq, map.Host);
                        }
                        else
                        {
                            Log.Warn(Category.NetAdapter, "Destination Perq {0} is unknown to me...", packet.DestinationHwAddress);
                            // Should do an actual RARP here...?
                        }
                    }

                    // Retranslate the EtherType/Length field if necessary
                    var perqType = PortMap((ushort)packet.Type);

                    if ((ushort)packet.Type != perqType)
                    {
                        Log.Write(Category.NetAdapter, "EtherType mapped from 0x{0:x4} to 0x{1:x4}", (ushort)packet.Type, perqType);
                        packet.Type = (EthernetPacketType)perqType;
                    }
                }

                // Print the (modified) packet
                Console.WriteLine(packet.PrintHex());

                // So send it already, sheesh
                _adapter.SendPacket(packet);
                return true;
            }
            catch (PcapException ex)
            {
                Log.Write(Category.NetAdapter, "Failed to send packet: {0}", ex.Message);
                return false;
            }
        }

        /// <summary>
        /// Callback for incoming packets:  make sure a valid Ethernet frame is
        /// received, perform NAT or handle RARP processing if appropriate, then
        /// ask the PERQ controller if it wants to handle the packet.  If yes,
        /// queue it up; if no, or if the queue is full, drop the packet.  The
        /// PERQ only expects a 10Mbit/half-duplex level of traffic, so there's
        /// no realistic expectation of "wire speed" levels of throughput here.
        /// </summary>
        void OnPacketArrival(object s, CaptureEventArgs e)
        {
            if (e.Packet.LinkLayerType != LinkLayers.Ethernet)
            {
                Log.Write(Category.NetAdapter, "Non-Ethernet packet type {0} ignored", e.Packet.LinkLayerType);
                return;
            }

            //
            // Start with the raw Ethernet frame
            //
            EthernetPacket raw;

            try
            {
                raw = (EthernetPacket)Packet.ParsePacket(e.Packet.LinkLayerType, e.Packet.Data);
                if (raw == null)
                {
                    Log.Warn(Category.NetAdapter, "Failed to parse packet: {0}", e.Packet);
                    return;
                }

                // The PERQ interface can't "see" its own transmissions, but
                // apparently SharpPcap does; silently drop 'em here
                if (raw.SourceHwAddress.Equals(_adapter.MacAddress)) return;

                Log.Write(Category.NetAdapter, "Received from {0} to {1} (type {2})",
                          raw.SourceHwAddress, raw.DestinationHwAddress, raw.Type);
                Log.Write(Category.NetAdapter, "SIZES: packet {0}, header {1}, payload {2}",
                          raw.Bytes.Length, raw.Header.Length, raw.PayloadData?.Length);                

                // If this is addressed to us specifically, NAT it!
                if (raw.DestinationHwAddress.Equals(_adapter.MacAddress))
                {
                    raw.DestinationHwAddress = _controller.MACAddress;
                }

                // Is it from a PERQ that we've seen before?
                var src = _nat.LookupHost(raw.SourceHwAddress);

                if (src != null)
                {
                    // Yes!  Translate the source address too
                    raw.SourceHwAddress = src.Perq;

                    // Update the stats to show they're still active
                    src.LastReceived = DateTime.Now;
                    src.Received++;

                    Log.Write(Category.NetAdapter, "NAT receive from Perq {0} via Host {1}", src.Perq, src.Host);
                }

                // If source is a PERQ, see if the Type/Length field needs remappin'
                if (IsPerqPrefix(raw.SourceHwAddress))
                {
                    // Translate the EtherType/Length field if necessary
                    var perqType = PortMap((ushort)raw.Type);

                    if ((ushort)raw.Type != perqType)
                    {
                        Log.Write(Category.NetAdapter, "EtherType mapped from 0x{0:x4} to 0x{1:x4}", (ushort)raw.Type, perqType);
                        raw.Type = (EthernetPacketType)perqType;
                    }
                }
            }
            catch (PcapException ex)
            {
                Log.Warn(Category.NetAdapter, "Failed to receive packet: {0}", ex.Message);
                return;
            }
            catch (Exception ex)
            {
                Log.Warn(Category.NetAdapter, "Failed to receive packet: {0}", ex.Message);
                return;
            }

            //
            // Look for RARPs, which are pretty rare these days and will
            // almost certainly be PERQemu (or maybe QEMU :-) emulated hosts
            // broadcasting a greeting
            //
            ARPPacket rarp;

            try
            {
                rarp = (ARPPacket)raw.Extract(typeof(ARPPacket));

                if (rarp != null)
                {
                    Console.WriteLine(rarp);

                    // The Op can be a Request (new host coming online) or a
                    // Reply (from others responding after our Request sent);
                    // In either case the payload contains a host+Perq pair
                    // that we should add or update in our table!
                    if (IsPerqPrefix(rarp.TargetHardwareAddress))
                    {
                        var seen = _nat.LookupPerq(rarp.TargetHardwareAddress);
                        if (seen == null)
                        {
                            // Woo!  Another Perqy came out to play!
                            seen = new NATEntry(rarp.SenderHardwareAddress, rarp.TargetHardwareAddress);
                            _nat.Add(seen);

                            if (rarp.Operation == ARPOperation.RequestReverse)
                            {
                                // Since this is the first time we've heard from this
                                // host, send a RARP reply, since it's unlikely anyone
                                // still has an in.rarpd running these days? :-)
                                SendReply(rarp);
                            }
                        }
                        else
                        {
                            // Nice to see you again!
                            seen.LastReceived = DateTime.Now;
                            seen.Received++;
                        }
                    }
                    // I'm pretty sure we can safely drop these here and not
                    // pass them on to the PERQ, which almost certainly won't
                    // do RARP (even under Accent).  HOWEVER, Accent's "new"
                    // message server (in S6+) will do actual IP ARPs, so we
                    // don't want to get in the way of those.
                    Log.Info(Category.NetAdapter, "Local RARP handling complete");
                    return;
                }
                // Definitely fall through here
            }
            catch (PcapException ex)
            {
                Log.Info(Category.NetAdapter, "Failed to parse RARP packet: {0}", ex.Message);
                // No biggie, just continue
            }

            //
            // Finally, ask the PERQ if it wants the packet.  If it can
            // handle it right away, pass it on through, otherwise deal
            // with the pending queue.
            //
            if (_controller.WantReceive(raw.DestinationHwAddress))
            {
                // Shortcut: is the receiver active and ready?
                if (_controller.CanReceive)
                {
                    if (_pending.IsEmpty)
                    {
                        _controller.DoReceive(raw.Bytes);
                        return;
                    }

                    // Push the newest, then pop and process the oldest
                    _pending.Enqueue(raw);

                    if (_pending.TryDequeue(out raw))
                    {
                        _controller.DoReceive(raw.Bytes);
                        return;
                    }

                    Log.Write(Category.NetAdapter, "Tried to dequeue but couldn't?  Count now {0}", _pending.Count);
                    return;
                }

                // Controller is busy or not receiving; check if the queue has room
                if (_pending.Count < MaxBacklog)
                {
                    _pending.Enqueue(raw);

                    Log.Write(Category.NetAdapter, "Queued for later, count now {0}", _pending.Count);
                    return;
                }

                // Queue is full, so toss the oldest packet.  If the PERQ is
                // that far behind either the 'net is busy and it can't keep
                // up, or it isn't actively receiving and we don't want to
                // inundate it with old traffic if it comes back online
                _pending.Enqueue(raw);
                _pending.TryDequeue(out raw);
                Log.Write(Category.NetAdapter, "Max backlog {0} reached, dropped oldest packet", _pending.Count);
            }
        }

        /// <summary>
        /// Check if there are packets queued up; the controller is feelin' frisky.
        /// </summary>
        public void CheckReceive()
        {
            if (!_pending.IsEmpty && _controller.CanReceive)
            {
                EthernetPacket packet;

                if (_pending.TryDequeue(out packet))
                {
                    _controller.DoReceive(packet.Bytes);
                }
            }
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
                Log.Info(Category.NetAdapter, "Exception on shutdown (ignored): {0}", e.Message);
            }
            finally
            {
                _adapter.OnPacketArrival -= OnPacketArrival;
                _adapter.Close();

                _nat.Flush();

                Log.Write(Category.NetAdapter, "Adapter shutdown");
            }
        }

        /// <summary>
        /// Map a PERQ-specific EtherType to something that will pass on an 802.3
        /// network on transmit, or back again on receive.  Returns the type code
        /// unmodified if not a known PERQ type.  This is hacky as all get out.
        /// </summary>
        ushort PortMap(ushort etherType)
        {
            foreach (var pt in PerqEtherTypes)
            {
                if (etherType == pt)
                {
                    return (ushort)(etherType ^ 0xf000);
                }
            }

            return etherType;
        }

        /// <summary>
        /// Return true if a given address is in the official 3RCC address block.
        /// </summary>
        public static bool IsPerqPrefix(PhysicalAddress addr)
        {
            var a = addr.GetAddressBytes();

            return (a[0] == 0x02 && a[1] == 0x1c && a[2] == 0x7c && a[3] <= 2);
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
        /// so this is going to require further consideration... :-/
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
            //var interfaces = NetworkInterface.GetAllNetworkInterfaces();
            //
            //foreach (NetworkInterface adapter in interfaces)
            //{
            //    if (!IsEthernet(adapter.NetworkInterfaceType)) continue;
            //
            //    Console.WriteLine("ID: {0}  Name: {1}", adapter.Id, adapter.Name);
            //    Console.WriteLine(adapter.Description);
            //    Console.WriteLine(string.Empty.PadLeft(adapter.Description.Length, '='));
            //    Console.WriteLine("  Interface type ......... : {0}", adapter.NetworkInterfaceType);
            //    Console.WriteLine("  Operational status ..... : {0}", adapter.OperationalStatus);
            //    Console.WriteLine("  Hardware address ....... : {0}", adapter.GetPhysicalAddress());
            //    Console.WriteLine();
            //}
            //Console.WriteLine();

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
            Console.WriteLine($"  NIC: {Name} - {Description}");
            Console.WriteLine($"  Address: {Address}\tRunning: {Running}\tPending: {_pending.Count}");

            _nat.DumpTable();
        }

        // Ethernet Type codes defined in E10Types.Pas (plus mapped equivalents)
        public static ushort[] PerqEtherTypes =
        {
            0x0000,     // FTPByteStreamType   = 0
            0xf000,
            0x0001,     // FTPEtherType        = 1
            0xf001,
            0x0006,     // EchoServerType      = 6
            0xf006,
            0x0007,     // TimeServerType      = 7
            0xf007,
            0x013b,     // CSDXServerType      = 315; 
            0xf13b,
            0x0008,     // ServerRequest       = 8
            0xf008
        };

        // All 1's broadcast
        public static PhysicalAddress Broadcast = new PhysicalAddress(new byte[] { 255, 255, 255, 255, 255, 255 });

        ICaptureDevice _adapter;
        INetworkController _controller;

        NATTable _nat;

        DateTime _lastGreeting = DateTime.Now;
        const int GreetingInterval = 30;        // Minimum, in seconds

        ConcurrentQueue<EthernetPacket> _pending;
        const int MaxBacklog = 5;               // Don't queue without bound
    }
}
