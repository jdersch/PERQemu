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
using PacketDotNet;
using PacketDotNet.Utils;

using PERQemu.Config;

namespace PERQemu.IO.Network
{
    /// <summary>
    /// Encapsulate a host Ethernet interface for sending and receiving PERQ
    /// packets on a real network.
    /// </summary>
    public class HostAdapter
    {
        public HostAdapter(INetworkController controller, string devName)
        {
            _nat = new NATTable();
            _pending = new ConcurrentQueue<EthernetPacket>();

            _controller = controller;
            _adapter = GetAdapter(devName);

            if (_adapter == null)
            {
                throw new UnimplementedHardwareException("Host adapter not found");
            }

            // Open the device and register our receive callback
            _adapter.Open(DeviceMode.Promiscuous);
            _adapter.OnPacketArrival += OnPacketArrival;

            // This _seems_ to avoid the spurious exception on shutdown
            _adapter.StopCaptureTimeout = new TimeSpan(100000000);

            // Initialize statistics
            _pktsSent = _pktsRecvd = _pktsIgnored = _pktsQueued = _pktsDropped = 0;

            Log.Info(Category.NetAdapter, "Device opened [Host MAC: {0}]", _adapter.MacAddress);
        }

        public string Name => _adapter?.Name;
        public string Description => _adapter?.Description;
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
                // Add our local NAT entry
                if (!_nat.Add(new NATEntry(_adapter.MacAddress, _controller.MACAddress, true)))
                {
                    Log.Warn(Category.All, "Another PERQ detected with our MAC address!?");
                }

                // Fire up the receive thread
                _adapter.StartCapture();

                Log.Info(Category.NetAdapter, "Adapter reset (packet capture started)");
                return;
            }

            // Announce our presence with authori-tie
            SendGreeting();
        }

        /// <summary>
        /// Flush oldest received packets to keep queue from growing too long.
        /// </summary>
        public void Flush()
        {
            int max = _pending.Count;
            int count = 0;

            // Todo: check a timestamp, or maybe keep most recent packet?
            // POS programs must explicitly be in a polling mode and may ignore
            // incoming traffic forever; Accent is more modern in that it tries
            // to dispatch packets as they arrive so queues shouldn't back up;
            // not sure about PNX (not enough testing/experience there).
            while (_pending.Count >= MaxBacklog)
            {
                EthernetPacket tossIt;
                if (_pending.TryDequeue(out tossIt))
                {
                    count++;
                    _pktsDropped++;
                }
            }

            if (count > 0)
                Log.Info(Category.NetAdapter, "Max backlog exceeded ({0}), flushed {1} packet(s)", max, count);
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
                _pktsSent++;
                _lastGreeting = DateTime.Now;

                Log.Info(Category.NetAdapter, "Sent RARP request from {0}", _controller.MACAddress);
            }
            catch (PcapException ex)
            {
                Log.Error(Category.NetAdapter, "Failed to send greeting packet: {0}", ex.Message);
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
                _pktsSent++;

                Log.Info(Category.NetAdapter, "Sent RARP reply to {0}", greeting.TargetHardwareAddress);
            }
            catch (PcapException ex)
            {
                Log.Error(Category.NetAdapter, "Failed to send greeting reply: {0}", ex.Message);
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
                    // Dump more data (if this ever happens); drop it like it's hot
                    return false;
                }

                Log.Info(Category.NetAdapter, "Sending from {0} to {1} (type {2})",
                          packet.SourceHwAddress, packet.DestinationHwAddress, packet.Type);
                Log.Debug(Category.NetAdapter, "SIZES: packet {0}, header {1}, payload {2}",
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

                            Log.Info(Category.NetAdapter, "NAT send to Perq {0} via Host {1}", map.Perq, map.Host);
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
                        Log.Info(Category.NetAdapter, "EtherType mapped from 0x{0:x4} to 0x{1:x4}", (ushort)packet.Type, perqType);
                        packet.Type = (EthernetPacketType)perqType;
                    }
                }

                // Now generate the checksum for the packet (for debugging);
                // SharpPcap _will_ generate and append this for us, apparently
                var crc = Crc32.Compute(packet.Bytes, 0, packet.Bytes.Length - 4);
                Log.Debug(Category.NetAdapter, "Computed CRC is {0:x8}", crc);

                // Print the (modified) packet
                if (Log.Level < Severity.Info) Console.WriteLine(packet.PrintHex());

                // So send it already, sheesh
                _adapter.SendPacket(packet);
                _pktsSent++;
                return true;
            }
            catch (PcapException ex)
            {
                Log.Error(Category.NetAdapter, "Failed to send packet: {0}", ex.Message);
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
                _pktsIgnored++;
                Log.Warn(Category.NetAdapter, "Non-Ethernet packet type {0} ignored", e.Packet.LinkLayerType);
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

                // Stuff we just drop because it's completely irrelevant to the
                // old PERQ and is just pure noise:  IPv6 and spanning tree
                // multicasts every 2 seconds... probably more we could add...
                if (raw.Type == EthernetPacketType.IpV6) return;
                if ((ushort)raw.Type == 0x0026) return;

                Log.Info(Category.NetAdapter, "Received from {0} to {1} (type {2:x}) [{3}]",
                          raw.SourceHwAddress, raw.DestinationHwAddress, raw.Type,
                          System.Threading.Thread.CurrentThread.ManagedThreadId);
                Log.Debug(Category.NetAdapter, "SIZES: packet {0}, header {1}, payload {2}",
                          raw.Bytes.Length, raw.Header.Length, raw.PayloadData?.Length);

                // Todo: wrap this in a DEBUG or report the results, else it's wasted effort
                // Recompute the checksum for the packet
                var len = raw.Bytes.Length - 4;
                var crc = Crc32.Compute(raw.Bytes, 0, len);
                Log.Debug(Category.NetAdapter, "Computed CRC is {0:x8}", crc);

                var check = ((raw.Bytes[len] << 24) |
                             (raw.Bytes[len + 1] << 16) |
                             (raw.Bytes[len + 2] << 8) |
                              raw.Bytes[len + 3]);

                Log.Debug(Category.NetAdapter, "Received CRC is {0:x8}", check);

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

                    Log.Info(Category.NetAdapter, "NAT receive from Perq {0} via Host {1}", src.Perq, src.Host);
                }

                // If source is a PERQ, see if the Type/Length field needs remappin'
                if (IsPerqPrefix(raw.SourceHwAddress) || raw.DestinationHwAddress.Equals(Broadcast))
                {
                    // Translate the EtherType/Length field if necessary
                    var perqType = PortMap((ushort)raw.Type);

                    if ((ushort)raw.Type != perqType)
                    {
                        Log.Info(Category.NetAdapter, "EtherType mapped from 0x{0:x4} to 0x{1:x4}", (ushort)raw.Type, perqType);
                        raw.Type = (EthernetPacketType)perqType;
                    }
                }
            }
            catch (PcapException ex)
            {
                Log.Warn(Category.NetAdapter, "(Pcap) Failed to receive packet: {0}", ex.Message);
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
                    Log.Info(Category.NetAdapter, "RARP {0} received from {1}",
                                                   rarp.Operation, rarp.TargetHardwareAddress);

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
                            seen.Flags &= ~(Flags.Stale);
                            seen.Received++;
                        }
                    }

                    if (rarp.Operation == ARPOperation.RequestReverse ||
                        rarp.Operation == ARPOperation.ReplyReverse)
                    {
                        // I'm pretty sure we can safely drop these here and not
                        // pass them on to the PERQ, which almost certainly won't
                        // do RARP (even under Accent).  HOWEVER, Accent's "new"
                        // message server (in S6+) will do actual IP ARPs, so we
                        // don't want to get in the way of those.
                        Log.Info(Category.NetAdapter, "Local RARP handling complete");
                        return;
                    }
                }
                // Definitely fall through here
            }
            catch (PcapException ex)
            {
                Log.Info(Category.NetAdapter, "Failed to parse RARP packet: {0}", ex.Message);
                // No biggie, just continue
            }

            //
            // Does the PERQ want this packet?
            //
            if (!_controller.WantReceive(raw.DestinationHwAddress))
            {
                _pktsIgnored++;
                return;
            }

            //
            // We've run the gauntlet and received the packet; if we can handle it
            // right away, pass it on through, otherwise deal with the pending queue
            //

            // DEBUGGING: Print the packet post-rewrites
            if (Log.Level < Severity.Info) Console.WriteLine(raw.PrintHex());

            // Shortcut: is the receiver active and ready?
            if (_controller.CanReceive)
            {
                if (_pending.IsEmpty)
                {
                    _pktsRecvd++;
                    _controller.DoReceive(raw.Bytes);
                    return;
                }

                // Push the newest, then pop and process the oldest
                _pending.Enqueue(raw);
                _pktsQueued++;

                if (_pending.TryDequeue(out raw))
                {
                    _pktsRecvd++;
                    _controller.DoReceive(raw.Bytes);
                    return;
                }

                Log.Info(Category.NetAdapter, "Tried to dequeue but couldn't?  Count is {0}", _pending.Count);
                return;
            }

            // Controller is busy or not receiving; check if the queue has room
            if (_pending.Count < MaxBacklog)
            {
                _pending.Enqueue(raw);
                _pktsQueued++;

                Log.Info(Category.NetAdapter, "Queued for later, count now {0}", _pending.Count);
                return;
            }

            // Queue is full, so toss the oldest packet.  If the PERQ is
            // that far behind either the 'net is busy and it can't keep
            // up, or it isn't actively receiving and we don't want to
            // inundate it with old traffic if it comes back online
            _pending.Enqueue(raw);
            _pktsQueued++;
            Flush();
        }

        /// <summary>
        /// Check if there are packets queued up; the controller is feelin' frisky.
        /// </summary>
        public void CheckReceive()
        {
            if (!_pending.IsEmpty && _controller.CanReceive)
            {
                EthernetPacket packet;

                if (!_pending.TryDequeue(out packet))
                {
                    Log.Warn(Category.NetAdapter, "Failure on TryDequeue!? Count is {0}", _pending.Count);
                    return;
                }

                _pktsRecvd++;
                _controller.DoReceive(packet.Bytes);
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

                Log.Info(Category.NetAdapter, "Adapter shutdown");
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
                    return (ushort)(etherType ^ EtherTypeMask);
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
        /// Find the adapter that matches the interface name.  The C# runtime
        /// gives back completely different names than the list SharpPcap (or
        /// its underlying LibPcap/WinPcap/AirPcap library) gives back, so this
        /// is going to require further consideration and way more testing! :-/
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
            Console.WriteLine("\nHost adapter status:");
            Console.WriteLine($"  NIC: {Name} - {Description}");
            Console.WriteLine($"  Address: {Address}\tRunning: {Running}\tPending: {_pending.Count}");
            Console.WriteLine("\nInterface statistics:");
            Console.WriteLine($"  Total sent: {_pktsSent}\tReceived: {_pktsRecvd}\tIgnored: {_pktsIgnored}");
            Console.WriteLine($"  Deferred:   {_pktsQueued}\tDropped: {_pktsDropped}");

            _nat.DumpTable();
        }

        // Mask for mapping PERQ EtherType codes that fall within the IEEE 802.3
        // length range (0..1535) to an unused range and back again.  (The range
        // is chosen from unassigned space that IANA hasn't officially allocated)
        const ushort EtherTypeMask = 0xb000;

        // Ethernet Type codes defined in E10Types.Pas (plus mapped equivalents)
        public static ushort[] PerqEtherTypes =
        {
            0x0000, EtherTypeMask,              // FTPByteStreamType
            0x0001, EtherTypeMask + 1,          // FTPEtherType
            0x0006, EtherTypeMask + 6,          // EchoServerType
            0x0007, EtherTypeMask + 7,          // TimeServerType
            0x0008, EtherTypeMask + 8,          // ServerRequest
            0x0090, EtherTypeMask + 144,        // Accent ConfigTest
            0x00db, EtherTypeMask + 219,        // Accent Time/repeater discovery?
            0x013b, EtherTypeMask + 315,        // CSDXServerType
            0x01c0, EtherTypeMask + 448,        // Accent EchoMe
            0x01c1, EtherTypeMask + 449         // Accent IAmAnEcho
        };
        //
        // PUP and the PUP "Addr Tran" (not explicitly noted in Accent?) are
        // problematic; they should be reassigned to their relocated assigned
        // numbers 0x0a00 and 0x0a01, but check with ContrAlto to see what if
        // any remapping goes on there?  May need special handling here.
        //  0x0200, EtherTypeMask + 512,        // PUP
        //  0x0201, EtherTypeMask + 513         // PUP Addr Trans
        //

        // All 1's layer 2 broadcast
        public static PhysicalAddress Broadcast = new PhysicalAddress(new byte[] { 255, 255, 255, 255, 255, 255 });

        ICaptureDevice _adapter;
        INetworkController _controller;

        NATTable _nat;

        DateTime _lastGreeting = DateTime.Today;
        const int GreetingInterval = 15;        // Minimum, in seconds

        ConcurrentQueue<EthernetPacket> _pending;
        const int MaxBacklog = 15;              // Don't queue without bound

        ulong _pktsRecvd, _pktsSent;            // Some basic statistics,
        ulong _pktsQueued, _pktsDropped;        // for debugging/curiosity
        ulong _pktsIgnored;
    }
}
