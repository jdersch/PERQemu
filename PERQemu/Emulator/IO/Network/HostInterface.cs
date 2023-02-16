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
using SharpPcap.LibPcap;
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
            _adapter = null;
        }

        public HostInterface(ICaptureDevice dev)
        {
            _adapter = dev;
        }

        public string Name => _adapter.Name;
        public string Description => _adapter.Description;

        public bool Start()
        {
            if (_adapter == null) return false;

            _adapter.Open(DeviceMode.Promiscuous);
            _adapter.OnPacketArrival += OnPacketArrival;
            _adapter.StartCapture();

            return _adapter.Started;
        }

        void OnPacketArrival(object s, CaptureEventArgs e)
        {
            Console.WriteLine(e.Packet);
        }

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
                _adapter.Close();
            }
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
            var devices = CaptureDeviceList.New();

            // If no devices were found print an error
            if (devices.Count < 1)
            {
                Console.WriteLine("No devices were found on this machine");
                return;
            }

            int i = 0;

            // Print out the devices
            foreach (var dev in devices)
            {
                Console.WriteLine("{0}) {1} {2}", i, dev.Name, dev.Description);
                i++;
            }
        }

        ICaptureDevice _adapter;
    }
}
