//
// Ether3MbitController.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Net.NetworkInformation;

namespace PERQemu.IO.Network
{
    /// <summary>
    /// PERQ side of the rare 3Mbit Ethernet controller.  To be implemented like
    /// ContrAlto using 3-in-10 encapsulation.  A placeholder for now.
    /// </summary>
    public class Ether3MbitController : INetworkController
    {
        public Ether3MbitController(PERQSystem sys)
        {
            _system = sys;
        }

        public void Reset()
        {
        }

        public void Shutdown()
        {
        }

        public void LoadRegister(byte address, int value)
        {
            throw new InvalidOperationException($"Unhandled write to port 0x{address:x}");
        }

        public void LoadCommand(int value)
        {
            throw new InvalidOperationException("Unhandled write to command port");
        }

        public int ReadRegister(byte address)
        {
            throw new InvalidOperationException($"Unhandled read from port 0x{address:x}");
        }

        public int ReadStatus()
        {
            return -1;
        }

        public bool WantReceive(PhysicalAddress dest)
        {
            throw new NotImplementedException();
        }

        public void DoReceive(byte[] packet)
        {
            throw new NotImplementedException();
        }

        // Debugging
        public void DumpEther()
        {
            Console.WriteLine("3Mbit Ethernet status:  Not yet implemented");
        }

        PERQSystem _system;
    }
}

/*
    Notes:

    Have to research the "3-in-10" encapsulation mechanism used to wrap old
    3Mbit packets to make sure the PERQ interface can talk to other emulators
    (ContrAlto, Darkstar) or potentially real hardware using the same strategy.

    Frame format was way different:  8 bit dst, 8 bit src, 16 bit type, up to
    554 bytes of payload and a 16 bit CRC...

    No known schematics of the PERQ board; microcode (and Pascal?) support is
    fairly complete so I should be able to figure out the interface from that.

 */