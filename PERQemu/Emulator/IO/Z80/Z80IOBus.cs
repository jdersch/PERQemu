//
// Z80IOBus.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
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

using Konamiman.Z80dotNet;
using System;

namespace PERQemu.IO.Z80
{   
    /// <summary>
    /// Z80IOBus implements the Konamiman Z80dotNet IMemory interface for the
    /// purposes of providing access to the IO board's Z80 Port I/O space.
    /// </summary>
    public class Z80IOBus : IMemory
    {
        public Z80IOBus(Z80System system)
        {
            _devices = new IZ80Device[Size];
            _z80System = system;
        }

        public void Reset()
        {
            foreach(IZ80Device device in _devices)
            {
                if (device != null)
                {
                    device.Reset();
                }
            }
        }

        //
        // IMemory Implementation
        //
        public int Size => 0x100;        // 256 IO addresses

        public byte this[int address]
        { 
            get { return ReadPort(address); }
            set { WritePort(address, value); }
        }

        public byte[] GetContents(int startAddress, int length) { return null; }
        public void SetContents(int startAddress, byte[] contents, int startIndex = 0, int? length = null) { }

        //
        // Implementation
        //
        public void RegisterDevice(IZ80Device device)
        {
            foreach (ushort portAddress in device.Ports)
            {
                if (_devices[portAddress] != null)
                {
                    throw new InvalidOperationException(
                        string.Format("Z80 I/O Port conflict: Device {0} already registered at port 0x{1}",
                        _devices[portAddress], portAddress));
                }
                else
                {
                    _devices[portAddress] = device;
                    _z80System.CPU.RegisterInterruptSource(device);
                }
            }
        }

        private byte ReadPort(int port)
        {
            IZ80Device device = _devices[port];
            byte value = 0x0;

            if (device != null)
            {
                value = device.Read((byte)port);

                Trace.Log(LogType.Z80State, "Z80 Port Read from 0x{0:x} ({1}), returning 0x{2:x}", port, device.Name, value);
            }
            else
            {
                Trace.Log(LogType.Z80State, "Z80 Port Read from 0x{0:x} unhandled, returning 0xff.", port);
            }

            return value;
        }

        private void WritePort(int port, byte value)
        {
            IZ80Device device = _devices[port];

            if (device != null)
            {
                device.Write((byte)port, value);

                Trace.Log(LogType.Z80State, "Z80 Port Write of 0x{0:x} to 0x{1:x} ({2})", value, port, device.Name);
            }
            else
            {
                Trace.Log(LogType.Z80State, "Z80 Port Write of 0x{0:x} to 0x{1:x} unhandled, returning 0.", value, port);
            }
        }

        private IZ80Device[] _devices;
        private Z80System _z80System;
    }
}
