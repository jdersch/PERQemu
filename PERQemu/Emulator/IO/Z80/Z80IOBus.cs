//
// Z80IOBus.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

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
            _devices = new List<IZ80Device>();
            _devicePorts = new IZ80Device[Size];
            _z80System = system;

            _status = new bool[Size];   // debug
        }

        public void Reset()
        {
            foreach (IZ80Device device in _devices)
            {
                if (device != null)
                {
                    device.Reset();
                }
            }
        }

        /// <summary>
        /// Debugging: print transitions of Z80 IRQ signals.  For EIO this might
        /// become a standalone priority encoder (Am9519)?
        /// </summary>
        public void ActiveInterrupts()
        {
            for (var d = 0; d < _devices.Count; d++)
            {
                if (_devices[d].IntLineIsActive)
                {
                    if (!_status[d])
                        Log.Debug(Category.Z80IRQ, "Device {0} raised, vector is {1:x2}",
                                  _devices[d].Name, _devices[d].ValueOnDataBus);
                    _status[d] = true;
                }
                else
                {
                    if (_status[d])
                        Log.Debug(Category.Z80IRQ, "Device {0} cleared", _devices[d].Name);
                    _status[d] = false;
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
            // Add to (the local copy of) the device list
            if (_devices.Contains(device))
            {
                throw new InvalidOperationException($"Z80 I/O device {device.Name} already registered");
            }
            _devices.Add(device);

            // Register the ports, checking for overlaps
            foreach (ushort portAddress in device.Ports)
            {
                if (_devicePorts[portAddress] != null)
                {
                    throw new InvalidOperationException($"Z80 I/O Port conflict: Device {_devicePorts[portAddress]} already registered at port 0x{portAddress:x2}");
                }

                _devicePorts[portAddress] = device;
            }

            // Tell the Z80 about it
            _z80System.CPU.RegisterInterruptSource(device);
        }

        byte ReadPort(int port)
        {
            IZ80Device device = _devicePorts[port];
            byte value = 0xff;

            if (device != null)
            {
                value = device.Read((byte)port);

                Log.Debug(Category.Z80, "Read 0x{0:x} from port 0x{1:x} ({2})", value, port, device.Name);
            }
            else
            {
                Log.Warn(Category.Z80, "Unhandled Read from port 0x{0:x}, returning 0xff", port);
            }

            return value;
        }

        void WritePort(int port, byte value)
        {
            IZ80Device device = _devicePorts[port];

            if (device != null)
            {
                device.Write((byte)port, value);

                Log.Debug(Category.Z80, "Write 0x{0:x} to port 0x{1:x} ({2})", value, port, device.Name);
            }
            else
            {
                Log.Warn(Category.Z80, "Unhandled Write of 0x{0:x} to port 0x{1:x}", value, port);
            }
        }

        // debug
        bool[] _status;

        List<IZ80Device> _devices;
        IZ80Device[] _devicePorts;
        Z80System _z80System;
    }
}
