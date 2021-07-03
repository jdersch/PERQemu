// IOBBus.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using Konamiman.Z80dotNet;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace PERQemu.IO.Z80_new
{   
    /// <summary>
    /// IOBIOBus implements the Konamiman Z80dotNet IMemory interface for the purposes of
    /// providing access to the IOB's Z80 Port I/O space.
    /// </summary>
    public class IOBIOBus : IMemory
    {
        public IOBIOBus(Z80System system)
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
        public byte this[int address] 
        { 
            get 
            {
                return ReadPort(address);
            }

            set 
            {
                WritePort(address, value);
            }
        }

        public int Size 
        { 
            get { return 0x100; } // 256 IO addresses
        }
        
        public byte[] GetContents(int startAddress, int length) { return null; }
        public void SetContents(int startAddress, byte[] contents, int startIndex = 0, int? length = null) { }

        //
        // Implementation
        //
        public void RegisterDevice(IZ80Device device)
        {
            foreach(ushort portAddress in device.Ports)
            {
                if (_devices[portAddress] != null)
                {
                    throw new InvalidOperationException(
                        String.Format("Z80 I/O Port conflict: Device {0} already registered at port 0x{1}",
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
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 Port Read from 0x{0:x} ({1}), returning 0x{2:x}", port, device.Name, value);
#endif
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 Port Read from 0x{0:x} unhandled, returning 0xff.", port);
#endif
            }

            return value;
        }

        private void WritePort(int port, byte value)
        {
            IZ80Device device = _devices[port];
            if (device != null)
            {
                device.Write((byte)port, value);
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 Port Write of 0x{0:x} to 0x{1:x} ({2})", value, port, device.Name);
#endif
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 Port Write of 0x{0:x} to 0x{1:x} unhandled, returning 0.", value, port);
#endif
            }
        }

        private IZ80Device[] _devices;
        private Z80System _z80System;
    }

    /// <summary>
    /// IOBIOBus implements the Konamiman Z80dotNet IMemory interface for the purposes of
    /// providing access to the IOB's Z80 Memory space.
    /// </summary>
    public class IOBMemoryBus : IMemory, IDMADevice
    {
        public IOBMemoryBus()
        {
            LoadROM();
        }

        //
        // IMemory Implementation
        //
        public byte this[int address]
        {
            get { return ReadByte(address); }
            set { WriteByte(address, value); }
        }

        public int Size
        {
            get { return 0x10000; } // 64K address space
        }

        public bool ReadDataReady => true;      // Always ready

        public bool WriteDataReady => true;     // Always ready

        public byte[] GetContents(int startAddress, int length) { return null; }
        public void SetContents(int startAddress, byte[] contents, int startIndex = 0, int? length = null) { }

        //
        // Implementation
        //
        private byte ReadByte(int address)
        {
            if (address < ROM_SIZE)
            {
                return _rom[address];
            }
            else if (address >= RAM_ADDRESS && address < RAM_ADDRESS + RAM_SIZE)
            {
                return _ram[address - RAM_ADDRESS];
            }
            else
            {
                // throw for now so I can see what's going on
                throw new InvalidOperationException(String.Format("Unexpected memory read at address 0x{0:x}.", address));
            }
        }

        private void WriteByte(int address, byte value)
        {
            if (address >= RAM_ADDRESS && address < RAM_ADDRESS + RAM_SIZE)
            {
                _ram[address - RAM_ADDRESS] = value;
            }
            else
            {
                // throw for now so I can see what's going on
                throw new InvalidOperationException(String.Format("Unexpected memory write at address 0x{0:x} of 0x{1:x}.", address, value));
            }
        }

        public byte DMARead(ushort address)
        {
            return ReadByte(address);
        }

        public void DMAWrite(ushort address, byte value)
        {
            WriteByte(address, value);
        }

        public void DMATerminate()
        {

        }

        private void LoadROM()
        {
            using (FileStream fs = new FileStream(Paths.BuildPROMPath("pz80.bin"), FileMode.Open, FileAccess.Read))
            {
                if (fs.Read(_rom, 0, _rom.Length) != _rom.Length)
                {
                    throw new InvalidOperationException("Invalid Z80 ROM size.");
                }
            }
        }

        private const int RAM_SIZE = 0x400;      // 1K of ram
        private const int RAM_ADDRESS = 0x2c00;
        private const int ROM_SIZE = 0x2000;     // 8K of rom
        private const int ROM_ADDRESS = 0x0;

        private byte[] _rom = new byte[ROM_SIZE];
        private byte[] _ram = new byte[RAM_SIZE];
    }
}
