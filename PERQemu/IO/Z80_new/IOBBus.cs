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
        public IOBIOBus()
        {

        }

        //
        // IMemory Implementation
        //
        public byte this[int address] 
        { 
            get { return 0; }
            set { }
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
    }

    /// <summary>
    /// IOBIOBus implements the Konamiman Z80dotNet IMemory interface for the purposes of
    /// providing access to the IOB's Z80 Memory space.
    /// </summary>
    public class IOBMemoryBus : IMemory
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
                return _ram[address];
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
                _ram[address] = value;
            }
            else
            {
                // throw for now so I can see what's going on
                throw new InvalidOperationException(String.Format("Unexpected memory write at address 0x{0:x} of 0x{1:x}.", address, value));
            }
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
