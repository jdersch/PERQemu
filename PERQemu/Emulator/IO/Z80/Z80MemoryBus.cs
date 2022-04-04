//
// Z80MemoryBus.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.IO;

namespace PERQemu.IO.Z80
{   
    /// <summary>
    /// Z80MemoryBus implements the Konamiman Z80dotNet IMemory interface to
    /// provide access to the IO board's Z80 Memory space.
    /// </summary>
    public class Z80MemoryBus : IMemory, IDMADevice
    {
        public Z80MemoryBus()
        {
            RAM_SIZE = IOBoard.Z80_RAM_SIZE;
            RAM_ADDRESS = IOBoard.Z80_RAM_ADDRESS;
            ROM_SIZE = IOBoard.Z80_ROM_SIZE;
            ROM_ADDRESS = IOBoard.Z80_ROM_ADDRESS;

            _rom = new byte[ROM_SIZE];
            _ram = new byte[RAM_SIZE];

            Log.Debug(Category.Z80, "Allocated {0:N1}KB ROM, {1:N1}KB RAM",
                                    ROM_SIZE / 1024, RAM_SIZE / 1024);
        }

        //
        // IMemory Implementation
        //
        public byte this[int address]
        {
            get { return ReadByte(address); }
            set { WriteByte(address, value); }
        }

        public int Size => 0x10000;             // 64K address space
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

            if (address >= RAM_ADDRESS && address < RAM_ADDRESS + RAM_SIZE)
            {
                return _ram[address - RAM_ADDRESS];
            }

            // throw for now so I can see what's going on
            throw new InvalidOperationException($"Unexpected memory read at address 0x{address:x}");
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
                throw new InvalidOperationException($"Unexpected memory write at address 0x{address:x} of 0x{value:x}");
            }
        }

        public void DMATerminate()
        {
        }

        public void LoadROM(string path)
        {
            using (var fs = new FileStream(path, FileMode.Open, FileAccess.Read))
            {
                if (fs.Read(_rom, 0, _rom.Length) != _rom.Length)
                {
                    throw new InvalidOperationException("Invalid Z80 ROM size");
                }

                Log.Info(Category.Emulator, "Loaded Z80 ROM from {0}", Paths.Canonicalize(path));
            }
        }

        // Configured by the IO Board
        private int RAM_SIZE;
        private int RAM_ADDRESS;
        private int ROM_SIZE;
        private int ROM_ADDRESS;

        private byte[] _rom;
        private byte[] _ram;
    }
}
