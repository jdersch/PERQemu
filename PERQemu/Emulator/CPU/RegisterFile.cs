//
// RegisterFile.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using System.Runtime.CompilerServices;

namespace PERQemu.Processor
{
    public partial class CPU
    {
        /// <summary>
        /// Implements the CPU's XY register file, containing 256 word-sized
        /// general purpose registers.  For the 16K and newer CPUs, includes
        /// the base register indexing feature.
        /// </summary>
        private class RegisterFile
        {
            public RegisterFile()
            {
                _registers = new int[256];
            }

            public void Reset()
            {
                // Clear the register file
                for (int i = 0; i < 256; i++)
                {
                    _registers[i] = 0;
                }

                _registerBase = 0;

                Log.Debug(Category.Registers, "Reset");
            }

            public byte RegisterBase
            {
                get { return _registerBase; }
                set { _registerBase = value; }
            }

            /// <summary>
            /// Return the contents of a register; index by the base register
            /// if appropriate (16K or newer CPU).
            /// </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public int ReadRegister(byte addr)
            {
                if (addr < 0x40)
                {
                    return _registers[addr | _registerBase];
                }
                else
                {
                    return _registers[addr];
                }
            }

            /// <summary>
            /// Write a result back to the register file.  If appropriate, apply
            /// the base register.
            /// </summary>
            /// <remarks>
            /// We ASSUME that the assembler sets W for R := (MQ | Victim) phrases
            /// on PERQ1A . This appears to be true, but we'll leave that up to
            /// the caller; this method does not enforce the W bit check.
            /// </remarks>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public void WriteRegister(byte addr, int val)
            {
                // Clip to 20 or 24 bits
                val &= CPUMask;

                if (addr < 0x40)
                {
                    _registers[addr | _registerBase] = val;
                    Log.Debug(Category.Registers, "%XY[{0:x2}]={1:x6}", (addr | _registerBase), val);
                }
                else
                {
                    _registers[addr] = val;
                    Log.Debug(Category.Registers, "XY[{0:x2}]={1:x6}", addr, val);
                }
            }

            // XY registers
            private int[] _registers;

            // Base register
            private byte _registerBase;
        }
    }
}
