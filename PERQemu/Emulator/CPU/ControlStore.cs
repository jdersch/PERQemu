//
// ControlStore.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using System;
using System.IO;
using System.Runtime.CompilerServices;

namespace PERQemu.Processor
{
    public partial class CPU
    {

        public enum ControlStoreWord
        {
            Low = 0,
            Middle,
            High
        }

        /// <summary>
        /// Implements the Writable Control Store, and functions to unscramble and
        /// load instructions into it.  Also manages the Boot ROM functionality.
        /// </summary>
        public class ControlStore
        {
            public ControlStore()
            {
                // For now, the only difference is size: 4K or 16K
                _microcode = new ulong[_wcsSize];
                _microcodeCache = new Instruction[_wcsSize];

                // This is currently fixed; PERQ-1 and PERQ-2 use 512 x 48 bits
                // of ROM, although the ROM overlays the low 2K of address space
                _romSize = 512;
                _rom = new ulong[_romSize];

                Log.Debug(Category.Microstore, "Allocated {0}K RAM, {1:n}K ROM",
                                               _wcsSize / 1024, _romSize / 1024f);
            }

            public void Reset()
            {
                // Clear the RAMs and the cache
                for (int i = 0; i < _wcsSize; i++)
                {
                    _microcode[i] = 0;
                    _microcodeCache[i] = null;
                }

                // Reset flags
                _wcsHold = false;
                _romEnabled = true;

                Log.Debug(Category.Microstore, "WCS reset, ROM enabled");
            }

            public bool Hold
            {
                get { return _wcsHold; }
                set { _wcsHold = value; }
            }

            public bool ROMEnabled
            {
                get { return _romEnabled; }
            }

            public ulong[] Microcode
            {
                get { return _microcode; }
            }


            /// <summary>
            /// Disable the Boot ROM and invalidate the cache.  The hardware does
            /// this through the first execution of the LoadOp function; the ROM
            /// is only re-enabled by pressing the boot button.
            /// </summary>
            public void DisableROM()
            {
                if (_romEnabled)
                {
                    for (int i = 0; i < _romSize; i++)
                    {
                        _microcodeCache[i] = null;
                    }

                    _romEnabled = false;
                }
            }


            /// <summary>
            /// Reads the instruction from the microcode RAM (or ROM, if enabled)
            /// at the given address, clipped to the address range for this CPU.
            /// </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public Instruction GetInstruction(ushort addr)
            {
                addr &= (ushort)WCSMask;

                // Return the precomputed microcode, unless it has yet to be cached
                if (_microcodeCache[addr] == null)
                {
                    ulong word;

                    if (_romEnabled)
                    {
                        if (addr < _romSize)
                        {
                            word = _rom[addr];
                        }
                        else
                        {
                            word = _microcode[addr];
                        }
                    }
                    else
                    {
                        word = _microcode[addr];
                    }

                    _microcodeCache[addr] = new Instruction(word, addr);
                }

                return _microcodeCache[addr];
            }

            /// <summary>
            /// Writes a 16-bit subword of a 48-bit microcode instruction to the
            /// control store.  Clips address to the range for this CPU.
            /// </summary>
            public void WriteControlStore(ControlStoreWord word, ushort addr, ushort data)
            {
                addr &= (ushort)WCSMask;

                ulong cs = _microcode[addr];

                _microcode[addr] = UnscrambleControlStoreWord(cs, word, data);

                // Invalidate cache for this instruction
                _microcodeCache[addr] = null;

                // Set the hold bit to inject a wait state
                _wcsHold = true;

                Log.Detail(Category.Microstore,
                          "Wrote {0:x4} at WCS {1:x4} ({2}) -- now contains {3:x12}",
                          data, addr, word, _microcode[addr]);
            }

            /// <summary>
            /// Unscrambles a control store word.  This is complicated by the fact
            /// that the bit mapping from the ALU output to the control store is
            /// all scrambled up by the hardware.
            ///
            /// This is not a very efficient routine.  But it doesn't get executed
            /// very frequently (usually only at OS load) so I'm going to leave it
            /// alone for now.
            /// </summary>
            private ulong UnscrambleControlStoreWord(ulong current, ControlStoreWord word, ushort data)
            {
                // We write the inverse of the data (outputs are active low)
                data = (ushort)(~data);

                switch (word)
                {
                    case ControlStoreWord.Low:
                        current = SetBits(data, current,
                            new int[] { 24, 25, 22, 27, 23, 29, 30, 31, 8, 9, 10, 11, 12, 13, 14, 15 });
                        break;

                    case ControlStoreWord.Middle:
                        current = SetBits(data, current,
                            new int[] { 16, 17, 18, 19, 20, 21, 26, 28, 0, 1, 2, 3, 4, 5, 6, 7 });
                        break;

                    case ControlStoreWord.High:
                        // The high word corresponds directly to the data being written,
                        // it's just shifted over 32 bits.  Yay.
                        current = ((current & 0x0000ffffffff) | (ulong)(data) << 32);
                        break;
                }
                return current;
            }

            /// <summary>
            /// Sets a series of bits in the 48-bit destination word from a 16-bit source word.
            /// </summary>
            public ulong SetBits(ulong sourceWord, ulong destWord, int[] destBits)
            {
                for (int sourceBit = 0; sourceBit < destBits.Length; sourceBit++)
                {
                    // Get the bit value from the source word
                    ulong bitValue = (sourceWord >> sourceBit) & 0x1;

                    // Frob that into the destination word at its magical location
                    destWord = (destWord & ~((ulong)0x1 << destBits[sourceBit])) | (bitValue << destBits[sourceBit]);
                }

                return destWord;
            }

            /// <summary>
            /// Loads the boot ROM from a file on disk and descrambles it.  This
            /// currently expects a ROM built by PRQMIC, from microcode sources,
            /// not an actual ROM dump from the PERQ.
            /// </summary>
            public void LoadROM(string path)
            {
                using (var fs = new FileStream(path, FileMode.Open))
                {
                    // Read all 512 scrambled instruction words in...
                    for (ushort i = 0; i < _romSize; i++)
                    {
                        ushort addr = 0;
                        ulong word = ReadMicrocodeWord(fs, out addr);

                        // The only address outside of range should be the last (which has addr 0xffff)
                        if (addr < _romSize)
                        {
                            _rom[addr] = word;
                        }
                    }
                    fs.Close();
                }
                Log.Info(Category.Emulator, "Loaded boot ROM from {0}", Paths.Canonicalize(path));
            }

            /// <summary>
            /// Load microcode from a file into RAM.
            /// </summary>
            /// <remarks>
            /// Loops over the input until EOF, since we don't know in advance
            /// what address range or what order the instructions are in.
            /// </remarks>
            public void LoadMicrocode(string path)
            {
                bool done = false;

                using (var fs = new FileStream(path, FileMode.Open))
                {
                    while (!done)
                    {
                        ushort addr = 0;
                        ulong word = ReadMicrocodeWord(fs, out addr);

                        // The only address outside of range should be the last (which has addr 0xffff)
                        if (addr != 0xffff)
                        {
                            _microcode[addr] = word;
                            _microcodeCache[addr] = null;
                        }
                        else
                        {
                            done = true;
                        }
                    }
                    fs.Close();
                }
                _romEnabled = false;

                Log.Info(Category.Microstore, "Loaded microcode from {0}", Paths.Canonicalize(path));
            }

            /// <summary>
            /// Reads a 48-bit word in from the given stream.
            /// </summary>
            private ulong ReadMicrocodeWord(FileStream fs, out ushort addr)
            {
                ulong word = 0;

                // Read the address, low bits first
                addr = (ushort)(fs.ReadByte());
                addr |= (ushort)(fs.ReadByte() << 8);

                // Read the instruction one byte at a time, low bits first
                word = (ulong)(fs.ReadByte());
                word |= (ulong)(fs.ReadByte()) << 8;
                word |= (ulong)(fs.ReadByte()) << 16;
                word |= (ulong)(fs.ReadByte()) << 24;
                word |= (ulong)(fs.ReadByte()) << 32;
                word |= (ulong)(fs.ReadByte()) << 40;

                return word;
            }


            // Writable control store, 48-bit words
            private ulong[] _microcode;

            // Decoded microcode cache
            private Instruction[] _microcodeCache;

            // Wait state flag when CPU is writing to RAM
            private bool _wcsHold;

            // Microcode ROM, 48-bit words
            private ulong[] _rom;

            // ROM size: 512 (typical) out of 2K (max)
            private int _romSize;

            // ROM enable latch
            private bool _romEnabled;
        }
    }
}
