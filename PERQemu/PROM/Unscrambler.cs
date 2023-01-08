//
// Unscrambler.cs - Copyright (c) 2022-2023 S. Boondoggle (skeezicsb@gmail.com)
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
using System.IO;
using System.Text;
using System.Text.RegularExpressions;

using PERQemu.Debugger;
using PERQemu.Processor;

namespace PERQemu
{
    /// <summary>
    /// Reassemble a boot PROM image from text dumps, rearranging the bits in
    /// the way LoadROM expects.  Thar be dragons here.
    /// </summary>
    /// <remarks>
    /// For now this is just to build boot PROMs, but it could potentially be
    /// extended to replace the silly Perl script hacks used to rebuild other
    /// synthesized ROMs, though this is typically one-time, "fire and forget".
    /// </remarks>
    public sealed class Unscrambler : CPU
    {
        static Unscrambler()
        {
            _name = "Generic CPU";
            _desc = "Fake CPU for building boot PROMs";
            _cycleTime = 170;

            _bits = 20;
            _mask = 0x0fffff;

            _wcsBits = 10;
            _wcsSize = 1024;
            _wcsMask = 0x03ff;
        }

        public Unscrambler()
        {
            _wcs = new ControlStore();
            _wcs.DisableROM();
        }

        /// <summary>
        /// Read six raw dumps in ASCII/hex, create one binary output.
        /// </summary>
        /// <remarks>
        /// There are too many unknowns, mysteries or outright errors in the
        /// process.  See the notes below for the gruesome details.
        /// </remarks>
        public void MakeBootPROM(string basename, string output, bool fourk)
        {
            ulong addr;
            ulong value;
            string line = string.Empty;

            var pattern = @"([a-f\d]{3}) ([a-f\d]{2})";
            var regex = new Regex(pattern, RegexOptions.Compiled | RegexOptions.IgnoreCase);

            //
            // Expects to find six files of the form "<basename>[A-F].txt" in the
            // PROM dir.  Creates "PROM/<output>.rom" if successful.
            //
            foreach (var part in new char[] { 'A', 'B', 'C', 'D', 'E', 'F' })
            {
                var file = $"{basename}{part}.txt";

                using (var fs = new FileStream(Paths.BuildPROMPath(file), FileMode.Open))
                {
                    Log.Write("Reading from {0}...", file);

                    while ((line = ReadString(fs)) != string.Empty)
                    {
                        var match = regex.Match(line);

                        if (match.Success && match.Groups.Count == 3)
                        {
                            addr = Convert.ToUInt16(match.Groups[1].Value, 16);
                            value = Convert.ToByte(match.Groups[2].Value, 16);
                        }
                        else
                        {
                            Log.Write("Bad input reading {0}: '{1}' (ignored)", file, line);
                            continue;
                        }

                        if (fourk)
                        {
                            // Shuffle the address bits - 4K CPU pinout
                            addr = _wcs.SetBits(addr, 0, new int[] { 4, 1, 0, 8, 2, 6, 7, 3, 5 });
                        }
                        else
                        {
                            // Shuffle the address bits - 16K CPU pinout
                            addr = _wcs.SetBits(addr, 0, new int[] { 5, 0, 3, 8, 2, 6, 4, 1, 7 });
                        }

                        // Get the current value
                        ulong data = _wcs.Microcode[addr];

                        // Update the instruction bits from the incoming byte
                        switch (part)
                        {
                            case 'A':
                                if (data != 0) Log.Warn(Category.All, "Possible address overwrite at 0x{0:x3}", addr);
                                data = _wcs.SetBits(value, data, new int[] { 6, 7, 5, 4, 3, 2, 1, 0 });
                                break;

                            case 'B':
                                data = _wcs.SetBits(value, data, new int[] { 12, 13, 15, 14, 11, 10, 9, 8 });
                                break;

                            case 'C':
                                data = _wcs.SetBits(value, data, new int[] { 20, 17, 16, 19, 18, 21, 26, 28 });
                                break;

                            case 'D':
                                data = _wcs.SetBits(value, data, new int[] { 25, 24, 27, 29, 23, 22, 31, 30 });
                                break;

                            case 'E':
                                data = _wcs.SetBits(value, data, new int[] { 37, 39, 36, 38, 32, 34, 33, 35 });
                                break;

                            case 'F':
                                data = _wcs.SetBits(value, data, new int[] { 46, 44, 47, 45, 43, 41, 42, 40 });
                                break;
                        }

                        // Write it back!
                        _wcs.Microcode[addr] = data;
                    }

                    fs.Close();
                }
            }


            // Now we've read 'em all in and constructed our raw instructions;
            // write them to the output file as a binary!
            using (var fs = new FileStream(Paths.BuildPROMPath($"{output}.rom"), FileMode.Create))
            {
                Log.Write("Writing to {0}.rom...", output);

                for (var i = 0; i < 512; i++)
                {
                    // Fetch in reverse order (address lines are inverted)
                    var inst = _wcs.Microcode[511 - i];

                    // Write out the address, low byte first
                    fs.WriteByte((byte)(i & 0xff));
                    fs.WriteByte((byte)((i >> 8) & 0xff));

                    // Write out the 48-bit instruction, low to high
                    for (var j = 0; j < 6; j++)
                    {
                        fs.WriteByte((byte)(inst & 0xff));
                        inst >>= 8;
                    }
                }
                fs.Close();
            }

            Log.Write("Done!");
        }

        /// <summary>
        /// Read a string; returns "" if at end of file.  Stops on newline,
        /// and returns the string with any CRLFs removed.
        /// </summary>
        private string ReadString(FileStream fs)
        {
            var sb = new StringBuilder();

            while (fs.Position < fs.Length)
            {
                var ch = (char)fs.ReadByte();

                if (ch == '\n') break;
                if (ch != '\r') sb.Append(ch);
            }

            return sb.ToString();
        }

        /// <summary>
        /// Load a ROM file, like the one we just wrote... see if it worked!
        /// </summary>
        public void Reload(string file)
        {
            _wcs.Reset();
            _wcs.LoadROM(Paths.BuildPROMPath($"{file}.rom"));
        }

        /// <summary>
        /// Shows the disassembly of the code we just built.  We hope.
        /// </summary>
        public void ShowDisassembly()
        {
            for (ushort i = 0; i < 512; i++)
            {
                var line = Disassembler.Disassemble(i, _wcs.GetInstruction(i));
                Console.WriteLine(line);
            }
        }

        // Just create our own private (temporary) WCS!
        private ControlStore _wcs;
    }
}


/*

    Unscrambler leverages the Instruction, Disassembler and ControlStore classes
    to reassemble text dumps from actual PERQ PROMs.  Making sense of the bits
    from a raw PROM dump is "challenging" to say the least.  The documentation,
    source code, and schematics are always out of sync, contradictory, missing,
    or just flat out wrong.  Par for the course.

    The ICL T2 Service Guide has one page that lists some helpful information
    regarding boot PROMs:

    T1, 4K - "MD"    T1, 16K - "MC"     T2, 16K - "TC"

    It also notes that "SC" and "SD" are for PERQ-1 systems but doesn't provide
    any specifics about what they mean; the best guess is that the xC means the
    address pins use the 16K wiring from the 16K CPU schematics, while the xD
    means the 4K CPU scheme.  I have available all three PERQ-1 variants:

        "PB" - 16K CPU, machine with IOB/Z80 v8.7
        "SC" - 16K CPU, machine with CIO/Z80 v10.17
        "SD" - 4K CPU, machine with CIO/Z80 v10.17

    and the SD PROMs do appear to use the 4K scheme.  However, the SC rip has
    some weird issues (or I've screwed up the wiring again).  Haven't yet done
    a pull from the PB PROMs, but I should try to verify that ONE of my PERQs
    is actually running boot code that matches the default "boot.bin" that's
    been bundled with PERQemu since the beginning...

	I _should_ figure out how to compute/compare the checksums, to see if any
	of these actually match the scant documentation available.
	

    Boot PROM Address Decoding
    --------------------------

    The PERQ writes the address inverted:
        BootData[511 - AddrScramble.Word] := BinFile^.MI

     8  7  6  5  4  3  2  1  0  <-- bit
    19 18 17 16  5  4  3  2  1  <-- address pin
    
     5  3  7  6  2  8  0  1  4  --> 4K "CPU-D" hardware mapping    PB
     7  1  4  6  2  8  3  0  5  --> 16K CPU rev D hardware mapping PB/TC

     0  1  2  3  4  5  6  7  8  --> remap
     2 18  5  3 17  1 16 19  4  --> pins in
     1  7  4  2  6  0  5  8  3  --> permute

    The PERQ-1 schematics from 1981 call the original 4K processor "CPU-D", while
    the PERQ-T2 schematics book from 1985 ALSO calls the 16K CPU rev "D".  Of
    course it does.  Comments in "boot_proms.pas" (V1.8) doesn't make a clear
    distinction about what particular board they're talking about.

    On the 4K the ROM type is HM7649 (a Harris 512 x 8 part);
    On the 16K CPU the chip type is Am27S29, the kind captured here.


    Boot PROM Data Decoding
    -----------------------
    
                             7  6  5  4  3  2  1  0 <-- data bit
                            14 13 12 11  9  8  7  6 <-- data pins

	xxF is (X)              40 42 41 43 45 47 44 46  --> 4K, 16K hardware mapping
	xxE is (Y)              35 33 34 32 38 36 39 37  --> same for all six proms!
    xxD is (A, W, ALU)      30 31 22 23 29 27 24 25
    xxC is (F, SF, B, H)    28 26 21 18 19 16 17 20  --> This would map U<> to
    xxB is (Z)               8  9 10 11 14 15 13 12  --> where it needs to go in
    xxA is (JMP, CND)        0  1  2  3  4  5  7  6  --> the uinst word directly!

    Data lines for the 4K CPU-D (PERQ-1) and the 16K CPU rev D (T2 schematics)
    are laid out identically.  Seems only the address pins are different.

        Translated:             Unscrambled:
         1: (ALU23: 0..3;       1: (Jmp: 0..15;
             ALU0:  0..1;           Cnd: 0..15;
             W:     0..1;           Z:   0..255;
             ALU1:  0..1;           SF:  0..15;
             A:     0..7;           F:   0..3;
             Z:     0..255;         ALU: 0..15;
             SFF:   0..63;          H:   0..1;
             H:     0..1;           W:   0..1;
             B:     0..1;           B:   0..1;
             JmpCnd:0..255)         A:   0..7;
                                    Y:   0..255;
                                    X:   0..255)

    So the sequence of permutations from ROM pins directly to the translated
    format (which is what UnscrambleControlStoreWord() expects:
    
        xxA:    30, 31, 29, 28, 27, 26, 25, 24
        xxB:    12, 13, 15, 14, 11, 10, 9, 8
        xxC:    20, 17, 16, 19, 18, 21, 22, 23
        xxD:    1, 0, 3, 5, 4, 2, 6, 7
        xxE:    37, 39, 36, 38, 32, 34, 33, 35
        xxF:    46, 44, 47, 45, 43, 41, 42, 40

    Except that the ROM loader does NOT call UnscrambleControlStoreWord().  So
    the ROM format is basically the straight MI format but with some fields and
    pins rearranged to accommodate the physical wiring on the CPU board;  the
    unscramble method is only called when the microcode uses WCSlo/mid/hi to
    load a word from memory.  So that was a day of pointless head scratching.

    Oh, and it turns out you should probably NOT reverse four wires from the
    ZIF socket to the data bus pins when reading the chips.  <facepalm>

 */
