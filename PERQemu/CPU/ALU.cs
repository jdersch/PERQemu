// alu.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using System;
using System.Collections.Generic;
using System.Text;

namespace PERQemu.CPU
{
    /// <summary>
    /// Represents the ALU's register file.
    /// </summary>
    [Serializable]
    public struct ALURegisterFile
    {
        public int  R;
        public int  Carry19;
        public bool Ovf;
        public bool Cry;
        public bool Leq;
        public bool Lss;
        public bool Geq;
        public bool Gtr;
        public bool Neq;
        public bool Eql;

        public override string ToString()
        {
            return
                String.Format("R={0:x5} C19={1} C15={2} Ovf={3}\n\tLeq={4} Lss={5} Geq={6} Gtr={7} Neq={8} Eql={9}",
                               R, Carry19, Cry, Ovf, Leq, Lss, Geq, Gtr, Neq, Eql);
        }
    }

    /// <summary>
    /// This provides a high-level abstraction of the ALU operations performed by the
    /// 74S181 ALUs and condition PALs used by the PERQ.  The 74181 is a 4-bit ALU; the
    /// PERQ used 5 of these tied together to do arithmetic on 20-bit quantities.
    ///
    /// It also used a special PAL for generating flags for various conditions (greater
    /// than, overflow, etc.) These are also emulated here.
    ///
    /// </summary>
    [Serializable]
    public sealed class ALU
    {
        public ALU()
        {
        }

        static ALU()
        {
            BuildFlagTable();
        }

        public void Reset()
        {
            _registers = new ALURegisterFile();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.AluState, "ALU: Reset.");
#endif
        }

        public void DoALUOp(int amux, int bmux, ALUOperation op)
        {
#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.AluState, "ALU: Inputs: Amux={0:x5} Bmux={1:x5}", amux, bmux);
#endif

            // Reset carry flag (arithmetic ops will set it as necessary)
            // but save the original value for use in addition/subtraction w/carry
            int lastCarry15 = _registers.Cry ? 1 : 0;
            _registers.Carry19 = 0;

            bool carry15 = false;
            bool arithX = false;
            bool arithY = false;

            switch (op)
            {
                case ALUOperation.A:
                    _registers.R = amux;
                    break;

                case ALUOperation.B:
                    _registers.R = bmux;
                    break;

                case ALUOperation.NotA:
                    _registers.R = ~amux;
                    break;

                case ALUOperation.NotB:
                    _registers.R = ~bmux;
                    break;

                case ALUOperation.AandB:
                    _registers.R = amux & bmux;
                    break;

                case ALUOperation.AandNotB:
                    _registers.R = amux & (~bmux);
                    break;

                case ALUOperation.AnandB:
                    _registers.R = ~(amux & bmux);
                    break;

                case ALUOperation.AorB:
                    _registers.R = amux | bmux;
                    break;

                case ALUOperation.AorNotB:
                    _registers.R = amux | (~bmux);
                    break;

                case ALUOperation.AnorB:
                    _registers.R = ~(amux | bmux);
                    break;

                case ALUOperation.AxorB:
                    _registers.R = amux ^ bmux;
                    break;

                case ALUOperation.AxnorB:
                    _registers.R = (amux & bmux) | ((~amux) & (~bmux));
                    break;

                case ALUOperation.AplusB:
                    _registers.R = (amux + bmux);
                    arithY = true;

                    // Check for carry-out from bit 19
                    _registers.Carry19 = _registers.R > 0xfffff ? 1 : 0;

                    // Check for carry-out from bit 15
                    carry15 = ((amux & 0xffff) + (bmux & 0xffff) > 0xffff);
                    break;

                case ALUOperation.AplusBplusCarry:
                    _registers.R = (amux + bmux + lastCarry15);
                    arithY = true;

                    // Check for carry-out from bit 19
                    _registers.Carry19 = _registers.R > 0xfffff ? 1 : 0;

                    // Check for carry-out from bit 15
                    carry15 = ((amux & 0xffff) + (bmux & 0xffff) + lastCarry15 > 0xffff);
                    break;

                case ALUOperation.AminusB:
                    _registers.R = (amux - bmux);
                    arithX = true;

                    _registers.Carry19 = ((amux - bmux) < 0) ? 0 : 1;

                    carry15 = ((amux & 0xffff) - (bmux & 0xffff) >= 0);
                    break;

                case ALUOperation.AminusBminusCarry:
                    _registers.R = (amux - bmux - (~lastCarry15 & 0x1));
                    arithX = true;

                    _registers.Carry19 = ((amux - bmux - (~lastCarry15 & 0x1)) < 0) ? 0 : 1;

                    carry15 = ((amux & 0xffff) - (bmux & 0xffff) - (~lastCarry15 & 0x1) >= 0);
                    break;

                default:
                    throw new UnimplementedInstructionException(String.Format("Unhandled ALU operation {0:x1}", op));
            }

            // Clip the result to 20 bits
            _registers.R = _registers.R & 0xfffff;

            // Inputs to the condition code PAL, used to build an index into the PAL array.
            bool r15 = (_registers.R & 0x8000) == 0;
            bool LAeqB = (_registers.R & 0xffff) != 0;
            bool Lb15 = (bmux & 0x8000) != 0;
            bool La15 = (amux & 0x8000) != 0;

            // Set flags that don't require heavy lifting
            _registers.Cry = carry15;
            _registers.Neq = LAeqB;
            _registers.Eql = !LAeqB;

            // Look up precomputed ALU PAL results
            int index = (r15 ? 0x1 : 0) |
                (La15 ? 0x2 : 0) |
                (Lb15 ? 0x4 : 0) |
                (LAeqB ? 0x8 : 0) |
                (arithX ? 0x10 : 0) |
                (arithY ? 0x20 : 0);

            // TODO: could make this faster by having an unsafe array
            int flags = _palFlags[index];

            // Set flags based on the returned table value
            _registers.Ovf = (flags & 0x1) != 0;
            _registers.Leq = (flags & 0x2) != 0;
            _registers.Lss = (flags & 0x4) != 0;
            _registers.Geq = (flags & 0x8) != 0;
            _registers.Gtr = (flags & 0x10) != 0;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.AluState, "ALU: Result: {0}", Registers);
#endif
        }

        /// <summary>
        /// Hacky entrypoint so MQ can set R...
        /// </summary>
        public void SetR(int r)
        {
            _registers.R = r;
        }

        public ALURegisterFile Registers
        {
            get { return _registers; }
        }

        /// <summary>
        /// Build a table of ALU status flags based on the ALU PAL equations.
        /// </summary>
        private static void BuildFlagTable()
        {
            _palFlags = new int[64];

            // index bits:
            // 0 r15
            // 1 la15
            // 2 lb15
            // 3 lAeqB
            // 4 arithX
            // 5 arithY

            for (int r15 = 0; r15 < 2; r15++)
            {
                for (int la15 = 0; la15 < 2; la15++)
                {
                    for (int lb15 = 0; lb15 < 2; lb15++)
                    {
                        for (int lAeqB = 0; lAeqB < 2; lAeqB++)
                        {
                            for (int arithX = 0; arithX < 2; arithX++)
                            {
                                for (int arithY = 0; arithY < 2; arithY++)
                                {
                                    int index = r15 | (la15 << 1) | (lb15 << 2) | (lAeqB << 3) | (arithX << 4) | (arithY << 5);

                                    // this is ugly
                                    bool bR15 = r15 == 0 ? false : true;
                                    bool bLa15 = la15 == 0 ? false : true;
                                    bool bLb15 = lb15 == 0 ? false : true;
                                    bool bLAeqB = lAeqB == 0 ? false : true;
                                    bool bArithX = arithX == 0 ? false : true;
                                    bool bArithY = arithY == 0 ? false : true;

                                    // Set flags based on the PAL equations
                                    bool ovf =
                                        !((bR15 & !bLa15) |
                                        //(arithX & arithY & r15) |
                                        (!bArithX & bR15 & !bLb15) |
                                        (!bArithY & bR15 & bLb15) |
                                        (!bR15 & bLa15) |
                                        (bArithY & !bR15 & bLb15) |
                                        (!bArithX & !bArithY & !bR15) |
                                        (bArithX & !bR15 & !bLb15));

                                    bool leq =
                                        !((bArithX & !bArithY & !bR15 & bLAeqB & bLb15 & !bLa15) |
                                        (!bArithX & bArithY & !bR15 & bLAeqB & !bLb15 & !bLa15) |
                                        (bR15 & bLAeqB & !bLa15) |
                                        //(arithX & arithY & r15 & LAeqB) |
                                        (!bArithX & bR15 & bLAeqB & !bLb15) |
                                        (!bArithY & bR15 & bLAeqB & bLb15));

                                    bool lss =
                                        !((bArithX & !bArithY & !bR15 & bLb15 & !bLa15) |
                                        (!bArithX & bArithY & !bR15 & !bLb15 & !bLa15) |
                                        (bR15 & !bLa15) |
                                        //(arithX & arithY & r15) |
                                        (!bArithX & bR15 & !bLb15) |
                                        (!bArithY & bR15 & bLb15));

                                    bool geq =
                                        !((bArithX & !bArithY & bR15 & !bLb15 & bLa15) |
                                        (!bArithX & bArithY & bR15 & bLb15 & bLa15) |
                                        (!bR15 & bLa15) |
                                        (!bArithX & !bArithY & !bR15) |
                                        (bArithX & !bR15 & !bLb15) |
                                        (bArithY & !bR15 & bLb15));

                                    bool gtr =
                                        !((!bLAeqB) |
                                        (bArithX & !bArithY & bR15 & !bLb15 & bLa15) |
                                        (!bArithX & bArithY & bR15 & bLb15 & bLa15) |
                                        (!bR15 & bLa15) |
                                        (!bArithX & !bArithY & !bR15) |
                                        (bArithX & !bR15 & !bLb15) |
                                        (bArithY & !bR15 & bLb15));

                                    int value = (ovf ? 0x1 : 0) |
                                        (leq ? 0x2 : 0) |
                                        (lss ? 0x4 : 0) |
                                        (geq ? 0x8 : 0) |
                                        (gtr ? 0x10 : 0);

                                    _palFlags[index] = value;
                                }
                            }
                        }
                    }
                }
            }
        }

        private static int[] _palFlags;

        private ALURegisterFile _registers;
    }
}

