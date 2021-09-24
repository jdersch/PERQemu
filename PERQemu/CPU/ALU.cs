// alu.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.CompilerServices;

namespace PERQemu.Processor
{
    public partial class CPU
    {
        /// <summary>
        /// Represents the ALU's flags.  These are used internally by the
        /// emulator; they are reflected in the micromachine through the
        /// microstate register.
        /// </summary>
        public struct ALUFlags
        {
            public bool CarryH;    // C19 on 20-bit; C23 on 24-bit
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
                    String.Format("C{0}={1} Cry={2} Ovf={3}\n\tLeq={4} Lss={5} Geq={6} Gtr={7} Neq={8} Eql={9}",
                                  (_bits - 1), CarryH, Cry, Ovf, Leq, Lss, Geq, Gtr, Neq, Eql);
            }
        }

        /// <summary>
        /// This provides a high-level abstraction of the ALU operations performed
        /// by the 74S181 ALUs and condition PALs used by the PERQ.  The 74181 is
        /// a 4-bit ALU slice; the PERQ used 5 of these tied together to do 20 bit
        /// arithmetic, or 6 of them in the 24-bit CPU.
        ///
        /// It also used a special PAL for generating flags for various conditions
        /// (greater than, overflow, etc.) These are also emulated here.
        /// </summary>
        protected sealed class ALU
        {
            static ALU()
            {
                BuildFlagTable();
            }

            public ALU()
            {
                _r = new ExtendedRegister((_bits - 16), 16);
                _oldR = new ExtendedRegister((_bits - 16), 16);
            }

            public void Reset()
            {
                _r.Value = 0;
                _oldR.Value = 0;

                _flags = new ALUFlags();        // quick & dirty reset
                _oldFlags = _flags;

                Trace.Log(LogType.AluState, "ALU: Reset.");
            }

            public ExtendedRegister R
            {
                get { return _r; }
            }

            public ExtendedRegister OldR
            {
                get { return _oldR; }
            }

            public ALUFlags Flags
            {
                get { return _flags; }
            }

            public ALUFlags OldFlags
            {
                get { return _oldFlags; }
            }

            /// <summary>
            /// Normal two-input ALU operation: combine AMUX and BMUX inputs
            /// according to the ALU field in the microinstruction.
            /// </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public void Execute(ALUOperation op, int amux, int bmux)
            {
                Trace.Log(LogType.AluState, "ALU: In: Amux={0:x6} Bmux={1:x6}", amux, bmux);

                // Reset carry flag (arithmetic ops will set it as necessary)
                // but save the original value for use in addition/subtraction w/carry
                int lastCarry15 = _flags.Cry ? 1 : 0;

                _flags.CarryH = false;

                bool carry15 = false;
                bool arithX = false;
                bool arithY = false;

                switch (op)
                {
                    case ALUOperation.A:
                        _r.Value = amux;
                        break;

                    case ALUOperation.B:
                        _r.Value = bmux;
                        break;

                    case ALUOperation.NotA:
                        _r.Value = ~amux;
                        break;

                    case ALUOperation.NotB:
                        _r.Value = ~bmux;
                        break;

                    case ALUOperation.AandB:
                        _r.Value = amux & bmux;
                        break;

                    case ALUOperation.AandNotB:
                        _r.Value = amux & (~bmux);
                        break;

                    case ALUOperation.AnandB:
                        _r.Value = ~(amux & bmux);
                        break;

                    case ALUOperation.AorB:
                        _r.Value = amux | bmux;
                        break;

                    case ALUOperation.AorNotB:
                        _r.Value = amux | (~bmux);
                        break;

                    case ALUOperation.AnorB:
                        _r.Value = ~(amux | bmux);
                        break;

                    case ALUOperation.AxorB:
                        _r.Value = amux ^ bmux;
                        break;

                    case ALUOperation.AxnorB:
                        _r.Value = (amux & bmux) | ((~amux) & (~bmux));
                        break;

                    case ALUOperation.AplusB:
                        _r.Value = (amux + bmux);
                        arithY = true;

                        // Check for carry-out from bit 19 / 23
                        _flags.CarryH = ((amux + bmux) > _mask);

                        // Check for carry-out from bit 15
                        carry15 = ((amux & 0xffff) + (bmux & 0xffff) > 0xffff);
                        break;

                    case ALUOperation.AplusBplusCarry:
                        _r.Value = (amux + bmux + lastCarry15);
                        arithY = true;

                        // Check for carry-out from bit 19 / 23
                        _flags.CarryH = ((amux + bmux + lastCarry15) > _mask);

                        // Check for carry-out from bit 15
                        carry15 = ((amux & 0xffff) + (bmux & 0xffff) + lastCarry15 > 0xffff);
                        break;

                    case ALUOperation.AminusB:
                        _r.Value = (amux - bmux);
                        arithX = true;

                        _flags.CarryH = !((amux - bmux) < 0);

                        carry15 = ((amux & 0xffff) - (bmux & 0xffff) >= 0);
                        break;

                    case ALUOperation.AminusBminusCarry:
                        _r.Value = (amux - bmux - (~lastCarry15 & 0x1));
                        arithX = true;

                        _flags.CarryH = !((amux - bmux - (~lastCarry15 & 0x1)) < 0);

                        carry15 = ((amux & 0xffff) - (bmux & 0xffff) - (~lastCarry15 & 0x1) >= 0);
                        break;

                    default:
                        throw new UnimplementedInstructionException(String.Format("Unhandled ALU operation {0:x1}", op));
                }

                // Inputs to the condition code PAL, used to build an index into the PAL array.
                // NOTE: these are actually the CCSR0 bits that appear in the uState register?!
                bool r15 = (_r.Value & 0x8000) == 0;
                bool LAeqB = (_r.Value & 0xffff) != 0;      // 16-bit equality
                bool Lb15 = (bmux & 0x8000) != 0;
                bool La15 = (amux & 0x8000) != 0;

                // Set flags that don't require heavy lifting
                _flags.Cry = carry15;
                _flags.Neq = LAeqB;
                _flags.Eql = !LAeqB;

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
                _flags.Ovf = (flags & 0x1) != 0;
                _flags.Leq = (flags & 0x2) != 0;
                _flags.Lss = (flags & 0x4) != 0;
                _flags.Geq = (flags & 0x8) != 0;
                _flags.Gtr = (flags & 0x10) != 0;

                Trace.Log(LogType.AluState, "ALU: Result: {0} {1}", _r.Value, Flags);
            }

            /// <summary>
            /// Dark magick to modify the ALU op if necessary for a Multiply or Divide step.
            /// Modifies the Amux and ALU op inputs as needed and runs the ALU; later the
            /// special function select updates the MQ register appropriately.
            /// </summary>
            public void Execute(ALUOperation curOp, int amux, int bmux, MulDivCommand inst, int mq)
            {
                ALUOperation modOp = curOp;

                Trace.Log(LogType.MulDiv, "ALUop: IN  op={0} amux={1:x6} mq={2:x4}", curOp, amux, mq);

                if (curOp == ALUOperation.AplusB || curOp == ALUOperation.AminusB)
                {
                    switch (inst)
                    {
                        case MulDivCommand.Off:
                            // Should never happen!
                            throw new InvalidOperationException("DoMulDivALUOp called with MulDivInst=OFF!?");

                        case MulDivCommand.UnsignedDivide:
                            //
                            // For a divide step, shift the MSB of the quotient
                            // (MQ<15> from the last cycle) into the LSB of the
                            // remainder (SHIFT<0> here, already shifted by amux
                            // select).  Later DispatchFunction() will shift the
                            // quotient in MQ and apply the computed Q0 bit.
                            //
                            int bit = (mq & 0x8000) >> 15;
                            amux = ((amux & (_mask - 1)) | bit);

                            //
                            // Next, examine the sign bit from the PREVIOUS cycle's
                            // result (R<15>) to determine if the current op should
                            // be an addition or subtraction (of the remainder).  Only
                            // do this if the current op is add/sub (first use of
                            // DivideStep is a shift).
                            //
                            if ((_oldR.Value & 0x8000) != 0)    // sign of previous R + or -?
                            {
                                modOp = ALUOperation.AplusB;
                            }
                            else
                            {
                                modOp = ALUOperation.AminusB;
                            }
                            break;

                        case MulDivCommand.UnsignedMultiply:
                        case MulDivCommand.SignedMultiply:
                            //
                            // For a multiply, check the LSB of the multiplier (current
                            // MQ register); if set, do an add, else pass thru unmolested.
                            // This happens before the MQ is shifted in DispatchFunction().
                            //
                            if ((mq & 0x1) == 1)
                            {
                                modOp = ALUOperation.AplusB;
                            }
                            else
                            {
                                modOp = ALUOperation.A;
                            }
                            break;
                    }
                }

                Trace.Log(LogType.MulDiv, "ALUop: OUT op={0} amux={1:x6}", modOp, amux);

                Execute(modOp, amux, bmux);
            }

            /// <summary>
            /// Sets the R register, since the compiler barfs at us if we try
            /// to expose the ExtendedRegister through a setter.  Bleah.
            /// </summary>
            public void SetResult(int r)
            {
                _r.Value = r;       // this populates .Lo and .Hi
            }

            /// <summary>
            /// Latch the result and flags from the previous cycle.
            /// </summary>
            public void LatchResult()
            {
                _oldR = _r;
                _oldFlags = _flags;
            }

            /// <summary>
            /// Build a table of ALU status flags based on the ALU PAL equations.
            /// </summary>
            private static void BuildFlagTable()
            {
                _palFlags = new int[64];

                // TODO: figure out how to incorporate the MulDiv/16K flags
                //  to clean up the crazy MulDiv mess?

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

            private ExtendedRegister _r;
            private ExtendedRegister _oldR;
            private ALUFlags _flags;
            private ALUFlags _oldFlags;

            private static int[] _palFlags;
        }
    }
}