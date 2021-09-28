// instruction.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Memory;

namespace PERQemu.Processor
{
    public partial class CPU
    {
        /// <summary>
        /// Represents a single PERQ microcode instruction and its component fields.
        /// This will pre-compute as much data (jump addresses, etc) as can be done.
        /// </summary>
        public sealed class Instruction
        {
            /// <summary>
            /// Builds a new Instruction object given a 48-bit microinstruction word.
            /// </summary>
            public Instruction(ulong uCode, ushort pc)
            {
                PC = pc;
                UCode = uCode;

                X = (byte)((uCode & 0xff0000000000) >> 40);
                Y = (byte)((uCode & 0x00ff00000000) >> 32);
                A = (AField)((uCode & 0x0000e0000000) >> 29);
                B = (byte)((uCode & 0x000010000000) >> 28);
                W = (byte)((uCode & 0x000008000000) >> 27);
                H = (byte)((uCode & 0x000004000000) >> 26);
                ALU = (ALUOperation)((uCode & 0x000003c00000) >> 22);
                F = (byte)((uCode & 0x000000300000) >> 20);
                SF = (byte)((uCode & 0x0000000f0000) >> 16);
                Z = (byte)((uCode & 0x00000000ff00) >> 8);
                CND = (Condition)((uCode & 0x0000000000f0) >> 4);
                JMP = (JumpOperation)((uCode & 0x00000000000f));

                ComputeCachedData();
            }

            public override string ToString()
            {
                return String.Format("uCode={0:x12} X={1:x1} Y={2:x1} A={3} B={4:x1} W={5:x1} H={6:x1} ALU={7} F={8:x1} SF={9:x1} Z={10:x1} CND={11} JMP={12}",
                                     UCode, X, Y, A, B, W, H, ALU, F, SF, Z, CND, JMP);
            }

            public static int ZOpFill(int z)
            {
                return (z & 0x3) | ((z & 0xc0) << 4);
            }

            /// <summary>
            /// Compute static data associated with this opcode that doesn't need to
            /// be computed on every execution.
            /// </summary>
            private void ComputeCachedData()
            {
                // Long constant
                LongConstant = (Z << 8) | (Y);

                // Inverted Z used in several places...
                NotZ = (~Z & 0xff);

                // ...like the IO address!
                IsIOInput = ((Z & 0x80) == 0);                  // MSB is the R/W flag
                IOPort = (byte)((Z & 0x80) | (NotZ & 0x7f));    // Low 7 bits (inverted!) are the port address

                // Base of dispatch address
                VectorDispatchAddress = (ushort)(ZOpFill(NotZ) | ((NotZ & 0x3c) << 4));

                // Next address
                switch (F)
                {
                    case 0:     // Short jump
                        NextAddress = (ushort)((PC & 0xf00) | NotZ);
                        break;

                    case 1:     // Short jump -- or Leap jump on 16K
                        NextAddress = (ushort)((PC & 0xf00) | NotZ);

                        if (SF == 0x7)
                        {
                            NextAddress = (ushort)((NotZ | ((0xff & (~Y)) << 8)) & _wcsMask);

                            if (Is4K)
                            {
                                Trace.Log(LogType.Warnings,
                                        "Leap not implemented on the 4K CPU.  Jumped to {0:x4} instead, not {1:x4}",
                                        NextAddress, (ushort)((NotZ) | ((0xff & (~Y)) << 8)) & _wcsMask);
                            }
                        }
                        break;

                    case 3:     // Long jump
                        NextAddress = (ushort)((0xfff & (~((Z | (SF << 8))))));
                        break;

                    default:
                        break;
                }

                IsSpecialFunction = (F == 0 || F == 2);

                // BMux input (for B != 0)
                // Select a constant.  If SF special function 0 (LongConstant) this is
                // 8 bits from Z, and 8 bits from Y.  Otherwise just 8 bits from Y.
                if (IsSpecialFunction && SF == 0)
                {
                    BMuxInput = LongConstant;
                }
                else
                {
                    BMuxInput = Y;
                }

                // Flag if we're wanting to access MDI or MDX this cycle
                WantMDI = (A == AField.MDI || A == AField.MDX);

                // Set memory request type if this instruction is a Fetch/Store
                if (F == 1 && SF >= 0x8)
                {
                    MemoryRequest = (MemoryCycle)SF;
                }
                else
                {
                    MemoryRequest = MemoryCycle.None;
                }
            }

            // Instruction fields
            public byte X;              // 8 bits
            public byte Y;              // 8 bits
            public AField A;            // 3 bits
            public byte B;              // 1 bit
            public byte W;              // 1 bit
            public byte H;              // 1 bit
            public ALUOperation ALU;    // 4 bits
            public byte F;              // 2 bits
            public byte SF;             // 4 bits
            public byte Z;              // 8 bits
            public Condition CND;       // 4 bits
            public JumpOperation JMP;   // 4 bits

            // Computed fields
            public int NotZ;
            public int BMuxInput;
            public int LongConstant;
            public ushort NextAddress;
            public ushort VectorDispatchAddress;
            public byte IOPort;
            public bool WantMDI;
            public bool IsIOInput;
            public bool IsSpecialFunction;
            public MemoryCycle MemoryRequest;

            // Original instruction
            public ulong UCode;         // 48 bits
            public ushort PC;
        }
    }

    public enum AField
    {
        Shifter = 0x0,
        NextOp = 0x1,
        IOD = 0x2,
        MDI = 0x3,
        MDX = 0x4,
        UState = 0x5,
        XYRegister = 0x6,
        TOS = 0x7
    }

    public enum ALUOperation
    {
        A = 0x0,
        B = 0x1,
        NotA = 0x2,
        NotB = 0x3,
        AandB = 0x4,
        AandNotB = 0x5,
        AnandB = 0x6,
        AorB = 0x7,
        AorNotB = 0x8,
        AnorB = 0x9,
        AxorB = 0xa,
        AxnorB = 0xb,
        AplusB = 0xc,
        AplusBplusCarry = 0xd,
        AminusB = 0xe,
        AminusBminusCarry = 0xf
    }

    public enum Condition
    {
        True = 0x0,
        False = 0x1,
        IntrPend = 0x2,
        Spare = 0x3,
        BPC3 = 0x4,
        CarryH = 0x5,   // Carry19 or Carry23
        Odd = 0x6,
        ByteSign = 0x7,
        Neq = 0x8,
        Leq = 0x9,
        Lss = 0xa,
        Ovf = 0xb,
        Carry15 = 0xc,
        Eql = 0xd,
        Gtr = 0xe,
        Geq = 0xf
    }

    public enum JumpOperation
    {
        JumpZero = 0x0,
        Call = 0x1,
        NextInstReviveVictim = 0x2,
        Goto = 0x3,
        PushLoad = 0x4,
        CallS = 0x5,
        VectorDispatch = 0x6,
        GotoS = 0x7,
        RepeatLoop = 0x8,
        Repeat = 0x9,
        Return = 0xa,
        JumpPop = 0xb,
        LoadS = 0xc,
        Loop = 0xd,
        Next = 0xe,
        ThreeWayBranch = 0xf
    }

    public class UnimplementedInstructionException : Exception
    {
        public UnimplementedInstructionException(string message)
            : base(message)
        {

        }
    }
}

