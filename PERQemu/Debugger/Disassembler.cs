// disassembler.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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
using System.Text;
using PERQemu.CPU;

namespace PERQemu.Debugger
{
    /// <summary>
    /// Disassembler contains routines for disassembling a microcode instruction.
    /// In many ways it is similar to the CPU class, except it's free of the burden
    /// of actually executing the code.
    /// </summary>
    public static class Disassembler
    {
        /// <summary>
        /// Returns a human-readable disassembly of a given microcode instruction.
        /// </summary>
        /// <param name="op"></param>
        /// <returns></returns>
        public static string Disassemble(ushort address, Instruction op)
        {
            _pc = address;

            StringBuilder sb = new StringBuilder();

            // Append the address and raw opcode
            sb.AppendFormat("{0:x4}: {1:x12}\t", address, op.UCode);

            string amux = DisassembleAmuxInput(op);
            string bmux = DisassembleBmuxInput(op);

            // Append ALU op
            if (op.W == 0)
            {
                // No writeback
                sb.AppendFormat("{0}. ", DisassembleALUOp(amux, bmux, op));
            }
            else
            {
                // Writeback to X reg
                if (op.X < 0x40)
                {
                    sb.AppendFormat("R{0:x2}% := {1}. ", op.X, DisassembleALUOp(amux, bmux, op));
                }
                else
                {
                    sb.AppendFormat("R{0:x2} := {1}. ", op.X, DisassembleALUOp(amux, bmux, op));
                }
            }

            // Append function, if any.
            string func = DisassembleFunction(op);
            if (func.Length > 0)
            {
                sb.AppendFormat("{0}. ", DisassembleFunction(op));
            }

            // Append jump.
            sb.AppendFormat("{0}.", DisassembleJump(op));

            return sb.ToString();
        }

        /// <summary>
        /// Show Amux field.
        /// </summary>
        /// <param name="uOp"></param>
        /// <returns></returns>
        private static string DisassembleAmuxInput(Instruction uOp)
        {
            string amux = "<invalid>";

            switch (uOp.A)
            {
                case AField.Shifter:    // Shifter output
                    amux = "Shifter";
                    break;

                case AField.NextOp:
                    amux = "Next Opcode";
                    break;

                case AField.IOD:        // Input port
                    amux = "IOD";
                    break;

                case AField.MDI:        // MDI
                    amux = "MDI";
                    break;

                case AField.MDX:        // Upper bits of MDI
                    amux = "MDX";
                    break;

                case AField.UState:     // Microstate Register
                    amux = "UState";
                    break;

                case AField.XYRegister: // XY register at location specified by X field
                    if (uOp.X < 0x40)
                    {
                        amux = String.Format("R{0:x2}%", uOp.X);
                    }
                    else
                    {
                        amux = String.Format("R{0:x2}", uOp.X);
                    }
                    break;

                case AField.TOS:        // Expression stack
                    amux = "TOS";
                    break;
            }

            return amux;
        }

        /// <summary>
        /// Decode Bmux field.
        /// </summary>
        /// <param name="uOp"></param>
        /// <returns></returns>
        private static string DisassembleBmuxInput(Instruction uOp)
        {
            string bmux = "<invalid>";

            // Select BMUX input
            if (uOp.B == 0)
            {
                if (uOp.Y < 0x40)
                {
                    bmux = String.Format("R{0:x2}%", uOp.Y);
                }
                else
                {
                    bmux = String.Format("R{0:x2}", uOp.Y);
                }
            }
            else
            {
                // Select a constant.  If SF special function 0 (LongConstant) this is
                // 8 bits from Z, and 8 bits from Y.  Otherwise just 8 bits from Y.
                if (IsSpecialFunction(uOp) && uOp.SF == 0)
                {
                    bmux = String.Format("{0:x4}", (uOp.Z << 8) | (uOp.Y));
                }
                else
                {
                    bmux = String.Format("{0:x2}", uOp.Y);
                }
            }

            return bmux;
        }

        private static bool IsSpecialFunction(Instruction uOp)
        {
            return (uOp.F == 0 || uOp.F == 2);
        }

        private static string DisassembleALUOp(string amux, string bmux, Instruction op)
        {
            string alu = "<invalid>";

            switch (op.ALU)
            {
                case ALUOperation.A:
                    alu = amux;
                    break;

                case ALUOperation.B:
                    alu = bmux;
                    break;

                case ALUOperation.NotA:
                    alu = String.Format("NOT {0}", amux);
                    break;

                case ALUOperation.NotB:
                    alu = String.Format("NOT {0}", bmux);
                    break;

                case ALUOperation.AandB:
                    alu = String.Format("{0} AND {1}", amux, bmux);
                    break;

                case ALUOperation.AandNotB:
                    alu = String.Format("{0} AND NOT {1}", amux, bmux);
                    break;

                case ALUOperation.AnandB:
                    alu = String.Format("{0} NAND {1}", amux, bmux);
                    break;

                case ALUOperation.AorB:         // a or b
                    alu = String.Format("{0} OR {1}", amux, bmux);
                    break;

                case ALUOperation.AorNotB:      // a or not b
                    alu = String.Format("{0} OR NOT {1}", amux, bmux);
                    break;

                case ALUOperation.AnorB:        // a nor b
                    alu = String.Format("{0} NOR {1}", amux, bmux);
                    break;

                case ALUOperation.AxorB:        // a xor b
                    alu = String.Format("{0} XOR {1}", amux, bmux);
                    break;

                case ALUOperation.AxnorB:       // a xnor b
                    alu = String.Format("{0} XNOR {1}", amux, bmux);
                    break;

                case ALUOperation.AplusB:       // a+b
                    alu = String.Format("{0} + {1}", amux, bmux);
                    break;

                case ALUOperation.AplusBplusCarry:  // a+b+oldcarry
                    alu = String.Format("{0} + {1} + Cry", amux, bmux);
                    break;

                case ALUOperation.AminusB:      // a-b
                    alu = String.Format("{0} - {1}", amux, bmux);
                    break;

                case ALUOperation.AminusBminusCarry: // a-b-carry
                    alu = String.Format("{0} - {1} - Cry", amux, bmux);
                    break;
            }

            return alu;
        }

        /// <summary>
        /// Decode Function and Special Function fields.
        /// </summary>
        /// <param name="uOp"></param>
        private static string DisassembleFunction(Instruction uOp)
        {
            string function = "";

            switch (uOp.F)
            {
                case 0x0:
                case 0x2:       // Special Functions

                    // Calculate shifter output when F=2
                    if (uOp.F == 0x2)
                    {
                        function = DisassembleShifterCommand(uOp.Z) + ", ";
                    }

                    switch (uOp.SF)
                    {
                        case 0x0:       // LongConstant
                            // Taken care of when the BMUX is selected
                            break;

                        case 0x1:       // ShiftOnR
                            if (uOp.F == 0)
                            {
                                function = "ShiftOnR";
                            }
                            break;

                        case 0x2:       // StackReset
                            function += "Reset eStack, inc DDS";
                            break;

                        case 0x3:       // TOS := (R)
                            function += "TOS := (R)";
                            break;

                        case 0x4:       // Push
                            function += "Push R";
                            break;

                        case 0x5:       // Pop
                            function += "Pop";
                            break;

                        case 0x6:       // CtrlRstOp := (Z)
                            function += "CtrlRstOp := (Z)";
                            break;

                        case 0x7:       // SrcRasterOp := (R)
                            function += "SrcRstOp := (R)";
                            break;

                        case 0x8:       // DstRasterOp := (R)
                            function += "DstRstOp := (R)";
                            break;

                        case 0x9:       // WidRasterOp := (R)
                            function += "WidRstOp := (R)";
                            break;

                        case 0xa:       // LoadOp
                            function += "LoadOp";
                            break;

                        case 0xb:       // BPC := (R)
                            function += "BPC := (R)";
                            break;

                        case 0xc:       // WCSL
                            function += "WCSL";
                            break;

                        case 0xd:       // WCSM
                            function += "WCSM";
                            break;

                        case 0xe:       // WCSH
                            function += "WCSH";
                            break;

                        case 0xf:       // IOB function
                            function += String.Format("IOB({0:x2})", uOp.IOPort);
                            break;
                    }
                    break;

                case 0x1:       // Store / Extended functions
                    switch (uOp.SF)
                    {
                        case 0x0:   // (R) := Victim Latch
                            function += "(R) := Victim";
                            break;

                        case 0x1:   // Multiply / Divide step
                            function += "Mul/DivStep";
                            break;

                        case 0x2:   // Load multiplier / dividend
                            function += "MQ := (R)";
                            break;

                        case 0x3:   // Load base register (not R)
                            function += "RBase := (R)";
                            break;

                        case 0x4:   // (R) := product or quotient
                            function += "(R) := MQ";
                            break;

                        case 0x5:   // Push long constant
                            function += String.Format("Push {0:x4}", uOp.Y | (uOp.Z << 8));
                            break;

                        case 0x6:   // address input := Shift
                            function += "NIA := Shift";
                            break;

                        case 0x7:   // Leap address generation
                            // This is handled by CalcAddress
                            break;

                        case 0x8:   // Fetch4R
                            function += "Fetch4R";
                            break;

                        case 0x9:   // Store4R
                            function += "Store4R";
                            break;

                        case 0xa:   // Fetch4
                            function += "Fetch4";
                            break;

                        case 0xb:   // Store4
                            function += "Store4";
                            break;

                        case 0xc:   // Fetch2
                            function += "Fetch2";
                            break;

                        case 0xd:   // Store2
                            function += "Store2";
                            break;

                        case 0xe:   // Read a single word
                            function += "Fetch";
                            break;

                        case 0xf:   // Store a single word
                            function += "Store";
                            break;

                        default:
                            function += "<invalid>";
                            break;
                    }
                    break;

                case 0x3:       // Long jump
                    // Handled in DoJump.
                    break;

                default:
                    function += "<invalid>";
                    break;

            }

            return function;
        }


        private static string DisassembleShifterCommand(int input)
        {
            int low = (~input) & 0x0f;
            int high = ((~input) & 0xf0) >> 4;

            string shifter = "<invalid>";

            // See if this is a left or right shift, or a rotate
            if (low == 0xf)
            {
                shifter = String.Format("Left Shift {0}", high);
            }
            else if ((0xf - low) == high)
            {
                shifter = String.Format("Right Shift {0}", high);
            }
            else if (
                (low == 0xd || low == 0xe) &&
                (high >= 0x8 && high <= 0xf))
            {
                shifter = String.Format("Rotate {0}", (high & 0x7) | (low == 0xd ? 0x0 : 0x8));
            }
            else
            {
                shifter = String.Format("BitField, offset {0} mask {1:x4}", high, (0x1ffff >> (0x10 - low)));
            }

            return shifter;
        }


        private static string DisassembleJump(Instruction uOp)
        {
            string jump = "<invalid>";

            switch (uOp.CND)
            {
                case Condition.True:    // True -- always jump
                    jump = "{0}";
                    break;

                case Condition.False:   // False -- never jump
                    jump = "Never {0}";
                    break;

                case Condition.IntrPend: // IntrPend
                    jump = "If IntrPend, {0}";
                    break;

                case Condition.Carry19: // C19
                    jump = "If C19, {0}";
                    break;

                case Condition.BPC3:    // Opfile empty
                    jump = "If Opfile Empty, {0}";
                    break;

                case Condition.Odd:     // Odd
                    jump = "If Odd, {0}";
                    break;

                case Condition.ByteSign: // Bytesign
                    jump = "If ByteSign, {0}";
                    break;

                case Condition.Neq:     // NotEqual
                    jump = "If Neq, {0}";
                    break;

                case Condition.Leq:     // Leq
                    jump = "If Leq, {0}";
                    break;

                case Condition.Lss:     // Lss
                    jump = "If Lss, {0}";
                    break;

                case Condition.Ovf:     // Ovf
                    jump = "If Ovf, {0}";
                    break;

                case Condition.Carry15: // Carry (from bit 15)
                    jump = "If Cry, {0}";
                    break;

                case Condition.Eql:     // Equal -- jump if Zero flag is set
                    jump = "If Eql, {0}";
                    break;

                case Condition.Gtr:     // Gtr -- jump if Gtr flag is set
                    jump = "If Gtr, {0}";
                    break;

                case Condition.Geq:     // Geq
                    jump = "If Geq, {0}";
                    break;
            }

            return String.Format(jump, DisassembleJumpType(uOp));
        }

        /// <summary>
        /// Decodes the jump field.
        /// </summary>
        /// <param name="uOp"></param>
        private static string DisassembleJumpType(Instruction uOp)
        {
            string type = "<invalid>";

            // Do action if condition is satisfied.
            switch (uOp.JMP)
            {
                case JumpOperation.JumpZero:
                    type = "JumpZero";
                    break;

                case JumpOperation.Call:
                    type = String.Format("Call {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.CallS:
                    type = String.Format("Call S");
                    break;

                case JumpOperation.Goto:
                    type = String.Format("Goto {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.PushLoad:
                    type = String.Format("PushLoad {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.VectorDispatch:
                    if (uOp.H == 0)
                    {
                        // Vector
                        int next = ZOpFill(~uOp.Z);
                        next = next | ((~uOp.Z & 0x3c) << 4);
                        type = String.Format("Vector {0:x4}", next);
                    }
                    else
                    {
                        // Dispatch
                        int next = ZOpFill(~uOp.Z);
                        next = next | ((~uOp.Z & 0x3c) << 4);
                        type = String.Format("Dispatch {0:x4}", next);
                    }
                    break;

                case JumpOperation.GotoS:
                    type = String.Format("GotoS {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.Return:
                    type = String.Format("Return");
                    break;

                case JumpOperation.JumpPop:
                    if (uOp.H == 0)
                    {
                        type = String.Format("JumpPop {0:x4}", CalcAddress(uOp));
                    }
                    else
                    {
                        type = String.Format("LeapPop {0:x4}", CalcAddress(uOp));
                    }
                    break;

                case JumpOperation.Repeat:
                    type = String.Format("Repeat {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.RepeatLoop:
                    type = String.Format("RepeatLoop");
                    break;

                case JumpOperation.Loop:
                    type = String.Format("Loop");
                    break;

                case JumpOperation.LoadS:
                    type = String.Format("LoadS {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.Next:
                    type = String.Format("Continue");
                    break;

                case JumpOperation.ThreeWayBranch:
                    type = String.Format("ThreeWayBranch");
                    break;

                case JumpOperation.NextInstReviveVictim:
                    if (uOp.H == 0)
                    {
                        type = String.Format("NextInst {0:x4}", DisassembleNextInst(uOp));
                    }
                    else
                    {
                        type = String.Format("Revive Victim");
                    }
                    break;
            }

            return type;
        }

        /// <summary>
        /// Calculate the next microinstruction address.
        /// </summary>
        /// <param name="uOp"></param>
        /// <returns></returns>
        private static string CalcAddress(Instruction uOp)
        {
            ushort addr = 0;

            switch (uOp.F)
            {
                case 0:     // Constant/Short Jump
                case 1:
                    if (uOp.SF != 0x7)
                    {
                        addr = (ushort)((_pc & 0xf00) | (0xff & (~uOp.Z)));
                    }
                    else
                    {
                        // Leap
                        addr = (ushort)(((0xff & (~uOp.Z)) | ((0xff & (~uOp.Y)) << 8)) & 0x3fff);   // 14 bits
                    }

                    break;

                case 3:     // Long Jump
                    addr = (ushort)((0xfff & (~((uOp.Z | (uOp.SF << 8))))));
                    break;

                default:
                    throw new UnimplementedInstructionException(
                            String.Format("Error: F value does not specify a jump."));
            }

            return String.Format("{0:x4}", addr);
        }


        private static int ZOpFill(int z)
        {
            return (z & 0x3) | ((z & 0xc0) << 4);
        }


        private static int DisassembleNextInst(Instruction uOp)
        {
            return (ushort)ZOpFill(~uOp.Z);
        }

        private static ushort _pc;
    }
}

