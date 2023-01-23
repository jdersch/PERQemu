//
// Disassembler.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using System.Text;

using PERQemu.Processor;

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
        public static string Disassemble(ushort address, CPU.Instruction op)
        {
            var sb = new StringBuilder();

            // Append the address and raw opcode
            sb.AppendFormat("{0:x4}: {1:x12}\t", address, op.UCode);

            var amux = DisassembleAmuxInput(op);
            var bmux = DisassembleBmuxInput(op);

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

            // Append function, if any
            var func = DisassembleFunction(op);

            if (func.Length > 0)
            {
                sb.AppendFormat("{0}. ", DisassembleFunction(op));
            }

            // Append jump
            sb.AppendFormat("{0}.", DisassembleJump(op));

            return sb.ToString();
        }


        static string DisassembleAmuxInput(CPU.Instruction uOp)
        {
            string amux = "<invalid>";

            switch (uOp.A)
            {
                case AField.Shifter:    // Shifter output
                    amux = "Shifter";
                    break;

                case AField.NextOp:     // Next byte from Op
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
                    amux = (uOp.X < 0x40) ? $"R{uOp.X:x2}%" : $"R{uOp.X:x2}";
                    break;

                case AField.TOS:        // Expression stack
                    amux = "TOS";
                    break;
            }

            return amux;
        }


        static string DisassembleBmuxInput(CPU.Instruction uOp)
        {
            string bmux = "<invalid>";

            // Select BMUX input
            if (uOp.B == 0)
            {
                // XY Register selected by Y
                bmux = (uOp.Y < 0x40) ? $"R{uOp.Y:x2}%" : $"R{uOp.Y:x2}";
            }
            else
            {
                // A constant (precomputed)
                bmux = $"{uOp.BMuxInput:x4}";
            }

            return bmux;
        }


        static string DisassembleALUOp(string amux, string bmux, CPU.Instruction op)
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
                    alu = $"Not {amux}";
                    break;

                case ALUOperation.NotB:
                    alu = $"Not {bmux}";
                    break;

                case ALUOperation.AandB:
                    alu = $"{amux} And {bmux}";
                    break;

                case ALUOperation.AandNotB:
                    alu = $"{amux} And Not {bmux}";
                    break;

                case ALUOperation.AnandB:
                    alu = $"{amux} Nand {bmux}";
                    break;

                case ALUOperation.AorB:
                    alu = $"{amux} Or {bmux}";
                    break;

                case ALUOperation.AorNotB:
                    alu = $"{amux} Or Not {bmux}";
                    break;

                case ALUOperation.AnorB:
                    alu = $"{amux} Nor {bmux}";
                    break;

                case ALUOperation.AxorB:
                    alu = $"{amux} Xor {bmux}";
                    break;

                case ALUOperation.AxnorB:
                    alu = $"{amux} Xnor {bmux}";
                    break;

                case ALUOperation.AplusB:
                    alu = $"{amux} + {bmux}";
                    break;

                case ALUOperation.AplusBplusCarry:
                    alu = $"{amux} + {bmux} + Cry";
                    break;

                case ALUOperation.AminusB:
                    alu = $"{amux} - {bmux}";
                    break;

                case ALUOperation.AminusBminusCarry:
                    alu = $"{amux} - {bmux} - Cry";
                    break;
            }

            return alu;
        }

        /// <summary>
        /// Decode Function and Special Function fields.
        /// </summary>
        static string DisassembleFunction(CPU.Instruction uOp)
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
                            // (minor aesthetic correction :-)
                            function = function.TrimEnd(' ', ',');
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
                            function += $"IOB({uOp.IOPort:x2})";
                            break;
                    }
                    break;

                case 0x1:       // Store / Extended functions
                    switch (uOp.SF)
                    {
                        // fixme: technically SF 0..7 are 16K only
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
                            function += $"Push {uOp.LongConstant:x4}";
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
                    // Handled in DoJump
                    break;

                default:
                    function += "<invalid>";
                    break;
            }

            return function;
        }


        static string DisassembleShifterCommand(int input)
        {
            int low = (~input) & 0x0f;
            int high = ((~input) & 0xf0) >> 4;

            string shifter = "<invalid>";

            // See if this is a left or right shift, or a rotate
            if (low == 0xf)
            {
                shifter = $"Left Shift {high}";
            }
            else if ((0xf - low) == high)
            {
                shifter = $"Right Shift {high}";
            }
            else if ((low == 0xd || low == 0xe) && (high >= 0x8 && high <= 0xf))
            {
                shifter = string.Format("Rotate {0}", (high & 0x7) | (low == 0xd ? 0x0 : 0x8));
            }
            else
            {
                shifter = string.Format("BitField, offset {0} mask {1:x4}", high, (0x1ffff >> (0x10 - low)));
            }

            return shifter;
        }


        static string DisassembleJump(CPU.Instruction uOp)
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

                case Condition.CarryH:  // C19 or C23
                    jump = (CPU.CPUBits == 20) ? "If C19, {0}" : "If C23, {0}";
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

            return string.Format(jump, DisassembleJumpType(uOp));
        }


        static string DisassembleJumpType(CPU.Instruction uOp)
        {
            string type = "<invalid>";

            // Do action if condition is satisfied
            switch (uOp.JMP)
            {
                case JumpOperation.JumpZero:
                    type = "JumpZero";
                    break;

                case JumpOperation.Call:
                    type = string.Format("Call {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.CallS:
                    type = "CallS";
                    break;

                case JumpOperation.Goto:
                    type = string.Format("Goto {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.PushLoad:
                    type = string.Format("PushLoad {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.VectorDispatch:
                    if (uOp.H == 0)
                    {
                        // Vector
                        type = $"Vector {uOp.ZFillAddress:x4}";
                    }
                    else
                    {
                        // Dispatch
                        type = $"Dispatch {uOp.ZFillAddress:x4}";
                    }
                    break;

                case JumpOperation.GotoS:
                    type = string.Format("GotoS {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.Return:
                    type = "Return";
                    break;

                case JumpOperation.JumpPop:
                    if (uOp.H == 0)
                    {
                        type = string.Format("JumpPop {0:x4}", CalcAddress(uOp));
                    }
                    else
                    {
                        type = string.Format("LeapPop {0:x4}", CalcAddress(uOp));
                    }
                    break;

                case JumpOperation.Repeat:
                    type = string.Format("Repeat {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.RepeatLoop:
                    type = "RepeatLoop";
                    break;

                case JumpOperation.Loop:
                    type = "Loop";
                    break;

                case JumpOperation.LoadS:
                    type = string.Format("LoadS {0:x4}", CalcAddress(uOp));
                    break;

                case JumpOperation.Next:
                    type = "Continue";
                    break;

                case JumpOperation.ThreeWayBranch:
                    type = "ThreeWayBranch";
                    break;

                case JumpOperation.NextInstReviveVictim:
                    if (uOp.H == 0)
                    {
                        type = string.Format("NextInst {0:x4}", (uOp.ZFillAddress & 0x3c03));
                    }
                    else
                    {
                        type = "Revive Victim";
                    }
                    break;
            }

            return type;
        }


        static string CalcAddress(CPU.Instruction uOp)
        {
            // Always clip, to remove any ambiguity?
            var addr = uOp.NextAddress & CPU.WCSMask;

            if (addr != uOp.NextAddress)
                throw new UnimplementedInstructionException($"addr={addr:x4} != uop={uOp.NextAddress:x4}");

            switch (uOp.F)
            {
                case 0:     // Short jump
                case 3:     // Long jump
                    return $"{addr:x4}";

                case 1:     // Short (4K) or Leap / Shift as addr source (16K)
                    return (!CPU.Is4K && uOp.SF == 6) ? "Shift" : $"{addr:x4}";

                default:
                    // throw new UnimplementedInstructionException("Error: F value does not specify a jump");
                    return "<ERROR: F does not specify a jump>";
            }
        }
    }
}

