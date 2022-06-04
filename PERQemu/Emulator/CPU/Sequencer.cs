//
// Sequencer.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.CompilerServices;

using PERQemu.Debugger;

namespace PERQemu.Processor
{
    public partial class CPU
    {
        /// <summary>
        /// Implements the Am2910 microsequencer, including its internal
        /// call stack, S register, and microcode jumps.  We also roll in
        /// the "2-bit kluge", the PC, and the Victim register.
        /// </summary>
        protected sealed class Sequencer
        {
            public Sequencer(CPU parent)
            {
                _cpu = parent;

                // Gotta keep 'em separated in case low bits overflow?
                _s = new ExtendedRegister((_wcsBits - 12), 12);
                _pc = new ExtendedRegister((_wcsBits - 12), 12);

                // Victim is a latch; treat as a single 12-14 bit value
                _victim = new ExtendedRegister(0, _wcsBits);

                _callStack = new CallStack();
            }

            public void Reset()
            {
                _s.Value = 0;
                _pc.Value = 0;
                _victim.Value = 0;
                _extendedOp = false;
                _callStack.Reset();

                Log.Debug(Category.Sequencer, "Reset");
            }

            /// <summary>
            /// Gets or sets the S register.
            /// </summary>
            /// <remarks>
            /// In the hardware S is comprised of the Am2910's built-in 12-bit
            /// S register plus the 2-bit kluge.  In the processor, S is always
            /// read or written as a full 12- or 14-bit address.  Internally
            /// there are operations that only use the low bits, but there's no
            /// reason to expose that outside the DispatchJump() method.  Thus,
            /// the processor sees S (and the PC, below) as simple ushorts.
            /// </remarks>
            public ushort S
            {
                get { return (ushort)_s.Value; }
                set { _s.Value = value; }
            }

            /// <summary>
            /// Gets or sets the microcode Program Counter.
            /// </summary>
            public ushort PC
            {
                get { return (ushort)_pc.Value; }
                set { _pc.Value = value; }
            }

            /// <summary>
            /// Gets or sets the Victim latch.
            /// </summary>
            /// <remarks>
            /// This is a bit of a cheat; since Victim is always 12- or 14-bits
            /// we just use the low half of the ExtendedRegister and let it do
            /// the masking for us.  Accesses in the hardware (all CPU types)
            /// are to the full address width, and no calculations that might
            /// overflow the low 12 bits are ever done directly, so we just
            /// expose the getter/setter as a ushort.  Yes, yes, I know...
            /// </remarks>
            public ushort Victim
            {
                get { return (ushort)_victim.Value; }
                set { _victim.Value = value; }
            }

            /// <summary>
            /// Support decoding of two-byte opcodes.
            /// </summary>
            public bool ExtendedOp
            {
                get { return _extendedOp; }
                set { _extendedOp = value; }
            }

            /// <summary>
            /// The Am2910 FULL pin is not wired up on the real PERQ, but allow
            /// access here for curiousity/debugging.
            /// </summary>
            /// <remarks>
            /// This is enabled in the PERQ24A CPU, whose sequencer has a 33-deep
            /// internal stack.  A field upgrade of original CPUs to the Am29C10
            /// could allow for an 8-level stack.  Exposing the FULL pin through
            /// the uState register would allow the microcode to detect this.
            /// </remarks>
            public bool StackFull
            {
                get { return _callStack.StackFull; }
            }

            /// <summary>
            /// Jumps based on the condition flags.
            /// </summary>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            public void DispatchJump(Instruction uOp)
            {
                bool conditionSatisfied = false;

                // Do jump operation based on ALU flags from the last instruction
                switch (uOp.CND)
                {
                    case Condition.True:
                        conditionSatisfied = true;
                        break;

                    case Condition.False:
                        conditionSatisfied = false;
                        break;

                    case Condition.IntrPend:
                        conditionSatisfied = _cpu._interrupt.Flag != InterruptFlag.None;
                        break;

                    case Condition.CarryH:
                        // Quirk: C19/C23 test is inverted!
                        conditionSatisfied = !_cpu._alu.OldFlags.CarryH;
                        break;

                    case Condition.BPC3:
                        conditionSatisfied = _cpu.OpFileEmpty;
                        break;

                    case Condition.Odd:
                        conditionSatisfied = (_cpu._alu.OldR.Value & 0x1) != 0;
                        break;

                    case Condition.ByteSign:
                        conditionSatisfied = (_cpu._alu.OldR.Value & 0x80) != 0;
                        break;

                    case Condition.Neq:
                        conditionSatisfied = _cpu._alu.OldFlags.Neq;
                        break;

                    case Condition.Leq:
                        conditionSatisfied = _cpu._alu.OldFlags.Leq;
                        break;

                    case Condition.Lss:
                        conditionSatisfied = _cpu._alu.OldFlags.Lss;
                        break;

                    case Condition.Ovf:
                        conditionSatisfied = _cpu._alu.OldFlags.Ovf;
                        break;

                    case Condition.Carry15:
                        conditionSatisfied = _cpu._alu.OldFlags.Cry;
                        break;

                    case Condition.Eql:
                        conditionSatisfied = _cpu._alu.OldFlags.Eql;
                        break;

                    case Condition.Gtr:
                        conditionSatisfied = _cpu._alu.OldFlags.Gtr;
                        break;

                    case Condition.Geq:
                        conditionSatisfied = _cpu._alu.OldFlags.Geq;
                        break;

                    default:
                        throw new UnimplementedInstructionException($"Unimplemented Condition {uOp.CND}");
                }

                // Dispatch jump action
                switch (uOp.JMP)
                {
                    //
                    // Jumps that can modify the 16K CPU extended stack (14 bits):
                    //

                    case JumpOperation.JumpZero:
                        _pc.Value = 0;
                        _callStack.Reset();
                        break;

                    case JumpOperation.Goto:
                        if (conditionSatisfied)
                        {
                            _pc.Value = uOp.NextAddress;
                        }
                        else
                        {
                            _pc.Lo++;
                        }
                        break;

                    case JumpOperation.Call:
                        if (conditionSatisfied)
                        {
                            _callStack.PushFull((ushort)(_pc.Value + 1));
                            _pc.Value = uOp.NextAddress;
                        }
                        else
                        {
                            _pc.Lo++;
                        }
                        break;

                    case JumpOperation.CallS:
                        _callStack.PushFull((ushort)(_pc.Value + 1));

                        if (conditionSatisfied)
                        {
                            _pc.Value = uOp.NextAddress;
                        }
                        else
                        {
                            _pc.Lo = _s.Lo;
                        }
                        break;

                    case JumpOperation.GotoS:
                        if (conditionSatisfied)
                        {
                            _pc.Value = uOp.NextAddress;
                        }
                        else
                        {
                            _pc.Lo = _s.Lo;
                        }
                        break;

                    case JumpOperation.Return:
                        if (conditionSatisfied)
                        {
                            _pc.Value = _callStack.PopFull();   // 14 bits (16K)
                        }
                        else
                        {
                            _pc.Lo++;
                        }
                        break;

                    case JumpOperation.JumpPop:
                        if (conditionSatisfied)
                        {
                            if (uOp.H == 0)
                            {
                                _callStack.PopLo();             // JumpPop
                                _pc.Lo = uOp.NextAddress;
                            }
                            else
                            {
                                _callStack.PopFull();           // LeapPop (16K)
                                _pc.Value = uOp.NextAddress;
                            }
                        }
                        else
                        {
                            _pc.Lo++;
                        }
                        break;

                    //
                    // Jumps that operate on 12-bit addresses only:
                    //

                    case JumpOperation.PushLoad:
                        if (conditionSatisfied)
                        {
                            _s.Value = uOp.NextAddress;
                            _pc.Lo++;
                            _callStack.PushLo(_pc.Lo);

                            Log.Debug(Category.Sequencer, "PushLoad: cond={0} S={1:x4}",
                                      conditionSatisfied, _s.Value);
                        }
                        else
                        {
                            _pc.Lo++;
                            _callStack.PushLo(_pc.Lo);
                        }
                        break;

                    case JumpOperation.LoadS:
                        _s.Value = uOp.NextAddress;
                        _pc.Lo++;

                        Log.Debug(Category.Sequencer, "Load S={0:x4}", _s.Value);
                        break;

                    case JumpOperation.RepeatLoop:
                        if (_s.Lo != 0)
                        {
                            Log.Detail(Category.Sequencer, "RepeatLoop S={0:x4}", _s.Lo);
                            _pc.Lo = _callStack.TopLo();
                            _s.Lo--;
                        }
                        else
                        {
                            Log.Debug(Category.Sequencer, "RepeatLoop done");
                            _pc.Lo++;
                            _callStack.PopLo();
                        }
                        break;

                    case JumpOperation.Repeat:
                        if (_s.Lo != 0)
                        {
                            Log.Detail(Category.Sequencer, "Repeat S={0:x4}", _s.Lo);
                            _pc.Lo = uOp.NextAddress;
                            _s.Lo--;
                        }
                        else
                        {
                            Log.Debug(Category.Sequencer, "Repeat done");
                            _pc.Lo++;
                        }
                        break;

                    case JumpOperation.Loop:
                        if (conditionSatisfied)
                        {
                            _pc.Lo++;
                            _callStack.PopLo();
                        }
                        else
                        {
                            _pc.Lo = _callStack.TopLo();
                        }
                        break;

                    case JumpOperation.Next:
                        _pc.Lo++;
                        break;

                    case JumpOperation.ThreeWayBranch:
                        if (conditionSatisfied)
                        {
                            if (_s.Lo != 0)
                            {
                                _pc.Lo++;
                                _callStack.PopLo();
                                _s.Lo--;
                            }
                            else
                            {
                                _pc.Lo++;
                                _callStack.PopLo();
                            }
                        }
                        else
                        {
                            if (_s.Lo != 0)
                            {
                                _pc.Lo = _callStack.TopLo();
                                _s.Lo--;
                            }
                            else
                            {
                                _pc.Lo = uOp.NextAddress;
                                _callStack.PopLo();
                            }
                        }
                        break;

                    //
                    // Special jumps
                    //

                    case JumpOperation.NextInstReviveVictim:
                        if (uOp.H == 0)
                        {
                            DoNextInst(uOp);
                        }
                        else
                        {
                            // Is the victim latch set?
                            if (Victim == 0)
                            {
                                throw new InvalidOperationException("Revive from unset victim latch!");
                            }

                            Log.Debug(Category.Sequencer, "PC restored from victim ({0:x4})", Victim);

                            _pc.Value = Victim;     // Revive
                            Victim = 0;             // Clear the latch
                        }
                        break;

                    case JumpOperation.VectorDispatch:
                        if (conditionSatisfied)
                        {
                            if (uOp.H == 0)
                            {
                                // Vector
                                _pc.Lo = (ushort)(uOp.ZFillAddress | ((_cpu._interrupt.Priority & 0xf) << 2));
                            }
                            else
                            {
                                // Dispatch
                                _cpu._shifter.Shift(_cpu._alu.OldR.Lo);
                                _pc.Lo = (ushort)(uOp.ZFillAddress | (((~_cpu._shifter.ShifterOutput) & 0xf) << 2));
                            }
                            Log.Debug(Category.Sequencer, "Dispatch to {0:x4}", _pc.Lo);
                        }
                        else
                        {
                            _pc.Lo++;
                        }
                        break;

                    default:
                        throw new UnimplementedInstructionException($"Unhandled Jump type {uOp.JMP}");
                }
            }

            /// <summary>
            /// Increment the BPC and update the program counter.
            /// </summary>
            /// <remarks>
            /// Handle debugging output of two-byte opcodes when NextInst() is
            /// used to dispatch the second byte.  (NextOp might also be used,
            /// so that case is handled elsewhere.)
            /// </remarks>
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            private void DoNextInst(Instruction uOp)
            {
                QCode q;
                byte next = _cpu._opFile[_cpu.BPC];

                if (_extendedOp)
                {
                    // Add 2nd byte and look up the extended op
                    _cpu._lastOpcode = (ushort)((_cpu._lastOpcode << 8) | next);

                    q = QCodeHelper.GetExtendedOpCode(_cpu._lastOpcode);
                    _extendedOp = false;
                }
                else
                {
                    q = QCodeHelper.GetQCodeFromOpCode(next);
                    _extendedOp = q.IsPrefix;
                }

                Log.Debug(Category.QCode, "NextInst is {0:x2}{1}{2} at BPC {3}", next,
                          (_extendedOp ? "+" : "-"), q.Mnemonic, _cpu.BPC);   // Subtle :-)

                // NextInst address generation requires a little extra care: use
                // the ZFill'ed address but mask off the four extra bits that are
                // to be filled in by the Op byte.  And we have to preserve the
                // "bank" (high 2 bits on 16K) bits so high-bank two byte ops work!
                _pc.Lo = (ushort)((uOp.ZFillAddress & 0xc03) | ((~next & 0xff) << 2));

                _cpu._lastOpcode = next;
                _cpu._incrementBPC = true;

                Log.Debug(Category.Sequencer, "NextInst Branch to {0:x4} ", _pc.Value);
            }

            /// <summary>
            /// Dumps the contents of the stack on the console (debugging)
            /// </summary>
            public void DumpContents()
            {
                _callStack.DumpContents();
            }


            // Access to the surrounding class
            private CPU _cpu;

            // 2910 S register
            private ExtendedRegister _s;

            // Microcode address
            private ExtendedRegister _pc;

            // Victim latch
            private ExtendedRegister _victim;

            // 2910 call stack
            private CallStack _callStack;

            // Support decoding two-byte ops
            private bool _extendedOp;
        }
    }
}