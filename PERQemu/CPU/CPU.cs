// cpu.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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
using System.IO;
using System.Text;

using PERQemu.IO;
using PERQemu.Memory;
using PERQemu.Debugger;
using System.Runtime.CompilerServices;

#if DEBUG
using PERQemu.IO.Z80.IOB;
using System.Collections.Generic;
#endif

namespace PERQemu.CPU
{
    /// <summary>
    /// PERQ hardware interrupts, listed in order of priority.
    /// </summary>
    [Flags]
    public enum InterruptType
    {
        None = 0x00,
        Z80DataOutReady = 0x01,
        Y = 0x02,
        HardDisk = 0x04,
        Network = 0x08,
        Z80DataInReady = 0x10,
        LineCounter = 0x20,
        X = 0x40,
        Parity = 0x80,
    }

    /// <summary>
    /// Implements a PERQ 1 4K or 16K CPU (depending on the SIXTEEN_K compiler flag).
    /// Since we only ever need one of these, and to simplify communication with other
    /// devices (interrupt requests, etc.) this is a singleton.
    ///
    /// There are some aspects that could be factored out of this class (like the DDS
    /// and boot ROM logic).
    /// </summary>    
    public sealed class PERQCpu
    {
        public PERQCpu(PERQSystem system)
        {
            _system = system;

            // Load the boot ROM
            LoadBootRom(Paths.BuildPROMPath("boot.bin"));

            // Create CPU components
            _alu = new ALU();
            _shifter = new Shifter();
#if SIXTEEN_K
            _mqShifter = new Shifter();
#endif
            _rasterOp = new RasterOp(system);
            _memory = system.MemoryBoard;
            _ioBus = new IOBus(system);

            Reset();
        }

        // "Real" clock speed in cycles/sec (based on 170ns cycle time).  Approx. 5.88Mhz.
        public static readonly int Frequency = 5882353;

        //
        // Z80/IO board sampling factor.  The PERQ was roughly 2.4x faster in clock rate
        // than the Z80 on the IOB, but clocking our fake Z80 even every other cycle is a
        // lot of overhead.  Make this more explicit and tunable here (8-16 is a pretty
        // good range?) and expose it so the "clock" hardware can simulate the 60Hz line
        // frequency "jiffy clock" more accurately.  Cuz why not.
        //
        public static readonly int IOFudge = 8;

        /// <summary>
        /// Reset the CPU, clear the writable control store and re-enable the boot ROM.
        /// </summary>
        public void Reset()
        {
            _romEnabled = true;
            _interruptFlag = InterruptType.None;

            _clocks = 0;
            _dds = 0;
            _iod = 0;
            _bpc = 0;
            _lastBmux = 0;
            _wcsHold = false;
            _incrementBPC = false;
            _victim = 0xffff;
            _stackPointer = 0;
            _pc.Value = 0;
            _s.Value = 0;
            _mq = 0;
#if SIXTEEN_K
            _registerBase = 0;
            _mqEnabled = false;
#endif

#if TRACING_ENABLED
            _lastPC = 0;
#endif
            _oldALURegisters = new ALURegisterFile();

            // Clear microcode
            for (int i = 0; i < _microcode.Length; i++)
            {
                _microcodeCache[i] = null;
                _microcode[i] = 0;
            }

            // Clear registers
            for (int i = 0; i < _r.Length; i++)
            {
                _r[i] = 0;
            }

            // Clear Estack
            for (int i = 0; i < _stack.Length; i++)
            {
                _stack[i] = 0;
            }

            _callStack.Clear();
            _callStack.Reset();

            _alu.Reset();
            _rasterOp.Reset();
            _memory.Reset();
            _ioBus.Reset();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.CpuState, "CPU: Reset.");
#endif
        }

        /// <summary>
        /// Executes a single microinstruction and runs associated hardware for one cycle.
        /// </summary>
        public void Execute()
        {
            // Decode the next instruction
            Instruction uOp = GetInstruction(PC);

            _clocks++;

            // Clock the memory state machine and set up any pending fetches
            _memory.Tick(uOp.MemoryRequest);

            //
            // Several conditions may stall the processor:
            //  1.  In the T0 & T1 after any Fetch, attempting to access MDI or MDX causes
            //      the processor to stall, but other instructions may run in those cycles;
            //  2.  The memory system may assert a "hold" if the IO or DMA is accessing
            //      memory; we don't actually enforce that, yet, but the "wait" may be set
            //      for stores issued in the wrong cycle;
            //  3.  WCS writes actually take two cycles to execute; we fake one wait state
            //      here with the _wcsHold boolean.
            // If any of those conditions are true we "abort" the current instruction until
            // the correct cycle comes around.
            //
            bool abort = (_wcsHold || _memory.Wait || (uOp.WantMDI && !_memory.MDIValid));

            if (abort)
            {
                // We're waiting for the next T3 or T2 cycle to come around on the guitar is
                // what we're doing.  We can't cheat and skip the wait states, as RasterOp
                // depends on T-state cycling through each word during overlapped fetch/stores.
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.MemoryState,
                        "CPU: Abort in T{0}\n\twait={1} needMDO={2} wantMDI={3} MDIvalid={4} WCShold={5}",
                        _memory.TState, _memory.Wait, _memory.MDONeeded, uOp.WantMDI, _memory.MDIValid, _wcsHold);
#endif

                // On aborts, no memory writes occur - no Tock()

                // WCS writes take two cycles to run on the real hardware, but just
                // one cycle here -- enough to screw up memory timing during the loop
                // when microcode is being loaded!  Clear the hold flag to continue.
                _wcsHold = false;
                return;
            }

            //
            // Run the microinstruction
            //

#if TRACING_ENABLED
            // This is a very expensive log, so only call it if we have to
            if (Trace.TraceOn && (LogType.Instruction & Trace.TraceLevel) != 0)
            {
                Trace.Log(LogType.Instruction, "uPC={0:x4}: {1}", PC, Disassembler.Disassemble(PC, uOp));
            }

            // Catch cases where the CPU is looping forever
            if (_lastPC == PC &&
                uOp.CND == Condition.True &&
                uOp.JMP == JumpOperation.Goto)
            {
                throw new UnimplementedInstructionException(String.Format("CPU has halted in a loop at {0:x5}", PC));
            }
#endif

            // If the last instruction was NextOp or NextInst, increment BPC at the start of this instruction
            if (_incrementBPC)
            {
                _bpc++;
                _incrementBPC = false;

#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.OpFile, "OpFile: BPC incremented to {0:x1}", BPC);
#endif
            }

            // Latch the ALU registers from the last micro-op before we do this instruction's
            // ALU operation since we need them for conditional jumps later.
            _oldALURegisters = _alu.Registers;

            // Now decode and execute the actual instruction:

            // Select ALU inputs:

            int bmux;

            // Select BMUX input
            if (uOp.B == 0)
            {
#if SIXTEEN_K
                if (uOp.Y < 0x40)
                {
                    bmux = _r[uOp.Y | _registerBase];   // Or-in Base register (16K only)
                }
                else
#endif
                {
                    bmux = _r[uOp.Y];
                }
            }
            else
            {
                bmux = uOp.BMuxInput;
            }

            _lastBmux = bmux;

            int amux = GetAmuxInput(uOp);

#if SIXTEEN_K
            // If the hardware multiply unit is enabled, run the (possibly modified) ALU op
            if (_mqEnabled)
            {
                DoMulDivALUOp(amux, bmux, _mq, uOp.ALU);
            }
            else
#endif
            {
                // Do ALU operation
                _alu.DoALUOp(amux, bmux, uOp.ALU);
            }

            // Do writeback if W bit is set
            DoWriteBack(uOp);

            // If enabled, clock the RasterOp pipeline
            if (_rasterOp.Enabled)
            {
                _rasterOp.Clock();
            }

            // Is a memory store operation in progress?  Pending RasterOp stores
            // supercede the ALU; otherwise write the last ALU result
            if (_system.MemoryBoard.MDONeeded)
            {
                if (_rasterOp.ResultReady)
                {
                    _memory.Tock(_rasterOp.Result());
                }
                else
                {
                    _memory.Tock((ushort)_alu.Registers.R);
                }
            }

            // Execute whatever function this op calls for
            DispatchFunction(uOp);

#if TRACING_ENABLED
            _lastPC = PC;
            _lastInstruction = uOp;
#endif

            // Jump to where we need to go...
            DispatchJump(uOp);

        }

        #region Getters and setters

        /// <summary>
        /// This is temporary while performance analysis is in effect.
        /// </summary>
        public long Clocks
        {
            get 
            {
                long v = _clocks;
                _clocks = 0;
                return v; 
            }
        }

        /// <summary>
        /// Returns the current DDS value.  (Not really a register, but useful! :-)
        /// </summary>
        [DebugProperty("dds")]
        public int DDS
        {
            get { return _dds; }
        }

        /// <summary>
        /// Ensures 4 bits of _bpc, for convenience.
        /// </summary>
        [DebugProperty("bpc")]
        public int BPC
        {
            get { return _bpc & 0xf; }
        }

        /// <summary>
        /// Exposes whether the last instruction was NextInst or NextOp
        /// </summary>
        public bool IncrementBPC
        {
            get { return _incrementBPC; }
        }

        /// <summary>
        /// Indicates whether the Opfile is empty or not,
        /// based on bit 4 of BPC.
        /// </summary>
        public bool OpFileEmpty
        {
            get { return (BPC & 0x8) != 0; }
        }

        /// <summary>
        /// The Victim Latch.
        /// </summary>
        [DebugProperty("victim")]
        public ushort Victim
        {
            get { return _victim; }
        }

        /// <summary>
        /// The uState register.
        /// </summary>
        [DebugProperty("ustate")]
        public int MicrostateRegister
        {
            get
            {
                return
                    BPC |
                    (_alu.Registers.Ovf ? 0x0010 : 0x0) |
                    (_alu.Registers.Eql ? 0x0020 : 0x0) |
                    (_alu.Registers.Cry ? 0x0040 : 0x0) |
                    (_alu.Registers.Lss ? 0x0080 : 0x0) |
                    (_stackPointer != 0 ? 0x0200 : 0x0) |
                    ((((~_lastBmux) >> 16) & 0xf) << 12);
            }
        }

        /// <summary>
        /// The Microcode PC, either 12 or 14 bits depending on the CPU.
        /// </summary>
        [DebugProperty("pc")]
        public ushort PC
        {
            get { return _pc.Value; }
            set { _pc.Value = value; }
        }

        /// <summary>
        /// The current interrupt status.
        /// </summary>
        [DebugProperty("int")]
        public InterruptType InterruptFlag
        {
            get { return _interruptFlag; }
        }

        /// <summary>
        /// Raises the specified interrupt.
        /// </summary>
        [DebugFunction("raise interrupt")]
        public void RaiseInterrupt(InterruptType i)
        {
            _interruptFlag |= i;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Interrupt, "Interrupt flag {0} raised, active interrupts now {1}", i, _interruptFlag);
#endif
        }

        /// <summary>
        /// Clears the specified interrupt, if set.
        /// </summary>
        [DebugFunction("clear interrupt")]
        public void ClearInterrupt(InterruptType i)
        {
#if TRACING_ENABLED
            if ((_interruptFlag & ~i) != _interruptFlag)
            {
                if (Trace.TraceOn)
                    Trace.Log(LogType.Interrupt, "Interrupt flag {0} cleared, active interrupts now {1}", i, _interruptFlag & ~i);
            }
#endif
            _interruptFlag &= ~i;
        }

        /// <summary>
        /// The CPU's microcode store (4 or 16K).
        /// </summary>
        [DebugProperty("ucode")]
        public ulong[] Microcode
        {
            get { return _microcode; }
        }

        /// <summary>
        /// The ALU's last result register "R".
        /// </summary>
        [DebugProperty("r")]
        public int R
        {
            get { return _alu.Registers.R; }
        }

#if SIXTEEN_K
        /// <summary>
        /// The base register for indexing XY.
        /// </summary>
        [DebugProperty("rbase")]
        public byte RegisterBase
        {
            get { return _registerBase; }
        }
#endif

        /// <summary>
        /// The Multiply/Divide MQ register.
        /// </summary>
        [DebugProperty("mq")]
        public int MQ
        {
            get { return _mq; }
        }

        /// <summary>
        /// The most recent word on the IO data bus.
        /// </summary>
        [DebugProperty("iod")]
        public int IOD
        {
            get { return _iod; }
        }

        /// <summary>
        /// The CPU's expression stack.
        /// </summary>
        [DebugProperty("estk")]
        public int[] EStack
        {
            get { return _stack; }
        }

        /// <summary>
        /// The expression stack pointer.
        /// </summary>
        [DebugProperty("estkptr")]
        public int EStackPointer
        {
            get { return _stackPointer; }
            set { _stackPointer = value; }
        }

        /// <summary>
        /// The Am2910 call stack.
        /// </summary>
        public CallStack CallStack
        {
            get { return _callStack; }
        }

        public RasterOp RasterOp
        {
            get { return _rasterOp; }
        }
        #endregion

        #region CPU Helper functions
        /// <summary>
        /// Reads the instruction from the microcode store (or ROM) at the given address.
        /// </summary>
        /// <param name="address"></param>
        /// <returns>The microinstruction to execute</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public Instruction GetInstruction(ushort address)
        {
            // Return the precomputed microcode, unless it has yet to be cached
            Instruction current = _microcodeCache[address];
            if (current == null)
            {
                ulong word;

                if (_romEnabled)
                {
                    if (address < 0x200)
                    {
                        word = _rom[address];
                    }
                    else
                    {
                        word = _microcode[address];
                    }
                }
                else
                {
                    word = _microcode[address];
                }

                current = _microcodeCache[address] = new Instruction(word, address);
            }

            return current;
        }

        /// <summary>
        /// Selects the proper AMUX input for the specified instruction
        /// </summary>
        /// <param name="uOp"></param>
        /// <returns>AMUX input</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private int GetAmuxInput(Instruction uOp)
        {
            int amux = 0;

            // Select AMUX input
            switch (uOp.A)
            {
                case AField.Shifter:
                    _shifter.Shift(_oldALURegisters.R);
                    amux = _shifter.ShifterOutput;
                    break;

                case AField.NextOp:
                    if (OpFileEmpty)
                    {
                        // Only latch if Victim is empty (0xffff indicates an unset Victim...)
                        if (_victim == 0xffff)
                        {
#if SIXTEEN_K
                            // 16K CPU - All 14 bits saved, according to VFY 2.x...
                            _victim = _pc.Value;
#else
                            // 4K CPU - Only the low 12 bits count for the Victim latch
                            _victim = _pc.Lo;
#endif
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.OpFile, "Victim register is now {0:x4}", _victim);
#endif
                        }
                    }

                    amux = _memory.OpFile[BPC];
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.QCode, "NextOp read from BPC[{0:x1}]={1:x2}", BPC, amux);
#endif
                    _incrementBPC = true;   // Increment BPC at the beginning of the next instruction
                    break;

                case AField.IOD:
                    amux = _iod;
                    break;

                case AField.MDI:
                    amux = _memory.MDI;
                    break;

                case AField.MDX:
                    amux = (_memory.MDI & 0xf) << 16;
                    break;

                case AField.UState:
                    amux = MicrostateRegister;
                    break;

                case AField.XYRegister:
#if SIXTEEN_K
                    if (uOp.X < 0x40)
                    {
                        amux = _r[uOp.X | _registerBase];   // Or-in Base register (16K only)
                    }
                    else
#endif
                    {
                        amux = _r[uOp.X];
                    }
                    break;

                case AField.TOS:
                    amux = _stack[_stackPointer];
                    break;

                default:
                    throw new UnimplementedInstructionException(String.Format("Unimplemented AMUX {0}", uOp.A));
            }

            return amux;
        }

        /// <summary>
        /// Selects the proper BMUX input for the specified instruction
        /// </summary>
        /// <param name="uOp"></param>
        /// <returns>BMUX input</returns>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private int GetBmuxInput(Instruction uOp)
        {
            int bmux = 0;

            // Select BMUX input
            if (uOp.B == 0)
            {
#if SIXTEEN_K
                if (uOp.Y < 0x40)
                {
                    bmux = _r[uOp.Y | _registerBase];   // Or-in Base register (16K only)
                }
                else
#endif
                {
                    bmux = _r[uOp.Y];
                }
            }
            else
            {
                bmux = uOp.BMuxInput;
            }

            return bmux;
        }

        /// <summary>
        /// If W bit set, write R back to the register file.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void DoWriteBack(Instruction uOp)
        {
            // Do it if the W (write) bit is set.
            // We ASSUME that the assembler sets W for R := (MQ | Victim) phrases...
            if (uOp.W == 1)
            {
#if SIXTEEN_K
                if (uOp.X < 0x40)
                {
                    _r[uOp.X | _registerBase] = _alu.Registers.R;   // Or-in Base register (16K only)

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.RegisterAssignment, "R|RBase{0:x2}={1:x5}",
                            uOp.X | _registerBase, _alu.Registers.R);
#endif
                }
                else
#endif
                {
                    _r[uOp.X] = _alu.Registers.R;
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.RegisterAssignment, "R{0:x2}={1:x5}", uOp.X, _alu.Registers.R);
#endif
                }
            }
        }

#if SIXTEEN_K
        /// <summary>
        /// Dark magick to modify the ALU op if necessary for a Multiply or Divide step.
        /// Modifies the Amux and ALU op inputs as needed and runs the ALU; later the
        /// special function select updates the MQ register appropriately.
        /// </summary>
        private void DoMulDivALUOp(int amux, int bmux, int mdReg, ALUOperation curOp)
        {
            ALUOperation modOp = curOp;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MulDiv, "MulDiv ALUop: IN  op={0} amux={1:x5} mq={2:x4}", curOp, amux, mdReg);
#endif

            if (curOp == ALUOperation.AplusB || curOp == ALUOperation.AminusB)
            {
                switch (_rasterOp.MulDivInst)
                {
                    case MulDivCommand.Off:
                        // Should never happen!
                        throw new InvalidOperationException("DoMulDivALUOp called with MulDivInst=OFF!?");

                    case MulDivCommand.UnsignedDivide:
                        //
                        // For a divide step, shift the MSB of the quotient (MQ<15> from the last cycle)
                        // into the LSB of the remainder (SHIFT<0> here, already shifted by amux select).
                        // Later DispatchFunction() will shift the quotient in MQ and apply the computed Q0 bit.
                        //
                        int bit = (mdReg & 0x8000) >> 15;           // save MQ<15> from last cycle
                        amux = ((amux & 0xffffe) | bit);            // xfer it into SHIFT<0> (current amux input)

                        //
                        // Next, examine the sign bit from the PREVIOUS cycle's result (R<15>) to determine
                        // if the current op should be an addition or subtraction (of the remainder).  Only
                        // do this if the current op is add/sub (first use of DivideStep is a shift).
                        //
                        if ((_oldALURegisters.R & 0x8000) != 0)     // was sign of previous R + or -?
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
                        // For a multiply, check the LSB of the multiplier (current MQ register);
                        // if set, do an add, else pass thru unmolested.  This happens before the
                        // MQ is shifted in DispatchFunction().
                        //
                        if ((mdReg & 0x1) == 1)
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

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MulDiv, "MulDiv ALUop: OUT op={0} amux={1:x5}", modOp, amux);
#endif

            // Execute the updated ALU op
            _alu.DoALUOp(amux, bmux, modOp);
        }
#endif

        /// <summary>
        /// Dispatches function and special function operations based on the instruction
        /// </summary>
        /// <param name="uOp"></param>
        private void DispatchFunction(Instruction uOp)
        {
            switch (uOp.F)
            {
                case 0x0:
                case 0x2:   // Special Functions

                    // Calculate shifter output when F=2
                    if (uOp.F == 0x2)
                    {
#if TRACING_ENABLED
                        if (Trace.TraceOn) Trace.Log(LogType.Shifter, "ShiftOnZ");
#endif
                        _shifter.SetShifterCommand(uOp.Z);
                    }

                    switch (uOp.SF)
                    {
                        case 0x0:   // LongConstant
                                    // Taken care of when the BMUX is selected
                            break;

                        case 0x1:   // ShiftOnR
                            if (uOp.F == 0)
                            {
#if TRACING_ENABLED
                                if (Trace.TraceOn) Trace.Log(LogType.Shifter, "ShiftOnR");
#endif
                                _shifter.SetShifterCommand(~(_alu.Registers.R));
                            }
                            break;

                        case 0x2:   // StackReset
                            StackReset();
                            break;

                        case 0x3:   // TOS := (R)
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.EStack, "TOS set to {0:x5}", _alu.Registers.R);
#endif
                            _stack[_stackPointer] = _alu.Registers.R;
                            break;

                        case 0x4:   // Push
                            StackPush(_alu.Registers.R);
                            break;

                        case 0x5:   // Pop
                            StackPop();
                            break;

                        case 0x6:   // CntlRasterOp := (Z)
                            _rasterOp.CntlRasterOp(uOp.Z);
                            break;

                        case 0x7:   // SrcRasterOp := (R)
                            _rasterOp.SrcRasterOp(_alu.Registers.R);
                            break;

                        case 0x8:   // DstRasterOp := (R)
                            _rasterOp.DstRasterOp(_alu.Registers.R);
                            break;

                        case 0x9:   // WidRasterOp := (R)
                            _rasterOp.WidRasterOp(_alu.Registers.R);
#if SIXTEEN_K
                            //
                            // The hardware multiply/divide support is enabled or disabled by setting the
                            // upper two bits in the WidRasterOp register.  Oof.  Due to the order in which
                            // ALU and Dispatch ops are done, we'll set an enable flag here to minimize the
                            // ugly interactions between ALU, CPU and RasterOp.
                            //
                            // Enabling the MulDiv unit sets the MQ shifter control word.
                            //
                            switch (_rasterOp.MulDivInst)       // taken from WidRasterOp reg bits <7:6>
                            {
                                case MulDivCommand.Off:
#if TRACING_ENABLED
                                    if (Trace.TraceOn && _mqEnabled) Trace.Log(LogType.MulDiv, "MulDiv unit disabled.");
#endif
                                    _mqEnabled = false;
                                    break;

                                case MulDivCommand.UnsignedDivide:
#if TRACING_ENABLED
                                    if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv enabled: Divide");
#endif
                                    _mqShifter.SetShifterCommand(ShifterCommand.LeftShift, 1, 0);
                                    _mqEnabled = true;
                                    break;

                                case MulDivCommand.UnsignedMultiply:
                                case MulDivCommand.SignedMultiply:
#if TRACING_ENABLED
                                    if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv enabled: Multiply");
#endif
                                    _mqShifter.SetShifterCommand(ShifterCommand.RightShift, 1, 0);
                                    _mqEnabled = true;
                                    break;
                            }
#endif
                            break;

                        case 0xa:   // LoadOp
                            _memory.LoadOpFile();

                            if (_romEnabled)
                            {
                                _romEnabled = false;        // OpFile load disables ROM

                                // And since we've disabled the ROM, we ought just
                                // for good measure to invalidate the microcode cache.
                                for (int i = 0; i < 512; i++)
                                {
                                    _microcodeCache[i] = null;
                                }
                            }
                            break;

                        case 0xb:   // BPC := (R)
                            _bpc = _alu.Registers.R & 0xf;  // bottom 4 bits of R
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.OpFile, "BPC set to {0:x1}", BPC);
#endif
                            break;

                        case 0xc:   // WCSL
                            WriteControlStore(ControlStoreWord.Low, (ushort)_oldALURegisters.R);
                            break;

                        case 0xd:   // WCSM
                            WriteControlStore(ControlStoreWord.Middle, (ushort)_oldALURegisters.R);
                            break;

                        case 0xe:   // WCSH
                            WriteControlStore(ControlStoreWord.High, (ushort)_oldALURegisters.R);
                            break;

                        case 0xf:   // IOB function
                            if (uOp.F == 0x0)
                            {
                                // This is Input if the msb of Z is unset, Output otherwise
                                if (uOp.IsIOInput)
                                {
                                    // Input
                                    _iod = _ioBus.IORead(uOp.IOPort);
                                }
                                else
                                {
                                    // Output
                                    _ioBus.IOWrite(uOp.IOPort, _alu.Registers.R);
                                }
                            }
                            break;

                        default:
                            throw new UnimplementedInstructionException(
                                String.Format("Unimplemented Special Function {0:x1}", uOp.SF));
                    }
                    break;

                case 0x1:   // Store / Extended functions
                    switch (uOp.SF)
                    {
#if SIXTEEN_K
                        case 0x0:   // (R) := Victim Latch
                            _alu.SetR(_victim);
                            DoWriteBack(uOp);       // late writeback
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.OpFile, "Read from Victim latch {0:x4}", _victim);
#endif
                            _victim = 0xffff;       // do NOT set PC, but DO clear the latch
                            break;

                        case 0x1:   // Multiply / DivideStep
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.MulDiv, "MulDiv step: MQ in ={0:x5} R={1:x5} R<15>={2}",
                                                          _mq, _alu.Registers.R, ((_alu.Registers.R & 0x8000) >> 15));
#endif
                            //
                            // For the hardware assisted Multiply/Divide steps, we've already done the ALU op
                            // on the high word of the product or quotient during the ALU execution above;
                            // here we take care of the low word in the MQ register.
                            //
                            switch (_rasterOp.MulDivInst)
                            {
                                case MulDivCommand.Off:
                                    // Should never happen...
                                    throw new InvalidOperationException("MulDiv error: step SF while not enabled??");

                                case MulDivCommand.UnsignedDivide:
                                    //
                                    // For one division step:
                                    //  1. Shift MQ left one bit;
                                    //  2. Save the complement of the MSB from R into the LSB of MQ.
                                    //
                                    // Thar be dragons here:
                                    //   ONLY complement the LSB if the ALU op was an add or subtract; this covers
                                    //   the setup step where the initial left shift is done but no Q0 bit computation
                                    //   is needed.  The 16K ALU does this based on ArithX/ArithY, the MDINSTR bits
                                    //   and some PAL logic; here we're just cheating.  (It's a no-win; either we peek
                                    //   at the ALU op here, or the ALU has to reach into the MQ reg...)
                                    //
                                    if ((uOp.ALU == ALUOperation.AplusB) || (uOp.ALU == ALUOperation.AminusB))
                                    {
                                        _mqShifter.Shift(_mq);
                                        _mq = _mqShifter.ShifterOutput | (~((_alu.Registers.R & 0x8000) >> 15) & 0x1);
                                    }
#if TRACING_ENABLED
                                    else
                                    {
                                        if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv step: Q0 bit skipped");
                                    }
#endif
                                    break;

                                case MulDivCommand.UnsignedMultiply:
                                case MulDivCommand.SignedMultiply:
                                    //
                                    // For one multiplication step:
                                    //  1. Shift MQ right one bit;
                                    //  2. Save the LSB from the current R into the MSB of MQ.
                                    //
                                    _mqShifter.Shift(_mq);
                                    _mq = _mqShifter.ShifterOutput | ((_alu.Registers.R & 0x1) << 15);
                                    break;
                            }
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.MulDiv, "MulDiv step: MQ out={0:x5} MQ<0>={1}", _mq, (_mq & 0x1));
#endif
                            break;

                        case 0x2:   // Load multiplier / dividend
                            _mq = (_alu.Registers.R & 0xffff);      // MQ register is 16 bits wide
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv load: MQ={0:x4}", _mq);
#endif
                            break;

                        case 0x3:   // Load base register (not R)
                            _registerBase = (byte)(~_alu.Registers.R);
                            break;

                        case 0x4:   // (R) := product or quotient
                            _alu.SetR(_mq & 0xffff);
                            DoWriteBack(uOp);                       // late writeback
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv read: MQ={0:x4}", _mq);
#endif
                            break;

                        case 0x5:   // Push long constant
                            StackPush(uOp.LongConstant);
                            break;

                        case 0x6:   // Address input (MA) := Shift
                            _shifter.Shift(_oldALURegisters.R);

                            //
                            // We hack in the NIA value into the decoded (cached) Instruction's "Next" field.
                            // This should always work since the Next field's value is never normally used
                            // for this type of instruction.
                            //
                            uOp.NextAddress = (ushort)(_shifter.ShifterOutput & 0x3fff);
                            break;

                        case 0x7:   // Leap address generation
                                    // This is handled by CalcAddress
                            break;
#else
                        //
                        // In the original 4K CPU, these special functions don't exist and are ignored.
                        // Since some diagnostic software (and VFY 2.x) test for MQ to determine CPU
                        // type, we allow these ops to succeed (rather than throw an exception).  Any
                        // read or write of MQ simply returns zero.
                        //
                        case 0x2:   // Load multiplier / dividend
                            _mq = 0;
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv load ignored (4K)");
#endif
                            break;

                        case 0x4:   // (R) := product or quotient
                            _alu.SetR(_mq);
#if TRACING_ENABLED
                            if (Trace.TraceOn) Trace.Log(LogType.MulDiv, "MulDiv read ignored (4K)");
#endif
                            break;
#endif

                        case 0x8:   // Fetch4R
                        case 0x9:   // Store4R
                        case 0xa:   // Fetch4
                        case 0xb:   // Store4
                        case 0xc:   // Fetch2
                        case 0xd:   // Store2
                        case 0xe:   // Fetch
                        case 0xf:   // Store
#if TRACING_ENABLED
                            _memory.RequestMemoryCycle(_clocks, _alu.Registers.R, uOp.MemoryRequest);
#else
                            _memory.RequestMemoryCycle(_alu.Registers.R, uOp.MemoryRequest);
#endif
                            break;

                        default:
                            throw new UnimplementedInstructionException(
                                String.Format("Unimplemented Special Function {0:x1}", uOp.SF));
                    }
                    break;

                case 0x3:   // Long jump
                            // Handled in DispatchJump
                    break;

                default:
                    throw new UnimplementedInstructionException(
                        String.Format("Unimplemented Function {0:x1}", uOp.F));
            }
        }

        /// <summary>
        /// Jumps based on the condition flags.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void DispatchJump(Instruction uOp)
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
                    conditionSatisfied = _interruptFlag != InterruptType.None;
                    break;

                case Condition.Carry19:
                    conditionSatisfied = _oldALURegisters.Carry19 == 0;
                    break;

                case Condition.BPC3:
                    conditionSatisfied = OpFileEmpty;
                    break;

                case Condition.Odd:
                    conditionSatisfied = (_oldALURegisters.R & 0x1) != 0;
                    break;

                case Condition.ByteSign:
                    conditionSatisfied = (_oldALURegisters.R & 0x80) != 0;
                    break;

                case Condition.Neq:
                    conditionSatisfied = _oldALURegisters.Neq;
                    break;

                case Condition.Leq:
                    conditionSatisfied = _oldALURegisters.Leq;
                    break;

                case Condition.Lss:
                    conditionSatisfied = _oldALURegisters.Lss;
                    break;

                case Condition.Ovf:
                    conditionSatisfied = _oldALURegisters.Ovf;
                    break;

                case Condition.Carry15:
                    conditionSatisfied = _oldALURegisters.Cry;
                    break;

                case Condition.Eql:
                    conditionSatisfied = _oldALURegisters.Eql;
                    break;

                case Condition.Gtr:
                    conditionSatisfied = _oldALURegisters.Gtr;
                    break;

                case Condition.Geq:
                    conditionSatisfied = _oldALURegisters.Geq;
                    break;

                default:
                    throw new UnimplementedInstructionException(String.Format("Unimplemented Condition {0}", uOp.CND));
            }

            // Dispatch jump action
            switch (uOp.JMP)
            {
                case JumpOperation.JumpZero:
                    _pc.Value = 0;
                    _callStack.Reset();
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

                case JumpOperation.NextInstReviveVictim:
                    if (uOp.H == 0)
                    {
                        DoNextInst(uOp);
                    }
                    else
                    {
                        // Revive the victim; unset victim latch
                        if (_victim == 0xffff)
                        {
                            throw new InvalidOperationException("Revive from unset victim latch!");
                        }
#if SIXTEEN_K
                        // On 16K CPU, Victim restores all 14 bits -- according to VFY 2.0!?
                        _pc.Value = _victim;
#else
                        // Only the low 12 bits are changed
                        _pc.Lo = _victim;
#endif
#if TRACING_ENABLED
                        if (Trace.TraceOn) Trace.Log(LogType.CpuState, "PC restored from victim ({0:x4})", _victim);
#endif
                        _victim = 0xffff;
                    }
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

                case JumpOperation.PushLoad:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.CpuState, "PushLoad: cond={0} S={1:x4}", conditionSatisfied, uOp.NextAddress);
#endif
                    if (conditionSatisfied)
                    {
                        _s.Value = uOp.NextAddress;
                        _pc.Lo++;
                        _callStack.PushLo(_pc.Lo);
                    }
                    else
                    {
                        _pc.Lo++;
                        _callStack.PushLo(_pc.Lo);
                    }
                    break;

                case JumpOperation.CallS:
                    _callStack.PushFull((ushort)(_pc.Value + 1));
                    _pc.Value = conditionSatisfied ? uOp.NextAddress : _s.Value;
                    break;

                case JumpOperation.VectorDispatch:
                    if (conditionSatisfied)
                    {
                        if (uOp.H == 0)
                        {
                            // Vector
                            _pc.Lo = (ushort)((uOp.VectorDispatchAddress & 0xffc3) | (InterruptPriority() << 2));
                        }
                        else
                        {
                            // Dispatch
                            _shifter.Shift(_oldALURegisters.R);
                            _pc.Lo = (ushort)((uOp.VectorDispatchAddress & 0xffc3) | (((~_shifter.ShifterOutput) & 0xf) << 2));
                        }
#if TRACING_ENABLED
                        if (Trace.TraceOn) Trace.Log(LogType.CpuState, "Dispatch to {0:x4}", _pc);
#endif
                    }
                    else
                    {
                        _pc.Lo++;
                    }
                    break;

                case JumpOperation.GotoS:
                    _pc.Value = conditionSatisfied ? uOp.NextAddress : _s.Value;
                    break;

                case JumpOperation.RepeatLoop:
                    if (_s.Lo != 0)
                    {
                        _pc.Lo = _callStack.TopLo();
                        _s.Lo--;
                    }
                    else
                    {
                        _pc.Lo++;
                        _callStack.PopLo();
                    }
                    break;

                case JumpOperation.Repeat:
                    if (_s.Lo != 0)
                    {
#if TRACING_ENABLED
                        if (Trace.TraceOn) Trace.Log(LogType.CpuState, "Repeat S={0:x4}", _s);
#endif
                        _pc.Lo = uOp.NextAddress;
                        _s.Lo--;
                    }
                    else
                    {
#if TRACING_ENABLED
                        if (Trace.TraceOn) Trace.Log(LogType.CpuState, "Repeat done.", _s);
#endif
                        _pc.Lo++;
                    }
                    break;

                case JumpOperation.Return:
                    if (conditionSatisfied)
                    {
                        _pc.Value = _callStack.PopFull();
                    }
                    else
                    {
                        _pc.Lo++;
                    }
                    break;

                case JumpOperation.JumpPop:
                    if (conditionSatisfied)
                    {
                        if (uOp.H != 0)
                        {
                            _callStack.PopFull();       // LeapPop (16K only?)
                        }
                        else
                        {
                            _callStack.PopLo();
                        }
                        _pc.Value = uOp.NextAddress;
                    }
                    else
                    {
                        _pc.Lo++;
                    }
                    break;

                case JumpOperation.LoadS:
                    _s.Value = uOp.NextAddress;
                    _pc.Lo++;
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.CpuState, "Load S={0:x4}", _s);
#endif
                    break;

                case JumpOperation.Loop:
                    if (conditionSatisfied)
                    {
                        _pc.Lo++;
                        _callStack.PopLo();  // ?
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
                            _pc.Value = uOp.NextAddress;
                            _callStack.PopLo();
                        }
                    }
                    break;

                default:
                    throw new UnimplementedInstructionException(String.Format("Unhandled Jump type {0}", uOp.JMP));
            }
        }

        /// <summary>
        /// Writes a 16-bit subword of a 48-bit microcode instruction to the control store.
        /// </summary>
        private void WriteControlStore(ControlStoreWord word, ushort data)
        {
            // Get the current microcode word
            ulong cs = _microcode[_s.Value];

            _microcode[_s.Value] = UnscrambleControlStoreWord(cs, word, data);

            // Invalidate cache for this instruction
            _microcodeCache[_s.Value] = null;

            // Set the hold bit to inject a wait state
            _wcsHold = true;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MicrocodeStore,
                    "Wrote {0},{1:x4} at control store {2:x4} -- now contains {3:x12}",
                    word, data, _s.Value, _microcode[_s.Value]);
#endif
        }

        /// <summary>
        /// Unscrambles a control store word.  This is complicated by the fact that
        /// the bit mapping from the ALU output to the controlstore is all scrambled up.
        ///
        /// This is not a very efficient routine.  But it doesn't get executed very frequently
        /// (usually only at OS load) so I'm going to leave it alone for now.
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
                    current = (ulong)((current & 0x0000ffffffff) | (ulong)(data) << 32);
                    break;
            }

            return current;
        }

        /// <summary>
        /// Sets a series of bits in the 48-bit destination word from a 16-bit source word.
        /// </summary>
        private ulong SetBits(ulong sourceWord, ulong destWord, int[] destBits)
        {
            for (int sourceBit = 0; sourceBit < destBits.Length; sourceBit++)
            {
                // Get the bit value from the source word
                ulong bitValue = (sourceWord >> sourceBit) & 0x1;

                // Frob that into the destination word at its magical location
                destWord = (destWord & ~((ulong)0x1 << destBits[sourceBit])) | ((ulong)bitValue << destBits[sourceBit]);
            }

            return destWord;
        }

        /// <summary>
        /// Increment the BPC and update the program counter.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void DoNextInst(Instruction uOp)
        {
            byte next = _memory.OpFile[BPC];

#if TRACING_ENABLED
            Trace.TraceLevel &= (~LogType.Instruction);
            if (Trace.TraceOn)
                Trace.Log(LogType.QCode, "NextInst is {0:x2}-{1} at BPC {2:x1}", next,
                    QCode.QCodeHelper.GetQCodeFromOpCode(next).Mnemonic, BPC);
#endif
#if SIXTEEN_K
            _pc.Lo = (ushort)(Instruction.ZOpFill(uOp.NotZ) | ((~next & 0xff) << 2));
#else
            _pc.Value = (ushort)(Instruction.ZOpFill(uOp.NotZ) | ((~next & 0xff) << 2));
#endif
#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.OpFile, "NextInst Branch to {0:x4} ", PC);
#endif
            _incrementBPC = true;
        }

        /// <summary>
        /// Implements the behavior of the CPU's interrupt priority encoder.
        /// </summary>
        private int InterruptPriority()
        {
            if (_interruptFlag != 0)
            {
                for (int i = 7; i >= 0; i--)
                {
                    if ((((int)_interruptFlag) & ((0x1) << i)) != 0)
                    {
                        return i;
                    }
                }
            }

            return 0;
        }

#endregion CPU Helper Functions

#region Boot ROM functions

        /// <summary>
        /// Loads the boot ROM from a file on disk and descrambles it.
        /// This currently expects a ROM built by PRQMIC, from microcode
        /// sources, not an actual ROM dump from the PERQ.
        /// </summary>
        private void LoadBootRom(string path)
        {
            FileStream fs = new FileStream(path, FileMode.Open);

            // Read all 512 scrambled instruction words in...
            for (ushort i = 0; i < 512; i++)
            {
                ushort addr = 0;
                ulong word = ReadMicrocodeWord(fs, out addr);

                // The only address outside of range should be the last (which has addr 0xffff)
                if (addr < 512)
                {
                    _rom[addr] = word;
                }
            }
            fs.Close();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.EmuState, "Read boot ROM from {0}.", path);
#endif
        }

        /// <summary>
        /// Reads a 48-bit word in from the given stream.
        /// </summary>
        private ulong ReadMicrocodeWord(FileStream fs, out ushort addr)
        {
            ulong word = 0;

            // Read the address, low bits first
            addr = (ushort)(fs.ReadByte());
            addr = (ushort)((addr | ((ushort)fs.ReadByte() << 8)));

            // Read the instruction one byte at a time, low bits first
            word = (ulong)(fs.ReadByte());
            word = (word | ((ulong)(fs.ReadByte()) << 8));
            word = (word | ((ulong)(fs.ReadByte()) << 16));
            word = (word | ((ulong)(fs.ReadByte()) << 24));
            word = (word | ((ulong)(fs.ReadByte()) << 32));
            word = (word | ((ulong)(fs.ReadByte()) << 40));

            return word;
        }

#endregion Boot ROM functions

#region Special Function Helpers

        /// <summary>
        /// Reset the expression stack (EStack).
        /// </summary>
        private void StackReset()
        {
            _stackPointer = 0;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.EStack, "EStack: Reset.");
#endif
            // Resetting the stack increments the diagnostic display
            IncrementDDS();
        }

        /// <summary>
        /// Push a 20-bit value onto the EStack.
        /// </summary>
        private void StackPush(int value)
        {
            _stackPointer++;

            // Check for stack overflow.  According to the PERQ documentation, behavior
            // in this state is undefined.  However, _real_ code seems to be buggy (i.e.
            // leaks pops/pushes) and depends on the EStack pointer wrapping around.
            // (For example, PERQMan's random number generator microcode does one too few
            // pops before returning...)
            if (_stackPointer > 15)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.Errors, "EStack overflow!");
#endif
                _stackPointer = 0;
            }

            _stack[_stackPointer] = value;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.EStack, "Estack: Pushed {0:x5}, pointer now {1}", value, _stackPointer);
#endif
        }

        /// <summary>
        /// Pop a 20-bit value from the EStack.
        /// </summary>
        private void StackPop()
        {
            _stackPointer--;

            // Check for stack underflow.  According to the PERQ documentation, behavior
            // in this state is undefined.  See StackPush().
            if (_stackPointer < 0)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.Errors, "EStack underflow!");
#endif
                _stackPointer = 15;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.EStack, "EStack: Popped, pointer now {0}", _stackPointer);
#endif
        }

        /// <summary>
        /// Increments the DDS (diagnostic counter)
        /// </summary>
        private void IncrementDDS()
        {
            _dds++;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.DDS, "DDS is now {0:d3}", _dds % 1000);
#endif

            // TODO: This should be moved elsewhere
            Console.Title = String.Format("DDS {0:d3}", _dds % 1000);
        }

#endregion Special Function Helpers

#region Debugger Commands

        [DebugFunction("disassemble microcode", "Disassembles microcode instructions from the WCS (@ current PC)")]
        private void DisassembleMicrocode()
        {
            DisassembleMicrocode(_pc.Value, 16);
        }

        [DebugFunction("disassemble microcode", "Disassembles microcode instructions from the WCS (@ [addr])")]
        private void DisassembleMicrocode(ushort startAddress)
        {
            DisassembleMicrocode(startAddress, 16);
        }

        [DebugFunction("disassemble microcode", "Disassembles microcode instructions from the WCS (@ [addr, len])")]
        private void DisassembleMicrocode(ushort startAddress, ushort length)
        {
            ushort endAddr = (ushort)(startAddress + length);

            if (startAddress >= _microcode.Length || endAddr >= _microcode.Length)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and {0}", _microcode.Length);
                return;
            }

            // Disassemble microcode
            for (ushort i = startAddress; i < endAddr; i++)
            {
                string line = Disassembler.Disassemble(i, GetInstruction(i));
                Console.WriteLine(line);
            }
        }

        [DebugFunction("load microcode", "Loads the specified microcode file into the PERQ's writable control store")]
        private void LoadMicrocode(string ucodeFile)
        {
            try
            {
                FileStream fs = new FileStream(ucodeFile, FileMode.Open);
                bool done = false;

                // Read instruction words in...
                while (!done)
                {
                    ushort addr = 0;
                    ulong word = ReadMicrocodeWord(fs, out addr);

                    // The only address outside of range should be the last (which has addr 0xffff)
                    if (addr != 0xffff)
                    {
                        Console.Write("{0:x} ", addr);
                        _microcode[addr] = word;
                        _microcodeCache[addr] = null;
                    }
                    else
                    {
                        done = true;
                    }
                }
                fs.Close();

                _romEnabled = false;

                Console.WriteLine("\nMicrocode loaded, boot ROM disabled.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load microcode from {0}, error: {1}", ucodeFile, e.Message);
            }
        }

        [DebugFunction("jump", "Set next microinstruction address")]
        private void JumpTo(ushort nextPC)
        {
            if (nextPC < 0 || nextPC >= _microcode.Length)
            {
                Console.WriteLine("Address outside of range 0..{0}; PC not modified.", _microcode.Length - 1);
            }
            else
            {
                _pc.Value = nextPC;
            }
        }

        [DebugFunction("show cpu registers", "Displays the values of the CPU registers")]
        public void ShowPC()
        {
            Console.WriteLine("\nPC={0:x4} BPC={1:x1} Victim={2:x4} DDS={3} UState={4:x2} Interrupt={5}",
                              PC, BPC, Victim, DDS, MicrostateRegister, InterruptFlag);
        }

        [DebugFunction("show xy register", "Displays the values of the XY registers")]
        private void ShowRegister()
        {
            for (int reg = 0; reg < 256; reg++)
            {
                Console.Write("R[{0:x2}]={1:x5}{2}", reg, _r[reg],
                              ((reg + 1) % 4 == 0) ? "\n" : "\t");
            }
            Console.WriteLine();
        }

        [DebugFunction("show xy register", "Displays the value of the specified XY register [0..255]")]
        private void ShowRegister(ushort reg)
        {
            if (reg > 255)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and 255");
                return;
            }
            Console.WriteLine("R[{0:x2}]={1:x5}", reg, _r[reg]);
        }

#if DEBUG
        [DebugFunction("set xy register", "Change the value of an XY register")]
        private void SetRegister(ushort reg, uint val)
        {
            if (reg > 255)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and 255");
                return;
            }
            _r[reg] = (int)(val & 0xffff);     // clip to 20 bits

            Console.WriteLine("R[{0:x2}]={1:x5} (dec {2})", reg, _r[reg], _r[reg]);
        }
#endif

        [DebugFunction("show memory", "Dumps the PERQ's memory (@ [addr])")]
        private void ShowMemory(uint startAddress)
        {
            ShowMemory(startAddress, 64);
        }

        [DebugFunction("show memory", "Dumps the PERQ's memory (@ [addr, len])")]
        private void ShowMemory(uint startAddress, uint length)
        {
            uint endAddr = (uint)(startAddress + length);

            if (startAddress >= _memory.MemSize || endAddr - 8 >= _memory.MemSize)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and {0}", _memory.MemSize - 1);
                return;
            }

            ushort[] mem = _memory.Memory;

            // Format and display 8 words per line
            for (uint i = startAddress; i < endAddr; i += 8)
            {
                StringBuilder line = new StringBuilder();

                line.AppendFormat("{0:x5}: {1:x4} {2:x4} {3:x4} {4:x4} {5:x4} {6:x4} {7:x4} {8:x4} ",
                    i, mem[i], mem[i + 1], mem[i + 2], mem[i + 3], mem[i + 4], mem[i + 5], mem[i + 6], mem[i + 7]);

                for (uint j = i; j < i + 8; j++)
                {
                    // Build ascii representation...
                    char high = (char)((mem[j] & 0xff00) >> 8);
                    char low = (char)((mem[j] & 0xff));

                    if (!Debugger.Debugger.IsPrintable(high))
                    {
                        high = '.';
                    }

                    if (!Debugger.Debugger.IsPrintable(low))
                    {
                        low = '.';
                    }

                    line.AppendFormat("{0}{1}", low, high);     // Reversed so strings read right :)
                }

                Console.WriteLine(line);
            }
        }

#if DEBUG
        [DebugFunction("find memory", "Find a specific value in the PERQ's memory")]
        private void FindMemory(ushort val)
        {
            FindMemory(0, val);
        }

        [DebugFunction("find memory", "Find a specific value in the PERQ's memory [@start, val]")]
        private void FindMemory(uint startAddress, ushort val)
        {
            if (startAddress >= _memory.MemSize)
            {
                Console.WriteLine("Argument out of range -- start address must be between 0 and {0}", _memory.MemSize - 1);
                return;
            }

            ushort[] mem = _memory.Memory;

            for (uint i = startAddress; i < _memory.MemSize; i++)
            {
                if (mem[i] == val) ShowMemory((i & 0xffffc), 4);        // show the quadword
            }
        }

        [DebugFunction("set memory", "Write a specific value in the PERQ's memory")]
        private void SetMemory(uint address, ushort val)
        {
            if (address >= _memory.MemSize)
            {
                Console.WriteLine("Argument out of range -- start address must be between 0 and {0}", _memory.MemSize - 1);
                return;
            }

            _memory.StoreWord((int)address, val);
        }

        [DebugFunction("show memstate", "Dump the memory controller state")]
        private void ShowMemQueues()
        {
            _system.MemoryBoard.DumpQueues();
        }
#endif

        [DebugFunction("show opfile", "Displays the contents of the opcode file")]
        private void ShowOpfile()
        {
            Console.WriteLine("BPC={0}. Opfile Contents:", BPC);

            for (int i = 0; i < 8; i++)
            {
                Console.WriteLine("{0}: {1:x2}", i, _system.MemoryBoard.OpFile[i]);
            }
        }

        [DebugFunction("show estack", "Displays the contents of the expression stack")]
        private void ShowEStack()
        {
            Console.WriteLine("EStack Pointer={0}.  Contents:", _stackPointer);

            for (int i = 0; i < 16; i++)
            {
                Console.WriteLine("{0} {1:00}: {2:x5}", (i == _stackPointer ? "=>" : "  "), i, _stack[i]);
            }
        }

        [DebugFunction("show cstack", "Displays the contents of the 2910 call stack")]
        private void ShowCStack()
        {
            _callStack.DumpContents();
        }

#if DEBUG
        [DebugFunction("set rasterop debug", "Enable extended RasterOp debugging")]
        private void SetRopDebug()
        {
            _rasterOp.Debug = true;
        }

        [DebugFunction("set rasterop logging", "Enable extended RasterOp logging")]
        private void SetRopLogging()
        {
#if TRACING_ENABLED
            // Bundled all these for convenience...
            Trace.TraceLevel |= (LogType.RasterOp | LogType.MemoryState | LogType.MemoryFetch | LogType.MemoryStore);
#endif
        }

        [DebugFunction("clear rasterop debug", "Disable extended RasterOp debugging")]
        private void ClearRopDebug()
        {
            _rasterOp.Debug = false;
        }

        [DebugFunction("clear rasterop logging", "Disable extended RasterOp logging")]
        private void ClearRopLogging()
        {
#if TRACING_ENABLED
            Trace.TraceLevel &= ~(LogType.RasterOp | LogType.MemoryState | LogType.MemoryFetch | LogType.MemoryStore);
#endif
        }

        [DebugFunction("show rasterop state", "Display current RasterOp unit status")]
        private void ShowRopState()
        {
            _rasterOp.ShowState();
        }

        [DebugFunction("show rasterop fifos", "Display current RasterOp source & destination FIFOs")]
        private void ShowRopFifos()
        {
            _rasterOp.ShowFifos();
        }

        [DebugFunction("show rasterop registers", "Display current RasterOp register contents")]
        private void ShowRopRegs()
        {
            _rasterOp.ShowRegs();
        }

        [DebugFunction("show z80 state", "Display current Z80 device ready state")]
        private void ShowZ80State()
        {
            Console.WriteLine("Z80 clock: {0}", ((Z80System)(_system.IOB.Z80System)).Clocks());
            //_system.IOB.Z80System.ShowReadyState();
        }
#endif
        #endregion


#if TRACING_ENABLED
        // Trace/debugging support
        private ushort _lastPC;
        private Instruction _lastInstruction;

        
#endif
        // CPU cycles elapsed
        private long _clocks;

#if SIXTEEN_K
        // Microcode store, 16k 48-bit words
        private ulong[] _microcode = new ulong[16384];

        // Decoded microcode cache
        private Instruction[] _microcodeCache = new Instruction[16384];
#else
        // Microcode store, 4k 48-bit words
        private ulong[] _microcode = new ulong[4096];

        // Decoded microcode cache
        private Instruction[] _microcodeCache = new Instruction[4096];
#endif

        // Microcode ROM, 512 48-bit words
        private ulong[] _rom = new ulong[512];

        // ROM enable latch
        private bool _romEnabled;

        // Wait state flag when loading WCS
        private bool _wcsHold;

        // Microcode address
        private ExtendedRegister _pc;

        // Victim latch
        private ushort _victim;

        // 2910 S register
        private ExtendedRegister _s;

        // 2910 call stack (actually belongs to the AMD microcode sequencer,
        // but that's sorta been conflated into this class...)
        private CallStack _callStack = new CallStack();

        // Expression stack
        private int[] _stack = new int[16];
        private int _stackPointer;

        // XY registers
        private int[] _r = new int[256];

#if SIXTEEN_K
        // Base register
        private byte _registerBase;
#endif

        // Byte Program Counter
        private int _bpc;
        private bool _incrementBPC;

        // Diagnostic counter
        private int _dds;

        // Interrupt flags
        private InterruptType _interruptFlag;

        // Arithmetic unit
        private ALU _alu;

        // Copy of last ALU register set
        private ALURegisterFile _oldALURegisters;

        // Copy of Bmux bits in microstate register
        // for mysterious, if not nefarious purposes
        private int _lastBmux;

        // Shifter
        private Shifter _shifter;

        // RasterOp
        private RasterOp _rasterOp;

        // Multiplier Quotient register ("MQ") only exists in the 16K CPU, but some
        // microcode (VFY 2.x, e.g.) tries to write/read from MQ to test for WCS size
        // on the fly.  So we define MQ for both CPU types, but essentially treat it
        // as read-only in the 4K variant.
        private int _mq;
#if SIXTEEN_K
        private Shifter _mqShifter;
        private bool _mqEnabled;
#endif

        // Memory card reference
        private MemoryBoard _memory;

        // IO data
        private int _iod;

        // IO bus reference
        private IOBus _ioBus;

        private PERQSystem _system;
    }
}
