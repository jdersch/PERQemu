//
// cpu.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Memory;
using PERQemu.Debugger;

namespace PERQemu.Processor
{

    /// <summary>
    /// PERQ hardware interrupts, listed in order of priority.
    /// </summary>
    // TODO: separate these so they can be raised/cleared without contention!
    // (possibly removing the need for write locks entirely)
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
    /// Implements the PERQ's custom microengine.
    /// </summary>
    public partial class CPU
    {
        static CPU()
        {
            // Our derived classes customize themselves in the static
            // constructor, setting various word sizes, masks, etc.
        }

        public CPU(PERQSystem system)
        {
            _system = system;
            _memory = _system.Memory;

            // Create CPU components
            _ustore = new ControlStore();
            _usequencer = new Sequencer(this);

            _alu = new ALU();
            _xy = new RegisterFile();
            _estack = new ExpressionStack();

            _shifter = new Shifter();
            _rasterOp = new RasterOp(_memory);

            _mqShifter = new Shifter();

            _intLock = new object();
        }

        /// <summary>
        /// Reset the CPU, clear the control store and re-enable the boot ROM.
        /// </summary>
        public void Reset()
        {
            // Reset the major components
            _xy.Reset();
            _alu.Reset();
            _estack.Reset();
            _ustore.Reset();
            _rasterOp.Reset();
            _usequencer.Reset();

            // Clear the op file
            for (int i = 0; i < 16; i++)
            {
                _opFile[i] = 0xff;
            }

            // Reset the rest
            _dds = 0;
            _bpc = 0;
            _iod = 0;
            _clocks = 0;
            _lastPC = 0;
            _lastBmux = 0;
            _refillOp = false;
            _incrementBPC = false;
            _interruptFlag = InterruptType.None;

            _mq = 0;
            _mqEnabled = false;

            Trace.Log(LogType.CpuState, "CPU: Reset.");
        }


        /// <summary>
        /// Runs the PERQ microengine for one microcycle.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Execute()
        {
            _clocks++;

            // Decode the next instruction
            Instruction uOp = _ustore.GetInstruction(_usequencer.PC);

            // Clock the memory state machine and set up any pending fetches
            _memory.Tick(uOp.MemoryRequest);

            // If a refill is in progress, load the next word (even if we abort
            // the current instruction below)
            if (_refillOp)
            {
                LoadOpWord();
            }

            //
            // Several conditions may stall the processor:
            //  1.  In the T0 & T1 after any Fetch, attempting to access MDI
            //      or MDX causes the processor to stall, but other instructions
            //      may run in those cycles;
            //  2.  The memory system may assert a "hold" if the IO or DMA is
            //      accessing memory; we don't actually enforce that, yet, but
            //      the "wait" may be set for stores issued in the wrong cycle;
            //  3.  WCS writes actually take two cycles to execute; we fake a
            //      wait state after any control store writes.
            //
            // If any of those conditions are true we "abort" the current
            // instruction until the correct cycle comes around.  We cannot
            // cheat and skip over aborts, as RasterOp depends on T-state
            // cycling through each word during overlapped fetch/stores.
            //
            if (_ustore.Hold || _memory.Wait || (uOp.WantMDI && !_memory.MDIValid))
            {
                // Waiting for the next T3 or T2 cycle to come around on the guitar
                Trace.Log(LogType.MemoryState,
                    "CPU: Abort in T{0}\n\twait={1} needMDO={2} wantMDI={3} MDIvalid={4} WCShold={5}",
                    _memory.TState, _memory.Wait, _memory.MDONeeded, uOp.WantMDI, _memory.MDIValid, _ustore.Hold);

                // On aborts, no memory writes occur - no Tock()                    

                // WCS writes take two cycles to run on the hardware.  If the
                // last op wrote the WCS, clear the hold flag to continue
                _ustore.Hold = false;

                return;
            }

#if TRACING_ENABLED
            // This is a very expensive log, so only call it if we have to
            if (Trace.TraceOn && (LogType.Instruction & Trace.TraceLevel) != 0)
            {
                Trace.Log(LogType.Instruction, "uPC={0:x4}: {1}", PC, Disassembler.Disassemble(PC, uOp));
            }
#endif
#if DEBUG
            // Catch cases where the CPU is looping forever
            if (_lastPC == PC &&
                uOp.CND == Condition.True &&
                uOp.JMP == JumpOperation.Goto)
            {
                throw new UnimplementedInstructionException(string.Format("CPU has halted in a loop at {0:x4}", PC));
            }
#endif

            // If the last instruction was NextOp or NextInst, increment
            // BPC at the start of this instruction
            if (_incrementBPC)
            {
                _bpc++;
                _incrementBPC = false;

                Trace.Log(LogType.OpFile, "OpFile: BPC incremented to {0:x1}", BPC);
            }

            // Latch the ALU result and flags from the last micro-op before we
            // do this instruction's op, to test for conditional jumps later.
            _alu.LatchResult();

            //
            // Execute the current instruction
            //

            // Select ALU inputs
            int bmux = _lastBmux = GetBmuxInput(uOp);
            int amux = GetAmuxInput(uOp);

            // If the hardware multiply unit is enabled, pass the MQ register
            // to the ALU (where it may be modified); otherwise, run a normal op.
            if (_mqEnabled)
            {
                _alu.Execute(uOp.ALU, amux, bmux, _rasterOp.MulDivInst, _mq);
            }
            else
            {
                _alu.Execute(uOp.ALU, amux, bmux);
            }

            // Do writeback if W bit is set
            if (uOp.W == 1)
            {
                _xy.WriteRegister(uOp.X, _alu.R.Value);
            }

            // If enabled, clock the RasterOp pipeline
            if (_rasterOp.Enabled)
            {
                _rasterOp.Clock();
            }

            // Is a memory store operation in progress?  Pending RasterOp stores
            // supercede the ALU; otherwise write the last ALU result
            if (_memory.MDONeeded)
            {
                if (_rasterOp.ResultReady)
                {
                    _memory.Tock(_rasterOp.Result());
                }
                else
                {
                    _memory.Tock(_alu.R.Lo);
                }
            }

            // Execute whatever function this op calls for
            DispatchFunction(uOp);

            // Save these for posterity
            _lastPC = PC;
            _lastInstruction = uOp;

            // Jump to where we need to go...
            _usequencer.DispatchJump(uOp);
        }


        #region Properties

        public static string Name => _name;
        public static string Description => _desc;
        public static int CPUBits => _bits;
        public static int CPUMask => _mask;
        public static int WCSBits => _wcsBits;
        public static int WCSSize => _wcsSize;
        public static int WCSMask => _wcsMask;
        public static ulong MicroCycleTime => _cycleTime;

        public static bool Is4K
        {
            get { return (_wcsSize == 4096); }
        }

        public ulong Clocks
        {
            get { return _clocks; }
        }

        public RasterOp RasterOp
        {
            get { return _rasterOp; }
        }

        public bool OpFileEmpty
        {
            get { return (BPC & 0x8) != 0; }
        }

        public bool IncrementBPC
        {
            get { return _incrementBPC; }
        }

        public ulong[] Microcode
        {
        	get { return _ustore.Microcode; }
        }

        #endregion

        #region Debugger properties
        /// <summary>
        /// Returns the current DDS value.  (Not really a register, but useful! :-)
        /// </summary>
        [DebugProperty("dds")]
        public int DDS
        {
            get { return _dds; }
        }

        /// <summary>
        /// Returns the OpFile contents.
        /// </summary>
        public byte[] OpFile
        {
            get { return _opFile; }
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
        /// The 2910's S Register.
        /// </summary>
        [DebugProperty("s")]
        public ushort S
        {
            get { return _usequencer.S; }
        }

        /// <summary>
        /// The Victim Latch.
        /// </summary>
        [DebugProperty("victim")]
        public ushort Victim
        {
            get { return _usequencer.Victim; }
        }

        /// <summary>
        /// The microstate register (20-bit).
        /// </summary>
        [DebugProperty("ustate")]
        public int Microstate
        {
            get { return ReadMicrostateRegister(0); }
        }

        /// <summary>
        /// On 24-bit CPUs, reads the Upper register (Microstate with H=1).
        /// Results on 20-bit CPUs are undefined.  This isn't ideal.
        /// </summary>
        [DebugProperty("upper")]
        public int Upper
        {
            get { return ReadMicrostateRegister(1); }
        }

        /// <summary>
        /// The uState register(s).
        /// </summary>
        /// <remarks>
        /// The H bit on the 24 bit CPU selects a second uState register, which
        /// is aka "Upper"; in that processor _lastbmux (bits 12:15) are always
        /// zero.  Note that we also don't implement the CCSR0 PAL here either,
        /// but I suspect only a few *really* esoteric diagnostics ever used that
        /// functionality, and Tony alludes to a possible hardware bug that made
        /// it not work on the 4k CPU anyway.  This is off into the serious
        /// periphery of PERQ esoterica.
        /// 
        /// FYI: can't attach the DebugProperty to a virtual method, so we add
        /// those separately above.  Mild, as hackish workarounds go.
        /// </remarks>
        public virtual int ReadMicrostateRegister(byte h)
        {
            // On the 20-bit CPUs, there's only one microstate register:
            return BPC |
                    (_alu.Flags.Ovf ? 0x0010 : 0x0) |
                    (_alu.Flags.Eql ? 0x0020 : 0x0) |
                    (_alu.Flags.Cry ? 0x0040 : 0x0) |
                    (_alu.Flags.Lss ? 0x0080 : 0x0) |
                    (_estack.StackEmpty ? 0x0 : 0x0200) |   // inverted!
                    ((((~_lastBmux) >> 16) & 0xf) << 12);
        }

        /// <summary>
        /// The Microcode PC, either 12 or 14 bits depending on the CPU.
        /// </summary>
        [DebugProperty("pc")]
        public ushort PC
        {
            get { return (ushort)_usequencer.PC; }
            set { _usequencer.PC = value; }
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
        public void RaiseInterrupt(InterruptType i)
        {
            lock (_intLock)
            {
                _interruptFlag |= i;
            }

            Trace.Log(LogType.Interrupt, "Interrupt {0} raised, active now {1}", i, _interruptFlag);
        }

        /// <summary>
        /// Clears the specified interrupt, if set.
        /// </summary>
        public void ClearInterrupt(InterruptType i)
        {
            lock (_intLock)
            {
                _interruptFlag &= ~i;
            }

#if TRACING_ENABLED
            if ((_interruptFlag & ~i) != _interruptFlag)
            {
                Trace.Log(LogType.Interrupt, "Interrupt {0} cleared, active now {1}", i, _interruptFlag & ~i);
            }
#endif
        }

        /// <summary>
        /// The ALU's last result register "R".
        /// </summary>
        [DebugProperty("r")]
        public int R
        {
            get { return _alu.R.Value; }
        }

        /// <summary>
        /// The base register for indexing the XY registers.
        /// </summary>
        [DebugProperty("rbase")]
        public byte RegisterBase
        {
            get { return _xy.RegisterBase; }
        }

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

        #endregion

        #region CPU Helper functions

        /// <summary>
        /// Selects the proper AMUX input for the specified instruction
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private int GetAmuxInput(Instruction uOp)
        {
            int amux = 0;

            // Select AMUX input
            switch (uOp.A)
            {
                case AField.Shifter:
                    _shifter.Shift(_alu.OldR.Lo);       // Shifter takes 16 bits of R
                    amux = _shifter.ShifterOutput;
                    break;

                case AField.NextOp:
                    if (OpFileEmpty)
                    {
                        // Only latch if Victim is empty (all ones indicates an unset Victim...)
                        if (_usequencer.Victim == _wcsMask)
                        {
                            _usequencer.Victim = _usequencer.PC;    // 12 or 14 bits

                            Trace.Log(LogType.OpFile, "Victim register is now {0:x4}", _usequencer.Victim);
                        }
                    }

                    amux = _opFile[BPC];
                    _incrementBPC = true;   // Increment BPC at the beginning of the next instruction

                    Trace.Log(LogType.QCode, "NextOp read from BPC[{0:x1}]={1:x2}", BPC, amux);
                    break;

                case AField.IOD:
                    amux = _iod;
                    break;

                case AField.MDI:
                    amux = _memory.MDI;
                    break;

                case AField.MDX:
                    amux = (_memory.MDI & (_bits == 24 ? 0x00ff : 0x000f)) << 16;
                    break;

                case AField.UState:
                    amux = ReadMicrostateRegister(uOp.H);
                    break;

                case AField.XYRegister:
                    amux = _xy.ReadRegister(uOp.X);
                    break;

                case AField.TOS:
                    amux = _estack.TOS;
                    break;

                default:
                    throw new UnimplementedInstructionException(string.Format("Unimplemented AMUX {0}", uOp.A));
            }

            return amux;
        }

        /// <summary>
        /// Selects the proper BMUX input for the specified instruction.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private int GetBmuxInput(Instruction uOp)
        {
            int bmux = 0;

            // Select BMUX input
            if (uOp.B == 0)
            {
                bmux = _xy.ReadRegister(uOp.Y);
            }
            else
            {
                bmux = uOp.BMuxInput;
            }

            return bmux;
        }

        /// <summary>
        /// Dispatches function and special function operations based on the instruction
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void DispatchFunction(Instruction uOp)
        {
            switch (uOp.F)
            {
                case 0x0:
                case 0x2:   // Special Functions

                    // Calculate shifter output when F=2
                    if (uOp.F == 0x2)
                    {
                        Trace.Log(LogType.Shifter, "ShiftOnZ");
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
                                Trace.Log(LogType.Shifter, "ShiftOnR");
                                _shifter.SetShifterCommand(~(_alu.R.Value));
                            }
                            break;

                        case 0x2:   // StackReset
                            _estack.StackReset();
                            IncrementDDS();
                            break;

                        case 0x3:   // TOS := (R)
                            _estack.TOS = _alu.R.Value;
                            break;

                        case 0x4:   // Push
                            _estack.Push(_alu.R.Value);
                            break;

                        case 0x5:   // Pop
                            _estack.Pop();
                            break;

                        case 0x6:   // CntlRasterOp := (Z)
                            _rasterOp.CntlRasterOp(uOp.Z);
                            break;

                        case 0x7:   // SrcRasterOp := (R)
                            _rasterOp.SrcRasterOp(_alu.R.Lo);
                            break;

                        case 0x8:   // DstRasterOp := (R)
                            _rasterOp.DstRasterOp(_alu.R.Lo);
                            break;

                        case 0x9:   // WidRasterOp := (R)
                            _rasterOp.WidRasterOp(_alu.R.Lo);

                            if (!Is4K)
                            {
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
                                        if (_mqEnabled)
                                            Trace.Log(LogType.MulDiv, "MulDiv: Unit disabled.");
                                        _mqEnabled = false;
                                        break;

                                    case MulDivCommand.UnsignedDivide:
                                        Trace.Log(LogType.MulDiv, "MulDiv Enabled: Divide");
                                        _mqShifter.SetShifterCommand(ShifterCommand.LeftShift, 1, 0);
                                        _mqEnabled = true;
                                        break;

                                    case MulDivCommand.UnsignedMultiply:
                                    case MulDivCommand.SignedMultiply:
                                        Trace.Log(LogType.MulDiv, "MulDiv Enabled: Multiply");
                                        _mqShifter.SetShifterCommand(ShifterCommand.RightShift, 1, 0);
                                        _mqEnabled = true;
                                        break;
                                }
                            }
                            break;

                        case 0xa:   // LoadOp
                            // NOTE:
                            // LoadOp triggers a hardware assisted refill of the Op file by copying
                            // the four words following a Fetch4 request into the 8x8 RAM.  Here we
                            // ask the memory controller if we're in the right cycle; this should
                            // always return true, since otherwise the microcode is buggy!
                            _refillOp = _memory.LoadOpFile();

                            if (_refillOp)
                            {
                                Trace.Log(LogType.OpFile, "OpFile: Load init.");
                            }
#if DEBUG
                            else
                            {
                                Trace.Log(LogType.Errors, "LoadOp called in wrong cycle?");
                            }
#endif

                            if (_ustore.ROMEnabled)
                            {
                                _ustore.DisableROM();   // LoadOp disables the Boot ROM
                            }
                            break;

                        case 0xb:   // BPC := (R)
                            _bpc = _alu.R.Lo & 0xf;     // bottom 4 bits of R
                            Trace.Log(LogType.OpFile, "BPC set to {0:x1}", BPC);
                            break;

                        case 0xc:   // WCSL
                            _ustore.WriteControlStore(ControlStoreWord.Low, _usequencer.S, _alu.OldR.Lo);
                            break;

                        case 0xd:   // WCSM
                            _ustore.WriteControlStore(ControlStoreWord.Middle, _usequencer.S, _alu.OldR.Lo);
                            break;

                        case 0xe:   // WCSH
                            _ustore.WriteControlStore(ControlStoreWord.High, _usequencer.S, _alu.OldR.Lo);
                            break;

                        case 0xf:   // IOB function
                            if (uOp.F == 0x0)
                            {
                                // Input if the msb of Z is unset, Output otherwise
                                if (uOp.IsIOInput)
                                {
                                    _iod = _system.IOBus.IORead(uOp.IOPort);
                                }
                                else
                                {
                                    _system.IOBus.IOWrite(uOp.IOPort, _alu.R.Lo);
                                }
                            }
                            break;

                        default:
                            throw new UnimplementedInstructionException(
                                String.Format("Unimplemented Special Function {0:x1}", uOp.SF));
                    }
                    break;

                case 0x1:   // Memory / Extended functions

                    if (uOp.SF <= 7)
                    {
                        if (Is4K)
                        {
                            // The 4K CPU doesn't have hardware to execute F=1, SF=0-7
                            // but some microcode actually uses this to determine what
                            // processor type it's running on!  We handle those odd
                            // cases here...
                            switch (uOp.SF)
                            {
                                case 0x0:
                                    _alu.SetResult(0);
                                    Trace.Log(LogType.Errors, "Read from Victim latch ignored (4K)");
                                    break;

                                case 0x4:
                                    _alu.SetResult(0);
                                    Trace.Log(LogType.Errors, "Read from MQ ignored (4K)");
                                    break;

                                default:
                                    Trace.Log(LogType.Errors, "Extended special function {0} ignored (4K)", uOp.SF);
                                    break;
                            }
                        }
                        else
                        {
                            // The 16K CPUs use the low SF bits to encode the
                            // Extended Special Functions:
                            switch (uOp.SF)
                            {
                                case 0x0:   // (R) := Victim Latch
                                    _alu.SetResult(_usequencer.Victim);
                                    _xy.WriteRegister(uOp.X, _alu.R.Value);

                                    Trace.Log(LogType.OpFile, "Read from Victim latch {0:x4}", _usequencer.Victim);

                                    // ReadVictim clears the latch, does not set PC
                                    _usequencer.Victim = 0xffff;
                                    break;

                                case 0x1:   // Multiply / DivideStep
                                    Trace.Log(LogType.MulDiv, "MulDiv step: MQ in ={0:x4} R={1:x6} R<15>={2}",
                                                               _mq, _alu.R.Lo, ((_alu.R.Lo & 0x8000) >> 15));
                                    //
                                    // For the hardware assisted Multiply/Divide steps, we've already done
                                    // the ALU op on the high word of the product or quotient during the ALU
                                    // execution above; here we take care of the low word in the MQ register.
                                    //
                                    switch (_rasterOp.MulDivInst)
                                    {
                                        case MulDivCommand.Off:
                                            throw new InvalidOperationException("MulDiv error: step SF while not enabled??");

                                        case MulDivCommand.UnsignedDivide:
                                            //
                                            // For one division step:
                                            //  1. Shift MQ left one bit;
                                            //  2. Save the complement of the MSB from R into the LSB of MQ.
                                            //
                                            // Thar be dragons here:
                                            //   ONLY complement the LSB if the ALU op was an add or subtract;
                                            //   this covers the setup step where the initial left shift is done
                                            //   but no Q0 bit computation is needed.  The 16K ALU does this based
                                            //   on ArithX/ArithY, the MDINSTR bits and some PAL logic; here we're
                                            //   just cheating.  (It's a no-win; either we peek at the ALU op here,
                                            //   or the ALU has to reach into the MQ reg...)
                                            //
                                            if ((uOp.ALU == ALUOperation.AplusB) || (uOp.ALU == ALUOperation.AminusB))
                                            {
                                                _mqShifter.Shift(_mq);
                                                _mq = _mqShifter.ShifterOutput | (~((_alu.R.Lo & 0x8000) >> 15) & 0x1);
                                            }
#if DEBUG
                                            else
                                            {
                                                Trace.Log(LogType.MulDiv, "MulDiv Step: Q0 bit skipped");
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
                                            _mq = _mqShifter.ShifterOutput | ((_alu.R.Lo & 0x1) << 15);
                                            break;
                                    }
                                    Trace.Log(LogType.MulDiv, "MulDiv Step: MQ out={0:x4} MQ<0>={1}", _mq, (_mq & 0x1));
                                    break;

                                case 0x2:   // Load multiplier / dividend
                                    _mq = _alu.R.Lo;        // MQ reg is 16 bits wide
                                    Trace.Log(LogType.MulDiv, "MulDiv Load: MQ={0:x4}", _mq);
                                    break;

                                case 0x3:   // Load base register (not R)
                                    _xy.SetRegisterBase((byte)(~_alu.R.Lo));
                                    break;

                                case 0x4:   // (R) := product or quotient
                                    _alu.SetResult(_mq & 0xffff);
                                    _xy.WriteRegister(uOp.X, _alu.R.Value);
                                    Trace.Log(LogType.MulDiv, "Read: MQ={0:x4}", _mq);
                                    break;

                                case 0x5:   // Push long constant
#if DEBUG
                                    if (uOp.LongConstant != _alu.R.Value)
                                    {
                                        Console.WriteLine("Push long const discrepancy: R={0:x6}, uOp={1:x6}!", _alu.R.Value, uOp.LongConstant);
                                        _system.Break();
                                    }
#endif
                                    _estack.Push(uOp.LongConstant);
                                    break;

                                case 0x6:   // Address input (MA) := Shift
                                    _shifter.Shift(_alu.OldR.Lo);
                                    //
                                    // We hack in the NIA value into the decoded (cached) Instruction's
                                    // "Next" field.  This should always work since the Next field's
                                    // value is never normally used for this type of instruction.  The
                                    // shifter provides 12- or 14-bits of address depending on CPU type.
                                    //
                                    uOp.NextAddress = _shifter.ShifterOutput;
                                    break;

                                case 0x7:   // Leap address generation
                                            // This is handled by CalcAddress
                                    break;
                            }
                        }
                    }
                    else if (uOp.SF <= 15)
                    {
                        // Common to all CPUs: SF 8..15 are the memory ops
                        _memory.RequestMemoryCycle(_alu.R.Value, uOp.MemoryRequest);
                    }
                    else
                    {
                        // Not possible...
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
        /// Load two bytes into the OpFile from the current incoming memory word.
        /// If the refill is complete, reset the flag (but the microcode sets BPC,
        /// not the hardware).
        /// </summary>
        public void LoadOpWord()
        {
            int opAddr = _memory.MIndex * 2;

            _opFile[opAddr] = (byte)(_memory.MDI & 0xff);
            _opFile[opAddr + 1] = (byte)((_memory.MDI & 0xff00) >> 8);

            if (Trace.TraceOn)
            {
                Trace.Log(LogType.OpFile, "Loaded {0:x2} into OpFile[{1:x}] from {2:x6}", _opFile[opAddr], opAddr, _memory.MADR);
                Trace.Log(LogType.OpFile, "Loaded {0:x2} into OpFile[{1:x}] from {2:x6}", _opFile[opAddr + 1], opAddr + 1, _memory.MADR);
            }

            if (_memory.MIndex == 3)
            {
                _refillOp = false;
            }
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

        /// <summary>
        /// Increments the DDS (diagnostic counter).
        /// </summary>
        public void IncrementDDS()
        {
            _dds++;
            Trace.Log(LogType.DDS, "DDS is now {0:d3}", _dds % 1000);

            // This is a little hacky, but since just about every PERQ
            // ever made used standard boot ROMs and the standard SYSB
            // microcode, having this hook here isn't too terrible...
            if (_dds == 149)
            {
                _system.PressBootKey();
            }
            // Send the event and let the updates fall where they may
            // _system.MachineStateChange(DDSChanged, _dds);

            // TODO: a general event-based way to provide hooks or
            // breakpoints when the DDS reaches a particular value might
            // be a cleaner way to allow a GUI, the debugger, or the boot
            // key-holder-downer-thingie to hook in...

            // TODO: This should be moved elsewhere - catch it in the CLI!?
            Console.Title = string.Format("DDS {0:d3}", _dds % 1000);
        }

        #endregion CPU Helper Functions

        #region Debugger support

        /// <summary>
        /// Provide access to the microstore for the debugger/disassembler.
        /// </summary>
        public Instruction GetInstruction(ushort addr)
        {
            return _ustore.GetInstruction(addr);
        }

        public void LoadMicrocode(string ucodeFile)
        {
        	_ustore.LoadMicrocode(ucodeFile);
        }

        /// <summary>
        /// Provide access to the XY registers for the debugger.
        /// </summary>
        public int ReadRegister(byte r)
        {
            return _xy.ReadRegister(r);
        }

        public void WriteRegister(byte r, int v)
        {
            _xy.WriteRegister(r, v);
        }

        /// <summary>
        /// Load the boot ROM into the microstore.
        /// </summary>
        public void LoadROM(string romFile)
        {
            _ustore.LoadROM(romFile);
        }

        public void ShowPC()
        {
            // TODO: move me  or  just return the formatted string?
            Console.WriteLine("\nPC={0:x4} BPC={1:x1} Victim={2:x4} DDS={3} UState={4:x2} Interrupt={5}",
                              PC, BPC, Victim, DDS, ReadMicrostateRegister(0), InterruptFlag);
        }

        public void ShowOpfile()
        {
            Console.WriteLine("BPC={0}. Opfile Contents:", BPC);

            for (int i = 0; i < 8; i++)
            {
                Console.WriteLine("   {0}: {1:x2}", i, _opFile[i]);
            }
        }

        public void ShowEStack()
        {
            _estack.DumpContents();
        }

        public void ShowCStack()
        {
            _usequencer.DumpContents();
        }

        #endregion

        //
        // Configurables set by derived classes
        //
        protected static string _name;
        protected static string _desc;

        protected static int _bits;
        protected static int _mask;

        protected static int _wcsBits;
        protected static int _wcsSize;
        protected static int _wcsMask;

        protected static ulong _cycleTime;

        // Major components, common to all CPU types
        private RasterOp _rasterOp;
        private Sequencer _usequencer;
        private ControlStore _ustore;
        private RegisterFile _xy;
        private ExpressionStack _estack;

        // Shared with the sequencer
        protected ALU _alu;
        protected Shifter _shifter;

        // Memory card reference
        private MemoryBoard _memory;

        // Parent
        private PERQSystem _system;

        //
        // Parts we implement in this file directly
        //

        // Multiplier Quotient register ("MQ") only exists in the 16K or newer CPUs,
        // but some microcode (VFY 2.x, e.g.) tries to write/read from MQ to test
        // for WCS size on the fly.  So we define MQ for all CPU types, but treat
        // it as read-only in the 4K variant.
        private int _mq;
        private bool _mqEnabled;
        private Shifter _mqShifter;

        // Note that the Op file is only 8 bytes, but BPC is a 4-bit counter...
        protected byte[] _opFile = new byte[16];

        // Flag if LoadOp is requested (hardware assisted refill of the 8x8 OpFile)
        private bool _refillOp;

        // Byte Program Counter
        protected int _bpc;
        protected bool _incrementBPC;

        // IO data
        private int _iod;

        // Interrupt flags
        protected InterruptType _interruptFlag;
        private object _intLock;

        //
        // Housekeeping
        //
        protected ulong _clocks;

        // Diagnostic counter
        private int _dds;

        // Copy of Bmux bits in microstate register
        // for mysterious, if not nefarious purposes
        protected int _lastBmux;

        // Trace/debugging support
        private ushort _lastPC;
        private Instruction _lastInstruction;
    }
}
