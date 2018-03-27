// rasterop.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.CPU;
#if DEBUG
using PERQemu.Debugger;
#endif

using System;
using System.IO;
using System.Text;
using System.Collections.Generic;

namespace PERQemu.Memory
{

    public class PowerOffException : Exception
    {
        public PowerOffException()
        {
            // someday when we're multithreaded, this will be significant;
            // abort the Z80 and video controller threads (turn the display dark :-)
            // and return to the configurator/debugger console.  it'll be rad.
        }
    }

    /// <summary>
    /// The direction of a RasterOp fetch/store.
    /// </summary>
    public enum Direction
    {
        LeftToRight,
        RightToLeft
    }

    /// <summary>
    /// Mask bits computed by the hardware that affect the operation
    /// of the Combiner.  We use them here to flag words in the FIFOs
    /// (useful in debugging).
    /// </summary>
    [Flags]
    public enum CombinerFlags
    {
        Invalid = 0x00,         // word not properly initialized (debugging)
        DontMask = 0x01,        // pass word unmodified; beginning of scan line
        LeftEdge = 0x02,        // word contains a left edge
        RightEdge = 0x04,       // word contains a right edge
        Both = 0x06,            // word contains both edges (shortcut)
        FullWord = 0x08,        // use all 16 bits
        Leftover = 0x10         // pass word unmodified; clear end of scan line
    }

    /// <summary>
    /// A memory word in the RasterOp datapath, augmented with debugging info
    /// (tracks the source address of a given word).
    /// </summary>
    public struct ROpWord
    {
        public ROpWord(int addr, int idx, ushort val)
        {
            Address = addr;
            Index = idx;
            Data = val;
            Mask = CombinerFlags.Invalid;
        }

        public void Clear()
        {
            Address = 0;
            Index = 0;
            Data = 0;
            Mask = CombinerFlags.Invalid;
        }

        public override string ToString()
        {
            return String.Format("Addr={0:x5} Idx={1} Data={2:x4} Mask={3}", Address, Index, Data, Mask);
        }

        public int Address;
        public int Index;
        public ushort Data;
        public CombinerFlags Mask;
    }


    /// <summary>
    /// Implements the PERQ's RasterOp hardware pipeline.  Works with the RasterOp microcode
    /// to feed quad words through the shifter/combiner to move rectangular regions of memory
    /// very quickly.
    /// </summary>
    public sealed class RasterOp
    {
        private RasterOp()
        {
            _ropShifter = new Shifter();            // Our own private Idaho
            _srcFifo = new Queue<ROpWord>(16);      // 4 quads (hardware limit)
            _destFifo = new Queue<ROpWord>(4);      // 1 quad
            _halfPipe = new ROpWord();              // 1 word, for overlap
            _rdsTable = new CombinerFlags[512];     // 9 bit index
            _rscTable = new EdgeStrategy[128];      // 7 bit index
        }

        public static RasterOp Instance
        {
            get { return _instance; }
        }

        public void Reset()
        {
            _enabled = false;
            _setupDone = false;
            _state = State.Off;
            _srcFifo.Clear();
            _destFifo.Clear();
#if DEBUG
            _ropDebug = false;
#endif
            // Move this to the constructor after debugging so it doesn't get
            // run on every reset (which is handy for making tweaks while debugging)
            LoadRasterOpROMs();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.RasterOp, "RasterOp: Reset.");
#endif
        }

        public bool Enabled
        {
            get { return _enabled; }
        }

        #region Debugging

#if DEBUG
        public bool Debug
        {
            get { return _ropDebug; }
            set { _ropDebug = value; }
        }

        public void SaveRopTables()
        {
            StringBuilder sb = new StringBuilder();

            sb.AppendLine("key:\tphase dir dwp wid idx\tresult\n");
            for (var i = 0; i < 512; i++)
            {
                sb.AppendFormat("{0:x3}\t  {1}    {2}   {3}   {4}   {5}\t{6}\n", i,
                                ((i & 0x180) >> 7),             // phase
                                ((i & 0x040) >> 6),             // direction
                                ((i & 0x030) >> 4),             // destwordpos
                                ((i & 0x00c) >> 2),             // widextraword
                                (i & 0x003),                    // index
                                (CombinerFlags)_rdsTable[i]);   // result
            }

            using (StreamWriter outfile = new StreamWriter(Paths.BuildPROMPath("mask_table.txt")))
            {
                outfile.Write(sb.ToString());
            }
            Console.WriteLine("Wrote RDS table.");

            sb.Remove(0, sb.Length);    // uh, sb = ""? sb.Clear()?
            sb.AppendLine("key:\tdir dst src  lo  xflg\tresult\n");
            for (var i = 0; i < 128; i++)
            {
                sb.AppendFormat("{0:x3}\t {1}   {2}   {3}   {4}   {5}\t{6}\n", i,
                                ((i & 0x40) >> 6),              // direction
                                ((i & 0x30) >> 4),              // dst mask
                                ((i & 0x0c) >> 2),              // src mask
                                ((i & 0x02) >> 1),              // leftover
                                (i & 0x01),                    // xtrasrcword && xoffset
                                (EdgeStrategy)_rscTable[i]);    // result
            }

            using (StreamWriter outfile = new StreamWriter(Paths.BuildPROMPath("edge_table.txt")))
            {
                outfile.Write(sb.ToString());
            }
            Console.WriteLine("Wrote RSC table.");
        }

        public void ShowState()
        {
            Console.WriteLine("RasterOp debugging is {0}.", _ropDebug);
            Console.WriteLine("RasterOp enabled is {0}.", _enabled);
            if (_enabled)
            {
                Console.WriteLine("\tState={0} Phase={1} Clock=T{2}",
                                 _state, _phase, MemoryBoard.Instance.TState);
                if (_setupDone)
                {
                    Console.WriteLine("\txOffset={0} lastSrc={1} srcAligned={2} LeftOver={3}",
                                     _xOffset, _lastSrcPosition, _srcNeedsAligned, _leftOver);
                    Console.WriteLine("\tMasks: Left={0:x4} Right={1:x4} Full={2:x4}",
                                    _leftEdgeMask, _rightEdgeMask, _bothEdgesMask);
                }
            }
        }

        public void ShowFifos()
        {
            Console.WriteLine("Half pipeline register: {0}", _halfPipe);
#if TRACING_ENABLED
            if (Trace.TraceOn)
            {
                DumpFifo("Source FIFO:\n", _srcFifo);
                DumpFifo("Destination FIFO:\n", _destFifo);
            }
            else
            {
                Console.WriteLine("Enable logging to show contents of FIFOs");
            }
#else
            Console.WriteLine("Tracing must be enabled.");
#endif
        }

        public void ShowRegs()
        {
            Console.WriteLine("CtlRasterOp:  {0}\n\tPhase={1} Dir={2} XtraSrcWord={3} Latch={4}",
                             (_enabled ? "Enabled" : "Disabled"), _phase, _direction, _extraSrcWord, _latchOn);
            Console.WriteLine("SrcRasterOp:  Func={0} WordPos={1} BitOffset={2}",
                              _function, _srcWordPosition, _srcBitOffset);
            Console.WriteLine("DstRasterOp:  Func={0} WordPos={1} BitOffset={2}",
                              _function, _destWordPosition, _destBitOffset);
#if SIXTEEN_K
            Console.WriteLine("WidRasterOp:  XtraWords={0} XtraBits={1} MulDiv={2}",
                              _widthExtraWords, _widthExtraBits, _muldivInst);
#else
            Console.WriteLine("WidRasterOp:  XtraWords={0} XtraBits={1}",
                              _widthExtraWords, _widthExtraBits);
#endif
        }
#endif

        #endregion

        /// <summary>
        /// Sets the RasterOp Control Register, enabling or disabling the RasterOp
        /// hardware.  Is used to set the "phase" of the action, determine the direction
        /// of the transfer, and sync the RasterOp state clock with the Memory state.
        /// </summary>
        /// <param name="value"></param>
        public void CntlRasterOp(int value)
        {
            bool saveExtraSrcWord = _extraSrcWord;      // Save in case we bail early...
            Direction saveDirection = _direction;       // ...and this too.  Sigh.

            _latchOn = (value & 0x40) != 0;             // Used by the hardware, no real use here
            _extraSrcWord = (value & 0x20) != 0;
            _phase = (Phase)((value & 0x1c) >> 2);
            _enabled = (value & 0x2) != 0;
            _direction = (value & 0x1) != 0 ? Direction.RightToLeft : Direction.LeftToRight;

            if (_enabled)
            {
                #region SorryIasked
                //
                // CntlRasterOp is always called in T1, two cycles ahead of a Fetch/Fetch4R.
                // I have no idea what the real hardware does if this rule is violated; probably
                // random bad things, no doubt.  This is true in every version of the RasterOp
                // microcode that I've been able to study.
                //
                // For our purposes this means that the first state transition happens in the next
                // cycle (T2), but four cycles from the T2 when dest or source fetch data appears
                // on MDI.  Due to this latency, the Idle phase actually happens at the beginning
                // of the three-quad cycle, not at the end (as illustrated in Tony Duell's CPU Tech
                // Reference).  We force this by setting initial state to SrcFetch.  Yeah, I know.
                //
                #endregion
                _state = State.SrcFetch;

                Setup();
            }
            else
            {
                if (MemoryBoard.Instance.MDONeeded && _destFifo.Count > 0)
                {
                    // Ugh. The microcode bailed on us before the destination words were all
                    // written! Reassert the enabled flag and set a special final state until
                    // we're actually finished.  Inelegant, but effective...
                    _enabled = true;
                    _extraSrcWord = saveExtraSrcWord;   // Restore saved values
                    _direction = saveDirection;         // for final flush
                    _state = State.Off;
                    _phase = Phase.Done;
                }
                else
                {
                    // No output pending; either we're really done, or pausing for an interrupt.
                    _state = State.Off;
                    _setupDone = false;
                }
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "CtlRasterOp:  {0}\n\tPhase={1} Dir={2} XtraSrcWord={3} Latch={4}",
                          (_enabled ? "Enabled" : "Disabled"), _phase, _direction, _extraSrcWord, _latchOn);
            DumpFifo("Source FIFO:\n", _srcFifo);
            DumpFifo("Destination FIFO:\n", _destFifo);
#endif
        }

        /// <summary>
        /// Sets the RasterOp Width register, and clears the source and destination word FIFOs.
        /// In the 16K CPU, the two upper bits also control the Multiply/Divide unit.
        /// </summary>
        /// <param name="value"></param>
        public void WidRasterOp(int value)
        {

#if SIXTEEN_K
            // 16K CPU: WidRasterOp<7:6> bits used for the Multiply/Divide Step control function
            _muldivInst = (MulDivCommand)((value & 0xc0) >> 6);
#endif
            _widthExtraWords = (value & 0x30) >> 4;
            _widthExtraBits = (value & 0xf);

            // Loading the width register clears the FIFOs
            _srcFifo.Clear();
            _destFifo.Clear();
            _halfPipe.Clear();

#if TRACING_ENABLED
#if SIXTEEN_K
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "WidRasterOp:  XtraWords={0} XtraBits={1} MulDiv={2}",
                         _widthExtraWords, _widthExtraBits, _muldivInst);
#else
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "WidRasterOp:  XtraWords={0} XtraBits={1}",
                         _widthExtraWords, _widthExtraBits);
#endif
#endif
        }

#if SIXTEEN_K
        /// <summary>
        /// Returns the current status of the Multiply/Divide unit.
        /// </summary>
        public MulDivCommand MulDivInst
        {
            get { return _muldivInst; }
        }
#endif

        /// <summary>
        /// Loads the RasterOp Destination register.
        /// </summary>
        /// <param name="value"></param>
        public void DstRasterOp(int value)
        {
            _function = (Function)(((int)_function & 0x4) | (((~value) & 0xc0) >> 6));
            _destWordPosition = (value & 0x30) >> 4;
            _destBitOffset = (value & 0xf);

            //#if TRACING_ENABLED
            //            // Ugh, since the PERQ doesn't really have a NOOP, the assembler builds an
            //            // instruction that does a dummy assignment to this register... which means
            //            // a massive amount of spurious log spewage.
            //            if (Trace.TraceOn)
            //                Trace.Log(LogType.RasterOp, "DstRasterOp:  Func={0} WordPos={1} BitOffset={2}",
            //                                                _function, _destWordPosition, _destBitOffset);
            //#endif
        }

        /// <summary>
        /// Loads the RasterOp Source register.  Upper bit also controls the PERQ1 power supply!
        /// </summary>
        /// <param name="value"></param>
        public void SrcRasterOp(int value)
        {
            _function = (Function)(((int)_function & 0x3) | (((~value) & 0x40) >> 4));
            _srcWordPosition = (value & 0x30) >> 4;
            _srcBitOffset = (value & 0xf);

            // Check for poweroff bit.  Stop emulation if so.
            if ((value & 0x80) == 0)
            {
                throw new PowerOffException();
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "SrcRasterOp:  Func={0} WordPos={1} BitOffset={2}",
                                            _function, _srcWordPosition, _srcBitOffset);
#endif
        }

        /// <summary>
        /// Runs the next step in the RasterOp cycle, or a No-op if the hardware isn't enabled.
        /// </summary>
        public void Clock()
        {
            ROpWord w;

            _state = NextState();

            if (!_enabled)
            {
                return;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: Clock: phase={0} state={1} Tstate={2} need1st={3} LeftOver={4}",
                                            _phase, _state, MemoryBoard.Instance.TState, _srcNeedsAligned, _leftOver);
#endif

            switch (_state)
            {
                case State.Idle:

                    // Realign source FIFO leading edge if starting a new scanline
                    if (_srcNeedsAligned && _srcFifo.Count > 0)
                    {
                        ClearLeadingSrcWords();
                    }
                    break;

                case State.DestFetch:
#if DEBUG
                    // FirstSource, XtraSource skip the DestFetch state, should never get here!
                    if (_phase == Phase.FirstSource || _phase == Phase.XtraSource)
                        throw new InvalidOperationException(String.Format("Unexpected state {0} during {1}", _state, _phase));
#endif

                    // Fetch the dest word, set its mask and queue the result!
                    w = FetchNextWord();
                    w.Mask = DestWordMask(w.Index);
                    _destFifo.Enqueue(ComputeResult(w));
#if TRACING_ENABLED
                    DumpFifo("Destination FIFO:\n", _destFifo);
#endif
                    break;

                case State.SrcFetch:

                    // Check if we're in leftOver and need to clear the previous scanline
                    if (_leftOver)
                    {
                        ClearExtraSrcWords();
                    }

                    // Always queue up the incoming source word
                    w = FetchNextWord();
                    _srcFifo.Enqueue(w);
#if TRACING_ENABLED
                    DumpFifo("Source FIFO:\n", _srcFifo);
#endif
                    break;

                case State.Off:

                    // If we get here, the microcode has bailed out while the last quad word is
                    // still being written out, and a fetch of the start of the next line is
                    // in progress (but is being ignored).  We queue up those words to avoid
                    // draining the source queue while destination words are still being written.
                    _srcFifo.Enqueue(FetchNextWord());
                    break;
            }
        }

        /// <summary>
        /// Gets the next word from the result queue.  Expected to be called only if
        /// a Store4/4R cycle is currently in progress..
        /// </summary>
        public ushort Result()
        {
            ROpWord dest;
#if DEBUG            
            // A few sanity checks, until things are debugged...
            if (!_enabled && _state != State.Off)
                throw new InvalidOperationException("Result needed while RasterOp disabled");

            if (_destFifo.Count < 1)
                throw new InvalidOperationException("Destination FIFO empty and result needed");
#endif

            // Get the destination word
            dest = _destFifo.Dequeue();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.RasterOp, "RasterOp: Returning result word: {0:x4}", dest.Data);
#endif
            return dest.Data;
        }

        /// <summary>
        /// Aligns and combines the Source and Destination words to produce a RasterOp
        /// result word.  Called during DestFetch as each word arrives to avoid
        /// complications from the delay in the overlapped Fetch/Store cycle.
        /// </summary>
        private ROpWord ComputeResult(ROpWord dest)
        {
            ROpWord src = dest;                         // initialize to silence the Xamarin compiler...
            EdgeStrategy e = EdgeStrategy.NoPopNoPeek;  // assume nothing!  irrelevant in non-edge cases anyway
            ushort aligned, combined;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.RasterOp, "RasterOp: Result dest word: {0}", dest);
#endif

            #region Align Words

            // The Emperor has made a critical error, and the time for our word
            // alignment has come.  Admiral Ackbar will explain the plan of attack:
            //
            // 1.   At the start of a scanline, our source FIFO is aligned and the
            //      half-pipeline register is "primed".  This means we should be
            //      guaranteed to have at least the first edge word in the FIFO;
            // 2.   If the source spans the quad but the destination doesn't (or
            //      vice versa), we have to account for the "two edges into one"
            //      problem - pull in the extra source word (source L+R -> dest B)
            //      or hold the source word one extra cycle (dest L+R <- source B);
            // 3.   In all other modes, we should be able to just pop the next word
            //      so the FIFOs move in lock step (while in the source region), he
            //      said, handwaving furiously.
            //
            // The RSC03 ROM tells the hardware how to cope with the source FIFO; here
            // we use a lookup table to deal with all the complicated edge alignment
            // rules outlined above.
            //
            // Many Bothans died to bring us this information...

            // Look at the destination word to find our edges and set the _leftOver flag
            switch (dest.Mask)
            {
                // Outside the first edge: return dest unmodified, don't touch the source FIFO
                case CombinerFlags.DontMask:
                    return dest;

                // Outside the second edge: return dest unmodified, but pop the source word too.
                // In some cases we do legitimately clear the last source word, so test for that.
                case CombinerFlags.Leftover:
                    if (_srcFifo.Count > 0)
                    {
                        src = _srcFifo.Dequeue();
#if DEBUG
                        src.Mask = SrcWordMask(src.Index);  // for debugging, not necessary otherwise
#endif
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.RasterOp, "RasterOp: Dropped src word: {0}", src);
#endif
                    }
                    return dest;

                // Full word (any phase): should always have a matching source word
                case CombinerFlags.FullWord:
                    try
                    {
                        src = _srcFifo.Dequeue();
#if DEBUG
                        src.Mask = SrcWordMask(src.Index);  // for debugging, not necessary otherwise
#endif
                    }
                    catch (InvalidOperationException)
                    {
#if DEBUG
                        Console.WriteLine("Source FIFO empty at Full word!");   // continuing will probably fail...
#else
                        throw new InvalidOperationException("Source FIFO empty and Result expected");
#endif
                    }
                    break;

                // Left edge: flag the beginning or end of the update region
                // Right edge: opposite of left, flag the other end :-)
                case CombinerFlags.LeftEdge:
                case CombinerFlags.RightEdge:
                    _leftOver = (dest.Mask == CombinerFlags.LeftEdge && _direction == Direction.RightToLeft) ||
                                (dest.Mask == CombinerFlags.RightEdge && _direction == Direction.LeftToRight);

                    if (_srcFifo.Count > 0)
                    {
                        src = _srcFifo.Peek();
                        src.Mask = SrcWordMask(src.Index);
                        e = GetEdgeStrategy(dest.Mask, src.Mask);
                    }
                    else if (_leftOver && (_extraSrcWord || _xOffset > 0))
                    {
                        // At the end of a line (either direction) we are peeking forward
                        // but have run out of source words.  In this case we copy the half-
                        // pipeline register (in essence, not popping the second edge word)
                        // to provide enough bits to complete the line.
                        src = _halfPipe;
                    }
                    else
                    {
#if DEBUG
                        Console.WriteLine("Source FIFO empty at {0} word!", dest.Mask);   // continuing will probably fail...
#else
                        throw new InvalidOperationException("Source FIFO empty and Result expected");
#endif
                    }
                    break;

                // Both edges in one word
                case CombinerFlags.Both:
                    _leftOver = true;       // But... but... it's false too!  Ow, my head.

                    try
                    {
                        src = _srcFifo.Peek();
                        src.Mask = SrcWordMask(src.Index);
                        e = GetEdgeStrategy(dest.Mask, src.Mask);
                    }
                    catch (InvalidOperationException)
                    {
#if DEBUG
                        Console.WriteLine("Source FIFO empty at Both edges word!");    // continuing will probably fail..
#else
                        throw new InvalidOperationException("Source FIFO empty and Result expected");
#endif
                    }
                    break;

                // Should never happen if our RDS00 ROM table is correct!
                case CombinerFlags.Invalid:
                    throw new InvalidOperationException("RasterOp Destination result word has Invalid mask");
            }

            // Pop the current word?
            if (e == EdgeStrategy.PopPeek || e == EdgeStrategy.PopNoPeek)
            {
                _srcFifo.Dequeue();
                // Mask was already set
            }

            // Peek ahead to the next?
            if (e == EdgeStrategy.PopPeek || e == EdgeStrategy.NoPopPeek)
            {
                src = _srcFifo.Dequeue();
                src.Mask = SrcWordMask(src.Index);
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: Next source word: {0}", src);
#endif

            #endregion

            #region Align Bits

            // The destination word is in the update region and our source word is aligned.
            // If bit alignment is needed, feed the saved word from the half pipe to the
            // shifter with the current word.  Note that the MSB of the combined shifter
            // inputs is always the leftmost pixel in the update region (so, dependent upon
            // the direction of transfer).
            if (_xOffset != 0)
            {
                if (_direction == Direction.LeftToRight)
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.RasterOp, "RasterOp: Result xtra (hi): {0:x4}", _halfPipe);
#endif
                    _ropShifter.Shift(src.Data, _halfPipe.Data);
                }
                else
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.RasterOp, "RasterOp: Result xtra (lo): {0:x4}", _halfPipe);
#endif
                    _ropShifter.Shift(_halfPipe.Data, src.Data);
                }

                // Save the current source word
                _halfPipe = src;

                // Grab the aligned source word from the shifter
                aligned = _ropShifter.ShifterOutput;
            }
            else
            {
                // Already in alignment
                aligned = src.Data;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: Result aligned:  {0:x4}", aligned);
#endif
            #endregion

            #region Combine 'em

            // Finally! Combine source & dest words using the appropriate mask
            switch (dest.Mask)
            {
                case CombinerFlags.LeftEdge:
                    combined = Combine(dest.Data, aligned, _leftEdgeMask);
                    break;

                case CombinerFlags.RightEdge:
                    combined = Combine(dest.Data, aligned, _rightEdgeMask);
                    break;

                case CombinerFlags.Both:
                    combined = Combine(dest.Data, aligned, _bothEdgesMask);
                    break;

                case CombinerFlags.FullWord:
                    combined = Combine(dest.Data, aligned, 0xffff);
                    break;

                default:
                    combined = dest.Data;   // This can't actually happen (silence a warning)
                    break;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: Result combined: {0:x4} (func={1})", combined, _function);
#endif

            #endregion


            // For debugging, we return the ROpWord, updated with the combined result.
            // At some point when this is fully debugged, I'll probably convert the dest
            // FIFO to a simple queue of ushorts and just pass back the results.  That's
            // more like the hardware, actually... no "dest fifo" at all, but with each
            // stage (shifter, combiner, result) making up the difference in timing of
            // the overlapped fetch/store cycle.
            dest.Data = combined;
            return dest;
        }

        /// <summary>
        /// Precomputes several values for the current RasterOp call so that
        /// they don't have to be repeated during Result() calls.
        /// </summary>
        private void Setup()
        {
            if (!_setupDone)
            {
                //
                // RasterOp shifter setup
                //
                // xOffset is the shift amount based on current register values;
                // set up the RasterOp shifter command.
                _xOffset = (16 + (_destBitOffset - _srcBitOffset)) & 0xf;
                _ropShifter.SetShifterCommand(ShifterCommand.Rotate, _xOffset, 0);

                //
                // Region bitmask setup
                //
                // Left edge is pretty straightforward
                _leftEdgeMask = (ushort)(0xffff >> _destBitOffset);

                // Right is more complex, because we have to take the remainder
                // of any bits that spill into the next word.  Crude?  But effective
                _rightEdgeMask = (ushort)~(0xffff >> (((_destBitOffset + _widthExtraBits) & 0xf) + 1));

                // If both edges are in the same word, combine the edge masks
                _bothEdgesMask = (ushort)~(_leftEdgeMask ^ _rightEdgeMask);  // xnor 'em

                //
                // "Half Pipeline Register" setup
                //
                // Determine if the source overlaps a word boundary
                bool spanSrcWords = (_srcBitOffset + _widthExtraBits > 15);

                // Determine if the destination overlaps a word boundary
                bool spanDestWords = (_destBitOffset + _widthExtraBits > 15);

                // Destination width, accounting for wrap & direction...
                int width = (4 + (_widthExtraWords - _destWordPosition)) & 0x3;

                // If we're spanning, the microcode already accounted for that;
                // we have to undo that to get an accurate source width.  This
                // makes perfect sense if you stare at it long enough, at 3AM.
                if (spanDestWords) width--;

                // Compute the 2nd source edge word to make the region check simpler
                _lastSrcPosition = (_srcWordPosition + width) & 0x3;

                // Bump for source spanning!  Note that _srcWordPos and our _lastSrcPos
                // are always LtoR; the uCode flips the _dstWordPos/_extraWords for RtoL
                // but we don't.  Because... uh... we just don't.
                if (spanSrcWords) _lastSrcPosition = (_lastSrcPosition + 1) & 0x3;

                // Set flags to indicate we are outside the update region to begin
                _srcNeedsAligned = true;        // The Pittsburgh variable
                _leftOver = false;

                _setupDone = true;

#if TRACING_ENABLED
                if (Trace.TraceOn)
                {
                    Trace.Log(LogType.RasterOp,
                              "RasterOp:  Setup:\n\txOffset={0} lastSrc={1} Left={2:x4} Right={3:x4} Full={4:x4}",
                                 _xOffset, _lastSrcPosition, _leftEdgeMask, _rightEdgeMask, _bothEdgesMask);
                    Trace.Log(LogType.RasterOp,
                              "\tspanSrc={0} spanDst={1} width={2}", spanSrcWords, spanDestWords, width);
                }
#endif
            }
        }

        /// <summary>
        /// Returns the next RasterOp state, determined by the current phase and Memory Tstate.
        /// </summary>
        private State NextState()
        {
            // RasterOp is clocked after the Memory "tick", but before the DispatchFunction
            // which may (at the end of the cycle) specify a phase change.  This means we can
            // complete the current quad-word cycle without concern for the next phase change
            // harshing our mellow.
            int _stateClock = MemoryBoard.Instance.TState;

            State next = _state;

            // NOTE: our state change happens in T2, to coincide with MDI on fetches, NOT
            // in the T1 when CntlRasterOp() is called (or in T3 when the Fetch is issued).
            switch (_state)
            {
                case State.Off:
                    if (!MemoryBoard.Instance.MDONeeded || _destFifo.Count == 0)
                    {
                        // Done waiting for the Destination Fifo to drain; really disable now
                        _enabled = false;
                        _setupDone = false;
                    }
                    break;

                case State.Idle:
                    if (_stateClock == 2)
                    {
                        if (_phase == Phase.FirstSource || _phase == Phase.XtraSource)
                        {
                            next = State.SrcFetch;
                        }
                        else
                        {
                            next = State.DestFetch;
                        }
                    }
                    break;

                case State.DestFetch:
                    if (_stateClock == 2)
                    {
                        next = State.SrcFetch;
                    }
                    break;

                case State.SrcFetch:
                    if (_stateClock == 2)
                    {
                        next = State.Idle;
                    }
                    break;

                default:
                    throw new InvalidOperationException(String.Format("Unexpected state {0} in phase {1} NextState", _state, _phase));
            }
            return next;
        }

        /// <summary>
        /// Returns the correct Mask for a given destination word in the current quad.
        /// </summary>
        /// <param name="index">Destination word index 0..3</param>
        /// <returns>Combiner mask value</returns>
        private CombinerFlags DestWordMask(int index)
        {
            // Create the 9-bit index into the RDS ROM lookup table
            int lookup = (((int)_phase & 0x3) << 7) |
                          ((int)_direction << 6) |
                          (_destWordPosition << 4) |
                          (_widthExtraWords << 2) |
                           index;

            // Annnnd return the result!
            CombinerFlags result = _rdsTable[lookup];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: DestWordMask lookup {0:x} --> {1}", lookup, result);
#endif
            return result;
        }

        /// <summary>
        /// Returns the correct Mask for a given source word in the current quad.
        /// </summary>
        /// <param name="index">Source word index 0..3</param>
        /// <returns>Combiner mask value</returns>
        private CombinerFlags SrcWordMask(int index)
        {
            // Create a 9-bit index into the RDS ROM lookup tablee
            int lookup = (((int)_phase & 0x3) << 7) |
                          ((int)_direction << 6) |
                          (_srcWordPosition << 4) |
                          (_lastSrcPosition << 2) |
                          index;

            // Same table as the dest word for source word mask!
            CombinerFlags result = _rdsTable[lookup];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: SrcWordMask lookup {0:x} --> {1}", lookup, result);
#endif
            return result;
        }

        /// <summary>
        /// Return the operation(s) to perform at the beginning or end of a
        /// scan line, given a dest and source word.
        /// </summary>
        private EdgeStrategy GetEdgeStrategy(CombinerFlags dstMask, CombinerFlags srcMask)
        {
            int lookup = (((int)_direction << 6) |
                         (((int)dstMask & 0x6) << 3) |           // XXX
                         (((int)srcMask & 0x6) << 1) |           // XXX
                          ((_leftOver ? 1 : 0) << 1) |
                          ((_extraSrcWord && _xOffset > 0) ? 1 : 0));

            EdgeStrategy result = _rscTable[lookup];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.RasterOp, "RasterOp: EdgeStrategy lookup {0:x3} --> {1}", lookup, result);
#endif
#if DEBUG
            if (result == EdgeStrategy.Unknown)
                Console.WriteLine("==> Unknown edge strategy {0:x3}! <==", lookup);    // Draw attention for debugging...
                                                                                       // should probably throw an exception in the release version... 
#endif
            return result;
        }

        /// <summary>
        /// Drop extra source words outside of the update region when searching for
        /// the first edge of a new scan line.  Updates _srcNeedsAligned when found.
        /// To avoid some overhead, is only called in Idle state if _srcNeedsAligned
        /// is true and the _srcFifo is not empty.  [could just inline this in Clock()
        /// and save a funtion call]
        /// </summary>
        private void ClearLeadingSrcWords()
        {
            // We only need to align the first edge in a Begin phase; else no-op
            if (_phase == Phase.Begin || _phase == Phase.BeginEnd || _phase == Phase.BeginEndClear)
            {
                ROpWord w = _srcFifo.Peek();
                w.Mask = SrcWordMask(w.Index);

                if ((w.Mask == CombinerFlags.Both) ||
                    (_direction == Direction.LeftToRight && w.Mask == CombinerFlags.LeftEdge) ||
                    (_direction == Direction.RightToLeft && w.Mask == CombinerFlags.RightEdge))
                {
                    _srcNeedsAligned = false;   // Found our first edge
                    _halfPipe = w;              // Prime the half-pipeline register
                }

                if (_srcNeedsAligned)
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.RasterOp, "RasterOp: --> Dropping leading word ({0})", w.Mask);
#endif
                    _srcFifo.Dequeue();
                }
            }
        }

        /// <summary>
        /// Drop extra words from the Source FIFO that are outside the update region.
        /// Called in SrcFetch if _leftOver is true.
        /// </summary>
        private void ClearExtraSrcWords()
        {
            // We only need to clear after the second edge in an End/Clear phase
            if (_phase == Phase.EndClear || _phase == Phase.BeginEndClear)
            {
                if (_srcFifo.Count == 0)
                {
                    // We've completed our DestFetch and used up all the source words; obviously
                    // there ain't no more to clear, so reset for the start of the next line.
                    _leftOver = false;
                    _srcNeedsAligned = true;
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.RasterOp, "RasterOp: --> Reset for first edge");
#endif
                }
                else
                {
                    // We're here when there are still extra source words left over from the previous
                    // scan line.  We only want to drop words through the end of the current quad.
                    ROpWord w = _srcFifo.Peek();

                    if ((_direction == Direction.LeftToRight && w.Index == 3) ||
                        (_direction == Direction.RightToLeft && w.Index == 0))
                    {
                        _leftOver = false;
                        _srcNeedsAligned = true;
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.RasterOp, "RasterOp: --> End of scan line, reset for first edge ({0})", w.Mask);
#endif
                        _srcFifo.Dequeue();
                    }

                    if (_leftOver)
                    {
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.RasterOp, "RasterOp: --> Clearing extra word ({0})", w.Mask);
#endif
                        _srcFifo.Dequeue();
                    }
                }
            }
        }

        /// <summary>
        /// Populates and returns a ROpWord with the address, index and data
        /// of the current memory word (but throws an error if MDI is invalid).
        /// </summary>
        private ROpWord FetchNextWord()
        {
            ROpWord w = new ROpWord();

            if (MemoryBoard.Instance.MDIValid)
            {
                // The microcode calculates the addresses and initiates fetches;
                // we just pluck the next incoming word off the MDI.
                w.Address = MemoryBoard.Instance.MADR;
                w.Index = MemoryBoard.Instance.MIndex;
                w.Data = MemoryBoard.Instance.MDI;
            }
            else
            {
#if DEBUG
                // For debugging we just try to continue, but we're pretty hosed at this point...
                Console.WriteLine("RasterOp: FetchNextWord while MDI was invalid!");
                w.Clear();
#else
                throw new InvalidOperationException("RasterOp: FetchNextWord while MDI was invalid!");
#endif
            }
            return w;
        }

        /// <summary>
        /// Masks and combines a source and destination word according to the current
        /// RasterOp function.
        /// </summary>
        /// <param name="dstWord">Destination</param>
        /// <param name="srcWord">Source</param>
        /// <param name="mask">Bitmask</param>
        /// <returns>The combined word</returns>
        private ushort Combine(ushort dstWord, ushort srcWord, ushort mask)
        {
            switch (_function)
            {
                case Function.Insert:
                    // Nothing; leave source alone
                    break;

                case Function.InsertNot:
                    srcWord = (ushort)(~srcWord);
                    break;

                case Function.And:
                    srcWord = (ushort)(srcWord & dstWord);
                    break;

                case Function.AndNot:
                    srcWord = (ushort)((~srcWord) & dstWord);
                    break;

                case Function.Or:
                    srcWord = (ushort)(srcWord | dstWord);
                    break;

                case Function.OrNot:
                    srcWord = (ushort)((~srcWord) | dstWord);
                    break;

                case Function.Xor:
                    srcWord = (ushort)(srcWord ^ dstWord);
                    break;

                case Function.Xnor:
                    srcWord = (ushort)((srcWord & dstWord) | ((~srcWord) & (~dstWord)));
                    break;
            }

            // Return the finished destination word
            dstWord = (ushort)((dstWord & ~mask) | (srcWord & mask));

            return dstWord;
        }

        /// <summary>
        /// Load the RDS and RSC ROM images from disk.
        /// There is no error or sanity checking done here (yet).  If those files
        /// are missing or corrupt, hilarity will ensue.
        /// </summary>
        private void LoadRasterOpROMs()
        {
            // RDS is a lookup table with a 9-bit index, returning a CombinerFlag
            FileStream fs = new FileStream(Paths.BuildPROMPath("rds00emu.rom"), FileMode.Open);

            for (int i = 0; i < 512; i++)
            {
                _rdsTable[i] = (CombinerFlags)fs.ReadByte();
            }
            fs.Close();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.EmuState, "Initialized RDS ROM lookup table.");
#endif

            // RSC is a lookup table with an 7-bit index, returning an EdgeStrategy
            fs = new FileStream(Paths.BuildPROMPath("rsc03emu.rom"), FileMode.Open);

            for (int i = 0; i < 128; i++)
            {
                _rscTable[i] = (EdgeStrategy)fs.ReadByte();
            }
            fs.Close();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.EmuState, "Initialized RSC ROM lookup table.");
#endif
        }


#if TRACING_ENABLED
        // This is an expensive debugging aid...
        private void DumpFifo(String line, Queue<ROpWord> q)
        {
            if (Trace.TraceOn)
            {
                if (q.Count > 0)
                {
                    ROpWord[] a = q.ToArray();

                    for (int i = 0; i < q.Count; i++)
                    {
                        line += String.Format("{0}\t{1}\n", i, a[i]);
                    }
                }
                else
                {
                    line += "\t<empty>\n";
                }
                Trace.Log(LogType.RasterOp, line);
            }
        }
#endif


        private enum Function
        {
            Insert = 0,
            InsertNot,
            And,
            AndNot,
            Or,
            OrNot,
            Xor,
            Xnor
        }

        private enum Phase
        {
            Begin = 0,
            Mid,
            End,
            BeginEnd,
            XtraSource,
            FirstSource,
            EndClear,
            BeginEndClear,
            Done
        }

        private enum State
        {
            Idle = 0,
            DestFetch,
            SrcFetch,
            Off
        }

        private enum EdgeStrategy
        {
            NoPopNoPeek = 0,
            NoPopPeek,
            PopNoPeek,
            PopPeek,
            Unknown = 7
        }

        // RasterOp state
        private State _state;
        private Phase _phase;
        private bool _setupDone;

        // CntlRasterOp register
        private bool _latchOn;
        private bool _enabled;
        private bool _extraSrcWord;
        private Direction _direction;

        // WidRasterOp register
        private int _widthExtraWords;
        private int _widthExtraBits;
#if SIXTEEN_K
        private MulDivCommand _muldivInst;
#endif

        // Src & DstRasterOp registers
        private Function _function;
        private int _srcBitOffset;
        private int _srcWordPosition;
        private int _destBitOffset;
        private int _destWordPosition;

        // Bit offset between SrcX and DstX
        private int _xOffset;

        // Compute the 2nd source edge word, to make region tests simpler
        private int _lastSrcPosition;

        // True if we're looking for the first edge (source FIFO)
        private bool _srcNeedsAligned;

        // True if the second edge has been processed (dest FIFO)
        private bool _leftOver;

        // Precompute bitmasks for regions that fall within one word, or left/right edges
        private ushort _leftEdgeMask;
        private ushort _rightEdgeMask;
        private ushort _bothEdgesMask;

        // Our own Shifter, so that we don't have to guess about the state of
        // the CPU's shifter (used by an interrupt routine during pauses, for
        // example) and reprogram it every cycle
        private Shifter _ropShifter;

        // The "half-pipeline register"
        private ROpWord _halfPipe;

        // Source words FIFO
        private Queue<ROpWord> _srcFifo;

        // This does not actually exist in the real hardware.  This is sort of a
        // handwavy stand-in for the memory pipeline.  Result words are queued
        // here during the RasterOp cycle and pulled out when a Store is pending.
        private Queue<ROpWord> _destFifo;

        // Mask lookup table:  This is a synthesized table roughly analogous to
        // the PERQ RDS00 PROM, returning a CombinerMask to mark words flowing
        // through the source and destination FIFOs.
        private CombinerFlags[] _rdsTable;

        // Edge lookup table:  Encodes edge processing rules for aligning the
        // source words, sort of what the RSC03 PROM does with the mysterious
        // "EVEN/ODD" stuff, but slightly less abstruse.
        private EdgeStrategy[] _rscTable;

#if DEBUG
        // Temporary debug switch
        private bool _ropDebug;
#endif

        private static RasterOp _instance = new RasterOp();
    }
}

