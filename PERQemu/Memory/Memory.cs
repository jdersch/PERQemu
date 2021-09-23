// memory.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using PERQemu.Processor;

namespace PERQemu.Memory
{

    public enum MemoryCycle
    {
        None = 0x0,
        Fetch4R = 0x8,
        Store4R = 0x9,
        Fetch4 = 0xa,
        Store4 = 0xb,
        Fetch2 = 0xc,
        Store2 = 0xd,
        Fetch = 0xe,
        Store = 0xf
    }

    /// <summary>
    /// Implements the PERQ's Memory board and memory state machine,
    /// which is also connected to the IO bus.
    /// </summary>
    public sealed class MemoryBoard
    {
        public MemoryBoard(PERQSystem system)
        {
            _system = system;
            Reset();
        }

        public void Reset()
        {
            _memory = new ushort[_memSize];

            _mdiQueue = new MemoryController(_system, "MDI");
            _mdoQueue = new MemoryController(_system, "MDO");

            _Tstate = 0;
            _mdi = 0;
            _wait = false;
            _hold = false;

            if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "Memory: Reset.");
        }

        public int MemSize
        {
            get { return _memSize; }
        }

        public ushort[] Memory
        {
            get { return _memory; }
        }

        public ushort MDI
        {
            get { return _mdi; }
        }

        public bool MDIValid
        {
            get { return _mdiQueue.Valid; }
        }

        public int MADR
        {
            get { return _mdiQueue.Address; }
        }

        public int MIndex
        {
            get { return _mdiQueue.WordIndex; }
        }

        public bool MDONeeded
        {
            get { return _mdoQueue.Valid; }
        }

        public bool Wait
        {
            get { return _wait; }
        }

        public bool Hold
        {
            get { return _hold; }
        }

        public int TState
        {
            get { return _Tstate; }
        }


        /// <summary>
        /// First half of the memory cycle: clocks the state counter, clocks the MDI and
        /// MDO queues, then sets up executes the current Fetch (if any).  Asserts the
        /// Wait signal if the CPU should abort this cycle.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Tick(MemoryCycle cycleType)
        {
            // Bump cycle counter
            _Tstate = (_Tstate + 1) & 0x3;

            Trace.Log(LogType.MemoryState, "\nMemory: Tick! T{0} cycle={1}", _Tstate, cycleType);

            // Segregate Fetches and Stores into separate queues
            if (IsFetch(cycleType))
            {
                _mdiQueue.Clock(cycleType);
                _mdoQueue.Clock(MemoryCycle.None);
            }
            else
            {
                _mdiQueue.Clock(MemoryCycle.None);
                _mdoQueue.Clock(cycleType);
            }

            ExecuteFetch();

            //
            // Set the wait flag if we need to abort the current instruction.  If
            // output is pending, we never wait; otherwise we let the conbined status
            // of the request queues determine our result.
            //
            if (MDONeeded)
            {
                _wait = false;
            }
            else
            {
                _wait = _mdiQueue.Wait || _mdoQueue.Wait;
            }
        }

        /// <summary>
        /// Second half of the memory cycle: if a store is pending, writes the output
        /// (from the ALU or current RasterOp result) to memory.
        /// </summary>
        /// <param name="input">Word to write (MDO)</param>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Tock(ushort input)
        {
            Trace.Log(LogType.MemoryState, "Memory: Tock! T{0} mdoNeeded={1} data={2:x4}",
                        _Tstate, MDONeeded, input);

            ExecuteStore((ushort)input);
        }

        /// <summary>
        /// Let the CPU know if we're in the correct cycle to initiate a
        /// LoadOp to refill the Op cache.  (Op file itself moved to the CPU)
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool LoadOpFile()
        {
            // If currently executing a Fetch4, start the refill on the
            // next T2 state (i.e., next cycle).
            if (_mdiQueue.Cycle == MemoryCycle.Fetch4 && _Tstate == 1)
            {
                return true;
            }
            return false;
        }

#if DEBUG
        /// <summary>
        /// Requests a specific memory cycle type at the specified address.  Here we route
        /// the request to the separate fetch and store queues, which vastly simplifies the
        /// complicated overlapped operation of RasterOp.
        /// </summary>
        public void RequestMemoryCycle(long id, int address, MemoryCycle cycleType)
        {
            Trace.Log(LogType.MemoryState,
                      "\nMemory: Requested {0} cycle in T{1} ID={2} addr={3:x6}",
                      cycleType, _Tstate, id, address);
            
            //
            // Queue up the request.  We're in no-man's land at the bottom of the CPU cycle,
            // but the queue controller will have stalled the processor until the correct
            // time, which will start at the next Tick().  In some cases (buggy microcode)
            // the hardware would actually ignore memory references; we don't do that here,
            // but read all the gory details in the comments at the end of MemoryController.cs.
            //
            if (IsFetch(cycleType))
            {
                _mdiQueue.Request(id, address, cycleType);
            }
            else
            {
                _mdoQueue.Request(id, address, cycleType);
            }
        }
#else
        /// <summary>
        /// Requests a specific memory cycle type at the specified address.  Here we route
        /// the request to the separate fetch and store queues, which vastly simplifies the
        /// complicated overlapped operation of RasterOp.
        /// </summary>
        public void RequestMemoryCycle(int address, MemoryCycle cycleType)
        {
            Trace.Log(LogType.MemoryState,
              "\nMemory: Requested {0} cycle in T{1} addr={2:x6}",
              cycleType, _Tstate, address);
            
            //
            // Queue up the request.  We're in no-man's land at the bottom of the CPU cycle,
            // but the queue controller will have stalled the processor until the correct
            // time, which will start at the next Tick().  In some cases (buggy microcode)
            // the hardware would actually ignore memory references; we don't do that here,
            // but read all the gory details in the comments at the end of MemoryController.cs.
            //
            if (IsFetch(cycleType))
            {
                _mdiQueue.Request(address, cycleType);
            }
            else
            {
                _mdoQueue.Request(address, cycleType);
            }
        }

#endif

        /// <summary>
        /// If the current MDI word is valid, fetches and returns the data in _mdi.
        /// If we're executing a LoadOp, copies the data into the appropriate word
        /// of the OpFile.  If the MDI pipeline is empty, is a no-op.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ExecuteFetch()
        {
            if (_mdiQueue.Valid)
            {
                _madr = _mdiQueue.Address;
                _mdi = FetchWord(_madr);
            }
        }

        /// <summary>
        /// If the current MDO word is valid, write the data to the current address.
        /// If the MDO pipeline is empty, this is a no-op.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void ExecuteStore(ushort data)
        {
            if (_mdoQueue.Valid)
            {
                StoreWord(_mdoQueue.Address, data);
            }
        }

        /// <summary>
        /// Fetches one word from memory (immediate).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort FetchWord(int address)
        {
            // Clip address to memsize range and read
            ushort data = _memory[address & _memSizeMask];

            Trace.Log(LogType.MemoryFetch, "Memory: Fetch addr {0:x5} --> {1:x4}", address & _memSizeMask, data);

            return data;
        }

        /// <summary>
        /// Stores one word into memory (immediate).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void StoreWord(int address, ushort data)
        {
            Trace.Log(LogType.MemoryStore, "Memory: Store addr {0:x5} <-- {1:x4}", address & _memSizeMask, data);

            // Clip address to memsize range and write
            _memory[address & _memSizeMask] = data;
        }

        /// <summary>
        /// Return true if the memory request type is a Fetch.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool IsFetch(MemoryCycle c)
        {
            return  c == MemoryCycle.Fetch ||
                    c == MemoryCycle.Fetch2 ||
                    c == MemoryCycle.Fetch4 ||
                    c == MemoryCycle.Fetch4R;
        }

#if DEBUG
        public void DumpQueues()
        {
            _mdiQueue.DumpQueue();
            _mdoQueue.DumpQueue();
        }
#endif

        #region Implementation notes
        //
        // All references to MDI (Memory Data IN) and MDO (Memory Data OUT) are from
        // the CPU's point of view - the opposite of the Memory Board's (and the
        // hardware schematics') point of view?
        //
        // Memory requests from the CPU/RasterOp unit are queued up according to
        // some elaborate timing rules.  Because there are several scenarios where
        // requests may overlap, fetches and stores are queued up in separate FIFOs.
        //
        // Currently the IO / DMA subsystem cheats and performs its memory accesses
        // directly, but it could at some point be integrated -- allowing us to more
        // accurately emulate the real PERQ which must give up processor cycles for
        // DMA. Thus, the "Hold" field of the microinstruction is basically ignored.
        //
        #endregion

        private MemoryController _mdiQueue;		// Queue for Fetch requests
        private MemoryController _mdoQueue;		// Queue for Store requests

        private int _Tstate;                // Current T-state
        private int _madr;                  // Address of word currently on MDI
        private ushort _mdi;                // Result of most recent fetch
        private bool _wait;                 // True if CPU has to wait for memory
        private bool _hold;                 // True if IO hold asserted (not implemented)


        // TODO: Memory board size should be configurable at runtime.
        // For the PERQ 1, the original "quarter meg" board was 256KB (128KW).  Some
        // "half meg" boards existed (512KB/256KW) but were rare; the most common was
        // the 1MB/512KW using 64K RAMs.  Some very rare 2MB/1MW boards used piggyback
        // 128Kb RAMs; the landscape boards used 256Kb RAMs.  It'd be cool to support
        // 'em all -- along with the "multi-meg" board (4MB, and possibly 8MB!) that
        // was available only with the incredibly rare 24-bit T4 machines.
#if TWO_MEG
        private const int _memSize     = 0x100000;  // 2MB == 1MW
        private const int _memSizeMask = 0x0fffff;  // i.e., full 20 bits
#else
        private const int _memSize = 0x80000;   // 1MB was more typical for a PERQ 1
        private const int _memSizeMask = 0x7ffff;
#endif

        private ushort[] _memory;

        private PERQSystem _system;
    }
}
