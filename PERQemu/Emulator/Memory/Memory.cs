// memory.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

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
    /// Main memory array.  Provides word addressing for normal CPU and I/O
    /// 16-bit accesses, or quad words for 64-bit video fetches.
    /// </summary>
    /// <remarks>
    /// C# purists may howl, but testing under 32-bit Mono showed a 30% speed
    /// increase when shifting video data out as longs, and now that we've 
    /// pretty much been freed/forced to run on 64-bits (on later MacOS, at
    /// any rate) we might as well take advantage.  Word access for normal
    /// CPU and I/O, Quads for video.  Booyah.
    /// </remarks>
    [StructLayout(LayoutKind.Explicit)]
    internal struct Core
    {
        [FieldOffset(0)]
        public ulong[] Quads;

        [FieldOffset(0)]
        public ushort[] Words;
    }

    /// <summary>
    /// Implements the PERQ's Memory board and memory state machine,
    /// which is also connected to the IO bus.
    /// </summary>
    public sealed class MemoryBoard     // TODO : ISystemBoard
    {
        public MemoryBoard(PERQSystem system)
        {
            _system = system;
            _memSize = _system.Config.MemorySizeInBytes / 2;  // KB -> KW
            _memSizeMask = _memSize - 1;

            _memory = new Core();
            _memory.Words = new ushort[_memSize];

            _mdiQueue = new MemoryController(this, "MDI");
            _mdoQueue = new MemoryController(this, "MDO");

            _videoController = new VideoController(_system);
        }

        public void Reset()
        {
            _Tstate = 0;
            _mdi = 0;
            _wait = false;
            _hold = false;

            _mdiQueue.Reset();
            _mdoQueue.Reset();

            Trace.Log(LogType.MemoryState, "Memory: Reset.");
        }

        public int MemSize => _memSize;
        public int MemSizeMask => _memSizeMask;
        public ushort[] Memory => _memory.Words;

        public ushort MDI => _mdi;
        public int  MADR => _mdiQueue.Address;
        public int  MIndex => _mdiQueue.WordIndex;
        public bool MDIValid => _mdiQueue.Valid;
        public bool MDONeeded => _mdoQueue.Valid;
        public bool Wait => _wait;
        public bool Hold => _hold;

        public int TState => _Tstate;
        public bool RopEnabled => _system.CPU.RasterOp.Enabled;
        public VideoController Video => _videoController;


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

            // Execute the fetch
            if (_mdiQueue.Valid)
            {
                _madr = _mdiQueue.Address;
                _mdi = FetchWord(_madr);
            }

            //
            // Set the wait flag if we need to abort the current instruction.
            // If output is pending, we never wait; otherwise we let the combined
            // status of the request queues determine our result.
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

            // Execute the store
            if (_mdoQueue.Valid)
            {
                StoreWord(_mdoQueue.Address, input);
            }
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

        /// <summary>
        /// Fetches one quadword from memory (immediate).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ulong FetchQuad(int address)
        {
            // Fast and furious: does not clip or bounds check!
            ulong data = _memory.Quads[address];

            // Buuuuut... to be useful, it has to be byte swapped.
            // Welp, there goes our salvage, guys.
            return ((data & 0x00ff00ff00ff00ff) << 8) | ((data & 0xff00ff00ff00ff00) >> 8);
        }

        /// <summary>
        /// Fetches one word from memory (immediate).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public ushort FetchWord(int address)
        {
            // Clip address to memsize range and read
            ushort data = _memory.Words[address & _memSizeMask];

            Trace.Log(LogType.MemoryFetch, "Memory: Fetch addr {0:x6} --> {1:x4}",
                      address & _memSizeMask, data);

            return data;
        }

        /// <summary>
        /// Stores one word into memory (immediate).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void StoreWord(int address, ushort data)
        {
            Trace.Log(LogType.MemoryStore, "Memory: Store addr {0:x6} <-- {1:x4}",
                      address & _memSizeMask, data);

            // Clip address to memsize range and write
            _memory.Words[address & _memSizeMask] = data;
        }

        /// <summary>
        /// Return true if the memory request type is a Fetch.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public bool IsFetch(MemoryCycle c)
        {
            return c == MemoryCycle.Fetch ||
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
        // hardware schematics') point of view!
        //
        // Memory requests from the CPU/RasterOp unit are queued up according to
        // some elaborate timing rules.  Because there are several scenarios where
        // requests may overlap, fetches and stores are queued up in separate FIFOs.
        // (This is for emulation only; the hardware doesn't do it that way.)
        //
        // Currently the IO / DMA subsystem cheats and performs its memory accesses
        // directly, but it could at some point be integrated -- allowing us to more
        // accurately emulate the real PERQ which must give up processor cycles for
        // DMA. Thus, the "Hold" field of the microinstruction is basically ignored.
        //
        #endregion


        private Core _memory;
        private MemoryController _mdiQueue;		// Queue for Fetch requests
        private MemoryController _mdoQueue;     // Queue for Store requests
        private VideoController _videoController;

        private int _memSize;
        private int _memSizeMask;

        private int _Tstate;
        private int _madr;
        private ushort _mdi;
        private bool _wait;
        private bool _hold;

        private PERQSystem _system;
    }
}
