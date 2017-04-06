// memory.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Text;

using PERQemu.CPU;
using PERQemu.IO;

using System.Runtime.Serialization;
using System.Security.Permissions;

namespace PERQemu.Memory
{
    /// <summary>
    /// Implements the PERQ's Memory board and memory state machine,
    /// which is also connected to the IO bus.
    /// </summary>
    [Serializable]
    public sealed class MemoryBoard : ISerializable
    {
        #region Serialization
        [SecurityPermissionAttribute(SecurityAction.LinkDemand,
         Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData(
            SerializationInfo info, StreamingContext context)
        {
            info.AddValue("mdiQueue", _mdiQueue);
            info.AddValue("mdoQueue", _mdoQueue);
            info.AddValue("Tstate", _Tstate);
            info.AddValue("wait", _wait);
            info.AddValue("mdi", _mdi);
            info.AddValue("loadOpFile", _loadOpFile);
            info.AddValue("opFile", _opFile);
            info.AddValue("memory", _memory);
        }

        private MemoryBoard(SerializationInfo info, StreamingContext context)
        {
            _mdiQueue = (QueueController)info.GetValue("mdiQueue", typeof(QueueController));
            _mdoQueue = (QueueController)info.GetValue("mdoQueue", typeof(QueueController));
            _Tstate = info.GetInt32("Tstate");
            _wait = info.GetBoolean("wait");
            _mdi = (ushort)info.GetValue("mdi", typeof(ushort));
            _loadOpFile = info.GetBoolean("loadOpFile");
            _opFile = (byte[])info.GetValue("opFile", typeof(byte[]));
            _memory = (ushort[])info.GetValue("memory", typeof(ushort[]));

            //
            // This is ugly.  Ensure that the singleton static instance is set to this newly
            // rehyrdated version.  This does mean that if more than one MemoryBoard is
            // deserialized, the others will be clobbered.  Oh well?
            //
            _instance = this;
        }

        #endregion

        private MemoryBoard()
        {
            Reset();
        }

        public static MemoryBoard Instance
        {
            get { return _instance; }
        }

        public void Reset()
        {
            _memory = new ushort[_memSize];

            _mdiQueue = new QueueController("MDI");
            _mdoQueue = new QueueController("MDO");

            _Tstate = 0;
            _mdi = 0;
            _wait = false;          // Stalls the CPU for requests issued in the wrong cycle
            _hold = false;          // Locks out the IO system at the request of the CPU (not implemented)

            _loadOpFile = false;

            for (int i = 0; i < 16; i++)
            {
                _opFile[i] = 0xff;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "Memory: Reset.");
#endif
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

        public byte[] OpFile
        {
            get { return _opFile; }
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
        public void Tick()
        {
            // Bump cycle counter
            _Tstate = (_Tstate + 1) & 0x3;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.MemoryState, "\nMemory: Tick! T{0}", _Tstate);
#endif
            _mdiQueue.Clock();
            _mdoQueue.Clock();

            ExecuteFetch();
            AssertWait();
        }

        /// <summary>
        /// Second half of the memory cycle: if a store is pending, writes the output
        /// (from the ALU or current RasterOp result) to memory.  If we're expecting a
        /// word to write and get a null instead, there's a bug or bad microcode somewhere
        /// and we're kinda screwed.
        /// </summary>
        /// <param name="input">Word to write (MDO)</param>
        public void Tock(ushort? input)
        {
            if (input != null)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.MemoryState, "Memory: Tock! T{0} mdoNeeded={1} input={2:x4}", _Tstate, MDONeeded, input);
#endif
                ExecuteStore((ushort)input);
            }
            else
            {
                // uh, nothing we can do but whine and move on?
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.MemoryState, "Memory: Tock! T{0} mdoNeeded={1} input=null", _Tstate, MDONeeded);
#endif
            }
        }

        /// <summary>
        /// Set the CPU Wait flag.
        /// </summary>
        private void AssertWait()
        {
            //
            // If output is pending, we never wait; otherwise we let the status
            // of both request queues determine our result.  This is deceptively
            // simple now... should just move it into Tick() and avoid the call?
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

        public void LoadOpFile()
        {
            // If we're currently doing a Fetch4, Set the loadOpFile flag;
            // this will start the refill on the next T2 state (i.e., next cycle).
            if (_mdiQueue.CurrentCycle == MemoryCycle.Fetch4 && _Tstate == 1 )
            {
#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.OpFile, "OpFile: Load init.");
#endif
                _loadOpFile = true;
            }
        }

        /// <summary>
        /// Requests a specific memory cycle type at the specified address.  Under some
        /// circumstances the memory controller will simply ignore requests that are
        /// issued during the wrong cycle; otherwise they may be held until the correct
        /// cycle comes around.
        /// </summary>
        /// <param name="cycleType">Any Fetch or Store type</param>
        /// <param name="address">Starting address</param>
        /// <param name="id">Transaction ID</param>
        public void RequestMemoryCycle(MemoryCycle cycleType, int address, long id)
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MemoryState, "\nMemory: Requested {0} cycle in T{1} ID={2} addr={3:x5}",
                                                cycleType, _Tstate, id, address);
#endif
            // *** TODO: need to put back in the "ignore" processing...? ***
            //      (See the hairy memory rules in QueueController.cs)

            //
            // Queue up the request.  We're in no-man's land at the bottom of the cycle,
            // but the queue controller will make sure that requests that are issued at
            // precisely the correct time will start at the next Tick().
            //
            if (IsFetch(cycleType))
            {
                _mdiQueue.Request(cycleType, address, id);
            }
            else
            {
                _mdoQueue.Request(cycleType, address, id);
            }
        }

        /// <summary>
        /// If the current MDI word is valid, fetches and returns the data in _mdi.
        /// If we're executing a LoadOp, copies the data into the appropriate word
        /// of the OpFile.  If the MDI pipeline is empty, is a no-op.
        /// </summary>
        private void ExecuteFetch()
        {
            if (_mdiQueue.Valid)
            {
                _madr = _mdiQueue.Address;
                _mdi = FetchWord(_madr);

                // Are we loading the Op file with the data?
                if (_loadOpFile)
                {
                    int opAddr = _mdiQueue.WordIndex * 2;
                    _opFile[opAddr] = (byte)(_mdi & 0xff);
                    _opFile[opAddr + 1] = (byte)((_mdi & 0xff00) >> 8);

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                    {
                        Trace.Log(LogType.OpFile, "Loaded {0:x2} into OpFile[{1:x}] from {2:x5}", _opFile[opAddr], opAddr, _madr);
                        Trace.Log(LogType.OpFile, "Loaded {0:x2} into OpFile[{1:x}] from {2:x5}", _opFile[opAddr + 1], opAddr + 1, _madr);
                    }
#endif
                    // Was that the last word in the quad?
                    if (_mdiQueue.WordIndex == 3)
                    {
                        _loadOpFile = false;
                    }
                }
            }
        }

        /// <summary>
        /// If the current MDO word is valid, write the data to the current address.
        /// If the MDO pipeline is empty, this is a no-op.
        /// </summary>
        /// <param name="data"></param>
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
        /// <param name="address"></param>
        /// <returns></returns>
        public ushort FetchWord(int address)
        {
            // Clip address to memsize range and read
            ushort data = _memory[address & _memSizeMask];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MemoryFetch, "Memory: Fetch {0:x4} <-- addr {1:x5}", data, address & _memSizeMask);
#endif
            return data;
        }

        /// <summary>
        /// Stores one word into memory (immediate).
        /// </summary>
        /// <param name="address"></param>
        /// <param name="data"></param>
        public void StoreWord(int address, ushort data)
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.MemoryStore, "Memory: Store {0:x4} --> addr {1:x5}", data, address & _memSizeMask);
#endif
            // Clip address to memsize range and write
            _memory[address & _memSizeMask] = data;
        }

        /// <summary>
        /// Return true if the memory request type is a Fetch.
        /// </summary>
        public bool IsFetch(MemoryCycle c)
        {
            return  c == MemoryCycle.Fetch ||
                    c == MemoryCycle.Fetch2 ||
                    c == MemoryCycle.Fetch4 ||
                    c == MemoryCycle.Fetch4R;
        }

        public void DumpQueues()
        {
            // dump out the queues... from the debugger... nice to hook this up...
        }

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
        // accurately emulate the real PERQ, but at some penalty in performance.  Thus,
        // the "Hold" field of the microinstruction is basically ignored.
        //
        // The MDI and MDO Queues are little pipelines of "instructions" that are
        // executed during specific T-states.  As incoming memory requests are "recognized"
        // (made ready for execution) instructions are issued to the proper instruction
        // queue to perform the actual reads/writes to memory, one per queue per cycle.
        // This allows all the various overlapped fetch/store modes (including RasterOp)
        // to work without a vastly complex state machine to track all the arcane PERQ
        // timing requirements.
        //
        #endregion

        /// <summary>
        /// Queue for Fetch requests.
        /// </summary>
        private QueueController _mdiQueue;

        /// <summary>
        /// Queue for Store requests.
        /// </summary>
        private QueueController _mdoQueue;

        private int _Tstate;                // Current T-state
        private int _madr;                  // Address of word currently on MDI (helpful in debugging RasterOp)
        private ushort _mdi;                // Result of most recent fetch
        private bool _wait;                 // True if CPU has to wait for memory
        private bool _hold;                 // True if IO hold asserted (not implemented)


        // Memory board size should at some point be configurable at runtime...
#if TWO_MEG
        private const int _memSize     = 0x100000;  // 2MB == 1MW
        private const int _memSizeMask = 0x0fffff;  // i.e., full 20 bits
#else
        private const int _memSize     = 0x80000;   // 1MB was more typical for a PERQ 1
        private const int _memSizeMask = 0x7ffff;
#endif
        /// <summary>
        /// The PERQ memory array.
        /// </summary>
        private ushort[] _memory;

        /// <summary>
        /// The opfile.  This probably belongs to the CPU, but since it's loaded
        /// by the memory state machine, I do declare that I'm putting it here!
        /// </summary>
        private byte[] _opFile = new byte[16];      // 16??  8 x 8...

        private bool _loadOpFile;

        private static MemoryBoard _instance = new MemoryBoard();

    }
}
