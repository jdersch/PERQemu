// callstack.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.CPU
{
    /// <summary>
    /// Represents a 16k CPU call stack, which on a real PERQ is implemented as an
    /// AMD 2901's stack and some external glue for the top 2 bits (literally, the
    /// "two bit kluge").  The top two bits are independent and are not affected by
    /// overflow from the lower 12 bits; this class manages that logic.
    ///
    /// </summary>

    /// Implementation note:
    /// The Am2910 documentation (Am2900 Family Data Book, pg 2-136) clearly states
    /// the behavior of the 2910's internal stack:  The pointer does NOT wrap.  When
    /// the stack pointer is reset to zero, the contents are undefined until a PUSH
    /// occurs; accordingly, any POP when the stack is empty is an undefined result.
    /// When the FULL condition is reached, a sixth PUSH overwrites the top word.
    /// Curiously, the FULL pin on the 2910 is not connected in the PERQ CPU (though
    /// it could have been wired to an unused bit in the microstate register?) so
    /// there is no way to know when you've exceeded the maximum stack depth!
    ///
    /// I've slightly tweaked Josh's original implementation to more accurately reflect
    /// the Am2910 documentation.  The stack is now an array [1..5] instead of [0..4],
    /// and the two stack pointers always point to the current top.  This fixes the a
    /// somewhat obscure case where TopLo() would attempt to read beyond the end of the
    /// array and barf.
    ///
    /// PERQ quirk: it appears that Boot or VFY is leaking a push... but SYSB clears
    /// the stack before entering the interpreter.

    public sealed class CallStack
    {
        public void Clear()
        {
            for (int i = 0; i < _cStackSize; i++)
            {
                _cStackHi[i] = 0;
                _cStackLo[i] = 0;
            }
        }

        public void Reset()
        {
            _cStackPointerLo = 0;
            _cStackPointerHi = 0;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.CpuState, "Call stack: Reset.");
#endif
        }

        /// <summary>
        /// Dumps the contents of the stack on the console (debugging)
        /// </summary>
        public void DumpContents()
        {
#if SIXTEEN_K
            // 16K CPU - print both stacks and pointers
            Console.WriteLine("CStack Pointer Lo={0}  Hi={1}.  Contents:", _cStackPointerLo, _cStackPointerHi);

            for (int i = 0; i < _cStackSize; i++)
            {
                Console.WriteLine("{0} {1}: {2:x5}   {3} {4}: {5:x5}",
                    (i == _cStackPointerLo ? "=>" : "  "), i, _cStackLo[i],
                    (i == _cStackPointerHi ? "=>" : "  "), i, _cStackHi[i]);
            }
#else
            // 4K CPU - just the low 12 bits
            Console.WriteLine("CStack Pointer={0}.  Contents:", _cStackPointerLo);

            for (int i = 0; i < _cStackSize; i++)
            {
                Console.WriteLine("{0} {1}: {2:x5}",
                    (i == _cStackPointerLo ? "=>" : "  "), i, _cStackLo[i]);
            }
#endif
        }

        /// <summary>
        /// Pushes a 12-bit address onto the sequencer's callstack
        /// </summary>
        public void PushLo(ushort address)
        {
            if (_cStackPointerLo < _cStackLimit) { _cStackPointerLo++; }

            _cStackLo[_cStackPointerLo] = (ushort)(address & 0xfff);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.CpuState, _cStackPointerLo, "Pushed {0:x5} onto call stack (lo).", address & 0xfff);
#endif
        }

        /// <summary>
        /// Pops a 12-bit address from the sequencer's callstack
        /// </summary>
        public ushort PopLo()
        {
            ushort address = _cStackLo[_cStackPointerLo];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.CpuState, _cStackPointerLo, "Popped {0:x5} from call stack (lo).", address);
#endif

            if (_cStackPointerLo > 0) { _cStackPointerLo--; }

            return address;
        }

        /// <summary>
        /// Returns the top of the sequencer's internal callstack without popping it.
        /// </summary>
        public ushort TopLo()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.CpuState, "Returned {0:x5} from top of call stack.", _cStackLo[_cStackPointerLo]);
#endif
            return _cStackLo[_cStackPointerLo];
        }

        /// <summary>
        /// Pushes a full 14-bit address onto the hybrid callstack
        /// </summary>
        public void PushFull(ushort address)
        {
            if (_cStackPointerLo < _cStackLimit) { _cStackPointerLo++; }
            if (_cStackPointerHi < _cStackLimit) { _cStackPointerHi++; }

            _cStackLo[_cStackPointerLo] = (ushort)(address & 0xfff);
            _cStackHi[_cStackPointerHi] = (ushort)(address & 0x3000);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.CpuState, _cStackPointerLo, "Pushed {0:x5} onto call stack.", address & 0x3fff);
#endif
        }

        /// <summary>
        /// Pops a full 14-bit address from the hybrid callstack
        /// </summary>
        /// <returns></returns>
        public ushort PopFull()
        {
            ushort address = (ushort)(_cStackLo[_cStackPointerLo] | _cStackHi[_cStackPointerHi]);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.CpuState, _cStackPointerLo, "Popped {0:x5} from call stack.", address);
#endif

            if (_cStackPointerLo > 0) { _cStackPointerLo--; }
            if (_cStackPointerHi > 0) { _cStackPointerHi--; }

            return address;
        }

        /// <summary>
        /// Returns the top of the callstack without popping it
        /// </summary>
        public ushort TopFull()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.CpuState, "Returned {0:x5} from top of call stack.",
                                            _cStackLo[_cStackPointerLo] | _cStackHi[_cStackPointerHi]);
#endif
            return (ushort)(_cStackLo[_cStackPointerLo] | _cStackHi[_cStackPointerHi]);
        }

        private const int _cStackSize = 6;
        private const int _cStackLimit = _cStackSize - 1;

        // Low 12 bits (2901)
        private ushort[] _cStackLo = new ushort[_cStackSize];
        private int _cStackPointerLo;

        // Upper 2 bits (extra logic)
        private ushort[] _cStackHi = new ushort[_cStackSize];
        private int _cStackPointerHi;
    }
}

