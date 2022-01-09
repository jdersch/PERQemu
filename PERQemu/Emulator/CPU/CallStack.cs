//
// CallStack.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.Processor
{
    public partial class CPU
    {
        /// <summary>
        /// Represents the microengine's 5-level call stack.
        /// </summary>
        /// <remarks>
        /// In the 4K CPU, this is the AMD 2910's 12-bit wide internal stack.
        /// In the expanded 16K processor, external logic adds 2 bits to give
        /// 14 bits of address range.  Note the top bits are independent and
        /// are not affected by overflow from the lower 12 bits; this class
        /// manages that logic.
        /// </remarks>
        private sealed class CallStack
        {
            public CallStack()
            {
                _loStack = new ushort[StackSize];
                _hiStack = new ushort[StackSize];

                _wcsHiMask = (_wcsMask - 0xfff);    // just the high bits
            }

            public void Reset()
            {
                for (int i = 0; i < StackSize; i++)
                {
                    _hiStack[i] = 0;
                    _loStack[i] = 0;
                }

                _loStackPtr = 0;
                _hiStackPtr = 0;

                Log.Debug(Category.Sequencer, "Call stack reset.");
            }

            /// <summary>
            /// Wire up the Am2910 FULL pin, though the PERQ hardware doesn't
            /// actually use it... yet.  See the implementation notes for more.
            /// </summary>
            public bool StackFull
            {
                get { return _loStackPtr == StackLimit; }
            }

            /// <summary>
            /// Pushes a 12-bit address onto the sequencer's callstack.
            /// </summary>
            public void PushLo(ushort address)
            {
                if (_loStackPtr < StackLimit) { _loStackPtr++; }

                _loStack[_loStackPtr] = (ushort)(address & 0xfff);

                Log.Debug(Category.Sequencer, "Pushed {0:x4} onto call stack (lo).", address & 0xfff);
            }

            /// <summary>
            /// Pops a 12-bit address from the sequencer's callstack.
            /// </summary>
            public ushort PopLo()
            {
                ushort address = _loStack[_loStackPtr];

                if (_loStackPtr > 0) { _loStackPtr--; }

                Log.Debug(Category.Sequencer, "Popped {0:x4} from call stack (lo).", address);
                return address;
            }

            /// <summary>
            /// Returns the top of the internal callstack without popping it.
            /// </summary>
            public ushort TopLo()
            {
                Log.Debug(Category.Sequencer, "Returned {0:x4} from top of call stack.", _loStack[_loStackPtr]);
                return _loStack[_loStackPtr];
            }

            /// <summary>
            /// Pushes a full microaddress onto the hybrid callstack.
            /// </summary>
            public void PushFull(ushort address)
            {
                if (_loStackPtr < StackLimit) { _loStackPtr++; }
                if (_hiStackPtr < StackLimit) { _hiStackPtr++; }

                _loStack[_loStackPtr] = (ushort)(address & 0xfff);
                _hiStack[_hiStackPtr] = (ushort)(address & _wcsHiMask);

                Log.Debug(Category.Sequencer, "Pushed {0:x4} onto call stack.", address & _wcsMask);
            }

            /// <summary>
            /// Pops a full microaddress from the hybrid callstack.
            /// </summary>
            public ushort PopFull()
            {
                ushort address = (ushort)(_loStack[_loStackPtr] | _hiStack[_hiStackPtr]);

                if (_loStackPtr > 0) { _loStackPtr--; }
                if (_hiStackPtr > 0) { _hiStackPtr--; }

                Log.Debug(Category.Sequencer, "Popped {0:x4} from call stack.", address);
                return address;
            }

            /// <summary>
            /// Returns the top of the hybrid callstack without popping it.
            /// </summary>
            public ushort TopFull()
            {
                ushort address = (ushort)(_loStack[_loStackPtr] | _hiStack[_hiStackPtr]);

                Log.Debug(Category.Sequencer, "Returned {0:x4} from top of call stack.", address);
                return address;
            }

            /// <summary>
            /// Dumps the contents of the stack on the console (debugging)
            /// </summary>
            public void DumpContents()
            {
                if (Is4K)
                {
                    // Just the low 12 bits
                    Console.WriteLine("CStack Pointer={0}.  Contents:", _loStackPtr);

                    for (int i = 0; i < StackSize; i++)
                    {
                        Console.WriteLine("{0} {1}: {2:x4}",
                                          (i == _loStackPtr ? "=>" : "  "), i, _loStack[i]);
                    }

                }
                else
                {
                    // Both stacks and pointers
                    Console.WriteLine("CStack Pointer Lo={0}  Hi={1}.  Contents:", _loStackPtr, _hiStackPtr);

                    for (int i = 0; i < StackSize; i++)
                    {
                        Console.WriteLine("{0} {1}: {2:x4}   {3} {4}: {5:x4}",
                                         (i == _loStackPtr ? "=>" : "  "), i, _loStack[i],
                                         (i == _hiStackPtr ? "=>" : "  "), i, _hiStack[i]);
                    }
                }
            }

            // Am2910 call stack limits
            private const int StackSize = 6;
            private const int StackLimit = StackSize - 1;

            // Low 12 bits (2910)
            private ushort[] _loStack;
            private int _loStackPtr;

            // Upper bits (extra logic)
            private ushort[] _hiStack;
            private int _hiStackPtr;

            // Housekeeping
            private int _wcsHiMask;
        }
    }
}
