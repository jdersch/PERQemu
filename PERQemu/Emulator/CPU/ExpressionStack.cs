//
// ExpressionStack.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
        /// Implements the PERQ's 16-level push-down Expression stack.
        /// Handles 20- or 24-bit values.
        /// </summary>
        protected class ExpressionStack
        {
            public ExpressionStack()
            {
                _stack = new int[16];
            }

            /// <summary>
            /// Reset the Estack object (emulator).
            /// </summary>
            public void Reset()
            {
                for (int i = 0; i < 16; i++)
                {
                    _stack[i] = 0;
                }

                _stackPointer = 0;
                Trace.Log(LogType.EStack, "Estack: Reset.");
            }

            /// <summary>
            /// Reset the expression stack (microcode).
            /// </summary>
            public void StackReset()
            {
                _stackPointer = 0;
                Trace.Log(LogType.EStack, "StackReset.");
            }

            /// <summary>
            /// Gets or sets the top element in the stack.
            /// </summary>
            public int TOS
            {
                get { return _stack[_stackPointer]; }
                set
                {
                    _stack[_stackPointer] = value & CPUMask;
                    Trace.Log(LogType.EStack, "TOS set to {0:x6}.", value);
                }
            }

            /// <summary>
            /// Gets the stack empty flag (used in Microstate register).
            /// </summary>
            public bool StackEmpty
            {
                get { return (_stackPointer == 0); }
            }

            /// <summary>
            /// Push a word value onto the EStack.
            /// </summary>
            public void Push(int value)
            {
                _stackPointer++;

                // Check for stack overflow.  According to the PERQ documentation,
                // behavior in this state is "undefined."  However, some code seems
                // to be buggy (i.e. leaks pops/pushes) and depends on the EStack
                // pointer wrapping around. (For example, PERQMan's random number
                // generator microcode does one too few pops before returning...)
                // The hardware doesn't enforce stack limits, so we don't either.
                if (_stackPointer > 15)
                {
                    Trace.Log(LogType.Errors, "Estack Overflow!");
                    _stackPointer = 0;
                }

                _stack[_stackPointer] = value & CPUMask;    // TOS = value;

                Trace.Log(LogType.EStack, "Estack pushed {0:x6}, pointer now {1}.",
                                          _stack[_stackPointer], _stackPointer);
            }

            /// <summary>
            /// Pop a word value from the EStack.
            /// </summary>
            public void Pop()
            {
                _stackPointer--;

                // Check for stack underflow.  See Push().
                if (_stackPointer < 0)
                {
                    Trace.Log(LogType.Errors, "Estack Underflow!");
                    _stackPointer = 15;
                }

                Trace.Log(LogType.EStack, "Estack popped, pointer now {0}.", _stackPointer);
            }

            /// <summary>
            /// Dumps the stack contents on the console (debugging).
            /// </summary>
            public void DumpContents()
            {
                Console.WriteLine("EStack Pointer={0}.  Contents:", _stackPointer);

                for (int i = 0; i < 16; i++)
                {
                    Console.WriteLine("{0} {1:00}: {2:x6}",
                                      (i == _stackPointer ? "=>" : "  "), i, _stack[i]);
                }
            }

            private int[] _stack;
            private int _stackPointer;
        }
    }
}