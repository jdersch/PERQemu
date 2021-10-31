// shifter.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.Processor
{
    public partial class CPU
    {

        public enum ShifterCommand
        {
            LeftShift = 0,
            RightShift,
            Rotate,
            Field
        }

        /// <summary>
        /// Implements the PERQ's 32-bit barrel shifter, which can do left and
        /// right shifts, rotates, or bitfield extractions every cycle.  Though
        /// the hardware only has one shared shifter datapath, we cheat a little
        /// here and allow the MQ/MulDiv unit to have its own, and the RasterOp
        /// unit to have one too.  That's a pretty big efficiency win, since we
        /// don't have to potentially reprogram the shifter command on every cycle.
        /// </summary>
        public sealed class Shifter
        {
            static Shifter()
            {
                BuildShifterTable();
            }

            public ushort ShifterOutput
            {
                get { return _output; }
            }

            /// <summary>
            /// Set up the shifter according to the PERQ's encoding rules
            /// (two 4-bit nibbles packed into 1 command byte).
            /// </summary>
            public void SetShifterCommand(int input)
            {
                _params = _shifterTable[input & 0xff];

                Trace.Log(LogType.Shifter, "Command={0} amount={1} mask={2:x4}",
                          _params.Command, _params.ShiftAmount, _params.ShiftMask);
            }

            /// <summary>
            /// Set the shifter command the easy way (internal calls).
            /// </summary>
            public void SetShifterCommand(ShifterCommand cmd, int amt, int mask)
            {
                _params.Command = cmd;
                _params.ShiftAmount = amt;
                _params.ShiftMask = mask;

                Trace.Log(LogType.Shifter, "Command={0} amount={1} mask={2:x4}",
                          _params.Command, _params.ShiftAmount, _params.ShiftMask);
            }

            /// <summary>
            /// Applies shifter logic to the given input word.
            /// </summary>
            public void Shift(int input)
            {
                Shift(input, input);
            }

            /// <summary>
            /// Performs left and right shifts (low word only) or combines the two
            /// (high + low) to do rotates or field operations.  The ability to
            /// specify two separate input words is used by RasterOp's "half word
            /// pipeline."
            /// </summary>
            public void Shift(int low, int high)
            {
                uint d;

                switch (_params.Command)
                {
                    case ShifterCommand.LeftShift:
                        _output = (ushort)(low << _params.ShiftAmount);

                        Trace.Log(LogType.Shifter, "Left shift by {0}: in={1:x4} out={2:x4}",
                                  _params.ShiftAmount, low, _output);
                        break;

                    case ShifterCommand.RightShift:
                        _output = (ushort)((low & 0x0ffff) >> _params.ShiftAmount);    // logical, not arithmetic

                        Trace.Log(LogType.Shifter, "Right shift by {0}: in={1:x4} out={2:x4}",
                                  _params.ShiftAmount, low, _output);
                        break;

                    case ShifterCommand.Rotate:
                        d = (uint)(((high & 0xffff) << 16) | (low & 0xffff));   // 32 bits
                        _output = (ushort)(0xffff & (d >> _params.ShiftAmount));

                        Trace.Log(LogType.Shifter, "Rotate by {0}: in={1:x4}{2:x4} out={3:x4}",
                                  _params.ShiftAmount, high, low, _output);
                        break;

                    case ShifterCommand.Field:
                        d = (uint)(((high & 0xffff) << 16) | (low & 0xffff));   // 32 bits
                        _output = (ushort)(((d >> _params.ShiftAmount)) & _params.ShiftMask);

                        Trace.Log(LogType.Shifter, "Field mask {0} rotated by {1} out={2:x4}",
                                  _params.ShiftMask, _params.ShiftAmount, _output);
                        break;
                }
            }

            /// <summary>
            /// Precomputes all possible shifter values to make computation at runtime cheap.
            /// </summary>
            private static void BuildShifterTable()
            {
                _shifterTable = new ShifterTableEntry[0x100];

                for (int i = 0; i < 0x100; i++)
                {
                    int low = (~i) & 0x0f;
                    int high = ((~i) & 0xf0) >> 4;

                    // See if this is a left or right shift, or a rotate
                    if (low == 0xf)
                    {
                        _shifterTable[i].Command = ShifterCommand.LeftShift;
                        _shifterTable[i].ShiftAmount = high;
                    }
                    else if ((0xf - low) == high)
                    {
                        _shifterTable[i].Command = ShifterCommand.RightShift;
                        _shifterTable[i].ShiftAmount = high;
                    }
                    else if (
                        (low == 0xd || low == 0xe) &&
                        (high >= 0x8 && high <= 0xf))
                    {
                        _shifterTable[i].Command = ShifterCommand.Rotate;
                        _shifterTable[i].ShiftAmount = (high & 0x7) | (low == 0xd ? 0x0 : 0x8);
                    }
                    else
                    {
                        _shifterTable[i].Command = ShifterCommand.Field;
                        _shifterTable[i].ShiftAmount = high;
                        _shifterTable[i].ShiftMask = (0x1ffff >> (0x10 - low));
                    }
                }
            }

            private struct ShifterTableEntry
            {
                public ShifterCommand Command;
                public int ShiftAmount;
                public int ShiftMask;
            }

            private ushort _output;
            private ShifterTableEntry _params;
            private static ShifterTableEntry[] _shifterTable;
        }
    }
}