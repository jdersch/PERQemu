// shifter.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace PERQemu.CPU
{
    [Serializable]
    public sealed class Shifter : ISerializable
    {
        #region Serialization
        [SecurityPermissionAttribute(SecurityAction.LinkDemand,
            Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData(
            SerializationInfo info, StreamingContext context)
        {
            info.AddValue("shifterOutput", _shifterOutput);
            info.AddValue("currentShifterParams", _shifterParams);
        }

        public Shifter(SerializationInfo info, StreamingContext context)
        {
            _shifterOutput = info.GetUInt16("shifterOutput");
            _shifterParams = (ShifterTableEntry)info.GetValue("currentShifterParams", typeof(ShifterTableEntry));

            BuildShifterTable();

            _instance = this;
        }

        #endregion

        public Shifter()
        {
        }

        static Shifter()
        {
            BuildShifterTable();
        }

        public static Shifter Instance
        {
            get { return _instance; }
        }

        /// <summary>
        /// Set up the shifter according to the PERQ's encoding rules
        /// (two 4-bit nibbles packed into 1 command byte).
        /// </summary>
        public void SetShifterCommand(int input)
        {
            _shifterParams = _shifterTable[input & 0xff];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Shifter, "Shifter: Command={0} amount={1} mask={2:x4}",
                    _shifterParams.Command, _shifterParams.ShiftAmount, _shifterParams.ShiftMask);
#endif
        }

        /// <summary>
        /// Set the shifter command the easy way (internal calls).
        /// </summary>
        public void SetShifterCommand(ShifterCommand cmd, int amt, int mask)
        {
            _shifterParams.Command = cmd;
            _shifterParams.ShiftAmount = amt;
            _shifterParams.ShiftMask = mask;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Shifter, "Shifter: Command={0} amount={1} mask={2:x4}",
                    _shifterParams.Command, _shifterParams.ShiftAmount, _shifterParams.ShiftMask);
#endif
        }

        /// <summary>
        /// Applies shifter logic to the given input word.
        /// </summary>
        /// <param name="input"></param>
        public void Shift(int input)
        {
            DoShift(input, input);
        }

        /// <summary>
        /// Applies shifter logic to the given input words.
        /// </summary>
        /// <param name="low"></param>
        /// <param name="high"></param>
        public void Shift(int low, int high)
        {
            DoShift(low, high);
        }

        /// <summary>
        /// Performs left & right shifts (low word only) or combines the two
        /// (high + low) to do rotates or field operations.  The ability to
        /// specify two separate input words is used by RasterOp's "half word
        /// pipeline."
        /// </summary>
        /// <param name="low"></param>
        /// <param name="high"></param>
        private void DoShift(int low, int high)
        {
            uint d;

            switch (_shifterParams.Command)
            {
                case ShifterCommand.LeftShift:
                    _shifterOutput = (ushort)(low << _shifterParams.ShiftAmount);
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Shifter, "Left shift by {0}: in={1}{2} out={3}",
                                                   _shifterParams.ShiftAmount, high, low, _shifterOutput);
#endif
                    break;

                case ShifterCommand.RightShift:
                    _shifterOutput = (ushort)((low & 0x0ffff) >> _shifterParams.ShiftAmount);    // logical, not arithmetic.
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Shifter, "Right shift by {0}: in={1}{2} out={3}",
                                                   _shifterParams.ShiftAmount, high, low, _shifterOutput);
#endif
                    break;

                case ShifterCommand.Rotate:
                    d = (uint)(((high & 0xffff) << 16) | (low & 0xffff));   // 32 bits
                    _shifterOutput = (ushort)(0xffff & (d >> _shifterParams.ShiftAmount));
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Shifter, "Rotate by {0}: in={1}{2} out={3}",
                                                   _shifterParams.ShiftAmount, high, low, _shifterOutput);
#endif
                    break;

                case ShifterCommand.Field:
                    d = (uint)(((high & 0xffff) << 16) | (low & 0xffff));   // 32 bits
                    _shifterOutput = (ushort)(((d >> _shifterParams.ShiftAmount)) & _shifterParams.ShiftMask);
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Shifter, "Field mask {0} rotated by {1}",
                                                   _shifterParams.ShiftMask, _shifterParams.ShiftAmount);
#endif
                break;
            }
        }

        public ushort ShifterOutput
        {
            get { return _shifterOutput; }
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

        // Shifter output
        private ushort _shifterOutput;
        private ShifterTableEntry _shifterParams;

        [Serializable]
        private struct ShifterTableEntry
        {
            public ShifterCommand Command;
            public int ShiftAmount;
            public int ShiftMask;
        }

        private static ShifterTableEntry[] _shifterTable;
        private static Shifter _instance;
    }
}

