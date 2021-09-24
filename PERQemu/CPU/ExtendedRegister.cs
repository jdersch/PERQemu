// extendedregister.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.Processor
{

    /// <summary>
    /// Represents an "extended" register, used in a number of ways throughout
    /// the PERQ CPU.  Defined as a low half and high half, can be treated as a
    /// single integer value (up to 32 bits).  Represents the 12-, 14- or 16-bit
    /// microaddress, Victim and S registers, or the 20- or 24-bit CPU and ALU
    /// registers.
    /// </summary>
    public struct ExtendedRegister
    {
        public ExtendedRegister(int highBits, int lowBits)
        {
            _loBits = lowBits;
            _loMask = (1 << lowBits) - 1;
            _lo = 0;

            _hiBits = highBits;
            _hiMask = (1 << highBits) - 1;
            _hi = 0;

            Console.WriteLine("new reg: lo {0} bits mask {1:x6} hi {2} bits mask {3:x6}",   // Debug
                             _loBits, _loMask, _hiBits, _hiMask);   
        }

        // todo: Hmm.  perhaps accept any value, and mask on the way out?
        // it seems there are a number of cases where the code assumes more
        // bits than are actually defined -- victim using 0xffff to indicate
        // "unset" for example -- and being too precise has broken things...
        // dang.
        public int Value
        {
            get { return ((_hi << _loBits) | _lo); }
            set
            {
                _lo = (ushort)(value & _loMask);                // Lo = value;
                _hi = (ushort)((value >> _loBits) & _hiMask);   // Hi = value;
            }
        }

        public ushort Lo
        {
            get { return _lo; }
            set { _lo = (ushort)(value & _loMask); }
        }

        public ushort Hi
        {
            get { return _hi; }
            set { _hi = (ushort)((value >> _loBits) & _hiMask); }
        }

        public override string ToString()
        {
            // return Value.ToString();
            return string.Format("[ExtendedRegister: Value={0}, Lo={1}, Hi={2}]", Value, Lo, Hi);   // Debug
        }

        private ushort _lo;
        private ushort _hi;

        private int _loBits, _loMask;
        private int _hiBits, _hiMask;
    }
}