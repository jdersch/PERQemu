// extendedregister.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
    /// Represents a 16k CPU "extended" register -- a 12-bit register with 2 extra bits tacked
    /// on.  These are used for the PC and S registers, and have some unique behaviors...
    /// </summary>
    public struct ExtendedRegister
    {
        /// <summary>
        /// The low 12 bits
        /// </summary>
        public ushort Lo
        {
            get { return _lo; }
            set { _lo = (ushort)(value & 0xfff); }
        }

        /// <summary>
        /// The high 2 bits
        /// </summary>
        public ushort Hi
        {
            get { return _hi; }
            set { _hi = (ushort)(value & 0x3); }
        }

        /// <summary>
        /// The whole 14 bit value
        /// </summary>
        public ushort Value
        {
            get
            {
#if SIXTEEN_K
                return (ushort)(_lo | (_hi << 12));
#else
                return (ushort)(_lo);
#endif
            }
            set
            {
                _lo = (ushort)(value & 0xfff);
#if SIXTEEN_K
                _hi = (ushort)((value >> 12) & 0x3);
#else
                _hi = 0;
#endif
            }
        }

        public override string ToString()
        {
            return Value.ToString();
        }

        private ushort _lo;
        private ushort _hi;
    }
}
