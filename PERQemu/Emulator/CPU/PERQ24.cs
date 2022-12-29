// PERQ24.cs - Copyright 2021 Josh Dersch (derschjo@gmail.com)
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
    /// <summary>
    /// This is the rare 24-bit, 16K CPU.  It is available when the
    /// "PERQ-2/T4" (aka "PERQ-4" or "PQ4000") configuration is selected.
    /// </summary>
    public sealed class PERQ24 : CPU
    {
        static PERQ24()
        {
            _name = "PERQ24";
            _desc = "PERQ24 16K CPU (24-bit)";
            _cycleTime = 170;

            _bits = 24;
            _mask = 0xffffff;

            _wcsBits = 14;
            _wcsSize = 16384;
            _wcsMask = 0x3fff;
        }

        public PERQ24(PERQSystem system) : base(system)
        {
        }

        public override int ReadMicrostateRegister(byte h)
        {
            if (h == 1)
            {
                // On PERQ24, uState1 is the upper 8 Bmux bits
                return  ((~_lastBmux) >> 16) & 0xff;
            }
            else
            {
                // Return the usual 20-bit uState
                return base.ReadMicrostateRegister(h);
            }
        }
    }
}