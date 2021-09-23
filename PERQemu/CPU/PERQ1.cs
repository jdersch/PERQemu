// PERQ1.cs - Copyright 2021 Josh Dersch (derschjo@gmail.com)
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
    public class PERQ1 : CPU
    {
        static PERQ1()
        {
            // 20-bit, 4K original CPU
            _name = "PERQ1";
            _desc = "PERQ-1 4K CPU (20-bit)";
            _cycleTime = 170;

            _bits = 20;
            _mask = 0x0fffff;

            _wcsBits = 12;
            _wcsSize = 4096;
            _wcsMask = 0x0fff;
        }

        public PERQ1(PERQSystem system) : base(system)
        {
        }
    }
}
