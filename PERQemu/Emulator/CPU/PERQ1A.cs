//
// PERQ1A.cs - Copyright (c) 2021-2023 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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

namespace PERQemu.Processor
{
    /// <summary>
    /// The 20-bit PERQ-1A processor, aka "16K CPU", is the workhorse of
    /// the PERQ line.  It can be used in any of the configurations.
    /// </summary>
    public sealed class PERQ1A : CPU
    {
        static PERQ1A()
        {
            _name = "PERQ1A";
            _desc = "PERQ-1A 16K CPU (20-bit)";
            _cycleTime = 170;

            _bits = 20;
            _mask = 0x0fffff;

            _wcsBits = 14;
            _wcsSize = 16384;
            _wcsMask = 0x3fff;
        }

        public PERQ1A(PERQSystem system) : base(system)
        {
        }
    }
}
