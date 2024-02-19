//
// PERQ1.cs - Copyright (c) 2021-2024 Josh Dersch (derschjo@gmail.com)
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
    /// This class implements the original PERQ 20-bit CPU with 4K of
    /// writable control store.
    /// </summary>
    /// <remarks>
    /// This CPU is only available in the PERQ-1 or very first generation
    /// of the PERQ-2.  It's here mostly for historical completeness, as
    /// much of the really interesting software (Accent and PNX) eventually
    /// dropped support for it.
    /// </remarks>
    public sealed class PERQ1 : CPU
    {
        static PERQ1()
        {
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
