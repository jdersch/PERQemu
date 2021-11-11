//
// IDMADevice.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80
{
    public interface IDMADevice
    {
        /// <summary>
        /// Indicates that the device has data ready to read.
        /// </summary>
        bool ReadDataReady { get; }

        /// <summary>
        /// Indicates that the device is ready for data to be written.
        /// </summary>
        bool WriteDataReady { get; }

        /// <summary>
        /// Terminates the current transfer (equivalent to setting TC).
        /// </summary>
        void DMATerminate();
    }
}
