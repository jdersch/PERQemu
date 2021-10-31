// iiodevice.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO
{
    public interface IIODevice
    {
        /// <summary>
        /// Indicates whether this IODevice instance handles the given IO port.
        /// </summary>
        bool HandlesPort(byte ioPort);

        /// <summary>
        /// Resets the given IO device
        /// </summary>
        void Reset();

        /// <summary>
        /// Clocks the IO device one cycle
        /// </summary>
        uint Clock();

        /// <summary>
        /// Does a read from the given port
        /// </summary>
        int IORead(byte ioPort);

        /// <summary>
        /// Does a write to the given port
        /// </summary>
        void IOWrite(byte ioPort, int value);
    }
}
