//
// IGPIBDevice.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.GPIB
{
    public delegate void BusWriteDelegate(byte talkerId, byte value, BusStatus flags);

    public interface IGPIBDevice
    {
        /// <summary>
        /// Indicates the address of this GPIB device.
        /// </summary>
        byte DeviceID { get; }

        /// <summary>
        /// Resets the given GPIB device.
        /// </summary>
        void BusReset();

        /// <summary>
        /// Read a byte from the bus (if we're the listener).
        /// </summary>
        void BusRead(byte value, BusStatus flags);

        /// <summary>
        /// Write a byte to the bus (if we're the talker).
        /// </summary>
        void RegisterBusWriteDelegate(BusWriteDelegate writeDelegate);

        /// <summary>
        /// Sets the talker address.
        /// </summary>
        void SetTalker(byte address);

        /// <summary>
        /// Sets the listener address.
        /// </summary>
        void SetListener(byte address);
    }
}
