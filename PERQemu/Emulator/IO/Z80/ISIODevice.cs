//
// ISIODevice.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Z80
{

    public delegate void ReceiveDelegate(byte rxValue);

    /// <summary>
    /// Generic interface for devices connected to a Z80 SIO controller.
    /// "Transmit" functions are FROM the SIO, TO the device implementation;
    /// likewise "Receive" functions are FROM the device, TO the SIO.
    /// </summary>
    public interface ISIODevice
    {
        /// <summary>
        /// Resets the device.
        /// </summary>
        void Reset();

        /// <summary>
        /// Registers a delegate used by the ISIODevice implementation to
        /// send data to the SIO's receiver.
        /// </summary>
        void RegisterReceiveDelegate(ReceiveDelegate rxDelegate);

        /// <summary>
        /// Writes a byte to the SIO device
        /// </summary>
        void Transmit(byte value);

        /// <summary>
        /// Sends an RS232 Break to the device.
        /// </summary>
        void TransmitBreak();

        /// <summary>
        /// Sends an SDLC Abort to the device.
        /// </summary>
        void TransmitAbort();
    }
}
