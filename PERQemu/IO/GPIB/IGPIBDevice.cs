// igpibdevice.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Text;

namespace PERQemu.IO.GPIB
{
    public interface IGPIBDevice
    {
        /// <summary>
        /// Indicates the deviceID of this GPIB device.
        /// </summary>
        byte DeviceID { get; }

        /// <summary>
        /// Resets the given GPIB device.
        /// </summary>
        void Reset();

		/// <summary>
		/// Sets the talker address.
		/// </summary>
		void SetTalker(byte address);

		/// <summary>
		/// Sets the listener address.
		/// </summary>
		void SetListener(byte address);

        /// <summary>
		/// Polls the GPIB device for data (if we're the talker).
        /// </summary>
        void Poll(ref Queue<byte> fifo);

        /// <summary>
		/// Does a write to the device (if we're the listener).
        /// </summary>        
        /// <param name="value"></param>
        void Write(byte value);
    }
}
