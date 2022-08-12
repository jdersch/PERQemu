//
// ISerialDevice.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using System.IO.Ports;

using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// For "real" devices, this extended delegate allows hardware status changes
    /// to be sent to the SIO with a recieved character, or asynchronously.
    /// </summary>
    public delegate void ReceiveStatusDelegate(byte rxValue, CharStatus rxStatus);

    /// <summary>
    /// Defines the interface between a "real" programmable serial port on the
    /// host (or an imaginary device that behaves like one, e.g., RSX:) and the
    /// PERQ's byte oriented serial port implementation.  The "hooks" here allow
    /// setting the parameters for the physical data transfer -- data bits, stop
    /// bits, parity, etc. -- by the SIO device.  Must implement ISIODevice to
    /// move data in or out of the virtual machine.
    /// </summary>
    public interface ISerialDevice : ISIODevice
    {
        /// <summary>
        /// Basic file ops.
        /// </summary>
        void Open();
        void Close();

        // Debugging
        void Status();

        /// <summary>
        /// Tell the device that the timer (baud rate clock) has changed.
        /// For PERQ-1 IOB/CIO, the Z80 CTC provides the baud clock (x16).
        /// For PERQ-2 EIO, the i8254 chip does it [not yet implemented].
        /// </summary>
        void NotifyRateChange(int newRate);

        /// <summary>
        /// Registers the extended status delegate.
        /// </summary>
        void RegisterStatusDelegate(ReceiveStatusDelegate rxDelegate);

        //
        // Properties
        //
        bool IsOpen { get; }
        int ByteCount { get; }
        int BaudRate { get; }

        string Port { get; set; }
        int DataBits { get; set; }
        Parity Parity { get; set; }
        StopBits StopBits { get; set; }

        //
        // Modem control pins
        //
        bool DTR { get; set; }
        bool RTS { get; set; }

        bool DCD { get; }
        bool CTS { get; }
        bool DSR { get; }

        // todo: handshaking?  errors?  shit.  SIO "knows" how to program
        // the pins so that has to be exposed here too...  or do we quietly
        // ignore that?
    }
}
