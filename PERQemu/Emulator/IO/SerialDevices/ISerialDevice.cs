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

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// Defines the interface between the PERQ's serial port implementation and
    /// some manner of real or imaginary device (for example, a real serial port
    /// or a file, or a network connection...)
    /// </summary>
    public interface ISerialDevice
    {
        void Reset();

        void Write(byte[] data, int index, int length);
        byte ReadByte();
        int ByteCount { get; }

        void Open();
        void Close();

        bool IsOpen { get; }

        string Port { get; set; }
        int BaudRate { get; set; }
        int DataBits { get; set; }
        StopBits StopBits { get; set; }
        Parity Parity { get; set; }
    }
}
