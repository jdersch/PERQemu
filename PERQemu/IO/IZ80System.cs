// IZ80System.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.SerialDevices;
using PERQemu.IO.Z80.IOB;
using System;
using System.Collections.Generic;
using System.Text;

namespace PERQemu.IO
{
    /// <summary>
    /// IZ80System provides an abstracted view of the IOB's Z80 subsystem.
    /// This interface is temporary while the new Z80 implementation is underway.
    /// </summary>
    public interface IZ80System
    {
        void Reset();
        void Clock();
        void LoadStatus(int status);
        void LoadData(int data);
        int ReadData();

        // TODO: the rest of likely really belong in IOB eventually
        void LoadFloppyDisk(string path);
        void SaveFloppyDisk(string path);
        void UnloadFloppyDisk();
        void SetSerialPort(ISerialDevice dev);
        string GetSerialPort();
        Queue<byte> FIFO { get; }
        Keyboard Keyboard { get; }
    }
}
