//
// IZ80System.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.Z80;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO
{
    public enum ExecutionMode
    {
        Synchronous,
        Asynchronous
    }

    /// <summary>
    /// IZ80System provides an abstracted view of the IOB's Z80 subsystem.
    /// This interface is temporary while the new Z80 implementation is underway.
    /// </summary>
    public interface IZ80System
    {
        bool SupportsAsync { get; }

        /// <summary>
        /// Resets the Z80 system.
        /// </summary>
        void Reset();

        /// <summary>
        /// Runs the Z80 system continuously on its own thread.
        /// </summary>
        void RunAsync();

        /// <summary>
        /// Executes one instruction and returns the cycles consumed in doing so.
        /// If system is running asynchronously, processor is halted afterwards.
        /// </summary>
        /// <returns></returns>
        uint SingleStep();

        /// <summary>
        /// Stops execution.  Only applicable to Asynchronous mode.
        /// </summary>
        void Stop();

        /// <summary>
        /// Writes to the Z80 Status register
        /// </summary>
        /// <param name="status"></param>
        void WriteStatus(int status);

        /// <summary>
        /// Writes to the Z80 Data register
        /// </summary>
        /// <param name="data"></param>
        void WriteData(int data);

        /// <summary>
        /// Reads from the Z80 Data register
        /// </summary>
        /// <returns></returns>
        int ReadData();

        // Debugging stuff
        void ShowZ80State();

        // TODO: the rest of these really belong in IOB eventually.
        void LoadFloppyDisk(string path);
        void SaveFloppyDisk(string path);
        void UnloadFloppyDisk();
        void SetSerialPort(ISerialDevice dev);
        string GetSerialPort();
        Queue<byte> FIFO { get; }
        Keyboard Keyboard { get; }
    }
}
