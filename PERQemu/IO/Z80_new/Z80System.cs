// Z80system.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using Konamiman.Z80dotNet;
using PERQemu.IO.SerialDevices;
using PERQemu.IO.Z80.IOB;
using System;
using System.Collections.Generic;

namespace PERQemu.IO.Z80_new
{
    public class Z80System : IZ80System
    {
        public Z80System(PERQSystem system)
        {
            _cpu = new Z80Processor();
        }

        public void Reset()
        {
            _cpu.Reset();
            _cpu.Memory = new IOBMemoryBus();
            _cpu.PortsSpace = new IOBIOBus();
        }

        /// <summary>
        /// Runs the Z80 for one instruction.
        /// </summary>
        /// <returns></returns>
        public void Clock()
        {
            // TODO:
            // Clock devices.
            _cpu.ExecuteNextInstruction();
        }

        public void LoadData(int data)
        {

        }

        public int ReadData()
        {
            return 0;
        }

        public void LoadStatus(int status)
        {

        }
        
        public void LoadFloppyDisk(string path) { }

        public void SaveFloppyDisk(string path) { }

        public void UnloadFloppyDisk() { }

        public void SetSerialPort(ISerialDevice dev) { }

        public string GetSerialPort() { return String.Empty; }

        public Queue<byte> FIFO { get; }

        public Keyboard Keyboard { get { return null; } }

        private Z80Processor _cpu;
    }
}
