//
// IOBoard.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO;

namespace PERQemu
{
    /// <summary>
    /// A base class for the PERQ's I/O boards.
    /// </summary>
    /// <remarks>
    /// Each PERQ requires one IO board which contains (at minimum) a Z80
    /// subsystem and hard disk controller.  The EIO board introduced with
    /// the PERQ-2 series adds additional features like an on-board Ethernet
    /// controller a real-time clock chip.
    /// 
    /// The 5-slot card cage also contains two option slots: CPU Option, for
    /// which no dedicated boards were designed, and the IO Option, which
    /// offered a number of configurations.  Because the interface for these
    /// is basically identical (as far as emulation is concerned), all of the
    /// IO and IO Option boards can share this same parent class.  See the
    /// file Docs/IOBoards.txt for more information.
    /// </remarks>

    public abstract class IOBoard : IIODevice
    {
        public IOBoard()
        {
            _portsHandled = new bool[256];
        }

        public abstract void Reset();

        public abstract int IORead(byte port);

        public abstract void IOWrite(byte port, int value);

        public bool HandlesPort(byte port)
        {
            return _portsHandled[port];
        }

        protected void RegisterPorts(byte[] ports)
        {
            foreach (var p in ports)
            {
                if (_portsHandled[p])
                {
                    Trace.Log(LogType.Errors, "Port {0:x} already registered!", p);
                }

                _portsHandled[p] = true;
            }
        }

        public bool LoadDisk(string unit)
        {
            return true;
        }

        public void UnloadDisk(string unit)
        {
        }

        public abstract uint Clock();               // fixme -- to be removed!

        public static readonly int IOFudge = 8;     // fixme -- to be removed!


        protected Z80System _z80;
        protected IStorageController _hardDisk;

        protected PERQSystem _sys;

        private bool[] _portsHandled;
    }
}
