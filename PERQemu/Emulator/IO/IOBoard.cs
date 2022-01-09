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

using PERQemu.IO.Z80;
using PERQemu.IO.HardDisk;

namespace PERQemu.IO
{
    /// <summary>
    /// A base class for the PERQ's I/O boards.
    /// </summary>
    /// <remarks>
    /// Each PERQ requires one IO board which contains (at minimum) a Z80
    /// subsystem and hard disk controller.  The EIO board introduced with
    /// the PERQ-2 series adds an on-board Ethernet controller, and additional
    /// Z80 peripherals like a second serial port and real-time clock chip.
    /// </remarks>
    public abstract class IOBoard : IIODevice
    {
        static IOBoard()
        {
            // Our derived classes customize themselves in the static
            // constructor, setting up ports and other goodies
        }

        public IOBoard(PERQSystem system)
        {
            _sys = system;
        }

        public static string Name => _name;
        public static string Description => _desc;
        public static ulong Z80CycleTime => _z80CycleTime;
        public static int Z80_RAM_SIZE => _z80RamSize;
        public static int Z80_RAM_ADDRESS => _z80RamAddr;
        public static int Z80_ROM_SIZE => _z80RomSize;
        public static int Z80_ROM_ADDRESS => _z80RomAddr;

        public Z80System Z80System
        {
            get { return _z80System; }
        }

        // public IStorageController DiskController
        public ShugartDiskController DiskController
        {
            get { return _hardDiskController; }
        }

        public bool HandlesPort(byte port)
        {
            return _portsHandled[port];
        }

        public bool SupportsAsync
        {
            get { return _z80System.SupportsAsync; }
        }


        //
        // Basic operations
        //

        public virtual void Reset()
        {
            _z80System.Reset();
            _hardDiskController.Reset();

            Trace.Log(LogType.IOState, "{0}: Board reset.", _name);
        }

        public virtual void RunAsync()
        {
            _z80System.RunAsync();
        }

        public virtual void Stop()
        {
            _z80System.Stop();
        }

        /// <summary>
        /// Runs one Z80 instruction, synchronously.
        /// </summary>
        public virtual uint Clock()
        {
            return _z80System.Run();
        }

        public abstract int IORead(byte port);

        public abstract void IOWrite(byte port, int value);

        //
        // Hard disk
        //

        public virtual bool LoadDisk(string mediaPath, int unit = 0)
        {
            // FIXME:
            // new interface should handle multiple units (at least two
            // drives, and later more with SMD?)
            // pathnames should be canonicalized, relative to Disks/ dir?
            // don't make the controller catch errors, do that here
            _hardDiskController.LoadImage(mediaPath /*, unit */);
            return true;
        }

        public virtual bool SaveDisk(int unit = 0)
        {
            // FIXME:
            // the name of the image is in the Config record; use when saving by default
            // will also handle multiple units
            // will handle error checking here and return a nice boolean

            // _hardDiskController.SaveImage(/* unit */);
            return true;    // FIXME ack.
        }

        public virtual bool SaveDisk(string mediaPath, int unit = 0)
        {
            _hardDiskController.SaveImage(mediaPath /*, unit */);
            return true;    // assume it worked, ugh.
        }

        public virtual void UnloadDisk(int unit = 0)
        {
            // FIXME the CDC 976x drives could be unloaded :-)
            // we should also allow the user to add or remove a second
            // Microp or 5.25" drive and actually release the storage
            // (rather than just initialize a blank image).  once we
            // start supporting 140mb Maxtors that's a LOT of wasted memory!
            // _hardDiskController.Unload(unit);
        }

        /// <summary>
        /// Populate the port lookup table.
        /// </summary>
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

        // Describe the specific board
        protected static string _name;
        protected static string _desc;
        protected static ulong _z80CycleTime;
        protected static int _z80RamSize;
        protected static int _z80RamAddr;
        protected static int _z80RomSize;
        protected static int _z80RomAddr;

        // Devices required by all I/O boards
        protected Z80System _z80System;

        // protected IStorageController _hardDisk;
        protected ShugartDiskController _hardDiskController;

        // I/O port map for this board
        private static bool[] _portsHandled = new bool[256];

        // Parent
        private PERQSystem _sys;
    }
}
