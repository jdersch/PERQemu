//
// IOBoard.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;
using PERQemu.Config;
using PERQemu.IO.Z80;
using PERQemu.IO.DiskDevices;

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

        public Z80System Z80System => _z80System;
        public bool SupportsAsync => _z80System.SupportsAsync;

        // public IStorageController DiskController
        public ShugartDiskController DiskController => _hardDiskController;

        public bool HandlesPort(byte port)
        {
            return _portsHandled[port];
        }

        //
        // Basic operations
        //

        public virtual void Reset()
        {
            _z80System.Reset();
            _hardDiskController.Reset();

            Log.Debug(Category.IO, "{0} board reset", _name);
        }

        /// <summary>
        /// Runs one Z80 instruction, synchronously.
        /// </summary>
        public virtual uint Run()
        {
            return _z80System.Run();
        }

        /// <summary>
        /// Runs the Z80 on its own thread, asynchronously.
        /// </summary>
        public virtual void RunAsync()
        {
            _z80System.RunAsync();
        }

        /// <summary>
        /// Stops the Z80 thread.
        /// </summary>
        public virtual void Stop()
        {
            _z80System.Stop();
        }

        //
        // IO Bus connection
        //

        public abstract int IORead(byte port);

        public abstract void IOWrite(byte port, int value);


        /// <summary>
        /// Creates disk devices appropriate for this board, loads them, and
        /// attaches them to their controllers.  Returns the new StorageDevice.
        /// </summary>
        public virtual StorageDevice LoadDisk(Drive dev)
        {
            if (dev.Type == DeviceType.Floppy)
            {
                var floppy = new FloppyDisk(_z80System.Scheduler, dev.MediaPath);

                if (!string.IsNullOrEmpty(dev.MediaPath))
                {
                    floppy.Load();
                }

                _z80System.FDC.AttachDrive((uint)dev.Unit, floppy);
                return floppy;
            }

            if (dev.Type == DeviceType.Disk14Inch)
            {
                var hard = new HardDisk(_sys.Scheduler, dev.MediaPath);

                hard.Load();

                // Unit # is irrelevant (for now); we only support one drive
                // FIXME when we switch to IStorageController for EIO...
                _hardDiskController.AttachDrive(hard);
                return hard;
            }

            throw new InvalidOperationException("Wrong disk type for this IO Board");
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
                    Log.Warn(Category.IO, "Port {0:x} already registered!", p);
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

        //protected IStorageController _hardDisk;
        protected ShugartDiskController _hardDiskController;

        // I/O port map for this board
        private static bool[] _portsHandled = new bool[256];

        // Parent
        private PERQSystem _sys;
    }
}
