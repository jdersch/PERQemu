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
            _drives = new StorageDevice[_sys.Config.Drives.Length];
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

            Log.Debug(Category.IO, "{0} board reset.", _name);
        }

        /// <summary>
        /// Runs one Z80 instruction, synchronously.
        /// </summary>
        public virtual uint Clock()
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

        //
        // Disk loading and unloading
        //

        public virtual void LoadDisk(DeviceType dev, string mediaPath, int unit = 0)
        {
            if (dev == DeviceType.Floppy)
            {
                _drives[unit] = new FloppyDisk(_z80System.Scheduler, mediaPath);
                _drives[unit].Load();
                _z80System.FDC.AttachDrive((uint)unit, (FloppyDisk)_drives[unit]);
            }
            else if (dev == DeviceType.Disk14Inch)
            {
                _drives[unit] = new HardDisk(_sys.Scheduler, mediaPath);
                _drives[unit].Load();
                _hardDiskController.AttachDrive((HardDisk)_drives[unit]);
            }
            else
            {
                throw new InvalidOperationException("Wrong disk type for this IO Board");
            }
        }

        public virtual void SaveDisk(int unit = 0)
        {
            if (_drives[unit] != null && _drives[unit].IsLoaded && _drives[unit].IsModified)
            {
                if (Settings.SaveDiskOnShutdown == Ask.Yes)
                {
                    Console.WriteLine("Saving unit " + unit);   // fixme
                    _drives[unit].Save();
                }
                else
                {
                    Console.WriteLine("Wanna save unit " + unit + "?  ya caint yet");
                }
            }
        }

        //public virtual void SaveDisk(string mediaPath, int unit = 0)
        //{
        //    if (_drives[unit].IsLoaded)
        //    {
        //        Console.WriteLine("Doing a SaveAs for unit " + unit);
        //        _drives[unit].SaveAs(mediaPath);
        //    }
        //}

        public virtual void UnloadDisk(int unit = 0)
        {
            _drives[unit]?.Unload();
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

        // Drives attached to this board
        protected StorageDevice[] _drives;

        // I/O port map for this board
        private static bool[] _portsHandled = new bool[256];

        // Parent
        private PERQSystem _sys;
    }
}
