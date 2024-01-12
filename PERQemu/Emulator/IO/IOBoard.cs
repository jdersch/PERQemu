//
// IOBoard.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;
using PERQemu.Config;
using PERQemu.IO.Z80;
using PERQemu.IO.DiskDevices;
using PERQemu.Processor;

namespace PERQemu.IO
{
    /// <summary>
    /// A superset of the DMA channel descriptions for IOB and EIO.
    /// </summary>
    public enum ChannelName
    {
        Unused = 0,
        uProc = 1,
        HardDisk = 2,
        Network = 3,
        NetXmit = 3,
        ExtA = 4,
        ExtB = 5,
        NetRecv = 6,
        Idle = 7
    }

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

        protected IOBoard(PERQSystem system)
        {
            _sys = system;
            _dmaRegisters = new DMARegisterFile();
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

        public IStorageController DiskController => _hardDiskController;
        public DMARegisterFile DMARegisters => _dmaRegisters;

        public bool HandlesPort(byte port)
        {
            return _portsHandled[port];
        }

        //
        // Basic operations
        //

        public virtual void Reset()
        {
            _dmaRegisters.Clear();

            _z80System.Reset();
            _hardDiskController.Reset();

            Log.Info(Category.IO, "{0} board reset", _name);
        }

        /// <summary>
        /// Runs one Z80 instruction, synchronously.
        /// </summary>
        public virtual void Run()
        {
            _z80System.Run();
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

        /// <summary>
        /// Completely shuts down this instance.
        /// </summary>
        public virtual void Shutdown()
        {
            _z80System.Shutdown();

            _z80System = null;
            _hardDiskController = null;

            for (var p = 1; p < _portsHandled.Length; p++)
                _portsHandled[p] = false;

            Log.Detail(Category.Emulator, "IOBoard shutdown.");
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

            // Allow all of these, although the "CIO"-specific types are kinda redundant
            if (dev.Type == DeviceType.Disk14Inch || dev.Type == DeviceType.DCIOShugart ||
                dev.Type == DeviceType.Disk8Inch || dev.Type == DeviceType.DCIOMicrop ||
                dev.Type == DeviceType.Disk5Inch)
            {
                var hard = new HardDisk(_sys.Scheduler, dev.MediaPath);

                hard.Load();

                _hardDiskController.AttachDrive((uint)dev.Unit, hard);
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
                    Log.Warn(Category.IO, "Port 0x{0:x} already registered!", p);
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

        // Parent
        protected PERQSystem _sys;

        // Devices required by all I/O boards
        protected Z80System _z80System;
        protected IStorageController _hardDiskController;

        // DMA register file
        protected DMARegisterFile _dmaRegisters;

        // I/O port map for this board
        static bool[] _portsHandled = new bool[256];
    }


    public delegate void LoadRegisterDelegate(ChannelName chan, int value);

    public class DMARegisterFile
    {
        public DMARegisterFile()
        {
            _portToChannelAction = new Dictionary<byte, LoadRegisterDelegate>();

            _channels = new DMAChannel[] {
                new DMAChannel(ChannelName.Unused),
                new DMAChannel(ChannelName.uProc),
                new DMAChannel(ChannelName.HardDisk),
                new DMAChannel(ChannelName.NetRecv),
                new DMAChannel(ChannelName.ExtB),
                new DMAChannel(ChannelName.ExtA),
                new DMAChannel(ChannelName.NetXmit),
                new DMAChannel(ChannelName.Idle)
            };
        }

        public void Clear()
        {
            for (var i = 0; i < _channels.Length; i++)
            {
                _channels[i].DataAddr.Value = 0;
                _channels[i].HeaderAddr.Value = 0;
            }
        }

        public void Assign(byte dataHi, byte dataLo, byte hdrHi, byte hdrLo)
        {
            // Four actions for every address pair
            _portToChannelAction.Add(dataHi, LoadDataHigh);
            _portToChannelAction.Add(dataLo, LoadDataLow);
            _portToChannelAction.Add(hdrHi, LoadHeaderHigh);
            _portToChannelAction.Add(hdrLo, LoadHeaderLow);

            Log.Info(Category.DMA, "DMA mapping assigned for ports {0:x}, {1:x}, {2:x}, {3:x}",
                                    dataHi, dataLo, hdrHi, hdrLo);
        }

        public void LoadRegister(ChannelName chan, byte port, int value)
        {
            LoadRegisterDelegate doit;

            if (_portToChannelAction.TryGetValue(port, out doit))
            {
                doit(chan, value);
                return;
            }

            Log.Warn(Category.DMA, "Could not load DMA register 0x{0:2x}, delegate not found!", port);
        }

        public void LoadDataHigh(ChannelName chan, int value)
        {
            _channels[(int)chan].DataAddr.Hi = ~value;
            Log.Debug(Category.DMA, "{0} data buffer addr (high) set to {1:x}", chan, value);
        }

        public void LoadDataLow(ChannelName chan, int value)
        {
            _channels[(int)chan].DataAddr.Lo = (ushort)value;
            Log.Debug(Category.DMA, "{0} data buffer addr (low) set to {1:x4}", chan, value);
        }

        public void LoadHeaderHigh(ChannelName chan, int value)
        {
            _channels[(int)chan].HeaderAddr.Hi = ~value;
            Log.Debug(Category.DMA, "{0} header buffer addr (high) set to {1:x}", chan, value);
        }

        public void LoadHeaderLow(ChannelName chan, int value)
        {
            _channels[(int)chan].HeaderAddr.Lo = (ushort)value;
            Log.Debug(Category.DMA, "{0} header buffer addr (low) set to {1:x4}", chan, value);
        }

        public int GetDataAddress(ChannelName chan)
        {
            return Unfrob(_channels[(int)chan].DataAddr);
        }

        public int GetHeaderAddress(ChannelName chan)
        {
            return Unfrob(_channels[(int)chan].HeaderAddr);
        }

        /// <summary>
        /// Unfrob a DMA address like the hardware do.  This is common to IOB
        /// and CIO, and any OIO device that uses DMA.  Not sure about EIO yet.
        /// </summary>
        /// <remarks>
        ///                                 ! Explained:
        /// tmp := 176000;                  ! Need to compliment upper 6 bits
        ///                                 ! of buffer address. The hardware
        ///                                 ! has an inversion for the upper
        ///                                 ! 10 bits of the 20 bit address.
        /// Buffer xor tmp, IOB(LHeadAdrL); ! Send lower 16 bits of logical
        ///                                 ! header address to channel ctrl.
        /// not 0, IOB(LHeadAdrH);          ! Send higher 4 bits of logical
        ///                                 ! header address to channel ctrl.
        ///                                 ! Remember, these bits are inverted.
        /// </remarks>
        int Unfrob(ExtendedRegister addr)
        {
            // Hi returns the upper 4 or 8 bits shifted; Lo needs to be unfrobbed
            var unfrobbed = addr.Hi | (~(0x3ff ^ addr.Lo) & 0xffff);
            Log.Debug(Category.DMA, "Unfrobbed {0:x} -> {1:x}", addr.Value, unfrobbed);

            return unfrobbed;
        }

        // Debugging
        public void DumpDMARegisters()
        {
            Console.WriteLine("DMA registers:");

            for (var i = ChannelName.uProc; i < ChannelName.Idle; i++)
            {
                Console.WriteLine(_channels[(int)i]);
                Console.WriteLine("Unfrobbed:          {0:x6}        {1:x6}",
                                  GetHeaderAddress(i), GetDataAddress(i));
            }
        }


        internal struct DMAChannel
        {
            public DMAChannel(ChannelName chan)
            {
                Name = chan;

                // Should be 20 bits for IOB/CIO, 20 or 24 bits for EIO!
                HeaderAddr = new ExtendedRegister(CPU.CPUBits - 16, 16);
                DataAddr = new ExtendedRegister(CPU.CPUBits - 16, 16);
            }

            public override string ToString()
            {
                return string.Format("{0}  Header: {1:x6}  Data: {2:x6}",
                                     $"{Name}".PadRight(10), HeaderAddr.Value, DataAddr.Value);
            }

            public ChannelName Name;
            public ExtendedRegister HeaderAddr;
            public ExtendedRegister DataAddr;
        }

        DMAChannel[] _channels;
        Dictionary<byte, LoadRegisterDelegate> _portToChannelAction;
    }
}
