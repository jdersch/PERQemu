//
// ShugartController.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using PERQemu.Processor;

namespace PERQemu.IO.DiskDevices
{
    /// <summary>
    /// Represents a PERQ 1's Shugart hard drive controller which manages
    /// drives in the Disk14Inch class (SA4000 interface).
    /// </summary>
    public sealed class ShugartDiskController
    {
        public ShugartDiskController(PERQSystem system)
        {
            _system = system;
            _disk = null;
            _busyEvent = null;
        }

        /// <summary>
        /// Perform a "hardware reset" of the controller and drive.
        /// </summary>
        public void Reset()
        {
            if (_disk != null)
            {
                _disk.Reset();
            }

            _cylinder = 0;
            _physCylinder = 0;
            _head = 0;
            _sector = 0;
            _seekState = SeekState.WaitForStepSet;

            if (_busyEvent != null)
            {
                _system.Scheduler.Cancel(_busyEvent);
                _busyEvent = null;
            }

            // Force a soft reset (calls ResetFlags)
            LoadCommandRegister((int)Command.Reset);

            Log.Debug(Category.HardDisk, "Shugart controller reset.");
        }

        /// <summary>
        /// Resets the flags ("soft" reset under microcode control).
        /// </summary>
        private void ResetFlags()
        {
            _controllerStatus = Status.Done;
            _seekComplete = 1;  // ??? check the manual...

            _serialNumberHigh = 0;
            _serialNumberLow = 0;
            _blockNumber = 0;
            _headerAddressLow = 0;
            _headerAddressHigh = 0;
            _dataBufferLow = 0;
            _dataBufferHigh = 0;
        }

        /// <summary>
        /// Attach the physical drive.
        /// </summary>
        public void AttachDrive(HardDisk dev)
        {
            _disk = dev;
            Log.Debug(Category.HardDisk, "Attached disk '{0}'", _disk.Info.Name);
        }

        /// <summary>
        /// Reads the status register.
        /// </summary>
        /// <remarks>
        /// Reading status DOES NOT clear pending interrupts.  (See behavior in disktest.mic)
        /// </remarks>
        public int ReadStatus()
        {
            Log.Debug(Category.HardDisk, "Read Shugart status, returned {0:x4}", DiskStatus);
            return DiskStatus;
        }

        /// <summary>
        /// Loads the Shugart command register.
        /// </summary>
        /// <remarks>
        /// Note:  Most of the info gleaned about the Shugart controller register
        /// behavior is from sysb.micro source.
        ///     Command bits:
        ///       0:2     drive command data      passed to state machine
        ///         3     seek direction flag \
        ///         4     pulses a single seek >  passed through to drive
        ///         5     fault clear         /
        ///       6:7     unit select!?      /    Z80 control bit (conflict!)
        /// </remarks>
        public void LoadCommandRegister(int data)
        {
            Command command = (Command)(data & 0x7);

            Log.Debug(Category.HardDisk, "Shugart command data: {0:x4}", data);
            Log.Debug(Category.HardDisk, "Shugart command is: {0}", command);

            switch (command)
            {
                case Command.Idle:
                    // Clear any running busy event...
                    _system.Scheduler.Cancel(_busyEvent);

                    // Now clear any currently pending interrupts (avoids DDS 163)
                    _system.CPU.ClearInterrupt(InterruptSource.HardDisk);
                    break;

                case Command.Reset:
                    // Reset clears any errors for the drive.
                    // It will interrupt when done.
                    ResetFlags();

                    Log.Debug(Category.HardDisk, "Shugart disk and state machine reset.");
                    SetBusyState();
                    break;

                case Command.ReadChk:
                    ReadBlock();
                    break;

                case Command.ReadDiag:
                    ReadBlock();
                    break;

                case Command.WriteFirst:
                    WriteBlock(true /* writeHeader */);
                    break;

                case Command.WriteChk:
                    WriteBlock(false /* writeHeader */);
                    break;

                case Command.Format:
                    FormatBlock();
                    break;

                default:
                    Log.Error(Category.HardDisk, "Unhandled Shugart command {0}", command);
                    // fixme throw or Log.Debug() instead?  neither should happen when complete...
                    break;
            }

            _seekData = data;
            ClockSeek();
        }

        public void LoadHeadRegister(int value)
        {
            // Hardware latches 4 bits
            _head = (byte)(value & 0x0f);

            Log.Debug(Category.HardDisk, "Shugart head set to {0x2}", _head);
        }

        public void LoadCylSecRegister(int value)
        {
            _sector = (ushort)(value & 0x1f);
            _head = (byte)((value & 0xe0) >> 5);
            _cylinder = (ushort)((value & 0xff80) >> 8);

            Log.Debug(Category.HardDisk, "Shugart cylinder/head/sector set to {0}/{1}/{2}", _cylinder, _head, _sector);
        }

        public void LoadSerialLowRegister(int value)
        {
            _serialNumberLow = value & 0xffff;

            Log.Debug(Category.HardDisk, "Shugart File Serial # Low set to {0:x4}", _serialNumberLow);
        }

        public void LoadSerialHighRegister(int value)
        {
            _serialNumberHigh = value & 0xffff;

            Log.Debug(Category.HardDisk, "Shugart File Serial # High set to {0:x4}", _serialNumberHigh);
        }

        public void LoadBlockRegister(int value)
        {
            _blockNumber = (value & 0xffff);

            Log.Debug(Category.HardDisk, "Shugart Block # set to {0:x4}", _blockNumber);
        }

        public void LoadHeaderAddrLowRegister(int value)
        {
            _headerAddressLow = (Unfrob(value)) & 0xffff;

            Log.Debug(Category.HardDisk, "Shugart Header Address Low set to {0:x4}", _headerAddressLow);
        }

        public void LoadHeaderAddrHighRegister(int value)
        {
            _headerAddressHigh = (~value) & 0xffff;

            Log.Debug(Category.HardDisk, "Shugart Header Address High set to {0:x4}", _headerAddressHigh);
        }

        public void LoadDataBufferAddrLowRegister(int value)
        {
            _dataBufferLow = (Unfrob(value)) & 0xffff;

            Log.Debug(Category.HardDisk, "Shugart Data Buffer Address Low set to {0:x4}", _dataBufferLow);
        }

        public void LoadDataBufferAddrHighRegister(int value)
        {
            _dataBufferHigh = (~value) & 0xffff;

            Log.Debug(Category.HardDisk, "Shugart Data Buffer Address High set to {0:x4}", _dataBufferHigh);
        }

        public int DiskStatus
        {
            get
            {
                return ((int)_controllerStatus |
                        (int)((_disk.Index ? HardStatus.Index : 0) |
                              (_disk.Trk00 ? HardStatus.TrackZero : 0) |
                              (_disk.Fault ? HardStatus.DriveFault : 0) |
                              (_disk.SeekComplete ? HardStatus.SeekComplete : 0) |
                              (_disk.Ready ? HardStatus.UnitReady : 0)));
            }
        }

        private void ClockSeek()
        {
            switch (_seekState)
            {
                case SeekState.WaitForStepSet:
                    if ((_seekData & 0x10) != 0)
                    {
                        _seekState = SeekState.WaitForStepRelease;

                        Log.Debug(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);
                        _seekComplete = 0;
                    }
                    break;

                case SeekState.WaitForStepRelease:
                    if ((_seekData & 0x10) == 0)
                    {
                        _seekState = SeekState.SeekComplete;

                        Log.Debug(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);
                    }
                    break;

                case SeekState.SeekComplete:
                    // Seek in the given direction
                    DoSingleSeek();
                    _seekComplete = 1;
                    _seekState = SeekState.WaitForStepSet;
                    _system.CPU.RaiseInterrupt(InterruptSource.HardDisk);

                    Log.Debug(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);
                    break;
            }
        }

        public void DoSingleSeek()
        {
            if ((_seekData & 0x8) == 0)
            {
                SeekTo((ushort)(_physCylinder - 1));
            }
            else
            {
                SeekTo((ushort)(_physCylinder + 1));
            }

            Log.Debug(Category.HardDisk, "Shugart seek to cylinder {0}", _physCylinder);
        }

        public void DoMultipleSeek(int cylCount)
        {
            if ((_seekData & 0x8) == 0)
            {
                SeekTo((ushort)(_physCylinder - cylCount));
            }
            else
            {
                SeekTo((ushort)(_physCylinder + cylCount));
            }

            Log.Debug(Category.HardDisk, "Shugart seek to cylinder {0}", _physCylinder);
        }

        private void SeekTo(ushort cylinder)
        {
            _physCylinder = cylinder;

            // Clip cylinder into range
            _physCylinder = (ushort)Math.Min(_disk.Geometry.Cylinders - 1, _physCylinder);
            _physCylinder = (ushort)Math.Max((ushort)0, _physCylinder);

        }

        /// <summary>
        /// Does a read from the cyl/head/sec specified by the controller registers.
        /// TODO: This is a DMA operation...
        /// </summary>
        private void ReadBlock()
        {
            // Read the sector from the disk...
            Sector sectorData = _disk.GetSector(_cylinder, _head, _sector);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            // Copy the data to the data buffer address
            // and the header to the header address
            for (int i = 0; i < sectorData.Data.Length; i += 2)
            {
                int word = sectorData.Data[i] | (sectorData.Data[i + 1] << 8);
                _system.Memory.StoreWord(dataAddr + (i >> 1), (ushort)word);
            }

            for (int i = 0; i < sectorData.Header.Length; i += 2)
            {
                int word = sectorData.Header[i] | (sectorData.Header[i + 1] << 8);
                _system.Memory.StoreWord(headerAddr + (i >> 1), (ushort)word);
            }

            Log.Debug(Category.HardDisk,
                      "Shugart sector read complete from {0}/{1}/{2}, wrote to memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Does a write to the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// TODO: This is a DMA operation...
        /// </summary>
        private void WriteBlock(bool writeHeader)
        {
            Sector sectorData = new Sector(_cylinder, _head, _sector,
                                           _disk.Geometry.SectorSize,
                                           _disk.Geometry.HeaderSize);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            for (int i = 0; i < sectorData.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(dataAddr + (i >> 1));
                sectorData.Data[i] = (byte)(word & 0xff);
                sectorData.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            if (writeHeader)
            {
                for (int i = 0; i < sectorData.Header.Length; i += 2)
                {
                    int word = _system.Memory.FetchWord(headerAddr + (i >> 1));
                    sectorData.Header[i] = (byte)(word & 0xff);
                    sectorData.Header[i + 1] = (byte)((word & 0xff00) >> 8);
                }
            }
            else
            {
                // Keep the original header data.
                Sector origSector = _disk.GetSector(_cylinder, _head, _sector);
                origSector.Header.CopyTo(sectorData.Header, 0);
            }

            // Write the sector to the disk...
            _disk.SetSector(sectorData, _cylinder, _head, _sector);

            Log.Debug(Category.HardDisk,
                      "Shugart sector write complete to {0}/{1}/{2}, read from memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Does a "format" of the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        private void FormatBlock()
        {
            Sector sectorData = new Sector(_cylinder, _head, _sector,
                                           _disk.Geometry.SectorSize,
                                           _disk.Geometry.HeaderSize);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            for (int i = 0; i < sectorData.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(dataAddr + (i >> 1));
                sectorData.Data[i] = (byte)(word & 0xff);
                sectorData.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the new header data...
            for (int i = 0; i < sectorData.Header.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(headerAddr + (i >> 1));
                sectorData.Header[i] = (byte)(word & 0xff);
                sectorData.Header[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the sector to the disk...
            _disk.SetSector(sectorData);

            Log.Debug(Category.HardDisk,
                      "Shugart sector format of {0}/{1}/{2} complete, read from memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Low words of Data & Header buffer addresses come in XNOR'd with 0x3ff for
        /// unknown reasons (must be some weird quirk with the controller hardware).
        ///
        /// To get the real address, we do the XNOR operation again...
        /// </summary>
        private int Unfrob(int value)
        {
            return (0x3ff & value) | ((~0x3ff) & (~value));
        }

        private void SetBusyState()
        {
            // Already busy?  Nothing to do here.
            if (_controllerStatus == Status.Busy)
            {
                return;
            }

            // Set busy flag (code 7), and queue a workitem for resetting it and
            // firing an interrupt.  Time would normally vary based on platter
            // rotation, seek and head settling time, etc.  5 is fine for now.
            _controllerStatus = Status.Busy;

            _busyEvent = _system.Scheduler.Schedule(_busyDurationNsec, (skew, context) =>
            {
                _controllerStatus = Status.Done;
                _system.CPU.RaiseInterrupt(InterruptSource.HardDisk);
            });
        }

        private enum SeekState
        {
            WaitForStepSet = 0,
            WaitForStepRelease,
            SeekComplete
        }

        /// <summary>
        /// Disk command.  See diskde.pas (line 917) for POS G or PerqDisk (Accent).
        /// </summary>
        private enum Command
        {
            Idle = 0x0,
            ReadChk = 0x1,
            ReadDiag = 0x2,
            WriteChk = 0x3,
            WriteFirst = 0x4,
            Format = 0x5,
            Seek = 0x6,
            Reset = 0x7
        }

        /// <summary>
        /// Controller status.  Not terribly detailed.
        /// </summary>
        private enum Status
        {
            Done = 0x0,
            Busy = 0x7
        }

        /// <summary>
        /// Status bits from the drive mapped to the DiskStatus word.
        /// </summary>
        [Flags]
        private enum HardStatus
        {
            Index = 0x08,
            TrackZero = 0x10,
            DriveFault = 0x20,
            SeekComplete = 0x40,
            UnitReady = 0x80
        }

        // The physical disk data
        private HardDisk _disk;

        // Controller status (3 bits)
        private Status _controllerStatus;

        // Head position information
        private ushort _cylinder;
        private ushort _physCylinder;
        private byte _head;
        private ushort _sector;

        private int _serialNumberLow;
        private int _serialNumberHigh;
        private int _blockNumber;
        private int _headerAddressLow;
        private int _headerAddressHigh;
        private int _dataBufferLow;
        private int _dataBufferHigh;

        private SeekState _seekState;
        private int _seekData;
        private int _seekComplete;

        // Work timing for reads/writes.  Assume 1ms for now.
        private ulong _busyDurationNsec = 1 * Conversion.MsecToNsec;
        private Event _busyEvent;

        private PERQSystem _system;
    }
}
