// shugartcontroller.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.CPU;
using PERQemu.PhysicalDisk;
using PERQemu.IO.Z80.IOB;
using PERQemu.Memory;

using System;
using System.IO;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace PERQemu.IO.HardDisk
{
    /// <summary>
    /// Represents a PERQ 1's Shugart hard drive controller
    /// </summary>
    public sealed class ShugartDiskController
    {
        private ShugartDiskController()
        {
            Reset();
            LoadImage(null);
        }

        public static ShugartDiskController Instance
        {
            get { return _instance; }
        }

        public void Reset()
        {
            ResetFlags();

            _cylinder = 0;
            _physCylinder = 0;
            _head = 0;
            _sector = 0;
            _busyTime = 0;
            _seekState = SeekState.WaitForStepSet;
        }

        public void Clock()
        {
            ClockSeek();

            _trackZero = (_physCylinder == 0 ? 1 : 0);

            // This is a hack to fudge up the index pulse for the drive.
            // (Makes diagnostics happy but isn't normally used for much.)
            // TODO: actually, for the CIO board this needs to be accurate;
            // it's how the microcode determines if it's talking to a Shugart
            // or a Micropolis drive at boot time.
            _clocks++;
            if (_clocks > 4)
            {
                _index = (_index == 0 ? 1 : 0);
                _clocks = 0;
            }

            if (_controllerStatus == Status.Busy)
            {
                // and now for our controller status countdown!
                _busyTime--;

                // if the operation is now done, we should interrupt to let the PERQ know...
                if (_busyTime == 0)
                {
                    _controllerStatus = Status.Done;
                    PERQCpu.Instance.RaiseInterrupt(InterruptType.HardDisk);
                }
            }
        }

        /// <summary>
        /// This is the Z80 portion of the hard disk controller, which seems out of place here.
        /// The Z80 can take control of stepping the heads in whatever direction the drive is currently
        /// set up to move in.
        /// </summary>
        /// <param name="message"></param>
        /// <param name="data"></param>
        /// <returns></returns>
        public bool RunStateMachine(Z80.IOB.PERQtoZ80Message message, byte data)
        {
            // One byte for Seek:
            //  byte 0 = seek count
            DoMultipleSeek(data);

            // Send completion message:
            //  SOM
            //  0xa (seek done)
            Z80System.Instance.FIFO.Enqueue(Z80.IOB.Z80System.SOM);
            Z80System.Instance.FIFO.Enqueue(0xa);

            return true;
        }

        public int ReadStatus()
        {
            // Reading status DOES NOT clear pending interrupts.
            // (See behavior in disktest.mic)
            // PERQCpu.Instance.ClearInterrupt(InterruptType.HardDisk);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Read Shugart status register, returned {0:x4}", DiskStatus);
#endif
            return DiskStatus;
        }


        public void LoadCommandRegister(int data)
        {

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.HardDisk, "Shugart command data: {0:x4}", data);
#endif
            // Note:  Most of the info gleaned about the Shugart controller register behavior is from
            // sysb.micro source.
            // Command bits:
            //  0:3     drive command data
            //    4     seek direction flag
            //    5     pulses a single seek
            Command command = (Command)(data & 0x7);

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.HardDisk, "Shugart command is {0}", command);
#endif

            switch (command)
            {
                case Command.Idle:
                    // Clear any pending interrupts.
                    PERQCpu.Instance.ClearInterrupt(InterruptType.HardDisk);
                    break;

                case Command.Reset:
                    // Reset clears any errors for the drive.
                    // It will interrupt when done.
                    ResetFlags();

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.HardDisk, "HardDisk: Shugart disk and state machine reset.");
#endif
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
                    Console.WriteLine("Unhandled Shugart command {0}", command);
                    break;
            }

            _seekData = data;
        }

        public void LoadHeadRegister(int value)
        {
            _head = value & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.HardDisk, "Shugart head set to {0:x4}", _head);
#endif
        }

        public void LoadCylSecRegister(int value)
        {
            _sector = (value & 0x1f);
            _head = (value & 0xe0) >> 5;
            _cylinder = (value & 0xff80) >> 8;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart cylinder/head/sector set to {0}/{1}/{2}", _cylinder, _head, _sector);
#endif
        }

        public void LoadSerialLowRegister(int value)
        {
            _serialNumberLow = value & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart File Serial # Low set to {0:x4}", _serialNumberLow);
#endif
        }

        public void LoadSerialHighRegister(int value)
        {
            _serialNumberHigh = value & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart File Serial # High set to {0:x4}", _serialNumberHigh);
#endif
        }

        public void LoadBlockRegister(int value)
        {
            _blockNumber = (value & 0xffff);

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.HardDisk, "Shugart Block # set to {0:x4}", _blockNumber);
#endif
        }

        public void LoadHeaderAddrLowRegister(int value)
        {
            _headerAddressLow = (Unfrob(value)) & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart Header Address Low set to {0:x4}", _headerAddressLow);
#endif
        }

        public void LoadHeaderAddrHighRegister(int value)
        {
            _headerAddressHigh = (~value) & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart Header Address High set to {0:x4}", _headerAddressHigh);
#endif
        }

        public void LoadDataBufferAddrLowRegister(int value)
        {
            _dataBufferLow = (Unfrob(value)) & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart Data Buffer Address Low set to {0:x4}", _dataBufferLow);
#endif
        }

        public void LoadDataBufferAddrHighRegister(int value)
        {
            _dataBufferHigh = (~value) & 0xffff;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk, "Shugart Data Buffer Address High set to {0:x4}", _dataBufferHigh);
#endif
        }

        public int DiskStatus
        {
            get
            {
                return (
                    (int)_controllerStatus |
                    (_index << 3) |
                    (_trackZero << 4) |
                    (_driveFault << 5) |
                    (_seekComplete << 6) |
                    (_unitReady << 7));
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

#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.HardDisk, "Shugart seek state transition to {0}", _seekState);
#endif
                        _seekComplete = 0;
                    }
                    break;

                case SeekState.WaitForStepRelease:
                    if ((_seekData & 0x10) == 0)
                    {
                        _seekState = SeekState.SeekComplete;

#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.HardDisk, "Shugart seek state transition to {0}", _seekState);
#endif
                    }
                    break;

                case SeekState.SeekComplete:
                    // Seek in the given direction
                    DoSingleSeek();
                    _seekComplete = 1;
                    _seekState = SeekState.WaitForStepSet;
                    PERQCpu.Instance.RaiseInterrupt(InterruptType.HardDisk);

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.HardDisk, "Shugart seek state transition to {0}", _seekState);
#endif
                    break;
            }
        }

        private void DoSingleSeek()
        {
            if ((_seekData & 0x8) == 0)
            {
                SeekTo(_physCylinder - 1);
            }
            else
            {
                SeekTo(_physCylinder + 1);
            }

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.HardDisk, "Shugart seek to cylinder {0}", _physCylinder);
#endif
        }

        private void DoMultipleSeek(int cylCount)
        {
            if ((_seekData & 0x8) == 0)
            {
                SeekTo(_physCylinder - cylCount);
            }
            else
            {
                SeekTo(_physCylinder + cylCount);
            }

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.HardDisk, "Shugart seek to cylinder {0}", _physCylinder);
#endif
        }

        private void SeekTo(int cylinder)
        {
            _physCylinder = cylinder;

            // Clip cylinder into range
            _physCylinder = Math.Min((int)_disk.Cylinders - 1, _physCylinder);
            _physCylinder = Math.Max(0, _physCylinder);
        }

        /// <summary>
        /// Does a read from the cyl/head/sec specified by the controller registers.
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
                int word = sectorData.Data[i] | (sectorData.Data[i+1] << 8);
                MemoryBoard.Instance.StoreWord(dataAddr + (i >> 1), (ushort)word);
            }

            for (int i = 0; i < sectorData.Header.Length; i += 2)
            {
                int word = sectorData.Header[i] | (sectorData.Header[i+1] << 8);
                MemoryBoard.Instance.StoreWord(headerAddr + (i >> 1), (ushort)word);
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk,
                          "Shugart sector read complete from {0}/{1}/{2}, wrote to memory at {3:x5}",
                          _cylinder, _head, _sector, dataAddr);
#endif
            SetBusyState();
        }

        /// <summary>
        /// Does a write to the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        private void WriteBlock(bool writeHeader)
        {
            Sector sectorData = new Sector(_cylinder, _head, _sector, _disk.DiskGeometry);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            for (int i = 0; i < sectorData.Data.Length; i += 2)
            {
                int word = Memory.MemoryBoard.Instance.FetchWord(dataAddr + (i >> 1));
                sectorData.Data[i] = (byte)(word & 0xff);
                sectorData.Data[i+1] = (byte)((word & 0xff00) >> 8);
            }

            if (writeHeader)
            {
                for (int i = 0; i < sectorData.Header.Length; i += 2)
                {
                    int word = Memory.MemoryBoard.Instance.FetchWord(headerAddr + (i >> 1));
                    sectorData.Header[i] = (byte)(word & 0xff);
                    sectorData.Header[i+1] = (byte)((word & 0xff00) >> 8);
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

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk,
                          "Shugart sector write complete to {0}/{1}/{2}, read from memory at {3:x5}",
                          _cylinder, _head, _sector, dataAddr);
#endif
            SetBusyState();
        }

        /// <summary>
        /// Does a "format" of the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        private void FormatBlock()
        {
            Sector sectorData = new Sector(_cylinder, _head, _sector, _disk.DiskGeometry);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            for (int i = 0; i < sectorData.Data.Length; i += 2)
            {
                int word = Memory.MemoryBoard.Instance.FetchWord(dataAddr + (i >> 1));
                sectorData.Data[i] = (byte)(word & 0xff);
                sectorData.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the new header data...
            for (int i = 0; i < sectorData.Header.Length; i += 2)
            {
                int word = Memory.MemoryBoard.Instance.FetchWord(headerAddr + (i >> 1));
                sectorData.Header[i] = (byte)(word & 0xff);
                sectorData.Header[i+1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the sector to the disk...
            _disk.SetSector(sectorData, _cylinder, _head, _sector);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.HardDisk,
                          "Shugart sector format of {0}/{1}/{2} complete, read from memory at {3:x5}",
                          _cylinder, _head, _sector, dataAddr);
#endif
            SetBusyState();
        }

        public void LoadImage(string path)
        {
            // Create a new PhysicalDisk for a 24mb Shugart drive...
            // TODO: this should account for the 4104, and also write
            // a type code byte into the header to make auto-discovery
            // of the disk image type more accurate...
            _disk = new ShugartDisk(true /* 24mb */);

            if (path != null)
            {
                // Load the disk image into it...
                FileStream fs = new FileStream(path, FileMode.Open);
                _disk.Load(fs);
                fs.Close();
            }
        }

        public void SaveImage(string path)
        {
            // Load the disk image into it...
            FileStream fs = new FileStream(path, FileMode.OpenOrCreate);
            _disk.Save(fs);
            fs.Close();
        }

        /// <summary>
        /// Low words of Data & Header buffer addresses come in XNOR'd with 0x3ff for unknown reasons
        /// (must be some weird quirk with the controller hardware).
        ///
        /// To get the real address, we do the XNOR operation again...
        /// </summary>
        /// <param name="value"></param>
        /// <returns></returns>
        private int Unfrob(int value)
        {
            return (0x3ff & value) | ((~0x3ff) & (~value));
        }

        private void SetBusyState()
        {
            // Set busy flag (code 7), and set the timer for resetting it.
            // time would normally vary based on platter rotation, etc.  5 is fine for now.
            _controllerStatus = Status.Busy;
            _busyTime = 5;
        }

        private void ResetFlags()
        {
            _controllerStatus = Status.Done;
            _trackZero = 0;
            _driveFault = 0;
            _seekComplete = 0;
            _unitReady = 1;
            _index = 1;

            _serialNumberHigh = 0;
            _serialNumberLow = 0;
            _blockNumber = 0;
            _headerAddressLow = 0;
            _headerAddressHigh = 0;
            _dataBufferLow = 0;
            _dataBufferHigh = 0;
        }

        // The physical disk data
        private ShugartDisk _disk;

        /// <summary>
        /// Status bits.  It is assumed (in the DiskStatus property)
        /// that these contain no more than their designated bits.
        /// Failure to ensure this will result in weirdness.
        /// This is hacky.
        /// </summary>
        private Status _controllerStatus;   // 3 bits
        private int _trackZero;             // 1 bit
        private int _driveFault;            // 1 bit
        private int _seekComplete;          // 1 bit
        private int _unitReady;             // 1 bit
        private int _index;                 // 1 bit

        // Head position information
        private int _cylinder;
        private int _physCylinder;
        private int _head;
        private int _sector;

        private int _serialNumberLow;
        private int _serialNumberHigh;
        private int _blockNumber;
        private int _headerAddressLow;
        private int _headerAddressHigh;
        private int _dataBufferLow;
        private int _dataBufferHigh;

        private SeekState _seekState;
        private int _seekData;

        private int _busyTime;
        private int _clocks;

        private static ShugartDiskController _instance = new ShugartDiskController();

        private enum SeekState
        {
            WaitForStepSet = 0,
            WaitForStepRelease,
            SeekComplete
        }

        /// <summary>
        /// Disk command.  See diskde.pas (line 917).
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

        private enum Status
        {
            Done = 0x0,
            Busy = 0x7
        }
    }
}
