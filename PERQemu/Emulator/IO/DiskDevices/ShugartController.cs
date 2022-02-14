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
            _head = 0;
            _sector = 0;
            _seekState = SeekState.WaitForStepSet;

            ClearBusyState();

            // Force a soft reset (calls ResetFlags)
            LoadCommandRegister((int)Command.Reset);

            Log.Debug(Category.HardDisk, "Shugart controller reset");
        }

        /// <summary>
        /// Resets the flags ("soft" reset under microcode control).
        /// </summary>
        private void ResetFlags()
        {
            _controllerStatus = Status.Done;

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

            // let's see if this heps
            _disk.SetSeekCompleteCallback(SeekCompletionCallback);

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
        /// behavior is from microcode (Boot, SysB, IO) sources.
        ///     Command bits:
        ///       0:2     drive command data      passed to state machine
        ///         3     seek direction flag \
        ///         4     pulses a single seek >  passed through to drive
        ///         5     fault clear         /
        ///       6:7     unit select!?      /    Z80 control bit (conflict!)
        /// </remarks>
        public void LoadCommandRegister(int data)
        {
            var command = (Command)(data & 0x07);
            _seekCommand = (SeekCommand)(data & 0x78);

            Log.Debug(Category.HardDisk, "Shugart command data: {0:x4}", data);
            Log.Debug(Category.HardDisk, "Shugart command is: {0}", command);

            // If the FaultClear bit is set, send that to the drive now
            if (_seekCommand.HasFlag(SeekCommand.FaultClear))
            {
                _disk.FaultClear();
            }

            // Look at the command bits
            switch (command)
            {
                case Command.Idle:
                    // Clear the busy status and the interrupt
                    ClearBusyState();
                    break;

                case Command.Reset:
                    // Reset clears the state machine, interrupts when done
                    ResetFlags();

                    Log.Debug(Category.HardDisk, "Shugart state machine reset");
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
                    break;
            }

            // Handle the seek bits
            ClockSeek();
        }

        public void LoadHeadRegister(int value)
        {
            // Hardware latches 4 bits
            _head = (byte)(value & 0x0f);
            _disk.HeadSelect(_head);

            Log.Debug(Category.HardDisk, "Shugart head latch set to {0}", _head);
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
                return ((int)_controllerStatus | (_disk == null ? 0 :
                        (int)((_disk.Index ? HardStatus.Index : 0) |
                              (_disk.Track0 ? HardStatus.TrackZero : 0) |
                              (_disk.Fault ? HardStatus.DriveFault : 0) |
                              (_disk.SeekComplete ? 0 : HardStatus.SeekComplete) |  // On Cylinder (inverted)
                              (_disk.Ready ? HardStatus.UnitReady : 0))));
            }
        }

        /// <summary>
        /// Track the transitions of the Seek command bit to initiate disk head
        /// movement.
        /// </summary>
        /// <remarks>
        /// The falling edge of a low-high-low transition transmits a step
        /// command to the drive.  It is presumed that the microcode determines
        /// the correct pulse duration and interval to accomplish buffered seeks.
        /// </remarks>
        private void ClockSeek()
        {
            if (_seekState == SeekState.WaitForStepSet && _seekCommand.HasFlag(SeekCommand.Step))
            {
                // Low to high
                _seekState = SeekState.WaitForStepRelease;
                Log.Debug(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);
            }
            else if (_seekState == SeekState.WaitForStepRelease && !_seekCommand.HasFlag(SeekCommand.Step))
            {
                // High to low
                _seekState = SeekState.WaitForSeekComplete;
                Log.Debug(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);

                // Don't queue a standard busy delay?  Wait for "on cylinder"
                // (i.e., SeekComplete) and then fire an interrupt.  But if the
                // Z80 is stepping the heads... hmm.
                SetBusyState();

                // Send it
                DoSingleSeek();
            }
        }

        /// <summary>
        /// Tell the drive to initiate (or continue) a seek one step in the
        /// desired direction, and update our cylinder counter.  We pass it
        /// the callback of the completion routine to be informed when the
        /// drive's heads reach the desired track.  Seeks performed by the
        /// microcode are typically unbuffered (one track at a time).
        /// </summary>
        /// <remarks>
        /// On the PERQ-1 IOB, the Z80 CTC is programmed to issue seek pulses
        /// to take advantage of buffering in the SA4000 drives.  The microcode
        /// still has to set up the registers for the DMA addresses, direction,
        /// and cyl/head/sec.  HardDiskSeekControl calls this routine to issue
        /// the command to the disk (with the proper Direction set).
        /// </remarks>
        public void DoSingleSeek()
        {
            _disk.SeekStep(_seekCommand.HasFlag(SeekCommand.Direction) ? 1 : 0);
        }

        /// <summary>
        /// Seek completion just resets the state machine.  Apparently the
        /// Shugart controller doesn't actually interrupt in this case, and
        /// for seeks it doesn't even set the "busy" status bits!?
        /// </summary>
        /// <remarks>
        /// The SA4000 doesn't really provide "seek complete" -- just "on
        /// cylinder" -- with the requirement that the driver allows ~20ms
        /// or two index pulses for "head settling" before any read/write
        /// commands can be issued.  When the Z80 performs seeks, it computes
        /// the delay based on the seek distance and a table in the code,
        /// waits that long, then interrupts the PERQ with a reply message to
        /// signal Seek Complete.  Yikes.
        /// </remarks>
        public void SeekCompletionCallback(ulong skewNsec, object context)
        {
            _seekState = SeekState.WaitForStepSet;
            Log.Debug(Category.HardDisk, "Shugart seek state transition to {0}", _seekState); 

            // Clear busy status
            // Technically if Track0 is true we should raise the interrupt
            // but otherwise the "on cylinder" (seek complete) doesn't?  Ugh.
            ClearBusyState();
        }

        /// <summary>
        /// Reads a block from cyl/head/sec into memory at the addresses
        /// specified by the controller registers.
        /// </summary>
        private void ReadBlock()
        {
            // todo: This is actually a DMA operation, but that's not
            // implemented yet.  So just do the whole block, lickety split

            if (_disk.CurCylinder != _cylinder || _disk.CurHead != _head)
                Log.Warn(Category.HardDisk, "Out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         _disk.CurCylinder, _cylinder, _disk.CurHead, _head);

            // Read the sector from the disk
            Sector sec = _disk.GetSector(_cylinder, _head, _sector);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            // Copy the data to the data buffer address
            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = sec.Data[i] | (sec.Data[i + 1] << 8);
                _system.Memory.StoreWord(dataAddr + (i >> 1), (ushort)word);
            }

            // And the header to the header address
            for (int i = 0; i < sec.Header.Length; i += 2)
            {
                int word = sec.Header[i] | (sec.Header[i + 1] << 8);
                _system.Memory.StoreWord(headerAddr + (i >> 1), (ushort)word);
            }

            Log.Debug(Category.HardDisk,
                      "Shugart sector read complete from {0}/{1}/{2}, to memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Does a write to the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        private void WriteBlock(bool writeHeader)
        {
#if DEBUG
            if (_disk.CurCylinder != _cylinder || _disk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         _disk.CurCylinder, _cylinder, _disk.CurHead, _head);
#endif
            // todo: Should be a DMA op.  See above.
            Sector sec = _disk.GetSector(_cylinder, _head, _sector);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(dataAddr + (i >> 1));
                sec.Data[i] = (byte)(word & 0xff);
                sec.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            if (writeHeader)
            {
                for (int i = 0; i < sec.Header.Length; i += 2)
                {
                    int word = _system.Memory.FetchWord(headerAddr + (i >> 1));
                    sec.Header[i] = (byte)(word & 0xff);
                    sec.Header[i + 1] = (byte)((word & 0xff00) >> 8);
                }
            }

            // Write the sector to the disk...
            _disk.SetSector(sec);

            Log.Debug(Category.HardDisk,
                      "Shugart sector write complete to {0}/{1}/{2}, from memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Does a "format" of the cyl/head/sec specified by the controller
        /// registers.  Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        private void FormatBlock()
        {
            Sector sec = new Sector(_cylinder, _head, _sector,
                                    _disk.Geometry.SectorSize,
                                    _disk.Geometry.HeaderSize);

            int dataAddr = _dataBufferLow | (_dataBufferHigh << 16);
            int headerAddr = _headerAddressLow | (_headerAddressHigh << 16);

            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(dataAddr + (i >> 1));
                sec.Data[i] = (byte)(word & 0xff);
                sec.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the new header data...
            for (int i = 0; i < sec.Header.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(headerAddr + (i >> 1));
                sec.Header[i] = (byte)(word & 0xff);
                sec.Header[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the sector to the disk...
            _disk.SetSector(sec);

            Log.Debug(Category.HardDisk,
                      "Shugart sector format of {0}/{1}/{2} complete, from memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Low words of Data and Header buffer addresses come in XNOR'd with
        /// 0x3ff for unknown reasons (must be some weird controller hardware
        /// quirk).  To get the real address, we do the XNOR operation again...
        /// </summary>
        private int Unfrob(int value)
        {
            return (0x3ff & value) | ((~0x3ff) & (~value));
        }

        /// <summary>
        /// Set the controller Busy status, allow for processing delay, then
        /// raise the HardDisk interrupt.
        /// </summary>
        private void SetBusyState()
        {
            // Already busy?  Nothing to do here.
            if (_controllerStatus == Status.Busy) return;

            // Set busy flag (code 7), and queue a workitem for resetting it and
            // firing an interrupt.  Time would normally vary based on platter
            // rotation, seek and head settling time, etc.  But we don't really
            // know that in advance, so for seeks don't queue anything; for other
            // commands (reads, writes, etc) just fake up a short/fixed delay.
            _controllerStatus = Status.Busy;

            if (_seekState != SeekState.WaitForSeekComplete)
            {
                _busyEvent = _system.Scheduler.Schedule(_busyDurationNsec, (skew, context) =>
                {
                    ClearBusyState(true);
                });
            }
        }

        /// <summary>
        /// Unconditionally clear the Busy state.  Raise or clear the disk
        /// interrupt, depending on the caller's situation.
        /// </summary>
        private void ClearBusyState(bool raiseInterrupt = false)
        {
            _controllerStatus = Status.Done;

            if (raiseInterrupt)
            {
                _system.CPU.RaiseInterrupt(InterruptSource.HardDisk);
            }
            else
            {
                _system.CPU.ClearInterrupt(InterruptSource.HardDisk);
            }
        }

        private enum SeekState
        {
            WaitForStepSet = 0,
            WaitForStepRelease,
            WaitForSeekComplete
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
        /// Extra command bits.  These directly drive pins on the SA4000
        /// interface cable!
        /// </summary>
        /// <remarks>
        /// The Shugart controller on the IOB hardwires unit select 0 (pulled
        /// to GND) and leaves unit select 1 and 2 unconnected. (The pin used
        /// for unit 3 is jumpered to provide Seek Complete.)  The register
        /// that latches the command bits could easily provide unit select 1
        /// so we define that here (for future fun), but bit 7 of the IOD is
        /// co-opted as the "Z80 enable" bit (so, no unit 2 select possible).
        /// It grieves me that they didn't provide for a second (external)
        /// drive or write the software to allow for it until POS G, when it
        /// was all re-written for the EIO board.  Sigh.
        /// </remarks>
        [Flags]
        private enum SeekCommand
        {
            Direction = 0x08,
            Step = 0x10,
            FaultClear = 0x20,
            Unit1 = 0x40
        }

        /// <summary>
        /// Controller status.  Super detailed.
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

        // Registers
        private ushort _cylinder;
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
        private SeekCommand _seekCommand;

        // Work timing for reads/writes.  Assume .1ms for now?  (The
        // actual mechanical delays are baked into the drive itself now)
        private ulong _busyDurationNsec = 100 * Conversion.UsecToNsec;
        private SchedulerEvent _busyEvent;

        private PERQSystem _system;
    }
}
