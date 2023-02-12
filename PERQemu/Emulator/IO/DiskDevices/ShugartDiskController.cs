//
// ShugartController.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
    public sealed class ShugartDiskController : IStorageController
    {
        public ShugartDiskController(PERQSystem system)
        {
            _system = system;
            _disk = null;
            _busyEvent = null;

            // These are assembled from separate register writes, so let
            // ExtendedRegisters do the work to combine 'em!
            _dataBuffer = new ExtendedRegister(4, 16);
            _headerAddress = new ExtendedRegister(4, 16);
            _serialNumber = new ExtendedRegister(16, 16);
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

            // Force a soft reset (calls ResetFlags)
            LoadCommandRegister((int)Command.Reset);

            Log.Debug(Category.HardDisk, "Shugart controller reset");
        }

        /// <summary>
        /// Resets the flags ("soft" reset under microcode control).
        /// </summary>
        void ResetFlags()
        {
            _blockNumber = 0;
            _serialNumber.Value = 0;
            _headerAddress.Value = 0;
            _dataBuffer.Value = 0;

            ClearBusyState();
        }

        /// <summary>
        /// Attach the physical drive.  For Shugart, unit # is irrelevant.
        /// </summary>
        public void AttachDrive(uint unit, StorageDevice dev)
        {
            if (_disk != null)
                throw new InvalidOperationException("ShugartController only supports 1 disk");

            _disk = dev as HardDisk;
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
            var stat = DiskStatus;
            Log.Debug(Category.HardDisk, "Shugart status: 0x{0:x4} ({1})", stat, (Status)stat);
            return stat;
        }

        /// <summary>
        /// Dispatch register writes.
        /// </summary>
        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                case 0xc1:    // Command register
                    LoadCommandRegister(value);
                    break;

                case 0xc2:    // Shugart Head register
                    // Hardware latches 4 bits
                    _head = (byte)(value & 0x0f);
                    _disk.HeadSelect(_head);

                    Log.Debug(Category.HardDisk, "Shugart head latch set to {0}", _head);
                    break;

                case 0xc8:    // Shugart Cylinder/Sector register
                    _sector = (ushort)(value & 0x1f);
                    _head = (byte)((value & 0xe0) >> 5);
                    _cylinder = (ushort)((value & 0xff00) >> 8);

                    Log.Debug(Category.HardDisk, "Shugart cyl/head/sector set to {0}/{1}/{2}", _cylinder, _head, _sector);
                    break;

                case 0xc9:    // Shugart File SN Low Register
                    _serialNumber.Lo = (ushort)value;

                    Log.Debug(Category.HardDisk, "Shugart File Serial # Low set to 0x{0:x4}", _serialNumber.Lo);
                    break;

                case 0xca:    // Shugart File SN High register
                    _serialNumber.Hi = value;

                    Log.Debug(Category.HardDisk, "Shugart File Serial # High set to 0x{0:x}", _serialNumber.Hi);
                    break;

                case 0xcb:    // Shugart Block Number register
                    _blockNumber = value & 0xffff;

                    Log.Debug(Category.HardDisk, "Shugart Block # set to 0x{0:x}", _blockNumber);
                    break;

                case 0xcc:    // Micropolis Sector register
                    // Added to the CIO board but ignored by the Shugart controller
                    // May initially log things to see that it's always being set to 0
                    if (value != 0)
                        Log.Warn(Category.HardDisk, "CIOShugart write of 0x{0:x} to MicropSec ignored!", value);
                    break;

                case 0xd0:    // Shugart Data Buffer Address High register (4 bits)
                    _dataBuffer.Hi = ~value;

                    Log.Debug(Category.HardDisk, "Shugart Data Buffer Address High set to 0x{0:x}", _dataBuffer.Hi);
                    break;

                case 0xd1:    // Shugart Header Address High register (4 bits)
                    _headerAddress.Hi = ~value;

                    Log.Debug(Category.HardDisk, "Shugart Header Address High set to 0x{0:x}", _headerAddress.Hi);
                    break;

                case 0xd8:    // Shugart Data Buffer Address Low register (frobbed)
                    _dataBuffer.Lo = (ushort)value;

                    Log.Debug(Category.HardDisk, "Shugart Data Buffer Address Low set to 0x{0:x4}", _dataBuffer.Lo);
                    break;

                case 0xd9:    // Shugart Header Address low register (frobbed)
                    _headerAddress.Lo = (ushort)value;

                    Log.Debug(Category.HardDisk, "Shugart Header Address Low set to 0x{0:x4}", _headerAddress.Lo);
                    break;

                default:
                    throw new InvalidOperationException($"Bad register write 0x{address:x2}");
            }
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
            _command = (Command)(data & 0x07);
            _seekCommand = (SeekCommand)(data & 0x78);

            // WHY, Three Rivers?
            if ((_command == Command.Reset && _seekCommand.HasFlag(SeekCommand.Step)) ||
                (_command == Command.Idle && _seekState == SeekState.WaitForStepRelease))
            {
                // Command code 6 (Seek) is never actually sent to the hardware's
                // disk state machine for some insane reason.  So a Step + Reset
                // gets mapped so that we don't gratuitously reset the controller
                // AND the disk on EVERY BLOODY SEEK.  ARGH.  Similarly, an "idle"
                // command can be the falling edge of a manual step pulse (by the
                // microcode, not the Z80, typically during boot).  
                _command = Command.Seek;
                Log.Debug(Category.HardDisk, "'Hidden' Seek command! Step={0} Dir={1}",
                                             _seekCommand.HasFlag(SeekCommand.Step),
                                             _seekCommand.HasFlag(SeekCommand.Direction));
            }

            Log.Debug(Category.HardDisk, "Shugart command: 0x{0:x4} ({1})", data, _command);

            // If the FaultClear bit is set, send that to the drive now
            if (_seekCommand.HasFlag(SeekCommand.FaultClear))
            {
                _disk.FaultClear();
            }

            // Look at the command bits
            switch (_command)
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

                case Command.Seek:
                    // Handle the seek bits
                    ClockSeek();
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
                    Log.Error(Category.HardDisk, "Unhandled Shugart command {0}", _command);
                    break;
            }
        }

        public int DiskStatus
        {
            get
            {
                return (int)(_disk == null ? Status.Done :
                            ((_controllerBusy ? Status.Busy : 0) |
                             (_disk.Index ? Status.Index : 0) |
                             (_disk.Track0 ? Status.TrackZero : 0) |
                             (_disk.Fault ? Status.DriveFault : 0) |
                             (_disk.SeekComplete ? Status.SeekComplete : 0) |
                             (_disk.Ready ? Status.UnitReady : 0)));
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
        void ClockSeek()
        {
            if (_seekState == SeekState.WaitForStepSet && _seekCommand.HasFlag(SeekCommand.Step))
            {
                // Low to high
                _seekState = SeekState.WaitForStepRelease;
                Log.Detail(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);
            }
            else if (_seekState == SeekState.WaitForStepRelease && !_seekCommand.HasFlag(SeekCommand.Step))
            {
                // High to low
                _seekState = SeekState.WaitForSeekComplete;
                Log.Detail(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);

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
            Log.Detail(Category.HardDisk, "Shugart seek state transition to {0}", _seekState);

            // Clear busy status
            // Technically if Track0 is true we should raise the interrupt
            // but otherwise the "on cylinder" (seek complete) doesn't?  Ugh.
            ClearBusyState();
        }

        /// <summary>
        /// Reads a block from cyl/head/sec into memory at the addresses
        /// specified by the controller registers.
        /// </summary>
        void ReadBlock()
        {
#if DEBUG
            if (_disk.CurCylinder != _cylinder || _disk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Read out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         _disk.CurCylinder, _cylinder, _disk.CurHead, _head);
#endif
            // Todo: This is actually a DMA operation, but that's not
            // implemented yet.  So just do the whole block, lickety split

            // Read the sector from the disk
            var sec = _disk.GetSector(_cylinder, _head, _sector);

            int data = IOBoard.Unfrob(_dataBuffer);
            int header = IOBoard.Unfrob(_headerAddress);

            // Copy the data to the data buffer address
            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = sec.Data[i] | (sec.Data[i + 1] << 8);
                _system.Memory.StoreWord(data + (i >> 1), (ushort)word);
            }

            // And the header to the header address
            for (int i = 0; i < sec.Header.Length; i += 2)
            {
                int word = sec.Header[i] | (sec.Header[i + 1] << 8);
                _system.Memory.StoreWord(header + (i >> 1), (ushort)word);
            }

            Log.Debug(Category.HardDisk,
                      "Shugart sector read from {0}/{1}/{2} to memory 0x{3:x6}",
                      _cylinder, _head, _sector, data);

            SetBusyState();
        }

        /// <summary>
        /// Does a write to the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        void WriteBlock(bool writeHeader)
        {
#if DEBUG
            if (_disk.CurCylinder != _cylinder || _disk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Write out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         _disk.CurCylinder, _cylinder, _disk.CurHead, _head);
#endif
            // Todo: Should be a DMA op.  See above.
            var sec = _disk.GetSector(_cylinder, _head, _sector);

            int data = IOBoard.Unfrob(_dataBuffer);
            int header = IOBoard.Unfrob(_headerAddress);

            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(data + (i >> 1));
                sec.Data[i] = (byte)(word & 0xff);
                sec.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            if (writeHeader)
            {
                for (int i = 0; i < sec.Header.Length; i += 2)
                {
                    int word = _system.Memory.FetchWord(header + (i >> 1));
                    sec.Header[i] = (byte)(word & 0xff);
                    sec.Header[i + 1] = (byte)((word & 0xff00) >> 8);
                }
            }

            // Write the sector to the disk...
            _disk.SetSector(sec);

            Log.Debug(Category.HardDisk,
                      "Shugart sector write to {0}/{1}/{2} from memory 0x{3:x6}",
                      _cylinder, _head, _sector, data);

            SetBusyState();
        }

        /// <summary>
        /// Does a "format" of the cyl/head/sec specified by the controller
        /// registers.  Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        void FormatBlock()
        {
            var sec = new Sector(_cylinder, _head, _sector,
                                 _disk.Geometry.SectorSize,
                                 _disk.Geometry.HeaderSize);

            int dataAddr = IOBoard.Unfrob(_dataBuffer);
            int headerAddr = IOBoard.Unfrob(_headerAddress);

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
                      "Shugart sector format of {0}/{1}/{2} from memory 0x{3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Set the controller Busy status, allow for processing delay, then
        /// raise the HardDisk interrupt.
        /// </summary>
        void SetBusyState()
        {
            // Already busy?  Nothing to do here.
            if (_controllerBusy) return;

            // Set busy flag (code 7), and queue a workitem for resetting it and
            // firing an interrupt.  Time would normally vary based on platter
            // rotation, seek and head settling time, etc.  But we don't really
            // know that in advance, so for seeks don't queue anything; for other
            // commands (reads, writes, etc) just fake up a short/fixed delay.
            _controllerBusy = true;

            // Compute the delay for a block operation...
            var delay = Settings.Performance.HasFlag(RateLimit.DiskSpeed) ?
                                BlockDelayNsec :                // Accurate
                                100 * Conversion.UsecToNsec;    // Fast

            // Idle doesn't set busy to begin with; Reset should be quick (but
            // do a minimal delay so the microcode can see the Busy transition
            // and then reset happen).  Give it 1 usec for now?  PNX 2 seems
            // sensitive to this.
            if (_command == Command.Reset)
            {
                delay = Conversion.UsecToNsec;
            }

            // Schedule the event to clear the busy bit and trigger an interrupt
            // if we're NOT doing a seek -- because reasons okay
            if (_command != Command.Seek)
            {
                _busyEvent = _system.Scheduler.Schedule(delay, (skew, context) =>
                {
                    ClearBusyState(true);
                });
            }
        }

        /// <summary>
        /// Unconditionally clear the Busy state.  Raise or clear the disk
        /// interrupt, depending on the caller's situation.
        /// </summary>
        void ClearBusyState(bool raiseInterrupt = false)
        {
            _controllerBusy = false;

            if (raiseInterrupt)
            {
                _system.CPU.RaiseInterrupt(InterruptSource.HardDisk);
            }
            else
            {
                _system.CPU.ClearInterrupt(InterruptSource.HardDisk);
            }
        }

        enum SeekState
        {
            WaitForStepSet = 0,
            WaitForStepRelease,
            WaitForSeekComplete
        }

        /// <summary>
        /// Disk command.  See diskde.pas (line 917) for POS G or PerqDisk (Accent).
        /// </summary>
        enum Command
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
        enum SeekCommand
        {
            Direction = 0x08,
            Step = 0x10,
            FaultClear = 0x20,
            Unit1 = 0x40
        }

        /// <summary>
        /// Status bits from the drive mapped to the DiskStatus word.
        /// </summary>
        [Flags]
        enum Status
        {
            Done = 0x0,
            Busy = 0x7,
            Index = 0x08,
            TrackZero = 0x10,
            DriveFault = 0x20,
            SeekComplete = 0x40,
            UnitReady = 0x80
        }

        // The physical disk data
        HardDisk _disk;

        // Registers
        ushort _cylinder;
        byte _head;
        ushort _sector;

        int _blockNumber;   // Not really used...

        ExtendedRegister _serialNumber;
        ExtendedRegister _headerAddress;
        ExtendedRegister _dataBuffer;

        // Controller status
        bool _controllerBusy;

        Command _command;
        SeekState _seekState;
        SeekCommand _seekCommand;

        // Work timing for reads/writes.  The mechanical delays are baked into
        // the drive itself (seek, head settling, but not rotational latency);
        // this accounts for the time to DMA the sector header and data (since
        // we don't actually model that, currently).  The absolute best case at
        // 100% utilization is 528 bytes (66 quads) * 680ns or 44.88usec, but
        // the drive's specified 7Mbits/sec (875KB/sec) max transfer rate means
        // at full tilt we could transfer a full block every ~585usec.  We can
        // fiddle with this to make it more realistic.
        readonly ulong BlockDelayNsec = 585 * Conversion.UsecToNsec;

        SchedulerEvent _busyEvent;

        PERQSystem _system;
    }
}
