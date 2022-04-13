//
// MicropolisDiskController.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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
using PERQemu.Processor;

namespace PERQemu.IO.DiskDevices
{
    /// <summary>
    /// Represents a Micropolis 8" hard drive controller which manages disk
    /// drives in the Disk8Inch class.  This is implemented in the PERQ as an
    /// adapter from the SA4000 interface to the Micropolis 1200-series drives.
    /// May be connected to a CIO (single drive?) or an EIO (1 or 2 drives).
    /// </summary>
    /// <remarks>
    /// This is all hugely speculative and likely to change as I dig into the
    /// microcode and state machine firmware.  Ugh.  For one thing, we'll just
    /// assume that the microcode doesn't try to get fancy and do interleaved
    /// seek operations across both drives, so any kind of unit select signal
    /// remains constant for the duration of the op.  Have got to try to find a
    /// source for the CIO version of the "new" Z80 code, to compare with the
    /// EIO, though even a disassembly of the actual ROM is better than nothing?
    /// </remarks>
    public sealed class MicropolisDiskController : IStorageController
    {
        public MicropolisDiskController(PERQSystem system)
        {
            _system = system;
            _disks = new HardDisk[2];
            _selected = 0;
            _busyEvent = null;
        }

        /// <summary>
        /// Perform a "hardware reset" of the controller and drive.
        /// </summary>
        public void Reset()
        {
            // Reset the attached drive(s)
            if (_disks[0] != null) _disks[0].Reset();
            if (_disks[1] != null) _disks[1].Reset();

            _cylinder = 0;
            _head = 0;
            _sector = 0;
            _seekState = SeekState.WaitForStepSet;

            ClearBusyState();

            // Force a soft reset (calls ResetFlags)
            LoadCommandRegister((int)Command.Reset);

            Log.Debug(Category.HardDisk, "Micropolis controller reset");
        }

        private HardDisk SelectedDisk => _disks[_selected];

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
        /// Attach a drive.  For CIO (PERQ-1) we assume 1 max?  EIO (PERQ-2)
        /// can handle two drives.
        /// </summary>
        /// <remarks>
        /// For now, assume units 1..2 -> _disks[0..1].  Ugh.  Messy.  Just use
        /// a dictionary?
        /// </remarks>
		public void AttachDrive(uint unit, StorageDevice dev)
        {

            if (unit < 1 || (_system.Config.Chassis == ChassisType.PERQ1 && unit > 1) ||
                            (_system.Config.Chassis != ChassisType.PERQ1 && unit > 2))
                throw new InvalidOperationException($"MicropolisController unit {unit} out of range");

            unit--;

            _disks[unit] = dev as HardDisk;

            _disks[unit].SetSeekCompleteCallback(SeekCompletionCallback);

            Log.Debug(Category.HardDisk, "Attached disk '{0}' (unit {1})", _disks[unit].Info.Name, unit);
        }

        /// <summary>
        /// Reads the status register.
        /// </summary>
        /// <remarks>
        /// Reading status DOES NOT clear pending interrupts.  (See behavior in disktest.mic)
        /// </remarks>
        public int ReadStatus()
        {
            Log.Debug(Category.HardDisk, "Read Micropolis status, returned {0:x4}", DiskStatus);
            return DiskStatus;
        }

        /// <summary>
        /// Dispatch register writes.
        /// </summary>
        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                case 0xc1:      // Command register
                    LoadCommandRegister(value);
                    break;

                case 0xc2:      // Head register
                                // Hardware latches 4 bits
                    _head = (byte)(value & 0x0f);
                    SelectedDisk.HeadSelect(_head);

                    Log.Debug(Category.HardDisk, "Micropolis head latch set to {0}", _head);
                    break;

                case 0xc8:  // Micropolis Cylinder/Sector register
                    // Format is different:
                    // Shugart packs it all in one word (sigh)
                    // Micropolis breaks it out into the format shared w/MFM
                    //      head
                    //      cyl msb
                    // Sector # is given in a separate register
                    _sector = (ushort)(value & 0x1f);
                    _head = (byte)((value & 0xe0) >> 5);
                    _cylinder = (ushort)((value & 0xff80) >> 8);

                    Log.Debug(Category.HardDisk, "Micropolis cylinder/head/sector set to {0}/{1}/{2}", _cylinder, _head, _sector);
                    break;

                case 0xc9:  // Micropolis File SN Low Register
                    _serialNumberLow = value & 0xffff;

                    Log.Debug(Category.HardDisk, "Micropolis File Serial # Low set to {0:x4}", _serialNumberLow);
                    break;

                case 0xca:  // Micropolis File SN High register
                    _serialNumberHigh = value & 0xffff;

                    Log.Debug(Category.HardDisk, "Micropolis File Serial # High set to {0:x4}", _serialNumberHigh);
                    break;

                case 0xcb:  // Micropolis Block Number register
                    _blockNumber = (value & 0xffff);

                    Log.Debug(Category.HardDisk, "Micropolis Block # set to {0:x4}", _blockNumber);
                    break;

                case 0xcc:
                    // Sector number
                    break;

                case 0xd0:  // Micropolis Data Buffer Address High register
                    _dataBufferHigh = (~value) & 0xffff;

                    Log.Debug(Category.HardDisk, "Micropolis Data Buffer Address High set to {0:x4}", _dataBufferHigh);
                    break;

                case 0xd1:  // Micropolis Header Address High register
                    _headerAddressHigh = (~value) & 0xffff;

                    Log.Debug(Category.HardDisk, "Micropolis Header Address High set to {0:x4}", _headerAddressHigh);
                    break;

                case 0xd8:  // Micropolis Data Buffer Address Low register
                    _dataBufferLow = (Unfrob(value)) & 0xffff;

                    Log.Debug(Category.HardDisk, "Micropolis Data Buffer Address Low set to {0:x4}", _dataBufferLow);
                    break;

                case 0xd9:  // Micropolis Header Address low register
                    _headerAddressLow = (Unfrob(value)) & 0xffff;

                    Log.Debug(Category.HardDisk, "Micropolis Header Address Low set to {0:x4}", _headerAddressLow);
                    break;

                default:
                    throw new InvalidOperationException($"Bad register {address}");
            }
        }

        /// <summary>
        /// Loads the Micropolis command register.
        /// </summary>
        /// <remarks>
        /// Note:  Most of the info gleaned about the Micropolis controller register
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

            Log.Detail(Category.HardDisk, "Micropolis command data: {0:x4}", data);
            Log.Debug(Category.HardDisk, "Micropolis command is: {0}", command);

            // If the FaultClear bit is set, send that to the drive now
            if (_seekCommand.HasFlag(SeekCommand.FaultClear))
            {
                SelectedDisk.FaultClear();
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

                    Log.Debug(Category.HardDisk, "Micropolis state machine reset");
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
                    Log.Error(Category.HardDisk, "Unhandled Micropolis command {0}", command);
                    break;
            }

            // Handle the seek bits
            ClockSeek();
        }

        public int DiskStatus
        {
            get
            {
                return ((int)_controllerStatus | (SelectedDisk == null ? 0 :
                        (int)((SelectedDisk.Index ? HardStatus.Index : 0) |
                              (SelectedDisk.Track0 ? HardStatus.TrackZero : 0) |
                              (SelectedDisk.Fault ? HardStatus.DriveFault : 0) |
                              (SelectedDisk.SeekComplete ? 0 : HardStatus.SeekComplete) |  // On Cylinder (inverted)
                              (SelectedDisk.Ready ? HardStatus.UnitReady : 0))));
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
                Log.Detail(Category.HardDisk, "Micropolis seek state transition to {0}", _seekState);
            }
            else if (_seekState == SeekState.WaitForStepRelease && !_seekCommand.HasFlag(SeekCommand.Step))
            {
                // High to low
                _seekState = SeekState.WaitForSeekComplete;
                Log.Detail(Category.HardDisk, "Micropolis seek state transition to {0}", _seekState);

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
        /// for the SA4000 drives.  With the "CIO Micropolis" I assume that the
        /// adapter takes advantage of the same strategy.  But when configured
        /// for the EIO more study will be required as to how the hardware does
        /// it.  In either case, the microcode still has to set up the registers
        /// for the DMA addresses, direction, and cyl/head/sec.
        /// </remarks>
        public void DoSingleSeek()
        {
            SelectedDisk.SeekStep(_seekCommand.HasFlag(SeekCommand.Direction) ? 1 : 0);
        }

        /// <summary>
        // TODO: Seek completion for the Micropolis
        /// </summary>
        public void SeekCompletionCallback(ulong skewNsec, object context)
        {
            _seekState = SeekState.WaitForStepSet;
            Log.Detail(Category.HardDisk, "Micropolis seek state transition to {0}", _seekState);

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
#if DEBUG
            if (SelectedDisk.CurCylinder != _cylinder || SelectedDisk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         SelectedDisk.CurCylinder, _cylinder, SelectedDisk.CurHead, _head);
#endif
            // Read the sector from the disk
            Sector sec = SelectedDisk.GetSector(_cylinder, _head, _sector);

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
                      "Micropolis sector read complete from {0}/{1}/{2}, to memory at {3:x6}",
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
            if (SelectedDisk.CurCylinder != _cylinder || SelectedDisk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         SelectedDisk.CurCylinder, _cylinder, SelectedDisk.CurHead, _head);
#endif
            // todo: Should be a DMA op.  See above.
            Sector sec = SelectedDisk.GetSector(_cylinder, _head, _sector);

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
            SelectedDisk.SetSector(sec);

            Log.Debug(Category.HardDisk,
                      "Micropolis sector write complete to {0}/{1}/{2}, from memory at {3:x6}",
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
                                    SelectedDisk.Geometry.SectorSize,
                                    SelectedDisk.Geometry.HeaderSize);

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
            SelectedDisk.SetSector(sec);

            Log.Debug(Category.HardDisk,
                      "Micropolis sector format of {0}/{1}/{2} complete, from memory at {3:x6}",
                      _cylinder, _head, _sector, dataAddr);

            SetBusyState();
        }

        /// <summary>
        /// Low words of Data and Header buffer addresses come in XNOR'd with
        /// 0x3ff for unknown reasons (must be some weird controller hardware
        /// quirk).  To get the real address, we do the XNOR operation again...
        // TODO: is this relevant to the Micropolis?
        /// </summary>
        private int Unfrob(int value)
        {
            return (0x3ff & value) | ((~0x3ff) & (~value));
        }

        /// <summary>
        /// Set the controller Busy status, allow for processing delay, then
        /// raise the HardDisk interrupt.
        // TODO: figure out interrupt requests from the Micropolis adapter, and
        // take the masking bit into consideration
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
        // TODO: update for Micropolis
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
        /// The Micropolis controller on the IOB hardwires unit select 0 (pulled
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
        private HardDisk[] _disks;

        // Controller status (3 bits)
        private Status _controllerStatus;

        // Registers
        private int _selected;
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

/*
struct DiskControl
{
    byte DriveSelect;   // bits 7:6
    byte BA;            // bits 5:4
    byte B;             // bits 3:0
}

struct StateMachineControl
{
    bool CRCNonFatal;   // bit 7 (unimplemented?)
    bool BusEnable;     // bit 6
    bool DBInterrupt;   // bit 5
    bool InterruptOn;   // bit 4
    bool Reset;         // bit 3 (active low)
    byte Function;      // bits 2:0
}

struct StateMachineStatus
{
    DeviceType DiskType;    // bits 10:9
    bool Index;             // bit 8 ("Index/2")?
    bool UnitReady;         // bit 7 (active low)
    bool OnCylinder;        // bit 6 (   "    " )
    bool Fault;             // bit 5 (   "    " )
    bool SeekError;         // bit 4 (   "    " )
    bool SMInterrupt;       // bit 3
    byte Status;            // bits 2:0
}

 
    Notes:

    parsing "disk.quick" for how to handle micropolis drives

    only two regs for writing?  dskctl and smctl
     
     DSKCTL<7>:  SA-4000 - Select Drive #2
                 M1200   - DriveSelect<0>
	 DSKCTL<6>:  SA-4000 - Select Drive #3
                 M1200   - DriveSelect<1>
	 DSKCTL<5>:  SA-4000 - Direction High = In, Low = Out

				 M1200   - BA<0>
	 DSKCTL<4>:  SA-4000 - FaultClear Low-High-Low
				 M1200   - BA<1>
	 DSKCTL<3>:  SA-4000 - Head<3>
				 M1200   - B<3>
	 DSKCTL<2>:  SA-4000 - Head<2>
				 M1200   - B<2>
	 DSKCTL<1>:  SA-4000 - Head<1>
				 M1200   - B<1>
	 DSKCTL<0>:  SA-4000 - Head<0>
				 M1200   - B<0>
				 
     SMCTL<7>:  T2 H - Makes CRC errors non-fatal(proposed).
     SMCTL<6>:  SA-4000 - Step H - Low-High-Low causes a disk step

				M1200   - BusEn H - used to latch data into drive control
						  electronics via DSKCTL<5:0>.
     SMCTL<5>:  T H - Enables "DB" interrupt.
	 SMCTL<4>:  Interrupts On H - Enable all interrupts to PERQ
	 SMCTL<3>:  Reset L - Reset disk controller when Low, must be set High

						  before doing any disk operations.
	 SMCTL<2>:  F2
	 SMCTL<1>:  F1 - See Data Operation Table
	 SMCTL<0>:  F0
	 
     SMSTAT<10>          - DiskType<1>
	 SMSTAT<9>           - DiskType<0>
	 SMSTAT<8>           - Index/2
	 SMSTAT<7>           - Unit Ready L
	 SMSTAT<6>           - On Cylinder L
	 SMSTAT<5>           - Fault L
	 SMSTAT<4>    SA4000 - Track 00 L
				   M1200 - Seek Error L
	 SMSTAT<3>           - State Machine Interrupt H
	 SMSTAT<2>           - Status<2>
	 SMSTAT<1>           - Status<1> See Status<2:0> section
	 SMSTAT<0>           - Status<0>

	SMSTAT<3>, SMSTAT<4>, SMSTAT<6>, and SMSTAT<7> also cause an interrupt
    to PERQ when asserted. Unit Ready, SMSTAT<7> will also cause an interrupt
    if de-asserted.  SMCTL<4> must be High to enable interrupts to PERQ.

    to transfer a byte to the adapter:
        BA<0:1> must both be high (\060)
        low bits of the ctrl byte onto low bits of dskctl
            latched by busen after 2 ucycles
        next, high bits latched
            another 2 ucycles
        then BA<0:1> set to tell the drive what the byte is
            0 is low 8 cyl bits <7:0>
            40 is head<2:0> and high cyl bits <11:10>   (i presume that's bits 9:8)
            20 is a set of function bits:
                <7> preamp gain high    we can ignore
                <6> restore             is this a recalibrate/track 0 op?
                <4> fault clear
                <3> offset minus
                <2> offset plus
                <0> write gate
                bits 1 & 5 are not used

    this is assumed to be the same strategy as used for the mfm disks!?  or not?

    seek and head sel:  both the low cyl and head/hi cyl bytes must be loaded
    (hey! they call out the 1223 explicitly, so the DO use the embedded ctrlr)

    the drive electronics figure out if cyl # changed and initiates a seek if 
    req'd, otherwise just a head sel
        on cyl deasserted until the seek finishes, then reasserted + interrupt;
        if an error occurs, seek error asserted + interrupt

    can do a head select only without loading the low cyl byte

    seek error asserted on timeout (500ms) or illegal track addr; seek err is
    cleared by a restore or reissuing a proper track addr (unless mech failure)

    restore works much like a seek, except it's unrecoverable if it fails

    fault clear is like restore; no effect if the fault can't be cleared; should
    be nearly instantaneous (no op delay)

    offset +- can be used to nudge the heads on retries  after an error (e.g.)
    but i don't think the emulator needs to worry about that :-)

    preamp gain boost may help read recoveries (ignore)

    write gate isn't under program control, but must be set to enable writing on
    the drive; resetting during a write op effects an override (shouldn't happen)

    data xfers:

    smctl<2:0> bits:  see the inscrutable table
        0 = idle            4 = correct     ("SMD only")
        1 = format          5 = bread
        2 = write           6 = fixph       (proposed)
        3 = cwrite          7 = read

    constants are loaded to define the headers; loaded before starting
    (these must be the analogues to all the specific registers in the sa4000
    interface, just not called out separately on the eio?)

    all transfers via dma under control of the disk hardware?  does the network
    have to share/interleave accesses?

    controller compares constants to find sector numbers, etc.  it "knows" about
    the logical header/data split format scheme (PH, LH, DB)

    ha!  the two "disk type" bits can adjust the preamble bits for the different
    supported drive types (shug/microp/mfm)!  sync marks (programmable but MUST
    be constant or the format becomes unreadable) and crc gen/check is handled
    by the disk state machine too

        do we really need to generate/check crc bits?  hmm probably not :-)

    four bytes appended after the sync mark during format: cyl, cylhead, sec,
    and zero.  on reading, cyl, cylhead and sec must match (sector found).  to
    read a specific sector the disk state machine actually has to watch all the
    sectors go by until the correct one is found, and then it reads the other
    parts of the block, otherwise wrong track/error signalled.  fortunately we
    don't have to do that (but should account for rotational latencies :-)

    writing the LH comes from the dma channel -> constant regs:  first six bytes
    are compared to the constants to make sure LH and PH agree, otherwise error
    and intr/abort.  four ops compare the LH/PH: write, read, check, read-check

    no data crc, and only read/write ops on data blocks, as you'd expect

    if any error occurs, smstat contains the code and no other ops performed
    until smctl<2:0> set to idle.

    ops may be chained, i.e., no need to respecify all the constants/registers
    between each one.

    it looks like you can program 0xd0 (const ptr) to set the constant register
    for the next op; subsequent writes to 0xd1 (ramfile) load and increment!

    interrupts can be masked

    where's unit select?

*/
