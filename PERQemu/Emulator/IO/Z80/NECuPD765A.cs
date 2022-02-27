//
// NECuPD765A.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using PERQemu.IO.DiskDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Implements the 765A FDC, or at least as much of it as the PERQ IOB uses.
    /// This means that only the Read, Write, Format, and Seek commands are
    /// functional at this time.  Not all interrupt modes are implemented, in
    /// particular transfers are DMA-only.
    /// </summary>
    /// <remarks>
    /// There are only two addressible registers; base address is the CSR:
    /// 
    ///     From v87.z80:           From param.eio:
    ///     FLPSTA  EQU  250Q       IOAFSTAT  EQU  040Q     FLOPPY STATUS
    ///     FLPDAT  EQU  251Q       IOAFDATA  EQU  041Q     FLOPPY DATA
    /// 
    /// This chip is common to all PERQ I/O Board types.
    /// </remarks>
    public class NECuPD765A : IZ80Device, IDMADevice
    {
        public NECuPD765A(byte baseAddress, Scheduler scheduler)
        {
            _scheduler = scheduler;
            _baseAddress = baseAddress;
            _ports = new byte[] { _baseAddress, (byte)(_baseAddress + 1) };

            _commandData = new Queue<byte>();
            _statusData = new Queue<byte>();
            _drives = new FloppyDisk[2];
            _pcn = new byte[2];
            _pollEvent = null;

            _commands = new CommandData[]
            {
                new CommandData(Command.ReadData, 0x1f, 9, ReadDataExecutor),
                new CommandData(Command.ReadTrack, 0x9f, 9, StubExecutor),
                new CommandData(Command.ReadDeletedData, 0x1f, 9, StubExecutor),
                new CommandData(Command.ReadId, 0xbf, 2, StubExecutor),
                new CommandData(Command.WriteData, 0x3f, 9, WriteDataExecutor),
                new CommandData(Command.FormatTrack, 0xbf, 6, FormatExecutor),
                new CommandData(Command.WriteDeletedData, 0x3f, 9, StubExecutor),
                new CommandData(Command.ScanEqual, 0x1f, 9, StubExecutor),
                new CommandData(Command.ScanLowOrEqual, 0x1f, 9, StubExecutor),
                new CommandData(Command.Recalibrate, 0xff, 2, SeekExecutor),
                new CommandData(Command.SenseInterruptStatus, 0xff, 1, SenseInterruptStatusExecutor),
                new CommandData(Command.Specify, 0xff, 3, SpecifyExecutor),
                new CommandData(Command.SenseDriveStatus, 0xff, 2, SenseDriveStatusExecutor),
                new CommandData(Command.ScanHighOrEqual, 0x1f, 9, StubExecutor),
                new CommandData(Command.Seek, 0xff, 3, SeekExecutor),
            };
        }

        public void Reset()
        {
            // Reset any attached drives
            foreach (var d in _drives)
            {
                d?.Reset();
            }

            _pcn.Initialize();
            _commandData.Clear();
            _statusData.Clear();

            if (_pollEvent != null)
            {
                _scheduler.Cancel(_pollEvent);
                _pollEvent = null;
            }

            _interruptsEnabled = false;
            _interruptActive = false;
            _nonDMAMode = false;
            _status = Status.RQM;
            _state = State.Command;
            _readByte = 0;
            _readDataReady = false;
            _writeByte = 0;
            _writeDataReady = false;
            _unitSelect = 0;
            _headSelect = 0;
            _seekEnd = false;
            _byteTimeNsec = FMByteTimeNsec;

            Log.Debug(Category.FloppyDisk, "Controller reset");
        }


        public string Name => "uPD765A FDC";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x24; // FLPVEC
        public bool IntLineIsActive => _interruptActive & _interruptsEnabled;

        public bool InterruptsEnabled
        {
            get { return _interruptsEnabled; }
            set { _interruptsEnabled = value; }
        }

        private byte StatusPort => _baseAddress;
        private byte DataPort => (byte)(_baseAddress + 1);

        private FloppyDisk SelectedUnit => _drives[_unitSelect];
        private bool SelectedUnitIsReady => SelectedUnit != null && SelectedUnit.Ready;

        private ulong ByteTimeNsec => _byteTimeNsec;

        //
        // IDMADevice Implementation
        //
        bool IDMADevice.ReadDataReady => _readDataReady;
        bool IDMADevice.WriteDataReady => _writeDataReady;

        void IDMADevice.DMATerminate()
        {
            _transfer.Aborted = true;
            Log.Detail(Category.FloppyDisk, "DMA transfer terminated");
        }

        /// <summary>
        /// Attach a floppy drive to the controller.  Technically the PERQ only
        /// ever supported one drive, as the second unit select line was not
        /// connected.  But it'd be nice to have two...
        /// </summary>
        public void AttachDrive(uint unit, FloppyDisk drive)
        {
            if (unit >= _drives.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(unit));
            }

            Log.Debug(Category.FloppyDisk, "Attached floppy unit {0}", unit);
            _drives[unit] = drive;
        }

        /// <summary>
        /// Selects a drive and head for the next operation.  Sets both the
        /// local flags AND pokes the drive -- this picks up the disk change
        /// signal or possibly flags head select errors.
        /// </summary>
        private void SelectUnitHead(byte select)
        {
            _unitSelect = select & 0x3;
            if (SelectedUnit != null)
            {
                SelectedUnit.DriveSelect = true;
                _byteTimeNsec = SelectedUnit.IsDoubleDensity ? MFMByteTimeNsec : FMByteTimeNsec;
            }

            _headSelect = (select & 0x4) >> 2;
            if (SelectedUnitIsReady)
            {
                SelectedUnit.HeadSelect = (byte)_headSelect;
            }
        }

        //
        // IZ80Device implementation (Read & Write)
        //

        #region Z80 IO Read/Write

        public byte Read(byte portAddress)
        {
            if (portAddress == StatusPort)
            {
               Log.Debug(Category.FloppyDisk, "FDC Status read: {0} (0x{1:x})",
                                               _status, (byte)_status);
                _interruptActive = false;
                return (byte)_status;
            }

            if (portAddress == DataPort)
            {
                _interruptActive = false;
                byte data = 0;

                if (_state == State.Result)
                {
                    // Read result data
                    if (_statusData.Count > 0)
                    {
                        byte value = _statusData.Dequeue();

                        // Clear RQM for the next 12 uSec
                        _status &= (~Status.RQM);

                        _scheduler.Schedule(12 * Conversion.UsecToNsec, (skew, context) =>
                        {
                            // Set DIO and RQM appropriately to signify readiness for next data
                            if (_statusData.Count > 0)
                            {
                                // More data to be read
                                _status |= (Status.DIO | Status.RQM);
                            }
                            else
                            {
                                // No more data, clear DIO (waiting for transfer
                                // from processor) and set RQM (ready to receive)
                                _status &= (~Status.DIO);
                                _status |= Status.RQM;
                                _state = State.Command;

                                // Returning to command mode is probably a good
                                // time to check if the drives need a pollin'
                                if (_pollPending) PollDrives();
                            }
                        });

                        data = value;
                    }
                    else
                    {
                        // Unexpected right now
                        throw new InvalidOperationException("FDC Status Read with no data available");
                    }
                }
                else if (_state == State.Execution)
                {
                    if (!_readDataReady)
                    {
                        throw new InvalidOperationException("DMA read with no data ready");
                    }

                    _readDataReady = false;
                    data = _readByte;
                }
                else
                {
                    // Unsure exactly what should happen here
                    throw new InvalidOperationException("FDC Status Read in Command phase");
                }

                return data;
            }

            throw new NotImplementedException($"Unexpected FDC register read from 0x{portAddress:x}");
        }

        public void Write(byte portAddress, byte value)
        {
            if (portAddress != DataPort)
            {
                throw new NotImplementedException($"Unexpected FDC register write to 0x{portAddress:x}");
            }

            _interruptActive = false;

            if (_state == State.Command)
            {
                if (_commandData.Count == 0)
                {
                    // Figure out what command this is
                    foreach (CommandData data in _commands)
                    {
                        if ((value & data.Mask) == (int)data.Command)
                        {
                            _currentCommand = data;
                            Log.Debug(Category.FloppyDisk, "Command is {0}", _currentCommand.Command);
                            break;
                        }
                    }

                    // TODO: Invalid command, handle this properly
                }

                // Store the command data away, reset bits 6 and 7 of the status
                // register, and queue a workitem to set the bits.
                // If this is the last byte of the command, commence execution.
                _commandData.Enqueue(value);

                _status &= (~Status.DIO & ~Status.RQM);

                if (_commandData.Count == _currentCommand.ByteCount)
                {
                    // Set EXM
                    _state = State.Execution;
                    _status |= Status.EXM;
                    _statusData.Clear();

                    if (_currentCommand.Command != Command.SenseInterruptStatus)
                    {
                        // Don't reset this for SenseInterruptStatus since this command needs this value
                        _seekEnd = false;
                    }
                    _scheduler.Schedule(12 * Conversion.UsecToNsec, _currentCommand.Executor);
                }
                else
                {
                    _scheduler.Schedule(12 * Conversion.UsecToNsec, (skew, context) =>
                    {
                        // Set DIO and RQM appropriately to signify readiness for next data
                        _status |= (Status.RQM);
                    });
                }
            }
            else if (_state == State.Execution)
            {
                if (!_writeDataReady)
                {
                    throw new InvalidOperationException("DMA write while not ready");
                }

                _writeByte = value;
                _writeDataReady = false;
            }
            else
            {
                // Unexpected
                throw new InvalidOperationException("Data write while in Result state");
            }
        }

        #endregion

        //
        // Command Executors
        //

        private void StubExecutor(ulong skewNsec, object context)
        {
            throw new NotImplementedException("FDC command not implemented");
        }

        #region Seek, Specify, Sense

        /// <summary>
        /// The Specify command sets chip parameters for delays and drive timings
        /// that we might not really care about.  It also starts the periodic
        /// poll of connected drives for status changes (drive door open/close
        /// events).
        /// </summary>
        private void SpecifyExecutor(ulong skewNsec, object context)
        {
            //
            // Read the data specified.
            // We currently ignore SRT (Step Rate Time), HUT (Head Unload Time)
            // and HLT (Head Load Time) because I'm not anal retentive.
            // TODO: However... the old Z80 actually sets these, and Accent
            // actually programs the values too.  So we _could_ actually set up
            // the proper delays that the I/O code expects... :-)
            //
            _commandData.Dequeue();     // command byte
            _commandData.Dequeue();     // SRT/HUT
            _nonDMAMode = (_commandData.Dequeue() & 0x1) != 0;

            // If we aren't polling, start the loop
            if (_pollEvent == null)
            {
                PollTimer(0, null);
            }

            FinishCommand();
        }

        /// <summary>
        /// Initiate a Seek or Recalibrate (which is just a seek to track 0).
        /// </summary>
        private void SeekExecutor(ulong skewNsec, object context)
        {
            var cmd = (Command)_commandData.Dequeue();
            SelectUnitHead(_commandData.Dequeue());
            var cylinder = (cmd == Command.Seek) ? _commandData.Dequeue() : (byte)0;

            // Set "Busy" bit for this drive:
            _status |= (Status)(0x1 << _unitSelect);

            if (SelectedUnitIsReady)
            {
                Log.Debug(Category.FloppyDisk, "Unit {0} seek to cyl {1} scheduled", _unitSelect, cylinder);

                _pcn[_unitSelect] = cylinder;
                SelectedUnit.SeekTo(cylinder, SeekCompleteCallback);
            }
            else
            {
                // Can't seek on a drive that's not ready (or not connected)
                Log.Warn(Category.FloppyDisk, "{0} issued but drive {1} not ready", cmd, _unitSelect);

                // Fire the callback to complete command
                SeekCompleteCallback(0, null);
            }
        }

        private void SeekCompleteCallback(ulong skewNsec, object context)
        {
            // Stimpy!  We made it!
            _seekEnd = true;

            // Can't ya read?  Exact change ONLY!
            _seekEnd &= !SelectedUnit.DiskChange;

            // Simply implode!
            SetErrorStatus(StatusRegister0.None);

            // Clear drive "Busy" bit and interrupt
            _status &= ~((Status)(0x1 << _unitSelect));
            _interruptActive = true;

            Log.Debug(Category.FloppyDisk, "Unit {0} seek to cyl {1} completed", _unitSelect, _pcn[_unitSelect]);
            FinishCommand();
        }


        private void SenseInterruptStatusExecutor(ulong skewNsec, object context)
        {
            _commandData.Dequeue();     // toss command byte

            // Return ST0 and PCN, and clear the interrupt active flag
            _interruptActive = false;

            _statusData.Enqueue((byte)_errorStatus);    // ST0 (SEEK END)
            _statusData.Enqueue(_pcn[_unitSelect]);     // PCN

            Log.Debug(Category.FloppyDisk, "SenseInterruptStatus: {0}", _errorStatus);
            FinishCommand();
        }

        /// <summary>
        /// Executes the floppy SENSE DRIVE STATUS command.  Not used by the
        /// old IOB Z80, but used by the new EIO Z80!
        /// </summary>
        private void SenseDriveStatusExecutor(ulong skewNsec, object context)
        {
            _commandData.Dequeue();                     // toss command byte
            SelectUnitHead(_commandData.Dequeue());     // get drive to query

            var ST3 = (StatusRegister3)_unitSelect;

            if (SelectedUnit != null)
            {
                // Build up ST3 result bits from the selected drive
                ST3 |= (SelectedUnit.HeadSelect > 0 ? StatusRegister3.Head : StatusRegister3.None) |
                       (SelectedUnit.IsSingleSided ? StatusRegister3.None : StatusRegister3.TwoSided) |
                       (SelectedUnit.Track0 ? StatusRegister3.Track0 : StatusRegister3.None) |
                       (SelectedUnit.Ready ? StatusRegister3.Ready : StatusRegister3.None) |
                       (SelectedUnit.Info.IsWritable ? StatusRegister3.None : StatusRegister3.WriteProtected) |
                       (SelectedUnit.Fault ? StatusRegister3.Fault : StatusRegister3.None);
            }
            else
            {
                // This is a guess...
                ST3 |= StatusRegister3.Fault;
            }

            _statusData.Enqueue((byte)ST3);

            Log.Debug(Category.FloppyDisk, "SenseDriveStatus: {0}", ST3);
            FinishCommand();
        }

        #endregion

        #region Reads, Writes

        private void ReadDataExecutor(ulong skewNsec, object context)
        {
            if (BeginSectorTransfer())
            {
                _scheduler.Schedule(SectorTimeNsec, SectorTransferCallback);
            }
        }

        private void WriteDataExecutor(ulong skewNsec, object context)
        {
            if (BeginSectorTransfer())
            {
                _scheduler.Schedule(SectorTimeNsec, SectorTransferCallback);
            }
        }

        private bool IsReadType(Command c)
        {
            return (c == Command.ReadId ||
                    c == Command.ReadData ||
                    c == Command.ReadTrack ||
                    c == Command.ReadDeletedData);
        }

        private bool IsWriteType(Command c)
        {
            return (c == Command.WriteData || c == Command.WriteDeletedData);
        }

        private bool BeginSectorTransfer()
        {
            _transfer = new TransferRequest();
            _transfer.Type = _currentCommand.Command;

            // Pull data from command regs:
            byte commandByte = _commandData.Dequeue();
            _transfer.MultiTrack = (commandByte & 0x80) != 0;
            _transfer.MFM = (commandByte & 0x40) != 0;
            _transfer.SkipDeletedAM = (commandByte & 0x20) != 0;
            SelectUnitHead(_commandData.Dequeue());
            _transfer.Cylinder = _commandData.Dequeue();
            _transfer.Head = _commandData.Dequeue();
            _transfer.Sector = _commandData.Dequeue();
            _transfer.Number = _commandData.Dequeue();
            _transfer.EndOfTrack = _commandData.Dequeue();
            byte gpl = _commandData.Dequeue();
            byte dtl = _commandData.Dequeue();

            _transfer.SectorLength = (_transfer.Number == 0) ? dtl : (ushort)(128 << _transfer.Number);
            _transfer.Aborted = false;

            // Transfers set the busy bit
            _status |= Status.CB;

#if DEBUG
            if (_transfer.MultiTrack)
                throw new InvalidOperationException("Floppy multi-track not supported");
#endif

            Log.Debug(Category.FloppyDisk,
                      "{0} unit {1}, C/H/S {2}/{3}/{4} (eot={5}, length={6})",
                      _transfer.Type == Command.ReadData ? "Read" : "Write",
                      _unitSelect,
                      _transfer.Cylinder,
                      _transfer.Head,
                      _transfer.Sector,
                      _transfer.EndOfTrack,
                      _transfer.SectorLength);

            if (!SelectedUnitIsReady)
            {
                // Post drive not ready (EquipChk doesn't cause Floppy to seize)
                _transfer.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                _transfer.ST1 = StatusRegister1.None;
                _transfer.ST2 = StatusRegister2.None;

                // Post the failure:
                FinishTransfer(_transfer);
                return false;
            }

            if (!SelectedUnit.Info.IsWritable && IsWriteType(_transfer.Type))
            {
                // Oh noes we forgot to put the sticker on
                _transfer.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                _transfer.ST1 = StatusRegister1.NotWritable;
                _transfer.ST2 = StatusRegister2.None;

                FinishTransfer(_transfer);
                return false;
            }

            return true;
        }


        private void SectorTransferCallback(ulong skewNsec, object context)
        {
            // Simulate the initial search for the sector, then kick off the sector transfer:
            Log.Detail(Category.FloppyDisk, "In transfer callback");

            // Set preliminary status info
            _transfer.ST0 = SetErrorStatus(StatusRegister0.None);

            // Validate the operation:  if the cylinder is out of range or the
            // sector does not exist on disk, flag the appropriate error and exit
            if (_transfer.Cylinder > SelectedUnit.Geometry.Cylinders ||
                _transfer.Cylinder != SelectedUnit.Cylinder)
            {
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST2 = StatusRegister2.WrongCylinder;
                FinishTransfer(_transfer);
                return;
            }

            // Validate the head vs. the number of sides on the disk:
            //if (SelectedUnit.IsSingleSided && _transfer.Head != 0)
            if (!SelectedUnitIsReady)
            {
                // The SA851 drive drops the Ready line if head 1 is selected
                // with a single sided diskette inserted; probably doesn't hurt
                // to set the NoData flag, but NotReady should cover it?  This
                // should also catch if an eject was done unexpectedly...
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.NoData;
                FinishTransfer(_transfer);
                return;
            }

            // Readin' or writin'?
            if (IsWriteType(_transfer.Type))
            {
                _transfer.SectorData = new Sector(_transfer.Cylinder,
                                                  _transfer.Head,
                                                  _transfer.Sector,
                                                  _transfer.SectorLength);
            }
            else
            {
                _transfer.SectorData = SelectedUnit.Read(_transfer.Cylinder,
                                                         _transfer.Head,
                                                         _transfer.Sector);
            }

            if (_transfer.SectorData.Data.Length == 0)
            {
                // No sector data available:
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.NoData;      // Sector not found
                FinishTransfer(_transfer);
                return;
            }

            // Check sector format; expect either FM500 or MFM500, matching the MFM flag
            if ((_transfer.MFM && _transfer.SectorLength != 256) ||
               (!_transfer.MFM && _transfer.SectorLength != 128))
            {
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.NoData;      // Sector not found (TODO: is this correct?)
                FinishTransfer(_transfer);
                return;
            }

            // Reset our byte counter, and away we go!
            _transfer.TransferIndex = 0;

            if (IsReadType(_transfer.Type))
            {
                // When reading, jump into the callback with the ready flag off;
                // gives us a few usec to fetch the first byte before going nuts
                _readDataReady = false;
                _scheduler.Schedule(ByteTimeNsec, SectorByteReadCallback);
            }
            else if (IsWriteType(_transfer.Type))
            {
                // If writing, request the first byte now, then queue the callback
                // to give DMA time to ship it to over
                _writeDataReady = true;
                _scheduler.Schedule(ByteTimeNsec, SectorByteWriteCallback);
            }
            else
            {
                throw new NotImplementedException();
            }
        }


        private void SectorByteReadCallback(ulong skewNsec, object context)
        {
            // If the last trip was the end of the transfer, we're done
            if (_transfer.Aborted || _transfer.TransferIndex == _transfer.SectorLength)
            {
                FinishSectorTransfer();
                return;
            }

            // If the last byte was not read by now, this is an overrun
            if (_readDataReady)
            {
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.OverRun;
                FinishTransfer(_transfer);
                return;
            }

            // Get the next byte to send
            _readByte = _transfer.SectorData.ReadByte((uint)_transfer.TransferIndex++);

            Log.Detail(Category.FloppyDisk, "Read byte {0} from index {1}",
                                            _readByte, _transfer.TransferIndex - 1);

            // Tell the DMA we're ready and schedule the next one
            _readDataReady = true;
            _scheduler.Schedule(ByteTimeNsec, SectorByteReadCallback);
        }


        private void SectorByteWriteCallback(ulong skewNsec, object context)
        {
            // If the last byte was not written, this is an overrun
            if (_writeDataReady)
            {
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.OverRun;
                FinishTransfer(_transfer);
                return;
            }

            Log.Detail(Category.FloppyDisk, "Write byte 0x{0:x2} to index {1}",
                                            _writeByte, _transfer.TransferIndex);

            // Write the received byte:
            _transfer.SectorData.WriteByte((uint)_transfer.TransferIndex++, _writeByte);

            // Sector complete?
            if (_transfer.Aborted || _transfer.TransferIndex == _transfer.SectorLength)
            {
                FinishSectorTransfer();
                return;
            }

            // Gimme gimme gimme, I need some more
            _writeDataReady = true;
            _scheduler.Schedule(ByteTimeNsec, SectorByteWriteCallback);
        }


        private void FinishSectorTransfer()
        {
            Log.Debug(Category.FloppyDisk, "Sector {0} transfer complete", _transfer.Sector);

            // Should only get here when a complete sector transfer has completed
            // so commit the new data if writing.  Prevents partial writes...
            if (IsWriteType(_transfer.Type))
            {
                SelectedUnit.Write(_transfer.SectorData);
            }

            // Did we reach the last sector in the track?  
            // Was the request aborted by the DMA controller?
            if (_transfer.Aborted || _transfer.Sector == _transfer.EndOfTrack)
            {
                // We're done, so report success
                _transfer.ST1 = StatusRegister1.None;
                _transfer.ST2 = StatusRegister2.None;
                FinishTransfer(_transfer);
                return;
            }

            // Nope!  Increment the sector counter and go 'round again
            _transfer.Sector++;

            _scheduler.Schedule(SectorTimeNsec, SectorTransferCallback);

            Log.Detail(Category.FloppyDisk, "Next sector transfer queued");
        }

        #endregion

        #region Format

        private void FormatExecutor(ulong skewNsec, object context)
        {
            _transfer = new TransferRequest();
            _transfer.Type = Command.FormatTrack;
            _transfer.MFM = (_commandData.Dequeue() & 0x40) != 0;
            SelectUnitHead(_commandData.Dequeue());
            _transfer.Number = _commandData.Dequeue();
            _transfer.SectorCount = _commandData.Dequeue();
            _commandData.Dequeue(); // GPL, discarded for now (we don't emulate gaps)
            _transfer.Pattern = _commandData.Dequeue();

            _transfer.SectorLength = (ushort)(128 << _transfer.Number);
            _transfer.SectorIndex = 0;

            // Should we do this?
            if (!SelectedUnit.Info.IsWritable)
            {
                _transfer.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                _transfer.ST1 = StatusRegister1.NotWritable;
                _transfer.ST2 = StatusRegister2.None;

                FinishTransfer(_transfer);
                return;
            }

            // Format works by doing 4 DMA transfers (for format parameters) per
            // sector on the track.  Indicate that we're ready for a DMA transfer,
            // then kick off the sector format
            _writeDataReady = true;

            _scheduler.Schedule(SectorTimeNsec, SectorFormatCallback);
        }


        private void SectorFormatCallback(ulong skewNsec, object context)
        {
            // If the last byte was not written, this is an overrun
            if (_writeDataReady)
            {
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.OverRun;
                FinishTransfer(_transfer);
                return;
            }

            // We need four bytes from the processor before we can format this
            // sector; collect them up:
            switch (_transfer.TransferIndex)
            {
                case 0:
                    _transfer.Cylinder = _writeByte;
                    _transfer.TransferIndex++;
                    break;

                case 1:
                    _transfer.Head = _writeByte;
                    _transfer.TransferIndex++;
                    break;

                case 2:
                    _transfer.Sector = _writeByte;
                    _transfer.TransferIndex++;
                    break;

                case 3:
                    _transfer.Number = _writeByte;

                    // Create a new sector
                    _transfer.SectorData = new Sector(_transfer.Cylinder,
                                                      _transfer.Head,
                                                      _transfer.Sector,
                                                      _transfer.SectorLength);

                    // Fill it with the format pattern byte (if given)
                    if (_transfer.Pattern != 0)
                    {
                        for (uint i = 0; i < _transfer.SectorLength; i++)
                            _transfer.SectorData.WriteByte(i, _transfer.Pattern);
                    }

                    // Sanity check: writable disk? Good sector address?  Neither
                    // of these are likely (format checks for write protect before
                    // we even begin, and the Z80 should never try to format with
                    // the wrong geometry).  The check below won't actually allow
                    // us to write past the end of track, but that's the only
                    // status code that makes sense to return here.  (Can probably
                    // just remove this after verifying with other Z80/OS combos.)
                    if (!SelectedUnit.WriteCheck(_transfer.SectorData))
                    {
                        _transfer.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                        _transfer.ST1 = SelectedUnit.Info.IsWritable ? StatusRegister1.EndCylinder
                                                                     : StatusRegister1.NotWritable;
                        _transfer.ST2 = StatusRegister2.None;
                        FinishTransfer(_transfer);
                        return;
                    }

                    // Write the sector
                    SelectedUnit.Write(_transfer.SectorData);

                    Log.Detail(Category.FloppyDisk,
                               "Formatted sector {0}, {1} bytes (pattern 0x{2:x2})",
                               _transfer.Sector, _transfer.SectorLength, _transfer.Pattern);

                    // Move to the next
                    _transfer.SectorIndex++;
                    _transfer.TransferIndex = 0;

                    // Done?
                    if (_transfer.SectorIndex == _transfer.SectorCount)
                    {
                        // Report success!
                        _transfer.ST0 = SetErrorStatus(StatusRegister0.None);
                        _transfer.ST1 = StatusRegister1.None;
                        _transfer.ST2 = StatusRegister2.None;
                        FinishTransfer(_transfer);
                        return;
                    }
                    break;

                default:
                    throw new InvalidOperationException("Unexpected state during Format operation");
            }

            // Do it again, do it again
            _writeDataReady = true;
            _scheduler.Schedule(_transfer.TransferIndex == 0 ? SectorTimeNsec : ByteTimeNsec,
                                SectorFormatCallback);
        }

        #endregion

        #region Finish Commands, SetStatus

        /// <summary>
        /// Finish a data transfer: queue up the results bytes and interrupt.
        /// </summary>
        private void FinishTransfer(TransferRequest request)
        {
            // Shut off DMA if early/abnormal termination?
            if (_readDataReady || _writeDataReady)
            {
                _readDataReady = false;
                _writeDataReady = false;
            }
            
            // Apply the "ID Information at Result Phase" rules
            // todo: if anything uses MT, that subtly changes things at eot
            if (request.Sector < request.EndOfTrack)
            {
                request.Sector++;
            }
            else
            {
                request.Cylinder++;
                request.Sector = 1;
            }

            Log.Detail(Category.FloppyDisk, "Transfer completed:");
            Log.Detail(Category.FloppyDisk, "C{0}/H{1}/S{2} N{3}", request.Cylinder, request.Head, request.Sector, request.Number);
            Log.Detail(Category.FloppyDisk, "ST0 = {0}", request.ST0);
            Log.Detail(Category.FloppyDisk, "ST1 = {0}", request.ST1);
            Log.Detail(Category.FloppyDisk, "ST2 = {0}", request.ST2);

            // Post result data to the status register queue:
            _statusData.Enqueue((byte)request.ST0);
            _statusData.Enqueue((byte)request.ST1);
            _statusData.Enqueue((byte)request.ST2);
            _statusData.Enqueue((byte)request.Cylinder);
            _statusData.Enqueue((byte)request.Head);
            _statusData.Enqueue((byte)request.Sector);
            _statusData.Enqueue((byte)request.Number);

            // Shut off the busy bit and raise the interrupt
            _status &= (~Status.CB);
            _interruptActive = true;
            FinishCommand();
        }

        /// <summary>
        /// Finish command processing by setting state, status and clearing
        /// the command data queue.
        /// </summary>
        private void FinishCommand()
        {
            // Reset EXM, set RQM and DIO to let the processor know results are
            // ready to read.
            _status &= (~Status.EXM);

            if (_statusData.Count > 0)
            {
                _status |= (Status.DIO | Status.RQM);
                _state = State.Result;
            }
            else
            {
                _status &= (~Status.DIO);
                _status |= Status.RQM;
                _state = State.Command;
            }

            _commandData.Clear();
        }

        /// <summary>
        /// Fill in the common fields of Status Register 0.  This is done often
        /// enough to do it consistently.  If a non-existent drive is selected,
        /// returns AbnormalTermination with NotReady and EquipChk set.
        /// </summary>
        private StatusRegister0 SetErrorStatus(StatusRegister0 error)
        {
            _errorStatus = error |
                           (StatusRegister0)_unitSelect |
                           (_seekEnd ? StatusRegister0.SeekEnd : StatusRegister0.None);

            if (SelectedUnitIsReady)
            {
                // Give back actual drive status
                _errorStatus |= (SelectedUnit.HeadSelect > 0 ? StatusRegister0.Head : StatusRegister0.None) |
                                (SelectedUnit.Fault ? StatusRegister0.EquipChk : StatusRegister0.None) |
                                (SelectedUnit.Ready ? StatusRegister0.None : StatusRegister0.NotReady);
            }
            else
            {
                // There ain't no drive there!  How will the PERQ react?
                //      Seek End = 0, Intr Code = Ready line state changed
                //      Seek End = 1, Intr Code = Abnormal termination (Seek/Recal)
                _errorStatus |= _seekEnd ? (StatusRegister0.AbnormalTermination | StatusRegister0.EquipChk) :
                                           (StatusRegister0.ReadySignalChanged /* | StatusRegister0.NotReady*/ );
            }

            return _errorStatus;
        }

        #endregion

        #region Drive Status Polling

        /// <summary>
        /// Polls the drives for ready line changes.
        /// </summary>
        /// <remarks>
        /// If a change in the ready status is detected (in our case, a floppy
        /// Load or Unload has set the DiskChange signal) then an interrupt is
        /// generated and a subsequent Sense Interrupt Status will return
        /// ReadySignalChanged in ST0.  It's unclear if the PERQ actually
        /// notices this, but it's built-in to the chip and it provides a way
        /// to clear DiskChange so we should do it.  Only checked once per
        /// second so it's very low overhead.
        /// </remarks>
        private void PollDrives()
        {
            // In Command mode and no busy flags?
            if (_state == State.Command && ((byte)_status & 0x1f) == 0)
            {
                Log.Detail(Category.FloppyDisk, "Starting drive poll");

                // Ok to poll
                var changed = false;

                for (var d = 0; d < _drives.Length; d++)
                {
                    if (_drives[d] != null && _drives[d].DiskChange)
                    {
                        changed = true;
                        _drives[d].DriveSelect = false; // Clears DiskChange
                        SelectUnitHead((byte)d);        // Reselect unit, head 0
                    }
                }

                // Set ST0 and Interrupt...
                if (changed)
                {
                    Log.Debug(Category.FloppyDisk, "Signalling status change!");
                    _seekEnd = false;
                    SetErrorStatus(StatusRegister0.ReadySignalChanged);
                    _interruptActive = true;
                }

                Log.Detail(Category.FloppyDisk, "Drive poll completed @ {0}", _scheduler.CurrentTimeNsec);
            }

            // Reset for next time
            _pollPending = false;
        }

        /// <summary>
        /// Signal that the drives should be polled for status changes, then
        /// reschedule again, until reset.  (The actual call to poll the drives
        /// can then be done when the controller isn't busy, avoiding weird
        /// state interactions having this run asynchronously.)
        /// </summary>
        private void PollTimer(ulong skewNsec, object context)
        {
            Log.Detail(Category.FloppyDisk, "Drive status poll requested @ {0}", _scheduler.CurrentTimeNsec);

            _pollPending = true;
            _pollEvent = _scheduler.Schedule(PollTimeNsec, PollTimer);
        }

        #endregion

        /// <summary>
        /// Supplemental data structure: Transfer request.
        /// </summary>
        private struct TransferRequest
        {
            // Transfer type
            public Command Type;

            // Disk Control
            public byte Head;
            public ushort Cylinder;
            public ushort Sector;
            public ushort SectorLength;
            public int Number;
            public int EndOfTrack;
            public bool MultiTrack;
            public bool MFM;
            public bool SkipDeletedAM;

            // Sector data (if for a valid sector)
            public Sector SectorData;
            public int TransferIndex;

            // Sector count and index (used while formatting)
            public ushort SectorCount;
            public int SectorIndex;
            public byte Pattern;

            // Cancellation
            public bool Aborted;

            // Status Bits (for transfer completion)
            public StatusRegister0 ST0;
            public StatusRegister1 ST1;
            public StatusRegister2 ST2;
        }


        private enum State
        {
            Command = 0,
            Execution,
            Result
        }

        [Flags]
        private enum Status
        {
            D0B = 0x1,      // FDD N Busy
            D1B = 0x2,
            D2B = 0x4,
            D3B = 0x8,
            CB = 0x10,      // Controller Busy (r/w in process, FDC will not accept commands)
            EXM = 0x20,     // Execution phase in non-DMA mode
            DIO = 0x40,     // Transfer direction - 1 = to processor, 0 = from processor
            RQM = 0x80,     // Request for Master - Data Reg is ready to xfer data
        }

        [Flags]
        private enum StatusRegister0
        {
            None = 0x0,
            Unit0 = 0x1,
            Unit1 = 0x2,    // Sadly, this pin is NC on the PERQ
            Head = 0x4,
            NotReady = 0x8,
            EquipChk = 0x10,
            SeekEnd = 0x20,
            AbnormalTermination = 0x40,     // Interrupt code bits are grouped:
            InvalidCommandIssue = 0x80,     // 00=Normal,  01=Abnormal
            ReadySignalChanged = 0xc0       // 10=Invalid, 11=RdyChanged
        }

        [Flags]
        private enum StatusRegister1
        {
            None = 0x0,
            MissingAddrMark = 0x1,
            NotWritable = 0x2,
            NoData = 0x4,
            Unused1 = 0x8,
            OverRun = 0x10,
            DataError = 0x20,
            Unused2 = 0x40,
            EndCylinder = 0x80
        }

        [Flags]
        private enum StatusRegister2
        {
            None = 0x0,
            MissingAddrMarkInDataField = 0x1,
            BadCylinder = 0x2,
            ScanNotSatisfied = 0x4,
            ScanEqualHit = 0x8,
            WrongCylinder = 0x10,
            DataErrorInDataField = 0x20,
            ControlMark = 0x40
        }

        [Flags]
        private enum StatusRegister3
        {
            None = 0x0,
            Unit0 = 0x1,
            Unit1 = 0x2,
            Head = 0x4,
            TwoSided = 0x8,
            Track0 = 0x10,
            Ready = 0x20,
            WriteProtected = 0x40,
            Fault = 0x80
        }

        private enum Command
        {
            ReadTrack = 0x02,
            ReadData = 0x06,
            ReadDeletedData = 0x0c,
            ReadId = 0x0a,
            WriteData = 0x05,
            FormatTrack = 0x0d,
            WriteDeletedData = 0x09,
            ScanEqual = 0x11,
            ScanLowOrEqual = 0x19,
            ScanHighOrEqual = 0x1d,
            Recalibrate = 0x07,
            SenseInterruptStatus = 0x08,
            Specify = 0x03,
            SenseDriveStatus = 0x04,
            Seek = 0x0f,
        }

        private struct CommandData
        {
            public CommandData(Command command, byte mask, int byteCount, SchedulerEventCallback executor)
            {
                Command = command;
                Mask = mask;
                ByteCount = byteCount;
                Executor = executor;
            }

            public Command Command;
            public byte Mask;
            public int ByteCount;
            public SchedulerEventCallback Executor;
        }


        private FloppyDisk[] _drives;
        private byte[] _pcn;

        private int _unitSelect;
        private int _headSelect;

        private Status _status;
        private State _state;

        private CommandData[] _commands;
        private CommandData _currentCommand;
        private Queue<byte> _commandData;
        private Queue<byte> _statusData;

        // The current transfer in progress
        private TransferRequest _transfer;

        private ulong _byteTimeNsec;

        private byte _readByte;
        private bool _readDataReady;

        private byte _writeByte;
        private bool _writeDataReady;

        // ST0 bits (used by SenseInterruptStatus)
        private StatusRegister0 _errorStatus;
        private bool _seekEnd;
        private bool _interruptsEnabled;
        private bool _interruptActive;
        private bool _nonDMAMode;

        // Timing constants
        // Sector time: 360 RPM, 26 sectors per rotation.  This is just for approximate timing.
        private readonly ulong SectorTimeNsec = (ulong)((166.667 / 26.0) * Conversion.MsecToNsec);

        // Timing for FM and MFM mode
        private readonly ulong FMByteTimeNsec = 27 * Conversion.UsecToNsec;
        private readonly ulong MFMByteTimeNsec = 13 * Conversion.UsecToNsec;

        // Status polling interval
        private readonly ulong PollTimeNsec = 1024 * Conversion.MsecToNsec;

        private volatile bool _pollPending;
        private SchedulerEvent _pollEvent;

        // System interface
        private byte _baseAddress;
        private byte[] _ports;
        private Scheduler _scheduler;

        // I hate this, Konamiman.  I hate this So much.
        public event EventHandler NmiInterruptPulse;
    }
}

/*
 
 Notes:

    We could implement the read/write "deleted data" functions by just setting
    a flag in the read/write executors to use the IsBad flag in the Sector.  I
    don't think an PERQ software ever bothered with that, though they do allow
    for those commands to be sent and the SK bit checked?
    
    Accent may have used the Read Track command; will probably implement that
    at some point.

    The head loading/settling/unloading times given in the Specify command are
    actually set by the firmware and by Accent's floppy drive, though there's
    little-to-no benefit to using them; the default 3ms head settling value is
    set in the DevicePerformance record and the FloppyDisk could reference it
    just to make floppy operations that much more agonizingly accurate. :-)
 
 */
