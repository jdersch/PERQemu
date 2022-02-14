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
    public class NECuPD765A : IZ80Device, IDMADevice
    {
        // TODO: Ctor should take base I/O address
        public NECuPD765A(Scheduler scheduler)
        {
            _scheduler = scheduler;
            _commandData = new Queue<byte>();
            _statusData = new Queue<byte>();
            _drives = new FloppyDisk[2];
            _pcn = new byte[2];

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

        //
        // IDMADevice Implementation
        //
        bool IDMADevice.ReadDataReady => _readDataReady;
        bool IDMADevice.WriteDataReady => _writeDataReady;

        void IDMADevice.DMATerminate()
        {
            _transfer.Aborted = true;
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

            Log.Debug(Category.FloppyDisk, "Attached disk '{0}'", drive.Info.Name);
            _drives[unit] = drive;
        }

        /// <summary>
        /// Disconnect a drive from the controller.
        /// </summary>
        /// <remarks>
        /// Not needed?  _drive.Unload() and .Load() can simply eject and read
        /// new floppy images in place, and the OnLoad() trigger can update the
        /// controller with a RDY change.  Hmm.
        /// </remarks>
        public void DetachDrive(uint unit)
        {
            if (unit >= _drives.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(unit));
            }

            if (_drives[unit] != null)
            {
                Log.Debug(Category.FloppyDisk, "Detached disk '{0}'", _drives[unit].Info.Name);
                _drives[unit] = null;
            }
        }

        private FloppyDisk SelectedUnit => _drives[_unitSelect];
        private bool SelectedUnitIsReady => SelectedUnit != null && SelectedUnit.Ready;

        private void SelectUnitHead(byte select)
        {
            _unitSelect = select & 0x3;
            _headSelect = (select & 0x4) >> 2;
        }

        //
        // IZ80Device implementation (Read & Write)
        //

        public byte Read(byte portAddress)
        {
            switch ((Register)portAddress)
            {
                case Register.Status:
                    Log.Debug(Category.FloppyDisk, "FDC Status read: {0} (0x{1:x})",
                                                   _status, (byte)_status);
                    return (byte)_status;

                case Register.Data:
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

                default:
                    throw new NotImplementedException();
            }
        }

        public void Write(byte portAddress, byte value)
        {
            switch ((Register)portAddress)
            {
                case Register.Data:
                    WriteData(value);
                    break;

                default:
                    throw new NotImplementedException($"Unexpected FDC register write to 0x{portAddress:x}");
            }
        }

        private void WriteData(byte value)
        {
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

                    // Invalid command, handle this properly
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

        //
        // Command Executors
        //

        private void StubExecutor(ulong skewNsec, object context)
        {
            throw new NotImplementedException("FDC command not implemented");
        }

        private void SpecifyExecutor(ulong skewNsec, object context)
        {
            //
            // Read the data specified.
            // We currently ignore SRT (Step Rate Time), HUT (Head Unload Time)
            // and HLT (Head Load Time) because I'm not anal retentive.
            // TODO: However... the old Z80 actually sets these, and Accent
            // actually programs the values too.  So we _could_ actually set up
            // the proper delays that the I/O code expects...
            //
            _commandData.Dequeue();     // command byte
            _commandData.Dequeue();     // SRT/HUT
            _nonDMAMode = (_commandData.Dequeue() & 0x1) != 0;

            // TODO: controller goes into polling mode and will interrupt if any
            // drive gets a disk inserted/removed

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

            SetErrorStatus(SelectedUnitIsReady ? StatusRegister0.None : StatusRegister0.AbnormalTermination);

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

        private void ReadDataExecutor(ulong skewNsec, object context)
        {
            if (BeginSectorTransfer())
            {
                _scheduler.Schedule(_sectorTimeNsec, SectorTransferCallback);
            }
        }

        private void WriteDataExecutor(ulong skewNsec, object context)
        {
            if (BeginSectorTransfer())
            {
                _scheduler.Schedule(_sectorTimeNsec, SectorTransferCallback);
            }
        }

        private void FormatExecutor(ulong skewNsec, object context)
        {
            _transfer = new TransferRequest();
            _transfer.Type = Command.FormatTrack;
            _transfer.MFM = (_commandData.Dequeue() & 0x40) != 0;
            SelectUnitHead(_commandData.Dequeue());
            _transfer.Number = _commandData.Dequeue();
            _transfer.SectorCount = _commandData.Dequeue();
            _commandData.Dequeue(); // GPL, discarded for now (we don't emulate gaps)
            _commandData.Dequeue(); // D, discarded (don't care about filler data)

            _transfer.SectorLength = (ushort)(128 << _transfer.Number);

            // Format works by doing 4 DMA transfers (for format parameters) per
            // sector on the track.  Indicate that we're ready for a DMA transfer,
            // then kick off the sector format
            _writeDataReady = true;

            _scheduler.Schedule(_sectorTimeNsec, SectorFormatCallback);
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

            Log.Debug(Category.FloppyDisk,
                    "{0} from unit {1}, C/H/S {2}/{3}/{4} to sector {5}, length {6}.",
                    _transfer.Type == Command.ReadData ? "Read" : "Write",
                    _unitSelect,
                    _transfer.Cylinder,
                    _transfer.Head,
                    _transfer.Sector,
                    _transfer.EndOfTrack,
                    _transfer.SectorLength);

            if (!SelectedUnitIsReady)
            {
                // Post drive not ready (TODO: this isn't correct, apparently?
                // FLOPPY hates this so much it hangs forever.)
                _transfer.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination); // StatusRegister0.EquipChk);
                _transfer.ST1 = StatusRegister1.None;
                _transfer.ST2 = StatusRegister2.None;

                // Post the failure:
                FinishTransfer(_transfer);
                return false;
            }

            return true;
        }

        private void SectorTransferCallback(ulong skewNsec, object context)
        {
            // Simulate the initial search for the sector, then kick off the sector transfer:
            Log.Debug(Category.FloppyDisk, "In transfer callback");

            // Set preliminary status info
            _transfer.ST0 = SetErrorStatus(StatusRegister0.None);

            // If Request was aborted by the DMA controller, stop here
            if (_transfer.Aborted)
            {
                // This is considered a success
                _transfer.ST1 = StatusRegister1.None;
                _transfer.ST2 = StatusRegister2.None;
                FinishTransfer(_transfer);
                return;
            }

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
            if (SelectedUnit.IsSingleSided && _transfer.Head != 0)
            {
                _transfer.ST0 |= StatusRegister0.AbnormalTermination;
                _transfer.ST1 = StatusRegister1.NoData;      // TODO: is this correct?  What does this really report for a single-sided disk?
                FinishTransfer(_transfer);
                return;
            }

            // Grab the sector:
            _transfer.SectorData = SelectedUnit.GetSector(_transfer.Cylinder,
                                                                 _transfer.Head,
                                                                 _transfer.Sector);

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

            // OK, we actually have sector data now!  Queue up the first callback:
            if (_transfer.Type == Command.ReadData)
            {
                _scheduler.Schedule(_byteTimeNsec, SectorByteReadCallback);
            }
            else if (_transfer.Type == Command.WriteData)
            {
                _writeDataReady = true;
                _scheduler.Schedule(_byteTimeNsec, SectorByteWriteCallback);
            }
            else
            {
                throw new NotImplementedException();
            }
        }

        private void SectorByteReadCallback(ulong skewNsec, object context)
        {
            if (_transfer.TransferIndex == _transfer.SectorLength)
            {
                // Done
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

            _readByte = _transfer.SectorData.Data[_transfer.TransferIndex++];
            _readDataReady = true;

            _scheduler.Schedule(_byteTimeNsec, SectorByteReadCallback);
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

            Log.Debug(Category.FloppyDisk, "Wrote byte 0x{0} to index {1}",
                                          _writeByte, _transfer.TransferIndex);

            // Write the next byte:
            _transfer.SectorData.Data[_transfer.TransferIndex++] = _writeByte;

            if (_transfer.TransferIndex == _transfer.SectorLength)
            {
                FinishSectorTransfer();     // Done
            }
            else
            {
                _writeDataReady = true;
                _scheduler.Schedule(_byteTimeNsec, SectorByteWriteCallback);
            }
        }

        private void FinishSectorTransfer()
        {
            Log.Debug(Category.FloppyDisk, "Sector {0} transfer complete", _transfer.Sector);

            // Always increment the sector counter
            _transfer.Sector++;

            // Are there any sectors left to transfer?
            if (_transfer.Sector > _transfer.EndOfTrack)
            {
                // Nope, done, report success:
                _transfer.ST0 = SetErrorStatus(StatusRegister0.None);
                _transfer.ST1 = StatusRegister1.None;
                _transfer.ST2 = StatusRegister2.None;
                FinishTransfer(_transfer);
            }
            else
            {
                _scheduler.Schedule(_sectorTimeNsec, SectorTransferCallback);

                Log.Debug(Category.FloppyDisk, "Next sector transfer queued");
            }
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

                    // Format the sector now:
                    Track diskTrack;

                    if (_transfer.SectorIndex == 0)
                    {
                        // First sector in the track to be formatted: Create a new, empty track,
                        // which will be filled during the rest of this operation.  (Note: we use
                        // the passed-in cylinder/head information for the track itself; it gets
                        // inserted into the disk image on the cylinder/head the drive is currently
                        // pointing to).  It is technically possible for the processor to change
                        // cyl/head designations in the middle of the format; IMD does not support
                        // this possibility and I don't expect the PERQ to try it.
                        diskTrack = new Track(_transfer.Cylinder,
                                              _transfer.Head,
                                              _transfer.SectorCount,
                                              _transfer.SectorLength);
                        SelectedUnit.SetTrack(_transfer.Cylinder, (byte)_headSelect, diskTrack);
                    }
                    else
                    {
                        // Track should already have been created.
                        diskTrack = SelectedUnit.GetTrack(SelectedUnit.Cylinder, (byte)_headSelect);
                    }

                    // Create a new sector and write it to the track:
                    Sector diskSector = new Sector(_transfer.Cylinder,
                                                   _transfer.Head,
                                                   _transfer.Sector,
                                                   _transfer.SectorLength);
                    diskTrack.AddSector(_transfer.Sector, diskSector);

                    // Move to the next sector
                    _transfer.SectorIndex++;
                    _transfer.TransferIndex = 0;

                    if (_transfer.SectorIndex == _transfer.SectorCount)
                    {
                        // Done, report success:
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
            _scheduler.Schedule(_transfer.TransferIndex == 0 ? _sectorTimeNsec : _byteTimeNsec,
                                SectorFormatCallback);
        }


        /// <summary>
        /// Finish a data transfer: queue up the results bytes and interrupt.
        /// </summary>
        private void FinishTransfer(TransferRequest request)
        {
            // Post result data to the status register queue:
            _statusData.Enqueue((byte)request.ST0);
            _statusData.Enqueue((byte)request.ST1);
            _statusData.Enqueue((byte)request.ST2);
            _statusData.Enqueue((byte)request.Cylinder);
            _statusData.Enqueue((byte)request.Head);
            _statusData.Enqueue((byte)request.Sector);
            _statusData.Enqueue((byte)request.Number);

            _interruptActive = true;
            FinishCommand();

            Log.Debug(Category.FloppyDisk, "Transfer completed");
        }

        /// <summary>
        /// Finish command processing by setting state, status and clearing
        /// the command data queue.
        /// </summary>
        private void FinishCommand()
        {
            // Reset EXM, set RQM and DIO to let the processor know results are ready to read
            _status &= ~Status.EXM;

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
                                           (StatusRegister0.ReadySignalChanged | StatusRegister0.NotReady);
            }
            
            return _errorStatus;
        }

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

            // Cancellation
            public bool Aborted;

            // Status Bits (for transfer completion)
            public StatusRegister0 ST0;
            public StatusRegister1 ST1;
            public StatusRegister2 ST2;
        }

        // From v87.z80:
        // FLPSTA  EQU     250Q         ;FLOPPY STATUS  OLD ADDRESS WAS 320Q
        // FLPDAT  EQU     251Q         ;FLOPPY DATA    OLD ADDRESS WAS 321Q
        private enum Register
        {
            Status = 0xa8,
            Data = 0xa9,
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
        private ulong _sectorTimeNsec = (ulong)((166.667 / 26.0) * Conversion.MsecToNsec);

        // FM time, halve this for MFM time
        private ulong _byteTimeNsec = (ulong)(27.0 * Conversion.UsecToNsec);

        // System interface
        private readonly byte[] _ports = { (byte)Register.Status, (byte)Register.Data };
        private Scheduler _scheduler;

        // I hate this, Konamiman.  I hate this So much.
        public event EventHandler NmiInterruptPulse;
    }
}
