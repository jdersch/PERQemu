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

            Log.Debug(Category.FloppyDisk, "Controller reset.");
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
            _transferRequest.Aborted = true;
        }

        /// <summary>
        /// Attach a floppy drive to the controller.  Technically the PERQ only
        /// ever supported one drive, as the second unit select line was not
        /// connected.  But it'd be nice to have two...
        /// </summary>
        public void AttachDrive(uint unit, FloppyDisk drive)
        {
            if (unit > _drives.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(unit));
            }

            _drives[unit] = drive;

            Log.Debug(Category.FloppyDisk, "Attached disk '{0}'", drive.Info.Name);
        }

        public void DetachDrive(uint unit)
        {
            if (unit > _drives.Length)
            {
                throw new ArgumentOutOfRangeException(nameof(unit));
            }

            if (_drives[unit] != null)
            {
                Log.Debug(Category.FloppyDisk, "Detached disk '{0}'", _drives[unit].Info.Name);
                _drives[unit] = null;
            }
        }

        public byte Read(byte portAddress)
        {
            switch ((Register)portAddress)
            {
                case Register.Status:
                    Log.Debug(Category.FloppyDisk, "FDC Status read: {0} (0x{1:x}).",
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
                                    // More data to be read.
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
                            throw new InvalidOperationException("FDC Status Read with no data available.");
                        }
                    }
                    else if (_state == State.Execution)
                    {
                        if (!_readDataReady)
                        {
                            throw new InvalidOperationException("DMA read with no data ready.");
                        }

                        _readDataReady = false;
                        data = _readByte;
                    }
                    else
                    {
                        // Unsure exactly what should happen here
                        throw new InvalidOperationException("FDC Status Read in Command phase.");
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
                            Log.Debug(Category.FloppyDisk, "Command is {0}.", _currentCommand.Command);
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
                    throw new InvalidOperationException("DMA write while not ready.");
                }

                _writeByte = value;
                _writeDataReady = false;
            }
            else
            {
                // Unexpected
                throw new InvalidOperationException("Data write while in Result state.");
            }
        }

        private void StubExecutor(ulong skewNsec, object context)
        {
            throw new NotImplementedException("FDC command not implemented.");
        }

        private void FormatExecutor(ulong skewNsec, object context)
        {
            _transferRequest = new TransferRequest();
            _transferRequest.Type = Command.FormatTrack;
            _transferRequest.MFM = (_commandData.Dequeue() & 0x40) != 0;
            SelectUnitHead(_commandData.Dequeue());
            _transferRequest.Number = _commandData.Dequeue();
            _transferRequest.SectorCount = _commandData.Dequeue();
            _commandData.Dequeue(); // GPL, discarded for now (we don't emulate gaps)
            _commandData.Dequeue(); // D, discarded (as above, we don't care about filler data -- TODO: probably we could!)

            _transferRequest.SectorLength = (ushort)(128 << _transferRequest.Number);

            // Format works by doing 4 DMA transfers (for format parameters) per
            // sector on the track.  Indicate that we're ready for a DMA transfer,
            // then kick off the sector format
            _writeDataReady = true;

            _scheduler.Schedule(_sectorTimeNsec, SectorFormatCallback);
        }

        private void ReadDataExecutor(ulong skewNsec, object context)
        {
            if (BeginSectorTransfer())
            {
                _scheduler.Schedule(_sectorTimeNsec, SectorTransferCallback);
            }
        }

        private void SpecifyExecutor(ulong skewNsec, object context)
        {
            //
            // Read the data specified.
            // We currently ignore SRT (Step Rate Time), HUT (Head Unload Time)
            // and HLT (Head Load Time) because I'm not anal retentive.
            //
            _commandData.Dequeue(); // command byte
            _commandData.Dequeue(); // SRT/HUT
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

            if (SelectedDriveIsReady)
            {
                // Set "Busy" bit for this drive:
                _status |= (Status)(0x1 << _unitSelect);
                _pcn[_unitSelect] = cylinder;

                SelectedUnit.SeekTo(cylinder, SeekCompleteCallback);

                Log.Debug(Category.FloppyDisk, "Unit {0} seek to {1} scheduled", _unitSelect, cylinder);
            }
            else
            {
                // Trying to seek on a drive that's not ready (or not connected?)
                StatusRegister0 ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                Log.Error(Category.FloppyDisk, "{0} issued but drive {1} not ready", cmd, _unitSelect);

                FinishCommand();
            }
        }

        private void SeekCompleteCallback(ulong skewNsec, object context)
        {
            // Stimpy!  We made it!
            _seekEnd = true;

            // Clear drive "Busy" bit:
            _status &= ~((Status)(0x1 << _unitSelect));
            _interruptActive = true;

            Log.Debug(Category.FloppyDisk, "Unit {0} seek to cyl {1} completed ({2}ns)",
                                            _unitSelect, _pcn[_unitSelect], skewNsec);

            // Return to Command state
            FinishCommand();
        }

        private void SenseInterruptStatusExecutor(ulong skewNsec, object context)
        {
            _commandData.Dequeue();     // toss command byte

            // Return ST0 and PCN, and clear the interrupt active flag
            _interruptActive = false;

            StatusRegister0 ST0 = SetErrorStatus(StatusRegister0.None);

            _statusData.Enqueue((byte)ST0);             // ST0 (SEEK END)
            _statusData.Enqueue(_pcn[_unitSelect]);     // PCN

            // "Neither the Seek or Recalibrate Command have a Result Phase. Therefore
            // it is mandatory to use the Sense Interrupt Status Command after these
            // commands to effectively terminate them and to provide verification of
            // where the head is positioned (PCN).  Issuing the Sense Interrupt Command
            // without interrupt pending is treated as an invalid command."

            // Uh, so that means we should clear it _here_ then?
            _seekEnd = false;

            FinishCommand();
        }

        private void SenseDriveStatusExecutor(ulong skewNsec, object context)
        {
            _commandData.Dequeue();                     // toss command byte
            SelectUnitHead(_commandData.Dequeue());     // get drive to query

            var st3 = (StatusRegister3)_unitSelect;

            if (SelectedUnit != null)
            {
                // Build up ST3 result bits from the selected drive
                st3 |= (SelectedUnit.HeadSelect > 0 ? StatusRegister3.Head : StatusRegister3.None) |
                       (SelectedUnit.IsSingleSided ? StatusRegister3.None : StatusRegister3.TwoSided) |
                       (SelectedUnit.Track0 ? StatusRegister3.Track0 : StatusRegister3.None) |
                       (SelectedUnit.Ready ? StatusRegister3.Ready : StatusRegister3.None) |
                       (SelectedUnit.Info.IsWritable ? StatusRegister3.None : StatusRegister3.WriteProtected) |
                       (SelectedUnit.Fault ? StatusRegister3.Fault : StatusRegister3.None);
            }
            // otherwise... what to return if no drive attached?

            _statusData.Enqueue((byte)st3);

            FinishCommand();
        }

        private void WriteDataExecutor(ulong skewNsec, object context)
        {
            if (BeginSectorTransfer())
            {
                _scheduler.Schedule(_sectorTimeNsec, SectorTransferCallback);
            }
        }

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

        private bool BeginSectorTransfer()
        {
            _transferRequest = new TransferRequest();
            _transferRequest.Type = _currentCommand.Command;

            // Pull data from command regs:
            byte commandByte = _commandData.Dequeue();
            _transferRequest.MultiTrack = (commandByte & 0x80) != 0;
            _transferRequest.MFM = (commandByte & 0x40) != 0;
            _transferRequest.SkipDeletedAM = (commandByte & 0x20) != 0;
            SelectUnitHead(_commandData.Dequeue());
            _transferRequest.Cylinder = _commandData.Dequeue();
            _transferRequest.Head = _commandData.Dequeue();
            _transferRequest.Sector = _commandData.Dequeue();
            _transferRequest.Number = _commandData.Dequeue();
            _transferRequest.EndOfTrack = _commandData.Dequeue();
            byte gpl = _commandData.Dequeue();
            byte dtl = _commandData.Dequeue();

            _transferRequest.Aborted = false;

            if (_transferRequest.Number == 0)
            {
                _transferRequest.SectorLength = dtl;
            }
            else
            {
                _transferRequest.SectorLength = (ushort)(128 << _transferRequest.Number);
            }

            Log.Debug(Category.FloppyDisk,
                "{0} from unit {1}, C/H/S {2}/{3}/{4} to sector {5} of length {6}.",
                _transferRequest.Type == Command.ReadData ? "Read" : "Write",
                _unitSelect,
                _transferRequest.Cylinder,
                _transferRequest.Head,
                _transferRequest.Sector,
                _transferRequest.EndOfTrack,
                _transferRequest.SectorLength);

            if (!SelectedDriveIsReady)
            {
                // Post drive not ready (TODO: this isn't correct, apparently?
                // FLOPPY hates this so much it hangs forever.)
                _transferRequest.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                _transferRequest.ST1 = StatusRegister1.None;
                _transferRequest.ST2 = StatusRegister2.None;

                // Post the failure:
                FinishTransfer(_transferRequest);
                return false;
            }

            return true;
        }

        private void SectorFormatCallback(ulong skewNsec, object context)
        {
            // If the last byte was not written, this is an overrun
            if (_writeDataReady)
            {
                _transferRequest.ST0 |= StatusRegister0.AbnormalTermination;
                _transferRequest.ST1 = StatusRegister1.OverRun;
                FinishTransfer(_transferRequest);
                return;
            }

            // We need four bytes from the processor before we can format this
            // sector; collect them up:
            switch (_transferRequest.TransferIndex)
            {
                case 0:
                    _transferRequest.Cylinder = _writeByte;
                    _transferRequest.TransferIndex++;
                    break;

                case 1:
                    _transferRequest.Head = _writeByte;
                    _transferRequest.TransferIndex++;
                    break;

                case 2:
                    _transferRequest.Sector = _writeByte;
                    _transferRequest.TransferIndex++;
                    break;

                case 3:
                    _transferRequest.Number = _writeByte;

                    // Format the sector now:
                    Track diskTrack;
                    if (_transferRequest.SectorIndex == 0)
                    {
                        // First sector in the track to be formatted: Create a new, empty track,
                        // which will be filled during the rest of this operation.  (Note: we use
                        // the passed-in cylinder/head information for the track itself; it gets
                        // inserted into the disk image on the cylinder/head the drive is currently
                        // pointing to).  It is technically possible for the processor to change
                        // cyl/head designations in the middle of the format; IMD does not support
                        // this possibility and I don't expect the PERQ to try it.
                        diskTrack = new Track(_transferRequest.Cylinder,
                                              _transferRequest.Head,
                                              _transferRequest.SectorCount,
                                              _transferRequest.SectorLength);
                        SelectedUnit.SetTrack(_transferRequest.Cylinder, (byte)_headSelect, diskTrack);
                    }
                    else
                    {
                        // Track should already have been created.
                        diskTrack = SelectedUnit.GetTrack(SelectedUnit.Cylinder, (byte)_headSelect);
                    }

                    // Create a new sector and write it to the track:
                    Sector diskSector = new Sector(_transferRequest.Cylinder,
                                                   _transferRequest.Head,
                                                   _transferRequest.Sector,
                                                   _transferRequest.SectorLength);
                    diskTrack.AddSector(_transferRequest.Sector, diskSector);

                    // Move to the next sector
                    _transferRequest.SectorIndex++;
                    _transferRequest.TransferIndex = 0;

                    if (_transferRequest.SectorIndex == _transferRequest.SectorCount)
                    {
                        // Done, report success:
                        _transferRequest.ST1 = StatusRegister1.None;
                        _transferRequest.ST2 = StatusRegister2.None;
                        FinishTransfer(_transferRequest);
                        return;
                    }
                    break;

                default:
                    throw new InvalidOperationException("Unexpected state during Format operation.");
            }

            // Do it again, do it again
            _writeDataReady = true;
            _scheduler.Schedule(_transferRequest.TransferIndex == 0 ? _sectorTimeNsec : _byteTimeNsec, SectorFormatCallback);
        }

        private void SectorTransferCallback(ulong skewNsec, object context)
        {
            // Simulate the initial search for the sector, then kick off the sector transfer:
            Log.Debug(Category.FloppyDisk, "In transfer callback");

            // Set preliminary status info
            _transferRequest.ST0 = SetErrorStatus(StatusRegister0.None);

            // If Request was aborted by the DMA controller, stop here
            if (_transferRequest.Aborted)
            {
                // This is considered a success
                _transferRequest.ST1 = StatusRegister1.None;
                _transferRequest.ST2 = StatusRegister2.None;
                FinishTransfer(_transferRequest);
                return;
            }

            // Validate the operation:  if the cylinder is out of range or the
            // sector does not exist on disk, flag the appropriate error and exit
            if (_transferRequest.Cylinder > SelectedUnit.Geometry.Cylinders ||
                _transferRequest.Cylinder != SelectedUnit.Cylinder)
            {
                _transferRequest.ST0 |= StatusRegister0.AbnormalTermination;
                _transferRequest.ST2 = StatusRegister2.WrongCylinder;
                FinishTransfer(_transferRequest);
                return;
            }

            //_transferRequest.SectorData.Data = null;

            // Validate the head vs. the number of sides on the disk:
            if (SelectedUnit.IsSingleSided && _transferRequest.Head != 0)
            {
                _transferRequest.ST0 |= StatusRegister0.AbnormalTermination;
                _transferRequest.ST1 = StatusRegister1.NoData;      // TODO: is this correct?  What does this really report for a single-sided disk?
                FinishTransfer(_transferRequest);
                return;
            }

            //Track diskTrack = SelectedUnit.GetTrack();

            // Grab the sector:
            _transferRequest.SectorData = SelectedUnit.GetSector(_transferRequest.Cylinder,
                                                                 _transferRequest.Head,
                                                                 _transferRequest.Sector);

            if (_transferRequest.SectorData.Data.Length == 0)
            {
                // No sector data available:
                _transferRequest.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                _transferRequest.ST1 = StatusRegister1.NoData;      // Sector not found
                FinishTransfer(_transferRequest);
                return;
            }

            // Check the sector format; we expect either FM500 or MFM500, matching the state of the MFM flag
            if ((_transferRequest.MFM && _transferRequest.SectorLength != 256) ||
                (!_transferRequest.MFM && _transferRequest.SectorLength != 128))
            {
                _transferRequest.ST0 = SetErrorStatus(StatusRegister0.AbnormalTermination);
                _transferRequest.ST1 = StatusRegister1.NoData;      // Sector not found (TODO: is this correct?)
                FinishTransfer(_transferRequest);
                return;
            }

            // OK, we actually have sector data now!  Queue up the first callback:
            if (_transferRequest.Type == Command.ReadData)
            {
                _scheduler.Schedule(_byteTimeNsec, SectorByteReadCallback);
            }
            else if (_transferRequest.Type == Command.WriteData)
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
            if (_transferRequest.TransferIndex == _transferRequest.SectorLength)
            {
                // Done
                FinishSectorTransfer();
                return;
            }

            // If the last byte was not read by now, this is an overrun
            if (_readDataReady)
            {
                _transferRequest.ST0 |= StatusRegister0.AbnormalTermination;
                _transferRequest.ST1 = StatusRegister1.OverRun;
                FinishTransfer(_transferRequest);
                return;
            }

            _readByte = _transferRequest.SectorData.Data[_transferRequest.TransferIndex++];
            _readDataReady = true;

            _scheduler.Schedule(_byteTimeNsec, SectorByteReadCallback);
        }

        private void SectorByteWriteCallback(ulong skewNsec, object context)
        {
            // If the last byte was not written, this is an overrun
            if (_writeDataReady)
            {
                _transferRequest.ST0 |= StatusRegister0.AbnormalTermination;
                _transferRequest.ST1 = StatusRegister1.OverRun;
                FinishTransfer(_transferRequest);
                return;
            }

            Log.Debug(Category.FloppyDisk, "Wrote byte 0x{0} to index {1}",
                                          _writeByte, _transferRequest.TransferIndex);

            // Write the next byte:
            _transferRequest.SectorData.Data[_transferRequest.TransferIndex++] = _writeByte;

            if (_transferRequest.TransferIndex == _transferRequest.SectorLength)
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
            Log.Debug(Category.FloppyDisk, "Sector {0} transfer complete.", _transferRequest.Sector);

            // Always increment the sector counter
            _transferRequest.Sector++;

            // Are there any sectors left to transfer?
            if (_transferRequest.Sector > _transferRequest.EndOfTrack)
            {
                // Nope, done, report success:
                _transferRequest.ST1 = StatusRegister1.None;
                _transferRequest.ST2 = StatusRegister2.None;
                FinishTransfer(_transferRequest);
            }
            else
            {
                _scheduler.Schedule(_sectorTimeNsec, SectorTransferCallback);

                Log.Debug(Category.FloppyDisk, "Next sector transfer queued.");
            }
        }

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

            Log.Debug(Category.FloppyDisk, "Transfer completed.");
        }

        private void SelectUnitHead(byte select)
        {
            _unitSelect = select & 0x3;
            _headSelect = (select & 0x4) >> 2;
        }

        /// <summary>
        /// Fill in the common fields of Status Register 0.  This is done
        /// often enough to do it consistently.  If a non-existent drive is
        /// selected, returns NotReady and error status ReadySignalChanged.
        /// </summary>
        private StatusRegister0 SetErrorStatus(StatusRegister0 error)
        {
            var st0 = error |
                    (StatusRegister0)_unitSelect |
                    (_seekEnd ? StatusRegister0.SeekEnd : StatusRegister0.None);

            if (SelectedUnit == null)
            {
                // There ain't no drive there!  How will the PERQ react?
                st0 |= StatusRegister0.NotReady | StatusRegister0.ReadySignalChanged;
            }
            else
            {
                // Give back actual drive status
                st0 |= (SelectedUnit.HeadSelect == 0 ? StatusRegister0.None : StatusRegister0.Head) |
                       (SelectedUnit.Fault ? StatusRegister0.EquipChk : StatusRegister0.None) |
                       (SelectedUnit.Ready ? StatusRegister0.None : StatusRegister0.NotReady);
            }

            Log.Debug(Category.FloppyDisk, "ST0 = {0}", st0);
            return st0;
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

        private FloppyDisk[] _drives;
        private FloppyDisk SelectedUnit => _drives[_unitSelect];

        private bool SelectedDriveIsReady => SelectedUnit != null && SelectedUnit.Ready;

        private byte[] _pcn;

        private int _unitSelect;
        private int _headSelect;

        // ST0 bits (used by SenseInterruptStatus)
        private bool _seekEnd;
        private bool _interruptsEnabled;
        private bool _interruptActive;
        private bool _nonDMAMode;

        private Status _status;
        private State _state;

        private CommandData _currentCommand;
        private Queue<byte> _commandData;
        private Queue<byte> _statusData;

        private Scheduler _scheduler;

        // Timing:
        // Sector time: 360 RPM, 26 sectors per rotation.  This is just for approximate timing.
        private ulong _sectorTimeNsec = (ulong)((16.6667 / 26.0) * Conversion.MsecToNsec);
        private ulong _byteTimeNsec = (ulong)(27.0 * Conversion.UsecToNsec);    // FM time, halve this for MFM time.

        // The current transfer in progress:
        private TransferRequest _transferRequest;

        private byte _readByte;
        private bool _readDataReady;

        private byte _writeByte;
        private bool _writeDataReady;


        // From v87.z80:
        // FLPSTA  EQU     250Q         ;FLOPPY STATUS  OLD ADDRESS WAS 320Q
        // FLPDAT  EQU     251Q         ;FLOPPY DATA    OLD ADDRESS WAS 321Q
        private enum Register
        {
            Status = 0xa8,
            Data = 0xa9,
        }

        private readonly byte[] _ports = { (byte)Register.Status, (byte)Register.Data };

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
            MissAddr = 0x1,
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
            DataMissAddr = 0x1,
            BadCylinder = 0x2,
            ScanNotSatisfied = 0x4,
            ScanEqualHit = 0x8,
            WrongCylinder = 0x10,
            DataDataError = 0x20,
            DataErrorInDataField = 0x40,
            ControlMark = 0x80
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

        private CommandData[] _commands;

        public event EventHandler NmiInterruptPulse;
    }
}
