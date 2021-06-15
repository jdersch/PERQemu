using Konamiman.Z80dotNet;
using PERQemu.PhysicalDisk;
using System;
using System.Collections.Generic;

namespace PERQemu.IO.Z80_new
{
    public class NECuPD765A : IZ80Device
    {
        // TODO: Ctor should take base I/O address
        public NECuPD765A(PERQSystem system)
        {
            _system = system;
            _commandData = new Queue<byte>();
            _statusData = new Queue<byte>();

            _commands = new CommandData[]
            {
                new CommandData(Command.ReadData, 0x1f, 8, StubExecutor),
                new CommandData(Command.ReadTrack, 0x9f, 8, StubExecutor),
                new CommandData(Command.ReadDeletedData, 0x1f, 8, StubExecutor),
                new CommandData(Command.ReadId, 0xbf, 8, StubExecutor),
                new CommandData(Command.WriteData, 0x3f, 8, StubExecutor),
                new CommandData(Command.FormatTrack, 0xbf, 5, StubExecutor),
                new CommandData(Command.WriteDeletedData, 0x3f, 8, StubExecutor),
                new CommandData(Command.ScanEqual, 0x1f, 8, StubExecutor),
                new CommandData(Command.ScanLowOrEqual, 0x1f, 8, StubExecutor),
                new CommandData(Command.Recalibrate, 0xff, 1, RecalibrateExecutor),
                new CommandData(Command.SenseInterruptStatus, 0xff, 1, SenseInterruptStatusExecutor),
                new CommandData(Command.Specify, 0xff, 2, StubExecutor),
                new CommandData(Command.SenseDriveStatus, 0xff, 1, StubExecutor),
                new CommandData(Command.ScanHighOrEqual, 0x1f, 8, StubExecutor),
                new CommandData(Command.Seek, 0xff, 2, SeekExecutor),
            };

            Reset();
        }

        public void Reset()
        {
            _interruptsEnabled = false;
            _interruptActive = false;
            _status = Status.RQM;
            _state = State.Command;
            _commandData.Clear();
            _statusData.Clear();
        }

        public string Name => "uPD765A FDC";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => _interruptActive & _interruptsEnabled;

        public byte? ValueOnDataBus => 0x24; // FLPVEC

        public bool InterruptsEnabled
        {
            get => _interruptsEnabled;
            set => _interruptsEnabled = value;
        }

        public byte Read(byte portAddress)
        {
            switch ((Register)portAddress)
            {
                case Register.Status:
                    return (byte)_status;

                case Register.Data:
                    // TODO: is this correct?
                    _interruptActive = false;

                    if (_state == State.Result)
                    {
                        // Read result data
                        if (_statusData.Count > 0)
                        {
                            byte value = _statusData.Dequeue();

                            if (_statusData.Count == 0)
                            {
                                _state = State.Command;
                            }

                            // Clear RQM for the next 12 uSec.
                            _status &= (~Status.RQM);

                            _system.Scheduler.Schedule(12 * Conversion.UsecToNsec, (skew, context) =>
                            {
                                // Set DIO and RQM appropriately to signify readiness for next data
                                _status |= (Status.DIO | Status.RQM);
                            });

                            return value;
                        }
                        else
                        {
                            // No data, just return 0 and ensure we're back in Command state.
                            _state = State.Command;
                            return 0;
                        }
                    }
                    else if (_state == State.Execution)
                    {
                        // Read FDC data if in non-DMA mode, and doing a read
                        throw new NotImplementedException("FDC Data read not implemented yet.");
                    }
                    else
                    {
                        // Unsure exactly what should happen here, return 0.
                        return 0;
                    }
                    break;

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
                    throw new NotImplementedException(String.Format("Unexpected FDC register write to 0x{0}", portAddress));
            }
        }

        private void WriteData(byte value)
        {
            _interruptActive = false;

            // TODO: enforce state (command vs. execution, etc.)
            if (_commandData.Count == 0)
            {
                // Figure out what command this is
                foreach (CommandData data in _commands)
                {
                    if ((value & data.Mask) == (int)data.Command)
                    {
                        _currentCommand = data;
                        
                        break;
                    }
                }

                // Invalid command, handle this properly
            }            

            // Store the command data away, reset bits 6 and 7 of the status register,
            // and queue a workitem to set the bits.
            // If this is the last byte of the command, commence execution.
            _commandData.Enqueue(value);

            _status &= (~Status.DIO & ~Status.RQM);

            if (_commandData.Count - 1 == _currentCommand.ByteCount)
            {
                // Set EXM
                _state = State.Execution;
                _status |= Status.EXM;
                _system.Scheduler.Schedule(12 * Conversion.UsecToNsec, _currentCommand.Executor);
            }
            else
            {
                _system.Scheduler.Schedule(12 * Conversion.UsecToNsec, (skew, context) =>
                {
                    // Set DIO and RQM appropriately to signify readiness for next data
                    _status |= (Status.RQM);
                });
            }
        }

        private void StubExecutor(ulong skewNsec, object context)
        {
            FinishCommand();
        }

        private void RecalibrateExecutor(ulong skewNsec, object context)
        {            
            // TODO: actually do something
            _interruptActive = true;
            FinishCommand();
        }

        private void SeekExecutor(ulong skewNsec, object context)
        {
            // TODO: actually do something
            _interruptActive = true;
            FinishCommand();
        }

        private void SenseInterruptStatusExecutor(ulong skewNsec, object context)
        {
            // Return ST0 and PCN, and clear the interrupt active flag
            _interruptActive = false;

            _statusData.Enqueue(0x20);    // ST0 (SEEK END)
            _statusData.Enqueue(1);       // PCN

            FinishCommand();
        }

        private void FinishCommand()
        {
            // Reset EXM, set RQM and DIO to let the processor know results are ready to read.
            _status &= ~Status.EXM;
            if (_statusData.Count > 0)
            {
                _status |= (Status.DIO | Status.RQM);
            }
            else
            {
                _status &= (~Status.DIO);
                _status |= Status.RQM;
            }
            _state = State.Result;

            _commandData.Clear();
        }

        private PERQSystem _system;

        private bool _interruptsEnabled;
        private bool _interruptActive;
        private Status _status;
        private State _state;
        private CommandData _currentCommand;
        private Queue<byte> _commandData;
        private Queue<byte> _statusData;


        // From v87.z80:
        // FLPSTA  EQU     250Q                    ;FLOPPY STATUS    OLD ADDRESS WAS 320Q
        // FLPDAT  EQU     251Q                    ;FLOPPY DATA   OLD ADDRESS WAS 321Q
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
            D0B = 0x1,  // FDD N Busy
            D1B = 0x2,
            D2B = 0x4,
            D3B = 0x8,
            CB = 0x10,  // Controller Busy (read/write in process, FDC will not accept commands.)
            EXM = 0x20, // Execution phase in non-DMA mode
            DIO = 0x40, // Transfer direction - 1 = to processor, 0 = from processor
            RQM = 0x80, // Request for Master - Data Reg is ready to xfer data
        }

        // TODO: need to specify masks to apply
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
