﻿//
// Sidewinder.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Diagnostics;
using System.Collections.Generic;

using PERQmedia;

namespace PERQemu.IO.TapeDevices
{
    /// <summary>
    /// Emulates the intelligent controller attached to the Archive Sidewinder
    /// QIC tape drive.  This handles all of the command and control functions
    /// and acts as intermediary between the host PERQ and the mechanical half
    /// (and underlying storage).
    /// </summary>
    /// <remarks>
    /// The baseline is a generic QIC-02 interfaced 4-track quarter-inch tape
    /// drive compatible with the Archive 3020I, but different media types can
    /// emulate the other Sidewinder models (9020I and 9045I) which are faster 
    /// and offer higher capacities!
    /// </remarks>
    public sealed class Sidewinder
    {
        public Sidewinder(Scheduler sched)
        {
            _scheduler = sched;
            _protocolEvent = null;
            _commandSequence = new Stack<SchedulerEventCallback>();

            _drive = new CartridgeTape(this, _scheduler, string.Empty);
            _status = new DriveStatus();

            InitBuffers();

            Reset();
        }

        public CartridgeTape Media => _drive;

        // Status lines sent to the PERQ
        public bool Ready => _ready;
        public bool Acknowledge => _acknowledge;
        public bool Exception => _exception;

        // Control lines from the PERQ
        public bool Online
        {
            get { return _online; }
            set { _online = value; }
        }

        public bool Request
        {
            get { return _request; }
            set { _request = value; }
        }

        public bool Xfer
        {
            get { return _xfer; }
            set { _xfer = value; }
        }

        // The bi-directional 8-bit "bus" for data/command transfers
        public byte Data
        {
            get { return _data; }
            set { _data = value; }
        }

        // Where we at?
        bool AtBeginningOfTape => _drive.AtBOT;
        bool AtEndOfTape => _drive.AtEOT;
        bool AtEndOfMedia => _drive.AtEOM;
        bool AtLogicalEOM => _drive.AtLogicalEOM;
        bool AtFileMark => _drive.AtFileMark;

        // Time (in ns, for scheduling) for the drive to read/write one sector
        public ulong OneSectorDelay => _drive.SeekTime;

        /// <summary>
        /// Reset the drive and controller.
        /// </summary>
        public void Reset()
        {
            if (_protocolEvent != null)
            {
                _scheduler.Cancel(_protocolEvent);
                _protocolEvent = null;
            }

            ResetBuffers();

            _state = State.Reset;
            _phase = Phase.Ready;
            _command = Command.None;
            _commandSequence.Clear();

            // Set our outgoing signals according to the spec
            _ready = false;
            _acknowledge = true;
            _exception = true;

            _atPosition = false;

            Log.Info(Category.Streamer, "Controller reset");
        }

        #region Buffer management

        // The hardware has 3, but we can be more flexible
        public const int MAXBUFFERS = 5;

        public bool WritesPending => _buffers.Count > 0;
        public bool BufferAvailable => _buffers.Count <= MAXBUFFERS;

        void InitBuffers()
        {
            // Zero them out, reset the pointer(s)
            _buffers = new Queue<BlockBuffer>();
            _hostBuffer = new BlockBuffer(BlockType.Empty);

            Log.Info(Category.Streamer, "Buffers initialized");
        }

        void ResetBuffers()
        {
            _buffers.Clear();
            _hostBuffer.Clear();
        }

        public BlockBuffer GetBuffer()
        {
            return _buffers.Dequeue();
        }

        public void PutBuffer(BlockBuffer buf)
        {
            _buffers.Enqueue(buf);
        }

        #endregion

        #region Command byte protocol

        /// <summary>
        /// Check the current state of the signal lines from the host.  This can
        /// initiate a new command, terminate execution of a command in progress
        /// or indicate that a data byte has been transmitted or received.
        /// </summary>
        public void CheckSignals()
        {
            //ENTER("ChkSig");

            var nextState = _state;

            if (GetCommandByte())
            {
                switch (_state)
                {
                    case State.Unloaded:
                    case State.Fault:
                    case State.Reset:
                        // Only valid command is Read Status
                        if (_command == Command.ReadStatus)
                        {
                            nextState = DoReadStatus();
                        }
                        else
                        {
                            nextState = FinishCommand();
                        }
                        break;

                    case State.Idle:

                        // Fire at will, commander!
                        switch (_command)
                        {
                            case Command.Select:
                                nextState = DoSelect();
                                break;

                            case Command.Rewind:
                            case Command.Erase:
                            case Command.Retension:
                                nextState = DoPosition();
                                break;

                            case Command.ReadStatus:
                                nextState = DoReadStatus();
                                break;

                            case Command.WriteData:
                            case Command.WriteFileMark:
                                nextState = StartWriting();
                                break;

                            case Command.ReadData:
                            case Command.ReadFileMark:
                                nextState = StartReading();
                                break;

                            default:
                                Log.Warn(Category.Streamer, "Unimplemented command {0}", _command);
                                break;
                        }
                        break;

                    case State.Busy:

                        switch (_command)
                        {
                            case Command.ReadStatus:
                                nextState = DoReadStatus();
                                break;

                            case Command.WriteData:
                            case Command.WriteFileMark:
                                nextState = ContinueWriting();
                                break;

                            case Command.ReadData:
                            case Command.ReadFileMark:
                                nextState = ContinueReading();
                                break;

                            default:
                                Log.Warn(Category.Streamer, "Unhandled signal change for {0} in Busy state", _command);
                                break;
                        }
                        break;

                    default:
                        throw new InvalidOperationException($"Unexpected state {_state} in CheckSignals");
                }
            }

            _state = nextState;
            //EXIT("ChkSig");
        }

        /// <summary>
        /// Check the status of incoming signal lines and perform the next step
        /// in the Request/Ready handshaking sequence to start a new command.
        /// Once accepted, return true until the command being executed completes
        /// and resets the state machine.
        /// </summary>
        private bool GetCommandByte()
        {
            if (_phase >= Phase.Accepted) return true;

            //ENTER("CmdByte");

            switch (_phase)
            {
                case Phase.Ready:
                    if (_request)
                    {
                        // Save the command byte from the bus latch
                        _command = (Command)_data;
                        Log.Write(Category.Streamer, "--> Read command byte: {0}", _command);   // debug

                        // If the request is for a read/write command, ONLINE
                        // has to be true; if not, that's an exception
                        if ((_command == Command.ReadData ||
                             _command == Command.ReadFileMark ||
                             _command == Command.WriteData ||
                             _command == Command.WriteFileMark) &&
                            !_online)
                        {
                            _illegal = true;
                            _exception = true;
                            _state = State.Fault;
                            Log.Warn(Category.Streamer, "Fault: {0} request while ONLINE false", _command);
                            return false;
                        }

                        // If the request is a write or erase and the cartridge
                        // is write protected, that's an exception too!
                        if ((_command == Command.Erase ||
                             _command == Command.WriteData ||
                             _command == Command.WriteFileMark) &&
                            !_drive.Info.IsWritable)
                        {
                            _exception = true;
                            _state = State.Fault;
                            Log.Info(Category.Streamer, "Fault: {0} request while write-protected", _command);
                            return false;
                        }

                        // Advance the state and pulse the ready line
                        _phase = Phase.Request;

                        // Set the delay so the microcode can catch the transition
                        var delay = (_command == Command.Select) ? 50 : 20;

                        // Things are funky after a reset, an exception, or reading
                        // a file mark: exception is true and ready is backwards!
                        if (_command == Command.ReadStatus)
                        {
                            _exception = false;     // Should be a 10usec delay
                            _ready = true;          // between these two transitions
                        }
                        else
                        {
                            _ready = false;
                        }

                        // Schedule the transition
                        _protocolEvent = _scheduler.Schedule((ulong)delay * Conversion.UsecToNsec, (skewNsec, context) =>
                        {
                            _ready = !_ready;       // Toggle it (ReadStatus is wackbirds)
                            _phase = Phase.Response;
                        });
                    }
                    break;

                case Phase.Response:
                    if (!_request)
                    {
                        // Request line dropped, so now we have the ball
                        _phase = Phase.RequestAck;

                        // Minimum turnaround of 20usec before completing the handshake
                        _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, (skewNsec, context) =>
                        {
                            _ready = !_ready;           // Toggle it again!
                            _phase = Phase.Accepted;    // Final stage of grief
                            CheckSignals();             // Cheeky!
                        });
                    }
                    break;

                case Phase.Request:
                case Phase.RequestAck:
                    // Shouldn't get anything from the host during these phases!
                    Log.Warn(Category.Streamer, "Unexpected status change in {0} phase", _phase);
                    break;

                default:
                    Log.Warn(Category.Streamer, "Bad transition in handshake phase {0}", _phase);
                    // So, uh, just reset and try again!?
                    _ready = false;
                    _exception = true;
                    _state = State.Fault;
                    _phase = Phase.Ready;
                    break;
            }

            //EXIT("CmdByte");
            return _phase >= Phase.Accepted;
        }

        #endregion

        #region Select, Status and Positioning

        State DoSelect()
        {
            // The PERQ microcode doesn't appear to ever send the Select command
            // and must assume that drive 0 is always selected by default?  Not
            // going to bother doing anything special here unless/until I see
            // that it's actually used.

            Log.Debug(Category.Streamer, "Select: Drive {0}", ((uint)_command & 0x1f));

            return FinishCommand();
        }


        /// <summary>
        /// Handle the special handshaking protocol of the ReadStatus command.
        /// Sends back a 6-byte status buffer, then turns the bus around and
        /// returns to Idle.  Clears any fault or exception flags and resets
        /// the status buffer.  May be re-entered by CheckSignals().
        /// </summary>
        State DoReadStatus()
        {
            //ENTER("RdStat");

            // If coming out of Reset drop the ACK line and snapshot status
            if (_state == State.Reset)
            {
                _acknowledge = false;
                UpdateStatus();
            }

            switch (_phase)
            {
                case Phase.Accepted:
                    // Set up for transfer
                    _phase = Phase.StatusReady;

                    // Schedule the next transition in 20usec
                    _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SendStatusByte);

                    return State.Busy;

                case Phase.StatusReady:
                case Phase.StatusAccepted:
                    // Shouldn't get any response from the microcode here?
                    Log.Debug(Category.Streamer, "ReadStatus: unexpected response in phase {0}", _phase);
                    return State.Busy;

                case Phase.StatusSent:
                    // They've taken the byte so we just drop ready.  Not worth
                    // scheduling a 250ns delay here!
                    _ready = false;
                    _phase = Phase.StatusAck;
                    return State.Busy;

                case Phase.StatusAck:
                    // Complete the handshake for the byte sent
                    _phase = Phase.StatusAccepted;

                    // Any more to send?
                    if (!_status.ReadComplete)
                    {
                        // Yep, schedule the next byte
                        _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SendStatusByte);
                        return State.Busy;
                    }

                    // Nope!  Reset and retire
                    _status.Clear();
                    _phase = Phase.Ready;
                    _state = State.Idle;

                    // Schedule our final transition
                    _protocolEvent = _scheduler.Schedule(10 * Conversion.UsecToNsec, SetReady);

                    // Fall through to finish the command
                    break;

                default:
                    Log.Error(Category.Streamer, "ReadStatus: impossible? Phase={0}", _phase);
                    // Shouldn't ever happen; fall out to clean up if it does...
                    break;
            }

            Log.Debug(Category.Streamer, "ReadStatus: Complete");
            return RunSequence();
        }

        void SendStatusByte(ulong skewNsec, object context)
        {
            // Put the next byte on the bus
            _data = _status.GetNextByte();
            _ready = true;
            _phase = Phase.StatusSent;
        }

        /// <summary>
        /// Implements the Position operations:  Erase, Rewind or Retension ops
        /// as encoded in the command byte.
        /// </summary>
        State DoPosition()
        {
            _commandSequence.Clear();

            // All three finish at BOT
            _commandSequence.Push(RewindComplete);

            if (_command == Command.Erase)
            {
                _commandSequence.Push(_drive.Rewind);
                _commandSequence.Push(_drive.Erase);
            }
            else if (_command == Command.Retension)
            {
                _commandSequence.Push(_drive.Rewind);
                _commandSequence.Push(_drive.Retension);
            }
            else if (_command != Command.Rewind)
            {
                throw new InvalidOperationException($"Invalid position command {_command}");
            }

            // All three start by rewinding, if necessary
            if (!AtBeginningOfTape)
            {
                _commandSequence.Push(_drive.Rewind);
            }

            return RunSequence();
        }

        /// <summary>
        /// Update status and raise Ready at the completion of a Position command.
        /// </summary>
        public void RewindComplete(ulong skewNsec, object context)
        {
            // In case we didn't move, delay ready to give the microcode
            // a chance to catch the falling AND rising edges!
            _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SetReady);

            _state = RunSequence();
        }

        #endregion

        #region Write commands

        /// <summary>
        /// Start the absurdly complex process of writing to the tape.  The initial
        /// command can be a WriteData or a WriteFileMark, followed by a sequence
        /// of continuous writes until the host is finished, an error occurs, or
        /// the end of the media is reached.  
        /// </summary>
        State StartWriting()
        {
            //ENTER("StartWr");

            //
            // Initialize a new Write sequence.  GetCommandByte checks that
            // Online is true, a tape is loaded and is not write protected
            //
            if (_command == Command.WriteFileMark)
                _commandSequence.Push(WriteFileMark);
            else
                _commandSequence.Push(WriteData);

            // If the host didn't do this explicitly before calling Write, the
            // drive automatically selects unit 0 and rewinds
            if (!AtBeginningOfTape)
            {
                Log.Info(Category.Streamer, "Tape needs to Rewind at start of new sequence");
                _commandSequence.Push(_drive.Rewind);
            }

            ResetBuffers();

            _atPosition = false;

            // Off we go...
            return RunSequence();
        }

        /// <summary>
        /// Handle continuations of the write sequence: in the Busy state, this
        /// means we're processing data bytes using the Xfer/Ack handshaking, OR
        /// we've fallen back to process a command change between blocks.
        /// </summary>
        State ContinueWriting()
        {
            //ENTER("ContWr");

            var nextState = _state;

            switch (_phase)
            {
                case Phase.Accepted:
                    //
                    // So if we've come back here, a new WriteData or WriteFileMark
                    // command has been issued; push that onto the queue and run it?
                    // That gets us back to WriteData and the Xfer/Ack dance again...
                    //
                    if (_command == Command.WriteFileMark)
                        _commandSequence.Push(WriteFileMark);
                    else
                        _commandSequence.Push(WriteData);

                    nextState = RunSequence();
                    break;

                //
                // Otherwise, run the Xfer/Ack state machine
                //
                case Phase.DataPaused:
                    //
                    // Here when we need to allocate a new buffer to accept the
                    // next block, either at T8-T9 or T22-T23.  At this point the
                    // host can decide to end or modify the sequence!
                    //
                    if (!_online)
                    {
                        Log.Debug(Category.Streamer, "Online dropped, terminating write sequence");

                        // Put us back in command mode
                        _phase = Phase.Ready;
                        _state = State.Idle;

                        // If Online drops at this point, the drive automatically
                        // writes a file mark and rewinds.  The PERQ microcode may
                        // instead explicitly issue these commands rather than rely
                        // on the controller's default behavior, so in that case
                        // this will generally only occur if the controller times
                        // out and drops online (or Resets) to recover?
                        _commandSequence.Push(RewindComplete);
                        _commandSequence.Push(_drive.Rewind);

                        // "If the last command was a WFM command, the controller
                        // will not write another file mark."  Hmm.
                        //if (_command != Command.WriteFileMark)
                        //    _commandSequence.Push(WriteFileMark);

                        nextState = RunSequence();
                        break;
                    }

                    if (_request)
                    {
                        Log.Debug(Category.Streamer, "Request raised, jumping out to receive new command!");

                        // Host wants to issue a new command.  At this point the
                        // byte is already on the bus, so we reset to the command
                        // phase and exit through GetCommandByte.  This lets the
                        // handshake complete without popping the sequence queue.
                        // If a Write or WriteFileMark is issued we'll come back
                        // here to push the new command back on the queue and
                        // continue where we left off...

                        _phase = Phase.Ready;
                        UpdateStatus();
                        CheckSignals();
                        break;
                    }

                    if (_xfer)
                    {
                        // If all the buffers are currently busy, pause until the
                        // mechanism catches up and frees one.  The minimum gap is
                        // 100usec (T22-T23 time shown in the spec), but I don't
                        // think that'll be an issue...  Note that only Write Data
                        // commands use the Xfer/Ack sequence; Write File Mark is
                        // synthesized by the controller and is handled elsewhere.

                        if (_buffers.Count >= MAXBUFFERS)
                        {
                            WaitForBuffer();
                        }
                        else
                        {
                            // Okay to proceed
                            Log.Info(Category.Streamer, "Resetting the host buffer for writing");

                            // Set up the buffer for the incoming block
                            _hostBuffer = new BlockBuffer(BlockType.Data);

                            // The first byte is already on the bus, so jump
                            // directly back into the data transfer loop
                            _phase = Phase.DataReady;

                            // Sigh.  Can't just fall through the case here, so... 
                            ContinueWriting();
                        }
                    }
                    break;

                case Phase.DataReady:
                    //
                    // Start or continue a sector transfer; the inter-sector gap
                    // is covered by DataPaused...
                    //
                    if (_xfer)
                    {
                        // Receiving a data block:  capture the current data byte
                        // from the bus; fire the Ack in .56 - 4.47 usec (for now,
                        // round to 2usec and see how that performs, may tune it)
                        _hostBuffer.PutByte(_data);

                        _phase = Phase.DataAck;
                        _ready = false;

                        _protocolEvent = _scheduler.Schedule(2 * Conversion.UsecToNsec, (skewNsec, context) =>
                         {
                             _acknowledge = true;
                             _phase = Phase.DataAccepted;
                         });
                    }
                    else
                    {
                        Log.Warn(Category.Streamer, "Caught a stray signal change in DataReady");
                    }
                    break;

                case Phase.DataAccepted:
                    //
                    // T14-T16 - Byte acknowledged
                    //
                    // If the current block is full (512 bytes recieved) then we
                    // call Flush to make sure the mechanism is running (started
                    // after the first buffer fills, or restarted after underrun).

                    if (_hostBuffer.Full)
                    {
                        Log.Info(Category.Streamer, "Host buffer is full!");

                        // Commit this one to the queue
                        _buffers.Enqueue(_hostBuffer);
                        _drive.Flush();

                        // Will need a new buffer
                        _phase = Phase.DataPaused;

                        // Reset Ack to indicate normal reception of the last byte;
                        // There's no response from the host after byte 512, but
                        // the transition to Ready will move us into the next phase.
                        // Spec says T22-T23 is >100usec!  That seems overly generous...
                        _acknowledge = false;
                        _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SetReady);
                    }
                    else
                    {
                        // Continue filling the current one
                        _phase = Phase.DataReady;

                        // Reset Ack (T14-T16 > .56us, < 1.12us); Xfer will reinitiate
                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, (skewNsec, context) =>
                        {
                            _acknowledge = false;
                        });
                    }
                    break;

                case Phase.DataSent:
                    // Here because the TSIO microcode is wrong: it doesn't wait
                    // properly to issue the ReadStatus command, giving only a
                    // short fixed delay and then proceeding without waiting for
                    // Ready to go true.  This workaround catches that case. :-/
                    if (_request)
                    {
                        Log.Warn(Category.Streamer, "** Microcode bug detected: Illegal transition (RQST when !RDY)");

                        // Set ready here to complete command exchange while the
                        // WriteFileMark command previously issued is completed
                        _ready = true;
                    }

                    if (!_request && (Command)_data == Command.ReadStatus)
                    {
                        // Reset now, so WriteFM can jump into SendStatusByte
                        // and kick off the status read normally.  Ugh.
                        _ready = false;
                        _command = (Command)_data;

                        Log.Warn(Category.Streamer, "** Microcode bug workaround: Latched {0} command", _command);
                    }
                    break;

                default:
                    Console.WriteLine($"Continue in wut now?  phase={_phase} state={_state}");
                    break;
            }

            //EXIT("ContWr");
            return nextState;
        }


        void WaitForBuffer(bool reading = false)
        {
            Log.Write(Category.Streamer, "Waiting for {0} buffer; retry in {1}ms...",
                                         reading ? "full" : "empty",
                                         OneSectorDelay * Conversion.NsecToMsec);
            if (reading)
            {
                _drive.Fetch();         // Start the ReadAhead
                _protocolEvent = _scheduler.Schedule(OneSectorDelay, ReadData);
            }
            else
            {
                _drive.Flush();         // Kick the WriteBehind
                _protocolEvent = _scheduler.Schedule(OneSectorDelay, (skewNsec, context) =>
                {
                    CheckSignals();     // FIXME
                });
            }
        }


        void WriteData(ulong skewNsec, object context)
        {
            //ENTER("WrData");

            if (_phase == Phase.Accepted)
            {
                // 
                // T8-T9: Starting or continuing a write.  Raise the Ready line
                // and jump to the "paused" state to allocate a new write buffer.
                // 
                _ready = false;
                _state = State.Busy;
                _phase = Phase.DataPaused;
                _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SetReady);
            }
            else
            {
                // ContinueWriting() handles the guts
                Log.Error(Category.Streamer, "WriteData entered in phase {0}?", _phase);
            }

            //EXIT("WrData");
        }

        public void WriteFileMark(ulong skewNsec, object context)
        {
            //ENTER("WriteFM");
            switch (_phase)
            {
                case Phase.Accepted:
                    //
                    // Just like WriteData, first entry requires a new buffer.  The
                    // hardware might just fake it, but for simplicity I require it
                    // so the blocks are serialized in order.  Drop the Ready line
                    // to show we're busy then loop if necessary to get a block.
                    //
                    _ready = false;
                    _state = State.Busy;

                    if (_buffers.Count > MAXBUFFERS)
                    {
                        WaitForBuffer();
                    }
                    else
                    {
                        //
                        // Good to go.  File marks are "special" blocks synthesized
                        // by the controller; we don't get any data from the host.
                        //
                        _hostBuffer = new BlockBuffer(BlockType.FileMark);

                        while (!_hostBuffer.Full)
                        {
                            // Write the unique 5-bit code for a file mark :-)
                            _hostBuffer.PutByte(0x1c);
                        }

                        // We use the DataSent phase during a Write sequence to wait for
                        // the WriteBehind to sync the file mark to tape.  That phase is
                        // only used for Reads so it does double duty here.  Lazy.  Yes.
                        _phase = Phase.DataSent;

                        // Commit that puppy and kick the writer
                        _buffers.Enqueue(_hostBuffer);
                        _drive.Flush();
                    }
                    break;

                case Phase.DataSent:
                    //
                    // WriteBehind calls us when the tape has stopped.  This ensures
                    // that the final file mark is flushed before rewinding!  At least
                    // one sector time has already elapsed, so we finally set Ready and
                    // return to DataPaused where the write sequence can continue.
                    //
                    if (_command == Command.WriteFileMark)
                    {
                        _phase = Phase.DataPaused;
                        _ready = true;
                    }
                    else if (_command == Command.ReadStatus)
                    {
                        Log.Warn(Category.Streamer, "** Microcode bug workaround: Joining {0} command", _command);

                        // Terminate the write sequence (return to Idle) and jump
                        // straight into the ReadStatus loop.  This seems like the
                        // least horrible option?  Oof.
                        _phase = Phase.Accepted;
                        UpdateStatus();
                        DoReadStatus();
                    }
                    else
                    {
                        Log.Error(Category.Streamer, "WriteFM caught unexpected signal in phase {0}?", _phase);
                    }
                    break;

                default:
                    Log.Error(Category.Streamer, "WriteFM entered in phase {0}?", _phase);
                    break;
            }

            //EXIT("WriteFM");
        }


        void SetReady(ulong skewNsec, object context)
        {
            _ready = true;
        }

        #endregion

        #region Read commands

        State StartReading()
        {
            //ENTER("StartRd");

            if (_phase == Phase.Accepted)
            {
                if (_command == Command.ReadFileMark)
                    _commandSequence.Push(ReadFileMark);
                else
                    _commandSequence.Push(ReadData);

                return RunSequence();
            }

            // If the host didn't do this explicitly before calling Read, the
            // drive selects unit 0 and rewinds.  The "at position" flag tells
            // us whether or not it's appropriate to rewind.  The read sequence
            // is a little weirder than the writes (which are one shot).
            if (!AtBeginningOfTape && !_atPosition)
            {
                Log.Info(Category.Streamer, "Tape needs to Rewind at start of new sequence?");
                _commandSequence.Push(_drive.Rewind);
            }

            // Zap the buffers
            ResetBuffers();

            // Off we go...
            return RunSequence();
        }

        State ContinueReading()
        {
            //ENTER("ContRd");

            switch (_phase)
            {
                case Phase.Accepted:
                    //
                    // Continuation of a sequence of Read Data or Read FM commands:
                    // push onto the queue and jump back in where we left off.
                    //
                    if (_command == Command.ReadFileMark)
                        _commandSequence.Push(ReadFileMark);
                    else
                        _commandSequence.Push(ReadData);

                    return RunSequence();

                case Phase.DataReady:
                    //
                    // At the gap between blocks: the host buffer is ready to send
                    // (if it's a Data buffer) and the host has the same options
                    // as the write sequence:  reset Online to terminate, set Xfer
                    // to start the transfer, or set Request to issue a new command.
                    //
                    if (!_online)
                    {
                        Log.Info(Category.Streamer, "Host offline on read (should rewind!?)");

                        _atPosition = false;
                        _acknowledge = false;
                        _phase = Phase.Ready;

                        return RunSequence();
                    }

                    if (_request)
                    {
                        Log.Info(Category.Streamer, "Host terminating read to issue a request!");

                        _acknowledge = false;
                        _phase = Phase.Ready;

                        CheckSignals();
                        break;
                    }

                    if (_xfer)
                    {
                        // T13 - They took the byte!  Drop Ready and send rest of the block
                        _ready = false;
                        _phase = Phase.DataAck;
                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, ReadData);
                    }
                    break;

                case Phase.DataSent:
                    if (_xfer)
                    {
                        // T20 - Host acknowledges bytes 2..512 of a block
                        _phase = Phase.DataAck;
                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, ReadData);
                    }
                    break;

                case Phase.DataAccepted:
                    if (!_xfer)
                    {
                        // T16/T23 - Host got the byte, so check for end-of-block
                        if (_hostBuffer.ReadComplete)
                        {
                            Log.Write(Category.Streamer, "{0} block transfer complete!", _hostBuffer.Type);

                            // Go get the next buffer
                            _phase = Phase.DataPaused;
                            _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, ReadData);
                        }
                        else
                        {
                            // Keep on truckin'
                            _phase = Phase.DataSent;
                            _protocolEvent = _scheduler.Schedule(2 * Conversion.UsecToNsec, SendDataByte);
                        }
                    }
                    break;

                default:
                    //
                    // We check here for Online changing state to catch cases where
                    // Stut gets confused or we get stuck, though that probably
                    // shouldn't happen if/when things are fully debugged.  The spec
                    // is pretty clear that the result is always to stop, rewind,
                    // and reset for a read status command...
                    Log.Error(Category.Streamer, "Unexpected entry in phase {0}, state {1} (online={2})",
                                                 _phase, _state, _online);

                    if (!_online)
                    {
                        Log.Info(Category.Streamer, "Drive offline, rewinding...");
                        _commandSequence.Push(RewindComplete);
                        _commandSequence.Push(_drive.Rewind);
                    }
                    break;
            }

            return State.Busy;
        }

        void SendDataByte(ulong skewNsec, object context)
        {
            // T12/T19 - Put the next byte on the bus
            _data = _hostBuffer.GetByte();
            _acknowledge = true;
        }

        void ReadData(ulong skewNsec, object context)
        {
            //ENTER("RdData");

            switch (_phase)
            {
                case Phase.Accepted:
                    // 
                    // T6-T8: Starting or continuing a read sequence.  Raise the Ready
                    // line and jump to the "paused" state to allocate a new buffer.
                    // 
                    _ready = false;
                    _state = State.Busy;
                    _phase = Phase.DataPaused;

                    // Start prefetching blocks!
                    _drive.Fetch();

                    // Come back to check if the data is ready to send
                    _protocolEvent = _scheduler.Schedule(OneSectorDelay, ReadData);
                    break;

                case Phase.DataPaused:
                    //
                    // T10/T24 - Before asserting ready we need to make sure the
                    // drive has filled a buffer for us.  If not, we pause here
                    // until one is read.  Then we check the type: a file mark
                    // means we bug out.
                    //
                    if (_buffers.Count > 0)
                    {
                        _hostBuffer = GetBuffer();

                        // Did we just land on a file mark?
                        if (_hostBuffer.Type == BlockType.FileMark)
                        {
                            Log.Info(Category.Streamer, "File mark read!");

                            // ReadAhead has already issued a tape stop and set
                            // the AtFileMark flag; here we set AtPosition and
                            // raise the exception to force Read Status
                            _atPosition = true;
                            _exception = true;
                            _ready = false;
                            _phase = Phase.Ready;
                            _state = FinishCommand();
                            break;
                        }

                        // Can't cope with bad blocks (yet)
                        if (_hostBuffer.Type != BlockType.Data && _hostBuffer.Type != BlockType.BadData)
                            throw new InvalidOperationException("Read a non-Data block");

                        // T11-T12: Jump into DataReady.  Since we're sending now,
                        // the first data byte has to be on the bus before we Ack,
                        // even if they reject the block
                        Log.Info(Category.Streamer, "Data block is ready, sending first byte");
                        _phase = Phase.DataReady;
                        _ready = true;

                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, SendDataByte);
                    }
                    else
                    {
                        WaitForBuffer(true);    // Start ReadAhead if necessary
                    }
                    break;

                case Phase.DataAck:
                    //
                    // T15/T21 - drop Acknowledge
                    //
                    _phase = Phase.DataAccepted;
                    _acknowledge = false;
                    break;

                default:
                    Log.Error(Category.Streamer, "ReadData entered in phase {0}?", _phase);
                    break;
            }

            //EXIT("RdData");
        }

        void ReadFileMark(ulong skewNsec, object context)
        {
            //ENTER("ReadFM");

            switch (_phase)
            {
                case Phase.Accepted:
                    // 
                    // T6-T8: Start the search.  Ready remains reset and we exit
                    // only by asserting Exception when a file mark is read OR we
                    // reach the end of the tape.
                    // 
                    _ready = false;
                    _state = State.Busy;
                    _phase = Phase.DataPaused;

                    // Start prefetching blocks!
                    _drive.Fetch();

                    // Come back to check if the data is ready to send
                    _protocolEvent = _scheduler.Schedule(OneSectorDelay, ReadData);
                    break;

                case Phase.DataPaused:
                    //
                    // Here when a buffer is available.  Ignore any block that
                    // isn't a file mark; EOM detection is handled by ReadAhead
                    //
                    if (_buffers.Count > 0)
                    {
                        _hostBuffer = GetBuffer();

                        // Did we find what we're lookin' for?
                        if (_hostBuffer.Type == BlockType.FileMark)
                        {
                            Log.Info(Category.Streamer, "File mark read!");

                            // ReadAhead has already issued a tape stop and set
                            // the AtFileMark flag; here we set the AtPosition
                            // flag and raise the exception to force Read Status
                            _atPosition = true;
                            _exception = true;
                            _ready = false;
                            _phase = Phase.Ready;
                            _state = FinishCommand();
                        }
                        else
                        {
                            Log.Info(Category.Streamer, "Discarding {0} block", _hostBuffer.Type);
                        }
                    }

                    // If still searching, schedule another callback
                    if (!AtFileMark) WaitForBuffer(true);
                    break;

                default:
                    Log.Error(Category.Streamer, "ReadFM entered in phase {0}?", _phase);
                    break;
            }

            //EXIT("ReadFM");
        }

        #endregion

        #region Command sequence actions

        public State RunSequence()
        {
            Log.Info(Category.Streamer, "RunSequence: {0} commands to go", _commandSequence.Count);

            // Any commands queued up?
            if (_commandSequence.Count > 0)
            {
                var method = _commandSequence.Pop();    // Save it
                method.Invoke(0, null);                 // Run it!

                return State.Busy;                      // I'm woikin' heeyah!
            }

            // We've reached the end and didn't abort, so call FinishCommand to
            // return our completion state (Idle, usually).
            return FinishCommand();
        }

        /// <summary>
        /// Common routine to finish a command.  Clears any remaining steps in
        /// the sequence if a command terminates prematurely.  Turns off the
        /// activity light if it's on.  NB: Does NOT change the Ready signal!
        /// </summary>
        State FinishCommand()
        {
            ENTER("Finish");
            UpdateStatus();

            // Return to idle, but check for abnormal conditions...
            State final;

            if (!_drive.IsLoaded)
            {
                _ready = false;
                _exception = true;
                final = State.Unloaded;     // Always overrides
                _commandSequence.Clear();
            }
            else if (_illegal)
            {
                final = State.Fault;        // Illegal command
                _commandSequence.Clear();
            }
            else if (_exception)
            {
                // If we're in Reset, remain there; otherwise Fault
                // (This affects the setting of a specific bit in B1)
                final = (_state == State.Reset ? State.Reset : State.Fault);
                _commandSequence.Clear();
            }
            else
            {
                // Are we there yet?  Are we there yet?
                final = (_commandSequence.Count > 0 ? State.Busy : State.Idle);
            }

            if (_drive.ActivityLight) _drive.ShowIcon(false);

            // Reset state machine to accept next command
            _phase = Phase.Ready;

            // Oh just set it already.  The whole elegant "run a sequence" thing
            // is a bloody disaster since the PERQ does things its own damn way
            _state = final;

            EXIT("Finish");
            return final;
        }

        /// <summary>
        /// Drive reports an underrun.  Just count 'em up.
        /// </summary>
        public void Underrun()
        {
            _status.Underruns++;
        }

        /// <summary>
        /// Drive reports no data found while reading, which could indicate that a
        /// blank or defective tape is loaded.  Count the errors and let the drive
        /// know when the threshold has been exceeded so it can handle the fault.
        /// </summary>
        public bool NoDataError()
        {
            _status.NoData++;
            return _status.NoData > 31;
        }

        /// <summary>
        /// Drive reports an exception (like EOM).  Set the _exception signal and
        /// punt the current command.  Host should respond with ReadStatus.
        /// </summary>
        public void DriveFault()
        {
            _exception = true;
            _acknowledge = false;
            _state = FinishCommand();
        }

        /// <summary>
        /// Updates the status buffer based on current conditions.  Resets the
        /// current byte counter to zero.
        /// </summary>
        void UpdateStatus()
        {
            _status.B0 = (!_drive.IsLoaded ? StatusByte0.NoCartridge :
                         ((_drive.Info.IsWritable ? 0 : StatusByte0.WriteProtected) |
                          (_status.NoData > 31 ? StatusByte0.UnrecoverableDataError : 0) |
                          ((AtEndOfMedia || AtLogicalEOM) ? StatusByte0.EndOfMedia : 0) |
                          (AtFileMark ? StatusByte0.FileMarkDetected : 0)));

            _status.B1 = ((_illegal ? StatusByte1.IllegalCommand : 0) |
                          (_status.NoData > 31 ? StatusByte1.NoData : 0) |
                          (AtBeginningOfTape ? StatusByte1.BeginningOfMedia : 0) |
                          (_state == State.Reset ? StatusByte1.ResetPowerOn : 0));

            // Todo: add missing bits in B0 & B1 if they're applicable...
            // For now, a "No Data" condition sets UDE in byte 0 and ND in byte 1
            // but other error conditions aren't yet generated or checked.

            // Error and Underrun counts are just incremented as they occur,
            // and are reset explicitly when the ReadStatus command completes.
        }

        #endregion

        /// <summary>
        /// Called by the mechanism when a new cartridge is inserted.
        /// </summary>
        public void Load()
        {
            Log.Write(Category.Streamer, "Drive reports new cartridge loaded");
            Reset();
        }

        /// <summary>
        /// Signal from the drive when the cartridge is ejected.
        /// </summary>
        public void Unload()
        {
            if (_protocolEvent != null) _scheduler.Cancel(_protocolEvent);

            // Premature ejectulation?
            _ready = false;
            _exception = true;
            _state = FinishCommand();

            Log.Write(Category.Streamer, "Drive reports cartridge ejected");
        }

        #region Debugging

        void ENTER(string rtn)
        {
            Log.Info(Category.Streamer, "{0,8}: IN  {1}, {2,-12} seq={3} | {4} {5} {6} | {7} {8} {9}",
                      rtn, _state, _phase, _commandSequence.Count,
                      _online ? "ONLINE" : "",
                      _request ? "RQST" : "",
                      _xfer ? "XFER" : "",
                      _exception ? "EXCPT" : "",
                      _ready ? "RDY" : "",
                      _acknowledge ? "ACK" : "");
        }

        void EXIT(string rtn)
        {
            Log.Info(Category.Streamer, "{0,8}: OUT {1}, {2,-12} seq={3} | {4} {5} {6} | {7} {8} {9}",
                      rtn, _state, _phase, _commandSequence.Count,
                      _online ? "ONLINE" : "",
                      _request ? "RQST" : "",
                      _xfer ? "XFER" : "",
                      _exception ? "EXCPT" : "",
                      _ready ? "RDY" : "",
                      _acknowledge ? "ACK" : "");
        }

        public void DumpState()
        {
            Console.WriteLine("Sidewinder controller status:");
            Console.WriteLine("  State={0} Handshake={1} Command={2} Data={3}",
                               _state, _phase, _command, _data);
            Console.WriteLine("  Current signals:  {0} {1} {2} | {3} {4} {5}",
                               _online ? "ONLINE" : "online",
                               _request ? "RQST" : "rqst",
                               _xfer ? "XFER" : "xfer",
                               _exception ? "EXCPT" : "excpt",
                               _ready ? "RDY" : "rdy",
                               _acknowledge ? "ACK" : "ack");
            Console.WriteLine("  Protocol event {0} active", _protocolEvent == null ? "is NOT" : "is");
            Console.WriteLine("  {0} events in command queue", _commandSequence.Count);
            Console.WriteLine("  Status byte 0: {0}", _status.B0);
            Console.WriteLine("  Status byte 1: {0}", _status.B1);
            Console.WriteLine("  ReadComplete flag is {0}", _status.ReadComplete);

            _drive.DumpState();

            DumpBuffers();
        }

        void DumpBuffers()
        {
            Console.WriteLine("\nBuffer status:");
            Console.WriteLine($"  Host buffer is {_hostBuffer.Type}, {_hostBuffer.Pointer} bytes");
            Console.WriteLine($"  Queue holds {_buffers.Count} buffers");

            foreach (var buf in _buffers)
            {
                Console.WriteLine($"  Queued buffer is {buf.Type}, {buf.Pointer} bytes");
            }
        }

        #endregion


        // State of the "intelligent" half (controller)
        State _state;
        Phase _phase;
        Command _command;

        // Some commands require multiple steps
        Stack<SchedulerEventCallback> _commandSequence;

        // The "dumb" half (mechanism and storage media)
        CartridgeTape _drive;

        // Status bytes from the drive itself
        DriveStatus _status;

        // Hardware signals     TO the host:
        bool _ready;            // READY
        bool _acknowledge;      // ACK
        bool _exception;        // EXCEPTION

        //                      FROM the host:
        bool _online;           // ONLINE
        bool _request;          // REQUEST
        bool _xfer;             // XFER

        byte _data;             // DATABUS <7:0>

        // Global conditions
        bool _illegal;
        bool _atPosition;

        // For passing blocks between the drive and controller
        Queue<BlockBuffer> _buffers;
        BlockBuffer _hostBuffer;

        // For timing protocol interactions
        Scheduler _scheduler;
        SchedulerEvent _protocolEvent;
    }

    /// <summary>
    /// Three MSBs (7:5) define the command issued to the drive.  These are sent
    /// with up to five significant bits (4:0) of command data.  Because only
    /// specific bit patterns are allowed, we just enumerate the legal ones and
    /// treat the rest as errors (they're supposed to be zeros).
    /// </summary>
    public enum Command
    {
        None = 0x0,         // Drive unselected
        Select = 0x01,      // We only allow selection of drive 0 (for now)
        Position = 0x20,    // Illegal! Requires one of the low 3 bits set
        Rewind = 0x21,
        Erase = 0x22,
        Retension = 0x24,
        WriteData = 0x40,
        WriteFileMark = 0x60,
        ReadData = 0x80,
        ReadFileMark = 0xa0,
        ReadStatus = 0xc0,
        Reserved = 0xe0
    }

    /// <summary>
    /// Drive controller state.
    /// </summary>
    public enum State
    {
        Unloaded = 0,       // No tape in the drive
        Idle,               // Have a tape, no action
        Busy,               // Executing a command/sequence
        Fault,              // Something went wrong (like, tape yanked out?)
        Reset               // Powered on or reset (waiting for ReadStatus)
    }

    /// <summary>
    /// States required for the various command, status or data byte handshaking
    /// exchanges.  Request/Ready lines lines are used for command and status
    /// while Xfer/Ack are for data; there are up to 40 state transitions for a
    /// complex write sequence.  Zoinks.
    /// </summary>
    public enum Phase
    {
        Ready = 0,          // Waiting for some action
        Request,            // Start of handshake
        Response,           // Command incoming
        RequestAck,         // Request acknowledged
        Accepted,           // Command received
        StatusReady,        // Ready to send status
        StatusAck,          // Host ready to receive
        StatusSent,         // Status byte sent
        StatusAccepted,     // Host ready for next one
        DataReady,          // Drive ready to send/recv
        DataSent,           // Drive/Host provides byte
        DataAck,            // Host/Drive takes byte
        DataAccepted,       // Ready for next one
        DataPaused          // Waiting for a free buffer
    }

    /// <summary>
    /// Gather up all the status bits and bobs.
    /// </summary>
    struct DriveStatus
    {
        public void Clear()
        {
            B0 = 0;
            B1 = 0;
            ErrorCount = 0;
            Underruns = 0;
            NoData = 0;
            _nextByte = 0;
        }

        public byte GetNextByte()
        {
            _nextByte++;

            switch (_nextByte)
            {
                case 1:
                    // If any bits are set, set the MSB
                    if (B0 != 0)
                        B0 |= StatusByte0.ExceptionByte0;

                    Log.Info(Category.Streamer, "Status byte 0: {0}", B0);
                    return (byte)B0;

                case 2:
                    // As with B0, set MSB if any flags set
                    if (B1 != 0)
                        B1 |= StatusByte1.ExceptionByte1;

                    Log.Info(Category.Streamer, "Status byte 1: {0}", B1);
                    return (byte)B1;

                case 3:
                    return (byte)(ErrorCount >> 8);

                case 4:
                    Log.Info(Category.Streamer, "Error count: {0}", ErrorCount);
                    return (byte)(ErrorCount & 0xff);

                case 5:
                    return (byte)(Underruns >> 8);

                case 6:
                    Log.Info(Category.Streamer, "Underruns: {0}", Underruns);
                    return (byte)(Underruns & 0xff);

                default:
                    Log.Warn(Category.Streamer, "Status buffer overrun, tried to read {0} bytes", _nextByte);
                    return 0;
            }
        }

        public bool ReadComplete => (_nextByte >= 6);

        public StatusByte0 B0;
        public StatusByte1 B1;
        public ushort ErrorCount;
        public ushort Underruns;
        public int NoData;

        int _nextByte;
    }

    /// <summary>
    /// Drive status bytes are internal to the Sidewinder's controller; the
    /// PERQ sends the status of the signal lines to the microcode!
    /// </summary>
    [Flags]
    enum StatusByte0
    {
        FileMarkDetected = 0x1,
        BIENotLocated = 0x2,
        UnrecoverableDataError = 0x4,
        EndOfMedia = 0x8,
        WriteProtected = 0x10,
        DriveNotOnline = 0x20,
        NoCartridge = 0x40,
        ExceptionByte0 = 0x80
    }

    [Flags]
    enum StatusByte1
    {
        ResetPowerOn = 0x1,
        Rsvd1 = 0x2,
        Rsvd2 = 0x4,
        BeginningOfMedia = 0x8,
        MoreThan8Retries = 0x10,
        NoData = 0x20,
        IllegalCommand = 0x40,
        ExceptionByte1 = 0x80
    }
}
