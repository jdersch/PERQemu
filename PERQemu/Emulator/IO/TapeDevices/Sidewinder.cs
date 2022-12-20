//
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
    /// Emulates the Archive Sidewinder 3020I QIC tape drive.
    /// </summary>
    /// <remarks>
    /// In theory, this is a generic QIC-02 interfaced 4-track quarter-inch tape
    /// drive.  Using the standard media, this provides 20MB of storage on a
    /// DC300XL cartridge.  In practice, it also can emulate the Sidewinder 9020I
    /// and 9045I 9-track drives, which are faster and offer higher capacities!
    /// </remarks>
    public partial class Sidewinder : StorageDevice
    {
        public Sidewinder(Scheduler sched, string filename) : base(filename)
        {
            _scheduler = sched;
            _motionEvent = null;
            _protocolEvent = null;

            // If instantiated without a cartridge loaded!
            if (string.IsNullOrEmpty(filename))
            {
                // Basic device profile
                Info = DeviceInfo.A3020;
                Geometry = DeviceGeometry.NoMedia;
                Specs = DevicePerformance.Archive30IPS;
            }

            _buffers = new BlockBuffer[5];      // For testing; make tunable?
            _status = new DriveStatus();

            _commandSequence = new Stack<SchedulerEventCallback>();

            _activityLight = false;

            InitBuffers();
            Reset();
        }

        // Status lines sent to the PERQ
        public bool Ready => _ready;
        public bool Acknowledge => _acknowledge;
        public bool Exception => _exception;
        public bool Direction => _fromHost;

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

        // Internal status
        private bool AtBeginningOfTape => _position == 0;
        private bool AtEndOfTape => _position == Geometry.Sectors - 1;
        private bool AtEndOfMedia => _position == _lastBlock;
        private bool AtFileMark => _atFileMark;

        /// <summary>
        /// Reset the drive and controller.
        /// </summary>
        public void Reset()
        {
            if (_motionEvent != null)
            {
                _scheduler.Cancel(_motionEvent);
                _motionEvent = null;
            }

            if (_protocolEvent != null)
            {
                _scheduler.Cancel(_protocolEvent);
                _protocolEvent = null;
            }

            ResetBuffers();

            _command = Command.None;
            _commandSequence.Clear();

            _position = 0;
            _tapeInMotion = false;
            _atFileMark = false;

            // Set our outgoing signals according to the spec
            _ready = false;
            _acknowledge = true;
            _exception = true;
            _fromHost = true;

            if (_activityLight) ShowIcon(false);

            _phase = Phase.Ready;
            _state = State.Reset;
            Log.Info(Category.Streamer, "Drive reset");
        }

        #region Command byte protocol

        /// <summary>
        /// Check the current state of the signal lines from the host.  This can
        /// initiate a new command, terminate execution of a command in progress
        /// or indicate that a data byte has been transmitted or received.
        /// </summary>
        public void CheckSignals()
        {
            ENTER("CkSig");

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
                                Log.Info(Category.Streamer, "Unimplemented command {0}", _command);
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
                                Console.WriteLine($"Unhandled signal change for {_command} in Busy state");
                                break;
                        }
                        break;

                    default:
                        throw new InvalidOperationException($"Unexpected state {_state} in CheckSignals");
                }
            }

            _state = nextState;
            EXIT("ChkSig");
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

            ENTER("CmdByte");

            switch (_phase)
            {
                case Phase.Ready:
                    if (_request)
                    {
                        // Save the command byte from the bus latch
                        _command = (Command)_data;
                        Log.Info(Category.Streamer, "--> Read command byte: {0}", _command);   // debug

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
                            Log.Write(Category.Streamer, "Fault: request {0} while ONLINE false", _command);
                            return false;
                        }

                        // If the request is a write or erase and the cartridge
                        // is write protected, that's an exception too!
                        if ((_command == Command.Erase ||
                             _command == Command.WriteData ||
                             _command == Command.WriteFileMark) &&
                            !Info.IsWritable)
                        {
                            _exception = true;
                            _state = State.Fault;
                            Log.Write(Category.Streamer, "Fault: request {0} while write-protected", _command);
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

            EXIT("CmdByte");
            return _phase >= Phase.Accepted;
        }

        #endregion

        #region Select, Status and Positioning

        private State DoSelect()
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
        private State DoReadStatus()
        {
            ENTER("RdStat");

            // If coming out of Reset drop the ACK line and snapshot status
            if (_state == State.Reset)
            {
                _acknowledge = false;
                UpdateStatus();
            }

            switch (_phase)
            {
                case Phase.Accepted:
                    // Set up for transfer: change direction signal
                    _fromHost = false;
                    _phase = Phase.StatusReady;

                    // Schedule the next transition in 20usec
                    _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, (skewNsec, context) =>
                    {
                        // Do it this way so we can be called during a Write
                        // or Read sequence without falling into FinishCommand
                        // if the host wishes to continue; "elegantly clunky"?
                        _commandSequence.Push(SendStatusByte);
                        RunSequence();
                    });

                    Log.Detail(Category.Streamer, "ReadStatus: dir change, sending first byte in 20us");
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
                    // Complete the handshake for the byte!
                    _phase = Phase.StatusAccepted;

                    // Any more to send?
                    if (!_status.ReadComplete)
                    {
                        // Yep!  Ship it
                        _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SendStatusByte);
                        return State.Busy;
                    }

                    // Nope!  All done - schedule our final transition
                    _protocolEvent = _scheduler.Schedule(10 * Conversion.UsecToNsec, (skewNsec, context) =>
                    {
                        _status.Clear();        // Reset
                        _fromHost = true;       // Turn around the bus 
                        _ready = true;          // Ready for new command
                        _phase = Phase.Ready;   // Fixme: this probably isn't right
                    });

                    // Fall through to finish the command
                    break;

                default:
                    Log.Error(Category.Streamer, "ReadStatus: impossible? Phase={0}", _phase);
                    // Shouldn't ever happen; fall out to clean up if it does...
                    break;
            }

            // If we fall out, we're done!  Reset the status buffer
            _status.Clear();
            Log.Debug(Category.Streamer, "ReadStatus complete");

            return RunSequence();
        }

        private void SendStatusByte(ulong skewNsec, object context)
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
        private State DoPosition()
        {
            _commandSequence.Clear();

            switch (_command)
            {
                case Command.Rewind:
                    _commandSequence.Push(Rewind);
                    break;

                case Command.Erase:
                case Command.Retension:
                    _commandSequence.Push(Rewind);
                    _commandSequence.Push(Wind);

                    if (_position != 0)
                    {
                        _commandSequence.Push(Rewind);
                    }
                    break;

                default:
                    throw new InvalidOperationException($"Invalid position command {_command}");
            }

            // Start the sequence and return Busy
            return RunSequence();
        }

        #endregion

        #region Write commands

        /// <summary>
        /// Start the absurdly complex process of writing to the tape.  The initial
        /// command can be a WriteData or a WriteFileMark, followed by a sequence
        /// of continuous writes until the host is finished, an error occurs, or
        /// the end of the media is reached.  
        /// </summary>
        private State StartWriting()
        {
            ENTER("StartWr");

            //
            // Initialize a new Write sequence
            //
            // These conditions are checked in GetCommandByte:
            //    ONLINE is true; tape IS loaded and is NOT write protected
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
                _commandSequence.Push(Rewind);
            }

            // Zap the internal buffers
            ResetBuffers();

            // Clear the current _lastBlock, assuming that a) a Read may have
            // found and set the EOM flag short of the actual full capacity, and
            // b) an Erase may not have been done first (is that required?).
            _lastBlock = Geometry.TotalBlocks - _buffers.Length;   // Wiggle room!

            // Off we go...
            return RunSequence();
        }

        /// <summary>
        /// Handle continuations of the write sequence: in the Busy state, this
        /// means we're processing data bytes using the Xfer/Ack handshaking, OR
        /// we've fallen back to process a command change between blocks.
        /// </summary>
        private State ContinueWriting()
        {
            ENTER("ContWr");

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
                        Log.Info(Category.Streamer, "Online dropped, terminating write sequence");

                        // Put us back in command mode
                        _phase = Phase.Ready;

                        // If Online drops at this point, the drive automatically
                        // writes a file mark and rewinds.  The PERQ microcode may
                        // instead explicitly issue these commands rather than rely
                        // on the controller's default behavior, so in that case
                        // this will generally only occur if the controller times
                        // out and drops online (or Resets) to recover?
                        _commandSequence.Push(Rewind);

                        // "If the last command was a WFM command, the controller
                        // will not write another file mark."
                        if (_command != Command.WriteFileMark)
                            _commandSequence.Push(WriteFileMark);

                        nextState = RunSequence();
                        break;
                    }

                    if (_request)
                    {
                        Log.Info(Category.Streamer, "Request raised, jumping out to receive new command!");

                        // Host wants to issue a new command.  At this point the
                        // byte is already on the bus, so we reset to the command
                        // phase and exit through GetCommandByte.  This lets the
                        // handshake complete without popping the sequence queue.
                        // If a Write or WriteFileMark is issued we'll come back
                        // here to push the new command back on the queue and
                        // continue where we left off...

                        // Don't leak that buffer!
                        if (_buffers[_hostBuffer].Empty)
                        {
                            Log.Info(Category.Streamer, "Releasing {0} buffer {1}", _buffers[_hostBuffer].Type, _hostBuffer);

                            // This is sort of annoying, because the microcode does
                            // NOT have to issue a new WriteData command for every
                            // block -- it can just drop Xfer and continue!  Argh.
                            _buffers[_hostBuffer].SetType(BlockType.Empty);
                        }

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

                        if (GetEmptyBuffer(ref _hostBuffer))
                        {
                            // Got one!
                            Log.Info(Category.Streamer, "Next host write buffer is {0}", _hostBuffer);

                            // Set up the buffer for the incoming block
                            _buffers[_hostBuffer].SetType(BlockType.Data);
                            _buffers[_hostBuffer].Clear();

                            // The first byte is already on the bus, so jump
                            // directly back into the data transfer loop
                            _phase = Phase.DataReady;

                            // Sigh.  Can't just fall through the case here, so... 
                            ContinueWriting();
                        }
                        else
                        {
                            WaitForBuffer();
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
                        // round to 4usec and see how that performs, may tune it)
                        _buffers[_hostBuffer].PutByte(_data);

                        _phase = Phase.DataAck;
                        _ready = false;

                        _protocolEvent = _scheduler.Schedule(4 * Conversion.UsecToNsec, (skewNsec, context) =>
                         {
                             _acknowledge = true;
                             _phase = Phase.DataAccepted;
                         });
                    }
                    else
                    {
                        Log.Info(Category.Streamer, "Caught a stray signal change in DataReady");
                    }
                    break;

                case Phase.DataAccepted:
                    //
                    // T14-T16 - Byte acknowledged
                    //
                    // If the current block is full (512 bytes recieved) then we
                    // call Flush to make sure the mechanism is running (started
                    // after the first buffer fills, or restarted after underrun).

                    if (_buffers[_hostBuffer].Full)
                    {
                        Log.Info(Category.Streamer, "Buffer {0} is full!", _hostBuffer);

                        Flush();

                        // Need a new buffer
                        _phase = Phase.DataPaused;

                        // Reset Ack to indicate normal reception of the last byte;
                        // There's no response from the host after byte 512, but
                        // the transition to Ready will move us into the next phase.
                        // Spec says T22-T23 is >100usec!
                        _acknowledge = false;
                        _protocolEvent = _scheduler.Schedule(100 * Conversion.UsecToNsec, SetReady);
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

                default:
                    Console.WriteLine($"Continue in wut now?  phase={_phase} state={_state}");
                    break;
            }

            EXIT("ContWr");
            return nextState;
        }


        private void WaitForBuffer()
        {
            // Wait one sector time for the writer to catch up
            var retry = Settings.Performance.HasFlag(RateLimit.TapeSpeed) ?
                                ((ulong)Specs.MinimumSeek * Conversion.UsecToNsec) :
                                SecTime90IPS;

            Log.Info(Category.Streamer, "Waiting for free buffer; retry in {0}ms...",
                                          retry * Conversion.NsecToMsec);

            if (!_tapeInMotion) Flush();    // Kick the drive again?

            // Come around, idiot, come around
            _protocolEvent = _scheduler.Schedule(retry, (skewNsec, context) =>
                {
                    CheckSignals();
                });
        }


        private void WriteData(ulong skewNsec, object context)
        {
            ENTER("WrData");

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

            EXIT("WrData");
        }

        private void WriteFileMark(ulong skewNsec, object context)
        {
            ENTER("WriteFM");
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

                    if (GetEmptyBuffer(ref _hostBuffer))
                    {
                        //
                        // Got our buffer!  File marks are "special" blocks synthesized
                        // by the controller; we don't get any data from the host.
                        //
                        _buffers[_hostBuffer].SetType(BlockType.FileMark);
                        _buffers[_hostBuffer].Clear();

                        while (!_buffers[_hostBuffer].Full)
                        {
                            // Write the unique 5-bit code for a file mark :-)
                            _buffers[_hostBuffer].PutByte(0x1c);
                        }

                        // We use the DataSent phase during a Write sequence to wait for
                        // the WriteBehind to sync the file mark to tape.  That phase is
                        // only used for Reads so it does double duty here.  Lazy.  Yes.
                        _phase = Phase.DataSent;

                        // Commit that puppy
                        Flush();
                    }
                    else
                    {
                        WaitForBuffer();
                    }
                    break;

                case Phase.DataSent:
                    //
                    // WriteBehind calls us when the tape has stopped.  This ensures
                    // that the final file mark is flushed before rewinding!  At least
                    // one sector time has already elapsed, so we finally set Ready and
                    // terminate the write sequence.
                    //
                    _ready = true;
                    _phase = Phase.Ready;
                    RunSequence();
                    break;

                default:
                    Log.Error(Category.Streamer, "WriteFileMark entered in phase {0}?", _phase);
                    break;
            }

            EXIT("WriteFM");
        }


        private void SetReady(ulong skewNsec, object context)
        {
            _ready = true;
        }


        private void Flush()
        {
            // We have a buffer to write, and we're not already in motion, fire it up
            if (DirtyBuffers() > 0 && !_tapeInMotion)
            {
                StartStop(WriteBehind);
            }
        }


        private void WriteBehind(ulong skewNsec, object context)
        {
            // Get the next dirty, dirty buffer
            if (!GetFullBuffer(ref _tapeBuffer))
            {
                Log.Info(Category.Streamer, "Underrun! No buffer ready for writing");

                _status.Underruns++;

                // Next buffer completion will call Flush to restart
                StartStop(null);
            }
            else
            {
                Log.Info(Category.Streamer, "Sweet! Buffer {0} is ready to flush @ pos {1}",
                          _tapeBuffer, _position);

                // Commit the block!
                WriteBuffer(_tapeBuffer, _position);

                // Was it a file mark?  Save for status...
                _atFileMark = (_buffers[_tapeBuffer].Type == BlockType.FileMark);

                // Clear the buffer now
                _buffers[_tapeBuffer].SetType(BlockType.Empty);
                _buffers[_tapeBuffer].Clear();

                // If we're on track 0, the erase bar is zapping the other tracks!
                if (_position < Geometry.Sectors)
                {
                    for (var trk = 1; trk < Geometry.Heads; trk++)
                    {
                        WriteBuffer(_tapeBuffer, _position + (trk * Geometry.Sectors));
                    }
                }

                // Advance position and check if we've hit the end of the media
                _position++;

                if (AtFileMark)
                {
                    // If we just wrote a file mark, spin down the tape with a
                    // call back to WriteFileMark to let it know.  It's not 100%
                    // clear from the spec if the tape _always_ stops when a WFM
                    // is issued while a "write sequence" is in progress, but on
                    // Reads it seems that file marks always raise an exception
                    // that requires a ReadStatus in response.  Let's assume that
                    // here as well.  Worst case is we just blow 200ms spinning
                    // the virtual tape down and back up for no reason!
                    Log.Info(Category.Streamer, "Wrote a file mark!");

                    UpdateStatus();
                    StartStop(WriteFileMark);
                    return;
                }

                if (AtEndOfMedia)
                {
                    // At the warning track; stop tape, set exception, require
                    // a Read Status to continue.  Jump back to DataReady for
                    // the last Write/WriteFM/offline processing
                    Log.Warn(Category.Streamer, "Reached the warning track, tape is almost full.");

                    StartStop(null);
                    _exception = true;
                    return;
                }

                if (AtEndOfTape)
                {
                    // Here we have to jam on the brakes and halt the whole write op
                    Log.Error(Category.Streamer, "Reached end of media, tape is full.  Hard stop.");

                    StartStop(null);
                    _exception = true;
                    FinishCommand();
                    return;
                }

                // If Online drops, stop motion?
                if (!Online)
                {
                    Log.Write(Category.Streamer, "Controller reports offline, stopping tape.");
                    StartStop(null);

                    return;
                }

                // Skip this if we aren't rate limiting and save hundreds milliseconds! :-)
                if (AtEndOfTape && Settings.Performance.HasFlag(RateLimit.TapeSpeed))
                {
                    // Lazily assume that Flush will start us back up, thereby
                    // simplifying this case tremendously.  Hopefully.
                    Console.WriteLine("\nAt EOT, have to change heads and direction!\n");
                    StartStop(null);
                }
                else
                {
                    // Keep on streamin'.  MinSeek is the total time to do one full block
                    var delay = Settings.Performance.HasFlag(RateLimit.TapeSpeed) ?
                                        ((ulong)Specs.MinimumSeek * Conversion.UsecToNsec) : SecTime90IPS;

                    _motionEvent = _scheduler.Schedule(delay, WriteBehind);
                    Log.Info(Category.Streamer, "Next tape block in {0}ms", delay * Conversion.NsecToMsec);
                }
            }
        }

        #endregion

        #region Read commands

        private State StartReading()
        {
            ENTER("StartRd");

            // READ command:
            //  Host asserts ONLINE and the READ command;
            //  If the "at position" flag is OFF:
            //      Drive rewinds to BOT and selects track 0
            //  Drive starts reading to load first block into the read buffer;
            //  if CRC and block address is good, starts transfer to the host
            //  Continues reading through the end of track 0; to continue,
            //  performs a stop, reverse, start on the next track up to ET.

            //  Read is terminated by the controller if a file mark is read;
            //      host is informed by an EXCEPTION and READ STATUS sequence

            //  Host may terminate the read by deactivating ONLINE; controller
            //      then performs an "offline" sequence

            //  Host may terminate by issuing a READ FILE MARK command; no more
            //      data is transferred and a RFM is performed (starting w/step 4)

            //  Host continues reading by issuing another READ command.  A file ID
            //  search can be performed by checking the block ID, but... how is
            //  that transferred to the host?  oh.  different thing.  nevermind

            //  todo: figure out how the "at position" bit is set...

            return State.Busy;  // placeholder
        }

        private State ContinueReading()
        {
            ENTER("ContRd");
            return State.Busy;
        }

        private void ReadData(ulong skewNsec, object context)
        {
            ENTER("RdData");
        }

        private void ReadFileMark(ulong skewNsec, object context)
        {
            ENTER("ReadFM");
            // READ FILE MARK command:
            //  Host asserts ONLINE and issues the RFM command
            //  If the "at position" flag is off, does a rewind first
            //  Controller then comes up to speed and reads until the next FM
            //  block is encountered
            //  If EOT is encountered, motion stops and drive asserts EXCEPTION
            //  and waits for READ STATUS.
            //  Host can terminate the RFM by dropping ONLINE; the drive then
            //  continues the command and performs an "offine sequence"
        }


        private void ReadAhead(ulong skewNsec, object context)
        {
            ENTER("RdAhead");

            // Preloads data buffers as long as we're in a read sequence
            // goes until it hits a FM and raises an exception, leaves the
            // tape where it's at (motor stopped)
            // opposite of FEEDME
        }

        #endregion

        #region Command sequence actions

        private State RunSequence()
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
        private State FinishCommand()
        {
            ENTER("Finish");
            UpdateStatus();

            if (_activityLight) ShowIcon(false);

            // Reset state machine to accept next command
            _phase = Phase.Ready;

            // Return to idle, but check for abnormal conditions...
            State final;

            if (!IsLoaded)
            {
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

            EXIT("Finish");
            return final;
        }

        /// <summary>
        /// Updates the status buffer based on current conditions.  Resets the
        /// current byte counter to zero.
        /// </summary>
        private void UpdateStatus()
        {
            _status.B0 = (!IsLoaded ? StatusByte0.NoCartridge :
                         ((Info.IsWritable ? 0 : StatusByte0.WriteProtected) |
                          ((AtEndOfMedia || AtEndOfTape) ? StatusByte0.EndOfMedia : 0) |
                          (AtFileMark ? StatusByte0.FileMarkDetected : 0)));

            _status.B1 = ((_illegal ? StatusByte1.IllegalCommand : 0) |
                          (AtBeginningOfTape ? StatusByte1.BeginningOfMedia : 0) |
                          (_state == State.Reset ? StatusByte1.ResetPowerOn : 0));

            // Todo: add missing bits in B0 & B1 if they're applicable...

            // Error and Underrun counts are just incremented as they occur,
            // and are reset explicitly when the ReadStatus command completes.
        }

        #endregion

        #region Drive actions

        /// <summary>
        /// Schedule an event to start or stop the capstan motor.  Specs.StartupDelay
        /// is the number of milliseconds to delay.  The _tapeInMotion flag is toggled.
        /// </summary>
        private void StartStop(SchedulerEventCallback nextStep)
        {
            // For accurate tape start/stop times, consult the Specs; otherwise
            // shorten it to a fixed 10ms (about 10x faster than typical)
            var delay = (Settings.Performance.HasFlag(RateLimit.TapeSpeed) ? Specs.StartupDelay : 10);

            Log.Write(Category.Streamer, "Motor {0} in {1}ms",
                       (_tapeInMotion ? "stopping" : "starting, up to speed"), delay);

            // Schedule it
            if (nextStep != null)
            {
                _motionEvent = _scheduler.Schedule((ulong)delay * Conversion.MsecToNsec, nextStep);

                // Toggle the motor state and update the icon now
                _tapeInMotion = !_tapeInMotion;
                ShowIcon(_tapeInMotion);
            }
            else
            {
                // Since there's nothin' else to do, update AFTER the delay
                _motionEvent = _scheduler.Schedule((ulong)delay * Conversion.MsecToNsec, (skewNsec, context) =>
                {
                    _tapeInMotion = !_tapeInMotion;
                    ShowIcon(_tapeInMotion);
                });
            }
        }

        /// <summary>
        /// Execute the steps required to rewind to BOT.  When complete the tape
        /// is stopped at position 0 and we exit to the dispatcher.
        /// </summary>
        private void Rewind(ulong skewNsec, object context)
        {
            Log.Write(Category.Streamer, "Rewind: tape is {0}, position {1}",
                       (_tapeInMotion ? "moving" : "stopped"), _position);

            // Rewinding is linear, but position counts logical blocks (over n
            // tracks).  If rewinding from an arbitrary position, mod the block
            // number and only count down from there.
            if (_position >= Geometry.Sectors)
            {
                Log.Write(Category.Streamer, "Rewind: Adjusting linear position {0} => {1}",
                           _position, _position % (Geometry.Sectors - 1));

                _position = _position % (Geometry.Sectors - 1);
            }

            if (!_tapeInMotion)
            {
                if (AtBeginningOfTape)
                {
                    // In case we didn't move, delay ready to give the microcode
                    // a chance to catch the falling AND rising edges!
                    _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SetReady);

                    _atFileMark = false;
                    _motionEvent = null;    // Clear the movement event
                    RunSequence();          // Return through the dispatcher
                }
                else
                {
                    StartStop(Rewind);      // Start up, call back to count down
                }
            }
            else
            {
                if (AtBeginningOfTape)
                {
                    StartStop(Rewind);      // Stop, and call back to finish command
                }
                else
                {
                    // Rewind at most about one second's worth at a time
                    var blocks = Math.Min(_position, SectorsPerSecond);
                    _position -= blocks;

                    ulong delay = (ulong)blocks * SecTime90IPS;
                    _motionEvent = _scheduler.Schedule(delay - skewNsec, Rewind);

                    ShowIcon(!_activityLight);
                }
            }
        }

        /// <summary>
        /// Wind the tape forward to EOT.  If the command is Erase, then zaps all
        /// the sector data along the way.  Assumes the tape has been rewound;
        /// finishes at position EOT (last sector on track 0) with the motor off.
        /// Blinks the icon along the way. :-)
        /// </summary>
        private void Wind(ulong skewNsec, object context)
        {
            Log.Write(Category.Streamer, "{0}: tape is {1}, position {2}",
                       _command, (_tapeInMotion ? "moving" : "stopped"), _position);

            // Like rewinding, this is a linear motion from BOT to EOT, but it
            // shouldn't ever start in the middle of the tape; assume we start
            // at zero and do one full pass.  Rather than call Format() in one
            // go (would make the emulator seriously hiccup?) we'll do it a few
            // blocks at a time, which lets us blink the activity light to show
            // progress.
            if (!_tapeInMotion)
            {
                if (AtEndOfTape)
                {
                    if (_command == Command.Erase)
                    {
                        // At the end of an erase, set the "end of media" marker
                        // in the block header of the last sector on the device
                        var eom = new BlockBuffer(this, BlockType.EndOfMedia);
                        eom.WriteTo(Geometry.TotalBlocks - 1);

                        // Housekeeping
                        _lastBlock = Geometry.TotalBlocks - _buffers.Length;    // Reset
                        Log.Write(Category.Streamer, "Erase: Last block is now {0}", _lastBlock);
                    }

                    _atFileMark = false;
                    _motionEvent = null;
                    RunSequence();
                }
                else
                {
                    StartStop(Wind);        // Start the motor, call back to start counting
                }
            }
            else
            {
                // Are we there yet?
                if (AtEndOfTape)
                {
                    StartStop(Wind);        // Stop!  Call back to finish the command
                }
                else
                {
                    // As with rewind, do about 1 second's worth
                    var blocks = Math.Min((Geometry.Sectors - 1) - _position, SectorsPerSecond);

                    // If erasing, actually zap the sectors.  The drive's erase
                    // bar does all n tracks at once!
                    if (_command == Command.Erase)
                    {
                        for (var h = 0; h < Geometry.Heads; h++)
                        {
                            for (var s = _position; s < _position + blocks; s++)
                            {
                                var sec = new BlockBuffer(this, BlockType.Empty);
                                sec.WriteTo(s + (h * Geometry.Sectors));
                            }
                        }
                    }

                    // Advance!
                    _position += blocks;

                    // Now schedule next callback and blink
                    ulong delay = (ulong)blocks * SecTime90IPS;
                    _motionEvent = _scheduler.Schedule(delay - skewNsec, Wind);

                    ShowIcon(!_activityLight);
                }
            }
        }

        #endregion

        #region Load, Unload and blinkenlight

        /// <summary>
        /// On load, check and set some flags 'n stuff.
        /// </summary>
        public override void OnLoad()
        {
            // It's the only way to be sure
            Reset();

            // Set our max LBN to the last possible block (as a backstop)
            _lastBlock = Geometry.TotalBlocks - 1;

            Log.Info(Category.Streamer, "{0} online", Info.Description);

            base.OnLoad();
        }

        public override void Unload()
        {
            Log.Info(Category.Streamer, "Tape cartridge is about to eject...");

            if (_protocolEvent != null) _scheduler.Cancel(_protocolEvent);
            if (_motionEvent != null) _scheduler.Cancel(_motionEvent);
            _tapeInMotion = false;
            _atFileMark = false;

            // Premature ejectulation?
            _exception |= (_state != State.Idle);

            FinishCommand();

            // Reset to baseline
            Info = DeviceInfo.A3020;
            Geometry = DeviceGeometry.NoMedia;
            Specs = DevicePerformance.Archive30IPS;
            base.Unload();
        }

        /// <summary>
        /// Set our icon state to show drive activity.
        /// </summary>
        private void ShowIcon(bool state)
        {
            PERQemu.Sys.MachineStateChange(WhatChanged.StreamerActivity, state);
            _activityLight = state;
        }

        #endregion

        #region Debugging

        private void ENTER(string rtn)
        {
            Log.Debug(Category.Streamer, "{0,8}: IN  {1}, {2} seq={3} | {4} {5} {6} | {7} {8} {9}",
                      rtn, _state, _phase, _commandSequence.Count,
                      _online ? "ONLINE" : "",
                      _request ? "RQST" : "",
                      _xfer ? "XFER" : "",
                      _exception ? "EXCPT" : "",
                      _ready ? "RDY" : "",
                      _acknowledge ? "ACK" : "");
        }

        private void EXIT(string rtn)
        {
            Log.Debug(Category.Streamer, "{0,8}: OUT {1}, {2} seq={3} | {4} {5} {6} | {7} {8} {9}",
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
            Console.WriteLine("  Current signals:  {0} {1} {2} | {3} {4} {5} {6}",
                               _online ? "ONLINE" : "online",
                               _request ? "RQST" : "rqst",
                               _xfer ? "XFER" : "xfer",
                               _exception ? "EXCPT" : "excpt",
                               _ready ? "RDY" : "rdy",
                               _acknowledge ? "ACK" : "ack",
                               _fromHost ? "drive < HOST" : "DRIVE > host");
            Console.WriteLine("  Protocol event {0} active", _protocolEvent == null ? "is NOT" : "is");
            Console.WriteLine("  {0} events in command queue", _commandSequence.Count);
            Console.WriteLine("  Status byte 0: {0}", _status.B0);
            Console.WriteLine("  Status byte 1: {0}", _status.B1);
            Console.WriteLine("  ReadComplete flag is {0}", _status.ReadComplete);
            Console.WriteLine("\nSidewinder mechanism status:");
            Console.WriteLine("  Tape head at position {0}, capstan {1} in motion, activity light is {2}",
                              _position,
                              _tapeInMotion ? "IS" : "is not",
                              _activityLight ? "ON" : "off");
            Console.WriteLine("  Motion event {0} active", _motionEvent == null ? "is NOT" : "is");
            Console.WriteLine("  Transfer timings: MinSeek={0}ms 90IPS={1}ms MaxLBN={2}, EOM={3}",
                              Specs.MinimumSeek / 1000.0,
                              SecTime90IPS * Conversion.NsecToMsec,
                              _lastBlock, Geometry.TotalBlocks - 1);

            Console.WriteLine("\nBuffer status:");
            Console.WriteLine($"  Host buffer = {_hostBuffer}, Tape buffer = {_tapeBuffer}");

            for (var buf = 0; buf < _buffers.Length; buf++)
            {
                if (_buffers[buf] == null)
                {
                    Console.WriteLine($"  Buffer {buf} is undefined");
                    continue;
                }

                Console.Write($"  Buffer {buf} is type {_buffers[buf].Type}, {_buffers[buf].Pointer} bytes ");
                Console.WriteLine(_buffers[buf].Reading ?
                                    (_buffers[buf].Ready ? "(ready)" :
                                    (_buffers[buf].ReadComplete ? "(completed)" : "(reading)")) :
                                    (_buffers[buf].Empty ? "(empty)" :
                                    (_buffers[buf].Full ? "(full)" : "(writing)")));
            }
        }

        #endregion

        /// <summary>
        /// Drive/controller state.
        /// </summary>
        internal enum State
        {
            Unloaded = 0,       // No media in the drive
            Idle,               // Have a tape, no action
            Busy,               // Executing a command
            Fault,              // Something went wrong (like, tape yanked out?)
            Reset               // Powered on or reset (waiting for ReadStatus)
        }

        /// <summary>
        /// States required for the various command, status or data byte handshaking
        /// exchanges.  Request/Ready lines lines are used for command and status
        /// while Xfer/Ack are for data; there are up to 40 state transitions for a
        /// complex write sequence.  Zoinks.
        /// </summary>
        internal enum Phase
        {
            Ready = 0,          // Waiting for some action  T0
            Request,            // Start of handshake       T1,T2
            Response,           // Command incoming         T3,T4
            RequestAck,         // Request acknowledged     T5
            Accepted,           // Command received         T6,T7
            StatusReady,        // Ready to send status     T8-T10
            StatusAck,          // Host ready to receive    T11
            StatusSent,         // Status byte sent!        T12,T13
            StatusAccepted,     // Host ready for next one  T14
            DataReady,          // Drive ready to send/recv T9
            DataAck,            // Host/Drive provides byte T10,11
            DataSent,           // Drive/Host takes byte    T12,13
            DataAccepted,       // Ready for next one       T14,15
            DataPaused          // Waiting for a free buffer!
        }

        // Hardware signals            TO the host:
        private bool _ready;        // READY
        private bool _acknowledge;  // ACK
        private bool _exception;    // EXCEPTION
        private bool _fromHost;     // DIRC

        //                             FROM the host:
        private bool _online;       // ONLINE
        private bool _request;      // REQUEST
        private bool _xfer;         // XFER

        private byte _data;         // DATABUS <7:0>

        // Position as a logical block number
        private int _position;
        private int _lastBlock;

        // State of the mechanism
        private State _state;
        private Phase _phase;
        private Command _command;

        // Global conditions
        private bool _illegal;
        private bool _tapeInMotion;
        private bool _atFileMark;
        private bool _activityLight;

        // Status bytes from the drive itself
        private DriveStatus _status;

        // RAM buffers built into the controller
        private BlockBuffer[] _buffers;
        private int _hostBuffer;
        private int _tapeBuffer;

        // Linear access subject to the tyranny of temporality
        private Scheduler _scheduler;
        private SchedulerEvent _motionEvent;    // Mechanical timing
        private SchedulerEvent _protocolEvent;  // Protocol interactions

        // Each command sequence is a list of steps
        private Stack<SchedulerEventCallback> _commandSequence;

        // Some useful timings, precomputed (in nsec)
        private ulong SecTime30IPS = (ulong)(17.73 * Conversion.MsecToNsec);
        private ulong SecTime90IPS = (ulong)(5.91 * Conversion.MsecToNsec);

        // For Position commands, how many sectors whizz by in one second @ 90ips
        const int SectorsPerSecond = 169;
    }

    /// <summary>
    /// Gather up all the status bits and bobs.
    /// </summary>
    internal struct DriveStatus
    {
        public void Clear()
        {
            B0 = 0;
            B1 = 0;
            ErrorCount = 0;
            Underruns = 0;
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

        private int _nextByte;
    }

    /// <summary>
    /// Drive status bytes are internal to the Sidewinder's controller; the
    /// PERQ sends the status of the signal lines to the microcode!
    /// </summary>
    [Flags]
    internal enum StatusByte0
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
    internal enum StatusByte1
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

    /// <summary>
    /// Three MSBs (7:5) define the command issued to the drive.  These are sent
    /// with up to five significant bits (4:0) of command data.  Because only
    /// specific bit patterns are allowed, we just enumerate the legal ones and
    /// treat the rest as errors (they're supposed to be zeros).
    /// </summary>
    internal enum Command
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

}
