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
    public class Sidewinder : StorageDevice
    {
        public Sidewinder(Scheduler sched, string filename) : base(filename)
        {
            _scheduler = sched;
            _motionEvent = null;
            _protocolEvent = null;

            // If instantiated without a cartridge loaded!
            if (string.IsNullOrEmpty(filename))
            {
                Geometry = DeviceGeometry.NoMedia;
            }

            // Only one supported device profile
            Info = DeviceInfo.A3020;

            _buffers = new BlockBuffer[5];      // for testing
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
        private bool AtFileMark => _buffers[_tapeBuffer].Type == BlockType.FileMark;    // FIXME method needed

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
            _handshake = Phase.Ready;

            _position = 0;
            _tapeInMotion = false;

            // Set our outgoing signals according to the spec
            _ready = false;
            _acknowledge = true;
            _exception = true;
            _fromHost = true;

            if (_activityLight) ShowIcon(false);

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
            // Okay, THIS is insane but it makes the signals pop in logs
            Log.Detail(Category.Streamer, "Signals: {0} {1} {2} | {3} {4} {5} {6}",
                       _online ? "ONLINE" : "online",
                       _request ? "RQST" : "rqst",
                       _xfer ? "XFER" : "xfer",
                       _exception ? "EXCPT" : "excpt",
                       _ready ? "RDY" : "rdy",
                       _acknowledge ? "ACK" : "ack",
                       _fromHost ? "drive < HOST" : "DRIVE > host");

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
                                Console.WriteLine($"Signal change for {_command} in Busy state, not yet handled");
                                break;
                        }
                        break;

                    default:
                        throw new InvalidOperationException($"Unexpected state {_state} in CheckSignals");
                }
            }

            _state = nextState;
        }

        /// <summary>
        /// Check the status of incoming signal lines and perform the next step
        /// in the Request/Ready handshaking sequence to start a new command.
        /// Once accepted, return true until the command being executed completes
        /// and resets the state machine.
        /// </summary>
        private bool GetCommandByte()
        {
            if (_handshake >= Phase.Accepted) return true;

            ENTER("GetCmdByte");

            switch (_handshake)
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
                        _handshake = Phase.Request;

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

                        Log.Detail(Category.Streamer, "Phase now {0}, ready {1}; next change in {2}us",
                                   _handshake, _ready, delay);

                        // Schedule the transition
                        _protocolEvent = _scheduler.Schedule((ulong)delay * Conversion.UsecToNsec, (skewNsec, context) =>
                        {
                            _ready = !_ready;       // Toggle it (ReadStatus is wackbirds)
                            _handshake = Phase.Response;
                            Log.Detail(Category.Streamer, "Phase now {0}, ready now {1}", _handshake, _ready);
                        });
                    }
                    break;

                case Phase.Response:
                    if (!_request)
                    {
                        // Request line dropped, so now we have the ball
                        _handshake = Phase.RequestAck;
                        Log.Detail(Category.Streamer, "Phase now {0}, next change in 20us", _handshake);

                        // Minimum turnaround of 20usec before completing the handshake
                        _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, (skewNsec, context) =>
                        {
                            _ready = !_ready;               // Toggle it again!
                            _handshake = Phase.Accepted;    // Final stage of grief
                            Log.Detail(Category.Streamer, "Phase now {0}, ready now {1}", _handshake, _ready);

                            CheckSignals();                 // Cheeky!
                        });
                    }
                    break;

                case Phase.Request:
                case Phase.RequestAck:
                    // Shouldn't get anything from the host during these phases!
                    Log.Warn(Category.Streamer, "Unexpected status change in {0} phase", _handshake);
                    break;

                default:
                    Log.Warn(Category.Streamer, "Bad transition in handshake phase {0}", _handshake);
                    // So, uh, just reset and try again!?
                    _ready = false;
                    _exception = true;
                    _state = State.Fault;
                    _handshake = Phase.Ready;
                    break;
            }

            EXIT("GetCmdByte");
            return _handshake >= Phase.Accepted;
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
            ENTER("ReadStatus");

            // If coming out of Reset drop the ACK line and snapshot status
            if (_state == State.Reset)
            {
                _acknowledge = false;
                UpdateStatus();
            }

            switch (_handshake)
            {
                case Phase.Accepted:
                    // Set up for transfer: change direction signal
                    _fromHost = false;
                    _handshake = Phase.StatusReady;
                    Log.Detail(Category.Streamer, "ReadStatus: dir change, sending first byte in 20us");

                    // Schedule the next transition in 20usec
                    _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SendStatusByte);
                    return State.Busy;

                case Phase.StatusReady:
                case Phase.StatusAccepted:
                    // Shouldn't get any response from the microcode here?
                    Log.Debug(Category.Streamer, "ReadStatus: unexpected response in phase {0}", _handshake);
                    return State.Busy;

                case Phase.StatusSent:
                    // They've taken the byte so we just drop ready.  Not worth
                    // scheduling a 250ns delay here!
                    _ready = false;
                    _handshake = Phase.StatusAck;
                    return State.Busy;

                case Phase.StatusAck:
                    // Complete the handshake for the byte!
                    _handshake = Phase.StatusAccepted;

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
                        _status.Clear();    // Reset
                        _fromHost = true;   // Turn around the bus 
                        _ready = true;      // Ready for new command
                    });

                    // Fall through to finish the command
                    break;

                default:
                    Log.Error(Category.Streamer, "ReadStatus: impossible? Phase={0}", _handshake);
                    // Shouldn't ever happen; fall out to clean up if it does...
                    break;
            }

            // If we fall out, we're done!  Reset the status buffer
            _status.Clear();
            Log.Debug(Category.Streamer, "ReadStatus complete");

            return FinishCommand();
        }

        private void SendStatusByte(ulong skewNsec, object context)
        {
            // Put the next byte on the bus
            _data = _status.GetNextByte();
            _ready = true;
            _handshake = Phase.StatusSent;
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
                    // Add 'em in backwards
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
            ENTER("StartWriting");

            //
            // Initialize a new Write sequence
            //
            // These conditions are checked in GetCommandByte:
            //    ONLINE is true; tape IS loaded and is NOT write protected
            //    The motor should NOT be running...
            //
            // We don't know how many Write* commands will follow, but ALL write
            // sequences end with a final file mark and a rewind (the "End write
            // data" sequence).  Each of the Write* commands assumes that they
            // can exit when Online drops to complete the sequence.
            //
            _commandSequence.Push(Rewind);
            _commandSequence.Push(WriteFileMark);

            if (_command == Command.WriteData)
            {
                _commandSequence.Push(WriteData);
            }

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
            ENTER("ContinueWriting");

            var nextState = _state;

            switch (_handshake)
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
                case Phase.DataReady:
                    //
                    // T11 / T23 - At the beginning of a new block
                    //
                    if (!_online)
                    {
                        Log.Info(Category.Streamer, "Online dropped, terminating write sequence");

                        // Don't stop the tape; set up conditions to start next
                        // command and exit through the gift shop
                        _handshake = Phase.Ready;

                        // This will retire the current WriteData and allow for
                        // the host to inject another Write or WriteFileMark (or
                        // maybe even ReadStatus!?  Then continue the writing...
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

                        // Don't leak that buffer!
                        if (_buffers[_hostBuffer].Empty)
                        {
                            Log.Debug(Category.Streamer, "Releasing {0} buffer {1}",
                                       _buffers[_hostBuffer].Type, _hostBuffer);

                            // This is annoying, because the microcode does NOT
                            // have to issue a new WriteData command for every damn
                            // block -- it can just drop Xfer and continue!  Argh.
                            _buffers[_hostBuffer].SetType(BlockType.Empty);
                        }

                        _handshake = Phase.Ready;
                        GetCommandByte();
                    }
                    else if (_xfer)
                    {
                        // Start (or continue) receiving a data block!  The data
                        // byte is already latched; fire the Ack in .56 - 4.47 usec
                        // (let's round to 1 and see how that performs)

                        _handshake = Phase.DataAck;
                        _ready = false;

                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, (skewNsec, context) =>
                         {
                             _acknowledge = true;
                             _handshake = Phase.DataAccepted;
                         });
                    }
                    else
                    {
                        Log.Error(Category.Streamer, "Fell through in {0}, unknown signal change!?", _handshake);
                    }
                    break;

                case Phase.DataAccepted:
                    //
                    // T14-T16 - Byte received
                    //
                    // Save the byte to the current buffer and see where we're at...
                    // If the current block is full (512 bytes recieved) then we
                    // call Flush to make sure the mechanism is running (started
                    // after the first buffer fills, or restarted after underrun).
                    // 
                    _buffers[_hostBuffer].PutByte(_data);

                    if (_buffers[_hostBuffer].Full)
                    {
                        Log.Info(Category.Streamer, "Buffer {0} is full!", _hostBuffer);

                        Flush();

                        // Need a new buffer
                        _handshake = Phase.DataPaused;

                        // Reset Ack and ContinueWriting!  There's no response from
                        // the host after byte 512 so we force the next transition.  Hmm.
                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, (skewNsec, context) =>
                            {
                                _acknowledge = false;
                                CheckSignals();
                            });
                    }
                    else
                    {
                        // Continue filling the current one
                        _handshake = Phase.DataReady;

                        // Reset Ack (T14-T16 > .56us, < 1.12us); Xfer will reinitiate
                        _protocolEvent = _scheduler.Schedule(Conversion.UsecToNsec, (skewNsec, context) =>
                            {
                                _acknowledge = false;
                            });
                    }
                    break;

                case Phase.DataPaused:
                    //
                    // Here when we need to allocate a new buffer to accept the
                    // next block, either at T8-T9 or T22-T23.  If all three
                    // buffers are currently busy, we have to pause until the
                    // mechanism catches up and frees one.  The minimum gap is
                    // 100usec (T22-T23 time shown in the spec).  We'll loop
                    // every 100usec to see if the write-behind completed a block
                    // commit so we can tell the host to continue.  We can adjust
                    // the inter-byte ack response time to pace the dataflow if
                    // we consistently overrun to prevent the Slinky effect. :-)
                    //
                    if (GetNextBuffer(ref _hostBuffer))
                    {
                        // Got one!
                        Log.Info(Category.Streamer, "Next host write buffer is {0}", _hostBuffer);

                        // Let the command which was paused know they can proceed
                        if (_command == Command.WriteFileMark)
                            WriteFileMark(0, null);
                        else
                            WriteData(0, null);
                    }
                    else
                    {
                        // Wait one sector time to see if that's reasonable
                        var retry = Settings.Performance.HasFlag(RateLimit.TapeSpeed) ?
                                            ((ulong)Specs.MinimumSeek * Conversion.UsecToNsec) :
                                            SecTime90IPS;

                        Log.Debug(Category.Streamer, "Waiting for free buffer; retry in {0}ms...",
                                  retry * Conversion.NsecToMsec);

                        // Debug
                        if (!_tapeInMotion && _motionEvent == null) Flush();    // Kick the drive

                        // Come around, idiot, come around
                        _protocolEvent = _scheduler.Schedule(retry, (skewNsec, context) =>
                        {
                            CheckSignals();
                        });
                    }
                    break;

                default:
                    Console.WriteLine("wut");
                    break;
            }

            return nextState;
        }


        private void WriteData(ulong skewNsec, object context)
        {
            ENTER("WriteData");

            if (_handshake == Phase.Accepted)
            {
                // 
                // Starting or continuing a write:  jump to the "paused" state
                // to allocate a new buffer.  That'll come back here when one is
                // available (handling any pauses if the mechanism is behind).
                // 
                _ready = false;
                _state = State.Busy;
                _handshake = Phase.DataPaused;
                CheckSignals();
            }
            else if (_handshake == Phase.DataPaused)
            {
                //
                // We got a buffer!  Set it up for data and jump into the data
                // transfer loop.
                //
                _buffers[_hostBuffer].SetType(BlockType.Data);
                _buffers[_hostBuffer].Clear();

                // First block (T8->T9 transition) is 20 usec; if there were
                // delays acquiring the buffer it may have taken longer (that's ok)
                _handshake = Phase.DataReady;
                _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SetReady);
            }
            else
            {
                Log.Error(Category.Streamer, "WriteData entered in phase {0}?", _handshake);
            }

            EXIT("WriteData");
        }

        private void WriteFileMark(ulong skewNsec, object context)
        {
            ENTER("WriteFM");

            if (_handshake == Phase.Accepted)
            {
                //
                // Just like WriteData, first entry requires a new buffer.  The
                // hardware might just fake it, but for simplicity I require it
                // so the blocks are serialized in order.  Drop the Ready line
                // to show we're busy then jump to DataPaused to get a block.
                //
                _ready = false;
                _state = State.Busy;
                _handshake = Phase.DataPaused;
                CheckSignals();
            }
            else if (_handshake == Phase.DataPaused)
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

                // Commit that puppy
                Flush();

                // The logic at ContinueWriting will handle our exit/continuation.
                // This is kinda janky, but the interface is kind of nutty.
                _handshake = Phase.DataReady;
                _protocolEvent = _scheduler.Schedule(20 * Conversion.UsecToNsec, SetReady);
            }
            else
            {
                Log.Error(Category.Streamer, "WriteFileMark entered in phase {0}?", _handshake);
            }

            EXIT("WriteFM");
        }

        private void SetReady(ulong skewNsec, object context)
        {
            _ready = true;              // Signal the host we're ready to go!
        }


        private void Flush()
        {
            // We have a buffer to write, and we're not already in motion, fire it up
            if (DirtyBuffers() > 0 && !_tapeInMotion && _motionEvent == null)
            {
                Log.Info(Category.Streamer, "Cain't flush if the dang thing ain't runnin'");
                StartStop(WriteBehind);
            }
        }

        private void WriteBehind(ulong skewNsec, object context)
        {
            ENTER("FEEDME");

            // Get the next dirty, dirty buffer
            if (!GetDirtyBuffer(ref _tapeBuffer))
            {
                Log.Info(Category.Streamer, "Underrun at time {0}, no buffer ready for writing",
                          _scheduler.CurrentTimeNsec);

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

                // Clear it
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

                if (AtEndOfMedia)
                {
                    // Here we have to jam on the brakes and halt the whole write op
                    Console.WriteLine("\nAt EOM!  Stop the world, we have to get off!\n");
                    StartStop(null);
                    _commandSequence.Clear();
                    _exception = true;
                    _handshake = Phase.Ready;       // ????
                    FinishCommand();                // !!!!
                    return;
                }

                // If Online drops, stop motion?
                if (!Online)
                {
                    Log.Write(Category.Streamer, "Controller reports offline, spinning down tape?");
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

            EXIT("FEEDME");
        }

        #endregion

        #region Read commands

        private State StartReading()
        {
            ENTER("StartReading");

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
            ENTER("ContinueReading");
            return State.Busy;
        }

        private void ReadData(ulong skewNsec, object context)
        {
            ENTER("ReadData");
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
            ENTER("ReadAhead");

            // Preloads data buffers as long as we're in a read sequence
            // goes until it hits a FM and raises an exception, leaves the
            // tape where it's at (motor stopped)
            // opposite of FEEDME
        }

        #endregion

        #region Buffer management

        private void InitBuffers()
        {
            // Zero them out, reset the pointer(s)
            for (var b = 0; b < _buffers.Length; b++)
            {
                _buffers[b] = new BlockBuffer(BlockType.Empty);
            }
            _hostBuffer = 0;
            _tapeBuffer = 0;

            Log.Info(Category.Streamer, "Buffers initialized");
        }

        private void ResetBuffers()
        {
            foreach (var b in _buffers)
            {
                b.Clear();
                b.SetType(BlockType.Empty);
            }
            _hostBuffer = 0;
            _tapeBuffer = 0;

            Log.Info(Category.Streamer, "Buffers reset");
        }

        /// <summary>
        /// Get the next empty buffer.  Return false if none currently available.
        /// </summary>
        private bool GetNextBuffer(ref int which)
        {
            for (var i = which; i < which + _buffers.Length; i++)
            {
                var nextBuf = i % _buffers.Length;

                // Be pedantic, make sure the buffer is properly reset (byte counter too)
                if (_buffers[nextBuf].Type == BlockType.Empty && _buffers[nextBuf].Empty)
                {
                    Log.Debug(Category.Streamer, "GetNextBuf: Buffer {0} is empty", nextBuf);
                    which = nextBuf;
                    return true;
                }
            }

            Log.Info(Category.Streamer, "GetNextBuf: No free buffers!");
            return false;
        }

        /// <summary>
        /// Update the index and return true if any unwritten buffers remain.
        /// </summary>
        private bool GetDirtyBuffer(ref int which)
        {
            for (var i = which; i < which + _buffers.Length; i++)
            {
                var nextBuf = i % _buffers.Length;

                // Don't snag any partially written ones!
                if (_buffers[nextBuf].Type != BlockType.Empty && _buffers[nextBuf].Full)
                {
                    Log.Debug(Category.Streamer, "GetDirty: Buffer {0} is fully dirty :-]", nextBuf);
                    which = nextBuf;
                    return true;
                }
            }

            Log.Info(Category.Streamer, "GetDirty: No dirty buffers remaining");
            return false;
        }

        /// <summary>
        /// Just return the count of uncommitted buffers.
        /// </summary>
        private int DirtyBuffers()
        {
            var count = 0;

            foreach (var b in _buffers)
            {
                if (b.Type != BlockType.Empty && b.Full) count++;
            }

            return count;
        }

        /// <summary>
        /// Commit a buffer to the underlying media, translating the linear tape
        /// position to C/H/S.  Sets the sector header from the BlockType.
        /// </summary>
        private void WriteBuffer(int index, int pos)
        {
            var hd = (byte)(pos / Geometry.Sectors);
            var sec = (ushort)(pos % Geometry.Sectors);
            var bad = _buffers[index].Type == BlockType.BadData;

            Log.Info(Category.Streamer, "WriteBuf: Writing {0} buffer {1} @ {2} to sector 0/{3}/{4}",
                      _buffers[index].Type, index, pos, hd, sec);

            // This seems like the long way 'round but, build up a new sector,
            // copy in the data, set the header bytes, and flush it out.  Not
            // exactly world beating efficiency here.  Clean it up "someday" :-)
            var sector = new Sector(0, hd, sec, Geometry.SectorSize, Geometry.HeaderSize, bad);
            _buffers[index].Data.CopyTo(sector.Data, 0);
            SetBlockType(sector, _buffers[index].Type);
            Write(sector);
        }

        private void SetBlockType(Sector sec, BlockType type)
        {
            var val = (uint)type;

            sec.WriteHeaderByte(0, (byte)(val >> 24));
            sec.WriteHeaderByte(1, (byte)(val >> 16));
            sec.WriteHeaderByte(2, (byte)(val >> 8));
            sec.WriteHeaderByte(3, (byte)val);

            Log.Debug(Category.Streamer, "Wrote header mark {0} to sector 0/{1}/{2}",
                       type, sec.HeadID, sec.SectorID);
        }

        private BlockType GetBlockType(Sector sec)
        {
            var type = (uint)((sec.ReadHeaderByte(0) << 24) |
                              (sec.ReadHeaderByte(1) << 16) |
                              (sec.ReadHeaderByte(2) << 8) |
                              (sec.ReadHeaderByte(3)));

            Log.Debug(Category.Streamer, "Read header mark {0} from sector {1}/{2}/{3}",
                       (BlockType)type, sec.CylinderID, sec.HeadID, sec.SectorID);

            return (BlockType)type;
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
            Log.Info(Category.Streamer, "FinishCommand: {0} left in queue, state is {1}",    // debug
                       _commandSequence.Count, _state);

            _commandSequence.Clear();
            UpdateStatus();

            if (_activityLight) ShowIcon(false);

            // Reset state machine to accept next command
            _handshake = Phase.Ready;

            // Return to idle, but check for abnormal conditions...
            State final;

            if (!IsLoaded)
            {
                final = State.Unloaded;     // Always overrides
            }
            else if (_illegal)
            {
                final = State.Fault;        // Illegal command
            }
            else if (_exception)
            {
                // If we're in Reset, remain there; otherwise Fault
                // (This affects the setting of a specific bit in B1)
                final = (_state == State.Reset ? State.Reset : State.Fault);
            }
            else
            {
                final = State.Idle;
            }

            Log.Write(Category.Streamer, "FinishCommand: queue clear, state is {0}\n", final);    // debug
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
                          (AtEndOfMedia ? StatusByte0.EndOfMedia : 0) |
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

                // Toggle the motor state and update the icon now (doh)
                _tapeInMotion = !_tapeInMotion;
                ShowIcon(_tapeInMotion);
            }
            else
            {
                // Since there's nothin' else to do, actually do the update AFTER the delay
                _motionEvent = _scheduler.Schedule((ulong)delay * Conversion.MsecToNsec, (skewNsec, context) =>
                {
                    _motionEvent = null;
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
            // progress.  Can consule the TapeSpeed rate limit setting to decide
            // how realistic to make things. :-)

            if (!_tapeInMotion)
            {
                if (AtEndOfTape)
                {
                    if (_command == Command.Erase)
                    {
                        // At the end of an erase, set the "end of media" marker
                        // in the block header.
                        var sec = Read(0, (byte)(Geometry.Heads - 1), (ushort)(Geometry.Sectors - 1));
                        SetBlockType(sec, BlockType.EndOfMedia);
                        Write(sec);

                        // Housekeeping
                        _lastBlock = Geometry.TotalBlocks - 3;  // Reset
                        Log.Write(Category.Streamer, "Erase: Last block is now {0}", _lastBlock);
                    }

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
                                var sec = new Sector(0, (byte)h, (ushort)s,
                                                      Geometry.SectorSize,
                                                      Geometry.HeaderSize);

                                SetBlockType(sec, BlockType.Empty);
                                Write(sec);
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

            // Premature ejectulation?
            _exception |= (_state != State.Idle);

            FinishCommand();

            Geometry = DeviceGeometry.NoMedia;
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


        #region Debugging

        private void ENTER(string rtn)
        {
            Log.Debug(Category.Streamer, "{0}: IN  {1}, {2} seq={3}, motor={4} | {5} {6} {7} | {8} {9}",
                      rtn, _state, _handshake, _commandSequence.Count, _tapeInMotion,
                      _online ? "ONLINE" : "",
                      _request ? "RQST" : "",
                      _xfer ? "XFER" : "",
                      _ready ? "RDY" : "",
                      _acknowledge ? "ACK" : "");
        }

        private void EXIT(string rtn)
        {
            Log.Debug(Category.Streamer, "{0}: OUT {1}, {2} seq={3}, motor={4} | {5} {6} {7} | {8} {9}",
                      rtn, _state, _handshake, _commandSequence.Count, _tapeInMotion,
                      _online ? "ONLINE" : "",
                      _request ? "RQST" : "",
                      _xfer ? "XFER" : "",
                      _ready ? "RDY" : "",
                      _acknowledge ? "ACK" : "");
        }

        public void DumpState()
        {
            Console.WriteLine("Sidewinder controller status:");
            Console.WriteLine("  State={0} Handshake={1} Command={2} Data={3}",
                               _state, _handshake, _command, _data);
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
            Console.WriteLine("  Transfer timings: 30IPS={0} 90IPS={1} MaxLBN={2}, EOM={3}",
                          SecTime30IPS, SecTime90IPS, _lastBlock - 2, _lastBlock);
            Console.WriteLine("  LOADED DRIVE MIN SEEK: {0}usec (raw), {1}nsec, {2}msec",
                              Specs.MinimumSeek,
                              (ulong)Specs.MinimumSeek * Conversion.UsecToNsec,
                              ((ulong)Specs.MinimumSeek * Conversion.UsecToNsec) * Conversion.NsecToMsec);
            Console.WriteLine("\nBuffer status:");
            Console.WriteLine($"  Host buffer = {_hostBuffer}, Tape buffer = {_tapeBuffer}");

            for (var buf = 0; buf < _buffers.Length; buf++)
            {
                if (_buffers[buf] == null)
                {
                    Console.WriteLine($"  Buffer {buf} is undefined");
                    continue;
                }

                Console.WriteLine($"  Buffer {buf} is type {_buffers[buf].Type}, {_buffers[buf].Pointer} bytes");
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
        private Phase _handshake;
        private Command _command;

        // Global conditions
        private bool _illegal;
        private bool _tapeInMotion;
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

    /// <summary>
    /// Sector header bytes interpreted as block types, which look suspiciously
    /// like .TAP markers, but I'm sure that's purely coincidental.  Still, you
    /// should make sure these match the TAPFormatter's Marker types, maybe?
    /// </summary>
    internal enum BlockType : uint
    {
        FileMark = 0x00000000,
        Data = 0x00000200,          // Good data (512 byte block)
        Empty = 0x10000200,         // Internal use; not written to tape
        BadData = 0x80000100,       // Bad flag (512 bytes present)
        EraseGap = 0xfffffffe,      // Not used by PERQemu/PERQmedia
        EndOfMedia = 0xffffffff     // Logical end of media
    }

    /// <summary>
    /// Block buffer on the drive's controller.  The Sidewinder has three fixed
    /// 512-byte buffers that it uses to stream blocks to and from the media;
    /// we supplement that with a type code (derived from the SectorHeader data)
    /// and keep a read/write pointer to simplify the byte-by-byte access that
    /// the microcode does (there's no DMA to or from the streamer).
    /// </summary>
    internal class BlockBuffer
    {
        public BlockBuffer(BlockType type)
        {
            _type = type;
            _data = new byte[512];
            _currentByte = 0;
        }

        /// <summary>
        /// Copy a data Sector from the media into a new block and set its type.
        /// </summary>
        public BlockBuffer(Sector fromSector)
        {
            // Blargh.  Add an extension method to Sector for translating
            // header bytes <=> BlockTypes?  Meh.
            var marker = (uint)((fromSector.ReadHeaderByte(0) << 24) |
                                (fromSector.ReadHeaderByte(1) << 16) |
                                (fromSector.ReadHeaderByte(2) << 8) |
                                (fromSector.ReadHeaderByte(3)));

            _type = (BlockType)marker;
            _data = new byte[512];
            fromSector.Data.CopyTo(_data, 0);
            _currentByte = 0;
        }

        public BlockType Type => _type;

        public byte[] Data => _data;
        public int Pointer => _currentByte;         // debug output

        public bool Full => _currentByte == _data.Length;
        public bool Empty => _currentByte == 0;


        public void Clear()
        {
            for (_currentByte = 0; _currentByte < _data.Length; _currentByte++)
            {
                _data[_currentByte] = 0;
            }

            _currentByte = 0;
        }

        public byte GetByte()
        {
            if (_currentByte < _data.Length)
            {
                return _data[_currentByte++];
            }

            return 0;
        }

        public void PutByte(byte value)
        {
            if (_currentByte < _data.Length)
            {
                _data[_currentByte++] = value;
            }
        }

        public void SetType(BlockType t)
        {
            _type = t;
        }

        BlockType _type;

        byte[] _data;
        int _currentByte;
    }
}
