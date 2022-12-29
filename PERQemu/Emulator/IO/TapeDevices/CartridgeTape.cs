//
// CartridgeTape.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.TapeDevices
{
    /// <summary>
    /// Emulates the mechanical operations of a QIC tape drive.  For the PERQ
    /// this means the Archive 'Sidewinder' 3020B, 9020B or 9045B basic drives.
    /// </summary>
    public class CartridgeTape : StorageDevice
    {
        /// <summary>
        /// Initialize the drive with the baseline specs.
        /// </summary>
        public CartridgeTape(Sidewinder control, Scheduler sched, string filename) : base(filename)
        {
            _controller = control;
            _scheduler = sched;
            _motionEvent = null;
            _command = Command.None;

            // If instantiated without a cartridge loaded!
            if (string.IsNullOrEmpty(filename))
            {
                // Basic device profile
                Info = DeviceInfo.A3020;
                Geometry = DeviceGeometry.NoMedia;
                Specs = DevicePerformance.Archive30IPS;
            }

            _activityLight = false;
        }

        // Things the controller wants to know
        public bool AtBOT => _position == 0;
        public bool AtEOT => _position == Geometry.Sectors - 1;
        public bool AtEOM => _position == Geometry.TotalBlocks - 1;
        public bool AtLogicalEOM => _position >= _lastBlock;

        public bool AtFileMark => _atFileMark;
        public bool TapeInMotion => _tapeInMotion;
        public bool ActivityLight => _activityLight;

        // One sector of tape travel (accounting for rate limit prefs)
        public ulong SeekTime => Settings.Performance.HasFlag(RateLimit.TapeSpeed) ?
                                ((ulong)Specs.MinimumSeek * Conversion.UsecToNsec) : SecTime90IPS;

        // Signals we need to snoop
        bool Offline => !_controller.Online;

        /// <summary>
        /// Reset the drive.
        /// </summary>
        public void Reset()
        {
            if (_motionEvent != null)
            {
                _scheduler.Cancel(_motionEvent);
                _motionEvent = null;
            }

            if (_activityLight) ShowIcon(false);

            _position = 0;
            _atFileMark = false;
            _tapeInMotion = false;

            Log.Info(Category.Streamer, "Drive reset");
        }

        #region Drive actions

        /// <summary>
        /// Schedule an event to start or stop the capstan motor.  Specs.StartupDelay
        /// is the number of milliseconds to delay.  The _tapeInMotion flag is toggled.
        /// </summary>
        void StartStop(SchedulerEventCallback nextStep)
        {
            // For accurate tape start/stop times, consult the Specs; otherwise
            // shorten it to a fixed 10ms (about 10x faster than typical)
            var delay = (Settings.Performance.HasFlag(RateLimit.TapeSpeed) ? Specs.StartupDelay : 10);

            Log.Debug(Category.Streamer, "Motor {0} in {1}ms",
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
        public void Rewind(ulong skewNsec, object context)
        {
            Log.Info(Category.Streamer, "Rewind: tape is {0}, position {1}",
                       (_tapeInMotion ? "moving" : "stopped"), _position);

            // Rewinding is linear, but position counts logical blocks (over n
            // tracks).  If rewinding from an arbitrary position, mod the block
            // number and only count down from there.
            if (_position >= Geometry.Sectors)
            {
                Log.Debug(Category.Streamer, "Rewind: Adjusting linear position {0} => {1}",
                           _position, _position % (Geometry.Sectors - 1));

                _position = _position % (Geometry.Sectors - 1);
            }

            if (!_tapeInMotion)
            {
                if (AtBOT)
                {
                    _atFileMark = false;
                    _motionEvent = null;        // Clear the movement event
                    _controller.RunSequence();  // Return through the dispatcher
                }
                else
                {
                    StartStop(Rewind);          // Start up, call back to count down
                }
            }
            else
            {
                if (AtBOT)
                {
                    StartStop(Rewind);          // Stop, and call back to finish command
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
        /// Start an Erase or Retension command in the dumbest way because using
        /// the context object in this way seems to be impossibly complicated or
        /// impossible and my brain has turned to jelly.  Just set the _command
        /// and call Wind to do the work.  There must be a a less gross way but
        /// I am too tired to find it.  Sigh.
        /// </summary>
        public void Erase(ulong skewNsec, object context)
        {
            _command = Command.Erase;
            Wind(skewNsec, context);
        }

        public void Retension(ulong skewNsec, object context)
        {
            _command = Command.Retension;
            Wind(skewNsec, context);
        }

        /// <summary>
        /// Wind the tape forward to EOT.  If the command is Erase, then zaps all
        /// the sector data along the way.  Assumes the tape has been rewound;
        /// finishes at position EOT (last sector on track 0) with the motor off.
        /// Blinks the icon along the way. :-)
        /// </summary>
        public void Wind(ulong skewNsec, object context)
        {
            Log.Info(Category.Streamer, "{0}: tape is {1}, position {2}",
                       _command, (_tapeInMotion ? "moving" : "stopped"), _position);

            // Like rewinding, this is a linear motion from BOT to EOT, but it
            // shouldn't ever start in the middle of the tape; assume we start
            // at zero and do one full pass.  Rather than call Format() in one
            // go (would make the emulator seriously hiccup?) we'll do it a few
            // blocks at a time, which lets us blink the activity light to show
            // progress.
            if (!_tapeInMotion)
            {
                if (AtEOT)
                {
                    _atFileMark = false;
                    _motionEvent = null;
                    _command = Command.None;
                    _controller.RunSequence();
                }
                else
                {
                    StartStop(Wind);        // Start the motor, call back to start counting
                }
            }
            else
            {
                // Are we there yet?
                if (AtEOT)
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
                        var sec = new BlockBuffer(BlockType.Empty);

                        for (var h = 0; h < Geometry.Heads; h++)
                        {
                            // OMFG. If doing the last chunk we either fail to write
                            // the last column OR we double-write the first position
                            // every time through.  Fencepost errors are SO ANNOYING
                            var killMeNow = (_position + blocks == Geometry.Sectors - 1 ? 1 : 0);

                            for (var s = _position; s < _position + blocks + killMeNow; s++)
                            {
                                Write(s + (h * Geometry.Sectors), sec);
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

        #region Streaming writes

        /// <summary>
        /// If there are blocks to commit to tape, make sure the the motor is
        /// running and the "write behind thread" is scheduled.
        /// </summary>
        public void Flush()
        {
            // Have a buffer to write, and not already in motion: fire it up
            if (_controller.WritesPending && !_tapeInMotion)
            {
                StartStop(WriteBehind);
            }
        }

        /// <summary>
        /// Stream blocks to the tape.  Loops continuously as long as the controller
        /// keeps the queue fed, or until we write a file mark or reach the EOM.
        /// Automatically exits if the host signals that it's offline.
        /// </summary>
        void WriteBehind(ulong skewNsec, object context)
        {
            // Get the next dirty, dirty buffer
            if (!_controller.WritesPending)
            {
                Log.Debug(Category.Streamer, "Writer: Underrun! No full buffers ready to write");
                _controller.Underrun();

                // Next buffer completion will call Flush to restart
                StartStop(null);
                return;
            }

            // If we stopped on a file mark, advance to the next block
            if (_atFileMark)
            {
                _atFileMark = false;
                _position++;
            }

            if (AtLogicalEOM)
            {
                // At the warning track; stop tape, set exception, require a Read
                // Status to continue.  Jump back to DataReady for the last Write/
                // WriteFM/offline processing.  Except that Stut doesn't do any of
                // that.  It hits the hard limit and immediately gives up?   Sigh.
                Log.Warn(Category.Streamer, "Writer: Reached the warning track, tape is almost full.");
            }

            if (AtEOM)
            {
                // Here we have to jam on the brakes and halt the whole write op
                Log.Error(Category.Streamer, "Writer: Reached end of physical media, tape is full.  Hard stop.");

                // Clear buffers so the controller doesn't get stuck; we're out
                // of room, so just dump the data overboard
                while (_controller.WritesPending) _controller.GetBuffer();

                StartStop(null);
                _controller.DriveFault();
                return;
            }

            // Grab the next buffer off the queue
            var block = _controller.GetBuffer();

            Log.Detail(Category.Streamer, "Writer: {0} buffer is ready to flush @ pos {1}",
                                        block.Type, _position);

            // Commit the block!
            Write(_position, block);

            // Was it a file mark?  Save for status...
            _atFileMark = (block.Type == BlockType.FileMark);

            // If we're on track 0, the erase bar is zapping the other tracks!
            if (_position < Geometry.Sectors)
            {
                for (var trk = 1; trk < Geometry.Heads; trk++)
                {
                    Write(_position + (trk * Geometry.Sectors), new BlockBuffer(BlockType.Empty));
                }
            }

            if (_atFileMark)
            {
                // If we just wrote a file mark, spin down the tape with a call-
                // back to WriteFileMark to let it know.  It's not 100% clear
                // from the spec if the tape _always_ stops when a WFM is issued...
                Log.Debug(Category.Streamer, "Wrote a file mark!");

                StartStop(_controller.WriteFileMark);
                return;
            }

            // Advance position and check for end of the media
            _position++;

            // If Online drops, stop motion?
            if (Offline)
            {
                Log.Debug(Category.Streamer, "Writer: Controller reports offline, stopping tape.");
                StartStop(null);
                return;
            }

            // Skip this if we aren't rate limiting and save hundreds milliseconds! :-)
            if (_position % Geometry.Sectors == 0 && Settings.Performance.HasFlag(RateLimit.TapeSpeed))
            {
                // Stop at the end of each full pass over the tape to reverse
                // direction.  Flush will start us back up.  This would be more
                // useful if we swapped the activity icon to show the tape wound
                // on the opposite spool, which would be subtle but sexy. :-)
                Log.Debug(Category.Streamer, "Writer: At EOT, have to change heads and direction!");
                StartStop(null);
            }
            else
            {
                // Keep on streamin'
                _motionEvent = _scheduler.Schedule(SeekTime, WriteBehind);
                Log.Detail(Category.Streamer, "Writer: Next tape block in {0}ms", SeekTime * Conversion.NsecToMsec);
            }
        }

        /// <summary>
        /// Write a buffer to the underlying device, computing C/H/S from the
        /// linear tape position.
        /// </summary>
        public void Write(int pos, BlockBuffer buf)
        {
            var hd = (byte)(pos / Geometry.Sectors);
            var sec = (ushort)(pos % Geometry.Sectors);

            var val = (uint)buf.Type;

            // Backstop: never overwrite the EOM marker!
            if (pos == Geometry.TotalBlocks - 1)
            {
                Log.Debug(Category.Streamer, "Writing EOM record at pos {0}", pos);
                val = (uint)BlockType.EndOfMedia;
            }

            Sectors[0, hd, sec].WriteHeaderByte(0, (byte)(val >> 24));
            Sectors[0, hd, sec].WriteHeaderByte(1, (byte)(val >> 16));
            Sectors[0, hd, sec].WriteHeaderByte(2, (byte)(val >> 8));
            Sectors[0, hd, sec].WriteHeaderByte(3, (byte)val);

            buf.Data.CopyTo(Sectors[0, hd, sec].Data, 0);

            IsModified = true;

            Log.Detail(Category.Streamer, "Wrote @ pos {0} to sector 0/{1}/{2}, type {3}",
                                        pos, hd, sec, buf.Type);
        }

        #endregion

        #region Streaming reads

        /// <summary>
        /// If the controller wants blocks and has buffer space available, make
        /// sure the motor's runnin' and the "read ahead thread" is active.
        /// </summary>
        public void Fetch()
        {
            if (_controller.BufferAvailable && !_tapeInMotion)
            {
                StartStop(ReadAhead);
            }
        }

        /// <summary>
        /// Stream reads from the tape to the host.  Attempts to read and buffer
        /// the next tape block every n msec (based on drive speed).  Stops when
        /// there are no free buffers, a file mark or end of media is reached, or
        /// the controller indicates an offline condition.
        /// </summary>
        void ReadAhead(ulong skewNsec, object context)
        {
            // If the controller reports that Online has dropped, just quietly exit
            if (Offline)
            {
                Log.Debug(Category.Streamer, "Reader: Controller reports offline, stopping tape.");
                StartStop(null);
                return;
            }

            // If we stopped on a file mark, reset and start at the next block
            if (_atFileMark)
            {
                _position++;
                _atFileMark = false;
            }

            // Have we reached the end?
            if (AtEOM)
            {
                Log.Error(Category.Streamer, "Reader: At end of media, hard stop.");
                StartStop(null);
                _controller.DriveFault();
                return;
            }

            // Make sure we have a buffer to read into
            if (!_controller.BufferAvailable)
            {
                Log.Debug(Category.Streamer, "Reader: Underrun! No free read buffers available");
                _controller.Underrun();
                StartStop(null);

                // The host is busy; exit and let them restart us with Fetch()
                return;
            }

            // Read the block at the current position
            var block = Read(_position);

            // Is this block a file mark?  Flag it and stop the tape; when the
            // host catches up the protocol will handle the exception.  We do
            // NOT advance the position pointer here!
            if (block.Type == BlockType.FileMark)
            {
                _controller.PutBuffer(block);
                _atFileMark = true;
                StartStop(null);
                return;
            }

            if (block.Type == BlockType.Empty || block.Type == BlockType.EraseGap)
            {
                if (_controller.NoDataError())
                {
                    // Thirty two empty blocks in a row constitutes an Unrecoverable
                    // Data Error, which could happen when trying to read an empty
                    // or unformatted tape.  We set an exception and halt
                    _controller.DriveFault();
                    StartStop(null);
                    return;
                }
            }
            else
            {
                // Otherwise, it's data; ship it!
                _controller.PutBuffer(block);
            }

            // Advance the pointer and reschedule the next read
            _position++;

            // Skip this if we aren't rate limiting and save hundreds milliseconds! :-)
            if (_position % Geometry.Sectors == 0 && Settings.Performance.HasFlag(RateLimit.TapeSpeed))
            {
                // Stop at the end of each full pass over the tape to reverse
                // direction.  Fetch will start us back up.  This would be more
                // useful if we swapped the activity icon to show the tape wound
                // on the opposite spool, which would be subtle but sexy. :-)
                Log.Debug(Category.Streamer, "Reader: At EOT, have to change heads and direction!");
                StartStop(null);
            }
            else
            {
                // Keep on keepin' on
                _motionEvent = _scheduler.Schedule(SeekTime, ReadAhead);
                Log.Detail(Category.Streamer, "Reader: Next tape block in {0}ms", SeekTime * Conversion.NsecToMsec);
            }
        }

        /// <summary>
        /// Read a device sector into a block buffer, computing C/H/S from the
        /// linear tape position.
        /// </summary>
        public BlockBuffer Read(int pos)
        {
            var hd = (byte)(pos / Geometry.Sectors);
            var sec = (ushort)(pos % Geometry.Sectors);

            var marker = (uint)((Sectors[0, hd, sec].ReadHeaderByte(0) << 24) |
                                (Sectors[0, hd, sec].ReadHeaderByte(1) << 16) |
                                (Sectors[0, hd, sec].ReadHeaderByte(2) << 8) |
                                (Sectors[0, hd, sec].ReadHeaderByte(3)));

            var block = new BlockBuffer((BlockType)marker);
            Sectors[0, hd, sec].Data.CopyTo(block.Data, 0);

            Log.Detail(Category.Streamer, "Reading @ pos {0} from sector 0/{1}/{2}, type {3}",
                                        pos, hd, sec, (BlockType)marker);
            return block;
        }

        #endregion

        #region Load, Unload and The Blinkenlight

        /// <summary>
        /// On load, check and set some flags 'n stuff.
        /// </summary>
        public override void OnLoad()
        {
            base.OnLoad();

            // It's the only way to be sure
            Reset();

            // Set the start of the "warning track"
            _lastBlock = Geometry.TotalBlocks - Sidewinder.MAXBUFFERS;

            Log.Info(Category.Streamer, "{0} online", Info.Description);

            // Give the controller a heads up!
            _controller.Load();
        }

        public override void Unload()
        {
            base.Unload();

            Log.Info(Category.Streamer, "Tape cartridge is about to eject...");

            if (_motionEvent != null) _scheduler.Cancel(_motionEvent);
            _tapeInMotion = false;
            _atFileMark = false;

            // Reset to baseline
            Info = DeviceInfo.A3020;
            Geometry = DeviceGeometry.NoMedia;
            Specs = DevicePerformance.Archive30IPS;

            // Tell the boss
            _controller.Unload();
        }

        /// <summary>
        /// Set our icon state to show drive activity.
        /// </summary>
        public void ShowIcon(bool state)
        {
            PERQemu.Sys.MachineStateChange(WhatChanged.StreamerActivity, state);
            _activityLight = state;
        }

        #endregion

        public void DumpState()
        {
            Console.WriteLine("\nSidewinder mechanism status:");
            Console.WriteLine("  Tape head at position {0}, capstan {1} in motion, activity light is {2}",
                              _position,
                              _tapeInMotion ? "IS" : "is not",
                              _activityLight ? "ON" : "off");
            Console.WriteLine("  Motion event {0} active", _motionEvent == null ? "is NOT" : "is");
            Console.WriteLine("  Transfer timings: MinSeek={0}ms 90IPS={1}ms MaxLBN={2}, EOM={3}",
                              SeekTime * Conversion.NsecToMsec,
                              SecTime90IPS * Conversion.NsecToMsec,
                              _lastBlock, Geometry.TotalBlocks - 1);
        }

        // Current drive command
        Command _command;

        bool _atFileMark;
        bool _tapeInMotion;
        bool _activityLight;

        // Position as a logical block number
        int _position;
        int _lastBlock;

        // Some useful timings, precomputed (in nsec)
        public ulong SecTime30IPS = (ulong)(17.73 * Conversion.MsecToNsec);
        public ulong SecTime90IPS = (ulong)(5.91 * Conversion.MsecToNsec);

        // For Position commands, how many sectors whizz by in one second @ 90ips
        public const int SectorsPerSecond = 169;

        // Backchannel for raising alerts/exceptions
        Sidewinder _controller;

        // For timing mechanical events
        Scheduler _scheduler;
        SchedulerEvent _motionEvent;
    }
}
