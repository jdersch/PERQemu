//
// RSXFilePort.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.IO;
using System.Collections.Generic;

using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// FilePort provides logic that emulates an RSX-11 host connected to the
    /// PERQ's simulated serial port.
    /// </summary>
    /// <remarks>
    /// In doing so, it provides a crude way to copy a file from the host to the
    /// emulated PERQ.  Under POS, one can do:
    /// 
    ///     "copy RSX:remoteFileName localFileName"
    /// 
    /// and it will copy incoming serial data to a file on the PERQ.
    /// 
    /// Conversely, one can do a "copy localFileName RSX:remoteFileName" to copy
    /// a PERQ file to the serial port.
    /// 
    /// This class acts as a serial sink/source and either provides a datastream
    /// from a local file (on the host) or writes an incoming stream to a local
    /// file.  This is a pretty nifty way to allow the PERQ limited access to the
    /// filesystem on the emulation host.  (Alas, no directory support...)
    /// 
    /// Unfortunately it only supports text files.
    ///         
    /// The RSX: file copy protocol from the PERQ is as follows (from stream.pas):
    /// 
    /// - RSX: as an output device, RSX:FileName generates
    ///     ^QPip FileName=TI:
    ///         For each character:
    ///          a) Flush RS232 input buffer.
    ///          b) Send RS232 character.
    ///          c) Wait for RS232 echo--in the case of
    ///             sending a carriage return, wait for
    ///             a line feed.
    ///      Send ^Z at end of file.
    /// - RSX: as an input device, RSX:FileName generates
    ///     ^QPip TI:=FileName    
    ///     For each line of RS232 input:
    ///         a) Send a ^Q.
    ///         b) Read from RS232 until end of line.
    ///         c) Send a ^S.
    ///     End of file is indicated by a '>' after an
    ///     end of line, and the ^S is not sent after
    ///     the end of file is received.
    ///     
    /// The above makes it obvious that the transfer is fairly idiosyncractic.
    /// It depends on specific (badly documented) behaviors of both POS and
    /// RSX-11.  In particular, the ^Q/^S bookending for the RSX: input device
    /// is basically enforcing flow control over a serial line that doesn't
    /// implement it (^S pauses the RSX-11's output, ^Q resumes it).  This code
    /// does not pay attention to the ^Q/^S, instead relying on some hackery in
    /// the RS232 class to pace the data such that the PERQ won't lose anything.
    /// (It makes things a lot simpler here at the cost of a bit of throughput.)
    /// 
    /// I've tried to comment up all of the oddities I've encountered.  This is
    /// very much a hack, but it is fairly useful to have (makes uploading source
    /// written on the host a lot easier).
    /// </remarks>
    public sealed class RSXFilePort : SerialDevice
    {
        public RSXFilePort(Z80System sys) : base(sys)
        {
            _system = sys;
            _inputQueue = new Queue<byte>(128);
            _fileStream = null;
            _sendEvent = null;
        }

        public override void Reset()
        {
            if (_sendEvent != null)
            {
                _system.Scheduler.Cancel(_sendEvent);
                _sendEvent = null;
            }

            if (_fileStream != null)
            {
                _fileStream.Close();
                _fileStream = null;
            }

            _isOpen = false;
            _isPaused = false;
            _transferState = TransferState.WaitingForCtrlQ;
            _fileName = string.Empty;
            _rsxCommand = string.Empty;

            _inputQueue.Clear();

            Log.Debug(Category.RS232, "RSX: port reset");
        }

        //
        // ISerialDevice implementation
        //

        public override string Name => "RSX port";

        public override string Port
        {
            get { return "RSX:"; }
            set { throw new InvalidOperationException("Cannot specify the port on RSX:"); }
        }

        public override bool DTR
        {
            get { return true; }
            set { Log.Detail(Category.RS232, "RSX: ignoring DTR pin set to {0}", value); }
        }

        public override bool RTS
        {
            get { return true; }
            set { Log.Detail(Category.RS232, "RSX: ignoring RTS pin set to {0}", value); }
        }

        public override bool DCD => true;
        public override bool CTS => true;
        public override bool DSR => true;

        public override int ByteCount => _inputQueue.Count;

        public override void Transmit(byte value)
        {
            SendData(value);
        }

        void ResetState()
        {
            _transferState = TransferState.WaitingForCtrlQ;
            Close();
        }

        /// <summary>
        /// "Receives" data from the host and sends it in a byte stream to the
        /// PERQ, as if it were being sent over the serial port.  Rate limited
        /// to a fixed 9600 baud (for now).
        /// </summary>
        void ReceiveData(ulong skewNsec, object context)
        {
            if (_inputQueue.Count > 0)
            {
                if (!_isPaused)
                {
                    var b = _inputQueue.Dequeue();

                    _rxDelegate(b);

                    // Do it again in a millisecond or so
                    _sendEvent = _system.Scheduler.Schedule(_charRateInNsec, ReceiveData, null);
                }
                else
                {
                    // Clear it so the ^Q in SendData restarts the receiver
                    _sendEvent = null;
                }
            }
            else
            {
                // In Read mode an empty queue means we're done; close it up
                if (_transferState == TransferState.Transferring && _fileAccess == FileAccess.Read)
                {
                    Log.Write("File '{0}' transfer from host is complete.", _fileName);

                    TransferComplete();
                }

                _sendEvent = null;
            }
        }

        /// <summary>
        /// Intercepts data written from the PERQ and runs the RSX state machine.
        /// When sending a file to the host, writes the data bytes into the output
        /// file; when reading, kicks off the receiver.
        /// </summary>
        public void SendData(byte b)
        {
            //
            // POS implicitly uses Xon/Xoff for flow control for RSX transfers.
            // Check for those characters but do not echo them back.
            //
            if (b == CtrlS)
            {
                _isPaused = true;
                Log.Detail(Category.RS232, "[Paused on ^S]");
            }
            else if (b == CtrlQ)
            {
                _isPaused = false;
                Log.Detail(Category.RS232, "[Resumed on ^Q]");
            }
            else if (_transferState == TransferState.WaitingForCtrlQ ||
                     _transferState == TransferState.WaitingForRSXCommand ||
                    (_transferState == TransferState.Transferring && _fileAccess == FileAccess.Write))
            {
                //
                // The RSX transfer needs the simulated RSX machine to echo back
                // data during the command input phase and during RSX-as-output.
                // Additionally, it expects that if CR is sent an LF is echoed back.
                // 
                if (b == CR)
                {
                    _inputQueue.Enqueue(LF);
                }
                else
                {
                    _inputQueue.Enqueue(b);
                }
            }

            // Run the state machine
            switch (_transferState)
            {
                case TransferState.WaitingForCtrlQ:
                    if (b == CtrlQ)
                    {
                        _rsxCommand = string.Empty;
                        _errorString = string.Empty;
                        _transferState = TransferState.WaitingForRSXCommand;
                    }
                    break;

                case TransferState.WaitingForRSXCommand:
                    if (b != CR)
                    {
                        // Ignore control characters
                        if (b >= 0x20)
                        {
                            _rsxCommand += (char)b;
                        }
                    }
                    else
                    {
                        // Command input complete, parse it to determine the
                        // filename and the mode if the input is correct
                        var success = ParseRSXCommand();

                        if (success)
                        {
                            // Got the filename and mode!  Now try to open it...
                            Log.Write("RSX: Host filename for transfer is '{0}'", _fileName);

                            try
                            {
                                _fileStream = new FileStream(_fileName, _fileMode, _fileAccess);
                            }
                            catch (Exception e)
                            {
                                // Couldn't open the filestream for whatever reason.
                                // The POS RSX: transfer protocol doesn't seem to
                                // have a good facility for ending the transfer due
                                // to an error; for writes, we just drop the output
                                // bits on the floor.  Reads return an empty file.
                                _errorString = e.Message;

                                Log.Write("Error: Could not open '{0}' on host: {1}", _fileName, _errorString);
                            }

                            // For reads, wait for the ^S that POS sends before
                            // loading the buffer.  The transfer is so finicky
                            // that sending data early just causes the copy to
                            // hang indefinitely.  Sigh.
                            if (_fileAccess == FileAccess.Read)
                            {
                                _transferState = TransferState.WaitingForCtrlS;
                            }
                            else
                            {
                                _transferState = TransferState.Transferring;
                            }

                            // Ceremonial.  (Informs the status output)
                            Open();
                        }
                        else
                        {
                            // Command input was invalid; restart state machine
                            // (this really should not ever happen...)
                            Log.Error(Category.RS232, "RSX: command parse failed, resetting");

                            ResetState();
                        }
                    }
                    break;

                case TransferState.WaitingForCtrlS:
                    if (b == CtrlS)
                    {
                        // Now that we're paused, go ahead and queue up the file
                        // so it's ready to go when the ^Q arrives.  Yeesh.
                        StartTransfer();

                        _transferState = TransferState.Transferring;
                    }
                    break;

                case TransferState.Transferring:

                    if (_fileAccess == FileAccess.Write)
                    {
                        // Writing: transfer ends on Ctrl-Z
                        if (b != CtrlZ)
                        {
                            if (_fileStream != null)
                            {
                                _fileStream.WriteByte(b);
                            }
                        }
                        else
                        {
                            Log.Write("File '{0}' transfer to host is complete.", _fileName);

                            // POS expects an LF from RSX at the end of the transfer
                            _inputQueue.Enqueue(LF);

                            TransferComplete();
                        }
                    }

                    // Reading:  handled in ReceiveData()

                    break;
            }

            // If there's data to send (echoback or file transfer), start it (if
            // not already scheduled).  The simulated round trip delay gives the
            // Z80 some time to process the tx/rx interrupts...
            if (_inputQueue.Count > 0 && _sendEvent == null)
            {
                _sendEvent = _system.Scheduler.Schedule(_charRateInNsec, ReceiveData, null);
            }
        }

        /// <summary>
        /// Parses the command from the PERQ.  We expect one of two formats:
        ///       Input:  Pip TI:=FileName
        ///       Output: Pip FileName=TI:
        /// </summary>
        bool ParseRSXCommand()
        {
            bool success = true;

            var tokens = _rsxCommand.Split(_separators);

            // Should be three tokens, and first token must be "Pip"
            if (tokens.Length != 3 || tokens[0] != _pipToken)
            {
                // Nope, it's not...
                success = false;
            }
            else
            {
                // Second token is either "TI:" or the filename
                if (tokens[1] == _TIToken)
                {
                    // Mode is Input from emulation host to PERQ
                    _fileMode = FileMode.Open;
                    _fileAccess = FileAccess.Read;
                    _fileName = tokens[2];
                }
                // If token #2 is the filename, then #3 must be "TI:"
                else if (tokens[2] == _TIToken)
                {
                    // Mode is output from PERQ to emulation host
                    _fileMode = FileMode.Create;
                    _fileAccess = FileAccess.Write;
                    _fileName = tokens[1];
                }
                else
                {
                    // Something's wrong, restart the state machine
                    success = false;
                }
            }

            return success;
        }

        /// <summary>
        /// When uploading from the host, clear the input queue and place required
        /// preamble the protocol requires, or the the error message if the file
        /// can't be found (or open failed for any reason).
        /// </summary>
        void StartTransfer()
        {
            // We must handle the null case -- it indicates the input file
            // could not be opened.  In this case we just return an empty file
            if (_fileStream == null)
            {
                // We will write the error into the file
                for (int i = 0; i < _errorString.Length; i++)
                {
                    _inputQueue.Enqueue((byte)_errorString[i]);
                }
                _inputQueue.Enqueue(CR);
                _inputQueue.Enqueue(LF);

                Log.Debug(Category.RS232, "RSX: sending error string '{0}'", _errorString);
            }
            else
            {
                Log.Debug(Category.RS232, "RSX: sending file, {0} bytes", _fileStream.Length);

                // Queue up the file contents for transmission.  POS expects CRLF
                // line endings (but only really looks for CR and throws away the
                // LFs).  A Unix file without CRs will appear as one long line and
                // not parse properly, so we replace naked LFs with CRLFs in that case
                var last = 0;

                while (_fileStream.Position < _fileStream.Length)
                {
                    var b = (byte)_fileStream.ReadByte();

                    if (b == LF && last != CR)
                    {
                        _inputQueue.Enqueue(CR);
                    }

                    _inputQueue.Enqueue(b);
                    last = b;
                }

                // Catch the special case where the file ends with CR and doesn't
                // have a final LF... POS just hangs... :-/
                if (last != LF)
                {
                    _inputQueue.Enqueue(LF);
                }
            }

            // End-of-file is indicated by a CR followed by a '>' character
            _inputQueue.Enqueue(CR);
            _inputQueue.Enqueue(EOT);
        }

        /// <summary>
        /// Finish a transfer by closing the stream.  Resets the state machine.
        /// </summary>
        void TransferComplete()
        {
            // Last byte in the queue, so close up shop
            if (_fileStream != null)
            {
                _fileStream.Close();
                _fileStream = null;
            }

            ResetState();
        }

        // Debugging
        public override void Status()
        {
            Console.WriteLine($"RSX device:  open {_isOpen}, transfer state {_transferState}");
            Console.WriteLine($"Line state:  {BaudRate} baud, {DataBits}-{Parity}-{StopBits}");
            if (_isPaused) Console.WriteLine($"[Paused ({ByteCount} bytes in output buffer)]");

            if (IsOpen)
            {
                Console.WriteLine($"Filename:    {_fileName} ({_fileAccess})");

                if (_fileAccess == FileAccess.Read)
                {
                    try
                    {
                        Console.WriteLine($"Transferred: {_fileStream.Position} of {_fileStream.Length} bytes");
                    }
                    catch
                    {
                        Console.WriteLine($"[Could not read byte counts, file may have closed]");
                    }
                }
                else
                {
                    try
                    {
                        Console.WriteLine($"Transferred: {_fileStream.Length} bytes");
                    }
                    catch
                    {
                        Console.WriteLine($"[Could not read byte count, file may have closed]");
                    }
                }
            }
        }


        enum TransferState
        {
            WaitingForCtrlQ,
            WaitingForRSXCommand,
            WaitingForCtrlS,
            Transferring
        }


        Queue<byte> _inputQueue;
        SchedulerEvent _sendEvent;

        string _fileName;
        FileMode _fileMode;
        FileAccess _fileAccess;
        FileStream _fileStream;

        TransferState _transferState;
        string _rsxCommand;
        string _errorString;
        bool _isPaused;

        ulong _charRateInNsec = Conversion.BaudRateToNsec(9600);

        const byte CtrlQ = 0x11;        // ^Q
        const byte CtrlS = 0x13;        // ^S
        const byte CtrlZ = 0x1a;        // ^Z
        const byte LF = 0x0a;           // LF
        const byte CR = 0x0d;           // CR/EOL
        const byte EOT = (byte)'>';     // special EOT token for RSX transfer from PERQ

        const string _pipToken = "Pip";
        const string _TIToken = "TI:";
        static char[] _separators = { ' ', '=' };
    }
}

