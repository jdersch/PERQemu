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
using System.Collections.Generic;
using System.IO.Ports;
using System.IO;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// FilePort provides logic that emulates an RSX-11 host connected to the PERQ's simulated
    /// serial port.
    /// 
    /// In doing so, it provides a crude way to copy a file from the host to the emulated PERQ.
    /// Under POS, one can do a "copy RSX:remoteFileName localFileName" and it will copy incoming
    /// serial data to a file on the PERQ.
    /// 
    /// Conversely, one can do a "copy localFileName RSX:remoteFileName" to copy a PERQ file to
    /// the serial port.
    /// 
    /// This class acts as a serial sink/source and either provides a data stream from a local file
    /// (on the host) or writes an incoming stream to a local file.  This is a pretty nifty way
    /// to allow the PERQ limited access to the filesystem on the emulation host.  (Alas, no directory
    /// support...)
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
    /// The above makes it obvious that the transfer is fairly idiosyncractic.  It depends on specific
    /// (badly documented) behaviors of both POS and RSX-11.  In particular, the ^Q/^S bookending for the
    /// RSX: input device is basically enforcing flow control over a serial line that doesn't implement it
    /// (^S pauses the RSX-11's output, ^Q resumes it).  This code does not pay attention to the ^Q/^S,
    /// instead relying on some hackery in the RS232 class to pace the data such that the PERQ won't lose
    /// anything.  (It makes things a lot simpler here at the cost of a bit of throughput.)
    /// 
    /// I've tried to comment up all of the oddities I've encountered.  This is very much a hack, but it
    /// is fairly useful to have (makes uploading source written on the host a lot easier).
    /// </summary>
    public class RSXFilePort : ISerialDevice
    {
        public RSXFilePort()
        {
            Reset();
        }

        public void Reset()
        {
            _inputQueue = new Queue<byte>(128);
            _fileStream = null;
            _transferState = TransferState.WaitingForCtrlQ;
            _fileName = string.Empty;
            _rsxCommand = string.Empty;
            _isOpen = false;
        }

        public string Port
        {
            get { return "RSX:"; }
            set { throw new InvalidOperationException("Cannot specify the port on an RSX file port."); }
        }

        public int BaudRate
        {
            get { return 9600; }
            set { }
        }

        public Parity Parity
        {
            get { return Parity.None; }
            set { }
        }

        public StopBits StopBits
        {
            get { return StopBits.None; }
            set { }
        }

        public int DataBits
        {
            get { return 8; }
            set { }
        }

        public int ByteCount
        {
            get { return _inputQueue.Count; }
        }

        public bool IsOpen
        {
            get { return _isOpen; }
        }

        public void Open()
        {
            _isOpen = true;
        }

        public void Close()
        {
            _isOpen = false;
        }

        public byte ReadByte()
        {
            if (_inputQueue.Count == 0)
            {
                throw new InvalidOperationException("RSX input stream is empty on read!");
            }

            byte b = _inputQueue.Dequeue();

            // If this is the last byte in the queue for an Input read, we are done.
            if (_inputQueue.Count == 0 &&
                _fileAccess == FileAccess.Read &&
                _transferState == TransferState.Transferring)
            {
                if (_fileStream != null)
                {
                    _fileStream.Close();
                    _fileStream = null;
                }

                Log.Write("Transfer of {0} from host is complete.", _fileName);
                ResetState();
            }


            return b;
        }

        public void Write(byte[] data, int index, int length)
        {
            for (int i = 0; i < length; i++)
            {
                byte b = data[i + index];

                //
                // The RSX transfer needs the simulated RSX machine to echo back data
                // during the command input phase and during RSX-as-output.
                // Additionally, it expects that if a CR is sent, an LF is echoed back...
                // 
                if ((_transferState == TransferState.Transferring && _fileAccess == FileAccess.Write) ||
                    _transferState == TransferState.WaitingForCtrlQ ||
                    _transferState == TransferState.WaitingForRSXCommand)
                {
                    if (b == _cr)
                    {
                        _inputQueue.Enqueue(_lf);
                    }
                    else
                    {
                        _inputQueue.Enqueue(b);
                    }
                }

                switch (_transferState)
                {
                    case TransferState.WaitingForCtrlQ:
                        if (b == _ctrlQ)
                        {
                            _rsxCommand = string.Empty;
                            _transferState = TransferState.WaitingForRSXCommand;
                        }
                        break;

                    case TransferState.WaitingForRSXCommand:
                        if (b != _cr)
                        {
                            //
                            // Ignore control characters.
                            //
                            if (b >= 0x20)
                            {
                                _rsxCommand += (char)b;
                            }
                        }
                        else
                        {
                            // Command input complete, parse it to determine the
                            // filename and the mode if the input is correct.
                            bool success = ParseRSXCommand();

                            if (success)
                            {
                                //
                                // Command input is valid.  We should have the file name
                                // and mode now.
                                //
                                _transferState = TransferState.Transferring;

                                //
                                // Open the file in the correct mode...
                                //
                                try
                                {
                                    _fileStream = new FileStream(_fileName, _fileMode, _fileAccess);
                                }
                                catch (Exception e)
                                {
                                    //
                                    // Couldn't open the filestream for whatever reason.
                                    // The POS RSX: transfer protocol doesn't seem to have a good 
                                    // facility for ending the transfer due to an error.
                                    // For writes, we'll just end up dropping the output bits on the floor.  
                                    // For reads, we'll just return an empty file.
                                    //
                                    _errorString = e.Message;

                                    Log.Write("Could not open {0} on host. Error: {1}",
                                              _fileName, _errorString);
                                }

                                if (_fileAccess == FileAccess.Read)
                                {
                                    // Read the file into the input queue, and append the special
                                    // eol/eot marker.
                                    UploadFileToBuffer();
                                }
                            }
                            else
                            {
                                //
                                // Command input was invalid, return to start of state machine.
                                //
                                ResetState();
                            }
                        }
                        break;

                    case TransferState.Transferring:
                        // We can handle the write case here, otherwise
                        // we handle it under Read() and do nothing here.
                        if (_fileAccess == FileAccess.Write)
                        {
                            // File transfer ends on Ctrl-Z
                            if (b != _ctrlZ)
                            {
                                if (_fileStream != null)
                                {
                                    _fileStream.WriteByte(b);
                                }
                            }
                            else
                            {
                                // Transfer is done.
                                if (_fileStream != null)
                                {
                                    _fileStream.Close();
                                    _fileStream = null;
                                }

                                ResetState();

                                //
                                // POS expects an LF from RSX at the end of the transfer.
                                //                                
                                _inputQueue.Enqueue(_lf);

                                Log.Write("Transfer of {0} to host is complete.", _fileName);
                            }
                        }
                        break;
                }
            }
        }

        private void ResetState()
        {
            _transferState = TransferState.WaitingForCtrlQ;
            _inputQueue.Clear();
        }

        private bool ParseRSXCommand()
        {
            bool success = true;

            //
            // We expect one of two formats:
            // Input:  Pip TI:=FileName
            // Output: Pip FileName=TI:
            //
            string[] tokens = _rsxCommand.Split(_separators);

            // Should be three tokens, and first token must be "Pip"
            if (tokens.Length != 3 || tokens[0] != _pipToken)
            {
                // Nope, it's not...
                success = false;
            }
            else
            {
                // Second token is either "TI:" or the filename.
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
                    // Mode is output from PERQ to emulation host.
                    _fileMode = FileMode.Create;
                    _fileAccess = FileAccess.Write;
                    _fileName = tokens[1];
                }
                else
                {
                    // Something's wrong, restart the state machine.
                    success = false;
                }
            }

            if (success)
            {
                Log.Write("Host filename for RSX: transfer is {0}.", _fileName);
            }

            return success;
        }

        private void UploadFileToBuffer()
        {
            _inputQueue.Clear();

            //
            // POS's RSX code expects a CR/LF before the real file data begins.
            //
            _inputQueue.Enqueue(_cr);
            _inputQueue.Enqueue(_lf);

            // We must handle the null case -- it indicates the input file
            // could not be opened.  In this case we just return an empty file.
            if (_fileStream != null)
            {
                for (int i = 0; i < _fileStream.Length; i++)
                {
                    _inputQueue.Enqueue((byte)_fileStream.ReadByte());
                }
            }
            else
            {
                // We will write the error into the file.
                for (int i = 0; i < _errorString.Length; i++)
                {
                    _inputQueue.Enqueue((byte)_errorString[i]);
                }
                _inputQueue.Enqueue(_cr);
                _inputQueue.Enqueue(_lf);
            }

            //
            // End-of-file is indicated by a CR followed by a '>' character.
            //
            _inputQueue.Enqueue(_cr);
            _inputQueue.Enqueue(_eot);
        }

        private enum TransferState
        {
            WaitingForCtrlQ,
            WaitingForRSXCommand,
            Transferring
        }

        private bool _isOpen;
        private Queue<byte> _inputQueue;
        private FileMode _fileMode;
        private FileAccess _fileAccess;
        private FileStream _fileStream;
        private TransferState _transferState;
        private string _rsxCommand;
        private string _fileName;
        private string _errorString;

        private const byte _ctrlQ = 0x11;      // ^Q
        private const byte _ctrlS = 0x13;      // ^S
        private const byte _ctrlZ = 0x1a;      // ^Z
        private const byte _lf    = 0x0a;      // LF
        private const byte _cr    = 0x0d;      // CR/EOL
        private const byte _eot   = (byte)'>'; // special EOT token for RSX transfer from PERQ

        private const string _pipToken = "Pip";
        private const string _TIToken = "TI:";
        private static char[] _separators = { ' ', '=' };
    }
}

