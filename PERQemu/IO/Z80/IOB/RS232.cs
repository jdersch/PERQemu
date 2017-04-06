// rs232.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;
using System.Text;
using System.IO.Ports;
using System.IO;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80.IOB
{
    /// <summary>
    /// Represents the PERQ's RS232 serial port.
    /// Talks to a physical RS232 device!
    /// </summary>
    [Serializable]
    public sealed class RS232 : IZ80Device
    {
        public RS232()
        {
            _serialDevice = new PhysicalPort(_defaultPort);

            Reset();
        }

        public string Port
        {
            set { _serialDevice.Port = value; }
            get { return _serialDevice.Port; }
        }

        public void Reset()
        {
            _messageIndex = 0;
            _messageData = new byte[32];
            _enabled = false;

            if (_serialDevice != null)
            {
                _serialDevice.Reset();
            }
        }

        /// <summary>
        /// Sets the device to use for the serial port.
        /// </summary>
        /// <param name="fs"></param>
        public void SetDevice(ISerialDevice device)
        {
            if (device == null)
            {
                throw new ArgumentNullException("device");
            }

            _serialDevice = device;
        }

        public bool RunStateMachine(PERQtoZ80Message message,  byte value)
        {
            bool retVal = false;

            _messageData[_messageIndex] = value;
            _messageIndex++;

            switch (message)
            {
                case PERQtoZ80Message.SetRS232Status:
                    // 4 bytes for RS232 status:
                    //  byte 0 = 3 (msg length)
                    //  byte 1 = enable/disable
                    //  byte 2 = clock rate
                    //  byte 3 = settings
                    if (_messageIndex > 3)
                    {
                        _messageIndex = 0;
                        SetRS232Status();
                        retVal = true;      // Done with message
                    }
                    break;

                case PERQtoZ80Message.RS232:
                    // Variable bytes for RS232 data:
                    //  byte 0 = length (no greated than 16)
                    //  byte 1-N = data
                    if (_messageIndex > 1 + _messageData[0])
                    {
                        _messageIndex = 0;
                        WriteRS232();
                        retVal = true;
                    }
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unhandled RS232 message {0}", message);
#endif
                    break;
            }

            return retVal;
        }

        public void Poll(ref Queue<byte> fifo)
        {
            if (_enabled && _serialDevice.ByteCount > 0)
            {
                _pollCount++;

                //
                // So, this is interesting.  We can't write the data out to the PERQ as fast as we want.
                // Certain OSes (POS, for example) don't know how to deal with a sudden influx
                // of serial data, and will just drop it on the floor.  (I should confirm this,
                // but I assume that on the PERQ side of things, the OS has a tiny circular buffer
                // that the ISR continually fills with data from the Z80 -- and if the OS doesn't grab data
                // out of it soon enough, it just gets overwritten.)  Basically, there's no notion of flow
                // control on the PERQ<->Z80 communication link as far as I can tell.  (Later versions of POS
                // may have dealt with this more elegantly.)
                //
                // So we have to pace ourselves here and only write data every so often (basically limiting
                // ourselves to what a REAL 9600 baud connection would provide).  This makes the RSX file
                // transfer backdoor kinda slow.
                //
                // The wonderful magic number '400' below was unscientifically produced by testing out
                // RSX transfers with various values until they stoppped dropping data, then
                // adding 100 for insurance.
                //
                if (_pollCount > 400)
                {
                    _pollCount = 0;

                    // Write message header:
                    //  SOM
                    //  2 (message type)
                    //  2 * count of bytes
                    //  Status1
                    //  Char1
                    //  ...
                    //  StatusN
                    //  CharN
                    //
                    // We can send at most 16 characters per message.  We choose to send only
                    // 1 at a time.  This is mostly a hack to make the FilePort serial sink work
                    // better with POS's RSX code.  At any rate, it's unlikely that this will cause
                    // any serious performance problems.
                    //
                    int charsToWrite = Math.Min(_serialDevice.ByteCount, 1);

                    fifo.Enqueue(Z80System.SOM);
                    fifo.Enqueue((byte)Z80toPERQMessage.RS232Data);
                    fifo.Enqueue((byte)(2 * charsToWrite));

                    for (int i = 0; i < charsToWrite; i++)
                    {
                        // Currently we are so optimistic that we assume no transmission errors.
                        fifo.Enqueue(0x00);     // No error status
                        fifo.Enqueue(_serialDevice.ReadByte());
                    }
                }
            }
        }

        public bool Enabled
        {
            get { return _enabled; }
        }

        private void SetRS232Status()
        {
            _enabled = _messageData[1] != 0;

            if (_enabled && !_serialDevice.IsOpen)
            {
                try
                {
                    _serialDevice.Open();
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.RS232, "RS232 port opened.");
#endif
                }
                catch
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unable to open physical serial port.");
#endif
                }
            }
            else if (!_enabled && _serialDevice.IsOpen)
            {
                try
                {
                    _serialDevice.Close();
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.RS232, "RS232 port closed.");
#endif
                }
                catch
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unable to close physical serial port.");
#endif
                }
            }

            // Set baud rate
            switch (_messageData[2])
            {
                case 0x1:
                    _serialDevice.BaudRate = 9600;
                    break;

                case 0x2:
                    _serialDevice.BaudRate = 4800;
                    break;

                case 0x4:
                    _serialDevice.BaudRate = 2400;
                    break;

                case 0x8:
                    _serialDevice.BaudRate = 1200;
                    break;

                case 0x10:
                    _serialDevice.BaudRate = 600;
                    break;

                case 0x20:
                    _serialDevice.BaudRate = 300;
                    break;

                case 0x40:
                    _serialDevice.BaudRate = 150;
                    break;

                case 0x57:      // Yes, this seems odd, but there you are.  110 baud is 0x57.  Not 0x80.
                    _serialDevice.BaudRate = 110;
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled baud rate setting {0}", _messageData[2]);
#endif
                    break;
            }

            // Set parity
            int parity = _messageData[3] & 0x3;
            switch (parity)
            {
                case 0:
                    _serialDevice.Parity = Parity.None;
                    break;

                case 1:
                    _serialDevice.Parity = Parity.Odd;
                    break;

                case 3:
                    _serialDevice.Parity = Parity.Even;
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unhandled parity setting {0}", parity);
#endif
                    break;
            }

            int stopBits = (_messageData[3] & 0x6) >> 2;

            try
            {
                switch (stopBits)
                {
                    case 0:
                        _serialDevice.StopBits = StopBits.None;
                        break;

                    case 1:
                        _serialDevice.StopBits = StopBits.One;
                        break;

                    case 2:
                        _serialDevice.StopBits = StopBits.OnePointFive;
                        break;

                    case 3:
                        _serialDevice.StopBits = StopBits.Two;
                        break;
                }
            }
            catch
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings,
                              "Stopbits setting of {0} not available on physical hardware.  Assuming One stop bit.",
                               stopBits);
#endif
                _serialDevice.StopBits = StopBits.One;
            }

            int transmitBits = (_messageData[3] & 0x30) >> 4;
            int receiveBits = (_messageData[3] & 0x60) >> 6;

#if TRACING_ENABLED
            if (Trace.TraceOn && transmitBits != receiveBits)
                Trace.Log(LogType.Warnings,
                         "Transmit bits {0} != Receive bits {1}, unsupported on physical hardware.  Assuming data bits of {2}",
                          transmitBits, receiveBits, transmitBits);
#endif

            switch (transmitBits)
            {
                case 0:
                    _serialDevice.DataBits = 5;
                    break;

                case 1:
                    _serialDevice.DataBits = 7;
                    break;

                case 2:
                    _serialDevice.DataBits = 6;
                    break;

                case 3:
                    _serialDevice.DataBits = 8;
                    break;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
            {
                Trace.Log(LogType.RS232, "RS232 enable status {0}", _enabled);
                Trace.Log(LogType.RS232, "RS232 baud set to {0}", _serialDevice.BaudRate);
                Trace.Log(LogType.RS232, "RS232 parity set to {0}", _serialDevice.Parity);
                Trace.Log(LogType.RS232, "RS232 stop bits set to {0}", _serialDevice.StopBits);
                Trace.Log(LogType.RS232, "RS232 data bits set to {0}", _serialDevice.DataBits);
            }
#endif
        }

        private void WriteRS232()
        {
            int dataLength = _messageData[0];

            try
            {
                _serialDevice.Write(_messageData, 1, dataLength);
            }
            catch
            {
#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Write to physical serial port failed.");
#endif
            }
        }

        public void GetStatus(ref Queue<byte> fifo)
        {
            // Return current tablet status (per Z80 v8.7):
            fifo.Enqueue(Z80System.SOM);
            fifo.Enqueue((byte)Z80toPERQMessage.RS232Status);   // Reply <7>
            fifo.Enqueue(0x3);                                  // Byte count
            fifo.Enqueue((byte)(_enabled ? 0x1 : 0x0));         // 1st byte: is it interrupt enabled?
            fifo.Enqueue(0x0);                                  // 2nd byte:  SIOACL clock rate
            fifo.Enqueue(0x0);                                  // 3rd byte:  SIOASB rcvr/xmit status byte

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Tablet, "--> RS232 message: GetStatus\tenabled: {0}", _enabled);
#endif
        }

        private ISerialDevice _serialDevice;
        private const string _defaultPort = "COM1";
        private byte[] _messageData;
        private int _messageIndex;
        private int _pollCount;
        private bool _enabled = false;
    }
}
