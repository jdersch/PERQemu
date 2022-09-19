//
// RealPort.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.IO.Ports;
using System.Collections.Generic;

using PERQemu;
using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// Encapsulates a System.IO.Ports.SerialPort to talk to a real serial port,
    /// or whatever USB hack passes for that nowadays.  Allows the emulator to
    /// reach out and touch a connected serial device, handling flow control and
    /// buffering to smooth data transmission with the PERQ.
    /// </summary>
    /// <remarks>
    /// Currently implemented for RS232 "A" only, assuming an IOB/CIO with a Z80
    /// CTC chip providing timing.  Figures out the baud rate setting to schedule
    /// queueing of incoming bytes so the PERQ sees realistic timing of an (up to)
    /// 9600 baud data stream.
    /// 
    /// NB:  To work around limitations of the SerialPort implementation, all of
    /// the configurable port settings are shadowed in local variables.  Many of
    /// the settings can only be applied prior to calling Open(), and will either
    /// be ignored or throw exceptions otherwise.  Extensive testing needs to be
    /// done to see if System.IO.Ports.SerialPort is as horrible as reported and
    /// if so, find adequate workarounds...
    /// </remarks>
    public class PhysicalPort : SerialDevice
    {
        public PhysicalPort(Z80System sys, string portName, SerialSettings portSet, string id) : base(sys, portName)
        {
            _name = id;                     // Distinguish RS232 "A" and "B"
            _host = portSet;                // The user's host-side configuration
            _perq = new SerialSettings();   // The PERQ's view (mostly ignored)
            _port = new SerialPort();
            _sendEvent = null;
        }

        public override void Reset()
        {
            if (_sendEvent != null)
            {
                _system.Scheduler.Cancel(_sendEvent);
                _sendEvent = null;
            }

            if (IsOpen)
            {
                // Flush data (this may not actually work)
                _port.DiscardInBuffer();
                _port.DiscardOutBuffer();
            }

            // Reset to PERQ defaults
            _perq.BaudRate = 9600;
            _perq.DataBits = 8;
            _perq.Parity = Parity.None;
            _perq.StopBits = StopBits.One;
            _dtr = true;
            _rts = true;
            _portChanged = false;

            // Adjust the pacing rate for scheduling characters to the PERQ
            _charRateInNsec = Conversion.BaudRateToNsec(_perq.BaudRate);

            Log.Info(Category.RS232, "Port {0} physical device reset", _name);
        }

        //
        // ISerialDevice implementation
        //

        /// <summary>
        /// Open the host device and apply the user-configured settings.  This
        /// runs at any rate they configure, while the PERQ side emulates/limits
        /// the data flow to the PERQ's restricted range of speeds.
        /// </summary>
        public override void Open()
        {
            // Most of the port's characteristics only take effect before calling
            // Open();  If _portChanged is set the user wants to force a change
            // so do a close and reopen to apply new settings.
            if (!_isOpen || _portChanged)
            {
                try
                {
                    if (_isOpen) Close();

                    _port.PortName = _portName;
                    _port.BaudRate = _host.BaudRate;
                    _port.DataBits = _host.DataBits;
                    _port.Parity = _host.Parity;
                    _port.StopBits = _host.StopBits;
                    _port.Handshake = Handshake.XOnXOff;
                    _port.DtrEnable = _dtr;
                    _port.RtsEnable = _rts;

                    _port.DataReceived += OnDataReceived;
                    _port.PinChanged += OnPinChange;
                    _port.ErrorReceived += OnError;

                    _port.Open();
                    _portChanged = false;
                }
                catch (Exception e)
                {
                    Log.Error(Category.RS232, "Could not open physical port: {0}", e.Message);
                }
            }

            _isOpen = _port.IsOpen;
            Log.Info(Category.RS232, "Port {0} is {1}", _name, _isOpen ? "now open" : "still closed!");
        }

        public override void Close()
        {
            // So apparently it's quite common to catch exceptions when trying
            // to close the port; catch (and ignore) 'em just in case
            try
            {
                _port.ErrorReceived -= OnError;
                _port.PinChanged -= OnPinChange;
                _port.DataReceived -= OnDataReceived;

                _port.Close();
            }
            catch (Exception e)
            {
                Log.Error(Category.RS232, "Exception on close: {0}", e.Message);
            }

            _isOpen = _port.IsOpen;
            Log.Info(Category.RS232, "Port {0} is {1}", _name, _isOpen ? "still open!" : "now closed");
        }


        public override int BaudRate
        {
            get { return _perq.BaudRate; }
        }

        public override int ByteCount
        {
            get { return (_isOpen ? _port.BytesToRead : 0); }
        }

        public override int DataBits
        {
            get { return _perq.DataBits; }
            set { _perq.DataBits = value; }
        }

        public override Parity Parity
        {
            get { return _perq.Parity; }
            set { _perq.Parity = value; }
        }

        public override StopBits StopBits
        {
            get { return _perq.StopBits; }
            set { _perq.StopBits = value; }
        }

        public override bool DTR
        {
            get { return (IsOpen ? _port.DtrEnable : _dtr); }
            set { _dtr = value; _portChanged |= (_isOpen && _port.DtrEnable != _dtr); }
        }

        public override bool RTS
        {
            get { return (IsOpen ? _port.RtsEnable : _rts); }
            set { _rts = value; _portChanged |= (_isOpen && _port.RtsEnable != _rts); }
        }

        public override bool DCD => (_isOpen ? _port.CDHolding : false);
        public override bool DSR => (_isOpen ? _port.DsrHolding : false);
        public override bool CTS => (_isOpen ? _port.CtsHolding : false);


        /// <summary>
        /// Compute new baud rate from the timer tick rate provided by the CTC.
        /// See notes below for detailed information about baud calculation.
        /// </summary>
        public override void NotifyRateChange(int newRate)
        {
            if ((_perq.BaudRate = Conversion.TimerCountToBaudRate(newRate)) > 0)
            {
                _charRateInNsec = Conversion.BaudRateToNsec(_perq.BaudRate);

                Log.Info(Category.RS232, "Port {0} baud rate (emulated) changed to {1}", _name, _perq.BaudRate);
            }
            else
            {
                Log.Warn(Category.RS232, "Port {0} bad baud rate {1} from the PERQ!", _name, newRate);
            }
        }

        /// <summary>
        /// Start (or continue) data transmission from the port TO the PERQ.  The
        /// system provides a huge buffer (4K by default!?) so we don't bother to
        /// copy the data again; just transmit the first character and schedule a
        /// callback to continue sending bytes at the proper pace until the buffer
        /// empties.
        /// </summary>
        private void OnDataReceived(object sender, SerialDataReceivedEventArgs e)
        {
            // If a _sendEvent is not already in progress, kick one off.  I'm
            // sure this is just one giant nasty race condition but lalalalala
            if (_sendEvent == null)
            {
                ReceiveByte(0, null);
            }
        }

        private void ReceiveByte(ulong skewNsec, object context)
        {
            if (ByteCount > 0)
            {
                // Fetch a byte from the physical device...
                var data = _port.ReadByte();

                // ...and send it to the PERQ
                _rxDelegate((byte)data);

                Log.Info(Category.RS232, "Read byte {0:x2} ({1} in input queue)", data, ByteCount);

                // Schedule the next byte according to current baud rate
                _sendEvent = _system.Scheduler.Schedule(_charRateInNsec, ReceiveByte, null);
            }
            else
            {
                _sendEvent = null;
            }
        }

        private void OnPinChange(object sender, SerialPinChangedEventArgs e)
        {
            Log.Info(Category.RS232, "Pin changed! {0}", e.EventType);
            // Send it via _errDelegate
        }

        private void OnError(object sender, SerialErrorReceivedEventArgs e)
        {
            Log.Info(Category.RS232, "Serial port error! {0}", e.EventType);
            // Send it via _errDelegate
        }

        /// <summary>
        /// Write a byte from the PERQ to the physical port.
        /// </summary>
        /// <remarks>
        /// ASSUMPTIONS:
        /// 1.  The large default output buffer (2K) will accept characters to
        ///     be queued even if flow control is in effect;
        /// 2.  Writes to the port won't block unless/until the queue is full,
        ///     which should rarely/never happen;
        /// 3.  This is all wrong and System.IO.SerialPort is hopelessly busted.
        ///     But we'll give it a go and see if it works at all...
        /// </remarks>
        public override void Transmit(byte value)
        {
            if (_isOpen)
            {
                var seriously = new byte[] { value };
                _port.Write(seriously, 0, 1);
                Log.Info(Category.RS232, "Wrote byte {0:x2} ({1} in output queue)", value, _port.BytesToWrite);

                // Poke the receiver.  This is a gross HACK, but less gross than
                // dedicating yet another thread to polling the $)#$&! serial port
                if (_sendEvent == null) ReceiveByte(0, null);
            }
            else
            {
                Log.Warn(Category.RS232, "Port {0} write ({1:x2}) failed, device not open!", _name, value);
            }
        }

        // todo:  if the PERQ actually wants to send Breaks, add an override for TransmitBreak()

        // Debugging
        public override void Status()
        {
            Console.WriteLine($"Serial port {Name}:  device is '{Port}', IsOpen={IsOpen}");
            Console.WriteLine($"Host settings: {_host}");
            Console.WriteLine($"PERQ settings: {_perq}");

            //if (IsOpen)
            //{
            Console.WriteLine($"Handshake: {_port.Handshake}  Break state: {_port.BreakState}");
            Console.WriteLine($"Rx buffer: {ByteCount}/{_port.ReadBufferSize}  " +
                              $"Tx buffer: {_port.BytesToWrite}/{_port.WriteBufferSize}, " +
                              $"pacing {_charRateInNsec * Conversion.NsecToMsec}ms");
            //}
            Console.WriteLine($"Pins:  DCD={DCD} DTR={DTR} DSR={DSR} RTS={RTS} CTS={CTS}");
        }


        // Host side
        private SerialPort _port;
        private SerialSettings _host;

        // PERQ side
        private SerialSettings _perq;
        private bool _dtr;
        private bool _rts;

        private SchedulerEvent _sendEvent;
        private ulong _charRateInNsec;
        private bool _portChanged;
    }
}

/*
    Baud rate clock notes:
    
    For IOB/CIO (Z80 @ 2.4576Mhz) the CTC is programmed with these values
    (channel 0) to generate the RS232-A port baud rate clock:

    Prescaler for timer: 16 (or 256)    2.4576Mhz = 407ns
    SIO driven @ 16x freq, so delta * 16 = bit rate, * 10 = char/irq rate
    
    9600    TC 1    delta 6512      -> 104192ns/bit or 1.042ms/char
    4800    TC 2    delta 13024     -> 208384ns/bit or 2.084ms/char
    2400    TC 4    delta 26048     -> 406768ns/bit or 4.168ms/char
    1200    TC 8    delta 52096     -> 833536ns/bit or 8.335ms/char
     600    TC 16   delta 104192    -> 1667072ns/bit, 16.670ms/char
     300    TC 32   delta 208384    -> 3334144ns/bit, 33.341ms/char
     150    TC 64   delta 416768    -> 6668288ns/bit, 66.682ms/char
     110    TC 87   delta 566544    -> 9064704ns/bit, 90.647ms/char

    At 9600 baud, scheduling a serial event at 1.042ms intervals could
    actually work; on the Z80 scheduler that's 256 clocks between chars,
    a rate that wouldn't put an undue strain on the emulator if there
    was a clean way to avoid polling... or make it "cheap" enough.

    EIO (Z80 @ 4Mhz) uses an Intel i8254 timer, so the programming will be
    different.  [Not yet implemented]
 */

/*
    The "right way" to avoid the worst of the system SerialPort bugs is to
    have yet another thread running to receive characters?  Oof.

    byte[] buffer = new byte[blockLimit];
    Action kickoffRead = null;
    kickoffRead = delegate {
        port.BaseStream.BeginRead(buffer, 0, buffer.Length, delegate (IAsyncResult ar) {
            try {
                int actualLength = port.BaseStream.EndRead(ar);
                byte[] received = new byte[actualLength];
                Buffer.BlockCopy(buffer, 0, received, 0, actualLength);

                raiseAppSerialDataEvent(received);
            }
            catch (IOException exc) {
                handleAppSerialError(exc);
            }

            kickoffRead();
        }, null);
    };
    kickoffRead();

*/