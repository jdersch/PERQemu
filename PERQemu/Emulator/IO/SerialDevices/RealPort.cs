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
    /// </remarks>
    public class PhysicalPort : SerialDevice
    {
        public PhysicalPort(Z80System sys, string portName) : base(sys, portName)
        {
            _system = sys;
            _portName = portName;
            _physicalPort = new SerialPort();
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
                Close();
            }

            // Reset to PERQ defaults
            // Apparently these ONLY work if the port is closed?
            _physicalPort.BaudRate = _baudRate = 4800;      // 9600 in POS G?
            _physicalPort.Handshake = Handshake.RequestToSend;
            _physicalPort.DtrEnable = true;
            _physicalPort.RtsEnable = true;

            _charRateInNsec = Conversion.BaudRateToNsec(_baudRate);

            Log.Debug(Category.RS232, "Physical port reset");
        }

        //
        // ISerialDevice implementation
        //

        public override void Open()
        {
            _physicalPort.PortName = _portName;
            _physicalPort.Open();
            _physicalPort.DataReceived += OnDataReceived;

            // todo: sign up for errors, pin changes
        }

        public override void Close()
        {
            //_physicalPort.DiscardInBuffer();
            //_physicalPort.DiscardOutBuffer();
            _physicalPort.DataReceived -= OnDataReceived;
            _physicalPort.Close();
        }


        public override bool IsOpen
        {
            get { return _physicalPort.IsOpen; }
        }

        public override int BaudRate
        {
            get { return _physicalPort.BaudRate; }
        }

        public override int ByteCount
        {
            get { return _physicalPort.BytesToRead; }
        }

        public override int DataBits
        {
            get { return _physicalPort.DataBits; }
            set
            {
                try { _physicalPort.DataBits = value; }
                catch { Log.Error(Category.RS232, "Failed to set data bits '{0}'", value); }
            }
        }

        public override Parity Parity
        {
            get { return _physicalPort.Parity; }
            set
            {
                try { _physicalPort.Parity = value; }
                catch { Log.Error(Category.RS232, "Failed to set parity '{0}'", value); }
            }
        }

        public override StopBits StopBits
        {
            get { return _physicalPort.StopBits; }
            set
            {
                try { _physicalPort.StopBits = value; }
                catch { Log.Error(Category.RS232, "Failed to set stop bits '{0}'", value); }
            }
        }

        public override bool DTR
        {
            get { return _physicalPort.DtrEnable; }
            set { _physicalPort.DtrEnable = value; }
        }

        public override bool RTS
        {
            get { return _physicalPort.RtsEnable; }
            set { _physicalPort.RtsEnable = value; }
        }

        public override bool DCD => _physicalPort.CDHolding;
        public override bool DSR => _physicalPort.DsrHolding;
        public override bool CTS => _physicalPort.CtsHolding;


        /// <summary>
        /// Compute new baud rate from the timer tick rate provided by the CTC.
        /// See notes below for detailed information about baud calculation.
        /// </summary>
        public override void NotifyRateChange(int newRate)
        {
            try
            {
                if ((_baudRate = Conversion.TimerCountToBaudRate(newRate)) > 0)
                {
                    // Try setting up the hardware to see if it's valid
                    // fixme: SerialPort requires that the device be closed when changing this!?
                    _physicalPort.BaudRate = _baudRate;

                    // Adjust the pacing rate for scheduling characters to the PERQ
                    _charRateInNsec = Conversion.BaudRateToNsec(_baudRate);

                    Log.Debug(Category.RS232, "Baud rate changed to {0}", _baudRate);
                }
            }
            catch (Exception e)
            {
                Log.Error(Category.RS232, "Failed to change baud rate: {0}", e.Message);
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
            Console.WriteLine($"[Rx count: {ByteCount}]");

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
                var data = _physicalPort.ReadByte();

                Log.Detail(Category.RS232, "Read byte {0:x2} ({1} in input queue)", data, ByteCount);

                // ...and send it to the PERQ
                _rxDelegate((byte)data);
                Console.WriteLine($"Byte '{data}' received and forwarded");
            }

            // More to send?
            if (ByteCount > 0)
            {
                // Schedule it according to current baud rate (~1ms @ 9600 baud)
                _sendEvent = _system.Scheduler.Schedule(_charRateInNsec, ReceiveByte, null);
                Console.WriteLine("More chars to send, rescheduling");
            }
            else
            {
                _sendEvent = null;
                Console.WriteLine("Input queue drained");
            }
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
        /// </remarks>
        public override void Transmit(byte value)
        {
            _physicalPort.Write($"{value}");    // Ugh.
            Log.Detail(Category.RS232, "Wrote byte {0:x2} ({1} in output queue)", value, _physicalPort.BytesToWrite);
        }

        public override void TransmitBreak()
        {
            // _physicalPort.BreakState = value;        // I bet this is just held and must be set manually...
            Log.Debug(Category.RS232, "TransmitBreak called!");
        }


        // Debugging
        public override void Status()
        {
            Console.WriteLine($"Serial port A device is '{_portName}', IsOpen={IsOpen}");
            Console.WriteLine($"Handshake: {_physicalPort.Handshake}  Break state: {_physicalPort.BreakState}");
            Console.WriteLine($"Baud rate: {_baudRate} ({BaudRate})  Bits: {DataBits}  Parity: {Parity}  StopBits: {StopBits}");
            Console.WriteLine($"Rx buffer: {ByteCount}/{_physicalPort.ReadBufferSize}  " +
                              $"Tx buffer: {_physicalPort.BytesToWrite}/{_physicalPort.WriteBufferSize}, " +
                              $"pacing {_charRateInNsec}ns");
            Console.WriteLine($"Pins:  DCD={_physicalPort.CDHolding} " +
                              $"DTR={_physicalPort.DtrEnable} " +
                              $"DSR={_physicalPort.DsrHolding} " +
                              $"RTS={_physicalPort.RtsEnable} " +
                              $"CTS={_physicalPort.CtsHolding}");
        }


        private int _baudRate;
        private ulong _charRateInNsec;

        private SerialPort _physicalPort;

        private SchedulerEvent _sendEvent;
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