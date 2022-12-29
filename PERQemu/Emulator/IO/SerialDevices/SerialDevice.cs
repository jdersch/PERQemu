//
// NullPort.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    [Flags]
    public enum CharStatus : byte
    {
        None = 0x0,
        InvalidChar = 0x1,
        PinChange = 0x2,
        ParityError = 0x4,
        FramingError = 0x8,
        Overrun = 0x10,
        Underrun = 0x20,
        BreakDetected = 0x40,
        DeviceError = 0x80
    }

    /// <summary>
    /// For "real" devices, this extended delegate allows hardware status changes
    /// to be sent to the SIO with a recieved character, or asynchronously.
    /// </summary>
    public delegate void ReceiveStatusDelegate(byte rxValue, CharStatus rxStatus);


    /// <summary>
    /// Provide the basis for implementation of serial devices that can exchange
    /// data with the SIO chip, and receive baud rate updates from the timer chip.
    /// </summary>
    public abstract class SerialDevice : ICTCDevice, ISIODevice
    {
        protected SerialDevice(Z80System sys)
        {
            _system = sys;
            _name = "Generic serial device";
            _portName = string.Empty;
            _rxDelegate = null;
            _errDelegate = null;
        }

        protected SerialDevice(Z80System sys, string port) : this(sys)
        {
            _portName = port;
        }

        public virtual void Reset()
        {
            Log.Debug(Category.RS232, "Device reset");
        }

        public virtual void Open()
        {
            _isOpen = true;
        }

        public virtual void Close()
        {
            _isOpen = false;
        }

        public virtual string Name
        {
            get { return _name; }
            protected set { _name = value; }
        }

        public virtual string Port
        {
            get { return _portName; }
            set { _portName = value; }
        }

        public virtual bool IsOpen => _isOpen;
        public virtual int ByteCount => 0;
        public virtual int BaudRate => 9600;

        public virtual int DataBits
        {
            get { return 8; }
            set { Log.Detail(Category.RS232, "Ignoring data bits set to {0}", value); }
        }

        public virtual Parity Parity
        {
            get { return Parity.None; }
            set { Log.Detail(Category.RS232, "Ignoring parity set to {0}", value); }
        }

        public virtual StopBits StopBits
        {
            get { return StopBits.One; }
            set { Log.Detail(Category.RS232, "Ignoring stop bits set to {0}", value); }
        }

        public virtual bool DTR
        {
            get { return false; }
            set { Log.Detail(Category.RS232, "Ignoring DTR pin set to {0}", value); }
        }

        public virtual bool RTS
        {
            get { return false; }
            set { Log.Detail(Category.RS232, "Ignoring RTS pin set to {0}", value); }
        }

        public virtual bool DCD => false;
        public virtual bool CTS => false;
        public virtual bool DSR => false;


        //
        // ICTCDevice implementation
        //

        public virtual void NotifyRateChange(int newRate)
        {
            Log.Detail(Category.RS232, "Clock rate change to {0} ignored for {1}", newRate, Name);
        }

        //
        // ISIODevice implementation
        //

        public virtual void RegisterReceiveDelegate(ReceiveDelegate rxDelegate)
        {
            _rxDelegate = rxDelegate;
        }

        public virtual void RegisterStatusDelegate(ReceiveStatusDelegate rxDelegate)
        {
            _errDelegate = rxDelegate;
        }

        public virtual void Transmit(byte value)
        {
            Log.Detail(Category.RS232, "Transmit byte 0x{0:x2} on {1} ignored", value, Name);
        }

        public virtual void TransmitAbort()
        {
            throw new NotImplementedException($"TransmitAbort on {Name}");
        }

        public virtual void TransmitBreak()
        {
            throw new NotImplementedException($"TransmitBreak on {Name}");
        }

        public virtual void Status()
        {
            Console.WriteLine($"No status available for this {Name}.");
        }


        protected Z80System _system;
        protected ReceiveDelegate _rxDelegate;
        protected ReceiveStatusDelegate _errDelegate;

        protected bool _isOpen;
        protected string _name;
        protected string _portName;
    }
}
