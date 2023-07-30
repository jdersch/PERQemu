//
// EIOZ80.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.IO.GPIB;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The EIO board's Z80 subsystem, used in all PERQ-2 models.
    /// </summary>
    /// <remarks>
    /// TODO: A ton of work to add all the new controllers.
    ///     Am9519 interrupt controller @ port ___
    ///     i8254 programmable timers (2x) @ ports ___,___ (replaces Z80CTC)
    ///     i8237 4-channel DMA chip @ port ___ (replaces Z80DMA)
    ///     Second Z80SIO for RS232B, VT100-style keyboard
    ///     Oki M5M5832 RTC chip @ port ___ (with write control lines!!)
    ///     Updated ports, vectors for all the chippies
    /// TODO: Figure out how the PERQ DMA will work
    /// </remarks>
    public sealed class EIOZ80 : Z80System
    {
        public EIOZ80(PERQSystem system) : base(system)
        {
            // Set up the EIO peripherals
            _fdc = new NECuPD765A(0xa8, _scheduler);    // TODO: ports!?
            _tms9914a = new TMS9914A(0xb8);             // TODO: ports
            //_perqToZ80Fifo = new PERQToZ80FIFO(_system);  // TODO: actual FIFO implementation
            //_z80ToPerqFifo = new Z80ToPERQFIFO(_system);  // TODO:    "     "         "
            _z80sioA = new Z80SIO(0xb0, _scheduler);    // TODO: ports
            _z80sioB = new Z80SIO(0xb4, _scheduler);    // TODO: ports!!
            _keyboard = new SerialKeyboard();

            // New stuff not yet implemented:
            // TODO: ports, vectors, etc.
            _irqControl = new Am9519();
            _timerA = new i8254PIT();
            _timerB = new i8254PIT();
            _dmac = new i8237DMA();
            _rtc = new Oki5832RTC();

            // Not sure about this one yet
            //_ioReg3 = new IOReg3(_perqToZ80Fifo, _keyboard, _fdc, _dmaRouter);

            // Same code for EIO/NIO (TODO: all variants?  8"/5.25", 24-bit?)
            _z80Debugger = new Z80Debugger("eioz80.lst");
        }

        // Port "A" is public, since it's a DMA-capable device...
        public override Z80SIO SIOA => _z80sioA;

        // No hard disk seek circuit on the EIO
        public override Z80CTC CTC => null;

        // TBD. I don't think the EIO injects wait states directly?
        // (Status register provides ready bits?)
        protected override bool FIFOInputReady => false;
        protected override bool FIFOOutputReady => false;

        void DeviceInit()
        {
            // Attach the configured tablet(s)
            if (_system.Config.Tablet.HasFlag(TabletType.BitPad))
            {
                var tablet = new BitPadOne(_scheduler, _system);
                _tms9914a.Bus.AddDevice(tablet);
            }

            if (_system.Config.Tablet.HasFlag(TabletType.Kriz))
            {
                var tablet = new KrizTablet(_scheduler, _system);
                _z80sioA.AttachDevice(1, tablet);
            }

            // Attach the keyboard
            _z80sioB.AttachDevice(1, _keyboard);

            // If enabled and configured, attach device to RS232 port A
            if (_system.Config.RSAEnable && Settings.RSADevice != string.Empty)
            {
                if (Settings.RSADevice == "RSX:")
                {
                    var rsx = new RSXFilePort(this);
                    _z80sioA.AttachPortDevice(0, rsx);
                    // _timerA.AttachDevice(0, rsx);
                }
                else
                {
                    var rsa = new PhysicalPort(this, Settings.RSADevice, Settings.RSASettings, "A");
                    _z80sioA.AttachPortDevice(0, rsa);
                    //_timerA.AttachDevice(0, rsa);  figure out how the PIT does it
                }
            }
            else
            {
                // Otherwise direct it to the bit bucket
                _z80sioA.AttachPortDevice(0, new NullPort(this));
            }

            // Now do RS232 port B
            if (_system.Config.RSBEnable && Settings.RSBDevice != string.Empty)
            {
                if (Settings.RSBDevice == "RSX:")
                {
                    var rsx = new RSXFilePort(this);
                    _z80sioB.AttachPortDevice(0, rsx);
                    // _timerB.AttachDevice(0, rsx);
                }
                else
                {
                    var rsb = new PhysicalPort(this, Settings.RSADevice, Settings.RSASettings, "B");
                    _z80sioB.AttachPortDevice(0, rsb);
                    //_timerB.AttachDevice(0, rsb);  figure out how the PIT does it
                }
            }
            else
            {
                // Otherwise direct it to the bit bucket
                _z80sioB.AttachPortDevice(0, new NullPort(this));
            }

            // TODO: now attach everything to the bus
        }

        protected override void DeviceReset()
        {
            throw new NotImplementedException();
        }

        protected override void DeviceShutdown()
        {
            // If serial devices are attached, close them properly
            _z80sioA.DetachDevice(0);   // RS232-A
            _z80sioB.DetachDevice(0);   // RS232-B
        }

        protected override void ClockDMA()
        {
            throw new NotImplementedException();
        }

        public override void WriteStatus(int status)
        {
            throw new NotImplementedException();
        }

        public override int ReadStatus()
        {
            throw new NotImplementedException();
        }

        public override void WriteData(int data)
        {
            throw new NotImplementedException();
        }

        public override int ReadData()
        {
            throw new NotImplementedException();
        }

        public override void QueueKeyboardInput(byte keyCode)
        {
            throw new NotImplementedException();
        }

        public override void DumpFifos()
        {
            throw new NotImplementedException();
        }

        public override void DumpPortAStatus()
        {
            _z80sioA.DumpPortStatus(0);
        }

        public override void DumpPortBStatus()
        {
            _z80sioB.DumpPortStatus(0);
        }

        //
        // EIO/NIO boards
        //
        Am9519 _irqControl;
        Z80SIO _z80sioA, _z80sioB;
        i8254PIT _timerA, _timerB;
        i8237DMA _dmac;
        Oki5832RTC _rtc;
        SerialKeyboard _keyboard;
    }
}
