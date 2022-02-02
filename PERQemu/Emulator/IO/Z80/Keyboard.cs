//
// Keyboard.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The PERQ-1 IOB Keyboard is an 8-bit parallel register that stores one
    /// byte from the keyboard.  TODO: on the EIO, the keyboard uses a serial
    /// channel on the SIO chip, so it's totally different.  Oof.
    /// </summary>
    public class Keyboard : IZ80Device
    {
        public Keyboard()
        {
            _lastKeycode = 0;
            Reset();
        }

        public void Reset()
        {
            _interruptsEnabled = false;
            _interruptActive = false;
        }

        public string Name => "Keyboard";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x28; // KBDVEC
        public bool IntLineIsActive => _interruptActive && _interruptsEnabled;

        public bool InterruptsEnabled
        {
            get { return _interruptsEnabled; }
            set { _interruptsEnabled = value; }
        }

        public event EventHandler NmiInterruptPulse;

        public void QueueInput(byte key)
        {
            _lastKeycode = key;
            _interruptActive = true;
        }

        public byte Read(byte portAddress)
        {
            if (!_interruptsEnabled)
            {
                Log.Debug(Category.Keyboard,
                          "Read while interrupts ignored (key {0} at dds {1}, {2} cycles)",
                          _lastKeycode, PERQemu.Sys.CPU.DDS, PERQemu.Sys.IOB.Z80System.Clocks);
            }

            _interruptActive = false;
            return _lastKeycode;
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException();
        }

        private byte _lastKeycode;

        private bool _interruptsEnabled;
        private bool _interruptActive;

        private byte[] _ports = new byte[] { 0x80 };
    }
}
