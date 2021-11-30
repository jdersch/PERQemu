//
// Keyboard.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

        public bool IntLineIsActive => _interruptActive && _interruptsEnabled;

        public byte? ValueOnDataBus => 0x28; //KBDVEC

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
