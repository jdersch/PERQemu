//
// Keyboard.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
    /// byte from the keyboard.
    /// </summary>
    public class Keyboard : IZ80Device
    {
        public Keyboard()
        {
            _lastKeycode = 0;
        }

        public void Reset()
        {
            _interruptsEnabled = false;
            _interruptActive = false;
        }

        public string Name => "Keyboard";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x28;    // KBDVEC
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

            Log.Detail(Category.Keyboard, "Queued key '{0}' (0x{1:x2})", (char)_lastKeycode, _lastKeycode);
        }

        public byte Read(byte portAddress)
        {
            if (!_interruptsEnabled)
            {
                Log.Debug(Category.Keyboard, "Read while interrupts disabled (active={0}) at DDS {1}",
                                             _interruptActive, PERQemu.Sys.CPU.DDS);
            }

            Log.Detail(Category.Keyboard, "Read key '{0}' (0x{1:x2})", (char)_lastKeycode, _lastKeycode);

            _interruptActive = false;
            return _lastKeycode;
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException("Keyboard port write");
        }


        byte _lastKeycode;

        bool _interruptsEnabled;
        bool _interruptActive;

        byte[] _ports = { 0x80 };
    }
}
