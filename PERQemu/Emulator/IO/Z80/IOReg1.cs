//
// IOReg1.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
    /// Implements I/O Register 1, a single byte I/O device connected to the Z80
    /// that is used to test whether the Z80->PERQ FIFO is ready.
    /// 
    /// TODO: could this just be folded into Z80ToPERQFIFO?
    /// </summary>
    public class IOReg1 : IZ80Device
    {
        public IOReg1(Z80ToPERQFIFO z80PerqFifo)
        {
            _z80PerqFifo = z80PerqFifo;
        }

        public void Reset()
        {
        }

        public string Name => "I/O REG 1";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => null;
        public bool IntLineIsActive => false;
        public bool InterruptsEnabled => false;

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            return (byte)(_z80PerqFifo.IsReady ? 0x0 : 0x40);
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException();
        }

        private byte[] _ports = { 0x88 };

        private Z80ToPERQFIFO _z80PerqFifo;
    }
}
