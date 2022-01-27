//
// TMS9914A.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
    public class TMS9914A : IZ80Device
    {
        public TMS9914A()
        {
        }

        public void Reset()
        {
        }

        public string Name => "TMS9914A";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x22;    // GPIVEC
        public bool IntLineIsActive => _interruptActive;

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            switch (portAddress)
            {
                case 0xb8:  // Interrupt Status 0:
                    return 0x10;        // BO set

                case 0xbb:  // Interrupt status 1:
                    return 0x01;        // IFC (idle state) set

                default:
                    return 0x00;
            }
        }

        public void Write(byte portAddress, byte value)
        {
            
        }

        private bool _interruptActive;

        private byte[] _ports = { 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf };
    }
}
