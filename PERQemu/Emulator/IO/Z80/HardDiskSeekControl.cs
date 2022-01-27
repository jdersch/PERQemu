//
// HardDiskSeekControl.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
    /// AKA "IOREG2"
    /// </summary>
    public sealed class HardDiskSeekControl : IZ80Device
    {
        public HardDiskSeekControl(PERQSystem system)
        {
            _system = system;
        }

        public void Reset()
        {
        }

        public string Name => "Shugart Seek Control";
        public byte[] Ports => _ports;
        public bool IntLineIsActive => false;
        public byte? ValueOnDataBus => null;

        public event EventHandler NmiInterruptPulse;


        public byte Read(byte portAddress)
        {
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            // The PERQ has set up the controller to do everything else,
            // we just send a pulse to seek the drive in whatever direction
            // it was set up to move in.
            if (value != 0)
            {
                _system.IOB.DiskController.DoSingleSeek();
            }
        }


        private byte[] _ports = { 0xd8 };
        private PERQSystem _system;
    }
}