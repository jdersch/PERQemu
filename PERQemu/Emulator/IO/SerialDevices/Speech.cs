//
// Speech.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// The PERQ "speech" device uses the MC3417 CVSD chip to produce audio
    /// output from a serial byte stream.  This is a placeholder for future
    /// connections to the SDL2 audio plumbing.
    /// </summary>
    public class Speech : ISIODevice
    {
        public Speech()
        {
        }

        public void RegisterReceiveDelegate(ReceiveDelegate rxDelegate)
        {
            throw new NotImplementedException();
        }

        public void Reset()
        {
            throw new NotImplementedException();
        }

        public void Transmit(byte value)
        {
            throw new NotImplementedException();
        }

        public void TransmitAbort()
        {
            throw new NotImplementedException();
        }

        public void TransmitBreak()
        {
            throw new NotImplementedException();
        }
    }
}
