//
// NullPort.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// Placeholder for unassigned RS232 ports.  Acts as a sink for all data
    /// sent to it; never sources any data.  Looks like a typical 9600-8-N-1
    /// port with no active modem control signals.
    /// </summary>
    public sealed class NullPort : SerialDevice
    {
        public NullPort(Z80System sys) : base(sys)
        {
        }

        public override string Name => "Unassigned RS232 port";
    }
}
