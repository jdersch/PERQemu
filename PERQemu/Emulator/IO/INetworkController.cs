//
// INetworkController.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using System.Net.NetworkInformation;

namespace PERQemu.IO
{
    public interface INetworkController
    {
        PhysicalAddress MACAddress { get; }
        bool CanReceive { get; }

        void Reset();
        void Shutdown();

        void LoadCommand(int value);
        void LoadRegister(byte address, int value);

        int ReadStatus();
        int ReadRegister(byte address);

        bool WantReceive(PhysicalAddress dest);
        void DoReceive(byte[] packet);

        void DumpEther();
    }
}
