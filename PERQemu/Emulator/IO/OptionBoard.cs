//
// OptionBoard.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO
{
    /// <summary>
    /// A base class for the PERQ's CPU Option and I/O Option boards.
    /// </summary>
    /// <remarks>
    /// The PERQ's 5-slot card cage contains two option slots: CPU Option,
    /// for which no dedicated boards were designed, and the IO Option, which
    /// came in a number of configurations.  For emulation purposes, all of
    /// the optional devices are attached on one OptionBoard.  See the file
    /// Docs/IOBoards.txt for more information.
    /// </remarks>
    public abstract class OptionBoard : IIODevice
    {
        static OptionBoard()
        {
            // Our derived classes customize themselves in the static
            // constructor, setting up ports and other goodies
        }

        public OptionBoard(PERQSystem system)
        {
            _sys = system;
        }

        public bool HandlesPort(byte port)
        {
            return _portsHandled[port];
        }

        //
        // Basic operations
        //

        public virtual void Reset()
        {
            Log.Info(Category.IO, "{0} board reset", _name);
        }

        public abstract uint Clock();

        public abstract int IORead(byte port);

        public abstract void IOWrite(byte port, int value);

        public virtual void LoadDisk()
        {
        }

        public virtual void LoadTape()
        {
        }

        public virtual void SaveDisk()
        {
        }

        public virtual void SaveTape()
        {
        }

        /// <summary>
        /// Populate the port lookup table.
        /// </summary>
        protected void RegisterPorts(byte[] ports)
        {
            foreach (var p in ports)
            {
                if (_portsHandled[p])
                {
                    Log.Warn(Category.IO, "Port {0:x} already registered!", p);
                }

                _portsHandled[p] = true;
            }
        }

        // Describe the specific board
        protected static string _name;
        protected static string _desc;

        // I/O port map for this board
        private static bool[] _portsHandled = new bool[256];

        // Parent
        protected PERQSystem _sys;
    }
}
