//
// DebugAttribute.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.Debugger
{
    /// <summary>
    /// Defines the Debuggable Attribute which is placed on objects we wish to
    /// expose to the debugger.  The debugger will use reflection to access them
    /// at runtime.
    /// </summary>
    [AttributeUsage(AttributeTargets.All)]
    public class Debuggable : Attribute
    {
        public Debuggable(string name, string desc = "")
        {
            _name = name.ToLower();
            _description = desc;
        }

        /// <summary>
        /// Gives the attributed property a human-readable name for the object
        /// that is exposed to the debugger.  It must be unique.
        /// </summary>
        public string Name
        {
            get { return _name; }
        }

        /// <summary>
        /// An (optional) description of the object.
        /// </summary>
        public string Description
        {
            get { return _description; }
        }

        /// <summary>
        /// Exposes the size (in bits) of the field, where applicable.
        /// Since the PERQ uses some oddly-sized values (20, 48 bits, etc.)
        /// this allows the debugger to display only the pertinent bits...
        /// </summary>
        public int Size
        {
            set { _size = value; }
            get { return _size; }
        }

        string _name;
        string _description;
        int _size;
    }
}

