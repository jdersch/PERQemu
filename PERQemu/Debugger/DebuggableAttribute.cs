// debuggableattribute.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;
using System.Text;

namespace PERQemu.Debugger
{
    /// <summary>
    /// Defines the Debuggable Attribute which is placed on objects we wish to expose to the
    /// debugger.  The debugger will use reflection to access them at runtime.
    /// </summary>
    [AttributeUsage(AttributeTargets.All)]
    public class Debuggable : Attribute
    {
        /// <summary>
        /// Constructor; specifies the friendly name exposed to the debu    gger; this is required.
        /// This is case insensitive (and will be converted to lowercase).
        /// All other attributes are optional.
        /// </summary>
        /// <param name="name"></param>
        public Debuggable(string name)
        {
            _name = name.ToLower();
        }

        /// <summary>
        /// Exposes the friendly name of this object
        /// </summary>
        public string Name
        {
            get { return _name; }
        }

        /// <summary>
        /// Exposes the size (in bits) of the field, where applicable.
        /// Since the PERQ uses some oddly-sized values (20, 48 bits, etc..) this allows
        /// the debugger to display only the pertinent bits...
        /// </summary>
        public int Size
        {
            set { _size = value; }
            get { return _size; }
        }

        private string _name;
        private int _size;
    }
}

