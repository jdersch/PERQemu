//
// CommandAttribute.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu
{
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class CommandAttribute : Attribute
    {
        public CommandAttribute(string cmdName, string cmdDesc = "")
        {
            _commandName = cmdName;
            _description = cmdDesc;
            _hidden = false;
        }

        public virtual string Name
        {
            get { return _commandName; }
        }

        public virtual string Description
        {
            get { return _description; }
        }

        public virtual bool IsDiscreet
        {
            get { return _hidden; }
            set { _hidden = value; }
        }

        private string _commandName;
        private string _description;
        private bool _hidden;
    }

    [AttributeUsage(AttributeTargets.Parameter)]
    public class KeywordMatchAttribute : Attribute
    {
        public KeywordMatchAttribute()
        {
            // If present, tells the CommandProcessor to match a string
            // argument against the Helpers like an enumeration!
        }
    }
}
