// debugattributes.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.Debugger
{
    public class DebugProperty : Attribute
    {
        public DebugProperty(string friendlyName)
        {
            _friendlyName = friendlyName;
        }

        public DebugProperty(string friendlyName, string description)
        {
            _friendlyName = friendlyName;
            _description = description;
        }

        public string FriendlyName
        {
            get { return _friendlyName; }
        }

        public string Description
        {
            get { return _description; }
        }

        private string _friendlyName;
        private string _description;
    }

    public class DebugFunction : Attribute
    {
        public DebugFunction(string commandName)
        {
            _commandName = commandName;
            _usage = "<No help available>";
        }

        public DebugFunction(string commandName, string description)
        {
            _commandName = commandName;
            _description = description;
        }

        public DebugFunction(string commandName, string description, string usage)
        {
            _commandName = commandName;
            _description = description;
            _usage = usage;
        }

        public string CommandName
        {
            get { return _commandName; }
        }

        public string Usage
        {
            get { return _usage; }
        }

        public string Description
        {
            get { return _description; }
        }

        private string _commandName;
        private string _description;
        private string _usage;
    }
}

