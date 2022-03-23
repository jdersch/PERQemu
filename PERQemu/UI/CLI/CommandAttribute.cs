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
    /// <summary>
    /// Command attribute marks a method to include in the command tree.
    /// A method may be "aliased" by attaching more than one attribute.
    /// </summary>
    /// <remarks>
    /// Name is required; it's the string that places the command in the tree.
    /// Name is broken on whitespace into words and lower-cased for parsing.
    /// 
    /// Description is optional help text displayed by ShowCommands().
    /// 
    /// Discreet may optionally be set to "hide" the command from the
    /// completion engine.  Discreet commands must be typed in full.
    /// 
    /// Repeatable may optionally be set to tell the parser that the command
    /// can be repeated (a shortcut for things like "step", "inst" by just
    /// pressing return rather than retyping or using up arrow.
    /// 
    /// Global optionally flags a command as accessible from any level in the
    /// command tree (i.e., regardless of prefix).  These are only checked if
    /// the typed command does not match the current tree. [not implemented]
    /// 
    /// The (reintroduced) Prefix flag explicitly calls out nodes where the
    /// "commands" list should be built.  This is where we define "subsystems"
    /// such as "configuration", "settings", "debug", etc.
    /// </remarks>
    [AttributeUsage(AttributeTargets.Method, AllowMultiple = true)]
    public class CommandAttribute : Attribute
    {
        public CommandAttribute(string cmdName, string cmdDesc = "")
        {
            _commandName = cmdName;
            _description = cmdDesc;
            _hidden = false;
            _repeats = false;
            _global = false;
            _prefix = false;
        }

        public virtual string Name
        {
            get { return _commandName; }
        }

        public virtual string Description
        {
            get { return _description; }
        }

        public virtual bool Discreet
        {
            get { return _hidden; }
            set { _hidden = value; }
        }

        public virtual bool Repeatable
        {
            get { return _repeats; }
            set { _repeats = value; }
        }

        public virtual bool Global
        {
            get { return _global; }
            set { _global = value; }
        }

        public virtual bool Prefix
        {
            get { return _prefix; }
            set { _prefix = value; }
        }

        private string _commandName;
        private string _description;
        private bool _hidden;
        private bool _repeats;
        private bool _global;
        private bool _prefix;
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

    [AttributeUsage(AttributeTargets.Parameter)]
    public class PathExpandAttribute : Attribute
    {
        public PathExpandAttribute()
        {
            // If present, tells the CommandProcessor that this string argument
            // is a file/path name and to do expansions against the filesystem 
            // which would/will be extremely cool
        }
    }

}
