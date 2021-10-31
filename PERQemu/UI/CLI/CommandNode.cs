//
// CommandNode.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Reflection;

namespace PERQemu.UI
{

    /// <summary>
    /// Info about methods to invoke and their (optional) arguments.
    /// </summary>
    public class MethodInvokeInfo
    {
        public MethodInvokeInfo(MethodInfo method, object instance)
        {
            if (method == null || instance == null)
            {
                throw new ArgumentNullException("Command method and instance must be non-null");
            }

            Method = method;
            Instance = instance;
        }

        public MethodInfo Method;
        public object Instance;
    }

    /// <summary>
    /// Argument nodes are fancy CommandNodes (allows for command completion of
    /// enumerated types, optional parameters, and human-readable help strings)
    /// </summary>
    public class ArgumentNode : CommandNode
    {

        public ArgumentNode(string name, string desc, ParameterInfo p) : base(name, desc)
        {
            Param = p;
            Helpers = new List<string>();

            SetHelperStrings();
        }

        public ArgumentNode(string name, string desc, ParameterInfo p, MethodInvokeInfo methodInvoke) : this(name, desc, p)
        {
            Method = methodInvoke;
        }


        public ParameterInfo Param;
        public List<string> Helpers;

        /// <summary>
        /// Match a string to this node's parameter.  If "fuzzy" then do a
        /// partial match (enum or boolean types), otherwise do an exact match.
        /// Returns null if the the parameter is null or the argument doesn't
        /// match (or is malformed).
        /// </summary>
        /// <remarks>
        /// For now I let integer types through rather than range and format
        /// check them here; they'll be caught/rejected with a helpful error
        /// message by GetCommandFromString.  This is more aesthetically
        /// pleasing and doesn't interrupt the user interaction as much, though
        /// it delays the feedback when a bad numeric parameter is entered.
        /// </remarks>
        public bool MatchArgument(string arg, bool fuzzy)
        {
            try
            {
                if (Param == null)      // No arg?
                    return false;       // No match

                if (Param.ParameterType.IsEnum || Param.ParameterType == typeof(bool))
                {
                    if (fuzzy)
                    {
                        return Helpers.Exists(x => x.StartsWith(arg, StringComparison.InvariantCultureIgnoreCase));
                    }
                    else
                    {
                        return Helpers.Exists(x => x.Equals(arg, StringComparison.InvariantCultureIgnoreCase));
                    }
                }
                else if (Param.ParameterType == typeof(char))
                {
                    return arg.Length > 0;      // force this to be ONE char?
                }
                else if (Param.ParameterType == typeof(string))
                {
                    return !string.IsNullOrEmpty(arg);
                }
                else if (Param.ParameterType == typeof(uint))
                {
                    //CommandExecutor.TryParseUint(arg);
                    return true;
                }
                else if (Param.ParameterType == typeof(ushort))
                {
                    //CommandExecutor.TryParseUshort(arg);
                    return true;
                }
                else if (Param.ParameterType == typeof(byte))
                {
                    //CommandExecutor.TryParseByte(arg);
                    return true;
                }
                else
                {
                    Console.WriteLine("** Unhandled parameter type for param " + Param.Name);
                    return false;
                }
            }
            catch
            {
                return false;
            }
        }

        private void SetHelperStrings()
        {
            if (Param.ParameterType.IsEnum)
            {
                // Expand the enumeration type and format them as strings
                var names = Param.ParameterType.GetEnumNames();

                foreach (string s in names)
                {
                    Helpers.Add(s.ToLower());
                }
            }
            else if (Param.ParameterType == typeof(bool))
            {
                Helpers.Add("true");
                Helpers.Add("false");
            }
            else if (Param.ParameterType == typeof(uint))
            {
                Helpers.Add(String.Format("[unsigned int] ({0})", Param.Name));
            }
            else if (Param.ParameterType == typeof(ushort))
            {
                Helpers.Add(String.Format("[0..65535] ({0})", Param.Name));
            }
            else if (Param.ParameterType == typeof(byte))
            {
                Helpers.Add(String.Format("[0..255] ({0})", Param.Name));
            }
            else if (Param.ParameterType == typeof(string))
            {
                Helpers.Add(String.Format("[string] ({0})", Param.Name));
            }
            else if (Param.ParameterType == typeof(float))
            {
                Helpers.Add(String.Format("[float] ({0})", Param.Name));
            }
            else if (Param.ParameterType == typeof(char))
            {
                Helpers.Add("[char]");
            }
            else
            {
                // Anything we don't handle yet...
                Helpers.Add(Param.ParameterType.ToString());
            }
        }
    }

    /// <summary>
    /// Defines a node in the CLI command tree.
    /// </summary>
    public class CommandNode
    {
        public CommandNode(string name, string desc)
        {
            Name = name.Trim().ToLower();
            Description = desc;
            Hidden = false;
            Method = null;
            Arguments = null;
            SubNodes = new List<CommandNode>();
        }

        public CommandNode(string name, string desc, MethodInvokeInfo methodInvoke) : this(name, desc)
        {
            Method = methodInvoke;
        }

        public string Name;
        public string Description;
        public bool Hidden;

        public MethodInvokeInfo Method;

        public ArgumentNode Arguments;
        public List<CommandNode> SubNodes;


        public override string ToString()
        {
            if (this.SubNodes.Count > 1)
            {
                return string.Format("{0} ({1})", this.Name, this.SubNodes.Count);
            }
            else
            {
                return this.Name;
            }
        }

        /// <summary>
        /// Add a subnode to the tree, with intervening nodes if required.
        /// </summary>
        /// <remarks>
        /// If the node to add already exists, do an "update in place" if it
        /// has Arguments or Methods.  This offends the sensibilities but is
        /// crudely effective.
        /// </remarks>
        public void AddSubNode(List<string> words, CommandNode cmd)
        {
            if (words.Count == 0)
            {
                return;
            }

            // Has the subnode for the first incoming word already been added?
            var subNode = FindSubNodeByName(words[0]);

            if (subNode == null)
            {
                // Is this the last word?
                if (words.Count == 1)
                {
                    this.SubNodes.Add(cmd);     // Add the new node here
                    return;                     // and we're done
                }
                else
                {
                    subNode = new CommandNode(words[0], "");
                    this.SubNodes.Add(subNode); // Add a glue node, continue
                }
            }
            else
            {
                // The node exists; are we at the last word?
                if (words.Count == 1)
                {
                    // Yes... so update in place.  Oof.
                    if (subNode.Arguments == null && cmd.Arguments != null)
                    {
                        subNode.Arguments = cmd.Arguments;
                    }

                    if (subNode.Method == null && cmd.Method != null)
                    {
                        subNode.Method = cmd.Method;
                    }

                    return;     // Done
                }
            }

            // Remove the word and recurse!
            words.RemoveAt(0);
            subNode.AddSubNode(words, cmd);
        }

        public CommandNode FindSubNodeByName(string name)
        {
            foreach (CommandNode sub in SubNodes)
            {
                if (sub.Name == name)
                {
                    return sub;
                }
            }
            return null;
        }
    }
}