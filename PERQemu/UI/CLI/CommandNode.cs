//
// CommandNode.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
            if (method == null)
                throw new ArgumentNullException(nameof(method), "Command method must be non-null");
            
            if (instance == null)
                throw new ArgumentNullException(nameof(instance), "Command instance must be non-null");
            
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
            DoPathExpand = false;
            Keyword = string.Empty;

            SetHelperStrings();

            if (p.GetCustomAttribute(typeof(PathExpandAttribute)) != null)
            {
                // Sanity check
                if (p.ParameterType != typeof(string))
                    throw new ArgumentException("Cannot tag non-string parameters with [PathExpand]");

                // Just sets a flag to alter the command prompt's behavior
                DoPathExpand = true;
            }

            var kma = p.GetCustomAttribute(typeof(KeywordMatchAttribute)) as KeywordMatchAttribute;
            if (kma != null)
            {
                if (p.ParameterType != typeof(string))
                    throw new ArgumentException("Cannot tag non-string parameters with [KeywordMatch]");

                // Set a flag for quick test, save the keyword this node should
                // match.  The setter for the helper strings finds the nodes to
                // update by a string match/dictionary lookup.
                DoKeywordMatch = true;
                Keyword = kma.Keyword;
            }
        }

        public ArgumentNode(string name, string desc, ParameterInfo p, MethodInvokeInfo methodInvoke) : this(name, desc, p)
        {
            Method = methodInvoke;
        }

        public ParameterInfo Param;
        public List<string> Helpers;

        public bool DoPathExpand;
        public bool DoKeywordMatch;
        public string Keyword;


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

                // Check the enumerated types for text matches
                if (Param.ParameterType.IsEnum || Param.ParameterType == typeof(bool))
                {
                    if (fuzzy)
                    {
                        return Helpers.Exists(x => x.StartsWith(arg, StringComparison.InvariantCultureIgnoreCase));
                    }

                    return Helpers.Exists(x => x.Equals(arg, StringComparison.InvariantCultureIgnoreCase));
                }

                // Check the rest
                if (Param.ParameterType == typeof(string))
                {
                    // Just check for null, with or without optional keyword matching
                    return !string.IsNullOrEmpty(arg);
                }

                if (Param.ParameterType == typeof(char))
                {
                    return arg.Length == 1;
                }

                if (Param.ParameterType == typeof(int) ||
                    Param.ParameterType == typeof(uint) ||
                    Param.ParameterType == typeof(ushort) ||
                    Param.ParameterType == typeof(byte))
                {
                    // skip the CommandExecutor.TryParseXXXX(arg) -- see above
                    return true;
                }

                // Should never get here!
                Console.WriteLine($"** Unhandled parameter type for param {Param.Name}");
                return false;
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
            else if (Param.ParameterType == typeof(int))
            {
                Helpers.Add($"[integer] ({Param.Name})");
            }
            else if (Param.ParameterType == typeof(uint))
            {
                Helpers.Add($"[unsigned int] ({Param.Name})");
            }
            else if (Param.ParameterType == typeof(ushort))
            {
                Helpers.Add($"[0..65535] ({Param.Name})");
            }
            else if (Param.ParameterType == typeof(byte))
            {
                Helpers.Add($"[0..255] ({Param.Name})");
            }
            else if (Param.ParameterType == typeof(string))
            {
                Helpers.Add($"[string] ({Param.Name})");
            }
            else if (Param.ParameterType == typeof(float))
            {
                Helpers.Add($"[float] ({Param.Name})");
            }
            else if (Param.ParameterType == typeof(char))
            {
                Helpers.Add($"[char] ({Param.Name})");
            }
            else
            {
                // Anything we don't handle yet...
                Helpers.Add(Param.ParameterType.ToString());
            }
        }


        /// <summary>
        /// For parameters with the [KeywordMatch] attribute, provide a hook to
        /// set/update the strings used in matching/completion.
        /// </summary>
        public void SetHelperStrings(string[] words)
        {
            Helpers.Clear();

            foreach (var w in words)
                Helpers.Add(w);
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
            Prefix = false;
            Repeats = false;
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
        public bool Prefix;
        public bool Repeats;

        public MethodInvokeInfo Method;

        public ArgumentNode Arguments;
        public List<CommandNode> SubNodes;


        public override string ToString()
        {
            if (SubNodes.Count > 1)
            {
                // Note: this isn't quite accurate given the way parameters
                // are handled now, and I think it's too "busy" visually...
                //return $"{Name} ({SubNodes.Count})... ";
                return $"{Name}... ";
            }

            return Name;
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
                    SubNodes.Add(cmd);          // Add the new node here
                    return;                     // and we're done
                }

                subNode = new CommandNode(words[0], "");
                SubNodes.Add(subNode);          // Add a glue node, continue
            }
            else
            {
                // The node exists; are we at the last word?
                if (words.Count == 1)
                {
                    // Yes... so update in place.  Oof.
                    if (cmd.Arguments != null)
                    {
                        if (subNode.Arguments == null)
                        {
                            subNode.Arguments = cmd.Arguments;
                        }
                        else
                        {
                            // If the existing argument node matches the new cmd,
                            // then check if they match; if so, the cmd's argument
                            // node has the method we want to attach!  Allowing
                            // overloaded parameter lists is hairy...
                            if ((cmd.Arguments.Name == subNode.Arguments.Name) &&
                                (cmd.Arguments.Method != null) &&
                                (subNode.Arguments.Method == null))
                            {
                                subNode.Arguments.Method = cmd.Arguments.Method;
                            }
                        }
                    }
                    else
                    {
                        if (subNode.Arguments != null)
                        {
                            cmd.Arguments = subNode.Arguments;  // XXX
                        }
                    }

                    if (subNode.Method == null && cmd.Method != null)
                    {
                        subNode.Method = cmd.Method;
                    }

                    // Update flags, since we can't guarantee the order in which
                    // nodes will be inserted.  NB: only positive assertions!
                    if (cmd.Prefix && !subNode.Prefix)
                    {
                        subNode.Prefix = cmd.Prefix;
                    }
                    if (cmd.Hidden && !subNode.Hidden)
                    {
                        subNode.Hidden = cmd.Hidden;
                    }
                    if (cmd.Repeats && !subNode.Repeats)
                    {
                        subNode.Repeats = cmd.Repeats;
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