//
// CommandExecutor.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Diagnostics;
using System.Collections.Generic;
using System.Reflection;
using System.Text;
using System.IO;

namespace PERQemu.UI
{

    /// <summary>
    /// Builds the CLI's command tree, and provides routines to parse command
    /// lines (from keyboard or disk file), validate arguments, and invoke the
    /// appropriate method to carry out the command.
    /// </summary>
    public class CommandExecutor
    {
        public CommandExecutor(List<object> commandObjects)
        {
            BuildCommandTree(commandObjects);
            _currentRoot = _commandRoot;
        }

        public CommandNode CommandTreeRoot
        {
            get { return _commandRoot; }
        }

        public CommandNode CurrentRoot
        {
            get { return _currentRoot; }
            set { _currentRoot = value; }
        }


        public void ExecuteScript(string scriptFile, bool verbose = false)
        {
            using (StreamReader sr = new StreamReader(scriptFile))
            {
                if (verbose) Console.WriteLine($"Reading from '{scriptFile}'...");

                // Scripts always execute from top level
                CurrentRoot = CommandTreeRoot;

                while (!sr.EndOfStream)
                {
                    string line = sr.ReadLine();

                    if (!string.IsNullOrWhiteSpace(line))
                    {
                        if (verbose) Console.WriteLine(line);

                        if (line.StartsWith("@", StringComparison.InvariantCulture))
                        {
                            Console.WriteLine($"** Nested script files not supported: '{line}' ignored");
                        }
                        else
                        {
                            ExecuteLine(line);
                        }
                    }
                }

                sr.Close();
            }
        }


        public void ExecuteLine(string line)
        {
            if (string.IsNullOrWhiteSpace(line) || line.StartsWith("#", StringComparison.CurrentCulture))
            {
                // Comments start with "#", just ignore them; blank/empty lines too
                return;
            }

            // todo: add "." for repeat command processing?
            //      keep a register that contains the last repeatable command
            //      things like 'step' or 'inst' repeat with just <CR>, but
            //      most things shouldn't
            //      '.' by itself clears the register
            //      '.cmd' executes cmd and sets the register
            //      commands that don't repeat just clear it quietly
            //      just make "repeatable" an attribute! off by default
            // todo: add "!" for shell-like history processing?

            if (line.StartsWith(":", StringComparison.CurrentCulture))
            {
                // Expressions start with ":", but require that Sys is instantiated
                if (PERQemu.Sys != null)
                {
                    PERQemu.Sys.Debugger.EvaluateExpression(line.Substring(1));
                }
                else
                {
                    Console.WriteLine("No PERQ defined; can't evaluate variable expression.");
                }
            }
            else if (line.StartsWith("@", StringComparison.CurrentCulture))
            {
                // A line beginning with an "@" indicates a script to execute
                string scriptFile = line.Substring(1);

                ExecuteScript(scriptFile, true);
            }
            else
            {
                string[] args = null;
                CommandNode command = GetCommandFromString(line, out args);

                if (command == null)
                {
#if DEBUG
                    Console.WriteLine($"Invalid command: '{line}'");
#else
                    Console.WriteLine("Invalid command.");
#endif
                }
                else
                {
                    InvokeConsoleMethod(command, args);
                }
            }
        }

        /// <summary>
        /// Pretty print the list of commands for a given starting point (key)
        /// in the command tree.  In debug mode, print the arg types too (more
        /// informative but less aesthetically pleasing).  Could do more here
        /// to coalesce duplicate entries, or introduce a "pager" for long lists
        /// and OMG I've lost my mind
        /// </summary>
        public void ShowCommands(string key)
        {
            if (_commandsAtNode.ContainsKey(key))
            {
                foreach (var val in _commandsAtNode[key])
                {
                    // todo: make the color scheme fit with the overal gestalt
                    // todo: take the terminal width into account
                    Console.ForegroundColor = ConsoleColor.Cyan;
                    Console.Write("{0}{1}", " ".PadLeft(Math.Max(1, val.Pad)), val.Cmd);

#if DEBUG
                    // This messes up our pretty formatting.  Should figure
                    // out a nice way to wrap long lines, then make that the
                    // standard output form?
                    if (val.Args != string.Empty)
                    {
                        Console.ForegroundColor = ConsoleColor.Gray;
                        Console.Write(val.Args);
                    }
#endif

                    Console.ResetColor();
                    Console.WriteLine($" - {val.Desc}");
                }
            }
            else
            {
                Console.WriteLine($"No commands help available for '{key}'");
            }
        }


        /// <summary>
        /// Invokes the method for a command.
        /// </summary>
        private void InvokeConsoleMethod(CommandNode command, string[] args)
        {
            if (command.Method == null)
            {
                throw new ArgumentException("Invalid (null) command");
            }

            ParameterInfo[] parameterInfo = command.Method.Method.GetParameters();
            object[] invokeParams;

            if (args == null)
            {
                invokeParams = null;
            }
            else
            {
                invokeParams = new object[parameterInfo.Length];
            }

            int argIndex = 0;
            for (int paramIndex = 0; paramIndex < parameterInfo.Length; paramIndex++)
            {
                ParameterInfo p = parameterInfo[paramIndex];

                if (p.IsOptional && argIndex >= args.Length)
                {
                    // Run out of supplied arguments, try the default value
                    invokeParams[paramIndex] = p.DefaultValue;

                    // Any remaining arguments must also have default values!
                    continue;
                }

                if (p.ParameterType.IsEnum)
                {
                    // This is an enumeration type; see if we can find an
                    // enumerant that matches the argument
                    FieldInfo[] fields = p.ParameterType.GetFields();

                    foreach (FieldInfo f in fields)
                    {
                        if (!f.IsSpecialName && args[argIndex].ToLower() == f.Name.ToLower())
                        {
                            invokeParams[paramIndex] = f.GetRawConstantValue();
                            break;
                        }
                    }

                    if (invokeParams[paramIndex] == null)
                    {
                        // Should/will have been sanity checked by the parser...
                        throw new ArgumentException($"Unknown value for parameter {argIndex}");
                    }

                    argIndex++;
                }
                else if (p.ParameterType.IsArray)
                {
                    // If a function takes an array type, I should do something here, yeah.
                    argIndex++;
                }
                else
                {
                    if (p.ParameterType == typeof(bool))
                    {
                        invokeParams[paramIndex] = bool.Parse(args[argIndex++]);
                    }
                    else if (p.ParameterType == typeof(int))
                    {
                        invokeParams[paramIndex] = TryParseInt(args[argIndex++]);
                    }
                    else if (p.ParameterType == typeof(uint))
                    {
                        invokeParams[paramIndex] = TryParseUint(args[argIndex++]);
                    }
                    else if (p.ParameterType == typeof(ushort))
                    {
                        invokeParams[paramIndex] = TryParseUshort(args[argIndex++]);
                    }
                    else if (p.ParameterType == typeof(byte))
                    {
                        invokeParams[paramIndex] = TryParseByte(args[argIndex++]);
                    }
                    else if (p.ParameterType == typeof(string))
                    {
                        invokeParams[paramIndex] = args[argIndex++];
                    }
                    else if (p.ParameterType == typeof(char))
                    {
                        invokeParams[paramIndex] = args[argIndex++][0];
                    }
                    else if (p.ParameterType == typeof(float))
                    {
                        invokeParams[paramIndex] = float.Parse(args[argIndex++]);
                    }
                    else
                    {
                        throw new ArgumentException($"Unhandled type for parameter {paramIndex}, type {p.ParameterType}");
                    }
                }
            }

            //
            // If we've made it THIS far, then we were able to parse all the
            // commands into what they should be.  Invoke the method on the
            // object instance associated with it.
            //
            command.Method.Method.Invoke(command.Method.Instance, invokeParams);
        }

        /// <summary>
        /// Splits a command into a list of words, taking care to handle quoted
        /// strings (which count as just one argument).
        /// </summary>
        public static List<string> SplitArgs(string commandString)
        {
            List<string> args = new List<string>();
            StringBuilder sb = new StringBuilder();
            ParseState state = ParseState.NonWhiteSpace;

            commandString = commandString.Trim();

            foreach (char c in commandString)
            {
                switch (state)
                {
                    case ParseState.NonWhiteSpace:
                        if (char.IsWhiteSpace(c))
                        {
                            // End of token
                            args.Add(sb.ToString());
                            sb.Clear();
                            state = ParseState.WhiteSpace;
                        }
                        else if (c == '"')
                        {
                            // Start of quoted string
                            state = ParseState.QuotedString;
                        }
                        else
                        {
                            // Character in token
                            sb.Append(c);
                        }
                        break;

                    case ParseState.WhiteSpace:
                        if (!char.IsWhiteSpace(c))
                        {
                            // Start of new token
                            if (c != '"')
                            {
                                sb.Append(c);
                                state = ParseState.NonWhiteSpace;
                            }
                            else
                            {
                                // Start of quoted string
                                sb.Append(c);
                                state = ParseState.QuotedString;
                            }
                        }
                        break;

                    case ParseState.QuotedString:
                        if (c == '"')
                        {
                            // End of quoted string.  Include the closing '"'!
                            sb.Append(c);
                            args.Add(sb.ToString());
                            sb.Clear();
                            state = ParseState.WhiteSpace;
                        }
                        else
                        {
                            // Character in quoted string
                            sb.Append(c);
                        }
                        break;
                }
            }

            if (sb.Length > 0)
            {
                // If they left a dangling quoted string, complete it!
                if (state == ParseState.QuotedString)
                {
                    sb.Append('"');
                }
                // Add the last token to the args list
                args.Add(sb.ToString());
            }

            return args;
        }

        /// <summary>
        /// Break down a command line into a method to invoke, with (optional)
        /// arguments.  Validates arguments along the way and returns a node
        /// with a valid Method or null.
        /// </summary>
        private CommandNode GetCommandFromString(string command, out string[] args)
        {
            args = null;

            List<string> cmdWords = SplitArgs(command);

            if (cmdWords.Count == 0)
            {
                return null;        // No command, bail out now
            }

            CommandNode current = _currentRoot;
            List<string> argWords = new List<string>();

            while (cmdWords.Count > 0)
            {
                var next = current.FindSubNodeByName(cmdWords[0]);

                if (next != null)
                {
                    current = next;
                }
                else
                {
                    // No subnode match; any arguments here?
                    if (current.Arguments == null)
                    {
                        return null;
                    }

                    ArgumentNode argNode = current.Arguments;

                    // Is it a match?
                    if (!argNode.MatchArgument(cmdWords[0], false /* exact */))
                    {
                        return null;
                    }

                    // Save and advance; ok to trim quotes around strings now!
                    argWords.Add(cmdWords[0].Trim('"'));
                    current = argNode;

                }
                cmdWords.RemoveAt(0);
            }

            // At the end of the command line.  There better be a method here...
            if (current.Method == null)
            {
                return null;    // Ruh roh...
            }

            // If we gathered any arguments, return 'em
            if (argWords.Count > 0)
            {
                args = argWords.ToArray();
            }

            return current;
        }

        public static Radix GetRadix(ref string arg)
        {
            // Special case for C programmers :-)
            if (arg.StartsWith("0x", StringComparison.InvariantCulture))
            {
                arg = arg.Remove(0, 2);
                return Radix.Hexadecimal;
            }

            switch (arg[0])
            {
                case 'b':
                    arg = arg.Remove(0, 1);
                    return Radix.Binary;

                case '%':
                case 'o':
                    arg = arg.Remove(0, 1);
                    return Radix.Octal;

                case 'd':
                    arg = arg.Remove(0, 1);
                    return Radix.Decimal;

                case '$':
                case 'x':
                    arg = arg.Remove(0, 1);
                    return Radix.Hexadecimal;

                default:
                    return Radix.Decimal;
            }
        }

        public static int TryParseInt(string arg)
        {
            int result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToInt32(arg, (int)r);
            }
            catch (OverflowException)
            {
                throw new ArgumentException($"{arg} out of range for a 32-bit {r} value");
            }
            catch (FormatException)
            {
                throw new ArgumentException($"{arg} is not a valid 32-bit {r} value");
            }

            return result;
        }

        public static uint TryParseUint(string arg)
        {
            uint result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToUInt32(arg, (int)r);
            }
            catch (OverflowException)
            {
                throw new ArgumentException($"{arg} out of range for a 32-bit {r} value");
            }
            catch (FormatException)
            {
                throw new ArgumentException($"{arg} is not a valid 32-bit {r} value");
            }

            return result;
        }

        public static ushort TryParseUshort(string arg)
        {
            ushort result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToUInt16(arg, (int)r);
            }
            catch (OverflowException)
            {
                throw new ArgumentException($"{arg} out of range for a 16-bit {r} value");
            }
            catch (FormatException)
            {
                throw new ArgumentException($"{arg} is not a valid 16-bit {r} value");
            }

            return result;
        }

        public static byte TryParseByte(string arg)
        {
            byte result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToByte(arg, (int)r);
            }
            catch (OverflowException)
            {
                throw new ArgumentException($"{arg} out of range for an 8-bit {r} value");
            }
            catch (FormatException)
            {
                throw new ArgumentException($"{arg} is not a valid 8-bit {r} value");
            }

            return result;
        }

        public void UpdateKeywordMatchHelpers(string key, string[] values)
        {
            foreach (var node in _matchDict[key])
            {
                node.SetHelperStrings(values);
            }
        }

        /// <summary>
        /// Builds the CLI command tree.
        /// </summary>
        /// <remarks>
        /// Adds "noise words" or "glue" nodes as SubNodes, but expands method
        /// parameters into an Arguments list (which acts as a singly-linked list
        /// that leads to the Method node).  This allows us to do completions on
        /// enums, booleans or "keyword" strings, which is fun.
        /// 
        /// Also now allows multiple attributes to be set on a method so that
        /// "aliases" can be easily created (such as allowing "exit" or "quit"),
        /// or defining top-level commands that can be accessed from other levels
        /// of the hierarchy.  Does NOT check for name collisions, though...
        /// </remarks>
        private void BuildCommandTree(List<object> commandObjects)
        {
            // Create the root of the command tree
            _commandRoot = new CommandNode("Root", "");
            _commandRoot.Prefix = true;

            // Build a list to hold "commands" help 
            var helpers = new List<CommandHelp>();

            // Temporary list to hold tagged parameters as we discover them
            var setters = new List<ArgumentNode>();

            // Now go discover our CommandAttributes!
            foreach (object commandObject in commandObjects)
            {
                Type type = commandObject.GetType();

                foreach (MethodInfo info in type.GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    object[] attribs = info.GetCustomAttributes(typeof(CommandAttribute), true);

                    // This cast should always succeed given that we're
                    // filtering for this type above.
                    foreach (CommandAttribute function in attribs)
                    {
                        // Reset our list and split the command name into words
                        var tokens = new List<string>(function.Name.Split(' '));

                        if (tokens.Count == 0)
                        {
                            throw new InvalidOperationException($"Command attribute {function.Name} has zero words!?");
                        }

                        // Create the new node to insert
                        var cmdNode = new CommandNode(tokens[tokens.Count - 1],
                                                      function.Description);

                        // Set any attribute flags in the node
                        cmdNode.Prefix = function.Prefix;
                        cmdNode.Hidden = function.Discreet;
                        cmdNode.Repeats = function.Repeatable;

                        // Create a fresh help node
                        var cmdHelp = new CommandHelp(function.Name,
                                                      string.Empty,
                                                      function.Description,
                                                      function.Prefix,
                                                      function.Discreet);

                        // Save a little effort later...
                        if (cmdHelp.Prefix) cmdHelp.Pad = tokens.Count;

                        // Do we have any parameters?
                        var args = info.GetParameters();

                        if (args.Length == 0)
                        {
                            // No argument here
                            cmdNode.Method = new MethodInvokeInfo(info, commandObject);

                            // Add to the root
                            _commandRoot.AddSubNode(tokens, cmdNode);
                        }
                        else if (args.Length == 1)
                        {
                            // Create a new "arguments" node
                            var argNode = new ArgumentNode(args[0].Name, "", args[0],
                                            new MethodInvokeInfo(info, commandObject));

                            // Save the args string
                            cmdHelp.Args = $" [{args[0].Name}]";

                            // Attach it to our command node
                            cmdNode.Arguments = argNode;

                            // Save if Keyword matching
                            if (argNode.DoKeywordMatch) setters.Add(argNode);

                            // And add that to the root
                            _commandRoot.AddSubNode(tokens, cmdNode);
                        }
                        else
                        {
                            // With multiple parameters, we create a new generic
                            // "glue" node from the command given (without method
                            // info) and add a chain of "argument nodes" that will
                            // eventually lead to the method we'll invoke.

                            // Add the generic/glue node to the root
                            _commandRoot.AddSubNode(tokens, cmdNode);

                            // Handle the case where all of the args are optional!
                            if (args[0].IsOptional)
                            {
                                cmdNode.Method = new MethodInvokeInfo(info, commandObject);
                            }

                            var tempRoot = cmdNode;
                            var nextToLastArg = args.Length - 1;

                            // Now loop over the arguments and link 'em in
                            for (var i = 0; i < args.Length; i++)
                            {
                                // Save it to the helper node
                                if (args[i].IsOptional)
                                    cmdHelp.Args += " {" + args[i].Name + "}";
                                else
                                    cmdHelp.Args += $" [{args[i].Name}]";

                                // Build a new parameter node
                                var argNode = new ArgumentNode(args[i].Name, "", args[i]);

                                // Save if Keyword matching
                                if (argNode.DoKeywordMatch) setters.Add(argNode);

                                // Attach the method here?
                                if ((i == nextToLastArg) || (i < nextToLastArg && args[i + 1].IsOptional))
                                {
                                    argNode.Method = new MethodInvokeInfo(info, commandObject);
                                }

                                // Skip overloaded arguments!
                                // "You are not expected to understand this"
                                if ((tempRoot.Arguments != null) &&
                                    (tempRoot.Arguments.Name == argNode.Name))
                                {
                                    argNode = tempRoot.Arguments;
                                }
                                else
                                {
                                    // Assign it
                                    tempRoot.Arguments = argNode;
                                }

                                // Now cheat: arguments always hang off the previous node
                                tempRoot = argNode;
                            }
                        }

                        // Save the help node in the temporary list
                        helpers.Add(cmdHelp);
                    }
                }
            }

            // Sort out the KeywordMatch-tagged parameter nodes
            BuildMatchDictionary(setters);

            // Organize the commands help
            BuildCommandsHelp(helpers);
        }

        /// <summary>
        /// Builds a dictionary that maps a particular keyword to a set of
        /// argument nodes that reference it.
        /// </summary>
        private void BuildMatchDictionary(List<ArgumentNode> list)
        {
            // Set up the dictionary for KeywordMatch-tagged nodes
            _matchDict = new Dictionary<string, ArgumentNode[]>();

            // A temp to hold all the nodes of a given match
            var temp = new List<ArgumentNode>();

            while (list.Count > 0)
            {
                var match = list[0].Keyword;

                // Extract all them matching nodes into temp...
                temp.AddRange(list.FindAll((obj) => obj.Keyword == match));

                // Create the dictionary mapping...
                _matchDict[match] = temp.ToArray();

                // ...then extract those items and whack the temp list!
                list.RemoveAll(x => temp.Contains(x));
                temp.Clear();
            }
        }

        /// <summary>
        /// Precompute a dictionary of nicely formatted lists of commands and
        /// their descriptions, organized by subsystem.  This lets ShowCommands()
        /// run fast without thinking hard.
        /// </summary>
        /// <remarks>
        /// This cheeky little routine transforms the single list of CommandHelp
        /// structs built up as the Command attributes were parsed to build the
        /// command tree.  It plucks out the "prefix" nodes which serve as keys
        /// into the _commandsAtNode dictionary, then filters and transfers the
        /// CommandHelp nodes.  Sure, it's a slow and horrible hack, but it only
        /// runs once at startup and it's way simpler than trying to construct
        /// the list by traversing the command tree after the fact.
        /// </remarks>
        private void BuildCommandsHelp(List<CommandHelp> list)
        {
            // Allocate the dictionary to copy into (by key)
            _commandsAtNode = new Dictionary<string, CommandHelp[]>();

            var keys = new List<CommandHelp>();
            var cmds = new List<CommandHelp>();
            var matches = new List<CommandHelp>();
            int widest = 0;

            // Look through the list to find prefix nodes
            keys.AddRange(list.FindAll((obj) => obj.Prefix));

            // Longest matches first!  (We stashed the # of tokens in Pad :-)
            keys.Sort((x, y) => y.Pad.CompareTo(x.Pad));

            // Add an empty key to catch the leftovers at the end
            keys.Add(new CommandHelp("", "", ""));

            foreach (var key in keys)
            {
                cmds.Clear();
                matches.Clear();

                // A pure, unreconstructed hack
                var match = (key.Cmd == "") ? "" : $"{key.Cmd} ";

                matches.AddRange(list.FindAll((obj) => obj.Cmd.StartsWith(match, StringComparison.InvariantCultureIgnoreCase)));

                // Transfer nodes that match this key
                foreach (var cmd in matches)
                {
#if DEBUG
                    // In DEBUG mode, expose the "hidden" commands...
                    if (!string.IsNullOrEmpty(cmd.Desc))
#else
                    if (!key.Hidden && !cmd.Hidden && !string.IsNullOrEmpty(cmd.Desc))
#endif
                    {
                        cmds.Add(cmd);
                    }

                    // Remove from the master list
                    list.Remove(cmd);
                }

                var cmdArray = cmds.ToArray();
                widest = 0;

                // Now measure and trim the bloody command strings
                for (int i = 0; i < cmdArray.Length; i++)
                {
                    if (cmdArray[i].Cmd.StartsWith(match, StringComparison.InvariantCultureIgnoreCase))
                        cmdArray[i].Cmd = cmdArray[i].Cmd.Substring(match.Length);

                    widest = Math.Max(widest, cmdArray[i].Cmd.Length);
                }

                // Zip through the array and set our pad values
                for (int i = 0; i < cmdArray.Length; i++)
                {
                    cmdArray[i].Pad = (widest - cmdArray[i].Cmd.Length) + 1;
                }

                // Sort by command.  Sort by description is interesting too
                Array.Sort(cmdArray, (x, y) => (x.Cmd.CompareTo(y.Cmd)));

                // Set our empty key for the top-level node
                string k = (key.Cmd == "") ? "root" : key.Cmd;

                // Finally stash 'em in the dictionary
                _commandsAtNode[k] = cmdArray;
            }
        }

        [Conditional("DEBUG")]
        public void DumpCommandTree(CommandNode node, int _indent)
        {
            Console.Write("".PadLeft(_indent));
            Console.WriteLine("Node: {0} - {1} (subnodes={2} hidden={3} prefix={4} repeat={5})",
                              node.Name,
                              node.Description,
                              node.SubNodes.Count,
                              node.Hidden,
                              node.Prefix,
                              node.Repeats
                             );

            if (node.Method != null)
            {
                Console.Write("".PadLeft(_indent + 2));
                Console.WriteLine("Method: {0}", node.Method.Method.Name);
            }

            if (node.Arguments != null)
            {
                ArgumentNode argNode = node.Arguments;

                Console.Write("".PadLeft(_indent + 2));
                Console.WriteLine("Parameter: {0} Pos: {1} {2}{3}{4} Helpers: {5}",
                                  argNode.Name,
                                  argNode.Param.Position,
                                  argNode.Param.IsOptional ? "(Optional)" : "",
                                  argNode.Param.HasDefaultValue ? "(HasDefault) " : "",
                                  argNode.Param.ParameterType.IsEnum ? "(IsEnum) " : "",
                                  string.Join(" ", argNode.Helpers.ToArray()));

                // Chase down the list
                if (argNode.Arguments != null)
                {
                    _indent += 2;
                    DumpCommandTree(argNode, _indent);
                    _indent -= 2;
                }
            }

            if (node.SubNodes.Count > 0)
            {
                foreach (CommandNode sub in node.SubNodes)
                {
                    _indent += 2;
                    DumpCommandTree(sub, _indent);
                    _indent -= 2;
                }
            }
        }


        public struct CommandHelp
        {
            public CommandHelp(string words, string args, string desc, bool pfx = false, bool hide = false)
            {
                Cmd = words;
                Args = args;
                Desc = desc;
                Pad = 0;
                Prefix = pfx;
                Hidden = hide;
            }

            public string Cmd;
            public string Args;
            public string Desc;
            public int Pad;
            public bool Prefix;
            public bool Hidden;
        }

        enum ParseState
        {
            NonWhiteSpace = 0,
            WhiteSpace = 1,
            QuotedString = 2,
        }

        private CommandNode _commandRoot;
        private CommandNode _currentRoot;

        private Dictionary<string, CommandHelp[]> _commandsAtNode;
        private Dictionary<string, ArgumentNode[]> _matchDict;

    }
}
