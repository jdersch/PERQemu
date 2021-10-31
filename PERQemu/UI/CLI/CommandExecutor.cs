//
// CommandExecutor.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Text;
using System.IO;

using PERQemu;

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
                Console.WriteLine("Reading from '{0}'...", scriptFile);

                while (!sr.EndOfStream)
                {
                    string line = sr.ReadLine();

                    if (!string.IsNullOrWhiteSpace(line))
                    {
                        if (verbose) Console.WriteLine(line);
                        ExecuteLine(line);
                    }
                }

                sr.Close();
            }
        }


        public void ExecuteLine(string line)
        {
            if (line.StartsWith("#", StringComparison.CurrentCulture))
            {
                // Comments start with "#", just ignore them
            }
            else if (line.StartsWith("@", StringComparison.CurrentCulture))
            {
                // A line beginning with an "@" indicates a script to execute.
                string scriptFile = line.Substring(1);

                ExecuteScript(scriptFile, true);
            }
            else
            {
                string[] args = null;
                CommandNode command = GetCommandFromString(line, out args);

                if (command == null)
                {
                    Console.WriteLine("Invalid command.");
                }
                else
                {
                    InvokeConsoleMethod(command, args);
                }
            }
        }


        public void ShowCommands()
        {
            ShowCommands(_commandRoot);
        }

        public void ShowCommands(string prefix)
        {
            ShowCommands(_commandRoot.FindSubNodeByName(prefix));
        }

        /// <summary>
        /// Shows a top-level command summary.
        /// </summary>
        public void ShowCommands(CommandNode root)
        {
            // Start at the given root
            foreach (var cmd in root.SubNodes)
            {
                if (!cmd.Hidden && !string.IsNullOrEmpty(cmd.Description))
                {
                    if (cmd.Arguments == null)
                    {
                        Console.WriteLine("{0} - {1}", cmd.Name, cmd.Description);
                    }
                    else
                    {
                        var helper = cmd.Arguments;

                        if (helper.Helpers.Count == 1)
                        {
                            Console.WriteLine("{0} [{1}] - {2}", cmd.Name, cmd.Arguments.Name, cmd.Description);
                        }
                        else
                        {
                            Console.WriteLine("{0} [*] - {1}", cmd.Name, cmd.Description);
                        }
                    }
                }
            }
        }

        /// <summary>
        /// Invokes the method for a command.
        /// </summary>
        private void InvokeConsoleMethod(CommandNode command, string[] args)
        {
            if (command.Method == null)
            {
                throw new ArgumentException("Invalid (null) command.");
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

                    // At this point, we bail out?
                    break;
                }

                if (p.ParameterType.IsEnum)
                {
                    // This is an enumeration type. See if we can find an
                    // enumerant that matches the argument.
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
                        throw new ArgumentException(String.Format("Unknown value for parameter {0}.", argIndex));
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
                        invokeParams[paramIndex] = (char)args[argIndex++][0];
                    }
                    else if (p.ParameterType == typeof(float))
                    {
                        invokeParams[paramIndex] = float.Parse(args[argIndex++]);
                    }
                    else
                    {
                        throw new ArgumentException(String.Format("Unhandled type for parameter {0}, type {1}", paramIndex, p.ParameterType));
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

        enum ParseState
        {
            NonWhiteSpace = 0,
            WhiteSpace = 1,
            QuotedString = 2,
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
                        else if (c == '\"')
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
                            if (c != '\"')
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
                        if (c == '\"')
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
                    sb.Append('\"');
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
                    else
                    {
                        cmdWords[0].Trim('\"'); // Okay to strip quotes now
                        argWords.Add(cmdWords[0]);  // Save it
                        current = argNode;          // Advance
                    }
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
                    // todo: accept 0x for the C programmers... :-)
                    arg = arg.Remove(0, 1);
                    return Radix.Hexadecimal;

                default:
                    return Radix.Decimal;
            }
        }

        private static uint TryParseUint(string arg)
        {
            uint result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToUInt32(arg, (int)r);
            }
            catch (System.OverflowException)
            {
                throw new ArgumentException(String.Format("{0} out of range for a 32-bit {1} value.", arg, r));
            }
            catch (System.FormatException)
            {
                throw new ArgumentException(String.Format("{0} is not a valid 32-bit {1} value.", arg, r));
            }

            return result;
        }

        private static ushort TryParseUshort(string arg)
        {
            ushort result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToUInt16(arg, (int)r);
            }
            catch (System.OverflowException)
            {
                throw new ArgumentException(String.Format("{0} out of range for a 16-bit {1} value.", arg, r));
            }
            catch (System.FormatException)
            {
                throw new ArgumentException(String.Format("{0} is not a valid 16-bit {1} value.", arg, r));
            }

            return result;
        }

        private static byte TryParseByte(string arg)
        {
            byte result = 0;
            Radix r = GetRadix(ref arg);

            try
            {
                result = Convert.ToByte(arg, (int)r);
            }
            catch (System.OverflowException)
            {
                throw new ArgumentException(String.Format("{0} out of range for an 8-bit {1} value.", arg, r));
            }
            catch (System.FormatException)
            {
                throw new ArgumentException(String.Format("{0} is not a valid 8-bit {1} value.", arg, r));
            }

            return result;
        }

        /// <summary>
        /// Builds the CLI command tree.
        /// </summary>
        /// <remarks>
        /// Adds "noise words" or "glue" nodes as SubNodes, but expands method
        /// parameters into an Arguments list (which acts as a singly-linked list
        /// that lead to the Method node).  This allows us to do completions on
        /// enums or booleans, which is fun.
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
                            throw new InvalidOperationException("Command attribute with zero words!? " + function.Name);
                        }

                        // Create the new node to insert
                        var cmdNode = new CommandNode(tokens[tokens.Count - 1],
                                                      function.Description);

                        // The better part of valor?
                        cmdNode.Hidden = function.IsDiscreet;

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

                            // Attach it to our command node
                            cmdNode.Arguments = argNode;

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

                            var tempRoot = cmdNode;

                            // Now loop over the arguments and link 'em in
                            for (var i = 0; i < args.Length; i++)
                            {
                                // Build a new parameter node
                                var argNode = new ArgumentNode(args[i].Name, "", args[i]);

                                // Attach the method here?
                                if ((args[i].Position == args.Length - 1) ||
                                    (i < args.Length - 1 && args[i + 1].IsOptional))
                                {
                                    argNode.Method = new MethodInvokeInfo(info, commandObject);
                                }

                                // Assign it
                                tempRoot.Arguments = argNode;

                                // Now cheat: arguments always hang off the previous node
                                tempRoot = argNode;
                            }
                        }
                    }
                }
            }
        }

//#if DEBUG
        public void DumpCommandTree(CommandNode node)
        {
            Console.Write("".PadLeft(_indent));
            Console.WriteLine("Node: {0} - {1} (subnodes={2} hidden={3})",
                              node.Name,
                              node.Description,
                              node.SubNodes.Count,
                              node.Hidden
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
                Console.WriteLine("Parameter: {0} Pos: {1} {2}{3} Helpers: {4}",
                                  argNode.Name,
                                  argNode.Param.Position,
                                  argNode.Param.HasDefaultValue ? "(HasDefault) " : "",
                                  argNode.Param.ParameterType.IsEnum ? "(IsEnum) " : "",
                                  String.Join(" ", argNode.Helpers.ToArray()));

                // Chase down the list
                if (argNode.Arguments != null)
                {
                    _indent += 2;

                    DumpCommandTree(argNode);
                    _indent -= 2;
                }
            }

            if (node.SubNodes.Count > 0)
            {
                foreach (CommandNode sub in node.SubNodes)
                {
                    _indent += 2;
                    DumpCommandTree(sub);
                    _indent -= 2;
                }
            }
        }

        private int _indent = 0;
//#endif

        private CommandNode _commandRoot;
        private CommandNode _currentRoot;
    }
}
