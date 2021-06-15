// debugger.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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
using System.Reflection;
using System.Text;
using System.IO;
using PERQemu.CPU;
using PERQemu.Memory;
using PERQemu.IO.Z80.IOB;

namespace PERQemu.Debugger
{

    /// <summary>
    /// Delegate for a command execution
    /// </summary>
    /// <param name="args">The args passed to this command</param>
    public delegate void Executor(string[] args);

    /// <summary>
    /// Defines a node in the debug command tree.
    /// </summary>
    public class DebuggerCommand
    {
        public DebuggerCommand(string name, String description, String usage, MethodInfo method)
        {
            Name = name.Trim().ToLower();
            Description = description;
            Usage = usage;
            Methods = new List<MethodInfo>(4);

            if (method != null)
            {
                Methods.Add(method);
            }

            SubCommands = new List<DebuggerCommand>();
        }

        public string Name;
        public string Description;
        public string Usage;
        public List<MethodInfo> Methods;
        public List<DebuggerCommand> SubCommands;

        public override string ToString()
        {
            if (this.Methods.Count == 0)
            {
                return String.Format("{0}... ({1})", this.Name, this.SubCommands.Count);
            }
            else
            {
                return this.Name;
            }
        }

        public void AddSubNode(List<string> words, MethodInfo method)
        {
            // We should never hit this case.
            if (words.Count == 0)
            {
                throw new InvalidOperationException("Out of words building command node.");
            }

            // Check the root to see if a node for the first incoming word has already been added
            DebuggerCommand subNode = FindSubNodeByName(words[0]);

            if (subNode == null)
            {
                // No, it has not -- create one and add it now.
                subNode = new DebuggerCommand(words[0], null, null, null);
                this.SubCommands.Add(subNode);

                if (words.Count == 1)
                {
                    // This is the last stop -- set the method and be done with it now.
                    subNode.Methods.Add(method);

                    // early return.
                    return;
                }
            }
            else
            {
                // The node already exists, we will be adding a subnode, hopefully.
                if (words.Count == 1)
                {
                    //
                    // If we're on the last word at this point then this is an overloaded command.
                    // Check that we don't have any other commands with this number of arguments.
                    //
                    int argCount = method.GetParameters().Length;
                    foreach (MethodInfo info in subNode.Methods)
                    {
                        if (info.GetParameters().Length == argCount)
                        {
                            throw new InvalidOperationException("Duplicate overload for debug command");
                        }
                    }

                    //
                    // We're ok.  Add it to the method list.
                    //
                    subNode.Methods.Add(method);

                    // and return early.
                    return;
                }
            }

            // We have more words to go.
            words.RemoveAt(0);
            subNode.AddSubNode(words, method);

        }

        public DebuggerCommand FindSubNodeByName(string name)
        {
            DebuggerCommand found = null;

            foreach (DebuggerCommand sub in SubCommands)
            {
                if (sub.Name == name)
                {
                    found = sub;
                    break;
                }
            }

            return found;
        }
    }

    public class DebuggerVariable
    {
        public DebuggerVariable(string name, String description, PropertyInfo property)
        {
            Name = name;
            Description = description;
            Property = property;
        }

        public string Name;
        public string Description;
        public PropertyInfo Property;

    }

    /// <summary>
    /// Provides debugging facilities for the PERQ emulator.
    /// It's neato keen!
    /// </summary>
    public class Debugger
    {
        public Debugger(PERQSystem system)
        {
            BuildCommandTree();
            BuildVariableList();

            _debuggerPrompt = new DebuggerPrompt(_commandRoot);
            _system = system;
        }

        /// <summary>
        /// Enters the debugger.
        /// </summary>
        /// <param name="message"></param>
        public RunState Enter(string message)
        {
            _nextState = RunState.Debug;

            if (!string.IsNullOrEmpty(message))
            {
                Console.WriteLine(message);
            }
            PrintStatus();
            return Run();
        }

        public RunState RunScript(string scriptFile)
        {
            _nextState = RunState.DebugScript;

            StreamReader reader = null;

            try
            {
                reader = new StreamReader(scriptFile);

                while (!reader.EndOfStream && _nextState == RunState.DebugScript)
                {
                    string command = reader.ReadLine();

                    if (command != String.Empty)
                    {
                        Console.WriteLine(command);
                        ExecuteLine(command);
                    }
                }

                if (reader.EndOfStream && _nextState == RunState.DebugScript)
                {
                    // script done.  return to debug mode
                    _nextState = RunState.Debug;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine(e.Message);
                _nextState = RunState.Debug;
            }
            finally
            {
                if (reader != null)
                {
                    reader.Close();
                }
            }

            return _nextState;
        }

        private RunState Run()
        {
            while (_nextState == RunState.Debug)
            {
                try
                {
                    // Get the command string from the prompt.
                    string command = _debuggerPrompt.Prompt().Trim();

                    if (command != String.Empty)
                    {
                        ExecuteLine(command);
                        _lastCommand = command;
                    }
                    else if (!string.IsNullOrWhiteSpace(_lastCommand))
                    {
                        Console.WriteLine("Repeating: {0}", _lastCommand);
                        ExecuteLine(_lastCommand);
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }

            return _nextState;
        }

        private void ExecuteLine(string line)
        {
            // Expressions start with ":"
            if (line.StartsWith(":"))
            {
                EvaluateExpression(line.Substring(1));
            }
            // Comments start with "#"
            else if (line.StartsWith("#"))
            {
                // Do nothing, ignore.
            }
            // Scripts start with "@"
            else if (line.StartsWith("@"))
            {
                // Strip script name off and try to parse it.
            }
            else
            {
                string[] args = null;
                List<MethodInfo> methods = GetMethodsFromCommandString(line, out args);

                if (methods == null)
                {
                    // Not a command.
                    Console.WriteLine("Invalid command.");
                }
                else
                {
                    InvokeDebugMethod(methods, args);
                }
            }
        }

        private void InvokeDebugMethod(List<MethodInfo> methods, string[] args)
        {
            MethodInfo method = null;

            //
            // Find the method that matches the arg count we were passed
            // (i.e. handle overloaded commands)
            //
            foreach (MethodInfo m in methods)
            {
                ParameterInfo[] paramInfo = m.GetParameters();

                if (args == null && paramInfo.Length == 0 ||
                    paramInfo.Length == args.Length)
                {
                    // found a match
                    method = m;
                    break;
                }
            }

            if (method == null)
            {
                // invalid argument count.
                // todo: display usage?
                throw new ArgumentException(String.Format("Invalid argument count to command."));
            }

            ParameterInfo[] parameterInfo = method.GetParameters();
            object[] invokeParams;

            if (args == null)
            {
                invokeParams = null;
            }
            else
            {
                invokeParams = new object[parameterInfo.Length];
            }

            for (int i = 0; i < parameterInfo.Length; i++)
            {
                ParameterInfo p = parameterInfo[i];

                if (p.ParameterType.IsEnum)
                {
                    //
                    // This is an enumeration type.
                    // See if we can find an enumerant that matches the argument.
                    //
                    FieldInfo[] fields = p.ParameterType.GetFields();

                    foreach (FieldInfo f in fields)
                    {
                        if (!f.IsSpecialName && args[i].ToLower() == f.Name.ToLower())
                        {
                            invokeParams[i] = f.GetRawConstantValue();
                        }
                    }

                    if (invokeParams[i] == null)
                    {
                        // no match, provide possible values
                        StringBuilder sb = new StringBuilder(String.Format("Invalid value for parameter {0}.  Possible values are:", i));

                        foreach (FieldInfo f in fields)
                        {
                            if (!f.IsSpecialName)
                            {
                                sb.AppendFormat("{0} ", f.Name);
                            }
                        }

                        sb.AppendLine();

                        throw new ArgumentException(sb.ToString());
                    }

                }
                else if (p.ParameterType.IsArray)
                {
                    //
                    // If a function takes an array type, i should do something here, yeah.
                    //
                }
                else
                {
                    // must be something more normal...
                    if (p.ParameterType == typeof(uint))
                    {
                        invokeParams[i] = TryParseUint(args[i]);
                    }
                    else if (p.ParameterType == typeof(ushort))
                    {
                        invokeParams[i] = TryParseUshort(args[i]);
                    }
                    else if (p.ParameterType == typeof(string))
                    {
                        invokeParams[i] = args[i];
                    }
                    else if (p.ParameterType == typeof(char))
                    {
                        invokeParams[i] = (char)args[i][0];
                    }
                    else
                    {
                        throw new ArgumentException(String.Format("Unhandled type for parameter {0}, type {1}", i, p.ParameterType));
                    }
                }
            }

            //
            // If we've made it THIS far, then we were able to parse all the commands into what they should be.
            // Invoke the method on the instance exposed by PERQSystem.
            //
            object instance = GetInstanceFromMethod(method);

            method.Invoke(instance, invokeParams);
        }

        private object GetInstanceFromProperty(PropertyInfo property)
        {
            // Grab the object instance from the System.
            // TODO: would be nice to have a more dynamic way to do this.
            switch (property.DeclaringType.Name)
            {
                case "Debugger":
                    return _system.Debugger;

                case "PERQCpu":
                    return _system.CPU;

                case "PERQSystem":
                    return _system;

                case "Z80System":
                    return _system.IOB.Z80System;
            }

            return null;
        }

        private object GetInstanceFromMethod(MethodInfo method)
        {
            // Grab the object instance from the System.
            // TODO: would be nice to have a more dynamic way to do this.
            switch (method.DeclaringType.Name)
            {
                case "Debugger":
                    return _system.Debugger;

                case "PERQCpu":
                    return _system.CPU;

                case "PERQSystem":
                    return _system;

                case "Z80System":
                    return _system.IOB.Z80System;
            }

            return null;
        }

        private object EvaluateExpression(string command)
        {
            // tokenize the expression
            //
            // We expect an expression of the form:
            // - ":<foo>" (in which case the variable is read)
            //     (<foo> may be a variable, an entire array, or an array element designated by <foo>[<index>])
            // - ":<foo>=<bar>" (in which case the variable is modified)
            //     <bar> may be a value that matches <foo>'s type or an expression that evaluates to a value...
            //

            //
            // First we just split the expression into its (at most) two parts which are separated by an '='
            // if present.
            // If there's no '=' then it's just a matter of evaluating the expression.
            // if there is an '=' then the expression must be evaluated and the result assigned to the variable.
            //

            // So check for that '='.

            if (command.Contains("="))
            {
                // It's an assignment.
                string[] tokens = command.Split(new char[] { '=' });
                string variable = tokens[0];
                string strValue = tokens[1];

                // We assume value is always an int right now because I am lazy.
                uint value = TryParseUint(strValue);

                // Get the property from the variable name
                bool isArray;
                uint arrayIndex;
                PropertyInfo property = GetPropertyFromToken(variable, out isArray, out arrayIndex);

                // If the property is not assignable, then we can't do anything here.
                if (!property.CanWrite && !property.PropertyType.IsArray)
                {
                    throw new ArgumentException("Variable is read-only.");
                }

                if (property.PropertyType.IsArray)
                {
                    if (!isArray)
                    {
                        //
                        // Cannot do assignment to an entire array.
                        //
                        throw new ArgumentException("Cannot assign to array, must specify element to assign to.");
                    }
                    else
                    {
                        //
                        // Assign to the array element
                        //
                        SetArrayElement(property, arrayIndex, value);
                    }
                }
                else
                {
                    //
                    // Assign to the variable.
                    //
                    SetVariable(property, value);
                }
            }
            else
            {
                // It's just a variable.
                // Get the property from the variable name
                bool isArray;
                uint arrayIndex;
                PropertyInfo property = GetPropertyFromToken(command, out isArray, out arrayIndex);

                if (property.PropertyType.IsArray)
                {

                    if (!isArray)
                    {
                        //
                        // Print the contents of the entire array.
                        //
                        PrintArray(command, property);
                    }
                    else
                    {
                        //
                        // just print the array element
                        //
                        PrintArrayElement(property, arrayIndex);
                    }
                }
                else
                {
                    //
                    // Just print the variable's value
                    //
                    PrintVariable(property);
                }
            }

            return null;
        }

        private void PrintArrayElement(PropertyInfo property, uint arrayIndex)
        {
            object instance = GetInstanceFromProperty(property);

            if (property.PropertyType == typeof(Int32[]))
            {
                Int32[] array = (Int32[])property.GetValue(instance, null);
                Console.WriteLine("{0:x8}", array[arrayIndex]);
            }
            else
            {
                throw new InvalidOperationException(string.Format("Unhandled type {0}", property.PropertyType));
            }
        }

        private void SetArrayElement(PropertyInfo property, uint arrayIndex, uint value)
        {
            object instance = GetInstanceFromProperty(property);

            if (property.PropertyType == typeof(Int32[]))
            {
                UInt32[] array = (UInt32[])property.GetValue(instance, null);
                array[arrayIndex] = value;
            }
            else
            {
                throw new InvalidOperationException(string.Format("Unhandled type {0}", property.PropertyType));
            }
        }

        private void PrintArray(string name, PropertyInfo property)
        {
            object instance = GetInstanceFromProperty(property);

            if (property.PropertyType == typeof(Int32[]))
            {
                Int32[] array = (Int32[])property.GetValue(instance, null);

                for (int i = 0; i < array.Length; i++)
                {
                    Console.Write("{0}[{1:x2}] = {2:x8}\t", name, i, array[i]);

                    if (i != 0 && (i % 4) == 0)
                    {
                        Console.WriteLine();
                    }
                }
            }
            else
            {
                throw new InvalidOperationException(string.Format("Unhandled type {0}", property.PropertyType));
            }

            Console.WriteLine();
        }

        private void PrintVariable(PropertyInfo property)
        {
            object instance = GetInstanceFromProperty(property);
            Console.WriteLine(property.GetValue(instance, null));
        }

        private void SetVariable(PropertyInfo property, uint value)
        {
            object instance = GetInstanceFromProperty(property);
            property.SetValue(instance, value, null);
        }

        private PropertyInfo GetPropertyFromToken(string token, out bool isArray, out uint arrayIndex)
        {
            //
            // Parse out the token, it may be an array access or just a normal variable
            //
            string varName = String.Empty;
            string index = String.Empty;
            isArray = false;
            arrayIndex = 0;

            VariableParseState state = VariableParseState.ParsingName;

            for (int i = 0; i < token.Length; i++)
            {
                char c = token[i];

                switch (state)
                {
                    case VariableParseState.ParsingName:
                        if (c == '[')
                        {
                            state = VariableParseState.ParsingIndex;
                        }
                        else if (c >= 'a' && c <= 'z')
                        {
                            varName += c;
                        }
                        else
                        {
                            throw new ArgumentException("Invalid syntax");
                        }
                        break;

                    case VariableParseState.ParsingIndex:
                        if (c == ']')
                        {
                            state = VariableParseState.ParsingDone;
                        }
                        else
                        {
                            //
                            // We add anything (to allow for various bases)
                            // but we'll of course check this for sanity later...
                            //
                            index += c;
                        }
                        break;

                    case VariableParseState.ParsingDone:
                        if (!char.IsWhiteSpace(c))
                        {
                            throw new ArgumentException("Invalid syntax");
                        }
                        break;
                }
            }

            //
            // OK, we made it here.  Check a few things.
            //
            if (index.Length > 0 && state != VariableParseState.ParsingDone)
            {
                throw new ArgumentException("Invalid syntax");
            }

            //
            // Look up the variable name and get the property data for it
            //
            PropertyInfo propInfo = null;
            foreach (DebuggerVariable var in _variableList)
            {
                if (var.Name == varName)
                {
                    propInfo = var.Property;
                    break;
                }
            }

            if (propInfo == null)
            {
                throw new ArgumentException("Invalid variable name");
            }

            //
            // If an array index was specified, then the variable had better be an array.
            //
            if (index.Length > 0 && !propInfo.PropertyType.IsArray)
            {
                throw new ArgumentException("{0} is not an array");
            }

            //
            // Otherwise we should be good, as long as the index parses.
            // Set our out params.
            //
            if (index.Length > 0)
            {
                arrayIndex = TryParseUint(index);
                isArray = true;
            }

            return propInfo;
        }

        private List<MethodInfo> GetMethodsFromCommandString(string command, out string[] args)
        {
            args = null;

            List<string> cmdArgs =
                new List<string>(
                    command.Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries));

            DebuggerCommand current = _commandRoot;
            int commandIndex = 0;

            while (true)
            {
                // If this node has an executor, then we're done
                // (We assume that the tree is correctly built and that only
                // leaves have executors)
                if (current.Methods.Count > 0)
                {
                    break;
                }

                if (commandIndex > cmdArgs.Count - 1)
                {
                    // Out of args!
                    return null;
                }

                // Otherwise we continue down the tree.
                current = current.FindSubNodeByName(cmdArgs[commandIndex]);
                commandIndex++;

                if (current == null)
                {
                    // If the node was not found, then the command is invalid.
                    return null;
                }
            }

            // Now current should point to the command with the executor
            // and commandIndex should point to the first argument to the command.

            cmdArgs.RemoveRange(0, commandIndex);

            args = cmdArgs.ToArray();
            return current.Methods;
        }

        private void PrintStatus()
        {
            _system.IOB.Z80System.ShowZ80State();
            _system.CPU.ShowPC();

            Console.WriteLine("ucode {0}",
                        Disassembler.Disassemble(_system.CPU.PC, _system.CPU.GetInstruction(_system.CPU.PC)));

            Console.WriteLine("inst  {0:x2}-{1} (@BPC {2})",
                        _system.MemoryBoard.OpFile[_system.CPU.BPC],
                        QCode.QCodeHelper.GetQCodeFromOpCode(_system.MemoryBoard.OpFile[_system.CPU.BPC]).Mnemonic,
                        _system.CPU.BPC);
        }

        [DebugFunction("go", "Starts or continues execution of the PERQ.")]
        private void Go()
        {
            _nextState = RunState.Run;
        }

        [DebugFunction("step", "Runs the PERQ for one microcycle.")]
        private void Step()
        {
            _nextState = RunState.SingleStep;
        }

        [DebugFunction("inst", "Runs the PERQ for one bytecode instruction dispatch.")]
        private void Inst()
        {
            _nextState = RunState.RunInst;
        }

        [DebugFunction("z80step", "Runs the PERQ for one Z80 instruction.")]
        private void Z80Step()
        {
            _nextState = RunState.RunZ80Inst;
        }

        [DebugFunction("reset", "Resets the PERQ.")]
        private void Reset()
        {
            _nextState = RunState.Reset;
        }

        [DebugFunction("exit", "Leaves the emulator.  Any disk state not saved will be lost.")]
        private void Exit()
        {
            _nextState = RunState.Exit;
        }

        /*
                private void SetBreakpoint(string[] args)
                {

                }

                private void ClearBreakpoint(string[] args)
                {

                }

                private void ToggleBreakpoint(string[] args)
                {

                }

                private void ShowBreakpoints(string[] args)
                {

                }
        */

#if TRACING_ENABLED
        [DebugFunction("set logging", "Enables logging for the specified event types")]
        private void SetLogging(LogType type)
        {
            Trace.TraceLevel |= type;
        }

        [DebugFunction("show logging", "Shows the currently enabled logging settings")]
        private void ShowLogging()
        {
            Console.WriteLine(Trace.TraceLevel);
        }

        [DebugFunction("clear logging", "Disables logging for the specified event types")]
        private void ClearLogging(LogType type)
        {
            Trace.TraceLevel &= ~type;
        }
#endif

        [DebugFunction("show variables", "Shows debugger variables and their descriptions.")]
        private void ShowVariables()
        {
            foreach (DebuggerVariable var in _variableList)
            {
                Console.WriteLine("{0} - {1}", var.Name, var.Description);
            }
        }

        [DebugFunction("show commands", "Shows debugger commands and their descriptions.")]
        private void ShowCommands()
        {
            foreach (DebuggerCommand cmd in _commandList)
            {
                Console.WriteLine("{0} - {1}", cmd.Name, cmd.Description);
            }
        }

        private static uint TryParseUint(string arg)
        {
            uint result = 0;
            bool hexadecimal = false;

            // If args starts with a "0x" it's hex, otherwise assume decimal
            if (arg.StartsWith("0x"))
            {
                hexadecimal = true;

                // Strip the "0x"
                arg = arg.Remove(0, 2);
            }

            try
            {
                result = uint.Parse(arg, hexadecimal ? System.Globalization.NumberStyles.HexNumber : System.Globalization.NumberStyles.Integer);
            }
            catch
            {
                Console.WriteLine("{0} was not a valid 32-bit decimal or hexadecimal constant.", arg);
                throw;
            }

            return result;
        }

        private static ushort TryParseUshort(string arg)
        {
            ushort result = 0;
            bool hexadecimal = false;

            // If args starts with a "0x" it's hex, otherwise assume decimal
            if (arg.StartsWith("0x"))
            {
                hexadecimal = true;

                // Strip the "0x"
                arg = arg.Remove(0, 2);
            }

            try
            {
                result = ushort.Parse(arg, hexadecimal ? System.Globalization.NumberStyles.HexNumber : System.Globalization.NumberStyles.Integer);
            }
            catch
            {
                Console.WriteLine("{0} was not a valid 16-bit decimal or hexadecimal constant.", arg);
                throw;
            }

            return result;
        }

        public static bool IsPrintable(char c)
        {
            return (Char.IsLetterOrDigit(c) ||
                    Char.IsSymbol(c) ||
                    Char.IsPunctuation(c));
        }

        /// <summary>
        /// Builds the debugger command tree.
        /// </summary>
        private void BuildCommandTree()
        {
            // Build the flat list which will be built into the tree.
            // I'm kinda overloading the use of DebuggerCommand here.  Sue me.
            _commandList = new List<DebuggerCommand>();

            Type[] debugTypes = {
                    typeof(PERQSystem),
                    typeof(PERQCpu),
                    typeof(Z80System),
                    typeof(IO.Z80_new.Z80System),
                    typeof(Debugger) };

            foreach (Type type in debugTypes)
            {
                foreach (MethodInfo info in type.GetMethods(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    object[] attribs = info.GetCustomAttributes(typeof(DebugFunction), true);

                    if (attribs.Length > 1)
                    {
                        throw new InvalidOperationException(String.Format("More than one DebugFunction attribute set on {0}", info.Name));
                    }
                    else if (attribs.Length == 1)
                    {
                        // We have a debugger attribute set on this method.
                        // This cast should always succeed given that we're filtering for this type above.
                        DebugFunction function = (DebugFunction)attribs[0];

                        DebuggerCommand newCommand = new DebuggerCommand(function.CommandName, function.Description, function.Usage, info);

                        _commandList.Add(newCommand);
                    }
                }
            }

            // Now actually build the command tree from the above list!
            _commandRoot = new DebuggerCommand("Root", null, null, null);

            foreach (DebuggerCommand c in _commandList)
            {
                string[] commandWords = c.Name.Split(' ');

                // This is kind of ugly, we know that at this point every command built above have only
                // one method.  When building the tree, overloaded commands may end up with more than one.
                _commandRoot.AddSubNode(new List<string>(commandWords), c.Methods[0]);
            }
        }

        private void BuildVariableList()
        {
            _variableList = new List<DebuggerVariable>();

            Type[] debugTypes = {
                    typeof(PERQCpu),
                    typeof(Z80System),
                    typeof(Debugger) };

            foreach (Type type in debugTypes)
            {
                foreach (PropertyInfo info in type.GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    object[] attribs = info.GetCustomAttributes(typeof(DebugProperty), true);

                    if (attribs.Length > 1)
                    {
                        throw new InvalidOperationException(String.Format("More than one DebugProperty attribute set on {0}", info.Name));
                    }
                    else if (attribs.Length == 1)
                    {
                        // We have a debugger attribute set on this property.
                        // This cast should always succeed given that we're filtering for this type above.
                        DebugProperty prop = (DebugProperty)attribs[0];

                        DebuggerVariable newVariable = new DebuggerVariable(prop.FriendlyName, prop.Description, info);

                        _variableList.Add(newVariable);
                    }
                }
            }
        }

        private enum VariableParseState
        {
            ParsingName,
            ParsingIndex,
            ParsingDone
        }

        private DebuggerPrompt _debuggerPrompt;
        private DebuggerCommand _commandRoot;
        private List<DebuggerCommand> _commandList;
        private static List<DebuggerVariable> _variableList;
        private string _lastCommand;

        private RunState _nextState;

        private PERQSystem _system;
    }
}

