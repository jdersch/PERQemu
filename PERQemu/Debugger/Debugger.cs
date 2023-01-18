//
// Debugger.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Reflection;

using PERQemu.UI;

namespace PERQemu.Debugger
{
    /// <summary>
    /// Allows the emulator to expose properties as Debugger variables that can
    /// be manipulated by the CLI.
    /// </summary>
    public class DebuggerVariable
    {
        public DebuggerVariable(string name, string description, PropertyInfo property, object instance)
        {
            Name = name;
            Description = description;
            Property = property;
            Instance = instance;
        }

        public string Name;
        public string Description;
        public PropertyInfo Property;
        public object Instance;
    }

    /// <summary>
    /// Provides debugging facilities for the PERQ emulator.  It's neato keen!
    /// </summary>
    public partial class PERQDebugger
    {
        public PERQDebugger(List<object> debugObjects)
        {
            BuildVariableList(debugObjects);
            InitBreakpoints();
        }

        /// <summary>
        /// Evaluates debugger variable expressions.
        /// </summary>
        /// <remarks>
        /// Variables exposed to the debugger are in the form:
        ///     ":<foo>"        (in which case the variable is read)
        ///     ":<foo>=<bar>"  (in which case the variable is modified)
        /// 
        /// <foo> may be a variable, an entire array, or an array element
        /// designated by <foo>[<index>].  <bar> may be a value that matches
        /// <foo>'s type or an expression that evaluates to a value...
        /// </remarks>
        public object EvaluateExpression(string command)
        {
            bool isArray;
            uint arrayIndex;
            DebuggerVariable v;

            //
            // First we just split the expression into its (at most) two parts
            // which are separated by an '=' if present.
            // If no '=' then it's just a matter of evaluating the expression.
            // if there is an '=' then the expression must be evaluated and the
            // result assigned to the variable.
            //
            // So check for that '='.
            //
            if (command.Contains("="))
            {
                // It's an assignment
                var tokens = command.Split(new char[] { '=' });
                string varName = tokens[0];
                string strValue = tokens[1];

                // We assume value is always an int right now because I am lazy
                var value = CommandExecutor.TryParseUint(strValue);

                v = GetVariableFromToken(varName, out isArray, out arrayIndex);

                // If the property is not assignable we can't do anything here
                if (!v.Property.CanWrite && !v.Property.PropertyType.IsArray)
                {
                    throw new ArgumentException("Variable is read-only");
                }

                if (v.Property.PropertyType.IsArray)
                {
                    if (!isArray)
                    {
                        // Cannot do assignment to an entire array
                        throw new ArgumentException("Cannot assign to array, must specify element to assign to");
                    }

                    // Assign to the array element
                    SetArrayElement(v, arrayIndex, value);
                }
                else
                {
                    // Assign to the variable
                    SetVariable(v, value);
                }
            }
            else
            {
                // It's just a variable; get the property from the variable name
                v = GetVariableFromToken(command, out isArray, out arrayIndex);

                if (v.Property.PropertyType.IsArray)
                {

                    if (!isArray)
                    {
                        // Print the contents of the entire array
                        PrintArray(v);
                    }
                    else
                    {
                        // Just print the array element
                        PrintArrayElement(v, arrayIndex);
                    }
                }
                else
                {
                    // Just print the variable's value
                    PrintVariable(v);
                }
            }
            return null;
        }


        void PrintArray(DebuggerVariable v)
        {
            if (v.Property.PropertyType == typeof(Int32[]))
            {
                var array = (Int32[])v.Property.GetValue(v.Instance, null);

                // todo: use Columnify()
                for (int i = 0; i < array.Length; i++)
                {
                    Console.Write("{0}[{1:x2}] = {2:x8}\t", v.Name, i, array[i]);

                    if (i != 0 && (i % 4) == 0)
                    {
                        Console.WriteLine();
                    }
                }
            }
            else
            {
                throw new InvalidOperationException($"Unhandled type {v.Property.PropertyType}");
            }

            Console.WriteLine();
        }


        void PrintArrayElement(DebuggerVariable v, uint arrayIndex)
        {
            if (v.Property.PropertyType == typeof(Int32[]))
            {
                var array = (Int32[])v.Property.GetValue(v.Instance, null);
                Console.WriteLine("{0:x8}", array[arrayIndex]);
            }
            else
            {
                throw new InvalidOperationException($"Unhandled type {v.Property.PropertyType}");
            }
        }


        void SetArrayElement(DebuggerVariable v, uint arrayIndex, uint value)
        {
            if (v.Property.PropertyType == typeof(Int32[]))
            {
                var array = (UInt32[])v.Property.GetValue(v.Instance, null);
                array[arrayIndex] = value;
            }
            else
            {
                throw new InvalidOperationException($"Unhandled type {v.Property.PropertyType}");
            }
        }


        void PrintVariable(DebuggerVariable v)
        {
            // todo: consult the user settings for preferred output radix!
            Console.WriteLine(v.Property.GetValue(v.Instance, null));
        }


        void SetVariable(DebuggerVariable v, uint value)
        {
            v.Property.SetValue(v.Instance, value, null);
        }


        DebuggerVariable GetVariableFromToken(string token, out bool isArray, out uint arrayIndex)
        {
            string varName = string.Empty;
            string index = string.Empty;
            isArray = false;
            arrayIndex = 0;

            VariableParseState state = VariableParseState.ParsingName;

            //
            // Parse out the token, it may be an array access or just a normal variable
            //
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
                            // We add anything (to allow for various bases)
                            // but we'll of course check this for sanity later...
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

            DebuggerVariable match = null;
            PropertyInfo propInfo = null;

            // Look up the variable name and get the property data for it
            foreach (var v in _variableList)
            {
                if (v.Name.ToLower() == varName.ToLower())
                {
                    match = v;
                    propInfo = v.Property;
                    break;
                }
            }

            if (propInfo == null)
            {
                throw new ArgumentException("Invalid variable name");
            }

            // If an array index was specified, then the variable had better be an array
            if (index.Length > 0 && !propInfo.PropertyType.IsArray)
            {
                throw new ArgumentException($"{propInfo.Name} is not an array");
            }

            // We should be good, as long as the index parses; set our out params
            if (index.Length > 0)
            {
                arrayIndex = CommandExecutor.TryParseUint(index);
                isArray = true;
            }

            return match;
        }


        public void ShowVariables()
        {
            foreach (DebuggerVariable var in _variableList)
            {
                Console.WriteLine("{0} - {1}", var.Name, var.Description);
            }
        }


        void BuildVariableList(List<object> debugObjects)
        {
            _variableList = new List<DebuggerVariable>();

            // Go look for Debuggable attributes!
            foreach (object debugObject in debugObjects)
            {
                var type = debugObject.GetType();

                foreach (PropertyInfo info in type.GetProperties(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic))
                {
                    var attribs = info.GetCustomAttributes(typeof(Debuggable), true);

                    if (attribs.Length > 1)
                    {
                        throw new InvalidOperationException($"More than one Debuggable attribute set on {info.Name}");
                    }

                    if (attribs.Length == 1)
                    {
                        // We have a debugger attribute set on this property.
                        // This cast should always succeed given that we're filtering for this type above.
                        var prop = (Debuggable)attribs[0];

                        _variableList.Add(new DebuggerVariable(prop.Name, prop.Description, info, debugObject));
                    }
                }
            }
        }

        enum VariableParseState
        {
            ParsingName,
            ParsingIndex,
            ParsingDone
        }

        List<DebuggerVariable> _variableList;
    }
}

