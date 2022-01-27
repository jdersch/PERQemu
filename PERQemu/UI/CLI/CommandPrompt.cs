//
// CommandPrompt.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Text;

namespace PERQemu.UI
{
    /// <summary>
    /// Interactive part of the command-line interface.  Provides for editing
    /// functions, tab-completion, history recall, and other nifty stuff.
    /// </summary>
    public class CommandPrompt
    {

        public CommandPrompt(CommandNode root)
        {
            _commandTreeRoot = root;
            _commandTree = root;
            _commandHistory = new List<string>(64);
            _historyIndex = 0;
            _prompt = "";
        }

        /// <summary>
        /// Invoke a "subsystem" -- change the prompt and narrow our grammar to
        /// include only the nodes below it.  Saves typing and looks old school.
        /// </summary>
        public CommandNode SetPrefix(string subsys)
        {
            var nodes = CommandExecutor.SplitArgs(subsys);
            var newRoot = _commandTreeRoot;

            foreach (var word in nodes)
            {
                newRoot = newRoot.FindSubNodeByName(word);

                if (newRoot == null)
                {
                    Log.Error(Category.Emulator, "Bad call to SetPrefix: couldn't set root to {0}", subsys);
                    return _commandTree;
                }
            }

            // Turtles all the way down
            _prompt = subsys;
            _commandTree = newRoot;

            return _commandTree;
        }

        /// <summary>
        /// Called when we exit a subsystem and return to the root prompt.
        /// </summary>
        public CommandNode ResetPrefix()
        {
            _commandTree = _commandTreeRoot;
            _prompt = "";

            return _commandTree;
        }

        /// <summary>
        /// Runs a nifty interactive debugger prompt.
        /// </summary>
        public string GetLine()
        {
            DisplayPrompt();
            ClearInput();
            UpdateOrigin();

            bool entryDone = false;

            while (!entryDone)
            {
                UpdateDisplay();

                // Read one keystroke from the console...
                ConsoleKeyInfo key = Console.ReadKey(true);

                // TODO: allow the classic emacs-like control chars too!

                // Parse special chars...
                switch (key.Key)
                {
                    case ConsoleKey.Escape:         // Clear input 
                        ClearInput();
                        break;

                    case ConsoleKey.Backspace:      // Delete last char
                        DeleteCharAtCursor(true /* backspace */);
                        break;

                    case ConsoleKey.Delete:         // Delete char at cursor
                        DeleteCharAtCursor(false /* delete */);
                        break;

                    case ConsoleKey.LeftArrow:
                        MoveLeft();
                        break;

                    case ConsoleKey.RightArrow:
                        MoveRight();
                        break;

                    case ConsoleKey.UpArrow:
                        HistoryPrev();
                        break;

                    case ConsoleKey.DownArrow:
                        HistoryNext();
                        break;

                    case ConsoleKey.Home:
                        MoveToBeginning();
                        break;

                    case ConsoleKey.End:
                        MoveToEnd();
                        break;

                    case ConsoleKey.Tab:
                        if (!InsideString())
                        {
                            DoCompletion(false /* silent */);
                        }
                        else
                        {
                            InsertChar(' ');    // fixme: convert to a space or ignore?
                        }
                        break;

                    case ConsoleKey.Spacebar:
                        if (!InsideString() &&
                            !_input.EndsWith(" ", StringComparison.CurrentCulture) &&
                            DoCompletion(true /* silent */))
                        {
                            UpdateDisplay();
                        }
                        else
                        {
                            InsertChar(key.KeyChar);
                        }
                        break;

                    case ConsoleKey.Enter:
                        DoCompletion(true /* silent */);

                        UpdateDisplay();
                        Console.WriteLine();
                        entryDone = true;
                        break;

                    default:
                        // Not a special key, just insert it if it's deemed printable.
                        if (char.IsLetterOrDigit(key.KeyChar) ||
                            char.IsPunctuation(key.KeyChar) ||
                            char.IsSymbol(key.KeyChar) ||
                            char.IsWhiteSpace(key.KeyChar))
                        {
                            InsertChar(key.KeyChar);
                        }
                        break;
                }
            }

            // Done.  Add to history if input is non-empty
            if (_input != string.Empty)
            {
                _commandHistory.Add(_input);
                HistoryIndex = _commandHistory.Count - 1;
            }

            return _input;
        }


        private void DisplayPrompt()
        {
            // (Hack) This is soooooper cheesy but if there's a "cleaner" way to
            // do it, it'd be sweet to have the editor do this automatically. :-)
            if ((_commandTree.Name == "configure" && PERQemu.Config.Changed) ||
                (_commandTree.Name == "settings" && Settings.Changed))
                Console.Write(_prompt + "*> ");
            else
                Console.Write(_prompt + "> ");
        }


        private void UpdateDisplay()
        {
            // If the current input string is shorter than the last, then we
            // need to erase a few chars at the end.
            string clear = string.Empty;

            int row, column;

            if (_input.Length < _lastInputLength)
            {
                StringBuilder sb = new StringBuilder(_lastInputLength - _input.Length);
                for (int i = 0; i < _lastInputLength - _input.Length; i++)
                {
                    sb.Append(' ');
                }

                clear = sb.ToString();
            }

            // If running under the profiler or on the IDE's console, avoid a
            // divide-by-zero (which goes into an infinite loop).  Needed for
            // the old Xamarin Studio/Profiler but maybe not on Visual Studio?
            if (Console.BufferWidth == 0)
            {
                column = _textPosition + _originColumn;
                row = _originRow;
            }
            else
            {
                column = ((_textPosition + _originColumn) % Console.BufferWidth);
                row = ((_textPosition + _originColumn) / Console.BufferWidth) + _originRow;
            }

            // Move cursor to origin to draw string
            Console.CursorLeft = _originColumn;
            Console.CursorTop = _originRow;
            Console.Write(_input + clear);

            // Move cursor to text position to draw cursor
            Console.CursorLeft = column;
            Console.CursorTop = row;
            Console.CursorVisible = true;

            _lastInputLength = _input.Length;
        }


        private bool InsideString()
        {
            var result = false;

            // Scan from the start of the line to where the cursor is now!
            foreach (var c in _input.ToCharArray(0, TextPosition))
            {
                if (c == '\"') { result = !result; }
            }

            return result;
        }

        private void MoveToBeginning()
        {
            TextPosition = 0;
        }

        private void MoveToEnd()
        {
            TextPosition = _input.Length;
        }

        private void MoveLeft()
        {
            TextPosition--;
        }

        private void MoveRight()
        {
            TextPosition++;
        }

        private void InsertChar(char c)
        {
            _input = _input.Insert(TextPosition, c.ToString());
            TextPosition++;
        }

        private void DeleteCharAtCursor(bool backspace)
        {
            if (_input.Length == 0)
            {
                return;         // Nothing to delete, bail
            }

            if (backspace)
            {
                if (TextPosition == 0)
                {
                    return;     // At the beginning of input, can't backspace 
                }
                else
                {
                    // Remove 1 char at the position before the cursor,
                    // and move the cursor back one char
                    _input = _input.Remove(TextPosition - 1, 1);
                    TextPosition--;
                }
            }
            else
            {
                if (TextPosition == _input.Length)
                {
                    return;     // At the end of input, can't delete
                }
                else
                {
                    // Remove one char at the current cursor position,
                    // but do not move the cursor
                    _input = _input.Remove(TextPosition, 1);
                }
            }
        }

        private void HistoryPrev()
        {
            if (HistoryIndex < _commandHistory.Count)
            {
                _input = _commandHistory[HistoryIndex];
                TextPosition = _input.Length;
                HistoryIndex--;
            }
        }

        private void HistoryNext()
        {
            if (HistoryIndex < _commandHistory.Count)
            {
                HistoryIndex++;
                _input = _commandHistory[HistoryIndex];
                TextPosition = _input.Length;
            }
            else
            {
                _input = string.Empty;
            }
        }

        private void ClearInput()
        {
            _input = string.Empty;
            HistoryIndex = _commandHistory.Count - 1;
            TextPosition = 0;
        }

        private void UpdateOrigin()
        {
            _originRow = Console.CursorTop;
            _originColumn = Console.CursorLeft;
        }

        private int TextPosition
        {
            get { return _textPosition; }
            set
            {
                // Clip input between 0 and the length of input (+1, to allow adding text at end)
                _textPosition = Math.Max(0, value);
                _textPosition = Math.Min(_textPosition, _input.Length);
            }
        }

        private int HistoryIndex
        {
            get { return _historyIndex; }
            set
            {
                _historyIndex = Math.Min(_commandHistory.Count - 1, value);
                _historyIndex = Math.Max(0, _historyIndex);
            }
        }


        public struct CompletionList
        {
            public CompletionList(CommandNode c, string s)
            {
                SearchRoot = c;
                Match = s;
                Completions = new List<string>();
            }

            public CommandNode SearchRoot;
            public string Match;
            public List<string> Completions;
        }

        /// <summary>
        /// Provide command completion based on the current input line.
        /// </summary>
        private bool DoCompletion(bool silent)
        {
            // Save off the current cursor row; this is an ugly-ish hack to
            // detect whether the match process output anything.  If the cursor
            // row changes then we'll need to move the prompt.
            int oldRow = Console.CursorTop;
            bool changed = false;

            CompletionList result = GetCompletions(_commandTree, _input);

            // If not running silent and we have completions, spit 'em out
            if (!silent && result.Completions.Count > 0)
            {
                Console.WriteLine();
                Console.WriteLine("Possible completions are:");
                PERQemu.CLI.Columnify(result.Completions.ToArray());
                DisplayPrompt();
            }

            // Did our input line change?
            if (result.Match != string.Empty)
            {
                changed = _input.Trim().ToLower() != result.Match.Trim().ToLower();

                _input = result.Match;
                TextPosition = _input.Length;
            }

            // If the cursor moved, update the input line
            if (!silent && oldRow != Console.CursorTop)
            {
                DisplayPrompt();
                UpdateOrigin();
            }

            return changed;
        }


        /// <summary>
        /// Return a list of possible word completions for the input line.
        /// </summary>
        /// <remarks>
        /// For a given input string and starting node, returns the disambiguated
        /// matching command string, expanded up to the point where one or more
        /// completions are available; the completion options are returned as a
        /// list.  Returns null and an empty match string if the user (or parser)
        /// is off into the weeds.
        /// </remarks>
        private CompletionList GetCompletions(CommandNode root, string input)
        {
            var result = new CompletionList(root, input);
            var tokens = CommandExecutor.SplitArgs(input);

            if (tokens.Count == 0)
            {
                result = FindCompletions(root, "");
                result.Completions.Sort();
                result.Match = string.Empty;
                return result;
            }

            StringBuilder sb = new StringBuilder();

            while (tokens.Count > 0)
            {
                result = FindCompletions(root, tokens[0]);

                // If there's no match at all, the input is bogus or we're
                // editing the command line (or entering a parameter).  In
                // that case, fall back and punt.  We'll resume completion
                // when the cursor is back to the end of the input line.
                if (result.SearchRoot == null)
                {
                    result.Match = string.Empty;
                    result.Completions.Clear();

                    return result;
                }

                // Advance to the next node
                root = result.SearchRoot;

                // We have a match for the current token.  If this isn't the
                // last thing on the input line, we _should_ only get one
                // unambiguous match, but if we don't, the right thing happens
                // anyway (no expansion, line remains intact, or it properly
                // expands as far as it can).
                if (tokens.Count > 1)
                {
                    // Expand our matched text
                    sb.AppendFormat("{0} ", result.Match);
                }
                else
                {
                    // At the last input token: decide what to do with the
                    // result from FindCompletions
                    if (result.Completions.Count > 1)
                    {
                        // An ambiguous match!  Save the partially disambiguated
                        // input, if any; we'll fall through and return
                        sb.Append(result.Match);
                    }
                    else if (result.Completions.Count == 1)
                    {
                        // An unambiguous match! Save the (possibly expanded) word
                        sb.AppendFormat("{0} ", result.Match);

                        // Clear completions and update for the result node
                        result.Completions.Clear();

                        // Add any SubNodes
                        if (root.SubNodes.Count > 0)
                        {
                            foreach (CommandNode c in root.SubNodes)
                            {
                                result.Completions.Add(c.ToString());
                            }
                        }

                        // If THIS node takes a parameter, add that too
                        if (root.Arguments != null)
                        {
                            result.Completions.AddRange(root.Arguments.Helpers);
                        }

                        // Make pretty
                        result.Completions.Sort();

                        // Any node with a method is a possible stopping point
                        if (root.Method != null)
                        {
                            // Make <CR> the _first_ option (aesthetics :-)
                            result.Completions.Insert(0, "<CR>");
                        }

                        // Special case: Is this a glue node?
                        if (tokens.Count == 1 &&
                            root.SubNodes.Count == 1 &&
                            root.Arguments == null &&
                            root.Method == null)
                        {
                            tokens.Add(root.SubNodes[0].Name);    // Yep, chase it!
                        }
                    }
                }
                tokens.RemoveAt(0);
            }

            result.Match = sb.ToString();
            return result;
        }

        /// <summary>
        /// Find any full or partial matches for a given command word starting
        /// with the given command node.  If one or more matches are found,
        /// return the longest common prefix and a list of possible completions.
        /// If no matches are found, return the input string and clear the list.
        /// </summary>
        private CompletionList FindCompletions(CommandNode root, string word)
        {
            var match = new CompletionList(null, String.Empty);

            // Always look for a SubNode match first
            foreach (CommandNode c in root.SubNodes)
            {
                if (!c.Hidden && c.Name.StartsWith(word, StringComparison.InvariantCultureIgnoreCase))
                {
                    match.SearchRoot = c;
                    match.Match = c.Name;
                    match.Completions.Add(c.ToString());
                }
            }

            // Ambiguous SubNode match: reset root and return best partial
            if (match.Completions.Count > 1)
            {
                match.Match = LongestCommonPrefix(match.Completions.ToArray());
                match.SearchRoot = root;

                return match;
            }

            // Unambiguous SubNode match: return early
            if (match.Completions.Count == 1)
            {
                return match;
            }

            // No matching SubNodes; check Parameters
            if (root.Arguments != null)
            {
                var argNode = root.Arguments;

                if (!argNode.MatchArgument(word, true /* fuzzy */))
                {
                    match.Match = word;
                    return match;               // No joy; bail early
                }

                // Start down the chain!
                match.SearchRoot = argNode;

                if (argNode.Param.ParameterType.IsEnum ||
                    argNode.Param.ParameterType == typeof(bool))
                {
                    // Matched an enum, get the list of one or more values;
                    // expand 'true' or 'false' so booleans match properly
                    match.Completions.AddRange(
                        argNode.Helpers.FindAll(
                            x => x.StartsWith(word, StringComparison.InvariantCultureIgnoreCase)));

                    match.Match = LongestCommonPrefix(match.Completions.ToArray());
                }
                else
                {
                    // Not an enum, so return the helper string(s)
                    match.Match = word;
                    match.Completions.AddRange(argNode.Helpers);
                }
            }
            return match;
        }

        /// <summary>
        /// Find the longest common prefix in an array of strings.
        /// </summary>
        private string LongestCommonPrefix(string[] a)
        {
            int size = a.Length;

            if (size == 0) return "";
            if (size == 1) return a[0];

            Array.Sort(a);
            int end = Math.Min(a[0].Length, a[size - 1].Length);

            // Find the common prefix between the first and last string
            int i = 0;
            while (i < end && a[0][i] == a[size - 1][i]) i++;

            return a[0].Substring(0, i);
        }


        private CommandNode _commandTree;
        private CommandNode _commandTreeRoot;

        private int _originRow;
        private int _originColumn;

        private string _prompt;
        private string _input;
        private int _textPosition;
        private int _lastInputLength;

        private List<string> _commandHistory;
        private int _historyIndex;
    }
}
