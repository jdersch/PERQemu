//
// CommandPrompt.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
            _commandHistory = new List<string>(99);
            _historyIndex = 0;

            _prompt = "";

            _lastWidth = Console.BufferWidth;
            _lastHeight = Console.BufferHeight;

            InitEditKeyMap();
            InitIntlKeyMap();
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
                    Log.Error(Category.Emulator, "SetPrefix couldn't set root to '{0}'", subsys);
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

        public string CurrentPrefix => _prompt;


        #region Command editor

        /// <summary>
        /// Runs a nifty interactive debugger prompt.
        /// </summary>
        public string GetLine()
        {
            ClearInput();
            _lastInputLength = 0;

            DisplayPrompt();
            UpdateDisplay();

            bool entryDone = false;

            while (!entryDone)
            {
                // Read one keystroke from the console
                ConsoleKeyInfo key = PERQemu.CLI.GetKeyEventually();
                ConsoleKey cmd = key.Key;

                // Map the basic EMACS-like control keys
                if (key.Modifiers == ConsoleModifiers.Control && _editKeyMap.ContainsKey(key.Key))
                {
                    cmd = _editKeyMap[key.Key];
                }

                // Edit or add to the command line
                switch (cmd)
                {
                    case ConsoleKey.Escape:
                        ClearInput();
                        break;

                    case ConsoleKey.Backspace:
                        DeleteCharAtCursor(true);
                        break;

                    case ConsoleKey.Delete:
                        DeleteCharAtCursor(false);
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

                    case ConsoleKey.Clear:
                        ClearScreen();
                        break;

                    case ConsoleKey.Tab:
                        if (InsideString())
                        {
                            InsertChar(' ');
                        }
                        else
                        {
                            DoCompletion(false);    // Show completions
                        }
                        break;

                    case ConsoleKey.Spacebar:
                        if (InsideString() ||
                            _input.EndsWith(" ", StringComparison.Ordinal) ||
                            !DoCompletion(true))    // Silent
                        {
                            InsertChar(' ');
                        }
                        break;

                    case ConsoleKey.Enter:
                        if (!DoCompletion(true)) MoveToEnd();
                        entryDone = true;
                        break;

                    case ConsoleKey.NoName:
                        // Quietly ignore -- bind things that mess up the input
                        // or will eventually be bound to other functions (^K for
                        // "kill" and ^Y for "yank", etc)
                        break;

                    default:
                        // Not a special key; handle possible translation
                        var c = key.KeyChar;

                        if (_intlKeyMap.ContainsKey(c))
                            c = _intlKeyMap[c];

                        // Insert character if it's deemed "printable"
                        if (char.IsLetterOrDigit(c) ||
                            char.IsPunctuation(c) ||
                            char.IsSymbol(c) ||
                            char.IsWhiteSpace(c))
                        {
                            InsertChar(c);
                        }
                        break;
                }

                UpdateDisplay();
            }

            // Clean up extraneous whitespace
            _input = _input.Trim();

            // todo: add "!" for shell-like history processing?  probably best
            // to do that here since we have access to the history buffer

            // Add to history
            AddToHistory(_input);

            Console.WriteLine();
            return _input;
        }

        /// <summary>
        /// Displays the current prompt and sets the origin.
        /// </summary>
        void DisplayPrompt()
        {
            // Force column zero
            Console.SetCursorPosition(0, Console.CursorTop);

            // A "workmanlike" way to show if there are unsaved changes
            // todo: this really needs to be less janky
            if ((_commandTree.Name == "configure" && PERQemu.Config.Changed) ||
                (_commandTree.Name == "settings" && Settings.Changed))
            {
                Console.Write($"{_prompt}*> ");
            }
            else
            {
                Console.Write($"{_prompt}> ");
            }

            // Reset for where we at now
            _lastRow = _originRow = Console.CursorTop;
            _lastColumn = _originColumn = Console.CursorLeft;
        }

        /// <summary>
        /// If the window has been resized, recompute the origin and clip the
        /// coordinates to fit the new console and refresh the prompt.
        /// </summary>
        bool UpdateOrigin()
        {
            if (Console.BufferHeight != _lastHeight || Console.BufferWidth != _lastWidth)
            {
                // Origin row is "sticky" relative to the bottom of the window
                _originRow = Math.Max(0, Console.BufferHeight - (_lastHeight - _originRow));
                _originRow = Math.Min(Console.BufferHeight - 1, _originRow);
                Console.SetCursorPosition(_originColumn, _originRow);

                _lastWidth = Console.BufferWidth;
                _lastHeight = Console.BufferHeight;
                return true;
            }

            return false;
        }

        /// <summary>
        /// Positions the cursor and updates the saved column and row, clipping
        /// to the window coordinates if necessary.
        /// </summary>
        bool UpdateCursor(int col, int row)
        {
            var changed = false;

            // Check if the cursor is out of bounds
            if (col < 0 || col > (Console.BufferWidth - 1) ||
                row < 0 || row > (Console.BufferHeight - 1))
            {
                col = Math.Max(0, col);
                col = Math.Min(Console.BufferWidth - 1, col);

                row = Math.Max(0, row);
                row = Math.Min(Console.BufferHeight - 1, row);
                changed = true;
            }

            if (col != Console.CursorLeft || row != Console.CursorTop)
            {
                Console.SetCursorPosition(col, row);
            }

            return changed;
        }

        /// <summary>
        /// Updates the command line and positions the cursor at the current
        /// text position, redisplaying the prompt if necessary.  Accounts for
        /// window resizing and updates the last column and row.
        /// </summary>
        void UpdateDisplay()
        {
            Console.CursorVisible = false;

            // Has the window changed or the cursor moved off our last position?
            if (UpdateOrigin() || UpdateCursor(_lastColumn, _lastRow))
            {
                DisplayPrompt();
            }

            // Current input string is shorter than the last?  Overprint with
            // spaces to erase, and always add one extra to account for cursor
            // when a long input line wraps
            var clear = " ".PadRight(Math.Max(1, _lastInputLength - _input.Length));

            Console.SetCursorPosition(_originColumn, _originRow);
            Console.Write(_input + clear);

            // Compute new cursor position
            int col = _originColumn + _textPosition;
            int row = _originRow;
            int lines = ((_originColumn + Math.Max(_input.Length, _lastInputLength)) / Console.BufferWidth) + 1;

            // Has input wrapped at the bottom of the screen?
            if (row + lines > Console.BufferHeight)
            {
                _originRow = Console.BufferHeight - lines;
            }

            // Account for line wrap
            if (col > Console.BufferWidth - 1)
            {
                col = ((_originColumn + _textPosition) % Console.BufferWidth);
                row = _originRow + ((_originColumn + _textPosition) / Console.BufferWidth);
            }

            // Move cursor to text position for next input and turn it back on
            UpdateCursor(col, row);
            Console.CursorVisible = true;

            // Save updated values for next time
            _lastColumn = col;
            _lastRow = row;
            _lastInputLength = _input.Length;
        }


        int TextPosition
        {
            get { return _textPosition; }
            set
            {
                // Clip input between 0 and the length of input (+1, to allow adding text at end)
                _textPosition = Math.Max(0, value);
                _textPosition = Math.Min(_textPosition, _input.Length);
            }
        }

        bool InsideString()
        {
            var result = false;

            // Scan from the start of the line to where the cursor is now!
            foreach (var c in _input.ToCharArray(0, TextPosition))
            {
                if (c == '\"') { result = !result; }
            }

            return result;
        }

        void ClearScreen()
        {
            Console.Clear();
            DisplayPrompt();
        }

        void ClearInput()
        {
            _input = string.Empty;
            HistoryIndex = _commandHistory.Count - 1;
            TextPosition = 0;
        }

        void MoveToBeginning()
        {
            TextPosition = 0;
        }

        void MoveToEnd()
        {
            TextPosition = _input.Length;
        }

        void MoveLeft()
        {
            TextPosition--;
        }

        void MoveRight()
        {
            TextPosition++;
        }

        void InsertChar(char c)
        {
            _input = _input.Insert(TextPosition, c.ToString());
            TextPosition++;
        }

        void DeleteCharAtCursor(bool backspace)
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

                // Remove 1 char at the position before the cursor,
                // and move the cursor back one char
                _input = _input.Remove(TextPosition - 1, 1);
                TextPosition--;
            }
            else
            {
                if (TextPosition == _input.Length)
                {
                    return;     // At the end of input, can't delete
                }

                // Remove one char at the current cursor position,
                // but do not move the cursor
                _input = _input.Remove(TextPosition, 1);
            }
        }

        #endregion

        #region Command history

        void HistoryPrev()
        {
            if (_commandHistory.Count > 0)
            {
                _input = _commandHistory[HistoryIndex];
                TextPosition = _input.Length;
                HistoryIndex--;     // clips to range
            }
        }

        void HistoryNext()
        {
            if (HistoryIndex < _commandHistory.Count - 1)
            {
                HistoryIndex++;
                _input = _commandHistory[HistoryIndex];
            }
            else
            {
                _input = string.Empty;
            }

            TextPosition = _input.Length;
        }

        void AddToHistory(string cmd)
        {
            if (cmd != string.Empty)
            {
                // Don't add if we repeat the previous command, since that's annoying
                if (_commandHistory.Count == 0 || _commandHistory[HistoryIndex] != cmd)
                {
                    _commandHistory.Add(cmd);
                }

                // Let's not grow without bound...
                if (_commandHistory.Count > MAX_HISTORY)
                {
                    _commandHistory.RemoveAt(0);
                }

                HistoryIndex = _commandHistory.Count - 1;
            }
        }

        int HistoryIndex
        {
            get { return _historyIndex; }
            set
            {
                _historyIndex = Math.Min(_commandHistory.Count - 1, value);
                _historyIndex = Math.Max(0, _historyIndex);
            }
        }

        public void ShowHistory()
        {
            Console.WriteLine("Command history:");

            for (var i = 0; i < _commandHistory.Count; i++)
            {
                Console.WriteLine($"{i,4} {_commandHistory[i]}");
            }
        }

        #endregion

        #region Command completion

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
        bool DoCompletion(bool silent)
        {
            bool changed = false;

            var result = GetCompletions(_commandTree, _input);

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
        CompletionList GetCompletions(CommandNode root, string input)
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

            var sb = new StringBuilder();

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
#if !DEBUG
                                if (!c.Hidden)
#endif
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
        CompletionList FindCompletions(CommandNode root, string word)
        {
            var match = new CompletionList(null, string.Empty);

            // Always look for a SubNode match first
            foreach (CommandNode c in root.SubNodes)
            {
#if DEBUG
                // In DEBUG builds, Discreet commands are rudely visible, how gauche!
                if (c.Name.StartsWith(word, StringComparison.InvariantCultureIgnoreCase))
#else
                // The better part of valor
                if (!c.Hidden && c.Name.StartsWith(word, StringComparison.InvariantCultureIgnoreCase))
#endif
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

                if (argNode.Param.ParameterType.IsEnum || argNode.DoKeywordMatch)
                {
                    match.Completions.AddRange(
                        argNode.Helpers.FindAll(
                            x => x.StartsWith(word, StringComparison.InvariantCultureIgnoreCase)));

                    match.Match = LongestCommonPrefix(match.Completions.ToArray());

                    // String args tagged with [KeywordMatch] aren't strictly enforced;
                    // if the user is freestylin' just let 'em go with it; fall back and
                    // treat it like a generic string argument
                    if (match.Completions.Count == 0 && argNode.DoKeywordMatch)
                    {
                        match.Match = word;
                        match.SearchRoot = null;
                    }
                }
                else if (argNode.Param.ParameterType == typeof(bool))
                {
                    // You can't handle the truth
                    if (word.StartsWith("t", StringComparison.InvariantCultureIgnoreCase))
                    {
                        match.Match = "true";
                        match.Completions.Add("true");
                    }
                    else if (word.StartsWith("f", StringComparison.InvariantCultureIgnoreCase))
                    {
                        match.Match = "false";
                        match.Completions.Add("false");
                    }
                    else
                    {
                        match.Match = "BAD BOOLEAN";
                        match.SearchRoot = null;
                    }
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
        string LongestCommonPrefix(string[] a)
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

        #endregion

        /// <summary>
        /// Map the classic EMACS control keys to the DOS-style line editor commands.
        /// </summary>
        void InitEditKeyMap()
        {
            _editKeyMap = new Dictionary<ConsoleKey, ConsoleKey>();

            _editKeyMap.Add(ConsoleKey.A, ConsoleKey.Home);
            _editKeyMap.Add(ConsoleKey.B, ConsoleKey.LeftArrow);
            _editKeyMap.Add(ConsoleKey.D, ConsoleKey.Delete);
            _editKeyMap.Add(ConsoleKey.E, ConsoleKey.End);
            _editKeyMap.Add(ConsoleKey.F, ConsoleKey.RightArrow);
            _editKeyMap.Add(ConsoleKey.H, ConsoleKey.Backspace);
            _editKeyMap.Add(ConsoleKey.I, ConsoleKey.Tab);
            _editKeyMap.Add(ConsoleKey.J, ConsoleKey.Enter);
            _editKeyMap.Add(ConsoleKey.K, ConsoleKey.NoName);   // For now
            _editKeyMap.Add(ConsoleKey.L, ConsoleKey.Clear);
            _editKeyMap.Add(ConsoleKey.M, ConsoleKey.Enter);
            _editKeyMap.Add(ConsoleKey.N, ConsoleKey.DownArrow);
            _editKeyMap.Add(ConsoleKey.P, ConsoleKey.UpArrow);
            _editKeyMap.Add(ConsoleKey.U, ConsoleKey.Escape);

            // todo: ^R - reprint line?
            // todo: ^T - twiddle two characters
            // todo: ^W - erase word (back)
            // todo: ^Y - yank word
        }

        /// <summary>
        /// This is a quick fix for mapping Unicode input characters to UTF-8/ASCII
        /// equivalents, to allow the parser to read the English input files when a
        /// non-English locale is in use.  See issue #1 (thanks blw) for info.
        /// </summary>
        void InitIntlKeyMap()
        {
            _intlKeyMap = new Dictionary<char, char>();

            // For Turkish:
            _intlKeyMap.Add('\u0130', 'I');
            _intlKeyMap.Add('\u0131', 'i');
            _intlKeyMap.Add('\u00fc', 'u');
            _intlKeyMap.Add('\u00dc', 'U');
            _intlKeyMap.Add('\u00f6', 'o');
            _intlKeyMap.Add('\u00d6', 'O');
            _intlKeyMap.Add('\u015f', 's');
            _intlKeyMap.Add('\u015e', 'S');
            _intlKeyMap.Add('\u00e7', 'c');
            _intlKeyMap.Add('\u00c7', 'C');
            _intlKeyMap.Add('\u011f', 'g');
            _intlKeyMap.Add('\u011e', 'G');

            // There must be a more general/simpler solution so that we can
            // easily extend this for other input locales; that's a long term
            // project, likely tied to moving toward a fully integrated GUI
            // with ALL the bells & whistles. :-)  Hopefully this works for now!
        }

        readonly int MAX_HISTORY = 100;     // Season to taste

        CommandNode _commandTree;
        CommandNode _commandTreeRoot;

        string _prompt;
        string _input;

        int _textPosition;
        int _originRow;
        int _originColumn;
        int _lastRow;
        int _lastColumn;
        int _lastWidth;
        int _lastHeight;
        int _lastInputLength;

        List<string> _commandHistory;
        int _historyIndex;

        Dictionary<ConsoleKey, ConsoleKey> _editKeyMap;
        Dictionary<char, char> _intlKeyMap;
    }
}

/*
    Notes:
    
    To map Turkish (and other Unicode) characters to their plain old UTF-8/ASCII-ish
    equivalents, a separate map could be used:
    
char[] turkishChars = new char[] { 0x131, 0x130, 0xFC, 0xDC, 0xF6, 0xD6, 0x15F, 0x15E, 0xE7, 0xC7, 0x11F, 0x11E };
char[] englishChars = new char[] { 'i', 'I', 'u', 'U', 'o', 'O', 's', 'S', 'c', 'C', 'g', 'G'};

    Specifically for Turkish, the "i"/"I" (with their dotted and non-dotted variants)
    should probably all map to the English so the parser can work...

    Upper İ   I
    U+0130  U+0049
    Lower i   ı
    U+0069  U+0131

    _Every_ case where data is written to disk/floppy/tape should probably be
    checked to make sure that it's strictly binary 8-bit values but I think
    we're okay there?  All file I/O is in binary, while all (?) text comes in
    through here -- so mapping inputs to (rough) UTF-8/ASCII equivalents ought
    to at least allow the parser to work.
*/