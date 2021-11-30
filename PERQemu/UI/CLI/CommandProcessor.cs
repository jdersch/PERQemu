//
// CommandProcessor.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.UI;

namespace PERQemu
{
    /// <summary>
    /// CommandProcessor is the top-level console command interpreter.  It
    /// wraps the CommandExecutor which does the dirty work.
    /// </summary>
    public class CommandProcessor
    {
        public CommandProcessor()
        {
            // Catch interrupts in the console
            Console.CancelKeyPress += OnCtrlC;

            // List of classes to interrogate for CommandAttributes
            var commandObjects = new List<object> {
                    this,
                    new ExecCommands(),
                    new DebugCommands(),
                    new ConfigCommands(),
                    new StorageCommands(),
                    new SettingsCommands()
                };

            // Build the tree
            _exec = new CommandExecutor(commandObjects);

            // Start up the line editor
            _editor = new CommandPrompt(_exec.CommandTreeRoot);
        }

        public void ReadScript(string script)
        {
            // Ignore the result...
            _exec.ExecuteScript(script);
        }

        public void Run()
        {
            _running = true;

            while (_running)
            {
                try
                {
                    string cmd = _editor.GetLine().Trim();

                    if (cmd != string.Empty)
                    {
                        _exec.ExecuteLine(cmd);
                    }
                }
                catch (Exception e)
                {
                    Console.WriteLine(e.Message);
                }
            }
        }

        public CommandNode Prefix
        {
            get { return _exec.CurrentRoot; }
        }

        /// <summary>
        /// Enter a subsystem.
        /// </summary>
        public void SetPrefix(string pfx)
        {
            _exec.CurrentRoot = _editor.SetPrefix(pfx);
        }

        /// <summary>
        /// Return to the top-level prompt.
        /// </summary>
        public void ResetPrefix()
        {
            _exec.CurrentRoot = _editor.ResetPrefix();
        }

        /// <summary>
        /// Catch ^C on the console and return to the prompt.
        /// </summary>
        void OnCtrlC(object sender, ConsoleCancelEventArgs e)
        {
            e.Cancel = true;
            Console.WriteLine("^C");

            if (PERQemu.Sys != null)
            {
                PERQemu.Sys.Break();
            }
        }

        #region CLI Utility Routines

        /// <summary>
        /// Print a nice columnar list of (reasonably short) strings.
        /// </summary>
        /// <param name="items">Strings to print</param>
        /// <param name="leftCol">Left indent</param>
        /// <param name="tabWidth">Column width</param>
        public void Columnify(string[] items, int leftCol = 4, int tabWidth = 15)
        {
            Console.Write(" ".PadLeft(leftCol));
            var col = leftCol;

            foreach (var i in items)
            {
                if (col + tabWidth > Console.BufferWidth)
                {
                    Console.WriteLine();
                    Console.Write(" ".PadLeft(leftCol));
                    col = leftCol;
                }

                // If you want to print long strings, pass a wider tabWidth.
                // But catch the occasional edge case and try to make it pretty.
                if (i.Length > tabWidth)
                {
                    var extra = i.Length % tabWidth;

                    Console.Write(i);
                    col += i.Length;

                    if ((col + tabWidth - extra) < Console.BufferWidth)
                    {
                        Console.Write(" ".PadRight(tabWidth - extra));
                        col += tabWidth - extra;
                    }
                }
                else
                {
                    Console.Write(i.PadRight(tabWidth));
                    col += tabWidth;
                }
            }
            Console.WriteLine();
        }

        public bool IsPrintable(char c)
        {
            return (char.IsLetterOrDigit(c) ||
                    char.IsSymbol(c) ||
                    char.IsPunctuation(c));
        }

        #endregion

        //
        // Basic built-in commands
        //

        [Command("about", "About PERQemu")]
        private void About()
        {
            PERQemu.PrintBanner();
        }

        [Command("help", "Show PERQemu help")]
        private void Help()
        {
            Console.WriteLine("This is PERQemu, an emulator for the Three Rivers PERQ workstation.");
            Console.WriteLine("Type 'commands' at the prompt to see which commands are available.");
            Console.WriteLine();
            Console.WriteLine("The command line editor provides tab completion and prompts to assist");
            Console.WriteLine("you.  Use the arrow keys to retrieve and edit previous command lines.");
            Console.WriteLine("Type 'help editor' for more information.");
            Console.WriteLine();
            Console.WriteLine("Many more commands are available to configure and customize the emulated");
            Console.WriteLine("PERQ, debug software running on it (or the emulator itself), and to set");
            Console.WriteLine("preferences that tailor PERQemu to your environment.  Type 'help ' and");
            Console.WriteLine("press the TAB key for more on-line assistance, or consult the User's Guide");
            Console.WriteLine("included with the distribution.");
        }

        [Command("help editor", "Show PERQemu command line editor help")]
        private void HelpCLI()
        {
            Console.WriteLine("Some commands may accept optional arguments or may require them; press");
            Console.WriteLine("the TAB key at any point to see a list of possible completions for the");
            Console.WriteLine("current input word, or preview the next expected argument.  Pressing");
            Console.WriteLine("the SPACE BAR or TAB key will expand the current input up to the longest");
            Console.WriteLine("unambiguous match.");
            Console.WriteLine();
            Console.WriteLine("String arguments may be a single word, or must be surrounded by quotes");
            Console.WriteLine("if they contain spaces.");
            Console.WriteLine();
            Console.WriteLine("Numeric arguments may be specified in decimal (default), or in another");
            Console.WriteLine("common base depending on preference or context:");
            Console.WriteLine("\tBase    \t Prefix    \tExample");
            Console.WriteLine("\tBinary: \t    b      \tb10001100");
            Console.WriteLine("\tOctal:  \t  o or %   \to377 or %177600");
            Console.WriteLine("\tDecimal:\td (or none)\td12345 or 54321");
            Console.WriteLine("\tHex:    \t0x, x or $ \t0xff, x3eff, $80000");
            Console.WriteLine();
            Console.WriteLine("The default output radix may be set with 'settings radix <base>'.");
        }

        [Command("commands", "Show console commands and their descriptions")]
        public void ShowCommands()
        {
            _exec.ShowCommands(_exec.CurrentRoot);
        }

        public void ShowCommands(string prefix)
        {
            var node = _exec.CommandTreeRoot;       // Help on multiple levels!

            foreach (var level in prefix.Split(' '))
            {
                node = node.FindSubNodeByName(level);

                if (node == null)
                {
                    Console.WriteLine("No help available for '{0}'.", prefix);
                    return;
                }
            }

            _exec.ShowCommands(node);
        }

        [Command("gui", "Start the graphical interface")]
        private void LaunchGUI()
        {
            // I'm looking at YOU, 64-bit Cocoa WinForms port that was promised
            // three YEARS ago.  Sigh.
            Console.WriteLine("Nope.  No cross-platform GUI available yet.");

            // TODO: build a flippin' Windows machine and try out the PERQolator
            // GUI there.  At least maybe we'd have a way to continue development...

            //Console.WriteLine("[Console now read-only; close the GUI to return.]");

            //// When started manually, return to the console if the user closes
            //// the front panel -- but exit if they explicitly choose File->Exit.
            //if (PERQemu.GUI.Run("FrontPanel"))
            //{
            //    _running = false;
            //}
            //else
            //{
            //    Console.WriteLine("[Return to CLI mode.]");
            //}
        }

        [Command("done", IsDiscreet = true)]
        private void Done()
        {
            Console.WriteLine("Already at top-level.");
        }

        [Command("exit")]
        [Command("quit", "Leave PERQemu")]
        private void Quit()
        {
            _running = false;
        }

        [Command("quit without save", "Leave PERQemu without committing changes")]
        private void QuitNow()
        {
            Settings.Changed = false;   // Force the "without save" part :-)
            _running = false;
        }

        [Conditional("DEBUG")]
        [Command("debug dump command tree")]
        public void DumpCommandTree()
        {
            Console.WriteLine("\nCommand tree:");
            _exec.DumpCommandTree(_exec.CurrentRoot, 0);
        }


        private bool _running;
        private CommandExecutor _exec;
        private CommandPrompt _editor;
    }
}
