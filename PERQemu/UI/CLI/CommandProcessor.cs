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
            Console.CancelKeyPress += new ConsoleCancelEventHandler(OnCtrlC);

            /*
            // List of classes to interrogate for CommandAttributes
            var commandObjects = new List<object> {
                    this,
                    new ExecCommands(),
                    new DebugCommands(),
                    new ConfigCommands(),
                    new StorageCommands(),
                    new SettingsCommands()
                };
            */

            // TODO: add classes here as DebugFunctions are converted over
            var commandObjects = new List<object>();
            commandObjects.Add(this);

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
                    string cmd = _editor.Prompt().Trim();

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
        [Command("xyzzy", IsDiscreet = true)]
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

            // todo: if the perq is running, interrupt it?  return to the cli...
        }


        //
        // Basic commands
        //

        [Command("help", "Show PERQolator help.")]
        private void Help()
        {
            // Dump out a text file?  For now, blat this out.
            Console.WriteLine("This is PERQolator, a PERQ emulator.  Commands may be entered through");
            Console.WriteLine("the command line, or a graphical interface may be invoked by typing");
            Console.WriteLine("'gui' or starting PERQolator from the shell with the '-g' switch.");
            Console.WriteLine();
            Console.WriteLine("Type 'commands' to see which commands are available.  Press the TAB key");
            Console.WriteLine("or SPACE BAR at any time to see a list of completions.  Use the arrow");
            Console.WriteLine("keys to retrieve and edit previous command lines.");
            Console.WriteLine();
            Console.WriteLine("For more help, see the User's Guide included with the distribution.");
            
        }

        [Command("commands", "Show console commands and their descriptions")]
        public void ShowCommands()
        {
            _exec.ShowCommands();
        }

        public void ShowCommands(string prefix)
        {
            _exec.ShowCommands(prefix);
        }

        //[Command("gui", "Start the graphical interface")]
        //private void LaunchGUI()
        //{
        //    Console.WriteLine("[Console now read-only; close the GUI to return.]");

        //    // When started manually, return to the console if the user closes
        //    // the front panel -- but exit if they explicitly choose File->Exit.
        //    if (PERQolator.GUI.Run("FrontPanel"))
        //    {
        //        _running = false;
        //    }
        //    else
        //    {
        //        Console.WriteLine("[Return to CLI mode.]");
        //    }
        //}

        [Command("done", IsDiscreet = true)]
        private void Done()
        {
            Console.WriteLine("Already at top-level.");
        }

        [Command("about", "About PERQemu")]
        private void About()
        {
            EntryPoint.PrintBanner();
        }

        [Command("exit")]
        [Command("quit", "Leave PERQolator")]
        private void Quit()
        {
            _running = false;
        }

        [Command("quit without save", "Leave PERQemu without committing changes")]
        private void QuitNow()
        {
            //Settings.Changed = false;   // Force the "without save" part :-)
            _running = false;
        }

// #if DEBUG
        [Command("dump command tree", IsDiscreet = true)]
        public void DumpCommandTree()
        {
            Console.WriteLine("\nCommand tree:");
            _exec.DumpCommandTree(_exec.CurrentRoot);
        }
// #endif

        private bool _running;
        private CommandExecutor _exec;
        private CommandPrompt _editor;
    }
}
