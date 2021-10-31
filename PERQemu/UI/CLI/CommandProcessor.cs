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

        [Command("about", "About PERQemu")]
        private void About()
        {
            EntryPoint.PrintBanner();
        }

        [Command("help", "Show PERQemu help.")]
        private void Help()
        {
            // For now, all help is just inline.  Eventually a guide or
            // even (gasp!) a web site should provide more in-depth info.
            // we can easily break this down to be more useful:
            //      help            - general overview 
            //      help commands   - cli usage (below)
            //      help media      - working with disks and floppies (and tapes someday)
            //      help emulator   - how to set up and run the emulated perq
            // and so on.
            // each subsystem should have its own help too.
            //      configure help  - how to load, modify and save a perq config
            //      settings help   - what user preferences can be set
            //      debug help      - in the debug version, specific debugger info
            // etc.
            Console.WriteLine("Type 'commands' at the prompt to see which commands are available.");
            Console.WriteLine("Some commands may accept optional arguments or may require them; press");
            Console.WriteLine("the TAB key at any point to see a list of possible completions for the");
            Console.WriteLine("current input word, or preview the next argument expected.  Pressing");
            Console.WriteLine("the SPACE BAR or TAB key will expand the current input up to the longest");
            Console.WriteLine("unambiguous match.");
            Console.WriteLine();
            Console.WriteLine("String arguments may be a single word, or must be surrounded by quotes");
            Console.WriteLine("if they contain spaces.  Numeric arguments may be specified in decimal");
            Console.WriteLine("(the default base), octal, binary, or hex blah blah blah." );
            Console.WriteLine("Use the arrow keys to retrieve and edit previous command lines.");
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
        //    if (PERQemu.GUI.Run("FrontPanel"))
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

        [Command("exit")]
        [Command("quit", "Leave PERQemu")]
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
