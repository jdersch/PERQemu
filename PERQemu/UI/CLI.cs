//
//  CLI.cs
//
//  Copyright (c) 2019, S. Boondoggle <skeezicsb@gmail.com>
//
//  This file is a part of PERQolator, a refactoring of PERQemu originally
//  written by Josh Dersch.
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Threading;

using PERQolator;
using PERQolator.Config;

namespace PERQolator.UI
{

    /// <summary>
    /// The CLI provides command parsing and execution via the console.  This is
    /// the default method of interaction on Unix.  If the GUI is enabled, the
    /// console remains as an output device for diagnostic messages.
    /// </summary>
    public class CLI
    {
        public CLI()
        {
            Console.WriteLine("CLI: constructor called.");
            // Here we build the DebuggerPrompt, but somehow aggregate both
            // debugging commands, configuration commands, and controller
            // commands together.

            // i wonder if i should do "subsystems" -- let the configurator
            // run its own parser?  that is, create a
            //      cli.prompt() for basic controller commands
            //      configurator.prompt() for when the user switches
            //          to "config mode" (like "config term" on cisco ios :)
            // the same engine could load different grammars based on a
            // different set of properties.  "[Command("foo")]" for regular
            // commands, "[ConfigCommand("yar")]" for configurator stuff?
            // but adding modes might make things too baroque.
            //
            // see the separate text file with the syntax ideas.  if the
            // whole grammar isn't too sprawling, keep it all together?
            // change the command completion to understand hierarchy?  (it
            // does already, right?)

            // heh.  could "cheat" a bit:  if the user types "config" and
            // hits return, rather than tab or space, we save that and just
            // prepend it (silently) to subsequent commands, until some
            // kind of exit was recognized... hmmm.  that way you could type
            //
            //  > config
            //  config> cpu perq1
            //  config> disk shugart24
            //
            // or
            //
            //  > config cpu perq1
            //  > config disk shugart24
            //
            // is this getting silly?  yes.  yes it is.

        }

        public void Initialize()
        {
            // initialize the terminal settings
            // build the command table for each subsystem
            // etc?
        }


    }
}
