//
// ExecCommands.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.UI
{
    /// <summary>
    /// Implements basic execution controls: starting, stopping, loading and
    /// saving ejectable media (floppies or tapes), screenshots, etc.
    /// </summary>
    public class ExecCommands
    {
        //
        // load::<media> -- insert removable media
        // save::<media> -- save removable media
        // unload::<media> -- remove " ".  alias: eject
        // create::<media> -- create new blank, formatted media
        // 

        [Command("power on", "Turn on the configured PERQ!")]
        public void PowerOn()
        {
            Console.WriteLine("Turning on the PERQ");
            
        }

        [Command("power off", "Turn off the PERQ")]
        public void PowerOff()
        {
            Console.WriteLine("Turning off the PERQ");
            
        }

        [Command("go")]
        [Command("start", "Start or restart the PERQ")]
        public void Start()
        {
            Console.WriteLine("Starting the PERQ...");
            
        }

        [Command("stop", "Stop or pause the PERQ")]
        public void Stop()
        {
            Console.WriteLine("Stopping the PERQ...");
            
        }

        [Command("reset", "Reset the PERQ")]
        public void Reset()
        {
            Console.WriteLine("Resetting the PERQ...");
            
        }
    }
}
