//
// Z80DebugCommands.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO;
using PERQemu.Debugger;

namespace PERQemu
{
    public partial class DebugCommands
    {
        //
        // Z80 Debugging
        //

        [Command("debug z80", "Enter the Z80 debugging subsystem", Prefix = true)]
        void SetZ80DebugPrefix()
        {
            PERQemu.CLI.SetPrefix("debug z80");
        }

        [Command("debug z80 done", "Return to PERQ debugger")]
        public void Z80DebugDone()
        {
            PERQemu.CLI.SetPrefix("debug");
        }

        [Command("debug z80 commands", "Show Z80 debugging commands")]
        public void ShowZ80DebugCommands()
        {
            PERQemu.CLI.ShowCommands("debug z80");
        }

        [Command("debug z80 inst", "Run one Z80 opcode")]
        public void DebugZ80Inst()
        {
            if (PERQemu.Controller.State <= RunState.Off)
            {
                Console.WriteLine("The PERQ is currently turned off.");
            }
            else
            {
                PERQemu.Controller.TransitionTo(RunState.RunZ80Inst);
                PERQemu.Sys.PrintStatus();
            }
        }

        [Command("debug z80 show registers", "Display contents of the Z80 registers")]
        void ShowZ80State()
        {
            PERQemu.Sys.IOB.Z80System.ShowZ80State();
        }

        //[Conditional("DEBUG")]
        [Command("debug z80 dump scheduler queue")]
        void DumpZ80Scheduler()
        {
            PERQemu.Sys.IOB.Z80System.Scheduler.DumpEvents("Z80");
        }

        // todo: bare bones right now - just display one byte.  expand this to
        // allow ranges and output options (radix, ascii, etc?)
        [Command("debug z80 show memory", "Display contents of a given memory location")]
        void ShowZ80Memory(ushort addr)
        {
            try
            {
                byte value = PERQemu.Sys.IOB.Z80System.Memory[addr];
                Console.WriteLine($"Address: 0x{addr:x4}  Value: 0x{value:x2}");
            }
            catch (Exception e)
            {
                Console.WriteLine($"Couldn't read {addr}: {e.Message}");
            }
        }

        // todo: rom disassembler, like the perq microcode disassembler?
        // todo: i/o port reads - and writes!?
        // todo: interrogate memory, fifos, peripheral controllers & registers, etc.
    }
}
