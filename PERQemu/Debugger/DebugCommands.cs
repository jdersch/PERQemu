//
// DebugCommands.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Text;
using System.Diagnostics;
using System.Collections.Generic;

using PERQemu.Debugger;
using PERQemu.Processor;

namespace PERQemu
{
    /// <summary>
    /// The CLI interface to the debugger: a set of commands to interrogate
    /// and display internal state of the main CPU, manage logging, and lots
    /// more.  Z80 debugging commands are continued in another file to keep
    /// things manageable.
    /// </summary>
    /// <remarks>
    /// Because the entire PERQ is now configurable, it seems easier to have
    /// our attributes attached to methods in a class that's instantiated once,
    /// so we don't have to track down instance objects at runtime.  (This was
    /// written before the changes to the Debugger that did away with the
    /// MethodInvokeInfo class and added the GetInstanceFrom*() approach.)
    /// The idea was that with a fully graphical interface AND a command-line
    /// we'd want a common API for getting data out of the CPU and its various
    /// component parts, and pull all of those Console.WriteLines OUT of there.
    /// Anyway, it's a work in progress.
    /// </remarks>
    public partial class DebugCommands
    {

        [Command("debug", "Enter the debugging subsystem")]
        [Command("debug z80 done", "Return to PERQ debugger")]
        public void SetDebugPrefix()
        {
            PERQemu.CLI.SetPrefix("debug");
        }

        [Command("debug commands", "Show debugging commands")]
        public void ShowDebugCommands()
        {
            PERQemu.CLI.ShowCommands("debug");
        }

        [Command("debug done", "Return to top level")]
        [Command("debug top", "Return to top level")]
        [Command("debug z80 top", "Return to top level")]
        public void DebugDone()
        {
            PERQemu.CLI.ResetPrefix();
        }

        [Command("debug show", "Show current debugger settings")]
        public void ShowDebugSettings()
        {
            Console.WriteLine("Current debugger settings:");
            // logging, radix, step mode, whatever
        }

        [Command("step")]
        [Command("debug step", "Run one microinstruction")]
        public void DebugStep()
        {
            PERQemu.Controller.TransitionTo(RunState.SingleStep);
        }

        [Command("inst")]
        [Command("debug inst", "Run one opcode")]
        public void DebugInst()
        {
            PERQemu.Controller.TransitionTo(RunState.RunInst);
        }

        [Command("debug set execution mode", "Set the execution mode for the virtual PERQ")]
        private void SetZ80ExecutionMode(ExecutionMode mode)
        {
            if (PERQemu.Sys.Mode != mode)
            {
                PERQemu.Sys.Mode = mode;
                Console.WriteLine("Execution mode changed to {0}.", mode);
            }
        }

        [Command("debug show execution mode", "Show the execution mode for the virtual PERQ")]
        private void ShowZ80ExecutionMode()
        {
            Console.WriteLine(PERQemu.Sys.Mode);
        }

#if TRACING_ENABLED
        //
        // Logging
        //

        [Command("debug set logging", "Set logging for specific events")]
        [Command("debug z80 set logging", "Set logging for specific events")]
        public void SetLogging(LogType category)
        {
            Trace.TraceLevel |= category;
            Console.WriteLine("Logging: " + Trace.TraceLevel);
        }

        [Command("debug clear logging", "Clear logging for certain events")]
        [Command("debug z80 clear logging", "Clear logging for certain events")]
        public void ClearLogging(LogType category)
        {
            Trace.TraceLevel &= ~category;
            Console.WriteLine("Logging: " + Trace.TraceLevel);
        }

        //[Command("debug set loglevel", "Set logging threshold")]
        //public void SetSeverity(Severity s)
        //{
        //    Console.WriteLine("Logging level set to " + s);
        //}

        [Command("debug enable logging", "Enable logging")]
        public void EnableLogging()
        {
            Console.WriteLine("Logging enabled");
        }

        [Command("debug disable logging", "Disable logging")]
        public void DisableLogging()
        {
            Console.WriteLine("Logging disabled");
        }
#endif

        //
        // Registers and Stacks
        //

        [Command("debug show cpu registers", "Display the values of the CPU registers")]
        private void ShowCPURegs()
        {
            PERQemu.Sys.CPU.ShowPC();
        }

        [Command("debug show opfile", "Display the contents of the opcode file")]
        private void ShowOpfile()
        {
            PERQemu.Sys.CPU.ShowOpfile();
        }

        [Command("debug show estack", "Display the contents of the expression stack")]
        private void ShowEStack()
        {
            PERQemu.Sys.CPU.ShowEStack();
        }

        [Command("debug show cstack", "Display the contents of the 2910 call stack")]
        private void ShowCStack()
        {
            PERQemu.Sys.CPU.ShowCStack();
        }

        [Command("debug show xy register", "Display the values of the XY registers")]
        private void ShowRegister()
        {
            var values = new string[256];

            for (var reg = 0; reg <= 255; reg++)
            {
                values[reg] = string.Format("XY[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister((byte)reg));
            }
            PERQemu.CLI.Columnify(values);
        }

        [Command("debug show xy register", "Display the value of an XY register")]
        private void ShowRegister(byte reg)
        {
            Console.WriteLine("R[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister(reg));
        }

        [Conditional("DEBUG")]
        [Command("debug set xy register", "Change the value of an XY register")]
        private void SetRegister(byte reg, uint val)
        {
            PERQemu.Sys.CPU.WriteRegister(reg, (int)val);   // Clips to 20-24 bits
            Console.WriteLine("R[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister(reg));
        }

        //
        // Memory and RasterOp
        //

        [Command("debug show memory", "Dump the PERQ's memory")]
        private void ShowMemory(uint startAddr, uint length = 64)
        {
            uint endAddr = startAddr + length;

            if (startAddr >= PERQemu.Sys.Memory.MemSize ||
                endAddr - 8 >= PERQemu.Sys.Memory.MemSize)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and {0}",
                                  PERQemu.Sys.Memory.MemSize - 1);
                return;
            }

            // Oof.  I could mask the address to the nearest quad word
            // and just fetch quads rather than copy the whole bloody array...
            ushort[] mem = PERQemu.Sys.Memory.Memory;

            // Format and display 8 words per line
            for (uint i = startAddr; i < endAddr; i += 8)
            {
                StringBuilder line = new StringBuilder();

                line.AppendFormat("{0:x6}: {1:x4} {2:x4} {3:x4} {4:x4} {5:x4} {6:x4} {7:x4} {8:x4} ",
                    i, mem[i], mem[i + 1], mem[i + 2], mem[i + 3], mem[i + 4], mem[i + 5], mem[i + 6], mem[i + 7]);

                for (uint j = i; j < i + 8; j++)
                {
                    // Build ascii representation...
                    char high = (char)((mem[j] & 0xff00) >> 8);
                    char low = (char)((mem[j] & 0xff));

                    if (!PERQemu.CLI.IsPrintable(high))
                    {
                        high = '.';
                    }

                    if (!PERQemu.CLI.IsPrintable(low))
                    {
                        low = '.';
                    }

                    line.AppendFormat("{0}{1}", low, high);     // Reversed so strings read right :)
                }

                Console.WriteLine(line);
            }
        }

#if DEBUG
        [Command("debug find memory", "Find a specific value in the PERQ's memory [@start, val]")]
        private void FindMemory(uint startAddress, ushort val)
        {
            if (startAddress >= PERQemu.Sys.Memory.MemSize)
            {
                Console.WriteLine("Argument out of range -- start address must be between 0 and {0}", PERQemu.Sys.Memory.MemSize - 1);
                return;
            }

            ushort[] mem = PERQemu.Sys.Memory.Memory;

            for (uint i = startAddress; i < mem.Length; i++)
            {
                if (mem[i] == val) ShowMemory((i & 0xffffc), 4);        // show the quadword
            }
        }

        [Command("debug set memory", "Write a specific value in the PERQ's memory")]
        private void SetMemory(uint address, ushort val)
        {
            if (address >= PERQemu.Sys.Memory.MemSize)
            {
                Console.WriteLine("Argument out of range -- start address must be between 0 and {0}", PERQemu.Sys.Memory.MemSize - 1);
                return;
            }

            PERQemu.Sys.Memory.StoreWord((int)address, val);
        }

        [Command("debug show memstate", "Dump the memory controller state")]
        private void ShowMemQueues()
        {
            PERQemu.Sys.Memory.DumpQueues();
        }

        [Command("debug set rasterop debug", "Enable extended RasterOp debugging")]
        private void SetRopDebug()
        {
            PERQemu.Sys.CPU.RasterOp.Debug = true;
        }

        [Command("debug clear rasterop debug", "Disable extended RasterOp debugging")]
        private void ClearRopDebug()
        {
            PERQemu.Sys.CPU.RasterOp.Debug = false;
        }

        [Command("debug show rasterop state", "Display current RasterOp unit status")]
        private void ShowRopState()
        {
            PERQemu.Sys.CPU.RasterOp.ShowState();
        }

        [Command("debug show rasterop fifos", "Display current RasterOp source & destination FIFOs")]
        private void ShowRopFifos()
        {
            PERQemu.Sys.CPU.RasterOp.ShowFifos();
        }

        [Command("debug show rasterop registers", "Display current RasterOp register contents")]
        private void ShowRopRegs()
        {
            PERQemu.Sys.CPU.RasterOp.ShowRegs();
        }
#endif

        //
        // Interrupts and I/O
        //

        [Command("debug raise interrupt", "Assert a specific CPU interrupt")]
        public void RaiseInterrupt(InterruptSource i)
        {
            PERQemu.Sys.CPU.RaiseInterrupt(i);
        }

        [Command("debug clear interrupt", "Clear a specific CPU interrupt")]
        public void ClearInterrupt(InterruptSource i)
        {
            PERQemu.Sys.CPU.ClearInterrupt(i);
        }

        //
        // Breakpoints (not yet implemented)
        //

        [Command("debug set breakpoint", "Set a breakpoint at microaddress [addr]")]
        public void SetBreakpoint(ushort addr)
        {
            Console.WriteLine("Setting a breakpoint at " + addr);
        }

        [Command("debug set breakpoint at dds", "Set a breakpoint when the DDS reaches [val]")]
        public void SetDDSBreakpoint(ushort dds)
        {
            Console.WriteLine("Setting a breakpoint at DDS " + dds);
        }

        [Command("debug set breakpoint io port", "Set a breakpoint on access to I/O port [port]")]
        public void SetIOBreakpoint(byte port)
        {
            Console.WriteLine("Setting a breakpoint on I/O port " + port);
        }

        [Command("debug clear breakpoint", "Clear a breakpoint at microaddress [addr]")]
        public void ClearBreakpoint(ushort addr)
        {
            Console.WriteLine("Clearing a breakpoint at " + addr);
        }

        [Command("debug clear all breakpoints", "Clear all breakpoints")]
        public void ClearBreakpoints()
        {
            Console.WriteLine("Clearing all breakpoints");
        }

        [Command("debug enable breakpoints", "Enable breakpoints")]
        public void EnableBreakpoints()
        {
            Console.WriteLine("Enabling breakpoints");
        }

        [Command("debug disable breakpoints", "Disable breakpoints")]
        public void DisableBreakpoints()
        {
            Console.WriteLine("Disabling breakpoints");
        }

        //
        // Microcode
        //

        [Command("debug load microcode", "Loads the specified microcode file into the PERQ's writable control store")]
        private void LoadMicrocode(string ucodeFile)
        {
            try
            {
                PERQemu.Sys.CPU.LoadMicrocode(ucodeFile);
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load microcode from {0}, error: {1}", ucodeFile, e.Message);
            }
        }

        [Command("debug disassemble microcode", "Disassembles microcode instructions from the WCS (@ current PC)")]
        private void DisassembleMicrocode()
        {
            DisassembleMicrocode(PERQemu.Sys.CPU.PC, 16);
        }

        [Command("debug disassemble microcode", "Disassembles microcode instructions from the WCS (@ [addr])")]
        private void DisassembleMicrocode(ushort startAddress)
        {
            DisassembleMicrocode(startAddress, 16);
        }

        [Command("debug disassemble microcode", "Disassembles microcode instructions from the WCS (@ [addr, len])")]
        private void DisassembleMicrocode(ushort startAddress, ushort length)
        {
            ushort endAddr = (ushort)(startAddress + length);

            if (startAddress >= PERQemu.Sys.CPU.Microcode.Length || endAddr >= PERQemu.Sys.CPU.Microcode.Length)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and {0}", PERQemu.Sys.CPU.Microcode.Length);
                return;
            }

            // Disassemble microcode
            for (ushort i = startAddress; i < endAddr; i++)
            {
                string line = Disassembler.Disassemble(i, PERQemu.Sys.CPU.GetInstruction(i));
                Console.WriteLine(line);
            }
        }

        [Command("debug jump", "Start or resume execution at a given microinstruction address")]
        private void JumpTo(ushort nextPC)
        {
            if (nextPC >= PERQemu.Sys.CPU.Microcode.Length)
            {
                Console.WriteLine("Address outside of range 0..{0}; PC not modified.", PERQemu.Sys.CPU.Microcode.Length - 1);
            }
            else
            {
                PERQemu.Sys.CPU.PC = nextPC;
                // resume execution
            }
        }

        //
        // Miscellany and temporary/debugging hacks
        //

        [Command("debug dump scheduler queue")]
        private void DumpScheduler()
        {
            PERQemu.Sys.Display.Status();
            PERQemu.Sys.VideoController.Status();
            PERQemu.Sys.Scheduler.DumpEvents();
        }

        [Conditional("DEBUG")]
        [Command("debug dump timers")]
        private void DumpTimers()
        {
            HighResolutionTimer.DumpTimers();
        }
    }
}
