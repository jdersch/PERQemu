//
// DebugCommands.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
            if (Log.LoggingAvailable)
            {
                Console.WriteLine("Console logging {0} enabled.", Log.ToConsole ? "is" : "is not");

                if (Log.ToFile)
                {
                    Console.WriteLine("File logging is enabled, current file is {0}",
                                      Paths.Canonicalize(Log.OutputFile));
                }

                Console.WriteLine("Log level: {0}", Log.Level);
                Console.WriteLine("Logging categories:");
                PERQemu.CLI.Columnify(Log.Categories.ToString().Split(' '));
            }
            else
            {
                Console.WriteLine("Logging is not available.");
            }
            // radix, step mode, whatever
        }

        [Command("debug show variables", "Show debugger variables and their descriptions")]
        private void ShowVars()
        {
            if (PERQemu.Sys == null)
            {
                Console.WriteLine("No PERQ defined.");
            }
            else
            {
                PERQemu.Sys.Debugger.ShowVariables();
            }
        }

        [Command("step")]
        [Command("debug step", "Run one microinstruction")]
        public void DebugStep()
        {
            if (PERQemu.Controller.State <= RunState.Off)
            {
                Console.WriteLine("The PERQ is currently turned off.");
            }
            else
            {
                PERQemu.Controller.TransitionTo(RunState.SingleStep);
                PERQemu.Sys.PrintStatus();
            }
        }

        [Command("inst")]
        [Command("debug inst", "Run one opcode")]
        public void DebugInst()
        {
            if (PERQemu.Controller.State <= RunState.Off)
            {
                Console.WriteLine("The PERQ is currently turned off.");
            }
            else
            {
                PERQemu.Controller.TransitionTo(RunState.RunInst);
                PERQemu.Sys.PrintStatus();
            }
        }

        [Command("debug set execution mode", "Set the execution mode for the virtual PERQ")]
        private void SetExecutionMode(ExecutionMode mode)
        {
            if (PERQemu.Controller.Mode != mode)
            {
                PERQemu.Controller.Mode = mode;
                Console.WriteLine("Execution mode changed to {0}.", mode);
            }
        }

        [Command("debug show execution mode", "Show the execution mode for the virtual PERQ")]
        private void ShowExecutionMode()
        {
            Console.WriteLine(PERQemu.Controller.Mode);
        }

        //
        // Logging
        //

        [Command("debug set logging", "Set logging for specific events")]
        [Conditional("TRACING_ENABLED")]
        public void SetLogging(Category category)
        {
            Log.Categories |= category;
            Console.WriteLine("Logging: {0}", Log.Categories);
        }

        [Command("debug clear logging", "Clear logging for certain events")]
        [Conditional("TRACING_ENABLED")]
        public void ClearLogging(Category category)
        {
            Log.Categories &= ~category;
            Console.WriteLine("Logging: {0}", Log.Categories);
        }

        [Command("debug set loglevel", "Set logging threshold")]
        public void SetSeverity(Severity severity)
        {
            if (severity != Log.Level)
            {
                Log.Level = severity;
                Console.WriteLine("Logging level set to " + severity);
            }
        }

        [Command("debug enable logging to console", "Enable logging to the console")]
        [Conditional("TRACING_ENABLED")]
        public void EnableLogging()
        {
            if (!Log.ToConsole)
            {
                Log.ToConsole = true;
                Console.WriteLine("Console logging enabled.");
            }
        }

        [Command("debug disable logging to console", "Disable logging to the console")]
        [Conditional("TRACING_ENABLED")]
        public void DisableLogging()
        {
            if (Log.ToConsole)
            {
                Log.ToConsole = false;
                Console.WriteLine("Console logging disabled.");
            }
        }

        [Command("debug enable logging to file", "Enable logging to file")]
        [Conditional("TRACING_ENABLED")]
        public void EnableFileLogging()
        {
            if (!Log.ToFile)
            {
                Log.ToConsole = true;
                // todo: call to init current log file?
                Console.WriteLine("File logging enabled.");
            }
        }

        [Command("debug disable logging to file", "Disable logging to file")]
        [Conditional("TRACING_ENABLED")]
        public void DisableFileLogging()
        {
            if (Log.ToFile)
            {
                Log.ToFile = false;
                // todo: call to close current log file
                Console.WriteLine("File logging disabled.");
            }
        }

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
            Console.WriteLine("XY[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister(reg));
        }

        [Conditional("DEBUG")]
        [Command("debug set xy register", "Change the value of an XY register")]
        private void SetRegister(byte reg, uint val)
        {
            PERQemu.Sys.CPU.WriteRegister(reg, (int)val);   // Clips to 20-24 bits
            Console.WriteLine("XY[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister(reg));
        }

        //
        // Memory and RasterOp
        //

        [Command("debug show memory", "Dump the PERQ's memory")]
        private void ShowMemory(uint startAddr, uint words = 64)
        {
            if (startAddr > PERQemu.Sys.Memory.MemSize - 1)
            {
                Console.WriteLine("Argument out of range -- must be between 0 and {0}",
                                  PERQemu.Sys.Memory.MemSize - 1);
                return;
            }

            // Round down to nearest octoword by masking off low address bits
            var start = (int)(startAddr & 0xfffffff8);
            var end = (int)Math.Min(PERQemu.Sys.Memory.MemSize, start + words);

            // Format and display 8 words per line
            for (var i = start; i < end; i += 8)
            {
                var line = new StringBuilder();
                line.AppendFormat("{0:x6}: ", i);

                // Words in hex (todo: add output radix support)
                for (var j = i; j < i + 8; j++)
                {
                    line.AppendFormat("{0:x4} ", PERQemu.Sys.Memory.FetchWord(j));
                }

                // ASCII representation
                for (var j = i; j < i + 8; j++)
                {
                    char high = (char)((PERQemu.Sys.Memory.FetchWord(j) & 0xff00) >> 8);
                    char low = (char)(PERQemu.Sys.Memory.FetchWord(j) & 0xff);

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
                PERQemu.Sys.CPU.LoadMicrocode(Paths.Canonicalize(ucodeFile));
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load microcode from '{0}': {1}", ucodeFile, e.Message);
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

        //[Conditional("DEBUG")]
        [Command("debug dump scheduler queue")]
        private void DumpScheduler()
        {
            PERQemu.Sys.Scheduler.DumpEvents("CPU");
        }

        //[Conditional("DEBUG")]
        [Command("debug dump timers")]
        private void DumpTimers()
        {
            HighResolutionTimer.DumpTimers();
        }
    }
}
