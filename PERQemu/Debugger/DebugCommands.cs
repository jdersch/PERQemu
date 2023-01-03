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
    /// MethodInvokeInfo class and added the GetInstanceFrom*() approach.)  The
    /// idea was that with a fully graphical interface AND a command-line we'd
    /// want a common API for getting data out of the CPU and its various parts,
    /// and pull all of those Console.WriteLines OUT of there.  Anyway, it's a
    /// work in progress.
    /// 
    /// NB: nearly all the debug commands require that the PERQ is instantiated
    /// in order to observe/manipulate state; the CheckSys() method provides a
    /// (hackish) common way to make sure the PERQ is defined.  It's also up to
    /// the individual methods to consider the impact of poking at the machine
    /// while it's running; some may require a pause/resume to operate reliably.
    /// </remarks>
    public partial class DebugCommands
    {

        [Command("debug", "Enter the debugging subsystem", Prefix = true)]
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
            Console.WriteLine("--------------------------");

            Console.WriteLine("Loaded Q-code set: {0}", QCodeHelper.Loaded);

            if (PERQemu.Sys != null)
            {
                Console.WriteLine("Breakpoints are {0}", PERQemu.Sys.Debugger.BreakpointsEnabled ? "enabled" : "disabled");
            }

            if (Log.ToConsole)
            {
                Console.WriteLine("Console logging is enabled, threshold {0}", Log.Level);
            }
            else
            {
                Console.WriteLine("Console logging is disabled");
            }

            if (Log.ToFile)
            {
                Console.WriteLine("File logging is enabled, threshold {0}", Log.FileLevel);
                Console.WriteLine("Current file is {0}", Paths.Canonicalize(Log.OutputFile));
            }
            else
            {
                Console.WriteLine("File logging is {0}", Log.LoggingAvailable ? "disabled" : "not available");
            }

            Console.WriteLine("Logging categories:");
            PERQemu.CLI.Columnify(Log.Categories.ToString().Split(' '));

            // todo: may just bite the bullet and have separate console logging
            // categories too, not just split severity levels... being able to
            // just spit the important stuff on the console while logging more
            // detailed traces to files would be nice...

            // todo: show default output radix, anything else?
        }

        [Command("debug show variables", "Show debugger variables and their descriptions")]
        void ShowVars()
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

        [Command("step", Repeatable = true)]
        [Command("debug step", "Run one microinstruction", Repeatable = true)]
        public void DebugStep()
        {
            if (PERQemu.Controller.State > RunState.Off)
            {
                PERQemu.Controller.TransitionTo(RunState.SingleStep);
                PERQemu.Sys.PrintStatus();
            }
            else
            {
                Console.WriteLine("The PERQ is currently turned off.");
            }
        }

        [Command("inst", Repeatable = true)]
        [Command("debug inst", "Run one Q-code", Repeatable = true)]
        public void DebugInst()
        {
            if (PERQemu.Controller.State > RunState.Off)
            {
                PERQemu.Controller.TransitionTo(RunState.RunInst);
                PERQemu.Sys.PrintStatus();
            }
            else
            {
                Console.WriteLine("The PERQ is currently turned off.");
            }
        }

        [Command("debug set execution mode", "Set the execution mode for the virtual PERQ")]
        void SetExecutionMode(ExecutionMode mode)
        {
            if (PERQemu.Controller.Mode != mode)
            {
                PERQemu.Controller.Mode = mode;
                Console.WriteLine("Execution mode changed to {0}.", mode);
            }
        }

        [Command("debug show execution mode", "Show the execution mode for the virtual PERQ")]
        void ShowExecutionMode()
        {
            Console.WriteLine(PERQemu.Controller.Mode);
        }

        //
        // Logging
        //

        [Command("debug set logging", "Set logging for specific events")]
        public void SetLogging(Category category)
        {
            Log.Categories |= category;
            Console.WriteLine("Logging: {0}", Log.Categories);
        }

        [Command("debug clear logging", "Clear logging for certain events")]
        public void ClearLogging(Category category)
        {
            Log.Categories &= ~category;
            Console.WriteLine("Logging: {0}", Log.Categories);
        }

        [Command("debug set console loglevel", "Set severity threshold for console logging")]
        public void SetConsoleSeverity(Severity severity)
        {
            if (severity != Log.Level)
            {
                Log.Level = severity;
                Console.WriteLine("Console logging level set to " + severity);
            }
        }

        [Command("debug enable console logging", "Enable logging to the console")]
        public void EnableLogging()
        {
            if (!Log.ToConsole)
            {
                Log.ToConsole = true;
                Console.WriteLine("Console logging enabled.");
            }
        }

        [Command("debug disable console logging", "Disable logging to the console")]
        public void DisableLogging()
        {
            if (Log.ToConsole)
            {
                Log.ToConsole = false;
                Console.WriteLine("Console logging disabled.");
            }
        }

#if TRACING_ENABLED

        [Command("debug set file loglevel", "Set severity threshold for file logging")]
        public void SetFileSeverity(Severity severity)
        {
            if (severity != Log.FileLevel)
            {
                Log.FileLevel = severity;
                Console.WriteLine("File logging level set to " + severity);
            }
        }

        [Command("debug enable file logging", "Enable logging to file")]
        public void EnableFileLogging()
        {
            if (!Log.ToFile)
            {
                Log.ToFile = true;
                Log.Write(Severity.None, Category.All, "Logging started at {0}", DateTime.Now);

                Console.WriteLine("File logging enabled.");
            }
        }

        [Command("debug disable file logging", "Disable logging to file")]
        public void DisableFileLogging()
        {
            if (Log.ToFile)
            {
                Log.Write(Severity.None, Category.All, "Logging stopped at {0}", DateTime.Now);
                Log.ToFile = false;

                Console.WriteLine("File logging disabled.");
            }
        }

#else
        // Define these to spit out an explanation rather than "Invalid command."

        [Command("debug set file loglevel")]
        private void NoLogLevel(Severity s)
        {
            Console.WriteLine("File logging is not available.");
        }

        [Command("debug enable file logging")]
        [Command("debug disable file logging")]
        private void NoLogging()
        {
            Console.WriteLine("File logging is not available.");
        }
#endif

        bool CheckSys()
        {
            if (PERQemu.Sys == null)
            {
                Console.WriteLine("Cannot execute; the PERQ is not powered on.");
                return false;
            }
            return true;
        }

        //
        // Registers and Stacks
        //

        [Command("debug show cpu registers", "Display the values of the CPU registers")]
        void ShowCPURegs()
        {
            if (CheckSys()) PERQemu.Sys.CPU.ShowCPUState();
        }

        [Command("debug show opfile", "Display the contents of the opcode file")]
        void ShowOpfile()
        {
            if (CheckSys()) PERQemu.Sys.CPU.ShowOpfile();
        }

        [Command("debug show estack", "Display the contents of the expression stack")]
        void ShowEStack()
        {
            if (CheckSys()) PERQemu.Sys.CPU.ShowEStack();
        }

        [Command("debug show cstack", "Display the contents of the 2910 call stack")]
        void ShowCStack()
        {
            if (CheckSys()) PERQemu.Sys.CPU.ShowCStack();
        }

        [Command("debug show xy register", "Display the values of the XY registers")]
        void ShowRegister()
        {
            if (!CheckSys()) return;

            var values = new string[256];

            for (var reg = 0; reg <= 255; reg++)
            {
                values[reg] = string.Format("XY[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister((byte)reg));
            }
            PERQemu.CLI.Columnify(values);
        }

        // todo: if :xy[n] works, and :xy[n]=m works, then these could be removed
        // and the previous one could be "show xy registers" to dump all of 'em

        [Command("debug show xy register", "Display the value of an XY register")]
        void ShowRegister(byte reg)
        {
            if (!CheckSys()) return;

            Console.WriteLine("XY[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister(reg));
        }

        [Conditional("DEBUG")]
        [Command("debug set xy register", "Change the value of an XY register")]
        void SetRegister(byte reg, uint val)
        {
            if (!CheckSys()) return;

            PERQemu.Sys.CPU.WriteRegister(reg, (int)val);   // Clips to 20-24 bits
            Console.WriteLine("XY[{0:x2}]={1:x6}", reg, PERQemu.Sys.CPU.ReadRegister(reg));
        }

        //
        // Memory and RasterOp
        //

        [Command("debug show memory", "Dump the PERQ's memory")]
        void ShowMemory(uint startAddr, uint words = 64)
        {
            if (!CheckSys()) return;

            if (startAddr > PERQemu.Sys.Memory.MemSize - 1)
            {
                Console.WriteLine("Start address must be in range 0..{0}", PERQemu.Sys.Memory.MemSize - 1);
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
                    var high = (char)((PERQemu.Sys.Memory.FetchWord(j) & 0xff00) >> 8);
                    var low = (char)(PERQemu.Sys.Memory.FetchWord(j) & 0xff);

                    high = PERQemu.CLI.IsPrintable(high) ? high : '.';
                    low = PERQemu.CLI.IsPrintable(low) ? low : '.';

                    line.AppendFormat("{0}{1}", low, high);     // Reversed so strings read right :)
                }

                Console.WriteLine(line);
            }
        }

#if DEBUG
        [Command("debug find memory", "Find a specific value in the PERQ's memory [@start, val]")]
        void FindMemory(uint startAddress, ushort val)
        {
            if (!CheckSys()) return;

            if (startAddress >= PERQemu.Sys.Memory.MemSize)
            {
                Console.WriteLine("Start address must be in range 0..{0}", PERQemu.Sys.Memory.MemSize - 1);
                return;
            }

            ushort[] mem = PERQemu.Sys.Memory.Memory;

            for (uint i = startAddress; i < mem.Length; i++)
            {
                if (mem[i] == val) ShowMemory((i & 0xffffc), 4);        // show the quadword
            }
        }

        [Command("debug set memory", "Write a specific value in the PERQ's memory")]
        void SetMemory(uint address, ushort val)
        {
            if (!CheckSys()) return;

            if (address >= PERQemu.Sys.Memory.MemSize)
            {
                Console.WriteLine("Address must be in range 0..{0}", PERQemu.Sys.Memory.MemSize - 1);
                return;
            }

            PERQemu.Sys.Memory.StoreWord((int)address, val);
        }

        [Command("debug show memstate", "Dump the memory controller state")]
        void ShowMemQueues()
        {
            if (CheckSys()) PERQemu.Sys.Memory.DumpQueues();
        }

        // todo: most of the really detailed rasterop debugging stuff is no longer
        // needed?  the rasterop unit seems to be 100% now, so other than performance
        // tuning much of this can be removed and archived...

        [Command("debug set rasterop debug", "Enable extended RasterOp debugging")]
        void SetRopDebug()
        {
            if (CheckSys()) PERQemu.Sys.CPU.RasterOp.Debug = true;
        }

        [Command("debug clear rasterop debug", "Disable extended RasterOp debugging")]
        void ClearRopDebug()
        {
            if (CheckSys()) PERQemu.Sys.CPU.RasterOp.Debug = false;
        }

        [Command("debug show rasterop state", "Display current RasterOp unit status")]
        void ShowRopState()
        {
            if (CheckSys()) PERQemu.Sys.CPU.RasterOp.ShowState();
        }

        [Command("debug show rasterop fifos", "Display current RasterOp source & destination FIFOs")]
        void ShowRopFifos()
        {
            if (CheckSys()) PERQemu.Sys.CPU.RasterOp.ShowFifos();
        }

        [Command("debug show rasterop registers", "Display current RasterOp register contents")]
        void ShowRopRegs()
        {
            if (CheckSys()) PERQemu.Sys.CPU.RasterOp.ShowRegs();
        }
#endif

        //
        // Interrupts and I/O
        //

        [Command("debug raise interrupt", "Assert a specific CPU interrupt")]
        public void RaiseInterrupt(InterruptSource irq)
        {
            if (CheckSys()) PERQemu.Sys.CPU.RaiseInterrupt(irq);
        }

        [Command("debug clear interrupt", "Clear a specific CPU interrupt")]
        public void ClearInterrupt(InterruptSource irq)
        {
            if (CheckSys()) PERQemu.Sys.CPU.ClearInterrupt(irq);
        }

        [Command("debug set ethernet status", "Force a return status code from the fake Ethernet device")]
        public void SetEtherStatus(byte status)
        {
            if (CheckSys())
            {
                var oio = PERQemu.Sys.OIO as IO.OIO;
                oio.Ether.SetStatus(status);
            }
        }

        [Command("debug patch ethernet status", "HACK to patch instruction to check Ethernet status (Accent S7)")]
        public void PatchEther()
        {
            if (CheckSys())
            {
                var inst = PERQemu.Sys.CPU.GetInstruction(0xdbc);
                Console.WriteLine($"Instruction at 0x0dbc is {inst}");

                // If it's an "or not", patch the ALU op to an "and"
                if (inst.UCode == 0xb3ef5a324203)
                {
                    Console.WriteLine("Patching the instruction!");

                    // New inst is: b3ef 5932 4203;
                    // scrambled:   b3ef 03b2 4249
                    // inverted:    b3ef fc4d bdb6

                    // Rewrite the low and middle words!
                    PERQemu.Sys.CPU.WCS.WriteControlStore(CPU.ControlStoreWord.Low, 0xdbc, 0xbdb6);
                    PERQemu.Sys.CPU.WCS.WriteControlStore(CPU.ControlStoreWord.Middle, 0xdbc, 0xfc4d);

                    // Confirmation
                    inst = PERQemu.Sys.CPU.GetInstruction(0xdbc);
                    Console.WriteLine($"Instruction at 0x0dbc is {inst}");
                }
            }
        }

#if DEBUG
        //
        // Breakpoints
        //
        // ARGH!  This would be SO much easier in a GUI!  Sigh.
        //
        [Command("debug set breakpoint", "Set a breakpoint")]
        public void SetBreakpoint(BreakpointType type, int watch) // action (string)  ARGH
        {
            if (!CheckSys()) return;

            if (GetBreakpoints(type).Count != 1)
            {
                Console.WriteLine($"Can't set breakpoint of type {type}.");
                return;
            }

            var list = GetBreakpoints(type)[0];

            if (watch < 0 || watch > list.Range)
            {
                Console.WriteLine($"{watch} is out of range 0..{list.Range}");
                return;
            }

            var action = GetDefaultActionFor(type);
            list.Watch(watch, action);

            Console.WriteLine($"Breakpoint set for {list.Name} {watch}");

            // For now, breakpoints must be explicitly enabled, because reasons.
            if (!PERQemu.Sys.Debugger.BreakpointsEnabled)
                Console.WriteLine("NOTE: breakpoints are not currently enabled.");
        }

        [Command("debug clear breakpoint", "Clear a breakpoint")]
        public void ClearBreakpoint(BreakpointType type, int watch)
        {
            if (!CheckSys()) return;

            if (GetBreakpoints(type).Count != 1)
            {
                Console.WriteLine($"Can't clear a breakpoint of type {type}.");
                return;
            }

            var list = GetBreakpoints(type)[0];

            if (!list.IsWatched(watch))
            {
                Console.WriteLine($"No {type} breakpoint at {watch}.");
                return;
            }

            list.Unwatch(watch);

            Console.WriteLine($"{type} breakpoint at {watch} cleared.");
        }

        [Command("debug reset breakpoints", "Reset breakpoint counters")]
        public void ResetBreakpoints(BreakpointType type)
        {
            if (!CheckSys()) return;

            if (GetBreakpoints(type).Count > 0)
            {
                foreach (var list in GetBreakpoints(type))
                {
                    list.ResetCounts();
                }
                Console.WriteLine($"{type} breakpoint counters reset.");
            }
        }

        [Command("debug enable breakpoints", "Enable breakpoints")]
        public void EnableBreakpoints()
        {
            if (CheckSys()) PERQemu.Sys.Debugger.EnableBreakpoints(true);
        }

        [Command("debug disable breakpoints", "Disable breakpoints")]
        public void DisableBreakpoints()
        {
            if (CheckSys()) PERQemu.Sys.Debugger.EnableBreakpoints(false);
        }

        [Command("debug edit breakpoint", "Change or reset a breakpoint", Prefix = true)]
        public void EditBreakpoint(BreakpointType type, int watch)
        {
            if (!CheckSys()) return;

            if (GetBreakpoints(type).Count != 1)
            {
                Console.WriteLine($"Can't edit breakpoints of type {type}.");
                return;
            }

            var list = GetBreakpoints(type)[0];

            if (!list.IsWatched(watch))
            {
                Console.WriteLine($"No {type} breakpoint at {watch}.");
                return;
            }

            // Make a copy of the breakpoint for editing
            var orig = list.GetActionFor(watch);

            _bp = new BreakpointEventArgs(type, watch, orig);     // CHEESE!
            _bpAction = new BreakpointAction();

            // Uh, isn't there some built-in way to do this?
            _bpAction.Count = orig.Count;
            _bpAction.Enabled = orig.Enabled;
            _bpAction.PauseEmulation = orig.PauseEmulation;
            _bpAction.Retriggerable = orig.Retriggerable;
            _bpAction.Script = orig.Script;
            _bpAction.Callback = orig.Callback;

            PERQemu.CLI.SetPrefix("debug edit breakpoint");
        }

        [Command("debug edit breakpoint commands", "Show breakpoint edit commands")]
        public void ShowEditCommands()
        {
            PERQemu.CLI.ShowCommands("debug edit breakpoint");
        }

        [Command("debug edit breakpoint enable", "Enable or disable the breakpoint")]
        public void EditEnable(bool enabled)
        {
            if (_bpAction.Enabled != enabled)
            {
                _bpAction.Enabled = enabled;
                Console.WriteLine($"Enabled changed to {enabled}.");
            }
        }

        [Command("debug edit breakpoint pause emulation", "Set/clear flag to pause emulation when triggered")]
        public void EditPause(bool pause)
        {
            if (_bpAction.PauseEmulation != pause)
            {
                _bpAction.PauseEmulation = pause;
                Console.WriteLine($"Pause Emulation changed to {pause}.");
            }
        }

        [Command("debug edit breakpoint retriggerable", "Set the breakpoint as one-shot or retriggerable")]
        public void EditRetrigger(bool oneshot)
        {
            if (_bpAction.Retriggerable != oneshot)
            {
                _bpAction.Retriggerable = oneshot;
                Console.WriteLine($"Retriggerable changed to {oneshot}.");
            }
        }

        [Command("debug edit breakpoint reset count", "Reset the breakpoint counter")]
        public void EditCount()
        {
            if (_bpAction.Count > 0)
            {
                _bpAction.Count = 0;
                Console.WriteLine("Count reset.");
            }
        }

        [Command("debug edit breakpoint script", "Run a script when the breakpoint triggers")]
        public void EditScript(string script)
        {
            // todo: validate/mangle the path, look it up using the Scripts/ dir, .scr suffix?
            if (_bpAction.Script != script)
            {
                _bpAction.Script = script;
                Console.WriteLine("Script '{0}' will be run.", Paths.Canonicalize(script));
            }
        }

        [Command("debug edit breakpoint no script", "Do not run a script when triggered")]
        public void EditNoScript()
        {
            if (_bpAction.Script != "")
            {
                _bpAction.Script = "";
                Console.WriteLine("No script will be run.");
            }
        }

        [Command("debug edit breakpoint show", "Show the current breakpoint")]
        public void EditShow()
        {
            const string fmt = "{0,10}  {1,-20} {2,-20}";

            // I am not proud of this
            var orig = _bp.Args[0] as BreakpointAction;

            Console.WriteLine("Breakpoint: {0} @ {1:x} ({2})", _bp.Type, _bp.Value, _bp.Value);

            Console.WriteLine(fmt, " ", "Original", "Edited");
            Console.WriteLine(fmt, "Enabled:", orig.Enabled, _bpAction.Enabled);
            Console.WriteLine(fmt, "Pause Emu:", orig.PauseEmulation, _bpAction.PauseEmulation);
            Console.WriteLine(fmt, "ReTrigger:", orig.Retriggerable, _bpAction.Retriggerable);
            Console.WriteLine(fmt, "Count:", $"{orig.Count} times", $"{_bpAction.Count} times");
            Console.WriteLine(fmt, "Script:",
                              (string.IsNullOrEmpty(orig.Script) ? "<none>" : orig.Script),
                              (string.IsNullOrEmpty(_bpAction.Script) ? "<none>" : _bpAction.Script));
        }

        [Command("debug edit breakpoint cancel", "Cancel and return to previous level")]
        public void EditCancel()
        {
            Console.WriteLine("Canceled.");
            SetDebugPrefix();
        }

        [Command("debug edit breakpoint done", "Save changes and return to previous level")]
        public void EditDone()
        {
            // Save/apply the current temporary action
            var list = GetBreakpoints(_bp.Type)[0];
            list.Watch(_bp.Value, _bpAction);

            Console.WriteLine("Saved.");
            SetDebugPrefix();
        }

        [Command("debug show breakpoints", "Show the status of all breakpoints")]
        public void ShowBreakpoints()
        {
            ShowBreakpoints(BreakpointType.All);
        }

        [Command("debug show breakpoints", "Show the status of breakpoints")]
        public void ShowBreakpoints(BreakpointType type)
        {
            if (!CheckSys()) return;

            if (type == BreakpointType.None)
            {
                Console.WriteLine("Don't be silly.");
                return;
            }

            int total = 0;
            var breakpoints = GetBreakpoints(type);

            foreach (var list in breakpoints)
            {
                if (list.Count > 0)
                {
                    total += list.Count;
                    Console.WriteLine();
                    Console.WriteLine("{0} watch list:", list.Name);
                    list.ShowActions();
                }
            }

            if (total == 0)
            {
                Console.WriteLine("No breakpoints currently defined.");
            }
        }

        List<BreakpointList> GetBreakpoints(BreakpointType bp)
        {
            var selected = new List<BreakpointList>();

            switch (bp)
            {
                case BreakpointType.IOPort:
                    selected.Add(PERQemu.Sys.Debugger.WatchedIOPorts);
                    break;

                case BreakpointType.Interrupt:
                    selected.Add(PERQemu.Sys.Debugger.WatchedInterrupts);
                    break;

                case BreakpointType.uAddress:
                    selected.Add(PERQemu.Sys.Debugger.WatchedMicroaddress);
                    break;

                case BreakpointType.MemoryLoc:
                    selected.Add(PERQemu.Sys.Debugger.WatchedMemoryAddress);
                    break;

                case BreakpointType.All:
                    selected.Add(PERQemu.Sys.Debugger.WatchedInterrupts);
                    selected.Add(PERQemu.Sys.Debugger.WatchedMicroaddress);
                    selected.Add(PERQemu.Sys.Debugger.WatchedIOPorts);
                    selected.Add(PERQemu.Sys.Debugger.WatchedMemoryAddress);
                    break;
            }

            return selected;
        }

        BreakpointAction GetDefaultActionFor(BreakpointType bp)
        {
            var action = new BreakpointAction();

            switch (bp)
            {
                case BreakpointType.uAddress:
                    action.Callback = OnInstAddr;
                    break;

                case BreakpointType.Interrupt:
                    action.Callback = OnInterrupt;
                    break;

                case BreakpointType.IOPort:
                case BreakpointType.MemoryLoc:
                    action.Callback = OnBreakpoint;
                    break;
            }

            return action;
        }

        //
        // Breakpoint handlers
        //

        void OnBreakpoint(BreakpointEventArgs a)
        {
            // Default action
            Console.WriteLine($"Breakpoint {a.Type} at {a.Value}");
        }

        void OnInstAddr(BreakpointEventArgs a)
        {
            var upc = (ushort)a.Value;

            Console.WriteLine($"Microinstruction at {upc:x4} executed!");
        }

        void OnInterrupt(BreakpointEventArgs a)
        {
            var irq = (InterruptSource)a.Value;
            var status = (bool)a.Args[0];

            Console.WriteLine($"CPU Interrupt {irq} now {status}");
        }
#endif

        //
        // Microcode
        //

        [Command("debug load microcode", "Load a microcode binary into the control store")]
        void LoadMicrocode([PathExpand] string binfile)
        {
            if (!CheckSys()) return;

            try
            {
                // todo: whoops, should probably not do this IF THE MACHINE IS RUNNING
                PERQemu.Sys.CPU.LoadMicrocode(Paths.Canonicalize(binfile));
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to load microcode from '{binfile}': {e.Message}");
            }
        }

        [Command("debug disassemble microcode", "Disassemble microinstructions (@ current PC)")]
        void DisassembleMicrocode()
        {
            DisassembleMicrocode(PERQemu.Sys.CPU.PC, 16);
        }

        [Command("debug disassemble microcode", "Disassemble microinstructions (@ [addr])")]
        void DisassembleMicrocode(ushort startAddress)
        {
            DisassembleMicrocode(startAddress, 16);
        }

        [Command("debug disassemble microcode", "Disassemble microinstructions (@ [addr, len])")]
        void DisassembleMicrocode(ushort startAddress, ushort length)
        {
            if (!CheckSys()) return;

            var endAddr = Math.Min(startAddress + length, PERQemu.Sys.CPU.Microcode.Length);

            if (startAddress > PERQemu.Sys.CPU.Microcode.Length - 1)
            {
                Console.WriteLine("Address out of range 0..{0}", PERQemu.Sys.CPU.Microcode.Length - 1);
                return;
            }

            // Disassemble microcode
            for (ushort i = startAddress; i < endAddr; i++)
            {
                var line = Disassembler.Disassemble(i, PERQemu.Sys.CPU.GetInstruction(i));
                Console.WriteLine(line);
            }
        }

        [Command("debug jump", "Start or resume execution at a given microaddress")]
        void JumpTo(ushort nextPC)
        {
            if (!CheckSys()) return;

            if (nextPC > PERQemu.Sys.CPU.Microcode.Length - 1)
            {
                Console.WriteLine("Address out of range 0..{0}; PC not modified.",
                                  PERQemu.Sys.CPU.Microcode.Length - 1);
            }
            else
            {
                PERQemu.Sys.CPU.PC = nextPC;
                // resume execution
            }
        }

        [Command("debug load qcodes", "Load Q-code definitions for opcode disassembly")]
        void LoadQCodes(QCodeSets qcodes)
        {
            QCodeHelper.LoadQCodeSet(qcodes);
        }

        /// <summary>
        /// Leverage the Instruction/ControlStore code to assist in building
        /// boot ROM images from raw hex dumps.  (Once they're all successfully
        /// ripped, converted to binaries and tested, this can be removed.)
        /// </summary>
        [Conditional("DEBUG")]
        [Command("debug make boot prom", Discreet = true)]
        void MakeROM(string basename, string output, bool fourk)
        {
            // Turn on logging
            Log.Categories |= Category.Microstore;
            Log.Level = Severity.Debug;

            var grinder = new Unscrambler();

            grinder.MakeBootPROM(basename, output, fourk);

            Console.WriteLine("\nDisassembly:");
            grinder.Reload(output);
            grinder.ShowDisassembly();
        }

        //
        // Miscellany and temporary/debugging hacks
        //

        [Conditional("DEBUG")]
        [Command("debug dump scheduler queue")]
        void DumpScheduler()
        {
            PERQemu.Sys.Scheduler.DumpEvents("CPU");
        }

        [Conditional("DEBUG")]
        [Command("debug dump timers")]
        void DumpTimers()
        {
            HighResolutionTimer.DumpTimers();
        }

        [Conditional("DEBUG")]
        [Command("debug dump fifos")]
        void DumpFifos()
        {
            PERQemu.Sys.IOB.Z80System.DumpFifos();
        }

        [Conditional("DEBUG")]
        [Command("debug dump qcodes")]
        void DumpQcodes()
        {
            QCodeHelper.DumpContents();
        }

        [Command("debug dump rs232a")]
        void ShowRS232Status()
        {
            PERQemu.Sys.IOB.Z80System.SIOA.DumpPortStatus(0);
        }

        [Command("debug dump streamer")]
        void ShowStreamerStatus()
        {
            if (PERQemu.Sys.OIO != null)
            {
                var hack = PERQemu.Sys.OIO as IO.OIO;
                hack.DumpTapeStatus();
                return;
            }

            Console.WriteLine("No streamer drive.");
        }

        [Command("debug show tape block", "Show contents of a tape block")]
        void ShowStreamerBlock(int pos)
        {
            if (!CheckSys()) return;

            if (pos >= PERQemu.Sys.Volumes[3].Geometry.TotalBlocks)
            {
                Console.WriteLine("Address must be in range 0..{0}", PERQemu.Sys.Volumes[3].Geometry.TotalBlocks - 1);
                return;
            }

            var dev = PERQemu.Sys.Volumes[3] as IO.TapeDevices.CartridgeTape;
            var block = dev.Read(pos);

            Console.WriteLine($"Block {pos} is type {block.Type}:");

            // Format and display 16 bytes per line
            for (var i = 0; i < block.Data.Length; i += 16)
            {
                var line = new StringBuilder();
                line.AppendFormat("{0:x3}: ", i);

                for (var j = i; j < i + 16; j++)
                {
                    line.AppendFormat("{0:x2} ", block.Data[j]);
                }

                // ASCII representation
                for (var j = i; j < i + 16; j += 2)
                {
                    var high = (char)block.Data[j];
                    var low = (char)block.Data[j + 1];

                    high = PERQemu.CLI.IsPrintable(high) ? high : '.';
                    low = PERQemu.CLI.IsPrintable(low) ? low : '.';

                    line.AppendFormat("{0}{1}", low, high);     // Reversed so strings read right :)
                }

                Console.WriteLine(line);
            }
        }

#if DEBUG
        // A temporary working copy for editing breakpoints
        BreakpointEventArgs _bp;
        BreakpointAction _bpAction;
#endif
    }
}
