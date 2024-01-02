//
// Z80System.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Threading;

using Konamiman.Z80dotNet;

using PERQemu.Debugger;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The Z80System is an embedded system that runs the PERQ's "low speed"
    /// peripherals.  It has its own ROM, RAM, DMA and peripheral bus and runs
    /// independently of the main CPU.  This class implements the underlying
    /// plumbing common to both the PERQ-1 IOB/CIO and PERQ-2 EIO/NIO boards.
    /// Subclasses attach the peripheral controllers, FIFOs, and other board-
    /// specific bits.
    /// </summary>
    public abstract class Z80System
    {
        protected Z80System(PERQSystem system)
        {
            _system = system;
            _running = false;

            _scheduler = new Scheduler(IOBoard.Z80CycleTime);

            _bus = new Z80IOBus(this);
            _memory = new Z80MemoryBus();
            _cpu = new Z80Processor();
            _cpu.Memory = _memory;
            _cpu.PortsSpace = _bus;
            _cpu.ClockSynchronizer = null;      // We'll do our own rate limiting

            _sync = new ManualResetEventSlim();
            _wakeup = long.MaxValue;
        }

        public bool SupportsAsync => true;
        public bool IsRunning => _running;
        public ulong Clocks => _cpu.TStatesElapsedSinceReset;
        public ulong Wakeup => (ulong)_wakeup;
        public ManualResetEventSlim Throttle => _sync;

        public Z80Processor CPU => _cpu;
        public Z80MemoryBus Memory => _memory;
        public Scheduler Scheduler => _scheduler;

        public NECuPD765A FDC => _fdc;
        public TMS9914A GPIB => _tms9914a;

        public abstract Z80SIO SIOA { get; }
        public abstract Z80CTC CTC { get; }

        public abstract void WriteStatus(int status);
        public abstract int ReadStatus();

        public abstract void WriteData(int data);
        public abstract int ReadData();

        public abstract void Run();
        public abstract void QueueKeyboardInput(byte keyCode);

        protected abstract bool FIFOInputReady { get; }
        protected abstract bool FIFOOutputReady { get; }

        protected abstract void DeviceReset();
        protected abstract void DeviceShutdown();

        // Debugging access
        public abstract void DumpFifos();
        public abstract void DumpPortAStatus();
        public abstract void DumpPortBStatus();


        /// <summary>
        /// Resets the Z80 subsystem and starts it running.
        /// </summary>
        /// <remarks>
        /// The Z80 starts up automatically on hardware reset.  The CPU boot ROMs
        /// turn it off after VFY/SYSB are loaded.  SYSB turns it on to read the
        /// boot char from the keyboard and turns it off again!  Then the OS and
        /// IO microcode finally turn it on for good during system establishment.
        /// Dizzying.
        /// </remarks>
        public void Reset(bool soft = false)
        {
            // Synchronize our scheduler to the main processor!
            _scheduler.Reset(_system.Scheduler.CurrentTimeNsec);
            _cpu.Reset();

            if (soft)
            {
                // A software-initiated reset may not do all devices...
                DeviceReset();
                Log.Debug(Category.Z80, "System (soft) reset");
            }
            else
            {
                // ...while a power-on or "hard" reset does everything
                _bus.Reset();
                Log.Debug(Category.Z80, "System reset");
            }

            // Release the hounds
            _running = true;
            _sync.Set();
        }

        /// <summary>
        /// Commences running the Z80 system on a new thread.  Use Stop() to stop.
        /// </summary>
        public void RunAsync()
        {
            if (_asyncThread != null)
            {
                throw new InvalidOperationException("Z80 thread is already running; Stop first");
            }

            // Clear the wait handle
            _sync.Set();

            // Fire off the Z80 thread
            _stopAsyncThread = false;
            _asyncThread = new Thread(AsyncThread) { Name = "Z80" };
            _asyncThread.Start();
        }

        /// <summary>
        /// The thread proc for asynchronous Z80 execution.
        /// </summary>
        void AsyncThread()
        {
            // Catch events from the controller
            PERQemu.Controller.RunStateChanged += OnRunStateChange;

            Log.Debug(Category.Controller, "[Z80 running on thread {0}]", Thread.CurrentThread.ManagedThreadId);

            do
            {
                try
                {
                    Run();

                    if (!_sync.IsSet)
                    {
                        _sync.Wait(10);     // don't hang forever...
                    }
                }
                catch (Exception e)
                {
                    _stopAsyncThread = true;
                    _system.Halt(e);
                }
            }
            while (!_stopAsyncThread);

            Log.Debug(Category.Controller, "[Z80 thread stopped]");

            // Detach
            PERQemu.Controller.RunStateChanged -= OnRunStateChange;
        }

        /// <summary>
        /// Stops execution of the Z80 thread, if running.
        /// </summary>
        public void Stop()
        {
            if (_asyncThread == null)
            {
                return;
            }

            Log.Detail(Category.Controller, "[Stop() called on Z80 thread]");
            _stopAsyncThread = true;
            _sync.Set();

            if (!Thread.CurrentThread.Equals(_asyncThread))
            {
                Log.Detail(Category.Controller, "[Z80 thread join called...]");
                while (!_asyncThread.Join(10))
                {
                    Log.Detail(Category.Controller, "[Waiting for Z80 thread to finish...]");
                }
                _asyncThread = null;
                Log.Detail(Category.Controller, "[Z80 thread exited]");
            }
        }

        public void Shutdown()
        {
            // Just in case
            Stop();

            // Any devices that need special attention?
            DeviceShutdown();

            _bus = null;
            _memory = null;
            _cpu = null;

            _scheduler = null;
            _sync.Dispose();

            Log.Detail(Category.Emulator, "Z80System shutdown.");
        }

        void OnRunStateChange(RunStateChangeEventArgs s)
        {
            Log.Debug(Category.Controller, "[Z80 state change event -> {0}]", s.State);
            _stopAsyncThread = (s.State != RunState.Running);
        }

        // FIXME: should move this to PERQsystem or DebugCommands?
        // FIXME: this massively expensive routine has to be rewritten...
        public void ShowZ80State()
        {
            IZ80Registers regs = _cpu.Registers;

            // TODO: should display shadow regs?
            var state = string.Format("Z80 PC=${0:x4} SP=${1:x4} AF=${2:x4} BC=${3:x4} DE=${4:x4} HL=${5:x4} IX=${6:x4} IY=${7:x4}",
                                         regs.PC, regs.SP, regs.AF, regs.BC, regs.DE, regs.HL, regs.IX, regs.IY);
            ushort offset = 0;
            var symbol = _z80Debugger.GetSymbolForAddress(regs.PC, out offset);
            var source = _z80Debugger.GetSourceLineForAddress(regs.PC);

            // Log the state
            Log.Debug(Category.Z80Inst, "{0}", state);

            // Write the whole thing
            Console.WriteLine("{0}\n\t{1}+0x{2:x} : {3}", state, symbol, offset, source);
        }

        /// <summary>
        /// Load the ROM code for this IO Board.
        /// </summary>
        public void LoadZ80ROM(string file)
        {
            try
            {
                _memory.LoadROM(Paths.BuildPROMPath(file));
            }
            catch
            {
                Log.Error(Category.Emulator, "Could not open Z80 ROM from {0}!", Paths.Canonicalize(file));
                throw;
            }
        }

        //
        // Common elements to all IO boards
        //
        protected Z80Processor _cpu;
        protected Z80MemoryBus _memory;
        protected Z80IOBus _bus;

        protected NECuPD765A _fdc;
        protected TMS9914A _tms9914a;

        protected Z80Debugger _z80Debugger;
        protected Scheduler _scheduler;
        protected PERQSystem _system;

        //
        // Plumbing
        //
        protected ManualResetEventSlim _sync;
        protected long _wakeup;

        Thread _asyncThread;

        volatile protected bool _running;
        volatile bool _stopAsyncThread;
    }
}
