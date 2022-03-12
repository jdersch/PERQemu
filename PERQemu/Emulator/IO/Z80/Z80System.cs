//
// Z80System.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.CompilerServices;

using Konamiman.Z80dotNet;

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.IO.GPIB;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The Z80System is an embedded system that runs the PERQ's "low speed"
    /// peripherals.  It has its own ROM, RAM, DMA and peripheral bus and runs
    /// independently of the main CPU.  This class directly implements both the
    /// "old" Z80 (PERQ-1 IOB or CIO boards) and the "new" Z80 (PERQ-2 EIO or
    /// NIO boards), as the differences are not yet perceived to warrant any
    /// kind of subclassing, he said, blissfully ignorant of the world of pain
    /// he was about to enter.
    /// </summary>
    public class Z80System
    {
        public Z80System(PERQSystem system)
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

            // todo: assign ports/base addresses of each peripheral chip or latch
            // based on the configured IO Board type.

            _seekControl = new HardDiskSeekControl(system);
            _perqToZ80Fifo = new PERQToZ80FIFO(system);
            _z80ToPerqFifo = new Z80ToPERQFIFO(system);
            _z80ctc = new Z80CTC(0x90, _scheduler);
            _z80sio = new Z80SIO(0xb0, _scheduler);
            _z80dma = new Z80DMA(0x98, _memory, _bus);
            _dmaRouter = new DMARouter(this);
            _fdc = new NECuPD765A(0xa8, _scheduler);
            _tms9914a = new TMS9914A(0xb8);
            _keyboard = new Keyboard();
            _ioReg3 = new IOReg3(_perqToZ80Fifo, _keyboard, _fdc, _dmaRouter);

            _z80dma.AttachDeviceA(_dmaRouter);
            _z80dma.AttachDeviceB(_memory);

            // Put devices on the bus
            _bus.RegisterDevice(_seekControl);
            _bus.RegisterDevice(_perqToZ80Fifo);
            _bus.RegisterDevice(_z80ToPerqFifo);
            _bus.RegisterDevice(_z80ctc);
            _bus.RegisterDevice(_z80sio);
            _bus.RegisterDevice(_z80dma);
            _bus.RegisterDevice(_fdc);
            _bus.RegisterDevice(_tms9914a);
            _bus.RegisterDevice(_keyboard);
            _bus.RegisterDevice(_ioReg3);

            // If this is an EIO we need:
            // todo: serial port "b"
            // todo: rtc chip

            // Attach the configured tablet(s)
            if (system.Config.Tablet.HasFlag(TabletType.BitPad))
            {
                BitPadOne tablet = new BitPadOne(_scheduler, system);
                _tms9914a.Bus.AddDevice(tablet);
            }

            if (system.Config.Tablet.HasFlag(TabletType.Kriz))
            {
                KrizTablet tablet = new KrizTablet(_scheduler, system);
                _z80sio.AttachDevice(1, tablet);
            }

            // Attach our debugger
            _z80Debugger = new Z80Debugger();
        }

        public bool SupportsAsync => true;
        public bool IsRunning => _running;
        public ulong Clocks => _cpu.TStatesElapsedSinceReset;

        public Z80Processor CPU => _cpu;
        public Scheduler Scheduler => _scheduler;
        public Keyboard Keyboard => _keyboard;

        // DMA Capable devices
        public NECuPD765A FDC => _fdc;
        public Z80SIO SIOA => _z80sio;
        public TMS9914A GPIB => _tms9914a;

        // Allow for external CTC triggers
        public Z80CTC CTC => _z80ctc;

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
                // During a "soft" reset (where the PERQ initiates through a
                // control register) the hardware only actually resets these:
                _fdc.Reset();
                _z80ctc.Reset();
                _z80sio.Reset();
                _tms9914a.Reset();

                // This appears to be necessary even if not technically correct
                _z80ToPerqFifo.Reset();
                _perqToZ80Fifo.Reset();
                _seekControl.Reset();

                Log.Debug(Category.Z80, "System (soft) reset");
            }
            else
            {
                // A power-on or "hard" reset does everything
                _bus.Reset();
                Log.Debug(Category.Z80, "System reset");
            }

            _running = true;
        }

        /// <summary>
        /// Runs the Z80 for one instruction (either mode).  If the Z80 is
        /// "turned off" by the PERQ, it's effectively a no-op.
        /// </summary>
        /// <remarks>
        /// The Z80 syncs itself to the main processor by comparing Scheduler
        /// time stamps; if the Z80 is behind the PERQ, it runs instructions
        /// until it catches up/exceeds the main CPU, then pauses/no-ops until
        /// it falls behind again.  In this way the Z80 always stays within a
        /// few microseconds (ahead or behind) and the crude/chunky "heartbeat"
        /// timer is eliminated.  This should better regulate the exchange of
        /// data through the FIFOs and resolve some annoying timing difficulties.
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void Run(int clocks = 1)
        {
            IZ80Registers regs = _cpu.Registers;

            do
            {
                if (_running)
                {
                    var ticks = 0;

                    // Is the master CPU clock ahead of us?
                    var diff = (long)(_system.Scheduler.CurrentTimeNsec - _scheduler.CurrentTimeNsec);

                    // Deal with the INIR special condition :-/
                    if (_memory[regs.PC] == 0xed && _memory[regs.PC + 1] == 0xb2 && !_perqToZ80Fifo.IsReady)
                    {
                        // This is a cheap way to inject a wait state; this
                        // emulates the blocking read of the PERQR latch
                        diff = 0;

                        Log.Detail(Category.FIFO, "Wait state for FIFO read (INIR)");
                    }

                    if (diff > 0)
                    {
#if DEBUG
                        // for now: debugging; future: actual InterruptEncoder as a bus device?
                        _bus.ActiveInterrupts();

                        // this is hugely expensive so only call it if selected
                        if (Log.Categories.HasFlag(Category.Z80Inst)) ShowZ80State();
#endif
                        // Yes!  Run an instruction
                        ticks = _cpu.ExecuteNextInstruction();

                        // And a DMA cycle
                        _z80dma.Clock();

                        // Run the scheduler
                        _scheduler.Clock(ticks);
                    }
                    else
                    {
                        // We really need a nanosleep here... a typical Z80 instruction
                        // at 2.4576Mhz might take 3-9usec, or up to 52ish CPU uinsts.
                        ticks = 1;
                    }

                    clocks -= ticks;
                }
                else
                {
                    clocks -= 1;
                }
            } while (clocks > 0);
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

            // Fire off the Z80 thread
            _stopAsyncThread = false;
            _asyncThread = new Thread(AsyncThread) { Name = "Z80" };
            _asyncThread.Start();
        }

        /// <summary>
        /// The thread proc for asynchronous Z80 execution.
        /// </summary>
        private void AsyncThread()
        {
            // Catch events from the controller
            PERQemu.Controller.RunStateChanged += OnRunStateChange;

            Log.Debug(Category.Controller, "[Z80 running on thread {0}]", Thread.CurrentThread.ManagedThreadId);

            do
            {
                try
                {
                    Run();

                    if (_stopAsyncThread) break;
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

            Log.Debug(Category.Controller, "[Stop() called on Z80 thread]");
            _stopAsyncThread = true;

            if (!Thread.CurrentThread.Equals(_asyncThread))
            {
                Log.Debug(Category.Controller, "[Z80 thread join called...]");
                while (!_asyncThread.Join(10))
                {
                    Log.Debug(Category.Controller, "[Waiting for Z80 thread to finish...]");
                }
                _asyncThread = null;
                Log.Debug(Category.Controller, "[Z80 thread exited]");
            }
        }

        private void OnRunStateChange(RunStateChangeEventArgs s)
        {
            Log.Debug(Category.Controller, "[Z80 state change event -> {0}]", s.State);
            _stopAsyncThread = (s.State != RunState.Running);
        }

        /// <summary>
        /// Sends data to the Z80.  Corresponds to IOB port 0xc7 (octal 307).
        /// Note that this is a 9-bit value, with the high bit passed to the
        /// FIFO to indicate whether or not the READY INT L interrupt should be
        /// raised when the FIFO is clear -- see PERQtoZ80Fifo for details.
        /// </summary>
        public void WriteData(int data)
        {
            _perqToZ80Fifo.Enqueue(data & 0x01ff);      // Include IOD<8>
        }

        /// <summary>
        /// Reads data from the Z80.  Coresponds to IOB port 0x46 (octal 106).
        /// </summary>
        public int ReadData()
        {
            // Some reads are issued specifically to make sure the buffer is
            // flushed, so the occasional "read from empty fifo" warning is okay
            return _z80ToPerqFifo.Dequeue();
        }

        /// <summary>
        /// Corresponds to IOB port 0xc1 (octal 301).
        /// The PERQ1 microcode uses port c1 to control both the hard drive and
        /// the Z80.  The lower 7 bits are hard disk control flags.
        ///
        /// From sysb.mic:
        /// !
        /// ! Turn Z80 Off
        /// Z80Off: 200, IOB(301), loc(7500);         ! shut off disk and Z80
        ///         0, IOB(307), loc(7501);           ! shut off Z80 output interrupts
        ///         IOB(106), loc(7502);              ! dismiss Z80 input interrupt
        ///         IOB(147), return, loc(7503);      ! dismiss memory parity interrupt
        ///
        /// Elsewhere:
        /// KeyRetry: 0, IOB(301);                    ! turn Z80 on
        ///         Call(WaitAWhile);                 ! Let it start up
        ///
        /// From this I am assuming that if the Z80 is "off", writing "0" to the Status register
        /// starts it up, and if it is "on" then writing 0x80 to it turns it off again.  NOTE in
        /// particular the order of those instructions: we still have to look at the on/off flag
        /// even if the Z80 isn't "running"!
        /// </summary>
        public void WriteStatus(int status)
        {
            if (status == 0x80 && _running)
            {
                Log.Debug(Category.Z80, "Shut down by write to Status register");
                _running = false;
            }
            else if (status == 0 && !_running)
            {
                Log.Debug(Category.Z80, "Started by write to Status register");
                Reset(true);
            }
        }

        // todo: these don't really belong here
        public void SetSerialPort(ISerialDevice dev) { }
        public string GetSerialPort() { return string.Empty; }

        public void ShowZ80State()
        {
            IZ80Registers regs = _cpu.Registers;

            // TODO: should display shadow regs?
            Log.Debug(Category.Z80Inst, "Z80 PC=${0:x4} SP=${1:x4} AF=${2:x4} BC=${3:x4} DE=${4:x4} HL=${5:x4}",
                      regs.PC, regs.SP, regs.AF, regs.BC, regs.DE, regs.HL);
            Log.Debug(Category.Z80Inst, "    IX=${0:x4} IY=${1:x4}", regs.IX, regs.IY);

            // TODO: this doesn't really belong here
            ushort offset = 0;
            string symbol = _z80Debugger.GetSymbolForAddress(regs.PC, out offset);
            string source = _z80Debugger.GetSourceLineForAddress(regs.PC);

            Log.Debug(Category.Z80Inst, "{0}+0x{1:x} : {2}", symbol, offset, source);
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
                Log.Error(Category.Emulator, "Could not open Z80 ROM from {0}!",
                          Paths.Canonicalize(file));
                throw;
            }
        }


        private Z80Processor _cpu;
        private Z80MemoryBus _memory;
        private Z80IOBus _bus;
        private Z80SIO _z80sio;
        private Z80CTC _z80ctc;
        private Z80DMA _z80dma;
        private IOReg3 _ioReg3;
        private DMARouter _dmaRouter;
        private PERQToZ80FIFO _perqToZ80Fifo;
        private Z80ToPERQFIFO _z80ToPerqFifo;
        private HardDiskSeekControl _seekControl;
        private NECuPD765A _fdc;
        private TMS9914A _tms9914a;

        private Keyboard _keyboard;

        private Z80Debugger _z80Debugger;
        private Scheduler _scheduler;
        private PERQSystem _system;

        private Thread _asyncThread;

        private volatile bool _running;
        private volatile bool _stopAsyncThread;
    }
}
