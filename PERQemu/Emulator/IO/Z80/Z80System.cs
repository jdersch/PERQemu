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

using PERQemu.Processor;
using PERQemu.Debugger;
using PERQemu.IO;
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

            // todo: assign ports/base addresses of each peripheral chip
            // based on the configured IO Board type.

            _perqToZ80Fifo = new PERQToZ80FIFO(system);
            _z80ToPerqFifo = new Z80ToPERQFIFO(system);
            _fdc = new NECuPD765A(_scheduler);
            _z80ctc = new Z80CTC(0x90, _scheduler);
            _z80sio = new Z80SIO(0xb0, _scheduler);
            _z80dma = new Z80DMA(0x98, _memory, _bus);
            _dmaRouter = new DMARouter(this);
            _tms9914a = new TMS9914A();
            _keyboard = new Keyboard();
            _seekControl = new HardDiskSeekControl(system);
            _ioReg1 = new IOReg1(_z80ToPerqFifo);
            _ioReg3 = new IOReg3(_perqToZ80Fifo, _keyboard, _fdc, _dmaRouter);

            _z80dma.AttachDeviceA(_dmaRouter);
            _z80dma.AttachDeviceB(_memory);

            // Put devices on the bus
            _bus.RegisterDevice(_fdc);
            _bus.RegisterDevice(_perqToZ80Fifo);
            _bus.RegisterDevice(_z80ToPerqFifo);
            _bus.RegisterDevice(_z80ctc);
            _bus.RegisterDevice(_z80dma);
            _bus.RegisterDevice(_seekControl);
            _bus.RegisterDevice(_keyboard);
            _bus.RegisterDevice(_z80sio);
            _bus.RegisterDevice(_tms9914a);
            _bus.RegisterDevice(_ioReg1);
            _bus.RegisterDevice(_ioReg3);

            // Attach peripherals

            // todo: consult the configuration record to decide which
            // tablet (or both) to attach
            // todo: finish the gpib and attach the bitpad if selected
            // todo: serial port "b"
            // todo: rtc chip

            KrizTablet tablet = new KrizTablet(_scheduler, system);
            _z80sio.AttachDevice(1, tablet);

            _z80Debugger = new Z80Debugger();

            // Do 10 rate adjustments / second?  Balance overhead v. accuracy
            _heartbeat = new SystemTimer(20f);

            // Compute how often (in CPU cycles) to sync the emulated processor
            // to real-time (used if Settings.RateLimit != Fast mode)
            _adjustInterval = (int)(_heartbeat.Interval / (IOBoard.Z80CycleTime * Conversion.NsecToMsec));
            Log.Info(Category.Emulator, "[Z80 rate adjust every {0} cycles]", _adjustInterval);
        }

        public bool SupportsAsync => true;
        public bool IsRunning => _running;
        public ulong Clocks => _cpu.TStatesElapsedSinceReset;

        public Z80Processor CPU => _cpu;
        public Scheduler Scheduler => _scheduler;
        public Keyboard Keyboard => _keyboard;

        // DMA Capable devices
        public NECuPD765A FDC => _fdc;
        public PERQToZ80FIFO PERQReadFIFO => _perqToZ80Fifo;
        public Z80ToPERQFIFO PERQWriteFIFO => _z80ToPerqFifo;
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
            bool cpr = _heartbeat.IsEnabled;

            _heartbeat.Reset();
            _scheduler.Reset();
            _cpu.Reset();

            if (soft)
            {
                // During a "soft" reset (where the PERQ initiates through a
                // control register) the hardware only actually resets these:
                _z80ctc.Reset();
                _z80sio.Reset();
                _tms9914a.Reset();

                // If our heartbeat stopped, restart it (relevant only in
                // asynch mode!)
                if (cpr)
                {
                    _heartbeat.Enable(true);
                }

                Log.Debug(Category.Z80, "System (soft) reset");
            }
            else
            {
                // A power-on or "hard" reset does everything
                _bus.Reset();
                Log.Debug(Category.Z80, "System reset");
            }

            _running = true;
            //_scheduler.DumpEvents("Z80");
        }

        /// <summary>
        /// Runs the Z80 for one instruction (either mode).  If the Z80 is
        /// "turned off" by the PERQ, it's effectively a no-op.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public uint Run(int clocks = 1)
        {
            var ticks = 0;  // How many actual clock ticks elapsed

            do
            {
                if (_running)
                {
                    ticks = _cpu.ExecuteNextInstruction();
                    _z80dma.Execute();
                    _scheduler.Clock(ticks);
                    clocks -= ticks;
                }
                else
                {
                    ticks += 1;
                    clocks -= 1;
                }
            } while (clocks > 0);

            return (uint)ticks;
        }

        /// <summary>
        /// Commences running the Z80 system on a new thread.  Use Stop() to stop.
        /// </summary>
        public void RunAsync()
        {
            if (_asyncThread != null)
            {
                throw new InvalidOperationException("Z80 thread is already running; Stop first.");
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

            _heartbeat.Enable(true);
            Console.WriteLine("[Z80 thread starting]");

            do
            {
                try
                {
                    Run(_adjustInterval);

                    if (_stopAsyncThread) break;

                    // Should probably always rate limit the Z80 for device timings to work
                    if (Settings.Performance.HasFlag(RateLimit.AccurateCPUSpeedEmulation))
                    {
                        _heartbeat.WaitForHeartbeat();
                    }
                }
                catch (Exception e)
                {
                    _system.Halt(e);
                }
            }
            while (!_stopAsyncThread);

            _heartbeat.Enable(false);
            Console.WriteLine("[Z80 thread stopped]");

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

            Console.WriteLine("[Stop() Z80 thread]");
            _stopAsyncThread = true;

            if (Thread.CurrentThread != _asyncThread)
            {
                Console.WriteLine("[Z80 thread join called on {0}...]", Thread.CurrentThread.ManagedThreadId);
                //_asyncThread.Join();
                _asyncThread = null;
                Console.WriteLine("[Z80 thread exited]");
           }
        }

        private void OnRunStateChange(RunStateChangeEventArgs s)
        {
            Console.WriteLine("[Z80 state change event -> {0}]", s.State);
            _stopAsyncThread = (s.State != RunState.Running);
        }

        /// <summary>
        /// Sends data to the Z80.
        /// Corresponds to IOB port 0xc7 (octal 307).   TODO: ports have to be configurable
        ///
        ///  The IOB schematic (p. 49 of the PDF, "IOA DECODE") sheds some light on the Z80 "input"
        ///  interrupt.  This is actually labeled as "Z80 READY INT L" (meaning it's active Low)
        ///  and seems to be enabled by the PERQ sending data with bit 8 high, and can be dismissed by
        ///  the PERQ sending data with bit 8 low.
        ///
        ///  IOD 8 is latched on a WRITE from the Z80 and gated with the PERQ->Z80 REQ L signal at a
        ///  NAND gate.  So -- if the PERQ sets IOD 8 high with a write, and there is no pending PERQ->
        ///  Z80 request, Z80 READY INT L will be low (triggered).
        /// </summary>
        public void WriteData(int data)
        {
            //
            // The 8th bit of the incoming data to this port is latched.
            // When set, the PERQ is requesting that the Z80 send a DataInReady interrupt when it is ready.
            // When cleared, the Z80 will clear any pending DataInReady interrupt.
            //
            if ((data & 0x100) == 0)
            {
                Log.Debug(Category.Z80IRQ, "DataInReady disabled, clearing interrupt");

                // TODO: move this logic into PERQToZ80FIFO?
                _system.CPU.ClearInterrupt(InterruptSource.Z80DataIn);
                _perqToZ80Fifo.SetDataReadyInterruptRequested(false);
            }
            else
            {
                Log.Debug(Category.Z80IRQ, "DataInReady enabled");

                _perqToZ80Fifo.SetDataReadyInterruptRequested(true);
            }

            // Queue up the byte 
            _perqToZ80Fifo.Enqueue((byte)data);
        }

        public int ReadData()
        {
            return _z80ToPerqFifo.Dequeue();
        }

        /// <summary>
        /// Corresponds to IOB port 0xc1 (octal 301).
        /// The PERQ1 microcode uses port c1 to control both the hard drive and the Z80.
        /// The lower 5 bits are hard disk control flags.
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

        public void SetSerialPort(ISerialDevice dev) { }

        public string GetSerialPort() { return string.Empty; }

        // todo:  move this into Z80DebugCommands?
        //[DebugFunction("show z80 registers", "Displays the values of the Z80 registers")]
        public void ShowZ80State()
        {
            IZ80Registers regs = _cpu.Registers;

            // TODO: should display shadow regs?
            Console.WriteLine("Z80 PC=${0:x4} SP=${1:x4} AF=${2:x4} BC=${3:x4} DE=${4:x4} HL=${5:x4}",
                              regs.PC, regs.SP, regs.AF, regs.BC, regs.DE, regs.HL);
            Console.WriteLine("    IX=${0:x4} IY=${1:x4}", regs.IX, regs.IY);

            // TODO: this doesn't really belong here
            ushort offset = 0;
            string symbol = _z80Debugger.GetSymbolForAddress(regs.PC, out offset);
            string source = _z80Debugger.GetSourceLineForAddress(regs.PC);

            Console.WriteLine();
            Console.WriteLine("{0}+0x{1:x} : {2}", symbol, offset, source);
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
                Log.Error(Category.Emulator, "Could not open Z80 ROM from {0}!", file);
                throw;
            }
        }

        //
        // Z80System is a big sucka!
        //
        private Z80Processor _cpu;
        private Z80MemoryBus _memory;
        private Z80IOBus _bus;
        private IOReg1 _ioReg1;
        private IOReg3 _ioReg3;
        private Z80SIO _z80sio;
        private Z80CTC _z80ctc;
        private Z80DMA _z80dma;
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
        private SystemTimer _heartbeat;
        private int _adjustInterval;

        private volatile bool _running;
        private volatile bool _stopAsyncThread;
    }
}
