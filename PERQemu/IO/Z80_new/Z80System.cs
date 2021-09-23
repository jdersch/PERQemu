// Z80system.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using Konamiman.Z80dotNet;
using PERQemu.CPU;
using PERQemu.Debugger;
using PERQemu.IO.SerialDevices;
using System;
using System.Collections.Generic;
using System.Threading;

namespace PERQemu.IO.Z80_new
{
    public class Z80System : IZ80System
    {
        public Z80System(PERQSystem system)
        {
            _system = system;
            _scheduler = new Scheduler(407);           // IOB Z80 clock runs at 2.4576Mhz, ~407nSec per clock tick.

            _bus = new IOBIOBus(this);
            _memory = new IOBMemoryBus();
            _cpu = new Z80Processor();
            _cpu.Memory = _memory;
            _cpu.PortsSpace = _bus;

            _fdc = new NECuPD765A(_scheduler);
            _perqToZ80Fifo = new PERQToZ80FIFO(system);
            _z80ToPerqFifo = new Z80ToPERQFIFO(system);
            _z80ctc = new Z80CTC(0x90, _scheduler);
            _z80dma = new Z80DMA(0x98, _memory, _bus);
            _seekControl = new HardDiskSeekControl(system);
            _keyboard = new Keyboard();
            _z80sio = new Z80SIO(0xb0, _scheduler);
            _tms9914a = new TMS9914A();
            _dmaRouter = new DMARouter(this);
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
            KrizTablet tablet = new KrizTablet(_scheduler, system);
            _z80sio.AttachDevice(1, tablet);
            
            _floppyDrive = new FloppyDrive();
            _fdc.AttachDrive(0, _floppyDrive);

            _z80Debugger = new Z80Debugger();
            _running = false;
        }

        public void Reset()
        {
            _scheduler.Reset();
            _cpu.Reset();
            _bus.Reset();
        }

        public bool SupportsAsync => true;

        [DebugFunction("show z80 registers", "Displays the values of the Z80 registers")]
        public void ShowZ80State()
        {
            IZ80Registers regs = _cpu.Registers;

            // TODO: should display shadow regs?
            Console.WriteLine("Z80 PC=${0:x4} SP=${1:x4} AF=${2:x4} BC=${3:x4} DE=${4:x4} HL=${5:x4}", regs.PC, regs.SP, regs.AF, regs.BC, regs.DE, regs.HL);
            Console.WriteLine("    IX=${0:x4} IY=${1:x4}", regs.IX, regs.IY);

            // TODO: this doesn't really belong here
            ushort offset = 0;
            string symbol = _z80Debugger.GetSymbolForAddress(regs.PC, out offset);
            string source = _z80Debugger.GetSourceLineForAddress(regs.PC);

            Console.WriteLine();
            Console.WriteLine("{0}+0x{1:x} : {2}", symbol, offset, source);
        }

        /// <summary>
        /// Runs the Z80 for one instruction.
        /// </summary>
        /// <returns></returns>
        public uint SingleStep()
        {
            if (_asyncThread != null)
            {
                throw new InvalidOperationException("Cannot single-step while Z80 is running asynchronously; Stop first.");
            }

            _lastExecutionMode = ExecutionMode.Synchronous;

            if (_running)
            {
                uint clocks = (uint)_cpu.ExecuteNextInstruction();
                _z80dma.Execute();

                // This is TERRIBLE
                for (uint i = 0; i < clocks; i++)
                {
                    _scheduler.Clock();
                }

                return clocks;
            }
            else
            {
                return 4; // nice round fudged number to fill time
            }
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

            _lastExecutionMode = ExecutionMode.Asynchronous;

            // Do not start the thread if the Z80 has been turned off.
            if (!_running)
            {
                return;
            }

            _stopAsyncExecution = false;
            _asyncThread = new Thread(AsyncThread);
            _asyncThread.Start();
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

            // Tell the thread to exit.
            _stopAsyncExecution = true;

            // Wait.
            _asyncThread.Join();
            _asyncThread = null;
        }

        /// <summary>
        /// The thread proc for asynchronous Z80 execution.
        /// </summary>
        private void AsyncThread()
        {
            while (_running && !_stopAsyncExecution)
            {
                int clocks = _cpu.ExecuteNextInstruction();
                _z80dma.Execute();

                // This is *STILL* TERRIBLE
                for (uint i = 0; i < clocks; i++)
                {
                    _scheduler.Clock();
                }
            }
        }

        private Thread _asyncThread;
        private volatile bool _stopAsyncExecution;

        /// <summary>
        /// Sends data to the Z80.
        /// Corresponds to IOB port 0xc7 (octal 307).
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
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 DataInReady interrupt disabled, clearing interrupt.");
#endif
                // TODO: move this logic into PERQToZ80FIFO?
                _system.CPU.ClearInterrupt(InterruptType.Z80DataInReady);
                _perqToZ80Fifo.SetDataReadyInterruptRequested(false);
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 DataInReady interrupt enabled.");
#endif
                _perqToZ80Fifo.SetDataReadyInterruptRequested(true);
            }

            // Only queue up this write if we're actually running.
            if (_running)
            {
                _perqToZ80Fifo.Enqueue((byte)data);
            }
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
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 system shut down by write to Status register.");
#endif
                _running = false;
                Stop();
            }
            else if (status == 0 && !_running)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 system started by write to Status register.");
#endif
                _running = true;
                Reset();

                if (_lastExecutionMode == ExecutionMode.Asynchronous)
                {
                    // Restart execution thread
                    RunAsync();
                }
            }
        }
        
        public void LoadFloppyDisk(string path) 
        {
            _floppyDrive.LoadDisk(new PhysicalDisk.FloppyDisk(path));
        }

        public void SaveFloppyDisk(string path) 
        {
            _floppyDrive.Disk.Save(path);
        }

        public void UnloadFloppyDisk() 
        {
            _floppyDrive.UnloadDisk();
        }

        public void SetSerialPort(ISerialDevice dev) { }

        public string GetSerialPort() { return String.Empty; }

        public Z80Processor CPU => _cpu;

        public Queue<byte> FIFO { get; }

        public IKeyboard Keyboard => _keyboard;

        // DMA Capable devices
        public NECuPD765A FDC => _fdc;
        public PERQToZ80FIFO PERQReadFIFO => _perqToZ80Fifo;

        public Z80ToPERQFIFO PERQWriteFIFO => _z80ToPerqFifo;

        public Z80SIO SIOA => _z80sio;

        public TMS9914A GPIB => _tms9914a;

        private PERQSystem _system;
        private Z80Processor _cpu;
        private IOBIOBus _bus;
        private IOBMemoryBus _memory;
        private IOReg1 _ioReg1;
        private IOReg3 _ioReg3;
        private NECuPD765A _fdc;
        private PERQToZ80FIFO _perqToZ80Fifo;
        private Z80ToPERQFIFO _z80ToPerqFifo;
        private Z80CTC _z80ctc;
        private Z80DMA _z80dma;
        private HardDiskSeekControl _seekControl;
        private Keyboard _keyboard;
        private Z80SIO _z80sio;
        private TMS9914A _tms9914a;
        private DMARouter _dmaRouter;

        private FloppyDrive _floppyDrive;

        private Scheduler _scheduler;

        private Z80Debugger _z80Debugger;

        private volatile bool _running;

        private ExecutionMode _lastExecutionMode;
    }
}
