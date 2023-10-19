//
// CIOZ80.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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
using System.Runtime.CompilerServices;

using Konamiman.Z80dotNet;

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.IO.GPIB;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The Z80 subsystem for the PERQ-1.  Implements the original IOB (running
    /// the old firmware) and the CIO (new firmware).
    /// </summary>
    public sealed class CIOZ80 : Z80System
    {
        public CIOZ80(PERQSystem system) : base(system)
        {
            // Set up the IOB/CIO peripherals
            _fdc = new NECuPD765A(0xa8, _scheduler);
            _tms9914a = new TMS9914A(0xb8);
            _seekControl = new HardDiskSeekControl(_system);
            _perqToZ80Fifo = new PERQToZ80Latch(_system);
            _z80ToPerqFifo = new Z80ToPERQLatch(_system);
            _z80ctc = new Z80CTC(0x90, _scheduler);
            _z80sio = new Z80SIO(0xb0, _scheduler);
            _z80dma = new Z80DMA(0x98, _memory, _bus);
            _dmaRouter = new DMARouter(this);
            _keyboard = new Keyboard();
            _ioReg3 = new IOReg3(_perqToZ80Fifo, _keyboard, _fdc, _dmaRouter);

            // Initialize DMAC
            _z80dma.AttachDeviceA(_dmaRouter);
            _z80dma.AttachDeviceB(_memory);

            // Attach our debugger
            if (system.Config.IOBoard == IOBoardType.IOB)
            {
                _z80Debugger = new Z80Debugger("oioz80.lst");
            }
            else
            {
                _z80Debugger = new Z80Debugger("cioz80.lst");
            }

            DeviceInit();
        }

        // Allow for external CTC triggers (disk seeks)
        public override Z80CTC CTC => _z80ctc;

        // Expose to the DMA router
        public override Z80SIO SIOA => _z80sio;

        // Do we need a wait state?
        protected override bool FIFOInputReady => _perqToZ80Fifo.IsReady;
        protected override bool FIFOOutputReady => _z80ToPerqFifo.IsReady;

        /// <summary>
        /// Initializes the IOB/CIO devices and attaches them to the bus.
        /// </summary>
        void DeviceInit()
        {
            // Attach the configured tablet(s)
            if (_system.Config.Tablet.HasFlag(TabletType.BitPad))
            {
                var tablet = new BitPadOne(_scheduler, _system);
                _tms9914a.Bus.AddDevice(tablet);
            }

            if (_system.Config.Tablet.HasFlag(TabletType.Kriz))
            {
                var tablet = new KrizTablet(_scheduler, _system);
                _z80sio.AttachDevice(1, tablet);
            }

            // If enabled and configured, attach device to RS232
            if (_system.Config.RSAEnable && Settings.RSADevice != string.Empty)
            {
                if (Settings.RSADevice == "RSX:")
                {
                    var rsx = new RSXFilePort(this);
                    _z80sio.AttachPortDevice(0, rsx);
                    _z80ctc.AttachDevice(0, rsx);
                }
                else
                {
                    var rsa = new PhysicalPort(this, Settings.RSADevice, Settings.RSASettings, "A");
                    _z80sio.AttachPortDevice(0, rsa);
                    _z80ctc.AttachDevice(0, rsa);
                }
            }
            else
            {
                // Otherwise direct it to the bit bucket
                _z80sio.AttachPortDevice(0, new NullPort(this));
            }

            // Everybody get on the bus!
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
        }

        protected override void DeviceReset()
        {
            // During a "soft" reset (initiated by the PERQ through a
            // control register) the hardware only actually resets these:
            _fdc.Reset();
            _z80ctc.Reset();
            _z80sio.Reset();
            _tms9914a.Reset();

            // This appears to be necessary even if not technically correct:
            _z80ToPerqFifo.Reset();
            _perqToZ80Fifo.Reset();
            _seekControl.Reset();
        }

        protected override void DeviceShutdown()
        {
            // If a serial device is attached, close it properly
            _z80sio.DetachDevice(0);    // RS232-A
        }

        /// <summary>
        /// Corresponds to IOB port 0xc1 (octal 301).
        /// The PERQ-1 microcode uses port c1 to control both the hard drive and
        /// the Z80.  The lower 7 bits are hard disk control flags.
        /// </summary>
        /// <remarks>
        /// From sysb.mic:
        /// 
        /// ! Turn Z80 Off
        /// Z80Off: 200, IOB(301), loc(7500);       ! shut off disk and Z80
        ///         0, IOB(307), loc(7501);         ! shut off Z80 output interrupts
        ///         IOB(106), loc(7502);            ! dismiss Z80 input interrupt
        ///         IOB(147), return, loc(7503);    ! dismiss memory parity interrupt
        ///
        /// Elsewhere:
        /// KeyRetry: 0, IOB(301);                  ! turn Z80 on
        ///         Call(WaitAWhile);               ! Let it start up
        ///
        /// From this I am assuming that if the Z80 is "off", writing "0" to the
        /// Status register starts it up, and if it is "on" then writing 0x80 to
        /// it turns it off again.  NOTE the particular order of those instructions:
        /// we still have to look at the on/off flag even if the Z80 isn't "running"!
        /// </remarks>
        public override void WriteStatus(int status)
        {
            bool prevState = _running;

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

            if (_running != prevState)
            {
                _system.MachineStateChange(WhatChanged.Z80RunState, _running);
            }
        }

        /// <summary>
        /// Reads the Z80 status register.  This isn't used by the IOB or CIO.
        /// </summary>
        public override int ReadStatus()
        {
            return 0;
        }

        /// <summary>
        /// Sends data to the Z80.  Corresponds to IOB port 0xc7 (octal 307).
        /// Note that this is a 9-bit value, with the high bit passed to the
        /// FIFO to indicate whether or not the READY INT L interrupt should be
        /// raised when the FIFO is clear -- see PERQtoZ80Fifo for details.
        /// </summary>
        public override void WriteData(int data)
        {
            _perqToZ80Fifo.Enqueue(data & 0x01ff);      // Include IOD<8>
        }

        /// <summary>
        /// Reads data from the Z80.  Coresponds to IOB port 0x46 (octal 106).
        /// </summary>
        public override int ReadData()
        {
            // Some reads are issued specifically to make sure the buffer is
            // flushed, so the occasional "read from empty fifo" warning is okay
            return _z80ToPerqFifo.Dequeue();
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
        /// timer is eliminated.  This helps regulate the exchange of data
        /// through the FIFOs and resolves some annoying timing difficulties.
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public override void Run()
        {
            // Is this thing on?
            if (!_running)
            {
                if (_system.Mode == ExecutionMode.Asynchronous)
                {
                    // Pause the thread until the Z80 is turned back on
                    _wakeup = long.MaxValue;
                    _sync.Reset();
                }
                return;
            }

            // Is the master CPU clock ahead of us?
            var diff = (long)(_system.Scheduler.CurrentTimeNsec - _scheduler.CurrentTimeNsec);

            if (diff <= 0)
            {
                if (_system.Mode == ExecutionMode.Asynchronous)
                {
                    // If we are less than one full microcycle ahead of the CPU,
                    // just spin; otherwise, block (when we return).  The PERQ
                    // will automatically wake us when it catches up.  This lets
                    // the Z80 thread actually sleep while the CPU churns through
                    // the ~9-55 Z80 clock cycles (3.6-22.4usec, or about 21-132
                    // PERQ microcycles on average) needed to catch up.  A poor
                    // man's "nanosleep()"...
                    diff = -diff;

                    if ((ulong)diff > _system.Scheduler.TimeStepNsec)
                    {
                        // Pause the thread
                        _sync.Reset();
                    }
                }
                return;
            }

            // Run an instruction!
            IZ80Registers regs = _cpu.Registers;

#if DEBUG
            // For now: debugging; future: actual InterruptEncoder as a bus device?
            _bus.ActiveInterrupts();

            // This is hugely expensive so only call it if selected
            if (Log.Categories.HasFlag(Category.Z80Inst)) ShowZ80State();
#endif
            //
            // The IOB/CIO hardware will pull the Z80's WAIT line low if access
            // to the two FIFO buffers is attempted while the contents are not
            // valid; this is true on IN or OUT instructions, as well as looping
            // variants INIR (in both IOB and CIO firmware) and OTIR (used only
            // by the CIO version).  There's no useful or obvious way to leverage
            // Z80dotNet's "Before" memory access delegates to inject a wait state
            // so this grody hack peeks ahead in the instruction stream to locate
            // IN/OUT or INIR/OUTIR accesses to the FIFOs and check their IsReady
            // properties, aborting (delaying) the execution until the microcode
            // has read/written data to clear the condition.  Sigh.
            //
            var peek = (ushort)(_memory[regs.PC] << 8 | _memory[regs.PC + 1]);

            if (peek == 0xdba0 || peek == 0xedb2)
            {
                if (!FIFOInputReady)
                {
                    Log.Debug(Category.FIFO, "Wait state for FIFO op ({0})",
                                              (peek == 0xdba0) ? "IN" : "INIR");
                    return;
                }
            }
            else if (peek == 0xd3d0 || (peek == 0xedb3 && regs.C == 0xd0))
            {
                if (!FIFOOutputReady)
                {
                    Log.Debug(Category.FIFO, "Wait state for FIFO op ({0})",
                                              (peek == 0xd3d0) ? "OUT" : "OTIR");
                    return;
                }
            }

            // Yes!  Run an instruction
            var ticks = _cpu.ExecuteNextInstruction();

            // Advance our wakeup time now so the CPU can chill a bit
            _wakeup = (long)(_scheduler.CurrentTimeNsec + ((ulong)ticks * IOBoard.Z80CycleTime));

            // Run a DMA cycle
            _z80dma.Clock();

            // Run the scheduler
            _scheduler.Clock(ticks);
        }

        /// <summary>
        /// Pass keyboard input from the UI to the IOB (parallel) input latch.
        /// </summary>
        public override void QueueKeyboardInput(byte keyCode)
        {
            _keyboard.QueueInput(keyCode);
        }

        // Debugging
        public override void DumpFifos()
        {
            _z80ToPerqFifo.DumpFifo();
            _perqToZ80Fifo.DumpFifo();
        }

        public override void DumpPortAStatus()
        {
            _z80sio.DumpPortStatus(0);
        }

        public override void DumpPortBStatus()
        {
            Console.WriteLine($"{_system.Config.IOBoard} board does not have a serial port B.");
        }

        Z80SIO _z80sio;
        Z80CTC _z80ctc;
        Z80DMA _z80dma;
        IOReg3 _ioReg3;
        DMARouter _dmaRouter;
        Keyboard _keyboard;
        PERQToZ80Latch _perqToZ80Fifo;
        Z80ToPERQLatch _z80ToPerqFifo;
        HardDiskSeekControl _seekControl;

    }
}
