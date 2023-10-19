//
// EIOZ80.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Config;
using PERQemu.Debugger;
using PERQemu.IO.GPIB;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The EIO board's Z80 subsystem, used in all PERQ-2 models.
    /// </summary>
    public sealed class EIOZ80 : Z80System
    {
        public EIOZ80(PERQSystem system) : base(system)
        {
            // Set up the infrastructure
            _perqToZ80Fifo = new PERQToZ80FIFO(_system);
            _z80ToPerqFifo = new Z80ToPERQFIFO(_system);

            _irqControl = new Am9519(0x60);
            _dmac = new i8237DMA(0x38, 0x30);

            // Set up the EIO peripherals
            _tms9914a = new TMS9914A(0);
            _fdc = new NECuPD765A(0x20, _scheduler);
            _z80sioA = new Z80SIO(0x10, _scheduler, true);
            _z80sioB = new Z80SIO(0x40, _scheduler, true);
            _timerA = new i8254PIT(0x50);
            _timerB = new i8254PIT(0x54);
            _rtc = new Oki5832RTC(0x76);

            // Create our serial devices
            _keyboard = new SerialKeyboard();
            // _speech = new Speech(...)

            // Same Z80 code for EIO/NIO
            // TODO: verify for all variants?  8"/5.25", 24-bit?
            _z80Debugger = new Z80Debugger("eioz80.lst");

            DeviceInit();
        }

        // Port "A" is public, since it's a DMA-capable device...
        public override Z80SIO SIOA => _z80sioA;

        // No hard disk seek circuit on the EIO
        public override Z80CTC CTC => null;

        // The EIO doesn't use the WAIT Z80 line like the IOB does; these are
        // just returned via status registers instead...
        protected override bool FIFOInputReady => _perqToZ80Fifo.IsReady;
        protected override bool FIFOOutputReady => _z80ToPerqFifo.IsReady;

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
                _z80sioA.AttachDevice(1, tablet);
            }

            // Attach the keyboard
            _z80sioB.AttachDevice(1, _keyboard);

            // If enabled and configured, attach device to RS232 port A
            if (_system.Config.RSAEnable && Settings.RSADevice != string.Empty)
            {
                if (Settings.RSADevice == "RSX:")
                {
                    var rsx = new RSXFilePort(this);
                    _z80sioA.AttachPortDevice(0, rsx);
                    // _timerA.AttachDevice(0, rsx);
                }
                else
                {
                    var rsa = new PhysicalPort(this, Settings.RSADevice, Settings.RSASettings, "A");
                    _z80sioA.AttachPortDevice(0, rsa);
                    //_timerA.AttachDevice(0, rsa);  figure out how the PIT does it
                }
            }
            else
            {
                // Otherwise direct it to the bit bucket
                _z80sioA.AttachPortDevice(0, new NullPort(this));
            }

            // Now do RS232 port B
            if (_system.Config.RSBEnable && Settings.RSBDevice != string.Empty)
            {
                if (Settings.RSBDevice == "RSX:")
                {
                    var rsx = new RSXFilePort(this);
                    _z80sioB.AttachPortDevice(0, rsx);
                    // _timerB.AttachDevice(0, rsx);
                }
                else
                {
                    var rsb = new PhysicalPort(this, Settings.RSADevice, Settings.RSASettings, "B");
                    _z80sioB.AttachPortDevice(0, rsb);
                    //_timerB.AttachDevice(0, rsb);  figure out how the PIT does it
                }
            }
            else
            {
                // Otherwise direct it to the bit bucket
                _z80sioB.AttachPortDevice(0, new NullPort(this));
            }

            // All aboard the bus
            _bus.RegisterDevice(_perqToZ80Fifo);
            _bus.RegisterDevice(_z80ToPerqFifo);
            _bus.RegisterDevice(_fdc);
            _bus.RegisterDevice(_rtc);
            _bus.RegisterDevice(_dmac);
            _bus.RegisterDevice(_timerA);
            _bus.RegisterDevice(_timerB);
            _bus.RegisterDevice(_z80sioA);
            _bus.RegisterDevice(_z80sioB);
            _bus.RegisterDevice(_tms9914a);
        }

        protected override void DeviceReset()
        {
            // Todo: verify all this with the schematics
            _fdc.Reset();
            _tms9914a.Reset();
            _z80ToPerqFifo.Reset();
            _perqToZ80Fifo.Reset();
        }

        protected override void DeviceShutdown()
        {
            // If serial devices are attached, close them properly
            _z80sioA.DetachDevice(0);   // RS232-A
            _z80sioB.DetachDevice(0);   // RS232-B
        }

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
                    // will wake us when it catches up.  The faster 4Mhz EIO Z80
                    // still takes ~13-80 PERQ microcycles to execute a complete
                    // instruction (using the typical 9-55 clocks per inst metric).
                    diff = -diff;

                    if ((ulong)diff > _system.Scheduler.TimeStepNsec)
                    {
                        // Pause the thread
                        _sync.Reset();
                    }
                }
                return;
            }

            // No WAIT line INIR/OTIR shenanigans on the EIO!
            // TODO: However, we might have to clock the interrupt controller here?
            // _irqControl.Clock();

            // Run an instruction
            var ticks = _cpu.ExecuteNextInstruction();

            // Advance our wakeup time now so the CPU can chill a bit
            _wakeup = (long)(_scheduler.CurrentTimeNsec + ((ulong)ticks * IOBoard.Z80CycleTime));

            // Clock the EIO DMA
            _dmac.Clock();

            // Run the scheduler
            _scheduler.Clock(ticks);
        }

        /// <summary>
        /// Writes the control register.  This part deals with the bits relevant
        /// to the Z80:
        ///     bit 2 - Z80 reset when clear
        ///     bit 1 - Enable write channel; interrupt when set
        ///     bit 0 - Enable read channel; interrupt when set
        /// </summary>
        public override void WriteStatus(int status)
        {
            bool prevState = _running;

            //
            // Check the Reset bit first
            //
            if (_running && ((status & 0x04) == 0))
            {
                Log.Debug(Category.Z80, "Shut down by write to Status register");
                _running = false;
            }
            else if (!_running && ((status & 0x04) != 0))
            {
                Log.Debug(Category.Z80, "Started by write to Status register");
                Reset(true);
            }

            if (_running != prevState)
            {
                _system.MachineStateChange(WhatChanged.Z80RunState, _running);
            }

            // Set the enable bits
            _perqToZ80Fifo.InterruptEnabled = ((status & 0x02) != 0);
            _z80ToPerqFifo.InterruptEnabled = ((status & 0x01) != 0);
        }

        /// <summary>
        /// Reads the status register.  Corresponds to IOA 125 (0x55).  The valid
        /// bits reflect the status of the FIFOs:
        ///     bit 15 - Read data ready when set       (i.e., data present)
        ///     bit 7  - Write channel ready when set   (i.e., FIFO is empty)
        /// </summary>
        public override int ReadStatus()
        {
            int status = (_z80ToPerqFifo.IsReady ? 0x8000 : 0); // Bit 15: read ready
            status |= (_perqToZ80Fifo.IsReady ? 0x0080 : 0);    // Bit 7: write ready

            return status;
        }

        /// <summary>
        /// Writes a byte to the Z80's input FIFO.
        /// </summary>
        public override void WriteData(int data)
        {
            _perqToZ80Fifo.Enqueue(data & 0xff);
        }

        /// <summary>
        /// Reads a byte from the Z80's output FIFO.
        /// </summary>
        public override int ReadData()
        {
            return _z80ToPerqFifo.Dequeue();
        }

        /// <summary>
        /// Pass keyboard input from the host to the emulated device.
        /// </summary>
        public override void QueueKeyboardInput(byte keyCode)
        {
            _keyboard.QueueInput(keyCode);
        }

        // debug
        public override void DumpFifos()
        {
            _z80ToPerqFifo.DumpFifo();
            _perqToZ80Fifo.DumpFifo();
        }

        public override void DumpPortAStatus()
        {
            _z80sioA.DumpPortStatus(0);
        }

        public override void DumpPortBStatus()
        {
            _z80sioB.DumpPortStatus(0);
        }

        //
        // EIO/NIO boards
        //
        Am9519 _irqControl;
        Z80SIO _z80sioA, _z80sioB;
        i8254PIT _timerA, _timerB;
        i8237DMA _dmac;
        Oki5832RTC _rtc;
        SerialKeyboard _keyboard;
        PERQToZ80FIFO _perqToZ80Fifo;
        Z80ToPERQFIFO _z80ToPerqFifo;

    }
}

/*
    Notes:

    FIFO interface from EIO.doc corresponds with the EIO source code defs:
    
    IOZ  REGISTER USE
    160  Write FIFO         bits 0:7 - Data from CPU
    161  Read FIFO          bits 0:7 - Data to CPU
    162  Status register    bit 7  - Read FIFO not full
							bit 6  - Write FIFO not empty
    170  Control register   bit 0  - Output ready

    //
    //        PERQ READ AND WRITE PORTS
    //
    //PERQ.IN   equ 160Q    ; PERQ Input port
    //PERQ.OUT  equ 161Q    ; PERQ Output port
    //PERQ.STS  equ 162Q    ; PERQ Status port
    //PERQ.Rdy  equ 170Q    ; PERQ Ready port

    ----
    GPIB
    ----

    ;
    ;        GPIB
    ;
    ;              READ REGISTERS
    ;
    GPIntSt0   EQU 000Q         ; INTERRUPT STATUS 0
    GPIntSt1   EQU 001Q         ; Interrupt status 1
    GPAdrSt    EQU 002Q         ; Address status
    GPBusSt    EQU 003Q         ; Bus Status
    GPAdrSw1   EQU 004Q         ; Address switch 1
    GPCmdPass  EQU 006Q         ; Command pass through
    GPDIn      EQU 007Q         ; DATA IN

    ;
    ;              WRITE REGISTERS
    ;
    GPIMsk0     equ 000Q        ; INTERRUPT MASK 0
    GPIMsk1     equ 001Q        ; INTERRUPT MASK 1
    GPAux       equ 003Q        ; AUXILLIARY COMMAND
    GPDOut      equ 007Q        ; DATA OUT

    GPControl   equ 173Q        ; 0 = Not controller, 1 = controller
    GPTriState  equ 174Q        ; 0 = Open Collector data, 1 = TriState

    Did they RENUMBER THE DECODES for the 9914 registers!?  WHAT?
    I'm assuming these are just remapped BY the Z80 code and presented to the
    PERQ in the shuffled order and the hardware obviously hasn't changed; look
    at the code in detail to see where this occurs.  SIMILARLY, double check if
    the port f*ckery for the SIO chip(s) is strictly necessary.  (In that case
    it DOES look like they specifically decode the addresses to swap regs 1&2.)

 */
