﻿//
// CanonController.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Memory;

namespace PERQemu.IO
{
    /// <summary>
    /// Implements the PERQ Canon Laser Printer controller.  This fairly simple
    /// hardware interface is driven by the microcode to stream bits to a Canon
    /// LBP-10 or LBP-CX print engine.  This controller provides the control and
    /// status registers, DMA/FIFO interface, and interrupt generation.  It runs
    /// a state machine similar to the actual hardware (contained in the CN100
    /// and CN200 PROMs), which is attached to an OIO board (any PERQ) or the
    /// MLO board (PERQ-2).
    /// </summary>
    /// <remarks>
    /// This implementation is the simplified (unfinished) version that doesn't
    /// use the serial command protocol or read back status bytes; it exposes
    /// only the signal lines that are present on the cable used by the actual
    /// hardware.  There is partial software support for gathering mechanical
    /// status but they never finished the hardware interface.  This should be
    /// remedied, eventually. :-|
    /// </remarks>
    public class CanonController
    {
        public CanonController(PERQSystem sys)
        {
            _system = sys;
            _state = ControllerState.Idle;
            _printer = new CanonPrinter(this, _system.Scheduler);
        }

        // Info
        public PaperCode Cassette
        {
            get { return _printer.PaperType; }
            set { _printer.LoadPaper(value); }
        }


        // Interface signals
        public bool ControllerPowerReady => true;       // CPRDY (NC)
        public bool VSync => _printer.VSyncRequest;     // VSYNC (I TOP L)
        public bool PrintRequest => _print;             // PRNT (I PRN L)
        public bool StatusRequest => false;             // CBSY (I STATUS REQ L)


        /// <summary>
        /// Power-on reset this instance and the attached printer.
        /// </summary>
        public void Reset()
        {
            _lineBuffer = new ScanLineBuffer { Bytes = new byte[CanonPrinter.ScanWidthInBytes] };

            _printer.Reset();
            InterfaceReset();
        }

        /// <summary>
        /// (Soft) Reset the interface/state machine.
        /// </summary>
        void InterfaceReset()
        {
            // Kill any outstanding event
            _system.Scheduler.Cancel(_delayEvent);
            _delayEvent = null;

            _marginDelay = _printer.GetMarginDelay();
            _lineCount = 0;

            _bob = false;
            _eop = false;
            _running = false;
            _testing = false;
            _irqEnabled = false;
            SetInterrupt(false);

            UpdateStatus();

            Log.Info(Category.Canon, "Interface reset");
        }

        /// <summary>
        /// Assert or deassert the "Y" interrupt.
        /// </summary>
        void SetInterrupt(bool raise)
        {
            // If running a test print, don't interrupt the PERQ!
            if (_testing) return;

            if (raise && _irqEnabled && !_irqRaised)
            {
                _system.CPU.RaiseInterrupt(Processor.InterruptSource.Y);
                _irqRaised = true;
            }
            else if ((!raise & _irqRaised) | !_irqEnabled)
            {
                _system.CPU.ClearInterrupt(Processor.InterruptSource.Y);
                _irqRaised = false;
            }
        }

        /// <summary>
        /// Update the current status register.
        /// </summary>
        /// <remarks>
        /// These are actually generated by the controller state machine, except
        /// for the printer Ready bit (poorly named as the "fault" interrupt).
        /// For my sanity, these are active true; the bits returned are inverted
        /// so the microcode sees the polarity it expects.
        /// </remarks>
        void UpdateStatus()
        {
            var ostatus = _status;

            _status = (_bob ? CanonStatus.BottomOfBand : 0) |
                      (_eop ? CanonStatus.EndOfPage : 0) |
                      (_printer.Ready ? CanonStatus.Ready : 0) |
                      (_statFull ? CanonStatus.StatusAvailable : 0);

            if (_status != ostatus)
                Log.Info(Category.Canon, "Controller status change: {0}", _status);
        }

        /// <summary>
        /// Read the IntStat or MechStat status registers.  Reading from IntStat
        /// also clears the interrupt line (but not the register contents).
        /// </summary>
        public int ReadStatus(byte address)
        {
            // Interrupt status register (4 bits, active low)
            if (address == 0x08)
            {
                UpdateStatus();
                SetInterrupt(false);
                Log.Info(Category.Canon, "Controller status read: {0} ({1:x})", _status, ~((int)_status) & 0x0f);
                return ~((int)_status) & 0x0f;
            }

            // Mechanical status register (16 bits)
            if (address == 0x09)
            {
                // This isn't fully implemented in the hardware and is partly
                // commented out in the microcode.  The CN100 PROM and 9403 FIFO
                // chips are partially wired up in the schematic to return the
                // STATFULL signal but the interface was incomplete.  For now,
                // return 0 (if this is ever called).
                Console.WriteLine("==> Canon Mechanical Status requested!! <==");   // debug
                return 0;
            }

            throw new InvalidOperationException($"Bad register read 0x{address:x2}");
        }

        /// <summary>
        /// Load the CanonCntl register (oct 205/0x85) to initiate commands to
        /// the state machine and/or printer.  This is the half-baked interface
        /// that manipulates control lines directly (no command byte sent via
        /// the serial SC/SCLK lines).
        /// </summary>
        /// <remarks>
        /// Decodes the 5 significant bits:
        ///      Bit 0: NotBlank (CcNotBlank, active LOW)
        ///      Bit 1: NotReset (CcNotReset, active LOW)
        ///      Bit 2: EnableVideo (CcEnbVideo, active HIGH, i.e. StartPage)
        ///      Bit 3: ReadStatus (CcStatRead, active HIGH)
        ///      Bit 4: EnableInterrupts (CcEnbInt, active HIGH)
        /// </remarks>
        public void LoadCommand(int value)
        {
            _command = (CanonControl)value;
            Log.Info(Category.Canon, "Write 0x{0:x2} to command register: {1}", value, _command);

            // Update on every load
            _irqEnabled = _command.HasFlag(CanonControl.EnableInterrupts);
            _print = _command.HasFlag(CanonControl.EnableVideo);

            // Check for Reset first; assume it supercedes other commands
            if (!_command.HasFlag(CanonControl.NotReset))
            {
                // Only reset once, on the falling edge.  Microcode sends the
                // reset, followed by a nop, followed by a "GetReady" (which is
                // just the NotReset/EnbIntr bits) to create a 340ns reset pulse
                if (_state != ControllerState.Reset)
                {
                    RunStateMachine(ControllerState.Reset);
                }
                return;
            }

            // Assume that a status read only comes in response to a fault, an
            // end of page, or some other condition NOT during printing?!  But
            // it isn't implemented in the hardware so I bet this isn't used
            // yet anyway...
            if (_command.HasFlag(CanonControl.StatusRead))
            {
                Log.Info(Category.Canon, "ReadStatus command!");
                // Not yet implemented in state machine...
                return;
            }

            // The EnableVideo (aka StartPage) bit latches the I PRN signal in
            // a flip flop directly.  Capture the initial transition to kick off
            // the printer's state too by asserting PRINT
            if (_print && !_running)
            {
                Log.Info(Category.Canon, "StartPage command!");
                RunStateMachine(ControllerState.Start);
                return;
            }

            // If no other command is asserted, just run the state machine
            RunStateMachine(_state);
        }

        /// <summary>
        /// Loads the Canon interface operational registers.  The Margin regs
        /// dictate the page/bitmap dimensions, while the LineCount is used to
        /// count down the number of lines in a band.
        /// </summary>
        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                case 0x84:      // LineCount

                    // The hardware loads IOD<3:0> into an 'LS191 so clip it to
                    // 4 bits.  This might be spuriously written when the Streamer
                    // is active, so I assume this means you can't run the tape
                    // drive and printer at the same time. :-(
                    _linesInBand = value & 0x0f;
                    break;

                case 0x94:      // Margin control
                    _marginCtrl = value;

                    // If the low 2 bits are 1s, resets the Am2942 (clears the
                    // internal counters).  Anything else is a no-op.
                    if (value == 3)
                    {
                        _leftMargin = 0;
                        _rightMargin = 0;
                    }
                    break;

                case 0x95:      // Left margin - blank words (1s complement)
                    _leftMargin = (~value & 0xff);
                    break;

                case 0x96:      // Right margin - line length (1s complement)
                    _rightMargin = (~value & 0xff);
                    break;

                default:
                    throw new InvalidOperationException($"Bad register write 0x{address:x2}");
            }

            Log.Info(Category.Canon, "Write 0x{0:x2} to data register 0x{1:x2}", value, address);
        }


        /// <summary>
        /// Run the state machine in response to a signal change in the printer.
        /// </summary>
        public void SignalStateChange()
        {
            RunStateMachine(_state);
        }

        /// <summary>
        /// Update the internal controller state in response to new commands or
        /// signal pin changes from the print engine.
        /// </summary>
        void RunStateMachine(ControllerState next)
        {
            Log.Debug(Category.Canon, "[Controller SM  IN state={0} next={1}]", _state, next);

            if (_printer.Ready)
            {
                switch (next)
                {
                    case ControllerState.Idle:
                        //
                        // Not much to do... but if we get here as a result of an
                        // EOP or loss of RDY from the printer, clear interrupt
                        // conditions just in case.  Screendump seems to need to
                        // see EOP, while CPrint barfs and resends the last page.
                        // It's all still stupidly inconsistent and mysterious so
                        // just mess with it 'til it works. :-/
                        //
                        _bob = false;
                        _eop = false;
                        Log.Debug(Category.Canon, "[Controller idle]");
                        break;

                    case ControllerState.Reset:
                        //
                        // Assume that asserting Reset always clears the interface
                        // regardless of the current status.  LoadCommand will issue
                        // on the initial assertion, so transition to Idle on the
                        // next pass (rising edge).
                        //
                        if (_state != ControllerState.Reset)
                        {
                            InterfaceReset();
                        }
                        else
                        {
                            next = ControllerState.Idle;
                        }
                        break;

                    case ControllerState.Start:
                        //
                        // The StartPage command bit literally asserts the PRINT pin
                        // (I PRN) on the interface; it's latched by a flip flop and
                        // must remain asserted until the rising edge of VSYNC.  If
                        // the printer is not asserting RDY then fall back to Idle.
                        //
                        next = ControllerState.WaitForVSReq;
                        _running = true;
                        _printer.SignalStateChange();      // Assert PRNT
                        break;

                    case ControllerState.WaitForVSReq:
                        //
                        // Wait for the printer to assert VSREQ to indicate that it
                        // is at the top of the page and ready to print.  There may
                        // be a delay of up to 10 seconds from PRINT to VSREQ if the
                        // printer is coming out of standby mode or we're waiting
                        // for the previous page to finish.
                        //
                        if (_printer.VSyncRequest)
                        {
                            Log.Debug(Category.Canon, "[Controller VSReq received]");

                            // VSync gets asserted automatically!  I TOP (VSReq)
                            // also clears the I PRN (PRNT) flip flop and sets
                            // the CN100 internal state to 7 (printing enabled)
                            _print = false;

                            // Start watching horizontal clock pulses
                            next = ControllerState.WaitForBD;
                        }
                        break;

                    case ControllerState.WaitForBD:
                        //
                        // The BD pulse synchronizes the start of each horizontal
                        // line.  Technically we wait for 't' usec (based on paper
                        // size and printer model) before sending VDO, the bitmap
                        // data.  We do this here to break up the possible loop of
                        // printer state transitions driving controller transitions
                        // endlessly.  This has become needlessly complicated...
                        //
                        if (_printer.Band && _delayEvent == null)
                        {
                            _delayEvent = _system.Scheduler.Schedule(_marginDelay, (skewNsec, context) =>
                            {
                                RunStateMachine(ControllerState.Printing);
                            });
                        }
                        else if (_printer.StatusBusy)
                        {
                            // Printer reached end of page and turned off the BD
                            // clock.  We probably should have caught that, but
                            // if not jump to EndOfPage...
                            next = ControllerState.EndOfPage;
                        }
                        break;

                    case ControllerState.Printing:
                        //
                        // Arrive here after the margin delay, so clear the event.
                        //
                        _delayEvent = null;

                        // If starting a new band, reload.
                        if (_lineCount == 0)
                        {
                            _lineCount = _linesInBand;
                            _bandAddr = _system.IOB.DMARegisters.GetHeaderAddress(ChannelName.ExtB);
                            Log.Info(Category.Canon, "[Controller line count reloaded ({0} lines @ 0x{1:x})]", _lineCount, _bandAddr);
                        }

                        // Another scanline to send?
                        if (_lineCount > 0)
                        {
                            _bob = false;

                            if (!_command.HasFlag(CanonControl.NotBlank))
                            {
                                // Blank bit (active low) just counts BD pulses
                                _printer.PrintBlank();
                            }
                            else
                            {
                                PrintNormal();
                            }

                            _lineCount--;
                        }

                        // Wait for the next interrupt
                        next = ControllerState.WaitForBD;

                        // Unless we reached end of the band?
                        if (_lineCount == 0) next = ControllerState.EndOfBand;

                        // Or the end of the page?
                        if (_printer.StatusBusy) next = ControllerState.EndOfPage;

                        break;

                    case ControllerState.EndOfBand:
                        //
                        // Fire the BOB interrupt; microcode handler will reload
                        // LineCount, DMA registers on the next print command.
                        //
                        Log.Debug(Category.Canon, "[Controller BOB reached]");

                        if (!_bob)
                        {
                            _bob = true;
                            UpdateStatus();
                            SetInterrupt(true);
                        }
                        next = ControllerState.Printing;
                        break;

                    case ControllerState.EndOfPage:
                        //
                        // Here when the IPRend signal goes low, since the printer
                        // counting its own clocks seems to be the only reliable
                        // way to figure out how the hell to end a page.  Ugh.
                        //
                        Log.Debug(Category.Canon, "[Controller EOP received]");

                        _eop = true;
                        UpdateStatus();
                        SetInterrupt(true);

                        _running = false;
                        next = ControllerState.Idle;
                        break;

                    default:
                        throw new InvalidOperationException($"Unknown controller state {next}");
                }
            }
            else
            {
                // If the printer dropped RDY, that's an abort
                Log.Info(Category.Canon, "[Printer not Ready, aborting ({0})]", next);
                UpdateStatus();
                SetInterrupt(true);

                _running = false;
                next = ControllerState.Idle;
            }

            _state = next;
            Log.Debug(Category.Canon, "[Controller SM OUT state={0}]", _state);
        }

        /// <summary>
        /// Render a "normal" line from memory into a scanline buffer then push
        /// it to the printer.
        /// </summary>
        void PrintNormal()
        {
            //
            // The hardware state machine counts bits and words to manage the DMA
            // request line, top up the FIFO when more quad words are needed, and
            // generate the VDO stream (synced with the BD pulses from the printer).
            // Here we cheat and do a whole scanline at a time directly from memory,
            // like the VideoController.  The microcode updates the DMA registers in
            // response to BOB interrupts to point to the start of the next block of
            // memory (one band, or 1-16 scanlines worth), so the state machine inits
            // the starting address for us.  Note that unlike video refresh (which
            // has dedicated bandwidth) this cheat avoids DMA cycle stealing from
            // the processor, so it's not 100% authentic.  Meh.
            //

            var wordWidth = _rightMargin - _leftMargin;     // These better be
            var addr = _bandAddr >> 2;                      // quad word aligned!

            // Debug
            if ((wordWidth % 4) != 0 || (_bandAddr & 0x3) != 0)
                Log.Warn(Category.Canon, "BAD WIDTH {0} OR DMA START ADDR 0x{1:x}", wordWidth, _bandAddr);

            // Grab quads from memory
            for (var i = 0; i < wordWidth / 4; i++)
            {
                _lineBuffer.Quads[i] = _system.Memory.FetchQuad(addr++);
                _bandAddr += 4;
            }

            // Ship it as bytes
            _printer.PrintLine(_leftMargin, wordWidth * 2, _lineBuffer.Bytes);
        }

        public void Shutdown()
        {
            // If output in progress, close/kill it
            if (_running) _printer.Reset();

            _printer.SavePageCount();
            _printer = null;
        }


        public void TestPrint()
        {
            // Run through the loop to test the printer!  Generate a bitmap and
            // send it to shake out functionality of the interface and printer
            // emulation, different paper sizes and output formats, etc.  This is
            // kinda goofy but there is a test button on the printer, so... :-)
        }


        // Debugging
        public void DumpStatus()
        {
            Console.WriteLine($"Canon controller state: {_state}");
            Console.WriteLine($"    Command: {_command}  Testing: {_testing}");
            Console.WriteLine($"  MarginCtl: {_marginCtrl}  Left: {_leftMargin}  Right: {_rightMargin}");
            Console.WriteLine($"  LineCount: {_linesInBand}  Current: {_lineCount} @ 0x{_bandAddr:x}");
            Console.WriteLine();
            Console.WriteLine($"Interface:   PRNT={_print} BOB={_bob} EOP={_eop} STATFULL={_statFull}");
            Console.WriteLine($"Interrupts:  Enabled={_irqEnabled} Raised={_irqRaised}");
            Console.WriteLine();

            _printer.DumpStatus();
        }


        PERQSystem _system;
        CanonPrinter _printer;

        ControllerState _state;
        CanonControl _command;
        CanonStatus _status;

        // Registers
        int _marginCtrl;
        int _linesInBand;
        int _leftMargin;
        int _rightMargin;

        // Signals
        bool _print;

        // Specific interrupt causes
        bool _bob;              // Bottom of Band
        bool _eop;              // End of page (IPRND received)
        bool _statFull;         // Mechanical status available

        bool _irqEnabled;
        bool _irqRaised;
        bool _running;
        bool _testing;

        int _bandAddr;
        int _lineCount;
        ScanLineBuffer _lineBuffer;

        ulong _marginDelay;
        SchedulerEvent _delayEvent;
    }

    /// <summary>
    /// Bits in the PERQ's Canon interface control register.
    /// </summary>
    [Flags]
    public enum CanonControl
    {
        NotBlank = 0x01,
        NotReset = 0x02,
        EnableVideo = 0x04,
        StatusRead = 0x08,
        EnableInterrupts = 0x10
    }

    /// <summary>
    /// Canon interrupt status register.  (Inverted when reported to PERQ)
    /// </summary>
    [Flags]
    public enum CanonStatus
    {
        NotReady = 0x0,
        BottomOfBand = 0x01,
        EndOfPage = 0x02,
        Ready = 0x04,
        StatusAvailable = 0x08
    }

    /// <summary>
    /// Controller states, based loosely on the CN100/CN200 PROMs.
    /// </summary>
    enum ControllerState
    {
        Idle = 0,
        Start,              // Assert PRINT to wake up printer
        WaitForVSReq,       // Wait for VSREQ line, send VSYNC
        WaitForBD,          // Wait for BD line, send VDO
        Printing,           // Print a line
        EndOfBand,          // Send BOB interrupt, wait for next
        EndOfPage,          // Deassert PRINT or get ready for next page
        Reset
    }
}
