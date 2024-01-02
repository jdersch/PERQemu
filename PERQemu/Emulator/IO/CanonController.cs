//
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


namespace PERQemu.IO
{
    public class CanonController
    {
        /// <summary>
        /// Implement's the PERQ Canon Laser Printer controller.  This fairly
        /// simple hardware interface is driven by the microcode to stream bits
        /// to a Canon LBP-10 or LBP-CX print engine.  This half of the circuit
        /// provides the control and status registers, DMA/FIFO interface, and
        /// interrupt generation.  It runs a state machine similar to the actual
        /// hardware (contained in the CN100/CN200 PROMs), which is attached to
        /// an OIO board (any PERQ) or the MLO board (PERQ-2).
        /// </summary>
        public CanonController(PERQSystem sys)
        {
            _system = sys;
            _printer = new CanonPrinter(this);
        }

        public PaperCode Cassette
        {
            get { return _printer.PaperType; }
            set { _printer.LoadPaper(value); }
        }

        public void Reset()
        {
            SetInterrupt(false);
            _irqEnabled = false;
            _busy = false;

            Log.Info(Category.Canon, "Interface reset");
            _printer.Reset();
        }

        /// <summary>
        /// Assert or deassert the "Y" interrupt.
        /// </summary>
        void SetInterrupt(bool raise)
        {
            if (raise && !_irqRaised)
            {
                _system.CPU.RaiseInterrupt(Processor.InterruptSource.Y);
                _irqRaised = true;
            }
            else if (!raise & _irqRaised)
            {
                _system.CPU.ClearInterrupt(Processor.InterruptSource.Y);
                _irqRaised = false;
            }
        }

        /// <summary>
        /// Read the IntStat or MechStat status registers.
        /// </summary>
        public int ReadStatus(byte address)
        {
            if (address == 0x08)
            {
                // Interrupt status register (4 bits, active low)
                var status =
                    (_printer.BottomOfBand ? 0 : 0x01) |
                    (_printer.EndOfPage ? 0 : 0x02) |
                    (_printer.PrinterFault ? 0x04 : 0) |
                    (_printer.StatFull ? 0 : 0x08);
                return status;
            }

            if (address == 0x09)
            {
                // Mechanical status register (16 bits)
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
        public void LoadCommand(int value)
        {
            _command = (CanonControl)value;
            Log.Info(Category.Canon, "Write 0x{0:x2} to command register: {1}", value, _command);

            // Decode the 5 significant bits:
            //      Bit 0: PrintBlankBand (CcNotBlank, active LOW)
            //      Bit 1: Reset (CcNotReset, active LOW)
            //      Bit 2: StartPage (CcEnbVideo, active HIGH)
            //      Bit 3: ReadStatus (CcStatRead, active HIGH)
            //      Bit 4: EnableInterrupts (CcEnbInt, active HIGH)

            // Update on every load
            _irqEnabled = _command.HasFlag(CanonControl.EnableInterrupts);

            // Check for Reset first; assume it supercedes other commands
            if (!_command.HasFlag(CanonControl.Reset))
            {
                Reset();
                return;
            }

            // Check the other bits
            if (_command.HasFlag(CanonControl.StartPage))
            {
                Log.Info(Category.Canon, "StartPage received");
                //
                // todo: tell the printer to set up for a new page
                //
                _busy = true;   // for now
                return;
            }

            // Assume that a status read only comes in response to a fault, an
            // end of page, or some other condition NOT during printing?!  But
            // it isn't implemented in the hardware so I bet this isn't used
            // yet anyway...
            if (_command.HasFlag(CanonControl.StatusRead))
            {
                Log.Info(Category.Canon, "ReadStatus requested! (busy={0})", _busy);
                return;
            }

            // If we're here, then we're either printing or some other weird or
            // unexpected condition has arisen... let's assume the best for now.

            if (_busy)
            {
                //
                // todo: tell the printer to print a line/band ?
                //
                Log.Info(Category.Canon, "Printing a {0} line",
                         _command.HasFlag(CanonControl.PrintBlankBand) ? "normal" : "blank");
                return;
            }

            Log.Info(Category.Canon, "Whoops.  Not sure what to do here.");
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
                    //
                    // This might be spuriously written when the Streamer is
                    // active, so we only reload the band count from it when
                    // the printer is running.  The hardware takes IOD<3:0>
                    // into an 'LS191 so clip it to 4 bits.
                    //
                    _lineCount = value & 0x0f;
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

        public void Shutdown()
        {
            // if an output file is open, fle it?
        }

        // Debugging
        public void DumpStatus()
        {
            Console.WriteLine("Canon controller state:");
            Console.WriteLine($"    Command: {_command}");
            Console.WriteLine($"  MarginCtl: {_marginCtrl}  Left: {_leftMargin}  Right: {_rightMargin}");
            Console.WriteLine($"  LineCount: {_lineCount}");
            Console.WriteLine();

            _printer.DumpStatus();
        }

        // Registers
        int _marginCtrl;
        int _lineCount;
        int _leftMargin;
        int _rightMargin;

        CanonControl _command;
        bool _busy;
        bool _irqEnabled;
        bool _irqRaised;

        PERQSystem _system;
        CanonPrinter _printer;
    }

    /// <summary>
    /// Bits in the PERQ's Canon interface control register.
    /// </summary>
    [Flags]
    public enum CanonControl
    {
        PrintBlankBand = 0x01,
        Reset = 0x2,
        StartPage = 0x4,
        StatusRead = 0x8,
        EnableInterrupts = 0x10
    }
}
