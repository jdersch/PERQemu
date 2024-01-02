//
// CanonPrinter.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
    /// <summary>
    /// Emulates the Canon LBP-10 (240dpi) and CX (300dpi) laser printers.
    /// These printers connect to a PERQ through the Canon "video" interface, a
    /// bitstream sent directly from the host to the laser marking engine (just
    /// like the NeXT laser printer, years later).
    /// </summary>
    /// <remarks>
    /// This implementation is the simplified (unfinished) version that doesn't
    /// use the serial command protocol or read back status bytes; it exposes
    /// only the signal lines that are present on the cable used by the actula
    /// hardware.  There is partial software support for gathering mechanical
    /// status but they never finished the hardware interface.  This should be
    /// remedied, eventually. :-|
    /// </remarks>
    public class CanonPrinter
    {
        public CanonPrinter(CanonController ctrl)
        {
            _control = ctrl;
            _cassette = Settings.CanonPaperSize;
            _resolution = Settings.CanonResolution;

            LoadPaper(_cassette);
        }

        // Status codes
        public PaperCode PaperType => _cassette;
        public bool BottomOfBand => _bob;
        public bool EndOfPage => _eop;
        public bool PrinterFault => _fault;
        public bool StatFull => false;          // Not implemented (yet!)


        public void Reset()
        {
            Log.Info(Category.Canon, "Printer reset");
        }


        /// <summary>
        /// Load paper into the printer.
        /// </summary>
        public void LoadPaper(PaperCode type, bool manualFeed = false)
        {
            // if we're printing, interrupt?  like, they pulled out the tray :-)
            switch (type)
            {
                case PaperCode.A4:
                case PaperCode.B5:
                case PaperCode.USLetter:
                case PaperCode.USLegal:
                    _cassette = type;
                    _pagesLeft = (uint)(manualFeed ? 1 : 100);
                    break;

                default:
                    _cassette = PaperCode.NoCassette;
                    _pagesLeft = 0;
                    break;
            }

            Log.Info(Category.Canon, "Paper tray loaded: {0}, ({1} pages left)", _cassette, _pagesLeft);
        }

        // Debugging
        public void DumpStatus()
        {
            Console.WriteLine("Canon LBP-{0} printer state:", (_resolution == 300 ? "CX" : "10"));
            // signals, status, etc
            Console.WriteLine($"  Resolution: {_resolution}dpi  Pages printed: {_totalPages}");
            Console.WriteLine($"  Paper tray: {_cassette}  Sheets: {_pagesLeft}");
        }

        CanonController _control;
        CommandCode _command;
        PaperCode _cassette;
        uint _resolution;

        uint _pagesLeft;        // How many sheets in the tray?
        uint _totalPages;       // So we run out of toner and the print gets lighter

        // Specific interrupt causes
        bool _bob;              // Bottom of Band
        bool _eop;              // End of page (aka IPRend)
        bool _fault;            // Printer fault (aka IReady)
        bool _statFull;         // Mechanical status available
    }

    /// <summary>
    /// Command code bytes recognized by the controller.  Any other values
    /// are rejected as illegal commands.
    /// </summary>
    enum CommandCode
    {
        Status0Request = 0x01,  // SR0
        Status1Request = 0x02,  // SR1
        Status2Request = 0x04,  // SR2
        Status4Request = 0x08,  // SR4
        Status5Request = 0x0b,  // SR5
        ControlOutput = 0x40,   // SCLK controller output mode
        PrinterOutput = 0x43,   // SCLK printer output mode
        Pause = 0x45,           // Pause request
        PauseRelease = 0x46,    // Pause release
        StartDrum = 0x49,       // Start drum rotation
        StopDrum = 0x4a,        // Stop drum rotation
        CassetteFeed = 0x4c,    // Cassette paper feed
        ManualFeed = 0x4f,      // Manual paper feed
        Retransmit = 0x5d       // Retransmission request release
    }

    /// <summary>
    /// Status 0 (Standard status).
    /// </summary>
    enum Status0
    {
        // Bit 1 = 0
        PrintRequest = 0x40,
        PaperDelivery = 0x20,
        RetransmitReq = 0x10,
        Wait = 0x08,
        Pause = 0x04,
        Call = 0x02
        // Bit 8 = parity (odd)
    }

    /// <summary>
    /// Status 1 (Operator-call information).
    /// </summary>
    enum Status1
    {
        // Bit 1 = 0
        NoCartridge = 0x40,
        // Bit 3 = reserved
        PaperOut = 0x10,
        PaperJam = 0x08,
        // Bit 6 = reserved
        TestPrinting = 0x02
        // Bit 8 = parity (odd)
    }

    //
    //  Status 2 is the Service-call information status byte; the emulator
    //  always returns 0 when this status is requested (no malfunctions).
    //
    //  Status 4 is the Number of Pages to be Retransmitted status byte.  It's
    //  a six bit count returned if a retransmission of data is required, but
    //  it's not yet known if the PERQ software ever uses this.  TBD.
    //
    //  Status 5 is the Paper Size status, which returns a PaperCode indicating
    //  what size paper is loaded in the printer.  The format is a zero in the
    //  first bit, the paper size in bits 2..7 (transmitted as PaperCode<5:0>) 
    //  and the usual trailing odd parity as bit 8.
    //  

    /// <summary>
    /// Paper sizes for the supported cassettes (status code values).
    /// </summary>
    public enum PaperCode
    {
        NoCassette = 0x0,
        A4 = 0x01,              // 297mm x 210mm
        USLetter = 0x04,        // 279mm x 216mm
        B5 = 0x09,              // 257mm x 182mm
        USLegal = 0x0c          // 356mm x 216mm
    }
}

/*
    Notes:

    To do it accurately, the printer should:

        Use PPRDY to indicate that the power is on, and read CPRDY from the
        controller to sense when it's ready to communicate;

        Be able to exchange commands and status bytes using the serial line
        based on SBSY/CBSY (who's talking) and SC/SCLK (the data and clock),
        while still printing in the background too!

        Use the RDY, VSREQ and BD lines to indicate status of the print in
        progress, reading the PRNT, VSYNC and VDO lines from the controller.

    For simplicity (in emulation) there's no need to _actually_ toggle the
    serial bits -- we can just exchange command and status bytes and use the
    Scheduler to indicate completion based on the hardware's timings.

    
    Ignoring the command/status exchange for now (not implemented fully on the
    PERQ) this simplifies to:

        Assume PPRDY (and CPRDY) are always true;
            [PPRDY comes true .5-1sec from power on]

        Assert RDY when we are able to accept a print request:
            A cassette is inserted and is not empty
            The state machine is idle
        
        When the controller asserts PRNT to start a new job:
            Create a new bitmap based on the paper size, resolution;
        *    Reset the current line and horizontal counters to zero;
            [Standby warm-up time is < 10sec; < 3.7sec continuous printing]
            Assert the VSREQ pin
            Set the state to WaitForVSync

        The controller asserts VSYNC at the start of a new page:
            Spec says VSYNC must be asserted within 10ms after VSREQ; the
            PERQ loops the signal back in the cable so its instantaneous :-|

            Printer deasserts VSREQ 5ms after VSYNC asserted.  VSYNC is
            supposed to be asserted from 5 to 400ms typ

            Spec says 212ms have to pass before asserting VDO; this
            allows time for the (typically 5mm) leading edge (top margin)
            to feed.  We'll have to see how the state machine does this.

            IT IS NOT AT ALL CLEAR IF BD IS USED DURING THE TOP MARGIN
            (212ms) WINDOW WHILE THE PAPER IS FEEDING; STEP THROUGH THE
            STATE MACHINE TO SEE IF IT RELIES ON BD TO COUNT DOWN LIKE
            A "FRONT PORCH" TIME

        The printer asserts BD to signal start of a horizontal line:
            The BD pulse is precisely timed to assure the left edge of
            the printed area lines up vertically;  the 't' value is
            measured from the falling edge of BD (assert LOW) to the
            first pixel from VDO.  BD timings are resolution dependent:
            
            240dpi: true for 16us +- 1us; edge-to-edge is 2246us +- 2%
            300dpi: true for 20us +- 1us; edge-to-edge is 1797us +- 2%
            
            't' values (for the left margin) are based on the table above.
            The PERQ seems to always assume Letter sized paper? :-(

        The controller uses the VDO line to send the bitstream:
            We'll simply wait for it to accumulate one scan line and
            ship it to us in one go?  Or at least one quad word at a time?

            The printer simply sets the current line of pixels in the
            bitmap and bumps its line counter; the controller computes
            the length of the page and deasserts PRN when done?  This
            part is muddy... :-(

            When we somehow determine that the page is complete, we
            write the bitmap into the filesystem using the appropriate
            formatter, then clear it for the next page

        To continue printing:
            The controller can set PRN high at almost any time after VSYNC
            starts the page; within 3.5s after VSREQ (4.6s Legal) is a
            don't care region... if PRN is deasserted during that time
            and is reasserted (even before the page is finished!?) then we
            set a standby timer (3.7s).  After page completion if PRN is
            reasserted we VSREQ again and printing continues without delay.

            If there are no more pages to print we deallocate the bitmap and
            return to standby mode.  This is badly diagrammed and confusing.
 */
