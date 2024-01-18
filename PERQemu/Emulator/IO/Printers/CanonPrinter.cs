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
using System.Text;
using System.IO;
using System.IO.Compression;

using SDL2;

using PERQmedia;

namespace PERQemu.IO
{
    /// <summary>
    /// Emulates the Canon LBP-10 (240dpi) and CX (300dpi) laser printers.
    /// These printers connect to a PERQ through the Canon "video" interface, a
    /// bitstream sent directly from the host to the laser marking engine (just
    /// like the NeXT laser printer, years later).
    /// </summary>
    /// <remarks>
    /// See the file Docs\LaserCanon.txt for implementation notes and details.
    /// </remarks>
    public partial class CanonPrinter
    {
        public CanonPrinter(CanonController ctrl, Scheduler sched)
        {
            _control = ctrl;
            _scheduler = sched;

            _cassette = Settings.CanonPaperSize;
            _resolution = Settings.CanonResolution;

            _printableArea = new SDL.SDL_Rect();

            LoadPaper(_cassette);
            LoadPageCount();
        }

        // Info
        public string Model => _resolution == 300 ? "LBP-CX" : "LBP-10";
        public uint PageCount => _totalPages;
        public PaperCode PaperType => _cassette;

        // Interface signals
        public bool PrinterPowerReady => true;          // PPRDY (NC)
        public bool VSyncRequest => _vsreq;             // VSREQ (I TOP L)
        public bool StatusBusy => _sbusy;               // SBSY (I PRND L)
        public bool Ready => _ready;                    // RDY (I RDY L)
        public bool Band => _bd;                        // BD (I BD)


        public void Reset()
        {
            // Clobber any print in progress
            if (_surface != IntPtr.Zero)
            {
                SDL.SDL_FreeSurface(_surface);
                _surface = IntPtr.Zero;
            }

            _pageBuffer = null;
            _lineCount = 0;
            _clocks = 0;

            // Clobber any outstanding events
            _scheduler.Cancel(_delayEvent);
            _delayEvent = null;

            _scheduler.Cancel(_sleepEvent);
            _sleepEvent = null;

            // Reset signals
            _bd = false;
            _ready = false;
            _vsreq = false;
            _sbusy = false;
            _standby = true;
            _running = false;
            _testPrint = false;
            _state = PrinterState.Standby;

            UpdateStatus();
            Log.Info(Category.Canon, "Printer reset");
        }

        /// <summary>
        /// Update internal status after a state change.
        /// </summary>
        void UpdateStatus()
        {
            var oldStat = _status;
            var oldOpStat = _operStatus;
            var oReady = _ready;

            bool _sensing = false;
            _status = 0;
            _operStatus = 0;

            // Update the Ready pin
            _ready = (_cassette != PaperCode.NoCassette) && (_pagesLeft > 0); // && (!_jammed) && (!_paused)

            if (_standby) _status |= Status0.Wait;
            if (_ready && _sensing) _status |= Status0.PrintRequest;
            if (_ready && _running) _status |= Status0.PaperDelivery;
            //if (_paused) _status |= Status0.Pause;

            if (_cassette == PaperCode.NoCassette) _operStatus |= Status1.NoCartridge;
            if (_pagesLeft == 0) _operStatus |= Status1.PaperOut;
            if (_testPrint) _operStatus |= Status1.TestPrinting;
            //if (_jammed) _operStatus |= Status1.PaperJam;

            if (_operStatus != 0) _status |= Status0.Call;

            if (_status != oldStat) Log.Info(Category.Canon, "Status change: {0}", _status);
            if (_operStatus != oldOpStat) Log.Info(Category.Canon, "Operator call: {0}", _operStatus);

            // Did it change?
            if (_ready != oReady)
            {
                // Clear our local state
                if (_state == PrinterState.Fault) RunStateMachine(_state);

                // If we were printing, tell the controller too
                if (_running) _control.SignalStateChange();
            }
        }

        /// <summary>
        /// Load paper into the printer.
        /// </summary>
        public void LoadPaper(PaperCode type, bool manualFeed = false)
        {
            switch (type)
            {
                case PaperCode.A4:
                case PaperCode.B5:
                case PaperCode.USLetter:
                case PaperCode.USLegal:
                    _cassette = type;
                    _pagesLeft = (uint)(manualFeed ? 1 : 100);

                    Log.Info(Category.Canon, "Paper tray loaded: {0}, ({1} pages left)", _cassette, _pagesLeft);
                    break;

                default:
                    _cassette = PaperCode.NoCassette;
                    _pagesLeft = 0;

                    Log.Info(Category.Canon, "Paper tray unloaded!");
                    break;
            }

            SetPageDimensions();
            UpdateStatus();
        }

        /// <summary>
        /// Set the page dimensions (in pixels) based on the current paper tray.
        /// </summary>
        /// <remarks>
        /// The source code all seems to assume US Letter, but maybe ICL tweaked
        /// the default to A4 instead?  The software also sets the maximum pixel
        /// dimensions to 9" x 11", but limits the printable area to 7.5" x ~10"
        /// (which also doesn't make much sense).  We'll support any of the four
        /// paper sizes listed in the Canon documentation, even if the current
        /// PERQ software can't use them all.
        /// </remarks>
        void SetPageDimensions()
        {
            switch (_cassette)
            {
                case PaperCode.USLetter:
                    _pageWidth = (int)(8.5 * _resolution);
                    _pageHeight = (int)(11 * _resolution);
                    _printableArea.w = (int)(8.19 * _resolution);   // 8.11 max
                    _printableArea.h = (int)(10.69 * _resolution);  // 10.82 max
                    break;

                case PaperCode.USLegal:
                    _pageWidth = (int)(8.5 * _resolution);
                    _pageHeight = (int)(14 * _resolution);
                    _printableArea.w = (int)(8.19 * _resolution);   // 8.11
                    _printableArea.h = (int)(13.69 * _resolution);  // 13.85
                    break;

                case PaperCode.A4:
                    _pageWidth = (int)(8.27 * _resolution);
                    _pageHeight = (int)(11.69 * _resolution);
                    _printableArea.w = (int)(8 * _resolution);
                    _printableArea.h = (int)(11.38 * _resolution);
                    break;

                case PaperCode.B5:
                    _pageWidth = (int)(6.93 * _resolution);
                    _pageHeight = (int)(9.84 * _resolution);
                    _printableArea.w = (int)(6.9 * _resolution);
                    _printableArea.h = (int)(9.8 * _resolution);
                    break;

                default:
                    _pageWidth = MaxWidth;
                    _pageHeight = MaxHeight;
                    _printableArea.w = _pageWidth;
                    _printableArea.h = _pageHeight;
                    break;
            }

            // Compute the upper left corner to center the result.  Eventually
            // I'll see some actual bitmaps and tweak these for best aesthetic
            // results.
            _printableArea.x = ((_pageWidth - _printableArea.w) / 2);
            _printableArea.y = ((_pageHeight - _printableArea.h) / 2);
        }

        /// <summary>
        /// Return the 't' value for left margin delay for the current setup.
        /// </summary>
        public ulong GetMarginDelay()
        {
            ulong delay;

            switch (_cassette)
            {
                case PaperCode.USLetter:
                case PaperCode.USLegal:
                    delay = (_resolution == 300) ? 108U : 135U;
                    break;

                case PaperCode.A4:
                    delay = (_resolution == 300) ? 145U : 182U;
                    break;

                case PaperCode.B5:
                    delay = (_resolution == 300) ? 323U : 404U;
                    break;

                default:
                    // Not relevant if there's no paper...
                    delay = 1U;
                    break;
            }

            Log.Debug(Category.Canon, "[Margin delay is {0}usec]", delay);
            return delay * Conversion.UsecToNsec;
        }

        /// <summary>
        /// Run the state machine in response to a signal change in the controller.
        /// </summary>
        public void SignalStateChange()
        {
            RunStateMachine(_state);
        }

        /// <summary>
        /// Runs the print engine state machine, much like the video controller.
        /// </summary>
        void RunStateMachine(PrinterState next)
        {
            ulong delay = 100;  // Default delay if debugging (or impatient)

            Log.Debug(Category.Canon, "[Printer SM  IN state={0} next={1}]", _state, next);

            switch (next)
            {
                case PrinterState.Standby:
                    //
                    // Printer is in "low power mode" where the mechanism is
                    // spun down, the laser off, and we're waiting for a new
                    // print command to wake us up again.  Assume we only are
                    // transitioned here after a power on or a timeout after
                    // the final page of a print run.  When PRNT is asserted,
                    // schedule the transition to Ready to start a new print.
                    //
                    _standby = true;

                    if (_control.PrintRequest)
                    {
                        if (Settings.Performance.HasFlag(RateLimit.PrinterSpeed))
                        {
                            // Actual warm up time is around 10 seconds, but we
                            // add up to 3.7 seconds in the Ready -> Start state
                            var rand = new Random();
                            delay = (ulong)rand.Next(3000, 6300);
                        }

                        Log.Info(Category.Canon, "[Printer warming up, starting in {0:n2} seconds]",
                                                 delay * Conversion.MsecToSec);
                        _delayEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, EngineStart);
                    }
                    break;

                case PrinterState.Ready:
                    //
                    // Here when ready to start or continue printing.
                    //
                    if (_control.PrintRequest)
                    {
                        // Don't get sleepy
                        if (_sleepEvent != null)
                        {
                            _scheduler.Cancel(_sleepEvent);
                            _sleepEvent = null;
                            Log.Info(Category.Canon, "[Printer sleep timer canceled]");
                        }

                        // Are we good to go?
                        if (!_running && _ready)
                        {
                            // Yep, set the flag and schedule the VSREQ transition
                            _running = true;

                            if (Settings.Performance.HasFlag(RateLimit.PrinterSpeed))
                            {
                                delay = 3500;
                            }
                            Log.Info(Category.Canon, "[Printer sending VSReq in {0:n2} seconds]",
                                                     delay * Conversion.MsecToSec);
                            _delayEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, SendVSReq);
                        }
                    }
                    else
                    {
                        // The controller has deasserted PRNT, so schedule an
                        // eventual return to Standby mode
                        if (_sleepEvent == null)
                        {
                            Log.Info(Category.Canon, "[Printer will sleep in 20 seconds]");
                            _sleepEvent = _scheduler.Schedule(20000 * Conversion.MsecToNsec, Standby);
                        }
                    }
                    break;

                case PrinterState.WaitForVSync:
                    //
                    // This always succeeds, since the PERQ just loops VSReq->VSync
                    // in the cable.  No microcode or state machine intervention.
                    //
                    if (_control.VSync)
                    {
                        Log.Debug(Category.Canon, "[Printer received VSync]");

                        // At this point, we've fed paper into the machine! :-)
                        _pagesLeft--;

                        // Turn off VSReq and start our BD clock
                        _delayEvent = _scheduler.Schedule(1 * Conversion.MsecToNsec, StartPage);
                        next = PrinterState.Printing;
                    }
                    break;

                case PrinterState.Printing:
                    //
                    // We don't actually have anything to do here; the BD clock
                    // fires to tell the controller to send us data, which it does
                    // directly through calls to PrintBlank/PrintLine.  We remain
                    // in this state until the end of page is reached, at which
                    // point the BD clock stops and the EndOfPage transition is
                    // triggered.  Goofy as hell but more efficient that way.
                    //
                    if (_lineCount > _printableArea.h && !_blanking)
                    {
                        // Debugging - warn if the PERQ has sent more data than
                        // we think we have room for
                        Log.Info(Category.Canon, "Scribbling outside the lines: {0} > {1}",
                                                 _lineCount, _printableArea.h);
                    }
                    break;

                case PrinterState.Fault:
                    //
                    // The only possibility is that we've removed the cassette or
                    // run out of paper; we don't emulate jams or other mechanical
                    // malfunctions.  So just loop here until the fault is cleared.
                    //
                    if (_ready)
                    {
                        // We're back!
                        next = PrinterState.Ready;  // or... Reset?
                    }
                    break;

                case PrinterState.EndOfPage:
                    //
                    // At this point, we actually process the bitmap and save the
                    // output.  I guess.  Makin' it up as I go.
                    //
                    if (_running)
                    {
                        Log.Info(Category.Canon, "[Printer at EOP!]");
                        _running = false;
                        _ready = false;
                        _sbusy = true;
                        UpdateStatus();
                        _control.SignalStateChange();

                        // Go process the bitmap and save the file
                        SavePage();

                        // Take a breather
                        _delayEvent = _scheduler.Schedule(delay, (skewNsec, context) =>
                        {
                            // And do it all again
                            _sbusy = false;
                            _ready = true;
                            UpdateStatus();
                            _control.SignalStateChange();
                            RunStateMachine(PrinterState.Ready);
                        });
                    }
                    break;

                default:
                    throw new InvalidOperationException("Unknown PrinterState or invalid transition");
            }

            _state = next;
            Log.Debug(Category.Canon, "[Printer SM OUT state={0}]", _state);
        }

        /// <summary>
        /// Start a new page.
        /// </summary>
        public void StartPage(ulong skewNsec, object context)
        {
            // Clear VSREQ and check our status
            _vsreq = false;
            UpdateStatus();

            if (!_ready)
            {
                // PRINT requests are ignored if we aren't ready...
                Log.Info(Category.Canon, "[Printer not ready, can't start page]");

                RunStateMachine(PrinterState.Fault);    // fixme
                return;
            }

            Log.Info(Category.Canon, "[Printer starting page]");

            // Initialize the bitmap
            _pageBuffer = new byte[ScanWidthInBytes * MaxHeight];   // 1bpp version

            _lineCount = 0;
            SetPageDimensions();

            // Start the line clock.  To reliably determine the end of a page,
            // we count down the maximum number of BD pulses based on the
            // paper length (loosely); the rated print speed is 8.1ppm for
            // Letter (and, presumably, A4 and B5) and 6.9ppm for Legal.  This
            // is as good a method as any, even if it seems a total hack.
            _clocks = _pageHeight;
            SendBD(0, null);
        }

        /// <summary>
        /// Print a blank line.
        /// </summary>
        public void PrintBlank()
        {
            Log.Info(Category.Canon, "Blanking (line={0})", _lineCount);
            _lineCount++;
            _blanking = true;
            RunStateMachine(PrinterState.Printing);
        }

        /// <summary>
        /// Print a line of pixels into the page buffer.  In BYTES.
        /// </summary>
        public void PrintLine(int margin, int width, byte[] data)
        {
            // Compute starting point in bytes
            var addr = (ScanWidthInBytes * _lineCount) + margin;

            Log.Info(Category.Canon, "Printing (line={0} @ {1}, left={2}, width={3})",
                                     _lineCount, addr, margin, width);

            Array.Copy(data, 0, _pageBuffer, addr, width);

            _lineCount++;
            _blanking = false;
            RunStateMachine(PrinterState.Printing);
        }

        /// <summary>
        /// Save the completed page to the Output directory in the preferred
        /// image format.  Currently one page per file.
        /// </summary>
        public void SavePage()
        {
            // Build the filename
            var ext = Paths.GetExtensionForImageFormat(Settings.CanonFormat);
            var filename = string.Format(Settings.CanonTemplate, Model, _totalPages, ext);

            // Save the page in the specified format
            switch (Settings.CanonFormat)
            {
                case ImageFormat.None:
                case ImageFormat.Jpeg:
                    // not implemented
                    break;

                case ImageFormat.Png:
                    if (SavePageAsPNG(filename))
                    {
                        Log.Write(Category.Canon, "Saved PNG output to '{0}'.", filename);
                        _totalPages++;
                    }
                    break;

                case ImageFormat.Tiff:
                    if (SavePageAsTIFF(filename))
                    {
                        Log.Write(Category.Canon, "Saved TIFF output to '{0}'.", filename);
                        _totalPages++;
                    }
                    break;

                case ImageFormat.Raw:
                    SavePageAsRaw(filename);
                    Log.Write(Category.Canon, "Saved raw output to '{0}'.", filename);
                    break;

                default:
                    throw new InvalidOperationException("Unknown or unimplemented Canon format");
            }
        }

        /// <summary>
        /// Come out of standby: spin the drum and scanner, warm the fixer, ready the laser!
        /// </summary>
        public void EngineStart(ulong skewNsec, object context)
        {
            Log.Info(Category.Canon, "[Printer is warmed up!]");

            _standby = false;
            _delayEvent = null;
            UpdateStatus();
            RunStateMachine(PrinterState.Ready);
        }

        /// <summary>
        /// Return to standby mode to save power. :-)
        /// </summary>
        public void Standby(ulong skewNsec, object context)
        {
            Log.Info(Category.Canon, "[Printer in standby mode]");

            _standby = true;
            _sleepEvent = null;
            UpdateStatus();
            RunStateMachine(PrinterState.Standby);
        }

        /// <summary>
        /// Raise the VSREQ line to indicate the printer is ready to start printing.
        /// </summary>
        public void SendVSReq(ulong skewNsec, object context)
        {
            Log.Debug(Category.Canon, "[Printer sending VSReq]");

            _vsreq = true;
            _control.SignalStateChange();
            RunStateMachine(PrinterState.WaitForVSync);
        }

        /// <summary>
        /// Toggle the BD signal (horizontal line clock).  When asserted, alerts
        /// the controller to start the next line.
        /// </summary>
        public void SendBD(ulong skewNsec, object context)
        {
            ulong delay;

            _bd = !_bd;

            if (_bd)
            {
                _clocks--;

                // Stop the clock?
                if (_clocks <= 0 || !_ready || _sbusy)
                {
                    Log.Info(Category.Canon, "[Printer stopping BD clock]");
                    _bd = false;
                    _delayEvent = null;

                    // Kick the state machine to process the page
                    RunStateMachine(PrinterState.EndOfPage);
                    return;
                }

                // Clock running: signal the controller
                Log.Debug(Category.Canon, "[Printer sending BD ({0})]", _clocks);
                _control.SignalStateChange();

                // BD pulse width (in usec)
                delay = (_resolution == 300) ? 16U : 20U;
            }
            else
            {
                // Microseconds between BD pulses
                delay = (_resolution == 300) ? 1781U : 2226U;
            }

            // Schedule the next transition
            _delayEvent = _scheduler.Schedule(delay * Conversion.UsecToNsec, SendBD);
        }


        /// <summary>
        /// Loads the saved page count for this printer model.
        /// </summary>
        public void LoadPageCount()
        {
            try
            {
                using (var fs = new FileStream(Paths.BuildResourcePath($"{Model}.dat"),
                                               FileMode.Open, FileAccess.Read))
                {
                    // Format:  one (binary) unsigned integer for total number of pages
                    // printed successfully.  Used to generate the next output file name
                    // when the default format is used.  This is total cheese.
                    _totalPages = fs.ReadUInt();
                    fs.Close();
                }
                Log.Info(Category.Canon, "Page counter loaded ({0})", _totalPages);
            }
            catch (Exception e)
            {
                Log.Info(Category.Canon, "Page counter initialized ({0})", e.Message);
                _totalPages = 0;
            }
        }

        /// <summary>
        /// Saves the page count for this printer.
        /// </summary>
        public void SavePageCount()
        {
            try
            {
                using (var fs = new FileStream(Paths.BuildResourcePath($"{Model}.dat"),
                                               FileMode.OpenOrCreate, FileAccess.Write))
                {
                    fs.WriteUInt(_totalPages);
                    fs.Close();
                }
                Log.Info(Category.Canon, "Page counter saved");
            }
            catch (Exception e)
            {
                Log.Info(Category.Canon, "Page counter NOT saved: {0}", e.Message);
            }
        }

        // Debugging
        public void DumpStatus()
        {
            Console.WriteLine($"Canon {Model} printer state:  {_state}");
            Console.WriteLine($"  Resolution: {_resolution}dpi  Pages printed: {_totalPages}");
            Console.WriteLine($"  Paper tray: {_cassette}  Sheets: {_pagesLeft}");
            Console.WriteLine($"  Status 0:   {_status}");
            if (_operStatus != 0)
                Console.WriteLine($"  Status 1:   {_operStatus}");
            Console.WriteLine();
            Console.WriteLine($"Interface:  Standby={_standby} RDY={_ready} VSREQ={_vsreq} BD={_bd}");
            if (_lineCount > 0 && _clocks > 0)
                Console.WriteLine($"[Printing at line {_lineCount} (clock={_clocks})]");
        }

        //
        // Largest bitmap the PERQ could theoretically send us.  This is based
        // on the sources (Pascal, microcode) and they don't really make sense;
        // At 300dpi, this is a 9" x 11" bitmap, which is wider than any of the
        // supported paper types but not long enough for USLegal.  Since there
        // is no evidence yet found for PERQ software that could switch paper
        // sizes (or query the printer to see what cassette was loaded) this is
        // fine for now.
        //
        public const int MaxWidth = 2704;           // Round up to multiple of 8 bits
        public const int MaxHeight = 3300;

        public const int ScanWidthInBytes = MaxWidth / 8;  // Used a lot

        CanonController _control;
        Scheduler _scheduler;

        PrinterState _state;
        Status0 _status;
        Status1 _operStatus;
        PaperCode _cassette;

        uint _resolution;

        // Signals
        bool _ready;
        bool _vsreq;
        bool _sbusy;
        bool _bd;

        bool _standby;
        bool _running;
        bool _blanking;
        bool _testPrint;

        uint _pagesLeft;
        uint _totalPages;

        int _lineCount;
        int _clocks;

        int _pageWidth;
        int _pageHeight;
        byte[] _pageBuffer;
        IntPtr _surface;
        SDL.SDL_Rect _printableArea;

        SchedulerEvent _delayEvent;
        SchedulerEvent _sleepEvent;
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
    [Flags]
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
    [Flags]
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
        A4 = 0x01,
        USLetter = 0x04,
        B5 = 0x09,
        USLegal = 0x0c
    }

    /// <summary>
    /// Printer state machine (simplified).
    /// </summary>
    enum PrinterState
    {
        Standby = 0,            // Power saving mode :-)
        Ready,                  // Idle, waiting for print command
        WaitForVSync,           // Waiting to start (top of page)
        Printing,               // Receiving the image data
        EndOfPage,              // Render and save completed image
        Fault,                  // Ready dropped during printing
        Reset                   // Power cycle/fault clear
    }
}
