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
using System.IO;
using System.Collections.Generic;

using PERQmedia;
using PERQemu.UI;
using PERQemu.UI.Output;

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

            _maxArea = new Region(0, 0, MaxWidth, MaxHeight);

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
            _pageBuffer = null;
            _pageCount = 0;
            _lineCount = 0;
            _clocks = 0;

            // Clobber any outstanding events
            _scheduler.Cancel(_delayEvent);
            _delayEvent = null;

            _scheduler.Cancel(_sleepEvent);
            _sleepEvent = null;

            _scheduler.Cancel(_animEvent);
            _animEvent = null;

            // Clear the icon, warning
            _showIcon = false;
            _resolutionWarning = false;

            // Reset signals
            _bd = false;
            _ready = false;
            _vsreq = false;
            _sbusy = false;
            _running = false;

            // Reset status and state
            _state = PrinterState.Standby;
            _status = _lastStatus = Status0.Wait;
            _operStatus = _lastOperStatus = 0;

            UpdateStatus();
            UpdateActivityIcon();
            Log.Info(Category.Canon, "Printer reset");
        }

        /// <summary>
        /// Update internal status after a state change.
        /// </summary>
        void UpdateStatus()
        {
            var oReady = _ready;

            // Update the Ready pin
            _ready = (_cassette != PaperCode.NoCassette) && (_pagesLeft > 0);

            // Status updates if Ready changed?
            if (!_ready && _status.HasFlag(Status0.PaperDelivery))
            {
                _status &= ~Status0.PaperDelivery;
            }

            // Operator status changed since last check?
            if (_operStatus == 0 && _status.HasFlag(Status0.Call))
            {
                _status &= ~Status0.Call;
            }
            else if (_operStatus != 0)
            {
                _status |= Status0.Call;
            }

            if (_status != _lastStatus) Log.Debug(Category.Canon, "Printer status change: {0}", _status);
            if (_operStatus != _lastOperStatus) Log.Info(Category.Canon, "Operator call: {0}", _operStatus);

            _lastStatus = _status;
            _lastOperStatus = _operStatus;

            // React to Ready change
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
                    _pagesLeft = (uint)(manualFeed ? 1 : 100);
                    _cassette = type;
                    _operStatus &= ~Status1.PaperOut;

                    Log.Info(Category.Canon, "Paper tray loaded: {0}, ({1} pages left)", _cassette, _pagesLeft);
                    break;

                default:
                    _pagesLeft = 0;
                    _cassette = PaperCode.NoCassette;
                    _operStatus |= Status1.PaperOut;

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
                    _pageArea.W = (uint)(8.5 * _resolution);
                    _pageArea.H = (uint)(11 * _resolution);
                    _printableArea.W = (uint)(8.19 * _resolution);
                    _printableArea.H = (uint)(10.86 * _resolution);
                    break;

                case PaperCode.USLegal:
                    _pageArea.W = (uint)(8.5 * _resolution);
                    _pageArea.H = (uint)(14 * _resolution);
                    _printableArea.W = (uint)(8.19 * _resolution);
                    _printableArea.H = (uint)(13.82 * _resolution);
                    break;

                case PaperCode.A4:
                    _pageArea.W = (uint)(8.27 * _resolution);
                    _pageArea.H = (uint)(11.69 * _resolution);
                    _printableArea.W = (uint)(8 * _resolution);
                    _printableArea.H = (uint)(11.38 * _resolution);
                    break;

                case PaperCode.B5:
                    _pageArea.W = (uint)(6.93 * _resolution);
                    _pageArea.H = (uint)(9.84 * _resolution);
                    _printableArea.W = (uint)(6.9 * _resolution);
                    _printableArea.H = (uint)(9.8 * _resolution);
                    break;

                default:
                    _pageArea.W = MaxWidth;
                    _pageArea.H = MaxHeight;
                    _printableArea.W = _pageArea.W;
                    _printableArea.H = _pageArea.H;
                    break;
            }

            // Compute the upper left corner to center the result.  Eventually
            // I'll see some actual bitmaps and tweak these for best aesthetic
            // results.
            _pageArea.X = ((_pageArea.W - _printableArea.W) / 2);
            _pageArea.Y = ((_pageArea.H - _printableArea.H) / 2);
            _printableArea.X = 0;
            _printableArea.Y = 0;
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

            Log.Detail(Category.Canon, "[Margin delay is {0}usec]", delay);
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

            Log.Detail(Category.Canon, "[Printer SM  IN state={0} next={1}]", _state, next);

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
                    _status |= Status0.Wait;

                    if (_control.PrintRequest)
                    {
                        if (Settings.Performance.HasFlag(RateLimit.PrinterSpeed))
                        {
                            // Actual warm up time is around 5 seconds, for the
                            // INTR (initial drum rotation) period.  (To show that
                            // something is happening, blink the activity icon :-)
                            delay = 5000;
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
                    // Unless we aren't.  Trust, but verify. :-)
                    UpdateStatus();

                    if (!_ready)
                    {
                        next = PrinterState.Fault;
                    }
                    else if (_control.PrintRequest)
                    {
                        // Don't get sleepy
                        if (_sleepEvent != null)
                        {
                            _scheduler.Cancel(_sleepEvent);
                            _sleepEvent = null;
                            Log.Debug(Category.Canon, "[Printer sleep timer canceled]");
                        }

                        // Are we good to go?
                        if (!_running)
                        {
                            // Yep, set the flag and start the clock
                            _running = true;
                            _status |= Status0.PaperDelivery;

                            next = PrinterState.Starting;

                            // Ugh. Don't ask. :-|
                            _lineCount = (_resolution == 300) ? -72 : -312;
                            _clocks = (int)((Math.Abs(_lineCount) + _pageArea.H + 7) / 8) * 8;

                            // Minimum delay based on top margin approximation if not rate limiting
                            delay = Settings.Performance.HasFlag(RateLimit.PrinterSpeed) ? 3500U :
                                    (_resolution == 300) ? 145U : 665U;

                            Log.Debug(Category.Canon, "[Printer starting clock in {0:n2} seconds]",
                                                     delay * Conversion.MsecToSec);
                            _delayEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, SendVSReq);
                        }
                    }
                    else
                    {
                        // The controller has deasserted PRNT, so schedule the
                        // return to Standby mode after LSTR (last rotation)
                        if (_sleepEvent == null)
                        {
                            delay = Settings.Performance.HasFlag(RateLimit.PrinterSpeed) ? 5700U : 2000U;

                            Log.Debug(Category.Canon, "[Printer will sleep in {0:N2} seconds]", delay / 1000.0);
                            _sleepEvent = _scheduler.Schedule(delay * Conversion.MsecToNsec, Standby);
                        }
                    }
                    break;

                case PrinterState.Starting:
                    //
                    // Loop for the mechanical delay between PRNT -> VSREQ to allow
                    // for paper feed and the start of the BD clock.  Although the
                    // printer starts the BD clock *before* VSREQ, the PERQ state
                    // machine ignores them until after the VSREQ/VSYNC handshake.
                    //
                    if (_vsreq)
                    {
                        Log.Debug(Category.Canon, "[Printer starting]");
                        next = PrinterState.WaitForVSync;
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
                        _status &= ~Status0.PaperDelivery;

                        // Clear VSREQ and check our status
                        _vsreq = false;
                        UpdateStatus();

                        if (!_ready)
                        {
                            // PRINT requests are ignored if we aren't ready... in the
                            // emulator, this could only really happen if the paper tray
                            // is yanked out (i.e., replaced by NoCassette)
                            Log.Debug(Category.Canon, "[Printer not ready, can't start page]");
                            _running = false;
                            next = PrinterState.Fault;
                        }
                        else
                        {
                            StartPage();
                            next = PrinterState.Printing;
                        }
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
                    if (_lineCount > _printableArea.H && !_blanking)
                    {
                        // Debugging - warn if the PERQ has sent more data than
                        // we think we have room for
                        Log.Debug(Category.Canon, "Scribbling outside the lines: {0} > {1}",
                                                 _lineCount, _printableArea.H);
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
                        Log.Debug(Category.Canon, "[Printer at EOP!]");
                        _bd = false;
                        _running = false;
                        _sbusy = true;
                        UpdateStatus();
                        _control.SignalStateChange();

                        // Go process the bitmap and save the file
                        if (Settings.CanonFormat == ImageFormat.Tiff)
                        {
                            AddPage();      // Queue it up
                        }
                        else
                        {
                            SavePage();     // Write it out
                        }

                        // Take a breather
                        _delayEvent = _scheduler.Schedule(delay * Conversion.UsecToNsec, (skewNsec, context) =>
                        {
                            // And do it all again
                            _sbusy = false;
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
            Log.Detail(Category.Canon, "[Printer SM OUT state={0}]", _state);

            UpdateActivityIcon();
        }

        /// <summary>
        /// Come out of standby: spin the drum and scanner, warm the fixer, ready the laser!
        /// </summary>
        void EngineStart(ulong skewNsec, object context)
        {
            Log.Debug(Category.Canon, "[Printer is warmed up!]");

            _delayEvent = null;
            _status &= ~Status0.Wait;
            _status |= Status0.PrintRequest;
            UpdateStatus();
            RunStateMachine(PrinterState.Ready);
        }

        /// <summary>
        /// Start a new page.
        /// </summary>
        void StartPage()
        {
            Log.Debug(Category.Canon, "[Printer starting page]");

            // Initialize the bitmap
            _pageBuffer = new byte[ScanWidthInBytes * MaxHeight];   // 1bpp version

            _pagesLeft--;
            SetPageDimensions();

            // For TIFF, set up the list in case we're doing multiple pages
            if (Settings.CanonFormat == ImageFormat.Tiff && _pageCount == 0)
            {
                _pageList = new List<Page>();
            }
        }

        /// <summary>
        /// Print a blank line.
        /// </summary>
        public void PrintBlank()
        {
            Log.Detail(Category.Canon, "Blanking (line={0})", _lineCount);
            _lineCount++;
            _blanking = true;
            RunStateMachine(_state);
        }

        /// <summary>
        /// Print a line of pixels into the page buffer.  In BYTES.
        /// </summary>
        public void PrintLine(int margin, int width, byte[] data)
        {
            // Compute starting point in bytes
            var addr = (ScanWidthInBytes * _lineCount) + margin - 1;

#if DEBUG
            if (_state != PrinterState.Printing || _lineCount < 0 || _lineCount >= _pageArea.H)
            {
                Log.Warn(Category.Canon, "PrintLine in state {0} at line {1} (clock {2})!?",
                                         _state, _lineCount, _clocks);
                return;
            }

            if (_blanking)
            {
                Log.Detail(Category.Canon, "End of blanking at line {0} (clock {1})", _lineCount, _clocks);
            }
#endif
            // Guard rails to prevent a crash if configuration/client software mismatched
            if (addr < 0 || addr - width > _pageBuffer.Length)
            {
                if (!_resolutionWarning)
                {
                    Log.Write(Severity.Warning, Category.All,
                              "Canon: Attempting to print outside the page boundary!\n" +
                              "Does the printer resolution match your CPrint configuration?");

                    // Keep the clock running so the CX doesn't hang
                    _clocks = 3300;     // Hardcoded in the PERQ microcode :-(
                    _resolutionWarning = true;
               }

                // Reset to valid range, though the output will be whacked
                if (addr < 0)
                {
                    addr = 0;
                    _lineCount = -1;    // skip ahead
                }
                else
                {
                    addr = _pageBuffer.Length - width;
                    _lineCount--;       // don't advance
                }
            }

            Log.Detail(Category.Canon, "Printing (line={0} @ {1}, left={2}, width={3})",
                                     _lineCount, addr, margin, width);

            Array.Copy(data, 0, _pageBuffer, addr, width);

            _lineCount++;
            _blanking = false;
            RunStateMachine(_state);
        }

        /// <summary>
        /// Adds the page to the list for multi-page TIFF output.
        /// </summary>
        void AddPage()
        {
            // Copy, crop and save the page (TIFF starts numbering at 0)
            var page = new Page(_resolution, _pageArea, _pageCount);
            page.CopyBits(_pageBuffer, _maxArea, _printableArea);

            _pageList.Add(page);
            _pageCount++;

            Log.Debug(Category.Canon, "[Saved page {0} to list]", _pageCount);
        }

        /// <summary>
        /// Save the completed page to the Output directory in the preferred
        /// image format.  Currently one page per file.
        /// </summary>
        void SavePage()
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
                    if (SaveAsPNG(filename))
                    {
                        Log.Write("Saved PNG output to '{0}'.", filename);
                        _totalPages++;
                    }
                    break;

                case ImageFormat.Tiff:
                    if (SaveAsTIFF(filename))
                    {
                        Log.Write("Saved TIFF output to '{0}' ({1}).", filename,
                                  _pageCount == 1 ? "1 page" : $"{_pageCount} pages");
                        _totalPages += _pageCount;
                        _pageCount = 0;
                        _pageList = null;
                    }
                    break;

                case ImageFormat.Raw:
                    SaveAsRaw(filename);
                    Log.Write(Category.Canon, "Saved raw output to '{0}'.", filename);
                    break;

                default:
                    throw new InvalidOperationException("Unknown or unimplemented Canon format");
            }
        }

        /// <summary>
        /// Return to standby mode to save power and mechanical wear & tear. :-)
        /// </summary>
        void Standby(ulong skewNsec, object context)
        {
            // If a multi-page output is pending, write it out now.  This extra
            // delay isn't ideal, but I haven't yet come up with a reasonable
            // way to determine when the PERQ is done with a document...
            if (Settings.CanonFormat == ImageFormat.Tiff && _pageCount > 0)
            {
                SavePage();
            }

            Log.Info(Category.Canon, "[Printer in standby mode]");

            _resolutionWarning = false;
            _showIcon = false;
            _sleepEvent = null;
            _status = Status0.Wait;     // Clear any other flags
            UpdateStatus();
            RunStateMachine(PrinterState.Standby);
        }


        /// <summary>
        /// Raise the VSREQ line to indicate the printer is ready to print,
        /// then start the BD clock.
        /// </summary>
        void SendVSReq(ulong skewNsec, object context)
        {
            if (!_control.PrintRequest)
            {
                // PRNT must remain asserted until VSREQ turns it off, so
                // if the controller reset or cancelled the request we bail!
                _running = false;
                RunStateMachine(PrinterState.Ready);
            }
            else
            {
                Log.Debug(Category.Canon, "[Printer sending VSReq]");

                _vsreq = true;
                _control.SignalStateChange();
                RunStateMachine(_state);

                // Start the horizontal clock!
                SendBD(0, null);
            }
        }

        /// <summary>
        /// Toggle the BD signal (horizontal line clock).  When asserted, alerts
        /// the controller to start the next line.
        /// </summary>
        void SendBD(ulong skewNsec, object context)
        {
            ulong delay;

            if (_bd)
            {
                // Have we run off the end of the page?  The CX microcode counts
                // a maximum of 3300 lines (hardcoded!) before it transitions to
                // EOP, while the LBP10 version just stops sending data whenever
                // it runs out of lines to print... so _lineCount won't increment
                // if the controller leaps ahead to waiting for the next VSReq
                // signal.  Sigh.  Running the clock until we've gone off the
                // _pageArea limit seems to be the only reasonable compromise.
                _clocks--;

                if (!_ready || _clocks <= 0)
                {
                    Log.Debug(Category.Canon, "[Printer stopping BD clock at line {0} (clock {1})]", _lineCount, _clocks);
                    _bd = false;

                    // "Accurate" mode means the clock actually runs for 6.2 seconds
                    // after VSync; printing takes ~5.94s at nominal BD pulse rate
                    delay = Settings.Performance.HasFlag(RateLimit.PrinterSpeed) ? 260000U : 1000U;

                    // Kick the state machine to process the page
                    _delayEvent = _scheduler.Schedule(delay * Conversion.UsecToNsec, (skew, ctxt) =>
                        {
                            RunStateMachine(PrinterState.EndOfPage);
                        });
                    return;
                }

                // Clock running: signal the controller
                Log.Detail(Category.Canon, "[Printer sending BD]");
                _control.SignalStateChange();

                // BD pulse width (in usec)
                delay = (_resolution == 300) ? 16U : 20U;
            }
            else
            {
                // Microseconds between BD pulses
                delay = (_resolution == 300) ? 1781U : 2226U;
            }

            // Toggle now so we start false and count properly
            _bd = !_bd;

            // Schedule the next transition
            _delayEvent = _scheduler.Schedule(delay * Conversion.UsecToNsec, SendBD);
        }

        /// <summary>
        /// Update the activity icon based on our current state (doing a little
        /// animation to show progress, since printing is kinda slow in real time).
        /// </summary>
        /// <remarks>
        /// Limits the rate of visual updates so the animations aren't too frenetic,
        /// in emulator time (not real time).  Will vary according to the speed of
        /// the host and the rate limit settings, but tuned for around 1/3rd sec at
        /// the nominal 60Hz display refresh.
        /// </remarks>
        void UpdateActivityIcon()
        {
            var elapsed = _scheduler.CurrentTimeNsec - _lastIconChange;
            var nextIcon = _lastIconShown;
            var wasShown = _showIcon;
            var doAnother = false;

            switch (_state)
            {
                case PrinterState.Standby:
                    //
                    // For the 5 second warm up time, blink the icon to show the
                    // printer is coming on-line.  Otherwise, it remains off.
                    //
                    if (_control.PrintRequest)
                    {
                        nextIcon = Display.PRINT_READY;
                        _showIcon = !_showIcon;
                        doAnother = true;
                    }
                    else
                    {
                        wasShown = true;    // Force it!
                        _showIcon = false;
                        _animEvent = null;
                    }
                    break;

                case PrinterState.Ready:
                case PrinterState.Starting:
                case PrinterState.WaitForVSync:
                case PrinterState.EndOfPage:
                    //
                    // When ready to print, turn on the plain icon (no blink).
                    //
                    _showIcon = true;
                    nextIcon = Display.PRINT_READY;
                    _animEvent = null;
                    break;

                case PrinterState.Printing:
                    //
                    // While actively printing animate the icon to show we're working!
                    // I'm just gonna rely on the fact that the PRINT_P1..P3 icons
                    // are numbered sequentially, and this is so bad, but la la la la la
                    // Gepetto promises it'll all be rolled into a Real GUI someday
                    //
                    if (!_showIcon || elapsed > ANIM_RATE)
                    {
                        _showIcon = true;
                        nextIcon++;
                        if (nextIcon == Display.PRINT_ALERT) nextIcon = Display.PRINT_P1;
                    }
                    break;

                case PrinterState.Fault:
                    //
                    // Ruh roh.  Blink the attention icon until the fault clears.
                    //
                    if (_lastIconShown != Display.PRINT_ALERT || !_showIcon)
                    {
                        _showIcon = true;
                        nextIcon = Display.PRINT_ALERT;
                    }
                    else
                    {
                        nextIcon = Display.PRINT_READY;
                    }
                    doAnother = true;
                    break;
            }

            // Anything changed?
            if (nextIcon != _lastIconShown || wasShown != _showIcon)
            {
                // Update & send it
                _lastIconShown = nextIcon;
                _lastIconChange = _scheduler.CurrentTimeNsec;
                PERQemu.Sys.MachineStateChange(WhatChanged.PrinterActivity, _showIcon, _lastIconShown);
            }

            // Do another "frame" of animation?
            if (_animEvent == null && doAnother)
            {
                _animEvent = _scheduler.Schedule(ANIM_RATE, AnimationStep);
            }
        }

        void AnimationStep(ulong skewNsec, object context)
        {
            _animEvent = null;
            UpdateActivityIcon();
        }

        /// <summary>
        /// Loads the saved page count for this printer model.
        /// </summary>
        void LoadPageCount()
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
                Log.Debug(Category.Canon, "Page counter loaded ({0})", _totalPages);
            }
            catch (Exception e)
            {
                Log.Debug(Category.Canon, "Page counter initialized ({0})", e.Message);
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
                Log.Debug(Category.Canon, "Page counter saved");
            }
            catch (Exception e)
            {
                Log.Debug(Category.Canon, "Page counter NOT saved: {0}", e.Message);
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
            Console.WriteLine($"Interface:  RDY={_ready} VSREQ={_vsreq} BD={_bd}");
            if (_running && _lineCount > 0)
                Console.WriteLine($"[Printing at line {_lineCount}]");
#if DEBUG
            Console.WriteLine($"Sugar:  Animation rate is {ANIM_RATE}");
            Console.WriteLine("  Icon {0} {1} visible, animation {2} running, last change {3}",
                              _lastIconShown,
                              _showIcon ? "IS" : "is NOT",
                              _animEvent != null ? "IS" : "is NOT",
                              _lastIconChange);
#endif
        }

        //
        // Largest bitmap the PERQ could theoretically send us is based on the
        // dimensions of a 9" x 11" bitmap, hardcoded into the sources (Pascal,
        // microcode).  This is wider than any of the supported paper types but
        // not long enough for A4 or USLegal.  Although there is no evidence yet
        // found of PERQ software that could switch paper sizes (or query the
        // printer to see what cassette was loaded), the max here allows for the
        // Page formtter to output ALL the paper types, even if the PERQ never
        // prints anything bigger than US Letter.  Yet.
        //
        public const int MaxWidth = 2704;       // Round up to multiple of 8 bits
        public const int MaxHeight = 4200;      // To cover US Legal...

        public const int ScanWidthInBytes = MaxWidth / 8;  // Used a lot

        CanonController _control;
        Scheduler _scheduler;

        PrinterState _state;
        Status0 _status;
        Status0 _lastStatus;
        Status1 _operStatus;
        Status1 _lastOperStatus;
        PaperCode _cassette;

        // Signals
        bool _ready;
        bool _vsreq;
        bool _sbusy;
        bool _bd;

        bool _running;
        bool _blanking;

        int _lineCount;
        int _clocks;

        uint _pagesLeft;
        uint _totalPages;

        uint _resolution;
        bool _resolutionWarning;

        byte[] _pageBuffer;
        ushort _pageCount;

        Region _maxArea;
        Region _pageArea;
        Region _printableArea;

        List<Page> _pageList;

        SchedulerEvent _delayEvent;
        SchedulerEvent _sleepEvent;
        SchedulerEvent _animEvent;

        // Some visual sugar
        bool _showIcon;
        int _lastIconShown;
        ulong _lastIconChange;

        ulong ANIM_RATE = 333 * Conversion.MsecToNsec;    // Tune me
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
    //  Status 1 flags NoCartridge and PaperJam are for completeness only; we
    //  don't simulate jams or missing/improperly installed toner cartridges. :-)
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
        Starting,               // BD clock started (top margin)
        WaitForVSync,           // Waiting to start (top of page)
        Printing,               // Receiving the image data
        EndOfPage,              // Render and save completed image
        Fault                   // Ready dropped during printing
    }
}
