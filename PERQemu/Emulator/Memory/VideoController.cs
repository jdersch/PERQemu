//
// VideoController.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
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
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

using PERQemu.IO;
using PERQemu.Config;
using PERQemu.Processor;

namespace PERQemu.Memory
{
    /// <summary>
    /// A performance cheat: provide a fixed size memory buffer for one video
    /// scanline, accessible as 64-bit quad words or 8-bit bytes (for doing
    /// fast copies when not cursor mixing, and byte-wide/unaligned copies
    /// when the cursor is enabled).
    /// </summary>
    [StructLayout(LayoutKind.Explicit)]
    struct ScanLineBuffer
    {
        [FieldOffset(0)]
        public ulong[] Quads;

        [FieldOffset(0)]
        public byte[] Bytes;
    }

    /// <summary>
    /// Runs the PERQ's video controller: implements the control and status
    /// registers that the microcode uses to drive the display, manages the
    /// video output stream and cursor positioning, and generates "line count"
    /// interrupts that drive things like the system (software) clock.  See
    /// the file Docs/Video.txt for more details than you care to know.
    /// </summary>
    public sealed class VideoController : IIODevice
    {

        public VideoController(PERQSystem system)
        {
            _system = system;
            _currentEvent = null;

            // Set up our display based on the current configuration
            if (_system.Config.Display == DisplayType.Landscape)
            {
                _displayWidth = 1280;
                _isPortrait = false;
            }
            else
            {
                _displayWidth = 768;
                _isPortrait = true;
            }

            // Compute these once
            _displayQuads = _displayWidth / 64;
            _displayBytes = _displayWidth / 8;

            // One scanline
            _scanlineData = new ScanLineBuffer { Bytes = new byte[_displayBytes] };

            // One quadword as 8 bytes
            _cursorData = new byte[8];
        }

        public void Reset()
        {
            _state = VideoState.Idle;
            _crtSignals = CRTSignals.None;
            _videoStatus = StatusRegister.None;
            _scanLine = 0;
            _displayAddress = 0;
            _cursorAddress = 0;
            _cursorX = 0;
            _cursorY = 0;
            _cursorFunc = CursorFunction.CTNormal;
            _lineCounter = 0;
            _lineCounterInit = 0;
            _lineCountOverflow = false;
            _startOver = false;

            if (_currentEvent != null)
            {
                _system.Scheduler.Cancel(_currentEvent);
                _currentEvent = null;
            }

            Log.Debug(Category.Display, "Video controller reset");
        }

        public int DisplayWidth => _displayWidth;
        public int DisplayHeight => _displayHeight;

        bool MicroInterruptEnabled => (_videoStatus & StatusRegister.DisableMicroInterrupt) == 0;
        bool DisplayEnabled => (_videoStatus & StatusRegister.EnableDisplay) != 0;
        bool CursorEnabled => (_videoStatus & StatusRegister.EnableCursor) != 0;
        bool VSyncEnabled => (_videoStatus & StatusRegister.EnableVSync) != 0;

        public bool HandlesPort(byte ioPort)
        {
            // Lazy slow routine to indicate whether this device handles the given port
            for (int i = 0; i < _handledPorts.Length; i++)
            {
                if (ioPort == _handledPorts[i]) { return true; }
            }

            return false;
        }


        public int IORead(byte ioPort)
        {
            switch (ioPort)
            {
                case 0x65:   // Read CRT signals
                    UpdateSignals();
                    Log.Debug(Category.Display, "Read CRT signals, returned {0}", _crtSignals);
                    return (int)_crtSignals;

                case 0x66:   // Read Hi address parity (unimplemented)
                    Log.Debug(Category.Display, "STUB: Read Hi address parity, returned 0");
                    return 0x0;

                case 0x67:   // Read Low address parity (unimplemented)
                    Log.Debug(Category.Display, "STUB: Read Low address parity, returned 0");
                    return 0x0;

                default:
                    throw new UnhandledIORequestException($"Unhandled Memory IO Read from port {ioPort:x2}");
            }
        }

        public void IOWrite(byte ioPort, int value)
        {
            switch (ioPort)
            {
                case 0xe0:  // Load line counter

                    // Line count register counts horizontal scan lines and generates
                    // an interrupt after N lines are scanned.
                    //  15:8    High byte is the "video control port", below
                    //     7    StartOver bit
                    //   6:0    2's complement of N
                    _lineCounterInit = 128 - (value & 0x7f);
                    _lineCounter = _lineCounterInit;
                    _lineCountOverflow = false;

                    // StartOver bit signals the end of the display list
                    _startOver = (value & (int)StatusRegister.StartOver) != 0;

                    // Deal with some special cases :-|
                    if (!_startOver && VSyncEnabled && (_lineCounterInit == 40 || _lineCounterInit == 43))                    
                    {
                        // ** PNX 1 & 2 HACK **
                        //
                        // For some reason ICL does video a little differently:
                        // they only set up one Vblank band instead of two, and
                        // don't set the StartOver bit properly.  This leads to
                        // a garbled display and cursor Y positioning problems.
                        //
                        // Thus, for PNX we force _startOver to be set when the
                        // VSync is true and the line count is off the end of
                        // the visible area.  It may not need to be this exact;
                        // more testing has to be done with ALL the other OSes
                        // to see if this can be generalized.
                        //
                        // See Docs/Video.txt for more information.
                        //
                        _startOver = (_scanLine > _lastVisibleScanLine);
                    }

                    if (_startOver && DisplayEnabled)
                    {
                        // ** PNX 2 HACK (part two) **
                        //
                        // Although I could hack the state machine to ignore the
                        // _startOver bit at the end of visible bands, resetting
                        // it here works too and keeps all the cruft in one place.
                        //
                        _startOver = false;
                    }

                    // Clear interrupt
                    _system.CPU.ClearInterrupt(InterruptSource.LineCounter);

                    Log.Debug(Category.Display, "Line counter set to {0} scanlines (value {1:x4}, StartOver {2})",
                                               _lineCounterInit, value, _startOver);
                    break;

                case 0xe1:  // Load display address register

                    // Display Addr (341 W) Address of first pixel on display.
                    // Must be a multiple of 256 words.
                    _displayAddress = UnFrobAddress(value);

                    Log.Debug(Category.Display, "Display Address Register set to {0:x6}", _displayAddress);
                    break;

                case 0xe2:  // Load cursor address register

                    // Same format as display address
                    _cursorAddress = UnFrobAddress(value);

                    Log.Debug(Category.Display, "Cursor Address Register set to {0:x6}", _cursorAddress);
                    break;

                case 0xe3:  // Video control port

                    // ** ACCENT S6 HACK **
                    // The S6 microcode doesn't properly initialize VidNext for
                    // "normal" display and passes in a bogus value instead; POS
                    // does it correctly.  Argh.  Crude workaround here...
                    if ((value & 0xff00) == 0xff00)
                    {
                        // Force just EnableDisplay, no cursor, map normal.
                        value = 0x8400;
                    }

                    // Video Ctrl (343 W) Mode control bits (see IOVideo.pas)
                    //  15:13   Cursor map function
                    //     12   Force bad parity (for hardware test)
                    //     11   Disable micro interrupt
                    //     10   Show screen (off during vertical blanking)
                    //      9   Vertical retrace
                    //      8   Show cursor
                    //    7:0   Low byte is the Line Count, above
                    _cursorFunc = (CursorFunction)((value & 0xe000) >> 13);
                    _videoStatus = (StatusRegister)(value & 0x1f00);

                    if (CursorEnabled)
                    {
                        _cursorY = 0;
                        Log.Debug(Category.Display, "Cursor Y enabled at line {0}", _scanLine);
                    }

                    // If either VSync or Display enables set, kick the state machine
                    if (VSyncEnabled)
                    {
                        _state = VideoState.VBlank;
                        RunStateMachine();
                    }
                    else if (DisplayEnabled)
                    {
                        _state = VideoState.Active;
                        RunStateMachine();
                    }

                    Log.Debug(Category.Display, "Video status port set to {0} @ line {1}", _videoStatus, _scanLine);
                    break;

                case 0xe4:  // Load cursor X position

                    // Cursor X Position (344 W)
                    //  15:8    not used
                    //   7:0    240 - X
                    //
                    // Cursor X position is only specifiable in 8-pixel offsets
                    // (moving the cursor within those 8 pixels is actually done
                    // in software by shifting the cursor bitmap to match.  Fun.
                    _cursorX = (240 - (value & 0xff));

                    Log.Debug(Category.Display, "Cursor X set to {0} (value={1})", _cursorX, value);
                    break;

                default:
                    throw new UnhandledIORequestException($"Unhandled IO Write to port {ioPort:x2}, data {value:x4}");
            }
        }

        /// <summary>
        /// Decode the display or cursor address:
        ///    15:4    addr >> 4 for .5-2MB boards; addr >> 1 for old 256K boards
        ///     3:2    address bits 21:20 on cards > 2 MB (24-bit systems)
        ///     1:0    not used
        /// </summary>
        /// <remarks>
        /// Currently the Configurator allows up to 8MB (4MW) of memory.  There
        /// were a few obscure references to Accent supporting this much RAM but
        /// no 8MB boards are known to have been produced.  To support the full
        /// 32MB/16MW address range, the display and cursor address registers
        /// and microcode/software support would have to be rewritten and the
        /// full 16 bits used as the base address.
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        int UnFrobAddress(int value)
        {
            // Early PERQ-1 256K memory board
            if (_system.Memory.MemSize < 0x40000)
            {
                return (value << 1);
            }

            // Half-meg through two meg boards
            if (_system.Memory.MemSize < 0x200000)
            {
                return (value << 4);
            }

            // Rare 4MB board (T4)
            return ((value & 0xc) << 18 | (value & 0xfff0) << 4);
        }


        void RunStateMachine()
        {
            switch (_state)
            {
                case VideoState.Idle:
                    // Do nothing.  The microcode will program the control
                    // register(s) to set things in motion again.
                    break;

                case VideoState.Active:
                    _currentEvent = _system.Scheduler.Schedule(_scanLineTimeNsec, (skew, context) =>
                    {
                        // We could range check.  Or just clip it.
                        RenderScanline(_scanLine % DisplayHeight);

                        _state = VideoState.HBlank;
                        RunStateMachine();
                    });
                    break;

                case VideoState.HBlank:
                    _currentEvent = _system.Scheduler.Schedule(_hBlankTimeNsec, (skew, context) =>
                    {
                        //
                        // Count scanlines:  When we fall off the end of the
                        // visible field, tell Display to render the frame.
                        //
                        _scanLine++;

                        if (_scanLine > _lastVisibleScanLine)
                        {
                            // Rate limit refresh in case the microcode is ignoring
                            // interrupts (so the emulator doesn't go bonkers)
                            if (_scanLine % DisplayHeight == 0)
                            {
                                _system.Display.Refresh();
                            }
                        }

                        //
                        // Line counter: count down this "band" until it hits zero.
                        // If it was non-zero to start with, return to Idle and wait
                        // for the microcode to set up the next one.  Otherwise,
                        // return to Active or VBlank to do the next line.
                        //
                        if (_lineCounter > 0) _lineCounter--;

                        if (_lineCounter == 0 && _lineCounterInit > 0)
                        {
                            _state = VideoState.Idle;
                        }
                        else if (VSyncEnabled)
                        {
                            _state = VideoState.VBlank;
                        }
                        else if (DisplayEnabled)
                        {
                            _state = VideoState.Active;
                        }
                        RunStateMachine();
                    });
                    break;

                case VideoState.VBlank:
                    _currentEvent = _system.Scheduler.Schedule(_scanLineTimeNsec, (skew, context) =>
                    {
                        // Take one active scanline interval to do nothing :-)
                        _state = VideoState.HBlank;
                        RunStateMachine();
                    });
                    break;
            }

            // Trigger an interrupt if the line counter is set and has reached 0
            if (_lineCounter == 0 && _lineCounterInit > 0)
            {
                if (MicroInterruptEnabled && !_lineCountOverflow)     // Just trigger it once...
                {
                    Log.Debug(Category.Display, "Line counter overflow @ scanline {0}", _scanLine);
                    _system.CPU.RaiseInterrupt(InterruptSource.LineCounter);
                }

                // Set our flag; this will be reset when _lineCounterInit is reloaded
                _lineCountOverflow = true;

                // Check the StartOver bit: at the end of the second vertical
                // blanking band we're about to start a new frame, so reset the
                // scan line counter before we return to idle.  The microcode
                // should then reenable the display at line zero!
                if (_startOver)
                {
                    _scanLine = 0;
                }
            }
        }

        void UpdateSignals()
        {
            //
            // The LineCounterOverflow status bit in the CRT Signals register should
            // mirror our interrupt status; don't just raise it for the one cycle when
            // we hit zero, but leave it set until the line counter is reset by IOWrite.
            // Accent specifically checks for this bit!
            //
            _crtSignals =
                (_isPortrait ? CRTSignals.LandscapeDisplay : CRTSignals.None) |     // Inverted!
                (_lineCountOverflow ? CRTSignals.LineCounterOverflow : CRTSignals.None) |
                (_state == VideoState.VBlank ? CRTSignals.VerticalSync : CRTSignals.None) |
                (_state == VideoState.HBlank ? CRTSignals.HorizontalSync : CRTSignals.None);
        }

        // Debug
        public void Status()
        {
            UpdateSignals();

            Console.WriteLine("counterInit={0}, count={1}, overflow={2}, intrEnabled={3}",
                              _lineCounterInit, _lineCounter, _lineCountOverflow, MicroInterruptEnabled);
            Console.WriteLine("scanline={0}, startOver={1}, state={2}, crt={3}",
                              _scanLine, _startOver, _state, _crtSignals);
        }

        /// <summary>
        /// Renders one video scanline, mixing in the cursor image when enabled.  
        /// Applies the current screen function and ships the scanline off to the
        /// display driver.
        /// </summary>
        public void RenderScanline(int renderLine)
        {
            // Set the start of this scanline, offset from start of display (by quads)
            int dispAddress = (_displayAddress >> 2) + (renderLine * _displayQuads);

            // The "fast" loop gobbles up quad words
            for (int w = 0; w < _displayQuads; w++)
            {
                _scanlineData.Quads[w] = TransformDisplayQuad(_system.Memory.FetchQuad(dispAddress + w));
            }

            // Now overlay the cursor bytes if enabled
            if (CursorEnabled)
            {
                // Calc the starting address of this line of cursor data
                int cursorAddress = (_cursorAddress >> 2) + _cursorY++;

                // Fetch the quad and break it into 8 bytes for easy mixin'!
                GetCursorQuad(cursorAddress);

                int cursorStartByte = _cursorX;
                int cursByte = 0;

                for (int dispByte = cursorStartByte; dispByte < cursorStartByte + 8; dispByte++)
                {
                    // Could be much cleverer about this and shorten the loop
                    // if the cursor is off the edge; for now, just clip to range
                    if ((dispByte >= 0) && (dispByte < _scanlineData.Bytes.Length))
                    {
                        _scanlineData.Bytes[dispByte] = TransformCursorByte(_scanlineData.Bytes[dispByte],
                                                                            _cursorData[cursByte]);
                    }
                    cursByte++;
                }
            }

            // Ship it!
            _system.Display.DrawScanline(renderLine, _scanlineData.Bytes);
        }

        /// <summary>
        /// Retrieve a quad word from memory and break it into an 8-byte array.
        /// </summary>
        /// <remarks>
        /// Like _scanlineData, reuse _cursorData[] to avoid memory allocation
        /// overhead.  Writes the bytes out in reverse order so we can just go
        /// fast and shift things.
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        void GetCursorQuad(int addr)
        {
            var quad = _system.Memory.FetchQuad(addr);

            for (int b = 0; b < 8; b++)
            {
                _cursorData[b] = (byte)(quad & 0xff);
                quad >>= 8;
            }
        }

        /// <summary>
        /// Transforms a display quad word based on the current Cursor function.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        ulong TransformDisplayQuad(ulong quad)
        {
            switch (_cursorFunc)
            {
                case CursorFunction.CTInvBlackHole:
                case CursorFunction.CTWhite:
                    return 0;

                case CursorFunction.CTCursorOnly:
                case CursorFunction.CTBlackHole:
                    return 0xffffffffffffffff;

                case CursorFunction.CTNormal:
                case CursorFunction.CTCursCompl:
                    return quad;

                case CursorFunction.CTInvert:
                case CursorFunction.CTInvCursCompl:
                    return ~quad;
            }

            // Can't actually happen
            throw new ArgumentException("Bad _cursorFunc in TransformDisplayQuad");
        }

        /// <summary>
        /// Transforms a cursor byte based on the Cursor function and display byte.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        byte TransformCursorByte(byte dispByte, byte cursByte)
        {
            switch (_cursorFunc)
            {
                case CursorFunction.CTInvBlackHole:
                case CursorFunction.CTWhite:
                    return cursByte;

                case CursorFunction.CTBlackHole:
                case CursorFunction.CTCursorOnly:
                    return (byte)~cursByte;

                case CursorFunction.CTNormal:
                    return (byte)(cursByte | dispByte);

                case CursorFunction.CTInvert:
                    return (byte)(~cursByte & dispByte);

                case CursorFunction.CTCursCompl:
                    return (byte)(cursByte ^ dispByte);

                case CursorFunction.CTInvCursCompl:
                    return (byte)(~cursByte ^ ~dispByte);
            }

            throw new ArgumentException("Bad _cursorFunc in TransformCursorByte");
        }

        [Flags]
        enum CRTSignals
        {
            None = 0x0,
            HorizontalSync = 0x1,
            VerticalSync = 0x2,
            LoopThrough = 0x4,
            Unused0 = 0x8,
            LineCounterOverflow = 0x10,
            Unused1 = 0x20,
            Unused2 = 0x40,
            LandscapeDisplay = 0x80     // set=Portrait, clear=Landscape!
        }

        [Flags]
        enum StatusRegister
        {
            None = 0x0,
            LineCountMask = 0x7f,       // 7 bits = 1..128 lines in the band
            StartOver = 0x80,           // High bit signals end of frame
            EnableCursor = 0x100,
            EnableVSync = 0x200,
            EnableDisplay = 0x400,
            DisableMicroInterrupt = 0x800,
            WriteBadParity = 0x1000
        }

        enum VideoState
        {
            Active = 0,
            HBlank,
            VBlank,
            Idle
        }

        enum CursorFunction
        {
            CTWhite = 0,        // InvertedCursorInvertedDisplay
            CTCursorOnly,       // InvertedCursor
            CTBlackHole,        // AndCursor
            CTInvBlackHole,     // NandCursorInvertedDisplay
            CTNormal,           // NorCursorInvertedDisplay
            CTInvert,           // OrCursor
            CTCursCompl,        // XnorCursorInvertedDisplay
            CTInvCursCompl      // XorCursor
        }

        byte[] _handledPorts =
        {
            0x65,               // Read CRT signals
            0x66,               // Read Hi address parity
            0x67,               // Read Low address parity
            0xe0,               // Load Line counter
            0xe1,               // Load Display address
            0xe2,               // Load Cursor address
            0xe3,               // Load Video status
            0xe4                // Load Cursor X position
        };

        //
        // Note: These timings are constant for both supported Displays!
        // This is strictly based on 60Hz refresh tied to the original 170ns
        // microcycle time; run the CPU faster or slower and the refresh rate
        // varies with it.  On a fast host, rate limiting the CPU (Settings)
        // will yield 60fps; running faster than that might produce some
        // strange behavior in OSes that rely on a 60Hz vertical retrace to
        // run their clocks, but that'd be a nice problem to have... :-|
        //
        readonly ulong _scanLineTimeNsec = 11900;   // 70 microcycles
        readonly ulong _hBlankTimeNsec = 3740;      // 22 microcycles

        // Width is configurable; both displays are the same height
        int _displayWidth;
        const int _displayHeight = 1024;
        const int _lastVisibleScanLine = _displayHeight - 1;

        // Trade a little space for speed
        int _displayQuads;
        int _displayBytes;
        bool _isPortrait = true;

        ScanLineBuffer _scanlineData;
        byte[] _cursorData;

        VideoState _state;
        SchedulerEvent _currentEvent;
        int _scanLine;
        int _lineCounterInit;
        bool _lineCountOverflow;
        bool _startOver;

        // IO registers
        int _lineCounter;
        int _displayAddress;
        int _cursorAddress;
        int _cursorX;
        int _cursorY;
        CursorFunction _cursorFunc;
        CRTSignals _crtSignals;
        StatusRegister _videoStatus;

        PERQSystem _system;
    }
}
