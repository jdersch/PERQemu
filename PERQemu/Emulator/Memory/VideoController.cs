//
// VideoController.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
    internal struct ScanLineBuffer
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
            _displayWords = _displayWidth / 16;
            _displayBytes = _displayWidth / 8;

            // One scanline
            _scanlineData = new ScanLineBuffer();
            _scanlineData.Bytes = new byte[_displayBytes];

            // One quadword as 8 bytes
            _cursorData = new byte[8];
        }

        public void Reset()
        {
            _state = VideoState.Idle;
            _crtSignals = CRTSignals.None;
            _scanLine = 0;
            _displayAddress = 0;
            _cursorAddress = 0;
            _cursorX = 0;
            _cursorY = 0;
            _cursorFunc = CursorFunction.CTNormal;
            _lineCounter = 0;
            _lineCounterInit = 0;
            _lineCountOverflow = false;

            UpdateSignals();

            if (_currentEvent != null)
            {
                _system.Scheduler.Cancel(_currentEvent);
                _currentEvent = null;
            }

            Log.Debug(Category.Display, "Video controller reset");
        }

        public int DisplayWidth => _displayWidth;
        public int DisplayHeight => _displayHeight;

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
                    //   bit    description
                    //  15:8    high byte is the "video control port", below
                    //     7    "start over" bit
                    //   6:0    2's complement of N
                    _lineCounterInit = 128 - (value & 0x7f);
                    _lineCounter = _lineCounterInit;
                    _lineCountOverflow = false;

                    if ((value & (int)StatusRegister.StartOver) != 0)
                    {
                        _scanLine = 0;      // StartOver bit signals end of display list
                    }

                    // Clear interrupt
                    _system.CPU.ClearInterrupt(InterruptSource.LineCounter);

                    Log.Debug(Category.Display, "Line counter set to {0} scanlines. (write was {1:x4})",
                                               _lineCounterInit, value);
                    break;

                case 0xe1:  // Load display address register

                    // Display Addr (341 W) Address of first pixel on display. Must be a
                    // multiple of 256 words.
                    _displayAddress = UnFrobAddress(value);

                    Log.Debug(Category.Display, "Display Address Register set to {0:x6}", _displayAddress);
                    break;

                case 0xe2:  // Load cursor address register

                    // Same format as display address
                    _cursorAddress = UnFrobAddress(value);

                    Log.Debug(Category.Display, "Cursor Address Register set to {0:x6}", _cursorAddress);
                    break;

                case 0xe3:  // Video control port

                    // Video Ctrl (343 W) Mode control bits
                    //  15:13   Cursor map function
                    //     12   Force bad parity (for hardware test)
                    //     11   Disable parity interrupt
                    //     10   Enable display (off during vertical blanking)
                    //      9   Enable Vertical Sync
                    //      8   Enable cursor
                    //    7:0   Low byte is the Line Count, above
                    _cursorFunc = (CursorFunction)((value & 0xe000) >> 13);
                    _videoStatus = (StatusRegister)(value & 0x1f00);

                    if ((_videoStatus & StatusRegister.EnableCursor) != 0)
                    {
                        _cursorY = 0;
                        Log.Debug(Category.Display, "Cursor Y enabled at line {0}", _scanLine);
                    }

                    if ((_videoStatus & StatusRegister.EnableVSync) != 0)
                    {
                        _state = VideoState.VBlankScanline;
                        RunStateMachine();
                    }

                    if ((_videoStatus & StatusRegister.EnableDisplay) != 0)
                    {
                        _state = VideoState.VisibleScanline;
                        RunStateMachine();
                    }

                    Log.Debug(Category.Display, "Video status port set to {0}", _videoStatus);
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
                    throw new UnhandledIORequestException(
                        string.Format("Unhandled IO Write to port {0:x2}, data {1:x4}", ioPort, value));
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
        private int UnFrobAddress(int value)
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


        private void RunStateMachine()
        {
            switch (_state)
            {
                case VideoState.Idle:
                    // Do nothing.  The microcode will program the control
                    // register(s) to set things in motion again.
                    break;

                case VideoState.VisibleScanline:
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
                        _scanLine++;

                        if (_scanLine > _lastVisibleScanLine)
                        {
                            // Off the end of the visible field; tell the Display
                            // to go render the frame, but rate limit the refresh
                            // in case the microcode is ignoring interrupts (so
                            // the emulator doesn't go bonkers).
                            if (_scanLine % DisplayHeight == 0)
                                _system.Display.Refresh();
                        }

                        // A bit of hackery: if the line count has gone to zero AND
                        // it was non-zero to start with, return to Idle and wait
                        // for the microcode to set up the next band (which should
                        // be the first of the VBlank bands).  However, at boot-up
                        // when things are running wild, just loop back to draw
                        // another line (which RenderLine will clip to the visible
                        // range).  This is all a bit cheesy.
                        if (_lineCounter > 0) _lineCounter--;

                        if (_lineCounter == 0 && _lineCounterInit > 0)
                        {
                            _state = VideoState.Idle;
                        }
                        else
                        {
                            _state = VideoState.VisibleScanline;
                        }
                        RunStateMachine();
                    });
                    break;

                case VideoState.VBlankScanline:
                    _currentEvent = _system.Scheduler.Schedule(_scanLineTimeNsec + _hBlankTimeNsec,
                                                               (skew, context) =>
                    {
                        if (_lineCounter > 0) _lineCounter--;

                        // If we're at the end of the vertical blanking interval,
                        // UpdateSignals() will raise the line counter interrupt.
                        // We just return to Idle and wait for the microcode to
                        // enable the next band (2nd Vblank or back to Visible).
                        if (_lineCounter == 0)
                        {
                            _state = VideoState.Idle;
                        }
                        else
                        {
                            _state = VideoState.VBlankScanline;
                        }
                        RunStateMachine();
                    });
                    break;
            }

            UpdateSignals();
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void UpdateSignals()
        {
            // Trigger an interrupt if the line counter is set and has reached 0
            if (_lineCounter == 0 && _lineCounterInit > 0)
            {
                if (ParityInterruptsEnabled && !_lineCountOverflow)     // Just trigger it once...
                {
                    Log.Debug(Category.Display, "Line counter overflow @ scanline {0}", _scanLine);
                    _system.CPU.RaiseInterrupt(InterruptSource.LineCounter);
                }

                // Set our flag; this will be reset when _lineCounterInit is reloaded
                _lineCountOverflow = true;
            }

            //
            // The LineCounterOverflow status bit in the CRT Signals register should
            // mirror our interrupt status; don't just raise it for the one cycle when
            // we hit zero, but leave it set until the line counter is reset by IOWrite.
            // Accent specifically checks for this bit!
            //
            _crtSignals =
                (_isPortrait ? CRTSignals.LandscapeDisplay : CRTSignals.None) |     // Inverted!
                (_lineCountOverflow ? CRTSignals.LineCounterOverflow : CRTSignals.None) |
                (_state == VideoState.VBlankScanline ? CRTSignals.VerticalSync : CRTSignals.None) |
                (_state == VideoState.HBlank ? CRTSignals.HorizontalSync : CRTSignals.None);
        }

        private bool CursorEnabled
        {
            get { return (_videoStatus & StatusRegister.EnableCursor) == StatusRegister.EnableCursor; }
        }

        private bool DisplayEnabled
        {
            get { return (_videoStatus & StatusRegister.EnableDisplay) == StatusRegister.EnableDisplay; }
        }

        private bool ParityInterruptsEnabled
        {
            get { return (_videoStatus & StatusRegister.EnableParityInterrupts) != StatusRegister.EnableParityInterrupts; }
        }

        // debug
        public void Status()
        {
            Console.WriteLine("_lineCounterInit={0}, count={1} overflow={2}",
                              _lineCounterInit, _lineCounter, _lineCountOverflow);
            Console.WriteLine("_scanline={0}, _state={1}, crt={2}",
                              _scanLine, _state, _crtSignals);
        }

        /// <summary>
        /// Renders one video scanline, mixing in the cursor image when enabled.  
        /// Applies the current screen function and ships the scanline off to the
        /// display driver.
        /// </summary>
        public void RenderScanline(int renderLine)
        {
            // Set the start of this scanline, offset from start of display (by quads)
            int dispAddress = (_displayAddress / 4) + (renderLine * _displayQuads);

            // The "fast" loop gobbles up quad words
            for (int w = 0; w < _displayQuads; w++)
            {
                _scanlineData.Quads[w] = TransformDisplayQuad(_system.Memory.FetchQuad(dispAddress + w));
            }

            // Now overlay the cursor bytes if enabled
            if (CursorEnabled)
            {
                // Calc the starting address of this line of cursor data
                int cursorAddress = (_cursorAddress / 4) + _cursorY++;

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
        private void GetCursorQuad(int addr)
        {
            var quad = _system.Memory.FetchQuad(addr);

            for (int b = 0; b < 8; b++)
            {
                _cursorData[b] = (byte)(quad & 0xff);
                quad = quad >> 8;
            }
        }

        /// <summary>
        /// Transforms a display quad word based on the current Cursor function.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ulong TransformDisplayQuad(ulong quad)
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
            throw new Exception("Bad _cursorFunc in TransformDisplayQuad");
        }

        /// <summary>
        /// Transforms a cursor byte based on the Cursor function and display byte.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private byte TransformCursorByte(byte dispByte, byte cursByte)
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

            throw new Exception("Bad _cursorFunc in TransformCursorByte");
        }

        [Flags]
        private enum CRTSignals
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
        private enum StatusRegister
        {
            None = 0x0,
            LineCountMask = 0x7f,       // 7 bits = 1..128 lines in the band
            StartOver = 0x80,           // High bit signals end of frame
            EnableCursor = 0x100,
            EnableVSync = 0x200,
            EnableDisplay = 0x400,
            EnableParityInterrupts = 0x800,
            WriteBadParity = 0x1000
        }

        private enum VideoState
        {
            VisibleScanline = 0,
            HBlank,
            VBlankScanline,
            Idle
        }

        private enum CursorFunction
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

        private byte[] _handledPorts =
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
        private readonly ulong _scanLineTimeNsec = 11900;   // 70 microcycles
        private readonly ulong _hBlankTimeNsec = 3740;      // 22 microcycles

        // Width is configurable; both displays are the same height
        private int _displayWidth;
        private const int _displayHeight = 1024;
        private const int _lastVisibleScanLine = _displayHeight - 1;

        // Trade a little space for speed
        private int _displayQuads;
        private int _displayWords;
        private int _displayBytes;
        private bool _isPortrait = true;

        private ScanLineBuffer _scanlineData;
        private byte[] _cursorData;

        private VideoState _state;
        private SchedulerEvent _currentEvent;
        private int _scanLine;
        private int _lineCounterInit;
        private bool _lineCountOverflow;

        // IO registers
        private int _lineCounter;
        private int _displayAddress;
        private int _cursorAddress;
        private int _cursorX;
        private int _cursorY;
        private CursorFunction _cursorFunc;
        private CRTSignals _crtSignals;
        private StatusRegister _videoStatus;

        private PERQSystem _system;
    }
}
