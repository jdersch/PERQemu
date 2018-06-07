// videocontroller.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

using System;
using PERQemu.Memory;
using PERQemu.IO;
using PERQemu.CPU;

namespace PERQemu.Display
{
    /// <summary>
    /// Implements functionality for the PERQ's video controller.
    /// </summary>
    public sealed class VideoController : IIODevice
    {

        private VideoController()
        {
            Reset();
        }

        public void Reset()
        {
            _crtSignals = CRTSignals.LandscapeDisplay;
            _state = VideoState.VisibleScanline;
            _scanLine = 0;
            _displayAddress = 0;
            _cursorAddress = 0;
            _cursorX = 0;
            _cursorY = 0;
            _cursorFunc = CursorFunction.AndCursor;
            _lineCounter = 0;
            _lineCounterInit = 0;
            _lineCountOverflow = false;
            _cycle = 0;

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Display, "Video Controller: Reset.");
#endif
        }

        public static VideoController Instance
        {
            get { return _instance; }
        }

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
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Read CRT signals, returned {0}", _crtSignals);
#endif
                    return (int)_crtSignals;

                case 0x66:   // Read Hi address parity (unimplemented)
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "STUB: Read Hi address parity, returned 0.");
#endif
                    return 0x0;

                case 0x67:   // Read Low address parity (unimplemented)
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "STUB: Read Low address parity, returned 0.");
#endif
                    return 0x0;

                default:
                    throw new UnhandledIORequestException(
                        String.Format("Unhandled Memory IO Read from port {0:x2}", ioPort));
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
                    //  15:7    not used
                    //   6:0    2's complement of N
                    _lineCounterInit = 128 - (value & 0x7f);
                    _lineCounter = _lineCounterInit;
                    _lineCountOverflow = false;

                    // Clear interrupt
                    PERQCpu.Instance.ClearInterrupt(InterruptType.LineCounter);

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Line counter set to {0} scanlines. (write was {1:x4})",
                                                   _lineCounterInit, value);
#endif
                    break;

                case 0xe1:  // Load display address register

                    // Display Addr (341 W) Address of first pixel on display. Must be a
                    // multiple of 256 words.
                    //  15:4    address >> 4 for .5-2MB boards; address >> 1 for old 256K boards
                    //   3:2    address bits 21:20 on cards > 2 MB (not yet supported)
                    //   1:0    not used
                    _displayAddress = (MemoryBoard.Instance.MemSize < 0x40000 ? value << 1 : value << 4);

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Display Address Register set to {0:x5}", _displayAddress);
#endif
                    break;

                case 0xe2:  // Load cursor address register

                    // Same format as display address
                    _cursorAddress = (MemoryBoard.Instance.MemSize < 0x40000 ? value << 1 : value << 4);

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Cursor Address Register set to {0:x5}", _cursorAddress);
#endif
                    break;

                case 0xe3:  // Video control port

                    // Video Ctrl (343 W) Mode control bits
                    //  15:13   Cursor map function
                    //     12   Force bad parity (for hardware test)
                    //     11   Disable parity interrupt
                    //     10   Enable display (off during vertical blanking)
                    //      9   Enable Vertical Sync
                    //      8   Enable cursor
                    //    7:0   not used
                    _cursorFunc = (CursorFunction)((value & 0xe000) >> 13);
                    _videoStatus = (StatusRegister)(value & 0x1f00);

                    if ((_videoStatus & StatusRegister.EnableCursor) != 0)
                    {
                        _cursorY = _scanLine;
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.Display, "Cursor Y set to {0}", _cursorY);
#endif
                    }

                    if ((_videoStatus & StatusRegister.EnableVSync) != 0)
                    {
                        _scanLine = 0;
                        _state = VideoState.VBlankScanline;
                    }

                    if ((_videoStatus & StatusRegister.EnableDisplay) != 0)
                    {
                        _state = VideoState.VisibleScanline;
                    }

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Video status port set to {0}", _videoStatus);
#endif
                    break;

                case 0xe4:  // Load cursor X position

                    // Cursor X Position (344 W)
                    //  15:8    not used
                    //   7:0    240 - X
                    //
                    // Cursor X position is only specifiable in 8-pixel offsets
                    // (moving the cursor within those 8 pixels is actually done
                    // in software by shifting the cursor bitmap to match.  Fun.
                    _cursorX = (int)(240 - (value & 0xff));

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Cursor X set to {0} (value={1})", _cursorX, value);
#endif
                    break;

                default:
                    throw new UnhandledIORequestException(
                        String.Format("Unhandled Memory IO Write to port {0:x2}, data {1:x4}", ioPort, value));
            }
        }

        public void Clock()
        {
            _cycle++;

            switch (_state)
            {
                case VideoState.VisibleScanline:
                    if (_cycle > _scanLineCycles)
                    {
                        _cycle = 0;
                        _state = VideoState.HBlank;
                        RenderScanline();
                    }
                    break;

                case VideoState.HBlank:
                    if (_cycle > _hBlankCycles)
                    {
                        _cycle = 0;
                        _scanLine++;
                        if (_lineCounter > 0) _lineCounter--;

                        if (_scanLine > _lastVisibleScanLine)
                        {
                            // Usually the microcode drives vertical blanking through
                            // the control register; but during bootup it sometimes lets
                            // the display run free, ignoring Vblank, but setting the
                            // Enable bit so that memory refresh happens.  Here we just
                            // force the state change so the display doesn't appear to
                            // freeze up...
                            _state = VideoState.VBlankScanline;
                            Display.Instance.Refresh();
                        }
                        else
                        {
                            _state = VideoState.VisibleScanline;
                        }
                    }
                    break;

                case VideoState.VBlankScanline:
                    if (_cycle > _scanLineCycles)
                    {
                        _cycle = 0;
                        if (_lineCounter > 0) _lineCounter--;

                        if (_lineCounter == 0)              // Trust what the microcode set
                        {
                            _state = VideoState.VisibleScanline;
                        }
                    }
                    break;
            }

            // Trigger an interrupt if the line counter is set and has reached 0
            if (_lineCounter == 0 && _lineCounterInit > 0)
            {
                if (ParityInterruptsEnabled && !_lineCountOverflow)     // Just trigger it once...
                {
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Display, "Line counter overflow, triggering interrupt @ scanline {0}", _scanLine);
#endif
                    PERQCpu.Instance.RaiseInterrupt(InterruptType.LineCounter);
                }

                // Set our flag; this will be reset when _lineCounterInit is reloaded
                _lineCountOverflow = true;
            }

            // The LineCounterOverflow status bit in the CRT Signals register should mirror our
            // interrupt status; don't just raise it for the one cycle when we hit zero, but
            // leave it set until the line counter is reset by an IOWrite.  Accent specifically
            // checks for this bit!

            _crtSignals =
                CRTSignals.LandscapeDisplay |
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

        public void RenderScanline()
        {
            int renderLine = _scanLine;

            // The PERQ video driver could run free when the microcode was ignoring
            // interrupts, producing a visual display of a rolling retrace across the
            // entire height of the tube.  It'd be fun to simulate that for accuracy's
            // sake, but for now just mod the value so it remains in the visible range.
            if (_scanLine < 0 || _scanLine > _lastVisibleScanLine)
            {
                renderLine = _scanLine % PERQ_DISPLAYHEIGHT;
            }

            for (int x = 0; x < PERQ_DISPLAYWIDTH_IN_WORDS; x++)
            {
                int dataAddress = renderLine * PERQ_DISPLAYWIDTH_IN_WORDS + x + _displayAddress;
                int screenAddress = renderLine * PERQ_DISPLAYWIDTH_IN_BYTES + (x * 2);
                Display.Instance.DrawWord(screenAddress, TransformDisplayWord(MemoryBoard.Instance.FetchWord(dataAddress)));
            }

            if (CursorEnabled)
            {
                RenderCursorLine();
            }
        }

        private void RenderCursorLine()
        {
            // Calc the starting address of this line of the cursor data
            int cursorAddress = ((_cursorAddress << 1) + (_scanLine - _cursorY) * 8);

            int cursorStartByte = _cursorX;
            int backgroundStartByte = _scanLine * PERQ_DISPLAYWIDTH_IN_BYTES + (_displayAddress << 1);
            int screenAddress = _scanLine * PERQ_DISPLAYWIDTH_IN_BYTES;

            // We draw 8 bytes (4 words) of horizontal cursor data combined with
            // the background data on that line based on the current cursor function.
            for (int x = cursorStartByte; x < cursorStartByte + 8; x++)
            {
                if (x >= 0 && x < PERQ_DISPLAYWIDTH_IN_BYTES)
                {
                    byte backgroundByte = GetByte(backgroundStartByte + x);
                    byte cursorByte = GetByte(cursorAddress + (x - cursorStartByte));

                    byte transformedByte = TransformCursorByte(backgroundByte, cursorByte);

                    Display.Instance.DrawByte(screenAddress + x, transformedByte);
                }
            }
        }

        private byte GetByte(int byteAligned)
        {
            ushort word = MemoryBoard.Instance.FetchWord(byteAligned >> 1);

            if ((byteAligned % 2) == 0)
            {
                return (byte)(word >> 8);
            }
            else
            {
                return (byte)(word);
            }
        }

        /// <summary>
        /// Transforms the display word based on the current Cursor function.
        /// </summary>
        private ushort TransformDisplayWord(ushort word)
        {
            switch (_cursorFunc)
            {
                case CursorFunction.InvertedCursorInvertedDisplay:
                case CursorFunction.NandCursorInvertedDisplay:
                case CursorFunction.NorCursorInvertedDisplay:
                case CursorFunction.XnorCursorInvertedDisplay:
                    return (ushort)~word;
            }

            return word;
        }

        /// <summary>
        /// Transforms the cursor byte based on the current Cursor function and the display bytes.
        /// </summary>
        private byte TransformCursorByte(byte dispWord, byte cursWord)
        {
            switch (_cursorFunc)
            {
                case CursorFunction.AndCursor:
                    return (byte)(cursWord & dispWord);

                case CursorFunction.OrCursor:
                    return (byte)(cursWord | dispWord);

                case CursorFunction.XorCursor:
                    return (byte)(cursWord ^ dispWord);

                case CursorFunction.InvertedCursor:
                case CursorFunction.InvertedCursorInvertedDisplay:
                    return (byte)~cursWord;

                case CursorFunction.NandCursorInvertedDisplay:
                    return (byte)~(cursWord & dispWord);

                case CursorFunction.NorCursorInvertedDisplay:
                    return (byte)~(cursWord | dispWord);

                case CursorFunction.XnorCursorInvertedDisplay:
                    return (byte)((cursWord & dispWord) | ((~cursWord) & (~dispWord)));

                default:
                    throw new ArgumentOutOfRangeException("Unexpected cursor function value.");
            }
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
            LandscapeDisplay = 0x80         // set=Portrait, clear=Landscape!
        }

        [Flags]
        private enum StatusRegister
        {
            None = 0x0,
            Unused0 = 0x001,
            Unused1 = 0x002,
            Unused2 = 0x004,
            Unused3 = 0x008,
            Unused4 = 0x010,
            Unused5 = 0x020,
            Unused6 = 0x040,
            Unused7 = 0x080,
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
            VBlankScanline
        }

        private enum CursorFunction
        {
            InvertedCursorInvertedDisplay = 0,
            InvertedCursor,
            AndCursor,
            NandCursorInvertedDisplay,
            NorCursorInvertedDisplay,
            OrCursor,
            XnorCursorInvertedDisplay,
            XorCursor
        }

        private byte[] _handledPorts =
        {
            0x65,   // Read CRT signals
            0x66,   // Read Hi address parity
            0x67,   // Read Low address parity
            0xe0,   // Load Line counter
            0xe1,   // Load Display address
            0xe2,   // Load Cursor address
            0xe3,   // Load Video status
            0xe4    // Load Cursor X position
        };

        /// <summary>
        /// Various handy portrait display constants
        /// </summary>
        public static int PERQ_DISPLAYWIDTH = 768;
        public static int PERQ_DISPLAYWIDTH_IN_WORDS = 48;
        public static int PERQ_DISPLAYWIDTH_IN_BYTES = 96;
        public static int PERQ_DISPLAYHEIGHT = 1024;

        /// <summary>
        /// Elapsed cycles, used for display timing.
        /// These are based on the below information, rounded to the
        /// nearest cycle (so they're not 100% accurate)
        ///
        /// Vertical:
        /// 1024 lines displayed
        /// 43 lines blanking
        /// ----
        /// 1067 lines
        ///
        /// Horizontal - Portrait montior
        /// 768 pixels displayed
        /// 244 blank
        /// -----
        /// 1012 pixel clocks
        ///
        /// Horizontal - landscape monitor (not yet implemented)
        /// 1280 pixels displayed
        /// 422 blank
        /// -----
        /// 1702 pixel clocks
        ///
        /// Perq master clock = 5.89Mhz = ~170ns.  For emulation purposes, this is the rate that
        /// dictates how often our VideoController Clock() is called, so:
        /// 
        /// Portrait pixel clock = 64.78824 MHz = 15.44ns -> ~11:1 clock ratio = 92 cpu clocks/line
        /// Visible part of each scan line = 768 bit times * 15.44ns / 11 = ~70 pixel clocks
        /// Horizontal retrace per scan line = 244 bit times * 15.44ns / 11 = ~22 pixel clocks
        ///
        /// Landscape pixel clock = 108.962 MHz = 9.1ns -> ~18.5:1 clock ratio = 92 cpu clocks/line!
        /// Visible part of scan line = 1280 bit times * 9.1ns / 18.5 = ~70 pixel clocks
        /// Horizontal retrace = 422 bit times * 9.1ns / 18.5 = ~22 pixel clocks
        /// 
        /// Vertical retrace is accounted for by the microcode setting up two bands totalling 43 lines.
        /// Because these are part of the normal interrupt service routine, we don't need to track our
        /// own vertical retrace timer; _vBlankCycles can be removed.
        /// </summary>
        private int _cycle;

        private const int _scanLineCycles = 70;
        private const int _hBlankCycles = 22;
        private const int _vBlankCycles = 4 * (_scanLineCycles + _hBlankCycles); // Unused
        private static int _lastVisibleScanLine = PERQ_DISPLAYHEIGHT - 1;

        private VideoState _state;
        private int _scanLine;
        private int _lineCounterInit;
        private bool _lineCountOverflow;

        // IO registers
        private int _lineCounter;
        private CRTSignals _crtSignals;
        private StatusRegister _videoStatus;
        private int _displayAddress;
        private int _cursorAddress;
        private int _cursorX;
        private int _cursorY;
        private CursorFunction _cursorFunc;

        private static VideoController _instance = new VideoController();
    }
}
