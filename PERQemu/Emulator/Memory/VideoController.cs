// videocontroller.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Runtime.InteropServices;
using System.Runtime.CompilerServices;

using PERQemu.IO;
using PERQemu.UI;
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

            // TODO: Allocate our fixed buffer based on the configured display
            _scanlineData = new ScanLineBuffer();
            _scanlineData.Bytes = new byte[PERQ_DISPLAYWIDTH_IN_BYTES];

            // One quadword as 8 bytes
            _cursorData = new byte[8];
        }

        public void Reset()
        {
            _crtSignals = CRTSignals.LandscapeDisplay;
            _scanLine = 0;
            _displayAddress = 0;
            _cursorAddress = 0;
            _cursorX = 0;
            _cursorY = 0;
            _cursorFunc = CursorFunction.CTNormal;
            _lineCounter = 0;
            _lineCounterInit = 0;
            _lineCountOverflow = false;

            Trace.Log(LogType.Display, "Video Controller: Reset.");

            if (_currentEvent != null)
            {
                _system.Scheduler.Cancel(_currentEvent);
                _currentEvent = null;
            }

            _state = VideoState.Idle;

            // Kick off initial tick
            RunStateMachine();
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
                    Trace.Log(LogType.Display, "Read CRT signals, returned {0}", _crtSignals);
                    return (int)_crtSignals;

                case 0x66:   // Read Hi address parity (unimplemented)
                    Trace.Log(LogType.Display, "STUB: Read Hi address parity, returned 0.");
                    return 0x0;

                case 0x67:   // Read Low address parity (unimplemented)
                    Trace.Log(LogType.Display, "STUB: Read Low address parity, returned 0.");
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
                    //  15:8    high byte is the "video control port", below
                    //     7    "start over" bit
                    //   6:0    2's complement of N
                    _lineCounterInit = 128 - (value & 0x7f);
                    _lineCounter = _lineCounterInit;
                    _lineCountOverflow = false;

                    if ((value & 0x80) != 0)
                    {
                        _scanLine = 0;      // StartOver bit signals end of display list
                    }

                    // Clear interrupt
                    _system.CPU.ClearInterrupt(InterruptType.LineCounter);

                    Trace.Log(LogType.Display, "Line counter set to {0} scanlines. (write was {1:x4})",
                                               _lineCounterInit, value);
                    break;

                case 0xe1:  // Load display address register

                    // Display Addr (341 W) Address of first pixel on display. Must be a
                    // multiple of 256 words.
                    //  15:4    address >> 4 for .5-2MB boards; address >> 1 for old 256K boards
                    //   3:2    address bits 21:20 on cards > 2 MB (not yet supported)
                    //   1:0    not used
                    _displayAddress = (_system.Memory.MemSize < 0x40000 ? value << 1 : value << 4);

                    Trace.Log(LogType.Display, "Display Address Register set to {0:x6}", _displayAddress);
                    break;

                case 0xe2:  // Load cursor address register

                    // Same format as display address
                    _cursorAddress = (_system.Memory.MemSize < 0x40000 ? value << 1 : value << 4);

                    Trace.Log(LogType.Display, "Cursor Address Register set to {0:x6}", _cursorAddress);
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
                        Trace.Log(LogType.Display, "Cursor Y enabled at line {0}", _scanLine);
                    }

                    if ((_videoStatus & StatusRegister.EnableVSync) != 0)
                    {
                        //_system.Scheduler.Cancel(_currentEvent);
                        _state = VideoState.VBlankScanline;
                        RunStateMachine();
                    }

                    if ((_videoStatus & StatusRegister.EnableDisplay) != 0)
                    {
                        //_system.Scheduler.Cancel(_currentEvent);
                        _state = VideoState.VisibleScanline;
                        RunStateMachine();
                    }

                    Trace.Log(LogType.Display, "Video status port set to {0}", _videoStatus);
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

                    Trace.Log(LogType.Display, "Cursor X set to {0} (value={1})", _cursorX, value);
                    break;

                default:
                    throw new UnhandledIORequestException(
                        String.Format("Unhandled Memory IO Write to port {0:x2}, data {1:x4}", ioPort, value));
            }
        }

        /// <summary>
        /// Not long for this world
        /// </summary>
        public uint Clock()
        {
            return 0;
        }

        public void RunStateMachine()
        {

            switch (_state)
            {
                case VideoState.Idle:
                    // Do nothing.  The microcode will program the control
                    // register(s) to set things in motion again.
                    Trace.Log(LogType.Display, "Video RunSM in {0} at {1}, scan={2} count={3}",
                                      _state, _system.Scheduler.CurrentTimeNsec,
                                     _scanLine, _lineCounter);
                    break;

                case VideoState.VisibleScanline:
                    _currentEvent = _system.Scheduler.Schedule(_scanLineTimeNsec, (skew, context) =>
                    {
                        // We could range check.  Or just clip it.
                        RenderScanline(_scanLine % PERQ_DISPLAYHEIGHT);

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
                            if (_scanLine % PERQ_DISPLAYHEIGHT == 0)
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
                    _currentEvent = _system.Scheduler.Schedule(_scanLineTimeNsec + _hBlankTimeNsec, (skew, context) =>
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
                    Trace.Log(LogType.Display, "Line counter overflow, triggering interrupt @ scanline {0}", _scanLine);
                    _system.CPU.RaiseInterrupt(InterruptType.LineCounter);
                }

                // Set our flag; this will be reset when _lineCounterInit is reloaded
                _lineCountOverflow = true;
            }

            // The LineCounterOverflow status bit in the CRT Signals register should
            // mirror our interrupt status; don't just raise it for the one cycle when
            // we hit zero, but leave it set until the line counter is reset by IOWrite.
            // Accent specifically checks for this bit!

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
        /// </summary>
        /// <remarks>
        /// Clips the current _scanLine to the visible range.  Applies the current
        /// cursor function and ships the scanline off to the display driver.
        /// </remarks>
        public void RenderScanline(int renderLine)
        {
            if (CursorEnabled)
            {
                // Set the start of this scanline, offset from start of display
                int dispAddress = _displayAddress + (renderLine * PERQ_DISPLAYWIDTH_IN_WORDS);
                int dispByte = 0;

                // Calc the starting address of this line of cursor data
                int cursorAddress = (_cursorAddress / 4) + _cursorY++;

                // Fetch the quad and break it into 8 bytes for easy mixin'!
                GetCursorQuad(cursorAddress);

                int cursorStartByte = _cursorX;
                int cursByte = 0;

                // The "slow" loop mixes in the cursor as we go
                for (int w = 0; w < PERQ_DISPLAYWIDTH_IN_WORDS; w++)
                {
                    var word = TransformDisplayWord(_system.Memory.FetchWord(dispAddress + w));

                    // First the high byte...
                    if (dispByte >= cursorStartByte && dispByte < cursorStartByte + 8)
                    {
                        _scanlineData.Bytes[dispByte++] = TransformCursorByte((byte)((word & 0xff00) >> 8), _cursorData[cursByte++]);
                    }
                    else
                    {
                        _scanlineData.Bytes[dispByte++] = (byte)((word & 0xff00) >> 8);
                    }

                    // Now the low byte
                    if (dispByte >= cursorStartByte && dispByte < cursorStartByte + 8)
                    {
                        _scanlineData.Bytes[dispByte++] = TransformCursorByte((byte)(word & 0x00ff), _cursorData[cursByte++]);
                    }
                    else
                    {
                        _scanlineData.Bytes[dispByte++] = (byte)(word & 0x00ff);
                    }
                }
            }
            else
            {
                // Set the start of this scanline, offset from start of display (by quads)
                int dispAddress = (_displayAddress / 4) + (renderLine * PERQ_DISPLAYWIDTH_IN_QUADS);

                // The "fast" loop gobbles up quad words
                for (int w = 0; w < PERQ_DISPLAYWIDTH_IN_QUADS; w++)
                {
                    _scanlineData.Quads[w] = TransformDisplayQuad(_system.Memory.FetchQuad(dispAddress + w));
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
        /// Transforms the display word based on the current Cursor function.
        /// </summary>
        /// <remarks>
        /// Will probably change the "broken" ones to display _something_ rather
        /// than nothing... although a POS test program will need to be written to
        /// see all the combinations.  Remember, when the screen is shrunk the
        /// bottom part can be forced on, off, or complemented... 
        /// </remarks>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private ushort TransformDisplayWord(ushort word)
        {
            return (ushort)(TransformDisplayQuad(word) & 0xffff);
        }

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

                default:
                    throw new ArgumentOutOfRangeException("Unexpected cursor function value.");
            }
        }

        /// <summary>
        /// Transforms the cursor byte based on the current Cursor function and display byte.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private byte TransformCursorByte(byte dispByte, byte cursByte)
        {
            switch (_cursorFunc)
            {
                case CursorFunction.CTInvBlackHole:
                case CursorFunction.CTWhite:
                    return (byte)cursByte;

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

        /// <summary>
        /// Various handy portrait display constants -- for now...
        /// </summary>
        public static int PERQ_DISPLAYWIDTH = 768;
        public static int PERQ_DISPLAYWIDTH_IN_QUADS = 12;
        public static int PERQ_DISPLAYWIDTH_IN_WORDS = 48;
        public static int PERQ_DISPLAYWIDTH_IN_BYTES = 96;
        public static int PERQ_DISPLAYHEIGHT = 1024;

        private readonly ulong _scanLineTimeNsec = 70 * 170;
        private readonly ulong _hBlankTimeNsec = 22 * 170;
        private static int _lastVisibleScanLine = PERQ_DISPLAYHEIGHT - 1;

        private ScanLineBuffer _scanlineData;
        private byte[] _cursorData;

        private VideoState _state;
        private Event _currentEvent;
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
