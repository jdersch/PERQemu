//
// Display.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using SDL2;

using System;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

using PERQemu.UI.Output;

namespace PERQemu.UI
{
    /// <summary>
    /// This implements only the bits necessary to blit a chunk of memory to an
    /// SDL window.  The actual interrupt/IO/rendering logic is handled by the
    /// VideoController class, which is responsible for invoking Refresh() when
    /// a display frame is ready.
    /// </summary>
    public sealed class Display
    {
        static Display()
        {
            MakePixelExpansionTable();
        }

        public Display(PERQSystem system)
        {
            _system = system;

            _customEventType = 0;
            _fpsTimerId = -1;

            _last = 0;
            _frames = 0;
            _prevClock = 0;
            _prevZ80Clock = 0;
            _floppyActive = false;
            _streamerActive = false;

            // Keep a local copy
            _displayWidth = _system.VideoController.DisplayWidth;
            _displayHeight = _system.VideoController.DisplayHeight;

            // Allocate our hunka hunka burnin' pixels
            _32bppDisplayBuffer = new int[(_displayWidth * _displayHeight)];

            // And our intermediate one
            _8bppDisplayBuffer = new long[(_displayWidth / 8 * _displayHeight)];
        }

        /// <summary>
        /// Set up our SDL window and the rendering machinery.
        /// </summary>
        public void Initialize()
        {
            if (_sdlWindow != IntPtr.Zero)
            {
                Log.Error(Category.Display, "Initialize called while already running!?");
                return;
            }

            Log.Debug(Category.Display, "Initializing");

            //
            // Create the display window
            //
            _sdlWindow = SDL.SDL_CreateWindow(
                "PERQ",
                SDL.SDL_WINDOWPOS_UNDEFINED,
                SDL.SDL_WINDOWPOS_UNDEFINED,
                _displayWidth,
                _displayHeight,
                SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN);

            if (_sdlWindow == IntPtr.Zero)
            {
                throw new InvalidOperationException("SDL_CreateWindow failed");
            }

            //
            // Create the renderer
            //
            _sdlRenderer = SDL.SDL_CreateRenderer(_sdlWindow, -1, SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED);
            if (_sdlRenderer == IntPtr.Zero)
            {
                // Fall back to software
                _sdlRenderer = SDL.SDL_CreateRenderer(_sdlWindow, -1, SDL.SDL_RendererFlags.SDL_RENDERER_SOFTWARE);

                if (_sdlRenderer == IntPtr.Zero)
                {
                    // Still no luck.
                    throw new InvalidOperationException("SDL_CreateRenderer failed");
                }
            }

            // Setting the renderer's "logical size" may help with the scaling
            // issue on displays too short to hold the entire screen?
            SDL.SDL_RenderSetLogicalSize(_sdlRenderer, _displayWidth, _displayHeight);

            // Initialize in our slightly greenish/grayish background color :-)
            // during "warmup" we'll fade in to full brightness.  because why not.
            SDL.SDL_SetRenderDrawColor(_sdlRenderer, 0x40, 0x48, 0x40, 0xff);

            //
            // Create the display texture
            //
            _displayTexture = SDL.SDL_CreateTexture(
                _sdlRenderer,
                SDL.SDL_PIXELFORMAT_ARGB8888,
                (int)SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_STREAMING,
                _displayWidth,
                _displayHeight);

            if (_displayTexture == IntPtr.Zero)
            {
                throw new InvalidOperationException(
                    string.Format("SDL_CreateTexture failed: {0}", SDL.SDL_GetError()));
            }

            //
            // Some visual "sugar"
            //
            if (PERQemu.HostIsUnix)
            {
                // Create a surface from the program icon and assign it to the new
                // window.  This should help identify the "mono-sgen" process on
                // Mac and Linux systems in the desktop UI.

                var icon = SDL_image.IMG_Load(Paths.BuildResourcePath("icon.png"));

                SDL.SDL_SetWindowIcon(_sdlWindow, icon);
                SDL.SDL_FreeSurface(icon);
            }

            // Create a texture for our floppy activity "light"
            _floppyTexture = SDL_image.IMG_LoadTexture(_sdlRenderer, Paths.BuildResourcePath("floppy.png"));

            // Put the floppy icon in the lower right hand corner
            _floppyRect = new SDL.SDL_Rect();
            _floppyRect.h = 32;
            _floppyRect.w = 32;
            _floppyRect.x = _displayWidth - 36;
            _floppyRect.y = _displayHeight - 36;

            // Create a texture for our streamer tape activity "light"
            _streamerTexture = SDL_image.IMG_LoadTexture(_sdlRenderer, Paths.BuildResourcePath("qictape.png"));

            // Put the tape icon just to the left of the floppy
            _streamerRect = new SDL.SDL_Rect();
            _streamerRect.h = 32;
            _streamerRect.w = 32;
            _streamerRect.x = _floppyRect.x - 36;
            _streamerRect.y = _floppyRect.y;

            // Init the display texture
            SDL.SDL_SetTextureBlendMode(_displayTexture, SDL.SDL_BlendMode.SDL_BLENDMODE_NONE);
            SDL.SDL_RenderClear(_sdlRenderer);

            // Set up our custom SDL render event, if not already done
            if (_customEventType == 0)
            {
                // Take two, they're small
                _customEventType = (SDL.SDL_EventType)SDL.SDL_RegisterEvents(2);

                _renderEvent = new SDL.SDL_Event();
                _renderEvent.type = _customEventType;
                _renderEvent.user.code = RENDER_FRAME;

                _fpsUpdateEvent = new SDL.SDL_Event();
                _fpsUpdateEvent.type = _customEventType + 1;
                _fpsUpdateEvent.user.code = UPDATE_FPS;

                Log.Debug(Category.Display, "Registered SDL events {0} and {1}",
                          _renderEvent.type, _fpsUpdateEvent.type);
            }

            // Register callbacks for our render events
            PERQemu.GUI.RegisterDelegate(_renderEvent.type, RenderDisplay);
            PERQemu.GUI.RegisterDelegate(_fpsUpdateEvent.type, UpdateFPS);

            _system.FloppyActivity += OnFloppyActivity;
            _system.StreamerActivity += OnStreamerActivity;

            // Tell the SDL EventLoop we're here
            PERQemu.GUI.AttachDisplay(_sdlWindow);

            // Register a timer and callback to update the FPS display
            if (_fpsTimerId < 0)
            {
                _fpsTimerCallback = new HRTimerElapsedCallback(RefreshFPS);
                _fpsTimerId = HighResolutionTimer.Register(2000d, _fpsTimerCallback, "FPS");
                HighResolutionTimer.Enable(_fpsTimerId, true);
            }

            // Force one update so the Mac will render the bloody window frame
            SDL.SDL_PumpEvents();
        }

        /// <summary>
        /// Expand the 1-bit pixels from the PERQ into a local intermediate
        /// 8-bit pixel buffer, one full scanline at a time.  This runs on the
        /// CPU thread (ugh, fix this) so it has to be fast.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void DrawScanline(int scanline, byte[] scanlineData)
        {
            int byteIndex = (_displayWidth / 8) * scanline;

            for (int i = 0; i < scanlineData.Length; i++)
            {
                _8bppDisplayBuffer[byteIndex++] = _bitToPixel[scanlineData[i]];
            }
        }

        /// <summary>
        /// Called by the VideoController to send a render event to the SDL 
        /// message loop so the screen will get drawn.
        /// </summary>
        public void Refresh()
        {
            SDL.SDL_PushEvent(ref _renderEvent);
        }

        /// <summary>
        /// Queue up a render event to update the FPS display.
        /// </summary>
        public void RefreshFPS(HRTimerElapsedEventArgs a)
        {
            // Thread safe, so we can call it in our timer callback
            SDL.SDL_PushEvent(ref _fpsUpdateEvent);
        }

        /// <summary>
        /// Queue an event to hide the display window (for debugging).
        /// </summary>
        public void Hide()
        {
            var e = new SDL.SDL_Event();
            e.type = SDL.SDL_EventType.SDL_WINDOWEVENT;
            e.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_HIDDEN;

            SDL.SDL_PushEvent(ref e);
        }

        /// <summary>
        /// Queue an event to restore the display if minimized/hidden.
        /// </summary>
        public void Restore()
        {
            var e = new SDL.SDL_Event();
            e.type = SDL.SDL_EventType.SDL_WINDOWEVENT;
            e.window.windowEvent = SDL.SDL_WindowEventID.SDL_WINDOWEVENT_RESTORED;

            SDL.SDL_PushEvent(ref e);
        }

        /// <summary>
        /// Paint the completed frame on the screen.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        void RenderDisplay(SDL.SDL_Event e)
        {
            IntPtr textureBits = IntPtr.Zero;
            int pitch = 0;
            int j = 0;

            // Expand our "fake palletized" 8 bits to 32, manually, because all
            // the faffing around with SDL-CS surfaces is just a complete waste
            // of time (on Mono, anyway).  This is painfully stupid.  But doing
            // this here (on the main/SDL thread) boosts frame rates by 10fps.
            for (int i = 0; i < _8bppDisplayBuffer.Length; i++)
            {
                var quad = (ulong)_8bppDisplayBuffer[i];
                _32bppDisplayBuffer[j++] = (int)((quad & 0xff00000000000000U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x00ff000000000000U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x0000ff0000000000U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x000000ff00000000U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x00000000ff000000U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x0000000000ff0000U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x000000000000ff00U) != 0 ? BLACK : WHITE);
                _32bppDisplayBuffer[j++] = (int)((quad & 0x00000000000000ffU) != 0 ? BLACK : WHITE);
            }

            // Stuff the display data into the display texture
            SDL.SDL_LockTexture(_displayTexture, IntPtr.Zero, out textureBits, out pitch);
            Marshal.Copy(_32bppDisplayBuffer, 0, textureBits, _32bppDisplayBuffer.Length);
            SDL.SDL_UnlockTexture(_displayTexture);

            // Render the display texture to the renderer
            SDL.SDL_RenderCopy(_sdlRenderer, _displayTexture, IntPtr.Zero, IntPtr.Zero);

            // Overlay the activity icon if the floppy drive is busy
            if (_floppyActive)
            {
                SDL.SDL_RenderCopy(_sdlRenderer, _floppyTexture, IntPtr.Zero, ref _floppyRect);
            }

            // And the streamer, too!
            if (_streamerActive)
            {
                SDL.SDL_RenderCopy(_sdlRenderer, _streamerTexture, IntPtr.Zero, ref _streamerRect);
            }

            // And show it to us
            SDL.SDL_RenderPresent(_sdlRenderer);

            // Update FPS count
            _frames++;
        }

        /// <summary>
        /// Update the FPS counter.  This is slow and horrible but necessary for
        /// debugging.  It should be improved...
        /// </summary>
        /// <remarks>
        /// For now(?) this is in the title bar, but it could be made optional
        /// (or only in DEBUG mode?) or moved to a footer area where other status
        /// info, like caps lock or mouse capture status is displayed...
        /// </remarks>
        void UpdateFPS(SDL.SDL_Event e)
        {
            // Snapshot our data points
            var now = HighResolutionTimer.ElapsedHiRes();
            var elapsed = now - _last;

            var state = _system.State;

            var inst = _system.CPU.Clocks;
            var z80inst = _system.IOB.Z80System.Clocks;

            // Compute our instruction timing and frame rate
            double ns = (elapsed * Conversion.MsecToNsec) / (inst - _prevClock);
            double zns = (elapsed * Conversion.MsecToNsec) / (z80inst - _prevZ80Clock);
            double fps = _frames / (elapsed * Conversion.MsecToSec);

            // Save for next time
            _prevClock = inst;
            _prevZ80Clock = z80inst;
            _last = now;
            _frames = 0;    // not safe.  don't even care anymore.

            // Update the title bar
            if (state == RunState.Running)
            {
                SDL.SDL_SetWindowTitle(_sdlWindow,
                    string.Format("PERQ - {0:N2} fps, CPU {1:N2}ns, Z80 {2:N2}ns", fps, ns, zns));
            }
            else
            {
                SDL.SDL_SetWindowTitle(_sdlWindow,
                    string.Format("PERQ is {0}", ((state == RunState.RunInst) ||
                                                  (state == RunState.RunZ80Inst) ||
                                                  (state == RunState.SingleStep)) ?
                                                  "single stepping" : state.ToString()));
            }
        }

        /// <summary>
        /// Catch signals from the floppy controller to update our activity light.
        /// </summary>
        void OnFloppyActivity(MachineStateChangeEventArgs a)
        {
            _floppyActive = (bool)a.Args[0];        // Unit 0
        }

        /// <summary>
        /// Catch signals from the QIC tape controller to update our activity light.
        /// </summary>
        void OnStreamerActivity(MachineStateChangeEventArgs a)
        {
            _streamerActive = (bool)a.Args[0];
        }

        // TODO: add Canon printer activity light!

        /// <summary>
        /// Close down the display and free SDL resources.
        /// </summary>
        public void Shutdown()
        {
            Log.Write(Category.Display, "Shutdown requested");

            // Disable and stop our timer
            if (_fpsTimerId >= 0)
            {
                HighResolutionTimer.Unregister(_fpsTimerId);
                _fpsTimerId = -1;
            }

            // Clear out both of our custom events
            SDL.SDL_FlushEvents(_customEventType, _customEventType + 1);

            // Unregister our delegates
            PERQemu.GUI.ReleaseDelegate(_renderEvent.type);
            PERQemu.GUI.ReleaseDelegate(_fpsUpdateEvent.type);

            _system.FloppyActivity -= OnFloppyActivity;
            _system.StreamerActivity -= OnStreamerActivity;

            // Tell the EventLoop we're going away
            PERQemu.GUI.DetachDisplay();

            // Clear the renderer
            if (_sdlRenderer != IntPtr.Zero)
            {
                SDL.SDL_DestroyRenderer(_sdlRenderer);
                _sdlRenderer = IntPtr.Zero;
            }

            // And finally close down the window
            if (_sdlWindow != IntPtr.Zero)
            {
                SDL.SDL_DestroyWindow(_sdlWindow);
                _sdlWindow = IntPtr.Zero;
                Log.Detail(Category.UI, "[DestroyWindow called]");
            }
        }

        /// <summary>
        /// Save a screenshot.  Only PNG for now...
        /// </summary>
        /// <remarks>
        /// This uses the still-kinda-clunky Output routines to save 1bpp bitmaps
        /// to bespoke PNG (and possibly other) files.  It should probably move to
        /// another class or just use the bloody SDL_image routines to copy the
        /// bitmap -> Surface -> .Save() and be done with it... Testing shows that
        /// it isn't necessary to pause the PERQ during the capture, since this runs
        /// on the CLI/main thread so there's no chance of "shearing" or capturing a
        /// partial screen update.
        /// </remarks>
        public void SaveScreenshot(string path)
        {
            // Use the last full frame
            var screen = new byte[_32bppDisplayBuffer.Length / 8];
            var screenAddr = 0;

            // Convert back from 32bpp -> 1bpp (byte swap, bit swap and invert!)
            for (var i = 0; i < _32bppDisplayBuffer.Length; i += 16)
            {
                ushort w = 0;
                for (var j = 0; j < 16; j++)
                {
                    if ((_32bppDisplayBuffer[i + j] & 0x00ffffff) > 0) // WHITE
                        w |= (ushort)(1 << (15 - j));
                }
                screen[screenAddr++] = (byte)(w >> 8);
                screen[screenAddr++] = (byte)(w & 0xff);
            }

            // Set up the metadata
            string[] keys = { "Title", "Creation Time", "Software" };
            string[] values = { $"PERQ screenshot ({PERQemu.Config.Current.Name})",
                                DateTime.Now.ToString("yyyy-MM-dd'T'HH:mm:ss.ffK"),
                                PERQemu.Version };

            // Pass 'em to the formatter
            var png = new PNGFormatter(keys, values);

            // Populate the page and save it
            var page = new Page(100, new Region(0, 0, (uint)_displayWidth, (uint)_displayHeight));
            page.CopyBits(screen);

            using (var fs = new FileStream(path, FileMode.Create, FileAccess.Write))
            {
                png.Save(page, fs);
                fs.Close();
            }
        }

        /// <summary>
        /// Debugging output.  Should be cleaned up and/or limited to DEBUG builds?
        /// </summary>
        public void Status()
        {
            Console.WriteLine("renderEvent={0}, fpsUpdateEvent={1}, frames={2}",
                              _renderEvent.type, _fpsUpdateEvent.type, _frames);

            var flags = SDL.SDL_GetWindowFlags(_sdlWindow);
            Console.WriteLine("flags={0}", (SDL.SDL_WindowFlags)flags);
        }

        /// <summary>
        /// A hack to speed up pixel operations, inspired by Contralto.  As each
        /// scanline is drawn, the 1bpp PERQ data is expanded into 8-bit pseudo-
        /// palletized pixels eight at a time.
        /// </summary>
        static void MakePixelExpansionTable()
        {
            _bitToPixel = new long[256];
            ulong tmp;

            for (int i = 0; i < 256; i++)
            {
                tmp = ((i & 0x01) != 0 ? 0x00000000000000ffU : 0U);
                tmp |= ((i & 0x02) != 0 ? 0x000000000000ff00U : 0U);
                tmp |= ((i & 0x04) != 0 ? 0x0000000000ff0000U : 0U);
                tmp |= ((i & 0x08) != 0 ? 0x00000000ff000000U : 0U);
                tmp |= ((i & 0x10) != 0 ? 0x000000ff00000000U : 0U);
                tmp |= ((i & 0x20) != 0 ? 0x0000ff0000000000U : 0U);
                tmp |= ((i & 0x40) != 0 ? 0x00ff000000000000U : 0U);
                tmp |= ((i & 0x80) != 0 ? 0xff00000000000000U : 0U);

                _bitToPixel[i] = (long)tmp;
            }
        }


        //
        // Display
        //
        int _displayWidth;
        int _displayHeight;

        // Buffers for rendering pixels.  The two-step shuffle blows a ton of RAM
        // for a reasonable boost in speed, even though it makes that vein in my
        // temple throb.  What's a few dozen wasted megabytes between friends?
        int[] _32bppDisplayBuffer;
        long[] _8bppDisplayBuffer;

        // Table of precomputed pixel expansions
        static long[] _bitToPixel;

        // Adjust WHITE for slightly bluish tint of Clinton P-104 :-)
        const uint WHITE = 0xfff3f3ff;
        const uint BLACK = 0xff000000;

        // Frame count
        long _frames;
        ulong _prevClock;
        ulong _prevZ80Clock;
        double _last;

        //
        // SDL
        //
        IntPtr _sdlWindow = IntPtr.Zero;
        IntPtr _sdlRenderer = IntPtr.Zero;

        // Rendering textures
        IntPtr _displayTexture = IntPtr.Zero;

        // Events and stuff
        SDL.SDL_EventType _customEventType;
        SDL.SDL_Event _renderEvent;
        SDL.SDL_Event _fpsUpdateEvent;

        const int RENDER_FRAME = 1;
        const int UPDATE_FPS = 2;

        int _fpsTimerId;
        HRTimerElapsedCallback _fpsTimerCallback;

        // Floppy activity "light"
        static IntPtr _floppyTexture = IntPtr.Zero;
        SDL.SDL_Rect _floppyRect;
        bool _floppyActive;

        // Tape streamer activity light!
        static IntPtr _streamerTexture = IntPtr.Zero;
        SDL.SDL_Rect _streamerRect;
        bool _streamerActive;

        // Parent
        PERQSystem _system;

    }
}
