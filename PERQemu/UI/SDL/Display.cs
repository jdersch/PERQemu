//
// Display.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Threading;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

namespace PERQemu.UI
{
    /// <summary>
    /// This implements only the bits necessary to blit a chunk of memory to a
    /// window and do keyboard/mouse input. The actual interrupt/IO/Rendering
    /// logic is handled by the VideoController class, which is responsible for
    /// invoking Refresh() when a display frame is ready.
    /// </summary>
    /// <remarks>
    /// All SDL2 interactions are wrapped up here, though it'd be much better to
    /// move the event loop and keyboard/mouse handling somewhere else (especially
    /// if/when a proper GUI is defined).  Like that'll happen.
    /// </remarks>
    public sealed class Display
    {
        static Display()
        {
            MakePixelExpansionTable();
        }

        public Display(PERQSystem system)
        {
            _system = system;

            _sdlRunning = false;

            _renderEventType = 0;

            _fpsTimerId = -1;

            _last = 0;
            _frames = 0;
            _prevClock = 0;
            _prevZ80Clock = 0;

            _keymap = new KeyboardMap(_system.Config.Chassis);

            _mouseOffTablet = false;
            _mouseButton = 0x0;
            _mouseX = 0;
            _mouseY = 0;

            // Keep a local copy
            _displayWidth = _system.VideoController.DisplayWidth;
            _displayHeight = _system.VideoController.DisplayHeight;

            // Allocate our hunka hunka burnin' pixels
            _32bppDisplayBuffer = new int[(_displayWidth * _displayHeight)];

            // And our intermediate one
            _8bppDisplayBuffer = new long[(_displayWidth / 8 * _displayHeight)];
        }

        public int MouseX => _mouseX;
        public int MouseY => _mouseY;
        public int MouseButton => _mouseButton;
        public bool MouseOffTablet => _mouseOffTablet;


        /// <summary>
        /// Set up our SDL window and start the display machinery in motion.
        /// </summary>
        public void InitializeSDL()
        {
            int retVal;

            if (_sdlRunning)
            {
                Trace.Log(LogType.Errors, "** InitializeSDL called while already running!?");
                return;
            }
            Console.WriteLine("[Initializing SDL on {0}]", Thread.CurrentThread.ManagedThreadId);

            // Necessary?  Helpful?
            SDL.SDL_SetMainReady();
            SDL.SDL_SetHint("SDL_WINDOWS_DISABLE_THREAD_NAMING", "1");

            // Get SDL humming
            if ((retVal = SDL.SDL_Init(SDL.SDL_INIT_EVERYTHING)) < 0)
            {
                throw new InvalidOperationException(string.Format("SDL_Init failed.  Error {0:x}", retVal));
            }

            if (SDL.SDL_SetHint(SDL.SDL_HINT_RENDER_SCALE_QUALITY, "0") == SDL.SDL_bool.SDL_FALSE)
            {
                throw new InvalidOperationException("SDL_SetHint failed to set scale quality.");
            }

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
                throw new InvalidOperationException("SDL_CreateWindow failed.");
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
                    throw new InvalidOperationException("SDL_CreateRenderer failed.");
                }
            }

            // todo: setting the renderer's "logical size" may help with the scaling
            // issue on displays too short to hold the entire screen?
            // SDL.SDL_RenderSetLogicalSize(_sdlRenderer, w, h);
            // todo: initialize in our slightly greenish/grayish background color :-)
            // during "warmup" we'll fade in to full brightness.  because why not.
            SDL.SDL_SetRenderDrawColor(_sdlRenderer, 0x04, 0xf4, 0x04, 0xff);

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

            SDL.SDL_SetTextureBlendMode(_displayTexture, SDL.SDL_BlendMode.SDL_BLENDMODE_NONE);

            // Set up our custom SDL render event, if not already done
            if (_renderEventType == 0)
            {
                // So, you have to "register" event types, the first of which
                // *just happens* to be SDL_USEREVENT, but you can't actually
                // _use_ them in the message handling loop (because C# insists
                // that the selector be a constant type, of course it does) so
                // just what the hell is the point of this, then?
                _renderEventType = SDL.SDL_RegisterEvents(1);
                _renderEvent = new SDL.SDL_Event();
                _renderEvent.type = (SDL.SDL_EventType)_renderEventType;
                _renderEvent.user.code = RENDER_FRAME;

                _fpsUpdateEvent = new SDL.SDL_Event();
                _fpsUpdateEvent.type = (SDL.SDL_EventType)_renderEventType;
                _fpsUpdateEvent.user.code = UPDATE_FPS;
            }

            // Register a timer and callback to update the FPS display
            if (_fpsTimerId < 0)
            {
                _fpsTimerCallback = new HRTimerElapsedCallback(RefreshFPS);
                _fpsTimerId = HighResolutionTimer.Register(3000d, _fpsTimerCallback);
            }

            _sdlRunning = true;

            HighResolutionTimer.Enable(_fpsTimerId, true);

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
            int rgbIndex = (_displayWidth / 8) * scanline;

            for (int i = 0; i < scanlineData.Length; i++)
            {
                _8bppDisplayBuffer[rgbIndex++] = _bitToPixel[scanlineData[i]];
            }
        }

        /// <summary>
        /// Send a render event to the SDL message loop so the screen will get drawn.
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
        /// Process any pending SDL events.  If the SDL window has not been
        /// initialized, this is a no-op.  This must be run on the main thread
        /// (on Mac, maybe not on Windows/Linux?) or events will be quietly
        /// ignored, because reasons.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SDLMessageLoop(ExecutionMode mode)
        {
            SDL.SDL_Event e;

            if (_sdlRunning)
            {
                if (mode == ExecutionMode.Synchronous)
                {
                    while (SDL.SDL_PollEvent(out e) != 0)
                    {
                        SDLMessageHandler(e);
                    }
                }
                else
                {
                    Console.WriteLine("[Entering SDLMessageLoop on {0}]", Thread.CurrentThread.ManagedThreadId);
                    while (SDL.SDL_WaitEvent(out e) != 0)
                    {
                        SDLMessageHandler(e);
                        if (_system.State != RunState.Running) break;
                    }
                    Console.WriteLine("[Exiting SDLMessageLoop]");
                }
            }
        }

        /// <summary>
        /// Handle SDL events.  This executes in the UI context (main thread).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void SDLMessageHandler(SDL.SDL_Event e)
        {
            switch (e.type)
            {
                case SDL.SDL_EventType.SDL_USEREVENT:
                    // <rant elided>
                    if (e.user.code == RENDER_FRAME)
                    {
                        RenderDisplay();
                    }
                    else if (e.user.code == UPDATE_FPS)
                    {
                        UpdateFPS();
                    }
                    break;

                case SDL.SDL_EventType.SDL_MOUSEMOTION:
                    OnMouseMove(e.motion.x, e.motion.y);
                    break;

                case SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN:
                    OnMouseDown(e.button.button);
                    break;

                case SDL.SDL_EventType.SDL_MOUSEBUTTONUP:
                    OnMouseUp(e.button.button);
                    break;

                case SDL.SDL_EventType.SDL_MOUSEWHEEL:
                    OnMouseWheel(e.wheel);
                    break;

                case SDL.SDL_EventType.SDL_KEYDOWN:
                    OnKeyDown(e.key.keysym.sym);
                    break;

                case SDL.SDL_EventType.SDL_KEYUP:
                    OnKeyUp(e.key.keysym.sym);
                    break;

                case SDL.SDL_EventType.SDL_QUIT:
                    // Stop the virtual machine, which will call ShutdownSDL()
                    PERQemu.Controller.PowerOff();
                    break;

                default:
                    //Console.WriteLine("Unhandled event type {0}, user.type {1}", e.type, e.user.type);
                    break;
            }
        }

        /// <summary>
        /// Paint the completed frame on the screen.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void RenderDisplay()
        {
            const uint WHITE = 0xfff0f0ff;
            const uint BLACK = 0xff000000;

            IntPtr textureBits = IntPtr.Zero;
            int pitch = 0;
            int j = 0;

            // Expand our "fake palletized" 8 bits to 32, manually, because all
            // the faffing around with SDL-CS surfaces is just a complete waste
            // of time (on Mono, anyway).  This is painfully stupid.  But doing
            // this here (on the main/SDL thread) boosts frame rates by 10fps.
            for (int i = 0; i < _8bppDisplayBuffer.Length; i++)
            {
                ulong quad = (ulong)_8bppDisplayBuffer[i];
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

            //// And show it to us
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
        private void UpdateFPS()
        {
            var now = HighResolutionTimer.ElapsedHiRes();
            var elapsed = now - _last;

            double inst = _system.CPU.Clocks - _prevClock;
            double z80inst = _system.IOB.Z80System.Clocks - _prevZ80Clock;
            double fps = _frames / (elapsed * Conversion.MsecToSec);
            double ns = (elapsed * Conversion.MsecToNsec) / inst;
            double zns = (elapsed * Conversion.MsecToNsec) / z80inst;
            _prevClock = _system.CPU.Clocks;
            _prevZ80Clock = _system.IOB.Z80System.Clocks;
            _last = now;
            _frames = 0;    // not safe.  don't even care anymore.

            var state = PERQemu.Sys.State;
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
        /// Close down the display and free SDL resources.
        /// </summary>
        public void ShutdownSDL()
        {
            Console.WriteLine("SDL Shutdown requested.");    // Debug

            if (_sdlRunning)
            {
                // Disable and stop our timer
                if (_fpsTimerId >= 0)
                {
                    HighResolutionTimer.Unregister(_fpsTimerId);
                    _fpsTimerId = -1;
                }

                // Make sure the PERQ has shut down and isn't firing events
                while (PERQemu.Controller.State != RunState.ShuttingDown &&
                       PERQemu.Controller.State != RunState.Off)
                {
                    Console.WriteLine("Waiting for PERQ to shut down...");
                    Thread.Sleep(25);
                }
                
                // Clear out our custom events
                SDL.SDL_FlushEvent((SDL.SDL_EventType)_renderEventType);

                //
                // Now shut things down nicely
                //
                if (_sdlRenderer != IntPtr.Zero)
                {
                    SDL.SDL_DestroyRenderer(_sdlRenderer);
                    _sdlRenderer = IntPtr.Zero;
                }

                if (_sdlWindow != IntPtr.Zero)
                {
                    SDL.SDL_DestroyWindow(_sdlWindow);
                    _sdlWindow = IntPtr.Zero;
                }

                SDL.SDL_Quit();

                _sdlRunning = false;
            }
        }

        //
        //  Mouse & Keyboard handling
        //

        void OnMouseWheel(SDL.SDL_MouseWheelEvent e)
        {
            if (e.y > 0)
            {
                /*
                    TODO/FIXME  have to test on a display that's too short
                    and figure out how to scroll the SDL display (scaling the
                    1bpp screen is way too ugly on a non-high dpi screen)
                */
                //Console.WriteLine("scroll up!");
                // _dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;
            }
            else if (e.y < 0)
            {
                //Console.WriteLine("scroll down!");
                // _dispBox.Top = 0;
            }
        }

        void OnMouseMove(int x, int y)
        {
            _mouseX = x;
            _mouseY = y;
        }

        /// <summary>
        /// Map the host mouse buttons to the Kriz tablet (passed straight through).
        /// The GPIB BitPad does its own mapping, since the four-button puck has a
        /// slightly strange layout.  Here's the chart:
        ///
        ///     host        Kriz        BitPad
        /// 0x8 XButton1    n/a         0x4 blue    or: alt+right
        /// 0x4 Right       0x4 right   0x8 green
        /// 0x2 Middle      0x2 middle  0x1 yellow  or: alt+left
        /// 0x1 Left        0x1 left    0x2 white
        /// 
        /// If we emulated the 1-button stylus or the 16-button mega puck we'd have
        /// to monkey with the mappings, but this is complicated enough...
        /// </summary>
        void OnMouseDown(byte button)
        {
            switch (button)
            {
                case 4:
                    _mouseButton = 0x8;                     // XButton
                    break;

                case 3:
                    _mouseButton = _ctrl ? 0x8 : 0x4;       // Right
                    break;

                case 2:
                    _mouseButton = 0x2;                     // Middle
                    break;

                case 1:
                    _mouseButton = _ctrl ? 0x2 : 0x1;       // Left
                    break;
            }
        }

        void OnMouseUp(byte button)
        {
            _mouseButton = 0x0;
        }

        /// <summary>
        /// Handles keyboard input from the Host side, handling "special" keys
        /// locally.  Key translation is then done, and applicable results are
        /// queued on the Z80 keyboard input buffer.
        /// </summary>
        /// <remarks>
        /// The following special modifiers make Kriz or BitPadOne tablet
        /// manipulation using a standard PC mouse easier:
        ///  - If Alt is held down, the Kriz tablet is put into "puck off tablet"
        ///    mode which allows relative mode to work better
        ///  - If Ctrl is held down, the mouse button mappings are altered to
        ///    allow a 3-button mouse emulate the 4-button GPIB puck (see above)
        /// </remarks>
        void OnKeyDown(SDL.SDL_Keycode keycode)
        {
            byte perqCode = 0;

            //
            // Handle any keys that may affect the Window itself, and are not passed
            // to the PERQ.
            //
            bool handled = false;
            switch (keycode)
            {
                // Allow Home/PageUp and End/PageDown keys to scroll the display.
                // Useful on laptop touchpads which don't simulate (or mice that
                // don't have) scroll wheels.  TODO: do SDL equivalent
                case SDL.SDL_Keycode.SDLK_HOME:
                    //_dispBox.Top = 0;                    
                    handled = true;
                    break;

                case SDL.SDL_Keycode.SDLK_END:
                    //_dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;                    
                    handled = true;
                    break;

                // Toggle the "lock" keys... this needs work.
                case SDL.SDL_Keycode.SDLK_CAPSLOCK:
                case SDL.SDL_Keycode.SDLK_NUMLOCKCLEAR:
                    _keymap.SetLockKeyState(keycode);
                    handled = true;
                    break;

                // Quirks: On Windows, the Control, Shift and Alt keys repeat when
                // held down even briefly.  The PERQ never needs to receive a plain
                // modifier key event like that, so skip the mapping step and quietly
                // handle them here.  TODO: WinForms did this; does SDL as well?
                case SDL.SDL_Keycode.SDLK_LSHIFT:
                case SDL.SDL_Keycode.SDLK_RSHIFT:
                    _shift = true;
                    handled = true;
                    break;

                case SDL.SDL_Keycode.SDLK_LCTRL:
                case SDL.SDL_Keycode.SDLK_RCTRL:
                    _ctrl = true;
                    handled = true;
                    break;

                case SDL.SDL_Keycode.SDLK_LALT:
                case SDL.SDL_Keycode.SDLK_RALT:
                case SDL.SDL_Keycode.SDLK_MENU:
                    _alt = true;
                    _mouseOffTablet = true;
                    handled = true;
                    break;

                // Provide a key to jump into the debugger when focus is on the PERQ,
                // rather than having to select the console window to hit ^C.
                case SDL.SDL_Keycode.SDLK_PAUSE:            // Windows keyboards
                case SDL.SDL_Keycode.SDLK_F8:               // Mac equivalent...
                    PERQemu.Controller.Break();
                    handled = true;
                    break;
            }

            // If the key wasn't handled above, see if there's an ASCII equivalent
            if (!handled)
            {
                perqCode = _keymap.GetKeyMapping(keycode, _shift, _ctrl);
                if (perqCode != 0)
                {
                    _system.IOB.Z80System.Keyboard.QueueInput(perqCode);   // Ship it!
                    handled = true;
                }
            }
        }

        /// <summary>
        /// Only used to handle the mouse button hacks.
        /// </summary>
        void OnKeyUp(SDL.SDL_Keycode keycode)
        {
            switch (keycode)
            {
                case SDL.SDL_Keycode.SDLK_LSHIFT:
                case SDL.SDL_Keycode.SDLK_RSHIFT:
                    _shift = false;
                    break;

                case SDL.SDL_Keycode.SDLK_LCTRL:
                case SDL.SDL_Keycode.SDLK_RCTRL:
                    _ctrl = false;
                    break;

                case SDL.SDL_Keycode.SDLK_LALT:
                case SDL.SDL_Keycode.SDLK_RALT:
                    _alt = false;
                    _mouseOffTablet = false;
                    break;
            }
        }

        //
        //  Other
        //

        public void SaveScreenshot(string path)
        {
            // TODO: have to rewrite for SDL2, which can only save as BMP!?
            // maybe we can fake up a WinForms-style bitmap directly from our
            // local copy of the perq's frame buffer and render that using the
            // jpg, png or other encoders...

            //EncoderParameters p = new EncoderParameters(1);
            //p.Param[0] = new EncoderParameter(Encoder.Quality, 100L);
            //_buffer.Save(path, GetEncoderForFormat(ImageForm, p);
        }

        //private ImageCodecInfo GetEncoderForFormat(ImageFormat format)
        //{
        //    ImageCodecInfo[] codecs = ImageCodecInfo.GetImageDecoders();

        //    foreach (ImageCodecInfo codec in codecs)
        //    {
        //        if (codec.FormatID == format.Guid)
        //        {
        //            return codec;
        //        }
        //    }
        //    return null;
        //}

        // todo: remove (eventually); for debugging
        public void Status()
        {
            Console.WriteLine("_sdlRunning={0}, _renderEventType={1}",
                              _sdlRunning, _renderEventType);
            Console.WriteLine("_frame={0} _mouseX,Y={1},{2} _alt={3}",
                              _frames, _mouseX, _mouseY, _alt);
        }

        /// <summary>
        /// A hack to speed up pixel operations, inspired by Contralto.  As each
        /// scanline is drawn, the 1bpp PERQ data is expanded into 8-bit pseudo-
        /// palletized pixels eight at a time.
        /// </summary>
        private static void MakePixelExpansionTable()
        {
            _bitToPixel = new long[256];
            ulong tmp;

            for (int i = 0; i < 256; i++)
            {
                tmp  = ((i & 0x01) != 0 ? 0x00000000000000ffU : 0U);
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
        private int _displayWidth;
        private int _displayHeight;

        // Buffers for rendering pixels.  The two-step shuffle blows a ton of
        // memory for a reasonable boost in speed, even though it makes that
        // vein in my temple throb.  What's a few dozen wasted megabytes
        // between friends?
        private int[] _32bppDisplayBuffer;
        private long[] _8bppDisplayBuffer;

        // Table of precomputed pixel expansions
        private static long[] _bitToPixel;

        // Mouse
        private int _mouseX;
        private int _mouseY;
        private int _mouseButton;
        private bool _mouseOffTablet;

        // Keyboard
        private bool _shift;
        private bool _ctrl;
        private bool _alt;
        private KeyboardMap _keymap;

        // Parent
        private PERQSystem _system;

        // Frame count
        private long _frames;
        private ulong _prevClock;
        private ulong _prevZ80Clock;
        private double _last;

        //
        // SDL
        //
        private IntPtr _sdlWindow = IntPtr.Zero;
        private IntPtr _sdlRenderer = IntPtr.Zero;

        // Rendering textures
        private IntPtr _displayTexture = IntPtr.Zero;

        // Events and stuff
        private UInt32 _renderEventType;
        private SDL.SDL_Event _renderEvent;
        private SDL.SDL_Event _fpsUpdateEvent;

        private const int RENDER_FRAME = 1;
        private const int UPDATE_FPS = 2;

        private int _fpsTimerId;
        private HRTimerElapsedCallback _fpsTimerCallback;

        //private delegate void DisplayDelegate();
        //private delegate void SDLMessageHandlerDelegate(SDL.SDL_Event e);

        private bool _sdlRunning;
    }
}

