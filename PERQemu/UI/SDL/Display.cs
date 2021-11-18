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
//using System.Windows.Forms;
//using System.Drawing;
//using System.Drawing.Imaging;
using System.Threading;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace PERQemu.UI
{
    /// <summary>
    /// This implements only the bits necessary to blit a chunk of memory to a
    /// window and do keyboard/mouse input. The actual interrupt/IO/Rendering
    /// logic is handled by the VideoController class, which is responsible for
    /// invoking Refresh() when a display frame is ready.
    ///
    /// Since we only ever want to have one of these it makes sense to have this
    /// be a singleton.
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

            _sdlRunning = false;
            _sdlPumpEvent = null;
            _renderEventType = 0;
            _textureLock = new ReaderWriterLockSlim();

            _stopwatch = new Stopwatch();
            _frame = 0;
            _prevClock = 0;

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

            SDL.SDL_SetHint("SDL_WINDOWS_DISABLE_THREAD_NAMING", "1");

            // Necessary?
            SDL.SDL_SetMainReady();

            // Get SDL humming
            if ((retVal = SDL.SDL_Init(SDL.SDL_INIT_EVERYTHING)) < 0)
            {
                throw new InvalidOperationException(string.Format("SDL_Init failed.  Error {0:x}", retVal));
            }

            if (SDL.SDL_SetHint(SDL.SDL_HINT_RENDER_SCALE_QUALITY, "0") == SDL.SDL_bool.SDL_FALSE)
            {
                throw new InvalidOperationException("SDL_SetHint failed to set scale quality.");
            }

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

            CreateDisplayTexture(false);

            // todo: initialize in our slightly greenish/grayish background color :-)
            // during "warmup" we'll fade in to full brightness.  because why not.
            SDL.SDL_SetRenderDrawColor(_sdlRenderer, 0x04, 0x4f, 0x04, 0xff);

            // Set up our custom event, if not already done.
            if (_renderEventType == 0)
            {
                // Register a User event for rendering.
                _renderEventType = SDL.SDL_RegisterEvents(1);
                _renderEvent = new SDL.SDL_Event();
                _renderEvent.type = (SDL.SDL_EventType)_renderEventType;
            }

            _stopwatch.Start();     // todo: could we use the SDL timer, if this is redundant?

            _sdlRunning = true;

            SDL.SDL_PumpEvents();   // so the mac will render the bloody window frame
            Reset();                // kick off our periodic polling loop
        }

        private void CreateDisplayTexture(bool filter)
        {
            _textureLock.EnterWriteLock();
            SDL.SDL_SetHint(SDL.SDL_HINT_RENDER_SCALE_QUALITY, filter ? "linear" : "nearest");

            _displayTexture = SDL.SDL_CreateTexture(
                _sdlRenderer,
                SDL.SDL_PIXELFORMAT_ARGB8888,
                (int)SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_STREAMING,
                _displayWidth,
                _displayHeight);

            if (_displayTexture == IntPtr.Zero)
            {
                throw new InvalidOperationException("SDL_CreateTexture failed.");
            }

            SDL.SDL_SetTextureBlendMode(_displayTexture, SDL.SDL_BlendMode.SDL_BLENDMODE_NONE);
            _textureLock.ExitWriteLock();
        }

        /// <summary>
        /// Expand the 1-bit pixels from the PERQ into 32-bit SDL pixels, one
        /// full scanline at a time.  Use the precomputed table to go faster.
        /// </summary>
        public void DrawScanline(int scanline, byte[] scanlineData)
        {
            int rgbIndex = scanline * _displayWidth;

            for (int i = 0; i < scanlineData.Length; i++)
            {
                for (int bit = 7; bit >= 0; bit--)
                {
                    _32bppDisplayBuffer[rgbIndex++] = _bitToPixel[scanlineData[i], bit];
                }
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
        /// Run the SDL message loop.  (For now!) Relies on the PERQ scheduler to
        /// check the loop ~ 16ms, which is plenty fast to poll for mouse, keyboard
        /// and screen refresh events at 60fps.  FIXME: move to another polling or
        /// scheduling mechanism -- like, interleaved with the CLI ReadKey call so
        /// the console and GUI may appear to run simultaneously?
        /// </summary>
        private void SDLMessageLoop(ulong skewNsec, object context)
        {
            SDL.SDL_Event e;

            // Run main message loop
            while (SDL.SDL_PollEvent(out e) != 0)
            {
                SDLMessageHandler(e);
            }

            // Reschedule to poll again at a reasonable interval
            // let's try 60ish per second? At least for now. Increase it if my
            // poor old Mac ever comes close to 60fps :-)
            _sdlPumpEvent = _system.Scheduler.Schedule((16 * Conversion.MsecToNsec), SDLMessageLoop);
        }

        /// <summary>
        /// Handle SDL events.  This executes in the UI context (main thread).
        /// </summary>
        private void SDLMessageHandler(SDL.SDL_Event e)
        {
            switch (e.type)
            {
                case SDL.SDL_EventType.SDL_USEREVENT:
                    // This should always be the case since we only define one
                    // user event, but just to be truly pedantic...
                    if (e.user.type == _renderEventType)
                    {
                        RenderDisplay();
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
                    PERQemu.Controller.PowerOff();  // Hit the brakes!
                    ShutdownSDL();                  // Now clobber the window
                    break;

                default:
                    break;
            }
        }

        /// <summary>
        /// Paint the completed frame on the screen.
        /// </summary>
        private void RenderDisplay()
        {
            // todo: figure out some kind of frame dropping for slow hosts
            // (consult Settings.RateLimit) but in "fast" mode just run 'em
            // as fast as the cpu can go?  (that might make the clocks run
            // too fast, but that'd be a nice problem to have...)
            // --> perhaps gate with a waithandle (fairly fast, lightweight)?

            IntPtr textureBits = IntPtr.Zero;
            int pitch = 0;

            // Stuff the display data into the display texture
            SDL.SDL_LockTexture(_displayTexture, IntPtr.Zero, out textureBits, out pitch);
            Marshal.Copy(_32bppDisplayBuffer, 0, textureBits, _32bppDisplayBuffer.Length);
            SDL.SDL_UnlockTexture(_displayTexture);

            // Render the display texture to the renderer
            SDL.SDL_RenderCopy(_sdlRenderer, _displayTexture, IntPtr.Zero, IntPtr.Zero);

            // And show it to us.
            SDL.SDL_RenderPresent(_sdlRenderer);

            // Update FPS count
            _frame++;

            if (_frame > 180)       // todo: configurable? :-)
            {
                UpdateFPS(_frame);
                _frame = 0;
            }
        }

        /// <summary>
        /// Update the FPS counter.
        /// </summary>
        /// <remarks>
        /// For now(?) this is in the title bar, but it could be made optional
        /// (or only in DEBUG mode?) or moved to a footer area where other status
        /// info, like caps lock or mouse capture status is displayed...
        /// </remarks>
        private void UpdateFPS(long frames)
        {
            _stopwatch.Stop();
            var elapsed = _stopwatch.ElapsedMilliseconds;
            _stopwatch.Restart();

            double inst = _system.CPU.Clocks - _prevClock;
            double fps = frames / (elapsed * Conversion.MsecToSec);
            double ns = (elapsed * (double)Conversion.MsecToNsec) / inst;
            _prevClock = _system.CPU.Clocks;

            SDL.SDL_SetWindowTitle(_sdlWindow, string.Format("PERQ - {0:N2} frames / sec, {1:N2}ns / cycle", fps, ns));
            Console.WriteLine("inst={0} elapsed={1} ns={2}", inst, elapsed, ns);  // raw / debug
        }

        /// <summary>
        /// Reset, restart the message loop.
        /// </summary>
        public void Reset()
        {
            if (_sdlRunning)
            {
                if (_sdlPumpEvent != null)
                    _system.Scheduler.Cancel(_sdlPumpEvent);

                SDLMessageLoop(0, null);
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
                //
                // Shut things down nicely.
                //
                _stopwatch.Stop();

                // Clear out our custom events
                SDL.SDL_FlushEvent((SDL.SDL_EventType)_renderEventType);

                if (_sdlPumpEvent != null)
                {
                    _system.Scheduler.Cancel(_sdlPumpEvent);
                    _sdlPumpEvent = null;
                }

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

                _sdlRunning = false;

                SDL.SDL_Quit();
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
                    _system.Break();
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
            Console.WriteLine("_sdlRunning={0}, _sdlPumpEvent={1}, _renderEventType={2}",
                              _sdlRunning, _sdlPumpEvent?.EventCallback.Method.Name, _renderEventType);
            Console.WriteLine("_frame={0}, _mouseX,Y={1},{2}",
                             _frame, _mouseX, _mouseY);
        }

        /// <summary>
        /// A hack to speed up pixel operations, inspired by Contralto.  As each
        /// scanline is drawn, the 1bpp PERQ data is expanded into SDL's 32bpp
        /// color pixels.  Rather than shift, mask and conditionally set every
        /// pixel on every line at 60fps, we precompute all that ONCE at startup
        /// then look up and copy 8 values in a tight loop.  Nanoseconds count!
        /// </summary>
        private static void MakePixelExpansionTable()
        {
            _bitToPixel = new int[256, 8];

            for (int i = 0; i < 256; i++)
            {
                for (int b = 0; b < 8; b++)
                {
                    // TODO: Accurately render Clinton P104 phosphor color? :-)
                    // For now, make it subtly more bluish white.
                    uint color = (i & (1 << b)) == 0 ? 0xfff0f0ff : 0xff000000;
                    _bitToPixel[i, b] = (int)color;
                }
            }
        }

        //
        // Display
        //
        private int _displayWidth;
        private int _displayHeight;

        // Buffer for rendering pixels.  SDL doesn't support 1bpp pixel formats,
        // so to keep things simple we use an array of ints and a 32bpp format.
        // What's a few extra megabytes between friends.
        private int[] _32bppDisplayBuffer;

        // Table of precomputed pixel expansions
        private static int[,] _bitToPixel;

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
        private Stopwatch _stopwatch;
        private long _frame;
        private ulong _prevClock;

        //
        // SDL
        //
        private IntPtr _sdlWindow = IntPtr.Zero;
        private IntPtr _sdlRenderer = IntPtr.Zero;

        // Rendering textures
        private IntPtr _displayTexture = IntPtr.Zero;
        private ReaderWriterLockSlim _textureLock;

        // Events and stuff
        private UInt32 _renderEventType;
        private SDL.SDL_Event _renderEvent;

        private delegate void DisplayDelegate();
        private delegate void SDLMessageHandlerDelegate(SDL.SDL_Event e);

        private bool _sdlRunning;
        private Event _sdlPumpEvent;
    }
}

