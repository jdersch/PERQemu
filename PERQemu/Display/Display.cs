// display.cs - Copyright 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.HostInterface;

using System;
using System.Windows.Forms;
using System.Drawing;
using System.Drawing.Imaging;
using SDL2;
using System.Threading;
using System.Runtime.InteropServices;
using System.Diagnostics;

namespace PERQemu.Display
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
            _textureLock = new ReaderWriterLockSlim();

            _stopwatch = new Stopwatch();
            _frame = 0;
            _prevClock = 0;

            _keymap = new KeyboardMap();

            _clickFlag = false;
            _mouseButton = 0x0;
            _mouseX = 0;
            _mouseY = 0;
        }

        /// <summary>
        /// Send a render event to the SDL message loop so the screen will get drawn.
        /// </summary>
        public void Refresh()
        {
            SDL.SDL_PushEvent(ref _renderEvent);
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


        public void SaveScreenshot(string path)
        {
            EncoderParameters p = new EncoderParameters(1);
            p.Param[0] = new EncoderParameter(Encoder.Quality, 100L);
            //_buffer.Save(path, GetEncoderForFormat(ImageFormat.Jpeg), p);
        }

        public int MouseX
        {
            get { return _mouseX; }
        }

        public int MouseY
        {
            get { return _mouseY; }
        }

        public int MouseButton
        {
            get { return _mouseButton; }
        }

        public bool MouseOffTablet
        {
            get { return _mouseOffTablet; }
        }

        //private void SDLMessageLoopThread()
        public void SDLMessageLoop()
        {
            // Schedule an event to poll for events at REASONABLE intervals
            // let's try 60 per second? At least for now. Increase it if my
            // poor old Mac ever comes close to 60fps :-)
            _sdlPumpEvent = _system.Scheduler.Schedule((ulong)(16.6667 * Conversion.MsecToNsec), (skew, context) =>
            {
                SDL.SDL_Event e;

                //
                // Run main message loop
                //
                while (SDL.SDL_PollEvent(out e) != 0)
                {
                    SDLMessageHandler(e);
                }

                SDLMessageLoop();
            });
        }

        public void ShutdownSDL()
        {
            Console.WriteLine("SDLShutdown requested.");    // Debug

            if (_sdlRunning)
            {
                //
                // Shut things down nicely.
                //
                _stopwatch.Stop();

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

        public void Status()
        {
            Console.WriteLine("_sdlRunning={0}, _sdlPumpEvent={1}",
                              _sdlRunning, _sdlPumpEvent.EventCallback.ToString());
            Console.WriteLine("_frame={0}, _mouseX,Y={1},{2}",
                             _frame, _mouseX, _mouseY);
        }

        private void SDLMessageHandler(SDL.SDL_Event e)
        {
            // Handle current messages.  This executes in the UI context.
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

                case SDL.SDL_EventType.SDL_KEYDOWN:
                    OnKeyDown(e.key.keysym.sym);
                    break;

                case SDL.SDL_EventType.SDL_KEYUP:
                    OnKeyUp(e.key.keysym.sym);
                    break;

                case SDL.SDL_EventType.SDL_QUIT:
                    ShutdownSDL();
                    break;

                default:
                    break;
            }
        }

        private void RenderDisplay()
        {
            //
            // Stuff the display data into the display texture
            //
            IntPtr textureBits = IntPtr.Zero;
            int pitch = 0;

            SDL.SDL_LockTexture(_displayTexture, IntPtr.Zero, out textureBits, out pitch);

            Marshal.Copy(_32bppDisplayBuffer, 0, textureBits, _32bppDisplayBuffer.Length);

            SDL.SDL_UnlockTexture(_displayTexture);

            //
            // Render the display texture to the renderer
            //
            SDL.SDL_RenderCopy(_sdlRenderer, _displayTexture, IntPtr.Zero, IntPtr.Zero);

            //
            // And show it to us.
            //
            SDL.SDL_RenderPresent(_sdlRenderer);

            // Update FPS count
            _frame++;

            if (_frame > 180)
            {
                UpdateFPS(_frame);
                _frame = 0;
            }
        }

        private void UpdateFPS(long frames)
        {
            _stopwatch.Stop();
            var elapsed = _stopwatch.ElapsedMilliseconds;
            _stopwatch.Restart();

            double inst = _system.CPU.Clocks - _prevClock;
            double fps = (frames * 1000.0) / elapsed;
            double ns = (elapsed / inst) * 1000000.0;
            _prevClock = _system.CPU.Clocks;

            SDL.SDL_SetWindowTitle(_sdlWindow, String.Format("PERQ - {0:N2} frames / sec, {1:N2}ns / cycle", fps, ns));
        }

        /// <summary>
        /// Set up our SDL window and start the display machinery in motion.
        /// </summary>
        public void InitializeSDL()
        {
            int retVal;

            SDL.SDL_SetHint("SDL_WINDOWS_DISABLE_THREAD_NAMING", "1");

            // Get SDL humming
            if ((retVal = SDL.SDL_Init(SDL.SDL_INIT_EVERYTHING)) < 0)
            {
                throw new InvalidOperationException(String.Format("SDL_Init failed.  Error {0:x}", retVal));
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
                0);     // SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE); FIXME ugh, on a 1080p display SDL shrnks the window slightly to avoid the Dock if it's exposed so make this host specific?

            if (_sdlWindow == IntPtr.Zero)
            {
                throw new InvalidOperationException("SDL_CreateWindow failed.");
            }

            _sdlRenderer = SDL.SDL_CreateRenderer(_sdlWindow, -1, SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED);
            if (_sdlRenderer == IntPtr.Zero)
            {
                Console.WriteLine("Could not create accelerated renderer?");    // FIXME fyi, debug, remove me

                // Fall back to software
                _sdlRenderer = SDL.SDL_CreateRenderer(_sdlWindow, -1, SDL.SDL_RendererFlags.SDL_RENDERER_SOFTWARE);

                if (_sdlRenderer == IntPtr.Zero)
                {
                    // Still no luck.
                    throw new InvalidOperationException("SDL_CreateRenderer failed.");
                }
            }

            CreateDisplayTexture(false);

            SDL.SDL_SetRenderDrawColor(_sdlRenderer, 0x00, 0x00, 0x00, 0xff);

            _sdlRunning = true;

            // Register a User event for rendering.
            _renderEventType = SDL.SDL_RegisterEvents(1);
            _renderEvent = new SDL.SDL_Event();
            _renderEvent.type = (SDL.SDL_EventType)_renderEventType;

            _stopwatch.Start();

            SDL.SDL_PumpEvents();   // so the mac will render the bloody window frame
            SDLMessageLoop();       // kick off our periodic polling loop
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

        void OnMouseWheel(object sender, MouseEventArgs e)
        {
            _clickFlag = e.Delta > 0;

            /*
            if (_clickFlag)
            {
                _dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;
            }
            else
            {
                _dispBox.Top = 0;
            }
            */
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
        /// Handles keyboard input from the Host side, handling "special" keys locally.
        /// Key translation is then done, and applicable results are queued on the Z80
        /// keyboard input buffer.
        /// </summary>
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
                // don't have) scroll wheels.
                case SDL.SDL_Keycode.SDLK_HOME:
                case SDL.SDL_Keycode.SDLK_PAGEUP:
                    //_dispBox.Top = 0;                    
                    handled = true;
                    break;

                case SDL.SDL_Keycode.SDLK_END:
                case SDL.SDL_Keycode.SDLK_PAGEDOWN:
                    //_dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;                    
                    handled = true;
                    break;

                // Toggle the "lock" keys... this needs work.
                case SDL.SDL_Keycode.SDLK_CAPSLOCK:
                case SDL.SDL_Keycode.SDLK_NUMLOCKCLEAR:
                case SDL.SDL_Keycode.SDLK_SCROLLLOCK:
                    _keymap.SetLockKeyState(keycode);
                    handled = true;
                    break;

                // Quirks: On Windows, the Control, Shift and Alt keys repeat when held down even
                // briefly.  The PERQ never needs to receive a plain modifier key event like that;
                // it's just a lot of noise, so skip the mapping step and quietly handle them here
                // (though they are still checked below for mouse options).
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
                    // Since the PERQ doesn't _have_ an "Alt" key, just ignore these entirely?
                    handled = true;
                    break;

                case SDL.SDL_Keycode.SDLK_PAUSE:            // Windows keyboards
                case SDL.SDL_Keycode.SDLK_F8:               // Create a Mac equivalent...
                    // Provide a key to jump into the debugger when focus is on the PERQ,
                    // rather than select the console and hit ^C.
                    _system.Break();
                    handled = true;
                    break;
            }

            // If the key wasn't handled above, let's see if we can get the ASCII equivalent.
            if (!handled)
            {
                perqCode = _keymap.GetKeyMapping(keycode, _shift, _ctrl);
                if (perqCode != 0)
                {
                    _system.IOB.Z80System.Keyboard.QueueInput(perqCode);   // Ship it!
                    handled = true;
                }
            }

            //
            // The following allow special modifiers that make Kriz tablet / BitPadOne tablet
            // manipulation using a standard PC mouse easier:
            //  - If Alt is held down, the Kriz tablet is put into "puck off tablet" mode
            //    which allows relative mode to work better (though it's still pretty clumsy)
            //  - If Ctrl is held down, The Left mouse button simulates Kriz/GPIB middle button
            //    and the Right mouse button simulates GPIB button 4 (blue; n/a on Kriz)
            //

            /*
            if (e.Alt)
            {
                _mouseOffTablet = true;
            }

            if (e.Control)
            {
                _mouseAltButton = true;
            }
            */
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
            }

            /*
            // Reset mouse tweaks if modifier keys are released
            if (!e.Alt)
            {
                _mouseOffTablet = false;
            }

            if (!e.Control)
            {
                _mouseAltButton = false;
            }
            */
        }

        private ImageCodecInfo GetEncoderForFormat(ImageFormat format)
        {
            ImageCodecInfo[] codecs = ImageCodecInfo.GetImageDecoders();

            foreach (ImageCodecInfo codec in codecs)
            {
                if (codec.FormatID == format.Guid)
                {
                    return codec;
                }
            }
            return null;
        }

        /// <summary>
        /// A hack to speed up pixel operations.  As each scanline is drawn,
        /// we have to expand the 1bpp PERQ data into SDL's 32bpp color pixels.
        /// Rather than shift, mask and conditionally set every pixel on every
        /// line at 60fps, we precompute all that ONCE at startup and then just
        /// look up and copy 8 values in a tight loop.  Every nanosecond counts!
        /// </summary>
        private static void MakePixelExpansionTable()
        {
            _bitToPixel = new int[256, 8];

            for (int i = 0; i < 256; i++)
            {
                for (int b = 0; b < 8; b++)
                {
                    // TODO: Clinton P104 phosphor color? :-)
                    uint color = (i & (1 << b)) == 0 ? 0xfffeefff : 0xff000000;
                    _bitToPixel[i, b] = (int)color;
                }
            }
        }

        //
        // Display
        //
        private const int _displayWidth = 768;      // TODO: make configurable!
        private const int _displayHeight = 1024;    // Same for portrait, landscape

        // Buffer for rendering pixels.  SDL doesn't support 1bpp pixel formats,
        // so to keep things simple we use an array of ints and a 32bpp format.
        // What's a few extra megabytes between friends.
        private int[] _32bppDisplayBuffer = new int[(_displayWidth * _displayHeight)];

        // Table of precomputed pixel expansions
        private static int[,] _bitToPixel;

        // Mouse
        private int _mouseX;
        private int _mouseY;
        private int _mouseButton;
        private bool _clickFlag;

        // Mouse tweaks
        private bool _mouseOffTablet;
        private bool _mouseAltButton;

        // Keyboard Stuff
        private bool _shift;
        private bool _ctrl;
        private KeyboardMap _keymap;

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

        private Event _sdlPumpEvent;

        //private Thread _sdlThread;
        private bool _sdlRunning;
    }
}

