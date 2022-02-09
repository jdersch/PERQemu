//
// EventLoop.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using SDL2;

using System;
using System.Threading;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PERQemu.UI
{
    /// <summary>
    /// Manages the setup, execution and shutdown of the SDL2 library.  The
    /// Display class still manages the keyboard and mouse events, for now.
    /// [Work in progress.]
    /// </summary>
    public sealed class EventLoop
    {
        public EventLoop()
        {
            _sdlRunning = false;
            _timerHandle = -1;
            _dispatch = new Dictionary<SDL.SDL_EventType, SDLMessageHandlerDelegate>();
        }

        /// <summary>
        /// Set up our SDL window and start the display machinery in motion.
        /// </summary>
        public void InitializeSDL()
        {
            int retVal;

            if (_sdlRunning)
            {
                Log.Error(Category.Emulator, "** InitializeSDL called while already running!?");
                return;
            }

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
                throw new InvalidOperationException("SDL_SetHint failed to set scale quality");
            }

            // That's it; windows created by Display (and someday, a full GUI)
            _sdlRunning = true;

            // Set up a timer to periodically run the SDL event loop.  Should be
            // no more than 16.667ms if we're to maintain 60fps on the Display
            _timerHandle = HighResolutionTimer.Register(15d, PERQemu.GUI.SDLMessageLoop);
            HighResolutionTimer.Enable(_timerHandle, true);

            Console.WriteLine("[Initialized SDL on {0}]", Thread.CurrentThread.ManagedThreadId);
        }


        /// <summary>
        /// Attach a delegate for an SDL event.
        /// </summary>
        public void RegisterDelegate(SDL.SDL_EventType e, SDLMessageHandlerDelegate d)
        {
#if DEBUG
            if (d == null)
                throw new InvalidOperationException("Can't register null delegate");

            if (_dispatch.ContainsKey(e))
                throw new InvalidOperationException($"Delegate already registered for event type {e}");

            Console.WriteLine("Attached delegate for SDL event type " + e);
#endif
            _dispatch[e] = d;
        }

        /// <summary>
        /// Release a delegate for an SDL event.
        /// </summary>
        public void ReleaseDelegate(SDL.SDL_EventType e)
        {
            if (_dispatch.ContainsKey(e))
            {
                _dispatch[e] = null;
                Console.WriteLine("Released delegate for SDL event type " + e);
            }
        }

        /// <summary>
        /// Process any pending SDL events.  This must be run on the main thread
        /// (on Mac, maybe not on Windows/Linux?) or events will be quietly
        /// ignored, because reasons.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public void SDLMessageLoop(HRTimerElapsedEventArgs a)
        {
            SDL.SDL_Event e;

            if (_sdlRunning)
            {
                while (SDL.SDL_PollEvent(out e) != 0)
                {
                    SDLMessageHandler(e);
                }
            }
        }

        /// <summary>
        /// Handle SDL events.  This executes in the UI context (main thread).
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private void SDLMessageHandler(SDL.SDL_Event e)
        {
            // If a delegate is registered, pass it the event
            if (_dispatch.ContainsKey(e.type))
            {
                _dispatch[e.type].Invoke(e);
                return;
            }

            // Deal with the ones that fall through
            if (e.type == SDL.SDL_EventType.SDL_QUIT)
            {
                // Stop the virtual machine, which will call ShutdownSDL()
                PERQemu.Controller.PowerOff();
            }
            else
            {
                Console.WriteLine("Unhandled event type {0}, user.type {1}", e.type, e.user.type);
            }
        }

        /// <summary>
        /// Close down the display and free SDL resources.
        /// </summary>
        public void ShutdownSDL()
        {
            Log.Debug(Category.Display, "SDL Shutdown requested");

            if (_sdlRunning)
            {
                //// Disable and stop our timer
                if (_timerHandle >= 0)
                {
                    HighResolutionTimer.Unregister(_timerHandle);
                    _timerHandle = -1;
                }

                // Clear out our custom events.  No, Artoo, shut them all down!
                SDL.SDL_FlushEvents(SDL.SDL_EventType.SDL_USEREVENT, SDL.SDL_EventType.SDL_LASTEVENT);

                SDL.SDL_Quit();

                _sdlRunning = false;
            }
        }


        public delegate void SDLMessageHandlerDelegate(SDL.SDL_Event e);

        private bool _sdlRunning;
        private int _timerHandle;

        private Dictionary<SDL.SDL_EventType, SDLMessageHandlerDelegate> _dispatch;
        //private PERQSystem _system;
    }
}
