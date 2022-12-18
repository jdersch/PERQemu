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
using System.Collections.Generic;
using System.Runtime.CompilerServices;

namespace PERQemu.UI
{
    /// <summary>
    /// Manages the setup, execution and shutdown of the SDL2 library.
    /// </summary>
    public sealed class EventLoop
    {
        public EventLoop()
        {
            _sdlRunning = false;
            _displayWindow = IntPtr.Zero;
            _timerHandle = -1;
            _uiEventDispatch = new Dictionary<SDL.SDL_EventType, SDLMessageHandlerDelegate>();

            _winFlags = 0;
            _winStateChanged = false;
            _resumeOnRestore = false;
        }

        public delegate void SDLMessageHandlerDelegate(SDL.SDL_Event e);

        public IntPtr DisplayWindow => _displayWindow;

        /// <summary>
        /// Initialize the SDL2 library.  Must be called from the main thread.
        /// </summary>
        public void InitializeSDL()
        {
            int retVal;

            if (_sdlRunning)
            {
                Log.Error(Category.Emulator, "InitializeSDL called while already running!?");
                return;
            }

            // Necessary?  Helpful?
            SDL.SDL_SetMainReady();
            SDL.SDL_SetHint("SDL_WINDOWS_DISABLE_THREAD_NAMING", "1");

            // Get SDL humming
            if ((retVal = SDL.SDL_Init(SDL.SDL_INIT_EVERYTHING)) < 0)
            {
                throw new InvalidOperationException($"SDL_Init failed.  Error {retVal}");
            }

            if (SDL.SDL_SetHint(SDL.SDL_HINT_RENDER_SCALE_QUALITY, "0") == SDL.SDL_bool.SDL_FALSE)
            {
                throw new InvalidOperationException("SDL_SetHint failed to set scale quality");
            }

            // Create system cursors
            _defaultCursor = SDL.SDL_CreateSystemCursor(SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_ARROW);
            _crossHairs = SDL.SDL_CreateSystemCursor(SDL.SDL_SystemCursor.SDL_SYSTEM_CURSOR_CROSSHAIR);

            if (_defaultCursor == IntPtr.Zero || _crossHairs == IntPtr.Zero)
            {
                throw new InvalidOperationException("SDL_CreateSystemCursor failed");
            }

            retVal = SDL_image.IMG_Init(SDL_image.IMG_InitFlags.IMG_INIT_JPG |
                                        SDL_image.IMG_InitFlags.IMG_INIT_PNG |
                                        SDL_image.IMG_InitFlags.IMG_INIT_TIF);
            if (retVal == 0)
            {
                throw new InvalidOperationException("Could not initialize SDL2 IMG library");
            }

            // That's it; windows created by Display (and someday, a full GUI)
            _sdlRunning = true;

            // Set up a timer to periodically run the SDL event loop.  To keep
            // overhead low when we start up, coalesce with the CLI timer (50ms)
            _timerHandle = HighResolutionTimer.Register(50d, PERQemu.GUI.SDLMessageLoop);
            HighResolutionTimer.Enable(_timerHandle, true);

            Log.Debug(Category.UI, "Initialized SDL");
        }

        /// <summary>
        /// Attach a PERQ Display window.  This kicks the SDL timer into high
        /// gear and enables our window event processing for cursor preference,
        /// pausing the emulator on minimize/restore, etc.
        /// </summary>
        public void AttachDisplay(IntPtr window)
        {
            if (_displayWindow != IntPtr.Zero)
            {
                throw new InvalidOperationException("AttachDisplay when already assigned");
            }

            Log.Debug(Category.UI, "Attaching the display");
            _displayWindow = window;
            _winFlags = SDL.SDL_GetWindowFlags(_displayWindow);
            _winStateChanged = true;

            // Adjust the timer for running the message loop.  It should be no
            // longer than 16.667ms if we're to maintain 60fps on the Display
            HighResolutionTimer.Enable(_timerHandle, false);
            HighResolutionTimer.Adjust(_timerHandle, 15d);
            HighResolutionTimer.Enable(_timerHandle, true);
        }

        /// <summary>
        /// Detach the display and downshift the timer.  The PERQ has gone away.
        /// </summary>
        public void DetachDisplay()
        {
            Log.Debug(Category.UI, "Detaching the display");
            _displayWindow = IntPtr.Zero;
            _winFlags = 0;
            _winStateChanged = false;

            // Pump the brakes
            HighResolutionTimer.Enable(_timerHandle, false);
            HighResolutionTimer.Adjust(_timerHandle, 50d);
            HighResolutionTimer.Enable(_timerHandle, true);
        }

        /// <summary>
        /// Attach a delegate for an SDL event.
        /// </summary>
        public void RegisterDelegate(SDL.SDL_EventType e, SDLMessageHandlerDelegate d)
        {
            if (d == null)
                throw new InvalidOperationException("Can't register null delegate");

            if (_uiEventDispatch.ContainsKey(e))
                throw new InvalidOperationException($"Delegate already registered for event type {e}");

            _uiEventDispatch.Add(e, d);
            Log.Detail(Category.UI, "Attached delegate for SDL event type {0}", e);
        }

        /// <summary>
        /// Release a delegate for an SDL event.
        /// </summary>
        public void ReleaseDelegate(SDL.SDL_EventType e)
        {
            if (_uiEventDispatch.Remove(e))
            {
                Log.Detail(Category.UI, "Released delegate for SDL event type {0}", e);
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

                if (_winStateChanged)
                {
                    UpdateWindowState();
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
            if (_uiEventDispatch.ContainsKey(e.type))
            {
                _uiEventDispatch[e.type].Invoke(e);
                return;
            }

            // Dispatch on Window events if a delegate is registered
            if (e.type == SDL.SDL_EventType.SDL_WINDOWEVENT && _displayWindow != IntPtr.Zero)
            {
                // If we ever have more than one window, ship event as a param
                var winEvent = e.window.windowEvent;

                switch (winEvent)
                {
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_ENTER:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_FOCUS_GAINED:
                        FocusCursor();
                        return;

                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_LEAVE:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_CLOSE:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_FOCUS_LOST:
                        ReleaseCursor();
                        return;

                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_SHOWN:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_HIDDEN:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_MINIMIZED:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_MAXIMIZED:
                    case SDL.SDL_WindowEventID.SDL_WINDOWEVENT_RESTORED:
                        // Accumulate and defer changes...
                        _winStateChanged = true;
                        return;

                    default:
                        Log.Detail(Category.UI, "Unhandled window event {0}", winEvent);
                        break;
                }
            }

            // Deal with the ones that fall through
            if (e.type == SDL.SDL_EventType.SDL_QUIT)
            {
                // Stop the virtual machine
                PERQemu.Controller.PowerOff();

                // todo: so if we have more than one window, does SDL fire a
                // quit message when any of them closes or only the last one?
            }
#if DEBUG
            else
            {
                Log.Detail(Category.UI, "Unhandled event type {0}, user.type {1}", e.type, e.user.type);
            }
#endif
        }

        /// <summary>
        /// Updates the state of the window based on the current flags.  Used to
        /// coalesce multiple updates when the events come in a flurry; resets the
        /// changed flag.
        /// </summary>
        private void UpdateWindowState()
        {
            var flags = SDL.SDL_GetWindowFlags(_displayWindow);
            Log.Detail(Category.UI, "Update: Window flags {0}", flags);

            if (flags != _winFlags)
            {
                if (((SDL.SDL_WindowFlags)flags & SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN) == 0)
                {
                    HideOrMinimize((SDL.SDL_WindowFlags)flags);
                }
                else
                {
                    UnhideOrRestore((SDL.SDL_WindowFlags)flags);
                }

                _winFlags = flags;
            }

            _winStateChanged = false;
        }

        /// <summary>
        /// Called when the Display window is minimized or hidden; if the
        /// PauseWhenMinimized setting is true, pause the emulator.
        /// </summary>
        private void HideOrMinimize(SDL.SDL_WindowFlags flags)
        {
            // Pause only if minimized or "iconified", not just hidden?  Splitting hairs?
            if (((flags & SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED) != 0) &&
                (Settings.PauseWhenMinimized && (PERQemu.Controller.State == RunState.Running)))
            {
                _resumeOnRestore = true;

                Log.Info(Category.Controller, "[Pausing execution]");
                PERQemu.Controller.TransitionTo(RunState.Paused);
            }
        }

        /// <summary>
        /// If we're embiggened, unpause if paused.
        /// </summary>
        private void UnhideOrRestore(SDL.SDL_WindowFlags flags)
        {
            // SDL2 on Windows will set *both* SHOWN and MINIMIZED at the same time
            // because THAT makes sense... if the "auto hide task bar" Windows setting
            // is enabled, the window will not restore unless you force it with CTRL-
            // SHIFT-right click.  Just shoot me now.  This hack may or may not help:
            if (((SDL.SDL_WindowFlags)_winFlags & SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED) != 0 &&
                (flags & SDL.SDL_WindowFlags.SDL_WINDOW_MINIMIZED) == 0)
            {
                SDL.SDL_RestoreWindow(_displayWindow);
            }

            if (_resumeOnRestore)
            {
                Log.Info(Category.Controller, "[Resuming execution]");
                PERQemu.Controller.TransitionTo(RunState.Running);
                _resumeOnRestore = false;
            }
        }

        /// <summary>
        /// Set our preferred cursor on window focus.
        /// </summary>
        private void FocusCursor()
        {
            if (Settings.CursorPreference == Cursor.Hidden)
            {
                SDL.SDL_ShowCursor(SDL.SDL_DISABLE);
                return;
            }

            if (Settings.CursorPreference == Cursor.Crosshairs)
            {
                SDL.SDL_SetCursor(_crossHairs);
                return;
            }
        }

        /// <summary>
        /// Restore the system cursor when leaving the window.
        /// </summary>
        private void ReleaseCursor()
        {
            SDL.SDL_SetCursor(_defaultCursor);
            SDL.SDL_ShowCursor(SDL.SDL_ENABLE);
        }

        /// <summary>
        /// Close down the timer and free SDL resources.
        /// </summary>
        public void ShutdownSDL()
        {
            Log.Debug(Category.UI, "SDL Shutdown requested");

            if (_sdlRunning)
            {
                // Disable and stop our timer
                if (_timerHandle >= 0)
                {
                    HighResolutionTimer.Unregister(_timerHandle);
                    _timerHandle = -1;
                }

                // Just in case?
                ReleaseCursor();

                // Free up stuff we allocated
                SDL.SDL_FreeCursor(_defaultCursor);
                SDL.SDL_FreeCursor(_crossHairs);

                // Clear out our custom events.  No, Artoo, shut them all down!
                SDL.SDL_FlushEvents(SDL.SDL_EventType.SDL_USEREVENT, SDL.SDL_EventType.SDL_LASTEVENT);

                SDL_image.IMG_Quit();
                SDL.SDL_Quit();

                _sdlRunning = false;
            }
        }


        private int _timerHandle;
        private bool _sdlRunning;
        private bool _resumeOnRestore;
        private bool _winStateChanged;
        private uint _winFlags;

        private IntPtr _displayWindow;
        private IntPtr _defaultCursor;
        private IntPtr _crossHairs;

        private Dictionary<SDL.SDL_EventType, SDLMessageHandlerDelegate> _uiEventDispatch;
    }
}

/*
    SDL Notes:
        EXPOSED events are ignored; because we update at (ideally) 60fps, the entire
        screen is refreshed often enough to ignore partial redraws (and these can come
        fast and furious in some situations).
        MOVED events are irrelevant and are ignored.
        Newer versions (Linux) also send "event 15" (window focus?) that are ignored.
        
    On MacOS X:
        Create window on power up sends SHOWN and RESTORED, with SHOWN flag set
        Hiding the app sends Hide event, sets HIDDEN
        Minimize button sends Hide event, sets HIDDEN and MINIMIZED
        Unhiding the app w/cmd-tab sends SHOWN event if HIDDEN;
            ALSO sends RESTORED event if MINIMIZED
        Clicking the icon in dock when minimized sends SHOWN, RESTORED events

    Windows:
        Create window on power up sends SHOWN, with SHOWN (and focus) flags set
        Hiding the app sends Hide event, with SHOWN *and* MINIMIZED flags?  WTF?
        *Can't get it back on-screen no matter what if "auto hide taskbar" is enabled*

    Linux:
        Create window sends Shown, Hide, Shown followed by Moved, Exposed, "event 15"
        Clicking minimize sends Hide event, with HIDDEN and MINIMIZED flags
        Restoring the window from the "dock" (Ubuntu) sends a Shown event with
            SHOWN and focus flags (+ event 15)

    Todo:
        Put back mouse wheel or PGUP/DN support for scrolling the display on
            short screens (or when obscured by the dock/taskbar)
        Test on high-DPI screens, multiple monitors
        Would be nice to allow full-screen/maximize option (full GUI so that DDS,
            floppy, pause/play/reset controls, etc could be integrated on screen)
*/
