//
// InputDevices.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.UI
{
    /// <summary>
    /// Translate SDL2 keyboard and mouse events to PERQ keystrokes and
    /// tablet inputs.
    /// </summary>
    public class InputDevices
    {
        public InputDevices(PERQSystem sys)
        {
            _system = sys;

            _keymap = new KeyboardMap(_system.Config.Chassis);

            _mouseOffTablet = false;
            _mouseButton = 0x0;
            _mouseX = 0;
            _mouseY = 0;
        }

        public int MouseX => _mouseX;
        public int MouseY => _mouseY;
        public int MouseButton => _mouseButton;
        public bool MouseOffTablet => _mouseOffTablet;

        public void Initialize()
        {
            // Set up our callbacks
            PERQemu.GUI.RegisterDelegate(SDL.SDL_EventType.SDL_KEYUP, OnKeyUp);
            PERQemu.GUI.RegisterDelegate(SDL.SDL_EventType.SDL_KEYDOWN, OnKeyDown);
            PERQemu.GUI.RegisterDelegate(SDL.SDL_EventType.SDL_MOUSEWHEEL, OnMouseWheel);
            PERQemu.GUI.RegisterDelegate(SDL.SDL_EventType.SDL_MOUSEMOTION, OnMouseMove);
            PERQemu.GUI.RegisterDelegate(SDL.SDL_EventType.SDL_MOUSEBUTTONUP, OnMouseUp);
            PERQemu.GUI.RegisterDelegate(SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN, OnMouseDown);
        }

        public void Shutdown()
        {
            // Unhook 'em
            PERQemu.GUI.ReleaseDelegate(SDL.SDL_EventType.SDL_KEYUP);
            PERQemu.GUI.ReleaseDelegate(SDL.SDL_EventType.SDL_KEYDOWN);
            PERQemu.GUI.ReleaseDelegate(SDL.SDL_EventType.SDL_MOUSEWHEEL);
            PERQemu.GUI.ReleaseDelegate(SDL.SDL_EventType.SDL_MOUSEMOTION);
            PERQemu.GUI.ReleaseDelegate(SDL.SDL_EventType.SDL_MOUSEBUTTONUP);
            PERQemu.GUI.ReleaseDelegate(SDL.SDL_EventType.SDL_MOUSEBUTTONDOWN);
        }

        //
        //  Mouse & Keyboard handling
        //

        void OnMouseWheel(SDL.SDL_Event e)
        {
            
            if (e.wheel.y > 0)
            {
                /*
                    TODO/FIXME  have to test on a display that's too short
                    and figure out how to scroll the SDL display (scaling the
                    1bpp screen is way too ugly on a non-high dpi screen)
                */
                //Console.WriteLine("scroll up!");
                // _dispBox.Top = _display.ClientRectangle.Height - VideoController.PERQ_DISPLAYHEIGHT;
            }
            else if (e.wheel.y < 0)
            {
                //Console.WriteLine("scroll down!");
                // _dispBox.Top = 0;
            }
        }

        void OnMouseMove(SDL.SDL_Event e)
        {
            _mouseX = e.motion.x;
            _mouseY = e.motion.y;
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
        void OnMouseDown(SDL.SDL_Event e)
        {
            // todo: if the perq tracks individual button states, we should too?
            switch (e.button.button)
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

        void OnMouseUp(SDL.SDL_Event e)
        {
            _mouseButton = 0x0;     // todo: see above
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
        void OnKeyDown(SDL.SDL_Event e)
        {
            var keycode = e.key.keysym.sym;
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
        void OnKeyUp(SDL.SDL_Event e)
        {
            var keycode = e.key.keysym.sym;

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

        // debugging
        public void Status()
        {
            Console.WriteLine("mouseX,Y={0},{1} alt={2}, caps={3}",
                              _mouseX, _mouseY, _alt, _keymap.CapsLock);
        }

        // Mouse
        int _mouseX;
        int _mouseY;
        int _mouseButton;
        bool _mouseOffTablet;

        // Keyboard
        bool _shift;
        bool _ctrl;
        bool _alt;

        KeyboardMap _keymap;

        // Parent
        PERQSystem _system;
    }
}
