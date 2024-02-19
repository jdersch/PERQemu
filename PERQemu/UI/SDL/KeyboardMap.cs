//
// KeyboardMap.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

using PERQemu.Config;

namespace PERQemu.UI
{
    /// <summary>
    /// Hold all the variations of a particular key on either of the two PERQ
    /// keyboard types.
    /// </summary>
    public struct PERQKey
    {
        /// <summary>
        /// Shortcut for the PERQ-2 keypad: some keys aren't modified by shift OR control!
        /// </summary>
        public PERQKey(byte value)
        {
            Normal = Shift = Control = CtrlShift = value;
        }

        /// <summary>
        /// Shortcut for the PERQ-1 keyboard: control always just sets the high bit.
        /// </summary>
        public PERQKey(byte normal, byte shifted)
        {
            Normal = normal;
            Shift = shifted;
            Control = (byte)(normal | 0x80);
            CtrlShift = (byte)(shifted | 0x80);
        }

        /// <summary>
        /// Fully specify a key.
        /// </summary>
        public PERQKey(byte normal, byte shifted, byte ctrl, byte ctrlShifted)
        {
            Normal = normal;
            Shift = shifted;
            Control = ctrl;
            CtrlShift = ctrlShifted;
        }

        public byte Normal;
        public byte Shift;
        public byte Control;
        public byte CtrlShift;
    }

    /// <summary>
    /// Maps the SDL keyboard codes to the PERQ ASCII representation.
    /// </summary>
    public sealed class KeyboardMap
    {
        public KeyboardMap(ChassisType machine)
        {
            if (machine == ChassisType.PERQ1)
                SetupPERQ1Map();
            else
                SetupPERQ2Map();

            // Set the initial state of the CAPS LOCK and NUM LOCK keys,
            // based on the Console's setting?
            _lockCaps = Console.CapsLock;
            _lockNums = Console.NumberLock;
        }

        public bool CapsLock => _lockCaps;
        public bool NumLock => _lockNums;

        // Toggle the state of a given "lock" key
        public void SetLockKeyState(SDL.SDL_Keycode keycode)
        {
            switch (keycode)
            {
                case SDL.SDL_Keycode.SDLK_CAPSLOCK:
                    _lockCaps = !_lockCaps;
                    Log.Debug(Category.Keyboard, "CAPS LOCK is {0}", _lockCaps);
                    break;

                case SDL.SDL_Keycode.SDLK_NUMLOCKCLEAR:
                    _lockNums = !_lockNums;
                    Log.Debug(Category.Keyboard, "NUM LOCK is {0}", _lockNums);
                    break;
            }
        }

        public void SetKeyMapping(SDL.SDL_Keycode keycode, PERQKey key)
        {
            _map[keycode] = key;
        }

        public byte GetKeyMapping(SDL.SDL_Keycode keycode, bool shift, bool control)
        {
            // Is it a key we recognize?
            if (!_map.ContainsKey(keycode)) return 0;

            // Yep!
            var perqKey = _map[keycode];

            // Account for the state of CAPS LOCK.  On a real PERQ 1, CAPS LOCK is
            // _really_ a SHIFT LOCK -- applies to every key, not just letters!  On
            // the PERQ 2, and here, we'll do the more expected CAPS LOCK behavior...
            shift |= ((keycode >= SDL.SDL_Keycode.SDLK_a && keycode <= SDL.SDL_Keycode.SDLK_z) && _lockCaps);

            // Okay, yes, I may be a little bit deranged
            return (shift ? (control ? perqKey.CtrlShift : perqKey.Shift) :
                            (control ? perqKey.Control : perqKey.Normal));
        }

        /// <summary>
        /// Map the custom PERQ-1 keyboard.
        /// </summary>
        void SetupPERQ1Map()
        {
            // Alphabetic
            SetKeyMapping(SDL.SDL_Keycode.SDLK_a, new PERQKey(0x61, 0x41));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_b, new PERQKey(0x62, 0x42));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_c, new PERQKey(0x63, 0x43));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_d, new PERQKey(0x64, 0x44));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_e, new PERQKey(0x65, 0x45));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_f, new PERQKey(0x66, 0x46));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_g, new PERQKey(0x67, 0x47));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_h, new PERQKey(0x68, 0x48));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_i, new PERQKey(0x69, 0x49));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_j, new PERQKey(0x6a, 0x4a));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_k, new PERQKey(0x6b, 0x4b));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_l, new PERQKey(0x6c, 0x4c));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_m, new PERQKey(0x6d, 0x4d));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_n, new PERQKey(0x6e, 0x4e));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_o, new PERQKey(0x6f, 0x4f));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_p, new PERQKey(0x70, 0x50));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_q, new PERQKey(0x71, 0x51));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_r, new PERQKey(0x72, 0x52));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_s, new PERQKey(0x73, 0x53));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_t, new PERQKey(0x74, 0x54));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_u, new PERQKey(0x75, 0x55));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_v, new PERQKey(0x76, 0x56));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_w, new PERQKey(0x77, 0x57));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_x, new PERQKey(0x78, 0x58));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_y, new PERQKey(0x79, 0x59));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_z, new PERQKey(0x7a, 0x5a));

            // Numeric
            SetKeyMapping(SDL.SDL_Keycode.SDLK_1, new PERQKey(0x31, 0x21));    // numeral 1, '!'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_2, new PERQKey(0x32, 0x40));    // numeral 2, '@'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_3, new PERQKey(0x33, 0x23));    // numeral 3, '#'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_4, new PERQKey(0x34, 0x24));    // numeral 4, '$'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_5, new PERQKey(0x35, 0x25));    // numeral 5, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_6, new PERQKey(0x36, 0x5e));    // numeral 6, '^'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_7, new PERQKey(0x37, 0x26));    // numeral 7, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_8, new PERQKey(0x38, 0x2a));    // numeral 8, '*'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_9, new PERQKey(0x39, 0x28));    // numeral 9, '('
            SetKeyMapping(SDL.SDL_Keycode.SDLK_0, new PERQKey(0x30, 0x29));    // numeral 0, ')'

            // Punctuation
            SetKeyMapping(SDL.SDL_Keycode.SDLK_MINUS, new PERQKey(0x2d, 0x5f));         // '-', '_'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_EQUALS, new PERQKey(0x3d, 0x2b));        // '=', '+'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKQUOTE, new PERQKey(0x60, 0x7e));     // '`', '~'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_LEFTBRACKET, new PERQKey(0x5b, 0x7b));   // '[', '{'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RIGHTBRACKET, new PERQKey(0x5d, 0x7d));  // ']', '}'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSLASH, new PERQKey(0x5c, 0x7c));     // '\', '|'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SEMICOLON, new PERQKey(0x3b, 0x3a));     // ';', ':'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_QUOTE, new PERQKey(0x27, 0x22));         // "'", '"'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_COMMA, new PERQKey(0x2c, 0x3c));         // ',', '<'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PERIOD, new PERQKey(0x2e, 0x3e));        // '.', '>'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SLASH, new PERQKey(0x2f, 0x3f));         // '/', '?'

            // Editing keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SPACE, new PERQKey(0x20, 0x20));     // SPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSPACE, new PERQKey(0x08, 0x08)); // BACKSPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_TAB, new PERQKey(0x09, 0x09));       // TAB (no back tab on PERQ)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_ESCAPE, new PERQKey(0x1b, 0x1b));    // ESCAPE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_INSERT, new PERQKey(0x1b, 0x1b));    // INSERT (same as ESC on PERQ)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DELETE, new PERQKey(0x7f, 0x7f));    // DELETE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DOWN, new PERQKey(0x0a, 0x0a));      // LINEFEED
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RETURN, new PERQKey(0x0d, 0x0d));    // ENTER

            // Function keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F1, new PERQKey(0x07, 0x07));        // HELP (^g)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_HELP, new PERQKey(0x07, 0x07));      // HELP (^g)    (Mac)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F2, new PERQKey(0x15, 0x15));        // OOPS (^u)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_CLEAR, new PERQKey(0x15, 0x15));     // OOPS (^u)    (Mac)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F3, new PERQKey(0x0a, 0x0a));        // LINEFEED (^j)

            Log.Debug(Category.Keyboard, "PERQ-1 map initialized");
        }

        /// <summary>
        /// Map the PERQ 2 "VT100-style" keyboard.
        /// NB: This keyboard sends the data inverted!
        /// </summary>
        void SetupPERQ2Map()
        {
            //
            // Todo: these crazy mappings worked out by Tony Duell and posted to
            // alt.sys.perq many moons ago.  These need to be verified against
            // the POS keyboard test program to make sure we can generate all
            // the correct codes!!
            //

            // Alphabetic
            SetKeyMapping(SDL.SDL_Keycode.SDLK_a, new PERQKey(0x9e, 0xbe, 0x1e, 0x3e));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_b, new PERQKey(0x9d, 0xbd, 0x1d, 0x3d));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_c, new PERQKey(0x9c, 0xbc, 0x1c, 0x3c));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_d, new PERQKey(0x9b, 0xbb, 0x1b, 0x3b));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_e, new PERQKey(0x9a, 0xba, 0x1a, 0x3a));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_f, new PERQKey(0x99, 0xb9, 0x19, 0x39));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_g, new PERQKey(0x98, 0xb8, 0x18, 0x38));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_h, new PERQKey(0x97, 0xb7, 0x17, 0x37));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_i, new PERQKey(0x96, 0xb6, 0x16, 0x36));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_j, new PERQKey(0x95, 0xb5, 0x15, 0x35));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_k, new PERQKey(0x94, 0xb4, 0x14, 0x34));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_l, new PERQKey(0x93, 0xb3, 0x13, 0x33));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_m, new PERQKey(0x92, 0xb2, 0x12, 0x32));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_n, new PERQKey(0x91, 0xb1, 0x11, 0x31));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_o, new PERQKey(0x90, 0xb0, 0x10, 0x30));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_p, new PERQKey(0x8f, 0xaf, 0x0f, 0x2f));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_q, new PERQKey(0x8e, 0xae, 0x0e, 0x2e));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_r, new PERQKey(0x8d, 0xad, 0x0d, 0x2d));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_s, new PERQKey(0x8c, 0xac, 0x0c, 0x2c));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_t, new PERQKey(0x8b, 0xab, 0x0b, 0x2b));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_u, new PERQKey(0x8a, 0xaa, 0x0a, 0x2a));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_v, new PERQKey(0x89, 0xa9, 0x09, 0x29));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_w, new PERQKey(0x88, 0xa8, 0x08, 0x28));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_x, new PERQKey(0x87, 0xa7, 0x07, 0x27));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_y, new PERQKey(0x86, 0xa6, 0x06, 0x26));
            SetKeyMapping(SDL.SDL_Keycode.SDLK_z, new PERQKey(0x85, 0xa5, 0x05, 0x25));

            // Numeric
            SetKeyMapping(SDL.SDL_Keycode.SDLK_1, new PERQKey(0xce, 0xde, 0x4e, 0x5e));    // numeral 1, '!'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_2, new PERQKey(0xcd, 0xbf, 0x4d, 0x3f));    // numeral 2, '@'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_3, new PERQKey(0xcc, 0xdc, 0x4c, 0x5c));    // numeral 3, '#'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_4, new PERQKey(0xcb, 0xdb, 0x4b, 0x5b));    // numeral 4, '$'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_5, new PERQKey(0xca, 0xda, 0x4a, 0x5a));    // numeral 5, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_6, new PERQKey(0xc9, 0xa1, 0x49, 0x21));    // numeral 6, '^'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_7, new PERQKey(0xc8, 0xd9, 0x48, 0x59));    // numeral 7, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_8, new PERQKey(0xc7, 0xd5, 0x47, 0x55));    // numeral 8, '*'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_9, new PERQKey(0xc6, 0xd7, 0x46, 0x57));    // numeral 9, '('
            SetKeyMapping(SDL.SDL_Keycode.SDLK_0, new PERQKey(0xcf, 0xd6, 0x4f, 0x56));    // numeral 0, ')'

            // Punctuation
            SetKeyMapping(SDL.SDL_Keycode.SDLK_MINUS, new PERQKey(0xd2, 0xa0, 0x52, 0x20));         // '-', '_'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_EQUALS, new PERQKey(0xc2, 0xd4, 0x42, 0x54));        // '=', '+'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKQUOTE, new PERQKey(0x9f, 0x81, 0x1f, 0x01));     // '`', '~'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_LEFTBRACKET, new PERQKey(0xa4, 0x84, 0x24, 0x04));   // '[', '{'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RIGHTBRACKET, new PERQKey(0xa2, 0x82, 0x22, 0x02));  // ']', '}'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSLASH, new PERQKey(0xa3, 0x83, 0x23, 0x03));     // '\', '|'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SEMICOLON, new PERQKey(0xc4, 0xc5, 0x44, 0x45));     // ';', ':'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_QUOTE, new PERQKey(0xd8, 0xdd, 0x58, 0x5d));         // "'", '"'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_COMMA, new PERQKey(0xd3, 0xc3, 0x53, 0x43));         // ',', '<'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PERIOD, new PERQKey(0xd1, 0xc1, 0x51, 0x41));        // '.', '>'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SLASH, new PERQKey(0xd0, 0xc0, 0x50, 0x40));         // '/', '?'

            // Editing keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SPACE, new PERQKey(0xdf));                        // SPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSPACE, new PERQKey(0xf7, 0xf7, 0x77, 0x77));  // BACKSPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_TAB, new PERQKey(0xf6, 0xf6, 0x76, 0x76));        // TAB
            SetKeyMapping(SDL.SDL_Keycode.SDLK_ESCAPE, new PERQKey(0xe4, 0xe4, 0x64, 0x64));     // ESCAPE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_INSERT, new PERQKey(0xe4, 0xe4, 0x64, 0x64));     // INSERT (same as ESC on PERQ)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DELETE, new PERQKey(0x80, 0x80, 0x00, 0x00));     // DELETE (ZERO? SRSLY?)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RETURN, new PERQKey(0xf2, 0xf2, 0x72, 0x72));     // RETURN

            // Arrows
            SetKeyMapping(SDL.SDL_Keycode.SDLK_UP, new PERQKey(0x7f, 0x7f, 0xe3, 0xe3));         // UP
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DOWN, new PERQKey(0x7e, 0x7e, 0xe2, 0xe2));       // DOWN
            SetKeyMapping(SDL.SDL_Keycode.SDLK_LEFT, new PERQKey(0x7d, 0x7d, 0xe1, 0xe1));       // LEFT 
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RIGHT, new PERQKey(0x7c, 0x7c, 0xe0, 0xe0));      // RIGHT

            // PERQ/VT100 keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_HELP, new PERQKey(0xf8, 0xf8, 0x78, 0x78));       // HELP
            SetKeyMapping(SDL.SDL_Keycode.SDLK_CLEAR, new PERQKey(0xea, 0xea, 0x6a, 0x6a));      // OOPS
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SYSREQ, new PERQKey(0x7f, 0x7e, 0x7d, 0x7c));     // BREAK (Todo: Pause/Break?)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SCROLLLOCK, new PERQKey(0xf3, 0xf3, 0xf1, 0xf1)); // NOSCRL
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PAGEDOWN, new PERQKey(0xf5, 0xf5, 0x75, 0x75));   // LINEFEED
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PAGEUP, new PERQKey(0xf4, 0xf4, 0xf9, 0xf9));     // SETUP (Todo: SYSREQ better choice?)

            // Keypad
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F1, new PERQKey(0x7b));         // PF1
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F2, new PERQKey(0x7a));         // PF2
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F3, new PERQKey(0x79));         // PF3
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F4, new PERQKey(0x74));         // PF4

            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_PERIOD, new PERQKey(0x6b));  // Keypad '.'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_MINUS, new PERQKey(0x6c));   // Keypad '-'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_COMMA, new PERQKey(0x6d));   // Keypad ','
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_ENTER, new PERQKey(0x73));   // ENTER

            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_0, new PERQKey(0x69));       // NumPad 0
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_1, new PERQKey(0x68));       // NumPad 1
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_2, new PERQKey(0x67));       // NumPad 2
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_3, new PERQKey(0x66));       // NumPad 3
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_4, new PERQKey(0x65));       // NumPad 4
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_5, new PERQKey(0x63));       // NumPad 5
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_6, new PERQKey(0x62));       // NumPad 6
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_7, new PERQKey(0x61));       // NumPad 7
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_8, new PERQKey(0x60));       // NumPad 8
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_9, new PERQKey(0x5f));       // NumPad 9

            Log.Debug(Category.Keyboard, "PERQ-2 (VT100) map initialized");
        }

        // One map to rule them all
        Dictionary<SDL.SDL_Keycode, PERQKey> _map = new Dictionary<SDL.SDL_Keycode, PERQKey>();

        // Have to catch/test for CAPS LOCK status ourselves, and maintain local state.  Ugh.
        bool _lockCaps;
        bool _lockNums;
    }
}

