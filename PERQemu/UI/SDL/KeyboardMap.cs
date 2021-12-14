//
// KeyboardMap.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
                    _lockCaps = ! _lockCaps;
                    Trace.Log(LogType.Keyboard, "CAPS LOCK is {0}", _lockCaps);
                    break;

                case SDL.SDL_Keycode.SDLK_NUMLOCKCLEAR:
                    _lockNums = ! _lockNums;
                    Trace.Log(LogType.Keyboard, "NUM LOCK is {0}", _lockNums);
                    break;
            }
        }

        public void SetKeyMapping(SDL.SDL_Keycode keycode, byte normalVal, byte shiftedVal)
        {
            _normal[keycode] = normalVal;
            _shifted[keycode] = shiftedVal;
        }

        public byte GetKeyMapping(SDL.SDL_Keycode keycode, bool shift, bool control)
        {
            byte perqCode = 0;

            // Account for the SHIFT modifier first, factoring in the state of CAPS LOCK too.
            // Note: on a real PERQ 1, CAPS LOCK is _really_ a SHIFT LOCK -- applies to every
            // key, not just letters!  On the PERQ 2, and here, we'll do the more expected
            // CAPS LOCK behavior.
            if (shift || ((keycode >= SDL.SDL_Keycode.SDLK_a && keycode <= SDL.SDL_Keycode.SDLK_z) && _lockCaps))
            {
                perqCode = _shifted.ContainsKey(keycode) ? _shifted[keycode] : (byte)0;
            }
            else
            {
                perqCode = _normal.ContainsKey(keycode) ? _normal[keycode] : (byte)0;
            }

            // Do this here?
            if (perqCode > 0)
            {
                if (control)
                {
                    perqCode = (byte)(perqCode | 0x80);   // Ctrl always sets high bit
                }
            }
            return perqCode;
        }

        /// <summary>
        /// Map the custom PERQ-1 keyboard.
        /// </summary>
        private void SetupPERQ1Map()
        {
            // Alphabetic
            SetKeyMapping(SDL.SDL_Keycode.SDLK_a, 0x61, 0x41);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_b, 0x62, 0x42);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_c, 0x63, 0x43);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_d, 0x64, 0x44);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_e, 0x65, 0x45);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_f, 0x66, 0x46);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_g, 0x67, 0x47);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_h, 0x68, 0x48);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_i, 0x69, 0x49);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_j, 0x6a, 0x4a);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_k, 0x6b, 0x4b);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_l, 0x6c, 0x4c);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_m, 0x6d, 0x4d);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_n, 0x6e, 0x4e);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_o, 0x6f, 0x4f);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_p, 0x70, 0x50);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_q, 0x71, 0x51);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_r, 0x72, 0x52);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_s, 0x73, 0x53);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_t, 0x74, 0x54);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_u, 0x75, 0x55);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_v, 0x76, 0x56);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_w, 0x77, 0x57);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_x, 0x78, 0x58);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_y, 0x79, 0x59);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_z, 0x7a, 0x5a);

            // Numeric
            SetKeyMapping(SDL.SDL_Keycode.SDLK_1, 0x31, 0x21);         // numeral 1, '!'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_2, 0x32, 0x40);         // numeral 2, '@'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_3, 0x33, 0x23);         // numeral 3, '#'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_4, 0x34, 0x24);         // numeral 4, '$'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_5, 0x35, 0x25);         // numeral 5, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_6, 0x36, 0x5e);         // numeral 6, '^'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_7, 0x37, 0x26);         // numeral 7, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_8, 0x38, 0x2a);         // numeral 8, '*'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_9, 0x39, 0x28);         // numeral 9, '('
            SetKeyMapping(SDL.SDL_Keycode.SDLK_0, 0x30, 0x29);         // numeral 0, ')'

            // Punctuation
            SetKeyMapping(SDL.SDL_Keycode.SDLK_MINUS, 0x2d, 0x5f);           // '-', '_'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_EQUALS, 0x3d, 0x2b);          // '=', '+'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKQUOTE, 0x60, 0x7e);       // '`', '~'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_LEFTBRACKET, 0x5b, 0x7b);     // '[', '{'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RIGHTBRACKET, 0x5d, 0x7d);    // ']', '}'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSLASH, 0x5c, 0x7c);       // '\', '|'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SEMICOLON, 0x3b, 0x3a);       // ';', ':'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_QUOTE, 0x27, 0x22);           // "'", '"'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_COMMA, 0x2c, 0x3c);           // ',', '<'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PERIOD, 0x2e, 0x3e);          // '.', '>'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SLASH, 0x2f, 0x3f);           // '/', '?'

            // Editing keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SPACE, 0x20, 0x20);      // SPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSPACE, 0x08, 0x08);  // BACKSPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_TAB, 0x09, 0x09);        // TAB (no back tab on PERQ)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_ESCAPE, 0x1b, 0x1b);     // ESCAPE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_INSERT, 0x1b, 0x1b);     // INSERT (same as ESC on PERQ)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DELETE, 0x7f, 0x7f);     // DELETE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DOWN, 0x0a, 0x0a);       // LINEFEED
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RETURN, 0x0d, 0x0d);     // ENTER

            // Function keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F1, 0x07, 0x07);         // HELP (^g)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_HELP, 0x07, 0x07);       // HELP (^g)    (Mac)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F2, 0x15, 0x15);         // OOPS (^u)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_CLEAR, 0x15, 0x15);      // OOPS (^u)    (Mac)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F3, 0x0a, 0x0a);         // LINEFEED (^j)

            Trace.Log(LogType.Keyboard, "PERQ-1 keyboard map initialized.");
        }

        /// <summary>
        /// Map the PERQ 2 "VT100-style" keyboard.
        /// </summary>
        private void SetupPERQ2Map()
        {
            //
            // TODO: um, according to Tony's mapping, the Ctrl and Ctrl-Shift
            // mappings on this keyboard might not be so straightforward... 
            // will have to boot up the POS keyboard test and see if we can
            // actually generate all the correct codes!!
            //

            // Alphabetic
            SetKeyMapping(SDL.SDL_Keycode.SDLK_a, 0x9e, 0xbe);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_b, 0x9d, 0xbd);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_c, 0x9c, 0xbc);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_d, 0x9b, 0xbb);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_e, 0x9a, 0xba);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_f, 0x99, 0xb9);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_g, 0x98, 0xb8);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_h, 0x97, 0xb7);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_i, 0x96, 0xb6);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_j, 0x95, 0xb5);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_k, 0x94, 0xb4);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_l, 0x93, 0xb3);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_m, 0x92, 0xb2);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_n, 0x91, 0xb1);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_o, 0x90, 0xb0);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_p, 0x8f, 0xaf);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_q, 0x8e, 0xae);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_r, 0x8d, 0xad);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_s, 0x8c, 0xac);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_t, 0x8b, 0xab);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_u, 0x8a, 0xaa);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_v, 0x89, 0xa9);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_w, 0x88, 0xa8);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_x, 0x87, 0xa7);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_y, 0x86, 0xa6);
            SetKeyMapping(SDL.SDL_Keycode.SDLK_z, 0x85, 0xa5);

            // Numeric
            SetKeyMapping(SDL.SDL_Keycode.SDLK_1, 0xce, 0xde);         // numeral 1, '!'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_2, 0xcd, 0xbf);         // numeral 2, '@'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_3, 0xcc, 0xdc);         // numeral 3, '#'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_4, 0xcb, 0xdb);         // numeral 4, '$'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_5, 0xca, 0xda);         // numeral 5, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_6, 0xc9, 0xa1);         // numeral 6, '^'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_7, 0xc8, 0xd9);         // numeral 7, '%'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_8, 0xc7, 0xd5);         // numeral 8, '*'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_9, 0xc6, 0xd7);         // numeral 9, '('
            SetKeyMapping(SDL.SDL_Keycode.SDLK_0, 0xc5, 0xd6);         // numeral 0, ')'

            // Punctuation
            SetKeyMapping(SDL.SDL_Keycode.SDLK_MINUS, 0xd2, 0xa0);           // '-', '_'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_EQUALS, 0xc2, 0xd4);          // '=', '+'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKQUOTE, 0x9f, 0x81);       // '`', '~'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_LEFTBRACKET, 0xa4, 0x84);     // '[', '{'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RIGHTBRACKET, 0xa2, 0x82);    // ']', '}'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSLASH, 0xa3, 0x83);       // '\', '|'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SEMICOLON, 0xc4, 0xc5);       // ';', ':'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_QUOTE, 0xd8, 0xdd);           // "'", '"'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_COMMA, 0xd3, 0xc3);           // ',', '<'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PERIOD, 0xd1, 0xc1);          // '.', '>'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SLASH, 0xd0, 0xc0);           // '/', '?'

            // Editing keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SPACE, 0xdf, 0xdf);      // SPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_BACKSPACE, 0xf7, 0xf7);  // BACKSPACE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_TAB, 0xf6, 0xf6);        // TAB
            SetKeyMapping(SDL.SDL_Keycode.SDLK_ESCAPE, 0xe4, 0xe4);     // ESCAPE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_INSERT, 0xe4, 0xe4);     // INSERT (same as ESC on PERQ)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DELETE, 0x80, 0x80);     // DELETE
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RETURN, 0xf2, 0xf2);     // RETURN

            // Arrows
            SetKeyMapping(SDL.SDL_Keycode.SDLK_UP, 0x7f, 0x7f);         // UP
            SetKeyMapping(SDL.SDL_Keycode.SDLK_DOWN, 0x7e, 0x7e);       // DOWN
            SetKeyMapping(SDL.SDL_Keycode.SDLK_LEFT, 0x7d, 0x7d);       // LEFT 
            SetKeyMapping(SDL.SDL_Keycode.SDLK_RIGHT, 0x7c, 0x7c);      // RIGHT

            // PERQ/VT100 keys
            SetKeyMapping(SDL.SDL_Keycode.SDLK_HELP, 0xf8, 0xf8);       // HELP
            SetKeyMapping(SDL.SDL_Keycode.SDLK_CLEAR, 0xea, 0xea);      // OOPS
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SYSREQ, 0x7f, 0x7e);     // BREAK
            SetKeyMapping(SDL.SDL_Keycode.SDLK_SCROLLLOCK, 0xf3, 0xf3); // NOSCRL
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PAGEDOWN, 0xf5, 0xf5);   // LINEFEED
            SetKeyMapping(SDL.SDL_Keycode.SDLK_PAGEUP, 0xf4, 0xf4);     // SETUP

            // Keypad
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F1, 0x7b, 0x7b);         // PF1
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F2, 0x7a, 0x7a);         // PF2
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F3, 0x79, 0x79);         // PF3
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F4, 0x74, 0x74);         // PF4

            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_PERIOD, 0x6b, 0x6b);  // Keypad '.'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_MINUS, 0x6c, 0x6c);   // Keypad '-'
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_COMMA, 0x6d, 0x6d);   // Keypad ','
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_ENTER, 0x73, 0x73);   // ENTER

            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_0, 0x69, 0x69);       // NumPad 0
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_1, 0x68, 0x68);       // NumPad 1
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_2, 0x67, 0x67);       // NumPad 2
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_3, 0x66, 0x66);       // NumPad 3
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_4, 0x65, 0x65);       // NumPad 4
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_5, 0x63, 0x63);       // NumPad 5
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_6, 0x62, 0x62);       // NumPad 6
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_7, 0x61, 0x61);       // NumPad 7
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_8, 0x60, 0x60);       // NumPad 8
            SetKeyMapping(SDL.SDL_Keycode.SDLK_KP_9, 0x5f, 0x5f);       // NumPad 9

            Trace.Log(LogType.Keyboard, "PERQ-2 (VT100) keyboard map initialized.");
        }

        // Maps for normal and shifted keys
        private Dictionary<SDL.SDL_Keycode, byte> _normal = new Dictionary<SDL.SDL_Keycode, byte>();
        private Dictionary<SDL.SDL_Keycode, byte> _shifted = new Dictionary<SDL.SDL_Keycode, byte>();

        // Have to catch/test for CAPS LOCK status ourselves, and maintain local state.  Ugh.
        private bool _lockCaps;
        private bool _lockNums;
    }
}

