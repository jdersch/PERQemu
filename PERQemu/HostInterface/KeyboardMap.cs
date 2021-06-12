// keyboardmap.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using SDL2;
using System.Collections.Generic;

namespace PERQemu.HostInterface
{
    /// <summary>
    /// Maps the SDL keyboard codes to the PERQ ASCII representation.
    /// </summary>
    public sealed class KeyboardMap
    {
        public KeyboardMap()
        {
            Reset();
        }

        public void Reset()
        {
            // Do this now, since we only have one keyboard type to worry about (for now)
            SetupPERQ1Map();

            // TODO: Get the state of the CAPS LOCK and NUM LOCK keys; set our local booleans
            _lockCaps = false;  // XXX Need cross-platform way to query these
            _lockNums = false;  // XXX in real time.  Way low priority, tho...

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "KeyboardMap: Reset.");
#endif
        }

        public bool CapsLock { get { return _lockCaps; } }

        public bool NumLock { get { return _lockNums; } }

        // Toggle the state of a given "lock" key (CAPS, NUM, SCROLL)
        public void SetLockKeyState(SDL.SDL_Keycode keycode)
        {
            switch (keycode) {
                case SDL.SDL_Keycode.SDLK_CAPSLOCK:
                    _lockCaps = ! _lockCaps;
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "CAPS LOCK is {0}", _lockCaps);
#endif
                    break;

                case SDL.SDL_Keycode.SDLK_NUMLOCKCLEAR:
                    _lockNums = ! _lockNums;
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "NUM LOCK is {0}", _lockNums);
#endif
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

        private void SetupPERQ1Map()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "KeyboardMap: SetupPERQ1Map() entered");
#endif
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
            // Josh maps some of these to mimic the PERQ keyboard; most keyboards don't have a
            // LINEFEED or HELP key, and as far as I know only the PERQ had an 'OOPS' key! :-)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F1, 0x07, 0x07);         // HELP (^g)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_HELP, 0x07, 0x07);       // HELP (^g)    (Mac)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F2, 0x15, 0x15);         // OOPS (^u)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_CLEAR, 0x15, 0x15);      // OOPS (^u)    (Mac)
            SetKeyMapping(SDL.SDL_Keycode.SDLK_F3, 0x0a, 0x0a);         // LINEFEED (^j)

            /*
            // Arrows
            // PERQ1 doesn't have these, so I map them to something useful for Pepper:
            //      unshifted: ^p/^n for up/down line, ^b/^f for left/right char
            //        shifted: ^a/^e for begin/end line, ^V/^v for up/down page
            // PERQ2 does have these, but we don't emulate that yet...
            SetKeyMapping(Keys.Up, 0xf0, 0xd6);         // UP (^p/^V)       (Mac)
            SetKeyMapping(Keys.Down, 0xee, 0xf6);       // DOWN (^n/^v)     (Mac)
            SetKeyMapping(Keys.Left, 0xe2, 0xe1);       // LEFT (^b/^a)     (Mac)
            SetKeyMapping(Keys.Right, 0xe6, 0xe5);      // RIGHT (^f/^e)    (Mac)

            // Keypad
            // PERQ1 doesn't have one, so nothing special here.
            SetKeyMapping(Keys.Add, 0x2b, 0x2b);        // Keypad '+'
            SetKeyMapping(Keys.Subtract, 0x2d, 0x2d);   // Keypad '-'
            SetKeyMapping(Keys.Multiply, 0x2a, 0x2a);   // Keypad '*'
            SetKeyMapping(Keys.Divide, 0x2f, 0x2f);     // Keypad '/'
            SetKeyMapping(Keys.Decimal, 0x2e, 0x2e);    // Keypad '.'

            // Need a USB keyboard or a WinPC w/full kbd to test out NumLock/shift on these...
            SetKeyMapping(Keys.NumPad0, 0x30, 0x30);    // NumPad 0
            SetKeyMapping(Keys.NumPad1, 0x31, 0x31);    // NumPad 1
            SetKeyMapping(Keys.NumPad2, 0x32, 0x32);    // NumPad 2
            SetKeyMapping(Keys.NumPad3, 0x33, 0x33);    // NumPad 3
            SetKeyMapping(Keys.NumPad4, 0x34, 0x34);    // NumPad 4
            SetKeyMapping(Keys.NumPad5, 0x35, 0x35);    // NumPad 5
            SetKeyMapping(Keys.NumPad6, 0x36, 0x36);    // NumPad 6
            SetKeyMapping(Keys.NumPad7, 0x37, 0x37);    // NumPad 7
            SetKeyMapping(Keys.NumPad8, 0x38, 0x38);    // NumPad 8
            SetKeyMapping(Keys.NumPad9, 0x39, 0x39);    // NumPad 9
            */
        }

        // Keys.KeyValue is from 0..255 for "real" keys; the modifiers use upper bits.  We'll
        // only care about mapping values in the ASCII range; the others don't get passed to
        // us in the KeyEvent anyway (I think :-)
        private Dictionary<SDL.SDL_Keycode, byte> _normal = new Dictionary<SDL.SDL_Keycode, byte>();
        private Dictionary<SDL.SDL_Keycode, byte> _shifted = new Dictionary<SDL.SDL_Keycode, byte>();

        // Have to catch/test for CAPS LOCK status ourselves, and maintain local state.  Ugh.
        private bool _lockCaps;
        private bool _lockNums;
    }
}

