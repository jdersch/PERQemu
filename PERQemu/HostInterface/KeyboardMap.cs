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

using System;
using System.Drawing;
using System.Windows.Forms;

namespace PERQemu.HostInterface
{
    /// <summary>
    /// Maps the .NET keyboard codes (enum Keys) to the PERQ ASCII representation.
    /// Should allow for a fully-managed, cross-platform implementation (tested on
    /// Windows/.NET, and Linux/Mono and Mac OS X/Mono) without the need for PInvokes.
    /// Since there's only one static mapping to worry about (PERQ-1 keyboard; no
    /// support for the VT100-style PERQ2) this is a singleton.
    /// </summary>
    public sealed class KeyboardMap
    {
        private KeyboardMap()
        {
            Reset();
        }

        public static KeyboardMap Instance
        {
            get { return _instance; }
        }

        public void Reset()
        {
            for (byte i = 0; i < 255; i++)
            {
                _normal[i] = 0;
                _shifted[i] = 0;
            }

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
        public void setLockKeyState(Keys k)
        {
            // TODO: Use the MS Control.() to check the current state of the
            // caps/num lock keys, update our internal settings
            switch (k) {
                case Keys.CapsLock:
                    _lockCaps = ! _lockCaps;
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "CAPS LOCK is {0}", _lockCaps);
#endif
                    break;

                case Keys.NumLock:
                    _lockNums = ! _lockNums;
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "NUM LOCK is {0}", _lockNums);
#endif
                    break;

                case Keys.Scroll:
                    // In Accent, though, this could be mapped to CTRL-LINEFEED to turn on "more mode" :-)
                    Console.WriteLine("Sorry, for now the PERQ doesn't care about SCROLL LOCK.");
                    break;
            }
        }

        public void setKeyMapping(Keys k, byte NormalVal, byte ShiftedVal)
        {
            byte index = Convert.ToByte((int)k & 0xff);

            _normal[index] = NormalVal;
            _shifted[index] = ShiftedVal;
        }

        public byte getKeyMapping(KeyEventArgs e)
        {
            byte perqCode = 0;
            byte rawKey = (byte)(e.KeyValue & 0xff);    // Strip off modifier bits

            // Account for the SHIFT modifier first, factoring in the state of CAPS LOCK too.
            // Note: on a real PERQ 1, CAPS LOCK is _really_ a SHIFT LOCK -- applies to every
            // key, not just letters!  On the PERQ 2, and here, we'll do the more expected
            // CAPS LOCK behavior.
            if ((e.Shift) || ((e.KeyCode >= Keys.A && e.KeyCode <= Keys.Z) && _lockCaps ))
            {
                perqCode = _shifted[rawKey];
            }
            else
            {
                perqCode = _normal[rawKey];
            }

            // Do this here?
            if (perqCode > 0)
            {
                if (e.Control)
                {
                    perqCode = (byte)(perqCode | 0x80);   // Ctrl always sets high bit
                }
            }
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Keyboard, "getKeyMapping: code={0}, val={1}, data={2}, mods={3}, raw={4}, cooked={5}",
                                            e.KeyCode, e.KeyValue, e.KeyData, e.Modifiers, rawKey, perqCode);
#endif
            return perqCode;
        }

        private void SetupPERQ1Map()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Keyboard, "KeyboardMap: SetupPERQ1Map() entered");
#endif
            // Alphabetic
            setKeyMapping(Keys.A, 0x61, 0x41);
            setKeyMapping(Keys.B, 0x62, 0x42);
            setKeyMapping(Keys.C, 0x63, 0x43);
            setKeyMapping(Keys.D, 0x64, 0x44);
            setKeyMapping(Keys.E, 0x65, 0x45);
            setKeyMapping(Keys.F, 0x66, 0x46);
            setKeyMapping(Keys.G, 0x67, 0x47);
            setKeyMapping(Keys.H, 0x68, 0x48);
            setKeyMapping(Keys.I, 0x69, 0x49);
            setKeyMapping(Keys.J, 0x6a, 0x4a);
            setKeyMapping(Keys.K, 0x6b, 0x4b);
            setKeyMapping(Keys.L, 0x6c, 0x4c);
            setKeyMapping(Keys.M, 0x6d, 0x4d);
            setKeyMapping(Keys.N, 0x6e, 0x4e);
            setKeyMapping(Keys.O, 0x6f, 0x4f);
            setKeyMapping(Keys.P, 0x70, 0x50);
            setKeyMapping(Keys.Q, 0x71, 0x51);
            setKeyMapping(Keys.R, 0x72, 0x52);
            setKeyMapping(Keys.S, 0x73, 0x53);
            setKeyMapping(Keys.T, 0x74, 0x54);
            setKeyMapping(Keys.U, 0x75, 0x55);
            setKeyMapping(Keys.V, 0x76, 0x56);
            setKeyMapping(Keys.W, 0x77, 0x57);
            setKeyMapping(Keys.X, 0x78, 0x58);
            setKeyMapping(Keys.Y, 0x79, 0x59);
            setKeyMapping(Keys.Z, 0x7a, 0x5a);

            // Numeric
            setKeyMapping(Keys.D1, 0x31, 0x21);         // numeral 1, '!'
            setKeyMapping(Keys.D2, 0x32, 0x40);         // numeral 2, '@'
            setKeyMapping(Keys.D3, 0x33, 0x23);         // numeral 3, '#'
            setKeyMapping(Keys.D4, 0x34, 0x24);         // numeral 4, '$'
            setKeyMapping(Keys.D5, 0x35, 0x25);         // numeral 5, '%'
            setKeyMapping(Keys.D6, 0x36, 0x5e);         // numeral 6, '^'
            setKeyMapping(Keys.D7, 0x37, 0x26);         // numeral 7, '%'
            setKeyMapping(Keys.D8, 0x38, 0x2a);         // numeral 8, '*'
            setKeyMapping(Keys.D9, 0x39, 0x28);         // numeral 9, '('
            setKeyMapping(Keys.D0, 0x30, 0x29);         // numeral 0, ')'

            // Punctuation
            setKeyMapping(Keys.OemMinus, 0x2d, 0x5f);           // '-', '_'
            setKeyMapping(Keys.Oemplus, 0x3d, 0x2b);            // '=', '+'
            setKeyMapping(Keys.Oemtilde, 0x60, 0x7e);           // '`', '~'
            setKeyMapping(Keys.OemOpenBrackets, 0x5b, 0x7b);    // '[', '{'
            setKeyMapping(Keys.OemCloseBrackets, 0x5d, 0x7d);   // ']', '}'
            setKeyMapping(Keys.OemPipe, 0x5c, 0x7c);            // '\', '|'
            setKeyMapping(Keys.OemBackslash, 0x5c, 0x7c);       // '\', '|'
            setKeyMapping(Keys.OemSemicolon, 0x3b, 0x3a);       // ';', ':'
            setKeyMapping(Keys.OemQuotes, 0x27, 0x22);          // "'", '"'
            setKeyMapping(Keys.Oemcomma, 0x2c, 0x3c);           // ',', '<'
            setKeyMapping(Keys.OemPeriod, 0x2e, 0x3e);          // '.', '>'
            setKeyMapping(Keys.OemQuestion, 0x2f, 0x3f);        // '/', '?'

            // Editing keys
            setKeyMapping(Keys.Space, 0x20, 0x20);      // SPACE
            setKeyMapping(Keys.Back, 0x08, 0x08);       // BACKSPACE
            setKeyMapping(Keys.Tab, 0x09, 0x09);        // TAB (no back tab on PERQ)
            setKeyMapping(Keys.Escape, 0x1b, 0x1b);     // ESCAPE
            setKeyMapping(Keys.Insert, 0x1b, 0x1b);     // INSERT (same as ESC on PERQ)
            setKeyMapping(Keys.Delete, 0x7f, 0x7f);     // DELETE
            setKeyMapping(Keys.LineFeed, 0x0a, 0x0a);   // LINEFEED
            setKeyMapping(Keys.Enter, 0x0d, 0x0d);      // ENTER
            setKeyMapping(Keys.Return, 0x0d, 0x0d);     // RETURN

            // Function keys
            // Josh maps some of these to mimic the PERQ keyboard; most keyboards don't have a
            // LINEFEED or HELP key, and as far as I know only the PERQ had an 'OOPS' key! :-)
            setKeyMapping(Keys.F1, 0x07, 0x07);         // HELP (^g)
            setKeyMapping(Keys.Help, 0x07, 0x07);       // HELP (^g)    (Mac)
            setKeyMapping(Keys.F2, 0x15, 0x15);         // OOPS (^u)
            setKeyMapping(Keys.OemClear, 0x15, 0x15);   // OOPS (^u)    (Mac)
            setKeyMapping(Keys.F3, 0x0a, 0x0a);         // LINEFEED (^j)

            // Arrows
            // PERQ1 doesn't have these, so I map them to something useful for Pepper:
            //      unshifted: ^p/^n for up/down line, ^b/^f for left/right char
            //        shifted: ^a/^e for begin/end line, ^V/^v for up/down page
            // PERQ2 does have these, but we don't emulate that yet...
            setKeyMapping(Keys.Up, 0xf0, 0xd6);         // UP (^p/^V)       (Mac)
            setKeyMapping(Keys.Down, 0xee, 0xf6);       // DOWN (^n/^v)     (Mac)
            setKeyMapping(Keys.Left, 0xe2, 0xe1);       // LEFT (^b/^a)     (Mac)
            setKeyMapping(Keys.Right, 0xe6, 0xe5);      // RIGHT (^f/^e)    (Mac)

            // Keypad
            // PERQ1 doesn't have one, so nothing special here.
            setKeyMapping(Keys.Add, 0x2b, 0x2b);        // Keypad '+'
            setKeyMapping(Keys.Subtract, 0x2d, 0x2d);   // Keypad '-'
            setKeyMapping(Keys.Multiply, 0x2a, 0x2a);   // Keypad '*'
            setKeyMapping(Keys.Divide, 0x2f, 0x2f);     // Keypad '/'
            setKeyMapping(Keys.Decimal, 0x2e, 0x2e);    // Keypad '.'

            // Need a USB keyboard or a WinPC w/full kbd to test out NumLock/shift on these...
            setKeyMapping(Keys.NumPad0, 0x30, 0x30);    // NumPad 0
            setKeyMapping(Keys.NumPad1, 0x31, 0x31);    // NumPad 1
            setKeyMapping(Keys.NumPad2, 0x32, 0x32);    // NumPad 2
            setKeyMapping(Keys.NumPad3, 0x33, 0x33);    // NumPad 3
            setKeyMapping(Keys.NumPad4, 0x34, 0x34);    // NumPad 4
            setKeyMapping(Keys.NumPad5, 0x35, 0x35);    // NumPad 5
            setKeyMapping(Keys.NumPad6, 0x36, 0x36);    // NumPad 6
            setKeyMapping(Keys.NumPad7, 0x37, 0x37);    // NumPad 7
            setKeyMapping(Keys.NumPad8, 0x38, 0x38);    // NumPad 8
            setKeyMapping(Keys.NumPad9, 0x39, 0x39);    // NumPad 9

            // Reference for keyboard stuff:
            //  See: msdn.microsoft.com/en-us/library/system.windows.forms.keys(v=vs.80).aspx
        }

        // Keys.KeyValue is from 0..255 for "real" keys; the modifiers use upper bits.  We'll
        // only care about mapping values in the ASCII range; the others don't get passed to
        // us in the KeyEvent anyway (I think :-)
        private byte[] _normal = new byte[256];
        private byte[] _shifted = new byte[256];

        // Have to catch/test for CAPS LOCK status ourselves, and maintain local state.  Ugh.
        private bool _lockCaps;
        private bool _lockNums;

        private static KeyboardMap _instance = new KeyboardMap();
    }
}

