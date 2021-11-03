// keyboard.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

namespace PERQemu.IO.Z80.IOB
{
    //
    // Notes from playing with the PERQ's actual keyboard --
    // reading raw scancodes from the device reveals the following:
    //
    // - Normal printable keys support CTRL, SHIFT, and CTRL+SHIFT
    //   modifiers.
    //
    // - Unprintable keys (INS, DEL, HELP, BACKSPACE, OOPS, LF, and RETURN)
    //   support CTRL modifiers (but not SHIFT or CTRL+SHIFT) (shift has no effect,
    //   CTRL+SHIFT = CTRL)
    //
    // For future reference:
    // The keys on the keyboard (for the PERQ 1/1A -- the PERQ 2 had an extended keyboard with function
    // and arrow keys, and a numeric keypad) are (starting from the upper-left and working across and down):
    //
    // Key      ASCII   Normal      Shifted     Ctrl    Ctrl+Shift
    //  INS     ^[      033         033         233     233
    //  TAB     ^i      011         011         211     211
    //  1       1       61          41 (!)      261     241
    //  2       2       62          100 (@)     262     300
    //  3       3       63          43 (#)      263     243
    //  4       4       64          44 ($)      264     244
    //  5       5       65          45 (%)      265     245
    //  6       6       66          136 (^)     266     336
    //  7       7       67          46 (&)      267     246
    //  8       8       70          52 (*)      270     252
    //  9       9       71          50 (()      271     250
    //  0       0       60          51 ())      260     251
    //  -/_     -       55          137 (_)     255     337
    //  +/=     =       75          53 (+)      275     253
    //  BkSp    ^h      10          10          210     210
    //  OOPS    ^u      25          25          225     225
    //  DEL             177         177         377     377
    //  Q       q       161         121         361     321
    //  W       w       167         127         367     327
    //  E       e       145         105         345     305
    //  R       r       162         122         362     322
    //  T       t       164         124         364     324
    //  Y       y       171         131         371     331
    //  U       u       165         125         365     325
    //  I       i       151         111         351     311
    //  O       o       157         117         357     317
    //  P       p       160         120         360     320
    // {/}      {       173         175 (})     373     375
    // [/]      [       133         135 (])     333     335
    // \/|      \       134         174 (|)     334     374
    // HELP     (bel)   007         007         207     207
    // A        a       141         101         341     301
    // S        s       163         123         363     323
    // D        d       144         104         344     304
    // F        f       146         106         346     306
    // G        g       147         107         347     307
    // H        h       150         110         350     310
    // J        j       152         112         352     312
    // K        k       153         113         353     313
    // L        l       154         114         354     314
    // :/;      ;       073         072 (:)     273     272
    // '/"      '       047         042 (")     247     242
    // RETURN   ^m      015         015         215     215
    // Z        z       172         132         372     332
    // X        x       170         130         370     330
    // C        c       143         103         343     303
    // V        v       166         126         366     326
    // B        b       142         102         342     302
    // N        n       156         116         356     316
    // M        m       155         115         355     315
    // </,      ,       054         074         254     274
    // >/.      .       056         076         256     276
    // ?//      /       057         077         257     277
    // LF       ^j      012         012         212     212
    //
    // The shifted value of the key obviously isn't based on the unshifted value, it's
    // simply the ASCII value of the resultant character (i.e. shifted 2 isn't simply "62"
    // with an extra bit set -- it's the ASCII value of '@'.  This makes sense, since it's
    // really an ASCII keyboard.
    //
    // For CTRL, bit 7 is or'd in.  That's it.
    //

    /// <summary>
    /// Represents the possible key modifiers for the PERQ keyboard
    /// </summary>
    [Flags]
    public enum PERQKeyboardModifiers
    {
        None = 0,
        Shift = 1,
        Ctrl = 2
    }
    
    public sealed class Keyboard : IZ80Device, IKeyboard
    {
        public Keyboard(PERQSystem system)
        {
            _system = system;

            Reset();
        }

        public void Reset()
        {
            _messageIndex = 0;
            _messageData = new byte[16];
            _kbdInput = new Queue<byte>();
        }

        public ReadyFlags BusyBit
        {
            get { return 0; }           // Keyboard doesn't have a Ready bit
        }

        public int BusyClocks
        {
            get { return 0; }           // Keyboard can always be polled
            set { int dummy = value; }
        }

        public bool RunStateMachine(PERQtoZ80Message message, byte value)
        {
            bool retVal = false;

            switch (message)
            {
                case PERQtoZ80Message.SetKeyboardStatus:
                    // Two bytes for keyboard status:
                    //  byte 0 = byte count?
                    //  byte 1 = on/off (0=off, 1=on)
                    _messageData[_messageIndex] = value;
                    _messageIndex++;
                    if (_messageIndex > 1)
                    {
                        _messageIndex = 0;
                        SetKeyboardStatus();
                        retVal = true;          // Done with message
                    }
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled keyboard message {0}", message);
#endif
                    break;
            }

            return retVal;
        }

        private void SetKeyboardStatus()
        {
            _enabled = (_messageData[1] != 0);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State, "Keyboard enabled state set to {0}", _enabled);
#endif
        }

        public void QueueInput(byte b)
        {
            // The PERQ's keyboard queue is only 16 chars, if it gets filled up we just
            // drop any incoming keys.
            if (_kbdInput.Count < 16)
            {
                _kbdInput.Enqueue(b);
            }
        }

        public void Poll(ref Queue<byte> fifo)
        {
            while (_enabled && _kbdInput.Count > 0)
            {
                // We're sending 1 character at a time.
                // Message format is:
                //  SOM
                //  1 (keyboard char message type)
                //  Keystroke ASCII code
                fifo.Enqueue(Z80System.SOM);                            // SOM
                fifo.Enqueue((byte)Z80toPERQMessage.KeyboardData);      // Keyboard char message type
                fifo.Enqueue(_kbdInput.Dequeue());                      // Data
            }

            //
            // Special case: Hack for bootchar to make it easier to override at startup.
            //
            if (PERQemu.Controller.BootChar != 0 && _enabled && _system.CPU.DDS < 154 && fifo.Count < 1)
            {
                _bootCharThrottle++;

                if (_bootCharThrottle > 1000)
                {
                    fifo.Enqueue(Z80System.SOM);                        // SOM
                    fifo.Enqueue((byte)Z80toPERQMessage.KeyboardData);  // Keyboard char message type
                    fifo.Enqueue(PERQemu.Controller.BootChar);          // Data

                    _bootCharThrottle = 0;
                }
            }
        }

        public bool Enabled
        {
            get { return _enabled; }
        }

        private bool _enabled = false;
        private Queue<byte> _kbdInput;
        private byte[] _messageData;
        private int _messageIndex;
        private int _bootCharThrottle;

        private PERQSystem _system;
    }
}
