// tablet.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.CPU;

using System;
using System.Collections.Generic;

namespace PERQemu.IO.Z80.IOB
{

    /// <summary>
    /// Implements Kriz Tablet functionality.  This was an electromagnetic ranging
    /// tablet designed by 3RCC engineer Stan Kriz (U.S. Patent #4455451!).  Came
    /// in portrait and landscape variants; for now we only emulate the portrait.
    /// </summary>
    public sealed class Tablet : IZ80Device
    {
        public Tablet()
        {
            Reset();
        }

        public void Reset()
        {
            _lastX = -1;
            _lastY = -1;
            _lastButton = -1;
            _enabled = false;
            _messageIndex = 0;
            _pollCount = 0;

            // Set for 60 updates/sec.
            _jiffyInterval = ((PERQCpu.Frequency / 60) / PERQCpu.IOFudge);

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Tablet, "Tablet: Reset");
#endif
        }

        public ReadyFlags BusyBit
        {
            get { return 0; }           // Tablet doesn't have a Ready bit
        }

        public int BusyClocks
        {
            get { return 0; }           // Tablet can always be polled
            set { int dummy = value; }
        }

        public bool RunStateMachine(PERQtoZ80Message message, byte value)
        {
            bool retVal = false;

            // Two bytes for tablet status:
            //  byte 0 = byte count (should always be 1)
            //  byte 1 = on/off (0=off, 1=on)
            _messageIndex++;
            if (_messageIndex > 1)
            {
                _messageIndex = 0;
                _enabled = ((value & 0x2) != 0);    // NOTE: per Z80 v8.7, bit 0x02 is flag
                retVal = true;                      // Done with message
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Tablet, "Tablet: message {0} data={1:x} enabled={2}",
                                          message, value, _enabled);
#endif
            return retVal;
        }

        public void Poll(ref Queue<byte> fifo)
        {
            _pollCount++;

            if (_enabled && fifo.Count == 0)
            {
                //
                // Based on my reading of the Kriz tablet detection code in IOOthers.pas
                // the Kriz tablet sends data constantly at about 60 samples per second.
                // POS detects the presence/absence of the tablet by waiting for a tablet
                // message within a certain timeout period.  We periodically send tablet
                // information every few polls...
                //
                if (_pollCount >= _jiffyInterval)
                {
                    int jiffies = (_pollCount / _jiffyInterval);
                    int x = 0;
                    int y = 0;
                    int button = Display.Display.Instance.MouseButton;

                    GetTabletPos(out x, out y, button);

                    fifo.Enqueue(Z80System.SOM);
                    fifo.Enqueue((byte)Z80toPERQMessage.TabletData);
                    fifo.Enqueue((byte)x);          // X low
                    fifo.Enqueue((byte)(x >> 8));   // X high
                    fifo.Enqueue((byte)y);          // Y low
                    fifo.Enqueue((byte)(y >> 8));   // Y high
                    fifo.Enqueue((byte)jiffies);    // (1/60th sec clocks since last
                                                    // message, according to perqz80.doc)

                    // Per Z80 v8.7, this 5th byte is accepted but ignored; time base
                    // maintenance moved to the video interrupt instead.
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Tablet, "Tablet polled: x={0} y={1} button={2} jiffies={3}",
                                                     x, y, button, jiffies);
#endif
                    _lastX = x;
                    _lastY = y;
                    _lastButton = button;
                    _pollCount = 0;
                }
            }
        }

        /// <summary>
        /// Get the mouse X, Y positions from the display and translate them into the range
        /// of the Kriz tablet. Coordinate translation based on tweaking, not solid data...
        /// </summary>
        private void GetTabletPos(out int x, out int y, int button)
        {
            //
            // From the Z80 disassembly (v87.z80):
            // ;   Tab1<7:0> = low X
            // ;   Tab2<3:0> = high X
            // ;   Tab2<4>   = unused (0)
            // ;   Tab2<5>   = TabCoil (always 0 for now)
            // ;   Tab2<6>   = TabOffTablet (1 -> mouse off tablet)
            // ;   Tab2<7>   = complement of Switches or-ed (~ TabBuf^.TabSwitch in Pascal)
            // ;   Tab3<7:0> = low Y
            // ;   Tab4<3:0> = high bits of Y
            // ;   Tab4<4>   = unused (0)
            // ;   Tab4<7:5> = the 3 Switches
            //
            // In summary: the upper 3 bits of Y are the three buttons
            // The high bit of X indicates that any button is pressed.
            //

            // Calc Y and X positions:
            y = (Display.VideoController.PERQ_DISPLAYHEIGHT - Display.Display.Instance.MouseY + 64);
            x = (Display.Display.Instance.MouseX + 64);

            // Mix in the button data:
            y = (y & 0x1fff) | ((Display.Display.Instance.MouseButton) << 13);
            x = (x & 0x7ff) | (Display.Display.Instance.MouseButton == 0 ? 0x8000 : 0x0000);

            // And set the mouse-off-tablet bit if necessary
            if (Display.Display.Instance.MouseOffTablet)
            {
                x |= 0x4000;
            }
        }

        public void GetStatus(ref Queue<byte> fifo)
        {
            // Return current tablet status:
            fifo.Enqueue(Z80System.SOM);
            fifo.Enqueue((byte)Z80toPERQMessage.TabletStatus);
            fifo.Enqueue((byte)(_enabled ? 0x2 : 0x0));     // Per Z80 v8.7: enabled flag is bit 1,
            fifo.Enqueue(0x0);                              // Send SIO B overrun count (fake it :-)

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Tablet, "Tablet: GetStatus() returns enabled={0}", _enabled);
#endif
        }

        private int _lastX;
        private int _lastY;
        private int _lastButton;
        private bool _enabled;
        private int _messageIndex;

        private int _jiffyInterval;
        private int _pollCount;
    }
}
