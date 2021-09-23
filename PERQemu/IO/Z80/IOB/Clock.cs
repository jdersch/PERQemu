// clock.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Processor;

using System;
using System.Collections.Generic;

namespace PERQemu.IO.Z80.IOB
{
    /// <summary>
    /// This device does not appear to be used by POS D, F or Accent S4.  There
    /// are two possibilities: the old "Stanley tablet" -- trackpad device!? --
    /// was never shipped as a product, though references to it are sprinkled here
    /// and there in the code.  Second, a "microcycle clock" (micro_second_ clock?)
    /// was part of the 3Mbit Ethernet board, but since we don't (yet?) emulate that
    /// the probe of the E3 control register fails and thus the clock device is
    /// never enabled.  There is relatively little overhead incurred by leaving
    /// this in the Z80 device list (an empty Poll() each Z80 cycle) but we could
    /// probably safely leave clockDev out entirely for now.  It may eventually be
    /// part of 3Mbit Ethernet support?
    /// </summary>
    public sealed class Clock : IZ80Device
    {
        public Clock(PERQSystem system)
        {
            _system = system;
            Reset();
        }

        public void Reset()
        {
            _messageIndex = 0;
            _messageData = new byte[16];
            _lastTick = 0;
        }

        public ReadyFlags BusyBit
        {
            get { return 0; }           // Clock doesn't maintain Ready status
        }

        public int BusyClocks
        {
            get { return 0; }           // Clock device is never busy
            set { int dummy = value; }
        }

        public bool RunStateMachine(PERQtoZ80Message message,  byte value)
        {
            bool retVal = false;

            switch (message)
            {
                case PERQtoZ80Message.SetClockStatus:
                    _messageData[_messageIndex] = value;

                    // Two bytes for clock status:
                    //  byte 0 = byte count?
                    //  byte 1 = on/off (0=off, 1=on)
                    _messageIndex++;
                    if (_messageIndex > 1)
                    {
                        _messageIndex = 0;
                        _enabled = (_messageData[1] != 0);
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.Z80State, "Clock enabled state set to {0}", _enabled);
#endif
                        retVal = true;      // Done with message
                    }
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled clock message {0}", message);
#endif
                    break;
            }

            return retVal;
        }


        public void Poll(ref Queue<byte> fifo)
        {
            if (_enabled)
            {
                int tickInterval = ((Z80System.Frequency / 60) / CPU.IOFudge);
                int elapsed = (((Z80System)(_system.IOB.Z80System)).Clocks() - _lastTick) / tickInterval;

                fifo.Enqueue(Z80System.SOM);                    // SOM
                fifo.Enqueue((byte)Z80toPERQMessage.ClockData); // Clock data message type
                fifo.Enqueue((byte)elapsed);                    // Data (# of 1/60th sec ticks since last status)
#if TRACING_ENABLED
                Console.WriteLine("Clock: jiffies since last Poll: {0} (interval {1})", elapsed, tickInterval); // fixme
#endif
                _lastTick = ((Z80System)(_system.IOB.Z80System)).Clocks();
            }
        }

        public bool Enabled
        {
            get { return _enabled; }        // Should be a GetStatus()?  Unused
        }

        private byte[] _messageData;
        private int _messageIndex;
        private bool _enabled = false;
        private int _lastTick;

        private PERQSystem _system;
    }
}
