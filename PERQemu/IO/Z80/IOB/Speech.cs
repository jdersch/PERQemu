// speech.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
    /// <summary>
    /// Represents the PERQ's "Speech" device (digital sound output)
    /// Currently drives the PC Squeaker, need to work in some DSound output or the likes.
    /// </summary>
    public sealed class Speech : IZ80Device
    {
        public Speech()
        {
            Reset();
        }

        public void Reset()
        {
            _messageIndex = 0;
            _messageData = new byte[64];
        }

        public bool RunStateMachine(PERQtoZ80Message message, byte value)
        {
            bool retVal = false;

            _messageData[_messageIndex] = value;
            _messageIndex++;

            switch (message)
            {
                case PERQtoZ80Message.Speech:
                    // Variable bytes for Speech data:
                    //  byte 0 = length (no greater than 32)
                    //  byte 1-N = data
                    if (_messageIndex > 1 + _messageData[0])
                    {
                        _messageIndex = 0;
                        MakeSound();
                        retVal = true;
                    }
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unhandled Speech message {0}", message);
#endif
                    break;
            }

            return retVal;
        }

        public void Poll(ref Queue<byte> fifo)
        {
            // Nothing.  Speech is output-only.
        }

        public bool Enabled
        {
            get { return _enabled; }
        }

        private void MakeSound()
        {
            int dataLength = _messageData[0];

            // Console.WriteLine("*** beep ***");
        }

        private byte[] _messageData;
        private int _messageIndex;
        private bool _enabled = false;
    }
}
