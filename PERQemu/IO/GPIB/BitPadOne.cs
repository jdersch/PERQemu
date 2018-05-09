// bitpadone.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.GPIB
{
    ///<summary>
    /// Implements an emulation of a Summagraphics BitPadOne tablet.
    /// This is used as a pointing device.
    ///</summary>
    public class BitPadOne : IGPIBDevice
    {
        public BitPadOne()
        {
            // The PERQ expects the factory default device address of 010 (octal)
            _myAddress = 8;

            //
            // How many samples/sec? The factory default is continuous streaming
            // at 200/sec, but the ICL T2 Service Guide suggests that they change
            // it to 40 samples/sec.  POS averages every four samples to smooth
            // the tablet data and reduce jitter, so the effective rate is 1/4 the
            // BitPad's hardware setting.  Maybe PNX doesn't use the same averaging
            // strategy?  In any case, choose a speed that balances overhead with
            // responsiveness.
            //
            // (NB: computed, not constant, since we may someday offer on-the-fly
            // configuration of IO board types; the EIO ran the Z80 at 4Mhz, faster
            // than the older IOB @ 2.45Mhz, so the IO "fudge" might change.  This
            // is very silly.)
            //
            _sampleRate = (PERQCpu.Frequency / 40) / PERQCpu.IOFudge;

            Reset();
        }

        public byte DeviceID
        {
            get { return _myAddress; }
        }

        public void Reset()
        {
            _lastUpdate = 0;
            _talking = false;
            _listening = false;

            _firstPoll = false;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.GPIB, "BitPadOne: Reset (address={0}).", _myAddress);
#endif
        }

        /// <summary>
        /// If the controller selects our address as the talker, enable sending tablet
        /// updates.  Otherwise, don't send any response to Poll()s.
        /// </summary>
        public void SetTalker(byte address)
        {
            _talking = (address == _myAddress);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.GPIB, "BitPadOne {0} talking.", (_talking ? "is" : "is NOT"));
#endif
            if (_talking)
            {
                _lastUpdate = 0;    // Reset counter so we don't start sampling immediately
            }
        }

        /// <summary>
        /// If the controller selects our address as the listener, someone is out to lunch.
        /// But wryly note the request for posterity, so future software archaeologists can
        /// puzzle over it.
        /// </summary>
        public void SetListener(byte address)
        {
            _listening = (address == _myAddress);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.GPIB, "BitPadOne {0} listening.", (_listening ? "is" : "is NOT"));
#endif
        }

        /// <summary>
        /// Sends a mouse position update.  Only queues updates if we're the current GPIB
        /// talker, if the current GPIB fifo is empty, and our sample timer has gone off.
        /// </summary>
        public void Poll(ref Queue<byte> fifo)
        {
            if (!_talking || Display.Display.Instance.MouseOffTablet)
            {
                // Unlike the Kriz, the BitPad does not send updates if off the tablet.
                // So bail here to allow relative mode to work... kind of...
                return;
            }

            //
            // From the BitPadOne documentation and from perusing the iogpib.pas
            // the data sent back to the PERQ looks like:
            //
            //      X X X X D1 Y Y Y Y D1 F D2
            //
            // Where XXXX/YYYY are X,Y coordinates in plain ASCII, ranging from
            // 0000-2200; D1 is configured to be "'" (single quote) and D2 is a
            // Linefeed (12 octal).  (This is "HPIB Format" in the BP1 manual.)
            // The delimiters and the range (switching between US/Metric) can
            // be configured on the BitPadOne itself, but the PERQ software
            // seems to require at least that D2 is LF (D1 can be any non-numeric
            // character at least for POS), and the US/Metric switch would obviously
            // only affect the positioning of the cursor.
            // This could be made configurable by the user but I don't see much use.
            //
            int x = 0;
            int y = 0;
            byte button = 0;

            GetTabletPos(out x, out y, out button);

            //
            // The PERQ expects a steady stream of updates from the BitPad when the
            // puck or cursor is on the tablet, so send an update even if the mouse
            // hasn't moved.  However, Accent will complain if the queue overflows
            // too often, so temper our update rate by checking that the fifo is
            // empty before sending.
            //
            if ((_lastUpdate++ > _sampleRate) && fifo.Count == 0)
            {
                WriteIntAsStringToQueue(x, ref fifo);
                fifo.Enqueue(_delimiter1);  // separator (')
                WriteIntAsStringToQueue(y, ref fifo);
                fifo.Enqueue(_delimiter1);  // separator (')
                fifo.Enqueue(_buttonMapping[button]);
                fifo.Enqueue(_delimiter2);  // LF                 

#if TRACING_ENABLED
                // For debugging GPIB, too much noise; log these updates on the Kriz channel :-)
                if (Trace.TraceOn)
                    Trace.Log(LogType.Tablet, "BitPadOne polled: x={0} y={1} button={2} update={3}",
                                        x, y, button, _lastUpdate);

                if (!_firstPoll)        // FIXME debug
                {
                    _firstPoll = true;
                    PERQSystem.Instance.Break();
                }
#endif
                _lastUpdate = 0;
            }
        }

        /// <summary>
        /// Handle writes to the BitPad.  This would make sense if the user could
        /// set it for remote configuration, which the hardware can do, but the
        /// PERQ never enabled that or supported it.
        /// </summary>
        public void Write(byte b)
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.GPIB, "BitPadOne: write requested ({0:x2})!?", b);
#endif
        }

        /// <summary>
        /// Sends the X, Y integer coordinates as four ASCII digit strings.
        /// Because THAT's efficient, 200 times a second.
        /// </summary>
        private void WriteIntAsStringToQueue(int i, ref Queue<byte> fifo)
        {
            string str = String.Format("{0:d4}", i);

            for (int j = 0; j < str.Length; j++)
            {
                fifo.Enqueue((byte)str[j]);
            }
        }

        /// <summary>
        /// Get the mouse X, Y positions from the display and translate them into
        /// the range of the BitPadOne tablet.
        /// </summary>
        private void GetTabletPos(out int x, out int y, out byte button)
        {
            //
            // Since we're using the emulated display area as the tablet surface,
            // there are certain ranges (mostly in the X axis) that we can't reach.
            // We may want to investigate some other means of tablet manipulation
            // for the emulator to make this possible.  (Hey, might be fun to support
            // an RS232 BitPadOne on the host PC...)
            //
            // Note also that unlike the Kriz tablet, the BitPadOne does not report
            // "puck off tablet" status, but it does stop transmitting in that case;
            // POS G at least has code to detect that.
            // 
            // Calculate Y and X positions.  The offsets tacked onto the end are based on
            // playing around with the interface, not on solid data and could be incorrect.
            //
            y = (Display.VideoController.PERQ_DISPLAYHEIGHT - Display.Display.Instance.MouseY) * 2 + 80;
            x = (Display.Display.Instance.MouseX) * 2 + 74;

            button = (byte)Display.Display.Instance.MouseButton;
        }

        private int _lastUpdate;
        private int _sampleRate;

        private byte _myAddress;
        private bool _talking;
        private bool _listening;

        private bool _firstPoll;

        private readonly byte[] _buttonMapping = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37,
                                                   0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46 };
        private const byte _delimiter1 = 0x27;      // '
        private const byte _delimiter2 = 0x0a;      // LF
    }
}
