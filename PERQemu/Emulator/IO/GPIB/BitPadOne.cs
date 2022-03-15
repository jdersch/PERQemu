//
// BitPadOne.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using System;

namespace PERQemu.IO.GPIB
{
    ///<summary>
    /// Implements an emulation of a Summagraphics BitPadOne tablet.
    /// This is used as a pointing device.
    ///</summary>
    public class BitPadOne : IGPIBDevice
    {
        public BitPadOne(Scheduler scheduler, PERQSystem system)
        {
            _system = system;
            _scheduler = scheduler;
            _txDelegate = null;
            _sampleEvent = null;
        }

        // The PERQ expects the factory default device address of 010 (octal)
        public byte DeviceID => 8;

        // Number of updates per second
        public const int SampleRate = 40;

        public void BusReset()
        {
            _talking = false;
            _listening = false;

            Log.Debug(Category.GPIB, "BitPadOne reset (address={0})", DeviceID);
        }

        /// <summary>
        /// The bus gives us back a handle to write to when we're talkin' heeyah.
        /// </summary>
        public void RegisterBusWriteDelegate(BusWriteDelegate writeDelegate)
        {
            _txDelegate = writeDelegate;
        }

        /// <summary>
        /// Ignore writes to the BitPad.  This would make sense if the user could
        /// set it for remote configuration, which the hardware can do, but the
        /// PERQ never enabled that or supported it.
        /// </summary>
        public void BusRead(byte value, BusStatus flags)
        {
            throw new InvalidOperationException("BitPadOne is not a Listener");
        }

        /// <summary>
        /// If the controller selects our address as the talker, enable sending
        /// tablet updates and start sampling the mouse.
        /// </summary>
        public void SetTalker(byte address)
        {
            _talking = (address == DeviceID);

            if (_talking && _sampleEvent == null)
            {
                _sampleEvent = _scheduler.Schedule(_sampleInterval, SendData);
            }

            Log.Debug(Category.GPIB, "BitPadOne {0} talking", (_talking ? "is" : "is NOT"));
        }

        /// <summary>
        /// If the controller selects our address as the listener, someone is out
        /// to lunch.  But wryly note the request for posterity, so future software
        /// archaeologists can puzzle over it.
        /// </summary>
        public void SetListener(byte address)
        {
            _listening = (address == DeviceID);

            Log.Debug(Category.GPIB, "BitPadOne {0} listening", (_listening ? "is" : "is NOT"));
        }

        /// <summary>
        /// Samples the mouse position and buttons n times per second and sends
        /// an update to the PERQ via the 9914/Z80.  Due to the asynchronous and
        /// unpredictable nature of the GPIB controller's polling -- the PERQ
        /// turns the tablet off and on quite frequently -- we collect the mouse
        /// data every trip, but only send it if we're the GPIB Talker.
        /// </summary>
        private void SendData(ulong skewNsec, object context)
        {
            // Please see the Notes below for tablet data format details!

            int x = 0;
            int y = 0;
            byte button = 0;

            // Calculate X and Y positions based on the "fudge factors" below:
            x = (_system.Mouse.MouseX + 38) * 2;
            y = (_system.VideoController.DisplayHeight - _system.Mouse.MouseY + 39) * 2;

            button = (byte)_system.Mouse.MouseButton;

            if (_talking && !_system.Mouse.MouseOffTablet)
            {
                // Send it!
                WriteIntAsStringToQueue(x);
                _txDelegate(DeviceID, _delimiter1, BusStatus.DAV);
                WriteIntAsStringToQueue(y);
                _txDelegate(DeviceID, _delimiter1, BusStatus.DAV);
                _txDelegate(DeviceID, _buttonMapping[button], BusStatus.DAV);
                _txDelegate(DeviceID, _delimiter2, BusStatus.DAV | BusStatus.EOI);

                // Log the Tablet update
                Log.Debug(Category.Tablet, "BitPadOne sampled: x={0} y={1} button={2} ({3})",
                          x, y, button, (char)_buttonMapping[button]);
            }

            // Still the talker?  Then reschedule our next update (though the
            // bus will quietly ignore us if our status changes in the meantime)
            if (_talking)
            {
                _sampleEvent = _scheduler.Schedule(_sampleInterval, SendData);
            }
            else
            {
                _sampleEvent = null;
            }
        }

        /// <summary>
        /// Sends the X, Y integer coordinates as four digit ASCII strings.
        /// Because THAT's efficient, 125 times a second.
        /// </summary>
        private void WriteIntAsStringToQueue(int i)
        {
            string str = string.Format("{0:d4}", i);

            for (int j = 0; j < str.Length; j++)
            {
                _txDelegate(DeviceID, (byte)str[j], BusStatus.DAV);
            }
        }

        private bool _talking;
        private bool _listening;
        private BusWriteDelegate _txDelegate;

        private readonly byte[] _buttonMapping = { 0x30, 0x32, 0x31, 0x33, 0x38, 0x35, 0x36, 0x37,
                                                   0x34, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46 };
        private const byte _delimiter1 = 0x2c;      // ,
        private const byte _delimiter2 = 0x0a;      // LF

        private ulong _sampleInterval = (1000 / SampleRate) * Conversion.MsecToNsec;
        private SchedulerEvent _sampleEvent;

        private Scheduler _scheduler;
        private PERQSystem _system;
    }
}

/*

    Notes:

    The Summagraphics factory default is continuous streaming at 125 samples/sec
    (max GPIB rate), but the ICL T2 Service Guide suggests that ICL/3RCC set it
    to 40 samples/sec.  POS, and likely the other OSes, ignores a few samples
    every time the puck or stylus is returned to the surface.  POS queues the
    tablet updates in a FIFO and averages several samples to reduce jitter; the
    effective rate is thus reduced a bit from the BitPad's hardware setting.
    For emulation purposes we are free to choose a sample rate that balances
    overhead with responsiveness (not limited to the table of available values
    in the manual).


    From the BitPadOne documentation and from perusing the iogpib.pas the data
    sent back to the PERQ looks like:

      X X X X D1 Y Y Y Y D1 F D2

    Where XXXX/YYYY are X,Y coordinates in plain ASCII, ranging from 0000-2200;
    D1 is configured to be "," (a *$!%#! COMMA) and D2 is a Linefeed (12 octal).
    (This is "HPIB Format" in the BP1 manual, which clearly and INCORRECTLY
    shows D1 as a single quote mark.)  The origin is defined as the lower left,
    so for the PERQ we flip the Y coordinate.

    The delimiters and the range (switching between US/Metric) can be configured
    on the BitPad itself, but the PERQ software requires that D2 is LF (D1 can
    be any non-numeric character for POS, but Accent expects a COMMA), and the
    US/Metric switch would obviously only affect the positioning of the cursor.
    This could be made configurable by the user but I don't see much use.

    The PERQ expects a steady stream of updates from the BitPad when the puck
    or stylus is on the tablet, even if the mouse hasn't moved.  Note also that
    unlike the Kriz tablet, the BitPadOne does not report "puck off tablet"
    status, but it does stop transmitting in that case; POS G (at least) has
    code to detect that.


    { Fudge factors for BitPad. }
    GPIBxFudge = 38;        { actual range in X and Y for BitPad: 0..2200 }
    GPIByFudge = 1061;      { of TX and TY: 0..1100 }
                            { of TabAbsX: 0..1100 }
                            { of TabAbsY: 0..1100 }
                            { of TabRelX: -38..1062   limited to 0..767 }
                            { of TabRelY: 1061..-39   limited to 1023..0 }

    With the introduction of the Landscape display, the driver modifies the
    width scaling accordingly.
    
*/
