//
// KrizTablet.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.Z80;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// Implements the Kriz tablet.  This is a custom electromagnetic ranging
    /// tablet designed and patented by PERQ engineer J. Stanley Kriz, used
    /// primarily on the PERQ-2 line of workstations.  Comes in both portrait
    /// and landscape orientations to match the selected display.  Interfaces
    /// with the Z80 via a serial port.
    /// </summary>
    public class KrizTablet : ISIODevice
    {
        public KrizTablet(Scheduler scheduler, PERQSystem system)
        {
            _scheduler = scheduler;
            _system = system;
            _sendEvent = null;

            // CIO and EIO use different sync characters!
            _sync = (byte)(_system.Config.Chassis == Config.ChassisType.PERQ1 ? 0x81 : 0x7e);
            
        }

        public void Reset()
        {
            // Schedule the first data event, which runs once every 1/60th of
            // a second, forever.  But don't re-register it again and again...
            if (_sendEvent != null)
            {
                _scheduler.Cancel(_sendEvent);
            }

            _sendEvent = _scheduler.Schedule(_dataInterval, SendData);
            Log.Debug(Category.Tablet, "Kriz reset");
        }

        public void RegisterReceiveDelegate(ReceiveDelegate rxDelegate)
        {
            _rxDelegate = rxDelegate;
        }

        public void TransmitAbort()
        {
            // Should never happen
            throw new NotImplementedException("TransmitAbort on Kriz");
        }

        public void TransmitBreak()
        {
            // Should never happen
            throw new NotImplementedException("TransmitBreak on Kriz");
        }

        public void Transmit(byte value)
        {
            // Should never receive data... but it does!?  PERQdebugger (aka
            // PERQman :-) in the POS G demo sends data to the tablet for some
            // unknown reason.  Ignore it to avoid halting the emulator, for
            // now; maybe figure out if it's supposed to be handled?
            Log.Debug(Category.Tablet, "Kriz received byte 0x{0:x2} (ignored)", value);
        }

        void SendData(ulong skewNsec, object context)
        {
            // See the Notes below for message format details!

            // Calc Y and X positions.  SDL provides absolute mouse positions
            // clipped to the PERQ screen dimensions for us, so there's no need
            // to adjust for display width.  Apply the X/Y "kluge" values based
            // on the POS tablet driver's expectations (see below)
            int tabX = _system.Mouse.MouseX + 64;
            int tabY = _system.VideoController.DisplayHeight - _system.Mouse.MouseY + 64;

            // Format 'em and invert (data is active low)
            var tab1 = (byte)~(((tabX >> 8) & 0x0f) |
                               (_system.Mouse.MouseOffTablet ? 0x40 : 0) |
                               (_system.Config.Display == Config.DisplayType.Landscape ? 0x20 : 0));
            var tab2 = (byte)~(tabX & 0xff);
            var tab3 = (byte)~(((tabY >> 8) & 0x0f) | (_system.Mouse.MouseButton << 5));
            var tab4 = (byte)~(tabY & 0xff);

            // Send the data off to the SIO
            _rxDelegate(_sync);     // sync
            _rxDelegate(tab1);      // high bits of X + flags
            _rxDelegate(tab2);      // low X
            _rxDelegate(tab3);      // high bits of Y + switch bits
            _rxDelegate(tab4);      // low Y
            _rxDelegate(0);         // junk bytes
            _rxDelegate(0);         // (see below)

            // Log the Tablet update
            Log.Debug(Category.Tablet, "Kriz sampled: x={0} y={1} button={2}",
                      tabX, tabY, (tab3 >> 5));

            // Wait 1/60th of a second and do it again
            _sendEvent = _scheduler.Schedule(_dataInterval, SendData);
        }


        static readonly ulong _dataInterval = (ulong)(16.666667 * Conversion.MsecToNsec);

        byte _sync;

        ReceiveDelegate _rxDelegate;
        SchedulerEvent _sendEvent;
        Scheduler _scheduler;
        PERQSystem _system;
    }
}

/*
    Notes:
 
    The Kriz tablets send updates every 1/60th of a second to the Z80 on
    serial port SIO B.  The message format is:
        <sync><data0>..<data4><pad0><pad1>

    The Sync char is 0x81 (for CIO) or 0x7e (EIO).  Two "junk" pad bytes are
    tacked on because, as the ROM explains:
    
        ; Note: A complete msg is only 4 chars.  But we count 2 extra chars
        ; and just throw them away.  This was done to overcome problem we had
        ; with SIO internal operation on Sync recognition when it is programmed 
        ; back into Hunt mode below.

    That comment is out of date; the format changed from 4 (v8.6) to 5 (v8.7)
    characters.  The SIO B receive routine counts from 0..6, so seven total bytes
    make up a complete tablet message.
     
    Data format (from the v8.7 ROM), with updates from "kriz.doc":

        ;   Byte0<7:0> = sync char (filtered out by SIO B hardware)
        ;   Byte1<7>   = ValidMsg       (0)
        ;   Byte1<6>   = TabOffTablet   (1 -> mouse off tablet)
        ;   Byte1<5>   = Landscape      (1 -> landscape tablet)
        ;   Byte1<4>   = unused         (0)
        ;   Byte1<3:0> = high bits of X
        ;   Byte2<7:0> = low X
        ;   Byte3<7:5> = the 3 Switches (right, middle, left)
        ;   Byte3<4>   = unused         (0)
        ;   Byte3<3:0> = high bits of Y
        ;   Byte4<7:0> = low Y
    
    The Z80 then reformats that into a different format to send to the PERQ.
    
    Also from v87.v80:
    
        ; Note: Tablet data is active low.

    This is useful information as it turns out, as are the "fudge factors" for
    applying cursor offsets (io_private.pas):

    { Fudge factors for Kriz Tablet. }
    KrizXfudge = 64;        { actual range of X is 0..895 }
    KrizYfudge = 1087;      { actual range of Y is 0..1151 }
                            { of TX, TabAbsX: 0..895  }
                            { of TX, TabAbsY: 0..1151 }
                            { of TabRelX: -64..831    limited to 0..767 }
                            { of TabRelY: 1087..-64   limited to 1023..0 }


    Note that the only way for the tablet itself to "know" if it's been enabled
    or disabled is to peek at the SIO's receiver status.  The hardware obviously
    can't do that and just transmits the data stream regardless of whether the
    Z80 is doing anything with it.  Unfortunately, that's all we can do here too.
    The SIO will ignore the messages when the tablet is disabled, and since it's
    relatively low impact to schedule and process this message, I'm not going to
    try to snoop the SIO's RxEnabled bit and maintain some kind of on/off state
    for the tablet.  The prefab Configurations just don't enable the Kriz for
    early PERQ-1 configs to avoid the extra overhead.
*/
