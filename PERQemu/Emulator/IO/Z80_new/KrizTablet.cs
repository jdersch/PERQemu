using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using PERQemu.Memory;

namespace PERQemu.IO.Z80_new
{
    /// <summary>
    /// Implements the Kriz tablet.
    /// TODO: This should be moved to a common peripherals directory/namespace once
    /// the final organization of IOB-related goods is determined.
    /// </summary>
    public class KrizTablet : ISIODevice
    {
        public KrizTablet(Scheduler scheduler, PERQSystem system)
        {
            _scheduler = scheduler;

            // TODO: maybe just pass Display in?
            _system = system;

            Reset();
        }

        public void Reset()
        {
            // Schedule the first Kriz data event, which runs once every 1/60th of a second, forever.
            _scheduler.Schedule(_dataInterval, SendData);
        }

        public void RegisterReceiveDelegate(ReceiveDelegate rxDelegate)
        {
            _rxDelegate = rxDelegate;
        }

        public void TransmitAbort()
        {
            // Should never happen.
            throw new NotImplementedException();
        }

        public void TransmitBreak()
        {
            // Should never happen.
            throw new NotImplementedException();
        }

        public void Transmit(byte value)
        {
            // Should never receive data.
            throw new NotImplementedException();
        }

        private void SendData(ulong skewNsec, object context)
        {
            // From V87.z80, the Kriz tablet data looks like:
            //;TABCHK checks for Tablet update to send to PERQ. Every 1/60 of a second,
            //;the Kriz Tablet sends a 5-byte Tablet update msg:
            //;   Byte0<7:0> = sync char (filtered out by SIO B hardware)
            //;   Byte1<3:0> = high bits of X
            //;   Byte1<4>   = unused (0)
            //;   Byte1<5>   = TabCoil (always 0 for now)
            //;   Byte1<6>   = TabOffTablet (1 -> mouse off tablet)
            //;   Byte1<7>   = 0
            //;   Byte2<7:0> = low X
            //;   Byte3<3:0> = high bits of Y
            //;   Byte3<4>   = unused (0)
            //;   Byte3<7:5> = the 3 Switches
            //;   Byte4<7:0> = low Y
            //;
            // (The Sync char is 0x81)
            //
            // Also from v87.v80:
            //; Note: Tablet data is active low.
            // This is useful information as it turns out.

            // TODO: there really should be thread synchronization here for retrieving this data.
            // Calc Y and X positions, coordinate translation based on tweaking so the host screen
            // coordinates line up with the PERQ's...
            int tabX = _system.Display.MouseX + 64;
            int tabY = VideoController.PERQ_DISPLAYHEIGHT - _system.Display.MouseY + 64;
            int buttons = _system.Display.MouseButton;
            bool offTablet = _system.Display.MouseOffTablet;

            // Send the data off to the SIO.
            _rxDelegate(0x81);                  // sync
            _rxDelegate((byte)~((tabX >> 8) | (offTablet ? 0x40 : 0x00)));     // high bits of X + TabOffTablet
            _rxDelegate((byte)~(tabX));         // low X
            _rxDelegate((byte)~((tabY >> 8) | (buttons << 5)));     // high bits of Y + switch bits
            _rxDelegate((byte)~(tabY));         // low Y
            _rxDelegate(0); // Garbage:  The Z80 code expects to read 6 characters for some reason, it throws away the last two.
            _rxDelegate(0);

            // Wait 1/60th of a second and do it again.
            _scheduler.Schedule(_dataInterval, SendData);
        }

        private static readonly ulong _dataInterval = (ulong)(16.666667 * Conversion.MsecToNsec);

        private ReceiveDelegate _rxDelegate;
        private Scheduler _scheduler;
        private PERQSystem _system;
    }
}
