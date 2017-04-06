// bitpadone.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.Text;

namespace PERQemu.IO.GPIB
{
    //
    // Implements an emulation of a Summagraphics BitPadOne tablet.
    // This is used as a pointing device.
    //
    [Serializable]
    public class BitPadOne : IGPIBDevice
    {
        public BitPadOne()
        {
            Reset();
        }

        public byte DeviceID 
        { 
            get 
            { 
                return 10;     // Most PERQ software seems to assume the tablet is device 10.
            } 
        }

        public void Reset()
        {
            _lastX = -1;
            _lastY = -1;
            _lastButton = -1;
        }

        public void Poll(ref Queue<byte> fifo)
        {
            //
            // From the BitPadOne documentation and from perusing the iogpib.pas
            // the data sent back to the PERQ looks like:
            //
            //      X X X X D1 Y Y Y Y D1 F D2
            //
            // Where XXXX/YYYY are X,Y coordinates in plain ASCII, ranging from
            // 0000-2200; D1 is configured to be "'" (single quote) and D2 is a
            // Linefeed (12 octal).
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
            
            if (x != _lastX || y != _lastY || button != _lastButton)
            {                
                WriteIntAsStringToQueue(x, ref fifo);
                fifo.Enqueue(_delimiter1);  // separator (')
                WriteIntAsStringToQueue(y, ref fifo);
                fifo.Enqueue(_delimiter1);  // separator
                fifo.Enqueue(_buttonMapping[button]);
                fifo.Enqueue(_delimiter2);   // LF                 
                
                _lastX = x;
                _lastY = y;
                _lastButton = button;
            }
        }

        public void Write(byte b)
        {
            // Nuttin' yet -- maybe a nice HPIB printer someday? POS "print" supports one...
        }

        private void WriteIntAsStringToQueue(int i, ref Queue<byte> fifo)
        {
            string str = String.Format("{0:d4}", i);

            for (int j = 0; j < str.Length; j++)
            {
                fifo.Enqueue((byte)str[j]);                
            }            
        }

        private void GetTabletPos(out int x, out int y, out byte button)
        {
            //
            // Get the X & Y position from the display and translate them into the range
            // of the BitPadOne tablet.
            //
            // Since we're using the emulated display area as the tablet surface, there are certain ranges
            // (mostly in the X axis) that we can't reach.  We may want to investigate some other
            // means of tablet manipulation for the emulator to make this possible.  (Hey, might be fun to
            // support an RS232 BitPadOne on the host PC...)
            //
            // Note also that unlike the Kriz tablet, the BitPadOne does not report "puck off tablet" status.
            //

            //
            // Calc y and x positions.  The offsets tacked onto the end are based on playing around with the
            // interface, not on solid data and could be incorrect.
            //
            y = (Display.VideoController.PERQ_DISPLAYHEIGHT - Display.Display.Instance.MouseY) * 2 + 82;
            x = (Display.Display.Instance.MouseX) * 2 + 74;

            button = (byte)Display.Display.Instance.MouseButton;
        }

        private int _lastX;
        private int _lastY;
        private int _lastButton;        

        private readonly byte[] _buttonMapping = { 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x41, 0x42, 0x43, 0x44, 0x45, 0x46 };
        private const byte _delimiter1 = (byte)'\'';
        private const byte _delimiter2 = 0xa;          // LF

    }
}
