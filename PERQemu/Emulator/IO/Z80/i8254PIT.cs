//
// i8254PIT.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Intel i8254 Programmable Interval Timer.
    /// </summary>
    /// <remarks>
    /// This chip provides baud rate clocks for the RS-232 ports and speech device
    /// on the EIO board.  It replaces the Z80 CTC used on the older IOB/CIO board.
    /// 
    /// The EIO uses two chips to provide six timers (baud rate clocks):
    ///     Timer   Chn
    ///       A      0      RS232 A Receive
    ///       A      1      Speech Transmit / Tablet Receive
    ///       A      2      RS232 A Transmit
    ///       B      0      RS232 B Receive
    ///       B      1      Keyboard Transmit/Receive
    ///       B      2      RS232 B Transmit
    /// </remarks>
    public class i8254PIT : IZ80Device
    {
        public i8254PIT(byte baseAddress)
        {
            _baseAddress = baseAddress;
            _ports = new byte[] {
                                    _baseAddress,
                                    (byte)(_baseAddress + 1),
                                    (byte)(_baseAddress + 2),
                                    (byte)(_baseAddress + 3)
                                };

            // _channels[]
        }

        public string Name => "i8254 PIT";
        public byte[] Ports => _ports;

        // Fixme: placeholders until implemented so we can start debugging
        public bool IntLineIsActive => false;
        public byte? ValueOnDataBus => null;

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        public void Reset()
        {
            Log.Debug(Category.CTC, "i8254 reset");
        }

        public byte Read(byte portAddress)
        {
            Log.Debug(Category.CTC, "Read from 0x{0:x2}, returning 0 (unimplemented)", portAddress);
            return 0;
        }

        public void Write(byte portAddress, byte value)
        {
            Log.Debug(Category.CTC, "Write 0x{0:x2} to port 0x{1:x2} (unimplemented)", value, portAddress);
        }

        byte _baseAddress;
        byte[] _ports;
    }
}

/*
    Notes:

    ;
    ;        COUNTER/TIMER CHIP
    ;
    CTC.ARx    equ 120Q            ; CHANNEL 0 (RS-232 Port A Receive Speed)
    CTC.Speech equ 121Q            ; CHANNEL 1 (RS-232 CH B Speech / Tablet)
    CTC.ATx    equ 122Q            ; CHANNEL 2 (RS-232 Port A Transmit Speed)
    CTCA.CSR   equ 123Q            ; Control Register
    CTC.SpSel  equ 01000000B       ; Select Speech counter
    CTC.ATxSel equ 10000000B       ; Select Port A Transmit counter
    CTC.ARxSel equ 00000000B       ; Select Port A Receive counter

    CTC.BRx    equ 124Q            ; CHANNEL 0 (RS-232 Port B Receive Speed)
    CTC.KB     equ 125Q            ; CHANNEL 1 (Keybaord Receive/Transmit Speed)
    CTC.BTx    equ 126Q            ; CHANNEL 2 (RS-232 Port B Transmit Speed)
    CTCB.CSR   equ 127Q            ; Control Register
    CTC.BTxSel equ 10000000B       ; Select Port B Transmit counter
    CTC.KBSel  equ 01000000B       ; Select Keyboard counter
    CTC.BRxSel equ 00000000B       ; Select Port B Receive counter
    CTC.M0     equ 00H             ; Select Mode 0
    CTC.M1     equ 02H             ; Select Mode 1
    CTC.M3     equ 06H             ; Select Mode 3  (Square wave)
    CTC.Latch  equ 00H             ; Latch counter
    CTC.LSB    equ 10H             ; Load LSB only
    CTC.MSB    equ 20H             ; Load MSB only
    CTC.Both   equ 30H             ; Load both LSB and MSB

*/
