//
// Oki5832RTC.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
    /// Oki M5M5832 Real-time Clock chip.  Provides a battery-backed calendar/
    /// clock for the EIO board.
    /// </summary>
    /// <remarks>
    /// This implementation uses the host's clock to provide the time and date,
    /// which is typically requested once by the OS at bootup.  In one of the
    /// most baffling decisions Three Rivers made, the RTC chip is not writable
    /// without a special clip and boot floppy; the extra latch and Z80 code to
    /// set the date and time after a battery replacement (or time zone change,
    /// to account for drift, etc.) was designed but then *deliberately removed*.
    /// WHY.  Seriously.  WTF.
    /// 
    /// All surviving real PERQ-2s out there are likely either running with dead
    /// batteries or with incorrect date and time settings, as the boot floppy
    /// and special DIL clip hack required to program the RTC is likely lost to
    /// the sands of time.  I have drawn up a small pluggable daughterboard that
    /// can be socketed into existing EIO boards (both 20- and 24-bit).  This
    /// module will simulate the original EIO board design that includes the
    /// write latch so that a new programming procedure can be developed and
    /// tested before PCBs are fabricated.
    /// </remarks>
    public class Oki5832RTC
    {
        public Oki5832RTC()
        {
        }
    }
}
