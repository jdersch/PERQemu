//
// FakeEthernet.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Network
{
    /// <summary>
    /// A temporary fake Ethernet controller.  Implement only enough to let
    /// Accent properly start up its Net/Msg servers.  For now only responds
    /// to the OIO ports.  To be replaced by a proper Ethernet someday!
    /// </summary>
    public class FakeEthernet
    {
        public FakeEthernet(PERQSystem sys)
        {
            _system = sys;
        }

        public void Reset()
        {
            _status = Status.None;
            _control = Control.None;
            Log.Info(Category.Ethernet, "Interface reset");
        }

        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                case 0x88:
                case 0x89:
                case 0x8a:    // Microsecond clock
                    break;

                case 0x8c:
                case 0x8d:
                case 0x8e:    // Bit counter
                    break;

                case 0x90:    // Address low word
                    break;

                case 0x91:
                case 0x92:
                case 0x93:    // Multicast regs
                    break;

                case 0x99:    // Control register
                    _control = (Control)value;
                    Log.Info(Category.Ethernet, "Wrote 0x{0:x2} to control register ({1})", value, _control);

                    // In order to get Accent to read the damn status, we must
                    // raise an interrupt, which basically means that any machine
                    // running Accent _must_ have an Ethernet interface (even if
                    // it isn't connected).  Why.  WHY, guys.  Couldn't spare the
                    // TWO MICROINSTRUCTIONS to check the damn status register at
                    // init to determine if the hardware is present?  Seriously.
                    //if (_control.HasFlag(Control.NetIntrEnable))
                    //{
                    //    Console.WriteLine("X interrupt raised");
                    //    _system.CPU.RaiseInterrupt(Processor.InterruptSource.X);
                    //}
                    break;

                case 0xd6:
                case 0xd7:
                case 0xde:
                case 0xdf:    // DMA setup
                    break;

                default:
                    throw new InvalidOperationException($"Unhandled write to port 0x{address:x}");
            }
        }

        public int ReadRegister(byte address)
        {
            switch (address)
            {
                case 0x06:    // Bit count
                case 0x07:
                    return 0;

                case 0x0f:    // Status
                    //Log.Error(Category.Ethernet, "READ FAKE E10 STATUS");
                    //
                    // Doesn't matter what we put here; Accent never reads it.  The
                    // EtherStatus function in the new NetMsg server is never called;
                    // the microcode only reads the register in response to an interrupt
                    // which, if there's no hardware in the machine, is never generated.
                    // Brilliancy.
                    //
                    _system.CPU.ClearInterrupt(Processor.InterruptSource.X);
                    Console.WriteLine($"X interrupt cleared, returning {_status}");
                    return (int)_status;

                case 0x70:    // Fake 3Mbit usec clock
                    return 0;

                case 0x7b:    // Fake 3Mbit status reg
                    // Without this, Accent dies on a trap 131 at startup :-(
                    return 0xffff;

                default:
                    throw new InvalidOperationException($"Unhandled write to port 0x{address:x}");
            }
        }

        public void SetStatus(byte stat)
        {
            _status = (Status)stat;
            Console.WriteLine($"Fake Ethernet status set to {_status}");
        }
            
        Control _control;
        Status _status;
        PERQSystem _system;
    }

    [Flags]
    /// <summary>
    /// OIO Ethernet control register bits.  NB: Reset is assert LOW.  Bits 7 and
    /// 9..15 are undefined in the hardware but may be used by the microcode.
    /// </summary>
    enum Control
    {
        None = 0x0,
        NetIntrEnable = 0x1,
        ClockIntrEnable = 0x2,
        ClockEnable = 0x4,
        CounterEnable = 0x8,
        Transmit = 0x10,
        NotReset = 0x20,
        Promiscuous = 0x40,
        SleepFlag = 0x80,
        Go = 0x100,
        StartFlag = 0x200
    }

    [Flags]
    enum Status
    {
        None = 0x0,
        CRCError = 0x1,
        Collision = 0x2,
        Complete = 0x4,
        Busy = 0x8,
        Unused = 0x10,
        Overflow = 0x20,
        PacketInProgress = 0x40,
        CarrierSense = 0x80
    }
}
