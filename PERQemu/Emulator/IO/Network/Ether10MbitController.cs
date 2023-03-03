//
// Ether10MbitController.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Net.NetworkInformation;
using PERQemu.Config;
using PERQemu.Processor;

namespace PERQemu.IO.Network
{
    /// <summary>
    /// PERQ side of the "homegrown" OIO or EIO 10Mbit Ethernet controller.
    /// </summary>
    /// <remarks>
    /// Some programming documentation in Docs/HW/Ethernet_Guide_Sep81.txt and
    /// much more info about this implementation in Docs/Network.txt.
    /// </remarks>
    public class Ether10MbitController : INetworkController
    {
        public Ether10MbitController(PERQSystem sys)
        {
            _system = sys;
            _nic = null;
            _timer = null;
            _response = null;

            // Physical address is configurable, but fixed;
            _physAddr = new MachineAddress(_system.Config);
            _physAddr.Low = _system.Config.EtherAddress;

            // If not set, generate a random one (to avoid conflicts by having
            // all the PERQs on your local net come up with the same default! :-)
            if (_physAddr.Low == 0)
            {
                _physAddr.Low = (ushort)_random.Next(1, 65534);
            }

            // Receive address can be programmed; set to HW initially
            _recvAddr = new MachineAddress(_system.Config);
            _recvAddr.Low = _physAddr.Low;

            _mcastGroups = new byte[6];

            // Set interrupt vector based on board type
            _irq = (_system.Config.IOBoard == IOBoardType.EIO ? InterruptSource.Network :
                                                                InterruptSource.X);

            _nic = new HostInterface(this, Settings.EtherDevice);

            Log.Write(Category.Ethernet, "Interface created {0}", _physAddr);
        }

        // Give back the hardware MAC address
        public PhysicalAddress MACAddress => _physAddr.PA;

        // The Multicast Command Byte
        public byte MCB => _mcastGroups[0];

        public void Reset()
        {
            if (_timer != null)
            {
                _system.Scheduler.Cancel(_timer);
            }
            _timer = null;

            if (_response != null)
            {
                _system.Scheduler.Cancel(_response);
            }
            _response = null;

            if (_clockInterrupt || _netInterrupt)
            {
                _system.CPU.ClearInterrupt(_irq);
            }
            _clockInterrupt = false;
            _netInterrupt = false;

            _bitCount = 0;
            _usecClock = 0;

            _state = State.Idle;
            _status = Status.None;
            _control = Control.None;

            for (var i = 0; i < _mcastGroups.Length; i++)
            {
                _mcastGroups[i] = 0;
            }

            _nic.Reset();

            Log.Write(Category.Ethernet, "Controller reset");
        }

        public void Shutdown()
        {
            _nic.Shutdown();
        }

        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                //
                // Microsecond clock setup - used for "exponential backoff" when
                // a collision occurs, can also be programmed as a general purpose
                // timer; fires an interrupt up to 65535 microseconds from enable
                //
                case 0x88:  // Microsecond clock control
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to usec clock (control)", value);
                    break;

                case 0x89:  // uSec clock timer high byte
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to usec clock (high)", value);
                    _usecClock = (ushort)((value << 8) | (_usecClock & 0xff));
                    break;

                case 0x8a:  // uSec clock timer low byte
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to usec clock (low)", value);
                    _usecClock = (ushort)((_usecClock & 0xff00) | (value & 0xff));
                    break;

                //
                // Bit counter setup
                //
                case 0x8c:  // Bit counter control
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to bit counter (control)", value);
                    break;

                case 0x8d:  // Bit counter high byte
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to bit counter (high)", value);
                    _bitCount = (ushort)((value << 8) | (_bitCount & 0xff));
                    break;

                case 0x8e:  // Bit counter low byte
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to bit counter (low)", value);
                    _bitCount = (ushort)((_bitCount & 0xff00) | (value & 0xff));
                    break;

                case 0x90:  // Low word of MAC address
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x4} to low address register 0x{1:x2}", value, address);

                    // Have to byte swap this, because reasons
                    _recvAddr.Low = (ushort)(value << 8 | (value & 0xff00) >> 8);
                    break;

                //
                // Multicast group bytes setup - On EIO, each byte is written
                // individually; on OIO, three 16-bit values are written and
                // distributed to the MCB and group bytes (command + 5 groups)
                //
                case 0x91:  // Multicast Grp1|Cmd
                case 0x92:  // Multicast Grp3|Grp2
                case 0x93:  // Multicast Grp5|Grp4
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x4} to multicast register 0x{1:x2}", value, address);
                    var offset = address - 0x91;
                    _mcastGroups[offset] = (byte)(value & 0xff);
                    _mcastGroups[offset + 1] = (byte)(value >> 8);
                    break;

                //
                // DMA setup - addresses for the header and data buffers.  Note
                // that each part of the address provided is munged in some unique
                // way.  Don't ask.
                //
                case 0xd6:  // Packet buffer addr, high 4 bits
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x} to DMA buffer address (high)", value);
                    _bufferAddress = ((~value & 0xf) << 16) | (_bufferAddress & 0x0ffff);
                    break;

                case 0xde:  // Packet buffer addr, low 16 bits
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x4} to DMA buffer address (low)", value);
                    _bufferAddress = (_bufferAddress & 0xf0000) | (~(value ^ 0x3ff) & 0xffff);
                    break;

                case 0xd7:  // Packet header addr, high 4 bits
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x} to DMA header address (high)", value);
                    _headerAddress = ((~value & 0xf) << 16) | (_headerAddress & 0x0ffff);
                    // If we cared, the header word count is bits <7:4> ??
                    break;

                case 0xdf:  // Packet header addr, low 16 bits
                    Log.Write(Category.Ethernet, "Wrote 0x{0:x4} to DMA header address (low)", value);
                    _headerAddress = (_headerAddress & 0xf0000) | (~(value ^ 0x3ff) & 0xffff);
                    break;

                default:
                    throw new InvalidOperationException($"Unhandled write to port 0x{address:x}");
            }
        }

        public void LoadCommand(int value)
        {
            // Todo: For now, assume OIO (port 0x99) although I think the EIO
            // programming model at this level is identical?

            _control = (Control)value;
            Log.Write(Category.Ethernet, "Wrote 0x{0:x2} to control register ({1})", value, _control);

            // If the NotReset signal is not asserted, then we reset :-)
            if (!_control.HasFlag(Control.NotReset))
            {
                Reset();
                return;
            }

            // If we're busy and the Go bit dropped, abandon what we're doing
            // and return to Idle!?  Todo: find out if this stops the timer or
            // just the network (Miasma source)
            if (_state != State.Idle && !_control.HasFlag(Control.Go))
            {
                Reset();
                return;
            }

            // See if the Go flag is on and start an action.  Note that while we
            // can "see" the StartFlag, the hardware can't...
            if (_control.HasFlag(Control.Go))
            {
                // Timer: enabled, not already running, count set?
                if (_control.HasFlag(Control.ClockEnable) && _usecClock > 0 && _timer == null)
                {
                    // Start it up
                    Log.Write(Category.Ethernet, "Timer enabled: will fire in {0}usec", _usecClock);
                    _timer = _system.Scheduler.Schedule(_usecClock * Conversion.UsecToNsec, ClockOverflow);
                }

                // Transmit flag?
                if (_control.HasFlag(Control.Transmit))
                {
                    DoTransmit();
                }
                else
                {
                    // Start a receive!
                    _state = State.Receiving;
                    _status |= Status.Busy;

                    // Is it the "special" one to fetch our own address?
                    if (MCB == 0xfe)
                    {
                        Log.Write(Category.Ethernet, "Special receive to fetch address!");

                        // The minimum delay is as long as it takes to DMA one
                        // quad word, but the microcode seems to bank on the fact
                        // that there's at least enough extra delay to hold off
                        // programming the DMA registers.  "The amount of time it
                        // takes the hardware to read a preamble" is 96 bit times,
                        // so let's round up to 10usec?  Oy vey.
                        _response = _system.Scheduler.Schedule(10 * Conversion.UsecToNsec, GetAddress);
                    }

                    // DoReceive() handles live packet reception
                }
            }
        }

        public int ReadRegister(byte address)
        {
            var retVal = 0;

            switch (address)
            {
                case 0x06:
                    retVal = (_bitCount & 0xff);
                    Log.Write(Category.Ethernet, "Read 0x{0:x2} from bit counter (low)", retVal);
                    return retVal;

                case 0x07:
                    retVal = (_bitCount >> 8);
                    Log.Write(Category.Ethernet, "Read 0x{0:x2} from bit counter (high)", retVal);
                    return retVal;

                default:
                    throw new InvalidOperationException($"Unhandled write to port 0x{address:x}");
            }
        }

        public int ReadStatus()
        {
            // Save the status we'll actually return to the caller
            var retVal = (int)_status;

            // If the clock overflowed, but the net is still busy, ONLY change
            // the overflow flag?
            if (_clockInterrupt)
            {
                _clockInterrupt = false;
                _status &= ~Status.Overflow;    // Turn off the flag for next time
            }

            // If we completed a packet xmit/recv, reset to Idle and clear the
            // "successful transmission" flag.
            if (_netInterrupt)
            {
                _netInterrupt = false;
                _state = State.Idle;

                // Sigh.  RecvComplete should be a separate bit, positively asserted
                // to clearly distinguish it from XmitComplete.  There are free bits!
                _status &= ~Status.Complete;
            }

            // Assume that reading the status register clears the interrupt
            // regardless of whether the net or timer raised it -- or both!?
            _system.CPU.ClearInterrupt(_irq);
            Log.Write(Category.Ethernet, "Read status: {0} interrupt cleared, returning {1}", _irq, retVal);
            return retVal;
        }


        void GetAddress(ulong nSkew, object context)
        {
            var addr = _headerAddress;

            Log.Debug(Category.Ethernet, "Writing machine address to 0x{0:x6}", addr);

            // DMA the address bytes into the header buffer
            _system.Memory.StoreWord(addr++, _physAddr.High);
            _system.Memory.StoreWord(addr++, _physAddr.Mid);

            // The low word's four nibbles are spread out like this:
            _system.Memory.StoreWord(addr++, (ushort)((_physAddr.Hn << 12) | (_physAddr.MHn << 4)));
            _system.Memory.StoreWord(addr, (ushort)((_physAddr.MLn << 12) | (_physAddr.Ln << 4)));

            FinishCommand();
        }

        /// <summary>
        /// Let the adapter know we're interested in a given packet.  This is a
        /// simple filter to tell the hardware to give us the data or drop the
        /// packet based solely on destination address; the PERQ can run many
        /// different protocols so we don't do any interpretation at all.
        /// </summary>
        public bool WantReceive(PhysicalAddress dest)
        {
            // Is the interface in the mood?
            if (_state == State.Receiving)
            {
                // Like, REALLY in the mood?
                if (_control.HasFlag(Control.Promiscuous)) return true;

                // See if it's our hardware addr or current receive addr
                if (dest.Equals(_physAddr.PA)) return true;
                if (dest.Equals(_recvAddr.PA)) return true;

                // Always accept L2 broadcasts, too
                if (dest.Equals(HostInterface.Broadcast)) return true;

                // Finally, loop through the multicast bytes (TODO)

                // Log the rejection
                Log.Write(Category.Ethernet, "Rejecting packet for {0}", dest);
                return false;
            }

            Log.Write(Category.Ethernet, "Ignoring packet for {0} (not in Receive mode)", dest);
            return false;
        }

        /// <summary>
        /// Handle the actual reception of an incoming packet.
        /// </summary>
        public void DoReceive(byte[] packet)
        {
            // We must be actively receiving!
            if (_state != State.Receiving)
            {
                Log.Write(Category.Ethernet, "DoReceive in state {0}, packet dropped", _state);
                return;
            }

            Log.Write(Category.Ethernet, "Receiving a packet!  {0} bytes, header @ 0x{1:x6}, data @ 0x{2:x6}",
                                         packet.Length, _headerAddress, _bufferAddress);

            int addr;
            ushort data;

            //
            // Receiving is the same as sending: SharpPcap will return addresses
            // formatted "normally" so we need to hand them back to the PERQ in
            // the order it expects.  Type has to get un-munged as well.  The
            // header is 14 bytes but the DMA always ships quad words, so again
            // the first word is a dummy.  We write a zero, but could just skip it?
            addr = _headerAddress;
            _system.Memory.StoreWord(addr++, 0);

            for (var i = 0; i < 7; i++)
            {
                data = (ushort)(packet[i * 2] << 8 | packet[i * 2 + 1]);
                _system.Memory.StoreWord(addr++, data);

                Console.WriteLine("Receive: mem[{0:x6}] <= packet[{1},{2}] = 0x{3:x4}",
                                 addr, i * 2, i * 2 + 1, data);
            }

            // Copy the data packet to the buffer location (n words).  Hope the
            // byte ordering is correct.  And deal with odd length packets by
            // padding with an extra byte
            addr = _bufferAddress;

            for (var i = 14; i < packet.Length; i += 2)
            {
                data = (ushort)(packet[i] << 8);
                if (i < packet.Length - 1) data |= packet[i + 1];
                _system.Memory.StoreWord(addr++, data);
            }

            // Update our status flag and bit count
            _status |= (Status.CarrierSense | Status.PacketInProgress);
            _bitCount = (ushort)(packet.Length * 8);

            // Compute delay for DMA copy and schedule the callback to complete
            var delay = (ulong)((_bitCount * .1) + 9.6) * Conversion.UsecToNsec;
            _response = _system.Scheduler.Schedule(delay, ReceiveComplete);

            Log.Write(Category.Ethernet, "Received {0} byte packet, callback in {1}usec",
                                         packet.Length, delay / 1000);
        }

        /// <summary>
        /// Send a packet.  Handles the DMA op to create the new buffer that
        /// will be handed off to the HostInterface for transmission.
        /// </summary>
        void DoTransmit()
        {
            // The bit count is written as a negative value and counts up;  the
            // later EIO hardware automatically stops when it crosses zero?  POS
            // takes the two's complement in the microcode while Accent does it
            // in the Pascal code that sets up the DCB.  To compute transmission
            // delay, take the absolute value...
            // Todo: make sure to test with older code that may rely on the earlier
            // firmware versions that don't do it this way!  Math.Abs()?
            _bitCount = (ushort)(0 - _bitCount);

            // Sanity checks:  the microcode isn't supposed to start a new send
            // if the receiver is active, and we should check the bit count to
            // make sure the value represents a legal packet length!
            if (_bitCount < 480 || _bitCount > 12144 || _state != State.Idle)
            {
                Log.Write(Category.Ethernet, "Transmit requested while {0} or bad bit count: {1}", _state, _bitCount);

                // Uh, what to do?  There's no error provision in the spec!
                // For now, set _bitCount to zero so that TransmitComplete()
                // won't attempt to send a bad packet, finish processing the
                // send normally, and let the microcode reset us...
                _bitCount = 0;
            }

            // Set up for sending!
            _state = State.Transmitting;
            _status |= (Status.CarrierSense | Status.Busy);

            var delay = (ulong)((_bitCount * .1) + 9.6) * Conversion.UsecToNsec;
            _response = _system.Scheduler.Schedule(delay, TransmitComplete);

            Log.Write(Category.Ethernet, "Transmitting {0} byte packet, callback in {1}usec",
                                        _bitCount / 8, delay / 1000);
        }

        //
        // Callbacks for timed events
        //

        /// <summary>
        /// Actually send the packet and finish up the command.
        /// </summary>
        void TransmitComplete(ulong nSkew, object context)
        {
            if (_bitCount > 0)
            {
                // Buffer (in bytes) for the complete raw packet
                byte[] packet = new byte[_bitCount / 8];

                int addr;
                ushort data;

                //
                // DMA the header and data buffer from the PERQ's memory and hand it
                // off to the host to construct and transmit the packet.  As in the
                // receive case, we copy two quads for the header, but skip over the
                // first (unused) word.  NB: the addresses are presented in "normal"
                // order (left to right), but the Length/Type field has to be swapped;
                // SharpPcap will re-invert it for sending over the wire!  Argh.
                //
                addr = _headerAddress + 1;

                for (var i = 0; i < 6; i++)
                {
                    data = _system.Memory.FetchWord(addr++);
                    packet[i * 2] = (byte)data;                 // Low byte
                    packet[i * 2 + 1] = (byte)(data >> 8);      // High byte

                    Console.WriteLine("Send: mem[{0:x6}] => packet[{1}] = {2:x2}, packet[{3}] = {4:x2}",
                                      addr - 1, i * 2, packet[i * 2], i * 2 + 1, packet[i * 2 + 1]);
                }

                // Do the Length/Type field
                data = _system.Memory.FetchWord(addr);
                packet[12] = (byte)(data >> 8);
                packet[13] = (byte)data;
                Console.WriteLine("Send: mem[{0:x6}] => packet[12] = {1:x2}, packet[13] = {2:x2}",
                                  addr - 1, packet[12], packet[13]);

                // Now do the payload
                addr = _bufferAddress;

                for (var i = 14; i < packet.Length; i += 2)
                {
                    data = _system.Memory.FetchWord(addr++);
                    packet[i] = (byte)(data >> 8);
                    if (i + 1 < packet.Length) packet[i + 1] = (byte)data;
                }

                // Hand off the complete packet!
                var result = _nic.SendPacket(packet);
                // Todo: The only error we get back is "collision"?  We'll just
                // have to see what Pcap gives us after sending a few :-)
            }

            // Complete the transmission!
            _status &= ~(Status.CarrierSense) | Status.Complete;

            FinishCommand();
        }

        /// <summary>
        /// Finish receive processing.
        /// </summary>
        void ReceiveComplete(ulong nSkew, object context)
        {
            // Reception complete!  Turn OFF these bits:
            _status &= ~(Status.CarrierSense | Status.PacketInProgress | Status.Complete);

            FinishCommand();
        }

        /// <summary>
        /// Handle an overflow of the microsecond clock.
        /// </summary>
        void ClockOverflow(ulong nSkew, object context)
        {
            _clockInterrupt = true;

            if (_control.HasFlag(Control.ClockIntrEnable))
            {
                // Update our status and raise the interrupt
                _status |= Status.Overflow;
                _system.CPU.RaiseInterrupt(_irq);
                _timer = null;
            }
        }

        /// <summary>
        /// Reset state when a packet operation is completed, and interrupt the CPU.
        /// </summary>
        void FinishCommand()
        {
            Log.Write(Category.Ethernet, "{0} complete, raising {1} interrupt", _state, _irq);

            _response = null;
            _state = State.Complete;
            _status &= ~Status.Busy;
            _netInterrupt = true;
            _system.CPU.RaiseInterrupt(_irq);
        }

        // Debugging
        public void DumpEther()
        {
            Console.WriteLine($"{_system.Config.IOBoard} Ethernet status:");
            Console.WriteLine($"  My MAC address:    {_physAddr} ({_physAddr.High},{_physAddr.Mid},{_physAddr.Low})");
            Console.WriteLine($"  Receive address:   {_recvAddr} ({_recvAddr.High},{_recvAddr.Mid},{_recvAddr.Low})");
            Console.WriteLine($"  Control register:  {(int)_control:x} ({_control})");
            Console.WriteLine($"  Status register:   {(int)_status:x} ({_status})");
            Console.WriteLine("  Controller state:  {0}, scheduler callback {1} pending", _state,
                              (_response != null ? "IS" : "is NOT"));
            Console.WriteLine($"  DMA addresses:     Header: 0x{_headerAddress:x6}  Buffer: 0x{_bufferAddress:x6}");

            Console.WriteLine("\n  Microsecond clock: {0} enabled, interrupt {1} enabled, {2} ticks",
                              (_control.HasFlag(Control.ClockEnable) ? "IS" : "Is NOT"),
                              (_control.HasFlag(Control.ClockIntrEnable) ? "IS" : "is NOT"),
                              _usecClock);
            if (_timer != null) Console.WriteLine("  Timer is running!");

            Console.WriteLine("\n  Bit counter:       {0} enabled, {1} count",
                              (_control.HasFlag(Control.CounterEnable) ? "IS" : "Is NOT"),
                              _bitCount);

            Console.WriteLine("\n  Multicast bytes:   {0}", string.Join(", ", _mcastGroups));

            if (_nic != null) _nic.DumpStatus();
        }

        enum State
        {
            Idle = 0,
            Reset,
            Receiving,
            Transmitting,
            Complete
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
            Complete = 0x4,         // 0 = Receive, 1 = Transmit
            Busy = 0x8,
            Unused = 0x10,
            Overflow = 0x20,
            PacketInProgress = 0x40,
            CarrierSense = 0x80
        }

        State _state;
        Control _control;
        Status _status;

        InterruptSource _irq;

        MachineAddress _physAddr;
        MachineAddress _recvAddr;

        byte[] _mcastGroups;

        int _headerAddress;
        int _bufferAddress;

        bool _netInterrupt;
        bool _clockInterrupt;

        ushort _bitCount;
        ushort _usecClock;

        static Random _random = new Random();

        HostInterface _nic;

        SchedulerEvent _response;
        SchedulerEvent _timer;
        PERQSystem _system;
    }
}
