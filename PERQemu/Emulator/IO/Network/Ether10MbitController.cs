//
// Ether10MbitController.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

            // Set interrupt vector, DMA channel based on board type
            if (_system.Config.IOBoard == IOBoardType.EIO)
            {
                _irq = InterruptSource.Network;
                _dmaTx = ChannelName.NetXmit;
                _dmaRx = ChannelName.NetRecv;
            }
            else
            {
                // Assume the OIO interface
                _irq = InterruptSource.X;
                _dmaTx = ChannelName.ExtA;
                _dmaRx = ChannelName.ExtA;
            }

            // Open the host network adapter
            _nic = new HostAdapter(this, Settings.EtherDevice);

            Log.Info(Category.Ethernet, "Interface created {0}", _physAddr);
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

            Log.Info(Category.Ethernet, "Controller reset");
        }

        public void Shutdown()
        {
            _nic.Shutdown();
        }

        /// <summary>
        /// Ethernet register loads.  Handles both OIO and EIO variants.
        /// </summary>
        /// <remarks>
        /// The microsecond clock is used for "exponential backoff" when a collision
        /// occurs, but Pcap insulates us from that.  It can also be programmed as a
        /// general purpose timer; fires an interrupt up to 65535usec from enable.
        /// 
        /// The bit counter is used by the hardware to know how many bytes to send,
        /// and by the receiver to count incoming bits (which must end as a multiple
        /// of 8 to know if the final byte count is valid).  Here we basically ignore
        /// the counter control register that the microcode uses to manage the counter
        /// and just assume it's active when needed.
        /// </remarks>
        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                //
                // Microsecond clock
                //
                case 0x88:      // OIO - Microsecond clock control
                case 0xdc:      // EIO -      "        "      "
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to usec clock (control)", value);
                    // Todo: actually run the clock!?
                    break;

                case 0x89:      // OIO - uSec clock timer high byte
                case 0xdd:      // EIO -  "     "     "    "    "
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to usec clock (high)", value);
                    _usecClock = (ushort)((value << 8) | (_usecClock & 0xff));
                    break;

                case 0x8a:      // OIO - uSec clock timer low byte
                case 0xde:      // EIO -  "     "     "    "   "
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to usec clock (low)", value);
                    _usecClock = (ushort)((_usecClock & 0xff00) | (value & 0xff));
                    break;

                //
                // Bit counter 
                //
                case 0x8c:      // OIO - Bit counter control
                case 0xd8:      // EIO -  "     "       "
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to bit counter (control)", value);
                    // Todo: Uh, actually do something?
                    break;

                case 0x8d:      // OIO - Bit counter high byte
                case 0xd9:      // EIO -  "     "     "    "
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to bit counter (high)", value);
                    _bitCount = (ushort)((value << 8) | (_bitCount & 0xff));
                    break;

                case 0x8e:      // OIO - Bit counter low byte
                case 0xda:      // EIO -  "     "     "   "
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to bit counter (low)", value);
                    _bitCount = (ushort)((_bitCount & 0xff00) | (value & 0xff));
                    break;

                //
                // Receive address
                //
                case 0x90:      // OIO - Low word of MAC address
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x4} to low address register 0x{1:x2}", value, address);

                    // Have to byte swap this, because reasons
                    _recvAddr.Low = (ushort)(value << 8 | (value & 0xff00) >> 8);
                    break;

                case 0xc9:      // EIO - Low word of MAC address (5th octet)
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to low address register (octet 5)", value & 0xff);
                    _recvAddr.Low = (ushort)((value << 8) | (_recvAddr.Low & 0xff00));
                    break;

                case 0xc8:      // EIO - Low word of MAC address (6th octet)
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to low address register (octet 6)", value & 0xff);
                    _recvAddr.Low = (ushort)((_recvAddr.Low & 0x00ff) | (value & 0xff));
                    break;

                //
                // Multicast group bytes setup - On EIO, each byte is written
                // individually; on OIO, three 16-bit values are written and
                // distributed to the MCB and group bytes (command + 5 groups)
                //
                case 0x91:      // OIO - Multicast Grp1|Cmd
                case 0x92:      // OIO - Multicast Grp3|Grp2
                case 0x93:      // OIO - Multicast Grp5|Grp4
                    var offset = address - 0x91;
                    _mcastGroups[offset] = (byte)(value & 0xff);
                    _mcastGroups[offset + 1] = (byte)(value >> 8);
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x4} to multicast register 0x{1:x2}", value, address);
                    break;

                case 0xca:      // EIO - Multicast command byte
                case 0xcb:      // EIO - Multicast group 1
                case 0xcc:      // EIO - Multicast group 2
                case 0xcd:      // EIO - Multicast group 3
                case 0xce:      // EIO - Multicast group 4
                case 0xcf:      // EIO - Multicast group 5
                    var mcgb = address - 0xca;
                    _mcastGroups[mcgb] = (byte)(value & 0xff);
                    Log.Debug(Category.Ethernet, "Wrote 0x{0:x2} to multicast register 0x{1:x2}", value, address);
                    break;

                default:
                    throw new InvalidOperationException($"Unhandled write to port 0x{address:x}");
            }
        }

        /// <summary>
        /// Write to the Ethernet control register.  For now, assume that the OIO
        /// (port 0x99) and EIO (port 0xc2) are programmed in the same way?
        /// </summary>
        public void LoadCommand(int value)
        {
            _control = (Control)value;
            Log.Info(Category.Ethernet, "Wrote 0x{0:x2} to control register ({1})", value, _control);

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
                    Log.Info(Category.Ethernet, "Timer enabled: will fire in {0}usec", _usecClock);
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
                    _state = State.ReceiveWait;
                    _status |= Status.Busy;

                    // Is it the "special" one to fetch our own address?
                    if (MCB == 0xfe)
                    {
                        Log.Info(Category.Ethernet, "Special receive to fetch address!");

                        // The minimum delay is as long as it takes to DMA one
                        // quad word, but the microcode seems to bank on the fact
                        // that there's at least enough extra delay to hold off
                        // programming the DMA registers.  "The amount of time it
                        // takes the hardware to read a preamble" is 96 bit times,
                        // so let's round up to 10usec?  Oy vey.
                        _response = _system.Scheduler.Schedule(10 * Conversion.UsecToNsec, GetAddress);
                    }

                    // Go see if the NIC has anything queued up!
                    _nic.CheckReceive();
                }
            }
        }

        public int ReadRegister(byte address)
        {
            var retVal = 0;

            switch (address)
            {
                case 0x07:      // OIO - Read bit counter high byte
                case 0x5b:      // EIO -  "    "     "     "    "
                    retVal = (_bitCount >> 8) & 0xff;
                    Log.Debug(Category.Ethernet, "Read 0x{0:x2} from bit counter (high)", retVal);
                    return retVal;

                case 0x06:      // OIO - Read bit counter low byte
                case 0x5a:      // EIO -  "    "     "     "   "
                    retVal = (_bitCount & 0xff);
                    Log.Debug(Category.Ethernet, "Read 0x{0:x2} from bit counter (low)", retVal);
                    return retVal;

                default:
                    throw new InvalidOperationException($"Unhandled read from port 0x{address:x}");
            }
        }

        /// <summary>
        /// Reads the status register.  OIO port 0017 (0x0f); EIO port 0122 (0x52)
        /// </summary>
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
            Log.Info(Category.Ethernet, "Read status: {0} interrupt cleared, flags {1} ({2:x})",
                                        _irq, _status, retVal);
            return retVal;
        }


        void GetAddress(ulong nSkew, object context)
        {
            // Get the header address from DMA
            var addr = _system.IOB.DMARegisters.GetHeaderAddress(_dmaRx);

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
            // Gimme gimme gimme
            if (_control.HasFlag(Control.Promiscuous)) return true;

            // Always accept L2 broadcasts, too
            if (dest.Equals(HostAdapter.Broadcast)) return true;

            // See if it's our hardware addr or current receive addr
            if (dest.Equals(_physAddr.PA)) return true;
            if (dest.Equals(_recvAddr.PA)) return true;

            // Finally, loop through the multicast bytes
            // (See Docs/Network.txt for more information!)
            var addr = dest.GetAddressBytes();
            if (addr[0] == 1 &&
                addr[1] == 0 &&
                addr[2] == 0 &&
                addr[3] == 0 &&
                addr[4] == 0 &&
                addr[5] != 0)
            {
                // No groups for you!
                if (MCB == 0xFF) return false;

                // Receive all?
                if (MCB == 0) return true;

                // Match any five specific groups
                if (addr[5] == _mcastGroups[1] ||
                    addr[5] == _mcastGroups[2] ||
                    addr[5] == _mcastGroups[3] ||
                    addr[5] == _mcastGroups[4] ||
                    addr[5] == _mcastGroups[5])
                    return true;
            }

            // Otherwise log the rejection
            Log.Info(Category.Ethernet, "Rejecting packet for {0}", dest);
            return false;
        }

        /// <summary>
        /// Are we in the mood to handle another packet?
        /// </summary>
        public bool CanReceive => _state == State.ReceiveWait;

        /// <summary>
        /// Handle the actual reception of an incoming packet.
        /// </summary>
        public void DoReceive(byte[] packet)
        {
            // We must be actively receiving!  There's an obvious race here, if
            // we don't make _state volatile or work out a MUCH saner way to have
            // the controller *pull* packets from the NIC when ready, not have it
            // push them at us.  Either strategy could lead to unsightly gaps or
            // delays when there's a single packet in the queue and we miss the
            // window between going into receive mode and the next packet arriving
            // to prod the queue.  This is mostly to make sure we don't have a
            // concurrency issue where the bit counter is getting overwritten!
            if (_state != State.ReceiveWait)
            {
                Log.Warn(Category.Ethernet, "DoReceive in state {0}, packet dropped", _state);
                return;
            }

            ushort data;

            // Update our state and status flag
            _state = State.Receiving;
            _status |= (Status.CarrierSense | Status.PacketInProgress);

            // Fetch the header and data buffer addresses from the DMAC
            var header = _system.IOB.DMARegisters.GetHeaderAddress(_dmaRx);
            var buffer = _system.IOB.DMARegisters.GetDataAddress(_dmaRx);

            Log.Info(Category.Ethernet, "Receiving {0} bytes to header @ 0x{1:x6}, data @ 0x{2:x6} [{3}]",
                                         packet.Length, header, buffer,
                                         System.Threading.Thread.CurrentThread.ManagedThreadId);
            Log.Debug(Category.Ethernet, "Receive bit count initial = {0:x} ({1})",
                                        _bitCount, (short)_bitCount);

            // Write the Ethernet frame's header to PERQ memory.  The header is
            // 14 bytes but the DMA always ships quad words, so the first word is
            // a dummy.  We write a zero, but could just skip it?
            _system.Memory.StoreWord(header++, 0);

            for (var i = 0; i < 6; i++)
            {
                data = (ushort)(packet[i * 2 + 1] << 8 | packet[i * 2]);
                _system.Memory.StoreWord(header++, data);
            }

            // Undo the length/type field swap that the software does
            data = (ushort)(packet[12] << 8 | packet[13]);
            _system.Memory.StoreWord(header, data);

            // DMA the packet data to the PERQ, reconstituted as 16-bit words
            for (var i = 14; i < packet.Length; i += 2)
            {
                data = packet[i];
                if (i + 1 < packet.Length) data |= (ushort)(packet[i + 1] << 8);
                _system.Memory.StoreWord(buffer++, data);
            }

            // Set the bit count as if the hardware had counted UP from the value
            // the microcode programmed; on receives the counter is intialized to
            // the 2's complement of 1518 (max frame size) because they use that
            // to detect giant packets!  Note also that the PERQ expects that the
            // 32-bit CRC is included in the bit count
            _bitCount += (ushort)(packet.Length * 8);

            // Compute delay for DMA copy and schedule the callback to complete;
            // include the "interpacket gap" so we can do back-to-back receives
            var delay = (ulong)((packet.Length * 8 * .1) + 9.6) * Conversion.UsecToNsec;
            _response = _system.Scheduler.Schedule(delay, ReceiveComplete);

            Log.Info(Category.Ethernet, "Received {0} bytes ({1} bits), callback in {2}usec",
                                          packet.Length, (short)_bitCount, delay / 1000);
        }

        /// <summary>
        /// Send a packet.  Handles the DMA op to create the new buffer that
        /// will be handed off to the HostInterface for transmission.
        /// </summary>
        void DoTransmit()
        {
            // The bit count is written as a negative value and counts up;  the
            // hardware automatically stops when it crosses zero.  POS takes the
            // two's complement in the microcode while Accent does it in Pascal
            // code that sets up the DCB.  To compute transmission delay, take
            // the absolute value...
            // Todo: Test this with older (pre-1983?) software that did it the
            // old way!?
            if ((short)_bitCount < 0) _bitCount = (ushort)(0 - _bitCount);

            // Sanity checks:  the microcode isn't supposed to start a new send
            // if the receiver is active, so a Reset should have been done first
            // to cancel the receive.  And we check the bit count to make sure
            // the value represents a legal packet length.  (The PERQ should do
            // these itself, so this might be removed after more testing.)
            if (_bitCount < 480 || _bitCount > 12144 || _state != State.Idle)
            {
                Log.Write(Category.Ethernet, "Transmit requested while {0} or bad bit count: {1}",
                                             _state, (short)_bitCount);

                // Uh, what to do?  There's no error provision in the spec!
                // For now, set _bitCount to zero so that TransmitComplete()
                // won't attempt to send a bad packet, finish processing the
                // send normally, and let the microcode reset us...
                _bitCount = 0;
            }

            // Set up for sending!
            _state = State.Transmitting;
            _status |= (Status.CarrierSense | Status.Busy);

            // Delay includes IPG and 32 bits of FCS
            var delay = (ulong)((_bitCount + 32) * .1 + 9.6) * Conversion.UsecToNsec;
            _response = _system.Scheduler.Schedule(delay, TransmitComplete);

            Log.Info(Category.Ethernet, "Transmitting {0} byte packet ({1} bits), callback in {2}usec",
                                         _bitCount / 8, (short)_bitCount, delay / 1000);
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
                // Buffer (in bytes) for the complete raw packet, including header
                // but NOT the FCS -- the hardware adds the CRC32 on send!  (In
                // this case, SharpPcap/host adapter adds it on our behalf)
                byte[] packet = new byte[(_bitCount / 8)];

                int addr;
                ushort data;

                // DMA the header buffer from the PERQ's memory.  The hardware
                // always transfers two quads for the header, but skips over the
                // first (unused) word.
                addr = _system.IOB.DMARegisters.GetHeaderAddress(_dmaTx) + 1;

                for (var i = 0; i < 6; i++)
                {
                    data = _system.Memory.FetchWord(addr++);
                    packet[i * 2] = (byte)data;
                    packet[i * 2 + 1] = (byte)(data >> 8);
                }

                // Do the header's Length/Type field (swapped!)
                data = _system.Memory.FetchWord(addr);
                packet[12] = (byte)(data >> 8);
                packet[13] = (byte)data;

                // Now copy the packet's payload from the buffer address
                addr = _system.IOB.DMARegisters.GetDataAddress(_dmaTx);

                for (var i = 14; i < packet.Length; i += 2)
                {
                    data = _system.Memory.FetchWord(addr++);
                    packet[i] = (byte)data;
                    if (i + 1 < packet.Length) packet[i + 1] = (byte)(data >> 8);
                }

                // Hand off the complete packet!
                var result = _nic.SendPacket(packet);

                // Todo: The only error we give back is "collision"?  So there
                // may not be any status to give, until/unless we handle events
                // from Pcap indicating a change at the host itself?

                // Set the bit counter to zero to indicate success
                _bitCount = 0;
            }

            // Complete the transmission!
            _status &= ~(Status.CarrierSense);
            _status |= Status.Complete;

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
            Log.Info(Category.Ethernet, "{0} complete, raising {1} interrupt", _state, _irq);

            _response = null;
            _state = State.Complete;
            _status &= ~Status.Busy;
            _netInterrupt = true;
            _system.CPU.RaiseInterrupt(_irq);
        }

        // Debugging
        public void DumpEther()
        {
            var headerAddress = _system.IOB.DMARegisters.GetHeaderAddress(_dmaRx);
            var bufferAddress = _system.IOB.DMARegisters.GetDataAddress(_dmaRx);

            Console.WriteLine($"{_system.Config.IOBoard} Ethernet status:");
            Console.WriteLine($"  My MAC address:    {_physAddr.ToPERQFormat()} ({_physAddr})");
            Console.WriteLine($"  Receive address:   {_recvAddr.ToPERQFormat()} ({_recvAddr})");
            Console.WriteLine($"  Control register:  {(int)_control:x} ({_control})");
            Console.WriteLine($"  Status register:   {(int)_status:x} ({_status})");
            Console.WriteLine("  Controller state:  {0}, scheduler callback {1} pending", _state,
                              (_response != null ? "IS" : "is NOT"));

            // IOB (OIO): One DMA for RX and TX
            Console.WriteLine($"  DMA addresses:     Header: 0x{headerAddress:x6}  " +
                              $"Buffer: 0x{bufferAddress:x6} ({_dmaRx})");

            // EIO: Separate RX, TX DMA channels
            if (_dmaRx != _dmaTx)
            {
                headerAddress = _system.IOB.DMARegisters.GetHeaderAddress(_dmaTx);
                bufferAddress = _system.IOB.DMARegisters.GetDataAddress(_dmaTx);

                Console.WriteLine($"  DMA addresses:     Header: 0x{headerAddress:x6}  " +
                                  $"Buffer: 0x{bufferAddress:x6} ({_dmaTx})");
            }

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
            ReceiveWait,
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
        ChannelName _dmaTx;
        ChannelName _dmaRx;

        MachineAddress _physAddr;
        MachineAddress _recvAddr;

        byte[] _mcastGroups;

        bool _netInterrupt;
        bool _clockInterrupt;

        ushort _bitCount;
        ushort _usecClock;

        static Random _random = new Random();

        HostAdapter _nic;

        SchedulerEvent _response;
        SchedulerEvent _timer;
        PERQSystem _system;
    }
}
