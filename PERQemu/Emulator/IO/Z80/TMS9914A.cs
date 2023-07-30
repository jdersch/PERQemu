//
// TMS9914A.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

using PERQemu.IO.GPIB;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Implements as much of the TMS9914A GPIB controller as the PERQ needs.
    /// While the software support is fairly complete, the primary use is to
    /// connect the BitPad pointing device.
    /// </summary>
    /// <remarks>
    /// Based on the original simulated GPIB, we'll provide basic talker and
    /// listener selection, data transfers, and try to hook up the Z80 DMA too.
    /// The fun part is figuring out how many status bits to spoof to keep the
    /// Z80 and PERQ convinced of the illusion that there's a real controller
    /// here doing all the crazy convoluted GPIB stuff behind the scenes.  Whew.
    /// </remarks>
    public class TMS9914A : IGPIBDevice, IZ80Device, IDMADevice
    {
        public TMS9914A(byte baseAddr)
        {
            _baseAddress = baseAddr;
            _ports = new byte[8];

            for (int i = 0; i < 8; i++)
                _ports[i] = (byte)(baseAddr + i);

            _rdRegisters = new byte[8];
            _wrRegisters = new byte[8];

            _interruptActive = false;
            _interruptsEnabled = true;

            _bus = new GPIBBus();
            _busFifo = new Queue<ushort>();

            // Put ourselves on the bus!
            _bus.AddDevice(this);
        }

        public byte DeviceID => 0;      // System Controller

        /// <summary>
        /// Hardware reset (power on or Z80 restart).
        /// </summary>
        public void Reset()
        {
            // Internal state
            ResetGPIB();

            // Bus devices
            _bus.Reset();
            _busFifo.Clear();

            Log.Debug(Category.GPIB, "Controller reset");
        }

        public string Name => "TMS9914A";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x22;    // GPIVEC
        public bool IntLineIsActive => _interruptActive;

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        public GPIBBus Bus => _bus;

        //
        // IDMADevice Interface
        //

        public bool ReadDataReady
        {
            get
            {
                throw new NotImplementedException("TMS9914A DMA read");
            }
        }

        public bool WriteDataReady
        {
            get
            {
                throw new NotImplementedException("TMS9914A DMA write");
            }
        }

        public void DMATerminate()
        {
            throw new NotImplementedException("TMS9914A DMA terminate");
        }

        //
        // IZ80Device Interface
        //

        void AssertInterrupt()
        {
            var oldIntr = _interruptActive;

            // Int0 and Int1 in the Interrupt Status 0 register are not storage
            // bits; they are asserted only when the following conditions are met:
            //      any unmasked bit in InterruptStatus1 is set, and/or
            //      any unmasked bit 2:7 in InterruptStatus0 is set, and
            //      the DisableAllInterrupts bit is not set

            var int0 = (((_rdRegisters[(int)ReadRegister.IntStatus0] &
                          _wrRegisters[(int)WriteRegister.IntMask0]) & 0x3f) != 0);

            var int1 = ((_rdRegisters[(int)ReadRegister.IntStatus1] &
                         _wrRegisters[(int)WriteRegister.IntMask1]) != 0);

            _interruptActive = ((int0 || int1) && _interruptsEnabled);

            if (_interruptActive != oldIntr)
                Log.Debug(Category.GPIB, "Interrupt {0}", _interruptActive ? "asserted" : "cleared");
        }

        public byte Read(byte portAddress)
        {
            var reg = portAddress - _baseAddress;
            byte retval = 0;

            switch ((ReadRegister)reg)
            {
                case ReadRegister.IntStatus0:
                    // To read the Interrupt Status 0 register, the INT0 and INT1
                    // bits have to be computed (as above).  But then we save the
                    // current status, turn OFF both BI and BO, and return the
                    // saved value.
                    bool int0 = (((_rdRegisters[(int)ReadRegister.IntStatus0] &
                                   _wrRegisters[(int)WriteRegister.IntMask0]) & 0x3f) != 0);

                    bool int1 = ((_rdRegisters[(int)ReadRegister.IntStatus1] &
                                  _wrRegisters[(int)WriteRegister.IntMask1]) != 0);

                    retval = (byte)((_rdRegisters[reg] & 0x3f) |
                                    (int0 ? (byte)InterruptStatus0.Int0 : 0x0) |
                                    (int1 ? (byte)InterruptStatus0.Int1 : 0x0));

                    // A read from the Interrupt Status 0 reg clears _both_ BI & BO!
                    _rdRegisters[(int)ReadRegister.IntStatus0] &= (byte)~InterruptStatus0.BO;
                    _rdRegisters[(int)ReadRegister.IntStatus0] &= (byte)~InterruptStatus0.BI;

                    Log.Debug(Category.GPIB, "Read 0x{0:x2} ({1}) from register {2}",
                                              retval, (InterruptStatus0)retval, reg);
                    AssertInterrupt();
                    return retval;

                case ReadRegister.AddressSwitch:
                case ReadRegister.AddressStatus:
                case ReadRegister.IntStatus1:
                case ReadRegister.BusStatus:
                    Log.Debug(Category.GPIB, "Read 0x{0:x2} from register {1}", _rdRegisters[reg], reg);
                    return _rdRegisters[reg];

                case ReadRegister.CmdPassThrough:
                    // This just reads the data lines...
                    retval = (byte)(_busFifo.Count > 0 ? (_busFifo.Peek() >> 8) : 0x0);
                    Log.Debug(Category.GPIB, "Read 0x{0:x2} from register {1}", retval, reg);
                    return retval;

                case ReadRegister.DataIn:
                    // Retrieve the latest data byte from the bus
                    ushort word = 0;

                    // Should only get here on a BI interrupt...
                    if (_busFifo.Count > 0)
                    {
                        // Get the current byte
                        word = _busFifo.Dequeue();
                        retval = (byte)(word >> 8);
                    }
                    else
                    {
                        Log.Warn(Category.GPIB, "DataIn read from empty register, returning 0");
                    }

                    // Now set up the flags for the next one (if any).  We do this
                    // here since the END interrupt (if unmasked) has to fire when
                    // BI is true prior to the byte being read!  (Without our FIFO
                    // to handwave over all the actual bus protocol, BI/END would
                    // get set by BusRead and just be cleared here.)
                    if (_busFifo.Count > 0)
                    {
                        word = _busFifo.Peek();
                        var flags = (BusStatus)(word & 0xff);

                        // Re-raise BI since there's another byte waiting
                        _rdRegisters[(int)ReadRegister.IntStatus0] |= (byte)InterruptStatus0.BI;

                        // And set the END bit on the NEXT byte if EOI set
                        if (flags.HasFlag(BusStatus.EOI))
                        {
                            _rdRegisters[(int)ReadRegister.IntStatus0] |= (byte)InterruptStatus0.END;
                        }

                        Log.Detail(Category.GPIB, "EOI on next byte {0}", flags.HasFlag(BusStatus.EOI));
                    }
                    else
                    {
                        // That was the last byte
                        _rdRegisters[(int)ReadRegister.IntStatus0] &= (byte)~(InterruptStatus0.BI | InterruptStatus0.END);
                    }

                    Log.Detail(Category.GPIB, "DataIn read 0x{0:x2} ({1} bytes remaining)", retval, _busFifo.Count);
                    AssertInterrupt();
                    return retval;

                default:
                    throw new InvalidOperationException("Invalid port address on read");
            }
        }

        public void Write(byte portAddress, byte value)
        {
            var reg = (WriteRegister)(portAddress - _baseAddress);
            _wrRegisters[(byte)reg] = value;

            switch (reg)
            {
                case WriteRegister.IntMask0:
                    Log.Debug(Category.GPIB, "{0} set to {1} ({2:x2})", reg, (InterruptStatus0)value, value);
                    break;

                case WriteRegister.IntMask1:
                    Log.Debug(Category.GPIB, "{0} set to {1} ({2:x2})", reg, (InterruptStatus1)value, value);
                    break;

                case WriteRegister.AddressRegister:
                case WriteRegister.ParallelPoll:
                case WriteRegister.SerialPoll:
                    // Just note it for now, not sure what to do with these...
                    Log.Debug(Category.GPIB, "Wrote 0x{0:x2} to register {1}", value, reg);
                    break;

                case WriteRegister.AuxiliaryCmd:
                    var cmd = (AuxiliaryCommand)(value & 0x1f);
                    bool cs = (value & 0x80) != 0;

                    DispatchAuxiliaryCommand(cmd, cs);
                    break;

                case WriteRegister.DataOut:
                    // BO bit is cleared by a write to DataOut
                    _rdRegisters[(int)ReadRegister.IntStatus0] &= (byte)~InterruptStatus0.BO;
                    AssertInterrupt();

                    if (_standby && _iTalk)
                    {
                        var flags = BusStatus.DAV;

                        if (_sendNextEOI)
                        {
                            flags |= BusStatus.EOI;
                            _sendNextEOI = false;
                        }

                        Log.Detail(Category.GPIB, "DataOut wrote 0x{0:x2} to device {1}", value, DeviceID);
                        _bus.BusWrite(DeviceID, value, flags);
                    }
                    else
                    {
                        DispatchGroupCommand(value);
                    }

                    // But since we don't have to wait around for a bunch of actual
                    // bus protocol to happen, just set BO now to indicate we're
                    // ready for another transfer.  Then we'll read IntStatus0 to
                    // dismiss the BO interrupt, like, y'know, "Got it.  We cool."
                    // We _could_ schedule some synthetic delay here to simulate a
                    // transmit delay.  But this is already Very Very Silly.
                    _rdRegisters[(int)ReadRegister.IntStatus0] |= (byte)InterruptStatus0.BO;
                    AssertInterrupt();
                    break;

                default:
                    throw new InvalidOperationException("Invalid port address on write");
            }
        }

        //
        // IGPIBDevice Interface
        //
        // Note: most of this is ignored, since we're the System Controller.
        // It just gives us a convenient hook for receiving tablet data like
        // a normal GPIB device.  GPIB is deeply weird.
        //

        public void BusReset()
        {
        }

        public void SetTalker(byte address)
        {
        }

        public void SetListener(byte address)
        {
        }

        public void RegisterBusWriteDelegate(BusWriteDelegate writeDelegate)
        {
        }

        /// <summary>
        /// Receives a byte from the Talker when we're in Listener (standby)
        /// mode.  Queues it up and sets BI so the Z80 will read it.
        /// </summary>
        public void BusRead(byte value, BusStatus flags)
        {
            // If the controller has stopped listening to the bus, drop the data
            // on the floor.  OSes running the "old Z80" turn off the tablet by
            // sending an 'untalk' to the tablet it so it stops transmitting.
            // But POS G (and others?) take a shortcut: they just turn off GPIB
            // interrupts.  Presumably the BitPad keeps yakking but the 9914
            // ignores the data?  Or maybe the "holdoff"/handshaking stuff in
            // the protocol clues in the tablet that it's being ignored?  In any
            // case, we drop the bytes so the FIFO doesn't grow and grow...
            if (!_iListen)
            {
                Log.Debug(Category.GPIB, "Bus read but controller not listening! (Dropped byte 0x{0:x2})", value);
                return;
            }

            // NB: I just push the status flags into the high byte of a 16-bit
            // word, to avoid using a struct or class for this.  Because we
            // don't want to do a bloody full-blown state machine to deal with
            // handshaking on every byte, with all of the complexity and extra
            // performance hit that entails, our GPIB appears to have a deep
            // FIFO and can always buffer data coming from the bus while the
            // Z80 does its little dance to read bytes into its circular buffer
            // and ultimately feed them to the PERQ, which hands them from the
            // microcode to Pascal.  Woof.
            _busFifo.Enqueue((ushort)((value << 8) + (byte)flags));

            // Set BI!
            _rdRegisters[(int)ReadRegister.IntStatus0] |= (byte)InterruptStatus0.BI;

            // Set an END interrupt too?
            if (_busFifo.Count == 1 && flags.HasFlag(BusStatus.EOI))
            {
                // If this is our first byte and EOI is set, we need to assert
                // the END bit too (the spec says END and BI get set at the same
                // time).  Reading from DataIn resets them.
                _rdRegisters[(int)ReadRegister.IntStatus0] |= (byte)InterruptStatus0.END;
            }

            AssertInterrupt();
        }

        //
        // GPIB Processing
        //

        void ResetGPIB()
        {
            // todo: determine if this is anywhere close to correct :-/
            _iListen = false;
            _iTalk = false;
            _listener = 0x1f;   // nobody
            _talker = 0x1f;     // nobody

            // Both interrupt bits are "held at 0 while swrst is set"
            // so _swrst just needs to be a state... or just set the
            // _interruptsEnabled flag like dai (which is unused anyway?) does
            _rdRegisters[(int)ReadRegister.IntStatus0] &= (byte)~InterruptStatus0.Int0;
            _rdRegisters[(int)ReadRegister.IntStatus0] &= (byte)~InterruptStatus0.Int1;
        }

        /// <summary>
        /// Process GPIB Auxiliary Commands - a small subset the PERQ
        /// uses - to do resets, standby, or set talker/listener flags.
        /// </summary>
        void DispatchAuxiliaryCommand(AuxiliaryCommand cmd, bool cs)
        {
            Log.Debug(Category.GPIB, "Auxiliary Command is {0}, cs {1}", cmd, cs);

            switch (cmd)
            {
                case AuxiliaryCommand.swrst:
                    if (cs)
                    {
                        ResetGPIB();
                    }
                    break;

                case AuxiliaryCommand.sic:
                    if (cs)
                    {
                        Log.Detail(Category.GPIB, "Setting IFC");
                        _rdRegisters[(int)ReadRegister.BusStatus] |= (byte)BusStatus.IFC;
                    }
                    else
                    {
                        Log.Detail(Category.GPIB, "IFC reset");
                        _rdRegisters[(int)ReadRegister.BusStatus] &= (byte)~BusStatus.IFC;
                    }
                    break;

                case AuxiliaryCommand.tcs:
                case AuxiliaryCommand.tca:
                    if (cs)
                    {
                        Log.Detail(Category.GPIB, "Setting ATN");
                        _rdRegisters[(int)ReadRegister.BusStatus] |= (byte)BusStatus.ATN;
                    }
                    else
                    {
                        Log.Detail(Category.GPIB, "ATN reset");
                        _rdRegisters[(int)ReadRegister.BusStatus] &= (byte)~BusStatus.ATN;
                    }
                    break;

                case AuxiliaryCommand.sre:
                    if (cs)
                    {
                        Log.Detail(Category.GPIB, "Setting REN");
                        _rdRegisters[(int)ReadRegister.BusStatus] |= (byte)BusStatus.REN;
                    }
                    else
                    {
                        Log.Detail(Category.GPIB, "REN reset");
                        _rdRegisters[(int)ReadRegister.BusStatus] &= (byte)~BusStatus.REN;
                    }
                    break;

                case AuxiliaryCommand.feoi:
                    Log.Debug(Category.GPIB, "Send next byte with EOI {0}", cs);
                    _sendNextEOI = cs;
                    break;

                case AuxiliaryCommand.lon:
                    _iListen = cs;
                    _bus.ControllerIsListener(cs);
                    break;

                case AuxiliaryCommand.ton:
                    _iTalk = cs;
                    break;

                case AuxiliaryCommand.gts:
                    _standby = cs;
                    break;

                case AuxiliaryCommand.dai:
                    _interruptsEnabled = cs;
                    break;

                case AuxiliaryCommand.rhdf:    // Release RFD holdoff
                case AuxiliaryCommand.hdfa:    // Holdoff on all data
                case AuxiliaryCommand.hdfe:    // Holdoff on EOI only
                    if (cs)
                    {
                        Log.Warn(Category.GPIB, "Unimplemented Aux Command {0} with cs {1}", cmd, cs);
                    }
                    // If turning off something we never turned on, ignore it! :-)
                    break;

                default:
                    // Log this in case we missed something important...
                    Log.Warn(Category.GPIB, "Unhandled Auxiliary Command is {0}, cs {1}", cmd, cs);
                    break;
            }

            // Check if a change above affected our interrupt status
            AssertInterrupt();
        }

        /// <summary>
        /// Interprets command data bytes which contain group commands (including
        /// the setting of talker/listener addresses).  These are "broadcast" to
        /// all devices on the bus.
        /// </summary>
        void DispatchGroupCommand(byte value)
        {
            var grp = (RemoteCommandGroup)((value & 0x60) >> 5);
            var data = (byte)(value & 0x1f);

            Log.Debug(Category.GPIB, "Remote command group is {0}, data 0x{1:x2}", grp, data);

            switch (grp)
            {
                case RemoteCommandGroup.AddressedCommandGroup:
                    //
                    // Nothing to do, since I'm not sure which (if any) of these
                    // ever get used by the PERQ, or even what they do.  But log
                    // whatever comes in for debugging.
                    //
                    switch ((AddressCommands)data)
                    {
                        case AddressCommands.dcl:
                        case AddressCommands.gtl:
                        case AddressCommands.gxt:
                        case AddressCommands.llo:
                        case AddressCommands.ppc:
                        case AddressCommands.ppu:
                        case AddressCommands.sdc:
                        case AddressCommands.spd:
                        case AddressCommands.spe:
                        case AddressCommands.tct:
                            Log.Debug(Category.GPIB, "Addressed Command received {0}", data);
                            break;

                        default:
                            Log.Debug(Category.GPIB, "Unknown Addressed Command received {0}", data);
                            break;
                    }
                    break;

                case RemoteCommandGroup.ListenAddressGroup:
                    //
                    // Set the listener address
                    //
                    if (data == 0x1f)
                    {
                        Log.Debug(Category.GPIB, "Listen Address Group 'unlisten' command received");
                        _iListen = false;    // Unlisten the controller (as with lon=false?)
                    }
                    else
                    {
                        Log.Debug(Category.GPIB, "Listen Address set to {0}", data);
                        _listener = data;
                    }

                    _bus.BroadcastListener(data);
                    break;

                case RemoteCommandGroup.TalkAddressGroup:
                    //
                    // Set the talker address
                    //
                    if (data == 0x1f)
                    {
                        Log.Debug(Category.GPIB, "Talker Address Group 'untalk' command received");
                        _iTalk = false;    // Untalk the controller too (same as ton=false?)
                    }
                    else
                    {
                        Log.Debug(Category.GPIB, "Talker Address set to {0}", data);
                        _talker = data;
                    }

                    _bus.BroadcastTalker(data);
                    break;

                case RemoteCommandGroup.SecondaryCommandGroup:
                    //
                    // Nothing to do with these either, since we don't care about secondary listeners
                    // or parallel polling...?  Log it in case something comes our way.
                    //
                    Log.Debug(Category.GPIB, "Secondary Command/Address {0} received (ignored)", data);
                    break;
            }
        }

        enum ReadRegister
        {
            IntStatus0 = 0,         // GPIIS0
            AddressSwitch = 1,      // GPIASW
            AddressStatus = 2,      // GPIAS
            CmdPassThrough = 3,     // GPICPT
            IntStatus1 = 4,         // GPIIS1
            BusStatus = 6,          // GPIBS
            DataIn = 7              // GPIIN
        }

        enum WriteRegister
        {
            IntMask0 = 0,           // GPIIM0
            AddressRegister = 1,    // GPIADD
            ParallelPoll = 3,       // GPIPP
            IntMask1 = 4,           // GPIIM1
            SerialPoll = 5,         // GPISP
            AuxiliaryCmd = 6,       // GPIAUX
            DataOut = 7             // GPIOUT
        }

        //
        //  NB: All TMS9914 registers are in big-endian BIT format!
        //          D0 (MSB) .. D7 (LSB)
        //

        /// <summary>
        /// Interrupt status (and mask) register 0 bits.
        /// </summary>
        [Flags]
        enum InterruptStatus0 : byte
        {
            Int0 = 0x80,
            Int1 = 0x40,
            BI = 0x20,
            BO = 0x10,
            END = 0x08,
            SPAS = 0x04,
            RLC = 0x02,
            MAC = 0x01
        }

        /// <summary>
        /// Interrupt status (and mask) register 1 bits.
        /// </summary>
        [Flags]
        enum InterruptStatus1 : byte
        {
            GET = 0x80,
            ERR = 0x40,
            UNC = 0x20,
            APT = 0x10,
            DCAS = 0x08,
            MA = 0x04,
            SRQ = 0x02,
            IFC = 0x01
        }

        /// <summary>
        /// GPIB is insane.  RMMC is short for Remote Multiple Message Coding.
        /// This is how to pick commands out of the data bytes sent following
        /// an auxiliary command.  Bit 8 is always DontCare (masked off); bits
        /// 7..6 select a command group; bits 5..1 select a specific action or
        /// setting within the group, but sometimes the definitions overlap and
        /// make no sense (secondary address vs. parallel poll enable/disable)?
        /// Madness.  But we have to pick this apart to watch for our talk and
        /// listen addresses, which is how the PERQ turns the BitPadOne on or
        /// off!  Oy vey.
        /// </summary>
        enum RemoteCommandGroup
        {
            AddressedCommandGroup = 0x0,
            ListenAddressGroup = 0x1,
            TalkAddressGroup = 0x2,
            SecondaryCommandGroup = 0x3
        }

        /// <summary>
        /// Here are the lower 5 bits for selecting various commands (which we
        /// mostly ignore) or setting the talker/listener addresses (which we
        /// care about).
        /// </summary>
        enum AddressCommands
        {
            gtl = 0x01,     // Go to local
            sdc = 0x04,     // Selected device clear
            ppc = 0x05,     // Parallel poll configure
            gxt = 0x08,     // Group execute trigger ("get" conflicts w/reserved word)
            tct = 0x09,     // Take control
            llo = 0x10,     // Local lock out
            dcl = 0x14,     // Device clear
            ppu = 0x15,     // Parallel poll unconfigure
            spe = 0x18,     // Serial poll enable
            spd = 0x19      // Serial poll disable
        }

        /// <summary>
        /// These enumerations come straight from the TMS9941A datasheet.
        /// </summary>
        enum AuxiliaryCommand
        {
            swrst = 0x00,   // Software Reset
            dacr = 0x01,    // Release DAC holdoff
            rhdf = 0x02,    // Release RFD holdoff
            hdfa = 0x03,    // Holdoff on all data
            hdfe = 0x04,    // Holdoff on EOI only
            nbaf = 0x05,    // New byte available false
            fget = 0x06,    // Force group execute trigger
            rtl = 0x07,     // Return to local
            feoi = 0x08,    // Send EOI with next byte
            lon = 0x09,     // Listen only
            ton = 0x0a,     // Talk only
            gts = 0x0b,     // Go to standby
            tca = 0x0c,     // Take control asynchronously
            tcs = 0x0d,     // Take control synchronously
            rpp = 0x0e,     // Request parallel poll
            sic = 0x0f,     // Send interface clear
            sre = 0x10,     // Send remote enable
            rqc = 0x11,     // Request control
            rlc = 0x12,     // Release control
            dai = 0x13,     // Disable all interrupts
            pts = 0x14,     // Pass through next secondary
            stdl = 0x15,    // Short T1 setting time
            shdw = 0x16,    // Shadow handshake
            vstdl = 0x17,   // Very short T1 delay
            rsv2 = 0x18,    // Request Bit 2
        }


        // Local state
        bool _standby;
        bool _iListen;
        bool _iTalk;

        byte _listener;
        byte _talker;

        bool _sendNextEOI;

        // Interrupts
        bool _interruptsEnabled;
        bool _interruptActive;

        // Registers
        byte _baseAddress;
        byte[] _ports;

        byte[] _rdRegisters;
        byte[] _wrRegisters;

        // The Bus
        GPIBBus _bus;
        Queue<ushort> _busFifo;
    }
}
