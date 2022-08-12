//
// Z80SIOChannel.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.IO.Ports;
using System.Collections.Generic;

using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    public partial class Z80SIO : IZ80Device, IDMADevice
    {
        /// <summary>
        /// One Z80 SIO Channel.  They're not _quite_ identical: only channel 1
        /// ("SIO B") holds the interrupt vector in its WR2/RR2 registers!
        /// </summary>
        internal class Channel
        {
            public Channel(int channelNumber, Scheduler scheduler)
            {
                _channelNumber = channelNumber;
                _scheduler = scheduler;

                _writeRegs = new byte[8];
                _readRegs = new byte[3];

                _rxFifo = new Queue<byte>();
                _txFifo = new Queue<byte>();

                _device = null;
                _port = null;
            }

            /// <summary>
            /// Reset this instance and any attached device (hard reset).
            /// </summary>
            public void Reset()
            {
                for (var i = 0; i < _writeRegs.Length; i++)
                    _writeRegs[i] = 0;

                for (var i = 0; i < _readRegs.Length; i++)
                    _readRegs[i] = 0;

                _rxFifo.Clear();
                _txFifo.Clear();

                _selectedRegister = 0;
                _huntMode = true;               // re-entered after Reset

                _extInterruptLatched = false;
                _txInterruptLatched = false;
                _rxInterruptLatched = false;
                _rxIntOnNextCharacter = false;
                _breakDetected = false;

                _device?.Reset();

                UpdateFlags();

                Log.Debug(Category.SIO, "Channel {0} reset", _channelNumber);
            }

            public bool InterruptLatched => _rxInterruptLatched || _txInterruptLatched || _extInterruptLatched;
            public bool StatusAffectsVector => (_writeRegs[1] & (byte)WReg1.StatusAffectsVector) != 0;
            public byte InterruptBase => _readRegs[2];      // Valid for Channel B only...
            public int InterruptOffset => _interruptOffset;

            // Debugging
            public ISerialDevice Port => _port;

            private bool RxEnabled => (_writeRegs[3] & (byte)WReg3.RxEnable) != 0;
            private bool TxEnabled => (_writeRegs[5] & (byte)WReg5.TxEnable) != 0;

            private WReg4Bits Bits => (WReg4Bits)((_writeRegs[4] & 0xc) >> 2);
            private bool SyncMode => Bits == WReg4Bits.SyncModesEnable;

            private WReg1IntEnables IntFlags => (WReg1IntEnables)((_writeRegs[1] & 0x18) >> 3);
            private bool IntOnFirstRxCharacter => IntFlags == WReg1IntEnables.RxIntOnFirstChar;
            private bool IntOnAllRxCharacters => IntFlags == WReg1IntEnables.RxIntOnAllParityAffectsVector ||
                                                 IntFlags == WReg1IntEnables.RxIntOnAllParityNotAffectsVector;

            private bool RxInterruptEnabled => (IntOnFirstRxCharacter && _rxIntOnNextCharacter) || IntOnAllRxCharacters;
            private bool TxInterruptEnabled => (_writeRegs[1] & (byte)WReg1.TxIntEnable) != 0;
            private bool ExtInterruptEnabled => (_writeRegs[1] & (byte)WReg1.ExtIntEnable) != 0;


            public void AttachDevice(ISIODevice device)
            {
                _device = device;

                _device.RegisterReceiveDelegate(ReceiveData);
            }

            public void DetachDevice()
            {
                ClosePort();
                _port = null;
                _device = null;
            }

            public void OpenPort(ISerialDevice port)
            {
                _port = port;
                _port.Open();
            }

            public void ClosePort()
            {
                if (_port != null)
                {
                    _port.Close();
                }
            }

            public byte ReadRegister()
            {
                if (_selectedRegister > _readRegs.Length)
                {
                    Log.Debug(Category.SIO, "Channel {0} read from invalid register {2}",
                                            _channelNumber, _selectedRegister);
                    return 0;
                }

                byte value = _readRegs[_selectedRegister];
                Log.Debug(Category.SIO, "Channel {0} read 0x{1:x2} from register {2}",
                                        _channelNumber, value, _selectedRegister);
                return value;
            }


            public byte ReadData()
            {
                byte data = 0;

                if (_rxFifo.Count == 0)
                {
                    Log.Warn(Category.SIO, "Channel {0} read from empty FIFO", _channelNumber);
                    return data;
                }

                data = _rxFifo.Dequeue();

                // Update interrupt status
                _rxIntOnNextCharacter = false;
                _rxInterruptLatched = (_rxFifo.Count > 0 && RxInterruptEnabled);

                UpdateFlags();

                Log.Debug(Category.SIO, "Channel {0} read data 0x{1:x2}, {2} remaining",
                                            _channelNumber, data, _rxFifo.Count);
                return data;
            }


            public void WriteRegister(byte value)
            {
                Log.Debug(Category.SIO, "Channel {0} write 0x{1:x2} to register {2}",
                                        _channelNumber, value, _selectedRegister);

                _writeRegs[_selectedRegister] = value;

                if (_selectedRegister == 0)
                {
                    //
                    // Write to WR0
                    //
                    // TODO: handle CRC resets

                    // Execute command:
                    WReg0Cmd cmd = (WReg0Cmd)((value & 0x38) >> 3);

                    Log.Debug(Category.SIO, "Channel {0} command is {1}", _channelNumber, cmd);

                    switch (cmd)
                    {
                        case WReg0Cmd.NullCode:
                            break;

                        case WReg0Cmd.ResetExtStatusInterrupts:
                            _extInterruptLatched = false;
                            break;

                        case WReg0Cmd.ResetTxInt:
                            _txInterruptLatched = false;
                            break;

                        case WReg0Cmd.ChannelReset:
                            // Whack everything...
                            Reset();
                            break;

                        case WReg0Cmd.ErrorReset:
                            // Clear everything but AllSent
                            _readRegs[1] &= (byte)~RReg1.AllSent;
                            break;

                        case WReg0Cmd.EnableIntOnRx:
                            _rxIntOnNextCharacter = IntOnFirstRxCharacter;
                            break;

                        //case WReg0Cmd.ReturnFromInt:
                        //case WReg0Cmd.SendAbort:
                        default:
                            throw new NotImplementedException($"SIO command {cmd}");
                    }

                    // Select register pointer
                    _selectedRegister = (value & 0x7);

                    Log.Detail(Category.SIO, "Channel {0} register pointer now {1}",
                                             _channelNumber, _selectedRegister);

                    UpdateFlags();
                }
                else
                {
                    // Handle special cases for bits which trigger actions when set:
                    switch (_selectedRegister)
                    {
                        case 1:
                            // Bits D4..D0 (various irq enables) referenced elsewhere;
                            // The WAIT/READY functons may affect DMA operation?
                            break;

                        case 2:
                            if (_channelNumber == 1)
                            {
                                // Copy interrupt vector into RR2 (Channel B only!)
                                _readRegs[2] = _writeRegs[2];
                            }
                            break;

                        case 3:
                            if ((_writeRegs[3] & (byte)WReg3.EnterHuntPhase) != 0)
                            {
                                // Re-enter HUNT mode
                                _huntMode = true;
                                Log.Detail(Category.SIO, "Entering hunt mode");
                            }

                            UpdateBitsPerChar((_writeRegs[3] & (byte)WReg3.RxBitsPerChar) >> 6);
                            break;

                        case 4:
                            // Clock mode and sync mode don't require action
                            UpdateParity();
                            UpdateStopBits();
                            break;

                        case 5:
                            // Enables and CRC mode don't require action
                            UpdateBitsPerChar((_writeRegs[5] & (byte)WReg5.TxBitsPerChar) >> 5);
                            UpdateOutputPins();

                            if ((_writeRegs[5] & (byte)WReg5.SendBreak) != 0)
                            {
                                _device.TransmitBreak();
                            }
                            break;
                    }
                    // Write to other register, next access is to reg 0
                    _selectedRegister = 0;

                    Log.Detail(Category.SIO, "Channel {0} status: rxEnabled {1}, txEnabled {2}, syncMode {3}, hunting {4}",
                                             _channelNumber, RxEnabled, TxEnabled, SyncMode, _huntMode);
                }
            }

            /// <summary>
            /// Accept a byte from the SIO and schedule it for transmission.
            /// </summary>
            public void WriteData(byte data)
            {
                if (TxEnabled)
                {
                    _txFifo.Enqueue(data);
                    _txInterruptLatched = false;

                    UpdateFlags();

                    // fixme: proper baud rate delay here!
                    _scheduler.Schedule(Conversion.BaudRateToNsec(9600), SendData, null);
                }

                Log.Debug(Category.SIO, "Channel {0} write data 0x{1:x2}, {2} pending",
                                        _channelNumber, data, _txFifo.Count);
            }

            /// <summary>
            /// Sends the the next byte on the transmit queue to the device.
            /// </summary>
            private void SendData(ulong skewNsec, object context)
            {
                if (_txFifo.Count > 0)
                {
                    var b = _txFifo.Dequeue();

                    _device.Transmit(b);

                    Log.Detail(Category.SIO, "Channel {0} Tx data: 0x{1:x2}, queue depth {2}",
                                             _channelNumber, b, _txFifo.Count);

                    // If the buffer just became empty, raise the Tx interrupt (if enabled)
                    if (_txFifo.Count == 0)
                    {
                        _txInterruptLatched = TxInterruptEnabled;
                    }
                }
            }

            /// <summary>
            /// Receive data from the device.  Invoked by the attached ISIODevice.
            /// </summary>
            private void ReceiveData(byte data)
            {
                if (RxEnabled)
                {
                    if (SyncMode)
                    {
                        if (_huntMode)          // Looking for sync byte(s)
                        {
                            if (data == _writeRegs[7])  // 8-bit sync value
                            {
                                Log.Detail(Category.SIO, "Channel {0} sync word matched", _channelNumber);
                                _huntMode = false;      // Exit hunt mode
                            }

                            return;
                        }

                        // Sync byte was received, so we're in data mode; fall through...
                    }

                    // Async receive, or Sync mode (not in hunt mode)
                    _rxFifo.Enqueue(data);
                    _rxInterruptLatched = RxInterruptEnabled;

                    UpdateFlags();

                    Log.Detail(Category.SIO, "Channel {0} Rx data: 0x{1:x2}, queue depth {2}",
                                             _channelNumber, data, _rxFifo.Count);
                }

                // If not enabled, squawk about spurious data received??
            }

            /// <summary>
            /// Invoked by an attached ISerialDevice when it has data and/or extra
            /// status information to send.  Updates error bits in RR1.
            /// </summary>
            private void ReceiveStatusData(byte data, CharStatus status)
            {
                // See what happened...
                if (status != CharStatus.None)
                {
                    if ((status & CharStatus.PinChange) != 0)
                    {
                        // If ext int enable (WR1) then latch it; UpdateFlags
                        // will update RR0 and trigger the interrupt
                        _extInterruptLatched = ExtInterruptEnabled;

                        Log.Debug(Category.SIO, "Channel {0} pin change received!", _channelNumber);
                    }

                    // Latch error bits in RR1
                    if ((status & CharStatus.ParityError) != 0)
                    {
                        _readRegs[1] |= (byte)RReg1.ParityError;
                    }

                    if ((status & CharStatus.FramingError) != 0)
                    {
                        _readRegs[1] |= (byte)RReg1.CrcFraming;
                    }

                    if ((status & CharStatus.BreakDetected) != 0)
                    {
                        // "The Break/Abort bit is not used in the Synchronous Receive mode."
                        _breakDetected = !SyncMode;
                    }
                }

                // If the character is valid, process it normally
                if ((status & CharStatus.InvalidChar) != 0)
                {
                    ReceiveData(data);
                }
                else
                {
                    // Just update RR0
                    UpdateFlags();
                }
            }


            /// <summary>
            /// Update the read registers and interrupt status.
            /// </summary>
            private void UpdateFlags()
            {
                // Save the state of the pins before we update 'em
                RReg0 oldPinState = (RReg0)_readRegs[0];

                // Set RX-related flags in RR0
                _readRegs[0] = (byte)((_rxFifo.Count > 0 ? RReg0.RxCharAvailable : 0) |
                                      (_txFifo.Count == 0 ? RReg0.TxBufferEmpty : 0) |
                                      (_huntMode ? 0x0 : RReg0.SyncHunt));

                // Physical port? Update modem control pins, even though this is
                // largely symbolic given the likely disparity between the host's
                // physical port and our virtual machine's actual execution rate...
                if (_port != null)
                {
                    _readRegs[0] |= (byte)(_port.DCD ? RReg0.DCDState : 0);
                    _readRegs[0] |= (byte)(_port.CTS ? RReg0.CTSState : 0);
                    _readRegs[0] |= (byte)(_breakDetected ? RReg0.BreakAbort : 0);
                }

                // If no interrupts pending, the Z80 returns V3..V1 = 011.  So
                // regardless of whichever channel we are, set up the default...
                // ... except I don't think it works; the Z80 ROM doesn't seem to
                // handle that default case.  Let's set it to zero and see if just
                // giving back the base vector works better?  Sigh.
                _interruptOffset = 0;

                // Now update the offset and interrupts bits in lowest-to-highest
                // priority order, and account for the channel # (A > B).  First,
                // did the tx buffer just become empty?
                if (_txFifo.Count == 0 && _txInterruptLatched)
                {
                    _interruptOffset = 0;
                }

                // If one of the modem control pins changed state (DCD or CTS),
                // bump the offset ("external/status interrupt")
                if ((((RReg0)_readRegs[0] & RReg0.DCDState) != (oldPinState & RReg0.DCDState) ||
                     ((RReg0)_readRegs[0] & RReg0.CTSState) != (oldPinState & RReg0.CTSState)) &&
                    _extInterruptLatched)
                {
                    _interruptOffset = 1;
                }

                // Receive character available?
                if (_rxInterruptLatched)
                {
                    _interruptOffset = 2;
                }

                // Errors received will latch the bits in RR1; just update a few
                // status bits if they've changed
                if (SyncMode || (!SyncMode && _txFifo.Count == 0))
                {
                    _readRegs[1] |= (byte)RReg1.AllSent;
                }
                else
                {
                    _readRegs[1] &= (byte)~RReg1.AllSent;
                }

                // If an error occurred ("special receive conditions") that we
                // care about, set the offset...
                if (((_readRegs[1] & 0xf0) != 0) && _rxInterruptLatched)
                {
                    // Except... the silly mode where Parity Errors aren't "special"
                    // (so the offset should already have been set, above)
                    var parErr = (_readRegs[1] & (byte)RReg1.ParityError) != 0;

                    if (!parErr || (parErr && (IntFlags == WReg1IntEnables.RxIntOnAllParityAffectsVector)))
                    {
                        _interruptOffset = 3;
                    }

                    // todo: rx overrun if rxfifo len > 3?  or just let it grow...
                    // end of frame/sdlc checking happens here, of course, if that is ever used?
                    // framing errors would come from the serial port, but crc errors generated here
                }

                // Finally: update the interrupt pending bit in RR0.  This is
                // problematic, as the SIO only updates the bit in channel A's
                // RR0 but it takes into account both channel's status?  The
                // PERQ may or may not even rely on this bit...
                _readRegs[0] |= (byte)(InterruptLatched ? RReg0.IntPending : 0);

                Log.Debug(Category.SIO, "Channel {0} RR0 = {1}", _channelNumber, (RReg0)_readRegs[0]);
                Log.Debug(Category.SIO, "Channel {0} RR1 = {1}", _channelNumber, (RReg1)_readRegs[1]);
                Log.Debug(Category.SIO, "Channel {0} IRQ status: Tx {1}/{2}, Rx {3}/{4}, Ext {5}/{6}, Vec {7}",
                          _channelNumber, _txInterruptLatched, TxInterruptEnabled,
                                          _rxInterruptLatched, RxInterruptEnabled,
                                          _extInterruptLatched, ExtInterruptEnabled, _interruptOffset);
            }

            /// <summary>
            /// Detect a change in the programmed number of data bits per char,
            /// and if necessary issue a change to the connected serial device.
            /// </summary>
            /// <remarks>
            /// This is fairly problematic if we want to do "real" pass-thru to
            /// the port, and not just due to limitations and bugs in the C#
            /// SerialPort class.  The Z80 SIO allows _separate_ sizes for Rx and
            /// Tx, which is kind of nuts; here we take the latest setting and
            /// compare it to the last, only changing it in the device based on 
            /// the last register write, if it differs from the hardware setting.
            /// Oof.  Ugly.  We might just shine on the PERQ and let it think it
            /// made the change, while configuring the host port through Settings
            /// to suit the user's actual hardware and synthesizing the data flow
            /// as bytes are received...
            /// </remarks>
            private void UpdateBitsPerChar(int bits)
            {
#if DEBUG
                var rxBits = ((_writeRegs[3] & (byte)WReg3.RxBitsPerChar) >> 6);
                var txBits = ((_writeRegs[5] & (byte)WReg5.TxBitsPerChar) >> 5);

                // Whine about this, at least until we see how different Z80 and
                // OSes handle setting this.  Unfortunately, the PERQ/Z80 doesn't
                // seem to set both values consistently; for speech (output only)
                // it never bothers to set the receive bits?
                if (rxBits != txBits)
                {
                    Log.Debug(Category.SIO, "Channel {0} receive bits ({1}), transmit bits ({2}) mismatch",
                                            _channelNumber, rxBits, txBits);
                }
#endif
                // Why, Zilog.  Why.
                var setBits = ((bits == 3) ? 8 :
                               (bits == 1) ? 7 :
                               (bits == 2) ? 6 : 5);

                if ((_port != null) && (setBits != _port.DataBits))
                {
                    _port.DataBits = setBits;

                    Log.Debug(Category.SIO, "Channel {0} bits per character now {1}",
                                            _channelNumber, setBits);
                }
            }

            /// <summary>
            /// Changes the port's parity setting in response to register updates.
            /// </summary>
            private void UpdateParity()
            {
                var enable = (_writeRegs[4] & (byte)WReg4.ParityEnable) != 0;
                var polarity = (_writeRegs[4] & (byte)WReg4.ParityEvenOdd) >> 1;

                var parity = ((!enable) ? Parity.None : (polarity == 1) ? Parity.Even : Parity.Odd);

                if ((_port != null) && (_port.Parity != parity))
                {
                    _port.Parity = parity;

                    Log.Debug(Category.SIO, "Channel {0} parity now {1}", _channelNumber, parity);
                }
            }

            /// <summary>
            /// Pull out all the stop bits.
            /// </summary>
            private void UpdateStopBits()
            {
                if (Bits != WReg4Bits.SyncModesEnable)
                {
                    // Map from the SIO bits to the SerialPort enum, sigh
                    var stopBits = ((Bits == WReg4Bits.One) ? StopBits.One :
                                    (Bits == WReg4Bits.Two) ? StopBits.Two : StopBits.OnePointFive);

                    if ((_port != null) && (_port.StopBits != stopBits))
                    {
                        _port.StopBits = stopBits;

                        Log.Debug(Category.SIO, "Channel {0} stop bits now {1}", _channelNumber, stopBits);
                    }
                }
            }

            /// <summary>
            /// Change the state of the programmable output pins (RTS, DTR).
            /// </summary>
            private void UpdateOutputPins()
            {
                if (_port != null)
                {
                    var dtr = (_writeRegs[5] & (byte)WReg5.DTR) != 0;
                    var rts = (_writeRegs[5] & (byte)WReg5.RTS) != 0;

                    // Only change 'em if different
                    if (_port.DTR != dtr) _port.DTR = dtr;
                    if (_port.RTS != rts) _port.RTS = rts;

                    // Logged on UpdateFlags
                }
            }


            private int _channelNumber;
            private int _selectedRegister;

            private bool _rxIntOnNextCharacter;
            private bool _rxInterruptLatched;
            private bool _txInterruptLatched;
            private bool _extInterruptLatched;

            private int _interruptOffset;

            private bool _huntMode;
            private bool _breakDetected;

            private byte[] _writeRegs;
            private byte[] _readRegs;

            // These two FIFOs on the real hardware are just 3 bytes deep.  Here
            // they're unbounded in size and are (ab)used as the communications
            // stream between the SIO channel and the device it's connected to.
            private Queue<byte> _rxFifo;
            private Queue<byte> _txFifo;
            private Scheduler _scheduler;

            private ISIODevice _device;
            private ISerialDevice _port;
        }

        //
        // Read registers
        //

        [Flags]
        private enum RReg0 : byte
        {
            RxCharAvailable = 0x1,
            IntPending = 0x2,
            TxBufferEmpty = 0x4,
            DCDState = 0x8,
            SyncHunt = 0x10,
            CTSState = 0x20,
            TxUnderrun = 0x40,
            BreakAbort = 0x80
        }

        [Flags]
        private enum RReg1 : byte
        {
            AllSent = 0x1,
            ExtraBits = 0x0e,
            ParityError = 0x10,
            RxOverrun = 0x20,
            CrcFraming = 0x40,
            EndOfFrame = 0x80
        }

        //
        // Write registers
        //

        private enum WReg0Cmd
        {
            NullCode = 0,
            SendAbort = 1,
            ResetExtStatusInterrupts = 2,
            ChannelReset = 3,
            EnableIntOnRx = 4,
            ResetTxInt = 5,
            ErrorReset = 6,
            ReturnFromInt = 7
        }

        private enum WReg0Crc
        {
            NullCode = 0,
            ResetRxCRC = 1,
            ResetTxCRC = 2,
            ResetTxUnderrun = 3,
        }

        [Flags]
        private enum WReg1
        {
            ExtIntEnable = 0x1,
            TxIntEnable = 0x2,
            StatusAffectsVector = 0x4,
            WaitReadyOnRT = 0x20,
            WaitReadyFunction = 0x40,
            WaitReadyEnable = 0x80,
        }

        private enum WReg1IntEnables
        {
            RxIntDisable = 0,
            RxIntOnFirstChar = 1,
            RxIntOnAllParityAffectsVector = 2,
            RxIntOnAllParityNotAffectsVector = 3,
        }

        [Flags]
        private enum WReg3
        {
            RxEnable = 0x1,
            SyncCharLoadInhibit = 0x2,
            AddressSearchMode = 0x4,
            RxCRCEnable = 0x8,
            EnterHuntPhase = 0x10,
            AutoEnables = 0x20,
            RxBitsPerChar = 0xc0
        }

        private enum WReg4
        {
            ParityEnable = 0x1,
            ParityEvenOdd = 0x2,
        }

        private enum WReg4Bits
        {
            SyncModesEnable = 0,
            One = 1,
            OnePointFive = 2,
            Two = 3,
        }

        private enum WReg4SyncMode
        {
            EightBit = 0,
            SixteenBit = 1,
            SDLCMode = 2,
            ExtSyncMode = 3,
        }

        private enum WReg4ClockMode
        {
            X1 = 0,
            X16 = 1,
            X32 = 2,
            X64 = 3
        }

        [Flags]
        private enum WReg5
        {
            TxCRCEnable = 0x1,
            RTS = 0x2,
            SDLC = 0x4,
            TxEnable = 0x8,
            SendBreak = 0x10,
            TxBitsPerChar = 0x60,
            DTR = 0x80
        }

    }
}
