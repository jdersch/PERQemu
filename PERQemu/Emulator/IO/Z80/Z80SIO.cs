//
// Z80SIO.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// A rough implementation of the Z80 SIO serial controller.
    /// It implements the operational modes that the PERQ IOB makes use of.
    /// Currently only enough is implemented to support the Kriz tablet.
    /// </summary>
    public class Z80SIO : IZ80Device, IDMADevice
    {
        public Z80SIO(byte baseAddress, Scheduler scheduler)
        {
            _scheduler = scheduler;

            _baseAddress = baseAddress;
            _ports = new byte[] { baseAddress,
                                (byte)(baseAddress + 1),
                                (byte)(baseAddress + 2),
                                (byte)(baseAddress + 3)
            };

            _channels = new Channel[2];
            _channels[0] = new Channel(0);
            _channels[1] = new Channel(1);
        }

        public void Reset()
        {
            _channels[0].Reset();
            _channels[1].Reset();

            Log.Debug(Category.SIO, "Reset");
        }

        public string Name => "Z80 SIO";
        public byte[] Ports => _ports;

        public bool IntLineIsActive
        {
            get { return _channels[0].InterruptLatched || _channels[1].InterruptLatched; }
        }

        public byte? ValueOnDataBus
        {
            get { return ComputeVector(); }
        }

        /// <summary>
        /// Compute the SIO's interrupt vector based on the chip's arcane rules.
        /// </summary>
        /// <remarks>
        /// The SIO's interrupt vector is only programmed into channel B's WR2
        /// register.  When read back from RR2 (or put on the bus when the IRQ
        /// is serviced) it is supposed to return the current vector based on
        /// the highest priority from _both_ channels.  Because channel A has
        /// higher priority, we have to get both offsets and compute the V3..V1
        /// bits from A or B but using the base vector from B, and only if the
        /// "Status Affects Vector" bit is set (only in B's WR1).  Oof.
        /// </remarks>
        public byte ComputeVector()
        {
            byte vector = _channels[1].InterruptBase;

            if (_channels[1].StatusAffectsVector)
            {
                // If an interrupt is pending on A, use it's offset; otherwise
                // assume that B is interrupting...
                var priority = (_channels[0].InterruptLatched ? _channels[0].InterruptOffset : _channels[1].InterruptOffset);

                // Now hack the offset into vector<3:1>
                vector = (byte)((vector & 0xf1) | ((priority & 0x07) << 1));
                Log.Debug(Category.SIO, "Status Affected Vector is {0} (prio={2})", vector, priority);
            }

            return vector;
        }

        public event EventHandler NmiInterruptPulse;

        // IDMADevice implementation
        public bool ReadDataReady => true;
        public bool WriteDataReady => true;

        public void DMATerminate()
        {
        }

        public void AttachDevice(int channel, ISIODevice device)
        {
            if (channel < 0 || channel > 1)
            {
                throw new ArgumentOutOfRangeException(nameof(channel));
            }

            _channels[channel].AttachDevice(device);
        }

        public byte Read(byte portAddress)
        {
            switch (portAddress - _baseAddress)
            {
                case 0:
                    return _channels[0].ReadData();

                case 1:
                    return _channels[0].ReadRegister();

                case 2:
                    return _channels[1].ReadData();

                case 3:
                    return _channels[1].ReadRegister();

                default:
                    throw new InvalidOperationException("Invalid SIO port address");
            }
        }

        public void Write(byte portAddress, byte value)
        {
            switch (portAddress - _baseAddress)
            {
                case 0:
                    _channels[0].WriteData(value);
                    break;

                case 1:
                    _channels[0].WriteRegister(value);
                    break;

                case 2:
                    _channels[1].WriteData(value);
                    break;

                case 3:
                    _channels[1].WriteRegister(value);
                    break;

                default:
                    throw new InvalidOperationException("Invalid SIO port address");
            }
        }

        /// <summary>
        /// One SIO Channel.  Though they're not _quite_ identical.  Channel B
        /// holds the interrupt vector in its WR2/RR2 registers!
        /// </summary>
        private class Channel
        {
            public Channel(int channelNumber)
            {
                _channelNumber = channelNumber;

                _writeRegs = new byte[8];
                _readRegs = new byte[3];

                _rxFifo = new Queue<byte>();
                _txFifo = new Queue<byte>();
            }

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
                _txInterruptLatched = false;
                _rxInterruptLatched = false;
                _rxIntOnNextCharacter = false;

                // Reset attached devices
                if (_device != null)
                {
                    _device.Reset();
                }

                UpdateFlags();

                Log.Debug(Category.SIO, "Channel {0} reset", _channelNumber);
            }

            public bool InterruptLatched => _rxInterruptLatched || _txInterruptLatched;
            public bool StatusAffectsVector => (_writeRegs[1] & (byte)WReg1.StatusAffectsVector) != 0;
            public byte InterruptBase => _readRegs[2];      // Channel B only...
            public int InterruptOffset => _interruptOffset;


            public void AttachDevice(ISIODevice device)
            {
                _device = device;

                _device.RegisterReceiveDelegate(ReceiveData);
            }


            public byte ReadRegister()
            {
                if (_selectedRegister < 3)
                {
                    byte value = _readRegs[_selectedRegister];
                    Log.Debug(Category.SIO, "Channel {0} read 0x{1:x2} from register {2}",
                                            _channelNumber, value, _selectedRegister);
                    return value;
                }

                Log.Debug(Category.SIO, "Channel {0} read from invalid register {2}",
                                        _channelNumber, _selectedRegister);
                return 0;
            }


            public byte ReadData()
            {
                byte data = 0;

                if (_rxFifo.Count > 0)
                {
                    data = _rxFifo.Dequeue();
                    UpdateFlags();
                }

                Log.Debug(Category.SIO, "Channel {0} data read: 0x{1:x2}",
                                        _channelNumber, data);
                return data;
            }


            public void WriteRegister(byte value)
            {
                Log.Debug(Category.SIO, "Channel {0} write 0x{1:x2} to register {2}",
                                        _channelNumber, value, _selectedRegister);

                _writeRegs[_selectedRegister] = value;

                if (_selectedRegister == 0)
                {
                    // Write to WR0:

                    // TODO: handle CRC resets

                    // Execute command:
                    WReg0Cmd cmd = (WReg0Cmd)((value & 0x38) >> 3);
                    switch (cmd)
                    {
                        case WReg0Cmd.NullCode:
                            break;

                        case WReg0Cmd.ResetExtStatusInterrupts:
                            _rxInterruptLatched = false;
                            _txInterruptLatched = false;
                            break;

                        case WReg0Cmd.ChannelReset:
                            Reset();
                            break;

                        case WReg0Cmd.EnableIntOnRx:
                            _rxIntOnNextCharacter = IntOnFirstRxCharacter;
                            break;

                        default:
                            throw new NotImplementedException($"SIO command {cmd}");
                    }

                    // Select register pointer
                    _selectedRegister = (value & 0x7);
                }
                else
                {
                    // Handle special cases for bits which trigger actions when set:
                    switch (_selectedRegister)
                    {
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
                                // re-enter HUNT mode
                                _huntMode = true;
                                Log.Detail(Category.SIO, "Entering hunt mode");
                            }
                            break;
                    }
                    // Write to other register, next access is to reg 0
                    _selectedRegister = 0;
                }

                Log.Debug(Category.SIO, "Channel {0} register pointer now {1}",
                                        _channelNumber, _selectedRegister);
            }


            public void WriteData(byte value)
            {
                Log.Debug(Category.SIO, "Channel {0} data write: 0x{1:x2}",
                                        _channelNumber, value);
            }

            /// <summary>
            /// Invoked by attached ISIODevice when it has data to send.
            /// </summary>
            private void ReceiveData(byte data)
            {
                // Accept data if RX enabled
                if (RxEnabled)
                {
                    if (SyncMode)
                    {
                        if (_huntMode)          // Looking for sync byte(s)
                        {
                            if (data == _writeRegs[7])  // 8-bit sync value
                            {
                                Log.Debug(Category.SIO, "Channel {0} sync word matched", _channelNumber);
                                _huntMode = false;      // Exit hunt mode
                            }
                        }
                        else if (!_huntMode)    // Collecting data
                        {
                            _rxFifo.Enqueue(data);
                            UpdateFlags();
                            Log.Debug(Category.SIO, "Channel {0} enqueued byte {1:x2}, fifo depth now {2}",
                                                    _channelNumber, data, _rxFifo.Count);
                        }
                    }
                    else // Async mode
                    {
                        throw new NotImplementedException("Async mode");
                    }
                }
            }


            // todo: TransmitData()


            private void UpdateFlags()
            {
                // Channel A (0) has priorities 4..7, Channel B (1) is 0..3. :-|
                var offset = (_channelNumber ^ 0x1) * 4;

                // Set RX-related flags and raise interrupts as necessary
                _readRegs[0] = (byte)(
                    (_huntMode ? 0x0 : 0x10) |          // SYNC/HUNT
                    (_txFifo.Count == 0 ? 0x04 : 0x0) | // tx buffer empty
                    (_rxFifo.Count > 0 ? 0x01 : 0x0)    // rx character available
                    );

                // If no interrupts pending, the Z80 returns V3..V1 = 011.  So
                // regardless of whichever channel we are, set up the default...
                _interruptOffset = 3;

                // Now update the offset and interrupts bits in lowest-to-highest
                // priority order, and account for the channel # (A > B).  First,
                // did the tx buffer just become empty?
                if (_txFifo.Count == 0 && _txInterruptLatched)
                {
                    _interruptOffset = offset;
                }

                // todo: If one of the modem control pins changed state (RTS or
                // DTR, maybe DCD?) then set _interruptOffset = offset + 1;

                if (_rxFifo.Count > 0 && RxInterruptEnabled)
                {
                    _rxIntOnNextCharacter = false;
                    _rxInterruptLatched = true;
                    _interruptOffset = offset + 2;
                }
                else
                {
                    _rxInterruptLatched = false;
                }

                // todo: If an error occurred ("special receive conditions")
                // that we care about, set _interruptOffset offset + 3;
            }


            private bool RxEnabled => (_writeRegs[3] & (byte)WReg3.RxEnable) != 0;
            private WReg4Bits Bits => (WReg4Bits)((_writeRegs[4] & 0xc) >> 2);
            private bool SyncMode => Bits == WReg4Bits.SyncModesEnable;
            private WReg1IntEnables IntFlags => (WReg1IntEnables)((_writeRegs[1] & 0x18) >> 3);
            private bool IntOnFirstRxCharacter => IntFlags == WReg1IntEnables.RxIntOnFirstChar;
            private bool IntOnAllRxCharacters => IntFlags == WReg1IntEnables.RxIntOnAllRxParityAffectsVector ||
                                                 IntFlags == WReg1IntEnables.RxIntOnAllRxParityNotAffectsVector;
            private bool RxInterruptEnabled => (IntOnFirstRxCharacter && _rxIntOnNextCharacter) || IntOnAllRxCharacters;

            private int _channelNumber;
            private int _selectedRegister;

            private bool _rxInterruptLatched;
            private bool _txInterruptLatched;
            private bool _rxIntOnNextCharacter;
            private int _interruptOffset;

            private bool _huntMode;

            private byte[] _writeRegs;
            private byte[] _readRegs;

            /// <summary>
            /// TODO:
            /// These two FIFOs on the real hardware are just 3 bytes deep.  Here
            /// they're unbounded in size and are (ab)used as the communications
            /// stream between the SIO channel and the device it's connected to.
            /// What we should really have here is a proper 3-level FIFO and a
            /// real stream-ish abstraction somewhere else that connects from the
            /// SIO to the device, but this will work for now.
            /// </summary>
            private Queue<byte> _rxFifo;
            private Queue<byte> _txFifo;

            private ISIODevice _device;
        }


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
            RxIntOnAllRxParityAffectsVector = 2,
            RxIntOnAllRxParityNotAffectsVector = 3,
        }

        private enum WReg3
        {
            RxEnable = 0x1,
            SyncCharLoadInhibit = 0x2,
            AddressSearchMode = 0x4,
            RxCRCEnable = 0x8,
            EnterHuntPhase = 0x10,
            AutoEnables = 0x20,
        }

        private enum WReg3RxBits
        {
            Rx5Bits = 0,
            Rx7Bits = 1,
            Rx6Bits = 2,
            Rx8Bits = 3,
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

        private enum WReg5
        {
            TxCRCEnable = 0x1,
            RTS = 0x2,
            SDLC = 0x4,
            TxEnable = 0x8,
            SendBreak = 0x10,
        }

        private enum WReg5TxBits
        {
            Tx5Bits = 0,
            Tx7Bits = 1,
            Tx6Bits = 2,
            Tx8Bits = 3,
        }

        private Channel[] _channels;
        private Scheduler _scheduler;
        private byte _baseAddress;
        private byte[] _ports;
    }
}
