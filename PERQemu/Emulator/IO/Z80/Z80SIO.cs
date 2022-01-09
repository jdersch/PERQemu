//
// Z80SIO.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
            _baseAddress = baseAddress;
            _scheduler = scheduler;
            _ports = new byte[] { baseAddress, (byte)(baseAddress + 1), (byte)(baseAddress + 2), (byte)(baseAddress + 3) };

            _channels = new Channel[2];
            _channels[0] = new Channel(0);
            _channels[1] = new Channel(1);
        }

        public void Reset()
        {
            _channels[0].Reset();
            _channels[1].Reset();

            Trace.Log(LogType.Z80SIO, "SIO reset.");
        }

        public string Name => "Z80 SIO";
        public byte[] Ports => _ports;

        public bool IntLineIsActive
        {
            get { return _channels[0].InterruptLatched || _channels[1].InterruptLatched; }
        }

        public byte? ValueOnDataBus
        {
            // TODO: this is bogus
            get { return (byte)(0x40 + (_channels[0].InterruptLatched ? _channels[0].InterruptOffset : _channels[1].InterruptOffset)); }
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
                    throw new InvalidOperationException("Invalid SIO port address.");
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
                    throw new InvalidOperationException("Invalid SIO port address.");
            }
        }

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
                _writeRegs.Initialize();
                _readRegs.Initialize();
                _rxFifo.Clear();
                _txFifo.Clear();

                _selectedRegister = 0;
                _huntMode = true;               // re-entered after Reset
                _txInterruptLatched = false;
                _rxInterruptLatched = false;
                _rxIntOnNextCharacter = false;

                if (_device != null)
                {
                    _device.Reset();
                }

                UpdateFlags();

                Trace.Log(LogType.Z80SIO, "SIO Channel {0} reset.", _channelNumber);
            }

            public bool InterruptLatched => _rxInterruptLatched || _txInterruptLatched;
            public int InterruptOffset => _interruptVector;

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
                    Trace.Log(LogType.Z80SIO, "SIO Channel {0} read {1:x} from register {2}",
                              _channelNumber, value, _selectedRegister);
                    return value;
                }
                else
                {
                    Trace.Log(LogType.Z80SIO, "SIO Channel {0} read from invalid register {2}",
                              _channelNumber, _selectedRegister);
                    return 0;
                }
            }

            public byte ReadData()
            {
                byte data = 0;

                if (_rxFifo.Count > 0)
                {
                    data = _rxFifo.Dequeue();
                    UpdateFlags();
                }

                Trace.Log(LogType.Z80SIO, "SIO Channel {0} data read: {1:x2}",
                          _channelNumber, data);
                return data;
            }

            public void WriteData(byte value)
            {
                Trace.Log(LogType.Z80SIO, "SIO Channel {0} data write: {1:x2}",
                          _channelNumber, value);
                // Nothing right now.
            }

            public void WriteRegister(byte value)
            {
                Trace.Log(LogType.Z80SIO, "SIO Channel {0} write {1:x2} to register {2}",
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
                            throw new NotImplementedException(string.Format("Unimplemented SIO command {0}.", cmd));
                    }

                    // Select register pointer
                    _selectedRegister = (value & 0x7);
                }
                else
                {
                    // Handle special cases for bits which trigger actions when set:
                    switch (_selectedRegister)
                    {
                        case 3:
                            if ((_writeRegs[3] & (byte)WReg3.EnterHuntPhase) != 0)
                            {
                                // re-enter HUNT mode
                                _huntMode = true;
                            }
                            break;

                        case 4:
                            
                            break;
                    }
                    // Write to other register, next access is to reg 0.
                    _selectedRegister = 0;
                }

                Trace.Log(LogType.Z80SIO, "SIO Channel {0} register pointer now {1}",
                          _channelNumber, _selectedRegister);
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
                        if (_huntMode)  // Looking for sync byte(s)
                        {
                            if (data == _writeRegs[7])  // 8-bit sync value
                            {
                                 Trace.Log(LogType.Z80SIO, "SIO Channel {0} sync word matched", _channelNumber);
                                _huntMode = false;  // exit hunt mode
                            }
                        }
                        else if (!_huntMode)    // Collecting data
                        {
                            _rxFifo.Enqueue(data);
                            UpdateFlags();
                            Trace.Log(LogType.Z80SIO, "SIO Channel {0} enqueued byte {1:x2}, fifo depth now {2}", _channelNumber, data, _rxFifo.Count);
                        }
                    }
                    else // Async mode
                    {
                        throw new NotImplementedException();
                    }
                }
            }

            private void UpdateFlags()
            {
                // Set RX-related flags and raise interrupts as necessary.
                _readRegs[0] = (byte)(
                    (_huntMode ? 0x0 : 0x10) |          // SYNC/HUNT
                    0x04 |                              // tx buffer empty
                    (_rxFifo.Count > 0 ? 0x01 : 0x00)   // rx character available
                    );

                if (_rxFifo.Count > 0 && RxInterruptEnabled)
                {
                    _rxIntOnNextCharacter = false;
                    _rxInterruptLatched = true;
                    _interruptVector = 4;
                }
                else
                {
                    _rxInterruptLatched = false;
                }
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
            private int _interruptVector;
            
            private bool _huntMode;

            private byte[] _writeRegs;
            private byte[] _readRegs;

            /// <summary>
            /// TODO:
            /// These two FIFOs on the real hardware are just 3 bytes deep.  Here they're unbounded in size and are (ab)used as the
            /// communications stream between the SIO channel and the device it's connected to.
            /// What we should really have here is a proper 3-level FIFO and a real stream-ish abstraction somewhere else that connects
            /// from the SIO to the device, but this will work for now.
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

        private bool _interruptActive;
        private byte _interruptVector;

        private Scheduler _scheduler;
        private byte _baseAddress;
        private byte[] _ports;
    }
}
