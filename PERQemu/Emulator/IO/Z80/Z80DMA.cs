//
// Z80DMA.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
    /// Implements most of the Z80 DMA controller.
    /// </summary>
    /// <remarks>
    /// The PERQ-1 IOB uses a Mostek MK3883N DMA chip (second source for the
    /// Zilog Z8410) which has one channel.  The PERQ-2 EIO uses the AMD Am9517
    /// (aka Intel i8237) 4-channel DMA chip.  We'll have to refactor things so
    /// each I/O Board loads its own DMA controller.
    /// 
    /// Under the "old Z80" (IOB) protocol, the read registers are not accessed,
    /// but apparently with the "new Z80" (CIO) they are!
    /// </remarks>
    public class Z80DMA : IZ80Device
    {
        public Z80DMA(byte baseAddress, Z80MemoryBus memoryBus, Z80IOBus ioBus)
        {
            _memoryBus = memoryBus;
            _ioBus = ioBus;
            _baseAddress = baseAddress;
            _wr = new byte[7];
            _statusData = new Queue<byte>();
        }

        public void Reset()
        {
            for (var i = 0; i < _wr.Length; i++)
                _wr[i] = 0;

            _writeBaseRegister = true;
            _baseRegister = 0;

            _portAddressAInit = 0;
            _portAddressBInit = 0;
            _blockLength = 0;
            _byteCounter = 0;
            _maskByte = 0;
            _matchByte = 0;
            _interruptControl = 0;
            _pulseControl = 0;
            _interruptVector = 0;
            _interruptActive = false;

            _enableDMA = false;
            _state = DMAState.Idle;
            _status = RR0.None;
            _statusMask = ReadMask.None;
            _statusData.Clear();

            Log.Debug(Category.Z80DMA, "Reset");
        }

        public string Name => "Z80 DMA";
        public byte[] Ports => new byte[] { _baseAddress };
        public byte? ValueOnDataBus => _interruptVector;    // TODO: implement dynamic vector based on type
        public bool IntLineIsActive => _interruptEnabled && _interruptActive;

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        public void AttachDeviceA(IDMADevice device)
        {
            _deviceA = device;
        }

        public void AttachDeviceB(IDMADevice device)
        {
            _deviceB = device;
        }

        public void Clock()
        {
            // TODO: handle "Interrupt on RDY" option

            // If DMA is in progress, make it happen
            if (_enableDMA)
            {
                _state = RunStateMachine();
            }
        }

        /// <summary>
        /// Run the DMA state machine.  We have to split the transaction into
        /// two bus cycles because the Z80 can't do two memory/port operations
        /// between instructions; data corruption results.
        /// </summary>
        /// <remarks>
        /// DANGER, Will Robinson!  This extremely simplistic first hack is to
        /// verify that the stupid Z80 is corrupting memory; it is NOT optimized
        /// and _requires_ that the transaction completes so that addresses and
        /// byte counters are properly checked.  If the Z80 issues an abort mid-
        /// transaction this house of cards collapses.  If it solves the problem
        /// I'll figure out how to make it more robust...
        /// </remarks>
        DMAState RunStateMachine()
        {
            DMAState nextState = _state;

            // Only here if enabled, so jump right in
            if (_state == DMAState.Idle)
            {
                _state = nextState = DMAState.SourceRead;
            }

            IDMADevice source;
            IDMADevice dest;

            ushort sourceAddress;
            bool sourceIsIO;
            ushort destAddress;
            bool destIsIO;

            // What direction is this going in
            WR0 wr0 = (WR0)_wr[0];
            WR1 wr1 = (WR1)_wr[1];
            WR2 wr2 = (WR2)_wr[2];

            if ((wr0 & WR0.DirectionAtoB) != 0)
            {
                // Source is A, dest is B
                source = _deviceA;
                sourceAddress = _portAddressA;
                sourceIsIO = (wr1 & WR1.MemoryOrIO) != 0;
                dest = _deviceB;
                destAddress = _portAddressB;
                destIsIO = (wr2 & WR2.MemoryOrIO) != 0;
            }
            else
            {
                source = _deviceB;
                sourceAddress = _portAddressB;
                sourceIsIO = (wr2 & WR2.MemoryOrIO) != 0;
                dest = _deviceA;
                destAddress = _portAddressA;
                destIsIO = (wr1 & WR1.MemoryOrIO) != 0;
            }

            // Set the (active low) Ready bit
            _status |= RR0.ReadyActive;

            switch (_state)
            {
                case DMAState.SourceRead:
                    if (source.ReadDataReady)
                    {
                        if (sourceIsIO)
                        {
                            _data = _ioBus[sourceAddress];
                        }
                        else
                        {
                            _data = _memoryBus[sourceAddress];
                        }

                        Log.Detail(Category.Z80DMA,
                                   "Source read 0x{0:x2} from {1} (0x{2:x4})",
                                   _data, source, sourceAddress);

                        nextState = DMAState.DestWrite;

                        // 0 = Ready active
                        _status &= ~RR0.ReadyActive;
                    }
                    break;

                case DMAState.DestWrite:
                    if (dest.WriteDataReady)
                    {
                        if (destIsIO)
                        {
                            _ioBus[destAddress] = _data;
                        }
                        else
                        {
                            _memoryBus[destAddress] = _data;
                        }

                        // Ready active
                        _status &= ~RR0.ReadyActive;

                        // Assume there's a next byte
                        nextState = DMAState.SourceRead;

                        // Update addresses & counters
                        _byteCounter--;

                        Log.Detail(Category.Z80DMA,
                                   "Dest write 0x{0:x2} to {1} (0x{2:x4}), {3} bytes left",
                                   _data, dest, destAddress, _byteCounter);

                        if ((wr1 & WR1.PortAAddressFixed) == 0)
                        {
                            if ((wr1 & WR1.PortAIncrements) == 0)
                            {
                                _portAddressA--;
                            }
                            else
                            {
                                _portAddressA++;
                            }
                        }

                        if ((wr2 & WR2.PortBAddressFixed) == 0)
                        {
                            if ((wr2 & WR2.PortBIncrements) == 0)
                            {
                                _portAddressB--;
                            }
                            else
                            {
                                _portAddressB++;
                            }
                        }
                    }

                    // Take action at the end of the block -- interrupt, restart, etc.
                    if (_byteCounter == 0)
                    {
                        Log.Debug(Category.Z80DMA, "Transfer complete");
                        nextState = DMAState.Idle;

                        if ((_interruptControl & 0x2) != 0)
                        {
                            _interruptActive = true;
                        }

                        WR5 wr5 = (WR5)_wr[5];

                        if ((wr5 & WR5.AutoRepeat) != 0)
                        {
                            _byteCounter = _blockLength;
                            _portAddressA = _portAddressAInit;
                            _portAddressB = _portAddressBInit;

                            Log.Debug(Category.Z80DMA, "Transfer auto-restarting");
                        }
                        else
                        {
                            source.DMATerminate();
                            dest.DMATerminate();
                            _enableDMA = false;
                        }
                    }
                    break;
            }
            return nextState;
        }

        /// <summary>
        /// Return status data from RR0..RR6.  It's assumed that the only port
        /// address that responds to reads will be the status register; status
        /// data must be prepared by commands written to WR6.
        /// </summary>
        public byte Read(byte portAddress)
        {
            if (_statusData.Count == 0)
            {
                Log.Warn(Category.Z80DMA, "Status read on 0x{0:x2} from empty queue, returning 0", portAddress);
                return 0x0;
            }

            var data = _statusData.Dequeue();

            Log.Detail(Category.Z80DMA, "Status read on 0x{0:x2}, returning 0x{1:x2}", portAddress, data);
            return data;
        }


        public void Write(byte portAddress, byte value)
        {
            // DMA is disabled when a control byte is written
            _enableDMA = false;

            if (_writeBaseRegister)
            {
                _baseRegister = 0;

                // Note: we start with the decode for reg 1; if no matches are found
                // we assume a match on reg 0.
                for (int i = 1; i < _decodes.Length; i++)
                {
                    if ((value & _decodes[i].Mask) == _decodes[i].Value)
                    {
                        _baseRegister = i;
                        break;
                    }
                }

                Log.Debug(Category.Z80DMA, "Write of 0x{0:x2} to base register {1}",
                                           value, _baseRegister);
                WriteBaseRegister(value);
            }
            else
            {
                // Write sub-registers
                WriteSubRegister(value);
            }
        }

        void WriteBaseRegister(byte value)
        {
            _wr[_baseRegister] = value;

            switch (_baseRegister)
            {
                case 0:
                    // bits D3-D6 indicate sub-registers to be written to,
                    // if none are set, return to base state.
                    _writeBaseRegister = (value & 0x78) == 0;
                    break;

                case 1:
                case 2:
                    // bit D6 indicates a sub-register is to be written
                    _writeBaseRegister = (value & 0x40) == 0;
                    break;

                case 3:
                    // bits D3 and D4:
                    _writeBaseRegister = (value & 0x18) == 0;
                    break;

                case 4:
                    // bits D2-D4:
                    _writeBaseRegister = (value & 0x1c) == 0;
                    break;

                case 6:
                    // handle command
                    ExecuteCommand(value);
                    _writeBaseRegister = true;
                    break;
            }
        }

        void WriteSubRegister(byte value)
        {
            byte regVal = _wr[_baseRegister];

            switch (_baseRegister)
            {
                case 0:
                    if ((regVal & 0x08) != 0)
                    {
                        _portAddressAInit = (ushort)((_portAddressAInit & 0xff00) | value);
                        _wr[_baseRegister] &= 0xf7;

                        Log.Debug(Category.Z80DMA, "Port A address now 0x{0:x4}", _portAddressAInit);
                    }
                    else if ((regVal & 0x10) != 0)
                    {
                        _portAddressAInit = (ushort)((_portAddressAInit & 0x00ff) | (value << 8));
                        _wr[_baseRegister] &= 0xef;

                        Log.Debug(Category.Z80DMA, "Port A address now 0x{0:x4}", _portAddressAInit);
                    }
                    else if ((regVal & 0x20) != 0)
                    {
                        _blockLength = (ushort)((_blockLength & 0xff00) | value);
                        _wr[_baseRegister] &= 0xdf;

                        Log.Debug(Category.Z80DMA, "Block length now 0x{0:x4}", _blockLength);
                    }
                    else if ((regVal & 0x20) != 0)
                    {
                        _blockLength = (ushort)((_blockLength & 0x00ff) | (value << 8));
                        _wr[_baseRegister] &= 0xbf;

                        Log.Debug(Category.Z80DMA, "Block length now 0x{0:x4}", _blockLength);
                    }
                    _writeBaseRegister = (value & 0x78) == 0;
                    break;

                case 1:
                case 2:
                    // Right now I'm not implementing variable timing, so just ignore this.
                    _writeBaseRegister = true;
                    break;

                case 3:
                    if ((regVal & 0x08) != 0)
                    {
                        _maskByte = value;
                        _wr[_baseRegister] &= 0xf7;
                    }
                    else if ((regVal & 0x10) != 0)
                    {
                        _matchByte = value;
                        _wr[_baseRegister] &= 0xef;
                    }
                    _writeBaseRegister = (value & 0x18) == 0;
                    break;

                case 4:
                    if ((regVal & 0x04) != 0)
                    {
                        _portAddressBInit = (ushort)((_portAddressBInit & 0xff00) | value);
                        _wr[_baseRegister] &= 0xfb;

                        Log.Debug(Category.Z80DMA, "Port Address B now 0x{0:x4}", _portAddressBInit);
                    }
                    else if ((regVal & 0x08) != 0)
                    {
                        _portAddressBInit = (ushort)((_portAddressBInit & 0x00ff) | (value << 8));
                        _wr[_baseRegister] &= 0xf7;

                        Log.Debug(Category.Z80DMA, "Port Address B now 0x{0:x4}", _portAddressBInit);
                    }
                    else if ((regVal & 0x10) != 0)
                    {
                        _interruptControl = value;
                        _wr[_baseRegister] &= 0xef;
                    }
                    else if ((_interruptControl & 0x08) != 0)
                    {
                        _pulseControl = value;
                        _interruptControl &= 0xf7;
                    }
                    else if ((_interruptControl & 0x10) != 0)
                    {
                        _interruptVector = value;
                        _interruptControl &= 0xef;

                        Log.Debug(Category.Z80DMA, "Interrupt vector now 0x{0:x4}", _interruptVector);
                    }
                    _writeBaseRegister = ((regVal & 0x1c) == 0) && ((_interruptControl & 0x18) == 0);
                    break;

                case 6:
                    _statusMask = (ReadMask)value;
                    Log.Debug(Category.Z80DMA, "Status read mask now 0x{0:x2}", value);
                    _writeBaseRegister = true;
                    break;

                default:        // Shouldn't happen
                    throw new InvalidOperationException("Unexpected subregister write");
            }
        }

        void ExecuteCommand(byte command)
        {
            switch (command)
            {
                case 0xc3:  // Reset
                    Reset();
                    break;

                case 0xa3:  // Reset and disable interrupts
                    Reset();
                    _interruptEnabled = false;
                    break;

                case 0xc7:  // Reset Port A timing
                case 0xcb:  // Reset Port B timing
                    // ignored for now
                    break;

                case 0xcf:  // Load starting addresses, reset byte counter
                    _byteCounter = 0;
                    _portAddressA = _portAddressAInit;
                    _portAddressB = _portAddressBInit;
                    break;

                case 0xd3:  // Continue from current locations, clear byte counter
                    _byteCounter = 0;
                    break;

                case 0xab:  // Enable interrupts
                    _interruptEnabled = true;
                    break;

                case 0xaf:  // Disable interrupts
                    _interruptEnabled = false;
                    break;

                case 0x87:  // Enable DMA
                    _enableDMA = true;
                    _byteCounter = (ushort)(_blockLength + 1);
                    break;

                case 0x83:  // Disable DMA
                    _enableDMA = false;
                    break;

                case 0x8b:  // Reinitialize Status Byte
                    _status = RR0.None;
                    break;

                case 0xbb:  // Read Mask follows
                    _statusMask = ReadMask.None;    // Clear it
                    _writeBaseRegister = false;     // Next write to read mask register
                    break;

                case 0xa7:  // Initiate Read Sequence
                case 0xbf:  // Read Status Byte
                    QueueStatusBytes((command == 0xa7));
                    break;

                // case 0xb3:  // Force Ready
                // case 0xb7:  // Enable After RETI

                // Unimplemented or bad command
                default:
                    throw new NotImplementedException($"DMA command 0x{command:x2}");
            }
        }

        /// <summary>
        /// Updates the current status byte and queues values from the Read
        /// Registers.  Like FDC status, the DMA chip expects that if you ask
        /// for all seven bytes, you're gonna read all seven bytes!
        /// </summary>
        void QueueStatusBytes(bool all)
        {
            //
            // Most of the status bits are active low, so inverted sense.  I'm
            // just guessing at the actual meanings for now:
            //
            // "Match found" is unimplemented, so always set
            // "DMA transfer has occurred" means byte count != block length?  (Active high)
            // "End of block" means byte count has reached zero?
            // "Ready active" is the state of the RDY pin?  Set by the state machine
            // 
            _status |= RR0.MatchFound;
            _status |= (_interruptActive ? RR0.None : RR0.InterruptPending);
            _status |= (_byteCounter == 0 ? RR0.None : RR0.EndOfBlock);
            _status |= ((_byteCounter > 0 && _byteCounter < _blockLength) ? RR0.XferHasOccurred : RR0.None);

            Log.Detail(Category.Z80DMA, "Status now {0} (0x{1:x2})", _status, (byte)_status);

            // Do they want all the bytes or just one?
            if (all)
            {
                // Queue up all the unmasked bytes, in order
                if (_statusMask.HasFlag(ReadMask.StatusByte))
                    _statusData.Enqueue((byte)_status);

                // RR1..2 return byte counter high byte, then low
                if (_statusMask.HasFlag(ReadMask.ByteCountHigh))
                    _statusData.Enqueue((byte)((_byteCounter & 0xff00) >> 8));

                if (_statusMask.HasFlag(ReadMask.ByteCountLow))
                    _statusData.Enqueue((byte)_byteCounter);

                // RR3..6 are the address bytes in low, high order!
                if (_statusMask.HasFlag(ReadMask.PortAddrALow))
                    _statusData.Enqueue((byte)_portAddressA);

                if (_statusMask.HasFlag(ReadMask.PortAddrAHigh))
                    _statusData.Enqueue((byte)((_portAddressA & 0xff00) >> 8));

                if (_statusMask.HasFlag(ReadMask.PortAddrBLow))
                    _statusData.Enqueue((byte)_portAddressB);

                if (_statusMask.HasFlag(ReadMask.PortAddrBHigh))
                    _statusData.Enqueue((byte)((_portAddressB & 0xff00) >> 8));
            }
            else
            {
                // Just the status, ma'am
                _statusData.Enqueue((byte)_status);
            }
        }

        enum DMAState
        {
            Idle = 0,
            SourceRead,
            DestWrite
        }

        [Flags]
        enum WR0
        {
            Transfer = 0x1,
            Search = 0x2,
            SearchTransfer = 0x3,
            DirectionAtoB = 0x4,
        }

        [Flags]
        enum WR1
        {
            MemoryOrIO = 0x8,
            PortAIncrements = 0x10,
            PortAAddressFixed = 0x20,
        }

        [Flags]
        enum WR2
        {
            MemoryOrIO = 0x8,
            PortBIncrements = 0x10,
            PortBAddressFixed = 0x20,
        }

        [Flags]
        enum WR3
        {
            StopOnMatch = 0x4,
            InterruptEnable = 0x20,
            DMAEnable = 0x40,
        }

        [Flags]
        enum WR5
        {
            ReadyActiveHigh = 0x8,
            CEWAITMultiplexed = 0x10,
            AutoRepeat = 0x20,
        }

        [Flags]
        enum ReadMask
        {
            None = 0x0,
            StatusByte = 0x1,
            ByteCountLow = 0x2,
            ByteCountHigh = 0x4,
            PortAddrALow = 0x8,
            PortAddrAHigh = 0x10,
            PortAddrBLow = 0x20,
            PortAddrBHigh = 0x40
        }

        [Flags]
        enum RR0
        {
            None = 0x0,
            XferHasOccurred = 0x1,
            ReadyActive = 0x2,
            InterruptPending = 0x8,
            MatchFound = 0x10,
            EndOfBlock = 0x20
        }

        struct RegisterDecodes
        {
            public RegisterDecodes(byte mask, byte value)
            {
                Mask = mask;
                Value = value;
            }

            public byte Mask;
            public byte Value;
        }

        RegisterDecodes[] _decodes =
        {
            new RegisterDecodes(0x80, 0x00),    // WR0, not actually decoded (assumed if no matches are found)
            new RegisterDecodes(0x87, 0x04),
            new RegisterDecodes(0x87, 0x00),
            new RegisterDecodes(0x83, 0x80),
            new RegisterDecodes(0x83, 0x81),
            new RegisterDecodes(0xc7, 0x82),
            new RegisterDecodes(0x83, 0x83)     // WR6
        };

        Z80IOBus _ioBus;
        Z80MemoryBus _memoryBus;
        IDMADevice _deviceA;
        IDMADevice _deviceB;
        byte _baseAddress;

        bool _writeBaseRegister;
        int _baseRegister;

        byte[] _wr;

        ushort _portAddressAInit;
        ushort _portAddressBInit;
        ushort _blockLength;
        byte _maskByte;
        byte _matchByte;
        byte _interruptControl;
        byte _pulseControl;
        byte _interruptVector;
        byte _data;

        DMAState _state;
        RR0 _status;
        ReadMask _statusMask;
        Queue<byte> _statusData;

        bool _enableDMA;
        bool _interruptActive;
        bool _interruptEnabled;

        // Current port addresses
        ushort _portAddressA;
        ushort _portAddressB;
        ushort _byteCounter;
    }
}
