//
// Z80SIO.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// An evolving implementation of the Z80 SIO serial controller, providing
    /// the operational modes that the PERQ IOB makes use of.  Each channel may
    /// have a basic ISIODevice attached (for pseudo devices not backed by a
    /// physical port) or an ISerialDevice (a superset that provides hooks for
    /// mapping register configuration to real hardware on the host).  Still a
    /// work in progress.
    /// </summary>
    public partial class Z80SIO : IZ80Device, IDMADevice
    {
        public Z80SIO(byte baseAddress, Scheduler scheduler, bool isEio = false)
        {
            _baseAddress = baseAddress;
            _ports = new byte[] {
                                    baseAddress,
                                    (byte)(baseAddress + 1),
                                    (byte)(baseAddress + 2),
                                    (byte)(baseAddress + 3)
                                };

            _channels = new Channel[2];
            _channels[0] = new Channel(0, scheduler);
            _channels[1] = new Channel(1, scheduler);

            _isEIO = isEio;
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
                // If an interrupt is pending on A, use its offset; otherwise
                // assume that B is interrupting...
                var priority = (_channels[0].InterruptLatched ? _channels[0].InterruptOffset + 4 :
                                                                _channels[1].InterruptOffset);

                // Now hack the offset into vector<3:1>
                vector = (byte)((vector & 0xf1) | ((priority & 0x07) << 1));
                Log.Detail(Category.SIO, "Status Affected Vector is 0x{0:x2} (prio={1})", vector, priority);
            }

            return vector;
        }

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        //
        // IDMADevice implementation
        //
        public bool ReadDataReady => true;
        public bool WriteDataReady => true;

        public void DMATerminate()
        {
            // This will likely be relevant for Speech?
        }


        public void AttachDevice(int channel, ISIODevice device)
        {
            if (channel < 0 || channel > 1)
            {
                throw new ArgumentOutOfRangeException(nameof(channel));
            }

            _channels[channel].AttachDevice(device);
        }

        public void AttachPortDevice(int channel, SerialDevice device)
        {
            AttachDevice(channel, device);
            _channels[channel].OpenPort(device);
        }

        public void DetachDevice(int channel)
        {
            _channels[channel].DetachDevice();
        }

        /// <summary>
        /// Read from a chip register.  For EIO, they scrambled the bits when
        /// the second SIO was added; the decoder puts data on 0+1, ctrl on 2+3.
        /// This means ports base+1 and base+2 are swapped.  SIGH.
        /// </summary>
        public byte Read(byte portAddress)
        {
            switch (portAddress - _baseAddress)
            {
                case 0:
                    return _channels[0].ReadData();

                case 1:
                    if (_isEIO)
                        return _channels[1].ReadData();
                    
                    return _channels[0].ReadRegister();

                case 2:
                    if (_isEIO)
                        return _channels[0].ReadRegister();
                    
                    return _channels[1].ReadData();

                case 3:
                    return _channels[1].ReadRegister();

                default:
                    throw new InvalidOperationException("Invalid SIO port address");
            }
        }

        /// <summary>
        /// Write to a register.  Same decoder screwiness as above for EIO.
        /// </summary>
        public void Write(byte portAddress, byte value)
        {

            switch (portAddress - _baseAddress)
            {
                case 0:
                    _channels[0].WriteData(value);
                    break;

                case 1:
                    if (_isEIO)
                        _channels[1].WriteData(value);
                    else
                        _channels[0].WriteRegister(value);
                    break;

                case 2:
                    if (_isEIO)
                        _channels[0].WriteRegister(value);
                    else
                        _channels[1].WriteData(value);
                    break;

                case 3:
                    _channels[1].WriteRegister(value);
                    break;

                default:
                    throw new InvalidOperationException("Invalid SIO port address");
            }
        }

        public void DumpPortStatus(int chan)
        {
            _channels[chan].Port?.Status();
        }


        Channel[] _channels;

        byte _baseAddress;
        byte[] _ports;
        bool _isEIO;
    }
}
