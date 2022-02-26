//
// PERQFifo.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Threading;

using PERQemu.Processor;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Output TO the PERQ, FROM the Z80.
    /// </summary>
    /// <remarks>
    /// For the original I/O board (IOB, CIO) this is a single 8-bit latch.
    /// We incorporate I/O REG 1 (status bit) here.  This class must be thread
    /// safe, so the _fifo (data latch) and _valid flag are protected by a lock.
    /// </remarks>
    public class Z80ToPERQFIFO : IZ80Device
    {
        public Z80ToPERQFIFO(PERQSystem system)
        {
            _system = system;
            _lock = new object();
        }

        public void Reset()
        {
            _fifo = 0;
            _valid = false;

            // Dismiss the interrupt
            _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);

            Log.Debug(Category.FIFO, "Z80->PERQ FIFO reset");
        }

        public string Name => "Z80->PERQ FIFO";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => null;
        public bool IntLineIsActive => false;       // Never interrupts

        public bool IsReady
        {
            get { lock (_lock) { return !_valid; } }
        }

        public event EventHandler NmiInterruptPulse;

        /// <summary>
        /// Interface to the PERQ side of the "FIFO" to read bytes from the Z80.
        /// </summary>
        public byte Dequeue()
        {
            byte value = 0;

            lock (_lock)
            {
                // Clear the PERQ interrupt on every read
                _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);

                if (_valid)
                {
                    value = _fifo;
                    _valid = false;

                    Log.Detail(Category.FIFO, "PERQ read byte 0x{0:x2} from latch", value);
                }
                else
                {
                    Log.Warn(Category.FIFO, "PERQ read from empty latch, returning 0");
                }
            }
            return value;
        }

        /// <summary>
        /// Reads the status of the latch, aka "I/O REG 1".
        /// </summary>
        public byte Read(byte portAddress)
        {
            return (byte)(IsReady ? 0x0 : 0x40);
        }

        /// <summary>
        /// Write a byte from the Z80 to the PERQ.
        /// </summary>
        public void Write(byte portAddress, byte value)
        {
            lock (_lock)
            {
                if (_valid)
                {
                    Log.Warn(Category.FIFO, "Z80 overran latch, byte 0x{0:x2} will be lost", _fifo);
                }

                _fifo = value;
                _valid = true;
            }

            // Let the PERQ know there's data available
            _system.CPU.RaiseInterrupt(InterruptSource.Z80DataOut);

            // Slow log moved outside the lock
            Log.Detail(Category.FIFO, "Z80 wrote byte 0x{0:x2} to latch", value);
        }


        private byte _fifo;
        private volatile bool _valid;
        private object _lock;

        private byte[] _ports = { 0x88, 0xd0 };     // IOReg1, PERQW

        private PERQSystem _system;
    }
}
