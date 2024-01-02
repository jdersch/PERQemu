//
// PERQToZ80Latch.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Processor;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Input TO the Z80, FROM the PERQ.
    /// </summary>
    /// <remarks>
    /// For the original I/O board (IOB, CIO) this is a single 8-bit latch.  If
    /// requested by the PERQ, raises the Z80DataIn interrupt when the "fifo" is
    /// free and can accept new data.
    /// </remarks>
    public class PERQToZ80Latch : IZ80Device
    {
        public PERQToZ80Latch(PERQSystem system)
        {
            _system = system;
            _lock = new object();
            _interruptsEnabled = false;
        }

        public void Reset()
        {
            _latch = 0;
            _valid = false;

            // Assume these reset as well
            _interruptActive = false;
            _dataReadyInterruptRequested = false;

            _system.CPU.ClearInterrupt(InterruptSource.Z80DataIn);

            Log.Debug(Category.FIFO, "PERQ->Z80 latch reset");
        }

        public string Name => "PERQ->Z80 Latch";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x20;    // PRQVEC

        public bool IsReady
        {
            get { lock (_lock) { return _valid; } }
        }

        public bool IntLineIsActive
        {
            get { lock (_lock) { return (_interruptActive && _interruptsEnabled && _valid); } }
        }

        public bool InterruptsEnabled
        {
            get { return _interruptsEnabled; }
            set { _interruptsEnabled = value; }
        }

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        /// <summary>
        /// Writes a byte from the PERQ to the Z80.
        /// </summary>
        /// <remarks>
        /// The IOB schematic (p. 49 of the PDF, "IOA DECODE") sheds some light
        /// on the Z80 "input" interrupt.  This is actually labeled as "Z80 READY
        /// INT L" (meaning it's active Low) and seems to be enabled by the PERQ
        /// sending data with bit 8 high, and can be dismissed by the PERQ sending
        /// data with bit 8 low.
        ///
        /// IOD 8 is latched on a WRITE from the Z80 and gated with the PERQ->Z80
        /// REQ L signal at a NAND gate.  So -- if the PERQ sets IOD 8 high with a
        /// write, and there is no pending PERQ->Z80 request, Z80 READY INT L will
        /// be low (triggered).
        /// </remarks>
        public void Enqueue(int value)
        {
            lock (_lock)
            {
                // Always clear the DataInReady CPU interrupt
                _system.CPU.ClearInterrupt(InterruptSource.Z80DataIn);

                if (_valid)
                {
                    Log.Warn(Category.FIFO, "PERQ overran latch, byte 0x{0:x2} will be lost", _latch);
                }

                // Latch the 8th bit of the incoming word
                _dataReadyInterruptRequested = ((value & 0x100) != 0);

                // Queue the byte
                _latch = (byte)value;
                _valid = true;

                // Interrupt the Z80 to signal we have data to read
                _interruptActive = true;
            }

            Log.Detail(Category.FIFO, "PERQ wrote byte 0x{0:x2} to latch", value);
        }

        /// <summary>
        /// Reads a byte from the PERQ.
        /// </summary>
        /// <remarks>
        /// This is generally called in response to an interrupt.  Returns 0x0
        /// if the latch is empty (though the hardware would return whatever was
        /// previously latched); this condition should only happen when the PERQ
        /// restarts the Z80 and forces a read to clear the buffer.
        /// </remarks>
        public byte Read(byte portAddress)
        {
            byte value = 0;

            lock (_lock)
            {
                // Clear the Z80 interrupt
                _interruptActive = false;

                if (_valid)
                {
                    value = _latch;
                    _valid = false;
                }
                else
                {
                    Log.Warn(Category.FIFO, "Z80 read from empty latch, returning 0");
                }

                // FIFO is empty; interrupt if the PERQ has asked us to
                if (_dataReadyInterruptRequested)
                {
                    _system.CPU.RaiseInterrupt(InterruptSource.Z80DataIn);
                }
            }

            Log.Detail(Category.FIFO, "Z80 read byte 0x{0:x2} from latch", value);
            return value;
        }

        public void Write(byte portAddress, byte value)
        {
            // Should never get called, this FIFO is read-only from the Z80 side.
            // If it does, we should yell about it.
            throw new NotImplementedException("Z80 write to read-only latch");
        }

        // debug
        public void DumpFifo()
        {
            Console.WriteLine($"PERQ->Z80 Latch: 0x{_latch:x2} (valid={_valid})");
            Console.WriteLine($"PERQ->Z80 Latch: IRQ active={_interruptActive} enabled={_interruptsEnabled} requested={_dataReadyInterruptRequested}");
        }

        bool _interruptActive;
        bool _interruptsEnabled;
        bool _dataReadyInterruptRequested;

        byte _latch;
        bool _valid;
        readonly object _lock;

        byte[] _ports = { 0xa0 };   // PERQR

        PERQSystem _system;
    }
}
