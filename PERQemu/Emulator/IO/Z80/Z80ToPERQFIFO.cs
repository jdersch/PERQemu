//
// Z80ToPERQFIFO.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Concurrent;

using PERQemu.Processor;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Output TO the PERQ, FROM the Z80.
    /// </summary>
    /// <remarks>
    /// For the EIO board this is an actual 16 x 8 FIFO, much like the original
    /// implementation from a much earlier version of PERQemu.  It also includes
    /// the control register, a one bit "output ready" flip flop that the Z80
    /// uses to trigger the PERQ's "uPROC INT" interrupt line (Z80DataOut).
    /// </remarks>
    public class Z80ToPERQFIFO : IZ80Device
    {
        public Z80ToPERQFIFO(PERQSystem system)
        {
            _system = system;
            _fifo = new ConcurrentQueue<byte>();
            _interruptEnabled = false;
            _outputReady = false;
        }

        public void Reset()
        {
            byte punt;

            // Clear the queue
            while (!_fifo.IsEmpty) _fifo.TryDequeue(out punt);

            // Dismiss the interrupt?
            _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);
            _interruptEnabled = false;
            _outputReady = false;

            Log.Debug(Category.FIFO, "Z80->PERQ FIFO reset");
        }

        public string Name => "Z80->PERQ FIFO";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x12;        // TODO: PERQO.ISR ??
        public bool IntLineIsActive => false;       // But it never interrupts?

        public bool IsReady => _outputReady;
        public bool IsEmpty => _fifo.IsEmpty;

        public bool InterruptEnabled
        {
            get { return _interruptEnabled; }

            set
            {
                _interruptEnabled = value;

                // The PERQ can dismiss the interrupt even if there are still
                // bytes in the output queue!
                if (_outputReady && !_interruptEnabled)
                {
                    _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);
                }
            }
        }

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        //
        // To the PERQ
        //

        /// <summary>
        /// Read a byte from the Z80 output FIFO to the PERQ.
        /// </summary>
        public byte Dequeue()
        {
            byte value = 0;

            if (!_fifo.TryDequeue(out value))
            {
                Log.Warn(Category.FIFO, "PERQ read from empty FIFO, returning 0");
            }
            else
            {
                Log.Detail(Category.FIFO, "PERQ read byte 0x{0:x2} from FIFO ({1} bytes left)", value, _fifo.Count);
            }

            // If the FIFO is empty, drop the CPU interrupt?
            if (_fifo.IsEmpty)
            {
                _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);
            }

            return value;
        }

        //
        // Z80 Bus
        //

        public byte Read(byte portAddress)
        {
            // This FIFO is write-only from the Z80 side; this shouldn't happen
            throw new InvalidOperationException("Z80 read from write-only FIFO");
        }

        /// <summary>
        /// Handle writes from the Z80:
        ///     port 170Q (0x78) - control register to set the output ready bit
        ///     port 160Q (0x70) - data bytes into the output FIFO
        /// 
        /// (This avoids setting up another IZ80Device for a 1-bit flip flop...)
        /// </summary>
        public void Write(byte portAddress, byte value)
        {
            if (portAddress == 0x78)
            {
                // Control port: set or clear the output ready bit
                _outputReady = ((value & 0x1) != 0);
                Log.Debug(Category.FIFO, "Z80 output ready now {0}", _outputReady);

                // Fall through to fire the CPU interrupt if necessary
            }
            else
            {
                // Data port: queue the byte if there's room
                if (_fifo.Count > 16)
                {
                    Log.Warn(Category.FIFO, "Z80 overran FIFO, byte 0x{0:x2} will be lost", value);
                }
                else
                {
                    _fifo.Enqueue(value);
                    Log.Detail(Category.FIFO, "Z80 wrote byte 0x{0:x2} to FIFO ({1} bytes)", value, _fifo.Count);
                }
            }

            // Let the PERQ know there's data available
            if (_outputReady && _interruptEnabled && !_fifo.IsEmpty)
            {
                _system.CPU.RaiseInterrupt(InterruptSource.Z80DataOut);
            }
        }

        // debug
        public void DumpFifo()
        {
            Console.WriteLine($"Z80->PERQ FIFO: IRQ enabled={_interruptEnabled} ready={_outputReady}");

            Console.Write("Z80->PERQ FIFO: ");

            if (_fifo.IsEmpty)
            {
                Console.WriteLine("is empty.");
            }
            else
            {
                var contents = _fifo.ToArray();

                foreach (var b in contents) Console.Write($" 0x{b:x2}");
                Console.WriteLine();
            }
        }

        PERQSystem _system;
        ConcurrentQueue<byte> _fifo;

        bool _interruptEnabled;
        bool _outputReady;

        byte[] _ports = { 0x70, 0x78 };     // PERQ.OUT, output ready flip flop
    }
}
