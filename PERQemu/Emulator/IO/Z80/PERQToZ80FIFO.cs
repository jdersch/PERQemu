//
// PERQToZ80FIFO.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
    /// Input TO the Z80, FROM the PERQ.
    /// </summary>
    /// <remarks>
    /// The EIO provides a 16 x 8 FIFO for short data transfers from the CPU to
    /// the Z80.  This allows command packets to be sent efficiently, as the
    /// hardware automatically interrupts the Z80 when data is available to read,
    /// and the main processor can query the status register to see when the FIFO
    /// has been emptied and the Z80 is ready for more input.
    /// </remarks>
    public class PERQToZ80FIFO : IZ80Device
    {
        public PERQToZ80FIFO(PERQSystem system)
        {
            _system = system;
            _fifo = new ConcurrentQueue<byte>();
            _interruptEnabled = false;
        }

        public void Reset()
        {
            byte punt;

            // Clear the decks
            while (!_fifo.IsEmpty) _fifo.TryDequeue(out punt);

            // Assume this resets as well?
            _system.CPU.ClearInterrupt(InterruptSource.Z80DataIn);
            _interruptEnabled = false;

            Log.Debug(Category.FIFO, "PERQ->Z80 FIFO reset");
        }

        public string Name => "PERQ->Z80 FIFO";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x14;    // PERQI.ISR

        public bool IsReady => !_fifo.IsEmpty;
        public bool IntLineIsActive => _interruptEnabled && IsReady;

        public bool InterruptEnabled
        {
            get { return _interruptEnabled; }
            set { _interruptEnabled = value; }
        }

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        //
        // From the PERQ
        //

        /// <summary>
        /// Writes a byte from the PERQ to the Z80 input FIFO.
        /// </summary>
        public void Enqueue(int value)
        {
            // TODO: Always clear the DataInReady CPU interrupt?
            _system.CPU.ClearInterrupt(InterruptSource.Z80DataIn);

            if (_fifo.Count > 16)
            {
                Log.Warn(Category.FIFO, "PERQ overran FIFO, byte 0x{0:x2} will be lost", value);
                return;
            }

            // Queue the byte
            _fifo.Enqueue((byte)value);

            Log.Detail(Category.FIFO, "PERQ wrote byte 0x{0:x2} to FIFO ({1} bytes)", value, _fifo.Count);
        }

        //
        // Z80 Bus
        //

        /// <summary>
        /// Handle reads from the Z80:
        ///     port 162Q (0x72) - FIFO status register
        ///     port 161Q (0x71) - data byte from the input FIFO from the PERQ
        /// </summary>
        /// <remarks>
        /// The FIFO status bits read by the Z80 are _not quite_ the same as the
        /// status returned to the CPU via IOB status read:
        ///     bit 7 - set if there's room in the output FIFO
        ///     bit 6 - same as IOD<7> (write channel output ready)
        /// To access the Z80's output FIFO, we just read the status word from
        /// the public interface and shuffle bits around.  This is... not great.
        /// </remarks>
        public byte Read(byte portAddress)
        {
            // Status register?
            if (portAddress == 0x72)
            {
                // This is... lazy.  To access the other FIFO, just read the same
                // status word from the public interface that the PERQ reads from,
                // then shuffle the bits around.  I'm not proud of this.
                var status = _system.IOB.Z80System.ReadStatus();
                var result = (byte)((status & 0x8000) != 0 ? 0x40 : 0);
                result |= (byte)(_fifo.Count < 16 ? 0x20 : 0);

                Log.Debug(Category.FIFO, "Z80 read FIFO status 0x{0:x}", result);
                return result;
            }

            // Read a byte from the PERQ

            byte value = 0;

            if (!_fifo.TryDequeue(out value))
            {
                Log.Warn(Category.FIFO, "Z80 read from empty FIFO, returning 0");
            }
            else
            {
                Log.Detail(Category.FIFO, "Z80 read byte 0x{0:x2} from FIFO ({1} bytes left)", value, _fifo.Count);

                // Is the FIFO empty?  Then interrupt if the PERQ has asked us to
                if (_fifo.IsEmpty && _interruptEnabled)
                {
                    _system.CPU.RaiseInterrupt(InterruptSource.Z80DataIn);
                }
            }
            return value;
        }

        public void Write(byte portAddress, byte value)
        {
            // Should never get called, this FIFO is read-only from the Z80 side.
            // If it does, we should yell about it.
            throw new NotImplementedException("Z80 write to read-only FIFO");
        }

        // debug
        public void DumpFifo()
        {
            Console.WriteLine($"PERQ->Z80 FIFO: IRQ enabled={_interruptEnabled}");

            Console.Write("PERQ->Z80 FIFO: ");

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

        byte[] _ports = { 0x71, 0x72 };  // PERQ.IN, status register?
    }
}
