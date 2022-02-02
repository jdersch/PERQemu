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
using System.Collections.Concurrent;

using PERQemu.Processor;

namespace PERQemu.IO.Z80
{

    // TODO/FIXME:  Damn.  The PERQ-1 IOB has just a single byte latch in each
    // direction ('374s), while the PERQ-2 EIO has an actual 16x10 (two '225s)
    // FIFO.  We could possibly make the PERQ-1 implementation more efficient
    // by not using a Queue (perhaps use an Interlocked scalar instead) or just
    // limit the depth to more accurately simulate the hardware.  Both the Z80
    // and the microcode check for space in the FIFOs before writing to them so
    // the unlimited depth might be a small performance enhancement but it also
    // might throw off some of the timing -- if the perq ignores a boot sector
    // request, the Z80 has already queued up 128 bytes that have to be tossed!
    // Hmm.  Setting IsReady to simulate the one byte FIFO is a logging nightmare
    // (as the Z80 spins and spins waiting to write each byte) but it is more
    // accurate and realistic.  Again I say: Hmm...


    /// <summary>
    /// Output TO the PERQ, FROM the Z80.
    /// </summary>
    public class Z80ToPERQFIFO : IZ80Device
    {
        public Z80ToPERQFIFO(PERQSystem system)
        {
            _system = system;
            _fifo = new ConcurrentQueue<byte>();
        }

        public void Reset()
        {
            byte dummy;

            // Clear the fifo
            while (_fifo.TryDequeue(out dummy)) { }

            // Dismiss the interrupt
            _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);

            Log.Debug(Category.FIFO, "Z80->PERQ FIFO reset");
        }

        public string Name => "Z80->PERQ FIFO";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => null;
        public bool IntLineIsActive => false;           // Never interrupts
        public bool IsReady => (_fifo.Count == 0);      // Accurate... but slow

        public event EventHandler NmiInterruptPulse;

        public byte Dequeue()
        {
            byte value = 0;

            if (_fifo.TryDequeue(out value))
            {
                Log.Debug(Category.FIFO, "Z80->PERQ read {0:x2}, {1} items left in queue",
                                            value, _fifo.Count);
            }
            else
            {
                Log.Debug(Category.FIFO, "Z80->PERQ read from empty FIFO, returning 0");
            }

            if (_fifo.Count == 0)
            {
                // No data left, clear the PERQ interrupt.
                _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);
            }

            return value;
        }

        public byte Read(byte portAddress)
        {
            // Should never happen, this FIFO is write-only from the Z80 side
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            _fifo.Enqueue(value);

            Log.Debug(Category.FIFO, "Z80->PERQ enqueued byte {0:x2}, {1} items in queue",
                                        value, _fifo.Count);

            // Since there's data available, let the PERQ know
            _system.CPU.RaiseInterrupt(InterruptSource.Z80DataOut);
        }

        private byte[] _ports = { 0xd0 };       // PERQW

        private ConcurrentQueue<byte> _fifo;
        private PERQSystem _system;
    }


    /// <summary>
    /// Input TO the Z80, FROM the PERQ
    /// </summary>
    public class PERQToZ80FIFO : IZ80Device
    {
        public PERQToZ80FIFO(PERQSystem system)
        {
            _system = system;
            _fifo = new ConcurrentQueue<byte>();
        }

        public void Reset()
        {
            byte dummy;

            // Clear the fifo
            while (_fifo.TryDequeue(out dummy)) { }

            _interruptsEnabled = false;
            _interruptActive = false;

            Log.Debug(Category.FIFO, "PERQ->Z80 FIFO reset");
        }

        public string Name => "PERQ->Z80 FIFO";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => 0x20;    // PRQVEC
        public bool IntLineIsActive => _interruptActive & _interruptsEnabled;

        public bool InterruptsEnabled
        {
            get { return _interruptsEnabled; }
            set { _interruptsEnabled = value; }
        }

        public event EventHandler NmiInterruptPulse;

        public void SetDataReadyInterruptRequested(bool requested)
        {
            _dataReadyInterruptRequested = requested;
        }

        public void Enqueue(byte value)
        {
            _fifo.Enqueue(value);

            // Interrupt the Z80 to let it know we have data to be read
            _interruptActive = true;

            Log.Debug(Category.FIFO, "PERQ->Z80 enqueued byte {0:x2}, {1} items in queue",
                                        value, _fifo.Count);
        }

        public byte Read(byte portAddress)
        {
            byte value = 0;

            if (!_fifo.TryDequeue(out value))
            {
                Log.Debug(Category.FIFO, "PERQ->Z80 read from empty fifo (int active {0}), returning 0",
                                            _interruptActive);
            }
            else
            {
                Log.Debug(Category.FIFO, "PERQ->Z80 dequeued byte {0:x2}, {1} items left in queue",
                                            value, _fifo.Count);
            }

            // If the input FIFO is empty, we will interrupt if the PERQ has
            // asked us to.  We will also clear the Z80 interrupt since there's
            // no data left to be consumed by the Z80.
            if (_fifo.Count == 0)
            {
                if (_dataReadyInterruptRequested)
                {
                    _system.CPU.RaiseInterrupt(InterruptSource.Z80DataIn);
                }

                _interruptActive = false;
            }

            return value;
        }

        public void Write(byte portAddress, byte value)
        {
            // Should never get called, this FIFO is read-only from the Z80 side.
            // If it does, we should yell about it.
            throw new NotImplementedException();
        }


        private bool _interruptsEnabled;
        private bool _interruptActive;
        private bool _dataReadyInterruptRequested;

        private byte[] _ports = { 0xa0 };   // PERQR

        private ConcurrentQueue<byte> _fifo;
        private PERQSystem _system;
    }
}
