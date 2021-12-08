//
// PERQFifo.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Concurrent;
using System.Collections.Generic;

using PERQemu.Processor;


namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Output TO the PERQ, FROM the Z80.
    /// </summary>
    public class Z80ToPERQFIFO : IZ80Device
    {
        public Z80ToPERQFIFO(PERQSystem system)
        {
            _system = system;
            //_lock = new ReaderWriterLockSlim(LockRecursionPolicy.SupportsRecursion);
            Reset();
        }

        public void Reset()
        {
            //_lock.EnterWriteLock();
            //_fifo = new Queue<byte>();
            //_lock.ExitWriteLock();
            _fifo = new ConcurrentQueue<byte>();
        }

        public byte Dequeue()
        {
            //_lock.EnterUpgradeableReadLock();
            byte value = 0;
            if (_fifo.Count > 0)
            {
                //_lock.EnterWriteLock();
                var ok = _fifo.TryDequeue(out value);
                //_lock.ExitWriteLock();

                Trace.Log(LogType.Z80FIFO, "Z80->PERQ FIFO read {0:x2}, {1:x4} items left in queue.",
                                            value, _fifo.Count);
            }
            else
            {
                Trace.Log(LogType.Warnings, "Z80->PERQ read from empty FIFO, returning 0.");
            }

            if (_fifo.Count == 0)
            {
                // No data left, clear the PERQ interrupt.
                _system.CPU.ClearInterrupt(InterruptSource.Z80DataOut);
            }

            //_lock.ExitUpgradeableReadLock();

            return value;
        }

        public string Name => "Z80->PERQ FIFO";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => false;    // Never interrupts

        public byte? ValueOnDataBus => null;     // As above

        public event EventHandler NmiInterruptPulse;

        public bool IsReady => true;            // For now we provide an "infinite" FIFO

        public byte Read(byte portAddress)
        {
            // Should never happen, this FIFO is write-only from the Z80 side
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            //_lock.EnterWriteLock();
            _fifo.Enqueue(value);
            //_lock.ExitWriteLock();

            Trace.Log(LogType.Z80FIFO, "Z80->PERQ FIFO enqueued byte {0:x2}, {1} items in queue.",
                                        value, _fifo.Count);

            // Since there's data available, let the PERQ know
            _system.CPU.RaiseInterrupt(InterruptSource.Z80DataOut);
        }

        private PERQSystem _system;
        // private Queue<byte> _fifo;
        private ConcurrentQueue<byte> _fifo;

        private byte[] _ports =
        {
            0xd0            // PERQW
        };

        // TODO: investigate using something more lightweight?
        //private ReaderWriterLockSlim _lock;
    }

    /// <summary>
    /// Input TO the Z80, FROM the PERQ
    /// </summary>
    public class PERQToZ80FIFO : IZ80Device
    {
        public PERQToZ80FIFO(PERQSystem system)
        {
            _system = system;
            //_lock = new ReaderWriterLockSlim(LockRecursionPolicy.SupportsRecursion);
            Reset();
        }

        public void Reset()
        {
            //_lock.EnterWriteLock();
            //_fifo = new Queue<byte>();
            _fifo = new ConcurrentQueue<byte>();
            _interruptsEnabled = false;
            _interruptActive = false;
            //_lock.ExitWriteLock();
        }       

        public string Name => "PERQ->Z80 FIFO";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => _interruptActive & _interruptsEnabled;

        public byte? ValueOnDataBus => 0x20; // PRQVEC

        public bool InterruptsEnabled 
        { 
            get { return _interruptsEnabled; }
            set { _interruptsEnabled = value; }
        }

        public event EventHandler NmiInterruptPulse;

        public void SetDataReadyInterruptRequested(bool requested)
        {
            //_lock.EnterWriteLock();
            _dataReadyInterruptRequested = requested;
            //_lock.ExitWriteLock();
        }

        public void Enqueue(byte value)
        {
            //_lock.EnterWriteLock();
            _fifo.Enqueue(value);

            // Interrupt the Z80 to let it know we have data to be read
            _interruptActive = true;

            Trace.Log(LogType.Z80FIFO, "PERQ->Z80 FIFO enqueued byte {0:x2}, {1} items in queue.",
                                        value, _fifo.Count);
            //_lock.ExitWriteLock();
        }

        public byte Read(byte portAddress)
        {
            byte value = 0;

            //_lock.EnterWriteLock();
            //if (_fifo.Count == 0)
            //{
            if (!_fifo.TryDequeue(out value))
            {
                Trace.Log(LogType.Z80FIFO, "PERQ->Z80 FIFO read from empty fifo (int active {0}), returning 0.",
                                            _interruptActive);
                //_lock.ExitWriteLock();
                return 0;
            }

            Trace.Log(LogType.Z80FIFO, "PERQ->Z80 FIFO dequeued byte {0:x2}, {1} items left in queue.",
                                        value, _fifo.Count);

            //
            // If the input FIFO is empty, we will interrupt if the PERQ has
            // asked us to.  We will also clear the Z80 interrupt since there's
            // no data left to be consumed by the Z80.
            //
            if (_fifo.Count == 0)
            {
                if (_dataReadyInterruptRequested)
                {
                    _system.CPU.RaiseInterrupt(InterruptSource.Z80DataIn);
                }
                _interruptActive = false;
            }
            //_lock.ExitWriteLock();

            return value;
        }

        public void Write(byte portAddress, byte value)
        {
            // Should never get called, this FIFO is read-only from the Z80 side.
            // If it does, we should yell about it.
            throw new NotImplementedException();
        }

        private PERQSystem _system;
        //private Queue<byte> _fifo;
        private ConcurrentQueue<byte> _fifo;
        private bool _interruptsEnabled;
        private bool _interruptActive;
        private bool _dataReadyInterruptRequested;

        // TODO: investigate using something more lightweight?
        //private ReaderWriterLockSlim _lock;

        private byte[] _ports =
        {
            0xa0            // PERQR
        };
    }
}
