using PERQemu.CPU;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    /// <summary>
    /// Output TO the PERQ, FROM the Z80.
    /// </summary>
    public class Z80ToPERQFIFO : IZ80Device
    {
        public Z80ToPERQFIFO(PERQSystem system)
        {
            _system = system;
        }

        public void Reset()
        {
            _fifo = new Queue<byte>();
        }

        public byte Dequeue()
        {
            byte value = 0;
            if (_fifo.Count > 0)
            {
                value = _fifo.Dequeue();

#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80->PERQ FIFO read {0:x2}, {1:x4} items left in queue.",
                                                value, _fifo.Count);
#endif
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings, "Z80->PERQ read from empty FIFO, returning 0.");
#endif
            }

            if (_fifo.Count == 0)
            {
                // No data left, clear the PERQ interrupt.
                _system.CPU.ClearInterrupt(InterruptType.Z80DataOutReady);
            }

            return value;
        }

        public string Name { get { return "Z80->PERQ FIFO"; } }

        public byte[] Ports { get { return _ports; } }

        public bool IntLineIsActive { get { return false; } }   // Never interrupts

        public byte? ValueOnDataBus { get { return null; } }    // As above

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            // Should never happen, this FIFO is write-only from the Z80 side.
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            _fifo.Enqueue(value);

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State,
                          "Z80->PERQ FIFO enqueued byte {0:x2}, {1} items in queue.",
                          value, _fifo.Count);
#endif

            // Since there's data available, let the PERQ know.
            _system.CPU.RaiseInterrupt(InterruptType.Z80DataOutReady);
        }

        private PERQSystem _system;
        private Queue<byte> _fifo;

        private byte[] _ports =
        {
            0xd0            // PERQW
        };
    }

    /// <summary>
    /// Input TO the Z80, FROM the PERQ
    /// </summary>
    public class PERQToZ80FIFO : IZ80Device
    {
        public PERQToZ80FIFO(PERQSystem system)
        {
            _system = system;
            Reset();
        }

        public void Reset()
        {
            _fifo = new Queue<byte>();
            _interruptEnabled = false;
        }

        public bool DataReadyInterruptRequested;

        public string Name { get { return "PERQ->Z80 FIFO"; } }

        public byte[] Ports { get { return _ports; } }

        public bool IntLineIsActive { get { return _interruptEnabled; } }

        public byte? ValueOnDataBus { get { return 0x20; } } // PRQVEC

        public event EventHandler NmiInterruptPulse;

        public void Enqueue(byte value)
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State,
                          "PERQ->Z80 FIFO enqueued byte {0:x2}, {1} items in queue.",
                          value, _fifo.Count);
#endif

            _fifo.Enqueue(value);

            // Interrupt the Z80 to let it know we have data to be read.
            _interruptEnabled = true;
        }

        public byte Read(byte portAddress)
        {
            if (_fifo.Count == 0)
            {
                // Hopefully this never happens.
                throw new InvalidOperationException("Z80 Read from empty PERQ->Z80 FIFO.");
            }

            byte value = _fifo.Dequeue();

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State,
                          "PERQ->Z80 FIFO dequeued byte {0:x2}, {1} items left in queue.",
                          value, _fifo.Count);
#endif

            //
            // If the input FIFO is empty, we will interrupt if the PERQ has asked us to.
            // We will also clear the Z80 interrupt since there's no data left to be consumed by
            // the Z80.
            //
            if (_fifo.Count == 0)
            {
                if (DataReadyInterruptRequested)
                {
                    _system.CPU.RaiseInterrupt(InterruptType.Z80DataInReady);
                }
                _interruptEnabled = false;
            }

            return value;
        }

        public void Write(byte portAddress, byte value)
        {
            // Should never get called, this FIFO is read-only from the Z80 side.
            // If it does, we should yell about it.
            throw new NotImplementedException();
        }

        private PERQSystem _system;
        private Queue<byte> _fifo;
        private bool _interruptEnabled;

        private byte[] _ports =
        {
            0xa0            // PERQR
        };
    }
}
