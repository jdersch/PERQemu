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
                    Trace.Log(LogType.Z80FIFO, "Z80->PERQ FIFO read {0:x2}, {1:x4} items left in queue.",
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

        public string Name => "Z80->PERQ FIFO";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => false;    // Never interrupts

        public byte? ValueOnDataBus => null;     // As above

        public event EventHandler NmiInterruptPulse;

        public bool IsReady { get => true; }        // Always true for now since we provide an "infinite" FIFO.

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
                Trace.Log(LogType.Z80FIFO,
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
            _interruptsEnabled = false;
            _interruptActive = false;
        }

        public bool DataReadyInterruptRequested;

        public string Name => "PERQ->Z80 FIFO";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => _interruptActive & _interruptsEnabled;

        public byte? ValueOnDataBus => 0x20; // PRQVEC

        public bool InterruptsEnabled 
        { 
            get => _interruptsEnabled;
            set => _interruptsEnabled = value;
        }

        public event EventHandler NmiInterruptPulse;

        public void Enqueue(byte value)
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80FIFO,
                          "PERQ->Z80 FIFO enqueued byte {0:x2}, {1} items in queue.",
                          value, _fifo.Count);
#endif

            _fifo.Enqueue(value);

            // Interrupt the Z80 to let it know we have data to be read.
            _interruptActive = true;
        }

        public byte Read(byte portAddress)
        {
            if (_fifo.Count == 0)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80FIFO,
                              "PERQ->Z80 FIFO read from empty fifo (int active {0}), returning 0.", _interruptActive);
#endif
                return 0;
            }

            byte value = _fifo.Dequeue();

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80FIFO,
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

        private PERQSystem _system;
        private Queue<byte> _fifo;
        private bool _interruptsEnabled;
        private bool _interruptActive;

        private byte[] _ports =
        {
            0xa0            // PERQR
        };
    }
}
