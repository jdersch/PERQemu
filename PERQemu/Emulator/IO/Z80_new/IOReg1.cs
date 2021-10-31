using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    /// <summary>
    /// Implements I/O Register 1, a single byte I/O device connected to the Z80
    /// that is used to test whether the Z80->PERQ FIFO is ready.
    /// 
    /// TODO: could this just be folded into Z80ToPERQFIFO?
    /// </summary>
    public class IOReg1 : IZ80Device
    {
        public IOReg1(Z80ToPERQFIFO z80PerqFifo)
        {
            _z80PerqFifo = z80PerqFifo;
        }

        public void Reset()
        {

        }

        public string Name => "I/O REG 1";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => false;

        public byte? ValueOnDataBus => null;

        public bool InterruptsEnabled
        {
            get => false;
            set { }
        }

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            return (byte)(_z80PerqFifo.IsReady ? 0x0 : 0x40);
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException();
        }

        private byte[] _ports = new byte[] { 0x88 };

        private Z80ToPERQFIFO _z80PerqFifo;
        
    }
}
