using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    public class TMS9914A : IZ80Device
    {
        public TMS9914A()
        {

        }

        public void Reset()
        {
            
        }

        public string Name => "TMS9914A";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => _interruptActive;

        public byte? ValueOnDataBus => 0x22;    // GPIVEC

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            switch (portAddress)
            {
                case 0xb8:  // Interrupt Status 0:
                    return 0x10;        // BO set

                case 0xbb:  // Interrupt status 1:
                    return 0x01;        // IFC (idle state) set

                default:
                    return 0x00;
            }
        }

        public void Write(byte portAddress, byte value)
        {
            
        }

        private bool _interruptActive;

        private byte[] _ports = { 0xb8, 0xb9, 0xba, 0xbb, 0xbc, 0xbd, 0xbe, 0xbf };
    }
}
