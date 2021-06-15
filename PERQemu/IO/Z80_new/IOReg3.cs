using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    /// <summary>
    /// Implements I/O Register 3, a single byte I/O device connected to the Z80
    /// that is used to control DMA and interrupts for various IOB devices.
    /// </summary>
    public class IOReg3 : IZ80Device
    {
        public IOReg3(PERQToZ80FIFO perqZ80fifo, Keyboard keyboard, NECuPD765A fdc)
        {
            _perqZ80fifo = perqZ80fifo;
            _keyboard = keyboard;
            _fdc = fdc;
        }

        public void Reset()
        {

        }

        public string Name  => "I/O REG 3";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => false;

        public byte? ValueOnDataBus =>  null;

        public bool InterruptsEnabled
        {
            get => false;
            set { }
        }

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            //
            // TODO (once we implement DMA):
            // hook up DMA controls here
            //

            //
            // Dole out Interrupt enables here:
            //
            // From v87.z80:
            //
            // PRQENB  EQU     4  ;PERQ INTERRUPT ENABLE
            // KBDENB  EQU     2  ;KBD INTERRUPT ENABLE
            // FLPENB  EQU     1  ;FLOPPY INTERRUPT ENABLE
            //
            _perqZ80fifo.InterruptsEnabled = ((value & 0x04) != 0);
            _keyboard.InterruptsEnabled = ((value & 0x02) != 0);
            _fdc.InterruptsEnabled = ((value & 0x01) != 0);
        }

        private byte[] _ports = new byte[] { 0xc8 };

        private PERQToZ80FIFO _perqZ80fifo;
        private Keyboard _keyboard;
        private NECuPD765A _fdc;
    }
}
