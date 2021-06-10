using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    public class Keyboard : IZ80Device, IKeyboard
    {
        public Keyboard()
        {
            Reset();
        }

        public void Reset()
        {
            _lastKeycode = 0;
            _interruptsEnabled = false;
            _interruptActive = false;
        }

        public string Name => "Keyboard";

        public byte[] Ports => _ports;

        public bool IntLineIsActive => _interruptActive;

        public byte? ValueOnDataBus => 0x28; //KBDVEC

        public bool InterruptsEnabled 
        { 
            get => _interruptsEnabled; 
            set => _interruptsEnabled = value; 
        }

        public event EventHandler NmiInterruptPulse;

        public void QueueInput(byte key)
        {
            _lastKeycode = key;
            _interruptActive = true; // _interruptsEnabled;
        }

        public byte Read(byte portAddress)
        {
            _interruptActive = false;
            return _lastKeycode;
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException();
        }

        private byte _lastKeycode;

        private bool _interruptsEnabled;
        private bool _interruptActive;

        private byte[] _ports = new byte[] { 0x80 };
    }
}
