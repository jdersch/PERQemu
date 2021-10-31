using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    public class Z80CTC : IZ80Device
    {
        public Z80CTC(byte baseAddress, Scheduler scheduler)
        {
            _baseAddress = baseAddress;
            _scheduler = scheduler;
            Reset();
        }

        public void Reset()
        {
            _interruptEnabled = false;
            _interruptVector = null;

            _channels = new Channel[4];
        }

        public string Name => "Z80 CTC";

        public byte[] Ports => new byte[] {
                    _baseAddress, (byte)(_baseAddress + 1),
                    (byte)(_baseAddress + 2), (byte)(_baseAddress + 3) };
         

        public bool IntLineIsActive => _interruptEnabled;

        public byte? ValueOnDataBus => _interruptVector;

        public event EventHandler NmiInterruptPulse;

        public byte Read(byte portAddress)
        {
            throw new NotImplementedException();
        }
        
        public void Write(byte portAddress, byte value)
        {
            int ch = (portAddress - _baseAddress);

            if ((_channels[ch].Control & ControlFlags.TimeConstant) != 0)
            {
                // TimeConstant bit was set in the last control byte,
                // so this byte sets the time constant for this channel.
                _channels[ch].TimeConstant = (value == 0) ? 256 : value;

                // Reset the counter w/new time constant
                ResetCounter(ch);

                // Clear the time constant flag
                _channels[ch].Control &= ~ControlFlags.TimeConstant;

                // TODO: technically should only start automatically if not running and trigger bit is set to zero
                if (!_channels[ch].Running)
                {
                    _channels[ch].Running = true;
                    QueueTimerTick(ch);
                }
            }
            else
            {
                ControlFlags control = (ControlFlags)value;
                if ((control & ControlFlags.ControlOrVector) != 0)
                {
                    // Control word
                    _channels[ch].Control = control;
                    if ((control & ControlFlags.Reset) != 0)
                    {
                        // Soft-reset.  Stop any executing timer
                        _channels[ch].Running = false;
                    }

                    // Clear interrupts if Interrupt flag in command is cleared.
                    _interruptEnabled = _interruptEnabled && ((control & ControlFlags.Interrupt) != 0);
                    
                }
                else
                {
                    // Set interrupt vector base if channel is 0, otherwise ignore (?)
                    if (ch == 0)
                    {
                        _interruptVectorBase = (byte)(value & 0xf8);
                    }
                }
            }
        }

        private void ResetCounter(int channel)
        {
            _channels[channel].Counter =
                    _channels[channel].TimeConstant * (((_channels[channel].Control & ControlFlags.Prescaler) != 0) ? 256 : 16);
        }

        private void QueueTimerTick(int channel)
        {
            // TODO: what's the clock rate here
            _scheduler.Schedule((ulong)(10.0 * (float)Conversion.UsecToNsec), channel, TimerTickCallback);
        }

        private void TimerTickCallback(ulong skewNsec, object context)
        {
            int channel = (int)context;

            if (!_channels[channel].Running)
            {
                // Timer was reset, do not continue.
                return;
            }

            // Decrement the counter, if we hit zero, fire an interrupt and restart the timer
            _channels[channel].Counter--;

            if (_channels[channel].Counter == 0)
            {
                if ((_channels[channel].Control & ControlFlags.Interrupt) != 0)
                {
                    _interruptEnabled = true;
                    _interruptVector = (byte)(_interruptVectorBase + channel * 2);
                }

                // Reset the counter
                ResetCounter(channel);
            }

            // Schedule next tick
            QueueTimerTick(channel);
        }

        private Scheduler _scheduler;
        private byte _baseAddress;

        private bool _interruptEnabled = false;
        private byte _interruptVectorBase;
        private byte? _interruptVector = null;

        [Flags]
        private enum ControlFlags
        {
            ControlOrVector = 0x01,
            Reset = 0x02,
            TimeConstant = 0x04,
            Trigger = 0x08,
            EdgeSelection = 0x10,
            Prescaler = 0x20,
            Mode = 0x40,
            Interrupt = 0x80,
        }


        private struct Channel
        {
            public ControlFlags Control;
            public int TimeConstant;
            public int Counter;
            public bool Running;
        }

        private Channel[] _channels;
    }
}
