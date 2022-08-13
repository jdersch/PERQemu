//
// Z80CTC.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// Implements the Zilog Z8430 CTC counter/timer chip (aka Mostek MK3882)
    /// used in the PERQ-1 IOB.
    /// </summary>
    public class Z80CTC : IZ80Device
    {
        public Z80CTC(byte baseAddress, Scheduler scheduler)
        {
            _scheduler = scheduler;
            _baseAddress = baseAddress;

            _ports = new byte[] {
                       _baseAddress,
                (byte)(_baseAddress + 1),
                (byte)(_baseAddress + 2),
                (byte)(_baseAddress + 3)
            };

            _channels = new Channel[] {
                new Channel(0, this),
                new Channel(1, this),
                new Channel(2, this),
                new Channel(3, this)
            };
        }

        /// <summary>
        /// Hardware reset of the CTC, all channels.
        /// </summary>
        public void Reset()
        {
            for (int i = 0; i < 4; i++)
                _channels[i].Reset();

            _interruptVector = null;
            _interruptActive = false;

            Log.Debug(Category.CTC, "Reset");
        }

        public string Name => "Z80 CTC";
        public byte[] Ports => _ports;

        public bool IntLineIsActive => _interruptActive;
        public byte? ValueOnDataBus => AcknowledgeInterrupt();

        public event EventHandler NmiInterruptPulse;


        public void AttachDevice(int channel, ICTCDevice dev)
        {
            _channels[channel].TimerClient = dev;
        }

        public byte Read(byte portAddress)
        {
            throw new NotImplementedException("Z80 CTC read");
        }

        public void Write(byte portAddress, byte value)
        {
            int ch = (portAddress - _baseAddress);

            if (_channels[ch].Control.HasFlag(ControlFlags.TimeConstant))
            {
                Log.Detail(Category.CTC, "Channel {0} loading TC {1}", ch, value);

                // TimeConstant bit was set in the last control byte,
                // so this byte sets the time constant for this channel
                _channels[ch].TimeConstant = (value == 0) ? 256 : value;

                // Reset the counter w/new time constant
                _channels[ch].ResetCounter();

                // Clear the time constant flag
                _channels[ch].Control &= ~ControlFlags.TimeConstant;

                // Start the timer if the conditions are right
                _channels[ch].Start();
            }
            else
            {
                ControlFlags control = (ControlFlags)value;
                Log.Detail(Category.CTC, "Control word: {0}", control);

                if (control.HasFlag(ControlFlags.ControlOrVector))
                {
                    // Control word
                    _channels[ch].Control = control;

                    if (control.HasFlag(ControlFlags.Reset))
                    {
                        _channels[ch].Stop();
                    }
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

        /// <summary>
        /// Clock the specified channel.  Assert the interrupt if the counter
        /// has reached zero.
        /// </summary>
        public int Clock(int channel)
        {
            var count = _channels[channel].Clock();

            Log.Detail(Category.CTC, "Clocked channel {0}, count now {1}", channel, count);

            return count;
        }

        /// <summary>
        /// Raises the CTC interrupt flag and sets the vector if one or more
        /// channels are enabled and requesting service; otherwise, clears it.
        /// </summary>
        /// <remarks>
        /// The individual channels request a rescan when one of their interrupt
        /// conditions is asserted (or cleared).  The scan is in priority order.
        /// </remarks>
        public void AssertInterrupt()
        {
            // Check all the channels
            for (var ch = 0; ch < _channels.Length; ch++)
            {
                if (_channels[ch].InterruptRequested && _channels[ch].Control.HasFlag(ControlFlags.Interrupt))
                {
                    _interruptVector = (byte)(_interruptVectorBase + ch * 2);
                    _interruptActive = true;
                    Log.Debug(Category.CTC, "Interrupt raised (chan={0})", ch);
                    return;
                }
            }

            // Nobody's home
            _interruptVector = null;
            _interruptActive = false;
        }

        /// <summary>
        /// Acknowledge and clear our interrupt flag, if appropriate.
        /// </summary>
        /// <remarks>
        /// If the Z80 is asking for our interrupt vector, it's the only way we
        /// can (easily) acknowledge that our interrupt is going to be serviced.
        /// </remarks>
        public byte? AcknowledgeInterrupt()
        {
            // Save the vector address that we're going to return for the
            // interrupt that's being serviced
            var vector = _interruptVector;

            // Now clear the interrupt request flag for the channel
            // (the Z80 has already fetched it)
            var ch = (vector != null) ? ((int)vector - _interruptVectorBase) / 2 : 0;

#if DEBUG
            // Sanity checks
            if (_interruptVector == null)
                Log.Warn(Category.CTC, "Z80 asked for vector and we haven't set one!?");

            if (ch < 0 || ch > 3)
                Log.Error(Category.CTC, "Vector {0} out of range on acknowledge", ch);
#endif

            _channels[ch].InterruptRequested = false;
            Log.Debug(Category.CTC, "Interrupt acknowledged (chan={0})", ch);

            return vector;
        }

        protected Scheduler _scheduler;
        private Channel[] _channels;

        private bool _interruptActive;

        private byte _interruptVectorBase;
        private byte? _interruptVector;

        private byte _baseAddress;
        private byte[] _ports;


        [Flags]
        internal enum ControlFlags
        {
            ControlOrVector = 0x01,
            Reset = 0x02,
            TimeConstant = 0x04,
            TimerTrigger = 0x08,
            EdgeSelection = 0x10,
            Prescaler = 0x20,
            CounterMode = 0x40,
            Interrupt = 0x80,
        }

        /// <summary>
        /// One independent channel in the CTC chip.
        /// </summary>
        internal class Channel
        {
            public Channel(int num, Z80CTC parent)
            {
                _ctc = parent;
                Number = num;
                TimerClient = null;

                Log.Debug(Category.CTC, "Channel {0} initialized", Number);
            }

            public void Reset()
            {
                Control = 0;
                TimeConstant = 1;
                Counter = 0;
                Running = false;
                InterruptRequested = false;

                Trigger = null;

                Log.Debug(Category.CTC, "Channel {0} reset", Number);
            }

            public ControlFlags Control;
            public int Number;
            public int TimeConstant;
            public int Counter;
            public bool Running;
            public bool InterruptRequested;

            public ICTCDevice TimerClient;

            private SchedulerEvent Trigger;
            private Z80CTC _ctc;


            /// <summary>
            /// Enable this channel as appropriate for the programmed mode.
            /// </summary>
            public void Start(bool pulse = false)
            {
                if (!Running)
                {
                    // In Counter mode?
                    if (Control.HasFlag(ControlFlags.CounterMode))
                    {
                        Running = true;
                    }
                    else
                    {
                        // Depending on the trigger mode, start automatically on
                        // Time Constant reload, or when external start pulse is applied
                        if ((pulse && Control.HasFlag(ControlFlags.TimerTrigger)) ||
                                     !Control.HasFlag(ControlFlags.TimerTrigger))
                        {
                            Running = true;
                            QueueTimerTick();
                        }
                    }

                    Log.Debug(Category.CTC, "Channel {0} started (running={1})", Number, Running);
                }
            }

            /// <summary>
            /// Apply an external clock pulse to this channel.  If in counter
            /// mode, decrement the counter.  In timer mode, start the timer
            /// (if programmed to start on external trigger).
            /// </summary>
            public int Clock()
            {
                if (Control.HasFlag(ControlFlags.CounterMode))
                {
                    if (Running)
                    {
                        if (Counter > 0) Counter--;

                        // Reached TC?  Request an interrupt (if configured)
                        InterruptRequested = (Counter == 0);

                        if (InterruptRequested) _ctc.AssertInterrupt();
                    }
                }
                else
                {
                    if (!Running) Start(true);
                }

                return Counter;
            }

            /// <summary>
            /// Soft reset the channel, and cancel any pending interrupt
            /// request or timer event.
            /// </summary>
            public void Stop()
            {
                if (Running)
                {
                    Running = false;
                    InterruptRequested = false;
                    _ctc.AssertInterrupt();

                    _ctc._scheduler.Cancel(Trigger);
                    Trigger = null;

                    Log.Debug(Category.CTC, "Channel {0} stopped", Number);
                }
            }

            /// <summary>
            /// Set the Counter value based on our current mode.
            /// </summary>
            public void ResetCounter()
            {
                // Counting or timing
                Counter = TimeConstant;

                // Apply the prescaler value if in Timer mode
                if (!Control.HasFlag(ControlFlags.CounterMode))
                {
                    Counter *= (Control.HasFlag(ControlFlags.Prescaler) ? 256 : 16);
                }

                // If we had previously asserted an interrupt, clear it?
                InterruptRequested = false;
                _ctc.AssertInterrupt();

                Log.Debug(Category.CTC, "Channel {0} counter reset ({1})", Number, Counter);
            }

            /// <summary>
            /// In Timer mode, schedule the event to fire when elapsed.
            /// </summary>
            private void QueueTimerTick()
            {
                if (Control.HasFlag(ControlFlags.CounterMode))
                {
                    Log.Warn(Category.CTC, "Ignoring timer tick for counter {0}", Number);
                    return;
                }

                //
                // For the IOB:  CLK is Z_CLK (2.4576Mhz) or ~407ns
                //      ZC/TO0 is the RS-232 baud rate clock (defaults to 4800?) ~13us
                //      ZC/TO1 is the 32KHz speech clock (schem says 16KHz, sigh) ~32us
                //      ZC/TO2 feeds a series of flipflops that combine with a 500KHz
                //              clock to form the disk stepper pulses (96KHz)
                //      CLK3 is TAB STAT, also derived from the 500KHz clock and
                //              possibly unused?  (Old touch tablet interface)
                // Thus, the min/max timer values at 2.4576Mhz are ~6.5uS to 26.6ms.
                // At 9600 baud, the CTC runs at its maximum rate to produce the SIO/0
                // clock (on channel 0), which is programmed in x16 mode.  Thus, the
                // SIO bit rate is 104.192usec @ 9600baud, or 1.042ms per char.
                //
                var interval = _ctc._scheduler.TimeStepNsec * (ulong)Counter;

                Trigger = _ctc._scheduler.Schedule(interval, TimerTickCallback, Number);

                Log.Debug(Category.CTC, "Channel {0} timer scheduled ({1}) at {2}",
                                        Number, interval, _ctc._scheduler.CurrentTimeNsec);
            }

            /// <summary>
            /// Timers run until stopped by a reset.  Tell the CTC to fire an
            /// interrupt, then reset and reschedule.
            /// </summary>
            private void TimerTickCallback(ulong skewNsec, object context)
            {
                Log.Debug(Category.CTC, "Channel {0} timer callback fired at {1}",
                                        context, _ctc._scheduler.CurrentTimeNsec);

                var oldIntr = InterruptRequested;

                if (Running && Control.HasFlag(ControlFlags.Interrupt))
                {
                    // Poke the Z80
                    InterruptRequested = true;

                    // Schedule next tick
                    QueueTimerTick();
                }
                else
                {
                    // If not running or interrupts are disabled, don't bother to reschedule
                    // This is an efficiency measure; the CTC on the IOB generates bit clocks
                    // for RS-232 and speech on channels 0 & 1; they don't interrupt and our
                    // simulated devices aren't clocking actual bit streams fercryinoutloud.
                    Trigger = null;
                    InterruptRequested = false;
                    Log.Debug(Category.CTC, "[Channel {0} timer not renewed]", context);

                    // HOWEVER, for baud rate generation we DO want to inform the "real"
                    // device (if registered) that the timer's rate has changed/been enabled
                    // so that it can compute and set a baud rate for the external device...
                    if (TimerClient != null)
                    {
                        TimerClient.NotifyRateChange(Counter);
                    }
                }

                // Rescan if status changed
                if (InterruptRequested != oldIntr)
                    _ctc.AssertInterrupt();
            }
        }
    }
}
