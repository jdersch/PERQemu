//
// Z80CTC.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
            _channels.Initialize();

            _interruptEnabled = false;
            _interruptVector = null;

            Log.Debug(Category.CTC, "Reset.");
        }

        public string Name => "Z80 CTC";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => _interruptVector;
        public bool IntLineIsActive => _interruptEnabled;

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
                Log.Debug(Category.CTC, "Channel {0} loading TC {1}", ch, value);

                // TimeConstant bit was set in the last control byte,
                // so this byte sets the time constant for this channel.
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

                if ((control & ControlFlags.ControlOrVector) != 0)
                {
                    // Control word
                    _channels[ch].Control = control;

                    if ((control & ControlFlags.Reset) != 0)
                    {
                        _channels[ch].Stop();
                    }

                    // Check/reset the interrupt flag
                    RaiseInterrupt();
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
        /// Clock the specified channel.
        /// </summary>
        public void Clock(int channel)
        {
            if (_channels[channel].Clock() == 0)
            {
                RaiseInterrupt();
            }
        }

        /// <summary>
        /// Raises the CTC interrupt flag and sets the vector if one or more
        /// channels are enabled and requesting service.
        /// </summary>
        public void RaiseInterrupt()
        {
            // Check all the channels
            for (var ch = 0; ch < _channels.Length; ch++)
            {
                if (_channels[ch].InterruptRequested && (_channels[ch].Control & ControlFlags.Interrupt) != 0)
                {
                    _interruptEnabled = true;
                    _interruptVector = (byte)(_interruptVectorBase + ch * 2);
                    Log.Debug(Category.CTC, "Interrupt raised (chan={0})", ch);
                    return;
                }
            }

            if (_interruptEnabled) Log.Debug(Category.CTC, "Interrupt cleared");

            // Nobody's home
            _interruptEnabled = false;
            _interruptVector = null;
        }

        /// <summary>
        /// Clear an interrupt, somehow.  It doesn't seem the Z80 doesn't have
        /// any easy or obvious way to actually signal the acknowledgement and
        /// handling of an interrupt without actually snooping the instruction
        /// stream?  SERIOUSLY?  >:-(  This deal is getting worse all the time...
        /// </summary>
        public void ClearInterrupt()
        {
            // Someday we'll figure out how to do this.
        }

        protected Scheduler _scheduler;
        private Channel[] _channels;

        private bool _interruptEnabled = false;
        private byte _interruptVectorBase;
        private byte? _interruptVector = null;

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
            Mode = 0x40,
            Interrupt = 0x80,
        }

        /// <summary>
        /// One independent channel in the CTC chip.
        /// </summary>
        // TODO: see if the Intel chip used in the EIO can use this class too?
        // if they're similar enough, move it into its own file, add an iface, etc.
        internal class Channel
        {
            public Channel(int num, Z80CTC parent)
            {
                _ctc = parent;
                Number = num;

                Control = 0;
                TimeConstant = 1;
                Counter = 0;
                Running = false;
                InterruptRequested = false;

                Trigger = null;

                Log.Debug(Category.CTC, "Channel {0} initialized.", Number);
            }

            public ControlFlags Control;
            public int Number;
            public int TimeConstant;
            public int Counter;
            public bool Running;
            public bool InterruptRequested;

            private Event Trigger;
            private Z80CTC _ctc;

            /// <summary>
            /// Enable this channel as appropriate for the programmed mode.
            /// </summary>
            public void Start(bool pulse = false)
            {
                if (!Running)
                {
                    // In Timer mode?
                    if ((Control & ControlFlags.Mode) == 0)
                    {
                        // Start automatically on Time Constant reload, or when
                        // programmed for external start pulse is applied
                        if ((pulse && (Control & ControlFlags.TimerTrigger) != 0) ||
                                     ((Control & ControlFlags.TimerTrigger) == 0))
                        {
                            Running = true;
                            QueueTimerTick();
                        }
                    }
                    else
                    {
                        // Counter mode
                        Running = true;
                    }

                    Log.Debug(Category.CTC, "Channel {0} started (running={1})", Number, Running);
                }
            }

            /// <summary>
            /// Apply an external clock pulse to this channel.  If in counter
            /// mode, decrement the counter.  In timer mode, start the timer
            /// (if programmed to start on external trigger).
            /// </summary>
            /// <remarks>
            /// This will almost certainly have to be converted to a scheduled
            /// countdown, or the other peripherals will have to manually Clock
            /// their own counters... ugh.
            /// </remarks>
            public int Clock()
            {
                if ((Control & ControlFlags.Mode) != 0)
                {
                    if (Running) Counter--;     // Decrement counter
                }
                else
                {
                    if (!Running) Start(true);  // Start timer?
                }

                return Counter;
            }

            /// <summary>
            /// Soft reset the channel, and cancel any pending interrupt
            /// request or timer event.
            /// </summary>
            public void Stop()
            {
                Running = false;
                InterruptRequested = false;
                _ctc._scheduler.Cancel(Trigger);
                Log.Debug(Category.CTC, "Channel {0} stopped.", Number);
            }

            /// <summary>
            /// Set the Counter value based on our current mode.
            /// </summary>
            public void ResetCounter()
            {
                // Counting or timing
                Counter = TimeConstant;

                // Apply the prescaler value if in Timer mode
                if ((Control & ControlFlags.Mode) == 0)
                {
                    Counter *= (((Control & ControlFlags.Prescaler) != 0) ? 256 : 16);
                }

                Log.Debug(Category.CTC, "Channel {0} counter reset ({1})", Number, Counter);
            }

            /// <summary>
            /// In Timer mode, schedule the event to fire when elapsed.
            /// </summary>
            private void QueueTimerTick()
            {
                if ((Control & ControlFlags.Mode) != 0)
                {
                    Console.WriteLine("No timer for counter " + Number);
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
                // clock (on channel 0).  TODO: don't actually run the serial and speech
                // channels.  There's just no reason to generate BIT CLOCKS for simulated
                // serial ports.  Sigh.  At least they don't interrupt the Z80.  Just
                // focus on the hard disk seek/"IO REG 2" thing.
                //
                var interval = _ctc._scheduler.TimeStepNsec * (ulong)Counter;

#if DEBUG
                // DEBUG - sanity check
                if (interval < 6512 || interval > 26673152)
                {
                    Console.WriteLine("Bad CTC interval calculation: {0} (time const={1})",
                                      interval, Counter);

                    interval = Math.Max(interval, _ctc._scheduler.TimeStepNsec * 16);
                    interval = Math.Min(interval, _ctc._scheduler.TimeStepNsec * 256 * 256);
                }
#endif
                Trigger = _ctc._scheduler.Schedule(interval, Number, TimerTickCallback);

                //Log.Debug(Category.CTC, "Channel {0} timer scheduled ({1})", Number, interval);
            }

            /// <summary>
            /// Timers run until stopped by a reset.  Tell the CTC to fire an
            /// interrupt, then reset and reschedule.
            /// </summary>
            private void TimerTickCallback(ulong skewNsec, object context)
            {
                //Log.Debug(Category.CTC, "Channel {0} timer callback fired", context);

                if (Running && ((Control & ControlFlags.Interrupt) != 0))
                {
                    InterruptRequested = true;

                    // Tell mom
                    _ctc.RaiseInterrupt();

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
                    Log.Debug(Category.CTC, "Channel {0} timer not renewed.", context);
                }
            }
        }
    }
}

