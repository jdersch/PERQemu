//
// HardDiskSeekControl.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
    /// AKA "IOREG2"
    /// </summary>
    public sealed class HardDiskSeekControl : IZ80Device
    {
        public HardDiskSeekControl(PERQSystem system)
        {
            _system = system;
            _stepClockEvent = null;
        }

        public void Reset()
        {
            if (_stepClockEvent != null)
            {
                _system.Scheduler.Cancel(_stepClockEvent);
                _stepClockEvent = null;

                Log.Debug(Category.HardDisk, "SeekControl reset");
            }
        }

        public string Name => "Shugart Seek Control";
        public byte[] Ports => _ports;
        public bool IntLineIsActive => false;
        public byte? ValueOnDataBus => null;

        public event EventHandler NmiInterruptPulse;


        public byte Read(byte portAddress)
        {
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            if (value != 0)
            {
                StepClockCallback(0, null);     // Go!
            }
        }

        /// <summary>
        /// Send a step pulse to the hard disk to move the heads.  The PERQ has
        /// set up the controller to seek in the proper direction and programmed
        /// the CTC with a cylinder count; we just clock the counter until it
        /// expires, then deschedule.  The CTC fires the completion interrupt.
        /// </summary>
        private void StepClockCallback(ulong skewNsec, object context)
        {
            // Step dem heads
            _system.IOB.DiskController.DoSingleSeek();

            // Clock the CTC and get back the count
            var leftToGo = _system.IOB.Z80System.CTC.Clock(Channel);

            if (leftToGo > 0)
            {
                Log.Debug(Category.HardDisk, "SeekControl scheduling step {0} in {1:n}ms",
                          leftToGo, StepTime * Conversion.NsecToMsec);
                _stepClockEvent = _system.Scheduler.Schedule(StepTime, StepClockCallback);
            }
            else
            {
                _stepClockEvent = null;
                Log.Debug(Category.HardDisk, "SeekControl count expired, descheduled");
            }
        }

        // The hardware runs on a 500KHz fixed clock source
        private readonly ulong StepTime = 500 * Conversion.UsecToNsec;

        // DiskStep wired to CTC channel 2
        private readonly int Channel = 2;

        private byte[] _ports = { 0xd8 };

        private SchedulerEvent _stepClockEvent;
        private PERQSystem _system;
    }
}
