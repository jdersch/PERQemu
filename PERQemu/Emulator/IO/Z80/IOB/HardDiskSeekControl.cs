// harddiskseekcontrol.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System.Collections.Generic;

using PERQemu.IO.HardDisk;

namespace PERQemu.IO.Z80.IOB
{
    /// <summary>
    /// This is a stub class that provides an interface for the Z80 to perform
    /// seeks of the Shugart hard disk heads.  It seemed slightly more logical to
    /// pull that weird Z80 code out of the ShugartController itself.  Here we just
    /// handle the Z80 protocol bits in the same way as the other Z80 devices.
    /// </summary>
    public sealed class HardDiskSeekControl : IZ80Device
    {
        public HardDiskSeekControl(PERQSystem system)
        {
            _system = system;
            Reset();
        }

        public void Reset()
        {
            _busyClocks = 0;
            _seekInProgress = false;
        }

        public ReadyFlags BusyBit
        {
            get { return ReadyFlags.Seek; }
        }

        public int BusyClocks
        {
            get { return _busyClocks; }
            set { _busyClocks = value; }
        }

        /// <summary>
        /// This is the Z80 interface to the Shugart hard disk controller.
        /// The Z80 can take control of stepping the heads in whatever direction
        /// the drive is currently set up to move in.
        /// </summary>
        public bool RunStateMachine(PERQtoZ80Message message, byte data)
        {
            // One byte for Seek:
            //  byte 0 = seek count
            _system.IOB.DiskController.DoMultipleSeek(data);

            // In "normal step mode", there should be a minimum of 1ms per step!
            // In "buffered" mode, pulses are accumulated in a counter at a faster
            // rate, then a 16-step acceleration curve is used to move the heads
            // at the hardware's nominal rate.  Note that a 20ms settle time is
            // required after any seek before a read/write operation!  That's a
            // busy time of around 14,700 Z80 cycles, so obviously we aren't going
            // to run our emulation at those glacial speeds... :-)
            _busyClocks = 10 * data;

            _seekInProgress = true;

            return true;
        }

        public void Poll(ref Queue<byte> fifo)
        {
            if (_busyClocks == 0 && _seekInProgress)
            {
                // Send completion message:
                //  SOM
                //  0xa (seek done)
                fifo.Enqueue(Z80System.SOM);
                fifo.Enqueue((byte)Z80toPERQMessage.SeekComplete);

                _seekInProgress = false;
            }
        }

        private int _busyClocks;
        private bool _seekInProgress;

        private PERQSystem _system;
    }
}