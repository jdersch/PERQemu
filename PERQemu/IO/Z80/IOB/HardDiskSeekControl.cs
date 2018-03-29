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
        public HardDiskSeekControl()
        {
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
            HardDisk.ShugartDiskController.Instance.DoMultipleSeek(data);

            _busyClocks = 5 * data;     // Should be 1.83ms per step :-)

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
    }
}