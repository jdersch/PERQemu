//
// QICTapeController.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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

using PERQmedia;
using PERQemu.Config;

namespace PERQemu.IO.TapeDevices
{
    /// <summary>
    /// Implements the PERQ's streaming tape interface, a "universal" controller
    /// which could slot into the CPU Option or IO Option slots (wire-wrapped
    /// version), or is included in the Multibus board (production/PCB version).
    /// This is the PERQ side which handles commands from the microcode and acts
    /// as a go-between with the emulated Archive Sidewinder tape drive.
    /// </summary>
    public sealed class QICTapeController : IStorageController
    {
        public QICTapeController(OptionBoardType board)
        {
            _board = board;

            // Set port numbers according to board type
            if (_board == OptionBoardType.MLO)
            {
                _controlPort = 0x92;
                _dataPort = 0x93;
            }
            else
            {
                _controlPort = 0x86;
                _dataPort = 0x84;
            }

            _drive = null;
        }


        public void Reset()
        {
            if (_drive != null)
            {
                _drive.Online = false;
                _drive.Request = false;
                _drive.Xfer = false;
                _drive.Reset();
            }

            Log.Debug(Category.Streamer, "Interface reset");
        }

        public void AttachDrive(uint unit, StorageDevice dev)
        {
            // Required by IStorageController but the split-brained nature of
            // the Sidewinder makes this a no-op
        }

        public void AttachDrive(uint unit, Sidewinder drive)
        {
            // Attach the Sidewinder (controller) here; the CartridgeTape
            // (underlying StorageDevice) is contained within
            if (_drive != null)
                throw new InvalidOperationException($"Controller only supports one tape unit");

            _drive = drive;

            Log.Info(Category.Streamer, "Attached streamer tape at unit {0}", unit);
        }

        /// <summary>
        /// Handle writes to the control or data registers.
        /// </summary>
        public void LoadRegister(byte address, int value)
        {
            // No drive?  Non-fatal, just log that we ignored it and bail
            if (_drive == null)
            {
                Log.Debug(Category.Streamer, "Write 0x{0:x2} to register 0x{1:x2} ignored (no drive!)",
                          value, address);
                return;
            }

            if (address == _controlPort)
            {
                var controlBits = (Control)value;

                Log.Debug(Category.Streamer, "Write 0x{0:x2} to control register 0x{1:x2}: {2}",
                          value, address, controlBits);

                if (controlBits.HasFlag(Control.Reset))
                {
                    _drive.Reset();         // Nuclear option
                }
                else
                {
                    // Set the status of the control signals based on the value
                    _drive.Online = controlBits.HasFlag(Control.Online);
                    _drive.Request = controlBits.HasFlag(Control.Request);
                    _drive.Xfer = controlBits.HasFlag(Control.Xfer);

                    _drive.CheckSignals();  // Boop!
                }

                return;
            }

            if (address == _dataPort)
            {
                Log.Detail(Category.Streamer, "Write 0x{0:x2} to data register 0x{1:x2}", value, address);
                _drive.Data = (byte)value;
                return;
            }

            throw new InvalidOperationException($"Bad register write 0x{address:x2}");
        }

        /// <summary>
        /// Read the controller's status register.
        /// </summary>
        public int ReadStatus()
        {
            // To identify the board type to the microcode, set or clear the 
            // StrIFace bits (1s = OIO, 0s = MLO)
            var status = (_board == OptionBoardType.OIO ? Status.OIOInterface : Status.None);

            if (_drive != null)
            {
                status |= (_drive.Ready ? Status.Ready : 0) |
                          (_drive.Acknowledge ? Status.Acknowledge : 0) |
                          (_drive.Exception ? Status.Exception : 0);
            }

            Log.Detail(Category.Streamer, "Read status 0x{0:x2}", (int)status);
            return (int)status;
        }

        /// <summary>
        /// Read the value on the QIC data bus.  Interpretation is contextual
        /// (but handled entirely by the microcode).  Here we return the current
        /// value on the bus assuming that the handshaking has already signaled
        /// that the drive placed a valid data or status byte in its latch.
        /// </summary>
        public int ReadData()
        {
            if (_drive == null)
            {
                Log.Debug(Category.Streamer, "Invalid read (no drive!)");
                return 0;
            }

            var data = _drive.Data;
            Log.Detail(Category.Streamer, "Read data 0x{0:x2}", data);
            return data;
        }


        public void DoSingleSeek()
        {
            throw new NotImplementedException();
        }


        public void DumpStatus()
        {
            if (_drive != null)
                _drive.DumpState();
        }

        Sidewinder _drive;
        OptionBoardType _board;

        // Handle OIO or MLO attachment
        byte _controlPort;
        byte _dataPort;
    }

    /// <summary>
    /// The controller buffers these control signals to the streamer and allows
    /// the microcode to set them directly.
    /// </summary>
    [Flags]
    enum Control
    {
        None = 0x0,
        Online = 0x1,
        Reset = 0x2,
        Ready = 0x4,
        Xfer = 0x5,
        Request = 0x8
    }

    /// <summary>
    /// Status lines from the streamer pass through the status register.  Two
    /// bits differentiate the board type (OIO or MLO) so the microcode can use
    /// the correct write ports.
    /// </summary>
    [Flags]
    enum Status
    {
        None = 0x0,
        Exception = 0x1,
        Acknowledge = 0x2,
        Ready = 0x4,
        OIOInterface = 0x60
    }
}
