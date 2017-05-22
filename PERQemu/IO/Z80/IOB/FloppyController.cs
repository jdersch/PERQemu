// floppycontroller.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.PhysicalDisk;

using System;
using System.Collections.Generic;
using System.IO;

namespace PERQemu.IO.Z80.IOB
{

    /// <summary>
    /// Represents a virtual PERQ floppy drive (w/floppy disk).
    /// This performs the various actions the drive is responsible for.
    /// </summary>
    public sealed class FloppyController : IZ80Device
    {

        public FloppyController()
        {
            _loaded = false;
            Reset();
        }

        public void Reset()
        {
            _messageIndex = 0;
            _messageData = new byte[256];
            _cylinder = 0;
            _head = 0;

            // There are at most 7 bytes in the NEC result registers
            _necStatus = new byte[7];
            _necStatusLength = 7;
        }

        public void LoadImage(string path)
        {
            // If path is non-null, we load the image;
            // if it is null, we create a new, empty image.
            if (path != null)
            {
                // This is a hacky way to determine the image type -- based on the file size!
                // This is terrible.
                // TODO: Splice in new ".pfd" format floppy header - similar to the hard disk
                //       format, determines geometry and filesystem type.
                FileInfo file = new FileInfo(path);

                DiskGeometry geometry = DiskGeometry.FloppyDSSD;

                switch (file.Length)
                {
                    case 1025024:
                        geometry = DiskGeometry.FloppyDSDD;
                        break;

                    case 512512:
                        geometry = DiskGeometry.FloppyDSSD;     // or... SSDD?
                        break;

                    case 256256:
                        geometry = DiskGeometry.FloppySSSD;
                        break;

                    default:
                        throw new InvalidOperationException("Unrecognized disk format.");
                }

                _disk = new RawFloppyDisk(geometry);
                FileStream fs = new FileStream(path, FileMode.Open);
                _disk.Load(fs);
                fs.Close();
                _loaded = true;

#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.FloppyDisk,
                              "Loaded floppy geometry is {0}/{1}/{2} with sector size {3}",
                              geometry.Cylinders, geometry.Tracks, geometry.Sectors, geometry.SectorSize);
#endif
            }
            else
            {
                _disk = new RawFloppyDisk(DiskGeometry.FloppyDSSD);
                _loaded = true;
            }
        }

        public void SaveImage(string path)
        {
            if (_disk != null && _loaded)
            {
                FileStream fs = new FileStream(path, FileMode.OpenOrCreate);
                _disk.Save(fs);
                fs.Close();
            }
            else
            {
                throw new InvalidOperationException("There is no loaded disk to save.");
            }
        }

        public void UnloadImage()
        {
            _disk = null;
            _loaded = false;
        }

        public void Poll(ref Queue<byte> fifo)
        {

        }

        public bool RunStateMachine(PERQtoZ80Message message, byte value)
        {
            bool retVal = false;

            _messageData[_messageIndex] = value;
            _messageIndex++;

            switch (message)
            {
                case PERQtoZ80Message.FloppyBoot:
                    // No args for this message
                    _messageIndex = 0;
                    Boot();
                    retVal = true;
                    break;

                case PERQtoZ80Message.SetFloppyStatus:
                    // 4 bytes for set status:
                    //  byte 0 = length (3)
                    //  byte 1 = single/double density (0=single, 1=double)
                    //  byte 2 = # heads (1 or 2)
                    //  byte 3 = Interrupt Enable
                    if (_messageIndex > 3)
                    {
                        _messageIndex = 0;
                        SetFloppyStatus();
                        retVal = true; // done with message
                    }
                    break;

                case PERQtoZ80Message.FloppyCommand:
                    // 5 bytes for message, followed by n bytes of data:
                    //  byte 0 = head / unit
                    //  byte 1 = cylinder
                    //  byte 2 = sector
                    //  byte 3 = command type
                    //  byte 4 = byte count
                    //  byte 5...n = data
                    if (_messageIndex > 4 + _messageData[4])
                    {
                        _messageIndex = 0;
                        DoFloppyCommand();
                        retVal = true;
                    }
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unhandled floppy message {0}", message);
#endif
                    break;
            }

            return retVal;
        }

        /// <summary>
        /// Returns a FIFO with the boot sector data in it.
        /// </summary>
        /// <returns></returns>
        private void Boot()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.FloppyDisk, "Doing floppy boot.");
#endif

            if (!_loaded)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.Warnings, "No disk in floppy drive.");
#endif

                // Return invalid boot message
                Z80System.Instance.FIFO.Enqueue(0x55);      // SOM for floppy boot is different
                Z80System.Instance.FIFO.Enqueue(0x12);      // Error during boot
                return;
            }

            // Read Cyl 1, Track 0, Sector 0 -- this should start with
            // 55 AA, if not it's not a boot sector
            Sector boot = _disk.GetSector(1, 0, 0);

            if (boot.ReadByte(0) != 0x55 ||
                boot.ReadByte(1) != 0xaa)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Disk does not appear to be bootable.");
#endif

                // Return invalid boot data message
                Z80System.Instance.FIFO.Enqueue(0x55);      // SOM for floppy boot is different
                Z80System.Instance.FIFO.Enqueue(0x12);      // Error during boot
            }

            int cylinder = 1;
            int track = 0;
            int sector = 1;

            int checksum = 0;
            ushort checksumWord = 0;
            bool checksumBit = false;

            int[] ints = new int[0x3000];
            int intscount = 0;

            for (int sectorCount = 0; sectorCount < 96; sectorCount++)
            {
                // Add start of message:
                // We're sending 128 bytes (1 sector) at a time
                Z80System.Instance.FIFO.Enqueue(0x55);      // SOM for floppy boot is different
                Z80System.Instance.FIFO.Enqueue(0x13);      // Valid boot data
                Z80System.Instance.FIFO.Enqueue((byte)_disk.DiskGeometry.SectorSize); // # of bytes in this message

                Sector s = _disk.GetSector(cylinder, track, sector);

                for (int b = 0; b < _disk.DiskGeometry.SectorSize; b++)
                {
                    byte data = s.ReadByte(b);
                    Z80System.Instance.FIFO.Enqueue(data);  // Data byte

                    if (!checksumBit)
                    {
                        checksumWord = data;
                        checksumBit = true;
                    }
                    else
                    {
                        checksumWord = (ushort)(checksumWord | (data << 8));
                        checksumBit = false;
                        checksum += checksumWord;
                        checksum = checksum & 0xfffff;
                        ints[intscount++] = checksumWord;
                    }
                }

                sector++;
                if (sector >= _disk.DiskGeometry.Sectors)
                {
                    sector = 0;

                    cylinder++;
                    if (cylinder >= _disk.DiskGeometry.Cylinders)
                    {
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.Warnings, "Ran out of cylinders reading boot sector...");
#endif
                        cylinder = 0;
                    }
                }
            }

            checksum = 0;

            // This is really stupid
            for (int i = 0x17ff; i >= 0; i--)
            {
                checksum += ints[i];
                checksum &= 0xfffff;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Boot Checksum is {0:x4}", checksum & 0xffff);
#endif
        }

        private void SetFloppyStatus()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.FloppyDisk, "Setting floppy status to Density {0}, Heads {1} IE {2}",
                                              _messageData[1], _messageData[2], _messageData[3]);
#endif

            switch (_messageData[1])
            {
                case 0:
                    _setDensity = Density.Single;
                    break;

                case 0x40:
                    _setDensity = Density.Double;
                    break;

                default:
                    _setDensity = Density.Invalid;

#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Invalid floppy density specification {0:x2}", _messageData[1]);
#endif
                    break;
            }
        }

        public void GetStatus(ref Queue<byte> fifo)
        {
            // Floppy GetStatus looks like:
            //  SOM
            //  0x10 (floppy status message type)
            //  N (number of bytes)
            //  <reg1>..<regN> NEC floppy registers
            fifo.Enqueue(Z80System.SOM);
            fifo.Enqueue((byte)Z80toPERQMessage.FloppyStatus);
            fifo.Enqueue((byte)_necStatusLength);

            for (int i = 0; i < _necStatusLength; i++)
            {
                fifo.Enqueue(_necStatus[i]);
            }
        }

        private void DoFloppyCommand()
        {
            byte head = _messageData[0];
            byte cyl = _messageData[1];
            byte sec = _messageData[2];
            FloppyCommand command = (FloppyCommand)_messageData[3];
            byte count = _messageData[4];

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.FloppyDisk, "Floppy command is {0}, cyl/head/sec {1}/{2}/{3}",
                                               command, cyl, head, sec);
#endif

            switch (command)
            {
                case FloppyCommand.Seek:
                    Seek(head, cyl, sec);
                    break;

                case FloppyCommand.Read:
                    Read(head, cyl, sec);
                    break;

                case FloppyCommand.Write:
                    Write(head, cyl, sec, count);
                    break;

                case FloppyCommand.Format:
                    Format(head, cyl, sec);
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.Warnings, "Unhandled floppy command {0}", command);
#endif
                    break;
            }
        }

        public void Seek(byte head, byte cyl, byte sec)
        {
            _head = (head == 0) ? 0 : 1;        // TODO: we're ignoring the device # here...
            _cylinder = cyl;

            bool error = false;

            if (!_loaded || _cylinder > _disk.DiskGeometry.Cylinders || _head > _disk.DiskGeometry.Tracks)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings, "Invalid Floppy seek to Cylinder {0} Head {1}", _cylinder, _head);
#endif

                error = true;
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.FloppyDisk, "Floppy seek to Cylinder {0} Head {1}", _cylinder, _head);
#endif
            }

            // Message out format:
            //  SOM
            //  0x11 (floppy done)
            //  0 for success, 1 for failure.
            Z80System.Instance.FIFO.Enqueue(Z80System.SOM);
            Z80System.Instance.FIFO.Enqueue((byte)Z80toPERQMessage.FloppyDone);
            Z80System.Instance.FIFO.Enqueue(error ? (byte)1 : (byte)0);

            // Set up the NEC status registers.
            // 2 registers for a seek operation
            _necStatusLength = 2;

            StatusRegister0 reg0 =
                StatusRegister0.FlpUnit0 |                      // TODO: we always assume device 0.
                (head == 0 ? 0 : StatusRegister0.FlpHead) |
                (_loaded ? 0 : StatusRegister0.FlpNotReady) |
                (0) |                                           // Equipment check
                (0) |                                           // Seek end
                (error ? StatusRegister0.FlpIntrCode0 : 0) |    // Interrupt code (0 = ok, 1 = unsuccessful command)
                (0);                                            // same (high bit not set for our purposes)

            _necStatus[0] = (byte)reg0;
            _necStatus[1] = (byte)cyl;
        }

        private void Read(byte head, byte cyl, byte sec)
        {
            head = (head == 0) ? (byte)0 : (byte)1;             // TODO: we're ignoring the device # here...
            bool error = false;

            if (!_loaded ||
                cyl > _disk.DiskGeometry.Cylinders ||
                head > _disk.DiskGeometry.Tracks ||
                sec > _disk.DiskGeometry.Sectors ||
                _setDensity != GetDiskDensity())
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings, "Invalid Floppy read from Cylinder {0} Head {1} Sector {2}.",
                                                _cylinder, _head, sec);
#endif

                error = true;
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.FloppyDisk, "Floppy read from Cylinder {0} Head {1} Sector {2}.",
                                                  _cylinder, _head, sec);
#endif
            }

            // Message format is:
            //  SOM
            //  5 (Floppy Data)
            //  0 for success, 1 for error
            //  byte count
            //  data
            Z80System.Instance.FIFO.Enqueue(Z80System.SOM);
            Z80System.Instance.FIFO.Enqueue((byte)Z80toPERQMessage.FloppyData);

            if (error)
            {
                Z80System.Instance.FIFO.Enqueue(1);     // Indicate an error
                Z80System.Instance.FIFO.Enqueue(1);     // Length (can't use 0 as the PERQ interprets that as 256)
                Z80System.Instance.FIFO.Enqueue(0);     // Bogus data
            }
            else
            {
                Z80System.Instance.FIFO.Enqueue(0);     // No error
                Z80System.Instance.FIFO.Enqueue((byte)_disk.DiskGeometry.SectorSize);  // A full sector

                // Read the sector in
                Sector sectorData = _disk.GetSector(cyl, head, sec-1);

                for (int b = 0; b < _disk.DiskGeometry.SectorSize; b++)
                {
                    byte data = sectorData.ReadByte(b);
                    Z80System.Instance.FIFO.Enqueue(data);
                }
            }

            // Set up the NEC status registers.
            // 7 registers for a read operation
            _necStatusLength = 7;

            StatusRegister0 reg0 =
                StatusRegister0.FlpUnit0 |                      // TODO: we always assume device 0.
                (head == 0 ? 0 : StatusRegister0.FlpHead) |
                (_loaded ? 0 : StatusRegister0.FlpNotReady) |
                (0) |                                           // Equipment check
                (0) |                                           // Seek end
                (error ? StatusRegister0.FlpIntrCode0 : 0) |    // Interrupt code (0 = ok, 1 = unsuccessful command)
                (0);                                            // Same (high bit not set for our purposes)

            StatusRegister1 reg1 =
                (_setDensity != GetDiskDensity() ? StatusRegister1.FlpMissAddr : 0 ) |  // Missing address mark
                (0) |                                                                   // Not writeable
                (0) |                                                                   // No data
                (0) |                                                                   // Overrun
                (0) |                                                                   // Data error
                (_disk != null && sec > _disk.DiskGeometry.Sectors ? StatusRegister1.FlpEndCylinder : 0); // end of cyl

            StatusRegister2 reg2 =
                (_setDensity != GetDiskDensity() ? StatusRegister2.FlpDataMissAddr : 0) |  // Missing address mark
                (0) |                                                                   // Bad cylinder
                (0) |                                                                   // Wrong cylinder
                (0);                                                                    // Data error in data

            _necStatus[0] = (byte)reg0;
            _necStatus[1] = (byte)reg1;
            _necStatus[2] = (byte)reg2;
            _necStatus[3] = (byte)cyl;
            _necStatus[4] = (byte)head;
            _necStatus[5] = (byte)sec;
            _necStatus[6] = _disk != null ? (byte)_disk.DiskGeometry.SectorSize : (byte)0;
        }

        private void Write(byte head, byte cyl, byte sec, int count)
        {
            head = (head == 0) ? (byte)0 : (byte)1;             // TODO: we're ignoring the device # here...
            bool error = false;

            if (!_loaded ||
                cyl > _disk.DiskGeometry.Cylinders ||
                head > _disk.DiskGeometry.Tracks ||
                sec > _disk.DiskGeometry.Sectors ||
                _setDensity != GetDiskDensity())
            {
                error = true;

#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings, "Invalid Floppy write to Cylinder {0} Head {1} Sector {2}.",
                                                _cylinder, _head, sec);
#endif
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.FloppyDisk, "Floppy write to Cylinder {0} Head {1} Sector {2}.",
                                                  _cylinder, _head, sec);
#endif
            }

            // Read the sector in.
            Sector sectorData = _disk.GetSector(cyl, head, sec - 1);

            // The starting offset for the data is at index 5 of the message data
            for (int b = 0; b < count; b++)
            {
                sectorData.Data[b] = _messageData[5 + b];
            }

            // Message out format:
            //  SOM
            //  0x11 (floppy done)
            //  0 for success, 1 for failure.
            Z80System.Instance.FIFO.Enqueue(Z80System.SOM);
            Z80System.Instance.FIFO.Enqueue((byte)Z80toPERQMessage.FloppyDone);
            Z80System.Instance.FIFO.Enqueue(error ? (byte)1 : (byte)0);

            // Set up the NEC status registers.
            // 7 registers for a write operation
            _necStatusLength = 7;

            StatusRegister0 reg0 =
                StatusRegister0.FlpUnit0 |                      // TODO: always unit 0. SL1 pin is NC on the hardware :-(
                (head == 0 ? 0 : StatusRegister0.FlpHead) |
                (_loaded ? 0 : StatusRegister0.FlpNotReady) |
                (0) |                                           // Equipment check
                (0) |                                           // Seek end
                (error ? StatusRegister0.FlpIntrCode0 : 0) |    // Interrupt code (0 = ok, 1 = unsuccessful command)
                (0);                                            // Same (high bit not set for our purposes)

            StatusRegister1 reg1 =
                (_setDensity != GetDiskDensity() ? StatusRegister1.FlpMissAddr : 0) |   // Missing address mark
                (0) |                                                                   // Not writeable
                (0) |                                                                   // No data
                (0) |                                                                   // Overrun
                (0) |                                                                   // Data error
                (_disk != null && sec > _disk.DiskGeometry.Sectors ? StatusRegister1.FlpEndCylinder : 0); // End of cyl

            StatusRegister2 reg2 =
                (_setDensity != GetDiskDensity() ? StatusRegister2.FlpDataMissAddr : 0) | // Missing address mark
                (0) |                                                                   // Bad cylinder
                (0) |                                                                   // Wrong cylinder
                (0);                                                                    // Data error in data

            _necStatus[0] = (byte)reg0;
            _necStatus[1] = (byte)reg1;
            _necStatus[2] = (byte)reg2;
            _necStatus[3] = (byte)cyl;
            _necStatus[4] = (byte)head;
            _necStatus[5] = (byte)sec;
            _necStatus[6] = _disk != null ? (byte)_disk.DiskGeometry.SectorSize : (byte)0;
        }

        private void Format(byte head, byte cyl, byte sec)
        {
            head = (head == 0) ? (byte)0 : (byte)1;             // TODO: we're ignoring the device # here...
            bool error = false;

            if (!_loaded ||
                cyl > _disk.DiskGeometry.Cylinders ||
                head > _disk.DiskGeometry.Tracks ||
                sec > _disk.DiskGeometry.Sectors)
            {
                error = true;

#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings, "Invalid Floppy format Cylinder {0} Head {1} Sector {2}.",
                                                _cylinder, _head, sec);
#endif
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.FloppyDisk, "Floppy format Cylinder {0} Head {1} Sector {2}.",
                                                  _cylinder, _head, sec);
#endif
            }

            // Read the sector in.
            Sector sectorData = _disk.GetSector(cyl, head, sec - 1);

            // The starting offset for the data is at index 5 of the message data
            for (int b = 0; b < _disk.DiskGeometry.SectorSize; b++)
            {
                sectorData.Data[b] = 0xff;
            }

            // Message out format:
            //  SOM
            //  0x11 (floppy done)
            //  0 for success, 1 for failure.
            Z80System.Instance.FIFO.Enqueue(Z80System.SOM);
            Z80System.Instance.FIFO.Enqueue((byte)Z80toPERQMessage.FloppyDone);
            Z80System.Instance.FIFO.Enqueue(error ? (byte)1 : (byte)0);

            // Set up the NEC status registers.
            // 7 registers for a format operation
            _necStatusLength = 7;

            StatusRegister0 reg0 =
                StatusRegister0.FlpUnit0 |                      // TODO: we always assume device 0.
                (head == 0 ? 0 : StatusRegister0.FlpHead) |
                (_loaded ? 0 : StatusRegister0.FlpNotReady) |
                (0) |                                           // Equipment check
                (0) |                                           // Seek end
                (error ? StatusRegister0.FlpIntrCode0 : 0) |    // Interrupt code (0 = ok, 1 = unsuccessful command)
                (0);                                            // Same (high bit not set for our purposes)

            StatusRegister1 reg1 =
                (0) |                                           // Missing address mark
                (0) |                                           // Not writeable
                (0) |                                           // No data
                (0) |                                           // Overrun
                (0) |                                           // Data error
                (_disk != null && sec > _disk.DiskGeometry.Sectors ? StatusRegister1.FlpEndCylinder : 0); // end of cyl

            StatusRegister2 reg2 =
                (0) |                                           // Missing address mark
                (0) |                                           // Bad cylinder
                (0) |                                           // Wrong cylinder
                (0);                                            // Data error in data

            _necStatus[0] = (byte)reg0;
            _necStatus[1] = (byte)reg1;
            _necStatus[2] = (byte)reg2;
            _necStatus[3] = (byte)cyl;
            _necStatus[4] = (byte)head;
            _necStatus[5] = (byte)sec;
            _necStatus[6] = _disk != null ? (byte)_disk.DiskGeometry.SectorSize : (byte)0;

        }

        /// <summary>
        /// Returns the density of the loaded disk (NOT necessarily the density that the PERQ has specified.)
        /// </summary>
        /// <returns></returns>
        private Density GetDiskDensity()
        {
            if (_disk == null)
            {
                return Density.Invalid;
            }
            else
            {
                return _disk.DiskGeometry.SectorSize == 128 ? Density.Single : Density.Double;
            }
        }

        private enum FloppyCommand
        {
            Read = 0x1,
            Write = 0x2,
            Format = 0x3,
            Seek = 0x4
        }

        private enum Density
        {
            Invalid = 0,    // TODO: make these agree with PERQfloppy.pl
            Single,
            Double
        }

        //
        // The following are the status registers in the original NEC controller
        // The naming convention is from the POS pascal code.  (See IO_UNIT.PAS)
        // StatusRegister3 is unused by POS so is not yet implemented.
        //

        [Flags]
        private enum StatusRegister0
        {
            FlpUnit0            = 0x1,
            FlpUnit1            = 0x2,
            FlpHead             = 0x4,
            FlpNotReady         = 0x8,
            FlpEquipChk         = 0x10,
            FlpSeekEnd          = 0x20,
            FlpIntrCode0        = 0x40,
            FlpIntrCode1        = 0x80
        }

        [Flags]
        private enum StatusRegister1
        {
            FlpMissAddr         = 0x1,
            FlpNotWritable      = 0x2,
            FlpNoData           = 0x4,
            Unused1             = 0x8,
            FlpOverRun          = 0x10,
            FlpDataError        = 0x20,
            Unused2             = 0x40,
            FlpEndCylinder      = 0x80
        }

        [Flags]
        private enum StatusRegister2
        {
            FlpDataMissAddr     = 0x1,
            FlpBadCylinder      = 0x2,
            Unused1             = 0x4,
            Unused2             = 0x8,
            FlpWrongCylinder    = 0x10,
            FlpDataDataError    = 0x20,
            Unused3             = 0x40,
            Unused4             = 0x80
        }

        private byte[] _necStatus;
        private int _necStatusLength;

        private PhysicalDisk.PhysicalDisk _disk;
        private bool _loaded;

        // Current read/write position
        private int _cylinder;
        private int _head;

        // Density type specified by the PERQ
        private Density _setDensity;

        private byte[] _messageData;
        private int _messageIndex;
    }
}
