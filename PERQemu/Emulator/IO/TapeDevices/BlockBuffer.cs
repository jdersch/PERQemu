//
// BlockBuffer.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.TapeDevices
{
    public partial class Sidewinder : StorageDevice
    {
        #region Buffer management

        private void InitBuffers()
        {
            // Zero them out, reset the pointer(s)
            for (var b = 0; b < _buffers.Length; b++)
            {
                _buffers[b] = new BlockBuffer(this, BlockType.Empty);
            }
            _hostBuffer = 0;
            _tapeBuffer = 0;

            Log.Info(Category.Streamer, "Buffers initialized");
        }

        private void ResetBuffers()
        {
            foreach (var b in _buffers)
            {
                b.Clear();
                b.SetType(BlockType.Empty);
            }
            _hostBuffer = 0;
            _tapeBuffer = 0;

            Log.Info(Category.Streamer, "Buffers reset");
        }

        /// <summary>
        /// Get the next empty buffer.  Return false if none currently available.
        /// </summary>
        private bool GetEmptyBuffer(ref int which)
        {
            // Try not to re-use the same buffer twice in a row!
            for (var i = which + 1; i < which + _buffers.Length; i++)
            {
                var nextBuf = i % _buffers.Length;

                // Be pedantic, make sure the buffer is properly reset (byte counter too)
                if (_buffers[nextBuf].Type == BlockType.Empty && _buffers[nextBuf].Empty)
                {
                    Log.Debug(Category.Streamer, "GetNextBuf: Buffer {0} is empty", nextBuf);
                    which = nextBuf;
                    return true;
                }
            }

            Log.Info(Category.Streamer, "GetEmpty: No free buffers!");
            return false;
        }

        /// <summary>
        /// Update the index and return true if any unwritten buffers remain.
        /// </summary>
        private bool GetFullBuffer(ref int which)
        {
            for (var i = which + 1; i < which + _buffers.Length; i++)
            {
                var nextBuf = i % _buffers.Length;

                // Don't snag any partially written ones!
                if (_buffers[nextBuf].Type != BlockType.Empty && _buffers[nextBuf].Full)
                {
                    Log.Debug(Category.Streamer, "GetDirty: Buffer {0} is fully dirty :-]", nextBuf);
                    which = nextBuf;
                    return true;
                }
            }

            Log.Info(Category.Streamer, "GetDirty: No dirty buffers remaining");
            return false;
        }

        /// <summary>
        /// Just return the count of uncommitted buffers.
        /// </summary>
        private int DirtyBuffers()
        {
            var count = 0;

            foreach (var b in _buffers)
            {
                if (b.Type != BlockType.Empty && b.Full) count++;
            }

            return count;
        }

        /// <summary>
        /// Commit a buffer to the underlying media.
        /// </summary>
        private void WriteBuffer(int index, int pos)
        {
            _buffers[index].WriteTo(pos);

            Log.Info(Category.Streamer, "WriteBuf: Writing @ pos {0} from buffer {1} (sector {2})",
                     pos, index, _buffers[index]);
        }

        /// <summary>
        /// Reads a tape block into a buffer for sending to the host, translating
        /// the tape position to C/H/S.
        /// </summary>
        private void ReadBuffer(int index, int pos)
        {
            _buffers[index] = new BlockBuffer(this, pos);

            Log.Info(Category.Streamer, "ReadBuf: Reading @ pos {0} into buffer {1} (sector {2})",
                      pos, index, _buffers[index]);
        }

        #endregion
    }


    /// <summary>
    /// Sector header bytes interpreted as block types, which look suspiciously
    /// like .TAP markers, but I'm sure that's purely coincidental.  Still, you
    /// should make sure these match the TAPFormatter's Marker types, maybe?
    /// </summary>
    internal enum BlockType : uint
    {
        FileMark = 0x00000000,
        Data = 0x00000200,          // Good data (512 byte block)
        Empty = 0x10000200,         // Internal use; not written to tape
        BadData = 0x80000100,       // Bad flag (512 bytes present)
        EraseGap = 0xfffffffe,      // Not used by PERQemu/PERQmedia
        EndOfMedia = 0xffffffff     // Logical end of media
    }

    /// <summary>
    /// Block buffer on the drive's controller.  The Sidewinder has three fixed
    /// 512-byte buffers that it uses to stream blocks to and from the media;
    /// we supplement that with a type code (derived from the SectorHeader data)
    /// and keep a read/write pointer to simplify the byte-by-byte access that
    /// the microcode does (there's no DMA to or from the streamer).
    /// </summary>
    /// <remarks>
    /// We wrap up a Sector here so that it can be read/written directly to the
    /// underlying StorageDevice atomically.  Buffers can be used for reading or
    /// writing, but we track the "direction" so that the definitions of empty/
    /// full (writing) or ready/complete (reading) make more sense.  This is a
    /// bit messy and inefficient, probably full of memory leaks or unnecessary
    /// GC pressure, but it simplifies the protocol handling by offloading buffer
    /// mechanics.  When writing, we don't know in advance where a block will
    /// actually land on the underlying device until we write it out, so the
    /// buffer pool just accepts the data and waits for the WriteBehind process
    /// to determine where the data ends up.
    /// </remarks>
    internal class BlockBuffer
    {
        /// <summary>
        /// Initialize a new WRITE buffer and set its type.  Write buffers are
        /// "positionless" until they are commtted to the tape, so the initial
        /// sector address (in C/H/S) is invalid until the buffer is flushed.
        /// </summary>
        public BlockBuffer(StorageDevice dev, BlockType type)
        {
            _dev = dev;

            // New Sector for storage, with an out-of-range address to catch
            // bad writes.  NB: The geometry here is fixed!  If there's no tape
            // in the drive we can't relay on the "NoMedia" geometry (doh!)
            _sector = new Sector(1, 0, 0, 512, 4, (type == BlockType.BadData));
            _currentByte = 0;
            _reading = false;

            SetType(type);
        }

        /// <summary>
        /// Initialize a new READ buffer from its position on the tape (hiding
        /// the C/H/S mapping from the controller).
        /// </summary>
        public BlockBuffer(StorageDevice dev, int pos)
        {
            _dev = dev;

            var hd = (byte)(pos / _dev.Geometry.Sectors);
            var sec = (ushort)(pos % _dev.Geometry.Sectors);

            _sector = _dev.Read(0, hd, sec);

            var marker = (uint)((_sector.ReadHeaderByte(0) << 24) |
                                (_sector.ReadHeaderByte(1) << 16) |
                                (_sector.ReadHeaderByte(2) << 8) |
                                (_sector.ReadHeaderByte(3)));

            _type = (BlockType)marker;
            _currentByte = 0;
            _reading = true;
        }

        public override string ToString()
        {
            return $"{_sector.CylinderID}/{_sector.HeadID}/{_sector.SectorID}, type {_type}";
        }

        public BlockType Type => _type;

        public byte[] Data => _sector.Data;     // no longer necessary??
        public uint Pointer => _currentByte;     // debug output
        public bool Reading => _reading;        // debug interest

        // This makes sense when writing...
        public bool Full => (!_reading && _currentByte == _sector.Data.Length);
        public bool Empty => (!_reading && _currentByte == 0);

        // ... but when reading we flip 'em  BECAUSE WHY NOT, OKAY?
        public bool Ready => (_reading && _currentByte == 0);
        public bool ReadComplete => (_reading && _currentByte == _sector.Data.Length);

        public void SetType(BlockType t)
        {
            var val = (uint)t;

            _sector.WriteHeaderByte(0, (byte)(val >> 24));
            _sector.WriteHeaderByte(1, (byte)(val >> 16));
            _sector.WriteHeaderByte(2, (byte)(val >> 8));
            _sector.WriteHeaderByte(3, (byte)val);

            _type = t;
        }

        public void SetDirection(bool reading)
        {
            _reading = reading;
        }

        public void Reset()
        {
            _currentByte = 0;
        }

        public void Clear()
        {
            for (_currentByte = 0; _currentByte < _sector.Data.Length; _currentByte++)
            {
                _sector.WriteByte(_currentByte, 0);
            }

            _currentByte = 0;
        }

        public byte GetByte()
        {
            if (_currentByte < _sector.Data.Length)
            {
                return _sector.ReadByte(_currentByte++);
            }

            return 0;
        }

        public void PutByte(byte value)
        {
            if (_currentByte < _sector.Data.Length)
            {
                _sector.WriteByte(_currentByte++, value);
            }
        }

        public void WriteTo(int pos)
        {
            // Update the sector address then write it to the StorageDev
            _sector.CylinderID = 0;
            _sector.HeadID = (byte)(pos / _dev.Geometry.Sectors);
            _sector.SectorID = (ushort)(pos % _dev.Geometry.Sectors);

            _dev.Write(_sector);
        }

        BlockType _type;
        Sector _sector;
        StorageDevice _dev;

        bool _reading;
        uint _currentByte;
    }
}
