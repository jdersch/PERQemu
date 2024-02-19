//
// BlockBuffer.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.TapeDevices
{
    /// <summary>
    /// Sector header bytes interpreted as block types, which look suspiciously
    /// like .TAP markers, but I'm sure that's purely coincidental.  Still, you
    /// should make sure these match the TAPFormatter's Marker types, maybe?
    /// </summary>
    public enum BlockType : uint
    {
        FileMark = 0x00000000,
        Data = 0x00000200,          // Good data (512 byte block)
        Empty = 0x10000200,         // Internal use (unwritten block)
        BadData = 0x80000200,       // Bad flag (512 bytes present)
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
    public struct BlockBuffer
    {
        public BlockBuffer(BlockType type)
        {
            _type = type;
            _data = new byte[512];      // Fixed length for the streamer
            _currentByte = 0;
            }

        public BlockType Type => _type;
        public byte[] Data => _data;
        public uint Pointer => _currentByte;    // debug output

        // This makes sense when writing...
        public bool Full => _currentByte == _data.Length;
        public bool Empty => _currentByte == 0;

        // ... but when reading we flip 'em
        public bool Ready => _currentByte == 0;
        public bool ReadComplete => _currentByte == _data.Length;

        public void Reset()
        {
            _currentByte = 0;
        }

        public void Clear()
        {
            for (_currentByte = 0; _currentByte < _data.Length; _currentByte++)
            {
                _data[_currentByte] = 0;
            }

            _currentByte = 0;
        }

        public void SetType(BlockType t)
        {
            _type = t;
        }

        public byte GetByte()
        {
            if (_currentByte < _data.Length)
            {
                return _data[_currentByte++];
            }

            return 0;
        }

        public void PutByte(byte value)
        {
            if (_currentByte < _data.Length)
            {
                _data[_currentByte++] = value;
            }
        }

        BlockType _type;
        byte[] _data;
        uint _currentByte;
    }
}
