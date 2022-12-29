//
//  CRC32Stream.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022, Boondoggle Heavy Industries, Ltd.
//  
//  This file inspired by bits of code from numerous sources, including
//      Rei Miyasaka (CodeProject Open License)
//      Stephen Brumme (Zlib License)
//
//  (Copies of all the relevant licenses included with this distribution)
//
//  This program is free software: you can redistribute it and/or modify
//  it under the terms of the GNU General Public License as published by
//  the Free Software Foundation, either version 3 of the License, or
//  (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program.  If not, see <http://www.gnu.org/licenses/>.
//

using System.IO;

namespace PERQmedia
{
    /// <summary>
    /// Encapsulates a Stream to calculate a checksum as data passes through.
    /// </summary>
    /// <remarks>
    /// To accommodate the split ReadHeader/ReadData structure of the formatter
    /// we make the current CRC values static and let the caller choose when to
    /// reset them.  Or I could make the caller actually save the partial value
    /// and pass it back in, but either way is kinda gross.  Should probably do
    /// it that way though.  This kinda made me throw up in my mouth a little.
    /// </remarks>
    public class CRC32Stream : Stream
    {
        static CRC32Stream()
        {
            GenerateTable();
            ResetChecksum();
        }

        public CRC32Stream(Stream stream)
        {
            _stream = stream;
        }

        public Stream OuterStream
        {
            get { return _stream; }
        }

        public override bool CanRead
        {
            get { return _stream.CanRead; }
        }

        public override bool CanSeek
        {
            get { return _stream.CanSeek; }
        }

        public override bool CanWrite
        {
            get { return _stream.CanWrite; }
        }

        public override long Length
        {
            get { return _stream.Length; }
        }

        public override long Position
        {
            get { return _stream.Position; }
            set { _stream.Position = value; }
        }

        public override void Flush()
        {
            _stream.Flush();
        }

        public override long Seek(long offset, SeekOrigin origin)
        {
            return _stream.Seek(offset, origin);
        }

        public override void SetLength(long value)
        {
            _stream.SetLength(value);
        }

        /// <summary>
        /// Get the checksum of the data read by the stream thus far.
        /// </summary>
        public static uint ReadCRC
        {
            get { return unchecked(_readCRC ^ 0xffffffff); }
        }

        /// <summary>
        /// Get the checksum of the data written to the stream thus far.
        /// </summary>
        public static uint WriteCRC
        {
            get { return unchecked(_writeCRC ^ 0xffffffff); }
        }

        /// <summary>
        /// Reset the read and write checksums.
        /// </summary>
        public static void ResetChecksum()
        {
            _readCRC = unchecked(0xffffffff);
            _writeCRC = unchecked(0xffffffff);
        }

        /// <summary>
        /// Read from the stream and update the read checksum.
        /// </summary>
        public override int Read(byte[] buffer, int offset, int count)
        {
            count = _stream.Read(buffer, offset, count);
            _readCRC = CalculateCRC32(_readCRC, buffer, offset, count);
            return count;
        }

        /// <summary>
        /// Write to the stream and update the write checksum.
        /// </summary>
        public override void Write(byte[] buffer, int offset, int count)
        {
            _stream.Write(buffer, offset, count);
            _writeCRC = CalculateCRC32(_writeCRC, buffer, offset, count);
        }

        /// <summary>
        /// Calculate a running checksum (CRC-32B) on the stream data.
        /// </summary>
        public uint CalculateCRC32(uint prev, byte[] buffer, int offset, int count)
        {
            uint crc = prev;

            unchecked
            {
                while (count-- != 0)
                {
                    crc = (crc >> 8) ^ _table[buffer[offset++] ^ (crc & 0xff)];
                }
            }

            return crc;
        }

        private static void GenerateTable()
        {
            unchecked
            {
                _table = new uint[256];
                uint crc;

                for (uint i = 0; i < _table.Length; i++)
                {
                    crc = i;

                    for (int j = 8; j > 0; j--)
                    {
                        if ((crc & 1) == 1)
                            crc = (crc >> 1) ^ Polynomial;
                        else
                            crc >>= 1;
                    }
                    _table[i] = crc;
                }
            }
        }

        // The underlying Stream
        Stream _stream;

        // Running checksum values
        private static uint _readCRC;
        private static uint _writeCRC;

        // Standard polynomial
        public static uint Polynomial = 0xedb88320;

        // Pre-generated table
        private static uint[] _table;
    }
}

/*

Interesting but too slow:

/// <summary>
/// Calculate a running CRC32 using a table-less algorithm.
/// </summary>
/// <remarks>
/// Curiously, this is almost half as fast as the "normal" algorithm,
/// even when skipping the call to generate the table.  But it does
/// save some space, I guess.  The other "slicing" variations on this
/// code may be faster and could be worth looking into if loading the
/// bigger PERQ-2 disk images gets sluggish.  I mean, who has time to
/// wait a whole 400ms to load a 150MB disk?  Seriously.
/// </remarks>
public uint CalculateCRC32_Tableless(uint prev, byte[] buffer, int offset, int count)
{
	uint crc = ~prev;

	unchecked
	{
		while (count-- != 0)
		{
			uint s = (crc & 0xff) ^ buffer[offset++];
			uint t = (s ^ (s << 6)) << 24;

			// Some temporaries to optimize xor
			uint x = (t >> 1) ^ (t >> 2);
			uint y = x ^ (x >> 3);
			uint z = (t >> 12) ^ (t >> 16);

			crc = (crc >> 8) ^ t ^ (t >> 23) ^ y ^ (y >> 6) ^ z ^ (z >> 10);
		}
	}
	return ~crc;
}
 */