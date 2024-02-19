//
//  StreamHelper.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022-2024, Boondoggle Heavy Industries, Ltd.
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
using System.Text;
using System.Runtime.CompilerServices;
using System.Collections.Generic;

namespace PERQmedia
{
    /// <summary>
    /// Stream extension methods to read and write multi-byte values in a
    /// consistent, platform-independent format.  All of these methods check
    /// for EOF and throw an EndOfStreamException if no data is available.
    /// </summary>
    /// <remarks>
    /// Not tested on anything but x86/x64 so far.  Does mono even run on any
    /// big-endien platforms?  Do any still exist?  Can't afford a 5GHz SPARC
    /// M8 box yet, so I guess we'll just have to wait and see.
    /// </remarks>
    public static class StreamHelper
    {
        /// <summary>
        /// Bug out if we'll hit EOF prematurely trying to read <param name="count">count</param>,
        /// bytes, since I'm too lazy to check every individual return code. :-P
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static bool CheckRead(this Stream fs, int count = 1)
        {
            if (fs.Position + count > fs.Length)
                throw new EndOfStreamException("Premature EOF reading file");

            return true;
        }

        /// <summary>
        /// A checked read of one byte, actually returned as a byte.
        /// </summary>
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static byte ReadByte(this Stream fs)
        {
            fs.CheckRead();
            return (byte)fs.ReadByte();
        }

        /// <summary>
        /// Writes a 16-bit value, MSB first.
        /// </summary>
        public static void WriteShort(this Stream fs, ushort val)
        {
            unchecked
            {
                fs.WriteByte((byte)(val >> 8));
                fs.WriteByte((byte)val);
            }
        }

        /// <summary>
        /// Reads a 16-bit value.
        /// </summary>
        public static ushort ReadShort(this Stream fs)
        {
            fs.CheckRead(2);

            ushort val = 0;

            val = (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();

            return val;
        }

        /// <summary>
        /// Writes a 32-bit value, MSB first.
        /// </summary>
        public static void WriteInt(this Stream fs, int val)
        {
            unchecked
            {
                fs.WriteByte((byte)(val >> 24));
                fs.WriteByte((byte)(val >> 16));
                fs.WriteByte((byte)(val >> 8));
                fs.WriteByte((byte)val);
            }
        }

        /// <summary>
        /// Reads a 32-bit value.
        /// </summary>
        public static int ReadInt(this Stream fs)
        {
            fs.CheckRead(4);

            int val = 0;

            val = (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();

            return val;
        }

        /// <summary>
        /// Writes a 32-bit unsigned value, MSB first.
        /// </summary>
        public static void WriteUInt(this Stream fs, uint val)
        {
            unchecked
            {
                fs.WriteByte((byte)(val >> 24));
                fs.WriteByte((byte)(val >> 16));
                fs.WriteByte((byte)(val >> 8));
                fs.WriteByte((byte)val);
            }
        }

        /// <summary>
        /// Reads a 32-bit unsigned value.
        /// </summary>
        public static uint ReadUInt(this Stream fs)
        {
            fs.CheckRead(4);

            uint val = 0;

            val = (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();
            val <<= 8;
            val |= (byte)fs.ReadByte();

            return val;
        }

        /// <summary>
        /// Writes a 64-bit value to the stream, MSB first.
        /// </summary>
        public static void WriteLong(this Stream fs, long val)
        {
            unchecked
            {
                fs.WriteByte((byte)(val >> 56));
                fs.WriteByte((byte)(val >> 48));
                fs.WriteByte((byte)(val >> 40));
                fs.WriteByte((byte)(val >> 32));
                fs.WriteByte((byte)(val >> 24));
                fs.WriteByte((byte)(val >> 16));
                fs.WriteByte((byte)(val >> 8));
                fs.WriteByte((byte)val);
            }
        }

        /// <summary>
        /// Reads a 64-bit value.
        /// </summary>
        public static long ReadLong(this Stream fs)
        {
            fs.CheckRead(8);

            long val = 0;

            for (int i = 0; i < 7; i++)
            {
                val |= (byte)fs.ReadByte();
                val <<= 8;
            }
            val |= (byte)fs.ReadByte();

            return val;
        }

        /// <summary>
        /// Writes a string as 8-bit bytes (UTF8 encoded).  Explicitly adds
        /// a null-terminator to make it simpler for ReadString (and other
        /// language implementations) to read the files back in.
        /// </summary>
        public static void WriteString(this Stream fs, string val)
        {
            var bytes = Encoding.UTF8.GetBytes(val);

            fs.Write(bytes, 0, bytes.Length);
            fs.WriteByte(0);
        }

        /// <summary>
        /// Reads a stream of (presumably) UTF8-encoded bytes up to and
        /// including a terminating byte, and returns a string.
        /// </summary>
        public static string ReadString(this Stream fs, byte term = 0)
        {
            byte b;
            var bytes = new List<byte>();

            // NB: Always reads at least one byte (null string terminator)
            while (fs.CheckRead() && (b = (byte)fs.ReadByte()) != term)
            {
                bytes.Add(b);
            }

            return Encoding.UTF8.GetString(bytes.ToArray());
        }
    }
}
