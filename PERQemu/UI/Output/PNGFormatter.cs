//
// PNGFormatter.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using System.IO;
using System.IO.Compression;
using System.Text;

using PERQmedia;

namespace PERQemu.UI.Output
{
    /// <summary>
    /// Write a Page as a 1bpp compressed PNG image file, with metadata.
    /// </summary>
    public class PNGFormatter
    {
        public PNGFormatter(string[] keywords, string[] metadata)
        {
            _keys = keywords;
            _values = metadata;
        }

        public void Save(Page page, Stream fs)
        {
            uint crc;

            // The fixed PNG header and trailer
            byte[] cookie = { 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a };
            byte[] trailer = { 0, 0, 0, 0, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82 };

            // Write the cookie
            fs.Write(cookie, 0, cookie.Length);

            // Write the header chunk length (fixed), data, and CRC
            fs.WriteUInt(13);

            using (var hdr = new CRC32Stream(fs))
            {
                hdr.ResetChecksum();
                hdr.Write(Encoding.ASCII.GetBytes("IHDR"), 0, 4);
                hdr.WriteUInt(page.BitWidth);
                hdr.WriteUInt(page.BitHeight);
                hdr.WriteByte(page.BitDepth);   // 1 = 1 bpp
                hdr.WriteByte(0);               // 0 = grayscale color type
                hdr.WriteByte(0);               // 0 = deflate compression
                hdr.WriteByte(0);               // 0 = filter type
                hdr.WriteByte(0);               // 0 = no interlace
                crc = hdr.WriteCRC;
            }
            fs.WriteUInt(crc);

            // Write a chunk for DPI
            fs.WriteUInt(9);

            using (var phys = new CRC32Stream(fs))
            {
                var ppm = (uint)(page.Resolution * 39.37);

                phys.ResetChecksum();
                phys.Write(Encoding.ASCII.GetBytes("pHYs"), 0, 4);
                phys.WriteUInt(ppm);    // X
                phys.WriteUInt(ppm);    // Y
                phys.WriteByte(1);      // Pixels per meter!
                crc = phys.WriteCRC;
            }
            fs.WriteUInt(crc);

            // Write the string chunks
            for (var i = 0; i < _keys.Length; i++)
            {
                // Compute the length for each one (with one specific null)
                fs.WriteUInt((uint)(_keys[i].Length + _values[i].Length + 1));

                using (var txt = new CRC32Stream(fs))
                {
                    txt.ResetChecksum();
                    txt.Write(Encoding.ASCII.GetBytes("tEXt"), 0, 4);
                    txt.Write(Encoding.ASCII.GetBytes(_keys[i]), 0, _keys[i].Length);
                    txt.WriteByte(0);
                    txt.Write(Encoding.ASCII.GetBytes(_values[i]), 0, _values[i].Length);
                    crc = txt.WriteCRC;
                }
                fs.WriteUInt(crc);
            }

            // ANOTHER copy, with the bloody filter type bytes added (per scanline)
            byte[] imageData;

            Log.Debug(Category.Formatter, "Image buffer: {0} bytes", page.Buffer.Length);

            // Now compress the bitmap and write the data chunk!
            using (var mem = new MemoryStream())
            {
                // Write the Zlib style header
                mem.WriteByte(120);             // CM + CINFO
                mem.WriteByte(1);               // FCHECK Bits

                // Deflate the bitmap data
                using (var cmp = new DeflateStream(mem, CompressionMode.Compress, true))
                {
                    for (var i = 0; i < page.BitHeight; i++)
                    {
                        cmp.WriteByte(0);   // Filter type 0
                        cmp.Write(page.Buffer, (int)(page.ScanWidth * i), (int)page.ScanWidth);
                    }
                }

                // Compute and tack on the checksum
                mem.WriteUInt(ComputeAdler32(mem.ToArray(), 2, (int)(mem.Length - 2)));

                // Now save the complete Zlib-format compressed data chunk
                imageData = mem.ToArray();
                Log.Debug(Category.Formatter, "Compressed: {0} bytes", imageData.Length);
            }

            // And write it into a PNG IDAT chunk!
            fs.WriteUInt((uint)(imageData.Length));

            using (var img = new CRC32Stream(fs))
            {
                img.ResetChecksum();
                img.Write(Encoding.ASCII.GetBytes("IDAT"), 0, 4);
                img.Write(imageData, 0, imageData.Length);
                crc = img.WriteCRC;
            }
            fs.WriteUInt(crc);

            // FINALLY write out the final chunk and save it!
            fs.Write(trailer, 0, trailer.Length);
            fs.Close();
        }

        /// <summary>
        /// Compute the Adler32 checksum required by the Zlib format.
        /// </summary>
        uint ComputeAdler32(byte[] data, int start, int length)
        {
            var s1 = 1;
            var s2 = 0;

            for (var i = 0; i < length; i++)
            {
                s1 = (s1 + data[start + i]) % 65521;
                s2 = (s1 + s2) % 65521;
            }

            Log.Debug(Category.Formatter, "Adler32 CRC: {0:x}", (uint)(s2 * 65536 + s1));
            return (uint)(s2 * 65536 + s1);
        }

        // Metadata (global for PNG)
        string[] _keys;
        string[] _values;
    }
}
