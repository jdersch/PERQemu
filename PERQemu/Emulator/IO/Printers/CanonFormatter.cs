//
// CanonFormatter.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.Text;
using System.IO;
using System.IO.Compression;
using System.Collections.Generic;

using PERQmedia;

namespace PERQemu.IO
{
    /// <summary>
    /// Custom file formatters for Canon laser printer output.  These directly
    /// transform a PERQ bitmap in the CanonPrinter's page buffer to a minimal
    /// (but fully specification compliant!) monochrome image file WITHOUT the
    /// absurd dance to upsample it to 8- or 32-bpp with SDL2, or requiring the
    /// import of System.Drawing (which requires a dependency on an external
    /// libgdi+ library which isn't typically found on Linux/Mac?).  Allows us
    /// to also write creator strings, timestamps, version info, etc. which the
    /// FAR simpler SDL_image or System.Drawing Bitmap classes don't seem to
    /// support (even though they're like 5 lines of code instead of all this).
    /// 
    /// I know.  This is probably a Bad Idea.  May rip it all out later but for
    /// now I'm having fun. :-P
    /// </summary>
    public partial class CanonPrinter
    {
        /// <summary>
        /// Save the page as a PNG image.  One per output file, default compression.
        /// </summary>
        bool SavePageAsPNG(string filename)
        {
            uint crc;

            // The fixed PNG header and trailer
            byte[] cookie = { 0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a };
            byte[] trailer = { 0, 0, 0, 0, 0x49, 0x45, 0x4e, 0x44, 0xae, 0x42, 0x60, 0x82 };

            // Some informative strings
            string[] keys = { "Title", "Creation Time", "Software" };
            string[] values = { $"PERQ Canon {Model} printer output",
                                DateTime.Now.ToString("yyyy-MM-dd'T'HH:mm:ss.ffK"),
                                PERQemu.Version };

            // Build the output path
            var fullpath = Paths.BuildOutputPath(filename);

            try
            {
                using (var fs = new FileStream(fullpath, FileMode.Create, FileAccess.Write))
                {
                    // Write the cookie
                    fs.Write(cookie, 0, cookie.Length);

                    // Write the header chunk length (fixed), data, and CRC
                    fs.WriteUInt(13);

                    using (var hdr = new CRC32Stream(fs))
                    {
                        hdr.ResetChecksum();
                        hdr.Write(Encoding.ASCII.GetBytes("IHDR"), 0, 4);
                        hdr.WriteUInt(MaxWidth);        // fixme: fixed size for testing
                        hdr.WriteUInt(MaxHeight);
                        hdr.WriteByte(1);               // 1 = 1 bpp
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
                        phys.ResetChecksum();
                        phys.Write(Encoding.ASCII.GetBytes("pHYs"), 0, 4);
                        phys.WriteUInt(_resolution == 300 ? 11811U : 9449U);    // X
                        phys.WriteUInt(_resolution == 300 ? 11811U : 9449U);    // Y
                        phys.WriteByte(1);              // Pixels per meter!
                        crc = phys.WriteCRC;
                    }
                    fs.WriteUInt(crc);

                    // Write the string chunks
                    for (var i = 0; i < keys.Length; i++)
                    {
                        // Compute the length for each one (with one specific null)
                        fs.WriteUInt((uint)(keys[i].Length + values[i].Length + 1));

                        using (var txt = new CRC32Stream(fs))
                        {
                            txt.ResetChecksum();
                            txt.Write(Encoding.ASCII.GetBytes("tEXt"), 0, 4);
                            txt.Write(Encoding.ASCII.GetBytes(keys[i]), 0, keys[i].Length);
                            txt.WriteByte(0);
                            txt.Write(Encoding.ASCII.GetBytes(values[i]), 0, values[i].Length);
                            crc = txt.WriteCRC;
                        }
                        fs.WriteUInt(crc);
                    }

                    byte[] imageData;

                    Log.Info(Category.Formatter, "Image buffer: {0} bytes", _pageBuffer.Length);

                    // Now compress the bitmap and write the data chunk!
                    using (var mem = new MemoryStream())
                    {
                        // Write the Zlib style header
                        mem.WriteByte(120);             // CM + CINFO
                        mem.WriteByte(1);               // FCHECK Bits

                        // Deflate the bitmap data
                        using (var cmp = new DeflateStream(mem, CompressionMode.Compress, true))
                        {
                            // At the start of every scanline inject the filter type
                            // byte.  Do it here rather than cheating and embedding an
                            // extra byte in the page buffer itself at PrintLine. Sigh.
                            for (var i = 0; i < MaxHeight; i++)
                            {
                                cmp.WriteByte(0);   // Filter type 0
                                cmp.Write(_pageBuffer, ScanWidthInBytes * i, ScanWidthInBytes);
                            }
                        }

                        // Compute and tack on the checksum
                        mem.WriteUInt(ComputeAdler32(mem.ToArray(), 2, (int)(mem.Length - 2)));

                        // Now save the complete Zlib-format compressed data chunk
                        imageData = mem.ToArray();
                        Log.Info(Category.Formatter, "Compressed: {0} bytes", imageData.Length);
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

                // Made it!
                return true;
            }
            catch (Exception e)
            {
                Log.Info(Category.Formatter, "Failed to save output: {0}", e.Message);
                return false;
            }
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

            Log.Info(Category.Formatter, "Adler32 CRC: {0:x}", (uint)(s2 * 65536 + s1));
            return (uint)(s2 * 65536 + s1);
        }

        /// <summary>
        /// Save the page as a TIFF.  Currently limited to a single page per file,
        /// but should/will be expanded to support multi-page output (so a full
        /// document can be exported in one go).  Exported as 1bpp bi-level with
        /// NO compression; use 'tumble' to convert to PDF with CCITT Group 4 if
        /// you want to produce Bitsavers-compatible output. :-)
        /// </summary>
        bool SavePageAsTIFF(string filename)
        {
            uint offset = 8;             // Initial data offset

            //
            // Create a list of tags we'll use, and populate it without worrying
            // about order or offsets for out-of-line data
            //
            var tags = new List<TIFFtag>();

            // Image data
            tags.Add(new TIFFtag(TagNames.ImageWidth, FieldType.Word, 1, MaxWidth));
            tags.Add(new TIFFtag(TagNames.ImageLength, FieldType.Word, 1, MaxHeight));
            tags.Add(new TIFFtag(TagNames.BitsPerSample, FieldType.Word, 1, 1));        // 1 bpp
            tags.Add(new TIFFtag(TagNames.SamplesPerPixel, FieldType.Word, 1, 1));      // 1 bpp
            tags.Add(new TIFFtag(TagNames.ResolutionUnit, FieldType.Word, 1, 2));       // Inch
            tags.Add(new TIFFtag(TagNames.MinSampleValue, FieldType.Word, 1, 0));
            tags.Add(new TIFFtag(TagNames.MaxSampleValue, FieldType.Word, 1, 1));
            tags.Add(new TIFFtag(TagNames.DefaultImageColor, FieldType.Word, 1, 1));

            // Rational types are out-of-band (8 bytes)
            var hack = (((ulong)_resolution * 10000U) << 32) + 10000U;                  // But why?
            tags.Add(new TIFFtag(TagNames.XResolution, FieldType.Rational, 1, 0, hack));
            tags.Add(new TIFFtag(TagNames.YResolution, FieldType.Rational, 1, 0, hack));

            // One strip for the bitmap? Force these external since the validator
            // seems to expect an array for Offsets and ByteCounts. :-|
            tags.Add(new TIFFtag(TagNames.RowsPerStrip, FieldType.DWord, 1, MaxHeight));
            tags.Add(new TIFFtag(TagNames.StripOffsets, FieldType.DWord, 1, offset));
            tags.Add(new TIFFtag(TagNames.StripByteCounts, FieldType.DWord, 1, MaxHeight * ScanWidthInBytes));

            // Bump to skip over the bitmap data
            offset += (MaxHeight * ScanWidthInBytes);

            // File info
            tags.Add(new TIFFtag(TagNames.NewSubfileType, FieldType.DWord, 1, 2));
            tags.Add(new TIFFtag(TagNames.Compression, FieldType.Word, 1, 1));
            tags.Add(new TIFFtag(TagNames.PhotometricInterpretation, FieldType.Word, 1, 1));
            tags.Add(new TIFFtag(TagNames.Orientation, FieldType.Word, 1, 1));          // TopLeft

            // For now, just one page
            tags.Add(new TIFFtag(TagNames.PageNumber, FieldType.Word, 2, 1));

            // Optional extras
            var str = $"PERQ Canon {Model} printer output";
            tags.Add(new TIFFtag(TagNames.ImageDescription, FieldType.ASCII, (uint)str.Length, 0, str));

            str = PERQemu.Version;
            tags.Add(new TIFFtag(TagNames.Software, FieldType.ASCII, (uint)str.Length, 0, str));

            str = $"{PERQemu.Config.Current.Chassis} configuration '{PERQemu.Config.Current.Name}'";
            tags.Add(new TIFFtag(TagNames.HostComputer, FieldType.ASCII, (uint)str.Length, 0, str));

            str = DateTime.Now.ToString("yyyy:MM:dd HH:mm:ss");
            tags.Add(new TIFFtag(TagNames.DateTime, FieldType.ASCII, (uint)str.Length, 0, str));

            //
            // Create the IFD ("Global IFD", or initial page of multi-page?) from
            // the list of desired tags.
            //
            var ifd = new IFD((ushort)tags.Count, tags.ToArray(), 0);

            // Ask the IFD to run through the tags to sum up the size of the data
            // block needed for fields that don't fit inside it, updating offsets
            // in those tags as necessary
            var dataSize = ifd.DataSizeInBytes(offset);

            Log.Info(Category.Formatter, "Data block size: {0} bytes", dataSize);

            // THIS is now the start of our IFD!
            offset += dataSize;

            // Debug
            ifd.DumpIFD();

            // Build the file path and let 'er rip
            var fullpath = Paths.BuildOutputPath(filename);

            try
            {
                using (var fs = new FileStream(fullpath, FileMode.Create, FileAccess.Write))
                {
                    // Write the header
                    fs.WriteByte((byte)'M');
                    fs.WriteByte((byte)'M');
                    fs.WriteShort(42);
                    fs.WriteUInt(offset);

                    Log.Info(Category.Formatter, "TIFF header written, IFD at {0}", offset);

                    // Write the entire bitmap in one strip
                    fs.Write(_pageBuffer, 0, _pageBuffer.Length);

                    Log.Info(Category.Formatter, "Bitmap data written, start of IFD overflow data at {0}", fs.Position);

                    // Now write the extra data at the end of the image block
                    ifd.WriteExtendedData(fs);

                    // And finally, write the IFD itself!
                    ifd.WriteIFD(fs);

                    offset += ifd.SizeInBytes();
                    if (offset != fs.Position) Console.WriteLine($"AT END: offset={offset}, pos={fs.Position}");

                    // Close it up (todo: multi-page support...)
                    fs.Close();
                    return true;
                }
            }
            catch (Exception e)
            {
                Log.Info(Category.Formatter, "Failed to save output: {0}", e.Message);
                return false;
            }
        }

        /// <summary>
        /// Save the page buffer as raw pixel data, full resolution.  Limited 
        /// utility as a debugging aid; can view results online with a tool like
        /// https://rawpixels.net or other utilities.  May be removed.
        /// </summary>
        bool SavePageAsRaw(string filename)
        {
            var fullpath = Paths.BuildOutputPath(filename);

            using (var fs = new FileStream(fullpath, FileMode.Create, FileAccess.Write))
            {
                fs.Write(_pageBuffer, 0, _pageBuffer.Length);
                fs.Close();
            }
            return true;
        }

        //
        // TIFF support
        //

        struct TIFFtag
        {
            public TIFFtag(TagNames tag, FieldType type, uint len, uint data, object ext = null)
            {
                Tag = tag;
                TagType = type;
                FieldLength = len;
                DataOffset = data;
                ExtendedData = ext;
                _sizeInBytes = 0;

                _sizeInBytes = SetSizeInBytes();
            }

            public uint SizeInBytes => _sizeInBytes;

            public override string ToString()
            {
                return $"[TIFF: Tag {(ushort)Tag} ({Tag}, {TagType}) Count {FieldLength} " +
                        ((_sizeInBytes > 0) ? $"Offset {DataOffset} ExtSize {_sizeInBytes}]" : $"Data {DataOffset}]");
            }

            uint SetSizeInBytes()
            {
                uint numBytes = 0;

                switch (TagType)
                {
                    // Strings: FieldLength is the string length
                    case FieldType.ASCII:

                        FieldLength++;                  // Adjust for null terminator

                        // Always assume external, even for short strings
                        numBytes = FieldLength;
                        numBytes += (numBytes % 2);     // Pad if necessary
                        break;

                    // Words (short): if > 1, store as array
                    case FieldType.Word:
                    case FieldType.SShort:
                        if (FieldLength > 2 || ExtendedData != null)
                        {
                            numBytes = FieldLength * 2;
                        }
                        else if (FieldLength < 2)
                        {
                            // Inline data (single word) -- stored left justified
                            DataOffset <<= 16;
                        }
                        break;

                    // DWord (int): if > 1, store as array
                    case FieldType.DWord:
                        if (FieldLength > 1 || ExtendedData != null)
                        {
                            numBytes = FieldLength * 4;
                        }
                        break;

                    // Rational: This horrible format is two double words (numerator,
                    // denominator) as a single 8-byte data type.  We pass them in as
                    // one Long for "convenience"
                    case FieldType.Rational:
                        // Always external, count not (currently) checked...
                        numBytes = 8;
                        break;

                    default:
                        Log.Warn(Category.Formatter, "No setter for multibyte type {0}", TagType);
                        break;
                }

                return numBytes;
            }

            public void WriteToStream(Stream s)
            {
                s.WriteShort((ushort)Tag);
                s.WriteShort((ushort)TagType);
                s.WriteUInt(FieldLength);
                s.WriteUInt(DataOffset);
            }

            public void WriteExtendedDataToStream(Stream s)
            {
                if (_sizeInBytes == 0) return;

                switch (TagType)
                {
                    case FieldType.ASCII:
                        var str = (string)ExtendedData;

                        Console.WriteLine($"Extended data: string '{str}'");
                        s.WriteString(str);     // includes null term

                        if (str.Length + 1 < _sizeInBytes)
                        {
                            s.WriteByte(0);     // pad byte
                        }
                        break;

                    case FieldType.Word:
                        var sa = (ushort[])ExtendedData;

                        for (var i = 0; i < FieldLength; i++)
                        {
                            Console.WriteLine($"Extended data: short {sa[i]}");
                            s.WriteShort(sa[i]);
                        }
                        break;

                    case FieldType.DWord:
                        var da = (uint[])ExtendedData;

                        for (var i = 0; i < FieldLength; i++)
                        {
                            Console.WriteLine($"Extended data: uint {da[i]}");
                            s.WriteUInt(da[i]);
                        }
                        break;

                    case FieldType.Rational:
                        var rat = (ulong)ExtendedData;

                        s.WriteUInt((uint)(rat >> 32));
                        s.WriteUInt((uint)(rat & 0xffffffff));
                        break;

                    default:
                        Log.Warn(Category.Formatter, "Unimplemented writer for field type {0}", TagType);
                        break;
                }
            }

            public TagNames Tag;
            public FieldType TagType;
            public uint FieldLength;
            public uint DataOffset;

            object ExtendedData;
            uint _sizeInBytes;
        }

        /// <summary>
        /// A TIFF Image Format Dictionary.
        /// </summary>
        struct IFD
        {
            public IFD(ushort count, TIFFtag[] tags, uint next)
            {
                TagCount = count;
                Tags = tags;
                NextIFDOffset = next;

                // A "well formed TIFF" presents the tags in ascending numerical order
                Array.Sort(Tags, (x, y) => x.Tag.CompareTo(y.Tag));
            }

            public uint SizeInBytes()
            {
                // Compute our own size: tags + count word + next pointer
                return (uint)((TagCount * 12) + 2 + 4);
            }

            /// <summary>
            /// Walk through the Tags and update the DataOffset fields, summing
            /// up the total external data space needed on the way.
            /// </summary>
            public uint DataSizeInBytes(uint start)
            {
                uint total = 0;

                for (var i = 0; i < TagCount; i++)
                {
                    if (Tags[i].SizeInBytes > 0)
                    {
                        Tags[i].DataOffset = start;
                        total += Tags[i].SizeInBytes;
                        start += Tags[i].SizeInBytes;
                    }
                }

                return total;
            }

            /// <summary>
            /// Write the extended data for any tag that has some.  Assumes the
            /// current stream is positioned to the correct starting point and
            /// that offsets for the tags are in sorted, ascending order.  This
            /// is so gross.
            /// </summary>
            public void WriteExtendedData(Stream s)
            {
                for (var i = 0; i < TagCount; i++)
                {
                    if (Tags[i].SizeInBytes > 0)
                    {
                        Log.Info(Category.Formatter, "Writing out-of-band data for tag {0}, offset={1}, pos={2}",
                                                 Tags[i].Tag, Tags[i].DataOffset, s.Position);
                        Tags[i].WriteExtendedDataToStream(s);
                    }
                }
            }

            /// <summary>
            /// Write the IFD to the output stream.  Tags only, no external data.
            /// </summary>
            public void WriteIFD(Stream s)
            {
                Log.Info(Category.Formatter, "Writing IFD at position {0}", s.Position);
                s.WriteShort(TagCount);

                for (var i = 0; i < TagCount; i++)
                {
                    Tags[i].WriteToStream(s);
                }

                s.WriteUInt(NextIFDOffset);
            }

            // Debugging
            public void DumpIFD()
            {
                Console.WriteLine("TIFF tags:");
                for (var i = 0; i < TagCount; i++)
                    Console.WriteLine(Tags[i].ToString());
            }

            public ushort TagCount;
            public TIFFtag[] Tags;
            public uint NextIFDOffset;
        }

        enum TagNames : ushort
        {
            // Required for Baseline bilevel (Class B), TIFF 6.0:
            NewSubfileType = 254,
            ImageWidth = 256,
            ImageLength = 257,
            BitsPerSample = 258,
            Compression = 259,
            PhotometricInterpretation = 262,
            Thresholding = 263,
            StripOffsets = 273,
            Orientation = 274,
            RowsPerStrip = 278,
            StripByteCounts = 279,
            MinSampleValue = 280,
            MaxSampleValue = 281,
            XResolution = 282,
            YResolution = 283,
            ResolutionUnit = 296,
            SamplesPerPixel = 277,
            PageNumber = 297,
            DefaultImageColor = 434,
            // Optional things we'll add for fun:
            ImageDescription = 270,
            Software = 305,
            DateTime = 306,
            HostComputer = 316,
        }

        enum FieldType : ushort
        {
            Byte = 1,
            ASCII = 2,
            Word = 3,
            DWord = 4,
            Rational = 5,
            SByte = 6,
            Undefined = 7,
            SShort = 8,
            SLong = 9,
            SRational = 10,
            Float = 11,
            Double = 12
        }
    }
}
