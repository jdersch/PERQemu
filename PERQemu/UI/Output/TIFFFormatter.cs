//
// TiffFormatter.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.IO;
using System.Collections.Generic;

using PERQmedia;

namespace PERQemu.UI.Output
{
    public class TIFFFormatter
    {
        public TIFFFormatter(string desc)
        {
            _description = desc;
        }

        /// <summary>
        /// Save a one-page TIFF.
        /// </summary>
        public void Save(Page page, Stream fs)
        {
            uint size = WritePage(page, fs, 0, 1);

            if (size != fs.Position)
                Console.WriteLine($"Final offset doesn't match position: {size} != {fs.Position}");
        }

        /// <summary>
        /// Save a multi-page TIFF.
        /// </summary>
        public void Save(List<Page> pages, Stream fs)
        {
            var numPages = pages.Count;
            uint offset = 0;

            while (pages.Count > 0)
            {
                offset = WritePage(pages[0], fs, offset, numPages);
                pages.RemoveAt(0);
            }

            if (offset != fs.Position)
                Console.WriteLine($"Final offset doesn't match position: {offset} != {fs.Position}");
        }

        /// <summary>
        /// Writes the header cookie and global IFD.
        /// </summary>
        uint WritePage(Page page, Stream fs, uint offset, int numPages)
        {
            var first = (page.PageNumber == 0);
            var last = (page.PageNumber + 1 == numPages);

            Log.Info(Category.Formatter, "Starting page {0} of {1} at offset {2}",
                     page.PageNumber + 1, numPages, offset);
            
            if (first) offset = 8;      // Allow for header
            
            //
            // Create a list of tags we'll use, and populate it without worrying
            // about order or offsets for out-of-line data
            //
            var tags = new List<TIFFtag>();

            // Image data
            tags.Add(new TIFFtag(TagNames.ImageWidth, FieldType.Word, 1, page.BitWidth));
            tags.Add(new TIFFtag(TagNames.ImageLength, FieldType.Word, 1, page.BitHeight));
            tags.Add(new TIFFtag(TagNames.BitsPerSample, FieldType.Word, 1, 1));        // 1 bpp
            tags.Add(new TIFFtag(TagNames.SamplesPerPixel, FieldType.Word, 1, 1));      // 1 bpp
            tags.Add(new TIFFtag(TagNames.ResolutionUnit, FieldType.Word, 1, 2));       // Inch
            tags.Add(new TIFFtag(TagNames.MinSampleValue, FieldType.Word, 1, 0));
            tags.Add(new TIFFtag(TagNames.MaxSampleValue, FieldType.Word, 1, 1));
            tags.Add(new TIFFtag(TagNames.DefaultImageColor, FieldType.Word, 1, 1));

            // Rational types are out-of-band (8 bytes)
            var hack = (((ulong)page.Resolution * 10000U) << 32) + 10000U;                  // But why?
            tags.Add(new TIFFtag(TagNames.XResolution, FieldType.Rational, 1, 0, hack));
            tags.Add(new TIFFtag(TagNames.YResolution, FieldType.Rational, 1, 0, hack));

            // One strip for the bitmap? Force these external since the validator
            // seems to expect an array for Offsets and ByteCounts. :-|
            tags.Add(new TIFFtag(TagNames.RowsPerStrip, FieldType.DWord, 1, page.BitHeight));
            tags.Add(new TIFFtag(TagNames.StripOffsets, FieldType.DWord, 1, offset));
            tags.Add(new TIFFtag(TagNames.StripByteCounts, FieldType.DWord, 1, (uint)page.Buffer.Length));

            // Bump to skip over the bitmap data
            offset += (uint)page.Buffer.Length;

            // File info
            tags.Add(new TIFFtag(TagNames.NewSubfileType, FieldType.DWord, 1, 2));
            tags.Add(new TIFFtag(TagNames.Compression, FieldType.Word, 1, 1));
            tags.Add(new TIFFtag(TagNames.PhotometricInterpretation, FieldType.Word, 1, 1));
            tags.Add(new TIFFtag(TagNames.Orientation, FieldType.Word, 1, 1));          // TopLeft

            // Oh, TIFF. :-/
            uint pg = (uint)((page.PageNumber << 16) + numPages);
            tags.Add(new TIFFtag(TagNames.PageNumber, FieldType.Word, 2, pg));

            if (first)
            {
                // Optional extras - only in the first IFD?
                var str = _description;
                tags.Add(new TIFFtag(TagNames.ImageDescription, FieldType.ASCII, (uint)str.Length, 0, str));

                str = PERQemu.Version;
                tags.Add(new TIFFtag(TagNames.Software, FieldType.ASCII, (uint)str.Length, 0, str));

                str = $"{PERQemu.Config.Current.Chassis} configuration '{PERQemu.Config.Current.Name}'";
                tags.Add(new TIFFtag(TagNames.HostComputer, FieldType.ASCII, (uint)str.Length, 0, str));

                str = DateTime.Now.ToString("yyyy:MM:dd HH:mm:ss");
                tags.Add(new TIFFtag(TagNames.DateTime, FieldType.ASCII, (uint)str.Length, 0, str));
            }

            //
            // Create the IFD from the list of tags
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

            // First page?  Write the header
            if (first)
            {
                fs.WriteByte((byte)'M');
                fs.WriteByte((byte)'M');
                fs.WriteShort(42);
                fs.WriteUInt(offset);       // First IFD immediately follows

                Log.Info(Category.Formatter, "TIFF header written, IFD at {0}", offset);
            }
            else
            {
                Log.Info(Category.Formatter, "Next IFD link at {0}", offset);
                fs.WriteUInt(offset);       // Pointer to here from PREVIOUS IFD
            }

            // Write the entire bitmap in one strip
            fs.Write(page.Buffer, 0, page.Buffer.Length);

            Log.Info(Category.Formatter, "Bitmap data written, start of IFD overflow data at {0}", fs.Position);

            // Now write the extra data at the end of the image block
            ifd.WriteExtendedData(fs);

            // And finally, write the IFD itself!
            ifd.WriteIFD(fs);
            offset += ifd.SizeInBytes();

            if (last)
            {
                // Final IFD in the chain
                fs.WriteUInt(0);
            }

            // Can't point to next IFD until we compute where it'll land, so for
            // now leave it unwritten.  Offset is 4 bytes short of fs.Position.
            // BRILLIANT format, guys.
            return offset;
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
            /// Does NOT write the link to the next IFD since that isn't known yet
            /// unless we walk the ENTIRE ()*$!)&% page list in advance.
            /// </summary>
            public void WriteIFD(Stream s)
            {
                Log.Info(Category.Formatter, "Writing IFD at position {0}, {1} bytes", s.Position, SizeInBytes());
                s.WriteShort(TagCount);

                for (var i = 0; i < TagCount; i++)
                {
                    Tags[i].WriteToStream(s);
                }
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

        string _description;
    }
}
