//
//  IMDFormatHelper.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022, Boondoggle Heavy Industries, Ltd.
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

using System;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using System.Collections.Generic;

namespace PERQmedia
{
    /// <summary>
    /// Handles the nuts and bolts of decoding the IMD file format.
    /// </summary>
    public class IMDFormatHelper
    {
        public IMDFormatHelper()
        {
            _heads = 0;
            _sectorSize = 0;
            _sectors = new List<Sector>();
        }

        public byte IMDVersion => _imdVersion;
        public bool DoubleSided => (_heads == 1);
        public bool DoubleDensity => (_sectorSize == 256);


        /// <summary>
        /// Parse the IMD header line to verify the cookie and pull out the
        /// file Version and ArchiveDate.
        /// </summary>
        public bool TryParseHeader(Stream fs, StorageDevice dev)
        {
            //
            // The IMD header is an ASCII, null-terminated "C" string:
            // IMD v.v: dd/mm/yyyy hh:mm:ss
            //
            // So, we can read and parse the first line in one swell foop:
            //
            var cookie = fs.ReadString((byte)'\n');
            cookie.TrimEnd('\r');    // annoying

            // Perl will never die
            var pattern = @"^IMD (\d)\.(\d{1,2}): (.*)";
            var match = Regex.Match(cookie, pattern);

            if (match.Success && match.Groups.Count == 4)
            {
                //
                // I only set aside a byte for the PRQM-format file version, as
                // it is highly unlikely this niche format will be modified 255
                // more times. :-)  We scale the IMD v.vv format as a one byte
                // value so we can record it, though it'll always get written
                // back in the current format anyway.  This is very fussy and
                // will break if the IMD version (which hasn't changed in over
                // 10 years?) is ever rolled beyond version 2.55.  Heh. :-)
                //
                var major = Convert.ToByte(match.Groups[1].Value);
                var minor = Convert.ToByte(match.Groups[2].Value);
                dev.FileInfo.Version = (byte)(major * 100 + minor);

                if (dev.FileInfo.Version != _imdVersion)
                {
                    Console.WriteLine("Note: File was written with IMD version ({0}.{1})!", major, minor);
                    Console.WriteLine("Continuing...");
                }

                // Save our archive date for posterity
                dev.FileInfo.ArchiveDate = DateTime.Parse(match.Groups[3].Value);

                // That was the easy part...
                return true;
            }

            return false;
        }

        /// <summary>
        /// Create a new IMD header string and return its ASCII encoding.
        /// </summary>
        public byte[] EncodeHeader(StorageDevice dev)
        {
            var header = string.Format("IMD {0}.{1}: {2:MM/dd/yyyy H:mm:ss}\r\n",
                                       dev.FileInfo.Version / 100,
                                       dev.FileInfo.Version % 100,
                                       dev.FileInfo.ArchiveDate);

            // Retrieve the UTF-8 block or an empty string
            header += dev.FileInfo.DecodeTextLabel();

            // Return the whole shebang, ASCII encoded
            return Encoding.ASCII.GetBytes(header);
        }

        /// <summary>
        /// Read a track's worth of sectors.
        /// </summary>
        /// <returns><c>true</c>, if track was read, <c>false</c> if EOF.</returns>
        public void ReadTrack(Stream fs, StorageDevice dev)
        {
            // Get the track info
            fs.CheckRead(5);

            var format = (Modulation)fs.ReadByte();
            var cyl = (ushort)fs.ReadByte();
            var head = (byte)fs.ReadByte();
            var count = (ushort)fs.ReadByte();
            var sizeIndex = fs.ReadByte();

#if DEBUG
            //Console.WriteLine("Reading fmt {0} cyl {1} hd {2} count {3}, size {4} @ {5}",
            //                  format, cyl, head, count, _sectorSizes[sizeIndex], fs.Position);
#endif

            // Sanity check.  The 8" floppy format is fixed at 77 cylinders,
            // 2 heads, 26 sectors, and the PERQ only supported 128-256 byte
            // sectors.  This just ensures the stream isn't out of synch.
            if ((format > Modulation.MFM250) ||
                (cyl > 77) ||
                ((head & 0x3f) > 1) ||
                (count > 26) ||
                (sizeIndex > _sectorSizes.Length - 1))
            {

                throw new InvalidOperationException("Invalid track header data");
            }

            //
            // Pull flags out of the head byte.  The head # is just the first
            // bit.  If we see head #1, it's a double sided disk.
            // 
            bool bCylMap = (head & 0x80) != 0;
            bool bHeadMap = (head & 0x40) != 0;
            head = (byte)(head & 0x1);

            if (head > _heads)
            {
                _heads = head;
            }

            //
            // Read sector numbering (interleave) map
            //
            _sectorOrdering = new List<ushort>(count);

            for (int i = 0; i < count; i++)
            {
                _sectorOrdering.Add((ushort)fs.ReadByte());
            }
#if DEBUG
            //Console.WriteLine("Sector map: " + string.Join(" ", _sectorOrdering.ToArray()));
#endif
            //
            // At this time, cyl and head maps are not supported.  It's not
            // expected any PERQ disk would use such a format, so if they're
            // present the image probably isn't from a PERQ... but we'll just
            // skip over 'em and try to continue.
            //
            if (bCylMap)
            {
                Console.WriteLine("IMD Cylinder mas not supported (ignored).");
                fs.Seek(count, SeekOrigin.Current);
            }

            if (bHeadMap)
            {
                Console.WriteLine("IMD Head map not supported (ignored).");
                fs.Seek(count, SeekOrigin.Current);
            }

            //
            // Sector size should be consistent and generally not change
            // because that leaves the integrity of the whole thing in doubt.
            // The oddball PNX hybrid format _might_ be one case where this is
            // legitimately done (bootable POS partition, followed by a PNX
            // filesystem on the same disk!)
            //
            var size = _sectorSizes[sizeIndex];

            if ((_sectorSize > 0 && _sectorSize != size) || size > 256)
            {
                Console.WriteLine("WARNING: sector size changed from {0} to {1}", _sectorSize, size);
            }

            //
            // Save the largest size we've seen so far; this determines
            // which geometry we set at the end (but it _should_ be the
            // same for all the tracks on a properly formatted PERQ disk!)
            //
            if (size > _sectorSize)
            {
                _sectorSize = size;
            }

            //
            // Read the sector data in for the whole track.
            //
            for (var i = 0; i < count; i++)
            {
                // 
                // Allocate the new sector, accounting for interleave.  Note:
                // IMD respects traditional floppy sector numbering starting
                // from 1, not from 0.  So we just quietly map from 1..26 to
                // 0..25 when reading, then put them back when writing.  Ugh!
                // 
                var sec = _sectorOrdering[i];
                var sector = new Sector(cyl, head, (ushort)(sec - 1), _sectorSize, 0);

                SectorRecordType type = (SectorRecordType)fs.ReadByte();
#if DEBUG
                //Console.WriteLine($"Reading sector {i} (mapped {sec}), type {type} @ {fs.Position}");
#endif
                switch (type)
                {
                    case SectorRecordType.Unavailable:
                        // IMD has no data for this sector.  Fill with a known
                        // pattern.  I forget why.  (0xe5 is the default byte
                        // value laid down by the formatter?)
                        Fill(sector, 0xe5);
                        sector.IsBad = true;
                        break;

                    case SectorRecordType.Normal:
                        fs.Read(sector.Data, 0, _sectorSize);
                        break;

                    case SectorRecordType.NormalError:
                    case SectorRecordType.NormalDeleted:
                    case SectorRecordType.DeletedError:
                        // Sector was marked in error but the data exists
                        fs.Read(sector.Data, 0, _sectorSize);
                        sector.IsBad = true;
                        break;

                    case SectorRecordType.Compressed:
                        Fill(sector, (byte)fs.ReadByte());
                        break;

                    case SectorRecordType.CompressedError:
                    case SectorRecordType.CompressedDeleted:
                    case SectorRecordType.CompressedDeletedError:
                        Fill(sector, (byte)fs.ReadByte());
                        sector.IsBad = true;
                        break;

                    default:
                        throw new InvalidOperationException(
                            string.Format("Unexpected IMD sector data type {0}", type));
                }

                // Just push it onto the list of read sectors; we'll write
                // them all out once we know the final geometry
                _sectors.Add(sector);
            }
        }

        /// <summary>
        /// Fill all the bytes in a sector with a particular value.
        /// </summary>
        private void Fill(Sector s, byte val)
        {
            for (uint i = 0; i < s.Data.Length; i++)
                s.WriteByte(i, val);
        }

        /// <summary>
        /// Assign the (linear) list of de-interleaved sectors to their
        /// proper slot in the C/H/S-addressed device data array.
        /// </summary>
        public void MapTracksToDevice(StorageDevice dev)
        {
#if DEBUG
            //
            // This will happen on almost every IMD floppy since the PERQ
            // never formatted track 0... All of the PERQmedia formatters
            // will write out the blank track 0 to avoid this warning.
            //
            if (_sectors.Count != dev.Geometry.TotalBlocks)
                Console.WriteLine("Sector count from image {0} doesn't match expected {1}!",
                                  _sectors.Count, dev.Geometry.TotalBlocks);
            else
                Console.WriteLine("Copying {0} sectors", _sectors.Count);
#endif
            foreach (var sec in _sectors)
            {
                dev.Write(sec);
            }

            // Can free the list now...
            _sectors.Clear();
        }

        /// <summary>
        /// Scan a buffer to see if all the bytes are the same so we can use
        /// IMD's simple run-length compression (all or nothin').
        /// </summary>
        public bool CanCompress(byte[] data, out byte fillByte)
        {
            // An empty sector can be null filled
            if (data.Length == 0)
            {
                fillByte = 0;
                return true;
            }

            fillByte = data[0];

            for (int i = 1; i < data.Length; i++)
            {
                if (data[i] != fillByte) return false;
            }

            return true;
        }

        /// <summary>
        /// Writes out a full image in IMD format.
        /// </summary>
        public void WriteTracks(Stream fs, StorageDevice dev)
        {
            // Init
            _heads = (byte)(dev.Geometry.Heads - 1);    // 0..1
            _sectorSize = dev.Geometry.SectorSize;

            byte format = DoubleDensity ? (byte)Modulation.MFM500 : (byte)Modulation.FM500;
            byte count = (byte)dev.Geometry.Sectors;
#if DEBUG
            Console.WriteLine($"WriteTracks {_heads} heads, size {_sectorSize}, fmt {format}, count {count}");
#endif
            // We only officially support two sector sizes (128 or 256) but
            // let's actually look up the code rather than fudge it
            byte sizeIndex = 0;
            while (sizeIndex < _sectorSizes.Length && _sectorSize != _sectorSizes[sizeIndex])
            {
                sizeIndex++;
            }
#if DEBUG
            // Sanity check
            if (sizeIndex > 1)
            {
                Console.WriteLine($"SizeIndex {sizeIndex} ({_sectorSizes[sizeIndex]} bytes) makes no sense");
            }
#endif
            // Start makin' tracks
            for (byte c = 0; c < dev.Geometry.Cylinders; c++)
            {
                for (byte h = 0; h < dev.Geometry.Heads; h++)
                {
                    // Write header for this track (cyl, head)
                    fs.WriteByte(format);
                    fs.WriteByte(c);
                    fs.WriteByte(h);
                    fs.WriteByte(count);
                    fs.WriteByte(sizeIndex);

                    // Write the sector map.  Remaps from 0..25 back to 1..26!
                    for (byte i = 1; i <= count; i++)
                    {
                        fs.WriteByte(i);
                    }
#if DEBUG
                    Console.WriteLine("Writing fmt {0} cyl {1} hd {2} count {3}, size {4} @ {5}",
                                      (Modulation)format, c, h, count, _sectorSizes[sizeIndex], fs.Position);
#endif
                    // Write the sectors in order, no interleave
                    for (byte s = 0; s < count; s++)
                    {
                        // Fetch the sector and figure out a type
                        Sector sec = dev.Sectors[c, h, s];
                        byte fill = 0;

                        if (sec.Data == null || sec.Data.Length == 0)
                        {
                            // No data was read; this is probably an error
                            // since we remap "unavailable" to "error"...
                            fs.WriteByte((byte)SectorRecordType.Unavailable);
                        }
                        else if (sec.IsBad)
                        {
                            if (CanCompress(sec.Data, out fill))
                            {
                                fs.WriteByte((byte)SectorRecordType.CompressedError);
                                fs.WriteByte(fill);
                            }
                            else
                            {
                                fs.WriteByte((byte)SectorRecordType.NormalError);
                                fs.Write(sec.Data, 0, _sectorSize);
                            }
                        }
                        else
                        {
                            if (CanCompress(sec.Data, out fill))
                            {
                                fs.WriteByte((byte)SectorRecordType.Compressed);
                                fs.WriteByte(fill);
                            }
                            else
                            {
                                fs.WriteByte((byte)SectorRecordType.Normal);
                                fs.Write(sec.Data, 0, _sectorSize);
                            }
                        }
                    }
                }
            }
        }

        //
        // Note: PERQMedia reads but does not write all of these IMD sector
        // types.  See the ReadMe file for more details.
        //
        // 00      Sector data unavailable - could not be read
        // 01 .... Normal data: (Sector Size) bytes follow
        // 02 xx Compressed: All bytes in sector have same value(xx)
        // 03 .... Normal data with "Deleted-Data address mark"
        // 04 xx Compressed  with "Deleted-Data address mark"
        // 05 .... Normal data read with data error
        // 06 xx Compressed  read with data error
        // 07 .... Deleted data read with data error
        // 08 xx Compressed, Deleted read with data error
        // 
        private enum SectorRecordType
        {
            Unavailable = 0,
            Normal = 1,
            Compressed = 2,
            NormalDeleted = 3,
            CompressedDeleted = 4,
            NormalError = 5,
            CompressedError = 6,
            DeletedError = 7,
            CompressedDeletedError = 8,
        }

        // 00 = 500 kbps FM   \   Note:   kbps indicates transfer rate,
        // 01 = 300 kbps FM    >          not the data rate, which is
        // 02 = 250 kbps FM   /           1/2 for FM encoding.
        // 03 = 500 kbps MFM
        // 04 = 300 kbps MFM
        // 05 = 250 kbps MFM
        public enum Modulation
        {
            Invalid = -1,
            FM500 = 0,
            FM300 = 1,
            FM250 = 2,
            MFM500 = 3,
            MFM300 = 4,
            MFM250 = 5,
        }


        // Temporary storage for reading 'em in
        private List<Sector> _sectors;
        private List<ushort> _sectorOrdering;

        // Flags based on the data that comes in
        private byte _heads;
        private ushort _sectorSize;

        // Sector sizes that IMD supports
        private static ushort[] _sectorSizes = { 128, 256, 512, 1024, 2048, 4096, 8192 };

        // Compatible version: IMD 1.18
        private readonly byte _imdVersion = 118;
    }
}
