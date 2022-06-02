//
//  PRQFormatter.cs
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

using PERQemu;

namespace PERQmedia
{
    /// <summary>
    /// Reads and writes the PERQmedia format.
    /// </summary>
    public class PRQFormatter : IMediaFormatter
    {
        public PRQFormatter()
        {
            // Our format helper should persist across header/data reads
            _helper = null;
        }

        public string Name => "PERQmedia Common Storage format";

        public bool ReadHeader(Stream fs, StorageDevice dev)
        {
            // Wrap with the CRC32 stream
            using (var crcfs = new CRC32Stream(fs))
            {
                return ReadHeaderWithCRC(crcfs, dev);
            }
        }

        public bool ReadData(Stream fs, StorageDevice dev)
        {
            using (var crcfs = new CRC32Stream(fs))
            {
                return ReadDataWithCRC(crcfs, dev);
            }
        }

        public bool Write(Stream fs, StorageDevice dev)
        {
            using (var crcfs = new CRC32Stream(fs))
            {
                return WriteWithCRC(crcfs, dev);
            }
        }

        /// <summary>
        /// Read the header section and verify that the file contains a
        /// PRQM-formatted image.
        /// </summary>
        private bool ReadHeaderWithCRC(CRC32Stream fs, StorageDevice dev)
        {
            try
            {
                // Reset the CRC
                CRC32Stream.ResetChecksum();

                // Reset the helper
                _helper = new PRQFormatHelper();

                // Scan the stream to see if it's a PRQM file
                if (!_helper.CheckHeader(fs))
                {
                    fs.Seek(0, SeekOrigin.Begin);   // Rewind
                    return false;
                }

                // Finish setting up our header
                dev.FileInfo.Format = Formatters.PRQFormat;
                dev.Info.Type = (DeviceType)fs.ReadByte();

                _helper.ReadDirectory(fs);
#if DEBUG
                Log.Detail(Category.MediaLoader, "Directory check:");
                _helper.PrintDirectory();

                Log.Detail(Category.MediaLoader, "Read header, file pos=" + fs.Position);
#endif
                // Is there a text label?
                if (_helper.TextLabelSize > 0)
                {
                    fs.CheckRead(_helper.TextLabelSize);

                    var label = new byte[_helper.TextLabelSize];
                    fs.Read(label, 0, label.Length);
                    dev.FileInfo.TextLabel = label;
                }

                Log.Detail(Category.MediaLoader, "Read text label, file pos={0}", fs.Position);

                // Is there an image label?
                if (_helper.ImageLabelSize > 0)
                {
                    fs.CheckRead(_helper.ImageLabelSize);

                    var label = new byte[_helper.ImageLabelSize];
                    fs.Read(label, 0, label.Length);
                    dev.FileInfo.ImageLabel = label;
                }

                Log.Detail(Category.MediaLoader, "Read image label, file pos={0}", fs.Position);

                // Now the info stuff
                dev.FileInfo.FSType = (FilesystemHint)fs.ReadByte();
                dev.FileInfo.ArchiveDate = DateTime.FromBinary(fs.ReadLong());
                dev.FileInfo.ArchivedBy = fs.ReadString();

                dev.Info.Name = fs.ReadString();
                dev.Info.Description = fs.ReadString();
                var flags = fs.ReadShort();

                // Decode the individual flags
                dev.Info.IsWritable = (flags & 0x1) != 0;
                dev.Info.IsBootable = (flags & 0x2) != 0;
                dev.Info.IsRemovable = (flags & 0x4) != 0;

                // Geometry
                var cyls = fs.ReadShort();
                var heads = (byte)fs.ReadByte();
                var secs = fs.ReadShort();
                var secSize = fs.ReadShort();
                var headSize = (byte)fs.ReadByte();
                dev.Geometry = new DeviceGeometry(cyls, heads, secs, secSize, headSize);

                // Drive specs
                var rpm = fs.ReadInt();
                var pulse = fs.ReadInt();
                var delay = fs.ReadInt();
                var minSeek = fs.ReadInt();
                var maxSeek = fs.ReadInt();
                var settle = fs.ReadInt();
                var xferRate = fs.ReadInt();
                dev.Specs = new DevicePerformance(rpm, pulse, delay, minSeek, maxSeek, settle, xferRate);

                Log.Detail(Category.MediaLoader, "Read info, file pos={0}", fs.Position);
                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Not a valid PRQM image: {0}", e.Message);
                return false;
            }
        }

        /// <summary>
        /// Read the data section from a PRQM file.
        /// </summary>
        private bool ReadDataWithCRC(CRC32Stream fs, StorageDevice dev)
        {
            if (fs.Position != _helper.DataStart)
            {
                Log.Debug(Category.MediaLoader,
                          "Not in start position {0}?! File pos={1}",
                          _helper.DataStart, fs.Position);
                // Seek there automatically?  Bomb?  (Implies as bad directory or header read)
            }

            // Directory tells us how much data space we'll need
            _helper.Data = new byte[_helper.DataSize];

            Log.Detail(Category.MediaLoader, "Allocated {0} bytes for data slurp", _helper.DataSize);

            // Slurp it in!
            if (fs.Read(_helper.Data, 0, _helper.DataSize) != _helper.DataSize)
            {
                // If we didn't get all the data, we're boned
                throw new EndOfStreamException("Short read of data section");
            }

            // Grab the CRC through the end of data
            var crcFromStream = CRC32Stream.ReadCRC;

            // Read in the saved CRC and compare it to the computed result
            var crcFromFile = fs.OuterStream.ReadUInt();

            Log.Debug(Category.MediaLoader, "Stored CRC is {0:x8}, computed {1:x8}",
                      crcFromFile, crcFromStream);

            if (crcFromFile != crcFromStream)
            {
                Log.Warn(Category.MediaLoader,
                         "CRC32 mismatch: Stored CRC is {0:x8}, computed {1:x8}",
                         crcFromFile, crcFromStream);
                return false;
            }

            // Make sure we didn't underrun (meaning, bad file)
            if (fs.Position != fs.Length)
            {
                Log.Debug(Category.MediaLoader, "All done.  file pos = " + fs.Position);
                Log.Debug(Category.MediaLoader, "Extra data? file len = " + fs.Length);
                return false;
            }

            // File read complete!  Determine how much data if uncompressed
            var blocks = dev.Geometry.TotalBlocks;
            var total = dev.Geometry.TotalBytes;

            if (total > _helper.DataSize)
            {
                Log.Debug(Category.MediaLoader, "Data size mismatch: computed {0} != dir {1}",
                          total, _helper.DataSize);
                Log.Debug(Category.MediaLoader, "Assuming the data is compressed!");

                // Decompress the data
                _helper.Decompress();

                if (_helper.Data.Length != total)
                {
                    Log.Warn(Category.MediaLoader, 
                              "Data size mismatch: computed {0} != actual {1}",
                              total, _helper.Data.Length);
                    // Probably screwed at this point?
                }
            }

            // Make room for the data
            dev.CreateSectors();

            var index = 0;

            // Turn the bytes into Sectors
            while (blocks-- != 0)
            {
                var c = (ushort)(_helper.Data[index++] << 8 | _helper.Data[index++]);
                var h = _helper.Data[index++];
                var s = (ushort)(_helper.Data[index++] << 8 | _helper.Data[index++]);
                var bad = _helper.Data[index++] != 0;

                dev.Sectors[c, h, s] = new Sector(c, h, s, dev.Geometry.SectorSize, dev.Geometry.HeaderSize, bad);

                // Copy the header data
                for (uint i = 0; i < dev.Geometry.HeaderSize; i++)
                {
                    dev.Sectors[c, h, s].WriteHeaderByte(i, _helper.Data[index++]);
                }

                // Copy the data bytes
                for (uint i = 0; i < dev.Geometry.SectorSize; i++)
                {
                    dev.Sectors[c, h, s].WriteByte(i, _helper.Data[index++]);
                }
            }

            return true;
        }

        /// <summary>
        /// Write a PERQmedia archive from a StorageDevice to the opened
        /// stream.  Compresses the data and computes the CRC.
        /// </summary>
        private bool WriteWithCRC(CRC32Stream fs, StorageDevice dev)
        {
            // Reset the CRC
            CRC32Stream.ResetChecksum();

            // Create a new helper object
            _helper = new PRQFormatHelper();

            // Initialize the directory
            _helper.InitFromDev(dev);

#if DEBUG 
            Log.Detail(Category.MediaLoader, "Before compression:");
            _helper.PrintDirectory();
            var sizeBefore = (double)_helper.DataSize;
#endif 
            // Call the helper to compress the stream!
            _helper.Compress(dev);

#if DEBUG 
            Log.Detail(Category.MediaLoader, "After compression:");
            _helper.PrintDirectory();
            var sizeAfter = (double)_helper.DataSize;

            Log.Info(Category.MediaLoader, "Compression ratio = {0:N}", (sizeBefore / sizeAfter));
            Log.Info(Category.MediaLoader, "Space savings = {0:N}%", 100.0 * (1.0 - (sizeAfter / sizeBefore)));
#endif 
            // Start with the fixed header
            _helper.WriteHeader(fs);
            _helper.WriteDirectory(fs);

#if DEBUG
            Log.Detail(Category.MediaLoader, "Wrote header, file pos=" + fs.Position);
            _helper.PrintDirectory();
#endif 
            // Text label
            if (_helper.TextLabelSize > 0)
            {
                fs.Write(dev.FileInfo.TextLabel, 0, _helper.TextLabelSize);
            }

            // Image label
            if (_helper.ImageLabelSize > 0)
            {
                fs.Write(dev.FileInfo.ImageLabel, 0, _helper.ImageLabelSize);
            }

            // Info
            fs.WriteByte((byte)dev.FileInfo.FSType);
            fs.WriteLong(dev.FileInfo.ArchiveDate.ToBinary());
            fs.WriteString(dev.FileInfo.ArchivedBy);

            fs.WriteString(dev.Info.Name);
            fs.WriteString(dev.Info.Description);

            // Encode the individual flags
            var flags = (ushort)((dev.Info.IsWritable ? 0x1 : 0) |
                                 (dev.Info.IsBootable ? 0x2 : 0) |
                                 (dev.Info.IsRemovable ? 0x4 : 0));

            fs.WriteShort(flags);

            fs.WriteShort(dev.Geometry.Cylinders);
            fs.WriteByte(dev.Geometry.Heads);
            fs.WriteShort(dev.Geometry.Sectors);
            fs.WriteShort(dev.Geometry.SectorSize);
            fs.WriteByte(dev.Geometry.HeaderSize);

            fs.WriteInt(dev.Specs.RPM);
            fs.WriteInt(dev.Specs.IndexPulse);
            fs.WriteInt(dev.Specs.StartupDelay);
            fs.WriteInt(dev.Specs.MinimumSeek);
            fs.WriteInt(dev.Specs.MaximumSeek);
            fs.WriteInt(dev.Specs.HeadSettling);
            fs.WriteInt(dev.Specs.TransferRate);

            // Data!
            fs.Write(_helper.Data, 0, _helper.DataSize);

            // Get the computed CRC
            uint crc = CRC32Stream.WriteCRC;

            Log.Debug(Category.MediaLoader, "Saving computed CRC: {0:x8}", crc);

            // Write it to the file
            fs.OuterStream.WriteUInt(crc);

            return true;
        }

        private PRQFormatHelper _helper;
    }
}
