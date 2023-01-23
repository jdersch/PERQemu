//
// TAPFormatter.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQemu;

namespace PERQmedia
{
    /// <summary>
    /// Reads and writes the "TAP" format, a magtape format defined by Bob Supnik
    /// for use with SIMH.
    /// </summary>
    /// <remarks>
    /// For PERQemu (at least in this initial implementation) we assume that all
    /// blocks are fixed at 512 bytes, as defined by the Archive Sidewinder
    /// documentation and the PERQ's software interface.
    /// </remarks>
    public class TAPFormatter : IMediaFormatter
    {
        public string Name => "SIMH Magtape Storage format";
        public byte Version => (byte)'0';

        /// <summary>
        /// TAP format doesn't specify any kind of a formal header, but we add a
        /// custom "extended" marker to identify ones written by PERQmedia.  If
        /// that header exists we can populate all of the metadata just like a
        /// PRQM archive.  Otherwise we try to validate the file by reading at
        /// least one 512-byte block.  If found, and the leading and trailing
        /// data markers appear to match, then we assume the image is okay and
        /// isn't random gibberish.
        /// </summary>
        public bool ReadHeader(Stream fs, StorageDevice dev)
        {
            try
            {
                uint recordType = 0;
                uint dataLength = 0;

                bool found = false;
                var count = 0;

                _fileVersion = 0;
                _driveType = 0;

                while (count < 10 && !found)
                {
                    // Read the first word
                    recordType = fs.ReadUInt();

                    // An End Of Media mark with no valid data records?  Hmmm.
                    if ((Marker)recordType == Marker.EndOfMedia && count == 0)
                    {
                        Log.Debug(Category.MediaLoader, "EOM at record {0}, blank or invalid tape?", count);
                        return false;
                    }

                    // Does the file contain a PRQT cookie?  Then it's one of ours!
                    if ((Marker)recordType == Marker.Cookie)
                    {
                        found = true;

                        for (var b = 0; b < _cookie.Length; b++)
                        {
                            if (fs.ReadByte() != _cookie[b])
                            {
                                Log.Debug(Category.MediaLoader, "Bad cookie!");
                                found = false;      // False alarm
                            }
                        }

                        _fileVersion = (byte)fs.ReadByte();
                        _driveType = (byte)fs.ReadByte();

                        if (found && _fileVersion == Version)
                        {
                            // Got a good cookie!  Must be ours, then!
                            Log.Debug(Category.MediaLoader, "Found a PERQmedia cookie!");
                            break;
                        }
                    }

                    // Tape mark?  Erase Gap or reserved word?  Other "class E"
                    // metadata?  For now, we'll ignore these, up to a point; a
                    // file that starts with n tape marks or "erase gaps" is
                    // almost certain to be bogus, but don't give up too easily?
                    if (recordType == 0x0 || recordType >= 0x90000000)
                    {
                        count++;
                    }
                    else
                    {
                        // Might be a valid data record: compute the block length
                        dataLength = recordType & 0x00ffffff;

                        // If it isn't a PERQ-sized block, bail out
                        if (dataLength != 512)
                        {
                            Log.Debug(Category.MediaLoader, "Image has non-PERQ block size: {0} bytes", dataLength);
                            return false;
                        }

                        // Read and toss the data; if the file is valid there has to
                        // be at least this much data available
                        var buf = new byte[dataLength];
                        fs.Read(buf, 0, (int)dataLength);

                        // Read the next word -- it has to match, or the record is bogus
                        dataLength = fs.ReadUInt();

                        if (dataLength != recordType)
                        {
                            Log.Debug(Category.MediaLoader, "First data block has length mismatch ({0} != {1})", dataLength, recordType);
                            return false;
                        }

                        // Must be good then
                        found = true;
                    }
                }

                // Never found a valid data block
                if (!found)
                {
                    Log.Debug(Category.MediaLoader, "No valid data marker in first {0} reads", count);
                    return false;
                }

                // Rewind
                fs.Seek(0, SeekOrigin.Begin);

                // Fake up the StorageDevice metadata
                dev.Info = DeviceInfo.A3020;
                dev.Specs = DevicePerformance.Archive30IPS;
                dev.Geometry = DeviceGeometry.QIC20;

                dev.FileInfo.Format = Formatters.TAPFormat;
                dev.FileInfo.FSType = FilesystemHint.Stut;      // Assumed, for now

                //
                // If the extended header is present those records are always
                // written at the start of the file.  If we read a valid version
                // byte (char '0' initially) then we'll read in and parse those
                // now and leave the file positioned for ReadData to start
                // loading sectors below.
                //
                if (_fileVersion == Version)
                {
                    Log.Info(Category.MediaLoader, "TAP formatter: Reading extended header...");

                    found = false;      // Stop when the first datablock is found

                    while (!found)
                    {
                        recordType = fs.ReadUInt();
                        var tag = recordType & 0xff000000;
                        var size = recordType & 0x00ffffff;

                        // Any non-metadata marker signals we've read the header
                        // data; back up by 4 bytes to position for ReadData
                        if (recordType < (uint)Marker.Cookie || recordType > (uint)Marker.DevSpecs)
                        {
                            Log.Debug(Category.MediaLoader, "Header complete at pos {0}", fs.Position);
                            found = true;
                            break;
                        }

                        if (recordType == (uint)Marker.Cookie)
                        {
                            size = fs.ReadUInt();                   // Skip over 'PRQT'
                            dev.FileInfo.Version = (byte)fs.ReadByte();
                            dev.Info.Type = (DeviceType)fs.ReadByte();
                        }
                        else if (tag == (uint)Marker.TextLabel)
                        {
                            fs.CheckRead((int)size);

                            var label = new byte[size];
                            fs.Read(label, 0, label.Length);
                            dev.FileInfo.TextLabel = label;

                            if ((size & 1) != 0) fs.ReadByte();     // Toss pad byte
                        }
                        else if (tag == (uint)Marker.ImageLabel)
                        {
                            fs.CheckRead((int)size);

                            var label = new byte[size];
                            fs.Read(label, 0, label.Length);
                            dev.FileInfo.TextLabel = label;

                            if ((size & 1) != 0) fs.ReadByte();     // Toss pad byte
                        }
                        else if (tag == (uint)Marker.DevInfo)
                        {
                            dev.FileInfo.FSType = (FilesystemHint)fs.ReadByte();
                            dev.FileInfo.ArchiveDate = DateTime.FromBinary(fs.ReadLong());
                            dev.FileInfo.ArchivedBy = fs.ReadString();

                            dev.Info.Name = fs.ReadString();
                            dev.Info.Description = fs.ReadString();
                            var flags = fs.ReadShort();

                            // Decode the individual flags (same format as PRQM)
                            dev.Info.IsWritable = (flags & 0x1) != 0;
                            dev.Info.IsBootable = (flags & 0x2) != 0;
                            dev.Info.IsRemovable = (flags & 0x4) != 0;

                            // Read the pad byte, if present
                            if ((size & 1) != 0) fs.ReadByte();     // Toss pad byte
                        }
                        else if (recordType == (uint)Marker.DevGeometry)
                        {
                            var cyls = fs.ReadShort();
                            var heads = (byte)fs.ReadByte();
                            var secs = fs.ReadShort();
                            var secSize = fs.ReadShort();
                            var headSize = (byte)fs.ReadByte();
                            dev.Geometry = new DeviceGeometry(cyls, heads, secs, secSize, headSize);
                        }
                        else if (recordType == (uint)Marker.DevSpecs)
                        {
                            var rpm = fs.ReadInt();
                            var pulse = fs.ReadInt();
                            var delay = fs.ReadInt();
                            var minSeek = fs.ReadInt();
                            var maxSeek = fs.ReadInt();
                            var settle = fs.ReadInt();
                            var xferRate = fs.ReadInt();
                            dev.Specs = new DevicePerformance(rpm, pulse, delay, minSeek, maxSeek, settle, xferRate);
                        }
                        else
                        {
                            Log.Warn(Category.MediaLoader, "TAP formatter: Unknown class E marker: {0:x} at pos {1}", recordType, fs.Position);
                        }

                        // Verify the record
                        var checkType = fs.ReadUInt();

                        if (checkType != recordType)
                        {
                            Log.Error(Category.MediaLoader, "TAP formatter: Data record mismatch: {0:x} != {1:x}, file corrupt?", recordType, checkType);
                            return false;
                        }
                    }

                    // Jump back, Jack.  Rewind 4 bytes to re-read the record
                    fs.Seek(-4, SeekOrigin.Current);
                    Log.Detail(Category.MediaLoader, "Header rewind, now at pos {0}", fs.Position);
                }

                // At last!
                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Not a valid TAP image: {0}", e.Message);
                return false;
            }
        }

        /// <summary>
        /// Read the data section of a TAP file.  Recognizes only data blocks;
        /// our custom markers should have been consumed by ReadHeader!  Will
        /// try to continue if non-PERQmedia created TAP files are read.
        /// </summary>
        public bool ReadData(Stream fs, StorageDevice dev)
        {
            try
            {
                // Because .tap files are sparse, we preallocate all of our
                // sectors to prevent problems trying to read an empty tape
                dev.Format();

                // Working set
                var data = new byte[dev.Geometry.SectorSize];       // 512 bytes
                var header = new byte[dev.Geometry.HeaderSize];     // 4 bytes
                var EOM = false;
                var block = 0;
                var head = 0;
                var sector = 0;

                Log.Debug(Category.MediaLoader, "Data read starting at pos {0}...", fs.Position);

                // I'm here to chew bubble gum and read tape blocks!
                while (!EOM)
                {
                    var bad = false;
                    var recordType = fs.ReadUInt();

                    if (recordType == (uint)Marker.EndOfMedia)
                    {
                        // ...and I'm all out of tape blocks.
                        EOM = true;
                        Log.Debug(Category.MediaLoader, "Read EOM marker at block {0}, EOF is {1}", block, (fs.Position == fs.Length));
                        FillBlock(0xff, ref data);
                    }
                    else if (recordType >= 0xff000000)
                    {
                        Log.Debug(Category.MediaLoader, "'Erase Gap' or reserved marker 0x{0:x} at block {1}", recordType, block);
                        FillBlock((byte)(recordType & 0xfe), ref data);
                    }
                    else if (recordType == (uint)Marker.FileMark)
                    {
                        // Don't log EVERY BLOODY SECTOR when loading an unformatted tape... :-|
                        if (data[0] != 0x1c)
                            Log.Detail(Category.MediaLoader, "File mark at block {0}", block);

                        FillBlock(0x1c, ref data);
                    }
                    else if (recordType == (uint)Marker.Empty)
                    {
                        // Sigh.  We write Empty blocks as "private data" (class 1)
                        // but to save some space the blocks of nulls are left out
                        FillBlock(0, ref data);
                    }
                    else if ((recordType & 0xf0000000) == 0xe0000000)
                    {
                        Log.Debug(Category.MediaLoader, "Stray metadata marker {0:x} at block {1}, skipped", recordType, block);
                        continue;   // This'll just spew and/or crash, probably
                    }
                    else
                    {
                        // It's a data block!  Get the length (low 24 bits)
                        var blockLen = recordType & 0x00ffffff;

                        // Bad block flag is the msb
                        bad = (recordType & 0x80000000) != 0;

                        // If bit 31 is set then bits 30:24 must be zero, so if
                        // we get anything else die horribly?  Shouldn't happen...
                        if (bad && (recordType & 0x7f000000) != 0)
                        {
                            Log.Error(Category.MediaLoader, "TAP formatter: Bad error flag at block {0}: {1:x} is bogus!", block, recordType);
                            return false;
                        }

                        // Should never happen on files we write, but may on non-
                        // PERQmedia written archives... either we're out of sync
                        // or the file has different blocking; either way we bail
                        // (and probably just blow up)
                        if (blockLen != 512)
                        {
                            Log.Error(Category.MediaLoader, "TAP formatter: Block {0} is wrong size ({1}), skipping it", block, blockLen);
                            continue;
                        }

                        // Read the block from the stream
                        fs.Read(data, 0, data.Length);

                        Log.Detail(Category.MediaLoader, "Read data block {0}, 0/{1}/{2} ({3} bytes)", block, head, sector, data.Length);
                    }

                    //
                    // Special case:  If we've reached the last block of the file
                    // and haven't seen an EndOfMedia tag, it's probably a new
                    // blank tape that hasn't had an Erase or Write pass done on
                    // it yet -- so force the EOM marker and avoid unpleasantness
                    // with spurious warnings and the like (or special-casing the
                    // StorageDevice Format() method to deal with tapes differently.
                    //
                    // This ain't ideal, but I'll document it.  Run Stut (emulated
                    // or standalone) to properly format tapes or they'll be full
                    // sized when saved as .tap files!
                    //
                    if (block == dev.Geometry.TotalBlocks - 1 && recordType != (uint)Marker.EndOfMedia)
                    {
                        Log.Info(Category.MediaLoader, "Inserting EOM record for unformatted tape");

                        recordType = (uint)Marker.EndOfMedia;
                        FillBlock(0xff, ref data);

                        // Prevent overruns, too...
                        EOM = true;
                    }

                    // For all blocks the header IS the marker
                    dev.Sectors[0, head, sector].WriteHeaderByte(0, (byte)(recordType >> 24));
                    dev.Sectors[0, head, sector].WriteHeaderByte(1, (byte)(recordType >> 16));
                    dev.Sectors[0, head, sector].WriteHeaderByte(2, (byte)(recordType >> 8));
                    dev.Sectors[0, head, sector].WriteHeaderByte(3, (byte)recordType);
                    dev.Sectors[0, head, sector].IsBad = bad;

                    // Copy in the sector data
                    data.CopyTo(dev.Sectors[0, head, sector].Data, 0);

                    // Bump the block count and adjust our StorageDev's C/H/S
                    block++;
                    sector++;
                    if (sector == dev.Geometry.Sectors)
                    {
                        Log.Debug(Category.MediaLoader, "Track {0} full at block {1}, reversing tape direction!", head, block);

                        // Wrap around to next track
                        sector = 0;
                        head++;
                        if (head == dev.Geometry.Heads && !EOM)
                        {
                            // Oh crap, we've overrun!
                            Log.Warn(Category.MediaLoader, "Overrun at block {0} (pos={1}), tape is too large!", block, fs.Position);
                            return false;
                        }
                    }

                    // Finally: check the end marker and make sure it matches
                    // ... unless we stuck in an EOM, so quietly ignore that  doh
                    var checkType = fs.ReadUInt();

                    if (checkType != recordType && !EOM)
                    {
                        Log.Error(Category.MediaLoader, "Record mismatch at block {0} (pos={1}), sync lost", block, fs.Position);
                        return false;
                    }
                }

                // Done!
                Log.Info(Category.MediaLoader, "TAP formatter: Read {0} blocks, total size {1} bytes", block, fs.Position);
                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Unable to load tape image: {0}", e.Message);
                return false;
            }
        }

        /// <summary>
        /// Write a QIC tape image in "extended" TAP format.
        /// </summary>
        public bool Write(Stream fs, StorageDevice dev)
        {
            uint recordType = 0;
            int size = 0;
            bool pad = false;

            Log.Debug(Category.MediaLoader, "Writing custom header...");

            //
            // Custom PERQ "extended" .tap file includes all the PERQmedia goodies
            //

            // Cookie
            recordType = (uint)Marker.Cookie;

            fs.WriteUInt(recordType);
            fs.Write(_cookie, 0, _cookie.Length);
            fs.WriteByte(_fileVersion);
            fs.WriteByte((byte)dev.Info.Type);
            fs.WriteUInt(recordType);

            // Text label, if present
            size = dev.FileInfo.TextLabel != null ? dev.FileInfo.TextLabel.Length : 0;

            if (size > 0)
            {
                pad = (size & 1) != 0;
                recordType = (uint)Marker.TextLabel;
                recordType |= (uint)(size & 0x00ffffff);

                fs.WriteUInt(recordType);
                fs.Write(dev.FileInfo.TextLabel, 0, size);
                if (pad) fs.WriteByte(0);
                fs.WriteUInt(recordType);
            }

            // Image label, if present
            size = dev.FileInfo.ImageLabel != null ? dev.FileInfo.ImageLabel.Length : 0;

            if (size > 0)
            {
                pad = (size & 1) != 0;
                recordType = (uint)Marker.ImageLabel;
                recordType |= (uint)(size & 0x00ffffff);      // Silently clip to 16MB!

                fs.WriteUInt(recordType);
                fs.Write(dev.FileInfo.ImageLabel, 0, size);
                if (pad) fs.WriteByte(0);
                fs.WriteUInt(recordType);
            }

            // Info

            // Because this is variable length, compute the size to set the marker;
            // length of the strings (one null terminator each), plus fixed fields
            size = (dev.FileInfo.ArchivedBy.Length +
                    dev.Info.Name.Length +
                    dev.Info.Description.Length + 3 + 11);

            pad = (size & 1) != 0;
            recordType = (uint)Marker.DevInfo;
            recordType |= (uint)(size & 0x00ffffff);

            fs.WriteUInt(recordType);
            fs.WriteByte((byte)dev.FileInfo.FSType);                // 1 byte
            fs.WriteLong(dev.FileInfo.ArchiveDate.ToBinary());      // 8 bytes
            fs.WriteString(dev.FileInfo.ArchivedBy);                // n+1 bytes
            fs.WriteString(dev.Info.Name);                          // n+1 bytes
            fs.WriteString(dev.Info.Description);                   // n+1 bytes

            // Encode the individual flags
            var flags = (ushort)((dev.Info.IsWritable ? 0x1 : 0) |
                                 (dev.Info.IsBootable ? 0x2 : 0) |
                                 (dev.Info.IsRemovable ? 0x4 : 0));
            fs.WriteShort(flags);                                   // 2 bytes
            if (pad) fs.WriteByte(0);
            fs.WriteUInt(recordType);

            // Geometry
            recordType = (uint)Marker.DevGeometry;

            fs.WriteUInt(recordType);
            fs.WriteShort(dev.Geometry.Cylinders);
            fs.WriteByte(dev.Geometry.Heads);
            fs.WriteShort(dev.Geometry.Sectors);
            fs.WriteShort(dev.Geometry.SectorSize);
            fs.WriteByte(dev.Geometry.HeaderSize);
            fs.WriteUInt(recordType);

            // Performance specs
            recordType = (uint)Marker.DevSpecs;

            fs.WriteUInt(recordType);
            fs.WriteInt(dev.Specs.RPM);
            fs.WriteInt(dev.Specs.IndexPulse);
            fs.WriteInt(dev.Specs.StartupDelay);
            fs.WriteInt(dev.Specs.MinimumSeek);
            fs.WriteInt(dev.Specs.MaximumSeek);
            fs.WriteInt(dev.Specs.HeadSettling);
            fs.WriteInt(dev.Specs.TransferRate);
            fs.WriteUInt(recordType);

            Log.Debug(Category.MediaLoader, "Writing data...");

            //
            // Dump all the sector data using "standard" .tap markers.
            //
            size = 0;

            for (byte hd = 0; hd < dev.Geometry.Heads; hd++)
            {
                for (ushort sec = 0; sec < dev.Geometry.Sectors; sec++)
                {
                    var block = dev.Read(0, hd, sec);
                    var marker = GetMarker(block.Header);

                    // HA!  We just write the header, the data, then the header again! :-P
                    fs.Write(block.Header, 0, dev.Geometry.HeaderSize);

                    // Unless it's a block that doesn't require writing the data!  Double ha!
                    if (marker == Marker.Data || marker == Marker.BadData)
                        fs.Write(block.Data, 0, dev.Geometry.SectorSize);

                    // And a second copy, for reading backwards :-)
                    fs.Write(block.Header, 0, dev.Geometry.HeaderSize);

                    Log.Info(Category.MediaLoader, "Wrote block {0}, 0/{1}/{2} type {3}", size, hd, sec, marker);
                    recordType = (uint)marker;
                    size++;

                    // No point in writing out empty blocks beyond the End of Media
                    // marker; this is a perfectly acceptable use of a goto, says I
                    if (marker == Marker.EndOfMedia) goto EOM;
                }
            }

        EOM:
            // Make sure the last thing written was an EOM!
            if ((Marker)recordType != Marker.EndOfMedia)
            {
                Log.Warn(Category.MediaLoader, "Last sector written was 0x{0:x}; adding EOM", recordType);
                fs.WriteUInt((uint)Marker.EndOfMedia);
                fs.WriteUInt((uint)Marker.EndOfMedia);
            }

            Log.Info(Category.MediaLoader, "TAP formatter: Wrote {0} blocks, total size {1} bytes", size, fs.Position);

            return true;
        }

        Marker GetMarker(byte[] header)
        {
            var m = (uint)((header[0] << 24) | (header[1] << 16) | (header[2] << 8) | header[3]);

            return (Marker)m;
        }

        void FillBlock(byte val, ref byte[] buf)
        {
            for (var i = 0; i < buf.Length; i++) buf[i] = val;
        }

        // For checking our custom marker/header data
        byte[] _cookie = { (byte)'P', (byte)'R', (byte)'Q', (byte)'T' };
        byte _fileVersion = (byte)'0';
        byte _driveType;
    }

    /// <summary>
    /// Marker types used by .TAP, but with custom markers defined for extra
    /// metadata (all the same stuff PRQM uses!).
    /// </summary>
    enum Marker : uint
    {
        FileMark = 0x00000000,
        Data = 0x00000200,          // Good data (512 byte block)
        Empty = 0x10000200,         // Internal use; not written to tape
        BadData = 0x80000200,       // Bad flag (512 bytes present)
        Cookie = 0xe0000006,        // 'PRQT' cookie (version 0)
        TextLabel = 0xe1000000,     // Text label (optional)
        ImageLabel = 0xe2000000,    // Image label (optional)
        DevInfo = 0xe4000000,       // Info rec (var size, same as PRQM)
        DevGeometry = 0xe5000008,   // Geometry rec (same as PRQM)
        DevSpecs = 0xe600001c,      // Specs rec (same as PRQM)
        EraseGap = 0xfffffffe,      // Not used by PERQemu/PERQmedia?
        EndOfMedia = 0xffffffff     // Logical end of media
    }

}

/*
    Streamer notes:

    I have to make a number of assumptions until more real live tape images
    are produced and I've had some time to really dig into the guts of the
    Stut format and the PERQ version of 'tar'.  For now, set the FSHint to
    'Stut' for all tape images.  Maybe the ReadHeader method could search for
    some specific signatures in the first n blocks to detect Stut format more
    definitively?

    The original "standard" TAP format doesn't store _any_ metadata about the
    contents, but the "extended" version does.  When writing, the "class E" tag
    is used to define metadata in "Tape Description Data Records" as defined by
    the Jan, 2022 version of the spec.  Details in PRQM_Format.txt.

    Because the PERQmedia format was designed primarily around disks with fixed
    sector sizes and TAP allows for every block to specify its own size, this
    formatter cannot handle any arbitrary .tap file.  For PERQ use, QIC tapes
    with fixed 512-byte blocks are all we can recognize, currently.  (This block
    size is set by Archive's controller, if not by the QIC-02 standard itself.)

    When writing out a .tap image, we only need to include all the blocks from
    0..EOM, since we can fill in the rest on read by just pre-formatting the
    StorageDevice's underlying Sectors[].  For a newly created file that doesn't
    have an EOM marker we add one on write; for a tape that ends prematurely we
    try to synthesize one on read.  Obviously for purely PERQ-only use the PRQM
    format saves lots of space, but it IS nice for debugging to be able to see
    the uncompressed data...
    
    There's a version of 'tar' for the PERQ, but I haven't looked into it yet.
    It's not clear if that was written as an alternative to Stut for writing
    tar archives to the streamer for interchange with other systems.  Or was
    it written specifically to use 9-track reel-to-reel tapes with the MLO
    board and the Ciprico Tapemaster?  Could PNX access any of the streamer
    or Multibus hardware?  Lots of unanswered questions.
*/
