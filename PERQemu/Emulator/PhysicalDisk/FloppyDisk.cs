// physicaldisk.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

namespace PERQemu.PhysicalDisk
{
    /// <summary>
    /// Presents data for a floppy disk, organized by cylinder, head, and sector,
    /// and provides constructors for loading from IMD file.
    /// </summary>
    public class FloppyDisk
    {
        public FloppyDisk(string imagePath)
        {
            _imagePath = imagePath;
            _tracks = new Track[2, 77];
            _cylinderCount = 77;
            _isSingleSided = true;
            _isWriteProtected = false;
            _isModified = false;

            using (FileStream fs = new FileStream(imagePath, FileMode.Open, FileAccess.Read))
            {
                LoadIMD(fs);
            }
        }

        public FloppyDisk()
        {
            _imagePath = string.Empty;
            _tracks = new Track[2, 77];
            _cylinderCount = 77;
            _isSingleSided = false;
            _isWriteProtected = false;
            _isModified = false;
        }

        public string Description
        {
            get { return _imdHeader; }
        }

        public bool IsSingleSided
        {
            get { return _isSingleSided; }
        }

        public bool IsWriteProtected
        {
            get { return _isWriteProtected; }
            set { _isWriteProtected = value; }
        }

        public string ImagePath
        {
            get { return _imagePath; }
        }

        public bool IsModified
        {
            get { return _isModified; }
        }

        public int CylinderCount
        {
            get { return _cylinderCount; }
        }

        /// <summary>
        /// Commits in-memory data back to disk.
        /// </summary>
        public void Save()
        {
            Save(_imagePath);
        }

        public void Save(string path)
        {
            using (FileStream fs = new FileStream(path, FileMode.Create, FileAccess.Write))
            {
                SaveIMD(fs);
            }
        }

        /// <summary>
        /// Returns sector data for the given address.
        /// </summary>
        public Sector GetSector(int cylinder, int head, int sector)
        {
            return _tracks[head, cylinder].ReadSector(sector);
        }

        public Track GetTrack(int cylinder, int head)
        {
            return _tracks[head, cylinder];
        }

        public void SetTrack(int cylinder, int head, Track track)
        {
            _tracks[head, cylinder] = track;
        }

        /// <summary>
        /// To be invoked when a modification is made to sector contents.
        /// TODO: Do this in a less clumsy way.
        /// </summary>
        public void SetModified()
        {
            _isModified = true;
        }

        /// <summary>
        /// Formats the given track with the specified sector count, size, and format.
        /// </summary>
        public void FormatTrack(Format format, int cylinder, int head, int sectorCount, int sectorSize)
        {
            _tracks[head, cylinder] = new Track(format, cylinder, head, sectorCount, sectorSize);
        }

        private void LoadIMD(Stream s)
        {
            _imdHeader = ReadIMDHeader(s);

            //
            // Read each track in and place it in memory.
            // We assume that there will be no more than 77 cylinders
            // and no more than 2 tracks.  We also do a basic sanity
            // check that no track appears more than once.
            //
            while (true)
            {
                Track t = new Track(s);

                if (t.Cylinder < 0 || t.Cylinder > 76)
                {
                    throw new InvalidOperationException(String.Format("Invalid cylinder value {0}", t.Cylinder));
                }

                if (t.Head < 0 || t.Head > 1)
                {
                    throw new InvalidOperationException(String.Format("Invalid head value {0}", t.Head));
                }

                if (_tracks[t.Head, t.Cylinder] != null)
                {
                    throw new InvalidOperationException(String.Format("Duplicate head/track", t.Head, t.Cylinder));
                }

                if (t.Head != 0)
                {
                    // Got a track on side 1, this must be a double-sided disk.
                    _isSingleSided = false;
                }

                _tracks[t.Head, t.Cylinder] = t;

                if (s.Position == s.Length)
                {
                    // End of file.
                    break;
                }
            }
        }

        private void SaveIMD(Stream s)
        {
            WriteIMDHeader(s, _imdHeader);

            for (int cylinder = 0; cylinder < 77; cylinder++)
            {
                for (int head = 0; head < 2; head++)
                {
                    Track t = _tracks[head, cylinder];

                    if (t != null)
                    {
                        t.Save(s);
                    }
                }
            }
        }

        private string ReadIMDHeader(Stream s)
        {
            StringBuilder sb = new StringBuilder();
            while (true && s.Position < s.Length)
            {
                byte b = (byte)s.ReadByte();

                if (b == 0x1a)
                {
                    break;
                }
                else
                {
                    sb.Append((char)b);
                }
            }

            return sb.ToString();
        }

        private void WriteIMDHeader(Stream s, string header)
        {
            byte[] asciiHeader = Encoding.ASCII.GetBytes(header);
            s.Write(asciiHeader, 0, asciiHeader.Length);
            s.WriteByte(0x1a);
        }

        private string _imdHeader;
        private bool _isSingleSided;
        private bool _isWriteProtected;
        private string _imagePath;
        private bool _isModified;
        private int _cylinderCount;

        private Track[,] _tracks;
    }

    /// <summary>
    /// Represents a single track's worth of sectors
    /// </summary>
    public class Track
    {
        /// <summary>
        /// Create a new, unformatted track with the specified format, sector size and sector count.
        /// Used when formatting a track.
        /// </summary>
        public Track(Format format, int cylinder, int head, int sectorCount, int sectorSize)
        {
            _format = format;
            _cylinder = cylinder;
            _head = head;
            _sectorCount = sectorCount;
            _sectorSize = sectorSize;

            // Create new sector dictionary and sector ordering map, but do not populate; this will be done
            // as sectors are added via AddSector.
            _sectors = new Dictionary<int, Sector>();
            _sectorOrdering = new List<int>(_sectorCount);
        }

        /// <summary>
        /// Create a new track loaded from the given stream.  The stream is expected to be positioned
        /// at the beginning of an IMD sector definition.
        /// </summary>
        public Track(Stream s)
        {
            bool bCylMap = false;
            bool bHeadMap = false;

            _format = (Format)s.ReadByte();
            _cylinder = s.ReadByte();
            _head = s.ReadByte();
            _sectorCount = s.ReadByte();
            int sectorSizeIndex = s.ReadByte();

            // Basic sanity check of values
            if (_format > Format.MFM250 ||
                _cylinder > 77 ||
                (_head & 0x3f) > 1 ||
                sectorSizeIndex > _sectorSizes.Length - 1)
            {
                throw new InvalidOperationException("Invalid header data for track.");
            }

            // Sector count should be 26 for normal PERQ floppies; post a warning if not.
            // This can happen on tracks with missing sector data.
            if (_sectorCount != 26)
            {
                Trace.Log(LogType.FloppyDisk, "Cylinder {0} head {1} has only {2} sectors.", 
                    _cylinder, _head, _sectorCount);
            }

            _sectorSize = _sectorSizes[sectorSizeIndex];

            bCylMap = (_head & 0x80) != 0;
            bHeadMap = (_head & 0x40) != 0;

            // Head is just the first bit.
            _head = (byte)(_head & 0x1);

            //
            // Read sector numbering
            //
            _sectorOrdering = new List<int>(_sectorCount);

            for (int i = 0; i < _sectorCount; i++)
            {
                _sectorOrdering.Add(s.ReadByte());
            }

            //
            // At this time, cyl and head maps are not supported.
            // It's not expected any PERQ disk would use such a format.
            //
            if (bCylMap | bHeadMap)
            {
                throw new NotImplementedException("IMD Cylinder and Head maps not supported.");
            }

            //
            // Read the sector data in.
            //
            _sectors = new Dictionary<int, Sector>();
            for (int i = 0; i < _sectorCount; i++)
            {
                SectorRecordType type = (SectorRecordType)s.ReadByte();
                byte compressedData;

                switch (type)
                {
                    case SectorRecordType.Unavailable:
                        // Nothing, sectors left null.
                        break;

                    case SectorRecordType.Normal:
                    case SectorRecordType.NormalDeleted:
                    case SectorRecordType.NormalError:
                    case SectorRecordType.DeletedError:
                        _sectors[_sectorOrdering[i]] = new Sector(_sectorSize, _format, s);
                        break;

                    case SectorRecordType.Compressed:
                    case SectorRecordType.CompressedDeleted:
                    case SectorRecordType.CompressedError:
                    case SectorRecordType.CompressedDeletedError:
                        compressedData = (byte)s.ReadByte();

                        // Fill sector with compressed data
                        _sectors[_sectorOrdering[i]] = new Sector(_sectorSize, _format, compressedData);
                        break;

                    default:
                        throw new InvalidOperationException(string.Format("Unexpected IMD sector data type {0}", type));
                }
            }
        }

        public void Save(Stream s)
        {
            s.WriteByte((byte)_format);
            s.WriteByte((byte)_cylinder);
            s.WriteByte((byte)_head);
            s.WriteByte((byte)_sectorCount);
            s.WriteByte(GetIMDSectorSize());

            //
            // Write sector numbering
            //
            for (int i = 0; i < _sectorCount; i++)
            {
                s.WriteByte((byte)_sectorOrdering[i]);
            }

            //
            // Write the sector data out in track order
            //
            for (int i = 0; i < _sectorCount; i++)
            {
                _sectors.TryGetValue(_sectorOrdering[i], out Sector sector);
                if (sector == null)
                {
                    // Mark this as unavailable.
                    s.WriteByte((byte)SectorRecordType.Unavailable);
                }
                else
                {
                    //
                    // Write out as "Normal".
                    // TODO: might be worthwhile to write out Compressed sectors at some point.
                    //
                    s.WriteByte((byte)SectorRecordType.Normal);
                    sector.Save(s);
                }
            }
        }

        public int Cylinder
        {
            get { return _cylinder; }
        }

        public int Head
        {
            get { return _head; }
        }

        public int SectorCount
        {
            get { return _sectorCount; }
        }

        public int SectorSize
        {
            get { return _sectorSize; }
        }

        public Format Format
        {
            get { return _format; }
        }

        public Sector ReadSector(int sector)
        {
            _sectors.TryGetValue(sector, out Sector s);
            return s;
        }

        public void AddSector(int sector, Sector newSector)
        {
            if (_sectorOrdering.Count == _sectorCount)
            {
                throw new InvalidOperationException("Track full.");
            }

            _sectorOrdering.Add(sector);
            _sectors[sector] = newSector;
        }

        private byte GetIMDSectorSize()
        {
            for (int i = 0; i < _sectorSizes.Length; i++)
            {
                if (_sectorSize == _sectorSizes[i])
                {
                    return (byte)i;
                }
            }

            // Should not happen.
            throw new InvalidOperationException(
                string.Format("No IMD sector size for {0}", _sectorSize));
        }

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

        private Format _format;
        private int _cylinder;
        private int _head;
        private int _sectorCount;
        private int _sectorSize;

        private List<int> _sectorOrdering;

        private Dictionary<int, Sector> _sectors;

        private static int[] _sectorSizes = { 128, 256, 512, 1024, 2048, 4096, 8192 };
    }

    public class Sector
    {
        public Sector(int sectorSize, Format format)
        {
            _data = new byte[sectorSize];
            _format = format;
        }

        public Sector(int sectorSize, Format format, byte compressedValue)
            : this(sectorSize, format)
        {
            for (int i = 0; i < _data.Length; i++)
            {
                _data[i] = compressedValue;
            }
        }

        public Sector(int sectorSize, Format format, Stream s)
            : this(sectorSize, format)
        {
            int read = s.Read(_data, 0, sectorSize);

            if (read != sectorSize)
            {
                throw new InvalidOperationException("Short read in sector data.");
            }
        }

        public void Save(Stream s)
        {
            s.Write(_data, 0, _data.Length);
        }

        public Format Format
        {
            get { return _format; }
        }

        public byte[] Data
        {
            get { return _data; }
        }

        private Format _format;
        private byte[] _data;
    }

    // 00 = 500 kbps FM   \   Note:   kbps indicates transfer rate,
    // 01 = 300 kbps FM    >          not the data rate, which is
    // 02 = 250 kbps FM   /           1/2 for FM encoding.
    // 03 = 500 kbps MFM
    // 04 = 300 kbps MFM
    // 05 = 250 kbps MFM
    public enum Format
    {
        Invalid = -1,
        FM500 = 0,
        FM300 = 1,
        FM250 = 2,
        MFM500 = 3,
        MFM300 = 4,
        MFM250 = 5,
    }
}
