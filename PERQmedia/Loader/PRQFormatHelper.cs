//
//  PRQFormatHelper.cs
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
using System.Diagnostics;
using System.IO;
using System.IO.Compression;

using PERQemu;

namespace PERQmedia
{
    /// <summary>
    /// The all-singing, all-dancing PERQmedia on-disk file format!
    /// 
    /// Provides a wrapper around the StorageDevice to assist the PRQFormatter
    /// in loading and saving all of the metadata available when the PERQmedia
    /// file format is used.  See PRQM_Format.txt for more details.
    /// </summary>
    public class PRQFormatHelper
    {
        public PRQFormatHelper()
        {
            // Fixed portions of the file header
            _driveType = (byte)DeviceType.Unused;
            _directory = new int[PM_DIR_SIZE];

            // Four variable-length data sections
            _textLabelSize = 0;
            _imageLabelSize = 0;
            _infoSize = 0;
            _data = null;

            UpdateDirectory();
        }

        public byte Version => _fileVersion;
        public byte DriveType => _driveType;

        public int TextLabelStart => _directory[0];
        public int TextLabelSize => _directory[1];

        public int ImageLabelStart => _directory[2];
        public int ImageLabelSize => _directory[3];

        public int InfoStart => _directory[4];
        public int InfoSize => _directory[5];

        public int DataStart => _directory[6];
        public int DataSize => _directory[7];

        public byte[] Data
        {
            get { return _data; }
            set { _data = value; }
        }

        /// <summary>
        /// Checks the cookie and version byte to verify that we're reading
        /// from a PRQM-format file.
        /// </summary>
        public bool CheckHeader(Stream fs)
        {
            byte b;

            // Give me cookies!
            for (int i = 0; i < _cookie.Length; i++)
            {
                b = (byte)fs.ReadByte();

                if (b != _cookie[i])
                {
                    Log.Debug(Category.MediaLoader, "Not a valid PRQM image: Bad cookie");
                    return false;
                }
            }

            // Check the file version
            if ((b = (byte)fs.ReadByte()) != _fileVersion)
            {
                Log.Debug(Category.MediaLoader, "Not a valid PRQM image: Version byte ({0}) doesn't match", b);
                return false;
            }

            return true;
        }

        /// <summary>
        /// Write the PRQM header to an output stream.
        /// </summary>
        public void WriteHeader(Stream fs)
        {
            fs.Write(_cookie, 0, _cookie.Length);
            fs.WriteByte(_fileVersion);
            fs.WriteByte(_driveType);
        }

        /// <summary>
        /// Set up for writing a new image from the given StorageDevice.
        /// Calculates the (dynamic) sizes of the Label and Info sections and
        /// copies the drive type for writing the header.
        /// </summary>
        public void InitFromDev(StorageDevice dev)
        {
            _driveType = (byte)dev.Info.Type;

            _textLabelSize = (dev.FileInfo.TextLabel != null) ? dev.FileInfo.TextLabel.Length : 0;
            _imageLabelSize = (dev.FileInfo.ImageLabel != null) ? dev.FileInfo.ImageLabel.Length : 0;

            _infoSize = dev.FileInfo.ArchivedBy.Length +
                        dev.Info.Name.Length +
                        dev.Info.Description.Length;

            _dataSize = dev.Geometry.TotalBytes;

            UpdateDirectory();
        }

        /// <summary>
        /// Read the directory from a file stream.
        /// </summary>
        public void ReadDirectory(Stream fs)
        {
            for (int i = 0; i < _directory.Length; i++)
            {
                _directory[i] = fs.ReadInt();
            }
        }

        /// <summary>
        /// Write the directory to a file stream.
        /// </summary>
        public void WriteDirectory(Stream fs)
        {
            for (var i = 0; i < _directory.Length; i++)
            {
                fs.WriteInt(_directory[i]);
            }
        }

        /// <summary>
        /// Recomputes the start and offset for each directory entry.  If the
        /// data has not been filled in, use the computed size based on device
        /// geometry, otherwise use the actual length of the byte array.  (The
        /// two values will differ based on whether the data is compressed or
        /// not.  The directory (and CRC!) should reflect how many bytes are
        /// actually written to the file, not the uncompressed size!
        /// </summary>
        /// <remarks>
        /// The directory contains the absolute byte offset (from the beginning
        /// of the file) and size in bytes of each variable-length section of
        /// the image archive.  This could be... less hardcodey.
        /// </remarks>
        public void UpdateDirectory()
        {
            // Text label
            _directory[0] = _cookie.Length + (_directory.Length * 4) + _fixedHeaderBytes;
            _directory[1] = _textLabelSize;

            // Image label
            _directory[2] = _directory[0] + _directory[1];
            _directory[3] = _imageLabelSize;

            // Info section
            _directory[4] = _directory[2] + _directory[3];
            _directory[5] = _infoSize + _fixedInfoBytes;

            // Data section
            _directory[6] = _directory[4] + _directory[5];
            _directory[7] = (_data == null) ? _dataSize : _data.Length;
        }

        [Conditional("DEBUG")]
        public void PrintDirectory()
        {
            Log.Detail(Category.MediaLoader, "Section\tStart\tSize");
            for (int i = 0; i < PM_DIR_ENTRIES; i++)
            {
                Log.Detail(Category.MediaLoader, "  {0}\t{1}\t{2}", i, _directory[i * 2], _directory[i * 2 + 1]);
            }
        }

        /// <summary>
        /// Compress the sector data from StorageDevice dev into _data[].
        /// </summary>
        public void Compress(StorageDevice dev)
        {
            using (var memoryStream = new MemoryStream())
            {
                using (var deflateStream = new DeflateStream(memoryStream, CompressionMode.Compress, true))
                {
                    using (var streamWriter = new BufferedStream(deflateStream))
                    {
                        foreach (Sector s in dev.Sectors)
                        {
                            streamWriter.WriteShort(s.CylinderID);
                            streamWriter.WriteByte(s.HeadID);
                            streamWriter.WriteShort(s.SectorID);
                            streamWriter.WriteByte(s.IsBad ? (byte)1 : (byte)0);

                            streamWriter.Write(s.Header, 0, s.Header.Length);
                            streamWriter.Write(s.Data, 0, s.Data.Length);
                        }
                    }
                }

                _data = memoryStream.ToArray();
            }

            UpdateDirectory();
        }

        /// <summary>
        /// Decompress the data from the bytes already loaded into _data[].
        /// </summary>
        public void Decompress()
        {
            // Stand back, I don't know how big this thing gets!
            byte[] tmp;

            using (var zippedMemoryStream = new MemoryStream(_data))
            {
                using (var unZippedMemoryStream = new MemoryStream())
                {
                    using (var deflateStream = new DeflateStream(zippedMemoryStream, CompressionMode.Decompress))
                    {
                        using (var unZippedBufferedStream = new BufferedStream(deflateStream))
                        {
                            unZippedBufferedStream.CopyTo(unZippedMemoryStream);
                        }

                        tmp = unZippedMemoryStream.ToArray();
                    }
                }
            }

            // Replace the original with the reinflated data
            _data = tmp;
        }

        // Header/ID section
        private byte[] _cookie = { (byte)'P', (byte)'R', (byte)'Q', (byte)'M' };
        private byte _fileVersion = (byte)'0';
        private byte _driveType;

        // Directory (n * 4 bytes)
        private int[] _directory;

        // Version + type bytes
        private readonly int _fixedHeaderBytes = 2;

        // Computed (no data copied)
        private int _textLabelSize;
        private int _imageLabelSize;
        private int _infoSize;
        private int _dataSize;

        // MediaInfo (n + 10 bytes)
        // DeviceInfo (n + 2 bytes)
        // DeviceFlags (2 bytes)
        // DeviceGeometry (8 bytes)
        // Characteristics (28 bytes)
        private readonly int _fixedInfoBytes = 50;

        // Temporary storage for compressing the Data section
        private byte[] _data;

        // Format Version 0 directory constants
        public static readonly int PM_DIR_ENTRIES = 4;
        public static readonly int PM_DIR_SIZE = PM_DIR_ENTRIES * 2;
    }
}
