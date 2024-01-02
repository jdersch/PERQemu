//
//  IMediaFormatter.cs
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

namespace PERQmedia
{
    public interface IMediaFormatter
    {
        /// <summary>
        /// A description of the file type this Formatter reads/writes.
        /// </summary>
        string Name { get; }

        /// <summary>
        /// Attempt to load the Header Sections of a media file using the given formatter.
        /// </summary>
        /// <returns><c>true</c> if the file is of the correct type, <c>false</c> otherwise.</returns>
        /// <param name="fs">A Stream opened for reading; on success, fs will be positioned
        /// at the first byte of the Data Section of the file; on failure, the file pointer will
        /// be rewound to the beginning of file</param>
        /// <param name="dev">A StorageDevice whose header fields will be filled in; on
        /// failure, dev will be unmodified</param>
        bool ReadHeader(Stream fs, StorageDevice dev);

        /// <summary>
        /// Attempt to load the Data Section of a media file using the given formatter.
        /// </summary>
        /// <returns><c>true</c> if the data was read succesfully, <c>false</c> otherwise.</returns>
        /// <param name="fs">A Stream pointing to the beginning of the Data Section; on
        /// success, <c>fs</c> will be at EOF, while on failure the position will be at the
        /// point where the error occurred</param>
        /// <param name="dev">A StorageDevice whose header fields have been initialized; on
        /// success, the sector data will be filled in and the file's CRC verified, while on
        /// failure the data will be incomplete or indeterminate</param>
        bool ReadData(Stream fs, StorageDevice dev);
        
        /// <summary>
        /// Write out a complete media file using the given formatter.
        /// </summary>
        /// <returns><c>true</c> if the file was written successfully, <c>false</c> otherwise.</returns>
        /// <param name="fs">A Stream opened for writing</param>
        /// <param name="dev">The StorageDevice object containing the in-memory image to be written</param>
        bool Write(Stream fs, StorageDevice dev);
    }
}
