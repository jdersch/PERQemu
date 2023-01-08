//
//  IMDFormatter.cs
//
//  Author:  S. Boondoggle <skeezicsb@gmail.com>
//
//  Copyright (c) 2022-2023, Boondoggle Heavy Industries, Ltd.
//
//  This code is based on the IMD code from PERQemu by Josh Dersch.  It
//  adheres to the IMD format specification (public domain) published by
//  Dave Dunfield, but this C# implementation is original and does not
//  use his code.
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

using PERQemu;

namespace PERQmedia
{
    /// <summary>
    /// Reads and writes floppies in IMD format.
    /// </summary>
    public class IMDFormatter : IMediaFormatter
    {
        public IMDFormatter()
        {
            _helper = null;
        }

        public string Name => "IMD floppy archive";

        public bool ReadHeader(Stream fs, StorageDevice dev)
        {
            try
            {
                _helper = new IMDFormatHelper();

                if (!_helper.TryParseHeader(fs, dev))
                {
                    Log.Debug(Category.MediaLoader, "Not a valid IMD image: Malformed header line or missing cookie");
                    return false;
                }

                // Any characters up to a literal 0x1a (^Z) are the text label
                var label = fs.ReadString(0x1a);
                dev.FileInfo.EncodeTextLabel(label);

                // Fill in some gaps
                dev.FileInfo.Format = Formatters.IMDFormat;

                dev.Info = DeviceInfo.SA851;
                dev.Specs = DevicePerformance.SA851;

                // We'll set the Geometry after reading the data portion.
                // So, with that we're all good to continue...
                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Not a valid IMD image: {0}", e.Message);
                return false;
            }
        }

        public bool ReadData(Stream fs, StorageDevice dev)
        {
            try
            {
                // Start readin' tracks and takin' names
                while (fs.Position < fs.Length)
                {
                    _helper.ReadTrack(fs);
                }

                // Now figure out what we got
                if (_helper.DoubleSided)
                {
                    dev.Geometry = (_helper.DoubleDensity ? DeviceGeometry.DSDD : DeviceGeometry.DSSD);
                }
                else
                {
                    dev.Geometry = (_helper.DoubleDensity ? DeviceGeometry.SSDD : DeviceGeometry.SSSD);
                }

                // Now allocate sectors
                dev.CreateSectors();

                // And copy 'em in!
                _helper.MapTracksToDevice(dev);

                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Not a valid IMD image: {0}", e.Message);
                return false;
            }
        }

        public bool Write(Stream fs, StorageDevice dev)
        {
            _helper = new IMDFormatHelper();

            // Are we translating from a different format?
            // Update our fields appropriately.
            if (dev.FileInfo.Format != Formatters.IMDFormat)
            {
                dev.FileInfo.Format = Formatters.IMDFormat;
                dev.FileInfo.Version = _helper.IMDVersion;
            }

            var bytes = _helper.EncodeHeader(dev);

            // Write out the header section
            fs.Write(bytes, 0, bytes.Length);
            fs.WriteByte(0x1a);

            // And now the data...
            _helper.WriteTracks(fs, dev);

            return true;
        }


        IMDFormatHelper _helper;
    }
}
