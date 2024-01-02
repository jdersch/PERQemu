//
//  RawFormatter.cs
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

using PERQemu;

namespace PERQmedia
{
    /// <summary>
    /// Reads and writes floppies in the "raw" format.  Also parses the
    /// "PFD" header if present.
    /// </summary>
    public class RawFormatter : IMediaFormatter
    {
        public string Name => "PERQemu 'raw' floppy format";

        public bool ReadHeader(Stream fs, StorageDevice dev)
        {
            try
            {
                byte b;

                // Check the cookie
                for (int i = 0; i < _cookie.Length; i++)
                {
                    b = (byte)fs.ReadByte();

                    if (b != _cookie[i])
                    {
                        Log.Debug(Category.MediaLoader, "Not a valid PFD image: Bad cookie (trying Raw)");
                        goto Rewind;  // Yep.  This is happening.
                    }
                }

                // Parse out the geometry hint
                b = (byte)fs.ReadByte();
                Log.Detail(Category.MediaLoader, "PFD geometry hint: {0:x2}", b);

                // There are only four valid codes... but some super early
                // ones might still exist with even older hints.  Sigh.
                switch (b)
                {
                    case 0x09:
                    case 0x41:
                        dev.Geometry = DeviceGeometry.SSSD;
                        break;

                    case 0x0a:
                    case 0x42:
                        dev.Geometry = DeviceGeometry.DSSD;
                        break;

                    case 0x11:
                    case 0x81:
                        dev.Geometry = DeviceGeometry.SSDD;
                        break;

                    case 0x12:
                    case 0x82:
                        dev.Geometry = DeviceGeometry.DSDD;
                        break;
                }

                // Make some sense of the filesystem hint byte
                b = (byte)fs.ReadByte();
                Log.Detail(Category.MediaLoader, "PFD filesystem hint: {0:x2}", b);

                // Only 5 values were defined...
                switch (b)
                {
                    case 0x01:
                        dev.FileInfo.FSType = FilesystemHint.RT11;
                        break;

                    case 0x02:
                        dev.FileInfo.FSType = FilesystemHint.POS;
                        break;

                    case 0x03:
                        dev.FileInfo.FSType = FilesystemHint.PNX;
                        break;

                    case 0x82:
                        dev.FileInfo.FSType = FilesystemHint.POS;
                        dev.Info.IsBootable = true;
                        break;

                    case 0x83:
                        dev.FileInfo.FSType = FilesystemHint.PNXBoot;
                        dev.Info.IsBootable = true;
                        break;

                    default:
                        dev.FileInfo.FSType = FilesystemHint.Unknown;
                        break;
                }

            Rewind:

                // If we couldn't figure out the geometry above, try guessing
                // based on the file size.  Better than nuthin'.
                if (dev.Geometry.Cylinders == 0)
                {
                    // Just bail if the size is ridiculous
                    if (fs.Length < 256256 || fs.Length > 1025024)
                    {
                        Log.Debug(Category.MediaLoader,
                                  "File too small or too big to be a valid raw/PFD floppy!");
                        return false;
                    }

                    if (fs.Length < 512512)
                    {
                        dev.Geometry = DeviceGeometry.SSSD;
                    }
                    else if (fs.Length == 512512)
                    {
                        // This could be DSSD or SSDD, but the latter is highly unlikely
                        dev.Geometry = DeviceGeometry.DSSD;
                    }
                    else if (fs.Length > 512512)
                    {
                        dev.Geometry = DeviceGeometry.DSDD;
                    }
                }

                // Assuming a floppy image of SOME kind, assign reasonable defaults
                dev.Info = DeviceInfo.SA851;
                dev.Specs = DevicePerformance.SA851;

                dev.FileInfo.Format = Formatters.RawFormat;

                // Always rewind whether header present or not
                fs.Seek(0, SeekOrigin.Begin);

                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Not a valid raw/PFD image: {0}", e.Message);
                return false;
            }
        }

        public bool ReadData(Stream fs, StorageDevice dev)
        {
            try
            {
#if DEBUG
                if (dev.Geometry.HeaderSize > 0)
                {
                    Log.Write(Severity.Heresy, Category.MediaLoader,
                              "Preposterous! Floppies don't have sector headers!  Ignoring.");
                    // Um, this is probably a fatal error but we'll try anyway?
                }
#endif
                // Allocate the space
                dev.CreateSectors();

                // Read in the PHD sectors in order
                for (ushort c = 0; c < dev.Geometry.Cylinders; c++)
                {
                    for (byte h = 0; h < dev.Geometry.Heads; h++)
                    {
                        for (ushort s = 0; s < dev.Geometry.Sectors; s++)
                        {
                            // Initialize the sector
                            dev.Sectors[c, h, s] = new Sector(c, h, s, dev.Geometry.SectorSize, 0);

                            // Just read the sector data
                            fs.Read(dev.Sectors[c, h, s].Data, 0, dev.Geometry.SectorSize);
                        }
                    }
                }
#if DEBUG
                if (fs.Position != fs.Length)
                {
                    Log.Debug(Category.MediaLoader,
                              "Data underrun? Read {0} bytes, file has {1} bytes",
                              fs.Position, fs.Length);
                }
#endif 
                // We made it!
                return true;
            }
            catch (EndOfStreamException e)
            {
                Log.Debug(Category.MediaLoader, "Unable to load floppy image: {0}", e.Message);
                return false;
            }
        }

        public bool Write(Stream fs, StorageDevice dev)
        {
            //
            // We're going to always add the little PFD cookie, since it's
            // innocuous and doesn't interfere with any of the formats.  We
            // just sneakily splice it in to the first data block:
            //
            for (int i = 0; i < _cookie.Length; i++)
            {
                dev.Sectors[0, 0, 0].WriteByte((uint)i, _cookie[i]);
            }

            dev.Sectors[0, 0, 0].WriteByte(7, (byte)((dev.Geometry.SectorSize >> 1) | dev.Geometry.Heads));
            dev.Sectors[0, 0, 0].WriteByte(8, FSHintToRaw(dev));

            // Now just copy the data all normal like
            for (ushort c = 0; c < dev.Geometry.Cylinders; c++)
            {
                for (byte h = 0; h < dev.Geometry.Heads; h++)
                {
                    for (ushort s = 0; s < dev.Geometry.Sectors; s++)
                    {
                        // Just write the sector data
                        fs.Write(dev.Sectors[c, h, s].Data, 0, dev.Geometry.SectorSize);
                    }
                }
            }

            return true;
        }

        /// <summary>
        /// Oops.  The new set of codes don't match the old PFD header codes!
        /// Map them back to the available ones so that older tools still read
        /// the values they expect.
        /// </summary>
        byte FSHintToRaw(StorageDevice dev)
        {
            switch (dev.FileInfo.FSType)
            {
                case FilesystemHint.RT11:
                case FilesystemHint.RT11Ext:
                    return 0x1;

                case FilesystemHint.POS:
                    return (byte)(dev.Info.IsBootable ? 0x82 : 0x02);

                case FilesystemHint.PNX:
                case FilesystemHint.PNXBoot:
                    return (byte)(dev.Info.IsBootable ? 0x83 : 0x03);
            }

            // Unknown
            return 0x0;
        }

        // The ill-conceived PFD cookie
        readonly static byte[] _cookie = { (byte)'P', (byte)'E', (byte)'R', (byte)'Q',
                                           (byte)'f', (byte)'l', (byte)'p' };
    }
}
