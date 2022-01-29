//
//  PHDFormatter.cs
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

namespace PERQmedia
{
    /// <summary>
    /// Reads and writes the original PERQ Hard Drive (.phd) images.
    /// </summary>
    public class PHDFormatter : IMediaFormatter
    {
        public string Name => "PERQemu Hard Disk format";

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
                        Console.WriteLine("Bad cookie -- not a valid PHD image");
                        fs.Seek(0, SeekOrigin.Begin);
                        return false;
                    }
                }

                // There's no file version byte
                // There's no media info (labels, hints, etc)

                // Ensure sector header data is present
                if (fs.ReadByte() != 1)
                {
                    Console.WriteLine("Sector header data must be present -- not a valid PHD image");
                    fs.Seek(0, SeekOrigin.Begin);
                    return false;
                }

                // Fill in what we can
                dev.FileInfo.Format = Formatters.PHDFormat;

                // Read cyl/sec/tracks and set Geometry
                ushort cyls = fs.ReadShort();
                ushort secs = (byte)fs.ReadByte();
                byte heads = (byte)fs.ReadByte();
                ushort secSize = 512;
                byte headSize = 16;
                dev.Geometry = new DeviceGeometry(cyls, heads, secs, secSize, headSize);

                // Fill in device specs (assuming it's a Shugart)
                if ((cyls == DeviceGeometry.Shugart12.Cylinders) &&
                    (secs == DeviceGeometry.Shugart12.Sectors))
                {
                    dev.Specs = DevicePerformance.SA4000;

                    switch (heads)
                    {
                        case 4:
                            dev.Info = new DeviceInfo(DeviceType.Disk14Inch, "Shugart12", "Shugart SA4004 (12MB)");
                            break;

                        case 8:
                            dev.Info = new DeviceInfo(DeviceType.Disk14Inch, "Shugart24", "Shugart SA4008 (24MB)");
                            break;

                        case 16:
                            dev.Info = new DeviceInfo(DeviceType.Disk14Inch, "Shugart48", "Shugart SA4104 (48MB)");
                            break;

                        default:
                            // Whoops.  Should we just continue?
                            Console.WriteLine("Bad Shugart disk geometry!");
                            break;
                    }
                }

                // Good header!
                return true;
            }
            catch (EndOfStreamException e)
            {
                Console.WriteLine(e.Message + " -- not a valid PHD image");
                return false;
            }
        }

        public bool ReadData(Stream fs, StorageDevice dev)
        {
            try
            {
                // Convenience
                var secSize = dev.Geometry.SectorSize;
                var headSize = dev.Geometry.HeaderSize;

                // Allocate the space
                dev.CreateSectors();

                // Read in the PHD sectors in order
                for (ushort c = 0; c < dev.Geometry.Cylinders; c++)
                {
                    for (byte h = 0; h < dev.Geometry.Heads; h++)
                    {
                        for (ushort s = 0; s < dev.Geometry.Sectors; s++)
                        {
                            // Bad sector flag
                            bool bad = fs.ReadByte() != 0;

                            // Initialize the sector
                            dev.Sectors[c, h, s] = new Sector(c, h, s, secSize, headSize, bad);

                            // Read the header data
                            fs.Read(dev.Sectors[c, h, s].Header, 0, headSize);

                            // And the sector data
                            fs.Read(dev.Sectors[c, h, s].Data, 0, secSize);
                        }
                    }
                }
#if DEBUG
                if (fs.Position != fs.Length)
                {
                    Console.WriteLine("Data underrun? Read {0} bytes, file has {1} bytes", fs.Position, fs.Length);
                }
#endif 
                // We made it!
                dev.IsLoaded = true;
            }
            catch (EndOfStreamException e)
            {
                Console.WriteLine("Read failed: " + e.Message);
                dev.IsLoaded = false;
            }

            return dev.IsLoaded;
        }


        public bool Write(Stream fs, StorageDevice dev)
        {
            // Write our PHD header
            fs.Write(_cookie, 0, _cookie.Length);
            fs.WriteByte(1);
            fs.WriteShort(dev.Geometry.Cylinders);
            fs.WriteByte((byte)dev.Geometry.Sectors);
            fs.WriteByte(dev.Geometry.Heads);

            // Write the data
            for (ushort c = 0; c < dev.Geometry.Cylinders; c++)
            {
                for (byte h = 0; h < dev.Geometry.Heads; h++)
                {
                    for (ushort s = 0; s < dev.Geometry.Sectors; s++)
                    {
                        fs.WriteByte(dev.Sectors[c, h, s].IsBad ? (byte)1 : (byte)0);
                        fs.Write(dev.Sectors[c, h, s].Header, 0, dev.Geometry.HeaderSize);
                        fs.Write(dev.Sectors[c, h, s].Data, 0, dev.Geometry.SectorSize);
                    }
                }
            }

            return true;
        }

        // The PHD cookie
        private static byte[] _cookie = { (byte)'P', (byte)'E', (byte)'R', (byte)'Q' };
    }
}
