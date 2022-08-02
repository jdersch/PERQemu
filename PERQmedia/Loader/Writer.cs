//
//  Writer.cs
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
    /// Extension methods for saving a StorageDevice to disk.
    /// </summary>
    public static class Writer
    {
        public static bool CanSaveWithFormat(this StorageDevice dev, Formatters fmt)
        {
            // Catch the obvious ones:
            if (fmt == Formatters.Unknown) return false;
            if (fmt == Formatters.PRQFormat) return true;

            // Now the specifics:
            switch (dev.Info.Type)
            {
                case DeviceType.DCIOShugart:
                case DeviceType.Disk14Inch:
                    return (fmt == Formatters.PHDFormat);

                case DeviceType.Floppy:
                    return (fmt == Formatters.IMDFormat || fmt == Formatters.RawFormat);

                /*
                    Handled above:
                    case DeviceType.DCIOMicrop:
                    case DeviceType.Disk8Inch:
                    case DeviceType.Disk5Inch:
                    case DeviceType.DiskSMD:
                    return (fmt == Formatters.PRQFormat);
                */

                case DeviceType.TapeQIC:
                    return (fmt == Formatters.TAPFormat);
            }

            return false;
        }

        public static void Save(this StorageDevice dev)
        {
            SaveAsWithFormat(dev, dev.Filename, dev.FileInfo.Format);
        }

        public static void SaveAs(this StorageDevice dev, string pathname)
        {
            SaveAsWithFormat(dev, pathname, dev.FileInfo.Format);
        }

        public static void SaveWithFormat(this StorageDevice dev, Formatters fmt)
        {
            SaveAsWithFormat(dev, dev.Filename, fmt);
        }

        public static void SaveAsWithFormat(this StorageDevice dev, string pathname, Formatters fmt)
        {
            // Default to using the new format if none given
            if (fmt == Formatters.Unknown)
            {
                fmt = Formatters.PRQFormat;
            }

            // TODO: return a bool for success/fail or throw?
            //       errors: bad pathnames, readonly, incompatible format?

            // Start by creating the file; if that fails, we die
            using (var fs = new FileStream(pathname, FileMode.Create, FileAccess.Write))
            {
                IMediaFormatter formatter;

                switch (fmt)
                {
                    case Formatters.PRQFormat:
                        formatter = new PRQFormatter();
                        break;

                    case Formatters.PHDFormat:
                        formatter = new PHDFormatter();
                        break;

                    case Formatters.RawFormat:
                        formatter = new RawFormatter();
                        break;

                    case Formatters.IMDFormat:
                        formatter = new IMDFormatter();
                        break;

                    default:
                        Log.Info(Category.MediaLoader, "Unknown or unimplemented file formatter! Using default PERQmedia format.");
                        formatter = new PRQFormatter();
                        break;
                }

                // Invoke the formatter to write out the image
                formatter.Write(fs, dev);

                // Save the (possibly new) format type and indicate success
                dev.FileInfo.Format = fmt;
                dev.IsModified = false;

                Log.Write("Saved {0}.", pathname);
            }
        }
    }
}
