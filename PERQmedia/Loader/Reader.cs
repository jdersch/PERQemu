//
//  Reader.cs
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

using System.IO;

using PERQemu;

namespace PERQmedia
{
    /// <summary>
    /// Extension methods for loading a StorageDevice from disk.
    /// </summary>
    public static class Reader
    {

        public static bool CanLoad(this StorageDevice dev)
        {
        	return CanLoad(dev, dev.Filename);
        }

        public static bool CanLoad(this StorageDevice dev, string pathname)
        {
            if (!string.IsNullOrEmpty(pathname) && File.Exists(pathname))
            {
                var formatters = FileUtilities.GetFormattersForFile(pathname);

                using (var fs = new FileStream(pathname, FileMode.Open, FileAccess.Read))
                {
                    foreach (var formatter in formatters)
                    {
                        if (formatter.ReadHeader(fs, dev))
                        {
                            // dev.Info.Type will be set
                            return true;
                        }
                    }
                }
            }

            return false;
        }

        public static void Load(this StorageDevice dev)
        {
            LoadFrom(dev, dev.Filename);
        }

        public static void LoadFrom(this StorageDevice dev, string pathname)
        {
            dev.IsLoaded = false;

            // No file? Quietly ignore
            if (string.IsNullOrEmpty(pathname))
            {
                return;
            }
                
            var formatters = FileUtilities.GetFormattersForFile(pathname);

            using (var fs = new FileStream(pathname, FileMode.Open, FileAccess.Read))
            {
                foreach (var formatter in formatters)
                {
                    if (formatter.ReadHeader(fs, dev))
                    {
                        // Read the data
                        dev.IsLoaded = formatter.ReadData(fs, dev);

                        // Either way, no going back...
                        break;
                    }

                    // This Formatter couldn't make sense of it; file is
                    // rewound to try the next one in the list
                }
            }

            if (dev.IsLoaded)
            {
                Log.Write("Loaded {0}.", pathname);

                // Save it
                dev.Filename = pathname;
                dev.IsModified = false;

                // Check for writability and set flag
                dev.Info.IsWritable = !(new FileInfo(pathname).IsReadOnly);

                // For formats that don't store an archive date, use the file's
                // creation date rather than "now"...
                if (dev.FileInfo.Format != Formatters.PRQFormat &&
                    dev.FileInfo.Format != Formatters.IMDFormat)
                {
                    dev.FileInfo.ArchiveDate = File.GetCreationTimeUtc(pathname);
                }

                // For grins, check the POS boot signature and set flag
                if (dev.Info.Type == DeviceType.Floppy)
                {
                    if (dev.Sectors[1, 0, 0].ReadByte(0) == 0x55 &&
                        dev.Sectors[1, 0, 0].ReadByte(1) == 0xaa)
                    {
                        Log.Info(Category.MediaLoader, "Boot floppy signature detected.");
                        dev.Info.IsBootable = true;
                    }

                    // If bootable and no hint is set, assume it's a POS boot
                    // floppy -- not sure how to detect a PNX hybrid floppy (yet)
                    if (dev.Info.IsBootable && dev.FileInfo.FSType == FilesystemHint.Unknown)
                    {
                        dev.FileInfo.FSType = FilesystemHint.POS;
                    }
                }

                // Signal our success
                dev.OnLoad();
            }
            else
            {
                // No joy.  Fell off the end, so all we can do is bail
                Log.Write("File {0} could not be read (unknown/bad format).", pathname);
            }
        }
    }
}
