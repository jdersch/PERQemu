//
// StorageCommands.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
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

using PERQmedia;
using PERQemu.Config;

namespace PERQemu.UI
{
    /// <summary>
    /// Commands for configuring storage devices (floppy, tape and hard drives),
    /// including top-level shortcuts for loading and unloading media files.
    /// </summary>
    public class StorageCommands
    {
        // top-level shortcuts (for compat w/previous perqemu cli):
        //      load floppy <file>          -- only one drive, assumes unit 0
        //      load harddisk <file>        -- assumes unit 1
        //      load harddisk <file> <n>    -- specifies unit in two-drive systems
        //                                  -- OR the mlo's smd slots if/when implemented
        //      load tape <file>            -- assume qic, only one drive
        //      load tape <file> <n>        -- if/when we have other drive types
        //      unload floppy               -- assumes 0
        //      unload harddisk             -- assumes 1
        //      unload harddisk <n>
        //      unload tape                 -- assume qic, only one drive
        //

        //      storage define              -- create geometries, specs, devices
        //      storage create <type> <file>  -- creates and formats a blank file on disk
        //
        //      storage load <file>         -- assigns it to unit 1
        //

        [Command("storage", "Enter the storage configuration subsystem")]
        public void SetStoragePrefix()
        {
            PERQemu.CLI.SetPrefix("storage");
        }

        [Command("storage done", "Exit storage configuration")]
        public void StorageDone()
        {
            PERQemu.CLI.ResetPrefix();
        }

        [Command("storage commands", "Show storage commands")]
        public void ShowStorageCommands()
        {
            PERQemu.CLI.ShowCommands("storage");
        }

        [Command("storage assign", "Attach a media file to a storage unit")]
        public void LoadMedia(byte unit, string filename)
        {
            Console.WriteLine("Test: load unit={0}, file={1}", unit, filename);
        }

        // todo: move the interface to the floppy/hard disk loading and saving
        // to PERQsystem?
        [Command("storage create", "Creates a new floppy, disk or tape image")]
        private void CreateMedia([KeywordMatch] string mediaType, string filename)
        {
            Console.WriteLine("got media type {0}, file {1}", mediaType, filename);
            //PERQemu.Sys.IOB.Z80System.LoadFloppyDisk(null);
            //Console.WriteLine("Created.");
        }

        // todo: allow for all supported types, not just shugart
        // todo: all media requires a pathname, though the actual file doesn't get
        //       written until it's saved.
        // todo: make relative to Disks/ dir
        //[Command("create harddisk", "Create and mounts a new, unformatted hard disk image")]
        //private void CreateHardDisk()
        //{
        //    //PERQemu.Sys.IOB.DiskController.LoadImage(null); // fixme 
        //    //Console.WriteLine("Created.");
        //}

        [Command("load floppy", "Mounts a floppy disk image")]
        [Command("storage load floppy", "Mounts a floppy disk image")]
        private void LoadFloppy(string imagePath)
        {
            try
            {
                // Assign to our configuration
                PERQemu.Config.Current.AssignMedia(0, imagePath);

                if (PERQemu.Controller.State > RunState.Off)
                {
                    PERQemu.Sys.LoadMedia(DeviceType.Floppy, imagePath, 0);
                    Console.WriteLine("Loaded.");
                }
                else
                {
                    Console.WriteLine("Assigned '{0}' to unit {1}.",
                                      Paths.Canonicalize(imagePath), 0);

                    // Force a reload at power on
                    PERQemu.Config.Changed = true;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [Command("unload floppy", "Unmounts a floppy disk image")]
        [Command("storage unload floppy", "Unmounts a floppy disk image")]
        private void UnloadFloppy()
        {
            // Ugh.  if the machine is running,
            // then if the unit exists and is loaded,
            // then see if the media has changed, is writable and the user settings
            // require saving (or asking)
            // THEN unload the disk and clear the media path.
            // otherwise just clear the mediapath for unit 0
            var floppy = PERQemu.Config.Current.Drives[0];  // always unit 0
            //PERQemu.Sys.UnloadMedia(floppy);
        }

        [Command("save floppy", "Save the current in-memory floppy disk to an image file")]
        private void SaveFloppyAs(string imagePath)
        {
            try
            {
                PERQemu.Sys.SaveMedia(DeviceType.Floppy, imagePath, 0); // hardcoded unit 0 ugh
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [Command("load harddisk", "Mount an existing hard disk image")]
        private void LoadHardDisk(string imagePath, int unit = 1)
        {
            try
            {
                // Save the image path into the configuration record so the
                // drive will be loaded at power up.  If the machine is already
                // configured, then dynamically load it -- a "hot swap" if you
                // will, even though the fixed disk types didn't support that!
                PERQemu.Config.Current.AssignMedia(unit, imagePath);

                if (PERQemu.Controller.State > RunState.Off)
                {
                    PERQemu.Sys.LoadMedia(DeviceType.Disk14Inch, imagePath, unit);  // FIXME
                    Console.WriteLine("Loaded.");
                }
                else
                {
                    Console.WriteLine("Assigned '{0}' to unit {1}.",
                                      Paths.Canonicalize(imagePath), unit);

                    // Force a reload at power on
                    PERQemu.Config.Changed = true;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        // todo: like floppy, controller should/will remember name of file loaded
        // todo: allow user to specify unit # when appropriate (default 0)
        [Command("save harddisk", "Save the current hard disk to an image file")]
        private void SaveHardDisk(string imagePath)
        {
            try
            {
                PERQemu.Sys.SaveMedia(DeviceType.Disk14Inch, imagePath, 1); // FIXME  ACK
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }


        /// <summary>
        /// Get the next available defined unit number of a particular type. naw
        /// </summary>
        public int GetUnitForDevice(DeviceType drv)
        {
            var u = (drv == DeviceType.Floppy ? 0 : 1);  // ugh
            return u;
        }

        /// <summary>
        /// Enter a private subsystem to define a new storage device type.
        /// Must reference a (previously defined) geometry and performance
        /// record.  Quietly sets the prefix, then restores it on "done".
        /// </summary>
        [Command("storage define", "Define a new drive type", Discreet = true)]
        private void StorageDefine(DeviceType type, string name)
        {
            _dev = new StorageDevice();
            _dev.Info.Type = type;
            _dev.Info.Name = name;
            PERQemu.CLI.SetPrefix("storage define");
        }

        [Command("storage define description", "Set description of the new drive")]
        private void DefineDesc(string desc)
        {
            _dev.Info.Description = desc;
        }

        [Command("storage define geometry", "Define the new drive's geometry")]
        private void DefineNewGeometry(string tag, ushort cyl, byte heads, ushort sec, ushort secSize = 512, byte hdrSize = 0)
        {
            // Add a new geometry record to the hash
            var geom = new DeviceGeometry(cyl, heads, sec, secSize, hdrSize);
            PERQemu.Config.AddGeometry(tag, geom);
            _dev.Geometry = geom;
        }

        [Command("storage define geometry")]
        private void DefineGeometry(string tag)
        {
            _dev.Geometry = PERQemu.Config.GetGeometry(tag);
        }
        [Command("storage define performance", "Define the new drive's performance characteristics")]
        private void DefineNewSpecs(string tag, int rpm, int pulse, int delay, int seekMin, int seekMax, int settle, int xfer)
        {
            var specs = new DevicePerformance(rpm, pulse, delay, seekMin, seekMax, settle, xfer);
            PERQemu.Config.AddDriveSpecs(tag, specs);
            _dev.Specs = specs;
        }

        [Command("storage define performance")]
        private void DefineSpecs(string tag)
        {
            _dev.Specs = PERQemu.Config.GetDriveSpecs(tag);
        }

        [Command("storage define removable", "Define if the drive has removable media")]
        private void DefineRemovable(bool canRemove)
        {
            _dev.Info.IsRemovable = canRemove;
        }

        [Command("storage define done", "Complete and save the new drive definition")]
        private void DefineDone()
        {
            PERQemu.Config.AddKnownDrive(_dev);
            SetStoragePrefix();
        }

        private static StorageDevice _dev;
    }
}
