//
// StorageCommands.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.Config;

namespace PERQemu.UI
{
    /// <summary>
    /// Commands for configuring storage devices, including top-level
    /// shortcuts for loading and unloading media files.
    /// </summary>
    public class StorageCommands
    {

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

        // NOW, for creating blank media... do we just define disk types and geometries
        // outside of the emulator and embed that in the header!?  it would be very cool
        // to have a Conf/DriveGeometries.cfg and load it at boot time, so that we don't
        // have to update the code when new disk types are imaged (or synthesized with
        // new installs)
        //
        //      unless DEBUG, storage define IsDescreet!
        //
        //      storage define <class> <type> <cyls> <heads> <sectors> <bytes> <hdr> <flags> <rpm> <desc>
        //      storage create media <type> <file>
        // SO,
        //      storage define Floppy "DSDD" 77 2 26 256 0 360
        //      storage define Disk5Inch "XT2190" 918 15 16 512 16 0 3600 "Maxtor XT2190 159MB MFM"
        //      storage define (subsystem)
        //          type <class>       -- DriveType
        //          name <string>       -- short name/key
        //          cylinders, heads,
        //          sectors, bytes,
        //          header bytes, rpm   -- numeric, required
        //          flags <enum>        -- optional, default 0x0
        //          description <string> -- human readable
        //          done                -- save and return
        //          cancel              -- return
        // then
        //      storage create MaxtorXT2190 <file>  -- creates a blank file on disk
        //      storage load harddisk <file>        -- assigns it to unit 1
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

        [Command("storage load media", "Attach a media file to a storage unit")]
        public void LoadMedia(byte unit, string filename)
        {
            Console.WriteLine("Test: load unit={0}, file={1}", unit, filename);
        }

        [Command("storage define", "Define a storage device and its geometry")]
        public void DefineGeometry(string name, DriveType type, ushort cyl, byte heads, byte sec, uint bps = 512)
        {
            Console.WriteLine("Defining a new drive type!  Class={0}, type={1}", type, name);
            Console.WriteLine("Geometry {0}/{1}/{2} with {3}-byte sectors ({4} KB capacity)",
                              cyl, heads, sec, bps, (cyl * heads * sec * bps) / 1024);
        }


        // todo: move the interface to the floppy/hard disk loading and saving
        // to PERQsystem?
        // todo: allow for second hard drive in perq2 models

        [Command("create floppy", "Creates and mounts a new, unformatted floppy disk image")]
        private void CreateFloppyDisk()
        {
            // todo: allow specification of sssd, ssdd, dssd, dsdd
            // all floppies created in imd format by default?
            PERQemu.Sys.IOB.Z80System.LoadFloppyDisk(null);
            Console.WriteLine("Created.");
        }

        [Command("load floppy", "Mounts a floppy disk image")]
        private void LoadFloppy(string imagePath)
        {
            try
            {
                PERQemu.Config.Current.AssignMedia(imagePath, 0);

                if (PERQemu.Sys.State != RunState.Off)
                {
                    PERQemu.Sys.LoadMedia(DriveType.Floppy, imagePath, 0);
                    Console.WriteLine("Loaded.");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [Command("unload floppy", "Unmounts a floppy disk image")]
        private void UnloadFloppy()
        {
            PERQemu.Sys.IOB.Z80System.UnloadFloppyDisk();
        }

        // todo: floppy remembers the path we loaded from so we don't have to?
        // todo: make relative to Disks/ dir

        [Command("save floppy", "Save the current in-memory floppy disk to an image file")]
        private void SaveFloppy(string imagePath)
        {
            try
            {
                PERQemu.Sys.IOB.Z80System.SaveFloppyDisk(imagePath);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        // todo: allow for all supported types, not just shugart
        // todo: allow for multiple units in perq 2 models
        // todo: allow user to define new types here?  or just use built-in ones?
        [Command("create harddisk", "Create and mounts a new, unformatted hard disk image")]
        private void CreateHardDisk()
        {
            PERQemu.Sys.IOB.DiskController.LoadImage(null);
            Console.WriteLine("Created.");
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
                // (Someday a mountable SMD pack option would take advantage, tho)
                PERQemu.Config.Current.AssignMedia(imagePath, unit);

                if (PERQemu.Sys.State != RunState.Off)
                {
                    PERQemu.Sys.LoadMedia(DriveType.Disk14Inch, imagePath, unit);
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
                PERQemu.Sys.IOB.DiskController.SaveImage(imagePath); // FIXME
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
        public int GetUnitForDevice(DriveType drv)
        {
            var u = (drv == DriveType.Floppy ? 0 : 1);  // ugh
            return u;
        }
    }
}
