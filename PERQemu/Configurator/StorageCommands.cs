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

        // top-level run time shortcuts (for compat w/previous perqemu cli):
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

        // storage subsystem provides the raw tools for manipulating devices:
        //
        //      storage define              -- create geometries, specs, devices
        //      storage create <type> <file>  -- creates and formats a blank file on disk
        //      storage load <file>         -- assigns it to first appropriate unit
        //      storage load <n> <file>     -- load unit <n> from <file>
        //      storage unload <n>          -- unload unit <n> (if removable)
        //      storage save <n>            -- save unit <n> (if loaded)
        //      storage save <n> <file>     -- save unit <n> as <file> (if loaded)
        //      storage save <n> <file> <fmt> -- saveaswithformat()
        //
        //  storage load/unload/save actually does the heavy lifting at runtime.

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

        //
        // Floppy Commands
        //

        [Command("load floppy", "Load a floppy disk image")]
        [Command("storage load floppy", "Load a floppy disk image")]
        private void LoadFloppy(string imagePath)
        {
            try
            {
                // Make _sure_ we have a floppy drive, which should be unit 0
                // For now we'll assume there's always exactly one but still check
                var vol = PERQemu.Config.Current.GetDrivesOfType(DeviceType.Floppy);

                if (vol.Length == 0)
                {
                    throw new InvalidConfigurationException("No floppy drive");
                }

                // todo: check for existing media! (do implicit unload/save, then reload)

                // Assign to our configuration
                if (PERQemu.Config.AssignMediaTo(vol[0].Unit, imagePath))
                {
                    // Success!  Is the PERQ running?
                    if (PERQemu.Controller.State == RunState.Unavailable)
                    {
                        // Nope, just report the assignment
                        Console.WriteLine(PERQemu.Config.Current.Reason);
                    }
                    else
                    {
                        PERQemu.Sys.LoadMedia(vol[0]);
                        Console.WriteLine("Loaded.");
                    }
                }
                else
                {
                    // Report why the Configurator failed to assign it
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load floppy image {0}: {1}", imagePath, e.Message);
            }
        }

        [Command("unload floppy", "Unload a floppy disk image")]
        [Command("storage unload floppy", "Unload a floppy disk image")]
        private void UnloadFloppy()
        {
            try
            {
                var vol = PERQemu.Config.Current.GetDrivesOfType(DeviceType.Floppy);

                if (vol.Length == 0)
                {
                    throw new InvalidConfigurationException("No floppy drive");
                }

                var unit = vol[0].Unit;

                // If the machine is running, check the drive and unload it
                if (PERQemu.Controller.State != RunState.Unavailable)
                {
                    var floppy = PERQemu.Sys.Volumes[unit];

                    if (floppy.IsLoaded)
                    {
                        if (floppy.IsModified)
                        {
                            // Do the save if requested
                            if (PERQemu.Sys.SaveMedia(unit))
                            {
                                Console.WriteLine("Floppy in drive {0} saved to '{1}'.",
                                                 unit, floppy.Filename);
                            }
                        }

                        // Tell the drive to unload itself; it will update the FDC
                        floppy.Unload();
                        Console.WriteLine("Floppy ejected.");
                    }
                }

                // Now zap the media path in the Configuration
                PERQemu.Config.Current.SetMediaPath(unit, string.Empty);
                Console.WriteLine("Drive {0} now unassigned.", unit);
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to unload floppy: {0}", e.Message);
            }
        }


        [Command("save floppy", "Save the loaded floppy disk (optionally with a new name)")]
        [Command("storage save floppy", "Save the loaded floppy disk (optionally with a new name)")]
        private void SaveFloppy(string imagePath = "")
        {
            // No PERQ, no save
            if (PERQemu.Controller.State == RunState.Unavailable)
            {
                Console.WriteLine("No PERQ!  Can't save a non-existent floppy.");
                return;
            }

            // Make sure we have a floppy drive configured, yadda yadda yadda
            var vol = PERQemu.Config.Current.GetDrivesOfType(DeviceType.Floppy);

            if (vol.Length == 0)
            {
                Console.WriteLine("No floppy drive!");
                return;
            }

            var unit = vol[0].Unit;

            try
            {
                // todo: need to create a canonical path based on what's given
                // (a reverse of the search method; see if there's a dir and if
                // not, prepend Disks/, see if there's an extension and if not
                // add the appropriate one

                // Save the canonicalized path to the Configuration and the live device
                if (!string.IsNullOrEmpty(imagePath))
                {
                    imagePath = Paths.Canonicalize(imagePath);  // fixme
                    PERQemu.Config.Current.SetMediaPath(unit, imagePath);
                    PERQemu.Sys.Volumes[unit].Filename = imagePath;
                }

                // Go save it
                if (PERQemu.Sys.SaveMedia(unit))
                {
                    Console.WriteLine("Floppy saved.");
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0}: {1}", imagePath, e.Message);
            }
        }

        //
        // Hard Disk Commands
        //

        private bool OKtoLoad()
        {
            // Hard drives in the PERQ are generally fixed and non-removable, so
            // we don't support having users add or delete them when the machine
            // is running.  If/when Multibus/SMD is implemented, we'll allow the
            // removable pack drives (CDC 976x) to be loaded and unloaded. :-)
            if (PERQemu.Controller.State > RunState.Off &&
                PERQemu.Controller.State < RunState.Halted)
            {
                Console.WriteLine("Cannot add or remove hard drives while the PERQ is running.");
                Console.WriteLine("Please power off the emulation to reconfigure storage devices.");
                return false;
            }

            return true;
        }

        [Command("load harddisk", "Load an existing hard disk image")]
        [Command("storage load harddisk", "Load an existing hard disk image")]
        private void LoadHardDisk(string imagePath, int unit = 1)
        {
            if (!OKtoLoad()) return;

            try
            {
                // todo: check for existing media! (do implicit unload/save, then reload)

                // Assign to our configuration if appropriate.  This should fail
                // if the unit number is not appropriate (2nd drive on a PERQ-1,
                // wrong type, etc.)
                if (PERQemu.Config.AssignMediaTo(unit, imagePath))
                {
                    // Success!  Report the assignment
                    Console.WriteLine(PERQemu.Config.Current.Reason);

                    // If the machine exists (off/halted), go ahead and load
                    if (PERQemu.Controller.State > RunState.Unavailable)
                    {
                        PERQemu.Sys.LoadMedia(PERQemu.Config.Current.Drives[unit]);
                        Console.WriteLine("Loaded.");
                    }
                }
                else
                {
                    // Report why the Configurator failed to assign it
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0}: {1}", imagePath, e.Message);
            }
        }

        [Command("unload harddisk", "Unload an existing hard disk image")]
        [Command("storage unload harddisk", "Unload an existing hard disk image")]
        private void UnLoadHardDisk(int unit = 1)
        {
            if (!OKtoLoad()) return;

            try
            {
                // check the unit number to make sure it's a hard disk?
                // if the machine is defined but off/halted:
                //      if drive is loaded and modified, check for autosave?
                //      unload it (hard drives will detach from their controllers
                //      and deallocate themselves?)
                // zap the filename in the config record
                Console.WriteLine("Unload harddisk not yet implemented...");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to unload disk unit {0}: {1}", unit, e.Message);
            }
        }

        [Command("save harddisk", "Save the current hard disk (optionally with a new name)")]
        [Command("storage save harddisk", "Save the current hard disk (optionally with a new name)")]
        private void SaveHardDisk(string imagePath = "", int unit = 1)
        {
            // Save should be okay any time the machine is defined
            if (PERQemu.Controller.State == RunState.Unavailable)
            {
                Console.WriteLine("No PERQ!  Can't save non-existent hard disks.");
                return;
            }

            try
            {
                // as with the floppy, should we briefly pause the emulation if running?
                // also need to patch up new filename if provided
                // should test new name before zapping the old one?
                // have to consider AGAIN how to do the save as, save as with fmt
                // commands in a sane and consistent way
                // try to find commonality with floppy/hard disk/tape and do common
                // load/unload/save/saveas/saveasfmt routines with stubs that pass in
                // the correct args/validated unit id/etc

                PERQemu.Sys.SaveMedia(unit);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save hard disk {0}: {1}", unit, e.Message);
            }
        }

        // a debugging thing... flesh it out or remove it eventually
        [Command("storage status", "Show status of current storage devices")]
        private void StorageStatus()
        {
            if (PERQemu.Controller.State != RunState.Unavailable)
                PERQemu.Sys.CheckMedia();
        }

        //
        // Creating new media files
        //

        /// <summary>
        /// Creates a new blank devices, names it, saves it in the Disks/ dir.
        /// </summary>
        [Command("storage create", "Creates a new blank, formatted floppy, disk or tape image")]
        private void CreateMedia([KeywordMatch] string mediaType,
                                 [PathExpand] string filename,
                                 bool overwrite = false)
        {
            // look up the mediaType string in the known drives list
            // if a valid type, check filename
            //      expand and validate (add Disks/, appropriate extension)
            //      allow overwriting but not by default
            //      if filename is valid:
            //          create the new StorageDevice
            //          .Format()
            //          .Save()
            // suhweeet.

            Console.WriteLine("got media type {0}, file {1}", mediaType, filename);
        }

        //
        // Defining Storage Types
        //
        // "Advanced" commands for loading the database of predefined storage
        // devices or for creating new ones.  These are not generally needed by
        // regular users as most supported drive types are already available,
        // but enthusiasts and tinkerers can play around.  Very little error
        // or sanity checking, so enter at your own risk.
        //

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
