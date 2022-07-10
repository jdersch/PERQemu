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
using System.IO;
using System.Diagnostics;

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
        // The storage subsystem provides the raw tools for manipulating devices:
        //
        //  storage define              -- create geometries, specs, devices
        //  storage list                -- show what types are available
        //  storage create <type> <file> -- creates and formats a blank file on disk
        //  storage load <file>         -- assigns it to next appropriate unit
        //  storage load <n> <file>     -- load unit <n> from <file>
        //  storage unload <n>          -- unload unit <n> (if removable)
        //  storage save <n>            -- save unit <n> (if loaded)
        //  storage save <n> <file>     -- save unit <n> as <file> (if loaded)
        //  storage save <n> <file> <fmt> -- saveaswithformat()

        [Command("storage", "Enter the storage configuration subsystem", Prefix = true)]
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

        [Command("storage status", "Show status of loaded storage devices")]
        private void StorageStatus()
        {
            if (PERQemu.Controller.State == RunState.Unavailable)
            {
                Console.WriteLine("The PERQ is powered off.");
                return;
            }

            PERQemu.Sys.CheckMedia();
        }

        //
        // Floppy Commands
        //

        [Command("load floppy", "Load a floppy disk image")]
        private void LoadFloppy(string filename)
        {
            try
            {
                LoadInternal(0, filename);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to load {filename}: {e.Message}");
            }
        }

        [Command("unload floppy", "Unload the loaded floppy disk")]
        private void UnloadFloppy()
        {
            try
            {
                UnloadInternal(0);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to unload floppy: {e.Message}");
            }
        }

        [Command("save floppy", "Save the loaded floppy disk")]
        private void SaveFloppy()
        {
            SaveFloppyAs("");       // Lazy...
        }

        [Command("save floppy as", "Save the loaded floppy disk with a new name")]
        private void SaveFloppyAs(string filename)
        {
            try
            {
                SaveInternal(0, filename);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to save floppy: {e.Message}");
            }
        }

        //
        // Hard Disk Commands
        //

        private bool OKtoLoad()
        {
            // Hard drives in the PERQ are generally fixed and non-removable;
            // Don't allow them to be updated while the machine is running
            if (PERQemu.Controller.State > RunState.Off &&
                PERQemu.Controller.State < RunState.Halted)
            {
                Console.WriteLine("Cannot add or remove hard drives while the PERQ is running.");
                Console.WriteLine("Please power off the emulation to reconfigure storage devices.");
                return false;
            }

            return true;
        }

        [Command("load harddisk", "Load a hard disk image")]
        private void LoadHardDisk(string filename)
        {
            if (!OKtoLoad()) return;

            try
            {
                LoadInternal(1, filename);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to load {filename}: {e.Message}");
            }
        }

        [Command("unload harddisk", "Unload the primary hard disk [unit 1]")]
        private void UnloadHardDisk()
        {
            UnloadHardDisk(1);
        }

        [Command("unload harddisk", "Unload a hard disk [unit #]")]
        private void UnloadHardDisk(int unit)   // "int unit = 1" here doesn't work
        {
            if (!OKtoLoad()) return;

            try
            {
                UnloadInternal(unit);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to unload hard disk {unit}: {e.Message}");
            }
        }

        [Command("save harddisk", "Save the hard disk")]
        private void SaveHardDisk()
        {
            SaveHardDiskAs("");     // Yeah, silly.
        }

        [Command("save harddisk as", "Save the hard disk with a new name")]
        private void SaveHardDiskAs(string filename)
        {
            try
            {
                SaveInternal(1, filename);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to save hard disk: {e.Message}");
            }
        }


        //
        // Common Routines
        //

        /// <summary>
        /// The common routine to load a device.
        /// </summary>
        /// <remarks>
        /// Rules for Loading:
        ///     if the machine is not defined:
        ///         update the Configuration record only; return
        ///     if the machine IS defined:
        ///         if the drive IS already loaded:
        ///             if the media is NOT Modified, implicit unload, continue
        ///             if the media IS Modified, tell user to save/unload manually, exit
        ///         if the drive is NOT loaded, load it.
        ///
        /// The caller must check the machine's running state and inform the user
        /// if the media type can't be loaded (non-removable types, generally).
        /// </remarks>
        private bool LoadInternal(int unit, string filename)
        {
            // If the machine is defined, see if the drive is loaded
            if (PERQemu.Controller.State > RunState.Unavailable && PERQemu.Sys.Volumes[unit].IsLoaded)
            {
                // Is the drive modified?
                if (PERQemu.Sys.Volumes[unit].IsModified)
                {
                    // Grumble about it.  Could let Unload prompt them to save...
                    Console.WriteLine($"Drive {unit} is modified; please save/unload first.");
                    return false;
                }

                // Nope! Do the implicit unload, go ahead with the reload
                UnloadInternal(unit);
            }

            // The Configurator performs a bunch of sanity checks, and if the
            // file is kosher sets the new canonicalized path in the Config
            // record.  On success or failure we get back a Reason string with
            // the result to inform the user.
            var ok = PERQemu.Config.AssignMediaTo(unit, filename);

            // Report success or failure
            Console.WriteLine(PERQemu.Config.Current.Reason);

            // If we successfully updated the Configuration, see if the
            // machine is in a state where we should update it too.
            if (ok && (PERQemu.Controller.State > RunState.Unavailable))
            {
                // LoadMedia will load or reload the drive as appropriate
                ok = PERQemu.Sys.LoadMedia(PERQemu.Config.Current.Drives[unit]);
            }

            return ok;
        }

        /// <summary>
        /// The common routine to unload a device.
        /// </summary>
        /// <remarks>
        /// Rules for Unloading:
        ///     if the machine IS defined:
        ///         if the drive is NOT a Removable type and the machine is NOT running, proceed
        ///             otherwise return (can't unload)
        ///         if the drive IS Removable:
        ///             if the drive is NOT Modified, unload.
        ///             if the drive IS Modified, do the save routine.
        ///     issue the Unload to the actual device (Volumes[unit])
        ///     clear the Volumes entry 
        ///     update the Configuration record; return.
        /// 
        /// The caller checks the run state against the device type; if not a
        /// removable drive, it rejects the request.  Thus we assume that it's
        /// okay to unload the device, but we do the check to see if it needs
        /// saving first.
        /// </remarks>
        private void UnloadInternal(int unit)
        {
            // If the machine is defined, check the drive and unload it
            if (PERQemu.Controller.State != RunState.Unavailable)
            {
                // Tell the PERQ to save/unload it
                PERQemu.Sys.UnloadMedia(unit);
            }

            // Now zap the media path in the Configuration
            PERQemu.Config.Current.SetMediaPath(unit, string.Empty);
            Console.WriteLine("Drive {0} unassigned.", unit);
        }

        /// <summary>
        /// Common routine to save devices upon request.
        /// </summary>
        /// <remarks>
        /// Rules for Saving:
        ///     If they give us a new filename, then we "save as" (and update
        ///     the config to show the new pathname)
        /// 
        ///     If they give us a new format, we change the extension and do a
        ///     SaveAsWithFormat, updating the info properties to reflect the change
        /// 
        ///     Any device can be saved in any machine state when defined;
        ///     should we automatically pause emulation to take a stable snapshot?
        /// 
        ///     if the PERQ is undefined, no save;
        ///     if bad unit, drive not loaded, is readonly - no save
        ///     if the format IS changed, the filename is changed;
        ///     if the filename IS changed:
        ///         qualify new name/new ext and check the new path:
        ///             valid? proceed, otherwise - no save
        ///             exists, and overwrite set? proceed, otherwise - no save
        ///         changes to filename sets IS modified
        ///     if the device is NOT modified - no save
        ///     if the device IS modified:
        ///         if the filename is NOT changed:  .Save()
        ///         if the file IS changed: .SaveAs()
        ///         if the format IS changed: .SaveAsWithFormat()
        ///
        ///     SaveMedia might need extra hooks to accommodate the format switch
        /// </remarks>
        public bool SaveInternal(int unit, string filename /* todo: format */)
        {
            // Save should be okay any time the machine is defined
            if (PERQemu.Controller.State == RunState.Unavailable)
            {
                Console.WriteLine($"No PERQ!  Can't save drive {unit}.");
                return false;
            }

            if (PERQemu.Sys.Volumes[unit] == null)
            {
                throw new InvalidOperationException($"Drive {unit} is not loaded");
            }

            // What's our current (loaded) filename?
            var pathname = PERQemu.Sys.Volumes[unit].Filename;

            // Is it read-only?  Then saving in place is verboten
            if (string.IsNullOrEmpty(filename) && !PERQemu.Sys.Volumes[unit].Info.IsWritable)
            {
                var cmd = PERQemu.Sys.Volumes[unit].Info.Type == DeviceType.Floppy ? "floppy" : "harddisk";

                Console.WriteLine($"The image '{pathname}' is read-only; cannot overwrite.");
                Console.WriteLine($"Please use 'save {cmd} as' to save a copy with a different name.");
                return false;
            }

            // If given a new name, clean it up and nudge it into the Disks/
            // directory, with a proper extension if needed.  No check here is
            // done to see if the new name will overwrite an existing one...
            if (!string.IsNullOrEmpty(filename))
            {
                var fmt = PERQemu.Sys.Volumes[unit].FileInfo.Format;
                var ext = FileUtilities.GetExtensionForFormat(fmt);

                pathname = Paths.QualifyPathname(filename, Paths.DiskDir, ext, true);

                // Is it actually different?
                if (pathname != PERQemu.Sys.Volumes[unit].Filename)
                {
                    Console.WriteLine($"Updating from {PERQemu.Sys.Volumes[unit].Filename}");

                    // Save the canonicalized path to the Configuration
                    PERQemu.Config.Current.SetMediaPath(unit, pathname);
                }
            }

            // Inform the PERQ.  It will pick up a changed filename and update
            // itself accordingly, calling the drive's .Save()
            var ok = PERQemu.Sys.SaveMedia(unit);

            if (ok)
            {
                Console.WriteLine($"Drive {unit} saved to '{pathname}'.");
            }

            // If Debugging isn't enabled we won't see messages if the drive
            // isn't loaded, isn't modified, or the user preferences say not
            // to autosave -- or, soon, if they reject the save when asked.
            // At that point we have already updated the Config with a new name
            // (whoops?) so this might still need some messin' with.
            return ok;
        }


        //
        // Create new media files
        //

        /// <summary>
        /// Creates a new blank devices, names it, saves it in the Disks/ dir.
        /// For now, all new drives are created in PRQM format by default. :-)
        /// </summary>
        [Command("storage create", "Creates a new blank, formatted floppy, disk or tape image")]
        private void CreateMedia([KeywordMatch("DriveTypes")] string driveType,
                                 [PathExpand] string filename,
                                 bool overwrite = false)
        {
            // Is it a known/valid type?
            var d = PERQemu.Config.GetKnownDeviceByName(driveType);

            if (d == null)
            {
                Console.WriteLine($"Sorry, don't know what a '{driveType}' is.");
                return;
            }

            // Fix up the pathname, see if it exists
            var pathname = Paths.QualifyPathname(filename, Paths.DiskDir, ".prqm", true);

            if (File.Exists(pathname) && !overwrite)
            {
                Console.WriteLine($"File '{pathname}' already exists.  Please choose another name,");
                Console.WriteLine("or to replace the file repeat the command with overwrite 'true'.");
                return;
            }

            // Don't create a file with the same name as one that's loaded!
            // This might not be foolproof, but make the check all the same
            if (PERQemu.Sys != null)
            {
                foreach (var drive in PERQemu.Sys.Volumes)
                {
                    if (drive?.Filename == pathname)
                    {
                        Console.WriteLine($"File '{pathname}' is currently loaded!");
                        Console.WriteLine("Please unload the drive first, or choose another filename.");
                        return;
                    }
                }
            }

            // Create a new copy
            var newDrive = new StorageDevice(d.Info, d.Geometry, d.Specs);
            newDrive.Filename = pathname;

            Console.Write("Formatting new drive... ");
            newDrive.Format();
            Console.WriteLine("done!");

            Console.WriteLine("Saving the image... ");
            newDrive.Save();

            // The formatter will announce success.  All done!
        }

        /// <summary>
        /// Lists the known device types, all purdy like.
        /// </summary>
        [Command("storage list", "List known storage devices")]
        private void ListKnownTypes()
        {
            string[] drives = PERQemu.Config.GetKnownDevices();
            Array.Sort(drives);

            Console.WriteLine("Drive Class    Drive Type    Description");
            Console.WriteLine("-----------    ----------    -----------------------------------------------");

            foreach (var d in drives)
            {
                var drive = PERQemu.Config.GetKnownDeviceByName(d);
                Console.WriteLine("{0}{1}{2}",
                                  drive.Info.Type.ToString().PadRight(15),
                                  drive.Info.Name.PadRight(14),
                                  drive.Info.Description);
            }
        }

        /// <summary>
        /// Shows the detailed specs for a particular drive.
        /// </summary>
        [Command("storage show", "Show device specifications")]
        private void ShowDeviceSpecs([KeywordMatch("DriveTypes")] string driveType)
        {
            var drive = PERQemu.Config.GetKnownDeviceByName(driveType);
            if (drive == null)
            {
                Console.WriteLine($"Unknown drive type '{driveType}'");
                return;
            }

            var capacity = drive.Geometry.TotalBytes / 1000000;     // "MB", not "MiB"
            var specs = drive.Specs.ToString().Split('\n');

            Console.WriteLine($"Device type:  {drive.Info.Name} (class {drive.Info.Type})");
            Console.WriteLine($"Description:  {drive.Info.Description}");
            Console.WriteLine($"Capacity:     {capacity}MB (formatted)");
            Console.WriteLine($"Geometry:     {drive.Geometry}");
            Console.WriteLine($"Performance:  {specs[0]}");
            Console.WriteLine($"              {specs[1]}");
            Console.WriteLine($"Removable:    {drive.Info.IsRemovable}");
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
        [Command("storage define", "Define a new drive type", Prefix = true, Discreet = true)]
        private void StorageDefine(DeviceType type, string name)
        {
            _dev = new StorageDevice();
            _dev.Info.Type = type;
            _dev.Info.Name = name;
            PERQemu.CLI.SetPrefix("storage define");
        }

        [Conditional("DEBUG")]
        [Command("storage define commands", "Show the sooper sekrit incantations")]
        private void ShowDefineCommands()
        {
            PERQemu.CLI.ShowCommands("storage define");            
        }

        [Command("storage define description", "Describe the drive's make and model")]
        private void DefineDesc(string desc)
        {
            _dev.Info.Description = desc;
        }

        [Command("storage define geometry", "Define the drive's geometry")]
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

        [Command("storage define performance", "Define the drive's performance characteristics")]
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

        [Command("storage define removable", "Define whether the drive has removable media")]
        private void DefineRemovable(bool canRemove)
        {
            _dev.Info.IsRemovable = canRemove;
        }

        [Command("storage define done", "Complete and save the new drive definition")]
        private void DefineDone()
        {
            try
            {
                PERQemu.Config.AddKnownDrive(_dev);
            }
            catch (Exception e)
            {
                Log.Error(Category.Emulator, "Could not define drive {0}: {1}",
                          _dev.Info.Name, e.Message);
            }
            SetStoragePrefix();
        }

        private static StorageDevice _dev;
    }
}
