//
// StorageCommands.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.UI
{
    /// <summary>
    /// Commands for configuring storage devices (floppy, tape and hard drives),
    /// including top-level commands for loading, unloading and saving media files.
    /// </summary>
    public class StorageCommands
    {

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
        void StorageStatus()
        {
            if (PERQemu.Controller.State == RunState.Off)
            {
                Console.WriteLine("The PERQ is powered off.");
                return;
            }

            // Summary
            for (var unit = 0; unit < PERQemu.Sys.Volumes.Length; unit++)
            {
                var dev = PERQemu.Sys.Volumes[unit];

                if (dev != null)
                {
                    Console.Write($"Drive {unit} ({dev.Info.Type}) is online, ");
                    if (dev.Info.IsRemovable) Console.Write("removable, ");

                    if (dev.IsLoaded)
                    {
                        Console.Write($"loaded ({dev.Filename})");
                        if (dev.Info.IsBootable) Console.Write(", bootable");
                        if (dev.Info.IsWritable) Console.Write(", writable");
                        if (dev.IsModified) Console.Write(", modified");
                    }
                    else
                    {
                        Console.Write("no media loaded");
                    }
                    Console.WriteLine();
                }
            }
        }

        [Command("storage status", "Show detailed status of a loaded storage device")]
        void StorageStatus(int unit)
        {
            if (PERQemu.Controller.State == RunState.Off)
            {
                Console.WriteLine("The PERQ is powered off.");
                return;
            }

            if (unit < 0 || unit > PERQemu.Sys.Volumes.Length)
            {
                Console.WriteLine("Bad unit number.");
                return;
            }

            var dev = PERQemu.Sys.Volumes[unit];

            if (dev == null)
            {
                Console.WriteLine($"Unit #{unit} is not loaded.");
                return;
            }

            // Detailed status
            Console.Write($"Drive {unit} ({dev.Info.Type}) is online");

            if (!dev.IsLoaded)
            {
                Console.WriteLine(", no media loaded.");
                return;
            }
            Console.WriteLine($", type {dev.Info.Name}");

            Console.WriteLine($"Filename: {dev.Filename} ({dev.FileInfo.Format})");

            Console.Write("Flags:    Loaded");
            if (dev.Info.IsRemovable) Console.Write(", removable");
            if (dev.Info.IsBootable) Console.Write(", bootable");
            if (dev.Info.IsWritable) Console.Write(", writable");
            if (dev.IsModified) Console.Write(", modified");
            Console.WriteLine(".");

            if (dev.Info.Type == DeviceType.Floppy)
            {
                Console.WriteLine($"FS hint:  {dev.FileInfo.FSType}");
            }

            if (dev.FileInfo.TextLabel != null)
            {
                var label = dev.FileInfo.DecodeTextLabel();

                if (label.Split('\n').Length > 1)
                {
                    Console.WriteLine("Label:\n------");
                    Console.WriteLine(label);
                    Console.WriteLine("------");
                }
                else
                {
                    Console.WriteLine($"Label:    {label}");
                }
            }

            // Other info?  (don't duplicate "storage show" drive type details)
        }


        #region Floppy commands

        //
        // Floppy Commands
        //

        [Command("load floppy", "Load a floppy disk image")]
        void LoadFloppy(string filename)
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
        void UnloadFloppy()
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
        void SaveFloppy()
        {
            SaveFloppyAs("");       // Lazy...
        }

        [Command("save floppy as", "Save the loaded floppy disk with a new name")]
        void SaveFloppyAs(string filename)
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

        [Command("save floppy as", "Save the loaded floppy disk with a new name and format")]
        void SaveFloppyAs(string filename, Formatters format)
        {
            if (format == Formatters.Unknown)
            {
                Console.WriteLine("Sorry, I'm not a mind reader.  Please choose a valid format.");
                return;
            }

            try
            {
                SaveInternal(0, filename, format);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to save floppy: {e.Message}");
            }
        }

        #endregion

        #region Hard disk commands

        //
        // Hard Disk Commands
        //

        bool OKtoLoad()
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
        void LoadHardDisk(string filename)  // todo: int unit = 1
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

        [Command("unload harddisk", "Unload a hard disk [unit #]")]
        void UnloadHardDisk(int unit = 1)
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
        void SaveHardDisk()
        {
            SaveHardDiskAs("");     // Yeah, silly.
        }

        [Command("save harddisk as", "Save the hard disk with a new name")]
        void SaveHardDiskAs(string filename)
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

        [Command("save harddisk as", "Save the hard disk with a new name and format")]
        void SaveHardDiskWith(string filename, Formatters format)
        {
            if (format == Formatters.Unknown)
            {
                Console.WriteLine("That doesn't make sense.  Please choose a valid format.");
                return;
            }

            try
            {
                SaveInternal(1, filename, format);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to save hard disk: {e.Message}");
            }
        }

        #endregion

        #region Tape commands

        //
        // Streamer Commands
        //

        [Command("load tape", "Load a tape cartridge")]
        void LoadTape(string filename)
        {
            try
            {
                LoadInternal(3, filename);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to load {filename}: {e.Message}");
            }
        }

        [Command("save tape", "Save the loaded cartridge tape")]
        void SaveTape()
        {
            SaveTapeAs("");       // Lazy...
        }

        [Command("save tape as", "Save the loaded tape with a new name")]
        void SaveTapeAs(string filename)
        {
            try
            {
                SaveInternal(3, filename);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to save tape: {e.Message}");
            }
        }

        [Command("save tape as", "Save the loaded tape with a new name and format")]
        void SaveTapeAs(string filename, Formatters format)
        {
            if (format == Formatters.Unknown)
            {
                Console.WriteLine("Sorry, what now?  Please choose a valid format.");
                return;
            }

            try
            {
                SaveInternal(3, filename, format);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to save floppy: {e.Message}");
            }
        }

        [Command("unload tape", "Unload the tape cartridge")]
        void UnloadTape()
        {
            try
            {
                UnloadInternal(3);
            }
            catch (Exception e)
            {
                Console.WriteLine($"Unable to unload tape: {e.Message}");
            }
        }

        #endregion

        #region Internal load, unload and save

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
        bool LoadInternal(int unit, string filename)
        {
            // If the machine is defined, see if the drive is loaded
            if (PERQemu.Controller.State > RunState.Off && PERQemu.Sys.Volumes[unit].IsLoaded)
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
            if (ok && (PERQemu.Controller.State > RunState.Off))
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
        void UnloadInternal(int unit)
        {
            // If the machine is defined, check the drive and unload it
            if (PERQemu.Controller.State != RunState.Off)
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
        /// </remarks>
        public bool SaveInternal(int unit, string filename)
        {
            // Make sure the unit is valid
            CheckSave(unit);

            var pathname = PERQemu.Sys.Volumes[unit].Filename;

            // Trying to save over a read-only file is verboten
            if (string.IsNullOrEmpty(filename) && !PERQemu.Sys.Volumes[unit].Info.IsWritable)
            {
                var t = PERQemu.Sys.Volumes[unit].Info.Type;
                var cmd = (t == DeviceType.Floppy ? "floppy" : (t == DeviceType.TapeQIC ? "tape" : "harddisk"));

                Console.WriteLine($"The image '{pathname}' is read-only; cannot overwrite.");
                Console.WriteLine($"Please use 'save {cmd} as' to save a copy with a different name.");
                return false;
            }

            // If given a new name, clean it up and nudge it into the Disks/
            // directory, with a proper extension if needed.
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

        /// <summary>
        /// Save with a specific format.
        /// </summary>
        /// <remarks>
        /// To minimize duplication of code, just check that the given format is
        /// compatible with the device and if so update it.  Then the normal save
        /// automatically picks up the new file extension and writes it in the
        /// new format.  This could probably be further streamlined, but it's ok
        /// for now...
        /// </remarks>
        public bool SaveInternal(int unit, string filename, Formatters format)
        {
            // Validate the unit
            CheckSave(unit);

            // Is the desired format compatible with this device?
            if (!PERQemu.Sys.Volumes[unit].CanSaveWithFormat(format))
            {
                throw new InvalidOperationException($"{format} incompatible with this device type");
            }

            // Set it and do the normal save
            PERQemu.Sys.Volumes[unit].FileInfo.Format = format;

            return SaveInternal(unit, filename);
        }

        void CheckSave(int unit)
        {
            // Save should be okay any time the machine is defined
            if (PERQemu.Controller.State == RunState.Off)
            {
                throw new InvalidOperationException($"No PERQ!");
            }

            // Weed out bad unit #s
            if (PERQemu.Sys.Volumes[unit] == null)
            {
                throw new InvalidOperationException($"Drive {unit} is not loaded");
            }

            // Drive exists but no current filename == empty drive
            if (string.IsNullOrEmpty(PERQemu.Sys.Volumes[unit].Filename))
            {
                throw new InvalidOperationException($"Drive {unit} is not loaded");
            }
        }

        #endregion

        #region Create, list and show

        //
        // Create new media files
        //

        /// <summary>
        /// Creates a new blank devices, names it, saves it in the Disks/ dir.
        /// For now, all new drives are created in PRQM format by default. :-)
        /// </summary>
        [Command("storage create", "Creates a new blank, formatted floppy, disk or tape image")]
        void CreateMedia([KeywordMatch("DriveTypes")] string driveType,
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
        void ListKnownTypes()
        {
            var drives = PERQemu.Config.GetKnownDevices();
            Array.Sort(drives);

            Console.WriteLine("Drive Type    Drive Class    Description");
            Console.WriteLine("----------    -----------    -----------------------------------------------");

            foreach (var d in drives)
            {
                var drive = PERQemu.Config.GetKnownDeviceByName(d);
                Console.WriteLine("{0}{1}{2}",
                                  drive.Info.Name.PadRight(14),
                                  drive.Info.Type.ToString().PadRight(15),
                                  drive.Info.Description);
            }
        }

        /// <summary>
        /// Shows the detailed specs for a particular drive.
        /// </summary>
        [Command("storage show", "Show device specifications")]
        void ShowDeviceSpecs([KeywordMatch("DriveTypes")] string driveType)
        {
            var drive = PERQemu.Config.GetKnownDeviceByName(driveType);
            if (drive == null)
            {
                Console.WriteLine($"Unknown drive type '{driveType}'");
                return;
            }

            // Take out sector headers to compute total usable space; show in
            // both MB and MiB because $)!#$& disk drive marketing
            var capacity = (drive.Geometry.TotalBlocks * drive.Geometry.SectorSize) / 1000000;
            var mib = (drive.Geometry.TotalBlocks * drive.Geometry.SectorSize) / 1048576.0;
            var specs = drive.Specs.ToString().Split('\n');

            if (drive.Info.Type == DeviceType.TapeQIC)
            {
                // Tapes use the spec fields a little differently than disks.
                // (Rely on specs.ToString including a newline.  Truly horrible.)
                specs[0] = string.Format("[IPS {0}, Start/Stop delay {1}ms, Rate {2:n}KB/sec]",
                                         drive.Specs.RPM,
                                         drive.Specs.StartupDelay,
                                         (drive.Specs.TransferRate / 1000.0));
                specs[1] = string.Format("[Min seek {0:n}ms, Max seek {1:n}ms]",
                                         (drive.Specs.MinimumSeek / 1000.0),
                                         (drive.Specs.MaximumSeek / 1000.0));
            }

            Console.WriteLine($"Device type:  {drive.Info.Name} (class {drive.Info.Type})");
            Console.WriteLine($"Description:  {drive.Info.Description}");
            Console.WriteLine($"Capacity:     {capacity}MB ({mib:n2}MiB), formatted");
            Console.WriteLine($"Geometry:     {drive.Geometry}");
            Console.WriteLine($"Performance:  {specs[0]}");
            Console.WriteLine($"              {specs[1]}");
            Console.WriteLine($"Removable:    {drive.Info.IsRemovable}");
        }

        #endregion

        #region Define new types

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
        void StorageDefine(DeviceType type, string name)
        {
            _dev = new StorageDevice();
            _dev.Info.Type = type;
            _dev.Info.Name = name;
            PERQemu.CLI.SetPrefix("storage define");
        }

        [Conditional("DEBUG")]
        [Command("storage define commands", "Show the sooper sekrit incantations")]
        void ShowDefineCommands()
        {
            PERQemu.CLI.ShowCommands("storage define");
        }

        [Command("storage define description", "Describe the drive's make and model")]
        void DefineDesc(string desc)
        {
            _dev.Info.Description = desc;
        }

        [Command("storage define geometry", "Define the drive's geometry")]
        void DefineNewGeometry(string tag, ushort cyl, byte heads, ushort sec, ushort secSize = 512, byte hdrSize = 0)
        {
            // Add a new geometry record to the hash
            var geom = new DeviceGeometry(cyl, heads, sec, secSize, hdrSize);
            PERQemu.Config.AddGeometry(tag, geom);
            _dev.Geometry = geom;
        }

        [Command("storage define geometry")]
        void DefineGeometry(string tag)
        {
            _dev.Geometry = PERQemu.Config.GetGeometry(tag);
        }

        [Command("storage define performance", "Define the drive's performance characteristics")]
        void DefineNewSpecs(string tag, int rpm, int pulse, int delay, int seekMin, int seekMax, int settle, int xfer)
        {
            var specs = new DevicePerformance(rpm, pulse, delay, seekMin, seekMax, settle, xfer);
            PERQemu.Config.AddDriveSpecs(tag, specs);
            _dev.Specs = specs;
        }

        [Command("storage define performance")]
        void DefineSpecs(string tag)
        {
            _dev.Specs = PERQemu.Config.GetDriveSpecs(tag);
        }

        [Command("storage define removable", "Define whether the drive has removable media")]
        void DefineRemovable(bool canRemove)
        {
            _dev.Info.IsRemovable = canRemove;
        }

        [Command("storage define done", "Complete and save the new drive definition")]
        void DefineDone()
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

        #endregion

        static StorageDevice _dev;
    }
}
