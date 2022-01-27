//
// Configurator.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Collections;
using System.Collections.Generic;

namespace PERQemu.Config
{
    /// <summary>
    /// Configurator works with the CLI and/or GUI to create, modify, load and
    /// save PERQ configurations.  It encapsulates all the rules about optional
    /// hardware combinations so the user can't create a PERQ that the Emulator
    /// can't instantiate and run properly.
    /// </summary>
    public sealed class Configurator
    {
        public Configurator()
        {
            _default = new Configuration();
            _prefabs = new Hashtable();

            Initialize();
        }

        public void Initialize()
        {
            Configuration conf;
            StorageDevice[] drives;

            // For now, we just build up a set of the standard PERQ models.
            // This really ought to be read in from a file instead.  Ugly...

            //
            // PERQ-1 drive setup: one floppy, one Shugart.  Should be a 12MB drive;
            // for now we'll default to the POS F.0 image (but should find a D.6!)
            //
            drives = new StorageDevice[2];
            drives[0] = new StorageDevice(DriveType.Floppy, 0, "");
            drives[1] = new StorageDevice(DriveType.Disk14Inch, 1, Paths.BuildDiskPath("f0.phd"));

            //
            // The original machine: 4K, 256K MEM, Portrait
            //
            conf = new Configuration("PERQ1", "Original PERQ (small)",
                                     ChassisType.PERQ1,
                                     CPUType.PERQ1,
                                     256 * 1024,
                                     IOBoardType.IOB,
                                     OptionBoardType.None,
                                     IOOptionType.None,
                                     DisplayType.Portrait,
                                     TabletType.BitPad,
                                     drives);
            _prefabs[conf.Name] = conf;

            //
            // The typical PERQ-1: 16K, 1MB, Portrait, Ethernet
            // 
            // Default drive will be a 24MB Shugart, POS F.1
            //
            drives[1].MediaPath = Paths.BuildDiskPath("f1.phd");

            conf = new Configuration("PERQ1A",
                                     "PERQ-1 w/16K CPU, 1MB",
                                     ChassisType.PERQ1,
                                     CPUType.PERQ1A,
                                     ONE_MEG,
                                     IOBoardType.IOB,
                                     OptionBoardType.OIO,
                                     IOOptionType.Link | IOOptionType.Ether,
                                     DisplayType.Portrait,
                                     TabletType.BitPad,
                                     drives);
            _prefabs[conf.Name] = conf;

            //
            // Reconfigure drives for the PERQ-2 machines.  Not sure if the
            // first, original PERQ-2 model could actually support two 8" drives
            // so to slightly differentiate it, we'll just allow one.
            //
            drives = new StorageDevice[2];
            drives[0] = new StorageDevice(DriveType.Floppy, 0, "");
            drives[1] = new StorageDevice(DriveType.Disk8Inch, 1, "");

            conf = new Configuration("PERQ2",
                                    "PERQ-2 w/16K CPU, 1MB",
                                    ChassisType.PERQ2,
                                    CPUType.PERQ1A,
                                    ONE_MEG,
                                    IOBoardType.EIO,
                                    OptionBoardType.None,
                                    IOOptionType.None,
                                    DisplayType.Portrait,
                                    TabletType.Kriz,
                                    drives);
            _prefabs[conf.Name] = conf;

            //
            // Let's say the T1 added support for the second 8" drive
            //
            drives = new StorageDevice[3];
            drives[0] = new StorageDevice(DriveType.Floppy, 0, "");
            drives[1] = new StorageDevice(DriveType.Disk8Inch, 1, "");
            drives[2] = new StorageDevice(DriveType.Disk8Inch, 2, "");

            //
            // The PERQ-2/T1 came with a 1MB Portrait by default
            //
            conf = new Configuration("PERQ2-T1",
                                    "PERQ-2/T1 w/16K CPU, 1MB",
                                    ChassisType.PERQ2,
                                    CPUType.PERQ1A,
                                    ONE_MEG,
                                    IOBoardType.EIO,
                                    OptionBoardType.None,
                                    IOOptionType.None,
                                    DisplayType.Portrait,
                                    TabletType.Kriz,
                                    drives);
            _prefabs[conf.Name] = conf;

            //
            // PERQ-2 T2 and T4 used 5.25" drives
            //
            drives[1] = new StorageDevice(DriveType.Disk5Inch, 1, "");
            drives[2] = new StorageDevice(DriveType.Disk5Inch, 1, "");

            //
            // Our PERQ-2/T2 comes with 2MB, Landscape by default
            //
            conf = new Configuration("PERQ2-T2",
                                    "PERQ-2/T2 w/16K CPU, 2MB",
                                    ChassisType.PERQ2T2,
                                    CPUType.PERQ1A,
                                    TWO_MEG,
                                    IOBoardType.EIO,
                                    OptionBoardType.None,
                                    IOOptionType.None,
                                    DisplayType.Landscape,
                                    TabletType.Kriz,
                                    drives);
            _prefabs[conf.Name] = conf;

            //
            // The rare PERQ-2/T4 came with 4MB, Landscape
            //
            conf = new Configuration("PERQ2-T4",
                                    "PERQ-2/T4 w/16K CPU, 4MB",
                                    ChassisType.PERQ2T2,
                                    CPUType.PERQ24,
                                    FOUR_MEG,
                                    IOBoardType.EIO,
                                    OptionBoardType.None,
                                    IOOptionType.None,
                                    DisplayType.Landscape,
                                    TabletType.Kriz,
                                    drives);
            _prefabs[conf.Name] = conf;

            //
            // Set our current config to defaults.  The user can always
            // override this at start-up by specifying a script on the
            // command line (or reading one in through the menu/CLI).
            // This way PERQsystem is always a valid starting configuration
            // and the GUI form can initialize properly.
            //
            _current = _default;
        }

        public Configuration Default
        {
            get { return _default = new Configuration(); }
        }

        public Configuration Current
        {
            get { return _current; }
            set { _current = value; }
        }

        public bool Changed
        {
            get { return _current.IsModified; }
            set { _current.IsModified = value; }
        }

        /// <summary>
        /// Gets the list of pre-defined configurations.
        /// </summary>
        public string[] GetPrefabs()
        {
            string[] keys = new string[_prefabs.Count];
            _prefabs.Keys.CopyTo(keys, 0);
            return keys;
        }

        /// <summary>
        /// Look up a pre-defined Configuration by name.  Tries the name as
        /// given, and in upper case.  Also recognizes the name "current".
        /// Maybe the _prefabs[] could contain "default", "current", "working"
        /// or other keys so they're just all kept in one place; that might
        /// make it simpler for the GUI to save/retrieve custom or modified
        /// configurations?  I dunno man, I'm just making this up as I go.
        /// </summary>
        public Configuration GetConfigByName(string name)
        {
            if (name.ToLower() == "current")
            {
                return _current;
            }
            else if (_prefabs.ContainsKey(name))
            {
                return (Configuration)_prefabs[name];
            }
            else if (_prefabs.ContainsKey(name.ToUpper()))
            {
                return (Configuration)_prefabs[name.ToUpper()];
            }
            else
            {
                return null;
            }
        }

        /// <summary>
        /// Initiate loading a configuration from disk.  This basically just
        /// hands the file off to the CommandExecutor and runs it as if the
        /// commands were being typed at the console, rather than work out a
        /// specific syntax and file format.  It's cheap, I know.
        /// </summary>
        public bool Load(string path)
        {
            try
            {
                // Feed the script to the CLI
                PERQemu.CLI.ReadScript(path);

                // Save the file name for saving later.  If it was a valid
                // config file (written by SaveConfiguration() below) all the
                // fields will have been set except Filename.
                _current.Filename = path;

                // Flag so the ExecutionController will reinitialize the
                // PERQSystem object at power on.
                _current.IsModified = true;

                // Run the validator just to be sure; we might be loading a
                // hand-written file that might not be 100% kosher.
                return Validate();
            }
            catch (Exception e)
            {
                // If called by the CLI, error messages should appear on the
                // console; the GUI can choose to restore the previous config
                // or set flags appropriately?
                _current.Reason = string.Format("Could not load file '{0}': {1}", path, e.Message);
                return false;
            }
        }

        /// <summary>
        /// Saves the configuration to disk as a simple script file that can
        /// be read at startup (command-line argument), with the "@file" syntax
        /// from the CLI, or using the "Load" button in the Configurator GUI.
        /// </summary>
        public bool Save()
        {
            // If no filename is set, return false so the GUI can put up a
            // save dialog; the CLI can prompt the user to supply a filename.
            if (string.IsNullOrEmpty(_current.Filename))
            {
                _current.Reason = "No filename given for saving.";
                return false;
            }

            try
            {
                using (StreamWriter sw = new StreamWriter(_current.Filename, false))
                {
                    //
                    // Write a small header, then enter configuration mode and
                    // write the basic things first.
                    //
                    sw.WriteLine("# PERQemu configuration file, written " + DateTime.Now);
                    sw.WriteLine("configure");
                    sw.WriteLine("name " + _current.Name);
                    sw.WriteLine("description \"" + _current.Description + "\"");
                    sw.WriteLine("chassis " + _current.Chassis);
                    sw.WriteLine("cpu " + _current.CPU);
                    sw.WriteLine("memory " + _current.MemorySizeInBytes / 1024);
                    sw.WriteLine("io board " + _current.IOBoard);
                    sw.WriteLine("display " + _current.Display);
                    sw.WriteLine("tablet " + _current.Tablet);
                    sw.WriteLine("option board " + _current.IOOptionBoard);

                    //
                    // Enumerate the IO options, if any.
                    //
                    if (_current.IOOptions != IOOptionType.None)
                    {
                        foreach (IOOptionType opt in Enum.GetValues(typeof(IOOptionType)))
                        {
                            if (_current.IOOptions.HasFlag(opt))
                                sw.WriteLine("option " + opt);
                        }
                    }

                    //
                    // Write out the storage configuration.  We only need to save
                    // any devices with an assigned media file.
                    //
                    foreach (var dev in _current.Drives)
                    {
                        if (!string.IsNullOrEmpty(dev.MediaPath))
                        {
                            sw.WriteLine("storage load {0} {1} {2}", dev.Device, dev.MediaPath, dev.Unit);
                        }
                    }

                    sw.WriteLine("done");
                    sw.Close();
                }

                _current.IsSaved = true;
                _current.Reason = string.Format("Configuration '{0}' saved to {1}.",
                                                _current.Name, Paths.Canonicalize(_current.Filename));
                return true;
            }
            catch (Exception e)
            {
                _current.IsSaved = false;
                _current.Reason = e.Message;
                return false;
            }
        }

        /// <summary>
        /// Validate the configuration and set the IsValid property.  This is a
        /// sanity check only; the configurator (CLI or GUI) should simply not
        /// allow bad combinations in the first place.  If it returns false, sets
        /// the Reason property to indicate errors; if valid but unusual, Reason
        /// will contain a warning or note explaining why (this routine does not
        /// log directly to the console).
        /// </summary>
        public bool Validate()
        {
            return Validate(_current);
        }

        public bool Validate(Configuration conf)
        {
            // If our configuration is valid but there are warnings, the
            // reason will be set but IsValid will still be true.
            conf.Reason = "";

            if (CheckCPU(conf) &&
                CheckMemory(conf) &&
                CheckIO(conf) &&
                CheckOptions(conf) &&
                CheckDisks(conf))
            {
                conf.IsValid = true;
            }
            else
            {
                conf.IsValid = false;
            }
            return conf.IsValid;
        }

        /// <summary>
        /// Checks that the CPU selected is compatible with the configuration.
        /// Note that the individual functions do NOT modify the IsValid flag!
        /// </summary>
        public bool CheckCPU()
        {
            return CheckCPU(_current);
        }

        public bool CheckCPU(Configuration conf)
        {
            switch (conf.CPU)
            {
                // 4K CPU only available in PERQ-1 or PERQ-2, not the later T1,
                // T2 or T4 models; 20-bits only, 2MB memory max.
                case CPUType.PERQ1:
                    if (conf.Chassis == ChassisType.PERQ2T2)
                    {
                        conf.Reason = "PERQ1 (4K) CPU not compatible with PERQ-2/T2 or 2/T4.";
                        return false;
                    }

                    if (conf.MemorySizeInBytes > TWO_MEG)
                    {
                        conf.Reason = "PERQ1 (4K) CPU supports a maximum of 2MB of memory.";
                        return false;
                    }
                    break;

                // 16K CPU in the PERQ-2 models didn't support the older 
                // quarter-meg or half-meg memory boards (nitpicky); max 2MB.
                case CPUType.PERQ1A:
                    if (conf.Chassis == ChassisType.PERQ2 && conf.MemorySizeInBytes < HALF_MEG)
                    {
                        conf.Reason = "PERQ2: 16K CPU requires minimum of 512KB of memory.";
                        return false;
                    }
                    else if (conf.Chassis == ChassisType.PERQ2T2 && conf.MemorySizeInBytes < ONE_MEG)
                    {
                        conf.Reason = "PERQ2-T2: 16K CPU requires minimum of 1MB of memory.";
                        return false;
                    }
                    else if (conf.MemorySizeInBytes > TWO_MEG)
                    {
                        conf.Reason = "PERQ1A (16K) CPU supports a maximum of 2MB of memory.";
                        return false;
                    }
                    break;

                // 24-bit, 16K CPU only supported in the PERQ-2/T2 chassis,
                // which coupled with a 24-bit EIO made it a "T4".
                case CPUType.PERQ24:
                    if (conf.Chassis != ChassisType.PERQ2T2)
                    {
                        conf.Reason = "24-bit CPU requires PERQ-2/T2 type chassis.";
                        return false;
                    }
                    break;
            }

            // Must be okay then.
            return true;
        }

        /// <summary>
        /// Checks the selected memory size against CPU addressing limits.
        /// Also checks the Display for a few special cases.
        /// </summary>
        /// <remarks>
        /// 20-bit CPUs can only support 2MB or less; the 24-bit CPU only
        /// supported 4MB (officially).  We allow up to 16MB (the "Universal
        /// Landscape Memory" board fully-populated with 1Mbit DRAMs) but it's
        /// unlikely any PERQ OS supported 8MB or 16MB.  A 32MB board in the
        /// PERQ form factor in 1985 would have been outrageously expensive.
        /// </remarks>
        public bool CheckMemory()
        {
            return CheckMemory(_current);
        }

        public bool CheckMemory(Configuration conf)
        {
            if (conf.CPU == CPUType.PERQ1 || conf.CPU == CPUType.PERQ1A)
            {
                if (conf.MemorySizeInBytes > TWO_MEG)
                {
                    conf.Reason = "20-bit CPU supports maximum of 2MB of memory.";
                    return false;
                }

            }
            else    // 24-bit CPU
            {
                if (conf.MemorySizeInBytes < FOUR_MEG)
                {
                    conf.Reason = "24-bit CPU shipped with minimum of 4MB memory.";
                    return false;
                }
                else if (conf.MemorySizeInBytes > FOUR_MEG)
                {
                    conf.Reason = "OS may not support more than 4MB of memory.";
                    // Just a warning
                }
            }

            // A weird special case
            if (conf.Chassis == ChassisType.PERQ1 &&
                conf.Display == DisplayType.Landscape &&
                conf.MemorySizeInBytes != ONE_MEG)
            {
                conf.Reason = "The exceedingly rare PERQ-1 with Landscape display only had 1MB of memory.";
                // But I'll allow it. :-)
            }
            return true;
        }

        /// <summary>
        /// Check the IO board against the chassis type, and see if the tablet
        /// selection makes sense.
        /// </summary>
        public bool CheckIO()
        {
            return CheckIO(_current);
        }

        public bool CheckIO(Configuration conf)
        {
            if (((conf.IOBoard == IOBoardType.IOB || conf.IOBoard == IOBoardType.CIO) && conf.Chassis != ChassisType.PERQ1) ||
                ((conf.IOBoard == IOBoardType.NIO || conf.IOBoard == IOBoardType.EIO) && conf.Chassis == ChassisType.PERQ1))
            {
                conf.Reason = string.Format("IO Board type {0} not compatible with {1} chassis.", conf.IOBoard, conf.Chassis);
                return false;
            }

            // A few little sanity checks
            if (conf.Chassis == ChassisType.PERQ1 && conf.Tablet.HasFlag(TabletType.Kriz))
            {
                conf.Reason = "PERQ-1 with Kriz tablet is unusual, though supported...";
            }

            // We'll allow it?  But you'll regret it
            if (conf.Tablet == TabletType.None)
            {
                conf.Reason = "No graphics tablet configured!?";
            }
            return true;
        }

        /// <summary>
        /// Checks the IO Options for conflicts.
        /// </summary>
        /// <remarks>
        /// For now, the following restrictions apply:
        ///     - Can't have two Ethernets in a system, unfortunately.  So an
        ///       EIO precludes adding an OIO with Ethernet, though the other
        ///       combinations are valid.
        ///     - Ether3 (when implemented) will be a PERQ-1 option only.
        ///     - MLO (when implemented) will be a PERQ-2 option only.
        ///     - Both Link and Streamer options are "universal" (but aren't
        ///       implemented yet).
        /// </remarks>
        public bool CheckOptions()
        {
            return CheckOptions(_current);
        }

        public bool CheckOptions(Configuration conf)
        {
            switch (conf.IOOptionBoard)
            {
                case OptionBoardType.None:
                    // nothin'
                    break;

                case OptionBoardType.Ether3:
                    if (conf.IOOptions != IOOptionType.None)
                    {
                        conf.Reason = string.Format("Bug? IO Option {0} but extra IO Options selected: {1}", conf.IOOptionBoard, conf.IOOptions);
                        conf.IOOptions = IOOptionType.None;         // force it
                    }

                    if (conf.Chassis != ChassisType.PERQ1)
                    {
                        // Not sure about this, but lets go with it
                        conf.Reason = string.Format("3Mbit Ethernet board not supported in {0}.", conf.Chassis);
                        return false;
                    }
                    break;

                case OptionBoardType.OIO:
                    if (conf.IOOptions == IOOptionType.None)
                    {
                        // Should probably at least add the Link at minimum?
                        conf.Reason = "OIO Board configured but no options selected?";
                        conf.IOOptionBoard = OptionBoardType.None;  // force it
                    }

                    if (conf.IOOptions.HasFlag(IOOptionType.SMD))
                    {
                        // Should reset this, rather than whine?
                        conf.Reason = "OIO Board extraneous option selected, ignored.";
                    }

                    // Disallow EIO + OIO Ethernet... but allow NIO?  Ugh.
                    if ((conf.IOBoard == IOBoardType.EIO) && conf.IOOptions.HasFlag(IOOptionType.Ether))
                    {
                        conf.Reason = string.Format("OIO Board Ethernet option conflicts with {0} board.", conf.IOBoard);
                        return false;
                    }
                    break;

                case OptionBoardType.MLO:
                    if (conf.Chassis == ChassisType.PERQ1)
                    {
                        conf.Reason = string.Format("Multibus/Laser option not supported in {0}.", conf.Chassis);
                        return false;
                    }
                    else
                    {
                        // Someday.  Will have to check other option combos.
                        conf.Reason = "Sorry, the MLO board is not yet supported.";
                        conf.IOOptionBoard = OptionBoardType.None;
                        return false;
                    }
            }

            return true;
        }

        /// <summary>
        /// Checks that the list of disk devices is appropriate for the selected
        /// IO board.
        /// </summary>
        /// <remarks>
        /// For now, traditional configuration rules apply:
        ///     PERQ-1 only has 1 floppy, 1 Shugart 14" disk.
        ///     PERQ-2 only has 1 floppy, 1 or 2 Micropolis 8" disks.
        ///     PERQT2 only has 1 floppy, 1 or 2 5.25" disks.
        /// At the moment, SMD Disks are not supported (requires the as-yet-
        /// unimplemented MLO option board).
        /// If any genuine 8" Micropolis images can be found that came from a
        /// PERQ-1, we'll allow that configuration (with a CIO board) as well.
        /// </remarks>
        public bool CheckDisks()
        {
            return CheckDisks(_current);
        }

        public bool CheckDisks(Configuration conf)
        {
            bool gotFloppy = false;
            bool gotMedia = false;
            int driveCount = 0;

            foreach (var drive in conf.Drives)
            {
                switch (drive.Device)
                {
                    case DriveType.None:
                        // Empty, skip it
                        break;

                    case DriveType.Floppy:
                        if (!gotFloppy)
                        {
                            gotFloppy = true;
                        }
                        else
                        {
                            conf.Reason = "This PERQ only supports one floppy drive.";
                            return false;
                        }
                        break;

                    case DriveType.Disk14Inch:
                    case DriveType.Disk8Inch:
                    case DriveType.Disk5Inch:
                        switch (conf.IOBoard)
                        {
                            case IOBoardType.IOB:
                            case IOBoardType.CIO:
                                if (!((drive.Device == DriveType.Disk14Inch) ||
                                      (drive.Device == DriveType.Disk8Inch && conf.IOBoard == IOBoardType.CIO)))
                                {
                                    conf.Reason = string.Format("IO board type '{0}' does not support disks of type '{1}'.", conf.IOBoard, drive.Device);
                                    return false;
                                }
                                driveCount++;
                                if (driveCount > 1)
                                {
                                    conf.Reason = string.Format("IO board type '{0}' only supports one hard disk.", conf.IOBoard);
                                    return false;
                                }
                                break;

                            case IOBoardType.EIO:
                            case IOBoardType.NIO:
                                if ((conf.Chassis == ChassisType.PERQ2 && drive.Device != DriveType.Disk8Inch) ||
                                    (conf.Chassis == ChassisType.PERQ2T2 && drive.Device != DriveType.Disk5Inch))
                                {
                                    conf.Reason = string.Format("IO board type '{0}' does not support disks of type '{1}'.", conf.IOBoard, drive.Device);
                                    return false;
                                }
                                driveCount++;
                                if (driveCount > 2)
                                {
                                    conf.Reason = string.Format("IO board type '{0}' supports a maximum of 2 hard disks.", conf.IOBoard);
                                    return false;
                                }
                                break;
                        }
                        break;

                    //
                    // Not yet implemented
                    //
                    case DriveType.DiskSMD:
                    case DriveType.Tape9Track:
                    case DriveType.TapeQIC:
                        conf.Reason = string.Format("Sorry, {0} devices are not yet supported.", drive.Device);
                        return false;
                }

                // Check to see if there's a media path defined; if no drives
                // are actually loaded, at least issue a warning (the PERQ won't
                // boot without a hard or floppy disk present)
                if (drive.MediaPath != string.Empty) gotMedia = true;
            }

            if (!gotMedia)
            {
                conf.Reason = "Note: the PERQ won't boot without a hard or floppy disk present.";
            }

            // Must be good then...
            return true;
        }

        public void UpdateStorage(IOBoardType newType)
        {
            UpdateStorage(_current, newType);
        }

        /// <summary>
        /// If the user configures a new IO board, we try to map over any
        /// already defined drives so they don't have to specify them again.
        /// Types are remapped for the kind of controller present on the new
        /// board, and media paths removed (assuming incompatibilities).  This
        /// is kind of a giant hack, one case where a GUI actually makes a LOT
        /// more sense.  Sigh.
        /// </summary>
        public void UpdateStorage(Configuration conf, IOBoardType newType)
        {
            Console.WriteLine("* Updating storage from {0} to {1}", conf.IOBoard, newType);

            foreach (var d in conf.Drives)
            {
                bool keepMedia = false;

                switch (d.Device)
                {
                    case DriveType.Floppy:
                    case DriveType.None:
                        // These are compatible with all IO board types
                        keepMedia = true;
                        break;

                    case DriveType.DiskSMD:
                    case DriveType.TapeQIC:
                    case DriveType.Tape9Track:
                        // Not implemented - ignore?  throw?
                        break;

                    case DriveType.Disk14Inch:
                        // Shugart 14" drives only valid on IOB or CIO (PERQ-1).
                        // Remapped to 8" or 5.25" on EIO/NIO depending on chassis.
                        switch (newType)
                        {
                            case IOBoardType.IOB:
                            case IOBoardType.CIO:
                                // Okay!
                                keepMedia = true;
                                break;

                            case IOBoardType.EIO:
                            case IOBoardType.NIO:
                                // Nope, change it
                                if (conf.Chassis == ChassisType.PERQ2)
                                    conf.Drives[d.Unit].Device = DriveType.Disk8Inch;
                                else if (conf.Chassis == ChassisType.PERQ2T2)
                                    conf.Drives[d.Unit].Device = DriveType.Disk5Inch;
                                break;
                        }
                        break;

                    case DriveType.Disk8Inch:
                        // Micropolis 8" drives are valid in PERQ1 on CIO (rare
                        // but allowed) or PERQ2 on EIO/NIO.  Change to Shugart 14"
                        // if switching to IOB, or MFM 5.25" for EIO/NIO (PERQ2T2)
                        switch (newType)
                        {
                            case IOBoardType.IOB:
                                // Nope
                                conf.Drives[d.Unit].Device = DriveType.Disk14Inch;
                                break;

                            case IOBoardType.CIO:
                                // Allow in the rare/weird case of a "CIO Micropolis"
                                // configuration, even if they were only theoretical
                                if (conf.Chassis == ChassisType.PERQ1)
                                    keepMedia = true;
                                break;

                            case IOBoardType.EIO:
                            case IOBoardType.NIO:
                                if (conf.Chassis == ChassisType.PERQ2)
                                    keepMedia = true;
                                else
                                    conf.Drives[d.Unit].Device = DriveType.Disk5Inch;
                                break;
                        }
                        break;

                    case DriveType.Disk5Inch:
                        // 5.25" MFM drives only valid on EIO/NIO in the PERQ2T2-type
                        // chassis; remapped to 8" in PERQ2 or 14" on PERQ1 IOB/CIO.
                        switch (newType)
                        {
                            case IOBoardType.IOB:
                            case IOBoardType.CIO:
                                conf.Drives[d.Unit].Device = DriveType.Disk14Inch;
                                break;

                            case IOBoardType.EIO:
                            case IOBoardType.NIO:
                                if (conf.Chassis == ChassisType.PERQ2T2)
                                    keepMedia = true;
                                else
                                    conf.Drives[d.Unit].Device = DriveType.Disk8Inch;
                                break;
                        }
                        break;
                }

                if (!keepMedia)
                {
                    Console.WriteLine("--> media for unit {0} unloaded", d.Unit);
                    conf.Drives[d.Unit].MediaPath = string.Empty;
                }
            }
        }


        // Common memory sizes in bytes
        public const int HALF_MEG = 512 * 1024;
        public const int ONE_MEG = 1024 * 1024;
        public const int TWO_MEG = 1024 * 1024 * 2;
        public const int FOUR_MEG = 1024 * 1024 * 4;

        private Hashtable _prefabs;
        private Configuration _default;
        private Configuration _current;
    }
}
