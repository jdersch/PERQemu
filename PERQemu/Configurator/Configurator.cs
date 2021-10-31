//
// Configurator.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
            //Log.Debug(Category.EmuState, "Configurator: initializing.");

            Configuration conf;
            StorageConfiguration[] drives;

            // For now, we just build up a set of the standard PERQ models.
            // This really ought to be read in from a file instead.  Ugly...

            //
            // PERQ-1 drive setup: one floppy, one Shugart.  Should be a 12MB drive;
            // for now we'll default to the POS F.0 image (but should find a D.6!)
            //
            drives = new StorageConfiguration[2];
            drives[0] = new StorageConfiguration(DriveType.Floppy, 0, "");
            drives[1] = new StorageConfiguration(DriveType.Disk14Inch, 1, Paths.BuildDiskPath("f0.phd"));

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
            drives = new StorageConfiguration[2];
            drives[0] = new StorageConfiguration(DriveType.Floppy, 0, "");
            drives[1] = new StorageConfiguration(DriveType.Disk8Inch, 1, "");

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
            drives = new StorageConfiguration[3];
            drives[0] = new StorageConfiguration(DriveType.Floppy, 0, "");
            drives[1] = new StorageConfiguration(DriveType.Disk8Inch, 1, "");
            drives[2] = new StorageConfiguration(DriveType.Disk8Inch, 2, "");

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
            drives[1] = new StorageConfiguration(DriveType.Disk5Inch, 1, "");
            drives[2] = new StorageConfiguration(DriveType.Disk5Inch, 1, "");

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
            get { return _default; }
        }

        public Configuration Current
        {
            get { return _current; }
            set { _current = value; }
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
            else if (_prefabs.ContainsKey(name.ToUpper())) {

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

                // Run the validator just to be sure; we might be loading a
                // hand-written file that might not be 100% kosher.
                Validate();

                // Let the GUI know we were successful, we think.
                return true;
            }
            catch (Exception e)
            {
                // Uh... this is ugly.  Not sure what to do here...
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
                    sw.WriteLine("memory " + _current.MemorySize / 1024);
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
                    // Write out the storage configuration.  We can only assume
                    // they haven't created something bogus.
                    //
                    foreach (StorageConfiguration disk in _current.Drives)
                    {
                        // This is the most likely to change?
                        sw.WriteLine("storage unit {0} type {1}{2}",  // can't do this yet :-(
                                     disk.Unit, disk.Device,
                                     (string.IsNullOrEmpty(disk.MediaPath) ? "" : " filename " + disk.MediaPath));
                    }

                    sw.WriteLine("done");
                    sw.Close();
                }

                _current.IsSaved = true;
                _current.Reason = string.Format("Configuration '{0}' saved to {1}.", _current.Name, _current.Filename);
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

            // If this is too slow, call out the tests one by one...
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

                    if (conf.MemorySize > TWO_MEG)
                    {
                        conf.Reason = "PERQ1 (4K) CPU supports a maximum of 2MB of memory.";
                        return false;
                    }
                    break;

                // 16K CPU in the PERQ-2 models didn't support the older 
                // quarter-meg or half-meg memory boards (nitpicky); max 2MB.
                case CPUType.PERQ1A:
                    if (conf.Chassis == ChassisType.PERQ2 && conf.MemorySize < HALF_MEG)
                    {
                        conf.Reason = "PERQ2: 16K CPU requires minimum of 512KB of memory.";
                        return false;
                    }
                    else if (conf.Chassis == ChassisType.PERQ2T2 && conf.MemorySize < ONE_MEG)
                    {
                        conf.Reason = "PERQ2-T2: 16K CPU requires minimum of 1MB of memory.";
                        return false;
                    }
                    else if (conf.MemorySize > TWO_MEG)
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
                        conf.Reason = "24-bit CPU requires PERQ-2/T4 type chassis.";
                        return false;
                    }
                    break;
            }

            // Must be okay then.
            return true;
        }

        /// <summary>
        /// Checks the selected memory size against constraints: 20-bit CPU can
        /// only support 2MB or less; 24-bit CPU only supported 4MB (officially).
        /// We allow up to 16MB (the "Universal Landscape Memory" board fully-
        /// populated with 1Mbit DRAMs) but it's unlikely any PERQ OS supported
        /// 8MB or 16MB... it'll be fun to find out someday!  Also checks the
        /// Display for a few special cases.
        /// </summary>
        public bool CheckMemory(Configuration conf)
        {
            if (conf.CPU == CPUType.PERQ1 || conf.CPU == CPUType.PERQ1A)
            {
                if (conf.MemorySize > TWO_MEG)
                {
                    conf.Reason = "20-bit CPU supports maximum of 2MB of memory.";
                    return false;
                }

            }
            else    // 24-bit CPU
            {
                if (conf.MemorySize < FOUR_MEG)
                {
                    conf.Reason = "24-bit CPU shipped with minimum of 4MB memory.";
                    return false;
                }
                else if (conf.MemorySize > FOUR_MEG)
                {
                    conf.Reason = "OS may not support more than 4MB of memory.";
                    // Just a warning
                }
            }

            // A weird special case
            if (conf.Chassis == ChassisType.PERQ1 &&
                conf.Display == DisplayType.Landscape &&
                conf.MemorySize != ONE_MEG)
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
        public bool CheckIO(Configuration conf)
        {
            if (((conf.IOBoard == IOBoardType.IOB || conf.IOBoard == IOBoardType.CIO) && conf.Chassis != ChassisType.PERQ1) ||
                ((conf.IOBoard == IOBoardType.NIO || conf.IOBoard == IOBoardType.EIO) && conf.Chassis == ChassisType.PERQ1))
            {
                conf.Reason = String.Format("IO Board type {0} not compatible with {1} chassis.", conf.IOBoard, conf.Chassis);
                return false;
            }

            // A few little sanity checks
            if (conf.Chassis == ChassisType.PERQ1 && conf.Tablet.HasFlag(TabletType.Kriz))
            {
                conf.Reason = "PERQ-1 with Kriz tablet is unusual, though supported...";
            }

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
        /// TODO: move this verbosity elsewhere
        /// PERQ-1 did not support the MLO board (which isn't implemented yet
        /// anyway) since it relied on the EIO's DMA hardware.  It's not clear
        /// if the 3Mbit Ethernet board could run in a PERQ-2, but I'm guessing
        /// it could not (backplane differences, though even in a PERQ-1 there
        /// were wire-wrapped motherboard mods to make it work).  It's entirely
        /// likely that CMU was the only place that had 3Mbit Ethernet boards!
        /// ---
        /// Due to microcode conflicts and DMA channel collisions, I don't think
        /// it's possible to run an OIO with Ethernet alongside an EIO; at best,
        /// it's ignored (no software support for two interfaces) and at worst it
        /// produces hardware conflicts (which may be what fucked up my T2? sigh).
        /// Note that someday I should add in support for the "universal" tape
        /// streamer and/or link board in the CPU Option slot.  That way a PERQ-1
        /// with a 3Mbit board, or a PERQ-2 with an MLO board could still use
        /// those options.  The PERQLink was also how they hooked up the Metheus
        /// color controller, so I'm guessing a T4+MLO could drive both the color
        /// display AND SMD disks.  In my fantasy world it can, fer dang sure.
        /// </remarks>
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

                    // Should reset this, rather than whine?
                    if (conf.IOOptions.HasFlag(IOOptionType.SMD))
                    {
                        conf.Reason = "OIO Board extraneous option selected, ignored.";
                    }

                    // Disallow EIO + OIO Ethernet... but allow NIO
                    if ((conf.IOBoard == IOBoardType.EIO) && conf.IOOptions.HasFlag(IOOptionType.Ether))
                    {
                        conf.Reason = string.Format("OIO Board Ethernet option conflicts with {0} board.", conf.IOBoard);
                        return false;
                    }
                    break;

                case OptionBoardType.MLO:
                    if (conf.Chassis != ChassisType.PERQ2T2)
                    {
                        conf.Reason = string.Format("Multibus/Laser option not supported in {0}.", conf.Chassis);
                        return false;
                    }
                    else
                    {
                        conf.Reason = "Sorry, the MLO board is not yet supported.";
                        conf.IOOptionBoard = OptionBoardType.None;
                        return false;
                    }
            }

            return true;
        }

        /// <summary>
        /// Checks that the list of disk devices is appropriate for the selected
        /// IO board.  For now, traditional configuration rules apply:
        ///     PERQ-1 only has 1 floppy, 1 Shugart 14" disk.
        ///     PERQ-2 only has 1 floppy, 1 or 2 Micropolis 8" disks.
        ///     PERQT2 only has 1 floppy, 1 or 2 5.25" disks.
        /// At the moment, SMD Disks (only in EIO+MLO T2 or T4 systems) are not
        /// supported.  Future enhancements include allowing a second floppy,
        /// up to four 5.25" drives, and up to four SMD drives.  Even the PERQ-1
        /// could be enhanced to allow for additional Shugart SA4x00 drives. :-)
        /// </summary>
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
                                if (drive.Device != DriveType.Disk14Inch)
                                {
                                    conf.Reason = string.Format("IO board type '{0}' does not support disks of type '{1}'.", conf.IOBoard, drive.Device);
                                    return false;
                                }
                                driveCount++;
                                if (driveCount > 1)
                                {
                                    conf.Reason = string.Format("IO board type '{0}' only supports one Shugart hard disk.", conf.IOBoard);
                                    return false;
                                }
                                break;

                            case IOBoardType.NIO:
                            case IOBoardType.EIO:
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

        // Common memory sizes
        public const int HALF_MEG = 512 * 1024;
        public const int ONE_MEG = 1024 * 1024;
        public const int TWO_MEG = 1024 * 1024 * 2;
        public const int FOUR_MEG = 1024 * 1024 * 4;

        private Hashtable _prefabs;
        private Configuration _default;
        private Configuration _current;

    }
}
