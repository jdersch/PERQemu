//
// Configurator.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;

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
            _current = _default;

            _quietly = !PERQemu.Initialized;

            _prefabs = new Hashtable();
            _geometries = new Hashtable();
            _driveSpecs = new Hashtable();
            _knownDrives = new Hashtable();
        }

        /// <summary>
        /// Load the pre-defined storage devices and machine specifications
        /// from the config files.  Only the basic PERQ-1 model is built-in
        /// (as a fallback); other data is loaded at runtime.
        /// </summary>
        public void Initialize()
        {
            // Set up our built-in device data
            AddGeometry("NoMedia", DeviceGeometry.NoMedia);
            AddGeometry("SSSD", DeviceGeometry.SSSD);
            AddGeometry("DSSD", DeviceGeometry.DSSD);
            AddGeometry("SSDD", DeviceGeometry.SSDD);
            AddGeometry("DSDD", DeviceGeometry.DSDD);
            AddGeometry("QIC20", DeviceGeometry.QIC20);
            AddGeometry("Shugart12", DeviceGeometry.Shugart12);
            AddGeometry("Shugart24", DeviceGeometry.Shugart24);

            AddDriveSpecs("SA851", DevicePerformance.SA851);
            AddDriveSpecs("SA4000", DevicePerformance.SA4000);
            AddDriveSpecs("Archive30IPS", DevicePerformance.Archive30IPS);

            try
            {
                // Read in the database of known devices
                PERQemu.CLI.ReadScript(Paths.BuildConfigPath("StorageDevices.db"));
            }
            catch (Exception e)
            {
                Log.Error(Category.MediaLoader, "** StorageDevices.db is missing or corrupted:");
                Log.Error(Category.MediaLoader, "** " + e.Message);
                Log.Error(Category.MediaLoader, "\nPlease restore this file from the PERQemu distribution.");
                Log.Error(Category.MediaLoader, "Will attempt to continue with limited functionality.");
            }

            // Add the default machine setup
            AddPrefab(Default);

            // Go look for more
            LoadPrefabs();
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

        public bool Quietly => _quietly;

        public void AddGeometry(string key, DeviceGeometry geom)
        {
            Log.Detail(Category.MediaLoader, "Adding drive geometry '{0}'", key);

            _geometries.Add(key.ToLower(), geom);
        }

        public DeviceGeometry GetGeometry(string key)
        {
            return (DeviceGeometry)_geometries[key.ToLower()];
        }

        public void AddDriveSpecs(string key, DevicePerformance perf)
        {
            Log.Detail(Category.MediaLoader, "Adding drive specs '{0}'", key);

            _driveSpecs.Add(key.ToLower(), perf);
        }

        public DevicePerformance GetDriveSpecs(string key)
        {
            return (DevicePerformance)_driveSpecs[key.ToLower()];
        }

        public void AddKnownDrive(StorageDevice dev)
        {
            Log.Detail(Category.MediaLoader, "Adding device definition '{0}', type {1}",
                                            dev.Info.Name, dev.Info.Type);

            _knownDrives.Add(dev.Info.Name.ToLower(), dev);

            // Update the "DriveTypes" keyword nodes!
            PERQemu.CLI.UpdateKeywordMatchHelpers("DriveTypes", GetKnownDevices());
        }

        public StorageDevice GetKnownDeviceByName(string key)
        {
            return (StorageDevice)_knownDrives[key];
        }

        public string[] GetKnownDevices()
        {
            var a = new string[_knownDrives.Keys.Count];
            _knownDrives.Keys.CopyTo(a, 0);
            return a;
        }

        /// <summary>
        /// Add to the list of predefined configurations.
        /// </summary>
        public bool AddPrefab(Configuration conf)
        {
            if (!_prefabs.ContainsKey(conf.Key))
            {
                Log.Detail(Category.MediaLoader, "Adding machine config '{0}'", conf.Key);
                _prefabs.Add(conf.Key, conf);
            }

            // Now go update the "Configs" keyword nodes
            PERQemu.CLI.UpdateKeywordMatchHelpers("Configs", GetPrefabs());
            return true;
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
        /// Look up a pre-defined Configuration by name.  The name "current"
        /// is reserved and returns the Current configuration.  This ought to
        /// be clarified or documented somewhere.  I dunno man, I'm just making
        /// this up as I go.
        /// </summary>
        public Configuration GetConfigByName(string name)
        {
            name = name.ToLower();

            if (name == "current")
            {
                return _current;
            }

            if (_prefabs.ContainsKey(name))
            {
                return (Configuration)_prefabs[name];
            }

            return null;
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

                // We loaded successfully; set the IsSaved flag to indicate
                // that this config is backed by a disk file, and save the
                // actual filename we loaded it from so we can re-save it in
                // place later if the user makes changes.
                _current.Filename = path;
                _current.IsSaved = true;

                // Reset the modified flag since we're loading afresh
                _current.IsModified = false;

                // Run the validator just to be sure; we might be loading a
                // hand-written file that might not be 100% kosher.
                return Validate();
            }
            catch (Exception e)
            {
                // If called by the CLI, error messages should appear on the
                // console; the GUI can choose to restore the previous config
                // or set flags appropriately?
                _current.Reason = $"Could not load file '{path}': {e.Message}";
                return false;
            }
        }

        /// <summary>
        /// Scan the Conf dir for saved system configs and preload them into
        /// the Prefabs list.  Sure.  Why not.
        /// </summary>
        void LoadPrefabs()
        {
            // Let's be discreet, shall we?
            _quietly = true;

            Log.Debug(Category.MediaLoader, "Loading configurations from '{0}'",
                      Paths.Canonicalize(Paths.ConfigDir));

            foreach (var file in Directory.EnumerateFiles(Paths.ConfigDir, "*.cfg"))
            {
                _current = new Configuration();

                if (Load(Paths.Canonicalize(file)))
                {
                    AddPrefab(_current);
                    Log.Detail(Category.MediaLoader, "Added configuration '{0}'", _current.Name);
                }
                else
                {
                    Log.Info(Category.MediaLoader, _current.Reason);
                }
            }

            // Reset for normal CLI interactions
            _quietly = false;
            _current = Default;
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
                    sw.WriteLine("# " + PERQemu.Version);
                    sw.WriteLine("configure");
                    sw.WriteLine("default");
                    sw.WriteLine("name " + _current.Name);
                    sw.WriteLine("description \"" + _current.Description + "\"");
                    sw.WriteLine("chassis " + _current.Chassis);
                    sw.WriteLine("cpu " + _current.CPU);
                    sw.WriteLine("memory " + _current.MemorySizeInBytes / 1024);
                    sw.WriteLine("io board " + _current.IOBoard);
                    sw.WriteLine("display " + _current.Display);
                    sw.WriteLine("tablet " + _current.Tablet);

                    // Save serial ports, if enabled
                    if (_current.RSAEnable) sw.WriteLine("enable rs232 a");
                    if (_current.RSBEnable) sw.WriteLine("enable rs232 b");

                    sw.WriteLine("option board " + _current.IOOptionBoard);

                    // Enumerate the IO options, if any
                    if (_current.IOOptions != IOOptionType.None)
                    {
                        foreach (IOOptionType opt in Enum.GetValues(typeof(IOOptionType)))
                        {
                            if (_current.IOOptions.HasFlag(opt))
                                sw.WriteLine("option add " + opt);
                        }
                    }

                    // If an Ethernet address has been set, save it
                    if (_current.EtherAddress != 0)
                    {
                        sw.WriteLine("ethernet address " + _current.EtherAddress);
                    }

                    // Todo: For PERQ-2, save the configured serial number if set

                    // Write out the storage configuration
                    for (var unit = 0; unit < _current.Drives.Length; unit++)
                    {
                        var dev = _current.Drives[unit];

                        sw.WriteLine("drive {0} {1} {2}", unit, dev.Type, dev.MediaPath);
                    }

                    sw.WriteLine("done");
                    sw.Close();
                }

                // Set flags and a nice result message
                _current.IsSaved = true;
                _current.IsModified = false;
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

            conf.IsValid = (CheckCPU(conf) &&
                            CheckMemory(conf) &&
                            CheckIO(conf) &&
                            CheckOptions(conf) &&
                            CheckStorage(conf));

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
                    if (conf.Chassis == ChassisType.PERQ2Tx)
                    {
                        conf.Reason = "PERQ1 (4K) CPU not compatible with PERQ2-Tx models.";
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

                    if (conf.Chassis == ChassisType.PERQ2Tx && conf.MemorySizeInBytes < ONE_MEG)
                    {
                        conf.Reason = "PERQ2-Tx: 16K CPU requires minimum of 1MB of memory.";
                        return false;
                    }

                    if (conf.MemorySizeInBytes > TWO_MEG)
                    {
                        conf.Reason = "PERQ1A (16K) CPU supports a maximum of 2MB of memory.";
                        return false;
                    }
                    break;

                // 24-bit, 16K CPU only supported in the PERQ-2/T2 chassis,
                // which coupled with a 24-bit EIO made it a "T4".
                case CPUType.PERQ24:
                    if (conf.Chassis != ChassisType.PERQ2Tx)
                    {
                        conf.Reason = "24-bit CPU requires the PERQ2-Tx type chassis.";
                        return false;
                    }
                    break;

                case CPUType.PERQ24A:
                    conf.Reason = "Sadly, the 24-bit 64K CPU doesn't exist.";
                    return false;
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
                if (conf.MemorySizeInBytes > FOUR_MEG)
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
                conf.Reason = "The rare PERQ-1 with Landscape display only had 1MB of memory.";
                // But I'll allow it. :-)
            }
            return true;
        }

        public bool HasEthernet(Configuration conf)
        {
            return (conf.IOBoard == IOBoardType.EIO ||
                    conf.IOOptionBoard == OptionBoardType.Ether3 ||
                    (conf.IOOptionBoard == OptionBoardType.OIO &&
                     conf.IOOptions.HasFlag(IOOptionType.Ether)));
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
                conf.Reason = $"IO Board type {conf.IOBoard} not compatible with {conf.Chassis} chassis.";
                return false;
            }

            // Make sure the serial ports exist; IOB/CIO only has one
            if ((conf.IOBoard == IOBoardType.IOB || conf.IOBoard == IOBoardType.CIO) && conf.RSBEnable)
            {
                conf.RSBEnable = false;
            }

            // A few little sanity checks
            if (conf.Chassis == ChassisType.PERQ1 && conf.Tablet.HasFlag(TabletType.Kriz))
            {
                conf.Reason = "PERQ-1 with Kriz tablet is unusual, though supported.";
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
        ///     - MLO (when implemented) will be a PERQ-2 option only.
        ///     - Both Link and Streamer options are "universal" (but aren't
        ///       fully implemented yet).
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
                        conf.Reason = $"Bug? IO Option {conf.IOOptionBoard} but extra IO Options selected: {conf.IOOptions}";
                        conf.IOOptions = IOOptionType.None;         // force it
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
                        conf.Reason = $"OIO Board Ethernet option conflicts with {conf.IOBoard} board.";
                        return false;
                    }
                    break;

                case OptionBoardType.MLO:
                    if (conf.Chassis == ChassisType.PERQ1)
                    {
                        conf.Reason = $"Multibus/Laser option not supported in {conf.Chassis}.";
                        return false;
                    }

                    // Someday.  Will have to check other option combos.
                    conf.Reason = "Sorry, the MLO board is not yet supported.";
                    conf.IOOptionBoard = OptionBoardType.None;
                    return false;
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
        ///     PERQ-2 only has 1 floppy, 1 Micropolis 8" disks.
        ///     PERQTx only has 1 floppy, 1 or 2 Micropolis 8" or 5.25" disks.
        /// With the addition of the QIC Tape streamer (OIO only, currently) any
        /// configuration may attach a single drive as unit 3 iff the controller
        /// is available.  (Rather than throw an error, if OIO or MLO is selected
        /// and the Tape option is missing, we just add it.  If a different type
        /// of Option board is present, that is an error; the user should choose
        /// to remove the drive or fix the option board selection.)
        /// 
        /// At the moment, SMD Disks are not supported (requires the as-yet-
        /// unimplemented MLO option board).  We now have photographic proof of
        /// an ICL-labeled "CIO" that in theory supports a Micropolis 8" drive
        /// in a PERQ-1 chassis!  We just have to obtain/create a disk image in
        /// order to test it...
        /// </remarks>
        public bool CheckStorage()
        {
            return CheckStorage(_current);
        }

        public bool CheckStorage(Configuration conf)
        {
            bool gotFloppy = false;
            bool gotHard = false;
            int driveCount = 0;

            for (var unit = 0; unit < conf.Drives.Length; unit++)
            {
                var drive = conf.Drives[unit];

                switch (drive.Type)
                {
                    case DeviceType.Unused:
                        // Empty, skip it
                        break;

                    case DeviceType.Floppy:
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

                    case DeviceType.Disk14Inch:
                        if ((conf.IOBoard == IOBoardType.EIO) || (conf.IOBoard == IOBoardType.NIO))
                        {
                            conf.Reason = $"IO board type '{conf.IOBoard}' does not support disks of type '{drive.Type}'.";
                            return false;
                        }

                        driveCount++;
                        if (driveCount > 1)
                        {
                            conf.Reason = $"IO board type '{conf.IOBoard}' only supports one hard disk.";
                            return false;
                        }

                        gotHard |= (drive.MediaPath != string.Empty);
                        break;

                    case DeviceType.Disk8Inch:
                    case DeviceType.Disk5Inch:
                        if ((conf.IOBoard == IOBoardType.IOB) ||
                            (conf.IOBoard == IOBoardType.CIO && drive.Type == DeviceType.Disk5Inch))
                        {
                            conf.Reason = $"IO board type '{conf.IOBoard}' does not support disks of type '{drive.Type}'.";
                            return false;
                        }

                        driveCount++;
                        if (driveCount > 1)
                        {
                            if (conf.IOBoard == IOBoardType.CIO)
                            {
                                conf.Reason = $"IO board type '{conf.IOBoard}' only supports one hard disk.";
                                return false;
                            }

                            if (conf.Chassis == ChassisType.PERQ2)
                            {
                                conf.Reason = $"The {conf.Chassis} chassis only supports one hard disk.";
                                return false;
                            }
                        }

                        if (driveCount > 2)
                        {
                            conf.Reason = $"IO board type '{conf.IOBoard}' supports a maximum of 2 hard disks.";
                            return false;
                        }

                        gotHard |= (drive.MediaPath != string.Empty);
                        break;

                    case DeviceType.TapeQIC:
                        if (conf.IOOptionBoard != OptionBoardType.OIO &&
                            conf.IOOptionBoard != OptionBoardType.MLO)
                        {
                            conf.Reason = $"IO Option board type '{conf.IOOptionBoard} doesn't support QIC Tape.";
                            return false;
                        }

                        // Have a drive slot defined but no controller?  Add it...
                        // This probably shouldn't come up, but this whole thing
                        // is a little bit janky.  Hmm.
                        if (!conf.IOOptions.HasFlag(IOOptionType.Tape))
                        {
                            conf.Reason = "QIC Tape unit defined but controller not configured (adding it).";
                            conf.IOOptions |= IOOptionType.Tape;
                        }
                        break;

                    //
                    // Not yet implemented
                    //
                    case DeviceType.DiskSMD:
                    case DeviceType.Tape9Track:
                        conf.Reason = $"Sorry, {drive.Type} devices are not yet supported.";
                        return false;
                }
            }

            // Currently the hard disk controller can't cope if there's no media loaded;
            // not sure how realistic it is to run the actual hardware with no disk attached...
            if (!gotHard)
            {
                conf.Reason = "The PERQ won't boot without a hard disk present.";
                return _quietly;    // Cheeeeeeese
            }

            // Must be good then...
            return true;
        }

        public void UpdateStorage(IOBoardType newType)
        {
            UpdateStorage(_current, newType);
        }

        /// <summary>
        /// If the user configures a new IO board or switches chassis types, try
        /// to map over any already defined drives so they don't have to specify
        /// them again.  Types are remapped for the kind of controller present
        /// on the new board, and media paths removed if incompatible.  This is
        /// kind of a giant hack, one case where a GUI actually makes a LOT more
        /// sense.  Sigh.
        /// </summary>
        public void UpdateStorage(Configuration conf, IOBoardType newType)
        {
            conf.Reason = string.Empty;

            for (var unit = 0; unit < conf.Drives.Length; unit++)
            {
                bool keepMedia = false;
                var d = conf.Drives[unit];

                switch (d.Type)
                {
                    case DeviceType.Floppy:
                    case DeviceType.Unused:
                        // These are compatible with all IO board types
                        keepMedia = true;
                        break;

                    case DeviceType.DiskSMD:
                    case DeviceType.Tape9Track:
                    case DeviceType.TapeQIC:
                        // Not relevant, since these are attached to option boards!
                        keepMedia = true;
                        break;

                    case DeviceType.Disk14Inch:
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
                                    conf.SetDeviceType(unit, DeviceType.Disk8Inch);
                                else if (conf.Chassis == ChassisType.PERQ2Tx)
                                    conf.SetDeviceType(unit, DeviceType.Disk5Inch);
                                break;
                        }
                        break;

                    case DeviceType.Disk8Inch:
                        // Micropolis 8" drives are valid in PERQ1 on CIO (rare
                        // but allowed) or PERQ2 on EIO/NIO.  Change to Shugart 14"
                        // if switching to IOB, or MFM 5.25" for EIO/NIO (PERQ2Tx)
                        switch (newType)
                        {
                            case IOBoardType.IOB:
                                // Nope
                                conf.SetDeviceType(unit, DeviceType.Disk14Inch);
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
                                    conf.SetDeviceType(unit, DeviceType.Disk5Inch);
                                break;
                        }
                        break;

                    case DeviceType.Disk5Inch:
                        // 5.25" MFM drives only valid on EIO/NIO in the PERQ2Tx-type
                        // chassis; remapped to 8" in PERQ2 or 14" on PERQ1 IOB/CIO.
                        switch (newType)
                        {
                            case IOBoardType.IOB:
                            case IOBoardType.CIO:
                                conf.SetDeviceType(unit, DeviceType.Disk14Inch);
                                break;

                            case IOBoardType.EIO:
                            case IOBoardType.NIO:
                                if (conf.Chassis == ChassisType.PERQ2Tx)
                                    keepMedia = true;
                                else
                                    conf.SetDeviceType(unit, DeviceType.Disk8Inch);
                                break;
                        }
                        break;
                }

                if (keepMedia && !string.IsNullOrEmpty(d.MediaPath))
                {
                    keepMedia = AssignMediaTo(unit, d.MediaPath);
                }

                if (!keepMedia)
                {
                    conf.Reason = $"Incompatible media for unit {unit} unloaded";
                    conf.SetMediaPath(unit, string.Empty);
                }
            }
        }

        /// <summary>
        /// Validate a media file and assign it to the appropriate device.
        /// </summary>
        public bool AssignMedia(string file)
        {
            return AssignMedia(_current, file);
        }

        public bool AssignMedia(Configuration conf, string file)
        {
            conf.Reason = string.Empty;

            var found = Paths.FindFileInPath(file, Paths.DiskDir, FileUtilities.KnownExtensions);

            if (string.IsNullOrEmpty(found))
            {
                conf.Reason = $"Could not find file '{file}'.";
                return false;
            }

            var type = FileUtilities.GetDeviceTypeFromFile(found);

            // Did it contain an actual recognizable image?
            if (type == DeviceType.Unused)
            {
                conf.Reason = $"File '{found}' did not contain loadable media type.";
                return false;
            }

            // Find the slot(s), if any, that match the device type
            var units = conf.GetDrivesOfType(type);

            if (units.Length == 0)
            {
                conf.Reason = $"Could not assign file '{found}':\n" +
                              $"No drives of type {type} in this configuration.";
                return false;
            }

            // Assign it to the first/only slot!
            conf.SetMediaPath(units[0].Unit, found);
            conf.Reason = $"Assigned file '{found}' to drive {units[0].Unit}.";
            conf.IsModified = true;
            return true;
        }

        /// <summary>
        /// Validate a media file and assign it to the appropriate device.
        /// </summary>
        public bool AssignMediaTo(int unit, string file)
        {
            return AssignMediaTo(_current, unit, file);
        }

        public bool AssignMediaTo(Configuration conf, int unit, string file)
        {
            conf.Reason = string.Empty;

            if (unit < 0 || unit > conf.MaxUnitNum)
            {
                conf.Reason = $"Drive number out of range 0..{conf.MaxUnitNum}.";
                return false;
            }

            var found = Paths.FindFileInPath(file, Paths.DiskDir, FileUtilities.KnownExtensions);

            if (string.IsNullOrEmpty(found))
            {
                conf.Reason = $"Could not find file '{file}'.";
                return false;
            }

            var type = FileUtilities.GetDeviceTypeFromFile(found);

            // Did it contain an actual recognizable image?
            if (type == DeviceType.Unused)
            {
                conf.Reason = $"File '{found}' did not contain loadable media type.";
                return false;
            }

            // Annnnd did it match the requested unit?
            if (type != PERQemu.Config.Current.Drives[unit].Type)
            {
                conf.Reason = $"Could not assign file '{found}' to drive {unit}:\n" +
                              $"Media type {type} not compatible with drive type {conf.Drives[unit].Type}.";
                return false;
            }

            // Phew!  Assign it!
            conf.SetMediaPath(unit, found);
            conf.Reason = $"Assigned file '{found}' to drive {unit}.";
            return true;
        }

        // Common memory sizes in bytes
        public const int HALF_MEG = 512 * 1024;
        public const int ONE_MEG = 1024 * 1024;
        public const int TWO_MEG = 1024 * 1024 * 2;
        public const int FOUR_MEG = 1024 * 1024 * 4;
        public const int MAX_MEMORY = 1024 * 1024 * 8;

        // For preloading, discreetly
        bool _quietly;

        Hashtable _prefabs;
        Hashtable _geometries;
        Hashtable _driveSpecs;
        Hashtable _knownDrives;

        Configuration _default;
        Configuration _current;
    }
}
