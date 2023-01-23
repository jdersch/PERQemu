//
// Configuration.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Text;
using System.Collections.Generic;

using PERQmedia;

namespace PERQemu.Config
{
    public struct Drive
    {
        public Drive(int unit, DeviceType dev, string path = "")
        {
            Unit = unit;
            Type = dev;
            MediaPath = path;
        }

        public int Unit;
        public DeviceType Type;
        public string MediaPath;
    }

    /// <summary>
    /// Describes the configuration of a PERQ system, including its storage
    /// devices.  The Configurator builds a list of standard configurations or
    /// allows the user to construct custom ones, which should cover every
    /// option on The Chart as more devices are added to the emulation.
    /// </summary>
    public class Configuration
    {
        public Configuration()
        {
            _name = "default";
            _key = "default";
            _description = "Default configuration";
            _filename = "";
            _chassis = ChassisType.PERQ1;
            _cpuBoard = CPUType.PERQ1A;
            _ioBoard = IOBoardType.IOB;
            _ioOptionBoard = OptionBoardType.OIO;
            _ioOptions = IOOptionType.Link | IOOptionType.Ether;
            _memSize = Configurator.ONE_MEG;
            _displayType = DisplayType.Portrait;
            _tabletType = TabletType.BitPad;

            // Pre-assign a standard disk image supplied with PERQemu
            // That way there's something to actually run, by default
            _drives = new Drive[MAX_DRIVES];
            _drives[0] = new Drive(0, DeviceType.Floppy);
            _drives[1] = new Drive(1, DeviceType.Disk14Inch, Paths.BuildDiskPath("f1.prqm"));
            _drives[2] = new Drive(2, DeviceType.Unused);
            _drives[3] = new Drive(3, DeviceType.Unused);

            // Users must explicitly enable the RS-232 ports
            _rsaEnabled = false;
            _rsbEnabled = false;

            // The default
            _etherAddr = 0;

            _validated = true;
            _modified = false;
            _saved = false;
        }

        #region Getters and Setters

        /// <summary>
        /// A short (one word) unique name to identify the config.
        /// </summary>
        public string Name
        {
            get { return _name; }
            set
            {
                _name = value;
                _key = _name.Trim().ToLower();
            }
        }

        /// <summary>
        /// Generated from Name, index into Prefabs.
        /// </summary>
        public string Key
        {
            get { return _key; }
        }

        /// <summary>
        /// A fuller description of the configuration.
        /// </summary>
        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        /// <summary>
        /// The file a config was loaded from.  Generated by default as a join
        /// of ConfigDir + key + ".cfg" (the standard suffix for now).
        /// </summary>
        public string Filename
        {
            get { return _filename; }
            set { _filename = value; }
        }

        /// <summary>
        /// This flag is set by the Configurator's Validate() but its use is
        /// not "enforced" or guaranteed to be correct.  Set to true for a new
        /// default Configuration (which it better be!)
        /// </summary>
        public bool IsValid
        {
            get { return _validated; }
            set { _validated = value; }
        }

        /// <summary>
        /// Set by the Configurator when an fields have been changed and the
        /// config should be re-Validated.  Set to false when a configuration
        /// is saved or loaded (successfully).
        /// </summary>
        public bool IsModified
        {
            get { return _modified; }
            set { _modified = value; }
        }

        /// <summary>
        /// Flag to indicate that the Configuration is backed by a disk file
        /// (in the Conf/ dir, but not guaranteed).  Set to true when loaded
        /// or saved (successfully), but false for a new Configuration that
        /// has not been named by the user.
        /// </summary>
        public bool IsSaved
        {
            get { return _saved; }
            set { _saved = value; }
        }

        /// <summary>
        /// A string that returns additional status, error or informational
        /// messages after some operation on the Configuration.  Configuration
        /// does NOT write on the Console, so the Reason string can be used by
        /// either the CLI or GUI as appropriate.
        /// </summary>
        public string Reason
        {
            get { return _reason; }
            set { _reason = value; }
        }

        public ChassisType Chassis
        {
            get { return _chassis; }
            set { _chassis = value; }
        }

        public CPUType CPU
        {
            get { return _cpuBoard; }
            set { _cpuBoard = value; }
        }

        public int MemorySizeInBytes
        {
            get { return _memSize; }
            set { _memSize = value; }
        }

        public IOBoardType IOBoard
        {
            get { return _ioBoard; }
            set { _ioBoard = value; }
        }

        public bool RSAEnable
        {
            get { return _rsaEnabled; }
            set { _rsaEnabled = value; }
        }

        public bool RSBEnable
        {
            get { return _rsbEnabled; }
            set { _rsbEnabled = value; }
        }

        public OptionBoardType IOOptionBoard
        {
            get { return _ioOptionBoard; }
            set { _ioOptionBoard = value; }
        }

        public IOOptionType IOOptions
        {
            get { return _ioOptions; }
            set { _ioOptions = value; }
        }

        public ushort EtherAddress
        {
            get { return _etherAddr; }
            set { _etherAddr = value; }
        }

        public DisplayType Display
        {
            get { return _displayType; }
            set { _displayType = value; }
        }

        public TabletType Tablet
        {
            get { return _tabletType; }
            set { _tabletType = value; }
        }

        public Drive[] Drives
        {
            get { return _drives; }
            set { _drives = value; }
        }

        #endregion

        public string Summary()
        {
            var sb = new StringBuilder();

            sb.AppendLine("Configuration: " + Name);
            sb.AppendLine("Description:   " + Description);

            if (!string.IsNullOrEmpty(_filename))
            {
                sb.AppendLine("Filename:      " + Filename);
            }

            sb.AppendLine("--------------");
            sb.AppendLine("Machine type:  " + Chassis);
            sb.AppendLine("CPU type:      " + CPU);
            sb.AppendLine("Memory size:   " + MemSizeToString());
            sb.AppendLine("Display type:  " + Display);
            sb.AppendLine("Tablet type:   " + Tablet);
            sb.AppendLine("IO board:      " + IOBoard);

            if (_rsaEnabled)
            {
                sb.Append("    RS-232 A:  ");
                sb.AppendLine(Settings.RSADevice == string.Empty ? "<unassigned>" : Settings.RSADevice);
            }

            if ((IOBoard == IOBoardType.EIO || IOBoard == IOBoardType.NIO) && _rsbEnabled)
            {
                sb.Append("    RS-232 B:  ");
                sb.AppendLine(Settings.RSBDevice == string.Empty ? "<unassigned>" : Settings.RSBDevice);
            }

            if (IOBoard == IOBoardType.EIO && EtherAddress != 0)
            {
                sb.AppendLine("    Ethernet:  node " + EtherAddress);
            }

            if (IOOptionBoard != OptionBoardType.None)
            {
                sb.Append("Option board:  " + IOOptionBoard);

                if (IOOptions != IOOptionType.None)
                {
                    sb.Append("  Options:  " + IOOptions);
                }
                sb.AppendLine();

                if ((IOOptionBoard == OptionBoardType.OIO ||
                     IOOptionBoard == OptionBoardType.Ether3) &&
                     EtherAddress != 0)
                {
                    sb.AppendLine("    Ethernet:  node " + EtherAddress);
                }
            }

            // Todo: for PERQ-2 models, backplane serial number

            sb.AppendLine();
            sb.AppendLine("Storage configuration:");
            sb.AppendLine("----------------------");

            for (var unit = 0; unit < _drives.Length; unit++)
            {
                var drive = _drives[unit];

                if (drive.Type != DeviceType.Unused)
                {
                    var hack = sb.Length;
                    sb.AppendFormat("Unit: {0}  Type: {1}", unit, drive.Type);
                    sb.AppendFormat("{0}File: {1}\n", " ".PadLeft(hack + 28 - sb.Length),
                                    (string.IsNullOrEmpty(drive.MediaPath) ? "<unassigned>" : Paths.Canonicalize(drive.MediaPath)));
                }
            }

            return sb.ToString();
        }

        public override string ToString()
        {
            return Name;
        }

        public int MaxDrives => MAX_DRIVES;
        public int MaxUnitNum => _drives.Length - 1;

        public string MemSizeToString()
        {
            if (_memSize < Configurator.ONE_MEG)
            {
                return string.Format("{0}KB", _memSize / 1024);
            }

            return string.Format("{0}MB", _memSize / Configurator.ONE_MEG);
        }

        public void SetMediaPath(int unit, string file)
        {
            _drives[unit].MediaPath = file;
        }

        public void SetDeviceType(int unit, DeviceType dev)
        {
            _drives[unit].Type = dev;
        }

        public Drive[] GetDrivesOfType(DeviceType type)
        {
            // This may seem silly now, but I dream of future PERQ configurations
            // with two floppies, four internal disks, a string of external SMD
            // drives, a streamer, a partridge, AND a pear tree.

            // For now it's basically 0=floppy, 1,2=harddisk, 3=streamer.  Meh.
            var match = new List<Drive>();

            for (var d = 0; d < _drives.Length; d++)
            {
                if (_drives[d].Type == type)
                {
                    match.Add(_drives[d]);
                }
            }

            return match.ToArray();
        }

        // For now...
        public const byte MAX_DRIVES = 4;

        string _name;               // short name
        string _key;                // unique key for matching
        string _description;        // brief description
        string _filename;           // set on load or save

        ChassisType _chassis;
        CPUType _cpuBoard;
        int _memSize;
        IOBoardType _ioBoard;
        OptionBoardType _ioOptionBoard;
        IOOptionType _ioOptions;
        DisplayType _displayType;
        TabletType _tabletType;
        Drive[] _drives;

        ushort _etherAddr;

        bool _rsaEnabled;
        bool _rsbEnabled;

        string _reason;
        bool _validated;
        bool _modified;
        bool _saved;
    }


    public class UnimplementedHardwareException : Exception
    {
        public UnimplementedHardwareException(string message) : base(message)
        {
        }
    }

    public class InvalidConfigurationException : Exception
    {
        public InvalidConfigurationException(string message) : base(message)
        {
        }
    }
}