//
// Configuration.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
        public Drive(DeviceType dev, string path = "")
        {
            Type = dev;
            MediaPath = path;
        }

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
            _memSize = 1024 * 1024;
            _displayType = DisplayType.Portrait;
            _tabletType = TabletType.BitPad;

            _drives = new Drive[MAX_DRIVES];
            _drives[0] = new Drive(DeviceType.Floppy);
            _drives[1] = new Drive(DeviceType.Disk14Inch);
            _drives[2] = new Drive(DeviceType.Unused);
            _drives[3] = new Drive(DeviceType.Unused);

            _validated = true;
            _modified = false;      // these don't belong here!?
            _saved = true;
        }

        #region Getters and Setters

        public string Name
        {
            get { return _name; }
            set
            {
                _name = value;
                _key = _name.Trim().ToLower();
            }
        }

        public string Key
        {
            get { return _key; }
        }

        public string Description
        {
            get { return _description; }
            set { _description = value; }
        }

        public string Filename
        {
            get { return _filename; }
            set { _filename = value; }
        }

        public bool IsValid
        {
            get { return _validated; }
            set { _validated = value; }
        }

        public bool IsModified
        {
            get { return _modified; }
            set { _modified = value; }
        }

        public bool IsSaved
        {
            get { return _saved; }
            set { _saved = value; }
        }

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
            StringBuilder sb = new StringBuilder();

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

            if (IOOptionBoard != OptionBoardType.None)
            {
                sb.Append("Option board:  " + IOOptionBoard);

                if (IOOptions != IOOptionType.None)
                {
                    sb.Append("  Options: " + IOOptions);
                }
                sb.AppendLine();
            }

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
                                    (string.IsNullOrEmpty(drive.MediaPath) ? "<not assigned>" : Paths.Canonicalize(drive.MediaPath)));
                }
            }

            return sb.ToString();
        }

        public override string ToString()
        {
            return Name;
        }

        public string MemSizeToString()
        {
            if (_memSize < Configurator.ONE_MEG)
            {
                return string.Format("{0}KB", _memSize / 1024);
            }
            else
            {
                return string.Format("{0}MB", _memSize / Configurator.ONE_MEG);
            }
        }

        public void AssignMedia(string file, int unit = 0)
        {
            _drives[unit].MediaPath = file;
        }

        //public int GetDriveByUnit(int unit)
        //{
        //    return _drives.FindIndex(d => (d.Unit == unit));
        //}

        //public List<StorageDevice> GetDriveByType(DriveType t)
        //{
        //    return _drives.FindAll(d => (d.Device == t));
        //}

        // For now...
        public const byte MAX_DRIVES = 4;

        private string _name;               // short name
        private string _key;                // unique key for matching
        private string _description;        // brief description
        private string _filename;           // set on load or save

        private ChassisType _chassis;
        private CPUType _cpuBoard;
        private int _memSize;
        private IOBoardType _ioBoard;
        private OptionBoardType _ioOptionBoard;
        private IOOptionType _ioOptions;
        private DisplayType _displayType;
        private TabletType _tabletType;
        private Drive[] _drives;

        private string _reason;
        private bool _validated;
        private bool _modified;
        private bool _saved;
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
