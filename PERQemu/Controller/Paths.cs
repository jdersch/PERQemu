// paths.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.IO;

namespace PERQemu
{
    /// <summary>
    /// Helpers for locating files needed to run the emulator and default
    /// locations for configuration files, disk images, etc.
    /// </summary>
    public static class Paths
    {
        /// <summary>
        /// Directory inside the app wrapper containing PROM images.
        /// </summary>
        public static string PROMDir
        {
            get { return Path.Combine(PERQemu.BaseDir, "PROM"); }
        }

        /// <summary>
        /// Default directory for PERQ configuration files.
        /// </summary>
        public static string ConfigDir
        {
            get { return Path.Combine(PERQemu.BaseDir, "Conf"); }
        }

        /// <summary>
        /// Default directory for PERQ hard and floppy disk images.
        /// </summary>
        public static string DiskDir
        {
            get { return Path.Combine(PERQemu.BaseDir, "Disks"); }
        }

        /// <summary>
        /// Output directory is for saving screenshots or printed output, from
        /// the Canon or other simulated printers.
        /// </summary>
        public static string OutputDir
        {
            get { return Path.Combine(PERQemu.BaseDir, "Output"); }
        }

        /// <summary>
        /// Settings dir is the root of the user's home directory (same on
        /// Windows and Unix).
        /// </summary>
        public static string SettingsDir
        {
            get
            {
                return PERQemu.HostIsUnix
                       ? Environment.GetEnvironmentVariable("HOME")
                       : Environment.ExpandEnvironmentVariables("%HOMEDRIVE%%HOMEPATH%");
            }
        }

        /// <summary>
        /// Return the path to the default configuration file, appropriate to
        /// the platform.   We save settings in a common format and avoid any
        /// platform-specific mechanisms.
        /// </summary>
        public static string SettingsPath
        {
            get
            {
                return Path.Combine(SettingsDir, (PERQemu.HostIsUnix
                                                  ? ".PERQemu_cfg"
                                                  : "PERQemu.cfg"));
            }
        }

        /// <summary>
        /// Try to make long crazy strings less long and crazy.
        /// </summary>
        public static string MakeRelative(string path)
        {
            // in .NET 5, so not on my old machine :-(
            // return Path.GetRelativePath(PERQemu.BaseDir, path);

            try
            {
                var full = Path.GetFullPath(path);

                if (full.StartsWith(PERQemu.BaseDir, PERQemu.HostIsUnix ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase))
                {
                    return full.Substring(PERQemu.BaseDir.Length + 1);
                }
                else
                {
                    return path;
                }
            }
            catch
            {
                Console.WriteLine("Could not make a relative path for '{0}'", path);
                return path;    // ??? or ""?  or null?
            }
        }

        public static string BuildPROMPath(string file)
        {
            return Path.Combine(PROMDir, file);
        }

        public static string BuildDiskPath(string file)
        {
            return Path.Combine(DiskDir, file);
        }

        public static string BuildOutputPath(string file)
        {
            return Path.Combine(OutputDir, file);
        }
    }
}

