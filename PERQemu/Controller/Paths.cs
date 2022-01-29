//
// Paths.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
        /// Default directory for saving screenshots or printed output
        /// from the Canon or other simulated printers.
        /// </summary>
        public static string OutputDir
        {
            get { return Path.Combine(PERQemu.BaseDir, "Output"); }
        }

        /// <summary>
        /// Settings dir is the root of the user's home directory.
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

        public static string BuildPROMPath(string file)
        {
            return Path.Combine(PROMDir, file);
        }

        public static string BuildConfigPath(string file)
        {
            return Path.Combine(ConfigDir, file);
        }

        public static string BuildDiskPath(string file)
        {
            return Path.Combine(DiskDir, file);
        }

        /// <summary>
        /// Builds an output path using the current (possibly user-modified)
        /// output directory.
        /// </summary>
        public static string BuildOutputPath(string file)
        {
            return Path.Combine(Settings.OutputDirectory, file);
        }

        /// <summary>
        /// Try to make long crazy strings less long and crazy.
        /// </summary>
        public static string Canonicalize(string path)
        {
            // in .NET 5, so not on my old machine :-(
            // return Path.GetRelativePath(PERQemu.BaseDir, path);

            try
            {
                // Special case: expand the Unix '~' to the users's home dir
                if (PERQemu.HostIsUnix && path.Length > 0 && path[0] == '~')
                {
                    if (path == "~")
                    {
                        path = SettingsDir;
                    }
                    else if (path.StartsWith("~/"))
                    {
                        path = Path.Combine(SettingsDir, path.Substring(2));
                    }
                    else
                    {
                        #region Ah-oo-gah!
                        /*
                            Ugh. This is nasty. Handling ~user requires that we
                            have to avoid expansions of the sort
                            
                                  ~user[/foo] => /home/path/user/user[/foo]
                                  
                             The quick and dirty solution makes these assumption
                             that the home directories on your machine use a common
                             root path, typically something like
                                  /home/<user>
                             or   /Users/<user>
                             
                             and NOT a Windows-ish
                                  "c:\\something\<user>\documents and settings"
                             kind of deal.

                            The "worst case" is that we mangle the path and
                            Open() fails and the user retypes the name to not
                            include a ~.  This could all be yanked out if/when
                            the Mono runtime could deal with ~ internally...
                         */
                        #endregion

                        // For now let's just blindly assume this works:
                        var usersDir = SettingsDir.Substring(0, SettingsDir.LastIndexOf('/'));
                        path = Path.Combine(usersDir, path.Substring(1));
                    }
                }

                // Let the runtime expand it
                var full = Path.GetFullPath(path);

                // See if we can trim it down some
                if (full == PERQemu.BaseDir)
                {
                    return "";    // Base relative to base is, uh, right here
                }
                else if (full.StartsWith(PERQemu.BaseDir, PERQemu.HostIsUnix ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase))
                {
                    return full.Substring(PERQemu.BaseDir.Length + 1);
                }
                else
                {
                    // Outside our working dir? for now, return the path unmodified
                    return path;
                }
            }
            catch (Exception e)
            {
                Console.WriteLine("Could not make a relative path for '{0}': {1}", path, e.Message);
                return path;
            }
        }
    }
}

