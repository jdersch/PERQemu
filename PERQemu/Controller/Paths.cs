//
// Paths.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
        /// Directory inside the app wrapper containing icons or UI resources.
        /// </summary>
        public static string ResourceDir
        {
            get { return Path.Combine(PERQemu.BaseDir, "Resources"); }
        }

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

        public static string BuildResourcePath(string file)
        {
            return Path.Combine(ResourceDir, file);
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
        /// Try to qualify a file name by applying a few simple search rules:
        ///     1. Try 'file' as given
        ///     2. Try 'file' in 'dir'
        ///     3. Try 'file' with each of the 'extensions' in current, 'dir'
        /// Returns the canonicalized pathname upon the first successful match,
        /// otherwise an empty string.
        /// </summary>
        public static string FindFileInPath(string file, string dir, params string[] extensions)
        {
            if (File.Exists(file))
            {
                return Canonicalize(file);
            }

            if (File.Exists(Path.Combine(dir, file)))
            {
                return Canonicalize(Path.Combine(dir, file));
            }

            // Try the alternate extensions?
            if (extensions.Length > 0)
            {
                var root = file;

                // Remove the current one
                if (Path.HasExtension(file))
                {
                    root = Path.GetFileNameWithoutExtension(file);
                }

                foreach (var ext in extensions)
                {
                    var found = Path.ChangeExtension(root, ext);
                    if (File.Exists(found))
                    {
                        return Canonicalize(found);
                    }

                    found = Path.Combine(dir, found);
                    if (File.Exists(found))
                    {
                        return Canonicalize(found);
                    }
                }
            }

            // Special case: if the file isn't found after all the transformations
            // and we're on a Unix host, scan for the Windows directory separator.
            // If something like Disks\g7.prqm slipped through, replace with '/'
            // and try again...
            if (PERQemu.HostIsUnix && (file.Contains("\\") || dir.Contains("\\")))
            {
                return FindFileInPath(file.Replace('\\', '/'), dir.Replace('\\', '/'), extensions);
            }

            return string.Empty;
        }

        /// <summary>
        /// Return a (presumed) simple filename with a directory and extension.
        /// </summary>
        /// <remarks>
        /// Attempts to transform 'file' into the form 'dir/file.extension'.  If
        /// file already has a directory specification it is not changed; if the
        /// file does not have any extension the one given is appended, otherwise
        /// an existing extension is only replaced if replaceExt is true.  The
        /// return string is "canonicalized".
        /// </remarks>
        public static string QualifyPathname(string file, string dir, string extension, bool replaceExt)
        {
            var path = Path.GetDirectoryName(file);

            if (path == string.Empty)
            {
                path = Path.Combine(dir, file);
            }
            else
            {
                path = file;    // Gotta go with what they gave us
            }

            if ((Path.HasExtension(path) && replaceExt) || !Path.HasExtension(path))
            {
                path = Path.ChangeExtension(path, extension);
            }

            return Canonicalize(path);
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
                    else if (path.StartsWith("~/", StringComparison.InvariantCulture))
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

                if (full.StartsWith(PERQemu.BaseDir, PERQemu.HostIsUnix ? StringComparison.Ordinal : StringComparison.OrdinalIgnoreCase))
                {
                    return full.Substring(PERQemu.BaseDir.Length + 1);
                }

                // Outside our working dir? for now, return the path unmodified
                return path;
            }
            catch (Exception e)
            {
                Console.WriteLine("Could not make a relative path for '{0}': {1}", path, e.Message);
                return path;
            }
        }
    }
}

