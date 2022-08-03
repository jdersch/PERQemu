//
// ConfigCommands.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;

using PERQemu.Config;

namespace PERQemu.UI
{
    /// <summary>
    /// Command-line interface to the Configurator.  There's no local state
    /// here, so CLI commands operate directly on the Current configuration.
    /// </summary>
    /// <remarks>
    /// If loading from a file the Configurator sets the "Quietly" flag to skip
    /// console output and validation checks so it can preload configurations
    /// at startup without potentially spewing spurious errors that might be
    /// confusing.  This is something of a hack, but it means we don't bother
    /// with a fancy file parser or separate format for config files (though it
    /// does mean our CLI grammar must be stable).
    /// </remarks>
    public class ConfigCommands
    {
        /// <summary>
        /// The GUI configurator runs in a modal dialog, so the user can't be
        /// frobbing the settings while the machine is running.  For the CLI
        /// we run a quick check so that methods that change things that could
        /// mess with a running instance are prohibited.  Returns true if the
        /// PERQ is off, false (with an error message) if it's running.
        /// </summary>
        private bool OKtoReconfig()
        {
            if (PERQemu.Controller.State > RunState.Off)
            {
                Console.WriteLine("Cannot reconfigure while the PERQ is running; please power down and try again.");
                return false;
            }

            return true;
        }

        [Command("configure", "Enter the configuration subsystem", Prefix = true)]
        public void SetConfigPrefix()
        {
            PERQemu.CLI.SetPrefix("configure");
        }

        [Command("configure commands", "Show configuration commands")]
        public void ShowConfigCommands()
        {
            PERQemu.CLI.ShowCommands("configure");
        }

        [Command("configure done", "Exit configuration mode, return to top-level")]
        public void ConfigDone()
        {
            PERQemu.CLI.ResetPrefix();

            // Overload: if loading from a file, caller will validate
            if (PERQemu.Config.Quietly)
            {
                return;
            }

            // Otherwise (CLI) check it and return to top-level
            if (!PERQemu.Config.Validate())
            {
                Console.WriteLine("This configuration is invalid!  Please correct the following error:");
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
            else if (PERQemu.Config.Current.IsModified && !PERQemu.Config.Current.IsSaved)
            {
                Console.WriteLine("Note: the configuration has been modified but not yet saved.");
                Console.WriteLine("Use the 'configure save' command to save your changes.");
            }
        }

        [Command("configure default", "Reset the machine to the default configuration")]
        public void SetDefault()
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current = PERQemu.Config.Default;
                return;
            }

            if (OKtoReconfig())
            {
                Console.WriteLine("Setting the machine to defaults.");
                PERQemu.Config.Current = PERQemu.Config.Default;
            }
        }

        [Command("configure list", "List available machine configurations")]
        public void ListPrefabs()
        {
            string[] prefabs = PERQemu.Config.GetPrefabs();
            Array.Sort(prefabs);

            Console.WriteLine("Standard configurations:");
            foreach (var key in prefabs)
            {
                Configuration perq = PERQemu.Config.GetConfigByName(key);
                Console.WriteLine("    {0} - {1}", perq.Name.PadLeft(10), perq.Description);
            }

            Console.WriteLine("Current configuration:");
            Console.WriteLine("    {0} - {1}",
                              PERQemu.Config.Current.Name.PadLeft(10),
                              PERQemu.Config.Current.Description);
        }

        [Command("configure show", "Show current configuration details")]
        public void ShowConfiguration()
        {
            ShowConfiguration("current");
        }

        [Command("configure show", "Show a pre-defined machine configuration")]
        public void ShowConfiguration([KeywordMatch("Configs")] string name)
        {
            Configuration conf = PERQemu.Config.GetConfigByName(name);

            if (conf == null)
            {
                Console.WriteLine($"No configuration matching '{name}'.");
                Console.WriteLine("Use the configuration 'list' command to see available configurations.");
            }
            else
            {
                Console.WriteLine(conf.Summary());
            }
        }

        [Command("configure check", "Check that the configuration is valid")]
        public void CheckConfig()
        {
            if (!PERQemu.Config.Validate())
            {
                Console.WriteLine("Configuration is not valid:");
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
            else
            {
                if (PERQemu.Config.Current.Reason != string.Empty)
                {
                    Console.WriteLine("Configuration is valid, with warnings:");
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
                else
                {
                    Console.WriteLine("This configuration is valid.");
                }
            }
        }

        /// <summary>
        /// Load a new configuration.  Tries the prefabs list first, then falls
        /// back to search the directory.
        /// </summary>
        [Command("configure load", "Load a saved configuration")]
        public void LoadConfig([KeywordMatch("Configs")] string name)
        {
            if (OKtoReconfig())
            {
                var key = name.Trim().ToLower();

                // Special case: if typed "configure load default" and
                // there's no file named "default"... do the implied thing?
                if (key == "default")
                {
                    SetDefault();
                    return;
                }

                // Try the prefabs first
                var prefab = PERQemu.Config.GetConfigByName(key);

                if (prefab != null)
                {
                    PERQemu.Config.Current = prefab;
                    PERQemu.Config.Changed = false;
                    Console.WriteLine($"Configuration '{PERQemu.Config.Current.Name}' selected.");
                    return;
                }

                // Fall back to the filesystem
                var path = Paths.QualifyPathname(key, Paths.ConfigDir, ".cfg", true);

                if (PERQemu.Config.Load(path))
                {
                    Console.WriteLine($"Configuration '{PERQemu.Config.Current.Name}' loaded.");
                }
                else
                {
                    Console.WriteLine($"Couldn't find a configuration named '{name}'.");
                }
            }
        }

        [Command("configure save", "Save the current configuration")]
        public void SaveConfig()
        {
            // This is a subtle manipulation to discourage using "default" as
            // a filename - the default config leaves Filename blank to make
            // the user set a new name when making changes.
            if (string.IsNullOrEmpty(PERQemu.Config.Current.Filename))
            {
                Console.WriteLine("No filename set!  Use 'configure name <file>' to set a name for the");
                Console.WriteLine("configuration, then try again.");
            }

            if (!PERQemu.Config.Save())
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
            else
            {
                // Save it to our prefabs too, so 'list' will included it
                PERQemu.Config.AddPrefab(PERQemu.Config.Current);
                PERQemu.Config.Changed = false;
                Console.WriteLine($"Configuration saved to '{PERQemu.Config.Current.Filename}'.");
            }
        }

        [Command("configure name", "Name the current configuration")]
        public void SetName(string name)
        {
            var key = name.Trim().ToLower();

            if (PERQemu.Config.GetConfigByName(key) != null)
            {
                Console.WriteLine("There's already a configuration by that name; please choose another.");
                return;
            }

            // Qualify the file name: lower case, trimmed, Conf/, add .cfg!
            PERQemu.Config.Current.Filename = Paths.QualifyPathname(name, Paths.ConfigDir, ".cfg", true);
            PERQemu.Config.Current.Name = name;
        }

        [Command("configure description", "Add a brief description of the current configuration")]
        public void SetDescription(string desc)
        {
            PERQemu.Config.Current.Description = desc;
            PERQemu.Config.Changed = true;
        }

        [Command("configure chassis", "Set the machine type")]
        public void SetChassis(ChassisType perq)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.Chassis = perq;
                return;
            }

            if (OKtoReconfig())
            {
                if (perq != PERQemu.Config.Current.Chassis)
                {
                    PERQemu.Config.Current.Chassis = perq;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine("{0} chassis selected.", perq);

                    // Check the storage configuration here too...
                    PERQemu.Config.UpdateStorage(PERQemu.Config.Current.IOBoard);
                    if (!string.IsNullOrEmpty(PERQemu.Config.Current.Reason))
                        Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        [Command("configure cpu", "Set the CPU type")]
        public void SetCPU(CPUType cpu)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.CPU = cpu;
                return;
            }

            if (OKtoReconfig())
            {
                if (cpu != PERQemu.Config.Current.CPU)
                {
                    PERQemu.Config.Current.CPU = cpu;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine($"{cpu} CPU selected.");
                }

                if (!PERQemu.Config.CheckCPU())
                {
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        private uint RoundToPowerOf2(uint n)
        {
            n--;
            n |= n >> 1;
            n |= n >> 2;
            n |= n >> 4;
            n |= n >> 8;
            n |= n >> 16;
            n++;
            return n;
        }

        [Command("configure memory")]
        public void SetMemory()
        {
            Console.WriteLine("Configure the memory capacity, from 256KB to 8MB.  The CPU type determines\n" +
                              "the maximum memory capacity, which must be a power of two:\n" +
                              "\t20-bit CPU: 256, 512, 1024 or 2048 (KB)\tor 1, 2 (MB)\n" +
                              "\t24-bit CPU: 2048, 4096 or 8192 (KB)    \tor 2, 4, 8 (MB)\n" +
                              "PERQemu will round up to the nearest legal value.");
        }

        [Command("configure memory", "Set the memory size")]
        public void SetMemory(uint size)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.MemorySizeInBytes = (int)(size * 1024);
                return;
            }

            if (OKtoReconfig())
            {
                if (size > 0 && size <= 8)
                {
                    // Shortcut: assume they mean megabytes
                    size = RoundToPowerOf2(size) * 1024;
                }
                else if (size > 128 && size < 1024)
                {
                    // 256 and 512 are valid for quarter & half-meg boards
                    size = RoundToPowerOf2(size);
                }
                else if (size >= 1024 && size <= 8192)
                {
                    // Round to the nearest supported capacity
                    size = RoundToPowerOf2((size + 1023) / 1024) * 1024;
                }
                else
                {
                    Console.WriteLine("That's a silly amount of memory.");
                    return;
                }

                size *= 1024;               // In bytes

                if ((int)size != PERQemu.Config.Current.MemorySizeInBytes)
                {
                    PERQemu.Config.Current.MemorySizeInBytes = (int)size;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine("Memory size set to {0}.", PERQemu.Config.Current.MemSizeToString());
                }

                if (!PERQemu.Config.CheckMemory())
                {
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        [Command("configure io board", "Configure the IO board type")]
        public void SetIO(IOBoardType ioType)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.IOBoard = ioType;
                return;
            }

            if (OKtoReconfig())
            {
                if (ioType != PERQemu.Config.Current.IOBoard)
                {
                    Console.WriteLine($"IO Board type {ioType} selected.");

                    // Tell the Configurator to update our storage options
                    // for the new board type, if necessary
                    PERQemu.Config.UpdateStorage(ioType);

                    // Now make the switch
                    PERQemu.Config.Current.IOBoard = ioType;
                    PERQemu.Config.Changed = true;
                }

                if (!PERQemu.Config.CheckIO())
                {
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        [Command("configure option board", "Configure the IO Option board type")]
        public void SetOptionIO(OptionBoardType oioType)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.IOOptionBoard = oioType;
                return;
            }

            if (OKtoReconfig())
            {
                if (oioType != PERQemu.Config.Current.IOOptionBoard)
                {
                    PERQemu.Config.Current.IOOptionBoard = oioType;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine($"IO Option board type {oioType} selected.");

                    // Board changed; reset the selected options to defaults
                    if (oioType == OptionBoardType.OIO)
                    {
                        PERQemu.Config.Current.IOOptions = IOOptionType.Link;
                    }
                    else if (oioType == OptionBoardType.MLO)
                    {
                        PERQemu.Config.Current.IOOptions = IOOptionType.SMD;
                    }
                    else
                    {
                        PERQemu.Config.Current.IOOptions = IOOptionType.None;
                    }
                }

                if (!PERQemu.Config.CheckOptions())
                {
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        [Command("configure option add", "Configure extra features of the IO Option board")]
        public void SetIOOption(IOOptionType opt)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.IOOptions |= opt;
                return;
            }

            if (OKtoReconfig())
            {
                switch (opt)
                {
                    // Always valid; resets the selected options
                    case IOOptionType.None:
                        if (PERQemu.Config.Current.IOOptions != opt)
                        {
                            PERQemu.Config.Current.IOOptions = opt;
                            PERQemu.Config.Changed = true;
                            Console.WriteLine("IO options reset.");
                        }
                        break;

                    // These are valid for OIO and MLO
                    case IOOptionType.Link:
                    case IOOptionType.LinkTape:
                    case IOOptionType.Tape:
                        if (PERQemu.Config.Current.IOOptionBoard == OptionBoardType.OIO ||
                            PERQemu.Config.Current.IOOptionBoard == OptionBoardType.MLO)
                        {
                            PERQemu.Config.Current.IOOptions |= opt;
                            PERQemu.Config.Changed = true;
                            Console.WriteLine($"IO option '{opt}' selected.");
                        }
                        else
                        {
                            Console.WriteLine("That option not compatible with the selected IO Option board.");
                        }
                        break;

                    // These are only valid for OIO, but Ether may conflict
                    // if the EIO board is selected.  Does it work with the NIO?
                    case IOOptionType.Ether:
                    case IOOptionType.EthCan:
                    case IOOptionType.EthCanTape:
                    case IOOptionType.Canon:
                    case IOOptionType.CanTape:
                        if (PERQemu.Config.Current.IOBoard == IOBoardType.NIO &&
                            opt.HasFlag(IOOptionType.Ether))
                        {
                            // Special case: if the user wants to add Ethernet to a 
                            // machine configured with an NIO board, I'll just switch
                            // to the EIO and remove the optional (incompatible) Ethernet
                            // option.  This is silly.
                            PERQemu.Config.Current.IOBoard = IOBoardType.EIO;
                            PERQemu.Config.Current.IOOptions &= ~(IOOptionType.Ether);
                            Console.WriteLine("* Adding Ethernet option to an NIO changes it to an EIO; reconfiguring.");

                            if (PERQemu.Config.Current.IOOptions == IOOptionType.None)
                            {
                                // If that was the only option selected, remove the board
                                PERQemu.Config.Current.IOOptionBoard = OptionBoardType.None;
                                Console.WriteLine("* Removed empty IO Option board.");
                            }
                            PERQemu.Config.Changed = true;
                        }
                        else if (PERQemu.Config.Current.IOOptionBoard == OptionBoardType.OIO)
                        {
                            PERQemu.Config.Current.IOOptions |= opt;
                            PERQemu.Config.Changed = true;
                            Console.WriteLine($"IO option '{opt}' selected.");
                        }
                        else
                        {
                            Console.WriteLine("That option is incompatible with the selected IO Option board.");
                        }
                        break;
                }

                if (!PERQemu.Config.CheckOptions())
                {
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        [Command("configure option remove", "Remove or disable extra features of the IO Option board")]
        public void RemoveIOOption(IOOptionType opt)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.IOOptions &= ~opt;
                return;
            }

            if (OKtoReconfig())
            {
                if (PERQemu.Config.Current.IOOptions.HasFlag(opt))
                {
                    PERQemu.Config.Current.IOOptions &= ~opt;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine($"IO Option '{opt}' deselected.");

                    if (!PERQemu.Config.CheckOptions())
                    {
                        Console.WriteLine(PERQemu.Config.Current.Reason);
                    }
                }
            }
        }

        [Command("configure display", "Configure the display device")]
        public void SetDisplay(DisplayType disp)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.Display = disp;
                return;
            }

            if (OKtoReconfig())
            {
                if (disp != PERQemu.Config.Current.Display)
                {
                    PERQemu.Config.Current.Display = disp;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine($"{disp} display option selected.");
                }

                if (!PERQemu.Config.CheckMemory())
                {
                    Console.WriteLine(PERQemu.Config.Current.Reason);
                }
            }
        }

        [Command("configure tablet", "Configure the pointing device(s)")]
        public void SetTablet(TabletType tab)
        {
            if (PERQemu.Config.Quietly)
            {
                PERQemu.Config.Current.Tablet = tab;
                return;
            }

            if (OKtoReconfig())
            {
                if (tab != PERQemu.Config.Current.Tablet)
                {
                    PERQemu.Config.Current.Tablet = tab;
                    PERQemu.Config.Changed = true;
                    Console.WriteLine($"Tablet option {tab} selected.");
                }
            }
        }

        [Command("configure enable rs232", "Enable use of a serial port")]
        public void EnableRS232(char port = 'a')
        {
            if (OKtoReconfig())
            {
                bool changed = EnableOrDisableSerial(port, true);

                if (changed && !PERQemu.Config.Quietly)
                    Console.WriteLine($"RS-232 port {char.ToUpper(port)} enabled.");
            }
        }

        [Command("configure disable rs232", "Disable use of a serial port")]
        public void DisableRS232(char port = 'a')
        {
            if (OKtoReconfig())
            {
                bool changed = EnableOrDisableSerial(port, false);

                if (changed && !PERQemu.Config.Quietly)
                    Console.WriteLine($"RS-232 port {char.ToUpper(port)} disabled.");
            }
        }

        private bool EnableOrDisableSerial(char port, bool flag)
        {
            switch (port)
            {
                case 'a':
                case 'A':
                    if (PERQemu.Config.Current.RSAEnable != flag)
                    {
                        PERQemu.Config.Current.RSAEnable = flag;
                        PERQemu.Config.Changed = true;
                        return true;
                    }
                    return false;

                case 'b':
                case 'B':
                    if (PERQemu.Config.Current.IOBoard == IOBoardType.EIO ||
                        PERQemu.Config.Current.IOBoard == IOBoardType.NIO)
                    {
                        if (PERQemu.Config.Current.RSBEnable != flag)
                        {
                            PERQemu.Config.Current.RSBEnable = flag;
                            PERQemu.Config.Changed = true;
                            return true;
                        }
                        return false;
                    }
                    break;
            }

            // Fall through if bad port
            Console.WriteLine($"Invalid RS-232 port '{port}'.");
            return false;
        }

        /// <summary>
        /// Get the device type from a media file and assign it to the first
        /// available matching drive slot.
        /// </summary>
        [Command("configure assign", "Assign a media file to a storage device")]
        public void ConfigAssign(string file)
        {
            // Configurator does the heavy lifting
            PERQemu.Config.AssignMedia(file);
            Console.WriteLine(PERQemu.Config.Current.Reason);
        }

        /// <summary>
        /// Assign a media file to a particular unit #.
        /// </summary>
        [Command("configure assign", "Assign a media file to storage device [unit #]")]
        public void ConfigAssign(string file, byte unit)
        {
            PERQemu.Config.AssignMediaTo(unit, file);
            Console.WriteLine(PERQemu.Config.Current.Reason);
        }

        /// <summary>
        /// Unassign a media file.
        /// </summary>
        [Command("configure unassign", "Unassign an assigned media file")]
        public void ConfigUnassign(string file)
        {
            // Assume they "assign"ed the file and qualified the path...
            var path = Paths.FindFileInPath(file, Paths.DiskDir, FileUtilities.KnownExtensions);

            // Now look for what was given (or its qualified version)
            foreach (var d in PERQemu.Config.Current.Drives)
            {
                if (d.MediaPath == file || d.MediaPath == path)
                {
                    ConfigUnassignUnit((byte)d.Unit);
                    return;
                }
            }
            Console.WriteLine($"File '{file}' not assigned.");
        }

        /// <summary>
        /// Unassign the media file for a specific unit #.
        /// </summary>
        [Command("configure unassign unit", "Unassign the media file for unit #")]
        public void ConfigUnassignUnit(byte unit)
        {
            if (unit < 0 || unit > PERQemu.Config.Current.MaxUnitNum)
            {
                Console.WriteLine($"Unit {unit} is out of range.");
                return;
            }

            PERQemu.Config.Current.SetMediaPath(unit, string.Empty);
            Console.WriteLine($"Drive {unit} unassigned.");
        }

        /// <summary>
        /// "Discreet" command used at loading to allocate a drive slot and give
        /// it a type and (optional) filename.  This way we don't have to rely on
        /// the chassis and IO board selections happening in a particular order.
        /// This one could allow users to mess with their configurations in fun
        /// and unexpected ways, so I guess it forces me to put in the work to
        /// eliminate assumptions (like "floppy always in slot 0").  Sigh.
        /// </summary>
        [Command("configure drive", Discreet = true)]
        public void ConfigDrive(byte unit, DeviceType dev, string file = "")
        {
            // NO error checking, no output, assumes "quiet"
            PERQemu.Config.Current.SetDeviceType(unit, dev);
            PERQemu.Config.Current.SetMediaPath(unit, file);
        }
    }
}
