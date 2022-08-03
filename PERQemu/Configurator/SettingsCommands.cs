//
// SettingsCommands.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.UI
{
    /// <summary>
    /// CLI interface to the Option setting commands.  Program preferences are
    /// automatically saved and loaded in a platform-neutral format in a fixed
    /// location in the user's home directory.  Hint: the "format" is just the
    /// series of CLI commands required to set the desired options, read in and
    /// run automatically at startup. ;-)
    /// </summary>
    public class SettingsCommands
    {

        [Command("settings", "Enter the settings subsystem", Prefix = true)]
        public void SetSettingsPrefix()
        {
            PERQemu.CLI.SetPrefix("settings");
        }

        [Command("settings commands", "Show settings commands and their descriptions")]
        public void ShowSettingsCommands()
        {
            PERQemu.CLI.ShowCommands("settings");
        }

        [Command("settings done", "Exit settings mode, return to top-level")]
        public void SettingsDone()
        {
            PERQemu.CLI.ResetPrefix();
        }

        [Command("settings show", "Show all program settings")]
        public void ShowSettings()
        {
            Console.WriteLine("Current settings:");
            Console.WriteLine("-----------------");
            Console.WriteLine("Autosave harddisks on shutdown: " + Settings.SaveDiskOnShutdown);
            Console.WriteLine("Autosave floppies on eject:     " + Settings.SaveFloppyOnEject);
            Console.WriteLine("Pause execution after reset:    " + Settings.PauseOnReset);
            Console.WriteLine("Pause when window minimized:    " + Settings.PauseWhenMinimized);
            Console.WriteLine("Cursor in PERQ display window:  " + Settings.CursorPreference);
            Console.WriteLine();
            Console.WriteLine("Performance options: " + Settings.Performance);
            Console.WriteLine();
            Console.WriteLine("Default radix for CPU debugger: " + Settings.DebugRadix);
            Console.WriteLine("Default radix for Z80 debugger: " + Settings.Z80Radix);
            Console.WriteLine();
            Console.WriteLine("Default output directory:   " + Settings.OutputDirectory);
            Console.WriteLine("Screenshot file format:     " + Settings.ScreenshotFormat);
            Console.WriteLine("Canon output file format:   " + Settings.CanonFormat);
            Console.WriteLine();
            Console.Write("Host serial port A device:  ");
            Console.WriteLine(Settings.RSADevice == string.Empty ? "<unassigned>" : Settings.RSADevice);
            Console.Write("Host serial port B device:  ");
            Console.WriteLine(Settings.RSBDevice == string.Empty ? "<unassigned>" : Settings.RSBDevice);
           
            if (Settings.Changed)
            {
                Console.WriteLine("\nModified settings have not been saved.");
            }
        }

        [Command("settings default", "Reset all program settings to defaults")]
        public void SetDefaults()
        {
            Settings.Reset();
            Console.WriteLine(Settings.Reason);
        }

        [Command("settings load", "Reload saved settings")]
        public void LoadSettings()
        {
            Settings.Load();
            Console.WriteLine(Settings.Reason);
        }

        [Command("settings save", "Save current settings")]
        public void SaveSettings()
        {
            Settings.Save();
            Console.WriteLine(Settings.Reason);
        }

        [Command("settings autosave harddisk", "Save harddisks on shutdown")]
        public void SetAutosaveHard(Ask doit)
        {
            if (doit != Settings.SaveDiskOnShutdown)
            {
                Settings.SaveDiskOnShutdown = doit;
                Settings.Changed = true;
                Console.WriteLine("Autosave of hard disks is now {0}.", doit);
            }
        }

        [Command("settings autosave floppy", "Save modified floppy disks on eject")]
        public void SetAutosaveFloppy(Ask doit)
        {
            if (doit != Settings.SaveFloppyOnEject)
            {
                Settings.SaveFloppyOnEject = doit;
                Settings.Changed = true;
                Console.WriteLine("Autosave of floppy disks is now {0}.", doit);
            }
        }

        [Command("settings pause on reset", "Pause the emulator after a reset")]
        public void SetPauseOnReset(bool doit)
        {
            if (doit != Settings.PauseOnReset)
            {
                Settings.PauseOnReset = doit;
                Settings.Changed = true;
                Console.WriteLine("Pause on reset is now {0}.", doit);
            }
        }

        [Command("settings pause when minimized", "Pause the emulator when the display window is minimized")]
        public void SetPauseWhenMinimized(bool doit)
        {
            if (doit != Settings.PauseWhenMinimized)
            {
                Settings.PauseWhenMinimized = doit;
                Settings.Changed = true;
                Console.WriteLine("Pause when minimized is now {0}.", doit);
            }
        }

        [Command("settings display cursor", "Change the system cursor when in the display window")]
        public void SetCursorPref(Cursor curs)
        {
            if (curs != Settings.CursorPreference)
            {
                Settings.CursorPreference = curs;
                Settings.Changed = true;
                Console.WriteLine("Cursor preference changed to {0}.", curs);
            }
        }

        [Command("settings performance option", "Set performance option flags")]
        public void SetPerformance(RateLimit opt)
        {
            // Ha this doesn't work as expected.  Duh.  Rethink this.
            if (Settings.Performance.HasFlag(opt))
            {
                Settings.Performance &= ~opt;
            }
            else
            {
                Settings.Performance |= opt;
            }
            Settings.Changed = true;
            Console.WriteLine("Performance options set to {0}", Settings.Performance);
        }

        [Command("settings output directory", "Set directory for saving printer output and screenshots")]
        public void SetOutputDir(string dir)
        {
            if (dir == string.Empty)
            {
                dir = Paths.OutputDir;      // Reset to default?  Hmm.
            }

            dir = Paths.Canonicalize(dir);

            if (dir != Settings.OutputDirectory)
            {
                Settings.OutputDirectory = dir;
                Settings.Changed = true;
                Console.WriteLine("Default output directory is now '{0}'.", dir);
            }
        }


        [Command("settings assign rs232 device", "Map a host device to a PERQ serial port")]
        public void SetRS232Device(char port, string hostDevice)
        {
            var dev = hostDevice;
            var curDev = string.Empty;

            switch (port)
            {
                case 'a':
                case 'A':
                    curDev = Settings.RSADevice;
                    port = 'A';
                    break;

                case 'b':
                case 'B':
                    curDev = Settings.RSBDevice;
                    port = 'B';
                    break;

                default:
                    Console.WriteLine($"Port {port} is invalid; please choose 'A' or 'B'.");
                    return;
            }

            if (CheckDevice(ref dev))
            {
                if (dev != curDev)
                {
                    if (port == 'A')
                        Settings.RSADevice = dev;
                    else
                        Settings.RSBDevice = dev;
                    
                    Settings.Changed = true;
                }

                Console.WriteLine($"Device '{dev}' assigned to serial port {port}.");
                return;
            }

            Console.WriteLine($"Device '{dev}' invalid or not found; port {port} unchanged.");
        }

        [Command("settings unassign rs232 device", "Unmap a device from a PERQ serial port")]
        private void UnSetRS232Device(char port = 'a')
        {
            var curDev = string.Empty;

            switch (port)
            {
                case 'a':
                case 'A':
                    curDev = Settings.RSADevice;
                    port = 'A';
                    break;

                case 'b':
                case 'B':
                    curDev = Settings.RSBDevice;
                    port = 'B';
                    break;

                default:
                    Console.WriteLine($"Port {port} is invalid; please choose 'A' or 'B'.");
                    return;
            }

            if (!string.IsNullOrEmpty(curDev))
            {
                if (port == 'A')
                    Settings.RSADevice = string.Empty;
                else
                    Settings.RSBDevice = string.Empty;
                
                Settings.Changed = true;
            }

            Console.WriteLine($"Serial port {port} unassigned.");
        }

        /// <summary>
        /// Checks a host serial device specification.
        /// </summary>
        // todo: generalize this when Ethernet, audio devices are added
        private bool CheckDevice(ref string dev)
        {
            // Any host:  allow "rsx" or "rsx:", upcase it
            if (dev.ToUpper() == "RSX" || dev.ToUpper() == "RSX:")
            {
                dev = "RSX:";
                return true;
            }

            // If Unix, allow "devN" or /dev/devN" form; prepend /dev if not supplied
            if (PERQemu.HostIsUnix)
            {
                if (!dev.StartsWith("/dev", StringComparison.InvariantCulture))
                    dev = "/dev/" + dev;

                return File.Exists(dev);
            }

            // If Windows, expect 'COMn:' form; append the ':' if not present
            if (dev.StartsWith("com", StringComparison.InvariantCultureIgnoreCase))
            {
                dev = dev.ToUpper();

                if (!dev.EndsWith(":", StringComparison.InvariantCulture))
                    dev = dev + ":";

                return File.Exists(dev);
            }

            return false;
        }

        /*
            TODO:
            settings::screenshot format [jpg, png, tiff, ?]
            settings::screenshot template [str] -- really?  cmon...
            settings::canon format [jpg, png, tiff, bmp, PDF!?]
            settings::canon template [str]      -- same 
            settings::logging directory         -- default: Output/
            settings::logging template [str]    -- hmm.
            settings::logging keep [n]          -- how many files
            settings::logging filesize [n]      -- in mb?  kb?
            -- oh hey, i know, let's use log4j instead of rolling our own... :-P

            Host interface to the network, serial and audio output devices
            is globally set for all virtual machines:
            
            settings::ethernet device [dev]         -- host interface to use
            settings::ethernet encapsulation [raw, udp, 3to10bridge]
            settings::ethernet use3rccPrefix        -- :-)
            settings::audio device [dev]            -- audio output device?

            When the PERQ Ethernet device is configured, we configure the
            emulator-specific stuff with the particular Configuration:
            
            configure::ethernet device [eio, oio]   -- only one or the other (automatic?)
            configure::ethernet address [n] [m]     -- last two octets only
                                                    -- or just one for 3mbit!?

            Can we use SDL2 for network access without requiring additional
            libraries like Pcap?  Can we use the SDL2 audio without worrying
            about extra configuration options?
        */
    }
}
