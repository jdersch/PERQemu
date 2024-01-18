//
// SettingsCommands.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
using System.IO.Ports;

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
            CheckSerialPorts();
            PERQemu.CLI.ResetPrefix();
        }

        [Command("settings show", "Show all program settings")]
        public void ShowSettings()
        {
            Console.WriteLine("Current settings:");
            Console.WriteLine("-----------------");
            Console.WriteLine($"Autosave harddisks on shutdown: {Settings.SaveDiskOnShutdown}");
            Console.WriteLine($"Autosave floppies on eject:     {Settings.SaveFloppyOnEject}");
            Console.WriteLine($"Autosave tapes on unload:       {Settings.SaveTapeOnUnload}");
            Console.WriteLine($"Pause execution after reset:    {Settings.PauseOnReset}");
            Console.WriteLine($"Pause when window minimized:    {Settings.PauseWhenMinimized}");
            Console.WriteLine($"Cursor in PERQ display window:  {Settings.CursorPreference}");
            Console.WriteLine();
            Console.WriteLine($"Rate limiting options:  {Settings.Performance}");
            Console.WriteLine();
            Console.WriteLine($"Default radix for CPU debugger: {Settings.DebugRadix}");
            Console.WriteLine($"Default radix for Z80 debugger: {Settings.Z80Radix}");
            Console.WriteLine();
            Console.WriteLine($"Default output directory:   {Settings.OutputDirectory}");
            Console.WriteLine($"Screenshot file format:     {Settings.ScreenshotFormat}");
            Console.WriteLine($"Canon output file format:   {Settings.CanonFormat}");
            Console.WriteLine($"Canon default paper type:   {Settings.CanonPaperSize}");
            Console.WriteLine($"Canon default resolution:   {Settings.CanonResolution}dpi");
            Console.WriteLine();
            Console.Write("Host serial port A device:  ");
            Console.WriteLine(Settings.RSADevice == string.Empty ? "<unassigned>" :
                              Settings.RSADevice == "RSX:" ? "RSX:" :
                              $"{Settings.RSADevice} {Settings.RSASettings}");
            Console.Write("Host serial port B device:  ");
            Console.WriteLine(Settings.RSBDevice == string.Empty ? "<unassigned>" :
                              Settings.RSBDevice == "RSX:" ? "RSX:" :
                              $"{Settings.RSBDevice} {Settings.RSBSettings}");
            Console.Write("Host Ethernet device:       ");
            Console.WriteLine(Settings.EtherDevice == string.Empty ? "<unassigned>" :
                              $"{Settings.EtherDevice}");

            if (Settings.Changed)
            {
                Console.WriteLine("\nModified settings have not been saved.");
            }
        }

        [Command("settings default", "Reset all program settings to defaults")]
        public void SetDefaults()
        {
            Settings.Reset();
            QuietWrite(Settings.Reason);
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

                QuietWrite($"Autosave of hard disks is now {doit}.");
            }
        }

        [Command("settings autosave floppy", "Save modified floppy disks on eject")]
        public void SetAutosaveFloppy(Ask doit)
        {
            if (doit != Settings.SaveFloppyOnEject)
            {
                Settings.SaveFloppyOnEject = doit;
                Settings.Changed = true;

                QuietWrite($"Autosave of floppy disks is now {doit}.");
            }
        }

        [Command("settings autosave tape", "Save modified streamer tapes on unload")]
        public void SetAutosaveTape(Ask doit)
        {
            if (doit != Settings.SaveTapeOnUnload)
            {
                Settings.SaveTapeOnUnload = doit;
                Settings.Changed = true;

                QuietWrite($"Autosave of streamer tapes is now {doit}.");
            }
        }

        [Command("settings pause on reset", "Pause the emulator after a reset")]
        public void SetPauseOnReset(bool doit)
        {
            if (doit != Settings.PauseOnReset)
            {
                Settings.PauseOnReset = doit;
                Settings.Changed = true;

                QuietWrite($"Pause on reset is now {doit}.");
            }
        }

        [Command("settings pause when minimized", "Pause the emulator when the display window is minimized")]
        public void SetPauseWhenMinimized(bool doit)
        {
            if (doit != Settings.PauseWhenMinimized)
            {
                Settings.PauseWhenMinimized = doit;
                Settings.Changed = true;

                QuietWrite($"Pause when minimized is now {doit}.");
            }
        }

        [Command("settings display cursor", "Change the system cursor when in the display window")]
        public void SetCursorPref(Cursor curs)
        {
            if (curs != Settings.CursorPreference)
            {
                Settings.CursorPreference = curs;
                Settings.Changed = true;

                QuietWrite($"Cursor preference changed to {curs}.");
            }
        }

        [Command("settings rate limit default", "Set default rate limits")]
        public void SetPerfDefaults()
        {
            Settings.Performance = RateLimit.CPUSpeed | RateLimit.DiskSpeed | RateLimit.TapeSpeed;

            QuietWrite("Rate limit options set to defaults.");
        }

        [Command("settings rate limit", "Set rate limit option flags")]
        public void SetPerformance(RateLimit opt)
        {
            // "None" clears all the options; otherwise, they toggle
            if (opt == RateLimit.None)
            {
                Settings.Changed = (Settings.Performance != RateLimit.None);
                Settings.Performance = opt;
            }
            else
            {
                if (Settings.Performance.HasFlag(opt))
                {
                    Settings.Performance &= ~opt;
                }
                else
                {
                    Settings.Performance |= opt;
                }
                Settings.Changed = true;
            }

            QuietWrite($"Rate limit options set to {Settings.Performance}.");
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

                QuietWrite($"Default output directory is now '{dir}'.");
            }
        }

        //
        // Todo: set image format and file naming scheme for Canon, screenshots
        //

        [Command("settings canon resolution", "Set resolution (model) of Canon laser printer to simulate")]
        public void SetCanonResolution(int dpi)
        {
            if (dpi != 240 && dpi != 300)
            {
                QuietWrite($"{dpi}dots per inch is not valid.  Please choose 240 (LBP-10) or 300 (LBP-CX).");
                dpi = 300;
            }

            if (Settings.CanonResolution != dpi)
            {
                Settings.CanonResolution = (uint)dpi;
                Settings.Changed = true;

                QuietWrite($"Canon printer resolution set to {dpi}dpi.");
            }
        }

        [Command("settings canon paper type", "Set default paper size for the Canon laser printer")]
        public void SetCanonPaperType(IO.PaperCode size)
        {
            if (size != Settings.CanonPaperSize)
            {
                Settings.CanonPaperSize = size;
                Settings.Changed = true;
                QuietWrite($"Canon default paper size set to {size}.");
            }
        }

        [Command("settings canon output format", "Set image file format for Canon laser printer output")]
        public void SetCanonOutputFormat(ImageFormat format)
        {
            if (format != Settings.CanonFormat)
            {
                Settings.CanonFormat = format;
                Settings.Changed = true;
                QuietWrite($"Canon default output format set to {format}.");
            }
        }

        [Command("settings assign rs232 device", "Map a host device to a PERQ serial port")]
        public void SetRS232Device(char port, [KeywordMatch("ComPorts")] string hostDevice,
                                  int baud = 9600, int data = 8, Parity par = Parity.None, StopBits stop = StopBits.One)
        {
            var dev = hostDevice;
            var devSettings = new SerialSettings(baud, data, par, stop);
            var curDev = string.Empty;
            SerialSettings curSettings;

            switch (port)
            {
                case 'a':
                case 'A':
                    curDev = Settings.RSADevice;
                    curSettings = Settings.RSASettings;
                    port = 'A';
                    break;

                case 'b':
                case 'B':
                    curDev = Settings.RSBDevice;
                    curSettings = Settings.RSBSettings;
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
                    {
                        Settings.RSADevice = dev;
                        Settings.RSASettings = devSettings;
                    }
                    else
                    {
                        Settings.RSBDevice = dev;
                        Settings.RSBSettings = devSettings;
                    }

                    Settings.Changed = true;
                    QuietWrite($"Device '{dev}' assigned to serial port {port}.");
                    return;
                }

                if (!devSettings.Equals(curSettings))
                {
                    if (port == 'A')
                    {
                        Settings.RSASettings = devSettings;
                    }
                    else
                    {
                        Settings.RSBSettings = devSettings;
                    }

                    Settings.Changed = true;
                    QuietWrite($"Serial port {port} settings changed.");
                }
                return;
            }

            Console.WriteLine($"Device '{dev}' invalid or not found; port {port} unchanged.");
        }

        [Command("settings unassign rs232 device", "Unmap a device from a PERQ serial port")]
        void UnSetRS232Device(char port = 'a')
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
        bool CheckDevice(ref string dev)
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

            // If Windows, expect 'COMn' form; the names should have been pre-
            // qualified by the KeywordMatch setter. ;-)
            if (dev.StartsWith("com", StringComparison.InvariantCultureIgnoreCase))
            {
                dev = dev.ToUpper();

                return true;    // Would have to do a serial .Open() to verify...
            }

            return false;
        }

        /// <summary>
        /// Checks the serial port assignments to make sure the same device isn't
        /// assigned to both channels.
        /// </summary>
        /// <remarks>
        /// For now (?) this is a warning only, since there's no harm in assigning
        /// port B to the same device as port A when initializing a PERQ-1 -- since
        /// there is no port B so it can't conflict.  But with PERQ-2/EIO configs,
        /// opening the same physical port twice will either fail outright, or act
        /// very strangely...
        /// </remarks>
        void CheckSerialPorts()
        {
            // If either (or both) is empty, or they don't match, no conflict!
            if (string.IsNullOrEmpty(Settings.RSADevice) ||
                string.IsNullOrEmpty(Settings.RSBDevice) ||
               Settings.RSADevice != Settings.RSBDevice)
                return;

            // If they do match issue a warning
            Log.Warn(Category.All,
                     "Note: Both RS-232 ports assigned to the same device; some configurations\n" +
                     "might not load properly.  Please check your settings to reassign ports.");
        }

        [Command("settings assign ethernet device", "Map a host network adapter to the PERQ Ethernet device")]
        public void SetEtherDev([KeywordMatch("NICs")] string hostDevice)
        {
            if (hostDevice != Settings.EtherDevice /* && hostDevice == "null" || it's a valid pcap device from the list */)
            {
                Settings.EtherDevice = hostDevice;
                Settings.Changed = true;
                QuietWrite($"Host adapter '{hostDevice}' assigned to the PERQ Ethernet device.");
            }
        }

        [Command("settings unassign ethernet device", "Unmap a host network adapter (disable PERQ Ethernet)")]
        public void UnsetEtherDev()
        {
            if (!string.IsNullOrEmpty(Settings.EtherDevice))
            {
                Settings.EtherDevice = string.Empty;
                Settings.Changed = true;
            }

            QuietWrite("Ethernet device unassigned.");
        }

        // Pure cheese.  Don't spew messages when reading on startup.
        void QuietWrite(string s)
        {
            if (PERQemu.Initialized) Console.WriteLine(s);
        }
    }
}

/*
	TODO:
	settings::screenshot format [jpg, png, tiff, ?]
	settings::screenshot template [str] -- really?  cmon...
	settings::canon template [str]      -- same 
	settings::logging directory         -- default: Output/
	settings::logging template [str]    -- hmm.
	settings::logging keep [n]          -- how many files
	settings::logging filesize [n]      -- in mb?  kb?
	-- oh hey, i know, let's use log4j instead of rolling our own... :-P

	Host interface to the network, serial and audio output devices
	is globally set for all virtual machines:

	settings::ethernet encapsulation [raw, udp, ???]
	settings::audio device [dev]            -- audio output device?
*/
