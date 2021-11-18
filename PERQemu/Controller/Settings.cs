//
// Settings.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Drawing.Imaging;

namespace PERQemu
{
    public enum Ask
    {
        Yes = 0,
        No,
        Maybe
    }

    public enum Radix
    {
        Binary = 2,
        Octal = 8,
        Decimal = 10,
        Hexadecimal = 16
    }

    [Flags]
    public enum RateLimit
    {
        Fast = 0,                               // 110% on the reactor
        AccurateCPUSpeedEmulation = 0x1,        // strive for accuracy
        AccurateDiskSpeedEmulation = 0x2,       // feel the pain
        AllowFrameSkipping = 0x4,               // because mono winforms sucks
        Accurate = 0x7,                         // keep it real
    	SlowWhenMinimized = 0x08,               // save the salmon
    }

    public static class Settings
    {
        static Settings()
        {
            Reset();
        }

        public static void Reset()
        {
            // Set to defaults
            SaveFloppyOnEject = Ask.Maybe;
            SaveDiskOnShutdown = Ask.Maybe;
            PauseOnReset = true;

            Performance = RateLimit.Accurate;

            DebugRadix = Radix.Decimal;
            Z80Radix = Radix.Decimal;

            OutputDirectory = Paths.OutputDir;
            ScreenshotFormat = ImageFormat.Jpeg;
            ScreenshotTemplate = "{0}_{1:000}.{2}";
            CanonFormat = ImageFormat.Tiff;
            CanonTemplate = "{0}_{1:000}.{2}";

            Reason = "Settings reset to defaults.";
            Changed = false;
        }

        // General
        public static Ask SaveDiskOnShutdown { get; set; }
        public static Ask SaveFloppyOnEject { get; set; }
        public static bool PauseOnReset { get; set; }

        // Performance
        public static RateLimit Performance { get; set; }

        // Debugger
        public static Radix DebugRadix { get; set; }
        public static Radix Z80Radix { get; set; }

        // Output
        public static string OutputDirectory { get; set; }
        public static ImageFormat ScreenshotFormat { get; set; }
        public static string ScreenshotTemplate { get; private set; }
        public static ImageFormat CanonFormat { get; set; }
        public static string CanonTemplate { get; private set; }

        // Logging
        //  LogDir, LogTemplate, LogSize, LogLimit = see Log.cs

        // Host Devices
        //public string HostRSADevice _devRSA;
        //public string HostRSBDevice _devRSB;
        //public string HostAudioDevice _devAudio;
        //public string HostEtherDevice _devEther;
        //public EtherEncapsulationType _netEncap;
        //public bool _netUse3rccMAC;

        // Housekeeping
        public static string Reason { get; set; }
        public static bool Changed { get; set; }


        /// <summary>
        /// Load the user preferences file, or fall back to the defaults
        /// if it doesn't exist.
        /// </summary>
        public static void Load()
        {
            try
            {
                PERQemu.CLI.ReadScript(Paths.SettingsPath);
                Reason = "Settings loaded.";
                Changed = false;
            }
            catch (Exception e)
            {
                Reason = "Failed to load settings: " + e.Message;
                Reset();
            }
        }

        public static bool Save()
        {
            if (!Changed)
            {
                // Lie.  Nothing changed, so nothing to save.
                Reason = "Settings unchanged.";
                return true;
            }

            try
            {
                using (StreamWriter sw = new StreamWriter(Paths.SettingsPath, false))
                {
                    //
                    // Write a small header, then enter configuration mode and
                    // write the basic things first.
                    //
                    sw.WriteLine("# PERQemu settings file, written " + DateTime.Now);
                    sw.WriteLine("# " + PERQemu.Version);
                    sw.WriteLine("#\n# * Please do not hand-edit this file. *\n#");
                    sw.WriteLine("settings");
                    sw.WriteLine("default");        // it's the only way to be sure
                    sw.WriteLine("autosave harddisk " + SaveDiskOnShutdown);
                    sw.WriteLine("autosave floppy " + SaveFloppyOnEject);
                    sw.WriteLine("pause on reset " + PauseOnReset);
                    sw.WriteLine("debug radix " + DebugRadix);
                    sw.WriteLine("z80 debug radix " + Z80Radix);
                    sw.WriteLine("screenshot format " + ScreenshotFormat);
                    sw.WriteLine("canon format " + CanonFormat);

                    if (!string.IsNullOrEmpty(OutputDirectory))
                    {
                        sw.WriteLine("output directory \"" + OutputDirectory + "\"");
                    }

                    sw.WriteLine("done");

                    // If the user has selected a valid PERQ, save it so it's
                    // the default machine configuration upon restart
                    if (PERQemu.Config.Current.IsValid &&
                        PERQemu.Config.Current.Name != "default")
                    {
                        sw.WriteLine("#\n# Most recent loaded configuration:");
                        sw.WriteLine("configure select " + PERQemu.Config.Current.Name);
                    }

                    sw.Close();
                }

                Reason = "Settings saved.";
                Changed = false;
                return true;
            }
            catch (Exception e)
            {
                Changed = true;     // have you tried turning it off and on again?
                Reason = "Could not save settings: " + e.Message;
                return false;
            }
        }
    }
}
