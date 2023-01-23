﻿//
// Settings.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

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

    public enum Cursor
    {
        DefaultArrow = 0,
        Crosshairs,
        Hidden
    }

    // Temporary:  remove dependency on System.Drawing since that's not
    // present on Linux (Mint, anyway) mono by default?
    public enum ImageFormat
    {
        None = 0,
        Jpeg,
        Png,
        Tiff
    }

    [Flags]
    public enum RateLimit
    {
        None = 0,                           // 110% on the reactor
        CPUSpeed = 0x1,                     // strive for accuracy
        DiskSpeed = 0x2,                    // feel the pain
        TapeSpeed = 0x4,                    // is this a trick question?
        StartupDelay = 0x10,                // for the truly hardcore
        FrameSkipping = 0x20                // not implemented
    }

    public struct SerialSettings
    {
        public SerialSettings(int baud, int data, Parity parity, StopBits stop)
        {
            BaudRate = baud;
            DataBits = data;
            Parity = parity;
            StopBits = stop;
        }

        public override string ToString()
        {
            return $"{BaudRate} {DataBits} {Parity} {StopBits}";
        }

        public int BaudRate;
        public int DataBits;
        public Parity Parity;
        public StopBits StopBits;
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
            SaveDiskOnShutdown = Ask.Maybe;
            SaveFloppyOnEject = Ask.Maybe;
            SaveTapeOnUnload = Ask.Maybe;
            PauseOnReset = true;
            PauseWhenMinimized = true;
            CursorPreference = Cursor.DefaultArrow;

            Performance = RateLimit.CPUSpeed | RateLimit.DiskSpeed | RateLimit.TapeSpeed;
            RunMode = ExecutionMode.Asynchronous;

            DebugRadix = Radix.Decimal;
            Z80Radix = Radix.Decimal;

            OutputDirectory = Paths.OutputDir;
            ScreenshotFormat = ImageFormat.Jpeg;
            ScreenshotTemplate = "{0}_{1:000}.{2}";
            CanonFormat = ImageFormat.Tiff;
            CanonTemplate = "{0}_{1:000}.{2}";

            RSADevice = string.Empty;
            RSBDevice = string.Empty;

            RSASettings = new SerialSettings(9600, 8, Parity.None, StopBits.One);
            RSBSettings = new SerialSettings(9600, 8, Parity.None, StopBits.One);

            Reason = "Settings reset to defaults.";
            Changed = false;
        }

        // General
        public static Ask SaveDiskOnShutdown;
        public static Ask SaveFloppyOnEject;
        public static Ask SaveTapeOnUnload;
        public static bool PauseOnReset;

        public static bool PauseWhenMinimized;
        public static Cursor CursorPreference;

        // Performance
        public static RateLimit Performance;
        public static ExecutionMode RunMode;

        // Debugger
        public static Radix DebugRadix;
        public static Radix Z80Radix;

        // Output
        public static string OutputDirectory;
        public static ImageFormat ScreenshotFormat;
        public static string ScreenshotTemplate { get; private set; }
        public static ImageFormat CanonFormat;
        public static string CanonTemplate { get; private set; }

        // Logging - see Log.cs
        //public static string LogDir;
        //public static string LogTemplate;
        //public static int LogSize;
        //public static int LogLimit;

        // Host Devices
        public static string RSADevice;
        public static string RSBDevice;

        public static SerialSettings RSASettings;
        public static SerialSettings RSBSettings;

        //public static string AudioDevice;
        //public static string EtherDevice;
        //public static EtherEncapsulationType EtherEncapsulation;
        //public static bool Use3RCCEtherMACPrefix;

        // Housekeeping
        public static string Reason;
        public static bool Changed;


        /// <summary>
        /// Load the user preferences file, or fall back to the defaults
        /// if it doesn't exist.
        /// </summary>
        public static void Load()
        {
            // Set the list of COM ports for the "assign rs232 device" command
            PERQemu.CLI.UpdateKeywordMatchHelpers("ComPorts", GetHostSerialPorts());

            try
            {
                if (!File.Exists(Paths.SettingsPath))
                {
                    // First time?  Missing file?  Set to defaults and save
                    Reset();
                    Changed = true;
                    Save();
                }
                else
                {
                    PERQemu.CLI.ReadScript(Paths.SettingsPath);
                    Reason = "Settings loaded.";
                    Changed = false;
                }
            }
            catch (Exception e)
            {
                Reason = $"Failed to load settings: {e.Message}";
                Reset();
            }
        }

        /// <summary>
        /// Write out a new user preferences file with the current settings.
        /// </summary>
        public static bool Save()
        {
            if (!Changed)
            {
                // Lie.  Nothing changed, so nothing to save...
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
                    sw.WriteLine("#\n# * Please take care if hand-editing this file! *");
                    sw.WriteLine("# *  Errors may cause PERQemu to fail to load.  *\n#");
                    sw.WriteLine("settings");
                    sw.WriteLine("default");
                    sw.WriteLine("autosave harddisk " + SaveDiskOnShutdown);
                    sw.WriteLine("autosave floppy " + SaveFloppyOnEject);
                    sw.WriteLine("autosave tape " + SaveTapeOnUnload);
                    sw.WriteLine("display cursor " + CursorPreference);
                    sw.WriteLine("pause on reset " + PauseOnReset);
                    sw.WriteLine("pause when minimized " + PauseWhenMinimized);

                    // Enumerate any rate limit options
                    if (Performance == RateLimit.None)
                    {
                        sw.WriteLine("rate limit none");
                    }
                    else
                    {
                        foreach (RateLimit opt in Enum.GetValues(typeof(RateLimit)))
                        {
                            if (Performance.HasFlag(opt))
                                sw.WriteLine("rate limit " + opt);
                        }
                    }

                    // Device mappings
                    if (!string.IsNullOrEmpty(RSADevice))
                    {
                        sw.Write("assign rs232 device a ");
                        sw.WriteLine(RSADevice == "RSX:" ? "RSX:" : $"{RSADevice} {RSASettings}");
                    }

                    if (!string.IsNullOrEmpty(RSBDevice))
                    {
                        sw.Write("assign rs232 device b ");
                        sw.WriteLine(RSBDevice == "RSX:" ? "RSX:" : $"{RSBDevice} {RSBSettings}");
                    }

                    // todo: audio, Ethernet

                    if (!string.IsNullOrEmpty(OutputDirectory))
                        sw.WriteLine("output directory \"" + OutputDirectory + "\"");

                    sw.WriteLine("#");
                    sw.WriteLine("# These options are not yet implemented:");
                    sw.WriteLine("# debug radix " + DebugRadix);
                    sw.WriteLine("# z80 debug radix " + Z80Radix);
                    sw.WriteLine("# screenshot format " + ScreenshotFormat);
                    sw.WriteLine("# canon format " + CanonFormat);
                    sw.WriteLine("#");

                    sw.WriteLine("done");

                    // If the user has selected a valid PERQ, save it so it's
                    // the default machine configuration upon restart
                    if (PERQemu.Config.Current.IsValid &&
                        PERQemu.Config.Current.Name != "default")
                    {
                        sw.WriteLine("#\n# Most recent loaded configuration:");
                        sw.WriteLine("configure load " + PERQemu.Config.Current.Name);
                    }

                    sw.Close();
                }

                Reason = "Settings saved.";
                Changed = false;
                return true;
            }
            catch (Exception e)
            {
                Changed = true;
                Reason = $"Could not save settings: {e.Message}";
                return false;
            }
        }

        /// <summary>
        /// Find the serial ports on a host according to slightly better heuristics
        /// (on Unix) than the built-in SerialPort.GetPortNames() call.
        /// </summary>
        public static string[] GetHostSerialPorts()
        {
            var ports = new List<string>();

            // Always available, as a fallback...
            ports.Add("RSX:");

            if (PERQemu.HostIsUnix)
            {
                // Ha ha ha, this doesn't actually work.  I'm just now finding this out.
                // if (Environment.OSVersion.Platform == PlatformID.MacOSX)

                // Okay, try looking for the MacOS X-style names first...
                ports.AddRange(Directory.GetFiles("/dev", "cu.*usb*"));
                ports.AddRange(Directory.GetFiles("/dev", "tty.*usb*"));

                if (ports.Count == 1)
                {
                    // Nothin'?  Well, just add whatever Mono gives us
                    ports.AddRange(SerialPort.GetPortNames());
                }
            }
            else
            {
                // Assume Windows finds the appropriate COMn-style names
                ports.AddRange(SerialPort.GetPortNames());
            }

            return ports.ToArray();
        }
    }
}