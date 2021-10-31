//
// ConfigCommands.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;
using System.Text;

using PERQemu.Config;

namespace PERQemu.UI
{
    /// <summary>
    /// Command-line interface to the Configurator.  Operates directly on the
    /// Current configuration, so changes are immediate.
    /// </summary>
    public class ConfigCommands
    {

        [Command("configure", "Enter the configuration subsystem")]
        public void SetConfigPrefix()
        {
            PERQemu.CLI.SetPrefix("configure");
        }

        [Command("configure commands", "Show configuration commands and their descriptions")]
        public void ShowConfigCommands()
        {
            PERQemu.CLI.ShowCommands("configure");
        }

        [Command("configure done", "Exit configuration mode, return to top-level")]
        public void ConfigDone()
        {
            if (!PERQemu.Config.Validate())
            {
                Console.WriteLine("This configuration is invalid!  Please correct the following error:");
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
            PERQemu.CLI.ResetPrefix();
        }

        [Command("configure default", "Reset the machine to the default configuration")]
        public void SetDefault()
        {
            Console.WriteLine("Setting the machine to defaults.");
            PERQemu.Config.Current = PERQemu.Config.Default;
            
        }

        [Command("configure list", "List pre-defined machine configurations")]
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

        [Command("configure select", "Select a pre-defined machine configuration")]
        public void SelectPrefab(string name)
        {
            Configuration newConf = PERQemu.Config.GetConfigByName(name);

            // Did we git a goodun, pa?
            if (newConf == null)
            {
                Console.WriteLine("No configuration matching '{0}'.", name);
                Console.WriteLine("Use the configuration 'list' command to see available configurations.");
            }
            else if (newConf != PERQemu.Config.Current)
            {
                Console.WriteLine("Configuration '{0}' selected.", newConf);
                PERQemu.Config.Current = newConf;
            }
        }

        [Command("configure show", "Show current configuration details")]
        public void ShowConfiguration()
        {
            ShowConfiguration("current");
        }

        [Command("configure show", "Show a pre-defined machine configuration")]
        public void ShowConfiguration(string name)
        {
            Configuration conf = PERQemu.Config.GetConfigByName(name);

            if (conf == null)
            {
                Console.WriteLine("No configuration matching '{0}'.", name);
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

        [Command("configure load")]
        public void LoadConfig()
        {
            Console.WriteLine("Please specify a filename to load a saved configuration.");
        }

        [Command("configure load", "Load a saved configuration")]
        public void LoadConfig(string file)
        {
            if (!PERQemu.Config.Load(file))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
            else
            {
                Console.WriteLine("Configuration loaded.");
            }
        }

        [Command("configure save", "Save the configuration to the current file")]
        public void SaveConfig()
        {
            if (!PERQemu.Config.Save())
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
            else
            {
                Console.WriteLine("Configuration saved.");
            }
        }

        [Command("configure save", "Save the configuration to a named file")]
        public void SaveConfig(string file)
        {
            PERQemu.Config.Current.Filename = file;
            SaveConfig();
        }

        [Command("configure name")]
        public void SetName()
        {
            Console.WriteLine("(Optional) Give the configuration a short name.  In the Configurator GUI,");
            Console.WriteLine("this name appears in the drop-down menu of machine types.");
        }

        [Command("configure name", "Name the current configuration")]
        public void SetName(string name)
        {
            PERQemu.Config.Current.Name = name;
        }

        [Command("configure description")]
        public void SetDescription()
        {
            Console.WriteLine("(Optional) Provide a short description for this configuration.  Supply a");
            Console.WriteLine("quoted string if you wish to include spaces in the description.");
        }

        [Command("configure description", "Add a short description of the current configuration")]
        public void SetDescription(string desc)
        {
            PERQemu.Config.Current.Description = desc;
        }

        [Command("configure chassis")]
        public void SetChassis()
        {
            Console.WriteLine("Configure the basic machine type.  Valid chassis types:");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(ChassisType))));
            Console.WriteLine();
            Console.WriteLine("The chassis selected affects what CPU, IO and Disk options are available.");
        }

        [Command("configure chassis", "Set the machine type")]
        public void SetChassis(ChassisType perq)
        {
            if (perq != PERQemu.Config.Current.Chassis)
            {
                Console.WriteLine("{0} chassis selected.", perq);
                PERQemu.Config.Current.Chassis = perq;
            }
        }

        [Command("configure cpu")]
        public void SetCPU()
        {
            Console.WriteLine("Configure the CPU board.  Valid CPU board types: ");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(CPUType))));
            Console.WriteLine();
            Console.WriteLine("Selected CPU type may depend on chassis and other options.");
        }

        [Command("configure cpu", "Set the CPU type")]
        public void SetCPU(CPUType cpu)
        {
            if (cpu != PERQemu.Config.Current.CPU)
            {
                Console.WriteLine("{0} CPU selected.", cpu);
                PERQemu.Config.Current.CPU = cpu;
            }

            if (!PERQemu.Config.CheckCPU(PERQemu.Config.Current))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
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
            Console.WriteLine("Configure the memory board, from 256KB to 16MB.  The amount of memory");
            Console.WriteLine("supported depends on the CPU type, and must be a power of two.");
            Console.WriteLine("\t20-bit CPU: 256, 512, 1024 or 2048 (KB)");
            Console.WriteLine("\t24-bit CPU: 2048, 4096, 8192, or 16384 (KB)");
            Console.WriteLine("Or enter 1, 2, 4, 8 or 16 for MB.  PERQemu will round up to the nearest\nlegal value.");
        }

        [Command("configure memory", "Set the memory size")]
        public void SetMemory(uint size)
        {
            if (size > 0 && size <= 16)
            {
                // shortcut - assume they mean megabytes
                size = RoundToPowerOf2(size) * 1024;
            }
            else if (size == 256 || size == 512)
            {
                // 256 and 512 are valid for quarter & half-meg boards
            }
            else if (size >= 1024 && size <= 32768)
            {
                // round to the nearest supported capacity
                size = RoundToPowerOf2(size / 1024) * 1024;
            }
            else
            {
                Console.WriteLine("That's a silly amount of memory.");
            }

            size *= 1024;               // in bytes

            if ((int)size != PERQemu.Config.Current.MemorySize)
            {
                PERQemu.Config.Current.MemorySize = (int)size;
                Console.WriteLine("Memory size set to {0}.", PERQemu.Config.Current.MemSizeToString());
            }

            if (!PERQemu.Config.CheckMemory(PERQemu.Config.Current))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
        }

        [Command("configure io board")]
        public void SetIO()
        {
            Console.WriteLine("Configure the IO board.  Valid IO board types are:");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(IOBoardType))));
            Console.WriteLine();
        }

        [Command("configure io board", "Configure the IO board type")]
        public void SetIO(IOBoardType io)
        {
            if (io != PERQemu.Config.Current.IOBoard)
            {
                Console.WriteLine("IO Board type {0} selected.", io);
                PERQemu.Config.Current.IOBoard = io;
            }

            if (!PERQemu.Config.CheckIO(PERQemu.Config.Current))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
        }

        [Command("configure option board")]
        public void SetOptionIO()
        {
            Console.WriteLine("Configure the IO Option board.  Valid IO Option types are:");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(OptionBoardType))));
            Console.WriteLine();
        }

        [Command("configure option board", "Configure the IO Option board type")]
        public void SetOptionIO(OptionBoardType oio)
        {
            if (oio != PERQemu.Config.Current.IOOptionBoard)
            {
                Console.WriteLine("IO Option board type {0} selected.", oio);
                PERQemu.Config.Current.IOOptionBoard = oio;

                // Board changed; reset the selected options to defaults
                if (oio == OptionBoardType.OIO)
                {
                    PERQemu.Config.Current.IOOptions = IOOptionType.Link;
                }
                else if (oio == OptionBoardType.MLO)
                {
                    PERQemu.Config.Current.IOOptions = IOOptionType.SMD;
                }
                else
                {
                    PERQemu.Config.Current.IOOptions = IOOptionType.None;
                }
            }

            if (!PERQemu.Config.CheckOptions(PERQemu.Config.Current))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
        }

        [Command("configure option")]
        public void SetIOOption()
        {
            Console.WriteLine("Configure an IO Option.  Valid IO Option types are:");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(IOOptionType))));
            Console.WriteLine();
        }

        [Command("configure option", "Configure extra features of the IO Option board")]
        public void SetIOOption(IOOptionType opt)
        {
            switch (opt)
            {
                // Always valid; resets the selected options
                case IOOptionType.None:
                    PERQemu.Config.Current.IOOptions = opt;
                    Console.WriteLine("IO options reset.");
                    break;

                // These are valid for OIO and MLO
                case IOOptionType.Link:
                case IOOptionType.LinkTape:
                case IOOptionType.Tape:
                    if (PERQemu.Config.Current.IOOptionBoard == OptionBoardType.OIO ||
                        PERQemu.Config.Current.IOOptionBoard == OptionBoardType.MLO)
                    {
                        PERQemu.Config.Current.IOOptions |= opt;
                        Console.WriteLine("IO option '{0}' selected.", opt);
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
                        Console.WriteLine("* Adding Ethernet option to an NIO changes it to an EIO; reconfiguring.");
                        PERQemu.Config.Current.IOBoard = IOBoardType.EIO;
                        PERQemu.Config.Current.IOOptions &= ~(IOOptionType.Ether);

                        if (PERQemu.Config.Current.IOOptions == IOOptionType.None)
                        {
                            // If that was the only option selected, remove the board
                            PERQemu.Config.Current.IOOptionBoard = OptionBoardType.None;
                            Console.WriteLine("* Removed empty IO Option board.");
                        }
                    }
                    else if (PERQemu.Config.Current.IOOptionBoard == OptionBoardType.OIO)
                    {
                        PERQemu.Config.Current.IOOptions |= opt;
                        Console.WriteLine("IO option '{0}' selected.", opt);
                    }
                    else
                    {
                        Console.WriteLine("That option is incompatible with the selected IO Option board.");
                    }
                    break;
            }

            if (!PERQemu.Config.CheckOptions(PERQemu.Config.Current))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
        }

        [Command("configure display")]
        public void SetDisplay()
        {
            Console.WriteLine("Configures the display device(s) attached to the PERQ.  Valid types are:");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(DisplayType))));
            Console.WriteLine();
        }

        [Command("configure display", "Configure the display device")]
        public void SetDisplay(DisplayType disp)
        {
            if (disp != PERQemu.Config.Current.Display)
            {
                PERQemu.Config.Current.Display = disp;
                Console.WriteLine("{0} display option selected.", disp);
            }

            if (!PERQemu.Config.CheckMemory(PERQemu.Config.Current))
            {
                Console.WriteLine(PERQemu.Config.Current.Reason);
            }
        }

        [Command("configure tablet")]
        public void SetTablet()
        {
            Console.WriteLine("Configures the pointing device(s) attached to the PERQ.  Valid types are:");
            Console.WriteLine(Columnify(Enum.GetNames(typeof(TabletType))));
            Console.WriteLine();
        }

        [Command("configure tablet", "Configure the pointing device(s)")]
        public void SetTablet(TabletType tab)
        {
            if (tab != PERQemu.Config.Current.Tablet)
            {
                PERQemu.Config.Current.Tablet = tab;
                Console.WriteLine("Tablet option {0} selected.", tab);
            }
        }


        private string Columnify(string[] options)
        {
            int width = 12;         // our simulated tab stop
            int col = 0;
            StringBuilder sb = new StringBuilder();

            foreach (var opt in options)
            {
                if (col + width > Console.BufferWidth)
                {
                    sb.Append("\n");
                    col = 0;
                }
                sb.Append(opt.PadLeft(width));
                col += width;
            }
            return sb.ToString();
        }
    }
}
