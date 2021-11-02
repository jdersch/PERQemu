//
// ExecCommands.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.SerialDevices;

namespace PERQemu
{
    /// <summary>
    /// Implements basic execution controls: starting, stopping, loading and
    /// saving ejectable media (floppies or tapes), screenshots, etc.
    /// </summary>
    public class ExecCommands
    {
        //
        // load::<media> -- insert removable media
        // save::<media> -- save removable media
        // unload::<media> -- remove " ".  alias: eject
        // create::<media> -- create new blank, formatted media
        // 

        [Command("power on", "Turn on the configured PERQ!")]
        public void PowerOn()
        {
            PERQemu.Controller.PowerOn();
        }

        [Command("power off", "Turn off the PERQ")]
        public void PowerOff()
        {
            PERQemu.Controller.PowerOff();
        }

        [Command("go")]
        [Command("start", "Start or restart the PERQ")]
        public void Start()
        {
            Console.WriteLine("Starting the PERQ...");

        }

        [Command("stop", "Stop or pause the PERQ")]
        public void Stop()
        {
            Console.WriteLine("Stopping the PERQ...");

        }

        [Command("reset", "Reset the PERQ")]
        public void Reset()
        {
            Console.WriteLine("Resetting the PERQ...");

        }

        //
        // Media: Floppies and Hard Disks
        //

        [Command("create floppy", "Creates and mounts a new, unformatted floppy disk image")]
        private void CreateFloppyDisk()
        {
            PERQemu.Sys.IOB.Z80System.LoadFloppyDisk(null);
            Console.WriteLine("Created.");
        }

        [Command("load floppy", "Mounts a floppy disk image")]
        private void LoadFloppy(string imagePath)
        {
            try
            {
                PERQemu.Sys.IOB.Z80System.LoadFloppyDisk(imagePath);
                Console.WriteLine("Loaded.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [Command("unload floppy", "Unmounts a floppy disk image")]
        private void UnloadFloppy()
        {
            PERQemu.Sys.IOB.Z80System.UnloadFloppyDisk();
        }

        [Command("save floppy", "Saves the current in memory floppy disk to an image file")]
        private void SaveFloppy(string imagePath)
        {
            try
            {
                PERQemu.Sys.IOB.Z80System.SaveFloppyDisk(imagePath);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [Command("create harddisk", "Creates and mounts a new, unformatted hard disk image")]
        private void CreateHardDisk()
        {
            PERQemu.Sys.IOB.ShugartDiskController.LoadImage(null);
            Console.WriteLine("Created.");
        }

        [Command("load harddisk", "Mounts an existing hard disk image")]
        private void LoadHardDisk(string imagePath)
        {
            try
            {
                PERQemu.Sys.IOB.ShugartDiskController.LoadImage(imagePath);
                Console.WriteLine("Loaded.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        // TODO:  HAVE TO DEAL WITH MULTIPLE UNITS, other drive types, etc
        [Command("save harddisk", "Save the current hard disk to an image file")]
        private void SaveHardDisk(string imagePath)
        {
            try
            {
                PERQemu.Sys.IOB.ShugartDiskController.SaveImage(imagePath); // FIXME
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }


        //
        // Miscellany
        //

        [Command("set bootchar", "Set the boot character (selects the OS to boot)")]
        private void SetBootChar(char bootChar)
        {
            PERQemu.Sys.BootChar = (byte)bootChar;
        }

        [Command("show bootchar", "Show the boot character")]
        private void ShowBootChar()
        {
            Console.Write("Bootchar is ");
            if (PERQemu.Sys.BootChar != 0)
            {
                Console.WriteLine(PERQemu.Sys.BootChar);
            }
            else
            {
                Console.WriteLine("unset");
            }
        }


        [Command("set rs232", "Configures the emulated serial port to use the specified device")]
        private void SetSerialPort(string devName)
        {
            ISerialDevice dev = null;

            try
            {
                if (!PERQemu.HostIsUnix && devName.StartsWith("com", StringComparison.InvariantCultureIgnoreCase))
                {
                    // COM port.  Try to instantiate & assign to system.
                    dev = new PhysicalPort(devName);
                }
                else if (PERQemu.HostIsUnix && devName.StartsWith("/dev", StringComparison.InvariantCultureIgnoreCase))
                {
                    // Unix device path.  Try to instantiate & assign to system.
                    dev = new PhysicalPort(devName);
                }
                else if (devName.Equals("rsx:", StringComparison.InvariantCultureIgnoreCase))
                {
                    // RSX device.
                    dev = new RSXFilePort();
                }
                else
                {
                    throw new ArgumentOutOfRangeException("Invalid device name. Expected /dev/path, COMn: or RSX:");
                }

                PERQemu.Sys.IOB.Z80System.SetSerialPort(dev);
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to set rs232 port - {0}", e.Message);
            }
        }

        [Command("show rs232", "Displays the current rs232 device")]
        private void ShowSerialPort()
        {
            Console.WriteLine("RS232 port is set to {0}", PERQemu.Sys.IOB.Z80System.GetSerialPort());
        }


        [Command("save screenshot", "Save a screenshot of the current PERQ display")]
        private void SaveScreenshot(string filePath)
        {
            string outputPath = filePath + ".jpg";  // FIXME

            try
            {
                PERQemu.Sys.Display.SaveScreenshot(outputPath);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error saving screenshot to {0} - Error: {1}", outputPath, e.Message);
            }
        }

    }
}