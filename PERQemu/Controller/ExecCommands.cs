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
        [Command("status", "Show current status of the PERQ")]
        [Command("debug status", "Show current status of the PERQ")]
        public void Status()
        {
            if (PERQemu.Sys == null)
            {
                Console.WriteLine("No PERQ!");
                return;
            }

            Console.WriteLine("Current run state is " + PERQemu.Sys.State);
            Console.WriteLine("Current configuration is " + PERQemu.Sys.Config.Name);

            // todo: check and show floppy and disk drive IsModified
        }

        [Command("power on", "Turn on the configured PERQ!")]
        public void PowerOn()
        {
            PERQemu.Controller.PowerOn();
        }

        // todo: warn if the modified disks aren't saved
        [Command("power off", "Turn off the PERQ")]
        public void PowerOff()
        {
            PERQemu.Controller.PowerOff();
        }

        [Command("go")]
        [Command("debug go")]
        [Command("start", "Start or restart the PERQ")]
        [Command("debug start", "Start or restart the PERQ")]
        public void Start()
        {
            PERQemu.Controller.TransitionTo(RunState.Running);
        }

        [Command("stop", "Stop or pause the PERQ")]
        [Command("debug stop", "Stop or pause the PERQ")]
        public void Stop()
        {
            PERQemu.Controller.TransitionTo(RunState.Paused);
        }

        [Command("reset", "Reset the PERQ")]
        [Command("debug reset", "Reset the PERQ")]
        public void Reset()
        {
            PERQemu.Controller.Reset();
        }

        //
        // Media: Floppies and Hard Disks
        //

        // todo: move the interface to the floppy/hard disk loading and saving
        // to PERQsystem?
        // todo: allow for second hard drive in perq2 models

        [Command("create floppy", "Creates and mounts a new, unformatted floppy disk image")]
        private void CreateFloppyDisk()
        {
            // todo: allow specification of sssd, ssdd, dssd, dsdd
            // all floppies created in imd format by default?
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

        // todo: floppy remembers the path we loaded from so we don't have to?
        // todo: make relative to Disks/ dir

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

        // todo: allow for all supported types, not just shugart
        // todo: allow for multiple units in perq 2 models
        // todo: allow user to define new types here?  or just use built-in ones?
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

        // todo: like floppy, controller should/will remember name of file loaded
        // todo: allow user to specify unit # when appropriate (default 0)
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
            PERQemu.Controller.BootChar = (byte)bootChar;
        }

        [Command("show bootchar", "Show the boot character")]
        private void ShowBootChar()
        {
            Console.Write("Bootchar is ");
            if (PERQemu.Controller.BootChar != 0)
            {
                Console.WriteLine((char)PERQemu.Controller.BootChar);
            }
            else
            {
                Console.WriteLine("unset");
            }
        }

        // todo: on eio/nio, allow for port b... hmm.
        // todo: move the interface to PERQsystem? or provide a hook to Z80system
        // todo: move these to SettingsCommands so they can be saved/reloaded
        [Command("set rs232", "Configure the emulated serial port to use the specified device")]
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

        // todo: allow for png formatter (read Settings.ScreenFormat)
        // todo: make path relative to Output/ (or Settings.OutputDir)
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