//
// ExecCommands.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
    /// Implements basic execution controls.
    /// </summary>
    public class ExecCommands
    {
        [Command("status", "Show current status of the PERQ")]
        [Command("debug status", "Show current status of the PERQ")]
        public void Status()
        {
            if (PERQemu.Controller.State == RunState.Unavailable)
            {
                Console.WriteLine("The PERQ is not powered on; no status available.");
                return;
            }

            Console.WriteLine("Current configuration is " + PERQemu.Sys.Config.Name);
            Console.WriteLine("Current run state is " + PERQemu.Sys.State);

            // DEBUG
            PERQemu.Sys.Display.Status();
            PERQemu.Sys.Mouse.Status();
            PERQemu.Sys.VideoController.Status();
        }

        [Command("power on", "Turn on the configured PERQ!")]
        public void PowerOn()
        {
            if (PERQemu.Controller.State > RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered on.");
                return;
            }

            PERQemu.Controller.PowerOn();
        }

        [Command("power off", "Turn off the PERQ")]
        public void PowerOff()
        {
            if (PERQemu.Controller.State <= RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered off.");
                return;
            }

            PERQemu.Controller.TransitionTo(RunState.Off);
        }

        [Command("reset", "Reset the PERQ")]
        [Command("debug reset", "Reset the PERQ")]
        public void Reset()
        {
            PERQemu.Controller.Reset();
        }

        [Command("go", "Power on and start the PERQ")]
        [Command("debug go", "Power on and start the PERQ")]
        public void Go()
        {
            // Implicitly power on (shortcut) if not already done
            if (PERQemu.Controller.State == RunState.Unavailable)
            {
                PERQemu.Controller.PowerOn();
            }

            Start();
        }

        [Command("start", "Run the PERQ")]
        [Command("debug start", "Run the PERQ")]
        public void Start()
        {
            PERQemu.Controller.TransitionTo(RunState.Running);
        }

        [Command("stop", "Stop or pause the PERQ")]
        [Command("debug stop", "Stop or pause the PERQ")]
        public void Stop()
        {
            // A quiet no-op if the machine isn't on...
            if (PERQemu.Controller.State > RunState.Off)
            {
                PERQemu.Controller.TransitionTo(RunState.Paused);
                PERQemu.Sys.PrintStatus();
            }
        }


        //
        // Miscellany
        //

        // fixme crap.  have to fix the ambiguity issue in the commandprompt where an
        // exact match that's a substring of a longer match is "ambiguous" even
        // when the user types a space or tab -- "set" and "settings" conflict, but
        // somehow it's not accepting the "set bootchar" even when typing it in full...
        [Command("bootchar", "Set the boot character (selects the OS to boot)")]
        private void SetBootChar(char ch)
        {
            PERQemu.Controller.BootChar = (byte)ch;
        }

        [Command("bootchar", "Show the boot character")]
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
        //[Command("set rs232", "Configure the emulated serial port to use the specified device")]
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

        //[Command("show rs232", "Displays the current rs232 device")]
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