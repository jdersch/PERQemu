//
// ExecCommands.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
    /// Implements basic execution controls.
    /// </summary>
    public class ExecCommands
    {
        [Command("status", "Show current status of the PERQ")]
        [Command("debug status", "Show current status of the PERQ")]
        public void Status()
        {
            if (PERQemu.Controller.State == RunState.Off)
            {
                Console.WriteLine("The PERQ is not powered on; no status available.");
                return;
            }

            Console.WriteLine($"Current configuration is {PERQemu.Sys.Config.Name}");
            Console.WriteLine($"Current run state is {PERQemu.Sys.State}");
            Console.WriteLine("The Z80 {0} running", PERQemu.Sys.IOB.Z80System.IsRunning ? "is" : "is not");

            // DEBUG
            PERQemu.Sys.Display.Status();
            PERQemu.Sys.Mouse.Status();
            PERQemu.Sys.VideoController.Status();
        }

        [Command("power on", "Turn on the configured PERQ")]
        public void PowerOn()
        {
            if (PERQemu.Controller.State > RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered on.");
                return;
            }

            PERQemu.Controller.PowerOn();
        }

        [Command("power off", "Turn off the running PERQ")]
        public void PowerOff()
        {
            if (PERQemu.Controller.State == RunState.Off)
            {
                Console.WriteLine("The PERQ is already powered off.");
                return;
            }

            PERQemu.Controller.PowerOff();
        }

        [Command("reset", "Reset the PERQ")]
        [Command("debug reset", "Reset the PERQ")]
        public void Reset()
        {
            PERQemu.Controller.Reset();
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
            PERQemu.Controller.Break();
        }

        [Command("go", "Power on and start the PERQ")]
        [Command("debug go", "Power on and start the PERQ")]
        public void Go()
        {
            // Implicitly power on (shortcut) if not already done
            if (PERQemu.Controller.State == RunState.Off)
            {
                PowerOn();
            }

            // If powered on, start (or resume); otherwise don't bother
            if (PERQemu.Controller.State > RunState.Off)
            {
                Start();
            }
        }


        //
        // Miscellany
        //

        [Command("bootchar", "Set the boot character (selects OS to boot)")]
        void SetBootChar(char key)
        {
            PERQemu.Controller.BootChar = (byte)key;
        }

        [Command("bootchar", "Show the boot character")]
        void ShowBootChar()
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


        [Command("save screenshot", "Save a screenshot of the current PERQ display")]
        void SaveScreenshot()
        {
            var file = string.Format(Settings.ScreenshotTemplate,
                                     DateTime.Now.ToString("yyyyMMdd-HHmmss"),
                                     Paths.GetExtensionForImageFormat(Settings.ScreenshotFormat));

            SaveScreenshot(Paths.BuildOutputPath(file));
        }

        [Command("save screenshot", "Save a named screenshot of the current PERQ display")]
        void SaveScreenshot(string file)
        {
            if (PERQemu.Controller.State == RunState.Off)
            {
                Console.WriteLine("The system is not powered on; screenshot not available.");
                return;
            }

            if (!Path.HasExtension(file))
            {
                file = Path.ChangeExtension(file,
                            Paths.GetExtensionForImageFormat(Settings.ScreenshotFormat));
            }

            try
            {
                PERQemu.Sys.Display.SaveScreenshot(file);
                Console.WriteLine($"Screenshot saved to '{file}'.");
            }
            catch (Exception e)
            {
                Console.WriteLine($"Error saving screenshot to {file}: {e.Message}");
            }
        }


        bool FindCanon(out IO.CanonController ctrl)
        {
            if (PERQemu.Controller.State == RunState.Off)
            {
                Console.WriteLine("The system is not powered on; printer not available.");
                ctrl = null;
                return false;
            }

            // See if the printer is attached
            if (PERQemu.Sys.Config.IOOptionBoard == Config.OptionBoardType.OIO &&
                PERQemu.Sys.Config.IOOptions.HasFlag(Config.IOOptionType.Canon))
            {
                var board = PERQemu.Sys.OIO as IO.OIO;
                ctrl = board.Canon;
                return true;
            }
            //else if (PERQemu.Sys.Config.IOOptionBoard == Config.OptionBoardType.MLO)
            //{
            //    var board = PERQemu.Sys.OIO as IO.MLO;
            //    ctrl = board.Canon;
            //    return true;
            //}

            Console.WriteLine("No Canon laser printer configured in this system.");
            ctrl = null;
            return false;
        }

        [Command("load paper", "Reload the paper tray of the Canon laser printer")]
        void LoadCanon()
        {
            IO.CanonController canon;

            if (FindCanon(out canon))
            {
                // If nothing loaded and no argument, load the default type!
                LoadCanon(canon.Cassette == IO.PaperCode.NoCassette ? Settings.CanonPaperSize : canon.Cassette);
            }
        }

        [Command("load paper", "Load a new tray of paper into the Canon laser printer")]
        void LoadCanon(IO.PaperCode size)
        {
            IO.CanonController canon;

            if (FindCanon(out canon))
            {
                canon.Cassette = size;
                if (size != IO.PaperCode.NoCassette) Console.WriteLine($"{size} paper loaded.");
            }
        }

#if DEBUG
        // debugging
        [Command("hide", "Hide the PERQ display", Discreet = true)]
        void HideDisplay()
        {
            PERQemu.Sys.Display.Hide();
            Console.WriteLine("Sent window hide event.");
        }

        [Command("unhide", "Restore the PERQ display", Discreet = true)]
        void ShowDisplay()
        {
            PERQemu.Sys.Display.Restore();
            Console.WriteLine("Sent window restore event.");
        }

        [Command("resize", "Resize the console display", Discreet = true)]
        void Resize()
        {
            Console.SetWindowSize(80, 25);
            Console.Clear();
            Console.WriteLine("Console window reset.");
        }
#endif

    }
}