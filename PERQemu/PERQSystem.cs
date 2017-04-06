// perqsystem.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// PERQemu is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with PERQemu.  If not, see <http://www.gnu.org/licenses/>.
//

using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;
using System.Text;
using PERQemu.IO;
using PERQemu.CPU;
using PERQemu.Memory;
using PERQemu.Debugger;
using PERQemu.IO.SerialDevices;
using PERQemu.IO.Z80.IOB;
using PERQemu.IO.HardDisk;

namespace PERQemu
{

    public enum RunState
    {
        Run = 0,
        SingleStep,
        RunInst,
        Debug,
        DebugScript,
        Reset,
        Pause,
        Exit,
    }

    /// <summary>
    /// PERQSystem encapsulates basic operation (normal execution, debugging, etc) of the emulated
    /// PERQ system.
    /// </summary>
    public sealed class PERQSystem
    {
        private PERQSystem()
        {
            // Start off debugging
            _state = RunState.Debug;
        }

        public static PERQSystem Instance
        {
            get { return _instance; }
        }

        public void Shutdown()
        {
            Display.Display.Instance.Shutdown();
        }

        public void Execute(string[] args)
        {
            //
            // Evaluate commandline args.  For now, we support only one argument -- a script to execute.
            //
            // TODO: add support for double-clicking on a .phd (hard disk) or .pfd (floppy) image to
            // automatically load up at launch?
            if (args.Length > 0)
            {
                if (args.Length == 1)
                {
                    _state = RunState.DebugScript;
                }
                else
                {
                    Console.WriteLine("usage: PERQemu.exe <scriptFile>");
                }
            }

            bool running = true;
            while (running)
            {
                switch (_state)
                {
                    case RunState.Run:
                    case RunState.SingleStep:
                    case RunState.RunInst:
                        try
                        {
                            RunState nextState = PERQCpu.Instance.Execute(_state);

                            // If we were broken into during execution, we should honor it.
                            if (_state != RunState.Debug)
                            {
                                _state = nextState;
                            }
                        }
                        catch (PowerOffException)
                        {
                            // This is thrown when the microcode tells the PERQ to power down.
                            // Catch here and tell the user.  Go back to debug state.
                            _state = RunState.Debug;
                            _debugMessage = "The PERQ has powered itself off.  Use the 'reset' command to restart the PERQ.";
                        }
                       catch (Exception e)
                        {
                            // The emulation has hit a serious error.
                            // Enter the debugger.
                            _state = RunState.Debug;
                            _debugMessage = String.Format("Break due to internal emulation error: {0}.  System state may be inconsistent.", e.Message);
                        }
                        break;

                    case RunState.Debug:
                        // Enter the debugger.  On return from debugger, switch to the specified state
                        _state = PERQemu.Debugger.Debugger.Instance.Enter(_debugMessage);
                        break;

                    case RunState.DebugScript:
                        _state = PERQemu.Debugger.Debugger.Instance.RunScript(args[0]);
                        break;

                    case RunState.Pause:
                        // Do nothing, just sleep so as not to starve the CPU in a busy loop.
                        System.Threading.Thread.Sleep(10);
                        break;

                    case RunState.Reset:
                        PERQCpu.Instance.Reset();
                        _state = RunState.Debug;
                        _debugMessage = "";
                        break;

                    case RunState.Exit:
                        Shutdown();
                        running = false;
                        break;
                }
            }
        }

        public void Break()
        {
            // User break into debugger
            _state = RunState.Debug;
        }

        /// <summary>
        /// Static.  Allows overriding the default OS boot character.
        /// (This is a key that, when held down at boot time will cause the PERQ microcode to
        /// select a different OS to boot.)
        /// </summary>
        public static byte BootChar
        {
            get { return _bootChar; }
            set { _bootChar = value; }
        }

        #region Debugger Commands

        [DebugFunction("set rs232", "Configures the emulated serial port to use the specified device")]
        private void SetSerialPort(string devName)
        {
            ISerialDevice dev = null;

            try
            {
                if (devName.ToLower().StartsWith("com"))        // TODO: allow /dev/nnn for Unix/Linux!
                {
                    // COM port.  Try to instantiate & assign to system.
                    dev = new PhysicalPort(devName);
                }
                else if (devName.ToLower().StartsWith("rsx:"))
                {
                    // RSX device.
                    dev = new RSXFilePort();
                }
                else
                {
                    throw new ArgumentOutOfRangeException("Invalid device name. Expected COMn: or RSX:");
                }

                Z80System.Instance.SetSerialPort(dev);
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to set rs232 port - {0}", e.Message);
            }
        }

        [DebugFunction("show rs232", "Displays the current rs232 device")]
        private void ShowSerialPort()
        {
            Console.WriteLine("RS232 port is set to {0}", Z80System.Instance.GetSerialPort());
        }

        [DebugFunction("create floppy", "Creates and mounts a new, unformatted floppy disk image")]
        private void CreateFloppyDisk()
        {
            Z80System.Instance.LoadFloppyDisk(null);
            Console.WriteLine("Created.");
        }

        [DebugFunction("load floppy", "Mounts a floppy disk image")]
        private void LoadFloppy(string imagePath)
        {
            try
            {
                Z80System.Instance.LoadFloppyDisk(imagePath);
                Console.WriteLine("Loaded.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("unload floppy", "Unmounts a floppy disk image")]
        private void UnloadFloppy()
        {
            Z80System.Instance.UnloadFloppyDisk();
        }

        [DebugFunction("save floppy", "Saves the current in memory floppy disk to an image file")]
        private void SaveFloppy(string imagePath)
        {
            try
            {
                Z80System.Instance.SaveFloppyDisk(imagePath);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("create harddisk", "Creates and mounts a new, unformatted hard disk image")]
        private void CreateHardDisk()
        {
            ShugartDiskController.Instance.LoadImage(null);
            Console.WriteLine("Created.");
        }

        [DebugFunction("load harddisk", "Mounts an existing hard disk image")]
        private void LoadHardDisk(string imagePath)
        {
            try
            {
                PERQemu.IO.HardDisk.ShugartDiskController.Instance.LoadImage(imagePath);
                Console.WriteLine("Loaded.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to load disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("save harddisk", "Saves the current hard disk to an image file")]
        private void SaveHardDisk(string imagePath)
        {
            try
            {
                ShugartDiskController.Instance.SaveImage(imagePath);
                Console.WriteLine("Saved.");
            }
            catch (Exception e)
            {
                Console.WriteLine("Unable to save disk image {0} - {1}", imagePath, e.Message);
            }
        }

        [DebugFunction("save screenshot", "Saves a screenshot of the current PERQ display in jpg format")]
        private void SaveScreenshot(string filePath)
        {
            string outputPath = filePath + ".jpg";
            try
            {
                Display.Display.Instance.SaveScreenshot(outputPath);
            }
            catch (Exception e)
            {
                Console.WriteLine("Error saving screenshot to {0} - Error: {1}", outputPath, e.Message);
            }
        }

        [DebugFunction("set bootchar", "Sets the boot character (which selects the OS to boot)")]
        private void SetBootChar(char bootChar)
        {
            BootChar = (byte)bootChar;
        }

        [DebugFunction("show bootchar", "Sets the boot character (which selects the OS to boot)")]
        private void ShowBootChar()
        {
            Console.Write("Bootchar is ");
            if (BootChar != 0)
            {
                Console.WriteLine((char)BootChar);
            }
            else
            {
                Console.WriteLine("unset");
            }
        }

        [DebugFunction("save state", "Saves a complete snapshot of the current PERQ state, including disk state, to the specified path.")]
        private void SaveState(string stateFile)
        {
            FileStream stateStream = File.Open(stateFile, FileMode.Create);
            BinaryFormatter formatter = new BinaryFormatter();
            formatter.Serialize(stateStream, PERQCpu.Instance);
            stateStream.Close();
        }

        [DebugFunction("load state", "Restores PERQ state from the specified snapshot.")]
        private void LoadState(string stateFile)
        {
            FileStream stateStream = File.Open(stateFile, FileMode.Open);
            BinaryFormatter formatter = new BinaryFormatter();
            PERQCpu.Instance = (PERQCpu)formatter.Deserialize(stateStream);
            stateStream.Close();
        }

        #endregion

        private RunState _state;
        private string _debugMessage;
        private static byte _bootChar;

        private static PERQSystem _instance = new PERQSystem();
    }
}

