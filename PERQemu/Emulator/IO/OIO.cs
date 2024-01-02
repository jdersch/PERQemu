//
// OIO.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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

using PERQmedia;
using PERQemu.Config;
using PERQemu.IO.Network;
using PERQemu.IO.TapeDevices;

namespace PERQemu.IO
{
    /// <summary>
    /// Represents an Option IO (OIO) card in a PERQ system.
    /// It comes in several configurations, containing hardware for some or all
    /// of the following options:
    ///     PERQLink -- parallel interface for microcode debugging;
    ///     Canon    -- bitstream interface to the LBP-10 or CX laser marking engine;
    ///     Ethernet -- 10Mbit Ethernet interface;
    ///     Streamer -- QIC tape interface.
    /// OIO configures itself from the Configuration record at runtime.
    /// </summary>
    public sealed class OIO : OptionBoard
    {
        static OIO()
        {
            _name = "OIO";
            _desc = "Option IO Board";
        }

        public OIO(PERQSystem system) : base(system)
        {
            // Always present
            _link = new PERQLink();
            RegisterPorts(_handledPorts);

            if (_sys.Config.IOOptions.HasFlag(IOOptionType.Ether))
            {
                if (string.IsNullOrEmpty(Settings.EtherDevice) || Settings.EtherDevice == "null")
                {
                    // A minimal interface to let Accent boot properly
                    _ethernet = new NullEthernet(_sys);
                }
                else
                {
                    try
                    {
                        // The real deal!
                        _ethernet = new Ether10MbitController(_sys);
                    }
                    catch (UnimplementedHardwareException e)
                    {
                        // Failed to open - bad device, or no permissions?
                        Log.Warn(Category.All, "{0}; no Ethernet available.", e.Message);

                        // Fall back to the fake one and continue
                        _ethernet = new NullEthernet(_sys);
                    }
                }
                RegisterPorts(_etherPorts);
            }

            if (_sys.Config.IOOptions.HasFlag(IOOptionType.Tape))
            {
                _streamer = new QICTapeController(_sys.Config.IOOptionBoard);
                RegisterPorts(_streamerPorts);
            }

            if (_sys.Config.IOOptions.HasFlag(IOOptionType.Canon))
            {
                _canon = new CanonController(_sys);
                RegisterPorts(_canonPorts);
            }
        }

        public INetworkController Ether => _ethernet;
        public QICTapeController Streamer => _streamer;
        public CanonController Canon => _canon;

        public override void Reset()
        {
            _link.Reset();

            if (_ethernet != null) _ethernet.Reset();
            if (_streamer != null) _streamer.Reset();
            if (_canon != null) _canon.Reset();

            base.Reset();
        }

        /// <summary>
        /// If the Streamer option is configured, create the Sidewinder and hook
        /// that to the OIO board, then load the media if a path is given.
        /// </summary>
        public override StorageDevice LoadTape(Drive drive)
        {
            // Sanity checks
            if (_streamer == null)
                throw new InvalidOperationException($"Request to load {drive.Type} but no drive attached");

            if (drive.Type != DeviceType.TapeQIC)
                throw new InvalidOperationException($"OIO can't load tape type {drive.Type}");

            // Attach the drive/controller to the PERQ
            var controller = new Sidewinder(_sys.Scheduler);

            _streamer.AttachDrive((uint)drive.Unit, controller);

            // If a media path given, tell the Sidewinder to load it
            if (!string.IsNullOrEmpty(drive.MediaPath))
            {
                controller.Media.LoadFrom(drive.MediaPath);
            }

            return controller.Media;
        }

        /// <summary>
        /// Read from the given port.
        /// </summary>
        public override int IORead(byte port)
        {
            switch (port)
            {
                case 0x06:      // Ethernet bit counter registers
                case 0x07:
                    if (_ethernet != null)
                    {
                        return _ethernet.ReadRegister(port);
                    }
                    break;

                case 0x08:      // Read Canon interrupt status
                case 0x09:      // Read Canon mechanical status
                    if (_canon != null)
                    {
                        return _canon.ReadStatus(port);
                    }
                    break;

                case 0x0f:
                    if (_ethernet != null)
                    {
                        return _ethernet.ReadStatus();
                    }
                    break;

                case 0x0d:      // Read streamer status
                    if (_streamer != null)
                    {
                        return _streamer.ReadStatus();
                    }
                    break;

                case 0x0e:      // Read streamer data
                    if (_streamer != null)
                    {
                        return _streamer.ReadData();
                    }
                    break;

                case 0x20:      // PERQlink input status port
                    return _link.ReadCommandStatus();

                case 0x22:      // PERQlink input data port
                    return _link.ReadData();

                default:
                    // Log a warning for invalid or unknown port read attempts
                    Log.Warn(Category.IO, "Unhandled OIO Read from port {0:x2}", port);
                    break;
            }

            // Unhandled things assume this?
            return 0xffff;
        }


        /// <summary>
        /// Write to the given port.
        /// </summary>
        /// <remarks>
        /// There is a port conflict between the streamer and the Canon printer
        /// on port 0x84 (oct 204) that I'm not sure how to resolve.  Either you
        /// simply cannot use the printer when the streamer is running OR there
        /// is some other mechanism to prevent conflicts?  I haven't seen any
        /// mention in the microcode or higher-level software.  Hmmm. :-|
        /// 
        /// For now, any write to the port goes to either or both of the devices
        /// depending on configuration.  This conflict is only likely to be a
        /// problem under Accent (multitasking) and not on POS.  I assume (but
        /// don't know) that PNX offers support for these peripherals and could
        /// also have a conflict if both are accessed simultaneously?
        /// </remarks>
        public override void IOWrite(byte port, int value)
        {
            switch (port)
            {
                case 0x84:      // Load Streamer data AND/OR Canon line count
                    if (_streamer != null)
                    {
                        _streamer.LoadRegister(port, value);
                    }

                    if (_canon != null)
                    {
                        _canon.LoadRegister(port, value);
                    }
                    break;

                case 0x85:      // Load Canon control port
                    if (_canon != null)
                    {
                        _canon.LoadCommand(value & 0x1f);
                    }
                    break;

                case 0x86:      // Load Streamer control
                    if (_streamer != null)
                    {
                        _streamer.LoadRegister(port, value);
                    }
                    break;

                case 0x88:
                case 0x89:
                case 0x8a:
                case 0x8c:
                case 0x8d:
                case 0x8e:
                case 0x90:
                case 0x91:
                case 0x92:
                case 0x93:      // Load Ethernet registers
                    if (_ethernet != null)
                    {
                        _ethernet.LoadRegister(port, value);
                    }
                    break;

                case 0x99:      // Load Ethernet control register
                    if (_ethernet != null)
                    {
                        _ethernet.LoadCommand(value);
                    }
                    break;

                case 0x94:      // Load Canon page margin control register
                case 0x95:      // Load Canon left margin register
                case 0x96:      // Load Canon line length register
                    if (_canon != null)
                    {
                        _canon.LoadRegister(port, value);
                    }
                    break;

                case 0xa1:      // PERQlink output status
                    _link.WriteCommandStatus(value);
                    break;

                case 0xa3:      // PERQlink output data
                    _link.WriteData(value);
                    break;

                default:
                    Log.Warn(Category.IO, "Unhandled OIO Write to port {0:x2}, data {1:x4}", port, value);
                    break;
            }
        }

        public override uint Clock()
        {
            _link.Clock();

            return 1;
        }

        public override void Shutdown()
        {
            // Make sure our Ethernet (if configured) is properly shut down!
            if (_ethernet != null && _ethernet.GetType() == typeof(Ether10MbitController))
            {
                _ethernet.Shutdown();
            }

            // If we stopped mid-print, tell Canon to close the file
            if (_canon != null)
            {
                _canon.Shutdown();
            }

            base.Shutdown();
        }

        /// <summary>
        /// Complete list of IO ports used by the Option IO boards.  At present
        /// we only emulate the Link and Streamer options.
        /// </summary>
        byte[] _handledPorts =
        {
            // PERQLink ports
            0x20,   // 040 ReadCSR: read PERQLink control status
            0x22,   // 042 ReadData: read PERQLink data
            0x25,   // 045 RdLnkReg: read PERQLink prom register (diagnostic?)
            0xa1,   // 241 WriteCSR: load PERQLink control register
            0xa3,   // 243 WriteData: load PERQLink data
            0xa4,   // 244 WrLnkReg: load PERQLink register (??) (diagnostic?)
            0xa6    // 246 WtDummy: load test value (??) (diagnostic)
        };

        byte[] _etherPorts =
        {
            // Ethernet ports
            0x06,   // 006 E10ORdBCLow: read Ethernet bit count low byte
            0x07,   // 007 E10ORdBCHgh:   "      "     "    "   high byte
            0x0f,   // 017 E10ORdNetCR:   "      "    control register
            0x88,   // 210 E10OWrUSCR: load Ethernet usec clock control
            0x89,   // 211 E10OWrUSHgh:  "     "       "    "   high byte
            0x8a,   // 212 E10OWrUSLow:  "     "       "    "   low byte
            0x8c,   // 214 E10OWrBCCR: load Ethernet bit count control
            0x8d,   // 215 E10OWrBCHgh:  "      "     "    "   high byte
            0x8e,   // 216 E10OWrBCLow:  "      "     "    "   low byte
            0x90,   // 220 E10OWrNetAddr: load Ethernet address low word
            0x91,   // 221 E10OWrMC1: load Ethernet multicast register (Grp1<<8|Cmd)
            0x92,   // 222 E10OWrMC2:   "      "        "         "    (Grp3<<8|Grp2)
            0x93,   // 223 E10OWrMC3:   "      "        "         "    (Grp5<<8|Grp4)
            0x99    // 231 E10OWrNetCR: load Ethernet control register
        };

        byte[] _canonPorts =
        {
            // Canon ports
            0x08,   // 010 IntStat: read Canon interrupt status (4 bits)
            0x09,   // 011 MechStat: read Canon mechanical status word
            0x84,   // 204 LineCount: load Canon lines-per-band register[*]
            0x85,   // 205 CanonCntl: load Canon control port (5 bits)
            0x94,   // 224 MargnCntl: load Canon page margin control port
            0x95,   // 225 LeftMar: load Canon blank words register (left margin)
            0x96    // 226 RightMar: load Canon line length register (right margin)
        };

        byte[] _streamerPorts =
        {
            // Streamer ports
            0x0d,   // 015 StrStat: read streamer state
            0x0e,   // 016 StrDataRcv: read streamer data
            0x84,   // 204 StrDataSnd: load streamer data[*] Conflicts with Canon!
            0x86    // 206 StrCntrl: load streamer control
        };


        // Attached devices
        PERQLink _link;
        INetworkController _ethernet;
        QICTapeController _streamer;
        CanonController _canon;
    }
}
