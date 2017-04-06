// iob.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.Text;
using PERQemu.IO.Z80.IOB;
using PERQemu.CPU;

namespace PERQemu.IO
{
    /// <summary>
    /// Represents an IOB card in a PERQ1 system.
    /// This contains hardware for the Shugart disk controller and a Z80
    /// for controlling low-speed devices.
    /// </summary>
    [Serializable]
    public class IOB : IIODevice
    {
        public IOB()
        {
            _hardDiskController = HardDisk.ShugartDiskController.Instance;
            _z80System = Z80System.Instance;
            Reset();
        }

        public void Reset()
        {
            _hardDiskController.Reset();
            _z80System.Reset();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.IOState, "IOB: Board reset.");
#endif
        }

        public bool HandlesPort(byte ioPort)
        {
            // Lazy slow routine to indicate whether this device handles the given port
            // right now this is set up to assume that if the device handles the port for
            // input then it also handles it for output; this is not correct, but hey.
            for (int i = 0; i < _handledPorts.Length; i++)
            {
                if (ioPort == _handledPorts[i])
                {
                    return true;
                }
            }

            return false;
        }

        /// <summary>
        /// Does a read from the given port
        /// </summary>
        /// <param name="ioPort"></param>
        /// <returns></returns>
        public int IORead(byte ioPort)
        {
            switch (ioPort)
            {
                case 0x40:  // Read disk status
                    return _hardDiskController.ReadStatus();

                case 0x46:  // Read Z80 data
                    return _z80System.ReadData();

                //case 0x55:  // Read Z80 status -- not used in IOB/CIO -- remove
                //    return _z80System.ReadStatus();

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled IOB Read from port {0:x2}", ioPort);
#endif
                    return 0xff;
            }
        }

        /// <summary>
        /// Does a write to the given port
        /// </summary>
        /// <param name="ioPort"></param>
        /// <param name="value"></param>
        public void IOWrite(byte ioPort, int value)
        {
            switch (ioPort)
            {
                case 0xc1:  // Shugart command/control register & Z80 status register
                    _hardDiskController.LoadCommandRegister(value);
                    Z80System.Instance.LoadStatus(value);
                    break;

                case 0xc2:  // Shugart Head register
                    _hardDiskController.LoadHeadRegister(value);
                    break;

                case 0xc7:  // Z80 data port
                    Z80System.Instance.LoadData(value);
                    break;

                case 0xc8:  // Shugart Cylinder/Sector register
                    _hardDiskController.LoadCylSecRegister(value);
                    break;

                case 0xc9:  // Shugart File SN Low Register
                    _hardDiskController.LoadSerialLowRegister(value);
                    break;

                case 0xca:  // Shugart File SN High register
                    _hardDiskController.LoadSerialHighRegister(value);
                    break;

                case 0xcb:  // Shugart Block Number register
                    _hardDiskController.LoadBlockRegister(value);
                    break;

                case 0xd0:  // Shugart Data Buffer Address High register
                    _hardDiskController.LoadDataBufferAddrHighRegister(value);
                    break;

                case 0xd1:  // Shugart Header Address High register
                    _hardDiskController.LoadHeaderAddrHighRegister(value);
                    break;

                // 0xd4,d5,dc,dd: load DMA registers -- Canon inteface

                case 0xd8:  // Shugart Data Buffer Address Low register
                    _hardDiskController.LoadDataBufferAddrLowRegister(value);
                    break;

                case 0xd9:  // Shugart Header Address low register
                    _hardDiskController.LoadHeaderAddrLowRegister(value);
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled IOB Write to port {0:x2}, data {1:x4}", ioPort, value);
#endif
                    break;
            }
        }

        /// <summary>
        /// Clocks subdevices
        /// </summary>
        public void Clock()
        {
            _hardDiskController.Clock();
            Z80System.Instance.Clock();
        }

        private HardDisk.ShugartDiskController _hardDiskController;
        private Z80System _z80System;

        /// <summary>
        /// Ports handled by the IOB board.
        /// </summary>
        private byte[] _handledPorts =
        {
            // Z80
            0x46,       // 106 CioZ80In: read Z80 input
            0xc1,       // 301 CioZ80Start: load Z80 control register
            0xc7,       // 307 CioZ80Out: load Z80 output

            // Hard disk
            0x40,       // 100 DskStat: read disk status register
            0xc2,       // 302 DskHead: load Shugart head register
            0xc8,       // 310 DskCylSec: load Shugart cylinder/sector register
            0xc9,       // 311 DskFSNlo: (FileL) load Shugart file serial # low bits
            0xca,       // 312 DskFSNhi: (FileH)   "     "      "     "    high bits
            0xcb,       // 313 DskLBN: (Block) load Shugart logical block #
            0xd0,       // 320 DatAdrH: load Shugart data buffer address high bits
            0xd1,       // 321 CWAdrH:    "     "    header  "      "      "    "
            0xd8,       // 330 DatAdrL: load Shugart data buffer address low bits
            0xd9,       // 331 CWAdrL:    "     "    header  "      "     "    "

            // DMA (TODO: only used by Canon?)
            0xd4,       // 324 DMAhi: load DMA data buffer address high bits (Only used by Canon?)
            0xd5,       // 325 HDRhi:   "   "  header  "      "      "    "           "
            0xdc,       // 334 DMAlo: load DMA data buffer address low bits (Only used by Canon?)
            0xdd        // 335 HDRlo:   "   "  header  "      "     "    "           "
        };
    }
}
