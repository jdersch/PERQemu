// oio.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO
{
    /// <summary>
    /// Represents an Option IO (OIO) card in a PERQ system.
    /// It comes in several configurations, containing hardware for some or all of the following options:
    ///     PERQLink -- parallel interface for microcode debugging;
    ///     Canon Laser Printer -- sends a bitstream to the Canon LBP-10 or CX laser marking engine;
    ///     Ethernet -- 10Mbit Ethernet (3Mbit boards were separate, never a "production" option);
    ///     Streamer -- QIC tape interface.
    /// Eventually this OIO implementation should support dynamic configuration so that individual options
    /// may be selected at runtime!
    /// </summary>
    public sealed class OIO : IIODevice
    {
        public OIO()
        {
            _link = new PERQLink();
            // _canon = new CanonPrinter();
            // _ether = new 10MbEthernet();
            // _streamer = new QICTape();
            Reset();
        }

        public void Reset()
        {
            _link.Reset();

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.IOState, "OIO: Board reset.");
#endif
        }

        public bool HandlesPort(byte ioPort)
        {
            // Lazy slow routine to indicate whether this device handles the given port
            // right now this is set up to assume that if the device handles the port for
            // input then it also handles it for output; this may not be correct.
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
            int retVal = 0xffff;        // Return -1?  Assume IO devices are active low?

            switch (ioPort)
            {
                case 0x06:
                case 0x07:  // lo, hi Ethernet bit counter
                    retVal = 0;   // always return 0 for now?
                    break;

                case 0x0f:  // fake Ethernet status register
                    retVal = (_fakeEtherCSR == 0x20 ? 0x4 : 0x8);   // if reset, return done; else busy?
                    break;

                case 0x20:  // PERQlink input status port
                    retVal = _link.ReadCommandStatus();
                    break;

                case 0x22:  // PERQlink input data port
                    retVal = _link.ReadData();
                    break;

                // case 0x25:
                //  read loopback/diagnostic?

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled OIO Read from port {0:x2}.", ioPort);
#endif
                    break;
            }

            return retVal;
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
                case 0x99:  // fake Ethernet control register
                    _fakeEtherCSR = value;
                    break;

                case 0xa1:  // PERQlink output status
                    _link.WriteCommandStatus(value);
                    break;

                case 0xa3:  // PERQlink output data
                    _link.WriteData(value);
                    break;

                // case 0xa4:
                //  write loopback/diagnostic?
                // case 0xa5:
                //  dummy write/diagnostic?

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Warnings, "Unhandled OIO Write to port {0:x2}, data {1:x4}", ioPort, value);
#endif
                    break;
            }
        }

        public void Clock()
        {
            _link.Clock();
        }

        private PERQLink _link;

        /// <summary>
        /// Complete list of IO ports used by the Option IO boards. At present we
        /// only provide the Link option.
        /// </summary>
        private byte[] _handledPorts =
        {
            // PERQLink ports
            0x20,   // 040 ReadCSR: read PERQLink control status
            0x22,   // 042 ReadData: read PERQLink data
            0x25,   // 045 RdLnkReg: read PERQLink prom register (diagnostic?)
            0xa1,   // 241 WriteCSR: load PERQLink control register
            0xa3,   // 243 WriteData: load PERQLink data
            0xa4,   // 244 WrLnkReg: load PERQLink register (??) (diagnostic?)
            0xa6,   // 246 WtDummy: load test value (??) (diagnostic)

            // Ethernet ports (TODO: not yet implemented)
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
            0x99,   // 231 E10OWrNetCR: load Ethernet control register
            0xd6,   // 326 E10OWrBufHi: load Ethernet buffer address high 4 bits
            0xd7,   // 327 E10OWrHdrHi:   "      "    header    "      "    "
            0xde,   // 336 E10OWrBufLo:   "      "    buffer    "    low 16 bits
            0xdf,   // 337 E10OWrHdrLo:   "      "    header    "      "    "

            // Canon ports (TODO: not yet implemented)
            0x08,   // 010 IntStat: read Canon interrupt status (4 bits)
            0x09,   // 011 MechStat: read Canon mechanical status word
            0x84,   // 204 LineCount: load Canon lines-per-band register[*]
            0x85,   // 205 CanonCntl: load Canon control port (5 bits)
            0x94,   // 224 MargnCntl: load Canon page margin control port
            0x95,   // 225 LeftMar: load Canon blank words register (left margin)
            0x96,   // 226 RightMar: load Canon line length register (right margin)
            0xff,   // 377 (??) dummy write used by Canon driver

            // Streamer ports (TODO: not yet implemented)
            0x0d,   // 015 StrStat: read streamer state
            0x0e,   // 016 StrDataRcv: read streamer data
         // 0x84,   // 204 StrDataSnd: load streamer data[*] Conflicts with Canon!
            0x86    // 206 StrCntrl: load streamer control
        };

        // fake Ethernet control register
        private int _fakeEtherCSR;

    }
}
