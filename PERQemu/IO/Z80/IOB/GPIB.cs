// gpib.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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

using PERQemu.IO.GPIB;

using System;
using System.Collections.Generic;

namespace PERQemu.IO.Z80.IOB
{
    /// <summary>
    /// GPIB provides a high-level (PERQ Z80-level) abstraction for the
    /// PERQ's TI TMS9914 GPIB controller.  Effectively this is the view
    /// of the GPIB controller that the Z80 exposes to the PERQ.
    ///
    /// There is enough additional logic to support communicating with
    /// the GPIB Summagraphics BitPadOne tablet at the moment; this is
    /// fairly sketchy and is far from general-purpose, mostly because
    /// at the moment I know of no PERQ software that even tries to talk
    /// to anything else over GPIB.
    ///
    /// When I move away from the "black box simulation" model for Z80
    /// then this will have to be fleshed out more significantly.
    /// </summary>
    public sealed class GPIB : IZ80Device
    {
        public GPIB()
        {
            Reset();
        }

        public void Reset()
        {
            _messageIndex = 0;
            _cmdIndex = 0;
            _cmdData = new byte[32];
            _registers = new byte[8];
            _busFifo = new Queue<byte>(128);

            ResetGPIB();
        }

        public bool RunStateMachine(PERQtoZ80Message message, byte value)
        {
            bool retVal = false;

            _messageIndex++;

            switch (_messageIndex - 1)
            {
                case 0:
                    // command type
                    _cmdType = (GPIBCommand)value;
                    _cmdIndex = 0;

#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.GPIB, "GPIB command is {0}", _cmdType);
#endif
                    break;

                case 1:
                    // data length
                    _cmdLength = value;

#if TRACING_ENABLED
                    if (Trace.TraceOn) Trace.Log(LogType.GPIB, "GPIB command length is {0}", _cmdLength);
#endif
                    break;

                default:
                    // Command data
                    _cmdData[_cmdIndex] = value;

                    switch (_cmdType)
                    {
                        case GPIBCommand.WriteRegisters:
                            // From perqz80.doc:
                            //
                            // The [WriteRegisters] command allows PERQ to directly write into the
                            // TI-9914 registers.  In this command, each pair of data bytes is
                            // treated  as the first being a register number, and the second a
                            // data value.
                            if ((_cmdIndex % 2) != 0)
                            {
                                WriteRegisters((GPIBWriteRegister)_cmdData[_cmdIndex-1], value);
                            }
                            break;

                        default:
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.GPIB, "Command data {0}:{1:x2}", _cmdIndex, value);
#endif
                            break;
                    }

                    _cmdIndex++;

                    //
                    // End of command.
                    //
                    if (_cmdIndex >= _cmdLength)
                    {
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.GPIB, "GPIB command ({0}, length {1}) is done.", _cmdType, _cmdLength);
#endif
                        retVal = true;
                        _messageIndex = 0;
                    }
                    break;
            }

            return retVal;
        }

        public void Poll(ref Queue<byte> fifo)
        {
            // If we are not listening, we return nothing and clear our FIFO
            if (!_listen)
            {
                _busFifo.Clear();
            }
            else
            {
                GPIBBus.Instance.Poll(ref _busFifo);

                int dataCount = _busFifo.Count;

                if (dataCount > 0)  // TODO: handle case where count > 255...
                {
                    fifo.Enqueue(Z80System.SOM);
                    fifo.Enqueue((byte)Z80toPERQMessage.GPIBData);
                    fifo.Enqueue((byte)dataCount);

                    for (int i = 0; i < dataCount ; i++)
                    {
                        fifo.Enqueue(_busFifo.Dequeue());
                    }
                }
            }
        }

        /// <summary>
        /// Writes incoming data to the GPIB registers
        /// </summary>
        /// <param name="value"></param>
        private void WriteRegisters(GPIBWriteRegister register, byte value)
        {
            // Save the value
            _registers[(int)register] = value;

            switch (register)
            {
                case GPIBWriteRegister.AuxiliaryCommand:
                    AuxiliaryCommand cmd = (AuxiliaryCommand)(value & 0x1f);
                    bool cs = (value & 0x80) != 0;
#if TRACING_ENABLED
                     if (Trace.TraceOn)
                         Trace.Log(LogType.GPIB, "GPIB Auxiliary command is {0}, cs {1}", cmd, cs);
#endif
                    DispatchAuxiliaryCommand(cmd, cs);
                    break;

                default:
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.GPIB, "GPIB Write Register {0} with {1:x2}", register, value);
#endif
                    break;
            }
        }

        private void DispatchAuxiliaryCommand(AuxiliaryCommand cmd, bool cs)
        {
            switch(cmd)
            {
                case AuxiliaryCommand.swrst:
                    ResetGPIB();
                    break;

                case AuxiliaryCommand.lon:
                    // TODO: are lon and ton mutually exclusive?
                    _listen = cs;
                    _talk = false;
                    break;

                case AuxiliaryCommand.ton:
                    _listen = false;
                    _talk = cs;
                    break;
            }
        }

        public void GetStatus(ref Queue<byte> fifo)
        {
            // Return current state of the 9914 chip:
            //  <SOM><17><6><reg1>..<reg6>
            // Register values are returned in the order defined by perqz80.doc/v87.z80
            fifo.Enqueue(Z80System.SOM);
            fifo.Enqueue((byte)Z80toPERQMessage.GPIBStatus);
            fifo.Enqueue(0x6);
            fifo.Enqueue(_registers[(int)GPIBReadRegister.InterruptStatus0]);
            fifo.Enqueue(_registers[(int)GPIBReadRegister.AddressSwitch]);
            fifo.Enqueue(_registers[(int)GPIBReadRegister.AddressStatus]);
            fifo.Enqueue(_registers[(int)GPIBReadRegister.CommandPassThrough]);
            fifo.Enqueue(_registers[(int)GPIBReadRegister.InterruptStatus1]);
            fifo.Enqueue(_registers[(int)GPIBReadRegister.BusStatus]);

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Tablet, "--> GPIB GetStatus()");
#endif
        }

        private void ResetGPIB()
        {
            _listen = false;
            _talk = false;
        }

        private enum GPIBCommand
        {
            WriteData = 1,
            WriteDataEOI = 2,
            WriteRegisters = 3
        }

        /// <summary>
        /// These enumerations come from perqz80.doc, and v87.z80.
        /// It'll take some study of the docs and PERQ code to see how the
        /// read registers are used; we can probably fake it based on the
        /// values written.  Kind of a hack.  But I started all this expecting
        /// that maybe it would help explain why Accent S4 isn't tracking the
        /// pointer, but it looks like maybe the PERQ never asks for status
        /// (from anything but the floppy) by default.  Or we're just not
        /// handling the GetStatus parsing correctly at all.  But it's never
        /// called.  SO GetStatus() here is never called.  Hmm.
        /// </summary>
        private enum GPIBWriteRegister
        {
            InterruptMask0 = 0,     // GPIIM0
            AddressRegister = 1,    // GPIADD
            ParallelPoll = 3,       // GPIPP
            InterruptMask1 = 4,     // GPIIM1
            SerialPoll = 5,         // GPISP
            AuxiliaryCommand = 6    // GPIAUX
        }

        private enum GPIBReadRegister
        {
            InterruptStatus0 = 0,   // GPIIS0
            AddressSwitch = 1,      // GPIASW
            AddressStatus = 2,      // GPIAS
            CommandPassThrough = 3, // GPICPT
            InterruptStatus1 = 4,   // GPIIS1
            BusStatus = 6           // GPIBS
        }

        /// <summary>
        /// These enumerations come straight from the TMS9941A datasheet.
        /// </summary>
        private enum AuxiliaryCommand
        {
            swrst =     0x00,       // Software Reset
            dacr =      0x01,       // Release DAC holdoff
            rhdf =      0x02,       // Release RFD holdoff
            hdfa =      0x03,       // Holdoff on all data
            hdfe =      0x04,       // Holdoff on EOI only
            nbaf =      0x05,       // New byte available false
            fget =      0x06,       // Force group execute trigger
            rtl =       0x07,       // Return to local
            feoi =      0x08,       // Send EOI with next byte
            lon =       0x09,       // Listen only
            ton =       0x0a,       // Talk only
            gts =       0x0b,       // Go to standby
            tca =       0x0c,       // Take control asynchronously
            tcs =       0x0d,       // Take control synchronously
            rpp =       0x0e,       // Request parallel poll
            sic =       0x0f,       // Send interface clear
            sre =       0x10,       // Send remote enable
            rqc =       0x11,       // Request control
            rlc =       0x12,       // Release control
            dai =       0x13,       // Disable all interrupts
            pts =       0x14,       // Pass through next secondary
            stdl =      0x15,       // Short TI setting time
            shdw =      0x16,       // Shadow handshake
            vstdl =     0x17,       // Very short T1 delay
            rsv2 =      0x18,       // Request Service Bit 2
        }

        // GPIB-specific flags
        private bool _listen;
        private bool _talk;

        private byte[] _registers;

        private Queue<byte> _busFifo;
        private byte[] _cmdData;
        private int _cmdIndex = 0;
        private GPIBCommand _cmdType;
        private int _cmdLength;
        private int _messageIndex;
    }
}
