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
								WriteRegisters((GPIBWriteRegister)_cmdData[_cmdIndex - 1], value);
							}
							break;

						default:
#if TRACING_ENABLED
							if (Trace.TraceOn)
								Trace.Log(LogType.GPIB, "Command data {0}:{1:x2}", _cmdIndex, value);
#endif
							DispatchGroupCommand(value);
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

		/// <summary>
		/// Polls all the simulated GPIB devices, to read any data from the current
		/// active talker.  Or maybe we should just poll the talker and save the loop
		/// overhead (with only one attached device for now, that's pretty minimal).
		/// </summary>
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

					for (int i = 0; i < dataCount; i++)
					{
						fifo.Enqueue(_busFifo.Dequeue());
					}
				}
			}
		}

		/// <summary>
		/// Writes incoming data to the GPIB registers.
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

		/// <summary>
		/// Parses GPIB Auxiliary Commands - for now just a small subset of them.
		/// </summary>
		private void DispatchAuxiliaryCommand(AuxiliaryCommand cmd, bool cs)
		{
			switch (cmd)
			{
				case AuxiliaryCommand.swrst:
					ResetGPIB();
					break;

				case AuxiliaryCommand.lon:
					_listen = cs;
					break;

				case AuxiliaryCommand.ton:
					_talk = cs;
					break;
			}
		}

		/// <summary>
		/// Interprets command data bytes which contain group commands (including the setting
		/// of talker/listener addresses).  These are "broadcast" to all devices on the bus,
		/// which essentially acts as an enable/disable for the BitPad.  If we ever implement
		/// another device, like a printer or a big ol' HP disk drive, this is how they'll
		/// share the bus.  I think.  If it lets Accent track the damn mouse I'll be thrilled...
		/// </summary>
		private void DispatchGroupCommand(byte value)
		{
			RemoteCommandGroup grp = (RemoteCommandGroup)((value & 0x60) >> 5);
			byte data = (byte)(value & 0x1f);

			switch (grp)
			{
				case RemoteCommandGroup.AddressedCommandGroup:
					// Nothing to do, really?  But log whatever comes in for debugging?
					switch ((AddressCommands)data)
					{
						case AddressCommands.dcl:
						case AddressCommands.gtl:
						case AddressCommands.gxt:
						case AddressCommands.llo:
						case AddressCommands.ppc:
						case AddressCommands.ppu:
						case AddressCommands.sdc:
						case AddressCommands.spd:
						case AddressCommands.spe:
						case AddressCommands.tct:
#if TRACING_ENABLED
							if (Trace.TraceOn)
								Trace.Log(LogType.GPIB, "GPIB Addressed Command received {0}", data);
#endif
							break;

						default:
#if TRACING_ENABLED
							if (Trace.TraceOn)
								Trace.Log(LogType.GPIB, "GPIB Unknown Addressed Command received {0}", data);
#endif
							break;
					}
					break;

				case RemoteCommandGroup.ListenAddressGroup:
					// Set the listener address
					if (data == 0x1f)
					{
#if TRACING_ENABLED
						if (Trace.TraceOn)
							Trace.Log(LogType.GPIB, "GPIB Listen Address Group 'unlisten' command received");
#endif
						_listen = false;    // ???
					}
					else
					{
#if TRACING_ENABLED
						if (Trace.TraceOn)
							Trace.Log(LogType.GPIB, "GPIB My Listen Address set to {0}", data);
#endif
						_listener = data;
					}
					GPIBBus.Instance.BroadcastListener(data);
					break;

				case RemoteCommandGroup.TalkAddressGroup:
					// Set the talker address
					if (data == 0x1f)
					{
#if TRACING_ENABLED
						if (Trace.TraceOn)
							Trace.Log(LogType.GPIB, "GPIB Talker Address Group 'untalk' command received");
#endif
						_talk = false;    // ???
					}
					else
					{
#if TRACING_ENABLED
						if (Trace.TraceOn)
							Trace.Log(LogType.GPIB, "GPIB My Talker Address set to {0}", data);
#endif
						_talker = data;
					}
					GPIBBus.Instance.BroadcastTalker(data);
					break;

				case RemoteCommandGroup.SecondaryCommandGroup:
#if TRACING_ENABLED
					if (Trace.TraceOn)
						Trace.Log(LogType.GPIB, "GPIB Secondary Command/Address {0} received (ignored)", data);
#endif
					// Nothing to do, really, since we don't care about secondary listeners
					// or parallel polling...
					break;
			}
		
		}

		/// <summary>
		/// Return the contents of the TMS9914's read registers, although these aren't even remotely
		/// accurate -- nor, it seems, are they ever requested, at least by POS D, F or Accent S4... :-(
		/// But here's where we might want to have the interrupt status bits reflect whether or not there
		/// are bytes waiting to be read.  Since the Z80 drops these into a circular buffer and the PERQ
		/// reads them out there, only user-written software is likely to use this facility.  Sigh.
		/// </summary>
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
            if (Trace.TraceOn) Trace.Log(LogType.GPIB, "GPIB GetStatus() called!");
#endif
        }

        private void ResetGPIB()
        {
            _listen = false;
            _talk = false;
			_listener = 0x1f;   // nobody
			_talker = 0x1f;		// nobody
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
            AuxiliaryCommand = 6,   // GPIAUX
			DataOut = 7				// GPIDO
        }

        private enum GPIBReadRegister
        {
            InterruptStatus0 = 0,   // GPIIS0
            AddressSwitch = 1,      // GPIASW
            AddressStatus = 2,      // GPIAS
            CommandPassThrough = 3, // GPICPT
            InterruptStatus1 = 4,   // GPIIS1
            BusStatus = 6,          // GPIBS
			DataIn = 7				// GPIDI
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

		/// <summary>
		/// GPIB is insane.  RMMC is short for Remote Multiple Message Coding.
		/// This is how to pick commands out of the data bytes sent following
		/// an auxiliary command.  Bit 8 is always DontCare (masked off); bits
		/// 7..6 select a command group; bits 5..1 select a specific action or
		/// setting within the group, but sometimes the definitions overlap and
		/// make no sense (secondary address vs. parallel poll enable/disable)?
		/// Madness.  But we have to pick this apart to watch for our talk and
		/// listen addresses, which is how the PERQ turns the BitPadOne on or
		/// off!  Oy vey.
		/// </summary>
		private enum RemoteCommandGroup
		{
			AddressedCommandGroup = 0x0,
			ListenAddressGroup = 0x1,
			TalkAddressGroup = 0x2,
			SecondaryCommandGroup = 0x3
		}

		/// <summary>
		/// Here are the lower 5 bits for selecting various commands (which we
		/// mostly ignore) or setting the talker/listener addresses (which we
		/// care about).
		/// </summary>
		private enum AddressCommands
		{
			gtl = 0x01,		// Go to local
			sdc = 0x04,		// Selected device clear
			ppc = 0x05,		// Parallel poll configure
			gxt = 0x08,		// Group execute trigger ("get" conflicts w/reserved word)
			tct = 0x09,		// Take control
			llo = 0x10,		// Local lock out
			dcl = 0x14,		// Device clear
			ppu = 0x15,		// Parallel poll unconfigure
			spe = 0x18,		// Serial poll enable
			spd = 0x19		// Serial poll disable
		}

        // GPIB-specific flags
        private bool _listen;
        private bool _talk;

		private byte _listener;
		private byte _talker;

        private byte[] _registers;

        private Queue<byte> _busFifo;
        private byte[] _cmdData;
        private int _cmdIndex = 0;
        private GPIBCommand _cmdType;
        private int _cmdLength;
        private int _messageIndex;
    }
}
