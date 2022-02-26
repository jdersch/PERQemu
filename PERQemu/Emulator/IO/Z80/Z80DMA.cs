//
// Z80DMA.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Z80
{
	/// <summary>
	/// Implements most of the Z80 DMA controller.
	/// </summary>
	/// <remarks>
	/// Aw, crap.  The PERQ-1 IOB uses a Mostek MK3883N DMA chip (second source
	/// for the Zilog Z8410) which has one channel.  The PERQ-2 EIO uses the
	/// Am9517 4-channel DMA chip (which Intel calls the i8237).  We'll have to
	/// refactor things so each I/O Board loads its own DMA controller.
	/// </remarks>
	public class Z80DMA : IZ80Device
	{
		public Z80DMA(byte baseAddress, Z80MemoryBus memoryBus, Z80IOBus ioBus)
		{
			_memoryBus = memoryBus;
			_ioBus = ioBus;
			_baseAddress = baseAddress;
			_wr = new byte[7];
		}

		public void Reset()
		{
			_wr.Initialize();
			_writeBaseRegister = true;
			_baseRegister = 0;

			_portAddressAInit = 0;
			_portAddressBInit = 0;
			_blockLength = 0;
			_byteCounter = 0;
			_maskByte = 0;
			_matchByte = 0;
			_interruptControl = 0;
			_pulseControl = 0;
			_interruptVector = 0;
			_interruptActive = false;

			_enableDMA = false;
			_state = DMAState.Idle;

			Log.Debug(Category.Z80DMA, "Reset");
		}

		public string Name => "Z80 DMA";
		public byte[] Ports => new byte[] { _baseAddress };
		public byte? ValueOnDataBus => _interruptVector;    // TODO: implement dynamic vector based on type
		public bool IntLineIsActive => _interruptEnabled && _interruptActive;

		public event EventHandler NmiInterruptPulse;

		public void AttachDeviceA(IDMADevice device)
		{
			_deviceA = device;
		}

		public void AttachDeviceB(IDMADevice device)
		{
			_deviceB = device;
		}

		public void Clock()
		{
			// TODO: handle "Interrupt on RDY" option

			// If DMA is in progress, make it happen
			if (_enableDMA)
			{
				_state = RunStateMachine();
			}
		}

		/// <summary>
		/// Run the DMA state machine.  We have to split the transaction into
		/// two bus cycles because the Z80 can't do two memory/port operations
		/// between instructions; data corruption results.
		/// </summary>
		/// <remarks>
		/// DANGER, Will Robinson!  This extremely simplistic first hack is to
		/// verify that the stupid Z80 is corrupting memory; it is NOT optimized
		/// and _requires_ that the transaction completes so that addresses and
		/// byte counters are properly checked.  If the Z80 issues an abort mid-
		/// transaction this house of cards collapses.  If it solves the problem
		/// I'll figure out how to make it more robust...
		/// </remarks>
		private DMAState RunStateMachine()
		{
			DMAState nextState = _state;

			// Only here if enabled, so jump right in
			if (_state == DMAState.Idle)
			{
				_state = nextState = DMAState.SourceRead;
			}

			IDMADevice source;
			IDMADevice dest;

			ushort sourceAddress;
			bool sourceIsIO;
			ushort destAddress;
			bool destIsIO;

			// What direction is this going in
			WR0 wr0 = (WR0)_wr[0];
			WR1 wr1 = (WR1)_wr[1];
			WR2 wr2 = (WR2)_wr[2];

			if ((wr0 & WR0.DirectionAtoB) != 0)
			{
				// Source is A, dest is B
				source = _deviceA;
				sourceAddress = _portAddressA;
				sourceIsIO = (wr1 & WR1.MemoryOrIO) != 0;
				dest = _deviceB;
				destAddress = _portAddressB;
				destIsIO = (wr2 & WR2.MemoryOrIO) != 0;
			}
			else
			{
				source = _deviceB;
				sourceAddress = _portAddressB;
				sourceIsIO = (wr2 & WR2.MemoryOrIO) != 0;
				dest = _deviceA;
				destAddress = _portAddressA;
				destIsIO = (wr1 & WR1.MemoryOrIO) != 0;
			}

			switch (_state)
			{
				case DMAState.SourceRead:
					if (source.ReadDataReady)
					{
						if (sourceIsIO)
						{
							_data = _ioBus[sourceAddress];
						}
						else
						{
							_data = _memoryBus[sourceAddress];
						}

						//Log.Detail(Category.Z80DMA,
						Log.Info(Category.Z80DMA,
								  "Source read 0x{0:x2} from {1} (0x{2:x4})",
								 _data, source, sourceAddress);

						nextState = DMAState.DestWrite;
					}
					break;

				case DMAState.DestWrite:
					if (dest.WriteDataReady)
					{
						if (destIsIO)
						{
							_ioBus[destAddress] = _data;
						}
						else
						{
							_memoryBus[destAddress] = _data;
						}

						// Assume there's a next byte
						nextState = DMAState.SourceRead;

						// Update addresses & counters
						_byteCounter--;

						//Log.Detail(Category.Z80DMA,
						//Log.Info(Category.Z80DMA,
						//          "Transfer of 0x{0:x2} from {1} (0x{2:x4}) to {3} (0x{4:x4}), {5} bytes left",
						//          _data, source, sourceAddress, dest, destAddress, _byteCounter);

						Log.Info(Category.Z80DMA,
								  "Dest write 0x{0:x2} to {1} (0x{2:x4}), {3} bytes left",
								  _data, dest, destAddress, _byteCounter);

						if ((wr1 & WR1.PortAAddressFixed) == 0)
						{
							if ((wr1 & WR1.PortAIncrements) == 0)
							{
								_portAddressA--;
							}
							else
							{
								_portAddressA++;
							}
						}

						if ((wr2 & WR2.PortBAddressFixed) == 0)
						{
							if ((wr2 & WR2.PortBIncrements) == 0)
							{
								_portAddressB--;
							}
							else
							{
								_portAddressB++;
							}
						}
					}

					// Take action at the end of the block -- interrupt, restart, etc.
					if (_byteCounter == 0)
					{
						Log.Debug(Category.Z80DMA, "Transfer complete");
						nextState = DMAState.Idle;

						if ((_interruptControl & 0x2) != 0)
						{
							_interruptActive = true;
						}

						WR5 wr5 = (WR5)_wr[5];

						if ((wr5 & WR5.AutoRepeat) != 0)
						{
							_byteCounter = _blockLength;
							_portAddressA = _portAddressAInit;
							_portAddressB = _portAddressBInit;

							Log.Debug(Category.Z80DMA, "Transfer auto-restarting");
						}
						else
						{
							source.DMATerminate();
							dest.DMATerminate();
							_enableDMA = false;
						}
					}
					break;
			}
			return nextState;
		}


		public byte Read(byte portAddress)
		{
			throw new NotImplementedException();
		}

		public void Write(byte portAddress, byte value)
		{
			// DMA is disabled when a control byte is written
			_enableDMA = false;

			if (_writeBaseRegister)
			{
				_baseRegister = 0;

				// Note: we start with the decode for reg 1; if no matches are found
				// we assume a match on reg 0.
				for (int i = 1; i < _decodes.Length; i++)
				{
					if ((value & _decodes[i].Mask) == _decodes[i].Value)
					{
						_baseRegister = i;
						break;
					}
				}

				Log.Debug(Category.Z80DMA, "Write of 0x{0:x2} to base register {1}",
										   value, _baseRegister);
				WriteBaseRegister(value);
			}
			else
			{
				// Write sub-registers
				WriteSubRegister(value);
			}
		}

		private void WriteBaseRegister(byte value)
		{
			_wr[_baseRegister] = value;

			switch (_baseRegister)
			{
				case 0:
					// bits D3-D6 indicate sub-registers to be written to,
					// if none are set, return to base state.
					_writeBaseRegister = (value & 0x78) == 0;
					break;

				case 1:
				case 2:
					// bit D6 indicates a sub-register is to be written
					_writeBaseRegister = (value & 0x40) == 0;
					break;

				case 3:
					// bits D3 and D4:
					_writeBaseRegister = (value & 0x18) == 0;
					break;

				case 4:
					// bits D2-D4:
					_writeBaseRegister = (value & 0x1c) == 0;
					break;

				case 6:
					// handle command
					ExecuteCommand(value);
					_writeBaseRegister = true;
					break;
			}
		}

		private void WriteSubRegister(byte value)
		{
			byte regVal = _wr[_baseRegister];

			switch (_baseRegister)
			{
				case 0:
					if ((regVal & 0x08) != 0)
					{
						_portAddressAInit = (ushort)((_portAddressAInit & 0xff00) | value);
						_wr[_baseRegister] &= 0xf7;

						Log.Debug(Category.Z80DMA, "Port A address now 0x{0:x4}", _portAddressAInit);
					}
					else if ((regVal & 0x10) != 0)
					{
						_portAddressAInit = (ushort)((_portAddressAInit & 0x00ff) | (value << 8));
						_wr[_baseRegister] &= 0xef;

						Log.Debug(Category.Z80DMA, "Port A address now 0x{0:x4}", _portAddressAInit);
					}
					else if ((regVal & 0x20) != 0)
					{
						_blockLength = (ushort)((_blockLength & 0xff00) | value);
						_wr[_baseRegister] &= 0xdf;

						Log.Debug(Category.Z80DMA, "Block length now 0x{0:x4}", _blockLength);
					}
					else if ((regVal & 0x20) != 0)
					{
						_blockLength = (ushort)((_blockLength & 0x00ff) | (value << 8));
						_wr[_baseRegister] &= 0xbf;

						Log.Debug(Category.Z80DMA, "Block length now 0x{0:x4}", _blockLength);
					}
					_writeBaseRegister = (value & 0x78) == 0;
					break;

				case 1:
				case 2:
					// Right now I'm not implementing variable timing, so just ignore this.
					_writeBaseRegister = true;
					break;

				case 3:
					if ((regVal & 0x08) != 0)
					{
						_maskByte = value;
						_wr[_baseRegister] &= 0xf7;
					}
					else if ((regVal & 0x10) != 0)
					{
						_matchByte = value;
						_wr[_baseRegister] &= 0xef;
					}
					_writeBaseRegister = (value & 0x18) == 0;
					break;

				case 4:
					if ((regVal & 0x04) != 0)
					{
						_portAddressBInit = (ushort)((_portAddressBInit & 0xff00) | value);
						_wr[_baseRegister] &= 0xfb;

						Log.Debug(Category.Z80DMA, "Port Address B now 0x{0:x4}", _portAddressBInit);
					}
					else if ((regVal & 0x08) != 0)
					{
						_portAddressBInit = (ushort)((_portAddressBInit & 0x00ff) | (value << 8));
						_wr[_baseRegister] &= 0xf7;

						Log.Debug(Category.Z80DMA, "Port Address B now 0x{0:x4}", _portAddressBInit);
					}
					else if ((regVal & 0x10) != 0)
					{
						_interruptControl = value;
						_wr[_baseRegister] &= 0xef;
					}
					else if ((_interruptControl & 0x08) != 0)
					{
						_pulseControl = value;
						_interruptControl &= 0xf7;
					}
					else if ((_interruptControl & 0x10) != 0)
					{
						_interruptVector = value;
						_interruptControl &= 0xef;

						Log.Debug(Category.Z80DMA, "Interrupt vector now 0x{0:x4}", _interruptVector);
					}
					_writeBaseRegister = ((regVal & 0x1c) == 0) && ((_interruptControl & 0x18) == 0);
					break;

				default:        // Shouldn't happen
					throw new InvalidOperationException("Unexpected subregister write");
			}
		}

		private void ExecuteCommand(byte command)
		{
			switch (command)
			{
				case 0xc3:  // Reset
				case 0xa3:  // TODO: this is a slightly different reset
					Reset();
					break;

				case 0xc7:
				case 0xc8:
					// Reset timings, ignored for now.
					break;

				case 0xcf:  // Load starting addresses, reset byte counter
					_byteCounter = 0;
					_portAddressA = _portAddressAInit;
					_portAddressB = _portAddressBInit;
					break;

				case 0xd3:  // Continue from current locations, clear byte counter
					_byteCounter = 0;
					break;

				case 0xab:
					_interruptEnabled = true;
					break;

				case 0xaf:
					_interruptEnabled = false;
					break;

				case 0x87:
					_enableDMA = true;
					_byteCounter = (ushort)(_blockLength + 1);
					break;

				case 0x83:
					_enableDMA = false;
					break;

				// Unimplemented
				case 0xbb:  //_readMask
				case 0xb3:
				case 0x88:
				case 0xb7:
					throw new NotImplementedException($"DMA command {command}");
			}
		}

		private enum DMAState
		{
			Idle = 0,
			SourceRead,
			DestWrite
		}

		private Z80IOBus _ioBus;
		private Z80MemoryBus _memoryBus;
		private IDMADevice _deviceA;
		private IDMADevice _deviceB;
		private byte _baseAddress;

		private bool _writeBaseRegister;
		private int _baseRegister;

		private byte[] _wr;

		private ushort _portAddressAInit;
		private ushort _portAddressBInit;
		private ushort _blockLength;
		private byte _maskByte;
		private byte _matchByte;
		private byte _interruptControl;
		private byte _pulseControl;
		private byte _interruptVector;

		private DMAState _state;
		private byte _data;

		private bool _enableDMA;
		private bool _interruptActive;
		private bool _interruptEnabled;

		// Current port addresses
		private ushort _portAddressA;
		private ushort _portAddressB;
		private ushort _byteCounter;

		[Flags]
		private enum WR0
		{
			Transfer = 0x1,
			Search = 0x2,
			SearchTransfer = 0x3,
			DirectionAtoB = 0x4,
		}

		[Flags]
		private enum WR1
		{
			MemoryOrIO = 0x8,
			PortAIncrements = 0x10,
			PortAAddressFixed = 0x20,
		}

		[Flags]
		private enum WR2
		{
			MemoryOrIO = 0x8,
			PortBIncrements = 0x10,
			PortBAddressFixed = 0x20,
		}

		[Flags]
		private enum WR3
		{
			StopOnMatch = 0x4,
			InterruptEnable = 0x20,
			DMAEnable = 0x40,
		}

		[Flags]
		private enum WR5
		{
			ReadyActiveHigh = 0x8,
			CEWAITMultiplexed = 0x10,
			AutoRepeat = 0x20,
		}

		private struct RegisterDecodes
		{
			public RegisterDecodes(byte mask, byte value)
			{
				Mask = mask;
				Value = value;
			}

			public byte Mask;
			public byte Value;
		}

		private RegisterDecodes[] _decodes =
		{
			new RegisterDecodes(0x80, 0x00),    // WR0, not actually decoded (assumed if no matches are found)
            new RegisterDecodes(0x87, 0x04),
			new RegisterDecodes(0x87, 0x00),
			new RegisterDecodes(0x83, 0x80),
			new RegisterDecodes(0x83, 0x81),
			new RegisterDecodes(0xc7, 0x82),
			new RegisterDecodes(0x83, 0x83),    // WR6
        };
	}
}
