//
// CIOMicropolisDiskController.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
//
// This file is part of PERQemu.
//
// PERQemu is free software: you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
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
using PERQemu.Processor;

namespace PERQemu.IO.DiskDevices
{
    /// <summary>
    /// Represents a Micropolis 8" hard drive controller which manages disk
    /// drives in the Disk8Inch class.  This is implemented in the PERQ as an
    /// adapter from the SA4000 interface to the Micropolis 1200-series drives.
    /// </summary>
    /// <remarks>
    /// Although the 1220 controller can manage a string of up to four drives,
    /// there doesn't appear to be any software support for setting the Drive
    /// Select value.  This statement in a recently unearthed document also
    /// corrects my misconception that the ICL cabinet could house two drives:
    /// 
    ///     "None of the PERQ cabinets has enough space for mounting more
    ///      than one 8" drive internally."
    ///         -- config.doc Rev 3, Steve Clark 18 Dec 84
    /// 
    /// As with the Shugart, emulation for now is limited to a single drive. :-(
    /// </remarks>
    public sealed class CIOMicropolisDiskController : IStorageController
    {

        //
        //  THIS VERSION IS INCOMPLETE and is not part of the standard build.
        //  The first attempt at a "CIO Micropolis" is saved here in case it
        //  can be completed once additional source, documentation, or even a
        //  good disassembly of a definitive microcode binary can be found.
        //  The standard MicropolisDiskController is now EIO only.
        //

        public CIOMicropolisDiskController(PERQSystem system)
        {
            _system = system;
            _disk = null;
            _busyEvent = null;

            // ExtendedRegisters to glom the register writes together!
            _cylinder = new ExtendedRegister(4, 8);
            _nibLatch = new ExtendedRegister(4, 4);

            // 8" drives are only supported on the 20-bit machines...
            _dataBuffer = new ExtendedRegister(4, 16);
            _headerAddress = new ExtendedRegister(4, 16);
        }

        /// <summary>
        /// Perform a "hardware reset" of the controller and drive.
        /// </summary>
        public void Reset()
        {
            if (_disk != null)
            {
                _disk.Reset();
            }

            // Clears busy and the interrupt
            ClearBusyState();

            // Force a soft reset (calls ResetFlags)
            LoadCommandRegister((int)Command.Reset);

            Log.Info(Category.HardDisk, "CIO Micropolis controller reset");
        }

        /// <summary>
        /// Resets the flags ("soft" reset under microcode control).
        /// </summary>
        void ResetFlags()
        {
            _controllerBusy = false;
            _illegalAddr = false;
            _seekComplete = true;

            _head = 0;
            _sector = 0;
            _cylinder.Value = 0;

            _nibSaved = 0;
            _nibLatch.Value = 0;
            _headerAddress.Value = 0;
            _dataBuffer.Value = 0;
        }

        /// <summary>
        /// Attach a drive.  For now, only a single unit is supported.
        /// </summary>
        public void AttachDrive(uint unit, StorageDevice dev)
        {
            if (_disk != null)
                throw new InvalidOperationException("CIO MicropolisController only supports 1 disk");

            _disk = dev as HardDisk;
            _disk.SetSeekCompleteCallback(SeekCompletionCallback);

            Log.Info(Category.HardDisk, "Attached disk '{0}'", _disk.Info.Name);
        }

        /// <summary>
        /// Reads the status register.
        /// </summary>
        /// <remarks>
        /// Reading status DOES NOT clear pending interrupts.  (See behavior in disktest.mic)
        /// </remarks>
        public int ReadStatus()
        {
            var stat = DiskStatus;
            Log.Write(Category.HardDisk, "CIO Micropolis status: 0x{0:x4} ({1})", stat, (Status)stat);
            return stat;
        }

        /// <summary>
        /// Dispatch register writes.
        /// </summary>
        public void LoadRegister(byte address, int value)
        {
            switch (address)
            {
                case 0xc1:      // Command register
                    LoadCommandRegister(value);
                    break;

                case 0xc2:      // Micropolis Nibble Bus register
                    //
                    // We save the low 4 bits here.  The nibble command byte tracks
                    // transitions of the BusEn pin to shift the low nibble into the
                    // upper nibble latch (_nibLatch) so that a full 8-bit byte is
                    // transferred to the drive.  In SOME versions of the microcode?
                    // In others this is still treated like the Shugart head select!?
                    // ARGH.
                    //
                    _nibLatch.Lo = (ushort)value;
                    Log.Write(Category.HardDisk, "Micropolis Nibble latch (low) set to 0x{0:x}", _nibLatch.Lo);
                    break;

                case 0xc8:      // "MicZero"
                    //
                    // Unused by the actual "ICL CIO" board?  Original version of
                    // the Micropolis microcode (mdsk.micro, B. Rosen, 1982) uses
                    // the Shugart regs with NO mention of the MicSec (314/0xcc)
                    // register.  On that board cioboot and ciosysb assign a 0 and
                    // don't touch this again.  Hmm.
                    //
                    _sector = (ushort)(value & 0x1f);
                    _head = (byte)((value & 0xe0) >> 5);
                    _cylinder.Lo = (ushort)((value & 0xff00) >> 8);

                    Log.Write(Category.HardDisk, "Shugart cyl/head/sector set to {0}/{1}/{2}", _cylinder.Lo, _head, _sector);
                    Log.Write(Category.HardDisk, "Micropolis Zero register set to 0x{0:x}", value);
                    break;

                case 0xc9:      // "MicSync"
                    //
                    // Does this set up the sync character used by the hardware to
                    // identify address marks (?) - low level format byte (?) is
                    // set to 17 (oct) at startup by cioboot, ciosysb but in other
                    // cases it's still used as the File SN (which we don't actually
                    // use to compare with the header bytes...)
                    //
                    Log.Write(Category.HardDisk, "Micropolis Sync register set to 0x{0:x}", value);

                    //_serialNumber.Lo = (ushort)value;
                    //Log.Write(Category.HardDisk, "Shugart File Serial # Low set to 0x{0:x4}", value);
                    break;

                case 0xca:      // "MicCylin"
                    //
                    // Low byte of the desired cylinder inverted!?  Or high word
                    // of the File SN?  Yes.  No.  Who knows?
                    //
                    _cylinder.Lo = (ushort)~value;
                    Log.Write(Category.HardDisk, "Micropolis Cyl # (low) set to 0x{0:x2}", _cylinder.Lo);

                    //_serialNumber.Hi = value;
                    //Log.Write(Category.HardDisk, "Shugart File Serial # High set to 0x{0:x}", value);
                    break;

                case 0xcb:      // "MicCylHd"
                    //
                    // Bits 7:4 are cylinder 11:8
                    // Bits 3:0 are the head select (inverted!?)
                    // Except when it's still being used for the Shugart "block #"
                    // Wheeee!  No documentation or schematics!  Looks like manual
                    // disassembly and step by step instruction traces ahead  arrrgh
                    //
                    _cylinder.Hi = (~value & 0xf0) >> 4;
                    _head = (byte)(~value & 0x0f);
                    Log.Write(Category.HardDisk, "Micropolis Cyl # (high) set to 0x{0:x}, Head 0x{1:x}", _cylinder.Hi, _head);

                    //_blockNumber = value & 0xffff;
                    //Log.Write(Category.HardDisk, "Shugart Block # set to 0x{0:x}", value);
                    break;

                case 0xcc:      // "MicSecNo"
                    //
                    // Sector number (8 bits, though only 5 are significant?)
                    // And yet, haven't seen a version of the code that actually
                    // sets this even though it (sometimes) is loaded into the
                    // controlstore.  Sigh.
                    //
                    _sector = (ushort)(value & 0xff);

                    Log.Write(Category.HardDisk, "Micropolis Sector # set to 0x{0:x2}", _sector);
                    break;

                case 0xd0:      // Micropolis Data Buffer Address High register
                    _dataBuffer.Hi = ~value;
                    Log.Write(Category.HardDisk, "Micropolis Data Buffer Address (high) set to 0x{0:x}", _dataBuffer.Hi);
                    break;

                case 0xd1:      // Micropolis Header Address High register
                    _headerAddress.Hi = ~value;
                    Log.Write(Category.HardDisk, "Micropolis Header Address (high) set to 0x{0:x}", _headerAddress.Hi);
                    break;

                case 0xd8:      // Micropolis Data Buffer Address Low register
                    _dataBuffer.Lo = (ushort)value;
                    Log.Write(Category.HardDisk, "Micropolis Data Buffer Address (low) set to 0x{0:x4}", _dataBuffer.Lo);
                    break;

                case 0xd9:      // Micropolis Header Address low register
                    _headerAddress.Lo = (ushort)value;
                    Log.Write(Category.HardDisk, "Micropolis Header Address (low) set to 0x{0:x4}", _headerAddress.Lo);
                    break;

                default:
                    throw new InvalidOperationException($"Bad register write 0x{address:x2}");
            }
        }

        /// <summary>
        /// Loads the Micropolis command register and dispatches the appropriate
        /// command to the state machine (bits 2:0 of the control reg 0xc1).
        /// </summary>
        /// <remarks>
        /// Note:  Most of the info gleaned about the CIO Micropolis controller
        /// behavior is from microcode sources.  EIO is better documented (and
        /// may be encoded differently!).
        ///     Command bits passed to the drive:
        ///         7   Reset the drive's I/O board (*)
        ///         6   Drive Select (always 0?)
        ///         5   BA1 bit
        ///         4   BusEnable bit
        ///         3   BA0 bit
        ///     Command bits for the controller:
        ///       2:0   Command
        /// 
        /// * Am I losing my mind?  This bit is never set if we don't pass it through
        /// since it's used to turn the Z80 off and on.  So on the CIO, for which we
        /// have no *$)!#*& schematics, does that bit do double duty?
        /// </remarks>
        public void LoadCommandRegister(int data)
        {
            var command = (Command)(data & 0x07);
            var nibCommand = (NibbleSelect)(data & 0x78);

            Log.Write(Category.HardDisk, "Micropolis command: 0x{0:x4} (SM {1}, Latch {2})",
                                          data, command, nibCommand);

            // Check the nibble command
            NibbleOnThis(nibCommand);

            // Look at the command bits
            switch (command)
            {
                case Command.Idle:
                    // Clear the busy status and the interrupt
                    ClearBusyState();
                    break;

                case Command.Reset:
                    // Reset clears the state machine, interrupts when done
                    ResetFlags();

                    Log.Write(Category.HardDisk, "Micropolis state machine reset");
                    SetBusyState();
                    break;

                case Command.ReadChk:
                    ReadBlock();
                    break;

                case Command.Read:
                    ReadBlock();
                    break;

                case Command.Write:
                    WriteBlock(true /* writeHeader */);
                    break;

                case Command.WriteChk:
                    WriteBlock(false /* writeHeader */);
                    break;

                case Command.Format:
                    FormatBlock();
                    break;

                default:
                    Log.Error(Category.HardDisk, "Unhandled Micropolis command {0}", command);
                    break;
            }
        }

        /// <summary>
        /// Try to make sense of the "nibble bus" command bits.
        /// </summary>
        void NibbleOnThis(NibbleSelect nibCommand)
        {
            Log.Write("Nibble this: new={0} | last={1}", nibCommand, _nibCommand);
            // Save and update
            var lastCommand = _nibCommand;
            _nibCommand = nibCommand;

            // This bit conflicts with the Z80 control bit, does it not? We'll see.
            if (nibCommand == NibbleSelect.Reset)
            {
                Log.Write("NIB RESET");
                return;
            }

            // This should always be unit 0 for the 8" drives.  Right?
            if (nibCommand.HasFlag(NibbleSelect.DriveSelect))
            {
                Log.Write("NIB DRIVE SELECT ASSERTED!?");
                return;
            }

            // If there wasn't a BusEn transition, bug out?
            if ((nibCommand & NibbleSelect.BusEn) == (lastCommand & NibbleSelect.BusEn))
            {
                Log.Write("NO BUSEN TRANSITION");
                return;
            }

            // Decode the BA0/BA1 bits to decide which register is selected
            var reg = (RegSelect)(nibCommand & (NibbleSelect.BA1 | NibbleSelect.BA0));

            Log.Write("REG SELECT IS {0}", reg);

            // Is this a rising or falling edge of the BusEn signal?
            var busEnRising = (!lastCommand.HasFlag(NibbleSelect.BusEn) && nibCommand.HasFlag(NibbleSelect.BusEn));

            // On the rising edge, bump the latch
            if (busEnRising)
            {
                if (reg == RegSelect.None)
                {
                    // Bump the low nibble to the high
                    _nibLatch.Hi = _nibLatch.Lo;
                    Log.Write(Category.HardDisk, "Micropolis Nibble latch (high) set to 0x{0:x}", _nibLatch.Hi);
                }
                else
                {
                    // For now just log this but all the action happens on the falling edge
                    Log.Write(Category.HardDisk, "BUSEN RISING (IGNORED) FOR REG {0}", reg);
                }
            }
            else
            {
                switch (reg)
                {
                    case RegSelect.None:
                        Log.Write("BUSEN FALLING WHILE NO REG SELECTED, SAVED LATCH {0}", _nibLatch.Value);
                        // Do we save our latched value here!?!?
                        _nibSaved = (byte)_nibLatch.Value;
                        break;

                    case RegSelect.HdReg:
                        //
                        // The Micropolis documentation EXPLICITLY STATES that the
                        // seek operation starts WHEN THE LSB OF THE CYLINDER VALUE
                        // is written, NOT this register.  But every example of the
                        // PERQ microcode does it backwards - writes the LSB first,
                        // THEN the combined cyl/head.  That makes more sense, I
                        // suppose, if you don't need to seek and just switch heads?
                        //
                        Log.Write("HEAD SELECT {0} ON BUSEN FALLING", _head);
                        _disk.HeadSelect(_head);

                        // Shouldn't happen, but just in case an attempt to
                        // seek off the end, use the "illegal address" response
                        if (_cylinder.Value > _disk.Geometry.Cylinders)
                        {
                            Log.Write("BAD SEEK! OFF THE END TO CYL {0}", _cylinder.Value);
                            _illegalAddr = true;
                            _seekComplete = true;
                            return;
                        }

                        // Doc says reset this after a new cylinder request is validated
                        _illegalAddr = false;

                        // Initiate an "implied" seek!
                        if (_disk.CurCylinder != _cylinder.Value)
                        {
                            _seekComplete = false;
                            _disk.SeekTo((ushort)_cylinder.Value);
                            Log.Write("INITIATED SEEK TO CYL {0}", _cylinder.Value);
                        }
                        else
                        {
                            Log.Write("SEEK SKIPPED, ALREADY AT CYL {0}", _cylinder.Value);
                        }
                        break;

                    case RegSelect.CylReg:
                        Log.Write("WAAAAAAAAIIIT FOR IT!!!");
                        break;

                    case RegSelect.CtlReg:
                        //
                        // Use the SAVED command!  The microcode has to "pulse"
                        // the command into the drive's controller and it executes
                        // on the falling edge of BusEn.  But the individual command
                        // bits have been toggled at this point.  Argh!
                        //
                        //      Set BA0/BA1 = 3, write to Command reg, BUSEN low
                        //      Put upper nibble to Nibble/Head reg
                        //      Transition BUSEN high
                        //      Put lower nibble into Nibble/Head reg
                        //      Transition BUSEN low
                        //          (This combines, saves 8-bits of nibble register)
                        //      Set BA0/BA1 to the desired reg, BUSEN remains low
                        //          (This can be ignored since BUSEN doesn't transition)
                        //      Set BUSEN high
                        //      Set BUSEN low
                        //          (On THIS falling edge execute the saved command:
                        //           for control reg, Restore or FaultReset;
                        //           for Head/Cyl reg, do the Head Select;
                        //           for Cylinder reg, initiate the Seek)
                        //
                        // So, how the hell do reads and writes work?  The state machine
                        // has to "know" if a seek is happening and wait until it's done?
                        // It looks like maybe the microcode always _explicitly_ does the
                        // seek and then starts the state machine to initiate the read?
                        // (We might be able to ignore the WEN bit processing?)
                        var control = (Control)_nibSaved;

                        Log.Write("CONTROL REG WRITE: {0}", control);

                        // If the FaultReset bit is set, send that to the drive now
                        if (control.HasFlag(Control.FaultReset))
                        {
                            _disk.FaultClear();
                            // I think this can fall thru?
                        }

                        // Restore == Seek to track 0 (recalibrate)
                        if (control.HasFlag(Control.Restore))
                        {
                            _seekComplete = false;
                            _disk.SeekTo(0);
                            SetBusyState();
                            break;
                        }

                        if ((control & ~(Control.FaultReset | Control.Restore)) != 0)
                        {
                            Log.Write("EXTRA BITS IGNORED (TOM/TOP/WEN)");
                        }
                        break;

                    default:
                        Console.WriteLine($"BAD COMBO reg=0x{reg:x}");
                        break;
                }
            }
        }

        /// <summary>
        /// Construct the current status word.
        /// </summary>
        public int DiskStatus
        {
            get
            {
                return (int)(_disk == null ? Status.Busy :
                            ((_controllerBusy ? Status.Busy : 0) |
                             (_illegalAddr ? Status.IllegalAddress : 0) |
                             (_disk.Index ? Status.Index : 0) |
                             (_disk.Fault ? Status.WriteFault : 0) |
                             (_seekComplete ? Status.SeekComplete : 0) |
                             (_disk.Ready ? Status.UnitReady : 0)));
            }
        }

        /// <summary>
        /// Single step pulse (used by Shugart, not by Micropolis).
        /// </summary>
        public void DoSingleSeek()
        {
            // This means that the Z80 tried to step the heads!?!?
            Log.Warn(Category.HardDisk, "Micropolis single seek step ignored!");
        }

        /// <summary>
        // Seek completion for the Micropolis.
        /// </summary>
        public void SeekCompletionCallback(ulong skewNsec, object context)
        {
            // Set our local flag.  True on Ready as well as seeks.
            _seekComplete = true;

            // Clear busy status
            ClearBusyState();
        }

        /// <summary>
        /// Reads a block from cyl/head/sec into memory at the addresses
        /// specified by the controller registers.
        /// </summary>
        void ReadBlock()
        {
#if DEBUG
            if (_disk.CurCylinder != _cylinder.Value || _disk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         _disk.CurCylinder, _cylinder.Value, _disk.CurHead, _head);
#endif
            // Todo: Do this as a DMA operation

            // Read the sector from the disk
            var sec = _disk.GetSector((ushort)_cylinder.Value, _head, _sector);

            // Unfrob the buffer addresses
            var data = _system.IOB.DMARegisters.GetDataAddress(ChannelName.HardDisk);
            var header = _system.IOB.DMARegisters.GetHeaderAddress(ChannelName.HardDisk);

            // Copy the data to the data buffer address
            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = sec.Data[i] | (sec.Data[i + 1] << 8);
                _system.Memory.StoreWord(data + (i >> 1), (ushort)word);
            }

            // And the header to the header address
            for (int i = 0; i < sec.Header.Length; i += 2)
            {
                int word = sec.Header[i] | (sec.Header[i + 1] << 8);
                _system.Memory.StoreWord(header + (i >> 1), (ushort)word);
            }

            Log.Write(Category.HardDisk,
                      "Micropolis sector read complete from {0}/{1}/{2}, to memory at 0x{3:x6}",
                      _cylinder.Value, _head, _sector, data);

            SetBusyState();
        }

        /// <summary>
        /// Does a write to the cyl/head/sec specified by the controller registers.
        /// Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        void WriteBlock(bool writeHeader)
        {
#if DEBUG
            if (_disk.CurCylinder != _cylinder.Value || _disk.CurHead != _head)
                Log.Warn(Category.HardDisk,
                         "Out of sync with disk: cyl {0}={1}, hd {2}={3}?",
                         _disk.CurCylinder, _cylinder.Value, _disk.CurHead, _head);
#endif
            // Todo: Should be a DMA op.  See above.

            var sec = _disk.GetSector((ushort)_cylinder.Value, _head, _sector);
            var data = _system.IOB.DMARegisters.GetDataAddress(ChannelName.HardDisk);
            var header = _system.IOB.DMARegisters.GetHeaderAddress(ChannelName.HardDisk);

            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(data + (i >> 1));
                sec.Data[i] = (byte)(word & 0xff);
                sec.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            if (writeHeader)
            {
                for (int i = 0; i < sec.Header.Length; i += 2)
                {
                    int word = _system.Memory.FetchWord(header + (i >> 1));
                    sec.Header[i] = (byte)(word & 0xff);
                    sec.Header[i + 1] = (byte)((word & 0xff00) >> 8);
                }
            }

            // Write the sector to the disk...
            _disk.SetSector(sec);

            Log.Write(Category.HardDisk,
                      "Micropolis sector write complete to {0}/{1}/{2}, from memory at 0x{3:x6}",
                      _cylinder.Value, _head, _sector, data);

            SetBusyState();
        }

        /// <summary>
        /// Does a "format" of the cyl/head/sec specified by the controller
        /// registers.  Does NOT commit to disk, only in memory copy is affected.
        /// </summary>
        void FormatBlock()
        {
            var sec = new Sector((ushort)_cylinder.Value, _head, _sector,
                                 _disk.Geometry.SectorSize,
                                 _disk.Geometry.HeaderSize);

            var data = _system.IOB.DMARegisters.GetDataAddress(ChannelName.HardDisk);
            var header = _system.IOB.DMARegisters.GetHeaderAddress(ChannelName.HardDisk);

            for (int i = 0; i < sec.Data.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(data + (i >> 1));
                sec.Data[i] = (byte)(word & 0xff);
                sec.Data[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the new header data...
            for (int i = 0; i < sec.Header.Length; i += 2)
            {
                int word = _system.Memory.FetchWord(header + (i >> 1));
                sec.Header[i] = (byte)(word & 0xff);
                sec.Header[i + 1] = (byte)((word & 0xff00) >> 8);
            }

            // Write the sector to the disk...
            _disk.SetSector(sec);

            Log.Write(Category.HardDisk,
                      "Micropolis sector format of {0}/{1}/{2} complete, from memory at 0x{3:x6}",
                      _cylinder.Value, _head, _sector, data);

            SetBusyState();
        }

        /// <summary>
        /// Set the controller Busy status, allow for processing delay, then
        /// raise the HardDisk interrupt.
        /// </summary>
        void SetBusyState()
        {
            // Already busy?  Nothing to do here.
            if (_controllerBusy) return;

            // Set busy flag (code 7), and queue a workitem for resetting it and
            // firing an interrupt.  Time would normally vary based on platter
            // rotation, seek and head settling time, etc.  But we don't really
            // know that in advance, so for seeks don't queue anything; for other
            // commands (reads, writes, etc) just fake up a short/fixed delay.
            _controllerBusy = true;

            // do this based on command being executed
            //if (_seekState != SeekState.WaitForSeekComplete)

            // if seek complete then we're done moving the heads?
            // if _illegal or reset or idle, just a short delay? else long?
            // should we pass in an interrupt_on_done flag here and/or the delay
            // so the caller can choose?

            {
                var delay = Settings.Performance.HasFlag(RateLimit.DiskSpeed) ?
                                                    BlockDelayNsec :
                                                    100 * Conversion.UsecToNsec;

                _busyEvent = _system.Scheduler.Schedule(delay, (skew, context) =>
                {
                    ClearBusyState(true);
                });
            }
        }

        /// <summary>
        /// Unconditionally clear the Busy state.  Raise or clear the disk
        /// interrupt, depending on the caller's situation.
        /// </summary>
        void ClearBusyState(bool raiseInterrupt = false)
        {
            _controllerBusy = false;

            if (raiseInterrupt)
            {
                _system.CPU.RaiseInterrupt(InterruptSource.HardDisk);
            }
            else
            {
                _system.CPU.ClearInterrupt(InterruptSource.HardDisk);
            }
        }

        /// <summary>
        /// Disk command bits 2:0 go to the hard disk state machine.
        /// </summary>
        enum Command
        {
            Idle = 0,           // No-op (clear command)
            ReadChk = 1,        // Read Data - Check Header
            Read = 2,           // Read Data - Read Header
            WriteChk = 3,       // Write Data - Check Header
            Write = 4,          // Write Data - Write Header
            Format = 5,         // Format Write
            Seek = 6,           // Seek (unused)
            Reset = 7           // Reset controller
        }

        /// <summary>
        /// Nibble bus select bits passed directly to the drive controller.
        /// </summary>
        [Flags]
        enum NibbleSelect
        {
            None = 0x0,
            BA0 = 0x08,
            BusEn = 0x10,
            BA1 = 0x20,
            DriveSelect = 0x40,
            Reset = 0x80
        }

        /// <summary>
        /// Micropolis drive/controller register select (combinations of BA1/BA0).
        /// </summary>
        enum RegSelect
        {
            CylReg = 0x0,
            HdReg = 0x8,
            CtlReg = 0x20,
            None = 0x28
        }

        /// <summary>
        /// Control register bits (when BA = 3).  Unused bits are assumed zero.
        /// </summary>
        [Flags]
        enum Control
        {
            None = 0x0,
            WriteEnable = 0x1,
            TrackOffsetPlus = 0x4,
            TrackOffsetMinus = 0x8,
            FaultReset = 0x10,
            Restore = 0x40,
            PreampHighGain = 0x80
        }

        /// <summary>
        /// Status bits from the drive/controller.
        /// </summary>
        [Flags]
        enum Status
        {
            Done = 0x0,
            SectorNotFound = 0x1,
            PhysicalCRCError = 0x2,
            FileNumMismatch = 0x3,
            LogBlockMismatch = 0x4,
            LogHeaderCRCError = 0x5,
            DataCRCError = 0x6,
            Busy = 0x7,
            Index = 0x08,
            IllegalAddress = 0x10,
            WriteFault = 0x20,
            SeekComplete = 0x40,
            UnitReady = 0x80
        }

        // The physical disk
        HardDisk _disk;

        // Registers
        byte _head;
        ushort _sector;
        ExtendedRegister _cylinder;

        ExtendedRegister _headerAddress;
        ExtendedRegister _dataBuffer;

        ExtendedRegister _nibLatch;
        NibbleSelect _nibCommand;
        byte _nibSaved;

        Command _command;

        bool _seekComplete;
        bool _controllerBusy;
        bool _illegalAddr;

        // Work timing for reads/writes, assuming the interface's documented
        // 5.64Mbit/sec (705KB/sec) max transfer rate (MFM, not GCR encoding)
        // transferring 528-byte sectors.  See ShugartController.cs for more
        // info about how this is derived.
        readonly ulong BlockDelayNsec = 749 * Conversion.UsecToNsec;

        SchedulerEvent _busyEvent;

        PERQSystem _system;
    }
}

/*
    Notes:

    Have to reconcile the CIO/Shugart/mystery implementation (so-called ICL CIO
    that is poorly documented) with the EIO/PERQ-2 version which is described in
    the Rose document.  This version has to take precedence as it's by far the
    more general case (PERQ-2 and 2/T1 models).

    I had hoped to avoid having to load different microcode ROMs but it appears
    unavoidable that eioboot and eio5boot will have to be distinguished by the
    configured disk type.  If we ever manage to locate/disassemble the POS R.4
    or PNX "CIO Micropolis" code the still-mostly-theoretical PERQ-1 support can
    be added back in.
    
*/