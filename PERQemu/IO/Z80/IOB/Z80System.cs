// z80system.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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
using System.Security.Permissions;

using PERQemu.CPU;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80.IOB
{

    public enum PERQtoZ80Message
    {
        Invalid = 0x0,
        RS232 = 0x1,
        FloppyCommand = 0x2,
        GPIBCommand = 0x3,
        Speech = 0x4,
        SetRS232Status = 0x5,
        SetTabletStatus = 0x6,
        SetKeyboardStatus = 0x7,
        HardDriveSeek = 0x8,
        SetSpeechStatus = 0x9,      // Z80 v8.7: was Voltage Set
        SetClockStatus = 0xa,
        GetStatus = 0xb,
        SetFloppyStatus = 0xc,
        FloppyBoot = 0xd,
    }

    public enum Z80toPERQMessage
    {
        Invalid = 0x0,
        KeyboardData = 0x1,
        RS232Data = 0x2,
        TabletData = 0x3,
        ClockData = 0x4,
        FloppyData = 0x5,
        GPIBData = 0x6,
        RS232Status = 0x7,
        TabletStatus = 0x8,
        KeyboardStatus = 0x9,
        SeekComplete = 0xa,
        Z80StatusChange = 0xb,
        VoltageData = 0xc,
        VoltageStatus = 0xd,
        ClockStatus = 0xe,
        GPIBStatus = 0xf,      // Is this really "missing" in POS D/F IO.micro!?
        FloppyStatus = 0x10,
        FloppyDone = 0x11,
        FloppyBootError = 0x12,
        FloppyBootData = 0x13,
    }

    [Flags]
    public enum ReadyFlags     // "Bits in Ready Flag Byte" per v87.z80
    {
        RS232 = 0x1,
        Speech = 0x2,
        Floppy = 0x4,
        GPIB = 0x8,
        Seek = 0x10,
        Z80 = 0x80             // "Change in Ready State"
    }
            
    /// <summary>
    /// Represents an IOB Z80 subsystem.  This currently simulates the behavior
    /// of the Z80 system, but does not actually _emulate_ it.  The PERQ1 and PERQ1A
    /// had no way to upload code to the Z80 so this is sufficient but kinda lame.
    /// At some point it would be nifty to actually emulate the Z80 here, for accuracy
    /// and to allow emulating a PERQ 2.  There are also significant differences between
    /// the 1's Z80 and the 2's Z80 (the 2 added DMA support for floppy access,
    /// doesn't support the hard disk, etc.)
    ///
    /// So if I do add PERQ2 support, this will need some serious refactoring.
    ///
    /// The Z80 system controls the PERQ's hard/floppy disk, serial ports, speech,
    /// GPIB, keyboard, clock and pointer devices.
    /// </summary>
    public sealed class Z80System
    {
        private Z80System()
        {
            _inputFifo = new Queue<byte>(256);
            _outputFifo = new Queue<byte>(256);
            _devices = new List<IZ80Device>(16);

            _hardDiskSeek = new HardDiskSeekControl();
            _floppyDisk = new FloppyController();
            _keyboard = new Keyboard();
            _gpib = new GPIB();
            _tablet = new Tablet();
            _rs232 = new RS232();
            _speech = new Speech();
            _clockDev = new Clock();

            BuildDeviceList();

            Reset();

            _clocks = 0;
            _running = false;
        }

        private void BuildDeviceList()
        {
            _devices.Clear();
            _devices.Add(_hardDiskSeek);
            _devices.Add(_floppyDisk);
            _devices.Add(_keyboard);
            _devices.Add(_gpib);
            _devices.Add(_tablet);
            _devices.Add(_rs232);
            _devices.Add(_speech);
            _devices.Add(_clockDev);
        }

        public void Reset()
        {
            foreach (IZ80Device device in _devices)
            {
                device.Reset();
            }

            // Idle the state machine
            _state = MessageParseState.WaitingForSOM;

            // Clear output fifo
            _outputFifo.Clear();

            // The Z80 is always ready for action!
            _deviceReadyState = ReadyFlags.Z80;

            _dataReadyInterruptRequested = false;
        }

        public static Z80System Instance
        {
            get { return _instance; }
        }

        public int Clocks()
        {
            return _clocks;
        }

        public void LoadFloppyDisk(string path)
        {
            _floppyDisk.LoadImage(path);
        }

        public void SaveFloppyDisk(string path)
        {
            _floppyDisk.SaveImage(path);
        }

        public void UnloadFloppyDisk()
        {
            _floppyDisk.UnloadImage();
        }

        public void SetSerialPort(ISerialDevice dev)
        {
            _rs232.SetDevice(dev);
        }

        public string GetSerialPort()
        {
            return _rs232.Port;
        }

        public Queue<byte> FIFO
        {
            get { return _outputFifo; }
        }

        public Keyboard Keyboard
        {
            get { return _keyboard; }
        }

        /// <summary>
        /// Corresponds to IOB port 0xc1 (octal 301).
        /// The PERQ1 microcode uses port c1 to control both the hard drive and the Z80.
        /// The lower 5 bits are hard disk control flags.
        ///
        /// From sysb.mic:
        /// !
        /// ! Turn Z80 Off
        /// Z80Off: 200, IOB(301), loc(7500);         ! shut off disk and Z80
        ///         0, IOB(307), loc(7501);           ! shut off Z80 output interrupts
        ///         IOB(106), loc(7502);              ! dismiss Z80 input interrupt
        ///         IOB(147), return, loc(7503);      ! dismiss memory parity interrupt
        ///
        /// Elsewhere:
        /// KeyRetry: 0, IOB(301);                    ! turn Z80 on
        ///         Call(WaitAWhile);                 ! Let it start up
        ///
        /// From this I am assuming that if the Z80 is "off", writing "0" to the Status register
        /// starts it up, and if it is "on" then writing 0x80 to it turns it off again.  NOTE in
        /// particular the order of those instructions: we still have to look at the on/off flag
        /// even if the Z80 isn't "running"!
        /// </summary>
        public void LoadStatus(int data)
        {
            if (data == 0x80 && _running)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 system shut down by write to Status register.");
#endif
                _running = false;
                Reset();
            }
            else if (data == 0 && !_running)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 system started by write to Status register.");
#endif
                _running = true;
                Reset();
                //SendStatusChange();   // Sent in our first Clock() by RefreshReadyState()?
            }
        }

        /// <summary>
        /// Sends data to the Z80.
        /// Corresponds to IOB port 0xc7 (octal 307).
        ///
        ///  The IOB schematic (p. 49 of the PDF, "IOA DECODE") sheds some light on the Z80 "input"
        ///  interrupt.  This is actually labeled as "Z80 READY INT L" (meaning it's active Low)
        ///  and seems to be enabled by the PERQ sending data with bit 8 high, and can be dismissed by
        ///  the PERQ sending data with bit 8 low.
        ///
        ///  IOD 8 is latched on a WRITE from the Z80 and gated with the PERQ->Z80 REQ L signal at a
        ///  NAND gate.  So -- if the PERQ sets IOD 8 high with a write, and there is no pending PERQ->
        ///  Z80 request, Z80 READY INT L will be low (triggered).
        /// </summary>
        public void LoadData(int data)
        {

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State,
                          "Z80 data port write: {0:x4} (byte {1:x2}), {2} items in queue.",
                          data, (byte)data, _inputFifo.Count);
#endif
            //
            // The 8th bit of the incoming data to this port is latched.
            // When set, the PERQ is requesting that the Z80 send a DataInReady interrupt when it is ready.
            // When cleared, the Z80 will clear any pending DataInReady interrupt.
            //
            if ((data & 0x100) == 0)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 DataInReady interrupt disabled, clearing interrupt.");
#endif
                PERQCpu.Instance.ClearInterrupt(InterruptType.Z80DataInReady);
                _dataReadyInterruptRequested = false;
            }
            else
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 DataInReady interrupt enabled.");
#endif
                _dataReadyInterruptRequested = true;
            }

            // Only queue up this write if we're actually running.
            if (_running)
            {
                _inputFifo.Enqueue((byte)data);
            }
        }

        /// <summary>
        /// Reads a byte from the Z80's FIFO, used for non-DMA transfers.
        /// Corresponds to IOB port 0x46 (106 oct).
        /// </summary>
        public int ReadData()
        {
            int retVal = 0;

            if (_outputFifo.Count > 0)
            {
                retVal = _outputFifo.Dequeue();

#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Z80State, "Z80 FIFO read {0:x2}, {1:x4} items left in queue.",
                                                retVal, _outputFifo.Count);
#endif
            }
            else
            {
                //
                // If the output queue is now empty, clear the output ready interrupt.
                // This serves to shut off interrupts even when the Z80 is "off" (see
                // SysB code snippet above); normally our Clock() handles it.
                //
                PERQCpu.Instance.ClearInterrupt(InterruptType.Z80DataOutReady);

#if TRACING_ENABLED
                if (Trace.TraceOn && _running)  // Don't whine needlessly :-)
                    Trace.Log(LogType.Warnings, "Z80 read from empty FIFO, returning 0.");
#endif
            }

            return retVal;
        }

        /// <summary>
        /// Clocks the Z80's state machine.
        ///
        /// Design note: If the Z80 is off, the hardware will still flip the interrupt status
        /// bits (see code snippet above from SysB) through reads and writes to the Z80 IO ports.
        /// Those cases are handled in Load/ReadData, with some duplication of the code here.  But
        /// it's silly to Poll all the attached devices during cycles when the Z80 is off.
        /// </summary>
        public void Clock()
        {
            if (!_running)
            {
                return;
            }

            _clocks++;

            //
            // Count down our busy timers, and if any devices come ready send an update
            //
            RefreshReadyState();

            //
            // Handle output first: Poll devices for new data.
            //
            for (int i = 0; i < _devices.Count; i++)
            {
                if (_devices[i].BusyClocks == 0)      // Are we busy?
                {
                    _devices[i].Poll(ref _outputFifo);
                }
            }

            // If we have data available in the FIFO we'll interrupt...
            if (_outputFifo.Count > 0)
            {
                PERQCpu.Instance.RaiseInterrupt(InterruptType.Z80DataOutReady);
            }
            else
            {
                PERQCpu.Instance.ClearInterrupt(InterruptType.Z80DataOutReady);
            }

            //
            // Now deal with any input we may have received
            //
            ParseInputFifo();

            //
            // If the input FIFO is empty, we will interrupt if the PERQ has asked us to.
            //
            if (_inputFifo.Count == 0 && _dataReadyInterruptRequested)
            {
                PERQCpu.Instance.RaiseInterrupt(InterruptType.Z80DataInReady);
            }
        }

        /// <summary>
        /// Reads the input FIFO and runs the PERQ->Z80 protocol, dispatching incoming
        /// commands to the correct device, and setting the ready status accordingly.
        /// </summary>
        private void ParseInputFifo()
        {
            if (_inputFifo.Count == 0)
            {
                return;
            }

            byte data = _inputFifo.Dequeue();

            switch (_state)
            {
                case MessageParseState.WaitingForSOM:
                    if (data == SOM)
                    {
                        _state = MessageParseState.MessageType;
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.Z80State, "Byte was SOM, transitioning to MessageType state.");
                    }
                    else if (data == 0xaa)
                    {
                        if (Trace.TraceOn)
                            Trace.Log(LogType.Warnings,
                                     "SOM byte sent to Z80 was 0xAA; this boot disk was likely meant for a PERQ 2.");
                    }
                    else
                    {
                        // Lots of these are spurious; seems the microcode sends a zero byte to turn off the
                        // Z80In interrupt, which the Z80 ignores.  We could probably check and ignore these
                        // in LoadData(), not bothering to queue them up in the first place.  But for now,
                        // just have to ignore 'em.  Maybe only log non-zero bytes here?  Those would indicate
                        // a situation worth investigating...
                        if (Trace.TraceOn && data != 0)
                            Trace.Log(LogType.Z80State,
                                     "Non-SOM byte sent to Z80 subsystem while waiting for SOM: {0:x2}", data);
#endif
                    }
                    break;

                case MessageParseState.MessageType:
                    _messageType = (PERQtoZ80Message)data;
#if TRACING_ENABLED
                    if (Trace.TraceOn)
                        Trace.Log(LogType.Z80State,
                                 "Z80 Message type is {0}, transitioning to Message state.", _messageType);
#endif
                    _state = MessageParseState.Message;
                    break;

                case MessageParseState.Message:
                    //
                    // The IOB's message format ("Old Z80") is really really annoying to parse, especially as
                    // compared to that of the CIO/EIO board ("New Z80").  Once we've gotten the message type,
                    // the length of the message is dependent on the type of the message sent.  Some have fixed
                    // lengths, some are variable.  Basically this requires a state machine for each individual
                    // message... so we just hand the bytes off to the target device and let it cope its own way. :)
                    //
                    bool done = false;

                    switch (_messageType)
                    {
                        case PERQtoZ80Message.SetKeyboardStatus:
                            done = _keyboard.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.SetFloppyStatus:
                        case PERQtoZ80Message.FloppyCommand:
                        case PERQtoZ80Message.FloppyBoot:
                            done = _floppyDisk.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.HardDriveSeek:
                            done = _hardDiskSeek.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.GPIBCommand:
                            done = _gpib.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.SetTabletStatus:
                            done = _tablet.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.SetRS232Status:
                        case PERQtoZ80Message.RS232:
                            done = _rs232.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.Speech:
                            done = _speech.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.SetClockStatus:
                            done = _clockDev.RunStateMachine(_messageType, data);
                            break;

                        case PERQtoZ80Message.GetStatus:
                            GetStatus(data);
                            done = true;
                            break;

                        default:
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.Warnings, "Unhandled Z80 message type {0}", _messageType);
#endif
                            // oof.  do we just bail here?  wait for som?
                            break;
                    }

                    if (done)
                    {
                        _state = MessageParseState.WaitingForSOM;
#if TRACING_ENABLED
                        if (Trace.TraceOn)
                            Trace.Log(LogType.Z80State, "{0} message complete.  Returning to WaitingForSOM state.", _messageType);
#endif
                    }
                    break;

                default:
                    throw new InvalidOperationException("Invalid Z80 message parsing state.");
            }
        }

        /// <summary>
        /// Updates the ready state for busy devices.
        /// </summary>
        private void RefreshReadyState()
        {
            ReadyFlags oldFlags = _deviceReadyState;
            _deviceReadyState = ReadyFlags.Z80;         // Z80 is always available...

            for (int i = 0; i < _devices.Count; i++)
            {
                if (_devices[i].BusyBit != 0)           // Skip devices without a Ready state
                {
                    if (_devices[i].BusyClocks != 0)    // If they're busy, clock 'em
                    {
                        _devices[i].BusyClocks--;
                    }

                    if (_devices[i].BusyClocks == 0)    // If they aren't busy now, set Ready bit
                    {
                        _deviceReadyState |= _devices[i].BusyBit;
                    }
                }
            }

            if (oldFlags != _deviceReadyState)          // If something changed, tell the PERQ
            {
                SendStatusChange();
            }
        }


        /// <summary>
        /// Sends the status change message.
        /// </summary>
        private void SendStatusChange()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State, "Sending Z80 device ready status: {0}.", _deviceReadyState);
#endif
            _outputFifo.Enqueue(Z80System.SOM);             // SOM
            _outputFifo.Enqueue((byte)Z80toPERQMessage.Z80StatusChange);
            _outputFifo.Enqueue((byte)_deviceReadyState);   // Data
        }

        /// <summary>
        /// Calls subdevices to retrieve status based on the requested flags.  This
        /// is a one-byte command (no count byte).
        /// </summary>
        private void GetStatus(byte requested)
        {
            Console.WriteLine("GetStatus: data={0}", requested);  // FIXME once debugged...

            for (int i = 1; i <= 256; i = (i << 1))
            {
                if ((requested & i) != 0)
                {
                    switch ((DeviceStatus)i)
                    {
                        case DeviceStatus.RS232:
                            _rs232.GetStatus(ref _outputFifo);
                            break;

                        case DeviceStatus.Floppy:
                            _floppyDisk.GetStatus(ref _outputFifo);
                            break;

                        case DeviceStatus.GPIB:
                            _gpib.GetStatus(ref _outputFifo);
                            break;

                        // This is likely extraneous, but it's part of the spec.
                        case DeviceStatus.Z80:
                            Console.WriteLine("Z80 GetStatus bit set");     // fixme
                            break;

                        default:
                            Console.WriteLine("Unhandled GetStatus type {0}", (DeviceStatus)i); // fixme
                            break;
                    }
                }
            }
        }

#if DEBUG
        public void ShowReadyState()
        {
            Console.WriteLine("Z80 Device Status:");
            for (int i = 0; i < _devices.Count; i++)
            {
                if (_devices[i].BusyBit != 0)       // Skip devices that don't maintain a busy flag
                {
                    Console.Write("\t{0} is ", _devices[i].BusyBit);
                    if (_devices[i].BusyClocks != 0)
                        Console.WriteLine("busy ({0} ticks).", _devices[i].BusyClocks);
                    else
                        Console.WriteLine("ready.");
                }
            }
            Console.WriteLine("--> Check: {0}", _deviceReadyState);
        }
#endif

        private enum MessageParseState
        {
            Illegal = 0,
            WaitingForSOM,
            MessageType,
            Message
        }


        [Flags]
        private enum DeviceStatus   // "Bits in Status Request Byte" per v87.z80
        {
            RS232 = 0x1,
            Tablet = 0x2,
            Keyboard = 0x4,
            Voltage = 0x8,          // Speech, as of v8.6 (voltage/temp removed)
            Clock = 0x10,
            Floppy = 0x20,
            GPIB = 0x40,
            Z80 = 0x80              // "Ready flag"
        }

        // Start of (most) messages sent from PERQ CPU <==> the Z80.
        public static readonly byte SOM = 0x6b;

        // IOB Z80's clock rate, ~ 2.5Mhz
        public static readonly int Frequency = 2456700;

        // Whether the Z80 has been started or not
        private bool _running;

        // Counts clock ticks when running (for computing 60Hz "jiffies")
        private int _clocks;

        private MessageParseState _state;
        private PERQtoZ80Message _messageType;
        private ReadyFlags _deviceReadyState;
        private bool _dataReadyInterruptRequested;

        // Devices:
        private List<IZ80Device> _devices;
        private HardDiskSeekControl _hardDiskSeek;
        private FloppyController _floppyDisk;
        private Keyboard _keyboard;
        private GPIB _gpib;
        private Tablet _tablet;
        private RS232 _rs232;
        private Speech _speech;
        private Clock _clockDev;

        // FIFO for Z80 data out (to PERQ)
        private Queue<byte> _outputFifo;

        // FIFO for Z80 data in (from PERQ)
        private Queue<byte> _inputFifo;

        private static Z80System _instance = new Z80System();
    }
}
