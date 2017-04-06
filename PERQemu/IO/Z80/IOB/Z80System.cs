// z80system.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.IO;
using System.Text;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Security.Permissions;

using PERQemu.CPU;
using PERQemu.IO.SerialDevices;

namespace PERQemu.IO.Z80.IOB
{

    public enum PERQtoZ80Message
    {
        Invalid         = 0x0,
        RS232           = 0x1,
        FloppyCommand   = 0x2,
        GPIBCommand     = 0x3,
        Speech          = 0x4,
        SetRS232Status  = 0x5,
        SetTabletStatus = 0x6,
        SetKeyboardStatus = 0x7,
        HardDriveSeek   = 0x8,
        SetSpeechStatus = 0x9,      // Z80 v8.7: was Voltage Set
        SetClockStatus  = 0xa,
        GetStatus       = 0xb,
        SetFloppyStatus = 0xc,
        FloppyBoot      = 0xd,
    }

    public enum Z80toPERQMessage
    {
        Invalid         = 0x0,      // "Old Z80 Output message numbers" (oioz80.mic)
        KeyboardData    = 0x1,
        RS232Data       = 0x2,
        TabletData      = 0x3,
        ClockData       = 0x4,
        FloppyData      = 0x5,
        GPIBData        = 0x6,
        RS232Status     = 0x7,      // bit 5 => message 5
        TabletStatus    = 0x8,      // message 6
        KeyboardStatus  = 0x9,      // message 7
        SeekComplete    = 0xa,
        Z80StatusChange = 0xb,
        VoltageData     = 0xc,
        VoltageStatus   = 0xd,      // message 11
        ClockStatus     = 0xe,      // message 12
        GPIBStatus      = 0xf,      // missing?
        FloppyStatus    = 0x10,     // message 14
        FloppyDone      = 0x11,
        FloppyBootError = 0x12,
        FloppyBootData  = 0x13,
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
    [Serializable]
    public sealed class Z80System : ISerializable
    {
        #region Serialization
        [SecurityPermissionAttribute(SecurityAction.LinkDemand,
         Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData(
            SerializationInfo info, StreamingContext context)
        {
            info.AddValue("state", _state);
            info.AddValue("messageType", _messageType);
            info.AddValue("messageData", _messageData);
            info.AddValue("deviceReadyState", _deviceReadyState);
            info.AddValue("deviceBusyClocks", _deviceBusyClocks);
            info.AddValue("frob", _dataReadyInterruptRequested);
            info.AddValue("floppyDisk", _floppyDisk);
            info.AddValue("keyboard", _keyboard);
            info.AddValue("gpib", _gpib);
            info.AddValue("tablet", _tablet);
            info.AddValue("rs232", _rs232);
            info.AddValue("speech", _speech);
            info.AddValue("clock", _clockDev);
            info.AddValue("fifo", _outputFifo);
            info.AddValue("clocks", _clocks);
        }

        private Z80System(SerializationInfo info, StreamingContext context)
        {
            _state = (MessageParseState)info.GetInt32("state");
            _messageType = (PERQtoZ80Message)info.GetInt32("messageType");
            _messageData = (byte[])info.GetValue("messageData", typeof(byte[]));
            _deviceReadyState = (ReadyFlags)info.GetInt32("deviceReadyState");
            _deviceBusyClocks = (int[])info.GetValue("deviceBusyClocks", typeof(int[]));
            _dataReadyInterruptRequested = info.GetBoolean("frob");
            _floppyDisk = (FloppyController)info.GetValue("floppyDisk", typeof(FloppyController));
            _keyboard = (Keyboard)info.GetValue("keyboard", typeof(Keyboard));
            _gpib = (GPIB)info.GetValue("gpib", typeof(GPIB));
            _tablet = (Tablet)info.GetValue("tablet", typeof(Tablet));
            _rs232 = (RS232)info.GetValue("rs232", typeof(RS232));
            _speech = (Speech)info.GetValue("speech", typeof(Speech));
            _clockDev = (Clock)info.GetValue("clock", typeof(Clock));
            _outputFifo = (Queue<byte>)info.GetValue("fifo", typeof(Queue<byte>));
            _clocks = info.GetInt32("clocks");

            _devices = new List<IZ80Device>(16);
            BuildDeviceList();
            _instance = this;
        }

        #endregion

        private Z80System()
        {
            _messageData = new byte[256];
            _outputFifo = new Queue<byte>(256);
            _inputFifo = new Queue<byte>(256);
            _devices = new List<IZ80Device>(16);
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

        public static Z80System Instance
        {
            get { return _instance; }
        }

        /// <summary>
        /// Elapsed clock cycles for the Z80 (~2.456Mhz).
        /// </summary>
        public int Clocks()
        {
            return _clocks;
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

            // All devices are ready
            _deviceReadyState = ReadyFlags.Z80 | ReadyFlags.Floppy | ReadyFlags.GPIB | ReadyFlags.RS232 | ReadyFlags.Seek | ReadyFlags.Speech;
            _dataReadyInterruptRequested = false;
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
        /// Corresponds to IOB port 41.
        /// The PERQ1 microcode uses port 41 to control both the hard drive and the Z80.
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
        /// <param name="data"></param>
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
                SendStatusChange();
            }
        }

        /// <summary>
        /// Corresponds to IOB port 47
        /// Sends data to the Z80.
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
        /// <param name="data"></param>
        public void LoadData(int data)
        {

#if TRACING_ENABLED
            if (Trace.TraceOn) Trace.Log(LogType.Z80State, "Z80 data port write: {0:x4}", data);
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
                if (Trace.TraceOn) Trace.Log(LogType.Z80State, "Z80 DataInReady interrupt enabled.");
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
        /// <returns></returns>
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
                // If the output queue is empty, clear the output ready interrupt.
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
            // Handle output first:
            // Poll devices for new data
            //
            for (int i = 0; i < _devices.Count; i++)
            {
                _devices[i].Poll(ref _outputFifo);
            }

            RefreshReadyState();

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
                    if( data == SOM )
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
                        if (Trace.TraceOn)
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
                    // The IOB's message format is really really annoying to parse, especially as compared
                    // to that of the CIO/EIO board.  Once we've gotten the message type, the length of the
                    // message is dependent on the type of the message sent.  Some have fixed lengths, some
                    // are variable.  Basically this requires a state machine for each individual message.
                    // Or I could make this code really tangled.
                    switch (_messageType)
                    {
                        case PERQtoZ80Message.SetKeyboardStatus:
                            if (_keyboard.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "Keyboard message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.FloppyBoot:
                        case PERQtoZ80Message.SetFloppyStatus:
                        case PERQtoZ80Message.FloppyCommand:
                            if (_floppyDisk.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
                                SetBusyState(ReadyFlags.Floppy);
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "Floppy message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.HardDriveSeek:
                            if (HardDisk.ShugartDiskController.Instance.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
                                SetBusyState(ReadyFlags.Seek);
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "Seek message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.GPIBCommand:
                            if (_gpib.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
                                SetBusyState(ReadyFlags.GPIB);
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "GPIB message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.SetTabletStatus:
                            if (_tablet.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "Tablet message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.SetRS232Status:
                        case PERQtoZ80Message.RS232:
                            if (_rs232.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
                                SetBusyState(ReadyFlags.RS232);
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "RS232 message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.Speech:
                            if (_speech.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
                                SetBusyState(ReadyFlags.Speech);
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "Speech message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        case PERQtoZ80Message.GetStatus:
                            GetStatus(data);
                            _state = MessageParseState.WaitingForSOM;
                            break;

                        case PERQtoZ80Message.SetClockStatus:
                            if (_clockDev.RunStateMachine(_messageType, data))
                            {
                                _state = MessageParseState.WaitingForSOM;
#if TRACING_ENABLED
                                if (Trace.TraceOn)
                                    Trace.Log(LogType.Z80State,
                                             "Clock message complete.  Returning to WaitingForSOM state.");
#endif
                            }
                            break;

                        default:
#if TRACING_ENABLED
                            if (Trace.TraceOn)
                                Trace.Log(LogType.Warnings, "Unhandled Z80 message type {0}", _messageType);
#endif
                            break;
                    }
                    break;

                default:
                    throw new InvalidOperationException("Invalid Z80 message parsing state.");
            }
        }

        /// <summary>
        /// Updates the ready state for busy devices
        /// </summary>
        private void RefreshReadyState()
        {
            bool statusChange = false;

            // This is slow and terrible.
            for (int i = 0; i < 17; i++)    // 17?
            {
                if (_deviceBusyClocks[i] != 0)
                {
                    _deviceBusyClocks[i]--;
                    if (_deviceBusyClocks[i] == 0)
                    {
                        statusChange = true;
                        SetReadyState((ReadyFlags)i);
                    }
                }
            }

            if (statusChange)
            {
                SendStatusChange();
            }
        }

        private void SendStatusChange()
        {
#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State, "Sending Z80 device status change message {0}.", _deviceReadyState);
#endif
            _outputFifo.Enqueue(Z80System.SOM);             // SOM
            _outputFifo.Enqueue((byte)Z80toPERQMessage.Z80StatusChange);
            _outputFifo.Enqueue((byte)_deviceReadyState);   // Data
        }

        /// <summary>
        /// Puts the specified device into the ready state.
        /// </summary>
        /// <param name="device"></param>
        private void SetReadyState(ReadyFlags device)
        {
            _deviceReadyState |= device;

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State, "Ready state for {0} set.  ReadyState now {1}.", device, _deviceReadyState);
#endif
        }

        /// <summary>
        /// Puts the specified device into the busy state
        /// </summary>
        /// <param name="device"></param>
        private void SetBusyState(ReadyFlags device)
        {
            _deviceReadyState &= (~device);

            _deviceBusyClocks[(int)device] = 50;            // Made up delay time

            _outputFifo.Enqueue(Z80System.SOM);             // SOM
            _outputFifo.Enqueue((byte)Z80toPERQMessage.Z80StatusChange);
            _outputFifo.Enqueue((byte)_deviceReadyState);   // Data

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.Z80State, "Busy state for {0} set.  ReadyState now {1}.", device, _deviceReadyState);
#endif
        }

        /// <summary>
        /// Calls subdevices to retrieve status based on the requested flags.
        /// </summary>
        /// <param name="requested"></param>
        private void GetStatus(byte requested)
        {
            Console.WriteLine("GetStatus: {0}", requested);

            for (int i = 1; i <= 256; i = (i << 1))
            {
                if ((requested & i) != 0)
                {
                    Console.WriteLine("Checking: {0}", i);
                    switch ((DeviceStatus)i)
                    {
                        // Hmm.  According to PerqIO.Pas, Tablet Status isn't used.  Sigh.
                        case DeviceStatus.Tablet:
                            _tablet.GetStatus(ref _outputFifo);
                            break;

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
                            Console.WriteLine("Z80 GetStatus bit set");
                            break;

                        default:
                            Console.WriteLine("Unhandled GetStatus type {0}", (DeviceStatus)i);
                            break;
                    }
                }
            }
        }

        private void BuildDeviceList()
        {
            _devices.Clear();
            _devices.Add(_floppyDisk);
            _devices.Add(_keyboard);
            _devices.Add(_gpib);
            _devices.Add(_tablet);
            _devices.Add(_rs232);
            _devices.Add(_speech);
            _devices.Add(_clockDev);
        }

        private enum MessageParseState
        {
            Illegal = 0,
            WaitingForSOM,
            MessageType,
            Message
        }

        [Flags]
        private enum ReadyFlags
        {
            RS232 = 0x1,
            Speech = 0x2,
            Floppy = 0x4,
            GPIB = 0x8,
            Seek = 0x10,
            Z80 = 0x80
        }

        [Flags]
        private enum DeviceStatus   // "Z80 Output request bits - Old protocol" (oioz80.mic)
        {
            RS232 = 0x1,            // OZ80DoSRS232 = #40 = 0x20    -> bit 5/message 5
            Tablet = 0x2,           // OZ80DoSTablet = #100 = 0x40  -> message 6
            Keyboard = 0x4,         // OZ80DoSKeybd = #200 = 0x80   -> message 7
            Voltage = 0x8,          // OZ80DoSVoltage = #1000 = 0x200 -> message 11 (unused)
            Clock = 0x10,           // OZ80DoSClock = #2000 = 0x400 -> message 12
            Floppy = 0x20,          // OZ80DoSFloppy = #10000 = 0x1000 -> message 14
            GPIB = 0x40,            // No GetStat for GPIB...
            Z80 = 0x80              // OZ80DoGetStat = #100000 = 0x8000 "Always on"
        }


        public static readonly byte SOM = 0x6b;         // Start of (most) messages sent from PERQ CPU <==> the Z80.
        public static readonly int Frequency = 2456700; // IOB Z80's clock rate, ~ 2.5Mhz

        // Whether the Z80 has been started or not
        private bool _running;

        // Counts clock ticks when running (for computing 60Hz "jiffies")
        private int _clocks;

        private MessageParseState _state;
        private PERQtoZ80Message  _messageType;
        private byte[] _messageData;
        private ReadyFlags _deviceReadyState;
        private int[] _deviceBusyClocks = new int[256];
        private bool _dataReadyInterruptRequested;

        // Devices:
        private List<IZ80Device> _devices;
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
