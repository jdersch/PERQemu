// realport.cs - Copyright 2006-2016 Josh Dersch (derschjo@gmail.com)
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
using System.IO.Ports;

namespace PERQemu.IO.SerialDevices
{
    /// <summary>
    /// Represents a real host-supplied physical Serial port.  Basically encapsulates
    /// a System.IO.Ports.SerialPort to talk to a real serial port.
    /// </summary>
    [Serializable]
    public class PhysicalPort : ISerialDevice
    {
        public PhysicalPort(string portName)
        {
            _portName = portName;
            Reset();
        }

        public void Reset()
        {
            if (_physicalPort != null)
            {
                _physicalPort.Close();
            }

            // TODO: make configurable
            _physicalPort = new SerialPort();
            _physicalPort.DataReceived += new SerialDataReceivedEventHandler(OnDataReceived);
            _physicalPort.Handshake = Handshake.None;
            _physicalPort.DtrEnable = true;
            _physicalPort.RtsEnable = true;
            _physicalPort.PortName = _portName;

            _inputQueue = new Queue<byte>(128);
        }

        public string Port
        {
            set { _physicalPort.PortName = value; }
            get { return _physicalPort.PortName; }
        }

        public int BaudRate
        {
            get { return _physicalPort.BaudRate; }
            set { _physicalPort.BaudRate = value; }
        }

        public Parity Parity
        {
            get { return _physicalPort.Parity; }
            set { _physicalPort.Parity = value; }
        }

        public StopBits StopBits
        {
            get { return _physicalPort.StopBits; }
            set { _physicalPort.StopBits = value; }
        }

        public int DataBits
        {
            get { return _physicalPort.DataBits; }
            set { _physicalPort.DataBits = value; }
        }

        public int ByteCount
        {
            get { return _inputQueue.Count; }
        }

        public bool IsOpen
        {
            get { return _physicalPort.IsOpen; }
        }

        public void Open()
        {
            _physicalPort.Open();
        }

        public void Close()
        {
            _physicalPort.Close();
        }

        public byte ReadByte()
        {
            if (_inputQueue.Count == 0)
            {
                throw new InvalidOperationException("Serial port queue is empty on read!");
            }

            return _inputQueue.Dequeue();
        }

        public void Write(byte[] data, int index, int length)
        {
            _physicalPort.Write(data, index, length);
        }

        private void OnDataReceived(object sender, SerialDataReceivedEventArgs e)
        {
            // TODO: this is ridiculously inefficient.
            for (int i = 0; i < _physicalPort.BytesToRead; i++)
            {
                _inputQueue.Enqueue((byte)(_physicalPort.ReadByte()));
            }
        }

        private string _portName;

        [NonSerialized]
        private SerialPort _physicalPort;

        private Queue<byte> _inputQueue;
    }
}

