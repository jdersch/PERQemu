// gpibbus.cs - Copyright 2006-2018 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.GPIB
{
    public sealed class GPIBBus
    {
        private GPIBBus()
        {
            _deviceDispatch = new IGPIBDevice[16];
            _devices = new List<IGPIBDevice>(16);

            //
            // Attach devices.  This should eventually be configurable,
            // though I have no idea what aside from the BitPadOne digitizer we'd
            // ever expect to support.  (Actually, the POS Print command supports
            // at least one GPIB-connected printer... dot-matrix baby!)
            //
            AddDevice(new BitPadOne());
            Reset();
        }

        public static GPIBBus Instance
        {
            get { return _instance; }
        }

        public void Reset()
        {
            foreach (IGPIBDevice device in _devices)
            {
                device.Reset();
            }
        }

        public void AddDevice(IGPIBDevice device)
        {
            if (_devices.Contains(device))
            {
                throw new InvalidOperationException("This component has already been added to the GPIB bus.");
            }

            _devices.Add(device);
            _deviceDispatch[device.DeviceID] = device;
        }

        /// <summary>
        /// Clocks any devices attached to the GPIB bus to allow them to enqueue data onto the bus.
        /// </summary>
        public void Poll(ref Queue<byte> fifo)
        {
            for (int i = 0; i < _devices.Count; i++)
            {
                _devices[i].Poll(ref fifo);
            }
        }

        /// <summary>
        /// Write a byte to the specified device on the bus (which should be selected as the listener!)
        /// </summary>
        public void Write(byte deviceId, byte value)
        {
            if (_deviceDispatch[deviceId] == null)
            {
#if TRACING_ENABLED
                if (Trace.TraceOn)
                    Trace.Log(LogType.Warnings, "No device is registered for GPIB ID {0:x2} write ({1:x2})",
                              deviceId, value);
#endif
                return;
            }

#if TRACING_ENABLED
            if (Trace.TraceOn)
                Trace.Log(LogType.GPIB, "Output sent to GPIB device {0:x2} ({1:x2}) handled by {2}",
                          deviceId, value, _deviceDispatch[deviceId]);
#endif

            _deviceDispatch[deviceId].Write(value);
        }

        /// <summary>
        /// When the controller receives a Talk Address Group command, send out the address
        /// to let our attached devices know who's the new talker.
        /// </summary>
        public void BroadcastTalker(byte addr)
        {
            for (int i = 0; i < _devices.Count; i++)
            {
                _devices[i].SetTalker(addr);
            }
        }

        /// <summary>
        /// Broadcasts the new listener address in response to a Listen Address Group command.
        /// </summary>
        public void BroadcastListener(byte addr)
        {
            for (int i = 0; i < _devices.Count; i++)
            {
                _devices[i].SetListener(addr);
            }
        }

        // Dispatch table for device IO
        private IGPIBDevice[] _deviceDispatch;

        // The devices attached to the bus
        private List<IGPIBDevice> _devices;

        private static GPIBBus _instance = new GPIBBus();
    }
}
