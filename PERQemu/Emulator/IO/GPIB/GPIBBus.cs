//
// GPIBBus.cs - Copyright (c) 2006-2022 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

using PERQemu;

namespace PERQemu.IO.GPIB
{
    /// <summary>
    /// GPIB control/status lines.
    /// </summary>
    [Flags]
    public enum BusStatus : byte
    {
        ATN = 0x80,
        DAV = 0x40,
        NDAC = 0x20,
        NRFD = 0x10,
        EOI = 0x08,
        SRQ = 0x04,
        IFC = 0x02,
        REN = 0x01
    }

    /// <summary>
    /// Provides the plumbing for transferring data between GPIB devices.  Due
    /// to the weird nature of the GPIB, this class isn't a passive dispatcher
    /// like the PERQ or Z80 buses; it keeps track of talkers and listeners as
    /// set by the controller and "knows" how to map data from the current
    /// talker to the current listener -- which can include the controller
    /// itself when in "standby" mode.  This is a limited subset of the crazy
    /// baroque GPIB structure; it just allows one active talker and one active
    /// listener, since that's enough to make the PERQ work for now.
    /// </summary>
    public sealed class GPIBBus
    {
        /// <summary>
        /// Create a new GPIB with up to 31 devices.  Address 0 is reserved
        /// for the system controller (TMS9914A) and 31 is reserved as "nobody".
        /// </summary>
        public GPIBBus()
        {
            _deviceDispatch = new IGPIBDevice[32];
            _devices = new List<IGPIBDevice>();
        }

        public void Reset()
        {
            foreach (IGPIBDevice device in _devices)
            {
                Log.Debug(Category.GPIB, "Calling bus reset on device {0}", device.DeviceID);
                device.BusReset();
            }
        }

        public void AddDevice(IGPIBDevice device)
        {
            if (_devices.Contains(device))
            {
                throw new InvalidOperationException("This component has already been added to the GPIB bus");
            }

            _devices.Add(device);
            _deviceDispatch[device.DeviceID] = device;
            device.RegisterBusWriteDelegate(BusWrite);
        }

        /// <summary>
        /// Clocks any devices attached to the GPIB bus to allow them to enqueue
        /// data onto the bus.  todo: if necessary, actually implement polling...
        /// </summary>
        //public void Poll(ref Queue<byte> fifo)
        //{
        //    for (int i = 0; i < _devices.Count; i++)
        //    {
        //        _devices[i].Poll(ref fifo);
        //    }
        //}

        /// <summary>
        /// Transmit a byte over the GPIB from the Talker to the Listener.
        /// </summary>
        /// <remarks>
        /// This method is invoked by the Talker through its assigned delegate.
        /// Currently the only supported input device is the BitPadOne tablet,
        /// but in theory the PERQ had fairly complete and flexible GPIB support
        /// and could support additional peripherals.  For now if the provided
        /// Id of the invoker is not the current talker (i.e., the PERQ turned
        /// off the tablet between or during an update!), we just quietly drop
        /// the data.  Similarly, if nobody is listening, writes are ignored.
        /// </remarks>
        public void BusWrite(byte talkerId, byte value, BusStatus flags)
        {
            // UGH.  Hack this so the controller is the "listener of last resort"
            if (_listenerId == NOBODY) _listenerId = 0;

            if (_deviceDispatch[_listenerId] == null)
            {
                Log.Warn(Category.GPIB, "Bus write from {0} but no listener: {1} 0x{2:x2}",
                         talkerId, flags, value);
                return;
            }

            if (talkerId != _talkerId)
            {
                Log.Debug(Category.GPIB, "Spurious or old data ({0} 0x{1:x2}) from talker {1} ignored",
                          flags, value, talkerId);
                return;
            }

            Log.Debug(Category.GPIB, "Write to device {0:x2} ({1:x2}) handled by {2}",
                                     _listenerId, value, _deviceDispatch[_listenerId]);

            // Do the BusRead on the listener!  Should we send our talker id so
            // the listener knows where it came from?  Mayyyybe.  We can add that.
            _deviceDispatch[_listenerId].BusRead(value, flags);
        }

        /// <summary>
        /// When the controller receives a Talk Address Group command, send out
        /// the address to let our attached devices know who's the new talker.
        /// </summary>
        public void BroadcastTalker(byte addr)
        {
            // Think globally
            for (int i = 0; i < _devices.Count; i++)
            {
                _devices[i].SetTalker(addr);
            }

            // Update locally
            if (_deviceDispatch[addr] != null)
            {
                _talkerId = addr;
            }
            else
            {
                // This might not need to be logged, as 'untalk all' is valid
                // and this would just be extraneous noise.  same below.
                Log.Debug(Category.GPIB, "No talker device at {0}", addr);
                _talkerId = NOBODY;
            }
        }

        /// <summary>
        /// Broadcasts the new listener address in response to a Listen Address
        /// Group command.
        /// </summary>
        public void BroadcastListener(byte addr)
        {
            // Send it
            for (int i = 0; i < _devices.Count; i++)
            {
                _devices[i].SetListener(addr);
            }

            // Save it
            if (_deviceDispatch[addr] != null)
            {
                _listenerId = addr;
            }
            else
            {
                Log.Debug(Category.GPIB, "No listener device at {0}", addr);
                _listenerId = NOBODY;
            }
        }

        public void ControllerIsListener(bool doit)
        {
            _listenerId = doit ? (byte)0x0 : NOBODY;
        }

        // An illegal talker/listener ID
        private const byte NOBODY = 0x1f;

        private byte _talkerId;
        private byte _listenerId;

        // Dispatch table for device IO
        private IGPIBDevice[] _deviceDispatch;

        // The devices attached to the bus
        private List<IGPIBDevice> _devices;
    }
}
