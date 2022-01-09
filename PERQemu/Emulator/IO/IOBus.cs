//
// IOBus.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
using System.Collections.Generic;

using PERQemu.Memory;

namespace PERQemu.IO
{
    public class UnhandledIORequestException : Exception
    {
        public UnhandledIORequestException(string message)
            : base(message)
        {

        }
    }

    /// <summary>
    /// IOBus acts as an arbiter between the PERQ CPU and various devices
    /// attached to the system.  It dispatches IO reads and writes to the
    /// correct devices.  It does not at this time deal with details like
    /// arbitration and timing.
    /// </summary>
    public sealed class IOBus
    {
        public IOBus()
        {
            _deviceDispatch = new IIODevice[256];
            _devices = new List<IIODevice>(16);
        }

        public void Reset()
        {
            foreach (IIODevice device in _devices)
            {
                device.Reset();
            }
        }

        public void AddDevice(IIODevice device)
        {
            if (device == null) return;

            if (_devices.Contains(device))
            {
                throw new InvalidOperationException("This component has already been added to the bus.");
            }

            _devices.Add(device);

            UpdateDispatchTable(device);
        }

        public void IOWrite(byte ioPort, int value)
        {
            value &= 0xffff;    // IOD is 16 bits wide; trim upper bits

            if (_deviceDispatch[ioPort] == null)
            {
                Log.Warn(Category.IO, "No device registered for port {0:x2} write ({1:x4})", ioPort, value);
                return;
            }

            if (!(_deviceDispatch[ioPort] is VideoController))  // This generates a lot of output
                Log.Debug(Category.IO, "Output sent to port {0:x2} ({1:x4}) handled by {2}",
                          ioPort, value, _deviceDispatch[ioPort]);

            _deviceDispatch[ioPort].IOWrite(ioPort, value);
        }

        public int IORead(byte ioPort)
        {
            if (_deviceDispatch[ioPort] == null)
            {
                Log.Warn(Category.IO, "No device registered for port {0:x2} read, returning 0", ioPort);
                return 0;
            }

            if (!(_deviceDispatch[ioPort] is VideoController))
                Log.Debug(Category.IO, "Input request from port {0:x2} handled by {1}",
                          ioPort, _deviceDispatch[ioPort]);

            int retVal = _deviceDispatch[ioPort].IORead(ioPort);

            if (!(_deviceDispatch[ioPort] is VideoController))
                Log.Debug(Category.IO, "Input received is {0:x4}", retVal);

            return retVal;
        }


        /// <summary>
        /// Adds a new device to the dispatch table.
        /// Will throw if conflicts are found.
        /// </summary>
        private void UpdateDispatchTable(IIODevice device)
        {
            Log.Debug(Category.IO, "Updating dispatch table for device {0}", device);

            for (int i = 0; i < 255; i++)
            {
                if (device.HandlesPort((byte)i))
                {
                    if (_deviceDispatch[i] != null)
                    {
                        throw new InvalidOperationException(
                            string.Format("IO Port conflict at {0:x2} between {1} and {2}",
                                           i, device, _deviceDispatch[i]));
                    }
                    else
                    {
                        _deviceDispatch[i] = device;
                    }
                }
            }
        }

        /// <summary>
        /// Dispatch table for device IO
        /// </summary>
        private IIODevice[] _deviceDispatch;

        /// <summary>
        /// The devices attached to the bus
        /// </summary>
        private List<IIODevice> _devices;
    }
}
