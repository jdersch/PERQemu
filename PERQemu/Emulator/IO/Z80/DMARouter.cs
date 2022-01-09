//
// DMARouter.cs - Copyright (c) 2006-2021 Josh Dersch (derschjo@gmail.com)
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
    public enum SelectedDMADevice
    {
        None = 0,
        Floppy = 1,
        PERQReadFIFO = 2,
        PERQWriteFIFO = 3,
        SIOA = 4,
        SIOB = 5,
        GPIB = 6,
    }

    /// <summary>
    /// Routes DMA requests to PERQ IOB's Z80 DMA-capable devices as controlled by IOReg3.  These are:
    /// Floppy, PERQ Read and Write FIFOs, SIO channel A and B, and GPIB.
    /// </summary>
    public class DMARouter : IDMADevice
    {
        public DMARouter(Z80System system)
        {
            _system = system;
        }

        public void SelectDMADevice(SelectedDMADevice device)
        {
            switch (device)
            {
                case SelectedDMADevice.None:
                    // No change
                    break;

                case SelectedDMADevice.Floppy:
                    _selectedDevice = _system.FDC;
                    break;

                case SelectedDMADevice.SIOB:
                    _selectedDevice = _system.SIOA;
                    break;

                default:
                    throw new NotImplementedException(string.Format("DMA not implemented for device {0}.", device));
            }

            if (device != SelectedDMADevice.None)
            {
                Trace.Log(LogType.Z80DMA, "Selected DMA device {0}", device);
            }
        }

        public bool ReadDataReady => _selectedDevice.ReadDataReady;
        public bool WriteDataReady => _selectedDevice.WriteDataReady;

        public void DMATerminate()
        {
            _selectedDevice.DMATerminate();
        }

        private Z80System _system;
        private IDMADevice _selectedDevice;
    }
}
