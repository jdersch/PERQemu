using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
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

                default:
                    throw new NotImplementedException(String.Format("DMA not implemented for device {0}.", device));
            }

#if TRACING_ENABLED
            if (device != SelectedDMADevice.None)
            {
                if (Trace.TraceOn) Trace.Log(LogType.Z80DMA, "Selected DMA device {0}", device);
            }
#endif
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
