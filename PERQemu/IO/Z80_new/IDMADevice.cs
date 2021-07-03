using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    public interface IDMADevice
    {
        /// <summary>
        /// Indicates that the device has data ready to read.
        /// </summary>
        bool ReadDataReady { get; }

        /// <summary>
        /// Reads a single byte from the DMA device
        /// </summary>
        /// <returns></returns>
        byte DMARead(ushort address);

        /// <summary>
        /// Indicates that the device is ready for data to be written
        /// </summary>
        bool WriteDataReady { get; }

        /// <summary>
        /// Writes a single byte to the DMA device
        /// </summary>
        /// <param name="value"></param>
        void DMAWrite(ushort address, byte value);

        /// <summary>
        /// Terminates the current transfer (equivalent to setting TC)
        /// </summary>
        void DMATerminate();
    }
}
