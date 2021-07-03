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
        /// Indicates that the device is ready for data to be written
        /// </summary>
        bool WriteDataReady { get; }

        /// <summary>
        /// Terminates the current transfer (equivalent to setting TC)
        /// </summary>
        void DMATerminate();
    }
}
