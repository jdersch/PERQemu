using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    public delegate void ReceiveDelegate(byte rxValue);
    /// <summary>
    /// Generic interface for devices connected to a Z80 SIO controller.
    /// "Transmit" functions are FROM the SIO, TO the device implementation; likewise
    /// "Receive" functions are FROM the device, TO the SIO.
    /// </summary>
    public interface ISIODevice
    {
        /// <summary>
        /// Registers a delegate used by the ISIODevice implementation to send data to the
        /// SIO's receiver.
        /// </summary>
        /// <param name="rxDelegate"></param>
        void RegisterReceiveDelegate(ReceiveDelegate rxDelegate);

        /// <summary>
        /// Writes a byte to the SIO device
        /// </summary>
        /// <param name="value"></param>
        void Transmit(byte value);

        /// <summary>
        /// Sends an SDLC Abort to the device
        /// </summary>
        void TransmitAbort();

        /// <summary>
        /// Sends an RS232 Break to the device
        /// </summary>
        void TransmitBreak();
    }
}
