using Konamiman.Z80dotNet;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace PERQemu.IO.Z80_new
{
    public interface IZ80Device : IZ80InterruptSource
    {
        string Name { get; }
        byte[] Ports { get; }

        void Reset();
        byte Read(byte portAddress);
        void Write(byte portAddress, byte value);

    }
}
