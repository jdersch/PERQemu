//
// IOReg3.cs - Copyright (c) 2006-2024 Josh Dersch (derschjo@gmail.com)
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
    /// <summary>
    /// Implements I/O Register 3, a single byte I/O device connected to the Z80
    /// that is used to control DMA and interrupts for various IOB devices.
    /// </summary>
    public class IOReg3 : IZ80Device
    {
        public IOReg3(PERQToZ80Latch perqZ80fifo, Keyboard keyboard, NECuPD765A fdc, DMARouter dmaRouter)
        {
            _perqZ80fifo = perqZ80fifo;
            _keyboard = keyboard;
            _fdc = fdc;
            _dmaRouter = dmaRouter;
        }

        public void Reset()
        {
        }

        public string Name  => "I/O REG 3";
        public byte[] Ports => _ports;
        public byte? ValueOnDataBus => null;
        public bool IntLineIsActive => false;
        public bool InterruptsEnabled => false;

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        public byte Read(byte portAddress)
        {
            throw new NotImplementedException("IOReg3 port read");
        }

        public void Write(byte portAddress, byte value)
        {
            //
            // Configure DMA.  From v87.z80:
            //
            //  D.FLOP EQU     1 * 40Q; DMA TO FLOPPY
            //  D.PRQR EQU     2 * 40Q; DMA TO PERQ READ
            //  D.PRQW EQU     3 * 40Q; DMA TO PERQ WRITE
            //  D.SIOA EQU     4 * 40Q; DMA TO SIO CHANNEL A
            //  D.SIOB EQU     5 * 40Q; DMA TO SIO CHANNEL B
            //  D.GPIB EQU     6 * 40Q; DMA TO GPIB
            //
            _dmaRouter.SelectDMADevice((SelectedDMADevice)((value & 0xe0) >> 5));

            //
            // Dole out Interrupt enables.  From v87.z80:
            //
            // PRQENB  EQU     4  ;PERQ INTERRUPT ENABLE
            // KBDENB  EQU     2  ;KBD INTERRUPT ENABLE
            // FLPENB  EQU     1  ;FLOPPY INTERRUPT ENABLE
            //
            _perqZ80fifo.InterruptsEnabled = ((value & 0x04) != 0);
            _keyboard.InterruptsEnabled = ((value & 0x02) != 0);
            _fdc.InterruptsEnabled = ((value & 0x01) != 0);
        }


        byte[] _ports = { 0xc8 };

        PERQToZ80Latch _perqZ80fifo;
        Keyboard _keyboard;
        NECuPD765A _fdc;
        DMARouter _dmaRouter;
    }
}
