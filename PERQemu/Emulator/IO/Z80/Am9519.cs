//
// Am9519.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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

namespace PERQemu.IO.Z80
{
    /// <summary>
    /// The AMD Am9519 interrupt priority encoder, used by the EIO Z80.
    /// </summary>
    public class Am9519 : IZ80Device
    {
        public Am9519(byte baseAddress)
        {
            _baseAddress = baseAddress;
            _ports = new byte[] { _baseAddress, (byte)(_baseAddress + 1) };
        }

        public string Name => "Am9519";
        public byte[] Ports => _ports;

        public bool IntLineIsActive
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public byte? ValueOnDataBus
        {
            get
            {
                throw new NotImplementedException();
            }
        }

        public event EventHandler NmiInterruptPulse { add { } remove { } }

        public void Reset()
        {
            Log.Debug(Category.Z80IRQ, "Am9519 interrupt controller reset");
        }

        public byte Read(byte portAddress)
        {
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException();
        }

        byte _baseAddress;
        byte[] _ports;
    }
}

/*
    Notes:
    
    ;
    ; Interrupt chip definitions
    ;
    INTCSR      equ 141Q    ; Interrupt controller control/status register
    I.PDMA      equ 00H     ; DMA to/from PERQ done interrupt
    I.PERQI     equ 01H     ; Data available in From PERQ FIFO
    I.PERQO     equ 02H     ; Data just read from To PERQ FIFO
    I.Floppy    equ 03H     ; Floppy interrupt
    I.GPIB      equ 04H     ; GPIB interrupt
    I.ZDMA      equ 05H     ; End of Process by DMA controller

    I.Single    equ 08H     ; Select single level
    I.CIMRIRR   equ 010H    ; Clear IMR and IRR
    I.CIMR      equ 020H    ; Clear IMR
    I.SIMR      equ 030H    ; Set IMR
    I.CIRR      equ 040H    ; Clear IRR
    I.SIRR      equ 050H    ; Set IRR
    I.CHISR     equ 060H    ; Clear highest ISR bit
    I.CISR      equ 070H    ; Clear ISR
    I.M01234    equ 080H    ; Load mode bits 0..4
    I.Fixed     equ 000H    ; Set for fixed priority
    I.Rotate    equ 001H    ; Set for rotating priority
    I.Indiv     equ 000H    ; Individual vectors
    I.Common    equ 002H    ; Common vector for group
    I.Interrupt equ 000H    ; Operate in interrupt move
    I.Polled    equ 004H    ; Operate in polled mode
    I.GrpLow    equ 000H    ; Group interrupt is active low
    I.GrpHigh   equ 008H    ; Group interrupt is active low
    I.ReqLow    equ 000H    ; Interrupt request is active low
    I.ReqHigh   equ 010H    ; Interrupt request is active high
    I.M567      equ 0A0H    ; Load Mode bits 5,6,7
    I.Arm       equ 001H    ; Arm interrupt chip
    I.DisArm    equ 002H    ; Disarm interrupt chip
    I.RdISR     equ 000H    ; Preselect to read ISR
    I.RdIMR     equ 004H    ; Preselect to read IMR
    I.RdIRR     equ 008H    ; Preselect to read IRR
    I.RdACR     equ 00CH    ; Preselect to read ACR
    I.WrIMR     equ 0B0H    ; Select IMR
    I.WrACR     equ 0C0H    ; Select ACR
    I.WrRES     equ 0E0H    ; Select response memory
    I.BY1       equ 00H     ; readback 1 byte
    I.BY2       equ 08H     ; readback 2 bytes
    I.BY3       equ 10H     ; readback 3 bytes
    I.BY4       equ 18H     ; readback 4 bytes

    INTDATA     equ 140Q    ; Interrupt controller data

*/
