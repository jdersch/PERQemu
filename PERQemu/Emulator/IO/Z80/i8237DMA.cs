//
// i8237DMA.cs - Copyright (c) 2006-2023 Josh Dersch (derschjo@gmail.com)
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
    /// AMD Am9517 (aka Intel i8237) DMA chip.
    /// </summary>
    /// <remarks>
    /// This four-channel DMA controller is used on the EIO board, replacing
    /// the single-channel Z80 DMA chip from the older IO boards.  This chip
    /// uses two blocks of addresses: control/status registers in one block,
    /// and four address/word count registers in a second.
    /// 
    /// While programmable, these tend to be fixed on the EIO:
    ///     Chn 0 - Floppy      Chn 2 - SIO
    ///     Chn 1 - GPIB        Chn 3 - PERQ
    /// </remarks>
    public class i8237DMA : IZ80Device
    {
        public i8237DMA(byte csrBase, byte chnBase)
        {
            _controlBase = csrBase;
            _channelBase = chnBase;
            _ports = new byte[16];

            // Register both blocks with the Z80
            for (int i = 0; i < 8; i++)
            {
                _ports[i] = (byte)(_controlBase + i);
                _ports[i + 8] = (byte)(_channelBase + i);
            }
        }

        public string Name => "i8237 DMA";
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
            Log.Debug(Category.Z80DMA, "i8237 reset");
        }

        public byte Read(byte portAddress)
        {
            throw new NotImplementedException();
        }

        public void Write(byte portAddress, byte value)
        {
            throw new NotImplementedException();
        }

        byte _controlBase;
        byte _channelBase;
        byte[] _ports;
    }
}

/*
    Notes:
    
    ;
    ;        DMA
    ;
    DMACSR      equ 070Q    ; DMA control and status
    DMAREQ      equ 071Q    ; DMA request register
    DMAMASK     equ 072Q    ; DMA Mask register
    D.Floppy    equ 00H     ; Channel 0 select
    D.GPIB      equ 01H     ; Channel 1 select
    D.SIO       equ 02H     ; Channel 2 select
    D.PERQ      equ 03H     ; Channel 3 select
    D.Set       equ 04H     ; Set mask register bit
    D.Clear     equ 00H     ; Clear mask register bit
    DMAMODE     equ 073Q    ; DMA Mode register
    D.Read      equ 008H    ; Read transfer
    D.Write     equ 004H    ; Write transfer
    D.AutoInit  equ 010H    ; Autoinitialize
    D.Incr      equ 000H    ; Increment mode selected
    D.Decr      equ 020H    ; Decrement mode selected
    D.Demand    equ 000H    ; Demand mode transfer
    D.Single    equ 040H    ; Single transfer
    D.Block     equ 080H    ; Block mode transfer
    DMAPOINT    equ 074Q    ; DMA clear pointer register
    DMATEMP     equ 075Q    ; DMA Temporary register(Read Only)
    DMAMCLR     equ 075Q    ; DMA Master Clear(Write Only)
    DMACLRMASK  equ 076Q    ; DMA Clear mask register
    DMASETMASK  equ 077Q    ; DMA Set/Clear Mask bits

    DMAADR0     equ 060Q    ; DMA Channel 0 address
    DMAWC0      equ 061Q    ; DMA Channel 0 word count
    DMAADR1     equ 062Q    ; DMA Channel 1 address
    DMAWC1      equ 063Q    ; DMA Channel 1 word count
    DMAADR2     equ 064Q    ; DMA Channel 2 address
    DMAWC2      equ 065Q    ; DMA Channel 2 word count
    DMAADR3     equ 066Q    ; DMA Channel 3 address
    DMAWC3      equ 067Q    ; DMA Channel 3 word count


    PDMAStart   equ 163Q    ; Force PERQ DMA cycle to fill/empty FiFos
    PDMAFlush   equ 164Q    ; Flush DMA FiFo to PERQ
    PDMADirect  equ 172Q    ; 0 = DMA from PERQ, 1 = DMA to PERQ

 */
