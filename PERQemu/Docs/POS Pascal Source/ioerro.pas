{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOErrors; 
{---------------------------------------------------------------- 
{ 
{ Abstract: 
{ 
{ I/O System Error Code Definitions 
 
 Copyright (C) 1981,1982, 1983 - The Three Rivers Computer Corporation 
----------------------------------------------------------------} 
{---------------------------------------------------------------- 
 Versions: 
    V1.8  23 Feb 83   R. Riggs       Add IOEEND, IOEFRA, IOEPAR
    V1.7  10 Feb 83   C. Beckett     Add dummy procedure so module compiles ok.
    V1.6   1 Feb 83   TV             Added EIO Disk Error, IOEPHM. Tony Vezza.
    V1.5   1 Feb 83   Sandeep Johar  IOEDNS - device not supported.
    V1.4   8 Dec 82   TV             Added EIO Disk Errors. Tony Vezza. 
    V1.3   7 Oct 82   C. Beckett     Added new IO sub-system errors 
    V1.2  13 May 81   Brad A. Myers  Added new error for Ether3MBaud errors 
    V1.1  12 May 81   Brad A. Myers  Added new errors and First and last 
    V1.0  ?? ??? ??   ??             Started 
----------------------------------------------------------------} 
 
Exports 
 
Imports SystemDefs from SystemDefs;  {using Ether3MBaud} 
 
Const 
        IOEIOC = 1;       { IO Complete } 
        IOEIOB = 0;       { IO Busy } 
        IOEBUN = -1;      { Bad Unit Number } 
        IOENBD = -2;      { Raw Block IO to this device is not implemented } 
        IOEWRF = -3;      { Write Failure } 
        IOEBSE = -4;      { BlockSize Error } 
        IOEILC = -5;      { Illegal Command for this device } 
        IOENHP = -6;      { Nil Header Pointer } 
        IOEADR = -7;      { Address Error } 
        IOEPHC = -8;      { Physical Header CRC Error } 
        IOELHC = -9;      { Logical Header CRC Error } 
        IOEDAC = -10;     { Data CRC Error } 
        IOEDNI = -11;     { Device Not Idle } 
        IOEUDE = -12;     { Undefined Error! } 
        IOENCD = -13;     { Device is not a character device } 
        IOECBF = -14;     { Circular Buffer Full } 
        IOELHS = -15;     { Logical Header SerialNum Mismatch } 
        IOELHB = -16;     { Logical Header Logical Block Number Mismatch } 
        IOECOR = -17;     { Cylinder Out of Range } 
        IOEDNR = -18;     { Device not ready } 
        IOEMDA = -19;     { Missing data address mark } 
        IOEMHA = -20;     { Missing header address mark } 
        IOEDNW = -21;     { Device not writable } 
        IOECMM = -22;     { Cylinder mis-match } 
        IOESNF = -23;     { Sector not found } 
        IOEOVR = -24;     { Overrun } 
        IOEUEF = -25;     { Undetermined equipment fault } 
        IOESOR = -26;     { Sector out of range } 
        IOETIM = -27;     { Time out error } 
        IOEFRS = -28;     { Floppy recalibrate done } 
        IOEDRS = -29;     { Disk recalibrate done } 
        IOET0  = -30;     { Can't find track zero } 
        IOECDI = -31;     { Data supplied to configuration command is bad } 
        IOERDI = -32;     { Register data for WriteRegs command is bad } 
        IOEBAE = -33;     { Buffer alignment error }
        IOENOC = -34;     { Not on Cylinder }
        IOEABN = -35;     { Abnormal Error }
        IOELHE = -36;     { Logical Header Mismatch }
        IOESME = -37;     { State Machine Error }
        IOESKE = -38;     { Drive Seek Error }
        IOEFLT = -39;     { Drive Fault }
        IOEDNS = -40;     { Device not supported }
        IOEPHM = -41;     { Physical Header Mismatch } 
{$ifc Ether3MBaud then} 
        IOEPTL = -42;     { Ether3 - received packet too large } 
{$endc}
        IOEEND = -43;     { End of data } 
        IOEFRA = -44;     { Framing error }
        IOEPAR = -45;     { Parity error }
         
    IOEFirstError = -45; 
 
    IOELastError = 0; 
             
Private 
procedure finally;
begin end.
