{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}

Module IO_Unit;

{--------------------------------------------------------------------------
{
{ IO_Unit - Unit IO routines.
{
{ Copyright (C) 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       IO_Unit exports constants, types, variables, and procedures needed
{       to perform IO on the various IO Units, (devices).
{
{ Design:
{       1) All procedures call device dependent routines to do the actual
{          IO
{
{-------------------------------------------------------------------------}

{ $Version V7.14 for POS }
{--------------------------------------------------------------------------
{ Change Log:
{
{
{  15 Nov 83   V7.14 Dirk Kalp
{                        Add ForceEOI field to GPIBDevCmdHead type definition.
{
{  27 Oct 83   V7.13 Dirk Kalp
{                        Add commands IOFlushIn, IODevRead, and IODevWrite.
{                        Add type definitions for use with IODevRead/Write.
{
{  12 Apr 83   V7.12 Chris Hughes
{                        Modify for icl cio micropois disk support:
{                        Add a new case to DskResult in the exports to 
{                        suit icl interface.
{
{  18 Feb 83   V7.11 Sandeep Johar
{                       IOCRNext should have ch as a var parameter.
{
{  11 Feb 83   V7.10 Roger Riggs
{                        Added definitions for IOReadId and IOFlush.
{                        Added definitions of Floppy physical headers
{                        as used by IOReadID and IOFormat.
{
{  10 Feb 83   V7.9  Roger Riggs
{                        Fixed documentation
{
{  12 Jan 83   V7.8  August G. Reinig
{                        Changed call to GPB_UnitIO to include LogAdr in call.
{
{  10 Jan 83   V7.5  August G. Reinig
{                        Removed calls to RS_GetStatus in IOGetStatus since
{                        RS_GetStatus no longer exists.
{
{  05 Jan 83   V7.4  August G. Reinig
{                        Changed IOBeep to use high volume writes.
{
{  16 Dec 82   V7.3  August G. Reinig
{                        Added call to Z80_UnitIO in UnitIO.
{
{  14 Dec 82   V7.2  August G. Reinig
{                        Added more status definitions.
{
{  13 Dec 82   V7.1  August G. Reinig
{                        Added devices EIODisk and Z80, removed SMDdisk.
{
{  27 Oct 82   V7.0  August G. Reinig,  Chuck Beckett
{                        Changed to interface to the new Z80 code and 
{                        micro code.  Most procedures call unit specific
{                        procedures to do IO.  Added the routines IOCRNext,
{                        IOCPresent, IOClearExceptions, IOSetExceptions.
{
{  29 Jan 82  WJHansen V6.2  made time out 300000
{
{  10 Dec 81  WJIansen V6.1  removed Arith; use long for Time
{                            add timeout for all other devices
{                              (especially GPIB and RS232)
{
{   4-Nov-81  DAS  V6.0  Fixed to work with 10 megabuad ethernet.
{
{   1-Jul-81  BAM  V5.4  Change Screen name to ScreenOut.
{
{   3-Jun-81  JPS  V5.3  Add Virgil headers and comments.
{
{  23-May-81  JPS  V5.2  Use new IOKeyDisable/IOKeyEnable in place of old
{                        DisCtlC/EnaCtlC.
{
{  16-Apr-81  GGR  V5.1  Add 3MHz Ethernet Drivers (JEB).
{
{  11-May-81  JPS  V5.0  Split IO into several modules.
{
{   6-May-81  JPS  V4.7  1. Use new form of the SetCylinder StartIO.
{                        2. Don't bother doing 10 trys in FindSize since only
{                           the last result was believed regardless of success
{                           or failure.
{                        3. Hang if we cannot figure out the size of the disk.
{
{  11-Apr-81  JPS  V4.6  Changes for virtual memory.
{
{  19-Mar-81  BAM  V4.5  Included modules now named IO_Init and IO_Proc
{
{   3-Mar-81  JPS  V4.4  1) Fix LocateDskHeads and FindSize to agree with V4.3.
{                        2) Teach the HardDisk timeout code about multi-sector
{                           operations.
{
{  28-Feb-81  JPS  V4.3  No longer do conversions on Disk Physical block
{                        numbers (reinstating changes made in V4.0).
{
{  25-Feb-81  GGR  V4.2  Added setting/reading of DskFill1 in UnitIO.
{                        Moved new/dispose of CB from UnitIO to IO.Init.
{
{  16-Feb-81  BAM  V4.1  Put back in conversions on Disk Physical block
{                        numbers; fixed botCursF bug.  Del XXX procedures;
{                        Changed to use new screen
{
{   9-Feb-81  BAM  V4.0  No longer does conversions on Disk Physical block
{                        numbers; fixed CursorUpdate to allow partial screen
{                        display and added procedure IOScreenSize to set a
{                        new size.
{
{  13-Jan-81  JQS  V3.3  Move creation of the IOSeg to memory manager init.
{                        Move $R- to private part.
{
{  20-Nov-80  JPS  V3.2  Initialize TabFifoInx in InitTablet.
{
{  17-Nov-80  JPS  V3.1  Export the interrupt table.
{                        Check SystemInitialized for control-C abort.
{
{  16-Nov-80  BAM  V3.0  Radically changed Cursor and Tablet interface.  New
{                        time procedures.  Split into another include file.
{
{  10-Oct-80  JPS  V2.2  Added support for the diagnostic display (DDS).
{
{  27-Sep-80  DAS  V2.1  Added timeout code to UnitIO for the
{                        hard disk.
{
{
{  19-Sep-80  DAS  V2.0  Added code for 24 MByte disks.
{
{-------------------------------------------------------------------------}

{*******************************}   Exports   {*****************************}

imports SystemDefs  from SystemDefs;

const

  { Device Code Assignments for device table }

{ NOTE: The order of device declaration is important;  all Z80-controlled }
{       devices have been assigned a sub-range of contiguous values.  Be  }
{       sure to Check IO_Defs_Private before modifying these values!      }

        IOStart = 0;            { System Initialization }
        HardDisk = 1;  { Has a device table entry }

{$ifc Ether3MBaud then}
        Ether3 = 2;  { Has a device table entry }
{$elsec} {$ifc Ether10MBaud then}
        Ether10 = 2;  { Has a device table entry }
{$endc} {$endc}

        Floppy    =  3;  
        RSA       =  4;  RS232Out = RSA; RS232In = RSA;
        RSB       =  5;  { valid for EIO boards only }
        Speech    =  6;
        GPIB      =  7;  GPIBIn = GPIB; GPIBOUT = GPIB;  
        Keyboard  =  8;  
        Timer     =  9;  
        Clock     = 10;  
        PointDev  = 11;  Tablet = PointDev; 
        TransKey  = 12;         
        ScreenOut = 13;         
        EIODisk   = 14;
        Z80       = 15;

        LastUnit = Z80;       { for unit validity checking }
        MaxUnit = LastUnit;   { highest legal device code }

        
        RSExt = 0;        { RS-232 Speeds }
        RS110 = 1;
        RS150 = 2;
        RS300 = 3;
        RS600 = 4;
        RS1200 = 5;
        RS2400 = 6;
        RS4800 = 7;
        RS9600 = 8; 
        RS19200 = 9;

        RS_MaxWords = 6;  
        RS_MaxBytes = RS_MaxWords * 2;

type
        UnitRng = 0..MaxUnit;

        IOBufPtr = ^IOBuffer;
        IOBuffer = array[0..0] of integer;

        CBufPtr = ^CBufr;
        CBufr = packed array[0..0] of char;     { same as Memory, except for }
                                                { character buffers }
        BigStr = String[255];                   { A big String }

        IOStatPtr = ^IOStatus;
        IOStatus = record
                HardStatus: integer;            { hardware status return }
                SoftStatus: integer;            { device independent status }
                BytesTransferred: integer
                end;

        
        IOCommands = (IOReset,      IORead,     IOWrite,      IOSeek,
                      IOFormat,     IODiagRead, IOWriteFirst, IOIdle,
                      IOWriteEOI,   IOConfigure,IOWriteRegs,  IOSense,
                      IOWriteHiVol, IOReadHiVol,IOReadId,     IOFlush,
                      IOFlushIn,    IODevRead,  IODevWrite );

        IOHeadPtr = ^IOHeader;
        IOHeader = record               { Hard disk header record }
             SerialNum : double;        { Serial number of the file }
             LogBlock  : integer;       { The logical block number }
             Filler    : integer;
             NextAdr   : double;        { Address of next block in the file }
             PrevAdr   : double         { Address of previous block }
             end;  

                               
       IOIntrTypes = (IOATNInterrupt, IODataInterrupt);

    { the following is useful for an IOWriteReg command to the RS232 }
 
    RS_WrtReg = packed record    { Write to Chip registers }
        ID : 0..255;
        case integer of
          0 : { Write to command register }
              (NextRegisterPointer :  0..7;
               Command  : (R_NullCommand,      R_SendAbort,
                           R_ResetExtStatInt,  R_ChannelReset,
                           R_EnIntNextRxChr,   R_TxIntPendingReset,
                           R_ErrorReset,       R_ReturnFromInt );
               ResetCRC : (R_NullResetCRC, R_RxCRC, R_TxCRC, R_UnderRun));

               { Write to SIO registers 1 and 2 is not done from the PERQ }

          3 : { Write to receiver logic and parms }
             (RSRcvEnable        :  boolean;
              SynCharLoadInhibit :  boolean;
              AddressSearchMode  :  boolean;
              RxCrcEnable        :  boolean;
              EnterHuntPhase     :  boolean;
              AutoEnables        :  boolean;
              RSRcvBits: (RS_5, RS_7, RS_6, RS_8));

          4 : { Write to control for both Tx and Rx }
            (RSParity   : (RS_NoParity,    RS_OddParity
                          ,RS_IllegParity, RS_EvenParity);
             RSStopBits : (RS_Syncr,       RS_St1
                          ,RS_St1x5,       RS_St2);
             SyncMode   : (R_8BitSync,     R_16BitSync
                          ,R_SdlcMode,     R_ExternalSync);
             ClockRate  : (R_X1, R_X16, R_X32, R_X64));

          5 : { Write to Tx control }
             (TxCrcEnable : boolean;
              RTS         : boolean;
              UseCrc16    : boolean;
              TxEnable    : boolean;
              SendBreak   : boolean;
              RSXmitBits  : (RS_Send5, RS_Send7, RS_Send6, RS_Send8);
              DTR         : boolean);

          6 : { Write to Sync Char #1 }
             (Sync1 : 0..255);

          7 : { Write to Sync Char #2 }
             (Sync2 : 0..255)
        end;


{ Type declarations useful for IODevRead/Write commands for GPIB }

       GPIBDevCmdHead = packed record
            Options: packed record   { Bitmap to select cmd actions }
                 SetInt0Mask    : boolean;
                 SetInt1Mask    : boolean;
                 OmitBusConfig  : boolean;
                 OmitUnListen   : boolean;
                 case boolean of
                   true:  ( { For IODevWrite cmd }
                            OmitGoToStandby : boolean;
                            WaitOnData      : boolean;
                            ForceEOI        : boolean;
                            fill1           : 0..1
                          );
                   false: ( { For IODevRead cmd }
                            HoldOffOnEOI    : boolean;
                            fill2           : 0..7
                          );
                 end;
            Int0Mask  : 0..255;   { Mask for 9914 Interrupt Reg 0 }
            Int1Mask  : 0..255;   { Mask for 9914 Interrupt Reg 1 }
            PrimAddr  : 0..255;   { Primary Address of device }
            SecAddr   : 0..255;   { Secondary Address of device }
            end;

       pGPIBDevCmdBuf = ^GPIBDevCmdBuf;  
       GPIBDevCmdBuf = packed record   { Structure of buffer passed to UnitIO }
            Header      : GPIBDevCmdHead;
            DevData     : packed array[0..0] of 0..255;
            end;

 

{ Type declarations useful for IOSense commands to UnitIO }

       pGPIBStat = ^GPIBStat;  
       GPIBStat = packed record        { IOSense to GPIB provides }
            IntStat0    : 0..255;   { Interrupt Status 0 }
            IntStat1    : 0..255;   { Interrupt Status 1 }
            IntAddrStat : 0..255;   { Address Status (last interrupt)}
            IntBusStat  : 0..255;   { Bus Status (last interrupt)}
            IntAddrSwch : 0..255;   { Address Switch (last interrupt) } 
            IntCmdPass  : 0..255;   { Command Pass Through (last interrupt)}
            CurAddrStat : 0..255;   { Address Status (now) }
            CurBusStat  : 0..255;   { Bus Status (now) }
            CurAddrSwch : 0..255;   { Address switch (now) }
            CurCmdPass  : 0..255;   { CurCmdPass (now) }
            end;
 
       pClockStat = ^ClockStat;  
       ClockStat = packed record    { IOSense to the Clock provides this }
            Cycles  : 0..255;
            Year    : 0..255;
            Month   : 0..255;
            Day     : 0..255;
            Hour    : 0..255;
            Minute  : 0..255;
            Second  : 0..255;
            Jiffies : 0..255
            end; 
       
       pZ80Stat = ^Z80Stat;
       Z80Stat = packed record        { IOSense to the Z80 provides the }
            MinorVersionNum : 0..255; { version number of the code running }
            MajorVersionNum : 0..255; { on the Z80, interpret as }
            end;                      { Version Major.Minor }

       pPointDevStat = ^PointDevStat; 
       PointDevStat = packed record   { zero means PointDev configured off }
            OnOff : 0..255;           { nonzero means PointDev configured on }
            end;  

       pKeyStat = ^IO_KeyStat;
       IO_KeyStat  = packed record
            OnOff    : 0..255;        { Zero means keyboard is configured off }
            OverFlow : 0..255;        { NonZero means overrun on in buffers }
            end;
 
   
  RS_StatusType = (RS_Config, RS_PStat, RS_GStat,
                     RS_MapBytes, RS_MapWords);


  pRS232Stat = ^RS232Stat;
  RS232Stat = packed record
    case RS_StatusType of
      RS_MapBytes: ( Byte : packed array [ 1..RS_MaxBytes] of 0..255 );
      RS_MapWords: ( Word : packed array [ 1..RS_MaxWords] of integer );
      RS_Config  : ( XmitRate : 0..255;
                     RcvRate : 0..255 
                   );
      RS_PStat   : ( Reg : packed array [1..6] of RS_WrtReg );
      RS_GStat   : { Read General Status - SIO Chip's Read Register #0 }
                 ( RxCharAvailable  :  boolean;
                   IntPending       :  boolean;
                   TxBufferEmpty    :  boolean;
                   DCD              :  boolean;
                   SyncHunt         :  boolean;
                   CTS              :  boolean;
                   TransmitUnderRun :  boolean;
                   BreakAbort       :  boolean;
                   { Read Special Condition - SIO Chip's Read Register #1 }
                   AllSent         :  boolean;
                   Residue         :  0..7;
                   ParityError     :  boolean;
                   RxOverRun       :  boolean;
                   CrcFramingError :  boolean;
                   EndOfFrame      :  boolean );
      end;
  


                                                      
 { Use of this status block is discouraged,  Use the IOSense, IOConfigure,
   and IOWriteRegs command with UnitIO instead of IOPutStatus and IOGetStatus }

  DevStatusBlock = packed record
    ByteCnt: integer;       { # of status bytes }
    case UnitRng of
      KeyBoard,
{$ifc Ether3MBaud then}
      Ether3,
{$endc}
      Clock: (DevEnable: boolean);

      Tablet: (OldStanleyEnable : boolean;  { should always be false }
               KrizEnable : boolean;
               case boolean of
                 true  : { GET STATUS }
                         ( TAbFill : 0..#77;
                           TabOverRun : 0..255 );
                 false : { PUT STATUS }
                         ( { nothing } ));

      RS232In,
      RS232Out: (RSRcvEnable : boolean;
                 RSFill      : 0..127;
                 RSSpeed     : 0..255;
                 RSParity    : (NoParity, OddParity, IllegParity, EvenParity);
                 RSStopBits  : (Syncr,Stop1,Stop1x5,Stop2);
                 RSXmitBits  : (Send5,Send7,Send6,Send8);
                 RSRcvBits   : (Rcv5,Rcv7,Rcv6,Rcv8));

      Floppy: (case integer of { Get or Put }
                 1: { GET STATUS }
                    (FlpUnit      : 0..3;
                     FlpHead      : 0..1;
                     FlpNotReady  : boolean;
                     FlpEquipChk  : boolean;
                     FlpSeekEnd   : boolean;
                     FlpIntrCode  : 0..3;
                     case integer of
                       1 {IORead, IOWrite, IOFormat}:
                         (FlpMissAddr    : boolean; { in data or header }
                          FlpNotWritable : boolean;
                          FlpNoData      : boolean;
                          FlpFill1       : 0..1;
                          FlpOverrun     : boolean;
                          FlpDataError   : boolean; { in data or header }
                          FlpFill2       : 0..1;
                          FlpEndCylinder : boolean;
                          FlpDataMissAddr: boolean; { in data }
                          FlpBadCylinder : boolean;
                          FlpFill3       : 0..3;
                          FlpWrongCylinder : boolean;
                          FlpDataDataError : boolean; { in data }
                          FlpFill4         : 0..3;
                          FlpCylinderByte  : 0..255;
                          FlpHeadByte      : 0..255;
                          FlpSectorByte    : 0..255;
                          FlpSizeSectorByte: 0..255 );
                       2 {IOSeek}:
                         (FlpPresentCylinder: 0..255));
                 2: { PUT STATUS }
                    (FlpDensity : 0..255;   { single = 0, double = #100 }
                     FlpHeads   : 0..255;   { 1 or 2 heads }
                     FlpEnable  : boolean );
                 3: { BYTE ACCESS }
                    (FlpByte1 : 0..255;
                     FlpByte2 : 0..255;
                     FlpByte3 : 0..255;
                     FlpByte4 : 0..255;
                     FlpByte5 : 0..255;
                     FlpByte6 : 0..255;
                     FlpByte7 : 0..255 )  );

      GPIBIn,
      GPIBOut: { GET STATUS ONLY }
               (Int1Status  : 0..255;
                AddrSwitch  : 0..255;
                AddrStatus  : 0..255;
                CmdPassThru : 0..255;
                Int0Status  : 0..255;
                BusStatus   : 0..255 )

      end;

{ hard status type information }

  DskResult = packed record
    case integer of
      0  : ( Result : integer );
      1 : ( CntlError : ( DskOK,
                              AddrsErr,            { address error }
                              PHCRC,               { Physical Header CRC }
                              LHSer,               { Logical Serial Wrong }
                              LHLB,                { Logical Block Wrong }
                              LHCRC,               { Logical Header CRC }
                              DaCRC,               { Data CRC }
                              Busy);
                Fill2     : boolean;
                TrackZero : boolean;
                WriteFault : boolean;
                SeekComplete : boolean;
                DriveReady : boolean);    
      2 :  {for cio micropolis disks}
           ( CioMCntlError: ( CioMDskOK,
                              CioMAddrsErr,            { address error }
                              CioMPHCRC,               { Physical Header CRC }
                              CioMLHSer,               { Logical Serial Wrong }
                              CioMLHLB,                { Logical Block Wrong }
                              CioMLHCRC,               { Logical Header CRC }
                              CioMDaCRC,               { Data CRC }
                              CioMBusy);
                CioMIndex     : boolean;
                CioMIllegalAddr : boolean;
                CioMFault : boolean;
                CioMSeekComplete : boolean;
                CioMDriveReady : boolean)    
      end;                             


{ Floppy special data descriptions }
{  This information is returned from IOReadID for the floppy }

   FlpPhyHdr = Packed Record                { used for IOFormat and IOReadID }
                Cylinder : 0..255;          { Cylinder number }
                Head : 0..255;              { Head number }
                Sector : 0..255;            { Sector number }
                Size : 0..255;              { Size (0=128 bytes, 1=256 bytes }
                end;
   pFlpPhyHdr = ^FlpPhyHdr;                 { RECAST to IOBufPtr for IOReadID }
{  This information is passed to IOFormat and specifies how to format }

   FlpFmtHdrs = Array [1..26] of FlpPhyHdr; { input to IOFormat command }
   pFlpFmtHdrs = ^FlpFmtHdrs;               { RECAST to IOBufPtr for IOFormat }

Var
        CtrlSPending : boolean;  { True: Control S has halted screen output }
        IOInProgress: boolean;   { false when speech is active }    
        IO24MByte : boolean;          { true if the disk is 24 MBytes }


Exception DevInterrupt(Unit: UnitRng; IntType: IOIntrTypes; ATNCause: Integer);



Function IOCRead(Unit: UnitRng; var Ch: char): integer;
                                            { read a character from a }
                                            { character device and return }
                                            { status: IOB no char available }
                                            {         IOC character returned }

Function IOCWrite(Unit: UnitRng; Ch: char): integer;
                                            { write Ch to character device }
                                            { and return status:           }
                                            {       IOB buffer full        }
                                            {       IOC character sent     }


Procedure UnitIO(Unit: UnitRng;                 { IO to block structured }
                 Bufr: IOBufPtr;                { devices }
                 Command: IOCommands;
                 ByteCnt: integer;
                 LogAdr: double;
                 HdPtr: IOHeadPtr;
                 StsPtr: IOStatPtr);

Procedure IOWait(var Stats: IOStatus);          { hang until I/O completes }

Function IOBusy(var Stats: IOStatus): boolean;  { true if I/O not complete }

Procedure IOPutStatus(Unit: UnitRng; var StatBlk: DevStatusBlock);
                                            { Set status on device Unit }
Procedure IOGetStatus(Unit: UnitRng; var StatBlk: DevStatusBlock);
                                            { Reads status on device Unit }
Procedure IOBeep;                           { You guessed it, BEEP! }

Function IOCRNext( Unit: UnitRng; Var Ch: char ): integer;
                                            { read next available character }
                                            { from a character device, we are }
                                            { like IOCRead except that we }
                                            { return only if we have }
                                            { character }

Function IOCPresent( Unit: UnitRng ): boolean;
                                            { return true if the device is a }
                                            { characte device with an }
                                            { available character }

Procedure IOClearExceptions;                { Turn off all exception raising }
                                            { for device interrupts }

Procedure IOSetExceptions( Unit : UnitRng;
                           IntType : IOIntrTypes;
                           var Setting:boolean);
                                            { disable or enable the raising }
                                            { of the exception DevInterrupt }
                                            { when the Unit sends an IntType }
                                            { interrupt.  Setting will be }
                                            { assigned the old setting }

{$ifc Ether3MBaud then}
Function Ether3Transmit(Buff: IOBufPtr; WdCnt: integer) : integer;

Function Ether3Receive(Buff: IOBufPtr; var WdCnt: integer; timeout: integer)
                      : integer;

Function Ether3Start(Promiscuous, Restart: boolean) : integer;
{$endc}   


{*******************************}   Private   {*****************************}

{$R-}

Const 
  debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} imports System from System;
{$endc}

Imports IOErrors from IOErrors;
Imports IO_Private from IO_Private;
Imports IOKeyboard from IOKeyboard;
Imports IODisk from IODisk;
Imports IORS from IORS;
Imports IOGpib from IOGpib;
Imports IOPointDev from IOPointDev; 
Imports IOFloppy from IOFloppy; 
Imports IOClock from IOClock;   
Imports IOZ80 from IOZ80;
Imports Screen from Screen;



Function IOCRead(Unit: UnitRng; var Ch: char): integer;
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Reads a character from a character device and returns a completion
{      or error code.
{
{ Parameters:
{
{      Unit - device from which to read the character.
{
{      Ch - character to read.
{
{ Returns:
{
{      A condition code as defined in the module IOErrors.
{
{---------------------------------------------------------------------------}

begin

  case Unit of
    KeyBoard  ,
    TransKey  : IOCRead := Key_ReadChar ( Unit, Ch );
    GPIB      : IOCRead := Gpb_ReadChar ( Ch );
    RSA, RSB  : IOCRead := Rs_ReadChar ( Unit, Ch );
    Otherwise : if Unit in [0..MaxUnit]
                then IOCRead := IOENCD
                else IOCRead := IOEBUN
    end { case };

end { IOCRead };

Function IOCWrite( Unit: UnitRng; Ch: char):integer;
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Writes a character to a character device and returns a completion
{      or error code.  Delays if the buffer is full.  Returns an error if
{      the condition doesn't clear up.
{
{ Parameters:
{
{      Unit - device onto which the character will be written.
{
{      Ch - character to write.
{
{ Returns:
{
{      Condition code as defined by the module IOErrors.
{
{---------------------------------------------------------------------------}

begin

  case Unit of
    ScreenOut: begin
                 SPutChr (Ch);
                 IOCWrite := IOEIOC;
               end;

    GPIB     : IOCWrite := Gpb_WriteChar ( Ch );

    RSA,
    RSB,
    Speech   : IOCWrite := Rs_WriteChar ( Unit,  Ch );

    Otherwise: if Unit in [0..MaxUnit]
               then IOCWrite := IOENCD
               else IOCWrite := IOEBUN
    end { case }

end { IOCWrite };

Procedure IOWait( var Stats: IOStatus );
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Hangs until an IO operation inititated by UnitIO is complete.
{
{ Parameters:
{
{      Stats - Status block that was given to UnitIO when the operation
{              was initiated.
{
{---------------------------------------------------------------------------}

begin
while Stats.SoftStatus = 0 do ;
end { IOWait };

Function IOBusy( var Stats: IOStatus): boolean;
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Determines whether or not I/O is complete.
{
{ Parameters:
{
{      Stats - Status block that was  given  to  UnitIO when the operation
{              was initiated.
{
{ Returns:
{
{      True if IO is not complete, false if it is.
{
{---------------------------------------------------------------------------}
 
begin
IOBusy := Stats.SoftStatus = 0
end { IOBusy };

Procedure IOPutStatus( Unit: UnitRng; var StatBlk:DevStatusBlock );
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Sets device's characteristics.  Has no effect if the device has no
{      settable status.
{
{ Parameters:
{
{      Unit - device whose characteristics  are  to  be set.
{
{      StatBlk - block containing characteristics to be set.
{
{---------------------------------------------------------------------------}

var
  TempPtr : pointer;

begin

  case Unit of          
    Floppy    : Flp_PutStatus( StatBlk );
    PointDev  : Ptr_PutStatus( StatBlk );
    RSA,
    RSB,
    Speech    : Rs_PutStatus( Unit, StatBlk );
    otherwise : { do nothing }
    end

end { IOPutStatus };

Procedure IOGetStatus( Unit: UnitRng; var StatBlk:DevStatusBlock );
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Reads device status.  Has no effect if the device has no readable
{      status.
{
{ Parameters:
{
{      Unit  -  device whose characteristics are to be read.
{
{      StatBlk - block to which device status is to  be returned.
{
{---------------------------------------------------------------------------}

  var BlkPtr: IOPtrKludge; i:integer;
begin

  case Unit of                        
    Floppy    : Flp_GetStatus( StatBlk );
    GPIB      : GPB_GetStatus( StatBlk );
    PointDev  : Ptr_GetStatus( StatBlk );
    otherwise : { do nothing }
    end

end { IOGetStatus };

Procedure UnitIO( Unit: UnitRng;
                  Bufr: IOBufPtr;
                  Command: IOCommands;
                  ByteCnt: integer;
                  LogAdr: double;
                  HdPtr: IOHeadPtr;
                  StsPtr: IOStatPtr );
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      IO to non-character devices.
{
{ Parameters:
{
{      Unit - the device.
{
{      Bufr - buffer for data transfers, if requested.
{
{      Command - operation to be performed on the device.
{
{      ByteCnt - number of bytes to be transferred.
{
{      LogAdr - logical address for block structured devices.
{
{      HdPtr - pointer to the logical header for operations with the hard disk.
{
{      StsPtr - resultant status from the operation.
{
{---------------------------------------------------------------------------}

var
  OldKeyEnable : boolean;
  StartTime, EndTime : record case boolean of
                         true  : (D : double);
                         false : (L : long )
                         end;

begin
  
  Key_Disable( OldKeyEnable );
  {$ifc SysTiming then}
  IOGetTime( StartTime.D );
  {$endc}
  
  StsPtr^.SoftStatus := IOEIOB;
  StsPtr^.HardStatus := 0;
  StsPtr^.BytesTransferred := 0;

  case unit of
   HardDisk,
   EIODisk  : Dsk_UnitIO( Unit, Bufr, Command, ByteCnt, LogAdr, HdPtr, StsPtr);
   RSA,
   RSB,
   Speech   : Rs_UnitIO( Unit, Bufr, Command, ByteCnt, StsPtr );
   Floppy   : Flp_UnitIO( Bufr, Command, ByteCnt, LogAdr, StsPtr );
   Z80      : Z80_UnitIO( Bufr, Command, ByteCnt, LogAdr, StsPtr );
   GPIB     : GPB_UnitIO( Bufr, Command, ByteCnt, LogAdr, StsPtr );  
   Clock    : Clk_UnitIO( Bufr, Command, ByteCnt, StsPtr );
   PointDev : Ptr_UnitIO( Bufr, Command, ByteCnt, StsPtr ); 
   otherwise: if Unit in [0..MaxUnit]
              then StsPtr^.SoftStatus := IOEILC
              else StsPtr^.SoftStatus := IOEBUN
   end;
   
  {$ifc SysTiming then}
  IOGetTime( EndTime.D );
  IOTime := EndTime.L - StartTime.L + IOTime;
  {$endc}
  Key_Enable( OldKeyEnable )

end { UnitIO };

procedure IOBeep;
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Causes the PERQ to beep.
{
{---------------------------------------------------------------------------}

  var Seg,Ofst: integer;
      BufPtr: IOBufPtr;
      Status: IOStatus;
      StatPtr: IOStatPtr;
      zilch: double;
      Alignment : integer;
      Buf: array[0..67] of integer;   { 63 + 4 so we can insure quad aligned }

begin 
  {$ifc not debug then}
  InLineByte({LSSN} 99);
  StorExpr(Seg);    

  LoadAdr(Buf);
  StorExpr(Ofst);  
  Alignment := 4 - land( Ofst, 3 ); { force to quad word alignment }
  Ofst := Ofst + Alignment;  

  BufPtr:=MakePtr(Seg,Ofst,IOBufPtr);                                  

  LoadAdr(Status);
  StorExpr(Ofst);
  StatPtr:=MakePtr(Seg,Ofst,IOStatPtr);

  for seg := 0 + Alignment to 63 + Alignment do
    if odd(seg) 
    then Buf[seg] := 0
    else Buf[seg] := -1;   

  UnitIO( Speech, BufPtr, IOWriteHiVol, 128, zilch, nil, StatPtr ) 
  {$endc}
end { beep };

Function IOCRNext( Unit: UnitRng; Var Ch: char ): integer;

{----------------------------------------------------------------------------
{
{ Abstract:
{
{      Reads a character from a character device and returns a completion
{      or error code.  We will always return with an error or a character.
{
{ Parameters:
{
{      Unit - device from which the character will be read.
{
{      Ch   - character read from device.
{
{ Returns:
{
{      Condition code as defined by the module IOErrors.
{
{--------------------------------------------------------------------------}

var
  result : integer;

begin

  repeat
    result := IOCRead( Unit, Ch )
  until result <> IOEIOB;

  IOCRNext := result

end;

Function IOCPresent( Unit: UnitRng ): boolean;

{----------------------------------------------------------------------------
{
{  Abstract:
{
{       Returns true if the Unit is a character device and has a character
{       available for reading.  Otherwise it returns false.  It does not
{       read the character.
{
{ Parameters:
{
{      Unit - device to check for characters.
{
{ Returns:
{
{      True if a character is available.
{
{--------------------------------------------------------------------------}

begin

  case Unit of
  
    Keyboard ,
    Transkey : IOCPresent := (KTBuf^.RdPtr <> KTBuf^.WrPtr);
    
    GPIB     : with pUDevTab^[GPIB].pCirBuf^ 
               do IOCPresent := (RdPtr <> WrPtr); 
               
    RSA      : with pUDevTab^[RSA].pCirBuf^ 
               do IOCPresent := (RdPtr <> WrPtr); 
    
    RSB      : with pUDevTab^[RSB] do
                 if pCirBuf <> nil
                 then IOCPresent := pCirBuf^.RdPtr <> pCirBuf^.WrPtr
                 else IOCPresent := false;
                            
    Otherwise: IOCPresent := false
    end

end;

Procedure IOClearExceptions;

{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Disable the raising of exceptions for device interrupts.  All
{      devices are affected.  Exception raising disabled is the normal
{      case.  This procedure also resets the enable mask of certain devices.
{
{---------------------------------------------------------------------------}

var
  i : integer;

begin

  for i := 0 to MaxUnit do begin
    RaiseException[i].Attention := false;
    RaiseException[i].DataAvailable := false
    end;
    
  loadexpr( GPIBIntMask );      { in case some GPIB interrupts are masked off }
  loadexpr( GPIB );             { turn on all GPIB interrupts }
  startio( EP_SetEnableMask );
  
  for i := RSA to Speech do begin
    loadexpr( RSIntMask );
    loadexpr( i );
    startio( EP_SetEnableMask )
    end;  
    
end;

procedure IOSetExceptions( Unit : UnitRng;
                           IntType : IOIntrTypes;
                           var Setting:boolean);

{----------------------------------------------------------------------------
{
{ Abstract:
{
{      Disables or enables the raising of an exception when the specified
{      device raises the specified interrupt.  Setting = true enables,
{      Setting = false disables.  Upon return, Setting will be set to
{      to true if the raising of the specified exception was previously
{      enabled, false if not.
{
{ Parameters:
{
{      Unit - device to enable/disable exceptions on.
{
{      IntType - Type of interrutps to enable/disable
{
{      Setting - True to enable, False to Disable.
{
{ Returns:
{
{      Setting - Returns previous state of exceptions.
{
{----------------------------------------------------------------------------}

var
  NewSetting : boolean;

begin

  if (Unit < 0)                    { check unit }
  or (Unit > MaxUnit)
  then exit( IOSetExceptions );    { return immediately if a bad parameters }


  NewSetting := Setting;

  case IntType of
    IOATNInterrupt: begin
                      Setting := RaiseException[Unit].Attention;
                      RaiseException[Unit].Attention := NewSetting
                    end;
    IODataInterrupt:begin
                      Setting := RaiseException[Unit].DataAvailable;
                      RaiseException[Unit].DataAvailable := NewSetting
                    end;
    otherwise : ;
    end

end.
