{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IO_Init;
{-----------------------------------------------------------------------------
{
{ IO_Init - Initialize the IO system.
{
{ Copyright (C) 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       IO_Init initializes the Interrupt Vector Table, the Device Table and
{       associated buffers, the Screen Package, the tablet and cursor, and
{       the Z80.  IO_Init imports various device dependent modules, which
{       contain device dependent initialization code.
{
{       This module is based on Version V5.9 of old I/O board's IO_Init
{       support module, written by Miles Barel and modified by numerous
{       times by just about every engineer at 3RCC.
{
{-----------------------------------------------------------------------------}
{ $Version V6.7 for POS}

{-----------------------------------------------------------------------------}
{
{ Change Log:
{
{   22 Feb 83  V6.7  Sandeep Johar
{                    Call Cf_Init before Screen Init.
{   
{   16 Feb 83  V6.6  Sandeep Johar
{                    Call Configuration.
{
{   04 Jan 83  V6.5  August G. Reinig
{                    Changed calls to SetDDS.
{
{   23 Dec 82  V6.4  August G. Reinig
{                    Added Tony Vezza's code to allocate disk buffers before
{                    giving the device table to the microcode.
{
{   15 Dec 82  V6.3  August G. Reinig
{                    Moved screen initialization, removed keyboard circular
{                    buffer initialization to IOKeyboard, shuffled other
{                    initialization code about.
{                    
{   13 Dec 82  V6.2  August G. Reinig
{                    add Tony's new disk initialization code
{
{   24 Nov 82  V6.1  Sandeep Johar
{                    add ethernet.
{
{   27 Oct 82  V6.0  August G. Reinig, Chuck Beckett
{                    Changed module to interface to new Z80 code and micro
{                    code.  Most of the initialization is now down by device
{                    specific routines.  
{   
{   16-Nov-81  DAS  V5.9  (JPS)  Put in John's code for Megabyte memory. 
{ 
{    4-Nov-81  DAS  V5.8  Added code for the 10 MBaud Ethernet. 
{  
{    1-Jul-81  BAM  V5.7  Changed name of Screen to ScreenOut 
{  
{   25-Jun-81  JPS  V5.6  Change EnableTablet to InitGPIB and do not enable 
{                         the tablet. 
{    
{    3-Jun-81  JPS  V5.5  Add Virgil headers and commands. 
{    
{   23-May-81  JPS  V5.4  Initialize new KeyEnable flag. 
{    
{   16-Apr-81  GGR  V5.3  Add 3MHz Ethernet Drivers (JEB). 
{    
{    1-Apr-81  GGR  V5.2  Moved time base to video refresh (clock is no 
{                         longer turned on in io_init). 
{    
{   31-Mar-81  GGR  V5.1  Added 4-button mouse support. 
{  
{   11-May-81  JPS  V5.0  Split IO into several modules. 
{    
{    6-May-81  JPS  V4.7  1. Use new form of the SetCylinder StartIO. 
{                         2. Don't bother doing 10 trys in FindSize since only 
{                            the last result was believed regardless of 
{                            success or failure. 
{                         3. Hang if we cannot figure out the size of the disk.
{    
{   11-Apr-81  JPS  V4.6  Changes for virtual memory. 
{    
{   19-Mar-81  BAM  V4.5  Changed name of included modules to IO_Init 
{                         and IO_Proc 
{    
{    3-Mar-81  JPS  V4.4  1) Fix LocateDskHeads and FindSize to agree with V4.3
{                         2) Teach the HardDisk timeout code about 
{                            multi-sector operations.
{  
{   28-Feb-81  JPS  V4.3  No longer do conversions on Disk Physical block 
{                         numbers (reinstating changes made in V4.0). 
{  
{   25-Feb-81  GGR  V4.2  Added setting/reading of DskFill1 in UnitIO. 
{                         Moved new/dispose of CB from UnitIO to IO.Init. 
{  
{   16-Feb-81  BAM  V4.1  Put back in conversions on Disk Physical block 
{                         numbers; fixed botCursF bug.  Del XXX procedures; 
{                         Changed to use new screen 
{    
{    9-Feb-81  BAM  V4.0  No longer does conversions on Disk Physical block 
{                         numbers; fixed CursorUpdate to allow partial screen 
{                         display and added procedure IOScreenSize to set a  
{                         new size. 
{    
{   13-Jan-81  JPS  V3.3  Move creation of the IOSeg to memory manager init. 
{                         Move $R- to private part. 
{  
{   20-Nov-80  JPS  V3.2  Initialize TabFifoInx in InitTablet. 
{    
{   17-Nov-80  JPS  V3.1  Export the interrupt table. 
{                         Check SystemInitialized for control-C abort. 
{    
{   16-Nov-80  BAM  V3.0  Radically changed Cursor and Tablet interface.  New 
{                         time procedures.  Split into another include file. 
{    
{   10-Oct-80  JPS  V2.2  Added support for the diagnostic display (DDS). 
{   
{   27-Sep-80  DAS  V2.1  Added timeout code to UnitIO for the 
{                         hard disk. 
{   
{  
{   19-Sep-80  DAS  V2.0  Added code for 24 MByte disks. 
{ 
{
{-----------------------------------------------------------------------------}

{*******************************}   Exports   {*****************************}

Procedure InitIO;
Procedure ReInitDevices;

{*******************************}   Private   {*****************************}

Const debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} imports System from System;
{$endc}

Imports IOErrors from IOErrors;
Imports IO_Private from IO_Private;
Imports Memory from Memory;
Imports Screen from Screen;
Imports Configuration from Configuration;

imports IOVideo from IOVideo;
imports IOKeyBoard from IOKeyBoard;
imports IODisk from IODisk;
imports IOGPIB from IOGPIB;
imports IORS from IORS;
imports IOPointDev from IOPointDev; 
imports IOClock from IOClock; 
imports IOFloppy from IOFloppy;
imports IOZ80 from IOZ80;
imports IO_Others from IO_Others; 
imports DiskDef from DiskDef;
{$ifc Ether10MBaud then}
Imports EtherInterrupt from EtherInterrupt;
{$endc}

Procedure InitIO;

{----------------------------------------------------------------------------}
{
{ Abstract:
{       InitIO initializes the Interrupt Vector Table, the Device Table and
{       associated buffers, the Screen Package, the tablet and cursor, and
{       the Z80.
{
{-----------------------------------------------------------------------------}

  var StatBuf: DevStatusBlock;
      i: integer;
{$ifc Ether3MBaud then}
      pEtherB: pEtherBuff;        { Ether3 }
{$endc}

function InitDeviceTable: UDeviceTable;

{-----------------------------------------------------------------------------
{
{ Abstract:
{       InitDeviceTable initializes the Device Table and associated buffers
{       for the microcode.  We are responsible for setting up anything in
{       the device table that the microcode needs at startio time.  We do
{       not set anything in the device table that the microcode does not need
{       at startio time.
{
{-----------------------------------------------------------------------------}


{ Interrupt priorities of devices in the device table }  
{ if you change these, change InitInterruptVectors }

const
  Disk_IntPr   =  0; { the highest priority device }
  Ether_IntPr  =  1;
  GPIB_IntPr   =  2;
  RSA_IntPr    =  3;
  RSB_IntPr    =  4;
  Speech_IntPr =  5;
  Floppy_IntPr =  6;
  KeyBd_IntPr  =  7;
  Screen_IntPr =  8;
  Point_IntPr  =  9;
  Clock_IntPr  = 10; 
  Z80_IntPr    = 11; { the lowest priority device }
  HighPriority =  0; { highest possible priority }
  LowPriority  = 15; { lowest possible priority }


function InitInterruptVectors: pointer;

{-----------------------------------------------------------------------------
{
{ Abstract:
{       InitInterruptVectors initializes the Interrupt Vector Table.
{
{-----------------------------------------------------------------------------}

type
  IntVecTable = array [Disk_IntPr..Z80_IntPr] of
    record                 { NO Fake Units Included Here! }
      SSN   : integer;
      GPtr  : integer;       
      Rtn   : 0..255;
      SLink : integer
      end;
var 
  i : integer;
  pIntTab : ^IntVecTable;

  
begin
  
  new( IOSegNum, 4, pIntTab );
  InitInterruptVectors := recast( pIntTab, pointer );

  {$ifc not debug then}
  for i := Disk_IntPr to Z80_IntPr do begin
  
    LoadAdr( pIntTab^[i] );    { get the address of the entry }
    
    case i of                  { get a VRD to the interrupt routine }
      Disk_IntPr   : MakeVRD( Dsk_Interrupt );
      Ether_IntPr  : {$ifc not Ether3MBaud
                     then}  MakeVRD( E10Srv );
                     {$elsec} MakeVRD( Ether3Intr );
                     {$endc}
      GPIB_IntPr   : MakeVRD( GPB_Interrupt );  
      RSA_IntPr    : MakeVRD( RSA_Interrupt );
      RSB_IntPr    : MakeVRD( RSB_Interrupt );
      Speech_IntPr : MakeVRD( Spc_Interrupt );
      Floppy_IntPr : MakeVRD( Flp_Interrupt );
      KeyBd_IntPr  : MakeVRD( Key_Interrupt );
      Point_IntPr  : MakeVRD( Ptr_Interrupt );
      Clock_IntPr  : MakeVRD( Clk_Interrupt );
      Screen_IntPr : MakeVRD( Vid_Interrupt ); 
      Z80_IntPr    : MakeVRD( Z80_Interrupt );
      otherwise    : while true do { hang }
      end;
      
    InLineByte({EXCH} 230 );   { reverse order of ETOS, ETOS-3 }
    InLineByte({MMS2} 201 );
    InLineByte({EXCH} 230 );
    InLineByte({MES2} 202 );
    InLineByte({EXCH2} 231 );

    InLineByte({MMS2} 201 );   { move ETOS, ETOS-3 to MTOS, MTOS-3 }
    InLineByte({MMS2} 201 );   { now it looks like we did a MakeVRD to Mstack }
    
    InLineByte({LDC4} 4 );     { push 4 onto ETOS }
    InLineByte({TLATE2} 228 ); { translate addr of pIntTab^[i] to phys addr }
    InLineByte({STMW} 182 )    { and move MTOS, MTOS-3 to pIntTab^[i] }
  end
  {$endc}

end { InitInterruptVectors };

var
  i : integer;
  ZeroEntry : DevTblEntry;

begin   {InitDeviceTable}
  
  SetDDS(311);
  { set up the device table defaults. }
  ZeroEntry.IntrCause.Number    := 0; { no cause for interrupts yet }
  ZeroEntry.IntrPriority.Number := 0; { turn off all priority bits }
  ZeroEntry.EnableMask.Number   := 0; { mask all interrupts for now }
  ZeroEntry.ATNCause.Number     := 0;
  ZeroEntry.pCirBuf   := nil;
  ZeroEntry.pStatus   := nil;
  ZeroEntry.pDataCtrl := nil;
  ZeroEntry.DeviceType  := Dev_Unused;
  ZeroEntry.Reserved[0] := 0;

  for i := 1 to MaxUnit do            { set remaining entries }
          InitDeviceTable[i] := ZeroEntry; 

  InitDeviceTable[IOStart].pDataCtrl := InitInterruptVectors;

  InitDeviceTable[Keyboard].IntrPriority.Bits[KeyBd_IntPr] := true; 
  InitDeviceTable[Ether10].IntrPriority.Bits[Ether_IntPr] := true;
  InitDeviceTable[HardDisk].IntrPriority.Bits[Disk_IntPr] := true;
  InitDeviceTable[EIODisk].IntrPriority.Bits[Disk_IntPr] := true;
  InitDeviceTable[Floppy].IntrPriority.Bits[Floppy_IntPr] := true;  
  InitDeviceTable[GPIB].IntrPriority.Bits[GPIB_IntPr] := true;
  InitDeviceTable[RSA].IntrPriority.Bits[RSA_IntPr] := true;
  InitDeviceTable[RSB].IntrPriority.Bits[RSB_IntPr] := true;
  InitDeviceTable[PointDev].IntrPriority.Bits[Point_IntPr] := true;
  InitDeviceTable[Clock].IntrPriority.Bits[Clock_IntPr] := true;
  InitDeviceTable[Speech].IntrPriority.Bits[Speech_IntPr] := true;
  InitDeviceTable[ScreenOut].IntrPriority.Bits[Screen_IntPr] := true;
  InitDeviceTable[Z80].IntrPriority.Bits[Z80_IntPr] := true;

  { microcode needs these pointers at StartIO time }
  
  SetDDS(312);
  New( IOSegNum, 4, PDskCtrl);
  InitDeviceTable[HardDisk].PDataCtrl := Recast( PDskCtrl, Pointer);
  
  SetDDS(313);
  New( IOSegNum, 4, PtrDCA);
  InitDeviceTable[EIODisk].PDataCtrl := Recast( PtrDCA, Pointer);

  SetDDS(314);                
  new( IOSegNum, 4, KrizInfo );
  InitDeviceTable[PointDev].pDataCtrl := recast( KrizInfo, pointer );

  SetDDS(315);
  new( IOSegNum, 4, TimeBuf );
  InitDeviceTable[Timer].pDataCtrl := recast( TimeBuf, pointer );
   
  SetDDS(316);
  InitDeviceTable[ScreenOut].pDataCtrl := Vid_SetUPUDevTab;  

end;   {InitDeviceTable}

var
  pMsg : pZ_Msg;
  
begin { InitIO }

  SetDDS( 301 );

  Z_IntDisabled  := false;
  Z_MsgNotAvailable := 0;
  KeyEnabled     := false;
  {$ifc debug 
  then}    IOSegNum := 0;
  {$elsec} IOSegNum := IOSeg;
  {$endc}

  for i := 0 to MaxUnit do begin   { don't allow the raising of exceptions }  
    RaiseException[i].Attention := false;
    RaiseException[i].DataAvailable := false
    end;

  new( IOSegNum, 4, pUDevTab );   { allocate the device table }
  SetDDS(310);
  pUDevTab^ := InitDeviceTable;   { Initialize the device table }

  setdds(338);
  Repeat Until CF_Init;
 
  {$ifc not debug then}
    SetDDS(340);                  { Initialize Screen Package }
    ScreenInit;
    {$endc}
  
  SetDDS(350);
  InLineByte({IntOff} 105);       { Turn off QCode Interrupts }
  {$ifc debug 
  then}
    LoadAdr(pUDevTab);
    StartIO(EP_IOStart);
  {$elsec}
    LoadAdr(pUDevTab^);  { Put the Virtual Address of Device Table on EStack }
    StartIO(EP_IOStart);
    DDS := DDS+5;                 { account for increments in microcode }
  {$endc}
  InLineByte({INTON} 106);        { enable interrupts }

 
  SetDDS(360);                    { allocate the Z80 messages }
  for i := 0 to 15 do begin
    new( IOSegNum, 4, pMsg );
    Z_QSysMsg( pMsg )
    end;

  SetDDS(370);
  Vid_Initialize;      SetDDS( 380 );       { Initialize the Video     }
  Key_Initialize;      SetDDS( 390 );       { Initialize Keyboard      }
  Dsk_Initialize;      SetDDS( 400 );       { Initialize the Harddisk  }
  Flp_Initialize;      SetDDS( 410 );       { Initialize the Floppy    }
  GPB_Initialize;      SetDDS( 430 );       { Initialize the GPIB      }   
  RS_Initialize;       SetDDS( 440 );       { Initialize the RS232     }
  Ptr_Initialize;      SetDDS( 450 );       { Initialize the Point Dev }
  Clk_Initialize;      SetDDS( 460 );       { Initialize the Clock     }
  Z80_Initialize;      SetDDS( 470 );       { Initialize the Z80       } 

  SetDDS(499);

end { InitIO };

procedure ReInitDevices;

{----------------------------------------------------------------------}
{
{  Abstract
{      Initialize Z80 devices which were not initialized before due to
{      lack of Z80 support.  (The Z80 proms aren't large enough to support
{      all the devices.  Thus, the system must load the Z80 program into
{      the Z80 ram sometime during initialization.  After it does this,
{      it calls us so that all the devices work.)
{
{-----------------------------------------------------------------------}

begin
 
  GPB_Initialize;      SetDDS( 430 );       { Initialize the GPIB      }   
  RS_Initialize;       SetDDS( 440 );       { Initialize the RS232     }
  Ptr_Initialize;      SetDDS( 450 );       { Initialize the Point Dev }
  Clk_Initialize;      SetDDS( 460 );       { Initialize the Clock     }

end.
