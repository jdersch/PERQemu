{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOZ80;

{--------------------------------------------------------------------------
{
{ IOZ80 - 'Private' Z80 type and variable declarations - available
{          to the IO subsystem.  Z80 routines.
{ 
{ Copyright (C) 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       IOZ80 exports variables, constants, and procedures the IO subsystem
{       uses to do Z80 IO.
{
{-------------------------------------------------------------------------}

{ $Version V0.4 for POS}
{-------------------------------------------------------------------------}
{ Change Log:
{
{ 10  Feb 83 V0.4 Roger Riggs
{            Fixed documentation
{
{ 04 Jan 82  V0.3 August G. Reinig
{            Added calls to SetDDS.  Changed the way Z80_UnitIO provides
{            sense data.  Made Z80_UnitIO wait for the Z80 to reply.
{
{ 16 Dec 82  V0.2 August G. Reinig
{            Changed format of Z80 messages to agree with what the Z80 expects
{
{ 14 Dec 82  V0.1 August G. Reinig
{            General improvements and bug fixing.
{
{ 24 Nov 82  V0.0 August G. Reinig
{            Created module.
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}

imports IO_Unit from IO_Unit;

procedure Z80_Initialize;
procedure Z80_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer; 
                      LogAdr  : double;
                      StsPtr  : IOStatPtr );
procedure Z80_Interrupt;


{***************************}   Private   {*****************************}

const
  debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} Imports System from System;
{$endc}

imports IO_Private from IO_Private;
imports IOErrors from IOErrors;
imports Virtual from Virtual;

var
  NotThere     : boolean;   { tells us if Z80 is a legal device }
  WaitForReply : boolean;
  NakReceived  : boolean;
  OurMsg : Z_Msg;
  pZ80HiVol : pHiVolBlock; 
  pZ80Status : pZ80Stat;

procedure Z80_Interrupt;

{---------------------------------------------------------------------------
{
{ Abstract:
{
{       Handle interrupts from the clock. Interrupts from the
{       Clock are responses to commands sent to the clock.
{
{---------------------------------------------------------------------------}

var
  HoldIntrCause : Z_CmdRegister;
  HoldATNCause  : integer;
  
begin
  
  loadexpr( Z80 );
  startio( EP_ReadCause );
  storexpr( HoldATNCause );
  storexpr( HoldIntrCause.Number );
  
  inlinebyte( {INTON} 106 );
  
  WaitForReply := false;
  if HoldIntrCause.Bits[Dev_NakReceived] then NakReceived := true;
  
end;

procedure Z80_Initialize; 

{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Initialize the clock by enabling interrupts.  This routine can be
{      called any number of times.  (Thus the check for nil pointers in
{      the device table.)  An initial sense is done to determine
{      if the clock is supported by the hardware.
{
{---------------------------------------------------------------------------}

var
  Bufr : pZ80Stat;
  StsPtr : IOStatPtr;
  LogAdr : double;

begin
  
  SetDDS(461);
  WaitForReply := false;
  NakReceived := false;
  
  OurMsg.pNext := nil;
  OurMsg.UCodeArea := 0;
  OurMsg.SOMDelimiter := Z_SOM;
  OurMsg.Device := Z80;
  
  if pUDevTab^[Z80].pDataCtrl = nil then begin   
    SetDDS(462);
    new( IOSegNum, 4, pZ80HiVol );
    pUDevTab^[Z80].pDataCtrl := recast( pZ80HiVol, pointer );
    pZ80HiVol^.DataByteCnt := 0;
    pZ80HiVol^.pDataBuffer := nil;
    end;
  
  if pUDevTab^[Z80].pStatus = nil then begin
    SetDDS(463);
    new( IOSegNum, 4, pZ80Status );
    pUDevTab^[Z80].pStatus := recast( pZ80Status, pointer );
    pZ80Status^.MajorVersionNum := 0;
    pZ80Status^.MinorVersionNum := 0;
    end;
    
  SetDDS(464);
  loadexpr( lor( shift( 1, Dev_StatReceived )
          , lor( shift( 1, Dev_AckReceived )
               , shift( 1, Dev_NakReceived ))));
  loadexpr( Z80 );
  StartIO( EP_SetEnableMask );

  NotThere := false;
  
  SetDDS(465);
  new( Bufr );
  new( StsPtr );
  SetDDS(466);
  Z80_UnitIO( recast( Bufr, IOBufPtr ), IOSense, 0, LogAdr, StsPtr );
  NotThere := (StsPtr^.SoftStatus <> IOEIOC);
  SetDDS(467);
  dispose( Bufr );
  dispose( StsPtr );
    
end;

procedure Z80_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer;
                      LogAdr  : double;
                      StsPtr  : IOStatPtr ); 

{-------------------------------------------------------------------------
{
{ Abstract:
{
{       Do IO to the Z80 device.  High volume read and write
{       allow loading and reading Z80 memory, Sense to
{       determine Z80 version number, Writeregs to call
{       a location in Z80 memory.
{
{ Parameters:
{
{      Bufr - buffer for data transfers, if requested.
{
{      Command - operation to be performed on the device.
{
{      ByteCnt - number of bytes to be transferred.
{
{      LogAdr - address used for WriteRegs
{
{      StsPtr - resultant status from the operation.
{
{-------------------------------------------------------------------------}


type
  ByteArray = packed array[0..7] of 0..255;  { max for ByteCnt is 8 }
  pByteArray = ^ByteArray;
  ManyPtrs = packed record case integer of
    1 : (pBuffer : IOBufPtr );
    2 : (pByte   : pByteArray );
    3 : (pStat   : pZ80Stat );
    4 : (Offset  : integer;
         Segment : integer )
    end;

var
  Users : ManyPtrs;
  PtrMsg : Z_MsgPtrKludge;
  MustDec : boolean;         { true -> must call DecIOCount }


procedure Error( ErrCode : integer );

begin
  
  if MustDec then DecIOCount( Users.Segment );
  StsPtr^.SoftStatus := ErrCode;
  exit( Z80_UnitIO )

end;

begin
  
  MustDec := false;
  Users.pBuffer := Bufr;                 
  
  StsPtr^.BytesTransferred := 0;
  if NotThere then Error( IOEBUN );

  case command of
    IOSense :begin
      OurMsg.ByteCount := Z_NoData;
      OurMsg.Command := ord( Z_Sense );
      end;
      
    IOWriteRegs : begin  
      OurMsg.Command := ord(Z_WriteRegisters);
      OurMsg.Data[Z_FirstData] := land( LogAdr[0], 255 );
      OurMsg.Data[Z_FirstData+1] := shift( LogAdr[0], -8 );
      OurMsg.ByteCount := Z_FirstData + 1;
      end;

    IOWriteHiVol, 
    IOReadHiVol : begin
      pZ80HiVol^.DataByteCnt := ByteCnt;
      pZ80HiVol^.pDataBuffer := Users.pBuffer;
      
      if land(Users.Offset, 3) <> 0 then Error( IOEBAE ); { quad aligned? }
      if ByteCnt < 2 then Error( IOEBSE );
      
      IncIOCount( Users.Segment );
      MustDec := true;
      
      if command = IOWriteHiVol
      then OurMsg.Command := ord(Z_OutHiVolumeStart)
      else OurMsg.Command := ord(Z_InHiVolumeStart);

      OurMsg.Data[Z_FirstData] := land( LogAdr[0], 255 );
      OurMsg.Data[Z_FirstData+1] := shift( LogAdr[0], -8 );
      OurMsg.Data[Z_FirstData+2] := land( ByteCnt, 255 );
      OurMsg.Data[Z_FirstData+3] := shift( ByteCnt, -8 );
      OurMsg.ByteCount := Z_FirstData + 3;
      end;

    Otherwise   : Error( IOEILC )
    end;

  loadexpr( 0 );
  startio( EP_UcodeMsg );
  storexpr( PtrMsg.Segment );
  storexpr( PtrMsg.Offset );
  if PtrMsg.pMsg = nil then PtrMsg.pMsg := Z_DqSysMsg;
  
  PtrMsg.pMsg^ := OurMsg;

  WaitForReply := true;
  NakReceived := false;

  loadexpr( ord(Z_Q0) );
  {$ifc debug 
  then}    loadadr( PtrMsg.pMsg );
  {$elsec} loadadr( PtrMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
  
  while WaitForReply do;
  if NakReceived then Error( IOEUDE );
  
  case command of
    IOSense : begin
      Users.pStat^ := pZ80Status^;
      StsPtr^.BytesTransferred := 2;
      end;

    IOWriteHiVol,
    IOReadHiVol : begin
      DecIOCount( Users.Segment );
      StsPtr^.BytesTransferred := ByteCnt - pZ80HiVol^.DataByteCnt 
      end; 

    otherwise : StsPtr^.BytesTransferred := ByteCnt;
    end;
  
  Stsptr^.SoftStatus := IOEIOC

end.
