{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOPointDev;

{--------------------------------------------------------------------------
{
{ IOPointDev - 'Private' PointDev type and variable declarations - available
{          to the IO subsystem.  PointDev routines.
{ 
{ Copyright (C) 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       IOPointDev exports variables, constants, and procedures
{       the IO subsystem uses to do PointDev IO.
{
{-------------------------------------------------------------------------}

{ $Version V0.4 for POS}
{-------------------------------------------------------------------------}
{ Change Log:
{
{ 10 Feb 83  V0.4 Roger Riggs
{            Fixed documentation
{
{ 10 Jan 83  V0.3 August G. Reinig
{            Changed PointBuf to agree with reality.
{
{ 04 Jan 83  V0.2 August G. Reinig
{            Changed calls to SetDDS.
{
{ 14 Dec 82  V0.1 August G. Reinig
{            Added Sense command,  changed to deal with Z80 refusing PointDev
{            commands.
{
{ 22 Sep 82  V0.0 August G. Reinig
{            Created module.
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}

imports IO_Unit from IO_Unit;

const  { Fudge factors for Kriz Tablet }
  KrizXFudge =   64;  { actual range of X is 0..895, of Y is 0..1151 }
  KrizYfudge = 1087;  { of TabAbsX : 0..895,  of TabAbsY : 0..1151 }
                      { of TabRelX : -64..831    limited to 0..767 }
                      { of TabRelY : 1087..-64   limited to 1023..0 }
  
type 
  pPointBuf = ^PointBuf;
  PointBuf = packed record
    XPos:    integer;           { X position of the pointer }
    YPos:    integer;           { Y position of the pointer }
    Buttons: integer;           { indicates which buttons are pressed }
    Filler:  integer;           { so micro code's area is quad word aligned }
    UCodeArea : packed array[0..3] of integer
    end;

{ On the buttons, the least significant bit indicates that the rightmost
  button is pressed.  The next bit indicates that the middle button is
  pressed.  The third bit indicates that the leftmost button is pressed.
  Bit seven indicates that the puck is off the pad. }

var  
  KrizInfo : pPointBuf;

procedure Ptr_Initialize;
procedure Ptr_PutStatus( var StatBlk : DevStatusBlock );
procedure Ptr_GetStatus( var StatBlk : DevStatusBlock );
procedure Ptr_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer;
                      StsPtr  : IOStatPtr );
procedure Ptr_Interrupt;


{***************************}   Private   {*****************************}

const
  debug = false;

{$ifc debug
then}    Imports TestSystem from TestSystem;
{$elsec} imports System from System;
{$endc}

imports IO_Private from IO_Private;
imports IOErrors from IOErrors;

const
  on = 255;
  off = 0;
  

var
  OurBuf : IOBufPtr;
  OurSts : IOStatPtr;
  Z80Responded : boolean;
  NakReceived : boolean;
  NotThere : boolean;
  OurMsg : Z_Msg;
  pPointDevStatus : pPointDevStat;

procedure Ptr_Interrupt;

{-------------------------------------------------------------------------
{
{ Abstract:
{
{       Handle PointDev interrupts, the will be only responses for
{       commands issued to the tablet.
{
{------------------------------------------------------------------------}

var
  HoldIntrCause : Z_CmdRegister;
  HoldATNCause  : integer;
  
begin

  loadexpr( PointDev );
  startio( EP_ReadCause );
  storexpr( HoldATNCause );
  storexpr( HoldIntrCause.Number );
  
  Z80Responded := true;
  if HoldIntrCause.Bits[Dev_NakReceived] then NakReceived := true;
  
  inlinebyte( {INTON} 106 );

end;

procedure Ptr_Initialize; 

{---------------------------------------------------------------------------
{
{ Abstract:
{
{       Prepare for IO to the PointDev.
{       Determine if tablet is connected by enabling it and
{       waiting a short while for it to send current coordinates.
{       If no data is received within a short time flag the tablet
{       as not being connected.
{
{---------------------------------------------------------------------------}

var
  time : long;

begin
  
  SetDDS(441);
  new( IOSegNum, 1, OurBuf );
  new( IOSegNum, 1, OurSts );

  Z80Responded := true;
  NakReceived := false;
                     
  OurMsg.pNext := nil;
  OurMsg.UcodeArea := 0;
  OurMsg.SOMDelimiter := Z_SOM;
  OurMsg.Device := PointDev;  
  
  if pUDevTab^[PointDev].pStatus = nil then begin
    SetDDS(442);
    new( IOSegNum, 4, pPointDevStatus );
    pUDevTab^[PointDev].pStatus := recast( pPointDevStatus, pointer );
    pPointDevStatus^.OnOff := 0;  { ie Off }
    end;
  
  SetDDS(443);
  loadexpr( lor( shift( 1, Dev_StatReceived )
          , lor( shift( 1, Dev_AckReceived )
          , shift( 1, Dev_NakReceived ))));
  loadexpr( PointDev );
  StartIO( EP_SetEnableMask );
            
  SetDDS(444);
  NotThere := false;   { assume the PointDev is there }
  Ptr_UnitIO( OurBuf, IOSense, 0, OurSts );
  if OurSts^.SoftStatus <> IOEIOC
  then NotThere := true
  else begin
    KrizInfo := recast( pUDevTab^[PointDev].pDataCtrl, pPointBuf );
    KrizInfo^.XPos := -1;                 { set an impossible pointer value }
    
    SetDDS(445);
    OurBuf^[0] := on;                     { turn the pointer device on }
    Ptr_UnitIO( OurBuf, IOConfigure, 1, OurSts );
    while OurSts^.SoftStatus <> IOEIOC do { hang if we failed };
    
    SetDDS(446);
    Time := TimeBuf^;                  { get current time }
    while (TimeBuf^ - Time <= 3)      { wait for 3 jiffies }
      and (KrizInfo^.XPos = -1 ) do;   { or for the tablet to send data }
    KrizTabConnected := (KrizInfo^.XPos <> -1);
    
    SetDDS(447);
    OurBuf^[0] := off;                    { turn the pointer device off }
    Ptr_UnitIO( OurBuf, IOConfigure, 1, OurSts );
    while OurSts^.SoftStatus <> IOEIOC do { hang if we failed };
    end;

end;

procedure Ptr_PutStatus( var StatBlk : DevStatusBlock );

{-------------------------------------------------------------------------
{
{ Abstract:
{
{       Put a status to the PointDev,  otherwise known as the KrizTablet
{       The tablet can only be turned enabled or disabled.
{
{ Parameters:
{
{       StatBlk - Address of status block
{
{
{-------------------------------------------------------------------------}


begin
  
  if not NotThere then begin   { i.e. if There }
    if StatBlk.KrizEnable
    then OurBuf^[0] := on
    else OurBuf^[0] := off;
  
    Ptr_UnitIO( OurBuf, IOConfigure, 1, OurSts );
    end;

end;





procedure Ptr_GetStatus( var StatBlk : DevStatusBlock ); 

{-------------------------------------------------------------------------
{
{ Abstract:
{
{      Get status from the PointDev.  
{
{ Parameters:
{
{       StatBlk - Address of status block
{
{-------------------------------------------------------------------------}

var
  pPtrStat : pPointDevStat;

begin 
  
  if not NotThere then begin     { i.e. if There }
    Ptr_UnitIO( OurBuf, IOSense, 0, OurSts );
    pPtrStat := recast( OurBuf, pPointDevStat );
  
    if pPtrStat^.OnOff = 0
    then StatBlk.KrizEnable := false
    else StatBlk.KrizEnable := true;
    end;

end;

procedure Ptr_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer;
                      StsPtr  : IOStatPtr ); 

{-------------------------------------------------------------------------
{
{ Abstract:
{
{       Execute IOSense and IOConfig commands for IOPointDev
{
{ Parameters:
{
{      Bufr - buffer for data transfers, if requested.
{
{      Command - operation to be performed on the device.
{
{      ByteCnt - number of bytes to be transferred.
{
{      StsPtr - resultant status from the operation.
{
{-------------------------------------------------------------------------}

var
  PtrMsg : Z_MsgPtrKludge;
  pPtrStatus : pPointDevStat;

procedure Error( ErrCode : integer );

begin

  StsPtr^.SoftStatus := Errcode;
  exit( Ptr_UnitIO )
  
end;

begin
  
  if NotThere then Error( IOEBUN );
  
  case command of
    IOSense     : begin
      OurMsg.Command := ord( Z_Sense );
      OurMsg.ByteCount := Z_NoData;
      end;

    IOConfigure : begin
      if ByteCnt <> 1 then Error( IOEBSE );
   
      OurMsg.Data[Z_FirstData] := land(Bufr^[0], 255);
      OurMsg.Command := ord( Z_Config );
      OurMsg.ByteCount := Z_FirstData;
      end;
    
    otherwise   : Error( IOEILC );
    end;

  Z80Responded := false;
  NakReceived := false;
        
  loadexpr( 0 );
  startio( EP_UcodeMsg );
  storexpr( PtrMsg.Segment );
  storexpr( PtrMsg.Offset );
  if PtrMsg.pMsg = nil then PtrMsg.pMsg := Z_DqSysMsg;
  
  PtrMsg.pMsg^ := OurMsg;

  loadexpr( ord(Z_Q0) );
  {$ifc debug 
  then}    loadadr( PtrMsg.pMsg );
  {$elsec} loadadr( PtrMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );   
   
  repeat until Z80Responded;
  if NakReceived then Error( IOEUDE );
  
  StsPtr^.SoftStatus := IOEIOC;
  StsPtr^.BytesTransferred := 1;
  
  if Command = IOSense then begin
    pPtrStatus := recast( Bufr, pPointDevStat );
    pPtrStatus^.OnOff := pPointDevStatus^.OnOff;
    end;
    
end.
