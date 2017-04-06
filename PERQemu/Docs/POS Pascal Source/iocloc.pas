{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOClock;

{--------------------------------------------------------------------------
{
{ IOClock - 'Private' Clock type and variable declarations - available
{          to the IO subsystem.  Clock routines.
{ 
{
{ Abstract:
{       IOClock exports variables, constants, and procedures the IO subsystem
{       uses to do Clock IO.
{
{ Copyright (C) 1982, 1983 Three Rivers Computer Corporation
{-------------------------------------------------------------------------}

{ $Version V0.2 for POS}
{-------------------------------------------------------------------------}
{ Change Log:
{
{ 10 Jan 83  V0.2 August G. Reinig
{            Changed calls to SetDDS.
{
{ 14 Dec 82  V0.1 August G. Reinig
{            Restricted Configure command to disallow setting of seconds.
{            Changed it to deal with the Z80 not accepting Clock commands.
{            Modified the initialization routine so that it can be called
{            more than once.
{
{ 12 Oct 82  V0.0 August G. Reinig
{            Created module.
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}

imports IO_Unit from IO_Unit;

procedure Clk_Initialize;
procedure Clk_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer;
                      StsPtr  : IOStatPtr );
procedure Clk_Interrupt;


{***************************}   Private   {*****************************}

const
  debug = false;
  
{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} Imports System from System;
{$endc}

imports IO_Private from IO_Private;
imports IOErrors from IOErrors;

var
  WaitForReply : boolean;
  NakReceived  : boolean;
  NotThere     : boolean;
  pClkStatus   : pClockStat;
  OurMessage   : Z_Msg;

procedure Clk_Interrupt;

{---------------------------------------------------------------------------}
{
{ Abstract
{      Handle interrupts from the clock
{
{---------------------------------------------------------------------------}

var
  HoldIntrCause : Z_CmdRegister;
  HoldATNCause  : integer;
  
begin
  
  loadexpr( Clock );
  startio( EP_ReadCause );
  storexpr( HoldATNCause );
  storexpr( HoldIntrCause.Number );
  
  inlinebyte( {INTON} 106 );
  
  WaitForReply := false;
  if HoldIntrCause.Bits[Dev_NakReceived] then NakReceived := true;

end;

procedure Clk_Initialize; 

{---------------------------------------------------------------------------}
{
{ Abstract
{      Initialize the clock by enabling interrupts
{
{---------------------------------------------------------------------------}

var
  Bufr : pClockStat;
  StsPtr  : IOStatPtr;
    
begin
  
  SetDDS(451);
  WaitForReply := false;
  NakReceived := false;
  
  OurMessage.pNext := nil;
  OurMessage.UCodeArea := 0;
  OurMessage.SOMDelimiter := Z_SOM;
  OurMessage.Device := CLock;

  if pUDevTab^[Clock].pStatus = nil then begin
    SetDDS(452);
    new( IOSegNum, 4, pClkStatus );
    pUDevTab^[Clock].pStatus := recast( pClkStatus, pointer );
    pClkStatus^.Cycles  := 0;
    pClkStatus^.Day     := 0;
    pClkStatus^.Month   := 0;
    pClkStatus^.Year    := 0;
    pClkStatus^.Hour    := 0;
    pClkStatus^.Minute  := 0;
    pClkStatus^.Second  := 0;
    pClkStatus^.Jiffies := 0;
    end;

  SetDDS(453);  
  loadexpr( lor( shift( 1, Dev_AckReceived )
          , lor( shift( 1, Dev_NakReceived )
          ,      shift( 1, Dev_StatReceived ) ) ) );
  loadexpr( Clock );
  StartIO( EP_SetEnableMask );
   
  SetDDS(454);
  NotThere := false;
  new( Bufr );
  new( StsPtr );
  SetDDS(455);
  Clk_UnitIO( recast( Bufr, IOBufPtr ), IOSense, 0, StsPtr );
  NotThere := StsPtr^.SoftStatus <> IOEIOC;  
  SetDDS(456);
  dispose( Bufr );
  dispose( StsPtr );   

end;

procedure Clk_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer;
                      StsPtr  : IOStatPtr ); 

{-------------------------------------------------------------------------}
{
{ Abstract
{       Do IO to the PointDev.  All we can do is a configure command
{       which turns the PointDev on or off.
{
{-------------------------------------------------------------------------}

procedure Error( ErrCode : integer );

begin
  
  StsPtr^.SoftStatus := ErrCode;
  exit( Clk_UnitIO )

end;


type
  ByteArray = packed array[0..7] of 0..255;  { max for ByteCnt is 8 }
  pByteArray = ^ByteArray;
  ManyPtrs = packed record case integer of
    1 : (pBuffer : IOBufPtr );
    2 : (pBytes  : pByteArray );
    3 : (pStat   : pClockStat )
    end;

var
  Users : ManyPtrs;
  PtrMsg : Z_MsgPtrKludge;
  ThisCmd : 0..255;
  i : integer;

begin
  
  Users.pBuffer := Bufr;                 
  StsPtr^.BytesTransferred := 0;
  
  if NotThere then Error( IOEBUN );

  case command of
    IOSense     : begin
                    ByteCnt := 0;
                    ThisCmd := ord( Z_Sense )
                  end;
    IOConfigure : if ByteCnt <> 6
                  then Error( IOEBSE )
                  else ThisCmd := ord( Z_Config );
    Otherwise   : Error( IOEILC )
    end;
  
  OurMessage.ByteCount := Z_NoData + ByteCnt;
  OurMessage.Command := ThisCmd;
  {$R-} 
  for i := 0 to ByteCnt - 1
    do OurMessage.Data[Z_FirstData+i] := Users.pBytes^[i];
  {$R=}
  
  loadexpr( 0 );
  startio( EP_UcodeMsg );
  storexpr( PtrMsg.Segment );
  storexpr( PtrMsg.Offset );
  if PtrMsg.pMsg = nil then PtrMsg.pMsg := Z_DqSysMsg;
  
  PtrMsg.pMsg^ := OurMessage;

  WaitForReply := true;
  NakReceived := false;

  loadexpr( ord(Z_Q0) );
  {$ifc debug 
  then}    loadadr( PtrMsg.pMsg );
  {$elsec} loadadr( PtrMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
  
  while WaitForReply do;
  
  if Command = IOSense 
  then begin
    if NakReceived then Error( IOEUDE );
    Users.pStat^ := pClkStatus^;
    StsPtr^.BytesTransferred := 8
    end
  else if NakReceived then Error( IOECDI );
  
  Stsptr^.SoftStatus := IOEIOC

end.
