Module AsyncGPIB;

{--------------------------------------------------------------------------
{
{ Abstract: module to do asynchronous hi-volume gpib i/o.
{
{      (C) ICL 1983
{
{-------------------------------------------------------------------------}

{-------------------------------------------------------------------------
{
{ Change Log:
{   4 May 83   V1.0  Sandeep Johar    Change log created, copyright added.
{
{  ?? ??? ??   V0.0  ???@ICL          Created, tested, debugged.
{
{-------------------------------------------------------------------------}


{*****************************}   Exports   {*****************************}

imports IO_Unit from IO_Unit;
  
var
  NakReceived : boolean;
  Z80Replied  : boolean;

exception AsTimeOut;
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Raised when a gpib hi-vol write times out.
{       See GpAsWait.
{-----------------------------------------------------------------------------}

procedure GpAsyncInit;
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Initialize the GPIB for Asynchronous High Volume IO.
{
{-----------------------------------------------------------------------------}

procedure GpAsyncWrite( Bufr      : IOBufPtr;
                        ByteCnt   : integer;
                        WaitTicks : double);
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Initiate a high volume write to the GPIB
{       NB The segment the buffer is in must be
{       locked down before calling this procedure.
{       (Increment the IO count for the segment)
{-----------------------------------------------------------------------------}

procedure GpAsWait( WaitTicks: Double);
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Wait until high volume I/O initiated by GpAsyncWrite has terminated.
{       WaitTicks is used to specify a time-out length on the I/O.
{       WaitTicks = 0 means wait for ever.
{-----------------------------------------------------------------------------}

procedure GpAsClose;
{-----------------------------------------------------------------------------}
{
{ Abstract
{       This procedure cleans up after asynchronous hi-vol i/os
{       have been done. It disposes of data areas created by
{       GpAsyncInit.
{-----------------------------------------------------------------------------}

{***************************}   Private   {*****************************}

Const 
  debug = false;

imports IO_Private from IO_Private;
imports System from System;
imports Virtual from Virtual;


type
  SimBufr = packed array[0..6] of integer;  { large enough for an IOSense }
  pSimBufr = ^SimBufr;
   
var
  OurBufr     : pSimBufr;
  OurStat     : IOStatPtr;
  OurMsg      : Z_Msg;
  ReplyMask   : Z_CmdRegister; { mask for determining if Z80 received msg }
  pGPIBSts    : pGPIBStat;     { points to status info in device table }
  pGPIBHiVol  : pHiVolBlock;   { points to high volume block }    



procedure gpAsyncInit;
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Initialize the GPIB for Asynchronous High Volume IO.
{
{-----------------------------------------------------------------------------}

var
  i : integer;
  
begin { gpAsyncInit }

  Z80Replied := true;
  NakReceived := false;
  
  OurMsg.pNext := nil;   
  OurMsg.UCodeArea    := 0;
  OurMsg.SOMDelimiter := Z_SOM;
  OurMsg.ByteCount    := 255;   
  OurMsg.Device       := GPIB;
  OurMsg.Command      := 255;
  for i := Z_FirstData to Z_Maxdata do OurMsg.Data[i] := 0;
    
  new( IOSegNum, 1, OurBufr );   { GPB_WriteCharacter and GPB_PutStatus }
 
  pGPIBHiVol := recast(pUDevTab^[GPIB].pDataCtrl,pHiVolBlock);  

end { gpAsyncInit };

procedure gpAsyncWrite( Bufr      : IOBufPtr;
                        ByteCnt   : integer;
                        WaitTicks : double);
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Initiate a high volume write to the GPIB
{       NB The segment the buffer is in must be
{       locked down before calling this procedure.
{       (Increment the IO count for the segment)
{-----------------------------------------------------------------------------}

type 
  DataArray = packed array[0..0] of Z_Data;
  pDataArray = ^DataArray;
  ByteArray = packed array[0..0] of 0..255;  
  pByteArray = ^ByteArray;
  ManyPtrs = packed record case integer of
    1 : (pBuffer : IOBufPtr);      { how user passes buffer to us }
    2 : (pByte   : pByteArray);    { lets us get at the individual bytes } 
    3 : (pData   : pDataArray);    { lets us get at a Msg worth of bytes }
    4 : (pStat   : pGPIBStat);     { lets us assign the status easily }
    5 : (Offset  : integer;        { lets us lock buffer in memory }
         Segment : integer)
    end;
    
var
  Users     : ManyPtrs;
  RegMsg    : Z_MsgPtrKludge;


begin
  GpAsWait(WaitTicks);
  
  Users.pBuffer := Bufr;     { we may need to look at the pointer differently }

  pGPIBHiVol^.DataByteCnt := ByteCnt;       { transfer this many bytes }
  pGPIBHiVol^.pDataBuffer := Users.pBuffer; { to/from this location }

  OurBufr^[0] := ByteCnt;    { to transfer, so that ByteCnt is in the }
  Users.pBuffer := recast( OurBufr, IOBufPtr ); { message to the Z80  }
  
  OurMsg.Command := ord(Z_OutHiVolumeStart);
  OurMsg.ByteCount := Z_NoData + 2;
  {$R-} OurMsg.Data := Users.pData^[0]; {$R=}

  loadexpr(0);
  startio( EP_UcodeMsg );           { get a Z_Msg }
  storexpr( RegMsg.Segment );
  storexpr( RegMsg.Offset );
  if RegMsg.pMsg = nil then RegMsg.pMsg := Z_DqSysMsg;

  
  RegMsg.pMsg^ := OurMsg;        

  loadexpr( ord(Z_Q0) );                { send the message }
  {$ifc debug 
  then}    loadadr( RegMsg.pMsg );
  {$elsec} loadadr( RegMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );   
end;

procedure GpAsWait( WaitTicks: double);
{-----------------------------------------------------------------------------}
{
{ Abstract
{       Wait until high volume I/O initiated by GpAsyncWrite has terminated.
{       WaitTicks is used to specify a time-out length on the I/O.
{       WaitTicks = 0 means wait for ever.
{-----------------------------------------------------------------------------}
var
  bc        : integer;
  StartTime : long;
  TimeOut   : packed record case boolean of
                 true: (cnt: long);
                 false: (dbl: double);
              end;
begin
  TimeOut.dbl := WaitTicks;
  if TimeOut.cnt <= 0  { wait for ever ? }
  then
     repeat
       bc := pGPIBHiVol^.DataByteCnt
     until
       bc = 0
  else
     begin
        StartTime := TimeBuf^;
        repeat
           if TimeBuf^ - StartTime > TimeOut.cnt
           then begin
                TimeOut.cnt := 0;
                GpAsClose;
                Raise AsTimeOut;
                end;
           bc := pGPIBHiVol^.DataByteCnt;
        until
           bc = 0;
     end;
end;

procedure GpAsClose;
{-----------------------------------------------------------------------------}
{
{ Abstract
{       This procedure cleans up after asynchronous hi-vol i/os
{       have been done. It disposes of data areas created by
{       GpAsyncInit.
{-----------------------------------------------------------------------------}
begin
   dispose(OurBufr);
end.
