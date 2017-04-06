{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module gpib;
{-----------------------------------------------------------
{
{ Abstract:
{    Support routines for PERQ GPIB devices.  The package maintains a
{    buffer (gpCommandBuffer) which holds either data bytes (sent with
{    procedure gpPutByte) or Auxillary commands (sent with gpAuxCommand).
{    The buffer is sent to the 9914 when full, or when gpFlushBuffer is
{    called.  If the buffer has data bytes when gpAuxCommand is called,
{    it will do a gpFlushBuffer.  Similarly, when gpPutByte is called,
{    it will flush the buffer if auxiliary commands are in gpCommandBuffer.
{
{ Written by Brian Rosen
{           Copyright(C) 1980, 1983, Three Rivers Computer Corporation
{----------------------------------------------------------------}
{$Version 1.8 }
{
{   Change log:
{
  31 Oct 83  V1.8  DLK 1. Add global variable, gpTimeOut, so user can set
                          timeout for IO operations.
                       2. Add procedures gpFlushOutput, gpFlushInput, gpReset.
                       3. Make gpGetByte also use gpTimeOut.
                       4. Modify bus configuration code to use the new cmds
                          IODevRead and IODevWrite.
                       5. Clean up some of the comments.
  
  14 Apr 83  V1.7  ICL Dave Anderson..initialised LogAdr argument to zero
                       (no time out of IO operations). Previously had a
                       random value.
  
  27 Jan 83  V1.6  RSR Modified calls to GPB_UnitIO to add LogAdr argument
                       Removed "The" from TheGPIBBitPad

  15 Dec 82  V1.5  AGR Changed Auxilary Command register number from 6
                       to 3 so that it agrees with new Z80 code.

  27 Oct 82  V1.4  AGR fixed to work with new IO system
                       added gpCleanup
                   
  27 Jan 82  V1.3  WJH added GPIBerror
                       added first test of gpBufPtr in gpAuxCommand
            
  15-Jun-81  V1.2  DAS Changed to compiler under POS D.
  
  2-Jan-81   V1.1  BR  fixed names of Controller Command Codes and
                       apAuxiliaryCommands to have "gp" prefix
                       to avoid name conflicts (especially get)
}


exports  

 const 
       GpibVersion = '1.8';
       gpBufSize = 12;
       gpBufMax = 11; {gpBufSize - 1}  

 { The following codes are the IEEE488-1975 Controller Command Codes.
   They are issued by the Controller-In-Charge while asserting ATN.  }
       gpacg = #000; {addressed group command}
       gpdcl = #024; {device clear}
       gpget = #010; {group execute trigger}
       gpgtl = #001; {go to local}
       gplag = #040; {listen address group}
       gpllo = #021; {local lockout}
       gpmla = #040; {my listen address}
       gpmta = #100; {my talk address}
       gpmsa = #140; {my secondary address}
       gpppc = #005; {parallel poll configure}
       gpppe = #140; {parallel poll enable}
       gpppd = #160; {parallel poll disable}
       gpppu = #025; {parallel poll unconfigure}
       gpscg = #140; {secondary copmmand group}
       gpsdc = #004; {selected device clear}
       gpspd = #061; {serial poll disable}
       gpspe = #060; {serial poll enable}
       gptct = #011; {take control}
       gptag = #100; {tahk address group}
       gpuag = #020; {universal address group}
       gpunl = #077; {unlisten}
       gpunt = #137; {untalk}

 type  { These commands are the major state change control commands
         of the TMS9914 chip which forms the interface to the GPIB.
         Consult the TI documentation on the TMS9914 for more information. }
       
       { These definitions are order dependent. }
       gpAuxiliaryCommands = (gpswrst,   {Chip Reset}
                              gpdacr,    {Release DAC holdoff}
                              gprhdf,    {Release RFD holdoff}
                              gphdfa,    {Holdoff all data}
                              gphdfe,    {Holdoff on End}
                              gpnbaf,    {Set NewByteAVailable false}
                              gpfget,    {Force Group Execute Trigger}
                              gprtl,     {Return to Local}
                              gpfeoi,    {force End or Identify}
                              gplon,     {Listen Only}
                              gpton,     {Talk Only}
                              gpgts,     {GoTo Standby}
                              gptca,     {Take Control Asynchronously}
                              gptcs,     {Take Control Synchronously}
                              gprpp,     {Request Parallel Poll}
                              gpsic,     {Set Interface Clear}
                              gpsre,     {Set Remote Enable}
                              gprqc,     {Request Control}
                              gprlc,     {Release Control}
                              gpdai,     {Disable All Interrupts}
                              gppts,     {Pass Through next Secondary}
                              gpstdl,    {Set T1 Delay}
                              gpshdw);   {Shadow Handshake}

       gpParmType = (gpOff, gpOn, gpDontCare);  {parameters for Aux Commands}
       gpByte = 0..255;    {Data byte for gpib transactions}
       gpRange = 0..gpBufMax;
       gpDeviceAddress = 0..31;         {legal addresses for devices on GPIB}
       gpBuffer = packed array [gpRange] of gpByte;
       gppBuffer = ^gpBuffer;  

 var   gpCommandBuffer: gppBuffer;      {place to put commands}
       gpBufPtr: 0..gpBufMax;          {pointer to gpCommandBuffer}
       gpHaveDataBytes, gpHaveAuxiliaryCommands: boolean;{true if buffer in use}       gpTimeOut: long;  { how long to wait for GPIB cmds, <= 0 means forever }

       
{   The package maintains a buffer (gpCommandBuffer) which holds
    either Data bytes (sent with procedure gpPutByte)
    or Auxillary Commands (sent with gpAuxCommand).
    The buffer is sent to the 9914 when full, or when gpFlushBuffer
    is called.  If the buffer holds data bytes when gpAuxCommand is
    called, it will do a gpFlushBuffer.  Similarly, when gpPutByte
    is called, it will flush the buffer if auxiliary commands are in
    gpCommandBuffer
}

 procedure gpInit;
           { Initialze GPIB package, called once only, turns off tablet,
             sets gpTimeOut to 0. }
 
 
 procedure gpAuxCommand(gpCmd: gpAuxiliaryCommands; gpParm:gpParmType);
           { Send an auxiliary command to TMS9914.
             Some commands require a parameter (gpOff/gpOn). }
 
 procedure gpPutByte(gpData: integer);
           { Put a data byte or a Control byte out on the data bus.
             TMS9914 must be in Controller Active State if the byte is a
             controller command byte.  Must be in Talk Only if a data byte. }
 
 procedure gpFlushBuffer;
           { Sends all bytes in buffer. }
 
 procedure gpITalkHeListens(gpAddr: gpDeviceAddress);
           { Set TMS9914 to be a Talker, set a device to be a listener.
             This procedure takes control of the bus, unlistens and untalks
             all devices (including itself), and sets a listener with
             MyListenAddress then sets TMS9914 to be the talker with TalkONly. }
 
 procedure gpHeTalksIListen(gpAddr: gpDeviceAddress);
           { Set TMS9914 to be a Listener, set a device to be a talker.
             This procedure takes control of the bus, unlistens and untalks
             all devices (including itself), and sets a talker with
             MyTalkAddress then sets TMS9914 to be the listener with ListenONly}

 procedure gpTbltOn;    
           { Turn the BitPad back on again. }  

 procedure gpTbltOff; 
           { Turn the BitPad (device address #10) off. }

 procedure gpSend(var gpBuf: gppBuffer; gpCount: gpRange);  
           { Send a buffer of user data to the 9914. }  

 procedure gpReceive( var gpBuf: gppBuffer; gpCount: gpRange);   
           {Get a buffer of data from the 9914 (Not implemented yet). }  

 function  gpGetByte: gpByte;      
           { Get a byte of data from the GPIB. }    

 procedure gpCleanup;
           { Cleans up after the GPIB package, turns the tablet back on. }

 procedure gpFlushOutput;
           { Flush the GPIB output pipeline from Perq through the Z80. }

 procedure gpFlushInput;
           { Flush the GPIB input pipeline from Z80 to the Perq. Any data
             received is left in the Perq input ring buffer. ATN is left
             asserted on the bus. }

 procedure gpReset;
           { Re-init the GPIB package and clear the bus interface and input
             ring buffer. IOReset is issued and Interface Clear is performed
             on the bus. }

exception GPIBerror(SoftStatus: integer);
{------------------------------------------
{Abstract:
{  Raised when GPIB encounters an error indication in softstatus from
{  UnitIO or IOCRead.  The condition should be corrected and the operation
{  retried.
{  The most likely error is a timeout: IOETIM (See IOErrors).
{-----------------------------------------}

private

imports IOGPIB from IOGPIB;
imports IO_Unit from IO_Unit;  
imports IO_Others from IO_Others;
imports IO_Private from IO_Private;
imports IOErrors from IOErrors; 
imports IOVideo from IOVideo;

var 
 gpStatPtr: IOStatPtr;
 gpTabMode : TabletMode;         { saves the current tablet mode }
 
procedure gpInit;

begin

  new(gpStatPtr);                {allocate a Status Pointer for UnitIO calls}
  new(gpCommandBuffer);          {allocate the buffer}

  gpTimeOut := 0;    {Timeout of 0 means wait forever on IO commands.}
  gpHaveAuxiliaryCommands := false;
  gpHaveDataBytes := false;
  gpBufPtr := 0;

  if  (TypePointDev = GPIBBitPad) { see if the GPIB bitpad is on }
  and (TabMode <> OffTablet) then begin
    gpTabMode := TabMode;         { remember the state we found the tablet in }
    IOSetModeTablet( OffTablet )  { turn the tablet off }
    end

end;

procedure gpAuxCommand(gpCmd: gpAuxiliaryCommands; gpParm:gpParmType);

var
  gpOffset: gpByte;

begin

  if gpHaveDataBytes then gpFlushBuffer;
  gpHaveAuxiliaryCommands := true;

  if gpParm = gpOn 
  then gpOffset := #200 
  else gpOffset := 0; 
  
  gpCommandBuffer^[gpBufPtr] := 3; {Auxiliary Command Register}
  gpCommandBuffer^[gpBufPtr+1] := ord(gpCmd) + gpOffset;
  gpBufPtr := gpBufPtr + 2;

  if gpBufPtr > gpBufMax then gpFlushBuffer;

end;

procedure gpPutByte(gpData: integer);

begin

  if gpHaveAuxiliaryCommands then gpFlushBuffer;
  gpHaveDataBytes := true;

  gpCommandBuffer^[gpBufPtr] := gpData;
  gpBufPtr := gpBufPtr + 1;

  if gpBufPtr > gpBufMax then gpFlushBuffer;

end;

procedure gpFlushBuffer;
begin
  if gpHaveAuxiliaryCommands 
  then GPB_UnitIO( Recast(gpCommandBuffer,IOBufPtr), IOWriteRegs
                 , gpBufPtr, recast(gpTimeOut, double), gpStatPtr)

  else if gpHaveDataBytes 
  then GPB_UnitIO( Recast(gpCommandBuffer,IOBufPtr), IOWrite
                 , gpBufPtr, recast(gpTimeOut, double), gpStatPtr)

  else gpStatPtr^.SoftStatus := IOEIOC;

  gpHaveAuxiliaryCommands := false; 
  gpHaveDataBytes := false;
  gpBufPtr := 0;

  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

end;

procedure gpITalkHeListens(gpAddr: gpDeviceAddress);
begin

  gpFlushBuffer;

  gpCommandBuffer^[0] := 0;       { Options byte }
                                  { Bytes 1 & 2 do not need to be set }
  gpCommandBuffer^[3] := gpAddr;  { Primary Address }
  gpCommandBuffer^[4] := 255;     { No Secondary Address }
  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IODevWrite
                 , 5, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

end;
 
procedure gpHeTalksIListen(gpAddr: gpDeviceAddress);

begin

  gpFlushBuffer;

  gpCommandBuffer^[0] := 0;       { Options byte }
                                  { Bytes 1 & 2 do not need to be set }
  gpCommandBuffer^[3] := gpAddr;  { Primary Address }
  gpCommandBuffer^[4] := 255;     { No Secondary Address }
  gpCommandBuffer^[5] := 0;       { Not waiting for data }
  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IODevRead
                 , 6, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

end;
 
procedure gpSend(var gpBuf: gppBuffer; gpCount: gpRange);
begin

  UnitIO(GpibOut
        , Recast(gpBuf,IOBufPtr)
        , IOWrite
        , gpCount
        , recast(gpTimeOut, double)
        , nil
        , gpStatPtr);

  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

end;
 
procedure gpReceive( var gpBuf: gppBuffer; gpCount: gpRange);

begin

end;
 
procedure gpTbltOff;
begin 

  gpITalkHeListens(#377);    { Talk/Listen to nobody }

end;

procedure gpTbltOn;
begin 
  
  gpHeTalksIListen(#10);     { Listen to BitPad }

end;
 
function  gpGetByte: gpByte;

{------------------------------------------------------------------------} 
{
{ Abstract
{      Return the next character sent to us via the GPIB.
{
{ Notes
{      To obtain the minimum turnaround time, we do direct StartIO's to 
{      the microcode.  We do not use the routines in IO_Unit for single 
{      character reads.  This saves us the overhead of many routine calls.
{
{------------------------------------------------------------------------}

var 
  Indicator : integer;     { indicates if a character is available }
  Results   : integer;     { the character and overrun indicater }
  StartTime: long;

begin
  
  StartTime := TimeBuf^;
  repeat
    loadexpr( GPIB );
    StartIO( EP_GetChar );   { tell ucode to give us a character }
    storexpr( Indicator );
    storexpr( Results );
    if (gpTimeOut > 0) and (Indicator = 0) then
       if (TimeBuf^ - StartTime) > gpTimeOut then raise GPIBerror( IOEIOB );
  until( Indicator <> 0 );       { loop till we have a character }

  if shift( Results, -8 ) = 255   { overrun? }
  then raise GPIBerror( IOEOVR );
  
  gpGetByte := land( Results, 255 );

end;

procedure gpCleanup;

begin

  if  (TypePointDev = GPIBBitPad)
  and (gpTabMode <> OffTablet)
  then IOSetModeTablet( gpTabMode )
  
end;
 
procedure gpFlushOutput;
begin

  gpFlushBuffer;

  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IOFlush
                 , 0, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

end;
 
procedure gpFlushInput;
begin

  gpFlushBuffer;

  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IOFlushIn
                 , 0, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

end;
 
procedure gpReset;
var i, Indicator, Results: integer;
begin

  gpBufPtr := 0;
  gpHaveAuxiliaryCommands := false;
  gpHaveDataBytes := false;
  gpTimeOut := 0;    {Timeout of 0 means wait forever on IO commands.}

  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IOReset
                 , 0, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

  gpCommandBuffer^[0] := 3;                  { Aux cmd reg }
  gpCommandBuffer^[1] := ord(gpsic) + #200;  { Send Interface Clear, on }
  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IOWriteRegs
                 , 2, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);

  for i := 1 to 100 do ;  { Delay at least 100 usec for IFC. }

  gpCommandBuffer^[0] := 3;                  { Aux cmd reg }
  gpCommandBuffer^[1] := ord(gpsic);         { Send Interface Clear, off }
  UnitIO(GPIB, Recast(gpCommandBuffer,IOBufPtr), IOWriteRegs
                 , 2, recast(gpTimeOut, double), nil, gpStatPtr);
  if gpStatPtr^.SoftStatus <> IOEIOC 
  then raise GPIBerror(gpStatPtr^.SoftStatus);
  
  repeat
    loadexpr( GPIB );
    StartIO( EP_GetChar );   { Tell ucode to give us a character. }
    storexpr( Indicator );
    storexpr( Results );
  until( Indicator = 0 );    { Loop till ring buffer is cleared. }

end.
