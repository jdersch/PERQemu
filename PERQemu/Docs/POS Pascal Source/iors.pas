{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IORS;
{--------------------------------------------------------------------------
{
{ IORS - RS IO routines.   
{
{ Abstract:
{       IORS exports procedures to perform IO on RS232 ports.
{       These routines are RS232 specific interrupt, initialization, and
{       general IO routines, and are exported to the IO subsystem modules.
{       ( IO_Unit, IO_Init )
{
{ Notes:
{       Speech is on an RS232 line, so all speech IO goes through this
{       module.
{
{ Copyright (C) 1982, 1983 Three Rivers Computer Corporation
{-------------------------------------------------------------------------}
             
{ $Version V1.3 for POS}
{--------------------------------------------------------------------------
{ Change Log:
{
{ 28 Oct 83  V1.3 Dirk Kalp
{            Added IOReadHiVol command for RSA.
{            Fix bug in WriteRegs cmd: Bytecnt must be < 13.
{
{ 27 Oct 83  V1.2 Dirk Kalp
{            Added IOFlushIn command for RSA and RSB. Also fix RS_ReadChar to
{            change its parameter only if has a character to return. Make
{            IORead return an error for Speech.
{
{ 25 Jul 83  V1.1 Sandeep Johar
{            Added an IOReset for RSB also.
{
{ 18 Apr 83  V1.0 Dave Anderson ICL
{            Changed to issue an IOReset during init as Z80 now comes up
{            with RSa turned off.
{
{  6 Apr 83  V0.9 August G. Reinig
{            Changed RS_WriteChar so that its critical section is non
{            interruptable.  Changed RS_PutStatus so that it assigns
{            to integers instead of bits were possible.  Gave RSA its
{            own interrupt routine (instead of having it cal interrupt
{            common).  Fixed some comments.
{
{  3 Mar 83  V0.8 Chuck Beckett
{            Exported private variables.  This will allow demos and other
{            time dependent stuff to access IORS directly.  (Caveat
{            hacker! Importing IORS is a really dumb thing to do
{            unless you have absolutely no other choice!
{
{  2 Mar 83  V0.7 Roger Riggs
{            Removed IOReset from RS_PutStatus, it was resetting
{            the baud rate to 9600 and is unnecessary.  Also added
{            Exit for Unit = Speech.
{
{  1 Mar 83  V0.6 Roger Riggs
{            Fixed <=3 to < 3 in PutStatus
{
{ 23 Feb 83  V0.5 Roger Riggs
{            Added Interpretation of status bits in RS_ReadChar
{            to return error codes for Parity and framing errors
{            Add IOFlush code returns IOEDNI if pipe to Z80 is NOT empty
{            otherwise return IOEIOC.
{            Also fixed RS_PutStatus to set only those parameters
{            specified by the length.
{
{ 06 Jan 83  V0.4 August G. Reinig
{            Removed RS_GetStatus, (it did nothing).  Changed Ch in 
{            RS_WriteChar from var to normal parameter.  Changed calls
{            to routines in IO_Private into the corresponding startio's 
{            to the microcode.
{
{ 05 Jan 83  V0.3 August G. Reinig
{            Fixed bug in initialization which caused IORS to think that
{            the Z80 wasn't recognizing commands to RSA and Speech.
{
{ 04 Jan 83  V0.2 August G. Reinig
{            Changed calls to SetDDS.
{
{ 14 Dec 82  V0.1 August G. Reinig
{            Modified to deal with Z80 not accepting commands.
{            Initialization routine can now be called more than once.
{
{ 22 Oct 82  V0.0 C.Beckett 
{            Created module.
{
{-------------------------------------------------------------------------}

{*******************************}   Exports   {*****************************}

Imports IO_Private from IO_Private;

var
  RSIntMask : integer;      { mask of interrupts to enable for RS232 }

procedure Rs_Initialize; 

Function  Rs_ReadChar (Unit : UnitRng ; var Ch: char ): integer;
Function  Rs_WriteChar(Unit : UnitRng ; Ch: char ): integer;

Procedure Rs_PutStatus(Unit : UnitRng ; var UserStatus : DevStatusBlock);

Procedure RsA_Interrupt;  { Interrupt handler for RS232 port 'A' }
Procedure RsB_Interrupt;  { Interrupt handler for RS232 port 'B' }
Procedure Spc_Interrupt;  { Interrupt handler for Speech }

Procedure RS_UnitIO (  Unit: UnitRng;
                       Bufr: IOBufPtr;
                       Command: IOCommands;
                       ByteCnt: integer;
                       StsPtr: IOStatPtr );  

type
    PortContext = record
      NakReceived : boolean; { We got a NAK on the last operation }
      Outstanding : boolean; { True: then I/O operation outstanding } 
      StatWaiting : boolean; { True: stat command outstanding }
      NotThere    : boolean; { True: Z80 won't accept commands to the device } 
      MsgWaiting  : boolean; { True: a message ready and waiting to be sent }
      WaitingMsg  : Z_Msg;   { The waiting message, used by WriteChar only }          RsQueue : Z_Queue;     { Micro-code queue for RS232 messages }
      HiVolPtr : pHiVolBlock;{ Pointer to high volume block for device }
      pStat : pRS232Stat;    { Pointer to the status information about device }
      end;
  
    ContextVector = record case integer of   { all this to avoid }
      1 : (RSAinfo : PortContext);           { repeated array evaluation }
      2 : (Info : array[RSA..Speech] of PortContext);
      end;
          
    DataArray = packed array[0..0] of Z_Data;
    pDataArray = ^DataArray;
    ByteArray = packed array[0..0] of 0..255; 
    pByteArray = ^ByteArray;
    ManyPtrs = packed record case integer of
      1 : (pBuffer : IOBufPtr);   { what we're sent }
      2 : (pByte   : pByteArray); { view as a list of bytes }
      3 : (pStat   : pRS232Stat); { view as a status buffer }
      4 : (pData   : pDataArray); { view as a list of data areas in Z_Msg's }  
      5 : (Offset  : integer;     { view as segment and offset }
           Segment : integer)
      end;
    
      
Var 
    Context : ContextVector; { Port-specific variables } 
    TempMsg : Z_MsgPtrKludge;
    MsgMask : integer;       { mask for those interrupts cause by Z80 msgs }
    OurMsg : Z_Msg;          { temporary holding place for messages to }
                             { Z80, avoids repeated pointer references }
{***************************}   Private   {*****************************}

{$R-}
Const 
  debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} imports System from System;
{$endc}

Imports IOErrors from IOErrors;
Imports Virtual from Virtual;
  

procedure RS_Initialize;
{---------------------------------------------------------------------------
{ Abstract:
{
{      RS232 initialization logic.
{---------------------------------------------------------------------------}

var { RS_Init }  
  pRS232Status : pRS232Stat;
  i : integer;
  StsPtr : IOStatPtr;
  Unit : UnitRng;

begin
  
  SetDDS(431);
  OurMsg.pNext := nil;            { put stuff that never changes into OurMsg }
  OurMsg.UCodeArea := 0;
  OurMsg.SOMDelimiter := Z_SOM;
  
  MsgMask := lor( shift( 1, Dev_AckReceived )      { interrupts due to msgs }
           , lor( shift( 1, Dev_NakReceived )
           ,      shift( 1, Dev_StatReceived ) ) );
  
  RSIntMask := lor( MsgMask                      { all possible interrupts }
             , lor( shift( 1, Dev_Attention )
             ,      shift( 1, Dev_DataAvailable ) ) );

  for Unit := RSA to Speech do { Intialize both entries in 'context' array }

  with Context.Info[Unit] do  
  with pUDevTab^[unit] do
    begin
      if Unit <> RSB then 
      if pDataCtrl = nil then begin
        SetDDS(432);
        new ( IOSegNum, 4, HiVolPtr );
        HiVolPtr^.DataByteCnt := 0;
        HiVolPtr^.pDataBuffer := nil;
        pDataCtrl := recast( HiVolPtr, pointer );
        end;
        
      if Unit <> Speech then
      if pCirbuf = nil then begin
        SetDDS(433);
        new( IOSegNum, 4, pCirBuf );
        pCirBuf^.Length := CirBufSize;
        pCirBuf^.RdPtr := 0;
        pCirBuf^.WrPtr := 0;
        end;
          
      if pStatus = nil then begin
        SetDDS(434);
        New( ioSegnum, 4, pStat ); { Allocate/connect a status area }
        pStatus := recast ( pStat, pointer );  
        end;

      RSQueue := Z_Q0; 
      Outstanding := false;
      NakReceived := false;
      StatWaiting := false;
      MsgWaiting := false;
      WaitingMsg := OurMsg;
      WaitingMsg.Device := Unit;
                    
      SetDDS(435);
      Loadexpr ( RSIntMask );           { Enable the RS232 by . . . }
      Loadexpr ( unit );
      StartIO (EP_SetEnableMask);
    end;
  
  SetDDS(436);
  new( pRS232Status );
  new( StsPtr );  
  SetDDS(437);
  for Unit := RSA to Speech do begin
    Context.Info[Unit].NotThere := false;
    RS_UnitIO( Unit, recast( pRS232Status, IOBufPtr ), IOSense, 0, StsPtr );
    Context.Info[Unit].NotThere := StsPtr^.SoftStatus <> IOEIOC;
    end;  
  SetDDS(438);
    RS_UnitIO( RSa, recast( pRS232Status, IOBufPtr ), IOReset, 0, StsPtr );
    If Not Context.Info[RSB].NotThere  Then
        RS_UnitIO( rsb, recast( pRS232Status, IOBufPtr ), IOReset, 0, StsPtr );
  SetDDS(439);
  dispose( pRS232Status );
  dispose( StsPtr );
   
end { RS_Init };

Function RS_ReadChar(Unit : UnitRng ; var Ch: char ): integer;
{---------------------------------------------------------------------------
{ Abstract:
{
{      Reads a character from an RS232 port and returns a completion
{      or error code.
{
{ Parameters:
{      Unit - The RS232 port on which read is done (RSA or RSB)
{      Ch - character read.
{
{ Returns:
{
{      A condition code as defined in the module IOErrors.
{---------------------------------------------------------------------------}

type
    CirEnt = packed record 
      case boolean of
      true:  ( Entry : integer);
      false: ( C : Char;
               Status : 0..255);
      end;

var results : CirEnt;
    CharIndicator : integer;

begin   

  LoadExpr( Unit );
  StartIO( EP_GetChar );
  StorExpr(CharIndicator);
  StorExpr(Results.Entry); 
  
  if CharIndicator = 0 then             { see if we got a character }
     RS_ReadChar := IOEIOB              { Circular buffer is empty } 
  else
     begin
     Ch := Results.C; 
     if Results.Status = 0 then         { check for any errors on character }
        RS_ReadChar := IOEIOC           { IO Complete }
     else
        begin
        if Results.Status = 255
        then RS_ReadChar := IOEOVR      { OverRun condition } 
        else if Land(Results.Status, #20) <> 0
        then RS_ReadChar := IOEPAR      { Parity error on character }
        else if Land(Results.Status, #40) <> 0
        then RS_ReadChar := IOEOVR      { Receiver overrun }
        else if Land(Results.Status, #100) <> 0
        then RS_ReadChar := IOEFRA      { Framing error on charcter }
        else if Land(Results.Status, #200) <> 0
        then RS_ReadChar := IOEEND      { End of SDLC frame }
        Else RS_ReadChar := IOEIOC;     { No error on character }
        end;
     end;

end { RS_ReadChar };

Function RS_WriteChar(Unit : UnitRng; Ch: char): integer;
{---------------------------------------------------------------------------
{ Abstract:
{
{      Writes a character to an RS232 port and returns a completion
{      or error code.
{
{ Parameters:
{
{      Unit - Port (A, Speech, or B) to read from.
{
{      Ch - character to write.
{
{ Returns:
{
{      A condition code as defined in the module IOErrors.
{---------------------------------------------------------------------------}
var 
  SaveMask, result, tempcount, holdseg, holdoff : integer;
  C : Char;

begin
  
  if Context.Info[Unit].NotThere then begin
    RS_WriteChar := IOEBUN;
    exit( RS_WriteChar );
    end;
 
  with Context.Info[unit] do begin
    if MsgWaiting         { a Z80 message waiting to go }
    then if WaitingMsg.ByteCount = Z_MaxData  { and it's full }
    then while MsgWaiting do;  { wait till it is sent }
                                
    inlinebyte( {INTOFF} 105 ); { no interrupts till we're done mucking }      
      
    if not MsgWaiting then begin   { if there is no message for the character }
      MsgWaiting := true;          { we've got a waiting message now }
      WaitingMsg.Command := ord( Z_SendData );  
      WaitingMsg.ByteCount := Z_NoData;
      end;
    
    TempCount := WaitingMsg.ByteCount + 1;  
    WaitingMsg.ByteCount := TempCount;
    WaitingMsg.Data[TempCount] := ord(Ch);

    if NOT OutStanding then begin { If no outstanding operation }
      OutStanding := true;        { then start up one. }  

      loadexpr( 0 );              { get a Z_Msg } 
      startio( EP_UcodeMsg );
      storexpr( TempMsg.Segment );
      storexpr( TempMsg.Offset );
      if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;
      
      TempMsg.pMsg^ := WaitingMsg; { get the message to send }
      MsgWaiting := false;         { it's not waiting anymore }
      
      loadexpr( RsQueue );         { send the message }
      {$ifc debug
      then}    loadadr( TempMsg.pMsg );
      {$elsec} loadadr( TempMsg.pMsg^ );
      {$endc}
      startio( EP_Z80Msg );
      end;                  

    if NakReceived              { NakReceived }
    then RS_WriteChar := IOEUDE 
    else RS_WriteChar := IOEIOC; { IO Complete } 
    NakReceived := false;
    inlinebyte( {INTON}  106 );
    end;

end { RS_WriteChar };

Procedure RS_PutStatus (Unit: UnitRng; var UserStatus:DevStatusBlock);
{---------------------------------------------------------------------------
{ Abstract:
{
{      Sets port's characteristics.  Translates old put status command
{      into a configure command and a writeregs command, mapping the
{      old status block into a set of Serial IO controller registers
{      and the baud rate for the configure command. 
{
{ Parameters:
{
{      Unit - device whose characteristics are to be set.
{      UserStatus - device status block containing characteristics
{                   to be set.
{ SideAffects:
{     
{      Sets SIO register settings not mapped from the old device status
{      block to their defaults.
{---------------------------------------------------------------------------}
var
  DefaultStat, Configit : pRS232Stat;
  pReturn: IOStatPtr;
  Len : Integer;

begin { RS_PutStatus }

  if Context.Info[Unit].NotThere
  or (Unit = Speech) then exit( RS_PutStatus) ;
  
  new ( pReturn );
  new ( DefaultStat );  { Now setup for the writeregs message }

  with DefaultStat^ do begin
    Word[1] := #000000; { write to register 0, setting : }
                        { NextRegisterPointer to 0 }
                        { Command to R_NullCommand }
                        { RsesetCRC to R_NullResetCRC }

    Word[2] := #020003; { write to register 3, setting : }
                        { SynCharLoadInhibit, AddressSearchMode, RxCRCEnable }
                        { and EnterHuntPhase to false }
                        { AutoEnables to true }
    Reg[2].RSRcvEnable := UserStatus.RSRcvEnable;
    if UserStatus.ByteCnt < 3     { If not specified set to 8 Bits }
    then Reg[2].RSRcvBits := RS_8
    else case UserStatus.RSRcvBits of  { Use bits Specified in status }
           Rcv5: Reg[2].RSRcvBits := RS_5;
           Rcv6: Reg[2].RSRcvBits := RS_6;
           Rcv7: Reg[2].RSRcvBits := RS_7;
           Rcv8: Reg[2].RSRcvBits := RS_8; 
           end; { case }

    len := 4;                     { Minimum is 4 bytes }

    if UserStatus.ByteCnt >=3 then begin
      Len := 8;                   { Up length to include the following }

      Word[3] := #040004;  { write to register 4, setting: }
                           { SyncMode to R_8BitSync }
                           { ClockRate to R_X16 }
      case UserStatus.RSParity of
        NoParity:    Reg[3].RSParity :=  RS_NoParity;
        OddParity:   Reg[3].RSParity :=  RS_OddParity;
        IllegParity: Reg[3].RSParity :=  RS_IllegParity;
        EvenParity:  Reg[3].RSParity :=  RS_EvenParity; 
        end; { case }
      case UserStatus.RSStopBits of
        Syncr:   Reg[3].RSStopBits := RS_Syncr;
        Stop1:   Reg[3].RSStopBits := RS_St1;
        Stop1x5: Reg[3].RSStopBits := RS_St1x5;
        Stop2:   Reg[3].RSStopBits := RS_St2;
        end; { case }

      Word[4] := #105005;  { write to register 5, setting; }
                           { TxCRCEnable, UseCRC16, SendBreak to false }
                           { RTS, TxEnable, DTR to true }
      case UserStatus.RSXmitBits of
        Send5: Reg[4].RSXmitBits :=  RS_Send5;
        Send7: Reg[4].RSXmitBits :=  RS_Send7;
        Send6: Reg[4].RSXmitBits :=  RS_Send6;
        Send8: Reg[4].RSXmitBits :=  RS_Send8;
        end; { case }
      end;

    end; { with DefaultStat^ }
  
  RS_UnitIO ( Unit, RECAST(DefaultStat, IOBufPtr), IOWriteRegs, len, pReturn );

  if UserStatus.ByteCnt > 1 { Set Baud rate only if included in byte count }
  then
    begin
    DefaultStat^.XmitRate := UserStatus.RSSpeed;  
    DefaultStat^.RcvRate := UserStatus.RSSpeed;  
    RS_UnitIO ( Unit, RECAST (DefaultStat, IOBufPtr), IOConfigure, 2,
                    pReturn ); { configure msg.}
    end;
  
  dispose ( DefaultStat );
  dispose ( pReturn );

end { RS_PutStatus };

Procedure RS_UnitIO (  Unit: UnitRng;
                       Bufr: IOBufPtr;
                       Command: IOCommands;
                       ByteCnt: integer;
                       StsPtr: IOStatPtr );
{---------------------------------------------------------------------------
{
{ Abstract:
{
{      Unit IO operations to RS232 ports.
{
{ Parameters:
{
{      Unit - the device.
{
{      Bufr - buffer for data transfers, if requested.
{
{      Command - operation to be performed on the port.
{
{      ByteCnt - number of bytes to be transferred.
{
{      StsPtr - resultant status from the operation.
{---------------------------------------------------------------------------}
 

var
  Users : ManyPtrs;  { lets us look at the user's data in many ways }

{ SUBORDINATE PROCEDURES:

  IOReset:  Do_Reset

  IOConfigure:  Do_Configure

  IOSense:  Do_Sense

  IORead:  Do_Read

  IOWrite:  Do_Write

  IOFlush:  Do_Flush

  IOFlushIn:  Do_FlushIn

  IOWriteHiVol:  Do_HiVolWrite

  IOWriteRegs:  Do_RegsWrite  }

procedure Error( ErrCode : integer );

{------------------------------------------------------------------}
{
{ Abstract
{       Something is wrong, set the SoftStatus and leave
{
{------------------------------------------------------------------}

begin
       
  Context.Info[Unit].NakReceived := false;     
  StsPtr^.SoftStatus := ErrCode;
  exit( RS_UnitIO )

end;

Procedure Do_HiVolWrite;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform hi volume writes to the RS232
{
{-----------------------------------------------------------------------------}

begin

  if Unit = RSB then Error( IOEILC );     { no hi vol to RSB }
  
  with Context.Info[unit] do begin
    if ByteCnt < 1 then Error( IOEBSE );  { validate the byte count } 

    if ByteCnt = 1 then begin             { single chararacter can't go DMA }
      StsPtr^.SoftStatus := RS_WriteChar (Unit, chr(Users.pByte^[0])); 
      exit (do_hivolwrite);       
      end;

    if LAnd(Users.Offset, 3) <> 0 then Error( IOEBAE ); { quad word buffer? }
  
    IncIOCount( Users.Segment );             { lock buffer into core for }

    HiVolPtr^.DataByteCnt := ByteCnt;        { high  volume transfer }
    HiVolPtr^.pDataBuffer := Users.pBuffer;  
  
    OurMsg.Device := unit;           { compose a high volume message }
    OurMsg.bytecount := Z_FirstData + 1;
    OurMsg.Command := ord ( Z_OutHiVolumeStart );  
    OurMsg.Data[Z_FirstData] := land(ByteCnt, 255);
    OurMsg.Data[Z_FirstData+1] := shift(ByteCnt, -8);

    loadexpr( 0 );                   { get a message to send }
    startio ( EP_UcodeMsg );
    storexpr( TempMsg.Segment );
    storexpr( TempMsg.Offset );
    if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;
    
    TempMsg.pMsg^ := OurMsg;         { prepare to send it }
  
    OutStanding := true;                
    loadexpr( RsQueue );             { send the message }
    {$ifc debug
    then}    loadadr( TempMsg.pMsg );
    {$elsec} loadadr( TempMsg.pMsg^ );
    {$endc}
    startio( EP_Z80Msg );
    
    repeat until NOT OutStanding;    { wait for it to be sent }
    DecIOCount( Users.Segment );     { free the buffer }
    
    StsPtr^.BytesTransferred := ByteCnt - HiVolPtr^.DataByteCnt; 
    
    if NakReceived then Error( IOEDNI );
    end; 

end; { Do_HiVolWrite }

Procedure Do_HiVolRead;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform hi volume read from the RS232
{
{-----------------------------------------------------------------------------}
var Ch: char;

begin

  if (Unit = RSB) or (Unit = Speech) then Error( IOEILC ); { Only on RSA }
  
  with Context.Info[unit] do begin
    if ByteCnt < 1 then Error( IOEBSE );  { validate the byte count } 

    if ByteCnt = 1 then begin             { single chararacter can't go DMA }
      StsPtr^.SoftStatus := RS_ReadChar(Unit, Ch); 
      if StsPtr^.Softstatus <> IOEIOB then Users.pByte^[0] := ord(Ch);
      exit (do_hivolread);       
      end;

    if LAnd(Users.Offset, 3) <> 0 then Error( IOEBAE ); { quad word buffer? }
  
    IncIOCount( Users.Segment );             { lock buffer into core for }

    HiVolPtr^.DataByteCnt := ByteCnt;        { high  volume transfer }
    HiVolPtr^.pDataBuffer := Users.pBuffer;  
  
    OurMsg.Device := unit;           { compose a high volume message }
    OurMsg.bytecount := Z_FirstData + 1;
    OurMsg.Command := ord ( Z_InHiVolumeStart );  
    OurMsg.Data[Z_FirstData] := land(ByteCnt, 255);
    OurMsg.Data[Z_FirstData+1] := shift(ByteCnt, -8);

    loadexpr( 0 );                   { get a message to send }
    startio ( EP_UcodeMsg );
    storexpr( TempMsg.Segment );
    storexpr( TempMsg.Offset );
    if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;
    
    TempMsg.pMsg^ := OurMsg;         { prepare to send it }
  
    OutStanding := true;                
    loadexpr( RsQueue );             { send the message }
    {$ifc debug
    then}    loadadr( TempMsg.pMsg );
    {$elsec} loadadr( TempMsg.pMsg^ );
    {$endc}
    startio( EP_Z80Msg );
    
    repeat until NOT OutStanding;    { wait for it to be sent }
    DecIOCount( Users.Segment );     { free the buffer }
    
    StsPtr^.BytesTransferred := ByteCnt - HiVolPtr^.DataByteCnt; 
    
    if NakReceived then Error( IOEDNI );
    end; 

end; { Do_HiVolRead }

Procedure Do_Write;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO write operations for RS232.
{-----------------------------------------------------------------------------}
var 
    BufrIndx,        { index to users buffer data }
    ReqCnt : integer;      { local count of bytes still not sent }
    MsgNum : integer;

begin with Context.Info[unit] do begin

  if ByteCnt < 1 then Error( IOEBSE );

  ReqCnt := ByteCnt;
  OurMsg.Device := unit;                { all messages will be to this unit }
  OurMsg.Command := ord ( Z_SendData ); { and of this type }
  
  MsgNum := -1;
  while ReqCnt > 0 do begin  
    MsgNum := MsgNum + 1;

    if ReqCnt > Z_DataSize 
    then Begin            { Too much to send in one msg }
      OurMsg.ByteCount := Z_MaxData; 
      ReqCnt := ReqCnt - Z_DataSize;
      end
    else Begin            { Use one msg: this will be last(or only) msg sent }
      OurMsg.ByteCount := ReqCnt + Z_NoData;
      ReqCnt := 0;
      end;
    
    { move a messages worth of data, (somewhat kludgy but effective and fast)} 
    {$R-} OurMsg.Data := Users.pData^[MsgNum]; {$R=} 

    while Outstanding do;     { wait for any previous messages }
    if NakReceived then begin { see if last message NAKed } 
      StsPtr^.BytesTransferred := ByteCnt - ReqCnt;
      Error( IOEUDE )
      end;
    
    loadexpr( 0 );
    startio ( EP_UcodeMsg );     { get a message to the Z80 }
    storexpr( TempMsg.Segment );
    storexpr( TempMsg.Offset );
    if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;

    TempMsg.pMsg^ := OurMsg;          { put the information into the message }

    OutStanding := true;                
    loadexpr( RsQueue );             { send the message }
    {$ifc debug
    then}    loadadr( TempMsg.pMsg );
    {$elsec} loadadr( TempMsg.pMsg^ );
    {$endc}
    startio( EP_Z80Msg );
    end; { while ReqCnt > 0}
   
  while Outstanding do;     { wait for any previous messages }
  StsPtr^.BytesTransferred := ByteCnt - ReqCnt;
  if NakReceived then Error( IOEUDE );
  end;
end; { Do_Write }

Procedure Do_Read;

{-----------------------------------------------------------------------------
{
{ Abstract:
{       Read characters from the circular buffer until no more
{       characters are left, there is an error, or we've filled
{       the caller's buffer.
{
{----------------------------------------------------------------------------}

label 1;

type
    CirEnt = packed record 
      case boolean of
      true:  ( Entry : integer);
      false: ( C : 0..255;
               Status : 0..255);
      end;

var results : CirEnt;
    CharIndicator : integer;
    i : integer;

begin   

  if Unit = Speech then Error( IOEILC );     { No Read to Speech }

  for i := 0 to ByteCnt - 1 do begin
    LoadExpr( Unit );
    StartIO( EP_GetChar );
    StorExpr(CharIndicator);
    StorExpr(Results.Entry); 
    
    Users.pByte^[i] := Results.C; 
    
    if CharIndicator = 0 then begin   { see if we got a character }
      StsPtr^.SoftStatus := IOEIOB;   { Circular buffer is empty } 
      StsPtr^.BytesTransferred := i;
      exit( Do_Read );
      end;

    if Results.Status <> 0 then begin { we may have an error }
      StsPtr^.BytesTransferred := i + 1;
           if Results.Status = 255 
      then StsPtr^.SoftStatus := IOEOVR      { OverRun } 
      else if Land(Results.Status, #20) <> 0
      then StsPtr^.SoftStatus := IOEPAR      { Parity error on character }
      else if Land(Results.Status, #40) <> 0
      then StsPtr^.SoftStatus := IOEOVR      { Receiver overrun }
      else if Land(Results.Status, #100) <> 0
      then StsPtr^.SoftStatus := IOEFRA      { Framing error on charcter }
      else if Land(Results.Status, #200) <> 0
      then StsPtr^.SoftStatus := IOEEND      { End of SDLC frame }
      else goto 1;     { No error on character }
      exit( Do_Read );
  1: { no error }
      end;
  end;
 
  StsPtr^.SoftStatus := IOEIOC;  { we've got all the characters }
  StsPtr^.BytesTransferred := ByteCnt;
 
end; { Do_Read }


Procedure Do_Reset;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO Reset operations for RS232.
{
{-----------------------------------------------------------------------------}

begin with context.Info[unit] do begin

  inlinebyte( {INTOFF} 105 );
  MsgWaiting := false;    { squash any waiting messages }
  OutStanding := false;
  NakReceived := false;
  inlinebyte( {INTON} 106 );

  OurMsg.Device := Unit; 
  OurMsg.ByteCount := Z_NoData;
  OurMsg.Command := ord( Z_Reset );
  
  loadexpr( 0 );
  startio ( EP_UcodeMsg );     { get a message to the Z80 }
  storexpr( TempMsg.Segment );
  storexpr( TempMsg.Offset );
  if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;

  TempMsg.pMsg^ := OurMsg;          { put the information into the message }

  OutStanding := true;                
  loadexpr( RsQueue );             { send the message }
  {$ifc debug
  then}    loadadr( TempMsg.pMsg );
  {$elsec} loadadr( TempMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
  
  while OutStanding do;
  if NakReceived then Error( IOEUDE );
  end;
end; { Do_Reset }

Procedure Do_Configure;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO Configure operations for RS232.
{-----------------------------------------------------------------------------}

begin with Context.Info[unit] do begin  

  if Unit <> Speech 
  then with Users.pStat^ do          
    if (XmitRate < RSExt) or (XmitRate > RS19200) 
    or (RcvRate  < RSExt) or (RcvRate  > RS19200) then Error( IOECDI );

  OurMsg.Device := Unit;
  OurMsg.ByteCount := Z_FirstData+1;
  OurMsg.Command := ord( Z_Config );
  OurMsg.Data[Z_FirstData] := Users.pStat^.XmitRate;
  OurMsg.Data[Z_FirstData+1] := Users.pStat^.RcvRate;

  loadexpr( 0 );
  startio ( EP_UcodeMsg );
  storexpr( TempMsg.Segment );
  storexpr( TempMsg.Offset );
  if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;

  TempMsg.pMsg^ := OurMsg;

  OutStanding := true;                
  loadexpr( RsQueue );             { send the message }
  {$ifc debug
  then}    loadadr( TempMsg.pMsg );
  {$elsec} loadadr( TempMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );

  while OutStanding do;
  if NakReceived then Error( IOEDNI );
  end;
end; { Do_Configure }

Procedure Do_RegsWrite;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO WriteRegs operations for RS232.
{-----------------------------------------------------------------------------}

var 
   WRIndx,MsgIndx : integer;
   RegPair : RS_WrtReg;

begin with context.Info[unit] do begin

  if odd( ByteCnt )  { byte count must be even }
  or (ByteCnt < 0)   { it must be non negative }
  or (ByteCnt > 12)  { permit no more than 6 registers to be written }
  then Error( IOEBSE );

  OurMsg.Device := unit;         { this is a write regs message }
  OurMsg.Bytecount := Z_NoData + ByteCnt;
  OurMsg.Command := ord ( Z_WriteRegisters );

  MsgIndx := Z_FirstData;
  for WrIndx := 1 to shift( ByteCnt, -1 ) do begin  { for each pair }
    RegPair := Users.pStat^.Reg[WrIndx];     { grab a local copy }
    if (RegPair.ID = 1)                  { check register range }
    or (RegPair.ID = 2)
    or (RegPair.ID > 7)
    then Error( IOERDI )                     { leave if bad }
    else begin
      OurMsg.Data[MsgIndx] := RegPair.ID;
      OurMsg.Data[MsgIndx + 1] := RegPair.Sync1; 
      MsgIndx := MsgIndx + 2;
      end;
    end;
        
  StsPtr^.BytesTransferred := ByteCnt;
  
  OutStanding := true;
        
  loadexpr( 0 );
  startio ( EP_UcodeMsg );      { get a sendable message }
  storexpr( TempMsg.Segment );
  storexpr( TempMsg.Offset );
  if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;

  TempMsg.pMsg^ := OurMsg;        { put data into it }

  OutStanding := true;                
  loadexpr( RsQueue );             { send the message }
  {$ifc debug
  then}    loadadr( TempMsg.pMsg );
  {$elsec} loadadr( TempMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
  
  while OutStanding do;
  if NakReceived then Error( IOEDNI );
  end;
end; { Do_RegsWrite }

Procedure Do_Sense;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO Sense operations for RS232.
{-----------------------------------------------------------------------------}

begin with Context.Info[unit] do begin

  OurMsg.Device := unit;
  OurMsg.Bytecount := Z_NoData;
  OurMsg.Command := ord ( Z_Sense );
        
  loadexpr( 0 );
  startio ( EP_UcodeMsg );
  storexpr( TempMsg.Segment );
  storexpr( TempMsg.Offset );
  if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;
         
  TempMsg.pMsg^ := OurMsg;

  StatWaiting := true; 
          
  loadexpr( RsQueue );             { send the message }
  {$ifc debug
  then}    loadadr( TempMsg.pMsg );
  {$elsec} loadadr( TempMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
   
  while StatWaiting and not NakReceived do
  
  Users.pStat^ := pStat^;
  if NakReceived then begin 
    StatWaiting := false;
    Error( IOEUDE );
    end;   
  end;     
end; { Do_Sense }

Procedure Do_Flush;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO Flush operations for RS232.
{-----------------------------------------------------------------------------}

begin with Context.Info[unit] do
  begin
  OurMsg.Device := unit;
  OurMsg.Bytecount := Z_NoData;
  OurMsg.Command := ord ( Z_Flush );
        
  loadexpr( 0 );
  startio ( EP_UcodeMsg );
  storexpr( TempMsg.Segment );
  storexpr( TempMsg.Offset );
  if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;
         
  TempMsg.pMsg^ := OurMsg;

  OutStanding := true; 
          
  loadexpr( RsQueue );             { send the message }
  {$ifc debug
  then}    loadadr( TempMsg.pMsg );
  {$elsec} loadadr( TempMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
   
  while OutStanding do;
  if NakReceived then Error( IOEDNI );
  end;

end; { Do_Flush }

Procedure Do_FlushIn;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Perform unit IO FlushIn operations for RS232.
{-----------------------------------------------------------------------------}

begin

  if Unit = Speech then Error( IOEILC );     { No FlushIn to Speech }

  with Context.Info[unit] do
  begin
  OurMsg.Device := unit;
  OurMsg.Bytecount := Z_NoData;
  OurMsg.Command := ord ( Z_FlushIn );
        
  loadexpr( 0 );
  startio ( EP_UcodeMsg );
  storexpr( TempMsg.Segment );
  storexpr( TempMsg.Offset );
  if TempMsg.pMsg = nil then TempMsg.pMsg := Z_DqSysMsg;
         
  TempMsg.pMsg^ := OurMsg;

  OutStanding := true; 
          
  loadexpr( RsQueue );             { send the message }
  {$ifc debug
  then}    loadadr( TempMsg.pMsg );
  {$elsec} loadadr( TempMsg.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
   
  while OutStanding do;
  if NakReceived then Error( IOEDNI );
  end;

end; { Do_FlushIn }

begin with Context.Info[Unit] do begin { UNIT IO MAINLINE: RS_UnitIO }

  if NotThere then Error( IOEBUN );

  if OutStanding
  then if Command <> IOReset
  then if Command <> IOSense
  then begin
    while OutStanding do;
    if NakReceived then Error( IOEUDE );
    end;
   
  StsPtr^.SoftStatus := IOEIOC;

  Users.pBuffer := Bufr;

  case Command of  
    IOConfigure  : Do_Configure;
    IOFlush      : Do_Flush; 
    IOFlushIn    : Do_FlushIn; 
    IORead       : Do_Read;
    IOReset      : Do_Reset;
    IOSense      : Do_Sense;
    IOWriteRegs  : Do_RegsWrite;  
    IOWrite      : Do_Write;
    IOWriteHiVol : Do_HiVolWrite;
    IOReadHiVol  : Do_HiVolRead;
    otherwise    :  StsPtr^.SoftStatus := IOEILC; 
    end;
  end;                    

end { RS_UnitIO };

Procedure Intr_Common (unit : UnitRng);

{-----------------------------------------------------------------------------
{
{ Abstract:
{       Intr_Common processes all RS232 interrupts.
{
{-----------------------------------------------------------------------------}

var 
  IntrCause : integer; 
  AtnCause  : integer;

procedure RaiseIt( WhichException : IOIntrTypes );

begin

 loadexpr( MsgMask );          { since we're about to raise an exception }
 loadexpr( Unit );             { allow only those gpib interrupts due to }
 startio( EP_SetEnableMask );  { messages sent to the Z80 }  
                               { raising an exception while the user is }
                               { processing the first can cause headaches }
 
 inlinebyte( {INTON} 106 );    { enable interrupts }
 
 raise DevInterrupt( Unit, WhichException, ATNCause );
 
 inlinebyte( {INTOFF} 105 );   { turn interrupts off }
 
 loadexpr( RSIntMask );        { allow all type of interrupts once again }
 loadexpr( Unit );             { if attentions have occurred while we were }
 startio( EP_SetEnableMask );  { out, we will get them }

end;

var
  IntMsg : Z_MsgPtrKludge;

begin with Context.Info[Unit] do begin

  LoadExpr (Unit);
  StartIO  (EP_ReadCause);
  StorExpr ( AtnCause );
  StorExpr ( Intrcause );
  
  if recast( rotate(IntrCause,Dev_NakReceived), boolean ) then begin { Nak? }
    OutStanding := false;
    NakReceived := true;
    MsgWaiting := false;
    end;

  if recast( rotate(IntrCause,Dev_StatReceived), boolean )           { Stat? }
  then StatWaiting := false;
       
  if recast( rotate(IntrCause,Dev_AckReceived), boolean )            { Ack? } 
  then if not MsgWaiting
       or NakReceived
       then OutStanding := false
       else begin
         MsgWaiting := false;
         OutStanding := true;
 
         loadexpr( 0 );
         startio ( EP_UcodeMsg );
         storexpr( IntMsg.Segment );
         storexpr( IntMsg.Offset );
         if IntMsg.pMsg = nil then IntMsg.pMsg := Z_DqSysMsg;
         
         IntMsg.pMsg^ := WaitingMsg;
         
         loadexpr( RsQueue );             { send the message }
         {$ifc debug
         then}    loadadr( IntMsg.pMsg );
         {$elsec} loadadr( IntMsg.pMsg^ );
         {$endc}
         startio( EP_Z80Msg );
         end;    
    
  if recast( rotate( IntrCause, Dev_Attention ), boolean )
  then if RaiseException[unit].Attention 
  then RaiseIt( IOATNInterrupt );
    
  if recast( rotate( IntrCause, Dev_DataAvailable ), boolean )
  then if RaiseException[unit].DataAvailable 
  then RaiseIt( IODataInterrupt );

  InLineByte( {INTON} 106 )  { enable further interrupts }
  end;  { with }

end;{ Intr_Common }

Procedure RSA_Interrupt;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       This is the interrupt routine for the RS232 port 'A'.
{-----------------------------------------------------------------------------}

var 
  IntrCause : integer; 
  AtnCause  : integer;

procedure RaiseIt( WhichException : IOIntrTypes );

begin

 loadexpr( MsgMask );          { since we're about to raise an exception }
 loadexpr( RSA );             { allow only those gpib interrupts due to }
 startio( EP_SetEnableMask );  { messages sent to the Z80 }  
                               { raising an exception while the user is }
                               { processing the first can cause headaches }
 
 inlinebyte( {INTON} 106 );    { enable interrupts }
 
 raise DevInterrupt( RSA, WhichException, ATNCause );
 
 inlinebyte( {INTOFF} 105 );   { turn interrupts off }
 
 loadexpr( RSIntMask );        { allow all type of interrupts once again }
 loadexpr( RSA );             { if attentions have occurred while we were }
 startio( EP_SetEnableMask );  { out, we will get them }

end;

var
  IntMsg : Z_MsgPtrKludge;

begin;

  LoadExpr ( RSA );
  StartIO  ( EP_ReadCause );
  StorExpr ( AtnCause );
  StorExpr ( Intrcause );
  
  if recast( rotate(IntrCause,Dev_NakReceived), boolean ) then begin { Nak? }
    Context.RSAInfo.OutStanding := false;
    Context.RSAInfo.NakReceived := true;
    Context.RSAInfo.MsgWaiting := false;
    end;

  if recast( rotate(IntrCause,Dev_StatReceived), boolean )           { Stat? }
  then Context.RSAInfo.StatWaiting := false;
       
  if recast( rotate(IntrCause,Dev_AckReceived), boolean )            { Ack? } 
  then if not Context.RSAInfo.MsgWaiting
       or Context.RSAInfo.NakReceived
       then Context.RSAInfo.OutStanding := false
       else begin
         Context.RSAInfo.MsgWaiting := false;
         Context.RSAInfo.OutStanding := true;
 
         loadexpr( 0 );
         startio ( EP_UcodeMsg );
         storexpr( IntMsg.Segment );
         storexpr( IntMsg.Offset );
         if IntMsg.pMsg = nil then IntMsg.pMsg := Z_DqSysMsg;
         
         IntMsg.pMsg^ := Context.RSAInfo.WaitingMsg;
         
         loadexpr( Context.RSAInfo.RsQueue );    { send the message }
         {$ifc debug
         then}    loadadr( IntMsg.pMsg );
         {$elsec} loadadr( IntMsg.pMsg^ );
         {$endc}
         startio( EP_Z80Msg );
         end;    
    
  if recast( rotate( IntrCause, Dev_Attention ), boolean )
  then if RaiseException[RSA].Attention 
  then RaiseIt( IOATNInterrupt );
    
  if recast( rotate( IntrCause, Dev_DataAvailable ), boolean )
  then if RaiseException[RSA].DataAvailable 
  then RaiseIt( IODataInterrupt );

  InLineByte( {INTON} 106 )  { enable further interrupts }

end;{ RSA_Interrupt }

Procedure Spc_Interrupt;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       This is the interrupt routine for the Speech port.
{-----------------------------------------------------------------------------}
begin
  Intr_Common( Speech );
end;{ Spc_Interrupt }


Procedure RSB_Interrupt;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       This is the interrupt routine for the RS232 port 'B'.
{-----------------------------------------------------------------------------}
begin
  Intr_Common ( RSB );
end.{ RS_BInterrupt }
