{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOGPIB;

{--------------------------------------------------------------------------
{
{ IOGPIB - 'Private' GPIB type and variable declarations - available
{          to the IO subsystem.  GPIB routines.
{ 
{ Copyright (C) 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       IOGPIB exports variables, constants, and procedures the IO subsystem
{       uses to do GPIB IO.
{
{-------------------------------------------------------------------------}






{ $Version V0.11 for POS}
{-------------------------------------------------------------------------}
{ Change Log:
{
{ 15 Nov 83  V0.11 Dirk Kalp
{            Add support for the ForceEOI option for IODevWrite command.
{
{ 27 Oct 83  V0.10 Dirk Kalp
{            Added IOFlushIn, IODevRead, and IODevWrite commands.
{            Fixed GPB_ReadChar to assign to its parameter only if it
{            actually has a character to return.
{
{ 29 Apr 83  V0.9  Dirk Kalp
{            Permit IOWriteRegs to reg 2 in GPB_UnitIO for use with EIO
{            GPIB Z80 code. (This will be a nop for CIO).
{
{ 28 Apr 83  V0.8  Sandeep Johar
{            Moved the Rangechecking off compiler directive into the private
{            part.
{
{  4 Apr 83  V0.7  Dirk Kalp
{            Fixed bug in GPB_GetStatus. IntStat0 had been excluded.
{
{ 22 Feb 83  V0.6  Roger Riggs
{            Added IOFlush command, returns IOEDNI if pipeline through
{            Z80 is NOT empty, otherwise return IOEIOC.  Added Status returns
{            to GPB_ReadChar, Return IOEEND if EOI on character.
{            Fixed documentation.  Fixed DecIOCount on wrong segment.
{
{  1 Feb 83  V0.5  Sandeep Johar
{            Make the GPIB unitio routines return IOEDNS when the device
{            is not supported and IOEDNR when it is not ready. If this 
{            works GPIB should never hang. Put 1983 in copyright notice.
{
{ 12 Jan 83  V0.4  August G. Reinig
{            Changed GPB_UnitIO to include a timeout count.
{            Changed way GPB_UnitIO sends messages to the Z80 so that it
{            does more error checking.
{
{ 06 Jan 82  V0.3  August G. Reinig
{            Fixed bug in GPB_UnitIO which propagated a Nak from one message
{            to all all future messages.  (Effectively shutting down the GPIB)
{
{ 04 Jan 82  V0.2  August G. Reinig
{            Changed calls to SetDDS.
{
{ 14 Dec 82  V0.1  August G. Reinig
{            Changed code to deal with possiblity of the Z80 not accepting
{            GPIB commands.  Initialization routine can now be called more
{            than once.
{            
{ 27 Oct 82  V0.0  August G. Reinig
{            Created module.
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}
{$R-}

imports IO_Unit from IO_Unit;
  
const  { Fudge factors for BitPad }
  GPIBxFudge =   38;  { actual range in X and Y for BitPad: 0..2200 }
  GPIByFudge = 1061;  { of TabABsX  : 0..1100 }
                      { of TabAbsY  : 0..1100 }
                      { of TabRelX  : -38..1062  limited to 0..767 }
                      { of TabRelY  : 1061..-39  limited to 1023..0 }
  
{ Update tells us if the puck was lifted off the pad.  Our interrupt routine
  clears it everytime it gets data from the gpib.  The tablet update routine
  adds one to it everytime it updates the tablet values.  If the tablet 
  update routine finds that we haven't set it to zero after a couple of
  times, it assumes the puck is off the bad.  Tablet update is in IOVideo. }
 
var
  GPIBTabBuf : packed record
    X : integer;
    Y : integer;
    Buttons : integer;
    Update : integer  
    end;
  GPIBTabletState : integer;            
  GPIBIntMask : integer;     { the interrupt mask for the gpib }

procedure GPB_Interrupt;
procedure GPB_Initialize;
function  GPB_ReadChar( var Ch : char ): integer;
function  GPB_WriteChar( var Ch : char ): integer;
procedure GPB_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer;
                      LogAdr  : Double;
                      StsPtr  : IOStatPtr );
procedure GPB_GetStatus( var StatBlk : DevStatusBlock );

{***************************}   Private   {*****************************}

Const 
  debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} imports System from System;
{$endc}

imports Virtual from Virtual;
imports IO_Private from IO_Private;
imports IOVideo from IOVideo;

const
  AuxCmd = #003;

type
  SimBufr = packed array[0..7] of integer;  { large enough for an IOSense }
  pSimBufr = ^SimBufr;
   
var 
  NotThere     : boolean;
  NakReceived  : boolean;  
  StatReceived : boolean;
  Z80Replied   : boolean;
  OurBufr      : pSimBufr;
  OurStat      : IOStatPtr;
  OurMsg       : Z_Msg;
  yTablet      : integer;
  xTablet      : integer;
  pGPIBSts     : pGPIBStat;     { points to status info in device table }
  pGPIBHiVol   : pHiVolBlock;   { points to high volume block }    
  MsgMask      : integer; { mask for determining if Z80 received msg }

procedure GPB_Interrupt;

{-----------------------------------------------------------------------------
{
{ Abstract:
{       GPB_Interrupt handles an interrupt from the GPIB.
{
{-----------------------------------------------------------------------------}

var
  IntrCause : integer;
  ATNCause  : integer;
  C : char;
  Indicator : integer;
  Results   : integer;
  

procedure RaiseIt( WhichException : IOIntrTypes );

{$R-}
begin

 loadexpr( MsgMask );          { since we're about to raise an exception }
 loadexpr( GPIB );             { allow only those gpib interrupts due to }
 startio( EP_SetEnableMask );  { messages sent to the Z80 }  
                               { raising an exception while the user is }
                               { processing the first can cause headaches }
 
 inlinebyte( {INTON} 106 );    { enable interrupts }
 
 raise DevInterrupt( GPIB, WhichException, ATNCause );
 
 inlinebyte( {INTOFF} 105 );   { turn interrupts off }
 
 loadexpr( GPIBIntMask );      { allow all type of interrupts once again }
 loadexpr( GPIB );             { if attentions have occurred while we were }
 startio( EP_SetEnableMask );  { out, we will get them }

end;

begin
  
  loadexpr( GPIB );
  startio( EP_ReadCause );      { why the interrupt? }
  storexpr( ATNCause );
  storexpr( IntrCause );        
    
  if land( IntrCause, MsgMask ) <> 0 then Z80Replied := true;
  
  if recast( rotate( IntrCause, Dev_NakReceived ), boolean ) { check nak bit }
  then NakReceived := true;
  
  if recast( rotate( IntrCause, Dev_StatReceived ), boolean )
  then StatReceived := true;

  if recast( rotate( IntrCause, Dev_Attention ), boolean ) { check ATN bit }
  then if RaiseException[GPIB].Attention then RaiseIt( IOATNInterrupt );
 
  if recast( rotate( IntrCause, Dev_DataAvailable ), boolean ) {data bit set?}
  then begin
    if  RaiseException[GPIB].DataAvailable then RaiseIt( IODataInterrupt );

    if  (TypePointDev = GPIBBitPad)
    and (TabMode <> OffTablet) then begin 
        
      loadexpr( GPIB );
      startio( EP_GetChar );
      storexpr( Indicator );
      storexpr( Results );
      C := chr( land( Results, 255 ) );     { get the character }
      
      while Indicator <> 0 do  begin        { if a character there }
        if shift( Results, -8 ) = 255       { check for overrun }
        then GPIBTabletState := 0         
        
        else if land( Results, #177 ) = #012  { LF - end of message }
        then begin 
          GPIBTabletState := 1;    { next characters will be X coordinates.}
          xTablet := 0;
          yTablet := 0
          end
                                    
        else case GPIBTabletState of   
          0 : ;{ we're out of sync, ignore the character }
          
          1 : { processing X coordinates }
            if  (C >= '0') and (C <= '9')
            then xTablet := xTablet * 10 + ord( C ) - ord( '0' )
            else GPIBTabletState := 2;  
            
          2 : { processing Y coordinates }
            if (C >= '0') and (C <= '9')
            then yTablet := yTablet * 10 + ord( C ) - ord( '0' )
            else GPIBTabletState := 3; 
                   
          3 : { got button, update bitpad information }
            with GPIBTabBuf do begin       
              X := xTablet;
              Y := yTablet;
              Buttons := ord(C);
              Update := 0;
              GPIBTabletState := 0
              end
          end;
        
        loadexpr( GPIB );
        startio( EP_GetChar );         { get another character }
        storexpr( Indicator );
        storexpr( Results );
        C := chr( land( Results, 255 ) )
        end 
      end
    end;
  
  inlinebyte( {INTON} 106 )

end;
{$R=}

procedure GPB_Initialize;

{-----------------------------------------------------------------------------
{
{ Abstract
{       Initialize the GPIB.
{
{-----------------------------------------------------------------------------}

var
  TimeOutCnt : long;



procedure SetUpGPIB;

{-----------------------------------------------------------------------------
{
{ Abstract:
{      Set up the GPIB and see if the bitpad is connected, when we leave,
{      the GPIB will be idle (no talkers and no listeners)
{
{-----------------------------------------------------------------------------}

type
  RegBuffer = packed array[0..9] of 0..255;
 
  
var                    
  StatPtr : IOStatPtr;
  RegPtr : ^RegBuffer;
  i : integer;
  
procedure DoUnitIO( Command : IOCommands; ByteCnt : integer );

begin
  
  GPB_UnitIO( recast( RegPtr, IOBufPtr )
            , Command
            , ByteCnt
            , recast( TimeOutCnt, double )
            , StatPtr );    
      
  If StatPtr^.SoftStatus <> IOEIOC Then GPIBPadConnected := False;

end;

begin
  
  SetDDS(420);  
  new( StatPtr );  
  new( RegPtr );
  
  GPIBPadConnected := true;                 { assume we have a bit pad }

  SetDDS(421);                              { wake up the bus }
  RegPtr^[0] := AuxCmd; RegPtr^[1] := #217; { SIC enable interface clear }
  DoUnitIO( IOWriteRegs, 2 );
  for i := 1 to 100 do;                     { wait at least 100usec }
  
  SetDDS(422);                              
  RegPtr^[0] := AuxCmd; RegPtr^[1] := #017; { SIC, disable interface clear }  
  RegPtr^[2] := AuxCmd; RegPtr^[3] := #014; { TCA, take control asynchronously}
  RegPtr^[4] := AuxCmd; RegPtr^[5] := #012; { TON, talk only off }
  RegPtr^[6] := AuxCmd; RegPtr^[7] := #011; { LON, listen only on }
  DoUnitIO( IOWriteRegs, 8 );
                          
  SetDDS(423);                            
  RegPtr^[0] := #077;                       { UNL, unlisten }
  RegPtr^[1] := #137;                       { UNT, untalk }
  RegPtr^[2] := #110;                       { tell bit pad to talk }
  DoUnitIO( IOWrite, 3 );
  
  SetDDS(424);
  RegPtr^[0] := AuxCmd; RegPtr^[1] := #014; { TCA, take control asychronously }
  RegPtr^[2] := AuxCmd; RegPtr^[3] := #012; { TON, talk only off }
  RegPtr^[4] := AuxCmd; RegPtr^[5] := #011; { LON, listen only on }
  DoUnitIO( IOWriteRegs, 6 );

  { if the BitPad is not connected, the above command }
  { should time out, setting GPIBPadConnected to false }

  if GPIBPadConnected then begin
    SetDDS(425);
    RegPtr^[0] := #077;                     { UNL, unlisten }
    RegPtr^[1] := #137;                     { UNT, untalk }
    DoUnitIO( IOWrite, 2 );
    end;

  SetDDS(426);
  RegPtr^[0] := AuxCmd; RegPtr^[1] := #213; { GTS, goto standby }
  DoUnitIO( IOWriteRegs, 2 );

  SetDDS(427);
  dispose( StatPtr );
  dispose( RegPtr );

end;

var
  i : integer;
  CircleBuf : CirBufPtr;
  
begin { GPB_Init }

  SetDDS(411);
  TimeOutCnt := 60;
  StatReceived := false; 
  Z80Replied := false;
  NakReceived := false;
  
  OurMsg.pNext := nil;           { assign to unchanging part of message }
  OurMsg.UCodeArea    := 0;
  OurMsg.SOMDelimiter := Z_SOM;
  OurMsg.Device       := GPIB;
    
  new( IOSegNum, 1, OurBufr );   { GPB_WriteCharacter and GPB_PutStatus }
  new( IOSegNum, 1, OurStat );   { use these two for calls to GPB_UnitIO }
 
  if pUDevTab^[GPIB].pDataCtrl = nil then begin
    SetDDS(412);
    new( IOSegNum, 4, pGPIBHiVol );  { allocate the high volume block }
    pUDevTab^[GPIB].pDataCtrl := recast( pGPIBHiVol, pointer );
    pGPIBHiVol^.DataByteCnt := 0;
    pGPIBHiVol^.pDataBuffer := nil;
    end;

  if pUDevTab^[GPIB].pStatus = nil then begin
    SetDDS(413);
    new( IOSegNum, 4, pGPIBSts );    { allocate the status block }
    pUDevTab^[GPIB].pStatus := recast( pGPIBSts, pointer );
    pGPIBSts^.IntStat0 := 0;
    pGPIBSts^.IntStat1 := 0;
    pGPIBSts^.IntAddrStat := 0;
    pGPIBSts^.IntBusStat  := 0;
    pGPIBSts^.IntAddrSwch := 0;
    pGPIBSts^.IntCmdPass  := 0;
    pGPIBSts^.CurAddrStat := 0;
    pGPIBSts^.CurBusStat  := 0;
    pGPIBSts^.CurAddrSwch := 0;
    pGPIBSts^.CurCmdPass  := 0;
    end;

  if pUDevTab^[GPIB].pCirBuf = nil then begin
    SetDDS(414);
    new( IOSegNum, 4, CircleBuf );  { allocate a circular buffer }
    CircleBuf^.Length := CirBufSize;
    CircleBuf^.RdPtr := 0;
    CircleBuf^.WrPtr := 0;
    pUdevTab^[GPIB].pCirBuf := CircleBuf;
    end;
  
  SetDDS(415);
  MsgMask := lor( shift( 1, Dev_AckReceived )      { interrupts due to msgs }
           , lor( shift( 1, Dev_NakReceived )
           ,      shift( 1, Dev_StatReceived ) ) );
  
  GPIBIntMask := lor( MsgMask                      { all possible interrupts }
               , lor( shift( 1, Dev_Attention )
               ,      shift( 1, Dev_DataAvailable ) ) );

  loadexpr( GPIBIntMask );
  loadexpr( GPIB );
  StartIO( EP_SetEnableMask );
  
  SetDDS(416); 
  NotThere := false;
  GPB_UnitIO( recast( OurBufr, IOBufPtr ), IOSense, 0
            , recast( TimeOutCnt, double ), OurStat ); 
  
  if (OurStat^.SoftStatus = IOEDNS) Or (OurStat^.SoftStatus = IOETIM)
  then NotThere := true
  else SetUpGPIB;

end { GPB_Init };

function GPB_ReadChar( var Ch : char ): integer;

{-----------------------------------------------------------------------------
{
{ Abstract
{       Do character reads from the GPIB
{
{-----------------------------------------------------------------------------}

var
  Indicator : integer;
  Results   : integer;
  
begin

  loadexpr( GPIB );
  StartIO( EP_GetChar );                { Get a character from GPIB }
  storexpr( Indicator );                { tells us if a character present }
  storexpr( Results );                  { the character and a status }


  if Indicator = 0                      { character available? }
  then GPB_ReadChar := IOEIOB   
  else
    begin
    Ch := chr( land( Results, 255 ) );  { return the character }
    Results := shift( Results, -8 );    { Isolate status code }
    if results = #240                   { Normal is INT0 and BI }
    then GPB_ReadChar := IOEIOC         { Normal Status }
    else if Results = 255
    then GPB_ReadChar := IOEOVR         { circular buffer overflowed }
    else if Land(Results, #10) <> 0
    then GPB_ReadChar := IOEEND         { EOI on character }
    else GPB_ReadChar := IOEIOC;        { Normal Status }
    end;
end;

function GPB_WriteChar( var Ch : char ): integer;

{-----------------------------------------------------------------------------
{
{ Abstract
{       Do one character writes to the GPIB.  (Use GPB_UnitIO)
{
{-----------------------------------------------------------------------------}

var
  AMessage : Z_MsgPtrKludge;
   
begin
  
  if NotThere then begin
    GPB_WriteChar := IOEDNS;
    exit( GPB_WriteChar )
    end;
  
  OurMsg.Command := ord(Z_SendData);
  OurMsg.ByteCount := Z_FirstData;
  OurMsg.Data[Z_FirstData] := ord( Ch );
                    
  loadexpr( 0 );                    
  startio( EP_UcodeMsg );
  storexpr( AMessage.Segment );
  storexpr( AMessage.Offset );
  if AMessage.pMsg = nil then AMessage.pMsg := Z_DqSysMsg;

  AMessage.pMsg^ := OurMsg;
  
  NakReceived := false;
  Z80Replied := false;
  
  loadexpr( ord(Z_Q0) );
  {$ifc debug
  then}    loadadr( AMessage.pMsg );
  {$elsec} loadadr( AMessage.pMsg^ );
  {$endc}
  startio( EP_Z80Msg );
  
  repeat until Z80Replied;
  
  if NakReceived And (Not NotThere) Then GPB_WriteChar := IOEDNR
      Else If NAKReceived Then GPB_WriteChar := IOEUDE
      Else If NotThere Then GPB_WriteChar := IOEDNS
      Else GPB_WriteChar := IOEIOC;

end;

procedure GPB_UnitIO( Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : integer; 
                      LogAdr  : Double;
                      StsPtr  : IOStatPtr );

{-----------------------------------------------------------------------------
{
{ Abstract
{       Do IO to the GPIB
{
{ Notes 
{    The LogAdr is a count of the number of jiffies to wait before timing
{    out an operation and resetting the GPIB.  It is recast into a long.
{
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
  BytesSent  : integer;
  Users      : ManyPtrs;
  ThisCmd    : integer;
  MustDec    : boolean;
  pOurBytes  : pByteArray;
  RegMsg     : Z_MsgPtrKludge;
  NumSending : integer;
  MsgNum     : integer; { which message of multiple messages }
  i          : integer; 
  TimeOut    : packed record case boolean of
                 true  : (Cnt : long);
                 false : (dbl : double);
                 end;
  StartTime  : long;  
  MustFlush  : boolean;
  MustFEOI   : boolean;
  
procedure Error( ErrCode : integer );

begin

  if MustDec
  then begin
       Users.pBuffer := Bufr;      { Decrement correct segment }
       DecIOCount( Users.Segment );
       end;

  StsPtr^.SoftStatus := ErrCode;
  if (Command = IOWriteHiVol)
  or (Command = IOReadHiVol)
  then StsPtr^.BytesTransferred 
                      := StsPtr^.BytesTransferred - pGPIBHiVol^.DataByteCnt
  else StsPtr^.BytesTransferred := BytesSent;


  exit( GPB_UnitIO )
  
end;

begin

  BytesSent := 0;            { no information sent yet }
  MustDec := false;          { don't need to lock user's buffer in core }
  Users.pBuffer := Bufr;     { we may need to look at the pointer differently }
  TimeOut.dbl := LogAdr;

  if NotThere then Error( IOEDNS );
  
  case Command of   
                                              
    IOWrite,
    IOWriteEOI  : begin
      if ByteCnt < 0 then Error( IOEBSE );
      ThisCmd := ord(Z_SendData); 
      end;

    IOSense     : begin
      ByteCnt := 0;
      ThisCmd := ord(Z_Sense)
      end;

    IOFlush     : begin
      ByteCnt := 0;
      ThisCmd := ord(Z_Flush)
      end;

    IOFlushIn   : begin
      ByteCnt := 0;
      ThisCmd := ord(Z_FlushIn)
      end;

    IOReset     : begin
      ByteCnt := 0;
      ThisCmd := ord(Z_Reset)
      end;

    IODevRead   : begin
      if ByteCnt <> 6 then Error( IOEBSE );
      { Next make sure we have not asked for more data than can fit in }
      { the GPIB input ring buffer. (Assume empty buffer at start.)    }
{$R-} if Users.pByte^[5] > ((CirBufSize-1) div 2) then Error( IOEBSE );
{$R=} ThisCmd := ord(Z_DevRead ); 
      end;

    IODevWrite  : begin
      if ByteCnt < 5 then Error( IOEBSE );
      ThisCmd := ord(Z_DevWrite); 
      MustFlush := false;
      MustFEOI := false;
      end;

    IOWriteRegs : begin
      if odd( ByteCnt ) or (ByteCnt < 0) then Error( IOEBSE );
{$R-} for NumSending := 0 to shift( ByteCnt, -1 ) - 1 do
        if (Users.pByte^[shift( NumSending, 1 )] > 6) then Error( IOERDI ); 
{R=}  ThisCmd := ord(Z_WriteRegisters)
      end;

    IOConfigure : begin
      if ByteCnt <> 1 then Error( IOEBSE ); 
      if  (Users.pByte^[0] <> 0)
      and (Users.pByte^[0] <> 255) then Error( IOECDI );
      ThisCmd := ord(Z_Config)
      end;

    IOWriteHiVol : begin
      pGPIBHiVol^.DataByteCnt := ByteCnt;       { transfer this many bytes }
      pGPIBHiVol^.pDataBuffer := Users.pBuffer; { to/from this location }

      if land( Users.Offset, 3) <> 0 then Error( IOEBAE ); 
      if ByteCnt < 0 then Error( IOEBSE );
        
      IncIOCount( Users.Segment ); { lock buffer into core }
      MustDec := true;             { must unlock before leaving }

      if ByteCnt = 1               { one character high volume writes }
      then begin                   { blow up, use low volume instead }
        Command := IOWrite;
        ThisCmd := ord(Z_SendData)
        end 
      else begin                   { make Bufr point to the number of bytes }
        OurBufr^[0] := ByteCnt;    { to transfer, so that ByteCnt is in the }
        Users.pBuffer := recast( OurBufr, IOBufPtr ); { message to the Z80  }
        StsPtr^.BytesTransferred := ByteCnt; { set BytesTransferred correctly} 
        ByteCnt := 2;
        ThisCmd := ord(Z_OutHiVolumeStart)
        end
      end;

    IOReadHiVol : begin
      pGPIBHiVol^.DataByteCnt := ByteCnt;       { transfer this many bytes }
      pGPIBHiVol^.pDataBuffer := Users.pBuffer; { to/from this location }
      
      if (Users.Offset mod 4) <> 0 then Error( IOEBAE ); 
      if ByteCnt < 2 then Error( IOEBSE ); { one char hi vol reads blow up }
  
      IncIOCount( Users.Segment ); { lock buffer into core }
      MustDec := true;             { must unlock before leaving }

      Users.pBuffer^[0] := ByteCnt;   { put ByteCnt into the user's buffer }
      StsPtr^.BytesTransferred := ByteCnt; { set BytesTransferred correctly }
      ByteCnt := 2;                   { so it gets put into the Z80 message }
      ThisCmd := ord(Z_InHiVolumeStart) 
      end;

    Otherwise   : Error( IOEILC )
    end;
 
  MsgNum := 0;  { sending the 0th message }
  repeat
    NumSending := ByteCnt - BytesSent;  { find out how many bytes we will }
    if NumSending > Z_DataSize          { put into the message }
    then NumSending := Z_DataSize
    else if Command = IOWriteEOI        { last packet, check for EOI } 
         then ThisCmd := ord(Z_EOIData);
    
    {$R-}    
    if NumSending = Z_DataSize               { sending a full message? }
    then OurMsg.Data := Users.pData^[MsgNum] { move in one fell swoop }
    else for i := 0 to NumSending-1 do 
           OurMsg.Data[ Z_FirstData + i ] := Users.pByte^[ BytesSent + i ]; 
    {$R=} 

    if (Command = IODevWrite) and (ByteCnt > Z_DataSize) then
       { The first msg will be Z_DevWrite and is followed by Z_SendData msgs. }
       { If the ForceEOI option is selected, the last msg must be Z_EOIData.  }
       if MsgNum = 0 then  { First msg }
          begin
          i := OurMsg.Data[Z_FirstData];  { Get hold of the Option byte. }
          { Now remove the WaitOnData and ForceEOI bits from the Option byte. }
          { If these options were selected, we must handle them at the end.   }
          { Since we have more data than can fit in the Z_DevWrite cmd msg to }
          { the Z80, we remove these option bits here and handle them at the  }
          { end. For ForceEOI, we send the last data msg as Z_EOIData instead }
          { of Z_SendData. For WaitOnData, we must do a Flush after the last  }
          { Z_SendData msg.                                                   }
          OurMsg.Data[Z_FirstData] := land(i, #237);
          MustFlush := (land(i, #040) = #040); { Was WaitOnData option set? }
          MustFEOI  := (land(i, #100) = #100); { Was ForceEOI option set?   }
          end
       else if (NumSending <= (ByteCnt - BytesSent)) and MustFEOI then
          { Last msg going out and EOI is required. }
          ThisCmd := ord(Z_EOIData)
       else
          ThisCmd := ord(Z_SendData);
          
    OurMsg.Command := ThisCmd;
    OurMsg.ByteCount := Z_NoData + NumSending;

    Z80Replied := false;             { indicate a message waiting }
    NakReceived := false;            { it isn't NAKed }
    StatReceived := false;           { and we don't have status information }

    loadexpr( 0 );
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

    BytesSent := BytesSent + NumSending;
    MsgNum := MsgNum + 1;  { another message sent }
   

    if TimeOut.Cnt <= 0     { no time out }
    then repeat until Z80Replied
    else begin 
      StartTime := TimeBuf^;
      repeat  
        if TimeBuf^ - StartTime > TimeOut.Cnt then begin  { time out }  
          TimeOut.Cnt := 0;                           { resets don't time out }
          GPB_UnitIO( Bufr, IOReset, 0, TimeOut.Dbl, StsPtr ); 
          Error( IOETIM );
          end; 
      until Z80Replied; 
      end; 



    { We assume that if the NotThere flag is reset then the device is }
    { being supported by the Z80. If a NAK is received for a sense    }
    { then the device is not supported and the IOEDNS condition is    } 
    { returned.                                                       }

    if (Command = IOFlush) and NakReceived Then Error(IOEDNI);
    if (Command = IOSense) And NakReceived Then Error(IOEDNS);
    if NakReceived And (Not NotThere) Then Error(IOEDNR);
    If StatReceived <> (Command = IOSense) Then Error(IOEUDE);

  until BytesSent = ByteCnt;
      
  if (Command = IODevWrite) and MustFlush then
     begin
     if TimeOut.Cnt > 0 then
        begin
        TimeOut.Cnt := TimeOut.Cnt - (TimeBuf^ - StartTime);
        if TimeOut.Cnt <= 0 then Error( IOETIM );
        end;
     GPB_UnitIO(Bufr, IOFlush, 0, TimeOut.Dbl, StsPtr);
     { StsPtr^.SoftStatus will be correctly set by the above call while }
     { StsPtr^.BytesTransferred will be handled corectly below.         }
     end
  else
     StsPtr^.SoftStatus := IOEIOC;           { indicate success }

  case Command of
    IOSense : begin
      Users.pStat^ := pGPIBSts^;
      StsPtr^.BytesTransferred := 10;
      end;
      
    IOWriteHiVol,
    IOReadHiVol : with StsPtr^ do
      BytesTransferred := BytesTransferred - pGPIBHiVol^.DataByteCnt;
    
    otherwise : StsPtr^.BytesTransferred := ByteCnt;
    end;
    
  if MustDec
  then begin
       Users.pBuffer := Bufr;    { Decrement users buffer }
       DecIOCount( Users.Segment )
       end;
end;

procedure GPB_GetStatus( var StatBlk : DevStatusBlock );

{-----------------------------------------------------------------------------
{
{ Abstract
{       Provide status information about the GPIB.  This is here only 
{       to provide compatibility with the old system.  The recommended way
{       to get status information is to use UnitIO and the IOSense command.
{
{-----------------------------------------------------------------------------}

var
  GPIBStatus : GPIBStat;
  TimeOut : long;

begin
  
  if not NotThere then begin   { if there }
    TimeOut := 0;
    GPB_UnitIO( recast( OurBufr, IOBufPtr ), IOSense
              , 0, recast( TimeOut, double ), OurStat );
    
    GPIBStatus := pGPIBSts^;
    StatBlk.Int1Status  := GPIBStatus.IntStat1;
    StatBlk.AddrSwitch  := GPIBStatus.IntAddrSwch;
    StatBlk.AddrStatus  := GPIBStatus.IntAddrStat;
    StatBlk.CmdPassThru := GPIBStatus.IntCmdPass;
    StatBlk.Int0Status  := GPIBStatus.IntStat0;
    StatBlk.BusStatus   := GPIBStatus.IntBusStat;
    end;
  
end.
