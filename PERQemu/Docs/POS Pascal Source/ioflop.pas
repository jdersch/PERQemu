{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOFloppy;
{-----------------------------------------------------------------------------
{
{ IOFloppy - Floppy IO routines.
{ Copyright (C) 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       IO_Floppy exports procedures to perform IO on the floppy.
{
{ Design:
{       1) UnitIO must increment and decrement the IOCount of the segments
{          which are involved in IO.
{       2) Segment faults must *never* happen while interrupts are off.
{
{-----------------------------------------------------------------------------}

{$Version V0.9 for POS}
{-------------------------------------------------------------------------}
{ Change Log: 
{ 17 May 83  V0.9 Eric Beattie, ICL
{            Inserted code to keep count of amount of data transferred on
{            reads and writes.
{
{  4 May 83  V0.8 Eric Beattie, ICL
{            Changed '>' in first line of procedure DoSeek to '>=' as per
{            instructions from Dave Anderson to completely fix bug Z3.
{
{ 12 Apr 83  V0.7 Dave Anderson, ICL
{            Moved Cylinder range check from IOWrite, IORead & IOFormat into
{            DoSeek, so can apply to IOSeek as well.
{
{ 11 Feb 83  V0.6 Roger Riggs
{            Added ReadID command, Returns cylinder head and sector of
{            next sector that passes under the head.
{
{ 31 Jan 83  V0.5 Roger Riggs
{            Added Code to raise device attention when attention
{            is received from the floppy. Also Fixed bug in track crossing
{            to correctly add ByteCnt div 2 to Buffer Offset.
{
{ 04 Jan 83  V0.4 August G. Reinig
{            Changed calls to SetDDS.
{
{ 23 Dec 82  V0.3 August G. Reinig
{            Added routine DoReset.  Changed DoSeek to call DoReset if entered
{            with CurrCyl = -1.  Seeks don't work if there was an error with
{            the last operation.
{
{ 20 Dec 82  V0.2 August G. Reinig
{            Removed local definition of CurrCyl in Flp_UnitIO.
{
{ 14 Dec 82  V0.1 August G. Reinig
{            Modified code to do startio's itself
{            
{ 27 Oct 82  V0.0 Roger Riggs
{            Created module.
{
{-------------------------------------------------------------------------}
{*******************************}   Exports   {*****************************}
Imports IO_Private from IO_Private;

Procedure FLP_Initialize;           { Floppy Initialization     }
Procedure FLP_Interrupt;            { Floppy interrupt handler  }
Procedure FLP_UnitIO(               { Floppy UnitIO routine     }
                Bufr: IOBufPtr;
                Command: IOCommands;
                ByteCnt: Integer;
                LogAdr: Double;
                StsPtr: IOStatPtr);

Procedure FLP_PutStatus(            { Set status on device Unit }
                var StatBlk: DevStatusBlock);
Procedure FLP_GetStatus(            { Read status on device Unit }
                var StatBlk: DevStatusBlock);

{*******************************}   Private   {*****************************}
const
    debug = false;
    
{$ifc debug
then}    Imports TestSystem from TestSystem;
{$elsec} Imports System from System;
{$endc}

Imports IOErrors from IOErrors;
Imports Virtual from Virtual;

Const
    FlpCylinders = 77;              { 0..76 cylinders on floppy }
    FlpSectors = 26;                { 1..26 cylinders on floppy } 

Type
    tStatus = packed array [0..9] of 0..255; { raw status buffer from floppy }
    pStatus = ^ tStatus;

Var
    BufferDesc : pHiVolBlock;   { pointer to data descriptor }
    StatusP : pStatus;              { pointer to status buffer }
    CurrCyl : Integer;              { Current cylinder on floppy }
    IntCause : Z_CmdRegister;       { Contains copy of interrupt cause }
    AtnCause : Z_CmdRegister;       { Contains copy of attention cause }
    Density : (DenSingle, DenDouble);{ Current density }
    FmtGpl : Array [Ord(DenSingle)..Ord(DenDouble)] of integer;
    Gpl : Array [Ord(DenSingle)..Ord(DenDouble)] of integer;
    SectorSize : Array [Ord(DenSingle)..Ord(DenDouble)] of integer;
    OurMsg : Z_Msg;
{$R-}

Procedure FLP_Initialize;           { Floppy Initialization     }

begin

SetDDS(401);
New(IOSegNum, 4, StatusP);           { Permanently allocated status buffer }
pUDevTab^[Floppy].pStatus := RECAST( StatusP, pointer );

SetDDS(402);
New(IOSegNum, 4, BufferDesc);       { Permanently Allocated data descriptor }
pUDevTab^[Floppy].pDataCtrl := RECAST( BufferDesc, pointer );

SetDDS(403);
CurrCyl := -1;                      { Force seek on first operation }

GPl[Ord(DenSingle)] := 7;           { Single density gap length }
Gpl[Ord(DenDouble)] := 14;          { Double density gap length }
FmtGpl[Ord(DenSingle)] := 27;       { Single density format gap length }
FmtGpl[Ord(DenDouble)] := 54;       { Double density format gap length }
SectorSize[Ord(DenSingle)] := 128;  { Single density sector size }
SectorSize[Ord(DenDouble)] := 256;  { Double density sector size }
Density := DenSingle;

OurMsg.pNext := nil;        { initialize the unchanging part of our message }
OurMsg.Device := Floppy;
OurMsg.UcodeArea := 0;
OurMsg.SOMDelimiter := Z_SOM;

SetDDS(404);
LoadExpr( lor( shift( 1, Dev_AckReceived )   { Put enable mask on E-Stack }
        , lor( shift( 1, Dev_NakReceived )
        , lor( shift( 1, Dev_StatReceived )
        ,      shift( 1, Dev_Attention )))));
LoadExpr(Floppy);                   { Put device number on E-Stack }
StartIO(EP_SetEnableMask);          { Enable interrupts for floppy }

end;

Procedure FLP_Interrupt;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       FloppyIntr handles a floppy interrupt.
{       If the handler is waiting for an interrupt
{       The interrupt cause is read and cleared.
{       The Interrupt and Attention causes are held for the low level handler
{
{       If the interrupt is an attention then the current cylinder
{       is set to -1 to force a seek on the next operation.
{
{-----------------------------------------------------------------------------}

var
    NewAtn, NewInt : Z_CmdRegister;

begin                               { Floppy Disk Interrupt Service }

    LoadExpr(Floppy);               { get Interrupt cause for floppy }
    StartIO(EP_ReadCause);          { get interrupt and attention reason }
    StorExpr(NewAtn.Number);        { Save attention cause field }
    StorExpr(NewInt.Number);        { Save interrupt cause }

    if NewInt.Bits[Dev_Attention]
    then
        begin
        CurrCyl := -1;              { Force seek on next operation }
        NewInt.Bits[Dev_Attention] := False;
        If RaiseException[Floppy].Attention
        then begin
             LoadExpr( lor( shift( 1, Dev_AckReceived )
                     , lor( shift( 1, Dev_NakReceived )
                     ,      shift( 1, Dev_StatReceived ))));
             LoadExpr(Floppy);      { Device floppy }
             StartIO( EP_SetEnableMask ); { Enable all but attentions }

             InLineByte({INTON} 106);
             Raise DevInterrupt (Floppy, IOAtnInterrupt, NewAtn.Number);
             InLineByte({INTOFF} 105);            

             LoadExpr( lor( shift( 1, Dev_AckReceived )
                     , lor( shift( 1, Dev_NakReceived )
                     , lor( shift( 1, Dev_StatReceived )
                     ,      shift( 1, Dev_Attention )))));
             LoadExpr(Floppy);      { Device floppy }
             StartIO( EP_SetEnableMask ); { Enable all interrupts }
             end;
        end;

    IntCause.Number := LOR(IntCause.Number, NewInt.Number);

InLineByte({INTON} 106)             { enable further interrupts }
end { FLP_Interrupt };

procedure SendIt;

{-----------------------------------------------------------------------}
{
{ Abstract:
{       
{      Send a message about the floppy to the Z80.  Caller has set the
{      proper values in OurMsg.
{
{-----------------------------------------------------------------------}

var
  AMessage : Z_MsgPtrKludge;

begin
  
  loadexpr( 0 );
  startio( EP_UcodeMsg );           { get a message from the micro code }
  storexpr( AMessage.Segment );
  storexpr( AMessage.Offset );
  if AMessage.pMsg = nil then AMessage.pMsg := Z_DqSysMsg;

  AMessage.pMsg^ := OurMsg;
  
  IntCause.Number := 0;             { clear interrupt cause }

  loadexpr( ord(Z_Q1) );            { send the message to the Z80 }
  {$ifc debug
  then}    loadadr( AMessage.pMsg );
  {$elsec} loadadr( AMessage.pMsg^ );
  {$endc}
  startio( EP_Z80Msg ); 
  
  repeat until IntCause.Number <> 0;  { wait for ACK/NAK }

end;

Procedure FLP_PutStatus(var StatBlk: DevStatusBlock);
{-----------------------------------------------------------------------------
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
{               FlpDensity is used to determine the density.
{
{-----------------------------------------------------------------------------}

begin

if Land(StatBlk.FlpDensity, #100) = 0     
then Density := DenSingle            
else Density := DenDouble;

OurMsg.Command := Ord(Z_Config);                  { Set command to configure}
OurMsg.data[Z_FirstData] := 26;                   { 26 sectors per track}
OurMsg.data[Z_FirstData+1] := Gpl[Ord(Density)];  { gap length for double den }
OurMsg.data[Z_FirstData+2] := Ord(Density);       { encoded data length }
OurMsg.data[Z_FirstData+3] := Statblk.FlpDensity; { use density as options }
OurMsg.ByteCount := Z_FirstData+3;                { total bytes in message }

SendIt;                                   { Send the Configure message }


OurMsg.Command := Ord(Z_Specify);  { Set command to specify }
OurMsg.Data[Z_FirstData+0] := 3;   { Step rate 3ms per track }
OurMsg.Data[Z_FirstData+1] := 10;  { Head Unload time 10 * 16ms }
OurMsg.Data[Z_FirstData+2] := 36;  { Head load time 36 * 2ms }
OurMsg.ByteCount := Z_FirstData+2; { total bytes in specify message }

SendIt;                            { Send the specify command }

end { IOPutStatus };

Procedure FLP_GetStatus(var StatBlk:DevStatusBlock );
{-----------------------------------------------------------------------------
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
{-----------------------------------------------------------------------------}

begin

OurMsg.ByteCount := 2;              { total bytes in Sense message }
OurMsg.Device := Floppy;            { Set device code }
OurMsg.Command := Ord(Z_Sense);     { Set command to sense }

SendIt;                             { send the Sense message }

If Not IntCause.Bits[Dev_StatReceived]
then StatusP^[0] := 0;              { Force no valid status from floppy }

Case StatusP^[0] of
    1:  begin
        StatBlk.FlpByte1 := StatusP^[1];
        StatBlk.FlpByte2 := StatusP^[2];
        end;
    3:  begin
        StatBlk.FlpByte1 := StatusP^[1];
        StatBlk.FlpByte2 := StatusP^[2];
        StatBlk.FlpByte3 := StatusP^[3];
        StatBlk.FlpByte4 := StatusP^[4];
        StatBlk.FlpByte5 := StatusP^[5];
        StatBlk.FlpByte6 := StatusP^[6];
        StatBlk.FlpByte7 := StatusP^[7];
        end;   
    otherwise:
        begin
        StatBlk.FlpByte1 := #230;   { Force equipment check & illegal command }
        end;
    end;

end { IOGetStatus };

Procedure FLP_UnitIO(   Bufr: IOBufPtr;
                        Command: IOCommands;
                        ByteCnt: integer;
                        LogAdr: double;
                        StsPtr: IOStatPtr );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      IO to non-character devices.
{
{ Parameters:
{
{      Bufr - buffer for datA transfers, if requested.
{
{      Command - operation to be performed on the device.
{
{      ByteCnt - number of bytes to be transferred.
{
{      LogAdr - logical address for block structured devices.
{
{      StsPtr - resultant status from the operation.
{
{-----------------------------------------------------------------------------}

  
  
var
    Cylinder : Integer;         { Desired cylinder }
    Sector : Integer;           { Desired sector }
    BytesLeft : Integer;        { Number of bytes remaining in transfer }    
    CurrBytes : Integer;        { Number of bytes in current transfer }
    Buf: IOPtrKludge;           { Internal copy of buffer pointer }
    ReadIDBuf : pFlpPhyHdr;     { Used to return ReadID data }
  
  Procedure IOErr(Err: integer);
  { return from UnitIO with error ERR }
  begin
  StsPtr^.SoftStatus := Err;
  if (Command = IORead) or (Command = IOWrite) then
     stsptr^.BytesTransferred := ByteCnt-BytesLeft;
  if Buf.Buffer <> nil then DecIOCount(Buf.Segment);
  Exit(FLP_UnitIO)
  end { IOErr };

  Procedure FlpErr(Cmd : Z_Commands);
  {---------------------------------------------------------------------------
  {
  { Abstract:
  {      Check hardware status after a floppy operation fails and issue the
  {      appropriate IO error.
  {
  {---------------------------------------------------------------------------}

  var Stat: DevStatusBlock;
      Err: integer;
  begin { FlpErr }
   IOGetStatus(Floppy,Stat); { let's find out what went wrong }
   with Stat do
    begin
     if FlpNotReady then Err := IOEDNR { device not ready }
     else if FlpEquipChk then Err := IOEUEF { undetermined equipment fault }
     else if Cmd <> Z_Seek then { check detailed status }
      begin
       if FlpOverRun then Err := IOEOVR { overrun }
       else if FlpDataMissAddr then Err := IOEMDA { missing data address mark }
       else if FlpMissAddr then Err := IOEMHA { missing header address mark }
       else if FlpNotWritable then Err := IOEDNW { device not writable }
       else if FlpBadCylinder or FlpWrongCylinder then Err := IOECMM
                                                 { cylinder mis-mAtch }
       else if FlpEndCylinder or FlpNoData then Err := IOESNF{sector not found}
       else if FlpDataDataError then Err := IOEDAC { data CRC error }
       else if FlpDataError then Err := IOELHC { logical header CRC error }
       else if FlpIntrCode = 2 then Err := IOEILC { illegal command }
       else Err := IOEUEF { undetermined equipment fault }
      end
     else if FlpIntrCode = 2 then Err := IOEILC { illegal command }
     else Err := IOEUEF { undetermined equipment fault };
     StsPtr^.HardStatus := FlpByte1;
     CurrCyl := -1;
     IOErr(Err)
    end
  end { FlpErr };

procedure DoReset;                  { Reset the Floppy }

begin  

  CurrCyl := -1;                    { Don't know where the drive is }

  OurMsg.ByteCount := 2;            { length is just device and command }
  OurMsg.Command := Ord(Z_Reset);   { reset floppy command }
  SendIt;                           { Send the Reset message }
  If Not IntCause.Bits[Dev_AckReceived] 
  then FlpErr(Z_Reset);

  OurMsg.Command := Ord(Z_Recal);   { Recalibrate command }
  OurMsg.Data[Z_FirstData] := 0;    { Unit zero }
  OurMsg.ByteCount := Z_FirstData;  { length is just device, cmd, options }
  SendIt;                           { Send the Recalibrate message }
  If Not IntCause.Bits[Dev_AckReceived] 
  then FlpErr(Z_Recal);

  CurrCyl := 0;                     { Recalibrate to cylinder 0 }

end;  




procedure DoSeek;

begin                               { perform seek to desired cylinder }

  if (Cylinder < 0) OR (Cylinder >= FlpCylinders * 2 )
      then IOErr(IOECOR);           { Take error for Cylinder out of range }
  if CurrCyl = -1                   { if there was an error last time }
  then DoReset;                     { reset the floppy before trying a seek }
    
  CurrCyl := -1;                    { In case of error force another seek }

  OurMsg.Command := Ord(Z_Seek);    { Seek floppy command }
  OurMsg.Data[Z_FirstData] := 0;    { Head select }
  OurMsg.Data[Z_FirstData+1] := Cylinder mod FlpCylinders;
  OurMsg.ByteCount := Z_FirstData+1;{ device, cmd, unit and cyl }
  SendIt;                           { send the seek message }

  If Not IntCause.Bits[Dev_AckReceived]
  then FlpErr(Z_Seek);

  CurrCyl := Cylinder;              { save new current cylinder }
        
end;

begin

stsptr^.BytesTransferred := 0;
BytesLeft := ByteCnt;               { Make our copy of byte count }
Cylinder := LogAdr[1];              { Make copy of cylinder }
Sector := LogAdr[0];                { Make Copy of sector number }

Buf.Buffer := Bufr;                 { Copy of buffer pointer }
if Land(Buf.Offset, 3) <> 0
then IOErr(IOEBAE);                 { Buffer alignment error }
if Bufr <> Nil
then IncIOCount(Buf.Segment);       { Increment IO Count on real buffers }

Case Command of
  IOReset: DoReset;

  IOSeek: DoSeek;

  IOWrite, 
  IORead:
    begin
    Repeat
      if (Sector < 1) OR (Sector > FlpSectors)
      then IOErr(IOESOR);           { Take error for sector out of range }
      if (BytesLeft Mod SectorSize[Ord(Density)] <> 0) OR
         (BytesLeft <= 0)
      then IOErr(IOEBSE);           { length must multiple of block size }
  
      if CurrCyl <> Cylinder        { I/O to same cylinder as before? }
      then DoSeek;
          
      CurrBytes := (FlpSectors + 1 - Sector) * SectorSize[Ord(Density)];
      if CurrBytes > BytesLeft      { Read to end of cylinder or desired }
      then CurrBytes := BytesLeft;  { Which ever is less }
  
      BufferDesc^.DataByteCnt := CurrBytes;
      BufferDesc^.pDataBuffer := Buf.Buffer;
  
      If Command = IORead                      { Set command to Floppy }
      then OurMsg.Command := Ord(Z_ReadData)
      else OurMsg.Command := Ord(Z_WriteData);
      OurMsg.Data[Z_FirstData] := Shift(Cylinder div FlpCylinders,2);
      OurMsg.Data[Z_FirstData+1] := Sector;    { starting sector number }
      OurMsg.Data[Z_FirstData+2] := Land(CurrBytes, 255);
      OurMsg.Data[Z_FirstData+3] := Land(Shift(CurrBytes, -8), 255);
      OurMsg.ByteCount := Z_FirstData+3;       { length }
      SendIt;                                  { send the read/write command }
      If Not IntCause.Bits[Dev_AckReceived]
      then begin
           if Command = IORead 
           then FlpErr(Z_ReadData) 
           else FlpErr(Z_WriteData);
           end;
  
      Buf.Offset := Buf.Offset + (CurrBytes Div 2);{ Update pointer to buffer }
      BytesLeft := BytesLeft - CurrBytes; { Reduce remaining byte count }
      Sector := 1;                  { start at beginning of next cylinder }
      Cylinder := Cylinder + 1;     { increment to next cylinder }
    Until BytesLeft = 0;
    stsptr^.BytesTransferred := ByteCnt;
    end;

  IOFormat:
    begin
    if (Sector < 1) OR (Sector > FlpSectors)
    then IOErr(IOESOR);           { Take error for sector out of range }
    if LAND(BytesLeft, #3) <> 0
    then IOErr(IOEBSE);           { length must multiple of 4 }

    if CurrCyl <> Cylinder        { I/O to same cylinder as before? }
    then DoSeek;

    BufferDesc^.DataByteCnt := BytesLeft;
    BufferDesc^.pDataBuffer := Buf.Buffer;

    OurMsg.Command := Ord(Z_Format);         { Format floppy command }
    OurMsg.Data[Z_FirstData] := Shift(Cylinder div FlpCylinders,2);
    OurMsg.Data[Z_FirstData+1] := BytesLeft Div 4; { number of sectors }
    OurMsg.Data[Z_FirstData+2] := 170;       { filler byte }
    OurMsg.Data[Z_FirstData+3] := FmtGPL[Ord(Density)]; { Insert Gap length }
    OurMsg.ByteCount := Z_FirstData+3;       { length }
    SendIt;                                  { send format command }

    If Not IntCause.Bits[Dev_AckReceived]
    then FlpErr(Z_Format);

    end;
  IOReadID :
    begin
    OurMsg.Command := Ord(Z_ReadID);    { ReadID floppy command }
    OurMsg.Data[Z_FirstData] := Shift(Cylinder div FlpCylinders,2);
    OurMsg.ByteCount := Z_FirstData;    { device, cmd, unit/head }
 
    SendIt;                             { send the seek message }

    If Not IntCause.Bits[Dev_StatReceived]
    then FlpErr(Z_Seek);

    if Land(#300, StatusP^[1]) <> 0
    then FlpErr(Z_ReadID);

    ReadIDBuf := RECAST(Bufr, pFlpPhyHdr);
    ReadIDBuf^.Cylinder := StatusP^[4];     { Copy Cylinder }
    ReadIDBuf^.Head     := StatusP^[5];     { Copy Head }
    ReadIDBuf^.Sector   := StatusP^[6];     { Copy Sector }
    ReadIDBuf^.Size     := StatusP^[7];     { Copy Encoded Size }
    end;
  OtherWise : IOErr(IOEILC);      { Force illegal command exit }
  end { Case Command };

if Buf.Buffer <> Nil
then
  begin
  DecIOCount(Buf.Segment);
  Buf.Buffer := nil;
  end;

StsPtr^.SoftStatus := IOEIOC;

end { IOFloppy }.
