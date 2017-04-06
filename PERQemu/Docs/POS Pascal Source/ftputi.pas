{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FTPUtils;

{---------------------------------------------------------------------
{
{ File Transfer Program - Code.
{
{ Copyright (C) 1980, 1981, 1982, 1983
{ Three Rivers Computer Corporation
{
{ Abstract:
{    This file contains the code portions for the File Transfer Program.
{
{-----------------------------------------------------------------------}



{$Version V7.8 for POS}
{---------------------------------------------------------------------
{ Change Log:
{
{  7 Mar 83  V7.8  WJHansen
{   Fix FTPDoEPacket to supply a Stat pointer in CurrRecv & set EType.
{   Implement sending directories in WriteFile.
{   Do not restart timer when receive alien packet.
{
{ 28 Feb 83  V7.7  WJHansen
{   Implement ShowMemUsage and call from File operations.
{   Change RS232 to FTP_RSA and FTP_RSB.
{   Remove MPOS.
{   for IOPutStatus, change ByteCount from 1 to 3.
{   Implement FTPDoEPacket.
{   Removed IOPutStatus(RS232) from FTPInit.
{   Don't rewrite(x-file) if doing ethertransfer. Fixes FullSegm error (1036).
{   Don't create file if it doesn't exist on sender. (1134)
{   Provide FTPSetEtherAddr.
{   Create PostAllReceives; call in NetAllocate, FTPErr, FTPSetEtherAddr.
{
{ V6.5   7 Dec 82 Sandeep Johar  Implement aliases.
{
{ V6.4   2 Dec 82 Chuck Beckett  14 Char. compiler fixes.
{
{ V6.3  14 Nov 82 Don Scelza     Fixed bug that causedPooling machine to hang
{                                if a request was made for a file of 0 length.
{
{ V6.2  19 Oct 82 Don Scelza     Fixed bug that caused FTP to hang if it was
{                                asked for a file that was not there.
{                                Resolved the 14-char identifier problem and
{                                restored the name pEtherAdRec. (Also restored
{                                in Ether10IO module.)
{
{ V6.1  30 Aug 82 M. Kristofic   Fix bug caused by 14 character identifier
{
{ V6.0  07 Jul 82 Don Scelza     Started code for doing multiple
{                                receives.
{
{ V5.7  23 Jun 82 Don Scelza     Put checks in ReadPacket for correct type
{                                and source.
{ 
{ V5.6  13 May 82 Don Scelza     Put code in WaitPacket to dump packets that
{                                are not of the correct type.
{
{ V5.5  13 May 82 Don Scelza     Fixed Broadcast bug in net allocate.
{
{ V5.4  11 May 82 Don Scelza     Fixed bug I put into NetAllocate.
{
{ V5.3  07 May 82 Don Scelza     Fixed bug in FTPGetFile.  it was checking
{                                wrong var for text mode.
{
{ V5.2  29 Apr 82 Don Scelza     Put code in service to drop packets
{                                for device FastEther that have 
{                                large bytecounts.
{
{ V5.1  23 Apr 82 Don Scelza     Put code in wait packet to see if
{                                the current ethernet packet was from the
{                                machine involved in the current transaction.
{
{ V5.0  20 Apr 82 Don Scelza     New code for ethernet transfers.
{                                Put in Address requests.  Fixed many bugs.
{
{ V4.2i 30 Mar 82 Sandeep Johar  Clearbuffers on entering FTPChkDev
{
{ V4.2h 22 Mar 82 Sandeep Johar  Debug = false;
{
{ V4.2g 22 Mar 82 Sandeep Johar  TimeOut for MPOS to 20 Secs.
{
{ V4.2f 17 Mar 82 Sandeep Johar  Add some netinitialization calls.
{
{ V4.2e 17 Mar 82 Sandeep Johar  get it to compile for POS. EtherNet On.
{
{ V4.2d 11 Mar 82 Sandeep Johar  Add the real clock (60hertz) timeout func.
{
{ V4.2c 11 Mar 8: Sandeep Johar  MPOSVersion timeout = 50 squared.
{
{ V4.2b 11 Mar 82 Sandeep Johar  Fixed the document header format.
{                                compiles with ethernet off, MPOSVersion On.
{
{ V4.2a 9 Mar 82  Sandeep Johar EtherNet Off. Fixing compile problems with
{                               MPOSVersion.
{
{ V4.2 26 Feb 82  Sandeep Johar Link off. Fixed uninited variable bug 
{                               in ReadPacket. EtherHeader aligned at 8
{                               word boundaries. Timeout count changed to 250.
{
{ V4.1 17 Feb 82  Sandeep Johar Fixed for compiles with 14 char compiler.
{                               LinkDev turned on.
{            
{ V4.0 12 Nov  81 Don Scelza   Added retry on checksum errors.  Start to
{                              add Ethernet stuff.
{
{ V3.0 9  Nov  81 Mark Faust    Support for VaxFTP.
{
{ V2.3 23 Sept 81 Diana Forgy  Changed name from FTP to FTPUtils to
{                               conform to software naming conventions.
{                               Changed documentation boxes to be
{                               parsable by Virgil.
{
{ V2.2  1-Jun-81 Brad Myers   Changed IO import.
{
{ V2.1 21-Apr-81 Don Scelza   Set LinkDev = False for distribution.
{
{ V2.0 10-Apr-81 Don Scelza   Took out PDP-11 stuff.  Changed error
{   processing.  Added hooks for Link as a device.  Added UtilProgress.
{
{ V1.0 18-Feb-81 Don Scelza   Changed to run with OS version C.
{
{ V0.1 29-Sep-80 Brad Myers  added error report on FNF and no room.
{
{ V0.0 ??-???-?? Don Scelza  started.
{-----------------------------------------------------------------------}


{********************} Exports {********************}


imports Ether10IO from Ether10IO;
    
Type

   FTPPacket = Record
       Cmd: Char;
       ByteCount: Integer;
       CheckSum: Integer;
       Case Integer Of
           1: (Buffer: Packed Array [0..255] of Char);
           2: (ErrMsg: String);                       { Byte 0 is length byte }
           3: (SrcFile: String;
               DestFile: String);
           4: (Name: String);
           5: (Add: EtherAddress);
       End;
        
   ErrStatus = (OK, TimeOut, ChkSumErr, RawIOErr);
    
   TransMode = (PERQPERQ, PERQ11, PERQVAX);    { Tells what machines }
                                               { are involved in the transfer }
    
    ByteIntRecord = Packed Record
        Case Integer Of                        { Used to get low order }
            1: (Whole: Integer);               { byte for checksums }
            2: (Lower: 0..255;
                Upper: 0..255);
        End;
        
    DevTypes =  { Valid transfer devices }
               (FTP_RSA, FTP_RSB, FastEther, EtherNet);      
    

Function  FTPGetFile(SrcFile,DestFile: String;  IsItText:Boolean;
                     Dev: DevTypes; Mode: TransMode): Boolean;
Function  FTPPutFile(SrcFile,DestFile: String;  IsItText:Boolean;
                     Dev: DevTypes; Mode: TransMode): Boolean;
Procedure FTPChkDev(Dev: DevTypes);
Procedure SendStopVax;
Procedure FTPInit;
Function  FTPAddRequest(Name: String;  Dev: DevTypes): Boolean;
procedure FTPSetEtherAddr(Addr: EtherAddress; Dev: DevTypes);      
procedure FTPSetMyAddr(Dev: DevTypes);
procedure FTPQuitNet;
function  FTPDoEPacket(pPkt: Pointer; pHdr: Pointer): Boolean;
procedure ShowMemUsage;


var MyAddr, HisAddr: EtherAddress;

CONST
      MAXALIAS = 10;

VAR NumAlias : 0..MAXALIAS;

Var MyName: Array[1..MAXALIAS] Of String;
    HisName : String;

const FastEType = 1;
      ByteType =  0;

const MaxRecv = 4;


{*******************} Private {********************}



{------------------------------------------------------------------------
{
{
{ The current devices supported by this code are:
{    FTP_RSA, FTP_RSB, FastEtherNet (EtherNet to the user)
{    and Ethernet (ByteStreamNet to the user)
{
{ 
{ **** HACKS that you should know about ****
{
{    FTPPutFile contains a assumption about how fields are allocated in
{    a record.   Watch out when not running on UCSD or PERQ.
{
{    The Ethernet portions of this program have been hacked in.  The device
{    EtherNet works in a fairly reasonable manner.  At each PutByte or getByte
{    bytes are copied to or from an Ethernet buffer.  Everything else works the
{    the same.  The device FastEther is not so clean.  It takes advantage of
{    how fields in records are allocated.  It uses absolute offsets into
{    buffers.  It goes directly to the file system.  All of this was done for
{    speed.  If you must make changes to the ethernet code be careful.
{
{
{ Protocols:
{
{    The protocol implemented by this code is very simple.   Any machine
{ can start a transfer.  However onces a transfer is started it is the
{ machine that is on the receiving end that controls the transfer.
{
{    To read a file a machine will send either a ReadT or ReadB packet.
{ The machine that receives the packet must reply with an Ack if it is
{ able to supply the file or an Abort if there was some error.  In this
{ case the ErrMsg portion of the Abort packet will contain an error message
{ that describes why the abort was sent.  At this point we can start the
{ data transfer.  The machine that requested the read, now the master of
{ the transfer, will send a ReadBlk packet.  The machine that is to supply
{ the data can send an Ack packet that contains the next block of data,
{ an Abort packet if there was some error  or an FTPEOF packet if there is
{ no more data left in the file.  If the packet was an Ack then the
{ master machine will reply with another ReadBlk packet.  If either of the
{ other two packets was received then the master will reply with an Ack and
{ terminate the transfer.
{
{    To write a file a machine will send either an AskReadT or AskReadB packet.
{ The machine that receives this packet will send an Ack if it is able to
{ fulfill the request or an Abort if there is some problem.  Again the 
{ ErrMsg field of the Abort will contain an error message.  At this point
{ the machine that received the write request will become master of
{ the transaction by sending either a ReadT or ReadB packet.  The machine that
{ sent the initial AskReadT or AskReadB packet must now handle the read request
{ just as if it were any other read request.  In this manner control of the
{ transaction is turned over to the machine that is receiving the data.
{
{    If there are any errors the machine that determined that an error
{ occured will send an Abort packet.  The other machine that is involved
{ in the transfer must send an ACK and the terminate its portion of the
{ transfer.
{
{-------------------------------------------------------------------------}
Imports SystemDefs From SystemDefs;
Imports IOErrors From IOErrors;
Imports IO_Unit From IO_Unit;
Imports FileSystem from FileSystem;
Imports UtilProgress from UtilProgress;
Imports IO_Others From IO_Others;
Imports Memory from Memory; 
Imports AlignMemory from AlignMemory;
Imports FileUtils from FileUtils;
Imports CmdParse from CmdParse;
Imports Stream from Stream;
imports FileTypes from FileTypes;   {for DirFile}


type MyEtherBuff = packed record
        case boolean of
            true:  (Reg: EtherBuffer);
            false: (Byte: packed array[0..1499] of char);
        end;
     pMyEtherBuff = ^MyEtherBuff;
        
    PtrToField = record
        case boolean of
           true: (P: pDirBlk);
           false:( Offset: integer;
               Segment: integer);
        end;

Var
    CurDevice: DevTypes;          { Type of the current transfer device }
    CurTMode: TransMode;          { Tells us what machine is at the other }
                                  { end of the line. }
    CurChar: Char;                { The current char from the input device }
    CurCharValid: Boolean;        { Tells us if CurChar is valid }
    WaitP: Char;                  { Type of packet that we are waiting for. }
    IsText: Boolean;              { True if current transfer is of a }
                                  { text file }
    IOErr: Integer;               { Raw I/O Device error number }
    CursIncr: Integer;            { Number of line to increment the cursor. }

    NetBuffAllocated: boolean;    { True if network buffers allocated.}
    Ethertransfer:    boolean;    { True if doing Block transfers on net. }
    PacketValid: boolean;         { True if the current packet is good. }
    SBuffIndex: integer;          { Index of next outgoing char in net buffer.}
    RBuffIndex: integer;          { Index of next incoming char in net buffer.}
    SendBuff: pMyEtherBuff;
    SendHeader: pEtherHeader;
    SendStatus: pEtherStatus;
    AddPtr: pEtherAdRec;
    EtherSeg: integer;
    PointerConversion: PtrToField;
    HaveAddr, HaveHisAddr: boolean;

Const
    Debug = false;
    DoDirectory = true;       { permits formatting directory }
    ShowMem = False;          { check to see if free memory has been absorbed}
    Verbose = True;           { Print some things to tell us all what is }
                              { going on. }
    ShowHash = True;          { Print # and ! }
    CheckCheck = True;        { Use calculated checksums }
    CheckChar = '*';          { If CheckCheck is false then this must be }
                              { the character that is in the checksum byte }
    ClearBuff = True;         { Try to clear buffers }

    TimeOutCount = 500;       { Square root of the timeout counter limit }
    
    RealTimeOut = 1200;       { 10 real seconds. uses 60hz clock.}
    Delaycount = 250;         { Delay counter for PDP-11 delay.  Slow down }
                              { PERQ's sending of characters. }
    MaxPacSize = 507;         { Max number of data bytes in a packet }
                              
    ReadT =      'A';         { Read a text file packet }
    AskReadT =   'B';         { Ask for a text read packet packet }
    ReadB =      'C';         { Read a binary file packet }
    AskReadB =   'D';         { Ask for a binary read packet packet }
    Abort =      'E';         { Abort packet }
    Ack =        'F';         { Ack packet }
    FTPEOF =     'G';         { EOF packet }
    ReadBlk =    'H';         { Ask for the next block of data. }
    StopVax =    'I';         { Stop job on Vax and return to normal mode }
    CSumError =  'J';         { There was a checksum error in the packet. }
    AddRequest = 'K';         { Request for ethernet address. }
    AddReply =   'L';         { Ethernet address reply }

    EOLN_Char = chr(#12);     { sent for end of line in text mode }
    CH_Return = chr(#15);     { ^M }
    CH_LF     = chr(#12);     { ^J }
    
    BytesInBlock = 512;
    StreamDataBytesInPacket = 252;
    EtherDataBytesInPacket = BytesInBlock;
    StreamPacketsPerBlock = BytesInBlock div StreamDataBytesInPacket;
    EtherPacketsPerBlock = BytesInBlock div EtherDataBytesInPacket;
    StreamFudge = 2;
    EtherFudge = 1;

{$IFC Debug Then} {$Message Debug on} {$ElseC} {$Message Debug off} {$Endc}
{$IFC Verbose Then} {$Message Verbose mode} 
    {$ElseC} {$Message Non-Verbose mode} {$Endc}
{$IFC ShowHash Then} {$Message ShowHash on} 
    {$ElseC} {$Message ShowHash off} {$Endc}
{$IFC CheckCheck Then} {$Message Checksums on} 
    {$ElseC} {$Message Checksums off} {$Endc}
{$IFC ClearBuff Then} {$Message ClearBuff on} 
    {$ElseC} {$Message ClearBuff off} {$Endc}

{$ifc debug then} imports NetStatus from NetStatus; {$endc}

type RecvRec = record
        Buff: pMyEtherBuff;
        Stat: pEtherStatus;
        Head: pEtherHEader;
        InUse: boolean;
        end;

var CurRecv: RecvRec;
    RecvArray: array[0..MaxRecv] of RecvRec;
    RecvIndx: integer;

    
Procedure WritePacket(Var Packet: FTPPacket;  PacketType: Char;
                      ValidBytes: Integer;  Var ErrCode: ErrStatus); Forward;



procedure ShowMemUsage;
{------------------------------------------------------------------
{ Abstract:
{   Check utilization of StreamSegment space.
{   It should not be continuously consumed by poll/put.
{-------------------------------------------------------------------}
var First, This, Last: MMAddress;
    FirstS, ThisS: SegmentNumber;
    memory: MMPointer;
    MemFree: long;
begin { ShowMemUsage }
   if ShowMem then begin
      MemFree := 0;
      FirstS := StreamSegment;
      ThisS := FirstS;
{$R-}
      repeat
        if SAT^[ThisS].Heap then { heap, advance to next segment in list }
          ThisS := SIT^[ThisS].HeapNext;
        memory.Segmen := ThisS;
        memory.Offset := 0;
        with SAT^[ThisS], SIT^[ThisS], memory.m^ do
          if not Full then begin
              First := Freelist;
              This := First;
              repeat { scan the freelist }
                Last := This;
                This := m[This].N;
                MemFree := MemFree + m[This].L;
              until This = First;
          end;
      until ThisS = FirstS;
{$R=}    
      writeln(' ':55, 'Free mem: ', MemFree:1);
   end;
end;  {ShowMemUsage}


procedure PostAllReceives;
{------------------------------------------------------------------
{ Abstract:
{   Reset the ether net and then post receives to all buffers.
{-------------------------------------------------------------------}
var
   I: integer;
begin
        E10Reset(Addptr);
        RecvIndx := 0;
        for I := 0 to (MaxRecv - 1) do begin
            {$ifc debug then} write('R'); {$endc}
            E10IO(EReceive, RecvArray[I].Head, 
                            recast(RecvArray[I].Buff, pEtherBuffer),
                            RecvArray[I].Stat, 0);
        end;
        {$ifc debug then} writeln; {$endc} 
end;

procedure NetPost;
{------------------------------------------------------------------
{
{ Abstract:
{   Post all need receives.
{
{-------------------------------------------------------------------}
  var I: integer;
    begin
    if (CurDevice <> EtherNet) and (CurDevice <> FastEther) then 
        exit(NetPost);
    {$ifc debug then} writeln('Net post in'); {$endc}
    I := (RecvIndx + 1) mod MaxRecv;
    while I <> RecvIndx do
        begin
        if not RecvArray[I].Stat^.CmdInProgress then
            begin
            {$ifc Debug then} writeln('  Receive at index ', I:1); {$endc}
            E10IO(EReceive, RecvArray[I].Head, 
                            recast(RecvArray[I].Buff, pEtherBuffer),
                            RecvArray[I].Stat, 0);
            end;
        I := (I + 1) mod MaxRecv;
        end;
    {$ifc debug then} writeln('Net post out'); {$endc}
    end;

procedure NetRelCur;
{------------------------------------------------------------------
{
{ Abstract:
{   Release the current network receive buffer if
{   it is not in use.
{
{-------------------------------------------------------------------}
    begin
    if (CurDevice <> EtherNet) and (CurDevice <> FastEther) then 
        exit(NetRelCur);
    {$ifc debug then} write('NetRelCur at index ', RecvIndx:1); {$endc}
    if (not RecvArray[RecvIndx].Stat^.CmdInProgress) then 
        begin 
        {$ifc debug then} writeln(' release'); {$endc}
        RecvArray[RecvIndx].InUse := false;
        RecvIndx := (RecvIndx + 1) mod MaxRecv;
        end
    else
        begin
        {$ifc debug then} writeln(' pending'); {$endc}
        end;
    end;    

procedure NetGetCur;
{------------------------------------------------------------------
{
{ Abstract:
{   Get the current receive buffer for the network.
{   Release the old "current" if it is not in use then
{   post any needed receives.
{
{-------------------------------------------------------------------}
    begin
    if (CurDevice <> EtherNet) and (CurDevice <> FastEther) then 
        exit(NetGetCur);
    {$ifc debug then} 
        write('NetGetCur at index ', RecvIndx:1, ' Stat '); 
        PrintPOinter(RecvArray[RecvIndx].Stat);
        writeln;
    {$endc}
    RecvArray[RecvIndx].InUse := true;
    CurRecv := RecvArray[RecvIndx];
    end;

function SwapByte(Wrd: integer): integer;
{-------------------------------------------------------------
{
{ Abstract:
{   This function is used to swap the two bytes of a word.
{
{ Parameters:
{   Wrd is the word that we are to swap the bytes of.
{
{ Results:
{   Return Wrd with the bytes swapped.
{
{-----------------------------------------------------------}
    begin 
    SwapByte := lor(shift(Wrd, -8), shift(Wrd, 8));
    end;


procedure FTPQuitNet;
{-------------------------------------------------------------------
{
{ Abstract:
{   Shut down the net, if needed.
{
{-------------------------------------------------------------------}
    begin
    if NetBuffAllocated then E10Reset(Addptr);
    end;


procedure NetAllocate;
{----------------------------------------------------------------------
{
{ Abstract:
{   Allocate network buffers and set up packet headers.
{   If buffers are already allocated just set up headers.
{
{---------------------------------------------------------------------}
  var SegSize, I: Integer;
    begin
    {$ifc debug then} 
        write('NetAllocate  Dev= ', Ord(CurDevice):1, '  BuffAllocated= ', 
               NetBuffAllocated, '  ');
    {$endc}
    if not HaveAddr then
        begin
        MyAddr := E10GetAdr;
        HaveAddr := true;
        end;

    if CurDevice = FastEther then 
        EtherTransfer := true
    else
        EtherTransfer := false;

    if not NetBuffAllocated then 
        begin
        {$ifc debug then} write('Allocate'); {$endc}
        for I := 0 to (MaxRecv - 1) do
            NewBuffer(recast(RecvArray[I].Buff, AlignedPointer), 4, 4);
        NewBuffer(recast(SendBuff, AlignedPointer), 4, 4);
        SegSize := ((((MaxRecv + 1) * WordSize(EtherHeader)) + 
                       WordSize(EtherAdRec) + 
                       ((MaxRecv + 1) * WordSize(EtherStatus))) div 256) + 1;
        CreateSegment(EtherSeg, SegSize, 1, SegSize);
        new(EtherSeg, 8, SendHeader);
        for I := 0 to (MaxRecv - 1) do
            begin
            new(EtherSeg, 8, RecvArray[I].Head);
            new(EtherSeg, 2, RecvArray[I].Stat);
            end;
        new(EtherSeg, 2, SendStatus);
        new(EtherSeg, 1, AddPtr);
        SetMobility(EtherSeg, UnMovable);
        end;

    if CurDevice = FastEther then
        SendHeader^.EType := FastEType
    else
        SendHeader^.EType := ByteType;

    SendHeader^.Dest.High := SwapByte(HisAddr.High);
    SendHeader^.Dest.Mid :=  SwapByte(HisAddr.Mid);
    SendHeader^.Dest.Low :=  SwapByte(HisAddr.Low);
    SendHeader^.Src.High :=  SwapByte(MyAddr.High);
    SendHeader^.Src.Mid :=   SwapByte(MyAddr.Mid);
    SendHeader^.Src.Low :=   SwapByte(MyAddr.Low);
    
    if not NetBuffAllocated then
        begin
        AddPtr^.LowAddress := SendHeader^.Src.Low;
        AddPtr^.MCB := MltCstNone;
        PostAllReceives;
        end;
    NetBuffAllocated := true;
    {$ifc debug then} writeln('   Return'); {$endc}
    end;    


Function GetByte(Var Ch:Char): ErrStatus;
{---------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to read a byte from the device specified by
{    CurDev.
{
{ Parameters:
{    On return Ch will hold the character that was read.
{
{ Environment:
{    This procedure assumes that CurCharValid has a valid value.
{    It is is True then assume that the current character is in
{    CurChar.  If it is False then we must read a character from
{    the device.
{
{ Results:
{    This procedure returns a standard error code.  OK means that there
{    were no errors.
{
{ Side Effects:
{    This procedure will change Ch.  It may also change CurCharValid. 
{
{ Errors:
{    The errors that may occur during the execution of this procedure deal
{    with the Raw I/O.  They are:
{        TimeOut and RawIoErr.
{
{ Design:
{-------------------------------------------------------------------------}
Var
    Count1,Count2:Integer;
    InByte,Result:Integer;
    Done, Good: Boolean;
    InCh: Char;
    
    Begin {GetByte}
    
{
{ See if there is a valid character in CurChar.  If so then just return it.
{ }

    If CurCharValid Then
        Begin
        GetByte:=OK;
        Ch:=CurChar;
        CurCharValid:=False;
        Exit(GetByte);
        End;
        
{
{ If we get here then we must try to read a character. 
{ }

    Count1:=0;
    Repeat
        Count2:=0;
        Repeat
            Begin
            Case CurDevice Of
                FTP_RSA: Result:=IOCRead(RSA,Ch);
                FTP_RSB: Result:=IOCRead(RSB,Ch);
                
                ETHERNET: begin
                    Ch := CurRecv.Buff^.Byte[RBuffIndex];
                    RBuffIndex := RBuffIndex + 1;
                    Result := IOEIOC;
                    end;
                End {Case};
                
            Count2:=Count2+1;
            End
          Until (Count2=TimeOutCount) Or (Result=IOEIOC);
        Count1:=Count1+1    ;
      Until (Count1=TimeOutCount) Or (Result=IOEIOC);
      
    If Count1 = TimeOutCount Then
        begin
        GetByte:=TimeOut;
        end
    Else
        Begin
        Ch := Chr(Land(Ord(Ch), #377));
        GetByte:=OK;
        End;
         
    End {GetByte};

Function PutByte(Ch:Char): ErrStatus;
{---------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to write a byte to the device specified by
{    CurDev.
{
{ Parameters:
{    Ch is the character that is to be written.
{
{ Results:
{    This procedure returns a standard error code.  OK means that there
{    were no errors.
{
{ Side Effects:
{    None
{
{ Errors:
{    The errors that may occur during the execution of this procedure deal
{    with the Raw I/O.  They are:  TimeOut and RawIOErr.
{
{ Design:
{-------------------------------------------------------------------------}
Var
    Count1,Count2:Integer;
    InByte,Result,Dum,Index: Integer;
    Done, Good: Boolean;
    Begin {PutByte}

    Count1:=0;
    Repeat
        Count2:=0;
        Repeat
            Begin
            
            Case CurDevice Of
                FTP_RSA: Begin
                    If (CurTMode = PERQ11) or (CurTMode = PERQVAX) Then
                        For Dum := 0 To DelayCount Do ;
                    Result:=IOCWrite(RSA,Ch);
                    End;
                FTP_RSB: Begin
                    If (CurTMode = PERQ11) or (CurTMode = PERQVAX) Then
                        For Dum := 0 To DelayCount Do ;
                    Result:=IOCWrite(RSB,Ch);
                    End;
                    
                ETHERNET: begin
                    SendBuff^.Byte[SBuffIndex] := Ch;
                    SBuffIndex := SBuffindex + 1;
                    Result := IOEIOC;
                    end;
                End {Case};
                
            Count2:=Count2+1;
            End
          Until (Count2=TimeOutCount) Or (Result=IOEIOC);
        Count1:=Count1+1;
      Until (Count1=TimeOutCount) Or (Result=IOEIOC);
      
    If Count1 = TimeOutCount Then
        PutByte:=TimeOut
    Else
        PutByte:=OK;
    
    End {PutByte};
    
    
    
Procedure ReadPacket(DoCheck: boolean;  Var Packet: FTPPacket;  
                     Var ErrCode:ErrStatus);
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to read a packet from the device specified
{    by CurDevice.
{
{ Parameters:
{    DoCheck tells if we should check Sender on the packet.
{    This is used to dump packets that are not part of the
{    current transaction.
{
{    Packet is the packet that we are to fill in. 
{
{    ErrCode is used to return an indication of the outcome of the request.
{
{ Side Effects:
{    This procedure will change FTPPacket and ErrCode.
{
{ Errors:
{    The errors that may occur during the execution of this procedure all
{    deal with Raw I/O transmition errors.  The errors that may occur are:
{        TimeOut, RawIOErr, ChkSumErr, and OK.
{
{ Design:
{--------------------------------------------------------------------------}
Var Ch: Char;
    I, Count1, Count2: Integer;
    Check:ByteIntRecord;
    ErrPacket: FTPPacket; 
Label 1;

    Begin {ReadPacket} 
    
    {$ifc debug then} writeln('ReadPacket ');  write('    ');{$endc}
    
{
{ Read the packet.
{ } 

    Count1 := 0;
    Count2 := 0;
1:  if (CurDevice = EtherNet) or (CurDevice = FastEther) then
            begin
            if not PacketValid then
                begin
                NetGetCur;
                repeat
                    Count1 := Count1 + 1;
                    if Count1 = TimeOutCount then
                        begin
                        Count1 := 0;
                        Count2 := Count2 + 1;
                        end;
                until (Count2 = TimeOutCount) or 
                        (not CurRecv.Stat^.CmdInProgress);
                if Count2 = TimeOutCount then
                    begin
                    PacketValid := false;
                    ErrCode := TimeOut;
                    NetRelCur;
                    NetPost;
                    {$ifc debug then} writeln('Timeout'); {$endc}
                    exit(ReadPacket);
                    end;
                {$ifc debug then} write('Recv Complete '); {$endc}
                end
            else
                PacketValid := false;

            if ((SendHeader^.Dest.High <> CurRecv.Head^.Src.High) or
                (SendHeader^.Dest.Mid <> CurRecv.Head^.Src.Mid)   or
                (SendHeader^.Dest.Low <> CurRecv.Head^.Src.Low))
                                                                and
               ((SendHeader^.Dest.High <> -1) or 
                (SendHeader^.Dest.Mid <> -1)  or
                (SendHeader^.Dest.Low <> -1)) 
                                                                and
               DoCheck                                          then
                        begin
                        PacketValid := false;
                        If Verbose and ShowHash Then Write('A');
                        NetRelCur;
                        NetPost;
                        goto 1;
                        end;

            if (CurDevice = Ethernet) then
                begin
                if CurRecv.Head^.EType <> ByteType then
                    begin
                    PacketValid := false;
                    If Verbose and ShowHash Then Write('T');
                    NetRelCur;
                    NetPost;
                    goto 1;
                    end;
                end
            else if (CurDevice = FastEther) then
                begin
                if CurRecv.Head^.EType <> FastEType then
                    begin
                    PacketValid := false;
                    If Verbose and ShowHash Then Write('t');
                    NetRelCur;
                    NetPost;
                    goto 1;
                    end;
                end;
            end;


    With Packet Do
        Begin
        
        if not Ethertransfer then
            begin
            RBuffIndex := 0;        
            ErrCode:=GetByte(Ch);
            If ErrCode <> Ok Then 
                begin
                {$ifc debug then} writeln('Bad Cmd'); {$endc}
                NetRelCur;
                NetPost;
                Exit(ReadPacket);
                end;
            Cmd:=Ch;
            
            ErrCode:=getByte(Ch);
            If ErrCode <> Ok Then 
                begin
                {$ifc debug then} write('Bad ByteCount'); {$endc}
                NetRelCur;
                NetPost;
                Exit(ReadPacket);
                end;
            ByteCount:=Ord(Ch);
            
            CheckSum:=Ord(Cmd) + ByteCount;
        
            For I:= 0 To ByteCount-4 Do     { Cmd, ByteCnt, CheckSum, 0 index }
                begin
                ErrCode:=GetByte(Ch);
                If ErrCode <> Ok Then 
                    begin
                    {$ifc debug then} write('Bad Checksum read'); {$endc}
                    NetRelCur;
                    NetPost;
                    Exit(ReadPacket);
                    end;
                CheckSum:=CheckSum + Ord(Ch);
                Buffer[I]:=Ch;
                End;
                
            ErrCode:=GetByte(Ch);            { The checksum byte }
            If ErrCode <> Ok Then 
                begin
                {$ifc debug then} write('Bad Checksum byte'); {$endc}
                NetRelCur;
                NetPost;
                Exit(ReadPacket);
                end;
            end
        else
            begin
            Cmd := CurRecv.Buff^.Byte[0];
            ByteCount := CurRecv.Buff^.Reg[1];
            end;
        
{
{ Check the checksum.
{ }

        CheckSum:=CheckSum + Ord(Ch);
        If CheckCheck Then
            Begin
            Check.Whole:=CheckSum;
            CheckSum:=Check.Lower;

            If EtherTransfer Then
               Begin
               If (CurRecv.Stat^.CRCError Or CurRecv.Stat^.Collision) Then
                   Begin
                   If Verbose And ShowHash Then Write('c');
                   WritePacket(ErrPacket, CSumError, 0, ErrCode);
                   If ErrCode <> OK Then Exit(ReadPacket);
                    NetRelCur;
                    NetPost;
                   GoTo 1;
                   End
               End 
            Else If CheckSum <> 0 Then 
                Begin
                If Verbose And ShowHash Then Write('C');
                NetRelCur;
                NetPost;
                WritePacket(ErrPacket, CSumError, 0, ErrCode);
                If ErrCode <> OK Then Exit(ReadPacket);
                GoTo 1;
                End;
            End
        Else
            Begin
            If Ch <> CheckChar Then
                Begin
                NetRelCur;
                NetPost;
                ErrCode:=ChkSumErr;
                Exit(ReadPacket);
                End;
            End;
        End {With};
            
    ErrCode:=OK;
    ShowProgress(CursIncr);
    If Verbose and ShowHash Then Write('#');
    {$ifc debug then} writeln('Exit readpacket.'); {$endc}
   End {ReadPacket};
    
    
    
Procedure WritePacket(Var Packet: FTPPacket;  PacketType: Char;
                      ValidBytes: Integer;  Var ErrCode: ErrStatus);
{---------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to write a packet.  It will build the packet
{    filling in the control fields.  The only portion of the packet that
{    is to be sent that needs to be valid is the data bytes.
{
{ Parameters:
{    Packet is the packet that we are to send.
{
{    PacketType is the type of packet that is to be sent.  This value will
{    be placed into the Cmd byte of the packet.
{
{    ValidBytes is a count of the number of valid data bytes in the packet.
{
{    ErrCode is used to return error information.
{
{ Environment:
{    The data bytes of Packet must be valid.
{
{ Side Effects:
{    This procedure will change error code.
{
{ Errors:
{    The errors that can occur during the execution of this procedure
{    are TimeOut and RawIOErr.
{
{ Design:
{-------------------------------------------------------------------------}
Var I, SegSize: Integer;
    CnvRec: ByteIntRecord;

    Begin {WritePacket}

    {$ifc debug then} writeln('WritePacket ');  write('    '); {$endc}
    
    With Packet Do
        Begin

        if not EtherTransfer then
            begin
            SBuffIndex := 0;
            Cmd:=PacketType;
            ByteCount:= ValidBytes + 3;
            CheckSum:= Ord(Cmd) + ByteCount;
        
            ErrCode:=PutByte(Cmd);
            If ErrCode <> Ok Then 
                begin
                {$ifc debug then} writeln('Put Cmd  Exit WritePacket'); {$endc}
                Exit(WritePacket);
                end;
        
            ErrCode:=PutByte(Chr(ByteCount));
            If ErrCode <> Ok Then 
                begin
                {$ifc debug then} 
                    writeln('Put ByteCount  Exit WritePacket'); 
                {$endc}
                Exit(WritePacket);
                end;

        
            For I:= 0 To (ValidBytes - 1) Do
                Begin
                CheckSum:=CheckSum + Ord(Buffer[I]);
                ErrCode:=PutByte(Buffer[I]);
                If ErrCode <> Ok Then Exit(WritePacket);
                End;
            CheckSum:= -CheckSum;
            CnvRec.Whole:=CheckSum;         { Get the low byte of the checksum }
            If CheckCheck Then
                ErrCode:=PutByte(Chr(CnvRec.Lower))
            Else
                ErrCode:=PutByte(CheckChar);
            end
        else
           begin
           SendBuff^.Byte[0] := PacketType;
           SendBuff^.Reg[1] := ValidBytes + 3;
           end; 
        End {With};

        if (CurDevice = Ethernet) or (CurDevice = FastEther) then
            begin
            {$ifc debug then} write('ESend '); {$endc}
            if (ValidBytes + 4) < MinDataBytes then ValidBytes := MinDataBytes; 
            E10WIO(ESend, SendHeader, recast(SendBuff, pEtherBuffer), 
                SendStatus, ValidBytes + 4);
            if SendStatus^.CRCError or SendStatus^.Collision then
                ErrCode := ChkSumErr
            else
                ErrCode := OK;
            end;

    If Verbose and ShowHash Then Write('!');
    {$ifc debug then} writeln('Exit WritePacket '); {$endc}

    End {WritePacket};
    
    

Procedure SendAck(NumBlock: Integer;  Var ErrCode: ErrStatus);
{-------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to send an Ack.
{
{ Parameters:
{    NumBlock is the number of data blocks that will make up the
{    file to be transmited.  This is only used in the first ack sent
{    to the other machine in the transfer.   0 means no data.
{
{    ErrCode is used to return error information.
{
{ Side Effects:
{    This procedure will change ErrCode.
{
{ Errors:
{  The following errors may occur:  TimeOut, RawIOErr.
{
{ Design:
{------------------------------------------------------------------------}
Var Packet: FTPPacket;
    ByteInt: ByteIntRecord;
    NumBytes: Integer;

    Begin {SendAck}
    {$ifc debug then} writeln('SendAck In '); {$endc}
    
    NumBytes := 0;
    If NumBlock <> 0 Then
        Begin
        if not EtherTransfer then
            begin
            ByteInt.Whole := NumBlock;
            Packet.Buffer[0] := Chr(ByteInt.Lower);
            Packet.Buffer[1] := Chr(ByteInt.Upper);
            end
        else
            begin
            SendBuff^.Reg[2] := NumBlock;
            end;
        NumBytes := 2;
        End;
    WritePacket(Packet, Ack, NumBytes, ErrCode);
    {$ifc debug then} writeln('SendAck out '); {$endc}
    End {SendAck};
    
    
    
    
Procedure SendAbort(Msg: String;  Var ErrCode: ErrStatus);
{-------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to send an Abort.
{
{ Parameters:
{    Msg is the error message that is to be sent to the machine.
{
{    ErrCode is used to return error information.
{
{ Side Effects:
{    This procedure will change ErrCode.
{
{ Errors:
{  The following errors may occur:  TimeOut, RawIOErr.
{
{ Design:
{------------------------------------------------------------------------}
Var Packet: FTPPacket;
    I: Integer;

    Begin {SendAbort}
    {$ifc debug then} writeln('SendAbort In '); {$endc}
    if ((CurDevice = Ethernet) or (CurDevice = FastEther)) and
         (not NetBuffAllocated) then NetAllocate;
    if not EtherTransfer then
        Packet.ErrMsg:=Msg
    else
        {$R-}
        begin
        for I := 0 to length(Msg) do
            SendBuff^.Byte[I + 4] := Msg[I];
        end;
        {$R=}
    WritePacket(Packet, Abort, Ord(Msg[0]) + 1, ErrCode);
    {$ifc debug then} writeln('SendAbout Out '); {$endc}
    End {SendAbort};
    
    
    
Function WaitPacket(Var Packet: FTPPacket;  PacketType: Char;
                    Var ErrCode: ErrStatus): Boolean;  Forward;
                    
                    
                                                                           

Procedure FTPErr(ErrCode: ErrStatus;  Packet: FTPPacket);
{----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is uced to print an error message on the users terminal.
{
{ Parameters:
{    ErrCode contains the error information the we are to give to the
{    user.
{
{    Packet is the packet that was being used when the error occurred.
{    For some errors there will be additional information in the packet.
{
{ Design:
{    If ErrCode is OK then we are to use the Packet to drive the
{    error reporting.  An example of this is when an Abort packet
{    is received this procedure will be called to print the error
{    information in the packet.
{
{    If the error was anything except a TimeOut or Abort received then
{    send an Abort and wait for a reply.
{
{--------------------------------------------------------------------------}
Var I: Integer;
    TmpPacket: FTPPacket;
    TmpErrCode: ErrStatus;
    ErrMsg: String;

    Begin {FTPErr}
    Write('** FTP Error:');


{
{ See if we were called because of a transmision error or a 
{ protocol error.
{ }

    If ErrCode <> Ok Then
        Begin
        
        Case ErrCode Of
            TimeOut: ErrMsg:=' Timeout error.';
            ChkSumErr: ErrMsg:='  Checksum Error.';
            RawIOErr: ErrMsg:='  Raw I/O Error.';
            End {Case};
            
        WriteLn(ErrMsg);
        If ErrCode <> TimeOut Then
            Begin
            SendAbort(ErrMsg,TmpErrCode);
            If Not WaitPacket(TmpPacket,Ack,TmpErrCode) Then
                WriteLn('** FTP Error: Could not ABORT transfer.');
            NetRelCur;
            NetPost;
            End;
        if Packet.Cmd = StopVax then
            writeln('** Could not reset VAX line.');
        End
    Else
        Begin
        With Packet Do
            Begin
            Write('Packet of type ');
            
            Case Cmd Of
                ReadT: Write('ReadText ');
                AskReadT: Write('AskReadText ');
                ReadB: Write('ReadBinary ');
                AskReadB: Write('AskReadBinary ');
                Abort: Write('Abort ');
                Ack: Write('ACK ');
                FTPEOF: Write('FTPEOF ');
                StopVax: Write('StopVax ');
                CSumError: Write('CheckSum error ');
                AddRequest: Write('AddressRequest ');
                AddReply: Write('AddressReply ');
                End {Case};
                
            Writeln('read.');
            If Cmd <> Abort Then
                Begin
                Write('** Waiting for a ');
                Case WaitP of
                    ReadT: Write('ReadText ');
                    AskReadT: Write('AskReadText ');
                    ReadB: Write('ReadBinary ');
                    AskReadB: Write('AskReadBinary ');
                    Abort: Write('Abort ');
                    Ack: Write('ACK ');
                    FTPEOF: Write('FTPEOF ');
                    StopVax: Write('StopVax ');
                    CSumError: Write('CheckSum error ');
                    AddRequest: Write('AddressRequest ');
                    AddReply: Write('AddressReply ');
                    end;
                writeln( ' packet.');
                WriteLn('** Protocol Error.');
                SendAbort('Protocol error!!',TmpErrCode);
                If Not WaitPacket(TmpPacket,Ack,TmpErrCode) Then
                    WriteLn('** FTP Could not ABORT transfer.');
                End
            Else
                Begin
                WriteLn;
                Write('** Remote machine sent - ');
                if not Ethertransfer then
                    writeln(ErrMsg)
                else
                    begin
                    for I := 0 to ord(CurRecv.Buff^.Byte[4]) - 1 do
                        write(CurRecv.Buff^.Byte[I + 5]);
                    writeln;
                    end;
                SendAck(0, TmpErrCode);
                End;
            End {With};
        End; 
    if ((CurDevice = FastEther) or (CurDevice = Ethernet)) and 
             NetBuffAllocated                              then
        begin
        {$ifc debug then} write('FTPError FLUSH ');  {$endc}       
        PostAllReceives;
        end;
    End {FTPErr};
    

Procedure SendStopVax;
{---------------------------------------------------------------------
{                                                                     
{ Abstract:
{   This procedure is used to send a StopVax packet.
{ 
{--------------------------------------------------------------------}

Var
    Packet  :FTPPacket;
    ErrCode :ErrStatus;

    Begin
    {$ifc debug then} writeln('SendStopVax In '); {$endc}
   if (CurDevice = Ethernet) or (CurDevice = FastEther) then NetAllocate;
    WritePacket(Packet,StopVax,0,ErrCode);
    If ErrCode <> Ok Then
        Begin
        FTPErr(ErrCode,Packet);
        End;
    {$ifc debug then} writeln('SendStopVax Out '); {$endc}
    End;
    
    
Function WaitPacket;
{------------------------------------------------------------------------
{
{ Abstract:
{    This function is used to wait for a packet of a specific type.
{    It will read the next packet that comes in on the I/O device.
{
{ Parameters:
{    On return Packet will contain the packet that was read.
{
{    PacketType is the type of packet that we are looking for.
{
{    ErrCode is used to return error information.
{
{ Resutls:
{    This function will return True if the next packet that is read
{    is of type PacketType.   It will return False if the packet was not
{    of type PacketType or if there was some error reading the packet.
{    If the packet was read correctly but not of PacketType then ErrCode
{    will contain OK and Packet will contain the packet that was read.
{
{ Side Effects:
{    This procedure will change Packet and ErrCode.
{
{ Errors:
{    The errors that can occur in this function are of two types.
{    First,  if the packet that was read was not of PacketType then
{    ErrCode will be OK and the return value will be false.  Second,
{    if there was an error reading the packet ErrCode will contain the
{    error status and the function will return false.
{
{ Design:
{-----------------------------------------------------------------------}

handler HelpKey(var RetStr: Sys9s);
    begin
    {$ifc debug then}
        write('Wait on ');
        PrintPointer(CurRecv.Stat);
        writeln;
    {$elsec}
        raise HelpKey(RetStr);
    {$endc}
    end;

    Begin {WaitPacket}
    WaitP:=PacketType;
    
{
{ See if we can read a packet.
{ }
    
    ReadPacket(true, Packet,ErrCode);
    If ErrCode <> Ok Then
        Begin
        WaitPacket:=False;
        Exit(Waitpacket);
        End;

{
{ If we get here then we have a valid packet.  See if it is of the
{ correct type.
{ }

    If Packet.Cmd <> PacketType Then
        WaitPacket:=False
    Else
        Waitpacket:=True;
        
    End {WaitPacket};
    
    
    
Function ReadFile(SrcFile,DestFile: String;  IsText: Boolean): Boolean;
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to read a file from another machine and
{    write it on the current machine.
{
{ Parameters:
{    SrcFile is the name of the file that we are to read.
{
{    DestFile is the name of the file that we are to create on the
{    machine.
{
{    Is Text is a boolean that tells us if the file is a text file.
{    True means that it is a text file.
{
{ Results:
{    This function will return True iff it could read the file from
{    the other machine.
{
{ Errors:
{    All errors that happen during the execution of this procedure will
{    be reported to the user as error messages.
{
{ Design:
{---------------------------------------------------------------------------}
Var Packet: FTPPacket;
    ErrCode: ErrStatus;
    I, CurBlock: Integer;
    TFile: Text;
    BFile: Packed File of 0..255;
    Pfile: FileID;
    dum: integer;
    ByteInt: ByteIntRecord;
    Ptr: pDirBlk;
    
    Begin {ReadFile}
    
    {$ifc debug then} writeln('ReadFile in '); {$endc}
    ShowMemUsage;
    ReadFile := False;
    CursIncr := 0;
    CurBlock := 0;


    if ((CurDevice = Ethernet) or (CurDevice = FastEther))  and
         (not NetBuffAllocated) then NetAllocate;

    NetRelCur;
    NetPost;
    
{
{ Ask the other machine if the file is there.
{ }

    Packet.SrcFile:=SrcFile;
    {$R-}
    if EtherTransfer then
        begin
        for I := 0 to Length(SrcFile) do
            SendBuff^.Byte[I + 4] := Packet.Buffer[I];
        end;
    {$R=}

    If IsText Then
        WritePacket(Packet,ReadT,Ord(SrcFile[0])+1,ErrCode)
    Else
        WritePacket(Packet,ReadB,Ord(SrcFile[0])+1,ErrCode);
    If ErrCode <> Ok Then
        Begin
        FTPErr(ErrCode,Packet);
        Exit(ReadFile);
        End;
        
{
{ Wait for the ACK packet that should come back.
{ }

    If Not WaitPacket(Packet,Ack,ErrCode) Then
        Begin
        FTPErr(ErrCode,Packet);
        NetRelCur;
        NetPost;  
        Exit(ReadFile);
        End;
        
    If Packet.ByteCount = 5 then
        begin
        if not EtherTransfer then
            begin
            ByteInt.Lower := Ord(Packet.Buffer[0]);
            ByteInt.Upper := Ord(Packet.Buffer[1]);
            end
        else
            ByteInt.Whole := CurRecv.Buff^.Reg[2];
    
        if ByteInt.Whole <> 0 then
            begin
            if not EtherTransfer then
                CursIncr := 1024 div ((ByteInt.Whole * StreamPacketsPerBlock)+ 
                                       StreamFudge)
            else
                CursIncr := 1024 div ((ByteInt.Whole * EtherPacketsPerBlock) + 
                                       EtherFudge);
            end;

        If Debug then
            Writeln('RF::  ',ByteInt.Whole:1,'  CursIncr: ',CursIncr:1);
        end;
        
{
{ Set up the file that we are to output to
{ }

    Pfile := FSEnter(DestFile);
    IF Pfile = 0 Then
       Begin
       SendAbort('Bad path or file name specified.', ErrCode);
       If ErrCode <> Ok Then
            Begin
            FTPErr(ErrCode,Packet);
            Exit(ReadFile);
            End;
       If Not WaitPacket(Packet,Ack,ErrCode) Then
            Begin
            FTPErr(ErrCode,Packet);
            NetRelCur;
            NetPost;  
            Exit(ReadFile);
            End;
       NetRelCur;
       NetPost;  
       Exit(ReadFile);
       End;

    if not EtherTransfer then
       If IsText Then
           Rewrite(TFile,DestFile)
       Else
           Rewrite(BFile,DestFile);

{
{ Everthing is OK.  The other machine is now waiting for read requests.
{ We will issue read requests and expect to get ACK's that contain the
{ next block of the file.  If we don't get an ACK the packet must be
{ either an EOF or an Abort.  If it is an EOF then just close the
{ files,  ACK the EOF and then exit.
{ }

    While True Do
        Begin

        NetRelCur;
        NetPost;  
        
        WritePacket(Packet, ReadBlk, 0, ErrCode);
        If ErrCode <> OK Then
            Begin
            FTPErr(ErrCode,Packet);
            Exit(ReadFile);
            End;
            
        If Not WaitPacket(Packet,ACK,ErrCode) Then
            Begin
            If Packet.Cmd = FTPEOF Then
                Begin
                SendAck(0, ErrCode);
                if not EtherTransfer then
                    begin
                    If IsText Then
                        Close(TFile)
                    Else
                        Close(BFile);
                    end
                else
                    FSClose(PFile, CurRecv.Buff^.Reg[2], CurRecv.Buff^.Reg[3]);
                ReadFile := True;
                NetRelCur;
                NetPost;  
                {$ifc debug then} writeln('ReadFile out '); {$endc}
                Exit(ReadFile);
                End
            Else 
                Begin
                if not EtherTransfer then
                    begin
                    If IsText Then
                        Close(TFile)
                    Else
                        Close(BFile);
                    end;
 (*  ***  *)    {else should close PFile, but don't know sizes}
                FTPErr(ErrCode,Packet);
                NetRelCur;
                NetPost;  
                Exit(ReadFile);
                End;
            End {Not Ack Packet};
    
{
{ If we get here then we know that we have a good data packet.
{ If we are running on a PDP-11 and the file that is being transfered
{ is a binary file then the other machine will always transmit an
{ even number of bytes.  This must be done so that no bytes are lost.
{ }
            
        If IsText Then
            Begin
            For I:=0 To Ord(Packet.ByteCount)-4 Do
                Begin
                If Packet.Buffer[I] = Chr(#12) Then
                    WriteLn(TFile)
                Else
                    Begin
                    TFile^ := Packet.Buffer[I];
                    Put(TFile);
                    End;
                End;
            End
        Else
            Begin
            if not EtherTransfer then
                begin
                For I:=0 To Ord(Packet.ByteCount)-4 Do
                    Begin
                    if not Ethertransfer then
                        BFile^:= Ord(Packet.Buffer[I])
                    else
                        BFile^ := Ord(CurRecv.Buff^.Byte[I + 4]);
                    Put(BFile);
                    End;
                end
            else
                begin
                PointerConversion.P := recast(CurRecv.Buff, pDirBlk);
                PointerConversion.Offset := PointerConversion.Offset + 2;
                Ptr := PointerConversion.P;
                FSBlkWrite(Pfile, CurBlock, Ptr);
                CurBlock := CurBlock + 1;
                end;
            End;
        End {While True Do};
            
    End {ReadFile};
    
    
    
    
Function WriteFile(SrcFile: String;  IsText: Boolean): Boolean;
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to write a file to another machine.
{
{ Parameters:
{    SrcFile is the name of the file that we are to write to the
{    other machine.
{
{    Is Text is a boolean that tells us if the file is a text file.
{    True means that it is a text file.
{
{ Results:
{    This function will return true iff it could write the file to the
{    other machine.
{
{ Errors:
{    All errors that happen during the execution of this procedure will
{    be reported to the user as error messages.
{
{ Design:
{    This procedure is only called from Service.  It assumes that a
{    Read request for file ScrFile has come in on the line.  This
{    procedure will either Ack the request and then wait for
{    Data requests or send an Abort.
{
{    If the machine that we are sending to is a PDP-11 and we are sending
{    a binary file then we must always send packets that contain an even
{    number of data bytes.  If a packet contains an odd number of bytes
{    then the last byte of the packet will be lost.
{
{---------------------------------------------------------------------------}
Type CharArray = packed array [0..15359] of char;

Var Packet,ReplyPacket: FTPPacket;
    ErrCode: ErrStatus;
    I,Index, CurBlock, Bits, Blocks: Integer;
    TFile: Text;
    EOFHit: Boolean;
    ByteInt: ByteIntRecord;
    BFile: Packed File of 0..255;
    Pfile : FileID;
    Dum: integer;
    FileName: String;
    Ptr: pDirBlk;
{$ifc DoDirectory then}
    IsDirectory: Boolean;
    pData: ptrFSDataEntry;
    DirText: ^CharArray;
    DirName: SimpleName;
    DirIndex, DirInxMax: integer;
    DirScanPtr: ptrScanRecord;
    TID: FileID;
{$endc}
    
label 1, 2;

    
    Begin {WriteFile}

    {$ifc debug then} writeln('Writefile in '); {$endc}
    ShowMemUsage;
    
    NetRelCur;
    NetPost;
    
{
{ Try to open the file.  If we can't then send an Abort and exit.
{ }

    CursIncr := 0;
    WriteFile := False;
    FileName:=SrcFile;
    CurBlock := 0;

    If EtherTransfer Then
    Begin
      PointerConversion.P := recast(SendBuff, pDirBlk);
      PointerConversion.Offset := PointerConversion.Offset + 2;
      Ptr := PointerConversion.P;
    End;
            
    Pfile := FSLookUp(FileName, Blocks, Bits);  {see if there}
    IF Pfile = 0 then
       begin
       SendAbort('File not found.',ErrCode);
       If ErrCode <> Ok Then
            Begin
            FTPErr(ErrCode,Packet);
            Exit(WriteFile);
            End;
       If Not WaitPacket(Packet,Ack,ErrCode) Then
            Begin
            FTPErr(ErrCode,Packet);
            NetRelCur;
            NetPost;  
            Exit(WriteFile);
            End;
       NetRelCur;
       NetPost;  
       Exit(WriteFile);
       end;

{$ifc DoDirectory then}
    new(pData);
    FSGetFSData(PFile, pData);
    IsDirectory := (pData^.FileType = DirFile);
    dispose(pData);

    if IsDirectory then {ok to have zero blocks}
    else
{$endc}
    If Blocks = 0 then
       begin
       SendAbort('File has 0 blocks',ErrCode);
       If ErrCode <> Ok Then
            Begin
            FTPErr(ErrCode,Packet);
            Exit(WriteFile);
            End;
       If Not WaitPacket(Packet,Ack,ErrCode) Then
            Begin
            FTPErr(ErrCode,Packet);
            NetRelCur;
            NetPost;  
            Exit(WriteFile);
            End;
       NetRelCur;
       NetPost;  
       Exit(WriteFile);
       end;
      
{$ifc DoDirectory then}
    if IsDirectory then begin
       {allocate array}
          new(DirText);
          DirIndex := 0;
          new (DirScanPtr);
          DirScanPtr^.InitialCall := true;
          DirScanPtr^.DirName := FileName;
       {FSScan to read dirctory into array}
          while (DirIndex<15000) 
                  and FSScan(DirScanPtr, DirName, TID) do begin
              for i := 1 to length(DirName) do 
                 DirText^[DirIndex-1+i] := DirName[i];
              if IsText then begin
                 DirIndex := DirIndex+length(DirName)+1;
                 DirText^[DirIndex-1] := EOLN_Char;
              end
              else begin
                 DirIndex := DirIndex + length(DirName) + 2;
                 DirText^[DirIndex-2] := CH_Return;
                 DirText^[DirIndex-1] := CH_LF;
              end;
          end;
          dispose(DirScanPtr);
       {sort array}
       {set Blocks, Bits and other control vars}
          DirInxMax := DirIndex-1;
          DirIndex := 0;
          Blocks := DirInxMax div 512 + 1;
          Bits := 8*(1+DirInxMax-512*(Blocks-1));
    end
    else
{$endc}
    if not Ethertransfer then
        If IsText Then
            Reset(TFile,FileName)
        Else
            Reset(BFile,FileName);

    if not Ethertransfer then
        CursIncr := 1024 div ((Blocks * StreamPacketsPerBlock) + StreamFudge)
    else
        CursIncr := 1024 div ((Blocks * EtherPacketsPerBlock) + EtherFudge);

    SendAck(Blocks, ErrCode);
    If ErrCode <> Ok Then
        Begin
        FTPErr(ErrCode,Packet);
        Exit(WriteFile);
        End;
    
{
{ If we get here we know that all is well.   The other machine will
{ now send ReadBlk packets.  After each ReadBlk packet we will send
{ either an Ack packet with the next block of data or an EOF
{ packet.
{ }

    EOFHit:=False;
    Index:=0;
    
    If Not WaitPacket(ReplyPacket,ReadBlk,ErrCode) Then
        Begin
        FTPErr(ErrCode,ReplyPacket);
        NetRelCur;
        NetPost;  
        Exit(WriteFile);
        End;

    NetRelCur;
    NetPost;
            
    While Not EOFHit Do
        Begin
{$ifc DoDirectory then}
        if IsDirectory then begin
            if not EtherTransfer then begin  {text or bytestreamnet}
                Packet.Buffer[Index] := DirText^[DirIndex];
                DirIndex := DirIndex+1;
            end
            else begin  {Binary and FastEther}
                {copy from DirText^[DirIndex] to Ptr}
                for i := 0 to 511 do
                   Ptr^.ByteBuffer[i] := recast(DirText^[DirIndex+i], FSBit8);
                Index := 511;
                DirIndex := DirIndex + 512;
            end;
        end
        else
{$endc}
        If IsText Then
            Begin
            If EoLn(TFile) Then
                Begin 
                Packet.Buffer[Index]:=EOLN_Char;
                End
            Else
                Begin
                Packet.Buffer[Index]:=TFile^;
                End;
            Get(TFile);
            End
        Else
            Begin
            if not Ethertransfer then
                begin
                Packet.Buffer[Index]:=Chr(BFile^);
                Get(BFile);
                end
            else
                begin
                FSBlkRead(PFile, CurBlock, Ptr);
                CurBlock := CurBlock + 1;
                if CurBlock > Blocks then EOFHit := true;
                Index := 511;
                end;
            End;
            
{ 
{ See if this packet is full.  If so then send it and wait for
{ the next data request.   The value that Index is compared to
{ must be odd (i.e.  send an even number of data bytes).  
{ If it is even then the PDP-11 side of binary
{ tansfers will lose a byte.
{ }

        If Index >= 251 Then
            Begin
1:          WritePacket(Packet,Ack,Index+1,ErrCode);
            If ErrCode <> Ok Then
                Begin
                FTPErr(ErrCode,Packet);
                Exit(WriteFile);
                End;
            
            If Not WaitPacket(ReplyPacket,ReadBlk,ErrCode) Then
                Begin
                if ReplyPacket.Cmd = CSumError then
                        begin
                        NetRelCur;
                        NetPost;  
                        if Verbose then 
                            begin
                            write('w');
                            end;
                        goto 1;
                        end;
                FTPErr(ErrCode,ReplyPacket);
                NetRelCur;
                NetPost;  
                Exit(WriteFile);
                End;

            Index:=0;
            NetRelCur;
            NetPost;  
                
            End
        Else
            Index:=Index+1;
            
{
{ See if we hit EOF on the last read.  If so then set EOFHit to true.
{ Also subtract one from Index.  We incremented it before we tested
{ for EOF.
{ }
{$ifc DoDirectory then}
        if IsDirectory then begin
            if DirIndex > DirInxMax then begin
                EOFHit := true;
                index := index-1;
                dispose(DirText);
            end;
        end
        else
{$endc}
        If IsText Then
            Begin
            If EOF(TFile) Then 
                Begin
                EOFHit:=True;
                Index:=Index-1;
                Close(TFile);
                End;
            End
        Else
            Begin
            If EOF(BFile) and (not EtherTransfer) Then 
                Begin
                EOFHit:=True;
                Index:=Index-1;
                CLose(BFile);
                End;
            End;
            
        End {While loop};
        

{
{ When we get here we have hit an EOF on the file.  Send any data that
{ is left and then send the EOF.   If the file is a binary file then
{ make sure to send an even number of data bytes.
{ }

    If (Not Odd(Index)) And (Not IsText) and 
       ((curTmode=PERQ11) or (CurTMode=PERQVAX)) Then 
        Begin
        Index:=Index+1;
        Packet.Buffer[Index]:=Chr(0);
        End;
        
2:  WritePacket(Packet,Ack,Index+1,ErrCode);
    If ErrCode <> OK Then
        Begin
        FTPErr(ErrCode,Packet);
        Exit(WriteFile);
        End;
        
    If Not WaitPacket(ReplyPacket,ReadBlk,ErrCode) Then
        Begin
        if ReplyPacket.Cmd = CSumError then
           begin
           if Verbose then 
                begin
                NetRelCur;
                NetPost;  
                write('W');
                end;
           goto 2;
           end 
        else
            begin
            NetRelCur;
            NetPost;  
            FTPErr(ErrCode,ReplyPacket);
            Exit(WriteFile);
            end;
        End;

    NetRelCur;
    NetPost;

    if not Ethertransfer then
        WritePacket(Packet,FTPEOF,0,ErrCode)
    else
        begin
        SendBuff^.Reg[2] := Blocks;
        SendBuff^.Reg[3] := Bits;    
        WritePacket(Packet,FTPEOF,4,ErrCode);
        end;

    If ErrCode <> OK Then
        Begin
        FTPErr(ErrCode,Packet);
        Exit(WriteFile);
        End;
        
    If Not WaitPacket(ReplyPacket,Ack,ErrCode) Then
        Begin
        FTPErr(ErrCode,ReplyPacket);
        NetRelCur;
        NetPost;  
        Exit(WriteFile);
        End;

    NetRelCur;
    NetPost;
        
    WriteFile := True;
    {$ifc debug then} writeln('WriteFile out'); {$endc}
    
    End {WriteFile};
    
 
Function FTPAddRequest(Name: String;  Dev: DevTypes): Boolean; 
{---------------------------------------------------------------------
{
{ Abstract:
{   Send a request for an address out on the net.
{
{ Parameters:
{   Name is the name that we are to get the address for.
{
{   Dev is the device to use for the transfer.
{
{ Parameters:
{   Return true if we could get the address.
{   Return false otherwise.
{
{---------------------------------------------------------------------}
  var Packet: FTPPacket;
      ErrCode: ErrStatus;
      I: integer;
      Retval: Boolean;
      F: text;
  label 1;

    begin

    {$ifc debug then} write('Address Request In '); {$endc}
    ShowMemUsage;
    
    CurDevice := Dev;  
    if (CurDevice = Ethernet) or (CurDevice = FastEther) then NetAllocate;
    if not ((CurDevice = Ethernet) or (CurDevice = FastEther)) then 
        begin
        RetVal := false;
        goto 1;
        end;

     HaveHisAddr := false;
    
     SendHeader^.Dest.High := -1;       { Broadcast }
     SendHeader^.Dest.Mid := -1;
     SendHeader^.Dest.Low := -1; 
     HisAddr.High := SwapByte(HisAddr.High);
     HisAddr.Mid :=  SwapByte(HisAddr.Mid);
     HisAddr.Low :=  SwapByte(HisAddr.Low);

     
     if not EtherTransfer then
        Packet.Name := Name
    else
        {$R-}
        begin
        for I := 0 to length(Name) do
            SendBuff^.Byte[I + 4] := Name[I];
        end;
        {$R=}
        
    CursIncr := 512;
    LoadCurs; 
    NetGetCur;
    WritePacket(Packet, AddRequest, Ord(Name[0]) + 1, ErrCode);
    if ErrCode <> OK then
        begin
        FTPErr(ErrCode, Packet);
        RetVal := false;
        goto 1;
        end;

    if not WaitPacket(Packet, AddReply, ErrCode) then
        begin
        FTPErr(ErrCode, Packet);
        RetVal := false;
        goto 1;
        end;

    
    if not EtherTransfer then
        HisAddr := Packet.Add
    else
        begin
        HisAddr.High := CurRecv.Buff^.Reg[2];
        HisAddr.Mid :=  CurRecv.Buff^.Reg[3];
        HisAddr.LoW :=  CurRecv.Buff^.Reg[4];
        end;
    
    HisAddr.High := SwapByte(HisAddr.High);
    HisAddr.Mid := SwapByte(HisAddr.Mid);
    HisAddr.Low := SwapByte(HisAddr.Low);
    
    NetAllocate;                            { Set up packet headers }

    RetVal := true;
    HaveHisAddr := true;

1:  if Verbose then
        if RetVal then
            writeln(Name, ' responded with address ', HisAddr.High:1, ' ',
                    HisAddr.Mid:1, ' ', HisAddr.Low:1, '.')
        else
            writeln('** Could not get address of ', Name);
            
    NetRelCur;
    NetPost;
    QuitProgress;
    FTPAddRequest := RetVal;
    end;
      


function NetAddRequest(Name: string): boolean;
{------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to send the address of this machine to the
{   requesting machine.
{
{ Parameters:
{   Name is the name that the address request is for.
{
{ Results:
{   Return True if the requested name is my name and
{   we could send my address back.  Return false otherwise.
{
{-------------------------------------------------------------------------}
Label 100;
  var Packet: FTPPacket;
      ErrCode: ErrStatus;
      Str1, Str2: String;
      I : Integer;
    begin    
    ShowMemUsage;
    
    if not ((CurDevice = Ethernet) or (CurDevice = FastEther)) then 
        begin
        NetAddRequest := false;
        exit(NetAddRequest);
        end;

    Str1 := Name;
    CnvUpper(Str1);
    For I := 1 To NumAlias Do
        Begin
        Str2 := MyName[I];
        CnvUpper(Str2);
        if Str1 = Str2 then
            begin
            GoTo 100;
            end;
        End;
(****
    Str2 := MyName;
    CnvUpper(Str2);   ***)

    NetRelCur;
    NetPost;
    NetAddRequest := false;
    exit(NetAddRequest);


100:
    {$ifc debug then} writeln('Responding with my address.'); {$endc} 
    SendHeader^.EType := CurRecv.Head^.EType;
    SendHeader^.Dest := CurRecv.Head^.Src;
    
    if not EtherTransfer then
        begin
        Packet.Add.High := SendHeader^.Src.High;
        Packet.Add.Mid := SendHeader^.Src.Mid;
        Packet.Add.Low := SendHeader^.Src.Low;
        end
    else
        begin
        SendBuff^.Reg[2] := SendHeader^.Src.High;
        SendBuff^.Reg[3] := SendHeader^.Src.Mid;
        SendBuff^.Reg[4] := SendHeader^.Src.Low;
        end;
        
    WritePacket(Packet, AddReply, 6, ErrCode);
    if ErrCode <> OK then
        begin
        FTPErr(ErrCode, Packet);
        NetAddRequest := false;
        end
    else
        NetAddRequest := true;
    NetRelCur;
    NetPost;
    end;
    
    
Function Service: Boolean;
{-------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is called when a request for service has come in on
{    one of the transfer devices.
{
{ Parameters:
{    None.
{
{ Results:
{    Return True if the request couuld be satisfied.  Return
{    False otherwise.
{
{ Environment:
{    This procedure assumes that CurDevice contains the device that is
{    to be serviced.
{
{ Side Effects:
{    This procedure will service the pending request.
{
{ Errors:
{    This procedure does not return any error information.  If there are
{    errors while this procedure is execution then an error message
{    will be printed at the users terminal.
{
{ Design:
{-----------------------------------------------------------------------}
Var 
    I: integer;
    Packet: FTPPacket;
    ErrCode: ErrStatus;
    ErrMessage: String;
    FileName: String;
    
    Begin {Service}
    
    {$ifc debug then} writeln('Service In '); {$endc}

    Service := False;
    
    
{
{ Get the packet and see what we have to do.
{ }

    ReadPacket(false, Packet, ErrCode);
    If ErrCode <> Ok Then
        Begin
        NetRelCur;
        NetPost;
        FTPErr(ErrCode, Packet);
        Exit(Service);
        End;

    if (CurDevice = EtherNet) or (CurDevice = FastEther) then
        begin
        SendHeader^.Dest := CurRecv.Head^.Src;
        if not HaveHisAddr then HisAddr :=  SendHeader^.Dest;
        end;
    
    if Ethertransfer then
        begin 
        if Packet.ByteCount > 255 then 
            begin
            NetRelCur;
            NetPost;
            PacketValid := false;
            exit(Service);
            end;
        for I := 0 to Packet.ByteCount do
            Packet.Buffer[I] := CurRecv.Buff^.Byte[I + 4];
        end;
            
    With Packet Do
        Begin
        Case Cmd Of
        AskReadT: Begin
            If Verbose Then
                Begin 
                WriteLn('File write request for text file ',Packet.DestFile,
                    ' received');
                End;
            SendAck(0, ErrCode);
            If ErrCode <> OK Then
                Begin
                NetRelCur;
                NetPost;
                FTPErr(ErrCode,Packet);
                Exit(Service);
                End;
            Service := ReadFile(Packet.SrcFile,Packet.DestFile,True);
            If Verbose then WriteLn(' Done');
            ShowMemUsage;
            End;
            
        AskReadB: Begin
            If Verbose Then
                Begin
                WriteLn('File write request for binary file ',Packet.DestFile,
                    ' received');
                End;
            SendAck(0, ErrCode);
            If ErrCode <> OK Then
                Begin
                NetRelCur;
                NetPost;
                FTPErr(ErrCode,Packet);
                Exit(Service);
                End;
            Service := ReadFile(Packet.SrcFile,Packet.DestFile,False);
            If Verbose then WriteLn(' Done');
            ShowMemUsage;
            End;
            
        ReadT: Begin
            If Verbose Then
                Begin
                WriteLn('File read request for text file ',Packet.SrcFile,
                    ' received');
                End;
            Service := WriteFile(Packet.SrcFile,True);
            If Verbose Then WriteLn(' Done');
            ShowMemUsage;
            End;
            
        ReadB: Begin
            If Verbose Then
                Begin
                WriteLn('File read request for binary file ',Packet.SrcFile,
                    ' received');
                End;
            Service := WriteFile(Packet.SrcFile,False);
            If Verbose Then WriteLn(' Done');
            ShowMemUsage;
            End;
            
        Abort: Begin
            FTPErr(OK,Packet);
            NetRelCur;
            NetPost;
            Exit(Service);
            End;
            
        AddRequest: begin
            if Verbose then
                writeln('Address request for machine ', Name);
            Service := NetAddRequest(Name);
            If Verbose Then WriteLn;
            end;

        ACK,FTPEOF, AddReply: Begin
            WaitP:= ReadT;
            FTPErr(Ok,Packet);
            NetRelCur;
            NetPost; 
            SendAbort('Protocol Error:  Expecting Read or Write', ErrCode);
            If ErrCode <> Ok Then
                Begin
                FTPErr(ErrCode,Packet);
                Exit(Service);
                End;
            If Not WaitPacket(Packet,Ack,ErrCode) Then
                Begin
                FTPErr(ErrCode,Packet);
                NetRelCur;
                NetPost;
                Exit(Service);
                End;
            NetRelCur;
            NetPost;
            Exit(Service);
            End;
        End {Case};
        
        End {With};
    {$ifc debug then} writeln('Service Out '); {$endc}
        
    End {Service};
            

    

Procedure FTPInit;
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is called to initialize the FTP code.
{
{ Side Effects:
{    This procedure will initialize the I/O devices for the machine
{    that it is running on.
{
{ Parameter:
{    Dev - What device is used for the transfer
{
{---------------------------------------------------------------------}
    Begin {FTPInit}
    {$ifc debug then} writeln('FTPinit In'); {$endc}
    CurCharValid:=False;
    
    NetBuffAllocated := false;
    PacketValid := false;
    CurRecv.Stat := nil;
    CurRecv.Head := nil;
    CurRecv.Buff := nil;
    Ethertransfer := false; 
    HaveAddr := false;
    HaveHisAddr := false;
    
    {$ifc debug then} writeln('FTPinit Out'); {$endc}
    End {FTPInit};
    
    
    
Function FTPPutFile(SrcFile,DestFile: String;  IsItText:Boolean;
                     Dev: DevTypes; Mode: TransMode): Boolean;
{------------------------------------------------------------------------
{
{ Abstract:
{    This is the interface routine that will write a file to another
{    machine.
{
{ Parameters:
{    SrcFile is the name of the file, on this machine, that we are to write.
{
{    DestFile is the name that is to be used when writing the file
{    on the other machine.
{
{    IsItText is a boolean that indicates if the file is a text file.
{    If true then the file is a text file.
{
{    Dev is the name of the device that we are to use to transfer
{    the file.
{
{    Mode indicates the type of machine that is on the other end
{    of Dev.
{
{ Results:
{    Return True if the file was transfered without error.  False
{    otherwise.
{
{ Side Effects:
{    This procedure will change: CurDevice and CurTMode.
{
{ Errors:
{    All errors are indicated by error messages.
{
{ Design:
{    **** Caution:  This procedure contains a HACK !!!!!
{
{------------------------------------------------------------------------}
Var 
    I: integer;
    Packet: FTPPacket;
    ErrCode: ErrStatus;
    Bytes: Integer;
    Good: Boolean;
    Ch: Char;

    Begin
    {$ifc debug then} writeln('FTPPutFile In '); {$endc}
         
    CurDevice:=Dev;
    CurTMode:=Mode;
    IsText:=IsItText;
    FTPPutFile := False;
    CursIncr := 0;
    
    if IsText and (CurDevice = FastEther) then
        begin
        writeln('** You can not  transfer a Text file accross FastEtherNet.');
        writeln('** Please use Binary mode.');
        FTPPutFile := false;
        QuitProgress;
        exit(FTPPutFile);
        end;
    
    If (dev = EtherNet) Or (Dev = FastEther) Then 
        begin
        if not HaveHisAddr then
            begin
            writeln('** You must do a connect before you can use the Ethernet.');
            QuitProgress;
            FTPPutFile := false;
            exit(FTPPutFile);
            end
        else
            NetAllocate;
        end;

    If ClearBuff then 
        begin
        if (CurDevice = Ethernet) or (CurDevice = FastEther) then
            begin
            SBuffIndex := 0;
            RBuffIndex := 0;
            end
        else if CurDevice = FTP_RSA then
            begin
            while IOCRead(RSA, CurChar) = IOEIOC do ;
            end
        else if CurDevice = FTP_RSB then
            begin
            while IOCRead(RSB, CurChar) = IOEIOC do ;
            end
        else
            While GetByte(Ch) <> TimeOut Do ;
        end;    
       
{
{ Send an AskForRead packet to the other machine.  This packet must
{ have both the Source and Dest file fields set.
{
{              ********  HACK ******** 
{
{ In order to calculate the number of bytes to be sent in this packet
{ we make the following assumptions:    SrcFile is allocated BEFORE
{ DestFile in the packet record.  This means that the number of bytes that
{ must be sent is the total size of the SrcFile field in the record
{ plus the number of bytes that are valid in the DestFile field.
{
{ }

    Packet.SrcFile:=SrcFile;
    Packet.DestFile:=DestFile;
    Bytes:= 82 + (Length(DestFile) + 1);  {total length of SrcFile and Dst}
    
    if Ethertransfer then
        begin
        for I := 0 to Bytes do
            SendBuff^.Byte[I + 4] := packet.Buffer[I];
        end;
        
    If IsText Then
        WritePacket(Packet,AskReadT,Bytes,ErrCode)
    Else
        WritePacket(Packet,AskReadB,Bytes,ErrCode);
    If ErrCode <> OK Then
        Begin
        FTPErr(ErrCode,Packet);
        QuitProgress;
        Exit(FTPPutFile);
        End;
        
{
{ Wait for the ACK that should come back.
{ }

    If Not WaitPacket(Packet,Ack,ErrCode) Then
        Begin
        FTPErr(ErrCode,Packet);
        QuitProgress;
        NetRelCur;
        NetPost;            
        Exit(FTPPutFile);
        End;

    NetRelCur;
    NetPost;            
        
{
{ Now just call service and let things happen.
{ }

    LoadCurs;
    FTPPutFile := Service;
    QuitProgress;
    {$ifc debug then} writeln('FTPPutFile out '); {$endc}
    
    End {PutFile};

    
    
    
    
Function FTPGetFile(SrcFile,DestFile: String;  IsItText:Boolean;
                     Dev: DevTypes; Mode: TransMode): Boolean;
{------------------------------------------------------------------------
{
{ Abstract:
{    This is the interface routine that will read a file from another
{    machine.
{
{ Parameters:
{    SrcFile is the name of the file that we are to read from
{    the other machine.
{
{    DestFile is the name that is to`be created on this machine.
{
{    IsItText is a boolean that indicates if the file is a text file.
{    If true then the file ic a text file.
{
{    Dev is the name of the device that we are to use to transfer
{    the file.
{
{    Mode indicates the type of machine that is on the other end
{    of Dev.
{
{ Results:
{    Return True if the file was transfered without error.  False
{    otherwise.
{
{ Side Effects:
{    This procedure will change: CurDevice and CurTMode.
{
{ Errors:
{    All errors are indicated by error messages.
{
{ Design:
{------------------------------------------------------------------------}
var Ch: Char;
    Good: Boolean;

    Begin
    {$ifc debug then} writeln('FTPGetFile In '); {$endc}

    CurDevice:=Dev;
    if IsItText and (CurDevice = FastEther) then
        begin
        writeln('** You can not transfer a Text file accross FastEtherNet.');
        writeln('** Please use Binary mode.');
        FTPGetFile := false;
        exit(FTPGetFile);
        end;
    
    CurTMode:=Mode;
    IsText:=IsItText;
    CursIncr := 0;
    FTPGetFile := False;
    
    If (CurDevice = FastEther) Or (CurDevice = EtherNet) Then 
        begin
        if not HaveHisAddr then
            begin
            writeln('** You must do a connect before you can use the Ethernet.');
            exit(FTPGetFile);
            end
        else
            NetAllocate;
        end;
    If ClearBuff then 
        begin
        if (CurDevice = Ethernet) or (CurDevice = FastEther) then
            begin
            SBuffIndex := 0;
            RBuffIndex := 0;
            end
        else if CurDevice = FTP_RSA then
            begin
            while IOCRead(RSA, CurChar) = IOEIOC do ;
            end
        else if CurDevice = FTP_RSB then
            begin
            while IOCRead(RSB, CurChar) = IOEIOC do ;
            end
        else
            While GetByte(Ch) <> TimeOut Do ;
        end;    

    LoadCurs;
    FTPGetFile := ReadFile(SrcFile,DestFile,IsText);
    QuitProgress;
    {$ifc debug then} writeln('FTPGetFile Out '); {$endc}
   
    End {GetFile};
    

procedure FTPSetEtherAddr(Addr: EtherAddress; Dev: DevTypes);      
{---------------------------------------------------------------------
{ Abstract:
{   Set up to communicate with Perq having ethernet address Addr.
{ Parameters:
{   Addr is the ethernet address of the remote Perq..
{   Dev is the device to use for the transfer.
{ Environment:
{    Must have called FTPInit before calling this.
{ Design:
{   Stolen from FTPAddRequest by omitting all ethernet transactions.
{---------------------------------------------------------------------}
begin
    {$ifc debug then} write('FTP Start In '); {$endc}

    CurDevice := Dev;  
    if not ((CurDevice = Ethernet) or (CurDevice = FastEther)) then begin
           write ('Illegal device for FTPSetEtherAddr');
           exit(FTPSetEtherAddr);
    end;
    HisAddr := Addr;    
    HisAddr.High := SwapByte(HisAddr.High);
    HisAddr.Mid := SwapByte(HisAddr.Mid);
    HisAddr.Low := SwapByte(HisAddr.Low);
    HaveHisAddr := true;
    NetAllocate;                            { Set up packet headers }
    PostAllReceives;
end;


procedure FTPSetMyAddr(Dev: DevTypes); 
{------------------------------------------------------------------------
{
{ Abstract:
{   Set the address of this machine and allocate ethernet buffers.
{
{ Parameters:
{   Dev is the current device.  It must be Ethernet or FastEther.
{
{-------------------------------------------------------------------------}
  var F: text;
      InLine, SDum, Broke: String; 
      I : Integer;

    begin
    NumAlias := 0;
    reset(F, '>Ethernet.Names');
    While (Not EOF(F)) And (NumAlias < MAXALIAS)  Do
        Begin
        NumAlias := NumAlias + 1;
        Readln(F, MyName[NumAlias]);
        End;
        
    close(F);
    MyAddr := E10GetAdr;
    HaveAddr := true;
    if Verbose then writeln('My name is ', MyName[1], ' @ ', 
        MyAddr.High:1, '  ', MyAddr.Mid:1, '  ',MyAddr.Low:1); 

    
    If Verbose And (NumAlias > 1) then 
        Begin
        Write('My aliases are: ');
        For I := 2 To NumAlias - 1 Do Write(MyName[I]:1, ' ,, ');
        Writeln(MyName[NumAlias]:1);
        End;


    CurDevice := Dev;
    NetAllocate;
    end;

    
    
    
Procedure FTPChkDev;
{---------------------------------------------------------------------------
{ Abstract:
{    This procedure is used to see if the device specified by
{    Dev need to be serviced.  If so then enter the service request
{    routine.
{
{ Side Effects:
{    This procedure may change CurChar and CurCharValid.
{
{ Errors:
{    None
{----------------------------------------------------------------------------}
Var Result: Integer;
    Ignore, Good: Boolean;
    Ch : Char;
    Begin {FTPChkDev}
    CursIncr := 0;

    (* Do we need a ClearBuff operation here ? *)

    Case Dev Of
        FTP_RSA: If IOCRead(RSA,CurChar) = IOEIOC Then
                   Begin
                   CurDevice:=FTP_RSA;
                   CurCharValid:=True;
                   LoadCurs;
                   Ignore := Service;
                   QuitProgress;
                   End;
        FTP_RSB: If IOCRead(RSB,CurChar) = IOEIOC Then
                   Begin
                   CurDevice:=FTP_RSB;
                   CurCharValid:=True;
                   LoadCurs;
                   Ignore := Service;
                   QuitProgress;
                   End;
         Ethernet, FastEther: begin
                CurDevice := Dev;
                NetAllocate;
                NetGetCur;
                {$ifc debug then}
                    write('Poll check ');
                    PrintPOinter(CurRecv.Stat);
                    writeln;
                {$endc}
                if not CurRecv.Stat^.CmdInProgress then
                    begin
                    {$ifc debug then} write('Poll got packet '); {$endc}
                    CurDevice := Dev;
                    if (CurDevice = Ethernet) then
                        begin
                        if CurRecv.Head^.EType <> ByteType then
                            begin
                            PacketValid := false;
                            {$ifc debug then} writeln('punt'); {$endc}
                            NetRelCur;
                            NetPost;
                            exit(FTPChkDev);
                            end;
                        end
                    else if (CurDevice = FastEther) then
                        begin
                        if CurRecv.Head^.EType <> FastEType then
                            begin
                            PacketValid := false;
                            {$ifc debug then} writeln('punt'); {$endc}
                            NetRelCur;
                            NetPost;
                            exit(FTPChkDev);
                            end;
                        end;
                    PacketValid := true;
                    SendHeader^.Dest := CurRecv.Head^.Src;
                    LoadCurs;
                    {$ifc debug then} writeln('Call service'); {$endc}
                    Ignore := Service;
                    QuitProgress;
                    end;
                end;
        End; 
    End {FTPChkDev};

    
function  FTPDoEPacket(pPkt: Pointer; pHdr: Pointer): Boolean;
{---------------------------------------------------------------------------
{ Abstract:
{    Checks the packet and peforms whatever FTP service it calls for.
{
{ Environment:
{    Must have called FTPInit before calling this.
{
{ Parameters: 
{    pPkt - pointer to an ethernet packet containing an ftp packet.
{    pHdr - pointer to the header for the ethernet packet.
{----------------------------------------------------------------------------}
var
    EHdr: pEtherHeader;

Begin {FTPDoEPacket}
    EHdr := recast(pHdr, pEtherHeader);
    if EHdr^.EType = ByteType then begin
        CurDevice := Ethernet;
        EtherTransfer := False;
    end
    else if EHdr^.EType = FastEType then begin
        CurDevice := FastEther;
        EtherTransfer := True;
    end;
    NetAllocate;
    CurRecv.Head := EHdr;
    CurRecv.Buff := recast(pPkt, pMyEtherBuff);
    {Create fake status to convince ReadPacket the packet is valid.}
       SendStatus^.CRCError := False;
       SendStatus^.Collision:= False;
       CurRecv.Stat := SendStatus;
    SendHeader^.Dest := EHdr^.Src;
    PacketValid := True;
    
    CursIncr := 0;
    LoadCurs;
                    {$ifc debug then} writeln('Call service'); {$endc}
    FTPDoEPacket := Service;
    QuitProgress;
    
End {FTPDoEPacket}.
