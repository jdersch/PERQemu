{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program TestEther;

{---------------------------------------------------------------------
{
{ Abstract:
{   This is a simple program that is used to test the 10 MBaud Ethernet
{   code.
{
{ Written by: Don Scelza.
{
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1983.
{
{---------------------------------------------------------------------}

{$Version 4.18 }
{------------------------------------------------------------------------
{
{ Change History:
{
{ 24 Jan 83  V4.18 Chuck Beckett
{        Added import of IO_Unit so that v4.17 fix will compile.
{
{ 24 Jan 83  V4.17 Roger Riggs
{        Added call to IOSetExceptions so TestEther will get
{        E10Receive exceptions until F2.
{
{  5 Jan 83  V4.16 Chuck Beckett
{        Fixed EtherAdRec/Addptr variable names problems.
{
{ 16 Nov 82  V4.15 Bill Braucher
{        Fixed names for 14-character compiler.
{
{ 30 Aug 82  v4.14 Michael R. Kristofic
{        Fix 14 character identifier bug
{
{ 26 may 82  v4.13 Don Scelza, Ellen Colwell
{        Fix the bug that caused bad address to get set if reset command
{        was not executed.
{
{ 22 Apr 82  V4.12 Don Scelza
{        Took out E10Init.  Put in handler for Ctrl Shift C.
{ 
{ 21 Apr 82  V4.11 Don Scelza
{        Added code to get address from net board.  Set HisAddr to
{        MyAddr + 1.
{
{ 15 Apr 82  V4.10 Don Scelza
{        Added code to print the number of bits on bytecount error
{        in DoRandom.
{ 
{ 13 Apr 82  V4.9  Don Scelza
{        Added code to Set type field in echo.
{        Do Multicasts.
{
{ 12 Apr 82  V4.8  Don Scelza
{        Take out byte swap of type field.
{
{ 12 Apr 82  V4.7  Don Scelza
{        Added code to deal with the new group addresses.
{
{ 08 Apr 82  V4.6  Don Scelza
{        Changed order of Sends and Recieves in Echo Server.
{        Set default timeout to 30 sec.
{
{ 08 Apr 82  V4.5  Don Scelza
{        Put in code to limit size of random packet to ByteCount.
{
{ 07 Apr 82  V4.4  Don Scelza
{        Changed all of the code in TestEther to use the procedures in
{        Ether10Io to generate byte counts.
{
{ 20 Jan 82  V4.3  Don Scelza
{        Added a timeout in the random packet procedure.
{        Added code to turn the tablet on and off.
{
{ 17 Jan 82  V4.2  Don Scelza
{        Simple changes to help debug the new ethernet board.
{
{ 29 Dec 81  V4.0  Don Scelza
{       Took out all of the code for running without Ether10IO.
{
{ 22 Dec 81  V3.4  Don Scelza
{       Fixed some problems in the changing of windows.
{       Also made some changes to the Intel code.
{
{  1 Dec 81  V3.3  Don Scelza
{       Put in code to talk to the intel box.
{
{ 30 Nov 81  V3.2  Don Scelza
{       Put in a handler for receive done.
{
{ 02 Nov 81  V3.1  Don Scelza
{       Changed to use the new Ether10IO interface.
{
{ 30 Oct 81  V2.2  Don Scelza
{       Version 2.2 is the same as 2.1. 
{
{ 29 Oct 81  V3.0  Don Scelza
{       Started to put in the hooks to use the EtherIO module.
{
{ 27 Oct 81  V2.1  Don Scelza
{       Added a loop around the timeout code in DoRandom.
{
{ 23 Oct 81  V2.0  Don Scelza
{       Added code to deal with the microsecond clock.
{ 
{ 20 Oct 81  V1.21 Don Scelza
{       Fixed a bug in the random packet error check.
{       Added the variables for the Echo delay.
{
{ 19 Oct 81  V1.20 Don Scelza
{       Added the code to do random sends.
{
{ 15 Oct 81  V1.19 Don Scelza
{       Added new functions.  Echo and Random sends.
{ 
{ 14 Oct 81  V1.18 Don Scelza
{       Changed the type in code for addresses.  It will swap the
{       byte to conform to the Intel order of bytes.
{
{ 14 Oct 81  V1.17 Don Scelza
{       Added the code to allow the user to set the type field.
{
{ 14 Oct 81  V1.16 Don Scelza
{       Added calls to the help routines in EtherHelp.
{ 
{ 13 Oct 81  V1.15 Don Scelza
{       Changed the number of bits in the packet to be the number of data
{       bytes in the packet.
{
{ 13 Oct 81  V1.14 Don Scelza
{       Changed the order of the words in the ethernet address record.
{
{ 09 Oct 81  V1.13 Don Scelza
{       Added code to allo the user to set all values of the address.
{
{ 07 Oct 81  V1.12 Don Scelza
{       Added Promiscuous Receive.
{
{ 07 Oct 81  V1.11 Don Scelza
{       Fixed a number of bugs and made improvements.
{
{ 06 Oct 81  V1.9  Don Scelza
{       Fixed bug in the number of bits that were pushed at the interface.
{
{ 06 Oct 81  V1.8  Don Scelza
{       Added code and variables so that sends and receives would have
{       seperate state.
{
{ 30 Sep 81  V1.7  Don Scelza
{       Started to keep change log.  Also fixed DoReset to conform
{       to the changed Reset microcode.
{
{ 29 Sep 81  V1.0  Don Scelza
{       Created the program.
{
{------------------------------------------------------------------------}

Imports Ether10IO from Ether10IO;

imports RandomNumbers from RandomNumbers;
imports Sail_String from Sail_String;

imports PopUpCurs from PopUpCurs;
imports PopUp from PopUp;

imports IO_unit from IO_unit; 
imports IO_Others from IO_Others; 
imports GetTimeStamp from GetTimeStamp;

imports System from System;
imports Screen from Screen;

imports AlignMemory from AlignMemory;
imports Memory from Memory; 

imports EtherHelp from EtherHelp; 
imports Load_Display from Load_Display;




{
{ Define the types and variables used by the Pop_Up window stuff.
{ }

const
    HelpIndex = 1;
    SendIndex = HelpIndex + 1;
    MultiIndex = SendIndex + 1;
    ReceiveIndex = MultiIndex + 1;
    PromIndex = ReceiveIndex + 1;
    EchoIndex = PromIndex + 1;
    RandomIndex = EchoIndex + 1;
    IntelIndex = RandomIndex + 1;
    LoadIndex = IntelIndex + 1;
    DisplayIndex = LoadIndex + 1;
    ResetINdex = DisplayIndex + 1; 
    StatusIndex = ResetIndex + 1;
    SetIndex = StatusIndex + 1; 
    QuitIndex = SetIndex + 1;
    NumCmds = QuitIndex; 

    MyAdrIndex = HelpIndex + 1;
    HisAdrIndex = MyAdrIndex + 1;
    NumByteIndex = HisAdrIndex + 1;
    TypeIndex = NumByteIndex + 1;
    TimeIndex = TypeIndex + 1;
    DelayIndex = TimeIndex + 1;
    GrpCmdIndex = DelayIndex + 1;
    GrpAddIndex = GrpCmdIndex + 1;
    SndGrpIndex = GrpAddIndex + 1;    
    NumVars = SndGrpIndex;
    

    SLoadIndex = HelpIndex + 1;
    RLoadIndex = SLoadIndex + 1;    
    SStatIndex = RLoadIndex + 1;
    RStatIndex = SStatIndex + 1;
    SHeadIndex = RStatIndex + 1;
    RHeadIndex = SHeadIndex + 1;
    SDCBIndex = RHeadIndex + 1;
    RDCBIndex = SDCBIndex + 1;
    ClkDCBIndex = RDCBIndex + 1;
    NumShowBuffers = ClkDCBIndex;
    NumLoadBuffers = RLoadIndex;

    CmdX = 600;
    CmdY = 500;
    VarX = CmdX;
    VarY = CmdY;

    VarWinIndex = 1;
    VarWinX = 0;
    VarWinY = 25;
    VarWinWidth = 768;
    VarWinHeight = 128;

    StatWinIndex = 2;
    StatWinX = 0;
    StatWinY = VarWinHeight + VarWinY + 1;
    StatWinWidth = 768;
    StatWinHeight = 180;

    TypeWinIndex = 3;
    TypeWinX = 0;
    TypeWinY = StatWinHeight + StatWinY + 1;
    TypeWinWidth = 768;
    TypeWinHeight = 1024 - TypeWinY - 1;



type                      
 MyNameAr = Array[1..NumCmds] of s25;
 MypNameAr = ^MyNameAr;
 MyNameDesc = Record
     header: s25;
     numCommands: integer;
     commands: MyNameAr;
     End;
 MypNameDesc = ^MyNameDesc;

 MyResRes = ^MyResArray;
 MyResArray = Record
    numIndices: integer;
    indices: Array[1..NumCmds] of integer;
    End;

var pCmdArray: MypNameDesc;
    pVarArray: MypNameDesc;
    pLoadDisArray: MypNameDesc; 
    OutPress: boolean;

var SDum: String;

const 
        Version = '4.17';


var
    SDCBPtr, RDCBPtr: pEtherDCB;
    SBuffPtr, RBuffPtr: pEtherBuffer;
    SHeadPtr, RHeadPtr: pEtherHeader;
    SStatPtr, RStatPtr: pEtherStatus;
    SavePtr: pEtherRegSave;
    Addptr: pEtherAdRec;
    ClkDCBPtr: puSClkDCB; 
    SEtherSeg, REtherSeg: Integer;
    RecvFinished, DoingStatus: boolean;
    NumBytes, NumRecv,  PackType, TimeOutCount, EchoCount: Integer;
    RandomCount, RandomErrors, DelayCount, NumTimeOut: integer;
    HisAdr, MyAdr, GrpAdr: EtherAddress;
    GrpCmdbyte: 0..255;
    GrpAddArray: array[0..4] of 0..255;
    SndGrp: integer;
    ResetDone: boolean;
    Enable : Boolean;


type pDouble = ^Double;
var DoublePtr: pDouble;

type LoadDisType = (LoadIt, DispIt);

procedure DoStatus; forward;


label 1; 

const UnSwap = false;

{$ifc UnSwap then}
    {$message TestEther is un-swappable}
    var MySeg, Ignore: integer;
{$endc}

procedure Allocate;
{------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is called to allocate the segment and the buffers out
{   of that segment for use by the rest of this program.
{ 
{ Side Effects:
{   This procedure will allocate a segment placeing the segment number 
{   in EtherSeg.  It will also allocate objects  from the segment and
{   set the pointers DCBPtr, BuffPtr, HeadPtr, SavePtr and StatPtr.
{
{ Note:
{   This procedure is an incredible hack.  The EtherBuffer must be
{   allocated on a one thousand word boundry.  To do this we must
{   do some interesting hacking.  Also the segment that is created
{   must be marked as Unmovable.  This will allow the memory
{   manager to place it into a reasonable part of memory before it
{   is tied down.
{
{----------------------------------------------------------------------}
  var SegSize: integer;
    begin
    NewBuffer(recast(SBuffptr, AlignedPointer), 4, 4);
    NewBuffer(recast(RBuffptr, AlignedPointer), 4, 4);
    
    SegSize := ((WordSize(EtherHeader) + WordSize(EtherStatus) + 
                WordSize(pEtherRegSave) + WordSize(EtherDCB) +
                WordSize(EtherAdRec) + WordSize(uSClkDCB)) + 256) div 256; 

    CreateSegment(SEtherSeg, SegSize, 1, SegSize);
    new(SEtherSeg, 2, SStatPtr);
    new(SEtherSeg, 4, SHeadPtr);
    new(SEtherSeg, 1, AddPtr);
    new(SEtherSeg, 2, SavePtr);
    SetMobility(SEtherSeg, UnMovable);

    CreateSegment(REtherSeg, SegSize, 1, SegSize);
    new(REtherSeg, 2, RStatPtr);
    new(REtherSeg, 4, RHeadPtr);
    SetMobility(REtherSeg, UnMovable);

    end;
     

               
procedure InitVars;
{----------------------------------------------------------------
{
{ Abstract:
{   This procedure initializes the variable selection menu. 
{
{--------------------------------------------------------------}
    begin
    new(pVarArray);
    pVarArray^.Commands[HelpIndex] := 'Help';
    pVarArray^.Commands[MyAdrIndex] := 'My Address';
    pVarArray^.Commands[HisAdrIndex] := 'His Address';
    pVarArray^.Commands[NumByteIndex] := 'Byte Count';
    pVarArray^.Commands[TypeIndex] := 'Packet Type';
    pVarArray^.Commands[TimeIndex] := 'Timeout (seconds)';
    pVarArray^.Commands[DelayIndex] := 'Echo delay count';
    pVarArray^.Commands[GrpCmdIndex] :='Group Cmd Byte';
    pVarArray^.Commands[GrpAddIndex] :='Group Addresses';
    pVarArray^.Commands[SndGrpIndex] := 'Send Group';
    pVarArray^.NumCommands := NumVars; 
    pVarArray^.header := 'Internal Variables';
    end;


procedure InitCmds;
{----------------------------------------------------------------
{
{ Abstract:
{   This procedure initializes the command parseing portions of the
{   world.
{
{--------------------------------------------------------------}
    begin
    InitCurs;
    InitPopUp;

    new(pCmdArray);
    pCmdArray^.Commands[HelpIndex] := 'Help';
    pCmdArray^.Commands[SendIndex] := 'Send';
    pCmdArray^.Commands[MultiIndex] := 'Multicast';
    pCmdArray^.Commands[ReceiveIndex] := 'Receive';
    pCmdArray^.Commands[EchoIndex] := 'Echo';
    pCmdArray^.Commands[RandomIndex] := 'Random Packets';
    pCmdArray^.Commands[IntelIndex] :=  'Intel Diagnostic';
    pCmdArray^.Commands[LoadIndex] := 'Load';
    pCmdArray^.Commands[DisplayIndex] := 'Display';
    pCmdArray^.Commands[ResetIndex] := 'Reset';
    pCmdArray^.Commands[StatusIndex] := 'Status'; 
    pCmdArray^.Commands[SetIndex] := 'SetVars';
    pCmdArray^.Commands[PromIndex] := 'Promiscious Receive';
    pCmdArray^.Commands[QuitIndex] := 'Quit';
    pCmdArray^.NumCommands := NumCmds; 
    pCmdArray^.header := 'Ethernet Test program';

    InitVars;

    new(pLoadDisArray);
    pLoadDisArray^.Commands[HelpIndex] := 'Help';
    pLoadDisArray^.Commands[SLoadIndex] := 'Send Buffer';
    pLoadDisArray^.Commands[RLoadIndex] := 'Receive Buffer';
    pLoadDisArray^.Commands[SStatIndex] := 'Send Status';
    pLoadDisArray^.Commands[RStatIndex] := 'Receive Status';
    pLoadDisArray^.Commands[SHeadIndex] := 'Send Header';
    pLoadDisArray^.Commands[RHeadIndex] := 'Receive Header';
    pLoadDisArray^.Commands[SDCBIndex] :=  'Send DCB';
    pLoadDisArray^.Commands[RDCBIndex] :=  'Receive DCB';
    pLoadDisArray^.Commands[ClkDCBIndex] := 'Clock DCB'; 
    pLoadDisArray^.NumCommands := RHeadIndex;
    pLoadDisArray^.Header := 'Which buffer?';
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




Procedure UpDateVars;
{------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to update values in the Variable Window.
{
{------------------------------------------------------------------}
  var X, Y, I: integer;
    begin
    ChangeWindow(VarWinIndex);
    X := VarWinX + 10;
    Y := VarWinY + 35; 
    SSetCursor(X, Y);
    Write('My Address   = ', SwapByte(MyAdr.High):1, ',,',
          SwapByte(MyAdr.Mid):1, ',,', SwapByte(MyAdr.Low):1, '          ',
          'Echo delay count    = ', DelayCount:5);
    Y := Y + 20;
    SSetCursor(X, Y);
    Write('His Address  = ', SwapByte(HisAdr.High):1, ',,',
          SwapByte(HisAdr.Mid):1,  ',,', SwapByte(HisAdr.Low):1, '          ',
          'Timeout (seconds)   = ',TimeOutCount:5);
    Y := Y + 20;
    SSetCursor(X, Y); 
    Write('Byte count   = ', NumBytes:5,
         '                  Packet Type         = ', PackType:5);
    Y := Y + 20;
    SSetCursor(X, Y); 
    Write('Grp Add Cmd  = ', GrpCmdByte:5,
         '                  Address groups      = ');
    for I := 0 to 4 do 
        begin
        write(GrpAddArray[I]:2);
        if I <> 4 then write(',,');
        end;
    Y := Y + 20;
    SSetCursor(X, Y); 
    write('Send Group   = ', SndGrp:5);
    ChangeWindow(TypeWinIndex);
    end;


procedure SetVar;
{--------------------------------------------------------------
{
{ Abstract:
{   This procedure is called to set a user setable variable.
{
{-------------------------------------------------------------}
  var I, CurX, CurY: integer;
      BradName: pNameDesc;
      BradRet: ResRes;
      SDum: string;
  label 1;

  handler Outside;
    begin
    goto 1;
    end;   
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(SetVar);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(SetVar);
    end;

   
    begin
    BradRet := Nil;
    ChangeWindow(TypeWinIndex);
    while true do
        begin
        IOReadTablet(CurX, CurY);
        BradName := recast(pVarArray, pNameDesc);
        Menu(BradName, False, 1, pVarArray^.NumCommands, VarX, VarY, 500, 
             BradRet);
        case BradRet^.Indices[1] of
            Helpindex: VarHelp;
            MyAdrIndex: begin
                write('Value of high word for MY address [',
                    SwapByte(MyAdr.High):1, '] = ');
                readln(SDum);
                if length(Sdum) <> 0 then
                    MyAdr.High := SwapByte(CVD(SDum));
                write('Value of mid word for MY address [',
                    SwapByte(MyAdr.Mid):1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then
                    MyAdr.Mid := SwapByte(CVD(SDum));
                write('Value of low word for MY address [',
                    SwapByte(MyAdr.Low):1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then 
                    MyAdr.Low := SwapByte(CVD(SDum));
                end;
            HisAdrIndex:  begin
                write('Value of high word for HIS address [',
                    SwapByte(HisAdr.High):1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then
                    HisAdr.High := SwapByte(CVD(SDum));
                write('Value of mid word for HIS address [',
                    SwapByte(HisAdr.Mid):1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then
                    HisAdr.Mid := SwapByte(CVD(SDum));
                write('Value of low word for HIS address [',
                    SwapByte(HisAdr.Low):1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then
                    HisAdr.Low := SwapByte(CVD(SDum));
                end;
            NumByteIndex: begin
                write('New byte count = ');
                readln(NumBytes);
                end;
            TypeIndex: begin
                write('Value for the packet type field [',
                    PackType:1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then
                    PackType := CVD(SDum);
                end;
            TimeIndex: begin
                write('New timeout value in seconds [',
                    TimeOutCount:1, '] = ');
                readln(SDum);
                if length(SDUm) <> 0 then TimeOutCount := CVD(SDum);
                end;
            DelayIndex: begin
                write('New Echo delay count[',
                    DelayCount:1, '] = ');
                readln(SDum);
                if length(SDUm) <> 0 then DelayCount := CVD(SDum);
                end;
            GrpCmdIndex: begin
                write('New value of Group Command byte [', 
                    GrpCmdByte:1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then GrpCmdbyte := CVD(SDum);
                end;
            GrpAddIndex: begin
                for I := 0 to 4 do
                    begin
                    write('New value for group address ', I:1, ' [',
                        GrpAddArray[I]:1, '] = ');
                    readln(SDum);
                    if length(SDum) <> 0 then GrpAddArray[I] := CVD(SDum);
                    end;
                end;
            SndGrpIndex: begin
                write('New Group for Multicasts [', 
                    SndGrp:1, '] = ');
                readln(SDum);
                if length(SDum) <> 0 then SndGrp := CVD(SDum);
                GrpAdr.Low := SwapByte(SndGrp);
                end;
                
            end;
        UpdateVars;
        DestroyRes(BradRet);
        BradRet := Nil;
        end;

    1: if BradRet <> Nil then DestroyRes(BradRet);
    UpdateVars;
    end;


function GetCmd: integer;
{--------------------------------------------------------------
{
{ Abstract:
{    Get the index of the command that the user pointed to.
{
{ Results:
{    Return the index of the press.
{
{-------------------------------------------------------------}
  var I, CurX, CurY: integer;
      BradName: pNameDesc;
      BradRet: ResRes;
    begin
    IOReadTablet(CurX, CurY);
    BradName := recast(pCmdArray, pNameDesc);
    BradRet := Nil;
    Menu(BradName, False, 1, pCmdArray^.NumCommands, CmdX, CmdY, 500, 
         BradRet);
    GetCmd := BradRet^.Indices[1];
    DestroyRes(BradRet);
    end;

handler Outside;
{-------------------------------------------------------------
{
{ Abstract:
{    Handle the OutSide exception. 
{
{---------------------------------------------------------------}
    begin
    OutPress := true;
    goto 1;
    end;



procedure DoQuit;
{-------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to exit the program.  It will clean up
{   the Network and any memory that needs to be dealt with.
{
{------------------------------------------------------------------}
    begin
    if ResetDone then E10Reset(AddPtr);
    ScreenReset;
    exit(TestEther);
    end;




procedure DoReset;
{-----------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to execute an Ethernet reset.
{ 
{-----------------------------------------------------------------}
    begin

    Addptr^.LowAddress := MyAdr.Low;
    Addptr^.MCB := GrpCmdByte;
    Addptr^.MultCst1 := GrpAddArray[0];
    Addptr^.MultCst2 := GrpAddArray[1];
    Addptr^.MultCst3 := GrpAddArray[2];
    Addptr^.MultCst4 := GrpAddArray[3];
    Addptr^.MultCst5 := GrpAddArray[4];
    E10Reset(Addptr);
    ResetDone:= true;
    
    RStatPtr^.CRCError := false;
    RStatPtr^.Collision := false;
    RStatPtr^.Busy := false;
    RStatPtr^.PIP := false;
    RStatPtr^.Carrier := false;
    RStatPtr^.SendError := false;
    RStatPtr^.CmdInProgress := false;
    RStatPtr^.RetryTime := 0;
    SStatPtr^ := RStatPtr^;
    DoublePtr := recast(RStatPtr, pDouble);
    DoublePtr^[0]:= 0;
    DoublePtr^[1]:= 0;
    end;
    

    

procedure DoSend(Ptr: pEtherBuffer;  Adr: EtherAddress;  Bytes: integer);
{-----------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to execute an Ethernet Send.  It
{   will send the buffer pointed to by Ptr to the machine at
{   address Adr.
{
{ Parameters:
{   Ptr is a pointer to the buffer that is to be sent.
{
{   Adr is the destionation address.
{
{   Bytes is the number of data bytes to send.
{ 
{-----------------------------------------------------------------}
    begin 
    
    {
    { Set up the header block.
    { }
    
    SHeadptr^.Dest := Adr;
    SHeadPtr^.Src := MyAdr;
    SHeadPtr^.EType := PackType;

    E10IO(ESend, SHeadptr, Ptr, SStatPtr, Bytes);
    end;
    

    

procedure DoReceive(Ptr: pEtherBuffer; Promiscuous: boolean);
{-----------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to execute an Ethernet reset.
{
{ Parameters:
{   Ptr is a pointer to the buffer that is to filled with the packet.
{
{   If Promiscuous is true then look for any packets on the
{   net.  If it is false only look for packets addressed to this
{   device.
{ 
{-----------------------------------------------------------------}
  var Cmd: EtherCommand;
    begin
    if Promiscuous then
        Cmd := EPromiscuousReceive
    else        
        Cmd := EReceive;
    E10IO(Cmd, RHeadptr, Ptr, RStatPtr, 0);
    end;
    

    
    

procedure DoStatus;
{---------------------------------------------------------------------
{
{ Abstract:
{   Print the status information from the current status block.
{
{--------------------------------------------------------------------}
  label 1;
  handler E10DByteError;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** Status - Bytecount error on last packet received.');
    exit(DoStatus);
    end;   
  var NumSend, NumRecv: Integer;

    begin
    ChangeWindow(StatWinIndex);
    writeln(chr(12));

    E10State(NumSend, NumRecv);

    write('CRC Error =           ', SStatPtr^.CRCError:5, '     ',
            RStatPtr^.CRCError:5);
    write('          Sends pending = ', NumSend:3);
    writeln;

    write('Collision =           ', SStatPtr^.Collision:5, '     ',
            RStatPtr^.Collision:5); 
    write('          Recvs pending = ', NumRecv:3);
    writeln;

    write('Receive or Transmit = ');
    if SStatPtr^.RecvTrans then
        write('Transmit. ')
    else
        write('Receive.  ');
    if RStatPtr^.RecvTrans then
        writeln('Transmit.    ')
    else
        writeln('Receive.     ');

    writeln('Busy =                ', SStatptr^.Busy:5, '     ',
           RStatptr^.Busy:5, 
           '          Random Count  = ', RandomCount:3);

    writeln('Clock Overflow =      ', SStatptr^.ClockOver:5, '     ',
           RStatptr^.ClockOver:5,
           '          Random Errors = ', RandomErrors:3);

    writeln('Packet in Progress =  ', SStatPtr^.PIP:5, '     ',
           RStatPtr^.PIP:5,
           '          Echo count    = ', EchoCount:3);

    writeln('Carrier =             ', SStatPtr^.Carrier:5, '     ',
            RStatPtr^.Carrier:5,
           '          Time outs     = ', NumTimeOut:3);

    writeln('Command in Progress = ', SStatptr^.CmdInProgress:5, '     ',
            RStatptr^.CmdInProgress:5);
 
    if RStatPtr^.BitsRecv = 0 then
       writeln('Data bytes in packet =', NumBytes:5, '     ', '    0')
    else
       writeln('Data bytes in packet =', Numbytes:5, '     ',
                ((RStatPtr^.BitsRecv div 8) - 18):5);
1:  ChangeWindow(TypeWinIndex);

    end;


procedure DoLoadDisplay(Which: LoadDisType);
{-----------------------------------------------------------------------
{
{ Abstract:
{   This pcoedure is called when the user executes a load or display
{   command.  It will ask if wh wants to see the send or receive buffers
{   and then call the routines in Load_Display.
{
{-----------------------------------------------------------------------}
  var I, CurX, CurY: integer;
      BradName: pNameDesc;
      BradRet: ResRes;
    
  handler Outside;
    begin
    exit(DoLoadDisplay);
    end;   
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(DoLoadDisplay);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(DoLoadDisplay);
    end;

    begin
    IOReadTablet(CurX, CurY);
    if Which = LoadIt then
        begin
        BradName := recast(pLoadDisArray, pNameDesc);
        BradRet := Nil;
        pLoadDisArray^.NumCommands := NumLoadBuffers;
        while true do
            begin
            Menu(BradName, False, 1, pLoadDisArray^.NumCommands, CmdX, CmdY, 
                500, BradRet);
            I := BradRet^.Indices[1];
            DestroyRes(BradRet);
            case I of
                HelpIndex: DisplayHelp;
                SLoadIndex: Load(SBuffPtr, NumBytes, CmdX, CmdY);
                RLoadIndex: Load(RBuffPtr, NumBytes, CmdX, CmdY);
                end;
            end;
        end
    else
        begin
        BradName := recast(pLoadDisArray, pNameDesc);
        BradRet := Nil;
        pLoadDisArray^.NumCommands := NumShowBuffers;
        while true do
            begin
            Menu(BradName, False, 1, pLoadDisArray^.NumCommands, CmdX, CmdY, 
                500, BradRet);
            I := BradRet^.Indices[1];
            DestroyRes(BradRet);
            case I of
                HelpIndex: DisplayHelp;
                SLoadIndex: Display(SBuffptr, NumBytes, CmdX, CmdY);
                RLoadIndex: Display(RBuffptr, NumBytes, CmdX, CmdY);
                SStatIndex: Display(SStatPtr, wordsize(EtherStatus) * 2, 
                    CmdX, CmdY);
                RStatIndex: Display(RStatPtr, wordsize(EtherStatus) * 2, 
                    CmdX, CmdY);
                SHeadIndex: Display(SHeadPtr, wordsize(EtherHeader) * 2, 
                    CmdX, CmdY);
                RHeadIndex: Display(RHeadPtr, wordsize(EtherHeader) * 2, 
                    CmdX, CmdY);
                SDCBIndex: Display(SDCBPtr, wordsize(EtherDCB) * 2, 
                    CmdX, CmdY);
                RDCBIndex: Display(RDCBPtr, wordsize(EtherDCB) * 2, 
                    CmdX, CmdY);
                ClkDCBIndex: Display(ClkDCBPtr, wordsize(uSClkDCB) * 2,
                    CmdX, CmdY);
                end;
            end;
        end;
    end;

function IsTimeOut(Time1, Time2: TimeStamp;  NumSec: Integer): boolean;
{-----------------------------------------------------------------------
{
{ Abstract:
{   This function is used to see if a timout has occured.
{
{ Parameters:
{   Time1 is the time that the operation was started.
{
{   Time2 is the current time.
{
{   NumSec is the number of seconds that are to pass befor a timeout.
{
{ Results:
{   If there are more than NumSec between Time1 and Time2 then return
{   true.  Otherwise return false.
{
{ Note:
{   This procedure only works for 0 <= (Time2 - Time1) <= 3600 (seconds).
{
{----------------------------------------------------------------------}
  var SecDiff: integer;
    begin
    if Time1.Hour <> Time2.Hour then
        { assume difference is only 1 hour.  this is where we assume }
        { that 0 <= (Time2 - Time1) <= 3600 (seconds).               }
        begin
        SecDiff := 3600
        end
    else
        begin
        SecDiff := 0
        end;
    SecDiff := SecDiff + (Time2.Minute - Time1.Minute) * 60;
    SecDiff := SecDiff + (Time2.Second - Time1.Second);
    IsTimeOut := SecDiff > NumSec;
    end;
    
    
    

procedure DoEcho;
{------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to echo packets on the net.
{
{-------------------------------------------------------------------}
  var I: Integer;
  Label 1;

  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(DoEcho);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(DoEcho);
    end;
  handler E10ReceiveDone(Stat: pEtherStatus);
    begin
    end;
  handler E10DByteError;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** Echo - Bytecount error on last packet received.');
    DoReset;
    NumBytes := 128;
    goto 1;
    end;   

    begin 
    ChangeWindow(TypeWinIndex);

    writeln;
    writeln('Echo server.  Type ^C to exit this mode.');
    writeln;
    EchoCount := 0; 
    IOSetModeTablet(offTablet);

    while true do
        begin
        DoReceive(RBuffptr, false);
        E10Wait(RStatPtr);
        EchoCount := EchoCount + 1;
        NumBytes := E10DataBytes(RStatPtr^.BitsRecv);

1:      for I := 1 to DelayCount do ;

        PackType := RHeadptr^.EType;
        DoSend(RBuffptr, RHeadptr^.Src, NumBytes);
        DoStatus;
        end;
    end;
    

procedure DoRandom;
{------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to send random packets on the net.
{   After a packet is sent it will try to receive the echo
{   of that packet.  It will then check to see if there were any
{   data errors.
{
{-------------------------------------------------------------------}
  var I, Loop, Words, MaxBytes: Integer;
      Time1, Time2: TimeStamp;
  label 1, 2;
       
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    NumBytes := MaxBytes;
    writeln;
    exit(DoRandom);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    NumBytes := MaxBytes;
    writeln;
    exit(DoRandom);
    end;
  handler E10ReceiveDone(Stat: pEtherStatus);
    begin
    end;
  handler E10DByteError;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** Random - Bytecount error on  packet.  Bytes = ',
            RStatptr^.BitsRecv:1);
    RandomErrors := RandomErrors + 1;
    DoReset;
    goto 1;
    end;   

    begin
    ChangeWindow(TypeWinIndex);
    writeln;
    writeln('Random Packet server.  Type ^C to exit this mode.');
    writeln;
    RandomCount := 0;
    RandomErrors := 0;
    NumTimeOut := 0;
    MaxBytes := NumBytes;
    InitRandom;
    IOSetModeTablet(offTablet);

    while true do
        begin
        Words := Random;
        Words := land(Words, #1777);
        if Words < 1 then Words := -Words;
        if Words > (MaxBytes div 2) then
            Words := MaxBytes div 2
        else if Words < 32 then
            Words := 32;
        for I := 0 to Words - 1 do
            SBuffPtr^[I] := Random;
        NumBytes := Words * 2;
 

        DoReceive(RBuffptr, false);
 2:     DoSend(SBuffptr, HisAdr, NumBytes);

        GetTStamp(Time1);
        while RStatPtr^.CmdInProgress do
            begin
            GetTStamp(Time2);
            if IsTimeOut(Time1, Time2, TimeOutCount) then
                begin
                NumTimeOut := NumTimeOut + 1;
                writeln('Timeout on random packet ', RandomCount:1);
                goto 2;
                end;
            end;

        DoStatus;
    
        RandomCount := RandomCount + 1;

        if NumBytes <> E10DataBytes(RStatptr^.BitsRecv) then
            begin
            RandomErrors := RandomErrors + 1;
            writeln('Byte count error:');
            goto 1;
            end;

        for I := 0 to Words - 1 do
            begin
            if RBuffPtr^[I] <> SBuffPtr^[I] then
                begin
                RandomErrors := RandomErrors + 1;
                writeln('Data error:  Send[', I:1, ']: ', SBuffPtr^[I]:1,
                    ' <> Receive[', I:1, ']: ', RBuffptr^[I]:1);
                goto 1;
                end;
            end;
        1: end;
    end;

Procedure DoIntel;
{----------------------------------------------------------------------
{
{ Abstract:
{   This procedure implementes the protocols for the
{   Intel development system.
{
{----------------------------------------------------------------------}
{$ifc false then}
  var IDTest, I: integer;
 
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    Exit(DoIntel);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    Exit(DoIntel);
    end;
  handler E10ReceiveDone(Stat: pEtherStatus);
    begin
    DoStatus;
    ChangeWindow(TypeWinIndex);
    end;

    begin
    ChangeWindow(TypeWinIndex);
    writeln('Intel development system diagnostic.');
    writeln('Type ^C to exit.');
    writeln;
    IOSetModeTablet(offTablet);

    PackType := 20481;                              { 5001H }



{ Get the first packet that the Intel box sends to us.
{ }

    DoReceive(RBuffptr, false);
    E10Wait(RStatPtr);
    
    write('Address of the Intel Box is ');
    write(RHeadptr^.Src.High:4:-16, '  ', 
          RHeadptr^.Src.Mid:4:-16, '  ',
          RHeadptr^.Src.Low:4:-16);
    writeln;
    
    if RHeadPtr^.EType <> PackType then
        begin
        writeln('Bad Packet type for Intel test.  Value was ',
                RHeadptr^.EType:1:16, 'H');
        exit(DoIntel);
        end;

    IDTest := 21764;                            { 55H in high byte, 4 in low }
    if RBuffPtr^[0] <> IDTest then
        begin
        writeln('Bad ID/Test for Intel test.  Value was ',
                RBuffPtr^[0]:1:16, 'H');
        exit(DoIntel);
        end; 


{
{ Start the handshake.
{}

    SHeadptr^.EType := PackType;
    SBuffPtr^[0] := 21763;                      { 55H,, 3 }
    DoSend(SBuffPtr, RHeadPtr^.Src, 2);
    for I := 1 to DelayCount do ;
    SBuffPtr^[0] := 21762;                      { 55H,, 3 }
    DoSend(SBuffPtr, RHeadPtr^.Src, 2);

{
{ We are now ready to go into the test loop.
{}

    DoReceive(RBuffptr, false);
    while true do
        begin
        E10Wait(RStatPtr);

        if RHeadPtr^.EType <> PackType then
            begin
            writeln('Bad Packet type for Intel test.  Value was ',
                    RHeadptr^.EType:1:16, 'H');
            exit(DoIntel);
            end;

        IDTest := 21761;                        { 55H in high byte, 1 in low }
        if RBuffPtr^[0] <> IDTest then
            begin
            writeln('Bad Loop ID/Test for Intel test.  Value was ',
                    RBuffPtr^[0]:1:16, 'H');
            exit(DoIntel);
            end;

        NumBytes := E10DataBytes(RStatPtr^.BitsRecv);
            
        for I := 2 to NumBytes do
            write(chr(RBuffPtr^[I]));
        writeln;
        end;               
    end;
{$elsec}
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** Not Implemented.');
    end;
{$endc}
    
  
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    DoQuit;
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    DoQuit;
    end;
  handler CtlShftC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    DoQuit;
    end;
  handler E10ReceiveDone(Stat: pEtherStatus);
    begin
    if not DoingStatus then 
        DoStatus
    else
        RecvFinished := true;
    end;
  handler E10NInited;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** You must do an init before you execute any commands.');
    goto 1;
    end;
  handler E10NReset;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** You must do a reset before you execute any commands.');
    goto 1;
    end;
  handler E10ByteCount;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** Bytecount error on packet to be sent.');
    goto 1;
    end;   
  handler E10DByteError;
    begin
    ChangeWindow(TypeWinIndex);
    write(chr(7));
    writeln('** Bytecount error on last packet received.');
    goto 1;
    end;   
    
begin 


InitCmds;

IOSetModeTablet(RelTablet);
IOCursorMode(TrackCursor);
OutPress := false;

writeln(chr(12));
SDum := '10MBaud Ethernet test program.  Version ';
SDum := Concat(SDum, Version);
ChangeTitle(SDum);

CreateWindow(VarWinIndex, VarWinX, VarWinY, VarWinWidth, VarWinHeight,
             'Internal Variables');
CreateWindow(StatWinIndex, StatWinX, StatWinY, StatWinWidth, StatWinHeight,
             'Network Status        Send ,,  Receive        General');
CreateWindow(TypeWinIndex, TypeWinX, TypeWinY, TypeWinWidth, TypeWinHeight,
             'Type Script');

ChangeWindow(TypeWinIndex);
writeln(chr(12));

{$ifc UnSwap then}
    makevrd(DoStatus);
    storexpr(MySeg);
    storexpr(Ignore);
    storexpr(Ignore);
    storexpr(Ignore);
    writeln('TestEther segment is ', MySeg:1);
    SetMobility(MySeg, UnSwappable);
{$endc}

Enable := True;
IOSetExceptions(Ether10, IODataInterrupt, Enable);


NumBytes := 128;
NumRecv := 0;

(***
HisAdr.Low := SwapByte(4);
HisAdr.Mid := SwapByte(31744);
HisAdr.High := SwapByte(540);
MyAdr.Low := SwapByte(2);
MyAdr.Mid := SwapByte(31744);
MyAdr.High := SwapByte(540);
***)

MyAdr := E10GetAdr;
MyAdr.High := SwapByte(MyAdr.High);
MyAdr.Mid := SwapByte(MyAdr.Mid);
MyAdr.Low := SwapByte(MyAdr.Low);
HisAdr := MyAdr;
HisAdr.Low := SwapByte(SwapByte(MyAdr.Low) + 1);

PackType := 0;

EchoCount := 0;
TimeOutCount := 10;
RandomCount := 0;
RandomErrors := 0;
DelayCount := 0;
NumTimeOut := 0;
RecvFinished := false;
DoingStatus := false;
ResetDone:= false;

Allocate;
GrpCmdbyte := MltCstNone;
GrpAddArray[0] := 1;
GrpAddArray[1] := 2;
GrpAddArray[2] := 3;
GrpAddArray[3] := 4;
GrpAddArray[4] := 5;
SndGrp := 1;
GrpAdr.High := 1;
GrpAdr.Mid := 0;
GrpAdr.Low := Swapbyte(SndGrp);

UpDateVars; 

DoublePtr := recast(SStatPtr, pDouble);
DoublePtr^[0]:= 0;
DoublePtr^[1]:= 0;

DoublePtr := recast(RStatPtr, pDouble);
DoublePtr^[0]:= 0;
DoublePtr^[1]:= 0;

LoadInit;

while true do
    begin
    IOSetModeTablet(RelTablet);
    case GetCmd of
        HelpIndex: CmdHelp;
        SendIndex: begin
            DoingStatus := true;
            DoSend(SBuffptr, HisAdr, NumBytes);
            DoStatus;
            DoingStatus := false;
            end;
        MultiIndex: begin
            DoingStatus := true;
            DoSend(SBuffptr, GrpAdr, NumBytes);
            DoStatus;
            DoingStatus := false;
            end;
        ReceiveIndex: begin
            RecvFinished := false; 
            DoingStatus := true;
            DoReceive(RBuffPtr, false);
            DoStatus;
            if RecvFinished then
                begin
                DoStatus;
                RecvFinished := false;
                end;
            DoingStatus := false;
            ChangeWindow(TypeWinIndex);
            end;
        EchoIndex: DoEcho;
        RandomIndex: DoRandom;
        IntelIndex: DoIntel;
        PromIndex: begin
            RecvFinished := false;
            DoingStatus := true;
            DoReceive(RBuffPtr, true);
            DoStatus;
            if RecvFinished then
                begin
                DoStatus;
                RecvFinished := false;
                end;
            DoingStatus := false;
            ChangeWindow(TypeWinIndex);
            end;
        LoadIndex: begin
            DoLoadDisplay(LoadIt);
            ChangeWindow(TypeWinIndex);
            end;            
        DisplayIndex: 
            begin
            DoLoadDisplay(DispIt);
            ChangeWindow(TypeWinIndex);
            end;            
        ResetIndex:
            begin
            DoReset;
            DoStatus;
            end;
        StatusIndex: DoStatus;
        SetIndex: SetVar;
        QuitIndex: DoQuit;
        end;
    1: if OutPress then
        begin
        write(chr(7));
        OutPress := false;
        end;
    end;

end.    
