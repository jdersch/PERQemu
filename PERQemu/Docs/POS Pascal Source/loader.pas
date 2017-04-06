{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Loader;

{-----------------------------------------------------------------------------
{
{       Loader - Perq system loader.
{       J. P. Strait    10 Feb 81.  rewritten as a module.
{       Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       This module implements the Perq POS system loader.  Given a run-file
{ name as input, it loads and executes that program.  When the program
{ terminates (normally or abnormally) it returns to the loader which returns
{ to its caller.
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{ Change Log:
{
{ 13 Dec 82  V3.1 Chuck Beckett
{ Fix for changed field name "Heap" in SIT.
{
{ 16 Nov 82  V3.0 Bill Braucher
{ Fixed names for 14-character compiler.
{
{ 20 Aug 82  V2.9 Michael R. Kristofic
{ Changed version # to 2.9 for F.1 release.
{
{  5 Jul 82  V2.8a Colin McPhail
{ Changes to let programs with >32 Kwords of GDB + XST load.
{
{  2 Mar 82  V2.8  Michael R. Kristofic
{ Set 1st word of each GDB to 0.
{
{  6 Jan 82  V2.7  Brad Myers
{ Fixed bug in reporting Qcode version incompatibility if hint succeeds
{
{ 10 Dec 81  V2.6  WJHansen
{ changes to use type long (xxTime, stretch(0), don't use Arith)
{
{ 25 Oct 81  V2.5  John Strait
{ Changes to agree with Memory V2.11.
{ }

{ 21 Jul 81  V2.4  John Strait
{ Change the loader to add segments to the segment table (marked as swapped-
{ out) rather than actually load them into memory.  This means that in some
{ sense the loader becomes part of the memory manager.
{ Add Hints.
{ }

{ 4 Jun 81  V2.3   John Strait
{ Add Virgil headers and comments.
{ }

{ 23 May 81  V2.2   John Strait
{ Use new IOKeyDisable/IOKeyEnable in place of old DisCtlC/EnaCtlC.
{ }

{ 18 May 81  V2.1   Brad Myers
{ Import IO_Unit and IO_Others instead of IO.
{ Added ** to error messages
{ }

{ 13 May 81  V2.0  John Strait
{ Use exceptions.
{ }

{ 13 Apr 81  V1.9  John Strait
{ Fix bug in multi-sector read code.
{ }

{ 10 Apr 81  V1.8  John Strait
{ Use new RunRead module.
{ }

{  6 Apr 81  V1.7  John Strait
{ 1) Fix bug in Q-code version processing.
{ 2) Stuff for virtual memory.
{ 3) Make use of new ProgramSN in RunHeader.
{ }

{ 30 Mar 81  V1.6  Brad Myers.
{ Use local lookup for seg file names.
{ }

{ 25 Mar 81  V1.5  John Strait.
{ Improved retries and messages from retry mechanism.
{ }

{ 25 Mar 81  V1.5  John Strait.
{ Improved retries and messages from retry mechanism.
{ }

{ 20 Mar 81  V1.4  Miles A. Barel
{ Added QCode Version Checks and long file names
{ }

{ 3 Mar 81  V1.3  John Strait.
{ Improve retry mechanism to immediately do a recalibrate after an
{ address error.
{ }

{ 1 Mar 81  V1.2  John Strait.
{ Speed up the loader by using multi-sector reads from the hard disk.
{ If errors, abort loading before loading the stack segment.
{ }

{ 23 Feb 81  V1.1  RFR.
{ Changed <= to = FSLookup test.
{ }

{ 10 Feb 81  V1.0  John Strait.
{ Start file.
{ }
{-----------------------------------------------------------------------------}

exports


   const LoaderVersion = '3.0';
   

   procedure Load( RunFileName: String );


private



   {$R-}
   
   
   imports RunRead from RunRead;
   imports System from System;
   imports FileSystem from FileSystem;
   imports Memory from Memory;
   imports Virtual from Virtual;
   imports Perq_String from Perq_String;
   imports DiskIO from DiskIO;
   imports ReadDisk from ReadDisk;
   imports IO_Others from IO_Others;
   imports IOErrors from IOErrors;
   imports Stream from Stream;



const
        DEBUG = false;
        Initialize = false;     { true indicates initialize GDB's to 0 }




var
        RunFile: RunFileType;
        CS,                             { initial code segment }
        SS,                             { initial stack segment }
        GP,                             { Initial Global Pointer }
        TP,                             { Initial Top Pointer }
        OldTP: integer;                 { old top pointer }

        OTP,                            { TP variables for 32 bit arithmetic }
        NTP  : packed record
                 case boolean of
                   true:  (msw, lsw : integer);
                   false: (full32   : long)
               end;
        
        ATPW1,                          { (NewTP - OldTP) : 3 variables reqd. }
        ATPW2,                          { in case difference >= 32 Kwords     }
        ATPW3  : integer;
        
        RunHead: RunInfo;
        
        FirstSeg,                       { first segment in run file }
        FirstUserSeg,                   { first user segment in run file }
        LastSeg: pSegNode;              { last segment so far }
        LoadData: integer;              { segment for internal allocation }
        Errors: Boolean;
        
        {$ifc SysTiming then}
        StartTime, EndTime: record case Integer of
                              1: (D: Double);
                              2: (DW: long);
                              3: (FSB: FSBit32)
                              end;
        {$endc}

procedure Load( RunFileName: String );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Given a run-file name as input, this procedure loads and executes
{       that program.  When the program terminates (normally or abnormally)
{       it returns to the loader which returns to its caller.
{
{ Parameters:
{       RunFileName - Name of the .RUN file to load.  ".RUN" is appended if
{       it is not already present.
{
{-----------------------------------------------------------------------------}

label 99;
var IgnoreB: Boolean;

  handler ExitProgram;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       This handles the ExitProgram exception when raised in the loader or
{       the user program.  It is necessary to catch this exception in order
{       that the loader can do necessary clean-up.
{
{-----------------------------------------------------------------------------}

  begin { ExitProgram }
    goto 99
  end;

  procedure ReadRun;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReadRun reads the run file which describes the program to be loaded.
{
{-----------------------------------------------------------------------------}

  var FirstImp, LastImp, I: pImpNode;
      S: pSegNode;
      K: integer;
      System: Boolean;
      Tmp: String;
      DwOffset: Integer;

    Handler ResetError( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       This handles errors raised by Reset when the .RUN file cannot be found.
{
{-----------------------------------------------------------------------------}

    begin { ResetError }
      Writeln('** Loader-F-', FileName, ' not found.');
      Exit(Load)
    end { ResetError };

  begin { ReadRun }
    Tmp := RunFileName;
    ConvUpper(Tmp);
    if Pos(Tmp,'.RUN') = 0 then
      AppendString(RunFileName,'.Run');
    Reset(RunFile,RunFileName);
    ReadRunFile(RunFile,LoadData,RunHead,FirstSeg,FirstUserSeg,LastSeg,True);
    if FirstSeg = nil then
      begin
        if RunHead.RFileFormat <> RFileFormat then
          Writeln('** Loader-F-', RunFileName,
                  ' has an incompatible run file format.')
        else
          Writeln('** Loader-F-', RunFileName, ' is ill-formed.');
        Errors := True;
        Exit(ReadRun)
      end;
    if not RunHead.System in [false,true] then
    else
      begin RunHead.System := false; RunHead.Version := -1 end;
    if RunHead.System then
      begin
        Writeln('** Loader-F-', RunFileName, ' was linked as a System,');
        Writeln('          it cannot be run as a user program.');
        Errors := True;
        Exit(ReadRun)
      end;
    if RunHead.Version <> SystemVersion then
      begin
        Writeln('** Loader-F-',RunFileName,' was linked with System.',
                RunHead.Version:1,'.Run');
        Writeln('          but the current system is version ',
                SystemVersion:1,'.');
        Writeln('          You must re-link.');
        Errors := True;
        Exit(ReadRun)
      end;
    GP := RunHead.InitialGP;
    DwOffset := Shift(RunHead.CurOffset,-1);
    If ODD(RunHead.CurOffset) Then DwOffset := DwOffset + 1;
    RunHead.StackSize := RunHead.StackSize + ((DwOffset + 127) div 128)
  end { ReadRun };

  procedure LoadCodeSegments;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       LoadCodeSegments loads all the code segments which make up the
{       program (with the exception of the ones which are already part
{       of the system).
{
{-----------------------------------------------------------------------------}

  var S: pSegNode;
      LastDw: Integer;
      CodeSeg: Integer;

    procedure LoadSeg;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       LoadSeg loads a single code segment.  It first determines whether the
{       code segment is already present in memory by calling the memory
{       manager.  If the code segment is not already present in memory, it
{       is loaded with a multi-sector read (if on hard disk) or with single
{       sector reads (if on the floppy).
{
{-----------------------------------------------------------------------------}

    { Load Code Segment in file FName and set S^.SSN,
        set S^.SSN to -1 if unsuccessful }
      var Seg: SegmentNumber;
          FName: FNString;
          Blks: Integer;
          FId: FileId;
          SId: SegId;
          Ignore: Integer;
          BufferIndex: Integer;
          FirstAddress: record case Integer of
                          1: (A: DiskAddr);
                          2: (D: Double)
                          end;
          pData: ptrDiskBuffer;
          Buffer: pSegBlock;
          Header: PtrHeader;
          Addr: DiskAddr;
          SATEdge: MMEdge;
          FHint: SegHint;
          HintFailed: Boolean;

      handler FSNotFnd( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       This handler is activated if a .SEG file is not found.
{
{-----------------------------------------------------------------------------}

      begin { FSNotFnd }
        Writeln('** Loader-F-', FileName, ' not found.');
        Errors := True;
        Exit(LoadSeg)
      end { FSNotFnd };

    begin { LoadSeg }
      S^.SSN := -1;  { assume the worst }
      FId := S^.Hint.FId;
      SId := FileIdToSegId(FId);
      pData := ReadDisk(SId);
      FHint.FId := FId;
      FHint.Update := pData^.FSData.FileWriteDate;
      HintFailed := (FHint.Word2 <> S^.Hint.Word2) or
                    (FHint.Word3 <> S^.Hint.Word3);
      Header := ReadHeader(SId);
      HintFailed := HintFailed or (Header^.SerialNum <> SId);
      if HintFailed then
        begin
          Write('** Loader-W-Hint failed for ');
          if S^.RootNam = nil then
            ReadSegNames(RunFile,LoadData,FirstUserSeg);
          FName := Concat(S^.RootNam^,'.SEG');
          Writeln(FName, ', using string name.');
          FId := FSLocalLookUp(FName,Ignore,Ignore);
          SId := FileIdToSegId(FId);
          pData := ReadDisk(SId);
          FHint.FId := FId;
          FHint.Update := pData^.FSData.FileWriteDate
        end;
      FindCodeSegment(CodeSeg,FHint);
      if CodeSeg = 0 then
       begin
        Header := ReadHeader(SId);
        Addr := Header^.NextAdr;
        Buffer := Recast(ReadDisk(Addr),pSegBlock);
        Header := ReadHeader(Addr);
        FirstAddress.A := LogAddrToPhysAddr(Header^.NextAdr);
        Blks := Buffer^.ImportBlock-1;
                { number of blocks containing code }
        if (Blks < 1) or (Blks > 128) then
          begin
            if S^.RootNam = nil then
               ReadSegNames(RunFile,LoadData,FirstUserSeg);
            Write('** Loader-F-', S^.RootNam^, '.SEG');
            if Blks < 1 then Write(' contains no code.')
            else Write(' is too long.');
            Writeln;
            Errors := True;
            Exit(LoadSeg)
          end;
        if Buffer^.QVersion <> QCodeVersion then
          begin
            if S^.RootNam = nil then
               ReadSegNames(RunFile,LoadData,FirstUserSeg);
            Writeln('** Loader-F-', S^.RootNam^,
                    '.SEG - QCode version incompatability.');
            Errors := True;
            Exit(LoadSeg)
          end;
        MakeEdge(SATEdge,SATSeg);
        CodeSeg := NewSegmentNumber;
        with SAT^[CodeSeg], SIT^[CodeSeg] do
          begin
            InUse := true;
            NotResident := true;
            Kind := CodeSegment;
            Full := false;
            Heap := false;
            RecentlyUsed := false;
            Moving := false;
            Size := Blks;
            Increment := 0;
            Maximum := Blks - 1;
            RefCount := 1;
            IOCount := 0;
            Freelist := 0;
            BootLoaded := False;
            Mobility := Swappable;
            SwapInfo.DiskLowerAddress := FirstAddress.D[0];
            SwapInfo.DiskUpperAddress := FirstAddress.D[1];
            SwapInfo.DiskId := FId;
            Update := FHint.Update;
            NextSeg := SATSeg;
            SIT^[SATEdge.T].NextSeg := CodeSeg
          end
       end;
      S^.SSN := CodeSeg;        { this is the SSN }
      if S^.ISN = RunHead.ProgramSN then CS := CodeSeg  { main program }
    end { LoadSeg };

  begin { LoadCodeSegments }
    if DEBUG then
      begin Writeln; Writeln('Load code segments'); Writeln end;
    CodeSeg := FirstSystemSeg;
    TP := StackLeader div 2;
    S := FirstSeg;
    while S <> FirstUserSeg do with S^ do { set SSN's }
      begin SSN := CodeSeg;
        CodeSeg := CodeSeg + 1;
        LastDw  := Shift(GDBOff,-1) + Shift(GDBSize,-1) - 1;
        if LastDw > TP then TP := LastDw;
        if DEBUG then
          Writeln('System segment ', SegId, ' is segment ', SSN:1);
        S := Next
      end;
    while S <> nil do with S^ do
      begin
        LoadSeg;
        LastDw  := Shift(GDBOff,-1) + Shift(GDBSize,-1) - 1;
        if LastDw > TP then TP := LastDw;
        if DEBUG then
          Writeln('User   segment ', SegId, ' is segment ', SSN:1);
        S := Next
      end
  end { LoadCodeSegments };

  procedure LoadStackSegment;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       LoadStackSegment loads XST's into the memory stack for all code
{       segments it has already loaded.  External references are resolved
{       at this time.
{
{-----------------------------------------------------------------------------}
        
  type StackArray = array[0..0] of Long;
       pStackArray = ^StackArray;
  var S,T: pSegNode;
      I: pImpNode;
      k: integer;
      XSTEntry: packed record
                  case boolean of
                  true: (SSN, XGP : integer);
                  false:(both     : long)
                end;
      Stack: pStackArray;
      Location: integer;
  begin { LoadStackSegment }
    Location := Shift(FirstUserSeg^.GDBOff,-1)-Shift(FirstUserSeg^.XSTSize,-1);
    Stack := MakePtr(SS, 0, pStackArray);
    S := FirstUserSeg;
    while S <> nil do
      with S^ do
        begin I := ImpList;
        if ((Shift(GDBOff,-1) - Shift(XSTSize,-1)) < Location) then
          begin Writeln('** Loader-F-Overlapping XST/GDBs for seg ',Segid);
            Errors := True
          end;
        Location := Shift(GDBOff,-1) - Shift(XSTSize,-1);
        while I <> nil do with I^ do
          begin
          XSTEntry.XGP := XGP;
                                   { figure out SSN of external segment }
          T := FirstSeg;           { chain through list to the right one }
          while T^.ISN <> XSN do T := T^.Next;
          XSTEntry.SSN := T^.SSN;        { put out it's SSN }
          
          Stack^[Location] := XSTEntry.both;
          Location := Location + 1;
          I := Next
          end;
        { Make room for GDB }
        Stack^[Location] := 0;    { Set 1st double word of global data to 0 }
        if Initialize then
          for k := 1 to Shift(GDBSize,-1) do
            begin Stack^[Location] := 0; Location := Location + 1 end;
        S := Next                      { for all segments }
        end
  end { LoadStackSegment };

begin { Load }
  {$ifc SysTiming then}
  IOGetTime(StartTime.D);
  {$endc}
  IOKeyEnable(True);
  Errors := False;
  CS := 0;

  CreateSegment(LoadData,4,4,200);
  ReadRun;

  if not Errors then
    begin
      SS := StackSegment;
      if DEBUG then writeln('StackSize = ',RunHead.StackSize:1);
      ChangeSize(SS,RunHead.StackSize);
      if DEBUG then writeln('StackIncr = ',RunHead.StackIncr:1);
      SetIncrement(SS,RunHead.StackIncr);
      LoadCodeSegments
    end;

  if not Errors then
    begin
      if DEBUG then begin Write('Code segments loaded, type cr'); Readln end;
      LoadStackSegment
    end;
  
  Close(RunFile);
  DecRefCount(LoadData);
  
  if not Errors then
    begin { adjust top of stack }
      if DEBUG then
        begin Write('type <cr> to adjust top of stack'); Readln end;
        InLineByte( #313 {LDTP} );
        StorExpr(OldTP);
        OTP.msw    := 0;
        OTP.lsw    := OldTP;
        OTP.full32 := OTP.full32 - 1;  { LDTP gives TP+1 as result }
        NTP.msw    := 0;
        NTP.lsw    := Shift(TP,1);
        NTP.full32 := NTP.full32 + 1;  { Allow for the odd word }
        if DEBUG then
          Writeln('OldTP = ',OTP.full32:1,'   NewTP = ',NTP.full32:1);
        
        NTP.full32 := NTP.full32 - OTP.full32;
        
        if ((NTP.msw = 0) and (NTP.lsw < 0)) then
          
          { A positive stack adjustment requiring 16 bits }
          begin
            if ODD(NTP.lsw) then ATPW3 := 1 else ATPW3 := 0;
            ATPW1 := Shift(NTP.lsw,-1);
            ATPW2 := ATPW1
          end
        
        else
          { The easy case: stack adjustment is a normal integer }
          begin
            ATPW1 := NTP.lsw;
            ATPW2 := 0;
            ATPW3 := 0
          end;
        
        LoadExpr(ATPW1);
        InLineByte( #275 {ATPW} );            { bump TP }
        LoadExpr(ATPW2);
        InLineByte( #275 {ATPW} );            { bump TP }
        LoadExpr(ATPW3);
        InLineByte( #275 {ATPW} );            { bump TP }
        if DEBUG then
          begin InLineByte( #313 {LDTP} );
            StorExpr(TP);
            NTP.msw    := 0;
            NTP.lsw    := TP;
            NTP.full32 := NTP.full32 - 1;  { LDTP gives TP+1 as result }
            Writeln('TP = ',NTP.full32:1)
          end;
         
         { load variable routine descriptor }
        if DEBUG then
          begin Write('type <cr> to call user program,  GP = ',
                      GP:1,'   CS = ',CS:1);
            Readln
          end
    end;
  
  {$ifc SysTiming then}
  IOGetTime(EndTime.D);
  LoadTime := EndTime.DW - StartTime.DW;
  IOTime := stretch(0);
  SwapTime := stretch(0);
  MoveTime := stretch(0);
  {$endc}

  if not Errors then
    begin { call user program: }
      LoadExpr(0);          { static link }
      LoadExpr(0);          { routine number }
      LoadExpr(GP);         { global pointer }
      LoadExpr(CS);         { code segment }
      InLineByte( #273 {CALLV} )
    end;
99: IOKeyDisable(IgnoreB)
end { Load }.
