{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program MakeBoot;

{-----------------------------------------------------------------------------
{
{       MakeBoot - Make Boot files.
{       J. P. Strait    10 Feb 81.  rewritten as a program.
{       Copyright (C) Three Rivers Computer Corporation, 1981.
{
{ Abstract:
{       MakeBoot is used to
{           a) Write the microcode boot area of the hard disk or a floppy disk.
{           b) Make a system xxx.Run file and interpreter microcode into
{              xxx.letter.Boot, xxx.letter.MBoot, xxx.letter.ZBoot and
{              xxx.letter.FBoot files.
{
{-----------------------------------------------------------------------------}

{$Version 5.4}
{-----------------------------------------------------------------------------
{ Change Log:
{
{ 23 Sep 83  5.4  Sandeep Johar
{   1> Added the EIODiskType variable, only defines when IsEIOSystem. Used to
{   determine the kind of disk we are targetting to. The /EIO5 switch is used 
{   to select 5.25" disks and EIO board combination. The /EIO switch implies 
{   micropolis Disk drives.
{   2> Fixed a bug in writing out the enlarged name segment. 
{
{ 27 Apr 83  5.3  Sandeep Johar
{   Changed the way EIOSystem is detected. Also for automatic we build all the
{   boot files as the default. These were changes done by D Golub and lost at
{   ICL.
{
{ 20 Apr 82  5.2  Dave Anderson ICL
{   Added Import of DiskDefs to allow selection of ICL CIO Micropolis ucode.
{   Resolved resulting name clashes!!
{   Changed to not do floating point boot file stuff if constant GetDblFloat
{   (imported from System) is false.
{
{ 4 Mar 83   5.1  Sandeep Johar
{   Fixed so that makeboot can handle more than 64 segment names. The number
{   is now 128. The size of the name segment is 2 blocks.
{
{ 2 Mar 83   5.0  Sandeep Johar
{   Added the floating point boot file build sequence.
{
{ 1 Mar 83   4.92 Sandeep Johar
{   Use configuration to figure out the kind of disk and use appropriate
{   microcode. This option is overridable on the command line. The /EIO 
{   /CIO switch are placed at the end of the command line. they only 
{   effect the choice of SysB and the IO microcodes. The makeing of the 
{   the ZBoot files are not effected.
{
{ 23 Feb 83  4.91 Sandeep Johar
{   Add the Z80 boot file creation stuff for the EIO case.
{
{  8 Feb 83  4.9  Brad Myers
{   Make the number of blocks in the boot file variable depending on how much
{        needed.
{   Ask question for screen size and allocate room accordingly.
{   Import IO_Unit instead of IO.
{   Import Configuration to see about the current screen size.
{
{ 27 Jan 83  4.8  Roger Riggs
{   Initialize ChangeCheckSum to zero in FixUpDiskAddresses
{
{ 13 Dec 82  4.7  Chuck Beckett
{   Changed to reflect new name "Heap" in SIT.
{
{ 16 Nov 82  4.6  Bill Braucher
{   Fixed names for 14-character compiler.
{
{ 25 May 82  4.5  Loretta Ferro
{   Makeboot no longer uses ETHER10.MICRO as default
{
{ 27 Jan 81  4.4  Brad Myers
{   Bug when overwriting a micro file.
{   Config file name error.
{
{  7 Jan 81  4.3  WJHansen
{   Put Micro in a separate segment so stack is smaller.
{
{ 31 dec 81  4.2  Ellen Colwell
{ Added provisions for marking boot files as type 15(boot) or 16(mboot)
{ in the File Information Block.
{ }

{ 25 Oct 81  4.1  John Strait
{ Changes to agree with V2.11 Memory.
{ }

{  3 Sep 81  4.0  Brad Myers
{ Added /Build switch that automatically builds a system without asking
{ questions.
{ }

{  1 Jun 81  3.9  John Strait
{ Add system configuration file: describes which segments are swappable.
{ Fix bugs.
{ }

{  2 May 81  3.8  George Robertson
{ Changed microcode boot area from 3k to 3.75k.
{}

{ 24 Apr 81  3.7  John Strait
{ Fix bug in FixUpDiskAddresses which prevented copying boot files.
{ Don't FixUpDiskAddresses when writing to the floppy.
{ }

{  6 Apr 81  3.6  John Strait
{ Retrofit changes made after 31 Mar 81.
{ Change FindFile to use FSSearch.
{ }

{ 10 Apr 81       John Strait
{ Convert to new RunRead module.
{ }

{ 31 Mar 81       John Strait
{ Add support for virtual memory.
{ }

{  8 Apr 81  3.5  Brad Myers
{  Fixed bug with boot file name; FSSearch for FontName
{}

{  1 Apr 81  3.4  Brad Myers
{       1) Removed all FPathName processing.
{       2) Provide defaults for interpreter microcode files
{       3) Use LocalLookup for names to be overwritten
{       4) Allow specification of RootName with .RUN on end
{       5) Imports FileAccess and FileDir instead of Spice-mumble
{}

{ 19 Mar 81  3.3  Brad Myers
{ Changed for new FS: PERQ_String; changed Vble PathName to FPathName.
{}

{ 18 Mar 81  3.2  John Strait
{ Allow copying an existing .Boot file or .MBoot file.
{ If rewriting some .Boot or .MBoot file, delete it first.  This is necessary
{   because the filesystem doesn't truncate on close, and if the new file is
{   shorter than the old one, the boot microcode will get a checksum error.
{}

{  9 Mar 81  3.1  John Strait
{ Make the explicitly typed <PathName> the first place to look for files, but
{ also look in the current path.  Reimplement most all abilities of the
{ original WriteBoot--allow rewriting only selected boot files.
{}

{  7 Mar 81  3.0  John Strait
{ Merge MakeBoot V1.3 and WriteBoot 2.0 into a single program.
{}


{ Old MakeBoot change history: }

{  4 Mar 81  1.3  John Strait
{ Allow the run file name to be typed as <PathName>RunFileName.  Prefix
{   all file names by <PathName>.
{}

{ 23 Feb 81  1.2  Don Scelza
{ Changed Header to RunHeader.  Name conflict with the CMU file system.
{}

{ 20 Feb 81  1.1  John Strait
{ Convert to system C.3.
{ }

{ 10 Feb 81  1.0  John Strait
{ Start File.
{ }



{ Old WriteBoot change history.

{ 23-Feb-81 GGR   Version 2.0
{     Added support for Spice Segment System.
{     Removed verify option.
{}

{ 8-Oct-80  JQS   Version 1.1
{     Clean-up and make DAS code a bit more consistent.
{ }

{ 30-Sep-80  DAS   Version 1.0
{     Added code to write boot floppies.
{ }
{-----------------------------------------------------------------------------}





   imports RunRead from RunRead;
   imports FileUtils from FileUtils;
   imports Memory from Memory;
   imports Perq_String from Perq_String;
   imports CmdParse from CmdParse;
   imports System from System;

   imports FileAccess from FileAccess;
   imports FileDir from FileDir;
   imports DiskIO from DiskIO;
   imports IO_Unit from IO_Unit;
   imports IOErrors from IOErrors;
   imports ControlStore from ControlStore;
   imports Configuration from Configuration;
   imports DiskDef from DiskDef;



const   MakeBVersion = '5.4';
        Initialize = False;     { true indicates initialize GDB's to 0 }
        MaxChunks = 512;        { Maximum number of allocatable 256 word }
                                { chunks - NOTE: 256 only allows use of }
                                { first 64K of memory }
        MaxSAT = 128;           { maximum of MaxSegment and 128 }
        MaxSIT = MaxSegment;    { maximum of MaxSegment and 96 }
        FirstCodeSeg = SysNameSeg+1; { **** watch out for this one **** }


        LengthBoot = 24;        { size of boot microcode in blocks }
        LengthInterpreter = 36; { size of interpreter microcode in blocks }
        
        EndBoot = LengthBoot - 1;
        EndInterpreter = LengthInterpreter - 1;
        
        LWinDB = WpDB - 1;      { last word in disk block }
        LWinFS = WpFS - 1;      { last word in floppy sector }
        FFT = 1;                { floppy first track }
        
        DefaultFont = 'Fix13.Kst'; { default character set name }
        
        MaxSNames = 512 div WordSize(SysSegName) - 1;


type    SATimage = array[0..MaxSAT] of SATEntry;
        pSATimage = ^SATimage;
        
        SITimage = array[0..MaxSIT] of SITEntry;
        pSITimage = ^SITimage;
        
        SNameImage = array[0..MaxSNames] of SysSegName;
        pSNameImage = ^SNameImage;
        
        pSegmentInfo = ^SegmentInfo;
        SegmentInfo = record
                        Name: SNArray;
                        Mobility: SegmentMobility;
                        Done: Boolean;
                        Next: pSegmentInfo
                        end;
        
        MicroArray = array[0..#7777] of MicroInstruction;
        pMicroArray = ^MicroArray;


var     RootName, RunName, BootName, MBootName, FontName, 
        FBootFile, ZBootFile: PathName;

        FName : PathName;
        RunSuffix : PString;
        PartName: PathName;             { name of partition where boots are }
        WriteZBoot: Boolean;            { Should Z80 boot file be written }
        ZFid : FileID;                  { Z80 boot file id being created. }
        Zbits, ZBlks : Integer;
        
        WriteFBoot: Boolean;
        FltFid : FileID;
        FltBits, FltBlks : Integer;
        
        Bits, Blks: Integer;

        HasDiskType : Boolean;          { Disk type specified in command line }
        IsEIOSystem: Boolean;           { The specified disk is the EIO disk }
        EIODiskType: (Micropolis, FiveQtrDisk);
        { current memory location: }
        CurSegNum: SegmentNumber;       { current SSN }
        CurSegBase,                     { base block number of current SSN }
        CurBlock,                       { block number in current SSN }
        NxtSegBase: Integer;            { base block number of next SSN }
        
        CS,                             { initial code segment }
        SS,                             { initial stack segment }
        GP,                             { Initial Global Pointer }
        TP,                             { Initial Top Pointer }
        OldTP: Integer;                 { old top pointer }
        
        RunHead: RunInfo;
        
        FirstSeg,                       { first segment in run file }
        FirstUserSeg,                   { first user segment in run file }
        LastSeg: pSegNode;              { last segment so far }

        FreeSeg: Integer;               { first free segment }

        Boot: file of SegBlock;         { output Boot file }
        BootId: Integer;                { file system Id of Boot file }
        SATp: pSATimage;
        SITp: pSITimage;
        SNamep: pSNameImage;
        OldSeg: SegmentNumber;
        
        SegList: pSegmentInfo;
        
        ArrowPos: Integer;
     
      
        MicroSeg: SegmentNumber;        { where to put the Micro array }
        Micro: pMicroArray;             { buffer for micro instructions }
        Buffer, VerBuffer: pSegBlock;   { output and verify buffers}
        Block, Word: integer;           { current block and word in system }
                                        { boot area on hard disk}
                                            
        PartBlock: integer;
            
        Sector, Cylinder, Bytes: integer; { current sector, cylinder and word }
                                        { in the boot area on the floppy }
        CheckSum    : integer;
        DoFloppy    : boolean;
        
        BootWrite   : boolean;          { during write of boot microcode }
        cheat       : DiskCheatType;
        OutputFile  : PathName;
        ok          : boolean;
        Disk        : integer;
        Ptr, Vptr   : ptrDiskBuffer;
        FId         : FileId;
        BootChar    : Char;
        BootIndex   : integer;
        SystemDA    : DiskAddr;
        InterpDA    : DiskAddr;
        InfoBlk     : FSBit32;
        InfoHeader  : ptrHeader;
        
        MStatPtr: IOStatPtr;
        HeadBlkPtr: IOHeadPtr;
    
        automatic   : boolean;
        autoBootKey : Char;
        
        FileData    : ptrFSDataEntry;   { used to set type=16|15 of boot files }        
        MBIsPortrait: boolean; { screen should be Portrait }
        MBScreenSize: Integer; { number of blocks in the screen }

  Procedure GetLn(var s: PathName; autoDef: PathName);
     begin
     if automatic then begin
                       s := autoDef;
                       writeLn(s);
                       end
     else readln(s);
     end;

  function Ask(Prompt, Default: string): boolean;
  var InLine: String;
      RetVal: Boolean;
  begin
    Write(Prompt,' [',Default,']: ');
    GetLn(InLine, Default);
    if Length(InLine) = 0 then InLine := Default;
    Ask := InLine[1] in ['T','t','Y','y']
  end {Ask};
  
  
  function GetBootChar:  Char;
     begin
     if automatic then begin
                       GetBootChar := AutoBootKey;
                       WriteLn(AutoBootKey);
                       end
     else begin
          if Eoln then GetBootChar := ' '
          else GetBootChar := Input^;
          ReadLn;
          end;
     end;

  function FindFile( var FileName: PathName ):  FileId;
  var Ignore: Integer;
  begin { FindFile }
    FindFile := FSSearch(FSSysSearchList,FileName,Ignore,Ignore);
  end { FindFile };

       
  procedure FloppyAddress;
      { convert disk Block into floppy Cylinder and Sector }
  const
        FSpT = 26;              { floppy sectors per track }
        FFS = 1;                { floppy first sector }
  var S: Integer;
  begin { FloppyAddress }
    S := Block * FSpDB + FIdS;
    Cylinder := S div FSpT + FFT;
    Sector := S mod FSpT + FFS
  end { FloppyAddress };
                                        
    
  procedure FloppyWrite;
      { write the first WpFS words (WpFS*2 bytes) of Buffer^.Block }
      { to the floppy at Sector, Cylinder }
  var Address,Zero: double;
      I,J: integer; 
  begin {FloppyWrite}
    I := 0;
    repeat I := I + 1;
      J := 0;
      repeat J := J + 1;
        Address[0] := Sector;
        Address[1] := Cylinder;
        UnitIO(Floppy,Recast(Buffer,IOBufPtr),IOWrite,WpFS*2,
               Address,nil,MStatPtr);
        if MStatPtr^.SoftStatus = IOEIOC then
          begin
            if (I <> 1) or (J <> 1) then
              Writeln(' Recovered.');
            Exit(FloppyWrite)
          end;
        Write(' Floppy error, status = ',MStatPtr^.SoftStatus:1);
        if J < 5 then Writeln(', retrying.')
      until J = 5;
     Zero[0] := 1;
     Zero[1] := 0;
     if I < 5 then
       begin Writeln(', recalibrating...');
         UnitIO(Floppy,Recast(Buffer,IOBufPtr),IOReset,0,Zero,nil,MStatptr);
         Writeln('done, retrying.')
       end
    until I = 5;
    Writeln(', aborted.');
    exit(MakeBoot)
  end {FloppyWrite};

   
  procedure DiskWrite(Blk : integer; Ptr : pSegBlock);
  var I, J: Integer;
      Address: Double;
  begin { DiskWrite }
    If IsEIOSystem And (EIODiskType = FiveQtrDisk) Then 
      Begin
        Address[0] := Blk Mod 16;
        Address[0] := LOR(Address[0], Shift(Blk Div 16, 8))
      End Else Address[0] := Blk;
    Address[1] := 0;
    HeadBlkPtr^.SerialNum[0] := -1;
    HeadBlkPtr^.SerialNum[1] := -1;
    HeadBlkPtr^.LogBlock := Blk;
    I := 0;
    repeat I := I + 1;
      J := 0;
      repeat J := J + 1;
        UnitIO(HardDisk,Recast(Ptr,IOBufPtr),IOWriteFirst,512,Address,
               HeadBlkPtr,MStatPtr);
        if MStatPtr^.SoftStatus = IOEIOC then
          begin
            if (I <> 1) or (J <> 1) then
              Writeln(' Recovered.');
            Exit(DiskWrite)
          end;
        Write(' Disk error, status = ',MStatPtr^.SoftStatus:1);
        if J < 5 then Writeln(', retrying.')
      until J = 5;
     if I < 5 then
       begin Writeln(', recalibrating...');
         DiskReset;
         Writeln('done, retrying.')
       end
    until I = 5;
    Writeln(', aborted.');
    exit(MakeBoot)
  end { DiskWrite };


  procedure WriteBlock( var Buffer: SegBlock; N: integer; CSum: Boolean );
     { write Buffer to memory and add N to CurBlock and Block.
       if CSum, accumulate the CheckSum }
  var Blocks, I, J: integer;
      P: pSegBlock;
  begin { WriteBlock }
    Boot^ := Buffer;
    for I := 1 to N do
      begin Put(Boot);
        if CSum then
          for J := 0 to 255 do
            CheckSum := CheckSum + Buffer.Block[J]
      end;
    CurBlock := CurBlock + N;
    Block := Block + N
  end { WriteBlock };

   
  procedure WriteWord( W: integer );
     { write W at (Block,Word) in the system boot area or onto floppy }
  var I: integer;
  begin { WriteWord }
    Buffer^.Block[Word] := W;
    if DoFloppy and BootWrite then
      begin
        Word := (Word + 1) mod WpFS;
        if Word = 0 then
          begin
            FloppyWrite;
            PartBlock := (PartBlock + 1) mod 4;
            if PartBlock = 0 then Block := Block + 1;
            Sector := Sector + 1;
            if Sector = 27 then
              begin
                Sector := 1;
                Cylinder := Cylinder + 1
              end
          end
      end
    else
      if BootWrite then
        begin
          Word := (Word + 1) mod WpDB;
          if Word = 0 then
            begin
              DiskWrite(Block,Buffer);
              Block := Block + 1 
            end
        end
      else
        begin
          Word := (Word + 1) mod WpDB;
          if Word = 0 then WriteBlock(Buffer^,1,False)
        end;
    CheckSum := CheckSum + W
  end { WriteWord };
    
    
  procedure WriteMicro( Start, Stop: integer );
      { write microinstructions from Start to Stop into the system boot area }
  const Complement = -1;
  var TLate: TransMicro;
      Addr: integer;
      UWord: MicroInstruction;
  begin { WriteMicro }
    for Addr := Start to Stop do
      begin UWord := Micro^[Addr];
        with TLate do
          begin
            ALU23:=UWord.ALU23;
            ALU0:=UWord.ALU0;
            W:=UWord.W;
            ALU1:=UWord.ALU1;
            A:=UWord.A;
            Z:=UWord.Z;
            SFF:=UWord.SFF;
            H:=UWord.H;
            B:=UWord.B;
            JmpCnd:=UWord.JmpCnd;
            Word3 := UWord.Word3;
            WriteWord(LNot(Word1));
            WriteWord(LNot(Word2));
            WriteWord(LNot(Word3))
          end
      end
  end { WriteMicro };


  procedure BootMicroCode;
  var BName: string;
      BFile: MicroFile;
      Ignore: Integer;
      HaveFile: boolean;
      firstTime: Boolean;
  begin { BootMicroCode }
    CheckSum := 0;
    Block := 0;
    FloppyAddress;
    Word := 0;
    HaveFile := False;
    BootWrite := True;
    BName := '';
    if automatic then 
      Begin
        If (IsEIOSystem) And (EIODiskType = micropolis) 
          Then BName := 'EIOSysB'
          Else If (IsEIOSystem) And (EIODIskType = FiveQtrDisk)
            then BName := 'EIO5SysB'
            Else BName := 'CIOSysB';
      End;
    firstTime := true;
    repeat
      Write('boot microcode file: ');
      GetLn(BName,BName);
      if BName <> '' then
        begin
          BName := Concat(BName,'.Bin');
          if FindFile(BName) <> 0 then
            begin
              HaveFile := true;
              Reset(BFile,BName);
              while not Eof(BFile) and (BFile^.Adrs >= 0) do
                begin
                  if (BFile^.Adrs < #4000) or (BFile^.Adrs > #7776) then
                    Writeln('** Micro address outside of boot area: ',
                            BFile^.Adrs:1:-8)
                  else Micro^[BFile^.Adrs] := BFile^.MI;
                  Get(BFile)
                end;
              Close(BFile)
            end
          else Writeln('** ', BName, ' not found.')
        end;
      if automatic then
        Begin
        if firsttime then
          Begin
            If IsEIOSystem 
              then BName := 'EIOVfy'
              else BName := 'CIOVfy';
          End
          Else BName := '';
        End;
      firstTime := false;
    until HaveFile and (BName = '');
    WriteMicro(#4000,#7776);
    while (Word <> 0) or (Block <= EndBoot) do WriteWord(-CheckSum);
    Writeln;
    Writeln('Boot area written.')
  end { BootMicroCode };


  procedure SystemCode;
  var Tmp: String;
      Ignore: integer;
      OldBootName: String;
      OldBootId: FileId;
      OldBoot: file of SegBlock;
      S: pSegmentInfo;
      BlocksSwappable, BlocksUnSwappable, BlocksFree: Integer;
      BootedMemoryInBlocks: Integer;
    
    
    procedure NameSegment( SegNum: Integer; SegName: SysSegName );
    var I, Len: Integer;
    begin { NameSegment }
      SNamep^[SegNum] := '        ';
      if SegLength > SysSegLength then Len := SysSegLength
      else Len := SegLength;
      for I := 1 to Len do SNamep^[SegNum][I] := SegName[I]
    end { NameSegment };
    
    
    procedure MakeSeg( SegName: SNArray; FileName: PathName;
                       SegSize: Integer; K: SegmentKind; IU: Boolean;
                       M: SegmentMobility );
     { make segment CurSegNum starting at NxtSegBase of size SegSize.  All of
       memory must be allocated by this procedure, and must be allocated in
       order of monotonically increasing addresses. }
    var I, Len: Integer;
        S: pSegmentInfo;
        S1, S2: SNArray;
        SSegName: SysSegName;

  
      procedure PrintMemLoc;
      var D: array[1..10] of char;
          i, l: integer;
          Block, Offset: integer;
      begin { PrintMemLoc }
        Write(SegName);
        if FileName = '' then Write(' ':6)
        else Write(' from ');
        Write(FileName);
        if Length(FileName) < 40 then Write(' ':40 - Length(FileName))
        else Write(' ');
        Write(' segment ', CurSegNum:2);
        Block := CurSegBase + CurBlock;
        Offset := 0;
        l := 0;
        repeat l := l + 1;
          D[l] := Chr(Offset mod 8 + Ord('0'));
          Offset := (Offset + 256 * (Block mod 8)) div 8;
          Block := Block div 8
        until (Block = 0) and (Offset = 0);
        Write(' at #');
        i := l;
        repeat Write(D[i]); i := i - 1 until i = 0;
        repeat Write(' '); l := l + 1 until l >= 8;
        with SITp^[CurSegNum] do
          if Mobility = Swappable then Write('SW')
          else
            if Mobility = LessSwappable then Write('LS')
            else
              if Mobility = UnSwappable then Write('US')
              else
                if Mobility = UnMovable then Write('UM');
        Writeln
      end { PrintMemLoc };


    begin { MakeSeg }
      if SegSize > 0 then
        with SATp^[CurSegNum], SITp^[CurSegNum] do
          begin
            for I := 1 to SysSegLength do
              if I > SegLength then SSegName[I] := ' '
              else SSegName[I] := SegName[I];
            NameSegment(CurSegNum,SSegName);
            CurSegBase := NxtSegBase;
            NotResident := not IU;
            Moving := false;
            RecentlyUsed := false;
            Heap := false;
            Kind := K;
            InUse := IU;
            Full := True;
            Lost := False;
            BaseUpper := Shift(CurSegBase,-8);
            BaseLower := LAnd(CurSegBase,#377);
            Size := SegSize - 1;
            if K = DataSegment then
             begin
              Increment := 0;
              if InUse then Maximum := Size else Maximum := 0;
              FreeList := 0
             end
            else
             with Update do
              begin
               Year := 0;
               Month := 1;
               Day := 1;
               Hour := 0;
               Minute := 0;
               Second := 0
              end;
            if InUse then RefCount := 255
            else RefCount := 0;
            IOCount := 0;
            Mobility := M;
            S := SegList;
            repeat
              if S <> nil then
                begin
                  S1 := S^.Name;
                  S2 := SegName;
                  for I := 1 to SegLength do S1[I] := UpperCase(S1[I]);
                  for I := 1 to SegLength do S2[I] := UpperCase(S2[I]);
                  if S1 = S2 then
                    begin
                      Mobility := S^.Mobility;
                      S^.Done := True;
                      S := nil
                    end
                  else S := S^.Next
                end
            until S = nil;
            BootLoaded := True;
            SwapInfo.BootLogBlock := Block;
            SITp^[OldSeg].NextSeg := CurSegNum;
            OldSeg := CurSegNum;
            CurBlock := 0;
            NxtSegBase := CurSegBase + SegSize;
            PrintMemLoc;
            if InUse then
              if Mobility <= UnSwappable then
                BlocksUnSwappable := BlocksUnSwappable + SegSize
              else BlocksSwappable := BlocksSwappable + SegSize
            else BlocksFree := BlocksFree + SegSize
          end
    end { MakeSeg };
  
  
    procedure LoadFont;
    { Load the font file }
      var StartLoc, FSize, Ignore: Integer;
          FFile: file of SegBlock;
          Ch: Char;
    begin { LoadFont }
      if FSSearch(FSSysSearchList,FontName,FSize,Ignore) = 0 then
        begin
          Writeln('** ', FontName, ' not found.');
          Exit(LoadFont)
        end;
      Reset(FFile,FontName);
      CurSegNum := FontSeg;
      MakeSeg('*Font*  ', FontName, FSize, DataSegment, True, UnSwappable);
      Close(FFile);
      Reset(FFile,FontName);
      while not EOF(FFile) do
        begin WriteBlock(FFile^,1,True);
          Get(FFile)
        end;
      Close(FFile)
    end { LoadFont };

  
    procedure LoadStandardSegments;
    var i: integer;
        Buffer: SegBlock;
    begin { LoadStandardSegments }
      for i := 0 to 255 do Buffer.Block[i] := 0;
      for i := 0 to MaxSAT do with SATp^[i] do
        begin NotResident := True;
          Moving := False;
          RecentlyUsed := False;
          Heap := False;
          Kind := CodeSegment;
          InUse := False;
          Full := True;
          Lost := False;
          BaseUpper := 0;
          BaseLower := 0;
          Size := 0
        end;
      for i := 0 to MaxSIT do with SITp^[i] do
        begin Increment := 0;
          Maximum := 0;
          FreeList := 0;
          RefCount := 0;
          IOCount := 0;
          Mobility := Swappable;
          BootLoaded := False;
          SwapInfo.DiskId := 0;
          SwapInfo.DiskLowerAddress := 0;
          SwapInfo.DiskUpperAddress := 0;
          NextSeg := 0
        end;
      for i := 0 to MaxSNames do SNamep^[i] := '        ';
      OldSeg := SATSeg;
      { create segment for System Segment Table; max # segments is 128 }
      CurSegNum := SATSeg;
      MakeSeg('*SAT*   ', '', 1, DataSegment, True, UnMovable);
             { room for 128 segments }
      WriteBlock(Buffer,1,False);
      CurSegNum := SITSeg;
      MakeSeg('*SIT*   ', '', 3, DataSegment, True, UnSwappable);
             { room for 96 entries }
      WriteBlock(Buffer,3,False);
      { The following is a hack so that the cursor will start on a 512 word
      { boundary }
      CurSegNum := CursorSeg;
      MakeSeg('*Cursor*', '', 2, DataSegment, True, UnMovable);
      WriteBlock(Buffer,2,True);
      { The following is a hack so that the screen will start on a 512 word
      { boundary }
      CurSegNum := ScreenSeg;
      MakeSeg('*Screen*', '', MBScreenSize, DataSegment, True, UnMovable);
      CurSegBase := CurSegBase + MBScreenSize  { skip screen segment on .BOOT }
                                               { file }
    end { LoadStandardSegments };
  
  
  
    procedure LoadCodeSegments;
    var S: pSegNode; LastWord: integer;
        Buffer: pSegBlock;
    
    
      procedure LoadSeg;
      { Load Code Segment in file FName and set S^.SSN,
          set S^.SSN to -1 if unsuccessful }
        var FName: String;
            Blks: Integer;
            FId: Integer;
            B: Integer;
            P: pSegBlock;
            Answer: String;
            Ignore: Integer;
      begin
        S^.SSN := -1;  { assume the worst }
        FName := Concat(S^.RootNam^, '.SEG');
        FId := FSLocalLookup(FName,Ignore,Ignore);
        if FId = 0 then
          begin Writeln('** ', FName, ' not found.');
            S^.SSN := -1;
            Exit(LoadSeg)
          end;
        FSBlkRead(FId,0,Recast(Buffer,pDirBlk));
        Blks:=Buffer^.ImportBlock-1;  { number of blocks containing code }
        if CurSegBase = -1 then
          begin               { unsucessful segment creation - no room }
            Writeln('** Insufficient memory to load ',FName);
            Exit(LoadSeg)
          end;
        MakeSeg(S^.SegId, FName, Blks, CodeSegment, True, UnSwappable);
        if Buffer^.ProgramSegment then
          if CS = 0 then CS := CurSegNum;
        for B := 1 to Blks do
          begin FSBlkRead(FId,B,Recast(Buffer,pDirBlk));
            WriteBlock(Buffer^,1,True)
          end;
        S^.SSN:=CurSegNum        { this is the SSN }
      end { LoadSeg };
    
    
    begin { LoadCodeSegments }
      New(0,256,Buffer);
      CurSegNum := FirstCodeSeg;
      TP :=StackLeader-1;
      S := FirstSeg;
      while S <> nil do with S^ do
        begin
          LoadSeg;
          CurSegNum := CurSegNum + 1;
          LastWord := GDBOff + GDBSize - 1;
          if LastWord > TP then TP := LastWord;
          S := Next
        end;
      Dispose(Buffer)
    end { LoadCodeSegments };
    
  
    procedure LoadStackSegment;
    var S,T: pSegNode;
        I: pImpNode;
        k: integer;
        Location, Offset: integer;
        Buffer: SegBlock;
        Temp: Integer;
        
        
      procedure Advance( N: integer );
      begin { Advance }
        if N > 0 then
          begin
            Location := Location + N;
            Offset := Offset + N;
            if Offset >= 256 then
              begin
                WriteBlock(Buffer,Offset div 256,True);
                Offset := Offset mod 256
              end
          end
      end { Advance };
      
        
    begin { LoadStackSegment }
      Temp := CurSegNum;
      CurSegNum := SS;
      for k := 0 to 255 do Buffer.Block[k] := 0;
      MakeSeg('*Stack* ', '', RunHead.StackSize, DataSegment, True, UnSwappable);
      if CurSegBase = -1 then
        begin
          Writeln('** Insufficient space for Stack Segment');
          Exit(MakeBoot)
        end;
      SS := CurSegNum;    { stack segment number }
      Location := 0;
      Offset := 0;
      Buffer.Block[Offset] := TP;
      Advance(1);
      Buffer.Block[Offset] := GP;
      Advance(1);
      S := FirstUserSeg;
      while S <> nil do
        with S^ do
          begin I := ImpList;
            if GDBOff - XSTSize < Location then
               Writeln('** Overlapping XST/GDB''s');
            Advance(GDBOff - XSTSize - Location);
            while I <> nil do with I^ do
              begin
                Buffer.Block[Offset] := XGP;
                Advance(1);
                                 { figure out SSN of external segment }
                T := FirstSeg;   { chain through list to the right one }
                while T^.ISN <> XSN do T := T^.Next;
                Buffer.Block[Offset] := T^.SSN;        { put out it's SSN }
                Advance(1);
                I := Next
              end;
            { Make room for GDB }
            if Initialize then
              for k:=1 to GDBSize do
                begin Buffer.Block[Offset] := 0; Advance(1) end;
            S:=Next                      { for all segments }
          end;
      if Offset <> 0 then WriteBlock(Buffer,1,True);
      CurSegNum := Temp
    end { LoadStackSegment };
    
    
    procedure LoadSNameSegment;
    var P: pSegBlock;
        T: Integer;
    begin { LoadSNameSegment }
      T := CurSegNum;
      CurSegNum := SysNameSeg;
      MakeSeg('*Names* ', '', 2, CodeSegment, True, Swappable);
      P := Recast(SNamep, pSegBlock);
      WriteBlock(P^, 1, True);
      P := RECAST( RECAST(P, Long) + 256, pSegBlock);
      WriteBlock(P^, 1, True);
      CurSegNum := T
    end { LoadSNameSegment };
  
    
    procedure LoadFreeSegment;
     begin { LoadFreeSegment }
      {Figure out how much memory have used so far.  If < 1/4 meg then
       make a 1/4 meg boot file; else make a 1/2 meg boot file.  Create
       the free segment accordingly}
      BootedMemoryInBlocks := NxtSegBase + IOSegSize + 1;
      if BootedMemoryInBlocks <= BlocksInQuartedMeg then
          BootedMemoryInBlocks := BlocksInQuarterMeg
      else if BootedMemoryInBlocks <= BlocksInHalfMeg then
          BootedMemoryInBlocks := BlocksInHalfMeg
      else begin
           WriteLn('** Boot file is bigger than 1/2 Megabyte; aborting');
           exit(MakeBoot);
           end;
      
      if BootedMemoryInBlocks - IOSegSize - NxtSegBase > 0 then
        begin
          { make a segment to sit on the free memory }
          MakeSeg('*unused*', '', BootedMemoryInBlocks-IOSegSize-NxtSegBase,
                  DataSegment, False, Swappable);
          CurSegNum := CurSegNum + 1
        end;
      if BootedMemoryInBlocks - NxtSegBase <> IOSegSize then
        if BootedMemoryInBlocks - NxtSegBase  < IOSegSize then
          begin Writeln('** Insufficient memory for IO segment');
            Exit(MakeBoot)
          end
        else
          begin Writeln('** Free memory lost');
            Exit(MakeBoot)
          end
    end { LoadFreeSegment };
  
    
    procedure LoadIOSegment;
    var T: Integer;
    begin { LoadIOSegment }
      { make the IO segment }
      T := CurSegNum;
      CurSegNum := IOSeg;
      MakeSeg('*IO*    ', '', IOSegSize, DataSegment, True, UnMovable);
      CurSegNum := T
    end { LoadIOSegment };


    procedure LoadSATandBootBlock;
    var P: record case integer of
            1: (P: pSegBlock);
            2: (Offset: Integer;
                Segment: Integer)
            end;
        I, J: Integer;
    begin { LoadSATandBootBlock }
      FreeSeg := CurSegNum;
      { close the loop }
      SITp^[OldSeg].NextSeg := SATSeg;
      { correct miscellaneous errors }
      SATp^[IOSeg].Full := False;
      SITp^[SS].Maximum := MMMaxIntSize;
      SITp^[SS].Increment := RunHead.StackIncr-1;
      { link the free segment numbers together }
      OldSeg := 0;
      for I := MaxSIT downto FreeSeg do
        begin SITp^[I].NextSeg := OldSeg; OldSeg := I end;
      { set up the BootBlock }
      SITp^[0].BootBlock.CS := CS;             { initial code segment }
      SITp^[0].BootBlock.SS := SS;             { initial stack segment }
      SITp^[0].BootBlock.XX := CS;             { unused }
      SITp^[0].BootBlock.VN := RunHead.Version;{ system version number }
      SITp^[0].BootBlock.FF := FreeSeg;        { first free segment number }
      SITp^[0].BootBlock.FC := FirstCodeSeg;   { first system code segment }
      FSBlkWrite(BootId,0,Recast(SATp,pDirBlk));
      Buffer := Recast(SATp,pSegBlock);
      for I := 0 to 255 do
        CheckSum := CheckSum + Buffer^.Block[I];
      P.P := Recast(SITp,pSegBlock);
      for I := 1 to 3 do { re-write SIT image }
        begin FSBlkWrite(BootId,I,Recast(P.P,pDirBlk));
          Buffer := Recast(P.P,pSegBlock);
          for J := 0 to 255 do
            CheckSum := CheckSum + Buffer^.Block[J];
          P.Offset := P.Offset + 256
        end
    end { LoadSATandBootBlock };
    
    
    procedure FixUpDiskAddresses;
    var Head: ptrHeader;
        Buff: ptrDiskBuffer;
        Addr: DiskAddr;
        P: record case integer of
            1: (P: pSegBlock);
            2: (Offset: Integer;
                Segment: Integer)
            end;
        I, J: Integer;
        ChangeCheckSum: Integer;
        Kludge: record case Integer of
                 1: (A: DiskAddr);
                 2: (D: Double)
                 end;
    begin { FixUpDiskAddresses }
      FSBlkRead(BootId,0,Recast(SATp,pDirBlk));   { read SAT image }
      P.P := Recast(SITp,pSegBlock);
      for I := 1 to 3 do { read SIT image }
        begin FSBlkRead(BootId,I,Recast(P.P,pDirBlk));
          Buffer := Recast(P.P,pSegBlock);
          P.Offset := P.Offset + 256
        end;
      Addr := FileIdToSegId(BootId);
      Head := ReadHeader(Addr);        { -1 block }
      ChangeCheckSum := 0;
      I := SATSeg;
      repeat
        I := SITp^[I].NextSeg;
        with SATp^[I], SITp^[I] do
          if InUse and (Kind = CodeSegment) then
            begin
              if Head^.LogBlock > SwapInfo.BootLogBlock then
                begin
                  Addr := FileIdToSegId(BootId);
                  Head := ReadHeader(Addr)
                end;
              while (Head^.LogBlock < SwapInfo.BootLogBlock) and
                    (Head^.NextAdr <> DblZero) do
                begin
                  Addr := Head^.NextAdr;
                  Head := ReadHeader(Addr)
                end;
              if Head^.LogBlock = SwapInfo.BootLogBlock then
                begin
                  Kludge.A := LogAddrToPhysAddr(Addr);
                  ChangeCheckSum := ChangeCheckSum - SwapInfo.BootLowerAddress;
                  SwapInfo.BootLowerAddress := Kludge.D[0];
                  ChangeCheckSum := ChangeCheckSum + SwapInfo.BootLowerAddress;
                  ChangeCheckSum := ChangeCheckSum - SwapInfo.BootUpperAddress;
                  SwapInfo.BootUpperAddress := Kludge.D[1];
                  ChangeCheckSum := ChangeCheckSum + SwapInfo.BootUpperAddress
                end
              else
                  Writeln('** Unable to find disk address of segment ', I:1)
            end
      until I = SATSeg;
      { re-write the SIT image }
      P.P := Recast(SITp,pSegBlock);
      for I := 1 to 3 do { re-write SIT image }
        begin FSBlkWrite(BootId,I,Recast(P.P,pDirBlk));
          Buffer := Recast(P.P,pSegBlock);
          P.Offset := P.Offset + 256
        end;
      { find the end of the file and fix its checksum }
      while Head^.NextAdr <> DblZero do
        begin
          Addr := Head^.NextAdr;
          Head := ReadHeader(Addr)
        end;
      Buff := ChangeDisk(Addr);
      Buff^.IntData[255] := Buff^.IntData[255] - ChangeCheckSum
    end { FixUpDiskAddresses };
  
  
  begin { SystemCode }
    repeat
      Write('Enter name of new system boot file [', BootName, ']: ');
      GetLn(Tmp, bootName);
      if Tmp = '' then Tmp := BootName;
      BootId := FSLocalLookUp(Tmp,Ignore,Ignore);
      if BootId = 0 then
        begin
          BootId := FSEnter(Tmp);
          if WhichDisk(FileIdToSegId(BootId)) = Disk then BootName := Tmp
          else
            begin FSDelete(Tmp);
              BootId := 0;
              Writeln('** ', Tmp, ' is on the wrong disk.')
            end
        end
      else
        if WhichDisk(FileIdToSegId(BootId)) = Disk then
          if Ask(Concat(Tmp, ' already exists, ok to rewrite'),'Yes') then
            begin
              FSDelete(Tmp);
              BootId := FSEnter(Tmp);
              BootName := Tmp
            end
          else BootId := 0
        else
          begin BootId := 0;
            Writeln('** ', Tmp, ' is on the wrong disk.')
          end
    until BootId <> 0;
    Rewrite(Boot,BootName);
    SystemDA := LogAddrToPhysAddr(FileIDtoSegID(BootId));
    repeat
      Write('Existing boot file to copy (type return to build a new one): ');
      GetLn(OldBootName, '');
      if OldBootName = '' then OldBootId := 1
      else OldBootId := FindFile(OldBootName);
      if OldBootId = 0 then
        Writeln('** ', OldBootName, ' not found.')
    until OldBootId <> 0;
    if OldBootName = '' then
      begin
        repeat
          Write('Enter name of character set');
          if FontName <> '' then Write(' [', FontName, ']');
          Write(':');
          GetLn(Tmp,fontName);
          if Tmp = '' then Tmp := FontName;
          if Tmp <> '' then
            if FindFile(Tmp) = 0 then
              begin
                Writeln('** ', Tmp, ' not found.');
                Tmp := ''
              end
        until Tmp <> '';
        FontName := Tmp;
        
        if doFloppy or (Cf_monitor <> CF_Landscape) then {probably want Portrait}
             MBIsPortrait := Ask('Make the Screen be Portrait', 'Yes')
        else MBIsPortrait := not Ask('Make the screen be Landscape', 'Yes');
        
        if MBIsPortrait then MBScreenSize := BlocksForPortraitScreen
        else MBScreenSize := BlocksForLandscapeScreen;
          
        CheckSum := 0;
        Block := 0;
        Word := 0;
        BlocksSwappable := 0;
        BlocksUnSwappable := 0;
        BlocksFree := 0;
        Writeln;
        Writeln('--- for each segment, one of the following is printed:');
        Writeln('                SW  -  swappable.');
        Writeln('                LS  -  less swappable.');
        Writeln('     <blank> or US  -  unswappable.');
        Writeln('                UM  -  unmovable.');
        Writeln;
        LoadStandardSegments;
        LoadFont;
        LoadCodeSegments;
        SS := CurSegNum;
        CurSegNum := CurSegNum + 1;
        NameSegment(SysNameSeg,'*Names* ');
        NameSegment(SS,'*Stack* ');
        NameSegment(IOSeg,'*IO*    ');
        LoadSNameSegment;
        LoadStackSegment;
        LoadFreeSegment;
        LoadIOSegment;
        LoadSATandBootBlock;
        S := SegList;
        while S <> nil do
          begin
            if not S^.Done then
              Writeln('** Segment ', S^.Name, 
                      ' was in the configuration file but wasn''t loaded.');
            S := S^.Next
          end;
        repeat WriteWord(-CheckSum)
        until Word = 0;
        Writeln;
        Writeln(BlocksSwappable:3, ' Swappable blocks.');
        Writeln(BlocksUnSwappable:3, ' Unswappable blocks.');
        Writeln(BlocksFree:3, ' Free blocks.');
        WriteLn(BootedMemoryInBlocks:3,' blocks in boot file.')
      end
    else
      begin
        Reset(OldBoot,OldBootName);
        while not Eof(OldBoot) do
          begin
            Boot^ := OldBoot^;
            Get(OldBoot);
            Put(Boot)
          end;
        Close(OldBoot)
      end;
    Writeln;
    if Disk <> 1 { Floppy } then
      begin
        Writeln;
        Write('Fixing up disk addresses... ');
        FixUpDiskAddresses
      end;
    Writeln(BootName,' written.');
    Close(Boot);
    
    {* added 31 dec 81 to mark type of boot file (boot type = 15) *}
    
    new(FileData); {* new ptr for File Information Block info *}
    FSGetFSData(BootId,FileData); {* get file info *}
    FileData^.FileType:= 15;
    FSSetFSData(BootId,FileData); {* replace corrected info *}
    dispose(FileData); {* free ptr *}
    
    {* end of 31 dec 81 change *}
    
    InfoHeader^.SerialNum := InfoBlk;
    InfoHeader^.LogBlock := 0;
    DiskIO(InfoBlk, Ptr, InfoHeader, DskRead);
    Ptr^.BootTable[BootIndex] := SystemDA;
    DiskIO(InfoBlk, Ptr, InfoHeader, DskWrite);
    Cheat.Addr := SystemDA;
    Writeln('System ', BootChar, '-boot disk address = ',Cheat.Dbl[0]:1:-10)
  end { SystemCode };

   
  Procedure Z80BootFile;
  Label 1;
  Var Tmp : String;
      SZFid : FileID;
      SZBlks, SZBits, I : Integer;
      Buff : pDirBlk;
    Begin
    While True Do
      Begin 
        If ZFid <> 0 Then
          Begin
            If Not Ask(Concat(ZBootFile, 
                          ' already exists. OK to rewrite'), 'Yes') Then
              Begin
                Write('Enter name of new Z80 boot file [', ZBootFile, ']: ');
                GetLn(Tmp, ZBootFile);
                if Tmp = '' then Tmp := ZBootFile;
                ZFid := FSLocalLookup(Tmp, Zblks, ZBits);
                If ZFid = 0 Then 
                  Begin
                    ZbootFile := Tmp;
                    GoTo 1;
                  End;
              End Else GoTo 1;
        End Else
          Begin
            Write('Enter name of new Z80 boot file [', ZBootFile:1,  ']: ');
            GetLn(Tmp, ZBootFile);
            If Tmp = '' Then Tmp := ZBootFile;
            ZFid := FSLocalLookup(Tmp, Zblks, ZBits);
            If ZFid = 0 Then 
              Begin
                ZBootFile := Tmp;
                GoTo 1;
              End;
          End; {If Then Else}
      End;  {While}
1: 
    Write('Enter file from which ZBoot file is to be created [System.ZBoot]: ');
    Getln(Tmp, 'System.ZBoot');
    If Tmp = '' Then Tmp := 'System.ZBoot';
    SZFid := FSLookup(Tmp, SZBlks, SZBits);
    If SZFid = 0 Then
      Begin
        Writeln('** File ', Tmp:1, ' not found.');
        Writeln;
        GoTo 1;
      End;

    { At this point ZFid is the file id of the destination }
    { SZFid is the fileid of the source.                   }

    If ZFid = 0 Then ZFid := FSEnter(ZBootFile);
    If ZFid = 0 Then
      Begin
        Writeln('** Could not create the Z80 boot file. Aborting.');
        Exit(Makeboot);
      End;

    New(0, 256, Buff);

    For I := 0 To SZBlks - 1 Do
      Begin
        FSBlkRead(SZFid, I, Buff);
        FSBlkWrite(ZFid, I , Buff);
      End;
    FSClose(ZFid, SZBlks, SZBits); 
  End;

  Procedure FltBootFile;
  Label 1;
  Var Tmp, STemp, DefFBoot : String;
      SFltFid : FileID;
      SFltBlks, SFltBits, I : Integer;
      Buff : pDirBlk;
    Begin
    While True Do
      Begin 
        If FltFid <> 0 Then
          Begin
            If Not Ask(Concat(FBootFile, 
                          ' already exists. OK to rewrite'), 'Yes') Then
              Begin
                Write('Enter name of new Double Precision boot file [', FBootFile, ']: ');
                GetLn(Tmp, FBootFile);
                if Tmp = '' then Tmp := FBootFile;
                FltFid := FSLocalLookup(Tmp, Fltblks, FltBits);
                If FltFid = 0 Then 
                  Begin
                    FbootFile := Tmp;
                    GoTo 1;
                  End;
              End Else GoTo 1;
        End Else
          Begin
            Write('Enter name of new Double Precision boot file [', FBootFile:1,  ']: ');
            GetLn(Tmp, FBootFile);
            If Tmp = '' Then Tmp := FBootFile;
            FltFid := FSLocalLookup(Tmp, Fltblks, FltBits);
            If FltFid = 0 Then 
              Begin
                FBootFile := Tmp;
                GoTo 1;
              End;
          End; {If Then Else}
      End;  {While}
1: 

    If IsEIOSystem Then DefFBoot := 'EIODblFloat.Bin'
      Else DefFBoot := 'CIODblFloat.Bin';     
    Write('Enter file from which FBoot file is to be created [',DefFBoot,']: ');
    Getln(Tmp, DefFBoot);
    
    If Tmp = '' Then Tmp := DefFBoot;
    STemp := SubStr(Tmp, Length(Tmp) - 3, 4);
    ConvUpper(STemp);
    If STemp <> '.BIN' Then AppendString(Tmp, '.Bin');

    SFltFid := FSLookup(Tmp, SFltBlks, SFltBits);
    If SFltFid = 0 Then
      Begin
        Writeln('** File ', Tmp:1, ' not found.');
        Writeln;
        GoTo 1;
      End;

    { At this point FltFid is the file id of the destination }
    { SFltFid is the fileid of the source.                   }

    If FltFid = 0 Then FltFid := FSEnter(FBootFile);
    If FltFid = 0 Then
      Begin
        Writeln('** Could not create the Double Precision boot file. Aborting.');
        Exit(Makeboot);
      End;

    New(0, 256, Buff);

    For I := 0 To SFltBlks - 1 Do
      Begin
        FSBlkRead(SFltFid, I, Buff);
        FSBlkWrite(FltFid, I , Buff);
      End;
    FSClose(FltFid, SFltBlks, SFltBits); 
  End;

  procedure InterpreterMicroCode;
  var IName, Tmp: String;
      IFile: MicroFile;
      Ignore: Integer;
      HaveFile: boolean;
      OldBootName: String;
      OldBootId: FileId;
      OldBoot: file of SegBlock;


    function ReadInterp( IName: PathName ): boolean;
    begin { ReadInterp }
      IName := Concat(IName,'.Bin');
      if FindFile(IName) <> 0 then
        begin
          Writeln(' Reading ',IName);
          ReadInterp := true;
          Reset(IFile,IName);
          while not Eof(IFile) and (IFile^.Adrs >= 0) do
            begin
              if (IFile^.Adrs < #0000) or (IFile^.Adrs > #7376) then
                Writeln('** Micro address outside of boot area: ',
                        IFile^.Adrs:1:-8)
              else Micro^[IFile^.Adrs] := IFile^.MI;
              Get(IFile)
            end;
          Close(IFile)
        end
      else
        begin
          Writeln('** ', IName, ' not found.');
          ReadInterp := false;
        end
    end { ReadInterp };


  begin { InterpreterMicroCode }
    repeat
      Write('Enter name of new micro boot file [', MBootName, ']: ');
      GetLn(Tmp, MBootName);
      if Tmp = '' then Tmp := MBootName;
      BootId := FSLocalLookUp(Tmp,Ignore,Ignore);
      if BootId = 0 then
        begin
          BootId := FSEnter(Tmp);
          if WhichDisk(FileIdToSegId(BootId)) = Disk then MBootName := Tmp
          else
            begin FSDelete(Tmp);
              BootId := 0;
              Writeln('** ', Tmp, ' is on the wrong disk.')
            end
        end
      else
        if WhichDisk(FileIdToSegId(BootId)) = Disk then
          if Ask(Concat(Tmp, ' already exists, ok to rewrite'),'Yes') then
            begin
              FSDelete(Tmp);
              BootId := FSEnter(Tmp);
              MBootName := Tmp
            end
          else BootId := 0
        else
          begin BootId := 0;
            Writeln('** ', Tmp, ' is on the wrong disk.')
          end
    until BootId <> 0;
    Rewrite(Boot,MBootName);
    InterpDA := LogAddrToPhysAddr(FileIDtoSegID(BootId));
    repeat
      Write('Existing boot file to copy (type return to build a new one): ');
      GetLn(OldBootName,'');
      if OldBootName = '' then OldBootId := 1
      else OldBootId := FindFile(OldBootName);
      if OldBootId = 0 then
        Writeln('** ', OldBootName, ' not found.')
    until OldBootId <> 0;
    if OldBootName = '' then
      begin
        CheckSum := 0;
        Block := 0;
        Word := 0;
        HaveFile := false;
        if CIODiskType=CIOMicropolis then Tmp:='CIOMIO' else Tmp:='CIOIO';
        if Ask('Use standard interpreter microcode files','Yes') then
          begin
            HaveFile := ReadInterp('Perq');
            If (IsEIOSystem) And (EIODiskType = Micropolis) 
              Then HaveFile := HaveFile Or ReadInterp('EIOIO')
              Else If (IsEIOSystem) And (EIODiskType = FiveQtrDisk) 
                Then HaveFile := HaveFile Or ReadInterp('EIO5IO')
                Else HaveFile := HaveFile Or ReadInterp(Tmp);
          end;
        repeat
          Write('Interpreter microcode file: ');
          GetLn(IName,'');
          if IName <> '' then
            HaveFile := HaveFile Or ReadInterp(IName)
        until HaveFile and (IName = '');
        WriteMicro(#0000,#7376);
        while (Word <> 0) or (Block <= EndInterpreter) do WriteWord(-CheckSum)
      end
    else
      begin
        Reset(OldBoot,OldBootName);
        while not Eof(OldBoot) do
          begin
            Boot^ := OldBoot^;
            Get(OldBoot);
            Put(Boot)
          end;
        Close(OldBoot)
      end;
    Writeln;
    Writeln(MBootName, ' written.');
    Close(Boot);

    {* added 31 dec 81 to mark type of boot file (mboot type = 16) *}
    
    new(FileData); {* new ptr for File Information Block info *}
    FSGetFSData(BootId,FileData); {* get file info *}
    FileData^.FileType:= 16;
    FSSetFSData(BootId,FileData); {* replace corrected info *}
    dispose(FileData); {* free ptr *}
    
    {* end of 31 dec 81 change *}
    
    InfoHeader^.SerialNum := InfoBlk;
    InfoHeader^.LogBlock := 0;
    DiskIO(InfoBlk, Ptr, InfoHeader, DskRead);
    Ptr^.InterpTable[BootIndex] := InterpDA;
    DiskIO(InfoBlk, Ptr, InfoHeader, DskWrite);
    Cheat.Addr := InterpDA;
    Writeln('Interpreter ', BootChar, '-boot disk address = ',
            Cheat.Dbl[0]:1:-10)
  end { InterpreterMicroCode };
   
   
  procedure WriteFloppyHeader;
  const
        FFS = 1;                { floppy first sector }
  var I: integer;
  begin { WriteFloppyHeader }
    PartBlock := 0;
    Sector := FFS;
    Cylinder := FFT;
    Buffer^.Block[0] := Lor(Shift(#252,8),#125);   {High byte, low byte}
    for I := 1 to LWinFS do Buffer^.Block[I] := 0;
    FloppyWrite;
  end { WriteFloppyHeader };
  
  
  procedure Init;
  var Word: Integer;
      Tmp, Ignore, TempS: PathName;
      Ok: Boolean;
      RunId, PathId: FileId;
      Part: Integer;
      FPathName: PathName;
      ConfigName: PathName;
      
  
    procedure ReadRun;
    var RunFile: RunFileType;
        FirstImp, LastImp, I: pImpNode;
        S: pSegNode;
        K: Integer;
        System: Boolean;
    begin { ReadRun }
      Reset(RunFile,RunName);
      ReadRunFile(RunFile,0,RunHead,FirstSeg,FirstUserSeg,LastSeg,True);
      if FirstSeg = nil then
        begin
          if RunHead.RFileFormat <> RFileFormat then
            Writeln('** ', RunName,
                    ' has an incompatible run file format.')
          else
            Writeln('** ', RunName, ' is ill-formed.');
          Exit(MakeBoot)
        end;
      ReadSegNames(RunFile,0,FirstUserSeg);
      Close(RunFile);
      if not (RunHead.System in [false,true]) then { bad run file }
        begin RunHead.System := false; RunHead.Version := -1 end;
      if not RunHead.System then
        begin
          Writeln('** ', RunName, ' is not a System program.');
          Exit(MakeBoot)
        end;
      GP:=RunHead.InitialGP;
      RunHead.StackSize :=
         RunHead.StackSize + (RunHead.CurOffset + 255) div 256;
      RunHead.StackIncr := RunHead.StackIncr;
      Close(RunFile)
    end { ReadRun };
    
    
    procedure ReadConfig;
    var S: pSegmentInfo;
        I: Integer;
        M: String;
        Config: Text;
        oldAuto: boolean;
    begin { ReadConfig }
      if FindFile(ConfigName) = 0 then ConfigName := '';
      repeat
        Write('Configuration file name ');
        if ConfigName <> '' then Write('[', ConfigName, ']');
        Write(': ');
        oldAuto := automatic;
        if (ConfigName = '') then automatic := false; 
        GetLn(Tmp,ConfigName);
        automatic := oldAuto;
        if Tmp = '' then Tmp := ConfigName;
        if Tmp <> '' then
          if FSExtSearch(FSSysSearchList,' .Config ',Tmp,i,i) = 0 then
            begin
              Writeln('** ', Tmp, ' not found.');
              Tmp := ''
            end
      until Tmp <> '';
      ConfigName := Tmp;
      Reset(Config, ConfigName);
      SegList := nil;
      while not Eof(Config) do
        begin
          New(S);
          with S^ do
            begin
              I := 0;
              while Config^ <> ' ' do
                begin
                  if I < SegLength then
                    begin I := I + 1;
                      Name[I] := Config^
                    end;
                  Get(Config)
                end;
              while I < SegLength do
                begin
                  I := I + 1;
                  Name[I] := ' '
                end;
              while (Config^ = ' ') and not Eoln(Config) do Get(Config);
              Readln(Config, M);
              ConvUpper(M);
              Mobility := UnSwappable;
              if M = '' then M := 'US';
              if M = 'SW' then Mobility := Swappable
              else
                if M = 'LS' then Mobility := LessSwappable
                else
                  if M = 'US' then Mobility := UnSwappable
                  else
                    if M = 'UM' then Mobility := UnMovable
                    else
                      begin
                        Writeln('** "', M,
                                '" is not valid for segment mobility,');
                        Writeln('** US assumed (file ', ConfigName, ').')
                      end;
              Done := False;
              Next := SegList;
              SegList := S
            end
        end;
      Close(Config)
    end { ReadConfig };


  Procedure ErrorExit;
     begin
     writeLn('** Bad arg.  Form is: MakeBoot system.<nn>/Build <bootKey> /<DiskType>');
     exit(MakeBoot);
     end;

  begin { Init }
    CreateSegment (MicroSeg, 48,1,48);
    new(MicroSeg, 1, Micro);
    for Word := 0 to #7777 do with Micro^[Word] do
     begin Word1 := 0; Word2 := 0; Word3 := 0 end;
    
    New(0,256,Ptr);
    New(0,256,Buffer);
    New(0,256,VerBuffer);
    New(0,4,MStatPtr);
    New(0,4,HeadBlkPtr);
    New(0,4,InfoHeader);
    
    automatic := false;
    HasDiskType := False;
    
    RemDelimiters(UsrCmdLine,' ',Ignore);
    GetSymbol(UsrCmdLine,Ignore,' /',Ignore);   { skip MakeBoot }
    RemDelimiters(UsrCmdLine,' /',Ignore);
    GetSymbol(UsrCmdLine,RootName,' /',Ignore);
    if RootName = '' then
      begin
        Write('Root file name: ');
        Readln(RootName)
      end
    else begin
         RemDelimiters(UsrCmdLine, ' /', Ignore);
         GetSymbol(UsrCmdLine, TempS, ' /', Ignore);
         ConvUpper(TempS);
         if TempS = 'BUILD' then
             begin
             automatic := true;
             RemDelimiters(UsrCmdLine, ' /',Ignore);
             GetSymbol(UsrCmdLine, TempS, ' /', Ignore);
             if (length(tempS) = 1) then AutoBootKey := tempS[1]
             else ErrorExit;
             if (AutoBootKey < chr(#141)) or (AutoBootKey > chr(#172)) then
                    ErrorExit;
             
             RemDelimiters(UsrCmdLine, ' /', Ignore);
             GetSymbol(UsrCmdLine, TempS, ' /', Ignore);
             If Length(TempS) <> 0 Then
               Begin
                 ConvUpper(TempS);
                 If (TempS <> 'EIO') And 
                    (TempS <> 'CIO') And 
                    (TempS <> 'EIO5') Then
                     ErrorExit;
                 HasDiskType := True;
                 IsEIOSystem := (TempS = 'EIO') Or (TempS = 'EIO5');
                 EIODiskType := Micropolis;
                 If TempS = 'EIO5' Then EIODiskType := FiveQtrDisk;
               End 
               Else HasDiskType := False;
             end
         else if TempS <> '' then ErrorExit;
         end;
    if not HasDiskType Then 
      begin
        { User didn't override disk type. Get it from Configuration }
        
        If Cf_IOBoard = Cf_EIO Then IsEIOSystem := true
          Else If Cf_IOBoard = Cf_CIO Then IsEIOSystem := False
            Else Begin
               Writeln('** UnKnown Disk Type encountered. Aborting');
               Exit(MakeBoot);
            End;
        If ISEIOSystem Then 
          begin
            if GetIntDiskKind = Mic5 
              Then EIODiskType := FiveQtrDisk
              Else EIODiskType := Micropolis;
          end;
        HasDiskType := True;
      End;
      
    if RootName = '' then
      if Ask('Write boot file to hard disk','Yes') then
        begin
          DoFloppy := false;
          Disk := 0;
        end
      else
        begin
          DoFloppy := true;
          Disk := 1;
         end
    else
      begin
        FPathName := '';
        ArrowPos := Length(RootName) + 1;
        repeat ArrowPos := ArrowPos - 1
        until (RootName[ArrowPos] = '>') or (ArrowPos = 1);
        if RootName[ArrowPos] = '>' then
          begin
            FPathName := Substr(RootName,1,ArrowPos);
            Delete(RootName,1,ArrowPos)
          end;
        if FPathName = '' then PathId := FSLocalLookUp(FSDirPrefix,Word,Word)
        else PathId := FSLocalLookUp(FPathName,Word,Word);
        if PathId = 0 then
          begin
            Writeln('** ', FPathName, ' is not a valid path.');
            Exit(MakeBoot)
          end;
        Disk := WhichDisk(FileIdToSegId(PathId));
        if RootName <> '' then
          begin
            tmp := SubStr(RootName,
                          Length(RootName)-Length('.Run')+1,
                          Length('.Run'));
            ConvUpper(tmp);
            if tmp = '.RUN' then
              RootName := SubStr(RootName, 1, Length(RootName)-Length('.RUN'));
            ConfigName := Concat(FPathName, Concat(RootName, '.Config'));
            RunName := Concat(FPathName, Concat(RootName, '.Run'));
            RunId := FindFile(RunName);
            if RunId = 0 then
              begin
                Writeln('** ', RunName, ' not found.');
                Exit(MakeBoot)
              end
          end;
        Cheat.Dbl[0] := 0;
        if Disk = 0 then
          begin
            DoFloppy := False;
            Cheat.Dbl[1] := DISKBITS;
            Writeln('Hard disk selected.')
          end
        else
          begin
            DoFloppy := True;
            Cheat.Dbl[1] := FLOPBITS;
            Writeln('Floppy selected.')
          end;
        InfoBlk := Cheat.Addr;
        if RootName <> '' then
          begin
            ReadConfig;
            ReadRun;
            Ok := False;
            Writeln;
            repeat
              Write('Which character to boot from? ');
              bootChar := GetBootChar;
              if (BootChar < chr(#141)) or (BootChar > chr(#172)) then
                WriteLn('** Bad boot character.')
              else Ok := True;
            until Ok;
            BootIndex := Ord(BootChar) - #141;
            InfoHeader^.SerialNum := InfoBlk;
            InfoHeader^.LogBlock := 0;
            DiskIO(InfoBlk, Ptr, InfoHeader, DskRead);
            SystemDA := Ptr^.BootTable[BootIndex];
            InterpDA := Ptr^.InterpTable[BootIndex];
            Cheat.Addr := SystemDA;
            Write('System ', BootChar, '-boot ');
            if SystemDA = DBLZERO then Write('is unused.')
            else Write('disk address = ',Cheat.Dbl[0]:1:-10);
            Writeln;
            Cheat.Addr := InterpDA;
            Write('Interpreter ', BootChar, '-boot ');
            if InterpDA = DBLZERO then Write('is unused.')
            else Write('disk address = ',Cheat.Dbl[0]:1:-10);
            Writeln;
            BootName := FPathName;
            AppendString(BootName, RootName);
            AppendChar(BootName, '.');
            AppendChar(BootName, BootChar);
            AppendString(BootName, '.Boot');
            MBootName := FPathName;
            AppendString(MBootName, RootName);
            AppendChar(MBootName, '.');
            AppendChar(MBootName, BootChar);
            AppendString(MBootName, '.MBoot');

            If FpathName = '' 
              Then PartName := FSDirPrefix 
              Else PartName := FPathName;
            FixFileName(PartName, False);
            Delete(PartName, PosC(PartName, '>') + 1, 
                             Length(PartName) - PosC(PartName, '>') );

            ZBootFile := PartName;
            AppendString(ZBootFile, RootName);
            AppendChar(ZBootFile, '.');
            AppendChar(ZBootFile, BootChar);
            AppendString(ZBootFile, '.ZBoot');
            
            FBootFile := PartName;
            AppendString(FBootFile, RootName);
            AppendChar(FBootFile, '.');
            AppendChar(FBootFile, BootChar);
            AppendString(FBootFile, '.FBoot');
            
            FontName := DefaultFont;
            if FindFile(FontName) = 0 then FontName := '';
            CS := 0;
            New(0,256,SATp);
            New(0,256,SITp);
            New(0,256,SNamep);
            NxtSegBase := 0
          end;
        Writeln
      end
  end { Init };
    
    
begin { MakeBoot }
  Writeln;
  Writeln('MakeBoot ', MakeBVersion, '   target memory manager version = ',
          MemoryVersion);
  Writeln;
  Init;
  Writeln;
  if automatic then BootWrite := Ask('Write the boot area', 'Yes')
  else BootWrite := Ask('Write the boot area', 'No');
  if BootWrite then
    begin
      if DoFloppy then WriteFloppyHeader;
      BootMicroCode;
    end;
  BootWrite := False;
  if RootName <> '' then
    begin
      Writeln;
      if Ask('Write a system boot file', 'Yes') then
        SystemCode;  
      Writeln;
      if Ask('Write an interpreter boot file', 'Yes') then
        InterpreterMicroCode;
      If (Not DoFloppy) And (IsEIOSystem) Then 
        Begin
          ZFid := FSLocalLookUP(ZBootFile, Zblks, ZBits);
          Writeln;
          If (ZFid = 0) Or Automatic 
            Then WriteZBoot := Ask('Write a Z80 Load Boot file', 'Yes')
            Else WriteZBoot := Ask('Write a Z80 load Boot file', 'No');
          If WriteZBoot Then Z80BootFile;
        End;

      If GetDblFloat {from System} and Not DoFloppy Then
        Begin
          FltFid := FSLocalLookup(FBootFile, FltBlks, FltBits);
          Writeln;
          If (FltFid = 0) Or Automatic 
            Then WriteFBoot := Ask('Write the Double Precision Boot file', 'Yes')
            Else WriteFBoot := Ask('Write the Double Precision Boot file', 'No');
          If WriteFBoot Then FltBootFile;
        End;

      RunSuffix := IntToStr(RunHead.Version);
      AppendString(RunSuffix, '.Run');
      
      Writeln;
      FName := PartName;
      AppendString(FName, 'Shell.');
      AppendString(FName, RunSuffix);
      If FSLocalLookup(Fname, Bits, Blks) = 0 
        Then Writeln('** File ', FName:1, ' needs to be created.');


      Writeln;
      FName := PartName;
      AppendString(FName, 'Login.');
      AppendString(FName, RunSuffix);
      If FSLocalLookup(Fname, Bits, Blks) = 0 
        Then Writeln('** File ', FName:1, ' needs to be created.');
      
      Writeln;
      FName := PartName;
      AppendString(FName, 'Link.');
      AppendString(Fname, RunSuffix);
      If FSLocalLookup(Fname, Bits, Blks) = 0 
        Then Writeln('** File ', FName:1, ' needs to be created.');
      
    end
end { MakeBoot }.

