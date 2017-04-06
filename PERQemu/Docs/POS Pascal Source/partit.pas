Program DoPartition;
{----------------------------------------------------------------------------
{
{ Partition Initialization
{ Copyright (C) 1984 - Perq Systems Corporation.
{
{ ABSTRACT:
{       Partition allows the user to (1) do once-only initialization of
{       a device, (2) partition the device, (3) initialize a new partition,
{       (4) split a partition into smaller partitions, or (5) combine to
{       adjacent partitions into one partition.
{       In once-only initialization, Partition creates all initial device
{       structures used by the SPICE Segment System (device support for
{       the virtual memory system), particularly the device free block
{       list and the device information block.
{       It also creates initial File System structures
{       on the device (in particular, the root directory is created).
{
{    copyright 1982, 1983  Three Rivers Computer Corporation
{
{----------------------------------------------------------------------------}

{$Version V4.4 for POS}
{----------------------------------------------------------------------------

 CHANGE LOG:

  28 Jun 84  V4.4  John Briggs
        Conditional compile of bad block diagnostics - printmap - turned OFF.
        Change NotOver64K to compare address with LastDA not 64K.
        VBNOutofRange in SetUpBadBlocks and PrintDIB is probably redundant -
        exception not raised by PhysAddrtoLogAddr.Put in checks for cylinder
        number out of range instead.

  27 Jun 84  V4.3  John Briggs
        Ignore the block if we get VBNOutofRange in SetupBadBlocks.
        Dont attempt to access partition addresses beyond 64K pages.
        Handle VBNoutRange in PrintDIB as boots may contain
        addresses greater than 31MB.

  14 Apr 84  V4.2  Sandeep Johar
        Handle VBNoutRange in SetupBadBlocks as bad block map may contain
        addresses greater than 31MB.
 
  31 Jan 84  V4.1  Sandeep Johar
        Fixed some command line problems for the auto mode.
        
  27 Oct 83  V4.0  Sandeep Johar
        1. Use the setupdisk procedure from diskparams.
        2. Retrofit the change made for amendment to G.3 (Scott Brown's change)

  22 Sep 83  V3.12 SSJ  
          Fixed for 5.25" disks.

   5 May 83  V3.11 SSJ for BAM Merged the ICL And BAM changes. The BAM changes 
                               were:
                               1> Handle VBNOutOfRange in checkinitialized.
                               2> Fix the /build switch for micropolis. It was
                                  not initializing Configuration to Winch24
                                  in automatic mode.
                                  
  13 Apr 83  V3.10 CDH         Modify for cio micropolis support:
          RoundDown modified for new format physical address (same as eio).
          Roundup modified for new cylinder size.
          Disktype ciomicrop used.
          New case branch added to checkNumNames for CIOMicrop: 
            checknumnames := 8.


  23 Feb 83  V3.9  BAM         Speed up by changing interlace to 16 from 8.

  18 Feb 83  V3.8  BAM         Tell what kind of disk it seems to be.

  29 Jan 83  V3.7  RSR         Fixed byte count argument to format cylinders
                               Changed FirstDA to StartDA in InitPartition.

  10 Jan 82  V3.6  AGR         Added MaxPartSize constant and reduced the
                               maximum size of a partition.  Previous code
                               could not initialize a maximum sized partition.

  04 Jan 82  V3.5  AGR         Fixed bytecount bug in FormatCylinders

  28 Dec 82  V3.4  AGR         Added code to deal with the micropolis disk on
                               the EIO system.  Added code to format the 
                               micropolis disk.  Changed dryrun code in 
                               InitPartition so that it doesn't read the disk.
                               Thus, Partition won't print an X for every block
                               of the partition.

  24 Nov 82  V3.3  BAM         Fixed bug in removing bad blocks: DskFirstWrite
                                 instead of DSKWRITEfirst (8 chars).

  28 Dec-81  V3.2  BAM         Some of the new cmdParse stuff.
                               Fix bug in Write Twice to floppy.
                               Test fewer conditions for /Build.
                               Check partition information block to see if
                                  can be read and written.

  23-Oct-81  V3.1  WJH         change to only write once in /Build mode

   2-Sep-81  V3.0  BAM         Allows /Build switch that doesn't ask questions

  27-Aug-81  V2.5  BAM         Fix message for dry run.
                               Yes or no request repeats default.
                               Tries to decide what size device is.
  24-Aug-81  V2.4  BAM         Fix bug when Split partition which had a
                                positive start and negative end.
  13-Aug-81  V2.3  BAM         Make partition name case insensitive.
                               Print "X" for each block bad on write.
                               Check part names to insure they are reasonable.
   4-Jun-81  V2.2  BAM         1) Allow renaming device
                               2) Change message from initialize
                               3) Output in unsigned decimal
                               4) Remove AdjustPartitions command
                               5) Warning message for Rename Partition
                               6) Import IO_Unit
  13-May-81  V2.1 BAM          Added random write into labels (headers)
                                 Use InLineBytes to speed up buffer equal
                                   check (NOTE: This won't work with paging)
  11-May-81  V2.0  BAM          During initialize; use random data and write
                                 twice
  14-Apr-81  V1.8  BAM          Add defaults for questions;
                                Changed "Root" to "Device" on print
                                Prompt for configuration like the scavenger
                                Prettify print of boots
   1-Apr-81  V1.7  BAM          Set up Root Directories correctly
  30-Mar-81  V1.6  BAM          Read after write for initialization of
                                 partition pages
                                Allow partitions with Incorrigible pages

  18-Mar-81  V1.5  JPS/DAS      Added code to allocate buffers on the
                                correct boundries.

  18-Mar-81  V1.4  JPS/BAM      Added local interlacing factor = 8.

  18-Mar-81  V1.3  JPS/BAM      Fixed a bug in RoundUp.

  17-Mar-81  V1.2  JPS/GGR      Removed WinchDebug and FloppyDebug.
                                Fixed RoundUp to work for 24 MByte disk.

   9-Mar-81  V1.1  John Strait  Added version number.
                                Partitions on cylinder boundaries.
                                Change PrintDIB to print DIB from a buffer
                                rather than always reading from the disk.
                                This makes DryRuns work better.

   9-Jan-81  George Robertson (ggr) at Carnegie-Mellon University
       Created.
----------------------------------------------------------------------------}


imports Arith from Arith;
imports Diskdef from Diskdef;
imports DiskUtility from DiskUtility;
imports Volumesystem from Volumesystem;
imports DiskIO from DiskIO;
imports AllocDisk from AllocDisk;
imports IO_Unit from IO_Unit;
imports FileTypes from FileTypes;
imports Perq_String from Perq_String;
imports Memory from Memory;
Imports System from System;
imports CmdParse from CmdParse;
imports DiskParams from diskparams;
Imports Stream From stream;

const PartVersion = '4.4';
      Interlace = 16;
      debug = true;   {if true, then report information when removing a block}
      MaxPartSize = 32520;  { large, and on a cylinder boundary }
      AllowAdjust = False;
      MAXBADBLOCKS = 101;
      MaxNumPOSParts = 10;
      MaxNumAccentParts = 15; {Watch out for ACCENT changing}
      printmap = false; {true => print the bad block map & blocks found in it}

var
    LastCyl       : integer; {cylinder number of last block on disk (as POS sees it)}
    DryRun        : Boolean;
    LabelPtr      : ptrHeader;
    BufferPtr     : ptrDiskBuffer;
    Label2        : ptrHeader;
    Buffer2       : ptrDiskBuffer;
    Buffer3       : ptrDiskBuffer;
    WordsPerBlock : FSBit32;
    Zero, One     : FSBit32;
    configuration : DeviceType;
    cheat         : DiskCheatType;
    Disk          : integer;
    LastDA        : FSBit32;         { DA of last block on disk }
    FirstDA       : FSBit32;         { DA of first block on device }
    RootDA        : FSBit32;         { DA of Root Information block }
    RootName      : string[8];
    PartNames     : array[0..63] of string[8];
    AutoNames     : array[1..10] of string[8];
    numAuto       : integer;
    autoIndex     : integer;
    PartDAs       : array[0..63] of DiskAddr;
    Boots         : array[0..25] of DiskAddr;
    Interps       : array[0..25] of DiskAddr;
    PagesLeft     : FSBit32;
    StartDA       : FSBit32;         { DA of first block of partition }
    EndDA         : FSBit32;         { DA of last block of partition }
    Name          : String[8];
    PName         : String;
    gotanswer     : boolean;
    PartNumber    : integer;
    c             : Char;
    automatic     : boolean;
    numheads      : integer;
    numcylinders  : integer;
    secpertrk     : integer;
    WriteCompCyl  : Integer;
    bootsize      : integer;
    diskname      : string;
    NumBadBlocks  : Integer;
    BadBlocks     : Packed Array[1.. MAXBADBLOCKS] Of DiskAddr;

{**************************** Utilities **************************************}

function CylinRange(PhysAddr : DiskAddr) : boolean;
var Cheat : DiskCheatType;
  begin
  Cheat.Addr := PhysAddr;
  case Disk of
    0: if EIOFlag or 
          (ciodisktype = ciomicropolis) or 
          (Configuration =Generic5Inch)
         then CylinRange := (LastCyl >= Abs(Cheat.Dbl[1]))
         else CylinRange := (LastCyl >= Shift(Cheat.Dbl[0], -8));
    1: CylinRange := true; {hopefully true!}
    end;
  end;

 
function NotOver64K(Addr : DiskAddr) : boolean;
  var 
   MyAddr : DiskCheatType;
   MyLastAddr : DiskCheatType;
   MyAddrHi  : integer;
   MyLastAddrHi : integer;
  begin
  MyAddr.Addr := Addr;
  MyLastAddr.Addr := LastDA;
{  NotOver64K := (Land(MyAddr.Dbl[1], #7400) = 0);}
  MyAddrHi := Land(MyAddr.Dbl[1], #7777); {ignore the top 4 bits}
  MyLastAddrHi := Land(MyLastAddr.Dbl[1], #7777); {ignore the top 4 bits}
  NotOver64K :=
    (
      ( MyAddrHi < MyLastAddrHi )   
      or
      (
        ( MyAddrHi = MyLastAddrHi )   
        and
        ( Abs(MyAddr.dbl[0]) <= Abs(MyLastAddr.dbl[0]) ) {unsigned numbers }
      )
    ); 
  end;

function Ask(msg, def: string) : boolean;
  var s:string; gotanswer:boolean;
  begin
  write(msg);
  gotanswer := false;
  repeat
    if def <> '' then Write('[',def,'] ');
    if automatic then begin
                      s := def;
                      WriteLn(s);
                      end
    else readln(input, s);
    if length(s) = 0 then s := def;
    if length(s) > 0 then
      gotanswer := s[1] in ['y','Y','n','N'];
    if not gotanswer then write('Yes or No? ')
  until gotanswer;
  ask := s[1] in ['y','Y']
  end;

function UpperEqual(s1, s2: string) : boolean;
  begin
  ConvUpper(s1);
  ConvUpper(s2);
  UpperEqual := s1=s2;
  end;

Function OKPartName(name: String): boolean;
  var i: integer;
      c: char;
      haveSpace: boolean;
  begin
  haveSpace := false;
  OKPartName := true;
  for i := 1 to 8 do
    begin
    c := name[i];
    if c = ' ' then haveSpace := true
    else if c in ['A'..'Z','a'..'z','0'..'9','.'] then
              if haveSpace then OKPartName := false
              else
         else OKPartName := false;
    end;
  end;

function GetName(var name : string; strlen : integer) : boolean;
  var s: string; i, len: integer;
  begin
  GetName := false;
  if automatic then
     if autoIndex > numAuto then 
        begin
        WriteLn;
        WriteLn('* Ran out of automatic names!  Type name: ');
        automatic := false;
        readln(s);
        end
     else begin
          s := autoNames[autoIndex];
          WriteLn(s);
          autoIndex := autoIndex+1;
          end
  else ReadLn(input,s);
  if length(s) <= 0 then exit(GetName);
  GetName := true;
 {$R-}
  name[0] := Chr(strlen);
 {$R=}
  if length(s) > strLen then len := strLen
  else len := length(s);
  
  for i:=1 to len do
    name[i] := s[i];
  for i:= len + 1 to strlen do
    name[i] := ' ';
  end;


procedure GetPartName(var name : string);
  var
    gotanswer : boolean;
    i : integer;
  begin
    gotanswer := false;
    repeat
      Write('Partition name (up to 8 chars): ');
      if GetName(name,8) then
         if OKPartName(name) then
            begin
            gotAnswer := true;
            for i := 0 to 63 do
               if UpperEqual(PartNames[i], name) then
                  begin
                  Write('** Name already in use!  ');
                  gotanswer := false;
                  i := 64;
                  end;
            end
         else WriteLn('** Name is illegal.');
    until gotanswer;
  end;


procedure WriteName(Blk : DiskAddr);
  var
    PartIdx : integer;
    DiskIdx : integer;
  begin
  PartIdx := WhichPartition(Blk);
  if PartIdx = 0 then
    begin
    Write('bogus!');
    exit(WriteName);
    end;
  DiskIdx := PartTable[PartIdx].PartDevice;
  Write(DiskTable[DiskIdx].RootPartition, ':',
        PartTable[PartIdx].PartName, '>');
  DiskIO(Blk, Buffer2, Label2, DskRead);
  if (Label2^.LogBlock <> -1) or (Label2^.SerialNum <> Blk)
    then Write('bogus!')
    else Write(Buffer2^.FSData.FileName);
  end;


procedure PrintDIB(BufferPtr: ptrDiskBuffer);
  var
    rootdib : boolean;
    i, j    : integer;
    name    : string[8];
    Blk     : DiskAddr;
    BlkValid : boolean;

handler VBNOutofRange( vid: VolID; vbn : VolBlockNumber );
  begin
{This code is probably never executed - PhysAddrtoLogAddr doesnt raise an exception}
  {should only happen for boots beyond 31MB}
  Write('* Disk address beyond 31MB - cant access ');
  BlkValid := false;
  end;
    
  begin
  name := '        ';
  for i := 1 to 8 do name[i] := BufferPtr^.PartName[i];
  rootdib := BufferPtr^.PartKind = Root;
  if rootdib then
    begin
    DeviceMount(Disk);
    WriteLn('Device Information Block Summary:');
    WriteLn('Device name = ',name);
    WriteLn('Device type = ',ord(BufferPtr^.PartDevice));
    if BufferPtr^.FillerKind = DiskRelative
    then writeln( 'Free list hints are disk relative.' )
    else writeln( 'Free list hints are parition relative.' );
    WriteLn('Partitions:');
    for i := 0 to 63 do
      if (BufferPtr^.SubParts[i] <> Zero)
         and
         (NotOver64K(BufferPtr^.SubParts[i])) then
        begin
        name := '        ';
        DiskIO(BufferPtr^.SubParts[i], Buffer2, Label2, DskRead);

        If NotOver64K(Buffer2^.PartEnd) then
          begin
          for j := 1 to 8 do name[j] := Buffer2^.PartName[j];
          Write(i,': DA = ',AddrToField(BufferPtr^.SubParts[i]):6:-10);
          WriteLn(', Name = ',name);
          end;
        end;

    WriteLn;
    WriteLn('Boots: ');
    for i := 0 to 25 do
      if (BufferPtr^.BootTable[i] <> Zero)
        or (BufferPtr^.InterpTable[i] <> Zero) then
          begin
          Write(chr(i + #141),': System      = ');
          if CylinRange(BufferPtr^.BootTable[i]) then
            begin
            BlkValid := true; {may be set false if get VBNOutofRange}
            Blk := PhysAddrToLogAddr(Disk, BufferPtr^.BootTable[i]);
            If BlkValid then WriteName(Blk);
            WriteLn;
            end
          else Writeln('* Disk address beyond 31MB - cant access ');

          if CylinRange(BufferPtr^.InterpTable[i]) then
            begin
            Write('   Interpreter = ');
            BlkValid := true; {may be set false if get VBNOutofRange}
            Blk := PhysAddrToLogAddr(Disk, BufferPtr^.InterpTable[i]);
            If BlkValid then WriteName(Blk);
            WriteLn;
            end
          else Writeln('* Disk address beyond 31MB - cant access ');
          end;
    DeviceDismount(Disk);
    end
  else
    begin
    WriteLn('Partition Information Block Summary:');
    WriteLn('Partition name = ',name);
    WriteLn('Start DA =       ',AddrToField(BufferPtr^.PartStart):6:-10);
    WriteLn('End DA =         ',AddrToField(BufferPtr^.PartEnd):6:-10);
    WriteLn('Root DA =        ',AddrToField(BufferPtr^.PartRoot):6:-10);
    WriteLn('Free head =      ',AddrToField(BufferPtr^.FreeHead):6:-10);
    WriteLn('Free tail =      ',AddrToField(BufferPtr^.FreeTail):6:-10);
    cheat.lng := BufferPtr^.NumFree;
    WriteLn('Number free =    ', cheat.dbl[1], ',', cheat.dbl[0]:6:-10);
    WriteLn('Root directory = ',AddrToField(BufferPtr^.RootDirID):6:-10);
    WriteLn('Bad segment =    ',AddrToField(BufferPtr^.BadSegID):6:-10);
    WriteLn('Device type =    ',ord(BufferPtr^.PartDevice));
    end;
  end;


procedure RoundDown( var Addr, Remainder: FSBit32 );
  var OldAddr: FSBit32;
      Cheat: DiskCheatType;
      Disk: Integer;
  begin { RoundDown }
    Cheat.Lng := LogAddrToPhysAddr(Addr);
    Disk := WhichDisk(Addr);
    OldAddr := Addr;
    case Disk of
      0: if EIOFlag or 
            (ciodisktype = ciomicropolis) or 
            (Configuration =Generic5Inch)
         then Cheat.Dbl[0] := 0
         else Cheat.Dbl[0] := LAnd(Cheat.Dbl[0], #177400);
      1: Cheat.Dbl[0] := 3
      end;
    Addr := PhysAddrToLogAddr(Disk, Cheat.Lng);
    Remainder := DoubleInt(AddrToField(OldAddr) - AddrToField(Addr))
  end { RoundDown };


procedure RoundUp( var Addr, Remainder: FSBit32 );
  var Add: Integer;
      OldAddr: FSBit32;
      nheads,
      nsecpertrk,
      ncyl : integer;
  begin { RoundUp }
    case WhichDisk(Addr) of
      0: If (configuration = Winch24) And (IO24MByte)
            Then Add := 240
            Else If Configuration = Winch12 then Add := 120
                 Else If Configuration = Winch24 then Add := 120
                      Else If Configuration = Generic5Inch then
                           begin
                           GetDiskParameters(nheads, nsecpertrk, ncyl);
                           Add := nheads*nsecpertrk;
                           end;
      1: Add := 6
      end;
    OldAddr := Addr;
    Addr := DoubleAdd(Addr, DoubleMul(DoubleInt(Add-1), WordsPerBlock ));
    RoundDown(Addr,Remainder);
    Remainder := DoubleInt(AddrToField(Addr) - AddrToField(OldAddr))
  end { RoundUp };
    
Procedure Random(var seed: integer);
  begin
  seed := Abs(seed);
  seed := LXor(seed, Shift(seed, 4));
  seed := LXor(seed, Shift(seed, -11));
  end;

Procedure RandomizeBuffer(buf: ptrDiskBuffer);
   var i, seed: integer;
   begin
   seed := #13205;
   for i := 0 to 255 do
     begin
     Random(seed);
     buf^.intdata[i] := seed;
     end;
   end; {RandomizeBuffer}

Procedure LabelRandomize(lab: ptrHeader; disk: integer);
   Const FakeSize = WordSize(Header);
   Type fakeLab = ^fakeLabRec;
        fakeLabRec = Array[1..FakeSize] of integer;
   var fake : fakeLab;
       i, seed: integer;
   begin
   seed := #13205;
   fake := RECAST(lab, fakeLab);
   for i := 1 to 8 do
      begin
      random(seed);
      fake^[i] := seed;
      end;
   lab^.SerialNum:=PhysAddrToLogAddr(disk, LogAddrToPhysAddr(lab^.SerialNum));
   lab^.PrevAdr:=PhysAddrToLogAddr(disk, LogAddrToPhysAddr(lab^.PrevAdr));
   lab^.NextAdr:=PhysAddrToLogAddr(disk, LogAddrToPhysAddr(lab^.NextAdr));
   end; {LabelRandomize}

Procedure ComplementBuffer(buf, bufOut: ptrDiskBuffer);
  var i: integer;
  begin
  for i := 0 to 255 do
    bufOut^.intdata[i] := LNot(buf^.intdata[i]);
  end;

procedure FormatCylinders( StartCyl : integer; EndCyl : integer );
      
{---------------------------------------------------------------------}
{
{ Abstract: 
{       Format the the specified cylinders.  This works with an EIO
{       micropolis disk only.
{
{---------------------------------------------------------------------}

label 1;

const
  HeadPatternSize = wordsize( Header ) - 1;
  StartTrack = 0;  { There are five tracks on a Micropolis disk. }
  EndTrack = 4;         

type
  tDataPattern = array[0..255] of integer;
  tHeadPattern = array[0..HeadPatternSize] of integer;

var
  StsPtr : IOStatPtr;
  pDataPattern : ^tDataPattern;
  pHeadPattern : ^tHeadPattern;
  i : integer;
  Cylinder : integer;
  Track : integer;
  LogAdr : double;
  FailCount : integer;
  BadTrackCount : integer;
      
begin     
  
  new( 0, 256, pDataPattern );
  new( StsPtr ); 
  new( pHeadPattern );
  
  for i := 0 to 255 do pDataPattern^[i] := lor( i, shift( i, 8 ) );
  for i := 0 to HeadPatternSize do pHeadPattern^[i] := lor( i, shift( i, 8 ) );

  BadTrackCount := 0;
    
  for Cylinder := StartCyl to EndCyl do
    for Track := StartTrack to EndTrack do begin
      LogAdr[1] := Cylinder;  { set up the physical address }
      LogAdr[0] := shift( Track, 8 );

      FailCount := 0;

    1:UnitIO( EIODisk,                           { format the current track }
              recast( pDataPattern, IOBufPtr ),
              IOFormat,
              24 * 2 * DiskBufSize,
              LogAdr,
              recast( pHeadPattern, IOHeadPtr ),
              StsPtr );       
      
      if StsPtr^.SoftStatus <> IOEIOC then begin { if format failed }
        FailCount := FailCount + 1; 
        if FailCount = 1 then writeln;     
        if FailCount < 5
        then write( 'Having trouble formatting track ' )
        else begin
          BadTrackCount := BadTrackCount + 1;
          write( 'Couldn''t format track ' );
          end;
        writeln( Track:0, ' on cylinder ', Cylinder:0, '.' );
        writeln( 'SoftStatus : ', StsPtr^.SoftStatus:4
               , '    HardStatus :', StsPtr^.HardStatus:16:-2 );

        if BadTrackCount > 10 then begin
          writeln;
          writeln( 'Too many bad tracks, aborting formatting.' );
          exit( DoPartition );
          end;
        
        UnitIO( EIODisk,                       { format the current track }
                recast( pDataPattern, IOBufPtr ),
                IOReset,
                24 * 2 * DiskBufSize,
                LogAdr,
                recast( pHeadPattern, IOHeadPtr ),
                StsPtr );       
        goto 1;
        end { if StsPtr^.Softstatus ~= IOEIOC };
      end { do loops }; 

  dispose( StsPtr );
  dispose( pDataPattern );
  dispose( pHeadPattern );
        
end;

procedure FormatDisk;

{---------------------------------------------------------------------}
{
{ Abstract: 
{       Format the useable portion of an EIO Micropolis disk.
{
{---------------------------------------------------------------------}

var
  LastPhyAddr : double;

begin

  LastPhyAddr := VolToPhyAddr( LastVolAddress( 0 ) );  { get last address }
  FormatCylinders( 0, LastPhyAddr[1] );                { 1st to last cylinder }
  
end; 




procedure FormatPartition( StartDA : DiskAddr; EndDA : DiskAddr );

{---------------------------------------------------------------------}
{
{ Abstract: 
{       Format a partition.
{
{---------------------------------------------------------------------}

var
  FirstPhyAddr,
  LastPhyAddr : packed record case boolean of
                  true : (B32 : FSBit32);
                  false: (Dbl : double );
                  end;

begin

  FirstPhyAddr.B32 := LogAddrtoPhysAddr( StartDA );
  LastPhyAddr.B32 := LogAddrtoPhysAddr( EndDA );
  
  FormatCylinders( FirstPhyAddr.Dbl[1], LastPhyAddr.Dbl[1] );

end;

Function IsBadBlock(Blk : DiskAddr): Boolean;
    Var I : Integer;
    Begin
    If Configuration <> Generic5Inch Then 
        Begin
        IsBadBlock := False;
        Exit(IsBadBlock);
        End;
    IsBadBlock := True;
    For I := 1 To NumBadBlocks Do
        If BadBlocks[I] = Blk Then 
          begin
{$ifc printmap then}
          writeln;
          writeln(Addrtofield(Blk):1:-10,' found in bad block map');
{$endc}
          Exit(IsBadBlock);
          end;
    IsBadBlock := False;
    End;


Procedure SetUpBadBlocks;
Type DiskBlock = Packed Array[0..255] of Integer;
     pDiskBlock = ^DiskBlock;
Var pBuffer : pDiskBlock;
    pHeader : IOHeadPtr;
    pStatus : IOStatPtr;
    I : Integer;
    PhyAddr : DiskCheatType;
    LogAdr : Double;
    BadLogAdr : FSBit32;
    Valid : boolean;

handler VBNOutofRange( vid: VolID; vbn : VolBlockNumber );
  begin
{This code is probably never executed - PhysAddrtoLogAddr doesnt raise an exception}
{$ifc printmap then}
  writeln('**VBNOutofRange Cyl: ',cheat.dbl[1]:1:-10,
                          ' Head/Sector: ',cheat.dbl[0]:1:-10);
{$endc}
  Valid := false;
  end;
    
    Begin
    New(0, 256, pBuffer);
    New(0, 2, pStatus);
    New(0, 8, pHeader);
    
    LogAdr[1] := 0;                             { cylinder 0 }
    LogAdr[0] := Lor(Shift(1, 8), 8);           { head 1, sector 8}
    While True Do
        begin
        UnitIO( EIODisk,
                RECAST(pBuffer, IOBufPtr),
                IODiagRead,
                512,
                LogAdr,
                pHeader,
                pStatus );

{$ifc printmap then}
        writeln('SET UP BAD BLOCK MAP');
        writeln('LastCyl = ', LastCyl:1);
        writeln;
{$endc} 
        I := 0;
        While I <= 254 Do
            Begin
            If pBuffer^[I] = -1 Then Exit(SetupBadBlocks);
            If NumBadBlocks >= MAXBADBLOCKS Then 
                Begin
                Writeln('** Too many bad blocks in the bad block map.');
                Writeln('** The map probably needs to be re-written.');
                Exit(DoPartition);
                End;
                
            Cheat.Dbl[0] := pBuffer^[I];                { Head/sector }
            Cheat.Dbl[1] := pBuffer^[I+1];              { cylinder number }
            If Abs(Cheat.Dbl[1]) <= LastCyl then
              begin
              Valid := true; {set false if get VBNOutofRange}
              BadLogAdr := PhysAddrToLogAddr(0, cheat.addr);
              if Valid then
                begin
                NumBadBlocks := NumBadBlocks + 1;
                BadBlocks[NumBadBlocks] := BadLogAdr;
{$ifc printmap then}
                writeln(AddrtoField(Badlogadr):1:-10,' in Badblock map ');
{$endc}
                end;
              end
{$ifc printmap then}
            else
              begin
              writeln('**Cyl too big - Cyl: ',cheat.dbl[1]:1:-10,
                      ' Head/Sector: ',cheat.dbl[0]:1:-10);
              end
{$endc}
                 ;
            I := I + 2;
            End;
        LogAdr[0] := LogAdr[0] + 1;
        If LAND(LogAdr[0], 15) = 0 Then
            Begin
            Writeln('** Bad block map overflows cyl 0, head 1.');
            Writeln('** The map probably needs to be re-written.');
            Exit(dopartition);
            End;
        End;
    Dispose(pBuffer);
    Dispose(pStatus);
    Dispose(pHeader);
    End;


{**************************** InitPartition **********************************}
Procedure InitPartition(PartName : string; StartDA, EndDA : FSBit32);
    Type
        BitMap = packed array [0..MaxPartSize] of boolean;
        ptrBitMap = ^BitMap;
    Var
        i,j           : integer;
        Blk           : DiskAddr;
        cheat         : DiskCheatType;
        InfoBlk       : FSBit32;
        RootBlk       : FSBit32;
        StartBlk      : FSBit32;
        EndBlk        : FSBit32;
        NextGoodBlk   : FSBit32;
        StartIdx      : integer;
        EndIdx        : integer;
        CurIdx        : integer;
        FirstIdx      : integer;
        numFree       : integer;
        OkBlock       : BitMap;
        writeTwice    : Boolean; 
        BogusAddrs    : boolean;
        testAfterFormat : Boolean;

     handler VBNOutofRange( vid: VolID; vbn : VolBlockNumber );
     { if we expect these because we are testing the disk, return }
     { if it's a real error, raise the exception again for the thing }
     { above us to handle }
     begin
       if not BogusAddrs then raise VBNOutofRange( vid, vbn );
     end;
      
     Procedure SetUpLabel(blk: FSBit32);
        begin
        with LabelPtr^ do
            begin
            SerialNum := Zero;
            LogBlock := 0;
            Filler := AddrToField(DoubleAdd(Blk,WordsPerBlock));
            if Blk = StartBlk then
                begin
                PrevAdr := Zero;
                NextAdr := DoubleAdd(Blk, WordsPerBlock);
                end
            else if Blk = EndDA then
                begin
                PrevAdr := DoubleSub(Blk, WordsPerBlock);
                NextAdr := Zero;
                Filler := StartIdx;
                end
            else
                begin
                PrevAdr := DoubleSub(Blk, WordsPerBlock);
                NextAdr := DoubleAdd(Blk, WordsPerBlock);
                end;
            end; {with LabelPtr}
        end; {SetUpLabel}
       
     Function EqualLabels(lab1, lab2: ptrHeader): boolean;
        begin
        EqualLabels := (lab1^.SerialNum = lab2^.SerialNum) and 
                       (lab1^.LogBlock  = lab2^.LogBlock)  and
                       (lab1^.Filler    = lab2^.Filler)    and
                       (lab1^.PrevAdr   = lab2^.PrevAdr)   and
                       (lab1^.NextAdr   = lab2^.NextAdr);
        end; {EqualLabels}

     Function EqualBuffers(buf1, buf2: ptrDiskBuffer): boolean;
           begin {use the qcode for block compares}
           LoadAdr(buf1^);
           LoadAdr(buf2^);
           InLineByte(#22 {LDCW} );
           InLineWord(256);
           InLineByte(#360 {STLATE} );
           InLineByte(#102 {translate mask} );
           InLineByte(#105 {EQUWORD} );
           InLineByte(0);
           InLineByte(129  {STL0}  );  {assign return value}
           end; {EqualBuffers}

    begin    

(*
    if  EIOFlag
    and (Configuration = Winch24)
    then if Ask( 'Do you wish to format the partition? ', 'No' )
    then if Ask( 'Are you sure? ', 'Yes' )
    then if not DryRun
    then FormatPartition( StartDA, EndDA );
*)

    WriteLn('Initialize partition ',PartName);
    WriteLn;
    
    NumBadBlocks := 0;
    If Configuration = Generic5Inch then SetupBadBlocks;

    BogusAddrs := false; { until further notice, all DiskAddrs are genuine }
    InfoBlk := StartDA;
    RootBlk := DoubleAdd(InfoBlk,WordsPerBlock);
    StartBlk := DoubleAdd(RootBlk,WordsPerBlock);
                                             
    StartIdx := AddrToField(StartBlk);
    EndIdx := AddrToField(EndDA);
      
    numFree := EndIdx-StartIdx+1;

    for i := 0 to EndIdx-StartIdx do
       OkBlock[i] := true;
    
    for i:=0 to DISKBUFSIZE-1 do BufferPtr^.IntData[i] := 0;

    BufferPtr^.FreeHead := StartBlk;
    BufferPtr^.FreeTail := EndDA;
    BufferPtr^.NumFree := DoubleInt(numFree);
    BufferPtr^.RootDirID := RootBlk;
    BufferPtr^.BadSegID := Zero;
    for i:=1 to 8 do BufferPtr^.PartName[i] := PartName[i];
    BufferPtr^.PartStart := StartDA;
    BufferPtr^.PartEnd := EndDA;
    BufferPtr^.PartRoot := RootDA;
    BufferPtr^.PartKind := Leaf;
    BufferPtr^.PartDevice := configuration;
    
    LabelPtr^.SerialNum := InfoBlk;
    LabelPtr^.LogBlock := 0;
    LabelPtr^.Filler := 0;
    LabelPtr^.PrevAdr := Zero;
    LabelPtr^.NextAdr := Zero;

    if not DryRun then
      begin
      OKBlock[0] := TryDiskIO(InfoBlk, BufferPtr, LabelPtr, DskFirstWrite, 1);
      if OKBlock[0] then
            OKBlock[0] := TryDiskIO(InfoBlk, Buffer3, Label2, DskRead, 1);
      if OKBlock[0] then OKBlock[0] := EqualLabels(LabelPtr, Label2);
      if OKBlock[0] then OKBlock[0] := EqualBuffers(BufferPtr, Buffer3);
      if not OKBlock[0] then
        begin
        WriteLn('** The partition information block Cannot be written.');
        WriteLn(Chr(7),'** This device is in trouble!!');
        WriteLn('** Reformat the device or put partitions in different places.');
        Exit(DoPartition);
        end;
      end;
    
    PrintDIB(BufferPtr);
    
    if not Ask('Do you want to initialize partition pages? ','Yes') then
      begin
      WriteLn(Chr(7),'* If you do not initialize the pages, the free list will not be set up.');
      if not Ask('Do you want to initialize partition pages? ','Yes') then
        begin
        WriteLn('* You should now Scavenge the partition to recover the files.');
        exit(InitPartition);
        end;
      end;
    
    testAfterFormat :=  Ask('Do you want to test after initializing? ','Yes');
    if testAfterFormat and (numFree > MaxPartSize) then
        begin
        WriteLn('*** Sorry, partition too big to test.');
        testAfterFormat := false;
        end;

    if testAfterFormat and (not automatic) then
        writeTwice := Ask('Do you want to write each block twice? ','Yes')
    else writeTwice := false;
    
       
    for i:=0 to DISKBUFSIZE-1 do BufferPtr^.IntData[i] := 0;
    
    BufferPtr^.FSData.FileBlocks := 0;
    BufferPtr^.FSData.FileBits := 4096;
    BufferPtr^.FSData.FileType := DirFile;
    BufferPtr^.FSData.FileName := 'ROOT.DR';

    BufferPtr^.SegKind := Permanent;
    BufferPtr^.LastBlk := -1;
    BufferPtr^.LastAddr := RootBlk;
    BufferPtr^.LastNegBlk := -1;
    BufferPtr^.LastNegAddr := RootBlk;
    BufferPtr^.NumBlksInUse := 1;
    
    LabelPtr^.SerialNum := RootBlk;
    LabelPtr^.LogBlock := -1;
    LabelPtr^.Filler := StartIdx;
    LabelPtr^.PrevAdr := Zero;
    LabelPtr^.NextAdr := Zero;
    
    if not DryRun then
      begin
      OKBlock[0] := TryDiskIO(RootBlk, BufferPtr, LabelPtr, DskFirstWrite, 1);
      if OKBlock[0] then
            OKBlock[0] := TryDiskIO(RootBlk, Buffer3, Label2, DskRead, 1);
      if OKBlock[0] then OKBlock[0] := EqualLabels(LabelPtr, Label2);
      if OKBlock[0] then OKBlock[0] := EqualBuffers(BufferPtr, Buffer3);
      if not OKBlock[0] then
        begin
        WriteLn('** The partition Root Directory block cannot be written.');
        WriteLn(Chr(7),'** This device is in trouble!!');
        WriteLn('** Reformat the device or put partitions in different places.');
        Exit(DoPartition);
        end;
      end;

    RandomizeBuffer(BufferPtr);
    ComplementBuffer(BufferPtr, Buffer2);

    LabelPtr^.SerialNum := Zero;
    LabelPtr^.LogBlock  := 0;
    LabelPtr^.Filler    := StartIdx;
    LabelPtr^.PrevAdr   := DoubleSub(EndDA,WordsPerBlock);
    LabelPtr^.NextAdr   := Zero;
    
    if not DryRun then
      DiskIO(EndDA,BufferPtr,LabelPtr,DskFirstWrite);
    
    Writeln;

    if writeTwice then begin
      BogusAddrs := true; { until furthur notice, we may have bogus addrs }
      LabelRandomize(LabelPtr, disk);
      end;
    
    FirstIdx := StartIdx;
    repeat
      i := FirstIdx;
      repeat
         Blk := FieldToAddr(Disk, i);
         if not writeTwice then SetUpLabel(blk);
         if (AddrToField(Blk)-FirstIdx) mod 200 = 0 then Write('w');
         if not DryRun then begin
           CurIdx := i-StartIdx;
           OKBlock[CurIdx] := Not IsBadBlock(Blk);
           If OKBlock[CurIdx] Then
           OKBlock[CurIdx] := TryDiskIO( Blk, BufferPtr, LabelPtr 
                                       , DskFirstWrite, 1 );
           if not OKBlock[CurIdx] then write('X'); 
           end; 
         i := i + Interlace;
      until ((i < 0) = (EndIdx < 0)) = (i > EndIdx)  { unsigned i > EndIdx };
      Writeln;
      FirstIdx := FirstIdx + 1;
    until FirstIdx = StartIdx + Interlace;
    Writeln;

  if testAfterFormat then
    begin
    FirstIdx := StartIdx;
    repeat
      i := FirstIdx;
      repeat
         Blk := FieldToAddr(Disk, i);
         if not writeTwice then SetUpLabel(blk);
         if (AddrToField(Blk)-FirstIdx) mod 200 = 0 then Write('r');  
         if not DryRun then begin  
           CurIdx := i-StartIdx;
           if   OKBlock[CurIdx] 
           then OKBlock[CurIdx] := TryDiskIO(Blk, Buffer3, Label2, DskRead, 1);
           if   OKBlock[CurIdx] 
           then OKBlock[CurIdx] := EqualLabels(LabelPtr, Label2);
           if   OKBlock[CurIdx] and writeTwice 
           then OKBlock[CurIdx] := EqualBuffers(BufferPtr, Buffer3);
           if not OKBlock[CurIdx] then Write('X'); 
           end;
         i := i + Interlace;
      until ((i < 0) = (EndIdx < 0)) = (i > EndIdx)  { unsigned i > EndIdx };
      Writeln;
      FirstIdx := FirstIdx + 1;
    until FirstIdx = StartIdx + Interlace;
    Writeln;
    end;
     
    if writeTwice then
       begin 
       BogusAddrs := false; { no more bogus disk addresses }
       FirstIdx := StartIdx;
       repeat
         i := FirstIdx;
         repeat
            Blk := FieldToAddr(Disk, i);
            SetUpLabel(blk);
            if (AddrToField(Blk)-FirstIdx) mod 200 = 0 then Write('w');
            if not DryRun then begin
              CurIdx := i-StartIdx;
              if OKBlock[CurIdx] 
              then OKBlock[CurIdx] := TryDiskIO(Blk, Buffer2, LabelPtr,
                                           DskFirstWrite, 1);
              if not OKBlock[CurIdx] then Write('X'); 
              end;
            i := i + Interlace;
         until ((i < 0) = (EndIdx < 0)) = (i > EndIdx)  {unsigned i > EndIdx };
         Writeln;
         FirstIdx := FirstIdx + 1;
       until FirstIdx = StartIdx + Interlace;
       Writeln;
   
       FirstIdx := StartIdx;
       repeat
         i := FirstIdx;
         repeat
            Blk := FieldToAddr(Disk, i);
            SetUpLabel(blk);
            if (AddrToField(Blk)-FirstIdx) mod 200 = 0 then Write('r'); 
            if not DryRun then begin
              CurIdx := i-StartIdx;
              if   OKBlock[CurIdx] 
              then OKBlock[CurIdx] := TryDiskIO(Blk,Buffer3,Label2,DskRead,1);
              if   OKBlock[CurIdx] 
              then OKBlock[CurIdx] := EqualLabels(LabelPtr, Label2);
              if   OKBlock[CurIdx] 
              then OKBlock[CurIdx] := EqualBuffers(Buffer2, Buffer3);
              if not OKBlock[CurIdx] then Write('X');
              end;
            i := i + Interlace;
         until ((i < 0) = (EndIdx < 0)) = (i > EndIdx) { unsigned i>EndIdx };
         Writeln;
         FirstIdx := FirstIdx + 1;
       until FirstIdx = StartIdx + Interlace;
       Writeln;
     end; {writeTwice}
      
{ test for bad blocks to leave out of file system }

if testAfterFormat then 
   begin
   FirstIdx := StartIdx; {use FirstIdx as real start of free blocks (in case
                           some at front are bad}
   i := 0;
   while i <= EndIdx-StartIdx do
     begin
     if not OKBlock[i] then
        begin
        Write('* Block ',i+StartIdx:1:-10,' bad, removing from Free List ');
        j := i;
        while (not OKBlock[j]) and (j < EndIdx-StartIdx)  do
           j := j+1;
        if not OKBlock[j] then j := 0;  {for end of list processing}
        
        WriteLn('up to ',j+StartIdx:1:-10);
        if debug then WriteLn('indexes are ',i:1:-10,' and ',j:1:-10);
        
      { j will be next good block or 0 if no more good ones }

        if j=0 then NextGoodBlk := Zero
        else NextGoodBlk := FieldToAddr(Disk, j+StartIdx);

        if debug then WriteLn('Next good block: ',AddrToField(NextGoodBlk):1:-10);

        if i = 0 then
           begin
           if debug then WriteLn('First block bad');
           DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead); {fix InfoBlk}
           BufferPtr^.FreeHead := NextGoodBlk;
           if not DryRun then
              DiskIO(InfoBlk, BufferPtr, LabelPtr, DskWrite);
           if j <> 0 then
             begin
             FirstIdx := j+StartIdx;
             if debug then WriteLn('Rewriting NextGoodBlk as head ');
             DiskIO(NextGoodBlk, BufferPtr, LabelPtr, DskRead);
             LabelPtr^.PrevAdr := Zero;
             if not dryRun then
                 DiskIO(NextGoodBlk, BufferPtr, LabelPtr, DskFirstWrite);
             end
           else writeLn('All blocks bad');
           end

        else if j=0 then {last block bad; lastGood is i+StartIdx-1}
           begin
           if debug then WriteLn('Last blk bad; last good is ',i+StartIdx-1:1:-10);
           blk := FieldToAddr(Disk, i+StartIdx-1); {last good}
           DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead); {fix InfoBlk}
           BufferPtr^.FreeTail := blk;
           if not DryRun then
              DiskIO(InfoBlk, BufferPtr, LabelPtr, DskWrite);
           DiskIO(blk, BufferPtr, LabelPtr, DskRead);
           LabelPtr^.NextAdr := Zero;
           LabelPtr^.Filler  := FirstIdx;
           if not dryRun then
              DiskIO(blk, BufferPtr, LabelPtr, DskFirstWrite);
           end
        
        else begin {somewhere in middle}
             blk := FieldToAddr(Disk, i+StartIdx-1); {last good}
             if debug then WriteLn('Last good is ',AddrToField(Blk):1:-10);
             DiskIO(blk, BufferPtr, LabelPtr, DskRead);
             LabelPtr^.NextAdr := NextGoodBlk;
             LabelPtr^.Filler := AddrToField(NextGoodBlk);
             if not dryRun then
                DiskIO(blk, BufferPtr, LabelPtr, DskFirstWrite);
             DiskIO(NextGoodBlk, BufferPtr, LabelPtr, DskRead);
             LabelPtr^.PrevAdr := blk;
             if not dryRun then
                DiskIO(NextGoodBlk, BufferPtr, LabelPtr, DskFirstWrite);
             end;
        
        if j = 0 then i := EndIdx-StartIdx+1
        else i := j;
        
        end {not OK} 

      else i := i+1; {OK}

      end;  {loop for i}
   
   j := numFree;
   for i := 0 to EndIdx-StartIdx do
     if not OKBlock[i] then j := j-1;

   if j <> numFree then { exists bad block }
       begin
       WriteLn('* Updating free count; lost ',numFree-j:1,' blocks.');
       DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead); {fix InfoBlk}
       BufferPtr^.NumFree := DoubleInt(j);
       blk := BufferPtr^.FreeTail;
       if not DryRun then
           DiskIO(InfoBlk, BufferPtr, LabelPtr, DskWrite);
       If FirstIdx <> StartIdx then {have to fix Filler of last block even if
                                     last block OK}
            begin
            if debug then WriteLn('Fix filler word of last block since first free changed');
            DiskIO(blk, BufferPtr, LabelPtr, DskRead);
            LabelPtr^.Filler := FirstIdx;
            if not DryRun then
                DiskIO(blk, BufferPtr, LabelPtr, DskFirstWrite);
            end;
       end; { exists bad block }
       
   end; {TestAfterFormat}

  end; {InitPartition}


{*********************** WriteRootDIB *************************}
procedure WriteRootDIB;
    var
      i : integer;
    begin
    WriteLn;
    WriteLn('Writing root Information Block.');
    WriteLn;
    
    for i:=0 to DISKBUFSIZE-1 do BufferPtr^.IntData[i] := 0;

    BufferPtr^.IntData[0] := BootSize;
    BufferPtr^.IntData[1] := SecPerTrk;
    BufferPtr^.IntData[2] := NumHeads;
    BufferPtr^.IntData[3] := NumCylinders;
    BufferPtr^.IntData[4] := WriteCompCyl;

    for i:=1 to 8 do BufferPtr^.PartName[i] := RootName[i];
    BufferPtr^.FillerKind := DiskRelative;
    BufferPtr^.PartStart := FirstDA;
    BufferPtr^.PartEnd := LastDA;
    BufferPtr^.PartRoot := RootDA;
    BufferPtr^.PartKind := Root;
    BufferPtr^.PartDevice := configuration;
    for i:=0 to 63 do BufferPtr^.SubParts[i] := PartDAs[i];
    for i:=0 to 25 do
      begin
      BufferPtr^.BootTable[i] := Boots[i];
      BufferPtr^.InterpTable[i] := Interps[i];
      end;
    
    LabelPtr^.SerialNum := RootDA;
    LabelPtr^.LogBlock := 0;
    LabelPtr^.Filler := 0;
    LabelPtr^.PrevAdr := Zero;
    LabelPtr^.NextAdr := Zero;

    if not DryRun then
      DiskIO(RootDA, BufferPtr, LabelPtr, DskFirstWrite);
    
    PrintDIB(BufferPtr);
    end;



{*********************** FirstPartition *************************}
Procedure FirstPartition;
    var
        NumPages   : integer;
        Rem        : FSBit32;
        L          : Long;
        gotanswer  : boolean;
        MaxPages   : integer;
        i, j       : integer;
        Pages, Remainder, NextDA: FSBit32;
        Temp       : FSBit32;
    Label 1;
    begin
(*
        if  EIOFlag
        and (Configuration = Winch24)
        then if Ask( 'Do you want to format the disk? ', 'No' )
        then if Ask( 'Are you sure? ', 'Yes' )
        then if not DryRun
        then FormatDisk;
*)
        for i:=0 to 63 do 
          begin
          PartDAs[i] := Zero;
          PartNames[i] := '        ';
          end;
        for i:=0 to 25 do
          begin
          Boots[i] := Zero;
          Interps[i] := Zero;
          end;
        StartDA := DoubleAdd(FirstDA, WordsPerBlock);

        RoundUp(StartDA, Remainder);
        PagesLeft := DoubleSub(PagesLeft, Remainder);
        NextDA := DoubleAdd(StartDA, DoubleMul(PagesLeft,WordsPerBlock));
        RoundDown(NextDA, Remainder);
        PagesLeft := DoubleSub(PagesLeft, Remainder);
        
        autoIndex := 1;
        if (Configuration = Generic5Inch) And (Automatic) 
            then autoindex := autoindex + 1;
        

        gotanswer := false;
        repeat
          Write('Name for root partition (up to 8 chars): ');
          gotanswer := GetName(RootName,8);
        until gotanswer;
        for PartNumber := 0 to 63 do
          begin
            Cheat.Lng := LogAddrToPhysAddr(StartDA);
            PartDAs[PartNumber] := StartDA;
            cheat.lng := PagesLeft;
            if (cheat.dbl[1] <> 0) or (cheat.dbl[0] < 0)
              then MaxPages := MaxPartSize
              else MaxPages := cheat.dbl[0];
            WriteLn('There are',MaxPages,' pages available for partition',
                  PartNumber,'.');
            Write('How many pages would you like in it? (0 => all) ');
            repeat
              if automatic then
                 begin
                 If configuration = Generic5Inch Then
                     begin
                     numpages := 10080;
                     NextDA := DoubleAdd(StartDA,
                                  DoubleMul(DoubleInt(NumPages),
                                            WordsPerBlock));
                     RoundDown(NextDA,Remainder);
                     NumPages := IntDouble(DoubleSub(DoubleInt(NumPages), 
                                                  Remainder));
                     If MaxPages < NumPages then numpages := maxpages;
                    end
                 else if maxPages < 10080 then numPages := maxPages
                     else numPages := 10080;
                 WriteLn(numPages:1);
                 end
              else ReadLn(NumPages);
              gotanswer := true;
              if (NumPages = 0) then NumPages := MaxPages;
              if (NumPages < 2) then
                begin
                Write('Too few pages.  How many pages? ');
                gotanswer := false;
                end;
              if (NumPages > MaxPages) then
                begin
                Write('Too many pages.  How many pages? ');
                gotanswer := false;
                end;
              NextDA := DoubleAdd(StartDA,
                                  DoubleMul(DoubleInt(NumPages),
                                            WordsPerBlock));
              RoundDown(NextDA,Remainder);
              Pages := DoubleSub(DoubleInt(NumPages), Remainder);
              Cheat.Lng := Remainder;
              if (Cheat.Dbl[0] <> 0) or (Cheat.Dbl[1] <> 0) then
                begin
                Cheat.Lng := Pages;
                Write('Not a track multiple.  Next smallest = ',
                      Cheat.Dbl[0]:1, '.  How many pages? ');
                GotAnswer := False
                end;
            until gotanswer;
            GetPartName(PName);
            PartNames[PartNumber] := PName;
            EndDA := DoubleSub(NextDA,WordsPerBlock);
            InitPartition(PartNames[PartNumber],StartDA,EndDA);
            StartDA := NextDA;
            PagesLeft := DoubleSub(PagesLeft,DoubleInt(NumPages));
            if PagesLeft = Zero then goto 1;
          end;
1:
    WriteRootDIB;
        
    end;  {FirstPartition}


{*********************** SplitPartition ******************************}
procedure SplitPartition;
    var
        i : integer;
        End1DA, Start2DA : FSBit32;
        MaxPages, NumPages : integer;
        Pages, Remainder, NextDA : FSBit32;
    begin
        if not Ask('Are you sure you want to split? ','Yes') then
          exit(SplitPartition);
        
        i := 62;
        while i <> PartNumber do
          begin
          PartDAs[i+1] := PartDAs[i];
          PartNames[i+1] := PartNames[i];
          i := i - 1;
          end;
        

        RoundUp(StartDA, Remainder);
        PartDAs[PartNumber] := StartDA;
        NextDA := DoubleAdd(EndDA, WordsPerBlock);
        RoundDown(NextDA, Remainder);
        EndDA := DoubleSub(NextDA, WordsPerBlock);
        MaxPages := AddrToField(NextDA) - AddrToField(StartDA);
        
        WriteLn('There are ', MaxPages, ' pages available.');
        Write('How many pages would you like in the first half? ');
        gotanswer := false;
        repeat
          ReadLn(NumPages);
          gotanswer := true;
          if (NumPages < 2) then
            begin
            Write('Too few pages.  How many pages? ');
            gotanswer := false;
            end;
          if (NumPages > MaxPages) then
            begin
            Write('Too many pages.  How many pages? ');
            gotanswer := false;
            end;
          NextDA := DoubleAdd(StartDA,
                              DoubleMul(DoubleInt(NumPages),
                                        WordsPerBlock));
          RoundDown(NextDA,Remainder);
          Pages := DoubleSub(DoubleInt(NumPages), Remainder);
          Cheat.Lng := Remainder;
          if (Cheat.Dbl[0] <> 0) or (Cheat.Dbl[1] <> 0) then
            begin
            Cheat.Lng := Pages;
            Write('Not a track multiple.  Next smallest = ',
                  Cheat.Dbl[0]:1, '.  How many pages? ');
            GotAnswer := False
            end;
        until gotanswer;
        
        End1DA := DoubleSub(NextDA,WordsPerBlock);
        InitPartition(PartNames[PartNumber], StartDA, End1DA);
        
        Start2DA := NextDA;
        PartDAs[PartNumber+1] := Start2DA;
        WriteLn;
        Write('Name of second half (new partition)? ');
        GetPartName(PName);
        for i := 1 to 8 do PartNames[PartNumber+1][i] := PName[i];
        InitPartition(PName, Start2DA, EndDA);
        
        WriteRootDIB; 
    end;


{*********************** MergePartition ******************************}
procedure MergePartition;
    var
        i : integer;
    begin
        WriteLn;
        WriteLn('About to merge selected partition with: ');
        DiskIO(PartDAs[PartNumber+1], BufferPtr, LabelPtr, DskRead);
        PrintDIB(BufferPtr);
        if not Ask('Are you sure you want to merge? ','Yes') then
          exit(MergePartition);
        
        DiskIO(PartDAs[PartNumber+1], Buffer2, Label2, DskRead);
        EndDA := Buffer2^.PartEnd;
        
        InitPartition(PartNames[PartNumber], StartDA, EndDA);
        
        for i := PartNumber+1 to 62 do
          begin
          PartDAs[i] := PartDAs[i+1];
          PartNames[i] := PartNames[i+1];
          end;
        PartDAs[63] := Zero;
        for i := 1 to 8 do PartNames[63][i] := ' ';
        
        WriteRootDIB;
    end;


{$ifc AllowAdjust then}
{*********************** AdjustPartition ******************************}
procedure AdjustPartition;
    var
        i : integer;
        NextDA, Remainder: FSBit32;
    begin
        if not Ask('Are you sure you want to adjust? ','Yes') then
          Exit(AdjustPartition);
        DiskIO(PartDAs[PartNumber+1], BufferPtr, LabelPtr, DskRead);

        RoundUp(StartDA, Remainder);
        PartDAs[PartNumber] := StartDA;
        NextDA := DoubleAdd(EndDA, WordsPerBlock);
        RoundDown(NextDA, Remainder);
        EndDA := DoubleSub(NextDA, WordsPerBlock);
        
        InitPartition(PartNames[PartNumber], StartDA, EndDA);
        
        WriteRootDIB;
    end;
{$endc}


{*********************** RePartition ******************************}
procedure RePartition;
    var 
        NumPOSParts : integer;
        NumParts : integer;
        i, j     : integer;
    begin
        DiskIO(RootDA, BufferPtr, LabelPtr, DskRead);
        NumParts := 0;
        NumPOSParts := 0;
        RootName := '        ';
        for i := 1 to 8 do RootName[i] := BufferPtr^.PartName[i];
        for i := 0 to 63 do
          begin
          PartDAs[i] := BufferPtr^.SubParts[i];
          PartNames[i] := '        ';
          if (PartDAs[i] <> Zero) then
            begin
            NumParts := NumParts + 1;
            If NotOver64K(PartDAs[i]) then
              begin
              DiskIO(PartDAs[i], Buffer2, Label2, DskRead);
              If NotOver64K(Buffer2^.PartEnd) then
                begin
                for j := 1 to 8 do
                  PartNames[i][j] := Buffer2^.PartName[j];
                NumPOSParts := NumPOSParts + 1;
                end;
              end;
            end;
          end;
        for i := 0 to 25 do
          begin
          Boots[i] := BufferPtr^.BootTable[i];
          Interps[i] := BufferPtr^.InterpTable[i];
          end;

        PrintDIB(BufferPtr);
  
        if Ask('Do you want to rename the Device? ','No') then
            begin
            WriteLn('**** WARNING ****');
            WriteLn('* After renaming the device, no programs currently on it will run!!');
            if Ask('* Are you sure you want to do this? ','No') then
              begin
              Write('New device name [',BufferPtr^.PartName,'] ');
              gotanswer := GetName(name,8);
              if gotAnswer and (not dryRun) then
                 begin
                 for i := 1 to 8 do
                    BufferPtr^.PartName[i] := name[i];
                 DiskIO(RootDA, BufferPtr, LabelPtr, DskWrite);
                 end;
              end;
            exit(RePartition);
            end;
        gotanswer := false;
        repeat
          Write('Which partition do you want to modify? ');
          ReadLn(PName);
          if length(PName) > 0 then
            begin
            name := '        ';
            for i := 1 to length(PName) do name[i] := PName[i];
            for i := 0 to NumPOSParts - 1 {was 63} do
              if UpperEqual(Name, PartNames[i]) then
                begin
                PartNumber := i;
                DiskIO(PartDAs[i], Buffer2, Label2, DskRead);
                StartDA := PartDAs[i];
                EndDA := Buffer2^.PartEnd;
                gotanswer := true;
                end;
            end;
        until gotanswer;
        
        WriteLn;
        PrintDIB(Buffer2);
        WriteLn;
        
        if (NumParts < MaxNumAccentParts) {was 64}
           and
           (NumPOSParts < MaxNumPOSParts) then
          if Ask('Do you want to split this partition? ','No') then
            begin
            SplitPartition;
            exit(RePartition);
            end;
        
        if PartNumber < NumPOSParts - 1 {was 63} then
          if (PartDAs[PartNumber+1] <> Zero) then {it ought to be now!}
            if Ask('Do you want to merge this partition with the next? ',
                 'No') then
              begin
              MergePartition;
              exit(RePartition);
              end;
        
{$ifc AllowAdjust then}
        if Ask('Do you want to adjust this partition to cylinder boundaries? ',
               'No') then
            begin
            AdjustPartition;
            exit(RePartition)
            end;
{$endc}
        
        if Ask('Do you want to initialize this partition? ','No') then
          begin
          InitPartition(PartNames[PartNumber], StartDA, EndDA);
          exit(RePartition);
          end;
        
        if Ask('Do you want to change the partition name? ','No') then
          begin
          WriteLn('**** WARNING ****');
          WriteLn('* After renaming the partition, no programs currently in it will run!!');
          if Ask('* Are you sure you want to do this? ','No') then
            begin
            Write('New partition name: ');
            GetPartName(PName);
            DiskIO(StartDA, BufferPtr, LabelPtr, DskRead);
            for i := 1 to length(PName) do BufferPtr^.PartName[i] := PName[i];
            for i := length(PName)+1 to 8 do BufferPtr^.PartName[i] := chr(32);
            if not DryRun then
                DiskIO(StartDA, BufferPtr, LabelPtr, DskFirstWrite);
            WriteLn;
            PrintDIB(BufferPtr);
            end;
          exit(RePartition);
          end;
        
    end;

Procedure PartParseCmdLine;
   Procedure ReportError;
        begin
        WriteLn('** For /Build switch, need device (F or H) followed by partition names');
        exit(DoPartition);
        end;
   var arg, newbroke, broke: String;
   begin
   RemDelimiters(UsrCmdLine, ' /', broke);
   GetSymbol(UsrCmdLine, arg, ' /', broke);  {remove partition}
   RemDelimiters(UsrCmdLine,' /',broke);
   GetSymbol(UsrCmdLine, arg, ' /',broke);
   ConvUpper(arg);
   numAuto := 0;
   DiskName := '';
   if arg = '' then exit(PartParseCmdLine)
   else if arg <> 'BUILD' then
     begin
     DiskName := Arg;
     RemDelimiters(UsrCmdLine,' /',broke);
     GetSymbol(UsrCmdLine, arg, ' /',newbroke);
     ConvUpper(Arg);
     If Arg <> 'BUILD' Then
       begin
       WriteLn('** Illegal arguments to Partition.');
       Writeln('** Follow /<DiskName> switch with /BUILD for 5.25" disks.');
       Writeln('** For other disks use /Build alone.');
       exit(DoPartition);
       end;
     numauto := 1;
     AutoNames[1] := DiskName;
     end;
   RemDelimiters(UsrCmdLine, ' /',broke);
   GetSymbol(UsrCmdLine, arg, ' /',broke);
   ConvUpper(arg);
   if arg = '' then ReportError
   else if arg[1] = 'H' then Configuration := Winch24
   else if arg[1] = 'F' then Configuration := FloppyDouble
   else ReportError;
   repeat
     RemDelimiters(UsrCmdLine, ' /',broke);
     GetSymbol(UsrCmdLine, arg, ' /',broke);
     if arg <> '' then
         begin
         numAuto := numAuto+1;
         autoNames[numAuto] := arg;
         end;
   until arg = '';
   automatic := true;
   end;

Procedure CheckNumNames;
   var numNeeded: integer;
   begin
   case Configuration of
     Winch12: numNeeded := 4;
     Winch24: if EIOFlag
              then numNeeded := 7
              else numNeeded := 6;
     CIOMicrop : numNeeded := 8;
     FloppySingle: numNeeded := 2;
     FloppyDouble: numNeeded := 2;
     Generic5Inch: numneeded := 1;
     end;
   if numAuto < numNeeded then
      begin
      WriteLn('** Number of names provided: ',numAuto:1,' is less than');
      WriteLn('** the number needed for the device: ',numNeeded:1);
      WriteLn('** Aborting!');
      exit(DoPartition);
      end;
   end;

Procedure CheckInitialized;
  Handler VBNOutOfRange(vid: VolID; vbn: VolBlockNumber);
    begin {disk probably no good}
    exit(CheckInitialized)
    end;

   var ok: boolean;
   begin
   ok := TryDiskIO(RootDA, BufferPtr, LabelPtr, DskRead, NUMTRIES);
   if ok then
      if (BufferPtr^.PartRoot <> RootDA) and
         (BufferPtr^.PartKind <> Root) and
         (BufferPtr^.PartDevice <> configuration) and
         (LabelPtr^.SerialNum <> RootDA) then {not initialized}
      else  begin
            WriteLn('*****************************************************');
            WriteLn('**  WARNING WARNING WARNING WARNING WARNING WARNING');
            WriteLn('*****************************************************');
            WriteLn(chr(7));
            WriteLn('**  This disk appears to contain useful data!!!!');
            automatic := false;
            if Ask('Do you want to destroy the contents of this disk? ','NO')
                then if Ask('Are you sure? ','NO') then
                        automatic := true; 
            end
   end;


{*********************** DoPartition ******************************}
    begin

    automatic := false;
    
    if SwappingAllowed then
         begin
         SavedSwapID := SwapID;
         ShouldReEnableSwapping := true;
         DisableSwapping;
         end;

        WriteLn('Device Partition initialization program version ',
                PartVersion, '.');
        
   PartParseCmdLine;

        WriteLn;
        DryRun := Ask('Do you want to debug? (does not do any writes) ','No');
        Writeln;

        WordsPerBlock := DoubleInt(DISKBUFSIZE);
        Zero := DoubleInt(0);
        One  := DoubleInt(1);
        new(0, 256, BufferPtr);
        new(0, 256, Buffer2);
        new(0, 256, Buffer3);
        new(0, 4, LabelPtr);
        new(0, 4, Label2);
    
        InitDiskIO;

        if not automatic then 
           begin
           repeat
              Write('Partition Harddisk (H) or Floppy (F)? ');
              ReadLn(c);
              if c = 'f' then c := 'F'
              else if c = 'h' then c := 'H';
            until (c = 'H') or (c = 'F');

           if c = 'F' then Configuration := FloppyDouble
           else Configuration := Winch24;
           end;
        

        if Configuration = FloppyDouble
        then begin
             repeat
               Write('Is this a Single (S) or Double (D) sided floppy? ');
               ReadLn(c);
               if c = 's' then c := 'S'
               else if c = 'd' then c := 'D';
             until (c = 'D') or (c = 'S');
             if c = 'D' then Configuration := FloppyDouble
             else Configuration := FloppySingle;
             end
      
        else begin
             Write('This seems to be a ');
             case GetIntDiskKinds of
                Shugart14: begin
                            Write('SHUGART 14-Inch');
                            Configuration := Winch12;
                           end;
                Mic8     : begin
                            Write('MICROPOLIS 8-Inch');
                            if not eioflag then
                              configuration:= CIOMicrop
                           end;
                Mic5     : begin
                            Write('5.25"');
                            configuration := Generic5Inch;
                           end;
                Otherwise: Write('*** ILLEGAL TYPE ***');
                end;
             WriteLn(' disk.');
             if not Ask('Is this right? ','Yes') then
                begin
                WriteLn('** Your IO board hardware is probably bad **');
                if not Ask('** Are you sure you want to continue? ','No')
                    then exit(DoPartition);
                end;

             If Configuration = Generic5Inch Then
                 begin
                 SetupDiskParams(Automatic, 
                                 DiskName, 
                                 NumHeads,
                                 NumCylinders,
                                 SecPerTrk, 
                                 Bootsize, 
                                 WriteCompCyl);
                 GetDiskParameter(NumHeads, secPerTrk, NumCylinders);
                 If Automatic and (DiskName = '') Then
                     Begin
                     WriteLn('** Illegal arguments to Partition.');
                     Writeln('** Disk Name must be specified for 5.25" with /Build');
                     Writeln('** For other disks use /Build alone.');
                     Exit(DoPartition);
                     End;
                 end;


             
             if Configuration = Winch12 then
               if IO24MByte then if Ask('Is this a 24 MByte disk? ','Yes') 
                                    then Configuration := Winch24
                                 else
               else if Ask('Is this a 12 MByte disk? ','Yes') then
                    else Configuration := Winch24;
             end;

        PagesLeft := NumberPages(configuration);
        LastDA := LastDiskAddr(configuration);

        cheat.lng := LastDA;
        cheat.dbl[0] := 0;
        cheat.dbl[1] := land(cheat.dbl[1], #177400);
        FirstDA := cheat.lng;
        RootDA := FirstDA;
        Disk := WhichDisk(RootDA);
        
        cheat.lng := LogAddrtoPhysAddr(LastDA);
        case Disk of
          0: if EIOFlag or 
                (ciodisktype = ciomicropolis) or 
                (Configuration =Generic5Inch)
               then LastCyl := Cheat.Dbl[1]
               else LastCyl := Shift(Cheat.Dbl[0], -8);
          1: LastCyl := 0; {hopefully not used}
          end;
{$ifc printmap then}
        writeln;
        writeln('LastCyl = ', LastCyl:1);
        writeln;
{$endc}
        if Automatic then CheckInitialized;
        if Automatic then CheckNumNames;    {CheckInitialized may clear auto }
        
        DeviceDismount(Disk);

        if automatic then FirstPartition
        else if Ask('Do you want to initialize the whole device? ','No') then
                 begin
                 if Ask('Are you sure? ','Yes') then FirstPartition;
                 end
             else RePartition;
        
        if Ask('Do you want the device remounted? ','Yes') then
          DeviceMount(Disk);
    end.
