{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Memory;
{-----------------------------------------------------------------------------
{
{       Memory - Perq memory manager.
{       J. P. Strait        1 Jan 80.
{       Copyright (C) Three Rivers Computer Corporation, 1980, 1982, 1983.
{
{ Abstract:
{       Memory is the Perq memory manager.  It supervises the segment tables
{       and exports procedures for manipulating memory segments.
{       Perq physical memory is segmented into separately addressable items
{       (called segments) which may contain either code or data.
{
{ Design:
{       See the Q-Code reference manual.
{
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------
{
{  3 Nov 83  V2.23  Sandeep Johar found with John Strait
{     Fix a bug in DecRefCount which was not causing the entire list of 
{     segments to be destroyed if the reference count hit zero.
{
{ 22 May 83  V2.22  Sandeep Johar
{     In DisableSwapping only destroy the not bootloaded segments.
{
{  2 May 83  V2.21  Sandeep Johar
{     Resident data segments which had pointers to spice segments on the 
{     disk were not being destroyed when swapping was disabled; fixed this
{     bug.
{
{ 31 Mar 83  V2.20  Sandeep Johar
{     Set and test 5 bits of a disk address to determine
{     if it is a floppy. The result is that 1MB is kept for
{     floppy and 31mb are availble for the disk.
{}

{ 28 Mar 83  V2.19  Brad Myers
{ Fix serious bug in InitMemory where it changes the value of a memory location
{ while trying to decide what the size of memory is.  Thanks to J Strait for
{ fix.
{}

{  8 Jan 83  V2.18  Brad Myers
{ Change Maximum number of blocks in a segment to 4096.
{ Move increment in SIT.
{ Make BootedMemoryInBlocks variable and put it in the SAT at boot time.
{ Comment that segments should not be > 256 for NEW or MakePtr.
{}

{  5 Jan 83  V2.17  Don Scelza
{ Changed SetMobility to not raise and exception if a
{ segment is set to UnMovable when swapping is not allowed.
{ This will allow the Ethernet code to work when the system
{ is booted from a floppy.
{}

{ 10 Dec 82  V2.16  C.Beckett for AGR
{ Increase size of IO segment.
{ }
{ 30 Nov 82  V2.15  J Strait.
{ Make SetIncrement and SetMaximum do nothing for heaps.
{ }

{ 17 Nov 82  V2.14  J Strait.
{ 1. Change name of Sharable bit to Heap in SAT.
{ 2. Overlay HeapNext field on Increment and Maximum.
{ 3. Make IncRefCount and DecRefCount follow HeapNext for heaps.
{ 4. Add new routines IncOneRef and DecOneRef for use by IncRefCount,
{    DecRefCount, MarkMemory, and ReleaseMemory.
{ 5. When RefCount=0 and IOCount<>0 sets HeapNext to self for heaps.
{ }

{ 16 Nov 81  V2.13  DAS
{ Fixed to work with ten MBaud ethernet.
{ }

{ 25 Oct 81  V2.12  JPS 
{ Enlarge segment table to 512 entries when more than 1/4 MByte.
{ }

{ 25 Oct 81  V2.11  JPS 
{ Implement full hints in Memory.
{ 1) The two added words (for the UpdateTime) are overlayed on the two
{    words of Increment, Maximum, and FreeList which are not used for
{    Code segments.
{ 2) ChangeSize and NewP now check to be sure the segment is a Data segment.
{ 3) FindCodeSegment compares hints.
{ }

{  6 Oct 81  V2.10  JPS 
{ Fix bug in CreateSegment:  It cannot call ChangeSize to shorten a hole
{ because the new size may be larger than 256 blocks.  ChangeSize would
{ raise BadSize exception.
{ }

{  4 Oct 81  V2.9  JPS 
{ Change InitMemory to figure out how much physical memory the machine has.
{ }

{ 18 Sep 81  V2.8  WJH 
{ Set Mobility to Swappable in CreateSegment.
{ }

{ 26 Jul 81  V2.7  JPS
{ Fix bug in order of operations in CreateSegment.
{ }

{ 21 Jul 81  V2.6  JPS
{ Don't use MaxSegment anywhere.
{ }

{ 29 Jun 81  V2.5  JPS
{ Destroy swapping files when swapping is disabled.
{ }

{ 4 Jun 81  V2.4  JPS
{ Add Virgil headers for exceptions.
{ }

{ 26 May 81  V2.3  JPS
{ Add CurrentSegment function.
{ Prevent Memory from being swapped during FindHole calls.
{ }

{ 21 may 81  V2.2  JPS
{ Initialize BootSegId in EnableSwapping.
{ }

{ 14 May 81  V2.1  GGR
{ Add support for 3 MBaud EtherNet.
{ }

{ 12 May 81  V2.0  JPS
{ 1) Split Memory into two modules: Memory (user callable routines, swappable)
{    and Virtual (system callable routines, unswappable).
{ 2) Move FileIdToSegId and SegIdToFileId into Memory.
{ 3) Use exceptions and get rid of MemoryError.
{ }

{ 24 Mar 81  V1.9  JPS
{ Begin adding stuff for virtual memory.
{ }

{ 23 Mar 81  V1.8  JPS
{ Convert to standard documentation form.
{ Delete DEBUG stuff.
{ Delete OutputF.
{ Add "MM" to the beginning of names which are exported but are not needed by
{ programs which import the memory manager.
{ Remove Concat call from MemoryError.
{ Remove import of Perq.String.
{ }

{ 24 Feb 81  V1.7  JPS
{ 1) Allow the ScreenSeg to change sizes.  To allow this, the memory
{    manager must prevent system segments (those with RefCount > 1) from
{    being moved into the area of memory which might be used for the
{    screen.  This is a hack which guarantees that the system can expand
{    the screen to its original size after returning from a user program.
{    This change was originally made by RFR, but had to be converted for
{    memory manager version 1.5 and greater.
{ 2) Remove PrintTable and PrintFreeList.
{ }

{ 23 Feb 81  V1.6  RFR
{ Added DK and CH to system boot record (they will be filled by the boot
{ microcode with the disk number and boot character used in booting).
{ }

{ 18 Feb 81  V1.5  JPS
{ Define fields in the SIT to remember names of boot loaded segments.
{ Remove most of InitMemory since it is done now by MakeBoot.  Make IOSeg
{ a constant.
{ }

{ 17 Feb 81  V1.4  DAS
{ Removed the include file SegNumbers.  Placed the segment number
{ definitions inline.
{ }

{ 16 Feb 81  V1.3  DAS
{ Changed to use Perq_String from Perq.String
{ }

{ 11 Feb 81        DCF
{ Changed calls to PString to conform to new PString.  This module is
{ compatable with the new System and Compiler.
{ }

{ 13 Jan 81  V1.2  JPS
{ 1) Allocate the IOSeg in memory manager initialization as a locked
{    segment at the high end of memory.  Use the last block in memory (the
{    Krnl no longer needs it).
{ 2) Move $R- to private part.
{ }

{ 10 Oct 80  V1.1  JPS
{ Add support for the diagnostic display (DDS).
{ }

exports


const MemoryVersion = '2.23';


imports SystemDefs from SystemDefs;
imports Code from Code;


const SATSeg = 1;              { SAT segment }
      SITSeg = 2;              { SIT segement }
      FontSeg = 3;             { font segment }
      ScreenSeg = 4;           { screen segment }
      CursorSeg = 5;           { cursor segment }
      IOSeg = 6;               { IO segment }
      SysNameSeg = 7;          { system segment names }
      
      BlocksInQuarterMeg = #1000; { 512 blocks in 1/4 meg memory}
      BlocksInHalfMeg    = #2000; { 1024 blocks in 1/2 meg memory}
      BlocksInMeg        = #4000; { 2048 blocks in 1 meg memory}

      BlocksForPortraitScreen = 192; { number of blocks needed for its segment}
      BlocksForLandscapeScreen = 320;
      
      MaxSegment = #137;       { should be 2**16 - 1 }

      SetStkBase = #60;
      SetStkLimit = #120;

{$ifc Ether3MBaud then}
      IOSegSize = 10;           { number of blocks in the IOSeg }
{$elsec}
{$ifc Ether10MBaud then}
      IOSegSize = 8;           { number of blocks in the IOSeg }
{$elsec}
      IOSegSize = 8;           { number of blocks in the IOSeg }
{$endc}
{$endc}
      
      SysSegLength = 8;       { length of name of a boot-loaded segment }

      MMMaxBlocks = #10000;   { maximum number of blocks in a segment }
      MMMaxCount =  #377;     { maximum reference count }
      MMMaxIntSize = MMMaxBlocks-1;
      MMMaxExtSize = MMMaxBlocks;
      


type MMBit4 = 0..#17;
     MMBit8 = 0..#377;
     MMBit12 = 0..#7777;
     MMIntSize = 0..MMMaxIntSize;
     MMExtSize = 1..MMMaxExtSize;
     MMAddress = integer;
     MMPosition = (MMLowPos, MMHighPos);

     SegmentNumber = integer;

     SegmentKind = (CodeSegment, DataSegment);
     
     SegmentMobility = (UnMovable, UnSwappable, LessSwappable, Swappable);

     MMFreeNode = record
      N: MMAddress;
      L: integer
      end;
      
     MMBlockArray = array[0..0] of array[0..127] of integer;
     
     pMMBlockArray = ^MMBlockArray;

     MMArray = record case Integer of
                1: (m: array[0..0] of MMFreeNode);
                2: (w: array[0..0] of Integer)
                end;

     pMMArray = ^MMArray;

     MMPointer = record case integer of
      1: (P: ^integer);
      2: (B: pMMBlockArray);
      3: (M: pMMArray);
      4: (Offset: MMAddress;
          Segmen: SegmentNumber)
      end;

     SATentry = packed record { Segment Address Table }
      { **** ENTRIES MUST BE TWO WORDS LONG **** }
      NotResident : boolean;            { 001 }
      Moving      : boolean;            { 002 }
      RecentlyUsed: boolean;            { 004 }
      Heap        : boolean;            { 010 }
      Kind        : SegmentKind;        { 020 }
      Full        : boolean;            { 040 }
      InUse       : boolean;            { 100 }
      Lost        : boolean;  { *** }   { 200 }
      BaseLower   : MMBit8;
      BaseUpper   : MMBit4;
      Size        : MMBit12
      end;

{$IFC WordSize(SATentry) <> 2 then}
{$MESSAGE ******* ERROR ***** SAT WRONG SIZE ****** }
{$ENDC}

     SITentry = packed record case integer of { Segment Information Table }
      { **** ENTRIES MUST BE EIGHT WORDS LONG **** }
      1: { real SIT entry }
         (NextSeg    : SegmentNumber;
          RefCount   : 0..MMMaxCount;
          IOCount    : 0..MMMaxCount;
          Mobility   : SegmentMobility;
          BootLoaded : Boolean;
          Increment  : MMIntSize; {used only if DataSegment and Heap is false}
          SwapInfo   : record case {BootLoaded:} Boolean of
                         True:  (BootLowerAddress: Integer;
                                 BootUpperAddress: Integer;
                                 BootLogBlock: Integer);
                         False: (DiskLowerAddress: Integer;
                                 DiskUpperAddress: Integer;
                                 DiskId: Integer)
                         end;
          case SegmentKind of
            DataSegment: (case {Heap:} Boolean of
                            False: (Maximum    : MMIntSize;
                                    Freelist   : MMAddress);
                            True:  (HeapNext   : SegmentNumber;
                                    Freelst    : MMAddress)
                         );
            CodeSegment: (Update     : TimeStamp)
         );
      2: { boot time information }
         (BootBlock: record
           CS: SegmentNumber;  { initial code segment }
           SS: SegmentNumber;  { initial stack segment }
           XX: Integer;        { used as interface between SysB and Config}
           VN: Integer;        { system version number }
           FF: SegmentNumber;  { first free segment number }
           FC: SegmentNumber;  { first system code segment }
           DK: integer;        { disk system was booted from }
           CH: integer         { char used in booting }
           end)
      end;

{$IFC WordSize(SITentry) <> 8 then}
{$MESSAGE ******* ERROR ***** SIT WRONG SIZE ****** }
{$ENDC}

     SATarray = array[0..0] of SATentry;

     SITarray = array[0..0] of SITentry;

     pSAT = ^SATarray;

     pSIT = ^SITarray;

     MMEdge = record
             H: SegmentNumber;  { Head }
             T: SegmentNumber   { Tail }
             end;

     SysSegName = packed array[1..SysSegLength] of Char;
     
     pSysNames = ^SysNameArray;
     
     SysNameArray = array[0..0] of SysSegName;
     
             
 procedure InitMemory;
 procedure DataSeg( var S: SegmentNumber );
 procedure CodeOrDataSeg( var S: SegmentNumber );
 procedure ChangeSize( S: SegmentNumber; Fsize: MMExtSize );
 procedure CreateSegment( var S: SegmentNumber;
                          Fsize, Fincrement, Fmaximum: MMExtSize );
 procedure IncRefCount( S: SegmentNumber );
 procedure SetMobility( S: SegmentNumber; M: SegmentMobility );
 procedure DecRefCount( S: SegmentNumber );
 procedure SetIncrement( S: SegmentNumber; V: MMExtSize );
 procedure SetMaximum( S: SegmentNumber; V: MMExtSize );
 procedure SetHeap( S: SegmentNumber; V: boolean );
 procedure SetKind( S: SegmentNumber; V: SegmentKind );
 procedure MarkMemory;
 procedure CleanUpMemory;
 procedure FindCodeSegment( var S: SegmentNumber; Hint: SegHint );
 procedure EnableSwapping( Where: Integer );
 procedure DisableSwapping;
 function  CurrentSegment: SegmentNumber;
 
 exception UnusedSegment( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       UnusedSegment is raised when the memory manager encounters a segment
{       number which references a segment which is not in use.  This may mean
{       that a bad segment number was passed to some memory manager routine
{       or that a bad address was de-referenced.
{
{ Parameters:
{       S - Segment number of the unused segment.
{
{-----------------------------------------------------------------------------}


 exception NotDataSegment( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       NotDataSegment is raised when the number of a code segment is passed
{       to some memory manager routine that requires the number of a data
{       segment.
{
{ Parameters:
{       S - Segment number of the code segment.
{
{-----------------------------------------------------------------------------}


 exception BadSize( S: SegmentNumber; Fsize: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       BadSize is raised when a bad Size value is passed to some memory
{       manager routine.  This usually means that the size passed to
{       CreateSegment or ChangeSize is greater than the maximum size or
{       less than one.
{
{ Parameters:
{       Fsize - The bad Size value.
{
{-----------------------------------------------------------------------------}


 exception BadIncrement( S: SegmentNumber; Fincrement: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       BadIncrement is raised when a bad Increment value is passed to some
{       memory manager routine.  This usually means that the increment passed
{       to CreateSegment is greater than 256 or less than one.
{
{ Parameters:
{       Fincrement - The bad Increment value.
{
{-----------------------------------------------------------------------------}


 exception BadMaximum( S: SegmentNumber; Fmaximum: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       BadMaximum is raised when a bad Maximum value is passed to some memory
{       manager routine.  This usually means that the maximum passed to
{       CreateSegment is greater than 256 or less than one.
{
{ Parameters:
{       Fmaximum - The bad Maximum value.
{
{-----------------------------------------------------------------------------}


 exception FullMemory;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       FullMemory is raised when there is not enough physical memory to
{       satisfy some memory manager request.  This is raised only after
{       swapping segments out and compacting memory.
{
{-----------------------------------------------------------------------------}


 exception CantMoveSegment( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CantMoveSegment is raised when the memory manager attempts to move
{       a segment which is UnMovable or has a non-zero IO count.
{
{ Parameters:
{       S - The number of the segment which cannot be moved.
{
{-----------------------------------------------------------------------------}


 exception PartNotMounted;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       PartNotMounted is raised when
{               1) the memory manager attempts to swap a data segment out for
{                  the first time
{       and     2) the partition which is to be used for swapping is no longer
{                  mounted.
{
{-----------------------------------------------------------------------------}


 exception SwapInFailure( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapInFailure is raised when the swap file cannot be found for a
{       segment which is marked as swapped out.  This is an error which
{       should never happen in a debugged system.  It usually means that
{       there is a bug in the memory manager or that the segment tables
{       have been clobbered.
{
{ Parameters:
{       S - The number of the segment which could not be swapped in.
{
{-----------------------------------------------------------------------------}


 exception EdgeFailure;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       EdgeFailure is raised by MakeEdge when it discovers that the SIT
{       entries are not linked together into a circular list.  This is an
{       error which should never happen in a debugged system.  It usually
{       means that there is a bug in the memory manager or that the segment
{       tables have been clobbered.
{
{-----------------------------------------------------------------------------}


 exception NilPointer;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       NilPointer is raised when a Nil pointer is used or passed to Dispose.
{
{-----------------------------------------------------------------------------}


 exception BadPointer;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       BadPointer is raised when a bad pointer is passed to Dispose.
{
{ Parameters:
{
{-----------------------------------------------------------------------------}


 exception FullSegment;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       FullSegment is raised by New when it discovers that there is not
{       enough room to allocate and the segment cannot be enlarged (its
{       size has reached its maximum).
{
{-----------------------------------------------------------------------------}


 exception NoFreeSegments;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       NoFreeSegments is raised when the memory manager discovers that all
{       of the segment numbers are in use and it needs another one.  This
{       is equivalent to "Segment table full".
{
{-----------------------------------------------------------------------------}


 exception SwapError;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapError is raised if the one of the memory managers swapping
{       routines is called when swapping is disabled.  This is an error which
{       should never happen in a debugged system.  It usually means that
{       there is a bug in the memory manager.
{
{-----------------------------------------------------------------------------}


 

var SAT: pSAT;
    SIT: pSIT;
    MMFirst, MMFree, MMLast, MMHeap: SegmentNumber;
    MMHole: MMEdge;
    MMState: (MMScan1, MMScan2, MMScan3, MMScan4, MMScan5,
              MMScan6, MMScan7, MMScan8, MMScan9, MMScan10,
              MMScan11,
              MMNotFound, MMFound);
    StackSegment: SegmentNumber;
    FirstSystemSeg: SegmentNumber;
    BootFileId: Integer;
    SwappingAllowed: Boolean;
    SwapId: Integer;
    MemoryInBlocks: Integer;  { amount of memory on this machine }
 
 
private


{$R-}


imports MoveMem from MoveMem;
imports System from System;
imports Virtual from Virtual;
imports DiskIO from DiskIO;
imports Stream from Stream;
imports FileAccess from FileAccess;
imports AllocDisk from AllocDisk;

      

var OldStreamSegment: SegmentNumber;
    OldHeapSegment: SegmentNumber;

Function SegIdtoFileId(id : SegId) : Integer;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Convert a two word SegId into a one word fileId.
{
{ Parameters:
{       id is a two word segId.
{
{ Returns:
{       A one word FileId; it may be pos or neg or zero.
{
{-----------------------------------------------------------------------------}

  var
    fid : Integer;
    disk : integer;
  begin
    fid := AddrToField(id);
    disk := WhichDisk(id);
    case disk of
      0: SegIdtoFileId := fid;
      1: SegIdtoFileId := lor(fid,#174000);
      otherwise: SegIdtoFileId := 0
    end;
  end;
  

Function FileIdtoSegId(id : Integer) : SegId;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Convert a one word FileId into a two word SegId.
{
{ Parameters:
{       id is a one word FileId.
{
{ Returns:
{       a two word SegId.
{
{-----------------------------------------------------------------------------}

  var
    disk : integer;
  begin
    if land(id,#174000) = #174000 then disk := 1 else disk := 0;
    case disk of
      0: FileIdtoSegId := FieldToAddr(disk,id);
      1: FileIdtoSegId := FieldToAddr(disk,land(id,#003777));
      otherwise: FileIdtoSegId := DBLZERO
    end;
  end;

procedure InitMemory;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       InitMemory initializes the memory manager.  It is called once at
{       system initialization and may not be called again.  If the system
{       was booted from a floppy, the system segments are all marked as
{       UnSwappable.
{
{-----------------------------------------------------------------------------}

var Memory, Memory0: MMPointer;
    Save0, BootedMemoryInBlocks: Integer;
    S: SegmentNumber;
    E: MMEdge;
begin { InitMemory }
 SetDDS(201);
 SAT := MakePtr(SATSeg,0,pSAT);
 SIT := MakePtr(SITSeg,0,pSIT);
 SetDDS(203);
 InLineByte( #143 {LSSN} );
 StorExpr(StackSegment);
 SetDDS(204);
 with SIT^[0].BootBlock do
  begin
   SetDDS(205);
   SystemVersion := VN;
   SetDDS(206);
   MMFree := FF;
   SetDDS(207);
   FirstSystemSeg := FC;
   SetDDS(208);
   SysDisk := DK;
   SetDDS(209);
   SysBootChar := CH;
   SetDDS(210)
  end;
 SetDDS(211);
 MMHeap := 0;
 SetDDS(212);
 MMFirst := SATSeg;
 SetDDS(213);
 S := SATSeg;
 SetDDS(214);
 Memory.Segmen := NewSegmentNumber;
 SetDDS(215);
 with SAT^[Memory.Segmen] do
  begin
   NotResident := False;
   Moving := False;
   InUse := True;
   BaseLower := 0;
   BaseUpper := 0;
   Size := 0
  end;
 SetDDS(216);
 Memory0.Segmen := SATSeg;
 Memory0.Offset := 0;
 Memory.Offset := 0;
 Save0 := Memory0.P^;
 MemoryInBlocks := #400;
 SetDDS(217);
 { figure out how much physical memory there is via a dual address test. }
 { here we assume that physical address space is a power of two that is  }
 { at least as large as 2^17 words.   V2.19 use LNot(mem) so don't change }
 { old value at that location }
 repeat
  MemoryInBlocks := MemoryInBlocks + MemoryInBlocks;
  SAT^[Memory.Segmen].BaseUpper := Shift(MemoryInBlocks,-8);
  Memory0.P^ := LNot(Memory.P^);
 until Memory0.P^ = Memory.P^;

 SetDDS(218);
 Memory0.P^ := Save0;
 SetDDS(219);
 ReleaseSegmentNumber(Memory.Segmen);
 SetDDS(220);
 MakeEdge(E,IOSeg);
 SetDDS(221);

 { Calculate BootedMemoryInBlocks.  We know that IOSeg is the last segment }
 with SAT^[IOSeg] do
   BootedMemoryInBlocks := Shift(BaseUpper, 8) + BaseLower + Size + 1;

 { if not an even multiple of 1/4 meg then die with DDS at 221 }
 
 if BootedMemoryInBlocks mod BlocksInQuarterMeg <> 0 then 
     while true do;  
 
 { move the IOSeg to high memory.  here we assume that the difference    }
 { between MemoryInBlocks and BootedMemoryInBlocks is a multiple of 256. }
 SAT^[IOSeg].BaseUpper := SAT^[IOSeg].BaseUpper +
                          Shift(MemoryInBlocks - BootedMemoryInBlocks,-8);
 SetDDS(222);
 { adjust free memory }
 SAT^[E.T].Size := SAT^[E.T].Size +
                   MemoryInBlocks - BootedMemoryInBlocks;
 SetDDS(223);
 repeat S := SIT^[S].NextSeg;
   with SAT^[S] do
     if (Kind = DataSegment) and not Full then
       begin Memory.Segmen := S;
         Memory.Offset := 0;
         with Memory.m^.m[0] do
           begin N := 0; L := Shift(Size+1, 7) end
       end
 until S = SATSeg;
 SetDDS(224);
 with SAT^[ScreenSeg] do
  ScreenLast := Shift(BaseUpper,8) + BaseLower + Size;
 SetDDS(225);
 New(IOSeg,4,BlockHeader);
 SetDDS(226);
 New(IOSeg,4,Status);
 SetDDS(227);
 SwappingAllowed := False;
 SetDDS(228);
 S := SATSeg;
 if SysBootChar <= Ord('Z') then { booted from floppy }
  repeat S := SIT^[S].NextSeg;
   if SAT^[S].InUse and (SIT^[S].Mobility > UnSwappable) then
    SIT^[S].Mobility := UnSwappable
  until S = SATSeg;
 SetDDS(229);
 if MemoryInBlocks > #1000 then { more than 1/4 MByte }
  begin
   SetDDS(230);
   SetMaximum(SITSeg,16);
   SetDDS(231);
   ChangeSize(SITSeg,16);
   SetDDS(232);
   SetMaximum(SATSeg,4);
   SetDDS(233);
   ChangeSize(SATSeg,4);
   SetDDS(234);
   for S := MaxSegment + 1 to 511 do ReleaseSegmentNumber(S);
   SetDDS(235)
  end;
 SetDDS(236)
end { InitMemory };


procedure DataSeg( var S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DataSeg is used to
{          1) - Determine if a given segment number represents a data segment.
{          2) - Find the default heap segment (in the case of an input parameter
{             of zero).
{
{ Parameters:
{       S - Data segment number--zero means the default heap segment.
{
{ Errors:
{       UnusedSegment  - if S is not in use.
{       NotDataSegment -  if S is not a data segment.
{
{-----------------------------------------------------------------------------}

begin { DataSeg }
 if not SAT^[S].InUse then
  if S = 0 then
   if MMHeap = 0 then raise UnusedSegment(S)
   else S := MMHeap
  else raise UnusedSegment(S);
 if SAT^[S].Kind <> DataSegment then raise NotDataSegment(S)
end { DataSeg };

procedure CodeOrDataSeg( var S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CodeOrDataSeg is used to
{          1) - Determine if a given segment number represents a defined segment.
{          2) - Find the default heap segment (in the case of an input parameter
{             of zero).
{
{ Parameters:
{       S - Data segment number--zero means the default heap segment.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{
{-----------------------------------------------------------------------------}

begin { CodeOrDataSeg }
 if not SAT^[S].InUse then
  if S = 0 then
   if MMHeap = 0 then raise UnusedSegment(S)
   else S := MMHeap
  else raise UnusedSegment(S)
end { CodeOrDataSeg };

procedure ChangeSize( S: SegmentNumber; Fsize: MMExtSize );
{-----------------------------------------------------------------------------
{
{ Abstract:
{        ChangeSize is used to change the size of an existing data segment.
{
{ Parameters:
{        S - Number of the segment whose size is to be changed.
{        Fsize - New size of the segment.
{
{ Errors:
{        UnusedSegment  - if S is not in use.
{        BadSize -  if Fsize is greater than the maximum size of S or less
{                 than one.
{        FullMemory  - if there is not enough physical memory to increase
{                    the size of S.
{        CantMoveSegment  - if the segment must be moved, but it is not
{                         movable or its IOCount is not zero.
{
{-----------------------------------------------------------------------------}

var Lsize: MMIntSize;
    T: SegmentNumber;
    Base: Integer;
    E: MMEdge;
    OldSize: Integer;
    ENext, HoleNext: SegmentNumber;
    SId: SegId;
    I: Integer;
begin { ChangeSize }
 DataSeg(S);
 with SAT^[S], SIT^[S] do
  begin
   if (Fsize < 1) or (Fsize > MMMaxExtSize) then raise BadSize(S,Fsize);
   Lsize := Fsize - 1;
   if Lsize > Maximum then raise BadSize(S,Fsize);
   if not BootLoaded then
    if SwapInfo.DiskId <> 0 then
     begin
      if Lsize > Size then
       begin
        SId := FileIdToSegId(SwapInfo.DiskId);
        for I := Size+1 to Lsize do
         WriteSpiceSegment(SId, I, 1, MakePtr(StackSegment, 0, ptrDiskBuffer));
        FlushAll
       end;
      if NotResident then Size := Lsize
     end;
   if Lsize > Size then
    if not SAT^[NextSeg].InUse then
     if Size + SAT^[NextSeg].Size + 1 >= Lsize then
      begin T := NextSeg;
       Size := Size + SAT^[T].Size + 1;
       NextSeg := SIT^[T].NextSeg;
       ReleaseSegmentNumber(T) 
      end;
   if Lsize > Size then
    begin
     if Mobility <= UnMovable then raise CantMoveSegment(S);
     if IOCount > 0 then { WaitIOComplete } raise CantMoveSegment(S);
  {
     MakeEdge(E,S);
     SwapOut(E);
     Keep1 := S;
     Keep2 := CurrentSegment;
     Keep3 := StackSegment;
     Keep4 := ReturnSegment;
     FindHole(Lsize, RefCount <= 1);
     if MMState = MMNotFound then
      begin FindHole(Size, RefCount <= 1);{ we are guaranteed to find this hole
       SwapIn(MMHole,S,MMLowPos);
       raise FullMemory
      end;
     Size := Lsize;
     SwapIn(MMHole,S,MMLowPos)
  }
     Keep1 := S;
     Keep2 := CurrentSegment;
     Keep3 := StackSegment;
     Keep4 := ReturnSegment;
     FindHole(Lsize, RefCount <= 1);
     MakeEdge(E,S);
     if MMState = MMNotFound then raise FullMemory
     else
      with SAT^[E.H], SIT^[E.H] do
       begin
        Moving := true;
        SAT^[MMHole.H].Moving := true;
        SAT^[MMHole.H].NotResident := false;
        CopySegment(E.H,MMHole.H,Shift(BaseUpper,8) + BaseLower);
        OldSize := Size;
        Size := SAT^[MMHole.H].Size;
        SAT^[MMHole.H].Size := OldSize;
        ENext := NextSeg;
        HoleNext := SIT^[MMHole.H].NextSeg;
        if E.H = MMHole.T then SIT^[MMHole.H].NextSeg := E.H
        else
         begin SIT^[MMHole.T].NextSeg := E.H;
          SIT^[MMHole.H].NextSeg := ENext
         end;
        if MMHole.H = E.T then SIT^[E.H].NextSeg := MMHole.H
        else
         begin SIT^[E.T].NextSeg := MMHole.H;
          SIT^[E.H].NextSeg := HoleNext
         end;
        SAT^[MMHole.H].NotResident := true;
        SAT^[MMHole.H].Moving := false;
        Moving := false;
        DeleteSegment(MMHole.H)
       end
    end;
   if Lsize < Size then
    begin T := NewSegmentNumber;
     SAT^[T].Size := Size - Lsize - 1;
     Base := Shift(BaseUpper,8) + BaseLower + Lsize + 1;
     SAT^[T].BaseUpper := Shift(Base,-8);
     SAT^[T].BaseLower := LAnd(Base,#377);
     Size := Lsize;
     SIT^[T].NextSeg := NextSeg;
     NextSeg := T;
     DeleteSegment(T)
    end
  end;
 if not SAT^[S].NotResident then
  begin
   MMFirst := SIT^[S].NextSeg;
   StartIO(SetStkLimit)         { in case we changed the size of the stack }
  end
end { ChangeSize };

procedure CreateSegment( var S: SegmentNumber;
                         Fsize, Fincrement, Fmaximum: MMExtSize );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CreateSegment is used to create a new data segment.  The size,
{       increment and maximum may be up to 4096 blocks but clearly the size
{       will be limited by the available memory.  In addition, IT IS NOT
{       POSSIBLE TO USE SEGMENTS BIGGER THAN 256 BLOCKS FOR NEW, DISPOSE,
{       OR MAKEPTR.  Therefore, the only way to address segments bigger than
{       256 blocks is using RasterOp.  
{
{ Parameters:
{       S - Set to the number of the new segment.
{       Fsize - Desired size of the new segment in blocks.
{       Fincrement - Increment size of the new segment in blocks.
{       Fmaximum - Maximum size of the new segment.
{
{ Errors:
{       BadSize  - if Fsize is greater than Fmaximum or less than one.
{       BadIncrement  - if Fincrement is greater than MMMaxExtSize or less
{                     than one.
{       BadMaximum  - if Fmaximum is greater than MMMaxExtSize or less than one.
{       FullMemory  - if there is not enough physical memory to create the
{                   segment.
{
{-----------------------------------------------------------------------------}

var memory: MMPointer;
    T, U: SegmentNumber;
    Base: Integer;
begin { CreateSegment }
 if (Fsize < 1) or (Fsize > MMMaxExtSize) then
  raise BadSize(S,Fsize);
 if (Fincrement < 1) or (Fincrement > MMMaxExtSize) then
  raise BadIncrement(S,Fincrement);
 if (Fmaximum < 1) or (Fmaximum > MMMaxExtSize) then
  raise BadMaximum(S,Fmaximum);
 if Fsize > Fmaximum then raise BadSize(S,Fsize);
 Keep1 := CurrentSegment;
 Keep2 := StackSegment;
 Keep3 := StackSegment;
 Keep4 := ReturnSegment;
 FindHole(Fsize - 1, True);
 if MMState = MMFound then
  begin
   T := MMHole.H;
   MMFirst := MMHole.H;
   with SAT^[T], SIT^[T] do
    begin
     if Size >= Fsize then   { make new unused segment to sit on extra memory }
      begin U := NewSegmentNumber;
       SAT^[U].Size := Size - Fsize;
       Base := Shift(BaseUpper,8) + BaseLower + Fsize;
       SAT^[U].BaseUpper := Shift(Base,-8);
       SAT^[U].BaseLower := LAnd(Base,#377);
       Size := Fsize - 1;
       SIT^[U].NextSeg := NextSeg;
       NextSeg := U
      end;
     memory.Segmen := T;
     memory.Offset := 0;
     BootLoaded := False;
     SwapInfo.DiskId := 0;
     SwapInfo.DiskLowerAddress := 0;
     SwapInfo.DiskUpperAddress := 0;
     Kind := DataSegment;
     Full := false;
     Heap := false;
     RecentlyUsed := false;
     Mobility := Swappable;       {V2.8}
     Moving := false;
     Increment := Fincrement - 1;
     Maximum := Fmaximum - 1;
     RefCount := 1;
     NotResident := false;
     InUse := true;
     Freelist := 0;
     with memory.m^.m[0] do
      begin N := 0; L := Shift(Fsize, 7) end;
     S := T
    end
  end
 else raise FullMemory
end { CreateSegment };

procedure IncOneRef( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       IncOneRef increments the number of references to a segment.  A
{       non-zero reference count prevents a segment from being destroyed.
{       A reference count greater than one indicates a system segment.
{
{ Parameters:
{       S - Number of the segment.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{
{-----------------------------------------------------------------------------}

begin { IncOneRef }
 CodeOrDataSeg(S);
 with SIT^[S] do
  if RefCount < MMMaxCount then RefCount := RefCount + 1
end { IncOneRef };

procedure IncRefCount( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       IncRefCount increments the number of references to a segment.  A
{       non-zero reference count prevents a segment from being destroyed.
{       A reference count greater than one indicates a system segment.
{       If S is a heap, all segments in the heap are incremented.
{
{ Parameters:
{       S - Number of the segment.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{
{-----------------------------------------------------------------------------}

var FirstS: SegmentNumber;
begin { IncRefCount }
 with SAT^[S] do
  if (Kind = DataSegment) and Heap then
   begin
    FirstS := S;
    repeat
     S := SIT^[S].HeapNext;
     IncOneRef(S)
    until S = FirstS
   end
  else IncOneRef(S)
end { IncRefCount };

procedure SetMobility( S: SegmentNumber; M: SegmentMobility );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetMobility sets the Mobility of a segment.  The mobility may be set to
{       one of the following values:
{          Swappable      - segment is a candidate for swapping or moving.
{          LessSwappable  - segment is a candidate for swapping or moving, but
{                           the memory manager will be more reluctant to swap.
{          UnSwappable    - segment may not be swapped, but may be moved.
{          UnMovable      - segment may not be swapped or moved.
{       The RecentlyUsed bit of the segment is cleared also.  Thus to make a
{       segment a candidate for swapping, set its mobility to Swappable (even
{       if it already swappable).
{
{ Parameters:
{       S - Segment number.
{       M - Mobility.
{
{ Errors:
{       UnusedSegment  - if S is not in use.
{       CantMoveSegment  - if the segment is changing from Swappable to UnMovable
{                        an attempt is made to move the segment to the high end
{                        of memory.  If it has a non-zero IO count this 
{                        error is issued.
{       FullMemory  - if the segment is changing from Swappable to UnSwappable,
{                   it is swapped out, and there isn't enough memory to swap
{                   it in.
{
{-----------------------------------------------------------------------------}

var E: MMEdge;
begin { SetMobility }
 CodeOrDataSeg(S);
 with SAT^[S], SIT^[S] do
  begin
   if (Mobility > UnSwappable) and (M = UnMovable) and SwappingAllowed then
    begin { move segment toward high end of memory }
     if IOCount <> 0 then { WaitIOComplete } raise CantMoveSegment(S);
     if not NotResident then { swap it out first }
      begin
       MakeEdge(E,S);
       SwapOut(E)
      end;
     Keep1 := S;
     Keep2 := CurrentSegment;
     Keep3 := StackSegment;
     Keep4 := ReturnSegment;
     FindHole(Size, RefCount <= 1);   { make enough space }
     Compact;                         { try to get hole at end of memory }
     FindHole(Size, RefCount <= 1);   { get the best hole }
     SwapIn(MMHole,S,MMHighPos)
    end
   else
    if (M <= UnSwappable) and NotResident then { must swap it in }
     begin
      FindHole(Size, RefCount <= 1);
      if M = UnMovable then SwapIn(MMHole,S,MMHighPos)
      else SwapIn(MMHole,S,MMLowPos)
     end;
   Mobility := M;
   RecentlyUsed := False
  end
end { SetMobility };

procedure DecOneRef( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DecOneRef decrements the reference count of a segment by one.  If
{       reference and IO counts both become zero:
{         - if the segment is a data segment, it is destroyed.
{         - if the segment is a code segment, it is destroyed only if it is in
{           the screen or is non-resident.
{
{ Parameters:
{       S - Number of the segment.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{
{-----------------------------------------------------------------------------}

begin { DecOneRef }
 CodeOrDataSeg(S);
 with SAT^[S], SIT^[S] do
  if (RefCount > 0) and (RefCount < MMMaxCount)then
   begin RefCount := RefCount - 1;
    if RefCount = 0 then
     if IOCount = 0 then
      begin
       if NotResident or (Kind = DataSegment) or
          (Shift(BaseUpper,8) + BaseLower <= ScreenLast) then
        DeleteSegment(S)
      end
     else
      if (Kind = DataSegment) and Heap then
       HeapNext := S
   end
end { DecOneRef };

procedure DecRefCount( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DecRefCount decrements the reference count of a segment by one.  If
{       reference and IO counts both become zero:
{         - if the segment is a data segment, it is destroyed.
{         - if the segment is a code segment, it is destroyed only if it is in
{           the screen or is non-resident.
{       If S is a heap, all segments in the heap are decremented.
{
{ Parameters:
{       S - Number of the segment.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{
{-----------------------------------------------------------------------------}

var FirstS, NextS: SegmentNumber;
begin { DecRefCount }
 with SAT^[S] do
  if (Kind = DataSegment) and Heap then
   begin
    FirstS := S;                   { save start, destroy this last }
    NextS := SIT^[S].HeapNext;     { We destroy the second segment first }
    repeat
     S := NextS;
     NextS := SIT^[S].HeapNext;    { remember the next before destroying S }
     DecOneRef(S);
    until S = FirstS
   end
  else DecOneRef(S)
end { DecRefCount };

procedure SetIncrement( S: SegmentNumber; V: MMExtSize );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetIncrement changes the increment size of a data segment.
{
{ Parameters:
{       S - Number of the segment.
{       V - New increment size.
{
{ Errors:
{       UnusedSegment  - if S is not in use.
{       NotDataSegment -  if S is not a data segment.
{       BadIncrement  - if V is greater than MMMaxExtSize or less than one.
{
{-----------------------------------------------------------------------------}

begin { SetIncrement }
 DataSeg(S);
 if (V < 1) or (V > MMMaxExtSize) then raise BadIncrement(S,V);
 if not SAT^[S].Heap then SIT^[S].Increment := V - 1
end { SetIncrement };

procedure SetMaximum( S: SegmentNumber; V: MMExtSize );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetMaximum changes the maximum size of a data segment.
{
{ Parameters:
{       S - Number of the segment.
{       V - New maximum size.
{
{ Errors:
{       UnusedSegment  - if S is not in use.
{       NotDataSegment -  if S is not a data segment.
{       BadMaximum  - if V is greater than MMMaxExtSize or less than one.
{
{-----------------------------------------------------------------------------}

begin { SetMaximum }
 DataSeg(S);
 if (V < 1) or (V > MMMaxExtSize) then raise BadMaximum(S,V);
 if not SAT^[S].Heap then SIT^[S].Maximum := V - 1
end { SetMaximum };

procedure SetHeap( S: SegmentNumber; V: boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetHeap changes the "Heap" attribute of a segment.  This should not
{       normally be changed by anyone other than Dynamic.
{
{ Parameters:
{       S - Number of the segment.
{       V - New value of the "Heap" attribute.
{
{ Errors:
{       UnusedSegment  - if S is not in use.
{
{-----------------------------------------------------------------------------}

begin { SetHeap }
 CodeOrDataSeg(S);
 SAT^[S].Heap := V
end { SetHeap };

procedure SetKind( S: SegmentNumber; V: SegmentKind );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetKind changes the kind (code or data) of a segment.
{
{ Parameters:
{       S - Number of the segment.
{       V - New kind of the segment.
{
{ Errors:
{       UnusedSegment  - if S is not in use.
{
{-----------------------------------------------------------------------------}

begin { SetKind }
 CodeOrDataSeg(S);
 SAT^[S].Kind := V;
 with SIT^[S] do
  if V = CodeSegment then
   with Update do
    begin
     Year := 0;
     Month := 1;
     Day := 1;
     Hour := 0;
     Minute := 0;
     Second := 0
    end
  else
   begin
    SAT^[S].Heap := False;
    Maximum := SAT^[S].Size;
    Increment := 0;
    FreeList := 0
   end
end { SetKind };

procedure MarkMemory;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       MarkMemory marks all currently in use segments as system segments
{       usually before loading a user program) by incrementing their
{       reference counts.
{
{-----------------------------------------------------------------------------}

var S: SegmentNumber;
begin { MarkMemory }
 S := SATSeg;
 repeat S := SIT^[S].NextSeg;
  if SAT^[S].InUse and (SIT^[S].RefCount > 0) then IncOneRef(S)
 until S = SATSeg;
 OldStreamSegment := StreamSegment;
 OldHeapSegment := MMHeap;
 StreamSegment := 0;
 MMHeap := 0
end { MarkMemory };

procedure CleanUpMemory;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CleanUpMemory destroys all user segments (usually at the end of a
{       program execution) by deecrementing the reference count of all
{       segments.
{
{-----------------------------------------------------------------------------}

var S, T: SegmentNumber;
begin { CleanUpMemory }
 MMHeap := OldHeapSegment;
 StreamSegment := OldStreamSegment;
 T := SATSeg;
 repeat
  S := T;
  repeat T := SIT^[T].NextSeg until SAT^[T].InUse;
  with SIT^[S] do
   if SAT^[S].InUse then DecOneRef(S)
 until T = SATSeg
end { CleanUpMemory };

procedure EnableSwapping( Where: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       EnableSwapping turns the swapping system on, determines where swap
{       files should be created, and locates the boot file.
{
{ Parameters:
{       Where - FileId of some file in the partition to be used for swap files.
{
{-----------------------------------------------------------------------------}

var Ptr: ptrDiskBuffer;
begin { EnableSwapping }
 SwapId := Where;
 SwapSId := FileIdToSegId(SwapId);
 if SysBootChar <= Ord('Z') then { floppy disk, don't bother }
  begin BootSerialNum[0] := 0;
   BootSerialNum[1] := 0;
   BootSegId := DblZero;
   BootFileId := 0
  end
 else { hard disk }
  begin
   if not DiskTable[0].InUse then
    begin Write('*** Mounting hard disk...');
     DeviceMount(0);
     Writeln('done.')
    end;
   Ptr := ReadDisk(DiskTable[0].InfoBlk);
   Kludge.A := Ptr^.BootTable[SysBootChar - Ord('a')];
   BootSerialNum := Kludge.D;
   BootSegId := PhysAddrToLogAddr(0,Kludge.A);
   BootFileId := SegIdToFileId(BootSegId)
  end;
 SwappingAllowed := True
end { EnableSwapping };

procedure DisableSwapping;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DisableSwapping attempts to swap in all segments which are swapped
{       out and then turns the swapping system off.  If there is not enough
{       physical memory to swap all segments in, swapping is not disabled.
{
{ Errors:
{       FullMemory  - if there isn't enough memory to swap all segments in.
{
{-----------------------------------------------------------------------------}

var S: SegmentNumber;
    Id: Integer;
begin { DisableSwapping }
 SwappingAllowed := False;   { so that we won't swap anything out }
 S := SATSeg;
 repeat S := SIT^[S].NextSeg;
  with SAT^[S] do
   begin
    if InUse and NotResident then
     begin FindHole(Size,SIT^[S].RefCount <= 1);
      if MMState = MMNotFound then
       begin SwappingAllowed := True;
        raise FullMemory
       end
      else
        SwapIn(MMHole,S,MMLowPos);
     end;    { if inuse ... }

    if InUse And (Kind = DataSegment) And (NOT SIT^[S].BootLoaded) then
     begin
      Id := SIT^[S].SwapInfo.DiskId;
      SIT^[S].SwapInfo.DiskId := 0;
      If ID <> 0 Then DestroySpiceSegment(FileIdToSegId(Id))
     end;  {if inuse ..}
   end;    { With SAT^[S] }
 until S = SATSeg
end { DisableSwapping };

procedure FindCodeSegment( var S: SegmentNumber; Hint: SegHint );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       FindCodeSegment searches for a code segment in the segment table
{       which has a certain SegHint.  If such a segment is found, its RefCount
{       is incremented and the segment number is returned.  Otherwise, a
{       zero segment number is returned.
{
{       Segments which
{          1) Have a DiskId equal to Hint.FId,
{          2) Have an Update date/time not equal to Hint.Update, and
{          3) Have a RefCount of zero
{       are deleted.  This is done because such a segments reference code
{       files which have been overwritten and are no longer valid.  Such
{       segments will not get the memory manager into trouble, but they
{       will never be used again, and it is just as well to get rid of them.
{
{       **** Hint must be a valid hint.  That is, the file specified
{       **** by Hint.FId must have a FileWriteDate equal to Hint.Update.
{
{ Parameters:
{       S - Return parameter set to zero or the number of the code segment.
{       Hint - Desired SegHint.
{
{-----------------------------------------------------------------------------}

var T: SegmentNumber;
    Found: Boolean;
    ActualHint: SegHint;
begin { FindCodeSegment }
 S := 0;
 T := SATSeg;
 Found := False;
 repeat T := SIT^[T].NextSeg;
  with SAT^[T], SIT^[T] do
   if InUse and (Kind = CodeSegment) and not BootLoaded then
    if SwapInfo.DiskId = Hint.FId then
     begin
      ActualHint.FId := SwapInfo.DiskId;
      ActualHint.Update := Update;
      Found := (ActualHint.Word1 = Hint.Word1) and
               (ActualHint.Word2 = Hint.Word2) and
               (ActualHint.Word3 = Hint.Word3);
      if not Found then { invalid segment }
       if RefCount = 0 then { delete it }
         DeleteSegment(T)
     end
 until Found or (T = SATSeg);
 if Found then
  begin
   S := T;
   IncRefCount(T)
  end
end { FindCodeSegment };

function CurrentSegment: SegmentNumber;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CurrentSegment finds the segment number of its caller.
{
{ Result:
{       CurrentSegment = Segment number of the caller of CurrentSegment.
{
{-----------------------------------------------------------------------------}

const {$Include Perq.QCodes.Dfs}
      {$Include ACB.Dfs}
var Stack: pMMArray;
    AP, MMCS: Integer;
begin { CurrentSegment }
 Stack := MakePtr(StackSegment,0,pMMArray);
 InLineByte(LDAP);
 StorExpr(AP);
 CurrentSegment := Stack^.w[AP + ACBRS]  { segment number of the caller }
end { CurrentSegment }.
