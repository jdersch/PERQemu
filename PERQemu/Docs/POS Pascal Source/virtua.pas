{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Virtual;
{-----------------------------------------------------------------------------
{
{       Virtual - Perq virtual memory manager.
{       J. P. Strait        1 Jan 80.
{       Copyright (C) Three Rivers Computer Corporation, 1980, 1982, 1983.
{
{ Abstract:
{       Virtual is the Perq virtual memory manager.  It supervises the
{       segment tables and exports procedures for swapping memory segments.
{       Virtual is the portion of the Perq memory manager which must remain
{       memory resident at all times.
{       Perq physical memory is segmented into separately swappable items
{       (called segments) which may contain either code or data.
{
{ Design:
{       See the Q-Code reference manual.
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{ Change Log:
{ 31 Mar 83  V3.2   Sandeep Johar
        Set and test 5 bits of a disk address to determine
        if it is a floppy. The result is that 1MB is kept for
        floppy and 31mb are availble for the disk.

{ 10 Dec 82  V3.1  Chuck Beckett.
{    Fix 14 Char Compiler errors.
{ }

{ 17 Nov 82  V2.10  J Strait.
{ 1. Change name of Sharable bit to Heap in SAT.
{ 2. Document warning about IncIOCount and DecIOCount.
{ }

{   3 May 82  V2.9  David Golub
{    Unscramble the segment pointers in Compact when it compacts out a
{    segment.
{    Make SwapSegmentsIn really keep all four segments in.
{ }

{  13 Jan 82  V2.8  Brad Myers (found by JPS)
{    Handler for XStackOverflow in SwapIn and SwapOut
{ }

{ 10 Dec 81  V2.7  WJHansen
{    change to use long arithmetic for xxTime;  remove Arith
{ }

{ 25 Oct 81  V2.6  JPS
{ Changes to agree with V2.11 Memory.
{ }

{ 24 Sep 81 V2.5  WJH
{ When delete a segment, make it swappable.
{ }

{ 3 Jul 81  V2.4  JPS
{ Fix bug where MMFirst was getting trashed by SwapIn.
{ }

{ 28 May 81  V2.3  BAM
{ New light.
{ }

{ 26 May 81  V2.2  JPS
{ Show swapping with a "light".
{ }

{ 21 May 81  V2.1  JPS
{ Fix various serious bugs in SwapIn procedure.
{ Delete several unused variables in various procedures.
{ }

{ 12 May 81  V2.0  JPS
{ 1) Split Memory into two modules: Memory (user callable routines, swappable)
{    and Virtual (system callable routines, unswappable).
{ 2) Move FileIdToSegId and SegIdToFileId into Virtual.
{ 3) Use exceptions rather than MemoryError.
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
{-----------------------------------------------------------------------------}

exports


const VirtualVersion = '3.2';


imports Memory from Memory;
imports IO_Unit from IO_Unit;
imports DiskIO from DiskIO;

          
 function  ReturnSegment: SegmentNumber;
 procedure ReleaseSegmentNumber( Seg: SegmentNumber );
 function  NewSegmentNumber: SegmentNumber;
 procedure MakeEdge( var E: MMEdge; S: SegmentNumber );
 procedure DeleteSegment( var S: SegmentNumber );
 procedure SwapOut( var E: MMEdge );
 procedure SwapIn( E: MMEdge; S: SegmentNumber; P: MMPosition );
 procedure Compact;
 procedure KeepSegments;
 procedure FindHole( Fsize: MMIntSize; ForUserSegment: Boolean );
 procedure IncIOCount( S: SegmentNumber );
 procedure DecIOCount( S: SegmentNumber );
 procedure SwapSegmentsIn( S1, S2, S3, S4: SegmentNumber );
 


var ScreenLast: Integer;
    Keep1, Keep2, Keep3, Keep4: SegmentNumber;
    Kludge: record case Integer of
             1: (A: DiskAddr);
             2: (D: Double)
             end;
    BlockHeader: IOHeadPtr;
    BlockAddress: Double;
    BlockSId: SegId;
    Status: IOStatPtr;
    BootSerialNum: Double;
    BootSegId: SegId;
    SwapSId: SegId;

 
 
private


imports Lights from Lights;

const PrintSwapInfo = False;
      ShowSwap = LightUsed;


{$R-}

imports MoveMem from MoveMem;
imports System from System;
imports IO_Others from IO_Others;
imports IOErrors from IOErrors;
imports FileAccess from FileAccess;
imports ReadDisk from ReadDisk;
imports AllocDisk from AllocDisk;
imports Except from Except;

{$ifc ShowSwap then}
imports Screen from Screen;
{$endc}


Function SegIdtoFileId(id : SegId) : Integer;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Convert a two word SegId into a one word FileId.
{
{ Parameters:
{       id is a two word SegId.
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

function ReturnSegment: SegmentNumber;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReturnSegment finds the segment number of the caller of the procedure
{       which called ReturnSegment by searching the call stack.
{
{ Result:
{       ReturnSegment = Segment number of the caller.
{
{ Design:
{       This routine depends on the Perq running a single process operating
{       system where the caller is in the same process as the memory manager.
{
{-----------------------------------------------------------------------------}

const {$Include Perq.QCodes.Dfs}
      {$Include ACB.Dfs}
var Stack: pMMArray;
    AP, MMCS: Integer;
begin { ReturnSegment }
 Stack := MakePtr(StackSegment,0,pMMArray);
 InLineByte(LDAP);
 StorExpr(AP);
 MMCS := Stack^.w[AP + ACBRS];  { segment number of the memory manager }
 repeat AP := Stack^.w[AP + ACBDL]
 until Stack^.w[AP + ACBRS] <> MMCS { caller is not the memory manager };
 ReturnSegment := Stack^.w[AP + ACBRS]
end { ReturnSegment };

procedure ReleaseSegmentNumber( Seg: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReleaseSegmentNumber releases a segment number to the list of segment
{       numbers which are not in use.
{
{ Parameters:
{       Seg - Segment number to return to the segment number free list.
{
{-----------------------------------------------------------------------------}

begin { ReleaseSegmentNumber }
 if MMFirst = Seg then MMFirst := SATSeg;
 with SAT^[Seg], SIT^[Seg] do
  begin
   NotResident := true;
   InUse := false;
   Kind := DataSegment;
   BootLoaded := false;
   SwapInfo.DiskId := 0;
   SwapInfo.DiskLowerAddress := 0;
   SwapInfo.DiskUpperAddress := 0;
   NextSeg := MMFree
  end;
 MMFree := Seg
end { ReleaseSegmentNumber };

function NewSegmentNumber: SegmentNumber;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       NewSegmentNumber allocates the next unused segment number.
{
{ Errors:
{       NoFreeSegments  if there are no unused segment numbers.
{
{-----------------------------------------------------------------------------}

begin { NewSegmentNumber }
 if MMFree = 0 then { EnlargeTable } raise NoFreeSegments;
 NewSegmentNumber := MMFree;
 with SAT^[MMFree], SIT^[MMFree] do
  begin NotResident := false;
   Moving := false;
   RecentlyUsed := false;
   Heap := false;
   Kind := DataSegment;
   InUse := false;
   Freelist := 0;
   RefCount := 0;
   IOCount := 0;
   Mobility := Swappable;
   BootLoaded := false;
   SwapInfo.DiskId := 0;
   SwapInfo.DiskLowerAddress := 0;
   SwapInfo.DiskUpperAddress := 0;
   MMFree := NextSeg
  end
end { NewSegmentNumber };

procedure MakeEdge( var E: MMEdge; S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       MakeEdge makes an MMEdge record which the head field set to a certain
{       segment number and the tail field set to the previous segment number
{       (in physical address order).
{
{ Parameters:
{       E - MMEdge record to build.
{       S - Segment to put in the head field.
{
{ Errors:
{       EdgeFailure  if MakeEdge can't find the previous segment number.
{
{-----------------------------------------------------------------------------}

begin { MakeEdge }
 E.H := S;
 E.T := SATSeg;
 while SIT^[E.T].NextSeg <> E.H do
  begin E.T := SIT^[E.T].NextSeg;
   if E.T = SATSeg then raise EdgeFailure
  end
end { MakeEdge };

procedure DeleteSegment( var S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DeleteSegment returns a segment to the free memory list.  This is
{       done (for example) when the segment's reference and IO counts
{       both reach zero.
{
{ Parameters:
{       S - Number of the segment to be destroyed.  To facilitate segment table
{           scanning loops that contain calls to DeleteSegment:
{           * If S was resident, it is changed to be the number of the segment
{             which represents the free memory.  This may not be the same as
{             the original value if the original segment is coalesced with an
{             adjacent free segment.
{           * If S was not resident, it is changed to be the number of the
{             segment which preceded it in the segment table.
{           * MMFirst is set to have the same value as S on exit.
{
{-----------------------------------------------------------------------------}

var E: MMEdge;

 procedure Coalesce;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Coalesce coalesces two adjacent segments given by E.H and E.T.  E.H
{       and E.T are both set to the number of the combined free segment
{       after releasing the segment number in E.H.
{
{-----------------------------------------------------------------------------}

 begin { Coalesce }
  with SAT^[E.T], SIT^[E.T] do
   if Shift(BaseUpper,8) + BaseLower + Size + 1 =
      Shift(SAT^[E.H].BaseUpper,8) + SAT^[E.H].BaseLower then
    begin Size := Size + SAT^[E.H].Size + 1;
     NextSeg := SIT^[E.H].NextSeg;
     ReleaseSegmentNumber(E.H);
     E.H := E.T
    end
 end { Coalesce };

begin { DeleteSegment }
 with SAT^[S] do
  begin
   if InUse and (SIT^[S].SwapInfo.DiskId <> 0) then
    if Kind = DataSegment then
     DestroySpiceSegment(FileIdToSegId(SIT^[S].SwapInfo.DiskId));
   MakeEdge(E,S);
   if InUse and NotResident then
    begin
     SIT^[E.T].NextSeg := SIT^[S].NextSeg;
     ReleaseSegmentNumber(S);
     S := E.T;
     MMFirst := E.T
    end
   else
    begin
     NotResident := true;
     InUse := false;
     SIT^[S].Mobility := Swappable;                 {V2.5}
     if not SAT^[E.T].InUse then Coalesce;
     E.T := E.H;
     E.H := SIT^[E.T].NextSeg;
     if not SAT^[E.H].InUse then Coalesce;
     S := E.T;
     MMFirst := E.T
    end
  end
end { DeleteSegment };

procedure SetDiskAddress( Seg: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetDiskAddress sets up BlockHeader, BlockAddress, and BlockSId for
{       the swap file associated with a given segment.
{
{ Parameters:
{       Seg - Number of the segment.
{
{ Result:
{       BlockHeader and BlockAddress set in preparation for a swap in or out.
{
{-----------------------------------------------------------------------------}

begin { SetDiskAddress }
 with SIT^[Seg] do
  with BlockHeader^ do
    begin
      if BootLoaded then
       begin
        BlockSId := BootSegId;
        SerialNum := BootSerialNum;
        LogBlock := SwapInfo.BootLogBlock;
        BlockAddress[0] := SwapInfo.BootLowerAddress;
        BlockAddress[1] := SwapInfo.BootUpperAddress
       end
      else
       begin
        BlockSId := FileIdToSegId(SwapInfo.DiskId);
        Kludge.A := LogAddrToPhysAddr(BlockSId);
        SerialNum := Kludge.D;
        if SAT^[Seg].Kind = CodeSegment then LogBlock := 1
        else LogBlock := 0;
        BlockAddress[0] := SwapInfo.DiskLowerAddress;
        BlockAddress[1] := SwapInfo.DiskUpperAddress
       end
    end
end { SetDiskAddress };

procedure DoDiskIO( Command: IOCommands; Seg: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DoDiskIO performs an IORead or IOWrite to transfer a segment between
{       memory and disk.
{
{ Parameters:
{       Command - IORead or IOWrite.
{       Seg - Number of the segment to transfer.
{
{ Errors:
{       DiskFailure  if unrecoverable disk error during swapping.
{
{-----------------------------------------------------------------------------}

var I, J: Integer;
    InitialHeader: IOHeader;
    Buffer: IOBufPtr;
    SId: SegId;
    BlocksLeft, BlockCount: Integer;
    Offset: Integer;
begin { DoDiskIO }
  with SAT^[Seg], SIT^[Seg] do
    begin
      BlocksLeft := Size + 1;
      Offset := 0;
      repeat
        InitialHeader := BlockHeader^;
        Buffer := MakePtr(Seg,Offset,IOBufPtr);
        if BlocksLeft < 64 then BlockCount := BlocksLeft
        else BlockCount := 63;
        I := 0;
        repeat I := I + 1;
          J := 0;
          repeat J := J + 1;
            UnitIO(HardDisk, Buffer, Command,
                   Shift(BlockCount,9), BlockAddress, BlockHeader, Status);
            if Status^.SoftStatus = IOEIOC then
              begin
                BlocksLeft := BlocksLeft - BlockCount;
                Offset := Offset + Shift(BlockCount,8);
                BlockAddress := BlockHeader^.NextAdr;
                BlockHeader^.LogBlock := BlockHeader^.LogBlock + 1
              end
            else
              with Status^ do
                begin
                  if (SoftStatus < IOEFirstError) or
                     (SoftStatus > IOELastError) then
                    SoftStatus := IOEUDE;
                  ErrorCnt[SoftStatus] := ErrorCnt[SoftStatus] + 1;
                  BlockHeader^ := InitialHeader
                end
          until (Status^.SoftStatus = IOEIOC) or
                (J = 5) or (Status^.SoftStatus = IOEADR);
          if Status^.SoftStatus <> IOEIOC then
            if I < 5 then DiskReset
        until (Status^.SoftStatus = IOEIOC) or (I = 5);
        if Status^.SoftStatus <> IOEIOC then
          begin
            Kludge.D := BlockAddress;
            if Command = IORead then
              raise DiskFailure('Swapping segment in', DskRead,
                                PhysAddrToLogAddr(0,Kludge.A),
                                Status^.SoftStatus)
            else
              raise DiskFailure('Swapping segment out', DskWrite,
                                PhysAddrToLogAddr(0,Kludge.A),
                                Status^.SoftStatus)
          end
      until BlocksLeft = 0
    end
end { DoDiskIO };

procedure SwapOut( var E: MMEdge );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapOut swaps a data segment out to disk.
{
{ Parameters:
{       E - An edge where the head is the segment to be swapped and the tail
{           is the previous segment.
{
{ Result:
{       E.T and E.H both are set to the number of the new free segment.
{
{ Errors:
{       PartNotMounted  if the swapping partition is not mounted.
{       SwapError  if attempt to swap segment out while swapping is disabled.
{
{-----------------------------------------------------------------------------}

label 1;
var T: SegmentNumber;
    TAllocated: Boolean;
    FirstWrite: Boolean;
    FId: Integer;
    SId: SegId;
    I, Partition: Integer;
    Ptr: ptrHeader;
    Head: MMEdge;
    {$ifc SysTiming then}
    StartTime, EndTime: record case Integer of
                            1: (D: Double);
                            2: (DW: long);
                            3: (FSB: FSBit32)
                            end;
    SaveIOTime: long;
    {$endc}
    {$ifc ShowSwap then}
    LightsOn: Boolean;
    {$endc}

 handler All( A, B, C, D: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       All exceptions are caught during SwapOut so that the segment table
{       can be cleaned up.
{
{ Errors:
{       The exception is re-raised.
{
{ Design:
{       If someone returns from the exception, we retry the SwapOut.
{
{-----------------------------------------------------------------------------}

 begin { All }
   SAT^[E.H].Moving := False;
   if TAllocated then ReleaseSegmentNumber(T);
   if FirstWrite then DestroySpiceSegment(SId);
   {$ifc ShowSwap then}
   if LightsOn then
    RasterOp(RNot, LightWidth, LightHeight,
                   LightSwap, LightY, SScreenW, SScreenP,
                   LightSwap, LightY, SScreenW, SScreenP);
   {$endc}
   RaiseP(A,B,C,D);
   Goto 1
 end { All };
 handler XStackOverflow;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       XStackOverflow is not caught by all above, so catch
{       separately.
{
{ Errors:
{       The exception is re-raised.
{
{ Design:
{       If someone returns from the exception, we retry the SwapOut.
{
{-----------------------------------------------------------------------------}

 begin { XStackOverflow }
   SAT^[E.H].Moving := False;
   if TAllocated then ReleaseSegmentNumber(T);
   if FirstWrite then DestroySpiceSegment(SId);
   {$ifc ShowSwap then}
   if LightsOn then
    RasterOp(RNot, LightWidth, LightHeight,
                   LightSwap, LightY, SScreenW, SScreenP,
                   LightSwap, LightY, SScreenW, SScreenP);
   {$endc}
   Raise XStackOverflow;
   Goto 1
 end { XStackOverflow };

begin { SwapOut }
 1:
 FirstWrite := False;
 TAllocated := False;
 {$ifc ShowSwap then}
 LightsOn := False;
 {$endc}
 {$ifc SysTiming then}
 IOGetTime(StartTime.D);
 SaveIOTime := IOTime;
 {$endc}
 T := NewSegmentNumber;
 TAllocated := True;
 MakeEdge(Head,SATSeg);
 if not SwappingAllowed then raise SwapError;
 with SAT^[E.H], SIT^[E.H] do
  begin Moving := true;
   {$ifc PrintSwapInfo then}
   Write(' *** Swap ', E.H:1, ' out');
   {$endc}
   if Kind = DataSegment then
    begin
     {$ifc ShowSwap then}
     RasterOp(RNot, LightWidth, LightHeight,
                    LightSwap, LightY, SScreenW, SScreenP,
                    LightSwap, LightY, SScreenW, SScreenP);
     LightsOn := True;
     {$endc}
     if SwapInfo.DiskId = 0 then
      begin
       Partition := WhichPartition(SwapSId);
       if Partition = 0 then raise PartNotMounted;
       SId := CreateSpiceSegment(Partition,Temporary);
       FirstWrite := True
      end
     else
      begin
       SId := FileIdToSegId(SwapInfo.DiskId);
       FirstWrite := False
      end;
     {$ifc PrintSwapInfo then}
     if FirstWrite then Write(' for the first time');
     Write('...');
     {$endc}
     if FirstWrite or (WhichDisk(SId) = 1 { Floppy }) then
      begin
       for I := 0 to Size do
        WriteSpiceSegment(SId, I, 1, MakePtr(E.H, Shift(I,8), ptrDiskBuffer));
       if FirstWrite then
        begin
         Ptr := ReadHeader(SId);
         Kludge.A := LogAddrToPhysAddr(Ptr^.NextAdr);
         SwapInfo.DiskLowerAddress := Kludge.D[0];
         SwapInfo.DiskUpperAddress := Kludge.D[1]
        end;
       FlushAll;
       if FirstWrite then SwapInfo.DiskId := SegIdToFileId(SId)
      end
     else
      begin SetDiskAddress(E.H);
       DoDiskIO(IOWrite,E.H)
      end
    end
   else
    {$ifc PrintSwapInfo then}
    Write('...')
    {$endc}
    ;
   NotResident := true;
   Moving := false;
   SAT^[T].Size := Size;
   SAT^[T].BaseUpper := BaseUpper;
   SAT^[T].BaseLower := BaseLower;
   SIT^[T].NextSeg := NextSeg;
   SIT^[E.T].NextSeg := T;
   BaseUpper := 0;
   BaseLower := 0;
   NextSeg := SATSeg;
   SIT^[Head.T].NextSeg := E.H;
   DeleteSegment(T);
   E.T := T;
   E.H := T;
   {$ifc PrintSwapInfo then}
   Writeln('done.')
   {$endc}
  end;
 {$ifc SysTiming then}
 IOTime := SaveIOTime;
 IOGetTime(EndTime.D);
 SwapTime := (EndTime.DW - StartTime.DW) + SwapTime;
 {$endc}
 {$ifc ShowSwap then}
 if LightsOn then
  RasterOp(RNot, LightWidth, LightHeight,
                 LightSwap, LightY, SScreenW, SScreenP,
                 LightSwap, LightY, SScreenW, SScreenP);
 {$endc}
end { SwapOut };

procedure SwapIn( E: MMEdge; S: SegmentNumber; P: MMPosition );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapIn swaps a segment in from disk.
{
{ Parameters:
{       E - An edge describing where to put the segment in memory.  The head
{           is a free segment which will be filled by the segment to be
{           swapped in.  The tail is the previous segment.
{       S - The segment to swap in.
{       P - The position (low end or high end) to use within the head segment
{           of the edge.
{
{ Errors:
{       SwapInFailure  if attempt to swap in a segment which was never swapped
{                      out.
{
{-----------------------------------------------------------------------------}

label 1;
var Base, NewSize: integer;
    FirstBlock, I: Integer;
    SEdge: MMEdge;
    OldSAT: SATEntry;
    OldSIT: SITEntry;
    {$ifc SysTiming then}
    StartTime, EndTime: record case Integer of
                            1: (D: Double);
                            2: (DW: long);
                            3: (FSB: FSBit32)
                            end;
    SaveIOTime: long;
    {$endc}
    {$ifc ShowSwap then}
    LightsOn: Boolean;
    {$endc}

 handler All( A, B, C, D: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       All exceptions are caught during SwapIn so that the segment table
{       can be cleaned up.
{
{ Errors:
{       The exception is re-raised.
{
{ Design:
{       If someone returns from the exception, we retry the SwapIn.
{
{-----------------------------------------------------------------------------}

 begin { All }
   SAT^[S] := OldSAT;
   SIT^[S] := OldSIT;
   {$ifc ShowSwap then}
   if LightsOn then
    RasterOp(RNot, LightWidth, LightHeight,
                   LightSwap, LightY, SScreenW, SScreenP,
                   LightSwap, LightY, SScreenW, SScreenP);
   {$endc}
   RaiseP(A,B,C,D);
   Goto 1
 end { All };
 handler XStackOverflow;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       XStackOverflow is not caught by all above, so catch
{       separately.
{
{ Errors:
{       The exception is re-raised.
{
{ Design:
{       If someone returns from the exception, we retry the SwapIn.
{
{-----------------------------------------------------------------------------}

 begin { XStackOverflow }
   SAT^[S] := OldSAT;
   SIT^[S] := OldSIT;
   {$ifc ShowSwap then}
   if LightsOn then
    RasterOp(RNot, LightWidth, LightHeight,
                   LightSwap, LightY, SScreenW, SScreenP,
                   LightSwap, LightY, SScreenW, SScreenP);
   {$endc}
   Raise XStackOverflow;
   Goto 1
 end { XStackOverflow };

begin { SwapIn }
 1:
 OldSAT := SAT^[S];
 OldSIT := SIT^[S];
 {$ifc ShowSwap then}
 LightsOn := False;
 {$endc}
 {$ifc SysTiming then}
 IOGetTime(StartTime.D);
 SaveIOTime := IOTime;
 {$endc}
 MakeEdge(SEdge,S);
 with SAT^[S], SIT^[S] do
  begin
   if SwapInfo.DiskId = 0 then raise SwapInFailure(S);
   {$ifc ShowSwap then}
   RasterOp(RNot, LightWidth, LightHeight,
                  LightSwap, LightY, SScreenW, SScreenP,
                  LightSwap, LightY, SScreenW, SScreenP);
   LightsOn := True;
   {$endc}
   Moving := true;
   {$ifc PrintSwapInfo then}
   Write(' *** Swap ', S:1, ' in...');
   {$endc}
   { compute new actual size of E.H }
   NewSize := SAT^[E.H].Size - Size;
   { compute new base address of S }
   if P = MMLowPos then
    begin BaseUpper := SAT^[E.H].BaseUpper;
     BaseLower := SAT^[E.H].BaseLower
    end
   else { P = MMHighPos }
    begin
     with SAT^[E.H] do
      Base := Shift(BaseUpper,8) + BaseLower;
     Base := Base + NewSize;
     BaseUpper := Shift(Base,-8);
     BaseLower := LAnd(Base,#377)
    end;
   NotResident := false;
   SetDiskAddress(S);
   if WhichDisk(BlockSId) = 1 { Floppy } then
    begin
     FirstBlock := BlockHeader^.LogBlock;
     for I := 0 to Size do
      ReadSpiceSegment(BlockSId, I+FirstBlock, 1,
                       MakePtr(S, Shift(I,8), ptrDiskBuffer))
    end
   else DoDiskIO(IORead,S);
   { remove S from the segment table }
   SIT^[SEdge.T].NextSeg := NextSeg;
   { put it back in at the right place }
   if NewSize = 0 { sizes are equal } then
    begin
     NextSeg := SIT^[E.H].NextSeg;
     SIT^[E.T].NextSeg := S;
     ReleaseSegmentNumber(E.H)
    end
   else
    begin
     with SAT^[E.H] do
      begin
       Size := NewSize - 1;                    { new size of the hole }
       Base := Shift(BaseUpper,8) + BaseLower  { old base of the hole }
      end;
     if P = MMLowPos then
      begin
       Base := Base + Size + 1;                { adjust base of the hole }
       SAT^[E.H].BaseUpper := Shift(Base,-8);
       SAT^[E.H].BaseLower := LAnd(Base,#377);
       NextSeg := E.H;
       SIT^[E.T].NextSeg := S
      end
     else { P = MMHighPos }
      begin
       NextSeg := SIT^[E.H].NextSeg;
       SIT^[E.H].NextSeg := S
      end;
     MMFirst := NextSeg
    end;
   Moving := false;
   {$ifc PrintSwapInfo then}
   Writeln('done.')
   {$endc}
  end;
 {$ifc SysTiming then}
 IOTime := SaveIOTime;
 IOGetTime(EndTime.D);
 SwapTime := (EndTime.DW-StartTime.DW) + SwapTime;
 {$endc}
 {$ifc ShowSwap then}
 if LightsOn then
  RasterOp(RNot, LightWidth, LightHeight,
                 LightSwap, LightY, SScreenW, SScreenP,
                 LightSwap, LightY, SScreenW, SScreenP);
 {$endc}
end { SwapIn };

procedure Compact;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compact compacts physical memory by moving as many segments as possible
{       toward low addresses.  System segments (those with a reference count
{       greater than one) will not be moved into the screen area, as segments
{       cannot jump over one another.
{
{ Errors:
{       CantMoveSegment  if attempt to move a segment with non-zero IO count.
{
{-----------------------------------------------------------------------------}

var S, S1, S2: SegmentNumber;
begin { Compact }
 {$ifc PrintSwapInfo then}
 Write(' *** Compact memory...');
 {$endc}
 S := SIT^[SATSeg].NextSeg;
 S1 := SATSeg;
 repeat S2 := S1;
  S1 := S;
  S := SIT^[S].NextSeg;
  with SAT^[S1] do
   if not SAT^[S1].InUse then { prevent moving system segment into screen }
    if (SIT^[S].RefCount <= 1) or
       (Shift(BaseUpper,8) + BaseLower > ScreenLast) then
     with SAT^[S], SIT^[S] do
      if not NotResident and (Mobility > UnMovable) then
       begin
        if IOCount <> 0 then { WaitIOComplete } raise CantMoveSegment(S);
        Moving := true;
        SAT^[S1].Moving := true;
        SAT^[S1].NotResident := false;
        with SAT^[S1] do
         CopySegment(S,S1,Shift(BaseUpper,8) + BaseLower + SAT^[S].Size + 1);
        SIT^[S1].NextSeg := NextSeg;
        SIT^[S2].NextSeg := S;
        SIT^[S].NextSeg := S1;
        SAT^[S1].NotResident := true;
        SAT^[S1].Moving := false;
        Moving := false;
        { segments are now in order: S2, S, S1 }
        DeleteSegment(S1);
        S1 := S2;                  { S1, S, next }
        MMFirst := S
       end
 until S = SATSeg;
 {$ifc PrintSwapInfo then}
 Writeln('done.')
 {$endc}
end { Compact };

procedure KeepSegments;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       KeepSegments marks the segments Keep1 through Keep4 as not
{       RecentlyUsed so that they won't be swapped out.
{
{-----------------------------------------------------------------------------}

begin { KeepSegments }
 SAT^[Keep1].RecentlyUsed := True;
 SAT^[Keep2].RecentlyUsed := True;
 SAT^[Keep3].RecentlyUsed := True;
 SAT^[Keep4].RecentlyUsed := True;
 SAT^[StackSegment].RecentlyUsed := True
end { KeepSegments };

procedure ClearRecentlyUsed;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ClearRecentlyUsed clears the RecentlyUsed flag of all segments in the
{       segment table (with the exception of Keep1 through Keep4).
{
{-----------------------------------------------------------------------------}

var S: SegmentNumber;
begin { ClearRecentlyUsed }
 S := SATSeg;
 repeat S := SIT^[S].NextSeg;
  SAT^[S].RecentlyUsed := False
 until S = SATSeg;
 KeepSegments
end { ClearRecentlyUsed };

procedure FindHole( Fsize: MMIntSize; ForUserSegment: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       FindHole attempts to find a hole (free memory) of a certain size.
{       It performs a first-fit search.  If a hole cannot be found, memory
{       is compacted, and another first-fit search is performed.  Eventually,
{       a swap-out pass will be performed.
{
{ Parameters:
{       Fsize - Minimum size of the hole.  This is an internal size--Fsize=n
{               means n+1 blocks.
{       ForUserSegment - True iff this hole is to be used for a user segment.
{               System segments may not be allocated in the screen area.
{
{-----------------------------------------------------------------------------}

 procedure MakeHole;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       MakeHole is used when FindHole's first-fit search fails.  It trys
{       compaction (and some day swapping) to make a hole of the appropriate
{       size.
{
{-----------------------------------------------------------------------------}

  procedure DeleteUnReferencedSegments;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DeleteUnReferencedSegments removes unused segments from the segment
{       table.  These are segments which are InUse with a Reference and IO
{       count of zero.
{
{-----------------------------------------------------------------------------}

  var E: MMEdge;
      Resident: Boolean;
  begin { DeleteUnReferencedSegments }
   {$ifc PrintSwapInfo then}
   Write(' *** Delete unreferenced segments...');
   {$endc}
   E.H := MMFirst;
   repeat
    E.T := E.H;
    E.H := SIT^[E.H].NextSeg;
    with SAT^[E.H], SIT^[E.H] do
     if InUse and (RefCount = 0) and (IOCount = 0) then
       begin
        Resident := not NotResident;
        DeleteSegment(E.H);
        MakeEdge(E, E.H);
        MMFirst := E.T;
        if Resident then
         with SAT^[E.H] do
          if (Fsize <= Size) and
             (ForUserSegment or
              (Shift(BaseUpper,8) + BaseLower > ScreenLast)) then
           begin
            MMHole := E;
            MMState := MMFound
           end
        end
   until (E.H = MMFirst) or (MMState = MMFound);
   {$ifc PrintSwapInfo then}
   Writeln('done.')
   {$endc}
  end { DeleteUnReferencedSegments };

  procedure SwapOldCodeSegments;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapOldCodeSegments marks old code segments (those with which are not
{       RecentlyUsed) as swapped out.  They are not actually swapped out
{       since the .Seg file already exists and must be correct.
{
{-----------------------------------------------------------------------------}

  var E: MMEdge;
  begin { SwapOldCodeSegments }
   E.H := MMFirst;
   repeat
    E.T := E.H;
    E.H := SIT^[E.H].NextSeg;
    with SAT^[E.H], SIT^[E.H] do
     if InUse and not NotResident and
        (Kind = CodeSegment) and not RecentlyUsed and
        (Mobility > UnSwappable) and (IOCount = 0) then
       begin
        SwapOut(E);
        MakeEdge(E, E.H);
        MMFirst := E.T;
        with SAT^[E.H] do
         if (Fsize <= Size) and
            (ForUserSegment or
             (Shift(BaseUpper,8) + BaseLower > ScreenLast)) then
          begin
           MMHole := E;
           MMState := MMFound
          end
       end
   until (E.H = MMFirst) or (MMState = MMFound)
  end { SwapOldCodeSegments };

  procedure SwapOldDataSegments;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapOldDataSegments swaps out old data segments (those with which
{       are not RecentlyUsed) as swapped out.
{
{-----------------------------------------------------------------------------}

  var E: MMEdge;
  begin { SwapOldDataSegments }
   E.H := MMFirst;
   repeat
    E.T := E.H;
    E.H := SIT^[E.H].NextSeg;
    with SAT^[E.H], SIT^[E.H] do
     if InUse and not NotResident and
        (Kind = DataSegment) and not RecentlyUsed and
        (Mobility > UnSwappable) and (IOCount = 0) then
       begin
        SwapOut(E);
        MakeEdge(E, E.H);
        MMFirst := E.T;
        with SAT^[E.H] do
         if (Fsize <= Size) and
            (ForUserSegment or
             (Shift(BaseUpper,8) + BaseLower > ScreenLast)) then
          begin
           MMHole := E;
           MMState := MMFound
          end
       end
   until (E.H = MMFirst) or (MMState = MMFound)
  end { SwapOldDataSegments };

 begin { MakeHole }
  MMState := succ(MMState);
  case MMState of
   MMScan2:  DeleteUnReferencedSegments;
   MMScan3:  Compact;
   MMScan4:  if SwappingAllowed then
              begin
               KeepSegments;
               {$ifc PrintSwapInfo then}
               Writeln(' *** Swap out old code segments.');
               {$endc}
               SwapOldCodeSegments
              end
             else MMState := MMNotFound;
   MMScan5:  Compact;
   MMScan6:  begin
              {$ifc PrintSwapInfo then}
              Writeln(' *** Swap out old data segments.');
              {$endc}
              SwapOldDataSegments
             end;
   MMScan7:  Compact;
   MMScan8:  begin
              ClearRecentlyUsed;
              {$ifc PrintSwapInfo then}
              Writeln(' *** Swap out code segments.');
              {$endc}
              SwapOldCodeSegments
             end;
   MMScan9:  Compact;
   MMScan10: begin
              {$ifc PrintSwapInfo then}
              Writeln(' *** Swap out data segments.');
              {$endc}
              SwapOldDataSegments
             end;
   MMScan11: Compact;
   MMNotFound:
   end;
  if MMState <> MMFound then MMHole.H := MMFirst
 end { MakeHole };

begin { FindHole }
 MMHole.H := MMFirst;
 MMState := MMScan1;
 repeat MMHole.T := MMHole.H;
  MMHole.H := SIT^[MMHole.T].NextSeg;
  with SAT^[MMHole.H] do
   if not InUse and (Fsize <= Size) and
      (ForUserSegment or (Shift(BaseUpper,8) + BaseLower > ScreenLast)) then
    MMState := MMFound
   else
    if MMHole.H = MMFirst then MakeHole
 until MMState in [MMNotFound, MMFound]
end { FindHole };

procedure IncIOCount( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       IncIOCount increments the count of input/output references to a
{       data segment.  A non-zero IO count prevents a segment from being
{       moved, swapped, or destroyed.
{
{       IncIOCount will increment the count of only one segment and thus
{       should not be applied to the base segment number of a heap.  The
{       segment number should be extracted from the pointer being used.
{
{ Parameters:
{       S - Segment number.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{       FullMemory  if S is not resident and there isn't enough memory to
{                   swap it in.
{
{-----------------------------------------------------------------------------}

begin { IncIOCount }
 with SIT^[S] do
  if IOCount < MMMaxCount then IOCount := IOCount + 1;
 if SAT^[S].NotResident then SwapSegmentsIn(S,S,StackSegment,ReturnSegment)
end { IncIOCount };

procedure DecIOCount( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       DecIOCount decrements the IO count of a data segment by one.  If the
{       reference and IO counts both become zero:
{         * if the segment is a data segment, it is destroyed.
{         * if the segment is a code segment, it is destroyed only if it is in
{           the screen or is non-resident.
{
{       DecIOCount will decrement the count of only one segment and thus
{       should not be applied to the base segment number of a heap.  The
{       segment number should be extracted from the pointer being used.
{
{ Parameters:
{       S - Number of the segment.
{
{ Errors:
{       UnusedSegment  if S is not in use.
{
{-----------------------------------------------------------------------------}

begin { DecIOCount }
 with SAT^[S], SIT^[S] do
  if (IOcount > 0) and (IOCount < MMMaxCount) then
   begin IOCount := IOCount - 1;
    if (RefCount = 0) and (IOCount = 0) then
     if NotResident or (Kind = DataSegment) or
        (Shift(BaseUpper,8) + BaseLower <= ScreenLast) then
      DeleteSegment(S)
   end
end { DecIOCount };

procedure SwapSegmentsIn( S1, S2, S3, S4: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapSegmentsIn ensures that when it returns, S1, S2, S3, and S4 are
{       resident.
{
{ Parameters:
{       S1, S2, S3, S4 - segments to swap in.
{
{ Errors:
{       NilPointer  if one of the segments is zero.
{       UnusedSegment  if one of the segments is not really in use.
{       FullMemory  if there isn't enough memory to swap one of the
{                   segments in.
{
{-----------------------------------------------------------------------------}

 procedure SwapOneIn( Seg: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SwapOneIn ensures that when it returns, Seg is resident.
{
{ Parameters:
{       Seg - segment to swap in.
{
{ Errors:
{       NilPointer  if Seg is zero.
{       Unused segment  if Seg is not really in use.
{       FullMemory  if there isn't enough memory to swap Seg in.
{
{-----------------------------------------------------------------------------}

 begin { SwapOneIn }
  with SAT^[Seg] do
   if not InUse then
     if Seg = 0 then raise NilPointer
     else raise UnusedSegment(Seg)
   else
    if NotResident then
     begin FindHole(Size,SIT^[Seg].RefCount <= 1);
      if MMState = MMNotFound then raise FullMemory
      else SwapIn(MMHole,Seg,MMLowPos)
     end
 end { SwapOneIn };

begin { SwapSegmentsIn }
  Keep1 := S1;
  Keep2 := S2;
  Keep3 := S3;
  Keep4 := S4;
  SwapOneIn(S1);
  SwapOneIn(S2);
  SwapOneIn(S3);
  SwapOneIn(S4)
end { SwapSegmentsIn }.
