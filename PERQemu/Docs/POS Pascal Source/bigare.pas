{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module BigArea;
{-----------------------------------------------------------------
      WJHansen   
      Copyright C, 1982, 1983 - Three Rivers Computer Corporation
      
 Abstract:
     Provides procedures to allocate and release large segments 
     and groups of segments.  For contiguous areas, the mobility
     can also be set.
{----------------------------------------------------------------}

{$Version V0.5 for POS}
{-----------------------------------------------------------------
    Change Log:
 
 13 Dec 82  V0.5  Chuck Beckett
      Changes to reflect "heap" field of SIT table.
 
 16 Nov 82  V0.4  Bill Braucher
      Fixed names for 14-character compiler.
 
 24 Jun 82  V0.3  WJHansen
      Correct CreateContiguousArea, ConsecutiveSegments.
      Add comment that PieceSize should be small.
      Export ConsecutiveSegments.
    
 23 Jun 82  V0.2  WJHansen
      Adapted CreateContiguousArea from George Mouradian.
    
 23 Jun 82  V0.1  WJHansen
      Enter code for ConsecutiveSegments, SortSegList, CreateBigArea,
        and DecBigAreaRef.
 
 22 Jun 82  V0.0  WJHansen
      Define procedural interface.
{----------------------------------------------------------------}



exports


imports Memory from Memory;

procedure  CreateBigArea (var S: SegmentNumber; TotSize, PieceSize: integer);
procedure  CreateContiguousArea (var S: SegmentNumber; 
                              TotSize: integer; Mob: SegmentMobility);
procedure  DecBigAreaRef (S: SegmentNumber; TotSize, PieceSize: integer);
procedure  DecContiguousAreaRef (S: SegmentNumber; TotSize: integer);
procedure  SortSegList;
function   ConsecutiveSegments(N: integer): SegmentNumber;

exception  BadMobility(M: SegmentMobility);
{----------------------------------------
{ Abstract:
{   Raised when CreateContiguousArea 
{   is given a disallowed Mob value.
{---------------------------------------}


  private


const
      DEBUG = FALSE;
      
      
      
imports Virtual from Virtual;

const {$include Perq.Qcodes.Dfs}

const

   NoSeg = 0;         {NIL for a segment number;  end of free list}
   
{$R-}   {because SAT and SIT have upper bound 0}


function ConsecutiveSegments(N: integer): SegmentNumber;
{-----------------------------------------------------------------
    Abstract:
       Finds a block of N consecutive unallocated segment number,
       allocates them, and return the segment number of the first. 
    Parameters:
       N - number of segment numbers to allocate.
    Returns:
       The segment number of the first of N previously
       unallocated segment numbers.
    Exceptions:
       NoFreeSegments - If no sequence of N free segment numbers can be found.
       BadSize - If N<1.
    Environment:
       Interrupts are assumed to be ON initially.  They are turned off
       and then back ON.
    Design:
       First calls SortSegList and then searches free list of segment
       numbers for a long enough group of segment numbers.
{----------------------------------------------------------------}
var
   Prev, This, GrpPrev, GrpStart: SegmentNumber;
   Done: Boolean;
begin
   if N<1 then 
      raise BadSize(NoSeg, N);

   SortSegList;

   Prev := MaxInt-1;
   This := MMFree;      {pt at start of list of unallocated seg nums}
   GrpPrev := MaxInt-1;
   GrpStart := This;  
   Done := (N=1);
   while not Done and (This <> NoSeg) do begin  {search for consec numbers}
      if This = Prev+1 then   {two consecutive segments}
         if This -GrpStart + 1 >=N then done := true
         else begin 
            Prev := This;
            This := SIT^[This].NextSeg;
         end
      else begin  {start a new group}
         GrpStart := This;
         GrpPrev := Prev;
         Prev := This;
         This := SIT^[This].NextSeg;
      end;
   end;

   if This=NoSeg then begin    {note: works even for N=1}
      ConsecutiveSegments := NoSeg; 
      raise NoFreeSegments;
   end
   else begin   {found a group: allocate them}
      ConsecutiveSegments := GrpStart; 
      {remove the group from list of unallocated segments
           This is a unitary operation, so interrupts are left ON}
      if GrpPrev=MaxInt-1 then
         MMFree := SIT^[This].NextSeg
      else 
         SIT^[GrpPrev].NextSeg := SIT^[This].NextSeg;
      for This := GrpStart to GrpStart+N-1 do 
         {initialize each segment allocated}
         with SAT^[This], SIT^[This] do begin 
            NotResident := True;    {must be set False when physical allocated}
            Moving := False;
            RecentlyUsed := False;
            Heap := False;
            Kind := DataSegment;
            InUse := False;
            Freelist := 0;
            RefCount := 0;
            IOCount := 0;
            Mobility := Swappable;
            BootLoaded := False;
            SwapInfo.DiskId := 0;
            SwapInfo.DiskLowerAddress := 0;
            SwapInfo.DiskUpperAddress := 0;
         end;
      SIT^[GrpStart+N-1].NextSeg := NoSeg;
   end;  {of allocating a group}
end;



procedure  CreateBigArea (var S: SegmentNumber; TotSize, PieceSize: integer);
{-----------------------------------------------------------------
    Abstract:
       Creates a big area, possibly composed of multiple segments.
       It will be swappable.
 
    Parameters:
       S - segment number assigned to area.  If the area is more than 
           one piece, consecutive segment numbers are given to the pieces.
       TotSize - total number of blocks to allocate to area.  (Each block
           is 256 words.)  The maximum value for TotSize is 32767,
           which corresponds to almost 16 megabytes (more than will
           fit in all but the largest swapping partition).
       PieceSize - Size (in blocks) of each segment to allocate for area.
           1 <= PieceSize <= 256.
           If PieceSize is less than 256, then swapping will be better,
           but addresses will not be contiguous.  Moreover, with a 
           small PieceSize, NoFreeSegments is more likely (because
           longer sequences of free segment numbers are needed).
           
           Recommended PieceSize: 15 blocks.
           
    Exceptions:
       FullMemory - There is not enough room in physical memory to
           satisfy the request.
       NoFreeSegments - Memory manager was unable to find a suitable
           sequence of consecutive segment numbers.
       BadSize - TotSize is negative or 1>PieceSize or 256<PieceSize.
    Calls:
       ConsecutiveSegments, CreateSegment, MakeEdge, ReleaseSegmentNumber
    Environment:
       Interrupts are assumed to be ON initially.  They are turned off
       and then back ON.
    Design:
       First finds consecutive segment numbers for the various pieces.
       Then allocates that many segments and uses those segment numbers.
       The address of the i'th word is 
            makeptr (S+i div (PieceSize*256),  i mod (PieceSize*256),  word),
       but the multiplies and divides can be done with shifting 
       if PieceSize is chosen appropriately.
       With PieceSize=16 we have the address as
            makeptr (S+Shift(i,-12), LAnd(i,#7777), word)
       If i is long and its pieces are i.hi and i.lo then we have
            makeptr (S+Shift(i.lo,-12)+Shift(i.hi,4), LAnd(i.lo,#7777), word).
    Note:
       CreateBigArea should eventually be revised to create segments in 
       the swapped out state.  Physical core should only be allocated 
       when the segment is referenced.  (Even now, disk space is only 
       assigned when a data segment is to be swapped out.)
{----------------------------------------------------------------}
var
   This, Temp, NSegs, LastSeg: SegmentNumber;
   E: MMEdge;
begin
   if (TotSize<1) then
      raise BadSize (NoSeg, TotSize);
   if (PieceSize<1) or (PieceSize>256) then
      raise BadSize (NoSeg, PieceSize);

   NSegs := (TotSize+PieceSize-1) div PieceSize;   {(ceiling function)}
   S := ConsecutiveSegments(NSegs);
   
   if DEBUG then
       writeln('Allocated ', NSegs:1, ' seg nums at ', S:1);
   if DEBUG then
       write('Discarded seg nums are ');
       
   LastSeg := S+NSegs-1;
   for This := S to LastSeg do begin
      if This<>LastSeg then  {not last piece}
         CreateSegment(Temp, PieceSize, 1, PieceSize)
      else   {allocate last piece possibly smaller than the others}
         CreateSegment(Temp, TotSize-(NSegs-1)*PieceSize, 1, PieceSize);

      InLineByte(IntOFF);
      MakeEdge(E, Temp);  {now SIT^[E.T].NextSeg = E.H = Temp}
      {Initialize This to represent Temp}
      SIT^[This] := SIT^[Temp];
      SAT^[This] := SAT^[Temp];
      {relink segnum list to use This for Temp}
      SIT^[E.T].NextSeg := This;
      ReleaseSegmentNumber(Temp);
      if MMFirst = Temp then 
         MMFirst := This;
      InLineByte(IntON);
      
      if DEBUG then write (Temp:4);
      
   end;
   
   if DEBUG then writeln;
end;

procedure  CreateContiguousArea (var S: SegmentNumber; 
                              TotSize: integer; Mob: SegmentMobility);
{-----------------------------------------------------------------
    Abstract:
       Creates a contiguous area of physical memory.  It is forced to
       remain resident because of the mobility.  Use of this module
       avoids the swap-out-swap-in of using CreateSegment and SetMobility.
 
    Parameters:
       S - segment number assigned to area.  If the area is more than 
           one piece, consecutive segment numbers are given to the pieces.
       TotSize - total number of blocks to allocate to area.  (Each block
           is 256 words.)   Max is 8*256, this is one megabyte.
       Mob - Mobility for the segment.  Must be UnSwappable or UnMovable.
    Exceptions:
       FullMemory - There is not enough room in physical memory to
           satisfy the request.
       NoFreeSegments - Memory manager was unable to find a suitable
           sequence of consecutive segment numbers.
       BadMobility - Mob must be UnSwappable or UnMovable.
    Environment:
       Interrupts are assumed to be ON initially.  They are turned off
       and then back ON.
    Calls:
       ContiguousSegments, Compact, FindHole, NewSegmentNumber.
{----------------------------------------------------------------}
var 
    memory: MMPointer;
    T, U, This, LastSeg, NSegs: SegmentNumber;
    Base: Integer;

begin { CreateContiguousArea }
 if (TotSize < 1) or (TotSize > 8*256) then
    raise BadSize(NoSeg, TotSize);
 if Mob>UnSwappable then
    raise BadMobility(Mob);
 NSegs := (TotSize+256-1) div 256;
 S := ConsecutiveSegments(NSegs);
   
 if DEBUG then
    writeln('Allocated ', NSegs:1, ' seg nums at ', S:1);
 
 Keep1 := CurrentSegment;
 Keep2 := StackSegment;
 Keep3 := StackSegment;
 Keep4 := ReturnSegment;
 if Mob=UnMovable then begin
    FindHole(TotSize-1, True);    {swap enough out}
    Compact;                      {get the hole to high end of memory}
 end;
 FindHole(TotSize - 1, True);     {allocate the area}
 if MMState = MMFound then begin
    T := MMHole.H;
 
    if DEBUG then
       writeln('allocating from T = ', T:2, '  Size=', SAT^[T].Size:1);

    InLineByte(IntOFF);
    if SAT^[T].Size >= TotSize then with SAT^[T], SIT^[T] do begin 
             { make new unused segment to sit on extra memory 
               put it BEFORE the new seg (at top of memory)}
       U := NewSegmentNumber;
       SAT^[U].Size := Size - TotSize;
       SAT^[U].BaseUpper := BaseUpper;
       SAT^[U].BaseLower := BaseLower;
       Base := Shift(BaseUpper,8) + BaseLower + Size + 1 - TotSize;
       BaseUpper := Shift(Base,-8);
       BaseLower := LAnd(Base,#377);
       Size := TotSize - 1;
       SIT^[MMHole.T].NextSeg := U;
       SIT^[U].NextSeg := S;
 
       if DEBUG then
          writeln('releasing part U = ', U:2, '  Size=', SAT^[U].Size:1);

    end
    else  {use all of T}
       SIT^[MMHole.T].NextSeg := S;
       
    {set the allocated SIT/SAT entries to point to pieces of T}
    Base := Shift(SAT^[T].BaseUpper,8) + SAT^[T].BaseLower;
    LastSeg := S+NSegs-1;
    for This := S to LastSeg do with SAT^[This], SIT^[This] do begin
       {Initialize This to represent a piece of T}
       NotResident := False;
       Moving := False;
       RecentlyUsed := False;
       Heap := False;
       Kind := DataSegment;
       Full := False;
       InUse := True;
       Lost := False;
       BaseLower := LAnd(Base,#377);
       BaseUpper := Shift(Base,-8);
       Base := Base + 256;   {advance past 256 blocks to next chunk}
       Size := 255;
       NextSeg := This+1;
       RefCount := 1;
       IOCount := 0;
       Mobility := Mob;
       BootLoaded := False;
       SwapInfo.DiskLowerAddress := 0;
       SwapInfo.DiskUpperAddress := 0;
       SwapInfo.DiskId := 0;
       Increment := 0;
       Maximum := 255;
       Freelist := 0;

       memory.Segmen := This;
       memory.Offset := 0;
       with memory.m^.m[0] do begin
          N := 0; 
          L := Shift(Size+1, 7); 
       end;
    end; {with}
    SAT^[LastSeg].Size := TotSize-(NSegs-1)*256-1;
    SIT^[LastSeg].Maximum := TotSize-(NSegs-1)*256-1;
    SIT^[LastSeg].NextSeg := SIT^[T].NextSeg;
    ReleaseSegmentNumber(T);    
    MMFirst := LastSeg;
    InLineByte(IntON);

 end
 else raise FullMemory;
end; { CreateContiguousArea }

procedure  DecBigAreaRef (S: SegmentNumber; TotSize, PieceSize: integer);
{-----------------------------------------------------------------
    Abstract:
       Releases a big area allocated with CreateBigArea.
 
    Parameters:
       S - segment number assigned to area.  If the area is more than 
           one piece, consecutive segment numbers are given to the pieces.
       TotSize - total number of blocks to allocate to area.  (Each block
           is 256 words.)
       PieceSize - Size of each segment to allocate for area.  If
           PieceSize is less than 256, then swapping will be better,
           but addresses will not be contiguous.
    Exceptions:
       BadSize - if the size of a segment in the group specified by
           the above parameters is inconsistent with the size that
           would have been given by CreateBigArea.
       others from DecRefCount.
    Design:
       Simply calls DecRefCount for each segment in the group.
{----------------------------------------------------------------}
var
   NSegs, LastSeg: SegmentNumber;
begin
   NSegs := (TotSize+PieceSize-1) div PieceSize;   {(ceiling function)}
   LastSeg := S+NSegs-1;
   for S := S to LastSeg do begin
      DataSeg(S);        {be sure it is a data segment and is InUse}
      if S=LastSeg then
          PieceSize := TotSize-(NSegs-1)*PieceSize;
      if SAT^[S].Size+1 <> PieceSize then
          raise BadSize(S, SAT^[S].Size+1);
      DecRefCount(S);
   end;
end;

procedure  DecContiguousAreaRef (S: SegmentNumber; TotSize: integer);
{-----------------------------------------------------------------
    Abstract:
       Releases a contiguous area allocated with CreateContiguousArea.
    Parameters:
       S - segment number assigned to area.  If the area is more than 
           one piece, consecutive segment numbers are given to the pieces.
       TotSize - total number of blocks to allocate to area.  (Each block
           is 256 words.)
    Environment:
       Interrupts are assumed to be ON initially.  They are turned off
       and then back ON.
    Calls:
       DecBigRefArea (with PieceSize=256).
    Exceptions:
       see DecBigAreaRef.
{----------------------------------------------------------------}
begin
   DecBigAreaRef(S, TotSize, 256);
end;

procedure  SortSegList;
{-----------------------------------------------------------------
    Abstract:
       Sorts the list of segment numbers so it is more likely
       that consecutive segment numbers can be found.
 
    Design:
       Sorts the segment number list and reconstructs it, putting longest
       consective secuence of numbers at the end.  Thus shorter
       consecutive sequecenes are used up first and longer
       sequences are saved for CreateXxxArea.
       
       Rather than do a sort, the groups of consecutive segment numbers
       are added to lists categorized by length.
       The front and rear of each list is in array
           Group
       Thus, for 1<=i<=NumGroups-1 all chains of length i are 
       put in the list starting at Group[i].Front and extending
       along its NextSeg pointers to Group[i].rear;
       All groups of length >= NumGroups are in the list under
       Group[NumGroups].
       
       After filing all sequences into Group, a new free list
       of segment numbers is built with the longest groups at the end.
       
    Environment:
       Interrupts are assumed to be ON initially.  They are turned off
       and then back ON.
{----------------------------------------------------------------}
const
   NumGroups = 65;
           {If PieceSize is 256 then a megabyte is 8 segments.
           However, if PieceSize is 32 (=8K words) then a 
           megabyte requires 64 consecutive segments.}
var
   Group: array [1..NumGroups] of record
                                    Front, Rear: SegmentNumber;
                                  end;

   procedure EndAGroup(Start,Finish: SegmentNumber);
   {-----------------------------
    Abstract: 
       Put a consecutive sequence of segment numbers into Group array.
    Parameters:
       Start, Finish - Segment numbers of first and last of the group.
    Design:
       SIT^[Group[i].Rear] is expected to be set later.
   {----------------------------}
   var
      i, ThisGrp: SegmentNumber;
   begin
      ThisGrp := Finish-Start+1;
      if ThisGrp > NumGroups then
         ThisGrp := NumGroups;
      for i := Start to Finish-1 do {link members of this group}
         SIT^[i].NextSeg := i+1;

      {link into Group list}
      if Group[ThisGrp].Rear=NoSeg then
         Group[ThisGrp].Front := Start
      else 
         SIT^[Group[ThisGrp].Rear].NextSeg := Start;
      Group[ThisGrp].Rear := Finish;
   end;

var
   i, CurGrp, MaxS: SegmentNumber;
   
This: SegmentNumber;
Cnt, TotCnt: integer;

begin
   MaxS := (SAT^[SITSeg].Size + 1) * 256 div WordSize(SITEntry) - 1;
   for i := 1 to NumGroups do begin
      Group[i].Front := NoSeg;
      Group[i].Rear  := NoSeg;
   end;

   InLineByte(IntOFF);

   for i := 1 to MaxS do  {mark all segments as free}
      SAT^[i].Lost := True;
   i := MMFirst;
   repeat                    {mark active segments as not free}
      SAT^[i].Lost := False;
      i := SIT^[i].NextSeg;
   until i=MMFirst;

   {file all sequences into Group lists}
   TotCnt := 0;
   CurGrp := NoSeg;
   for i := 1 to MaxS do
      if SAT^[i].Lost then begin {free seg}
         TotCnt := TotCnt+1;
         if CurGrp=NoSeg then  {start a group}
            CurGrp := i
         else; {extend CurGrp}
      end
      else if CurGrp<>NoSeg then begin
         EndAGroup(CurGrp, i-1);
         CurGrp := NoSeg;
      end;
   if CurGrp<>NoSeg then {end last group}
      EndAGroup(CurGrp, MaxS);


   if DEBUG then begin
      writeln('Found ', TotCnt:1, ' free segment numbers');
      TotCnt := 0;
      writeln;
      writeln('Grp  #');
      for i := 1 to NumGroups do 
         if Group[i].Rear <> NoSeg then begin
            This := Group[i].Front;
            Cnt := 1;  {rear does not get counted}
            while This<> Group[i].Rear do begin
                Cnt := Cnt+1;
                This := SIT^[This].NextSeg;
            end;
            TotCnt := TotCnt+Cnt;
            writeln (i:2, Cnt:4);
         end;
      writeln(' =',TotCnt:4);
   end;   
   
   {now link groups together}
   MMFree := NoSeg;
   for i := NumGroups downto 1 do
      if Group[i].Rear<>NoSeg then begin
         SIT^[Group[i].Rear].NextSeg := MMFree;
         MMFree := Group[i].Front;
      end;

   InLineByte(IntON);
end.
      
