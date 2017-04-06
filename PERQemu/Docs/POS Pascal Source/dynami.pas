{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Dynamic;
{-----------------------------------------------------------------------------
{
{       Dynamic - Perq dynamic memory allocation and de-allocation.
{       J. P. Strait      1 Jan 80.
{       Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1982, 1983.
{
{ Abstract:
{       Dynamic implements Pascal dynamic allocation - New and Dispose.
{       Memory of a given size with a given alignment may be allocated from
{       any data segment with the standard procedure New which calls
{       the NewP procedure of Dynamic.
{
{       Data segments are created with CreateSegment from Memory or
{       CreateHeap from Dynamic.  Segments created with CreateSegment
{       are automatically enlarged when they become full.  They are
{       enlarged by multiples of the segment's increment size until
{       there is enough free memory for the allocation.  When an attempt
{       is made to increase the segment past its maximum size, an
{       exception is raised.
{
{       Segments created with CreateHeap do not have increment sizes or
{       maximum sizes.  Whenever a segment becomes full, another segment
{       of the same size is created and linked to the full segment.
{       This new segment is given the same reference count as the parent.
{       Allocation is potentially done from any of the segments.  Thus
{       There are a heap of segments from which to allocate.  This heap
{       is identified by the segment number of the first segment
{       allocated.  If an allocation is attempted which is larger than
{       the size of the segments, one larger segment is created.
{
{       Both heaps and segments may be destroyed by DecRefCount.
{       IncRefCount and DecRefCount applied to a single segment in a
{       heap increment or decrement all segments in the heap.
{       DecIOCount and IncIOCount increment and decrement only a
{       single segment.
{
{       Dispose may be used for both segments and heaps.  Memory that
{       is deallocated by Dispose becomes a candidate for allocation
{       with New.
{
{       The default segment (the one obtained by New(P) without an
{       explicit segment number) is made by CreateSegment(4,4,256)
{       for 1/4 MByte systems and is made by CreateHeap(20) for
{       larger systems.  This segment may be destroyed by DecRefCount(0).
{
{ Design:
{       Free memory within each segment is linked into a circular freelist
{       in order of address.  Each free node is at least two words long and
{       is of the form
{
{       record Next:   Integer;
{              Length: Integer;
{              Rest:   2*Length - 2 words
{              end;
{
{       Where Next*2 is the address of the next free node and Length*2 is the
{       number of free words.
{
{-----------------------------------------------------------------------------}

{----------------------------------------------------------------
{
{  3 Nov 83  V2.6  Sandeep Johar
{ Fixed bug in new that was causing the heap to always grow by 
{ 2 segments.
{
{ 11 Feb 83  V2.5  Brad Myers.
{ Fixed bug introduced in V2.4 when allocating out of Heaps.
{ }

{  9 Feb 83  V2.4  Brad Myers.
{ Make sure NEWs are not done out of large segments.
{ Comment segment size restriction.
{ }

{ 30 Nov 82  V2.3  J Strait.
{ Copy Mobility when adding a new segment to the heap.
{ }

{ 17 Nov 82  V2.2  J Strait.
{ Combine old Dynamic and JpsDynamic.
{ Do not automatically convert Segments into Heaps.
{ Reindent to make pretty.
{ Remove version constant.
{ Copy reference count when adding a new segment to the heap.
{ Change DestroyHeap to merely call DecRefCount.
{ }

{  7 May 82  V2.1  J Strait.
{ Change search order to start with newest segment rather than oldest.
{ }

{  1 Jan 82  V2.0  J Strait.
{ Change to automatically chain segments together: add CreateHeap, DestroyHeap.
{ }

{ 26 Oct 81  V1.4  JPS
{ Make certain that the segment passed to NewP and DisposeP is a data segment.
{ }

{ 12 May 81  V1.3  JPS
{ Use exceptions rather than MemoryError.
{ }

{ 23 Mar 81  V1.2  JPS
{ Convert to standard documentation form.
{ }

{ 13 Jan 81  V1.1  JPS
{ Fix assignment of nil in New and Dispose.
{ Change bad constant #200000 to #100000.
{ Move $R- to private part.
{ }

exports


imports Memory from Memory;


 procedure NewP( S: SegmentNumber; A: integer; var P: MMPointer; L: integer );
 procedure DisposeP( var P: MMPointer; L: integer );
 procedure CreateHeap( var S: SegmentNumber; Size: MMExtSize );
 procedure DestroyHeap( S: SegmentNumber );
 
 exception NotAHeap( S: SegmentNumber );

 exception SegTooBigForNew( S: SegmentNumber );
 {-----------------------------------------------------------------------------
   Abstract: Raised when allocate out of a segment that has > 256 blocks or
             when try to create a heap of > 256 blocks.
   Parameters: The segment allocating from.
 -----------------------------------------------------------------------------}

private


{$R-}

function LT(x,y: integer): boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Unsigned comparison of two numbers.
{
{ Parameters:
{       x - Left hand operand.
{       y - Right hand operand.
{
{ Result:
{       LT == x < y.
{
{-----------------------------------------------------------------------------}

begin { LT }
  LT := ((x < 0) = (y < 0)) = (x < y)
end { LT };

function LE(x,y: integer): boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{ Parameters:
{       x - Left hand operand.
{       y - Right hand operand.
{
{ Result:
{       LT == x <= y.
{
{-----------------------------------------------------------------------------}

begin { LE }
  LE := ((x < 0) = (y < 0)) = (x <= y)
end { LE };

function GT(x,y: integer): boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{ Parameters:
{       x - Left hand operand.
{       y - Right hand operand.
{
{ Result:
{       LT == x > y.
{
{-----------------------------------------------------------------------------}

begin{ GT }
  GT := ((x < 0) = (y < 0)) = (x > y)
end { GT };

function GE(x,y: integer): boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{ Parameters:
{       x - Left hand operand.
{       y - Right hand operand.
{
{ Result:
{       LT == x >= y.
{
{-----------------------------------------------------------------------------}

begin { GE }
  GE := ((x < 0) = (y < 0)) = (x >= y)
end { GE };

procedure DisposeP( var P: MMPointer; L: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Deallocate memory.
{
{ Parameters:
{       P - Pointer to the memory.
{       L - Length in words, 0 represents a length of 2**16.
{           If L is odd, L+1 words are de-allocated.
{
{ Errors:
{       NilPointer  if P is nil.
{       BadPointer  1) if the Offset part is odd.
{                   2) if Offset+Length > size of segment.
{                   3) if the node to be Dispose overlaps some node
{                      that is already free.
{                   4) if the segment is not InUse or not a DataSegment.
{
{-----------------------------------------------------------------------------}

var Last, This, Next: MMAddress;
    Found: boolean;
    memory: MMPointer;
begin { DisposeP }
  if P.P = nil then raise NilPointer;
  with SAT^[P.Segmen] do
    if not InUse or (Kind <> DataSegment) then
      raise BadPointer;
  if Odd(P.Offset) then raise BadPointer;
  This := Shift(P.Offset, -1);
  L := Shift(L + 1, -1);
  if L = 0 then L := #100000;
  if Shift(This + L - 1, -7) > SAT^[P.Segmen].Size then raise BadPointer;
  memory.Segmen := P.Segmen;
  memory.Offset := 0;
  with SAT^[P.Segmen], SIT^[P.Segmen], memory.m^ do
    begin
      Next := Freelist;
      if Full then
        begin
          m[This].L := L;
          m[This].N := This
        end
      else
        begin
          Found := false;
          repeat
            Last := Next;
            Next := m[Next].N;
            if Next > Last then { normal case }
              Found := (This >= Last) and (This < Next)
            else { we are wrapping around }
              Found := (This < Next) or (This >= Last)
          until Found;
          if GE(Last + m[Last].L, This) and (This >= Last) then
            if Last + m[Last].L = This then
              begin { coalesce This node with Last node }
                L := m[Last].L + L;
                This := Last
              end
            else { overlapping nodes } raise BadPointer
          else m[Last].N := This;
          if GE(This + L, Next) and (This < Next) then
            if This + L = Next then
              begin { coalesce This node with Next node }
                L := L + m[Next].L;
                if m[Next].N = Next then Next := This
                else Next := m[Next].N
              end
            else { overlapping nodes } raise BadPointer;
          m[This].N := Next;
          m[This].L := L
        end;
      Full := false;
      Freelist := This;
    end;
  P.P := nil
end { DisposeP };

procedure NewP( S: SegmentNumber; A: integer; var P: MMPointer; L: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Allocate memory.
{
{ Parameters:
{       S - Number of the segment from which to allocate (if created by
{           CreateSegment) or root segment of the heap (if created by
{           CreateHeap).  0 means the default data segment.
{       A - Alignment of node in words relative to beginning of segment,
{           0 represents an alignment of 2**16.  if A is odd, A+1 is used
{           as the alignment.
{       P - Set to point to the memory that was allocated.  If the data
{           segment is full and cannot be increased, P is set to nil.
{       L - Length in words, 0 represents a length of 2**16.
{           If L is odd, L+1 words are allocated.
{
{ Errors:
{       FullSegment  if the segment has reached its maximum size and there
{                    isn't enough room for the node.
{       FullMemory  if NewP tries to expand the segment, but there enough
{                   physical memory to do so.
{       UnusedSegment  if S is not InUse.
{       NotDataSegment  if S is not a DataSegment.
{
{-----------------------------------------------------------------------------}

var First, This, Last: MMAddress;
    FirstS, ThisS: SegmentNumber;
    State: (ScanSegs, ScanList, Found, NotFound);
    memory: MMPointer;
    Wasted: Integer;

  procedure BiteAChunk;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Bite a chunk of memory out of the current free node.  The current free
{       node is specified by This, and the previous free node is specified
{       by Last.  This node must be at least L+Wasted words long.
{
{-----------------------------------------------------------------------------}

  var ThisL: integer;
      Next, Free: MMAddress;
  begin { BiteAChunk }
    with memory.m^ do
      begin
        if Wasted = 0 then
          begin
            Next := m[This].N;
            ThisL := m[This].L
          end
        else
          begin
            Next := m[This].N;
            ThisL := m[This].L - Wasted;
            m[This].L := Wasted;
            Last := This;
            This := This + Wasted
          end;
        if ThisL = L then
          if Last = This then SAT^[ThisS].Full := true
          else
            begin
              m[Last].N := Next;
              SIT^[ThisS].Freelist := Last
            end
        else
          begin
            Free := This + L;
            SIT^[ThisS].Freelist := Free;
            m[Free].L := ThisL - L;
            if Last = This then m[Free].N := Free
            else
              begin
                m[Last].N := Free;
                m[Free].N := Next
              end
          end
      end
  end { BiteAChunk };

  procedure EnlargeSegment;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Enlarge the current segment in an attempt to make free space for
{       allocation.  The segment is enlarged in multiples of its increment
{       size.  If the size reaches the segment's maximum size, State is set
{       to NotFound so that NewP will return nil.
{
{-----------------------------------------------------------------------------}

  var Inc, Len: integer;
      FreeP: MMPointer;
  begin { EnlargeSegment }
    State := ScanList;
    with SAT^[ThisS], SIT^[ThisS], memory.m^ do
      repeat
        if Size + Increment + 2 > Maximum + 1 then State := NotFound
        else
          begin
            ChangeSize(ThisS,Size + Increment + 2);
            FreeP.Segmen := ThisS;
            FreeP.Offset := Shift(Size - Increment,8);
            DisposeP(FreeP,Shift(Increment + 1,8));
            This := FreeList;
            Wasted := (A - This mod A) mod A;
            if LE(L + Wasted, m[This].L) then
              begin
                State := Found;
                Last := This;
                while m[Last].N <> This do Last := m[Last].N
              end
          end
      until State <> ScanList
  end { EnlargeSegment };

  procedure AddASegment;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Add a segment to the current heap to make free space for
{       allocation.  The added segment normally has the same size
{       as the first segment in the heap.  If the desired allocation
{       is larger than the original segment, the new segment is made
{       large enough for the allocation.
{
{-----------------------------------------------------------------------------}

  var Blocks: Integer;
  begin { AddASegment }
    Blocks := Shift(L+127,-7);
    if Blocks < SAT^[S].Size+1 then Blocks := SAT^[S].Size + 1;
    CreateSegment(ThisS,Blocks,1,Blocks);
    if SIT^[S].Mobility <> Swappable then SetMobility(ThisS,SIT^[S].Mobility);
    SIT^[ThisS].RefCount := SIT^[S].RefCount;
    SIT^[ThisS].HeapNext := SIT^[S].HeapNext;
    SAT^[ThisS].Heap := True;
    SIT^[S].HeapNext := ThisS
  end { AddASegment };

begin { NewP }
  if S = 0 then
    begin
      if MMHeap = 0 then
        if MemoryInBlocks > #1000 then { more than 1/4 MByte }
          CreateHeap(MMHeap,20)
        else CreateSegment(MMHeap,4,4,255);
      S := MMHeap
    end
  else
    with SAT^[S] do
      if not InUse then raise UnUsedSegment(S)
      else
        if Kind <> DataSegment then raise NotDataSegment(S)
        else
          if not heap then
             if SIT^[S].Maximum >= 256 then Raise SegTooBigForNew(S);

  L := Shift(L + 1, -1);
  if L = 0 then L := #100000;
  A := Shift(A + 1, -1);
  if A = 0 then A := #100000;
  State := ScanSegs;
  FirstS := S;
  ThisS := S;
  repeat
    if SAT^[ThisS].Heap then { heap, advance to next segment in list }
      ThisS := SIT^[ThisS].HeapNext;
    memory.Segmen := ThisS;
    memory.Offset := 0;
    with SAT^[ThisS], SIT^[ThisS], memory.m^ do
      if not Full then
        begin
          First := Freelist;
          This := First;
          State := ScanList;
          repeat { scan the freelist }
            Last := This;
            This := m[This].N;
            Wasted := (A - This mod A) mod A;
            if LE(L + Wasted, m[This].L) then State := Found
            else
              if This = First then State := ScanSegs
          until State <> ScanList
        end;
    if State = ScanSegs then
      if SAT^[ThisS].Heap then { heap }
        begin
          if ThisS = FirstS then { full circle, make another } 
            begin
              AddASegment;
              ThisS := FirstS;
            end;
        end
      else { segment, enlarge it } EnlargeSegment
  until State <> ScanSegs;
  if State = Found then
    begin
      BiteAChunk;
      P.Segmen := ThisS;
      P.Offset := Shift(This,1)
    end
  else
    begin
      P.P := nil;
      raise FullSegment
    end
end { NewP };

procedure CreateHeap( var S: SegmentNumber; Size: MMExtSize );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Create the root data segment for a Heap.
{
{ Parameters:
{       S - Set to the number of the new segment.
{       Size - The size of the initial segment and all subsequent segments.
{              Must be <= 256.
{
{ Errors:
{       SegTooBigForNew  if Size is > 256.
{-----------------------------------------------------------------------------}

begin { CreateHeap }
  if size > 256 then Raise SegTooBigForNew(0);
  CreateSegment(S,Size,1,Size);
  SIT^[S].HeapNext := S;
  SAT^[S].Heap := True
end { CreateHeap };

procedure DestroyHeap( S: SegmentNumber );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Destroy a Heap or a Segment.
{
{ Parameters:
{       S - The segment number of the root of the Heap.
{
{ Errors:
{       UnusedSegment  if S is not InUse.
{       NotDataSegment  if S is not a DataSegment.
{
{-----------------------------------------------------------------------------}

var FirstS, NextS: SegmentNumber;
begin { DestroyHeap }
  with SAT^[S] do
    if not InUse then raise UnUsedSegment(S)
    else
      if Kind <> DataSegment then raise NotDataSegment(S);
  DecRefCount(S)
end { DestroyHeap }.
