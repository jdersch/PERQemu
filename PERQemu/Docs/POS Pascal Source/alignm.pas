{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module AlignMemory;

{------------------------------------------------------------------------
{
{ AlignMemory - Allocated aligned buffers.
{ J. P. Strait  29 Sep 81.
{ Copyright (C), 1981, 1982, 1983 Three Rivers Computer Corporation.
{
{ Abstract:
{   This module is used to allocate buffers which need to be aligned on
{   boundaries that are multiples of 256 words.
{
{------------------------------------------------------------------------}

{------------------------------------------------------------------------
{
{ Change history:
{
{ 30 Jun 82  V1.2  D. Scelza
{ Changed to set segment unswappable first.  This
{ will keep the segment from being swapped to be moved
{ to high memory.
{
{ 20 Oct 81  V1.1  D. Scelza
{ Fixed a bug in the alignment procedure.
{
{ 29 Sep 81  V1.0  J. Strait
{ Start file.
{
{------------------------------------------------------------------------}

exports


  type AlignedBuffer = array[0..0] of array[0..255] of Integer;
       AlignedPointer = ^AlignedBuffer;


  procedure NewBuffer( var P: AlignedPointer; S, A: Integer );
  exception BadAlignment( A: Integer );

private

imports Memory from Memory;

procedure NewBuffer( var P: AlignedPointer; S, A: Integer );
{------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to allocate buffers which need to be aligned on
{   boundaries that are multiples of 256 words.  A new segment is allocated
{   which is somewhat larger than the desired buffer size.  The segment is
{   set to be unmovable so that the alignment can be guaranteed.
{
{ Parameters:
{   P - Set to point to a new buffer which is aligned as desired.
{   S - Desired size of the buffer in 256 word blocks.
{   A - Alignment in 256 word blocks.  That is, 1 means aligned on a 256
{       word boundary, 2 means a 512 word boundary, and so on.
{
{ Errors:
{   BadAlignment  if A is less than one or greater than 256.
{   BadSize  (memory manager)  if S is less than one or S+A-1 is greater
{            than 256.
{   Other memory manager exceptions raised by CreateSegment.
{
{------------------------------------------------------------------------}

var Seg: SegmentNumber; 
    Wasted: Integer;
begin { NewBuffer }
  if (A <= 0) or (A > 256) then raise BadAlignment(A);
  if (S <= 0) or (S+A-1 > 256) then raise BadSize(0,S);
  CreateSegment(Seg,S+A-1,1,S+A-1);
  SetMobility(Seg,UnSwappable);
  SetMobility(Seg,UnMovable);
  {$R-}
  with SAT^[Seg] do
    Wasted := A - (Shift(BaseUpper,8) + BaseLower) mod A;
  {$R=}
  if Wasted = A then Wasted := 0;
  ChangeSize(Seg,S+Wasted);
  P := MakePtr(Seg,Shift(Wasted,8), AlignedPointer)
end { NewBuffer }.
