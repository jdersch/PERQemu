{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module MoveMem;
{-----------------------------------------------------------------------------
{
{       MoveMem - Move memory.
{       J. P. Strait   ca. 1 Jan 80.
{       Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1982, 1983.
{
{ Abstract:
{       MoveMem is used to move a segment from one location to another in
{       physical memory.  The two locations may overlap.
{
{-----------------------------------------------------------------------------}


{-----------------------------------------------------------------------------
{ Change Log:
{
{  7 Jan 82 V1.8 Brad Myers
{      Change IOKeyDisable ... to IntOn off.
{
{  4 Jan 82 V1.7 WJHansen
{      Don't call IOGetTime or IOKeyXXXAble if system not initialized
{
{ 10 Dec 81 V1.6  WJHansen
{    change DoubleWORD AND xxTime to long; remove Arith
{
{ 9 Jul 81  V1.5  JPS
{ Use IOKeyDisable and IOKeyEnable rather than INTOFF and INTON.
{ }

{ 18 May 81  V1.4  BAM
{ Change import from IO to IO_Others.
{ }

{ 18 Apr 81  V1.3  JPS
{ Gather statistics.
{ }

{ 24 Mar 81  V1.2  JPS
{ Convert to standard documentation form.
{ }

{ 17 Feb 81  V1.1  JPS
{ Set R- in private part.
{ }
{-----------------------------------------------------------------------------}

exports


imports Memory from Memory;


procedure CopySegment( SrcSeg, DstSeg: SegmentNumber; NewDstBase: Integer );


private


imports Raster from Raster;
imports System from System;
imports IO_Others from IO_Others;


{$R-}

procedure CopySegment( SrcSeg, DstSeg: SegmentNumber; NewDstBase: Integer );
{------------------------------------------------------------------------------
{
{ Abstract:
{       CopySegment is used to move a segment from one location to another in
{       physical memory.
{
{ Parameters:
{       SrcSeg - Number of the segment which represents the source address and
{                source size.
{       DstSeg - Number of the segment which represents the destination
{                address.
{       NewDstBase - New value of the base address for DstSeg.
{
{ Result:
{       Base address of SrcSeg - set to the old base address of DstSeg.
{       Base address of DstSeg - set the NewDstBase.
{
{ Design:
{       CopySegment moves segments without swapping them, and is designed
{       in such a way that it may move itself or its own stack.  In
{       order to be able to do this, CopySegment is in a code segment by
{       itself so that any call is guaranteed to be a cross-segment call.
{       Thus when the move operation is complete, a cross segment return
{       is done, and the CodeBase micro-code register is reloaded.  Movemem
{       executes a special StartIO instruction to cause the micro-code to
{       reload its StackBase register.  This code segment must never exceed
{       256 words in length so that when CopySegment moves itself, the new
{       copy cannot overlap the old one.
{
{       CopySegment uses RasterOp to copy the memory in order that the copy
{       be done as an indivisible operation.
{
{       DO NOT CHANGE this routine unless you fully understand the 
{       entire system.
{
{-----------------------------------------------------------------------------}

var Size: Integer;
    SrcBase, DstBase: Integer;
    {$ifc SysTiming then}
    StartTime, EndTime: record case integer of
                          1: (D: Double);
                          2: (DW: long);
                          end;
    {$endc}
begin { CopySegment }
{$ifc SysTiming then}
  if SystemInitialized then IOGetTime(StartTime.D);   
{$endc}
 with SAT^[SrcSeg] do
  SrcBase := Shift(BaseUpper,8) + BaseLower;
 with SAT^[DstSeg] do
  DstBase := Shift(BaseUpper,8) + BaseLower;
 Size := SAT^[SrcSeg].Size + 1;
 InLineByte( #151 {INTOFF} );           { shut off interrupts }
 with SAT^[SrcSeg] do
  begin BaseUpper := Shift(DstBase,-8);
   BaseLower := LAnd(DstBase,#377)
  end;
 with SAT^[DstSeg] do
  begin BaseUpper := Shift(NewDstBase,-8);
   BaseLower := LAnd(NewDstBase,#377)
  end;
 RasterOp(RRpl,                         { raster-op function }
          512,                          { width }
          Size * 8,                     { height }
          0,                            { destination X }
          DstBase * 8,                  { destination Y }
          32,                           { destination scan line length }
          SAT,                          { destination base address }
          0,                            { source X }
          SrcBase * 8,                  { source Y }
          32,                           { source scan line length }
          SAT);                         { source base address }
 StartIO(SetStkBase);                   { in case we moved the stack }
 if SystemInitialized then begin
    InLineByte( #152 {INTON} );            { interrupts are OK now }
{$ifc SysTiming then}
    IOGetTime(EndTime.D);   
    MoveTime := (EndTime.DW - StartTime.DW) + MoveTime;
{$endc}
    end;   
end { CopySegment }.
