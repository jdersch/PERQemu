{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module UtilProgress;
{-----------------------------------
{
{ Abstract: Progress Reporting Routines
{
{ Copyright (C) 1981, 1983  Three Rivers Computer Corporation
{
{ Abstract:
{    Routines to show progress of utilities.
{
{------------------------------------}

{$Version 1.19 for POS}
{-----------------------------------
{ Change Log: 
{
{  10 Feb 83 1.20  Brad Myers
{  Fixed for landscape monitor.
{
{  31 Jan 83 1.18  Roger Riggs
{  Added null handler for FSNotFnd Exception so that the caller of this
{  will not get an expected exception.

{  16 Nov 82 1.17  Bill Braucher
{  Fixed names for 14-character compiler.

{  18 Jan 82 1.16  Brad Myers
{  Busy cursor bounce off borders.
{  Start progress at top.
{  QuitProgress set function.
{  Normal progress ignore window boundary.
{  Use time to initialize Busy.
{ }

{   6 Jan 82 1.15  Brad Myers
{  Keep busy cursor in window.
{ }

{  29-Dec-81 1.14  Brad Myers
{  Add busy procedures for when busy but don't know how long until done.
{ }

{   2-Dec-81 1.13  John Strait
{  1) Use type Long instead of FSBit32 from Arith.
{  2) Reset default cursor in QuitProgress.
{ }

{  28-Sep-81 1.12  John Strait
{  Use long arithmetic in a new routine so that when the number of steps
{  is larger than 1024 we can show finer grain.
{ }

{  31-Aug-81 1.11  John Strait
{  Show finer grain for Stream files.  This means that UtilProgress now
{  uses long arithmetic.
{ }

{  31-Aug-81 1.10  John Strait
{  Add stuff to show progress reading Stream files.
{ }

{  19-May-81 1.9  Brad Myers
{  New IO module name.
{ }

{  9-Mar-81 1.8  Don Scelza
{  Changed module name and added code to 
{  allow the user to specify the number of scan lines.
{ }

{ 20-Feb-81 1.7  Brad A. Myers 
{ New cursor
{ }

{  2-Jan-81  1.6  Miles A. Barel
{ Initial Creation
{ }

Exports
    Procedure LoadCurs;
    Procedure ShowProgress(NumLines: Integer);
    Procedure QuitProgress;
    Procedure StreamProgress( var F: File );
    Procedure ComputeProgress( Current, Max: Integer );
    Procedure LoadBusy;

Private

Const UtilCursor = 'UtilProgress.Cursor';

Var CursPos:integer;
    busyX : integer; {if -1 then doing regular update}
    xInc, yInc, seed: integer;
    CursMin, CursMax: integer;
    
Imports IO_Others from IO_Others;
Imports System from System;
Imports Stream from Stream;
Imports FileSystem from FileSystem;
Imports RandomNumbers from RandomNumbers;
Imports Screen from Screen;


Procedure LoadCurs;
{------------------------------
{ Abstract:
{    Sets up the cursor before showing progress.
{------------------------------}
  var x,y:integer;
      Curs: CurPatPtr;
      funct: integer;
      win: WinRange;
      hasTitle: boolean;
begin
new(0,4,Curs);
for y:=0 to 63 do
    for x:=0 to 3 do
        Curs^[y,x] := 0;
Curs^[ 2, 0] := #700;
Curs^[ 3, 0] := #3100;
Curs^[ 4, 0] := #4200;
Curs^[ 5, 0] := #10200;
Curs^[ 6, 0] := #61777;
Curs^[ 7, 0] := #121001;
Curs^[ 8, 0] := #121776;
Curs^[ 9, 0] := #121020;
Curs^[10, 0] := #121740;
Curs^[11, 0] := #121040;
Curs^[12, 0] := #121700;
Curs^[13, 0] := #61100;
Curs^[14, 0] := #17600;
IOLoadCursor(Curs,0,0);
Dispose(Curs);
IOCursorMode(IndepCursor);
funct := (DefCursFunct Mod 2)+ord(CTCursCompl); {Don't change screen color}
IOSetFunction(RECAST(Funct, CursFunction));
CursMin := 0;
CursMax := SBitHeight-1;
CursPos:=CursMin;
IOSetCursorPos(SBitWidth-20,CursPos);
busyX := -1;
end { LoadCurs };

Procedure LoadBusy;
{------------------------------
{ Abstract:
{    Sets up the cursor so that we can show that we are busy.  In busy mode,
{    each ShowProgress moves the cursor by one in a random direction.  This
{    should be used when an operation is taking place and the utility cannot
{    tell how long until it is done.
{------------------------------}
  var fid: FileID;
      blks, bits: integer;
      Curs: CurPatPtr;
      funct: integer;
      win: WinRange;
      hasTitle: boolean;
      x: integer;
      l: Double;

Handler FSNotFnd(Name:PathName);
begin
(* Null handler so that caller will not get unexpected exception *)
end;

begin
new(0,4,Curs);
fid := FSLookUp(UtilCursor, blks, bits);
if fid = 0 then LoadCurs
else begin
     FSBlkRead(fid, 0, RECAST(curs, pDirBlk));
     IOLoadCursor(Curs,0,0);
     Dispose(Curs);
     IOCursorMode(IndepCursor);
     funct := (DefCursFunct Mod 2)+ord(CTCursCompl);
     IOSetFunction(RECAST(Funct, CursFunction));
     end;
busyX := SBitWidth - 60;
GetWindowParms(win, x, CursMin, x, CursMax, hasTitle);
CursMin := CursMin - 15;
CursMax := CursMax+CursMin-60;
CursPos:=((CursMax-CursMin) div 2) + CursMin;
IOSetCursorPos(busyX,CursPos);
(***
seed := #13205;
***)
xInc := -2;
yInc := -2;
InitRandom;
IOGetTime(l);
for x := 1 to Abs(l[0]) mod 10 do
  blks := Random;
end { LoadBusy };

Procedure QuitProgress;
{----------------------------------------
{ Abstract:
{    No more progress to report, turn off the cursor.
{
{ Calls:
{    IOCursorMode.
{-----------------------------------------}
begin
IOCursorMode(OffCursor);
IOLoadCursor(DefaultCursor,0,0);
IOSetFunction(RECAST(DefCursFunct, CursFunction));
end;


Function Randm(min, max: integer): integer;
{----------------------------------------
{ Abstract:
{    Produce a random number -1, 0, or 1.
{-----------------------------------------}
  begin
(***
  seed := Abs(seed);
  seed := LXor(seed, Shift(seed, 4));
  seed := LXor(seed, Shift(seed, -11));
  Randm := (LAnd(Seed, #077777) mod (max-min+1)) + min;
****)
  Randm := (LAnd(Random, #077777) mod (max-min+1)) + min;
  end;

Procedure ShowProgress(NumLines: Integer);
{-----------------------------------------
{ Abstract:
{    If started by LoadCurs then Indicate progress by moving the cursor
{    down a certain number of scan lines.  If started by LoadBusy then
{    update busy cursor to show that doing something.
{
{ Parameters:
{    NumLines is the number of scan lines to move the cursor.
{
{ Side Effects:
{    CursPos is modified.
{    BusyX is modified if <> -1.
{
{ Environment:
{     Assumes LoadCurs or LoadBusy has been called.
{
{ Calls:
{    IOSetCursorPos.
{-----------------------------------------}
begin
if busyX = -1 then
   begin
   if CursPos+NumLines > CursMax then CursPos := CursMin;
   CursPos:=CursPos + NumLines;
   IOSetCursorPos(SBitWidth-20,CursPos);
   end
else begin
     if Randm(1, 7) = 1 then
        begin
        yInc := Randm(-4,4);
        xInc := Randm(-4,4);
        end;
     if (busyX + xInc > SBitWidth-60) or (busyX + xInc < 0) then xInc := -xInc;
     if (CursPos+yInc > CursMax) or (CursPos+yInc < 0) then yInc := -yInc;
     CursPos := CursPos + yInc;
     busyX := busyX + xInc; 
     IOSetCursorPos(busyX, CursPos);
     end;
end;


Procedure StreamProgress( var F: File );
{-----------------------------------------
{ Abstract:
{    Indicate progress reading a Stream file.
{
{ Parameters:
{    F is a Stream file which has been Reset.
{
{ Side Effects:
{    CursPos is modified.
{
{ Calls:
{    IOSetCursorPos.
{
{ Errors:
{    NotOpen  if F is not open.
{    NotReset  if F is open but not Reset.
{-----------------------------------------}
var P: record case integer of
         1: (P: ^FileType);
         2: (Offset: Integer;
             Segment: Integer)
         end;
    Bits, Size, Pos: Long;
    T: Integer;
begin
LoadAdr(F);
StorExpr(P.Offset);
StorExpr(P.Segment);
with P.P^, Flag do
  if ReadError <> 0 then
    begin
      if FNotOpen then Raise NotOpen
      else
        if FNotReset then Raise NotReset(StreamName(P.P^))
        else
          if FEof then CursPos := SBitHeight-1
    end
  else
    if LengthInBlocks = 0 then CursPos := 0
    else
      begin
        Bits := Stretch(256 * 16);
        Size := Stretch(LengthInBlocks - 1) * Bits + Stretch(LastBlockLength);
        Pos := Stretch(BlockNumber - 1) * Bits;
        if SizeInWords = 0 then
          begin
            T := SizeInBits { hack needed because of compiler bug };
            Pos := Pos + Stretch(Index * T)
          end
        else Pos := Pos + Stretch(Index * 16);
        CursPos := Shrink(Stretch(SBitHeight) * Pos div Size);
        if CursPos < 0 then CursPos := 0
        else
          if CursPos >= SBitHeight then CursPos := SBitHeight-1
      end;
IOSetCursorPos(SBitWidth-20,CursPos)
end;

Procedure ComputeProgress( Current, Max: Integer );
{-----------------------------------------
{ Abstract:
{    Indicate progress given a current and maximum value.
{
{ Parameters:
{    Current is the current value.
{    Max is the maximum value.
{
{ Side Effects:
{    CursPos is modified.
{
{ Calls:
{    IOSetCursorPos.
{-----------------------------------------}
begin
if Max <= 0 then CursPos := 0
else
  begin
    CursPos := Shrink(Stretch(SBitHeight-1) * Stretch(Current) div
                      Stretch(Max));
    if CursPos < 0 then CursPos := 0
    else
      if CursPos >= SBitHeight then CursPos := SBitHeight-1
  end;
IOSetCursorPos(SBitWidth-20,CursPos)
end.
