{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program Chars;


{------------
{
{     Chars - Write characters to screen.
{     John P. Strait.
{     Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{     Chars writes an arbitrary character string to the PERQ display at all
{ possible quad-word alignments.  A press on the tablet magnifies the bits
{ that are written on the screen by 8 in x and y.  This is used to investigate
{ Raster-Op problems.
{
{------------}

{------------
{
{ 21 Aug 81  JPS  V1.1  Add copyright notice and change history.
{                       Convert to POS D.5.
{
{------------}

 imports Screen from Screen;
 imports IO_Others from IO_Others;
 imports Memory from Memory;

{$R-  We're gonna treat the screen as a big array. }

const  FontLength = 6 * #400;
 
 
type ScanLine = array[0..47] of packed array[-15..0] of Boolean;
     ScreenArray = array[0..0] of ScanLine;
     pScreenArray = ^ScreenArray;
     Phont = array[1..FontLength] of integer;
     pFont = ^Phont;
     
var S: String;
    L, Pos, D, LastD, X, Y, Func, SeeX, SeeY, I, J: Integer;
    Scrn: pScreenArray;
    Part: packed array[0..63,0..63] of Boolean;
    Ch: Char;
    ActualFont, CopyFont: pFont;

 procedure Blot(X,Y: Integer);
 var I, J: Integer;
 begin { Blot }
  for I := 2 to 6 do
   for J := 2 to 6 do
    Scrn^[#200 + Y * #10 + I]
         [(#200 + X * #10 + J) div #20]
         [-LAnd(#200 + X * #10 + J,#17)] := true
 end { Blot };
 
begin { Chars }
 Reset(Input,'Console:');
 Rewrite(Output,'Console:');
 Write('Raster-Op function: '); Readln(Func); SChrFunc(Func);
 IOSetModeTablet(RelTablet);
 IOCursorMode(TrackCursor);
 Scrn := Recast(SScreenP,pScreenArray);
 ActualFont := MakePtr(FontSeg,0,pFont);
 CopyFont := MakePtr(ScreenSeg,#140000 - FontLength - #404 - #3000,pFont);
 repeat
  ChangeTitle('Press the title line to type a string, press below to magnify');
  CopyFont^ := ActualFont^;
  repeat until TabSwitch;
  IOReadTablet(SeeX,SeeY);
  if SeeY <= #16 then
   begin
    Write(Chr(12));
    SSetCursor(#100,#100); Write('string: ');
    Readln(S);
    X := #100;
    Y := #140;
    if Length(S) <> 0 then
     repeat SSetCursor(10,Y); Write(X:4:8);
      SChrFunc(Func);
      SSetCursor(X,Y); Write(S);
      X := X + 1;
      Y := Y + #15
     until X = #200
   end
  else
   begin
    SeeY := SeeY - 32;
    SeeX := SeeX - 32;
    for Y := SeeY to SeeY + 63 do
     for X := SeeX to SeeX + 63 do
      Part[X - SeeX,Y - SeeY] := Scrn^[Y][Shift(X,-4)][-LAnd(X,#17)];
    Write(Chr(12));
    CopyFont^ := ActualFont^;
    X := #100 - SeeX mod #100;  if X = #100 then X := 0;
    Y := #100 - SeeY mod #100;  if Y = #100 then Y := 0;
    Blot(X,-2);
    if X = 0 then Blot(X + #100,-2);
    Blot(-2,Y);
    if Y = 0 then Blot(Y + #100,-2);
    X := #20 - SeeX mod #20;  if X = #20 then X := 0;
    Y := #20 - SeeY mod #20;  if Y = #20 then Y := 0;
    repeat Blot(X,-1);
     Line(DrawLine,#200 + X * #10,#200,#200 + X * #10,#1200,
          Recast(Scrn,RasterPtr));
     X := X + #20
    until X > #100;
    repeat Blot(-1,Y);
     Line(DrawLine,#200,#200 + Y * #10,#1200,#200 + Y * #10,
          Recast(Scrn,RasterPtr));
     Y := Y + #20
    until Y > #100;
    for Y := 0 to 63 do
     for X := 0 to 63 do
      if Part[X,Y] then
       Blot(X,Y)
   end
 until False
end { Chars }.
