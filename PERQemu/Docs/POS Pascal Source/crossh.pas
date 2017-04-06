{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program Cross;


{
{  copyright 1981, 1982, 1983  Three Rivers computer Corporation
{
{---------------------------------------------------------------------------
{ Modification History
{
{ 24 Feb 83  V0.3 Brad Myers
{            Fixed for landscape screen.
{
{ 25 Jun-81  V0.2 Diana Connan Forgy
{            Changed imports IO to imports IO_Unit
{            for compatability with POS D.
{
{ 3 -Apr-81  V0.1 Diana Connan Forgy
{            Converted to run under POS C.
{
{----------------------------------------------------------------------------}


imports Memory from Memory;
imports Screen from Screen;
imports Raster from Raster;
imports IO_Unit from IO_Unit;
imports IOErrors from IOErrors;

var X1,X2,Y1,Y2,XSpace,YSpace: Integer;
    UseXor: boolean;
    SDum: String;
    Ch: Char;
    
begin
Reset(Input);
ReWrite(Output);

Write('X spacing: ');
ReadLn(XSpace);
Write('Y spacing: ');
ReadLn(YSpace);
Write('Use XOR for the "Y" lines: [no] ');
ReadLn(SDum);
UseXor := false;
if sdum <> '' then
  if SDum[1] in ['y','Y'] then UseXor := true;


RasterOp(RXOr,SBitWidth, SBitHeight,0,0,SScreenW,SScreenP,0,0,SScreenW,SScreenP);

X1 := 0;
X2 := 0;
Y1 := 0;
Y2 := SBitHeight-1;

repeat
    Line(DrawLine,X1,Y1,X2,Y2,SScreenP);
    X1 := X1 + XSpace;
    X2 := X2 + XSpace;
until (X1 > SBitWidth-1);


X1 := 0;
X2 := SBitWidth-1;
Y1 := 0;
Y2 := 0;

repeat
    if UseXor then
        Line(XOrLine,X1,Y1,X2,Y2,SScreenP)
    else
        Line(DrawLine,X1,Y1,X2,Y2,SScreenP);
    Y1 := Y1 + YSpace;
    Y2 := Y2 + YSpace;
until (Y1 > SBitHeight-1);

while true do
    begin
    if IOCRead(TransKey,Ch) = IOEIOC Then
        RasterOP(RNot,SBitWidth,SBitHeight,0,0,SScreenW,SScreenP,0,0,SScreenW,SScreenP);
    end;


end.


