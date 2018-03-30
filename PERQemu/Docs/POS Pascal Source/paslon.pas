{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module PasLong;


{-----------------------------------------------------------------------------
{
{       PasLong - Extra stream package input conversion routines.
{       J. P. Strait & Michael R. Kristofic  ca. 15 Sep 81.
{       Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{ 
{ Abstract:
{       PasLong is the extra character input module of the Stream package.
{       Its routines are called by code generated by the Pascal compiler 
{       in response to variations on Read, Readln, Write and Writeln 
{       statements. It is one level above Module Stream and uses Stream's
{       lower-level input routines.
{
{-----------------------------------------------------------------------------}


{ 15 Sep 81  V2.2 John Strait and Mike Kristofic.
{ Creation - Double precision read and write routine ReadD and WriteD.
{ }


exports
 

imports Stream from Stream;
 
 
 procedure ReadD( var F: FileType; var X: long; B: integer );
 procedure WriteD( var F: FileType; X : long; Field, B: integer );


private

{$R-  shut off range checks }


const Tab = Chr(#11);
      FF = Chr(#14);
      
procedure ReadD( var F: FileType; var X: long; B: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Reads an double integer in free format with base B.   B may be
{       any integer between 2 and 36, inclusive.
{
{ Parameters:
{       X - the double to be read.
{       F - the file from which X is to be read.
{       B - the base of X.   It may be any integer between 2 and 36,
{           inclusive.  If B is less than zero and the user does not
{           type an explicit plus or minus sign, X is read as an
{           unsigned number.
{
{ Errors:
{       PastEof  -if an attempt is made to read F past the Eof.
{       NotNumber - if non-numeric input is encountered in the file.
{       LargeNumber - if the number is not in the range -2^31..2^32-1.
{       BadBase - if the base is not in 2..36.
{
{ Design:
{       Number is read into the low order word of two double precision
{       integers to avoid overflow.
{
{-----------------------------------------------------------------------------}

const
    DigitOffset = -1 + Ord('0');
    LegalChOffset = -11 + Ord('A');
    CaseDiff = -Ord('a') + Ord('A');
    AlphaOffset = -Ord('A') + 10;
var Ch: char;
    S: (Unsigned, Negative, Positive);
    C: set of '0'..'Z';  { Legal characters for base B. }
    Large: boolean;      { True if the number read exceeds the valid range. }
    DB,
    Lower,    { Low order double precsion word. }
    Upper,    { High order double precsion word. }
    T, 
    XX: record case Integer of
                                 1: (D: long);
                                 2: (Lower: Integer;
                                     Upper: Integer)
                             end;
begin { ReadD }

  if B < 0 then { read unsigned }
    begin 
      S := Unsigned;
      B := -B
    end
  else S := Positive;

  if not (B in [2..36]) then raise BadBase(StreamName(F), B);
  DB.D := B;

  with F, Flag do
    begin
      if not CharReady then GetC(F);
      if FEof then raise PastEof(StreamName(F));
      while (Element.C in [' ', Tab, FF]) and not FEof do   { Skip to 1st }
        begin                                               { interesting }  
          CharReady := False;                               { character.  }
          GetC(F)
        end;
      if not FEof then
        begin 
          Large := false;
          if Element.C = '+' then { unary + }
            begin 
              S := Positive;
              CharReady := False;
              GetC(F)
            end
          else
            if Element.C = '-' then { unary - }
              begin     
                S := Negative;
                CharReady := False;
                GetC(F)
              end;
              
          if B < 11 then C := ['0'..Chr(B + DigitOffset)]   { Just digits. }
          else C := ['0'..'9','A'..Chr(B + LegalChOffset)]; { Digits, letters.}
              
          if Element.C in ['a'..'z'] then Ch := Chr(Ord(Element.C) + CaseDiff)
                                     else Ch := Element.C;  { Uppercase only. }

          if not (Ch in C) then raise NotNumber(StreamName(F));
          Upper.D := 0;
          Lower.D := 0;

          repeat 
            CharReady := False;
            GetC(F);
            if Ch in ['0'..'9'] then T.D := Ord(Ch) - Ord('0')
                                else T.D := Ord(Ch) + AlphaOffset;
            Lower.D := Lower.D * DB.D + T.D;
            T.D := Lower.Upper;
            Lower.Upper := 0;
            Upper.D := Upper.D * DB.D + T.D;
            case S of
              Unsigned: Large := Large or (Upper.Upper <> 0);
              Positive: Large := Large or (LAnd(Upper.Lower,#100000) <> 0);
              Negative: Large := Large or
                                         ((LAnd(Upper.Lower,#100000) <> 0) and
                                         {(LAnd(Upper.Lower,#077777)<> 0)  and}
                                         ((Upper.Lower <> #100000)  or
                                         (Lower.Lower <> 0)))
            end;
            if Element.C in ['a'..'z'] then
                 Ch := Chr(Ord(Element.C) + CaseDiff)      { Uppercase only }
            else Ch := Element.C
          until not (Ch in C);
          
          if Large then raise LargeNumber(StreamName(F));
          XX.Upper := Upper.Lower;
          XX.Lower := Lower.Lower;
          if S = Negative then X := -XX.D
                          else X := XX.D
        end
    end
end { ReadD };

procedure WriteD( var F: FileType; X: long; Field, B: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Writes an double integer in fixed format with base B.
{
{ Parameters:
{       X - the double to be written.
{       F - the file into which X is to be written.
{       Field - the size of the field into which X is to be written.
{       B - the base of X.  It is an integer whose absolute value must
{           be between 2 and 36, inclusive.  If B is less than zero,
{           X is written as an unsigned number.
{
{ Errors:
{       BadBase  -if the base is not in 2..36.
{
{ Design:
{       Value written from two double precision words to avoid overflow.
{
{-----------------------------------------------------------------------------}


{$R-}

const
    AlphaOffset = Ord('A') - 10;
var Ch: char;
    D: array[1..35] of char;  { Character version of the number built here. }
    Negative: boolean;        { True if X < 0 }
    DB, 
    Lower,    { Low order word } 
    Upper,    { High order word } 
    T, 
    XX: record case Integer of
                            1: (D: long);
                            2: (Lower: Integer;
                                Upper: Integer)
                            end;
     N,          { Index for D. } 
     K: integer; { The next digit. }
     
begin { WriteD }
     
  if not (Abs(B) in [2..36]) then raise BadBase(StreamName(F), B);
  Negative := false;
  XX.D := X;
  if B < 0 then { write unsigned number } B := -B
  else { write signed number }
    if XX.Upper < 0 then
      begin Negative := true; XX.D := -XX.D end;
  DB.D := B;
  Upper.Lower := XX.Upper;
  Upper.Upper := 0;
  Lower.Lower := XX.Lower;
  Lower.Upper := 0;
  N := 0;
     
  repeat 
    N := N + 1;
    T.Lower := 0;
    T.Upper := Shrink(Upper.D mod DB.D);
    Upper.D := Upper.D div DB.D;
    T.D := Lower.D + T.D;
    Lower.D := T.D div DB.D;
    K := Shrink(T.D mod DB.D);
    if K >= 10 then D[N] := Chr(K + AlphaOffset)
               else D[N] := Chr(K + Ord('0'))
  until (Upper.D = 0) and (Lower.D = 0);
     
  if Negative then
   begin N := N + 1; D[N] := '-' end;
  for K := N + 1 to Field do
   begin F.Element.C := ' '; PutC(F) end;   { Fill field with blanks. }
  for K := N downto 1 do
   begin F.Element.C := D[K]; PutC(F) end   { Write number. }
     
end { WriteD }.