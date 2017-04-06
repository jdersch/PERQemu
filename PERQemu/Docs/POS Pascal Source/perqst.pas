{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module PERQ_String;
{-----------------------------------------------------------------------------
{
{ PERQ String hacking routines.
{ Written by: Donald Scelza
{ Copyright (C) 1980, 1981, 1982, 1983  Three Rivers Computer Corporation
{
{
{ Abstract:
{    This module implements the string hacking routines for the
{    Three River PERQ Pascal.
{
{---------------------------------------------------------------------------}


{{$Version V2.10}
{---------------------------------------------------------------------------
{
{ Change Log:
{
{  5 Mar 82 V2.10 Ed Frankenberry
{ Added function Upper and Debug compilation constant.
{
{ 24 Feb 82 V2.9  Ed Frankenberry
{ Added Pad function to return fixed length numeric string.
{
{ 19 Feb 82 V2.8  Ed Frankenberry
{ Added IntToStr function and procedure PrependChar.
{
{ 10 Feb 82  V2.7  David Golub
{ Fix spelling errors.
{
{ 08 Feb 82 V2.6  J Conner
{ Format headers for MPOS
{
{ 18 Dec 81  V2.5 J. Strait
{ Use StrLong from Except rather than exporting a duplicate definition.
{
{ 08 Dec 81  V2.4 Don scelza
{ Changed for use in the new O.S.
{
{ 20 May 81  V2.3 JE Ball (CMU) and Brad Myers
{ Rewrote copy loop in Insert
{ Added new function: RevPosC
{
{ 13-May-81  V2.2 John Strait
{ Use exceptions.
{
{ 19-Jan-81  V2.1 Diana Connan Forgy
{ Changed ConCat and SubStr to functions.
{
{ 16-Dec-80  V2.0 Brad A. Myers
{ Added New procedures: PosC, AppendString, AppendChar, UpperCase, ConvUpper.
{
{ 27-Nov-80 V1.1  Don Scelza
{ Fixed a bug in SubStr.  Added calls to UserError for errors.
{ (Not released)
{
{ 30-Apr-80 V1.0 Don Scelza
{ Changed ConCat and SubStr to use temp strings for their hacking.
{ This will allow a use to pass the same string as input and result
{ parameters.
{
{ 08-Apr-80 V0.0  Don Scelza
{ Created the string hacking module.
{
{-----------------------------------------------------------------------}

{-------------------------------------------------------------------------
{
{ Strings in PERQ Pascal are stored a single character per byte with
{ the byte indexed by 0 being the length of the string.  When the routines
{ in this module must access the length byte they must turn off range
{ checking.
{
{------------------------------------------------------------------------}

{********************}  Exports  {********************}


Const MaxPStringSize=255;          { Length of strings}
Type PString = String[MaxPStringSize];

Procedure Adjust(Var STR: PString; LEN:Integer);
Function  Concat(Str1, Str2: PString): PString;
Function  Substr(Source: PString;  Index, Size: Integer): PString;
Procedure Delete(Var Str: PString;  Index, Size: Integer);
Procedure Insert(Var Source, Dest: PString; Index: Integer);
Function  Pos(Source, Mask: PString): Integer;

FUNCTION  PosC(s: PString; c: Char): Integer;
PROCEDURE AppendString(var s1: PString; s2: PString);
PROCEDURE AppendChar(var s: PString; c: Char);
FUNCTION  UpperCase(c: Char): Char;
PROCEDURE ConvUpper(Var s: PString);

Exception StrBadParm;
{-------------------------------------------------------------------------
 Abstract: Raised when bad index or length parameters passed to procedures or
           sometimes when string will be too long (other times, StrLong is
           raised in this case
-------------------------------------------------------------------------}

Function  RevPosC(s: PString; c: char): integer;
Procedure PrependChar (c: char; var s: PString);
function  IntToStr (N: integer): PString;
function  Pad (PadCh: char; str: PString; len: integer): PString;
function  Upper (s: string): PString;


{********************}  Private  {********************}

imports Except from Except;

const Debug = False;


{$R-}



Procedure Adjust(var Str:PString; Len:Integer);
{--------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to change the dynamic length of a string.
{
{ Parameters:
{    Str is the string that is to have the length changed.
{
{    Len is the new length of the string.  This parameter must be
{    This value must be no greater than MaxPStringSize.
{
{ Environment:
{    None
{
{ Results:
{    This procedure does not return a value.
{
{ Side Effects:
{    This procedure will change the dynamic length of Str.
{
{ Errors:
{    If Len > MaxPStringSize then raise StrLong exception.
{
{ Design:
{    Simple.
{----------------------------------------------------------------------}
    Begin
    If (Len>MaxPStringSize) Then Raise StrLong;
    Str[0]:=Chr(Len);
    End {Adjust};



Function Concat(Str1,Str2:PString):PString;
{--------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to concatenate two string together.
{
{ Parameters:
{    Str1 and Str2 are the two strings that are to be concatenated.
{
{ Environment:
{    None
{
{ Results:
     This function will return a single string as described by the 
{    parameters.
{
{ Errors:
{    If Length(Str1) + Length(Str2) is greater then MaxPStringSize then
{      raise StrLong exception.
{
{ Design:
{ ------------------------------------------------------------------------}
    Var
        Leng,Indx,I: Integer;
        Temp:PString;
    
    Begin
    
{ 
{ Check the lengths of the strings.  If they are too long then
{ generate a runtime error.
{ }

    Leng:=Length(Str1)+Length(Str2);
    If Leng > MaxPStringSize Then Raise StrLong;
    
{
{ Now make the new string.
{ }

    Temp:=Str1;
    Indx:=Length(Str1)+1;
    For I:=1 To Length(Str2) Do
        Begin
        Temp[Indx]:=Str2[I];
        Indx:=Indx+1;
        End;
    
{
{ Now set the size of the string.
{ }

    Temp[0]:=Chr(Leng);
    Concat:=Temp;
    End {Concat};
    

Function SubStr(Source:PString; Index, Size:Integer):PString;
{-------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to return a sub portion of the string passed
{    as a parameter.
{
{ Parameters:
{    Source is the string that we are to take a portion of.
{
{    Index is the starting position in Source of the substring.
{
{    Size is the size of the substring that we are to take.
{
{ Environment:
{    None
{
{ Results:
{    This function returns a substring as described by the parameter list.
{
{ Errors:
{    If Index or Size are greater than MaxPStringSize then raise
{       StrBadParm exception.
{
{ Design:
{--------------------------------------------------------------------------}
    Var
        I,StrEnd: Integer;
        Temp:PString;
    
    Begin
    
{
{ Check the parameters.
{ }

    If (Index>MaxPStringSize) Or (Size>MaxPStringSize) Then Raise StrBadParm;
    
{
{ Now make up the substring.
{ }

    If Index>Length(Source) Then
        Begin
        Temp:='';
        Temp[0]:=Chr(0);
        End
    Else
        Begin
        If Length(Source) < (Index+Size) Then
            Size:=Length(Source)-Index+1;
        For I:=1 To Size Do
            Begin
            Temp[I]:=Source[Index+I-1];
            End;
        Temp[0]:=Chr(Size);
        End;
     SubStr:=Temp;

   End {Substr};
    
    


Procedure Delete(var Str:PString; Index, Size:Integer);
{-----------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to remove characters from a string.
{
{ Parameters:
{    Str is the string that is to be changed.  Characters will be removed from
{    this string.
{
{    Index is the starting position for the delete.
{
{    Size is the number of character that are to be removed.  Size characters
{    will be removed from Str starting at Index.
{
{ Environment:
{    None
{
{ Results:
{    This procedure does not return a value.
{
{ Side Effects:
{    This procedure will change Str.
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
    Var
        I,Offset: Integer;
        
    Begin
    
{
{ Check to see if he gave an index that was out of range.
{ }

    If Length(Str) < Index Then Exit(Delete);
    
    
    Offset:=0;
    For I:=(Index+Size) To Length(Str) Do
        Begin
        Str[Index+Offset]:=Str[I];
        Offset:=Offset+1;
        End;

    Str[0]:=Chr(Index+Offset-1)
    
    
    End {Delete};
    



Procedure Insert(var Source, Dest:PString; Index:Integer);
{-----------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to insert a string into the middle of another
{    string.
{
{ Parameters:
{    Source is the string that is to be inserted.
{
{    Dest is the string into which the inseration is to be made.
{
{    Index is the starting position, in Dest, for the inseration.
{
{ Environment:
{    None
{
{ Results:
{    This procedure does not return a value.
{
{ Side Effects:
{    This procedure will insert Source in Dest starting at location Index.
{
{ Errors:
{    If the resulting string is too long then generate a runtime error.
{
{ Design:
{----------------------------------------------------------------------------}
    Var
        Offset,Leng,LSource: Integer;
        Temp: Char;
        
    Begin
    LSource:=Length(Source);
    Leng:=Length(Dest)+LSource;

{
{ Check to see if the string is going to be too large.
{ }

    If Leng > MaxPStringSize Then Raise StrBadParm;
    
{
{ Check to see if Index is larger than the length of the string.
{ If so then set it to the end of the string.
{ }

    If Index > Length(Dest) Then Index:= Length(Dest)+1;


{
{ Make the insertions.
{ FIXED jeb 5/4/81
{ }
{**    For Offset:= 0 To LSource-1 Do
        Begin
        Temp:=Dest[Index+Offset];
        Dest[Index+Offset]:=Source[Offset+1];
        Dest[Index+Offset+LSource]:=Temp;
        End;
 **}
 
    for Offset := length(Dest) downto index do
      dest[Offset+LSource] := dest[Offset];
      
    for Offset := 1 to LSource do
      dest[Index+Offset-1] := Source[Offset];
      
{
{ Adjust the length.
{ }

    Dest[0]:=Chr(Leng);
    
    End {Insert};
    

PROCEDURE AppendString(var s1: PString; s2: PString);
{-----------------------------------------------------------------------------
{  Abstract: puts s2 on the end of s1
{  
{  Parameters : s1 is the left String and s2 goes on the end.
{
{  Calls: PerqString.Concat.
{
{  SideEffects : modifies s1.
{           
-----------------------------------------------------------------------------}
   begin
   s1 := Concat(s1,s2);
   end;

PROCEDURE AppendChar(var s: PString; c: Char);
{-----------------------------------------------------------------------------
{  Abstract: puts c on the end of s
{  
{  Parameters : s is the left String and c goes on the end.
{
{  SideEffects : modifies s.
{           
{-----------------------------------------------------------------------------}
   var len: Integer;
   begin
   len := Length(s);
   Adjust(s, len+1);
   s[len+1] := c;
   end;

FUNCTION UpperCase(c: Char): Char;
{-----------------------------------------------------------------------------
{  Abstract: Changes c to uppercase if letter.
{  
{  Parameters: c is any char.
{
{  Returns: char is uppercase if letter otherwise unchanged.
{
{-----------------------------------------------------------------------------}
   const delta = ord('a')-ord('A');
   begin
   if c IN ['a'..'z'] then UpperCase := chr(ord(c)-delta)
   else UpperCase := c;
   end; {UpperCase}

Procedure ConvUpper(Var s: PString);
{------------------------------------------------------------------------
{   Abstract: Converts s to all upper case
{
{   Parameters: s, passed by reference, to be converted 
{-------------------------------------------------------------------------}
    Var i:Integer;
    const delta = ord('a')-ord('A');
    Begin
    For i:=1 To Length(s) Do
        if s[i] in ['a'..'z'] Then
                s[i]:=Chr(Ord(s[i]) - delta);
    End {ConvUpper};

Function PosC(s: PString; c: char): integer;
{-----------------------------------------------------------------------------
{  Abstract: Tests if c is a member of s.
{  
{  Parameters: c is any char; s is string to test for c member of.
{
{  Returns: index of first c in s (from beginning of string) or zero if not
{             there.
{
{-----------------------------------------------------------------------------}
   var i : integer;
   begin
   for i := 1 to length(s) do
      if s[i] = c then begin
                       PosC := i;
                       exit(PosC);
                       end;
   PosC := 0;
   end; {PosC}

Function RevPosC(s: PString; c: char): integer;
{-----------------------------------------------------------------------------
{  Abstract: Tests if c is a member of s.
{  
{  Parameters: c is any char; s is string to test for c member of.
{
{  Returns: index of first c in s (from end of string) or zero if not there.
{
{-----------------------------------------------------------------------------}
   var i : integer;
   begin
   for i := length(s) downto 1 do
      if s[i] = c then begin
                       RevPosC := i;
                       exit(RevPosC);
                       end;
   RevPosC := 0;
   end; {RevPosC}

Function Pos(Source, Mask:PString):Integer;
{----------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to find the position of a pattern in a
{    given string.
{
{ Parameters:
{    Source is the string that is to be searched.
{
{    Mask is the pattern that we are looking for.
{
{ Environment:
{    None
{
{ Results:
{    If Mask occured in Source then the index into Source of the first
{    character of Mask will be returned.  If Mask was not found then
{    return 0.
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
    Label 1;
    Var
        MaskLength,Indx,I: Integer;
        
    Begin
    MaskLength:=Length(Mask);
    Pos:=0;
    
{
{ Scan for the first character of Mask in Source.
{ }
    
    For I:= 1 To (Length(Source)-MaskLength+1) Do
        Begin
        Indx:=1;
        
{
{ Look for the pattern.
{ }

        While (Source[I+Indx-1]=Mask[Indx]) And (Indx <= MaskLength) Do
            Begin
            Indx:=Indx+1;
            End;
        
{
{ See if we matched the entire mask.
{ }

        If (Indx-1)=MaskLength Then
            Begin
            Pos:=I;
            GoTo 1;
            End;
        End;
    
    1:
    End {Pos};


Procedure PrependChar (c: char; var s: PString);
{-------------------------------------------
{  Abstract:
{    puts c at the beginning of s
{           
{  Parameters:
{    s is the original String and c goes on the front.
{           
{  Side-Effects:
{    modifies s if c is not Nul.
{
{  Calls:
{    Adjust (from PERQ_String).
{-------------------------------------------}
const NUL = chr(#000);
var len, i: integer;
begin
{$IFC Debug THEN }
   writeln('[PrependChar.]');
{$ENDC }
   len := Length(s);
   if c <> NUL then begin
        Adjust(s, len + 1);
     (* Insert(dummy, s, 2);     { insert wouldn't work correctly here } *)
        for i := len + 1 downto 2 do
           s[i] := s[i - 1];     { copy s shifted one position }
        s[1] := c
      end   { then }
end;   { PrependChar }

function IntToStr (N: integer): PString;
{----------------------------------------------------------------
{ Abstract:
{    Converts the integer N to a string.
{
{ Calls:
{    PrependChar.
 ----------------------------------------------------------------}
var ch: char;
    digit, dummy: integer;
    str: PString;
begin
{$IFC Debug THEN }
   writeln('[IntToStr.]');
{$ENDC }
   dummy := abs(N);
   if dummy = 0 then str := '0' else str := '';
   while dummy <> 0 do begin
      digit := dummy mod 10;
      ch := chr(digit + ord('0'));
      PrependChar(ch, str);
      dummy := dummy div 10
    end;   { while }
   if N < 0 then PrependChar('-', str);
   IntToStr := str
end;   { IntToStr }


function Pad (PadCh: char; str: PString; len: integer): PString;
{----------------------------------------------------------------
{ Abstract:
{    Forces a string to a certain length using PadCh to left justify.
{
{ Returns:
{    The longer string (preceeded by PadCh's).
{
{ Calls:
{    PrependChar.
 ----------------------------------------------------------------}
begin
{$IFC Debug THEN }
   writeln('[Pad.]');
{$ENDC }
   while (length(str) < len) and (length(str) < MAXPSTRINGSIZE) do
      PrependChar(PadCh, Str);
   Pad := Str
end;   { Pad }


function Upper (s: string): PString;
{------------------------------------------------------
{ Abstract:
{    Makes the ConvUpper procedure (of CmdParse) into
{    a function.  Makes s into all capital letters.
{
{ Returns:
{    the upper case equivalent of string s
{
{ Calls:
{    ConvUpper from PERQ_String
 ------------------------------------------------------}
begin
   ConvUpper(s);   { changes the local copy only }
   Upper := s
end.   { Upper }
