{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Sail_String;
{-----------------------------------------------------------------------------
{
{ PERQ String hacking routines.
{ Written by: Joseph M. Newcomer
{ Copyright (C) 1980
{ Joseph M. Newcomer, Carnegie-Mellon University
{ and
{ , 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{    This module implements  SAIL-like string hacking routines for the
{    Three River PERQ Pascal.  It is a complete replacement for
{    PERQ_String
{
{---------------------------------------------------------------------------}



{-----------------------------------------------
{
{Change Log:
{
{  16-Jun-81  V2.4  J Newcomer.
{
{ Added various new functions:
{    Initial
{    ULInitial
{    ULIndex2
{    Squeeze
{ }

{ Date: 11-Jun-81 V2.3
{ Who: Joseph M. Newcomer
{ Rewrote the inner loop of InsertChars (Perq_String Insert).  It could
{ never have worked correctly; it certainly wasn't working right now.
{ }

{ Date: 6-Jun-81  V2.2
{ Who: Joseph M. Newcomer
{ Created SAIL_STRING to make up for dozens of missing functions
{ in PERQ_String and general Pascal string support
{ }

{ Date: 19-Jan-81  V2.1
{ Who: Diana Connan Forgy
{ Changed ConCat and SubStr to functions.
{ }

{ Date: 16-Dec-80  V2.0
{ Who: Brad A. Myers
{ Added New procedures: PosC, AppendString, AppendChar, UpperCase, ConvUpper.
{ }

{ 27-Nov-80  1.1  Don Scelza
{ Fixed a bug in SubStr.  Added calls to UserError for errors.
{ (Not released)
{ }

{ Date: 30-Apr-80
{ Who: Don Scelza
{ Changed ConCat and SubStr to use temp strings for their hacking.
{ This will allow a use to pass the same string as input and result
{ parameters.
{ }

{ Date: 8-Apr-80
{ Who: Don Scelza
{ Created the string hacking module.
{ }



{-------------------------------------------------------------------------
{
{ Strings in PERQ Pascal are stored a single character per byte with
{ the byte indexed by 0 being the length of the string.  When the routines
{ in this module must access the length byte they must turn off range
{ checking.
{
{------------------------------------------------------------------------}

{********************}  Exports  {********************}


Const 
      MaxPStringSize=255;          { Length of strings}
      Inf = -32742;                { magic value decoded as length-of-string }
Type 
     PString = String[MaxPStringSize];
     
type BreakType = (Append,Retain,Skip,
                  FoldUp,FoldDown,
                  Inclusive,Exclusive);

type BreakKind = set of BreakType;

type BreakRecord = record
     Breakers: set of chr(0)..chr(#177);
     Omitters: set of chr(0)..chr(#177);
     Flags: BreakKind
     end;

type BreakTable = ^ BreakRecord;


Exception StrBadParm;
{-------------------------------------------------------------------------
 Abstract: Raised when bad index or length parameters passed to procedures or
           sometimes when string will be too long (other times, StrLong is
           raised in this case
-------------------------------------------------------------------------}

Procedure Adjust      (var Str:PString; Len:Integer);
procedure AppendChar  (var s: PString; c: Char);
procedure AppendString(var s1: PString; s2: PString);
Function  Concat      (Str1,Str2:PString):          PString;
function  CVD         (S:PString):                  integer;
function  CVH         (S:PString):                  integer;
function  CVHS        (I: integer) :                PString;
function  CVHSS       (I: integer; W:integer):      PString;
function  CvInt       (S:PString; R: integer):      integer;
function  CVN         (I:integer; W: integer; B: integer; Fill: Pstring):Pstring;
function  CVO         (S:PString):                  integer;
function  CVOS        (I:integer):                  Pstring;
function  CVOSS       (I:integer; W:integer):       Pstring;
function  CVS         (I:integer):                  Pstring;
function  CVSS        (I:integer; W:integer):       Pstring;
Function  CvUp        (s: PString):                 PString;
Procedure DeleteChars (Var Str:PString;  Index,Size:Integer);
function  GetBreak:                                 BreakTable;
Function  Index1      (s: PString; c: char):        integer;
Function  Index2      (Source, Mask:PString):       Integer;
function  Initial     (S1,S2: PString):             boolean;
Procedure InsertChars (Source:PString; var Dest:PString; Index:Integer);
function  Lop         (var Str: PString):           PString;
procedure ReplaceChars(var S: PString; NewS: PString; Index: integer);
function  Scan        (var S: Pstring; BT: breaktable; var BRK: Pstring):Pstring;
procedure SetBreak    (var BT: BreakTable; Break, Omit: PString; Options: BreakKind);
function  ShowBreak   (BT: BreakTable):                          PString;
function  Squeeze     (S: PString):                              PString;
function  str         (Ch: char):                                PString;
function  Strip       (S: PString):                              PString;
Function  SubstrFor   (Source:PString;  Index,Size: Integer):    PString;
Function  SubstrTo    (Source:PString;  Index,EndIndex: Integer):PString;
function  Trim        (S: PString):                              PString;
Function  ULIndex2    (Source, Mask:PString):                    Integer;
function  ULInitial   (S1,S2: PString):                          boolean;
Function  UpEQU       (S1: PString; S2: PString):                boolean;

{********************}  Private  {********************}
{$R-}
Const

    StrErrTooLong=1;            { The result will be too long }
    StrErrParm=2;               { Bad parameters to the call }
    CR = chr(#15);
    LF = chr(#12);
    Tab= chr(#11);
    Debug = false;

Imports System from System;
Imports Except from Except;

type BreakSet = set of chr(0)..chr(#177);

Procedure StrError(Message: Pstring; ErrNum:Integer);
{-------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is called when a String module error
{    occures.
{
{ Parameters:
{    Message is some informative text
{    ErrNum is the number of the error that occured.
{
{ Environment:
{    None
{
{ Results:
{    This procedure does not return a value.
{
{ Side Effects:
{    This procedure may cause a halt.
{
{ Design:
{    MODIFIED 24-Jul-81 by ezf to replace obsolete UserError calls with
{    exceptions used by PERQ_String.
{------------------------------------------------------------------------}
    Begin
    Case ErrNum Of
        StrErrTooLong: (*
            UserError(Concat(Concat('SAIL_String error:  ',Message),
                                    ' String too long.'));
                        *) Raise StrLong;
        StrErrParm: (*
            UserError(Concat(Concat('SAIL_String error: ',Message),
                                    '  Bad parameter to call.'));
                     *) Raise StrBadParm;
      End {Case};
    End;
    
  


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
{    If Len > MaxPStringSize then generate a runtime error.
{
{ Design:
{    Simple.
{----------------------------------------------------------------------}
    Begin
    If (Len>MaxPStringSize) Then StrError(Concat('Adjust: ',cvs(Len)),StrErrTooLong);
    Str[0]:=Chr(Len);
    End {Adjust};

procedure AppendChar(var s: PString; c: Char);
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{  Abstract: puts c on the end of s
{  
{  Parameters : s is the left String and c goes on the end.
{
{  SideEffects : modifies s.
{           
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
   var len: Integer;
   begin
   len := Length(s);
   Adjust(s, len+1);
   s[len+1] := c;
   end;

procedure AppendString(var s1: PString; s2: PString);
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{  Abstract: puts s2 on the end of s1
{  
{  Parameters : s1 is the left String and s2 goes on the end.
{
{  Calls: PerqString.Concat.
{
{  SideEffects : modifies s1.
{           
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
   begin
   s1 := Concat(s1,s2);
   end;

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
{    generate a runtime error.
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
    If Leng > MaxPStringSize Then StrError('ConCat: ',StrErrTooLong);
    
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

function CVD(S:PString):integer;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts a decimal string to an integer
{    Conversion stops at the first character which is not legal in the
{    radix used.
{    Characters <= space which precede the value are ignored.  
{ Parameters:
{    S is the string to be converted
{ Environment:
{    None
{
{ Results:
{    An integer containing the value  
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVD}
   CVD := CVInt(S,10);
end {CVD};

function CVH(S:PString):integer;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an hexadecimal string to an integer
{    Conversion stops at the first character which is not legal in the
{    radix used.
{    Characters <= space which precede the value are ignored.  
{    Lower case 'a'..'f' are the same as upper case 'A'..'F'
{ Parameters:
{    S is the string to be converted
{ Environment:
{    None
{
{ Results:
{    An integer containing the value  
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVH}
   CVH := CVInt(S,16);
end {CVH};
    

function CVHS(I:integer):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string using hexadecimal radix
{ Parameters:
{    I is the integer to be converted
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    represent the value expressed in hexadecimal
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVHS}
    CVHS := CVN(I,0,16,' ');
end {CVHS};

function CVHSS(I:integer; W:integer):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string of width W, padding on the left with
{    spaces if necessary to fill out the width; radix will be hexadecimal
{ Parameters:
{    I is the integer to be converted
{    W is the minimum field width to be produced
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    be of at least width W, and be filled on the left with spaces and
{    the conversion will be done in hexadecimal radix
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVHSS}
     CVHSS := CVN(I,W,16,' ');
end {CVHSS};

function CvInt(S:PString; R: integer): integer;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts a string to an integer according to radix R
{    Conversion stops at the first character which is not legal in the
{    radix used.
{    Characters <= space which precede the value are ignored.  
{    A sign is permitted.
{    Lower case 'a'..'z' are the same as upper case 'A'..'Z'
{ Parameters:
{    S is the string to be converted
{    R is the radix to use
{ Environment:
{    None
{
{ Results:
{    An integer containing the value  
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
var
    negative: boolean;
    V: integer;
    D: integer;
    Ptr: integer;
    Done: boolean;
    Digits: set of '0'..'Z';
    
begin {CvInt}
   V := 0;
   Ptr := 1;
   negative := false;
   
   Done := false;
   while not Done do
     begin
       if Ptr <= length(S) then
         if S[Ptr] <= ' ' then
            Ptr := Ptr + 1
         else
            Done := true
       else
         Done := true;
     end;
     
   if Ptr > length(S) then
      begin
        CvInt := 0;
        exit(CvInt);
      end;
   
   if S[Ptr] = '-' then
      begin
         Ptr := Ptr + 1;
         Negative := true;
      end
   else
   if S[Ptr] = '+' then
      begin
         Ptr := Ptr + 1;
      end;
      
   { Establish the set of legal characters }
   
   Digits :=  [ ];
   for D := 0 to R-1 do
      begin
      if D < 10 then Digits := Digits + [chr(D+ord('0'))]
                else Digits := Digits + [chr(D-10+ord('A'))];
      end;
      
   { We now scan the number. R controls the radix.  
     There is currently no check for overflow }
     

    Done := false;
    repeat
     if Ptr <= length(S) then
       begin
        if S[Ptr] in ['a'..'z'] then S[Ptr] := chr(Ord(S[Ptr])-#40); {case fold}
         if S[Ptr] in Digits then
             begin
             V := V * R;
             if S[Ptr] in ['0'..'9'] then V := V + (Ord(S[Ptr])-Ord('0'))
                                     else V := V + (Ord(S[Ptr])-Ord('A')+10);
             end
         else
             Done := true;
         Ptr := Ptr + 1;
       end
      else
        Done := true;
    until Done;
    
    if Negative then V := - V;
    CvInt := V;
   
end {CvInt};


function CVN(I:integer; W:integer; B: integer; Fill: Pstring):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string of width W, padding on the left with
{    'Fill' if necessary to fill out the width.  The base for the conversion
{    is B.  If B>10, the letters 'A'..'Z' will be used to compute the
{    character for the representation.  Using a base > 36 will produce
{    bogus results
{ Parameters:
{    I is the integer to be converted
{    W is the minimum field width to be produced
{    B is the base to use (2..36)
{    Fill is a one-character string to fill on the left
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    be of at least width W, and be filled on the left with Fill, and
{    converted according to radix B
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
var N: integer;
    t: integer;
    neg: boolean;
    S: Pstring;

   procedure XN;
   var R: integer;
   begin {XN}
     if N = 0 then
       begin {done}
         for T := T + 1 to W do
             S := Concat(S,Fill);
         if Neg then AppendChar(S,'-');
       end {done}
      else
       begin {next}
         R := abs(N mod B);
         N := N div B;
         T := T + 1;
         XN;
         if R > 9 then 
           AppendChar(S,chr(R+55)) 
         else 
           AppendChar(S,chr(48+R));
       end {next};
   end {XN};

begin {CVN}
{ Converts value I to a string, using base B, and width W, padding with Fill }


   if (length(Fill) = 0) and (W > 0) then Fill := ' ';
   if I < 0 then
      begin {negative}
      T := 1;
      Neg := true;
      end {negative}
    else
      begin {positive}
      T := 0;
      Neg := false;
      end {positive};
    
    B := abs(B);
    
    if (Fill = '0') and Neg then
       begin
         S := '-';
         Neg := false;
       end
     else
         S:= '';
         
     N := abs(I);
     
     if N = 0 then
       begin
       for T := T+1 to W do S := Concat(S,Fill);
       S := Concat(S,'0');
       end
     else
       XN;
       
     CVN := S;
     
    
end {CVN};

function CVO(S:PString):integer;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an octal string to an integer
{    Conversion stops at the first character which is not legal in the
{    radix used.
{    Characters <= space which precede the value are ignored.  
{ Parameters:
{    S is the string to be converted
{ Environment:
{    None
{
{ Results:
{    An integer containing the value  
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVO}
   CVO := CVInt(S,8);
end {CVO};

function CVOS(I:integer):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string using octal radix
{ Parameters:
{    I is the integer to be converted
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    represent the value expressed in octal
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVOS}
    CVOS := CVN(I,0,8,' ');
end {CVOS};

function CVOSS(I:integer; W:integer):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string of width W, padding on the left with
{    spaces if necessary to fill out the width; radix will be octal
{ Parameters:
{    I is the integer to be converted
{    W is the minimum field width to be produced{
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    be of at least width W, and be filled on the left with spaces and
{    the conversion will be done in octal radix
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVOSS}
     CVOSS := CVN(I,W,8,' ');
end {CVOSS};

function CVS(I:integer):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string using decimal radix
{ Parameters:
{    I is the integer to be converted
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    represent the value expressed in decimal
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {CVS}
    CVS := CVN(I,0,10,' ');
end {CVS};

function CVSS(I:integer; W: integer):Pstring;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Converts an integer to a string of width W, padding on the left with
{    spaces if necessary to fill out the width; radix will be decimal
{ Parameters:
{    I is the integer to be converted
{    W is the minimum field width to be produced
{ Environment:
{    None
{
{ Results:
{    A string containing the character representation; the string will
{    be of at least width W, and be filled on the left with spaces and
{    the conversion will be done in decimal radix
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}

begin {CVSS}
    CVSS := CVN(I,W,10,' ');
end {CVSS};

Function CvUp(s: PString):PString;
{------------------------------------------------------------------------
{   Abstract: Converts s to all upper case
{
{   Parameters: s,  to be converted 
{
{   Result:
{     The uppercase version of S;
{ -------------------------------------------------------------------------}
    Var i:Integer;
    const delta = ord('a')-ord('A');
    Begin {CvUp}
    For i:=1 To Length(s) Do
        if s[i] in ['a'..'z'] Then
                s[i]:=Chr(Ord(s[i]) - delta);
    CvUp := s;
    End {CvUp};

Procedure DeleteChars(var Str:PString; Index, Size:Integer);
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
        
    Begin {DeleteChars}
    
{
{ Check to see if he gave an index that was out of range.
{ }

    if Index = Inf then index := length(Str);
    if Size  = Inf then Size := length(Str);
    
    If Length(Str) < Index Then Exit(DeleteChars);
    
    
    Offset:=0;
    For I:=(Index+Size) To Length(Str) Do
        Begin
          Str[Index+Offset]:=Str[I];
          Offset:=Offset+1;
        End;

    Str[0]:=Chr(Index+Offset-1)
    
    
    End {DeleteChars};
    

function GetBreak: BreakTable;
var BT: BreakTable;
begin {GetBreak}
    new(BT);
    BT^.Breakers := [ ];
    BT^.Omitters := [ ];
    BT^.Flags := [ ];
    GetBreak := BT;
end {GetBreak};

Function Index1(s: PString; c: char): integer;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
{  Abstract: Tests if c is a member of s.
{  
{  Parameters: c is any char; s is string to test for c member of.
{
{  Returns: index of c in s or zero if not there.
{
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
   var i : integer;
   begin {Index1}
   Index1 := 0;
   for i := 1 to length(s) do
      if s[i] = c then begin
                       Index1 := i;
                       exit(Index1);
                       end;
   end {Index1};
 

Function Index2(Source, Mask:PString):Integer;
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
        
Begin {Index2}
    MaskLength:=Length(Mask);
    Index2:=0;
    
{
{ Scan for the first character of Mask in Source.
{ }
    
    For I := 1 To (Length(Source)-MaskLength+1) Do
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
            Index2:=I;
            GoTo 1;
            End;
        End;
    
    1:
    End {Index2};

function Initial(S1,S2: PString):boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{   This function returns true if S2 is an initial string of S1.  The 
{   comparison is case-sensitive.  A null string is an initial substring
{   of any string.
{
{ Parameters:
{    S1  is the string to be tested
{    S2  is the string which is the initial substring to test for
{    
{ Environment:
{    None
{
{ Results:
{    true if S2 is an initial substring of S1
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
var
   I: integer;
begin {Initial}
if length(S2) > length(S1) then 
   begin
   Initial := false;
   exit(Initial);
   end;
   
for I := 1 to length(S2) do
   begin
      if S1[I] <> S2[I] then
         begin
           Initial := false;
           exit(Initial);
         end;
   end;
Initial := true;
end {Initial};

Procedure InsertChars(Source: Pstring; var Dest:PString; Index:Integer);
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
        Offset,Leng,LSource,LDest: Integer;
        Temp: Char;
        
    Begin
    LSource:=Length(Source);
    LDest := Length(Dest);
    Leng:=LDest+LSource;

{
{ Check to see if the string is going to be too large.
{ }

    If Leng > MaxPStringSize Then StrError(Concat('InsertChars: [ ',
                                              Concat(cvs(Leng), '] ')),
                                           StrErrTooLong);
    
{
{ Check to see if Index is larger than the length of the string.
{ If so then set it to the end of the string.
{ }

    If (Index > Length(Dest)) or (Index = Inf) Then Index:= Length(Dest)+1;

{
{ Adjust the length.
{ }

    Dest[0]:=Chr(Leng);
    

{ Make a hole for the insertion. }

    for Offset := LDest downto Index do
         Dest[Offset+LSource] := Dest[Offset];
{
{ Make the insertions.
{ }
    For Offset:= 0 To LSource-1 Do
        Begin
        Dest[Index+Offset]:=Source[Offset+1];
        End;
    End {Insert};
 
function Lop(var Str: PString): PString;
begin {Lop}
   if Length(Str) = 0 then
       begin
       Lop := '';
       Exit(Lop);
       end;
   Lop := SubStrFor(Str,1,1);
   DeleteChars(Str,1,1);
end {Lop};

procedure ReplaceChars (var S: PString; NewS: PString; Index: integer);
var I: integer;
    MaxReplacement : integer;
    DebugString: PString;
begin {ReplaceChars}
                       
    if Index <= 0 then Index := 1;
    
    MaxReplacement := Index + length(NewS)-1;
    if MaxReplacement > length(S) then MaxReplacement := length(S);
    for I := Index to MaxReplacement do
      begin
        S[I] := NewS[I+1-index];
      end;
    
end {ReplaceChars};

function  Scan     (var S: Pstring; BT: breaktable; var BRK: Pstring):Pstring;
{------------------------------------------------------------------------------
{ Abstract:
{    Scans the string S according to the breaktable specifications of BT;
{    
{ Parameters:
{    S is a string to be scanned
{    
{    BT is a breaktable initialized by SetBreak
{
{    BRK is the break character
{
{ Environment:
{    Unknown
{
{ Results:
{    The initial substring determined by the breaktable is removed from
{    S and returned as the value of the function
{    
{    The BRK variable contains the string (character) which caused the
{    scan to stop, or the null string if the string was exhausted
{
{ Errors:
{    None
{
{-------------------------------------------------------------------------}
type Direction = (Up,Down,None);
var
   S1: PString;
   I:  integer;
   Left: integer;
   Right: integer;
   Ch: Char;
   Fold: Direction;
begin {scan}
   BRK := '';
   I := 1;
   if FoldUp in BT^.Flags then Fold := Up
   else
   if FoldDown in BT^.Flags then Fold := Down
   else
   Fold := None;
   
   While I <= length(S) do
       begin
         Ch := S[I];
         if Fold <> None then
            if Ch in ['A'..'Z','a'..'z'] then
               begin {DoFold}
                 if Fold = Down then Ch := Chr(Lor (Ord(Ch), #40))
                                else Ch := Chr(Land(Ord(Ch),#137));
                 S[I] := Ch;
               end {DoFold};
         Left := I;
         Right := I;
         
         {$ifc Debug then} Write('[''',Ch,'''] '); {$endc}
       
         if Ch in BT^.Breakers then
           begin {break}
             BRK := str(Ch);
             
             {$ifc Debug then} Write('Brk=''',Brk,''', '); {$endc}
            
             if (Skip in BT^.Flags) or (Retain in BT^.Flags) or
                (Ch in BT^.Omitters) then 
                 Left := Left - 1;
              
             {$ifc Debug then} Write('Left=',Left:1,', '); {$endc}
             
             if (Skip in BT^.Flags) or (Append in BT^.Flags) or
                (Ch in BT^.Omitters) then 
                 Right := Right + 1;
                 
             {$ifc Debug then} Write('Right=',Right:1,', '); {$endc}
             
             S1 := SubStrTo(S,1,Left);
             S := SubStrTo(S,Right,inf);
             
             {$ifc Debug then} WriteLn('S1=''',S1,''', S=''',S,''''); {$endc}
             Scan := S1; 
             exit(Scan);         
           end {break}
         else
       if Ch in BT^.Omitters then 
         begin
           {$ifc Debug then} Write('DeleteChars(''',S,''',',I:1,',1)='''); {$endc}
           DeleteChars(S,I,1);
           {$ifc Debug then} WriteLn(S,''''); {$endc}
           I := I - 1;
         end;
       I := I + 1;
          
       end;

    Scan := S;
    S := '';
   
end {scan};

procedure SetBreak(var BT: BreakTable; Break, Omit: PString; Options: BreakKind);
{------------------------------------------------------------------------------
{ Abstract:
{    Initializes a breaktable according to the specifications of Break, Omit
{    and Options.
{
{    Break specifies the set of characters (as a string) on which a
{          scanning break will occur
{
{    Omit  specifies the set of characters which will be removed from
{          the string
{
{    Options allows specification of one option from each of the
{            following groups:
{
{    Inclusive: The Break set is the set of characters on which a break
{               will occur
{    Exclusive: The Break set is the set of characters on which a break
{               will not occur
{
{    ~          If no option is specified from this group, 'Inclusive'
{               is assumed.
{    ----------
{    Skip:      Upon return, the break character will be in the break
{               variable. The result of the scan will be all characters up
{               to the break character, and the input string is modified
{               to start immediately after the break character.
{    Append:    Upon return, the break character will be in the break
{               variable.  The result of the scan will be all characters up
{               to and including the break character, and the input string
{               is modified to start immediately after the break character
{    Retain:    Upon return, the break character will be in the break
{               variable.  The result of the scan will be all characters up
{               to the break character, and the input string is modified
{               to start at the break character.
{    
{    ~          If no option is specified from this group, 'Skip' is assumed.
{    -----------
{    FoldUp:    Before anything else is done, each character which is
{               alphabetic is folded to uppercase.  Note that break sets
{               are case sensitive, but this is done before the break test.
{               This folding proceeds until the break condition is reached.
{    FoldDown:  Similar to FoldUp, except upper case alphabetics are made
{               lower case.
{
{    ~          If no option is specified from this group, no case folding
{               will be done.
{
{               *** No guarantees about behavior are made if more than
{                   one option is selected from each set group.  ***
{    
{ Parameters:
{    S is a string to be scanned
{    
{    BT is a breaktable initialized by SetBreak
{
{    BRK is the break character
{
{ Environment:
{    Unknown
{
{ Results:
{    The initial substring determined by the breaktable is removed from
{    S and returned as the value of the function
{    
{    The BRK variable contains the string (character) which caused the
{    scan to stop, or the null string if the string was exhausted
{
{ Errors:
{    StrErrParm: Illegal combinations of options
{
{-------------------------------------------------------------------------}
var 
    S:  PString;
begin {SetBreak}
   If ((FoldUp in Options) and (FoldDown in Options)) 
      or
      ((Exclusive in Options) and (Inclusive in Options))
      then
         StrError('SetBreak: ',StrErrParm);
   BT^.Flags := Options;
   if Exclusive in Options then BT^.Breakers := [chr(0)..Chr(#177)];
   while length(Break) > 0 do
     begin
        S := lop(Break);
        if Exclusive in Options then
            BT^.Breakers := BT^.Breakers - [ S[1] ]
         else
            BT^.Breakers := BT^.Breakers + [ S[1] ];
     end;
     
   while length(Omit) > 0 do
     begin
        S := lop(Omit);
        BT^.Omitters := BT^.Omitters + [ S[1] ];
     end;
end {SetBreak};

function ShowBreak(BT: BreakTable): PString;
var
    S : PString;
procedure AddChr(Var S: string; BS: BreakSet);
var I: integer;
begin {AddChr}
    for I := 0 to #177 do
        begin
           if chr(I) in BS then 
              begin
                S := Concat(S,str(chr(I)));
                if chr(I) = '''' then S := Concat(S,str(chr(I)));
              end;
        end;
    S := Concat(S,str(''''));
    S := Concat(S,str(chr(CR)));
    S := Concat(S,str(chr(LF)));
end {AddChr};
begin {ShowBreak}
    S := 'Break: ''';
    AddChr(S,BT^.Breakers);
    S := Concat(S,'Omit: ''');
    AddChr(S,BT^.Omitters);
    S := Concat(S,'Flags:');
    if Skip in BT^.Flags then S := Concat(S,' Skip');
    if Append in BT^.Flags then S := Concat(S,' Append');
    if Retain in BT^.Flags then S := Concat(S,' Retain');
    
    if FoldUp in BT^.Flags then S := Concat(S,' FoldUp');
    if FoldDown in BT^.Flags then S := Concat(S,' FoldDown');
    S := Concat(S,str(chr(CR)));
    S := Concat(S,str(chr(LF)));
    ShowBreak := S;

end {ShowBreak};

function Squeeze(S: PString):PString;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Removes all spaces and tabs from a string 
{
{ Parameters:
{    S: The string to be squozen
{
{ Environment:
{    None
{
{ Results:
{    A string which has all spaces and tabs removed
{
{ Errors: 
{
{ Design:
{--------------------------------------------------------------------------}
var
   InCursor: integer;
   T: PString;
begin {squeeze}
   InCursor := 1;
   T := '';
   while InCursor <= length(S) do
     begin
       if (S[InCursor] <> ' ') and (S[InCursor] <> Tab) then
          begin {copy}
             AppendChar(T,S[InCursor]);
          end {copy};
       InCursor := InCursor + 1;
     end;
    Squeeze := T;
       
end {squeeze};

function Str(Ch:char):PString;
{-------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to coerce a character to a string
{
{ Parameters:
{    Ch: the character to be coerced to a string
{
{ Environment:
{    None
{
{ Results:
{    A string value for a one-character string containing the character
{
{ Errors: 
{
{ Design:
{--------------------------------------------------------------------------}
begin {Str}
   Str[0] := chr(1);
   Str[1] := chr(Ch);
end {Str};

function Strip(S: PString): PString;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Converts sequences of spaces, tabs, CR and LF to a single space
{
{ Parameters:
{    S: The string to be squozen
{
{ Environment:
{    None
{
{ Results:
{    A string which has all sequences of spaces, tabs, CR and LF changed 
{    to a single space
{ Errors: 
{
{ Design:
{--------------------------------------------------------------------------}
var
   InCursor: integer;
   InSequence: boolean;
   T: PString;
begin {Strip}
   InCursor := 1;
   T := '';
   InSequence := false;
   
   while InCursor <= length(S) do
     begin
       if (S[InCursor] = ' ') or (S[InCursor] = Tab)
          or
          (S[InCursor] = CR) or (S[InCursor] = LF) then
          begin {Squoze}
             if not InSequence then
                begin {DoStrip}
                  AppendChar(T,' ');
                  InSequence := true;
                end {DoStrip};
          end {Squoze}
          else
          begin {copy}
             AppendChar(T,S[InCursor]);
             InSequence := false;
          end {copy};
       InCursor := InCursor + 1;
     end;
    Strip := T;
       
end {Strip};

Function SubStrFor(Source:PString; Index, Size:Integer):PString;
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
{    If Index+Size exceed the dynamic length of the string, return
{       Index to DynamicLength; no error message is generated
{
{ Errors: 
{    If Index or Size exceed MaxPStringSize, give an error
{
{ Design:
{--------------------------------------------------------------------------}
    Var
        I,StrEnd: Integer;
        Temp:PString;
    
    Begin {SubStrFor}
    
{
{ Check the parameters.
{ }

    if Index = Inf then Index := length(Source);
    if Size = Inf then Size := length(Source);
    If (Index>MaxPStringSize) Or (Size>MaxPStringSize) Then StrError('SubStrFor: ',StrErrParm);
    
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
     SubStrFor:=Temp;

   End {SubstrFor};
    

Function SubStrTo(Source:PString; Index, EndIndex:Integer):PString;
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
{    EndIndex is the Ending position in the source of the substring. 
{
{ Environment:
{    None
{
{ Results:
{    This function returns a substring as described by the parameter list.
{
{    If Index or EndIndex  exceed the dynamic length of the string, return
{       Index to DynamicLength; no error message is generated
{
{ Errors: 
{    If Index or EndIndex exceed MaxPStringSize, give an error
{
{ Design:
{--------------------------------------------------------------------------}
    Var
        I,StrEnd: Integer;
        Temp:PString;
    
    Begin {SubStrTo}
    
{
{ Check the parameters.
{ }
    if Index = Inf then index := length(Source);
    if EndIndex = Inf then EndIndex := length(Source);
    
    If (Index>MaxPStringSize) Or (EndIndex>MaxPStringSize) Then StrError('SubStrTo: ',StrErrParm);
    
{
{ Now make up the substring.
{ }

    If (Index>Length(Source)) or (EndIndex < Index) Then
        Begin
          Temp:='';
          Temp[0]:=Chr(0);
        End
    Else
        Begin
        If Length(Source) < EndIndex Then
            EndIndex := length(Source);
        For I:=Index To EndIndex Do
            Begin
            Temp[I-Index+1]:=Source[I];
            End;
        Temp[0]:=Chr(EndIndex-Index+1);
        End;
     SubStrTo:=Temp;

   End {SubstrTo};

function Trim(S: PString): PString;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Deletes leading and trailing spaces and tabs from a string
{
{ Parameters:
{    S: The string to be squozen
{
{ Environment:
{    None
{
{ Results:
{    A string which has all leading and trailing spaces and tabs removed
{ Errors: 
{
{ Design:
{--------------------------------------------------------------------------}
var
   InCursor: integer;
   LastNonSpace: integer;
   Leading: boolean;
   T: PString;
begin {Trim}
   InCursor := 1;
   T := '';
   LastNonSpace := 0;
   Leading := true;
   
   while InCursor <= length(S) do
     begin
       if (S[InCursor] = ' ') or (S[InCursor] = Tab)
          or
          (S[InCursor] = CR) or (S[InCursor] = LF) then
          begin {Squoze}
             if not Leading then
                begin {DoStrip}
                  AppendChar(T,S[InCursor]);
                end {DoStrip};
          end {Squoze}
          else
          begin {copy}
             AppendChar(T,S[InCursor]);
             Leading := false;
             LastNonSpace := length(T);
          end {copy};
       InCursor := InCursor + 1;
     end;
    Adjust(T,LastNonSpace);
    Trim := T;
       
end {Trim};

Function ULIndex2(Source, Mask:PString):Integer;
{----------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to find the position of a pattern in a
{    given string without case sensitivity.
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
begin {ULIndex2}
    ULIndex2 := Index2(cvUp(Source),cvUp(Mask));
end {ULIndex2};

function ULInitial(S1,S2: PString):boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{   This function returns true if S2 is an initial string of S1.  The 
{   comparison is case-insensitive.  A null string is an initial substring
{   of any string.
{
{ Parameters:
{    S1  is the string to be tested
{    S2  is the string which is the initial substring to test for
{    
{ Environment:
{    None
{
{ Results:
{    true if S2 is an initial substring of S1
{
{ Side Effects:
{    None
{
{ Errors:
{    None
{
{ Design:
{----------------------------------------------------------------------------}
begin {ULInitial}
if length(S2) > length(S1) then 
   begin
   ULInitial := false;
   exit(ULInitial);
   end;

   ULInitial := Initial(cvUp(S1),cvUp(S2));   
end {ULInitial};

Function UpEQU(S1: PString;S2: PString):boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Compares two strings for case-independent equality
{
{ Parameters:
{    S1, S2 are the strings to be compared
{
{ Environment:
{    None
{
{ Results:
{    true if the strings are equal, false if they are not, independent
{    of case
{
{ Side Effects:
{    none.
{
{ Errors:
{    none.
{
{ Design:
{----------------------------------------------------------------------------}
begin {UpEQU}
   UpEQU := CvUp(S1) = CvUp(S2);
end {UpEQU}.    
