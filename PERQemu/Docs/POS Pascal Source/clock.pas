{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Clock;
{-----------------------------------------------------------------------------
{
{       Clock - Perq clock routines.
{       J. P. Strait       1 Feb 81.
{       Copyright (C) Three Rivers Computer Corporation, 1981.
{
{ Abstract:
{       Clock implements the Perq human-time clock.  Times are represented
{       internally by a TimeStamp record which has numeric fields for Year,
{       Month, Day, Hour, Minute, and Second.  Times may also be expressed
{       by a string of the form YY MMM DD HH:MM:SS where MMM is a three (or
{       more) letter month name and HH:MM:SS is time of day on a 24 hour
{       clock.
{
{       The clock module exports routines for setting and reading the current
{       time as either a TimeStamp or a character string, and exports routines
{       for converting between TimeStamps and strings.
{       
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{ Change Log:
{
{ V1.7   2 Mar 83  Don Scelza
{ Added code to handle the PERQ-2 hardware clock.
{
{ V1.6  17 Dec 81 WJHansen
{    Set Past.Lower in SetTStamp
{
{ V1.5  12 May 81  Brad Myers.
{ If invalid time string then raise a signal.
{
{ V1.4  19 Mar 81  Brad Myers.
{ PERQ_String.
{
{ V1.3   2 Mar 81  John Strait.
{ Don't export IO from Clock and GetTimeStamp.
{
{ V1.2   2 Mar 81  John Strait.
{ Add title, copyright, and abstract.
{ Add version number.
{ Add retroactive change history.
{
{ V1.1  28 Feb 81  Brian Rosen.
{ Check for invalid month numbers in StampToString.
{
{ V1.0   1 Feb 81  John Strait.
{ Start file.
{
{-----------------------------------------------------------------------------}

exports


imports GetTimeStamp from GetTimeStamp;
     
     
const ClockVersion = '1.7';


type TimeString = String;


procedure SetTStamp( Stamp: TimeStamp );
procedure SetTString( String: TimeString);
procedure GetTString( var String: TimeString );
procedure StampToString( Stamp: TimeStamp; var String: TimeString );
procedure StringToStamp( String: TimeString; var Stamp: TimeStamp);

Exception BadTime;
{----------------------------------------------------
 Abstract: Raised when a string passed does not represent a valid time
----------------------------------------------------}


procedure GetPERQ2GMT(var Stamp: TimeStamp);
procedure GetPERQ2Local(var Stamp: TimeStamp);
procedure PutPERQ2Offset;

exception GTSNotPERQ2;
{-----------------------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if any of the *PERQ2* procedures are called
{   and the current machine is not a PERQ-2.
{
{----------------------------------------------------------------------------}

exception GTSNoZ80;
{-----------------------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if GetPERQ2GMT is called and the Z80 does not
{   respond;
{
{----------------------------------------------------------------------------}

const OffsetFile = '>HoldOffset.TimeStamp';


private


{$R-}


imports IO_Others from IO_Others;
imports Perq_String from Perq_String;
imports IO_Unit from IO_Unit;
imports Configuration from Configuration;
imports Stream from Stream;
imports IOErrors from IOErrors;

type OffsetStamp = long;


const MonthNames = 'JanFebMarAprMayJunJulAugSepOctNovDec';

const
    SecPerYear = 31536000;
    SecPerLeap = 31622400;
    SecPerDay  = 86400;
    SecPerHour = 3600;
    SecPerMin  = 60;
    



procedure SetTStamp( Stamp: TimeStamp );
{----------------------------------------------------
 Abstract: Sets time to be time specified by Stamp
 Parameters: stamp is new time
 SideEffects: Changes current time
----------------------------------------------------}
var DoublePast: Double;
begin { SetTStamp }
  IOGetTime(DoublePast);
  Past.Lower := DoublePast[0];
  Past.Upper := DoublePast[1];
  PastStamp := Stamp
end { SetTStamp };



procedure SetTString( String: TimeString);
{----------------------------------------------------
 Abstract: Sets time to be time specified by String
 Parameters: string is the string of the new time
 SideEffects: Changes current time
 Errors: Raises BadTime is string is invalid (malformed or illegal time)
----------------------------------------------------}
var LocalPast: Double;
    LocalStamp: TimeStamp;
begin { SetTString }
  IOGetTime(LocalPast);
  StringToStamp(String,LocalStamp); {may raise BadTime}
  Past.Upper := LocalPast[1];
  Past.Lower := LocalPast[0];
  PastStamp := LocalStamp
end { SetTString };


procedure GetTString( var String: TimeString );
{----------------------------------------------------
 Abstract: Returns the current time as a string
 Parameters: string is the string to be set with the current time
----------------------------------------------------}
var Stamp: TimeStamp;
begin { GetTString }
  GetTStamp(Stamp);
  StampToString(Stamp,String)
end { GetTString };



procedure StampToString( Stamp: TimeStamp; var String: TimeString );
{----------------------------------------------------
 Abstract: Returns a string for the time specified by stamp
 Parameters: stamp is time to get string for;
             string is set with time represented by stamp
----------------------------------------------------}
var Y: Integer;
begin { StampToString }
  with Stamp do
    begin String := '';
      if (Month < 1) or (Month > 12) then Month := 1;
      Y := Year + 1980;
      AppendChar( String, Chr(Day div 10 + Ord('0')));
      AppendChar( String, Chr(Day mod 10 + Ord('0')));
      AppendChar( String, ' ');
      AppendString( String, SubStr( MonthNames, Month * 3 - 2, 3));
      AppendChar( String, ' ');
      AppendChar( String, Chr(Y mod 100 div 10 + Ord('0')));
      AppendChar( String, Chr(Y mod 10 + Ord('0')));
      AppendChar( String, ' ');
      AppendChar( String, Chr(Hour div 10 + Ord('0')));
      AppendChar( String, Chr(Hour mod 10 + Ord('0')));
      AppendChar( String, ':');
      AppendChar( String, Chr(Minute div 10 + Ord('0')));
      AppendChar( String, Chr(Minute mod 10 + Ord('0')));
      AppendChar( String, ':');
      AppendChar( String, Chr(Second div 10 + Ord('0')));
      AppendChar( String, Chr(Second mod 10 + Ord('0')))
    end
end { StampToString };



procedure StringToStamp( String: TimeString; var Stamp: TimeStamp );
{----------------------------------------------------
 Abstract: Converts string into a time stamp
 Parameters: string is the string containing time;
             stamp is stamp set with time according to string
 Errors: Raises BadTime is string is invalid (malformed or illegal time)
----------------------------------------------------}
var Pos, Year, Month, Day, Hour, Minute, Second, DpM: Integer;
    MonthString, MN: TimeString;
    Ch: Char;


  procedure NextCh;
  begin { NextCh }
    if Pos > Length(String) then Ch := ' '
    else
      begin Ch := String[Pos]; Pos := Pos + 1 end
  end { NextCh };


  procedure GetNum( var N: Integer; Min, Max: Integer);
  var Good: Boolean;
  begin { GetNum }
    N := 0;
    Good := False;
    while (Ch >= '0') and (Ch <= '9') do
      begin
        if N > 1000 then Raise BadTime
        else
          begin Good := True; N := N * 10 + Ord(Ch) - Ord('0') end;
        NextCh
      end;
    if not Good then Raise BadTime
    else
      if (N < Min) or (N > Max) then Raise BadTime
  end { GetNum };
  
  
  procedure SkipSpaces;
  begin { SkipSpaces }
    if Ch <> ' ' then Raise BadTime;
    while (Ch = ' ') and (Pos <= Length(String)) do NextCh
  end { SkipSpaces };
  
  
begin { StringToStamp }
  Pos := 1;
  NextCh;
  if Ch = ' ' then SkipSpaces;
  GetNum(Day,1,31);
  if Ch = '-' then NextCh else SkipSpaces;
  MonthString := '';
  while ((Ch >= 'a') and (Ch <= 'z')) or
        ((Ch >= 'A') and (Ch <= 'Z')) do
    begin
      if Length(MonthString) < 3 then AppendChar(MonthString,Ch);
      NextCh
    end;
  Month := 0;
  ConvUpper(MonthString);
  MN := MonthNames;
  ConvUpper(MN);
  repeat Month := Month + 1
  until (Month > 12) or
        (MonthString = SubStr( MN, Month * 3 - 2, 3));
  if Month > 12 then Raise BadTime;
  if Ch = '-' then NextCh else SkipSpaces;
  GetNum(Year,0,99);
  if (Year <= 43) then Year := Year + 2000
  else
    if Year >= 80 then Year := Year + 1900
    else Raise BadTime;
  if Month in [1, 3, 5, 7, 8, 10, 12] then DpM := 31
  else
    if Month = 2 { Feb } then
      if (Year mod 4 = 0) and
         ((Year mod 100 = 0) = (Year mod 400 = 0)) then
        { leap year } DpM := 29
      else DpM := 28
    else DpM := 30;
  if Day > DpM then Raise BadTime;
  SkipSpaces;
  GetNum(Hour,0,23);
  if Ch = ':' then NextCh else Raise BadTime;
  GetNum(Minute,0,59);
  if Ch = ':' then
    begin NextCh;
      GetNum(Second,0,59)
    end
  else Second := 0;
  SkipSpaces;
  if Pos <= Length(String) then Raise BadTime;
  Stamp.Year := Year - 1980;
  Stamp.Month := Month;
  Stamp.Day := Day;
  Stamp.Hour := Hour;
  Stamp.Minute := Minute;
  Stamp.Second := Second
end { StringToStamp };


{$r+}
procedure GetPERQ2GMT(var Stamp: TimeStamp);
{---------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to get the GMT time from the clock chip on the
{   PERQ-2 I/O board.
{
{ Parameters:
{   Stamp is a TimeStamp that will be set to contain the current
{   GMT from the PERQ-2 clock.
{
{ Exceptions:
{   if the current IO board is not an EIO then raise GTSNotPERQ2. 
{
{--------------------------------------------------------------------------}
  var StsPtr:     IOStatPtr;
      Bufr:       IOBufPtr;
      ByteCnt:    integer;
      LogAdr:     double;
      HdPtr:      IOHeadPtr;
      Clkp:       pClockStat;

    begin
    if CF_IOBoard <> CF_EIO then raise GTSNotPERQ2;
    new(StsPtr);
    new(Bufr);
    LogAdr[0] := 0;
    LogAdr[1] := 0;
    new(HdPtr);
    UnitIO(Clock, Bufr, IOSense,  ByteCnt, LogAdr, HdPtr, StsPtr); 
    Clkp := recast(Bufr, pClockStat); 
    if StsPtr^.SoftStatus <> IOEIOC then raise GTSNoZ80;  
    Stamp.Year :=   ClkP^.Year;
    Stamp.Month :=  ClkP^.Month;
    Stamp.Day :=    ClkP^.Day;
    Stamp.Hour :=   ClkP^.Hour;
    Stamp.Minute := ClkP^.Minute;
    Stamp.Second := ClkP^.Second;
    dispose(StsPtr);
    dispose(Bufr);
    dispose(HdPtr);
    end;

procedure GetPERQ2Offset(var Stamp: OffsetStamp);
{---------------------------------------------------------------------------
{
{ Abstract:
{   Get the current offset from GMT.
{
{ Parameters:
{   Stamp will be set to be the current offset from GMT.
{   If the file that holds the offset was not found then
{   return a 0.
{
{ Exceptions:
{   if the current IO board is not an EIO then raise GTSNotPERQ2. 
{
{--------------------------------------------------------------------------}
  handler ResetError( FileName: PathName );
    begin
    Stamp :=   0;
    exit(GetPERQ2Offset);
    end;
  var F: file of OffsetStamp;
  
    begin
    if CF_IOBoard <> CF_EIO then raise GTSNotPERQ2;
    reset(F, OffsetFile);
    Stamp := F^;
    close(F);
    end;   

function CnvSecToStamp(LocSec: OffsetStamp): TimeStamp;
{---------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to convert a number of seconds since
{   1-Jan-80 00:00:00 to a timestamp.
{
{ Parameters:
{   LocSec is the number of seconds since the time base.
{
{ Results:
{   Return a time stamp that that is LocSec seconds since the base.
{
{--------------------------------------------------------------------------}
  var Val: TimeStamp;
      DpM, NumYears, NumLeap, temp: long;
      I: integer;
      label 1;
    begin
    
    NumYears := shrink(LocSec div SecPerYear);  { Don't worry about 100 and }
    NumLeap := ((NumYears - 1) div 4) + 1;      { 400 years.  PERQs won't be }
                                                { around that long. }
    if (LocSec - (NumYears * SecPerYear) - (NumLeap * SecPerDay)) < 0 then
        begin
        NumYears := NumYears - 1;
        NumLeap := ((NumYears - 1) div 4) + 1;
        end;
    LocSec := LocSec - (NumYears * SecPerYear) - (NumLeap * SecPerDay);
    Val.Year := shrink(NumYears);
    
    Val.Month := 1;
    for I := 1 to 11 do
        begin
        if I in [1, 3, 5, 7, 8, 10, 12] then 
            DpM := 31
        else
            begin
            if I = 2 then
                begin
                if (Val.Year mod 4 = 0) and
                   ((Val.Year mod 100 = 0) = (Val.Year mod 400 = 0)) then
                       DpM := 29
                else 
                       DpM := 28
                end
            else 
                DpM := 30;
            end;
        if LocSec >= (DpM * SecPerDay) then
            begin
            Val.Month := I + 1;
            LocSec := LocSec - (DpM * SecPerDay);
            end
        else
            goto 1;
        end;

1:
    Val.Day := shrink(LocSec div SecPerDay) + 1;
    LocSec := LocSec - ((Val.Day - 1) * SecPerDay);
    
    Val.Hour := shrink(LocSec div SecPerHour);
    temp := val.hour;
    LocSec := LocSec - (temp * SecPerHour);
    
    Val.Minute := shrink(LocSec div SecPerMin);
    LocSec := LocSec - (Val.Minute * SecPerMin);
    
    Val.Second := shrink(LocSec);
    
    CnvSecToStamp := Val;
    
    end;

function CnvStampToSec(Stamp: TimeStamp): OffsetStamp;
{---------------------------------------------------------------------------
{
{ Abstract:
{   Convert a time stamp to a number of seconds since 1-Jan-80 00:00:00.
{
{ Parameters:
{   Stamp is the timestamp that is to be converted.
{
{ Results:
{   Return the number of seconds that has passed between Stamp and the
{   time base.
{
{
{--------------------------------------------------------------------------}
  var NumLeap: integer;
      Val, temp: long;
      I, DpM: integer;
    begin

    Val := Stamp.Year * SecPerYear;

    NumLeap := ((Stamp.Year - 1) div 4) + 1;  { Don't worry about 100 and 400 }
    Val := Val + (NumLeap * SecPerDay);       { years.  PERQs won't be around.}

    for I := 1 to (Stamp.Month - 1) do
        begin
        if I in [1, 3, 5, 7, 8, 10, 12] then 
            DpM := 31
        else
            begin
            if I = 2 then
                begin
                if (Stamp.Year mod 4 = 0) and
                   ((Stamp.Year mod 100 = 0) = (Stamp.Year mod 400 = 0)) then
                       DpM := 29
                else DpM := 28
                end
            else 
                DpM := 30;
            end;
        Val := Val + (DpM * SecPerDay);
        end;

    Val := Val + ((Stamp.Day - 1) * SecPerDay);
    
    temp := Stamp.Hour;  { the following * overflows unless we explicitly 
                           coerce }

    Val := Val + (temp * SecPerHour);

    Val := Val + (Stamp.Minute * SecPerMin);
    
    Val := Val + Stamp.Second;
    
    CnvStampToSec := Val;
    
    end;

procedure GetPERQ2Local(var Stamp: TimeStamp);
{---------------------------------------------------------------------------
{
{ Abstract:
{   Obtain the local time from the PERQ-2 clock.  The local time is
{   generated using the GMT provided by the hardware clock and adding
{   in the time in the offset stored on disk.
{
{ Parameters:
{   Stamp will be set to be the current time stamp.  If no local offset
{   was found on the disk then all fileds of Stamp will be -1.
{
{ Exceptions:
{   if the current IO board is not an EIO then raise GTSNotPERQ2. 
{
{--------------------------------------------------------------------------}
  var GMT: TimeStamp;
      Offset, GMTSec, LocSec: OffsetStamp;
    begin
    if CF_IOBoard <> CF_EIO then raise GTSNotPERQ2;
    GetPERQ2GMT(GMT);
    GetPERQ2Offset(Offset);
    GMTSec := CnvStampToSec(GMT);
    LocSec := GMTSec - Offset;
    Stamp := CnvSecToStamp(LocSec);
    end;
    
        


procedure PutPERQ2Offset;
{---------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to write an offset from GMT on the disk.
{   It will read the current System time and create an offset file
{   that gives the offset of the current system time from the GMT
{   returned by hardware clock.
{
{ Exceptions:
{   if the current IO board is not an EIO then raise GTSNotPERQ2. 
{
{--------------------------------------------------------------------------}
  var GMT, Stamp: TimeStamp;
      F: file of OffsetStamp;
      OFFSet: OffsetStamp;   
    begin
    if CF_IOBoard <> CF_EIO then raise GTSNotPERQ2;
    GetPERQ2GMT(GMT);
    GetTStamp(Stamp);
    Offset := CnvStampToSec(GMT) - CnvStampToSec(Stamp);
    rewrite(F, OffsetFile);
    F^ := Offset;
    put(f);
    close(f);
    end.
{$r=}
