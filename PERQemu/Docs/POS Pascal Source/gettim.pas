{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module GetTimeStamp;
{-----------------------------------------------------------------------------
{
{       GetTimeStamp - Perq get time routine.
{       J. P. Strait       1 Feb 81.
{       Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       GetTimeStamp implements the read-time-as-TimeStamp function for the
{       Clock module.  See the Clock module for more details.
{
{ Design:
{       GetTimeStamp is a separate module so that it may be imported into the
{       resident system without importing all the other Clock routines.  Once
{       virtual memory is implemented, GetTimeStamp and Clock should be merged
{       into a single module.
{
{-----------------------------------------------------------------------------}

{ V1.4   1 Jun 81  Brad Myers.
{ Add comments.
{ }

{ V1.3  18 May 81  Brad Myers.
{ Change IO to IO_Others.
{ }

{ V1.2   2 Mar 81  John Strait.
{ Don't export IO.
{ }

{ V1.1   2 Mar 81  John Strait.
{ Add title, copyright, abstract.
{ Add version number.
{ Add change history retroactively.
{ }

{ V1.0   1 Feb 81  John Strait.
{ Start file.
{ }


{/////////////////////////} Exports {\\\\\\\\\\\\\\\\\\\\\\\\}

const GetTSVersion = '1.4';


type TimeStamp = packed record
       { the fields in this record are ordered this way to optimize bits }
       Hour:   0..23;
       Day:    1..31;
       Second: 0..59;
       Minute: 0..59;
       Month:  1..12;
       Year:   0..63;  { year since 1980 }
       end;

     TimeReference = record
                       Lower: Integer;
                       Upper: Integer
                       end;

procedure GetTStamp( var Stamp: TimeStamp );


var PastStamp: TimeStamp;
    Past: TimeReference;

{/////////////////////////} Private {\\\\\\\\\\\\\\\\\\\\\\\\}

imports IO_Others from IO_Others;


{$R-}


procedure GetTStamp( var Stamp: TimeStamp );
{---------------------------------------------------------------------
 Abstract: returns a timeStamp for the current time
 Parameters: Stamp is set to be the stamp for the current time
---------------------------------------------------------------------}
var DoublePresent: Double;
    Present: TimeReference;
    Elapsed, T, DpM, Y: Integer;
    Done: Boolean;
  
  
  function GE( X, Y: Integer ): Boolean;
   {---------------------------------------------------------------------
    Abstract: does an unsigned >=
    Parameters: X and Y are integers to compare
    Returns: True if x >= y (unsigned)
   ---------------------------------------------------------------------}
  begin
    GE := ((X < 0) = (Y < 0)) = (X >= Y)
  end;


begin { GetTStamp }
  IOGetTime(DoublePresent);
  Present.Lower := DoublePresent[0];
  Present.Upper := DoublePresent[1];
  Done := False;
  with PastStamp do
    repeat
      { Restrict Elapsed time to the range 0..32767.  This avoids problems of
        double precision arithmetic and allow dispensing with div's and mod's
        after the Minute level since 32767 60ths of a second is between 9 and
        10 minutes. }
      if GE(Present.Lower, Past.Lower) then
        if Present.Upper = Past.Upper
          then Elapsed := Present.Lower - Past.Lower
        else Elapsed := 32767
      else
        if Present.Upper = Past.Upper + 1
          then Elapsed := Present.Lower - Past.Lower
        else Elapsed := 32767;
      if Elapsed < 0 then Elapsed := 32767;
      Elapsed := Elapsed div 60;
      if Elapsed = 0 then Done := true
      else
        begin
          T := Past.Lower + Elapsed * 60;
          if (Past.Lower < 0) and (T >= 0) then { carry into upper part }
            Past.Upper := Past.Upper + 1;
          Past.Lower := T;
          T := Second + Elapsed;
          if T >= 60 then
            begin Second := T mod 60;
              Elapsed := T div 60;
              T := Minute + Elapsed;
              if T >= 60 then
                begin Minute := T - 60;
                  T := Hour + 1;
                  if T >= 24 then
                    begin Hour := 0;
                      T := Day + 1;
                      if Month in [1, 3, 5, 7, 8, 10, 12] then DpM := 31
                      else
                        begin Y := Year + 1980;
                          if Month = 2 { Feb } then
                            if (Y mod 4 = 0) and
                               ((Y mod 100 = 0) = (Y mod 400 = 0)) then
                              { leap year } DpM := 29
                            else DpM := 28
                          else DpM := 30
                        end;
                      if T > DpM then
                        begin Day := 1;
                          T := Month + 1;
                          if T > 12 then
                            begin Month := 1;
                              Year := Year + 1
                            end
                          else Month := T
                        end
                      else Day := T
                    end
                  else Hour := T
                end
              else Minute := T
            end
          else Second := T
        end
  until Done;
  Stamp := PastStamp
end { GetTStamp }.
