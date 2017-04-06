{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FloppyFormat;

{------------------------------------------------------------------
{
{ Abstract:
{
{    This module formats and test floppy disks for Floppy.
{    It was created from the March, 1981 program Floppy.
{
{  copyright (C) 1983  Three Rivers Computer
{------------------------------------------------------------------}

{------------------------------------------------------------------
{
{ Change Log:
{
{  3 Feb 84 Dirk Kalp V2.0
{ No change except to note that memory refresh is still performed
{ in procedure FormatFloppy.
{
{ 28 Oct 82 Roger Riggs V0.2
{ Changed to use new functions in IOFormat in Unit to allow
{ interleaved sectors
{ 
{ 27 Jan 82 DCF V0.1
{ Changed name from Format to FloppyFormat to conform to
{ Floppy naming conventions.
{
{ 15 Dec 81 DCF V0.0
{ Created Format from version V2.0 of Program Floppy.
{
{------------------------------------------------------------------}



Exports

Procedure FormatFloppy; 

Private

Imports FloppyDefs from FloppyDefs;
Imports UtilProgress from UtilProgress;
     
type
    FmtData = Packed Record
        Cyl : 0..255;           { cylinder number of this cylinder }
        Head : 0..255;          { head number 0/1 }
        Sector: 0..255;         { Sector number of this sector }
        Size: 0..255;           { size of this sector }
        end;
    FloppyBuffer = Packed record 
        case boolean of
            true: (Words: array[0..127] of integer);
            false: (SecInfo: array[1..26] of FmtData);
        end;

     pFloppyBuffer = ^FloppyBuffer;

var Cylinder, Sector, MaxCylinder, WordsPerSector: integer;
    Continue, HadError : boolean;
    Buffer: pFloppyBuffer;
    Status: IOStatPtr;
    SetStatus: DevStatusBlock;
    Address: Double;
    Seed: integer;
    DoubleDensity, BothHeads: Boolean;

Procedure Initialize;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Initialize state for formatting floppy.
{
{-----------------------------------------------------------------}

var DensWord: String;

begin

 New(0, 256, Buffer);
 New(Status);
 
 If Dens = DDens then DensWord := 'double' else DensWord := 'single';
 
 DoubleDensity := Dens = DDens;
 BothHeads := Sides = 2;

 SetStatus.ByteCnt := 3;
 SetStatus.FlpDensity := Shift(Ord(DoubleDensity),6);
 SetStatus.FlpHeads := Ord(BothHeads) + 1;
 SetStatus.FlpEnable := true;
 IOPutStatus(Floppy,SetStatus);

 If Dens = DDens then WordsPerSector := 128
 else WordsPerSector := 64;
 if Sides = 2 then MaxCylinder := 153
 else MaxCylinder := 76;

 { do a dummy read cause if we don't, the floppy won't work at all }

 Address[0] := 1; Address[1] := 1;
 UnitIO(Floppy, Recast(Buffer,IOBufPtr), IORead, WordsPerSector*2,
        Address, nil, Status);

end { Initialize };

Function Random: integer;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Generate a random number.
{
{ Results:
{    Return a random number.
{
{---------------------------------------------------------------------}
 begin { Random }
  Seed := 5;
  Seed := Abs(Seed);
  Seed := LXor( Seed, Shift( Seed, 4) );
  Seed := LXor( Seed, Shift( Seed, -11) );
  Random := Seed
 end { Random };
 
Procedure PrintError;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Print an error message.
{
{---------------------------------------------------------------------}
var
    ErrMsg :string;
 begin { PrintError }
  ErrMsg := IOErrString(Status^.SoftStatus);
  If Status^.SoftStatus = -21 then
  begin
       Writeln(Bell);                            
       Writeln('** Error: ',ErrMsg);
       Continue := False;
       Exit (PrintError);
  end;
  Writeln(Bell);                            
  Writeln('** Error: ',ErrMsg);
  Writeln('[Soft status = ', Status^.SoftStatus:1,
          ', Hard status = ', Status^.HardStatus:1:8,']');
  Writeln('** cylinder = ', Cylinder:1, ', sector = ', Sector:1);
  Writeln;
  HadError := true;
  Continue := GetConfirm ( NullIdleProc, True, 'Continue? ', 3, Switches) = 1;
 end { PrintError };
 
Procedure Flop(Cmd: IOCommands; Bytes: integer);
{-----------------------------------------------------------------------
{
{ Abstract:
{    Perform a floppy command.
{
{ Parameters:
{    Cmd is the floppy command that is to be done.
{
{    Bytes is the number of bytes that are to be transfered.
{
{---------------------------------------------------------------------}
 label 2;
 var I, J: integer;
     Zero: Double;
     ErrMsg :string;
 begin { Flop }

  for I := 1 to 3 do
   begin
    for J := 1 to 4 do
     begin
      UnitIO(Floppy,Recast(Buffer,IOBufPtr),Cmd,Bytes,Address,nil,Status);
      if (Status^.SoftStatus = IOEIOC) OR
         (Status^.SoftStatus = IOEDNW) then goto 2;
      ErrMsg := IOErrString(Status^.SoftStatus);
      Write('** Floppy Error: ', ErrMsg);
      Writeln('  [SoftStatus = ', Status^.SoftStatus:1,
              ', HardStatus = ', Status^.HardStatus:1,' ]');
      Writeln('Retry.....     Track: ', Address[1]:1,
              '     Sector: ', Address[0]:1);
     end;
    Zero[0] := 1;
    Zero[1] := 0;
    if I < 3
    then UnitIO(Floppy,Recast(Buffer,IOBufPtr),IOReset,0,Zero,nil,Status)
   end;
  Writeln('Abort*****     Track: ',Address[1]:1,'     Sector: ', Address[0]:1);
  2:
 end { Flop };
 
Procedure CheckBuffer;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Check the words read from the floppy.
{
{---------------------------------------------------------------------}
 var I, Tmp, Should: integer;
     Error: Boolean;
 begin { CheckBuffer }
  Tmp := Random;
  Seed := Buffer^.Words[0];
  Error := false;
  for i := 1 to WordsPerSector-1 do
   begin Should := Random;
    if Buffer^.Words[I] <> Should then
     begin Writeln('** data mismatch on read, buffer[', I:1, '] = ',
                   Buffer^.Words[I]:1, ', but ought to be ', Should:1);
      Error := true
     end
   end;
  if Error then
   begin Writeln;
    Writeln('** error, data mis-match on read,');
    Writeln('** cylinder = ', Cylinder:1, ', sector = ', Sector:1);
    HadError := true;
    Continue := GetConfirm ( NullIdleProc, True, 'Continue? ', 3, Switches) = 1;
   end;
  Seed := Tmp
 end { CheckBuffer };
 
 procedure WriteSequential; forward;
 procedure ReadSequential; forward;

Procedure FormatFloppy;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Format the floppy.
{
{---------------------------------------------------------------------}
 var i : integer;
    s : Integer;
 label 3;
 begin { Format }

  Verify := Verify;     { !!!! Memory Refresh !!!!! }
  LoadCurs;
  Initialize;
  Sector := 1;
  Address[0] := Sector;
  for Cylinder := 0 to MaxCylinder do
   begin
    for i := 1 to 26 do Buffer^.SecInfo[i].Sector := 0; 
    s := 1;
    for i := 1 to 26
    do  begin
        if s > 26 then s := s - 26;
        while Buffer^.SecInfo[s].Sector <> 0
        do begin s := s + 1; if s > 26 then s := s - 26 end;
        Buffer^.SecInfo[s].Cyl := Cylinder Mod 77;
        Buffer^.SecInfo[s].Head := Cylinder Div 77;
        Buffer^.SecInfo[s].Sector := i;
        Buffer^.SecInfo[s].Size := WordsPerSector Div 128; { 0/1 }
        s := s + Interleave;
        end;

    Repeat
     HadError := false;
     Continue := true;
     Address[1] := Cylinder;
     ComputeProgress (Cylinder, MaxCylinder);
     Flop(IOFormat,26*4);
     if Status^.SoftStatus <> IOEIOC then PrintError;
     if not Continue then goto 3;
     if FmtTest then
         begin
         Write('Cylinder ', Cylinder:1, '  Writting');
         Continue := true;
         WriteSequential;
         if not Continue then goto 3;
         if not HadError then
          begin
           Write(' Reading');
           Continue := true;
           ReadSequential;
           if not Continue then goto 3;
          end;
         WriteLn;
         end;
    until not HadError;
   end;
  3: Write(Bell);  {beep when done}
 QuitProgress;
 Dispose(Status);
 Dispose(Buffer);
 end { FormatFloppy };
 
Procedure WriteSequential;
 {-----------------------------------------------------------------------
 {
 { Abstract:
 {    Do a sequential write of the floppy sectors with random data.
 {
 {-----------------------------------------------------------------------}
 label 4;
 var I, First: integer;
 begin { WriteSequential }
  Continue := true;
  First := 1;
  Sector := 1;
  repeat
    Address[0] := Sector;
    Address[1] := Cylinder;
    for I := 0 to WordsPerSector-1 do Buffer^.Words[I] := Random;
    Flop(IOWrite, WordsPerSector * 2);
    if Status^.SoftStatus <> IOEIOC then PrintError;
    if not Continue then goto 4;
    Sector := Sector + Interlace;
    if Sector > 26 then
      begin First := First + 1;
      Sector := First
      end
    until First > Interlace;
  4:
 end { WriteSequential };
 
Procedure ReadSequential;
 {-----------------------------------------------------------------------
 {
 { Abstract:
 {    Do a sequential read of the floppy sectors.
 {
 {-----------------------------------------------------------------------} 
 label 5;
 var First: integer;
 begin { ReadSequential }
  Continue := true;
  First := 1;
  Sector := 1;
  repeat
    Address[0] := Sector;
    Address[1] := Cylinder;
    Flop(IORead, WordsPerSector * 2);
    if Status^.SoftStatus <> IOEIOC then PrintError;
    if not Continue then goto 5;
    CheckBuffer;
    if not Continue then goto 5;
    Sector := Sector + Interlace;
    if Sector > 26 then
      begin First := First + 1;
      Sector := First
      end
  until First > Interlace;
  5:
 end { ReadSequential }.
