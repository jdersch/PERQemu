{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Floppy;

{--------------------------------------------------------------------------
{
{ Abstract:
{    This program is used to deal with floppy disks.  It can be
{    used to test and format floppies.
{
{  copyright 1981, 1982, 1983  Three Rivers Computer Corporation
{
{-------------------------------------------------------------------------}


{ 16 Nov 82  V2.2  Bill Braucher
{ Fixed names for 14-character compiler.

{  3 Jun 81  V2.1  Brad Myers
{ Changed IO import to IO_Unit
{ Added Bell on error or finish format
{ }

{ 16 Mar 81  V2.0  Don Scelza
{ Created the program Floppy from the program TestFloppy.
{ }



 imports IOErrors from IOErrors;
 imports IO_Unit from IO_Unit;
 imports PERQ_String from Perq_String;
 imports CmdParse from CmdParse;
 imports Screen from Screen;
 
const Version = '2.2';
      Bell = Chr(7);
      
type FloppyBuffer = array[0..127] of integer;
     pFloppyBuffer = ^FloppyBuffer;

var DoubleDensity, BothHeads: boolean;
    Cylinder, Sector, MaxCylinder, WordsPerSector: integer;
    StartCylinder, EndCylinder: integer;
    Continue, HadError, InFormat: boolean;
    Buffer: pFloppyBuffer;
    Status: IOStatPtr;
    SetStatus: DevStatusBlock;
    Address: Double;
    Interlace, Limit: integer;
    Seed: integer;
    CmdTable: CmdArray;
    Broke, InLine, Cmd: String;
    Running: Boolean;
    
const
    FormatIndex = 1;
    SeqReadIndex = 2;
    SeqWriteIndex = 3;
    RandomIndex = 4;
    QuitIndex = 5;
    HelpIndex = 6;
    NumCmds = 6;
    Delimiters = ' ';

label 1;
    
 
procedure InitCmds;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to initialize the command table.
{
{---------------------------------------------------------------------}
    begin
    CmdTable[FormatIndex] :=   'FORMAT    Format the floppy.';
    CmdTable[SeqReadIndex] :=  'READ      Read sequential sectors of the floppy.';
    CmdTable[SeqWriteIndex] := 'WRITE     Write sequential sectors of the floppy.';
    CmdTable[RandomIndex] :=   'RANDOM    Do random reads and writes.';
    CmdTable[HelpIndex] :=     'HELP      Print this message';
    CmdTable[QuitIndex] :=     'QUIT      Leave the program.';
    end;


 function Random: integer;
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
  Seed := Abs(Seed);
  Seed := LXor( Seed, Shift( Seed, 4) );
  Seed := LXor( Seed, Shift( Seed, -11) );
  Random := Seed
 end { Random };
 
 
 function Ask(S: String): Boolean;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Ask a simple yes or no question.
{
{ Parameters:
{    S is the question that is to be asked.
{
{ Results:
{    Return true if the user said yes.  Return false otherwise.
{
{---------------------------------------------------------------------}
 var Answer: Char;
 begin { Ask }
  repeat Write(S); Readln(Answer)
  until Answer in ['y', 'Y', 'n', 'N'];
  Ask := Answer in ['y', 'Y']
 end { Ask };
 
 
 procedure Error;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Print an error message.
{
{---------------------------------------------------------------------}
 begin { Error }
  Writeln(Bell);
  Writeln('*** error, soft status = ', Status^.SoftStatus:1,
          ', hard status = ', Status^.HardStatus:1:8);
  Writeln('*** cylinder = ', Cylinder:1, ', sector = ', Sector:1);
  HadError := true;
  Continue := Ask('continue? ')
 end { Error };
 
 
 procedure Flop(Cmd: IOCommands; Bytes: integer);
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
 label 1;
 var I, J: integer;
     Zero: Double;
 begin { Flop }
  for I := 1 to 5 do
   begin
    for J := 1 to 5 do
     begin
      UnitIO(Floppy,Recast(Buffer,IOBufPtr),Cmd,Bytes,Address,nil,Status);
      if Status^.SoftStatus = IOEIOC then goto 1;
      Writeln('*** error, softstatus = ', Status^.SoftStatus:1, ', retry')
     end;
    Zero[0] := 1;
    Zero[1] := 0;
    if I < 5 then UnitIO(Floppy,Recast(Buffer,IOBufPtr),IOReset,0,Zero,nil,Status)
   end;
  1:
 end { Flop };
 
 

 procedure CheckBuffer;
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
  Seed := Buffer^[0];
  Error := false;
  for i := 1 to WordsPerSector-1 do
   begin Should := Random;
    if Buffer^[I] <> Should then
     begin Writeln('*** data mismatch on read, buffer[', I:1, '] = ',
                   Buffer^[I]:1, ', but ought to be ', Should:1);
      Error := true
     end
   end;
  if Error then
   begin Writeln;
    Writeln('*** error, data mis-match on read,');
    Writeln('*** cylinder = ', Cylinder:1, ', sector = ', Sector:1);
    HadError := true;
    Continue := Ask('continue? ')
   end;
  Seed := Tmp
 end { CheckBuffer };
 
 
 procedure WriteSequential; forward;
 procedure ReadSequential; forward;

 procedure Format(DoTest: Boolean);
{-----------------------------------------------------------------------
{
{ Abstract:
{    Format the floppy.
{
{ Parameters:
{     If DoTest is true then check each cylinder after we format it.
{
{---------------------------------------------------------------------}
 var FmtCylinder: integer;
 label 1;
 begin { Format }
  Writeln;
  Writeln('begin formatting');
  Writeln;
  InFormat := true;
  Sector := 1;
  Address[0] := Sector;
  for FmtCylinder := 1 to MaxCylinder do
   begin 
    Repeat
     HadError := false;
     Continue := true;
     Cylinder := FmtCylinder;
     Address[1] := Cylinder;
     Writeln('cylinder ', FmtCylinder:1,' format started');
     Flop(IOFormat,0);
     if Status^.SoftStatus <> IOEIOC then Error;
     if not Continue then goto 1;
     if DoTest then
         begin
         StartCylinder := FmtCylinder;
         EndCylinder := FmtCylinder;
         Continue := true;
         WriteSequential;
         if not Continue then goto 1;
         if not HadError then
          begin
           Continue := true;
           ReadSequential;
           if not Continue then goto 1;
          end
         end;
    until not HadError;
    Writeln('cylinder ', FmtCylinder:1,' format complete');
   end;
  1: InFormat := false;
 Write(Bell);  {beep when done}
 end { Format };

 

 procedure WriteSequential;
 {-----------------------------------------------------------------------
 {
 { Abstract:
 {    Do a sequential write of the floppy sectors with random data.
 {
 {-----------------------------------------------------------------------}
 label 1;
 var I, First: integer;
 begin { WriteSequential }
  if not InFormat then Writeln else Write('    ');
  Writeln('begin sequential write');
  if not InFormat then Writeln;
  Continue := true;
  for Cylinder := StartCylinder to EndCylinder do
   begin First := 1;
    Sector := 1;
    repeat
     Address[0] := Sector;
     Address[1] := Cylinder;
     for I := 0 to WordsPerSector-1 do Buffer^[I] := Random;
     Flop(IOWrite,WordsPerSector + WordsPerSector);
     if Status^.SoftStatus <> IOEIOC then Error;
     if not Continue then goto 1;
     Sector := Sector + Interlace;
     if Sector > 26 then
      begin First := First + 1;
       Sector := First
      end
    until First > Interlace;
    if InFormat then Write('    ');
    Writeln('cylinder ', Cylinder:1,' write complete')
   end;
  1:
 end { WriteSequential };

 

 procedure ReadSequential;
 {-----------------------------------------------------------------------
 {
 { Abstract:
 {    Do a sequential read of the floppy sectors.
 {
 {-----------------------------------------------------------------------} 
 label 1;
 var First: integer;
 begin { ReadSequential }
  if not InFormat then Writeln else Write('    ');
  Writeln('begin sequential read');
  if not InFormat then Writeln;
  Continue := true;
  for Cylinder := StartCylinder to EndCylinder do
   begin First := 1;
    Sector := 1;
    repeat
     Address[0] := Sector;
     Address[1] := Cylinder;
     Flop(IORead,0);
     if Status^.SoftStatus <> IOEIOC then Error;
     if not Continue then goto 1;
     CheckBuffer;
     if not Continue then goto 1;
     Sector := Sector + Interlace;
     if Sector > 26 then
      begin First := First + 1;
       Sector := First
      end
    until First > Interlace;
    if InFormat then Write('    ');
    Writeln('cylinder ', Cylinder:1, ' read complete')
   end;
  1:
 end { ReadSequential };
  
 
  
 procedure RandomReadWrite( Limit: integer );
 {-----------------------------------------------------------------------
 {
 { Abstract:
 {    Do random reads and writes of the floppy.
 {
 {-----------------------------------------------------------------------} 
 label 1;
 var I, J: integer;
 begin { RandomReadWrite }
  Writeln;
  Writeln('begin ', Limit:1, ' random reads and writes');
  Writeln;
  Continue := true;
  for I := 1 to Limit do
   begin Sector := abs(Random) mod 26 + 1;
    Cylinder := abs(Random) mod MaxCylinder + 1;
    Address[0] := Sector;
    Address[1] := Cylinder;
    if Odd(Random div #40) then { write }
     begin
      for J := 0 to WordsPerSector-1 do Buffer^[J] := Random;
      Flop(IOWrite,WordsPerSector + WordsPerSector);
      if Status^.SoftStatus <> IOEIOC then Error;
      if not Continue then goto 1
     end
    else
     begin
      Flop(IORead,0);
      if Status^.SoftStatus <> IOEIOC then Error;
      if not Continue then goto 1;
      CheckBuffer;
      if not Continue then goto 1
     end
   end;
  1:
 end { RandomReadWrite };
  
 
procedure Help;
{-------------------------------------------------------
{
{ Abstract: 
{    Print a help message.
{
{--------------------------------------------------------}
var I: integer;
    begin
    writeln('Floppy is a program that is used to format and test floppies.');
    writeln(' The valid commands are:');
    for I := 1 to NumCmds do
        writeln('    ',CmdTable[I]);
    end;

  
begin { Floppy }

 New(Buffer);
 New(Status);

 ChangeTitle(Concat(Concat('Floppy version ',Version), '    Type HELP if you need it.'));

 DoubleDensity := Ask('double density? ');
 BothHeads := Ask('both heads? ');
 Write('interlacing factor for writes? '); Readln(Interlace);

 SetStatus.ByteCnt := 3;
 SetStatus.FlpDensity := Shift(Ord(DoubleDensity),6);
 SetStatus.FlpHeads := Ord(BothHeads) + 1;
 SetStatus.FlpEnable := true;
 IOPutStatus(Floppy,SetStatus);

 if DoubleDensity then WordsPerSector := 128
 else WordsPerSector := 64;
 if BothHeads then MaxCylinder := 153
 else MaxCylinder := 76;

 { do a dummy read cause if we don't, the floppy won't work at all }

 Address[0] := 1; Address[1] := 1;
 UnitIO(Floppy,Recast(Buffer,IOBufPtr),IORead,0,Address,nil,Status);
 
 Write('please enter a seed for the random number generator: '); Readln(Seed);
 
 Running := true;
 InitCmds;
 writeln;
 repeat
  write('*');
  readln(InLine);
  Remdelimiters(InLine, Delimiters, Broke);
  GetSymbol(InLine, Cmd, Delimiters, Broke);
  CnvUpper(Cmd);
  if Length(Cmd) = 0 then goto 1;
  case UniqueCmdIndex(Cmd, CmdTable, NumCmds) of 
    FormatIndex: begin
         if Ask('test during formatting? ') then
           Format(true)
         else Format(false);
        end;
        
    SEQWriteIndex: begin
        StartCylinder := 1;
        EndCylinder := MaxCylinder;
        WriteSequential;
       end;
       
   SEQReadIndex: begin
        StartCylinder := 1;
        EndCylinder := MaxCylinder;
        ReadSequential;
       end;
       
   RandomIndex: begin
        StartCylinder := 1;
        EndCylinder := MaxCylinder;
        Write('how many? ');
        Readln(Limit);
        RandomReadWrite(Limit);
       end;
       
   QuitIndex: Running := false;
   
   HelpIndex: Help;
   
   otherwise: begin
       writeln(Cmd, ' is not a valid command.   Type HELP if you need it.');
       end;
   
  end;

1:
 until (not Running);
 
end { TestFloppy }.
