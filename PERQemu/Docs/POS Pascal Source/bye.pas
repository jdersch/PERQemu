{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program bye;
{----------------------------------------------------------------------------
{
{ Abstract:
{    Bye is the logout program for the Perq O.S.  Can turn machine off
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corperation,  1981, 1982, 1983
{
{---------------------------------------------------------------------------}

{ 27 APR 83  V2.4  Eric Beattie
{ Put in 3rcc check for micropolis to remain compatible
{
{ 13 Apr 83  V2.3  Chris Hughes
{ Bye Wait doesn't need to move heads for cio micropolis disk.
{
{ 29 Jun 81  V2.2  John Strait
{ Make Bye disable swapping so that swap files get deleted before turning
{ off the machine.
{ }

{  23-Jun-81  V2.1  Brad Myers
{  Allow arguments as switches in addition to as parameters
{  Output address in unsigned decimal.
{ }

{  2-Jun-81  V2.0  Brad Myers
{  Fixed for when Goodby.bin not found
{  Add code to move disk head to center for shipping.
{  Added help for arguments
{  Change cursor function to Normal
{ }

{ 19-Mar-81  V1.1  Brad Myers
{  Change to PERQ_String.
{ }

{  4-Mar-81  V1.0  Don Scelza
{  Created Bye.
{ }

imports System from System;
imports Clock from Clock;
imports ControlStore from ControlStore;
imports CmdParse from CmdParse;
Imports DiskIO from DiskIO;
Imports IO_Unit from IO_Unit;
Imports IO_Others from IO_Others;
Imports Stream From Stream;
Imports PERQ_String From PERQ_String;
Imports Memory from Memory;
Imports Diskdef from Diskdef;   {using ciodisktype}

var Str, Broke: String;
    I, indx: Integer;
    F: MicroFile;
    CmdTable: CmdArray;
    
const NumLines = 5;
      offIndx = 1;
      waitIndx = 2;
      helpIndx = 3;
      NumCmds = 3;

Procedure DoWait;
{---------------------------------------------------------------
 Abstract: Moves heads to center and waits
---------------------------------------------------------------}
   var  addr : DiskAddr;
        ptr : ptrDiskBuffer; 
        hptr : ptrHeader;
        dskcommand : DiskCommand;
        dev: integer;
        
   begin
   
   if (not EIOflag) and (ciodisktype <> ciomicropolis) then
    begin
     if IO24MByte then dev := 1 else dev := 0;
     addr := LastDiskAddr(RECAST(dev, DeviceType));
     WriteLn('Moving disk heads to center of disk (address ',
            AddrToField(addr):1:-10,')');
   
     NEW(0, 256, ptr);
     NEW(0, 4, hptr);
     DiskIO(addr, ptr, hptr, DskRead)
    end;
   
   WriteLn;
   WriteLn('** Turn off machine now.  **');
   while true do;
   end; {DoWait}
   
Handler ResetError(fileName: PathName);
  begin
  WriteLn('** Turn off machine now.  **');
  while true do;
  exit(Bye);
  end;

begin

CmdTable[offIndx] := 'OFF    - Turns off machine after logging out.';
CmdTable[waitIndx] := 'WAIT   - Moves disk heads to center and waits until ^C or power down.';
CmdTable[helpIndx] := 'HELP   - This message.';

RemDelimiters(UsrCmdLine, ' /', Broke);
GetSymbol(UsrCmdLine, Str, ' /', Broke); {Bye}
RemDelimiters(UsrCmdLine, ' /', Broke);
GetSymbol(UsrCmdLine, Str, ' ',Broke);
CnvUpper(Str);
if str = '' then indx := 0
else begin
     Indx := UniqueCmdIndex(Str, CmdTable, NumCmds);
     case indx of
           offIndx, waitIndx: ;  {do these later}
           helpIndx: begin
                     WriteLn;
                     WriteLn('Bye is used to log off of the PERQ');
                     WriteLn('The Bye command line is of the form: ');
                     WriteLn('         Bye [param]');
                     WriteLn('Where param is one of the following:');
                     WriteLn;
                     for i := 1 to NumCmds do WriteLn(cmdTable[i]);
                     WriteLn;
                     exit(Bye);
                     end;
           otherwise: begin
                      WriteLn('** Illegal argument to Bye.  Type "Bye Help" for help');
                      exit(Bye);
                      end;
          end;
     end;

if SwappingAllowed then
    begin
    savedSwapID := swapID;
    shouldReEnableSwapping := true;     { in case machine is not turned off }
    DisableSwapping;                    { get rid of swap files }
    end;

write('User ', CurUserName, ' logged off at ');
GetTString(Str);
writeln(Str);

UsrCmdLine := 'Bye';
DefCursFunct := Ord(CTNormal);

ShellName :=  Concat(LogConst, Concat(StrVersion, '.Run'));
RFileName := ShellName;

for I := 1 to NumLines do writeln;

IOSetFunction(CTNormal);

if indx = offIndx then
    begin
    reset(F, 'GoodBy.Bin');
    LoadControlStore(F);
    JumpControlStore(#5000);
    end
else if indx = waitIndx then DoWait;
   
end.

