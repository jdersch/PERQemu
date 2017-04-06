{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Shell;

{--------------------------------------------------------------------
{
{ Abstract:
{    This program interprets commands for the Perq Operating System.
{    When it exits, the name of the next program to run is
{    left in RFileName.
{
{ Copyright (C) Three Rivers Computer Corporation,  1981, 1982, 1983
{
{---------------------------------------------------------------------}



{----------------------------------------------------------------------
{ The Shell is a user program that is called by the system to interpret
{ commands typed by the user.  The Shell must do the following things
{ if it is to perform correctly:
{
{    a) the command typed by the user must be placed into the
{       system variable UsrCmdLine.
{
{    b) when the Shell exits it must have placed the name of the 
{       next program to be run into the system variable RFileName.
{
{ This program may be replaced by the user with any program that
{ he wishes.  To do this a program must change both of the system
{ variables RFileName and ShellName.  Once this is done the loader
{ will load the program whose name is in ShellName as the system shell.
{ The new user program can then set RFileName to cause a program
{ to be loaded.  The name of the system shell is in the system const
{ ShellConst.
{

{----------------------------------------------------------------------
{ Change log:
{
{ 27 Oct 83 V4.0  Sandeep Johar
{ Setup the 5.25" disk parameters in the OS when mounting.
{
{ 20 May 83 V3.91 Sandeep Johar
{ Initialize the file system floppy to be single density while mounting.

{ 10 Feb 83 V3.9 Brad Myers
{ Fixed for landscape monitor: window holds more than 80 characters in title
{ and default screen size.

{ 16 Nov 82 V3.8 Bill Braucher
{ Fixed names for 14-character compiler.

{ 17 May 82 V3.7 Michael R. Kristofic
{ Fix screensize help message.

{ 17 May 82 V3.6 Ellen Colwell
{ Path wasn't being displayed correctly in the title line.

{ 13 May 82 V3.5 Ellen Colwell
{ Fix the path bug that crashes system by using D. Golub's DoPath routine
{ for MPOS.  Also set CtlSPending=false in CtlCAbort handler instead of
{ doing a keyboard reset.  This should fix that annoying feature of losing
{ typeahead when ^C typed.

{ 27 Jan 82 V3.4 Brad Myers
{ Add Helper Module.
{ Move DoSwap into its own module.
{ Move PointAllowed check into PopCmdParse module.


{ 26 Jan 82 V3.3 Brad Myers
{ foo~bar not changed to "foo bar" where foo is a predefined command.
{    by using new cmdParse routine NextString.
{ Put default file before a "," if useDefault.


{ 21 Jan 82 V3.2 Brad Myers
{ Check for @ if UseCmd.
{ Remove isFloppy test.
{ Message if Tablet unplugged.
{ Moved definitions of control records into new file ShellDefs.Pas.
{ /Help to path.
{ Fixed help from Profile.
{ * in filename from profile means use system version number
{ PointAllowed switch from System.


{ 5 Jan 82 V3.1 Brad Myers
{ Fixed user interface for all Shell-implemented utilities.
{ Changed cmd record to be 3 words smaller.
{

{ 6 Dec 81 V3.0 WJH
{  General changes:
{    remove most commands;  use new command table and PopCmdParse
{    prompt with special char
{    check for table entry (and use it) before checking for file
{    change all errors to ** form
{    if no other help available, convert 'HELP x' to 'x/HELP'
{    if there are command files, but InCmdFile is False, ExitAllCmdFiles
{          (for Scrounge)
{    StreamResetKeyBoard (just in case somebody did Read instead of ReadLn)
{  Changes to specific commands:
{    Run only tries to run the named file  (doesn't process cmds)
{    read text for '?' from profile
{    implement ReRun command to use default file and new args
{    path prompts for arg if none given
{    mount and dismount take abbreviated arguments
{    ScreenSize accept parameters and allow 1..8 for 128..1024
{    Allow Yes & No as arguments to Swap and Statistics
{ }

{ 3 Dec 81   V2.1   WJH
{   Change to use Ctrl
{   Change BigStr to PString

{
{ 13-Aug-81  V2.0   BAM
{  1) Added popup windows.
{  2) Made time default a compile time constant.
{  3) Removed CmdParse stuff and imported that module.
{  4) Added user-defined items to cmd Table
{  5) Move initialization to a different module so can swap
{  6) Changed RUN to always run the default file and not set the default file
{  7) Mount and dismount will ask for arg if not supplied
{  8) Changed help message to tell about Press
{ }

{
{ 7-Aug-81   V1.19   DAS
{ Added the code to update the system time.  Also added code for
{ help files.
{ }

{
{ 30-Jun-81  V1.18   BAM
{  Remove setup to allow automatic re-enabling of swapping after scavenger.
{  Change so swap + doesn't ask to where but just uses default.
{  Use new variables to determine default screen on and comp for scav, comp,
{     etc.
{  Use these defaults as the defaults for ScreenSize
{ }

{
{  2-Jun-81  V1.17   BAM
{  Add setup to allow automatic re-enabling of swapping after scavenger.
{ }

{
{ 23-May-81  V1.16   JPS
{   Add file parameter to StreamKeyBoardReset procedure call.
{   Clear CtrlCPending in CtlCAbort handler.
{ }

{
{ 14-May-81  V1.15   BAM
{   1) Catch exceptions for File not found and string too long
{   2) Break MainBody into procedures.
{   3) Type uses last file specified
{   4) Reorder commands alphabetically using indices for commands
{   5) Catch CtlCAbort signal so faster back into shell
{   6) Catch StrLong for filenames
{   7) Path checks to make sure can still find shell
{   8) Added "RUN" command
{ }

{
{ 18-Apr-81  V1.14   JPS
{ Implement timing statistics.
{ Fix minor bug in command processing: set HaveCmd to False at beginning of
{   each case rather than at end.
{ }

{
{ 31-Mar-81  V1.13   BAM
{ Added SetPath code to Shell and changed import to FileSystem instead of 
{ Fileutils.  Put RunFile Name at right of Title line instead of over default
{ path; remove extra :: bug after cmdFile over; default extention for cmd files
{ different message for non-unique cmd; made NumCmds a constant; can
{ abbreviate setsearch
{ }

{
{ 19-Mar-81  V1.12   BAM
{ PERQ_String.
{ }

{
{ 17-Mar-81  V1.11   BAM
{ Put current path name rather than user name in header.
{ }

{
{ 16-Mar-81  V1.10   BAM
{ Type Compatibility.
{ }

{
{  9-Mar-81  V1.9   BAM
{ Fixed Path so no message and prints old path. Imports fileutils.
{ Deleted CnvUpper.
{ }

{  4-Mar-81  V1.8   DAS
{ Added the Details command.  Added code to display the user
{ name in the title window.
{ }

{  3-Mar-81  V1.7   DAS
{ Added code to print command read from command file.  Added code
{ to handle ! as first character on a line as a comment.
{ }

{ 26-Feb-81  V1.6   DAS
{ Added the rename command.
{}

{ 25-Feb-81  V1.5   DAS
{ Fixed Mount, Dismount and Path so that they do not exit the command loop.
{ Added a warning to the path command.
{ }

{ 24-Feb-81  V1.4   JPS
{    Add Scavenger, Type, Directory, Delete, Copy, Mount, DisMount commands to
{       call the new file utilities.
{    Automatically shrink the screen for the compiler and scavenger.
{    Add an explicit ScreenSize command.
{    Fix the @ command.
{ }

{ 20-Feb-81  V1.3   JPS
{    Recreate window 0 if it doesn't have a title.
{    Add RefreshWindow(0).
{    Add LogIn command.
{ }

{ 16-Feb-81  V1.2   DAS & JPS
{    Added the code for time of day and windows.
{ }

{ 16-Feb-81  V1.1   DCF
{    Module CmdParse is now part of Shell. This includes the
{    Constants CmdPVersion and MaxCmds, the Type CmdArray,
{    the Function UniqueCmdIndex, and the three procedures
{    CnvUpper, RemDelimiters, and Getsymbol.
{ }

{ 12-Feb-81  V1.0  Diana Connan Forgy
{    Shell is a command interpreter and is 
{    compatable with new System, PString, and
{    Compiler.
{ }
{---------------------------------------------------------------------}



{///////////////////////////////} EXPORTS {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

imports CmdParse from CmdParse;

Function CheckHelp(switches: pSwitchRec): boolean;
Function CheckTwo(name: CString; var ans: boolean; s1, s2: string): boolean;

{///////////////////////////////} PRIVATE {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

const
     ShellVersion = 'V3.8';
                  
Imports Clock       from Clock;
Imports PERQ_String from Perq_String;
Imports System      from System;
Imports InitShell   from InitShell;
Imports PopCmdParse from PopCmdParse;
Imports Screen      from Screen;
Imports FileUtils   from FileUtils;  {for FSRemoveDots}
Imports IO_Others   from IO_Others;  {using IOKeyClear}
Imports IO_Unit     from IO_Unit;    {using CtlSPending}
Imports Profile     from Profile;
Imports FileAccess  from FileAccess; {using NotAFile}
Imports ShellDefs   from ShellDefs;
Imports Stream      from Stream;     {using StreamKeyBoardReset, StreamSegment}
Imports Helper      from Helper;
Imports DoSwap      from DoSwap;
Imports DiskParams  from DiskParams;
Imports Volumesystem From VolumeSystem;

Const
    Debug = false;
    DotDot = Chr(#214);  {^L with high bit set gets .... char in font}
    HelpIndex = '>HelpDir>Shell.Index';
    
Var
    Ctrl: pCtrlRec;
    Cmd, Args: PString;
    NoRunFile: Boolean;
    TitleStr: STitStrType;
    TimStr: TimeString;

    TmpSSize, right: Integer;
    TmpSComplemented: Boolean;
    TmpSOff: Boolean;
    SizeSpecified: Boolean;
    
   
Procedure Help;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is called when the user requests help.
{
{ Environment:
{    CmdLine is assumed to contain the command typed by the
{    user with HELP removed.
{
{ Side Effects:
{    This procedure will change CmdLine.
{
{-----------------------------------------------------------------------}
Var 
       Desired, s: string;
       FileName: PathName;  
       Entry: PString;
       fid: FileID;
       Dum: Integer;
       isSw: Boolean;
       EndCH: char;
label 1;
 Handler PNotFound(f:string);
    begin
    goto 1;
    end;

    begin
    EndCH := NextIdString(Args, Desired, isSw);
    if isSw then
        begin
        ConvUpper(Desired);
        if POS('HELP', Desired)=1 then begin
            Desired := '';
            isSw := False;
            EndCh := CCR;
            end;
        end
    else ConvUpper(Desired);
    if not (EndCH in [' ', CCR]) or isSw then 
       writeln('** Use:   "Help name" or "Help"')
    else If Length(Desired) = 0 Then
        if FSInternalLookUp(HelpIndex, dum, dum) <> 0 then GiveHelp(HelpIndex)
        else begin
             WriteLn('    This is the Three Rivers Computer Corporation PERQ.');
             WriteLn('    Commands are either a System command name or a Run File name.');
             WriteLn('    Type "?" for a list of valid system commands or press');
             WriteLn('    down with the pen or puck for a menu of commands.');
             WriteLn('    When the menu is displayed, press in the gauge area at');
             WriteLn('    the bottom and move left or right while pressed to');
             WriteLn('    scroll the menu.');
             WriteLn('    Type "<name of .RUN file>" to run a program.');
             WriteLn('    Type "Help <Command>" for help on <Command>.');
             End
    Else
        Begin
        s := ConCat(Desired,'.HELP');
        FileName := Concat('>HelpDir>', s);
        fid := FSInternalLookUp(FileName, dum, dum);
        if fid = 0 then 
           begin
           FileName := s;
           fid := FSLookUp(FileName, dum, dum);
           end;
        if fid = 0 then
            begin
            PFileInit(CurPFile, 'HaveHelp');
            Entry := PFileEntry;
            while Entry <> '' do
                begin
                EndCH := NextIdString(Entry, s, isSw);
                ConvUpper(s);
                if s = Desired then begin
                    Cmd := Desired;
                    Args := '/HELP';
                    NoRunFile := False;
                    exit(Help);
                    end
                else
                    Entry := PFileEntry;
                end;
            end;
        
   1:   if fid = 0 then 
           WriteLn('** No help on ',Desired,' available.')
        else begin
           Args := FileName;
           Cmd := 'TypeFile';
           NoRunFile := False;
           end;
        End;
        
    End {Help};
    
  
   
Procedure ListCmds;
{-----------------------------------------------------------------------
{ Abstract:
{    Called for '?'.  Reads profile file and displays help.
{-----------------------------------------------------------------------}
handler PNotFound(FileName:string);
begin
    writeln('** Profile file not found.  Commands available are:');
    DumpCmds;
    exit(ListCmds);
    end;

Var 
    Entry, IgnoreStr: PString;
    i: integer;
    NumWritten: integer;
    ignorech: char;
    IgnoreB: Boolean;
    
    Begin
    NumWritten := 0;
    PFileInit(CurPFile, 'ShellCommands');
    writeln('The Commands available are:');
    Entry := PFileEntry;
    while Entry <> '' do begin
        for i := 1 to 4 do 
            ignorech := NextIdString(Entry, IgnoreStr, IgnoreB);
        if Entry<>'' then begin
            adjust(Entry, length(Entry)-1); {remove CCR}
            NumWritten := NumWritten + 1;
            writeln(Entry);
            end;
        Entry := PFileEntry;
        end;
    if NumWritten = 0 then raise PNotFound(CurPFile);
    writeln;
    writeln('To see programs you may run, type ''dir >*.run''.');
    end;
    

Procedure FixDefaultFile(AddDefault, SetDefault: Boolean);
{-------------------------------------------------------------------------
{ Abstract:
{    This procedure is used to adjust the command line.  It is called by
{    commands that wish to use the default file or set it.
{
{ Parameters:
{    AddDefault - if True, insert LastFileName at beginning of Args
{    SetDefault - if True, set the LastFileName to the first word
{       in Args.
{
{ Side Effects:
{    May modify Args and LastFileName.
{
{ Environment:
{    Assumes Cmd has the first keyword from UsrCmdLine and that Args
{    has the rest.
{------------------------------------------------------------------------}
var
   InFiles, OutFiles: pArgRec;
   Switches: pSwitchRec;
   TArgs: PString;
   ok: Boolean;
   IgnoreS: String;
   
   begin
   TArgs := Args;
   ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, IgnoreS);
   if AddDefault and ok then 
       if InFiles^.name = '' then 
           Args := concat(LastFileName, concat(' ', Args));
   if SetDefault and ok then
       if InFiles^.name <> '' then
           LastFileName := InFiles^.name;
   DstryArgRec(InFiles);
   DstryArgRec(OutFiles);
   DstrySwitchRec(Switches);
   end;


Procedure CreateTitleLine;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Creates the title line.
{
{ Side Effects:
{    Sets the global vble TitleStr.
{
{------------------------------------------------------------------------}
    var left, i: Integer;
    begin
    left := (SNumTitleChars div 2) - 5;
    TitleStr := FSDirPrefix;
    If length(TitleStr) > left then 
       begin
       TitleStr[left-1] := DotDot;
       TitleStr[left] := DotDot;
       TitleStr := Substr(TitleStr, 1, left);
       end
    else for i := length(titleStr)+1 to left do
           AppendChar(titleStr, ' ');

    TitleStr := Concat(TitleStr, '    POS ');
    TitleStr := Concat(TitleStr, MainVersion);
    TitleStr := Concat(TitleStr, '.');
    TitleStr := Concat(TitleStr, StrVersion);
    TitleStr := Concat(TitleStr, '  ');
    AppendChar(TitleStr, chr(SysBootChar));
    TitleStr := Concat(TitleStr,'-boot     ');
    end;


Procedure UpdateTime;
{----------------------
{Abstract:
{   Updates the display of time in the title line.
{   Called as the 'IdleProc' of GetCmdLine.
{----------------------}
var
   NewTimeStr: Timestring;
begin
     GetTString(NewTimeStr);
     If NewTimeStr <> TimStr then
           Begin
           TimStr := NewTimeStr;
           ChangeTitle(Concat(TitleStr,TimStr));
           End;
end; {UpdateTime}


{$Ifc SysTiming Then}
Procedure Time( Val: long);
{-------------------------------------------------------------------------
{
{ Abstract:
{    Prints a timing value.
{
{ Parameters:
{    Val  - Time in sixtieths of a second.
{
{------------------------------------------------------------------------}
    var Ones, Tenths: long;
    begin
    Val := Val + stretch(3);  { round }
    Val := Val div stretch(6);  { convert to tenths }
    Ones := Val div stretch(10);
    Tenths := Val - Ones * stretch(10); { mod }
    Write(Ones:5, '.', Tenths:1)
    end;

Procedure PrintTiming;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Prints the timing statistics for the previous program.
{
{------------------------------------------------------------------------}
    var Execute, Effective, Duty: long;
        Val, Ones, Tenths: long;
    Begin
    Writeln;
    Write('Load '); Time(OldLoadTime); Writeln(' secs.');
    Execute := OldExecuteTime;
    Execute := Execute - OldLoadTime;
    Write('Exec '); Time(Execute); Writeln(' secs.');
    Write('IO   '); Time(OldIOTime); Writeln(' secs.');
    Write('Swap '); Time(OldSwapTime); Writeln(' secs.');
    Write('Move '); Time(OldMoveTime); Writeln(' secs.');
    If Execute = stretch(0) Then Duty := stretch(1000)
    Else
        Begin
        Effective := Execute - OldSwapTime;
        Effective := Effective - OldMoveTime;
        Val := Effective * stretch(1000);  { scale }
        Val := Val + Execute div stretch(2);  { round }
        Duty := Val div Execute;
        End;
    Ones := Duty div stretch(10);
    Tenths := Duty - Ones * stretch(10); { mod }
    Writeln('Duty ', Ones:5, '.', Tenths:1, ' percent.');
    Writeln;
    End;
{$Endc}


Function CheckTwo(name: CString;var ans: boolean; s1, s2: string): boolean;
{-------------------------------------------------------------------------
  Abstract: Checks to see if name is one of the two specified strings.
  Parameters: name - the name to check.
              ans - true if s1, false if s2.
              s1 and s2 - the two strings to check against.
  Returns: true if ok else false.            
-------------------------------------------------------------------------}
  var SwTable: CmdArray;
      i: integer;
  begin
  ConvUpper(name);
  CheckTwo := true;
  SwTable[1] := s1;
  SwTable[2] := s2;
  case UniqueCmdIndex(name, swTable, 2) of
    1: ans := true;
    2: ans := false;
    otherwise: CheckTwo := false;
    end;
  end;


Function CheckHelp(switches: pSwitchRec): boolean;
{-------------------------------------------------------------------------
{ Abstract: Returns true if one switch is HELP.
{-------------------------------------------------------------------------}
  Const HelpStr = 'HELP';
  begin
  CheckHelp := false;
  while switches <> NIL do
    begin
    ConvUpper(switches^.switch);
    if length(switches^.switch) > 0 then
       if SubStr(helpStr, 1, length(switches^.switch)) = switches^.switch
          then CheckHelp := true;
    switches := switches^.next;
    end;
  end;


Procedure DoStatistics;
{-------------------------------------------------------------------------
 Abstract: Handles the Statistics command
-------------------------------------------------------------------------}
var
   InFiles, OutFiles: pArgRec;
   Switches: pSwitchRec;
   TArgs: PString;
   ok, on: Boolean;
   err: String;
   
   begin
  {$Ifc SysTiming Then}
   TArgs := Args;
   ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
   while (Infiles^.next = NIL) and (inFiles^.name='') and (outFiles^.next=NIL) 
         and (outFiles^.name = '') and (switches = NIL) and ok do
      begin
      Write('Enable statistics? ');
      Readln(TArgs);
      ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
      end;
   if ok then
      if (infiles^.next <> NIL) or (outFiles^.next <> NIL) or
         (outFiles^.name <> '') then ok := false;
   if ok then ok := CheckTwo(inFiles^.name, on,'YES','NO');
   if (not ok) or (switches <> NIL) then
      begin
      if not CheckHelp(switches) then err := '** '
      else err := '     ';
      WriteLn(err, 'Statistics takes "Yes" or "No" as the only parameter.');
      WriteLn(err, 'It turns statistics gathering on or off.');
      end
   else PrintStatistics := on;
   DstryArgRec(InFiles);
   DstryArgRec(OutFiles);
   DstrySwitchRec(Switches);
  {$ElseC}
   Writeln('** System statistics gathering not enabled.');
  {$Endc}
   end;


Procedure DoMntDismount(mnt: boolean);
{-------------------------------------------------------------------------
{ Abstract: Handles the mount and dismount commands
{ Parameters: if mnt is true then do a mount
{             else do a disMount
{-------------------------------------------------------------------------}
var
   InFiles, OutFiles: pArgRec;
   Switches: pSwitchRec;
   TArgs: PString;
   ok, flpy: Boolean;
   err: String;
   disk: integer;
   SetStatus : DevStatusBlock;
   nheads, Ncyls, NSecs, Bootsize, WriteCompCyl : Integer;
   DiskName : String;
   
   begin
   if mnt then Cmd := 'Mount'
   else Cmd := 'Dismount';
   
   If Mnt Then
     Begin
     SetStatus.ByteCnt := 3;
     SetStatus.FlpDensity := 0;
     SetStatus.FlpHeads := Ord(2 {sides} - 2) + 1;
     SetStatus.FlpEnable := True;
     IOPutStatus(Floppy, SetStatus);
     End;
     
   TArgs := Args;
   ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
   
   while (Infiles^.next = NIL) and (inFiles^.name='') and (outFiles^.next=NIL) 
         and (outFiles^.name = '') and (switches = NIL) and ok do
      begin
      Write('Device to ',cmd,': ');
      Readln(TArgs);
      ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
      end;
   
   DiskName := OutFiles^.Name;
   if ok then
      if (infiles^.next <> NIL) or (outFiles^.next <> NIL) then ok := False;
   if ok then
      if (GetIntDiskKind <> Mic5) And (DiskName <> '') Then ok := false;

   if ok then ok := CheckTwo(inFiles^.name, flpy, 'FLOPPY', 'HARDDISK');
   If ok And (GetIntDiskKind = Mic5) And Mnt And (not flpy) then
      SetupDiskParams(True,
                      DiskName,
                      Nheads,
                      NCyls,
                      NSecs,
                      BootSize,
                      WriteCompCyl)
    Else OK := DiskName = '';

   if (not ok) or (switches <> NIL) then
      begin
      if not CheckHelp(switches) then err := '** '
                 else err := '     ';
      WriteLn(err,Cmd, ' takes "Floppy" or "Harddisk" as the only parameter.');
      If Mnt And (not flpy) then
         Writeln(err, cmd, ' takes the DiskName as a second parameter for 5.25" disks');
      WriteLn(err, 'It ',cmd,'s the specified device.');
      end
   else if mnt then FSMount(ord(flpy))  {true = 1; false = 0}
      else FSDismount(ord(flpy));
   DstryArgRec(InFiles);
   DstryArgRec(OutFiles);
   DstrySwitchRec(Switches);
   end;

Procedure DoPath;
{-------------------------------------------------------------------------
 Abstract: Handles the path command
-------------------------------------------------------------------------}
var
   InFiles, OutFiles: pArgRec;
   Switches: pSwitchRec;
   TArgs: PString;
   ok: Boolean;
   err: String;
   i: integer;
   newPath, oldPath: PathName;
   fid: FileID;

procedure CheckArgs;
{-------------------------------------------------------------------------
{
{ Abstract:
{   Gets the arguments from the command line and checks them for correct
{   form (one input, no outputs)
{
{ Side Effects:
{   Sets InFiles, OutFiles, Switches, Err, Ok to result of parse.
{   Ok is true if arguments are correctly formed (0 or 1 'input' file,
{   no output files).
{
{-------------------------------------------------------------------------}
     begin
     ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
     if ok then
        if (infiles^.next <> NIL) or (outFiles^.next <> NIL)
         or (outFiles^.name <> '') then ok := false;
     end;

   begin { DoPath }

   TArgs := Args;
   CheckArgs;
   if ok then
      if (inFiles^.name = '') and (switches = NIL) then
         begin
         DstryArgRec(InFiles);
         DstryArgRec(OutFiles);
         DstrySwitchRec(Switches);
         Write('New default path: [', FSDirPrefix, '] ');
         ReadLn(TArgs);
         CheckArgs;
         end;

   if (not ok) or (switches <> NIL) then
      begin
      if not CheckHelp(switches) then err := '** '
      else err := '     ';
      WriteLn(err, 'Path takes a directory name as the only parameter.  The final ">" may be');
      WriteLn(err, '  omitted.  Path changes the current path.');
      end
   else if infiles^.name <> '' then
        begin
        newPath := inFiles^.name;
        FSRemoveDots(newPath);
        if newPath[length(newPath)] <> '>' then AppendChar(newPath, '>');
        fid := FSLookUp(newPath, i, i);
        if fid <> 0 then
             begin
             FSDirPrefix := newPath;
             WriteLn('New path = ',FSDirPrefix);
             CreateTitleLine;
             ChangeTitle(Concat(TitleStr,TimStr));
             end {fid <> 0}
        else StdError(ErDirNotFound, newPath, false);
        end; {else}
   DstryArgRec(InFiles);
   DstryArgRec(OutFiles);
   DstrySwitchRec(Switches);
   End;



Procedure DoScreenSize;
{-------------------------------------------------------------------------
 Abstract: Handles the ScreenSize command
-------------------------------------------------------------------------}
var
   InFiles, OutFiles: pArgRec;
   Switches: pSwitchRec;
   TArgs: PString;
   ok, isHelp, leave: Boolean;
   err: String;
   newPath: PathName;
   fid: FileID;
   N, C, Inx: integer;
   
   Function DoSwitch: boolean;
    {-------------------------------------------------------------------------
     Abstract: Handles all switches.  Uses the global switches. Sets isHelp if
               one of the switches is help.
     Returns: True if ok.  False if any error or isHelp.
    -------------------------------------------------------------------------}
       var tswitch: pSwitchRec;
           SwTable: CmdArray;
           i: integer;
       begin
       DoSwitch := true;
       isHelp := false;
       
       SwTable[1] := 'ON';
       SwTable[2] := 'OFF';
       SwTable[3] := 'COMPLEMENT';
       SwTable[4] := 'NOCOMPLEMENT';
       SwTable[5] := 'HELP';
       tswitch := switches;
       while tswitch <> NIL do
           begin
           CnvUpper(tswitch^.switch);
           i := UniqueCmdIndex(tswitch^.Switch, SwTable, 5);
           if (i < 5) and (tswitch^.arg <> '') then
                  begin
                  StdError(ErNoSwParam, tswitch^.switch, false);
                  DoSwitch := false;
                  end
           else begin
                if i <= 4 then SizeSpecified := true
                else if i >= 5 then DoSwitch := false;
                case i of
                   1: TmpSOff := False;
                   2: TmpSOff := True;
                   3: TmpSComplemented := True;
                   4: TmpSComplemented := False;
                   5: isHelp := true;
                   6: StdError(ErBadSwitch, tswitch^.switch,false);
                   7: StdError(ErSwNotUnique, tswitch^.switch, false);
                  end;
                end;
           tswitch := tswitch^.next;
           end;  {while}
       end;

   begin
   TArgs := Args;
   repeat
      leave := true;
      ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
      if ok then
         if (infiles^.next <> NIL) or (outFiles^.next <> NIL) or
            (outFiles^.name <> '') then ok := false;
      if (not ok) or (not DoSwitch) then
         begin
         if not isHelp then err := '** '
         else err := '     ';
         writeln(err,'  Command line is: ScreenSize nn /switch');
         writeln(err,'  where nn is 1..8 or a multiple of 128 up to 1024.');
         writeln(err,'  Valid switches are:');
         writeLn(err,'    ON  OFF:                  The bottom portion will (not) be visible.');
         writeLn(err,'    COMPLEMENT  NOCOMPLEMENT: Specifies the color of the bottom part.');
         writeLn(err,'    HELP :                    Displays this message.');
         ok := false;
         end
      else begin
           ok := true;
           if inFiles^.name = '' then
              begin
              Write('Screen Size for next program: [8] ');
              Readln(TArgs);
              if TArgs = '' then TArgs := '8'
              else leave := false;
              end
           else TArgs := inFiles^.name;
           end;
    DstryArgRec(InFiles);
    DstryArgRec(OutFiles);
    DstrySwitchRec(Switches);
    until leave;
  if ok then
     begin
     {convert first argument to an integer and check for 1..8, 128, ...}
     N := 0;
     Inx := 1;
     while Inx <= length(TArgs) do
        begin
        C := ord(TArgs[Inx]) - ord('0');
        if (C<0) or (C>9) then
                 begin
                 writeln('** ''', TArgs[Inx], ''' is not a digit.');
                 exit (DoScreenSize);
                 end;
        N := 10*N + C;
        Inx := Inx+1;
        end;
     if (1<=N) and (N<=8) then TmpSSize := 128*N
     else if (N<128) or (N> 1024) then
              begin
              writeLn('** Bad number to ScreenSize.');
              exit(DoScreenSize);
              end
          else {round to closest multiple of 128}
              TmpSSize := 128*((N div 64 + 1) div 2);
     if not SizeSpecified then
         begin
         SizeSpecified := True;
         TmpSComplemented := DefScrComp;
         TmpSOff := DefScrOff;
         end;          
     write('Next program will run with ', TmpSSize:1, 
                  ' screen lines. The bottom will be ');
     if not TmpSOff then write ('on') else write('off');
     if TmpSComplemented then writeln (' and complemented.')
     else writeln ('.');
     end; {ok}
  end;
    

{--------------------
{ Declarations for main program
{-------------------}
 
label 1;

var
    NeedReadNextCmd: Boolean;


Handler CtlCAbort;
  begin
  CtrlCPending := False;
  CtrlSPending := False;
  StreamKeyboardReset(Ctrl^.CmdFileList^.CmdFile);
  WriteLn('^C');
  NeedReadNextCmd := True;
  goto 1;
  end;


var
    HasTitle: Boolean;
    WhichCmd: integer;
    UpCmd: string;
    TCmd: string;
    timeOuts : integer;
    
    Timebuf: RECORD CASE boolean of
           true: (t: ^TimeStamp);
           false: (p: pDirBlk);
          END;
    i: integer;

    isSw: Boolean;
    Err: String;
    BlankCh: string;
    IgnoreInt: Integer;
    IgnoreWind: WinRange;
    IgnoreCh: char;
    IgnoreS: String;


Procedure WriteTime;
label 1;
  Handler NotAFile(id: SegID);
     begin
     TimeFID := FSEnter(TimeFileName);
     FSClose(TimeFID, 1, TimeFBitSize);
     goto 1;
     end;
   begin
1: GetTStamp(Timebuf.t^);
   FSBlkWrite(TimeFID, 0, Timebuf.p);
   end;
        

{Shell MAIN PROGRAM begins here}


Begin
    BlankCh := ' ';
    
    If ShellCtrl = NIL then InitCmds;
    Ctrl := RECAST(ShellCtrl, pCtrlRec);
    Ctrl^.FirstPress := True;            {because popup uses NEW}
    StreamSegment := CmdSegment;

    TmpSSize := SBitHeight;
    TmpSComplemented := DefScrComp;
    TmpSOff := DefScrOff;
    SizeSpecified := False;
    
    CreateTitleLine;
    if not isFloppy then NEW(Timebuf.p);
             
    ChangeWindow(0);
    GetWindowParms(IgnoreWind,IgnoreInt,IgnoreInt,IgnoreInt,IgnoreInt,HasTitle);
    GetTString(TimStr);
    If HasTitle Then
        Begin
        RefreshWindow(0);
        ChangeTitle(Concat(TitleStr,TimStr));
        writeln;
        End
    Else
        CreateWindow(0,0,0,SBitWidth,SBitHeight,Concat(TitleStr,TimStr));
    
    {$Ifc SysTiming Then}
    If PrintStatistics Then PrintTiming;
    {$Endc}
    
    if not InCmdFile and (Ctrl^.CmdFileList^.next <> NIL) then 
        ExitAllCmdFiles(Ctrl^.CmdFileList)       {Scrounge said forget it}
    else StreamKeyBoardReset(Ctrl^.CmdFileList^.CmdFile);

    if UseCmd then  {previous program run has already set UsrCmdLine}
        begin
        WriteLn(CmdFileChar, UsrCmdLine);
        UseCmd := False;
        i := PosC(UsrCmdLine, '!');
        if i <> 0 then UsrCmdLine := SubStr(UsrCmdLine, 1, i-1);
        RemDelimiters(UsrCmdLine, ' ', IgnoreS);
        if UsrCmdLine[1] = '@' then
           begin
           if not DoCmdFile(UsrCmdLine, Ctrl^.CmdFileList, err) then
             WriteLn(err);
           NeedReadNextCmd := true; {will read from cmd file if OK}
           end
        else begin
             NeedReadNextCmd := False;
             GetSymbol(UsrCmdLine, Cmd, '/ ~', IgnoreS);
             RemDelimiters(UsrCmdLine, ' ', IgnoreS);
             Args := UsrCmdLine;
             end;
        end
    else
        NeedReadNextCmd := True;

1:  NoRunFile:=True;
        
    While NoRunFile do Begin
    
        UpdateTime;
        
        {write current time to TimeFile}
        if not isFloppy then WriteTime;

        { Get the command line. This leaves Args starting with delimiter.}
        if NeedReadNextCmd then
            begin
            timeOuts := ErrorCnt[IOETim];
            WhichCmd := GetCmdLine(UpdateTime, 
                       '', Args, Cmd, 
                       Ctrl^.CmdFileList, 
                       Ctrl^.CmdDesc, 
                       Ctrl^.FirstPress,
                       true);
            if ErrorCnt[IOETim] > timeOuts then
               WriteLn('* WARNING - Tablet seems to be unplugged');
            if Args<>'' then 
                adjust(Args, length(Args)-1);    {get rid of CR}
            if debug then
                writeln('Cmd:',Cmd,'    Args:',Args,'!   index:',WhichCmd:1);
            end 
        else begin
            NeedReadNextCmd := True;
            WhichCmd := PopUniqueCmdIndex(Cmd, Ctrl^.CmdDesc);
            end;
        
        if WhichCmd > Ctrl^.NumCmds then 
            case WhichCmd - Ctrl^.NumCmds of
            1: {unknown command;  assume it's a .RUN file name}
                NoRunFile := False;
            2: {non-unique} begin
                StdError(ErCmdNotUnique,Cmd, false);
                WriteLn('** Type ? for the current list of known commands');
                end;
            3: {name was empty;  do nothing}   ;
            4: {switch;  check for HELP} begin
                UpCmd := Cmd;
                ConvUpper(UpCmd);
                if 1=POS(UpCmd, 'HELP') then Help
                else StdError(ErBadSwitch, cmd, false);
                end;
            5: {illegal char after command}
                StdError(ErIllCharAfter,cmd, false);
            end
                
{$R-}
        else with Ctrl^.CmdTable^[WhichCmd] do
           begin
{$R=}
           Case {with}Index Of
           IdxHelp:        Help;
           IdxQuest:       ListCmds;
           IdxPath:        DoPath;
           IdxMount:       DoMntDismount(true);
           IdxDismount:    DoMntDismount(false);
           IdxStatistics:  DoStatistics;
           IdxSwap:        DoSwap(args);
           IdxScreenSize:  DoScreenSize;
           IdxPause:       Begin
                               writeln;
                               WriteLn(Args);
                               Write('Type <return> to continue.');
                               ReadLn;
                               End;
           IdxRun:  begin
                        FixDefaultFile({with}AddDefault, {with}SetDefault);
                        IgnoreCh := NextString(Args, Cmd, isSw);
                        if length(Args)>0 then 
                            adjust(Args, length(Args)-1);    {Remove CCR}
                        if Cmd = '' then
                            writeln('** No file to run specified.')
                        else if isSw then
                            writeln('** Need a file name, not a switch: /',Cmd)
                        else 
                            NoRunFile := False;
                        end;
           IdxReRun: begin
                        if LastFileName = '' then 
                            writeln('** No previous file specified to run.')
                        else begin
                            Cmd := LastFileName;
                            NoRunFile := False;
                            end;
                        end;
           IdxOther: begin {other commands in table}
                        {Do default processing on the Args.  
                        {Then construct a command line out of:
                        {   RunString,
                        {   Args.
                        {If the beginning of RunString is 'RUN' then 
                        {the run command is simulated.  Otherwise, the
                        {cmd and args are run back through the command parser.}
                        FixDefaultFile({with}AddDefault, {with}SetDefault);
                        TCmd := {with}RunString;
                        IgnoreCh := NextIDString(TCmd, Cmd, IsSw);
                        if TCmd<>'' then
                            TCmd[length(TCmd)] := ' ';  {replace CR with space}
                        insert (TCmd, Args, 1);
                        convupper(Cmd);
                        if Cmd='RUN' then  {skip default file process}
                            begin
                            IgnoreCh := NextString(Args, Cmd, isSw);
                            if length(Args)>0 then 
                                adjust(Args, length(Args)-1);    {Remove CCR}
                            if Cmd = '' then
                                writeln('** No file to run specified.')
                            else if isSw then
                                writeln('** Need a file name, not a switch: /',Cmd)
                            else begin
                                 if Cmd[length(cmd)] = '*' then
                                   begin
                                   Adjust(cmd, length(cmd)-1);  {remove *}
                                   Cmd := Concat(Cmd,StrVersion);
                                   end;
                                 NoRunFile := False;
                                 end;
                            end
                        else if Cmd[1] <> '@' then 
                            NeedReadNextCmd := False
                        else if not DoCmdFile(concat(Cmd, Args),
                                        Ctrl^.CmdFileList, Err) then
                            writeln(Err);
                        end;
               end; {case CmdIndex}
           if ({with}ScreenSize<>8) and not SizeSpecified then begin
               TmpSSize := 128*{with}ScreenSize;
               TmpSComplemented := DefScrComp;
               TmpSOff := DefScrOff;
               end;
           end; {else with}
        End;  {while NoRunFile}
  
  
{
{If we get here, then Cmd contains a Run file name and Args contains arguments.
{Update title line then set system globals so system will load file.
{ }
     
    UsrCmdLine := concat(Cmd, concat(' ', Args));

    if not isFloppy then WriteTime;
     
    If SubStr(Cmd, Length(Cmd)-3, 4) <> '.RUN' then
        RFileName:=Concat(Cmd, '.RUN') 
    Else
        RFileName:=Cmd;
    
    UseCmd := False;

    right := (SNumTitleChars div 2) - 5 + 22;

    Adjust(TitleStr, SNumTitleChars);      {Make it full length}
    {copy tail of run file name into right side of title}
    for I := SNumTitleChars downto right do
        if length(RFileName) > SNumTitleChars-I then
            if I > right+1 then
               TitleStr[I] := RFileName[length(RFileName)-(SNumTitleChars-I)]
            else TitleStr[I] := DotDot {chars right and right+1}
        else TitleStr[I] := ' ';
    ChangeTitle(TitleStr);

    NextSSize         := TmpSSize;
    NextSComplemented := TmpSComplemented;
    NextSOff          := TmpSOff;
    
    InCmdFile := Ctrl^.CmdFileList^.next <> NIL;

End{shell}.
