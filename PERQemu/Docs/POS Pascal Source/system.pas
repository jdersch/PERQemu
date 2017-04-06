{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program System;
{------------------------------------------------------------
{     System - POS main program.
{     Perq Software Group.
{     Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1983.
{
{ Abstract:
{     Initialize POS
{     and go into loop alternately running Shell and user program
{------------------------------------------------------------}

{$Version 2.20 for POS}
{------------------------------------------------------------
{ Change Log:
{
{      20 May 83  V2.20 S Johar
{                              Put the SIDFail handler into the hardcopy
{                              handler.
{
{      26 Apr 83  V2.19 E Beattie and S Johar
{                              Change ScrnDmp exception to HardCopy.
{                              Call SID instead of screen_dump
{                              Change system letter back to G.
{
{      14 apr 83  V2.18 Chris Hughes
{                              Treat cio micropolis as eio in PrintFailMessage
{
{      12 Apr 83  V2.17 Dave Anderson, ICL
{                              Added Screen_Dump Import and Handler.
{                              Put conditionals round the Floating-Point
{                              microcode load stuff.
{                              Changed System letter to R for ICL.
{ 
{      28 Feb 83  V2.16 Sandeep Johar
{                              Catch the UnImplQcode and the WCSSizeError
{                              exceptions in system.
{
{      28 feb 83  V2.15 Sandeep Johar
{                              Retro-fit Roger Riggs mods to load Z80 only when
{                              when running on an EIO system. Improved error
{                              reporting during the Z80 load and add some 
{                              SetDDS calls.
{
{      18 Feb 83  V2.14 David Golub
{                              Call Configuration module
{                              Load Floating-Point Microcode overlay
{                              Change System letter to G.
{
{      27 Jan 83  V2.13 RSR    Updated comment on decl of Default pointer.
{                              Fixed Zboot file name.
{
{      20 Jan 83  V2.12 RSR    Added boot letter to Z80 Boot filename
{
{      12 Jan 83  V2.11 AGR    Moved definition of CurRFilName to the end
{                              of the export list to make system compatible
{                              with V2.7.  Taught PrintFailMsg about EIO disks.
{       5 Jan 83  V2.10 DAS    Made changes that allow use of Ethernet
{                              if the system was booted from floppy.
{       4 Jan 83  V2.9  CDB    to retrofit:
{       3 Nov 82  V2.7  BAM    Added new global set to the current run file for
{                              the debugger.
{
{      18 Dec 82  V2.8 AGR     Added code to load the Z80 memory.
{
{      06 Oct 82  V2.7 AGR     Added call to IOClearExceptions to prevent
{                              unwanted raising of exceptions by device
{                              interrupt routines.
{
{      16 Aug 82  V2.6 DLK     Added new variables to permit selection of
{                              either Kriz Tablet or BitPad.  Permit true
{                              relative mode for either.
{
{       4 Mar 82  V2.5 LFK     Change system letter to F.
{
{       8 Feb 82  V2.4 EMC     Shrink screen even more for floppy.
{
{       8 Feb 82  V2.3 EMC     Turn off ethernet if booting from floppy.
{
{      21 Jan 82  V2.2 BAM     Add new variables isFloppy, PopAllowed.
{       6 Jan 82  V2.1 WJH     Use smaller screen for floppy booted on 1/4 MB
{
{       4 Dec 81  V2.0 WJH     Change to ShellCtrl from three vars
{                              add UseCmd, UserPtr, and UserInt
{                              add TimeFID
{                              remove CmdFile
{                              rearrange definitions of variables
{                              change xxTime variables to Long
{                              move StreamKeyBoardReset to Shell
{       1-Dec-81  BAM          Added the new exception HELPKey and a default
{                                handler.
{                              Added default handler for Flush Fail that works
{                                 like diskFailure.
{                              Make ^C^C clear ^S.
{      05-NOV-81  DAS          Added calls to EtherInit.
{      13-Aug-81  BAM          Add new vbles to hold pointers for the shell.
{      30-Jun-81  BAM          Add new vbles for DefScrOff and Comp and demo.
{      25-Jun-81  BAM          Change to window 0 before error type out.
{      25-Jun-81  JPS          Turn tablet off in command loop.
{      23-Jun-81  BAM          Address for DiskFailure printed in unsigned
{                               decimal
{       1-Jun-81  BAM          Do automatic re-enable of swapping if turned off
{                               by Shell (i.e. for Scavenger)
{      28-May-81  BAM          Handler for Stream Reset error
{      23-May-81  JPS          Replace enable/disable of control-c processing
{                              (exported by System) with enable/disable of
{                              KeyBoard interrupts (exported by IO_Others).
{      22-May-81  JPS          Fix control-c processing.
{      18-May-81  JPS          Use StreamKeyBoardReset.
{                              Change TwoCtlC exception to CtlCAbort.
{      13-May-81  BAM          Remove old comments about stand alone
{                              Add vble for default cursor function (screen
{                                 color) that can be set by login from profile
{                              Add comments to some exceptions
{      13-May-81  JPS          1. Add exceptions and default handlers for
{                                 control-C.
{                              2. Add procedures to enable and disable
{                                 control-C processing.
{                              3. Add exception to ExitProgram.
{                              4. Remove vestiges of old system: standard error
{                                 procedures.
{      12-May-81  BAM          Add default handlers for many exceptions
{                              Use new IO
{       6-Apr-81  JPS          Retrofit changes for exceptions to version of
{                               system that diverged after 15-Mar-81.
{                              Virtual memory and timing statistics.
{                              Get rid of "System" conditional compile.
{                              Change main version from C to D.
{      15-Mar-81  JPS          Put in stuff for exceptions.
{      28-Mar-81  BAM          Changed length of UsrCmdLine to 255.
{      19-Mar-81  BAM          PERQ_String.
{      17-Mar-81  BAM          Set time when come up so Login LookUp faster.
{       3-Mar-81  DAS          Added variables for Login.  Added
{                              InCmdFile for Shell.
{      25-Feb-81  DAS          Added FlushAll call in command.
{      24-Feb-81  JPS          Add code to shrink and expand the screen.
{                              *** Caution *** The Shell must never be loaded
{                              when the screen is small, because it might
{                              allocate a buffer for a command file.  This
{                              buffer will hang around after the Shell returns
{                              to the system.
{      23-Feb-81  RFR          Added SysDisk, SysBootChar definitions.
{                              Added call to FSSetupSystem.
{      19-Feb-81  John Strait  Incorporate system version number into the
{                              name of the LogIn run file.
{      17-Feb-81   Don Scelza  Removed the include file for SysVers.
{                              Put the code inline in System.
{      16-Feb-81  John Strait  Removed the Loader to a separate module.
{      16-Feb-81  Don Scelza   Changed System to use Perq.String and
{                              changed MainVersion to C.
{      11-Feb-81  Diana Forgy  Took out command interpreter and made
{                              most of it program Shell.
{
{                              This program is compatable with the
{                              new Compiler and PString.
{
{
{      17-Nov-80  Don Scelza   Added the code for LastFileName.
{                              This will allow a user to supply the
{                              name of a file once for the Editor, Compiler,
{                              and Linker.
{      14-Nov-80  Don Scelza   Added conditional compile for stand
{                              alone programs
{      10 Oct 80  JPS          Add support for the diagnostic display (DDS).
{      23 Sep 80  JPS and DAS  Add PAUSE command.
{                              Announce 12 or 24 MByte disk.
{}



{********************} Exports {********************}

Const MainVersion = 'G';
      DebugSystemInit = False;
      FirstDDS = 199;
      ShellConst = 'Shell.';
      LogConst = 'LogIn.';
      PFileConst = 'Default.Profile';

      SysTiming = True;    { Gather System timing statistics.  If this constant
                             is changed, IO, Loader, Memory, Movemem, System,
                             and Shell should be re-compiled, and the System
                             should be re-linked. }

      ScreenDump = True;  { Support ICL written Screen Dump software}
      GetDblFloat = False; { True if Double precision floating point ucode 
                            is to be loaded from separate file}

Type Sys9s = String[10];


Var UsrCmdLine: String[255];    {Command line entered by user}
    UseCmd: Boolean;            {Set True to tell shell to execute UsrCmdLine}
    InCmdFile: Boolean;         {True if shell commands from file}
    LastFileName,               {Name of file to use if none given}
    RFileName,                  {Name of next program to run}
    ShellName: String;          {Name of Shell}

    CurUserID,                  {Index of user in System.Users}
    CurGroupID: 0..255;         {Groupid of current user}
    CurUserName,                {LogIn name of current user}
    CurPFile: String;           {Name of current profile file}
    UserMode: Boolean;          {True while executing user program}

    CtrlCPending: Boolean;      {True if one control-C typed}

    NextSSize: Integer;         {Screen size for next program}
    NextSComplemented: Boolean; {Whether to complement bottom for next pgm}
    NextSOff: Boolean;          {Whether bottom should display data bits}
    DefCursFunct: Integer;      {What to set curs func to after each prog}
    DefScrComp: Boolean;        {Default value for NextSComplemented}
    DefScrOff: Boolean;         {Default value for NextSOff}

    ShellCtrl: pointer;         {Pointer to information record for Shell}
    TimeFID: integer;           {File ID of file holding current time}
    CmdSegment: Integer;        {SegmentNumber of seg holding command files}

    InPmd: Boolean;             {True if in Scrounge (PostMortemDump)}
    SysDisk: Integer;           {Number of the disk booted from}
    SysBootChar: Integer;       {Ord(char held down to boot)}

    StrVersion: string;         {System version number as a string}
    SystemVersion: Integer;     {Integer giving system version number}
    SystemInitialized: Boolean; {True after system initialized}
    DDS: Integer;               {Keeps current diagnostic display value}
    ShouldReEnableSwapping: Boolean;  {True if swapping must be reenabled}
    SavedSwapId: Integer;       {Save id of where to swap to}

    {$ifc SysTiming then}
    LoadTime, OldLoadTime: long;
    ExecuteTime, OldExecuteTime: long;
    SwapTime, OldSwapTime: long;
    MoveTime, OldMoveTime: long;
    IOTime, OldIOTime: long;
    PrintStatistics: Boolean;
    {$endc}

    UserPtr: pointer;           {A pointer variable for use between user
                                programs.  (Use IncRefCount to keep segment)}
    UserInt: integer;           {May be a segment number for UserPtr}

    DemoInt: Integer;           {reserved for Demo system}

    isFloppy: Boolean;          {true if booted from floppy, else false}
    pointAllowed: Boolean;      {true if should use pointing device}

    DefRealRelTablet:boolean;{true if KrizTablet/BitPad in true relative mode }
    DefTabletType: integer;     {assigned ord(KrizTablet) by Login etc }

    CurRFileName: String;       {the current run file; used by the symbollic
                                 debugger}

{*** WARNING!!  IF YOU CHANGE THE EXPORTED PROCEDURES AND EXCEPTIONS, MAKE
{***            SURE THE NUMBERS FOR THE FOLLOWING EXCEPTIONS ARE UPDATED
{***            AND RECOMPILE SCROUNGE IF CHANGED !!!!! *****}

{*** WARNING!!  DO NOT CHANGE THE ORDER OF THE ^C EXCEPTIONS !!!!! *****}

Procedure Command;
Procedure SetDDS( Display: Integer );
Procedure SysVers( n: integer; var S: string );

Const ErrCtlC = 4; {******}
Exception CtlC;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CtlC is raised by the KeyBoard interrupt routine when a control-c
{       is typed.  If you handle this exception you should clear
{       CtrlCPending in your handler. If you are catching control-c's to
{       try to prevent aborts, you should enable CtlCAbort also, since the
{       Stream package will raise it when the control-c is read.
{
{-----------------------------------------------------------------------------}

Const ErrCtlCAbort = 5; {******}
Exception CtlCAbort;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CtlCAbort is raised by the KeyBoard interrupt routine when the second
{       of two adjacent control-c's is typed.  It is also raised by the
{       Stream package when a control-c is read.  If you handle this exception
{       you should clear CtrlCPending in your handler.
{
{       When this is raised by the KeyBoard interrupt routine, the KeyBoard
{       type-ahead buffer is cleared.  If you want to prevent this, you must
{       catch CtlC also.
{
{       If your program uses a Text file and you want to clear the line editing
{       buffer for that file, you should call the Stream routine
{       StreamKeyBoardReset(F) (assuming F is the name of the file).  If F
{       is a Text file which is attached to the console, this will get rid
{       of the character F^ points to and clear Stream's line editing buffer.
{
{-----------------------------------------------------------------------------}

Const ErrCtlShftC = 6; {******}
Exception CtlShftC;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CtlShftC is raised by the KeyBoard interrupt routine when a control-
{       shift-c is typed.  If you handle this exception you should clear
{       CtrlCPending in your handler.
{
{       When this is raised by the KeyBoard interrupt routine, the KeyBoard
{       type-ahead buffer is cleared.  You cannot prevent this.
{
{       If your program uses a Text file and you want to clear the line editing
{       buffer for that file, you should call the Stream routine
{       StreamKeyBoardReset(F) (assuming F is the name of the file).  If F
{       is a Text file which is attached to the console, this will get rid
{       of the character F^ points to and clear Stream's line editing buffer.
{
{-----------------------------------------------------------------------------}

Const ErrExitProgram = 7; {******}
Exception ExitProgram;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ExitProgram is raised to abort (or exit) a program.  The default
{       handler for CtlCAbort and Scrounge raise this exception.
{
{       WARNING: No one but System and Loader should Handle this exception.
{                Anyone may raise it to exit a program.
{
{-----------------------------------------------------------------------------}

Const ErrHelpKey = 8; {******}
Exception HelpKey(var retStr: Sys9s);
{-----------------------------------------------------------------------------
{
{ Abstract:
{       HelpKey is raised when the HELP key is hit.
{
{ Parameters:
{       retStr - the set of characters to put into the input stream.  This
{                should be set by the handler if it continues    from the
{                exception.   Likely values are "/Help<CR>" and chr(7)
{                (the current value returned).  The key board interupt routine
{                sets retStr to '' before raising this exception so if not set,
{                and the handler resumes, nothing will be put into the input
{                stream.
{ Resume:
{         Allowed.  Should set retStr first.
{-----------------------------------------------------------------------------}

{$ifc ScreenDump then}
const ErrHardCopy = 9; {******}
Exception HardCopy;
{-----------------------------------------------------------------------------
{
{ Abstract:
{         HardCopy is raised when Control-Shift-P is hit.
{         Default handler calls Screen_Dump procedure in module Screen_Dump.
{ Resume:
{         Allowed.
{-----------------------------------------------------------------------------}
{$endc}

type DoubleWord = ^integer;    {should use Long instead}

{********************} Private {********************}


{$A- Don't Reset(Input) or Rewrite(Output). }

Imports SystemDefs    from SystemDefs;
Imports Except        From Except;
Imports IO_Init       From IO_Init;
Imports IO_Others     From IO_Others;
Imports IO_Unit       From IO_Unit;
Imports IO_Private    From IO_Private;
Imports Memory        From Memory;
Imports Stream        From Stream;
Imports FileSystem    From FileSystem;
Imports Screen        From Screen;
Imports Raster        From Raster;
Imports Perq_String   from Perq_String;
Imports Reader        from Reader;
Imports GetTimeStamp  from GetTimeStamp;
Imports Loader        from Loader;
Imports ReadDisk      from ReadDisk;
Imports AllocDisk     from AllocDisk;
Imports IOErrMessages from IOErrMessages;
Imports LoadZ80       from LoadZ80;
Imports DiskDef       from DiskDef;
{$ifc Ether10MBaud then} Imports Ether10IO from Ether10IO;       {$endc}
Imports Configuration from Configuration;
{$ifc getDblFloat then} Imports LoadDblFloat From LoadDblFloat; {$Endc}
Imports ControlStore  from ControlStore;
Imports Sid           from Sid;



label 99;    { end of System command loop }

var CurTime: double; {used to initialize time in GetTimeStamp}
    Ignore: Integer;
    StartTime, EndTime: long;
    StartAddress : integer;
    LogAdr : double; { used for call to UnitIO }
    StsPtr : IOStatPtr;
    Z80BootName : String;



Handler CtlC;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CtlC is the standard handler for the CtlC exception.
{
{-----------------------------------------------------------------------------}
begin { CtlC }
  CtrlCPending := True
end { CtlC };

Handler CtlCAbort;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CtlCAbort is the standard handler for the CtlCAbort exception.
{
{-----------------------------------------------------------------------------}

begin { CtlCAbort }
  CtrlCPending := False;
  CtrlSPending := False;
  Writeln('^C');
  raise ExitProgram
end { CtlCAbort };

Handler CtlShftC;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CtlC is the standard handler for the CtlShftC exception.
{
{-----------------------------------------------------------------------------}

begin { CtlShftC }
  CtrlCPending := False;
  raise CtlShftC
end { CtlShftC };

Handler ExitProgram;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ExitProgram is one of the two standard handlers for the ExitProgram
{       exception.
{
{-----------------------------------------------------------------------------}

var IgnoreB: Boolean;
begin { ExitProgram }
  IOKeyDisable(IgnoreB);   { So no ^C aborts; Loader will re-enable }
  Goto 99
end { ExitProgram };


Handler DevInterrupt( Unit: UnitRng; IntType: IOIntrTypes; ATNCause: integer );

{-----------------------------------------------------------------------}
{
{ Abstract
{       DevInterrupt is raised by device interrupt routines when some
{       program has requested the raising of such interrupts.  This is
{       not desired as a normal rule, thus system clears all interrupt
{       exception raising when a program terminates.  However, an exception
{       could be raised before system has a chance to do the clear.  We
{       try to catch that exception here.
{
{--------------------------------------------------------------------------}

begin

  inlinebyte( {INTOFF} 105 );   { prevent furthur interrupts }

  IOClearExceptions;            { prevent furthur exceptions }

  inlinebyte( {INTON} 106 );    { allow interrupts once again }

end;

Procedure SetDDS( Display: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SetDDS sets the diagnostic display to a particular value.
{
{ Parameters:
{       Display - Desired value of the diagnostic display.
{
{-----------------------------------------------------------------------------}
  Var I: Integer;
    Begin
    if Display < DDS then DDS := DDS - 1000;
    for I := DDS+1 to Display do InLineByte( #373 {IncDDS} );
    DDS := Display
    End { SetDDS };



procedure SysVers( n: integer; var S: string );
{------------------------------------------------------------------------
{
{ Abstract:
{    This procedure will provide the caller with a string that is the
{    version number of the current system.
{
{ Parameters:
{    n is the minor version number of the system.
{
{    S will be set to the current minor version of the system.
{
{-----------------------------------------------------------------------}
  var C: string;
    begin { SysVers }
    S := ''; C[0] := Chr(1);
    repeat
        C[1] := Chr(Ord(n mod 10) + Ord('0'));
        S:= Concat(C, S);
        n := n div 10
    until n = 0
    end { SysVers };


Procedure Command;
{ ------------------------------------------------------------
{
{ Abstract:
{   This procedure alternately loads Shell and the user programs
{   whose runfile names are generated by Shell. It is invoked by
{   the main program in System and can be exited only if the
{   user types ^C or if a runtime error occurs.
{
{ ------------------------------------------------------------ }
var T: String;

begin
      T:=ShellName;      {initially, this is set to LogIn}
      RFileName:=T;
      repeat
            InPmd := False;

            {set up screen}
            SCurOff;
            ChangeWindow(0);
            SCurChr('_');
            SChrFunc(RRpl);
            IOCursorMode(OffCursor);
            IOLoadCursor(DefaultCursor,0,0);
            IOSetFunction(RECAST(DefCursFunct, CursFunction));
            NextSSize := 1024;
            NextSComplemented := False;
            NextSOff := False;

            if shouldReEnableSwapping then
               begin
               EnableSwapping(SavedSwapId);
               shouldReEnableSwapping := false;
               end;

            {$ifc SysTiming then}
            OldLoadTime := LoadTime;
            OldExecuteTime := ExecuteTime;
            OldSwapTime := SwapTime;
            OldMoveTime := MoveTime;
            OldIOTime := IOTime;
            LoadTime := stretch(0);
            ExecuteTime := stretch(0);
            SwapTime := stretch(0);
            MoveTime := stretch(0);
            IOTime := stretch(0);
            {$endc}

            MarkMemory;
            UserMode:=true;
               {$ifc SysTiming then}
               IOGetTime(recast(StartTime, double));
               {$endc}

            CurRFileName := T; {global to tell the debugger what is being run}
            Load(T);         {run the program}

               {$ifc SysTiming then}
               IOGetTime(recast(EndTime, double));
               {$endc}
            UserMode:=false;
            T:=RFileName;
            RFileName:=ShellName;

            IOClearExceptions; { don't let device interrupts raise exceptions }
            CleanUpMemory;
            FlushAll;
            IOSetModeTablet(offTablet);
            IOSetRealRelTablet(DefRealRelTablet);
            IOChooseTablet(recast(DefTabletType, TabletType));
            If SysBootChar <= Ord('Z') Then { booted from floppy }
               if (NextSSize=1024) and (MemoryInBlocks=#1000) then
                   NextSSize := 768 {=1024-256};   { use smaller screen }
            SSetSize(NextSSize, NextSComplemented, NextSOff);
            {$ifc SysTiming then}
            ExecuteTime := EndTime - StartTime;
            {$endc}

      until false;
end;






{--------------------------------------------------------
{
{  This is the main program. After it initializes the
{  Diagnostic Display, Memory, IO, Filesystem, and other
{  system features, it asks the user to log in and then
{  enters a loop that repeatedly calls procedure Command.
{
{------------------------------------------------------- }


  Handler BadPart(msg, partName: string);
  {-------------------------------------------------------
   Abstract: Handles a bad partition so can tell that should run scavenger and
              which partition is bad
  -------------------------------------------------------}
    begin
    ChangeWindow(0);
    WriteLn;
    WriteLn('** ',msg,' for ',partName,'.  Run Scavenger.');
    Raise BadPart(msg, partName);
    end;

  Handler PartFull(partName: string);
  {-------------------------------------------------------
   Abstract: Handles a partition full (no free blocks) so can tell that should
             delete some files and then run scavenger
  -------------------------------------------------------}
    begin
    ChangeWindow(0);
    WriteLn;
    WriteLn('** No free blocks in ',partName,'.  Delete some files then run Scavenger.');
    Raise PartFull(partName);
    end;

  Handler FSNotFnd(name: PathName);
  {-------------------------------------------------------
   Abstract: Handles File not found by continuing so LookUp can return zero and
              remain compatible
  -------------------------------------------------------}
      begin end;
  Handler FSBadName(name: PathName);
  {-------------------------------------------------------
   Abstract: Handles Can't create file (due to illegal name) by continuing so
             LookUp can return zero and remain compatible
  -------------------------------------------------------}
      begin end;

  Handler HelpKey(var retStr: Sys9s);
  {----------------------------------------------------------------------------
    Abstract: Sets the string to be "/HELP<CR>" and continues.
  ----------------------------------------------------------------------------}
     Const HelpStr = '/HELP';
     begin
     retStr := HelpStr;
     end;

{$ifc ScreenDump then}
   Handler HardCopy;
  {----------------------------------------------------------------------------
    Abstract: Dumps the PERQ screen to a plotter and continues.
  ----------------------------------------------------------------------------}
  
    Handler SidFail(Why : SidWhy);
    Begin
      If (Why = SidNone) Or (Why = SidBroken) Then IOBeep;
    End;

    Handler HardCopy;
      Begin
      End;

     begin
     Sid(''); 
     end;
{$endc}
 
  Handler ResetError(name: PathName);
  {-------------------------------------------------------
   Abstract: Handles File not found for stream so can print file which was
             not found
  -------------------------------------------------------}
      begin
      ChangeWindow(0);
      WriteLn;
      WriteLn('** Unable to reset ',name);
      Raise ResetError(name);
      end;

  Handler DiskError(msg: string);
  {-------------------------------------------------------
   Abstract: Disk error handler; prints message
  -------------------------------------------------------}
    begin
    ChangeWindow(0);
    WriteLn;
    WriteLn('** Disk Error: ',msg);
    Raise DiskError(msg);
    end;

{$ifc GetDblFloat then}
  Handler UnImplQCode;
  {--------------------------------------------------------------------------

       Abstract:
          This exception is raised when a Qcode used for the Floating
          point (double precision) is called but the appropriate overlay
          files have not been loaded.  The handler reraises the exception
          which is then caught by scrounge and the debugger is entered.

   -------------------------------------------------------------------------}
    Begin
      Writeln;
      Writeln('** Double Precision microcode not loaded.');
      Raise UnImplQCode;
    End;
{$endc}


  Handler WCSSizeError;
  {------------------------------------------------------
      Abstract:
           This exception is raised when the system attempts to
           load or jump to a control store address greater than 4K 
           and the system is running on a 4K CPU. The exception
           is reraised and the debugger entered.
           
  --------------------------------------------------------}
   Begin
     Writeln('** Access of a control store address greater than 4K attempted.');
     Writeln('** You are currently running with only 4K of writable control store.');
     Raise WCSSizeError;
   End;
   
  Procedure PrintFailMsg(cmd: DiskCommand; addr: DiskAddr;
                           softStat: integer);
  {-------------------------------------------------------
   Abstract: Prints useful information for DiskFailure or FlushFail.
   Parameters: cmd is operation doing on device when aborted
               addr is address operating on when error occured; it is
                  decomposed into Device, head, cylinder and sector
               softStat is softstatus of error.  If not IOEIOC then it and
                 the string corresponding to it are printed
  -------------------------------------------------------}
   var disk: integer;
       cheat: DiskCheatType;
       dskAddr : Packed Record case boolean of
                   true: (i: integer);
                   false: (sec: 0..29;
                           head: 0..7;
                           cyl: 0..201);
                   end;
       msg: String;
    begin
    disk := WhichDisk(addr);
    case cmd of
      DskRead: WriteLn('READ');
      DskWrite: WriteLn('WRITE');
      DskFirstWrite: WriteLn('WRITE FIRST');
      DskReset: WriteLn('RECALIBRATE');
      DskHdrRead: begin
                  disk := 1; {special case}
                  WriteLn('FLOPPY HEADER READ');
                  end;
      DskHdrWrite: begin
                   disk := 1; {special case}
                   WriteLn('FLOPPY HEADER WRITE');
                   end;
      otherwise: WriteLn('Unknown operation');
      end;
    if softStat <> IOEIOC then
       begin
       msg := IOErrString(softStat);
       WriteLn('    Error is: ',softStat:1,' = ',msg);
       end;
    Write('    Address is ',AddrToField(addr):1:-10);

    cheat.lng := LogAddrToPhysAddr(addr);
    if disk <> 0 
    then  WriteLn( '. Floppy; Sector ', cheat.dbl[0]:1
                 , ', cylinder ', cheat.dbl[1]:1)
    else if EIOFlag or (ciodisktype = ciomicropolis)
    then  WriteLn( '. HardDisk;  Cylinder ', Cheat.dbl[1]:1
                 , ', head ', shift( Cheat.dbl[0], -8 ):1
                 , ', sector ', land( Cheat.dbl[0], 255 ):1 )
    else begin                  
      dskAddr.i := cheat.dbl[0];
      WriteLn( '. HardDisk;  Cylinder ', dskAddr.cyl:1, ', head '
             , dskAddr.head:1, ', sector ', dskAddr.sec:1);
      end;
    end;

  Handler DiskFailure(msg: string; cmd: DiskCommand; addr: DiskAddr;
                           softStat: integer);
  {-------------------------------------------------------
   Abstract: IO failure during a disk or floppy operation.  This handler prints
             a lot of information.
   Parameters: msg is message to print
               cmd is operation doing on device when aborted
               addr is address operating on when error occured; it is
                  decomposed into Device, head, cylinder and sector
               softStat is softstatus of error.  If not IOEIOC then it and
                 the string corresponding to it are printed
  -------------------------------------------------------}
    begin
    ChangeWindow(0);
    WriteLn;
    Write('** Disk Error: ',msg,' on ');
    PrintFailMsg(cmd, addr, softStat);
    Raise DiskFailure(msg, cmd, addr, softStat);
    end;

  Handler FlushFail(msg: string; cmd: DiskCommand; addr: DiskAddr;
                           softStat: integer);
  {-------------------------------------------------------
   Abstract: IO failure during a FLUSH operation.  This handler prints
             a lot of information.
   Parameters: msg is message to print
               cmd is operation doing on device when aborted
               addr is address operating on when error occured; it is
                  decomposed into Device, head, cylinder and sector
               softStat is softstatus of error.  If not IOEIOC then it and
                 the string corresponding to it are printed
  -------------------------------------------------------}
    begin
    ChangeWindow(0);
    WriteLn;
    Write('** Flush Error: ',msg,' on ');
    PrintFailMsg(cmd, addr, softStat);
    Raise FlushFail(msg, cmd, addr, softStat);
    end;

Begin {System}

DDS := FirstDDS;
SystemInitialized := false;
UserMode := false;
InPmd := false;
ShellCtrl := NIL;
DefCursFunct := Ord(CTNormal);
DefScrOff := false;
DefScrComp := false;
CtrlCPending := False;
SetDDS(200);

InitMemory;              { DDS 201-299 }
SetDDS(300);
InitIO;                  { DDS 301-499 }
SetDDS(500);
InitStream;              { DDS 501-599 }
SetDDS(600);
FSInit;                  { DDS 601-699 }
SetDDS(700);

CmdSegment := 0;
Reset(Input,'Console:');
Rewrite(Output,'Console:');
SetDDS(800);

InitExceptions;
SetDDS(810);
SysVers(SystemVersion,StrVersion);
SetDDS(820);
IOGetTime(curTime);
SetDDS(822);
Past.Lower := curTime[0];
Past.Upper := curTime[1];
SetDDS(824);
curTime[0] := 0;
curTime[1] := 0;
PastStamp := Recast(curTime, TimeStamp);
SetDDS(900);

FSSetupSystem(SysBootChar);
SetDDS(950);
If SysBootChar > Ord('Z') Then { booted from hard disk }
    Begin
    isFloppy := false;
    SetDDS(951);
    EnableSwapping(FSLocalLookUp('>',Ignore,Ignore));
    SetDDS(952);
    End
else isFloppy := true;

{$ifc Ether10MBaud then}
SetDDS(960);
E10Init;        {960 - 969}
SetDDS(969);
{$endc}

if CF_IOBoard = CF_EIO
then
    begin
    SetDDS(970);                            { Starting to load Z80 }
    Z80BootName := concat( Concat('System.', StrVersion),'.?.ZBoot');
    Z80BootName[Length(Z80BootName) - 6] := Chr(SysBootChar);

    
    LogAdr[0] := 0;                         { Force starting address on error }
    case TekLoad( Z80BootName, StartAddress ) of
      TekOK  : LogAdr[0] := StartAddress;   { if boot worked, start here }
      TekIO  :
          writeln('IO Error Reading ', Z80BootName, ', Z80 Boot failed.' );
      TekFNF : 
          writeln( Z80BootName, ' not found, Z80 Boot failed.' );
      TekFMT :
          writeln( Z80BootName, ' is formatted incorrectly, Z80 Boot failed.');
      end;

    new( StsPtr );
    UnitIO( Z80, nil, IOWriteRegs, 0, LogAdr, nil, StsPtr );
    dispose( StsPtr );
    ReInitDevices;                          { Re-initialize Z80 devices }
    SetDDS(979);                            { Z80 Load Complete }
    end;


{$ifc GetDblFloat then}
SetDDS(980);
LoadDblFloat;                          { load 64-bit Floating Point 
                                         Microcode overlay }
{$endc}

SetDDS(999);


ShellName := Concat(LogConst, Concat(StrVersion, '.Run'));
SystemInitialized := true;
InCmdFile := False;
{$ifc SysTiming then}
PrintStatistics := False;
{$endc}

While True Do
  Begin
    Command;
    99: ;
  End;

End {System}.
