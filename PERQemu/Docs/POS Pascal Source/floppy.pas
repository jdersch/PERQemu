{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program floppys;
{-----------------------------------------------------------------
{
{ Program Floppys - Diana Connan Forgy
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{
{     Floppys is the command interpreter for all of the Floppy
{     Disk environment commands. It calls the routines in 
{     FloppyUtils, which call routines in FloppyTransfers
{     (formerly PLX and PLXUtil), FloppyFormat (formerly Floppy),
{     and FloppyCopy (formerly FloppyDup).
{
{------------------------------------------------------------------}
{$Version V2.0 for POS}
{-----------------------------------------------------------------
{
{ Change Log:
{
{  3 Feb 83 V2.0  Dirk Kalp
{ Much hacking done to fix lots of problems including making the program
{ work using Perq2 double density (i.e., mask apparent hardware problems).
{ Fixed up the retry and verify mechanisms and added command XSHOWERRORS
{ for diagnostic use. Changes made to all the other floppy modules.
{
{ 30 Mar 83 V1.3  Brad Myers
{ Allow the wait switch for Compare.
{
{ 24 Feb 83 V1.2  Jerry Conner
{ Fixed bugs in FloppyUtils and FloppyTransfers.
{
{ 14 Oct 82 V1.1  Roger Riggs
{ Updated to support new I/O subsystem.  Added Interleave switch
{ to control the order of phyical sectors during Format commands.
{
{ 12 May 82 V1.0 Brad Myers
{ Fix lots of bugs and make work for POS.
{
{  3 May 82 V0.4 JLC
{ Change error message text and catch DevNotFree exception.
{
{ 19 Apr 82 V0.3 JLC
{ Change FloppyFSFloppy to BootFloppyVers from systemdefs
{
{ 23 Mar 82 V0.2 CB
{ Added calls to initialization and termination logic for floppy IO
{ under MPOS. 
{
{ 28 Feb 82 V0.1 DCF
{ Added commands Mount, Dismount, Pause, FSFloppy, and Path 
{ and conditional compilation for use in MPOS boot floppies.
{
{ 23 Dec 81 V0.0 DCF 
{ Created Program Floppys.
{
{-----------------------------------------------------------------}

Imports PopCmdParse from PopCmdParse;
Imports FloppyUtils from FloppyUtils;
Imports FloppyDefs from FloppyDefs;
Imports Clock from Clock;
Imports FileUtils from FileUtils;
Imports FloppyTransfers from FloppyTransfers;
Imports System from System;
Imports Stream from Stream;

{$IFC BootFloppyVers THEN}
Imports Memory from Memory; { for stuff to turn swapping on and off }
Imports Screen from Screen; { to shrink screen when swapping's disabled }
{$ENDC}

{$IFC MPOSVersion THEN}
imports DevInterf from DevInterf;
{$ENDC}


Label 1;

const Version = 'V2.0';

var TitleLine, NotWithFSFloppy: String;
    OnCmdLine: Boolean;
         




Procedure SetInitialState;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Set the initial state of the program by clearing strings,
{     setting Blooleans and other variables to their default
{     values.
{
{-----------------------------------------------------------------}

begin
     
     { initialize Floppy strings }
     
     TitleLine := Concat (Concat ('FLOPPY ', Version), 
               '   Type HELP if you need it.'); 
     NotWithFSFloppy := '** You cannot use this routine when running from a filesystem floppy.';     
     FirstCmd := False;
     CommandName := '';
     SwitchName := '';
     
     { set the defaults as SAFE!!!  }
     
     Confirm := True;
     HoldConfirm := True;
     AskSingle := False;
     HoldAskSingle := False;
     AskWild := true;
     HoldAskWild := true;
     ConfirmNonWildDelete := false;
     HHoldConfirmNonWildDelete := false;
     FSFlop := False;
     Interleave := 2;     
     Verify := True;
     HoldVerify := True;
     
     
     { assume the floppy's been formatted. }
     
     Sides := 2;
     Unformatted := False;
     
     { initialize strings that will be used in FloppyUtils }
     
     Answer := '';
     InAnswer := ''; 
     OutAnswer := ''; 
     InQuery := ''; 
     OutQuery := '';
     
     Finished := False;
     ExitRoutine := False; 

     InFiles := NIL;
     OutFiles := NIL;
     Switches := NIL;
     DummyList := NIL;

end { SetInitialState };



Procedure SetCmdTable;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Set up the table of commands called in Floppy.
{
{-----------------------------------------------------------------}

begin 

{$R-}
      
     FirstPress := True;
     CmdTable ^.Header                       := 'FLOPPY CMD';
     
     CmdTable^.Commands [ GetIndex ]         := 'GET';
     CmdTable^.Commands [ PutIndex ]         := 'PUT';
     CmdTable^.Commands [ CmprIndex ]        := 'COMPARE';
     CmdTable^.Commands [ CompIndex ]        := 'COMPRESS';
     CmdTable^.Commands [ DelIndex ]         := 'DELETE';
     CmdTable^.Commands [ DirIndex ]         := 'DIRECTORY';
     CmdTable^.Commands [ HelpIndex ]        := 'HELP';
     CmdTable^.Commands [ RenIndex ]         := 'RENAME';
     CmdTable^.Commands [ TypeIndex ]        := 'TYPE';
     CmdTable^.Commands [ ZeroIndex ]        := 'ZERO';
     CmdTable^.Commands [ DupIndex ]         := 'DUPLICATE';
     CmdTable^.Commands [ FlpGetIndex ]      := 'FLOPPYGET';
     CmdTable^.Commands [ FlpPutIndex ]      := 'FLOPPYPUT';
     CmdTable^.Commands [ FormatIndex ]      := 'FORMAT';
     CmdTable^.Commands [ DensIndex ]        := 'DENSITY';
     CmdTable^.Commands [ FastIndex ]        := 'FAST';
     CmdTable^.Commands [ SafeIndex ]        := 'SAFE';
     CmdTable^.Commands [ ConfIndex ]        := 'CONFIRM';
     CmdTable^.Commands [ NoConfIndex ]      := 'NOCONFIRM';
     CmdTable^.Commands [ AskIndex ]         := 'ASK';
     CmdTable^.Commands [ NoAskIndex ]       := 'NOASK';
     CmdTable^.Commands [ VerIndex ]         := 'VERIFY';
     CmdTable^.Commands [ NoVerIndex ]       := 'NOVERIFY';
     CmdTable^.Commands [ MountIndex ]       := 'MOUNT';
     CmdTable^.Commands [ DismtIndex ]       := 'DISMOUNT';
     CmdTable^.Commands [ PauseIndex ]       := 'PAUSE';
     CmdTable^.Commands [ FSFlopIndex ]      := 'FSFLOPPY';
     CmdTable^.Commands [ PathIndex ]        := 'PATH';
     CmdTable^.Commands [ QuitIndex ]        := 'QUIT';
     CmdTable^.Commands [ XShowErrIndex ]    := 'XSHOWERRORS';
     
{$R=}

end { SetCmdTable };




Procedure SetSwitchTable;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Set up the table of switches possible in Floppy.
{
{-----------------------------------------------------------------}

begin

{$R-}
     
     FirstPress := True;
     SwitchTable^.Header := '';
     
     SwitchTable^.Commands [ AskSwitch ]    := 'ASK';
     SwitchTable^.Commands [ NoAskSwitch ]  := 'NOASK';
     SwitchTable^.Commands [ VerSwitch ]    := 'VERIFY';
     SwitchTable^.Commands [ NoVerSwitch ]  := 'NOVERIFY';
     SwitchTable^.Commands [ HelpSwitch ]   := 'HELP';
     SwitchTable^.Commands [ ConfSwitch ]   := 'CONFIRM';
     SwitchTable^.Commands [ NoConfSwitch ] := 'NOCONFIRM';

     SwitchTable^.Commands [ DblDensity ]   := 'DBLDENSITY';
     SwitchTable^.Commands [ SingleDensity ]:= 'SINGLEDENSITY';
     SwitchTable^.Commands [ DblSided ]     := 'DBLSIDED';
     SwitchTable^.Commands [ SingleSided ]  := 'SINGLESIDED';
     SwitchTable^.Commands [ Test ]         := 'TEST';
     SwitchTable^.Commands [ SWInterleave ] := 'INTERLEAVE';
     SwitchTable^.Commands [ NoTest ]       := 'NOTEST';
     SwitchTable^.Commands [ ShortSwitch ]  := 'SHORT';
     SwitchTable^.Commands [ LongSwitch ]   := 'LONG';
     SwitchTable^.Commands [ DelSwitch ]    := 'DELETE';
     SwitchTable^.Commands [ NoDelSwitch ]  := 'NODELETE';
     SwitchTable^.Commands [ WaitSwitch ]   := 'WAIT';
     SwitchTable^.Commands [ NoWaitSwitch ] := 'NOWAIT';
     SwitchTable^.Commands [ FSSwitch ]     := 'FSFLOPPY';

     {}
     { Switches used only by the XSHOWERRORS cmd.
     {}
     SwitchTable^.Commands [ XAllSwitch ]   := 'XALL';
     SwitchTable^.Commands [ XNoneSwitch ]  := 'XNONE';
     SwitchTable^.Commands [ XDDensSwitch ] := 'XDDENS';
     SwitchTable^.Commands [ XTrackSwitch ] := 'XTRACK';
     SwitchTable^.Commands [ XVerifySwitch ]:= 'XVERIFY';

{$R=}
end { SetSwitchTable };



{ Main Program }

     Handler CtlCAbort;
     begin
          CtrlCPending := False;
          StreamKeyboardReset (FlpCmdFile^.CmdFile);
          Writeln ('^C');
          RestoreSwitches;
          QuitProgress;
          DstryArgRec(DummyList);
          Goto 1;
     end { CtlCAbort };
     
{$ifc MPOSVersion then}
Handler DevNotFree;
begin
writeln('** Floppy drive not available');
exit(floppys);
end;
{$endc}

begin
     

     SetInitialState;
{$ifc MPOSVersion then}
     FlpInitialize;
{$endc}
     Init;    { procedure in FloppyTransfers to initialize the floppy stuff. }
     AllocNameDesc (NumCmds, 0, CmdTable);
     AllocNameDesc (NumSwitches, 0, SwitchTable);
     SetCmdTable;
     SetSwitchTable;
     InitCmdFile (FlpCmdFile, 0);
     OnCmdLine := False;
     CmdLine := '';
     FSAddToTitleLine (TitleLine);
     
     { If using a filesystem floppy, want to disable swapping
     { and shrink the screen, then reenable swapping after
     { execution finishes.                                   }
     
     {$IFC BootFloppyVers THEN }
     
     If SwappingAllowed then
     begin
          SavedSwapID := SwapID;
          ShouldReEnableSwapping := True;
          DisableSwapping;
          SSetSize (256, false, false);
          FSAddToTitleLine (TitleLine);
     end;
     
     {$ENDC}
     
     
     I := GetShellCmdLine ( CommandName, FlpCmdFile, CmdTable);
     If (I <> NumCmds + 3) or (FlpCmdFile^.next <> NIL) then OnCmdLine := True;
     If (I >= 1) and (I <= NumCmds) then
           If not ParseCmdArgs (InFiles, OutFiles, Switches, Error) then
              if i <> PauseIndex then 
                  begin
                  Writeln (Error);
                  goto 1;
                  end;
     While True do
     begin

          Case I of
               
               { All of the routines called in this case
               { statement are in Module FloppyUtils.   }
                   
                   GetIndex:    Get; 
                   PutIndex:    {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} Put;
                                {$ENDC}
                   CmprIndex:   Compare;
                   CompIndex:   {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} Compress;
                                {$ENDC}
                   DelIndex:    DeleteFile;
                   DirIndex:    Directory; 
                   RenIndex:    Rename;
                   TypeIndex:   TypeFile;
                   ZeroIndex:   {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} Zero;
                                {$ENDC}
                   DupIndex:    {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} CopyFloppy;
                                {$ENDC}
                   FlpGetIndex: {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} DumpToDisk;
                                {$ENDC}
                   FlpPutIndex: {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} DumpToFloppy;
                                {$ENDC}
                   FormatIndex: {$IFC BootFloppyVers THEN} 
                                      writeln (NotWithFSFloppy);
                                {$ELSEC} Format;
                                {$ENDC}
                   FastIndex:   Fast;
                   SafeIndex:   Safe;
                   HelpIndex:   Help;
                   QuitIndex:   begin
{$ifc MPOSVersion then}
                                     FlpDone;
{$endc}
                                     Quit;
                                     If Finished then Exit (Floppys);
                                end;
                   DensIndex:   ShowDensity;
                   ConfIndex:   DoConfirm;  
                   NoConfIndex: NoConfirm;  
                   AskIndex:    DoAsk;
                   NoAskIndex:  NoAsk;
                   VerIndex:    DoVerify;
                   NoVerIndex:  NoVerify;
                   MountIndex:  Mount;
                   DismtIndex:  Dismount;
                   PauseIndex:  Pause;
                   PathIndex:   begin
                                     Path;
                                     If ChangedPath then
                                          FSAddToTitleLine (TitleLine);
                                end;
                   FSFlopIndex: FSFloppy;
                   XShowErrIndex: if CommandName <> 'XSHOWERRORS' then
                                     {}
                                     { Make this cmd hard to get at so users
                                     { do not stumble upon it.
                                     {}
                                     StdError(ErBadCmd, CommandName, false)
                                  else XShowErrors;
                   NumCmds + 1: StdError(ErBadCmd, CommandName, false);
                   NumCmds + 2: StdError(ErCmdNotUnique, CommandName, false);
                   NumCmds + 3: {nothing}; 
                   NumCmds + 4: begin
                                     CommandName := Concat ('/', CommandName);
                                     CmdLine := Concat (CommandName, CmdLine);
                                     FirstSwitch (CmdLine) ;
                                end; 
                   NumCmds + 5: WriteLn('** Illegal character found.');
                   Otherwise:   StdError(ErBadCmd, CommandName, false);
            
               end { of Case statement };
          
          If (OnCmdLine) and (FlpCmdFile^.next = NIL) then 
          { the command was gotten from Shell.}
          begin
               DestroyNameDesc (CmdTable);
               DestroyNameDesc (SwitchTable);
               Exit (Floppys);
          end;

   1:     if I <> NumCmds + 3 then writeln;
          DstryArgRec(InFiles);
          DstryArgRec(OutFiles);
          DstrySwitchRec(Switches);
          I := GetCmdLine( NullIdleProc, 'FLOPPY', CmdLine, CommandName, 
                           FlpCmdFile, CmdTable, FirstPress, True);

          If (I >= 1) and (I <= numCmds) then
               If not ParseStringArgs 
                    (CmdLine, InFiles, OutFiles, Switches, Error) then
                 if i <> PauseIndex then {the argument to pause is undefined}
                     begin
                     Writeln(Error);
                     goto 1;
                     end;
  end { of While True Do loop };
end.
      
      
