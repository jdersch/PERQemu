{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FloppyUtils;
{------------------------------------------------------------------
{
{ Module FloppyUtils - Diana Connan Forgy
{ Copyright (C) Three Rivers Computer Corporation, 1981.
{
{ Abstract:
{
{     FloppyUtils calls the routines in modules FloppyTransfers,
{     FloppyFormat, and FloppyCopy, which are used by Program Floppy, 
{     the Floppy disk environment program.
{
{-----------------------------------------------------------------}

{$Version V2.0 for POS}
{------------------------------------------------------------------
{
{ Change Log:
{
{  3-Feb-84  V2.0  Dirk Kalp
{ Add procedure XShowErrors to handle the XSHOWERRORS Floppy command.
{ This command is used to turn ONN/OFF certain error reporting that
{ is, by current default, suppressed for floppy operations. This includes
{ the abundant errors that are sometimes produced with Perq2 double density
{ floppy operations as well as errors on floppy track operations and special
{ Verify error messages. This command is intended for in-house diagnostic use
{ and some effort has been made to hide its existence from normal users (i.e.,
{ you must type the command name in full and in upper case and it is not listed
{ by the Help msg for Floppy). See also the Floppytransfers module.
{
{ 26-Oct-83  V1.4  Scott Brown
{ Program around compiler bug; "directory/HELP" used to cause execution
{ of an undefined qcode because of a bad offset in the EXGO instruction.
{
{ 30 Mar 83 V1.3  Brad Myers (for Kwok Sheh)
{ Added Wait switch for Compare Command
{
{ 24 Feb 83 V1.2  Jerry Conner
{ Fix bug parsing input and output names.
{
{ 14 Oct 82 V1.1  Roger Riggs
{ Updated to support new I/O subsystem.  Added Interleave switch
{ to control the order of phyical sectors during Format commands.
{
{ 12 May 82 V1.0  Brad Myers
{ Make work for POS.  Fixed lots of bugs.
{
{  3 May 82 V0.5  JLC
{ Change text in Path routine.
{
{ 19 Apr 82 V0.4  JLC
{ Change FloppyFSFloppy to BootFloppyVers
{
{ 18 Mar 82 SSJ V0.3
{ Change every occurrence of Terminate to FLTerminate.
{
{ 28 Feb 82 DCF V0.2
{ Added procedures Mount, Dismount, Pause, FSFloppy, and Path
{ for use with MPOS boot floppies. Also added conditional
{ compilation to reduce the size of .Seg files on boot floppies.
{
{ 27 Jan 82 DCF V0.1
{ Various bug fixes for POS D.6 release.
{
{ 23 Dec 81 DCF V0.0
{ Created FloppyUtils.
{
{-----------------------------------------------------------------}

Exports 

Procedure FirstSwitch (Line: String);
Procedure RestoreSwitches;
Procedure Get;
Procedure Compare;
Procedure DeleteFile;
Procedure Directory;
Procedure Rename;
Procedure TypeFile;
Procedure Help;
Procedure ShowDensity;
Procedure DoConfirm;
Procedure NoConfirm;
Procedure Fast;
Procedure Safe;
Procedure DoAsk;
Procedure NoAsk;
Procedure DoVerify;
Procedure NoVerify;
Procedure Mount;
Procedure Dismount;
Procedure Pause;
Procedure FSFloppy;
Procedure Path;
Procedure Quit;
Procedure XShowErrors;
Function CVD(S : String): Integer;

Imports PopCmdParse from PopCmdParse;
Imports FloppyDefs from FloppyDefs;
Imports FloppyTransfers from FloppyTransfers;

var ChangedPath: Boolean; { to tell Floppy to change the title 
                          { line if the path has been changed. }

{$IFC not BootFloppyVers THEN} { These routines and modules  aren't
                              { needed for filesystem floppies, so we
                              { can save some space this way.        }
Procedure Put;
Procedure Compress;
Procedure Zero;
Procedure CopyFloppy;
Procedure DumpToDisk;
Procedure DumpToFloppy;
Procedure Format;

Imports FloppyFormat from FloppyFormat;
Imports FloppyCopy from FloppyCopy;
{$ENDC}


Private

Imports Pmatch from PMatch;
Imports FileSystem from FileSystem;
Imports FileUtils from FileUtils;

var InCurrent, OutCurrent: pArgRec;
    CurSwitch: pSwitchRec;
    DoTest, DblD, DblS, IllegalSwitch, JustHelp: Boolean;
    Caller: String;
    ZFNoConfirm: boolean
    


Procedure DoSwitches (CalledFrom: String);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Process the standard switches (Ask/NoAsk, Confirm/NoConfirm,
{     Verify/NoVerify, Help) for a command before continuing 
{     with its execution.
{
{-----------------------------------------------------------------}
     

var NonUnique, Nonexistent: String;

begin
    
     JustHelp := False;
     IllegalSwitch := False;
     If Switches = NIL then Exit (DoSwitches);
     CurSwitch := Switches;
     While CurSwitch <> NIL do
          begin
          SwitchName := CurSwitch^.Switch;
          NonUnique := ' is not a unique switch.';
          Nonexistent := ' is an invalid switch.';
          WhichSwitch := PopUniqueCmdIndex (SwitchName, SwitchTable);
          
          If (CurSwitch^.Arg <> '') and (WhichSwitch <> SWInterleave)
          then begin
               StdError(ErNoSwParam, SwitchName, false);
               IllegalSwitch := True;
               Exit (DoSwitches);
               end;
          
          Case WhichSwitch of
          
              AskSwitch:      If ((CalledFrom = 'PUT') or 
                                  (CalledFrom = 'GET') or 
                                  (CalledFrom = 'RENAME'))  
                              then begin
                                   AskSingle := true;
                                   AskWild := true;
                                   end
                              else IllegalSwitch := True;
 
            NoAskSwitch:      If ((CalledFrom = 'PUT') or   
                                  (CalledFrom = 'GET') or 
                                  (CalledFrom = 'RENAME'))  
                              then begin
                                   AskSingle := false;
                                   AskWild := false;
                                   end
                              else IllegalSwitch := True;
 
              VerSwitch:      If ((CalledFrom = 'PUT') or
                                  (CalledFrom = 'GET') or 
                                  (CalledFrom = 'COMPRESS') or 
                                  (CalledFrom = 'DUPLICATE') or 
                                  (CalledFrom = 'FLOPPYGET') or 
                                  (CalledFrom = 'FLOPPYPUT'))  
                              then Verify := true
                              else IllegalSwitch := True;
                               
 
            NoVerSwitch:      If ((CalledFrom = 'PUT') or
                                  (CalledFrom = 'GET') or 
                                  (CalledFrom = 'COMPRESS') or 
                                  (CalledFrom = 'DUPLICATE') or 
                                  (CalledFrom = 'FLOPPYGET') or 
                                  (CalledFrom = 'FLOPPYPUT'))  
                              then Verify := false
                              else IllegalSwitch := True;
                               
 
             ConfSwitch:      If ((CalledFrom = 'PUT') or
                                  (CalledFrom = 'GET') or 
                                  (CalledFrom = 'DELETE') or 
                                  (CalledFrom = 'DIRECTORY') or 
                                  (CalledFrom = 'ZERO') or 
                                  (CalledFrom = 'RENAME') or 
                                  (CalledFrom = 'DUPLICATE') or 
                                  (CalledFrom = 'FLOPPYGET') or 
                                  (CalledFrom = 'FORMAT'))  
                              then begin
                                   Confirm := true;
                                   if CalledFrom = 'DELETE' then
                                      ConfirmNonWildDelete := true;
                                   end
                              else IllegalSwitch := True;
                              
 
           NoConfSwitch:      If ((CalledFrom = 'PUT') or
                                  (CalledFrom = 'GET') or 
                                  (CalledFrom = 'DELETE') or 
                                  (CalledFrom = 'DIRECTORY') or 
                                  (CalledFrom = 'ZERO') or 
                                  (CalledFrom = 'RENAME') or 
                                  (CalledFrom = 'DUPLICATE') or 
                                  (CalledFrom = 'FLOPPYGET') or 
                                  (CalledFrom = 'FORMAT'))  
                              then begin
                                   Confirm := false;
                                   ZFNoConfirm := true; {kludge}
                                   if CalledFrom = 'DELETE' then
                                      ConfirmNonWildDelete := false;
                                   end
                              else IllegalSwitch := True;
                               
 
              HelpSwitch:      If FirstCmd then Help else JustHelp := True; 

              DblDensity:      If CalledFrom = 'FORMAT' then Dens := DDens
                               Else IllegalSwitch := True;
                               
 
              SingleDensity:   If CalledFrom = 'FORMAT' then Dens := SDens
                               Else IllegalSwitch := True;
                               
 
              SingleSided:    If ((CalledFrom = 'FORMAT') or
                                  (CalledFrom = 'ZERO') or
                                  (CalledFrom = 'DUPLICATE') or 
                                  (CalledFrom = 'FLOPPYGET') or 
                                  (CalledFrom = 'FLOPPYPUT')) then Sides := 1
                              Else IllegalSwitch := True;

              DblSided:    If ((CalledFrom = 'FORMAT') or
                                  (CalledFrom = 'ZERO') or
                                  (CalledFrom = 'DUPLICATE') or 
                                  (CalledFrom = 'FLOPPYGET') or 
                                  (CalledFrom = 'FLOPPYPUT')) then Sides := 2
                               Else IllegalSwitch := True;


              Test:            If CalledFrom = 'FORMAT' then FmtTest:= true
                               Else IllegalSwitch := True;


              NoTest:          If CalledFrom = 'FORMAT' then FmtTest:= false
                               Else IllegalSwitch := True;


              SWInterleave:    if  (CalledFrom = 'FORMAT') AND
                                   (CVD(CurSwitch^.Arg) >= 0) AND
                                   (CVD(CurSwitch^.Arg) <= 25)
                               then Interleave := CVD(CurSwitch^.Arg)
                               else IllegalSwitch := True;

              ShortSwitch:     If CalledFrom = 'DIRECTORY' then LongDir:= False
                               Else IllegalSwitch := True;


              LongSwitch:      If CalledFrom = 'DIRECTORY' then LongDir:= True
                               Else IllegalSwitch := True;


              NoDelSwitch:     If (CalledFrom = 'DUPLICATE') or 
                                    (CalledFrom = 'FLOPPYPUT') 
                                    or (CalledFrom = 'FLOPPYGET') then 
                                    DeleteScratch := False
                               Else IllegalSwitch := True;

              DelSwitch:     If (CalledFrom = 'DUPLICATE') or 
                                    (CalledFrom = 'FLOPPYPUT') 
                                    or (CalledFrom = 'FLOPPYGET') then 
                                    DeleteScratch := true
                               Else IllegalSwitch := True;

             NoWaitSwitch:     If (CalledFrom = 'TYPE') or
                                  (CalledFrom = 'COMPARE') then Wait := false
                               Else IllegalSwitch := True;
              
             WaitSwitch:     If (CalledFrom = 'TYPE') or 
                                (CalledFrom = 'COMPARE') then Wait := true
                               Else IllegalSwitch := True;
              
              FSSwitch:        If    (CalledFrom = 'MOUNT') 
                                  or (CalledFrom = 'DISMOUNT') 
                                  or (CalledFrom = 'PAUSE')
                               then FSFlop := True
                               Else IllegalSwitch := True;

              NumSwitches + 1: IllegalSwitch := True;
              NumSwitches + 2: Writeln ('** ', SwitchName:1, NonUnique);
              NumSwitches + 3: { do nothing };
              NumSwitches + 4: { do nothing };
              NumSwitches + 5: Writeln ('** Illegal character found.');
              Otherwise:       IllegalSwitch := True;
          
          end { Case };
          if illegalSwitch then StdError(ErBadSwitch,SwitchName, false);
          if whichSwitch > NumSwitches then illegalSwitch := true;
          if illegalSwitch then exit(DoSwitches);
          CurSwitch := CurSwitch^.Next;
      end { WHILE not end of switch list };

end { DoSwitches };


Procedure BigBeep(j: integer);
var i: integer;
begin
   for i:=1 to j do Write(Bell);
end;


Function CVD(S : String): Integer;

Var
    i : Integer;

    Sum : Integer;
begin
if Length(S) <= 0 then begin CVD := -1; Exit(CVD); end;

Sum := 0;
for i := 1 to Length(s)
do  begin
    if (s[i] < '0') or (s[i] > '9')
    then begin CVD := -1; Exit(CVD) end;
    Sum := Sum * 10 + Ord(S[i]) - Ord('0');
    end;
CVD := Sum;
end;


Procedure XShowErrors;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     This procedure is provided to handle the XSHOWERRORS cmd
{     that is used for in-house testing of Floppy. It permits us
{     to turn ON/OFF error reporting for certain floppy operations.
{     The cmd has switches only to select the actions. The first 3
{     of the valid switches below take an ON/OFF arg.
{     (eg: XSHOWERRORS /XDDENS=OFF)
{
{     Valid switches are:
{         XDDENS  - ON to show all errors on Perq2 double density
{         XTRACK  - ON to show errors for track operations
{         XVERIFY - ON to get special msg when a Verify error occurs
{         XALL    - turns ON all the above switches
{         XNONE   - turns OFF all the above switches
{     The initial defaults are all OFF.
{
{
{-----------------------------------------------------------------}
var Arg: string;
    ShowDDErr, ShowTrkErr, ShowVerErr: boolean;

   function ArgVal(SwName: string): boolean;
   var str: string;
   begin
      if Pos('ON', Arg) = 1 then
         ArgVal := true
      else if Pos('OFF', Arg) = 1 then
         ArgVal := false
      else
         begin
         StdError(ErSwParam, SwName, false);
         exit(XShowErrors);
         end;
      if length(Arg) = 1 then
         begin
         str := ConCat(ConCat('** Argument for ', SwName), ' is not unique.');
         StdError(ErAnyError, str, false);
         exit(XShowErrors);
         end;
   end;

begin
   if Switches = NIL then exit(XShowErrors);
   ShowDDErr := not HidingDDensErrors;
   ShowTrkErr := ShowTrackErrors;
   ShowVerErr := ShowVerifyErrors;
   IllegalSwitch := false;
   CurSwitch := Switches;
   while CurSwitch <> NIL do
      begin
      SwitchName := CurSwitch^.Switch;
      Arg := CurSwitch^.Arg;
      ConvUpper(Arg);
      WhichSwitch := PopUniqueCmdIndex(SwitchName, SwitchTable);
      case WhichSwitch of
         HelpSwitch:
            begin
            writeln('   XSHOWERRORS');
            writeln('      Turn ON or OFF the display of certain Floppy errors. Selections are');
            writeln('      provided through command line switches where the first 3 switches');
            writeln('      listed below take an argument (eg: XSHOWERRORS /XTRACK=ON).');
            writeln;
            writeln('      Valid switches are:');
            writeln('          XDDENS  - ON to show all errors on Perq2 double density');
            writeln('          XTRACK  - ON to show errors for track operations');
            writeln('          XVERIFY - ON to get special msg when a Verify error occurs');
            writeln('          XALL    - turns ON all the above switches');
            writeln('          XNONE   - turns OFF all the above switches');
            writeln('      The initial defaults are all OFF.');
            exit(XShowErrors);
            end;
            
         XAllSwitch:
            if Arg <> '' then
               begin
               StdError(ErNoSwParam, SwitchName, false);
               exit(XShowErrors);
               end
            else
               begin
               ShowDDErr := true;
               ShowTrkErr := true;
               ShowVerErr := true;
               end;
            
         XNoneSwitch:
            if Arg <> '' then
               begin
               StdError(ErNoSwParam, SwitchName, false);
               exit(XShowErrors);
               end
            else
               begin
               ShowDDErr := false;
               ShowTrkErr := false;
               ShowVerErr := false;
               end;

         XDDensSwitch:
            ShowDDErr := ArgVal('XDDENS');

         XTrackSwitch:
            ShowTrkErr := ArgVal('XTRACK');

         XVerifySwitch:
            ShowVerErr := ArgVal('XVERIFY');

         NumSwitches + 1: IllegalSwitch := True;
         NumSwitches + 2: 
             begin
             Writeln ('** ', SwitchName:1, ' is not unique.');
             exit(XShowErrors);
             end;
         NumSwitches + 3: { do nothing };
         NumSwitches + 4: { do nothing };
         NumSwitches + 5:
             begin
             Writeln ('** Illegal character found.');
             exit(XShowErrors);
             end;
         Otherwise:       IllegalSwitch := True;
     
      end{case};
      if illegalSwitch then
         begin
         StdError(ErBadSwitch,SwitchName, false);
         exit(XShowErrors);
         end;
      CurSwitch := CurSwitch^.Next;
      end{while};
                            
   HidingDDensErrors := not ShowDDErr;
   ShowTrackErrors := ShowTrkErr;
   ShowVerifyErrors := ShowVerErr;
end { XShowErrors };
     


Procedure NextInput;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Get a user-specified input file to process.
{
{-----------------------------------------------------------------}
begin
     CnvUpper (InCurrent^.Name);
     InCurrent := InCurrent^.Next;
end { NextInput };
     


Procedure NextOutput;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Get a user-specified output file to process.
{
{-----------------------------------------------------------------}
begin
     CnvUpper (OutCurrent^.Name);
     OutCurrent := OutCurrent^.Next; 
end { NextOutput };     




Function Size (List: pArgRec): Integer;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Count the elements in an inputs or outputs list.
{
{ Returns:
{
{     The number of elements in the list.
{
{-----------------------------------------------------------------}
var I: integer;

begin
     I := 0;
     While List <> NIL do
     begin
          List := List^.Next;
          I := I + 1;
     end;
     Size := I;
end { Size };


Function GetInputs (InQuery: String): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Prompt user for input files to process.
{
{-----------------------------------------------------------------}
begin
     GetInputs := false;
     repeat
         Write(InQuery);
         Readln(InAnswer);
     Until InAnswer <> '';
     If not ParseStringArgs (InAnswer, InFiles, OutFiles, Switches, Error) then
        StdError(ErAnyError, error, false)
     else
        GetInputs := true;
end { GetInputs };


Function GetOutputs (OutQuery: String; PromptForMore: Boolean): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Prompt user for output files to process.
{
{-----------------------------------------------------------------}
var temp: pArgRec;
begin
     GetOutputs := false;
     DummyList := NIL;
     repeat
        Write(OutQuery);
        if not PromptForMore then {print default which is input}
          begin
          Write(' [');
          temp := infiles;
          while temp <> NIL do
             begin
             Write(temp^.name);
             if temp^.next <> NIL then write(', ');
             temp := temp^.next;
             end;
          Write('] ');
          end;
        Readln (OutAnswer);
     until (OutAnswer = '') or (not PromptForMore);
     If OutAnswer = '' then
        begin
        GetOutputs := true;
        exit(GetOutputs);
        end;
      
     If not ParseStringArgs (OutAnswer, OutFiles, DummyList, Switches, Error)
       then begin
            StdError(ErAnyError, error, false);
            exit(GetOutputs);
            end;
     if DummyList^.name <> '' then writeln('** No additional outputs allowed.')
     else GetOutputs := true;
     DstryArgRec(DummyList);
end { GetOutputs };
     

Procedure FirstSwitch (Line: String);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Called when the first thing on a command line was a
{     switch.
{
{-----------------------------------------------------------------}
var InDummy, OutDummy: pArgRec;

begin
     Caller := '';
     InDummy := NIL;
     OutDummy := NIL;
     FirstCmd := True;
     If ParseStringArgs (Line, InDummy, OutDummy, Switches, Error)
         then DoSwitches (Caller) 
     else writeln(Error);
     
     FirstCmd := False;
     RestoreSwitches;
end { FirstSwitch };


Procedure RestoreSwitches;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Restore the switches Confirm, Ask, and Verify to their
{     global value, which was saved in HoldConfirm, HoldAsk,
{     and HoldVerify. This is done after each routine so that
{     if a switch's value was changed for that routine, it
{     will be restored after the routine has executed.
{
{-----------------------------------------------------------------}

begin
     Confirm := HoldConfirm;
     ConfirmNonWildDelete := HHoldConfirmNonWildDelete;
     AskSingle := HoldAskSingle;
     AskWild := HoldAskWild;
     Verify := HoldVerify;
     Sides := 2;
     ExitRoutine := False;
end { RestoreSwitches };



Procedure MakeFloppyName(var name: PathName);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Modifies the name to make it a floppy name.  Takes off all directories
{     and truncates the main part to 6 characters and the extension to 3.
{
{-----------------------------------------------------------------}
   var Loc: integer;
       ext, first: String[6];
   begin
   Loc := RevPosC (name, '>');
   If Loc <> 0 then Delete(name, 1, Loc);
   Loc := PosC(name, '.');
   if loc = 0 then 
      begin
      AppendChar(name, '.');
      loc := length(name);
      end;
   if loc > 6 then first := SubStr(name, 1, 6)
   else first := subStr(name, 1, loc-1);
   if length(name) - loc > 3 then
      ext := subStr(name, loc, 4)
   else ext := subStr(name, loc, length(name)-loc+1);
   name := Concat(first, ext);
   end;  {MakeFloppyName}


Function CheckInputsAndOutputs(Procedure WriteHelp): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Checks all the inputs and outputs to see that they are legal, whether
{     the switches are OK or are HELP, and whether there are the same
{     number of inputs and outputs
{
{ Parameters:
{     WriteHelp is the help procedure.
{
{ Returns:
{     True if OK, false if should exit due to an error.
{
{ Environment:
{     Uses and modifies the globals Caller, InQuery, OutQuery, InFiles,
{     OutFiles, Switches, IllegalSwitch, JustHelp, OutCurrent, InCurrent
{     
{-----------------------------------------------------------------}
     begin
     CheckInputsAndOutputs := false;
     DoSwitches(Caller);
     If IllegalSwitch then exit(CheckInputsAndOutputs);
     If JustHelp then WriteHelp;
     If InFiles^.Name = '' then 
          begin
          if not GetInputs(InQuery) then exit(CheckInputsAndOutputs);
          DoSwitches (Caller);
          If IllegalSwitch then exit(CheckInputsAndOutputs);
          If JustHelp then WriteHelp;
          If OutFiles^.Name = '' then 
             begin
             DstrySwitchRec(Switches);
             if not GetOutputs(OutQuery,False) then
                 exit(CheckInputsAndOutputs); 
             DoSwitches(Caller);
             If IllegalSwitch then exit(CheckInputsAndOutputs);
             If JustHelp then WriteHelp;
             end;
          end;
     InCurrent := InFiles;
     If OutFiles^.Name <> '' then 
          begin
          OutCurrent := OutFiles; 
          If Size (InCurrent) <> Size (OutCurrent) then
             begin
             Writeln ('** Number of input arguments (', Size(InCurrent):1,')');
             WriteLn ('**    is not equal to number of output arguments (',
                        Size(OutCurrent):1, ').');
             exit(CheckInputsAndOutputs);
             end;
          end
     Else OutCurrent := InFiles;
     CheckInputsAndOutputs := true;
     FindSidesAndDensity(true);
     If Unformatted then exit(CheckInputsAndOutputs);
     end; {CheckInputsAndOutputs}


Function CheckNames(var source, destination: PathName): boolean;
{------------------------------------------------------------------
{
{ Abstract:
{     Checks source and destination to see if they are OK.  Also removes Quotes
{     from them.
{
{ Returns:
{    True if OK; false if error.  If error, message has already been printed.
{
{------------------------------------------------------------------}
    begin
    CheckNames := false;
    if IsPattern(Source) or IsPattern(Destination) then
          writeln('** Illegal use of wildcards.')
    else if (not RemoveQuotes(Source)) or (not RemoveQuotes(Destination)) then
             StdError(ErBadQuote, '', false)
    else CheckNames := true;
    if source = '' then source := destination;
    if destination = '' then destination := source;
    end;  {CheckNames}


Procedure HelpOnlySwitch;
  begin
  Writeln ('   HELP is the only valid switch.');
  end;

Procedure SetTrueGlobally;
  begin
  Writeln ('   This will be the case globally.');
  end;


Procedure Get;
{------------------------------------------------------------------
{
{ Abstract:
{ 
{     Copy a floppy file onto the hard disk.
{
{------------------------------------------------------------------}

var Source, Destination: PathName;
    
label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   GET FloppyFile(s) DiskFile(s) - copy floppy file(s) onto disk.');
          Writeln ('   Valid switches are ASK, NOASK, CONFIRM, NOCONFIRM, VERIFY, NOVERIFY, and HELP.');
          goto 1;
     end { WriteHelp };
begin
     Caller := 'GET';
     InQuery := 'What floppy file(s) should be copied to disk? ';     
     OutQuery := 'What hard disk file name(s) should they have? ';

     if CheckInputsAndOutputs(WriteHelp) then
       While InCurrent <> NIL do
          begin
          Source := InCurrent^.Name;
          Destination := OutCurrent^.Name;
          if not CheckNames(source, destination) then goto 1;
          
          MakeFloppyName(Source);
          
          GetFile (Source, Destination);
          NextInput;
          NextOutput;
          
          end { While not end of input list };
1:   RestoreSwitches;

end { Get };




Procedure Compare;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Compare a disk file with a floppy file and report any
{     differences between them.
{
{------------------------------------------------------------------}

var DiskFile, FloppyFile: pathName;

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   COMPARE DiskFile(s) FloppyFile(s). ');
          Writeln ('   Read the two files and see if they are the same.');
          WriteLn ('   Switches: The Wait switch stops if differences');
          WriteLn ('   HELP and NoWait also allowed');
          goto 1;
     end { WriteHelp };

begin
     Wait := false;  {the default}
     Caller := 'COMPARE';
     InQuery := 'Which disk file(s) should be examined? ';
     OutQuery := 'Which floppy file(s) should they be compared with? ';
     
     If CheckInputsAndOutputs(WriteHelp) then
        While InCurrent <> NIL do
          begin
          DiskFile:= InCurrent^.Name;
          FloppyFile := OutCurrent^.Name;
          
          if not CheckNames(DiskFile, FloppyFile) then goto 1;
          
          MakeFloppyName(FloppyFile);

          DoCompare (DiskFile, FloppyFile);
          
          NextInput;
          NextOutput;
          
          end { While not end of input list };
1:   RestoreSwitches;     

end { Compare };




Procedure DeleteFile;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Delete a file from the floppy. This entails destroying
{     the floppy directory's reference to the file; the file
{     itself is not destroyed until it is overwritten.
{
{------------------------------------------------------------------}

var FileName: String;
    WildName, SaveConfirm: boolean;
    
label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   DELETE FloppyFile(s) - destroy specified file(s). ');
          Writeln ('   Valid switches are CONFIRM, NOCONFIRM, and HELP.');
          goto 1;
     end { WriteHelp };

begin
     Caller := 'DELETE';
     InQuery := 'Which floppy file(s) should be deleted? ';
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     If InFiles^.Name = '' then 
          begin
          if not GetInputs (InQuery) then goto 1;
          DoSwitches (Caller);
          If IllegalSwitch then goto 1;
          If JustHelp then Writehelp;
          end;
     InCurrent := InFiles;
     SaveConfirm := Confirm;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     While InCurrent <> NIL do
          begin
          FileName:= InCurrent^.Name;
          if not RemoveQuotes(FileName) then
             begin
             StdError(ErBadQuote, '', false);
             goto 1;
             end;
          WildName := IsPattern(FileName);
          if WildName then
             if AskWild or Confirm then
                {make sure Confirm is true}
                Confirm := true
             else
                {Confirm stays false}
          else {not a wildname}
             if AskSingle or (Confirm and ConfirmNonWildDelete) then
                Confirm := true
             else
                Confirm := false;
          DoDelete (FileName,true);
          NextInput;
          Confirm := SaveConfirm;
                    
     end { While not end of input loop };
1:   RestoreSwitches;

end { DeleteFile};




Procedure Directory;
{------------------------------------------------------------------
{
{ Abstract:
{ 
{     Display a list of the files on the floppy either to
{     console or to a user-specified output file.
{
{------------------------------------------------------------------}

var InName, OutName: String;
    FileOpen: Boolean;

label 1;


     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   DIRECTORY [FloppyFile(s)] [~ DiskFile] - list the floppy files');
          WriteLn('    and put the output in the specified disk file.');
          Writeln ('   Valid switches are CONFIRM, NOCONFIRM, SHORT, LONG, and HELP.');
          Writeln ('   LONG is the default; the directory contains all information.');
          Writeln ('   SHORT lists only the names of the files.');
          Writeln ('   If a disk file name is given, output will be directed to that file.');

          RestoreSwitches;
          if fileopen then Close (OutFile);     
          exit(Directory);
(*
          goto 1;
*)
     end { WriteHelp };

begin { Directory }
     LongDir := true; {the default}
     FileOpen := False;
     Caller := 'DIRECTORY';
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     
     { IF ANY ARGUMENTS ARE GIVEN, PROCESS THEM HERE: }
     
     If InFiles <> NIL then InCurrent := InFiles 
     Else InName := '';
     If OutFiles <> NIL then 
          begin
          OutCurrent := OutFiles;
          If Size(OutFiles) > 1 then
               begin
               StdError(ErOneOutput, 'Directory', false);
               goto 1;
               end;
          end
      Else OutName := '';     
     
     { See whether this directory should be of all files
     { or only of some.                                }
     
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     While InCurrent <> NIL do
          begin
          inName := InCurrent^.Name;
          If OutCurrent <> NIL then 
               begin
               OutName := OutCurrent^.Name;
               if not RemoveQuotes(OutName) then
                  begin
                  StdError(ErBadQuote, '', false);
                  goto 1;
                  end;
               OutCurrent := NIL; { only one output file allowed. } 
               end; { IF output file is specified loop }
          if not RemoveQuotes(InName) then
                  begin
                  StdError(ErBadQuote, '', false);
                  goto 1;
                  end;
          DoDirectory (InName, OutName, FileOpen);
          NextInput;
          end; { WHILE InCurrent <> NIL loop }

1:   RestoreSwitches;
     if fileopen then Close (OutFile);     

end { Directory };



Procedure Rename;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Rename a floppy file to a use-specified name.
{
{------------------------------------------------------------------}

var OldName, NewName: String;

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   RENAME FloppyFile(s) FloppyFile(s) - give floppy file(s) new name(s).');
          Writeln ('   Valid switches are ASK, NOASK, CONFIRM, NOCONFIRM, and HELP.');
          goto 1;
     end { WriteHelp };

begin
     Caller := 'RENAME';
     InQuery := 'Which floppy file(s) should be renamed? ';
     OutQuery := 'What should the new name(s) be? ';
     
     If CheckInputsAndOutputs(WriteHelp) then
       While InCurrent <> NIL do
          begin
          OldName := InCurrent^.Name;
          NewName := OutCurrent^.Name;
          if not CheckNames(oldName, newName) then goto 1;
          DoRename (OldName, NewName);
          NextInput;
          NextOutput;
          end { While not end of input list };     
1:   RestoreSwitches;     
end { Rename };



Procedure TypeFile;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Type a floppy file to console.
{
{------------------------------------------------------------------}

var FloppyFile: String;

label 1;


     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   TYPE FloppyFile(s) - display file(s) contents on console.');
          Writeln ('   Valid switches are WAIT, NOWAIT and HELP.');
          Writeln ('   NOWAIT types the file without pausing at each FormFeed.');
          goto 1;
     end { WriteHelp };

begin
     
     Wait := true; {the default}
     Caller := 'TYPE';
     InQuery := 'Which file(s) should be typed? ';
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     If InFiles^.Name = '' then 
          begin
          if not GetInputs (InQuery) then goto 1;
          DoSwitches (Caller);
          If IllegalSwitch then goto 1;
          If JustHelp then WriteHelp;
          end;
     If OutFiles^.Name <> '' then
          begin
          StdError(ErNoOutfile, 'Type', false);
          goto 1;
          end;
     InCurrent := InFiles;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     While InCurrent <> NIL do
          begin
          FloppyFile := InCurrent^.Name;
          if not RemoveQuotes(FloppyFile) then
             begin
             StdError(ErBadQuote, '', false);
             goto 1;
             end;
          DoType (FloppyFile);
          NextInput;
          end { While not end of input loop };
1:   RestoreSwitches;     

end { TypeFile };



Procedure Help;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Display online documentation about all of the
{     commands available in the Floppy program.
{
{------------------------------------------------------------------}

begin

   writeln('   This is a list of all the floppy commands.  To see the applicable');
   WriteLn('   switches for each, type the command followed by /HELP.');
   WriteLn;
   writeln('   FLOPPY Commands are:');
   writeln;
   writeln('     GET       - Copy a floppy file onto a disk file.');
   writeln('                 If there are no arguments you will be prompted for them.');
   writeln('     PUT       - Copy a disk file onto a floppy.');
   writeln('                 If there are no arguments you will be prompted for them.');
   writeln('     COMPARE   - Verify that disk and floppy files are identical.');
   writeln('     COMPRESS  - Coalesce free space on the floppy.');
   writeln('     DELETE    - Delete a file from the floppy.');
   writeln('     DIRECTORY - Print the directory of the floppy. If there is a');
   writeln('                 second arg, the output goes to that disk file.');
   writeln('     FAST      - Set to NOASK and NoConfirm.');
   writeln('     SAFE      - (The default) ASK only on wildcards.');
   writeln('                 CONFIRM, except for DELETE of non-wild names.');
   writeln('     RENAME    - Change the name of a floppy file in place.');
   writeln('     TYPE      - Print a file from the floppy on the screen.');
   writeln('     ZERO      - Write a new directory on the floppy.');
   writeln('     DUPLICATE - Copy the contents of one floppy onto another.');
   writeln('                 This creates a set of scratch files on the disk');
   writeln('                 which will be deleted after they have been copied');
   writeln('                 unless you specify that they remain with the NODELETE switch.');
   writeln('     FLOPPYGET - Copy the contents of a floppy onto the hard disk.');
   writeln('     FLOPPYPUT - Copy the disk files made by FLOPPYGET onto a floppy.');
   writeln('                 The scratch files from FLOPPYGET will not be deleted');
   writeln('                 unless you specify so with the DELETE switch.');
   writeln('     FORMAT    - Format (reinitialize) a floppy.');
   writeln('     DENSITY   - Find out whether the floppy is single or double density.');
   writeln('     PATH      - Path to a different harddisk directory. Takes');
   writeln('                 as its argument the name of the new path.');
   writeln('     @<file>   - Execute command file <file>.');
   writeln('     QUIT      - Exit FLOPPY.');
   writeln('     PAUSE     - Tell the program to wait until your next input.');
   writeln;
   writeln('   Commands that can also be used as switches follow.');
   writeln('   When used as commands they are global and when used as');
   writeln('   switches to a command they are local to that command.');
   writeln;
   writeln('     HELP      - When used as a command it prints this message;');
   writeln('                 As a switch, it provides information about the');
   writeln('                 command it accompanies.');
   writeln('     CONFIRM   - Require confirmation before deleting or overwriting a file.');
   writeln('     NOCONFIRM - Do not require confirmation before deleting or overwriting.');
   writeln('     ASK       - Require confirmation on each file name.');
   writeln('     NOASK     - Do not require confirmation on each filename.');
   writeln('     VERIFY    - Verify correctness of data transfers.');
   writeln('     NOVERIFY  - Do not verify correctness of transfers.');
   writeln;
   writeln('   Commands that are used only with filesystem floppies');
   writeln('   are listed below. A filesystem floppy is one that can');
   writeln('   be used exactly like a hard disk; a Perq will run from');
   writeln('   such a floppy just as it normally does from hard disk.');
   writeln('   These floppies cannot be used for backing up files like');
   writeln('   the RT-11 format floppies that are used for most Floppy');
   writeln('   commands.');
   writeln;
   writeln('     FSFLOPPY  - This is actually a switch. It permits you to');
   writeln('                 use the following commands:');
   writeln('     MOUNT     - Mount the floppy and run from it.');
   writeln('     DISMOUNT  - Dismount the floppy and run from hard disk.');
   
end { Help };


Function NoArgCheck(Procedure WriteHelp): boolean;
{------------------------------------------------------------------
{ Abstract: Checks switches and arguments.
{ Returns: True if OK; false if error.
{------------------------------------------------------------------}
  begin
  NoArgCheck := false;
  DoSwitches (Caller);
  If IllegalSwitch then exit(NoArgCheck);
  If JustHelp then WriteHelp;
     
  { If the user has typed an argument, print error message and exit. }
     
  If (InFiles^.Name <> '') or (OutFiles^.Name <> '') then
        Writeln('** Command ',caller,' does not have any inputs or outputs.')
  else NoArgCheck := true;
  end;


Procedure ShowDensity;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Tell the user whether the floppy is single or
{     double density.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   DENSITY - tells whether the density is single or double.');
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'DENSITY';
     if not NoArgCheck(WriteHelp) then goto 1;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     If Dens = SDens then Writeln('This is a single density floppy.')
     Else if Dens = DDens then writeln('This is a double density floppy.');
1:   RestoreSwitches;     
end { ShowDensity };


Procedure DoConfirm;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set global switch CONFIRM to require confirmation before
{     overwriting files.
{
{------------------------------------------------------------------}

label 1;


     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   CONFIRM -  Require confirmation before overwriting or deleting files.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'CONFIRM';
     if not NoArgCheck(WriteHelp) then goto 1;
     HoldConfirm := true;
     HHoldConfirmNonWildDelete := true; {don't put in NoConfirm proc}
1:   RestoreSwitches;     
end { DoConfirm };


Procedure NoConfirm;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set global switch NOCONFIRM to supress confirmation before
{     overwriting files.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   NOCONFIRM -   Do not require confirmation before overwriting or deleting files.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'NOCONFIRM';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldConfirm := false;
1:   RestoreSwitches;     
end { NoConfirm };



Procedure Safe;
{------------------------------------------------------------------
{
{ Abstract:
{
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   SAFE - Require confirmation before overwriting or deleting files.');
          Writeln ('   Ask for confirmation on wildcards.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'SAFE';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldConfirm := True;
     HoldAskSingle := false;
     HoldAskWild := true;
     HHoldConfirmNonWildDelete := false; {not in Fast}
1:   RestoreSwitches;     
end { Safe };




Procedure Fast;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set the NOCONFIRM switch globally for the user.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   FAST - Do not require confirmation on any files, even');
          Writeln ('   if overwriting or deleting.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'FAST';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldConfirm := False;
     HoldAskSingle := false;
     HoldAskWild := false;
1:   RestoreSwitches;     
end { Fast };



Procedure DoAsk;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set the ASK switch globally for the user.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   ASK - Require confirmation on each name.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'ASK';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldAskSingle := true;
     HoldAskWild := true;
1:   RestoreSwitches;     
end { DoAsk };



Procedure NoAsk;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set the NOASK switch globally for the user.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   NOASK - Do not require confirmation on each name.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'NOASK';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldAskSingle := false;
     HoldAskWild := false;
1:   RestoreSwitches;     
end { NoAsk };



Procedure DoVerify;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set the VERIFY switch globally for the user.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   VERIFY - Check correctness of data transfered.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };
    
begin
     Caller := 'VERIFY';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldVerify := True;
1:   RestoreSwitches;     
end { Verify };



Procedure NoVerify;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set the NOVERIFY switch globally for the user.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   NOVERIFY - Do not verify correctness of transfers.');
          SetTrueGlobally;
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'NOVERIFY';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     HoldVerify := False;
1:   RestoreSwitches;     
end { NoVerify };



Procedure FSFloppy;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Set the FSFLOP switch globally. This enables use of File
{     System floppy commands.
{
{------------------------------------------------------------------}

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   FSFLOPPY - Allows you to use FileSystem floppy commands Mount and Dismount.');
          SetTrueGlobally;
          HelpOnlySwitch;
          exit(FSFloppy);
     end { WriteHelp };

begin
     Caller := 'FSFLOPPY';
     if not NoArgCheck(WriteHelp) then exit(FSFloppy);
     FSFlop := true;
end { FSFloppy };


Procedure Mount;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Mount the floppy (so that the user can run from it).
{
{-----------------------------------------------------------------}
label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
         Writeln ('   MOUNT - Mount the floppy and run from it.');
         Writeln ('   To be used ONLY for Filesystem floppies.');
         HelpOnlySwitch;
         goto 1;
     end { WriteHelp };

begin
     Caller := 'MOUNT';
     if not NoArgCheck(WriteHelp) then goto 1;
     If not FSFlop then
     begin
       Writeln ('** MOUNT must be enabled using the /FSFloppy switch.');
       WriteLn ('** MOUNT should only be used with file system floppies.');
       Goto 1;
     end;
     FSMount (1);
1:   RestoreSwitches;
end { Mount };


Procedure Dismount;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Dismount the floppy (so that the user can run from the
{     hard disk).
{
{-----------------------------------------------------------------}
label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
         Writeln ('   DISMOUNT - Dismount the floppy and run from hard disk.');
         Writeln ('   To be used ONLY for Filesystem floppies.');
         HelpOnlySwitch;
         goto 1;
     end { WriteHelp };

begin
     Caller := 'DISMOUNT';
     if not NoArgCheck(WriteHelp) then goto 1;
     If not FSFlop then
     begin
       Writeln ('** DISMOUNT must be enabled using the /FSFloppy switch.');
       WriteLn ('** DISMOUNT should only be used with file system floppies.');
       Goto 1;
     end;
     FSDismount (1);
1:   RestoreSwitches;
end { Dismount };


Procedure Pause;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Pause until the user wants to continue.
{
{-----------------------------------------------------------------}
label 1;
var Resume: String;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   PAUSE - Wait for the user to type a carriage return. Useful in command files.');
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'PAUSE';
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     Write ('Type carriage return to continue.');
     Readln(Resume);
1:   RestoreSwitches;
end { Pause };



Procedure Path;
{------------------------------------------------------------------
{
{  Abstract: 
{
{     Allow the user to specify a new path on the harddisk.
{
{-------------------------------------------------------------------}
label 1;
var I: integer;
    NewPath, OldPath: PathName;
    FID: FileID;
   
     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   PATH [PathName] - Path to a new directory on the harddisk.');
          Writeln ('   Its argument is the name of the new path.');
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     NewPath := '';
     ChangedPath := False;
     Caller := 'PATH';
     DoSwitches (Caller);
     If JustHelp then WriteHelp;
     if IllegalSwitch then Goto 1;
     If InFiles^.Name = '' then
          While NewPath = '' do
          begin
               Write ('What directory do you want to path to? ');
               Readln (NewPath);
          end
     Else NewPath := InFiles^.Name;
     FSRemoveDots(NewPath);
     If NewPath [Length(NewPath)] <> '>' then AppendChar(NewPath, '>');
     FID := FSLookUp(NewPath, I, I);
     If FID <> 0 then
          begin
          OldPath := FSDirPrefix;
          FSDirPrefix := NewPath;
          FID := FSLookUp('Floppy.Run', I, I);
          If FID = 0 then
             repeat
               FSDirPrefix := OldPath;
               WriteLn('* WARNING!  Floppy cannot be found using path ',
                       NewPath,  '!!');
               I := GetConfirm(NullIdleProc, true,
                    '* Are you sure you want to change to this path? ',
                    2, switches);
             until I < 3
          Else I := 1;
          If I = 1 then
                begin
                FSDirPrefix := NewPath;
                WriteLn('New path = ',FSDirPrefix);
                ChangedPath := True;
                end;
          end {fid <> 0}
     else StdError(ErDirNotFound, NewPath, false);
1:   RestoreSwitches;
End { Path };


{$IFC not BootFloppyVers THEN}  { These routines aren't used with
                               { filesystem floppies, so we can save
                               { some space with a conditional
                               { compilation here.             }


Procedure Put;
{------------------------------------------------------------------
{
{ Abstract:
{ 
{     Copy a disk file onto the floppy.
{
{------------------------------------------------------------------}

var Source, Destination: PathName;

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   PUT DiskFile(s) FloppyFile(s) - copy disk file(s) onto floppy.');
          Writeln ('   Valid switches are ASK, NOASK, CONFIRM, NOCONFIRM, VERIFY, NOVERIFY, and HELP.');
          goto 1;
     end { WriteHelp };

begin
     Caller := 'PUT';
     InQuery := 'What disk file(s) should be copied to floppy? ';     
     OutQuery := 'What floppy file name(s) should they have? ';
     if CheckInputsAndOutputs(WriteHelp) then
       While InCurrent <> NIL do
          begin
          Source := InCurrent^.Name;
          Destination := OutCurrent^.Name;
          if not CheckNames(source, destination) then goto 1;
          
          MakeFloppyName(Destination);

          { Now call the FloppyTransfers routine that does the work.   }
          
          PutFile (Source, Destination);
          NextInput;
          NextOutput;
          end { While not end of input list };
1:   RestoreSwitches;
end { Put };



Procedure Compress;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Move all of the unused blocks to the end of the
{     floppy.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   COMPRESS - Compact the floppy. (Warning - this takes a long time.)');
          Writeln ('   Valid switches are VERIFY, NOVERIFY, and HELP.');
          goto 1;
     end { WriteHelp };

begin
     Caller := 'COMPRESS';
     if not NoArgCheck(WriteHelp) then goto 1;
     writeLn(chr(7),'* Compressing the floppy takes a very long time.');
     if GetConfirm(NullIdleProc,True,
       '  Are you sure that you want to do this? ', 3, Switches) = 2 then
           goto 1;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     Squish;
     
1:   RestoreSwitches;
end { Compress };



Procedure Zero;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Create a new directory on a floppy. If there are
{     any files on the floppy, references to them will
{     be destroyed and the space that they occupy can
{     be overwritten.
{
{------------------------------------------------------------------}

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   Zero - writes new directory on floppy. Destroys old contents.');
          Writeln ('   Valid switches are NOCONFIRM, SINGLESIDED, DBLSIDED, and HELP.');
          Writeln ('   SINGLESIDED creates a directory for a single-sided'); 
          Writeln ('   floppy; the default is double-sided.');
          goto 1;
     end { WriteHelp };

begin
     ZFNoConfirm := false;
     Sides := 2;
     Caller :='ZERO';
     if not NoArgCheck(WriteHelp) then goto 1;
     Confirm := not ZFNoConfirm; {kludge to handle local noconfirmaation only}
     If Confirm  then
       begin
       if GetConfirm( NullIdleProc, True, 'Zero the floppy? ', 3, Switches) = 2
          then goto 1;
       Writeln ('* Zeroing the floppy will destroy its contents.');
       If GetConfirm(NullIdleProc, True,
              '  Is this what you want to do? ', 3, Switches) = 2 then goto 1;
       end;
     
     If (Sides <> 1) and Confirm then
         If GetConfirm (NullIdleProc, True, 
           'The default number of sides is 2. Is this what you want? ', 
           1, Switches) = 2 then goto 1;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     
     ZeroFloppy;
     If not ExitRoutine then
         Writeln ('Created new directory for ', Sides:1, '-sided floppy.');
1:   RestoreSwitches;
end { Zero };



Function GetDiskName(var name: PathName): boolean;
{------------------------------------------------------------------
{ Abstract: Gets the disk name from the first input for Duplicate, etc.
{ Returns: true if OK; false if error.
{------------------------------------------------------------------}
     begin
     GetDiskName := false;
     If OutCurrent^.Name <> '' then 
          begin
          If Size(OutCurrent) > 1 then
               begin
               Writeln ('** Only one disk file name is allowed.');
               exit(GetDiskName);
               end;
          Name := OutCurrent^.Name;
          if not RemoveQuotes(Name) then
            begin
            StdError(ErBadQuote, '', false);
            exit(GetDiskName);
            end;
          OutCurrent := NIL; { only one file name is permitted. }
          end { IF of OutCurrent <> NIL loop }
     Else Name := 'Floppy.Scratch';
     Name := Concat (Name, '. ');
     GetDiskName := true;
     end;


Procedure CopyFloppy;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Copy all of the contents of one floppy onto another.
{
{------------------------------------------------------------------}

var DiskName: PathName;

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   DUPLICATE [~DiskFileName] - duplicate one floppy onto another.');
          Writeln ('   Valid switches are VERIFY, NOVERIFY, CONFIRM,');
          Writeln ('   NOCONFIRM, DELETE, NODELETE, SINGLESIDED, DBLSIDED, and HELP.');
          Writeln ('   If a disk file name is specified, the contents of the');
          Writeln ('   floppy will be dumped into a set of files with that'); 
          Writeln ('   name. The default is Floppy.Scratch.A --> n.'); 
          Writeln ('   If NODELETE is specified, the scratch files will'); 
          Writeln ('   not be deleted after copying and can be reused by FLOPPYPUT later.');
          goto 1;
     end { WriteHelp };

begin
     
     Caller := 'DUPLICATE';
     DeleteScratch := True;
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     OutCurrent := OutFiles;
     
     if (infiles^.name <> '') or (infiles^.next <> NIL) then 
        begin
        WriteLn('** Duplicate has no inputs and one optional output.');
        goto 1;
        end;
     if not GetDiskName(diskName) then goto 1;
     
     InitDup;
     Write('Insert Master Floppy to be copied and then type carriage return:');
     readln;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     
     Write('The floppy is ',sides:1,'-sided and ');
     if Dens = SDens then Write('Single')
     else write('Double');
     Write(' density.');
     If Confirm then
        if GetConfirm(NullIdleProc, True, '  Is this what you want? ',     
                1, Switches) <> 1 then goto 1
        else
     else writeln; 
     ReadMaster (DiskName);
     If ExitRoutine then 
       begin
       FinDup;
       Goto 1;
       end;
     BigBeep(6);
     if DupSuccess then
        begin
        Write('Insert blank formatted floppy and then type carriage return:');
        readln;
        WriteCopy (DiskName);
        BigBeep(6);
        if (DupSuccess) and (not exitRoutine) then FlTerminate (DiskName)
        else begin
             WriteLn;
             WriteLn('** Copy onto new floppy failed.  Not deleting scratch files!');
             end;
        end;
     FinDup;
     
1:   RestoreSwitches;
     
end { CopyFloppy };



Procedure DumpToDisk;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Copy all of the contents of a floppy onto the
{     hard disk.
{
{------------------------------------------------------------------}

var DiskName: PathName;

label 1;


     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   FLOPPYGET [~DiskFileName] - dump contents of floppy onto hard disk.');
          Writeln ('   Valid switches are VERIFY, NOVERIFY, CONFIRM,');
          WriteLn ('   NOCONFIRM, SINGLESIDED, DBLSIDED, and HELP.');
          Writeln ('   If a disk file name is specified, the contents of the');
          Writeln ('   floppy will be dumped into a set of files with that'); 
          Writeln ('   name. The default is Floppy.Scratch.A --> n.'); 
          goto 1;
     end { WriteHelp };

begin
     
     
     Caller := 'FLOPPYGET';
     DeleteScratch := False;
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     OutCurrent := OutFiles;
     if (infiles^.name <> '') or (infiles^.next <> NIL) then 
        begin
        WriteLn('** FloppyGet has no inputs and one optional output.');
        goto 1;
        end;
     if not GetDiskName(diskName) then goto 1;
     
     Write('Insert Master Floppy to be copied and then type carriage return:');
     readln;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     Write('The floppy is ',sides:1,'-sided and ');
     if Dens = SDens then Write('Single')
     else write('Double');
     Write(' density.');
     If Confirm then
        if GetConfirm(NullIdleProc, True, '  Is this what you want? ',     
                1, Switches) <> 1 then goto 1
        else
     else writeln; 
     InitDup;
     ReadMaster (DiskName);
     BigBeep(6);
     FinDup;

1:   RestoreSwitches;
     
end { DumpToDisk };



Procedure DumpToFloppy;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Copy all of the files that were created by a DumpToDisk
{     onto a floppy.
{
{------------------------------------------------------------------}

var DiskName: String;
    FID, Blocks, Bits: Integer;
label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   FLOPPYPUT [DiskFileName] - dump files DiskFileName onto floppy.');
          Writeln ('   Valid switches are VERIFY, NOVERIFY, CONFIRM,');
          WriteLn ('   NOCONFIRM, SINGLESIDED, DBLSIDED, DELETE, NODELETE, and HELP.');
          Writeln ('   If a disk file name is specified, the contents of a');
          Writeln ('   set of files with that name will be copied onto the'); 
          Writeln ('   floppy.  If DELETE is specified, the scratch files will'); 
          Writeln ('   be deleted after copying.');
          goto 1;
     end { WriteHelp };

begin
     
     Caller := 'FLOPPYPUT';
     DeleteScratch := False;
     DoSwitches (Caller);
     If IllegalSwitch then goto 1;
     If JustHelp then WriteHelp;
     OutCurrent := InFiles;
     if (outfiles^.name <> '') or (outfiles^.next <> NIL) then 
        begin
        WriteLn('** FloppyPut has one optional inputs and no outputs.');
        goto 1;
        end;
     if not GetDiskName(diskName) then goto 1;
     
     Write('Insert blank formatted floppy and then type carriage return:');
     readln;
     FindSidesAndDensity(true);
     If Unformatted then goto 1;
     Write('The floppy is ',sides:1,'-sided and ');
     if Dens = SDens then Write('Single')
     else write('Double');
     Write(' density.');
     If Confirm then
        if GetConfirm(NullIdleProc, True, '  Is this what you want? ',     
                1, Switches) <> 1 then goto 1
        else
     else writeln; 
     InitDup;
     WriteCopy (DiskName);
     if (deleteScratch) then
        if (DupSuccess) and (not exitRoutine) then FlTerminate(DiskName)
        else begin
             WriteLn;
             WriteLn('** Copy onto new floppy failed.  Not deleting scratch files!');
             end;
     FinDup;
     BigBeep(6);
1:   RestoreSwitches;
     
end { DumpToFloppy };



Procedure Format;
{------------------------------------------------------------------
{
{ Abstract:
{
{     Format a floppy, setting the sides and density.
{     Provide the option of testing the floppy during
{     formatting.
{
{------------------------------------------------------------------}

var DensString: String;

label 1;

     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   FORMAT - reinitialize (or initialize) a floppy.');
          Writeln ('   Valid switches are NOCONFIRM, DBLDENSITY,');
          Writeln ('   SINGLEDENSITY, SINGLESIDED, DBLSIDED, TEST, NOTEST and HELP.');
          Writeln ('   The default formatting is single density, double sided.'); 
          Writeln ('   DBLDENSITY formats double-density floppies.');
          Writeln ('   SINGLESIDED formats single-sided floppies.');
          Writeln ('   TEST verifies formatting with a CRC.'); 
          goto 1;
     end { WriteHelp };

begin
     
     Caller := 'FORMAT';

     { Set defaults }
    
     ZFNoConfirm := false;
     FmtTest := false;
     if not NoArgCheck(WriteHelp) then goto 1;
     DoSwitches(Caller); {First time is just to find out if switches ok
                         or HELP so don't get hung on FindSides...}
     
     Sides := 2;
     Dens := SDens; {our default}
     Verify := False;

     DoSwitches (Caller);{ Change density and sides and test if user wants to }
     Confirm := not ZFNoConfirm; {same kludge as in ZERO}
     { If the user has typed an argument, print error message and exit. }
     
     If Confirm then
          begin
          If GetConfirm ( NullIdleProc, True, 
                   'Format the floppy? ', 3, Switches) <> 1
                         then goto 1;
          Writeln ('* Formatting the floppy will destroy its current contents.');
          If GetConfirm( NullIdleProc, True, 
                   '* Are you sure you want to do this? ', 3, Switches) <> 1
                         then goto 1;
          end;
     
     If Dens = SDens then DensString := 'single' else DensString := 'double';
     If (Sides = 2) and (Dens = SDens) and Confirm then
          If GetConfirm 
               ( NullIdleProc, True, 
                 'The default setting is Double Sided, Single Density. Is this what you want? ',     
                1, Switches) <> 1 then goto 1; 

     writeln ('   Formatting Sides = ', Sides:1,' , ', DensString:1, ' density.');
     FormatFloppy;
1:   RestoreSwitches;
     Unformatted := False;
end { Format };


{$ENDC}     


Procedure Quit;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Destroy lists created during execution and exit program.
{
{-----------------------------------------------------------------}

label 1;


     Procedure WriteHelp;
     {------------------------------------------------------------
     {
     { Abstract:
     {
     {     Write a help message about this particular command.
     {
     {------------------------------------------------------------}
     begin
          Writeln ('   QUIT - Exit program Floppy.');
          HelpOnlySwitch;
          goto 1;
     end { WriteHelp };

begin
     Caller := 'QUIT';
     if not NoArgCheck(WriteHelp) then goto 1;
     
     DestroyNameDesc (CmdTable);
     DestroyNameDesc (SwitchTable);
     Finished := True;
     Exit (Quit);
     
1:   RestoreSwitches;
end {Quit}.




