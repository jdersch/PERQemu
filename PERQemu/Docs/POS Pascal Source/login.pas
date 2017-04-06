{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program LogIn;

{---------------------------------------------------------------------------
{
{ Abstract:
{    This is the login program.  It is called at both boot time and
{    anytime a LogIn command is executed.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corp.  1981, 1983.
{
{----------------------------------------------------------------------------}


{$Version 3.10 }
{----------------------------------------------------------------------------
{ Change Log:
{
{  1-Sep-83  V3.10  Scott Brown
{ Handle exception raised due to a bad clock on the EIO board.
{
{ 24-Mar-83  V3.9   Don Scelza
{ Set DefRealRelTablet to false at startup.
{
{  2-Mar-83  V3.8   Don Scelza
{ Set DefRealRelTablet back to IsLandScape.
{ Fixed a number of small bugs.
{
{  2-Mar-83  V3.7  WJHansen
{ Fix so principal.user is only checked at boot time.
{ Set DefRealRelTablet to True.
{
{ 15-Feb-83  V3.6 Joseph Ginder, WJHansen
{ Changed Checklogin to check file "principal.user" when no login name 
{ is specified on the command line.  Also, no password is asked for
{ when none is required.  Re-wrote CheckLogin, CheckUser and added
{ LoginReset and GetPrincipalUser.  
{
{ 10-Feb-83  V3.5  Brad Myers
{ Fixed for landscape monitor: Base default for realrel on screen type.
{
{  3-Feb-83  V3.4  Sandeep Johar
{ TabletType is the real spelling of the switch, not Tabletype.
{
{ 27-Jan-83  V3.3  Roger Riggs
{ Removed "The" from Tablet types
{
{ 18-jan-83  V3.2  Sandeep Johar
{ Fix the arguement to the tablet switch to be really tablet or bitpad.
{
{ 08-Dec-82  V3.1  CDBeckett for August G. Reinig
{ Modified Dirk's changes slightly to fit with the K1 machines
{
{ 06-Oct-82  V3.0  Don Scelza
{ Added code to get time from the net.
{
{ 15-Sep-82  V2.3  Dirk Kalp
{ Make HandleSwitches accurately check the arguments supplied to switches
{ PointAllowed, RealRelative, and TabletType.
{
{ 16-Aug-82  V2.2  Dirk Kalp
{ Added new parameters RealRelative and TabletType.
{
{  8-May-82  V2.1  WJHansen
{ Set up so errors accessing System.Users will log user in as Guest.
{ Fix so it only asks for time once.
{ Provide error message in case no Shell is found.
{ Remove a few variables from the set of global declarations.
{
{ 21-Jan-82  V2.0  Brad Myers
{ Revamped switch and profile handling.
{ Added new parameter: PointAllowed.
{ Better help.
{ Prints system version number in title line.
{
{ 11-Jan-82  V1.17  Brad Myers
{ Fixed shell name to not be full path.
{
{  6-Jan-82  V1.16  Brad Myers
{ SCurOn for Password.
{ Check path from profile.
{
{  7-Dec-81  V1.15  WJHansen
{ allow switches when give name after prompt
{ Initialize TimeFID
{ for /Command, use  UseCmd and HoldCmd
{ install ShellOne and ShellTwo
{ install HoldPath, HoldCmd, HoldPFile
{
{ 30-Nov-81  V1.15  WJHansen
{ Don't display password
{ Default name is Guest
{ Default path is Sys:User>Guest>
{ Setup to use new general command file scheme
{ Accept /HELP
{ Accept /Commands=file   and   /Profile=file
{ Accept Login/Help and give explanation
{ Don't set Shell to LogIn when name or password is invalid; allow continue
{ Accept hours and minutes only for time.  (use old day)
{ If ResetError on >System.Users, login as 'no name'.
{ Don't use cmdfile from profile is one is given by /CommandFile.
{ 
{ 10-Aug-81  V1.14  Brad Myers
{ Added default time read from file
{ Set default path if not in profile
{ Deallocate old shell info
{
{ 30-Jun-81  V1.13  Brad Myers
{ Added new profile option for Default screen on and comp when shrink
{ Added default time read from file
{
{ 19-May-81  V1.12  Brad Myers
{ Added new profile option for cursor function (screen color)
{ Changed to use exceptions that come from FileUtils and Clock
{ Removed call to ChangeWindow(0) at top
{
{ 11-May-81  V1.11  Don Scelza
{ Added code to set shell name.
{ And ability to give a command file.
{
{ 11-May-81  V1.10  Don Scelza
{ Added a call to FixFileName in the profile code.
{ 
{ 8-May-81   V1.9  Don Scelza
{ First released version of the new LogIn
{
{ 29-Apr-81  V1.7  Don Scelza
{ Added code to set the name of the profile file.
{ 
{ 19-Mar-81  V1.4  Brad Myers
{ PERQ.String to PERQ_String.
{ 
{  5-Mar-81  V1.3  Don Scelza
{ Added calls to the UserPass module.
{
{  3-Mar-81  V1.2  Don Scelza
{ Added code to show boot character.  Added coe to ask for user
{ name and set ID fields.
{
{ 19-Feb-81  V1.1  John Strait
{ Incorporate system version number into the name of the Shell run file.
{ Make a constant for LogIn version number.
{
{ 16-Feb-81  V1.0  Don Scelza
{ Created LogIn program.
{----------------------------------------------------------------------------}


imports System      from System;
imports Screen      from Screen;
imports Clock       from Clock;
imports Perq_String from Perq_String;
imports Memory      from Memory;
imports UserPass    from UserPass;
imports CmdParse    from CmdParse;
imports Profile     from Profile;
imports FileSystem  from FileSystem;
imports FileUtils   from FileUtils;
imports IO_Unit     from IO_Unit;
imports IO_Others   from IO_Others;
imports IOErrors    from IOErrors;
imports Stream      from Stream;
imports ShellDefs   from ShellDefs;
imports AllocDisk   from AllocDisk;
imports EtherTime   from EtherTime;
imports Ether10IO   from Ether10IO;
imports Configuration from Configuration;
imports Except      from Except;

const LogInVersion = '3.10';

label 1;

var
    CmdTable, ScrBotTable: CmdArray;
    HoldPFile, HoldShell, HoldPath, HoldCmd: String;
    ShellOne, ShellTwo: String;
    switches: pSwitchRec;
    Words, OutWords: pArgRec;
    haveTime : boolean;

const Debug = false;
      
const PathIndex = 1;
      SetIndex = 2;
      ShellIndex = 3;
      CmdIndex = 4;
      CursIndex = 5;
      ScrIndex = 6;
      HelpIndex = 7;
      PointIndex = 8;
      ProfIndex = 9;
      RealRelIndex = 10;
      TabletIndex = 11;
      NumCmds = 11;

      OnIndex = 1;
      OffIndex = 2;
      BlackIndex = 3;
      WhiteIndex = 4;
      ScrBotNumCmds = 4;

procedure TrySetShell(NewName: string);
{-------------------------------------------------------
{ Abstract:
{    If HoldShell, the next shell, has not been set,
{    this procedure checks that NewName is a valid file and
{    sets HoldShell if it is.
{ Parameter:
{    NewName - Possible name for shell.
{------------------------------------------------------}
var 
    FID: FileID;
    dum: integer;
    TempName: PathName;
begin
if HoldShell = '' then begin
    tempName := NewName;
    FID := FSExtSearch(FSSysSearchList, ' .Run ', tempName, Dum, Dum);
    if FID <> 0 then
        HoldShell := NewName
    else
        writeln('** Could not find ', NewName, ' to be Shell.');
    end;
end;


Procedure AppendSwitches(var s1, s2: pSwitchRec);
{-----------------------------------------------------------
{
{ Abstract:
{    Append the switchRec s2 on the end of s1.  OK if either or both are NIL.
{
{-----------------------------------------------------------}
  var tempS: pSwitchRec;
  begin
  if s1 = NIL then s1 := s2
  else begin
       tempS := s1;
       while tempS^.next <> NIL do
         tempS := tempS^.next;
       tempS^.next := s2;
       end;
  end;

Procedure DoHelp;
  var i : integer;
  begin
  WriteLn;
  WriteLn('    Login allows a person to use the system.  Type your name and');
  WriteLn('    password.  Use the UserControl program to enter a new user.');
  WriteLn('    The null name (type a return) will usually login as "Guest".');
  WriteLn('    The switches to login, which can also appear in the user''s');
  WriteLn('    profile, are: ');
  for i := 1 to NumCmds do
    WriteLn('      ',CmdTable[i]);
  WriteLn;
  Exit(Login);
  end;


Procedure CheckSwitchesForHelpProfileAndShell(var switches: pSwitchRec;
             fromProfile: boolean);
{-----------------------------------------------------------
{
{ Abstract:
{    Go through the switch list.  If a help switch is found and not from
{    profile then do help and exit.  Remove the help switch.  If a
{    profile switch is found, if fromProfile then set CurPFile to the argument,
{    else set HoldPFile to that value.  Remove the profile switch.  If Shell
{    switch then if fromProfile then set ShellTwo else set ShellOne. Remove
{    the shell command.
{
{ Parameters:  Switches - the switch list to modify.  May change the value
{                         of switches if first item on list is profile or help.
{                         Changes all switches to uppercase.
{
{-----------------------------------------------------------}
   var tempS: pSwitchRec;
       lastS: pSwitchRec;
       removeIT: boolean;
   begin
   lastS := NIL;
   tempS := switches;
   while tempS <> NIL do
     begin
     ConvUpper(tempS^.switch);
     case UniqueCmdIndex(tempS^.switch, cmdTable, NumCmds) of
        HelpIndex : begin
                    if not fromProfile then DoHelp;
                    removeIt := True;
                    end;
        ProfIndex : begin
                    if fromProfile then CurPFile := tempS^.arg
                    else HoldPFile := tempS^.arg;
                    removeIt := true;
                    end;
        ShellIndex: begin
                    if fromProfile then ShellTwo := tempS^.arg
                    else ShellOne := tempS^.arg;
                    removeIt := true;
                    end;
        otherwise: removeIt := false;
        end;
     if removeIt then
        if lastS = NIL then switches := tempS^.next
        else lastS^.next := tempS^.next
     else lastS := tempS^.next;
     tempS := tempS^.next;
     end; {while}
   end;  {CheckSwitchesForHelpProfileAndShell}


procedure DoProfile;
{-------------------------------------------------------
{
{ Abstract:
{    Read the profile file.  Checks for errors.  If none, then
{    puts any switches found at the front of the global Switch list.
{
{------------------------------------------------------}
  label 3;
  var PLine: CString;
      sw, swlist : pSwitchRec;
      ins, outs: pArgRec;
      err: String;
      
  handler PNotFound(FName: string);
    begin
    writeln('** User profile file ', FName, ' not found.');
    if curPFile <> PFileConst then
       begin
       curPFile := PFileConst;
       goto 3;
       end
    else exit(DoProfile);
    end;

  begin
  swList := NIL;

3: PFileInit(CurPFile, 'Login');
   WriteLn('Reading profile file ',CurPFile);
   PLine := PFileEntry;
   while PLine <> '' do
     begin
     if not ParseStringArgs(PLine, ins, outs, sw, err) then
        begin
        write(err,' in profile.');
        WriteLn('** Profile ignored!!');
        exit(DoProfile);
        end;
     if (ins^.name <> '') or (outs^.name <> '') or (ins^.next <> NIL) or
        (outs^.next <> NIL) then 
          begin
          WriteLn('** Profile entry "',PLine,'" is malformed. Profile ignored.');
          exit(DoProfile);
          end;
     CheckSwitchesForHelpProfileAndShell(sw, true);
     AppendSwitches(swList, sw);
     PLine := PFileEntry;
     end;
   AppendSwitches(swList, switches);
   switches := swList;
   end;


Procedure HandleSwitches;
{-------------------------------------------------------
{
{ Abstract:
{    Handles the switches except Shell, Help, and Profile.  (Should have 
{    called CheckSwitchesForHelpProfileAndShell first.
{
{------------------------------------------------------}
  Procedure DoPush(name: PathName);
     handler SrchWarn(fileName: PathName);
       begin
       WriteLn('** Cannot fill search list with ',filename);
       exit(DoPush);
       end;
     handler SrchErr(fileName: PathName);
       begin
       WriteLn('** Cannot fill search list with ',filename);
       exit(DoPush);
       end;
     begin
     FSPushSearchItem(name, FSSysSearchList);
     end;
  Procedure DoPop;
     handler SrchWarn(fileName: PathName);
       begin
       WriteLn('** Cannot pop last item of list');
       exit(DoPop);
       end;
     handler SrchErr(fileName: PathName);
       begin
       WriteLn('** Cannot pop last item of list');
       exit(DoPop);
       end;
     begin
     FSPopSearchItem(FSSysSearchList);
     end;

  var i: integer;
  begin
  while switches <> NIL do
     begin
     case UniqueCmdIndex(switches^.switch, CmdTable, NumCmds) of
     PathIndex: begin
                holdPath := switches^.arg;
               end;
     SetIndex: if switches^.arg <> '' then
                  if switches^.arg = '-' then DoPop
                  else begin
                       if switches^.arg[length(switches^.arg)] <> '>'
                          then AppendChar(switches^.arg, '>');
                       FixFileName(switches^.arg, false);
                       DoPush(switches^.arg);
                       end;
     CmdIndex: begin
               HoldCmd := switches^.arg;
               end;
     CursIndex: begin
                if Length(switches^.arg) <> 1 then
                  WriteLn('** CursorFunction takes an integer argument')
                else begin
                     i := Ord(switches^.arg[1])- Ord('0');
                     if (i < 0) or (i > 7) then
                       WriteLn('** CursorFunction argument must be between 0 and 7.')
                     else DefCursFunct := i;
                     end;
                end;
     ScrIndex: begin
               ConvUpper(switches^.arg);
               case UniqueCmdIndex(switches^.arg, ScrBotTable,
                       ScrBotNumCmds) of
                         OnIndex : DefScrOff := false;
                         OffIndex: DefScrOff := true;
                         BlackIndex: DefScrComp := true;
                         WhiteIndex: DefScrComp := false;
                         ScrBotNumCmds+1: StdError(ErSwParam, switches^.switch, false);
                         Otherwise: WriteLn('** Parameter ', switches^.arg, ' is not unique.');
                         end;
               end;
     PointIndex: begin
                 ConvUpper(switches^.arg);
                 if switches^.arg <> '' then
                     begin
                     if switches^.arg[1]='F' then PointAllowed := false
                     else PointAllowed := true;
                     end;
                 end;
     RealRelIndex: begin
                     ConvUpper(switches^.arg);
                     if switches^.arg = ''
                     then writeln( '** RealRel requires an arg.' )
                     else DefRealRelTablet := switches^.arg[1] = 'T'
                   end;
     TabletIndex: begin
                    ConvUpper( switches^.arg );
                    if switches^.arg = ''
                    then writeln( '** TabletType requires an arg.' )

                    else if switches^.arg[1] = 'T'
                    then if KrizTabConnected
                         then DefTabletType := ord(KrizTablet)
                         else writeln('** Your tablet is not connected.' )

                    else if switches^.arg[1] = 'B'
                    then if GPIBPadConnected
                         then DefTabletType := ord(GPIBBitPad)
                         else writeln('** Your BitPad is not connected.')

                    else writeln('** TabletType argument must be TABLET or BITPAD.')
                  end;
     NumCmds+1: StdError(ErBadSwitch,switches^.switch, false);
     NumCmds+2: StdError(ErSwNotUnique, switches^.switch, false);
     end {case};

    switches := switches^.next;
    end;
end;  {HandleSwitches}



procedure ReadPassWord(Name: String; var PassWord: String);
{-----------------------------------------------------------
{ Abstract:
{    Reads password without displaying it.
{ Parameter:
{    PassWord - Set to the string read in.
{ Design:
{    Processes backspace, oops, ^H, ^U, and RETURN.
{-----------------------------------------------------------}
    label 2;
    handler HELPkey (var Replace: Sys9s); begin
       writeln;
       WriteLn;
       writeln('   Enter the secret password for user ''', Name, '''');
       Write('Password: ');
       goto 2;
    end;
       
    const
       CtlU = chr( ord('U') - ord('A') + 1);
       CtlH = chr( ord('H') - ord('A') + 1);
       ChCR = chr( ord('M') - ord('A') + 1);
    var 
       reading: Boolean;
       Ch: char;
    begin
 2:  SCurOn;
     PassWord := '';
     reading := true;
     while reading do begin
          while IOCRead(TransKey, Ch) <> IOEIOC do {nothing};
          case Ch of 
             '''': begin
                     while IOCRead(TransKey, Ch) <> IOEIOC do {nothing};
                     AppendChar(PassWord, Ch);
                   end;
             CtlU: PassWord := '';
             CtlH: if length(PassWord)>=1 then 
                     Adjust(PassWord, length(PassWord)-1);
             ChCR: reading := false;
             otherwise:
                   AppendChar(PassWord,Ch);
          end;
       end;
    WriteLn;
    SCurOff;
    end;



procedure HandleLine(S: String);
{-----------------------------------------------------------
{
{ Abstract:
{    Processes the input S as a line.  Sets Words, OutWords, and Switches
{    variables.  Appends switches found at END of global switches.  If help,
{    then do help immediately.  Do not use this for switches in profile.
{ Calls: CheckSwitchesForHelpProfileAndShell.
{
{-----------------------------------------------------------}
var Err: CString;
   sw: pSwitchRec;
   begin
    if not ParseStringArgs(S, Words, OutWords, sw, Err) then
       begin
       writeln(Err);
       exit(Login);
       end
    else if (OutWords^.next <> NIL) or (Words^.next <> NIL) then
       begin
       writeln('** No '','' allowed');
       exit(LogIn);
       end
    else begin
         CheckSwitchesForHelpProfileAndShell(sw, false);
         AppendSwitches(switches, sw);
         end;
end; {HandleLine}

procedure LoginReset;
{-------------------------------------------------------
{ Abstract:
{    Reset Global vars and Switches..
{------------------------------------------------------}
begin
   HoldPFile := '';
   HoldPath  := '';
   HoldCmd   := '';
   HoldShell := '';
   ShellOne := '';
   ShellTwo := '';
   switches := NIL;
end;

procedure CheckLogIn;
{-----------------------------------------------------------
{
{ Abstract:
{    This procedure is used to see if the user is valid.
{    Check the name and password.
{
{ Side Effects:
{    This procedure will change the current user ID, 
{    current group ID and Current user name.
{
{-----------------------------------------------------------}
   var
       UsrRec: UserRecord;    {is set inside CheckUser & used outside}
       Name: String;          {User name.  Set in HelpKey handler.}
    
   label 2;
   handler HELPkey (var Replace: Sys9s); begin
       writeln; 
       WriteLn;
       writeln('   Type in your login name or press the return key to log in as "Guest".');
       WriteLn;
       Name := '';
       goto 2;
   end;
    
   function CheckUser(Name, PassWord: String): Boolean;
   {---------------------
   { Abstract:
   {   Checks name and password entered by user.
   {   Is a separate procedure so exceptions can be handled.
   {--------------------}
       handler ALL(a,b,c,d: integer); begin
          writeln;
          writeln('** There is some problem with System.Users.');
          writeln('** Use the NEWFILE option of UserControl.');
          CurUserID := 0; 
          CurGroupID := 0;
          CurPFile := PFileConst;
          CurUserName := 'Guest';
          exit(CheckLogin);
       end;
       handler CtlC; begin
          Name := '';
          goto 2;
       end;
       handler CtlCAbort; begin
          Name := '';
          goto 2;
       end;
       handler CtlShftC; begin
          Name := '';
          goto 2;
       end;
   begin {CheckUser}
      if not ValidUser(Name, Password, UsrRec) then 
          if (Name='Guest') and (not FindUser('Guest', UsrRec)) then begin
              writeln;
              writeln('** There is no entry for ''Guest'' in System.Users');
              writeln('** Use the ADD option of UserControl');
              CurUserID := 0; 
              CurGroupID := 0;
              CurPFile := PFileConst;
              CurUserName := 'Guest';
              exit (CheckLogin);
          end
          else CheckUser := false
      else CheckUser := true;
   end {CheckUser};
   
   procedure GetPrincipalUser(var Name: String);
   {---------------------
   { Abstract:
   {   Gets name from the principal.user file.
   {   Is a separate procedure so exceptions can be handled.
   {--------------------}
   handler All(a,b,c,d: integer);
   begin
      Name := '';
      goto 2;
   end;

   const pufname = '>Principal.User'; 
   var pu: text;

   begin  {GetPrincipalUser}
       {try to get name from pufname}
       reset(pu, pufname);
       readln(pu, Name);
       writeln('Name from ', pufname, ' is "', Name, '"');
       close(pu);
   end; {GetPrincipalUser}
 
   var
       PassWord: String;
       BootTime: boolean;
       isSwitch: Boolean;
       Word: CString;
       Break: char;

   begin  {CheckLogin}
     BootTime := (UsrCmdLine=''); 
     Break := NextId(Word, isSwitch); {remove cmd from line}
     HandleLine(UsrCmdLine);
     if Words^.name <> '' then
        Name := Words^.name
     else if BootTime then
        GetPrincipalUser(Name)
     else Name := '';
   
2:   if length(Name) = 0 then begin
        write('Please enter your name: ');
        readln(Name);
        HandleLine(Name);
        Name := Words^.Name;
     end;

     Password := '    ';
     if length(Name) = 0 then 
        Name := 'Guest'
     else if OutWords^.name <> '' then begin
            PassWord := OutWords^.name;
            Password := Concat(Password, '    '); 
     end;
     If not (CheckUser(Name, Password)) then begin
         write('Password: ');
         ReadPassWord(Name, PassWord);
         HandleLine(PassWord);
         PassWord := Words^.name;
         Password := Concat(Password, '    ');
         If not (CheckUser(Name, PassWord)) then begin
             writeln('** Invalid user or password.');
             LoginReset;
             Name := '';
             goto 2; {Yech!}
         end;
     end;
     CurUserName := UsrRec.Name;
     CurUserID := UsrRec.UserId;
     CurGroupID := UsrRec.GroupID;
     if HoldPFile <> '' then
         CurPFile := HoldPFile
     else if length(UsrRec.Profile) <> 0 then
         CurPFile := UsrRec.Profile;
    
   end {CheckLogin};
    
    

procedure ClearUserState;
{---------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to clear the system state that is
{    associated with a user.
{
{ Side Effects:
{    This procedure will clear some of the variables in system.
{
{--------------------------------------------------------------}
  Handler SrchWarn(name: PathName);
    begin
    Exit(ClearUserState);
    end;
  Handler SrchErr(name: PathName);
    begin
    Exit(ClearUserState);
    end;
  
  var s: PathName;
      Ctrl:  pCtrlRec;
      dev: integer;
            
  begin
  if ShellCtrl <> NIL then
      begin
      Ctrl := recast(ShellCtrl, pCtrlRec);
      DstryCmdFiles(Ctrl^.CmdFileList);
      ShellCtrl := NIL;
      DecRefCount(CmdSegment);
      CmdSegment := 0;
      end;

  DefCursFunct := ord(CTNormal);  {in case not specified}
  DefScrOff := False;
  DefScrComp := False;
  PointAllowed := True;
  PrintStatistics := False;

  DefRealRelTablet := SIsLandscape;     {default for realRel depends on type
                                             of screen }


  IOSetModeTablet( offtablet );         { turn off any tablet }
  IOChooseTablet( KrizTablet );         { choose the KrizTablet }
  IOSetModeTablet( tabAbsTablet );      { try turning it on }
  IOChooseTablet( GPIBBitPad );         { try turning on the BitPad }
  IOSetModeTablet( offtablet );         { turn off any tablet }

       if KrizTabConnected then DefTabletType := ord(KrizTablet)
  else if GPIBPadConnected then DefTabletType := ord(GPIBBitPad)
  else DefTabletType := ord(NoPointDev);


  LastFileName := '';

  dev := ord(not isFloppy);
  if DiskTable[dev].InUse then
    begin
    WriteLn('* Dismounting device ',dev:1);
    FSDismount(dev); {dismount other device than booted from}
    end;
    
  repeat
      FSPopSearchItem(FSSysSearchList);
  until false;
  
  {NOTE: exit is via FSPopSearchItem failure}

  end;
    

procedure GetTime;
{---------------------------------------------------------------
{
{ Abstract:
{    Gets the time from user and sets it.
{
{ Side Effects:
{    Sets the time.
{
{--------------------------------------------------------------}
var
    TimeIn: String;
    OldTime: string;
    Stamp, GMTStamp: TimeStamp;
    GMT: boolean;

var
    Str: string;
    Timebits: integer;
    blks: integer;
    buf: RECORD CASE boolean of
           true: (t: ^TimeStamp);
           false: (p: pDirBlk);
           END;

label 2, 3, 4;
Handler HELPkey (var Replace: Sys9s);
   begin
   WriteLn;
   WriteLn;
   WriteLn('      Type the date and time in military (24 hour) format.  Seconds are optional.');
   WriteLn('      Example:  21 Jan 82  15:32');
   WriteLn;
   goto 2;
   end;

Handler BadTime;
   begin
   goto 2;
   end;

Handler E10NoHardware;
    begin
    goto 3;
    end;

Handler GTSNoZ80;
    begin
    goto 4;
    end;
    
Handler GTSNotPERQ2;
    begin
    goto 4;
    end;



  procedure get_EIO_time;
  
    handler InxCase;
    begin
    writeln('** Bad clock on EIO board -- contact Field Service for repair.');
    goto 4;
    end;
    
  begin
              GMT := false;
              GetPERQ2Local(Stamp);
              GetPERQ2GMT(GMTStamp);
              if (Stamp.Hour) = (GMTStamp.Hour) then GMT := true;
              StampToString(Stamp, OldTime);
              write('EIO clock returned ', OldTime);
              if GMT then
                  writeln(' GMT')
              else
                  writeln;
              SetTString(OldTime);     {may raise BadTime and go back to 2}
              haveTime := true;
              exit(getTime);
  end;
  


  begin
    OldTime := '';
    if not isFloppy then 
        begin
        if CF_IOBoard = CF_EIO then
            begin
            get_EIO_time;
            end;
            
        4: OldTime := GetEtherTime(60);

        if OldTime = '' then
            begin
            3: TimeFID := FSInternalLookUp(TimeFileName, blks, Timebits);
            if (TimeFID = 0) or (blks<>1) or (Timebits<>TimeFBitSize) then
                begin
                TimeFID := FSEnter(TimeFileName);
                FSClose(TimeFID, 1, TimeFBitSize);
                end
            else begin
                 if OldTime = '' then
                    begin
                    NEW(buf.p);
                    FSBlkRead(TimeFID, 0, buf.p);
                    StampToString(buf.t^, OldTime);
                    Adjust(OldTime, length(OldTime)-3); {remove seconds}
                    end;
                end;
            end
        else
            begin
            writeln('TimeServer returned ', OldTime);
            SetTString(OldTime);     {may raise BadTime and go back to 2}
            haveTime := true;
            exit(getTime);
            end;
        end;

  2: if OldTime = '' then 
         write('Enter date and time as DD MMM YY HH:MM:SS :')
     else 
         write('Enter time as HH:MM or full date: [', OldTime, '] ');
     if not eoln then 
         read(TimeIn)
     else
         TimeIn := OldTime;
     Readln;

     if (length(TimeIn)<=8) and (PosC(TimeIn, ' ')=0) 
             and (PosC(TimeIn, ':')<>0) and (OldTime<>'') then
         Str := concat(substr(OldTime, 1, 10), TimeIn)
     else 
         Str := TimeIn;
     SetTString(Str);     {may raise BadTime and go back to 2}
     haveTime := true;
  end;
    

{-----------------------------
{ LogIn    Main program
{----------------------------}   
var 
   IgnoreI: integer;
   Dum: Integer;
   Str: string;   

begin
DefTabletType := ord(GPIBBitPad);
DefRealRelTablet := false;
Str := Concat('LogIn version ', LogInVersion);
Str := Concat(Str, '      ');
Str := Concat(Str, '    POS ');
Str := Concat(Str, MainVersion);
AppendChar(Str,'.');
Str := Concat(Str, StrVersion);
Str := Concat(Str, '  ');
AppendChar(Str, chr(SysBootChar));
Str := Concat(Str, '-boot     ');
ChangeTitle(Str);

    CmdTable[PathIndex] := 'PATH            set the default path to argument.';
    CmdTable[SetIndex] :=  'SETSEARCH       push (or pop with -) argument onto search list.';
    CmdTable[ShellIndex] :='SHELL           set the name of the shell program.';
    CmdTable[CmdIndex] :=  'COMMAND         set the first command to run.  Use @ to run a command file.';
    CmdTable[CursIndex] := 'CURSORFUNCTION  set the default cursor function.  Arg is 0..7.';
    CmdTable[ScrIndex] :=  'SCREENBOTTOM    set bottom of screen.  Args are ON, OFF, WHITE, BLACK.';
    CmdTable[HelpIndex] := 'HELP';
    CmdTable[PointIndex] :='POINTALLOWED    pointing device is used? (TRUE implies popUp menus allowed).';
    CmdTable[ProfIndex] := 'PROFILE         set the profile to use.';
    CmdTable[RealRelIndex]:='REALRELATIVE    mode of tablet is true relative? (TRUE imples yes).';
    CmdTable[TabletIndex]:='TABLETTYPE       select the tablet to be used. (Args are TABLET or BITPAD).';


    ScrBotTable[OnIndex] :=    'ON';
    ScrBotTable[OffIndex] :=   'OFF';
    ScrBotTable[BlackIndex] := 'BLACK';
    ScrBotTable[WhiteIndex] := 'WHITE';

HaveTime := UsrCmdLine<>'';

1:

LoginReset;     { Reset switches and global vars }
if not haveTime then GetTime;
CheckLogIn;     { Check for a valid user }
WriteLn('Initializing for user: ',curUserName);
ClearUserState; { Clear the user's state }
DoProfile;      { Get the profile file }
HandleSwitches; { Handle Switches set up by user and profile }

if HoldPath <> '' then
   begin
   if HoldPath[length(HoldPath)] <> '>' then AppendChar(HoldPath, '>');
   if FSInternalLookUp(HoldPath, dum, dum) = 0 then
      begin
      FixFileName(HoldPath, true);
      WriteLn('** New path ',HoldPath,' doesn''t exist.');
      HoldPath := '';
      end;
   end;

if HoldPath = '' then HoldPath := ':User>Guest>';

if FSInternalLookUp(HoldPath, dum, dum) = 0 then HoldPath := '>';
FSRemoveDots(HoldPath);

FSDirPrefix := HoldPath;
    
if ShellOne<>'' then TrySetShell(ShellOne);   {try shell from command line}
if ShellTwo<>'' then TrySetShell(ShellTwo);   {try shell from profile}
TrySetShell(concat(ShellConst, concat(StrVersion, '.Run')));
if HoldShell <> '' then
    ShellName := HoldShell
else begin {no shell, loop running login}
   writeln;
   writeln('** No Shell is found.  You may use the /Shell= switch.');
   writeln('** (Any program may be named after the =.)');
   UsrCmdLine := 'Login'; {prevents use of Principal.User}
   goto 1;
end;

RFileName := ShellName;

if HoldCmd<>'' then begin
    if RemoveQuotes(HoldCmd) then;
    UsrCmdLine := HoldCmd;
    UseCmd := True;
    end
else UseCmd := False;

end.
