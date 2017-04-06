{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program UserControl;

{--------------------------------------------------------------------------
{
{ Abstract:
    This program is used to maintain the user file.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corporation,  1981, 1982, 1983
{
{--------------------------------------------------------------------------}


{{$Version V1.4}
{---------------------------------------------------------------
{ Change Log:
{
{ 19-Feb-82  V1.4  WJHansen
{ Fixed /PROFILE and /PASSWORD
{ fixed limiting name to 60 chars
{ }

{ 28-Jan-82  V1.3  Chuck Beckett
{ Changed user interface - now standard with D.6 release
{1}

{ 29-Apr-81  V1.2  Don Scelza
{ Added code to Remove user & deal with profiles.
{ }

{ 19-Mar-81  V1.1  Brad Myers
{ Changed PERQ.String to PERQ_String.
{ }

{  5-Mar-81  V1.0  Don Scelza
{ Created the program UserControl.
{ }
{------------------------------------------------------------------------}

imports Screen      from Screen;
imports IO_Unit     from IO_Unit;
imports IOErrors    from IOErrors;
imports system      from system;
imports CmdParse    from CmdParse;
imports UserPass    from UserPass;
imports Perq_String from Perq_String;
imports FileUtils   from FileUtils;

var Name, Pass, Cmd, InLine, Broke, Prof,
          PromptString, Err: String;
    BreakChar: Char;
    CmdIndex, SwiIndex: integer;
    UserInfo: UserRecord;
    CmdTable, SwiTable: CmdArray;
    User, Group: IDType;
    Prompting, Doit, IsSwitch: boolean;
    Scan, Switch: pSwitchRec;
    Arg, Arg2: pArgRec;
    
Const HelpIndex = 1;
      AddUserIndex = 2;
      CheckUserIndex = 3;
      NewFileIndex = 4;
      ListIndex = 5;
      QuitIndex = 6;
      RemUserIndex = 7;
      NumCmds = 7;
      
      PassIndex = 1;
      GroupIndex = 2;
      ProfIndex = 3;
      SwiHelpIndex = 4;
      NumSwis = 4;
      BadSwitch = 5;
      SwiNotUnique = 6;
      
      Delimiters = ' ';
      
      GroupDefault = 1;
      DefPro1 = 'sys:user>';
      DefPro2 = '>Profile';
      
      Version = 'V1.4';

label 1;


procedure Init;
{--------------------------------------------------------------------
{
{ Abstract:
{    This procedue is called to initialize the program.
{
{-------------------------------------------------------------------}
    begin
    FSAddToTitleLine(Concat(Concat('User Control ',VERSION),' Type HELP for help  '));
    CmdTable[HelpIndex] :=      'HELP';
    CmdTable[AddUserIndex] :=   'ADDUSER';
    CmdTable[RemUserIndex] :=   'REMOVEUSER';
    CmdTable[CheckUserIndex] := 'CHECKUSER';
    CmdTable[QuitIndex] :=      'QUIT';
    CmdTable[NewFileIndex] :=   'NEWFILE';
    CmdTable[ListIndex] :=      'LISTUSERS';

    SwiTable[PassIndex] := 'PASSWORD';
    SwiTable[GroupIndex] := 'GROUP';
    SwiTable[ProfIndex] := 'PROFILE';
    SwiTable[SwiHelpIndex] := 'HELP';
    
    PromptString := ConCat( 'USERCONTROL', CmdChar);
    end;


procedure PrintHelp(Indx: integer);
{----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to supply help to the user.
{
{------------------------------------------------------------------------}
var I: Integer;
begin
  Doit := false;
  case Indx of
    HelpIndex: begin;
writeln('    ');
writeln('    USERCONTROL - ');
writeln('    ');
writeln('              Enables the maintenance of user information for a single PERQ,');
writeln('              including User''s Name, Password, Group ID, and Profile Path. ');
writeln('              This information is maintained in the User File.');
writeln('              ');
writeln('    COMMANDS');
writeln('    ');
writeln('    HELP or /HELP or "HELP" key - Type this message.');
writeln('      ');
writeln('        The HELP command can appear as either a switch or a command. It can');
writeln('        appear ANYWHERE in a command line.  If is is the only item on the');
writeln('        line, you will receive a brief abstract of USERCONTROL.');
writeln('        ');
writeln('        Examples:  USERCONTROL HELP    ! request from command level');
writeln('                   USERCONTROL /HELP   ! request from command level');
writeln('                   USERCONTROL',CmdChar,'HELP             ! interactive request');
writeln('                   USERCONTROL',CmdChar,'/help            ! interactive request');
writeln('                    ');
writeln('        If the /HELP switch is used anywhere on a command line, then');
writeln('        that command will be explained.');
writeln('        ');
writeln('        Examples:  USERCONTROL QUIT /HELP  ! explains the quit command');
writeln('                   USERCONTROL',CmdChar,'LISTUSERS /HELP      ! explains the listusers command');
writeln('    ');
writeln('    QUIT - Exit from USERCONTROL.');
writeln('    ');
writeln('');
writeln('    LISTUSERS - List all current Users.');
writeln('    ');
writeln('    ADDUSER - Add new information about a user, either a current user, in');
writeln('              which case the new information UPDATES the previous information,');
writeln('              or a totally new user, in which case the command ADDS the new');
writeln('              user and his/her information to the User File.');
writeln('    ');
writeln('    ');
writeln('    REMOVE name');
writeln('');
writeln('        Removes a user and his or her information from the user file.');
writeln('    ');
writeln('    ');
writeln('    CHECKUSER - Validates a user name and password pair. ');
writeln('        ');
writeln('        ');
writeln('    NEWFILE - DESTROY all old user information.  Create a new User File.');
writeln('        ');
        end;
   { Print Help Procedure - Help for CheckUser }


    CheckUserIndex: begin;
    
writeln('');
writeln('    CHECKUSER - Validate a user name and password for correctness.');
writeln('');
    end;


   { Print Help Procedure - Help for AddUserIndex }


    AddUserIndex: begin;
    
writeln('');
writeln('');
writeln('    ADDUSER name [/PASSWORD] [/GROUP=group] [/PROFILE=path] [/HELP]');
writeln('    ');
writeln('        Adds user information.  If user is new to the system, (the name');
writeln('        supplied is not described in the User file), then the information');
writeln('        supplied is ADDED to a NEW user File entry.  If the user is already');
writeln('        described on the system, the information supplied UPDATES that');
writeln('        user''s previous information.');
writeln('        ');
writeln('        Argument - name - is a string of from one to 31 characters which');
writeln('                          is prompted for as part of the LOGIN procedure.');
writeln('                          The string should contain no blanks, commas, ');
writeln('                          equal signs, or slashes.');
writeln('                          ');
writeln('                          ');
writeln('        Valid Switches are:');
writeln('                          ');
writeln('                 /PASSWORD  - takes no arguments, but causes USERCONTROL to ');
writeln('                          prompt for a new password interactively.');
writeln('                          ');
writeln('                          If this switch is not entered a default');
writeln('                          password of the null string is established.                                     ');
writeln('                          All PASSWORDS MUST BE ENTERED INTERACTIVELY,');
writeln('                          to insure secure and accurate password assignment.');
writeln('                          ');
writeln('                 /GROUP - is an integer between 0 and 255 decimal, which');
writeln('                          will be the user''s group identifier.  If this ');
writeln('                          switch is entered without an argument USERCONTROL');
writeln('                          will prompt for one. If this switch is not entered,');
writeln('                          the user''s group ID will remain unchanged. For new');
writeln('                          users, the default identifier is "1".   ');
writeln('    ');
writeln('                 /PROFILE -  is a valid full path to the user''s profile file.');
writeln('                          If this switch is entered without an argument');
writeln('                          user control will prompt for one.  If this PROF');
writeln('                          switch is not entered the user''s profile will');
writeln('                          remain unchanged.  For new users, the default');
writeln('                          profile is "SYS:USER>name>PROFILE".');
writeln('    ');
    end;
   { Print Help Procedure - Help for QUIT }


    QuitIndex: begin;
    
writeln('    ');
writeln('    QUIT ');
writeln('    ');
writeln('        Terminates an interactive USERCONTROL session and returns to the');
writeln('        command level.  Quit takes no arguments or switches other than');
writeln('        help.  If the quit command is issued from the command level, ie:');
writeln('        on the same line as USERCONTROL, it serves as a "No-Operation".');
writeln('        ');
    end;


   { Print Help Procedure - Help for NewFile }


    NewFileIndex: begin;
    
writeln('            ');
writeln('    NEWFILE   ');
writeln('');
writeln('        Creates a new User File, destroying the old file.');
writeln('    ');
    end;


   { Print Help Procedure - Help for LISTUSERS }


    ListIndex: begin;
    
writeln('');
writeln('    LISTUSERS ');
writeln('    ');
writeln('        Lists all current users on the screen.  Provides each user''s name');
writeln('        and their profile path.');
writeln('        ');
    end;


   { Print Help Procedure - Help for ! }


    RemUserIndex: begin; 
    
writeln('    ');
writeln('    REMOVEUSER - Remove/Delete a user and his/her information.  The User File');
writeln('                 entry for that user is deleted.');
writeln('                 ');
    end;

    otherwise: StdError (ErBadCmd, CmdTable[CmdIndex], false);
  end {case};
end;


Function ConVid (Str: String; var Ident: IdType): boolean;
{----------------------------------------------------------------------
{
{ Abstract:
{    Converts a string to a User ID.
{
{------------------------------------------------------------------------}
  var I: Integer;
      Ch: Char;
      Bad: boolean;
      
    begin
      bad := not (length(Str) > 0);
      I := 0;
      while (length(Str) > 0) and not bad do
        begin
          Ch := Str[1];
          Delete(Str,1,1);
          if ( (ord(ch) < ord('0') ) or 
               (ord(ch) > ord('9') ) ) then {not numeric}
            bad := true
          else
            I := I * 10 + ord(ch) - ord('0');
          
          if I > 255 then bad := true;
        end;
      
      Ident := Recast ( I, IdType);
      ConVid := not Bad;
    end;

function GetName(var NameStr: string): boolean;
{----------------------------------------------------------------------
{
{ Abstract:
{    Returns a valid user name from command line or interactively.
{
{------------------------------------------------------------------------}

var OK: boolean;

  
begin
  OK := true;
  if (Arg^.Name = '') then  { derive a valid name } 
    begin
      scan := switch;  { we have no name is it an exception? }
      
      while scan <> nil do
        begin
          ConvUpper (Scan^.Switch);
          if scan^.switch = 'HELP' then { user is requesting help }
            begin
              OK := false;  { help switch presence overrides our }
              scan := nil;    { detected error: we always give help}
            end
          else
            scan := scan^.next { continue to scan for help switch };
        end;
        
      if OK then { we have no name and should have one }
        if Prompting then { let's prompt for one }
          begin
            repeat
              write ('    Name: ');
              readln (NameStr);
            until ( Length(NameStr) > 0);
          end
        else { error: name is mandatory }
          begin
            OK := false;
            StdError (ErAnyError, '** You must supply a name', false);
          end;
    end {begin}
    
  else { we have a name }
    
    begin
      NameStr := arg^.name;
      if length(NameStr)>60 then Adjust(NameStr,60);
      if (arg^.next <> nil) or (arg2^.name <> '') then { error in name }
        begin
          OK := false;
          StdError (ErOneInput, 'Name', false);
        end;
    end;
  
  GetName := OK;
  
end;

procedure ReadPaOnce(var PassWord: String);
{-----------------------------------------------------------
{ Abstract:
{    Reads password without displaying it.
{ Parameter:
{    PassWord - Set to the string read in.
{ Design:
{    Processes backspace, oops, ^H, ^U, and RETURN.
{-----------------------------------------------------------}
       
    const
       CtlU = chr( ord('U') - ord('A') + 1);
       CtlH = chr( ord('H') - ord('A') + 1);
       ChCR = chr( ord('M') - ord('A') + 1);
    var 
       reading: Boolean;
       Ch: char;
    begin
    SCurOn;
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


procedure ReadPassWord(var Pass: String);
{-----------------------------------------------------------
{ Abstract:
{    Reads password without displaying it.
{ Parameter:
{    PassWord - Set to the string read in.
{-----------------------------------------------------------}

  var Pass2 : string;
  
  begin
    repeat
      write('    Password: ');
      ReadPaOnce(Pass);
      write('       Again: ');
      ReadPaOnce(Pass2);
      if (pass <> pass2) then writeln ('Entries are different.  Try again.');
    until (pass = pass2);
  end;

procedure Add;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Add a user to the user file.
{
{--------------------------------------------------------------------------}
var Change: Boolean;
  

Handler HelpKey(var Ret: sys9s);
  begin
    PrintHelp(AddUserIndex);
    Doit := false;
    Ret:= concat ( '/HELP',CCR);
  end;
  
begin     
  if (Doit) then { derive default values for user info  }
  
  { The default values we derive are placed in the arguments we will }
  { use to the AddUser function.  User specified values overide our  }
  { defaults by being assigned to the arguments we will use.         }
  
    begin
      Doit := (Doit and GetName(name) );
      Pass := '    ';
      { fill defaults used when no current info exists }
      Group := 1;
      Prof := Concat( Concat ( DefPro1, Name), DefPro2);
      if FindUser (Name, UserInfo) then 
        begin
          if UserInfo.InUse then  {user already exists }
            begin { fill defaults from current information }
              Group := UserInfo.GroupId;
              Prof := UserInfo.Profile;
            end
        end;
    end; { derive defaults }

  while switch <> nil do  { Extract user input from switches }
    begin
      convupper (switch^.switch);
      SwiIndex := UniqueCmdIndex(Switch^.Switch, SwiTable, NumSwis);
      case SwiIndex of
      
      PassIndex: begin
                  if (switch^.arg <> '') then { error: /PASS takes no args }
                    begin
                      Doit := false;
                      Stderror (ErNoSwParam, Switch^.Switch, False);
                    end
                  else 
                    if Doit then { no error - let's get the password }
                      begin
                        writeln('For User ',Name);
                        ReadPassWord(Pass);
                        Pass := Concat(Pass,'    ');
                      end;
                end;
      GroupIndex: begin
                    if (switch^.arg <> '') then { group is supplied }
                      begin
                        if not ConvId (Switch^.Arg, Group) then
                          begin
                            Doit := false;
                            StdError (ErSwParam, switch^.switch, false);
                          end;
                      end
                    else { we must prompt for group if interactive }
                      if (Prompting) then { get group interactively }
                        begin
                          write ('    Group ID: ');
                          readln (Group);
                        end 
                      else { one line command mode }
                        Group := GroupDefault;
                  end; 
      ProfIndex: begin
                   if (switch^.arg <> '') then { Profile is supplied }
                     Prof :=  switch^.arg
                   else { we must prompt for Profile }
                     if Prompting then { get Profile interactively }
                       begin
                         write ('    Prof: ');
                         readln (Prof);
                       end 
                     else { one line command mode }
                       Prof := 'ProfDefault'
                 end;
      otherwise: StdError (ErAnyError, '** Internal logic error', true);

             end; {case}
           
      switch := switch^.next;
    end; { do while }

  if (Doit) then { Apply User Information to User File }
    if not AddUser(Name, Pass, Group, Prof) then
      writeln('Could not make changes to the user file.');
end;

procedure Kill;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Remove a user from the file.
{
{--------------------------------------------------------------------------}
begin
  if Doit then
    if GetName (Name) then { we've got a name }
      if Doit then
        if not RemoveUser(Name) then
          writeln('** Could not remove user from the user file.');
end;
    
    

procedure WipeFile;
{--------------------------------------------------------------------------
{
{ Abstract:
{    Execute the New File function, which wipes out a file.
{
{--------------------------------------------------------------------------}
  var Confirm: string;
  
  begin
    if (Arg^.Name <> '') then { we got an argument }
      begin
        Doit := false;
        StdError (ErNoCmdParam, CmdTable[CmdIndex], false);
      end;
    
    if Doit then
      if prompting then
        begin
          writeln(' You will be destroying all current user information!');
          write(' Do you really want to do this? [no] :');
          readln (Confirm);
          if (length (Confirm) = 0) then 
            Doit := false
          else
            begin
              ConvUpper (Confirm);
              if (Confirm[1] <> 'Y') then Doit:= false;
            end
        end;
      
    if Doit then
      begin
        NewUserFile;
        writeln ('New File Created');
      end;

  end;

procedure ListThem;
{--------------------------------------------------------------------------
{
{ Abstract:
{    Provide a list of the valid users of the system.
{
{--------------------------------------------------------------------------}
  begin
    if (Arg^.Name <> '') then { we got an argument }
        begin
          Doit := false;
          StdError (ErNoCmdParam, CmdTable[CmdIndex], false);
        end;
    
    if Doit then
      ListUsers;
  
  end;

procedure Check;
{-------------------------------------------------------------------------
{
{ Abstract:
{    Check to see if a user is valid.
{
{--------------------------------------------------------------------------}

Handler HelpKey(var Ret: sys9s);
  begin
    PrintHelp(AddUserIndex);
    Doit := false;
    Ret:= concat ( '/HELP',CCR);
  end;
  
  begin
    Doit := (Doit and GetName (Name) );
    
    if Doit then
      begin
        write('    Password: ');
        ReadPaOnce(Pass);
        Pass := Concat(Pass,'    ');
        if not ValidUser(Name, Pass, UserInfo) then
            writeln('Invalid user or password.')
        else
            begin
            writeln(Name, ' is a valid user.');
            writeln('User ID:  ', UserInfo.UserID:1);
            writeln('Group ID: ', UserInfo.GroupID:1);
            end
      end;
  end;


begin
  Init;
  InLine := UsrCmdLine;
  BreakChar := NextIdString(InLine,Cmd,IsSwitch);
  Prompting := BreakChar = CCR;

  repeat
    Doit := true;
    if Prompting then
      begin                  {If interactive, then this     }
        write(PromptString); {Prompt gets commands for loop }
        readln(InLine);
      end;

    BreakChar := NextIdString(InLine,Cmd,IsSwitch);
    
    { Parse the input string }
 
    Doit := (Doit and ParseStringArgs (InLine, arg, arg2, switch, err) );
    if err <> '' then { we got an error }
      begin
        Doit := false;
        StdError (ErAnyError, err, false);
      end;
      
    ConvUpper(Cmd);
    CmdIndex := UniqueCmdIndex(Cmd, CmdTable, NumCmds);
    
    scan := switch;  { Convert Switches and look for the HELP switch }
    SwiIndex := 0;
    while (scan <> nil) and (SwiIndex <> SwiHelpIndex) do
      begin
        ConvUpper ( Scan^.Switch);
        SwiIndex := UniqueCmdIndex(Scan^.Switch, SwiTable, NumSwis);
        case SwiIndex of
          SwiHelpIndex: begin
            Doit := false;  
            PrintHelp (CmdIndex);
          end;
          SwiNotUnique: begin
                          Doit := false;
                          StdError (ErSwNotUnique, Scan^.Switch, false);
                        end;
          BadSwitch: begin
                       Doit := false;
                       StdError (ErBadSwitch, Scan^.Switch, false);
                     end;
          Otherwise: if CmdIndex = AddUserIndex then { Add takes switches}
                        Scan^.Switch := SwiTable[SwiIndex]
                     else { others don't so flag them }
                       begin
                         Doit := false;
                         StdError (ErBadSwitch, Scan^.Switch, false);
                       end;
        end;{case}
        scan := scan^.next { continue to scan };
      end;
      
      if doit then
        case CmdIndex of
          HelpIndex: PrintHelp(HelpIndex);
          CheckUserIndex: Check;
          AddUserIndex: Add;
          QuitIndex: Exit(UserControl);
          NewFileIndex: Wipefile;
          ListIndex: ListThem;
          RemUserIndex: Kill; 
          otherwise: StdError (ErBadCmd, Cmd, false);
        end {case};
      
    DstryArgRec(Arg);
    DstryArgRec(Arg2);
    DstrySwitchRec(Switch);
    
1:until not Prompting { loops in interactive, single command otherwise };

end.
