{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FTPUser;
{-------------------------------------------------------------------------
{
{ Abstract:
{    This program provides the user interface to the File Transfer Module.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1982, 1983.
{
{-------------------------------------------------------------------------}

{$Version V7.9 For POS}
{--------------------------------------------------------------------------
{ Change Log:
{
{ 15 Mar 83  V7.9  WJHansen
{  Call RestoreSwitches in FTP after each command.
{  Make NeedNewStatusDisplay local to DoSwitches;
{  do DisplayStatus anywhere else that changes switches.
{  Catch BadBaudRate in StrSetDev and RestoreSwitches.
{  Remove global NonUnique and NonExistent.
{  Set HoldConfirm in SaveSwitches.
{  If there is a parsing error, ask user if he wants to continue.
{  Add NeedDisplayStatus to RestoreSwitches.
{  Put control-G's in error messages.
{  Add warning about local and remote prefixes on Poll command.
{  Implement FTPError.
{  Reorder routines.
{  Check for extra text on line in FirstSwitch.
{  In DoSwitches, do not set baud rate unless device is RS232.
{
{  7 Mar 83  V7.8  WJHansen
{  Remove check for zero block file in Put.  (Allows putting directory.)
{  Do not reset remote machine name for Device command.
{
{ 28 Feb 83  V7.7  WJHansen     
{  Remove MPOS.
{  Implement switches for RemotePrefix and LocalPrefix.
{  Implement RSA and RSB.
{  Install SaveSwitches for default switch setting.
{  Remove E10ReceiveDone handler.
{  Use version number from FTPVersion and put in valid $version line.
{  Remove definition of x-ConfirmNonWildDelete and x-AskWild.
{  Remove some unneeded global private variables.
{  Move all switch init from Initialize to SetInitialState.
{  Correct and improve various messages.
{  Do IOPutStatus before setting baudrate.
{  Poll mode show 'Done'.
{  If file has 0 blocks after FTPGetFile, Delete the file.
{  Use [None] as default prefix.
{
{ V7.6   11 Feb 83  Brad Myers   Revise windows for landscape.
{
{ V6.23  07 Jan 83  S Johar      Fix an exit statement in putf.
{
{ V6.22  03 Dec 82  S Johar      Do switch processing before checking if
{                                a conn has been made when using ethernet.
{
{ V6.21  15 Nov 82  D Scelza     Changed default device to be Ethernet.
{
{ V6.9   28 Oct 82  D Kalp       1. Import new module FTPVersion and use it to
{                                   to get the version number.
{                                2. Put the status display at top of screen
{                                   and make calls to DisplayStatus more 
{                                   precise.
{                                3. Add procedure FixupWindows to be used
{                                   instead of ScreenReset on exit.
{                                4. Make POLL cmd work with switches.
{                                5. Fix bugs in PutF and GetF to release 
{                                   memory allocated to InFiles, OutFiles, etc.
{                                   using DstryArgRec. Hacked over rest of code
{                                   to make sure memory gets deallocated.
{                                6. Add BadBaudRate Handler to DoSwitches.
{                                7. Fix some other minor bugs.
{
{ V6.8   19 Oct 82  D Scelza     See change log in FTP.
{
{ V6.6   19 Jul 82  D Scelza     Fixed memory bug in FTP.
{
{ V6.5   15 Jul 82  D Scelza     First release of FTP with multiple receives.
{
{ V5.6   14 Jun 82  D Scelza     Added code to retry connects.
{
{ V5.5   07 May 82  D Scelza     Fixed bug I put in in FTPUtils.
{
{ V5.4   07 May 82  D Scelza     Fixed bug in FTPUtils.
{
{ V5.3   22 Apr 82  M Faust      Fixed bug in CheckFile which caused it not
{                                to return a value.
{
{ V5.2   22 Apr 82  D Scelza     Fixed some bad exit commands.
{
{
{ V5.1   21 Apr 82  M Faust      Status window, and local and remote file
{                                prefixes.
{
{ V5.0   20 Apr 82  D Scelza     New Changes.  Connect and new interface.
{
{ V4.12  12 Apr 82  D Scelza     Put in device ByteStreamNet.
{
{ V4.11  22 Mar 82  S Johar      Cleanup headers/ Version Nums
{
{ V4.10  17 Mar 82  S Johar      Import Ether10IO.pas. E10Init on Exit.
{
{ V4.9    9 Mar 82  S Johar      Fix to do the right Thing with NetDev Off.
{                                convert to standard document format. Start
{                                MPOSVersion.
{
{ V4.8   26 Feb 82  S Johar      Remove link as a device.
{
{ V4.7   19 Feb 82  S Johar      SendStopVax on quit of FTP.
{
{ V4.6   19 Feb 82  S Johar      Export the Retry Var to Ftp.Pas
{
{ V4.5   18 feb 82  S Johar      Fixed the bug causing all putfiles to be
{                                in text mode.
{
{ V4.4   12 Feb 82  S Johar      Added the SetAddrs routine. Check for correct
{                                addressing before file transfers.
{
{ V4.3    2-Feb-82  S Johar      Fixes of bugs in Interface.
{
{ V4.2   21-Jan-82  S Johar      Include EtherNet as a device, as a conditional
{                                option.
{
{ V4.1   19-Jan-82  S Johar      Catch The HelpKey Exception whenever reading
{                                in values for mode, device, baud etc.
{
{ V4.0   19-Jan-82  S Johar      Released for D6.
{
{ V3.3   15-Jan-82  S Johar      Baud, Device, Mode made local Switches in
{                                addition to being global commands. 
{
{ V3.2   15-Jan-82  S Johar      Polling moved to the background.
{
{ V3.1    4-Jan-82  S Johar      UserInterface changed to use new parseCmd
{                                File renamed to FTPUSER.PAS. Main program
{                                In Ftp.pas. DEVICE command removed for now.
{
{ V3.0   10-Nov-81  Mark Faust   Support for VaxFTP.
{
{
{ V2.3   23-Sep-81  Diana Forgy  Renamed FTPMain FTP to conform to 
{                                software naming conventions. Broke
{                                up FTPMainProc into smaller units.
{                                Added procedures InitCommandFile, 
{                                CloseCommandFile, and DoCommandFile
{                                to handle input from command files.
{ V2.2    1-Jun-81  Brad Myers   Changed to catch exceptions from RS232baud.
{ V2.1   21-Apr-81  Don Scelza   Release version without the link.
{ V2.0   13-Apr-81  Don Scelza   Changed to use the new FTP module.
{ V1.0    7-Nov-80  Brad Myers   second name defaults, no extra byte
{ V0.1   30-Sep-80  Brad Myers   Added Lookup of files locally to
{                                prevent bombing out on reset or
{                                rewrite.  Added BAUD command and
{                                automatic setting to 9600 if 
{                                PERQPERQ.
{ V0.0   ??-???-??  Don Scelza   Started
{
{----------------------------------------------------------------------------}



EXPORTS

Imports CmdParse From CmdParse;
Imports FtpUtils From FtpUtils;
Imports PopUp From PopUp;        {here for pNameDesc}
    
Const 
      NumCmds = 17;
      
      ModePerqIndex = 1;
      ModePDP11Index = ModePerqIndex + 1;
      ModeVaxIndex   = ModePDP11Index + 1;
      NumModes = ModeVaxIndex;
      
      DevRS232Index = 1;
      DevRSAIndex = DevRS232Index + 1;
      DevRSBIndex = DevRSAIndex + 1;
      DevEtherIndex = DevRSBIndex + 1;
      DevByteIndex  = DevEtherIndex + 1;
      NumDev = DevByteIndex;

      AskSwitch = 1;
      NoAskSwitch = AskSwitch + 1;
      ConfSwitch = NoAskSwitch + 1;
      NoConfSwitch = ConfSwitch + 1;
      TextSwitch = NoConfSwitch + 1;
      NoTextSwitch = TextSwitch + 1;
      HelpSwitch = NoTextSwitch + 1;
      DeviceSwitch = HelpSwitch + 1;
      ModeSwitch = DeviceSwitch + 1;
      BaudSwitch = ModeSwitch + 1;
      ConnSwitch = BaudSwitch + 1;
      RemoteSwitch  = ConnSwitch + 1;
      LocalSwitch  = RemoteSwitch + 1;
      NumSwitches = LocalSwitch;

Var
    InFiles, OutFiles : pArgRec;
    Switches : pSwitchRec;
    Caller : String;
    HelpString : String[255];
    JustHelp, IllegalSwitch : Boolean;
    Finished : Boolean;
    TitleLine : String[255];
    
    FTPDev: DevTypes;
    FtpMode : TransMode;
    HoldDevice : DevTypes;
    HoldMode : TransMode;
    LocalPrefix, RemotePrefix, HoldLocalPrefix, HoldRemotePrefix :String;
    AskSingle, HoldAskSingle,IsText,HoldIsText, Confirm, HoldConfirm : Boolean;
    HoldBaudRate : String;
    DefBaudRate : String;
    BaudRate : String;
    
    CmdTable, SwitchTable : pNameDesc;
    AddrSet : Boolean;
    DoRetry : Boolean;
    NeedNewStatusDisplay : Boolean;
    
Procedure FixupWindows;
Procedure DisplayStatus;
Procedure Initialize;
Procedure FirstSwitch(Line : string);
Procedure DoSwitches(CalledFrom : String);
Procedure RestoreSwitches;    
Procedure SaveSwitches;    
Procedure SetAsk(Val: Boolean);
Procedure SetConfirm(Val : Boolean);
Procedure SetText(Val : Boolean);
Procedure Connect;
Procedure GetF;
Procedure PutF;
Procedure Quit;
Procedure Mode;
Procedure Device;
Procedure Help;
Procedure Poll;
Procedure FastPoll;
Procedure Baud;
Procedure SetRemotePrefix;
Procedure SetLocalPrefix;
Procedure FTPError(A,B,C: string; WillContinue: Boolean);

PRIVATE


Const Debug = False;

Const

    MainWindow = 2;
    StatWindow = 1;

    FF = chr(#014);             { FormFeed }
    Bell = chr(#007);           { sound the alarm }
    Ch_RETURN = chr(#015);      { ^M = RETURN }
    Ch_LF = chr(#012);          { ^J = LF }

Imports FtpVersion From FtpVersion;

Imports PopCmdParse From PopCmdParse;
Imports Perq_String from Perq_String;

Imports FileSystem from FileSystem;
Imports FileUtils from FileUtils;
Imports IO_Unit From IO_Unit;
Imports IO_Others From IO_Others;
Imports IOErrors From IOErrors;   

imports Configuration from Configuration;
Imports System From System;
Imports RS232Baud from RS232Baud;
Imports Stream from Stream;
Imports Screen From Screen;
Imports UtilProgress from UtilProgress;


Var 
    ModeNames,DeviceNames: CmdArray;


Procedure Help;
{----------------------------------------------------------
{ Abstract: The Help for FTP
{
{-----------------------------------------------------------}
var 
    Dum: integer;
Begin
    Writeln;
    writeln('      FTP transfers files between a Perq and some other ');
    writeln('      machine.  Connecting devices presently supported ');
    writeln('      are the RS232 cable and the ETHERNET.');
    Writeln;
    writeln('      To use FTP first make sure that the machines are connected.');
    Writeln;
    writeln('      FTP Commands are:');
    writeln('          GET         -  Retrieve a file from the remote machine.');
    writeln('                         You will be prompted for arguments not');
    writeln('                         supplied.');
    Writeln;
    writeln('          PUT         -  Send a file to the remote machine. You');
    writeln('                         will be prompted for arguments not ');
    writeln('                         supplied.');
    Writeln;
    writeln('          QUIT        -  Quit the Program.');
    Writeln;
    writeln('          POLL        -  Allow the remote machine to control');
    writeln('                         the transfer.');
    Writeln;
    writeln('      The following may be either commands, switches, or switches');
    writeln('      on other commands.  As commands or switches by themselves,');
    writeln('      they set the overall default.  As switches on other commands,');
    writeln('      they set values for that command alone.');
    Writeln;
    writeln('          CONNECT     -  Get the address of the remote machine');
    writeln('                         for an Ethernet transfer.');
    Writeln;
    writeln('          DEVICE      -  Sets the name of the Device used for the');
    writeln('                         transfer.  Valid Devices are:');
    For Dum := 1 To NumDev Do writeln(' ':30, DeviceNames[Dum]);
    Writeln;
    writeln('          LOCALPREFIX -  Sets a prefix to be prepended to all');
    writeln('                         local file pathnames.');
    Writeln;
    writeln('          REMOTEPREFIX-  Sets a prefix to be prepended to all');
    writeln('                         remote file pathnames.');
    Writeln;
    writeln('          BAUD        -  Sets the Baud Rate for RS232 transfers.');
    Writeln;
    writeln('          MODE        -  Sets the type of the remote machine.');
    writeln('                         Valid types are:');
    For Dum := 1 To NumModes Do writeln(' ':30, ModeNames[Dum]);
    Writeln;
    writeln('          ASK         -  FTP asks permission for each transfer.');
    writeln('          NOASK       -  Turns the permission request off.');
    Writeln;
    writeln('          NOCONFIRM   -  Overwrite files without notice.');
    writeln('          CONFIRM     -  Get confirmation before overwriting an');
    writeln('                         existing file. **WARNING**');
    writeln('                            CONFIRM only works for local files.');
    Writeln;
    writeln('          BINARY      -  Does not examine the bits of the file.');
    writeln('                         For Perq-Perq, this works for any file.');
    writeln('          TEXT        -  Asserts that the file is text only;');
    writeln('                         line ends are given special treatment.');
    Writeln;
End;
    
   

Procedure FixupWindows;
{--------------------------------------------------------------------
{
{ Abstract:
{
{   Before leaving Ftp, set the cursor position of window 0 to 
{   correspond to the line below the current line in MainWindow.
{
{-------------------------------------------------------------------}
Var x, y: integer;
Begin
   ChangeWindow(MainWindow);
   writeln;
   SReadCursor(x, y);
   ChangeWindow(0);
   SSetCursor(x, y);
End;


Procedure DisplayStatus;
{--------------------------------------------------------------------
{
{ Abstract:
{
{   Display status of major modes in the StatWindow.  Return to
{   MainWindow when through.
{
{-------------------------------------------------------------------}
Var x, y: integer;
Begin
ChangeWindow(StatWindow);
Write(FF);
SReadCursor(x, y);
SSetCursor(x, y+21);
Writeln('Local Prefix:      [',LocalPrefix,']');
Writeln('Remote Prefix:     [',RemotePrefix,']');
        {the following line depends on the order of names set up 
        in Initialize}
Writeln('Device:            [',DeviceNames[2+ord(FtpDev)],']');
Writeln('Baud:              [',BaudRate,']');
Writeln('Mode:              [',ModeNames[1+ord(FtpMode)],']');
Writeln('Remote host:       [',HisName,']');
if IsText then
    Write('Text/Binary:       [Text]')
else
    Write('Text/Binary:       [Binary]');
ChangeWindow(MainWindow);
End;

Procedure Initialize; 
{--------------------------------------------------------------------
{
{ Abstract:
{   Initialize the system.  Set up the Mode and Device arrays and the
{   local and remote filename prefixes.  Create Status and Main Window.
{
{--------------------------------------------------------------------}
const
    StatusHeight = 133;
    
(*
  {temporary kludge for f.240: }
    SBitWidth=768;
    SBitHeight=1024;
*)

Var
    SDum :String;
Begin
(*
  {temporary kludge for f.240: }
    writeln(Bell, '* Using constants for SBitWidth and SBitHeight');
    readln;
*)
    
    ScreenReset;
    SDum := ConCat('File Transfer Program. Version ',FTPVersion);
    CreateWindow(MainWindow, 0, StatusHeight+1, SBitWidth,
                   SBitHeight-StatusHeight-1, SDum);

    ModeNames[ModePerqIndex] := 'PERQ';
    ModeNames[ModePDP11Index] := 'PDP-11';
    ModeNames[ModeVaxIndex] := 'VAX';

  {Note that StrSetDev assumes that the only way a 
   device name can be non-unique is to be R or RS.}
  {The bizarre order of storing names is to make 
   correct the Status display and the help in procedure Device.}
    
    if Cf_RS232Ports = 2 then begin
       DeviceNames[DevRS232Index] := 'RS232  (same as RSA)';
       DeviceNames[DevRSAIndex] := 'RSA';
       DeviceNames[DevRSBIndex] := 'RSB';
    end
    else begin
       DeviceNames[DevRS232Index] := 'RSA    (same as RS)';
       DeviceNames[DevRSAIndex]   := 'RS232  (same as RS)';
       DeviceNames[DevRSBIndex]   := 'RS';
    end;
    DeviceNames[DevEtherIndex] := 'ETHERNET';
    DeviceNames[DevByteIndex] := 'BYTESTREAMNET';
    
    AddrSet := False;
    HisName := '';
    
    CreateWindow(StatWindow, 0, 0, SBitWidth, StatusHeight, 'FTP Status');
    DisplayStatus;
End;


procedure StrSetDev(Cmd: String);
{------------------------------------------------------------------------
{
{ Abstract:
{   Return the Device index that is associated with a name.
{
{ Parameters:
{   Cmd is the device name that we are looking for.
{
{ Results:
{   Return the device that was named by Cmd. 
{
{ Side Effects:
{   Set FTPDev.
{
{-------------------------------------------------------------------------}
Var Dum : Integer;
    OldDev: DevTypes;
    StatBlk: DevStatusBlock;

  Handler BadBaudRate;
  begin
     FTPError('Illegal Baud rate chosen: ', BaudRate, '', True);
     exit(StrSetDev);
  end;

Begin
    OldDev := FTPDev;
    CnvUpper(Cmd);
    If Debug then Writeln(' In StrSetDev. Cmd = ', Cmd:1);
    Dum:=UniqueCmdIndex(Cmd,DeviceNames,NumDev);
    Case Dum Of
        DevRS232Index: FtpDev := FTP_RSA;
        DevRSAIndex:   FtpDev := FTP_RSA;
        DevRSBIndex:   if CF_RS232Ports = 2 then
                           FtpDev := FTP_RSB
                       else
                           FtpDev := FTP_RSA;
        DevEtherIndex: FtpDev := FastEther;
        DevByteIndex:  FtpDev := EtherNet;
        NumDev+2:      begin  {not unique, must be R or RS}
                         FtpDev := FTP_RSA;
                         if CF_RS232Ports = 1 then
                             writeln(Bell, '* Assuming RS232 port')
                         else
                             writeln(Bell, '* Assuming RS232 port "RSA:"');
                       end;
        Otherwise: FTPError('Illegal value for DEVICE: "', CMD, '"', True);
    End {Case};
    
    With StatBlk Do Begin
        RSRcvEnable:=True;
        RSFill:=0;
        RSParity:=NoParity;
        RSStopBits:=Stop1;
        RSXmitBits:=Send8;
        RSRcvBits:=Rcv8;
        ByteCnt:=3;
    End;
    if FTPDev=FTP_RSA then begin 
        IOPutStatus(RSA,StatBlk);
        SetBaud(BaudRate, True);
    end
    else if FTPDev=FTP_RSB then begin
        IOPutStatus(RSB,StatBlk);
        SetBaud(BaudRate, True);
    end;
End;


 
Function StrToMode(Cmd: String) : TransMode;
{---------------------------------------------------------------------
{
{ Abstract:
{   Return the mode that is associated with a name.
{
{ Parameters:
{   Cmd is the name that we are to get the mode for.
{
{ Results:
{   Return the mode that is assoicated with Cmd.
{
{ Side Effects:
{   Sets FTPMode.
{
{---------------------------------------------------------------------}
Var Dum : Integer;
Begin
    CnvUpper(Cmd);
    If Debug then Writeln(' In StrToMode. Cmd = ', Cmd:1);
    Dum:=UniqueCmdIndex(Cmd,ModeNames,NumModes);
    Case Dum Of
        ModePerqIndex:  FTPMode:=PERQPERQ;
        ModePDP11Index: FTPMode:=PERQ11;
        ModeVaxIndex:   FTPMode:=PERQVAX;
        Otherwise: FTPError('Illegal value for MODE: "', CMD, '"', True);
    End {Case};
    StrToMode := FTPMode;
End;




Procedure DoSwitches(CalledFrom : String);
{--------------------------------------------------------------------
{ Abstract:
{   Process the switches that are on the command line.
{ Parameters:
{   Called from is the name of the routine that called this one.
{ Side Effects:
{   Set the global state assoicated with the switches.
{-----------------------------------------------------------------------}
Var CurSwitch : pSwitchRec;
    NonUnique, NonExistent : String;
    WhichSwitch : Integer;
    MachName: string;
    ConnReq: boolean;
    SwitchName : CString;
    NeedNewStatusDisplay: Boolean;
    
  label 6;
  Handler BadBaudRate;
  begin
     FTPError('Illegal baud rate: ', CurSwitch^.Arg, '', True);
     goto 6;
  end;

Begin
    JustHelp := False;
    IllegalSwitch := False;
    If Switches = Nil Then Exit(DoSwitches);
    If Switches^.Switch = '' Then Exit(DoSwitches);
    CurSwitch := Switches;
    ConnReq := false;
    NeedNewStatusDisplay := False;
    
    While CurSwitch <> Nil Do
    Begin
        SwitchName := CurSwitch^.Switch;
        NonUnique := ' is not a unique switch.';
        nonexistent := ' is an invalid switch.';
        WhichSwitch := PopUniqueCmdIndex(SwitchName, SwitchTable);
        
        If (CurSwitch^.Arg <> '')
           and (WhichSwitch In [1..NumSwitches])  
           and not (WhichSwitch in [BaudSwitch, DeviceSwitch, ModeSwitch,
                                ConnSwitch, RemoteSwitch, LocalSwitch])
         Then
            FTPError(SwitchName,' does not take any arguments.  Ignoring: ',
                        CurSwitch^.Arg, True)
        else
        
        Case WhichSwitch Of
        
            AskSwitch :  AskSingle := True;
                        
            NoAskSwitch: AskSingle := False;
         
            ConfSwitch : Confirm := True;
                        
            NoConfSwitch:Confirm := False;
        
            TextSwitch : Begin 
                             IsText := True;
                             NeedNewStatusDisplay := true;
                         End;
                        
            ConnSwitch : Begin 
                             ConnReq := true;
                             MachName := CurSwitch^.Arg;
                             NeedNewStatusDisplay := true;
                         End;

            NoTextSwitch:Begin
                             IsText := False;
                             NeedNewStatusDisplay := true;
                         End;
            
            HelpSwitch : JustHelp := True;
            
            ModeSwitch : Begin
                             NeedNewStatusDisplay := true;
                             FtpMode := StrToMode(CurSwitch^.Arg);
                         End;

            BaudSwitch : Begin  
                             if FTPDev in [FTP_RSA, FTP_RSB] then
                                SetBaud(CurSwitch^.Arg, True);
                             BaudRate := CurSwitch^.Arg;
                             NeedNewStatusDisplay := true;
                         End;

            DeviceSwitch:begin
                             NeedNewStatusDisplay := true;
                             StrSetDev(CurSwitch^.Arg);
                         End;

            LocalSwitch: begin
                             NeedNewStatusDisplay := true;
                             LocalPrefix := CurSwitch^.Arg;
                         End;

            RemoteSwitch:begin
                             NeedNewStatusDisplay := true;
                             RemotePrefix := CurSwitch^.Arg;
                         End;


            NumSwitches + 1: FTPError(SwitchName, NonExistent, '', True);
                             
            NumSwitches + 2: FTPError(SwitchName, NonUnique, '', True);
                             
            NumSwitches + 3: ;
            
            NumSwitches + 4: ;
            
            NumSwitches + 5 :FTPError('Illegal Character Found.','','',True);
                              
            OtherWise       :FTPError(SwitchName, NonExistent, '', True);
        End;   (* Case *)
 6:     CurSwitch := CurSwitch^.Next;
    End;
  
  if NeedNewStatusDisplay then
      DisplayStatus;

  if ConnReq then begin
    if (FTPDev <> FastEther) and (FTPDev <> EtherNet) then
        FTPError('CONNECT is only valid for Ethernet or ByteStream.', '', '',
            True)
    else begin
        HisName := MachName;
        repeat
            AddrSet := FTPAddRequest(HisName, FTPDev)
        until AddrSet or (not DoRetry);
        if not AddrSet then begin
           HisName := '';
           DisplayStatus;
        end;
        
    end;
  end;
End;



Procedure FirstSwitch(Line : string);
{---------------------------------------------------------------------
{ Abstract:
{   Processes a command line that has switches, but no command.
{---------------------------------------------------------------------}
Var InDummy, OutDummy : pArgRec;
    Error: string;
Begin
    Caller := '';
    InDummy := Nil;
    OutDummy := Nil;
    DstrySwitchRec(Switches);
    If ParseStringArgs(Line, InDummy, OutDummy, Switches, Error) Then begin
        If (InDummy^.Name <> '') Or (OutDummy^.Name <> '') Then
            FTPError('Invalid arguments ignored: ', InDummy^.Name, 
                       OutDummy^.Name, False);
    
        DoSwitches(Caller);
        if JustHelp then 
            Help;
    end
    Else 
         FTPError(Error, '', '', False);
    SaveSwitches;
    DstryArgRec(InDummy);
    DstryArgRec(OutDummy);
End;



Procedure RestoreSwitches;
{--------------------------------------------------------------------
{
{ Abstract:
{   Restore switch values to default state.
{
{---------------------------------------------------------------------}
var
    NeedDisplayStatus: Boolean;
  
  Handler BadBaudRate;
  begin
     FTPError('Illegal Baud rate chosen: ', HoldBaudRate, '', True);
     if NeedDisplayStatus then
        DisplayStatus;
     exit(RestoreSwitches);
  end;

Begin
    NeedDisplayStatus := 
           (IsText <> HoldIsText)
        or (FtpDev <> HoldDevice)
        or (FtpMode <> HoldMode)
        or (LocalPrefix <> HoldLocalPrefix)
        or (RemotePrefix <> HoldRemotePrefix)
        or (BaudRate <> HoldBaudRate);
   
    Confirm := HoldConfirm;
    AskSingle := HoldAskSingle;
    IsText := HoldIsText;
    FtpDev := HoldDevice;
    FtpMode := HoldMode;
    LocalPrefix := HoldLocalPrefix;
    RemotePrefix := HoldRemotePrefix;
    if (FTPDev in [FTP_RSA, FTP_RSB]) and (BaudRate <> HoldBaudRate) then
        SetBaud(HoldBaudRate, True);
    BaudRate := HoldBaudRate;
    
    if NeedDisplayStatus then
        DisplayStatus;
End;



Procedure SaveSwitches;
{--------------------------------------------------------------------
{ Abstract:
{   Store current switches values into default state.
{---------------------------------------------------------------------}
Begin
    HoldConfirm      := Confirm;
    HoldAskSingle    := AskSingle;
    HoldIsText       := IsText;
    HoldBaudRate     := BaudRate;
    HoldDevice       := FtpDev;
    HoldMode         := FtpMode;
    HoldLocalPrefix  := LocalPrefix;
    HoldRemotePrefix := RemotePrefix;
End;



Function Size(List : pArgRec) : Integer;
{--------------------------------------------------------
{ Abstract: 
{   Get the length of the ArgList.  (Dirk's Routine)
{
{ Parameters:
{   List if the argument list that we want the length of.
{
{ Results:
{   Return the length of List.
{ 
{---------------------------------------------------------}
Var I : Integer;
Begin
    I := 0;
    While List <> Nil Do
    Begin
        List := List^.Next;
        I := I + 1;
    End;
    Size := I;
End;



function GetInputs(Query : String): Boolean;
{--------------------------------------------------------
{ Abstract: 
{    Get The Input File.    (Dirk's Routine)
{
{ Parameters:
{   Query is the question that we will ask the user.
{
{ Side Effects:
{   Sets both InFiles and OutFiles.
{
{---------------------------------------------------------}
label 1;
Var Answer : String;
    Error: string;
Begin
1:  write(Query);
    ReadLn(Answer);
    GetInputs := (Answer<>'');
    DstryArgRec(InFiles);
    DstryArgRec(OutFiles);
    DstrySwitchRec(Switches);
    If Not ParseStringArgs(Answer, InFiles, OutFiles, Switches, Error) Then
    Begin
        FTPError(Error, '', '', False);
        GoTo 1;
    End;
End;



Function YesNo(Prompt : String): Boolean;
{--------------------------------------------------------
{ Abstract:
{   Ask a Yes or No question.
{
{ Parameters:
{   Prompt is the question that is to be asked.
{
{ Results:
{   return true if first char in input is 'y' or 'Y'
{   return false if firstchar in input is 'n' or 'N'
{
{---------------------------------------------------------}

Var Ans : String;
    Val : Integer;

Procedure Dummy;
Begin
End;

Begin
    While True Do
    Begin
        DstrySwitchRec(Switches);
        Val := GetConfirm(Dummy, True, Prompt, 3, switches);
        DoSwitches('');
        if JustHelp then begin
             writeln;
             writeln(HelpString);
        end;
        If Val = 1 then 
        Begin
            YesNo := True;
            Exit(YesNo);
        End;
        If Val = 2 Then 
        Begin
            YesNo := False;
            Exit(YesNo);
        End;
    End;
End;


Procedure FTPError(A,B,C: string;  WillContinue: Boolean);
{--------------------------------------------------------
{ Abstract:
{   Report an error message.  If DoRetry, ask before continuing.
{
{ Parameters:
{   A, B, C - Three strings that together comprise the error message.
{             They are 'var' only to save time on the call.
{   WillContinue - if true, prints a message that exec is continuing.
{---------------------------------------------------------}

Begin
    writeln(Bell, '** ', A, B, C);
    if DoRetry then begin
        write('** RETURN to continue.  Control-c to abort. ');
        readln;
    end
    else if WillContinue then
       writeln('** ...will continue by using the valid parts of the command');
End;


Function TransConf(Src, Dest : string): Boolean;
{----------------------------------------------------------
{ Abstract: 
{   Confirm the Transfer of srcfile to DestFile
{
{ Parameters:
{   Src is the name of the source file.
{
{   Dest is the name of the destination.
{
{ Results:
{   Return True if it is ok to do the transfer.
{   Return False otherwise.
{
{----------------------------------------------------------}
Var Ans : String;
    Val : Integer;
Begin
    Write('      Transfer ', Src:1, ' ==> ', Dest:1);
    HelpString := '      Do you really want to copy the file?';
    TransConf := YesNo(' (Yes or No): ');
End;


Function CheckFile(Fil: String): Boolean;
{----------------------------------------------------------
{ Abstract: 
{   If the file exists then ask if it should be deleted.
{
{ Parameters:
{   Fil is the name of the file that we are about to overwrite.
{
{ Results:
{   Return True if it is ok to overwrite the current file.
{   Return False otherwise.
{
{-----------------------------------------------------------}
Var pFile : FileId;
    Blks, Bits : Integer;
Begin
    CheckFile := True;
    pFile := FSLocalLookUp(Fil, Blks, Bits);
    HelpString := '        If you answer yes the old version of your file
            will be overwritten.';
    If pFile <> 0 Then 
    Begin
        Write('** File ', Fil:1,' already exists. OverWrite?');
        CheckFile := YesNo(' (Yes or No): ');
    End;
End;


Procedure Connect;
{---------------------------------------------------------------------
{
{ Abstract:
{   Procedure to implement the connect command.
{
{---------------------------------------------------------------------}
    Handler HelpKey(Var RetStr : Sys9s);
    Begin
        writeln('The connect command is used to obtain the address');
        writeln('of the remote machine.');
        Exit(Connect);
    End;

    Handler CtlC;
    Begin
        CtrlCPending := False;
        IOKeyClear; 
        QuitProgress;
        Exit(Connect);
    End;

    Handler CtlCAbort;
    Begin
        CtrlCPending := False;
        QuitProgress;
        Exit(Connect);
    End;

var 
    SDum: CString;
Begin
    Caller := 'CONNECT';
    DoSwitches(Caller);
    If JustHelp then begin
        writeln('  CONNECT [RemoteMachineName]');
        writeln('        Specify the name of the remote machine.');
        writeln('        This name must be one of the names in the ');
        writeln('        file >Ethernet.Names on that machine.');
        exit(Connect);
    end; 
    If IllegalSwitch Then Exit(Connect);
    
    if (FTPDev <> Ethernet) and (FTPDev <> FastEther) then
        begin
        FTPError('CONNECT is only valid for Ethernet devices.', '', '', False);
        exit(Connect);
        end;

    FTPSetMyAddr(FTPDev);

    If InFiles^.Name = '' Then begin
        SDum := concat('Name of the remote machine: [', 
                         concat(HisName, '] '));
        if not GetInputs(SDum)
            then exit(Connect);
    end;

    If Size(Infiles) <> 1 Then
        FTPError('There can be only one name for the remote machine.  Ignoring "',
            InFiles^.Next^.Name, '"', True);

    HisName := InFiles^.Name;
    repeat
        AddrSet := FTPAddRequest(HisName, FTPDev)
    until AddrSet or (not DoRetry);
    if not AddrSet then HisName := '';
    
    DisplayStatus;
End; 


Procedure GetF;
{-----------------------------------------------------------
{ Abstract: 
{   Get a file from Remote machine
{
{-----------------------------------------------------------}
Label 1, 2, 6;
Var pfile: FileID;
    pData: ptrFSDataEntry;
    TransComp: Boolean;
    ins, outs: pArgRec;
    SrcFile, DestFile: string;

Handler CtlC;
Begin
    CtrlCPending := False;
    IOKeyClear;
    QuitProgress;
    Exit(GetF);
End;

Handler CtlCAbort;
Begin
    CtrlCPending := False;
    QuitProgress;
    Exit(GetF);
End;

Begin
    Caller := 'GET';
    DoSwitches(Caller);
    If JustHelp then begin
        writeln('  GET [RemoteFileName(s)] [~ LocalFileName(s)]');
        writeln('        Retrieves files from the remote machine.');
        writeln('        If there is more than one file name, the');
        writeln('        lists must be separated by commas and be');
        writeln('        the same length.');
    end; 
    If IllegalSwitch or JustHelp Then 
       goto 1;
    
    If (Not AddrSet) And (FtpDev in [FastEther, EtherNet]) Then 
    Begin
      FTPError('First set the Addresses of the local and remote machines.',
                   '', '', False);
      Exit(GetF);
    End;

    If InFiles^.Name = '' Then 
    Begin
     6:
        if not GetInputs('File names on the remote machine: [Exit] ')
            then goto 1;
        DoSwitches(Caller);
        If JustHelp Then begin
            writeln('    Specify the name of the file you wish to get');
            writeln('    from the remote machine.');
            goto 6;
        end;
        If IllegalSwitch then GoTo 1;
    End;
    If OutFiles^.Name = '' Then DstryArgRec(OutFiles);
    
    If OutFiles <> Nil Then 
        If Size(Infiles) <> Size(OutFiles) Then 
        Begin
            FTPError('Number of Source and Destination files are unequal.',
                   '', '', False);
            goto 1;
        End;

    ins := InFiles;
    outs := OutFiles;
    While ins <> Nil Do
    Begin
        SrcFile := Concat(RemotePrefix,ins^.Name);
        If outs = Nil Then DestFile := Concat(LocalPrefix,ins^.Name)
        Else Begin
            DestFile := Concat(LocalPrefix,outs^.Name);
            outs := outs^.Next;
        End;

        If AskSingle Then If Not TransConf(SrcFile, DestFile) Then GoTo 2;
        If ConFirm Then If Not CheckFile(DestFile) Then GoTo 2;
            
        pfile := FSEnter(DestFile);
        IF pfile = 0 THEN
            FTPError(DestFile, ' is not a valid path or file name.', '', 
                   ins^.Next<>NIL)
        ELSE Begin
            TransComp := False;
            Repeat
                begin
                LoadCurs;
                TransComp:=FTPGetFile(SrcFile,DestFile,IsText,FtpDev,FTPMode);
                if not TransComp then 
                    WriteLn('** Could not get ',SrcFile, ' as ', DestFile);
                end;
            Until (TransComp) or (not DoRetry);
            
            new(pData);
            FSGetFSData(pfile, pData);
            if pData^.FileBlocks=0 then
                FSDelete(DestFile);
            dispose(pData);
        End;
2:      ins := ins^.Next;
    End;
1 : 
End;
    

Procedure PutF;
{-----------------------------------------------------------
{ Abstract: 
{   Put one or more files onto Remote machine.
{-----------------------------------------------------------}
Label 1, 2, 6;
Var 
    pfile: FileID;
    dum, Blocks: integer;
    TransComp: Boolean;
    ins, outs: pArgRec;
    SrcFile, DestFile: string;

Handler CtlC;
Begin
    CtrlCPending := False;
    IOKeyClear;
    QuitProgress;
    Exit(PutF);
End;

Handler CtlCAbort;
Begin
    CtrlCPending := False;
    QuitProgress;
    Exit(PutF);
End;

Begin
    Caller := 'PUT';
    DoSwitches(Caller);
    If JustHelp then begin
        writeln('  PUT [LocalFileName(s)] [~ RemoteFileName(s)]');
        writeln('        Stores files to the remote machine.');
        writeln('        If there is more than one file name, the');
        writeln('        lists must be separated by commas and be');
        writeln('        the same length.');
    end; 
    If IllegalSwitch or JustHelp Then 
       goto 1;
    
    If (Not AddrSet) And (FtpDev in [Ethernet, FastEther]) Then 
    Begin
      FTPError('First set the Addresses of the local and remote machines.',
            '', '', False);
      Exit(PutF);
    End;

    If InFiles^.Name = '' Then
    Begin
     6:
        if not GetInputs('Names of Local Files to be sent: [Exit] ') then
            goto 1;
        DoSwitches(Caller);
        If JustHelp Then begin
            writeln('    Specify the name of the file you wish to put');
            writeln('    to the remote machine.');
            goto 6;
        end;
        If IllegalSwitch Then GoTo 1;
    End;
    If OutFiles^.Name = '' Then DstryArgRec(OutFiles);
    
    If OutFiles <> Nil Then 
        If Size(InFiles) <> Size(OutFiles) Then
        Begin
            FTPError('Number of source and destination files are unequal.',
               '', '', False);
            goto 1;
        End;
   
    ins := InFiles;
    outs := OutFiles;
    While ins <> Nil Do
    Begin
        SrcFile := Concat(LocalPrefix,ins^.Name);
        If outs = nil then DestFile := Concat(RemotePrefix,ins^.Name)
        Else Begin
            DestFile := Concat(RemotePrefix,outs^.Name);
            outs := outs^.Next;
        End;
        
        pfile := FSLocalLookUp(SrcFile,Blocks,dum);
        IF pfile = 0 THEN
            FTPError('File ',SrcFile,' not found', ins^.Next<>NIL)
(*
        ELSE If Blocks = 0 then
            FTPError(SrcFile,' has 0 blocks in it.', '', ins^.Next<>NIL)
*)
        Else Begin
            If AskSingle Then If Not TransConf(SrcFile, DestFile) Then 
                GoTo 2;
            Repeat
               LoadCurs;
               TransComp := FTPPutFile(SrcFile,DestFile,IsText,FtpDev,FTPMode);
               If not transComp then 
                    Writeln('** Could not store ', SrcFile, ' as ', DestFile);
            Until (TransComp) or (not DoRetry);
        end;
2:      ins := ins^.Next;
    End;
1:
End;


Procedure FastPoll;
{----------------------------------------------------------------
{
{ Abstract: Is there something for us from other side.
{
{---------------------------------------------------------------}
Begin
    FTPChkDev(FTPDev);
End;


Procedure Poll;
{-----------------------------------------------------------
{
{ Abstract: Wait for a remote action
{
{-----------------------------------------------------------}
Label 1;
Var Ch: Char;
Begin
    Caller := 'POLL';
    DoSwitches(Caller);
    if JustHelp then 
        writeln('   POLL - Makes this Perq wait for commands from another.');
    If IllegalSwitch or JustHelp then 
       goto 1;
    If (InFiles^.Name <> '') Or (OutFiles^.Name <> '') Then
        FTPError('Invalid arguments ignored: ', InFiles^.Name, OutFiles^.Name,
                     True);
    if LocalPrefix<>'' then
        writeln('*  Local prefix "', LocalPrefix, '" will be ignored');
    if RemotePrefix<>'' then
        writeln('*  Remote prefix "', RemotePrefix, '" will be ignored');
    if (FTPDev = Ethernet) or (FTPDev = FastEther) then FTPSetMyAddr(FTPDev);
    WriteLn('Type any character other than RETURN to exit poll mode.');
    repeat
      While IOCRead(TransKey,Ch) <> IOEIOC Do
        FTPChkDev(FTPDev);
    until chr(LAnd(ord(ch),#177))<>CH_Return;
    Writeln;
1: 
End;
    

Procedure Quit;
{----------------------------------------------------------
{ Abstract:
{   The Quit command.
{
{ Side Effects:
{   If we should quit then set Finished.
{
{-----------------------------------------------------------}
Begin
    Caller := '';
    DoSwitches(Caller);
    if JustHelp then 
        writeln('   QUIT - Exits from FTP.');
    If IllegalSwitch Or JustHelp Then 
        Finished := False 
    Else 
        Finished := True;
    If Finished And (FTPMODE = PERQVAX) then SendStopVax;
    If Finished then FTPQuitNet;
End;
    

Procedure SetAsk(Val : Boolean);
{--------------------------------------------------------
{ Abstract: 
{   Sets the Global Variable AskSingle To the Given Value.
{   The Value is set in the Global Context.
{
{ Parameters:
{   Val is the default value
{
{---------------------------------------------------------}
Begin
    If Val Then Caller := 'ASK' Else Caller := 'NOASK';
    DoSwitches(Caller);
    If JustHelp then begin
        If Val  Then begin
            writeln('  ASK - When this switch is set, FTP will prompt');
            writeln('        for permission before each file transfer.');
        end
        Else begin
            writeln('  NOASK - When this switch is set, FTP will NOT prompt');
            writeln('          for permission before each file transfer.');
        end;
        exit(SetAsk);
    end; 
    if IllegalSwitch then exit(SetAsk);
    
    If (InFiles^.Name <> '') Or (OutFiles^.Name <> '') Then
        FTPError('Invalid arguments ignored: ', InFiles^.Name, OutFiles^.Name,
               False);
    
    HoldAskSingle := Val;
    AskSingle := Val;
End;


Procedure SetConfirm(Val : Boolean);
{--------------------------------------------------------
{ Abstract: 
{   Sets the Global Variable Confirm To the Given Value.
{   The Value is set in the Global Context.
{
{ Parameters:
{   Val is the default value
{
{---------------------------------------------------------}
Begin
    If Val Then Caller := 'CONFIRM' Else Caller := 'NOCONFIRM';
    DoSwitches(Caller);
    If JustHelp then begin
        If Val  Then begin
            writeln('  CONFIRM - When this switch is set,  FTP will prompt');
            writeln('        for permission before overwriting an existing');
            writeln('        file on this Perq.  Caution:');
            writeln('        ** DOES NOT APPLY to the Perq doing a POLL.');
        end
        Else begin
            writeln('  NOCONFIRM - When this switch is set, FTP will NOT ');
            writeln('        prompt for permission before overwriting an ');
            writeln('        existing file.');
        end;
        exit(SetConfirm);
    end; 
    If IllegalSwitch then exit(SetConfirm);
    
    If (InFiles^.Name <> '') Or (OutFiles^.Name <> '') Then
        FTPError('Invalid arguments ignored: ', InFiles^.Name, OutFiles^.Name,
               True);
    
    HoldConfirm := Val;
    Confirm := Val;
End;


Procedure SetText(Val : Boolean);
{--------------------------------------------------------
{ Abstract: 
{   Sets the Global Variable IsText To the Given Value.
{   The Value is set in the Global Context.
{
{ Parameters:
{   Val is the default value
{
{---------------------------------------------------------}
Begin
    If Val Then Caller := 'TEXT' Else Caller := 'BINARY';
    DoSwitches(Caller);
    If JustHelp then begin
        If Val  Then begin
            writeln('   TEXT - When this switch is set, special line');
            writeln('        delimiters are used.  For general files');
            writeln('        use the BINARY switch.');
        end
        Else begin
            writeln('    BINARY - When this switch is set, no special');
            writeln('        processing is done to line endings.  For');
            writeln('        special processing, use the TEXT switch.');
        end;
        exit(SetText);
    end; 
    If IllegalSwitch then exit(SetText);
    
    If (InFiles^.Name <> '') Or (OutFiles^.Name <> '') Then
        FTPError('Invalid arguments ignored: ', InFiles^.Name, OutFiles^.Name,
            True);
    
    IsText := Val;
    HoldIsText := Val;
    DisplayStatus;
End;


Procedure Mode;
{-----------------------------------------------------------
{ Abstract: Change the mode
{-----------------------------------------------------------}
Label 1,2;
var i, Dum :integer;
    Cmd : String;
    HDum: Sys9s;

Handler HelpKey(Var RetStr : Sys9s);
Begin
    Writeln;
    Writeln('MODE sets the type of the remote machine.  This type');
    Writeln('affects the delay between sending RS232 bytes and a');
    Writeln('few other minor wrinkles.  Valid types are:');
    For Dum := 1 To NumModes Do Writeln('     ', ModeNames[Dum]);
    Writeln;
    Writeln('* The present remote machine is ', ModeNames[1+ord(FtpMode)]:1, '.');
    Exit(Mode);
End;


Begin
    Caller := '';
     
    DoSwitches(Caller);
    If JustHelp Then 
        raise HelpKey(HDum);
    If IllegalSwitch then Exit(Mode);
           
    If Infiles <> Nil Then
    Begin
        Cmd := Infiles^.Name;
        If Infiles^.Next <> Nil Then 
            FTPError('Only one parameter allowed for MODE.  Ignoring "',
                  InFIles^.Next^.Name, '"', True);
        If Cmd <> '' Then GoTo 1;
    End;

 2:
    Writeln('Present remote machine expected is ',ModeNames[1+ord(FtpMode)]:1);
    Write('Type of remote machine [Perq]: ');
    ReadLn(Cmd);
    If Cmd = '' Then Cmd := 'PERQ';

 1:
    CnvUpper(Cmd);
    FTPMode := StrToMode(Cmd);

    if (FTPMode = PERQVAX) and (FTPDEV = FASTETHER) then
        begin
        writeln(Bell, '* Setting device to ByteStream to transfer to VAX.');
        FTPDev := EtherNet;
        end;

    HoldMode := FTPMode;
    DisplayStatus;
End;


Procedure Device;
{----------------------------------------------------------------
{ Abstract: Establish the type of device used in the transfer.
{
{---------------------------------------------------------------}
Label 1,2;
Var I, Dum : Integer;
    Cmd : String;
    HDum: Sys9s;

Handler HelpKey(Var RetStr: Sys9s);
Begin
    Writeln;
    Writeln;
    Writeln('        What device will be used for the transfer?');
    Writeln('        Valid Devices are:');
    For Dum := 1 To NumDev Do Writeln('          ', DeviceNames[Dum]);
    Writeln;
    Writeln('* Present Device is ',DeviceNames[2 + Ord(FtpDev)]:1);
    Exit(Device);
End;
    
Begin
    Caller := '';
    HelpString := '        The DEVICE Command establishes the type of device
            used for the transfer.';

    DoSwitches(Caller);
    If JustHelp Then
        raise HelpKey(HDum);
    If IllegalSwitch Then Exit(Device);
    
    If Infiles <> Nil Then 
    Begin
        Cmd := Infiles^.Name;
        If Infiles^.Next <> Nil Then
            FTPError('Only one parameter allowed for DEVICE.  Ignoring "',
                       Infiles^.Next^.Name, '"', True);
        If Cmd <> '' Then GoTo 1;
    End;
 2:
    Writeln('Present communication device is ', DeviceNames[1+Ord(FtpDev)]:1);
    Write('Type of device desired [EtherNet]: ');
    ReadLn(Cmd);
    If Cmd = '' Then Cmd := 'ETHERNET';

 1:
    CnvUpper(Cmd);
    StrSetDev(Cmd);

    if (FTPMode = PERQVAX) and (FTPDEV = FASTETHER) then
        begin
        writeln(Bell, '* Setting device to ByteStream to transfer to VAX.');
        FTPDev := EtherNet;
        end;

    HoldDevice := FTPDev;
    DisplayStatus;
End;


procedure Baud;
{-----------------------------------------------------------
{ Abstract: Change the baud rate.
{ ChangeLog: 4 Jan 81 New User Interface.   (S Johar)
{-----------------------------------------------------------}
Label 1,2;
Var 
    Answer : String;
    Cmd : String;
    OldBaudRate : String;
    HDum: Sys9s;
    
  Handler BadBaudRate;
  begin
     FTPError('Illegal parameter for command BAUD: ', BaudRate, '', False);
     BaudRate := OldBaudRate;
     exit(Baud);
  end;


  Handler HelpKey(var RetStr : Sys9s);
  Begin
      Writeln;
      Writeln;
      Writeln('Baud Rate is the Bit rate at which the FTP Transfer will ');
      Write  ('be done. Valid rates are 300, 1200, 2400, 4800, 9600');
      if Cf_RS232MaxSpeed>8 then writeln(', 19200')
      else writeln;
      Writeln;
      Writeln;
      Writeln('* Present Baud Rate is ', BaudRate:1);
      Exit(Baud);
  End;     
  
 begin
    OldBaudRate := BaudRate; 
    Caller := '';
    HelpString := 
        '      The BAUD Command establishes the bit rate for the transfer.';
     
    DoSwitches(Caller);
    If JustHelp Then 
        raise HelpKey(HDum);
    If IllegalSwitch  then Exit(Baud);
           
    If Infiles <> Nil Then
    Begin
        Cmd := Infiles^.Name;
        If Infiles^.Next <> Nil Then 
            FTPError('Only one parameter allowed for BAUD.  Ignoring "',
                  InFiles^.Next^.Name, '"', True);
        If Cmd <> '' Then 
        Begin
            BaudRate := Cmd;
            GoTo 1;
        End;
    End;

2:     
     Writeln('Present Baud Rate =', BaudRate:1);
     Write('New Baud Rate[',DefBaudRate:1,']: ');
     Readln(answer);
     If AnsWer = '' Then BaudRate := DefBaudRate Else BaudRate := Answer;

1:
     if FTPDev in [FTP_RSA, FTP_RSB] then
         SetBaud(BaudRate, TRUE);
     HoldBaudRate := BaudRate;
     DisplayStatus;
 End;



Procedure SetRemotePrefix;
{-----------------------------------------------------------
{
{ Abstract:
{
{    Set the prefix for the remote pathname.
{
{----------------------------------------------------------}


    Handler HelpKey(Var RetStr : Sys9s);
    Begin
        writeln('The RemotePrefix command sets a prefix for the names');
        writeln('of files on the remote machine.');
        Exit(SetRemotePrefix);
    End;

    Handler CtlC;
    Begin
        CtrlCPending := False;
        IOKeyClear;
        Exit(SetRemotePrefix);
    End;

    Handler CtlCAbort;
    Begin
        CtrlCPending := False;
        Exit(SetRemotePrefix);
    End;

Begin
    HelpString := 
        '      REMOTEPREFIX  <RemotePathName>';

    If (InFiles^.Name = '') 
            and not DoRetry {i.e., not in cmd file  HACK!!!} Then 
        if not GetInputs('Prefix to use for remote pathname: [none] ') then 
            {Name is already = ''};

    If Size(Infiles) <> 1 Then
        FTPError('There can be only one prefix for the remote pathname.  Ignoring "',  InFiles^.Next^.Name, '"', True);

    RemotePrefix := InFiles^.Name; 
    HoldRemotePrefix := RemotePrefix; 
    DisplayStatus;

    if length(RemotePrefix)>0 then
        if RemotePrefix[length(RemotePrefix)] <> '>' then
            writeln(Bell, '*  Usually a prefix should end with a ">"');
End; 


Procedure SetLocalPrefix;
{-----------------------------------------------------------   
{
{ Abstract:
{
{   Set the prefix for the local pathname.
{
{----------------------------------------------------------}

    Handler HelpKey(Var RetStr : Sys9s);
    Begin
        writeln('The LocalPrefix command sets a prefix for the names');
        writeln('of files on the local machine.');
        Exit(SetLocalPrefix);
    End;

    Handler CtlC;
    Begin
        CtrlCPending := False;
        IOKeyClear;
        Exit(SetLocalPrefix);
    End;

    Handler CtlCAbort;
    Begin
        CtrlCPending := False;
        Exit(SetLocalPrefix);
    End;

Begin
    HelpString := 
        '      LOCALPREFIX  <LocalPathName>';

    If (InFiles^.Name = '') 
            and not DoRetry {i.e., not in cmd file  HACK!!!} Then 
        if not GetInputs('Prefix to use for local pathname: [none] ') then 
            {Name is already = ''};

    If Size(Infiles) <> 1 Then
        FTPError('There can be only one prefix for the local pathname.  Ignoring "',  InFiles^.Next^.Name, '"', True);

    LocalPrefix := InFiles^.Name; 
    HoldLocalPrefix := LocalPrefix; 
    DisplayStatus;

    if length(LocalPrefix)>0 then
        if LocalPrefix[length(LocalPrefix)] <> '>' then
            writeln(Bell, '*  Usually a prefix should end with a ">"');
End.
