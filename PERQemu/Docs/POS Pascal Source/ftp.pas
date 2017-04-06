{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Ftp;

{---------------------------------------------------------------------
{
{ ABSTRACT
{
{   This file contains the main program for the FTP Utility.
{   The commands are actually implemented in the file ftpuser.pas
{   which is imported by this program.
{
{ Written By: Sandeep Johar
{
{ CopyRight (C) Three Rivers Corporation, 1982, 1983.
{
{--------------------------------------------------------------------}

{$Version V7.9 for POS}
{--------------------------------------------------------------------}
{ Change Log:
{
{  15 Mar 83  V7.9  WJHansen
{    Don't call DisplayStatus in mainloop.
{    Call RestoreSwitches in mainloop.
{    Set OnCmdLine only if it is a valid command or FTP/HELP.
{    Call FTPError for parsing errors.
{    Fix so can read switches on command line.
{  
{   7 Mar 1983   V7.8  WJHansen
{    Import IO_Unit and IO_Others.
{
{  28 Feb 1983   V7.7  WJHansen 
{    Remove MPOS stuff.
{    Remove E10ReceiveDone handler.  (Use no-interrupt version of ethernet.)
{    Implement RemotePrefix and LocalPrefix switches.
{    Read switches from profile.
{    Install call on ShowMemUsage.
{    Synchronize version number with FTPVersion & do dollar-Version.
{    Don't exit if initial command line starts with switches.
{    Move some variables from global to FTPUser to local here.
{    Make CommandName a CString.
{    Move all switch initialization to SetInitialState.
{
{      15 Nov 82   D Scelza   V6.10      Changed default device to be 
{                                        Ethernet.
{
{      28 Oct 82   D Kalp     V6.9       1. Import new module FTPVersion and 
{                                           use it to get the version number.
{                                        2. Make calls to DisplayStatus more 
{                                           precise.
{                                        3. Use procedure FixupWindows instead
{                                           of ScreenReset on exit.
{                                        4. Change placement of DstryArgRec,
{                                           etc.
{
{      19 Oct 82   D Scelza   V6.8       Bug fixes in FTPUtils.
{                                        Fixed File-Not-Found on polling 
{                                        machine bug.
{                                        Resolved the 14-char identifier 
{                                        problem.
{
{      16 Sep 82  M Kristofic V6.7       Fix handling of garbage user
{                                        commands (per EMC)
{
{      19 Jul 82   D Scelza   V6.6       Put in code to release switches
{                                        and input and output files.
{
{      15 Jul 82   D Scelza   V6.5       First release of multi receive.
{
{      14 Jun 82   D Scelza   V5.3       Put in code to do a Quit if
{                                        command file executed from 
{                                        command line.
{
{      22 Apr 82   D Scelza   V5.2       Put in code to shut down the
{                                        net on ^C exits;
{
{      21 Apr 82   M Faust    V5.1       Status window, and local and remote
{                                        file prefixes.
{
{      20 Apr 82   D Scelza   V5.0       New commands and bug fixes.
{
{      14 Apr 82   D Scelza   V4.93      Put in ^C handlers.
{
{      22 Mar 82   S Johar    V4.92      Cleanup the headers, version nums etc
{
{      11 Mar 82   S Johar    V4.91      Catch the DevNotFree Exception.
{
{       9 Mar 82   S Johar    V4.9       Grab the RS232 Device.
{
{       9 Mar 82   S johar    V4.8       Do the right thing when EtherVer 
{                                        is Off. Put in standard format.
{
{      26 Feb 82   S Johar   V4.7        Link as device removed.
{
{      19 Feb 82   S Johar   V4.6        Put in Retry if from a command file.
{
{      17 Feb 82   S Johar    V4.5       Set baud rate default to 9600.
{                                        renamed 'Notext' to 'binary'.
{                                        Set IsText to False at start up.
{                                        Bring Poll mode back. Linkdev on.
{
{      12 Feb 82   S Johar    V4.4       Fixed the Addressing Bug. 
{                                        Ethernet On.
{
{       2 Feb 82    S Johar   V4.3       Fixes of Bugs found in Interface.
{
{      21-Jan-82  S Johar     V4.2       Include EtherNet as a device, 
{                                        as a conditional option.
{
{      19-Jan-82  S Johar     V4.1       Catch The HelpKey Exception whenever 
{                                        reading in values for mode, device,
{                                        baud etc.
{
{      19 Jan 82    S Johar   V4.0       Released for D6.
{
{      15 Jan 82    S johar   V3.3       Device, Mode and Baud made into
{                                        local switches in addition to being
{                                        global commands. This Version Corr to
{                                        V3.3 of ftpuser.pas.
{
{      15 Jan 82    S johar   V3.2       Polling moved to background.
{
{       6 Jan 82    S Johar   V3.1       Created.
{---------------------------------------------------------------------}

Imports FTPVersion from FTPVersion;
Imports ftputils from ftputils;
Imports ftpuser from ftpuser;

Imports Perq_string From Perq_string;
Imports CmdParse From CmdParse;
Imports PopCmdParse From PopCmdParse;

Imports FileUtils from fileutils;         {for FSAddToTitleLine}
imports IO_Unit from IO_Unit;             {for RSxxx}
imports IO_Others from IO_Others;         {for IOKeyClear}
Imports System From System;               {for CurPFile}
imports Configuration from Configuration; {for Cf_RS232MaxSpeed}
imports Profile from Profile;

Const GetIndex = 1;
      PutIndex = GetIndex + 1;
      ModeIndex = PutIndex + 1;
      BaudIndex = ModeIndex + 1;
      PollIndex = BaudIndex + 1;
      AskIndex = PollIndex + 1;
      HelpIndex = AskIndex + 1;
      ConnIndex = HelpIndex + 1;
      NoAskIndex = ConnIndex + 1;
      DeviceIndex = NoAskIndex + 1;
      ConfIndex = DeviceIndex + 1;
      NoConfIndex = ConfIndex + 1;
      TextIndex = NoConfIndex + 1;
      NoTextIndex = TextIndex + 1;
      QuitIndex = NoTextIndex + 1;
      LocalIndex = QuitIndex + 1;
      RemoteIndex = LocalIndex + 1;
      
      Debug = False;
     

var
    NonUnique, NonExistent: string;     
      
Procedure SetInitialState;
{----------------------------------------------------------------
 Abstract : Initialize the program.
-----------------------------------------------------------------}
Begin
    NonUnique := ' is not unique. Type HELP if you need it.';
    NonExistent := ' is an invalid command. Type HELP if you need it.';
    TitleLine := Concat( Concat ('FTP ', FTPVersion),
                         '   Type HELP if you need it.');
                             
    InFiles := Nil;
    OutFiles := Nil;
    Switches := Nil;

(*
{ Initialize Configuration module.  Not needed in F.237 and later. }
    writeln('Calling Cf_Init');
    if Cf_Init then {nothing};
*)

    case Cf_RS232MaxSpeed of
       RSExt: DefBaudRate := '110';
       RS110: DefBaudRate := '110';
       RS150: DefBaudRate := '150';
       RS300: DefBaudRate := '300';
       RS600: DefBaudRate := '600';
       RS1200: DefBaudRate := '1200';
       RS2400: DefBaudRate := '2400';
       RS4800: DefBaudRate := '4800';
       RS9600: DefBaudRate := '9600';
       RS19200: DefBaudRate := '19200';
       otherwise: DefBaudRate := '9600';
    end;

    BaudRate := DefBaudRate;
    HoldBaudRate := DefBaudRate;
    FTPDev := FastEther;
         {If FTPDev is set to FTP_RSA or FTP_RSB, 
           IOPutStatus and SetBaudRate will have to be done somewhere below.}
    HoldDevice := FastEther;
    FTPMode := PERQPERQ;
    HoldMode := PerqPerq;
    
    IsText := False;
    HoldIsText := False;
    ConFirm := True;
    HoldConfirm := True;
    AskSingle := False;
    HoldAskSingle := False;
    
    HisName := '';
    LocalPrefix := '';
    RemotePrefix := '';

    AddrSet := False;
End;


Procedure SetCmdTable;
{-----------------------------------------------------------------
 Abstract: Initialize command Table.
------------------------------------------------------------------}
Begin

{$R-}
    CmdTable^.Header := 'Ftp CMD';
    CmdTable^.Commands[GetIndex]     := 'GET';
    CmdTable^.Commands[PutIndex]     := 'PUT';
    CmdTable^.Commands[ModeIndex]    := 'MODE';
    CmdTable^.Commands[BaudIndex]    := 'BAUD';
    CmdTable^.Commands[PollIndex]    := 'POLL';
    CmdTable^.Commands[HelpIndex]    := 'HELP';
    CmdTable^.Commands[ConnIndex]    := 'CONNECT';
    CmdTable^.Commands[DeviceIndex]  := 'DEVICE';
    CmdTable^.Commands[AskIndex]     := 'ASK';
    CmdTable^.Commands[NoAskIndex]   := 'NOASK';
    CmdTable^.Commands[ConfIndex]    := 'CONFIRM';
    CmdTable^.Commands[NoConfIndex]  := 'NOCONFIRM';
    CmdTable^.Commands[TextIndex]    := 'TEXT';
    CmdTable^.Commands[NoTextIndex]  := 'BINARY';
    CmdTable^.Commands[QuitIndex]    := 'QUIT';
    CmdTable^.Commands[LocalIndex]   := 'LOCALPREFIX';
    CmdTable^.Commands[RemoteIndex]  := 'REMOTEPREFIX';
    
    If debug then writeln('QuitIndex = ', QuitIndex);
{$R=}

End;


Procedure SetSwitchTable;
{----------------------------------------------------------------
 Abstract: Initailize switch table.
-----------------------------------------------------------------}
Begin

{$R-}
    SwitchTable^.Header := '';
    
    With SwitchTable^ Do
    Begin
        Commands[AskSwitch]      := 'ASK';
        Commands[NoAskSwitch]    := 'NOASK';
        Commands[ConfSwitch]     := 'CONFIRM';
        Commands[NoConfSwitch]   := 'NOCONFIRM';
        Commands[TextSwitch]     := 'TEXT';
        Commands[NoTextSwitch]   := 'BINARY';
        Commands[DeviceSwitch]   := 'DEVICE';
        Commands[BaudSwitch]     := 'BAUD';
        Commands[ModeSwitch]     := 'MODE';
        Commands[HelpSwitch]     := 'HELP';
        Commands[ConnSwitch]     := 'CONNECT';
        Commands[RemoteSwitch]   := 'REMOTEPREFIX';
        Commands[LocalSwitch]    := 'LOCALPREFIX';
    End;
{$R=}
End;


Handler CtlC;
Begin
    CtrlCPending := False;
    IOKeyClear;
    Quit;
    If Finished Then 
        Begin
        FixupWindows;
        FTPQuitNet;
        Exit(ftp);
        End;
End;

Handler CtlCAbort;
Begin
    CtrlCPending := False;
    IOKeyClear;
    Quit;
    If Finished Then 
        Begin
        FixupWindows;
        FTPQuitNet;
        Exit(ftp);
        End;
End;

Handler CtlShftC;
Begin
    CtrlCPending := False;
    IOKeyClear;
    Quit;
    If Finished Then 
        Begin
        FTPQuitNet;
        FixupWindows;
        Exit(ftp);
        End;
End;

label 6;
handler PNotFound(FName: String);
begin
   goto 6;
end;


{ The Main Program }

Label 1;

Var 
    CmdLine, CommandName: CString;
    OnCmdLine, FirstPress:Boolean;
    FTPCmdFile: pCmdList;
    I : Integer;
    SWs: ProfStr;
    Error: string;
Begin

    SetInitialState;   {set up FTP and initial switch values}
    Initialize;        {set up FTPUser}
    FTPInit;           {set up FTPUtils}
    
    AllocNameDesc(NumCmds, 0, CmdTable);
    AllocNameDesc(NumSwitches, 0, SwitchTable);
    
    SetCmdTable;
    SetSwitchTable;
    
    InitCmdFile(FtpCmdFile, 0);
    OnCmdLine := False;
    CmdLine := '';
    
    FSAddToTitleLine(TitleLine);
    
    {Read switches from Profile}
    DoRetry := True;  {require interaction to continue}
    HelpString := 'HELP requested in profile.';
    PFileInit(CurPFile, 'FTP');
    SWs := PFileEntry;
    if SWs <> '' then 
       writeln('Processing switches from ', CurPFile);
    while SWs <> '' do begin
       writeln(' ':6, SWs);
       If Not ParseStringArgs(SWs,InFiles,OutFiles, Switches, Error) Then
           FTPError(CurPFile, ' has error: ', Error, True);
       if InFiles^.Name <> '' then 
           FTPError('Ignoring non-switch text from profile: "',
                           InFiles^.Name, '"', True);
       if OutFiles^.Name <> '' then 
           FTPError('Ignoring non-switch text from profile: "', 
                           OutFiles^.Name, '"', True);
       DoSwitches('PROFILE');
       DstryArgRec(InFiles);
       DstryArgRec(OutFiles);
       DstrySwitchRec(Switches);
       SWs := PFileEntry;
    end;
    SaveSwitches;    
    
6:
    ShowMemUsage;
    
    I := GetShellCmdLine(CommandName, FtpCmdFile, CmdTable);
    If Debug then Writeln('Shell parse :', I:1, 'Of ', NumCmds);
    If (I <= NumCmds) Or (I = NumCmds+4) 
             Or (FtpCmdFile^.Next <> Nil) Then 
        OnCmdLine := True;
    If (I >= 1) And (I <= NumCmds) Then 
        If Not ParseCmdArgs(InFiles, OutFiles, Switches, Error) Then
        Begin
            FTPError(Error, '', '', False);
            GoTo 1;
        End;
    CmdLine := Concat('=', UsrCmdLine);

    FirstPress := True;
    While True Do
    Begin
        If IsText and Debug Then Writeln('TextMode');
        If OnCmdLine Then DoRetry := True Else DoRetry := FTPCmdFile^.Next<>Nil;
        Case I Of
        
            { All Of these routines can be found in FtpUser.pas }
            
            GetIndex :      GetF;
            PollIndex :     Poll;
            PutIndex :      PutF;
            ModeIndex:      Mode;
            BaudIndex:      Baud;
            HelpIndex:      Help;
            AskIndex:       SetAsk(True);
            NoAskIndex:     SetAsk(False);
            ConfIndex:      SetConFirm(True);
            NoConfIndex:    SetConFirm(False);
            TextIndex :     SetText(True);
            NoTextIndex:    SetText(False);
            ConnIndex:      Connect;
            DeviceIndex:    Device;
            QuitIndex :     Begin
                                Quit;
                                If Finished Then
                                    Begin
                                    FixupWindows;
                                    Exit(ftp);
                                    End;
                            End;
            LocalIndex:     SetLocalPrefix;
            RemoteIndex:    SetRemotePrefix;

            NumCmds + 1:    FTPError(CommandName, NonExistent, '', False);
            NumCmds + 2:    FTPError(CommandName, NonUnique, '', False);
            NumCmds + 3:    ;
            NumCmds + 4:    Begin
                                CommandName := Concat('/', CommandName);
                                CmdLine := Concat(CommandName, CmdLine);
                                If Debug then Writeln('FirstSwitch: ',CmdLine);
                                FirstSwitch(CmdLine);
                                if not JustHelp then
                                    OnCmdLine := False;
                            End;
            NumCmds + 5:    FTPError('Illegal character just after ',
                                CommandName, '', False);
            OtherWise:      FTPError(CommandName, NonExistent, '', False);
        End; (* Case *)

        If (OnCmdLine) And (FtpCmdFile^.Next = Nil) Then
        Begin
            Switches := nil;
            Quit;
            DestroyNameDesc(CmdTable);
            DestroyNameDesc(SwitchTable);
            FixupWindows;
            Exit(Ftp);
        End;
        
        RestoreSwitches;
        
1:

        DstryArgRec(InFiles);
        DstryArgRec(OutFiles);
        DstrySwitchRec(Switches);
              
        Writeln;
        I := GetCmdLine(NullIdleProc,'FTP', CmdLine, CommandName, FtpCmdFile, 
                        CmdTable, FirstPress, True);
        If Debug then Writeln('Val returned = ', I:1, ' of ', NumCmds:1);        
        If (I>=1) and (I <=NumCmds) Then
            If Not ParseStringArgs(CmdLine,InFiles,OutFiles, Switches, Error)
            Then Begin
                FTPError(Error, '', '', False);
                GoTo 1;
            End;
    End;
End.
