{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program SetTime;
{------------------------------------------------------------------------------
{ Abstract:
{
{   Allows the user to set the system date and time.  Format is exactly the
{   same as that of Login.
{
{ Written by: Mark G. Faust
{ Entirely rewritten by Brad Myers  4 Jan 82
{
{ Copyright (C) 1981, Three Rivers Computer Corporation.
{-----------------------------------------------------------------------------}
                   
{------------------------------------------------------------------------------
{ Change Log:
{
{  1-Sep-83  V3.3  Scott Brown
{ Handle exception raised in GetPERQ2Local due to a bad clock on the EIO board.
{ 
{ 21-Apr-83  V3.2  Don Scelza
{ Initialized BadSwitch.  Should fix "Do nothing" bug.
{
{ 29-Mar-83  V3.1  Don Scelza
{ Fixed a bug.
{
{  4-Mar-83  V3.0  Don Scelza
{ Made changes for G.2 and PERQ2.
{
{  4-Jan-82  V2.0  Brad Myers
{ ReWritten from new Login.
{
{ 19-Sep-81  V1.0  Mark G. Faust
{ Program written.
{-----------------------------------------------------------------------------}

imports    System from System;
imports    CmdParse from CmdParse;
imports    Clock from Clock;
imports    PERQ_String from PERQ_String;
imports    FileUtils from FileUtils;
imports    EtherTime from EtherTime;
imports    Configuration from Configuration;
imports    Except from Except;

                                 
const 
    Title = 'SetTime V3.3.  Type /Help for Help';
    HelpSwitch = 1;
    UserSwitch = HelpSwitch + 1;
    ServerSwitch = UserSwitch + 1;
    EIOSwitch = ServerSwitch + 1;
    SetOffSwitch = EIOSwitch + 1;
    NumSwitches = SetOffSwitch;
    

Exception Impossible;

var TimeIn, OldTime: string;
    TStamp: TimeStamp;
    s: CString;
    c: Char;
    i: integer;
    isSwitch: boolean;
    HoldCmdline:        String[255];
    Cmd, Switch:        integer;
    SwitchP, Switches:  pSwitchRec;
    Inputs, Outputs:    pArgRec;
    InP, OutP:          pArgRec;
    TmpArg:             pArgRec;
    CmdLine, CanCommand:String;
    InF:                pCmdList; 
    Broke:              String;
    FirstPress:         boolean;
    ErrStr:             string;
    BadSwitch:          boolean;
    SwitchTab:          CmdArray;
    WhereFrom:          (Server, User, EIO);
    SetOff:             boolean;
    SDum:               string;

label 1;

Handler BadTime;
   begin
   WriteLn('** Illegal time string.  Type SetTime/Help for format.');
   Exit(SetTime);
   end;

Procedure Help;
  begin
  WriteLn;
  WriteLn('     This program is used to set the time for the PERQ.');
  writeln;
  writeln('     The new time can come from one of three sources:');
  writeln('         User specified');
  writeln('         Network Time Server');
  writeln('         EIO Clock (PERQ-2 only)');
  writeln;
  writeln('     The valid switches are:');
  writeln('         /User          - Ask the user (default).');
  writeln('         /Server        - Get the time from the network time server.');
  writeln('         /EIOBoard      - Get the local time from the EIO clock.');
  writeln('         /SetGMTOffset  - Set the Local to GMT offset.');
  writeln;
  WriteLn('     The format for the time is: ');
  WriteLn('         DD MMM YY HH:MM:SS');
  WriteLn('     Where DD is the day in the form:     12');
  WriteLn('     Where MMM is the month in the form:  May');
  WriteLn('     Where YY is the day in the form:     82');
  WriteLn('     Where HH is the hour in the form:    16 (twenty-four hour clock)');
  WriteLn('     Where MM is the minutes in the form: 35');
  WriteLn('     Where SS is the seconds in the form: 10');
  WriteLn('     The seconds are optional and the time may be set without changing the day');
  WriteLn('     by simply typing the time in the form: HH:MM:SS');
  WriteLn;
  exit(SetTime);
  end;

procedure InitCmds;

{-----------------------------------------------------------------
{
{ Abstract:
{   Initialize the command processing tables.
{
{------------------------------------------------------------------}

begin

    FirstPress := true;
    InF := nil;
    SwitchTab[HelpSwitch]     :=  'HELP';
    SwitchTab[SETOFFSwitch]   :=  'SETGMTOFFSET';
    SwitchTab[EIOSwitch]      :=  'EIOCLOCK';
    SwitchTab[SERVERSwitch]   :=  'SERVER';
    SwitchTab[USERSwitch]     :=  'USER';
    end (* InitCmds *);

procedure ParseC;
  var I: integer;
    begin
    Inputs := nil;
    Switches := nil;
    SetOff := false;
    BadSwitch := false;
    
    I := POS(UsrCmdLine, '/');
    if I = 0 then
        begin
        Wherefrom := User;
        exit(ParseC);
        end
    else
        begin
        UsrCmdLine := substr(UsrCmdLine, I, length(UsrCmdLine) - I + 1);
        end;
    if not ParseStringArgs(UsrCmdLine, Inputs, Outputs, Switches, ErrStr) then
        begin
        writeln(ErrStr);
        exit(SetTime);
        end;

    while Switches <> nil do
        begin
        SwitchP := Switches;
        ConvUpper(SwitchP^.Switch);
        Switch := UniqueCmdIndex(SwitchP^.Switch, SwitchTab, NumSwitches);
        case Switch of
            HelpSwitch: begin
                Help;
                exit(SetTime);
                end;
            UserSwitch: WhereFrom := User; 
            ServerSwitch: WhereFrom := Server;
            EIOSwitch: WhereFrom := EIO;
            SetOffSwitch: begin
                 SetOff := true;
                 if CF_IOBoard <> CF_EIO then raise GTSNotPERQ2;
                 end;
            NumSwitches + 1: begin
                writeln('** ', Switchp^.Switch, ' is not a valid switch.');
                BadSwitch := true;
                end;
            NumSwitches + 2: begin
                writeln('** ', Switchp^.Switch, ' is not unique.');
                BadSwitch := true;
                end;
            end;
        Switches := SwitchP^.Next
        end;
    end;

handler GTSNotPERQ2;
    begin
    TimeIn := '';
    writeln('** This machine is not a PERQ2');
    goto 1;
    end;
handler GTSNoZ80;
    begin
    TimeIn := '';
    writeln('** The ZBoot file has not been loaded on this machine.');
    goto 1;
    end;



procedure get_EIO_time;

  handler InxCase;
  begin
  writeln('** Bad clock on EIO board -- contact Field Service for repair.');
  exit(settime);
  end;
  
begin
GetPERQ2Local(TStamp);
StampToString(TStamp, TimeIn);
end;



begin

 InitCmds;
 WhereFrom := User;

 FSAddToTitleLine(Title);
 
 HoldCmdLine := UsrCmdLine;
 ParseC;
 
 if BadSwitch then exit(SetTime);
 TimeIn := '';

  case WhereFrom of
    Server: begin
            I := 0;
            TimeIn := '';
            repeat
                TimeIn := GetEtherTime(60);
                I := I + 1
            until (I = 5) or (TimeIn <> '');
            end;
    User: begin
          UsrCmdLine := HoldCmdLine;
          c := NextID(s, isSwitch); {remove SetTime}
          if (c <>' ') and (c <> CCR) then 
            StdError(ErIllCharAfter,'SetTime',true);
          if UsrCmdLine <> '' then
            if UsrCmdLine[length(UsrCmdLine)] = CCR then
                Adjust(UsrCmdLine, length(UsrCmdLine)-1); {remove CCR from end}
          
          I := POS(UsrCmdLine, '/');
          if I <> 0 then
            UsrCmdLine := SubStr(UsrCmdLine, 1, I - 1);
          GetTString(OldTime);
          RemDelimiters(UsrCmdLine, ' ',s);
          if UsrCmdLine <> '' then 
            TimeIn := UsrCmdLine
          else 
            begin
            write('Enter time as HH:MM or full date: [', OldTime, '] ');
            readLn(TimeIn);
            if timeIn = '' then exit(SetTime);
            end;

         if (length(TimeIn) <= 8) and (PosC(TimeIn, ':') <> 0) then
            TimeIn := concat(substr(OldTime, 1, 10), TimeIn);
         end;

    
    EIO: begin
         get_EIO_time;
         end;
    end;
 
1: if TimeIn = '' then 
    begin
    write('** Could not get time from ');
    case WhereFrom of
        Server: writeln('Time Server.');
        User: writeln('User.');
        EIO:  writeln('EIO clock');
        end;
    exit(SetTime);
    end;
        
 StringToStamp(TimeIn,TStamp);  { may raise BadTime }
 SetTStamp(TStamp);

 GetTString(TimeIn);
 writeln('NewTime is ',TimeIn);
 
 if SetOff then
    begin
    PutPERQ2Offset;
    writeln('Local time <=> GMT offset set.');
    end; 
end.

