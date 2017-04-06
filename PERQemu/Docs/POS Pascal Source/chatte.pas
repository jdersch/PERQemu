{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program chatter;
{---------------------------------------------------------------------
 Abstract: Chatter makes the Perq into a terminal on another system using
           the RS-232 interface.  Characters received on the RS-232 line
           are echoed to the typeout window.  Characters typed on Perq's
           keyboard are sent (in ASCII) on the RS-232 interface.  Typing
           Control-R will prompt for a command in the command window.
           Avaialable Commands are:
              S - Save all typeout on a file, prompts for filename
              T - Transmit file, send the file as if it were typed
                     prompts for file name
              C - Close the Save file
              B - Set Baud Rate, prompts for new baud rate
              D - Set device (RSA or RSB).
              P - Set parity (EVEN ODD or NONE)
              Q - Quit Chatter
 
 Copyright (C) 1980, 1983 - Three Rivers Computer Corporation.
{--------------------------------------------------------------------}

{$Version V1.1 for POS}
  const  ChatterVersion = '1.1';
{---------------------------------------------------------------------
  Change log:
     21 May 84  V1.0  BB   Include parity option. 

     27 Oct 83  V0.9  DLK  Fix Device cmd to accept user response in
                           lower case.  

     17 Mar 83  V0.8  WJH  Really set baud when startup.  
                           Implement Device command and support two ports.
                           Put ^G in error messages.

     11-Feb-83  V0.7  BAM  Fixed for landscape monitor.
                           Fixed for help key.
      3-Jun-81  V0.6  BAM  Fixed for version D:
                            IO_Unit.
                            Handle some Exceptions.
                           Added setting of baud to 4800 at entry.
      7-Apr-81  V0.5  BAM  Fixed Back space and remove null.
     21-Feb-81  V0.4  JPS & BR  Convert to system C.3.
                           Add Clock to header
                           Fix Control C processing so that ^Cs typed
                           are passed through.  Also fixed a bug in
                           RS-232 input handling which caused a range
                           check error when characters with errors
                           were read.
     
     21-Nov-80  V0.3  JPS  Announce correct version number in top window.
     
     19-Nov-80  V0.2   BR  Speeded up typeout by calling sputchr directly
                           instead of using stream package

     29-Oct-80  V0.1   BR  Added transmit/save file stuff, baud rate settings
                           and window features
     
     ??-???-??  V0.0  MAB  Created program
{--------------------------------------------------------------------}


imports IO_Unit from IO_Unit;
imports IOErrors from IOErrors;
imports Screen from Screen;
imports Perq_String from Perq_String;
imports Clock from Clock;
imports RS232baud from RS232baud;
imports Stream from Stream;
imports system from system;
imports Configuration from Configuration;
imports IORS from IORS;

{R-}
var Last,This,RIn, ch: char;
    HPos: integer;
    wf, rf: text;
    fn,title,rate,TimStr,NewTimStr,parity: string;
    SaveChars, ItWorked: boolean;
    CurDevice: UnitRng;
    DevStr: String;
    UserStatus:DevStatusBlock;
    CurrentSpeed:0..255;

Const DefaultBaud = '9600';

procedure writefile;
var line: string [128];
    i: integer;

  procedure SendC(c: char);
   begin
     write(c);
     if IOCWrite(CurDevice,c) <> IOEIOC then
            writeln('RSOut Error')
   end;

  Handler ResetError(fileName: PathName);
    begin
    WriteLn('** ',fileName,' not found.');
    ChangeWindow(2);
    Exit(WriteFile);
    end; 

begin {writefile}
 write('File to send: ');
 readln(fn);
 if fn = '' then
    begin
    ChangeWindow(2);
    exit(WriteFile);
    end;
 reset(wf,fn);
 ChangeWindow(2);
 while not eof(wf) do
  begin
   readln(wf,line);
   for i := 1 to length(line) do SendC(line[i]);
   SendC(chr(#15));
   SendC(chr(#12));
  end;
 close(wf);
end; {writefile}

procedure writeit(c: char);
begin
 sputchr(c);
 if SaveChars then write(rf,c);
end;

label 1;

Handler BadBaudRate;
  begin
  writeln('** No such speed');
  goto 1;
  end;

Handler RewriteError(fileName: PathName);
  begin
  writeln('** Unable to create file ',filename);
  goto 1;
  end;

Handler CtlC;
{--------------------------------------------------------------------
 Abstract: Handle this so ^C will never cause an abort.
{-------------------------------------------------------------------}
  begin
  end;

Handler HelpKey(var s: Sys9s);
{--------------------------------------------------------------------
 Abstract: Handle this to get ^G.
{-------------------------------------------------------------------}
  begin
  s := ' ';
  s[1] := chr(7);
  end;

begin
reset(input);
rewrite(output);
write(chr(#14));
GetTString(TimStr);
title := Concat( Concat('Chatter V', ChatterVersion),
                 '  Type ^R for extra functions.           ');
createwindow(1,0,0,SBitWidth,56,title);
WriteLn('Setting baud rate to ',DefaultBaud);

createwindow(2,0,57,SBitWidth,SBitHeight-57,TimStr);
SCurOn;
SaveChars := false;

CurDevice := RSA;
rate := DefaultBaud;
currentspeed:=Rs9600;
SetRS232Port(rate, CurDevice);

HPos:=0;
Last:=' ';

while true do
    begin
    if IOCRead(CurDevice,RIn) = IOEIOC then
        begin
        RIn:=chr(LAnd(ord(RIn),#177));
        if RIn = chr(9) then
            begin
            writeit(' ');
            HPos:=HPos+1;
            while (HPos mod 8) <> 0 do
                begin
                writeit(' ');
                HPos:=HPos+1
                end
            end
        else if RIn = chr(8) then {BS}
            begin
            SBackSpace(' ');
            HPos:=HPos-1
            end
        else if RIn = chr(0) then {ignore}
        else
            begin
            writeit(RIn);
            HPos:=HPos+1
            end;
        if RIn in [chr(#12),chr(#15)] then
            HPos:=0
        end
    else if IOCRead(TransKey,This) = IOEIOC then
        begin
        if This = chr(8) then This:=chr(#177);
        if (This = chr(#22)) then
                begin
                 ChangeWindow(1);
                 write(chr(#14));
                 write('S-ave on file, T-ransmit file, C-lose file, B-aud Rate, D-evice,P-arity, Q-uit: ');
                 readln(ch);
                 case ch of
                  'T','t': WriteFile;
                  'S','s': begin
                            if SaveChars then close(rf);
                            write('File to save type-out on: ');
                            readln(fn);
                            rewrite(rf,fn);
                            title := Concat('Transactions being saved on file ',fn);
                            Changewindow(2);
                            ChangeTitle(title);
                            SaveChars := true;
                           end;
                  'C','c': begin
                            if SaveChars then close(rf);
                            ChangeWindow(2);
                            ChangeTitle(' ');
                            SaveChars := false;
                           end;
                  'B','b': begin
                            write('New baud rate: ');
                            readln(rate);
                            if rate='110' then CurrentSpeed:=RS110;
                            if rate='150' then CurrentSpeed:=RS150;
                            if rate='300' then CurrentSpeed:=RS300;
                            if rate='600' then CurrentSpeed:=RS600;
                            if rate='1200' then CurrentSpeed:=RS1200;
                            if rate='2400' then CurrentSpeed:=RS2400;
                            if rate='4800' then CurrentSpeed:=RS4800;
                            if rate='9600' then CurrentSpeed:=RS9600;
                            SetRS232Port(rate, CurDevice);
                           end;
                  'D','d': begin
                            if CurDevice=RSA then write('New device [RSA] ')
                            else write ('New device [RSB] ');
                            readln(DevStr);
                            ConvUpper(DevStr);
                            if DevStr='RSA' then CurDevice := RSA
                            else if (DevStr='RSB') and
                               (Cf_RS232Ports>1) then CurDevice := RSB
                            else if DevStr <> '' then writeln('** Huh?');
                            SetRS232Port(rate, CurDevice);
                           end;
                  'P','p':begin
                            write('New parity:');
                            readln(parity);
                            ConvUpper(parity);
                            If parity='ODD' then
                            begin
                                 with UserStatus do
                                 begin
                                      RsSpeed:=CurrentSpeed;
                                      RsRcvEnable:=true;
                                      RsParity:=OddParity;
                                      RsStopbits:=Stop1;
                                      RsXmitbits:=Send7;
                                      RsRcvbits:=Rcv7;
                                      bytecnt:=6;
                                 end;
                            Rs_Putstatus(CurDevice,UserStatus);
                            end
                            else if parity='EVEN' then
                            begin
                                 with UserStatus do
                                 begin
                                      RsSpeed:=CurrentSpeed;
                                      RsRcvEnable:=true;
                                      RsParity:=EvenParity;
                                      RsStopbits:=Stop1;
                                      RsXmitbits:=Send7;
                                      RsRcvbits:=Rcv7;
                                      bytecnt:=6;
                                 end;
                            Rs_Putstatus(CurDevice,UserStatus);
                            end
                            else if parity='NONE' then
                            begin
                                 with UserStatus do
                                 begin
                                      RsSpeed:=CurrentSpeed;
                                      RsRcvEnable:=true;
                                      RsParity:=NoParity;
                                      RsStopbits:=Stop1;
                                      RsXmitbits:=Send8;
                                      RsRcvbits:=Rcv8;
                                      bytecnt:=6;
                                 end;
                            Rs_Putstatus(CurDevice,UserStatus);
                            end;
                          end;


                  'Q','q': begin
                            CreateWindow(0,0,0,SBitWidth,SBitHeight,'');
                            if SaveChars then Close(rf);
                            Exit(chatter);
                           end;
                  Otherwise: writeln('** Huh?');
                 end;
              1:  ChangeWindow(2);
               end
        else if IOCWrite(CurDevice,This) <> IOEIOC then
                  writeln('RSOut Error')
             else Last:=This
        end
    else begin
              GetTString(NewTimStr);
              if NewTimStr <> TimStr then
                  begin
                   TimStr := NewTimStr;
                   ChangeTitle(TimStr);
                  end
         end
    end;
end.
