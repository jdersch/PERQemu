{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program Baud_Set;
{-----------------------------------------------------------------
{
{ Program Baud_Set (SetBaud utility) - Diana Connan Forgy
{ Copyright (C) 1981,1982, 1983 - Three Rivers Computer Corporation
{
{ Abstract:
{
{     Sets baud rate for RS232.
{
{-----------------------------------------------------------------}


{-----------------------------------------------------------------
{
{ Change Log:
{
{ 16 Nov 1982 V0.2 Bill Braucher
{ Fixed names for 14-character compiler.
{
{ 11 May 1982 V0.1 Michael R. Kristofic
{ Complete rewrite to fix bugs, use standard user interface.
{ Used MakeDir as a template.
{
{ 10 Sept 1981 V0.0 DCF
{ Created SetBaud.
{
{-----------------------------------------------------------------}
   
const Title = 'SetBaud V0.2  Type /Help for Help';

imports CmdParse   from CmdParse;
imports FileUtils  from FileUtils;
imports PERQ_String from PERQ_String;
imports RS232Baud from RS232Baud;

var switchAr: CmdArray;


Function HandleLine(ins, outs: pArgRec; switches: pSwitchRec; err: string;
                    ok: boolean): boolean;
                    
  Procedure DoHelp;
  {-----------------------------------------------------------------
  {
  { Abstract:
  {
  {     Displays help message and terminates Setbaud.
  {
  {-----------------------------------------------------------------}
      begin
      WriteLn;
      WriteLn('     SetBaud is used to set the baud rate of the PERQ.');
      WriteLn('     The syntax forthe command is:    SetBaud <newBaudRate>');
      Writeln('     Valid baud rates are:');
      Writeln('     9600,  4800,  2400,  1200,  600,  300,  150, and 110.');
      WriteLn('     The only switch is HELP.');
      WriteLn;
      exit(Baud_Set);
      end;
      
  Function ValidRate (Baud: String): Boolean;
  {-----------------------------------------------------------------
  {
  { Abstract:
  {
  {     Check to see that the baud rate typed in is valid.
  {
  {-----------------------------------------------------------------}
    
    begin
        
         ValidRate := False;
         If Baud = '9600' then ValidRate := True;
         If Baud = '4800' then ValidRate := True;
         If Baud = '2400' then ValidRate := True;
         If Baud = '1200' then ValidRate := True;
         If Baud = '600'  then ValidRate := True;
         If Baud = '300'  then ValidRate := True;
         If Baud = '150'  then ValidRate := True;
         If Baud = '110'  then ValidRate := True;
         
    end { ValidRate };
          
  var i: integer;
  
  begin { HandleLine }
  if not ok then StdError(ErAnyError, err, true);
  while switches <> NIL do
     begin
     ConvUpper(switches^.switch);
     i := UniqueCmdIndex(switches^.switch, switchAr, 1);
     case i of
          1 : DoHelp;
          otherwise: StdError(ErBadSwitch, switches^.switch, true);
          end;
     switches := switches^.next;
     end;
  if (outs^.name <> '') or (outs^.next <> NIL) then
      StdError(ErNoOutFile, 'SetBaud', true);
  if ins^.next <> NIL then StdError(ErOneInput, 'SetBaud', true);
  if ins^.name = '' then HandleLine := false
  else begin
       if not ValidRate(ins^.name) then
         StdError(ErAnyError,'** Bad baud rate',true);
       SetBaud(ins^.name,true);
       WriteLn('New baud rate is: ',ins^.name);
       HandleLine := true;
       end;
  end; { HandleLine }

var ins, outs: pArgRec;
    switches: pSwitchRec;
    err: String;
    ok: boolean;
    c: Char;
    s: CString;
    isSwitch: boolean;
      
begin { Baud_Set }

  FSAddToTitleLine(title);

  switchAr[1] := 'HELP';
  
  s := '';
  err := '';
  c := NextId(s, isSwitch);  {remove "SetBaud"}
  if (c<>' ') and (c<>CCR) then StdError(ErIllCharAfter, 'SetBaud', true);
  ok := ParseCmdArgs(ins, outs, switches, err);
  while not HandleLine(ins, outs, switches, err, ok) do
     begin
     Write('New baud rate: ');
     ReadLn(s);
     ok := ParseStringArgs(s, ins, outs, switches, err);
     end;
end.
