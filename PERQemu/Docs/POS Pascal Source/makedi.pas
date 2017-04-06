{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program MakeDir;
{----------------------------------------------------------------------------
  User level program to create directories
  Written by: Brad A. Myers
  Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation
-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
 Versions:
   V2.3  16-Nov-82 Bill Braucher Fixed names for 14-character compiler.
   V2.2   9 Dec-81 Brad Myers  Small bugs
   V2.1   9 Dec-81 Brad Myers  Remove dots
   V2.0  24-Nov-81 Brad Myers  Convert to new interface using new CmdParse
   V1.3   3-Jun-81 Brad Myers  Catch new exception
   V1.2  31-Mar-81 Brad Myers  Removes ">" from end of name if there
   V1.1  26-Mar-81 Brad Myers  Fixed so doesn't uppercase directories
   V1.0  16-Mar-81 Brad Myers  Created using Type as template
----------------------------------------------------------------------------}
   
Const Title = 'MakeDir V2.3.  Type /Help for Help';

imports CmdParse   from CmdParse;
imports FileUtils  from FileUtils;
imports PERQ_String from PERQ_String;

var switchAr: CmdArray;
      
Handler MkDirErr(msg: String; fileName: PathName);
  begin
  WriteLn('** ',msg, filename);
  exit(MakeDir);
  end;

Procedure DoHelp;
  begin
  WriteLn;
  WriteLn('     MakeDir is used to create new directories.  The syntax for');
  WriteLn('     the command is:    MakeDir <newDirectoryName>');
  WriteLn('     The only switch is HELP.');
  WriteLn;
  exit(MakeDir);
  end;

Function HandleLine(ins, outs: pArgRec; switches: pSwitchRec; err: string;
                    ok: boolean): boolean;
  var MyFID: FileID;
      i: integer;
  begin
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
      StdError(ErNoOutFile, 'MakeDir', true);
  if ins^.next <> NIL then StdError(ErOneInput, 'MakeDir', true);
  if ins^.name = '' then HandleLine := false
  else begin
       if ins^.name[length(ins^.name)] = '>' then
                Delete(ins^.name, length(ins^.name), 1);
       if not RemoveQuotes(ins^.name) then StdError(ErBadQuote, '', true);
       MyFid := FSMakeDirectory(ins^.name);
       FSRemoveDots(ins^.name);
       if MyFid = 0 then StdError(ErCannotCreate, ins^.name, true)
       else WriteLn('New directory is: ',ins^.name);
       HandleLine := true;
       end;
  end;

var ins, outs: pArgRec;
    switches: pSwitchRec;
    err: String;
    ok: boolean;
    c: Char;
    s: CString;
    isSwitch: boolean;
      
begin
  FSAddToTitleLine(title);

  switchAr[1] := 'HELP';
  
  s := '';
  err := '';
  c := NextId(s, isSwitch);  {remove "makedir"}
  if (c<>' ') and (c<>CCR) then StdError(ErIllCharAfter, 'MakeDir', true);
  ok := ParseCmdArgs(ins, outs, switches, err);
  while not HandleLine(ins, outs, switches, err, ok) do
     begin
     Write('Directory to create: ');
     ReadLn(s);
     ok := ParseStringArgs(s, ins, outs, switches, err);
     end;
  end.
