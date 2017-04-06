{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module PopCmdParse;
{---------------------------------------------------------------
{
{ Abstract:
{    This module provides procedures to help with PopUp menus.  See
{    the module PopUp for the definition of pNameDesc and for some
{    useful procedures for creating and destroying pNameDescs.
{
{ Written by Brad Myers    Nov 18, 1981
{
{ Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corperation
{
{---------------------------------------------------------------}


{------------------------------------------------------------------
{ Change Log:
{
{ Date: 14-Feb 83  V1.9 Brad Myers
{ Fixed small bug that allowed typeahead chars to show even though press
{ if typed before prompt appeared.

{ Date: 26 Jan-82  V1.8 Brad Myers
{ Only character illegal after a command is "=" in IGetCmdLine.
{ Extra blank input at the end of command files.
{ Use PointAllowed.


{ Date: 21 Jan-82  V1.7 Brad Myers
{ No popup if isFloppy
{

{ Date:  8 Jan-82  V1.6 Brad Myers
{ Fixed Type ahead bug if in a command file.
{ Fixed PopUniqueCmdIndex to find a command if it equals another even if it
{   is shorter.  Eg.  in table are "p" and "path".  "p" will get "p" even
{    though is not unique.
{ Help on PopUp menus.

{ Date:  5 Jan-82  V1.5 Brad Myers
{ No beep if Pop not OK for GetCmdLine and GetConfirm.

{ Date:  4 Jan-82  V1.4 Brad Myers
{ Fixed bug in CmdFile prompt character in GetCmdLine.
{ Re-instated IdleProc.

{ Date: 30 Dec-81  V1.3 Brad Myers
{ Fixed Destroy ==> Dstry for new CmdParse.
{ Added NullIdleProc.

{ Date: 30 Nov-81  V1.1 Brad Myers
{ Moved alloc and destroy Name desc to popUp.
{ Change GetConfirm to take prompt and default.  

{ }
{ Date: 19 Nov-81  V1.0 Brad Myers
{ Started.  
{ }
{------------------------------------------------------------------}


{********************} Exports {********************}

Imports CmdParse from CmdParse;
Imports PopUp from PopUp;

Function PopUniqueCmdIndex(Cmd: CString; Var names: pNameDesc): Integer;

Function GetCmdLine(Procedure IdleProc; prompt: String;
                    var line, cmd: CString; var inF: pCmdList;
                    var names: pNameDesc; var firstPress: boolean;
                    popOK: boolean): integer;

Function GetShellCmdLine(var cmd: CString; var inF: pCmdList;
                 var names: pNameDesc): integer;

Function GetConfirm(Procedure IdleProc; popOK: boolean;
                     prompt: String; def: integer;
                     var switches: pSwitchRec): integer;

Procedure NullIdleProc;

{********************} Private {********************}

Imports IO_Others from IO_Others;
Imports Screen from Screen;
Imports PERQ_String From PERQ_String;
Imports Stream From Stream;
Imports System from System;

Exception Impossible;

  

Procedure NullIdleProc;
{----------------------------------------------------------------
 Abstract: This procedure does nothing.  It is useful as an IdleProc
           parameter to other procedures when no IdleProc is needed.
-------------------------------------------------------------------------}
  begin
  { do nothing }
  end;


Function PopUniqueCmdIndex(Cmd: CString; Var names: pNameDesc): Integer;
{----------------------------------------------------------------
 Abstract: This procedure is used to do a unique lookup in a popUp command
           table.  It is the same as UniqueCmdIndex except the table of
           names is the kind used by popUp menus.  If cmd is the full name
           of one of the names in names, even if is also a sub-part of
           other names, it is returned as the one found.
 Parameters: Cmd - the command that we are looking for.

             CmdTable - a table of the valid commands.  The first valid
                        command in this table must start at index 1.

             NumCmds - the number of valid command in the table.
 Returns: The index of Cmd in CmdTable.  If Cmd was not found then return
          NumCmds + 1.  If Cmd was not unique then return NumCmds+2.
-------------------------------------------------------------------------}
   Var FoundCount, FoundIndex, I: Integer;
       temp: CString;
   Begin
   ConvUpper(cmd);
   FoundCount:=0;
   For I:= 1 To names^.numCommands Do
      Begin
{$R-}
      temp := names^.commands[i];
{$R=}
      ConvUpper(temp);
      If Pos(temp,Cmd) = 1 Then
         Begin
         if Length(temp)=Length(Cmd) then
            begin
            PopUniqueCmdIndex := i;
            exit(PopUniqueCmdIndex);
            end;
         FoundCount:=FoundCount+1;
         FoundIndex:=I;
         End;
      End;
   If FoundCount = 0 Then FoundIndex:=names^.numCommands+1;
   If FoundCount > 1 Then FoundIndex:=names^.numCommands+2;
   PopUniqueCmdIndex:=FoundIndex;
   End {PopUniqueCmdIndex};
        

Function IGetCmdLine(Procedure IdleProc; prompt: String;
                    var line, cmd: CString; var inF: pCmdList;
                    var names: pNameDesc; var firstPress: boolean;
                    popOK, confirming: boolean): integer;
{------------------------------------------------------------------
   Abstract:  Does the actual work for GetCmdLine and GetConfirm.
   Parameters:  see GetCmdLine.
                confirming - if true then no command files allowed and
                             no parsing of line is done.
   Returns: if confirming then returns index if pop or
            numCommands+6, else see GetCmdLine
------------------------------------------------------------------}
label 69, 68;

Procedure DoMenu(names: pNameDesc; var res: ResRes);
  Handler OutSide;
    begin
    goto 69;
    end;
  Handler HelpKey(var s: Sys9s);
    begin
    WriteLn;
    WriteLn('                     HELP ON POPUP MENUS:');
    WriteLn('    The PopUp menu shows the commands you can execute.');
    WriteLn('    To execute one of the commands, simply press on the command.');
    WriteLn('    To scroll the menu, press in the gauge area at');
    WriteLn('    the bottom and move left or right while pressed.  When released,');
    WriteLn('    the menu will stop scrolling.  The further from the center you move,');
    WriteLn('    the faster the menu scrolls.');
    WriteLn;
    StreamKeyboardReset(inF^.cmdFile);
    goto 68;
    end;
  begin
  Menu(names, false, 1, names^.numCommands, -1, -1, 300, res);
  end;

Const CtrlG = chr(7);

var popleave, charLeave : boolean;
    res: ResRes;
    c: Char;
    err: CString;
    isSwitch, leave: boolean;
    
   begin
(**
   if inf^.next <> NIL then
      repeat
        leave := true;
        if eof(inF^.cmdFile) then 
           begin
           ExitCmdFile(inF);
           if inf^.next <> NIL then leave := false;
           end;
      until leave;
***)
    

68: if not confirming then
        if inF^.next = NIL then Write(prompt, CmdChar)
        else write(prompt, CmdFileChar);
69: popLeave := false;
    charLeave := false;
    if (inF^.next = NIL) and popOK and (not isFloppy) and pointAllowed then 
         begin
         IOSetModeTablet(relTablet);
         IOCursorMode(TrackCursor);
         end;
   Repeat
     IdleProc;
     If tabSwitch and (inF^.next = NIL) and popOK and (not isFloppy)
                  and pointAllowed then
          if (keyLength > 0) then Write(CtrlG)
          else popLeave := true
     else charLeave := FullLn(inf^.cmdFile);
   Until charLeave or popLeave;
   if popLeave then 
       begin
       SCurOff;
       if firstPress then begin
                          firstPress := false;
                          InitPopUp;
                          end;
       DoMenu(names, res);
       if res^.numIndices <> 1 then Raise Impossible;
       IOSetModeTablet(offTablet);
       IOCursorMode(offCursor);
       Line := '';
     {$R-}
       cmd := names^.commands[res^.indices[1]];
     {$R=}
       WriteLn(cmd);
       IGetCmdLine := res^.indices[1];
       DestroyRes(res);
       end
  else begin
       IOSetModeTablet(offTablet);
       IOCursorMode(offCursor);
       if EOF(inf^.cmdFile) then
          begin
          ExitCmdFile(inF);
          IGetCmdLine := names^.numCommands + 3;
          line := '';
          cmd := '';
          writeln;
          exit(IGetCmdLine);
          end;
       readln(inF^.cmdFile, line);
       if inF^.next <> NIL then writeLn(line);
       RemDelimiters(line, ' ', err);
       if err = '@' then 
           if confirming then 
              begin
              WriteLn('** Command file not allowed');
              line := '';
              end
           else begin
                if not DoCmdFile(line, inF, err) then WriteLn(err);
                goto 68;
                end;
       if confirming then begin
                          IGetCmdLine := names^.numCommands+6;
                          exit(IGetCmdLine);
                          end;
       c := NextString(line, cmd, isSwitch);  {doesn't remove c from line}
       if isSwitch then IGetCmdLine := names^.numCommands+4
       else if (c = '=') then IGetCmdLine := names^.numCommands+5
       else if cmd = '' then IGetCmdLine := names^.numCommands + 3
       else IGetCmdLine := PopUniqueCmdIndex(cmd, names);
       end;
  end; {IGetCmdLine}


Function GetCmdLine(* Procedure IdleProc; prompt: String;
                    var line, cmd: CString; var inF: pCmdList;
                    var names: pNameDesc; var firstPress: boolean;
                    popOK: boolean): integer *);
{------------------------------------------------------------------
   Abstract:  Reads a line from the input file.  While waiting for a CR or a
              press do IdleProc.  If press, then create a popUp window.  Put
              name selected into line and return index.  If type a line,
              put it into line.  If first ID in line is not a switch then check
              to see if in names.  If so, returns index.  If not unique then
              returns numCommands+2.  If not found then returns numCommands+1.
              If line was empty (naked CR or comment), then returns
              numCommands+3.  numCommands+4 => switch. numCommands+5 => illegal
              character after command.
    Parameters:  IdleProc - This procedure is called repeatedly until a full
                            line is typed.  It should execute quickly and not
                            futz with the keyboard or stream.  An application
                            is a procedure that displays the time in the title
                            line.

                 prompt - the prompt string to print for the user.  Do not put
                          the prompt separator (>) on the end of the prompt;
                          GetCmdLine will do that for you.  If reading from
                          a command file, GetCmdLine change the prompt
                          appropriately.

                 line - set to the line read including starting with the first
                        significant character after the first command.  It will
                        not contain any comments.

                 cmd - the first command taken off the line.  (It is not in
                       line).  This will be valid even if the return value is
                       greater than numCommands.  It will be '' if no command
                       found before the first significant break character.

                 inF - a command file list created by InitCmdFile.  Just call
                       InitCmdFile and pass in the inF returned.  This
                       procedure manages the list and handles all command
                       files.  The application calling GetCmdLine will never
                       see an '@'.

                 names - a variable length array of names used for popUp menus
                         and for matching the input cmd against.

                 firstPress - USER MUST SET firstPress to true before first
                              call to this procedure and then not modify it.

                 popOk - if true, then GetCmdLine will allow a PopUp menu when
                         press on button to chose an item.  If false, then
                         no popUp allowed.
    Returns: index into the names array or:

                numCommands + 1 ==> Name not found in array

                numCommands + 2 ==> Name not unique

                numCommands + 3 ==> Name was empty

                numCommands + 4 ==> First command was a switch (it is in Cmd).

                numCommands + 5 ==> Illegal character found after command.
    Calls: NextIdString, PopUniqueCmdIndex, Menu (from PopUp), PopInit,
           IOSetModeTablet, IOCursorMode, FullLn, ReadLn, DestroyRes,
           DoCmdFile, ExitCmdFile, RemDelimiters
------------------------------------------------------------------}
  begin
  GetCmdLine := IGetCmdLine(IdleProc, prompt, line, cmd, inF, names,
                            firstPress, popOK, false);
  end;


Function GetConfirm(* Procedure IdleProc; popOK: boolean;
                     prompt: String; def: integer;
                     var switches: pSwitchRec): integer *);
{------------------------------------------------------------------
  Abstract: Handles a question that is answered Yes or No where the answer
            should come from the keyboard.  Prompt followed by default (if any)
            is printed.  Prompt may be null.  If illegal input is typed,
            GetConfirm re-asks but doesn't use prompt.
  Parameters:  IdleProc - This procedure is called repeatedly until a full
                            line is typed.  It should execute quickly and not
                            futz with the keyboard or stream.  An application
                            is a procedure that displays the time in the title
                            line.

               prompt - the prompt to display for question.
               
               default - index of the default answer: 1 = true or yes;
                         2 = false or no; other numbers mean no default.
               
               popOK - tells whether a popUp window is allowed.

               switches - set to NIL or a list of switches specified.  Be sure
                          to handle the switches first since one might be HELP.
  Returns: 1 if true or yes.

           2 if false or no.

           3 if naked return when no default and switches <> NIL.  This
             means that there was no argument but a switch was hit.  If an
             answer is still needed, the application should re-call GetConfirm.
------------------------------------------------------------------}
  var  names: pNameDesc;
       line, cmd, err: CString;
       inF: pCmdList;
       ins, outs: pArgRec;
       i: integer;
       ok, firstPress: boolean;
  begin
  firstPress := true;
  InitCmdFile(inF, 0);
  AllocNameDesc(2, 0, names);
{$R-}
  names^.header := 'Confirm';
  names^.commands[1] := 'YES';
  names^.commands[2] := 'NO';
  switches := NIL;

  write(prompt);
  repeat
    if def = 1 then Write('[Yes] ')
    else if def = 2 then Write('[No] ');

    i := IGetCmdLine(IdleProc, '', line, cmd, inF, names, firstPress,
                       popOK, true);
    if i <= 2 then ok := true
    else begin
         ok := ParseStringArgs(line, ins, outs, switches, err);
         if ok then if (ins^.next <> NIL) or (outs^.next <> NIL)
                   or (outs^.name <> '') then ok := false; 
         if ok then if ins^.name = '' then 
                        if switches <> NIL then i := 3
                        else if (def=1) or (def=2) then i := def
                        else ok := false
                    else begin
                         i := PopUniqueCmdIndex(ins^.name, names);
                         if i > 2 then ok := false;
                         end;
         if not ok then begin
                        Write('** Illegal input.  Type Yes or No');
                        if popOK and (not isFloppy) and pointAllowed then
                           Write(' or press for Menu: ')
                        else write(': ');
                        end;
         DstryArgRec(ins);
         DstryArgRec(outs);
         if not ok then DstrySwitchRec(switches);
         end;
  until ok;
  GetConfirm := i;
  DstryCmdFiles(inF);
  DestroyNameDesc(names);
  end;


Function GetShellCmdLine(var cmd: CString; var inF: pCmdList;
                 var names: pNameDesc): integer;
{------------------------------------------------------------------
   Abstract:  This routine is similar to GetCmdLine except that it works on
              the command line specified to the Shell.  It is should be used
              by programs that use GetCmdLine to parse the Shell command line.
              Command files are handled by GetShellCmdLine.  The user can
              call ParseCmdArgs after GetShellCmdLine to get the arguments
              to the command.
    Parameters:  cmd - the first command taken off the line.  This will be
                       valid even if the return value is
                       greater than numCommands.  It will be '' if no command
                       found before the first significant break character.

                 inF - a command file list created by InitCmdFile.  Just call
                       InitCmdFile and pass in the inF returned.  This
                       procedure manages the list and handles all command
                       files.

                 names - a variable length array of names used for popUp menus
                         and for matching the input cmd against.
    Returns: Identical to GetCmdLine.  Viz: index in the array or

                numCommands + 1 ==> Name not found in array

                numCommands + 2 ==> Name not unique

                numCommands + 3 ==> Name was empty

                numCommands + 4 ==> First command was a switch (it is in Cmd).

                numCommands + 5 ==> Illegal character found after command.
------------------------------------------------------------------}
    var isSwitch: boolean;
        err: string;
        c: Char;
    begin
    c := NextID(cmd, isSwitch);  {remove Cmd}
    if (c <> ' ') and (c <> CCR) then GetShellCmdLine := names^.numCommands+5
    else begin
         c := NextID(cmd, isSwitch);
         if cmd <> '' then
             if cmd[1] = '@' then 
                begin
                AppendString(cmd, UsrCmdLine);
                if not DoCmdFile(cmd, inF, err) then WriteLn(err);
                GetShellCmdLine := names^.numCommands + 3;
                cmd := '';
                exit(GetShellCmdLine);
                end;
         if isSwitch then GetShellCmdLine := names^.numCommands+4
         else if (c <> ' ') and (c <> CCR) and (c <> '~') then
               GetShellCmdLine := names^.numCommands+5
         else if cmd = '' then GetShellCmdLine := names^.numCommands + 3
         else begin
              Adjust(err, 1);
              err[1] := c;
              AppendString(err, UsrCmdLine);
              UsrCmdLine := err;
              GetShellCmdLine := PopUniqueCmdIndex(cmd, names);
              end;
         end;
   end.
