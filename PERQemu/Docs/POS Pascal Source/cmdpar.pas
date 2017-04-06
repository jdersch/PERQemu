{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module CmdParse;
{---------------------------------------------------------------
{ Abstract:
{    This module provides a number of routines to help with command parsing.
{
{ Written by Don Scelza    April 30, 1980
{
{ Copyright (C) 1980, 1981, 1982, 1983 - Three Rivers Computer Corporation
{---------------------------------------------------------------}


{------------------------------------------------------------------
{ Change Log:
{
{ Date: 16 Nov-82  V3.7 Bill Braucher
{ Fixed names for 14-character compiler.
{ }

{ Date: 26 Jan-82  V3.6 Brad Myers
{ New procedure NextString like NextID only doesn't remove final character.
{ }

{ Date:  5 Jan-82  V3.5 Brad Myers
{ Fixed ParseStringArgs so inputs and outputs correctly set to NIL if error.
{ Additional error message from ParseStringArgs for , in outputs.
{ }

{ Date: 29 Dec-81  V3.4 Brad Myers
{ No period after ErAnyError message.
{ New error message: ErBadQuote.
{ Changed so err set to '' if no error (to avoid StringTooLong abort)
{ }

{ Date: 28 Dec-81  V3.3 Brad Myers
{ New procedure for removing quotes.  
{ New procedure for reporting errors.  
{ Renamed destroy procedures to Dstry... due to name conflicts
{ }

{ Date: 18 Dec-81  V3.2 Brad Myers
{ Allow quoting of special characters for NextID.  
{ }
{
{ Date: 27 Nov-81  V3.1 Brad Myers
{ New procedure: DestroyCmdFiles.  
{ }

{ Date: 13 Nov-81  V3.0 Brad Myers
{ Add new commands to make parsing easier.  Also don't require importing
{ system for UsrCmdLine.  
{ }

{ Date: 13-May-81  V2.0 John Strait
{ Change length of most strings to be 255 characters in length.  The strings
{   in CmdArrays are left as 80 characters because 255 character strings take
{   a lot of space and we figure that no command names will be longer than
{   80 characters.
{ }

{ Date: 19-Mar-81  Brad Myers
{ PERQ_String.
{ }

{ Date: 12-Feb-81  Diana Forgy
{ Module is now compatable with new PString, System, and Compiler.
{ }

{ Date: 23-Sep-80 Miles Barel
{ Incremented MaxCmds to 30.  Added the version number.
{ }

{ Date: 30-Apr-80 Don Scelza
{ Create CmdParse
{ }
{------------------------------------------------------------------}


{********************} Exports {********************}

Const CmdPVersion = '3.5';
      MaxCmds = 30;
      MaxCString = 255;
      CCR = Chr(13);  {same as standard CR}
      CmdChar = Chr(24);
      CmdFileChar = Chr(26);
      
Type CString = String[MaxCString];
     CmdArray = Array[1..MaxCmds] Of String;

Procedure CnvUpper(Var Str:CString);  {*** USE ConvUpper IN PERQ_String***}
Function UniqueCmdIndex(Cmd:CString; Var CmdTable: CmdArray;
                        NumCmds:Integer): Integer;
Procedure RemDelimiters(Var Src:CString; Delimiters:CString;
                        Var BrkChar:CString);
Procedure GetSymbol(Var Src,Symbol:CString; Delimiters:CString;
                    Var BrkChar:CString);

Function NextID(var id: CString; var isSwitch: Boolean): Char;
Function NextIDString(var s, id: CString;var isSwitch: Boolean): Char;

Type pArgRec = ^ArgRec;
     ArgRec = RECORD
                name: CString;
                next: pArgRec;
              END;

     pSwitchRec = ^SwitchRec;
     SwitchRec = RECORD
                   switch: CString;
                   arg: CString;
                   correspondingArg: pArgRec;
                   next: pSwitchRec;
                 END;

Function ParseCmdArgs(var inputs, outputs: pArgRec; var switches: pSwitchRec;
                      var err: String): boolean;
Function ParseStringArgs(s: CString; var inputs, outputs: pArgRec;
                      var switches: pSwitchRec; var err: String): boolean;
Procedure DstryArgRec(var a: pArgRec);
Procedure DstrySwitchRec(var a: pSwitchRec);

Type pCmdList = ^CmdListRec;
     CmdListRec = RECORD
                    cmdFile: Text;
                    isCharDevice: Boolean;
                    next: pCmdList;
                    seg: Integer;
                  END;

Procedure InitCmdFile(var inF: pCmdList; seg: Integer);
Function DoCmdFile(line: CString; var inF: pCmdList; var err: String): boolean;
Procedure ExitCmdFile(var inF: pCmdList);
Procedure ExitAllCmdFiles(var inF: pCmdList);
Procedure DstryCmdFiles(var inF: pCmdList);

Function RemoveQuotes(var s: CString): boolean;

Type ErrorType = (ErBadSwitch, ErBadCmd, ErNoSwParam, ErNoCmdParam, ErSwParam,
                  ErCmdParam, ErSwNotUnique, ErCmdNotUnique, ErNoOutFile,
                  ErOneInput, ErOneOutput, ErFileNotFound, ErDirNotFound,
                  ErIllCharAfter, ErCannotCreate, ErAnyError, ErBadQuote);
                  
Procedure StdError(err: ErrorType; param: CString; leaveProg: Boolean);
Function NextString(var s, id: CString; var isSwitch: Boolean): Char;

{********************} Private {********************}

Imports PERQ_String From PERQ_String;
Imports System From System;
Imports Stream from Stream;
Imports FileUtils from FileUtils;

Exception Impossible;
Const NextIdBreakChars = ' ,=~/';
      Quote = '''';
      

Procedure InitCmdFile(var inF: pCmdList; seg: Integer);
{------------------------------------------------------------------
   Abstract:  Initializes inF to be a valid Text File corresponding to the
              keyboard.  This must be called before any other command file
              routines.  The application should then read from inF^.cmdFile.
              E.g.    ReadLn(inFile^.cmdFile, s);    
              or      while not eof(inFile^.cmdFile) do ...
              Use popup only if inF^.next = NIL (means no cmd File).
              Is a fileSystem file if not inF^.isCharDevice.
              InF will never be NIL.  The user should not modify the pCmdList
              pointers; use the procedures provided.
  Parameters: InF - is set to the new command list.

              seg - the segment number to allocate the command file list out
                    of.  If the application doesn't care, use 0.  This is
                    useful for programs like the Shell that require the
                    list of command files to exist even after the program
                    terminates.  For other applications, use 0.
------------------------------------------------------------------}
  begin
  New(seg, 1, inF);
  Reset(inF^.cmdFile, 'console:');
  inF^.isCharDevice := true;
  inF^.seg := seg;
  inF^.next := NIL;
  end;


Function DoCmdFile(line: CString; var inF: pCmdList; var err: String): boolean;
{------------------------------------------------------------------
   Abstract:  This procedure is meant to handle an input line that specifies
              that a command file should be invoked.  The application will
              find a line that begins with an @ and will call this procedure
              passing that line.  This procedure maintains a stack of command
              files so that command files can contain other command files.
              Be sure to call InitCmdFile before calling this procedure.
   Parameters: line - the command line found by the application.  It is OK if
                      it starts with an @ but it is also OK if it doesn't.

               inF - the list of command files.  This was originally created by
                     InitCmdFile and maintained by these procedures.  If the
                     name is a valid file, a new entry is put on the front
                     of inF describing it.  If there is an error, then inF is
                     not changed.  In any case, inF will always be valid.

               err - if there is an error, then set to a string describing the
                     error, complete with preceding '** '. If no error, then
                     set to ''.  The application can simply do:
                         if not DoCmdFile(s, inF, err) then WriteLn(err);
   Returns: True if OK, or false if error.
------------------------------------------------------------------}
   var id: CString;
       isSwitch, leave, isDevice: boolean;
       c: Char;
       blks, bits, i: integer;
       tempCmd: pCmdList;
       fid: FileID;
       tempS: String;
  Procedure HaveError(s: String);
      begin
      err := Concat('** ',s);
      DoCmdFile := false;
      Exit(DoCmdFile);
      end;
   Handler ResetError(f: PathName);
      begin
      if inF^.next = NIL then Raise Impossible;
      tempCmd := inF^.next;
      DISPOSE(inF);
      inF := tempCmd;
      HaveError(Concat('Command file not found: ',id));
      end;
   label 1;
   begin
   err := '';
   DoCmdFile := true;
   i := PosC(line, '@');
   if i <> 0 then line := SubStr(line, i+1, length(line)-i);
   c := NextIDString(line, id, isSwitch);
   if isSwitch then HaveError('Switch cannot be a command file.');
   if id = '' then HaveError('Command file name cannot be null');
   if (c <> CCR) then HaveError('Command file must be only item on line.');
   isDevice := FSIsFSDev(id, tempS) <> 0;
   if not isDevice then
       fid := FSExtSearch(FSSysSearchList, ' .CMD ',id, blks, bits);
   if isDevice or (fid <> 0) then
      begin  {have a good cmd file}
      leave := false;
      if not inF^.isCharDevice then
          repeat
            if EOF(inF^.cmdFile) then begin
                                      Close(inF^.cmdFile); {no new one}
                                      goto 1;
                                      end;
            c := inF^.cmdFile^;
            if c <> ' ' then leave := true
            else get(inF^.cmdFile);
         until leave;
      NEW(inF^.seg, 1, tempCmd);
      tempCmd^.next := inF;
      tempCmd^.seg := inF^.seg;
      inF := tempCmd;
   1: RESET(inF^.cmdFile, id);
      inF^.isCharDevice := isDevice;
      end
   else HaveError(Concat('Command file not found: ',id));
   end;


Procedure ExitCmdFile(var inF: pCmdList);
{------------------------------------------------------------------
   Abstract:  Remove top command file from list.
              Call this whenever come to end of a command file.
   Parameters: InF - the list of command files.  It must never be NIL.
                     The top entry is removed from inF unless
                     attempting to remove last command file, when it is simply
                     re-initialized to be the console.  It is OK to call
                     this routine even when at the last entry of the list.
                     Suggested use:
                         While EOF(inF^.cmdFile) do ExitCmdFile(inF);
------------------------------------------------------------------}
   var tempCmd: pCmdList;
   begin
   tempCmd := inF^.next;
   Close(inF^.cmdFile);
   if tempCmd = NIL then Reset(inF^.cmdFile, 'console:')
   else begin
        Dispose(inF);
        inF := tempCmd;
        end;
   end;


Procedure ExitAllCmdFiles(var inF: pCmdList);
{------------------------------------------------------------------
   Abstract:  Remove all command file from list.  Use when get an error or a
              ^SHIFT-C to reset all command files.
   Parameters: InF - the list of command files.  It must never be NIL.
                     All entries but the last are removed.
------------------------------------------------------------------}
   begin
   while inF^.next <> NIL do
      ExitCmdFile(inF);
   ExitCmdFile(inF);  {once more to clean out the console entry}
   end;


Procedure DstryCmdFiles(var inF: pCmdList);
{------------------------------------------------------------------
   Abstract:  Removes all command files from list.
   Parameters: InF - the list of command files.  All entries are removed and
                     InF set to NIL.
------------------------------------------------------------------}
   begin
   if inF <> NIL then
      begin
      while inF^.next <> NIL do
         ExitCmdFile(inF);
      Close(inF^.cmdfile);
      Dispose(inF);
      inF := NIL;
      end;
   end;


Function FindNonSpace(s: CString; start: integer): integer;
{----------------------------------------------------------
  Abstract: Find the first Non-space character from the front of s.
  Parameters:
     s - string to search for non-space in. NOTE: no range checking is done
         so there better be a non-space to prevent a string overflow.

     start - the first character to look at in s
  Results: Returns the index of the first non-space.  Note that this may
           be start if there are no spaces at the beginning of s.
--------------------------------------------------------------}
   begin
   while s[start] = ' ' do
      start := start+1;
   FindNonSpace := start;
   end;


Function NextID(var id: CString; var isSwitch: Boolean): Char;
{----------------------------------------------------------
  Abstract:
     Gets the next word off UsrCmdLine and returns it.  It is OK to call this
     routine when UsrCmdLine is empty (id will be empty and return will be CCR)
     This procedure also removes comments from s (from ! to end of line
     is ignored).  This is exactly like NextIDString except it uses the
     UsrCmdLine by default.
 
  WARNING: It is a bad idea to mix calls to NextID and RemDelimiters/GetSymbol
           since the latter 2 may change UsrCmdLine in a way that causes
           NextID to incorrectly report that an id is not a switch.   This
           procedure also appends a CCR to the end of UsrCmdLine so it should
           not be printed after this procedure is called.
 
  Parameters:
     id - set to the next word on UsrCmdLine.  If there are none, then id will
          be the empty string.

     isSwitch - tells whether the word STARTED with a slash "/".  The slash is
                not returned as part of the name.
 
  Results:
     The character returned is the next "significant" character after the id.
     The possible choices are "=" "," " " "~" CCR. CCR is used to mean the end
     of the line was hit before a significant character.  If there are spaces
     after the id and then one of the other break characters defined above,
     then the break character is returned.  If there is a simply another id
     and no break characters, then SPACE is returned.
 
  SideEffects:
     Puts a CCR at the end of UsrCmdLine so it is a bad idea to print
     UsrCmdLine after NextID is called the first time.  Removes id and
     separators from front of UsrCmdLine.  The final character is also removed.
--------------------------------------------------------------}
    begin
    NextID := NextIDString(UsrCmdLine, id, isSwitch);
    end; {NextID}


Function NextIDString(var s, id: CString; var isSwitch: Boolean): Char;
{----------------------------------------------------------
 Abstract:
    Gets the next word off s and returns it.  It is OK to call this
    routine when s is empty (id will be empty and return will be CCR)
    This procedure also removes comments from s (from ! to end of line
    is ignored).  This is exactly like NextID except it allows the user
    to specify the  string to parse.

 WARNING:
          It is a bad idea to mix calls to NextIDString and
          RemDelimiters/GetSymbol since the latter 2 may
          change s in a way that causes
          NextIDString to incorrectly report that an id is not a switch.

 Parameters:
    s - String to parse.  Changed to remove id and separators from front.  The
        final character is also removed.

    id - set to the next word in string.  If there are none, then id will
         be the empty string.

    isSwitch - tells whether the word STARTED with a slash "/".  The slash is
               not returned as part of the name.

 Results:
    The character returned is the next "significant" character after the id.
    The possible choices are "=" "," " " "~" CCR. CCR is used to mean the end
    of the line was hit before a significant character.  If there are spaces
    after the id and then one of the other break characters defined above,
    then the break character is returned.  If there is a simply another id
    and no break characters, then SPACE is returned.
--------------------------------------------------------------}
    var start, finish: Integer;
        leave, inQuote: boolean;
    begin
    isSwitch := false;
    start := 1;
    if Length(s) = 0 then AppendChar(s, CCR)
    else if s[length(s)] <> CCR then
             begin
             finish := PosC(s, '!');
             if finish <> 0 then s := SubStr(s, 1, finish-1); {remove comment}
             AppendChar(s, CCR);
             end;
    start := FindNonSpace(s, 1);
    if s[start] = '/' then begin
                           isSwitch := true;
                           start := FindNonSpace(s, start+1);
                           end;
    finish := start;
    inQuote := false;
    repeat
      while PosC(NextIDBreakChars, s[finish]) = 0 do
         begin
         inQuote := (not inQuote) and (s[finish] = Quote);
         finish := finish+1;
         end;
      if (s[finish] <> CCR) and (finish <> 1) then
         begin
         leave := not inQuote;
         if inQuote then
            begin
            finish := finish+1;
            inQuote := false;
            end;
         end
      else leave := true; 
    until leave;
    id := SubStr(s, start, finish-start);
    finish := FindNonSpace(s,finish);
    if PosC(NextIDBreakChars, s[finish]) <> 0 then
         begin
         if s[finish] = '/' then NextIDString := ' '
         else begin
              NextIDString := s[finish];
              finish := finish+1; {remove break char}
              end;
         end
    else NextIDString := ' ';
    s := SubStr(s, finish, 1+Length(s)-finish);
    end;  {NextIDString}


Function NextString(var s, id: CString; var isSwitch: Boolean): char;
{----------------------------------------------------------
 Abstract:
    Gets the next word off s and returns it.  It is OK to call this
    routine when s is empty (id will be empty and return will be CCR)
    This procedure also removes comments from s (from ! to end of line
    is ignored).  The character after id is NOT removed from s.  This
    is like NextIDString except the character is removed in NextIDString.

 Parameters:
    s - String to parse.  Changed to remove id and separators from front.
        Final character is NOT removed.

    id - set to the next word in string.  If there are none, then id will
         be the empty string.

    isSwitch - tells whether the word STARTED with a slash "/".  The slash is
               not returned as part of the name.

 Results:
    The character returned is the next "significant" character after the id.
    This character remains at the front of s.
    The possible choices are "=" "," " " "~" CCR. CCR is used to mean the end
    of the line was hit before a significant character.  If there are spaces
    after the id and then one of the other break characters defined above,
    then the break character is returned.  If there is a simply another id
    and no break characters, then SPACE is returned.

--------------------------------------------------------------}
  var s1: CString;
      c: Char;
  begin
  c := NextIDString(s,id,isSwitch);
  if c <> CCR then
     begin
     Adjust(s1, 1);
     s1[1] := c;
     AppendString(s1,s);
     s := s1;
     end;
  NextString := c;
  end;


Function RemoveQuotes(var s: CString): boolean;
{----------------------------------------------------------
 Abstract: Changes all quoted quotes ('') into single quotes (').

 Parameters:  s - string to remove quotes from.  It is changed.

 Returns: true if all ok.  False if a single quote ended the string.
          In this case s still contains that quote.

--------------------------------------------------------------}
   var start: integer;
   begin
   RemoveQuotes := true;
   start := 1;
   while start <= length(s) do {continuously re-evaluate length}
      begin
      if (s[start] = Quote) then
            if start = length(s) then RemoveQuotes := false
            else Delete(s, start, 1); {remove ' and skip next char}
      start := start+1;
      end;
   end;


Function ParseCmdArgs(var inputs, outputs: pArgRec; var switches: pSwitchRec;
                      var err: String): boolean;
{----------------------------------------------------------
 Abstract:
    Parses the command line assuming standard form.  The command should be
    removed from the front using NextId before ParseCmdArgs is called.

 Parameters:
    inputs - set to list describing the inputs.  There will
             always be at least one input, although the name may be empty.

    outputs - set to list describing the outputs. There will
              always be at least one output, although the name may be empty.

    switches - set to the list of switches, if any.  Each switch points to
               the input or output it is attached to.  This may be NIL if
               the switch appears before any inputs.  If a global switch,
               the application can ignore this pointer.  If a switch is
               supposed to be local, the application can search for each
               input and output through the switches looking for the switches
               that correspond to this arg.  Switches may be NIL if there are
               none.

    err - set to a string describing the error if there is one.  This string
            can simply be printed.  If no error, then set to ''.

 Returns:
    false if there was a reported error so the cmdLine should be rejected.  In
    this case, the pArgRecs should be Destroyed anyway.

--------------------------------------------------------------}
   begin
   ParseCmdArgs := ParseStringArgs(UsrCmdLine, inputs, outputs, switches, err);
   end;


Function ParseStringArgs(s: CString; var inputs, outputs: pArgRec;
                      var switches: pSwitchRec; var err: String): boolean;
{----------------------------------------------------------

 Abstract:
    Parses the string assuming standard form.  The command should be
    removed from the front using NextId before ParseCmdArgs is called.

 Parameters:
    s - the string to parse.

    inputs - set to list describing the inputs.  There will
             always be at least one input, although the name may be empty.

    outputs - set to list describing the outputs. There will
              always be at least one output, although the name may be empty.

    switches - set to the list of switches, if any.  Each switch points to
               the input or output it is attached to.  This may be NIL if
               the switch appears before any inputs.  If a global switch,
               the application can ignore this pointer.  If a switch is
               supposed to be local, the application can search for each
               input and output through the switches looking for the switches
               that correspond to this arg.  Switches may be NIL if there are
               none.

    err - set to a string describing the error if there is one.  This string
            can simply be printed.  If no error, then set to ''.

 Returns:
    false if there was a reported error so the string should be rejected.  In
    this case, the pArgRecs should be Destroyed anyway.

--------------------------------------------------------------}
   const DEL = Chr(#177);
   var id: CString;
       c, c2: Char;
       cur, before: pArgRec;
       curSw, beforeSw: pSwitchRec;
       state : (ins, outs);
       isSwitch, inSwitch, switchArgs, foundSquiggle: boolean;
       inputCnt, outputCnt: integer;
       
    Procedure HaveError(s: String);
        begin
        err := Concat('** ',s);
        ParseStringArgs := false;
        exit(ParseStringArgs);
        end;
    Procedure CreateCur(s: CString);
        begin
        NEW(cur);
        if before = NIL then
           if state = ins then inputs := cur
           else outputs := cur
        else before^.next := cur;
        before := cur;
        cur^.name := s;
        cur^.next := NIL;
        end;
 begin
 err := '';
 ParseStringArgs := true;
 inputCnt := 0;
 outputCnt := 0;
 state := ins;
 before := NIL;
 beforeSw := NIL;
 cur := NIL;
 switches := NIL;
 switchArgs := false;
 inSwitch := false;
 foundSquiggle := false;
 inputs := NIL;
 outputs := NIL;
 
 c2 := NextIdString(s, id, isSwitch);
 repeat
   c := c2;
   if isSwitch then
      begin
      switchArgs := false;
      New(curSw);
      if beforeSw = NIL then switches := curSw
      else beforeSw^.next := curSw;
      beforeSw := curSw;
      curSw^.switch := id;
      if before = NIL then curSw^.correspondingArg := NIL
      else curSw^.correspondingArg := cur;
      curSw^.arg := '';
      curSw^.next := NIL;
      inSwitch := true;
      end
   else if switchArgs then
        begin
        curSw^.arg := id;
        switchArgs := false;
        inSwitch := false;
        end
   else begin
        inSwitch := false;
        CreateCur(id);
        if state = ins then inputCnt := inputCnt+1
        else outputCnt := outputCnt+1;
        end;
   
   c2 := NextIdString(s, id, isSwitch); {check next to see if switch}
   if (c = ' ') and isSwitch then c := DEL;
      
   case c of
      DEL : ;
      CCR : begin
            if inputCnt = 0 then begin
                                 state := ins;
                                 before := NIL;
                                 CreateCur('');
                                 end;
            if outputCnt = 0 then begin
                                  state := outs;
                                  before := NIL;
                                  CreateCur('');
                                  end;
            end;
      ' ' : if (inputCnt = 1) and (state = ins) then
                 begin
                 state := outs;
                 before := NIL;
                 end
            else if (state=outs) and (outputCnt = 0) then {ok (switch)}
            else if inputCnt = 1 then HaveError('If more than one input, separate them by "," and signal output by "~".')
            else if state = outs then HaveError('Separate outputs by ",".')
            else if inputCnt <> 0 then
                    HaveError('If more than one input, signal output by "~".');
      '=' : if inSwitch then switchArgs := true
            else HaveError('Only use "=" for arguments to switches.  Use "~" to signal output.');
      '~' : begin
            if outputCnt = 0 then begin
                                  state := outs;
                                  before := NIL;
                                  end
                
            else if foundSquiggle then HaveError('Only one "~" allowed since it signals the beginning of output.')
            else HaveError('Illegal "~" since output already signalled by the space.');
            foundSquiggle := true;
            end;
      ',' : begin
            if (state = outs) and (not foundSquiggle) then HaveError('If more than one output, use "~" before first.');
            cur := NIL;
            end;
      otherwise: WriteLn('** Impossible character returned: ',c);
      end;


until c = CCR;
end;


Procedure DstryArgRec(var a: pArgRec);
{----------------------------------------------------------
  Abstract: Deallocates the storage used by a ArgRec list.
  Parameters: a - the head of the list of ArgRecs to deallocate.
                  It is set to NIL.  OK if NIL before call.
--------------------------------------------------------------}
   var temp: pArgRec;
   begin
   while a <> NIL do
      begin
      temp := a^.next;
      dispose(a);
      a := temp;
      end;
  end;


Procedure DstrySwitchRec(var a: pSwitchRec);
{----------------------------------------------------------
  Abstract: Deallocates the storage used by a SwitchRec list.
  Parameters: a - the head of a list of pSwitchRecs to deallocate.
                  It is set to NIL. OK if NIL before call.
--------------------------------------------------------------}
   var temp: pSwitchRec;
   begin
   while a <> NIL do
      begin
      temp := a^.next;
      dispose(a);
      a := temp;
      end;
  end;

                  

Procedure StdError(err: ErrorType; param: CString; leaveProg: Boolean);
{----------------------------------------------------------
  Abstract: Prints out an error message with a parameter and then optionally
            exits the user program.
  Parameters: err - the error type found
              leaveProg - if true then after reporting error, raises ExitProg
                          to return to the shell.  If false then simply returns
              param - parameter for the error.  The message printed is:

     ErBadSwitch    - "** <PARAM> is an invalid switch."

     ErBadCmd       - "** <PARAM> is an invalid command."

     ErNoSwParam    - "** Switch <PARAM> does not take any arguments."

     ErNoCmdParam   - "** Command <PARAM> does not take any arguments."

     ErSwParam      - "** Illegal parameter for switch <PARAM>."

     ErCmdParam     - "** Illegal parameter for command <PARAM>."

     ErSwNotUnique  - "** Switch <PARAM> is not unique."

     ErCmdNotUnique - "** Command <PARAM> is not unique."

     ErNoOutFile    - "** <PARAM>  does not have any outputs."

     ErOneInput     - "** Only one input allowed for <PARAM>."

     ErOneOutput    - "** Only one output allowed for <PARAM>."

     ErFileNotFound - "** File <PARAM> not found."

     ErDirNotFound  - "** Directory <PARAM> does not exist."

     ErIllCharAfter - "** Illegal character after <PARAM>."

     ErCannotCreate - "** Cannot create file <PARAM>."

     ErBadQuote     - "** Cannot end a line with Quote."
     
     ErAnyError     - "<PARAM>"

--------------------------------------------------------------}
  begin
  if err <> ErAnyError then Write('** ');
  Case err of
     ErBadSwitch   : Write(param,' is an invalid switch');
     ErBadCmd      : Write(param,' is an invalid command');
     ErNoSwParam   : Write('Switch ',param,' does not take any arguments');
     ErNoCmdParam  : Write('Command ',param,' does not take any arguments');
     ErSwParam     : Write('Illegal parameter for switch ',param);
     ErCmdParam    : Write('Illegal parameter for command ',param);
     ErSwNotUnique : Write('Switch ',param,' is not unique');
     ErCmdNotUnique: Write('Command ',param,' is not unique');
     ErNoOutFile   : Write(param, ' does not have any outputs');
     ErOneInput    : Write('Only one input allowed for ',param);
     ErOneOutput   : Write('Only one output allowed for ',param);
     ErFileNotFound: Write('File ',param,' not found');
     ErDirNotFound : Write('Directory ',param,' does not exist');
     ErIllCharAfter: Write('Illegal character after ',param);
     ErCannotCreate: Write('Cannot create file ',param);
     ErBadQuote    : Write('Cannot end a line with Quote');
     ErAnyError    : Write(param);
     otherwise     : Write('****** IMPOSSIBLE ERROR ******');
     end;
   if err <> ErAnyError then WriteLn('.')
   else WriteLn;
   if leaveProg then Raise ExitProgram;
   end;


Procedure CnvUpper(Var Str:CString);
{----------------------------------------------------------
  Abstract: This procedure is used to convert a string to uppercase.
  Parameters: Str is the string that is to be converted.
  Side Effects: This procedure will change Str.
****WARNING**** THIS PROCEDURE WILL SOON BE REMOVED.  USED THE PROCEDURE
                ConvUpper IN PERQ_STRING
--------------------------------------------------------------}
  Begin
  ConvUpper(str);
  End;
        
        
Function UniqueCmdIndex(Cmd:CString; Var CmdTable: CmdArray;
                        NumCmds:Integer): Integer;
{----------------------------------------------------------------
  Abstract: This procedure is used to do a unique lookup in a command
            table.
  Parameters: Cmd - the command that we are looking for.
 
              CmdTable - a table of the valid commands.  The first valid
                         command in this table must start at index 1.
 
              NumCmds - the number of valid command in the table.
  Results: This procedure will return the index of Cmd in CmdTable.  If
           Cmd was not found then return NumCmds + 1.  If Cmd was not
           unique then return NumCmds+2.
-------------------------------------------------------------------------}
    Var FoundCount,FoundIndex,I:Integer;
        Begin
        FoundCount:=0;
        For I:= 1 To NumCmds Do
            Begin
            If Pos(CmdTable[I],Cmd) = 1 Then
                Begin
                FoundCount:=FoundCount+1;
                FoundIndex:=I;
                End;
            End;
        If FoundCount = 0 Then FoundIndex:=NumCmds+1;
        If FoundCount > 1 Then FoundIndex:=NumCmds+2;
        
        UniqueCmdIndex:=FoundIndex;
        End {UniqueLookUp};
        
        
        
        
        
Procedure RemDelimiters(Var Src:CString; Delimiters:CString;
                        Var BrkChar:CString);
{-------------------------------------------------------------------------
 Abstract: This procedure is used to remove delimiters from the front
           of a string.
 Parameters:
    Src - the string from which we are to remove the delimiters.
    Delimiters - a string that contains the characters that are to
                 be considered delimiters.
    BrkChar - will hold the character that we broke on.
 Side Effects: This procedure will change both Src and BrkChar.
-------------------------------------------------------------------------}
  Var StrPos:Integer;
      SDum:CString;
  Label 1;
      Begin
      StrPos:=1;
      While StrPos <= Length(Src) Do
          Begin
          SDum:=SubStr(Src,StrPos,1);
          If Pos(Delimiters,SDum) = 0 Then GoTo 1;
          StrPos:=StrPos+1;
          End;
 1:
     If StrPos > Length(Src) Then
         Begin
         Src:='';
         BrkChar:='';
         End
     Else
         Begin
         BrkChar:=SubStr(Src,StrPos,1);
         Src:=SubStr(Src,StrPos,Length(Src)-StrPos+1);
         End;
     End {RemDelimiters};
     
     
 Procedure GetSymbol(Var Src,Symbol:CString; Delimiters:CString;
                    Var BrkChar:CString);
{-------------------------------------------------------------------------
 Abstract: This procedure is used to remove the first symbol from the beginning
           of a string.
 Parameters:
    Src - the string from which we are to remove the symbol.

    Symbol - a string that is used to return the next symbol.

    Delimiters - a string that defines what characters are to be
    considered delimiters.  Any character in this string will be used
    to terminate the next symbol.

    BrkChar - used to return the character that stopped the scan.
 Side Effects:
    This procedure will remove the first symbol from Src and place it
    into Symbol.  It will place the character that terminated the scan
    into BrkChar.
-------------------------------------------------------------------------}
 Var StrPos:Integer;
     SDum:CString;
 Label 1;
     Begin
     StrPos:=1;
     While StrPos <= Length(Src) Do
         Begin
         SDum:=SubStr(Src,StrPos,1);
         If Pos(Delimiters,SDum) <> 0 Then GoTo 1;
         StrPos:=StrPos+1;
         End;
 1:
     If StrPos > Length(Src) Then
         Begin
         Symbol:=Src;
         Src:='';
         BrkChar:='';
         End
     Else
         Begin
         BrkChar:=SubStr(Src,StrPos,1);
         Symbol:=SubStr(Src,1,StrPos-1);
         Src:=SubStr(Src,StrPos,Length(Src)-StrPos+1);
         End;
     End {GetSymbol}.
     
