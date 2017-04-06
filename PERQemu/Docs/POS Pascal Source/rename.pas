{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Rename;
{----------------------------------------------------------------------------
{
{        Rename - Rename files.
{        Don Scelza and John Strait   26 Feb 81.
{        Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{        program call:
{        
{             Rename OldName NewName
{---------------------------------------------------------------------------}


{----------------------------------------------------------------------------
{    Change Log:
{
{ 16 Feb 83  V5.5   Jerry Conner
{ Eliminate implicit setting of ask switches via confirm switches.
{ }

{ 13 May 82  V5.4   Michael R. Kristofic
{ Check for too many files (for the files array) in wild card case
{ }

{ 26 Jan 82  V5.3   brad Myers
{ Parse*Line ==> Parse*Args
{ No Continue question.
{ }

{  4 Jan 82  V5.2   ezf
{ Modified to use V1.4 of GetConfirm().
{ }

{ 29 Dec 81  V5.1   Brad Myers
{ Small bug fixes.
{ }

{ 30-Nov 81  V5.0   Brad Myers
{ New user interface from new CmdParse.
{ Error if trying to rename directory.
{ Quick sort names if wild card.
{ FSRemoveDots used
{ }

{ 17-Aug 81  V4.0   Brad Myers
{ Fixed so output file of * renames non-dir part of source.
{ Renaming to other wild card output when not wild input is error.
{ }

{ 23-Jun-81  V3.3  Brad Myers
{ Change so requires explicit yes or no
{ }

{ 27-May-81  V3.2  Brad Myers
{ Catch new exception from FSRename
{ }

{ 21-May-81  V3.1  Brad Myers
{ Pre-scan all names into big array
{ }

{ 16-May-81  V3.0  Brad Myers
{ Fix bug in rename to file already there
{ Added rename/help
{ added multi-file rename
{ }

{ 28-Mar-81  V2.0  Brad Myers
{ Uses FSAddToTitleLine
{ }

{ 19-Mar-81  V1.2  Brad Myers
{ Changed Perq.string to PERQ_String; asks if can rename to file already there.
{ Imports FileUtils instead of filesystem
{ }

{  9-Mar-81  V1.1  Don Scelza
{ Added code to set the title window.  Check format of error messages.
{ }
{---------------------------------------------------------------------------}



const Version = 'V5.5';

  imports CmdParse from CmdParse;
  imports Perq_String from Perq_String;
  imports FileUtils from FileUtils;
  imports PMatch from PMatch;
  imports PopCmdParse from PopCmdParse;
  imports QuickSort from QuickSort;
  
Const MaxFiles = 400;

Type FileAr = Array[0..MaxFiles] of SimpleName;
     pFileAr = ^FileAr;

var SrcName, DestName, SrcPattern, DestPattern: PathName;
    scanptr : ptrScanRecord;
    switches: pSwitchRec;
    ins, outs, dums: pArgRec;
    
    CmdTable: CmdArray;
    Verify, Wild, safe, gotVerify, foundOne, destWild : boolean;
    id: FileID;
    dum, destIndex: integer;
    files: pFileAr;
    fileCnt, i: integer;
    c: Char;
    s, err: CString;
    isSwitch: boolean;
         
Const AskIndex = 1;
      NoAskIndex = 2;
      ConfirmIndex = 3;
      NoConfirmIndex = 4;
      HelpIndex = 5;
      NumCmds = 5;    
  
Function Confirm(prompt: String; def: integer;cont: boolean): Boolean; forward;

Procedure Error(err: String);
{-------------------------------------------------------------------------
 Abstract: Reports an error then exits.
 Parameters: err is string to print
------------------------------------------------------------------------}  
  begin
  WriteLn(err);
  exit(Rename);
  end;


Procedure HandleSwitches(switches: pSwitchRec; inConfirm: boolean);
{---------------------------------------------------------------
 Abstract: Handles switches if any.
 Parameters: switches - the list of switches.  May be NIL.
             inConfirm - tells whether called from a Confirm.
 SideEffects: Sets Verify and safe switches
-------------------------------------------------------------------}
   var i: integer;
   begin
   while switches <> NIL do
     begin
     ConvUpper(Switches^.switch);
     i := UniqueCmdIndex(Switches^.switch, CmdTable, NumCmds);
     if (i <= NumCmds) and (switches^.arg <> '') then
        begin
        StdError(ErNoSwParam, switches^.switch, not inConfirm);
        if inConfirm then 
           if not Confirm('*  Continue? ',2, true) then exit(Rename);
        end;
     case i of
       AskIndex: begin
                 Verify := true;
                 gotVerify := true;
                 end;
       NoAskIndex: begin
                   Verify := false;
                   gotVerify := true;
                   end;
       ConfirmIndex: begin
                     safe := true;
                     end;
       NoConfirmIndex: begin
                       safe := false;
                       end;
       HelpIndex: 
        if inConfirm then
             begin
             WriteLn;
             WriteLn('         Confirm the rename by typing "yes" or "no" (or "y" or "n")');
             WriteLn('         or by pressing a mouse button to get a popup menu.');
             WriteLn('         If you have a menu, press over YES or NO to give that');
             WriteLn('         answer.  For more help, exit and type "rename/help"');
             WriteLn;
             end
        else begin
          WriteLn;
          writeln('     Rename is used to change the name of files.');
          writeln('     The source can have wild cards in it as long as the');
          Writeln('     destination has the same wild cards in the same order.');
          Writeln('     All files that match the source will be renamed by');
          Writeln('     taking the characters that match each wild card and');
          Writeln('     putting those characters in the corresponding place in');
          Writeln('     the destination.  If the source has no wild cards and');
          Writeln('     destination has exactly one *, then the non-directory');
          Writeln('     part of the source replaces the * in the destination.');
          writeln;
          writeln('     For a description of the wild cards; see Directory/Help');
          WriteLn;
          writeln('        Command line is of the form:');
          writeln('          Rename <src file name> <dest file name> {/Switch}');
          writeln('     The valid switches are:');
          for i := 1 to NumCmds do writeln('         ', CmdTable[i]);
          exit(Rename);
          end;
       NumCmds+2 : begin
                   StdError(ErSwNotUnique, Switches^.switch, not inConfirm);
                   if inConfirm then 
                      if not Confirm('*  Continue? ',2, true) then exit(Rename);
                   end;
       otherwise: begin
                  StdError(ErBadSwitch, Switches^.switch, not inConfirm);
                  if inConfirm then 
                      if not Confirm('*  Continue? ',2, true) then exit(Rename);
                  end;
      end; {case}
     switches := switches^.next;
     end;
   end;

Function Confirm(prompt: String; def: integer; cont: boolean): Boolean;
{-------------------------------------------------------------------------
 Abstract: Asks user if wants to do something.
 Parameters: prompt - string to type as prompt.
             def - default value.  1=true,2=false,3=no default.
             cont - tells whether question is "Continue? ".
 Returns: true if 'y' or false if 'n'
------------------------------------------------------------------------}  
  var i: integer;
      cSwitches: pSwitchRec;
  begin
  repeat
    if cont then Write(Chr(7));
    i := GetConfirm(NullIdleProc(**), true, prompt, def, cSwitches);
    HandleSwitches(cSwitches, true);
    if not cont then prompt := '    Confirm: ';
  until i < 3;
  Confirm := i=1;
  end;


Procedure DoOneRename(src, dest: PathName);
{-------------------------------------------------------------------------
 Abstract: Renames src to dest.
 Parameters: src is source filename; dst is dst;  neither can have wildcards
             src must already exist
 SideEffects: Changes name of src to dest.
------------------------------------------------------------------------}  
   Handler DelError(fileName: PathName);
   {-------------------------------------------------------------------------
    Abstract: Handles delete errors.
   ------------------------------------------------------------------------}  
     var ans: String[1];
     begin
     Writeln('** Unable to delete ', filename);
(*
     if wild then if Confirm('*   Continue? ',2,true) then
*)
        exit(DoOneRename);
(*
     exit(Rename);
*)
     end; {DelError}

   Handler RenError(msg: String; fileName: PathName);
   {-------------------------------------------------------------------------
    Abstract: Handles rename errors so can give useful message.
    Parameters: msg is message; filename is filename.
   ------------------------------------------------------------------------}  
     begin
     Writeln('** ', msg, filename);
(*
     if wild then if Confirm('*   Continue? ', 2,true) then
*)
        exit(DoOneRename);
(*
     exit(Rename)
*)
     end; {RenError}

   Handler RenDir(fileName: PathName);
   {-------------------------------------------------------------------------
    Abstract: Handles rename errors so can give useful message.
    Parameters: msg is message; filename is filename.
   ------------------------------------------------------------------------}  
     begin
     WriteLn('** ',filename,' is a directory; can''t rename.');
(*
     if wild then if Confirm('*  Continue? ', 1,true) then
*)
            exit(DoOneRename)
(*
                  else
     else Error('');
*)
     end; {RenError}

   Handler RenToExist(fileName: PathName);
   {-------------------------------------------------------------------------
    Abstract: Handles case where rename to existing file; if not safe or
              confirm then continues with Rename; else exits DoOneRename.
    Parameters: filename is dest that exists.
   ------------------------------------------------------------------------}  
     begin
     if safe then
       If not Confirm(Concat(Concat('* ',filename),
                             ' already exists!! Delete? '), 2, false) then
               exit(DoOneRename);
     end; {RenToExist}

  begin
  FSRemoveDots(dest);
  Write('  ', src,' ==> ',dest);
  if Verify then
     If not Confirm(' ', 1, false) then
               exit(DoOneRename)
     else
  else writeLn;
  FSRename(src, dest);
  end; {DoOneRename}


procedure GetDirName(var FileName: PathName);
{---------------------------------------------------------------
 Abstract: This procedure is used to get the directory information from
               the search pattern.
 SideEffects: Allocates and initializes scanPtr; changes filename to remove dir
-------------------------------------------------------------------}
var I, dum: Integer;
    fid: FileID;
  begin
  new(scanptr);
  scanptr^.InitialCall := true;
  scanptr^.DirName := '';
  i := RevPosC(fileName, '>');
  if i <> 0 then
     begin
     scanptr^.DirName := SubStr(fileName, 1, i);
     fileName := SubStr(fileName, i+1, length(fileName)-i);
     fid := FSLocalLookUp(scanPtr^.DirName, dum, dum);
     if fid = 0 then
        begin
        WriteLn('** Directory ',scanPtr^.dirName,' not found.');
        exit(Rename);
        end;
     end;
  end; {GetDirName}


Function CheckDestWild(var Dest: PathName; var index: Integer): boolean;
{---------------------------------------------------------------
 Abstract: Checks to see if Dest contains wild cards.  If so, then sees if
            contains exactly one *.  If so then sets destWild, if contains
            other or more wild cards, then asks user if name OK.  If no
            wild cards, then sets destWild to false
 Parameters: Dest is the destination file name to check
             index is set with index of star if exactly one
 Returns: DestWild.

***BUG*** Doesn't work for abc'**

-------------------------------------------------------------------}
  var temp: PathName;
  begin
  CheckDestWild := false;
  if not IsPattern(dest) then exit(CheckDestWild);
  index := PosC(dest, '*');
  temp := dest;
  if index <> 0 then Delete(temp, index, 1);
  if IsPattern(temp) then
      if safe then
        begin
        WriteLn('*  Destination file: "',dest,'" will contain wild cards.');
        if not confirm('*  Are you sure this is what you want to do? ',
                         2, false) then
               exit(Rename);
        if not gotVerify then verify := true;
        gotVerify := true;
        end
      else {don't even mention it}
  else if index <> 0 then CheckDestWild := true;
  end;


Handler BadPatterns;
{---------------------------------------------------------------
 Abstract: Handles the exception generated by PattMap if the source and
            destination patterns don't match; exits rename.
-------------------------------------------------------------------}
  begin
  WriteLn('** ',SrcPattern,' and ',destPattern,' do not have the same wild cards in the same order.');
  exit(Rename);
  end;


begin { Rename }
  safe := true;
  gotVerify := false;
  destWild := false;
  
  FSAddToTitleLine(Concat(Concat('Rename ', Version),
                          '.  Type Rename/Help for help'));
  CmdTable[AskIndex] :=     'ASK        ask before renaming a file.';
  CmdTable[NoAskIndex] :=   'NOASK      don''t ask before renaming files.';
  CmdTable[ConfirmIndex] := 'CONFIRM    ask before renaming to existing file.';
  CmdTable[NoConfirmIndex]:='NOCONFIRM  don''t ask before renaming to existing file.';
  CmdTable[HelpIndex] :=    'HELP       print this message.';

  c := NextID(s, isSwitch);  {remove Rename}
  if (c <> ' ') and (c <> CCR) then StdError(ErIllCharAfter, 'RENAME', true);
  if not ParseCmdArgs(ins, outs, switches, err) then Error(err);
  HandleSwitches(switches, false);
  repeat
     if (ins^.next <> NIL) then StdError(ErOneInput, 'RENAME', true)
     else if (outs^.next) <> NIL then StdError(ErOneOutput, 'RENAME', true)
     else if ins^.name = '' then
        begin
        Write('File to rename: ');
        ReadLn(s);
        if not ParseStringArgs(s, ins, outs, switches, err) then Error(err);
        HandleSwitches(switches, false);
        end;
     if (ins^.name <> '') and (outs^.name = '') then
        begin
        Write('Rename ', ins^.name,' to: ');
        ReadLn(s);
        if not ParseStringArgs(s, outs, dums, switches, err) then Error(err);
        HandleSwitches(switches, false);
        if (dums^.next <> NIL) or (dums^.name <> '') then
           begin
           WriteLn('** Specify the output file name first here.');
           outs^.name := '';
           end;
        end;
  until (ins^.name <> '') and (outs^.name <> '');
  if (ins^.next <> NIL) then StdError(ErOneInput, 'RENAME', true)
  else if (outs^.next) <> NIL then StdError(ErOneOutput, 'RENAME', true);
  SrcPattern := ins^.name;
  DestPattern := outs^.name;
  
  FSRemoveDots(SrcPattern);
  
  wild := IsPattern(srcPattern);
  if not wild then destWild := CheckDestWild(destPattern, destIndex);
  
  if not gotVerify then Verify := Wild or destWild;

  if wild then
       begin
       NEW(files);
       GetDirName(srcPattern);
       foundOne := false;
       fileCnt := 0;
       while FSScan(scanptr,files^[fileCnt],id) do
         if fileCnt < MaxFiles then fileCnt := fileCnt+1
         else StdError(ErAnyError,'** Too many files to consider.',true);
       StringSort(fileCnt-1, RECAST(files, pStrArray), true);
       for i := 0 to FileCnt-1 do
         if PattMap(files^[i],srcPattern,destPattern,destName,true) then
            begin
            foundOne := true;
            DoOneRename(Concat(scanPtr^.dirName, files^[i]), destName);
            end;
       if not foundOne then WriteLn('** No files found');
       end
  else begin
       if not RemoveQuotes(srcPattern) then StdError(ErBadQuote, '', true);
       id := FSSearch(FSSysSearchList, srcPattern, dum, dum);
       if id = 0 then WriteLn('** ',srcPattern,' not found.')
       else if destWild then
           begin
           srcName := srcPattern;
           GetDirName(srcName); {remove directory part from srcName}
           Delete(destPattern, destIndex, 1);       {remove the one star}
           Insert(srcName, destPattern, destIndex); {insert new string}
           if not RemoveQuotes(srcPattern) then StdError(ErBadQuote, '', true);
           DoOneRename(srcPattern, destPattern);
           end
       else begin
          if not RemoveQuotes(srcPattern) then StdError(ErBadQuote, '', true);
          if not RemoveQuotes(destPattern) then StdError(ErBadQuote, '', true);
          DoOneRename(srcPattern, destPattern);
          end;
       end;

end { Rename }.
