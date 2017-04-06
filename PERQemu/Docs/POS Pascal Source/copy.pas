program Copy;
{---------------------------------------------------------------------
{ Copy - ?
{ Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation.
{ Copyright (C) 1983, 1984 - Perq Systems Corporation.
{
{ Abstract:
{   Copy a file.
{  
{ PERQ is a Trademark of Perq Systems Corporation.
{---------------------------------------------------------------------}



{$Version 'Copy V5.6'}
{---------------------------------------------------------------------
  Change log:

  7-Feb-84  RJB  V5.6
 JumpLines algorithm fixed to allow copying of files greater than 512 blocks.

 22 Jan 83  RSR   V5.5   Speed up copies. Reduces head movement by
                         Reading several blocks of source before writting dest.
 13 May 82  MRK   V5.4   Check for too many files to consider - wild card case.
 26 Jan 82  ezf   V5.3   Maximum of 600 files.
                         Check for directory before asking for confirm.
                         Changed parse*line to parse*args
  4 Jan 82  ezf   V5.2   Changed to use V1.4 of GetConfirm() in PopCmdParse.
 29 Dec 81  BAM   V5.1   Small bug fixes.
  2 Dec 81  BAM   V5.0   New user interface from new CmdParse.
                         Quick sort names if wild card.
                         FSRemoveDots.
 17-Aug-81  BAM   V4.0   Fixed so output file of * copies non-dir part of
                          source.
                         Copying to other wild card output when not wild input
                          is error.
 25 Jun 81  BAM   V3.2   Fixed so source is searched for if not in current dir
                          and no wild cards
 23 Jun 81  BAM   V3.1   Confirm requires yes or no
 20 May 81  BAM   V3.0   Multi-file copy and switches
  1 Apr 81  BAM   V2.1   Hand for source device moves once per char
 28 Mar 81  BAM   V2.0   Changed to use FileUtils.FSAddToTitleLine
 19 Mar 81  BAM   V1.5   Changed FileString to PathName; Changed to use
                          FSIsFSDev to decide whether to use Stream. PERQ_Str
  
  9 Mar 81  DAS   V1.4   Added progress code.
  
  2 Mar 81  JPS   V1.3   Change FSLookup of destination file to FSEnter.
                         Add version number.

  1 Mar 81   BR   V1.2   if either file name has a colon, use Stream package
                         instead of FileSystem.  Allows copying to RSX:

 20 Feb 81   DS   V1.1   Allow prompts for files.  Give error messages
                         if something went wrong.

 ?? ??? ??  GR/RR V1.0   Create Program
----------------------------------------------------------------------}

imports System       from System;
imports CmdParse     from CmdParse;
imports FileUtils    from FileUtils;
imports Perq_String  from Perq_String;
imports UtilProgress from UtilProgress;
imports PMatch       from PMatch;
imports Stream       from Stream;
imports PopCmdParse  from PopCmdParse;
imports QuickSort    from QuickSort;
imports FileTypes    from FileTypes;
imports Memory       from Memory;

const MaxFiles = 600;
  
Type FileAr = Array[0..MaxFiles] of SimpleName;
     pFileAr = ^FileAr;

const
  Version = 'V5.6';
  MultiSize = 32;                  { Number of blocks to read before write }

var SrcName, DestName, SrcPattern, DestPattern: PathName;
    scanptr : ptrScanRecord;
  
    CmdTable: CmdArray;
    Verify, Wild, safe, gotVerify, foundOne, destWild : boolean;
    id: FileID;
    dum, destIndex: integer;
    MultiSeg : Integer;
    MultiBuf : pDirBlk;
    files: pFileAr;
    fileCnt, i: integer;
    c: Char;
    s, err: CString;
    isSwitch: boolean;
    ins, outs, dums: pArgRec;
    switches: pSwitchRec;
    pData: ptrFSDataEntry;
     
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
 Parameters: err is string to print.
------------------------------------------------------------------------}  
  begin
  WriteLn(err);
  exit(Copy);
  end;


Procedure HandleSwitches(switches: pSwitchRec; inConfirm: boolean);
{---------------------------------------------------------------
 Abstract: Handles switches if any.
 Parameters: switches - the list of switches to handle.
             inConfirm - tells whether the switches arose in a confirm.
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
           if not Confirm('  Continue? ',2, true) then exit(Copy);
        end;
     if i < HelpIndex then gotVerify := true;
     case i of
       AskIndex: Verify := true;
       NoAskIndex: Verify := false;
       ConfirmIndex: begin
                     safe := true;
                     verify := true;
                     end;
       NoConfirmIndex: begin
                       safe := false;
                       verify := false;
                       end;
       HelpIndex: 
        if inConfirm then
             begin
             WriteLn;
             WriteLn('         Confirm the Copy by typing "yes" or "no" (or "y" or "n")');
             WriteLn('         or by pressing a mouse button to get a popup menu.');
             WriteLn('         If you have a menu, press over YES or NO to give that');
             WriteLn('         answer.  For more help, exit and type "Copy/help"');
             WriteLn;
             end
        else begin
             WriteLn;
             writeln('     Copy is used to replicate a file.');
             writeln('     The source can have wild cards in it as long as the');
             WriteLn('     destination has the same wild cards in the same order.');
             WriteLn('     All files that match the source will be copied by');
             WriteLn('     taking the characters that match each wild card and');
             WriteLn('     putting those characters in the corresponding place in');
             WriteLn('     the destination.  If the source does not have wild');
             WriteLn('     cards in it, then the search list is used to try to');
             WriteLn('     find the file.  If the source has no wild cards and');
             WriteLn('     destination has exactly one *, then the non-directory');
             WriteLn('     part of the source replaces the * in the destination.');
             writeln;
             writeln('     For a description of the wild cards; see Directory/Help');
             WriteLn;
             writeln('        Command line is of the form:');
             writeln('          Copy <src file name> <dest file name> {/Switch}');
             writeln('     The valid switches are:');
             for i := 1 to NumCmds do writeln('         ', CmdTable[i]);
             exit(Copy);
             end;
          NumCmds+2 : begin
                      StdError(ErSwNotUnique, Switches^.switch, not inConfirm);
                      if inConfirm then 
                       if not Confirm('*  Continue? ',2, true) then exit(Copy);
                      end;
          otherwise: begin
                     StdError(ErBadSwitch, Switches^.switch, not inConfirm);
                     if inConfirm then 
                       if not Confirm('*  Continue? ',2, true) then exit(Copy);
                     end;
          end;
     switches := switches^.next;
     end;
  end;


Function Confirm(prompt: String; def: integer; cont: boolean): Boolean;
{-------------------------------------------------------------------------
 Abstract: Asks user if wants to do something.
 Parameters: prompt - prompt to print out.
             def - default value.  1=true, 2=false, 3=no default.
             cont - if true then question is "Continue?"
 Returns: true if 'y' or false if 'n'
------------------------------------------------------------------------}  
  var i: integer;
      cSwitches: pSwitchRec;
  begin
  repeat
    if cont then Write(chr(7));
    i := GetConfirm(NullIdleProc, true, prompt, def, cSwitches);
    HandleSwitches(cSwitches, true);
    if not cont then prompt := '    Confirm: ';
  until i < 3;
  Confirm := i=1;
  end;


Procedure DoBlockCopy(SrcFid, DstFid : FileID; Length : Integer);

var
    Blocks : Integer;
    Remaining : Integer;
    current : Integer;
    i : integer;
    JumpLines : Integer;

begin

if Length = 0
then JumpLines := 512
else if    Length > 512
     then  JumpLines := 1
     else  JumpLines := 512 div Length;

Remaining := Length;
Current := 0;

While Remaining > 0
do  begin
    if Remaining > MultiSize
    then Blocks := MultiSize
    else Blocks := Remaining;

    For i := 0 to Blocks - 1
    do  begin
        MultiBuf := MAKEPTR(MultiSeg, 256 * i, pDirBlk);
        FSBlkRead(SrcFid, Current + i, MultiBuf);
        ShowProgress(JumpLines);
        end;

    For i := 0 to Blocks - 1
    do  begin
        MultiBuf := MAKEPTR(MultiSeg, 256 * i, pDirBlk);
        FSBlkWrite(DstFid, Current + i, MultiBuf);
        ShowProgress(JumpLines);
        end;

    Remaining := Remaining - Blocks;
    Current := Current + Blocks;
    end;

end;

Procedure DoOneCopy(src, dest: PathName);
{-------------------------------------------------------------------------
 Abstract: Copies src to dest.
 Parameters: src is source filename; dest is destination;
             neither can have wildcards; src must already exist.
------------------------------------------------------------------------}  
  var dum, j: integer;
      numblks,numbits : integer;
      OutStream, InStream, dev: boolean;
      JumpLines: integer;
      srcFileID, destfileid: FileID;
      InF,OutF: Text;
      CharCount: integer;
      gotAns: boolean;
      
  Procedure DoExit(def: integer);
  {-------------------------------------------------------------------------
   Abstract: Exits from DoOneCopy or Copy.  If wild then asks user which;
             otherwise exits from Copy
  ------------------------------------------------------------------------}  
    begin
(*
    if wild then
       if Confirm('*   Continue? ', def, true) then
*)  
           exit(DoOneCopy);
(*
    exit(Copy);
*)
    end; {DoExit}
  
  Handler ResetError(fileName: PathName);
  {-------------------------------------------------------------------------
   Abstract: Prints message and asks if should abort
  ------------------------------------------------------------------------}  
    begin
    StdError(ErFileNotFound,Filename, false);
    DoExit(2);
    end;
  Handler FSNotFnd(fileName: PathName);
  {-------------------------------------------------------------------------
   Abstract: Prints message and asks if should abort
  ------------------------------------------------------------------------}  
    begin
    StdError(ErFileNotFound,Filename, false);
    DoExit(2);
    end;
  Handler FSBadName(fileName: PathName);
  {-------------------------------------------------------------------------
   Abstract: Prints message and asks if should abort
  ------------------------------------------------------------------------}  
    begin
    StdError(ErCannotCreate, Filename, false);
    DoExit(2);
    end;
  Handler RewriteError(fileName: PathName);
  {-------------------------------------------------------------------------
   Abstract: Prints message and asks if should abort
  ------------------------------------------------------------------------}  
    begin
    StdError(ErCannotCreate, Filename, false);
    DoExit(2);
    end;
  
  var s: String;
  begin
  FSRemoveDots(dest);
  OutStream := (FSIsFSDev(dest, s) <> 0);
  InStream := (FSIsFSDev(src, s) <> 0);
  if not inStream then
    if wild or destWild then
       srcFileId := FSInternalLookUp(src, NumBlks, numBits) {will be full name}
    else srcFileId := FSSearch(FSSysSearchList, src, NumBlks, numBits);
  FSRemoveDots(src);
  write('  ', src,' ==> ',dest);
  if not inStream then
     begin
     FSGetFSData(srcFileID, pData);
     if pData^.FileType = DirFile then
        begin
        WriteLn;
        WriteLn('** ',src,' is a directory; can''t copy.');
        DoExit(1);
        end;
     end;
  if Verify then if not Confirm(' ', 1, false) then exit(DoOneCopy)
                 else
  else writeLn;
  if not outStream then
     begin
     destFileId := FSInternalLookUp(dest, dum, dum);
     if (destFileId <> 0) then
        begin
        if safe then
           begin
           Write('* ',dest,' already exists!! Delete? ');
           if not Confirm('', 2, false) then exit(DoOneCopy)
           end;
        FSGetFSData(destFileID, pData);
        if pData^.FileType = DirFile then
           begin
           WriteLn('** ',dest,' is a directory; can''t overwrite.');
           DoExit(1);
           end;
        end;
     end; 
  dev := InStream or OutStream;
  if InStream then LoadBusy
  else LoadCurs;
  if dev then
     begin
     if not InStream then 
         begin
         if NumBlks = 0 then JumpLines := 512
         else if    NumBlks > 1024
              then  JumpLines := 1
              else  JumpLines := 1024 div NumBlks;
         end;
     reset(InF,src);
     rewrite(OutF,dest);
     CharCount := 0;
     while not eof(InF) do
       begin
       if Eoln(InF) then Writeln(OutF) else Write(OutF, InF^);
       Get(InF);
       CharCount := CharCount + 1;
       if InStream then ShowProgress(1)
       else if CharCount = 512 then
           begin
           ShowProgress(JumpLines);
           CharCount := 0;
           end;
       end;
     close(OutF);
     end
  else
     begin
     destFileId := FSEnter(dest);
     if NumBlks = 0 then JumpLines := 512
     else if    Numblks > 1024
          then  JumpLines := 1
          else  JumpLines := 1024 div NumBlks;

     DoBlockCopy(SrcFileID, DestFileID, Numblks);

     FSClose(destfileid,numblks,numbits);
     end;
  QuitProgress;
  end; {DoOneCopy}


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
               exit(Copy);
        if not gotVerify then verify := true;
        gotVerify := true;
        end
      else {don't even mention it}
  else if index <> 0 then CheckDestWild := true;
  end;   


procedure GetDirName(var FileName: PathName);
{---------------------------------------------------------------
 Abstract: This procedure is used to get the directory information from
               the search pattern.
 Parameters: FixFileName should have been called on FileName; it is
              changed to have directory part removed
 SideEffects: Initializes scanPtr; changes filename to remove dir
-------------------------------------------------------------------}
var I, dum: Integer;
    fid: FileID;
  begin
  new(scanptr);
  scanptr^.InitialCall := true;
  scanptr^.DirName := '';
  i := RevPosC(fileName, '>');
  if i <> 0 then begin
                 scanptr^.DirName := SubStr(fileName, 1, i);
                 fileName := SubStr(fileName, i+1, length(fileName)-i);
                 fid := FSInternalLookUp(scanPtr^.DirName, dum, dum);
                 if fid = 0 then
                    begin
                    WriteLn('** Directory ',scanPtr^.dirName,' not found.');
                    exit(Copy);
                    end;
                 end;
  end; {GetDirName}


Handler BadPatterns;
{---------------------------------------------------------------
 Abstract: Handles the exception generated by PattMap if the source and
            destination patterns don't match; exits copy.
-------------------------------------------------------------------}
  begin
  WriteLn('** ',SrcPattern,' and ',destPattern,' do not have the same wild cards in the same order.');
  exit(Copy);
  end;

begin
  CreateSegment(MultiSeg, MultiSize, 1, MultiSize);
  new(pData);
  safe := true;
  gotVerify := false;
  destWild := false;
  
  FSAddToTitleLine(Concat(Concat('Copy ', Version),
                          '.  Type /Help for help'));
  CmdTable[AskIndex] :=       'ASK         ask before copying a file.';
  CmdTable[NoAskIndex] :=     'NOASK       don''t ask before copying files.';
  CmdTable[ConfirmIndex] :=   'CONFIRM     ask before copying to existing file.';
  CmdTable[NoConfirmIndex] := 'NOCONFIRM   don''t ask before copying to existing file.';
  CmdTable[HelpIndex] :=      'HELP        print this message.';

  c := NextID(s, isSwitch);  {remove Copy}
  if (c <> ' ') and (c <> CCR) then StdError(ErIllCharAfter, 'COPY', true);
  if not ParseCmdArgs(ins, outs, switches, err) then Error(err);
  HandleSwitches(switches, false);
  repeat
     if (ins^.next <> NIL) then StdError(ErOneInput, 'COPY', true)
     else if (outs^.next) <> NIL then StdError(ErOneOutput, 'COPY', true)
     else if ins^.name = '' then
        begin
        Write('File to copy: ');
        ReadLn(s);
        if not ParseStringArgs(s, ins, outs, switches, err) then Error(err);
        HandleSwitches(switches, false);
        end;
     if (ins^.name <> '') and (outs^.name = '') then
        begin
        Write('Copy ', ins^.name,' to: ');
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
  if (ins^.next <> NIL) then StdError(ErOneInput, 'COPY', true)
  else if (outs^.next) <> NIL then StdError(ErOneOutput, 'COPY', true);
  SrcPattern := ins^.name;
  DestPattern := outs^.name;

  if destPattern[length(destPattern)] = '>' then
        AppendChar(destPattern, '*');

  wild := IsPattern(srcPattern);
  if not wild then destWild := CheckDestWild(destPattern, destIndex);
  
  if not gotVerify then Verify := Wild or destWild;

  if wild then
       begin
       NEW(files);
       foundOne := false;
       GetDirName(srcPattern);
       fileCnt := 0;
       while FSScan(scanptr,files^[fileCnt],id) do
          if fileCnt < MaxFiles then fileCnt := fileCnt+1
          else StdError(ErAnyError,'** Too many files to consider.',true);
       StringSort(fileCnt-1, RECAST(files, pStrArray), true);
       for i := 0 to FileCnt-1 do
          if PattMap(files^[i], srcPattern, destPattern, destName, true)
            then begin
                 foundOne := true;
                 DoOneCopy(Concat(scanPtr^.dirName, files^[i]), destName);
                 end;
       if not foundOne then WriteLn('** No files found.');
       end
  else if destWild then
        begin
        if (FSIsFSDev(srcPattern, s) <> 0) then
           begin
           WriteLn('** When copying from a device, you must specify the output filename explicitly.');
           exit(Copy);
           end;
        if not RemoveQuotes(srcPattern) then StdError(ErBadQuote, '', true);
        id := FSSearch(FSSysSearchList, srcPattern, i, i);
        if id = 0 then begin
                       WriteLn('** ',srcPattern,' not found.');
                       exit(Copy);
                       end;
        srcName := srcPattern;
        GetDirName(srcName); {remove directory part from srcName}
        Delete(destPattern, destIndex, 1);       {remove the one star}
        Insert(srcName, destPattern, destIndex); {insert new string}
        if not RemoveQuotes(destPattern) then StdError(ErBadQuote, '', true);
        DoOneCopy(srcPattern, destPattern);
        end
  else begin
       if not RemoveQuotes(srcPattern) then StdError(ErBadQuote, '', true);
       if not RemoveQuotes(destPattern) then StdError(ErBadQuote, '', true);
       DoOneCopy(srcPattern, destPattern);
       end;

end { Copy }.
