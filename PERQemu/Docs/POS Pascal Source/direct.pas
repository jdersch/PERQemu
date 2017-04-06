{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program Direct;

{-------------------------------------------------------------------------
{
{ Abstract:
{    Direct is the directory listing program for the system.
{
{ Written by: C-MU Spice group.
{
{    Copyright 1981, 1982, 1983  Three Rivers Computer Corporation
{
{-------------------------------------------------------------------------}

{-------------------------------------------------------------------------
{    Change log:
{
{ 16 Nov-82  V4.6  Bill Braucher
{ Fixed names for 14-character compiler.
{ }

{ 12 May-82  V4.5  Michael R. Kristofic
{ Check for too many files in directory.
{ }

{ 25 Jan-82  V4.4  Brad Myers
{ Dir ~files bug.
{ Dir of one file => all.
{ Bigger file array.
{ }

{ 18 Jan-82  V4.3  Brad Myers
{ Make /sort=size/size faster.
{ Make /size the default for /sort=size.
{ }

{ 28 Dec-81  V4.2  Brad Myers
{ Put free list in file for /All.
{ Print directory when file not found.
{ Allow any wild card in directory part.
{ }

{ 22 Dec-81  V4.1  Brad Myers
{ Fixed Sorting algorithm.
{ }

{  9 Dec-81  V4.0  Brad Myers
{ Change to use new cmdParse stuff.
{ Look up name if not end in > to see if directory.
{ New sort by time or size.
{ }

{ 17-Aug-81  V3.3  Brad Myers
{ Changed to allocate file lists out of the heap so can tell if won't fit.
{ }

{ 23-Jun-81  V3.2  Brad Myers
{ Fixed counter of number of files and directories to not include the
{  directories passed through but not used
{ }

{ 19-May-81  V3.1  Brad Myers
{ New exceptions
{ Fix help message to tell about new wild cards
{ Changed the LISTDIRECTORY switch to list or not directories with no matches
{ }

{ 16-Apr-81  V3.0  Brad Myers
{ Added ability to list multiple directories matching the pattern (but NOT
{  multiple partitions or devices);
{ Added switch LISTDIRECTORIES which lists directories if match dirSpec but not
{  spec
{ Added switch for multicolumn or single column listing
{ }

{ 30-Mar-81  V2.1  Brad Myers
{ Changed SpiceDir to FileDir
{ }

{ 28-Mar-81  V2.0  Brad Myers
{ Uses FSAddToTitleLine.  Outputs to file in alpha order;
{ tells when directory name part is bad
{ }

{ 19-Mar-81  V1.6  Brad Myers
{ Changed to PERQ_String; Search => PMatch; use FSScan from FileUtils.
{ }

{ 11-Mar-81  V1.5  Don Scelza
{ Added code to allow direct to work with sub directories.
{ }

{ 10-Mar-81  V1.4  Don Scelza
{ Added Brad's changes to the scanner and sort routiens.
{ }

{ 3-Mar-81   V1.3  John Strait
{ Expand the "All" form of the directory to print FileType, LastBlk, full
{   creation time, update time, and access time.
{ Report version number and current date.
{ Don't do an FSLookUp on files because that sets the access time and thus
{   makes printing the access time kind of silly.
{ }

{ 2-Mar-81   V1.2  Don Scelza
{ Changed switch parsing.  Added Help switch.
{ }

{ 1-Mar-81   V1.1  Brian Rosen
{ Added switches and printing of time and date.
{ }

{ ??-??-81   V1.0  C-MU Spice group.
{ Created the Direct program.
{ }
{-------------------------------------------------------------------------}

const Version =      'V4.6';
      Title = 'Direct V4.6. Type "Direct/Help" for help.';

imports System from System;
imports CmdParse from CmdParse;
imports FileUtils from FileUtils;
imports Perq_String from Perq_String;
imports PMatch from PMatch;
imports ReadDisk from ReadDisk;
imports FileDir from FileDir;
imports Clock from Clock;
imports AllocDisk from ALlocDisk;
imports UtilProgress from UtilProgress;
imports Memory from Memory;


Const MaxDirEntries = 600;

Type BigNameArray = array[1..MaxDirEntries] of SimpleName;
     pBigNameArray = ^BigNameArray;
     
     IntArray = array[1..MaxDirEntries] of integer;
     pIntArray = ^IntArray;
     
     pDirStack = ^dirStack;
     dirStack = RECORD
                   dirName: PathName;
                   next: pDirStack;
                   prev: pDirStack;
                END;
      
     sortHow = (fakeSort, noSort, nameSort, createSort, upDateSort,
                accessSort, sizeSort);

     FakeLong = PACKED RECORD
                        case integer of
                               1: (l: Long);
                               2: (bits: integer;
                                   blks: integer);
                               3: (lsecond: 0..29;  {5} {except for low bit}
                                   lminute: 0..59;  {6}
                                   lhour:   0..23;  {5}
                                   lunused:  0..1;   {1}
                                   lday:     1..31;  {5}
                                   lmonth:   1..12;  {4}
                                   lyear:    0..63   {6}
                                  );
                       END;

    LongFakeArray = array[1..MaxDirEntries] of FakeLong;
    pLongArray = ^LongFakeArray;
    pCmdArray = ^CmdArray;
    
Const StrSiz = (WordSize(BigNameArray) + 2*WordSize(IntArray) + 255) div 256;
      LongSiz = (WordSize(LongFakeArray) + 2*WordSize(IntArray) + 255) div 256;

var
  pTop, pBot, pTemp : pDirStack;
  dirHasStar        : boolean;
  totFiles          : integer;
  totBlocks         : integer;
  totDirectories    : integer;
  allDirs           : integer;
  dir, spec, dirSpec: PathName;

var
  howSort   : sortHow;
  scanptr : ptrScanRecord;
  stkindx   : integer;
  SearchMask: PathName;
  up1,up2   : SimpleName;
  l1, l2    : LONG;
  upperNameArray : pBignameArray;
  longAr    : pLongArray;
  namearray : BignameArray;
  indx      : IntArray;
  stacki    : pIntArray;
  stackj    : pIntArray;
  idarray   : IntArray;
  randomseed: integer;
  All       : boolean;
  TotalBlks : integer;
  Buffer    : ptrDiskBuffer;
  TempString,
  CurrentTime,
  CreateTime,
  UpdateTime,
  AccessTime: string;
  Switches  : pCmdArray;
  SortVal  : pCmdArray;
  OutFile   : Text;
  OutFileName : PathName;
  NumMatch  : Integer;
  
const DebugConst = true;
  
const Delimiters = ' /:';
      FNDelimiters = ' /'; 
      
      FastIndex = 1;
      SizeIndex = 2;
      AllIndex = 3;
      SortIndex = 4;
      DelIndex = 5;
      PartIndex = 6;
      ListDirsIndex = 7;
      OneColumnIndex = 8;
      MultiColumnIndex = 9;
{$IFC DebugConst THEN}
      HelpIndex = 10;
{$ELSEC}
      DebugIndex = 10;
      HelpIndex = 11;  {** Add new commands BEFORE help}
{$ENDC}
      FileIndex = 12;  {*** Just for the special error message}
      NumCmds = FileIndex;
      NumSortCmd = ord(SizeSort);
      

{$IFC DebugConst THEN}
      debug = false;
{$ELSEC}
var debug: boolean;
{$ENDC}

Type HowMuchType = (mUnspecified, mFast, mAll, mSize);

var DoSwitch: packed record 
                 case boolean of
                        true: (w: integer);
                        false: (del, part, listDirs,
                                oneColumn, fileOut: boolean;
                                howMuch: HowMuchType
                               );
              end;


{$R-}


Function FindBase(fileName: PathName; VAR dir, dirSpec, spec: PathName;
                     VAR dirHasStar: boolean): boolean;
{----------------------------------------------------------------------------
Abstract: gets the directory name from filename and returns the directory
          part and the fileName part.  If no fileName part then returns 
          true else false.  ("Star" here means any wild card.)
Parameters: fileName is the user-supplied fileName which should contain one
            or more wildcards and should have the device and partition on
            the front.  dir is the directory part of the filename to
            the left of the star (includes device and partition).
            DirSpec is the directory part to the right and including the
            leftmost star.  If it would be null, set to '*>'.
            Spec is the rest.  If spec would be null, then set to '*'.
            If a directory has a star, then dirHasStar set to true else false.
            If no partition or device, then error set to true, else false.
            If device or partition has a star, then assumed to be part of
            valid name so not called a wildcard.
Returns: True if all OK else false if error
----------------------------------------------------------------------------}
  var i, len, partIndex, index: integer;
      state : (start, devFound, partFound, inStar, leave);
      s: String[1];
  begin
  len := Length(fileName);
  FindBase := false; {assume error}
  state := start;
  i := 1;
  Adjust(s, 1);
  dirHasStar := false;
    repeat
    if i > len then if state < partFound then exit(FindBase)  {error}
                    else state := leave
    else if fileName[i] = ':' then if state = start then state := devFound
                                   else exit(FindBase)  {error}
    else begin
         s[1] := fileName[i];
         if IsPattern(s) then if state >= partFound then state := inStar
                              else {ok to have *'s in dev and part}
         else if (fileName[i] = '>') then 
              case state of
                 start : exit(FindBase); {error}
                 devFound : begin
                            partIndex := i;
                            state := partFound;
                            end;
                 partFound: partIndex := i;
                 inStar: begin
                         dirHasStar := true;
                         index := i;
                         end;
                 end; {case}
         end;
  i := i+1;
  until state = leave;
 
  FindBase := true; {no errors}

  dir := SubStr(fileName, 1, partIndex); {part with no stars}
  if dirHasStar then dirSpec := SubStr(fileName, partIndex+1, index-partIndex)
  else begin
       dirSpec := '*>';
       index := partIndex;
       end;
  if len = index then spec := '*'  { is "...foo>" }
  else spec := SubStr(fileName, index+1, len-index);
  
  IF debug then WriteLn('fileName: ',fileName,' dir: ',dir,
                        ' dirSpec: ',dirSpec,' spec: ',spec,' dirHasStar: ',
                        dirHasStar);
  end; {FindBase}


procedure InitSwitches;
{--------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to set the initial state of the
{    directory program.
{
{ Side Effects:
{    This procedure will change some of the imnternal state of 
{    the program.
{
{---------------------------------------------------------------------}
var I: Integer;
    begin
    NEW(Switches);
    Switches^[FastIndex] :=     'FAST:        Print a short directory.';
    Switches^[SortIndex] :=     'SORT:        Specify what to sort on.';
    Switches^[SizeIndex] :=     'SIZE:        Print size information.';
    Switches^[AllIndex] :=      'ALL:         Give all information.';
    Switches^[HelpIndex] :=     'HELP:        Print this message.';
    Switches^[DelIndex] :=      'DELIMITER:   Output file names as <name> | <name>';
    Switches^[PartIndex] :=     'PARTITIONS:  Give partition information.';
    Switches^[ListDirsIndex] := 'LISTDIRECTORIES: Show directories with no matches.';
    Switches^[OneColumnIndex]:= 'ONECOLUMN:   Print all in one column; implied by File.';
    Switches^[MultiColumnIndex]:='MULTICOLUMN: Print in 4 columns.';
    
{$IFC not DebugConst THEN}
    Switches^[DebugIndex] := 'DEBUG';
{$ENDC}
    Switches^[FileIndex] := 'FILE';  {*** Just for the special error message}
    end;


procedure InitSort;
{--------------------------------------------------------------------
{ Abstract:
{    Allocates and initializes the sort cmd array.
{---------------------------------------------------------------------}
  begin
  NEW(sortVal);
  SortVal^[ord(noSort)] :=     'NOSORT:     Don''t sort the directory.';
  SortVal^[ord(nameSort)] :=   'NAME:       Sort by name of the file.';
  SortVal^[ord(accessSort)] := 'ACCESSDATE: Sort by last access (read) date.';
  SortVal^[ord(upDateSort)] := 'UPDATEDATE: Sort by last update (write) date.';
  SortVal^[ord(createSort)] := 'CREATEDATE: Sort by create date.';
  SortVal^[ord(sizeSort)] :=   'SIZE:       Sort by file size.';
  end;
    

procedure Init;
{--------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to set the initial state of the
{    directory program.
{
{ Side Effects:
{    This procedure will change some of the imnternal state of 
{    the program.
{
{---------------------------------------------------------------------}
   begin   
   DoSwitch.w := 0;
   DoSwitch.howMuch := mUnspecified;
    
   OutFileName := '';
   howSort := nameSort;
   sortVal := NIL;
   switches := NIL;
   
{$IFC not DebugConst THEN}
   debug := false;
{$ENDC}
   end;


procedure DoHelp;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to supply the user with help information.
{
{-----------------------------------------------------------------------}
var I: Integer;
  begin
  WriteLn;
  writeln('     Direct is used to list the files in the File System.');
  writeln('     Valid direct commands have the form:');
  writeln('       Direct Dirs>FileSpec ~ outFile /<Switch>.../<Switch>');
  Writeln('     Both the directory part (Dirs) and file part (FileSpec) can');
  Writeln('     have wild cards.  The Dirs part is matched against all');
  Writeln('     directories and the FileSpec matched against all leaf files'); 
  WriteLn;
  Writeln('     If an output file is specified, then the directory listing will');
  Writeln('     be directed to that file:  Use "infile <SPACE or ~> outfile".');
  WriteLn;
  writeln('     The wild cards are as follows:');
  Writeln('       "*" matches 0 or more characters');
  Writeln('       "&" matches 1 or more characters');
  Writeln('       "#" matches exactly 1 character');
  Writeln('       "''0" matches any digit');
  Writeln('       "''A" or "''a" matches any alphabetic');
  Writeln('       "''@" matches any non-alphanumeric');
  Writeln('       "''*" matches "*"; other wild cards can be quoted also');
  WriteLn;
  writeln('     The valid values for <Switch> are:');
  if switches = NIL then InitSwitches;
  for I := 1 to HelpIndex do writeln('       ',Switches^[I]);
  WriteLn;
  Writeln('     The Sort switch requires an argument.  Specify as "/Sort=arg".');
  Writeln('     The valid arguments for Sort are: ');
  if sortVal = NIL then InitSort;
  for i := 1 to NumSortCmd do Writeln('       ',SortVal^[i]);
  exit(Direct);
  end;
    

procedure HandleSwitches(var switch: pSwitchRec);
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to scan the switches off of the command line.
{
{ Environment:
{    This procedure assumes that a call to RemDelimiters has been made.
{      (first slash has been removed).
{
{ Side Effects:
{    This procedure will change DoSwitch.
{
{------------------------------------------------------------------------}
var Indx: Integer;
    begin
    if switches = NIL then InitSwitches;
    while switch <> NIL do
       begin
       ConvUpper(switch^.switch);
       Indx := UniqueCmdIndex(switch^.switch, Switches^, NumCmds);
       if (indx <= NumCmds) and (indx <> SortIndex) then
          if switch^.arg <> '' then StdError(ErNoSwParam, switch^.switch,true);
       case Indx of
          AllIndex: begin
                    DoSwitch.HowMuch := mAll;
                    DoSwitch.Part := true;
                    end;
          SizeIndex: DoSwitch.HowMuch := mSize;
          PartIndex: DoSwitch.Part := true;
          DelIndex: begin
                    DoSwitch.Del := true;
                    DoSwitch.FileOut := true;
                    DoSwitch.OneColumn := true;
                    end;
          FastIndex: DoSwitch.HowMuch := mFast;
          HelpIndex: DoHelp;
          SortIndex: begin
                     if sortVal = NIL then InitSort;
                     if switch^.arg = '' then StdError(ErSwParam,'SORT',true);
                     ConvUpper(switch^.arg);
                     indx :=UniqueCmdIndex(switch^.arg, SortVal^, NumSortCmd);
                     if indx <= NumSortCmd then 
                         begin
                         howSort:=RECAST(indx, SortHow);
                         if DoSwitch.HowMuch = mUnSpecified then
                            if howSort = SizeSort then
                                   DoSwitch.HowMuch := mSize
                            else if howSort >= createSort then
                                   DoSwitch.HowMuch := mAll; 
                         end
                     else StdError(ErSwParam,'SORT',true);
                     end;
          ListDirsIndex : DoSwitch.ListDirs := true;
          OneColumnIndex : DoSwitch.OneColumn := true;
          MultiColumnIndex : begin
                             if DoSwitch.Del then
                                StdError(ErAnyError,'** Cannot have multi-column list with delimiters',true);
                             DoSwitch.OneColumn := false;
                             end;

{$IFC not DebugConst THEN}
          DebugIndex : Debug := true;
{$ENDC}

          FileIndex: StdError(ErAnyError,'** Use "~fileName" to send output to a file.', true);
          NumCmds+2: StdError(ErSwNotUnique,switch^.switch,true);
          otherwise: StdError(ErBadSwitch, switch^.switch, true);
          end; {case}
       switch := switch^.next;
       end;  {while loop}
    end;


Procedure HandleLine;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to read the command line and set the
{    filename and switches.
{
{ Side Effects:
{    This procedure will change SearchMask and the switch array;
{
{-----------------------------------------------------------------------}
  var fid: FileID;
      i: integer;
      temp: PathName;
      ins, outs: pArgRec;
      switch: pSwitchRec;
      err: String;
      ok: boolean;
      s: CString;
      isSwitch: boolean;
      c: Char;
  begin
  err := '';
  c := NextString(UsrCmdLine, s, isSwitch);   {remove "direct"}
  ok := ParseCmdArgs(ins, outs, switch, err);
  if not ok then StdError(ErAnyError,err,true);
  HandleSwitches(switch);
  if (outs^.next <> NIL) then StdError(ErOneOutput, 'Direct',true);
  if ins^.next <> NIL then StdError(ErOneInput, 'Direct',true);
  if outs^.name <> '' then
     begin
     doSwitch.oneColumn := true;
     doSwitch.fileOut := true;
     outFileName := outs^.name;
     end;
  if ins^.name = '' then SearchMask := '*'
  else SearchMask := ins^.name;
  if (SearchMask[length(searchMask)] <> '>') and
          (SearchMask[length(searchMask)] <> '*')
            then begin
                 temp := SearchMask;
                 AppendChar(temp, '>');
                 fid := FSLookUp(temp, i, i);
                 if fid <> 0 then SearchMask := temp;
                 end;
  end;
    
        
procedure Sort1(f,l : integer; var cut1,cut2: integer);
{----------------------------------------------------------------------
{
{ Abstract:
{    Procedure used to sort the directory entries.
{
{---------------------------------------------------------------------}
  var
    i,j : integer;
    temp: integer;
    a   : string;
    k   : integer; {cut}
    
  begin
    i := f;
    j := l;
    k := randomseed mod (l-f+1);
    k := k+f;
    if (k > l) or (k < f) then 
      WriteLn('** Internal Error');
    randomseed := randomseed + 1;
    a:= upperNamearray^[indx[k]];
    
    while i < j do
      begin
        up1 := upperNamearray^[indx[i]];
        up2 := upperNamearray^[indx[j]];
        while (up2 >= a) and (j >= f) do 
          begin
            j := j-1;
            if j >= f then
              begin
                up2 := upperNamearray^[indx[j]];
              end;
          end;
        while (up1 < a) and (i <= l) do 
          begin
            i := i+1;
            if i <= l then
              begin
                up1 := upperNamearray^[indx[i]];
              end;
          end;
        if i < j then
          begin
            temp := indx[i];
            indx[i] := indx[j];
            indx[j] := temp;
            if (i = k) then k := j;
          end;
      end;
    temp := indx[k];
    indx[k] := indx[j+1];
    indx[j+1] := temp;
    cut1 := i;
    cut2 := j;
  end;

 
   

procedure Sort(i,j: integer);
{----------------------------------------------------------------------
{
{ Abstract:
{    Procedure used to sort the directory entries.
{    Algorithm from "The Design and Analysis for Computer Algorithms"
{    by Aho, Hobcroft and Ullman.
{
{---------------------------------------------------------------------}
  var
    cut1,cut2,k,l : integer;
    Str : String;
  
  begin
    for k := 1 to j do {conv all to upper case}
       begin
        Str := nameArray[k];
        for l := 1 to Length(Str) do
            Str[l] := chr(LAnd(ord(Str[l]), #337));  {cheap uppercase}
        UpperNameArray^[k] := Str;
       end;

    randomseed := 1;
    if i >= j then exit(Sort);
    stkindx := 1;
    stacki^[stkindx] := i;
    stackj^[stkindx] := j;
    while stkindx >= 1 do
      begin
        i := stacki^[stkindx];
        j := stackj^[stkindx];
        stkindx := stkindx - 1;
        Sort1(i,j,cut1,cut2);
        if (cut2 >= i) and (i < cut1-1) and ((cut1-1-i) > 0) then
          begin {maintaining stack; find what to do next}
            stkindx := stkindx + 1;
            stacki^[stkindx] := i;
            stackj^[stkindx] := cut1-1;
          end
        else cut2 := cut2+1;
        if (cut1 <= j) and (cut2+1 < j) and ((j-cut2-1) > 0) then
          begin
            stkindx := stkindx + 1;
            stacki^[stkindx] := cut2+1;
            stackj^[stkindx] := j;
          end;
      end;
  end;

        
procedure LngSort1(f,l : integer; var cut1,cut2: integer);
{----------------------------------------------------------------------
{
{ Abstract:
{    Procedure used to sort the directory entries as longs.
{
{---------------------------------------------------------------------}
  var
    i,j : integer;
    temp: integer;
    a   : LONG;
    k   : integer;
    
  begin
    i := f;
    j := l;
    k := randomseed mod (l-f+1);
    k := k+f;
    if (k > l) or (k < f) then 
      WriteLn('** Internal Long Error');
    randomseed := randomseed + 1;
    a:= longAr^[indx[k]].l;
    
    while i < j do
      begin
        l1 := longAr^[indx[i]].l;
        l2 := longAr^[indx[j]].l;
        while (l2 >= a) and (j >= f) do 
          begin
            j := j-1;
            if j >= f then
              begin
                l2 := longAr^[indx[j]].l;
              end;
          end;
        while (l1 <  a) and (i <= l) do 
          begin
            i := i+1;
            if i <= l then
              begin
                l1 := longAr^[indx[i]].l;
              end;
          end;
        if i < j then
          begin
            temp := indx[i];
            indx[i] := indx[j];
            indx[j] := temp;
            if (i = k) then k := j;
          end;
      end;
    temp := indx[k];
    indx[k] := indx[j+1];
    indx[j+1] := temp;
    cut1 := i;
    cut2 := j;
  end;

 
   

procedure AssignTime(i: integer; time: TimeStamp);
{----------------------------------------------------------------------
{
{ Abstract:
{    Assigns the ith index in longAr to be time. Moved around so can sort
{    on long array.
{
{---------------------------------------------------------------------}
   begin
   with longAr^[i], time do
      begin
      lyear := year;
      lmonth := month;
      lday := day;
      lhour := hour;
      lminute := minute;
      lsecond := second div 2;
      lunused := 0;
      end;
   end;


procedure LongSort(i,j: integer);
{----------------------------------------------------------------------
{
{ Abstract:
{    Procedure used to sort the directory entries as longs.
{    howSort should be one of createSort, upDateSort, accessSort, sizeSort);
{
{---------------------------------------------------------------------}
  var
    cut1,cut2,k,l : integer;
    lng : Long;
  
  begin
    for k := 1 to j do {set up the array with values}
       begin
       FSBlkRead(idarray[k],FIBlk,Recast(Buffer,pDirBlk));
       with Buffer^.FSData do
          case howSort of
                createSort: AssignTime(k, FileCreateDate);
                upDateSort: AssignTime(k, FileWriteDate);
                accessSort: AssignTime(k, FileAccessDate);
                sizeSort: begin
                          longAr^[k].blks := FileBlocks;
                          longAr^[k].bits := FileBits;
                          end;
                end; {case}
       end;

    randomseed := 1;
    if i >= j then exit(LongSort);
    stkindx := 1;
    stacki^[stkindx] := i;
    stackj^[stkindx] := j;
    while stkindx >= 1 do
      begin
        i := stacki^[stkindx];
        j := stackj^[stkindx];
        stkindx := stkindx - 1;
        LngSort1(i,j,cut1,cut2);
        if (cut2 >= i) and (i < cut1-1) and ((cut1-1-i) > 0) then
          begin
            stkindx := stkindx + 1;
            stacki^[stkindx] := i;
            stackj^[stkindx] := cut1-1;
          end
        else cut2 := cut2+1;
        if (cut1 <= j) and (cut2+1 < j) and ((j-cut2-1) > 0) then
          begin
            stkindx := stkindx + 1;
            stacki^[stkindx] := cut2+1;
            stackj^[stkindx] := j;
          end;
      end;
  end;

        
procedure DoSort(i,j: integer);
{----------------------------------------------------------------------
{
{ Abstract:
{    Sorts the directory entries.
{    If there is not enough room in memory, then leaves
{    names unsorted.
{
{---------------------------------------------------------------------}

   var k: integer;
   begin
   if howSort = noSort then
      for k := 1 to 600 do
        indx[k] := k
   else if howSort = nameSort then Sort(i,j)
   else LongSort(i,j);
   end;

 

procedure OneColDir;
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to output file names to a file.
{
{---------------------------------------------------------------------}
  var JumpLine, i, t, inc: Integer;
    begin  
      if howSort <= nameSort then 
        begin
        t := 1;
        inc := 1;
        end
      else begin
           t := NumMatch-1;
           inc := -1;
           end;
      if NumMatch <= 1 then
        JumpLine := 1204
      else
        JumpLine := 1024 div (NumMatch - 1);
      for i := 1 to NumMatch-1 do
         begin
         ShowProgress(JumpLine);
         Write(namearray[indx[t]]);
         if DoSwitch.Del then 
              Write(' | ', namearray[indx[t]]);
         t := t+inc;
         writeln;
         end;
    end;


procedure ShortFastDir;
{---------------------------------------------------------------------
{
{ Abstract:
{    This pocedure is used to supply a simple short directory.
{
{---------------------------------------------------------------------}
  var JumpLine, len, xpos, realIndex, numRows, k, j, l: Integer;
      down: boolean;
    begin  
    down := howSort > nameSort;
    if NumMatch <= 1 then
        JumpLine := 1204
    else
        JumpLine := 1024 div (NumMatch - 1);
    numrows := (NumMatch - 1 + 3) div 4;
    for k := 1 to numrows do
        begin
        xpos := 0;
        for j := 1 to 4 do
            begin
            realindex := (j - 1)*numrows + k;
            if realindex <= (NumMatch - 1) then
                begin
                if down then realIndex := numMatch-realIndex;
                ShowProgress(JumpLine);
                Write(namearray[indx[realindex]]);
                len := length(namearray[indx[realindex]]);
                xpos := xpos + len;
                if len >= 20 then begin
                                  Write('  ');
                                  xpos := xpos+2;
                                  end
                else for l := 1 to 20 - (xpos mod 20) do 
                        begin
                        Write(' ');
                        xpos := xpos + 1;
                        end;
                end;
            end;
        WriteLn;
        end
   end;


procedure LongSlowDir;
{----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to do a long directory.
{
{----------------------------------------------------------------------}
  var JumpLine, t, j, inc, xpos, l: Integer;
      needRead: boolean;
  begin
  xpos := 0;
  if howSort <= nameSort then begin
                              j := 1;
                              inc := 1;
                              end
  else begin
       j := NumMatch-1;
       inc := -1;
       end;
 needRead := NOT ((howSort = sizeSort) and (DoSwitch.HowMuch = mSize));
 if NumMatch <= 1 then JumpLine := 1204
 else JumpLine := 1024 div (NumMatch - 1);
 if DoSwitch.HowMuch=mAll then
      WriteLn(' Name                Blocks  Bits  LastBlk  Kind  Creation   Update     Access');
 TotalBlks := 0;
 for t := 1 to NumMatch-1 do
      begin
      if needRead then
           FSBlkRead(idarray[indx[j]],FIBlk,Recast(Buffer,pDirBlk))
      else begin
           Buffer^.FSData.FileBlocks := longAr^[indx[j]].blks;
           Buffer^.FSData.FileBits := longAr^[indx[j]].bits;
           end;
      with Buffer^, FSData do
         begin
         if DoSwitch.HowMuch=mAll then WriteLn;
         ShowProgress(JumpLine);
         Write(' ',namearray[indx[j]]);
         if DoSwitch.Del then  Write(' | ',namearray[indx[j]]);
         xpos := length(namearray[indx[j]]);
         for l := xpos to 18 do 
              Write(' ');
         Write(' ');
         TotalBlks := TotalBlks + FileBlocks;
         if DoSwitch.HowMuch=mSize then Write(FileBlocks, FileBits:6)
         else if DoSwitch.HowMuch=mAll then
             begin
             StampToString(FileCreateDate,CreateTime);
             StampToString(FileWriteDate,UpdateTime);
             StampToString(FileAccessDate,AccessTime);
             Write(FileBlocks:6, FileBits:6, LastBlk:9, FileType:6);
             TempString := SubStr(CreateTime,1,9);
             Write('  ', TempString);
             TempString := SubStr(UpdateTime,1,9);
             Write('  ', TempString);
             TempString := SubStr(AccessTime,1,9);
             Write('  ', TempString);
             WriteLn;
             Write(' ':48);
             TempString := SubStr(CreateTime,11,8);
             Write('  ', TempString);
             TempString := SubStr(UpdateTime,11,8);
             Write('   ', TempString);
             TempString := SubStr(AccessTime,11,8);
             Write('   ', TempString);
             end
         end;
      WriteLn;
      j := j+inc;
      end;
 WriteLn;
 WriteLn('Total ',TotalBlks:1,' Blocks');
    
 totBlocks := totBlocks+totalBlks;
    
 end;


function CheckIfDir(var dir, fullDir, dirSpec: PathName;
                      name: PathName): boolean;
{---------------------------------------------------------------
{
{ Abstract:
{    sees if name is a directory and if so, checks to see if matches dirSpec;
{     if so, adds to Top of dir stack 
{
{ Parameters:
{    dir is dir name in without partition name
{    fulldir is full name
{    dirSpec is pattern to match directories to 
{    name is file name that may be a directory
{
{ Returns:
{    true if file was dir and accepted else false;
{
{ Side Effects:
{    Alters stack of dirs if name is dir
{-------------------------------------------------------------------}
  var s: String[3];
      pTemp: pDirStack;
      tempName: PathName;
    Procedure AddIt;
        begin
        NEW(pTemp);
        if pBot = NIL then pBot := pTemp;
        pTemp^.next := pTop;
        if pTop <> NIL then pTop^.prev := pTemp;
        pTemp^.prev := NIL;
        pTop := pTemp;
        pTemp^.dirName := Concat(fullDir, name);
        if debug then writeln('~~~NEW Dir found is ',pTemp^.dirName);
        CheckIfDir := true;
        end; {AddIt}
  begin
  CheckIfDir := false;
  s := Substr(name, length(name)-2,3);
  ConvUpper(s);
  if s = '.DR' then
     begin
     name := SubStr(name, 1, length(name)-3);  {remove .DR}
     AppendChar(name, '>');
     if all then AddIt
     else begin
          tempName := Concat(dir,name);
          if debug then WriteLn('~~checking dir "',tempName,'" against spec: ',
                dirSpec);
          if PattMatch(tempName,dirSpec,true) then AddIt;
          end;
     end;
  end; {CheckIfDir}


Procedure RemovePartition(var dir: PathName);
{---------------------------------------------------------------
{
{ Abstract:
{    Removes device and partition from dir so can do match 
{
{-------------------------------------------------------------------}
  var i: integer;
  begin
  i := PosC(dir, '>');
  if i = 0 then writeLn('***WHERE HAS THE PARTITION GONE?? ',dir)
  else dir := SubStr(dir, i+1, length(dir)-i);
  end;


procedure DoDirScan(spec, dirSpec, dir: PathName; dirHasStar: Boolean;
                    VAR tooMany : Boolean);
{---------------------------------------------------------------
{
{ Abstract:
{    Gets all files matching pattern into global array 
{
{ Parameters:
{    SearchMask is pattern to match against
{    dir is dir inside of
{    dirHasStar tells whether to do complex stuff
{
{ Returns:
{    tooMany = true if there are too many files matching the pattern
{              in the directory (for the file array to handle), else false
{
{ Environment:
{    Assumes ScanPtr set up with correct directory
{
{ Side Effects:
{    Sets globals NumMatch and namearray
{    Alters stack of dirs if dirHasStar and dir found that matches
{-------------------------------------------------------------------}
  var fullDir: PathName;
      isDir, told: Boolean;
  begin
  isDir := false;
  tooMany := false;
  NumMatch := 1;
  fullDir := dir;
  RemovePartition(dir);
  while (FSScan(scanptr, namearray[NumMatch], idarray[NumMatch])) do
    begin
      if dirHasStar then isDir := CheckIfDir(dir, fullDir, dirSpec,
                                                 namearray[NumMatch]);
      if All then
          begin
          indx[NumMatch] := NumMatch;
          if NumMatch < MaxDirEntries then NumMatch := NumMatch + 1
                                      else tooMany := true;
          end
      else
       begin
       if PattMatch(namearray[NumMatch],spec,true) then
        begin
          indx[NumMatch] := NumMatch;
          if NumMatch < MaxDirEntries then NumMatch := NumMatch + 1
                                      else tooMany := true;
        end;
       end;
    end; { while }
  end;
    

Procedure ProcessOneDir(spec, dirSpec, dir: PathName; dirHasStar: boolean);
{---------------------------------------------------------------
{
{ Abstract:
{    Prints one directory to file or screen 
{
{ Parameters:
{    SearchMask is pattern to match against
{    dir is directory to match in
{    dirHasStar tells whether to worry about updating list of directories
{      and to use the more complex checking
{-------------------------------------------------------------------}
      var fid: FileID;
          dum: integer;
          tooMany : Boolean;
      begin
      
      fid := FSInternalLookup(dir, dum, dum);
      if fid = 0 then
        begin
        WriteLn;
        StdError(ErDirNotFound,dir,false);
        exit(ProcessOneDir);
        end;

      scanptr^.InitialCall := true;
      scanptr^.dirName := dir;
      
      
      DoDirScan(spec, dirSpec, dir, dirHasStar, tooMany);
      
      if DoSwitch.ListDirs or (numMatch > 1) then
         begin
         WriteLn;
         WriteLn('   --- In ',dir,' --- ');
         WriteLn;
         
         if tooMany then
           begin
           Writeln('** Directory has too many files.');
           Writeln('** All files will not be listed.');
           Writeln('** Delete some files or rename them to another',
                   ' directory.');
           Writeln;
           end;
      
         DoSort(1, NumMatch-1);
      
         if DoSwitch.HowMuch <> mFast then LongSlowDir
         else if DoSwitch.OneColumn then OneColDir
              else ShortFastDir;
        
         writeln;  
         NumMatch := NumMatch - 1;
         if NumMatch = 0 then writeln('** No files found.')
         else if NumMatch = 1 then writeln('1 file found.')
         else begin
              if tooMany then
                begin
                Writeln('** Directory has too many files.');
                Writeln('** All files were not listed.');
                Writeln('** Delete some files or rename them to another',
                        ' directory.');
                Writeln;
                end;
              writeln(NumMatch:1, ' files found.');
              end;
      
         totFiles := totFiles+NumMatch;
         totDirectories := totDirectories+1;
         end
      else if (NumMatch = 1) and (not dirHasStar) then
              begin
              WriteLn;
              WriteLn('   --- In ',dir,' --- ');
              WriteLn;
              WriteLn('** No files found.');
              end;
        
      allDirs := allDirs + 1;
      end; {ProcessOneDir}


Procedure BreadthFirstSearch(spec, dirSpec, dir: PathName);
{---------------------------------------------------------------
{
{ Abstract:
{    Goes through all directories and files that match spec starting at dir
{     and prints their contents 
{
{ Parameters:
{    spec is pattern to match against
{    dir is directory to match in
{-------------------------------------------------------------------}
   var leave: boolean;
       
   begin

   totFiles := 0;
   totBlocks := 0;
   totDirectories := 0;
   allDirs := 0;

   pBot := NIL;
   pTop := NIL;
   leave := false;
   
   repeat
      ProcessOneDir(spec, dirSpec, dir, true);
      
      if pBot <> NIL then
         begin
         dir := pBot^.dirName;
         pTemp := pBot;
         pBot := pBot^.prev;
         if pBot <> NIL then pBot^.next := NIL;
         DISPOSE(pTemp);
         end
      else leave := true; 

  until leave;
  
  WriteLn;
  Write(' Grand Total: ');
  if DoSwitch.HowMuch <> mFast then
     begin
     Write(totBlocks:1,' block');
     if totBlocks <> 1 then write('s');
     Write(' in ');
     end;
  Write(totFiles:1, ' file');
  if totFiles <> 1 then write('s');
  Write(' in ',totDirectories:1, ' director');
  if totDirectories <> 1 then write('ies')
  else write('y');
  Write(' out of ',allDirs:1, ' director');
  if allDirs <> 1 then write('ies')
  else write('y');
  WriteLn(' scanned.');
  end;
     

Procedure DoAllocate;
{----------------------------------------------------------------------
 Abstract:  Allocates memory for sorting arrays if possible.
---------------------------------------------------------------------}
  Handler FullMemory;
   {----------------------------------------------------------------------
    Abstract:
       If memory is full, then mark seg global variable to tell no sorting
           and print message.
   ---------------------------------------------------------------------}
    begin
    WriteLn;
    WriteLn('*** No room in memory to allocate sorting arrays! ');
    WriteLn('*** Names will not be sorted.');
    WriteLn;
    howSort := noSort;
    exit(DoAllocate);
    end;
 var seg: integer;
 begin
 if howSort = noSort then exit(DoAllocate);
 if howSort = nameSort then 
     begin
     CreateSegment(seg, StrSiz, 1, StrSiz+1);
     NEW(seg, 1, upperNameArray);
     end
 else begin
      CreateSegment(seg, LongSiz, 1, LongSiz+1);
      NEW(seg, 1, longAr);
      end;
 NEW(seg, 1, stacki);
 NEW(seg, 1, stackj);
 end;


Procedure MyDisplayPartitions;
{----------------------------------------------------------------------
 Abstract:  Displays partitions to correct output fileDisplays partitions
            to correct output file.
---------------------------------------------------------------------}
   var i,j: integer;
   begin
   for i := 0 to MaxDisks-1 do
      with DiskTable[i] do
        if InUse then
           begin
           WriteLn('Device #',i:1,'  Device Name: ',RootPartition);
           for j := 1 to MAXPARTITIONS do
              with PartTable[j] do
                 if PartInUse and (PartDevice = i) then
                     WriteLn('             ',PartName:8,': Start = ',
                             AddrToField(PartStart):10:-10,' End = ',
                             AddrToField(PartEnd):10:-10,' Free = ',
                             IntDouble(PartNumFree));
           end;
  end;


begin

  FSAddToTitleLine(title);
  Init;
  HandleLine;

  LoadCurs;
  
  if DoSwitch.FileOut then
      begin
      if outFileName = '' then
         begin
         Write('File for output: ');
         Readln(outFileName);
         end;
      writeln('Output will be directed to file ', OutFileName);
      rewrite(Output, OutFileName);
      end;
            

  new(scanptr);
  DoAllocate;
  
  WriteLn;
  FSRemoveDots(SearchMask);
  if FindBase(SearchMask, dir, dirSpec, spec, dirHasStar) then
     begin
        begin
        if (spec = '*') and (dirSpec = '*>') then all := true
        else all := false;
        if not(dirHasStar or IsPattern(dirSpec) or
              (DoSwitch.HowMuch<>mUnSpecified)) then DoSwitch.HowMuch := mAll;
        if DoSwitch.HowMuch = mUnspecified then DoSwitch.HowMuch := mFast
        else if (DoSwitch.HowMuch <> mFast) or (howSort > nameSort)
             then New(0, 256, Buffer);
        GetTString(CurrentTime);
        WriteLn;
        Write('Direct ', Version, '   ', CurrentTime);
        if not dirHasStar then begin
                               WriteLn('   Files matching ', spec,' : ');
                               ProcessOneDir(spec, '', dir, false);
                               end
        else begin
             WriteLn;
             WriteLn('   Files matching ', spec,' in directories matching ',
                     dir,dirSpec,' : ');
             BreadthFirstSearch(spec, dirSpec, dir);
             end;
        end
     end
  else WriteLn('** Filename ',SearchMask,' is malformed.');
        
WriteLn;
if DoSwitch.Part then MyDisplayPartitions;

    
if DoSwitch.fileOut then 
    begin
    close(Output);
    end;
end.

