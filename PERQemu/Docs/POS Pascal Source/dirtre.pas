{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program DirTree(input,output);
{--------------------------------------------------------------------------
 Abstract:
    DirTree draws a tree on the screen showing the current directory structure
    and then allows the user to select a new path by pointing with the mouse.
 
 Written by Brad A. Myers
 
 Copyright (C) 1981, 1983 - The Three Rivers Computer Corporation
--------------------------------------------------------------------------}

{--------------------------------------------------------------------------
   Change log:
    29 Mar 83  Brad Myers  V4.1  Increase number of dirs from 100 to 150.
    14 Feb 83  Brad Myers  V4.0  Fix bug when current directory is off screen;
                                 Fix bug when last directory doesn't have room
                                   for *;
                                 Add ability to get block count for dirs;
                                 Make work for landscape monitor.
    12 Nov 82  A. G. Reinig V3.3  Removed checking of read an write
                                 pointers of the keyboard circular
                                 buffer.  Used IOCPresent instead.
    29 Dec 81  Brad Myers  V3.2  Add Help Handler, busyCursor
     9 Dec 81  Brad Myers  V3.1  Remove dots. If tree too deep, display *
    25 Nov 81  Brad Myers  V3.0  New user interface using new cmdParse.
    29 Oct 81  Brad Myers  V2.1  Fixed so char causes exit.
                                 Added DirTree cursor
    15 Sep 81  Don Scelza  V2.0  Added code to allow for path change.
     3-Jun-81  Brad Myers  V1.2  Added comments
    ??-???-81  Brad Myers  V1.1  Allowed DirTree on multiple partitions
    ??-???-81  Brad Myers  V1.0  Started
--------------------------------------------------------------------------}


const Title = 'DirTree V4.1.  Type /Help for Help';
      DirTreeCursor = 'DirTree.Cursor';
      
imports FileUtils from Fileutils;
imports Screen from Screen;
imports FileDir from FileDir;
imports PERQ_String from PERQ_String;
imports CmdParse from CmdParse;
imports allocDisk from AllocDisk;
imports IO_Others from IO_Others;
imports IO_Unit from IO_Unit;
imports UtilProgress from UtilProgress;
imports System from System;

type  DirRec = record
                 name: SimpleName;
                 fullName: PathName;
                 x,y: integer;   {of where displayed}
                 blocks: long;   {number of blocks used in this directory}
                 rightX: integer;  {end of where displayed}
                 rightXBlocks: Integer; {rightX of block count if doBlocks}
                 hasChild: boolean;
                 parent: integer;
                 level: integer;
                 fid: FileID;
              end;

type pDirStack = ^dirStack;
     dirStack = RECORD
                   dirName: PathName;
                   dirInd: integer;
                   next: pDirStack;
                   prev: pDirStack;
                END;

var dumWindR : WinRange;

    treeRoot: PathName; {dir list used while searching for directories}
    dirs: Array[1..150] of DirRec;  {dir list used for displaying directories}

    numDirs, xPos, i, maxLevels, winX, winY, winH, winW, winRX: integer;
    scanptr : ptrScanRecord;
    levelSize: array[1..9] of integer;
    pBot, pTop, pTemp: pDirStack;  
    fid, rootFid: FileID;
    dumBool: boolean;
    curLevel: integer;
    CurPathIndex: Integer;
    switchAr: CmdArray;
    
    debug, wait, doBlocks: boolean;
    pData: ptrFSDataEntry;
    
label 1;


Procedure DoHelp;
{---------------------------------------------------------------
  Abstract: Prints out help and then exits
-------------------------------------------------------------------}
  begin
  WriteLn;
  WriteLn('     DirTree is used to create a picture of the directory structure');
  WriteLn('     of your machine.  It erases the entire screen and then draws');
  WriteLn('     the picture.  Afterwards, it will allow you to select a directory');
  WriteLn('     which will then be the new path.  Do this by selecting');
  WriteLn('     a directory by pointing at it with the cursor and pressing.  Exit without');
  WriteLn('     changing the path by pressing away from a directory name or by');
  WriteLn('     typing any character.  If you specify a device, partition, or');
  WriteLn('     directory on the command line, DirTree will use that as the root.');
  WriteLn('     If you do not give a name on the command line, DirTree does not prompt.');
  WriteLn('     for one; it uses the current default device, instead.');
  WriteLn('     If there is no room for a directory at the right margin, its');
  WriteLn('     parent is marked with a "*" after the ">".  Command line is');
  WriteLn('     of the form:');
  WriteLn('                   DirTree [directory Name] {/switch}');
  WriteLn('     The switches understood by DirTree are:');
  WriteLn('         HELP');
  WriteLn('         WAIT   - Allow picking the path.  This is the default');
  WriteLn('         NOWAIT - Don''t allow picking the path; exit after displaying tree.');
  WriteLn('         BLOCKS - Count all the blocks in each directory.  Sums the');
  WriteLn('                    counts for higher level directories after a "~".  Does not');
  WriteLn('                    include overhead of files headers or directory blocks.');
  WriteLn('                    This takes about 4 times longer than a regular DirTree.');
  exit(DirTree);
  end;


Procedure AddIt(dir, name: PathName; level, parent: integer; fid: FileID);
{---------------------------------------------------------------
  Abstract: Adds a newly found directory to the lists
  Parameters: directory IN WHICH the new directory was found
              name is the simpleName of the new directory found
              level is the level of the directory found
              parent is the array index of dir (the parent of name)
              fid is the file ID of name
  SideEffects: Adds name to both directory lists
               Increments numDirs and LevelSize[level]
  Environment: Assumes lists and numDirs properly initialized
-------------------------------------------------------------------}
   var pTemp: pDirStack;
        begin

        if debug then writeLn('Dir ',name,' found in ',dir,
             '. Numdirs=',numdirs:1,' level =',level:1,' parent=',parent:1);
        
        numDirs := numDirs+1;

        NEW(pTemp);
        if pBot = NIL then pBot := pTemp;
        pTemp^.next := pTop;
        if pTop <> NIL then pTop^.prev := pTemp;
        pTemp^.prev := NIL;
        pTop := pTemp;
        pTemp^.dirInd := numDirs;
        pTemp^.dirName := Concat(dir, name);
        
        dirs[numDirs].name := name;
        dirs[numDirs].fullName := pTemp^.dirName;
        dirs[numDirs].level := level;
        dirs[numDirs].parent := parent;
        dirs[numDirs].fid := fid;
        dirs[numDirs].blocks := 0;
        dirs[numDirs].rightX := 0;
        dirs[numDirs].rightXblocks := 0;
        dirs[numDirs].hasChild := false;

        dirs[parent].hasChild := true;

        levelSize[level] := levelSize[level] + 1;
        
        end; {AddIt}


Function LongWidth(l: Long): Integer;
{---------------------------------------------------------------
  Abstract: counts the number of characters required to print the positive
            number l.
-------------------------------------------------------------------}
  var i: Integer;
  begin
  if l = 0 then LongWidth := 9
  else begin
       i := 0;
       while l > 0 do
         begin
         i := i+1;
         l := l div 10;
         end;
       LongWidth := i*9;
       end;
  end; {LongWidth}


Procedure DoOneLevel(level: integer);
{---------------------------------------------------------------
  Abstract: Displays all the directories in a partiticular level
  Parameters: level is the level to display
  SideEffects: Changes xPos
  Environment: Assumes xPos is set and dir array has been filled with all
                the items at this level
  Design: Have to do levels in order from left to right
          Assumes the width of the current font is 9
-------------------------------------------------------------------}
   var len, maxLen: integer;
       yOffset, y: integer;
       i: integer;
   begin
   yOffset := (winH-26) div (levelSize[level]+1);
   if debug then WriteLn('~~~~level ',level:1,' size=',levelSize[level]:1,
                         ' yOffset=',yOffset:1);
   y := winY+yOffset+5;
   maxLen := 0;
   for i := 1 to numDirs do
     if dirs[i].level = level then
        begin
        ShowProgress(1);
        len := 9*length(dirs[i].name);
        if len > maxLen then maxLen := len;
        if XPos+len+9 > winRX then {extra 9 is for * in case needed}
           begin
           if dirs[i].parent <> 0 then
              with dirs[dirs[i].parent] do
                 if rightX <> 0 then
                    begin
                    SSetCursor(rightx, y);
                    Write('*');
                    end;
           dirs[i].x := 0;
           dirs[i].y := 0;
           dirs[i].rightX := 0;
           dirs[i].rightXBlocks := 0;
           end
        else begin
             SSetCursor(XPos, y);
             if dirs[i].name = '' then 
               begin
               adjust(dirs[i].name, 1);
               dirs[i].name[1] := chr(#177);
               end;
             Write(dirs[i].name);
             dirs[i].x := xPos;
             dirs[i].y := y;
             dirs[i].rightX := xPos+len;
             if doBlocks then
               begin
               SSetCursor(xPos, y+13);
               Write(dirs[i].blocks:1);
               dirs[i].rightXBlocks := xPos+LongWidth(dirs[i].blocks);
               end;
             if dirs[i].parent <> 0 then
               Line(DrawLine, xPos, y-7, dirs[dirs[i].parent].rightx-2, 
                   dirs[dirs[i].parent].y-7, SScreenP);
             end;
        y := y+yOffset;
        end;
   xPos := xPos+maxLen+100;
   end;


Procedure ShowBlockSums;
{---------------------------------------------------------------
  Abstract: Shows the sums of all the block counts.
-------------------------------------------------------------------}
   var level, i: Integer;
   begin
   for level := maxLevels downto 1 do
     begin
     for i := 1 to numDirs do
      if dirs[i].level = level then
        if dirs[i].parent <> 0 then
          dirs[dirs[i].parent].blocks := dirs[dirs[i].parent].blocks+
                                         dirs[i].blocks;
     end;
   for i := 1 to numDirs do
     if dirs[i].hasChild and (dirs[i].rightXBlocks <> 0) then
       with dirs[i] do
        if rightXBlocks+LongWidth(blocks)+9 <= winRX then {extra 9 is for "~"}
          begin
          SSetCursor(rightXBlocks, y+13);
          Write('~',blocks:1);
          end;
   end;


Function UpperEqual(name1, name2: SimpleName): boolean;
{---------------------------------------------------------------
  Abstract: Tests to see if two strings are equal after converting to uppercase
  Parameters: name1 and name2 are strings to compare
  Returns: True if equal after uppercasing
-------------------------------------------------------------------}
    begin
    ConvUpper(name1);
    ConvUpper(name2);
    UpperEqual := name1=name2;
    end;


Procedure DoDirScan(dir: PathName; dirInd: integer);
{---------------------------------------------------------------
 Abstract: Scans through one directory or partition searching for sub-
            directories and adds any found to lists.  If directory doing is at
            a new level, then display the previous level
 Parameters: dir is the pathName of the directory to search through
             dirInd is the index in the directory array of dir
 SideEffects: Changes dir list and arrays
 Calls: AddIt, DoOneLevel
-------------------------------------------------------------------}
  var s: String[2];
      name: SimpleName;
      dum: integer;
      fid: FileID;
      i, disk: integer;
  label 1;
  begin

  scanptr^.InitialCall := true;
  scanptr^.dirName := dir;
      
  ShowProgress(1);
  if dirs[dirInd].level > curLevel then
     begin
     DoOneLevel(curLevel);
     curLevel := curLevel+1;
     end;
  if debug then begin
                WriteLn;
                WriteLn('in dirScan, dir=',dir,' index=',dirInd:1);
                end;
  if dir[length(dir)] = ':' then  {is a device; find all partitions}
     begin
     for i := 0 to MAXDISKS-1 do
        if UpperEqual(DiskTable[i].RootPartition, SubStr(dir, 1,length(dir)-1))
           then begin
                disk := i;
                goto 1;
                end;
     {if get to here then device not found}
       WriteLn('** Device ',dir,' not found; aborting');
       Exit(DirTree);
   1: for i := 1 to MAXPARTITIONS do
         if PartTable[i].PartInUse and (disk=PartTable[i].PartDevice) then
             begin
             name := partTable[i].partName;
             AppendChar(name, '>');
             fid := FSLookUp(Concat(dir, name), dum, dum);
             AddIt(dir, name, dirs[dirInd].level+1, dirInd, fid); 
             end;
     end {partition}
  else while FSScan(scanptr, name, fid) do  {is a directory; find subdirs}
    begin
    if doBlocks then
      begin
      FSGetFSData(fid, pData);
      dirs[dirInd].blocks := dirs[dirInd].blocks + pData^.FileBlocks;
      end;
    ShowProgress(1);
    if length(name) > 3 then
     if name[length(name)-2] = '.' then
       begin
       s := Substr(name, length(name)-1,2);
       ConvUpper(s);
       if s = 'DR' then
            begin
            name := SubStr(name, 1, length(name) -3);
            if name = '..' then
               begin
               WriteLn;
               WriteLn('** Directory ',dir,' contains a bad directory: ...DR; aborting');
               exit(DirTree);
               end;
            AppendChar(name, '>');
            AddIt(dir, name, dirs[dirInd].level+1, dirInd, fid);
            end;
       end;
    end;
 end; {DoDirScan}


Procedure BreadthFirstSearch;
{---------------------------------------------------------------
 Abstract: Does a breadth first scan through all directories, adding any
             directories found to the lists.  After finishing each level,
             displays the level on the screen
 SideEffects: Initializes dir array and lists and then fills them
 Calls: DoDirScan 
 Design: Adds directories found to the front of a list using AddIt.  Every time
         finish scanning a directory, take the next directory off the end of
         the list.  This way, all the directories at a certain level are
         guaranteed to be processed before anything at the next level.  In
         addition, this guarantees that the directories at the next level
         will be in a corresponding order.
-------------------------------------------------------------------}
   var leave: boolean;
       dir: PathName;
       dirInd, i: integer;
   
   begin

   pBot := NIL;
   pTop := NIL;
   leave := false;
   
   dir := treeRoot;
   i := RevPosC(Substr(treeRoot, 1, length(treeRoot)-1), '>');
   SSetCursor(winX+3, winY+16);
   Write('Root is ',treeRoot);
   if doBlocks then
     begin
     WriteLn;
     Write('Showing block totals');
     end;
   if i <> 0 then  dirs[1].name := SubStr(treeRoot, i+1, length(treeRoot)-i)
   else dirs[1].name := treeRoot;
   dirs[1].fullName := treeRoot;
   dirs[1].level := 1;
   dirs[1].parent := 0;
   dirs[1].fid := rootFid;
   dirs[1].blocks := 0;
   dirInd := 1;;
   numDirs := 1;
   curLevel := 1;
   
   levelSize[1] := 1;
   
   repeat
      ShowProgress(1);
      DoDirScan(dir, dirInd);
      
      if pBot <> NIL then
         begin
         dir := pBot^.dirName;
         dirInd := pBot^.dirInd;
         pTemp := pBot;
         pBot := pBot^.prev;
         if pBot <> NIL then pBot^.next := NIL;
         DISPOSE(pTemp);
         end
      else leave := true; 

  until leave;
  end;
     

Procedure ReadCmdLine;
{-----------------------------------------------------------------------
 Abstract: This procedure is used to read the command line and set the
           partition name.
 Side Effects: This procedure will change partition name;
-----------------------------------------------------------------------}
    var s, s2: CString;
        c: Char;
        isSwitch, ok: boolean;
        MyFID: FileID;
        i, dum: integer;
        ins, outs: pArgRec;
        switches: pSwitchRec;
        
    begin
    debug := false;
    wait := true;
    doBlocks := false;
    switchAr[1] := 'HELP';
    switchAr[2] := 'DEBUG';
    switchAr[3] := 'WAIT';
    switchAr[4] := 'NOWAIT';
    switchAr[5] := 'BLOCKS';
    s := '';
    s2 := '';
    c := NextId(s, isSwitch);  {remove "Dirtree"}
    if (c <> ' ') and (c <> CCR) then StdError(ErIllCharAfter, 'DirTree',true);
    ok := ParseCmdArgs(ins, outs, switches, s2);
    if not ok then StdError(ErAnyError, s2, true);
    while switches <> NIL do
       begin
       ConvUpper(switches^.switch);
       i := UniqueCmdIndex(switches^.switch, switchAr, 5);
       if (i <= 5) and (switches^.arg <> '') then
           StdError(ErNoSwParam, switches^.switch, true);
       case i of
            1 : DoHelp;
            2 : Debug := true;
            3 : wait := true;
            4 : wait := false;
            5 : doBlocks := true;
            otherwise: StdError(ErBadSwitch, switches^.switch,true);
            end;
       switches := switches^.next;
       end;
    if (outs^.next <> NIL) or (outs^.name <> '') then
       StdError(ErNoOutFile, 'DirTree', true);
    if ins^.next <> NIL then StdError(ErOneInput, 'DirTree', true);
    if ins^.name = '' then treeRoot := DefaultDeviceName
    else treeRoot := ins^.name;
    
    if treeRoot[length(treeRoot)] = ':' then rootFid := 0 {is a device name}
    else begin
         if treeRoot[length(treeRoot)] <> '>' then AppendChar(treeRoot, '>');
         FSRemoveDots(treeRoot);
         rootFid := FSLocalLookUp(treeRoot, dum, dum);
         if rootFid = 0 then StdError(ErDirNotFound,treeRoot, true);
         end;
    end;



procedure ToggleBox(Index: Integer);
{---------------------------------------------------------------
{
{ Abstract:
{    Toggle the box that surrounds the directory who's
{    information is in Dirs[I].
{
{ Parameters:
{    Index if the index into Dirs of the box to change.
{
{---------------------------------------------------------------}
    begin
    with dirs[index] do
     if rightX <> 0 then
       begin
       RasterOp(RNot, rightX-x+5, 15, x-4, y-14, SScreenW, SScreenP,
                                    x-4, y-14, SScreenW, SScreenP);
       end;
    end;

procedure ToggleHighLight(Index: Integer);
{---------------------------------------------------------------
{
{ Abstract:
{    Toggle the HighLight that surrounds the directory who's
{    information is in Dirs[I].
{
{ Parameters:
{    Index if the index into Dirs of the HighLight to change.
{
{---------------------------------------------------------------}
    begin
    with dirs[index] do
     if rightX <> 0 then
       begin
       RasterOp(RNot, rightX-x+5, 15, x-4, y-14, SScreenW, SScreenP,
                                    x-4, y-14, SScreenW, SScreenP);
       RasterOp(RNot, rightX-x+9, 19, x-6, y-16, SScreenW, SScreenP,
                                    x-6, y-16, SScreenW, SScreenP);
       end;
    end;

Function Track: integer;
{-------------------------------------------------------------
{
{ Abstract:
{    Track the pen through the tree.  Draw a box around any
{    directory that you pass over.
{
{ Parameters:
{    When there is a penn press the X and Y values will be
{    placed into X and Y.
{
{-----------------------------------------------------------}
  var CurX, CurY, DirX, DirY, DirRightX, CurIndex, Index, x,y: Integer;
  label 1;

  Handler HelpKey(var retStr: Sys9s);
    begin
    SReadCursor(x,y);
    SSetCursor(winX+3, winY+winH-13);
    SChrFunc(RNot);
    Write(' Press to change path or exit; any key to exit; for more help, type DirTree/Help.');
    SChrFunc(RRpl);
    SSetCursor(x,y);
    goto 1;
    end;

    begin
    CurIndex := 0;
    while not TabSwitch do
        begin
        if IOCPresent( Keyboard ) then exit(DirTree);  {char ==> exit}
        IOReadTablet(CurX, CurY);
        for Index := 1 to NumDirs do
            begin
            DirX := Dirs[Index].X;
            DirY := Dirs[Index].Y;
            DirRightX := Dirs[Index].RightX;
            if (CurX >= DirX) and (CurX <= DirRightX) and 
               (CurY >= (DirY - 13)) and (CurY <= DirY) then
                begin
                if Index <> CurIndex then
                    begin
                    if CurIndex <> 0 then ToggleHighLight(CurIndex);
                    ToggleHighLight(Index);
                    CurIndex := Index;
                    end;
                GoTo 1;
                end;
            end;
        if CurIndex <> 0 then ToggleHighLight(CurIndex);
        CurIndex := 0;
  1:    end;
    if CurIndex <> 0 then ToggleHighLight(CurIndex);
    Track := curIndex;
    end;
            
        

procedure ChangePath;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to change the current path to
{    the one pointed to be the user.
{
{---------------------------------------------------------------------}
  var I: Integer;
      begin
      i := Track;
      if i <> 0 then
              begin
              if CurPathIndex <> 0 then ToggleBox(CurPathIndex);
              ToggleBox(I);
              FixFileName(Dirs[I].FullName, false);
              FSSetPrefix(Dirs[I].FullName);
              exit(Changepath);
              end;
      end;
           
procedure GetCursor;
{-----------------------------------------------------------------------
{
{ Abstract:
{    Reads the cursor out of a file and loads it if found.  If not found then
{    no effect.
{
{---------------------------------------------------------------------}
   var fid: FileID;
       blks, bits: integer;
       tree: CurPatPtr;
   begin
   fid := FSLookUp(DirTreeCursor, blks, bits);
   if fid <> 0 then begin
                    New(0,4,tree);
                    FSBlkRead(fid, 0, RECAST(tree, pDirBlk));
                    IOLoadCursor(tree, 0, 0);
                    Dispose(tree);
                    end;
   end;
       

begin

  debug := false;
  
  FSAddToTitleLine(title);
  ReadCmdLine;

  if doBlocks then New(pData);
  
  NEW(scanPtr);
  For i := 1 to 9 do
    levelSize[i] := 0;

  Write(chr(12));
  GetWindowParms(dumWindR, winX, winY, winW, winH, dumBool);
  winRX := winX+winW-6;

  xPos := winX+3;
  winY := winY+3;
  
  LoadBusy;
  BreadthFirstSearch;
 
  maxLevels := 9;
  while levelSize[maxLevels] = 0 do
    maxLevels := maxLevels-1;
  
  for i := curLevel to maxLevels do
    DoOneLevel(i);
  
  if doBlocks then ShowBlockSums;
  
  SSetCursor(winX+3,winY+29);
  
  CurPathIndex := 0;
  fid := FSLocalLookup(FSDirPrefix, i, i);
  for i := 1 to numDirs do
    if dirs[i].fid = fid then
         begin
         ToggleBox(i);
         CurPathIndex := i;
         goto 1;
         end;

1:

QuitProgress;
if IOCPresent( Keyboard ) or (not wait) then exit(DirTree);

GetCursor;

IOSetModeTablet(RelTablet);
IOCursorMode(TrackCursor);


ChangePath;

end.
