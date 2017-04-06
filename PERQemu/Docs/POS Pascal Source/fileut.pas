{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module FileUtils;
{----------------------------------------------------------------------
{Abstract: 
{
{ Filesystem utilities not needed by the system
 
   Written by Brad Myers.  March 5, 1981.
   
   Copyright 1983, Three Rivers Computer Corporation

----------------------------------------------------------------------}

{$Version V1.12 for POS}
{----------------------------------------------------------------------
 Change log:

 10 Feb 83  Brad Myers    V1.12  Fixed for landscape screen (longer title line)
 16 Nov 82  Bill Braucher V1.11 Fixed names for 14-character compiler.
  2 Dec 81  BAM  V1.10    Rename to its original name changes capitalization.
 30 Nov 81  BAM  V1.9     Rename of a file to its original name works.
                          Exception for renaming a directory.
                          New routine FSRemoveDots.
 23 Jun 81  BAM  V1.8     Fixed so rename when couldn't enter file leaves it in
                           old directory
  1 Jun 81  BAM  V1.7     Added Get and Set FSData procedures
 27 May 81  BAM  V1.6     Added new exception to rename to handle file exists
 19 May 81  BAM  V1.5     Added new procedure for Extension lookup written by
                            JPS;
 12 May 81  BAM  V1.4     Added exceptions;
 16 Apr 81  BAM  V1.3     Fixed so Rename xx to y>..>y works;
 28 Mar 81  BAM  V1.2     Removed SetPath routine (is in shell);
                          Changed Push and Pop to take SearchList as argument.
                          Added AddToTitleLine procedure so programs will
                           display the current path
 26 Mar 81  BAM  V1.1     Changed MakeDirectory so doesn't uppercase the name;
                            checks to see if file exists; and changes the arg
                            to be the full path name of the new directory
                          Added headers;
 19 Mar 81  BAM  V1.0     PERQ_String;
 17 Mar 81  BAM  V0.3     New procedures FSPushSearchItem and Pop;
 16 Mar 81  BAM  V0.2     Fixed MakeDirectory to do more error checking;
  5 Mar 81  BAM  V0.1     Created this module by breaking off from FileSystem;
                           Also Made FSScan faster; added FSMakeDirectory
----------------------------------------------------------------------}


{********************} Exports {********************}

imports FileSystem from FileSystem;

type
  ptrScanRecord = ^ScanRecord;
  ScanRecord    = record
                    InitialCall : boolean;
                    Blk         : DiskAddr;
                    Entry       : Integer;
                    DirName     : PathName;
                  end;
                  
Procedure FSDelete(filename: PathName);
Function  FSScan(scanptr : ptrScanRecord; var name : SimpleName;
                 var id : FileID) : boolean;
Procedure FSRename(SrcName, DestName: PathName);
Function FSMakeDirectory(var DirName: PathName): FileID;
Procedure FSSetSearchList(sList: SearchList);
Procedure FSPopSearchItem(var sList: SearchList);
Procedure FSPushSearchItem(name: PathName; var sList: SearchList);
Procedure FSAddToTitleLine(msg: String); {adds as much of msg as possible to
                                          title line after the current path}
Exception DelError(FileName: PathName);
{----------------------------------------------------------------------
  Abstract: Raised when can't delete file (because not there)
  Parameters: FileName is file that can't delete
----------------------------------------------------------------------}

Exception RenError(msg: String; FileName: PathName);
{----------------------------------------------------------------------
  Abstract: Raised when can't rename file
  Parameters: msg is reason can't rename and fileName is file with the
               problem.  To print message, use "WriteLn('** ',msg,filename);"
----------------------------------------------------------------------}

Exception MkDirErr(msg: String; dirName: PathName);
{----------------------------------------------------------------------
  Abstract: Raised when can't make a directory because
             1) a file named dirName already exists
             2) dirName cannot be entered (bad subdir part)
             3) dirName is empty
             4) dirName is ROOT.DR (reserved directory name)
  Parameters:  msg explains problem with makedir attempt;
               dirName is name attempted to use.
              Use "WriteLn('** ',msg,dirName);"
----------------------------------------------------------------------}

Exception SrchWarn(fileName: PathName);
{----------------------------------------------------------------------
   Abstract: Raised if try to Pop last item or push into last hole of the
              Search List
   Parameters: '' if Pop; name of item trying to push if Push
   Resume: ALLOWED; if resume then does the operation anyway
----------------------------------------------------------------------}

Exception SrchErr(fileName: PathName);
{----------------------------------------------------------------------
  Abstract: Raised if try to Pop empty list or push onto full list for the
              Search List
  Parameters: '' if Pop; name of item trying to push if Push
  Resume: NOT allowed
----------------------------------------------------------------------}

Function FSExtSearch(var SList : SearchList; Extensions: String; 
                     var FileName : PathName;
                     var BlksInFile, BitsInLBlk: Integer) : FileID;
                                          
Exception RenToExist(fileName: PathName);
{----------------------------------------------------------------------
  Abstract: Raised when try to rename to a file that already exists.  Not 
            raised if renaming a file to its own name (no-op).
  Parameters: fileName - new name that already exists
  Resume: ALLOWED; If you wish to rename anyway; just continue and FSRename
           will delete the DestName; In this case; you should be prepared
           to accept DelError;
----------------------------------------------------------------------}

Exception RenDir(fileName: PathName);
{----------------------------------------------------------------------
  Abstract: Raised when try to rename a directory.
  Parameters: fileName - name of the source directory.
  Resume: ALLOWED; If you wish to rename anyway; just continue and FSRename
           will do the operation.  RenToExist etc. may still be raised.
----------------------------------------------------------------------}

Procedure FSGetFSData(id: FileID; pData: ptrFSDataEntry);
Procedure FSSetFSData(id: FileID; pData: ptrFSDataEntry);
Procedure FSRemoveDots(var fname: PathName);


{********************} Private {********************}

imports FileAccess from FileAccess;
imports FileDir from FileDir;
imports Perq_String from Perq_String;
imports FileTypes from FileTypes;   
imports Screen from Screen;


Exception Impossible;
{----------------------------------------------------------------------
  Abstract: Raised when impossible things happen (i.e.; should never be
             raised)
----------------------------------------------------------------------}

const DEBUG = false;


Procedure FSDelete(filename : PathName);
{----------------------------------------------------------------------
  Abstract: Deletes filename from directory and from filesystem;  fileName is
             deleted from the current path only (not search lists) if it
             doesn't contain device or partition info
  Parameters: filename is the name of the file to be deleted
  SideEffects: filename is deleted from the current directory if it exists;
                if not then nothing is done (and the user is not notified)
  Calls: DeleteFileID; DestroySpiceSegment
  Errors: Raises DelError(fileName) if can't delete file
----------------------------------------------------------------------}
  var
    id : SegID;
  
  begin
    FixFilename(filename,true);
    if length(filename) = 0 then exit(FSDelete);
    id := DeleteFileID(filename);
    if id <> DBLZERO then DestroySpiceSegment(id)
    else Raise DelError(filename);
  end {FSDelete};


Procedure FSRename(SrcName, DestName: PathName);
{----------------------------------------------------------------------
  Abstract: Changes the name of SrcName to DestName;  both are in the current
            path (not search lists) if not fully specified 
  Parameters: SrcName is the name of the file to change and DestName is the
               name it should be given
  Returns: True if rename is successful; false if can't be done because:
            1) - destName already exists
            2) - SrcName and destName are in different partitions
            3) - SrcName doesn't exist
            4) - SrcName or DestName is malformed
  SideEffects: The name of the file corresponding to SrcName is changed
  Calls: DeleteFileID; DestroySpiceSegment
  Errors: Raises RenError(msg, fileName) - if can't rename file where message
           explains why (do "Write(' ** ',msg,fileName)" in handler)
          Raises RenToExist(DestName) - if filename already exists;  If you
              wish to rename anyway; just continue and FSRename will delete
              the DestName; In this case; you should be prepared to accept
              DelError;
----------------------------------------------------------------------}
  var
    id : SegID;
    id2 : SegID;
    PTR : ptrDiskBuffer;
    SrcPartition, DestPartition: integer;
    OK: Boolean;
    ShortDestName, ShortSrcName: PathName;
  begin
    FixFilename(SrcName,true);
    FixFilename(DestName,true);
    ShortDestName := DestName;
    OK := GetDisk(ShortDestName, DestPartition);
    if not OK then Raise RenError('Bad Partition name on ',DestName);
    ShortSrcName := SrcName;
    OK := GetDisk(ShortSrcName, SrcPartition);
    if not OK then Raise RenError('Bad Partition name on ',SrcName);
    if SrcPartition <> DestPartition then
         Raise RenError('Files not in the same partition: ',SrcName);
    ID := GetFileID(SrcName);
    if ID = DblZero then Raise RenError('File not found: ',SrcName);
    Ptr := ReadDisk(id);
    if ptr^.FSData.FileType = DirFile then Raise RenDir(SrcName);
    ID2 := GetFileID(DestName);
    if ID = ID2 then {renaming to itself}
       begin
       id := DeleteFileID(Srcname);  {remove orig capitilization}
       OK := PutFileID(DestName, ID);  {modifies DestName so can put in Fib}
       if not OK then Raise RenError('Couldn''t re-enter ',DestName);
       end
    else begin
         if ID2 <> DblZero then
            begin
            Raise RenToExist(DestName);
            FSDelete(DestName);
            ID2 := GetFileID(DestName);
            if ID2 <> DblZero then Raise Impossible; {file exists after delete}
            end;
         OK := PutFileID(DestName, ID);  {modifies DestName so can put in Fib}
         if not OK then Raise RenError('Couldn''t enter ',DestName);
         id := DeleteFileID(Srcname);
         end;
    Ptr := ChangeDisk(id); {should already be in a buffer from read above}
    Ptr^.FSData.FileName := DestName;
    FlushDisk(id);
  end {FSRename};


Function FSScan(scanptr : ptrScanRecord; var name: SimpleName;
                var id : FileID): boolean;
{----------------------------------------------------------------------
  Abstract: At each call returns the next entry in a directory.  The names
             returned are in random order. 
  Parameters: scanPtr is a pointer to a ScanRecord which controls the scan.
               At the first call, scanPtr^.InitialCall should be set to true
               and scanPtr^.dirName should be set to the directory to scan
               through.  No fields should be modified by the caller after
               the initial setting.  The dirName field of the scanPtr record
               is modified to contain the Full path name of the directory.
               name is set to the name of the file found on this call and id is
               its fileID;  scanPtr is modified after each call so the next
               call will return the next name in the directory
  Returns: True if a valid name and id returned; false if the directory has
            been exhausted in which case name and id are NOT valid
----------------------------------------------------------------------}
  var i: integer;
      ptr: ptrDiskBuffer;
      hdr: ptrHeader;
      dirID : SegID;
      
 begin
   if scanPtr^.initialCall then
      begin
        FixFileName(scanPtr^.DirName, false);
        dirID := GetFileID(scanPtr^.DirName);
        if dirID = DBLZero then 
           begin
            FSScan := false;
            exit(FSScan);
           end;
        scanPtr^.Entry := 0;
        scanPtr^.initialCall := false;
        hdr := ReadHeader(DirID);
        scanPtr^.Blk := hdr^.NextAdr;
      end;
  
  while true do
    begin
     if scanPtr^.Blk = DBLZERO then 
       begin
         FSScan := false;
         exit(FSScan);
       end;
     ptr := ReadDisk(scanPtr^.blk);
     for i := scanPtr^.entry to FILESPERDIRBLK-1 do
       begin
         if ptr^.entry[i].InUse then
           begin
             name := ptr^.Entry[i].filename;
             scanPtr^.entry := i+1;
             id   := SegIDtoFileID(ptr^.Entry[i].ID);
             FSScan := true;
             exit(FSScan);
           end;
       end;
     hdr := ReadHeader(scanPtr^.blk);
     scanPtr^.blk := hdr^.nextAdr;
     scanPtr^.entry := 0;
   end;
 end {FSScan};
 

Function FSMakeDirectory(var dirName: PathName): FileID;
{----------------------------------------------------------------------
  Abstract: Create a new directory named dirName. 
  Parameters: DirName is the name of the directory to create; the name is 
               changed to be the full path name of the directory created
  Returns: The fileID of the directory
  SideEffects: Creates a file named dirName (appending a ".DR" to end if not
                there.  Sets the FileType field to DirFile; and sets the
                FileBits to 4096
  Errors: Raises MkDirErr(msg, dirName) if
             1) a file named dirName already exists
             2) dirName cannot be entered (bad subdir part)
             3) dirName is empty
             4) dirName is ROOT.DR (reserved directory name)
            where msg describes error.  Do not continue from this signal
----------------------------------------------------------------------}
  var
    id       : SegID;
    fid      : FileID;
    ptr      : ptrDiskBuffer;
    dum      : integer;
    tempName : PathName;
    
  Handler FSBadName(fileName: PathName);
    begin
    Raise MkDirErr('Can''t create file named ',dirName);
    end;
 
  begin
   tempName := dirName;
   ConvUpper(tempName);
   if (SubStr(tempName, length(tempName)-2,3) <> '.DR') then
         begin
         AppendString(tempName,'.DR');
         AppendString(dirName,'.DR');
         end;
    if (tempName = 'ROOT.DR') then
            Raise MkDirErr('The name "ROOT" is reserved: ',dirName)
    else if (tempName = '.DR') then Raise MkDirErr('Null filename: ',dirName)
    else if FSInternalLookUp(tempName,dum,dum) <> 0 then
          Raise MkDirErr('Can''t overwrite old file with directory: ',dirName);
    FixFileName(dirName,false);  {so user can see full name}
    fid := FSEnter(dirName);  {error caught above}
    if fid = 0 then Raise Impossible;
    id := FileIDToSegID(fid);
    ptr := ChangeDisk(id); {will read FIBlk and mark as changed}
    ptr^.FSData.FileType := DirFile;
    ptr^.FSData.FileBits := 4096;
    FlushDisk(id);
    FSMakeDirectory := fid;
  end {FSMakeDirectory};  
 

Procedure FSSetSearchList(sList: SearchList);
{----------------------------------------------------------------------
  Abstract: Assign the system search list. 
  Parameters: sList is new search list.  It is a bad idea to not include
               a partition which contains a full set of system files
  SideEffects: Changes system search list
----------------------------------------------------------------------}
  begin
    FSSysSearchList := sList;
  end {FSSetSearchList};
 
 

Procedure FSPopSearchItem(var sList: SearchList);
 {------------------------------------------------------------------------
   Abstract: Removes the most recent item from the search list
   Parameters: sList is search list to pop from (it is modified)
   Errors: Raises SrchWarn('') if try to pop last item; if continue from it
            then pops it anyway;
           Raises SrchErr('') if list empty and try to pop; don't continue from
             this one
-------------------------------------------------------------------------}
  var i: integer;
      leave: boolean;
  begin
   i := 0;
   leave := false;
   repeat
     i := i+1; {start at 1}
     if i > SearchSizeList then leave := true
     else if sList[i] <> '' then leave := true;
   until leave;
   if i = SearchSizeList then Raise SrchWarn('')
   else if i > SearchSizeList then Raise SrchErr('');
   sList[i] := '';
  end {FSPopSearchItem};
  

Procedure FSPushSearchItem(name: PathName; var sList: SearchList);
 {------------------------------------------------------------------------
   Abstract: adds name to the front of the search list
   Parameters: name is new name to add to the front of the search list
               searchList is modified to have name at front
   Errors: Raises SrchWarn(name) if try to push into last item; if continue
              from it then pushes it anyway;
           Raises SrchErr(name) if list full and try to push; don't
              continue from this one
   Environment: Assumes oldest item in list is at high position (e.g. 5)
 -------------------------------------------------------------------------}
  var i: integer;
      leave: boolean;
  begin
  if sList[1] <> '' then Raise SrchErr(name);
  i := -1;
  leave := false;
  repeat
     i := i+1; {start at 0}
     if i >= SearchSizeList then leave := true
     else if sList[i+1] <> '' then leave := true;
  until leave;
  if i = 1 then Raise SrchWarn(name);
  sList[i] := name;
  end {FSPushSearchItem};


Procedure FSAddToTitleLine(msg: String); 
{-------------------------------------------------------------------------
 Abstract: adds as much of msg as possible to title line after the current path
           which is truncated to 35 characters
 Parameters: msg is string to be displayed.  The first 43 characters of it are
             displayed
 Side Effects: Changes current window's title line
------------------------------------------------------------------------}
    Const DotDot = chr(#214);
    var Str: String[255];
        left, i: Integer;
    begin
    Str := FSDirPrefix;
    left := (SNumTitleChars div 2) - 5;
    If length(Str) > left then 
       begin
       Str[left-1] := DotDot;
       Str[left] := DotDot;
       Str := Substr(Str, 1, left);
       end
    else for i := length(str) + 1 to left do
         AppendChar(str, ' ');
    Str := Concat(Str, '  ');
    Str := Concat(Str, msg);
    Str := Substr(Str, 1, SNumTitleChars);
    ChangeTitle(str);
    end; {FSAddToTitleLine}


Procedure FSGetFSData(id: FileID; pData: ptrFSDataEntry);
{-------------------------------------------------------------------------
 Abstract: Returns the FSDataEntry description of a file
 Parameters: id is the FileID for the file that data wanted for
             pData is a pointer to a data block to which the FSData is copied.
               Memory for this pointer must be allocated before the call
------------------------------------------------------------------------}
  var p: Record case Boolean of
            true: (p: pDirBlk);
            false: (b: ptrDiskBuffer);
            end;
  begin
  NEW(p.p);
  FSBlkRead(id, FIBlk, p.p);
  pData^ := p.b^.FSData;
  DISPOSE(p.p);
  end;


Procedure FSSetFSData(id: FileID; pData: ptrFSDataEntry);
{-------------------------------------------------------------------------
 Abstract: Changes the FSDataEntry of a file
 Parameters: id is the fileID of the file to be modified
             pData is the FSDataEntry to set id to.  The entire FSDataEntry
                description of id is changed, so the user should use
                FSGetFSData to read the FSDataEntry and then change the
                desired fields only
 Side Effects: Changes the FSDataEntry for id
------------------------------------------------------------------------}
  var p: Record case Boolean of
            true: (p: pDirBlk);
            false: (b: ptrDiskBuffer);
            end;
  begin
  NEW(p.p);
  FSBlkRead(id, FIBlk, p.p);
  p.b^.FSData := pData^;
  FSBlkWrite(id, FIBlk, p.p);
  DISPOSE(p.p);
  end;


Function FSExtSearch(var SList : SearchList; Extensions: String; 
                     var FileName : PathName;
                     var BlksInFile, BitsInLBlk: Integer) : FileID;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       FSExtSearch performs a breadth-first lookup of a file using a specified
{       searchlist and a list of extensions.  The search order is as follows:
{         1) Try the name with each extension in the current directory.
{         2) Repeat steps 1 in each path specified in the searchlist.
{       If the file is found, the FileName is changed to be the full file name
{       actually found.
{
{ Parameters:
{       SList        - Searchlist to use.
{       Extensions   - List of extensions to try with a single space after each
{                      extension.  E.g.  '.Pas .Micro .Cmd .Dfs '.  The string
{                      must have a single trailing space.  A single leading
{                      space or a pair of adjacent spaces causes FSExtSearch
{                      to look for the file exactly as typed (with no extension
{                      appended).  Other extra spaces are not allowed.  If
{                      Extensions does not end in a space, then one added.
{       FileName     - Name of file to find, set to be the full name of the 
{                      file that was actually found.
{       BlksInFile   - Length of file in blocks.
{       BitsInLBlk   - Bits in last block of file.
{
{ Returns:
{    0 if file not found or id of file
{
{ Errors:
{    Raises FSNotFnd if file not found
{
{-----------------------------------------------------------------------------}

   var P: integer;
       Tmp, Ext: String;
       NameDotExtension: PathName;
       PathAndName: PathName;
       FileNumber: FileID;
       I: Integer;
   label 1;
     begin { FSExtSearch }
     FileNumber := 0;
     if Filename = '' then goto 1;
     if Extensions[length(Extensions)] <> ' ' then AppendChar(Extensions, ' ');
  { try current path }
     PathAndName := FileName;
     FixFileName(PathAndName,True);
     Tmp := Extensions;
     while (FileNumber = 0) and (Tmp <> '') do
         begin
         P := PosC(Tmp,' ');
         Ext := SubStr(Tmp,1,P-1);
         Delete(Tmp,1,P);
         NameDotExtension := Concat(PathAndName, Ext);
         FileNumber := FSInternalLookUp(NameDotExtension,BlksInFile,BitsInLBlk)
         end;
     if FileNumber = 0 then
         begin
         { use the search list }
         I := 0;
         repeat
           I := I + 1;
           if SList[I] <> '' then
               begin
               Tmp := Extensions;
               PathAndName := Concat(SList[I],FileName);
               FileNumber := 0;
               while (FileNumber = 0) and (Tmp <> '') do
                   begin
                   P := PosC(Tmp,' ');
                   Ext := SubStr(Tmp,1,P-1);
                   Delete(Tmp,1,P);
                   NameDotExtension := Concat(PathAndName, Ext);
                   FileNumber :=
                      FSInternalLookUp(NameDotExtension,BlksInFile,BitsInLBlk);
                   end
               end
         until (FileNumber <> 0) or (I = SearchSizeList);
         end;
 1:  FSExtSearch := FileNumber;
     if FileNumber <> 0 then FileName := NameDotExtension
     else Raise FSNotFnd(FileName);  
     end; { FSExtSearch }


Procedure FSRemoveDots(var fname: PathName);
{------------------------------------------------------------------------
 Abstract: Removes "."s and ".."s from file name leaving correct full name
 Parameters: fname is file name.  It is changed to not have dots. 
-------------------------------------------------------------------------}
  var start: integer;
  Procedure GetName(dirName: PathName; var index: integer; var s: SimpleName);
      {------------------------------------------------------------------------
        Abstract: gets the first dir name from dirName and returns it in s
        Parameters: dirName is the directory name to look at; index is where
                    in dirName to start. s is string of directory name found;
                    index is left at > or EOS
       -----------------------------------------------------------------------}
     var j, endIndex, temp: integer;
     begin
     j := index;
     endIndex := -1;
     repeat
        if j > length(dirName) then endindex := j
        else if dirName[j] = '>' then endindex := j
        else j := j+1;
     until endindex > -1;
     if endIndex <= start then s := ''
     else if endIndex-index > 25-3 then exit(FSRemoveDots) {name too long}
     else s := SubStr(dirName, index, endIndex-index);
     index := endIndex; {at '>' or EOString}
     end; {GetName}
  
 var i, j, last, second: integer;
     s: SimpleName;
 begin
  FixFileName(fName, false);
  if length(fName) = 0 then exit(FSRemoveDots);
  if PosC(fName,':') <> 0 then
     begin
     start := PosC(fName, '>')+1;
     if start = 0 then exit(FSRemoveDots);
     end
  else exit(FSRemoveDots);
  
  i:= start;
  last :=start;
  second := start;
  repeat
    GetName(fName, i, s);  {s is assigned; exits if s would be too big}
    if s = '.' then begin { "\.|" --del "\." unless "\" = start and "|"
                                is end when del "." }
                    if (second = start) and (i>length(fName)) then j := 0
                    else j := 1;
                    Delete(fName, second-j, j+1);
                    i := start; {start over}
                    last := start;
                    second := start;
                    end
    else if s = '..' then begin { "\x/..|" --del "\x/.." unless "\" = start                                  and "|" is EOS when del "x/.."  }
                          if (last = start) and (i > Length(fName))
                              then j := 0
                          else j := 1;
                          Delete(fName, last-j, i-last+j);
                          i := start;
                          second :=start;
                          last := start;
                          end
    else if (i = second) and (i <= length(fName)) then {have ">>"}
          exit(FSRemoveDots)
    else begin
         i := i+1; {past '>'}
         last := second;
         second := i;
         end;
  until s = '';
 end. {FSRemoveDots}
