{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FileDir;
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{ The directory structure for PERQ FileSystem
{ Written by:  CMU Spice Group
{
{     Copyright 1981, 1982, 1983 Three Rivers Computer Corporation
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
  Versions:  
     V2.6  20-Apr-81  John Strait Use DiskSegment for allocation.
     V2.5   3-Apr-81  Brad Myers  Fixed bug when creating file with name that
                                   contains ..
     V2.4  30-Mar-81  Brad Myers  Changed name from SpiceDir
                                   Changed length of Default strings
     V2.3  26-Mar-81  Brad Myers  Added comments
     V2.2  19-Mar-81  Brad Myers  PERQ_String
     V2.1  11-Mar-81  Brad Myers  Fixed Get- Put-InDir so don't read filename
                                   if not inuse
     V2.0   9-Mar-81  Brad Myers  Changed directory syntax to dev:part>dir>fn
     V1.1   1-Mar-81  Brad Myers  Changed to smaller HASHFRAMESIZE and smaller
                                    entry size
     V1.0  ??-???-??  CMU people  Started
-----------------------------------------------------------------------------}

{************************}Exports{**********************************}

imports FileDefs from FileDefs;

function  GetFileID(name : PathName) : SegID;
function  PutFileID(var name : PathName; id : SegID) : boolean;
function  DeleteFileID(name : PathName) : SegID;
function  GetDisk(var name : PathName; var partition : integer) : boolean;

var
  DefaultPartitionName : SimpleName; {includes device name and ends in a ">" }
  DefaultDeviceName    : SimpleName; {ends in a colon}


{************************}Private{**********************************}

const
  DEBUG = false;
  
imports FileAccess from FileAccess;
imports AllocDisk from AllocDisk;
imports Perq_String from Perq_String;
imports DiskIO from DiskIO;

const
  MAXPARSELEVEL =   10; {max num of dirs in a name = 9 (1 for filename) }
  HASHFRAMESIZE =   31;

type

  ParseVector = record
                  NumFiles : integer;
                  Filename : array [0..MAXPARSELEVEL-1] of SimpleName;
                end;


Function FileHash(name : SimpleName) : integer;
{-----------------------------------------------------------------------------
  Abstract: converts a simple filename into the hash function to use with it
  Parameters: name is name to hash
  Returns: hash in range 0..HASHFRAMESIZE-1
-----------------------------------------------------------------------------}
  var
    sum,i : integer;

  begin
    sum := 0;
    ConvUpper(name);
    for i := 1 to length(name) do
      sum := sum + ord(name[i]);
    FileHash := sum mod HASHFRAMESIZE ;
  end;


Function GetIDFromDir(dirid: SegID; name : SimpleName; deleteit : boolean) : SegID;
{-----------------------------------------------------------------------------
  Abstract: Looks up name in a directory and deletes it if specified
  Parameters: dirID    - SegID of directory to look in
              name     - simpleName to look up there
              deleteIt - tells whether to remove the entry from the directory
  Returns: DBLZERO if name not in directory else the SegID of the file
  SideEffects: Removes entry from file if deleteIt
  Design: Hashes the name and uses that as the logical block number in the
          directory to look in.  Searches through that block looking for name
          (converting both name and entry to uppercase).  If not there then
          adds HASHFRAMESIZE to the block number and reads that block. Decides
          name not there when reads an entirely empty block
  Calls: FileHash, ReadSpiceSegment, ConvUpper, WriteSpiceSegment (if deleteit)
  NOTE: Counts on the fact that reading non-existant blocks returns all zeros
         and that zero is FALSE;
        THIS ALGORITHM MAY FAIL to find a valid entry if there have been a lot
         of deletes and no enters.
-----------------------------------------------------------------------------}
  var
    base,hashindx,indx : integer;
    i                  : integer;
    ptr                : ptrDiskBuffer;
    valid              : boolean;
    str, teststr       : SimpleName;

  begin
    base := 0;
    str := name;
    ConvUpper(str);
    
    hashindx := FileHash(name);
    if debug then WriteLn('in GetIDFromDir, name =',name,' hash =',hashindx:1);
    new(DiskSegment,1,ptr);
    while true do
      begin
        indx := hashindx + base;
        ReadSpiceSegment(dirid,indx,1,ptr);
        valid := false;
        for i := 0 to FILESPERDIRBLK-1 do
          begin
            with ptr^.Entry[i] do
              begin
                valid := valid or InUse;
                if DEBUG then WriteLn('Entry[',i:1,'] = ',filename,' Used=',InUse);
                if InUse then
                   begin
                    teststr := Filename;
                    ConvUpper(teststr);
                    if str = teststr then
                      begin
                        if DEBUG then WriteLn('found as entry ',i:1,' id = ',AddrToField(id):1);
                        GetIDFromDir := ID;
                        if deleteit then
                          begin
                            InUse := false;
                            WriteSpiceSegment(dirid,indx,1,ptr);
                          end;
                        dispose(ptr);
                        exit(GetIDFromDir);
                      end; {found it}
                   end; {inUse}
              end;
          end;
        if not valid then
          begin
            GetIDFromDir := DBLZERO;
            dispose(ptr);
            exit(GetIDFromDir);
          end;
        base := base + HASHFRAMESIZE;
      end;
  end;


Procedure PutIDInDir(dirid: SegID; name : SimpleName; id : SegID);
{-----------------------------------------------------------------------------
  Abstract: adds an entry to a directory
  Parameters: dirID - SegID of directory to add the entry to
              name  - simpleName of file to enter and
              id    - the SegId of the file name
  SideEffects: Adds an entry to the directory for dirID
  Errors: IT IS AN ERROR TO PutIDInDir if already there!!  THIS IS, HOWEVER,
          ONLY SOMETIMES CAUGHT by this procedure.  When it is, DiskError is
          called
  Design: Hashes the name and uses that as the logical block number in the
          directory to look in.  Searches through that block looking for name
          (converting both name and entry to uppercase).  If finds name then
          error else if finds an empty slot then enters name and id and sets
          InUse.  If not there and block full, then adds HASHFRAMESIZE to
          block number and tries that block
  Calls: FileHash, ReadSpiceSegment, ConvUpper, WriteSpiceSegment
  NOTE: Counts on the fact that reading non-existant blocks returns all zeros
         and that zero is FALSE;
-----------------------------------------------------------------------------}
  var
    base,hashindx,indx : integer;
    i                  : integer;
    ptr                : ptrDiskBuffer;
    str, teststr       : SimpleName;

  begin
    base := 0;
    str := name;
    ConvUpper(str);
    hashindx := FileHash(name);
    new(DiskSegment,1,ptr);
    while true do
      begin
        indx := hashindx + base;
        ReadSpiceSegment(dirid,indx,1,ptr);
        for i := 0 to FILESPERDIRBLK-1 do
          begin
            if DEBUG then WriteLn('Entry[',i:1,'] = ',ptr^.Entry[i].filename,' Used=',ptr^.Entry[i].InUse);
            if ptr^.Entry[i].InUse then
                   begin
                    teststr := ptr^.Entry[i].Filename;
                    ConvUpper(teststr);
                    if str = teststr then
                      begin
                       Raise DiskError('PutIdInDir when already there'); (***)
(***THE FOLLOWING CODE DOESN'T WORK**
                        ptr^.Entry[i].ID := id; {keep old name capitilization}
                        WriteSpiceSegment(dirid,indx,1,ptr);
                        dispose(ptr);
                        exit(PutIDInDir);
*************************************)
                      end; {found it}
                   end {inUse}
            else {not InUse--works only if check if file in dir before put}
                  begin
                    ptr^.Entry[i].Filename := name;
                    ptr^.Entry[i].ID := id;
                    ptr^.Entry[i].InUse := true;
                    WriteSpiceSegment(dirid,indx,1,ptr);
                    dispose(ptr);
                    exit(PutIDInDir);
                  end; {not inUse}
          end; {for loop}
        base := base + HASHFRAMESIZE;
      end; {while loop}
  end; {PutIDInDir}


Function GetDisk(var name : PathName; var partition : integer) : boolean;
{-----------------------------------------------------------------------------
  Abstract: Given a name, remove the device and partition specification and
             find the partition number
  Parameters: name - the full file name to parse; the device and partition
               are optional.  The device and partition if there are removed
               from the name string;
              partition - set to the partition specified or the default
  Returns: False if specified device or partition malformed or not there
  SideEffects: Mounts the partition if not already
  Calls: FindPartition, MountPartition
-----------------------------------------------------------------------------}
  var
    indx, indx2 : integer;
    partname  : SimpleName;
    devicename: SimpleName;  
      
  begin
    if DEBUG then WriteLn('GetDisk: name = ',name);
    GetDisk  := false;
    
{defPartname includes the device name and ends in a ">" }
{defdevName ends in a colon}

    if length(name) = 0 then exit(GetDisk);
    indx := PosC(name, ':');
    if indx > 1 then {a device was specified}
        devicename := substr(name,1,indx) {including the colon}
    else devicename := DefaultDeviceName;
    
    
    if indx >= 1 then {have a partition name}
      begin
        indx2 := PosC(name, '>');
        if (indx2-indx-1 > MAXPARTCHARS) or (indx2 = 0) or (indx2 <= indx) then
          exit(GetDisk);
        partname := substr(name,indx+1,indx2-indx);
        name:=substr(name,indx2+1,length(name)-indx2); {remove dev and part}
        partname := concat(devicename,partname);
      end
    else
        partname := DefaultPartitionName;

    if DEBUG then WriteLn('devName=',deviceName,' partName = ',partName);

    partition := FindPartition(partname);
    
    if partition = 0 then exit(GetDisk);
    
    GetDisk := true;
    if not PartTable[partition].PartMounted then 
      partition := MountPartition(partname);
  end;


Function ParseFilename(name : PathName; var pv : ParseVector; 
                       var partition : integer) : boolean;
{-----------------------------------------------------------------------------
  Abstract: Parses all the directories off of name and returns them in order
            in an array of strings
  Parameters: name - the full file name to parse;
              pv - an array of the directories of name; the
               last entry in pv is the simplename of
               the file.  All directories along the path have ".DR" appended
               to end.  If the last char of name is ">", the last simplename
               will be looked up as a directory.  ".." and "." are parsed
               out of the name and do not show up in pv ("." refers to current
               directory and ".." refers to parent directory)
              partition - set to the partition specified or the default
  Returns: False if specified device or partition malformed or not there or 
            any of the directories or simpleNames are longer than 25 characters
  Calls: GetDisk
-----------------------------------------------------------------------------}
  var
    indx : integer;
    ok   : boolean;

  begin
    IF DEBUG then WriteLn('Parsing ', name);
    ParseFilename := false;
    ok := GetDisk(name,partition);
    if not ok then exit(ParseFilename);
    
    if PosC(name,'>')=1 then name := SubStr(name, 2, length(name)-1);
    
    pv.NumFiles := 0;
    while length(name) > 0 do
      begin
        indx := PosC(name,'>');
        if indx <> 0 then
          begin
            if indx > 25-3 then exit(ParseFilename);
            pv.Filename[pv.NumFiles] := Concat(substr(name,1,indx-1),'.DR');
            name := substr(name,indx+1,length(name)-indx);
            if pv.Filename[pv.numFiles] = '..DR' then {no change}
            else if pv.Filename[pv.numFiles] = '...DR' then 
                     if pv.NumFiles > 0 then pv.NumFiles := pv.NumFiles-1
                     else {peg at root}
                 else pv.NumFiles := pv.NumFiles + 1;
          end
        else
          begin
            if name = '.' then {no change}
            else if name = '..' then 
                     if pv.NumFiles > 0 then pv.NumFiles := pv.NumFiles-1
                     else  {empty name}
                 else begin
                      if length(name) > 25 then exit(ParseFilename);
                      pv.Filename[pv.NumFiles] := name;
                      pv.NumFiles := pv.NumFiles + 1;
                      end;
            Adjust(name,0);
          end;
      end;
  ParseFilename := true;
  end; {ParseFileName}
  

Function GetRootDirID(partition : integer) : SegID;
{-----------------------------------------------------------------------------
  Abstract: returns the root directory for a partition
  Parameters: partition is the partition to find the root for
  Returns: The SegID of the root
  NOTE: No checking is done on the validity of partition arg
-----------------------------------------------------------------------------}
  begin
    GetRootDirID := PartTable[partition].PartRootDir;
  end;


Function GetFileID(name : PathName) : SegID;
{-----------------------------------------------------------------------------
  Abstract: Find the SegID for name (does a lookUp)
  Parameters: name - the full name (including all directories and optional
               device and partition) of the file to look up
  Returns: The SegID of the file or DBLZERO if not there or mal-formed
  Calls: ParseFilename, GetRootDirID, GetIDFromDir
-----------------------------------------------------------------------------}
  var
    pv  : ParseVector;
    i   : integer;
    id  : SegID;
    partition: integer;
    ok  : boolean;

  begin
    GetFileID := DBLZERO;
    ok := ParseFilename(name,pv,partition);
    if not ok then exit(GetFileID);
      
    id := GetRootDirID(partition);
    for i := 0 to pv.NumFiles-1 do
      begin
        if id = DBLZERO then exit(GetFileID);
        id := GetIDFromDir(id,pv.Filename[i],false);
      end;
    GetFileID := id;
  end;


Function PutFileID(var name : PathName; id : SegID) : boolean;
{-----------------------------------------------------------------------------
  Abstract: enters name with SegID id into a directory
  Parameters: name - the full name (including all directories and optional
               device and partition) of the file to enter; it is changed to
               remove all ">..>" and ">.>"s and remove the device (the name
               returned can be entered in the FileID block's FSData.Filename).
              id - SegID of file; 
  Returns: True if file successfully entered; false if device, partition or
            a sub-directory is mal-formed
  NOTE: ***IT IS ILLEGAL TO CALL PutFileID FOR A NAME THAT IS ALREADY IN THE***
        ***DIRECTORY BUT THIS IS ONLY SOMETIMES CAUGHT IF ATTEMPTED***
  Calls: ParseFilename, GetRootDirID, GetIDFromDir, PutIDInDir
-----------------------------------------------------------------------------}
  var
    pv    : ParseVector;
    i     : integer;
    dirid : SegID;
    partition  : integer;
    ok    : boolean;
    tempName: PathName;
    
  begin
    PutFileID := false;
    ok := ParseFilename(name,pv,partition);
    if not ok then exit(PutFileID);
      
    dirid := GetRootDirID(partition);
    tempName := '';
    for i := 0 to pv.NumFiles-2 do
      begin
        if dirid = DBLZERO then exit(PutFileID);
        tempName := Concat(tempName,       { remove .DR from directory name }
               Substr(pv.Filename[i], 1, length(pv.Filename[i])-3));
        AppendChar(tempName, '>');
        dirid := GetIDFromDir(dirid,pv.Filename[i],false);
      end;
    if dirid = DBLZERO then exit(PutFileID);
    PutIDInDir(dirid,pv.Filename[pv.NumFiles-1],id);
    tempName := Concat(tempName, pv.Filename[pv.NumFiles-1]);
    PutFileID := true;
    name := tempName;
  end;


Function DeleteFileID(name : PathName) : SegID;
{-----------------------------------------------------------------------------
  Abstract: Removes the directory entry for name
  Parameters: name - the full name (including all directories and optional
               device and partition) of the file to remove from directory 
  Returns: SegID of file removed from Directory or DBLZERO if not there or
            part of name is mal-formed
  Calls: ParseFilename, GetRootDirID, GetIDFromDir
-----------------------------------------------------------------------------}
  var
    pv    : ParseVector;
    i     : integer;
    dirid : SegID;
    oldid : SegID;
    partition  : integer;
    ok    : boolean;

  begin
    ok := ParseFilename(name,pv,partition);
    if not ok then
      begin
        DeleteFileID := DBLZERO;
        exit(DeleteFileID);
      end;
      
    dirid := GetRootDirID(partition);
    for i := 0 to pv.NumFiles-1 do
      begin
        if dirid = DBLZERO then
          begin
            DeleteFileID := DBLZERO;
            exit(DeleteFileID);
          end;
        oldid := dirid;
        dirid := GetIDFromDir(dirid,pv.Filename[i],false);
      end;
    if dirid <> DBLZERO then
      DeleteFileID := GetIDFromDir(oldid,pv.Filename[pv.NumFiles-1],true)
    else DeleteFileID := DBLZERO;
  end

(* DEBUG
;

var s: String;
    pv: ParseVector;
    ok: Boolean;
    i: Integer;
    
begin
DefaultDeviceName := 'DEV:';
DefaultPartitionName := 'DEV:PART>';

repeat
Write('filename: ');
readln(s);
ok := ParseFileName(s,pv,i);
if ok then for i := 0 to pv.NumFiles-1 do
   WriteLn('pv[',i:1,'] = ',pv.FileName[i])
else writeLn('NOT OK');
until false;

end
(* DEBUG *)
.
