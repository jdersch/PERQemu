{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module FileSystem;
{--------------------------------------------------------------------
{
{ Abstract:
{
{
{ Spice Interim File System.

 Written by: Richard F. Rashid
 Date      : February 24, 1981
 Copyright (C) 1981 - Carnegie-Mellon University
{
{   Copyright 1981, 1982, 1983  Three Rivers Computer Corporation
{--------------------------------------------------------------------}

{--------------------------------------------------------------------
 Change history

 31 Mar 83  SSJ   V7.4    Set and test 5 bits of a disk address to determine
                          if it is a floppy. The result is that 1MB is kept for
                          floppy and 31mb are availble for the disk.
  4 Jan 82  BAM   V7.3    New exception for closing a directory.
  3 Dec 81  BAM   V7.2    New version of FlushFail.
 12 May 81  BAM   V7.1    Fixed to allow FlushFail on open.
 12 May 81  BAM   V7.0    Added Exceptions for FSLookUp and FSEnter.
                          Catch string too long from FixFileName
  9 Apr 81  BAM   V6.7    FSClose no longer sets any dates (set by open/enter).
  3 Apr 81  BAM   V6.6    Fixed bug in entering filename with .. in it.
 27 Mar 81  BAM   V6.5    Fixed Close to do Trucate.
 26 Mar 81  BAM   V6.4    Added comments.
 19 Mar 81  BAM   V6.3    import PERQ_String not PERQ.String.
 17 Mar 81  BAM   V6.2    Added new procedure FSIsFSDev.
 10 Mar 81  BAM   V6.1    Changed FixFileName to create a full file spec;
                           removed self-init from FixFileName
  9 Mar 81  BAM   V6.0    changed syntax of directories
  5 Mar 81  BAM   V5.0    Removed many procedures into FileUtils; removed
                           Import of DiskIO from export; changed size of
                           FileName strings; improved Scan;

  3 Mar 81  BR and JPS    Improve setting of dates in FSEnter/FSClose/FSLookUp.
                  V4.2    Set FileType in FSEnter.
                          Make Id a Var parameter ro FSScan as it should be.

  1 Mar 81  BR    V4.1    Set Dates in FSEnter/FSClose.

 26 Feb 81  DAS and JPS   Added FSRename.  Exported FixFileName.
                  V4.0
--------------------------------------------------------------------}

{********************} Exports {********************}

imports FileDefs from FileDefs;

const
  FSVersion     = '7.4';    { File system version number }
  BlksPerFile   =#077777;   { Max blocks in each file }
  FirstBlk      =0;         { Block number of the first data block }
                            { in a file }
  LastBlk       =#077776;   { Block number of the last data block }
                            { in a file. }
  FIBlk         =-1;        { Block number of the File Information Block }
  BootLength    = 60 + 128; { Size of the bootstrap area on disk--the }
                            { first n blocks on the disk.  the microcode }
                            { boot area is 60 blocks, the Pascal boot  }
                            { area is 128 blocks (32K). }
  StartBlk      =BootLength;{ The block number of the FIBlk of the first}
                            { user file. }
  SysFile       = -1;       { File ID of the system area on disk. }
  SEARCHSIZELIST = 5;       { Max number of directories on search list. }
                                 
type
  DirBlk= Record                      { Record for reading disk blocks }
           Case Integer Of 
           2: (
                Buffer:Array[0..255] Of Integer
              );
           3: (
                ByteBuffer: Packed Array [0..511] of FSBit8
              )
           End;

  PDirBlk= ^DirBlk;
       
  FileID        = integer;
  BlkNumbers    = integer;
      
  SearchList    = array[1..SEARCHSIZELIST] of PathName;
  ptrSearchList = ^SearchList;
                      
var 
  FSDirPrefix:PathName;  {current default directory including device and part}
  FSSysSearchList: SearchList;

function FSLookUp(FileName:PathName;Var BlkInFile,BitsInLBlk: Integer): FileID;
                       {uses current system search list}

function FSLocalLookUp(FileName:PathName; Var BlkInFile,BitsInLBlk: Integer):
            FileID;    {doesn't use any search lists}

function  FSSearch(var slist : SearchList; var FileName : PathName;
                   var BlkInFile, BitsInLBlk: integer) : FileID;
                       {uses specified search list instead of system one; is
                        var so no copying; changes FileName to be full filename
                        actually used}

function  FSEnter(FileName:PathName): FileID;
procedure FSClose(UserFile:FileID; Blks,Bits:Integer);
procedure FSBlkRead(UserFile:FileID; Block:BlkNumbers; Buff:PDirBlk);
procedure FSBlkWrite(UserFile:FileID; Block:BlkNumbers; Buff:PDirBlk);
procedure FSInit;
procedure FSMount(disk : integer);
procedure FSDismount(disk : integer);

procedure FSSetPrefix(prefixname : PathName);  {FSSetPrefix just assigns the
                                                vble; use FileUtils.FSSetPath
                                                to do processing on new path}

procedure FSGetPrefix(var prefixname : PathName);
function  FileIDtoSegID(id : FileID) : SegID;
function  SegIDtoFileID(id : SegID) : FileID;
procedure FSSetupSystem(bootchar: integer);
procedure FixFilename(var filename : PathName; nulliserror : boolean);
Function FSIsFSDev(name: PathName; var devName: String): integer;

Exception FSNotFnd(name: PathName);
{--------------------------------------------------------------------
  Abstract: Raised if file looked up is not found.  If this exception is not
            handled by client, the lookup or search will return zero
  Parameters: name is the name not found
--------------------------------------------------------------------}

Exception FSBadName(name: PathName);
{--------------------------------------------------------------------
  Abstract: Raised if file entered is illegal because:
              1) the device or partition specified is not valid
              2) a directory name specified does not exist
              3) the length of the simpleName is > 25 characters
            If this exception is not handled by the client, the Enter will
            return zero 
  Parameters: name is the name that is illegal
--------------------------------------------------------------------}

Function FSInternalLookUp(FileName:PathName; Var BlkInFile,BitsInLBlk:Integer):
                FileID;

Exception FSDirClose;
{--------------------------------------------------------------------
  Abstract: Raised if attempt to FSClose a directory file.  This is usually
            a bad idea since directories are spare files with an invalid
            length field. 
  RESUME: Allowed.  Will close the file as if nothing had happened.
--------------------------------------------------------------------}

const
  FSDebug = false;

{********************} Private {********************}

imports DiskIO from DiskIO;
imports FileAccess from FileAccess;
imports FileDir from FileDir;
imports AllocDisk from AllocDisk;
imports Perq_String from Perq_String;
imports FileTypes from FileTypes;
imports Except from Except;  {using STRLong}

var
  BufPtr     : ptrDiskBuffer;
  Initialized: integer;
     

Function SegIDtoFileID(id : SegID) : FileID;
{--------------------------------------------------------------------
  Abstract: Convert a two word SegId into a one word fileID
  Parameters: id is a two word segID
  Returns: A one word FileID; it may be pos or neg or zero
--------------------------------------------------------------------}
  var
    fid : FileID;
    disk : integer;
    
  begin
    fid := AddrToField(id);
    disk := WhichDisk(id);
    case disk of
      0: SegIDtoFileID := fid;
      1: SegIDtoFileID := lor(fid,#174000);
      otherwise: SegIDtoFileID := 0
    end;
  end;
  

Function FileIDtoSegID(id : FileID) : SegID;
{--------------------------------------------------------------------
  Abstract: Convert a one word FileID into a two word SegID
  Parameters: id is a one word FileID
  Returns: a two word SegID
--------------------------------------------------------------------}
  var
    disk : integer;
    
  begin
    if land(id,#174000) = #174000 then disk := 1 else disk := 0;
    case disk of
      0: FileIDtoSegID := FieldToAddr(disk,id);
      1: FileIDtoSegID := FieldToAddr(disk,land(id,#003777));
      otherwise: FileIDtoSegID := DBLZERO
    end;
  end;
  

Procedure FSInit;
{--------------------------------------------------------------------
  Abstract: Initializes the FileSystem; call BEFORE FSSetUpSystem;
            Also initialize SegSystem and DirSystem
  SideEffects: Initializes; sets global Initialized to true; sets Prefix and
               Search list to null
--------------------------------------------------------------------}
  var i : integer;
  begin
    InitDiskIO;
    InitBuffers;
    InitAlloc;
    new(DiskSegment,256,BufPtr);
    FSDirPrefix:='';
    Initialized := #12345;
    for i := 1 to SEARCHSIZELIST do
       FSSysSearchList[i] := '';
    end {FSInit};

Procedure FixFilename(var filename : PathName; nulliserror : boolean);
{--------------------------------------------------------------------
  Abstract: Makes fileName a full path name by adding as many defaults as
            necessary
  Parameters: filename is name to fix; it is modified to have the full path
               name as follows:
                  (dev):(rest) - no change
                  :(rest)      - adds DefaultDevice from AllocDisk to front
                  >(rest)      - adds DefaultPartition from AllocDisk to front
                  (rest)       - adds FSDirPrefix to front;
              if nullIsError-then no change to name if fileName = '' else
               changes '' to FSDirPrefix
  Errors: allows STRLong to pass through from PERQ_String;
           This means that fileName is invalid
--------------------------------------------------------------------}
  var
    indx : integer;
    prefix : PathName;
    
  begin
    if length(fileName) <> 0 then
      begin
       indx := PosC(filename, ':');
       if indx > 1 then {a device was specified so no change to filename}
       else if indx = 1 then fileName := Concat(DefaultDeviceName,
                                      Substr(filename, 2,Length(fileName)-1))
            else {no ":"}
             if fileName[1] = '>' then 
                fileName := Concat(DefaultPartitionName,
                                     SubStr(fileName,2,length(filename)-1))
             else filename := concat(FSDirPrefix,filename);
      end
    else 
      if not nulliserror then filename := FSDirPrefix;
  end;


Procedure FSMount(disk: integer);
{--------------------------------------------------------------------
  Abstract: Mounts the disk specified and prints all partitions
  Parameters: Disk is device to mount (0=HardDisk, 1=Floppy)
  Calls: DeviceMount and DisplayPartitions
--------------------------------------------------------------------}
  begin
    DeviceMount(disk);
    DisplayPartitions;
  end {FSMount};


Procedure FSDismount(disk: integer);
{--------------------------------------------------------------------
  Abstract: Dismounts the disk specified and prints all partitions
  Parameters: Disk is device to dismount (0=HardDisk, 1=Floppy)
  Calls: DeviceDismount and DisplayPartitions
--------------------------------------------------------------------}
  begin
    DeviceDismount(disk);
    DisplayPartitions;
  end {FSDismount};


Procedure FSSetPrefix(prefixname : PathName);
{--------------------------------------------------------------------
  Abstract: Sets the default pathName
  Parameters: prefixname is new name;  no checking is done
  SideEffects: changes FSDirPrefix
--------------------------------------------------------------------}
  begin
    FSDirPrefix := prefixname;
  end {FSSetPrefix};


Procedure FSGetPrefix(var prefixname : PathName);
{--------------------------------------------------------------------
  Abstract: Returns the default pathName
  Parameters: prefixname is current name; it is set with current value
--------------------------------------------------------------------}
  begin
    prefixname := FSDirPrefix;
  end {FSGetPrefix};


Function FSInternalLookUp(FileName:PathName;Var BlkInFile,BitsInLBlk:Integer):
                FileID;
{--------------------------------------------------------------------
  Abstract: Does a lookup of FileName in the current path only.
  Parameters: FileName is a filename.  BlkInFile and BitsInLBlk are set with
               the number of blocks in the file and the number of bits in the
               last block respectively.
  Returns: 0 if file doesn't exist; else the FileID of the file
  Errors: This procedure does not raise any errors!!
  SideEffects: Sets the FileAccessDate of the file
  Calls: FixFileName, GetFileID, GetTStamp, SegIDToFileID
--------------------------------------------------------------------}
  var
    id : SegID;
    time: TimeStamp;
    ptr: ptrDiskBuffer;
    hdr: ptrHeader;
    fid: integer;
    
  Handler STRLong;
   {--------------------------------------------------------------------
     Abstract: String too long means file name invalid so just return zero
                from procedure
   --------------------------------------------------------------------}
    begin
    Exit(FSInternalLookUp);
    end;

  Handler FlushFail(msg: String; operation: DiskCommand; addr: DiskAddr;
                       softStat: integer);
   {--------------------------------------------------------------------
     Abstract: Handles flushFail since OK to fail here (in case write protected
               floppy).
   --------------------------------------------------------------------}
    begin
    Exit(FSInternalLookUp);
    end;
      
  begin
    FSInternalLookUp := 0;
    FixFilename(filename,true);
    if length(filename) = 0 then exit(FSInternalLookUp);
    id := GetFileID(filename);
    if id = DBLZERO then exit(FSInternalLookUp);
    ptr := ChangeDisk(id);
    blkinfile := ptr^.FSData.FileBlocks;
    bitsinlblk := ptr^.FSData.FileBits;
    GetTStamp(Time);
    ptr^.FSData.FileAccessDate := Time;
    FSInternalLookUp := SegIDtoFileID(id);
    FlushDisk(id);
  end {FSInternalLookUp};


Function FSLocalLookUp(FileName:PathName;Var BlkInFile,BitsInLBlk:Integer):
                FileID;
{--------------------------------------------------------------------
  Abstract: Does a lookup of FileName in the current path only.
  Parameters: FileName is a filename.  BlkInFile and BitsInLBlk are set with
               the number of blocks in the file and the number of bits in the
               last block respectively.
  Returns: 0 if file doesn't exist; else the FileID of the file
  SideEffects: Sets the FileAccessDate of the file
  Errors: Raises FSNotFnd if file not there (if not caught, then lookup
            returns 0)
  Calls: InternalLookUp
--------------------------------------------------------------------}
  var f: FileID;
  begin
  f := FSInternalLookUp(FileName, blkInFile, BitsInLBlk);
  FSLocalLookUp := f;
  if f = 0 then Raise FSNotFnd(FileName);
  end;


Function FSSearch(var slist : SearchList; var filename : PathName;
                   var blkinfile,bitsinlblk: integer) : FileID;
{--------------------------------------------------------------------
  Abstract: Does a lookup of FileName straight first and then with each of
             the names in slist on the front.
  Parameters: slist - a searchList; any non-'' entries are assumed to be
               paths and are put on the front of the filename.  The first
               one to be tried is slist[1].  The first match is the one used;
               No checking is done on the validity of the entries in slist

              filename - the file to be looked up;  it is changed to be the
               full name of the file if found.  If fileName is empty then
               file not found.

              BlkInFile and BitsInLBlk - set with the number of blocks in
               the file and the number of bits in the last block respectively.
  Returns: 0 if file doesn't exist in any path; else the FileID of the file
  SideEffects: Sets the FileAccessDate of the file
  Errors: Raises FSNotFnd if file not there (if not caught, then lookup
            returns 0)
  Calls: FSLocalLookUp, Concat
--------------------------------------------------------------------}
  var
    i   : integer;
    fid : FileID;
  
  begin
  
  if fileName <> '' then 
    begin
    fid := FSInternalLookUp(filename,blkinfile,bitsinlblk);
    if fid <> 0 then
      begin
        FSSearch := fid;
        FixFileName(fileName, true);  {so full name}
        exit(FSSearch);
      end;
      
    for i := 1 to SEARCHSIZELIST do
       begin
       if length(slist[i]) <> 0 then
         begin
         fid:=FSInternalLookUp(Concat(slist[i],FileName),blkinfile,bitsinlblk);
         if fid <> 0 then 
              begin
                FSSearch := fid;
                FileName := Concat(slist[i],FileName);
                exit(FSSearch);
              end;
         end;
       end;
    end;  
  FSSearch := 0;
  Raise FSNotFnd(FileName);
  end;
  

Function FSLookUp(FileName: PathName; Var BlkInFile, BitsInLBlk: Integer)
          : FileID;
{--------------------------------------------------------------------
  Abstract: Does a lookup of fileName first in the current path and then
             in each of the entries of the system search list.
  Parameters: filename is the file to be looked up;
              BlkInFile and BitsInLBlk are set with the number of blocks in
               the file and the number of bits in the last block respectively.
  Returns: 0 if file doesn't exist in any path; else the FileID of the file
  SideEffects: Sets the FileAccessDate of the file
  Errors: Raises FSNotFnd if file not there (if not caught, then lookup
            returns 0)
  Calls: FSSearch with FSSysSearchList as the sList
--------------------------------------------------------------------}
   begin
   FSLookUp := FSSearch(FSSysSearchList, fileName, BlkinFile,BitsInLBlk);
   end; {FSLookUp}


Function FSEnter(FileName:PathName): FileID;
{--------------------------------------------------------------------
  Abstract: Enters the file in the current path.
  Parameters: filename is the file to be entered.  It may or may not exist;
              if not exists then is created;
  Returns: 0 if file can't be created because part of its name is invalid (e.g.
           the device, partition or directory specified doesn't exist)
           else the FileID of the file
  SideEffects: Creates a file if necessary and enters it into the directory;
               if creating, then sets size to zero and create date;  Sets type
               to 0 (UnknownFile);
               whether or not creating; sets WriteDate and AccessDate
  Errors: Raises FSBadName if name passed is illegal due to a device, 
           partition, or directory name in path not existing or target name
           is longer than 25 characters or illegal in some other way.  If
           this exception is not caught, Enter returns zero.
--------------------------------------------------------------------}
  var
    id       : SegID;
    str      : PathName;
    partition: integer;
    ok       : boolean;
    time     : TimeStamp;
    creating : boolean;
  
  Handler STRLong;
   {--------------------------------------------------------------------
     Abstract: If string too long then bad name; raise BadName and if that
                returns, simply return 0 from FSEnter.
   --------------------------------------------------------------------}
    begin
    Raise FSBadName(fileName);
    exit(FSEnter);
    end;
  
  begin
    FSEnter := 0;
    
    FixFilename(filename,true);
    if length(filename) = 0 then 
       begin
       Raise FSBadName(fileName);
       exit(FSEnter);
       end;

    id := GetFileID(filename);
    creating := id = DBLZERO;
    if creating then {id=DBLZERO}
      begin
        str := filename;
        ok := GetDisk(str,partition); {modifies Str to remove dev and part}
        if ok then
          begin
          id := CreateSpiceSegment(partition,permanent);
          if id <> DBLZERO then ok := PutFileID(fileName,id) {removes dev ..s}
          else ok := false;
          end;
        if not ok then
          begin
            if id <> DBLZERO then DestroySpiceSegment(id);
            Raise FSBadName(fileName);
            exit(FSEnter);
          end
      end;
    ReadSpiceSegment(id,FIBlk,1,BufPtr);
    with BufPtr^.FSData do
     begin
      GetTStamp(time);
      if creating then
       begin
        FileBlocks := 0;
        FileBits   := 0;
        FileType   := 0;
        FileType   := UnknownFile;
        FileCreateDate := time;
       end;
      FileWriteDate := time;
      FileAccessDate := time;
     end;
    if creating then BufPtr^.FSData.FileName := fileName;
    WriteSpiceSegment(id,FIBlk,1,BufPtr);
     
    FSEnter := SegIDtoFileID(id);
  end {FSEnter};  
    

Procedure FSClose(UserFile:FileID; Blks,Bits:Integer);
{--------------------------------------------------------------------
  Abstract: Closes a file (setting size).
  Parameters: UserFile is ID of file to close; Blks is the size of the file in
               blks and bits is the number of bits in the last block;
  SideEffects: Truncates file to size specified; does a FlushAll
  Errors: Raises FSDirClose if attempt to close a directory file.  If
          resume from this exception, then closes normally.
--------------------------------------------------------------------}
  var
    sid : SegID;
    
  begin
    sid := FileIDtoSegID(UserFile);
    ReadSpiceSegment(sid,FIBlk,1,BufPtr);
    with BufPtr^.FSData do
     begin
      if FileType = DirFile then Raise FSDirClose;
      FileBlocks:=Blks;
      FileBits:=Bits;
     end;
    WriteSpiceSegment(sid,FIBlk,1,BufPtr);
    TruncateSpiceSegment(sid, Blks);
    FlushAll;
  end {FSClose};


Procedure FSBlkWrite(UserFile:FileID; Block:BlkNumbers; Buff:PDirBlk);
{--------------------------------------------------------------------
  Abstract: Writes one block onto a file.
  Parameters: UserFile is ID of file to write on
              Block is number of block to write (starting at zero);
              Buff is buffer holding data to write onto the file at Block
  SideEffects: Changes the data of block Block
  Calls: WriteSpiceSegment
--------------------------------------------------------------------}
  var
    sid : SegID;
    
  begin
    if FSDebug then 
      begin
        WriteLn('FSBlkWrite: ',UserFile,' ',Block);
      end;
    sid := FileIDtoSegID(UserFile);
    WriteSpiceSegment(sid,Block,1,Recast(Buff,ptrDiskBuffer));
  end {FSBlkWrite};


Procedure FSBlkRead(UserFile:FileID; Block:BlkNumbers; Buff:PDirBlk);
{--------------------------------------------------------------------
  Abstract: Reads one block of a file.  If block specified is not part of the
             file then simply zeros the buffer
  Parameters: UserFile is ID of file to read from
              Block is number of block to read (starting at zero);
              Buff is buffer to copy data into
  Calls: ReadSpiceSegment
--------------------------------------------------------------------}
  var
    sid : SegID;
    
  begin
    if FSDebug then 
      begin
        WriteLn('FSBlkRead: ',UserFile,' ',Block);
      end;
    sid := FileIDtoSegID(UserFile);
    ReadSpiceSegment(sid,Block,1,Recast(Buff,ptrDiskBuffer));
  end {FSBlkRead};


Procedure FSSetupSystem(bootchar: integer);
{--------------------------------------------------------------------
  Abstract: Call this after FSInit to set up the system and print a lot of
             messages
  Parameters: boot char is ord of key held down to boot
  SideEffects: Mounts device from which booted; Mounts all of its partitions
               Sets AllocDisk's DefaultDeviceName and DefaultPartitionName.
               Sets FSDirPrefix to be root of current Partition and adds that
               path to the bottom of the search list
--------------------------------------------------------------------}
  var
    defdisk : integer;
    paddr,laddr : DiskAddr;
    part        : integer;
    ptr         : ptrDiskBuffer;
    
  begin
    if bootchar > ord('Z') then
      begin
        bootchar := bootchar - ord('a');
        defdisk := 0;
      end
    else 
      begin
        bootchar := bootchar - ord('A');
        defdisk := 1;
      end;
    
    
    WriteLn('Mounting disk #',defdisk,'...');
    FSMount(defdisk);
    WriteLn('...done.');
    
    ptr := ReadDisk(DiskTable[defdisk].InfoBlk);
    paddr := ptr^.BootTable[bootchar];
    laddr := PhysAddrToLogAddr(defdisk,paddr);
    part := WhichPartition(laddr);
    if part = 0 then part := 1;
    
    DefaultDeviceName := Concat(DiskTable[defdisk].RootPartition,':');
    DefaultPartitionName := Concat(
                            Concat(DefaultDeviceName,PartTable[part].PartName),
                            '>');
    FSSetPrefix(DefaultPartitionName);
    FSSysSearchList[SEARCHSIZELIST] := DefaultPartitionName;
    WriteLn('FileName prefix set to: ',DefaultPartitionName);
    WriteLn;
  end;


Function FSIsFSDev(name: PathName; var devName: String): integer;
 {------------------------------------------------------------------------
   Abstract: determine whether name is a file that the filesystem knows how
              to handle
   Parameters: name is a name of a file; devName will be assigned the device
                name IF NOT FSDevice  
                DevName will be in upper case and does NOT contain the colon. 
   Returns: 0 if name doesn't contain a : or if dev is one the filesystem
             knows about; the index of the colon otherwise
 -------------------------------------------------------------------------}
   var i: integer;
       name2: PathName;
   begin
     devName := '';
     i := PosC(name, ':');
     FSIsFSDev := i;
     if i > 1 then {have device name}
       begin
         devName := SubStr(name, 1, i-1);
         ConvUpper(devName);
         for i := 0 to MAXDISKS-1 do
            if DiskTable[i].InUse then
               begin
                 name2 := DiskTable[i].RootPartition;
                 ConvUpper(name2);
                 if name2=devName then 
                   begin
                     FSIsFSDev := 0;  {is FS}
                     exit(FSIsFSDev);
                   end;
               end; {if get here then not FS}
       end {i>1}
     else FSIsFSDev := 0;  {isFS}
   end {FSIsFSDev}.
