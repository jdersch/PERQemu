module AllocDisk;
{--------------------------------------------------------------------------
{ AllocDisk: Module to handle allocating and freeing of pages off disk
{ Written by CMU-people
{ Copyright (C) 1980, 1981, 1982, 1983 Three Rivers Computer Corporation
{ Copyright (C) 1983, 1984 - Perq Systems Corporation.
{
{
{Abstract:
{    Allocdisk allocates and deallocates disk pages.
{
{ The partition has some number of contiguous pages on it. 
{ The number of pages in a partition is specified
{ when the partition is created (using the Partition program).  
{ Segments can be created within a partition, e.g. segments may not
{ span partitions.

{ The entire disk can be thought of as a partition (the Root Partition)
{
{ A DiskInformationBlock (DiskInfoBlock or DIB) contains all the fixed
{ information about a disk, including its partion names, locations and sizes.
{ It also contains a table used to locate boot segments
{ 
{ A disk can be 'mounted' which means that its root partition is known
{ to the system as an entry in the DiskTable.
{ 
{ A Partition Information Block (PartInfoBlock or PIB) contains all of the
{ fixed information about a partition,
{ 
{ A partition can also be 'mounted', and this is usually done as part of
{ mounting the disk itself. Partitions mounted are entries in the PartTable
{   
{ Within a partition, segments are allocated as doubly linked lists of pages
{ 
{ The Free List of a segment is a doubly linked list of free pages
{
{ This module maintains this list, as well as the DeviceTable and PartTable
{ It contains procedures for mounting and dismounting disks and partitions, 
{ as well as allocating and deallocating space within a partition.

{ When allocating pages, the module updates the PartInfoBlock every
{ MaxAllocs calls on AllocDisk
{
{ Since the system may crash some time between updates, the pointers
{ and free list size may not be accurate.
{
{ When a partition is mounted, the pointers are checked to see if they
{ point to free pages.  If not, the head of the pointer is found by
{ looking at the "filler" word of the block the free head does point to
{ (which presumably was allocated after the last update of PartInfoBlock).
{ The filler word has a short pointer to the next "free" page, and forms
{ a linked list to the real free list header.  Likewise, if the Free tail
{ does not have a next pointer of 0, a deallocate is presumed to have been done
{ since a PartInfoBlock update, and NextAdr pointers are chased to find the
{ real end of the Free List.
{
{--------------------------------------------------------------------------}

{--------------------------------------------------------------------------
 Change Log:

{
{ 11 Jul 84  V2.12  Sandeep Johar
  Only read PIB if less than 64k even if DIB points to a greater disk
  address. Really do it right this time.
  
{  9-Jun-84  V2.11  Sandeep Johar
{ Do it right this time.
{
{  8-Jun-84  V2.10  Sandeep Johar
{ Changes to handle large disks.
{
    V2.9  16-Nov-82  Bill Braucher Fixed names for 14-char compiler.
    V2.8   3-Dec-81  Brad Myers   Changed definition of FlushFail
    V2.7  15-Jul-81  John Strait  Fixed bug in DeallocChain: increment
                                  PartNumOps by the length of the deallocated
                                  chain.  Change order of operations in
                                  AllocDisk routine to reduce probability
                                  of creating an inconsistent free list.
    V2.6  23-Jun-81  Brad Myers  Changed UpdatePartInfo to only write PartInfo
                                  if changed so can boot from write-protected
                                  floppies.
    V2.5   1-Jun-81  Brad Myers  Made UpdatePartInfo more fail-safe.
    V2.4  12-May-81  Brad Myers  New exceptions; no Write statements.
                                 Changed addresses to print in unsigned decimal
    V2.3  20-Apr-81  John Strait Use DiskSegment for allocation.
    V2.2  14-Apr-81  Brad Myers  Changed to report "device name" instead of
                                  "root partition" in display partitions
                                 ForgetAll at end DeviceDismount
    V2.1  27-Mar-81  Brad Myers  Fixed bug in FindPartition for missing device
                                  and added comments
    V2.0  19-Mar-81  Brad Myers  Combined previous changes
    V1.3   ?-Mar-81   ??         Fixed GetPartName
    V1.2  10-Mar-81  Brad Myers  Bit32 => FSBit32; fixed for new FS syntax
    V1.0  ??-???-??  CMU people  written
--------------------------------------------------------------------------}

{ 
{******************} exports {***************************}

imports Arith from Arith;
imports ReadDisk from ReadDisk;

const
  MAXDISKS      =  2;  {Floppy and HardDisk}
  MAXPARTITIONS = 10;  {Maximum number of mounted partitions}
  MAXPARTCHARS  =  8;  {how many characters in a partition name}

type

  PartString   = string[MAXPARTCHARS];

  DeviceRecord = record                 {entry in the DeviceTable}
                    InfoBlk: DiskAddr;  {where the DiskInfoBlock is}
                    InUse  : boolean;   {this DeviceTable entry is valid}
                    RootPartition: PartString {name of this disk}
                 end;
  
  PartRecord   = record                      {entry in the PartTable}
                    PartHeadFree : DiskAddr; {pointer to Head of Free List}
                    PartTailFree : DiskAddr; {pointer to tail of Free List}
                    PartInfoBlk  : DiskAddr; {pointer to PartInfoBlock}
                    PartRootDir  : DiskAddr; {pointer to Root Directory}
                    PartNumOps   : integer;  {how many operations done since
                                              last update of PartInfoBlock}
                    PartNumFree  : FSBit32;  {HINT of how many free pages}
                    PartInUse    : boolean;  {this entry in PartTable is valid}
                    PartMounted  : boolean;  {this partition is mounted}
                    PartDevice   : integer;  {which disk this partition is in}
                    PartStart    : DiskAddr; {Disk Address of 1st page}
                    PartEnd      : DiskAddr; {Disk Address of last page}
                    PartKind     : PartitionType; {Root or Leaf}
                    PartName     : PartString {name of this partition}
                  end;


var
  DiskTable   : array [0..MAXDISKS-1]  of DeviceRecord;
  PartTable   : array [1..MAXPARTITIONS] of PartRecord;
  

procedure InitAlloc;  {initialize the AllocDisk module, called during boot}

procedure DeviceMount(disk: integer);  {mount a disk}

procedure DeviceDismount(disk : integer);  {dismount a disk}

function  MountPartition(name : string) : integer;  {mount a partion,
                                                     return PartTable index}

procedure DismountPartition(name : string);  {dismount a partition}

function  FindPartition(name : string) : integer;  {given a partion name, look
                                             for it in PartTable, return index}

function  AllocDisk(partition : integer) : DiskAddr;  {allocate a free page
                                                       from a partition}

procedure DeallocDisk(addr : DiskAddr);  {return a page to the free list}

procedure DeallocChain(firstaddr,lastaddr : DiskAddr; numblks : integer);
                                {return a bunch of pages to free list}
                                
function  WhichPartition(addr : DiskAddr) : integer;  {given a Disk Address,
                                        figure out which partition it is in}

procedure DisplayPartitions;  {print the PartTable}

Exception NoFreePartitions;
{-----------------------------------------------------
 Abstract:  Raised when too many partitions are accessed at one time.  The
             limit is MAXPARTITIONS. 
-------------------------------------------------------}

Exception BadPart(msg, partName: String);
{-----------------------------------------------------
 Abstract:  Raised when there is something wrong with a partition.  This
            means that the Scavenger should be run.
 Parameters: msg is the problem and partName is the partition name.  Print
             error message as: WriteLn('** ',msg,' for ',partName); 
-------------------------------------------------------}

Exception PartFull(partName: String);
{-----------------------------------------------------
 Abstract:  Raised when there are no free blocks in a partition to be
             allocated.  This means that some files should be deleted and then
             the Scavenger should be run.
 Parameters: partName is the full partition 
-------------------------------------------------------}

{******************} private {***************************}

imports PERQ_String from PERQ_String;

const 
  MAXTRIES      = 500;
  MAXALLOCS     = 50;   {how many operations to do on a partition before
                         updating the PartInfoBlock}
  DEBUG         = false;
  
var
  Initialized : integer;
  

Procedure InitAlloc;
{-----------------------------------------------------
{ Abstract
{   Initialize the AllocDisk module
{
{ Side Effects
{   Sets Initialized to a magic number; sets all InUse and PartInUse to false
{
{-------------------------------------------------------}
  var
    i : integer;
    
  begin
    if Initialized = #12345 then exit(InitAlloc);  {allow multiple calls}
    Initialized := #12345;
    
    for i := 0 to MAXDISKS-1    do DiskTable[i].InUse     := false;
    for i := 1 to MAXPARTITIONS do PartTable[i].PartInUse := false;
  end;
  

Function FreePartition : integer;
{-----------------------------------------------------
{ Abstract
{   Find a free entry in the PartTable
{
{ Returns
{   The index to a free entry in PartTable
{
{ Environment
{   PartTable must be initialized
{
{ Errors
{   Raise NoFreePartitions if no slots available in table
{-------------------------------------------------------}
  var
    i : integer;
    
  begin
    for i := 1 to MAXPARTITIONS do
      begin
        if not PartTable[i].PartInUse then 
          begin
            FreePartition := i;
            exit(FreePartition);
          end;
      end;
    Raise NoFreePartitions;
  end;


Procedure UpdatePartInfo(partition : integer);
{-----------------------------------------------------
{ Abstract
{   For an mounted partition, increment the PartNumOps count and
{   if there has been at least MaxAllocs operations, read the PartInfoBlock,
{   update the Free list head, tail and number, then write it back out
{       (causes writing out of partition info occasionally)
{
{ Parameters
{   partition - the index into the PartTable
{
{ Side Effects
{   Increments PartNumOps or sets to zero, updates PartInfoBlock if has changed
{
{ Calls
{   ChangeDisk, FlushDisk
{
{-------------------------------------------------------}
  var
    ptr : ptrDiskBuffer;
    
  Procedure DoFlush;
  {-----------------------------------------------------
  { Abstract
  {    Calls Flush so can handle FlushFail if is Floppy in which case
  {    flush fail is ignored
  {-------------------------------------------------------}
    Handler FlushFail(msg: String; operation: DiskCommand; addr: DiskAddr;
                      softStat: integer);
    {-----------------------------------------------------
    { Abstract
    {    If Partition being flushed is the harddisk then re-raise else
    {     if floppy then ignore it assuming it is because floppy is write
    {     protected
    {-------------------------------------------------------}
       begin
       if PartTable[partition].PartDevice = 0 then
             Raise FlushFail(msg, operation, addr, softStat)
       else Exit(UpdatePartInfo);
       end;
    begin
    FlushDisk(PartTable[partition].PartInfoBlk); {force the block out}
    end;

  begin
    with PartTable[partition] do
    begin
      if not (PartInUse and PartMounted) then Exit(UpdatePartInfo);

      PartNumOps := PartNumOps + 1;  {increment operation count}
      if PartNumOps > MAXALLOCS then {has it been a while since last update?} 
        begin
        ptr := ChangeDisk(PartInfoBlk); {Mark as changed}
        ptr^.FreeHead := PartHeadFree;  {update the free list stuff}
        ptr^.FreeTail := PartTailFree;
        ptr^.NumFree  := PartNumFree;
        PartNumOps := 0;                {start update count all over again}
        DoFlush;
        end;
    end;
  end;
  

Procedure GetPartName(ptr : ptrDiskBuffer; var name : PartString);
{-----------------------------------------------------
{ Abstract
{  given a pointer to a PartInfoBlock, copy the name of the partition into
{  a PartString
{
{ Parameters
{   ptr - a pointer to a disk buffer containing a PartInfoBlock
{   name - a Part String where the name of the partition is returned
{
{ Environment
{   ptr must have a PartInfoBlock read into it
{
{ Calls
{   adjust
{
{-------------------------------------------------------}
  var
    i : integer;
    ch: char;
  
  begin
    name := '        ';
    for i := 1 to MAXPARTCHARS do
      begin
        ch := ptr^.PartName[i];
        if ch = ' ' then
          begin
            Adjust(name, i-1);
            exit(GetPartName);
          end;
        name[i] := ch;
      end;
  end;
  

Procedure DeviceMount(disk : integer);
{-----------------------------------------------------
{ Abstract:
{    Mount the device specified by disk if not already mounted
{
{ Parameters:
{    Disk is a device; it should be zero for HardDisk and 1 for Floppy
{
{ Environment:
{    Expects DiskTable to be initialized
{
{ Side Effects:
{    Sets the DiskTable for device; loads PartTable with Part names on dev
{
{ Errors:
{    Error if no free partition slots in PartTable;

{ NOTE: No mention is made if device has partitions with names same as those
{       already loaded
{
{-------------------------------------------------------}
Label 1;
  var
    i,j       : integer;
    partition : integer;
    cheat     : DiskCheatType;
    ptr       : ptrDiskBuffer;
    infoptr   : ptrDiskBuffer;
  
  begin
    with DiskTable[disk] do
      begin
        if InUse then Exit(DeviceMount);
        InUse := true;
        cheat.Dbl[0] := 0;
        cheat.Dbl[1] := RECORDIOBITS + shift(disk,13);
        InfoBlk := cheat.Addr;
        ptr := ReadDisk(InfoBlk);
               
        GetPartName(ptr,RootPartition);
        i := 0;
        
        new(DiskSegment,1,infoptr);
        infoptr^ := ptr^;
        
        while infoptr^.SubParts[i] <> DBLZERO do
          begin
            cheat.addr := infoptr^.SubParts[i];
            If Land(Cheat.Dbl[1], #7400) <> 0 then goto 1;
            ptr := ReadDisk(infoptr^.SubParts[i]);
            partition := FreePartition;
            Cheat.Addr := ptr^.PartEnd;
            If LAnd(Cheat.Dbl[1], #7400) <> 0  Then
              GoTo 1;
            with PartTable[partition] do
              begin
                PartInUse    := true;
                PartDevice   := disk;
                PartInfoBlk  := infoptr^.SubParts[i];
                
                GetPartName(ptr,PartName);
                
                if DEBUG then WriteLn('DeviceMount: ',PartName);
                
                PartHeadFree := ptr^.FreeHead;
                PartTailFree := ptr^.FreeTail;
                PartStart    := ptr^.PartStart;
                PartEnd      := ptr^.PartEnd;
                PartKind     := ptr^.PartKind;
                PartNumFree  := ptr^.NumFree;
                PartRootDir  := ptr^.RootDirID;
                PartMounted  := false;
              end;
1:
            i := i + 1;
          end;
        dispose(infoptr);
      end;
  end;
  

Procedure DisplayPartitions;
{-----------------------------------------------------
{ Abstract:
{     Displays information about the current partitions on the screen
{
{ Environment:
{     Assumes PartTable and DiskTable set up;
{
{ Calls:
{     AddrToField; IntDouble, WriteLn;
{-------------------------------------------------------}
  var
    i,j : integer;
  
  begin
  
    for i := 0 to MAXDISKS-1 do
      begin
        with DiskTable[i] do
          begin
            if InUse then
              begin
                Write('Device # ',i:1);
                WriteLn('  Device name: ',RootPartition);
                for j := 1 to MAXPARTITIONS do
                  begin
                    with PartTable[j] do
                      begin
                        if PartInUse and (PartDevice = i) then
                          begin
                            WriteLn('             ',PartName:8,': Start = ',
                                    AddrToField(PartStart):10:-10,' End = ',
                                    AddrToField(PartEnd):10:-10,' Free = ',
                                    IntDouble(PartNumFree));
                          end;
                      end;
                  end;
              end;
          end;
      end;
  end;


Procedure DeviceDismount(disk : integer);
{-----------------------------------------------------
{ Abstract:
{    Removes device disk (0 or 1) from DiskTable and removes all its partitions
{
{ Parameters:
{    Disk is a device (0= HardDisk; 1=Floppy)
{
{ Side Effects:
{    Sets DiskTable[disk].InUse to false and removes all of disk's partitions
{
{ Calls:
{    DismountPartition
{-------------------------------------------------------}
  var
    i        : integer;
    rootname : string;
    newname  : string;
  
  begin
    rootname := DiskTable[disk].RootPartition;
    AppendChar(rootname,':');
    
    DiskTable[disk].InUse := false;  {set at top in case fail}
    for i := 1 to MAXPARTITIONS do
      begin
        with PartTable[i] do
          begin
            if (PartDevice = disk) and PartInUse then 
              begin
                PartInUse := false;
                newname := concat(rootname,PartName);
                DismountPartition(newname);
              end;
          end;
      end;
    DiskTable[disk].InUse := false;
    ForgetAll;
  end;
  

Function UpperEqual(str1,str2 : string) : boolean;
{-----------------------------------------------------
{ Abstract:
{    Converts str1 and str2 to uppercase and then compares them
{
{ Parameters:
{    Str1 and str2 are strings to be compared
{
{ Returns:
{    True if strings have equal upper case parts
{-------------------------------------------------------}
  begin
    ConvUpper(str1);
    ConvUpper(str2);
    UpperEqual :=  (str1 = str2);
  end;
  

Function FindPartition(name : string ) : integer;
{-----------------------------------------------------
{ Abstract:
{   Searches through partition table looking for a partition named name;
{    if found; returns its index in the table;
{
{ Parameters:
{     name is partition name of form "dev:part>" or ":part>" or "part>" where
{      the final ">" is optional in all forms.
{     If dev isn't specified then searches through all partition names.
{     If dev is specified; then only checks those partitions on that
{     device; name may be in any case
{
{ Returns:
{     index in PartTable of FIRST partition with name name (there may be more
{     than one partition with the same name in which case it uses the oldest
{     one) or zero if not found or name malformed;
{
{ Calls:
{     UpperEqual
{
{ Design:
{     No device name specified is signaled by disk=MAXDISKS; otherwise
{      disk is set to be the device which the device part of name specifies
{-------------------------------------------------------}
  var
    i,indx, len   : integer;
    disk          : integer;
    devicename    : string;
    
  label 1;
  
  begin
    FindPartition := 0;
    if length(name) = 0 then
        exit(FindPartition);
      
    disk := MAXDISKS;
    
    if name[length(name)] = '>' then Adjust(name, length(name)-1);
    
    indx := PosC(name, ':');
    if indx > 1 then {device specified}
      begin
        devicename := substr(name,1,indx-1);
        if DEBUG then WriteLn('FindPartition: devname = ',devicename);
        name := substr(name,indx+1,length(name)-indx);
        for i := 0 to MAXDISKS-1 do
          begin
            if UpperEqual(DiskTable[i].RootPartition,devicename) then
              begin
                disk := i;
                goto 1;
              end;
          end;
        exit(FindPartition);  {dev not found}
      end
    else if indx=1 then Delete(name, 1, 1);   {remove preceeding colon}
      
 1: if DEBUG then WriteLn('FindPartition: name = ',name,' disk = ',disk);
      
    for i := 1 to MAXPARTITIONS do
      begin
        if DEBUG then WriteLn('FindPart: PartName = ',PartTable[i].PartName);
        if PartTable[i].PartInUse and 
           UpperEqual(PartTable[i].PartName,name) and
           ((disk = PartTable[i].PartDevice) or (disk = MAXDISKS)) then
          begin
            FindPartition := i;
            exit(FindPartition);
          end;
      end;
      
    FindPartition := 0;  {part not found}
  end;
  

Function MountPartition(name : string) : integer;
{-----------------------------------------------------
{ Abstract:
{     Searches for partition name in part table and mounts it if not mounted
{     already; tries to read the head and tail of free list to see if valid

{
{ Parameters:
{     name is partition name of form "dev:part>" where "dev" and ">" are
{     optional
{
{ Returns:
{     index in part Table of partition for name or zero if not found
{
{ Side Effects:
{      if not mounted already, then reads in PartInfoBlk and sets partTable
{       fields;  tries to read the head and tail of free list to see if valid
{
{ Errors:
{     if no free slots for partition then Raises NoFreePartitions
{     if can't find free list head or tail then Raises BadPart
{
{ Calls:
{    FindPartition
{-------------------------------------------------------}
  var
    ptr : ptrDiskBuffer;
    hdr : ptrHeader;
    i   : integer;
    cheat : DiskCheatType;
    partition: integer;
    disk: integer;
    
  begin
    partition := FindPartition(name);
    MountPartition := partition;
    if partition = 0 then exit(MountPartition);
    
    
    with PartTable[partition] do
    begin
      disk := PartDevice;
      if PartMounted then exit(MountPartition);
      PartMounted := true;
    
      ptr := ReadDisk(PartInfoBlk);
      PartHeadFree := ptr^.FreeHead;
      PartNumFree  := ptr^.NumFree;
      PartTailFree := ptr^.FreeTail;
      PartRootDir  := ptr^.RootDirID;
      
      if DEBUG then
        begin
          WriteLn('PartHeadFree = ',AddrToField(PartHeadFree));
          WriteLn('PartTailFree = ',AddrToField(PartTailFree));
          WriteLn('PartRootDir  = ',AddrToField(PartRootDir));
        end;

      for i := 1 to MAXTRIES do
        begin
          hdr := ReadHeader(PartHeadFree);
          if (hdr^.SerialNum = DBLZERO) and (hdr^.PrevAdr = DBLZERO) then
            begin
              i := MAXTRIES + 1;
            end
          else
            begin
              PartNumFree := DoubleSub(PartNumFree,DoubleInt(1));
              PartHeadFree := FieldToAddr(disk,hdr^.Filler);
            end;
        end;
      
      if (hdr^.SerialNum <> DBLZERO) or (hdr^.PrevAdr <> DBLZERO) then
        Raise BadPart('Can''t find free list head',name);

      for i := 1 to MAXTRIES do
        begin
          hdr := ReadHeader(PartTailFree);
          if (hdr^.SerialNum = DBLZERO) and (hdr^.NextAdr = DBLZERO) then
            begin
              i := MAXTRIES + 1;
            end
          else
            begin
              PartNumFree := DoubleAdd(PartNumFree,DoubleInt(1));
              PartTailFree := hdr^.NextAdr;
            end;
        end;
      
      if (hdr^.SerialNum <> DBLZERO) or (hdr^.NextAdr <> DBLZERO) then
        Raise BadPart('Can''t find free list tail',name);

      PartInUse := true;
      PartNumOps := MAXALLOCS+1;
      UpdatePartInfo(partition);
    end;
  end;


Procedure DismountPartition(name : string);
{-----------------------------------------------------
{ Abstract:
{     Removes partition name from PartTable
{
{ Parameters:
{     name is partition name of form "dev:part>" where "dev" and ">" are
{     optional
{
{ Side Effects:
{     Writes out part information in table if partition InUse and mounted
{
{ Calls:
{     UpdatePartInfo, ForgetAll
{-------------------------------------------------------}
  var
    partition : integer;
    
  begin
    partition := FindPartition(name);
    if partition = 0 then exit(DismountPartition);
    
    with PartTable[partition] do
    begin
      if not (PartInUse and PartMounted) then exit(DismountPartition);
      PartMounted := false;
      PartNumOps := MAXALLOCS+1;
      UpdatePartInfo(partition);
      ForgetAll;
    end;
  end;
 

Function AllocDisk(partition: integer) : DiskAddr;
{-----------------------------------------------------
{ Abstract:
{     Allocate a free block from partition
{
{ Parameters:
{     Partition is the partition index to allocate the block from
{
{ Returns:
{     Disk Address of newly freed block;
{
{ Side Effects:
{     Updates the partition info to note block freed; changes header in buffer
{     of block; writes new head of free list with its next and prev fields 
{     set to zero and its filler set to next free block; decrements PartNumFree
{
{ Errors:
{     Raises PartFull if no free blocks in partition
{     Raises BadPart if free list inconsistent
{
{ Calls:
{     ReadHeader, ChangeHeader, FlushDisk, UpdatePartInfo 
{-------------------------------------------------------}
  var
    next : DiskAddr;
    freehdptr,nexthdptr : ptrHeader;

  begin
    with PartTable[partition] do
    begin
      freehdptr := ReadHeader(PartHeadFree);
      next := freehdptr^.NextAdr;
      if (next = DBLZERO) then 
           Raise PartFull(PartName);

      nexthdptr := ChangeHeader(next);
      if nexthdptr^.PrevAdr <> PartHeadFree then
           Raise BadPart('Inconsistent free list',PartName);
        
      nexthdptr^.SerialNum := DBLZERO;
      nexthdptr^.PrevAdr   := DBLZERO;
      nexthdptr^.Filler := AddrToField(nexthdptr^.NextAdr);

      AllocDisk := PartHeadFree;
      PartHeadFree := next;
      PartNumFree := DoubleSub(PartNumFree,DoubleInt(1));

      FlushDisk(next);

      UpdatePartInfo(partition);
    end;
  end;
  

Function WhichPartition(addr : DiskAddr) : integer;
{-----------------------------------------------------
{ Abstract:
{     Given a disk address; find the partition it is in
{
{ Parameters:
{     addr is a disk address
{
{ Returns:
{     index of partition addr falls inside of or zero if none
{
{ Calls:
{    DoubleBetween
{
{ NOTE: DOESN'T CHECK IF ENTRY IN TABLE IS MOUNTED OR INUSE!!! (***Bug***???)
{-------------------------------------------------------}
  var
    i    : integer;
    
  begin
    for i := 1 to MAXPARTITIONS do
      begin
        with PartTable[i] do
          begin
            if DoubleBetween(addr,PartStart,PartEnd) then
              begin
                WhichPartition := i;
                exit(WhichPartition);
              end;
          end;
      end;
    WhichPartition := 0;
  end;


Procedure AddToTail(partition: integer; addr : DiskAddr; isend : boolean);
{-----------------------------------------------------
{ Abstract:
{     internal procedure to add a block to the end of a free list
{
{ Parameters:
{     partition is index into part table of partition to deallocate into;
{     addr is block to deallocate; isEnd determines whether this is the
{     last block to be deallocated in this chain
{
{ Side Effects:
{     Changes PartTable[partition] by adding addr to tail of its free list
{     Flushes PartTailFree and addr after changing the headers
{
{ Calls:
{     ChangeHeader, FlushDisk, AddrToField
{-------------------------------------------------------}
  var
    hdptr : ptrHeader;
    disk  : integer;

  begin
    with PartTable[partition] do
    begin
      hdptr := ChangeHeader(addr);
      hdptr^.SerialNum := DBLZERO;
      hdptr^.PrevAdr := PartTailFree;
      if isend then 
        begin
          hdptr^.NextAdr := DBLZERO;
        end;
      hdptr^.Filler := AddrToField(PartHeadFree);
      hdptr := ChangeHeader(PartTailFree);
      hdptr^.NextAdr := addr;
      FlushDisk(PartTailFree);
      FlushDisk(addr);
    end;
  end;


Procedure DeallocDisk(addr : DiskAddr);
{-----------------------------------------------------
{ Abstract:
{     Returns block addr to whatever partition it belongs to
{
{ Parameters:
{     addr is block to deallocate
{
{ Side Effects:
{     adds addr to free list; increments PartNumFree
{
{ Calls:
{     AddToTail, WhichPartition, UpdatePartInfo
{-------------------------------------------------------}
  var
    hdptr    : ptrHeader;
    partition: integer;

  begin
    partition := WhichPartition(addr);
    with PartTable[partition] do
    begin
      AddToTail(partition,addr,true);
      PartTailFree := addr;
      PartNumFree := DoubleAdd(PartNumFree,DoubleInt(1));
      UpdatePartInfo(partition);
    end;
  end;


Procedure DeallocChain(firstaddr,lastaddr : DiskAddr; numblks : integer);
{-----------------------------------------------------
{ Abstract:
{   Deallocates a chain of blocks
{
{ Parameters:
{   firstAddr and lastAddr are addresses of blocks to deallocate (inclusive)
{   and numBlks is number of blocks to free
{
{ Side Effects:
{   Frees first and last addr using AddToTail; middle blocks not changed
{
{ Calls:
{   AddToTail, ChangeHeader, WhichPartition, DoubleAdd, FlushDisk,
{   UpdatePartInfo, DoubleInt
{
{ NOTE:  No checking is done to see if numBlks is correct
{-------------------------------------------------------}
  var
    hdptr     : ptrHeader;
    partition : integer;

  begin
    partition := WhichPartition(firstaddr);
    with PartTable[partition]do
    begin
      AddToTail(partition,firstaddr,false);
      hdptr := ChangeHeader(lastaddr);
      hdptr^.SerialNum := DBLZERO;
      hdptr^.NextAdr   := DBLZERO;
      hdptr^.Filler    := AddrToField(PartHeadFree);
      PartTailFree := lastaddr;
      PartNumFree := DoubleAdd(PartNumFree,DoubleInt(numblks));
      FlushDisk(lastaddr);
      PartNumOps := PartNumOps + numblks;
      UpdatePartInfo(partition);
    end;
  end.
