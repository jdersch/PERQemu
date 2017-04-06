{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module DiskIO;
{---------------------------------------------------------------------------
{
{ Abstract
{
{ This is an implementation of the DiskIO interface which uses the
{ new VolumeSystem module.  It is provided as a compatibility module
{ in order to support the k1 hardware with little modification of system
{ software above the level of DiskIO.  Eventually diskio will be removed
{ from the system and higher level software will need to modified to use
{ VolumeSystem directly; in particular, this will be required to support 
{ more than a single hard disk and single floppy.
{
{ Old Abstract:
{
{ This module implements the basic low level operations to disk devices.
{ It services the Hard Disk and the Floppy.  When dealing with the floppy
{ here, the structures on the hard disk are mapped to the structures
{ on the floppy.
{
{--------------------------------------------------------------------------}

{$Version 4.8 for POS}
{---------------------------------------------------------------------------
 Change history.
   
   9 Sep 83  SSJ  V4.8  Added the Generic5Inch disk type. Also fixed 
                        numberpages and LastDiskAddr to do the right thing.

   7 Sep 83  SSJ  V4.7  Fixed an exit in trydiskio. Was trying to exit diskio
                        for addr=0.
                        
  27 Apr 83  SSJ  V4.6  Increased NumPages for the EIO micropolis too. There
            is a conflict between what 3rcc and icl think the number is!!
            Also fixed TryDiskIO to return true for addr = 0 (a WJH fix)
              
  24 feb 83  CDH  V4.5  Changes for icl cio micropolis:
            Value CIOMicrop added to type DeviceType in place of 
            Unused1 in exports.
            NumberPages changed to deal with both devicetype 
            winch24/micropolis (to make existing programs work) and 
            devicetype CIOMicrop, in both cases returning 62K - 1 pages; 
            removed case branch for Unused1.  
            LastDiskAddr changed to include CIOMicrop and exclude Unused1.  
            Added command VolReset to handlers for VolIoFailure.

  10 Jan 83  AGR  V4.4  Added code to convert VolHdr commands to the 
                        appropriate DskHdr commands.
  27 Dec 82  AGR  V4.3  Replaced calls to Arith with long arithmatic. 
                        Replaced call to GetIntDiskKind with check of EIOflag
                        since EIO systems can have only one type disk.
   8 Dec 82   TV  V4.2  Fix some address type conversion problems. Tony Vezza.
  17 Nov 82  THD  V4.1  Add type FillerSemantics and add field FillerKind to 
                        the definition of type DiskBuffer.
   9 Nov 82  THD  V4.0  Reimplement using VolumeSystem and DiskAddressing 
                        interfaces.
  12 Jan 82  BAM  V3.13 Fix bug in FloppyIO failure so deallocates storage.
  24 Jun 81  BAM  V3.12 Fix to prevent retry if floppy write and device not
                          writable.
  28 May 81  BAM  V3.11 Fix bug in floppy header die-On-error.
                        Add Recalibrate light to floppy
                        New light definitions
  26 May 81  JPS  V3.10 Use new Lights module.
  19 May 81  BAM  V3.9  Fixed position of Reset light
  12 May 81  BAM  V3.8  Removed print out for DiskErrors; more accurate print
                          out of errors when happen; blink "light" during
                          IOReset.
                        Added new Exceptions DiskFailure and BadDevice;
                        Removed Procedure DiskError;
                        Use new IO
   6 May 81  JPS  V3.7  Fix bug in DiskReset by using the new form of the
                          SetCylinder StartIO.
  20 Apr 81  JPS  V3.6  Use DiskSegment consistently.
                        Set DiskSegment as UnSwappable for virtual memory.
   9 Apr 81  BAM  V3.5  Fix Retry so no recal on last time; DiskError tell op.
                         Fixed bug in DoDiskIO exit
  30 Mar 81  BAM  V3.4  Added Retry count to TryDisk and Const NumTries.
  27 Mar 81  BAM  V3.3  Added comments and WriteLn to DiskError.
  19 Mar 81  BAM  V3.2  Combined JPS+GGR and BAM's changes.
  17 Mar 81  GGR  V3.1  Removed WinchDebug and FloppyDebug.
  17 Mar 81  JPS  V3.0  Changed MapAddr and UnMapAddr to handle 24 Mbyte
                          drives.
                        Changed harddisk interlace factor to one.
                        Removed partition kind 'node'.
                        Added TryDiskIO, and changed DiskError printout.
                        Changed FirstDiskBlock for harddisks and
                        LastDiskBlock for 24 Mbyte disks.
                        Improved retry and retry messages from DiskIO and
                          FloppyIO.
  16 Mar 81  BAM  V2.2  Changed directory to have extra bits
   5 Mar 81  BR   V2.1  Added comments
   5 Mar 81  BAM  V2.0  Changed Definitions and import FileDefs.
   3 Mar 81  JPS  V1.4  Fix DiskReset to agree with IO V4.4.
   1 Mar 81  BR   V1.3  Change FileDate to TimeStamp.
   1 Mar 81  JPS  V1.2  Export the DiskReset routine.
  28 Feb 81  JPS  V1.1  Get rid of the LogMapping parameter to MapAddr by
                          teaching UnitIO not to fool with a hard disk address.
---------------------------------------------------------------------------}

{******************} exports {***************************}

imports FileDefs from FileDefs;
imports IOErrors from IOErrors;

const
    HARDNUMBER          = 0;             {device code of Shugart Disk}
    FLOPPYNUMBER        = 1;             {device code of FloppyDisk}

    {a Disk Address can be distinguished from a Segment Address by the
      upper two bits (in 32 bits).  These bits have a nonzero code to
      which disk the address is part of}

    RECORDIOBITS        = #140000;       {VirtualAddress upper 16 bits of disk}
    DISKBITS            = RECORDIOBITS + (HARDNUMBER*(#20000));
    FLOPBITS            = RECORDIOBITS + (FLOPPYNUMBER*(#20000));

    {The following definitions tell how many entries there are in the
     three pieces of the random index.  The first piece (Direct)
     are blocks whose DiskAddresses are actually contained in the
     Random Index (which is part of the FileInformationBlock).
     The second section has a list of blocks each of which contain 128 Disk
     Addresses of blocks in the file, forming a one level indirect addressing
     scheme.
     For very large files, the third section (DblInd) has DiskAddresses of
     blocks which point to other blocks which contain 128 DiskAddresses
     of blocks in the file, forming a two level indirect scheme.}

    DIRECTSIZE          =  64; { Entries in FIB of blocks directly accessable }
    INDSIZE             =  32; { Entries in FIB of 1 level indirect blocks }
    DBLINDSIZE          =   2; { Entries in FIB of 2 level indirect blocks }

    FILESPERDIRBLK      = 16;  { 256 / SizeOf(DirEntry) }
    NUMTRIES            = 15;  { number of tries at transfer before aborting }
    
type
    {Temporary segments go away when processes are destroyed,
     Permanent segments persist until explicitly destroyed
     Bad Segments are not well formed segments which are not
     readable by the Segment system}

    SpiceSegKind  = (Temporary, Permanent, Bad);
    PartitionType = (Root, UnUsed, Leaf); {A Root Partition is a device}
    DeviceType    = (Winch12, Winch24, FloppySingle, FloppyDouble,
                     CIOMicrop, Generic5Inch);

    MyDble = Array [0..1] of integer;
    
    DiskCheatType = record
                      case integer of
                        1: (
                             Addr    : DiskAddr
                           );
                        2: (
                             Dbl     : MyDble { should be IO.Double but don't
                                                import IO in export section }
                           );
                        3: (
                             Seg     : SegID
                           );
                        4: (
                             Lng     : FSBit32
                           )
                       end;
                       
    { A directory is an ordinary file which contains SegIDs of files
      along with their names.  Directories are hash coded by file name
      to make lookup fast.  They are often sparse files (ie contain
      unallocated blocks between allocated blocks).  The file name is a
      SimpleName, since a directory can only contain entries for
      files within the partition (and thus device) where the directory
      itself is located }

    DirEntry   = packed record 
                   InUse    : boolean; {true if this DirEntry is valid}
                   Deleted  : boolean; {true if entry deleted but not expunged}
                   Archived : boolean; {true if entry is on backup tape}
                   UnUsed   : 0..#17777;   {reserved for later use}
                   ID       : SegID;
                   Filename : SimpleName
                 end;

    { The fillers in headers which are used as hints to the new head of the 
      free list when a block is allocated in a partition (by AllocDisk) have
      been represented as unsigned 16 bit numbers denoting disk relative
      logical block numbers.  For disks larger than 64K blocks this is not
      possible and a partition relative denotation is used instead on all
      future disks.  FillerSemantics is the type of a field in the DIB of
      a disk which tells which interpretation applies for that disk. }
    FillerSemantics = (DiskRelative, PartRelative);

    DiskBuffer = packed record
                   case integer of
                     1: (
                          Addr : array [0..(DISKBUFSIZE div 2)-1] of DiskAddr
                        );
                     2: (
                          IntData : array [0..DISKBUFSIZE-1] of FSBit16
                        );
                     3: (
                          ByteData : packed array [0..DISKBUFSIZE*2-1] of FSBit8
                        );
  {4 is format of the FileInformationBlock; the FIB has Logical Block -1 }
                     4: ( 
                          
                          FSData     : FSDataEntry;

                          {The Random Index is a hint of the DiskAddresses
                           of the blocks that form the file.
                           It has three parts as noted above.  Notice
                           that all three parts are always there, so
                           that even in a very large file, the first
                           DIRECTSIZE blocks can be located quickly
                           The blocks in the Random index have logical
                           block numbers that are negative.  The logical
                           block number of Indirect[0] is -2 (the FIB is -1)
                           the last possible block's number is 
                           -(INDSIZE+DBLINBDSIZE+1)}

                          Direct     : array [0..DIRECTSIZE-1] of DiskAddr;
                          Indirect   : array [0..INDSIZE-1]    of DiskAddr;
                          DblInd     : array [0..DBLINDSIZE-1] of DiskAddr;
                          
                          SegKind    : SpiceSegKind;
                          
                          NumBlksInUse : integer; {segments can have gaps, 
                                                  block n may exist when block 
                                                  n-1 has never been allocated.
                                                  NumBlksInUse says how many
                                                  data blocks are actually used
                                                  by the segment}
                          LastBlk    : FSBit16;  {Logical Block Number of 
                                                  largest block allocated}
                          LastAddr   : DiskAddr; {DiskAddr of LastBlk }
                          LastNegBlk : FSBit16;  {Logical Block Number of
                                                  largest pointer block
                                                  allocated}
                          LastNegAddr: DiskAddr  {Block number of LastNegBlk}
                        );
 {5 is the format of a DiskInformationBlock or a PartitionInformationBlock}
                      5: (
                          
                          {The Free List is a chain of free blocks linked
                          by their headers }

                          FreeHead   : DiskAddr; {Hint of Block Number of the
                                                  head of the free list}
                          FreeTail   : DiskAddr; {Hint of Block Number of the
                                                  tail of the free list}
                          NumFree    : FSBit32;  {Hint of how many blocks
                                                  are on the free list}
                          RootDirID  : SegID;    {where to find the Root
                                                  Directory}
                          BadSegID   : SegID;    {where the bad segment is}

                          {when booting, the boot character is indexed into
                           the following tables to find where code to be
                           boot loaded is found }

                          BootTable  : array [0..25] of DiskAddr; {qcode}
                          InterpTable: array [0..25] of DiskAddr; {microcode}
                          PartName   : packed array [1..8] of char;
                          PartStart  : DiskAddr;
                          PartEnd    : DiskAddr;
                          SubParts   : array [0..63] of DiskAddr;
                          PartRoot   : DiskAddr;
                          PartKind   : PartitionType;
                          PartDevice : DeviceType
                        );
 {6 is the format of a block of a Directory}
                     6: (
                          Entry      : array [0..FILESPERDIRBLK-1] of DirEntry
                        );
 {7 is a format for DiskInformationBlocks which contains a flag denoting
    the meaning of filler words in the header of blocks on the disk;
    it takes advantage of the fact that the definitions of PIBs and DIB are 
    intertwined in variant 5 above so that the initial words in a DIB can
    be assumed to contain 0's since initial words are only used in blocks
    which are actually PIBs.  Hence all shugart disk volumes should have the
    value DiskRelative for the FillerKind field by default.}
                     7: (FillerKind : FillerSemantics)
                  end;
                  
    ptrDiskBuffer = ^DiskBuffer;
    
    Header     = packed record           {format of a block header}
                   SerialNum : DiskAddr; {Actually has the SegID of the file}
                   LogBlock  : integer;  {logical block number}
                   Filler    : integer;  {holds a hint to a candidate
                                          for the FreeHead}
                   PrevAdr   : DiskAddr; {Disk Address of the next block in
                                          this segment}
                   NextAdr   : DiskAddr; {Disk Address of the previous block in
                                          this segment}
                 end;

    ptrHeader  = ^Header;
    
    DiskCommand= (DskRead, DskWrite, DskFirstWrite, DskReset, DskHdrRead,
                      DskHdrWrite);  {last ones for error reporting}

var
    DiskSegment : integer;               {a memory segment for DiskIO}
                     
procedure InitDiskIO;  {initialize DiskIO, called at boot time}

procedure ZeroBuffer(ptr : ptrDiskBuffer);  {write zeroes in all words of the
                                            buffer. When reading an unallocated
                                            block, Zeros are returned in the
                                            buffer}

function WhichDisk(addr : DiskAddr) : integer;  {Tells you which disk number a
                                                 DiskAddr is on}

function AddrToField(addr : DiskAddr) : integer;  {gives you a one word short
                                                   address by taking the lower
                                                   byte of the upper word and
                                                   the upper byte of the lower
                                                   word.  The upper byte of the
                                                   upper word can't have any
                                                   significant bits for the 12
                                                   or 24 megabyte disks.  The
                                                   lower byte of the lower word
                                                   is always zero (since a disk
                                                   address is a page address,
                                                   which is 256 words
                                                  }


function FieldToAddr(disk: integer; fld : integer) : DiskAddr;
                                                  { Makes a DiskAddr out of a
                                                    short address and a disk
                                                    number
                                                  }


procedure DiskIO(addr : DiskAddr; ptr : ptrDiskBuffer; 
                 hptr : ptrHeader; dskcommand : DiskCommand); {Do a disk
                                                               operation, if
                                                               errors occur,
                                                               exits via
                                                               DiskError}


function LogAddrToPhysAddr(addr : DiskAddr) : DiskAddr;
                                      {translate a Logical Disk Address (used
                                       throughout the system) to and from a
                                       physical Disk Address (the kind the disk
                                       contoller sees) Logical Disk Addresses
                                       use a sequential numbering system
                                       Physical Disk Addresses have a
                                       Cylinder-Head-Sector system This routine
                                       calls MapAddr (a private routine which
                                       does the translation) Map Addr
                                       implements interlace algorithm}

function PhysAddrToLogAddr(disk : integer; addr : DiskAddr) : DiskAddr;

function LastDiskAddr(DevType : DeviceType) : DiskAddr; {Gets the Disk Address
                                                         of the last possible
                                                         page on the device}


function NumberPages(DevType : DeviceType) : FSBit32;  {Return the number of
                                                        pages on a device}


procedure DiskReset;  {Reset the disk controller and recalibrate the actuater}

function TryDiskIO(addr : DiskAddr;  ptr : ptrDiskBuffer;
                   hptr : ptrHeader;  dskcommand : DiskCommand;
                   numTries: integer) : boolean;
                                      {Try a disk operation, but, return false
                                       if error occurred
                                      }

Exception DiskFailure(msg: String; operation: DiskCommand; addr: DiskAddr;
                       softStat: integer);
Exception DiskError(msg: String);
Exception BadDevice;


Var ErrorCnt : Array[IOEFirstError..IOELastError] of integer;


{******************} private {***************************}

imports VolumeSystem from VolumeSystem;
imports DiskUtility from DiskUtility;
imports Screen from Screen; {used to force error messages to window 0}
imports System from System; {using UserError}
imports Memory from Memory; {using CreateSegment}
imports Lights from Lights;

const 
  DEBUG                 = false; 
  ShowRecalibrate       = LightUsed;
  


 {when mapping this file system onto the floppy, FSpDB floppy sectors are
 used for each disk page.  Two sectors of each track of the floppy hold
 headers for the remaining pages on that track}

type

  LocalCheatType = record
                    case integer of
                      1: (
                           Addr    : DiskAddr
                         );
                      2: (
                           Dbl     : Double
                         );
                      3: (
                           Seg     : SegID
                         );
                      4: (
                           Lng     : Long
                         )
                     end;
                     
  PhyDiskAddress = Long;

var
  DInitialized   : integer;



Procedure InitDiskIO;
{-------------------------------------------------
{ Abstract:
{   Initializes the DiskIO package
{
{ SideEffects:
{   Creates a new segment (DiskSegment).  Sets Initialzed.
{
{ Calls:
{   CreateSegment, InitVolumeSystem
{-------------------------------------------------}
   var 
      i : integer;

   begin
   if DInitialized = #12345 then exit(InitDiskIO);
   DInitialized := #12345;
   for i := IOEFirstError to IOELastError do
      ErrorCnt[i] := 0;
   InitVolumeSystem;
   end;


Function NumberPages(DevType:DeviceType) : FSBit32;
{-------------------------------------------------
{ Abstract:
{    Returns number of pages on a disk (actually the highest logical
{    block number possible for a DeviceType).
{
{ Parameters:
{    DevType: a device code
{-------------------------------------------------}
  var
    NumPages : long;
    numheads : integer;
    numsecpertrk : integer;
    numcyls : integer;
    Tmp : Long; 
  begin

    InitDiskIO;
 
   case DevType of            

     Winch12:  { 202 Cyls * 4 heads * 30 Sectors - 30 (boot area) - 1 }
       NumPages := stretch( 24209 );

     Winch24:
       if EIOFlag then
         Case GetIntDiskKind Of
                  {mic8- 478 Cyls * 5 heads * 24 Sectors - 48 (boot area) - 1 }
             Mic8:  NumPages := 63431  ;
             Mic5:  begin
                        GetDiskParameters(numheads, numsecpertrk, numcyls);
                        NumPages := stretch(numheads) * 
                                                numsecpertrk*numcyls - 33;
                        Tmp := NumHeads * NumSecPerTrk;
                        If NumPages > 63431 Then 
                         NumPages := 63431 - (63432 Mod Tmp);
                    end;
         end
       else
         case CIOdisktype of
           CIOshugart :  {202 Cyls * 8 heads * 30 Sectors - 30 (boot area) - 1}
              NumPages := 48449;
           CIOmicropolis:{530 Cyls * 5 heads * 24 Sectors - 24(boot area) - 1 }
              Numpages := 63487;  {can't use more than 31Mb worth}
           CIOUnknown :
              NumPages := 0;
         end;

     CIOMicrop : {530 Cyls * 5 heads * 24 Sectors - 24(boot area) - 1 }
       Numpages := 63487;  {can't use more than 31Mb worth}
     
     Generic5Inch:
          begin
          GetDiskParameters(numheads, numsecpertrk, numcyls);
          NumPages := stretch(numheads) * numsecpertrk*numcyls - 33;
          Tmp := NumHeads * NumSecPerTrk;
          If NumPages > 63431 Then 
              NumPages := 63431 - ((63432 + 32) Mod Tmp);
          end;
          
     FloppySingle:
       NumPages := stretch(431); 

     FloppyDouble:
       NumPages := stretch(893); 
     
     end;

   NumberPages := recast( NumPages, FSBit32 );

  end;


Function LastDiskAddr(DevType:DeviceType) : DiskAddr;
{-------------------------------------------------
{ Abstract:
{    Returns Disk Address of last possible page on a disk
{
{ Parameters:
{    DevType: a device code
{
{ Calls:
{    NumberOfPages
{-------------------------------------------------}
  var
    cheat : LocalCheatType;
  begin
    cheat.lng := stretch(DiskBufSize) * recast( NumberPages(DevType), long );
    case DevType of
      Winch12,
      Winch24,
      Generic5Inch,
      CIOMicrop:      cheat.dbl[1] := lor(cheat.dbl[1],DISKBITS);
      FloppySingle,
      FloppyDouble: cheat.dbl[1] := lor(cheat.dbl[1],FLOPBITS);
    end;
    LastDiskAddr := cheat.addr;
  end;


Procedure IncError(err: Integer);
{-------------------------------------------------
{ Abstract:
{    Increments the count of the error err unless err is undefined in which
{      case increments undefined error
{
{ Parameters
{    err is error number
{-------------------------------------------------}
   begin
   if (err < IOEFirstError) or (err > IOELastError) then
          err := IOEUDE;
   ErrorCnt[err] := ErrorCnt[err] + 1;
   end;


Procedure ZeroBuffer(ptr : ptrDiskBuffer);
{-------------------------------------------------
{ Abstract:
{    clear buffer to zeroes
{
{ Parameters:
{    ptr - a pointer to a disk buffer
{-------------------------------------------------}
  var
    i : integer;

  begin
    for i := 0 to (DISKBUFSIZE div 2) - 1 do
      begin
        ptr^.Addr[i] := DBLZERO;
      end;
  end;


Function WhichDisk(addr : DiskAddr) : integer;
{-------------------------------------------------
{ Abstract:
{    Return disk device of a DiskAddr
{
{ Parameters
{    addr a DiskAddr that you want the device code for
{-------------------------------------------------}
  var
    cheat : LocalCheatType;
        
  begin
    cheat.Addr := addr;
    WhichDisk := land(shift(cheat.Dbl[1],-13),#1);
  end;


Function AddrToField(addr : DiskAddr) : integer;
{-------------------------------------------------
{ Abstract:
{    return a short (one word) disk address by taking the low byte of the
{    most significant word of the Disk Address and the upper byte of the
{    least significant word
{
{ Parameters:
{    addr - the Disk Address of to get a short address for
{-------------------------------------------------}

    Var
        Cheat : LocalCheatType;
    
    Begin
        Cheat.Addr := Addr;
        If ( Cheat.Dbl[ 0] = 0 ) And ( Cheat.Dbl[ 1] = 0 )
            Then
                AddrToField := -1
            Else        
                AddrToField := Shift(Cheat.Dbl[0],-8) + Shift(Cheat.Dbl[1],8)
    End;


Function FieldToAddr(disk: integer; fld : integer) : DiskAddr;
{-------------------------------------------------
{ Abstract:
{    Return a DiskAddress given a short address and the disk device code
{
{ Parameters:
{    disk - the disk device code 
{    fld  - the short address (returned by AddrToField at some point)
{-------------------------------------------------}
  var
    addr: DiskAddr;
    cheat : LocalCheatType;

    Begin
        If Fld = -1
            Then
                Begin
                    Cheat.Dbl[ 0] := 0;
                    Cheat.Dbl[ 1] := 0;
                    FieldToAddr := Cheat.Addr
                End
            Else
                Begin
                    Cheat.Dbl[ 0] := Shift(Fld,8);
                    Cheat.Dbl[ 1] :=
                            Lor(Shift(Fld,-8) + Shift(Disk,13),RECORDIOBITS);
                    FieldToAddr := Cheat.Addr
                End
    End;


Function LogAddrToPhysAddr(addr : DiskAddr) : DiskAddr;
{-------------------------------------------------
{ Abstract:
{    Convert a Logical DiskAddress to a Physical Disk Address
{
{ Parameters:
{    addr - the disk address to be converted
{
{ Calls:
{-------------------------------------------------}
  var
    VA : VolAddress;
    Coerce : packed record
                case integer of
                   0: (Dbl : Double);
                   1: (Lng : Long);
                   2: (DA  : DiskAddr)
                end;
  
  begin
        Coerce.DA := Addr;
        If ( Coerce.Dbl[ 0] = 0 ) And ( Coerce.Dbl[ 1] = 0 )
            Then
                Begin
                    LogAddrToPhysAddr := Coerce.DA
                End
            Else
                case WhichDisk(addr) of
                   HardNumber:
                       Begin
                           Coerce.Dbl[ 1] := 0;
                           Coerce.Dbl[ 0] := AddrToField(addr);
                           VA.Volume := 0;
                           VA.BlockNumber := Coerce.Lng;
                           Coerce.Dbl := VolToPhyAddr( VA);
                           LogAddrToPhysAddr := Coerce.DA
                       End;
                   FloppyNumber:
                       Begin
                           Coerce.Dbl[ 1] := 0;
                           Coerce.Dbl[ 0] := AddrToField(addr);
                           VA.Volume := 4;
                           VA.BlockNumber := Coerce.Lng;
                           Coerce.Dbl := VolToPhyAddr( VA);
                           LogAddrToPhysAddr := Coerce.DA
                       End
                 End { Case }
  end;


Function PhysAddrToLogAddr(disk : integer; addr : DiskAddr) : DiskAddr;
{-------------------------------------------------
{ Abstract:
{    Translate a Physical Disk Address to a logical disk address
{
{ Parameters:
{    disk - Device code of disk
{    addr - Physical address to translate
{
{ Calls:
{    UnMapAddr
{-------------------------------------------------}
  var
    VID : VolID;
    VA : VolAddress;
  
    coerce : packed record
                case integer of
                   0: (da  : DiskAddr);
                   1: (pda : PhyDiskAddress);
                   2: (Dbl : Double);
                   3: (Lng : Long)
             end;
  
  begin
        Coerce.DA := Addr;
        If ( Coerce.Dbl[ 0] = 0 ) And ( Coerce.Dbl[ 1] = 0 )
            Then
                Begin
                    PhysAddrToLogAddr := Coerce.DA
                End
            Else        
                case disk of
                    HardNumber:
                        Begin
                            VID := 0;
                            Coerce.DA := addr;
                            VA := PhyToVolAddr( VID, Coerce.Dbl);
                            Coerce.Lng := VA.BlockNumber;
                            PhysAddrToLogAddr :=
                                 FieldToAddr(disk, Coerce.Dbl[ 0])
                        End;
                    FloppyNumber:
                        Begin
                            VID := 4;
                            Coerce.DA := addr;
                            VA := PhyToVolAddr( VID, Coerce.Dbl);
                            Coerce.Lng := VA.BlockNumber;
                            PhysAddrToLogAddr :=
                                 FieldToAddr(disk, Coerce.Dbl[ 0])
                        End;
                  End { Case }
  end;


procedure VolHdrToLogHdr(disk   : integer;
                         volhdr : ptrVolHeaderBuffer;
                         loghdr : ptrHeader);

{---------------------------------------------------------
{ Abstract:
{    translate a header buffer in the format used by 
{    VolumeSystem to that used by DiskIO
{
{ Parameters:
{    disk   - disk on which the logical header is to appear to reside
{    volhdr - pointer to a buffer containing a header in 
{             VolumeSystem format
{    loghdr - pointer to a buffer to contain the header 
{             translated into DiskIO format
{
{--------------------------------------------------------}

    Var
        coerce : packed record
                case integer of
                   0: (da  : DiskAddr);
                   1: (pda : PhyDiskAddress);
                   2: (Dbl : Double);
                   3: (Lng : Long)
             end;

   begin
   Coerce.Lng := volhdr^.SerialNumber;
   loghdr^.SerialNum := FieldToAddr(disk, Coerce.Dbl[ 0]);
   with loghdr^ do
      with volhdr^ do
         begin
         LogBlock := SegmentBlockNumber;
         filler   := FreeListHint;
         Coerce.Lng := PreviousBlock;
         PrevAdr  := FieldToAddr(disk, Coerce.Dbl[ 0]);
         Coerce.Lng := NextBlock;
         NextAdr  := FieldToAddr(disk, Coerce.Dbl[ 0]);
         end;
   end; { VolHdrToLogHdr }


procedure LogHdrToVolHdr(loghdr : ptrHeader;
                         volhdr : ptrVolHeaderBuffer);


{---------------------------------------------------------
{ Abstract:
{    translate a header buffer in the format used by
{    DiskIO to that used by VolumeSystem
{
{ Parameters:                                              
{    volhdr - pointer to a buffer containing a header in 
{             VolumeSystem format
{    loghdr - pointer to a buffer to contain the header 
{             translated into DiskIO format
{
{--------------------------------------------------------}

Var
    coerce : packed record
                case integer of
                   0: (Dbl : Double);
                   1: (Lng : Long)
             end;
  
   begin
   Coerce.Dbl[ 1] := 0;
   Coerce.Dbl[ 0] := AddrToField(loghdr^.SerialNum);
   If ( Coerce.Dbl[ 0] = -1 ) Then Coerce.Dbl[ 1] := -1;
   volhdr^.SerialNumber := Coerce.Lng;
   with loghdr^ do
      with volhdr^ do
         begin
         SegmentBlockNumber := LogBlock;
         FreeListHint       := filler;
         Coerce.Dbl[ 1] := 0;
         Coerce.Dbl[ 0] := AddrToField(loghdr^.PrevAdr);
         If ( Coerce.Dbl[ 0] = -1 ) Then Coerce.Dbl[ 1] := -1;
         PreviousBlock  := Coerce.Lng;
         Coerce.Dbl[ 1] := 0;
         Coerce.Dbl[ 0] := AddrToField(loghdr^.NextAdr);
         If ( Coerce.Dbl[ 0] = -1 ) Then Coerce.Dbl[ 1] := -1;
         NextBlock      := Coerce.Lng
         end;
   end; { LogHdrToVolHdr }


Procedure DiskReset;
{-------------------------------------------------
{ Abstract:
{   Reset the Hard Disk and recalibrate the drive
{
{ Calls:
{   VolDiskReset
{
{-------------------------------------------------}
   var
     HardDisk : PhyDiskID;

   handler NoSuchVol(vid : VolID);

      var
         HardDisk : PhyDiskID;
         v        : VolID;

      begin
      HardDisk.kind    := IntDisk; { hard disk is the internal disk }
      HardDisk.IntUnit := 0;       {    with unit number 0          }
      v :=VolMount(HardDisk);
      VolDiskReset(v);
      VolDisMount(HardDisk);
      exit(DiskReset);
      end;

   begin
   { assuming the hard disk is mounted as vol 0, reset it; 
     if it is not mounted it will be in the NoSuchVol handler }
   VolDiskReset(0);
   end; 





Procedure DiskIO(addr       : DiskAddr; 
                 ptr        : ptrDiskBuffer; 
                 hptr       : ptrHeader;
                 dskcommand : DiskCommand);
{-------------------------------------------------
{ Abstract:
{   
{
{ Parameters:
{   
{-------------------------------------------------}

var
   Cmd   : VolIOCommand;
   vaddr : VolAddress;
   Dbl : Double;
   vptr  : ptrVolBuffer;
   vhptr : ptrVolHeaderBuffer;
    coerce : packed record
                case integer of
                   0: (Dbl : Double);
                   1: (Lng : Long)
             end;


    Handler VolIOFailure( Msg: String;
                          Operation: VolIOCommand;
                          Addr: VolAddress;
                          SoftStat: Integer);
        
        Var
            Com : DiskCommand;
            Fld : packed record
                case integer of
                   0: (Dbl : Double);
                   1: (Lng : Long)
                 end;
            Disk : Integer;
            
        Begin
            Case Operation Of
                VolRd : Com := DskRead;
                VolWr : Com := DskFirstWrite;
                VolWrCheck : Com := DskWrite;
                VolHdrRead : Com := DskHdrRead;
                VolHdrWrite : Com := DskHdrWrite;
                VolReset : Com := DskReset;
              End; { Case }
            Fld.Lng := Addr.BlockNumber;
            If Addr.Volume = 0
                Then Disk := 0
                Else Disk := 1;
            Raise DiskFailure(Msg,Com,FieldToAddr(Disk,Fld.Dbl[ 0]),SoftStat)
        End;

  handler VolErrInc(x : integer);
  
    begin
    IncError(x);
    end; { VolErrInc handler}

begin

    If Addr = DBLZERO
        Then
            Begin
                If DskCommand = DskRead
                    Then
                        Begin
                            HPtr^.SerialNum := DBLZERO;
                            HPtr^.PrevAdr   := DBLZERO;
                            HPtr^.NextAdr   := DBLZERO;
                            HPtr^.LogBlock  := 0;
                            HPtr^.Filler    := 0;
                            ZeroBuffer( Ptr)
                        End;
                Exit( DiskIO)
            End;
                                            

{convert a DiskIO format address to a VolAddess}
case WhichDisk(addr) of
   HARDNUMBER:
      vaddr.Volume := 0;
   FLOPPYNUMBER:
      vaddr.Volume := 4;
   end;

Coerce.Dbl[ 1] := 0;
Coerce.Dbl[ 0] := AddrToField(addr);
vaddr.BlockNumber := Coerce.Lng;

{coerce data buffer pointer from DiskIO to VolumeSystem terms}
vptr := recast(ptr, ptrVolBuffer);

{tranlate header to Volume System format}
new(DiskSegment, 4, vhptr);
LogHdrToVolHdr(hptr, vhptr);

{translate command to VolumeSystem terms}
case dskCommand of
   DskRead:
      Cmd := VolRd;
   DskFirstWrite:
      Cmd := VolWr;
   DskWrite:
      Cmd := VolWrCheck
   end;

{do the VolumeSystem operation}
VolIO(vaddr, vptr, vhptr, cmd);

{translate the header back to DiskIO format}
if cmd = VolRd then 
case WhichDisk(addr) of
       HARDNUMBER:
           VolHdrToLogHdr(0, vhptr, hptr);
       FLOPPYNUMBER:
           VolHdrToLogHdr(1, vhptr, hptr)
       end;

dispose(vhptr);

end; { DiskIO }

function TryDiskIO(addr       : DiskAddr;  
                   ptr        : ptrDiskBuffer;
                   hptr       : ptrHeader; 
                   dskcommand : DiskCommand;
                   numTries   : integer
                            ) : boolean;
{-------------------------------------------------
{ Abstract:
{   
{
{ Parameters:
{   
{
{ Returns:
{   true if tranfer went ok, false if errors occurred
{-------------------------------------------------}

var
   Cmd   : VolIOCommand;
   vaddr : VolAddress;
   Dbl : Double;
   vptr  : ptrVolBuffer;
   vhptr : ptrVolHeaderBuffer;
    coerce : packed record
                case integer of
                   0: (Dbl : Double);
                   1: (Lng : Long)
             end;

    Handler VolIOFailure( Msg: String;
                          Operation: VolIOCommand;
                          Addr: VolAddress;
                          SoftStat: Integer);
        
        Var
            Com : DiskCommand;
            Fld : packed record
                case integer of
                   0: (Dbl : Double);
                   1: (Lng : Long)
                 end;
            Disk : Integer;
            
        Begin
            Case Operation Of
                VolRd : Com := DskRead;
                VolWr : Com := DskFirstWrite;
                VolWrCheck : Com := DskWrite;
                VolHdrRead : Com := DskHdrRead;
                VolHdrWrite : Com := DskHdrWrite;
                VolReset : Com := DskReset;
              End; { Case }
            Fld.Lng := Addr.BlockNumber;
            If Addr.Volume = 0
                Then Disk := 0
                Else Disk := 1;
            Raise DiskFailure(Msg,Com,FieldToAddr(Disk,Fld.Dbl[ 0]),SoftStat)
        End;

  handler VolErrInc(x : integer);
  
    begin
    IncError(x);
    end; { VolErrInc handler}

begin

    If Addr = DBLZERO
        Then
            Begin
                If DskCommand = DskRead
                    Then
                        Begin
                            HPtr^.SerialNum := DBLZERO;
                            HPtr^.PrevAdr   := DBLZERO;
                            HPtr^.NextAdr   := DBLZERO;
                            HPtr^.LogBlock  := 0;
                            HPtr^.Filler    := 0;
                            ZeroBuffer( Ptr)
                        End;
                TryDiskIO := True;
                Exit( TryDiskIO)
            End;

{convert a DiskIO format address to a VolAddess}
case WhichDisk(addr) of
   HARDNUMBER:
      vaddr.Volume := 0;
   FLOPPYNUMBER:
      vaddr.Volume := 4;
   end;
Coerce.Dbl[ 1] := 0;
Coerce.Dbl[ 0] := AddrToField(addr);
vaddr.BlockNumber := Coerce.Lng;

{coerce data buffer pointer from DiskIO to VolumeSystem terms}
vptr := recast(ptr, ptrVolBuffer);

{tranlate header to Volume System format}
new(DiskSegment, 4, vhptr);
LogHdrToVolHdr(hptr, vhptr);

{translate command to VolumeSystem terms}
case dskCommand of
   DskRead:
      Cmd := VolRd;
   DskFirstWrite:
      Cmd := VolWr;
   DskWrite:
      Cmd := VolWrCheck
   end;

{do the VolumeSystem operation}
TryDiskIO := TryVolIO(vaddr, vptr, vhptr, cmd, numtries);

{translate the header back to DiskIO format}
if cmd = VolRd then 
case WhichDisk(addr) of
       HARDNUMBER:
           VolHdrToLogHdr(0, vhptr, hptr);
       FLOPPYNUMBER:
           VolHdrToLogHdr(1, vhptr, hptr)
       end;

dispose(vhptr);

end.

