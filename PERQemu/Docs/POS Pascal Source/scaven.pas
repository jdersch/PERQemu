{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program Scavenger;
{-----------------------------------------------------------------------
{ ABSTRACT:
{       The scavenger analyzes the disk for inconsistencies in the
{       structure of segments (within limits allowed by the SPICE Segment
{       System) and directories (within limits allowed by the SPICE File
{       System).  If directed to do so, the scavenger attempts to
{       correct any inconsistencies it finds.
{
{   copyright 1982, 1983  Three Rivers Computer Corporation
{
{-----------------------------------------------------------------------}



{$Version V5.6 for POS}
{-----------------------------------------------------------------------
{ Change Log:
{
{ 10 jul 84  V5.6  John Briggs
{ Fixed FixPairTable to test for 8inch micropolis disks - it used to get
{ away with it until the config bug was fixed in v5.5
{
{ 29 jun 84  V5.5  John Briggs
{ Added :-10 to writelns of block numbers.
{ Fix bug setting up configuration for 8inch micropolis disks - test on 
{ eioflag was the wrong way round.
{ Change NotOver64K to compare address with EndBlk not 64K.
{ VBNOutofRange in SetUpBadBlocks and PrintDIB is probably redundant -
{ exception not raised by PhysAddrtoLogAddr.Put in checks for cylinder
{ number out of range instead.
{
{ 27 jun 84  V5.4  John Briggs
{ Fix bug - giving a partition name of all spaces is matched with a null entry
{ in PartNames - now check PartDAs[i] <> Zero.
{ Ignore the block if we get VBNOutofRange in SetupBadBlocks.
{ Handle VBNOutOfRange in PrintDIB as boots may contain addresses greater 
{ than 31MB.
{ Set PartNames[i] = '        ' and PartDAs[i] = Zero for addresses > 64K
{ pages (uses proc NotOver64K) to prevent accessing these addresses.
{ 
{ 14 Apr 84  V5.3  Sandeep Johar
{ Handle VBNOutOfRange in setupbad blocks as map may contain addresses greater 
{ than 31MB.
{
{  6 Feb 84  V5.2  Sandeep Johar
{ Fixed a bug in setupbadblocks.
{
{  1-Nov-83  V5.1  Scott Brown for Rick Raschid
{ Fix a bug in releasing large temp files.
{
{ 26 Oct 83  V5.0      Sandeep Johar
{           Called setupdisk to initialize kind of disk.
{
{ 20 Oct 83  V4.92     Sandeep Johar
{           Changed the question " this seems to be a 5.25" disk "
{
{ 11 Oct 83  V4.91     Sandeep Johar
{           Read the bad block list from cylinder 0, head 2, sector 0 for 5.25
{           Setup the parameters for the particular 5.25" disk being used.
{
{ 10 Oct 83  V4.9      S Johar
{           Added the 5.25" disk type.
{           Added a handler For CtlC in PrintMap to exit it early.
{
{ 21 Jul 83  V4.8      S Johar For  J Strait
{                                Look for loops through the Prev pointer
{                                as well as through the Next pointer.  Loops
{                                through either pointer will bomb the code
{                                looks for free blocks but the Scavenger
{                                was only breaking loops through Next.
{
{ 13 Apr 83  V4.7  Chris Hughes  Modify for cio micropolis disks:
{           Fixpairtable and printfailblock changed to reflect new physical 
{           address structure.  NB These have not been changed for eio disks!!
{           PrintFailBlock treats cio micropolis as eio. 
{           The 'rewrite bad blocks' prompt changed to 'rewrite incorrigible 
{           blocks'.  
{           The new disktype CIOMicrop is used internally (edit search for
{           ciomicrop to find out where).
{           Bug removed: if you don't allow changes to the disk, still 
{           prompt for tracks/blocks to ignore.
{
{ 23 Feb 83  V4.6  Brad Myers   Speed up by making interlace 16 instead of 8.
{
{ 18 Feb 83  V4.5  Brad Myers   Ask about apparent disk type.
{                               Handle VBNOutOfRange in DiskIO.
{
{ 15 Feb 83  V4.4  Brad Myers   Fixed for landscape monitor.
{                               Allow partition name on command line.
{                               Fix print out of bad block phys addr.
{
{ 28 Dec 82  V4.3  August G. Reinig
{                                Taught scavenger about EIO systems.
{                                Changed AddrToIdx to handle AddrToField's
{                                returning -1 when passed a nil DiskAddr
{                                instead of zero.  Fixed bug in code keeping
{                                track of temp segments when scavenger
{                                shouldn't change the disk.  Fixed bug in
{                                assignment of the free list hint of the last
{                                block in the free list.
{
{ 19 Oct 82  V4.2  Brad Myers    (1) Delete temporary and bad, etc. segments
{                                     during w.f.c. building.
{                                 (2) Don't make the free list into the
{                                     bad segment.
{                                 (3) Fix bug in loop blink.
{                                 (4) Print out names of files put into bad
{                                     segment.
{                                 (5) Mark the first block of each chain in
{                                     the bad segment with a DEL.
{                                 (6) Allow blocks to be typed in as a long.
{                                 (7) Default for number of retries on pass 1
{                                     changed to be 1.
{ 28 Apr 82  V4.1  Ellen Colwell Change variable names to be distinct in
{                                eight characters.
{ 25 Mar 82  V4.0  Brad Myers   Allow specifying a cyl, head to skip.
{ 24 Mar 82  V3.4  Brad Myers   DoDiskIO has more retries in later passes.
{                               Ask for bad blocks.
{                               Fix ask for device.
{                               BlinkScreen if find errors for Serial or
{                               Logical block consistency.
{                               More info on block failure.
{ 16 Mar 82  V3.3  J Strait     Fix bug: Initialize cursor pictures before
{                               dismounting the partition.  If the
{                               file is read after dismounting and the
{                               file is in the partition being scavenged,
{                               the Scavenger usually screws up the
{                               freelist.
{ 19 Jan 82  V3.2  BAM          Move pictures of cursors into a file so
{                               Scavenger file is smaller.
{                               Change messages from "Root" to "Device".
{                               Added TryCount which may be <> NUMTRIES.
{                               Consolidated ShowFail messages and print
{                               correct block number
{                               Default for Check Serial numbers = No if small
{                               memory.
{ 30-Jun-81  V3.1  BAM          Disable Swapping
{                               DirScavenge fixed to put right name for dir in
{                               FI blk.
{ 23-Jun-81  V3.0  BAM          Scavenger decides whether is big or small
{                               device; just tell it floppy or harddisk
{                               Fixed bad chain processing so put in bad file
{                               if found due to bad block number, etc.
{                               Fixed bug in Scav title for Partition info
{                               block
{  3-Jun-81  V2.11 BAM          Unsigned decimal output
{                               Add question for check length in DirScavenge
{                               IO => IO_Others
{  1-May-81  V2.10 BAM          Changed default for fixing block numbers to
{                               false and moved check to after RebuildIndex
{                               Added new file type SwapFile whose length is
{                               not checked
{                               Change SegKind of bad file to Permanent and
{                               set FSData.FileType to bad if entered
{                               successfully
{ 24-Apr-81  V2.9  BAM          Fixed bug in loop check blink
{                               Deleted checking for bad block loops
{                               Fixed DirScavenge to fix bad file length
{                               If file name starts with .. then bad name
{ 20-Apr-81  V2.8  BAM          Allow deleting of old directories when
{                               creating new ones
{                               Check before Rebuild directories for enough
{                               free pages
{                               Fixed title just before exit
{                               Removed import of CursorIO
{  9-Apr-81  V2.7  BAM          1) AddToTitleLine => ChangeTitle
{                               2) Add Default value for Rebuild directories
{                               3) Don't change default partition name
{                               4) Deallocate storage of SegNext so room for
{                                  directory recreation
{                               5) Scav for bad blocks <= NO
{                               6) Fix so loops detected
{ 30-Mar-81  V2.6  BAM          SpiceDir,Seg => FileDir, FileAccess;
{                               TryDiskIO now has retry count;
{                               Use FSAddToTitleLine (from FileUtils)
{                               Remove Bad boots
{ 25-Mar-81  V2.5  JPS          New Memory Manager interface;
{ 24-Mar-81  V2.4  BAM          Does UpperCase on Partnames so don't have to
{                               type in same case as specified;
{                               Fixed bug in foo.dr processing in DirScavenge
{                               when foo.dr not directory;
{                               Fixed DirScavenge output format
{
{ 19-Mar-81  V2.3  BAM          Changed Bit32 to FSBit32; Changed format for
{                                output of Root and Partitions
{
{ 17-Mar-81  V2.2  BAM          Added CanReWrite as global set if CanChange.
{
{ 17-Mar-81  V2.1  GGR          Undo Rebuild directory test.
{                               Removed WinchDebug and FloppyDebug.
{                               Removed DryRun question.
{                               Removed temporary fix to DismountPartition bug
{                               Replaced incorrigible page detection with
{                               use of TryDiskIO.
{                               Set interlace to 8 for new FS.
{
{ 14-Mar-81  V2.0  John Strait  Set Interlace to 1 for CMU interlaced FS.
{
{ 13-Mar-81  V1.9  John Strait  Set window title to show current actions.
{                               Make DebugMax a constant False.
{                               Try to prevent Scavenger from hanging when
{                               blocks are linked into loops - put such chains
{                               into the bad segment.
{                               Try to make scavenging a full disk partition
{                               possible by doing block number and serial
{                               number checks in separate passes when the
{                               user says not to try one pass.
{                               Move some of the code for bad blocks into the
{                               first pass so that it will work when the
{                               user says there isn't enough memory to do it
{                               in one pass.
{
{ 12-Mar-81  V1.8  John Strait  Build the bad segment right.
{                               Add default for Ask.
{
{ 12-Mar-81  V1.7  John Strait  Make Interlace a constant 8.
{
{ 12-Mar-81  V1.6  John Strait  Do a ForgetAll after dismounting a partition.
{                               Normally, when a device is dismounted before
{                               any of its partitions have been mounted by
{                               MountPartition (this happens if you mount a
{                               device and then scavenge without touching the
{                               partition first).  In this case, PartInUse is
{                               true, but PartMounted is false.  This means
{                               that DismountPartition will do nothing, but
{                               the InfoBlk will still be in a buffer.
{                               This bug really ought to be fixed in
{                               DismountPartition.
{
{ 12-Mar-81  V1.5  John Strait  Be clever: try to get as many data segments
{                               as possible on the screen.
{                               Make cursors show which interlace pass is
{                               being done.
{
{ 10-Mar-81  V1.4  John Strait  Fix a bug or two in the treatment of BAD
{                               blocks versus INCORRIGIBLE blocks that I
{                               introduced in V1.3.
{                               Implement a variable interlacing factor
{                               (local to the Scavenger).
{
{  7-Mar-81  V1.3  John Strait  Fix a bug or two in the treatment of BAD
{                               blocks versus INCORRIGIBLE blocks.
{                               Report BAD blocks and INCORRIGIBLE blocks
{                               separately.
{                               Ask about rebuilding the directory if
{                               not Consistent or CanChange.
{                               ---
{
{  6-Mar-81  V1.2  John Strait  Fix Pass 1, Step 4 for INCORRIGIBLE blocks.
{                               When rebuilding the directory, allow re-
{                               building the random indexes also (the code to
{                               do this was modeled after Scavenge in
{                               SpiceSeg).
{
{  5-Mar-81  V1.1  John Strait  Add version number constant.
{                               Teach Scavenger to check for unreadable blocks
{                               and mark them as INCORRIGIBLE.
{                               Allocate many pointers on 4 and 256 words.
{
{ 15-Jan-81  George Robertson (ggr) at Carnegie-Mellon University
{ Created.
{----------------------------------------------------------------------}

{$R-}
imports FileAccess from FileAccess;
imports DiskIO from DiskIO;
imports Arith from Arith;
imports Memory from Memory;
imports FileDir from FileDir;
imports IO_Unit from IO_Unit;
imports IO_Others from IO_Others;
imports IOErrors from IOErrors;
imports Screen from Screen;
imports PERQ_String from PERQ_String;
imports System from System;
imports FileSystem from FileSystem;  {for cursors}
imports DiskDef from DiskDef;
imports DiskParams From DiskParams;
imports Stream from Stream;

const ScavVersion = 'V5.6';
      ScavCursors = 'Scavenger.Animate';

type
    BitMap = packed array [0..0] of boolean;
    ptrBitMap = ^BitMap;

var
    SegHead : ptrBitMap;      { bit=1 => block is segment head }

{***********************************************************************
{ DEFINITIONS:
{
{  A well-formed chain (wfc) is a maximal set of pages p0,...,pn
{       such that, for i=0 to n, pi.NextAdr = Adr(p(i+1)) and
{       p(i+1).PrevAdr = Adr(pi).
{
{  A well-formed segment (wfs) is a set of pages p(-m),...,p(-1),...,pn
{       with m >= 1 and n >= 1 such that
{       1. p(-m),...,p(-1),...,pn is a wfc
{       2. p(i+1).LogBlock > pi.LogBlock
{       3. p(-m).PrevAdr = 0
{       4. pn.NextAdr = 0
{       5. pi.SerialNum = Adr(p(-1)) for each pi
{
{  Note: Conditions 2 (logical block consistency) and 5 (serial number
{       consistency) are only checked if the scavenger has enough primary
{       memory.  In particular, disks larger than 12 Mbytes need more than
{       256K words Mp.
{       The SPICE Segment System checks for these types of consistency at
{       runtime, but does not recover from all such errors.
{
{  Note:  The segment header block is not checked for consistency (the
{       SPICE Segment System does that at runtime).
{
{  The SegmentKind (Temporary, Bad, or Permanent) is indicated in the segment
{       header (logical block -1 of the segment).  Temporary segments and
{       old bad segments are reclaimed by the scavenger.
{
{  A page is GOOD if it is part of a wfs.
{  A page is FREE if its SerialNum is 0, or if it is in a chain
{       with some FREE page before it and some FREE page after it.
{  A page is INCORRIGIBLE if its SerialNum is -1 or it exhibits read errors or
{       a bad sector label which cannot be corrected by writing.  Incorrigible
{       pages are not marked free or bad, hence are lost (until another
{       scavenge, which may reclaim them).
{  A page is BAD if it is none of the above.  Scavenger links all bad pages
{       into one segment (which it creates), and puts a pointer to that
{       segment into the disk information block.
{
{*****************************************************************************}

Const
    CURMAXY = 1005;
    INCORRIGIBLE = -1;
    BADADDR = -2;

    DebugMax = False;           { true => print map }
    DryRun   = False;           { true => no write operations }

    Interlace = 16;
    MAXBADBLOCKS = 101;

Type
    IntArray = array [0..0] of integer;
    ptrIntArray = ^IntArray;


Var
    LastCyl : integer; {cylinder number of last block on disk (as POS sees it)}
    ScavPartitionName: SimpleName; {The full name of the partition to be fixed}
    ScavDeviceName: SimpleName;  {The device name of the partition being fixed}
    ScavTitle: String;
    Configuration : DeviceType;
    CanChange : boolean;        { true => can make changes to the disk }
    CanReWrite: boolean;        { true => can ReWrite bad blocks }
    CheckBlockNum : boolean;    { true => check logical block consistency. }
    CheckSerialNum : boolean;   { true => check serial number consistency. }
    DeleteTemps : boolean;      { true => delete temporary segments }
    Consistent : boolean;       { true => no errors found in scavenge }
    DelOldDirs : boolean;       { true => can delete old directories }
    Debug : boolean;            { true => print map for all errors }
    TempCnt : integer;          { Count of deleted temporary segments. }
    EvilCnt : integer;          { Count of incorrigible pages. }
    FreeCnt : FSBit32;          { Count of blocks on free list. }
    InfoBlk : FSBit32;
    ScavStrtBlk : FSBit32;
    StartField : integer;
    EndBlk : FSBit32;
    EndField : integer;
    EndIdx : integer;   { Idx ranges from 1 to EndField-StartField+1 }
    BadHeadAddr : FSBit32;
    LabelPtr : ptrHeader;
    BufferPtr : ptrDiskBuffer;
    Buffer2   : ptrDiskBuffer;
    Label2    : ptrHeader;
    ZeroBufPtr : ptrDiskBuffer;
    WordsPerBlock : FSBit32;
    Zero, One : FSBit32;
    TypeDisk : integer;
    Disk : integer;
    CurY, CurX : integer;
    FirstIdx : integer;         { First block in this interlace pass }
    YMod : integer;             { Every YInc blocks, incr Y by YMod }
    YInc : integer;
    XInc : integer;             { Every interlace pass, incr X by XInc }
    ReadCursor : CurPatPtr;
    WriteCursor : CurPatPtr;
    BusyCursor : CurPatPtr;
    LstCursor : CurPatPtr;
    PartNames : array [0..63] of string[8];
    PartDA    : DiskAddr;
    PartDAs   : array [0..63] of DiskAddr;
    PName     : string;
    OnePass   : boolean;
    Pass      : integer;
    cheat     : DiskCheatType;
    i, j, cnt : integer;
    SegSize, SegBMSize, SegNext, SegPrev, SegBlkNum, SegSerNum : integer;
    SegConfirmed, SegFree, SegSegHead, SegChainHead, SegTempHead : integer;
    gotanswer : boolean;
    Name      : string[8];
    s         : string;
    c         : char;
    RootName  : string[8];

    Next : ptrIntArray;
    Prev : ptrIntArray;
    BlockNum : ptrIntArray;
    SerNum : ptrIntArray;
    ChainHead : ptrBitMap;
    Confirmed : ptrBitMap;
    FreeBlk : ptrBitMap;
    TempHead : ptrBitMap;

    NumBadBlocks : Integer;
    BadBlocks : Packed Array[1..MAXBADBLOCKS] Of Integer;

    DiskName : String;
    NHeads,
    NCyls,
    NSecs,
    BootSize,
    WriteCompCyl: Integer;
    { create bad segment }

    BadHead, BadTail, BadPages: Integer;
    TryCount : integer;
    auto: Boolean;
    CURMAXX: Integer;
    
Const NumIgnoreBlocks = 15;
      NIgnorePairs = 6;
Var
(**)    BadBlks: Array[1..NumIgnoreBlocks] of integer;
(**)    ignoreBlocks, ignoreCheck, ignorePairs, temp: integer;
(**)    BadPairs: Array[1..NIgnorePairs] of
(**)                Record case boolean of
(**)                         true:  (cyl, head: integer);
(**)                         false: (firstBad, lastBad: integer);
(**)                End;


{*********************** Utilities **************************************}


function CylinRange(PhysAddr : DiskAddr) : boolean;
var Cheat : DiskCheatType;
  begin
  Cheat.Addr := PhysAddr;
  case Disk of
    0: if EIOFlag or 
          (ciodisktype = ciomicropolis) or 
          (Configuration =Generic5Inch)
         then CylinRange := (LastCyl >= Abs(Cheat.Dbl[1]))
         else CylinRange := (LastCyl >= Shift(Cheat.Dbl[0], -8));
    1: CylinRange := true; {hopefully true!}
    end;
  end;

 
function NotOver64K(Addr : DiskAddr) : boolean;
  var
   MyAddr : DiskCheatType;
   MyLastAddr : DiskCheatType;
   MyAddrHi  : integer;
   MyLastAddrHi : integer;
  begin
  MyAddr.Addr := Addr;
  MyLastAddr.Addr := EndBlk;
{  NotOver64K := (Land(MyAddr.Dbl[1], #7400) = 0);}
  MyAddrHi := Land(MyAddr.Dbl[1], #7777); {ignore the top 4 bits}
  MyLastAddrHi := Land(MyLastAddr.Dbl[1], #7777); {ignore the top 4 bits}
  NotOver64K :=
    (
      ( MyAddrHi < MyLastAddrHi )   
      or
      (
        ( MyAddrHi = MyLastAddrHi )   
        and
        ( Abs(MyAddr.dbl[0]) <= Abs(MyLastAddr.dbl[0]) ) {unsigned numbers }
      )
    ); 
  end;



function AddrToIdx(DA : DiskAddr) : integer;
    var
        fld : integer;
    begin
    fld := AddrToField(DA);
    if fld = -1
    then AddrToIdx := 0
    else AddrToIdx := fld - StartField + 1;
    end;


Procedure ReadUnsigned(var i: Integer);
  var l: Record case boolean of
           true: (l: Long);
           false: (high, low: Integer);
           end;
  begin
  Read(l.l);
  i := l.low;
  end;

Procedure RemSpaces;
  var again: boolean;
  begin
  repeat
    if length(UsrCmdLine) > 0 then
       if UsrCmdLine[1] = ' ' then 
         begin
         Delete(UsrCmdLine, 1, 1);
         again := true;
         end
       else again := false
    else again := false;
  until not again;
  end;

Procedure GetID(var id: String);
  var i: Integer;
  begin
  AppendChar(UsrCmdLine, ' ');
  for i := 1 to length(UsrCmdLine) do
     if UsrCmdLine[i] = ' ' then
       begin
       id := SubStr(UsrCmdLine, 1, i-1);
       UsrCmdLine := SubStr(UsrCmdLine, i+1, length(UsrCmdLine)-i);
       exit(GetID);
       end;
  end;

function IdxToField(Idx : integer) : integer;
    begin
    if Idx = 0 then
      begin
      IdxToField := 0;
      exit(IdxToField);
      end;
    IdxToField := Idx + StartField - 1;
    end;


function IdxToAddr(Disk, Idx : integer) : DiskAddr;
    begin
    if Idx = 0 then
      begin
      IdxToAddr := FieldToAddr(Disk, 0);
      exit(IdxToAddr);
      end;
    IdxToAddr := FieldToAddr(Disk, Idx + StartField - 1);
    end;


{$ifc debugMax then}
procedure ProdUser(msg: string);
    begin
    WriteLn;
    WriteLn(msg,' Type <cr> to proceed.');
    ReadLn;
    end;
{$endc}


function Ask(msg, default : string) : boolean;
  var s:string;  gotanswer:boolean;
  begin
    Write(msg);
    if default <> '' then Write(' [', default, ']');
    Write('? ');
    gotanswer := false;
    repeat
      if auto and (default <> '') then 
        begin
        s := default;
        WriteLn(s);
        end
      else begin
           readln(input, s);
           if s = '' then s := default;
           end;
      if length(s) > 0 then
        gotanswer := s[1] in ['y','Y','n','N'];
      if not gotanswer then
        begin
        Write('Yes or No');
        if default <> '' then Write(' [', default, ']');
        Write('? ');
        end;
    until gotanswer;
    ask := s[1] in ['y','Y'];
    end;

procedure DoExit;
 begin
 if auto then begin
              Write('** Type CR To exit: ');
              readln;
              end;
 exit(Scavenger);
 end;

procedure PrintEntry(i : integer);
    begin
        Write(IdxToField(i):6:-10, ' ', IdxToField(Next^[i]):6:-10, ' ');
        if OnePass or (Pass = 1) then Write(IdxToField(Prev^[i]):6:-10, ' ');
        if OnePass or (Pass = 2) then Write(BlockNum^[i]:6:-10, ' ');
        if OnePass or (Pass = 3) then Write(IdxToField(SerNum^[i]):6:-10, ' ');
        if FreeBlk^[i] then Write('F') else Write(' ');
        if Confirmed^[i] then Write('C') else Write(' ');
        if SegHead^[i] then Write('S') else Write(' ');
        if ChainHead^[i] then Write('H') else Write(' ');
        WriteLn;
    end;


{$ifc debugMax then}
procedure PrintMap;
  Handler CtlC;
    begin
      CtrlCPending := False;
      Exit(PrintMap);
    end;

    var
        i : integer;
    begin
    for i := 1 to EndIdx do
        PrintEntry(i);
    end;
{$endc}


function IsValidDA (DA : DiskAddr) : boolean;
    var
        cheat : DiskCheatType;
    begin
    cheat.addr := DA;
    IsValidDA := true;
    if (DA = Zero) then exit(IsValidDA);
    if land(cheat.dbl[0],#377) <> 0 then IsValidDA := false;
    if land(cheat.dbl[1],#177400) <> TypeDisk then IsValidDA := false;
    if AddrToIdx(DA) > EndIdx then IsValidDA := false;
    if AddrToIdx(DA) < 0 then IsValidDA := false;
    end;


procedure BadBlock(Blk: integer; msg: string);
    begin
    Consistent := false;
    if DEBUG then
        begin
        Write(msg);
        PrintEntry(Blk);
        end;
    Confirmed^[Blk] := false;
    SegHead^[Blk] := false;
    end;


procedure BadChain(head: integer; msg: string);
    var
        i : integer;
    begin
    while head <> 0 do
        begin
        i := Next^[head];
        if (i = BADADDR) or (i = INCORRIGIBLE) then exit(BadChain);
        BadBlock(head, msg);
        head := i;
        end;
    end;


procedure GoodChain(head: integer);
    var
        i, j, cur : integer;
    begin
    cur := head;
    while cur <> 0 do
        begin
        i := Next^[cur];
        if (i = BADADDR) or (i = INCORRIGIBLE) then
          BadChain(head, 'Bogus chain:        ');
        Confirmed^[cur] := true;
        cur := i;
        end;
    end;


procedure ConfirmPart(head: integer; tail: integer);
    var
        i : integer;
    begin
    while (head > 0) and (head <> tail) do
        begin
        i := Next^[head];
        if (i = BADADDR) or (i = INCORRIGIBLE) then exit(ConfirmPart);
        Confirmed^[i] := true;
        head := i;
        end;
    end;

procedure FreeChain(head: integer; tail: integer);
{------------------------------------------------------------------------
  Abstract: Marks from head to tail EXCLUSIVE as free
------------------------------------------------------------------------}
    var
        i : integer;
    begin
    while (head <> 0) and (head <> tail) do
        begin
        i := Next^[head];
        if (i = BADADDR) or (i = INCORRIGIBLE) then exit(FreeChain);
        FreeBlk^[i] := true;
        SegHead^[i] := false;
        ChainHead^[i] := false;
        head := i;
        end;
    end;

procedure FreeWholeChain(head: integer; tail: integer);
{------------------------------------------------------------------------
  Abstract: Marks from head to tail INCLUSIVE as free
------------------------------------------------------------------------}
    begin
    while (head > 0) do {stop when 0, BADADDR or INCORIGIBLE}
       begin
       FreeBlk^[head] := true;
       SegHead^[head] := false;
       ChainHead^[head] := false;
       if (head = tail) then exit(FreeWholeChain);
       head := Next^[head];
       end;
    end;


function NextFree (idx: integer) : integer;
    var
        i : integer;
    begin
    for i := idx+1 to EndIdx do
        if FreeBlk^[i] then
            begin
            NextFree := i;
            exit(NextFree);
            end;
    NextFree := 0;
    end;


function PrevFree (idx: integer) : integer;
    var
        i : integer;
    begin
    for i := idx-1 downto 1 do
        if FreeBlk^[i] then
            begin
            PrevFree := i;
            exit(PrevFree);
            end;
    PrevFree := 0;
    end;


procedure CurUpdate(crs: CurPatPtr; idx: integer);
    begin
    if LstCursor <> crs then IOLoadCursor(crs,7,7);
    LstCursor := crs;
    CurY := (idx div YInc) * YMod;
    CurX := (FirstIdx-1) * XInc + XInc div 2;
    IOSetCursorPos(CurX, CurY);
    end;


procedure CurPicInit;
    label 2;

    Procedure GetCursors;
      var fid: FileID;
          blks, bits: integer;
      Handler All(a,b,c,d: integer);
          begin
          ReadCursor := DefaultCursor;
          BusyCursor := DefaultCursor;
          RasterOp(RXNor, 64, 64, 0, 0, 4, WriteCursor, 0, 0, 4, WriteCursor);
          goto 2;
          end;
      begin
      fid := FSLookUp(ScavCursors, blks, bits);
      FSBlkRead(fid, 0, RECAST(ReadCursor, pDirBlk));
      FSBlkRead(fid, 1, RECAST(WriteCursor, pDirBlk));
      FSBlkRead(fid, 2, RECAST(BusyCursor, pDirBlk));
      end;

    begin
    new(0,4,ReadCursor);
    new(0,4,WriteCursor);
    new(0,4,BusyCursor);
    GetCursors;
2:  end;


procedure CurInit;
    var
        i : integer;
    label 1;
    begin
    XInc := CURMAXX div Interlace;
    for i := 1 to 30 do
        begin
        YMod := (CURMAXY * i) div EndIdx;
        if YMod > 0 then
            begin
            YInc := i;
            goto 1;
            end;
        end;
1:  IOCursorMode(IndepCursor);
    CurY := 0;
    IOLoadCursor(ReadCursor,7,7);
    LstCursor := ReadCursor;
    end;


(**)Procedure FixPairTable;
(**)  {-------------------------------------------------------
(**)   Abstract: Changes the Pair table from cyl, head to first, last logical
(**)             addr.
      { NB not amended for eio!!!!
(**)  -------------------------------------------------------}
(**)   var cheat: DiskCheatType;
(**)       i: integer;
(**)       dskAddr : Packed Record case boolean of
(**)                   true: (i: integer);
(**)                   false: (sec: 0..29;
(**)                           head: 0..7;
(**)                           cyl: 0..201);
(**)                   end;
(**)   begin
(**)   cheat.dbl[1] := 0;
(**)   for i := 1 to ignorePairs do
(**)     begin
          if EIOFlag Or (CIOdisktype = CIOmicropolis) then
           begin
            cheat.dbl[0] := shift(badpairs[i].head,8);
            cheat.dbl[1] := badpairs[i].cyl;
           end
          else
           begin
(**)        dskAddr.head := BadPairs[i].head;
(**)        dskAddr.cyl := BadPairs[i].cyl;
(**)        dskAddr.sec := 0;
(**)        cheat.dbl[0] := dskAddr.i;
           end;
(**)      BadPairs[i].firstBad:=AddrToField(PhysAddrToLogAddr(disk,cheat.lng));
          if (configuration = ciomicrop) 
             or
             (EIOFlag and (configuration = Winch24)) then
           begin 
            cheat.dbl[0] := cheat.dbl[0]+23;
           end
          else
           if Configuration = Generic5inch then 
            begin
             cheat.dbl[0] := cheat.dbl[0] + 15;
            end
           else
            begin
(**)         dskAddr.sec := 29;
(**)         cheat.dbl[0] := dskAddr.i;
            end;
(**)      BadPairs[i].lastBad:=AddrToField(PhysAddrToLogAddr(disk,cheat.lng));
(**)     end;
(**)   end;


Procedure PrintFailBlock(addr: DiskAddr);
  {-------------------------------------------------------
   Abstract: Prints useful information for Disk read failure.
   Parameters: addr - address operating on when error occured; it is
                  decomposed into Device, head, cylinder and sector
               softStat - softstatus of error.
  -------------------------------------------------------}
   var cheat: DiskCheatType;
       dskAddr : Packed Record case boolean of
                   true: (i: integer);
                   false: (sec: 0..29;
                           head: 0..7;
                           cyl: 0..201);
                   end;
    begin
    Write('** Failed to read block ',AddrToField(addr):1:-10);
    cheat.lng := LogAddrToPhysAddr(addr);
    if disk = 0 then
      if EIOFlag or (ciodisktype = ciomicropolis) then { also for 5 inch }
            WriteLn(' (HardDisk;  Cyl ', Cheat.dbl[1]:1,
                     ', head ', shift( Cheat.dbl[0], -8 ):1,
                     ', sector ', land( Cheat.dbl[0], 255 ):1 )
      else begin
           dskAddr.i := cheat.dbl[0];
           WriteLn(' (HardDisk;  Cyl ', dskAddr.cyl:1,', head ',
                          dskAddr.head:1,', sector ',dskAddr.sec:1,')');
           end
    else WriteLn(' (Floppy; Sector ',cheat.dbl[0]:1,', cyl ',
                    cheat.dbl[1]:1,')');
    end;

function DoDiskIO(addr : DiskAddr; ptr : ptrDiskBuffer;
                        hptr : ptrHeader; dskcommand : DiskCommand): Boolean;

  handler VBNOutofRange( VID : VolId; VBN : VolBlockNumber );
    begin
    PrintFailBlock(addr);
    WriteLn('** because it has an illegal address.');
    if Pass <= 1 then WriteLn(' Marked as Incorrigible.');
    DoDiskIO := false;
    EvilCnt := EvilCnt + 1;
    Consistent := false;
    exit(DoDiskIO);
    end;  { catch exception caused by rounding of addrs > max addr }

    var
      ok : boolean;
      badhptr : ptrHeader;
      tries: integer;

    begin { DoDiskIO }
    if Pass > 1 then tries := NUMTRIES
    else tries := TryCount;
    DoDiskIO := True;
    if dskcommand = DskRead then CurUpdate(ReadCursor,AddrToIdx(addr))
        else CurUpdate(WriteCursor,AddrToIdx(addr));
    ok := TryDiskIO(addr, ptr, hptr, dskcommand, tries);
    if ok then exit(DoDiskIO);
    PrintFailBlock(addr);
    if CanChange then
      if CanReWrite and (Pass <= 1) then
        begin
        new(badhptr);
        badhptr^.SerialNum := DoubleInt(INCORRIGIBLE);
        badhptr^.Filler := 0;
        badhptr^.LogBlock := 0;
        badhptr^.NextAdr := Zero;
        badhptr^.PrevAdr := Zero;
        ok := TryDiskIO(addr, ptr, badhptr, dskFirstWrite, 1);
        dispose(badhptr);
        if ok then
          begin
          ok := TryDiskIO(addr, ptr, hptr, dskcommand, 1);
          if ok then
            begin
            WriteLn(' ReWritten!! Now marked as Bad.');
            exit(DoDiskIO);
            end;
          end;
        end;
    if Pass <= 1 then WriteLn(' Marked as Incorrigible.');
    DoDiskIO := false;
    EvilCnt := EvilCnt + 1;
    Consistent := false;
    end { DoDiskIO };


procedure WriteName(Blk : DiskAddr);
    var
      PartIdx : integer;
      DiskIdx : integer;
    begin
      PartIdx := WhichPartition(Blk);
      if PartIdx = 0 then
        begin
        Write('bogus!');
        exit(WriteName);
        end;
      DiskIdx := PartTable[PartIdx].PartDevice;
      Write(DiskTable[DiskIdx].RootPartition, ':',
            PartTable[PartIdx].PartName, '>');
      DiskIO(Blk, Buffer2, Label2, DskRead);
      if (Label2^.LogBlock <> -1) or (Label2^.SerialNum <> Blk)
        then Write('bogus!')
        else Write(Buffer2^.FSData.FileName);
    end;

function CheckFile(Blk : DiskAddr): boolean;
    var
      PartIdx : integer;
      DiskIdx : integer;
    begin
      CheckFile := false;
      PartIdx := WhichPartition(Blk);
      if PartIdx = 0 then exit(CheckFile);
      DiskIO(Blk, Buffer2, Label2, DskRead);
      if (Label2^.LogBlock <> -1) or (Label2^.SerialNum <> Blk)
        then exit(CheckFile)
        else CheckFile := true;
    end;


procedure BlinkScreen;
  const BlinkWait = 10000;
  var invFunct, k: Integer;
  begin
  invFunct := LXOr(DefCursFunct, 1);
  IOSetFunction(RECAST(invFunct, CursFunction));
  for k := 1 to BlinkWait do;
  IOSetFunction(RECAST(DefCursFunct, CursFunction));
  end;

Procedure SetUpBadBlocks;
Type DiskBlock = Packed Array[0..255] of Integer;
     pDiskBlock = ^DiskBlock;
Var pBuffer : pDiskBlock;
    pHeader : IOHeadPtr;
    pStatus : IOStatPtr;
    I : Integer;
    PhyAddr : DiskCheatType;
    LogAdr : Double;
    BadLogAdr : FSBit32;
    Valid : boolean;

handler VBNOutofRange( VID : VolId; VBN : VolBlockNumber );
  begin
{This code is probably never executed - PhysAddrtoLogAddr doesnt raise an exception}
  Valid := false;
  end;
    
    Begin
    New(0, 256, pBuffer);
    New(0, 2, pStatus);
    New(0, 8, pHeader);
    
    LogAdr[1] := 0;                             { cylinder 0 }
    LogAdr[0] := Lor(Shift(1, 8), 8);           { head 1, sector 8}
    While True Do
        begin
        UnitIO( EIODisk,
                RECAST(pBuffer, IOBufPtr),
                IODiagRead,
                512,
                LogAdr,
                pHeader,
                pStatus );
        I := 0;
        While I <= 254 Do
            Begin
            If pBuffer^[I] = -1 Then Exit(SetupBadBlocks);
            If NumBadBlocks >= MAXBADBLOCKS Then 
                Begin
                Writeln('** Too many bad blocks in the bad block map.');
                Writeln('** The map probably needs to be re-written.');
                Writeln('** Contact your Field Service representative for aid');
                Exit(scavenger);
                End;
                
            Cheat.Dbl[0] := pBuffer^[I];                { Head/sector }
            Cheat.Dbl[1] := pBuffer^[I+1];              { cylinder number }
            If Abs(Cheat.Dbl[1]) <= LastCyl then
              begin
              Valid := true; {set false if get VBNOutofRange}
              BadLogAdr := PhysAddrToLogAddr(0, cheat.addr);
              if Valid then
                begin
                NumBadBlocks := NumBadBlocks + 1;
                BadBlocks[NumBadBlocks] := AddrToField(BadLogAdr);
                end;
              end;

            I := I + 2;
            End;
        LogAdr[0] := LogAdr[0] + 1;
        If LAND(LogAdr[0], 16) = 0 Then
            Begin
            Writeln('** Bad block map overflows cyl 0, head 1.');
            Writeln('** The map probably needs to be re-written.');
            Writeln('** Contact your Field Service representative for help.');
            Exit(scavenger);
            End;
        End;
    Dispose(pBuffer);
    Dispose(pStatus);
    Dispose(pHeader);
    End;

Function MyAddrToField(inA: DiskAddr): Integer;
  var i: Integer;
  begin
  i := AddrToField(inA);
  if i = -1 then MyAddrToField := 0
  else MyAddrToField := i;
  end;

procedure PrintDIB(DIBDA : DiskAddr);
    var
      rootdib : boolean;
      i       : integer;
      name    : string[8];
      cheat   : DiskCheatType;
      Blk1    : FSBit32;
      Blk2    : FSBit32;
      BlkValid : boolean;
      Blk1Valid : boolean;
      Blk2Valid : boolean;
      Deleted : boolean;

handler VBNOutofRange( vid: VolID; vbn : VolBlockNumber );
  begin
{This code is probably never executed - PhysAddrtoLogAddr doesnt raise an exception}
  {should only happen for boots beyond 31MB}
  BlkValid := false;
  end;
    
    begin
    DiskIO(DIBDA, BufferPtr, LabelPtr, DskRead);
    name := '        ';
    for i := 1 to 8 do name[i] := BufferPtr^.PartName[i];
    rootdib := BufferPtr^.PartKind = Root;
    if rootdib then
      begin
      WriteLn('Dev Info Block Summary:');
      WriteLn('Dev name = ',name);
      WriteLn('Dev Type = ',ord(BufferPtr^.PartDevice));
      WriteLn('Partitions:');
      For i := 0 to 63 do
        if (PartDAs[i] <> Zero) then
          begin
          Write(i,': DA = ',MyAddrToField(PartDAs[i]):6:-10);
          WriteLn(', Name = ',PartNames[i]);
          end;
      WriteLn;
      WriteLn('Boots:');
      for i := 0 to 25 do
        if (BufferPtr^.BootTable[i] <> Zero)
          or (BufferPtr^.InterpTable[i] <> Zero) then
            begin
            if CylinRange(BufferPtr^.BootTable[i]) then
              begin
              BlkValid := true; {may be set false if get VBNOutofRange}
              Blk1 := PhysAddrToLogAddr(Disk, BufferPtr^.BootTable[i]);
              Blk1Valid := BlkValid;
              end
            else Blk1Valid := false;

            if CylinRange(BufferPtr^.InterpTable[i]) then
              begin
              BlkValid := true; {may be set false if get VBNOutofRange}
              Blk2 := PhysAddrToLogAddr(Disk, BufferPtr^.InterpTable[i]);
              Blk2Valid := BlkValid;
              end
            else Blk2Valid := false;

            Deleted := false;
            if (Blk1Valid) and (Blk2Valid) then
              if (not (CheckFile(blk1) and CheckFile(blk2))) and CanChange
                then begin
                     WriteLn('** Deleting boot ',chr(i + #141));
                     BufferPtr^.BootTable[i] := Zero;
                     BufferPtr^.InterpTable[i] := Zero;
                     DiskIO(DIBDA, BufferPtr, LabelPtr, DskWrite);
                     Deleted := true;
                     end;
            if not deleted then
                 begin
                 Write(chr(i + #141),': System = ');
                 if DebugMax then
                     begin
                     cheat.addr := BufferPtr^.BootTable[i];
                     Write(cheat.dbl[0]:6:-10,',',cheat.dbl[1]:6:-10,' = ');
                     end;
                 if Blk1Valid then 
                   WriteName(Blk1)
                 else write('* Disk address beyond 31MB - cant access');
                 Write(', Interpreter = ');
                 if DebugMax then
                     begin
                     cheat.addr := BufferPtr^.InterpTable[i];
                     Write(cheat.dbl[0]:6:-10,',',cheat.dbl[1]:6:-10,' = ');
                     end;
                 if Blk2Valid then 
                   WriteName(Blk2)
                 else write('* Disk address beyond 31MB - cant access');
                 WriteLn;
                 end;
            end;
      end {root}
    else
      begin
      WriteLn('Partition Info Block Summary for ', name);
      WriteLn('Start = ', MyAddrToField(BufferPtr^.PartStart):1:-10,
              '. End = ', MyAddrToField(BufferPtr^.PartEnd):1:-10,
              '. Device DA = ', MyAddrToField(BufferPtr^.PartRoot):1:-10,
              '. Device Type = ', ord(BufferPtr^.PartDevice):1:-10);
      cheat.lng := BufferPtr^.NumFree;
      WriteLn('Free: head = ', MyAddrToField(BufferPtr^.FreeHead):1:-10,
              '. tail = ', MyAddrToField(BufferPtr^.FreeTail):1:-10,
              '. count = ', cheat.dbl[1]:1:-10, ', ', cheat.dbl[0]:1:-10);
      WriteLn('Root directory = ', MyAddrToField(BufferPtr^.RootDirID):1:-10,
              '. Bad segment = ', MyAddrToField(BufferPtr^.BadSegID):1:-10);
      end;
    end;


Procedure ShowFail(block: integer; when: string);
    begin
    Writeln;
    Writeln('**  Block ', block:1:-10, ' was found to be incorrigible during');
    Writeln('**  ',when,', but was thought to be good before.');
    Writeln('**  Suggest you re-run the Scavenger and specify this block to ignore.');
    Writeln;
    Writeln('**  Scavenger aborted.');
    Writeln;
    DoExit;
    end;

Procedure ShowPartFail(s: String);
    begin
    Writeln;
    Writeln('** Could not ',s,' the Partition Info Block!');
    Writeln;
    Writeln('** Scavenger aborted.');
    Writeln;
    DoExit;
    end;

{************************* AnalyzeDisk ***********************************
{
{  AnalyzeDisk creates a map of all disk pages in memory, and checks for
{       well-formed segments.  If there is not enough memory for the
{       complete map, it skips the checking of logical block and serial
{       number consistency.  The map consists of:
{         Next array - ptr to the next block of the segment or INCORRIGIBLE
{            or BADADDR (if invalid disk address).
{         Prev array - ptr to previous block of segment or BADADDR.
{         BlockNum (if enough memory) - logical block numbers.
{         SerNum (if enough memory) - serial numbers.
{         ChainHead - a bit map indicating where the heads of chains are.
{         SegHead - a bit map indicating where segment headers are.
{         Confirmed - a bit map indicating which blocks are GOOD.
{         Free - a bit map indicating which blocks are FREE.
{       After AnalyzeDisk, Next, Free, and SegHead are available for other
{       parts of the scavenger.
{}
Procedure AnalyzeDisk;
    var
        i, j, k, NextIdx, PrevIdx, LBN, SN : integer;
        Blk : FSBit32;
        FoundSegHead, done, isToBeFreed : boolean;
        cheat : DiskCheatType;
    Label 121, 131, 132, 141, 211, 221, 231;

    begin
    Pass := 1;

{ PASS 1: Sweep disk building Next, BlockNum, SerNum, ChainHead, SegHead, Free,
{       and Confirmed.  At the end of Pass 1, all well-formed chains should
{       be identified. }


{ PASS 1, Step 1: Zero map }
    for i := 1 to EndIdx do
      begin
      Next^[i] := 0;
      Prev^[i] := 0;
      if OnePass and CheckBlockNum then BlockNum^[i] := 0;
      if OnePass and CheckSerialNum then SerNum^[i] := 0;
      Confirmed^[i] := false;
      ChainHead^[i] := false;
      SegHead^[i] := false;
      FreeBlk^[i] := false;
      TempHead^[i] := false;
      end;

    NumBadBlocks := 0;
    If Configuration = Generic5Inch Then SetupBadBlocks;
{ PASS 1, Step 2: Sweep disk building map }
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to sweep disk:');
{$endc}
    ChangeTitle(Concat(ScavTitle, '   Read device.'));
    FirstIdx := 1;
    repeat
      i := FirstIdx;
      while i <= EndIdx do
        with LabelPtr^ do
          begin
          Blk := IdxToAddr(Disk,i);

(**)       temp := AddrToField(Blk);
(**)       for ignoreCheck := 1 to ignorePairs do
(**)         if (temp >= BadPairs[ignoreCheck].firstBad) and
                (temp <= BadPairs[ignoreCheck].lastBad) then
(**)               begin
(**)               WriteLn('** Skipping ',AddrToField(blk):1:-10, ' from Range');
(**)               Next^[i] := INCORRIGIBLE;
(**)               Prev^[i] := INCORRIGIBLE;
(**)               evilCnt := evilCnt+1;
(**)               goto 121;
(**)               end;

(**)      for ignoreCheck := 1 to ignoreBlocks do
(**)         if temp = BadBlks[ignoreCheck] then
(**)            begin
(**)            WriteLn('** Skipping ',AddrToField(blk):1:-10);
(**)            Next^[i] := INCORRIGIBLE;
(**)            Prev^[i] := INCORRIGIBLE;
(**)            evilCnt := evilCnt+1;
(**)            goto 121;
(**)            end;

          If Configuration = Generic5Inch Then 
             for ignorecheck := 1 to NumBadBlocks do
                if temp = badblocks[ignorecheck] then
                   begin
                   Writeln('** Skipping bad block ', AddrToField(blk):1:-10);
                   Next^[i] := INCORRIGIBLE;
                   Prev^[i] := INCORRIGIBLE;
                   evilcnt := evilcnt + 1;
                   goto 121;
                   end;
                
          if DoDiskIO(Blk, BufferPtr, LabelPtr, DskRead) then
            begin
            if (SerialNum = Zero) then FreeBlk^[i] := true
              else
                begin
                if (LogBlock = -1) and (SerialNum = Blk) then
                    begin
                    SegHead^[i] := true;
                    if bufferPtr^.segKind <> Permanent  { if a temp seg }
                    then if DeleteTemps
                         then tempHead^[i] := true   { delete it }
                         else tempCnt := tempCnt+1;  {just count them}
                    end;
                if (PrevAdr = Zero) then ChainHead^[i] := true;
                end;

            if IsValidDA(NextAdr)
              then Next^[i] := AddrToIdx(NextAdr)
              else Next^[i] := BADADDR;

            if IsValidDA(PrevAdr)
              then Prev^[i] := AddrToIdx(PrevAdr)
              else Prev^[i] := BADADDR;
            if OnePass and CheckBlockNum then
              BlockNum^[i] := LogBlock;
            if OnePass and CheckSerialNum then
              SerNum^[i] := AddrToIdx(SerialNum)
            end
          else
            begin
            Next^[i] := INCORRIGIBLE;
            Prev^[i] := INCORRIGIBLE;
            end;
  121:    i := i + Interlace;
          end;
      FirstIdx := FirstIdx + 1;
    until FirstIdx > Interlace;

    {$ifc DEBUGMAX then} PrintMap; {$endc}

    FirstIdx := Interlace; { For cursor }

{ PASS 1, Step 3: Check for loops through the Next Pointer using the
 Confirmed bit map;  if find a loop; break it by setting the next ptr
 to BADADDR; assumes Confirmed array all false; resets it to all false 
 at end of step }
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to look for loops:');
{$endc}
    ChangeTitle(Concat(ScavTitle, '   Checking for loops.'));
    for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if not Confirmed^[i] then
            begin
            j := i;
            done := false;
            repeat
              Confirmed^[j] := true;
              if (Next^[j] = 0) or (Next^[j] = BADADDR) or
                 (Next^[j] = INCORRIGIBLE) then done := true
              else if Confirmed^[Next^[j]] then { been there before }
                         begin
                         done := true;
                         if Next^[j] = i then begin
                                              Next^[j] := BADADDR;
                                              BlinkScreen;
                                              end
                         else if Prev^[Next^[j]] = j then {ok}
                              else begin
                                   Next^[j] := BADADDR;
                                   BlinkScreen;
                                   end;
                         end
                   else j := Next^[j];
            until done;
            end; {not confirmed}
       end; {loop}

  for i := 1 to EndIdx do
       Confirmed^[i] := false;

{ PASS 1, Step 4: Check for loops through the Prev pointer using the
  Confirmed bit map;  if find a loop; break it by setting the Prev ptr
  to BADADDR; assumes Confirmed array all false; resets it to all
  false at end of step }
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to look for Prev loops:');
{$endc}
    ChangeTitle(Concat(ScavTitle, '   Checking for Prev loops.'));
    for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if not Confirmed^[i] then
            begin
            j := i;
            done := false;
            repeat
              Confirmed^[j] := true;
              if (Prev^[j] = 0) or (Prev^[j] = BADADDR) or
                 (Prev^[j] = INCORRIGIBLE) then done := true
              else if Confirmed^[Prev^[j]] then { been there before }
                         begin
                         done := true;
                         if Prev^[j] = i then begin
                                              Prev^[j] := BADADDR;
                                              BlinkScreen;
                                              end
                         else if Next^[Prev^[j]] = j then {ok}
                              else begin
                                   Prev^[j] := BADADDR;
                                   BlinkScreen;
                                   end;
                         end
                   else j := Prev^[j];
            until done;
            end; {not confirmed}
       end; {loop}

  for i := 1 to EndIdx do
       Confirmed^[i] := false;


{ PASS 1, Step 5: Sweep map checking for wfc's. }

{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to check for well-formed chains:');
{$endc}
{This loop turns on the confirmed bit for all wfc.  Now it also frees any
 well formed chains that are not permanent segments}

    ChangeTitle(Concat(ScavTitle, '   Checking for well-formed chains.'));
    for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if ChainHead^[i] then
            begin
            FoundSegHead := false;
            isToBeFreed := false;
            j := i;
            while true do
                begin
                if (Next^[j] = INCORRIGIBLE) or (Next^[j] = BADADDR)
                  then goto 132;
                if SegHead^[j] then begin
                                    FoundSegHead := true;
                                    isToBeFreed := tempHead^[j];
                                    end;
                if Next^[j] = 0 then goto 131;
                if Prev^[Next^[j]] <> j then goto 132;
                j := Next^[j];
                end;
        131:
            if FoundSegHead then
                 begin
                 GoodChain(i);
                 if isToBeFreed then begin
                                     tempCnt := tempCnt+1;
                                     FreeWholeChain(i,j);
                                     end;
                 end;
        132:
            end;
        end;

    {$ifc DEBUGMAX then} PrintMap; {$endc}


{ PASS 1, Step 6: Mark all blocks in free chains. }
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, Mark all blocks in free chains:');
{$endc}
    ChangeTitle(Concat(ScavTitle, '   Looking for free chains.'));
    FirstIdx := Interlace;
    for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if (Next^[i] = INCORRIGIBLE) then FreeBlk^[i] := false;
        if Next^[i] <> BADADDR then
          if FreeBlk^[i] and not (FreeBlk^[Next^[i]]) then
            begin
            j := Next^[i];
            while j <> 0 do
                begin
                if (Next^[j] = BADADDR) or (Next^[j] = INCORRIGIBLE) then
                    goto 141;
                if FreeBlk^[j] then
                    begin
                    FreeChain(i,j);
                    goto 141;
                    end;
                j := Next^[j];
                end;
        141:
            end;
        end;


{ PASS 1, Step 7: Sweep map checking for additional Free blocks. }
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to check FREE blocks:');
{$endc}
    ChangeTitle(Concat(ScavTitle, '   Looking for more free blocks.'));
    FirstIdx := Interlace;
    for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if FreeBlk^[i] and not Confirmed^[i] then
           begin
           {search for beginning of list}
           j := i;
           while FreeBlk^[j] and (prev^[j] > 0)  do
              if prev^[prev^[j]] < 0 then prev^[j] := 0 {prev is bad or incor}
              else j := Prev^[j];
           {search for end}
           k := i;
           while FreeBlk^[k] and (next^[k] > 0) do
              if next^[next^[k]] < 0 then next^[k] := 0 {next is bad or incor}
              else k := Next^[k];
           ConfirmPart(j,k);
           if Prev^[j] <> 0 then
              begin
              Next^[prev^[j]] := BadAddr;  {found part of a file}
              prev^[j] := 0;
              BlinkScreen;
              end;
           if Next^[k] <> 0 then
              begin
              Prev^[Next^[k]] := BadAddr;  {found part of a file}
              next^[k] := 0;
              BlinkScreen;
              end;
           end;
        end;


{ PASS 1, Step 8: Verify Next and Prev links so that bad blocks can be
                  linked together in some semblence of the original order. }
    if CanChange then
      begin
      ChangeTitle(Concat(ScavTitle,
                        '   Verifying next and previous links.'));
      FirstIdx := Interlace;
      for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor,i);
        NextIdx := Next^[i];
        if (NextIdx <> 0) and
           (NextIdx <> BADADDR) and (NextIdx <> INCORRIGIBLE) then
            if Prev^[NextIdx] <> i then Next^[i] := BADADDR;
        PrevIdx := Prev^[i];
        if (PrevIdx <> 0) and
           (PrevIdx <> BADADDR) and (PrevIdx <> INCORRIGIBLE) then
            if Next^[PrevIdx] <> i then Prev^[i] := BADADDR;
        { mark heads of chains of bad pages }
        if Prev^[i] = BADADDR then ChainHead^[i] := True
        end;
      end;

    {$ifc DEBUGMAX then} PrintMap; {$endc}


    Pass := 2;

{ PASS 2, Step 1: Sweep disk build block number map. }
    if not OnePass and CheckBlockNum then
      begin
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to sweep disk building more map.');
{$endc}
        ChangeTitle(Concat(ScavTitle, '   Read device (2nd pass).'));
        BlockNum := MakePtr(SegPrev, 0, ptrIntArray);
        FirstIdx := 1;
        repeat
          i := FirstIdx;
          while i <= EndIdx do
            with LabelPtr^ do
              begin
              Blk := IdxToAddr(Disk, i);
              if Next^[i] <> INCORRIGIBLE then
                if DoDiskIO(Blk, BufferPtr, LabelPtr, DskRead) then
                  BlockNum^[i] := LogBlock
                else ShowFail(AddrToField(blk), 'Pass 2');
              i := i + Interlace;
              end;
          FirstIdx := FirstIdx + 1;
        until FirstIdx > Interlace;
      end;


{ PASS 2, Step 2: Sweep map checking for logical block consistency. }
    if CheckBlockNum then
      begin
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to check for logical block consistency:');
{$endc}
      ChangeTitle(Concat(ScavTitle,
                         '   Checking for logical block consistency.'));
      FirstIdx := Interlace;
      for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if ChainHead^[i] and Confirmed^[i] then
            begin
            LBN := BlockNum^[i];
            j := Next^[i];
            while j <> 0 do
                begin
                if LBN >= BlockNum^[j] then
                    begin { Inconsistancy found }
                    BlinkScreen;
                    BadChain(i, 'Bad block number:  ');
                    goto 211;
                    end;
                LBN := BlockNum^[j];
                j := Next^[j];
                end;
        211:
           end;
        end {for};
    {$ifc DEBUGMAX then} PrintMap; {$endc}
      end {if};


{ PASS 3, Step 1: Sweep disk serial number map. }
    if not OnePass and CheckSerialNum then
      begin
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to sweep disk building more map.');
{$endc}
        ChangeTitle(Concat(ScavTitle, '   Read device (3rd pass).'));
        Pass := 3;
        SerNum := MakePtr(SegPrev, 0, ptrIntArray);
        FirstIdx := 1;
        repeat
          i := FirstIdx;
          while i <= EndIdx do
            with LabelPtr^ do
              begin
              Blk := IdxToAddr(Disk, i);
              if Next^[i] <> INCORRIGIBLE then
                if DoDiskIO(Blk, BufferPtr, LabelPtr, DskRead) then
                  SerNum^[i] := AddrToIdx(SerialNum)
                else ShowFail(AddrToField(blk), 'Pass 3');
              i := i + Interlace;
              end;
          FirstIdx := FirstIdx + 1;
        until FirstIdx > Interlace;
      end;


{ PASS 3, Step 2: Sweep map checking for serial number consistency. }
    if CheckSerialNum then
      begin
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to check for serial number consistency:');
{$endc}
      ChangeTitle(Concat(ScavTitle,
                         '   Checking for serial number consistency.'));
      FirstIdx := Interlace;
      for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if ChainHead^[i] and Confirmed^[i] then
            begin
            SN := SerNum^[i];
            j := Next^[i];
            while j <> 0 do
                begin
                if SerNum^[j] <> SN then
                    begin { Inconsistancy found }
                    BlinkScreen;
                    BadChain(i, 'Bad serial number:  ');
                    goto 221;
                    end;
                j := Next^[j];
                end;
        221:
            end;
        end;
    {$ifc DEBUGMAX then} PrintMap; {$endc}
      end;


{ PASS 4, Step 1: Sweep map checking for any bad blocks. }
    Pass := 4;
    if Consistent then
      begin
{$ifc DEBUGMAX then}
 ProdUser('AnalyzeDisk, about to check for any bad blocks:');
{$endc}
      ChangeTitle(Concat(ScavTitle, '   Checking for bad blocks.'));
      FirstIdx := Interlace;
      for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor, i);
        if not Confirmed^[i] and not FreeBlk^[i] then
          begin
          Consistent := false;
          goto 231;
          end;
        end;
    231:
      end;



{ PASS 4, Step 2: link together all the bad chains. }
    if CanChange and not Consistent then
      begin
      ChangeTitle(Concat(ScavTitle,
                         '   Linking bad chains together.'));
      FirstIdx := Interlace;
      for i := 1 to EndIdx do
        begin
        CurUpdate(BusyCursor,i);
        if not Confirmed^[i] and not FreeBlk^[i] and
           (Next^[i] <> INCORRIGIBLE) and ChainHead^[i] then { bad chain head }
          begin
          BadPages := BadPages + 1;
          if BadHead = 0 then BadHead := i;
          if BadTail <> 0 then Next^[BadTail] := i;
          BadTail := i;
          while (Next^[BadTail] <> 0) and (Next^[BadTail] <> BADADDR) do
            begin
            BadPages := BadPages + 1;
            if FreeBlk^[BadTail] then
               WriteLn(chr(7),'* BUG: block ',BadTail,' on bad but free');
            BadTail := Next^[BadTail];
            end;
          end;
        end;
      end;
    end {AnalyzeDisk};


{***************************** BuildFreeList ****************************
{
{ Using the Next array built in AnalyzeDisk, BuildFreeList reconstructs the
{       disk free page list and the disk information block.
{       It also constructs a permanent segment out of all bad pages, and
{       puts a pointer to that segment into the disk information block.
{}
Procedure BuildFreeList;
    var
        i, j: Integer;
        FreeHead, FreeTail: Integer;
        BadBlkNum, RootIdx : integer;
        Blk, PrevBlk : FSBit32;
        NextIdx, PrevIdx: Integer;
    begin
{$ifc DEBUGMAX then}
 ProdUser('BuildFreeList, about to start:');
{$endc}
    for i := 0 to DISKBUFSIZE-1 do ZeroBufPtr^.IntData[i] := 0;

    { build the free list }

    ChangeTitle(Concat(ScavTitle, '   Building the free list.'));
    FreeCnt := Zero;
    FreeHead := NextFree(0);
    FreeTail := PrevFree(EndIdx+1);
    FirstIdx := 1;
    repeat
      i := FirstIdx;
      while i <= EndIdx do
        with LabelPtr^ do
          begin
          if FreeBlk^[i] then
              begin
              Blk := IdxToAddr(Disk,i);
              FreeCnt := DoubleAdd(FreeCnt, One);
              SerialNum := Zero;
              LogBlock := 0;
              NextIdx := NextFree(i);
              PrevIdx := PrevFree(i);
              Filler := IdxToField(NextIdx);
              if PrevIdx = 0 then PrevAdr := Zero
              else PrevAdr := IdxToAddr(Disk, PrevIdx);
              if NextIdx = 0 then NextAdr := Zero
              else NextAdr := IdxToAddr(Disk, NextIdx);
              if Filler = 0 then Filler := IdxToField(FreeHead);
              if not DryRun then
                if not DoDiskIO(Blk, ZeroBufPtr, LabelPtr, DskFirstWrite) then
                  ShowFail(AddrToField(blk), 'building the free list');
              end;
          i := i + Interlace
          end;
      FirstIdx := FirstIdx + 1;
    until FirstIdx > Interlace;

    { build the bad segment }

    if Consistent or (BadHead = 0) then BadHeadAddr := Zero
    else
      begin
{$ifc DEBUGMAX then}
 ProdUser('BuildFreeList, about to build bad segment:');
{$endc}
      ChangeTitle(Concat(ScavTitle, '   Building the bad segment.'));
      FirstIdx := Interlace;
      i := BadHead;
      BadHeadAddr := IdxToAddr(Disk,BadHead);
      BadBlkNum := -1;
      PrevBlk := Zero;
      while i <> 0 do
        with LabelPtr^ do
          begin
          Consistent := false;
          Blk := IdxToAddr(Disk,i);
          if not DoDiskIO(Blk, BufferPtr, LabelPtr, DskRead) then
                  ShowFail(AddrToField(blk), 'building the bad segment');
          if SegHead^[i] then
             WriteLn('* Part of file "',bufferPtr^.fsdata.fileName,
                     '" going into bad segment');
          if FreeBlk^[i] then
               WriteLn(chr(7),'* BUG: block ',i:1,' on bad but free');
          Filler := 0;
          PrevAdr := PrevBlk;
          if (Next^[i] = 0) or (Next^[i] = BADADDR) then NextAdr := Zero
          else NextAdr := IdxToAddr(Disk, Next^[i]);
          if i = BadHead then
            with BufferPtr^ do
              begin
              for j := 0 to DISKBUFSIZE-1 do IntData[j] := 0;
              with FSData do
                begin
                FileBlocks := BadPages - 1;
                FileBits := 4096;
                FileName := 'BadFile!';
                end;
              SegKind := Bad;
              LastBlk := BadPages-2;
              LastAddr := IdxToAddr(Disk,BadTail);
              LastNegBlk := -1;
              LastNegAddr := BadHeadAddr;
              NumBlksInUse := BadPages;
              end
          else if chainHead^[i] then {mark first byte of block}
                bufferPtr^.byteData[0] := #177; {DEL}
          SerialNum := BadHeadAddr;
          LogBlock := BadBlkNum;
          BadBlkNum := BadBlkNum + 1;
          if not DryRun then
            if not DoDiskIO(Blk, BufferPtr, LabelPtr, DskFirstWrite) then
                  ShowFail(AddrToField(blk), 'building the bad segment');
          PrevBlk := Blk;
          i := Next^[i];
          if i = BadAddr then i := 0;
          end;
      { the following ensures that RebuildDirectory will make a file }
      { out of the bad segment }
      { Confirmed^[BadHead] := True;
      { SegHead^[BadHead] := True;
      { ChainHead^[BadHead] := True;
      {}
      end;


{ Finally, write out new partition information block. }
    ChangeTitle(Concat(ScavTitle,
                       '   Writing the new Partition information block.'));
    if not DoDiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead) then
      ShowPartFail('read');

    RootIdx := AddrToIdx(BufferPtr^.RootDirID);
    if (Next^[RootIdx] = BADADDR) or (Next^[RootIdx] = INCORRIGIBLE)
       or not Confirmed^[RootIdx] or not SegHead^[RootIdx]
       or FreeBlk^[RootIdx] then
            begin
            BufferPtr^.RootDirID := Zero;
            WriteLn;
            WriteLn('Lost the root directory, suggest rebuilding it!');
            consistent := false;
            end;

    BufferPtr^.FreeHead := IdxToAddr(Disk,FreeHead);
    BufferPtr^.FreeTail := IdxToAddr(Disk,FreeTail);
    BufferPtr^.NumFree := FreeCnt;
    BufferPtr^.BadSegID := BadHeadAddr;

    LabelPtr^.SerialNum := InfoBlk;
    LabelPtr^.LogBlock := 0;
    LabelPtr^.Filler := 0;
    LabelPtr^.PrevAdr := Zero;
    LabelPtr^.NextAdr := Zero;

    if not DryRun then
      if not DoDiskIO(InfoBlk, BufferPtr, LabelPtr, DskFirstWrite) then
        ShowPartFail('write');
    end;

Function UpperEqual(n1, n2: PathName): boolean;
   begin
   ConvUpper(n1);
   ConvUpper(n2);
   UpperEqual := n1=n2;
   end;



{**************************** CheckDirectory *******************************}

    {$INCLUDE DirScavenge}

{**************************** Scavenger ************************************}
    begin
    ScavTitle := Concat('Scavenger ', ScavVersion);
    ChangeTitle(ScavTitle);

    If SwappingAllowed then
        begin
        SavedSwapID := SwapID;
        ShouldReEnableSwapping := true;
        DisableSwapping;
        end;

    new(0,256,BufferPtr);
    new(0,4,LabelPtr);
    new(0,256,ZeroBufPtr);
    new(0,256,Buffer2);
    new(0,4,Label2);
    CURMAXX := SBitWidth;
    InitDiskIO;

    WordsPerBlock := DoubleInt(DISKBUFSIZE);
    Zero := DoubleInt(0);
    One  := DoubleInt(1);
    Pass := 0;

    WriteLn;
    WriteLn(ScavTitle);
    WriteLn;
    
    RemSpaces;
    GetID(pName);
    RemSpaces;
    GetID(pName);
    if pName <> '' then auto := true
    else auto := false;
    
    
    repeat
      gotAnswer := true;
      Write('Which device to scavenge? (F = Floppy, H = Harddisk) : [H] ');
      if auto then begin
                   c := 'H';
                   writeLn(c);
                   end
      else begin
           if eoln then c := 'H'
           else read(c);
           readln;
           end;
      if (c='f') or (c='F') then Configuration := FloppyDouble
      else if (c='h') or (c='H') then Configuration := Winch24
      else gotAnswer := false;
    until gotanswer;

    if Configuration = Winch24 then   {harddisk}
      begin
      Write('This seems to be a ');
      case GetIntDiskKinds of
         Shugart14: begin
                     Write('SHUGART 14-Inch');
                     Configuration := Winch12;
                    end;
         Mic8     : begin
                     Write('MICROPOLIS 8-Inch');
                     if not eioflag then
                      configuration := ciomicrop
                    end;
         Mic5     : begin
                     Write('5.25"');
                     configuration := generic5Inch;
                    end;
         Otherwise: Write('*** ILLEGAL TYPE ***');
         end;
      WriteLn(' disk.');
      if not Ask('Is this right? ','Yes') then
         begin
         WriteLn('** Your IO board hardware is probably bad **');
         if not Ask('** Are you sure you want to continue? ','No') then
            exit(Scavenger);
         end;
      
      if Configuration = Winch12 then  
        if IO24MByte then if Ask('Is this a 24 MByte disk','Yes') 
                             then Configuration := Winch24
                          else
        else if Ask('Is this a 12 MByte disk','Yes') then
             else Configuration := Winch24;
      end
    else begin
         cheat.lng := Zero;
         cheat.dbl[1] := lor(cheat.dbl[1], FLOPBITS);
         InfoBlk := cheat.lng;
         DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead);
         if (BufferPtr^.PartKind <> Root) or
             ((BufferPtr^.PartDevice <> FloppySingle) and
              (BufferPtr^.PartDevice <> FloppyDouble)) then
                 if not Ask('** This does not look like a fileSystem floppy.  Continue','No') then DoExit;
         If BufferPtr^.PartDevice = FloppySingle then
           if Ask('Is this a Single Sided Floppy','Yes') then
                Configuration := FloppySingle
           else
         else if Ask('Is this a Double Sided Floppy','Yes') then
              else Configuration := FloppySingle;
         end;
    If Configuration = Generic5Inch Then
        begin
        diskname := '';
        If Auto then
            begin
            remspaces;
            GetID(DiskName);
            End;
        SetupdiskParams(Auto, DiskName,
                        NHeads,
                        NCyls,
                        NSecs,
                        BootSize,
                        WriteCompCyl);
        End;

    WriteLn;
    { DryRun := Ask('Do a dry run','No');
    { Writeln;
    {}
    CanChange := Ask('Can I make changes to your disk','Yes');
    if CanChange then
      CanChange := Ask('Are you sure','Yes');
    if MemoryInBlocks <= 512 then s := 'NO'
    else s := 'Yes';
    CheckBlockNum :=
      Ask('Do you want logical block number consistency checking',s);
    CheckSerialNum := Ask('Do you want serial number consistency checking',s);
    if CheckBlockNum or CheckSerialNum
      then OnePass := Ask('Is there enough memory to do it in one pass','Yes')
      else OnePass := true;
    Write('How many tries for a suspect read? [1] ');
    if auto then begin
                 TryCount := 1;
                 WriteLn(TryCount);
                 end
    else begin
         if eoln then TryCount := 1
         else Read(TryCount);
         readln;
         end;
    DeleteTemps := false;
    CanReWrite := false;
    if CanChange then
      begin
      DeleteTemps := Ask('Do you want temporary segments deleted','Yes');
      CanReWrite := Ask('Can I rewrite incorrigible blocks','No');
      end;      {*** CDH Bug FIX ***}
    if auto then ignorePairs := 0
    else
(**)  repeat
(**)      gotAnswer := true;
(**)      IgnorePairs := 0;
(**)      Write('Type pairs to ignore (cyl head cyl head ...): [] ');
(**)      while not eoln do
(**)        begin
(**)        if ignorePairs >= NIgnorePairs then
(**)           begin
(**)           WriteLn('** Can only remember ',NIgnorePairs:1,' pairs!');
(**)           gotAnswer := false;
(**)           end
(**)        else ignorePairs := ignorePairs+1;
(**)        ReadUnsigned(BadPairs[ignorePairs].cyl);
(**)        ReadUnsigned(BadPairs[ignorePairs].head);
(**)        end;
(**)      Readln;
(**)  until gotAnswer;
    if auto then IgnoreBlocks := 0
    else
(**)  repeat
(**)      gotAnswer := true;
(**)      IgnoreBlocks := 0;
(**)      Write('Type other blocks to ignore: [] ');
(**)      while not eoln do
(**)        begin
(**)        if ignoreBlocks >= NumIgnoreBlocks then
(**)           begin
(**)           WriteLn('** Can only remember ',NumIgnoreBlocks:1,' blocks!');
(**)           gotAnswer := false;
(**)           end
(**)        else ignoreBlocks := ignoreBlocks+1;
(**)        ReadUnsigned(BadBlks[ignoreBlocks]);
(**)        end;
(**)      Readln;
(**)  until gotAnswer;
    Debug := Ask('Do you want complete error listing','Yes');
    {*** DebugMax is now a constant.
    DebugMax := false;
    if Debug then
      DebugMax := Ask('Do you want complete map listing','No');
    ***}

    case Configuration of
        Winch12: TypeDisk := DISKBITS;
        Winch24: TypeDisk := DISKBITS;
        FloppySingle: TypeDisk := FLOPBITS;
        FloppyDouble: TypeDisk := FLOPBITS;
        ciomicrop : TypeDisk := DISKBITS;
        Generic5Inch : TypeDisk := DISKBITS;
        Otherwise: Raise DiskError('Scavenger: Bad config.')
    end;

    cheat.lng := Zero;
    cheat.dbl[1] := lor(cheat.dbl[1], TypeDisk);
    ScavStrtBlk := DoubleAdd(cheat.lng, WordsPerBlock);
    InfoBlk := cheat.lng;
    EndBlk := LastDiskAddr(Configuration);
    Disk := WhichDisk(InfoBlk);
        
    cheat.lng := LogAddrtoPhysAddr(EndBlk);
    case Disk of
      0: if EIOFlag or 
            (ciodisktype = ciomicropolis) or 
            (Configuration =Generic5Inch)
           then LastCyl := Cheat.Dbl[1]
           else LastCyl := Shift(Cheat.Dbl[0], -8);
      1: LastCyl := 0; {hopefully not used}
      end;

(**) FixPairTable;

    Consistent := true;
    TempCnt := 0;
    EvilCnt := 0;
    BadHead := 0;
    BadTail := 0;
    BadPages := 0;


{ Build partition map. }
{$ifc DebugMax then}
 ProdUser('About to build partition map.');
{$endc}
    DeviceMount(Disk);
    DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead);
    RootName := '';
    for i := 1 to 8 do
      if BufferPtr^.PartName[i] <> chr(32) then
        begin
        Adjust(RootName,i);
        RootName[i] := BufferPtr^.PartName[i];
        end;

    for i := 0 to 63 do
      begin
      PartDAs[i] := Zero;
      PartDA := BufferPtr^.SubParts[i];
      PartNames[i] := '        ';
      if PartDA <> Zero then
        begin
        If NotOver64K(PartDA) then
          begin
          DiskIO(PartDA, Buffer2, Label2, DskRead);
          If NotOver64K(Buffer2^.PartEnd) then
            begin
            for j := 1 to 8 do
              PartNames[i][j] := Buffer2^.PartName[j];
            PartDAs[i] := PartDA; {within 64K pages}
            end;
          end;
        end;
      end;


{ Tell user about the Root DIB and ask for partition name. }
    WriteLn;
    PrintDIB(InfoBlk);
    WriteLn;
    gotanswer := false;
    repeat
      Write('Which partition do you want to scavenge? ');
      if auto then WriteLn(PName)
      else ReadLn(PName);
      if length(PName) > 0 then
        begin
        name := '        ';
        for i := 1 to length(PName) do name[i] := PName[i];
        for i := 0 to 63 do
          if UpperEqual(Name, PartNames[i]) then
            begin
            if PartDAs[i] <> Zero then
              begin
              gotanswer := true;
              DiskIO(PartDAs[i], Buffer2, Label2, DskRead);
              InfoBlk := PartDAs[i];
              ScavStrtBlk := DoubleAdd(InfoBlk,WordsPerBlock);
              EndBlk := Buffer2^.PartEnd;
              end;
            end;
        end;
      if not gotAnswer then auto := false;
    until gotanswer;
    WriteLn;
    ScavDeviceName := RootName;
    AppendChar(ScavDeviceName,':');
    ScavPartitionName := concat(ScavDeviceName,PName);
    AppendChar(ScavPartitionName,'>');

    AppendString(ScavTitle, '    ');
    AppendString(ScavTitle, ScavPartitionName);
    AppendString(ScavTitle, '  ');
    ChangeTitle(ScavTitle);

    CurPicInit;
    WriteLn('Dismounting partition ',ScavPartitionName);
    DismountPartition(ScavPartitionName);
    ForgetAll;                               {*** see V1.6 change history ***}
    WriteLn;
    PrintDIB(InfoBlk);

    StartField := AddrToField(ScavStrtBlk);
    EndField := AddrToField(EndBlk);
    EndIdx := EndField - StartField + 1;

    if EndIdx < 0 then Raise DiskError('Scavenger: Not enough room.');
    CurInit;

{ Allocate storage for map produced by AnalyzeDisk. }
    SegSize := ((EndIdx+1) div 256) + 1;
    SegBMSize := ((EndIdx+1) div (256*16)) + 1;

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegNext, SegSize, 1, SegSize);
    Next := MakePtr(SegNext, 0, ptrIntArray);

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegPrev, SegSize, 1, SegSize);
    Prev := MakePtr(SegPrev, 0, ptrIntArray);

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegConfirmed, SegBMSize, 1, SegBMSize);
    Confirmed := MakePtr(SegConfirmed, 0, ptrBitMap);

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegChainHead, SegBMSize, 1, SegBMSize);
    ChainHead := MakePtr(SegChainHead, 0, ptrBitMap);

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegSegHead, SegBMSize, 1, SegBMSize);
    SegHead := MakePtr(SegSegHead, 0, ptrBitMap);

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegFree, SegBMSize, 1, SegBMSize);
    FreeBlk := MakePtr(SegFree, 0, ptrBitMap);

    MMFirst := SATSeg; { be clever: try to get this segment on the screen }
    CreateSegment(SegTempHead, SegBMSize, 1, SegBMSize);
    TempHead := MakePtr(SegTempHead, 0, ptrBitMap);

    if CheckBlockNum and OnePass then
        begin
        MMFirst := SATSeg; { be clever: try to get this segment on the screen }
        CreateSegment(SegBlkNum, SegSize, 1, SegSize);
        BlockNum := MakePtr(SegBlkNum, 0, ptrIntArray);
        end;

    if CheckSerialNum and OnePass then
        begin
        MMFirst := SATSeg; { be clever: try to get this segment on the screen }
        CreateSegment(SegSerNum, SegSize, 1, SegSize);
        SerNum := MakePtr(SegSerNum, 0, ptrIntArray);
        end;

{ Analyze the disk.  Produces a map consisting of the Next array (next pointers
{       or BAD/INCORRIGIBLE marks), the Free bit array, and the SegHead array.
{       AnalyzeDisk verifies well-formed segments.  Segments that are not
{       well-formed are marked BAD. }
    AnalyzeDisk;

{ Reconstruct the Free Page List, and construct a bad page segment. }
    if CanChange then
        begin
        BuildFreeList;
        InitBuffers;
        InitAlloc;
        DeviceMount(Disk);
        i := MountPartition(ScavPartitionName);

        DismountPartition(ScavPartitionName);
        end;

{ Print summary of results. }
    ChangeTitle(Concat(ScavTitle, '   Summary.'));
    WriteLn;
    if Consistent then
        WriteLn('No errors found.')
      else
        begin
        WriteLn('Found ',badPages:1,' bad pages and ',evilCnt:1,
                ' incorrigible pages.');
(******
        if BadPages <> 0 then
          begin
          WriteLn('The following pages were bad:');
          cnt := 9;
          for i := 1 to EndIdx do
            begin
            if not Confirmed^[i] and not FreeBlk^[i] and
               (Next^[i] <> INCORRIGIBLE) then
                begin
                Write(IdxToField(i):6:-10, ' ');
                cnt := cnt - 1;
                if (cnt <= 0) then
                    begin
                    cnt := 9;
                    WriteLn;
                    end;
                end;
            end;
          if cnt <> 9 then WriteLn;
          end; {bad pages}
        if EvilCnt <> 0 then
          begin
          WriteLn;
          WriteLn('The following pages were incorrigible:');
          cnt := 9;
          for i := 1 to EndIdx do
            begin
            if not Confirmed^[i] and not FreeBlk^[i] and
               (Next^[i] = INCORRIGIBLE) then
                begin
                Write(IdxToField(i):6:-10, ' ');
                cnt := cnt - 1;
                if (cnt <= 0) then
                    begin
                    cnt := 9;
                    WriteLn;
                    end;
                end;
            end;
          if cnt <> 9 then WriteLn;
          end; {evil}
*******)
        end;

    if CanChange then
      begin
      WriteLn;
      Write('Temporary segments deleted = ');
      if TempCnt = 0 then WriteLn('none.') else WriteLn(TempCnt:6:-10);
      WriteLn;
      PrintDIB(InfoBlk);
      end
    else WriteLn('Would have deleted ',tempCnt:1,' temporary segments');

{ Reconstruct File System Directory. }
    if Consistent or CanChange then
      begin
      WriteLn;
      if badPages <> 0 then
        begin
        s := 'YES';
        Writeln('Suggest rebuilding the directory.');
        end
      else s := 'No';
      DecRefCount(SegNext);  {need room for RebuildDirectory}
      if Ask('Do you want to rebuild the directories',s) then
        begin
        DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead);
        i := IntDouble(BufferPtr^.NumFree);
        if i < 200 then
            begin
            WriteLn('*** WARNING *** There are only ',i:1:-10,' blocks free in the partition.');
            WriteLn('*** This may not be enough to rebuild the directories ***');
            WriteLn('*** Suggest you delete some files before rebuilding ***');
            s := 'NO';
            end;
        if Ask('Are you sure you want to rebuild the directories',s) then
             begin
             ChangeTitle(Concat(ScavTitle, '   Rebuilding the directories.'));
             RebuildDirectory;
             end;
        end;
      end;

    ChangeTitle(ScavTitle);
    WriteLn;
    if Ask('Do you want the partition remounted','Yes') then
      begin
      DeviceMount(Disk);
      i := MountPartition(ScavPartitionName);
      end;


    end.  {Scavenger}
