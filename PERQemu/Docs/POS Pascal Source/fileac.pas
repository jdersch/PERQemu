{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module FileAccess;
{---------------------------------------------------------------------------
{
{ Abstract:
{ Module to handle reading, writing, entering and deleting files independant
{ from the directory structure.
{
{ Written by the CMU Spice Group
{
{   Copyright 1983, Three Rivers Computer Corporation

---------------------------------------------------------------------------}

{---------------------------------------------------------------------------
  Change history.

   16 Nov 82  V1.8 Bill Braucher  Fixed names for 14-character compiler.
   28 May 81  V1.7 BAM  Changes to Lights module.
   26 May 81  V1.6 JPS  Use new Lights module.
   19 May 81  V1.5 BAM  Fix position of scavenge light.
   12 May 81  V1.4 BAM  Prevent scavenge from being recursive.
    7 Mar 81  V1.3 JPS  Use DiskSegment for memory allocation.
   30 Mar 81  V1.2 BAM  Renamed from SpiceSeg
   27 Mar 81  V1.1 BAM  Added comments; fixed truncate
   19 Mar 81  V1.0 BAM  Changed Bit32 to FSBit32; add FBLANKSIZE to Index
   17 Mar 81  V0.2 GGR  Fixed bug in SegAddr checking for logical block
                         inconsistency.
    6 Mar 81  V0.1 JPS  Export the Index procedure.
    ? ??? ??  V0.0 CMU-Spice Group  Started
---------------------------------------------------------------------------}


{******************} exports {***************************}

imports Arith from Arith;
imports DiskIO from DiskIO;
imports AllocDisk from AllocDisk;

function  CreateSpiceSegment(partition : integer; kind : SpiceSegKind) : SegID;
procedure DestroySpiceSegment(id : SegID);
procedure TruncateSpiceSegment(id : SegID; len : integer);
procedure ReadSpiceSegment(id : SegID; firstblk,numblks : integer; 
                           ptr : ptrDiskBuffer);
procedure WriteSpiceSegment(id : SegID; firstblk,numblks : integer;
                            ptr : ptrDiskBuffer);
procedure Index(logblk : integer; var indblk,indoff : integer);

Exception BadLength(len: integer);
{---------------------------------------------------------------------------
 Abstract: Raised if try to truncate file to a length < 0
 Parameters: len is bad length
---------------------------------------------------------------------------}

Exception NotAFile(id: SegID);
{---------------------------------------------------------------------------
 Abstract: Raised when an operation is attempted and the SegID passed does not
            seem to be the id for a valid file
 Parameters: id is the bad id
---------------------------------------------------------------------------}

{******************} private {***************************}

imports Lights from Lights;

const
  DEBUG = false;
  SHOWSCAV = LightUsed; {RasterOp square on screen when scav so can tell}
  WRITEOK = FALSE; {Write a message when scav so can tell}
  
var
  inScav: boolean;  {used to prevent recursive calls to scavenge}
  

imports ReadDisk from ReadDisk;
imports Except from Except;

{$ifc ShowScav then}
imports Screen from Screen;
{$endc}


Procedure Index(logblk : integer; var indblk,indoff : integer);
{---------------------------------------------------------------------------
 Abstract: Find the index block and the offset from the top of the block for
           a logical block of a file
 Parameters: logBlk - the logical block of the file to look up; may be 
              negative
             indBlk - the logical block number of the index block which holds
              the address for logblk
             indoff - the offsetin indBlk to use in reading the address (the
              array index to use in DiskBuffer^.Addr).  It is correctly set
              even if the indBlk is the FIBlk
---------------------------------------------------------------------------}
  const FBLANKSIZE = WordSize(FSDataEntry);

{$IFC FBLANKSIZE Mod 2 <> 0 THEN}
{$MESSAGE **ERROR** FBLANKSIZE not even}
{$ENDC}

  begin
    if logblk >= 0 then
      begin
        if logblk < 64 then
          begin
            indblk := -1;
            indoff := (FBLANKSIZE div 2) + logblk;
          end
        else 
          begin
            indblk := (-((logblk - 64) div 128)) - 4;
            indoff :=    (logblk - 64) mod 128;
          end;
      end
    else
      begin
        if logblk > -4  then
          begin
            indblk := -1;
            indoff := (FBLANKSIZE div 2)+DIRECTSIZE+INDSIZE+((-logblk)-2);
          end
        else if logblk > -36 then
          begin
            indblk := -1;
            indoff := (FBLANKSIZE div 2)+DIRECTSIZE+((-logblk)-4);
          end
        else
          begin
            indblk := (-(((-logblk)-36) div 128))-2;
            indoff :=    ((-logblk)-36) mod 128;
          end
      end;
  end;


Function CheckHeader(id : SegID; logblk : integer; hdr : ptrHeader) : boolean;
{---------------------------------------------------------------------------
 Abstract: Check to see if the hdr is valid for file id with logical block
            logBlk
 Parameters: id     - the SegID (serialNumber) of the file; 
             logBlk - the logical block number and
             hdr    - the header of a block which is supposed to correspond 
                      to both
 Returns: True if the header serial number and logical block match the 
           arguments else false
---------------------------------------------------------------------------}
  begin
    if (hdr^.SerialNum = id) and
       (hdr^.LogBlock = logblk) then CheckHeader := true
    else CheckHeader := false;
  end;


Function FindBlock(id : SegID; logblk : integer) : DiskAddr;
{---------------------------------------------------------------------------
 Abstract: Searches down the block chain (using next or prev addresses in
            header) starting from id to find the logical block logBlk or a
            hole where it should go
 Parameters: id     - the SegID of the file; 
             logBlk - the logical block number which may be positive or negative
 Returns: The diskAddress of the block if found; if not in chain then returns
           the block BEFORE where the block should go (i.e. the block with 
           abs(log address) < abs(logBlk)); if abs(logBlk) bigger than last
           (pos or neg) block of file then returns last (pos or neg) block
---------------------------------------------------------------------------}
  var
    next : DiskAddr;
    hdr  : ptrHeader;
    test : boolean;
    newnext : DiskAddr;

  begin
    next := id;
    while true do 
      begin
        hdr := ReadHeader(next);
        if logblk < 0 then 
          begin
            newnext := hdr^.PrevAdr;
            test := hdr^.LogBlock <= logblk
          end
        else 
          begin
            newnext := hdr^.NextAdr;
            test := hdr^.LogBlock >= logblk
          end;
        if (newnext = DBLZERO) or test then
          begin
            FindBlock := next;
            exit(FindBlock);
          end;
        next := newnext;
      end;
  end;


Function LastSegBlock(id : SegID) : DiskAddr;
{---------------------------------------------------------------------------
 Abstract: Find the last positive block of a file
 Parameters: id is the SegID of the file
 Returns: The diskAddress of the last block of the file
 Design: Reads the FiBlk and then reads the header of the block the FiBlk says
          is last; if not then tries to find a block bigger than any block
          there should be on the file (32767)
 Calls: FindBlock, (ReadDisk routines)
---------------------------------------------------------------------------}
  var
    ptr     : ptrDiskBuffer;
    hdr     : ptrHeader;
    lastaddr: DiskAddr;
    lastblk : integer;
   
  begin
    ptr := ReadDisk(id);
    lastblk  := ptr^.LastBlk;
    lastaddr := ptr^.LastAddr;
    hdr := ReadHeader(lastaddr);
    if CheckHeader(id,lastblk,hdr) and
       (hdr^.NextAdr = DblZero) then
      LastSegBlock := lastaddr
    else
      LastSegBlock := FindBlock(id,#77777);
  end;


Function LastNegSegBlock(id : SegID) : DiskAddr;
{---------------------------------------------------------------------------
 Abstract: Find the last negative block of a file
 Parameters: id is the SegID of the file
 Returns: The diskAddress of the last block of the file
 Design: Reads the FiBlk and then reads the header of the block the FiBlk says
          is last; if not then tries to find a block smaller than any block
          there should be on the file (-32768)
 Calls: FindBlock, (ReadDisk routines)
---------------------------------------------------------------------------}
  var
    ptr     : ptrDiskBuffer;
    hdr     : ptrHeader;
    lastblk : integer;
    lastaddr: DiskAddr;

  begin
    ptr := ReadDisk(id);
    lastblk  := ptr^.LastNegBlk;
    lastaddr := ptr^.LastNegAddr;
    hdr := ReadHeader(lastaddr);
    if CheckHeader(id,lastblk,hdr) and
       (hdr^.PrevAdr = DblZero) then
      LastNegSegBlock := lastaddr
    else
      LastNegSegBlock := FindBlock(id,#100000);
  end;


Procedure Scavenge(id : SegID);
{---------------------------------------------------------------------------
 Abstract: Rebuilds random index of file id
 Parameters: id is the SegID of the file
 SideEffects: Changes the FiBlk of file
 Errors: Raises DiskError if something wrong
---------------------------------------------------------------------------}
 Handler All(a, b, c, d: integer);
    begin
    {$ifc showScav then}
      if inScav then
         RasterOp(RNot, LightWidth, LightHeight,
                        LightScavenge, LightY, SScreenW, SScreenP,
                        LightScavenge, LightY, SScreenW, SScreenP);
    {$endc}
    inScav := false;
    RaiseP(a, b, c, d);
    end;

  var
    addr,next   : DiskAddr;
    hdr         : ptrHeader;
    ptr,indptr  : ptrDiskBuffer;
    i,indoff    : integer;
    numindexblks: integer;
    logblk      : integer;
    newlogblk   : integer;
    indblk      : integer;
    
  begin
    if inScav then
        Raise DiskError('Recursive call to Scavenge not allowed.  Run Scavenger!');
    inScav := true;
    numindexblks := 0;
    addr := id;
    next := addr;
    
    ptr := ReadDisk(id);

{$ifc WriteOK then}
    WriteLn('Scavenging file: ',ptr^.FSData.FileName,
            ' (',AddrToField(id):1,')');
{$endc}
{$ifc ShowScav then}
  RasterOp(RNot, LightWidth, LightHeight,
                 LightScavenge, LightY, SScreenW, SScreenP,
                 LightScavenge, LightY, SScreenW, SScreenP);
{$endc}
    
    while next <> DBLZERO do
      begin
        hdr := ReadHeader(addr);
        logblk := hdr^.LogBlock;
        next := hdr^.PrevAdr;
        if next <> DBLZERO then
          begin
            numindexblks := numindexblks + 1;
            hdr := ReadHeader(next);
            newlogblk := hdr^.LogBlock;

            if (newlogblk >= logblk) or (hdr^.NextAdr <> addr) then
              Raise DiskError('Unable to scavenge at runtime. Run Scavenger!');

            addr := next;
          end;
      end;

    hdr := ReadHeader(id);
    next := hdr^.PrevAdr;

    ptr := ChangeDisk(id);
    hdr := ChangeHeader(id);
    hdr^.PrevAdr := DBLZERO;
    ptr^.LastNegBlk := -1;
    ptr^.LastNegAddr := id;
    ptr^.NumBlksInUse := ptr^.NumBlksInUse - numindexblks;
    for i := 0 to DIRECTSIZE-1 do ptr^.Direct[i]   := DBLZERO;
    for i := 0 to INDSIZE-1    do ptr^.Indirect[i] := DBLZERO;
    for i := 0 to DBLINDSIZE-1 do ptr^.DblInd[i]   := DBLZERO;
    FlushDisk(id);

{$ifc WriteOK then}
    Write('Scavenger: rebuilding Index table...');
{$endc}
    

    if next <> DBLZERO then DeallocChain(addr,next,numindexblks);

    addr := id;
    next := addr;
    while next <> DBLZERO do
      begin
        hdr := ReadHeader(addr);
        logblk := hdr^.LogBlock;
        next := hdr^.NextAdr;
        if next <> DBLZERO then
          begin
            hdr := ReadHeader(next);
            newlogblk := hdr^.LogBlock;

            if (newlogblk <= logblk) or (hdr^.PrevAdr <> addr) then
              begin
            {$ifc writeOK then}
              WriteLn('Failed!!');
              WriteLn('Scavenger: newlogblk (',newlogblk:1,') <> logblk (',
                        logblk:1,')');
            {$endc}
              Raise DiskError('Unable to scavenge at runtime. Run Scavenger!');
              end;

            if DEBUG then
              WriteLn('Scavenger: adding index for block ',newlogblk);
            
            new(DiskSegment,1,indptr);
            Index(newlogblk,indblk,indoff);
            ReadSpiceSegment(id,indblk,1,indptr);
            indptr^.Addr[indoff] := next;
            WriteSpiceSegment(id,indblk,1,indptr);
            dispose(indptr);

            addr := next;
          end;
      end;
   {$ifc WriteOK then}
    WriteLn('Done');
   {$endc}
   {$ifc ShowScav then}
    RasterOp(RNot, LightWidth, LightHeight,
                   LightScavenge, LightY, SScreenW, SScreenP,
                   LightScavenge, LightY, SScreenW, SScreenP);
   {$endc}
  inScav := false;
  end;


function  CreateSpiceSegment(partition : integer; kind : SpiceSegKind) : SegID;
{---------------------------------------------------------------------------
 Abstract: Create a new empty file on partition specified
 Parameters: partition is the partition in which to allocate file; kind is the
              type of the file
 Returns: ID of file created
 Errors: Raises NotAFile if block at id does not seem to be a valid FIBlk
---------------------------------------------------------------------------}
  var
    i   : integer;
    id  : DiskAddr;
    hdr : ptrHeader;
    ptr : ptrDiskBuffer;

  begin
    id := AllocDisk(partition);
    ptr := ChangeDisk(id);
    hdr := ChangeHeader(id);
    hdr^.SerialNum := id;
    hdr^.LogBlock := -1;
    hdr^.PrevAdr  := DBLZERO;
    hdr^.NextAdr  := DBLZERO;
    ptr^.SegKind := kind;
    ptr^.LastBlk := -1;
    ptr^.LastAddr := id;
    ptr^.LastNegBlk := -1;
    ptr^.LastNegAddr := id;
    ptr^.NumBlksInUse := 1;
    for i := 0 to DIRECTSIZE-1 do ptr^.Direct[i] := DBLZERO;
    for i := 0 to INDSIZE-1    do ptr^.Indirect[i] := DBLZERO;
    for i := 0 to DBLINDSIZE-1 do ptr^.DblInd[i] := DBLZERO;
    FlushDisk(id);
    CreateSpiceSegment := id;
  end;


Procedure DestroySpiceSegment(id : SegID);
{---------------------------------------------------------------------------
 Abstract: Delete a file
 Parameters: id is the SegId of file to delete
 SideEffects: removes id from filesystem
 Errors: Raises NotAFile if block at id does not seem to be a valid FIBlk
---------------------------------------------------------------------------}
  var
    hdr : ptrHeader;
    ptr : ptrDiskBuffer;
    blks: integer;

  begin
    hdr := ReadHeader(id);
    if CheckHeader(id,-1,hdr) then
      begin
        hdr := ChangeHeader(id);
        hdr^.SerialNum := DBLZERO;
        ptr := ReadDisk(id);
        blks := ptr^.NumBlksInUse;
        DeallocChain(LastNegSegBlock(id),LastSegBlock(id),blks);
      end;
  end;


Function SegAddr(id : SegID; logblk : integer;
                  var prev,cur,next : DiskAddr) : boolean;
{---------------------------------------------------------------------------
 Abstract: Finds the current, next and previous blocks for a specified logical
            block number
 Parameters: id is the SegId of file; logBlk is the logical block number of 
              block to find; prev, cur and next are set with the blocks before,
              of and after the logBlk block, respectively if found;
             if not found, then prev, cur and next are those of the existing
              block closer to -1
 Returns: true if found else false
 Errors: Raises NotAFile if bad header (if block at id does not seem to be
           a valid FIBlk)
---------------------------------------------------------------------------}
  var
    i,indblk,indoff : integer;
    found           : boolean;
    inc             : integer;
    ptr             : ptrDiskBuffer;
    hdr             : ptrHeader;
    addr            : DiskAddr;
    cheat           : DiskCheatType;

  begin
    hdr := ReadHeader(id);
    if not CheckHeader(id,-1,hdr) then Raise NotAFile(id);
    SegAddr := false;

    i := logblk;
    found := false;
    if logblk < 0 then inc := 1 else inc := -1;

    while (i <> -1) and (not found) do
      begin
        Index(i,indblk,indoff);
        if SegAddr(id,indblk,prev,cur,next) then
          begin
            ptr  := ReadDisk(cur);
            addr := ptr^.Addr[indoff];
            hdr  := ReadHeader(addr);
            if DEBUG then 
              begin
                Write('SegAddr: cur  = ',AddrToField(cur));
                WriteLn(' addr = ',AddrToField(addr));
              end;
            if CheckHeader(id,i,hdr) then
              begin
                cur  := addr;
                next := hdr^.NextAdr;
                prev := hdr^.PrevAdr;
                found := true;
              end;
          end;
        if not found then i := i + inc;
      end;  
    
    if not found then
      begin
        hdr  := ReadHeader(id);
        cur  := id;
        next := hdr^.NextAdr;
        prev := hdr^.PrevAdr;
      end;

    if logblk = i then SegAddr := true 
    else if inc = -1 then
      begin
        if next = DBLZERO then SegAddr := false
        else
          begin
            hdr := ReadHeader(next);
            if hdr^.LogBlock <= logblk then
              begin
                Scavenge(id);
                SegAddr := SegAddr(id,logblk,prev,cur,next);
              end;
          end;
      end
    else if prev = DBLZERO then SegAddr := false
        else
          begin 
            hdr := ReadHeader(prev);
            if hdr^.LogBlock >= logblk then
              begin
                Scavenge(id);
                SegAddr := SegAddr(id,logblk,prev,cur,next);
              end;
          end;
    if DEBUG then WriteLn('SegAddr: id = ',AddrToField(id),' logblk = ',logblk,
            ' prev = ',AddrToField(prev),' cur = ',AddrToField(cur),
            ' next = ',AddrToField(next));
  end;


Procedure TruncateSpiceSegment(id : SegID; len : integer);
{---------------------------------------------------------------------------
 Abstract: Removes blocks from file to make the new length len
 Parameters: id is the SegId of file; len is the new length (one greater than
             the last logical block number since files start at 0) 
 SideEffects: Shortens the file
 Errors: Raises BadLength is length to truncate file to is < 0
         Raises NotAFile if block at id does not seem to be a valid FIBlk
---------------------------------------------------------------------------}
  var
    hdr           : ptrHeader;
    logblk        : integer;
    ptr           : ptrDiskBuffer;
    test          : boolean;
    prev,cur,next : DiskAddr;
    numdeallocated: integer;
    indptr        : ptrDiskBuffer;
    indblk,indoff : integer;
    prevlogblk    : integer;

  begin
    if len < 0 then Raise BadLength(len);
    test := SegAddr(id,len,prev,cur,next);
    if not test then prev := cur;  {block specified doesn't exist}
  {prev is the last block to keep}
    if prev = DBLZERO then Exit(TruncateSpiceSegment);
    hdr := ChangeHeader(prev);
    cur := hdr^.NextAdr;
    prevlogblk := hdr^.LogBlock;
    hdr^.NextAdr := DBLZERO;
    FlushDisk(prev);
    numdeallocated := 0;

    new(DiskSegment,1,indptr);
    while cur <> DBLZERO do
      begin
        hdr := ReadHeader(cur);
        next := hdr^.NextAdr;
        logblk := hdr^.LogBlock;
        DeallocDisk(cur);
        cur := next;
        numdeallocated := numdeallocated + 1;
        Index(logblk,indblk,indoff);
        ReadSpiceSegment(id,indblk,1,indptr);
        indptr^.Addr[indoff] := DBLZERO;
        WriteSpiceSegment(id,indblk,1,indptr);
      end;
    dispose(indptr);

    ptr := ChangeDisk(id);
    ptr^.NumBlksInUse := ptr^.NumBlksInUse - numdeallocated;
    ptr^.LastBlk  := prevlogblk;
    ptr^.LastAddr := prev;
    FlushDisk(id);
  end;


Procedure ReadSpiceSegment(id : SegID; firstblk,numblks : integer;
                            ptr : ptrDiskBuffer);
{---------------------------------------------------------------------------
 Abstract: Reads one or more blocks from file
 Parameters: id       - the SegId of file;
             firstBlk - the logical blk # of first to read
             numBlks  - the number of blocks to read
             ptr      - where the data should be put 
 NOTE: If the blocks specified to read don't exist; ptr^ is filled with zeros
 Errors: Raises NotAFile if block at id does not seem to be a valid FIBlk
---------------------------------------------------------------------------}
  var
    hdr          : ptrHeader;
    srcptr       : ptrDiskBuffer;
    TwoFiftySix  : FSBit32;
    prev,cur,next: DiskAddr;
    i            : integer;
    found        : boolean;

  begin
    if DEBUG then 
      WriteLn('ReadSpiceSegment: id = ',AddrToField(id),' firstblk = ',firstblk,              ' numblks = ',numblks);
            
    TwoFiftySix := DoubleInt(256);
    found := SegAddr(id,firstblk,prev,cur,next);
    for i := firstblk to firstblk + numblks - 1 do
      begin
        if cur = DBLZERO then ZeroBuffer(ptr)
        else
          begin
            srcptr := ReadAhead(cur);
            hdr := ReadHeader(cur);
            if hdr^.LogBlock = i then ptr^ := srcptr^
            else ZeroBuffer(ptr);
            if i >= hdr^.LogBlock then
              begin
                cur := next;
                next := hdr^.NextAdr;
              end;
          end;
        ptr := Recast(DoubleAdd(Recast(ptr,FSBit32),TwoFiftySix),ptrDiskBuffer);
      end;
  end;


Procedure WriteSpiceSegment(id : SegID; firstblk,numblks : integer;
                             ptr : ptrDiskBuffer);
{---------------------------------------------------------------------------
 Abstract: Writes one or more blocks onto file
 Parameters: id       - the SegId of file;
             firstBlk - the logical blk # of first to write
             numBlks  - the number of blocks to write
             ptr      - where the data should come from 
 SideEffects: Changes the data in the file and may cause new blocks to be
              allocated and file length changed
 Errors: Raises NotAFile if block at id does not seem to be a valid FIBlk
---------------------------------------------------------------------------}
  var
    hdr            : ptrHeader;
    TwoFiftySix    : FSBit32;
    prev,cur,next  : DiskAddr;
    indptr         : ptrDiskBuffer;
    dstptr         : ptrDiskBuffer;
    i,indblk,indoff: integer;
    numallocated   : integer;
    found          : boolean;
    addr           : DiskAddr;

  begin
    if DEBUG then 
      WriteLn('WriteSpiceSegment: id = ',AddrToField(id),' firstblk = ',
              firstblk,' numblks = ',numblks);
            
    found := SegAddr(id,firstblk,prev,cur,next);
    TwoFiftySix := DoubleInt(256);
    numallocated := 0;

    for i := firstblk to firstblk + numblks - 1 do
      begin
        hdr := ReadHeader(cur);
        if i = hdr^.LogBlock then
          begin
            dstptr := ChangeDisk(cur);
            dstptr^ := ptr^;
            cur := next;
            hdr := ReadHeader(next);
            next := hdr^.NextAdr;
          end
        else
          begin {create new block}
            addr := AllocDisk(WhichPartition(id));
            numallocated := numallocated + 1;
            
            if i < 0 then
              begin
                next := cur;
                cur  := prev;
              end;

            hdr := ChangeHeader(addr);
            hdr^.SerialNum := id;
            hdr^.PrevAdr := cur;
            hdr^.NextAdr := next;
            hdr^.LogBlock := i;
            dstptr := ChangeDisk(addr);
            dstptr^ := ptr^;
            FlushDisk(addr);

            if cur <> DBLZERO then
              begin
                hdr := ChangeHeader(cur);
                hdr^.NextAdr := addr;
                FlushDisk(cur);
              end;

            if next <> DBLZERO then
              begin
                hdr := ChangeHeader(next);
                hdr^.PrevAdr := addr;
                FlushDisk(next);
              end;

            { Update random index }
            cur := addr;

            new(DiskSegment,1,indptr);
            Index(i,indblk,indoff);
            ReadSpiceSegment(id,indblk,1,indptr);
            indptr^.Addr[indoff] := cur;
            WriteSpiceSegment(id,indblk,1,indptr);
            dispose(indptr);
          end; {create new block}
        ptr := Recast(DoubleAdd(Recast(ptr,FSBit32),TwoFiftySix),ptrDiskBuffer);
      end; {for loop}

    i := firstblk + numblks - 1;
    
    ptr := ReadDisk(id);

    if (i > ptr^.LastBlk) or (i < ptr^.LastNegBlk) or (numallocated <> 0) then
      begin
        ptr := ChangeDisk(id);
        if i > ptr^.LastBlk then 
          begin
            ptr^.LastBlk  := i;
            ptr^.LastAddr := cur;
          end
        else if i < ptr^.LastNegBlk then
          begin
            ptr^.LastNegBlk  := i;
            ptr^.LastNegAddr := cur;
          end;

        ptr^.NumBlksInUse := ptr^.NumBlksInUse + numallocated;
      end;
  end.

