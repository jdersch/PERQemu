{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module MultiRead;
{-----------------------------------------------------------------------------
   MultiRead - Module to Multi-sector read a file into memory.

   Written by Brad Myers    24 Jul 81.

   Copyright (C) Three Rivers Computer Corporation, 1981, 1983.

 Abstract:
       This Module exports a procedure to read a file very quickly into
       memory.  The memory for the blocks of the file to be read must be
       allocated contiguously before the call is made.  Typically, this
       will be done by using CreateSegment.

-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
  Change Log:
      1-Mar-83  V1.1  Brad Myers  Fixed so that doesn't read from beginning
                                    of file if specify high block number.
     24-Jul-81  V1.0  Brad Myers  Created by modifying code in the Loader
-----------------------------------------------------------------------------}

{//////////////////////////} EXPORTS {\\\\\\\\\\\\\\\\\\\\\\\\\\}

Imports FileSystem from FileSystem;

Procedure MultiRead(fid: FileID; addr: pDirBlk; firstBlock,numBlocks: integer);

{//////////////////////////} PRIVATE {\\\\\\\\\\\\\\\\\\\\\\\\\\}

{$R-}
   
imports DiskIO from DiskIO;
imports IO_Unit from IO_Unit;
imports IOErrors from IOErrors;
imports ReadDisk from ReadDisk;
imports FileAccess from FileAccess;
   
CONST Debug = FALSE;

{$ifc debug then}
  var debug1: boolean;
{$elsec}
CONST Debug1 = TRUE;
{$endc}


Procedure MultiRead(fid: FileID; addr: pDirBlk; firstBlock,numBlocks: integer);
{-----------------------------------------------------------------------------
  Abstract: Does a multi-sector read on the file specified into the memory
            pointed to by addr
  NOTE: This only works for contiguous files. 
  Parameters: fid - the fileID of the file to read from.
              addr - the address of the start of the memory to read the file
                into.  This must be pre-allocated.
              firstBlock - the logical block number of the first to read
                (the first legal value is 0; -1 will not work).
              numBlocks - the count of the number of blocks to transfer.
-----------------------------------------------------------------------------}
var physBlk: FSBit32;
    Status: IOStatPtr;
    BlockHeader: IOHeadPtr;
    BlockAddress, sid: record case Integer of
                          1: (A: DiskAddr);
                          2: (D: Double)
                          end;
    I, J, IndOff, IndBlk: Integer;
    BlockCount: Integer;
    Buf: record case Integer of
                 1: (P: IOBufPtr);
                 2: (Offset: Integer;
                     Segment: Integer);
                 3: (f: pDirBlk);
                 4: (d: ptrDiskBuffer);
                 end;
    hdr: ptrHeader;
    begin
    if firstBlock < 0 THEN Raise DiskError('Attempt to read illegal block');
    NEW(0,4,Status);
    NEW(0,4,BlockHeader);
    Buf.f := addr;
    sid.a := FileIdToSegId(fid);
    if (firstBlock > 1) and DEBUG1 then
       begin
       Index(firstBlock, indBlk, indOff);  {find the index block holding addr}
       FSBlkRead(fid, indBlk, buf.f);
       physBlk := buf.d^.addr[indOff];  {get the addr of the desired block}
       end
    else begin
         physBlk := sid.a;
         repeat
           hdr := ReadHeader(physBlk); {hdr is for the block before physBlk}
           physBlk := hdr^.nextAdr;
         until hdr^.logBlock = firstBlock-1; 
         end;
    
    BlockAddress.A := LogAddrToPhysAddr(physBlk);
    sid.a := LogAddrToPhysAddr(sid.a);
    with BlockHeader^ do
              begin
              SerialNum := sid.d;
              NextAdr := BlockAddress.D;
              PrevAdr := BlockAddress.D;
              LogBlock := firstBlock;
              Filler := 0
              end;
    repeat
      if numBlocks < 64 then BlockCount := numBlocks
      else BlockCount := 63;
      I := 0;
      repeat
         I := I + 1;
         J := 0;
         repeat
            J := J + 1;
            UnitIO(HardDisk, Buf.P, IORead,
                     Shift(BlockCount,9), BlockAddress.D, BlockHeader, Status);
            if Status^.SoftStatus = IOEIOC then
                begin
                numBlocks := numBlocks - BlockCount;
                Buf.Offset := Buf.Offset + Shift(BlockCount,8);
                BlockAddress.D := BlockHeader^.NextAdr;
                BlockHeader^.LogBlock := BlockHeader^.LogBlock + 1
                end
            else with Status^ do
                  begin
                  if (SoftStatus < IOEFirstError) or
                       (SoftStatus > IOELastError) then
                      SoftStatus := IOEUDE;
                  ErrorCnt[SoftStatus] := ErrorCnt[SoftStatus] + 1
                  end
         until (Status^.SoftStatus = IOEIOC) or
                  (J = 5) or (Status^.SoftStatus = IOEADR);
         if Status^.SoftStatus <> IOEIOC then
              if I < 3 then DiskReset
     until (Status^.SoftStatus = IOEIOC) or (I = 3);
     if Status^.SoftStatus <> IOEIOC then
            raise DiskFailure('** Multi-read failure', DskRead,
                       PhysAddrToLogAddr(0,BlockAddress.A), Status^.SoftStatus)
  until numBlocks = 0;
  Dispose(Status);
  Dispose(BlockHeader);
  end { MultiRead }

{$ifc debug then}
;

imports memory from memory;

var fid: fileId;
    i,j: integer;
    ans: String[1];
    fn: string;
    
begin
Write('Filename: ');
readLn(fn);
Write('Save top? ');
readln(ans);
if ans = 'y' then 
  begin
  fid := FSEnter(fn);
  for i := 0 to 100 do
    FSBlkWrite(fid, i, MakePtr(ScreenSeg, i*256, pDirBlk));
  FSClose(fid, 101, 4096);
  end;

fid := FSLookUp(fn, i, j);
Write('Show picture starting with which block (<100)?');
Readln(j);
Write('Use new method? ');
readln(debug1);

MultiRead(fid, MakePtr(ScreenSeg, 14592, pDirBlk), j, i-j);

end

{$endc}

.

