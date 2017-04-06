{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module ReadDisk;
{--------------------------------------------------------------------------
{
{ Abstract:
{
{   Module to Read and write to the disk using a buffer system

   Written by the CMU Spice Group
{
{   Copyright (c) 1981, 1982, 1983  Three Rivers Computer Corporation
{
--------------------------------------------------------------------------}

{--------------------------------------------------------------------------
 Versions:

     12 Jan 82  BAM   V1.5  Fixed FlushFail bugs in FlushAll, ForgetAll. 
      3 Dec 81  BAM   V1.4  Change FlushFail to provide more information. 
                             Change FlushBuffer to get the info.
     12 May 81  BAM   V1.3  Add exception for Flush failure
     20 Apr 81  JPS   V1.2  Make ReadDisk use its own segment (BufferSegment)
                            rather than DiskIO's (DiskSegment).
                            Set the BufferSegment and UnSwappable for virtual
                            memory.
     14 Apr 81  BAM   V1.1  Fixed ForgetAll to set Flags to false.
     19 Mar 81  BAM   V1.0  Changed Bit32 to FSBit32.
     17 Mar 81  GGR   V0.1  Changed number of buffers from 4 to 8.
     ?? ??? ??  CMU Spice Group  V0.0  Started
--------------------------------------------------------------------------}


{******************} exports {***************************}
imports DiskIO from DiskIO;

function  ReadDisk(addr : DiskAddr)     : ptrDiskBuffer;
function  ChangeDisk(addr : DiskAddr)   : ptrDiskBuffer;
function  ReadHeader(addr : DiskAddr)   : ptrHeader;
function  ChangeHeader(addr : DiskAddr) : ptrHeader;
procedure FlushDisk(addr : DiskAddr);
procedure WriteDisk(addr : DiskAddr; ptr : ptrDiskBuffer; hdptr : ptrHeader);
procedure WriteHeader(addr : DiskAddr; ptr : ptrDiskBuffer; hdptr : ptrHeader);

procedure InitBuffers;
function  FindDiskBuffer(dskaddr : DiskAddr; alwaysfind : boolean) : integer;
procedure ReleaseBuffer(indx : integer);
procedure FlushBuffer(indx : integer);
procedure FlushAll;
procedure ChangeBuffer(indx : integer);
procedure ChgHdr(indx : integer);
procedure UseBuffer(indx,numtimes : integer);
function  BufferPointer(indx : integer) : ptrDiskBuffer;
function  HeaderPointer(indx : integer) : ptrHeader;
function  ReadAhead(addr : DiskAddr)     : ptrDiskBuffer;
procedure ForgetAll;

Exception FlushFail(msg: String; operation: DiskCommand; addr: DiskAddr;
                       softStat: integer);
{--------------------------------------------------------------------------
  Abstract: Raised when the system is unable to flush out a buffer.  The buffer
            is marked as flushed out, however, so the error will not repeat
            the next time a buffer needs to be flushed
  Parameters: Same as DiskFailure (in DiskIO)
  Resume: ALLOWED, but has no effect (procedure will return normally as if
           flush had been successful)
--------------------------------------------------------------------------}

{******************} private {***************************}
imports Memory from Memory;

const
  NUMBUFFERS    = 8;
  DEBUG         = false;

type
  
  BufferEntries = record
                    HdrChanged  : boolean;
                    Changed     : boolean;
                    UseCnt      : integer;
                    Addr        : DiskAddr;
                    BuffPtr     : ptrDiskBuffer;
                    HeadPtr     : ptrHeader
                  end;

  BufferTable   = array [1..NUMBUFFERS] of BufferEntries;

var
  Table : BufferTable;
  BuffersInitialized : integer;
  BufferSegment: integer;


Function  ReadDisk(addr : DiskAddr) : ptrDiskBuffer;
{--------------------------------------------------------------------------
  Abstract: Read the block specified and return the ptr of the buffer read
              into
  Parameters: addr is the address of the block to read
  Returns: ptr to buffer read into
--------------------------------------------------------------------------}
  var 
    indx : integer;

  begin
    indx := FindDiskBuffer(addr,true);
    UseBuffer(indx,1);
    ReadDisk := BufferPointer(indx);
  end;
  

Function ReadAhead(addr : DiskAddr) : ptrDiskBuffer;
{--------------------------------------------------------------------------
  Abstract: Identical to ReadDisk
  Parameters: addr is the address of the block to read
  Returns: ptr to buffer read into
--------------------------------------------------------------------------}
  var 
    indx,indx2  : integer;
    hdr         : ptrHeader;

  begin
    indx := FindDiskBuffer(addr,true);
    UseBuffer(indx,1);
    ReadAhead := BufferPointer(indx);
    
    {
    indx := FindDiskBuffer(addr,false);
    if indx <> 0 then
      begin
        UseBuffer(indx,1);
        ReadAhead := BufferPointer(indx);
      end
    else
      begin
        indx := FindDiskBuffer(addr,true);
        UseBuffer(indx,2);
        ReadAhead := BufferPointer(indx);
        hdr := HeaderPointer(indx);
        if hdr^.NextAdr <> DBLZERO then 
          indx2 := FindDiskBuffer(hdr^.NextAdr,true);
        if indx2 = indx then ReadAhead := ReadDisk(addr);
      end;
    }
  end;


Function ReadHeader(addr : DiskAddr) : ptrHeader;
{--------------------------------------------------------------------------
  Abstract: Reads block specified and returns a ptr to a buffer describing
              its header
  Parameters: addr is the address of the block to read
  Returns: ptr to header read into
--------------------------------------------------------------------------}
  var 
    indx : integer;

  begin
    indx := FindDiskBuffer(addr,true);
    UseBuffer(indx,1);
    ReadHeader := HeaderPointer(indx);
  end;


Function  ChangeDisk(addr : DiskAddr) : ptrDiskBuffer;
{--------------------------------------------------------------------------
  Abstract: Reads block specified and returns a ptr to its data; in addition,
              mark file as changed so flush will write it out
  Parameters: addr is the address of the block to read
  Returns: ptr to buffer holding the block read into
--------------------------------------------------------------------------}
  var
    indx : integer;
    
  begin
    indx := FindDiskBuffer(addr,true);
    UseBuffer(indx,1);
    ChangeBuffer(indx);
    ChangeDisk := BufferPointer(indx);
  end;


Function  ChangeHeader(addr : DiskAddr) : ptrHeader;
{--------------------------------------------------------------------------
  Abstract: Reads block specified and returns a ptr to its data; in addition,
              mark file as header changed so flush will write it out using
              IOWriteFirst
  Parameters: addr is the address of the block to read
  Returns: ptr to header read into
--------------------------------------------------------------------------}
  var 
    indx : integer;

  begin
    indx := FindDiskBuffer(addr,true);
    UseBuffer(indx,1);
    ChgHdr(indx);
    ChangeHeader := HeaderPointer(indx);
  end;


Procedure FlushDisk(addr : DiskAddr);
{--------------------------------------------------------------------------
   Abstract: Removes block specified from buffer system and writes it out if
              changed.  If addr not in buffer then NO-OP
   Parameters: addr is block to flush
--------------------------------------------------------------------------}
  var 
    indx : integer;

  begin
    indx := FindDiskBuffer(addr,false);
    if indx <> 0 then FlushBuffer(indx);
  end;


Procedure WriteDisk(addr : DiskAddr; ptr : ptrDiskBuffer; hdptr : ptrHeader);
{--------------------------------------------------------------------------
  Abstract: Writes out a block using DskWrite.  If block for addr is
             in a buffer then Release it first.
  Parameters: addr - the address of the block to write
              ptr - points to a buffer of data
              hdptr  - points to a buffer of header
--------------------------------------------------------------------------}
  var
    indx : integer;

  begin
    indx := FindDiskBuffer(addr,false);
    if indx = 0 then DiskIO(addr,ptr,hdptr,DskWrite)
    else 
      begin
        ReleaseBuffer(indx);
        DiskIO(addr,ptr,hdptr,DskWrite);
      end;
  end;


Procedure WriteHeader(addr : DiskAddr; ptr : ptrDiskBuffer; hdptr : ptrHeader);
{--------------------------------------------------------------------------
  Abstract: Writes out a block using DskFirstWrite.  If block for addr is
             in a buffer then Release it first.
  Parameters: addr is the address of the block to write
              ptr points to a buffer of data
              hdptr points to a buffer of header
--------------------------------------------------------------------------}
  var
    indx : integer;

  begin
    indx := FindDiskBuffer(addr,false);
    if indx = 0 then DiskIO(addr,ptr,hdptr,DskFirstWrite)
    else 
      begin
        ReleaseBuffer(indx);
        DiskIO(addr,ptr,hdptr,DskFirstWrite);
      end;
  end;


Procedure InitBuffers;
{--------------------------------------------------------------------------
  Abstract: Initializes the buffer system
--------------------------------------------------------------------------}
  var
    i : integer;
    cheat : DiskCheatType;

  begin
    if BuffersInitialized = #12345 then exit(InitBuffers);
    BuffersInitialized := #12345;
    
    i := NumBuffers + (NumBuffers * WordSize(Header) + 255) div 256;
    CreateSegment(BufferSegment,i,1,i);
    SetMobility(BufferSegment,UnSwappable);
    for i := 1 to NUMBUFFERS do
      begin  
        with Table[i] do
        begin
          Addr    := DBLZERO;
          new(BufferSegment,256,BuffPtr);
          cheat.Lng := Recast(BuffPtr,FSBit32);
          if DEBUG then
            WriteLn('BuffPtr[',i,'] = ',cheat.Dbl[0],' ',cheat.Dbl[1]);
        end;
      end;
    for i := 1 to NUMBUFFERS do
      begin  
        with Table[i] do
        begin
          new(BufferSegment,4,HeadPtr);
          cheat.Lng := Recast(HeadPtr,FSBit32);
          if DEBUG then
            WriteLn('HeadPtr[',i,'] = ',cheat.Dbl[0],' ',cheat.Dbl[1]);
        end;
      end;
  end;


function FindDiskBuffer(dskaddr : DiskAddr; alwaysfind : boolean) : integer;
{--------------------------------------------------------------------------
  Abstract: Finds the buffer that contains the data for block dskAddr.
  Parameters: dskAddr  - is address to find buffer for
              alwaysFind tells whether to read in if not found
  Returns: Index of buffer found or zero if not there
--------------------------------------------------------------------------}
  var
    i,cnt,l : integer;

  begin
    cnt := 32000;
    for i := 1 to NUMBUFFERS do
      begin
        with Table[i] do
        begin
          if Addr = dskaddr then
            begin
              if DEBUG then WriteLn('FindBuffer: return value = ',i);
              FindDiskBuffer := i;
              exit(FindDiskBuffer);
            end;
          if alwaysfind then 
            begin
              if Addr = DBLZERO then 
                begin
                  cnt := -1;
                  l := i;
                end
              else
                begin
                  if cnt > UseCnt then 
                    begin
                      cnt := UseCnt;
                      l := i; 
                    end;
                  if UseCnt > 0 then UseCnt := UseCnt - 1;
                end;
            end;
        end;
      end;
    
    if not alwaysfind then
      begin
        if DEBUG then WriteLn('FindBuffer: return value = ',0);
        FindDiskBuffer := 0;
        exit(FindDiskBuffer);
      end;
    
    with Table[l] do
    begin
      if DEBUG then WriteLn('Flushing entry addr = ',AddrToField(Addr));
      if (HdrChanged or Changed) then 
        begin
          if HdrChanged then DiskIO(Addr,BuffPtr,HeadPtr,DskFirstWrite)
          else DiskIO(Addr,BuffPtr,HeadPtr,DskWrite);
        end;

      HdrChanged := false;
      Changed    := false;
      Addr       := DBLZERO;
      DiskIO(dskaddr,BuffPtr,HeadPtr,DskRead);
      Addr       := dskaddr;
      UseCnt     := 0;      
    end;   

    if DEBUG then WriteLn('FindBuffer: return value = ',l);
    
    FindDiskBuffer := l;
  end;


Procedure ReleaseBuffer(indx : integer);
{--------------------------------------------------------------------------
  Abstract: Mark the table entry as unused.
  Parameters: indx  - is entry to mark
--------------------------------------------------------------------------}
  begin
    with Table[indx] do
    begin
      Addr := DBLZERO;
    end;
  end;


Procedure FlushBuffer(indx : integer );
{--------------------------------------------------------------------------
  Abstract: Write out the data for the buffer indx if changed and then mark
             the buffer as not changed.
  Parameters: indx  - is buffer to flush
  Errors: FlushFail  - is raised if cannot Flush a buffer due to a write error
--------------------------------------------------------------------------}
  Handler DiskFailure(msg: String; operation: DiskCommand; addr: DiskAddr;
                       softStat: integer);
     begin
     Table[indx].HdrChanged := false;
     Table[indx].Changed := false;
     Raise FlushFail(msg, operation, addr, softStat);
     exit(FlushBuffer);  {in case some one resumes from the FlushFail};
     end;
  begin
    if DEBUG then WriteLn('FlushBuffer: indx = ',indx);
    with Table[indx] do
      begin
      if (HdrChanged or Changed) then 
        begin
          if HdrChanged then DiskIO(Addr, BuffPtr, HeadPtr,
                                        DskFirstWrite)
          else DiskIO(Addr,BuffPtr,HeadPtr,DskWrite);
        end;
      HdrChanged := false;
      Changed := false;
      end;
  end;
  

Procedure FlushAll;
{--------------------------------------------------------------------------
  Abstract: Writes out the data for all the buffers and then mark them all as
             unchanged.
  Errors: FlushFail  - is raised if cannot Flush a buffer due to a write error.
           Does not stop at first error, but goes and tries all buffers before
           raising the exception
--------------------------------------------------------------------------}
  var
    i : integer;
    flushLost: boolean;
    o: DiskCommand;
    a: DiskAddr;
    s: integer;
    
  Procedure DoFlush(i: integer);
    Handler FlushFail(msg: String; operation: DiskCommand; addr: DiskAddr;
                       softStat: integer);
       begin
       flushLost := true;
       o := operation;
       a := addr;
       s := softStat;
       end;
    begin
    FlushBuffer(i);
    end;
    
  begin
    flushLost := false;
    for i := 1 to NUMBUFFERS do
      begin  
        DoFlush(i);
      end;
  if flushLost then Raise FlushFail('Flush ALL',o,a,s);
  end;
  

Procedure ForgetAll;
{--------------------------------------------------------------------------
  Abstract: Writes out the data for all the buffers and then mark them all as
             unused.
  Errors: FlushFail  - is raised if cannot Flush a buffer due to a write error.
           Does not stop at first error, but goes and tries all buffers before
           raising the exception
--------------------------------------------------------------------------}
  var
    i : integer;
    flushLost: boolean;
    o: DiskCommand;
    a: DiskAddr;
    s: integer;
    
  Procedure DoFlush(i: integer);
    Handler FlushFail(msg: String; operation: DiskCommand; addr: DiskAddr;
                       softStat: integer);
       begin
       flushLost := true;
       o := operation;
       a := addr;
       s := softStat;
       end;
    begin
    FlushBuffer(i);
    end;
    
  begin
    flushLost := false;
    for i := 1 to NUMBUFFERS do
      begin  
        DoFlush(i);
        Table[i].HdrChanged := false;
        Table[i].Changed := false;
        Table[i].UseCnt := 0;
        Table[i].Addr := DBLZERO;
      end;
  if flushLost then Raise FlushFail('Forget ALL',o,a,s);
  end;


Procedure ChangeBuffer(indx : integer);
{--------------------------------------------------------------------------
  Abstract: Mark a buffer as changed.
  Parameters: indx  - is buffer to mark
--------------------------------------------------------------------------}
  begin
    Table[indx].Changed := true;
  end;


Procedure ChgHdr(indx : integer);
{--------------------------------------------------------------------------
  Abstract: Mark a buffer as having its header changed.
  Parameters: indx  - is buffer to mark
--------------------------------------------------------------------------}
  begin
    Table[indx].HdrChanged := true;
  end;


Procedure UseBuffer(indx,numtimes : integer);
{--------------------------------------------------------------------------
  Abstract: Mark a buffer as used.
  Parameters: indx  - is buffer to mark
               numTimes -  the number to incrememt use count by
--------------------------------------------------------------------------}
  begin
    Table[indx].UseCnt := Table[indx].UseCnt + numtimes;
  end;


Function  BufferPointer(indx : integer) : ptrDiskBuffer;
{--------------------------------------------------------------------------
  Abstract: return the bufferPtr for a buffer.
  Parameters: indx  - is buffer
  Returns: Ptr to buffer
--------------------------------------------------------------------------}
  begin
    BufferPointer := Table[indx].BuffPtr;
  end;


Function  HeaderPointer(indx : integer) : ptrHeader;
{--------------------------------------------------------------------------
  Abstract: return the header Ptr for a buffer.
  Parameters: indx  - is buffer
  Returns: ptr to header
--------------------------------------------------------------------------}
  begin
    HeaderPointer := Table[indx].HeadPtr;
  end.
