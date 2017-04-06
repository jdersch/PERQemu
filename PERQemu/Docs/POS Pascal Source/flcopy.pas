{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FloppyCopy;

{------------------------------------------------------------------
{
{     FloppyCopy - Duplicate floppy disks.
{     J. P. Strait             26 Sep 80.
{     CopyRight (C) Three Rivers Computer Corporation, 1980, 1982, 1983.
{
{      Abstract:
{
{      FloppyCopy is used to duplicate floppy disks and is called
{      by routines Duplicate, DumpToDisk, and DumpToFloppy in
{      Module FloppyUtils. The routines Read Master and WriteCopy
{      prompt for insertion of the master floppy and blank floppys.
{      If Verify is specified as a switch, there will be
{      verification with a CRC.
{
{------------------------------------------------------------------}


{------------------------------------------------------------------
{      Change log:
{
{
{  3 Feb 84 V2.0  Dirk Kalp
{                 Fixed up Flop to try harder for Perq2 double density
{                 and to suppress errors.
{
{ 20 Oct 82 V1.2  August G. Reinig
{                 Changed ReadMaster and WriteCopy to use multi-sector
{                 floppy reads and writes.  Changed WriteCopy to use
{                 multiread.  Fixed bug in CheckifEnoughRoom dealing
{                 with FindPartition returning zero.  Changed the verify
{                 procedure to do direct compares of the two sets of data.
{
{ 14 Oct 82 V1.1  Roger Riggs
{                 Updated to support new I/O subsystem.  Added Interleave
{                 switch to control the order of phyical sectors during
{                 Format commands.
{
{      V3.0  BAM  12 May 82  Read Master checks to see if enough room on disk.
{      V2.3  SSJ  18 Mar 82  Changed every occurence of 'terminate'
{                               to FlTerminate.
{
{      V2.2  DCF   9 Feb 82  Added "insert formatted blank floppy" to
{                            user message in WriteCopy.
{
{      V2.1  DCF  27 Jan 82  Changed name to FloppyCopy to comform
{                            to Floppy naming conventions.
{
{      V2.0  DCF  23 Dec 81  Changed Program FloppyDup into Module
{                            FloppyCopy, to be imported by the
{                            POS D.6 - compatable Program Floppy.
{
{      V1.6  DLK   6 Nov 81  Allow user to confirm number of sides.
{      V1.5  DLK  29 Oct 81  (1) Modified to handle single/double density
{                                and single/double sided floppys.
{                            (2) Added procedure "FindSidesAndDensity".
{                            (3) Changed to always delete scratch files.
{
{      V1.4  DCF  10 Oct 81  (1) Included Virgil headers.
{                            (2) Default "no" answers to questions.
{                            (3) Deletes scratch files if user wants to.
{
{      V1.3  BAM   1 Jun 81  Change to use IO_Unit (OS D)
{
{      V1.2  JPS  23 Oct 80  Replace checksum that doesn't work with CRC
{                            that does (I hope).
{
{      V1.1  JPS  16 Oct 80  Add verify ability.
{
{      V1.0  JPS  26 Sep 80  Start program.
{------------------------------------------------------------------}




Exports
 imports FileUtils from FileUtils;

 procedure InitDup;
 procedure ReadMaster (N: PathName);
 procedure WriteCopy (N: PathName);
 procedure FLTerminate (N: PathName);
 procedure FinDup;

Private

 imports FloppyDefs from FloppyDefs;
 imports Memory from Memory;
 imports AllocDisk from AllocDisk;
 imports IO_Others from IO_Others;
 imports FileDefs from FileDefs;
 imports MultiRead from MultiRead;

const
  SecPerTrk = 26;
  Tracks = 77;
  SDWords = 64;
  DDWords = 128;
  BlocksPerFile = 128;
  MaxBlocksPerTransfer = 63; { large number of disk blocks which have an }
                             { integer byte count }

{$ifc DiskBufSize mod SDWords <> 0
then}{$Message = DiskBufSize not a multiple of SDWords.}
{$endc}
{$ifc DiskBufSize mod DDWords <> 0
then}{$Message = DiskBufSize not a multiple of DDWords.}
{$endc}

type
  DataArea = packed record case integer of
    1 : (DskBlocks : array[0..0] of array[0..DiskBufSize-1] of integer);
    2 : (DDSectors : array[0..0] of array[0..DDWords-1] of integer);
    3 : (SDSectors : array[0..0] of array[0..SDWords-1] of integer);
    4 : (Words     : array[0..0] of integer);
    end;
  pDataArea = ^DataArea;

var
  pData : pDataArea;     
  pVData : pDataArea;    { for verify purposes }
  outSeg: Integer;       { segment number of buffer for output }
  outFid: FileID;        { file id of current output file }
  F: Integer;            { file number }
  Status: IOStatPtr;
  MaxSectorsPerTransfer : integer;
  BlocksPerTransfer : integer;
  WordsPerSector: integer;
  BytesPerSector: integer;
  Files, MaxFiles: integer;
  Len: Integer;
  OurDens : DensityTypes;

 procedure NewFile (var N: PathName);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Append the letters A, B, C, etc. onto the name of
{     temporary scratch files. This creates files named
{     "Floppy.Scratch.A", "Floppy.Scratch.B", etc. as more
{      filesare needed.
{
{ Parameters:
{
{     N - name of temporary scratch files.
{
{-----------------------------------------------------------------}

 begin { NewFile }
  Len := Length (N);
  N [Len] := Chr(F + Ord('A'));
  F := F + 1;
 end { NewFile };



 procedure InitDup;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Set up the initial states needed for Duplication
{     of floppies. Includes allocation of buffers.
{
{-----------------------------------------------------------------}

 label 1;

 Handler FullMemory;

 begin

   BlocksPerTransfer := BlocksPerTransfer - 1;
   if BlocksPerTransfer = 0 then raise FullMemory;
   goto 1;

 end;

 begin { Initialize }

  DupSuccess := true;
  New(Status);
  Maxfiles := 0;

  BlocksPerTransfer := MaxBlocksPerTransfer;

1:CreateSegment(outSeg, 2*BlocksPerTransfer, 1, 2*BlocksPerTransfer);
  pData := MakePtr(outSeg, 0, pDataArea);  
  pVData := MakePtr(outSeg, BlocksPerTransfer*DiskBufSize, pDataArea );

 end { Initialize };



 procedure FinDup;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Deallocate the buffers that were needed for Duplication
{     of floppies.
{
{-----------------------------------------------------------------}
 begin { Deallocate buffers }
  DISPOSE(Status);
  DecRefCount(outSeg);
 end { FinDup };



 procedure FLTerminate (N: PathName);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Delete the temporary scratch files created for
{     Duplication. If user has requested 'NODELETE'
{     switch, do not delete the files.
{
{-----------------------------------------------------------------}
 var fileName : PathName;
 begin { FLTerminate }
  Len := Length (N);
  If DeleteScratch then
     for F := 0 to MaxFiles - 1 do
        begin
        N[Len] := Chr(F + Ord('A')); { Delete(N) }
        Filename := N;
        Fixfilename(Filename, true);
        FSDelete(Filename); { delete "floppy.scratch" files }
        end;
 end { FLTerminate };



 procedure Flop( Cmd: IOCommands;  pData: pDataArea
               ; StartSector: integer; SectorCnt : integer );

 var NumTries : integer;
     Address  : Double;
     Buffer   : IOBufPtr;
     ErrMsg   : string;
     SoftStatus : integer;
     ByteCnt  : integer;
     MaxRetry : integer;

 begin { Flop }

  Buffer := recast( pData, IOBufPtr );
  Address[0] := (StartSector mod SecPerTrk) + 1;
  Address[1] := StartSector div SecPerTrk;
  ByteCnt := SectorCnt*BytesPerSector;

  {}
  { Try harder for Perq2 double density.
  {}
  if (Dens = DDens) and (Cf_IOBoard  = Cf_EIO) then
     MaxRetry := 24
  else
     MaxRetry := 12;
  DupSuccess := false;
  NumTries := 0;
  repeat
     UnitIO( Floppy, Buffer, Cmd, ByteCnt, Address, nil, Status );
     NumTries := NumTries + 1;
     SoftStatus := Status^.SoftStatus;
     if SoftStatus = IOEIOC then
        DupSuccess := true        { we are done }
     else
       begin
       ErrMsg := IOErrString(SoftStatus);
       if SoftStatus = IOEDNW then
          begin
          Writeln('** Floppy Error: ', Errmsg);
          exit(Flop);
          end;
       if (Dens = DDens) and HidingDDensErrors and (NumTries < MaxRetry) then
          {}
          { i.e., don't report the error on double density unless
          { it's our last try.
          {}
       else
          begin
          Write('** Floppy Error: ', Errmsg);
          writeln( ' [SoftStatus = ', SoftStatus:1
                   , ', HardStatus = ', Status^.HardStatus:1:8, ']' );
          if NumTries < MaxRetry then
             writeln( 'Retry.....     Track: ', Address[1]:1
                      , '     Sector: ', Address[0]:1)
          else
             writeln( '** Aborted     Track: ', Address[1]:1
                    , '     Sector: ', Address[0]:1);
          end;
       if land( NumTries, 3 ) = 0 then  { reset the device every fourth error }
          UnitIO(Floppy,Buffer,IOReset,0,Address,nil,Status);
       end;
  until DupSuccess or (NumTries = MaxRetry);
   
 end { Flop };



Function CheckIfEnoughRoom( N: PathName; Req: Integer ): Boolean;
{-----------------------------------------------------------------
{
{ Abstract: Checks to see if there is enough room in the partition for N
{           for the copy.
{ Parameters: N - a file name containing a partition part.
{             tracks - the number of tracks to do.
{ Returns: true if enough room else false.
{
{-----------------------------------------------------------------}
  var partNumber: integer;
      room: boolean;
      fakeLong: Record case boolean of
                 true: (l: Long);
                 false: (f: FSBit32);
                End;
{$ifc MPOSVersion then}
      d: Disks;
      partTable: Parts;
{$endc}
  begin
  i := PosC(N, '>');
  partNumber := FindPartition(SubStr(N, 1, i));
  if partNumber = 0
  then begin
    writeln( 'The Partition name in ', N, ' is bad.' );
    CheckIfEnoughRoom := false;
    end
  else begin
      {$ifc MPOSVersion then}
       AllocInfo(d, partTable);
      {$endc}
       fakeLong.f := PartTable[partNumber].PartNumFree;
       room := fakeLong.l > req;
       if not room then
          begin
          WriteLn('** Partition ', PartTable[partNumber].PartName,
                  ' only has ',fakeLong.l:1,' blocks free, but ', req:1,
                  ' are needed.  Aborting.');
          end;
       CheckIfEnoughRoom := room;
       end;
  end;
   
Function Compare( DataPtr1: pDataArea; DataPtr2: pDataArea 
                ; DataBlocks: integer; BlockSize: integer ): boolean;
                
{---------------------------------------------------------------------}
{
{ Abstract
{       Compare two data areas, returning true if they are equal, false
{       otherwise.  THIS PROCEDURE DESTROYS THE SECOND DATA AREA!!
{
{ Notes
{       RasterOp the first data onto the second using Xor.  If the two
{       sets of data are equal, the second will now be zero.  RasterOp
{       each block of the second data area onto the first, using Or.
{       If any bit in the second data area is not zero, a bit will be set
{       in the first block of the second data area.  Now check the first
{       block of the second data area for all zeros.
{
{---------------------------------------------------------------------}

var
  i : integer;
  ScanLines : integer;
  OffSet : integer;
  LocalData : DataArea;           
  
begin
  
  ScanLines := BlockSize div 4;   { a block's rectangle is four words wide }
                                  { and ScanLines lines long }

  rasterop( RXor, 64, DataBlocks*ScanLines   { move DataArea1 to DataArea2 }
          , 0, 0, 4, DataPtr2
          , 0, 0, 4, DataPtr1 );

  OffSet := 0;                           { move blocks in DataArea2 to the }
  for i := 1 to DataBlocks -1 do begin   { first block in DataArea2 }  
    OffSet := OffSet + ScanLines;
    rasterop( ROr, 64, ScanLines
            , 0, 0, 4, DataPtr2
            , 0, Offset, 4, DataPtr2 );
    end;

  if BlockSize = SDWords { copy into localdata to avoid pointer dereferences } 
  then LocalData.SDSectors[0] := DataPtr2^.SDSectors[0]
  else LocalData.DDSectors[0] := DataPtr2^.DDSectors[0];
  
  Compare := true;
  {$R-}                   { now check first block of DataArea2 for zeros }
  for i := 0 to BlockSize - 1 do
    if LocalData.Words[i] <> 0 then Compare := false;
  {$R=}

end;

procedure ReportBadCompare( StartSector: integer; SectorCnt: integer );

var
  Track : integer;
  Sector : integer;
  temp : integer;

begin
     
  Track := StartSector div SecPerTrk;
  Sector := (StartSector mod SecPerTrk) + 1;
  write( 'Verify failed between Track ', Track:0 , ' Sector ', Sector:0 );

  Temp := StartSector + SectorCnt - 1;
  Track := Temp div SecPerTrk;   
  Sector := (Temp mod SecPerTrk) + 1;  
  writeln( ' and Track', Track:0 , ' Sector ', Sector:0 );

end;

procedure ReadMaster (N: PathName);

{-----------------------------------------------------------------
{
{ Abstract:
{
{     Read the master floppy and dump its files onto the
{     disk in temporary scratch files.
{
{ Parameters:
{
{     N - the filename that the user has specified for
{         the scratch files. If no name was specified, the
{         files are called "Floppy.Scratch".
{
{-----------------------------------------------------------------}

var 
  I, dum: Integer;
  Sectors : integer;
  StartSector : integer;
  SecPerBlk :integer;
  SectorsThisTransfer : integer;
  BlocksThisTransfer : integer;
  BlocksNeeded : integer;
  BlocksInFile : integer;
  BlocksWritten : integer;
  OffSet : integer; 
  msg: string[255];

label 1;

begin { ReadMaster }

  if Dens = SDens
  then WordsPerSector := SDWords
  else WordsPerSector := DDWords;
  BytesPerSector := 2*WordsPerSector;
  SecPerBlk := DiskBufSize div WordsPerSector;

  Sectors := (Tracks*Sides*SecPerTrk) - 1;
  
  if Verify                               { we need space for verify purposes }
  or (BlocksPerTransfer = MaxBlocksPerTransfer)  { we can't use more space }
  then MaxSectorsPerTransfer := SecPerBlk * BlocksPerTransfer
  else if 2*BlocksPerTransfer < MaxBlocksPerTransfer
       then MaxSectorsPerTransfer := SecPerBlk * 2 * BlocksPerTransfer
       else MaxSectorsPerTransfer := SecPerBlk * MaxBlocksPerTransfer;

  BlocksNeeded := (Sectors + SecPerBlk - 1) div SecPerBlk;
  BlocksWritten := 0;
  BlocksInFile := 0;

  Files := (BlocksNeeded + BlocksPerFile - 1) div BlocksPerFile;
  if Files > MaxFiles then Maxfiles := Files;
  F := 0;
  FSRemoveDots(N);
  if not CheckIfEnoughRoom( N, BlocksNeeded ) then begin
    ExitRoutine := True;
    exit( ReadMaster );
    end;

  LoadCurs;
  StartSector := 0;
  while StartSector <= Sectors do begin

   SectorsThisTransfer := Sectors - StartSector + 1;
   if SectorsThisTransfer > MaxSectorsPerTransfer
   then SectorsThisTransfer := MaxSectorsPerTransfer;

   BlocksThisTransfer := (SectorsThisTransfer + SecPerBlk - 1) div SecPerBlk;

   Flop( IORead, pData, StartSector, SectorsThisTransfer );
   if not DupSuccess then  goto 1;
   if verify then begin
     Flop( IORead, pVData, StartSector, SectorsThisTransfer );   
     if not DupSuccess then  goto 1; 
     if not Compare( pData, pVData, SectorsThisTransfer, WordsPerSector )
     then ReportBadCompare( StartSector, SectorsThisTransfer );
     end;
   StartSector := StartSector + SectorsThisTransfer;
   
   OffSet := 0;  { word offset into words just read off the floppy }
   for i := 0 to BlocksThisTransfer - 1 do begin { write floppy data to disk }

     if BlocksInFile mod BlocksPerFile = 0 then begin
       NewFile(N);
       writeln ('Writing file ',N, ' onto hard disk.');
       if confirm then begin
         outfid := FSLocalLookup(N,dum,dum);
         if outFID <> 0 then begin
           msg := ConCat(N, ' already exists on harddisk! Delete? ');
           if GetConfirm(NullIdleProc, false, msg, 2, switches) = 1
           then IOCursorMode(indepCursor)
           else begin
             ExitRoutine := True;
             goto 1;
             end;
           end;
         end;
       outFid := FSEnter(N); 
       BlocksInFile := 0;
       end;

     FSBlkWrite( outFid, BlocksInFile, MakePtr(outSeg, Offset, pDirBlk) );
     OffSet := OffSet + DiskBufSize;
     BlocksInFile := BlocksInFile + 1;
     BlocksWritten := BlocksWritten + 1;
     ComputeProgress( BlocksWritten, BlocksNeeded );

     if BlocksInFile mod BlocksPerFile = 0
     then FSClose(outFid, BlocksInFile, 4096);
     end;
   end;

1:if BlocksInFile mod BlocksPerFile <> 0   { close the last open file }
  then FSClose(outFid, BlocksInFile, 4096);

  QuitProgress;

end { ReadMaster };

 procedure WriteCopy (N: PathName);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Write the temporary scratch files onto a new floppy disk.
{
{ Parameters:
{
{     N - the name of the temporary files. If no name was
{         specified, the default is "Floppy.Scratch".
{
{-----------------------------------------------------------------}

label 1;

var
  dum : integer;
  BlocksFromFile : integer;
  BlocksNeeded : integer;
  SecPerBlk : integer;
  Sectors : integer;
  StartSector : integer;
  SectorsThisTransfer : integer;
  BlocksThisTransfer : integer;
  BlocksRead : integer;
  BlocksToRead : integer;  
  FullName : PathName;

begin { WriteCopy }

  if Dens = SDens
  then WordsPerSector := SDWords
  else WordsPerSector := DDWords;
  BytesPerSector := 2*WordsPerSector;
  SecPerBlk := DiskBufSize div WordsPerSector;

  Sectors := (Tracks*Sides*SecPerTrk) - 1;

  if Verify                               { we need space for verify purposes }
  or (BlocksPerTransfer = MaxBlocksPerTransfer)  { we can't use more space }
  then MaxSectorsPerTransfer := SecPerBlk * BlocksPerTransfer
  else if 2*BlocksPerTransfer < MaxBlocksPerTransfer
       then MaxSectorsPerTransfer := SecPerBlk * 2 * BlocksPerTransfer
       else MaxSectorsPerTransfer := SecPerBlk * MaxBlocksPerTransfer;

  BlocksNeeded := (Sectors + SecPerBlk - 1) div SecPerBlk;
  BlocksFromFile := 0;

  Files := (BlocksNeeded + BlocksPerFile - 1) div BlocksPerFile;
  if Files > MaxFiles then Maxfiles := Files;
  F := 0;

  LoadCurs;  
  StartSector := 0;
  while StartSector <= Sectors do begin

    SectorsThisTransfer := Sectors - StartSector + 1;
    if SectorsThisTransfer > MaxSectorsPerTransfer
    then SectorsThisTransfer := MaxSectorsPerTransfer;

    BlocksThisTransfer := (SectorsThisTransfer + SecPerBlk - 1) div SecPerBlk;

    BlocksRead := 0;
    while BlocksRead < BlocksThisTransfer do begin;

      if (BlocksFromFile mod BlocksPerFile) = 0 then begin
         FullName := N;
         NewFile(FullName);
         outFID := FSSearch(FSSysSearchList, FullName, dum, dum);
         if outFID = 0 then begin
           writeln('** DiskFile ', FullName:1, ' not found!');
           dupSuccess := false;
           goto 1;
           end;
         FSRemoveDots(FullName);
         writeln ('Reading file ',FullName, ' from the HardDisk.');
         BlocksFromFile := 0;
         end;

      BlocksToRead := BlocksThisTransfer - BlocksRead;
      if BlocksToRead > BlocksPerFile - BlocksFromFile
      then BlocksToRead := BlocksPerFile - BlocksFromFile;

      MultiRead( outFid, makeptr(outSeg, BlocksRead*DiskBufSize, pDirBlk)
               , BlocksFromFile, BlocksToRead );
      BlocksFromFile := BlocksFromFile + BlocksToRead;
      BlocksRead := BlocksRead + BlocksToRead;
      end;

    Flop( IOWrite, pData, StartSector, SectorsThisTransfer );
    if not DupSuccess then goto 1;
    if verify then begin
      Flop( IORead, pVData, StartSector, SectorsThisTransfer );   
      if not DupSuccess then  goto 1;
      if not Compare( pData, pVData, SectorsThisTransfer, WordsPerSector )
      then ReportBadCompare( StartSector, SectorsThisTransfer );
      end;
    StartSector := StartSector + SectorsThisTransfer;

    ComputeProgress( StartSector, Sectors );
    end;

1:QuitProgress;

end { WriteCopy }.
