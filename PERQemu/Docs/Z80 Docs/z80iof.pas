module Z80IOFloppy;

{*****************************************************************************}
{
{ Z80IOFloppy
{   User-level quick-and-dirty floppy support
{
{ Copyright (C) 1983, Richard H. Gumpertz
{                     Carnegie-Mellon University
{                     Computer Science Department
{                     Pittsburgh, Pennsylvania  15213
{
{ Abstract:
{
{-----------------------------------------------------------------------------}
{ History:
{    8 Nov 83  V1.7  RHG   Split Z80FloppyTransfer into Z80ReadFloppy and
{                          Z80WriteFloppy.
{
{    7 Nov 83  V1.6  RHG   Added Interleaving and Skews to Z80FormatFloppy.
{                          Added option of busy-looping on Z80Response instead
{                          of sleeping because it seems to run a bit better.
{                          Note, however, that it eats up much CPU time that
{                          might be spent running another process, so I left
{                          the default action to be sleeping.
{
{    4 Nov 83  V1.5  RHG   Split out of Z80IO.
{
{   31 Oct 83  V1.4  RHG   Added quick and dirty support for Floppy to Z80IO.
{*****************************************************************************}

{********************************}  EXPORTS  {********************************}

const
  TracksOnSide = 77;
  SectorsOnTrack = 26;
  SectorsOnSide = SectorsOnTrack * TracksOnSide;

procedure Z80FloppyInit(WantDoubleDensity: boolean);

function Z80ReadFloppy(UserBufPtr: pointer; SectorNumber: integer): boolean;

function Z80WriteFloppy(UserBufPtr: pointer; SectorNumber: integer): boolean;

function Z80FormatFloppy(Sides: integer; Interleave, CylinderSkew, SideSkew: integer): boolean;

{********************************}  PRIVATE  {********************************}

const                           { Compile-time options follow: }
  SleepUntilCompletion = true;  { True iff we should sleep in Z80Response }

imports Z80IOLowLevel from Z80IOLowLevel;

imports Z80IOPrivate from Z80IOPrivate;    { for BufferInfo.BufPtr, etc. }

const
  {$include Perq.QCodes.dfs}

var
  CurHead:      integer;
  CurCylinder:  integer;
  CurSector:    integer;
  CurDoubleDensity:boolean;

procedure Z80FloppyInit(WantDoubleDensity: boolean);

{*****************************************************************************}
{
{ Author:       Richard H. Gumpertz @ CMU
{ Abstract:     Reset the floppy drive
{ Parameters:   WantDoubleDensity - true if and only if using double-density.
{ Side effects: Raises appropriate exceptions to report errors.
{ Exceptions:   Z80KernelError, Z80NAK
{
{*****************************************************************************}

var
  args:         Z80Arguments;

begin

  if CheckInit(Floppy) <> Success
    then exit(Z80FloppyInit);

  if not OldZ80IO
    then Z80Send(Floppy, ZReset, nilArguments, 0, nilBuffer, 0);

  with args.FloppyConfig do
    begin
      SectorsPerTrack := SectorsOnTrack;
      if WantDoubleDensity
        then begin
          SectorSize := 1;      { log2(256 div 128) }
          GapLength := 14;
        end
        else begin
          SectorSize := 0;      { log2(128 div 128) }
          GapLength := 7;
        end;
      SkipDeletedAddressMark := false;
      DoubleDensity := WantDoubleDensity;
      AutoSide1Continue := false;

      CurDoubleDensity := WantDoubleDensity;
      BufferInfo[Floppy].ByteLimit := Shift(128, SectorSize);

      Z80Send(Floppy, ZConfig, args, 4, nilBuffer, 0);
    end;

{$ifc not EIOSendDataBug then}
  if not OldZ80IO
    then with args.FloppyArgs do
      begin
        HeadStepTime := 3;
        HeadUnloadTime := 192 div 16;  { More than a disk revolution (167 ms) }
        DisableDMA := false;
        HeadLoadTime := 36 div 2;

        Z80Send(Floppy, ZSpecify, args, 3, nilBuffer, 0);
      end;
{$endc}

  CurCylinder := -1;   { Force a Seek }

end;

{$ifc false then}

function FloppyStatus: integer;

{*****************************************************************************}
{
{ Author:       Richard H. Gumpertz @ CMU
{ History:      Major portions stolen from FloppyStatus in PerqFloppy
{ Abstract:     Fetch and decode the Floppy status
{ Return value: the appropriate error code, as defined in PerqIOErrors
{ Side effects: Raises appropriate exceptions to report errors.
{ Exceptions:   Z80KernelError
{
{*****************************************************************************}

var
  Status:       Z80ReturnStatus;

begin

  if CheckInit(Floppy) <> Success
    then begin
      Z80FloppyStatus := -1;
      exit(Z80FloppyStatus);
    end;

  Z80Send(Floppy, ZSense, nilArguments, 0, nilBuffer, 0);
  Status := recast(DevTab[Floppy]^.StatusIOCB^.data, Z80ReturnStatus);

  with Status do
    if StatusType = 0 then Z80FloppyStatus := IOEUEF
    else if (OldZ80IO and (StatusType = 1)) or ((not OldZ80IO) and (StatusType = 2)) then
      if DriveReady then Z80FloppyStatus := IOEUEF
      else Z80FloppyStatus := IOEDNR
    else if NotReady then Z80FloppyStatus := IOEDNR
    else if EquipmentCheck then Z80FloppyStatus := IOEUEF
    else if StatusType < 3 then
      if InterruptCode = InvalidCommand then Z80FloppyStatus := IOEILC
      else Z80FloppyStatus := IOEUEF
    else if Overrun then Z80FloppyStatus := IOEOVR
    else if NoAddrMark then
      if NoAddrMarkLoc = InHeader then Z80FloppyStatus := IOEMHA
      else Z80FloppyStatus := IOEMDA
    else if BadCylinder then Z80FloppyStatus := IOECMM
    else if EndCylinder or NoData then Z80FloppyStatus := IOESNF
    else if DataError then
      if DataErrorLoc = InHeader then Z80FloppyStatus := IOELHC
      else Z80FloppyStatus := IOEDAC
    else if InterruptCode = InvalidCommand then Z80FloppyStatus := IOEILC
    else Z80FloppyStatus := IOEUEF;

end;

{$endc}

function Z80FloppyOp(Buffer: PhysicalAddress; SectorNumber: integer; cmd: Z80Command): boolean;

{*****************************************************************************}
{
{ Author:       Richard H. Gumpertz @ CMU
{ History:      General idea stolen from FloppyOp in PerqFloppy
{ Abstract:     Perform a ZRead, ZWrite, ZFormat, or ZReset operation on the
{               floppy disk drive.
{ Parameters:   Buffer - the buffer to/from which data should be transferred
{               SectorNumber - the encoded sector number to/from which data
{                              should be transferred
{               cmd - Either ZRead, ZWrite, ZFormat, or ZReset
{ Return value: true if and only if the operation succeeds
{ Exceptions:   Z80KernelError
{
{*****************************************************************************}

var
  WantCylinder: integer;
  WantSector:   integer;
  WantHead:     integer;
  args:         Z80Arguments;
  s:            integer;

handler Z80NAK(Device: Z80Device);
  begin
    Z80FloppyOp := false;
    exit(Z80FloppyOp);
  end;

procedure DoReset;
  begin
    CurCylinder := -1;

    if not OldZ80IO
      then Z80Send(Floppy, ZReset, nilArguments, 0, nilBuffer, 0);

    with args.FloppyArgs do
      begin
        Unit := 0;
        Head := 0;

        Z80Send(Floppy, ZRecal, args, 1, nilBuffer, 0);
      end;

    repeat until Z80Response(Floppy, SleepUntilCompletion) <> 0;

    CurCylinder := 0;
    CurHead := 0;

  end;

procedure DoSeek;
  begin
    if CurCylinder < 0
      then DoReset;

    CurCylinder := -1;

    with args.FloppyArgs do
      begin
        Unit := 0;
        Head := WantHead;
        Cylinder := WantCylinder;

        Z80Send(Floppy, ZSeek, args, 2, nilBuffer, 0);
      end;

    repeat until Z80Response(Floppy, SleepUntilCompletion) <> 0;

    CurCylinder := WantCylinder;
    CurHead := WantHead;

  end;

begin

  if cmd = ZReset
    then begin
      DoReset;
      Z80FloppyOp := true;
      exit(Z80FloppyOp);
    end;

  WantSector := (SectorNumber mod SectorsOnTrack) + 1;
  WantCylinder := (SectorNumber div SectorsOnTrack) mod TracksOnSide;
  WantHead := SectorNumber div SectorsOnSide;

  if (CurCylinder <> WantCylinder) or (CurHead <> WantHead)
    then DoSeek;

  case cmd of
    ZRead,ZWrite:
      with args.FloppyArgs do
        begin
          Unit := 0;
          Head := WantHead;
          Sector := WantSector;
          ByteCount := BufferInfo[Floppy].ByteLimit;

          Z80Send(Floppy, cmd, args, 4, Buffer, ByteCount);
        end;
    ZFormat:
      with args.FloppyArgs do
        begin
          Unit := 0;
          Head := WantHead;
          SectorsPerTrack := SectorsOnTrack;
          Filler := 0;
          if CurDoubleDensity
            then GapLength := 54
            else GapLength := 27;

          Z80Send(Floppy, cmd, args, 4, Buffer, SectorsPerTrack*4);
        end;
    otherwise:
      begin
        Z80FloppyOp := false;
        exit(Z80FloppyOp);
      end
  end;

  repeat until Z80Response(Floppy, SleepUntilCompletion) <> 0;

  Z80FloppyOp := true;

end;

type
  SingleDensitySector = array[1 .. 128 div 2] of bit16;
  DoubleDensitySector = array[1 .. 256 div 2] of bit16;
  pBoth = record
    case boolean of
      false:(pSingleDensitySector: ^SingleDensitySector);
      true: (pDoubleDensitySector: ^DoubleDensitySector)
    end;

procedure MoveIt(Source: pBoth; Destination: pBoth);
  begin
    if CurDoubleDensity
      then Destination.pSingleDensitySector^ := Source.pSingleDensitySector^
      else Destination.pDoubleDensitySector^ := Source.pDoubleDensitySector^;
  end;    

function Z80ReadFloppy(UserBufPtr: pointer; SectorNumber: integer): boolean;

{*****************************************************************************}
{
{ Author:       Richard H. Gumpertz @ CMU
{ History:      General idea stolen from FloppyIO in PerqFloppy
{ Abstract:     Read Z80MaxByteCount(Floppy) bytes from a floppy.
{ Parameters:   UserBufPtr - points to buffer to which data should be
{                            transferred
{               SectorNumber - the encoded sector number from which data
{                              should be transferred
{ Return value: true if and only if the operation succeeded
{ Exceptions:   Z80KernelError
{
{*****************************************************************************}

var
  i:            integer;
  j:            integer;

label 0;

begin

  if CheckInit(Floppy) <> Success
    then begin
      Z80ReadFloppy := false;
      exit(Z80ReadFloppy);
    end;

  with BufferInfo[Floppy] do
    begin

      for i := 1 to 2
        do begin
          for j := 1 to 2
            do if Z80FloppyOp(BufPtr[NextBuffer].phys, SectorNumber, ZRead)
              then begin
                MoveIt(recast(BufPtr[NextBuffer],pBoth), recast(UserBufPtr,pBoth));
                Z80ReadFloppy := true;
                exit(Z80ReadFloppy);
              end;

          if not Z80FloppyOp(nilBuffer, 0, ZReset)
            then goto 0;
        end;

0:    Z80ReadFloppy := false;

    end;

end;

function Z80WriteFloppy(UserBufPtr: pointer; SectorNumber: integer): boolean;

{*****************************************************************************}
{
{ Author:       Richard H. Gumpertz @ CMU
{ History:      General idea stolen from FloppyIO in PerqFloppy
{ Abstract:     Write Z80MaxByteCount(Floppy) bytes to a floppy.
{ Parameters:   UserBufPtr - points to buffer from which data should be
{                            transferred
{               SectorNumber - the encoded sector number to which data
{                              should be transferred
{ Return value: true if and only if the operation succeeded
{ Exceptions:   Z80KernelError
{
{*****************************************************************************}

var
  i:            integer;
  j:            integer;

label 0;

begin

  if CheckInit(Floppy) <> Success
    then begin
      Z80WriteFloppy := false;
      exit(Z80WriteFloppy);
    end;

  with BufferInfo[Floppy] do
    begin

      MoveIt(recast(UserBufPtr,pBoth), recast(BufPtr[NextBuffer],pBoth));

      for i := 1 to 2
        do begin
          for j := 1 to 2
            do if Z80FloppyOp(BufPtr[NextBuffer].phys, SectorNumber, ZWrite)
              then begin
                Z80WriteFloppy := true;
                exit(Z80WriteFloppy);
              end;

          if not Z80FloppyOp(nilBuffer, 0, ZReset)
            then goto 0;
        end;

0:    Z80WriteFloppy := false;

    end;

end;

function Z80FormatFloppy(Sides: integer; Interleave, CylinderSkew, SideSkew: integer): boolean;

{*****************************************************************************}
{
{ Author:       Richard H. Gumpertz @ CMU
{ Abstract:     Format a floppy disk.
{ Parameters:   Sides - the number of sides on this disk
{               Interleave - the sector-interleaving factor (OldZ80IO forces 1)
{                            Successive logical sectors are placed on every
{                            Interleave'th physical sector.  Once formatted,
{                            the floppy hardware deals only in logical sectors.
{                            1 => 1,2,...,26
{                            2 => 1,14,2,15,...,13,26
{                            3 => 1,10,19,2,11,20,...,8,17,26,9,18
{                            4 => 1,14,8,21,2,15,9,22,...,6,19,13,26,7,20
{                            5 => 1,22,17,12,7,2,23,18,13,8,...,5,26,21,16,11,6
{                            etc.
{               CylinderSkew - successive cylinders can be shifted so that
{                              after an operation on the last sector of a
{                              cylinder and a seek to the next cylinder, the
{                              first sector of that next cylinder will reach
{                              the heads shortly.  This parameter controls that
{                              shift; it is measured from sector 1 of a track
{                              to sector 1 of the next track.  0 means no skew.
{               SideSkew - a similar skew except this determines the shift
{                          between side 1 and side 2 of each cylinder.
{ Restrictions: Interleave, CylinderSkew, and SideSkew are ignored under
{               OldZ80IO; the implicit values 1, 0, and 0 are used instead.
{ Return value: true if and only if the operation succeeded
{ Exceptions:   Z80KernelError
{
{*****************************************************************************}

var
  Cylinder:     integer;
  Side:         integer;
  Sector:       integer;
  i:            integer;

begin

  if CheckInit(Floppy) <> Success
    then begin
      Z80FormatFloppy := false;
      exit(Z80FormatFloppy);
    end;

  if not Z80FloppyOp(nilBuffer, 0, ZReset)
    then begin
      Z80FormatFloppy := false;
      exit(Z80FormatFloppy);
    end;

  with BufferInfo[Floppy], BufPtr[NextBuffer].ptr^ do
    for Cylinder := 0 to TracksOnSide-1
      do for Side := 0 to Sides-1
        do begin

          if not OldZ80IO
            then begin

              for i := 0 to SectorsOnTrack-1
                do with Doubles[i] do
                  begin
                    Byte0 := Cylinder;
                    Byte1 := Side;
                    Byte2 := 0;             { set to the sector number below }
                    if CurDoubleDensity
                      then Byte3 := 1       { N = log2(256 div 128) }
                      else Byte3 := 0;      { N = log2(128 div 128) }
                  end;

              i := (Cylinder * CylinderSkew) + (Side * SideSkew) - Interleave;
              for Sector := 1 to SectorsOnTrack
                do begin
                  i := (i + Interleave) mod SectorsOnTrack;
                  while Doubles[i].Byte2 <> 0
                    do i := (i + 1) mod SectorsOnTrack;
                  Doubles[i].Byte2 := Sector;
                end;

            end;

          if not Z80FloppyOp(BufPtr[NextBuffer].phys, (Side * TracksOnSide + Cylinder) * SectorsOnTrack, ZFormat)
            then begin
              Z80FormatFloppy := false;
              exit(Z80FormatFloppy);
            end;

        end;

  Z80FormatFloppy := true;

end.
