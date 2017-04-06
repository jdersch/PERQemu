Program FixPart;
{-------------------------------------------------------------
{ FixPart - Brad A. Myers
{ Copyright (C) 1981, 1982, 1983 - The Three Rivers Computer Corporation
{ Copyright (C) 1983 - Perq Systems Corporation.
{
{ Abstract:
{   This is an experimental program to fix up the partition information
{   blocks.  It should eventually be incorporated into the scavenger.
{-------------------------------------------------------------}

{$Version V1.3 for POS}
{-------------------------------------------------------------
{ Change Log:
{
{  2 Feb 84  V1.3  Sandeep Johar
{ User SetupDiskParams from DiskParams.pas
{
{ 23-Nov-83  V1.2  Scott Brown
{ Rename local procedure SetUpDiskParams to DoSetUpDiskParams to avoid
{ a conflict with procedure SetUpDiskParams exported by module DiskParams.
{
{ 18 Oct 83  V1.1  Sandeep Johar     Made it get 5.25' disk information.
{
{  5 Oct 83   V1.0  Sandeep Johar    Was not handling micropolis (EIO) 
{                                      correctly.
{ 13 apr 83  V0.9  Chris Hughes      Modify for cio micropolis support:
{       RoundDown and RoundUp modified as in Partition.
{       DiskType ciomicrop used.
{
{ 18 Feb 83  V0.8  Brad A. Myers     Tell what kind of disk it thinks it is;
{                                      Use longs instead of negative numbers;
{                                      Catch exceptions.
{ 01 Jan 83  V0.7  August G. Reinig  Added code to deal with the micropolis
{                                      disk on the EIO system.
{ 19-May-82  V0.6  Brad A. Myers     Fix so 0 is legal in CheckAndReportAddr. 
{                                      Allow adding a partition at the end.
{ 16-Dec-81  V0.5  Brad A. Myers     Fix to not skip partition numbers. 
{                                      Print more info for bad part #.
{ 10-Aug-81  V0.4  Brad A. Myers     Fix to handle bad number of free blocks
{                                       in a partition and to try to discover
{                                       what kind of disk it is
{ 30-Jun-81  V0.3  Brad A. Myers     Turn off swapping
{ 27-May-81  V0.2  Brad A. Myers     Made less succeptible to disk failures
{  8-May-81  V0.1  Brad A. Myers     created
{-------------------------------------------------------------}


Const Version = 'V1.3';

imports DiskIO from DiskIO;
imports AllocDisk from AllocDisk;
imports Memory from Memory;
imports System from System;
imports Except from Except;
imports DiskDef from DiskDef;
imports IO_Unit from IO_Unit;  {using IO24MByte}
imports VolumeSystem from VolumeSystem;
imports diskutility from diskutility;
imports diskparams from diskparams;
imports stream from stream;

Type PName = Packed Array [1..8] of char;
     HowBad = Packed Record case boolean of 
                       true: (OK: integer);
                       false: (noRead: Boolean;
                               disagrees: boolean;
                               notcyl: boolean;
                               startBad: boolean;
                               nameBad: boolean;
                               endBad: boolean;
                               badFreeCount: Boolean);
                       end;
     
var Configuration: DeviceType;
    DiskInfoBlk: FSBit32;
    DiskParts: Array[0..64] of DiskAddr; {parts according to Disk Info blk}
    PartParts: Array[0..63] of DiskAddr; {parts according to part Info blks}
    partBad : array[0..63] of HowBad;
    diskBad: boolean;
    Buf: ptrDiskBuffer;
    labelPtr: ptrHeader;
    i, j, config, disk, TypeDisk, last : integer;
    cheat: DiskCheatType;
    firstPart: DiskAddr;
    addr, next, tempAddr: DiskAddr;
    LastAddr: DiskAddr;
    ok, DIBRead, okPIB : boolean;
    remainder: FSBit32;
    WordsPerBlock : FSBit32;
    nameOK: boolean;
    ans: String[1];
    s: String;
    c: Char;
    freeCount: FSBit32;
    dskType: IntDiskKinds;

    numheads      : integer;
    numcylinders  : integer;
    secpertrk     : integer;
    WriteCompCyl  : Integer;
    bootsize      : integer;
    diskname      : string;

    
Function ReadUnsigned(def: Integer): Integer;
  var l: Record case boolean of
               true: (l: Long);
               false: (high, low: Integer);
               end;
   begin
   if eoln then ReadUnsigned := def
   else begin
        Read(l.l);
        ReadUnsigned := l.low;
        end;
   readln;
   end;

Function CheckName(name: String): boolean;
   var i: integer;
       c: Char;
   begin
   CheckName := true;
   for i := 1 to length(name) do
      if not (name[i] in ['A'..'Z','a'..'z','0'..'9','.']) then
              CheckName := false;
   end;

Function WritePart(name: PName): boolean;
  var i: integer;
      c: char;
      haveSpace: boolean;
  begin
  haveSpace := false;
  WritePart := true;
  for i := 1 to 8 do
    begin
    c := name[i];
    if c = ' ' then haveSpace := true
    else if c in ['A'..'Z','a'..'z','0'..'9','.'] then
              if haveSpace then WritePart := false
              else
         else WritePart := false;
    Write(chr(LOr(ord(c), #200)));
    end;
  end;

Function DoubleGtr(d1, d2: FSBit32): boolean;  {returns true if d1 > d2}
   var temp: myDouble;
   begin
   temp.ptr := DoubleSub(d2, d1);
   DoubleGtr := temp.msw < 0;
   end;

procedure RoundDown( var Addr, Remainder: FSBit32 );

  handler VBNOutofRange( VID : VolId; VBN : VolBlockNumber );
  begin end;  { catch exception caused by rounding of addrs > max addr }

  var OldAddr: FSBit32;
      Cheat: DiskCheatType;
      Disk: Integer;
  begin { RoundDown }
    Cheat.Lng := LogAddrToPhysAddr(Addr);
    Disk := WhichDisk(Addr);
    OldAddr := Addr;
    case Disk of
      0: if EIOFlag or 
            (ciodisktype = ciomicropolis) Or
            (Configuration = Generic5Inch)
         then Cheat.Dbl[0] := 0
         else Cheat.Dbl[0] := LAnd(Cheat.Dbl[0], #177400);
      1: Cheat.Dbl[0] := 3
      end;
    Addr := PhysAddrToLogAddr(Disk, Cheat.Lng);
    Remainder := DoubleSub( DoubleInt(AddrToField(OldAddr)),
                            DoubleInt(AddrToField(Addr)) )
  end { RoundDown };

procedure ShowPart(buf: ptrDiskBuffer);
  var dum: boolean;
      i: integer;
  Handler OvflLi;
  {------------------------------------------------------------------
    Abstract: This may be raised by the IntDouble for free number;
               print out a message
  ------------------------------------------------------------------}
    begin
    WriteLn('**Illegal**');
    exit(ShowPart);
    end;
  
  begin
  Write('Partition "');
  dum := WritePart(RECAST(buf^.PartName, PName));
  Write('": Start = ',AddrToField(buf^.PartStart):6:-10,' End = ',
                      AddrToField(buf^.PartEnd):6:-10,' Free = ');
  i := IntDouble(Buf^.numFree);
  WriteLn(i);
  end;

Procedure AssignName(buf: ptrDiskBuffer; name: String);
  var i, len: integer;
  begin
  len := Length(name);
  if len > 8 then len := 8;
  for i := 1 to len do
    buf^.partName[i] := name[i];
  for i := len+1 to 8 do
    buf^.partName[i] := ' ';
  end;

procedure RoundUp( var Addr, Remainder: FSBit32 );
{------------------------------------------------------------------------------
  Abstract: Rounds up Addr to nearest track boundary.  Sets Addr to new value
------------------------------------------------------------------------------}
  var Add: Integer;
      OldAddr: FSBit32;
      nheads,
      nsecpertrk,
      ncyl : integer;

  begin { RoundUp }
    case WhichDisk(Addr) of
      0: If (Configuration = Winch24) And (IO24MByte) 
           then Add := 240
           else If Configuration = Winch12 Then Add := 120
             else if Configuration = Winch24 then Add := 120
               Else If Configuration = Generic5Inch then 
                 begin
                 GetDiskParameters(nHeads, nsecpertrk, ncyl);
                 Add := nheads * nsecpertrk;
                 end;
      1: Add := 6
      end;
    OldAddr := Addr;
    Addr := DoubleAdd(Addr, DoubleMul(DoubleInt(Add-1), WordsPerBlock ));
    RoundDown(Addr,Remainder);
    Remainder := DoubleSub( DoubleInt(AddrToField(Addr)),
                            DoubleInt(AddrToField(OldAddr)) )
  end { RoundUp };

Function OKAddr(addr: DiskAddr; zeroOk: Boolean): Boolean;
  Handler OvflLi;
  {------------------------------------------------------------------
    Abstract: This may be raised by RoundUp for bad addresses.
  ------------------------------------------------------------------}
    begin
    OkAddr := false;
    exit(OKAddr);
    end;

   var temp: DiskAddr;
   begin
   if addr = DBLZero then
       begin
       OKAddr := zeroOK;
       exit(OKAddr);
       end;
   temp := addr;
   RoundUp(temp, remainder);
   OKAddr :=  (temp = addr);
   end;


Function Ask(prompt, def: String): Boolean;
{-----------------------------------------------------
 Abstract: Asks a question
 Parameters: prompt is prompt for question;
             def is default answer
 Returns: True if answer is YES, false if NO
-------------------------------------------------------}
  var s: string;
      gotAnswer: boolean;
  begin
  Write(prompt);
  if def <> '' then write(' [',def,']');
  write('? ');
  gotanswer := false;
  repeat
    readln(s);
    if s = '' then s := def;
    if length(s) > 0 then
      gotAnswer := s[1] in ['y','Y','n','N'];
    if not gotAnswer then
       begin
       Write('Yes or No');
       if def <> '' then Write(' [',def,']');
       Write('? ');
       end;
  until gotAnswer;
  ask := s[1] in ['y','Y'];
  end;


Function CheckAndReportAddr(addr: DiskAddr; zeroOk, sub1, endOK: Boolean): boolean;
  var up, down, temp: DiskAddr;
  begin
  temp := addr;
  if sub1 then temp := DoubleSub(temp, WordsPerBlock);
  if (temp <> DblZero) and 
     (not (endOK and (temp = DoubleAdd(LastAddr, WordsPerBlock))))
     and (DoubleGtr(temp, LastAddr)) then
       begin
       CheckAndReportAddr := false;
       up := LastAddr;
       if not sub1 then RoundUp(up, temp);
       WriteLn('** Addr is too big.  Maximum is ',AddrToField(up):1:-10,' **');
       end
  else if not OKAddr(addr, zeroOK) then
        begin
        CheckAndReportAddr := false;
        down := addr;
        up := addr;
        RoundUp(up, temp);
        RoundDown(down, temp);
        if sub1 then
              begin
              down := DoubleSub(down, WordsPerBlock);
              up := DoubleSub(up, WordsPerBlock);
              end;
        WriteLn('** Addr is not legal.  Next Higher = ',AddrToField(up):1:-10,
                 ', next lower = ',AddrToField(down):1:-10,' **');
        end
  else begin
       CheckAndReportAddr := true;
       exit(CheckAndReportAddr);
       end;
  end;

Function CheckFree(start, nextStart, free: FSBit32): Boolean;
{-----------------------------------------------------
 Abstract: Checks to see if number of free blocks is less than the size of the
           partition
 Parameters: Start is the start of the partition.
             nextStart is the start of the next partition.
             free is the number of free blocks.
 Returns: true if free < end-start, else false
-------------------------------------------------------}
  Handler OvflLi;
    begin
    CheckFree := false;
    exit(CheckFree);
    end;
  
  begin
  CheckFree := IntDouble(free) < AddrToField(nextStart) - AddrToField(start);
  end;

Procedure MyDisplayPartitions;
{-----------------------------------------------------
 Abstract: Displays information about the current partitions on the screen
 Environment: Assumes PartTable and DiskTable set up;
 Calls: AddrToField; IntDouble, WriteLn;
-------------------------------------------------------}
  var
    i, j : integer;
  
label 1;
  Handler OvflLi;
  {------------------------------------------------------------------
    Abstract: This may be raised by the IntDouble for free number;
               print out a message and resume.
  ------------------------------------------------------------------}
    begin
    WriteLn('**Illegal**');
    goto 1;
    end;

  begin
  i := 0;
  with DiskTable[disk] do
     begin
     Write('Device #',disk:1);
     WriteLn('  Device name: ',RootPartition);
     for j := 1 to MAXPARTITIONS do
            with PartTable[j] do
                  if PartInUse and (PartDevice = disk) then
                        begin
                        WriteLn('      #',i:1,' ',PartName:8,': Start = ',
                                    AddrToField(PartStart):6:-10,' End = ',
                                    AddrToField(PartEnd):6:-10,' Free = ',
                                    IntDouble(PartNumFree));
                    1:  i := i+1;
                        end;
     end;
  end;


label 2;

begin

WriteLn;
WriteLn('FixPart ',version);
WriteLn;
WriteLn('************************************************************');
WriteLn('  WARNING WARNING WARNING WARNING WARNING WARNING WARNING');
WriteLn('   !!!!!!! This program is for expert use only!!!!!!! ');
WriteLn(' It is an experimental program to fix the partition and disk');
WriteLn('  information blocks');
WriteLn('  WARNING WARNING WARNING WARNING WARNING WARNING WARNING');
WriteLn('************************************************************');
WriteLn;
Write('Type Control-C now to abort or CR to continue: ');
ReadLn;
WriteLn;
 

if SwappingAllowed then
     begin
     SavedSwapID := SwapID;
     ShouldReEnableSwapping := true;
     DisableSwapping;
     end;

NEW(0, 256, buf);
NEW(0, 4, labelPtr);

diskBad := false;
WordsPerBlock := DoubleInt(256);

for i := 0 to 63 do
     begin
     DiskParts[i] := DBLZero;
     PartParts[i] := DBLZero;
     PartBad[i].OK := 0;
     end;

DIBRead := true;

repeat
  Write('Fix harddisk (H) or Floppy (F)? ');
  ReadLn(c);
  if c = 'f' then c := 'F'
  else if c = 'h' then c := 'H';
until (c = 'H') or (c = 'F');

if c = 'F' then Configuration := FloppyDouble
else Configuration := Winch24;

if Configuration = Winch24 then
  begin
  TypeDisk := DISKBITS;
  
  Write('This seems to be a ');
  
  dskType := GetIntDiskKinds;
  
  case dskType of
     Shugart14: begin
                 Write('SHUGART 14-Inch');
                 Configuration := Winch12;
                end;
     Mic8     : begin
                 Write('MICROPOLIS 8-Inch');
                 configuration := ciomicrop
                end;
     Mic5     : begin
                 Write('5.25-Inch');
                 configuration := Generic5Inch;
                end;
                
     Otherwise: Write('*** ILLEGAL TYPE ***');
     end;
  WriteLn(' disk.');
  if not Ask('Is this right? ','Yes') then
     begin
     WriteLn('** Your IO board hardware is probably bad **');
     if not Ask('** Are you sure you want to continue? ','No') then
        exit(FixPart);
     end;
  
  if Configuration = Winch12 then
    if IO24MByte then if Ask('Is this a 24 MByte disk? ','Yes') 
                         then Configuration := Winch24
                      else
    else if Ask('Is this a 12 MByte disk? ','Yes') then
         else Configuration := Winch24;
  end

else begin
     TypeDisk := FLOPBITS;
     cheat.lng := DBLZERO;
     cheat.dbl[1] := Lor(Cheat.Dbl[1], FLOPBITS);
     DiskInfoBLk := Cheat.lng;
     DIBRead := TryDiskIO(DiskInfoBlk, buf, labelPtr, DskRead, NUMTRIES);
     if (not DIBRead) or (buf^.PartKind <> Root) or 
         ((buf^.PartDevice <> FloppySingle) and
          (buf^.PartDevice <> FloppyDouble)) then
            If not Ask('** This does not look like a file system floppy.  Continue? ','No') then exit(FixPart);
     If buf^.PartDevice = FloppySingle then
        if Ask('Is this a Single Sided Floppy','Yes') then
              Configuration := FloppySingle
        else
     else if Ask('Is this a Double Sided Floppy','Yes') then
          else Configuration := FloppySingle;
     end;

If Configuration = Generic5Inch Then SetupDiskParams(False,
                                                     '',
                                                     NumHeads,
                                                     NumCylinders,
                                                     SecPerTrk,
                                                     Bootsize, 
                                                     writecompcyl);

cheat.lng := DBLZero;
cheat.dbl[1] := lor(cheat.dbl[1], TypeDisk);

DiskInfoBLk := cheat.lng;

firstPart := DoubleAdd(diskInfoBlk, wordsPerBlock);
RoundUp(firstPart, remainder);

disk := WhichDisk(DiskInfoBlk);
If EIOFlag And (configuration = CIOMicrop) 
    Then LastAddr := LastDiskAddr(Winch24)
    Else LastAddr := LastDiskAddr(Configuration);

if DIBRead then 
   DIBRead := TryDiskIO(DiskInfoBlk, buf, labelPtr, DskRead, NUMTRIES);

if not DIBRead then begin
               WriteLn('** Bad news!! Can''t read Device info block');
               diskBad := true;
               end
else begin
     for i := 0 to 63 do
        DiskParts[i] := buf^.subparts[i];
     i := 0;
     DiskParts[64] := DoubleInt(1);
     while DiskParts[i] = DBLZero do i := i+1;
     if i > 63 then
           begin
           WriteLn('Disk Information block thinks there are no partitions.');
           DiskBad := true;
           end
     else if i <> 0 then
         begin
         WriteLn('DIB says first partition not at index 0 (it says ',i:1,').');
         DiskBad := true;
         end;
     end;

DiskParts[64] := DBLZero;

i := 0;
addr := firstPart;

repeat
  WriteLn('Doing partition # ',i:1,' at address ',AddrToField(addr):1:-10);
  PartParts[i] := addr;
  if not DIBRead then DiskParts[i] := addr;
  if addr = DBLZero then
     begin
     WriteLn('Address is zero, that can''t be right.');
     PartBad[i].notcyl := true;
     end
  else begin
       If not OKAddr(addr, false) then
          begin
          WriteLn('Address ',AddrToField(addr):1:-10,' is not on a cyl bndry');
          WriteLn('It must be wrong');
          PartBad[i].notcyl := true;
          end;
       end;
  if DiskParts[i] = DBLZero then
     begin
     WriteLn('Disk says address is zero.  Assuming DIB is bad.');
     DiskBad := true;
     end
  else begin
       If not OKAddr(DiskParts[i], false) then
           begin
           WriteLn('Disk Info block says partition starts on a non cyl bndry');
           WriteLn('DIB must be wrong');
           DiskBad := true;
           end
       else if PartBad[i].notcyl then 
            begin
            WriteLn('Disk addr seems OK so using it instead');
            addr := DiskParts[i];
            PartParts[i] := addr;
            end; 
       end;
  if DiskParts[i] <> addr then
      begin
      WriteLn('Disk info block says partition ',i:1,' is in the wrong place');
      WriteLn('Addr in DIB = ',AddrToField(DiskParts[i]):1:-10,
            ' but should be ',AddrToField(addr):1:-10);
      if addr = FirstPart then
         begin
         WriteLn('Since is first partition, DIB probably bad');
         DiskBad := true;
         end
      else PartBad[i].disagrees := true;
      end;
  ok := TryDiskIO(addr, buf, labelPtr, DskRead, NUMTRIES);
  if not ok then
    begin
    WriteLn('** CANNOT READ PARTITION INFO BLOCK!! ');
    PartBad[i].noRead := true;
    freeCount := DBLZERO;
    if DIBRead then
        begin
        if DiskBad then WriteLn('DIB is suspect, but using it anyway');
        next := DiskParts[i+1];
        if next = DBLZero then
           begin
           WriteLn('DIB[',i+1:1,']=0; assuming done all partitions');
           next := DoubleAdd(LastAddr, WordsPerBlock);
           end;
        end
    else next := DBLZERO;  {will request new next below}
    end
  else begin
       next := DoubleAdd(buf^.partEnd, wordsPerBlock);
       freeCount := buf^.NumFree;
       Write('Name of Partition is "');
       nameOk := WritePart(RECAST(buf^.PartName, PName));
       WriteLn('"');
       if not nameOK then
         begin
         if Ask('* The name seems bad; is it bad','Yes') then
              partBad[i].nameBad := true;
         end;
       if buf^.partStart <> addr then
         begin
         WriteLn('* Partition ',i:1,' info block ',AddrToField(addr):1:-10,
                  ' has wrong data in its start block');
         WriteLn('* Conjecture that PIB messed up.');
         partBad[i].startBad := true;
         end;
      if buf^.partEnd = LastAddr then
         if DiskParts[i+1] = DBLZero then WriteLn('Done all partitions')
         else begin
              WriteLn('* Partition goes to end of device but DIB says there are more partitions');
              if DiskBad then WriteLn('* This is probably because DIB is bad.')
              else begin
                   WriteLn('Assuming DIB is bad');
                   DiskBad := true;
                   end
              end
      else if DiskParts[i+1] = DBLZero then
         begin
         WriteLn('* DIB says no more partitions but PIB says not yet to end of dev');
         if PartBad[i].OK <> 0 then
             begin
             WriteLn('* This is probably because PIB is wrong.');
             next := DoubleAdd(LastAddr, WordsPerBlock);
             PartBad[i].endBad := true;
             end
         else if DiskBad then WriteLn('This is probably because DIB is bad')
          else begin
               WriteLn('* Assuming PIB is wrong');
               next := DoubleAdd(LastAddr, WordsPerBlock);
               PartBad[i].endBad := true;
               end;
         end;
      end; {Buf OK}
   if not OKAddr(next, false) then
         begin
         WriteLn('* The next addr isn''t on a track boundary');
         PartBad[i].endBad := true;
         if OKAddr(DiskParts[i+1], false) then next := DiskParts[i+1]
         else begin
              WriteLn('* Disk next is also messed up.');
              WriteLn('** I have no idea where next partition starts **');
              Repeat
                 Write('** Please type next addr or ^C to abort: ');
                 j := ReadUnsigned(0);
                 next := FieldToAddr(disk, j);
                 ok := CheckAndReportAddr(next, false, false, true);
              Until ok;
              end;
         end
  else if DIBRead and
           (not (buf^.partEnd = LastAddr) and (DiskParts[i+1] = DBLZero)) then
        if not OKAddr(DiskParts[i+1], false) then
           begin
           WriteLn('* Next DIB address bad but PIB one seems ok.  Assuming Disk bad.');
           DiskBad := true;
           end
    else if next <> DiskParts[i+1] then {both are OK addresses, just different}
        begin
        WriteLn('* End addr for partition ',i:1,' does not match start of next according to DIB.');
        if partBad[i].OK <> 0 then 
           begin
           WriteLn('* This PIB definitely seems bad.');
           PartBad[i].endBad := true;
           next := DiskParts[i+1];
           end
        else if DiskBad then WriteLn('Assuming DIB is wrong')
        else begin
             WriteLn('* Assuming PIB is wrong and DIB is right.');
             partBad[i].endBad := true;
             next := DiskParts[i+1];
             end;
        end;

if not CheckFree(addr, next, freeCount) then
  begin
  WriteLn('* Free count is wrong');
  partBad[i].badFreeCount := true;
  end;

addr := next;
i := i+1;
until DoubleGtr(next, LastAddr);
last := i-1;

WriteLn;
WriteLn('-------------------------------------');
WriteLn('            Summary');
WriteLn('-------------------------------------');

j := 0;

For i := 0 to 63 do
  if PartBad[i].OK <> 0 then
     begin
     j := j+1;
     WriteLn('Partition # ',i:1,' thought to be bad.');
     if PartBad[i].noRead then WriteLn('   PIB could not be read');
     if PartBad[i].notcyl then WriteLn('   Not on cyl bndry');
     if PartBad[i].disagrees then WriteLn('   It disagrees with DIB');
     if PartBad[i].startBad then WriteLn('   Its start addr is bad');
     if PartBad[i].nameBad then WriteLn('   Its name is bad');
     if PartBad[i].endBad then WriteLn('   Its end addr is bad');
     if PartBad[i].badFreeCount then WriteLn('   It has an illegal free count');
     end;
WriteLn;
If DiskBad then WriteLn('Disk information block seems bad.')
else if j <> 0 then WriteLn('Disk information block seems OK')
else begin
     WriteLn('** All OK **');
     exit(FixPart);
     end;

WriteLn;
WriteLn('-------------------------------------');
WriteLn('Now try to fix errors.');
WriteLn('-------------------------------------');
if not Ask('Do you want to try to fix the errors', 'N') then exit(FixPart);

WriteLn('Dismounting device');

DeviceDismount(disk);

WriteLn(' Part #   DiskPart     PartPart');
j := 0;
for i := 0 to 63 do
  if (DiskParts[i] <> DBLZero) or (PartParts[i] <> DBLZero) or (i <= last+1) then
    begin
    Write('  ',i:2,'  ',AddrToField(DiskParts[i]):6:-10);
    if DiskParts[i] <> PartParts[i]
      then begin
           Write('  ** <> **  ');
           j := 1;
           end
      else Write('            ');
    WriteLn(AddrToField(PartParts[i]):6:-10);
    end;
WriteLn;
WriteLn(' (',-1:1:-10,' means not used.)');
if j <> 0 then WriteLn(' (PartPart numbers are likely to be more correct.)');
writeln;

for i := 0 to 63 do
  if (DiskParts[i] <> DBLZero) or (PartParts[i] <> DBLZero) or (i <= last+1) then
     begin
     WriteLn;
     addr := PartParts[i];
     repeat
         j := AddrToField(addr);
         Write('Address for partition # ',i:1,' (',-1:1:-10,' means delete) [',
                 j:1:-10,'] ');
         j := ReadUnsigned(j);
         if j = -1 then addr := DBLZero
         else addr := FieldToAddr(disk, j);
         ok := CheckAndReportAddr(addr, true, false, false);
     until ok;
     DiskParts[i] := addr;
     PartParts[i] := addr;
     if addr = DBLZero then goto 2;
     WriteLn('Reading Partition # ',i:1,' addr ',AddrToField(addr):1:-10);
     okPIB := TryDiskIO(addr, buf, labelPtr, DskRead, NUMTRIES);
     if not okPIB then
       begin
       WriteLn('**CANNOT READ PIB**');
       ZeroBuffer(buf);
       end
     else ShowPart(buf);
     repeat
        Write('Partition name');
        if okPIB and (not PartBad[i].nameBad) then 
          begin
          Write(' [');
          ok := WritePart(RECAST(buf^.partName, PName));
          Write('] :');
          end
        else write(': ');
        ReadLn(s);
        if s = '' then ok := okPIB and (not PartBad[i].nameBad) 
        else ok := CheckName(s);
        if not ok then WriteLn('** Illegal name.');
     until ok;
        
     if s <> '' then AssignName(buf, s);
     WriteLn('Partition start address will be ',AddrToField(addr):1:-10);
     buf^.partStart := addr;
     repeat
       if PartParts[i+1] = DBLZero then next := LastAddr
       else next := DoubleSub(PartParts[i+1],WordsPerBlock);
       j := AddrToField(next);
       Write('Partition end address [',j:1:-10,'] ');
       j := ReadUnsigned(j);
       next := FieldToAddr(disk, j);
       tempAddr := DoubleAdd(next, WordsPerBlock);
       ok := CheckAndReportAddr(tempAddr, true, true, false);
     until ok;
     buf^.PartEnd := next;
     for j := 0 to 63 do
       buf^.subparts[j] := DBLZero;
     buf^.PartRoot := DiskInfoBlk;
     buf^.PartKind := Leaf;
     buf^.PartDevice := Configuration;
     if PartBad[i].badFreeCount then
              begin
              WriteLn('** Setting free count to zero');
              buf^.NumFree := DBLZERO;
              end;
     WriteLn('Writing partition # ',i:1);
     labelPtr^.SerialNum := addr;
     labelPtr^.LogBlock := 0;
     labelPtr^.Filler := 0;
     labelPtr^.PrevAdr := DBLZero;
     labelPtr^.NextAdr := DBLZero;
     j := ErrorCnt[IOEDNW];
     okPIB := TryDiskIO(addr, buf, labelPtr, DskFirstWrite, NUMTRIES);
     if not okPIB then
       begin
       if j <> ErrorCnt[IOEDNW] then 
          WriteLn(Chr(7),'**** FLOPPY NOT WRITABLE, please put a write-tab on it.')
       else begin
            WriteLn(Chr(7),'**** CANNOT WRITE PIB!! ****');
            WriteLn('    You need to reformat the device or re-partition the');
            WriteLn('    entire device putting the partitions in different places.');
            WriteLn       ('****************************');
            end;
       if not Ask(' Continue with FixPart','N') then exit(FixPart);
       end;
  2: end;  {loop}

WriteLn;
WriteLn('Writing the Disk Information block');

DIBRead := TryDiskIO(DiskInfoBlk, buf, labelPtr, DskRead, NUMTRIES);
if not DIBRead then
   begin
   WriteLn('** CANNOT READ DIB');
   ZeroBuffer(buf);
   end;
repeat
   Write('Disk name');
   if DIBRead then 
     begin
     Write(' [');
     ok := WritePart(RECAST(buf^.partName, PName));
     Write('] ');
     end
   else begin
        ok := false;
        write(': ');
        end;
   ReadLn(s);
   if s <> '' then ok := CheckName(s); {else ok already set}
   if not ok then WriteLn('** Illegal name.');
until ok;
if s <> '' then AssignName(buf, s);

for j := 0 to 63 do
       buf^.subparts[j] := DiskParts[j];

buf^.PartRoot := DiskInfoBlk;
buf^.PartKind := Root;
buf^.PartDevice := Configuration;
labelPtr^.SerialNum := DiskInfoBlk;
labelPtr^.LogBlock := 0;
labelPtr^.Filler := 0;
labelPtr^.PrevAdr := DBLZero;
labelPtr^.NextAdr := DBLZero;
j := ErrorCnt[IOEDNW];
ok := TryDiskIO(DiskInfoBlk, buf, labelPtr, DskFirstWrite, NUMTRIES);

if not ok then
       begin
       if j <> ErrorCnt[IOEDNW] then 
          WriteLn(Chr(7),'**** FLOPPY NOT WRITABLE, please put a write-tab on it.')
       else WriteLn(Chr(7),'*** CANNOT WRITE DISK INFORMATION BLOCK, YOU NEED TO RE-FORMAT THE DEVICE ***',Chr(7));
       exit(FixPart);
       end;

WriteLn;
WriteLn('Now mount the device');
WriteLn;

DeviceMount(disk);
MyDisplayPartitions;

WriteLn;
WriteLn('------------------------------------------------------------------');
WriteLn('*** YOU SHOULD NOW RUN THE SCAVENGER ON ANY PARTITIONS CHANGED ***');
WriteLn('------------------------------------------------------------------');
WriteLn;

end.
