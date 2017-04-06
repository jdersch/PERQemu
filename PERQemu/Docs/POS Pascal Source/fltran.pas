{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FloppyTransfers;

{----------------------------------------------------------------------
{
{ Written by: Miles A. Barel
{ Copyright (C) Three Rivers Computer Corporation 1980, 1981
{ 
{ Abstract:
{
{     FloppyTransfers contains the routines that deal with RT11-
{     formatted floppy disks. It was created from Program PLX
{     and Module PLXUtil for the January, 1982 release of POS 
{     Version D.6.
{
{----------------------------------------------------------------------}
{$Version V2.0 for POS}
{----------------------------------------------------------------------
{
{ Change Log:                                                  
{
{      3 Feb 84  V2.0  Dirk Kalp
{     Lots of hacking done to fix many problems. Fixed up the retry and
{     verify mechanisms. Made Floppy program work on Perq2 double density.
{     Refer to comments in the following changed procedures: Flop, FlpBlk,
{     RWSector, EmptyBuffer, VerifyTrack, Putfile, Getfile, Squish, and
{     Init.
{
{      9 May 83  V1.5  Sandeep Johar
{     Retrofit brads fix so that the program does not continue if there
{     is an error during compare. It stops only if the wait flag is on.
{      
{     16 Feb 83  V1.4  August G. Reinig
{     Added buffered sectors.  Removed MPOS code.
{
{     24 Feb 83  V1.3  Jerry Conner
{     Fix bugs associated with deleting write protected disks, renaming
{     to existing files, and device not ready.
{
{     17 Dec 82  V1.2  August G. Reinig
{     Incorporated changed suggest by Sue Lapin dealin with computing
{     the size of the disk directory.  Previously, floppy assumed theat the
{     disk directory was always 4 segments long.  Changed the const MaxExtra.
{ 
{     14 Oct 82  V1.1  Roger Riggs
{     Updated to support new I/O subsystem.  Added Interleave switch
{     to control the order of phyical sectors during Format commands.
{
{     12 May 82  V1.0  Brad Myers
{     Fixed bugs and made to work with POS
{
{     21 Apr 82  V0.4 J. Conner
{     Change showprogress to computeprogress
{
{     19 Apr 82  V0.3 J. Conner
{     Change FloppyFSFloppy to BootFloppyVers
{
{     16 Mar 82 C. Beckett V0.2
{     Interfaced module to MPOS floppy IO.
{
{     27 Jan 82 DCF V0.1
{     Changed name from RT11 to FloppyTransfers, added
{     RT11Utils (ne PLXUtils) to it.
{
{     15 Dec 81 Diana Connan Forgy V0.0 
{     Redesigned Version V1.11 of Program PLX to create 
{     Module FloppyTransfers.
{
{----------------------------------------------------------------------}



exports

Imports FloppyDefs from FloppyDefs;
imports FileSystem from FileSystem;
Imports Stream from Stream;
Imports QuickSort from QuickSort;


const    

    PermStatus = #2000;    {   "     "    "  permanent   "   }
    UnusedStatus = #1000;  { Status word for free space }
    TermStatus = #4000;    {   "     "    "  list termination }
    MaxExtra = 15;         { Max # of extra dir words allowable if we are
                             to modify the directory }
    SecPerTrk = 26;        { Sectors per Track }
    STracks = 76;          { highest single sided track }
    DTracks = 153;         {    "    double    "    "   }
    DirStart = 6;          { First Block of directory }
    DirSize = 512;         { # words in an RT-11 directory Segment }
    HeadSize = 5;          { # header words in an RT-11 directory segment }
    RTDate = 6;
    RTSegs = 0;            { constant offsets into RT-11 directories }
    RTNext = 1;
    RTHighest = 2;
    RTExtra = 3;
    RTStart = 4;
    RTEntry = 5;
    RTStat = 0;
    RTName0 = 1;
    RTName1 = 2;
    RTExt = 3;
    RTLen = 4;
    RTJobChan = 5;
    RTBits = 7;
    
type DateRec = packed record case boolean of
                true: (IntVal: integer);
                false:(Year: 0..31;
                       Day:  0..31;
                       Mon:  0..31)
               end;
    FlpBlock = array [0..255] of integer;
    FlpBlkPtr = ^FlpBlock;
    DirPtr = ^FlpDirEntry;
    FlpDirEntry = record
                 Status: integer;
                 Name0, Name1, Ext: integer;
                 Size: integer;
                 JobChan: integer;
                 Date: DateRec;
                 ExtraWords: array [1..MaxExtra] of integer;
                 Next: DirPtr;
                 StartBlock: integer
                end;
    FlpDirHeader = record
                 SegsAvail: integer;
                 ExtraBytes: integer;
                 FirstBlock: integer
             end;
    DirSeg = array[0..DirSize-1] of integer;
    DirSegPtr = ^DirSeg;
    OnOffType = (On,Off);
                 
var Src, Dst: string;
    DirList: DirPtr;       { Head of Current Directory }
    VerBuf: FlpBlkPtr;     {    "   floppy buffer "  verifying IO transactions}
    FlpBuf: FlpBlkPtr;     {    "   floppy buffer "   "       "      }
    CurDate: DateRec;      { date for new files }
    CurFileCnt: integer;   { number of entries, used and free in the Current }
                           { Directory }
    SecPerBlk: integer;    { Sectors Per Block (4-Single Dens, 2-Double Dens) }
    Status: IOStatPtr;     { global status block for IO transactions }
    GStatus: IOStatPtr;    { global status block for Squeeze transaction }
    SetStatus: DevStatusBlock;
    MaxFBlock: integer;     { Highest valid block on the floppy }
    DirHead: FlpDirHeader;    { Header info about Current Directory }
    DirSegBuf: DirSegPtr;
    HaveCurDate: Boolean;  { set if date specified so is current on disk }
    
    Prompt: String;
    VerifyError: boolean;  { if true, verify on read or write failed }

Function  FlpBlk(Blk: integer; Buf: FlpBlkPtr; Cmd: IOCommands): boolean; 
Procedure ConvertName(FN:string; var Name0, Name1, Ext: integer;
                                 var WildName,WildExt,Valid: boolean);
Procedure Lookup(FN: String; var Blk, Siz, Bits: integer);
Function  VerBlock (blockNum: integer; dskBlk: FlpBlkPtr): boolean;
Procedure GetRad50(RadWord: integer; Var C0,C1,C2: char);
Procedure PrintDate(var F: Text; Day,Month,Year:integer);

Procedure GetFile (FlpName: String; DiskName: PathName);
Procedure Init;
Procedure DoRename (OldName, NewName: String);
Procedure DoCompare (DiskFile, FlpFile: String);
Procedure DoType (FlpFile: String);
Procedure DoDirectory (FlpFile, OutFileName: String; var FileOpen: Boolean);
Procedure FindSidesAndDensity(askIfFail: boolean);
Procedure DoDelete (FlpName: String; Unload: Boolean);

{$IFC not BootFloppyVers THEN} { if using a filesystem floppy, save
                              { space this way - these routines won't
                              { be used anyway.                   }
Procedure PutFile (DiskName: PathName; FlpName: String);
Procedure ZeroFloppy;
Procedure Squish;

{$ENDC} 
Function EmptyBuffer : boolean;  { call this after last call to FlpBlk }
                                 { with a command of IOWrite }
private

Imports System from System;

Imports IO_Others from IO_Others;  {using IOKeyClear}
Imports FileUtils from FileUtils;
Imports PMatch from PMatch;
Imports Clock from Clock;


const
    LastModDay = 28;       { Date used for default date - should be the }
    LastModMonth = 10;      { date of last modification, since the date  }
    LastModYear = 81;      { should not be in the past! }
    STSize = 13;           { Skew Table Size - Size of table used for
                             determining inter-track sector skew }
    SkewFactor = 6;
    IntFactor = 2;         { normal sector interlace factor }
    TentStatus = #400;     {   "     "    "  tentative files }
    DDWords = 128;         { number of words in a Double density sector }
    SDWords = 64;          { number of words in a Single density sector }
    DDBytes = 256;
    SDBytes = 128;

type   
    SDSector = array[0..SDWords-1] of integer;
    DDSector = array[0..DDWords-1] of integer;
    Buffer = record case integer of
      1 : ( DTrk : array[1..SecPerTrk] of DDSector );
      2 : ( STrk : array[1..SecPerTrk] of SDSector );
      end;
    pTrkBuffer = ^Buffer;
       
var
    Skew: array[0..STSize-1] of integer;
                           { table to compute inter-track sector skew }
    Rad50: packed array[0..#50] of char;
    VerTbl, ShortTbl: ^CmdArray;
    DoDumRead: boolean;    { Flag to perform bogus read  
                             before doing anything for real}
                           
    { variables for buffering of floppy sectors }
    TrkBufPtr : pTrkBuffer;   { points to storage for a double density track } 
    VerBufPtr : pTrkBuffer;
    Dirty : array[1..SecPerTrk+1] of boolean;  { indicates if sector changed } 
    Verified : array[1..SecPerTrk] of boolean; { indicate sector verified }
    SomeDirty : boolean;      { indicates if any sector changed }
    BufferedTrkNum : integer; { which track is buffered }
    BufferedSecSize : integer; { how large are its sectors }
    TrkBuffered : boolean;    { indicates if buffer contains a read track }

    SDMaxRetry : integer;    { The maximum number of retries for Verify will  }
    DDMaxRetry : integer;    { be made dependent upon the density as well as  }
                             { the machine type (i.e., we will try harder for }
                             { Perq2 double density in order to overcome      }
                             { apparent hardware problems on Perq2 floppy.)   }

    LastTrkReadFail: integer; { The track addr that just failed a trackread, }
                              { if any. (-1 means none)                      }
{ The routines below, GetDate through GetCurDate,
{  are from former module PLXUtil. }



function WriteDirectory: boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{      Writes out the loaded directory to the floppy.  
{      Returns True if successful. 
{
{ Note
{      We know about the buffering of sectors on writes to the floppy.
{      Thus, when we are done, we empty the sector buffer.  Since 
{      anyone writing to the floppy must use us before the write is
{      recorded, no other writers need know about the buffering scheme.
{
{-----------------------------------------------------------------}

  var EntPerBlk,BaseBlock,CurBase,i,j,k,DirSegs:integer; Tmp,Tmp1: DirPtr;
      Adding: boolean;
      TmpDir: record case integer of
              0: (RT: DirSegPtr);
              1: (Seg,Ofst: integer);
              2: (Flp: FlpBlkPtr)
          end; 
      Temp : integer;
begin
WriteDirectory := false;
EntPerBlk:=(DirSize-HeadSize) div (RTDate+1+DirHead.ExtraBytes div 2) - 1;
{ coalesce free space first }
Tmp:=DirList;
while Tmp <> nil do
    begin
    if Tmp^.Status = UnusedStatus then
        begin
        Adding:=true;
        while Adding do
            if Tmp^.Next = nil then
                Adding:=false
            else if Tmp^.Next^.Status <> UnusedStatus then
                Adding:=false
            else
                begin
                Tmp^.Size:=Tmp^.Size+Tmp^.Next^.Size;
                Tmp1:=Tmp^.Next;
                Tmp^.Next:=Tmp1^.Next;
                CurFileCnt:=CurFileCnt-1;
                Dispose(tmp1)
                end
        end;
    Tmp:=Tmp^.Next
    end;
{ Make sure there is enough directory space }
if EntPerBlk * DirHead.SegsAvail < CurFileCnt then
    begin
    writeln('** Directory Full');
    exit(WriteDirectory)
    end;
BaseBlock:=DirHead.FirstBlock;
Tmp:=DirList;
{ finally write out the blocks }
DirSegs:=(CurFileCnt + (EntPerBlk -1)) div EntPerBlk;
for i:=1 to DirSegs do
    begin
    { write out each directory block - first the header info }
    DirSegBuf^[0]:=DirHead.SegsAvail;
    if i <> DirSegs then
        DirSegBuf^[1]:=i+1
    else
        DirSegBuf^[1]:=0;
    DirSegBuf^[2]:=DirSegs;
    DirSegBuf^[3]:=DirHead.ExtraBytes;
    DirSegBuf^[4]:=BaseBlock;
    CurBase:=RTEntry;
    j:=1;
    while (j<=EntPerBlk) and ((i-1)*EntPerBlk+j <= CurFileCnt) do
        begin
        { then the "Per File" info }
        with Tmp^ do
            begin
            DirSegBuf^[CurBase+RTStat]:=Status;
            DirSegBuf^[CurBase+RTName0]:=Name0;
            DirSegBuf^[CurBase+RTName1]:=Name1;
            DirSegBuf^[CurBase+RTExt]:=Ext;
            DirSegBuf^[CurBase+RTLen]:=Size;
            DirSegBuf^[CurBase+RTJobChan]:=JobChan;
            DirSegBuf^[CurBase+RTDate]:=Date.IntVal;
            if DirHead.ExtraBytes <> 0 then
                for k:=1 to DirHead.ExtraBytes div 2 do
                    DirSegBuf^[CurBase+RTDate+k]:=ExtraWords[k];
            CurBase:=CurBase+RTDate+1+DirHead.ExtraBytes div 2;
            BaseBlock:=BaseBlock+Size;
            Tmp:=Next
            end;
        DirSegBuf^[CurBase+RTStat]:=TermStatus;
        j:=j+1
        end;
    { Finally the actual write }
    TmpDir.RT:=DirSegBuf;
    Temp := DirStart+(i-1)*2;
    if not FlpBlk(Temp,TmpDir.Flp,IOWrite) then exit(WriteDirectory);
    TmpDir.Ofst:=TmpDir.Ofst+256;
    if not FlpBlk(Temp+1,TmpDir.Flp,IOWrite) then exit(WriteDirectory);
    if not EmptyBuffer then exit(WriteDirectory); { make sure writes happen }
    end;
WriteDirectory := true;
end { WriteDirectory };
        

Procedure UnloadDirectory;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Gives back memory used by directory list. 
{
{-----------------------------------------------------------------}

  var Tmp: DirPtr;
begin
while DirList <> nil do
    begin
    Tmp:=DirList^.Next;
    Dispose(DirList);
    DirList:=Tmp
    end
end { UnloadDirectory };


Function LoadDirectory: boolean ;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Loads contents of Floppy directory into FlpDirEntry 
{     records for manipulation. 
{
{-----------------------------------------------------------------}


  var TmpDir: record case integer of
          0: (RT: DirSegPtr);
          1: (seg,ofst: integer);
          2: (Flp: FlpBlkPtr)
        end;
      EntrySize,CurSeg,CurBase,BlkCnt, i: integer;
      NewEntry,TmpEntry: DirPtr;
begin
LoadDirectory := false;
if DirList <> nil then UnloadDirectory;    { give back previous space }
CurFileCnt:=0;
TmpEntry:=nil;
TmpDir.RT:=DirSegBuf;
if not EmptyBuffer then exit(LoadDirectory); { make sure we read the floppy }
if not FlpBlk(DirStart,TmpDir.Flp,IORead) then exit(LoadDirectory);
DirHead.SegsAvail:=TmpDir.Flp^[RTSegs];
if (TmpDir.Flp^[RTSegs] < TmpDir.Flp^[RTHighest]) or 
   (TmpDir.Flp^[RTStart] <> (DirStart + DirHead.SegsAvail * 2)) then
    begin
    writeln('** Bad Directory');
    Exit(LoadDirectory);
    end;
BlkCnt:=TmpDir.Flp^[RTStart];              { First Block of First File }
with DirHead do
    begin
    ExtraBytes:=TmpDir.Flp^[RTExtra];
    if ExtraBytes > MaxExtra*2 then
        begin
        writeln('** Directory contains too many extra bytes');
        Exit(LoadDirectory);
        end;
    if ExtraBytes = 0 then
        begin
        writeln('** No space in directory to preserve "Bits In',
                ' Last Block"');
        If GetConfirm (NullIdleProc, True, 'Continue?', 3, Switches) = 2 
             then Exit(LoadDirectory);
        end;
    EntrySize:=RTDate+1+ExtraBytes div 2;
    FirstBlock:=TmpDir.Flp^[RTStart]
    end;
CurSeg:=1;
while CurSeg <> 0 do
    begin                   { First Load the Directory Block }
    if CurSeg <> 1 then
        begin
        TmpDir.RT:=DirSegBuf;
        if not FlpBlk(DirHead.SegsAvail+CurSeg*2,TmpDir.Flp,IORead) 
        then exit(LoadDirectory);
        end;
    TmpDir.Ofst:=TmpDir.Ofst+256;
    if not FlpBlk(DirHead.SegsAvail+1+CurSeg*2,TmpDir.Flp,IORead) 
    then exit(LoadDirectory);
    CurSeg:=DirSegBuf^[RTNext];    { Get Link to Next Block }
    CurBase:=RTEntry;
    while (DirSegBuf^[CurBase+RTStat] <> TermStatus) and (CurBase+EntrySize < 512) do
        begin
        CurFileCnt:=CurFileCnt+1;
        New(NewEntry);
        with NewEntry^ do
            begin
            Status:=DirSegBuf^[CurBase+RTStat];
            Name0:=DirSegBuf^[CurBase+RTName0];
            Name1:=DirSegBuf^[CurBase+RTName1];
            Ext:=DirSegBuf^[CurBase+RTExt];
            Size:=DirSegBuf^[CurBase+RTLen];
            JobChan:=DirSegBuf^[CurBase+RTJobChan];
            Date.IntVal:=DirSegBuf^[CurBase+RTDate];
            if DirHead.ExtraBytes <> 0 then
                for i:=1 to DirHead.ExtraBytes div 2 do
                    ExtraWords[i]:=DirSegBuf^[CurBase+RTDate+i];
            if (EntrySize = RTDate+1) or (DirSegBuf^[CurBase+RTBits] < 0) or 
                (DirSegBuf^[CurBase+RTBits] > 4096) then
                    ExtraWords[1]:=4096;
            Next:=nil;
            StartBlock:=BlkCnt;
            BlkCnt:=BlkCnt+Size
            end;
        if TmpEntry = nil then
            DirList:=NewEntry
        else
            TmpEntry^.Next:=NewEntry;
        TmpEntry:=NewEntry;
        CurBase:=CurBase+EntrySize
        end
    end;
loadDirectory := true;
end { LoadDirectory };


Function GetVal(S:string):integer;
{-----------------------------------------------------------------
{
{ Abstract:
{
{    Returns integer value of string (or -1 if  it 
{    gets bad input). 
{
{-----------------------------------------------------------------}

var val,i:integer;
  begin
  val:=0;
  if Length(S) = 0 then val := -1
  else for i:=1 to Length(s) do
          if s[i] in ['0'..'9'] then val:=val*10+(ord(s[i])-ord('0'))
          else begin
               GetVal := -1;
               exit(GetVal)
               end;
  GetVal:=Val;
  end { GetVal };
  

function SetDate(var NewDay, NewMon, NewYear : string): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Sets the current date.
{
{-----------------------------------------------------------------}

  var Months: CmdArray;
      da,mo,yr: integer;
begin
SetDate := false;
    Months[1]:='JAN';
    Months[2]:='FEB';
    Months[3]:='MAR';
    Months[4]:='APR';
    Months[5]:='MAY';
    Months[6]:='JUN';
    Months[7]:='JUL';
    Months[8]:='AUG';
    Months[9]:='SEP';
    Months[10]:='OCT';
    Months[11]:='NOV';
    Months[12]:='DEC';
da := GetVal(NewDay);
yr := GetVal(NewYear);
mo := UniqueCmdIndex(NewMon,Months,12);

IF Da = -1 then exit(SetDate);
if (Mo > 12) or ((Mo = 2) and (Da > 29)) then exit(SetDate); 
if ((Mo in [4,6,9,11]) and (Da > 30)) then exit(SetDate)
     else if Da > 31 then exit(SetDate);
IF (Yr  < 80) or (Yr > 99) then exit(setDate)
     else Yr := Yr - #110;
CurDate.Day := da;
CurDate.Mon := mo;
CurDate.Year := yr;
SetDate := true;
haveCurDate := true;
end { SetDate };


Procedure GetCurDate;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Gets the current date.
{
{-----------------------------------------------------------------}

  var DateStr, DashDels, Dels, Broke: string; 
      NewYear, NewMon, NewDay: String;
      CopyInput: Text;
      success: boolean;
begin

Dels := ' ';
Broke := ' ';
if haveCurDate then exit(GetCurDate);
Reset(CopyInput, 'CONSOLE:');  {compiler bug}
DashDels[0]:=chr(1);
DashDels[1]:='-';
GetTString(DateStr);
repeat 
    CnvUpper(DateStr);
    RemDelimiters (DateStr, Dels, Broke);
    GetSymbol (DateStr, NewDay, ' -', Broke);
    RemDelimiters (DateStr, ' -', Broke);
    GetSymbol (DateStr, NewMon, ' -', Broke);
    RemDelimiters (DateStr, ' -', Broke);
    GetSymbol (DateStr, NewYear, Dels, Broke);
    success := SetDate(NewDay, NewMon, NewYear);
    if not success then 
      begin
      Write('Please type in the date (dd-mmm-yy): ');
      readln(dateStr);
      end;
until success;

end { GetCurDate };



Procedure GetRad50 (RadWord: integer; Var C0, C1, C2: char);
{----------------------------------------------------------------
{
{ Abstract:
{
{     Translate a  Rad50 format word into three characters,
{     C0, C1, and C2.
{
{ Parameters:
{
{     RadWord - Rad50 representation of characters.
{
{     C0, C1, C2 - the three characters to be put into
{     character format. 
{
{-----------------------------------------------------------------}
var I, J: integer;

begin
     
     I := 0;
     J := RadWord;
     while (J >= #3100) or (J < 0) do
     begin
         I := I + 1;
         J := J - #3100
     end;
     c0 := Rad50[I];
     J := RadWord - I * #3100;
     c1 := Rad50[J div 40];
     c2 := Rad50[J mod 40]

end { GetRad50 };



function PutRad50 (C0, C1, C2: char; var RadWord: integer): boolean;
{----------------------------------------------------------------
{
{ Abstract:
{
{     Translate C0, C1, and C2 from character representation to
{     Rad50 format.
{
{ Parameters:
{
{     C0, C1, C2 - the characters that will be translated 
{                  and put into the new word.
{
{     RadWord - the Rad50 representation that will be created
{               for C0, C1, and C2.
{
{ Returns:
{
{     True if the translation succeeded, false otherwise.
{
{
{-----------------------------------------------------------------}

var I: integer;

begin

     PutRad50 := false;
     
     { Convert the characters gotten into upper case first. }
     
     C0 := UpperCase (C0);
     C1 := UpperCase (C1);
     C2 := UpperCase (C2);
     

     { Translate character C0 into Rad50 representation. }
     
     I := 0;
     while (I < 40) and (Rad50[I] <> C0) do I := I + 1; 
     
     if I = 40 then
     begin
         writeln('** Character "', C0,'" cannot be written on a floppy.');
         exit (PutRad50);
     end;
     RadWord := I * 1600;


     { Translate character C1 into Rad50 representation. }
     
     I := 0;
     while (I < 40) and (Rad50[I] <> C1) do I := I + 1;
     if I = 40 then
     begin
         writeln('** Character "', C1,'" cannot be written on a floppy.');
         exit (PutRad50);
     end;
     RadWord:=RadWord + I * 40;
     

     { Translate character C2 into Rad50 representation. }
     
     I := 0;
     while (I < 40) and (Rad50[I] <> C2) do I := I + 1;
     if I = 40 then
     begin
         writeln('** Character "', C2,'" cannot be written on a floppy.');
         exit (PutRad50);
     end;

     { Assign value to RadWord and set function value to
     { true since it succeeded.                        }
     
     RadWord := RadWord + I;
     PutRad50 := true;

end { PutRad50 };
     


Procedure ConvertName (FN: string; 
                       var Name0, Name1, Ext: integer;
                       var WildName, WildExt, Valid: boolean);
{-----------------------------------------------------------------
{
{ Abstract:
{
{      Convert file name FN into Rad50 format for storage
{      on the floppy.
{
{ Parameters:
{
{     FN - String form of filename.
{
{     Name0 - 
{
{     Name1 -
{
{     Ext - 
{ 
{     WildName - True if wildcard for name has been given.
{
{     WildExt - True if wildcard for extension has been given.
{
{     Valid - True if conversion succeeded, false if bad
{             name was given.
{
{-----------------------------------------------------------------}

var I,J:integer; 
    Ch: array [0..6] of char;

begin

     for I := 1 to 6 do ch[I] := ' ';
     I := 1;
     {$R-};
     Valid := False;
     WildName := False;
     
     { Examine the first six characters of FN filename
     { string to see if there's a '*' before the
     { extension. If so, set WildName to true.       }
     
     While (I <= length (FN)) and (FN[I] <> '.') and (I <= 6) do
     begin
         if FN[I] = '*' then WildName := True;
         ch[I] := FN[I];
         I := I + 1
     end { WHILE };
     
     {$R+}
     
     If WildName then
         
         { The asterisk has to be the first character in
         { FN. If it's not, print error message and exit.   }
         
         If I <> 2 then
         begin
             writeln ('** Illegal use of wildcards.');
             exit(ConvertName)
         end { IF error condition }
         else
     else
     
     { Make sure that the first 6 characters in the name can
     { be translated into Rad50 format and translate them. 
     { If they cannot, exit.                                }
     
     begin
         if not PutRad50 (ch[1], ch[2], ch[3], Name0) then exit(ConvertName);
         if not PutRad50 (ch[4], ch[5], ch[6], Name1) then exit(ConvertName);
     end { IF asterisk in name loop };
     
     {$R-}
     
     { Now look to see if there's an asterisk in the extension. }
     
     WildExt := false;
     while (FN[I] <> '.') and (I <= length (FN)) do I := I + 1;
     if I > length (FN) then Ext := 0 { there's no extension on the filename. }
     else
     begin
         for J := 1 to 3 do ch[J] := ' ';
         J := 1;
         while (J <= 3) and ((I+J) <= length (FN)) do
         begin
             ch[J]:=FN[I + J];
             if ch[J] = '*' then WildExt := true;
             J:=J+1
         end { WHILE };
         
         { Make sure that the asterisk is the first character in
         { the extension. If it's not, print an error message and
         { exit.                                                }
         
         If WildExt then
             if J <> 2 then
             begin
                 writeln('** Illegal use of wildcards');
                 exit(ConvertName)
             end { IF error condition }
         else
     
     { Translate the three characters of the extension
     { into Rad50 form. If they can't be translated, exit.   }
     
     else if not PutRad50 (ch[1], ch[2], ch[3], Ext) then exit(ConvertName);
     end { IF there's an extension loop };
     
     { We now know that the name converted is valid and that
     { everything went o.k.                                 }
     
     Valid := True;

end { ConvertName };



Procedure PrintDate (var F: Text; Day, Month, Year: integer);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Print the date in a file.
{
{ Parameters:
{
{     F - textfile in which the date will be printed.
{
{     Day, Month, Year - integer representations of date.
{
{-----------------------------------------------------------------}

begin

     { Write the day and a dash in the file F.     }
     
     Write (F, day:2, '-':1);
     
     { Convert numeric representation of month into 
     { an abbreviation and write it in file F.     }
     
     case month of
         1:  Write (F, 'JAN');
         2:  Write (F, 'FEB');
         3:  Write (F, 'MAR');
         4:  Write (F, 'APR');
         5:  Write (F, 'MAY');
         6:  Write (F, 'JUN');
         7:  Write (F, 'JUL');
         8:  Write (F, 'AUG');
         9:  Write (F, 'SEP');
         10: Write (F, 'OCT');
         11: Write (F, 'NOV');
         12: Write (F, 'DEC');
         otherwise: write (F, '   ')
       end;
     
     { Convert the year into readable form, then
     { write another dash and the year in file F.   }
     
     Write (F, '-':1, year + #110:2)

end { PrintDate };



Procedure FirstSector (Blk: integer; var Trk, Sec: integer);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Calculate the first physical sector for logical block Blk.
{
{ Parameters:
{
{     Blk - logical block.
{
{     Trk, Sec - physical track and sector.
{
{-----------------------------------------------------------------}

var tmp, tmp1: integer;

begin

     tmp := Blk * SecPerBlk;       { logical sector - now interlace }
     Trk := tmp div SecPerTrk;     { physical track }
     tmp := tmp mod SecPerTrk;     { relative sector # }
     tmp1 := Skew[Trk mod STSize]; { first sector to be used on this track }
     
     { Calculate physical sector }
     
     while tmp > 0 do
     begin       
         tmp1 := tmp1 + IntFactor;
         if tmp1 = (SecPerTrk + 1) then tmp1 := 1
              else if tmp1 > SecPerTrk then tmp1 := 2;
         if odd(tmp1) and (tmp1 = Skew [Trk mod STSize]) then
             tmp1 := tmp1 + 1;    { wrap from odd sectors to even sectors }
         tmp := tmp - 1
     end { WHILE };
     sec := tmp1

end { FirstSector };



Procedure NextSector(var Trk,Sec: integer);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Advance to next sequential sector.
{
{ Parameters:
{
{     Trk, Sec - physical track and sector.
{
{-----------------------------------------------------------------}


var tmp,tmp1: integer;

begin

     tmp := Skew[Trk mod STSize];       { first sector on this track }
     tmp1:=sec + IntFactor;             { next sector }
     if tmp1 = (SecPerTrk + 1) then tmp1 := 1
          else if tmp1 > SecPerTrk then tmp1 := 2;
      
     { check for sector wraparound }

     if odd(tmp1) and (tmp1 = tmp) then tmp1 := tmp1 + 1
     else if (not odd(tmp1)) and (tmp1 = tmp + 1) then
     begin
         Trk:=Trk+1;                    { go on to the next track }
         tmp1:=Skew[Trk mod STSize]
     end { IF };

     Sec := tmp1

end { NextSector };


Function Flop(Cmd: IOCommands; BufPtr: IOBufPtr; Bytes: integer; 
              Adr: double): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Perform operation Cmd on the floppy, using BufPtr^, 
{     moving Bytes bytes, to floppy address Adr
{
{
{-----------------------------------------------------------------}

  label 1;
  const jretries = 4;
  var i,j, iretries:integer;
      ErrMsg :string;
begin
Flop := true;
{}
{ Try a little harder for double density on Perq2.
{}
if (Dens = DDens) and (Cf_IOBoard = Cf_EIO) then
   iretries := 6
else
   iretries := 3;
for i:=1 to iretries do
    begin
    for j:=1 to jretries do
        begin
        UnitIO(Floppy,BufPtr,Cmd,Bytes,Adr,nil,Status);
        if Status^.SoftStatus = IOEIOC then goto 1;
        with Status^ do
            begin    
            ErrMsg := IOErrString(SoftStatus);
            If SoftStatus = -21 then 
               begin
               Flop := False;
               writeln('** Floppy Error: ',ErrMsg);
               Exit (Flop);
               end;
            if (Dens = DDens) and HidingDDensErrors
            and ((i + j) <> (iretries + jretries)) then
               {}
               { i.e., don't report the error on double density unless
               { it's our last try.
               {}
            else
               begin
               write('** Floppy Error: ',ErrMsg);
               write(' [SoftStatus = ',SoftStatus:1);
               writeln(', HardStatus = ', HardStatus:1:8,'] ');
               if (i = iretries) and (j = jretries) then
                  begin
                  write('** Aborted');
                  writeln('      Track: ',Adr[1]:1,'      Sector: ', Adr[0]:1);
                  Flop := false;
                  exit(Flop)
                  end
               else
                  write('Retry.....');
               writeln('      Track: ', Adr[1]:1, '      Sector: ', Adr[0]:1);
               end
            end;
        end{for j ...};
    UnitIO(Floppy,BufPtr,IOReset,0,Adr,nil,Status) {Adr doesn't matter}
    end{for i ...};
1: end { Flop };

procedure VerifyTrack;

{------------------------------------------------------------------------
{
{ Abstract:
{       Read the Buffered sector from the floppy and compare this copy
{       with the buffered copy.
{
{------------------------------------------------------------------------}

label 1;

var
  Fake : record case integer of
    1 : (Ptr : pTrkBuffer);
    2 : (seg,ofst : integer);
    3 : (IOPtr : IOBufPtr );
    end;
  Adr : double;
  WordSize : integer;
  i,j : integer;
  ScanLines : integer;
  Alignment : integer;
  LocalWords : array[0..DDWords+2] of integer;
  MaxRetry: integer;
  ErrMsg: string;
  
begin

  WordSize := BufferedSecSize div 2;

  Fake.Ptr := VerBufPtr;
  Adr[0] := 1;
  Adr[1] := BufferedTrkNum; 

  {}
  { Try MaxRetry times to read the track successfully.
  { Don't bother to tell the user about failures - he will see
  { enough of them later.
  {}
  MaxRetry := 4;   { Maybe just limit retries on track reads to small number. }
  { if Dens = DDens then MaxRetry := DDMaxRetry else MaxRetry := SDMaxRetry; }
  i := 0;
  repeat
     i := i +1;
     if (i mod 4) = 0 then  {toss in a recalibrate and see if it helps}
             UnitIO( Floppy, Fake.IOPtr, IOReset, 0  , Adr, nil, status );   
     UnitIO( Floppy, Fake.IOPtr, IORead, SecPerTrk*BufferedSecSize
        , Adr, nil, status );   
     if ShowTrackErrors and (Status^.SoftStatus <> IOEIOC) then
        begin
        ErrMsg := IOErrString(Status^.SoftStatus);
        write('** Track Error: ',ErrMsg);
        write(' [SoftStatus = ',Status^.SoftStatus:1);
        writeln(', HardStatus = ', Status^.HardStatus:1:8,'] ');
        if i = MaxRetry then
           writeln('Aborting Track Read. Will get sectors individually!')
        else
           begin
           write('Retry.....');
           writeln('      Track: ',Adr[1]:1);
           end;
        end;
  until (Status^.SoftStatus = IOEIOC) or (i = MaxRetry);
        
  if Status^.SoftStatus <> IOEIOC then
     begin 
     {}
     { Mark all sectors as not verified and exit. 
     { We will try to verify individual sectors later.
     {}
     for i := 1 to SecPerTrk do Verified[i] := false;
     exit( VerifyTrack );
     end;
  
  ScanLines := WordSize div 4;
  rasterop( RXor, 64, ScanLines * SecPerTrk
          , 0, 0, 4, VerBufPtr
          , 0, 0, 4, TrkBufPtr );

  inlinebyte( {LSSN} 99 );   { get quad word pointer to Local Area }
  storexpr( Fake.Seg );
  loadadr( LocalWords );
  storexpr( Fake.Ofst );
  Alignment := land( 4 - land( Fake.Ofst, 3 ), 3 ); 
  Fake.Ofst := Fake.Ofst + Alignment;
  
  rasterop( RXor, 64, ScanLines, 0, 0, 4, Fake.Ptr, 0, 0, 4, Fake.Ptr );
  
  for i := 0 to SecPerTrk-1 do
    rasterop( ROr, 64, ScanLines
            , 0, 0, 4, Fake.Ptr
            , 0, i*ScanLines, 4, VerBufPtr );

  for i := Alignment to WordSize + Alignment - 1 do
    if LocalWords[i] <> 0 then goto 1;

  for i := 1 to SecPerTrk do Verified[i] := true;
  exit( VerifyTrack );
  
1:for i := 1 to SecPerTrk do begin
    Verified[i] := true;
    if WordSize = DDWords
    then begin
      for j := 0 to WordSize - 1 do
        if VerBufPtr^.DTrk[i,j] <> 0 then 
          begin
          Verified[i] := false;
          j := WordSize - 1;
          if ShowVerifyErrors then
             if (SomeDirty and Dirty[i])   {for Write}
             or (not SomeDirty)            {for Read}
             then writeln('%% VERIFY ERROR found in VerifyTrack!');
          end;
      end
    else begin
      for j := 0 to WordSize - 1 do
        if VerBufPtr^.STrk[i,j] <> 0 then 
          begin
          Verified[i] := false;
          j := WordSize - 1;
          if ShowVerifyErrors then
             if (SomeDirty and Dirty[i])   {for Write}
             or (not SomeDirty)            {for Read}
             then writeln('%% VERIFY ERROR found in VerifyTrack!');
          end;
      end;
    end;

end;

function EmptyBuffer: boolean;

{------------------------------------------------------------------------
{
{ Abstract:
{       Write any changed sectors in the buffered track to the floppy.
{       Set up the buffer as empty.
{
{ Note Bene:
{       For all callers who are doing IOWrite commands with Verify, all
{       retries MUST be made here. This is necessary since EmptyBuffer
{       flushes the track buffer and makes it available for buffering
{       sectors for a new track; thus, previously buffered data will no
{       longer be available. Thus the retry loop for verifying in PutFile
{       has been eliminated.
{
{------------------------------------------------------------------------}

label 1, 2;

var
  Fake : record case integer of
    1: (ptr : pTrkBuffer );
    2: (seg,ofst : integer );
    3: (IOPtr : IOBufPtr );
    end;
  Adr : double;
  i, j, TryCnt : integer;
  Bytes : integer; 
  StartSec : integer;
  WordSize : integer;
  MaxRetry : integer;
  VerIOPtr : IOBufPtr;

begin
  
  EmptyBuffer := false;
  if SomeDirty then
    begin 
    Fake.ptr := TrkBufPtr;
    Adr[1] := BufferedTrkNum;  
    Bytes := 0;
    StartSec := 1;
    WordSize := BufferedSecSize div 2;
    
    for i := 1 to SecPerTrk+1 do   { SecPerTrk + 1 is never dirty }
      if Dirty[i]                  { need to write this sector }
      then Bytes := Bytes + BufferedSecSize  { so increment bytes to write }
      else begin                    
        if Bytes <> 0 then begin   { need to write some sectors }
          Fake.Ofst := Fake.Ofst + (StartSec - 1)*WordSize;
          Adr[0] := StartSec;
          {}
          { When all sectors are dirty, this turns into a track write.
          {}
          if not Flop( IOWrite, Fake.IOPtr, Bytes, Adr ) then goto 1;
          Bytes := 0;
          end;
        Fake.ptr := TrkBufPtr;
        StartSec := i + 1;     { next sector may need to be written }
        end;
        
    if Verify then
       begin
       {}
       { We attempt to verify the whole track at once using VerifyTrack.
       { If any of the "dirty" sectors (written out above) are marked as
       { not verified, we then try to verify them on an individual basis.
       { If the verify still fails, we will write the sector again and 
       { perform the sector verify again.
       {}
       VerifyTrack;   
       if Dens = DDens then MaxRetry := DDMaxRetry else MaxRetry := SDMaxRetry;
       Fake.Ptr := TrkBufPtr;
       for i := 1 to SecPerTrk do
          begin
          if Dirty[i] and not Verified[i] then
             begin
             TryCnt := 0;
             Adr[0] := i;  { Sector number }
             VerIOPtr := recast(VerBuf, IOBufPtr);
2:           VerifyError := false;
             if not Flop(IORead, VerIOPtr, WordSize*2, Adr) then
                begin
                writeln( 'Bad at Sector ', i:0, ' Track ', BufferedTrkNum:0 );
                goto 1;
                end;
             {$R-}
             for j := 0 to WordSize-1 do
                if VerIOPtr^[j] <> Fake.IOPtr^[j] then
             {$R=} begin
                   VerifyError := true;
                   TryCnt := TryCnt + 1;
                   if ShowVerifyErrors then
                      writeln('%% VERIFY ERROR on sector in EmptyBuffer!');
                   if TryCnt > MaxRetry then
                      begin
                      writeln('* Too many failures, aborting');  
                      goto 1;
                      end;
                   writeln('* Block incorrectly transferred, retrying');   
                   if TryCnt mod 4 = 0 then {recalibrate}      
                      UnitIO( Floppy, VerIOPtr, IOReset, 0, Adr, nil, Status);
                   if not Flop(IOWrite, Fake.IOPtr, WordSize*2, Adr) then
                      begin
                      writeln('Bad at Sector ',i:0,' Track ',BufferedTrkNum:0);
                      goto 1;
                      end;
                   goto 2;
                   end;
             Verified[i] := true;
             end;
          Fake.Ofst := Fake.Ofst + WordSize;
          end{for};
       end;

    LastTrkReadFail := -1;
    end{if SomeDirty};
  {}
  { If we get here, then the track buffer was successfully written out,
  { verified (if Verify selected), and emptied of any dirty sectors.
  {}
  EmptyBuffer := true;   

1:SomeDirty := false;
  for i := 1 to SecPerTrk do Dirty[i] := false;
  BufferedTrkNum := -1;
  BufferedSecSize := 0;
  TrkBuffered := false;
  
end;

function RWSector( Cmd: IOCommands
                 ; BufPtr: IOBufPtr
                 ; Bytes : integer
                 ; Adr : double ): boolean;

{----------------------------------------------------------------------
{
{ Abstract
{       Read/Write a floppy sector.  Using the buffer if we can.
{
{----------------------------------------------------------------------}

label 1, 2, 3;

type
  Sector = record case integer of
    1 : ( DD : DDSector );
    2 : ( SD : SDSector );
    end;
  pSector = ^Sector;

var
  Fake : record case integer of
    1: (ptr : pTrkBuffer );
    2: (seg,ofst : integer );
    3: (IOPtr : IOBufPtr );
    end;
  i : integer;
  SecNum : integer;
  SecPtr : pSector;
  IOPtr, VerIOPtr : IOBufPtr;
  TWord, WordSize, MaxRetry: integer;
  TryCnt : integer;
  ErrMsg: string;

begin  
  
  RWSector := false; 
  if Bytes <> DDBytes then if Bytes <> SDBytes then exit( RWSector );

  WordSize := Bytes div 2;
  SecNum := Adr[0];  
  SecPtr := recast( BufPtr, pSector );

  if Cmd = IOWrite then
    begin
    if BufferedTrkNum <> Adr[1] then
      begin                                     { wrong track }
      if not EmptyBuffer then exit( RWSector ); { so empty the buffer } 
      {}
      { Note that EmptyBuffer must handle the
      { Verify and retry loop since we will
      { next designate TrkBufPtr^ to hold a
      { different track and will assign a
      { sector of data for that track below.
      {}
      BufferedTrkNum := Adr[1];                 { start buffering a }
      BufferedSecSize := Bytes;                 { different track }
      end;
    Dirty[SecNum] := true;
    SomeDirty := true;
    if Bytes = DDBytes then
      TrkBufPtr^.DTrk[SecNum] := SecPtr^.DD
    else
      TrkBufPtr^.STrk[SecNum] := SecPtr^.SD;
    end

  else if Cmd = IORead then
     begin
     if (BufferedTrkNum <> Adr[1])               { wrong track }
     or not TrkBuffered then                     { or we've not read it }
       begin
       if not EmptyBuffer then exit( RWSector ); { clear the buffer }
       Adr[0] := 1;                              { read in the whole track }
       IOPtr := recast( TrkBufPtr, IOBufPtr );
       {}
       { If the trackread just failed for this track on previous call to
       { RWSector, don't bother to try again. Instead, just get the desired
       { sector using Flop. (This optimization was done at the last so please
       { forgive the hacked in implementation.)
       {}
       if (LastTrkReadFail = Adr[1]) then
          begin
          Status^.SoftStatus := IOEIOC + 1; {Hack}
          goto 3;                           {Pardon me}
          end;
       {}
       { Try MaxRetry times to read the track successfully.
       { Don't bother to tell the user about failures - he will see
       { enough of them later.
       {}
       MaxRetry := 4;
       {if Dens=DDens then MaxRetry := DDMaxRetry else MaxRetry := SDMaxRetry;}
       TryCnt := 0;
       LastTrkReadFail := Adr[1];  { Set it to failure till we read the track.}
       repeat
          TryCnt := TryCnt +1;
          if (TryCnt mod 4) = 0 then  {toss in recalibrate to see if it helps}
                  UnitIO( Floppy, IOPtr, IOReset, 0  , Adr, nil, status );
          UnitIO( Floppy, IOPtr, IORead, Bytes * SecPerTrk, Adr, nil, status );
          if ShowTrackErrors and (Status^.SoftStatus <> IOEIOC) then
             begin
             ErrMsg := IOErrString(Status^.SoftStatus);
             write('** Track Error: ',ErrMsg);
             write(' [SoftStatus = ',Status^.SoftStatus:1);
             writeln(', HardStatus = ', Status^.HardStatus:1:8,'] ');
             if TryCnt = MaxRetry then
                writeln('Aborting Track Read. Will get sectors individually!')
             else
                begin
                write('Retry.....');
                writeln('      Track: ',Adr[1]:1);
                end;
             end;
       until (Status^.SoftStatus = IOEIOC) or (TryCnt = MaxRetry);
3:     if Status^.SoftStatus <> IOEIOC then { read failed }
         begin
         Adr[0] := SecNum;                      { try reading just the sector }
         if not Flop( IORead, BufPtr, Bytes, Adr ) then exit( RWSector );
         if verify then
           begin
           if not Flop( IORead, IOPtr, Bytes, Adr ) then exit( RWSector ); 
           {$R-}
           for i := 0 to WordSize - 1 do
             if BufPtr^[i] <> IOPtr^[i] then                                                   begin
               VerifyError := true;
               if ShowVerifyErrors then
                  writeln('%% VERIFY ERROR on sector in RWSector!');
               goto 1;  { exit this for loop }
               end;
           {$R=}
1:         end;
         RWSector := true;
         exit( RWSector );
         end 
       else  { the track read succeeded }
          begin
          LastTrkReadFail := -1; { i.e.,the addr of no track means no failure }
          BufferedTrkNum := Adr[1];
          BufferedSecSize := Bytes;
          TrkBuffered := true;
          if Verify then VerifyTrack;
          end;
       end;

     if Verify and not Verified[SecNum] then  {VerifyTrack failed}
        begin
        {}
        { Verify the sector by itself.
        {}
        Fake.Ptr := TrkBufPtr;
        Fake.Ofst := Fake.Ofst + (SecNum - 1)*WordSize;
        Adr[0] := SecNum;
        RWSector := false;
        VerIOPtr := recast( VerBuf, IOBufPtr );
        if not Flop( IORead, Fake.IOPtr, Bytes, Adr ) then exit( RWSector );
        if not Flop( IORead, VerIOPtr, Bytes, Adr ) then exit( RWSector );
        {$R-}
        for i := 0 to WordSize - 1 do 
          if VerIOPtr^[i] <> Fake.IOPtr^[i] then
             begin
             VerifyError := true;
             if ShowVerifyErrors then
                writeln('%% VERIFY ERROR on sector in RWSector!');
             goto 2;
             end;
        {$R=}
2:      end; 

     if Bytes = DDBytes then
        SecPtr^.DD := TrkBufPtr^.DTrk[SecNum]
     else
        SecPtr^.SD := TrkBufPtr^.STrk[SecNum];

     end{if Cmd = IORead};
  
  RWSector := true;
  
end;


Function FlpBlk( Blk: integer; Buf: FlpBlkPtr; 
                 Cmd: IOCommands): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{      Read or write a 'block' of the floppy - 
{      determined by Cmd. 
{
{ Note: FlpBlk previously returned false (for failure) only if the
{       physical IO operation failed (i.e., Status^.SoftStatus <>
{       IOEIOC). FlpBlk will now return false for IOWrite whenever
{       Verify is selected and the verify fails after the prescribed
{       number of retries. Since sectors to be written are buffered
{       in a track buffer, we can not perform any verification until
{       the track buffer is actually written out by the EmptyBuffer
{       routine. For IORead, verify failure is reported using the
{       VerifyError flag instead of FlpBlk failure.
{
{-----------------------------------------------------------------}

  var FakePtr: record case boolean of
                    true: (ptr: FlpBlkPtr);
                    false:(seg,ofst: integer)
                end;
      Trk,Sec,Bytes,Inc,i: integer;
      Adr: double;
      success: Boolean;
begin
FlpBlk := true;
if (Blk < 0) or (Blk > MaxFBlock) then
    begin
    writeln('** Block ',Blk:1,' out of range');
    FlpBLk := false;
    exit(FlpBLk)
    end;
FakePtr.ptr:=Buf;
Bytes := 512 div SecPerBlk;         { bytes in a sector }
Inc := Bytes div 2;                 { words in a sector }
FirstSector(Blk,Trk,Sec);
for i:=1 to SecPerBlk do
    begin
    Adr[0]:=Sec;
    Adr[1]:=Trk+1;           { +1 since we don't use track 0 side 0 }
    success := RWSector(Cmd,recast(FakePtr.Ptr,IOBufPtr),Bytes,Adr);
    if not success then begin
                        FlpBlk := false;
                        exit(FlpBLk);
                        end;
    if i <> SecPerBlk then
        begin
        FakePtr.Ofst:=FakePtr.Ofst+Inc;
        NextSector(Trk,Sec)
        end
    end
end { FlpBlk }; 


Procedure Init;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Initialize the necessary things for a Floppy run 
{     (cursor, necessary variables, etc.).
{
{-----------------------------------------------------------------}

var i:integer; c:char

begin
     Prompt := Concat ('FLOPPY', CmdChar);
     
     HaveCurDate := false;
     CurDate.Day:=LastModDay;
     CurDate.Mon:=LastModMonth;
     CurDate.Year:=LastModYear-#110;
     
     DoDumRead:=true;
     Rad50[0] := ' ';
     DirList := NIL;

     for i := 0 to 25 do Rad50 [i + 1] := chr (ord('A') + i);
     Rad50[27] := '$';
     Rad50[28] := '.';
     Rad50[29] := chr(#177);
     for i := 30 to 39 do Rad50[i] := chr (ord('0') + (i - 30));
     for i := 0 to STSize - 1 do Skew[i] := (1 + i * SkewFactor) mod SecPerTrk;
     
     New(Status);
     New(0,256,FlpBuf);
     New(0,256,VerBuf);
     New(DirSegBuf);  
     
     New( 0, 4, TrkBufPtr );   { allocate the track buffer } 
     New( 0, 4, VerBufPtr );
     for i := 1 to SecPerTrk+1 do Dirty[i] := false;  
     for i := 1 to SecPerTrk do Verified[i] := false;
     SomeDirty := false;
     BufferedTrkNum := -1;
     TrkBuffered := false;
     BufferedSecSize := 0;

     {}
     { For Perq2, we will hide double density errors in order to make
     { the apparent hardware problem on EIO Floppy less obnoxious.
     { Also we will try harder on Verify.
     {}
     SDMaxRetry := 10;
     if Cf_IOBoard = Cf_EIO then
        begin
        DDMaxRetry := 20;
        HidingDDensErrors := true;
        end
     else
        begin
        DDMaxRetry := 10;
        HidingDDensErrors := false;
        end;
     ShowTrackErrors := false;
     ShowVerifyErrors := false;
     LastTrkReadFail := -1;
end { Init };


Procedure FindSidesAndDensity(askIfFail: boolean);
{-----------------------------------------------------------------
{
{ Abstract:
{    
{     Accesses the floppy to discover its density.
{     If floppy is not loaded or ready, the answer will be 1 side, single
{     density.  This procedure sets the variables assocociated 
{     with numSides and density.
{ Parameters: if askIfFail, then if the formatcheck fails, asks the user
{              if should continue of if should abort.
{
{-----------------------------------------------------------------}
var Adr: Double;
    
  Function DoTry(numTries: Integer; newDens: DensityTypes): boolean;
    {-----------------------------------------------------------------
      Abstract: Try to access the floppy.
      Parameters: numTries - the number of tries to make.  If zero, the
                             status is changed but no tries are made.
                             If one, then one try is made, etc.  If greater
                             than one and finds error, reports it.  If one,
                             no errors are reported.
                  newDens - the density to set to.
      Returns: True if success; false if failure
    -----------------------------------------------------------------}
     var tries: integer;
         bytes: integer;
         msg: String;
         firsttime : Boolean;
     begin
     DoTry := true;
     Dens := newDens;
     FirstTime := True;
     with SetStatus do
       begin
       ByteCnt:=3;
       FlpDensity := Shift(ord(Dens = DDens),6);
       FlpHeads := ord( 2 {Sides} - 2) + 1;
       FlpEnable:=true
       end;
     IOPutStatus(Floppy,SetStatus);
     
     tries := 0;
     while tries < numTries do
        begin
        if Dens = SDens then Bytes := 128 else Bytes := 256;
        UnitIO(Floppy,recast(FlpBuf,IOBufPtr),IORead, Bytes, Adr, nil, Status);
        if (Status^.SoftStatus = IOEIOC) then exit(DoTry);
        tries := tries + 1;
        if status^.SoftStatus = IOEDNR then 
            begin
            if FirstTime Then Writeln('* Floppy is not ready insert floppy, or type ^C to abort.');
            tries := 0;
            end;
        firsttime := False;
        if numTries <> 1 then
          begin
          msg := IOErrString(Status^.SoftStatus);
          if Dens = SDens then Write('** Single') else write('** Double');
          WriteLn(' density read failed: ', msg);
          end;
        end;
     DoTry := false;
     end; {DoTry}

begin
  
   unformatted := false;
   Adr[0] := 2;
   Adr[1] := 1;  {track 1 on side 1, sector 2}

   {** First try once for each of single and double density.  If fails, then
       try multiple times to see if transient failure, then ask user **}
   
   if not DoTry(1, SDens) then {try double density}
     if not DoTry(1, DDens) then
        if not DoTry(5, SDens) then
           if not DoTry(5, DDens) then
              begin
              WriteLn(Chr(7),'* This floppy does not appear to be formatted!');
              If askIfFail then
                if GetConfirm( NullIdleProc, True, 
                    '* Continue anyway? ', 2, Switches) = 1
                  then begin
                       If GetConfirm( NullIdleProc, True, 
                                   '   Single Density? ', 1, Switches) = 1
                         then if not DoTry(0, SDens) then;
                       end
                else Unformatted := True
              else Unformatted := true;
              if UnFormatted then Exit (FindSidesAndDensity);
              end;
   
   {set Density parameter for side reading}
   
   if Dens = DDens then SecPerBlk := 2
     else SecPerBlk := 4;

   
   if Sides = 1 then MaxFBlock := (STracks * SecPerTrk) div SecPerBlk
     else MaxFBlock := (DTracks * SecPerTrk) div SecPerBlk; 

end; {FindSidesAndDensity}


Procedure writeChar (c: char; var f: Text; var cnt: integer);
begin
   if c <> ' ' then write(f, c:1) else cnt := cnt + 1;
end { WriteChar };
   

Procedure DoDirectory (FlpFile, OutFileName: String; var FileOpen: Boolean);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     List the floppy's directory to console or a 
{     specified file.
{
{ Parameters:
{
{     FlpFile - a user-specified floppy file name. If no
{               name is specified, a directory of all of
{               the files on the floppy is written, otherwise
{               a directory of the specified file is.
{
{     OutFileName - a user-specified disk file name. If
{               no name is specified output is directed
{               to console, otherwise it is put into the
{               specified output file.
{
{     FileOpen - since only one output file may be specified
{               but any number of input files may be, this
{               Boolean tells whether or not the output
{               file has already been opened.
{
{-----------------------------------------------------------------}


var Selective, Match, WildN, WildE, VName: boolean;
    WantN0, WantN1, WantExt,  temp: integer; 
    TmpDir: record case integer of
          0: (RT: DirSegPtr);
          1: (seg,ofst: integer);
          2: (Flp: FlpBlkPtr)
     end;
     NumDirSegs,
     FBlock, EntrySize, DSegs, CurSeg, CurBase: integer; 
     a, b, c: char;
     Date: DateRec;
     TotFiles, TotFree,  cnt: integer;
     unUsed, v1, v2, v3, short: boolean;
     FID: FileId;
     dum: integer;
     msg: string[255];
          
begin

     Short := not LongDir;
     
     { If a filename was specified, check for its validity. If 
     { nothing specified, user wants directory of entire floppy. }
     
     if Length(FlpFile) <> 0 then
     begin
         ConvertName (FlpFile, WantN0, WantN1, WantExt, WildN, WildE, VName);
         if not VName then exit (DoDirectory);
         Selective:=true
     end
     else Selective := False;
     
     If not FileOpen then
     begin
          { If no output file specified write to console,
          { else write to specified file.                         }
          
          if Length (OutFileName) = 0 then ReWrite (outFile, 'CONSOLE:')
          else 
               
          {find out if ReWrite attempt will succeed}
               
          begin
     FSRemoveDots (OutFileName);
     FID := FSLocalLookUp (OutFileName, dum, dum);
     IF FID <> 0 then
          If Confirm then
          begin
             Writeln ('* ', OutFileName:1);
             msg :=   '  Already exists on hard disk. Delete? ';
             if GetConfirm ( NullIdleProc, True, Msg, 2, Switches) <> 1
                   then exit (DoDirectory);
          end { IF };
     
             temp:=FSEnter (OutFileName);  
             if temp = 0 then 
             begin
                 WriteLn ('** Cannot create file ', OutFileName);
                 exit (DoDirectory);
             end { IF };
             
             ReWrite (outFile, OutFileName);
             WriteLn('Output being directed to file: ',OutFileName);
          end { IF };
          FileOpen := True;
     end { IF FileOpen };
     
     TotFree := 0;
     TotFiles := 0;
     
     TmpDir.RT := DirSegBuf;

     if not EmptyBuffer 
     then exit( DoDirectory ); { make sure we read this floppy }
     if not (FlpBlk(DirStart, TmpDir.Flp, IORead)) then exit (DoDirectory);

     NumDirSegs := TmpDir.Flp^[RTSegs];

     if (TmpDir.Flp^[RTSegs] < TmpDir.Flp^[RTHighest]) or 
          (TmpDir.Flp^[RTStart] <> (DirStart + NumDirSegs * 2)) then
     begin
         writeln('** Bad Directory.');
         exit(DoDirectory)
     end { IF };
     
     if not selective then writeln(outfile);
     DSegs := TmpDir.Flp^[RTHighest];
     EntrySize := RTDate + 1 + TmpDir.Flp^[RTExtra] div 2;
     if EntrySize = RTDate + 1 then
       writeln(outFile, '** No room for "Bits In Last Block" in this directory');
     CurSeg:=1;
     while CurSeg <> 0 do
     begin
         
         if CurSeg <> 1 then
         begin
             TmpDir.RT:=DirSegBuf;
             if not FlpBlk(NumDirSegs+CurSeg*2,TmpDir.Flp,IORead) 
             then exit(DoDirectory)
         end { IF };
         
         TmpDir.Ofst := TmpDir.Ofst + 256;
         if not FlpBlk(NumDirSegs+1+CurSeg*2,TmpDir.Flp,IORead) 
         then exit (DoDirectory);
         CurSeg := DirSegBuf^[RTNext];
         FBlock := DirSegBuf^[RTStart];
         CurBase := RTEntry;
         
         while (DirSegBuf^[CurBase+RTStat] <> TermStatus) 
             and (CurBase+EntrySize < 512) do
         begin
             unused := false;
             match := true;
             if DirSegBuf^[CurBase+RTStat] = UnusedStatus then
             begin
                 if Selective or short then Match:=false
                 else 
                 begin
                      write(outFile,'     <unused>  ');
                      unused := true;
                 end { IF };
                 TotFree:=TotFree+DirSegBuf^[CurBase+RTLen]
             end { IF }
             else
                 if (Not Selective) or
                    ((((DirSegBuf^[CurBase+RTName0] = WantN0) and
                    (DirSegBuf^[CurBase+RTName1] = WantN1)) or WildN) and
                    ((DirSegBuf^[CurBase+RTExt] = WantExt) or WildE)) then
                  begin
             
             { Write the directory information in the output
             { file using Procedure WriteChar.             }
             
                       cnt := 0;
     
                       write(outfile, '     ');
                       GetRad50(DirSegBuf^[CurBase+RTName0], a, b, c);
                       writeChar(a, outFile, cnt);
                       writeChar(b, outFile, cnt);
                       writeChar(c, outFile, cnt);
     
                       GetRad50(DirSegBuf^[CurBase+RTName1], a, b, c);
                       writeChar(a, outFile, cnt);
                       writeChar(b, outFile, cnt);
                       writeChar(c, outFile, cnt);
     
                       GetRad50(DirSegBuf^[CurBase+RTExt], a, b, c);
                       write(outFile, '.', a, b, c);
                       for i := 1 to cnt do write(outfile, ' ');
                       TotFiles := TotFiles + 1
     
                  end { IF }
                  else Match := false;
             
                 if Match then
                     if short then writeLn(outFile)
                 else 
                 begin
                      write(outFile,'  ');
                      if unused then write(outFile,'---------')
                      else with Date do
                      begin
                           IntVal:=DirSegBuf^[CurBase+RTDate];
                           PrintDate(outFile,Day,Mon,Year)
                      end;
                      write(outFile,'  Block:',FBlock:5,' Size:',
                              DirSegBuf^[CurBase+RTLen]:5,' Bits:');
                      if (EntrySize = RTDate+1) or 
                         (DirSegBuf^[CurBase+RTBits] < 0)
                             or (DirSegBuf^[CurBase+RTBits] > 4096) then
                                    writeln(outFile,4096:4)
                      else writeln(outFile,DirSegBuf^[CurBase+RTBits]:4)
                 end { IF };
                 
                 FBlock:=FBlock+DirSegBuf^[CurBase+RTLen];
                 CurBase:=CurBase+EntrySize
             end { IF };
                 
     end;
     
     write(outFile, '     ', TotFiles:1,' File');
     if totFiles <> 1 then write(outFile, 's');
     if not Selective then write(outFile, ' in Use');
     writeln(outFile);
     write(outFile, '     ', TotFree:1,' Free Block');
     if TotFree <> 1 then write(outFile, 's');
     writeln(outFile);
     if not Selective then writeln(outFile);
     
     {Close(outFile);}

end { DoDirectory };


procedure Lookup (FN: String; var Blk, Siz, Bits: integer);
{-----------------------------------------------------------------
{
{ Abstract:
{
{      Looks up file FN on the floppy and returns its starting 
{      block, size and bits in the last block.  Size is 0
{      if the file is not found, -1 if the file specification is
{      invalid.
{
{ Parameters:
{
{     FN - Floppy file name.
{
{     Blk - starting block of FN.
{
{     Siz - size of file FN.
{
{     Bits - bits in last block.
{
{-----------------------------------------------------------------}

var TmpDir: record case integer of
        0: (RT: DirSegPtr);
        1: (seg,ofst: integer);
        2: (Flp: FlpBlkPtr)
      end; 
    NumDirSegs,
    FBlock, EntrySize, DSegs, CurSeg, CurBase, N0, N1, Ex: integer; 
    a, b, c: char;
    WildN, WildE, Valid: boolean;
    
begin
     ConvertName (FN, N0, N1, Ex, WildN, WildE, Valid);
     Siz := -1;
     if not Valid then exit (Lookup);
     if WildN or WildE then
     begin
         writeln ('** Illegal use of wildcards');
         exit (Lookup)
     end;
     TmpDir.RT := DirSegBuf; 
     if not EmptyBuffer then exit( LookUp ); { make sure we read this floppy }
     if not FlpBlk(DirStart, TmpDir.Flp, IORead) then exit(LookUp); 
     NumDirSegs := TmpDir.Flp^[RTSegs];
     if (TmpDir.Flp^[RTSegs] < TmpDir.Flp^[RTHighest] ) or 
        (TmpDir.Flp^[RTStart] <> (DirStart  +  NumDirSegs * 2)) then
         begin
         writeln('** Bad Directory');
         exit(LookUp);
         end;
     DSegs := TmpDir.Flp^[RTHighest];
     EntrySize := RTDate + 1 + TmpDir.Flp^[RTExtra] div 2;
     CurSeg := 1;
     Siz := 0;
     while CurSeg <> 0 do
     begin
         if CurSeg <> 1 then
         begin
             TmpDir.RT := DirSegBuf;
             if not FlpBlk(NumDirSegs + CurSeg * 2,TmpDir.Flp,IORead) then
             begin
                 siz := -1;
                 exit(LookUp);
             end;
         end;
         TmpDir.Ofst := TmpDir.Ofst + 256;
         if not FlpBlk(NumDirSegs + 1 + CurSeg * 2,TmpDir.Flp,IORead) then
         begin
             siz := -1;
             exit (LookUp);
         end;;
         CurSeg := DirSegBuf^[RTNext];
         FBlock := DirSegBuf^[RTStart];
         CurBase := RTEntry;
         while (DirSegBuf^[CurBase + RTStat] <> TermStatus) 
               and (CurBase + EntrySize < 512) do
         begin
             if DirSegBuf^[CurBase + RTStat] <> UnusedStatus then
                 if (DirSegBuf^[CurBase + RTName0] = N0) and
                    (DirSegBuf^[CurBase + RTName1] = N1) and
                    (DirSegBuf^[CurBase + RTExt] = Ex) then
                     begin
                          Blk := FBlock;
                          Siz := DirSegBuf^[CurBase + RTLen];
                          if (EntrySize = RTDate + 1) or 
                             (DirSegBuf^[CurBase + RTBits] < 0) or 
                             (DirSegBuf^[CurBase + RTBits] > 4096) then
                          Bits := 4096
                          else Bits := DirSegBuf^[CurBase + RTBits];
                          Exit(Lookup)
                     end { IF };
             FBlock := FBlock + DirSegBuf^[CurBase + RTLen];
             CurBase := CurBase + EntrySize
        end {inner WHILE }
     end { WHILE (CurSeg <> 0) }
end { Lookup };


function VerBlock (blockNum: integer; dskBlk: FlpBlkPtr): boolean;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Verify a block after transferring it.
{
{ Parameters:
{
{     BlockNum - Number of the block that will be checked.
{
{     dskBlk - harddisk block.
{
{     FlpBlkPtr - pointer to floppy block.
{
{ Returns:
{
{     True if successful, false otherwise.
{
{-----------------------------------------------------------------}
var i : integer;
begin
    VerBlock := FALSE;
    if not EmptyBuffer then exit( VerBlock );
    if not FlpBlk(blockNum, VerBuf,IORead) then exit(VerBlock);
    For i := 0 to 255 {words per block} DO
       if VerBuf^[i] <> dskBlk^[i] THEN Exit(VerBlock);
    VerBlock := TRUE;
end { VerBlock };
   
  

Procedure GetFile (FlpName: String; DiskName: PathName);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Copy a floppy file onto hard disk.
{
{ Parameters:
{
{     FlpName - String name of the floppy file.
{
{     DiskName - string name of hard disk file.
{
{-----------------------------------------------------------------}

label 2;

var Blk,Len,Bits,i,j, dum:integer; 
    FID: FileId;
    v1, v2, v3, exitLoop: BOOLEAN;
    adr: double;
    msg: STRING [255];
    MaxRetry: integer;


begin
     
     FSRemoveDots (DiskName);
     if AskSingle {we don't handle wild cards} then
     begin
        msg := ConCat('   Get [FLOPPY] ', ConCat(FlpName, ConCat(' to ',
                      ConCat(DiskName, ' ? '))));
        if GetConfirm( NullIdleProc, true,msg,2,Switches) <> 1 then exit(GetFile);
     end;

     { Make sure that the file specified as FlpName exists. }
     
     Lookup (FlpName, Blk, Len, Bits);
     if Len = 0 then
     begin
         writeln('** ', FlpName:1,' not found on Floppy.');
         exit(GetFile);
     end { IF }
     else if Len = -1 then exit(GetFile);
     
     { Make sure that the file specified as DiskFile does
     { not already exist.                                  }
     
     FID := FSLocalLookUp (DiskName, dum, dum);
     IF FID <> 0 then
          If Confirm then
          begin
             Writeln ('* ', DiskName:1);
             msg :=   '  Already exists on hard disk. Delete? ';
             if GetConfirm ( NullIdleProc, True, Msg, 2, Switches) <> 1
                   then exit (GetFile);
          end { IF };
     

     { Open a file for the copy to go into.        }
     
     FID := FSEnter(DiskName);
     
     { Make sure there's room on disk for it       }
     
     if FID = 0 then
     begin
         WriteLn('** Cannot enter file named ', DiskName);
         exit(GetFile);
     end { IF };
     
     Writeln ('  [Floppy]', FlpName:1, ' ==> ', DiskName:1);
     If Verify then Writeln ('    Verifying transfer');
     LoadCurs;
      
     VerifyError := false;    { no errors yet }   
     J := 0; 
     If Dens = DDens then MaxRetry := DDMaxRetry else MaxRetry := SDMaxRetry;
     for i := 0 to Len - 1 do
       begin
       if not FlpBlk (Blk + I, FlpBuf, IORead) then goto 2;
       FSBlkWrite(FID, I, recast(FlpBuf,pDirBlk));
       if Verify then 
         if not VerifyError then 
           J := 0
         else
           begin
           J := J + 1;
           if J > MaxRetry then
             begin
             writeln('* Too many failures, aborting');
             goto 2;
             end;
           WriteLn('* Block incorrectly transferred, retrying');   
           if J mod 4 = 0 then {recalibrate}      
             UnitIO( Floppy, RECAST(VerBuf, IOBufPtr)    
                      , IOReset, 0, Adr, nil, Status);
           i := i-1;   { What would Niklaus Wirth say? }
           VerifyError := false;
           end;
       ComputeProgress(i+1,Len);
       end;

     FSClose (FID, Len, Bits);
2:   QuitProgress;

end { GetFile };



Procedure DoRename (OldName, NewName: String);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Change the name of a floppy file from OldName to NewName.
{
{ Parameters:
{
{     OldName - Present name of floppy file.
{
{     NewName - Name that the present name should be changed to.
{
{-----------------------------------------------------------------}

var WildN, WildE, Valid: boolean;
    SN0, SN1, SEx, DN0, DN1, DEx: integer;
    tmp: DirPtr;
    dum, Siz: integer;
    msg: string[255];
    SaveConfirm: boolean;
    
begin
 
     { Check validity of OldName }
     
     ConvertName(OldName, SN0, SN1, SEx, WildN, WildE, Valid);
     If not Valid then exit(DoRename);  {error message printed by ConvertName}
     
     { Process wildcards for OldName's name (WildN) or extension (WildE) }
     
     if WildN or WildE then
     begin
         writeln('** Rename does not permit the use of wildcards.');
         exit(DoRename)
     end;
    
     { Check validity of NewName }
     
     ConvertName(NewName, DN0, DN1, DEx, WildN, WildE, Valid);
     If not Valid then exit(DoRename);  {error message printed by ConvertName}
     
     { Process wildcards for NewName's name (WildN) or extension (WildE) }
     
     if WildN or WildE then
     begin
         writeln('** Rename does not permit the use of wildcards.');
         exit(DoRename)
     end;
     
     If not LoadDirectory then exit(DoRename);
     
     
     if AskSingle {we don't handle wild cards} then
     begin
        msg := ConCat('   Rename ', ConCat(OldName, ConCat(' to ',
                      ConCat(NewName, ' ? '))));
        if GetConfirm( NullIdleProc, true,msg,2,Switches) <> 1 then exit(DoRename);
     end;

     { Travel through the floppy directory looking for OldName. }

     Tmp := DirList;
     While tmp <> NIL do
     begin
        if tmp^.status = PermStatus then
           if (tmp^.name0=SN0) and (tmp^.name1=SN1) and (tmp^.ext=SEx) then
           begin
               LookUp(NewName, dum, Siz, dum);
               if Siz > 0 then {it already exists}
               begin
                  if Confirm then
                  begin
                     msg := ConCat('   ', ConCat(NewName,
                                  ' already exists on FLOPPY! Delete? '));
                     if GetConfirm( NullIdleProc, true, msg, 2, Switches) <> 1 then 
                        exit(DoRename);
                  end;
                  {delete it!}
                  SaveConfirm := Confirm;
                  Confirm := false;
                  DoDelete(NewName, false);
                  Confirm := SaveConfirm;
               end;
               tmp^.name0 := DN0;
               tmp^.name1 := DN1;
               tmp^.ext := DEx;
               Writeln('   ', OldName, ' renamed to ', NewName);
               if not WriteDirectory then exit(DoRename);
               UnLoadDirectory;
               exit(DoRename);
           end;
        tmp := tmp^.next;
     end;
     WriteLn('** ', OldName, ' was not found on floppy.');
     UnloadDirectory;

end { DoRename };



Procedure DoCompare (DiskFile, FlpFile: String);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     See if a disk and a floppy file are identical.
{
{ Parameters:
{
{     DiskFile - Name of the disk file.
{
{     FlpFile - Name of the floppy file to be compared with it.
{
{-----------------------------------------------------------------}

Label 1, 2;

var fBlk, fLen, fBits, dLen, dBits, i, j,  numbits: integer; 
    dFID: FileId;
    pDskBlk: pDirBlk; 
    bad: boolean;
    SavedDiskName: PathName;

Procedure WaitForConfirm;
  begin
  if wait then 
     begin
     Write('* type CR to continue: ');
     readln;
     end;
  end;
  
begin
       
     pDskBlk := RECAST (VerBuf, pDirBlk);
     
     { Make sure that the disk file exists.  }
     
     SavedDiskName := DiskFile;
     dFID := FSSearch(FSSysSearchList, DiskFile,dLen, dBits);
     FSRemoveDots(DiskFile);
     if dFID = 0 then
         begin
         writeln('** ', SavedDiskName:1, ' was not found on the hard disk.');
         WaitForConfirm;
         exit(DoCompare)
         end
     else if dLen <= 0 then exit(DoCompare);
     
     { Make sure that the floppy file exists. }
     
     Lookup (FlpFile, fBlk, fLen, fBits);
     if fLen = 0 then
     begin
         writeln('** ', FlpFile:1, ' was not found on the floppy.');
         WaitForConfirm;
         exit(DoCompare);
     end
     else if fLen = -1 then exit(DoCompare);
     
     { Compare the lengths of the two files before reading
     { them. If the lengths aren't identical, assume that
     { the files are different and exit.                  }
     
     writeln('   Compare ',DiskFile:1,' with [FLOPPY]', FlpFile:1);
     if (dLen <> fLen) or (dBits <> fBits) then
     begin
          Writeln('** ', FlpFile:1, ' length: ',flen:1,'|',fbits:1, ' is not equal to');
          Writeln('        ', DiskFile:1, ' length: ',dlen:1,'|',dbits:1);
          WaitForConfirm;
          exit(DoCompare);
     end;
     
     bad := false;
     LoadCurs;
     
     { Now read the files and compare them.            }
     
     for i := 0 to dLen - 2 do
     begin
         ComputeProgress(i+1,dLen);
         if not FlpBlk(fBlk+i,FlpBuf,IORead) then goto 2;
         FSBlkRead(dFID,i,pDskBlk);
         for j := 0 to 255 {BlockSize} do
             if FlpBuf^[j] <> pDskBlk^.buffer[j] then 
             begin
                 bad := true;
                 WriteLn('** The files are not equal at addr ',i:1, '|',j:1);
                 WaitForConfirm;
                 goto 1;
             end { IF };
     1: end { FOR };
     
     
     { Do the last block differently to take care of extra bits. }

     ComputeProgress(dLen,dLen);
     QuitProgress;
     If not FlpBlk(fBlk+dLen-1,FlpBuf,IORead) then exit(DoCompare);
     FSBlkRead(dFID,dLen-1,pDskBlk);
     for j := 0 to (dBits div 16)-1  do  {to last even word}
         If FlpBuf^[j] <> pDskBlk^.buffer[j] then 
         begin
             WriteLn('** Files not equal at addr ',i:1, '|',j:1);
             WaitForConfirm;
             goto 2;
          end { IF };
          numBits := dBits mod 16;
          if numBits <> 0 then
          begin
              { zeros in on right }
              i := SHIFT(FlpBuf^[dBits div 16], 16- numBits);
              j := SHIFT(pDskBlk^.buffer[dBits div 16], 16- numBits);
              if i <> j then
              begin
                 WriteLn('** Files not equal at addr ',dLen-1:1, '|',
                                                       dbits-1:1);
                 WaitForConfirm;
                 goto 2;
              end { IF };
          end { IF };
     if not bad then WriteLn('Disk file ',DiskFile, ' and Floppy file ', FlpFile, ' are identical.');
  2: QuitProgress;

end { DoCompare };
     


Procedure DoType (FlpFile: String);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Type a file on the screen.
{
{ Parameters:
{
{     FlpFile - the file to be displayed on the screen.
{
{-----------------------------------------------------------------}

Const FF = chr(#014);
      Blotch = Chr (#213);   { symbol to signify EOF }

label 2;

Type PArC = Packed Array [0..511] of Char;
     FakePtr = Record
                  Case boolean of
                       true : (w: FlpBlkPtr);
                       false : (c: ^PArC);
                  end { Case };
var Blk, Len, Bits, i, j: integer; 
    FID: FileId;
    data: FakePtr;

    procedure DoFF;

      Handler CtlC;
      begin
         CtrlSPending := false;
         writeln('^C');
         IOKeyClear;
         QuitProgress;
         exit(DoType);
      end;
      
    begin
       if Wait then
       begin
          writeln;
          writeln('   ** ^Q for MORE **');
          CtrlSPending := true;
          while CtrlSPending do ;
       end;
    end{DoFF};
    
    
begin
 
     { Make sure that the file exists. If not, exit.  }
     
     Lookup (FlpFile, Blk, Len, Bits);
     if Len = 0 then
     begin
         writeln('** ', FlpFile:1,' was not found on the floppy.');
         exit(DoType);
     end { IF }
     else if Len = -1 then exit(DoType);
     
     { Write out everyting before the last block. }

     Writeln;
     data.w := FlpBuf;
     LoadCurs;
     
     for i := 0 to Len - 2 do
     begin
         ComputeProgress(i+1,Len);
         if not FlpBlk(Blk+i,data.w,IORead) then goto 2;
         For j := 0 to 511 do 
         begin
            if data.c^[j] = FF then DoFF;
            write(data.c^[j]);
         end;
     end { FOR };
     
     { Do the last block.   }
     
     ComputeProgress(Len,Len);
     
     if not FlpBlk (Blk + len - 1, data.w, IORead) then goto 2;
     for j := 0 to (bits div 8) - 1 do
     begin
        if data.c^[j] = FF then DoFF;
        write (data.c^[j]);
     end;
     Writeln (Blotch);
     
     2: QuitProgress;
end { DoType };



{$IFC not BootFloppyVers THEN} { PutFile, ZeroFloppy, and Squish aren't
                              { necessary for FileSystem floppies, hence
                              { this conditional compilation to save
                              { some space.                          }     

Procedure PutFile (DiskName: PathName; FlpName: String);
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Copy a disk file onto a floppy.
{
{ Parameters:
{
{     DiskName - name of the disk file to be copied.
{
{     FlpName - name that the floppy file will have (if
{               unspecified, it's the same as DiskName).
{
{-----------------------------------------------------------------}

label 2;

var Best,Tmp: DirPtr; FID: FileId; 
    Blks,Bits,N0,N1,Ex,i,j,temp: integer;
    WildN,WildE,Valid: boolean;
    msg: STRING[255];
    exitLoop: BOOLEAN;
    adr: double;
    SavedDiskName: PathName;
    
const RetryCnt = 10;

begin
     ConvertName(FlpName, N0, N1, Ex, WildN, WildE, Valid);
     if not Valid then Exit(PutFile);
     if WildN or WildE then
         begin
         writeln('** Illegal use of wildcards');
         exit(PutFile)
         end;
     SavedDiskName := DiskName;
     FID := FSSearch(FSSysSearchList, DiskName,Blks, Bits);
     FSRemoveDots(DiskName);
     if FID = 0 then
         begin
         writeln('** ', SavedDiskName:1, ' not found on the hard disk.');
         exit(PutFile)
         end;
     if AskSingle {we don't handle wild cards} then
     begin
        msg := ConCat('   Put ', ConCat(DiskName, ConCat(' to [FLOPPY] ',
                      ConCat(FlpName, ' ? '))));
        if GetConfirm( NullIdleProc, true,msg,2,Switches) <> 1 then exit(PutFile);
     end;

         If Blks = 0 then 
         begin
            writeln ('** ', DiskName:1, ' is empty.');
            Exit (PutFile);
         end;
         
     GetCurDate;
     if not LoadDirectory then exit(PutFile);
     Best:=nil;
     Tmp:=DirList;
     while Tmp <> nil do
         with tmp^ do
             begin
             if Status = UnusedStatus then
                 begin
                 if Size >= Blks then
                     if Best = nil then
                         Best:=Tmp
                     else
                         if Size < Best^.Size then
                             Best:=Tmp
                 end
             else if Status = PermStatus then
                 if (Name0 = N0) and (Name1 = N1) and (Ext = Ex) then
                     If Confirm then
                     begin
                          msg := CONCAT
                              (FlpName, ' already exists on floppy. Delete? ');
                          msg := concat ('* ', msg);
                          if GetConfirm ( NullIdleProc, True, Msg, 2, Switches) = 1 then
       
    { don't allow this block as the space for the new file - so if
      we should die before done, we might be able to recover the old file }
                            
                          Status:=UnusedStatus 
                          else
                         exit(PutFile)
                     end
                     else
                        Status := UnusedStatus;
             Tmp:=Next
             end;
     if Best = nil then
         begin
         writeln('** No room on floppy for ',FlpName);
         exit(PutFile)
         end;
     if Best^.Size > Blks then
         begin
         { create a new entry to take up the remaining extra space }
         new(Tmp);
         with Tmp^ do
             begin
             Status:=UnusedStatus;
             Size:=Best^.Size-Blks;
             JobChan:=0;
             Date:=CurDate;
             ExtraWords[1]:=4096;
             Next:=Best^.Next;
             StartBlock:=Best^.StartBlock+Blks;
             CurFileCnt:=CurFileCnt+1
             end
         end
     else
         Tmp:=Best^.Next;
     with Best^ do
         begin
         Status:=PermStatus;
         Name0:=N0;
         Name1:=N1;
         Ext:=Ex;
         Size:=Blks;
         JobChan:=0;
         Date:=CurDate;
         ExtraWords[1]:=Bits;
         Next:=Tmp
         end;
     
     LoadCurs;
     Writeln ('  ', DiskName:1, ' ==> [Floppy]', FlpName:1); 
     if verify then WriteLn('    Verifying transfer');
     
     {}
     { Note: If Verify is selected, it will now be done in the routine
     {       EmptyBuffer which handles the actual writing of sectors to
     {       the floppy.
     {}
     VerifyError := false;         { no errors yet }
     for i := 0 to Blks-2 do begin { transfers all but the last block }
       {assume disk read has no errors}
       FSBlkRead(FID,i,recast(FlpBuf,pDirBlk));    
       if not FlpBlk(Best^.StartBlock+i,FlpBuf,IOWrite) then goto 2;
       ComputeProgress(i+1,Blks);
       end;
     
     
     FSBlkRead(FID,i,recast(FlpBuf,pDirBlk)); {read last block}
     i := bits mod 16;
     IF i <> 0 then begin
      temp := bits div 16;            {zero out bad part}
      FlpBuf^[temp] := SHIFT(SHIFT(FlpBuf^[temp],16-i),i-16); 
      end;
     for i := ((Bits+15) div 16) to 255 {blocksize}
       do FlpBuf^[i] := 0; {zero out end of last block}

     if not FlpBlk(Best^.StartBlock+Blks-1,FlpBuf,IOWrite) then goto 2; 
     if not EmptyBuffer then goto 2;   { force the write }
 
     if not WriteDirectory then {nothing}; 
     UnloadDirectory;
     
     ComputeProgress(Blks,Blks);
     2: QuitProgress;     

end { PutFile };



Procedure ZeroFloppy;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Zero a floppy and put a new directory on it. 
{
{-----------------------------------------------------------------}
var temp: boolean;
begin
     
     with DirHead do
         begin
         SegsAvail:=4;
         ExtraBytes:=2;
         FirstBlock:=DirStart+4*2
         end;
     new(DirList);
     with DirList^ do
         begin
         Status:=UnusedStatus;
         Date:=CurDate;
         ExtraWords[1]:=4096;
         Next:=nil;
         StartBlock:=DirHead.FirstBlock;
         Size:=MaxFBlock-StartBlock
         end;
     CurFileCnt:=1;
     if not WriteDirectory then exitRoutine := true;
     UnloadDirectory

end { ZeroFloppy };



Procedure Squish;
{-----------------------------------------------------------------
{
{ Abstract:
{
{     Compact all the free space on the floppy. 
{
{ Note:
{     Squish will no longer attempt to continue from IOWrites that
{     can't be verified (when Verify is selected). I.e., it will not
{     just leave the bad block behind; instead it will abort. See
{     comments in the code below and in routine EmptyBuffer.
{
{-----------------------------------------------------------------}

label 1;

var TotFree, NextBlock, i, j: integer; 
    Tmp, Tmp1, Tmp2: DirPtr; 
    a, b, c: char;
    verify, exitLoop: BOOLEAN;
    adr: double;
    MaxRetry: integer;


begin


write(chr(7));
if not LoadDirectory then exit(Squish);
if verify then WriteLn('   Verifying compress');
if Dens = DDens then MaxRetry := DDMaxRetry else MaxRetry := SDMaxRetry;
writeln('Directory Loaded');
TotFree:=0;
CurFileCnt:=0;
NextBlock:=DirHead.FirstBlock;
Tmp2:=nil;
Tmp:=DirList;
DirList:=nil;
while Tmp <> nil do
    with Tmp^ do
        begin
        if Status = UnusedStatus then
            begin
            { Accumulate Free Space }
            writeln('Skipping ',Size:1,' blocks');
            TotFree:=TotFree+Size;
            Tmp1:=Next;
            dispose(Tmp);
            Tmp:=Tmp1
            end
        else
            begin
            if NextBlock <> StartBlock then
                begin
                write('Moving [',Size:1,'] ');
                for i:=0 to Size-1 do begin
                  VerifyError := false;
                  j := 0;
                  repeat  { read data block, verifying read if desired }
                    ExitLoop := true;
                    if not FlpBlk(StartBlock+i,FlpBuf,IORead) then goto 1;
                    if verify then if VerifyError then begin
                      j := j + 1;
                      if j >= MaxRetry
                      then WriteLn('Too many failures, using what I have.')
                      else begin 
                        ExitLoop := false;
                        WriteLn('Block ',i:1,' incorrectly read; retrying');
                        if not EmptyBuffer then goto 1;
                        if j mod 4 = 0 {recalibrate}
                        then UnitIO( Floppy, RECAST(VerBuf, IOBufPtr)
                                   , IOReset, 0, Adr, nil, GStatus );
                        VerifyError := false;
                        end;
                      end;          
                  until ExitLoop;
                  VerifyError := false;
                  j := 0;
                  repeat { write data block, verifying write if desired}
                    ExitLoop := true; 
                    if not FlpBlk(NextBlock+i,FlpBuf,IOWrite) then goto 1;
                    {}
                    { Now flush the buffered sectors. Note that FlpBlk may
                    { have also (implicitly) called EmptyBuffer if the sectors
                    { in the block were split between 2 tracks. 
                    {}
                    if not EmptyBuffer then goto 1;
                   {}
                   { Pull out this chunk of code now that EmptyBuffer
                   { has been changed (out of necessity) to do the verify.
                   { Thus, we just won't permit a block to be left behind
                   { and Squish to continue.
                   {
                   {if verify then begin
                   {  if not EmptyBuffer then goto 1;
                   {  if VerifyError then begin
                   {    j := j + 1;
                   {    if j >= retrycnt 
                   {    then WriteLn('Too many failures, leaving bad block')
                   {    else begin
                   {      ExitLoop := false;
                   {      Writeln( 'Block ', i:0
                   {             , ' incorrectly written: retrying' );
                   {      if j mod 5 = 0 {recalibrate}
                   {      then UnitIO( Floppy, RECAST(VerBuf, IOBufPtr)
                   {                 , IOReset, 0, Adr, nil, GStatus );
                   {      exitLoop := false;
                   {      end;
                   {    end;
                   {  end;
                   {}
                  until ExitLoop;
                end;
                StartBlock:=NextBlock
                end;
            GetRad50(Name0,a,b,c);
            write(a,b,c);
            GetRad50(Name1,a,b,c);
            write(a,b,c,'.');
            GetRad50(Ext,a,b,c);
            writeln(a,b,c);
            NextBlock:=NextBlock+Size;
            CurFileCnt:=CurFileCnt+1;
            if DirList = nil then
                DirList:=Tmp
            else
                Tmp2^.Next:=Tmp;
            Tmp2:=Tmp;
            Tmp:=Next
            end
        end;   new(Tmp1);
with Tmp1^ do
    begin
    Status:=UnusedStatus;
    Date:=CurDate;
    ExtraWords[1]:=4096;
    Next:=nil;
    StartBlock:=NextBlock;
    Size:=TotFree
    end;
if DirList = nil then
    DirList:=Tmp1
else
    Tmp2^.Next:=Tmp1;
CurFileCnt:=CurFileCnt+1;
writeln('Writing the Directory');
if not WriteDirectory then {nothing};
UnloadDirectory;
exit(Squish);
1: begin
   WriteLn('** Sorry, your Floppy Directory is DEAD');
   exit(Squish);
   end;

end { Squish };

{$ENDC}



Procedure DoDelete (FlpName: String; Unload: Boolean); 
{----------------------------------------------------------------
{
{ Abstract:
{
{     Delete a file from a floppy. This destroys the
{     file's name in the directory, not the file; the
{     space that the file has occupied can then be
{     overwritten.
{
{ Parameters:
{              
{     FlpName - name of the file to be deleted.
{     Unload - true if directory is to be unloaded.
{
{-----------------------------------------------------------------}

var Tmp: DirPtr; 
    N0, N1, Ex: integer; 
    NotFound, WildN, WildE, Valid, Deld: boolean;
    i: integer; a, b, c: char;
    msg: string[255];
    DoIt: boolean;
    
begin
     Deld := false;          { true if any files have been deleted }
     ConvertName(FlpName, N0, N1, Ex, WildN, WildE, Valid);
     if not Valid then exit(DoDelete);
     if not LoadDirectory then exit(DoDelete);
     Tmp := DirList;
     NotFound := true;
     while Tmp <> nil do
         with Tmp^ do
             begin
             if Status = PermStatus then
                 if (((Name0 = N0) and (Name1 = N1)) or WildN)
                   and ((Ext = Ex) or WildE) then
                   begin
                     NotFound := false;
                     msg := 'Delete ';
                     i  :=  length(msg);
                     Adjust(msg, i+12);
                     GetRad50(Name0, a, b, c);
                     msg[i+1]  :=  a; msg[i+2] := b; msg[i+3] := c;
                     GetRad50(Name1, a, b, c);
                     msg[i+4]  :=  a; msg[i+5]  :=  b; msg[i+6] := c;
                     msg[i+7]  :=  '.';
                     GetRad50(Ext, a, b, c);
                     msg[i+8]  :=  a; msg[i+9]  :=  b; msg[i+10]  :=  c;
                     msg[i+11]  :=  '?';
                     msg[i+12]  :=  ' ';
                     DoIt := true;
                     if Confirm then
                        if GetConfirm ( NullIdleProc, True, Msg, 1, Switches) <> 1 then
                           DoIt := false;
                     if DoIt then
                     begin
                         Status := UnusedStatus;
                         Deld := true;
                         msg := SubStr(msg, i+1, 10);
                     end
                  end;
             Tmp := Next
             end;
     if NotFound then writeln('** ', FlpName:1, ' was not found on the floppy.')
     else
      { only write it if we deleted something }
     if Deld then if WriteDirectory then  Writeln('   ', msg:1, ' deleted.');     if unload then UnloadDirectory;
     
end { DoDelete }.

