{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Patch;
{-------------------------------------------------
{
{ Patch - File Patch Utility
{ Copyright (C) 1981, 1982, 1983  Three Rivers Computer Corporation
{
{ Abstract:
{    Program to patch individual blocks of a file
{
{-------------------------------------------------}

{$Version V1.12 for POS}
{--------------------------------------------------
{  Change Log:
{
{  18 Mar 83  V1.12  Brad Myers
{   Fix so works with large disk addresses (bigger than 32000) for Sys:.
{ 
{  18 Mar 83  V1.11  WJHansen
{   Remove incorrect prompt position after text modification.
{   Add error message if block not written when editing sys:.
{   Show physical disk address even if not in Sys: mode.
{   Remove the range test on block number for reading.
{   Add control-G's to error messages.
{   Implement rap-around on cursor keys.
{   Implement selection with puck.
{ 
{  15 Feb 83 V1.10 Brad Myers
{                       Fix for landscape monitor.
{ 
{  30 Nov 82 V1.9 SLBrown
{                       Fix to include Perq.QCodes.Dfs instead of
{                       importing QCodes.Dfs
{ 
{   7 Mar 82 V1.8 Mike Kristofic
{                       Fix bug - enable reading of last block in a file
{
{  19 Nov 81 V1.7 WJH   change FS Title to have PatchVersion first
{                       use default file as initial file
{
{  17 Nov 81 V1.6 WJH   allow writing to !Disk!
{                       implement  Next, Previous, Address, Byte, Word, Go
{                       make Mode a parameter to GetVal and use for
{                          parsing decimal integer strings
{                       split ReadIn out of ReadBlk
{                       make Dump accept mode on cmd line
{                          and split into SetMode and ShowBlk
{                       put file name in FS Title line
{                       center the display of data (left to right)
{                       move header line to above data display
{                       always prompt with PATCH>
{                       use SYS: as name to access entire disk
{                       allow DEL and INS to end insertion
{                       fix backspace in insertion
{                       print uncomplemented octal in diskaddress
{
{  12 Nov 81 V1.5 WJH   change to Document form of change log
{                       put in Document headers
{                       add !Disk! as option for reading
{                       add comments after octal character codes
{                       display headers if reading !Disk!
{                       accept file name from command line 
{                       add parameter to CleanUp
{ 
{ 3 Jun 81  V1.4  Brad Myers
{                       Add print out of file length
{                       Change IO import for OS version D.
{                       Fix Backspace
{ 
{ 1 Apr 81  V1.3  Miles A. Barel
{                       Fix initialization bug
{ 
{ Date Unknown  V1.2  Miles A. Barel
{                       Creation and previous debugging and modifications
{ 
{---------------------------------------------------}

    Imports System      from System;
    Imports CmdParse    from CmdParse;
    Imports Perq_String from Perq_string;

const
{$INCLUDE Perq.QCodes.Dfs}

{    Imports QCodes      from QCodes.Dfs;}

    Imports FileSystem  from FileSystem;
    Imports DiskIO      from DiskIO;
    Imports IO_Unit     from IO_Unit;
    Imports IO_Others   from IO_Others;    {for tablet stuff}
    Imports IOErrors    from IOErrors;
    Imports Screen      from Screen;
    Imports FileAccess  from FileAccess;   {for Index}
    Imports ReadDisk    from ReadDisk;     {for FlushAll}
    
{$R-}

    Const
        PatchVersion = 'Patch V1.12';
        Leader = 9;     { Chars -1 occupied by left margin leader text }
        BodyLine = 8;   { line to start display of block contents }
        PromptLine = 44;{ where to prompt for commands }
        DpyAdrLine = 42;{ where to display result of Address command }
        HeaderLine = 2; { where to display header block }
        KSetWid = 9;    { width of standard font }
        KSetHigh = 18;  { height of standard font plus extra vertical spacing }
        OnCount = 3000; { counter values for cursor blink }
        OffCount = 1000;
    
type
     Region = (Prompt, AllBottom);
     
     bufr = record case integer of   { buffer for block of file }
                   0: (ints: array[0..255] of integer);
                   1: (byts: packed array[0..511] of 0..255);
                   2: (chrs: packed array[0..511] of char)
          end;
     BufPtr = ^bufr;
     
     CheatType = record case integer of
         1: (Addr: DiskAddr);
         2: (Lng:  long);
         3: (Dbl:  array [0..1] of integer);
         4: (B32:  FSBit32);
         end;
     
     ModeType = (Decimal, Octal, Bite, ASCII);
    
Var
        Buffer: BufPtr;         { Data Buffer }
        HdPtr: ptrHeader;       { buffer for block headers (EditDisk only) }
        FileIx: integer;        { Ix of current File }
        FileInUse: string;      { Name of most recent file }
        Changes: boolean;       { true when the current block is dirty }
        LastBlock: integer;     { Block most recently inspected/patched }
        EndBlock: integer;      { # of Last Block of the File}
        HaveBlk,                { true when a block is loaded }
        HaveFile,               { true when a file is open }
        EditDisk: Boolean;      { true to edit entire disk }

        ModeNames: CmdArray;    { list of all known modes }
        CmdNames: CmdArray;     { list of all known commands }

        HomeX,                  { cursor positions when at home }
        HomeY: integer;
        CursorOn: Boolean;      { true when cursor is displayed }
        Mode: ModeType;         { current display mode }
        Counter,                { used for cursor blink }
        Selection,              { current selection }
        PerLine,                { # entries printed on a line }
        Width: integer;         { character positions used per entry }
        
        DpyAddr: integer;       { currently displayed disk address }
        HaveDpyAddr: Boolean;   { true iff DpyAddr has a valid value }
        

Procedure PrintDiskAddr (Tag: String; DAddr: DiskAddr);
{-------------------------------------------------------------
{ Abstract:
{    print Addr in both decimal and octal  
{-------------------------------------------------------------}
var
   Cheat: CheatType;
begin
   Cheat.Addr := DAddr;
   write (Tag, AddrToField(DAddr):6, '  (#');
   if Cheat.Lng<0 then write('1') else write('0');
   Cheat.Dbl[1] := LAnd(Cheat.Dbl[1], #77777);
   writeln(Cheat.Lng:10:8, ')');
end; 


procedure OctNumber(val,width: integer);
{-------------------------------------------------------------
{ Abstract:
{    Print val in octal with minimum width of width - pad with 0's 
{-------------------------------------------------------------}
  var trik: packed record case boolean of
          true: (int:integer);
          false:(ogits:packed array[1..5] of 0..7)
        end;
      wid: integer;
begin
with trik do
    begin
    int:=val;
    if width < 6 then
        if val < 0 then
            wid:=6
        else
            begin
            wid := 5;
            while (wid > width) and (ogits[wid] = 0) and (wid > 1) do wid:=wid-1
            end
    else
        wid := width;
    while wid > 6 do
        begin
        write('0');
        wid:=wid-1
        end;
    if wid = 6 then
        begin
        if val < 0 then write('1') else write('0');
        wid:=5
        end;
    while wid > 0 do
        begin
        write(chr(ogits[wid]+ord('0')));
        wid:=wid-1
        end
    end
end { OctNumber };


procedure GetLine(var str:string; var TermChr: char);
{-------------------------------------------------------------
{ Abstract:
{    read a line of characters from keyboard
{-------------------------------------------------------------}
  var i:integer; c:char;
begin
while IOCRead(TransKey,c) <> IOEIOC do;
str:='';
while not (c in [chr(#12{^J=LF}),chr(#15{^M=CR}),chr(#33{INS}),chr(#177{DEL})]) do
    begin
    case c of
        chr(#10 {^H=BS}):  if length(str) > 0 then
                       begin
                       SClearChar(str[Length(str)], RXor);
                       str[0]:=chr(length(str)-1);
                       end;
        chr(#25 {^U}):  if length(str) > 0 then
                       begin
                       for i:=length(str) downto 1 do 
                           SClearChar(str[i], RXor);
                       str:=''
                       end;
        otherwise: begin
                   str[0]:=chr(length(str)+1);
                   str[length(str)]:=c;
                   write(c)
                   end
      end { case };
    while IOCRead(TransKey,c) <> IOEIOC do ;
    end;
TermChr := c
end { GetLine };


Procedure GetVal(Mode: ModeType; var Line:string; 
                 var RetVal:integer; var GotVal:boolean);
{-------------------------------------------------------------
{ Abstract:
{    parse a new value from LINE and put the result into VAL.  GOTVAL returns 
{       true if all is OK 
{    Assumes Cursor is positioned to enable printing of error messages 
{ Parameters:
{    Mode - what mode value is wanted.
{    Line - string to parse.  Parsed string is removed.
{    Val  - value found on front of string.
{    GotOne-set true if a valif value is found.
{-------------------------------------------------------------}
  var i:integer; Neg:boolean;
      val: Long; {use long so too-big numbers work ok}
      fake: record case boolean of
               true: (l: LOng);
               false: (high, low: Integer);
               end;
begin
if Length(Line) > 0 then        { if null, ignore command }
    begin
    val:=0;
    Neg:=false;
    GotVal:=true;       { assume we'll be OK }
    case Mode of
        Decimal: begin
                 for i:=1 to Length(Line) do
                     if Line[i] in ['0'..'9'] then
                         val:=val*10+(ord(Line[i])-ord('0'))
                     else
                          if (i=1) and (Line[1] in ['+','-']) then
                              Neg:=Line[1] = '-'
                          else
                              begin
                              WriteLn('** Bad Decimal Input');
                              GotVal:=false;
                              Exit(GetVal)
                              end;
                 if Neg then Val:=-Val
                 end;
   Octal,Bite:   begin
                 for i:=1 to Length(Line) do
                     if Line[i] in ['0'..'7'] then
                         val:=val*8+(ord(Line[i])-ord('0'))
                     else
                         if (i=1) and (Line[1] in ['+','-']) then
                             Neg := Line[1] = '-'
                         else
                             begin
                             WriteLn('** Bad Octal Input');
                             GotVal:=false;
                             Exit(GetVal)
                             end;
                 if Neg then Val:=-Val;
                 if Mode = Bite then
                     if (Val < 0) or (Val > 255) then
                         begin
                         Write('** Value ');
                         fake.l := val;
                         OctNumber(fake.low,1);
                         WriteLn(' will not fit in a byte');
                         GotVal:=false
                         end
                 end { Octal,Bite };
        ASCII:   case Length(Line) of
                     1: Val:=ord(Line[1]);
                     3: begin
                        for i:=1 to 3 do
                            if Line[i] in ['0'..'7'] then
                                val:=val*8+(ord(Line[i])-ord('0'))
                            else
                                begin
                                WriteLn;
                                WriteLn('** Bad ASCII Input');
                                GotVal:=false;
                                Exit(GetVal)
                                end;
                        if val > #377 then
                            begin
                            Write('** Value ');
                            fake.l := val;
                            OctNumber(fake.low,3);
                            WriteLn(' will not fit in a byte');
                            GotVal:=false
                            end
                        end;
                     Otherwise:
                        begin
                        WriteLn('** Bad ASCII Input');
                        GotVal:=false
                        end
                   end { case }
      end; { case }
    fake.l := val;
    RetVal := fake.low;  {use low part; unsigned so don't use shrink}
    end { if Length > 0  }
else
    GotVal:=false
end { GetVal };


procedure Position(Line,Column: integer);
{-------------------------------------------------------------
{ Abstract:
{    set output cursor to the column'th character on the line'th line 
{-------------------------------------------------------------}
begin
SSetCursor(HomeX+Column*KSetWid,HomeY+Line*KSetHigh)
end { Position };


Procedure CleanUp(Where: Region);
{-------------------------------------------------------------
{ Abstract:
{    Clear Out General Prompt Area of screen 
{-------------------------------------------------------------}
  var TmpY: integer;
      TmpHgt: integer;
begin
TmpY:=HomeY+((PromptLine-1)*KSetHigh);
TmpHgt:=SBitHeight-TmpY-5;
if Where=AllBottom then begin 
   HaveDpyAddr := False;
   TmpHgt := TmpHgt + 2*KSetHigh;
   TmpY := TmpY - 2*KSetHigh;
   end;
RasterOp(RXOR,SBitWidth-6,TmpHgt,3,TmpY,SScreenW,SScreenP,
                                 3,TmpY,SScreenW,SScreenP);
Position(PromptLine,0);
if Where=Prompt then
   Write('PATCH>');
end { CleanUp };

Procedure Help;
{-------------------------------------------------------------
{ Abstract:
{    Display command options
{-------------------------------------------------------------}
begin
CleanUp(AllBottom);
Position(DpyAdrLine,0);
WriteLn('I/M/J/L = Up/Down/Left/Right: Move Current Selection');
WriteLn('Byte/Word nnn :Select the nnn''th Byte/Word in block');
WriteLn;
WriteLn('"Ins"-key    : Alter Current Selection');
WriteLn('               ("LF" to accept and advance to next)');
WriteLn('File         : Select New Patch File');
WriteLn('Read         : Read a New Block from the Current Patch File');
WriteLn('Write        : Save the Current Block in the Current Patch File');
WriteLn('ChangeMode   : Change Patch Mode (Octal, Decimal, Byte, and ASCII');
WriteLn('Stuff        : Fill a Portion of the Current Block with a Single Value');
WriteLn('"Help"-key   : Type This Message');
WriteLn('Quit         : Exit this Program');
WriteLn;
WriteLn('Address      : Display selection as a disk address');
WriteLn('Go           : Read block whose disk address is displayed');
WriteLn('Next/Previous: Read next/previous logical block');
end { Help };

 
Procedure PrintTitle;
{-------------------------------------------------------------
{ Abstract:
{    Print Basic Prompt Line w/FileName and w/Current Block 
{-------------------------------------------------------------}
  var TmpY: integer;
      XBlk, XOff: integer;
      IndexBlk: ptrDiskBuffer;
begin
Position(HeaderLine,0);
Write('Block: ', LastBlock:1);
if not EditDisk then 
  if LastBlock=-1 then 
    write ('  (disk loc:', FileIx:7, ')')
  else begin
    Index(LastBlock, XBlk, XOff);
    new(IndexBlk);
    FSBlkRead(FileIx, XBlk, recast(IndexBlk, pDirBlk));
    write ('  (disk loc:', AddrToField(IndexBlk^.Addr[XOff]):7, ')');
    dispose(IndexBlk);
end;
case Mode of
   ASCII:   write('   Mode: ASCII');
   Bite:    write('   Mode: Bytes');
   Decimal: write('   Mode: Decimal');
   Octal:   write('   Mode: Octal');
   end;
WriteLn('   (Highest block #: ', EndBlock:1, ')');
if EditDisk then
   with HdPtr^ do begin
      writeln;
      PrintDiskAddr('     Serial Number  ', SerialNum);
      writeln      ('     Logical Block  ', LogBlock:1, 
                    '       Filler ', Filler:1);
      PrintDiskAddr('     Previous Block ', PrevAdr);
      PrintDiskAddr('     Next Block     ', NextAdr);
      end;
Position(PromptLine,0);
Write('PATCH>');
end { PrintTitle };


Procedure UnderLine(Line,StartPos,Width: integer; TurnOn: boolean);
{-------------------------------------------------------------
{ Abstract:
{    underline text on line LINE starting with the STARTPOS+1 character and 
{    proceeding for WIDTH-1 characters 
{-------------------------------------------------------------}
  const MaxWid = 8;             { max # chars we can underline }
        MaxWrds = (MaxWid*KSetWid + 15) div 8;
  var   i,TmpX,TmpY: integer;
begin
if HaveBlk then
    begin
    TmpX:=HomeX+(StartPos+1)*KSetWid;
    TmpY:=HomeY+((Line+BodyLine)*KSetHigh)+1;
    if TurnOn then
        RasterOp(RXNor,(Width-1)*KSetWid,2,TmpX,TmpY,SScreenW,SScreenP,
                                           TmpX,TmpY,SScreenW,SScreenP)
    else
        RasterOp(RXor,(Width-1)*KSetWid,2,TmpX,TmpY,SScreenW,SScreenP,
                                          TmpX,TmpY,SScreenW,SScreenP)
    end
end { UnderLine };


Procedure Select(Ix: integer);
{-------------------------------------------------------------
{ Abstract:
{ 'SELECT' the Ix'th word or byte in the current block for possible alteration 
{-------------------------------------------------------------}
begin
{ turn off line under previous selection }
if CursorOn then
    UnderLine(Selection div PerLine, Leader+(Selection mod PerLine)*Width,
        Width,false);
if Ix < 0 then
    Exit(Select);
if Mode in [Decimal,Octal] then
    if Ix > 255 then
        Ix := Ix - 256
    else
else  {ASCII, Bite}
    if IX > 511 then
        Ix := Ix - 512;
{ set up new selection }
Selection:=Ix;
CursorOn:=true;
Counter:=OnCount;
UnderLine(Selection div PerLine, Leader+(Selection mod PerLine)*Width,Width,
    true)
end { Select };


Procedure DecrSelect(Dec: integer);
{-------------------------------------------------------------
{ Abstract:
{    decrement the current selection by Dec 
{-------------------------------------------------------------}
begin
if HaveBlk then
    if Selection >= Dec then
        Select(Selection - Dec)
    else if mode in [Decimal, Octal] then
        Select(Selection+256 - Dec)
    else {Ascii, Bite}
        Select(Selection+512 - Dec);
end { DecrSelect };


Procedure Up;
{-------------------------------------------------------------
{ Abstract:
{    Move cursor to line above
{-------------------------------------------------------------}
begin DecrSelect(PerLine) end;


Procedure Down;
{-------------------------------------------------------------
{ Abstract:
{    move cursor to line below
{-------------------------------------------------------------}
begin 
if HaveBlk then
   Select(Selection+PerLine)
end;


Procedure Left;
{-------------------------------------------------------------
{ Abstract:
{    move cursor one position to left
{-------------------------------------------------------------}
begin DecrSelect(1) end;


Procedure Right;
{-------------------------------------------------------------
{ Abstract:
{    move cursor one position to the right
{-------------------------------------------------------------}
begin 
if HaveBlk then
   Select(Selection+1);
end;


Procedure SelectNth(Size: integer; var CmdLine: CString);
{-------------------------------------------------------------
{ Abstract:
{    Select Nth word or byte in buffer.
{ Parameter:
{    Size - for bytes, size is one,  for words, it is two.
{ Environment:
{    The value for N is taken from the command line or prompted for.
{-------------------------------------------------------------}
var
    Line: string;
    N: integer;
    OK: Boolean;
begin 
if CmdLine='' then begin
    WriteLn;
    Write('Select number [0]?');
    ReadLn(Line);
    end
else
    Line := CmdLine;
if Length(Line) = 0 then
    N := 0
else begin
    GetVal(Decimal, Line, N, OK);
    if not OK then Exit(SelectNth);
    end;
if N<0 then Exit(SelectNth);

{The following code adjusts N so it picks the right Selection.
Decimal and Octal modes are word addressed.  ASCII and Bite modes
are byte addressed.  If the addressing size is the same as the 
Size parameter, then N is unchanged.   If Size is words and addressing
is bytes, then N is doubled;  if Size is bytes and addressing is words, 
then N is halved. }

if not (Mode in [Decimal, Octal]) then  {ASCII, Bite}
    N := N*Size
else {Decimal, Octal}
    if Size=1 then
        N := N div 2;

Select(N);
end {SelectNth};


Procedure XYSelect(X, Y: integer);
{-------------------------------------------------------------
{ Abstract:
{    Select display entry based on X, Y coordinates.
{ Parameter:
{    X - X-coord of selection.
{    Y - Y-coord of selection.
{ Design:
{    First convert coordinates into character positions on display.
{    Then convert these to the index of the selected entry.
{    
{    The coordinates are adjusted by -KsetWid+2, +KSetHigh-2
{    to reflect the user's perception of where the cursor is pointing.
{-------------------------------------------------------------}
var
    L, C: integer;   {line & column}
    Ix: integer;     {index of entry on display}
begin 
L := (Y+KSetHigh-2 - HomeY) div KSetHigh;
C := (X-KSetWid+2  - HomeX) div KSetWid;

Ix := (L-BodyLine)*PerLine  +  (C-Leader) div Width;

if Ix<0 then exit(XYSelect);
if Mode in [Decimal,Octal] then
    if Ix > 255 then
        exit(XYSelect)
    else
else  {ASCII, Bite}
    if IX > 511 then
        exit(XYSelect);
        
Select(Ix);
end {XYSelect};


Procedure PutVal(Ix,Val: integer);
{-------------------------------------------------------------
{ Abstract:
{    put value Val in the Ix'th element of Buffer 
{    and update the display 
{-------------------------------------------------------------}
begin
case mode of
    Decimal: begin
             Position(Ix div PerLine+BodyLine,Leader+(Ix mod PerLine)*Width);
             Write(Val:8);
             Buffer^.Ints[Ix]:=Val
             end;
      Octal: begin
             Position(Ix div PerLine+BodyLine,Leader+(Ix mod PerLine)*Width+2);
             OctNumber(Val,6);
             Buffer^.Ints[Ix]:=Val
             end;
       Bite: begin
             Position(Ix div PerLine+BodyLine,Leader+(Ix mod PerLine)*Width+1);
             OctNumber(Val,3);
             Buffer^.Byts[Ix]:=Val
             end;
      ASCII: begin
             Position(Ix div PerLine+BodyLine,Leader+(Ix mod PerLine)*Width+1);
             if chr(Val) in [' '..'~'] then
                 Write(chr(Val):3)
             else
                 OctNumber(Val,3);
             Buffer^.Chrs[Ix]:=chr(Val)
             end
    end { case }
end { PutVal };


Procedure ShowBlk;
{-------------------------------------------------------------
{ Abstract:
{    Display contents of buffer in current mode
{-------------------------------------------------------------}
var
   i: integer;
begin
 write(chr(#14 {^L=FF}));
 HaveDpyAddr := False;
 for i:=0 to 31 do
     begin
     Position(i+BodyLine,0);
     if Mode = Decimal then
         Write('    ', i*PerLine:4)
     else begin
         Write('    ');
         OctNumber(i*PerLine,4);
         end;
     Write(':')
     end;
 if Mode in [Decimal,Octal] then
     for i:= 0 to 255 do
         PutVal(i,Buffer^.Ints[i])
 else
     for i:=0 to 511 do
         PutVal(i,Buffer^.Byts[i]);
 PrintTitle;        { Print a new Title Line }
end { ShowBlk };
 

Procedure SetMode(var CmdLine: CString);
{-------------------------------------------------------------
{ Abstract:
{    select patch mode  
{ Parameter:
{    CmdLine:  may contain mode
{ Calls:
{    ShowBlk - to display block in new mode
{-------------------------------------------------------------}
  label 1; var i:integer; Line: string;
begin
1:
if CmdLine<>'' then begin
    Line := CmdLine;
    CmdLine := '';
    end
else begin
    WriteLn;
    Write('Mode (Decimal, Octal, Byte{octal}, ASCII) [');
    case Mode of
       Decimal: write('Decimal');
       Octal  : write('Octal');
       Bite   : write('Byte');
       ASCII  : write('ASCII')
     end;
    write(']? ');
    ReadLn(Line);
    end;
if Length(Line) <> 0 then
    begin
    CnvUpper(Line);
    case UniqueCmdIndex(Line,ModeNames,4) of
        1: begin
           if Mode in [ASCII, Bite] then
               Selection := Selection div 2;
           Mode:=Decimal;
           PerLine:=8;
           Width:=8
           end;
        2: begin
           if Mode in [ASCII, Bite] then
               Selection := Selection div 2;
           Mode:=Octal;
           PerLine:=8;
           Width:=8
           end;      
        3: begin
           if Mode in [Decimal, Octal] then
               Selection := Selection * 2;
           Mode:=Bite;
           PerLine:=16;
           Width:=4
           end;
        4: begin
           if Mode in [Decimal, Octal] then
               Selection := Selection * 2;
           Mode:=ASCII;
           PerLine:=16;
           Width:=4
           end;
        5: begin
           WriteLn('No Such Mode');
           goto 1
           end;
        6: begin
           WriteLn(Line,' is not unique');
           goto 1
           end
      end { case }
     end;
if HaveBlk then ShowBlk;
end {SetMode};


Procedure ShowAddr;
{-------------------------------------------------------------
{ Abstract:
{    Display current selection as a disk address.  
{-------------------------------------------------------------}
var 
   Cheat: CheatType;
   WordNum: 0..255;
begin
   CleanUp (AllBottom);
   Position (DpyAdrLine, 0);
   if Mode in [Decimal, Octal] then 
       WordNum := Selection
   else
       WordNum := Selection div 2;
   Cheat.Dbl[0] := Buffer^.ints[WordNum];
   Cheat.Dbl[1] := Buffer^.ints[WordNum+1];
   write ('Words ', WordNum:1, '-', WordNum+1:1);
   PrintDiskAddr(' contain address ', Cheat.Addr); 
   DpyAddr := AddrToField(Cheat.Addr);
   HaveDpyAddr := True;
   Position(PromptLine,0);
   Write('PATCH>');
end; 


Procedure WriteBlk;
{-------------------------------------------------------------
{ Abstract:
{    writes current patch block out to the file 
{-------------------------------------------------------------}
var 
    Line: string;
    DidIt: Boolean;
begin
  WriteLn;
  if not HaveBlk then
    WriteLn('** No Block to Write')
  else begin
    if Changes then begin        { Only write if block has changed }
        if EditDisk then begin
           write ('''!Ok'' to write block? [No]');
           readln(Line);
           if line = '!Ok' then begin
              DiskIO(FieldToAddr(HardNumber, LastBlock),
                  recast(Buffer, ptrDiskBuffer),
                  HdPtr,
                  DskFirstWrite);
              Changes := False;
              WriteLn('  Done');
           end
           else
              write ('** Got "', Line, 
                        '" rather than "!Ok". Block not written.');
        end
        else begin
           FSBlkWrite(FileIx,LastBlock,recast(Buffer,pDirBlk));
           Changes:=false;
           WriteLn('  Done');
        end;
    end
    else writeln('** Block not changed, so not written.');
  end;
end { WriteBlk };


Function CheckChanges: Boolean;
{-------------------------------------------------------------
{ Abstract:
{    Check for changes to current block and write if desired.
{ Returns:
{    True if no changes  
{         or if block is written 
{         or if user wants to ignore changes.
{    False if buffer is to stay as is.
{-------------------------------------------------------------}
var
    WriteIt: Boolean;
    Line: String;
begin
if not Changes then
    CheckChanges := True
else
    begin
    write('** Changed block not Saved! Write it? [Yes] ');
    readln(Line);
    if length(Line) = 0 then
        WriteIt := True
    else if Line[1] in ['Y','y'] then
        WriteIt := True
    else
        WriteIt := False;
    if WriteIt then begin
        CheckChanges := True;
        WriteBlk;
        end
    else begin
        write('** Ignore changes? [No]');
        readln(Line);
        if length(Line)=0 then
            CheckChanges := False
        else if Line[1] in ['Y','y'] then
            CheckChanges := True
        else 
            CheckChanges := False;
        end;
    end
end;


Procedure ReadIn(Blk:integer);
{-------------------------------------------------------------
{ Abstract:
{    Reads in the indicated block.
{ Parameter:
{    Blk - Field form of address of desired block.
{-------------------------------------------------------------}
begin
if not HaveFile then
    begin
    WriteLn('** No Patch File');
    Exit(ReadIn)
    end;
if not CheckChanges then begin
    WriteLn('Aborted');
    Exit(ReadIn);
    end;
(*
if ((EndBlock>=0) and ((Blk < -1) or (Blk > EndBlock)))
   or  ((EndBlock<0) and (Blk<-1) and (Blk>EndBlock))  
   or  (EditDisk and (Blk=-1))   then
    begin
    WriteLn('** Block ',Blk,' out of Range');
    Exit(ReadIn)
    end;
*)
(* *)
LastBlock:=Blk;
if EditDisk then 
   DiskIO(FieldToAddr(HardNumber, Blk),
          recast(Buffer, ptrDiskBuffer),
          HdPtr,
          DskRead)
else begin
     if Blk<-1023 then
        begin
        WriteLn('** Block ',Blk,' too small');
        Exit(ReadIn)
        end;
     FSBlkRead(FileIx,Blk,recast(Buffer,pDirBlk));
     end;
Changes:=false;
HaveBlk:=true;
ShowBlk;           { Get it up on the screen }
Selection:=0;
CursorOn:=true;
Counter:=OnCount;
UnderLine(0,Leader,Width,true);
end {ReadIn};


Procedure ReadBlk(var CmdLine: CString);
{-------------------------------------------------------------
{ Abstract:
{    read in a new block from the file 
{-------------------------------------------------------------}
var
    Line:string; 
    Blk: integer; 
    OK: Boolean;
begin
if CmdLine='' then begin
    WriteLn;
    Write('Read Block[',LastBlock:1,']? ');
    ReadLn(Line);
    end
else
    Line := CmdLine;
if Length(Line) = 0 then
    ReadIn(LastBlock)
else begin
     GetVal(Decimal, Line, Blk, OK);
     if OK then ReadIn(Blk);
    end;
end { ReadBlk };


Procedure NewFile(FileStr: CString);
{-------------------------------------------------------------
{ Abstract:
{    Select a new file for Inspection/Alteration 
{-------------------------------------------------------------}
var
    Line, ULine: STitStrType; 
    i, temp: integer;
    Cheat: CheatType;
begin
WriteLn;
if not CheckChanges then begin
    WriteLn('Aborted');
    Exit(NewFile);
    end;
Changes:=false;
HaveFile:=false;
HaveBlk:=false;
LastBlock:=0;
CursorOn:=false;
write(chr(#14) {^L=FF});
HaveDpyAddr := False;
if FileInUse='' then 
     FileInUse := LastFileName;
Line := FileStr;
if Line='' then 
     if FileInUse <> '' then begin
         Write(PatchVersion,'   File?  [',FileInUse,'] ');
         ReadLn(Line);
         if Length(Line) = 0 then Line:=FileInUse;
         end
     else begin
         Write(PatchVersion,'   File? ');
         readln(Line);
         end;
repeat
    ULine := Line;
    CnvUpper(ULine);
    if ULine = 'SYS:' then begin
       FlushAll;            {make sure disk is up to date}
       EditDisk := True;
       FileInUse:= Line;
       HaveFile := True;
       if IO24MByte then
          Cheat.B32 := NumberPages(Winch24)
       else Cheat.B32 := NumberPages(Winch12);
       EndBlock := Cheat.Dbl[0];
       end
    else begin
      EditDisk := False;
      if length(Line) <> 0 then 
         FileIx:=FSSearch(FSSysSearchList,Line,EndBlock,i)
      else FileIx := 0;
      if FileIx = 0 then begin
         WriteLn('** ',Line,' not found');
         Write(PatchVersion,'  File[',FileInUse,']? ');
         ReadLn(Line);
         if Length(Line) = 0 then Line:=FileInUse;
         end
      else
         begin
         FileInUse:=Line;
         HaveFile:=true;
         EndBlock := EndBlock-1;
         end
      end;
until HaveFile;
Write(chr(#14 {^L=FF}));
HaveDpyAddr := False;
temp := SNumTitleChars - 32;
if length(Line)> temp then 
    Line := concat ('...', substr(Line, length(Line)-temp-4, temp-3))
else if length(Line)<temp then 
    Line := concat (Line, 
              substr('                                                ', 
                     1, temp-length(Line)));
Line := concat ('  ', Line);
Line := concat (PatchVersion, Line);
Line := concat (Line, '  Type HELP for aid');
ChangeTitle(Line);
Line := '';
ReadBlk(Line);
end { NewFile };


Procedure Insertion;
{-------------------------------------------------------------
{ Abstract:
{    Procedure to alter the contents of the current location 
{-------------------------------------------------------------}
  var Line:string; Term: char; Val: integer; GotOne: boolean;
begin
if HaveBlk then
    repeat
        CleanUp(Prompt);
        Position(PromptLine,0);
        Write('Enter New Value: ');
        GetLine(Line,Term);
        if Term=chr(#177{DEL}) then
            exit (Insertion);
        GetVal(Mode, Line,Val,GotOne);
        if not GotOne then
            exit (Insertion);
        PutVal(Selection,Val);
        Changes:=true;
        if Term = chr(#12{^J=LF}) then
            begin
            val:=Selection;
            Select(Selection+1);
            if val = Selection then 
                exit(Insertion);
            end;
    until Term in [chr(#15 {^M=CR}), chr(#33{INS})];
end { Insertion };


Procedure Stuff;
{-------------------------------------------------------------
{ Abstract:
{   Store the same value in a sequence of locations
{-------------------------------------------------------------}
  var Low,High,Val,i: integer; GotOne:boolean; Line:String;
begin
if HaveBlk then
    begin
    WriteLn;
    Write('Stuff from ');
    if Mode in [Decimal,Octal] then
        Write('(word offset)? ')
    else
        Write('(byte offset)? ');
    readln(Low);
    if (Low < 0) or (Low >= 32*PerLine) then
        begin
        WriteLn('** ',Low:1,' out of range');
        exit(Stuff)
        end;
    if mode in [Decimal,Octal] then
        Write('To (word offset)? ')
    else
        Write('To (byte offset)? ');
    readln(High);
    if (High < Low) or (High >= 32*PerLine) then
        begin
        WriteLn('** ',High:1,' out of range');
        exit(Stuff)
        end;
    Write('Stuff With? ');
    readln(Line);
    GetVal(Mode, Line,Val,GotOne);
    if GotOne then
        begin
        for i:=Low to High do
            PutVal(i,val);
        Changes:=true;
        Select(High)
        end
    end
end { Stuff };


Procedure QuitIt;
{-------------------------------------------------------------
{ Abstract:
{    Exit from Patch.  First allow saving block if changed.
{-------------------------------------------------------------}
begin
if not CheckChanges then 
    WriteLn('Aborted')
else
    Exit(Patch);
end { QuitIt };


Procedure Commander;
{-------------------------------------------------------------
{ Abstract:
{    read commands and interpret them
{-------------------------------------------------------------}
var
   CmdCh: char;
   i:integer;
   ThisCmd, TChar: CString;
   CmdLine: CString;
   PrevSwitch: Boolean;
   TabX, TabY: integer;
begin
PrevSwitch := False;
while IOCRead(TransKey,CmdCh) <> IOEIOC do
    if HaveBlk then
        begin
        if not TabSwitch then 
            PrevSwitch := False
        else if PrevSwitch then {nothing}
        else begin {switch was just pressed down}
            PrevSwitch := True;
            IOReadTablet(TabX, TabY);
            XYSelect(TabX, TabY);
            end;
        Counter:=Counter-1;
        if Counter = 0 then {blink underline}
            if CursorOn then
                begin
                CursorOn:=false;
                Counter:=OffCount;
                UnderLine(Selection div PerLine, Leader+(Selection mod PerLine)*
                    Width,Width,false)
                end
            else
                begin
                CursorOn:=true;
                Counter:=OnCount;
                UnderLine(Selection div PerLine, Leader+(Selection mod PerLine)
                    *Width,Width,true)
                end
        end;
case CmdCh of
    'J','j': Left;
    'L','l': Right;
    'I','i': Up;
    'M','m': Down;
    chr(#33 {INS}): begin
              if not CursorOn then
                  begin
                  CursorOn:=true;
                  Counter:=OnCount;
                  UnderLine(Selection div PerLine, Leader+(Selection mod PerLine)
                      *Width,Width,true)
                  end;
              Insertion;
              CleanUp(Prompt);
              end;
    chr(7 {^G=BEL}): Help;
    chr(#25 {^U}),' ',chr(#15 {^M=CR}),chr(#10 {^H=BS}):  CleanUp(Prompt);
    OtherWise: begin
               if not CursorOn then
                   begin
                   CursorOn:=true;
                   Counter:=OnCount;
                   UnderLine(Selection div PerLine, Leader+(Selection mod PerLine)
                       *Width,Width,true)
                   end;
               CleanUp(Prompt);
               CmdLine:='';
               repeat
                   case CmdCh of
                       chr(#25 {^U}): begin
                                 CleanUp(Prompt);
                                 Exit(Commander)
                                 end;
                       chr(#10 {^H=BS}): begin
                                 if Length(CmdLine) = 1 then
                                     begin
                                     CleanUp(Prompt);
                                     Exit(Commander)
                                     end;
                                 SClearChar(CmdLine[Length(CmdLine)], RXor);
                                 CmdLine[0]:=chr(Length(CmdLine)-1)
                                 end;
                       otherwise:begin
                                 Write(CmdCh);
                                 CmdLine[0]:=chr(Length(CmdLine)+1);
                                 CmdLine[Length(CmdLine)]:=CmdCh
                                 end
                     end { case };
                    while IOCRead(TransKey,CmdCh) <> IOEIOC do ;
                until CmdCh = chr(#15 {^M=CR});
                WriteLn;
                GetSymbol(CmdLine, ThisCmd, ' /', TChar);
                RemDelimiters(CmdLine, ' ', TChar);
                CnvUpper(ThisCmd);
                case UniqueCmdIndex(ThisCmd,CmdNames,13) of
                    1: Help;
                    2: WriteBlk;
                    3: ReadBlk(CmdLine);
                    4: QuitIt;
                    5: NewFile(CmdLine);
                    6: Stuff;
                    7: {NewMode} SetMode (CmdLine);
                    8: {Next} if not EditDisk then
                                  ReadIn(LastBlock+1)
                              else {EditDisk}
                                  ReadIn(AddrToField(HdPtr^.NextAdr));
                    9: {Previous} if not EditDisk then
                                  ReadIn(LastBlock-1)
                              else {EditDisk}
                                  ReadIn(AddrToField(HdPtr^.PrevAdr));
                   10: {Address} ShowAddr;
                   11: {Go}   if not EditDisk then
                                  WriteLn('** Not editing device')
                              else if not HaveDpyAddr then
                                  WriteLn('** Use ''Address'' to display one')
                              else
                                  ReadIn(DpyAddr);
                   12: {Byte} SelectNth(1, CmdLine);
                   13: {Word} SelectNth(2, CmdLine);
                   14: WriteLn('** Illegal Command - type HELP for aid');
                   15: WriteLn('** ''', ThisCmd, ''' is not unique')
                   end { case }
                end { otherwise }
  end { case }
end { Commander };
                       

{-------------------------------------------------------
{ Abstract:
{   Main program:  initialize and call command processor
{-------------------------------------------------------}
var
   TSymbol, TChar: CString;       { discard symbol in parsing UsrCmdLine }
begin
CreateWindow(0,0,0,SBitWidth,1024,PatchVersion);  {if can't create full height,
                                                   then aborts }
Write(chr(#14) {^L=FF});        { clear and home screen }
SReadCursor(HomeX,HomeY);
FileInUse:='';
LastBlock:=0;
Mode:=Octal;
PerLine:=8;
Width:=8;
HaveBlk:=false;
HaveFile:=false;
HaveDpyAddr := False;
Changes:=false;
CursorOn:=false;

CmdNames[1]:='HELP';
CmdNames[2]:='WRITE';
CmdNames[3]:='READ';
CmdNames[4]:='QUIT';
CmdNames[5]:='FILE';
CmdNames[6]:='STUFF';
CmdNames[7]:='CHANGEMODE';
CmdNames[8]:='NEXT';
CmdNames[9]:='PREVIOUS';
CmdNames[10]:='ADDRESS';
CmdNames[11]:='GO';
CmdNames[12]:='BYTE';
CmdNames[13]:='WORD';

ModeNames[1]:='DECIMAL';
ModeNames[2]:='OCTAL';
ModeNames[3]:='BYTE';
ModeNames[4]:='ASCII';

New(0,256,Buffer);
New(0,256,HdPtr);
GetSymbol(UsrCmdLine, TSymbol, ' /', TChar);
RemDelimiters(UsrCmdLine, ' ', TChar);
NewFile(UsrCmdLine);
IOSetModeTablet(RelTablet);
IOCursorMode(TrackCursor);
while true do Commander
end { Patch }.

