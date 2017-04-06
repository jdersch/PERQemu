{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program TypeFile;
{---------------------------------------------------------------------
{
{ Abstract:
{    Program to type a text file to the console very swiftly.
{
{ Written by: Don Scelza
{
{ Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation.
{
{----------------------------------------------------------------------}


{-----------------------------------------------------------------------
   Change Log:

{ 15 Feb 83  V4.5   Brad Myers
{ Fixed for landscape monitor.
{

{ 21 Dec 82  V4.4   Brad Myers
{ Made handle CR correctly.
{

{ 26 apr 82  V4.3   Ellen Colwell
{ Add .FOR and .DAT as another file extension to recognize.
{

{ 19 Jan 82  V4.2   Brad Myers
{ Wait for confirmation before scrolling at end of page.
{ }

{ 29 Dec-81  V4.1   Brad Myers
{ Remove computations so push for RasterOp not reverse video.
{ StdError.
{ }

{ 21 Dec-81  V4.0   Brad Myers
{ New CmdParse.
{ }

{ 20-Nov-81  V3.8   Mark Faust
{ Fixed bug in profile reading stuff to catch PNotFound exception.
{ }

{ 19-Nov-81  V3.7   J. Strait
{ Fix bug introduced in V3.5, NewFont must be set nil if using standard font.
{ }

{ 23-Oct-81  V3.6   J. Strait
{ Fix bug in translating virtual addresses for DrawByte (special Raster-Op).
{ }

{ 19-Sep-81 V3.5    Mark Faust
{ If /Font file name isn't found then we print an error message and use the
{ existing font for that window.
{
{ Improved the command parsing for /Font:
{
{ }

{ 17-Sep-81  V3.4   Mark Faust
{ Updated help text to reflect change of CR to ^Q for continuing after FF
{ Added new option /Font:<fontfile> to use that font when typing a file.
{ Now uses the user profile under conditional compilation.
{ Moved CtlC handler to VeryFastPrint so that it can clean up properly when
{ using the /Font: option.
{
{ }

{ 13-Aug-81  V3.3   Brad Myers
{ Added blotch before file type out and fixed message when file length <> 
{ hint
{ }

{ 23-Jun-81  V3.2   Brad Myers
{ IOKeyClear on ^C
{ }

{ 19-May-81  V3.1   Brad Myers
{ Switches for no wait on ^L and help
{ Use new FSExtLookUp
{ Waits on ^Q rather than CR (and ^C will abort)
{ }

{ ??-Apr-81  V3.0  John Strait
{ Very fast print using new RasterOp entry point
{ }

{ 31-Mar-81  V2.0  Brad Myers
{ Title line; asks before ^L; blotch at end of file
{ }

{ 19-Mar-81  V1.2  Brad Myers
{ Changed PERQ.String to PERQ_String; fixed so works if BlkNum wrong or zero.
{  No FF before print
{ }

{  9-Mar-81  V1.1  Don Scelza
{ Added the calls to Progress.
{ }

{  9-Mar-81  V1.0  Don Scelza
{ Created the new version of type based on design by Brad Myers.
{ }
{----------------------------------------------------------------------}


imports System       from System;
imports CmdParse     from CmdParse;
imports FileUtils    from FileUtils;
imports IO_Unit      from IO_Unit;
imports IO_Others    from IO_Others;
imports Screen       from Screen;
imports Perq_String  from Perq_String;
imports ReadDisk     from ReadDisk;
imports UtilProgress from UtilProgress;
imports Profile      from Profile;
imports memory       from memory;  {using screenSeg}


const FirstTitle =  'TypeFile V4.4.  Type /Help for Help';
      SecondTitle = 'TypeFile V4.4.  ';
      UsingProfile = True;  { True to compile in User Profile code }
      LF = Chr(#012);
      FF = Chr(#014);
      CR = Chr(#015);
      Blotch = Chr(#213);  {^K with high bit set is EOF blotch}
      StartBlotch = Chr(#212);  {^J with high bit set is EOF blotch}
      Nul = Chr(#000);
      Del = Chr(#177);
      RopAddr = #4010; { = LOr( Shift(#4010,8), Shift(#4010,-8) ) }
      
      Extensions = ' .Pas .For .Dat .Micro .Cmd .Dfs .Doc .Prose ';
      
var
  OldFont         : FontPtr;
  NewFont         : FontPtr;

  MyFID           : FileID;
  Bits, CurIndex, 
  Blocks, CurBlock,
  JumpLines       : integer;
  CmdTable        : CmdArray;
  NumCmds         : integer;
  wait, ok        : boolean;
  ins, outs       : pArgRec;
  switches        : pSwitchRec;
  err             : String;
  
Procedure DoError(err: ErrorType; param: CString; fromProfile: Boolean);
   begin
   if fromProfile then Write('** Profile malformed:  ');
   StdError(err, param, not fromProfile);
   end;

Procedure DoHelp;
  var i: integer;
  begin
  WriteLn;
  writeln('        Type is used to print files on the PERQ display.');
  writeln('        Command line is of the form:');
  writeln('            Type FileSpec {, FileSpec} {/Switch}');
  writeln('        The valid switches are:');
  for I := 1 to NumCmds do
     writeln('            ', CmdTable[I]);
  WriteLn;
  WriteLn('        An entry can be put in the user profile to specify the default settings');
  WriteLn('        of switches and the default font.');
  WriteLn('        It has the form:  #TypeFile /Font=FontFileName');
  WriteLn('                                    /NoWait');
  Writeln;
  exit(TypeFile);
  end;


function ReadFont(FileName: PathName; fromProfile: boolean):FontPtr;
{-----------------------------------------------------------------------------
{
{ Abstract:
{   Read a font file and return a pointer to the font.  Returns nil if file
{   not found.  
{
{ Parameters:
{   FileName :string    is the file which contains the desired font.
{
{ Returns:
{   Returns a pointer to a record of type FontPtr which contains the font
{   desired.  
{
{ Side Effects: 
{   Sets FontSegID so that the segment in which the font resides can later be
{   destroyed.  Prints and error message and exits if font file not found.
{                                                                   
{ Errors:
{   Passes along exception FullMemory if not enough storage for the font.
{ 
{
{-----------------------------------------------------------------------------}
var
    FontId      :FileId;
    BlocksInFile:integer;
    CurBlock    :integer;
    BitsInLastBlock, seg  :integer;
    

begin
FontId := FSExtSearch(FSSysSearchList,' .KST ',FileName,BlocksInFile,BitsInLastBlock);
if FontId = 0 then
    begin
    if fromProfile then Write('** Profile error: ');
    write('** Font ',FileName,' not found.');
    ReadFont := nil;
    if not fromProfile then begin
                            WriteLn;
                            Exit(TypeFile);
                            end
    else begin
         WriteLn('  Using default.');
         exit(ReadFont);
         end;
    end;
CreateSegment(seg,BlocksInFile,1,BlocksInFile);
ReadFont := MakePtr(seg,0,FontPtr);
for CurBlock := 0 to BlocksInFile-1 do
    FSBlkRead(FontId,CurBlock,MakePtr(seg,256*CurBlock,pDirBlk));
end;         


procedure DoSwitches(switches: pSwitchRec; fromProfile: boolean);
{--------------------------------------------------------------------
{
{ Abstract:
{    Handles switches.
{
{--------------------------------------------------------------------}
    var i: integer;
    begin
    while switches <> NIL do
       begin
       ConvUpper(Switches^.switch);
       case UniqueCmdIndex(Switches^.switch, CmdTable, NumCmds) of
          1: if switches^.arg <> '' then
                  DoError(ErNoSwParam, 'Wait', fromProfile)
             else Wait := true;
          2: if switches^.arg <> '' then
                  DoError(ErNoSwParam, 'NoWait', fromProfile)
             else Wait := false;
          3: if switches^.arg = '' then
                  DoError(ErSwParam, 'Font', fromProfile)
             else NewFont := ReadFont(switches^.arg, fromProfile);
          4: DoHelp;
          otherwise: DoError(ErBadSwitch, switches^.switch, fromProfile);
          end;
       switches := switches^.next;
       end;
    end;

{$ifc UsingProfile then}
procedure ReadUserProfile;
{---------------------------------------------------------------------
{
{ Abstract:
{
{
{   Read use profile if present.  Profile settings can be overridden by
{   switched on command line.
{
{---------------------------------------------------------------------}

    handler PNotFound(FileName :string);
    {--------------------------------------------------------------------
    {
    { Abstract:  Handle exception raised by PFileInit when unable to find
    {            user profile. We don't do anything since not having a
    {            profile is not an error, simply an indication of bogosity.
    {
    {--------------------------------------------------------------------}

    begin
    exit(ReadUserProfile);
    end;


var PLine: CString;

begin                       
PFileInit(CurPFile,'TYPEFILE');
PLine := PFileEntry;
while PLine <> '' do
    begin
    ok := ParseStringArgs(PLine, ins, outs, switches, err);
    if not ok then DoError(ErAnyError, err, true)
    else DoSwitches(switches, true);
    PLine := PFileEntry;
    end;
end;

                        
{$endc}

Function GetFileID(var name: PathName): boolean;
{--------------------------------------------------------------------
{
{ Abstract:
{    Get the FileID for name.  
{
{ Returns: True if file found else false;
{
{ Side Effects:
{    This procedure will set the globals MyFID, Blocks, Bits, JumpLines if
{    it returns true.
{
{--------------------------------------------------------------------}
    var i: integer;
    begin
    GetFileID := false;
    if name = '' then WriteLn('** Filename is empty.')
    else if not RemoveQuotes(name) then StdError(ErBadQuote,'',false)
    else begin
         MyFID := FSExtSearch(FSSysSearchList,Extensions,name,Blocks,Bits);
         if MyFID = 0 then StdError(ErFileNotFound, name, false)
         else begin
              GetFileID := true;
              if Blocks = 0 then JumpLines := 1
              else JumpLines := 1024 div Blocks;
              ChangeTitle(SubStr(Concat(SecondTitle, name), 1, 80));
              end;
         end;
    end;
    

procedure VeryFastPrint;
{---------------------------------------------------------------------
{
{ Abstract:
{    Print the file very very quickly.
{
{---------------------------------------------------------------------}


    Handler CtlC;
    {-----------------------------------------------------------------------
    {
    { Abstract:
    {
    {   Handle control C abort. Clean up and exit.
    {
    {-----------------------------------------------------------------------}

    begin
    CtrlSPending := false;
    SCurOff;
    WriteLn('^C');
    IOKeyClear;
    if newFont <> NIL then SetFont(oldFont);
    Exit(VeryFastPrint);
    end;




  var  id : SegID;
       ptr: ptrDiskBuffer;
       hdr: ptrHeader;
       i, stop, blkCnt: integer;
       c: Char;
       
       Win: WinRange;
       X, Y, Width, Height, MaxX, LinesWritten, LinesInWindow, funct: Integer;
       HasTitle, inOr, checkNotLF: Boolean;
       
       FontP: FontPtr;
       Termination: (CharCount, ScreenWidth, ControlChar);
    
    procedure DoFF;
    {---------------------------------------------------------------------
    {
    { Abstract:
    {    Waits for ^Q on FF if should.
    {
    {---------------------------------------------------------------------}
       begin
       if wait then begin
                    WriteLn;
                    Write('  ** ^Q for MORE **');
                    SCurOn;
                    CtrlSPending := true;
                    while CtrlSPending do;
                    SCurOff;
                    IOKeyClear;
                    LinesWritten := 1;
                    end;
       if inOr then begin
                    SChrFunc(RRpl);
                    funct := RRpl;
                    inOr := false;
                    end;
       SPutChr(FF);
       SReadCursor(X, Y)
       end;
    procedure DoLF;
    {---------------------------------------------------------------------
    {
    { Abstract:
    {    Does a WriteLn and waits for ^Q if at end of page and will scroll.
    {
    {---------------------------------------------------------------------}
       begin
       if inOr then begin
                    SChrFunc(RRpl);
                    funct := RRpl;
                    inOr := false;
                    end;
       WriteLn;
       SReadCursor(X, Y);
       if wait then
          begin
          LinesWritten := LinesWritten + 1;
          if (LinesWritten >= LinesInWindow) then
                    begin
                    Write('  ** ^Q for MORE **');
                    SCurOn;
                    CtrlSPending := true;
                    while CtrlSPending do;
                    SCurOff;
                    IOKeyClear;
                    SSetCursor(x,y);
                    Write('                    ');
                    SSetCursor(x,y);
                    LinesWritten := 1;
                    end;
          end;
       end;

    procedure DoCR;
    {---------------------------------------------------------------------
    {
    { Abstract:
    {    Does a CR and changes char mode to OR.
    {
    {---------------------------------------------------------------------}
       begin
       Write(CR);
       SReadCursor(X, Y);
       inOr := true;
       funct := ROr;
       SChrFunc(ROr);
       end;

    begin
    WriteLn;
    WriteLn(' ----- ',ins^.name,' -----');
    WriteLn(StartBlotch);
    GetWindowParms(Win, X, Y, Width, Height, HasTitle);
    if newFont <> NIL then SetFont(newFont);
    FontP := GetFont;
    Width := Width - 6;
    x := X + 3;
    MaxX := X + Width;
    LinesInWindow := ((Height) div FontP^.height) - 1;
    LinesWritten := 3;
    id := FileIDToSegID(myFid);
    hdr := ReadHeader(id);  {FIBlk}
    id := hdr^.nextAdr;
    LoadCurs;
    blkCnt := 0;
    SReadCursor(X,Y);
    checkNotLF := false;
    inOr := false;
    funct := RRpl;
    while id <> DBLZERO do
      begin
      blkCnt := blkCnt+1;
      ptr := ReadDisk(id);
      hdr := ReadHeader(id);
      id := hdr^.nextAdr;
      if id = DBLZERO then 
        stop := bits div 8 {last block}
      else 
        stop := 512;
      i := 0;
      repeat
        if checkNotLF then
          begin
          if chr(ptr^.ByteData[i]) <> LF then DoCR;
          checkNotLF := false;
          end;
        while CtrlSPending do ;
        LoadExpr(ord(funct) + Shift(SScreenW, 3));  { ROP funct and screen w }
        LoadExpr(X);                  { destination X }
        LoadExpr(Y);                  { destination Y }
        LoadExpr(ScreenSeg);          { destination base }
        LoadExpr(0);
        LoadAdr(FontP);
        InLineByte( 239 {LDDW} );
        LoadAdr(ptr^);
        LoadExpr( i );
        LoadExpr( stop );
        LoadExpr( MaxX );
        LoadExpr( RopAddr );
        InLineByte( 240 {STLATE} );   { translate font and buffer addresses }
        InLineByte( #165 {7,,5} );
        InLineByte( 240 {STLATE} );   { translate screen address }
        InLineByte( #7 {0,,7} );
{
        **** This is a HACK.   This funny Raster-Op entry point is a
        **** three address instruction and there is no three address
        **** TLate.  We get away with it by:
        ****    1) Do two TLates.
        ****    2) Translate the screen address after the other two.
        ****    3) Count on the fact that the screen is not swappable.
        ****    4) Count on the fact that the two TLates and the JCS
        ****       are indivisible, but the first TLate can take a segment
        ****       fault if necessary.

        (Tos)   = Maximum X-coordinate + 1.
        (Tos-1) = Maximum byte offset + 1.
        (Tos-2) = Byte offset from the beginning of the byte array.
        (Tos-3) = Address of the byte array as an offset from the base of the
                  memory stack.
        (Tos-4) = Character set address as an offset from the base of the
                  memory stack.
        (Tos-5) = Destination base address as an offset from the base of the
                  memory stack.
        (Tos-6) = Destination Y-coordinate.
        (Tos-7) = Destination X-coordinate.
        (Tos-8) = Raster-op function.
}
        InLineByte( 191 {JCS} );
{
        (Tos)   = Current X-Coordinate.
        (Tos-1) = Next byte offset.
        (Tos-2) = Termination condition:
                    0 - Character count exhausted.
                    1 - Screen width exhausted.
                    2 - Control character encountered.
}
        StorExpr(X);
        StorExpr(i);
        StorExpr(Termination);
        SSetCursor(X,Y);
        if Termination = ControlChar then
          begin
            c := chr(ptr^.ByteData[i]);
            if c = FF then DoFF
            else if c = LF then DoLF
            else if c = CR then checkNotLF := true
            else begin
                 SPutChr(c);
                 SReadCursor(X, Y)
                 end;
            i := i + 1;
            if i >= stop then Termination := CharCount
          end
        else if Termination = ScreenWidth then DoLF;
      until Termination = CharCount;
      ShowProgress(JumpLines);
      end;
    Writeln(Blotch);
    QuitProgress;
    if newFont <> NIL then SetFont(oldFont);
    if blkCnt <> Blocks then
       begin
       WriteLn;
       WriteLn('** WARNING: Number of blocks in file (',blkCnt:1,') does not agree with length hint (',Blocks:1,').');
       WriteLn('**          The file was probably not properly closed.');
       WriteLn('** Unless the file is a directory, suggest you delete the file or fix the length');
       WriteLn('**          by running the Scavenger and rebuilding the directories with the');
       WriteLn('**          lengths checked.');
       end;
    if ins^.next <> NIL then DoFF;
    end;


Procedure HandleLine;
  var MyFID: FileID;
      i: integer;
      isSwitch,leave: Boolean;
      s: CString;
      c: Char;
  begin
  s := '';
  err := '';
  c := NextId(s, isSwitch);  {remove "TypeFile"}
  if (c<>' ') and (c<>CCR) then
      DoError(ErIllCharAfter,'TypeFile',true);
  ok := ParseCmdArgs(ins, outs, switches, err);
  repeat
     leave := true;
     if not ok then StdError(ErAnyError, err, true);
     DoSwitches(switches, false);
     if (outs^.name <> '') or (outs^.next <> NIL) then
         StdError(ErNoOutFile,'TypeFile',true);
     if (ins^.name = '') and (ins^.next = NIL) then 
        begin
        Write('File(s) to type: ');
        Readln(s);
        ok := ParseStringArgs(s, ins, outs, switches, err);
        leave := false;
        end;
  until leave;
  end;


begin
NewFont := nil;
OldFont := GetFont;
FSAddToTitleLine(FirstTitle);

    CmdTable[1] := 'WAIT      wait for ^Q when typing a ^L; (The default).';
    CmdTable[2] := 'NOWAIT    don''t wait.';
    CmdTable[3] := 'FONT      FONT=FontFile uses FontFile for the font';
    CmdTable[4] := 'HELP      print this message.';
    NumCmds := 4;
  
    wait := true;
       
{$ifc UsingProfile then}
    ReadUserProfile;    
{$endc}

HandleLine;

while ins <> NIL do
   begin
   if GetFileID(ins^.name) then VeryFastPrint;
   ins := ins^.next;
   end;
end.
