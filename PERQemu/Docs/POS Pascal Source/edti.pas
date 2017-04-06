{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module EditorInitialize;

{
{  copyright 1983  Three Rivers Computer Corporation
{
{-----------------------------
{ Change Log:
   16 Feb 83  V2.8  WJHansen
       Adapt for LandScape monitor.
       Speed up init by not writing blocks in the swap file.
{
{   5 Aug 82  V2.6  WJHansen
{      Add comment containing new cursor for LinePointer
{      Initialize LastButton 
{
{  20 May 82  V2.4  WJHansen
{      Initialize the rest of the Positions accessed by FixUp.
{
{  11 May 82  V2.3  WJHansen
{      Initialize  PFirst and PLast to avoid bomb in FixUp after delete.
{----------------------------}

exports procedure EditInit;




private

 imports Editor from Editor;

 imports EditorUtilities from EditorU;
{$IFC KeySelection THEN}
 imports EditorKeySelection from EditorK;
{$ENDC}
 imports Helper      from Helper;
 imports Memory      from Memory;
 imports FileSystem  from FileSystem;
 imports FileUtils   from FileUtils;
 imports IO_Others   from IO_Others;
 imports CmdParse    from CmdParse;
 imports Perq_String from Perq_String;
 imports System      from System;
 imports Screen      from Screen;
{$ifc ReplayVersion then}
 imports DiskIO      from DiskIO;
 imports AllocDisk   from AllocDisk;
{$endc}



 {-- initialization --}
    
       
 procedure EditInit;
 var i: integer;
     P: pChunk;
     FileNumber: FileId;
{$ifc ReplayVersion then}
     Blocks, Bits: Integer;
{$endc}
     tch : char;
     
     
  procedure InitEmpty;
  var BufferSegment: integer;
  begin { InitEmpty }
   if DEBUG2 then Writeln('enter InitEmpty');
   CreateSegment(BufferSegment, MaxMemPage + 1, 1, MaxMemPage + 1);
   SetMobility(BufferSegment,UnSwappable);
   EmptyFirst.Chunk := nil;
   for i := MaxMemPage downto 0 do with Txt[i] do
    begin New(BufferSegment,256,Buffer);
     TPage := SwapFile * (MaxDiskBlock+1) + i;
(*
     FSBlkWrite(IdFile[SwapFile],i,Recast(Buffer,pDirBlk));
*)
     Dirty := false;
     Age := 0;
     NewChunk(P);
     with P^ do
      begin CPage := SwapFile * (MaxDiskBlock+1) + i;
       First := 0;
       Length := MaxLength;
       OrderP := 0;
       OrderC := 0;
       Next := EmptyFirst.Chunk;
       if EmptyFirst.Chunk <> nil then EmptyFirst.Chunk^.Prev := P
       else EmptyLast.Chunk := P;
       EmptyFirst.Chunk := P
      end
    end;
   LastMemPage := MaxMemPage;
   Pages := SwapFile * (MaxDiskBlock+1) + MaxMemPage + 1;
   EmptyFirst.Chunk^.Prev := nil;
   EmptyFirst.Offset := 0;
   EmptyLast.Offset := MaxOffset;
   if DEBUG2 then Writeln('exit InitEmpty')
  end { InitEmpty };
  
  
  procedure InitFilled;
{$ifc ReplayVersion then}
  var IdBlock: ptrDiskBuffer;
      Disk, Part: Integer;
{$endc}
  
  
   procedure ReadFile;
   var Block: integer;
{$ifc not ReplayVersion then}
       FileNumber, Blocks, Bits: Integer;
{$endc}
       NameWanted: String;
       P, Q: Position;
       T: pChunk;
       TxtIndex: integer;
   begin { ReadFile }
    CreatingFile := False;
{$ifc ReplayVersion then}
    if Replay = NotReplaying then
     begin
{$endc}
      repeat
       NameWanted := FileName;
       if RemoveQuotes(FileName) then {do nothing};
       FileNumber := FSExtSearch(
                        FSSysSearchList,
                        ' .Pas .For .Dat .Micro .Cmd .Dfs .Prose ',
                        FileName,     {modified}
                        Blocks, 
                        Bits);
       if FileNumber = 0 then
        begin
         FSRemoveDots(FileName);
         Writeln;
         Writeln('** ', FileName, ' not found.');
         Writeln('** Type new name or RETURN to create ', FileName);
         Writeln;
         Write('File name', CmdChar, ' ');
         if Eoln then CreatingFile := True
         else Read(FileName);
         Readln
        end
      until (FileNumber <> 0) or CreatingFile;
{$ifc ReplayVersion then}
      SendTranscript(FileNumber);
{$endc}
      LastFileName := NameWanted;      {set default file in system}
{$ifc ReplayVersion then}
     end
    else { replaying } CreatingFile := FileNumber = 0;
{$endc}
    if FileNumber <> 0 then
     begin
      Write('Reading ', FileName);
      IdFile[EditFile] := FileNumber;
      Block := 0;
      LeftPart.Chunk := nil;
      RightPart.Chunk := nil;
      while Block < Blocks do
       begin NewChunk(T);
        with T^ do
         begin CPage := EditFile * (MaxDiskBlock+1) + Block;
          First := 0;
          Length := 512;
          OrderP := 0;
          OrderC := 0;
          Next := nil;
          Prev := nil
         end;
        if LeftPart.Chunk = nil then
         begin LeftPart.Chunk := T;
          LeftPart.Offset := 0
         end
        else
         begin RightPart.Chunk^.Next := T;
          T^.Prev := RightPart.Chunk
         end;
        RightPart.Chunk := T;
        RightPart.Offset := 0;
        Block := Block + 1
       end;
      if LeftPart.Chunk = nil then
       begin LeftPart := EmptyFirst; RightPart := EmptyFirst end
      else
       begin T^.Length := Bits div 8;
        RightPart.Offset := T^.Length - 1
       end;
      CreatingFile := EQ(LeftPart,RightPart)
     end
   end { ReadFile }; 
  
  var
     L: 0..MaxNLine;  
     
  begin { InitFilled }
   if DEBUG2 then Writeln('enter InitFilled');
   LeftPart := EmptyFirst;
{$ifc ReplayVersion then}
   if Replay <> NotReplaying then
    begin
     Writeln;
     NextTranscript;
     FileNumber := TranscriptWord;
     NextTranscript;
     if FileNumber = 0 then
      begin
       Writeln('** Replay transcript of file creation.');
       FileName := ''
      end
     else
      begin
       Disk := WhichDisk(FileIdToSegId(FileNumber));
       if not DiskTable[Disk].InUse then
        begin
         Writeln('** Correct device no longer mounted.');
         Editing := False;
         Exit(EditInit)
        end;
       Part := WhichPartition(FileIdToSegId(FileNumber));
       if Part <> 0 then
        if not PartTable[Part].PartInUse then Part := 0;
       if Part = 0 then
        begin
         Writeln('** Correct partition no longer mounted.');
         Editing := False;
         Exit(EditInit)
        end;
       New(0,256,IdBlock);
       FSBlkRead(FileNumber,-1,Recast(IdBlock,pDirBlk));
       FileName := DiskTable[Disk].RootPartition;
       AppendChar(FileName, ':');
       AppendString(FileName, PartTable[Part].PartName);
       AppendChar(FileName, '>');
       AppendString(FileName, IdBlock^.FSData.FileName);
       Blocks := IdBlock^.FSData.FileBlocks;
       Bits := IdBlock^.FSData.FileBits;
       Dispose(IdBlock);
       Writeln('** Replay transcript on file ', FileName)
      end;
     if not DemoSwitch then begin
       Write('** Type RETURN to begin.');
       Readln;
     end;
    end;
{$endc}
   if FileName = '' then
    begin
{$ifc ReplayVersion then}
     if Replay = NotReplaying then SendTranscript(0);
{$endc}
     CreatingFile := true
    end
   else ReadFile;
   Attach(Cursor1,EmptyFirst,WriteCursor);
   FilledFirst := Cursor1.Pos;
   Cursor1.Ch := CR;
   Add1C(Cursor1);
   Cursor1.Ch := LF;
   Add1C(Cursor1);
   FilledLast := Cursor1.Pos;
   Cursor1.Ch := Etx;
   Add1C(Cursor1);
   EmptyFirst := Cursor1.Pos;
   Detach(Cursor1);

   if CreatingFile then begin
     Add(FilledFirst,2,SourceFirst);  
     Add(FilledLast, -1, SourceLast);
   end 
   else begin
     SourceFirst := LeftPart;
     SourceLast  := RightPart;
   end;
   SelectFirst  := SourceFirst;     SelectLast  := SourceFirst;
   Attach(Cursor2,SelectFirst,ReadCursor);
   if Cursor2.Ch = CR then
    begin Add1C(Cursor2);
     if Cursor2.Ch = LF then SelectLast := Cursor2.Pos
    end;
   Detach(Cursor2);
  
   {initialize position variables so there will be no bomb in Fixup 
   during the following call on Split}

   InsertFirst  := FilledLast;      InsertLast  := FilledLast;
   DeleteFirst  := FilledLast;      DeleteLast  := FilledLast;
   FindFirst    := FilledLast;      FindLast    := FilledLast;
   ReplaceFirst := FilledLast;      ReplaceLast := FilledLast;
   PFirst       := FilledLast;      PLast       := FilledLast;
   ScreenFirst  := SourceFirst;     ScreenLast  := FilledLast;
{$IFC KeySelection THEN}
   NoteFirst    := SourceFirst;     OldFirst    := SourceFirst;
{$ENDC}
   Display := SourceFirst;          Tmp := SourceFirst;
   DrawCursor.Pos := SourceFirst;
   for L := 0 to LastLine do begin
      Ln[L].Start := ScreenFirst;  Ln[L].Finish := ScreenLast;
   end;

   Split(EmptyFirst);
   if not CreatingFile then
    begin Split(FilledLast);
     Join(FilledFirst,LeftPart);
     Join(RightPart,FilledLast)
    end;
   FilledFirst.Chunk^.OrderP := 0;
   FilledFirst.Chunk^.OrderC := 0;
   ReOrder(FilledFirst);
   if DEBUG2 then Writeln('    exit InitFilled')
  end { InitFilled };
  
  
  procedure GetParameters;
  var Word: string;
      TBits: Integer;
      NextCh: char;
      isSwitch: Boolean;
      
  begin { GetParameters }
   FileName := '';
   DEBUG1 := false;
   DEBUG2 := false;
   DEBUG3 := false;
   NextCh := NextId(Word, isSwitch);        {remove 'Editor'}
   while NextCh <> CCR do begin
      if NextCh = '=' then
          StdError(ErNoSwParam, Word, True);
      if NextCh = '~' then
          StdError(ErNoOutFile, 'Editor', True);
      if NextCh = ',' then
          StdError(ErOneInput, 'Editor', True);
          
      NextCh := NextId(Word, isSwitch);    {get next item}
      if not isSwitch then 
          if FileName = '' then
              FileName := Word
          else StdError(ErNoOutFile, 'Editor', True)
      else begin  {process switch}
         CnvUpper(Word);
         if Word = 'D1' then DEBUG1 := true
         else if Word = 'D2' then DEBUG2 := true
         else if Word = 'D3' then DEBUG3 := true
{$ifc ReplayVersion then}
         else if (Pos('REPLAY', Word)=1) or (Word = 'DEMO') then begin
            if Word = 'DEMO' then begin
              DemoSwitch := True;
              Replay := ReplayFree;
            end
            else
              Replay := ReplayCommand;
            TId := FSLookUp('>Editor.Transcript',TBlocks,TBits);
            TWords := TBits div 16;
            if (TId = 0) or (TBlocks = 0) then
                StdError(ErFileNotFound, 'Editor.Transcript', True);
         end
{$endc}
         else if Pos('HELP', Word)=1 then begin
            {$IFC KeySelection THEN}
               GiveHelp('>HelpDir>EditorHelp>EditorK.Index');
            {$ELSEC}
               GiveHelp('>HelpDir>EditorHelp>Editor.Index');
            {$ENDC}
            exit(Editor);
         end
         else 
            StdError(ErBadSwitch, Word, True);
      end;
   end;
{$ifc ReplayVersion then}
   if (FileName = '') and (Replay = NotReplaying) then
{$elsec}
   if FileName = '' then
{$endc}
    begin Write('file name', CmdChar, ' ');
     Readln(FileName)
    end;
  end { GetParameters };
  
  
  procedure InitPointers;
  var PointerSegment, i, j: integer;
  begin { InitPointers }
   CreateSegment(PointerSegment,5,1,10);
   New(PointerSegment,256,TextPointer);
   New(PointerSegment,256,UpPointer);
   New(PointerSegment,256,DownPointer);
   New(PointerSegment,256,LinePointer);
   New(PointerSegment,256,ThumbPointer);
   for i := 0 to 63 do
    for j := 0 to 3 do
     TextPointer^[i,j] := 0;
   UpPointer^ := TextPointer^;
   DownPointer^ := TextPointer^;
   LinePointer^ := TextPointer^;
   ThumbPointer^ := TextPointer^;
   TextPointer^[ 2,0] := #037400;
   TextPointer^[ 3,0] := #036000;
   TextPointer^[ 4,0] := #036000;
   TextPointer^[ 5,0] := #037000;
   TextPointer^[ 6,0] := #023400;
   TextPointer^[ 7,0] := #021600;
   TextPointer^[ 8,0] := #000700;
   TextPointer^[ 9,0] := #000340;
   TextPointer^[10,0] := #000160;
   TextPointer^[11,0] := #000070;
   TextPointer^[12,0] := #000034;
   TextPointer^[13,0] := #000016;
   TextPointer^[14,0] := #000007;
   TextPointer^[15,0] := #000003; TextPointer^[15,1] := #100000;
   TextPointer^[16,0] := #000001; TextPointer^[16,1] := #140000;
                                  TextPointer^[17,1] := #100000;
   xTextPointer := 0;
   yTextPointer := 0;
   UpPointer^[3,0] := #004000;
   UpPointer^[4,0] := #016000;
   UpPointer^[5,0] := #037000;
   UpPointer^[6,0] := #077400;
   UpPointer^[7,0] := #116200;
   for i := 8 to 25 do UpPointer^[i,0] := #016000;
   xUpPointer := 4;
   yUpPointer := 0;
   for i := 0 to 22 do DownPointer^[i,0] := UpPointer^[25-i,0];
   xDownPointer := 4;
   yDownPointer := 25;
   LinePointer^[0,0] := #000000; LinePointer^[0,1] := #020000;
   LinePointer^[1,0] := #000000; LinePointer^[1,1] := #010000;
   LinePointer^[2,0] := #000000; LinePointer^[2,1] := #014000;
   LinePointer^[3,0] := #007777; LinePointer^[3,1] := #176000;
   LinePointer^[4,0] := #007777; LinePointer^[4,1] := #177000;
   for i := 5 to 8 do LinePointer^[i] := LinePointer^[8-i];
   xLinePointer := 5;
   yLinePointer := 4;
   ThumbPointer^[ 0,0] := #001600;
   ThumbPointer^[ 1,0] := #007740;
   ThumbPointer^[ 2,0] := #037770;
   ThumbPointer^[ 3,0] := #034070;
   ThumbPointer^[ 4,0] := #070034;
   ThumbPointer^[ 5,0] := #060014;
   ThumbPointer^[ 6,0] := #160016;
   ThumbPointer^[ 7,0] := #160016;
   for i := 8 to 14 do ThumbPointer^[i,0] := ThumbPointer^[14-i,0];
   xThumbPointer := 7;
   yThumbPointer := 7
  end { InitPointers };
 
 procedure InitWindowParms;
 var
   HasTitle: Boolean; 
 begin
    GetWindowParms(CurWIndX, WinOrgX, WinOrgY, WinWidth, WinHeight, HasTitle);

    {Compute usable area}
    WinOrgX := WinOrgX-3;
    WinWidth := WinWidth+7;
    WinOrgY := WinOrgY-3;
    WinHeight := WinHeight+7;
    (* don't adjust for title: can't SSetCursor up there:
         if HasTitle then begin
             WinOrgY := WinOrgY - 15;
             WinHeight := WinHeight + 15;
         end;
    *)
    
    ScreenChars := (WinWidth - XMargin - XMargin) div CharWidth;
    ScreenLines := (WinHeight- YMargin - YMargin) div LineHeight;
    if (ScreenChars < 30) or (ScreenLines < 15) then 
        StdError(ErAnyError, 'Window smaller than Editor allows', true);

    NLine := ScreenLines + FirstLine;
    LastLine := NLine - 1;
    NColumn := ScreenChars + FirstColumn;
    LastColumn := NColumn - 1;
    HomeX := WinOrgX + XMargin + FirstColumn*(-CharWidth);
    HomeY := WinOrgY + YMargin + LineHeight*(-FirstLine);
    DotsAcross := NColumn-1;
    RightLimit  := WinWidth  + WinOrgX;
    BottomLimit := WinHeight + WinOrgY;
 end;
 
 begin { EditInit }
  Writeln(EdVersion);
  InitWindowParms;

  for Files := 0 to MaxFileIndex do IdFile[Files] := 0;
  Files := 2;
  IdFile[SwapFile] := FSEnter('>Editor.Swap$');
  if IdFile[SwapFile] = 0 then
   begin Writeln;
    Writeln('Cannot open swap file.');
    Editing := False;
    Exit(EditInit)
   end;
{$ifc ReplayVersion then}
  TId := 0;
  TBlock := 0;
  TWord := 0;
  TBuffer := nil;
  Replay := NotReplaying;
  DemoSwitch := False;
  New(0,256,TBuffer);
{$endc}
  CreateSegment(ChunkSeg,2,2,10);
  GetParameters;
{$ifc ReplayVersion then}
  TabX := 0;
  TabY := 0;
  OldTabX := -1;
  OldTabY := -1;
{$endc}
  Switch := False;
  OldSwitch := False;
  LastButton := NoneB;  
  MiddleSwitch := False;
  LeftSwitch := False;
  RightSwitch := False;
  BottomSwitch := False;
  ThisLine := 0;
  ThisColumn := 0;
  Scrn := MakePtr(ScreenSeg,0,pScreen);
  RepeatCount := 0;
  Count := 0;
  Verify := false;
  AtColumn := -1;
  Direction := '>';
  Quoted := False;
  Editing := true;
  FileChanged := False;

  InitEmpty;

  DrawCursor.Attached := false;
  Attach(DrawCursor,EmptyFirst,ReadCursor);
  Detach(DrawCursor);
  Cursor1.Attached := false;
  Attach(Cursor1,EmptyFirst,ReadCursor);
  Detach(Cursor1);
  Cursor2.Attached := false;
  Attach(Cursor2,EmptyFirst,ReadCursor);
  Detach(Cursor2);

  InitFilled;

  EofDot := 0;
  SelectDot := 0;
  ScreenFDot := 0;
  ScreenLDot := 0;
{$IFC KeySelection THEN}
  OldDot := 0;
  NoteDot := 0;
{$ENDC}

  CreateWindow(1, 0, 0, 16*SScreenW, 1024, '');
  InitWindowParms;
  {debug:    
    writeln('window: (', WinOrgX:1, ',', WinOrgY:1, ')   ', 
                     WinWidth:1, 'x', WinHeight:1);
    writeln('NColumn: ', NColumn:4, '   NLine: ', NLine:4);
    writeln('HomeX:   ', HomeX:4,   '   HomeY: ', HomeY:4);
    readln;
  :gubed}

  InitPointers;
  RefreshScreen;
  with Ln[-1] do
   begin Start := ScreenFirst;
    Finish := ScreenFirst;
    Length := 0
   end;
  for tch := Chr(#000) to Chr(#377) do begin 
     lcletters[tch] := False;
{$IFC KeySelection THEN}
     AllChars[tch] := True   
{$ENDC}
  end;
  UCletters := lcletters;
{$IFC KeySelection THEN}
  LettersAndDigits := lcletters;
{$ENDC}
  for tch := 'a' to 'z' do begin
     lcletters[tch] := True;
     UCletters[chr(ord(tch)+UClesslc)] := True;
{$IFC KeySelection THEN}
     LettersAndDigits[chr(ord(tch)+UClesslc)] := True;
     LettersAndDigits[tch] := True;
{$ENDC}
  end;
{$IFC KeySelection THEN}
  for tch := '0' to '9' do LettersAndDigits[tch] := True;
  PrintChars := LettersAndDigits;
  for tch := '!' to '/' do PrintChars[tch] := True;
  for tch := ':' to '@' do PrintChars[tch] := True;
  for tch := '[' to '`' do PrintChars[tch] := True;
  for tch := '{' to '~' do PrintChars[tch] := True;
  StmtChars := PrintChars;
  StmtChars['.'] := False;
  StmtChars[';'] := False;
  StmtChars['!'] := False;
  StmtChars['?'] := False;
  PageChars := AllChars;
  PageChars[Etx] := False;
  PageChars[FF]  := False;
  LineChars := AllChars;
  LineChars[LF]  := False;
  LineChars[Etx] := False;
  OldGChar := Chr(#000);
{$ENDC}
  WordChars := ['0'..'9', 'A'..'Z', 'a'..'z'];
  ThisSelect := SelectNone;
  NeedPrompt := true;
  CurrentPointer := TextP;
  IOLoadCursor(TextPointer,xTextPointer,yTextPointer);
  if PointAllowed then
     IOSetModeTablet(RelTablet);
  MovePointer(ThisLine,ThisColumn);
  OnPointer;
  if DEBUG2 then Writeln('exit EditInit')
 end { EditInit }.

