{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Editor;
{---------------------------------------------------------------------
{     Editor - 3RCC Perq text editor.
{     J. P. Strait          1 Jan 80.
{     Copyright (C) 1980, 1981, 1982,  Three Rivers Computer Corporation.
{
{ Abstract:
{     Allows user to examine and modify files.
{----------------------------------------------------------------------}
      
{----------------------------------------------------------------------      
{ Change Log:

   3 May 83  V2.10 JLConner
    Restore the puck buttons to old assignments.
    
   8 Mar 83  V2.9  WJHansen
    Do LoadCurs/QuitProgress only when needed if repeat count > 1.
    Fix CursOutside exceptions that happened during insertion.  (EditorU)
    Be sure selection is underlined after substituting for a line. (EditorU)
    In Insert routine, OOPS is echoed by return to start of line.
    
  15 Feb 83  V2.8  WJHansen
    Avoid long file name errors in EditorT.  (892)
    (V2.7 in interim F.2 version:)  Use spelling 'lcletters' consistently.
    Remove Rewrite(Output); Reset(Input);  they seemed to cause load error.
    Use 10 buffers and MVBW to speed up WriteFile in EditorT.
          (Is now 25% faster for writing Editor.Pas)
    Adapt for LandScape Monitor.
    Unswap scroll arrows.
    Handle PartFull in Terminate, FlushTranscript, and CleanPage (619).
    Use UtilProgress in Find.
    Find set O marker if move more than two positions.
    Set character mode after Insert and Replace.
    Speed up EditInit by not writing dummy blocks to swap file.
    Draw a line under the prompt line.
    Note that item 7 of V1.16 fixed problem (538)
    Handle ReNDir in EditorT.
    Remove Smooth scroll if near end of screen.
    Limit back scan in Thumb.
    Use Draw instead of Show for thumbing to O, N.

 
       4 Feb 83  WJH  V2.8a 1) Make OtherNames menu of commands
                               and change MenuNames to include Menu and HELP.
                            2) Fix Copy command at EOT.
                            3) Fix errors from upper left corner. (855)
                            4) Smooth scroll if select near screen.
                            5) Limit back scan in Show and ScrollDown. (554)
                            6) Swap scroll arrows.
 
{     20 Sep 82  WJH  V2.7  1) Fix select-by-move so can also click.
{                           2) Speed up DrawUnderLine.
{
{      8 Sep 82  WJH  V2.7b 1) Try new word/line select by move
{                           2) Define base end for Extend
{                           3) Allow ThumbL-2 in E command
{
{      1 Sep 82  WJH  V2.7a 1) Change TrackPointer and Extend
{                               to test various button approachs.
{                           2) Add HaveKey and popup menu
                               Export word and line primitives from EditorK.
{
{     29 Jul 82  WJH  V2.6  1) Change button assignments for new tablet.
{                              Change LastWasYellow to LastButton.
{                           2) Add Copy command.
{                           3) Make Extend shrink selection from nearest end.
{                           4) Set command after Delete to be Insert.
{                           5) Improve speed of FixUp and ReOrder.
{                           6) Test for file-too-big in ReOrder.
{                           7) Enable use of LinePointer.
{
{     25 May 82  WJH  V2.5  1) Make replay work despite ^C from F or R.
{
{     20 May 82  WJH  V2.4  1) Initialize other Positions accessed by FixUp.
{
{     11 May 82  WJH  V2.3  1) Initialize PFirst&PLast to avoid bomb.
{
{      6 May 82  WJH  V2.2  1) Mention CtlC in prompts for Find and Replace.
{                           2) Fix bug in typing INS as first key.
{                           3) Add .DAT to FSExtSearch.
{                           4) Handle CtlC in Replace.
{   
{  1-Apr-82  V2.1  Scott L. Brown
{ Add the ".FOR" extension to FSExtSearch to enable the editor to
{ find FORTRAN files.  (I do my small part to help perpetuate the
{ dreaded illness we all know as FORTRAN.)
{
{     28 Jan 82  WJH  V2.0  1) Change to V2.0
{
{     23 Jan 82  WJH  V1.16 1) insert phrases 'Change Log' and 'Abstract'
{                           2) remove Help processor to a separate module. 
{                           3) remove ' in file names
{                           4) UtilProgress while writing file
{                           5) EditorT/W: don't delete current file
{                           6) EditorI use FSExtSearch
{                           7) reset default file
{                           8) use var EdVersion 
{                           9) fix bug when replace end of file with nothing
{                          10) set screen to start of line after replace
{                          11) terminate Find and Replace if ^c typed
{                          12) point at the found text in Verify/Replace
{                          13) permit quoting of ctl-shift-c
{                          14) fix unassigned ptr bug in EditorI
{                          15) if user types "ed/help" just call Helper
{                          16) merge EditorFile into EditorT
{                          17) test PointAllowed[System] before use tablet
{                          18) allow HELP during replay
{                          19) set flag FileChanged & test in EditorT
{                          20) make BackSpace to ^J work
{                          21) ctl-BackSpace fixed to not go to initial ^J
{                          22) remove special treatment of FF in keyselection
{                          23) use CmdChar in prompts
{                          24) set pointer after Replace and Extend
{
       6-Jan-82  WJH  V1.15 1) Handle HELPkey.
      
      23-Oct-81  JPS  V1.14 1) Fix bug in translating virtual addresses 
                               for DrawByte (special Raster-Op).
                 WJH        2) Fix Help so it doesn't reopen file in Print
      
      28 Sep 81  WJH  V1.13 1) Use EditorK.Help for KeySelection editor.
                            2) Remove 'R' from version.
                            3) Import IO_Others instead of IO.
      
      17 Aug 81  WJH  V1.12 1) Fix erase BOT in Scrollup.  (Also fixes
                               an uncaught exception in SSetCursor.)

      24 Jul 81  WJH  V1.11 1) Add DEMO switch.  Causes no stop in replay.
      
      20 Jul 81  WJH  v1.10 1) Fix more mode so 23 char line doesn't
                               blank following lines.
                            2) Fix some decls so they are outside KeySelect.
                            
      26 Jun 81  WJH  V1.9  1) Turn tablet on at start and off during commands.
                            2) Correct N, O if base text deleted.
                            3) Change L to integer in Show.
                            4) Make selection not display ^C at end of file.
                            5) Fix ^W in Insert when word is across end of line
                            6) Fix More mode after X or LF.
                            7) Make thumb bar two lines high.
                            8) Fix scroll down of 79 char line.
                            9) Make caseless find the standard option.
                           10) Make file name in prompt standard.
                           11) Fix More mode so underline is fast.
      
      18 Jun 81  JPS  V1.8  1) Fix bug in transcript/replay.
                 WJH        2) Make small change in frequency of transcript
                               flushing.  Flush only after commands that
                               change the text and after carriage return
                               in Insert command.
      
      12 Jun 81  JPS  V1.7  1) Add 4-button mouse support (adapted from change
                               made by George Robertson).
                            2) Fix bug in transcript/replay.
      
       3 Jun 81  WJH  V1.6  1) Adapt for system version D43:
                               a) Change call on FSRename in EditorT.
                               b) Use exception in NewChunk in EditorU.
                            2) EditorT: Use FSAddToTitle.
      
      15 May 81  WJH  V1.5K 1) Add key driven selection commands:
                               L, W, space (and friends), G, M, X, N, T, B, LF.
                            2) Fix bug in AddC: Remove range error.
                            3) Change ' ' for no command to 'NoCommand'.
                            4) Change direction so it usually reverts.
                            5) Remove old meaning of X.
                            6) Modify init of SourceFirst (irrelevant).
                            7) Make Find command ignore case unless search
                               string is upper case.
                            8) Verify reverts to NO after most commands
                               (and especially after Replace).
                            9) Add ThumbBar location for old screen.
                           10) Add Note command and thumbbar location for it.
                           11) Fix bug so DrawUnderLine stops at right edge.
                           12) Fix bug in V R '...''.
                           13) Display file name in prompt line.
                           14) Change Help processor for multiple columns.
                               
       8 May 81  JPS  V1.5  1) Fix bugs in hysteresis code for pen presses.
                            2) Add transcript/replay controlled by the
                               ReplayVersion constant.
                            3) Fix small bugs in treatment of Screen cursor
                               during Prompt, Warn, Error, and Status.
       
      29 Apr 81  JPS  V1.4  1) Speed up the Editor by using the new Raster-Op
                               Entry point that draws from a packed character
                               buffer.
                            2) Change the QuoteCur character.
                            3) Add '^' processing to the Help routine.

      13 Apr 81  JPS  V1.3  1) Take advantage of virtual memory.
                            2) Use new special characters.

      31 Mar 81  JPS  V1.2  1) Change name of Editor.Swap to Editor.Swap$.
                            2) Use a breadth-first search on edit-file name by
                               extension and directory.
                            3) Add Cmd to list of extensions.
                            4) Remember full edit-file name.
                            5) Allow ":" in file names at termination time.
      
      25 Mar 81  BAM  V1.1  1) PERQ_String; import FileUtils

      23 Mar 81  JPS  V1.0  1) Fix "expression out of range" problems in
                               a) Selecting last line of the screen.
                               b) BackWord when word ends at right edge of
                                  screen.
                            2) Fix bug in inserting raw ^J (LF).
      
      24 Feb 81  DAS  V0.8  Changed FullDisk to always return false.
                 JPS        Fooled around with backup files and renaming in
                            order to work with the CMU file system.

      23 Feb 81  DAS  V0.7  Changed call to Initialize to EditInit.
      
      19 Feb 81  JPS  V0.6  Make the editor compatible with the new POS VC.3.

      06 Feb 81  JPS  V0.5  1. Fix bug that hangs the Editor when thumbing
                               to the beginning of the file.
                            2. Don't require confirmation on Quit Update and
                               Quit Write (undo V0.4 addition).
                            3. Turn cursor off during commands (undo V0.4
                               addition).
                            4. Change ^Y and ^N to INS and DEL in prompt line.
                            5. Use ^OOPS for delete line and ^BS for delete
                               word.
                            6. Fix bug in initial selection when the file
                               begins with an end-of-line.
                            7. Display "V" and "@" in prompt line in distinct
                               columns.
                            8. Minor adjustments to computation of "@" column.
                            9. Fix hang in Replace command when only the
                               end-of-text marker is selected.
                           10. Never require confirmation for Delete.
                           11. Add Help command.
                           12. Implement control-" as a quote character.
      
      20 Jan 81  JPS  V0.4  1. Represent ends-of-lines by CR LF pairs.  This
                               speeds up reading and writing files because no
                               data conversion is necessary.
                            2. Make piece table point initially to the
                               un-edited file, but use another swap file for
                               new text.
                            3. Don't re-use text after it is deleted, just
                               dispose the piece table entries.
                            4. When text is duplicated, don't copy it, just
                               copy the piece table entries.
                            5. Don't page blocks in before they are paged out.
                            6. Fix the bug in BSing or OOPSing over the ends
                               of long lines.
                            7. Change DEL in Replace to be the same as ^N
                               rather than the same as ' '.
                            8. Fix many minor bugs.
                            9. Redraw Selection (at top of screen) after
                               a Replace.
                           10. Because of change 3 above, don't forget the
                               old insertion after a Replace.
                           11. Make Replace with Verify point at the target.
                           12. Don't turn the pointer on and off all the time.
                           13. Only remember reasonable commands for the INS
                               key to re-execute.
                           14. Implement one level of backup file on the
                               file Editor.Backup.
                           15. Don't redraw the screen after trival Deletes
                               and Replaces.
                           16. Only redraw the R C and V during Find and
                               Replace.
                           17. Make warning and error messages display R C V.
                           18. Display control characters as reverse video
                               letters.
                           19. Attempt to display column number of the first
                               character of the selection (when it's on the
                               screen).
      
      24 Sep 80  JPS  V0.3  Fix bug where Find command doesn't show the select.
                            Reformat the screen a bit.
                            Add .Text and .Micro to list of default extensions.
                            Allow creation of a file by typing its name on
                               the command line.
                            Change most errors into warnings.
                            Make INS key repeat last command.
                            Avoid unnecessary re-drawing of the screen.
                            Fix bug in re-inserting last insertion.
                            Fix several bugs having to do with very long lines.
                            Fix the backwards find bug.
{---------------------------------------------------------------------}



exports


const

      { condition compilation: }
      
      ReplayVersion = True;
      KeySelection  = True;   {TRUE to compile key driven text selection mode}

      UseMenu       = False;  {TRUE to allow popup menu}
      SweepScope    = False;  {TRUE to hold left button & sweep for word/line}
      
          {Note that as of version 2.9 replay will fail if 
           SweepScope or UseMenu is enabled and the user
           uses the facility.}

{$ifc ReplayVersion then}
                   {$message Compiling transcript/replay version.}
{$endc}
{$IFC KeySelection THEN}
                   {$message Compiling key driven selection version.}
{$endc}
{$IFC UseMenu THEN}
                   {$message Will have popup menu.}
{$endc}
{$IFC SweepScope THEN}
                   {$message Will have SweepScope.}
{$endc}



  imports IO_Others  from IO_Others;
  imports Screen     from Screen;
  imports FileSystem from FileSystem;


const
      
      EditorVersion = '2.10';

var
      EdVersion: String[25];
      
const
      
      BS1      = Chr(#010);    { back space }
      BS2      = Chr(#350);    { back space: control-h }
      BS3      = Chr(#310);    { back space: control-H }

      BW1      = Chr(#210);    { back word: control-BS }
      BW2      = Chr(#367);    { back word: control-w }
      BW3      = Chr(#327);    { back word: control-W }

      BL1      = Chr(#225);    { back line: control-OOPS }
      BL2      = Chr(#365);    { back line: control-u }
      BL3      = Chr(#325);    { back line: control-U }

      Oops     = Chr(#025);    { OOPS }

      CR       = Chr(#015);    { carriage return }
      LF       = Chr(#012);    { line feed }
      FF       = Chr(#014);    { form feed }
{$IFC KeySelection THEN}
      TAB      = Chr(#011);           { tab }
      CtlTAB   = Chr(#200+Ord(TAB));  { control-tab }
      TABLength= 5;
      CtlCR    = Chr(#200+Ord(CR));   { control-return }
      CtlLF    = Chr(#200+Ord(LF));   { control-line feed }
{$ENDC}

      NoCommand= Chr(#000);    { initial value for LastCommand }
      Help     = Chr(#007);    { HELP }

      QuoteCh  = Chr(#242);    { ^" }

      NormalCur= '_';          { normal insert pencil (cursor character) }
      QuoteCur = Chr(#000);    { quoted insert pencil (cursor character) }

      EolMarker= Chr(#215);    { end of line marker }
      BotMarker= Chr(#212);    { beginning of text marker }
      EotMarker= Chr(#213);    { end of text marker }
      DotDotDot= Chr(#214);    { ... }
      BlankMark= Chr(#211);    { blank marker }

      Ins      = Chr(#033);    { INS }
      Del      = Chr(#177);    { delete }


      Stx      = Chr(#002);    { start of text }
      Etx      = Chr(#003);    { end of text }
   
      Bel      = Chr(#007);    { bell }

      Accept1  = Chr(#033);    { INS }
      Accept2  = Chr(#371);    { ctrl-y }
      Accept3  = Chr(#331);    { ctrl-y }

      Reject1  = Chr(#177);    { DEL }
      Reject2  = Chr(#356);    { ctrl-n }
      Reject3  = Chr(#316);    { ctrl-n }
      UClesslc = Ord('A') - Ord('a');
      
      { limits: }                                                      
      
      MaxDiskBlock = 4095;
      MaxFileIndex = 7;
      MaxDiskPage = (MaxDiskBlock+1) * (MaxFileIndex+1) - 1;
      MaxMemPage = 15;
      MaxOffset = 511;          { this **MUST** be 511, i.e. one disk block }
      MaxLength = 512;          { MaxOffset + 1 }

      EditFile = 0;             { index into IdFile for the original file }
      SwapFile = 1;             { index into IdFile for the swap file }
      
      { screen characteristics }
      
      CharHeight = 13;
      CharWidth = 9;
      LineHeight = 16;
      
      XMargin = 5;
      YMargin = 5;
      
      MaxNLine = 100;
      MaxNColumn = 150;
      
      FirstLine = -7;
      PromptL = -7;
      InsertL = -6;
      DeleteL = -5;
      FindL   = -4;
      ReplacL = -3;
      SelectL = -2;
      ThumbL  = -1;
      FirstColumn = -4;
      UpC = -3;
      DownC = -2;
      GatherC = -2;
      LSelectC = -1;
      TimeC = 79 - 18;
{$IFC KeySelection THEN}
      AtC = TimeC - 4;
      MoreingC = AtC-2;
      VerifyC = MoreingC -1;
{$ELSEC}
      AtC = TimeC - 5;
      VerifyC = AtC - 2;
{$ENDC}
      CountC = VerifyC - 6;
      RepeatC = CountC - 6;
      
{$ifc ReplayVersion then}
      TabPos = -1;
      TabHit = -2;
      SearchFail = -3;
      SearchOK = -4;
{$endc}
      
      
type FileIndex = 0..MaxFileIndex;
     DiskPage = 0..MaxDiskPage;
     MemPage = 0..MaxMemPage;
     OffsetRange = 0..MaxOffset;
     LengthRange = 0..MaxLength;
     
     pChunk = ^ChunkRec;
     ChunkRec = record
                 CPage:  DiskPage;
                 First:  OffsetRange;
                 Length: LengthRange;
                 OrderP: integer;
                 OrderC: integer;
                 Next:   pChunk;
                 Prev:   pChunk
                 end;
                
     Position = record
                 Chunk:  pChunk;
                 Offset: OffsetRange
                 end;
                 
     ReadWrite = (ReadCursor, WriteCursor);
                
     Cursor = record
               Pos: Position;
               Attached: boolean;
               Writing:  boolean;
               ChPage:   MemPage;
               ChOffset: OffsetRange;
               Ch:       Char
               end;
     
     ButtonKind = (NoneB, LeftB, MiddleB, RightB, BottomB);

     LineIndex = FirstLine..MaxNLine;
     ColumnIndex = FirstColumn..MaxNColumn;
     
     Selection = (SelectNone, SelectChar, SelectWord, SelectLine);
     CharTable = array [Chr(#000)..Chr(#377)] of boolean;
     
     UnderKind = (White, Black, ExtraBlack);
     DisplayLine = record case integer of
                    1: (Dot: array[0..47] of packed array[0..15] of 0..1);
                    2: (Word: array[0..47] of integer)
                    end;
     DisplayScreen = array[0..0] of DisplayLine;
     pScreen = ^DisplayScreen;
     
     pSwapBuffer = ^SwapBuffer;
     SwapBuffer = packed array[OffsetRange] of char;
     
     
{$ifc ReplayVersion then}
     ReplayMode = (ReplayCharOrHit,
                   ReplayCR,
                   ReplayCommand,
                   ReplayFree,
                   NotReplaying);
{$endc}
     
     
     
var DEBUG1, DEBUG2, DEBUG3: boolean;
    FileName: String;
    CreatingFile: boolean;
    FileChanged: Boolean;
    Files: FileIndex; { number of open files }
    IdFile: array[FileIndex] of FileId;
    Txt: array[MemPage] of record
          Buffer: pSwapBuffer;
          TPage: DiskPage;
          Dirty: boolean;
          Age: integer
          end;
    Time: integer;
    LastMemPage: MemPage;
    Pages: DiskPage;
    
    Ln: array[-1..MaxNLine] of record
         Col: ColumnIndex;
         Start, Finish: Position;
         Length: integer
         end;
    
    { Positions which are automatically updated during Join and Split.
      if you add a Position variable, you must add it to FixUp. }
    
    EmptyFirst,   EmptyLast:    Position;
    FilledFirst,  FilledLast:   Position;
    ScreenFirst,  ScreenLast:   Position;
    SelectFirst,  SelectLast:   Position;
    InsertFirst,  InsertLast:   Position;
    DeleteFirst,  DeleteLast:   Position;
    FindFirst,    FindLast:     Position;
    ReplaceFirst, ReplaceLast:  Position;
    SourceFirst,  SourceLast:   Position;
{$IFC  KeySelection THEN}
    OldFirst,     NoteFirst:    Position;
{$ENDC}
    Display:                    Position;
    LeftPart,     RightPart:    Position;
    PFirst,       PLast:        Position;
    Tmp:                        Position;
    
    { Cursors which are automatically updated during Join and Split,
      and which reserve pages in Mem.
      if you add a Cursor variable, you must add it to Fixup and Mem. }
    
    DrawCursor, Cursor1, Cursor2: Cursor;
    
    ThisLine, OldLine: LineIndex;
    ThisColumn, OldColumn, AtColumn, OldAtColumn: ColumnIndex;
    
{$ifc ReplayVersion then}
    TabX, TabY, OldTabX, OldTabY: Integer;
{$endc}
    
    Switch, OldSwitch, Press: boolean;
    MiddleSwitch,   {Yellow}
    LeftSwitch,     {White}
    RightSwitch,    {Green}
    BottomSwitch:   {Blue}
           boolean;
    LastButton: ButtonKind;
    RawCh, Ch: char;
    Quoted: Boolean;
    NeedPrompt, Editing: boolean;
    ThisSelect: Selection;
    WordChars: set of '0'..'z';
    UCletters, lcletters: CharTable;
{$IFC  KeySelection THEN}
    StmtChars, LineChars, PageChars, AllChars, 
      PrintChars, LettersAndDigits: CharTable;

    OldDot,     {Position prior to last ThumbBar operation.}
    NoteDot,    {Position saved by Note Command.}
{$ENDC}    
    EofDot, ScreenFDot, ScreenLDot, SelectDot, PagesPerDot: integer;
    
    Jump: array[char] of integer;
    PatLength: integer;
    
    Direction: char;
    RepeatCount, OldRepeatCount, Count, OldCount: integer;
    Verify, OldVerify: boolean;
{$IFC  KeySelection THEN}
    Moreing, OldMoreing: Boolean;
{$ENDC}    

    
    Scrn: pScreen;
    TextPointer, UpPointer, DownPointer, ThumbPointer, LinePointer: CurPatPtr;
    xTextPointer, yTextPointer,
    xUpPointer, yUpPointer,
    xDownPointer, yDownPointer,
    xThumbPointer, yThumbPointer,
    xLinePointer, yLinePointer: integer;
    CurrentPointer, NewPointer: (NoP, TextP, LineP, UpP, DownP, ThumbP);
    
    ChunkSeg: Integer;
    
  (* The next group of variables used to be constants with these def'ns:
      ScreenChars = (768 - XMargin - XMargin) div CharWidth;
      ScreenLines = (1024 - YMargin - YMargin) div LineHeight;
      NLine = ScreenLines + FirstLine;
      LastLine = NLine - 1;
      NColumn = ScreenChars + FirstColumn;
      LastColumn = NColumn - 1;
      HomeX = XMargin - FirstColumn * CharWidth;
      HomeY = YMargin - FirstLine * LineHeight;
      RightLimit = 768;
      BottomLimit=1024;
      DotsAcross = NColumn - 1;
  *)
      ScreenChars: integer;   {max number of chars across line}
      ScreenLines: integer;   {max number of lines}
      NLine: integer;         {number of lines in text display area}
      LastLine: integer;      {index of the last line in text display area}
      NColumn: integer;       {number of columns in text display area}
      LastColumn: integer;    {index of the last column in text area}
      HomeX, HomeY: integer;  {screen coords of upper left char in text area}
      RightLimit: integer;    {distance from screen left to window right}
      BottomLimit: integer;   {distance from screen top to window bottom}
      DotsAcross: integer;    {number of positions in ThumbLine}
      
      CurWIndX: WinRange;     {current window (differs from base for replay)}
      WinOrgX, WinOrgY,       {screen coords of upper left corner of usable}
      WinHeight, WinWidth:    {size of usable area of window}
            integer;
   
{$ifc ReplayVersion then}
    Replay: ReplayMode;
    TranscriptWord: Integer;
    TId: FileId;
    TBlock, TWord: Integer;
    TBlocks, TWords: Integer;
    TBuffer: pDirBlk;
    DemoSwitch : Boolean;
{$endc}
    

{$ifc UseMenu then}
    HaveKey: Boolean;     {Used to tell KeyStruck to use value of RawCh}
{$endc}
      
      
 private
 
 
 
  
  imports EditorInitialize   from EditorI;
  imports EditorUtilities    from EditorU;
  imports EditorTerminate    from EditorT;
{$IFC KeySelection THEN}
  imports EditorKeySelection from EditorK;
{$ENDC}
  imports Perq_String  from Perq_String;
  imports UtilProgress from UtilProgress;
  imports Raster       from Raster; 
  imports Helper       from Helper;
  
    
{$ifc UseMenu then}
  imports PopUp        from PopUp;
    
  var   MenuNames, OtherNames: pNameDesc;   
  const NumNames = 6; 
        NumOther = 10;     
{$endc}

    
 {-- the editor --}
    
 procedure Edit;
 var CmdPrompt, NewFile: String;
{$IFC KeySelection THEN}
     ThisCmd, LastCmd, CmdCh: char;
     LastDirection: Char;
     SaveSDot: integer;
     SaveSFirst: Position;
{$ELSEC}
     ThisCmd, LastCmd: Char;
{$ENDC}
     Automatic, Success: boolean;
     SL   : LineIndex;
     SC   : ColumnIndex;
     
     BaseFirst: Position;   {used for rubber band extend}
     PuckMoved: Boolean;    {for click select}
  
  
  procedure HelpTheUser;
  begin
     RasterOp(RXor,WinWidth,WinHeight,  
                   WinOrgX,WinOrgY,SScreenW,Scrn,  
                   WinOrgX,WinOrgY,SScreenW,Scrn);
     SSetCursor(WinOrgX+10,WinOrgY+10);
{$IFC KeySelection THEN}
     writeln('Reading Help File: EditorK.Index');
     GiveHelp('>HelpDir>EditorHelp>EditorK.Index');
{$ELSEC}
     writeln('Reading Help File: Editor.Index');
     GiveHelp('>HelpDir>EditorHelp>Editor.Index');
{$ENDC}
     RefreshScreen
  end { HelpTheUser };
  
  procedure SaveSpot;
    {Save the status of ScreenFDot for tests in SetSpot.
    { Note: use SaveSpot/SetSpot only across calls that will
    {       not modify the text.  The SaveSFirst pointer is
    {       not known to FixUp.          }
  begin
     SaveSDot := ScreenFDot;
     SaveSFirst := ScreenFirst;
  end;
  function SetSpot: Boolean;
  begin
     SetSpot := False;
     if abs(SaveSDot-ScreenFDot) > 2 then begin
{$ifc KeySelection then}
         MovePencil(ThumbL, OldDot);
         Write(' ');
         OldDot := SaveSDot;
         OldFirst := SaveSFirst;
         DrawThumbBar;
         SetSpot := True;
{$endc}
     end;
  end;
  
  
  function Accept: Boolean;
  begin { Accept }
   if Quoted then Accept := False
   else
    Accept := (RawCh = Accept1) or (RawCh = Accept2) or (RawCh = Accept3)
  end { Accept };
  
  
  function Reject: Boolean;
  begin { Reject }
   if Quoted then Reject := False
   else
    Reject := (RawCh = Reject1) or (RawCh = Reject2) or (RawCh = Reject3)
  end { Reject };
  
  
  procedure CheckCRLF( var P, Q: Position );
  begin { CheckCRLF }
   Attach(Cursor1,P,ReadCursor);
   if Cursor1.Ch = LF then
    begin Sub1C(Cursor1);
     if Cursor1.Ch = CR then P := Cursor1.Pos
    end;
   ReAttach(Cursor1,Q);
   if Cursor1.Ch = CR then
    begin Add1C(Cursor1);
     if Cursor1.Ch = LF then Q := Cursor1.Pos
    end;
   Detach(Cursor1)
  end { CheckCRLF };
 
 
  procedure GatherLine( L: LineIndex );
  
  
   procedure G( var P, Q: Position );
   var C: integer;
       EolCount: integer;
   
   
    procedure EchoChar;
    var Done: Boolean;
    begin { EchoChar }
     if (not Quoted and (RawCh = CR)) or (Ch = LF) then
{CR or LF}
      begin
       if Ch = LF then
        if NE(PFirst,Cursor2.Pos) then
         begin Sub1C(Cursor2);
          if Cursor2.Ch = CR then
           begin C := C - 1;
            MovePencil(L,C);
            EraseChar;
            MovePencil(L,C);
            Ch := CR
           end
          else Add1C(Cursor2)
         end;
       Add1C(Cursor2);
       if Ch = CR then
        begin Cursor2.Ch := LF;
         if EQ(Cursor2.Pos,EmptyLast) then CreateEmptyPage;
         Add1C(Cursor2);
         Write(EolMarker);
         C := C + 1;
         EolCount := EolCount + 1
        end
       else
        begin WriteChar(LF); C := C + 1 end
      end
     else
      if not Quoted and
         ((RawCh = BS1) or (RawCh = BS2) or (RawCh = BS3)) then
{BS}
       begin
        if NE(PFirst,Cursor2.Pos) then
         begin Sub1C(Cursor2);
          C := C - 1;
          if Cursor2.Ch = LF then
           begin Sub1C(Cursor2);
            if Cursor2.Ch = CR then EolCount := EolCount - 1
            else Add1C(Cursor2)
           end;
          if C >= GatherC then
           begin MovePencil(L,C);
            EraseChar;
            MovePencil(L,C)
           end
         end
       end
      else
       if not Quoted and
          ((RawCh = BW1) or (RawCh = BW2) or (RawCh = BW3)) then
{BW}
        begin Done := False;
         repeat
          if EQ(PFirst,Cursor2.Pos) then Done := True
          else
           begin Sub1C(Cursor2);
            if Cursor2.Ch <> ' ' then 
             if (Cursor2.Ch = LF) and (EolCount > 0) then
              begin Sub1C(Cursor2);
               if Cursor2.Ch = CR then EolCount := EolCount - 1
               else
                begin Add1C(Cursor2); Done := True end
              end
             else
              begin Add1C(Cursor2); Done := True end
            end;
          if not Done then C := C - 1
         until Done;
         if NE(PFirst,Cursor2.Pos) then
          begin Sub1C(Cursor2);
           C := C - 1;
           if NE(PFirst,Cursor2.Pos) and (Cursor2.Ch in WordChars) then
            begin
             repeat Sub1C(Cursor2);
              C := C - 1
             until EQ(PFirst,Cursor2.Pos) or not (Cursor2.Ch in WordChars);
             if NE(PFirst,Cursor2.Pos) then
              begin Add1C(Cursor2); C := C + 1 end
            end
          end;
         if C < GatherC then ClearLine(L,GatherC) else ClearLine(L,C)
        end
       else
        if not Quoted and
           ((RawCh = BL1) or (RawCh = BL2) or (RawCh = BL3)) then
{BL}
         begin
          if EolCount = 0 then
           begin ReAttach(Cursor2,PFirst);
            C := GatherC
           end
          else
           begin EolCount := EolCount - 1;
            repeat
             repeat Sub1C(Cursor2);
              C := C - 1
             until Cursor2.Ch = LF;
             Sub1C(Cursor2);
             if Cursor2.Ch <> CR then Add1C(Cursor2)
            until Cursor2.Ch = CR
           end;
          if C < GatherC then ClearLine(L,GatherC) else ClearLine(L,C)
         end
        else
         if not Quoted and (RawCh = Oops) then
{OOPS}
          Write(Bel)
         else
{text}
          begin Add1C(Cursor2);
           if Ch = ' ' then WriteChar(BlankMark) else WriteChar(Ch);
           C := C + 1
          end;
     if C > LastColumn then
      begin DeleteChar(L,GatherC);
       Add1C(Cursor1);
       C := C - 1;
       MovePencil(L,C)
      end
     else
      if NE(PFirst,Cursor1.Pos) and (C < LastColumn) then
       begin
        repeat Sub1C(Cursor1);
         Ch := Cursor1.Ch;
         InsertChar(L,GatherC);
         WriteChar(Ch);
         C := C + 1
        until EQ(PFirst,Cursor1.Pos) or (C = LastColumn);
        MovePencil(L,C)
       end
    end { EchoChar };
    
  
   begin { G }
    MovePencil(L,GatherC);
    if Automatic then
     begin RawCh := Accept1; Quoted := False end
    else NextChar;
    if not (Accept or Reject) then
     begin Attach(Cursor1,EmptyFirst,ReadCursor);
      Attach(Cursor2,EmptyFirst,WriteCursor);
      PFirst := Cursor1.Pos;
      EolCount := 0;
      C := GatherC;
      ClearLine(L,GatherC);
      repeat Cursor2.Ch := Ch;
       if EQ(Cursor2.Pos,EmptyLast) then CreateEmptyPage;
       EchoChar;
       NextChar
      until Accept or Reject;
      if Accept then
       begin Collect(P,Q);
        if EQ(PFirst,Cursor2.Pos) then
         begin P := FilledLast; Q := FilledLast end
        else
         begin P := PFirst;
          Add(Cursor2.Pos,-1,Q);
          EmptyFirst := Cursor2.Pos;
          Split(EmptyFirst)
         end
       end;
      Detach(Cursor1);
      Detach(Cursor2);
      DrawLn(L)
     end
   end { G };
   
   
  begin { GatherLine }
   case L of
    FindL:   G(FindFirst,    FindLast);
    ReplacL: G(ReplaceFirst, ReplaceLast)
    end
  end { GatherLine };
 
  
  function GoodPattern: boolean;
  begin { GoodPattern }
   GoodPattern := false;
   if EQ(FindFirst,FilledLast) then Warn('no pattern in F buffer')
   else
    if Subtract(FindLast,FindFirst) < 0 then Warn('pattern too long')
    else GoodPattern := true
  end { GoodPattern };
  
  procedure FindSetUp;
  
  
   procedure FindFSetUp;
   const FirstChar = 0;
         LastChar = #377;
   var I: integer;
       C: char;
       Done: boolean;
   begin { FindFSetUp }
    PatLength := Subtract(FindLast,FindFirst) + 1;
    for C := Chr(FirstChar) to Chr(LastChar) do Jump[C] := PatLength;
    Attach(Cursor1,FindFirst,ReadCursor);
    I := PatLength;
    repeat I := I - 1;
     Jump[Cursor1.Ch] := I;
     if lcletters[Cursor1.Ch] then                 {Caseless}
        Jump[Chr(Ord(Cursor1.Ch)+UClesslc)] := I; {Caseless}
     Done := EQ(Cursor1.Pos,FindLast);
     Add1C(Cursor1)
    until Done;
    Detach(Cursor1)
   end { FindFSetUp };
  
  
   procedure FindRSetUp;
   const FirstChar = 0;
         LastChar = #377;
   var I: integer;
       C: char;
       Done: boolean;
   begin { FindRSetUp }
    PatLength := Subtract(FindLast,FindFirst) + 1;
    for C := Chr(FirstChar) to Chr(LastChar) do Jump[C] := -PatLength;
    Attach(Cursor1,FindLast,ReadCursor);
    I := -PatLength;
    repeat I := I + 1;
     Jump[Cursor1.Ch] := I;
     if lcletters[Cursor1.Ch] then                 {Caseless}
        Jump[Chr(Ord(Cursor1.Ch)+UClesslc)] := I; {Caseless}
     Done := EQ(Cursor1.Pos,FindFirst);
     Sub1C(Cursor1)
    until Done;
    Detach(Cursor1)
   end { FindRSetUp };
   
   
  begin { FindSetUp }
   if Direction = '>' then FindFSetUp else FindRSetUp
  end { FindSetUp };
  
  
  procedure FindText;
   var
     MaxPage, CurrPage: integer;
   
   procedure FindForward;
   var Distance: integer;
       SourcePos: Position;
       State: (Searching, Matching, Found, NotFound);
   begin { FindForward }
    CurrPage := -1;
    Add(SourceFirst,PatLength - 1,SourcePos);
    State := Searching;
    Attach(Cursor1,FindLast,ReadCursor);
    Attach(Cursor2,SourcePos,ReadCursor);
    repeat ReAttach(Cursor2,SourcePos);
     if GT(SourcePos,SourceLast) then State := NotFound
     else
      begin 
       if CurrPage <> Cursor2.Pos.Chunk^.OrderP then begin
           CurrPage := Cursor2.Pos.Chunk^.OrderP;
           ComputeProgress(CurrPage, MaxPage);
       end;
       Distance := Jump[Cursor2.Ch];
       if Distance > 0 then Add(SourcePos,Distance,SourcePos)
       else
        begin ReAttach(Cursor1,FindLast);
         State := Matching;
         repeat
          if EQ(Cursor1.Pos,FindFirst) then State := Found
          else
           begin Sub1C(Cursor1);
            Sub1C(Cursor2);
{Caseless}
            if Cursor1.Ch = Cursor2.Ch then {State stays Matching}
            else if not lcletters[Cursor1.CH] then 
               State := Searching
            else if Chr(Ord(Cursor1.Ch)+UClesslc) <> Cursor2.Ch then
               State := Searching
{Exact case
            if Cursor1.Ch <> Cursor2.Ch then State := Searching
}
           end
         until State <> Matching;
         if State = Searching then
          begin
           Distance := Jump[Cursor2.Ch] - Subtract(FindLast,Cursor1.Pos);
           if Distance > 1 then Add(SourcePos,Distance,SourcePos)
           else Add1(SourcePos)
          end
        end
      end
    until State <> Searching;
    if State = Found then
     begin PFirst := Cursor2.Pos;
      PLast := SourcePos
     end
    else
     begin PFirst := FilledLast;
      PLast := FilledLast
     end;
    Detach(Cursor1);
    Detach(Cursor2)
   end { FindForward };
  
  
   procedure FindReverse;
   var Distance: integer;
       SourcePos: Position;
       State: (Searching, Matching, Found, NotFound);
   begin { FindReverse }
    CurrPage := -1;
    Add(SourceLast,1 - PatLength,SourcePos);
    State := Searching;
    Attach(Cursor1,FindFirst,ReadCursor);
    Attach(Cursor2,SourcePos,ReadCursor);
    repeat ReAttach(Cursor2,SourcePos);
     if LT(SourcePos,SourceFirst) then State := NotFound
     else
      begin        
       if CurrPage <> Cursor2.Pos.Chunk^.OrderP then begin
           CurrPage := Cursor2.Pos.Chunk^.OrderP;
           ComputeProgress(CurrPage, MaxPage);
       end;
       Distance := Jump[Cursor2.Ch];
       if Distance < 0 then Add(SourcePos,Distance,SourcePos)
       else
        begin ReAttach(Cursor1,FindFirst);
         State := Matching;
         repeat
          if EQ(Cursor1.Pos,FindLast) then State := Found
          else
           begin Add1C(Cursor1);
            Add1C(Cursor2);
{Caseless}
            if Cursor1.Ch = Cursor2.Ch then {State stays Matching}
            else if not lcletters[Cursor1.CH] then 
               State := Searching
            else if Chr(Ord(Cursor1.Ch)+UClesslc) <> Cursor2.Ch then
               State := Searching
{Exact case
            if Cursor1.Ch <> Cursor2.Ch then State := Searching
}
           end
         until State <> Matching;
         if State = Searching then
          begin
           Distance := Jump[Cursor2.Ch] + Subtract(Cursor1.Pos,FindFirst);
           if Distance < -1 then Add(SourcePos,Distance,SourcePos)
           else Sub1(SourcePos)
          end
        end
      end
    until State <> Searching;
    if State = Found then
     begin PFirst := SourcePos;
      PLast := Cursor2.Pos
     end
    else
     begin PFirst := FilledLast;
      PLast := FilledLast
     end;
    Detach(Cursor1);
    Detach(Cursor2)
   end { FindReverse };
   
  label 6;
  Handler CtlC;  begin        {terminate search on CtlC}
     CtrlCPending := False;
     IOKeyClear;
  {$ifc ReplayVersion then}
     if Replay = NotReplaying then
  {$endc}
     begin
         if Cursor1.Attached then
             Detach(Cursor1);
         if Cursor2.Attached then
             Detach(Cursor2);
         PFirst := FilledLast;    {indicate string not found}
         PLast :=  FilledLast;
         goto 6;
     end;
  end;   


  begin { FindText }

{$ifc ReplayVersion then}
     if Replay <> NotReplaying then begin
         if TranscriptWord <> SearchOK then begin   
               {the search failed or was not made}
             PFirst := FilledLast;
             PLast := FilledLast;
             if TranscriptWord = SearchFail then
                 NextTranscript;
             exit (FindText);
         end;
     end;
{$endc}

    MaxPage := FilledLast.Chunk^.OrderP;
    if Direction = '>' then FindForward else FindReverse;

6:

{$ifc ReplayVersion then}
    if Replay <> NotReplaying then
        {TranscriptWord is still SearchOK; complete the search before going on}
        NextTranscript
    else  { Replay = NotReplaying }
        if EQ (PFirst, FilledLast) then 
            SendTranscript(SearchFail)
        else
            SendTranscript(SearchOK);
{$endc}

  end { FindText };



{$IFC KeySelection THEN}
  procedure AdvancePtrs(First, Last, New: Position);
  begin
     if LE(First, OldFirst) and LE(OldFirst, Last) then
        OldFirst := New;
     if LE(First, NoteFirst) and LE(NoteFirst, Last) then
        NoteFirst := New;
  end;
{$ENDC}

   
  
  procedure Insert( Command: char );
  var FirstL: LineIndex;
      FirstC: ColumnIndex;
      
   procedure InsertOld;
   var P, Q: Position;
   begin { InsertOld }
    if EQ(InsertFirst,FilledLast) then
     if EQ(DeleteFirst,FilledLast) then
      begin Warn('no text in I and D buffers');
       RawCh := Reject1;
       Quoted := False
      end
     else
      begin InsertFirst := DeleteFirst;
       InsertLast := DeleteLast;
       DeleteFirst := FilledLast;
       DeleteLast := FilledLast;
       DrawLn(InsertL);
       DrawLn(DeleteL);
       Tmp := InsertFirst
      end
    else
     begin Prompt('Insert:  copying last insertion');
      Join(LeftPart,RightPart);
      Copy(InsertFirst,InsertLast);
      Split(RightPart);
      InsertFirst := PFirst;
      InsertLast := PLast;
      Tmp := InsertFirst
     end
   end { InsertOld };
      
      
   procedure InsertNew;
   var L: LineIndex;
       C: ColumnIndex;
       EolCount: integer;
       Blanks: integer;
       CountingBlanks: boolean;
       
      
    procedure EchoChar;
    var LeadingBlanks: integer;
        Done: Boolean;
       
        
     procedure CountLeadingBlanks;
     begin { CountLeadingBlanks }
      Attach(Cursor2,Ln[L].Start,ReadCursor);
      LeadingBlanks := 0;
      while Cursor2.Ch = ' ' do
       begin Add1C(Cursor2); LeadingBlanks := LeadingBlanks + 1 end;
      Detach(Cursor2)
     end { CountLeadingBlanks };
        
        
     procedure NextLine;
     begin { NextLine }
      Add(Cursor1.Pos,-1,Ln[L].Finish);
      Ln[L].Length := C;
      L := L + 1;
      EolCount := EolCount + 1;
      if L + 3 > LastLine then
       begin L := L - 1; ScrollUp(0,L,1) end
      else ScrollDown(L,LastLine,1);
      Ln[L].Start := Cursor1.Pos;
      C := LeadingBlanks;
      while LeadingBlanks > 0 do
       begin Cursor1.Ch := ' ';
        if EQ(Cursor1.Pos,EmptyLast) then
         begin Split(RightPart);
          CreateEmptyPage;
          Join(EmptyLast,RightPart)
         end;
        Add1C(Cursor1);
        LeadingBlanks := LeadingBlanks - 1
       end;
      MovePencil(L,C)
     end { NextLine };
     
     
     procedure PrevLine;
     var i: integer;
         P, Q: Position;
     begin { PrevLine }
      if EolCount = 0 then
       begin C := FirstC;
        ReAttach(Cursor1,PFirst);
        ClearLine(L,C)
       end
      else
       begin ClearLine(L,0);
        ReAttach(Cursor1,Ln[L].Start);
        Sub1C(Cursor1);
        if Cursor1.Ch = LF then
         begin Sub1C(Cursor1);
          if Cursor1.Ch <> CR then Add1C(Cursor1)
         end;
        EolCount := EolCount - 1;
        if (L < LastLine div 2) and not Bot(ScreenFirst) then
         ScrollDown(0,L,1)
        else
         begin L := L - 1;
          ScrollUp(L+1,LastLine,1)
         end;
        C := Ln[L].Length - 1;
        ClearLine(L,C);
        MovePencil(L,C)
       end
     end { PrevLine };
     
       
    begin { EchoChar }
     if (not Quoted and (RawCh = CR)) or (Ch = LF) then
{CR or LF }
      begin
       if Ch = LF then
        if NE(PFirst,Cursor1.Pos) then
         begin Sub1C(Cursor1);
          if Cursor1.Ch = CR then
           begin
            if C = 0 then PrevLine
            else
             begin C := C - 1;
              MovePencil(L,C);
              EraseChar;
              MovePencil(L,C)
             end;
            Ch := CR
           end
          else Add1C(Cursor1)
         end;
       Add1C(Cursor1);
       if Ch = CR then
        begin
         FlushTranscript;
         Cursor1.Ch := LF;
         if EQ(Cursor1.Pos,EmptyLast) then
          begin Split(RightPart);
           CreateEmptyPage;
           Join(EmptyLast,RightPart)
          end;
         Add1C(Cursor1);
         CountLeadingBlanks;
         EraseChar;
         C := C + 1;
         NextLine;
{$ifc ReplayVersion then}
         if Replay <> NotReplaying then CheckReplay(ReplayCR)
{$endc}
        end
       else
        begin WriteChar(LF); C := C + 1 end
      end
     else
      if not Quoted and
         ((RawCh = BS1) or (RawCh = BS2) or (RawCh = BS3)) then
{BS}
       begin
        if NE(PFirst,Cursor1.Pos) then
         begin Sub1C(Cursor1);
          if C = 0 then
           begin
            if Cursor1.Ch = LF then
             begin Sub1C(Cursor1);
              if Cursor1.Ch <> CR then Add1C(Cursor1)
             end;
            PrevLine
           end
          else
           begin C := C - 1;
            MovePencil(L,C);
            EraseChar;
            MovePencil(L,C)
           end;
         end
       end
      else
       if not Quoted and
          ((RawCh = BW1) or (RawCh = BW2) or (RawCh = BW3)) then
{BW}
        begin Done := False;
         repeat
          if EQ(PFirst,Cursor1.Pos) then Done := True
          else
           begin Sub1C(Cursor1);
            if Cursor1.Ch <> ' ' then 
             if (Cursor1.Ch = LF) and (EolCount > 0) then
              begin Sub1C(Cursor1);
               if Cursor1.Ch <> CR then Done := True 
              end
             else Done := True; 
            if C = 0 then PrevLine
            else C := C - 1
           end;
         until Done;
         if (Cursor1.Ch in WordChars) and NE(PFirst,Cursor1.Pos) then begin
            while NE(PFirst,Cursor1.Pos) and (Cursor1.Ch in WordChars) do begin
               Sub1C(Cursor1);
               if (Cursor1.Ch in WordChars) then 
                  if C=0 then PrevLine
                  else C := C-1;
            end;
            if not (Cursor1.Ch in WordChars) then 
               Add1C(Cursor1);
         end;
         ClearLine(L,C)
        end
       else
        if not Quoted and
           ((RawCh = BL1) or (RawCh = BL2) or (RawCh = BL3)) then
{BL}
         PrevLine
        else
         if not Quoted and (RawCh = Oops) then 
{OOPS}
(*  old code
          Write(Bel);
*)
           {move to beginning of line}
           repeat  {do BS operations to start of line}
             if NE(PFirst,Cursor1.Pos) then begin 
               Sub1C(Cursor1);
               if C = 0 then begin
                 if Cursor1.Ch = LF then begin
                   Sub1C(Cursor1);
                   if Cursor1.Ch <> CR then Add1C(Cursor1);
                 end;
                 PrevLine;
               end
               else begin 
                 C := C - 1;
                 MovePencil(L,C);
                 EraseChar;
                 MovePencil(L,C)
               end;
             end;
           until (C=0) or EQ(PFirst, Cursor1.Pos)
         else
{text}
          begin Add1C(Cursor1);
           WriteChar(Ch);
           C := C + 1;
           if C = NColumn then
            begin LeadingBlanks := 0; NextLine end
          end;
     AtColumn := C;
     Prompt('');
     MovePencil(L,C)
    end { EchoChar };
    
    
   begin { InsertNew }
    L := FirstL;
    C := FirstC;
    AtColumn := C;
    Prompt('');
    MovePencil(L,C);
    PFirst := EmptyFirst;
    Attach(Cursor1,EmptyFirst,WriteCursor);
    Join(LeftPart,EmptyFirst);
    Join(EmptyLast,RightPart);
    if FirstC = 0 then
     begin Ln[FirstL].Start := PFirst;
      if FirstL = 0 then ScreenFirst := PFirst
     end;
    EolCount := 0;
    repeat Cursor1.Ch := Ch;
     if EQ(Cursor1.Pos,EmptyLast) then
      begin Split(RightPart);
       CreateEmptyPage;
       Join(EmptyLast,RightPart)
      end;
     EchoChar;
     NextChar
    until Accept or Reject;
    if EQ(Cursor1.Pos,EmptyFirst) then { empty insertion }
     begin RawCh := Reject1; Quoted := False end
    else
     if Accept then
      begin FirstL := L;
       FirstC := C;
       EmptyFirst := Cursor1.Pos;
       InsertFirst := PFirst;
       Add(EmptyFirst,-1,InsertLast);
       Split(InsertFirst);
       DrawLn(InsertL)
      end;
    Detach(Cursor1);
    Split(EmptyFirst);
    Split(RightPart)
   end { InsertNew };
     
  
  begin { Insert }
   Display := ScreenFirst;
   ClearLine(PromptL,FirstColumn);
   if Command = 'A' then
    if Eot(SelectLast) then
     if Eot(SelectFirst) then
      RightPart := SelectFirst
     else
      begin Sub1(SelectLast);
       Add(SelectLast,1,RightPart)
      end
    else Add(SelectLast,1,RightPart)
   else RightPart := SelectFirst;
   Add(RightPart,-1,LeftPart);
   { if NE(LeftPart,FilledFirst) then } Show(RightPart,0,LastLine-3);
   ScreenPos(RightPart,FirstL,FirstC);
   ClearLine(FirstL,FirstC);
   ScrollDown(FirstL+1,LastLine,2);
   { ScrollDown(FirstL+2,LastLine); }
   Draw(RightPart,Ln[FirstL].Finish,FirstL+2,FirstC);
   Split(RightPart);
   if Command = 'A' then Prompt('Append:  INS accepts, DEL rejects')
   else
    if Command = 'I' then Prompt('Insert:  INS accepts, DEL rejects')
    else Prompt('Substitute:  INS accepts, DEL rejects');
   MovePencil(FirstL,FirstC);
   if Automatic then
    begin RawCh := Accept1; Quoted := False end
   else NextChar;
   Tmp := RightPart;
   if Reject then Join(LeftPart,RightPart)
   else
    begin
     if Accept then InsertOld
     else InsertNew;
     if Accept then
      begin
       if (Command = 'S') and not Eot(SelectFirst) then
        begin Collect(DeleteFirst,DeleteLast);
         if Eot(SelectLast) then Sub1(SelectLast);
         if EQ(Tmp,RightPart) then Add(SelectLast,1,Tmp);
         Add(SelectLast,1,RightPart);
         Split(RightPart);
{$IFC KeySelection THEN}
         AdvancePtrs(SelectFirst, SelectLast, RightPart);   {Fix Old and Note}
{$ENDC}
         DeleteFirst := SelectFirst;
         DeleteLast := SelectLast;
         SelectFirst := RightPart;
         SelectLast := RightPart;
         CheckCRLF(SelectFirst,SelectLast);
         DrawLn(DeleteL);
         DrawLn(SelectL)
        end;
       Join(LeftPart,InsertFirst);
       Join(InsertLast,RightPart);
       DrawThumbBar;
       Display := ScreenFirst
      end
     else
      begin Join(LeftPart,RightPart); Tmp := RightPart end
    end;
   Success := Accept;
   if EQ(Display,ScreenFirst) then JoinScreen(Tmp,FirstL,FirstC)
   else Draw(Display,FilledLast,0,-1);
   if Success then begin
      ThisSelect := SelectChar;
      if OnScreen(SelectFirst,0,LastLine) then
         ScreenPos(SelectFirst,ThisLine,ThisColumn);
   end;
  end { Insert };
  
  
  procedure Delete;
  var L: LineIndex;
      C: ColumnIndex;
      N: integer;
  begin { Delete }
   {===== Removed 7 Feb 81   -JPS
   Display := ScreenFirst;
   =====}
   Show(SelectFirst,0,LastLine);
   {===== Removed 7 Feb 81   -JPS
   if Verify and not Automatic then
    begin ClearLine(PromptL,FirstColumn);
     Prompt('Delete:  INS accepts, DEL rejects');
     repeat NextChar
     until Accept or Reject
    end
   else
    begin RawCh := Accept1; Quoted := False end;
   Success := Accept;
   if Success and not Eot(SelectFirst) then
   =====}
   Success := True;
   if not Eot(SelectFirst) then
    begin ScreenPos(SelectFirst,L,C);
     Add(SelectFirst,-1,LeftPart);
     if Eot(SelectLast) then Sub1(SelectLast);
     Add(SelectLast,1,RightPart);
     Split(SelectFirst);
     Split(RightPart);
     Join(LeftPart,RightPart);
     InsertFirst := FilledLast;
     InsertLast := FilledLast;
     Collect(DeleteFirst,DeleteLast);
{$IFC KeySelection THEN}
     AdvancePtrs(SelectFirst, SelectLast, RightPart);   {Fix Old and Note}
{$ENDC}
     DeleteFirst := SelectFirst;
     DeleteLast := SelectLast;
     SelectFirst := RightPart;
     SelectLast := RightPart;
     ThisSelect := SelectChar;
     CheckCRLF(SelectFirst,SelectLast);
     DrawThumbBar;
     DrawLn(InsertL);
     DrawLn(DeleteL);
     DrawLn(SelectL);
     JoinScreen(RightPart,L,C);
     if OnScreen(SelectFirst,0,LastLine) then
      ScreenPos(SelectFirst,ThisLine,ThisColumn);
     ThisCmd := 'I';      {set to insert the item deleted}
     CmdCh   := 'I';
    end
   {===== Removed 7 Feb 81   -JPS
   else
    if NE(Display,ScreenFirst) then Draw(Display,FilledLast,0,-1)
   =====}
  end { Delete };
  

  procedure CopyCommand;
       {copy the selection into DeleteFirst...DeleteLast.
        Don't copy into InsertFirst...InsertLast, because then
        cannot copy the end of file marker.
       }
  var
     P: Position;
  begin { CopyCommand }
        Success := True;
        Collect(DeleteFirst, DeleteLast);
        if EOT(SelectLast) then begin
            if EOT (SelectFirst) then begin
                PFirst := FilledLast;
                PLast := FilledLast;
            end
            else begin
                Add(SelectLast, -1, P);
                Copy(SelectFirst, P);
            end;
        end
        else
           Copy(SelectFirst, SelectLast);
        DeleteFirst := PFirst;
        DeleteLast := PLast;
        InsertFirst := FilledLast;
        InsertLast := FilledLast;;
        DrawLn(InsertL);
        DrawLn(DeleteL);
        ThisCmd := 'I';      {set to insert the item copied}
        CmdCh   := 'I';
  end { CopyCommand };
  
  
  procedure Find;
  var 
      SFirst, SLast: Position;
      Done: boolean;
      StartSDot: integer;
  begin { Find }
   Prompt('Find:  enter target string');
   Verify := False;    {This is a kludge so IOKeyClear will be called for 
                        Ctl-C.}
   SaveSpot;
   GatherLine(FindL);
   Success := Accept;
   if Success then
    if GoodPattern then
     begin Count := 0;
      if Direction = '>' then
       begin Add(SelectFirst,1,SourceFirst);
        Add(FilledLast,-1,SourceLast)
       end
      else
       begin Add(FilledFirst,2,SourceFirst);
        Add(SelectLast,-1,SourceLast)
       end;
      FindSetUp;
      LoadCurs;
      Done := false;
      Prompt('Finding, control-c to abort');
      if RepeatCount = 0 then RepeatCount := 1;
      repeat
       if Count = RepeatCount then Done := true
       else
        begin FindText;
         if EQ(PFirst,FilledLast) then Done := true
         else
          begin Count := Count + 1;
           Prompt('');
           if Direction = '>' then Add(PFirst,1,SourceFirst)
           else Add(PLast,-1,SourceLast)
          end
        end
      until Done;
      QuitProgress;
      CurrentPointer := NoP;
      if NE(PFirst,FilledLast) then
       begin SFirst := SelectFirst;
        SLast := SelectLast;
        SelectFirst := PFirst;
        SelectLast := PLast;
        CheckCRLF (SelectFirst,SelectLast);
        ThisSelect := SelectChar;
        DrawLn(SelectL);
        Show(SelectFirst,0,LastLine);
        if not SetSpot then
           DrawThumbBar;
        UnderLine(SFirst,SLast,White);
        UnderLine(SelectFirst,SelectLast,Black);
        if OnScreen(SelectFirst,0,LastLine) then
         ScreenPos(SelectFirst,ThisLine,ThisColumn);
       end
      else Warn('pattern not found')
     end
  end { Find };
  
  
  procedure Replace;
  var Good: boolean;
      Done, EmptyReplacement, ReDisplayScreen: boolean;
      L: LineIndex;
      C: ColumnIndex;

   handler CtlC; begin          {This handler is in case a ^-c occurs
                                 while we happen to be executing code
                                 outside FindText.}
      CtrlCPending := False;
      if not Verify then
          IOKeyClear;
      Done := True;
   end;  
  
   procedure SetUp;
   begin { SetUp }
    Prompt('Replace:  enter target string');
    GatherLine(FindL);
    Good := false;
    Success := Accept;
    if Success then
     if GoodPattern then
      begin Prompt('Replace:  enter replacement string');
       GatherLine(ReplacL);
       EmptyReplacement := EQ(ReplaceFirst,FilledLast);
       Good := Accept
      end
   end { SetUp };
  
  
  begin { Replace }
   SetUp;
   if Good then
    begin SourceFirst := SelectFirst;
     if EQ(SelectLast,FilledLast) then Add(FilledLast,-1,SourceLast)
     else SourceLast := SelectLast;
     Count := 0;
     Tmp := SelectFirst;
     Done := false;
     ReDisplayScreen := False;
     FindSetUp;
     LoadCurs;
     if not Verify then Prompt('Replacing, control-c to abort');
     while not Done do begin
      if Verify then Prompt('Finding, control-c to abort') else Prompt('');
      FindText;
      RawCh := Accept1;
      Quoted := False;
      if EQ(PFirst,FilledLast) then Done := true
      else
       begin 
        Count := Count + 1;
        if Verify then
         begin Show(PFirst,0,LastLine);
          UnderLine(PFirst,PLast,ExtraBlack);
          ScreenPos(PFirst,ThisLine,ThisColumn);
          if CurrentPointer <> TextP then
           begin IOLoadCursor(DefaultCursor,xTextPointer,yTextPointer);
            CurrentPointer := TextP
           end;
          MovePointer(ThisLine,ThisColumn);
          DrivenPointer;
          Prompt('Replace:  INS replaces, <space> doesn''t, DEL aborts');
          NextChar;
          while not (Accept or Reject or (RawCh = ' ')) do
           begin Write(Bel); NextChar end;
          Done := Reject;
          OffPointer
         end;
        if Accept then
         begin
          Done := Done or (Count = RepeatCount);
          Add(PFirst,-1,LeftPart);
          Add(PLast,1,RightPart);
          if Verify then ScreenPos(PFirst,L,C)
          else
           begin
            ReDisplayScreen := { last occurrance starts on the screen }
                Ge(PFirst,ScreenFirst) and Le(PFirst,ScreenLast);
            if Ge(ScreenFirst,PFirst) and Le(ScreenFirst,PLast) then
             { this occurrance straddles ScreenFirst }
             ScreenFirst := ReplaceFirst;
            if Ge(ScreenLast,PFirst) and Le(ScreenLast,PLast) then
             { this occurrance straddles ScreenLast }
             ScreenLast := ReplaceLast
           end;
          if EQ(SelectFirst,PFirst) then
           begin
            if EmptyReplacement then SelectFirst := RightPart
            else SelectFirst := ReplaceFirst;
            SourceFirst := SelectFirst
           end;
          if EQ(SelectLast,PLast) then
           begin
            if EmptyReplacement then SelectLast := LeftPart
            else SelectLast := ReplaceLast;
            SourceLast := SelectLast
           end;
          Tmp := ReplaceFirst;
          Split(PFirst);
          Split(RightPart);
          Collect(PFirst,PLast);
          if EmptyReplacement then begin
             Join(LeftPart,RightPart);
             Tmp := RightPart;
          end
          else
           begin Copy(ReplaceFirst,ReplaceLast);
            Join(LeftPart,ReplaceFirst);
            Join(ReplaceLast,RightPart);
            ReplaceFirst := PFirst;
            ReplaceLast := PLast
           end;
          if Direction = '>' then 
             if EQ(RightPart, FilledLast) then
                Done := True
             else SourceFirst := RightPart
          else SourceLast := LeftPart;
          if Verify then JoinScreen(Tmp,L,C);
          if GT(SelectFirst,SelectLast) then {deleted entire selection}
           begin Done := true;  SelectLast := SelectFirst end
         end {Accept replacement}
        else
         begin {did not choose to make the change to this instance}
          Count := Count - 1;
          if Verify then UnderLine(PFirst,PLast,Black);
          if Direction = '>' then Add(PFirst,1,SourceFirst)
          else Add(PLast,-1,SourceLast)
         end
       end;   {found the search string}
     end;  {while not Done}
     QuitProgress;
     CurrentPointer := NoP;
     Prompt('Replaced');
     if Count > 0 then begin 
       ThisSelect := SelectChar;
       FileChanged := True;
       CheckCRLF(SelectFirst,SelectLast);
       DrawLn(SelectL);
       if not Verify then
        if ReDisplayScreen then begin
{$IFC KeySelection THEN}  {this may fix a bug, but uses code only included
                           with KeySelection}
            Attach(DrawCursor, ScreenFirst, ReadCursor);
            if not IsLnStart(DrawCursor) then
               BckLnStart(DrawCursor, ScreenFirst);
            Detach(DrawCursor);
{$ENDC}
            Draw(ScreenFirst,FilledLast,0,-1)
        end
        else Show(Tmp,-1,-1);
        if OnScreen(Tmp,0,LastLine) then
           ScreenPos(Tmp,ThisLine,ThisColumn)
      end;
     if (RepeatCount <> 0) and (Count <> RepeatCount) and Accept then
      Warn('pattern not found')
    end
  end { Replace };
  
  
  function Thumb( var Q: Position ): Boolean;
  {returns true unless finds OldFirst or NoteFirst}
  var P: pChunk;
      T: integer;
      i, Limit: integer;
  begin { Thumb }
   Thumb := True;
   if ThisColumn <= 0 then Add(FilledFirst,2,Q)
   else
    if ThisColumn = ScreenFDot then Q := ScreenFirst
    else
     if ThisColumn = SelectDot then Q := SelectFirst
     else
{$IFC KeySelection THEN}
     if ThisColumn = OldDot then begin
          Thumb := False;
          Q := OldFirst;
     end
     else
     if ThisColumn = NoteDot then begin
          Thumb := False;
          Q := NoteFirst;
     end
     else
{$ENDC}
      if ThisColumn >= EofDot then Q := FilledLast
      else
       begin T := ThisColumn * PagesPerDot;
        P := FilledFirst.Chunk;
        Q.Chunk := P;
        while P^.OrderP < T do
         begin Q.Chunk := P; P := P^.Next end;
        Q.Offset := 0;
        if EQ(Q,FilledFirst) then Add(Q,2,Q)
        else
        (* 
          CRofCRLF(Q);
        (* *)         
         begin 
          Limit := NColumn*NLine-1;
          i := 0;
          Attach(DrawCursor,Q,ReadCursor);
          repeat
           repeat Sub1C(DrawCursor);
                  i := i+1;
           until (DrawCursor.Ch = LF) or (i>=Limit);
           Sub1C(DrawCursor);
           if DrawCursor.Ch <> CR then Add1C(DrawCursor)
          until (DrawCursor.Ch = CR) or (i>=Limit);
          if i<Limit then 
             Add(DrawCursor.Pos,2,Q)
          else if BOT(DrawCursor.Pos) then
             Add(FilledFirst,2,Q)
          else 
             Q := DrawCursor.Pos;
          Detach(DrawCursor);
         end;
        (* *)
       end;
  end { Thumb };
  
  
  procedure CharSelect;
  begin { CharSelect }
   TextPos(ThisLine,ThisColumn,SelectFirst);
   SelectLast := SelectFirst;
   CheckCRLF(SelectFirst,SelectLast);
   ThisSelect := SelectChar;
  end { CharSelect };

{$IFC KeySelection THEN} 
    {The following three routines can be shortened with the 
    Fwdxxx and Bckxxx routines.  They must be exported from EditorK.}
{$ENDC}  
  
  procedure WordSelect;
  begin { WordSelect }
   Attach(Cursor1,SelectFirst,ReadCursor);
   if Cursor1.Ch in WordChars then
    begin
     repeat Sub1C(Cursor1)
     until not (Cursor1.Ch in WordChars);
     Add(Cursor1.Pos,1,SelectFirst)
    end;
   ReAttach(Cursor1,SelectLast);
   if Cursor1.Ch in WordChars then
    begin
     repeat Add1C(Cursor1)
     until not (Cursor1.Ch in WordChars);
     Add(Cursor1.Pos,-1,SelectLast)
    end;
   Detach(Cursor1);
   CheckCRLF(SelectFirst,SelectLast);
   ThisSelect := SelectWord;
  end { WordSelect };

  
  procedure LineSelect;
  var Done: Boolean;
  begin { LineSelect }
   TextPos(ThisLine,0,SelectFirst);
   Attach(Cursor1,SelectFirst,ReadCursor);
   if Cursor1.Ch = LF then
    begin Sub1C(Cursor1);
     if Cursor1.Ch <> CR then Add1C(Cursor1)
    end;
   Done := false;
   repeat
    if Cursor1.Ch = CR then
     begin Add1C(Cursor1);
      if Cursor1.Ch = LF then Done := true
     end;
    if not Done then
     if Eot(Cursor1.Pos) then Done := true
     else Add1C(Cursor1)
   until Done;
   SelectLast := Cursor1.Pos;
   Detach(Cursor1);
   CheckCRLF(SelectFirst,SelectLast);
   ThisSelect := SelectLine;
  end { LineSelect };
  
 

  procedure Extend(SetBase: Boolean);
  var P, OldFirst, OldLast: Position;
      Done: Boolean;
      C: Cursor;
  begin { Extend }
     if ThisLine<=ThumbL then begin
        if Thumb(P) then {nothing};
     end
     else if ThisColumn < 0 then 
        TextPos(ThisLine,0,P)
     else 
        TextPos(ThisLine,ThisColumn,P);
     OldFirst := SelectFirst;
     OldLast := SelectLast;
     if SetBase then
        if LT(P,SelectFirst) then
           SelectFirst := P
        else if GT(P,SelectLast) then
           SelectLast := P
        else if Subtract(SelectLast, P) < Subtract(P,SelectFirst) then 
           {shrink right end toward middle}
           SelectLast := P
        else 
           {shrink left end toward middle}
           SelectFirst := P
     else {use old base}
        if LT(BaseFirst, P) then begin
           SelectFirst := BaseFirst;
           SelectLast := P;
        end
        else begin
           SelectFirst := P;
           SelectLast := BaseFirst;
        end;
     case ThisSelect of
        SelectNone,
        SelectChar: ;
        SelectWord: WordSelect;
        SelectLine: begin Attach(Cursor1,SelectFirst,ReadCursor);
                     repeat
                      repeat Sub1C(Cursor1)
                      until Cursor1.Ch = LF;
                      Sub1C(Cursor1);
                      if Cursor1.Ch <> CR then Add1C(Cursor1)
                     until Cursor1.Ch = CR;
                     Add(Cursor1.Pos,2,SelectFirst);
                     ReAttach(Cursor1,SelectLast);
                     if Cursor1.Ch = LF then
                      begin Sub1C(Cursor1);
                       if Cursor1.Ch <> CR then Add1C(Cursor1)
                      end;
                     Done := false;
                     repeat
                      if Cursor1.Ch = CR then
                       begin Add1C(Cursor1);
                        if Cursor1.Ch = LF then Done := true;
                       end;
                      if not Done then
                       if Eot(Cursor1.Pos) then Done := true
                       else Add1C(Cursor1);
                     until Done;
                     SelectLast := Cursor1.Pos;
                     Detach(Cursor1);
                    end;
     end;
     CheckCRLF(SelectFirst,SelectLast);
     
     if LT(SelectFirst, OldFirst) then begin 
           CheckCRLF(SelectFirst, OldFirst);
           UnderLine(SelectFirst, OldFirst, Black);
     end
     else if LT(OldFirst, SelectFirst) then begin
           Add(SelectFirst, -1, P);
           UnderLine(OldFirst, P, White);
     end;
     if LT(OldLast, SelectLast) then begin
           CheckCRLF(OldLast, SelectLast);
           UnderLine(OldLast, SelectLast, Black);
     end
     else if LT(SelectLast, OldLast) then begin
           Add(SelectLast, 1, P);
           UnderLine(P, OldLast, White);
     end;
     
     {maybe need to set ThisLine, ThisColumn for column number display}
     if NE(SelectFirst, OldFirst) then begin
           if OnScreen(SelectFirst,0,LastLine) then
               ScreenPos(SelectFirst,ThisLine,ThisColumn);
     end
     else
           if OnScreen(SelectLast,0,LastLine) then
               ScreenPos(SelectLast,ThisLine,ThisColumn);
     if (OldFirst<>SelectFirst) or (OldLast<>SelectLast) then begin
        DrawLn(SelectL);
        DrawThumbBar;
     end;
     
     if SetBase then {set base to be farthest from puck}
         if abs(Subtract(SelectLast, P)) < abs(Subtract(P,SelectFirst)) 
                   then 
             BaseFirst := SelectFirst
         else begin
{$ifc KeySelection then}  {(This code is used for rubberbanding.
                            It is controlled by KeySelection switch only
                            because it uses routines from EditorK.)}
             C.Attached := False;
             Attach(C, SelectLast, ReadCursor);
             case ThisSelect of
                 SelectNone, SelectChar:  begin
                         BaseFirst := SelectLast;
                         CheckCRLF(BaseFirst, P);
                    end;
                 SelectWord: BckWdStart(C, BaseFirst, LettersAndDigits);
                 SelectLine: BckLnStart(C, BaseFirst);
             end;
             Detach(C);
{$elsec}
             BaseFirst := SelectLast;
             CheckCRLF(BaseFirst, P);
{$endc}
         end;
  end { Extend };
  
  
  procedure SetAtColumn;
  begin { SetAtColumn }
   if OnScreen(SelectFirst,0,LastLine) then
    begin ScreenPos(SelectFirst,SL,SC);
     if SC <> AtColumn then
      begin AtColumn := SC;
       Prompt('')
      end
    end
   else AtColumn := -1
  end { SetAtColumn };

  
{$ifc UseMenu then}
  procedure PopPop; 
  label 30;
  handler OutSide;
     begin
        goto 30;
     end;
  var result: ResRes;
  begin {PopPop}
     Menu(OtherNames, False, 1, NumOther, TabRelX, TabRelY, -1, Result);
     case Result^.indices[1] of
     1: {ScrollForwd}    RawCh := LF;
     2: {ScrollBckwd}    RawCh := CtlLF;
     3: {VerifyReplace}  begin
                           Verify := True;
                           RawCh := 'R';
                         end;
     4: {Replace}        RawCh := 'R';
     5: {Insert}         RawCh := 'I';
     6: {Append}         RawCh := 'A';
     7: {Substitute}     RawCh := 'S';
     8: {FindForwd}      begin
                           Direction := '>';
                           RawCh := 'F';
                         end;
     8: {FindBckwd}      begin
                           Direction := '<';
                           RawCh := 'F';
                         end;
     9: {SelectAll}      RawCh := '*';
     10: {Quit}          RawCh := 'Q';
     end;
     DestroyRes(Result);
30:
  end; {PopPop}

  procedure UsePop; 
  label 30;
  handler OutSide;
     begin
        goto 30;
     end;
  var result: ResRes;
  begin {UsePop}
     Menu(MenuNames, False, 1, NumNames, TabRelX, TabRelY, -1, Result);
     case Result^.indices[1] of
     1: {Delete}  RawCh := 'D';
     2: {Copy}    RawCh := 'C';
     3: {ReInsert}begin
                    RawCh := Ins;
                    LastCmd := 'I';
                  end;
     4: {ReSubst} begin
                    RawCh := Ins;
                    LastCmd := 'S';
                  end;
     4: {Menu}    PopPop;
     4: {HELP}    RawCh := Help;
     end;
     Quoted := False;
     HaveKey := True;
     DestroyRes(Result);
30:
  end; {UsePop}
{$endc}

  procedure ReSpot;
  var Tmp: Position;
  begin 
       SaveSpot;
       if Thumb(Tmp) then 
           Show(Tmp, 0, LastLine div 2) 
       else
           Draw(Tmp, FilledLast, 0, -1);
       if SetSpot then {nothing};
  end;
  
  
  procedure TrackPointer;
  var
     P: Position;
     StartSDot: integer;
  begin { TrackPointer }
   if NeedPrompt then
    begin
{$ifc ReplayVersion then}
     if Replay = NotReplaying then Prompt(CmdPrompt)
     else Prompt('Replay:  SPACE, CR, LF, INS, DEL');
{$elsec}
     Prompt(CmdPrompt);
{$endc}
     NeedPrompt := false
    end;
   NextPointer;

   { The screen is divided in these areas:
        *---*---*---------------*
        |   |   |               |
        |   |   |    ThumbP     |
        |   |   |               |
        |   |D*-*---------------*
        |   |o| |               |
        | U |w|L|               |
        | p |n|i|               |
        | P |P|n|     TextP     |
        |   | |e|               |
        |   | |P|               |
        |   | | |               |
        |   | | |               |
        |   | | |               |
        |   | | |               |
        *---*-*-*---------------*
     
     A press in the lower UpP area will move window down through file,
     but a press in the upper left UpP area will move the window up
     so it is aligned with where the pointer would be in the file.
   }
   
   if ThisLine <= ThumbL then
        if ThisColumn <= UpC then 
             NewPointer := UpP                        { scroll-up bar }
        else if ThisColumn <= LSelectC then
             NewPointer := DownP                      { scroll-down bar }
        else
             NewPointer := ThumbP                     { thumb bar }
   else if ThisColumn<=UpC then 
        NewPointer := UpP                             { scroll-up bar }
   else if ThisColumn = LSelectC then
        NewPointer := LineP                           { point at entire line }
   else if ThisColumn = DownC then
        NewPointer := DownP                           { scroll-down bar }
   else NewPointer := TextP;                          { normal text ptr }
   
   if (NewPointer=UpP) and RightSwitch then
        NewPointer := DownP                           { down in up bar }
   else if (NewPointer=DownP) and MiddleSwitch then
        NewPointer := UpP;                            { up in down bar }
  
   if NewPointer <> CurrentPointer then
    begin
     case NewPointer of
      TextP:  IOLoadCursor(DefaultCursor,xTextPointer,yTextPointer);
      LineP:  IOLoadCursor(LinePointer,xLinePointer,yLinePointer);
      DownP:  IOLoadCursor(DownPointer,xDownPointer,yDownPointer);
      UpP:    IOLoadCursor(UpPointer,xUpPointer,yUpPointer);
      ThumbP: IOLoadCursor(ThumbPointer,xThumbPointer,yThumbPointer)
      end
    end;
   CurrentPointer := NewPointer;
   if Switch then 
    if not OldSwitch then begin 
     NeedPrompt := true;
     if RightSwitch and (CurrentPointer in [TextP, LineP, ThumbP]) then 
         Extend(True)
{$ifc UseMenu then}
     else if MiddleSwitch and (CurrentPointer in [TextP, LineP, ThumbP]) then 
         UsePop
{$endc}
     else
      case CurrentPointer of
       TextP,
       LineP:  if ThisLine >= 0 then
                begin
                 UnderLine(SelectFirst,SelectLast,White);
                 if ThisColumn < 0 then 
                    LineSelect
                 else if BottomSwitch then
                    LineSelect
{$ifc not UseMenu then}
                 else if MiddleSwitch then 
                    if (LastButton <> MiddleB) or (ThisSelect<>SelectWord) or 
                         (ThisColumn<>OldColumn)or(ThisLine<>OldLine)then begin
                       CharSelect;
                       WordSelect;
                    end
                    else  
                       LineSelect
{$endc}
                 else  {LeftSwitch}
                    if LastButton <> LeftB then CharSelect
                    else if (ThisLine<>OldLine) or (ThisColumn<>OldColumn) then
                       CharSelect
                    else if ThisSelect=SelectChar then
                       WordSelect
                    else if ThisSelect=SelectWord then
                       LineSelect
                    else CharSelect;
                 UnderLine(SelectFirst,SelectLast,Black);
                 DrawLn(SelectL);
                 DrawThumbBar
                end
               else Warn('Pointer off screen, can''t select');
       UpP:    if ThisLine >= 0 then
                   ScrollUp(0,LastLine,ThisLine)
               else ScrollDown(0,LastLine, -ThisLine);
       DownP:  if ThisLine >= 0 then
                   ScrollDown(0,LastLine,ThisLine)
               else ScrollUp(0,LastLine, -ThisLine);
       ThumbP: ReSpot;
       end; {case}
     if LeftSwitch then         LastButton := LeftB
     else if MiddleSwitch then  LastButton := MiddleB
     else if RightSwitch then   LastButton := RightB
     else                       LastButton := NoneB;
     PuckMoved := False;
     OldLine := ThisLine;
     OldColumn := ThisColumn;
     SetAtColumn
    end {not OldSwitch}
{$ifc SweepScope then}
    else if (LastButton=LeftB) and (CurrentPointer=TextP) 
            and LeftSwitch then begin                     { left held down }
     UnderLine(SelectFirst,SelectLast,White);
     if abs(OldColumn-ThisColumn) > 6 then begin
         ThisLine := OldLine;
         LineSelect;
         PuckMoved := True;
     end 
     else if abs(OldColumn-ThisColumn) > 1 then begin
         WordSelect;
         PuckMoved := True;
     end
     else if PuckMoved then begin
         ThisLine := OldLine;
         ThisColumn := OldColumn;
         CharSelect;
         PuckMoved := False;
     end;
     UnderLine(SelectFirst,SelectLast,Black);
     DrawLn(SelectL);
     SetAtColumn;
    end
{$endc}
{The following is commented out until I can see how to make it work for replay}
{
{   else if (LastButton=MiddleB) and (CurrentPointer=TextP) 
{           and MiddleSwitch then                          { middle held down }
{        Extend(False)
{ };
  end { TrackPointer };
  
 
  procedure printchunks;

  
   procedure printlist( Q, R: Position; S: string );
   var P: pChunk;
       X: record case integer of
           1: (P: pChunk);
           2: (Seg: integer;
               Off: integer)
           end;
       done: boolean;
   begin { printlist }
    write(chr(12));
    writeln(s);
    P := Q.Chunk;
    Writeln('starting offset = ',Q.Offset);
    repeat done := P = R.Chunk;
     with P^ do
      begin X.P := P;
       write('chunk=', X.Seg:1, ',', X.Off:1);
       X.P := next;
       write(' next=', X.Seg:1, ',', X.Off:1);
       X.P := prev;
       write(' prev=', X.Seg:1, ',', X.Off:1);
       write(' cpage=', cpage:1, ' first=', first:1, ' length=', length:1);
       writeln(' order=', orderp:1, ',', orderc:1);
       P := next
      end
    until done;
    writeln('ending offset = ',R.Offset);
    nextchar
   end { printlist };
  
  
  begin { printchunks }
   printlist(selectfirst,selectlast,'** Select **');
   printlist(FilledFirst,FilledLast,'** Filled **');
   printlist(emptyfirst,emptylast,'** Empty **');
   RefreshScreen
  end { printchunks };


  procedure CommandPrefix;
  
  
   procedure GatherNumber;
   var NextNumber, Digit: integer;
   begin { GatherNumber }
    MovePencil(PromptL,RepeatC);
    Write('R    ');
    Digit := 1;
    NextNumber := 0;
    repeat TrackPointer;
     if Digit = 5 then { trying to make a 5 digit number }
      begin Warn('repeat count too large');
       exit(GatherNumber)
      end;
     MovePencil(PromptL, RepeatC + Digit);
     Write(Ch);
     NextNumber := NextNumber * 10 + Ord(Ch) - Ord('0');
     Digit := Digit + 1;
     repeat TrackPointer until KeyStruck
    until not (Ch in ['0'..'9']);
    RepeatCount := NextNumber
   end { GatherNumber };
  
  
  begin { CommandPrefix }
   RepeatCount := 0;
   while Ch in ['0'..'9', '>', '.', '+', '<', ',', '-', 'V', 'v'] do
    begin TrackPointer;
     if Ch in ['0'..'9'] then GatherNumber
     else
      begin
       if Ch in ['>', '.', '+', '<', ',', '-'] then
        begin
         if Ch in ['>', '.', '+'] then Direction := '>'
         else Direction := '<';
{$IFC KeySelection THEN}
         LastDirection := Direction;
{$ENDC}
         MovePencil(PromptL,FirstColumn);
         Write(Direction)
        end
       else
        begin Verify := not Verify;
         MovePencil(PromptL,VerifyC);
         if Verify then Write('V') else Write(' ')
        end;
       repeat TrackPointer until KeyStruck
      end
    end
  end { CommandPrefix };
        
 Handler HELPkey (var S: sys9s);  begin
    s := ' ';
    s[1] := Help;
 end;

 begin { Edit }
  LastCmd := NoCommand;
  LastDirection := '>';
  CmdPrompt := EdVersion;
  if length(FileName) <= 28 then
     CmdPrompt := Concat (CmdPrompt, Concat ('   ', FileName))
  else begin
     AppendChar(CmdPrompt, ' ');
     AppendChar(CmdPrompt, ' ');
     AppendChar(CmdPrompt, DotDotDot);
     AppendChar(CmdPrompt, DotDotDot);
     CmdPrompt := Concat (CmdPrompt, 
                   Substr (FileName, Length(Filename)-26, 27) );
  end;
  if CreatingFile then
   begin
    NewFile := Concat('creating new file: ',FileName);
    Warn(NewFile);
    CreatingFile := false;
    NeedPrompt := false
   end
  else NeedPrompt := true;
  while Editing do
   begin
    repeat TrackPointer until KeyStruck;
    RepeatCount := 0;
    Count := 0;
    CommandPrefix;
    NeedPrompt := true;
{$IFC KeySelection THEN}
    if RawCh = Ins then
     begin CmdCh := LastCmd;
      Direction := LastDirection;
      Automatic := true
     end
    else
     begin CmdCh := RawCh;
      Automatic := false
     end;
    if CmdCh in ['a'..'z'] then 
     ThisCmd := Chr(Ord(CmdCh) + UClesslc)   
    else 
     ThisCmd := CmdCh;
{$ELSEC}
    if RawCh = Ins then
     begin ThisCmd := LastCmd;
      Automatic := true
     end
    else
     begin ThisCmd := RawCh;
      Automatic := false
     end;
    if ThisCmd in ['a'..'z'] then
     ThisCmd := Chr(Ord(ThisCmd) - Ord('a') + Ord('A'));
{$ENDC}
    if ThisCmd in ['A', 'C', 'D', 'E', 'F', 'I', 'Q', 'R', 'S', 'V', '*',
{$IFC KeySelection THEN}
                'W', ';', 'L', 'P',        'G', 'M',
                ' ', CR, BS1, BS2, BS3, TAB, CtlTAB, CtlCR,
                BW1, BW2, BW3, BL1, BL2, BL3, 
                LF, CtlLF, 'T', 'B', 'N',
                'X', 
{$ELSEC}
                'X',
{$ENDC}
                Help, '!'] then
     begin
{$ifc ReplayVersion then}
      if Replay <> NotReplaying then
       begin
        CheckReplay(ReplayCommand);
        if Replay = NotReplaying then Exit(Edit)
       end;
{$endc}
      Success := true;
      OffPointer;
      if PointAllowed then
         IOSetModeTablet(OffTablet);
      case ThisCmd of
       '!'  : begin
               ClearLine(FirstLine,FirstColumn);
               NextChar;
               if Ch in ['1'..'4'] then
                case Ch of
                 '1': begin Write('Debug 1 ');
                       DEBUG1 := not DEBUG1;
                       if DEBUG1 then Write('on') else Write('off')
                      end;
                 '2': begin Write('Debug 2 ');
                       DEBUG2 := not DEBUG2;
                       if DEBUG2 then Write('on') else Write('off')
                      end;
                 '3': begin Write('Debug 3 ');
                       DEBUG3 := not DEBUG3;
                       if DEBUG3 then Write('on') else Write('off')
                      end;
                 '4': printchunks
                 end
               else Write(Bel);
               NextChar
              end;
       'A', 'I', 'S':   Insert(ThisCmd);
       'C':             CopyCommand;
       'D':             Delete;
       'E':             Extend(True);
       'F':             Find;
       'Q':             Editing := false;
       'R':             Replace;
       'V':             begin Verify := not Verify; NeedPrompt := true end;
{$IFC KeySelection THEN}
       'W',';','L','P': 
                        PickUnit(CmdCh);
       'G':             GotoChar(Automatic);
       'M':             begin
                           Moreing := not Moreing;
                           NeedPrompt := True;
                        end;
       'N':             begin
                           MovePencil(ThumbL, NoteDot);
                           Write(' ');
                           NoteDot := ScreenFDot;
                           NoteFirst := ScreenFirst;
                           DrawThumbBar;
                        end;
       ' ', CR, TAB, 
       CtlCR, CtlTAB,
       BS1, BS2, BS3,
       BW1, BW2, BW3,
       BL1, BL2, BL3:   begin
                           CharMove(CmdCh);
                           if not Moreing then Direction := '>';
                        end;
       LF,CtlLF,'T','B':ScrollCmd(CmdCh);
       'X':             begin
                           if KeyThumb then ReSpot;
                           ScreenPos(SelectFirst, ThisLine, ThisColumn);
                           if (ThisLine<0) or (ThisLine>LastLine) then begin
                              ThisLine := 0;
                              ThisColumn := LastColumn-1;
                           end;
                        end;
{$ELSEC}
       'X':             RefreshScreen;
{$ENDC}
       '*':             begin
                         Add(FilledFirst,2,SelectFirst);
                         SelectLast := FilledLast;
                         DrawLn(SelectL);
                         DrawThumbBar;
                         UnderLine(SelectFirst,SelectLast,Black)
                        end;
       Help:            HelpTheUser
       end;
      if Success then
       begin
{$ifc ReplayVersion then}
        if ThisCmd in ['A', 'I', 'S', 'D', 'F', 'R'] then 
           FlushTranscript;
{$endc}
{$IFC KeySelection THEN}
        if ThisCmd in ['A', 'I', 'S', 'D', 'F', 'R', 'G'] then begin
            LastDirection := Direction;
            LastCmd := CmdCh   
        end;
        if ThisCmd in ['A', 'I', 'S', 'D', 'F', 'Q', 'R', 'T', 'B',
                       'X', '*', LF, CtlLF] then begin
            Direction := '>';
            if ThisCmd<>'*' then Verify := False;
            Moreing := False;
        end;
{$ELSEC}
        if ThisCmd in ['A', 'I', 'S', 'D', 'F', 'R'] then LastCmd := ThisCmd;
{$ENDC}
        if ThisCmd in ['A', 'I', 'S', 'D'] then
            FileChanged := True;  {note that Success is True at this point}
        if ThisCmd in ['S', 'D', 'F', '*'] then ThisSelect := SelectChar
       end
      else LastCmd := NoCommand;
      SetAtColumn;
      if PointAllowed then
         IOSetModeTablet(RelTablet);
      OnPointer;
     end {if in [ ... ]}
{$IFC not KeySelection THEN}
    else Write(Bel);
{$ELSEC}
    else begin 
       if Reject then begin
          Moreing := False;
          Direction := '>';
          Verify := False;
       end;
       Write(Bel);
    end;
{$ENDC}
    OldLine := FirstLine;
    OldColumn := FirstColumn;
    MovePointer(ThisLine,ThisColumn)
   end {while Editing}
 end { Edit };
  
 
 
begin { Editor }
 EdVersion := concat('Editor ', EditorVersion);
{$ifc not ReplayVersion then}
 AppendChar(EdVersion, 'r');
{$endc}
{$ifc not KeySelection then}
 AppendChar(EdVersion, 'k');
{$endc}
{$ifc SweepScope then}
 AppendChar(EdVersion, 'S');
{$endc}
{$ifc UseMenu then}
 AppendChar(EdVersion, 'M');
{$endc}

 EditInit;

{$ifc UseMenu then}
 HaveKey := False;
 InitPopUp;
 AllocNameDesc(NumNames, 0, MenuNames);
 with MenuNames^ do begin
    Header := '';
    NumCommands := NumNames;
{$R-}
    Commands[1] := 'Delete';
    Commands[2] := 'Copy';
    Commands[3] := 'ReInsert';
    Commands[4] := 'ReSubst';
    Commands[5] := 'Menu';
    Commands[6] := 'HELP';
{$R=}
 end;
 AllocNameDesc(NumOther, 0, OtherNames);
 with OtherNames^ do begin
    Header := '';
    NumCommands := NumOther;
{$R-}
    Commands[1] := 'ScrollForwd';
    Commands[2] := 'ScrollBckwd';
    Commands[3] := 'VerifyReplace';
    Commands[4] := 'Replace';
    Commands[5] := 'Insert';
    Commands[6] := 'Append';
    Commands[7] := 'Substitute';
    Commands[8] := 'FindForwd';
    Commands[8] := 'FindBckwd';
    Commands[9] := 'SelectAll';
    Commands[10] := 'Quit';
{$R=}
 end;
{$endc}

 while Editing do begin Edit; Terminate end
end { Editor }.
