{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module EditorUtilities;
{------------------------
{ Abstract:
{     Utility routines for Editor
{-----------------------}

{------------------------
{ Change Log:
    
    23 Feb 83   V2.9  WJHansen
       Introduce DrawBLoc to limit E-Stack depth utilization. (proved futile)
       Fix Dot in DrawThumbBar so it does not go off screen.
       If substitute a line, be sure selection is underlined afterward.

    15 Feb 82   V2.8   WJHansen
       Limit backward scan in ScrollDown and Show.
       Put HaveKey under UseMenu switch.
       Adapt for landscape monitor.
       Fix Collect to actually reset pointers.
       Handle PartFull.
       Draw a line under PromptL.
       Commented out code to smooth scroll if within 5 lines of end of screen.

{   28 Sep 82   V2.7   WJHansen
{      Use Ln.Length in DrawUnderLine
{      Convert to use SScreenW (even in DrawByte)
{
{    1 Sep 82   V2.7b  WJHansen
{      Use HaveKey in KeyStruck
{
{   29 Jul 82   V2.6   WJHansen
{      Remove a useless test in F in Fixup.
{      Change to use IF in ReOrder instead of mod.
{      Test for file too big in ReOrder (this is much too late).
{
{   26 May 82   V2.5   WJHansen
{      Fix ReplayPrompt for SearchFail and SearchOK.
{-----------------------}



exports

 imports Editor from Editor;
 imports System from System;
  
  
 {-- screen utilities --}

  
 procedure NextPointer;
 procedure MovePointer( L: LineIndex; C: ColumnIndex );
 procedure OnPointer;
 procedure DrivenPointer;
 procedure OffPointer;
 procedure MovePencil( L: LineIndex; C: ColumnIndex ); 
 procedure ClearLine( L: LineIndex; C: ColumnIndex ); 
 procedure WriteChar( Ch: Char );
 procedure EraseChar;
 procedure DeleteChar( L: LineIndex; C: ColumnIndex ); 
 procedure InsertChar( L: LineIndex; C: ColumnIndex );    {10}
 procedure ClearScreen( L: LineIndex; C: ColumnIndex );   
 function  KeyStruck: boolean;
 procedure NextChar; 
 function  Accept: Boolean;
 function  Reject: Boolean;
 procedure Prompt( S: string ); 
 procedure Status( S: string ); 
 procedure Error( S: string ); 
 procedure Warn( S: string ); 
 
 
 
 
 {-- disk/memory paging routines --}
 
 
 function Mem( D: DiskPage ): MemPage;       {20}
 procedure CreateEmptyPage;             
 
 
 
 
 {-- position and cursor routines --}
 
 
 procedure Add1( var P: Position ); 
 procedure Sub1( var P: Position );    
 procedure Add( P: Position; N: integer; var Q: Position ); 
 procedure Detach( var C: Cursor ); 
 procedure Attach( var C: Cursor; P: Position; RW: ReadWrite ); 
 procedure ReAttach( var C: Cursor; P: Position ); 
 procedure Add1C( var C: Cursor ); 
 procedure Sub1C( var C: Cursor ); 
 procedure AddC( var C: Cursor; N: integer );     {30}
 function  Subtract( P1, P2: Position ): integer; 
 function  LT( P1, P2: Position ): boolean; 
 function  LE( P1, P2: Position ): boolean;     
 function  EQ( P1, P2: position ): boolean; 
 function  NE( P1, P2: position ): boolean; 
 function  GT( P1, P2: Position ): boolean; 
 function  GE( P1, P2: Position ): boolean; 
 function  Bot( P: Position ): boolean; 
 function  Eot( P: Position ): boolean; 
 
 
 
 
 {-- display routines --}
 
 
 procedure TextPos( L: LineIndex; C: ColumnIndex; var P: Position ); {40}
 procedure ScreenPos( P: Position
                      var L: LineIndex; var C: ColumnIndex );   
 procedure DrawThumbBar;
 procedure DrawScrollBar;      
 procedure DrawLn( L: LineIndex ); 
 procedure Draw( P, Q: Position; L: LineIndex; C: ColumnIndex ); 
 procedure UnderLine( First, Last: Position; U: UnderKind ); 
 function  OnScreen( P: Position; L1, L2: LineIndex ): boolean;
 procedure Show( P: Position; L1, L2: LineIndex ); 
 procedure ScrollUp( L1, L2: LineIndex; Distance: integer ); 
 procedure ScrollDown( L1, L2: LineIndex; Distance: integer );       {50}
 procedure JoinScreen( P: Position; L: LineIndex; C: ColumnIndex );  
 procedure RefreshScreen; 
 
 
 
 {-- text maintenance routines --}
 
 
 procedure FixUp( OldC, NewC: pChunk; Adjust: integer);     
 procedure Split( var P: Position ); 
 procedure Join( var P, Q: Position ); 
 procedure Collect( P, Q: Position ); 
 procedure Copy( First, Last: Position ); 
 procedure ReOrder( Start: Position );
 procedure NewChunk( var P: pChunk );
 
 
 
{$ifc ReplayVersion then}
 {-- transcript routines --}
 
 
 procedure NextTranscript;                    {60}
 procedure SendTranscript( Word: Integer );   
 procedure FlushTranscript;
 procedure CheckReplay( Where: ReplayMode );
{$endc}
 
 
 
 
private




 imports Screen      from Screen;
 imports Raster      from Raster;
 imports IO_Others   from IO_Others;
 imports IO_Unit     from IO_Unit;
 imports IOErrors    from IOErrors;
 imports Memory      from Memory;
 imports System      from System;     {for PointAllowed}
 imports FileSystem  from FileSystem;
 imports AllocDisk   from AllocDisk;  {for PartFull}
 imports Clock       from Clock;
 imports Helper      from Helper;
{$ifc ReplayVersion then}
 imports Perq_String from Perq_String;
{$endc}
 
 


var OldTime: TimeStamp;


 
 
 {-- utilities --}
 
 
 procedure NextPointer;
 var X, Y, OldX, OldY, ThisL, ThisC: integer;
     SwitchInfo: packed record case integer of
                   1: (I: Integer);
                   2: (Yellow: Boolean;
                       White:  Boolean;
                       Green:  Boolean;
                       Blue:   Boolean)
                   end;
 begin { NextPointer }
{$ifc ReplayVersion then}
  if Replay <> NotReplaying then
   begin
    if TranscriptWord = TabPos then
     begin
      NextTranscript;
      TabX := TranscriptWord;
      NextTranscript;
      TabY := TranscriptWord;
      NextTranscript;
      OldSwitch := False;
      Switch := False;
      IOSetTabPos(TabX, TabY)
     end
    else
     if TranscriptWord = TabHit then
      begin
       CheckReplay(ReplayCharOrHit);
       if Replay = NotReplaying then Exit(NextPointer);
       NextTranscript;
       SwitchInfo.I := TranscriptWord;
       with SwitchInfo do
        begin
         MiddleSwitch := Yellow;
         LeftSwitch := White;
         RightSwitch := Green;
         BottomSwitch := Blue
        end;
       NextTranscript;
       OldSwitch := False;
       Switch := True
      end
   end
  else
   begin
{$endc}
    OldSwitch := Switch;
    InLineByte( #151 {INTOFF} );
{$ifc ReplayVersion then}
    TabX := TabRelX;
    TabY := TabRelY;
{$elsec}
    X := TabRelX;
    Y := TabRelY;
{$endc}
    Switch := TabSwitch;
    MiddleSwitch := TabYellow;
    LeftSwitch := TabWhite;
    RightSwitch := TabGreen;
    BottomSwitch := TabBlue;
    InLineByte( #152 {INTON} );
{$ifc ReplayVersion then}
    if Switch and not OldSwitch then
     begin
      SendTranscript(TabPos);
      SendTranscript(TabX);
      SendTranscript(TabY);
      SendTranscript(TabHit);
      with SwitchInfo do
       begin
        Yellow := MiddleSwitch;
        White := LeftSwitch;
        Green := RightSwitch;
        Blue := BottomSwitch
       end;
      SendTranscript(SwitchInfo.I);
      OldTabX := TabX;
      OldTabY := TabY
     end
   end;
{$endc}
  OldX := OldColumn * CharWidth + HomeX + CharWidth div 2;
  OldY := OldLine * LineHeight + HomeY + CharHeight div 2;
{$ifc ReplayVersion then}
  if (Abs(TabX - OldX) > CharWidth div 2 + 2) or
     (Abs(TabY - OldY) > LineHeight div 2 + 3) then
{$elsec}
  if (Abs(X - OldX) > CharWidth div 2 + 2) or
     (Abs(Y - OldY) > LineHeight div 2 + 3) then
{$endc}
   begin
{$ifc ReplayVersion then}
    X := TabX - HomeX;
{$elsec}
    X := X - HomeX;
{$endc}
    if X >= 0 then ThisC := X div CharWidth
    else ThisC := (X - CharWidth + 1) div CharWidth;
    if ThisC < FirstColumn then ThisColumn := FirstColumn
    else
     if ThisC > LastColumn then ThisColumn := LastColumn
     else ThisColumn := ThisC;
{$ifc ReplayVersion then}
    Y := TabY - HomeY;
{$elsec}
    Y := Y - HomeY;
{$endc}
    if Y >= 0 then ThisL := Y div LineHeight
    else ThisL := (Y - LineHeight + 1) div LineHeight;
    if ThisL < FirstLine then ThisLine := FirstLine
    else
     if ThisL > LastLine then ThisLine := LastLine
     else ThisLine := ThisL
   end
  else
   begin
    ThisColumn := OldColumn;
    ThisLine := OldLine
   end
 end { NextPointer };
 
 
 procedure MovePointer(L: LineIndex; C: ColumnIndex);
 begin { MovePointer }
  IOSetCursorPos( C * CharWidth + HomeX + CharWidth div 2,
                  L * LineHeight + HomeY + CharHeight div 2 );
  ThisColumn := C;
  ThisLine := L
 end { MovePointer };
 
 
 procedure OffPointer;
 begin { OffPointer }
  IOCursorMode(OffCursor)
 end { OffPointer };
 
 
 procedure DrivenPointer;
 begin { DrivenPointer }
  IOCursorMode(IndepCursor)
 end { DrivenPointer };
 
 
 procedure OnPointer;
 begin { OnPointer }
  if not PointAllowed then
   IOCursorMode(IndepCursor)
  else
   IOCursorMode(TrackCursor)
 end { OnPointer };
 
    
 procedure MovePencil(L: LineIndex; C: ColumnIndex);
 begin { MovePencil }
  SSetCursor( C * CharWidth + HomeX,
              L * LineHeight + CharHeight + HomeY )
 end { MovePencil };
 
 
    { The '-1's in the following code sections are for underlines and
    { reverse-video control characters.  They extend one bit to the left
    { of standard characters. }
 
 procedure WriteChar(Ch: Char);
 var XC, YC: Integer;
 begin { WriteChar }
  if Ch >= ' ' then SPutChr(Ch)
  else
   begin SReadCursor(XC,YC);
    SPutChr( Chr(Ord(Ch) + Ord('@')) );
    RasterOp(RNot, CharWidth, CharHeight + 2,
             XC - 1, YC-CharHeight - 1, SScreenW, Scrn,
             XC - 1, YC-CharHeight - 1, SScreenW, Scrn)
   end
 end { WriteChar };
 
 
 procedure EraseChar;
 var XC, YC: Integer;
 begin { EraseChar }
  SReadCursor(XC,YC);
  RasterOp(RXor, CharWidth, CharHeight + 2,
           XC - 1, YC - CharHeight - 1, SScreenW, Scrn,
           XC - 1, YC - CharHeight - 1, SScreenW, Scrn)
 end { EraseChar };
 
 
 procedure ClearLine(L: LineIndex; C: ColumnIndex);
 var XC, YC: integer;
 begin { ClearLine } 
  XC := C * CharWidth + HomeX;
  YC := L * LineHeight + HomeY;
  RasterOp(RXor, RightLimit - XC, LineHeight,
           XC - 1, YC - 1, SScreenW, Scrn,
           XC - 1, YC - 1, SScreenW, Scrn);
  if (C < 0) and (L >= 0) then DrawScrollBar;
  MovePencil(L,C)
 end { ClearLine };
 
 
 procedure DeleteChar(L: LineIndex; C: ColumnIndex);
 var XC, YC: integer;
 begin { DeleteChar }
  XC := C * CharWidth + HomeX;
  YC := L * LineHeight + HomeY;
  RasterOp(RRpl, RightLimit - XC - CharWidth, LineHeight, 
           XC - 1, YC - 1, SScreenW, Scrn,
           XC + CharWidth - 1, YC - 1, SScreenW, Scrn);
  MovePencil(L,LastColumn);
  EraseChar
 end { DeleteChar };
 
 
 procedure InsertChar(L: LineIndex; C: ColumnIndex);
 var XC, YC: integer;
 begin { InsertChar }
  XC := C * CharWidth + HomeX;
  YC := L * LineHeight + HomeY;
  RasterOp(RRpl, RightLimit - XC - CharWidth, LineHeight, 
           XC + CharWidth - 1, YC - 1, SScreenW, Scrn,
           XC - 1, YC - 1, SScreenW, Scrn);
  MovePencil(L,C);
  Write(' ')
 end { InsertChar };
   
 
 procedure ClearScreen(L: LineIndex; C: ColumnIndex);
 var i, j: integer;
     XC, YC: integer;
 begin { ClearScreen }
  ClearLine(L,C);
  if L <> LastLine then
   begin XC := HomeX;
     YC := (L+1) * LineHeight + HomeY;
     RasterOP(RXor, RightLimit - XC, BottomLimit - YC,
              XC - 1, YC - 1, SScreenW, Scrn,
              XC - 1, YC - 1, SScreenW, Scrn)
    end;
  if L < 0 then DrawThumbBar;
  if C < 0 then DrawScrollBar;
  MovePencil(L,C)
 end { ClearScreen };
    
 function KeyStruck: boolean;
 var Status: integer;
     Time: TimeStamp;
     TimeStr: String;
     X, Y: Integer;
 begin { KeyStruck }
{$ifc ReplayVersion then}
  if Replay <> NotReplaying then
   begin
    if (TranscriptWord = TabPos) or (TranscriptWord = TabHit)
           or (TranscriptWord = SearchOK) or (TranscriptWord = SearchFail) then
     Status := IOEIOB
    else
     begin RawCh := Chr(LAnd(TranscriptWord,#377));
      CheckReplay(ReplayCharOrHit);
      if Replay = NotReplaying then
       begin KeyStruck := False;
        Exit(KeyStruck)
       end;
      NextTranscript;
      Status := IOEIOC
     end
   end
  else
   begin
{$endc}

{$ifc UseMenu then}
    if not HaveKey then
       Status := IOCRead(KeyBoard,RawCh)
    else begin
       HaveKey := False;
       Status := IOEIOC;
    end;
{$elsec}
    Status := IOCRead(KeyBoard,RawCh);
{$endc}

{$ifc ReplayVersion then}
    if Status = IOEIOC then
     begin
      if (TabX <> OldTabX) or (TabY <> OldTabY) then
       begin
        SendTranscript(TabPos);
        SendTranscript(TabX);
        SendTranscript(TabY);
        OldTabX := TabX;
        OldTabY := TabY
       end;
      SendTranscript(Ord(RawCh))
     end
   end;
{$endc}
  if Status = IOEIOC then
   begin KeyStruck := true;
    if LAnd(Ord(RawCh), #200) = 0 then Ch := RawCh
    else Ch := Chr(LAnd(Ord(RawCh), #37))
   end
  else
   begin KeyStruck := false;
    if Status <> IOEIOB then Error('IOCRead failure')
   end;
  CtrlCPending := false;   { just in case the user typed control-C }
  CtrlSPending := false;   { just in case the user typed control-S }
  GetTStamp(Time);
  if Time.Second <> OldTime.Second then
   begin StampToString(Time,TimeStr);
    SReadCursor(X,Y);
    MovePencil(PromptL,TimeC);
    Write(TimeStr);
    SSetCursor(X,Y);
    OldTime := Time;
    Y := HomeY+PromptL*LineHeight+CharHeight+1;
    RasterOp(RXNor, WinWidth-XMargin-XMargin, 1,
               WinOrgX+XMargin, Y, SScreenW, Scrn,
               WinOrgX+XMargin, Y, SScreenW, Scrn);
   end;
 end { KeyStruck };
 
 
 procedure NextChar;
 
    procedure GetQuoted;
    const CtlC = chr(#3);
    label 1;
    handler CtlShftC; begin
       CtrlCPending := False;
       RawCh := chr(LOr(#200, ord(CtlC)));
       Ch    := CtlC;
{$ifc ReplayVersion then}
       if Replay=NotReplaying then
           SendTranscript(Ord(RawCh));
{$endc}
       goto 1;
    end;
    begin {GetQuoted}
       SCurChr(QuoteCur);
       repeat
   {$ifc ReplayVersion then}
        if Replay <> NotReplaying then NextPointer
   {$endc}
       until KeyStruck;
     1:
       SCurChr(NormalCur)
    end {GetQuoted};
 
 begin { NextChar }
  SCurOn;
  repeat
{$ifc ReplayVersion then}
   if Replay <> NotReplaying then NextPointer
{$endc}
  until KeyStruck;
  Quoted := RawCh = QuoteCh;
  if Quoted then
      GetQuoted;
  SCurOff
 end { NextChar };


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
  
 
 
 procedure Prompt(S: String);
 var Time: TimeStamp;
     TimeStr: String;
     X, Y: Integer;
  
  
  procedure WriteNum( Letter: Char; C: Integer;
                      var Old: Integer; New: Integer );
  begin { WriteNum }
   MovePencil(PromptL,C);
   if New = 0 then Write('     ')
   else
    begin Write(Letter, New:1);
     if New <= 999 then Write(' ');
     if New <= 99 then Write(' ');
     if New <= 9 then Write(' ')
    end;
   Old := New
  end { WriteNum };
 
 
 begin { Prompt }
  SReadCursor(X,Y);
  if S <> '' then
   begin ClearLine(PromptL,FirstColumn);
    Write(Direction,S);
    OldRepeatCount := 0;
    OldCount := 0;
{$IFC not KeySelection THEN}
    NeedPrompt := true;
    OldVerify := False;
{$ENDC}
    OldAtColumn := -1;
    OldTime.Second := (OldTime.Second + 59) mod 60   { OldTime.Second-1 }
   end;
  if RepeatCount <> OldRepeatCount then
   WriteNum('R', RepeatC, OldRepeatCount, RepeatCount);
  if Count <> OldCount then
   WriteNum('C', CountC, OldCount, Count);
  if Verify <> OldVerify then
   begin MovePencil(PromptL,VerifyC);
    if Verify then Write('V')
    else Write(' ');
   end;
{$IFC KeySelection THEN}
  MovePencil(PromptL,MoreingC);
  if Moreing then Write('M')
  else Write(' ');
{$ENDC}
  if AtColumn <> OldAtColumn then
   begin MovePencil(PromptL,AtC);
    if AtColumn >= 0 then
     begin Write('@', AtColumn+1:1);
      if AtColumn < 9 then Write(' ')
     end
    else Write('   ');
    OldAtColumn := AtColumn
   end;
  GetTStamp(Time);
  if Time.Second <> OldTime.Second then
   begin StampToString(Time,TimeStr);
    MovePencil(PromptL,TimeC);
    Write(TimeStr);
    OldTime := Time
   end;
  SSetCursor(X,Y);
  Y := HomeY+PromptL*LineHeight+CharHeight+1;
  RasterOp(RXNor, WinWidth-XMargin-XMargin, 1,
               WinOrgX+XMargin, Y, SScreenW, Scrn,
               WinOrgX+XMargin, Y, SScreenW, Scrn);
 end { Prompt };
 
 
 procedure Status(S: String);
 var X, Y: Integer;
 begin { Status }
  if DEBUG2 then
   begin SReadCursor(X,Y);
    ClearLine(PromptL,FirstColumn);
    Write(S);
    OldRepeatCount := 0;
    OldCount := 0;
    OldVerify := False;
{$IFC KeySelection THEN}
    OldMoreing := False;
{$ENDC}
    OldAtColumn := -1;
    OldTime.Second := (OldTime.Second + 59) mod 60;  { OldTime.Second-1 }
    Prompt('');   { print R C V }
    NextChar;
    NeedPrompt := true;
    SSetCursor(X,Y)
   end
 end { Status };
 
 
 procedure Error(S: String);
 var X, Y: Integer;
 begin { Error }
  SReadCursor(X,Y);
  ClearLine(PromptL,FirstColumn);
  Write(BEL, '*** ', S, ', <space> to continue ***');
  OldRepeatCount := 0;
  OldCount := 0;
  OldVerify := False;
{$IFC KeySelection THEN}
    OldMoreing := False;
{$ENDC}
  OldAtColumn := -1;
  OldTime.Second := (OldTime.Second + 59) mod 60;  { OldTime.Second-1 }
  Prompt('');   { print R C V }
  repeat NextChar until Ch = ' ';
  ClearLine(PromptL,FirstColumn);
  NeedPrompt := true;
  SSetCursor(X,Y)
 end { Error };
 
 
 procedure Warn(S: String);
 var X, Y: Integer;
 begin { Warn }
  SReadCursor(X,Y);
  ClearLine(PromptL,FirstColumn);
  Write('*** ', S, ' ***');
  OldRepeatCount := 0;
  OldCount := 0;
  OldVerify := False;
{$IFC KeySelection THEN}
    OldMoreing := False;
{$ENDC}
  OldAtColumn := -1;
  OldTime.Second := (OldTime.Second + 59) mod 60;  { OldTime.Second-1 }
  Prompt('');   { print R C V }
  NeedPrompt := false;
  SSetCursor(X,Y)
 end { Warn };
 
 
 function CleanPage: MemPage;
   handler PartFull(PName: string);
   begin
      MovePencil(40, 5);
      writeln ('                                                         ');
      writeln ('  Swapping partition "', PName, '" is full               ');
      writeln (BEL,'                                                       ');
      writeln ('  You will have to delete some files from that partition   ');
      writeln ('  and rerun the edit using the /REPLAY switch.            ');
      writeln ('                                                         ');;
      exit(Editor);
   end;
 var M, Oldest: MemPage;
     OldAge: Integer;
     UnReserved: set of MemPage;
 begin { CleanPage }
  Status('Enter CleanPage');
  M := LastMemPage;
  Oldest := M;
  OldAge := Txt[M].Age;
  UnReserved := [0..MaxMemPage] -
                [DrawCursor.ChPage, Cursor1.ChPage, Cursor2.ChPage];
  repeat
   if M = MaxMemPage then M := 0 else M := M + 1;
   if M in UnReserved then
    with Txt[M] do
     begin
      if Age >= OldAge then
       if Age > OldAge then
        begin Oldest := M; OldAge := Txt[M].Age end
       else Oldest := M;
      if Age < MaxMemPage then Age := Age + 1
     end
  until M = LastMemPage;
  with Txt[Oldest] do
   if Dirty then
    begin
     if DEBUG2 then
      begin ClearLine(PromptL,FirstColumn);
       Write('Swap out page ', TPage mod (MaxDiskBlock+1):1,
             ' to file ', TPage div (MaxDiskBlock+1):1);
       NextChar;
       NeedPrompt := true;
       MovePencil(ThisLine,ThisColumn)
      end;
     FSBlkWrite(IdFile[TPage div (MaxDiskBlock+1)],
                TPage mod (MaxDiskBlock+1),
                Recast(Buffer,PDirBlk));
     Dirty := False
    end;
  CleanPage := Oldest;
  Status('Exit CleanPage')
 end { CleanPage };
 
 
 function Mem(D: DiskPage): MemPage;
 var State: (Scanning, NotFound, Found);
     M: MemPage;
 begin { Mem }
  Status('Enter Mem');
  State := Scanning;
  M := LastMemPage;
  repeat
   if M = MaxMemPage then M := 0 else M := M + 1;
   if D = Txt[M].TPage then
    begin State := Found; Txt[M].Age := 0 end
   else
    if M = LastMemPage then State := NotFound
  until State <> Scanning;
  if State = NotFound then
   begin M := CleanPage;
    with Txt[M] do
     begin
      TPage := D;
      Age := 0;
      if DEBUG2 then
       begin ClearLine(PromptL,FirstColumn);
        Write('Swap in page ', TPage mod (MaxDiskBlock+1):1,
              ' from file ', TPage div (MaxDiskBlock+1):1);
        NextChar;
        NeedPrompt := true;
        MovePencil(ThisLine,ThisColumn)
       end;
      FSBlkRead(IdFile[D div (MaxDiskBlock+1)],
                D mod (MaxDiskBlock+1),
                Recast(Buffer,PDirBlk))
     end
   end;
  LastMemPage := M;
  Mem := M;
  Status('Exit Mem')
 end { Mem };
 
 
 procedure CreateEmptyPage;
 var P: pChunk;
     M: MemPage;
 begin { CreateEmptyPage }
  Status('Enter CreateEmptyPage');
  NewChunk(P);
  with P^ do
   begin CPage := Pages;
    Pages := Pages + 1;
    First := 0;
    Length := MaxLength;
    OrderP := 0;
    OrderC := 0;
    Next := nil;
    Prev := EmptyLast.Chunk
   end;
  EmptyLast.Chunk^.Next := P;
  EmptyLast.Chunk := P;
  M := CleanPage;
  with Txt[M] do
   begin TPage := P^.CPage;
    Age := 0
   end;
  Status('Exit CreateEmptyPage')
 end { CreateEmptyPage };
 
 
 procedure Add1(var P: Position);
 begin { Add1 }
  if P.Offset + 1 = P.Chunk^.Length then
   begin P.Chunk := P.Chunk^.Next;
    P.Offset := 0
   end
  else P.Offset := P.Offset + 1
 end { Add1 };
 
 
 procedure Sub1(var P: Position);
 begin { Add1 }
  if P.Offset = 0 then
   begin P.Chunk := P.Chunk^.Prev;
    P.Offset := P.Chunk^.Length - 1
   end
  else P.Offset := P.Offset - 1
 end { Sub1 };
 
 
 procedure Add(P: Position; N: integer; var Q: Position);
 var Offset, Length: integer;
 begin { Add }
  Q.Chunk := P.Chunk;
  Offset := P.Offset + N;
  if Offset < 0 then
   begin
    repeat Q.Chunk := Q.Chunk^.Prev;
     if Q.Chunk = nil then Offset := 0
     else Offset := Offset + Q.Chunk^.Length
    until Offset >= 0;
    if Q.Chunk = nil then Q := FilledFirst else Q.Offset := Offset
   end
  else
   if Offset >= Q.Chunk^.Length then
    begin Length := Q.Chunk^.Length;
     repeat Offset := Offset - Length;
      Q.Chunk := Q.Chunk^.Next;
      if Q.Chunk = nil then Length := Offset + 1
      else Length := Q.Chunk^.Length
     until Offset < Length;
     if Q.Chunk = nil then Q := FilledLast else Q.Offset := Offset
    end
   else Q.Offset := Offset
 end { Add };
 
 
 procedure Detach(var C: Cursor);
 begin { Detach }
  with C do
   if Attached then Attached := false
   else Error('internal error: attempt to Detach an unattached cursor')
 end { Detach };
 
 
 procedure Attach(var C: Cursor; P: Position; RW: ReadWrite);
 begin { Attach }
  with C, Pos do
   begin
    if Attached then
     Error('internal error: attempt to Attach an attached cursor');
    Pos := P;
    ChPage := Mem(Chunk^.CPage);
    ChOffset := Chunk^.First + Offset;
    Attached := true;
    Writing := RW = WriteCursor;
    if Writing then Txt[ChPage].Dirty := true;
    Ch := Txt[ChPage].Buffer^[ChOffset]
   end
 end { Attach };
 
 
 procedure ReAttach(var C: Cursor; P: Position);
 begin { ReAttach }
  with C, Pos do
   if Chunk^.CPage = P.Chunk^.CPage then
    begin Pos := P;
     ChOffset := Chunk^.First + Offset;
     Ch := Txt[ChPage].Buffer^[ChOffset]
    end
   else
    begin Detach(C);
     if Writing then Attach(C,P,WriteCursor)
     else Attach(C,P,ReadCursor)
    end
 end { ReAttach };
 
 
 procedure Add1C(var C: Cursor);
 begin { Add1C }
  with C, Pos do
   begin
    if Writing then Txt[ChPage].Buffer^[ChOffset] := Ch;
    if Offset + 1 < Chunk^.Length then
     begin Offset := Offset + 1;
      ChOffset := ChOffset + 1
     end
    else
     if Chunk^.Next <> nil then
      begin Chunk := Chunk^.Next;
       Offset := 0;
       ChPage := Mem(Chunk^.CPage);
       ChOffset := Chunk^.First;
       if Writing then Txt[ChPage].Dirty := true
      end;
    Ch := Txt[ChPage].Buffer^[ChOffset]
   end
 end { Add1C };
 
 
 procedure Sub1C(var C: Cursor);
 begin { Sub1C }
  with C, Pos do
   begin
    if Writing then Txt[ChPage].Buffer^[ChOffset] := Ch;
    if Offset > 0 then
     begin Offset := Offset - 1;
      ChOffset := ChOffset - 1
     end
    else
     if Chunk^.Prev <> nil then
      begin Chunk := Chunk^.Prev;
       Offset := Chunk^.Length - 1;
       ChPage := Mem(Chunk^.CPage);
       ChOffset := Chunk^.First + Offset;
       if Writing then Txt[ChPage].Dirty := true
      end;
    Ch := Txt[ChPage].Buffer^[ChOffset]
   end
 end { Sub1C };
 
 
 procedure AddC(var C: Cursor; N: integer);
 var NewOffset, Length: integer;
 begin { AddC }
  with C, Pos do
   begin
    if Writing then Txt[ChPage].Buffer^[ChOffset] := Ch;
    NewOffset := Offset + N;
    if NewOffset < 0 then
     begin
      repeat Chunk := Chunk^.Prev;
       if Chunk = nil then NewOffset := 0
       else NewOffset := NewOffset + Chunk^.Length
      until NewOffset >= 0;
      if Chunk = nil then Pos := FilledFirst else Offset := NewOffset;
      ChPage := Mem(Chunk^.CPage);
      ChOffset := Chunk^.First + NewOffset;
      if Writing then Txt[ChPage].Dirty := true
     end
    else
     if NewOffset >= Chunk^.Length then
      begin Length := Chunk^.Length;
       repeat NewOffset := NewOffset - Length;
        Chunk := Chunk^.Next;
        if Chunk = nil then NewOffset := 0
        else Length := Chunk^.Length
       until NewOffset < Length;
       if Chunk = nil then Pos := FilledLast else Offset := NewOffset;
       ChPage := Mem(Chunk^.CPage);
       ChOffset := Chunk^.First + NewOffset;
       if Writing then Txt[ChPage].Dirty := true
      end
    else
     begin Offset := NewOffset;
      ChOffset := ChOffset + N
     end;
    Ch := Txt[ChPage].Buffer^[ChOffset]
   end
 end { AddC };
 
 
 function Subtract(P1, P2: Position): integer;
    { can be used only if the distance from P1 to P2 is < 32768 }
 var N: integer;
 begin { Subtract }
  N := -P2.Offset;
  while (P2.Chunk <> P1.Chunk) and (P2.Chunk <> nil) do
   begin N := N + P2.Chunk^.Length;
    if N > 0 then P2.Chunk := P2.Chunk^.Next
    else P2.Chunk := nil
   end;
  if P2.Chunk = nil then N := -1
  else N := N + P1.Offset;
  Subtract := N
 end { Subtract };
 
 
 function LT(P1, P2: Position): boolean;
 begin { LT }
  if P1.Chunk = P2.Chunk then LT := P1.Offset < P2.Offset
  else
   if P1.Chunk^.OrderP = P2.Chunk^.OrderP then
    LT := P1.Chunk^.OrderC < P2.Chunk^.OrderC
   else LT := P1.Chunk^.OrderP < P2.Chunk^.OrderP
 end { LT };
 
 
 function LE(P1, P2: Position): boolean;
 begin { LE }
  if P1.Chunk = P2.Chunk then LE := P1.Offset <= P2.Offset
  else
   if P1.Chunk^.OrderP = P2.Chunk^.OrderP then
    LE := P1.Chunk^.OrderC <= P2.Chunk^.OrderC
   else LE := P1.Chunk^.OrderP < P2.Chunk^.OrderP
 end { LE };
 
 
 function EQ(P1, P2: position): boolean;
 begin { EQ }
  EQ := (P1.Chunk = P2.Chunk) and (P1.Offset = P2.Offset)
 end { EQ };
 
 
 function NE(P1, P2: position): boolean;
 begin { NE }
  NE := (P1.Chunk <> P2.Chunk) or (P1.Offset <> P2.Offset)
 end { NE };
 
 
 function GT(P1, P2: Position): boolean;
 begin { GT }
  if P1.Chunk = P2.Chunk then GT := P1.Offset > P2.Offset
  else
   if P1.Chunk^.OrderP = P2.Chunk^.OrderP then
    GT := P1.Chunk^.OrderC > P2.Chunk^.OrderC
   else GT := P1.Chunk^.OrderP > P2.Chunk^.OrderP
 end { GT };
 
    
 function GE(P1, P2: Position): boolean;
 begin { GE }
  if P1.Chunk = P2.Chunk then GE := P1.Offset >= P2.Offset
  else
   if P1.Chunk^.OrderP = P2.Chunk^.OrderP then
    GE := P1.Chunk^.OrderC >= P2.Chunk^.OrderC
   else Ge := P1.Chunk^.OrderP > P2.Chunk^.OrderP
 end { GE };
 
 
 function Bot(P: Position): boolean;
 begin { Bot }
  if (P.Offset <> 0) or (P.Chunk^.Prev <> nil) then Sub1(P);
  if (P.Offset <> 0) or (P.Chunk^.Prev <> nil) then Sub1(P);
  Bot := EQ(P,FilledFirst)
 end { Bot };
 
 
 function Eot(P: Position): boolean;
 begin { Eot }
  Eot := EQ(P,FilledLast)
 end { Eot };
 
    
 procedure TextPos(L: LineIndex; C: ColumnIndex; var P: Position);
 begin { TextPos }
  Status('Enter TextPos');
  if Ln[L].Length = 0 then { off screen } P := ScreenLast
  else
   with Ln[L] do
    if C >= Length then Add(Start,Length - 1,P)
    else Add(Start,C,P);
  Status('Exit TextPos')
 end { TextPos };
 
 
 procedure ScreenPos(P: Position; var L: LineIndex; var C: ColumnIndex);
 begin { ScreenPos }
  Status('Enter ScreenPos');
  L := -1;
  repeat L := L + 1
  until LE(P,Ln[L].Finish) or (L = LastLine);
  if GE(P,Ln[L].Start) then
   if LE(P,Ln[L].Finish) then C := Subtract(P,Ln[L].Start)
   else { OffScreen }
    begin L := LastLine + 1; C := 0 end
  else { OffScreen }
   begin L := -1; C := 0 end;
  Status('Exit ScreenPos')
 end { ScreenPos };
 
 
 procedure DrawThumbBar;
 var i: integer;
     Y: integer;
 
 
  function Dot( P: Position ): integer;
  var C, Col: integer;
  begin { Dot }
   C := P.Chunk^.OrderC + P.Offset;
   Col := (P.Chunk^.OrderP + C div MaxLength) div PagesPerDot;
   if Col>LastColumn then 
       Dot := LastColumn
   else 
       Dot := Col;
  end { Dot };
  
 
 begin { DrawThumbBar }
  MovePencil(ThumbL,EofDot); Write(' ');
  MovePencil(ThumbL,SelectDot); Write(' ');
  MovePencil(ThumbL,ScreenFDot); Write(' ');
  MovePencil(ThumbL,ScreenLDot); Write(' ');
{$IFC KeySelection Then}
  MovePencil(ThumbL,OldDot);  Write(' ');
  MovePencil(ThumbL,NoteDot);  Write(' ');
{$ENDC}
  Y := ThumbL * LineHeight + HomeY + CharHeight div 2;
  Line(DrawLine,
        HomeX - CharWidth div 2, Y,
        LastColumn * CharWidth + HomeX + CharWidth, Y,
       SScreenP);
  PagesPerDot := (FilledLast.Chunk^.OrderP + DotsAcross) div
                 DotsAcross;
  if PagesPerDot = 0 then PagesPerDot := 1;
  EofDot := Dot(FilledLast) + 1;
  ScreenFDot := Dot(ScreenFirst);
  ScreenLDot := Dot(ScreenLast);
  SelectDot := Dot(SelectFirst);
{$IFC KeySelection Then}
  OldDot    := Dot(OldFirst);
  NoteDot   := Dot(NoteFirst);
  MovePencil(ThumbL,OldDot);  Write('O');   
  MovePencil(ThumbL,NoteDot);  Write('N');   
{$ENDC}
  MovePencil(ThumbL,EofDot); Write(EotMarker);
  MovePencil(ThumbL,SelectDot); Write('S');
  MovePencil(ThumbL,ScreenFDot); Write('(');
  MovePencil(ThumbL,ScreenLDot); Write(')');
 end { DrawThumbBar };

 
 procedure DrawScrollBar;
 var X: integer;
 begin { DrawScrollBar }
  X := HomeX - CharWidth div 2;
  Line (DrawLine,
         X, ThumbL * LineHeight + HomeY + CharHeight div 2,
         X, LastLine * LineHeight + HomeY + CharHeight,
        SScreenP);
 end { DrawScrollBar };
   
 
 procedure DrawLn(L: LineIndex);
 
 
  procedure D( LineCh: char; P, Q: Position );
  var R, S, T: Position;
      i: integer;
      LastCh: Char;
  
  
   procedure WriteCh;
   var Ch: char;
   begin { WriteCh }
    Ch := DrawCursor.Ch;
    if LastCh = CR then
     if Ch = LF then Write(EolMarker)
     else
      begin WriteChar(CR);
       if Ch <> CR then
        if Ch = ' ' then WriteChar(BlankMark)
        else WriteChar(Ch)
      end
    else
     if Ch <> CR then
      if Ch = ' ' then WriteChar(BlankMark)
      else WriteChar(Ch);
    LastCh := Ch
   end { WriteCh };
   
    
  begin { D }
   ClearLine(L,GatherC - 2);
   WriteChar(LineCh);
   WriteChar('{');
   if NE(P,FilledLast) then
    begin 
     if EQ(Q,FilledLast) then Sub1(Q);
     R := P;
     i := LastColumn div 2 - 10;
     while (i > 0) and NE(R,Q) do
      begin i := i - 1; Add1(R) end;
     T := Q;
     i := LastColumn div 2 - 10;
     while (i > 0) and NE(T,P) do
      begin i := i - 1; Sub1(T) end;
     S := T;
     i := 10;
     while (i > 0) and NE(S,P) do
      begin i := i - 1; Sub1(S) end;
     Attach(DrawCursor,P,ReadCursor);
     LastCh := ' ';
     while NE(DrawCursor.Pos,R) and NE(DrawCursor.Pos,S) do
      begin WriteCh; Add1C(DrawCursor) end;
     if NE(DrawCursor.Pos,S) then
      begin Write(' ',DotDotDot,DotDotDot,' ');
       ReAttach(DrawCursor,T);
       Sub1C(DrawCursor);
       LastCh := DrawCursor.Ch;
       Add1C(DrawCursor)
      end;
     while NE(DrawCursor.Pos,Q) do
      begin WriteCh; Add1C(DrawCursor) end;
     WriteCh;
     if LastCh = CR then WriteChar(CR);
     Detach(DrawCursor)
    end;
   WriteChar('}')
  end { D };
 
 
 begin { DrawLn }
  case L of
   InsertL: D('I', InsertFirst,  InsertLast);
   DeleteL: D('D', DeleteFirst,  DeleteLast);
   FindL:   D('F', FindFirst,    FindLast);
   ReplacL: D('R', ReplaceFirst, ReplaceLast);
   SelectL: D('S', SelectFirst,  SelectLast)
   end
 end { DrawLn };
   
   
 procedure DrawUnderLine( First,Last: Position; L: LineIndex; C: ColumnIndex;
                          U: UnderKind );
    { underline from First to Last starting at screen coordinates L,C }
 var FirstC: ColumnIndex;
     XC, YC, Width: integer;
     Func1, Func2: integer; 
     P: Position;
     Done: Boolean;
 begin { DrawUnderLine }
  FirstC := C;
  XC := C * CharWidth + HomeX;
  YC := L * LineHeight + CharHeight + 1 + HomeY;
  if U = White then
   begin Func1 := RXor; Func2 := RXor end
  else
   begin Func1 := RXnor;
    if U = Black then Func2 := RXor
    else Func2 := RXnor
   end;

  Done := False;
  repeat
      P := Ln[L].Finish;   {set P to nominal end}
      {find the end of underline on current line}
         if LE(Last, P) then begin
            Done := True;
            ScreenPos(Last, L, C);
            P := Last;     {adjust P to actual end}
         end
         else 
            C := Subtract(P, Ln[L].Start);
         {increment count if not end in CRLF}
         Attach(DrawCursor, P, ReadCursor);
         if DrawCursor.Ch<>LF then 
            C := C+1
         else begin
            Sub1C(DrawCursor);
            if DrawCursor.Ch<>CR then C := C+1;
         end;
         Detach(DrawCursor);
      {draw the line}
      Width := (C - FirstC) * CharWidth;
      if U = ExtraBlack then
       begin XC := XC + 1; Width := Width - 2 end;
      RasterOp(Func1, Width, 1,
               XC - 1, YC, SScreenW, Scrn,
               XC - 1, YC, SScreenW, Scrn);
      YC := YC + 1;
      RasterOp(Func2, Width, 1,
               XC - 1, YC, SScreenW, Scrn,
               XC - 1, YC, SScreenW, Scrn);
      if L = LastLine then 
          Done := True 
      else begin
          L := L + 1;
          FirstC := 0;
          XC := HomeX;
          YC := YC + LineHeight - 1;
      end;
  until Done;
 end { DrawUnderLine };
 
 
 procedure Draw(P, Q: Position; L: LineIndex; C: ColumnIndex);
    { draw from P to Eot, set Ln, and set ScreenLast if possible }
 var Done, NewLine, SelectOnScreen, ChangeThumbBar: boolean;
     Ch: char;
     SL, SelFirst, SelLast: Position;
     SelL: LineIndex;
     SelC: ColumnIndex;
     FontP: FontPtr;
     Termination: (CharCount, ScreenWidth, ControlChar);
     X, Y: Integer;
     NewChOffset, AdvanceC, Max, TMax: Integer;
     Redrawing: Boolean;
     CharP: pSwapBuffer;
     FirstChar, LastCharP1: Integer;
     
     DrawBLoc: integer;
     DrawBArgs: packed record case Boolean of
                   true: (i: integer);
                   false: (func: 0..7; width: 0..#17777);
                end;
  
  procedure Advance( N: Integer );
  begin { Advance }
   if EQ(DrawCursor.Pos,Q) then Done := true
   else
    begin AddC(DrawCursor,N);
     if DrawCursor.Pos.Chunk = nil then Done := true
    end
  end { Advance };
  
  
 begin { Draw }
  Status('Enter Draw');
  FontP := GetFont;
  ChangeThumbBar := false;
  if C <= 0 then
   begin Ln[L].Start := P;
    if L = 0 then
     begin ScreenFirst := P;
      ChangeThumbBar := true
     end
   end;
  if C < 0 then
   begin
    Redrawing := False;
    if L = 0 then ClearScreen(-1,FirstColumn)
    else ClearScreen(L,FirstColumn);
    C := 0;
    MovePencil(L,0)
   end
  else
   begin
    Redrawing := True;
    ClearLine(L,C)
   end;
  if (L = 0) and (C = 0) then
   begin MovePencil(L,-2);
    if Bot(P) then SPutChr(BotMarker) else WriteChar(' ');
    MovePencil(0,0)
   end;
  NewLine := false;
  Done := false;
  Attach(DrawCursor,P,ReadCursor);
  if LT(DrawCursor.Pos,SelectFirst) then SelectOnScreen := false
  else
   if GT(DrawCursor.Pos,SelectLast) then SelectOnScreen := false
   else
    begin SelFirst := DrawCursor.Pos;
     SelL := L;
     SelC := C;
     SelectOnScreen := true
    end;
  repeat
   if EQ(DrawCursor.Pos,Q) then Done := true;
   Ch := DrawCursor.Ch;
   if EQ(DrawCursor.Pos,SelectFirst) then
    begin SelFirst := DrawCursor.Pos;
     SelL := L;
     SelC := C;
     SelectOnScreen := true
    end;
   if Done or (Chr(Land(Ord(Ch), #177)) < ' ') then { draw a single character }
    begin
     SL := DrawCursor.Pos;
     if Ch = CR then
      begin
       Advance(1);
       if not Done and (DrawCursor.Ch = LF) then
        begin SL := DrawCursor.Pos;
         WriteChar(' ');
         Advance(1);
         NewLine := true
        end
       else WriteChar(Ch)
      end
     else
      begin
       if EQ(DrawCursor.Pos,FilledLast) then SPutChr(EotMarker)
       else WriteChar(Ch);
       Advance(1)
      end;
     C := C + 1
    end
   else
    begin
     Max := DrawCursor.Pos.Chunk^.Length - DrawCursor.Pos.Offset;
     if DrawCursor.Pos.Chunk = Q.Chunk then { stop before end of text }
      begin TMax := Q.Offset - DrawCursor.Pos.Offset;
       if TMax < Max then Max := TMax
      end;
     if DrawCursor.Pos.Chunk = SelectFirst.Chunk then
      if SelectFirst.Offset > DrawCursor.Pos.Offset then { stop before select }
       begin
        TMax := SelectFirst.Offset - DrawCursor.Pos.Offset;
        if TMax < Max then Max := TMax
       end;
     if C + Max > NColumn then { stop at end of line }
      Max := NColumn - C;
     CharP := Txt[DrawCursor.ChPage].Buffer;
     FirstChar := DrawCursor.ChOffset;
     LastCharP1 := FirstChar + Max;
     SReadCursor(X, Y);
     {$R-}
     DrawBLoc := LOr( Shift(#4010,8), Shift(#4010,-8) );
     DrawBArgs.Func := RRpl;
     DrawBArgs.Width := SScreenW;
     LoadExpr(DrawBArgs.i);        { Raster-Op function and width }
     LoadExpr(X);                  { destination X }
     LoadExpr(Y);                  { destination Y }
     LoadExpr(ScreenSeg);          { destination base }
     LoadExpr(0);
     LoadAdr(FontP);
     InLineByte( 239 {LDDW} );
     LoadAdr( CharP^ );
     LoadExpr( FirstChar );
     LoadExpr( LastCharP1 );
     LoadExpr( RightLimit );
     LoadExpr( DrawBLoc );
     InLineByte( 240 {STLATE} );   { translate font and buffer addresses }
     InLineByte( #165 {7,,5} );
     InLineByte( 240 {STLATE} );   { translate screen address }
     InLineByte( #7 {0,,7} );
     {$R=}
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
     *****
     ***** Note that the E-Stack achieves its maximum depth.
     *****
          
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
     StorExpr(NewChOffset);
     StorExpr(Termination);
     AdvanceC := NewChOffset - DrawCursor.ChOffset;
     Add(DrawCursor.Pos, AdvanceC - 1, SL);
     Advance(AdvanceC);
     C := C + AdvanceC;
     MovePencil(L,C)
    end;
   if C = NColumn then NewLine := true;
   if NewLine or Done then
    begin NewLine := false;
     Ln[L].Finish := SL;
     Ln[L].Length := C;
     L := L + 1;
     C := 0;
     if L > LastLine then Done := true;
     if not Done then
      begin Ln[L].Start := DrawCursor.Pos;
       if Redrawing then ClearLine(L,0) else MovePencil(L,0)
      end
    end
  until Done;
  if (L > LastLine) or Eot(DrawCursor.Pos) then
   begin ScreenLast := SL;
    ChangeThumbBar := true;
    for L := L to LastLine do with Ln[L] do
     begin Start := FilledLast;
      Finish := FilledLast;
      Length := 1
     end
   end;
  Detach(DrawCursor);
  if SelectOnScreen then
   begin ChangeThumbBar := true;
    if LE(SelectLast,SL) then SelLast := SelectLast
    else SelLast := SL;
    DrawUnderLine(SelFirst,SelLast,SelL,SelC,Black)
   end;
  if ChangeThumbBar then DrawThumbBar;
  Status('Exit Draw')
 end { Draw };
 
 
 procedure UnderLine(First, Last: Position; U: UnderKind);
    { underline the portion of screen from First to Last }
 var L: LineIndex;
     C: ColumnIndex;
 begin { UnderLine }
  Status('Enter UnderLine');
  if LT(First,ScreenFirst) then First := ScreenFirst;
  if GT(Last,ScreenLast) then Last := ScreenLast;
  if LE(First,Last) then
   begin ScreenPos(First,L,C); DrawUnderLine(First,Last,L,C,U) end;
  Status('Exit UnderLine')
 end { UnderLine };
 
 
 function OnScreen(P: Position; L1, L2: LineIndex): boolean;
 var L: LineIndex;
     C: ColumnIndex;
 begin { OnScreen }
  Status('enter OnScreen');
  if (L1 > LastLine) or (L2 < 0) then OnScreen := False
  else
   begin
    ScreenPos(P, L, C);
    if L < L1 then OnScreen := Bot(ScreenFirst)
    else OnScreen := L <= L2
   end;
  if DEBUG1 then
   begin ClearLine(PromptL,FirstColumn);
    if (L1 <= LastLine) and (L2 >= 0) then
     Write('L=', L:1, ' C=', C:1, ' ');
    Write('L1=', L1:1, ' L2=', L2:1);
    if (L1 > LastLine) or (L2 < 0) then Write('  Off')
    else
     if L < L1 then
      if Bot(ScreenFirst) then Write('  On')
      else Write('  Off')
     else
      if L <= L2 then Write('  On')
      else Write('  Off');
    Write('Screen');
    NextChar;
    NeedPrompt := true;
    MovePencil(ThisLine,ThisColumn)
   end
 end { OnScreen };
 
(*
 function BackToLineStart(var P: position; N: integer; var L: integer;
                                    Lim: Position): Boolean;
 {--------------------------
 {Abstract:  Moves P back to a beginning of a line that is N screen lines
 {      back, or is a CRLF prior to Lim.  Each line is limited to 
 {      2240 characters.
 { Parameters:
 {      P - position from which to start search.  Is set to point to 
 {          text position that should be at beginning of a screen line.
 {      N - maximum number of screen lines to go back.
 {      L - number of screen lines actually gone back.
 {          If N is zero or one, this will be one.
 {          If Lim is in the k'th line before the initial P, with k<N,
 {          then L will be k.  (If they are in same line, K=1.)
 {      Lim - a limiting position.  Halt search at a CRLF or 2240
 {          that is here or earlier.  Often will be right after a CRLF.
 { Returns:
 {      True if Lim was encountered within N lines of P.
 {-------------------------}
 var 
    C, SLine: integer;   {C counts columns on line.  SLine counts screen lines}
    S: Position;
    GotIt: Boolean;
    Limit: integer;
 begin
  if BOT(P) then begin
    L := 0;
    BackToLineStart := BOT(Lim);
  end
  else begin
    SLine := N;
    Attach(DrawCursor,P,ReadCursor);
    if DrawCursor.Ch = LF then begin {count CRLF as one char}
       Sub1C(DrawCursor);
       if DrawCursor.Ch <> CR then Add1C(DrawCursor);
    end;
    Limit := N*NColumn; {each line is limited to this length}
    repeat C := 1;
     repeat
      repeat Sub1C(DrawCursor);
       C := C + 1;
      until (DrawCursor.Ch = LF) or (C>=Limit);
      Sub1C(DrawCursor);
      if DrawCursor.Ch <> CR then Add1C(DrawCursor);
     until (DrawCursor.Ch = CR) or (C>=Limit);
     SLine := SLine - (C+LastColumn) div NColumn;
     GotIt := LE(DrawCursor.Pos, Lim);
     if DrawCursor.Ch=CR then begin
         Add1C(DrawCursor);
         if DrawCursor.Ch = LF then 
             GotIt := LT(DrawCursor.Pos, Lim);
         Sub1C(DrawCursor);
     end;
    until (SLine <= 0) or GotIt;

    while SLine < 0 do begin
        AddC(DrawCursor, NColumn);
        SLine := SLine+1;
    end;
    P := DrawCursor.Pos;
    if DrawCursor.Ch = CR then begin
        Add1C(DrawCursor);
        if DrawCursor.Ch=LF then
            Add(DrawCursor.Pos, 1, P);      {go forward over CRLF}
    end;
    Detach(DrawCursor);
    BackToLineStart := LE(P, Lim);
    L := N - SLine;
  end;
 end; {BackToLineStart}
*)
 
 procedure Show(P: Position; L1, L2: LineIndex);
 var L, C: Integer;
     Limit: integer;
(*
     T, S: Position;
     Lx: integer;
     Done: Boolean;
(* *)
 begin { Show }
  Status('enter Show');
  if not OnScreen(P,L1,L2) then
   begin
(* *)
    Limit := NColumn*NLine - 1;
    L := LastLine div 2;
    C := 0;
    Attach(DrawCursor,P,ReadCursor);
    repeat C := 0;
     repeat
      repeat Sub1C(DrawCursor);
       C := C + 1
      until (DrawCursor.Ch = LF) or (C>=Limit); 
      Sub1C(DrawCursor);
      if DrawCursor.Ch <> CR then Add1C(DrawCursor)
     until (DrawCursor.Ch = CR) or (C>=Limit);
     L := L - (C + LastColumn) div NColumn
    until (L <= 0) or Bot(DrawCursor.Pos);
    Add(DrawCursor.Pos,2,P);  { skip CR LF }
    Detach(DrawCursor);
    while L < 0 do
     begin Add(P,NColumn,P);
      L := L + 1
     end;
    Draw(P,FilledLast,0,-1)
(* 
    Done := False;
    if LT(P, ScreenFirst) then begin
       Add(P, 5*NColumn, S);
       if LT(ScreenFirst, S) then begin {try to use ScrollDown}
           Add(ScreenFirst, -1, T);
           if BackToLineStart(T, 5, L, P) then begin
              for L := L downto 1 do
                 ScrollDown(0, LastLine, 1);
              Done := True;
           end;
       end;
    end
    else begin  {P is below screen.  Try first to use ScrollUp}
       Add(ScreenLast, 5*NColumn, S);
       if LT(P, S) then begin {try to use ScrollUp}
          T := P;
          Add(ScreenLast, 1, S);
          if BackToLineStart(T, 5, L, S) then begin
              for L := L downto 1 do
                 ScrollUp(0, LastLine, 1);
              Done := True;
          end;
       end;
    end;
    if not Done then begin
       {if get here, display the text by jumping to it}
       Add(FilledFirst, 2, S);
       if BackToLineStart(P, LastLine div 2, L, S) then {nothing};
       Draw(P,FilledLast,0,-1);
    end;
(* *)
   end;
  Status('exit Show');
 end { Show };
  
 
 procedure ScrollUp(L1, L2: LineIndex; Distance: integer);
 var i, j: integer;
     P: Position;
     XC, YC, Height: integer;
 begin { ScrollUp }
  Status('Enter ScrollUp');
  if L2 > L1 then
   begin
    if (L1 = 0) and Eot(ScreenLast) then
     while Eot(Ln[Distance-1].Finish) and (Distance > 0) do
      Distance := Distance - 1;
    if Distance > 0 then
     begin XC := HomeX;
      YC := L1 * LineHeight + HomeY;
      Height := (L2 - L1 - Distance + 1) * LineHeight;
      if Height > 0 then
       RasterOp(RRpl, RightLimit - HomeX, Height,
                XC - 1, YC - 1, SScreenW, Scrn,
                XC - 1, YC + Distance * LineHeight - 1, SScreenW, Scrn);
      YC := (L2 - Distance + 1) * LineHeight + HomeY;
      RasterOp(RXor, RightLimit - HomeX, Distance * LineHeight,
               XC - 1, YC - 1, SScreenW, Scrn,
               XC - 1, YC - 1, SScreenW, Scrn);
      for i := L1 to L2-Distance do Ln[i] := Ln[i+Distance];
      if (L2 = LastLine) and not Eot(ScreenLast) then
       begin Add(ScreenLast,1,P);
        Draw(P,FilledLast,LastLine-Distance+1,0)
       end
      else
       for i := L2 - Distance + 1 to L2 do
        with Ln[i] do
         begin Start := FilledLast;
          Finish := FilledLast;
          Length := 1
         end;
      if L1 = 0 then
       begin ScreenFirst := Ln[0].Start;
        if not Bot(ScreenFirst) then { get rid of the Bot marker }
         begin MovePencil(0,-2);
          SPutChr(' ')
         end;
        DrawThumbBar
       end
     end
   end;
  Status('Exit ScrollUp')
 end { ScrollUp };
 
 
 procedure ScrollDown(L1, L2: LineIndex; Distance: integer);
    {Move lines L1 through L2 down in display by Distance lines.
    Bring in enough lines to fill top of screen.}
 var i, j: integer;
     L: LineIndex;
     C: ColumnIndex;
     P, Q, S: Position;
     XC, YC, Height: integer;
     Limit: integer;
 begin { ScrollDown }
  Status('Enter ScrollDown');
  if L2 > L1 then
   begin
   
    {Set P and Q to beginning and end of text to move onto screen.  
    Change distance if encounter BOT.}
    if L1 = 0 then
     if Bot(ScreenFirst) then Distance := 0
     else
      begin 
       Add(ScreenFirst, -1, Q);
(*
       P := Q;
       Add(FilledFirst, 2, S);
       if BackToLineStart(P, Distance, j, S) then {nothing};
(* *)
       Limit := NColumn*NLine - 1;
       Attach(DrawCursor,Q,ReadCursor);
       if DrawCursor.Ch = LF then begin
          Sub1C(DrawCursor);
          if DrawCursor.Ch <> CR then Add1C(DrawCursor);
       end;
       j := 0;   {number of screen lines gone backward}
       {Find enough preceding CRLFs to fill the void}
       repeat i := 0;    {i is number of characters in current line}
        repeat
         repeat Sub1C(DrawCursor);
          i := i + 1
         until (DrawCursor.Ch = LF) or (i>Limit);
         Sub1C(DrawCursor);
         if DrawCursor.Ch <> CR then Add1C(DrawCursor)
        until (DrawCursor.Ch = CR) or (i>Limit);
        j := j + (i + LastColumn) div NColumn
       until (j >= Distance) or Bot(DrawCursor.Pos);
       Add(DrawCursor.Pos,2,P); { skip CR LF }
       Detach(DrawCursor);
       while j > Distance do
        begin Add(P,NColumn,P);
         i := i - NColumn;
         j := j - 1
        end;
(* *)
       Distance := j;
      end;
      
    {Do the moves.  First RasterOp down the existing text.}
    if Distance > 0 then
     begin XC := HomeX;
      YC := L1 * LineHeight + HomeY;
      Height := (L2 - L1 - Distance + 1) * LineHeight;
      if Height > 0 then
       RasterOp(RRpl, RightLimit - HomeX, Height,
                XC - 1, YC + Distance * LineHeight - 1, SScreenW, Scrn,
                XC - 1, YC - 1, SScreenW, Scrn);
      RasterOp(RXor, RightLimit - HomeX, Distance * LineHeight,
               XC - 1, YC - 1, SScreenW, Scrn,
               XC - 1, YC - 1, SScreenW, Scrn);
      for i := L2 downto L1+Distance do Ln[i] := Ln[i-Distance];
      if L2 = LastLine then
       begin ScreenLast := Ln[LastLine].Finish;
        DrawThumbBar
       end;
      if L1 = 0 then Draw(P,Q,0,0)
      else
       for i := L1 to L1+Distance-1 do
        with Ln[i] do
         begin Start := ScreenFirst;
          Finish := ScreenFirst;
          Length := 1
         end
     end
   end;
  Status('Exit ScrollDown')
 end { ScrollDown };
 
 
 procedure JoinScreen(P: Position; L: LineIndex; C: ColumnIndex);
 var Q: Position;
     UpperL, LowerL: LineIndex;
 begin { JoinScreen }
  Attach(DrawCursor,P,ReadCursor);
  if not Eot(DrawCursor.Pos) then
   repeat
    repeat Add1C(DrawCursor)
    until (DrawCursor.Ch = CR) or Eot(DrawCursor.Pos);
    if (DrawCursor.Ch = CR) and not Eot(DrawCursor.Pos) then
     begin Add1C(DrawCursor);
      if DrawCursor.Ch <> LF then Sub1C(DrawCursor)
     end
   until (DrawCursor.Ch = LF) or Eot(DrawCursor.Pos);
  Q := DrawCursor.Pos;
  Detach(DrawCursor);
  Draw(P,Q,L,C);
  if OnScreen(Q,0,LastLine-1) then
   begin ScreenPos(Q,UpperL,C);
    UpperL := UpperL + 1;
    LowerL := LastLine;
    if Eot(Q) then ClearScreen(UpperL,0)
    else
     begin Add1(Q);
      while (LowerL > 0) and NE(Ln[LowerL].Start,Q) do LowerL := LowerL - 1;
      if EQ(Ln[LowerL].Start,Q) and (LowerL>=UpperL) and not Eot(Q) then begin
       ScrollUp(UpperL,LastLine,LowerL-UpperL);
       if EQ(SelectFirst, Q) then
           UnderLine(SelectFirst, SelectLast, Black);
      end
      else
       Draw(Q,FilledLast,UpperL,-1)
     end
   end
 end { JoinScreen };
   
  
 procedure RefreshScreen;
 var Time: TimeStamp;
 begin { RefreshScreen }
  Status('enter RefreshScreen');
  CreateWindow(1,0,0,16*SScreenW,1024,'');
      {Need to do CreateWindow here because a window with a title line is
       established in EditorT.}
  DrawLn(InsertL);
  DrawLn(DeleteL);
  DrawLn(FindL);
  DrawLn(ReplacL);
  DrawLn(SelectL);
  Draw(ScreenFirst,FilledLast,0,-1);
  NeedPrompt := true;
  OldRepeatCount := 0;
  OldCount := 0;
  OldVerify := False;
{$IFC KeySelection THEN}
  OldMoreing := True;
  Moreing := False;
  Direction := '>';
{$ENDC}
  OldAtColumn := -1;
  GetTStamp(Time);
  OldTime := Time;
  OldTime.Second := (OldTime.Second + 59) mod 60;  { OldTime.Second-1 }
  case CurrentPointer of
   TextP:  IOLoadCursor(TextPointer,xTextPointer,yTextPointer);
   UpP:    IOLoadCursor(UpPointer,xUpPointer,yUpPointer);
   DownP:  IOLoadCursor(DownPointer,xDownPointer,yDownPointer);
   LineP:  IOLoadCursor(LinePointer,xLinePointer,yLinePointer);
   ThumbP: IOLoadCursor(ThumbPointer,xThumbPointer,yThumbPointer)
   end;
  Status('exit RefreshScreen')
 end { RefreshScreen };
  
 
 procedure FixUp(OldC, NewC: pChunk; Adjust: integer);
 var L: LineIndex;
 
 
  procedure F( var P: Position );
  begin { F }
   with P do
    if Chunk = OldC then
     if Offset >= Adjust then begin 
        Chunk := NewC;
        Offset := Offset - Adjust
     end
     else if Adjust=1000 then begin {called from Collect}
        Chunk := FilledLast.Chunk;
        OffSet := FilledLast.Offset;
     end;
  end { F };
  
 begin { FixUp }
  F(EmptyFirst);   F(EmptyLast);
  F(FilledFirst);  F(FilledLast);
  F(ScreenFirst);  F(ScreenLast);
  F(SelectFirst);  F(SelectLast);
  F(InsertFirst);  F(InsertLast);
  F(DeleteFirst);  F(DeleteLast);
  F(FindFirst);    F(FindLast);
  F(ReplaceFirst); F(ReplaceLast);
  F(SourceFirst);  F(SourceLast);
  F(Display);
  F(LeftPart);     F(RightPart);
  F(PFirst);       F(PLast);
  F(Tmp);
  F(DrawCursor.Pos);
  F(Cursor1.Pos);
  F(Cursor2.Pos);
{$IFC KeySelection THEN}
  F(OldFirst);     F(NoteFirst);
{$ENDC}
  for L := 0 to LastLine do
   begin F(Ln[L].Start); F(Ln[L].Finish) end
 end { FixUp };
 
    
 
 procedure Split(var P: Position);
    { split text before P.  assume Sub1(P) has been saved if necessary }
 var OldC, NewC: pChunk;
     Adjust: integer;
 begin { Split }
  Status('Enter Split');
  with P do
   if Offset = 0 then
    begin
     if Chunk^.Prev <> nil then Chunk^.Prev^.Next := nil;
     Chunk^.Prev := nil
    end
   else
    begin OldC := Chunk;
     NewChunk(Chunk);
     NewC := Chunk;
     Adjust := Offset;
     with Chunk^ do
      begin Prev := nil;
       Next := OldC^.Next;
       if Next <> nil then Next^.Prev := Chunk;
       Length := OldC^.Length - Adjust;
       OrderC := OldC^.OrderC + Adjust;
       OrderP := OldC^.OrderP + OrderC div MaxLength;
       OrderC := OrderC mod MaxLength;
       CPage := OldC^.CPage;
       First := OldC^.First + Adjust
      end;
     with OldC^ do
      begin Length := Offset; Next := nil end;
     Offset := 0;
     FixUp(OldC,NewC,Adjust)
    end;
  Status('Exit Split')
 end { Split };
 
 
 procedure Join(var P, Q: Position);
    { join P (left) to Q (right). assume P points to last character in chunk
       and Q to first character in chunk }
 var PC, QC: pChunk;
     Adjust: integer;
 begin { Join }
  Status('Enter Join');
  PC := P.Chunk;
  QC := Q.Chunk;
  if (PC^.CPage = QC^.CPage) and (PC^.First + PC^.Length = QC^.First) then
   begin Adjust := PC^.Length;
    Q.Offset := Adjust;
    PC^.Next := QC^.Next;
    if PC^.Next <> nil then PC^.Next^.Prev := PC;
    PC^.Length := PC^.Length + QC^.Length;
    { Dispose(Q.Chunk); }
    Q.Chunk := PC;
    FixUp(QC,PC,-Adjust)
   end
  else
   begin PC^.Next := Q.Chunk;
    QC^.Prev := P.Chunk
   end;
  ReOrder(P);
  Status('Exit Join')
 end { Join };
 
 
 procedure Collect(P, Q: Position);
 var R: Position;
     T, U: pChunk;
 begin { Collect }
  (* don't re-use characters
  if NE(P,FilledLast) then
   begin Join(Q,EmptyFirst);
    EmptyFirst := P;
    P := FilledLast;
    Q := FilledLast
   end
  *)
  if NE(P,FilledLast) then
   begin
    if P.Offset <> 0 then Split(P);
    if Q.Offset <> Q.Chunk^.Length - 1 then
     begin Add(Q,1,R);
      Split(R)
     end;
    T := P.Chunk;
    while T <> nil do
     begin U := T;
      T := T^.Next;
         FixUp(U,FilledLast.Chunk,1000);
      Dispose(U)
     end
   end
 end { Collect };
 
 
 procedure Copy(First, Last: Position);
 var Done: boolean;
     Scan, T: pChunk;
 begin { Copy }
  (* don't actually copy the characters, just the pieces
  Attach(Cursor1,EmptyFirst,WriteCursor);
  Attach(Cursor2,First,ReadCursor);
  repeat Cursor1.Ch := Cursor2.Ch;
   if EQ(Cursor1.Pos,EmptyLast) then CreateEmptyPage;
   Add1C(Cursor1);
   Done := EQ(Cursor2.Pos,Last);
   Add1C(Cursor2)
  until Done;
  PFirst := EmptyFirst;
  Add(Cursor1.Pos,-1,PLast);
  EmptyFirst := Cursor1.Pos;
  Detach(Cursor1);
  Detach(Cursor2);
  Split(EmptyFirst)
  *)
  Scan := First.Chunk;
  PFirst.Chunk := nil;
  Done := false;
  repeat
   NewChunk(T);
   T^ := Scan^;
   T^.OrderP := 0;
   T^.OrderC := 0;
   if PFirst.Chunk = nil then PFirst.Chunk := T
   else
    begin PLast.Chunk^.Next := T;
     T^.Prev := PLast.Chunk
    end;
   PLast.Chunk := T;
   if Scan = Last.Chunk then Done := true
   else
    begin Scan := Scan^.Next;
     if Scan = nil then Done := true
    end
  until Done;
  PFirst.Chunk^.Prev := nil;
  PFirst.Chunk^.First := PFirst.Chunk^.First + First.Offset;
  PFirst.Chunk^.Length := PFirst.Chunk^.Length - First.Offset;
  PFirst.Offset := 0;
  if PFirst.Chunk = PLast.Chunk then
   PLast.Offset := Last.Offset - First.Offset
  else PLast.Offset := Last.Offset;
  PLast.Chunk^.Length := PLast.Offset + 1;
  PLast.Chunk^.Next := nil
 end { Copy };
 
 
 procedure ReOrder(Start: Position);
 var P: pChunk;
     OP, OC: integer;
 begin { ReOrder }
  with Start.Chunk^ do
   begin P := Next;
    OP := OrderP;
    OC := OrderC + Length
   end;
  while P <> nil do with P^ do begin 
    if OC >= MaxLength then begin
       OP := OP + 1;
       OC := OC - MaxLength;
       if OP > MaxDiskBlock then begin
          error ('file too big, crudely truncated');
          Next := NIL;
       end;
    end; 
    OrderP := OP;
    OrderC := OC;
    OC := OC + Length;
    P := Next
  end
 end { ReOrder };



 procedure NewChunk(var P: pChunk);
 label 1;
 

  handler FullSegment;
  begin { FullSegment }
   CreateSegment(ChunkSeg,2,2,10);
   Goto 1;
  end;


 begin { NewChunk }
1:New(ChunkSeg,1,P);
 end { NewChunk }
{$ifc ReplayVersion then}
                 ;


  procedure ExitTranscript;
  begin { ExitTranscript }
    Prompt(Concat('', '*** Replay finished, suggest Quit-Update ***'));
    if DemoSwitch then Editing := False;
  end { ExitTranscript };
  
  
  procedure NextTranscript;
  begin { NextTranscript }
   if Replay = NotReplaying then { shouldn't happen }
    begin
     TranscriptWord := 0;
     SendTranscript(0)
    end
   else
    begin
     if (TBlock >= TBlocks) or
        ((TBlock = TBlocks-1) and (TWord >= TWords)) then
      begin
       TranscriptWord := 0;
       Replay := NotReplaying;
       ExitTranscript
      end
     else
      begin
       if TWord = 0 then FSBlkRead(TId,TBlock,TBuffer);
       TranscriptWord := TBuffer^.Buffer[TWord];
       TWord := TWord + 1;
       if TWord >= 256 then
        begin
         TWord := 0;
         TBlock := TBlock + 1
        end
      end
    end
  end { NextTranscript };
  
  
  procedure SendTranscript(Word: Integer);
  begin { SendTranscript }
   if Replay = NotReplaying then
    begin
     TBuffer^.Buffer[TWord] := Word;
     TWord := TWord + 1;
     if TWord >= 256 then
      begin
       FlushTranscript;
       TWord := 0;
       TBlock := TBlock + 1
      end
    end
  end { SendTranscript };
  
  
  procedure FlushTranscript;
    handler PartFull(PName: String);
    begin
       Error('Transcript partition is full.  Quit-Update.');
       exit(FlushTranscript);
    end;
  begin { FlushTranscript }
   if Replay = NotReplaying then
    begin
     if TWord <> 0 then
      begin
       if (TBlock = 0) and (TId = 0) then
        begin
         TId := FSEnter('>Editor.Transcript');
         if TId = 0 then Error('Could not create Editor.Transcript file')
        end;
       if TId <> 0 then
        begin
         FSBlkWrite(TId,TBlock,TBuffer);
         FSClose(TId,TBlock+1,TWord*16)
        end
      end
    end
  end { FlushTranscript };


  procedure CheckReplay(Where: ReplayMode);
  var Status: Integer;
      Ch: Char;
      Done: Boolean;
   
   
   procedure ReplayPrompt;
   var ReplayPrompt: String;
   begin { ReplayPrompt }
    case Where of
     ReplayCharOrHit:
      if TranscriptWord = TabHit then
       ReplayPrompt := 'Replay:  About to press on tablet'
      else if TranscriptWord = SearchOK then
       ReplayPrompt := 'Replay:  About to do a successful search'
      else if TranscriptWord = SearchFail then
       ReplayPrompt := 'Replay:  About to skip a failing search'
      else
       begin
        ReplayPrompt := 'Replay: About to type ';
        if (Ord(RawCh) < #40) or (LAnd(Ord(RawCh),#200) <> 0) then
         begin
          if (RawCh = BS1) or (RawCh = BS2) or (RawCh = BS3) then
           AppendString(ReplayPrompt, 'BACK SPACE')
          else
           if (RawCh = BW1) or (RawCh = BW2) or (RawCh = BW3) then
            AppendString(ReplayPrompt, 'BACK WORD')
          else
           if (RawCh = BL1) or (RawCh = BL2) or (RawCh = BL3) then
            AppendString(ReplayPrompt, 'BACK LINE')
          else
           if (RawCh = Accept1) or (RawCh = Accept2) or (RawCh = Accept3) then
            AppendString(ReplayPrompt, 'INS')
          else
           if (RawCh = Reject1) or (RawCh = Reject2) or (RawCh = Reject3) then
            AppendString(ReplayPrompt, 'DEL')
          else
           if RawCh = CR then
            AppendString(ReplayPrompt, 'RETURN')
          else AppendString(ReplayPrompt,'control char')
         end
        else AppendChar(ReplayPrompt,RawCh)
       end;
     ReplayCR:
       ReplayPrompt := 'Replay:  About to type next line';
     ReplayCommand:
       begin
        ReplayPrompt := 'Replay:  About to execute ';
        if RawCh = Ins then AppendString(ReplayPrompt,'Ins')
        else
         if RawCh = Help then AppendString(ReplayPrompt, 'Help')
         else AppendChar(ReplayPrompt, RawCh);
        AppendString(ReplayPrompt, ' command')
       end;
     ReplayFree:
       begin ReplayPrompt := ''; NeedPrompt := True end
     end;
    if ReplayPrompt <> '' then Prompt(ReplayPrompt)
   end { ReplayPrompt };
   
   
   procedure ControlReplay;
   begin { ControlReplay }
    Done := True;
    if Ch = ' ' then Replay := ReplayCharOrHit
    else
     if Ch = CR then Replay := ReplayCR
     else
      if Ch = LF then Replay := ReplayCommand
      else
       if Ch = Ins then
        begin
         Replay := ReplayFree;
         Prompt('Replay:  SPACE, CR, LF, INS, DEL')
        end
       else
        if Ch = Del then
         begin
          Replay := NotReplaying;
          NeedPrompt := True;
          ExitTranscript
         end
        else if Ch=Bel then begin   {HelpKey hit}
           GiveHelp('>HelpDir>EditorHelp>Replay.Help');
           RefreshScreen;
           ReplayPrompt;
           Done := False;
        end
        else
         begin
          Write(Bel);
          Done := False
         end
   end { ControlReplay };
   
   
  begin { CheckReplay }
   Status := IOCRead(KeyBoard,Ch);
   if Status = IOEIOC then ControlReplay;
   if Where >= Replay then
    begin
     ReplayPrompt;
     repeat
      repeat Status := IOCRead(KeyBoard,Ch)
      until Status = IOEIOC;
      ControlReplay
     until Done
    end;
   if (Replay <> ReplayCharOrHit) and (Replay <> NotReplaying) then
    NeedPrompt := True
  end { CheckReplay }
{$endc}
                     .
