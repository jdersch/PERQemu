{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module IOVideo;
{---------------------------------------------------------------------------}
{
{  Abstract: Private Video type and variable declarations.
{            IOVideo exports variables, constants, and procedures that the IO
{            subsystem uses to do video manipulation
{
{  Copyright (c) 1982, 1983, Three Rivers Computer Corporation
{
{
{--------------------------------------------------------------------------}
{$Version V0.5 for POS }
{--------------------------------------------------------------------------}
{ Change Log
{
{ 23 May 83  V0.5 Brad Myers
{            Fix bug in TabMouse calculation for Kriz Tablet.
{
{ 28 Feb 83  V0.4 Brad Myers
{            Changed way GPIB and Kriz ShiftXAmts are set: Only doubled if
{            landscape and absolute.
{
{  9 Feb 83  V0.3 Brad Myers
{            Fixed for Landscape monitor.  Import Screen to get screen
{            parameters at initialization time.  Fixed for Document.
{
{ 27 Jan 83  V0.2 Roger Riggs
{            Fixed Bugs in switches from Kriz Tablet.
{            Removed StanleyTablet boolean.
{            Corrected initialization of TabMouse to be Ord('0')
{            Removed "The" from Pointer types.
{
{ 12 Jan 82  V0.1 August G. Reinig
{            Fixed bugs in TabletUpdate when the pointer is the Kriz tablet
{
{ 19 Oct 82  V0.0 August G. Reinig
{            Created the module.
{ 
{--------------------------------------------------------------------------}

exports

imports IO_others from IO_others; 
imports IO_Private from IO_Private; 

const
  TabIgnore = 2;       { number of points to ignore after when ignoring }

var
  Cursor: CurPatPtr;              { Cursor Pattern }  

  CursorX, CursorY: integer;      { new cursor coordinates }
  OldCurY,                        { previous Cursor Y position }
  OldCurX: integer;               { previous Cursor X position MOD 8 }

  PointX, PointY: integer;        { the point of the cursor }

  TabCount : integer;             { number of points left to ignore }   

  CursF: integer;                 { function currently in use}
  BotCursF: integer;              { function for area below used area}
  BotComplemented: boolean;       { whether bot is complemented or not}
  TabMode: TabletMode;            { Current mode of the tablet }
  CCursMode: CursMode;            { Current mode of cursor }
  newFunct: Boolean;              { Tells when have a new function to
                                          insure that cursor redisplayed }   
  
Procedure Vid_Initialize;         { Initialization for the video }
Procedure Vid_Interrupt;          { Interrupt Routine for the video device }
Function  Vid_SetUpUDevTab: pointer;  { Set up the pointers in the microcode }
                                  { device table that the micro needs }

private

Imports Screen from Screen;

Const
  debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} Imports System from System;
{$endc}

imports Memory from Memory;
imports IOpointdev from IOpointdev;
imports IOgpib from IOgpib;

Const
  DisCst0 = #1154;     { consts to terminate Display List; add in funct }
  DisCst1 = #1351;
  MinCurY = 0;           { Minimum Y value for cursor }
  MaxCurY = 1023;        { Maximum Y value for cursor }
  SegSize = 128;         { # lines / display segment }
  CrsHeight = 64;        { Height of the cursor }
  CrsConst0 = #370;      { constants to compute funny X position }
  VisOnly = #2000;       { mode bits - Visual screen only }
  VisAndCur = #2400;     { visual screen and cursor }
  Map = #100000;         { cursor map function }
  TabAverage = 2;        { we average 2^TabAverage number of data points }    

type
  DispPtr = ^DisplayFile;
  DisplayFile = array[0..11] of
    packed record case boolean of
      true: (int: integer);
      false:(LineCount  : 0..127;
             StartOver  : boolean;
             ShowCursor : boolean;
             VerticalRetrace : boolean;
             ShowScreen : boolean;
             DisableMicroInterrupt: boolean;
             WriteBadParity : boolean;
             Map: (CursOnly, CCursOnly, Compl,    ComplInv,
                   Normal,   Invert,    CursComp, InvCursCompl))
      end;
        
  ScrCtlPtr = ^ ScrCtlBlock;
  ScrCtlBlock = packed record
    Cmd: DispPtr;
    ScreenBase : Integer;
    CursorBase : Integer;
    Unused1    : Integer;
    Unused2    : Integer;
    CursX      : integer;
    filler     : integer
    end;

var 
  ScrBuf : ScrCtlPtr;              { Screen control blocks }
  DisFile0,DisFile1 : DispPtr;     { Screen Display lists - double bufrs }
  SumX : integer;                 { Sums of last 4 X and Y points }
  SumY : integer;
   
  

procedure InitTablet;
{----------------------------------------------------------------------------}
{
{ Abstract:
{       Initialize the tablet values that we set in Tablet Update. Default
{       for RealRel is false if portrait but true if landscape monitor.
{
{----------------------------------------------------------------------------}

begin

  TypePointDev  := NoPointDev; { at first, no pointer }
  RealRelTablet := false; 
  TabMode       := offTablet;    

  TabAbsX := 100;         { set up original tablet coordinates }
  TabAbsY := 100;
  TabRelX := 384;
  TabRelY := 512;   

  OldCurY := -1;          { Guarantee that Cursor Will be in a new position }
  OldCurX := 0;           { Initialize Cursor Pattern; at extreme left }

  TabFinger := false;     { consider the puck to be off the pad } 
  TabCount  := TabIgnore; { and ignore this many points }

  TabSwitch := false;     { no buttons are pressed }
  TabWhite  := False;
  TabGreen  := False;
  TabBlue   := False;
  TabYellow := False;
  TabMouse  := Ord('0');       
  
  TabLeft   := TabWhite;
  TabMiddle := TabYellow;
  TabRight  := TabGreen;
  
  GPIBTabBuf.Update := 0;    { prepare things for the GPIB }
  BitPadTimeOut := 1;    

end;

procedure InitCursor;   
{-----------------------------------------------------------------------------
{
{ Abstract:
{       InitCursor initializes the default cursor.  Assumes ScreenInit has
{       been called to figure out the parameters of the current screen.
{
{-----------------------------------------------------------------------------}

var 
  i,j:integer; 
  Pattern: CurPatPtr;

begin

  CCursMode := OffCursor;
  CursF := #100000;  {CTNormal}
  BotCursF := 0;
  BotComplemented := false;
  SBottomY := SMaxBitHeight - 1;
  SRightX := SBitWidth - 1;

  GPIBShiftXAmt := -1;
  KrizShiftXAmt := 0;
  
  newFunct := false;

  Cursor := MakePtr(CursorSeg,0,CurPatPtr);
  DefaultCursor := MakePtr(CursorSeg,256,CurPatPtr);

  for i := 0 to 63 do
    for j := 0 to 3 do
      DefaultCursor^[i,j] := 0;

  DefaultCursor^[ 0, 0] := #176000;
  DefaultCursor^[ 1, 0] := #170000;
  DefaultCursor^[ 2, 0] := #170000;
  DefaultCursor^[ 3, 0] := #174000;
  DefaultCursor^[ 4, 0] := #116000;
  DefaultCursor^[ 5, 0] := #107000;
  DefaultCursor^[ 6, 0] := #003400;
  DefaultCursor^[ 7, 0] := #001600;
  DefaultCursor^[ 8, 0] := #000700;
  DefaultCursor^[ 9, 0] := #000340;
  DefaultCursor^[10, 0] := #000160;
  DefaultCursor^[11, 0] := #000070;
  DefaultCursor^[12, 0] := #000034;
  DefaultCursor^[13, 0] := #000016;
  DefaultCursor^[14, 0] := #000007;
  DefaultCursor^[15, 0] := #000002;

  Cursor^ := DefaultCursor^;

  PointX := 0;
  PointY := 0

end { InitCursor };


procedure Vid_Initialize;
{----------------------------------------------------------------------------}
{
{  Abstract:
{       Initialize all the variables in IO_Others that we set, set up the
{       default cursor, enable Video interrupts.
{
{  WARNING: This must be called AFTER ScreenInit.
{
{---------------------------------------------------------------------------}


begin
  
  SetDDS( 371 );
  InitTablet;    SetDDS(372);
  InitCursor;    SetDDS(373);

  pUDevTab^[ScreenOut].EnableMask.Number := 0;  { enable screen interrupts }
  pUDevTab^[ScreenOut].EnableMask.Bits[Dev_ScreenUpdate] := true;
  
  loadexpr( pUDevTab^[ScreenOut].EnableMask.Number );
  loadexpr( ScreenOut );
  StartIO( EP_SetEnableMask )  

end;  

Procedure Vid_Interrupt;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Vid_Interrupt (formerly TabIntr) handles the screen retrace interrupt.
{       It smoothes the tablet data and updates the displayed cursor
{       position (if it is visible and has moved).
{-----------------------------------------------------------------------------}



procedure TabletUpdate;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       TabletUpdate smoothes the tablet data.
{
{-----------------------------------------------------------------------------}

const
  delta = ord('A') - ord('9') - 1;

var
  TX, TY: integer;
  Switches : integer;
  XFudge : integer;
  YFudge : integer;

begin { TabletUpdate }

  case TypePointDev of
    NoPointDev : exit( TabletUpdate );

    KrizTablet : 
      if land( KrizInfo^.Buttons, #200 ) <> 0 then begin { puck off tablet }
        TabFinger := false;                    { tell user }
        TabCount := TabIgnore;                 { and start ignoring data }
        exit( TabletUpdate )                   { That's all folks }
        end

      else if TabCount > 0 then begin          { ignoring a data points }
        TabCount := TabCount - 1;              { we've just ignored one }
         exit( TabletUpdate )
         end

      else begin                            { new point to work with }
        Switches := KrizInfo^.Buttons;      { make local copy of switches }
        TX := shift( KrizInfo^.XPos, KrizShiftXAmt ); { multiply by two if
                                                        landscape and absolute
                                                        since otherwise cannot
                                                        cover screen }
        TY := KrizInfo^.YPos;               { that of the GPIB bitpad }
        TabGreen  := land( Switches, 4 ) <> 0;  { right button }
        TabYellow := land( Switches, 2 ) <> 0;  { middle button }
        TabWhite  := land( Switches, 1 ) <> 0;  { left button }
        TabBlue   := false;  { we have only 3 buttons }
        TabSwitch := land( Switches, 7 ) <> 0; { any buttons at all }
 
        { provide actual output from the puck by changing Kriz's output }
        { to look like GPIB's solely to be compatible with GPIB }
        TabMouse := ord('0') + ord(TabYellow) + Shift(ord(TabWhite), 1) +
                    Shift(ord(TabGreen), 3);
        if TabMouse > ord('9') then TabMouse := TabMouse + delta;

        
        XFudge := KrizXfudge;
        YFudge := KrizYfudge
        end;

    GPIBBitPad :
      if GPIBTabBuf.Update > BitPadTimeOut then begin   { puck off pad }
        TabFinger := false;
        TabCount := TabIgnore;             { start ignoring points }
        GPIBTabBuf.Update := 0;            
        exit( TabletUpdate )
        end

      else if TabCount > 0 then begin      { ignoring data points }
        GPIBTabBuf.Update := GPIBTabBuf.Update + 1;
        TabCount := TabCount - 1;
        exit( TabletUpdate )
        end
        
      else begin 
        GPIBTabBuf.Update := GPIBTabBuf.Update + 1;
        TX := shift( GPIBTabBuf.X, GPIBShiftXAmt ); { map if not landscape }
        TY := shift( GPIBTabBuf.Y, -1 );     { map to more reasonable range }
        
        TabMouse := GPIBTabBuf.Buttons;
        if TabMouse > ord('9')
        then Switches := TabMouse - delta      { convert from hex }
        else Switches := TabMouse;
        TabYellow := land( Switches, 1 ) = 1;
        TabWhite  := land( Switches, 2 ) = 2;
        TabBlue   := land( Switches, 4 ) = 4;                                
        TabGreen  := land( Switches, 8 ) = 8;         
        TabSwitch := land( Switches, 15 ) <> 0;  { any buttons whatsoever }   
        
        XFudge := GPIBxFudge;
        YFudge := GPIByFudge
        end;
    end;
  
  TabLeft := TabWhite;                     { get the new button values } 
  TabMiddle := TabYellow;
  TabRight := TabGreen;
            
  if TabFinger
  then begin  
    SumX := SumX - shift( SumX, -TabAverage ) + TX; { remove point's average }
    SumY := SumY - shift( SumY, -TabAverage ) + TY; { and add newest point }
    end
  else begin                               { puck just landed on pad }    
    SumX := Shift( TX, TabAverage );       { use current point as all }
    SumY := Shift( TY, TabAverage );       { the points in the sum }
    TabAbsX := TX;                         { use current point as last point }
    TabAbsY := TY;
    TabFinger := true                      { indicate puck is on pad }
    end;
  
  TX := shift( SumX, -TabAverage );        { average last 4 points to get }
  TY := shift( SumY, -TabAverage );        { this point }
  
  if RealRelTablet
  then begin
    TabRelX := TabRelX + (TX - TabAbsX);   
    TabRelY := TabRelY - (TY - TabAbsY)
    end
  else begin
    TabRelX := TX - Xfudge;
    TabRelY := YFudge - TY
    end;
    
  TabAbsX := TX;
  TabAbsY := TY;
  
       if TabRelX < SLeftX  then TabRelX := SLeftX
  else if TabRelX > SRightX then TabRelX := SRightX;
  
       if TabRelY < STopY    then TabRelY := STopY
  else if TabRelY > SBottomY then TabRelY := SBottomY;
   
end { TabletUpdate };

procedure CursorUpdate;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CursorUpdate updates the displayed cursor position (if it has changed).
{
{-----------------------------------------------------------------------------}

var
  DispList,NewDisp: DispPtr;
  ix,j,yscreen: integer;
  CX, CY, NewCurX, W, X: integer;

begin
  CX := CursorX + 64 - PointX;
  CY := CursorY - PointY;
  ScrBuf^.CursX:=CrsConst0 - Shift(CX,-3);

  if  (CursorX >= SLeftX)
  and (CursorX <= SRightX) then begin { point of cursor is visible }
    NewCurX := LAnd(CX,7);
    if NewCurX <> OldCurX then begin
      { must shift the pattern for proper X alignment }
      RasterOp(RRpl,               { function }
               56,                 { width }
               64,                 { height }
               NewCurX,            { destination x coordinate }
               0,                  { destination y coordinate }
               4,                  { destination words/scan line }
               MakePtr(CursorSeg,0,RasterPtr),       { dest. base addr. }
               OldCurX,            { source x coordinate }
               0,                  { source y coordinate }
               4,                  { source words/scan line }
               MakePtr(CursorSeg,0,RasterPtr));      { src. base addr. }

      if NewCurX < OldCurX  { clear piece of cursor left behind }
      then begin
        X := NewCurX + 56;
        W := 64 - X
        end
      else begin
        X := 0;
        W := NewCurX
        end;

      RasterOp(RXor,               { function }
               W,                  { width }
               64,                 { height }
               X,                  { destination x coordinate }
               0,                  { destination y coordinate }
               4,                  { destination words/scan line }
               MakePtr(CursorSeg,0,RasterPtr),       { dest. base addr. }
               X,                  { source x coordinate }
               0,                  { source y coordinate }
               4,                  { source words/scan line }
               MakePtr(CursorSeg,0,RasterPtr));      { src. base addr. }

      OldCurX:=NewCurX
      end
    end;

  if (CY <> OldCurY)
  or (CursorY < MinCurY)
  or newFunct then begin
    { CursorY < MinCurY allows forced re-compute for changing screen size }
    newFunct := false;

    if ScrBuf^.Cmd = DisFile0 { switch to the other display list }
    then DispList := DisFile1
    else DispList := DisFile0;

    ix:=0;
    yscreen:=0;

    if (CursorY < MinCurY)
    or (CursorY > SBottomY)  { cursor not in displayed area }

    then while yscreen < MaxCurY do begin
      if yscreen < SBottomY
      then DispList^[ix].int:=VisOnly + CursF
      else DispList^[ix].int:=VisOnly+BotCursF; {below displayed area}
      ix:=ix+1;
      yscreen:=yscreen+SegSize
      end

    else begin { set up display list for new cursor position }
           { incr down in 128 line segments until we get to cursor }
      while (yscreen + SegSize) < CY do begin
        DispList^[ix].int:=VisOnly + CursF;
        ix:=ix+1;
        yscreen:=yscreen+SegSize
        end;

      j:=CY - yscreen;  { now for possible partial until cursor }
      if j > 0 then begin
        DispList^[ix].int:=VisOnly + CursF + (SegSize - j);
        ix:=ix+1;
        yscreen:=yscreen+j
        end;
      { now for the cursor - do we show all or clip at the bot of screen }
      if (crsHeight + yscreen) >= SBottomY
      then j:=SBottomY-yscreen+1     { clip }
      else j:=crsHeight;             { show all }

      DispList^[ix].int:=VisAndCur + CursF + (SegSize - j);
      ix:=ix+1;
      yscreen:=yscreen+j;
      { now the rest of the screen - get to 128 word boundary }
      j:=SegSize - (yscreen mod SegSize);
      if (j <> SegSize) then begin
        if yscreen < SBottomY
        then DispList^[ix].int := VisOnly + CursF + (SegSize - j)
        else DispList^[ix].int := VisOnly + BotCursF + (segSize - j);
        ix:=ix+1;
        yscreen:=yscreen+j
        end;
      while yscreen < MaxCurY do begin
        if yscreen < SBottomY
        then DispList^[ix].int:=VisOnly + CursF
        else DispList^[ix].int:=VisOnly + BotCursF;
        ix:=ix+1;
        yscreen:=yscreen+SegSize
        end;
      end;

    DispList^[ix].int:=DisCst0+BotCursF;
    DispList^[ix+1].int:=DisCst1+BotCursF;
    ScrBuf^.Cmd:=DispList
    end;

  OldCurY:=CY

end { CursorUpdate };

begin { Scr_Interrupt (formerly TabIntr)  Tablet/Clock Interrupt Service }

  TabletUpdate;

  case CCursMode of
    trackCursor: case TabMode of
      ScrAbsTablet,
      TabAbsTablet: begin
                      if TabAbsX < SLeftX  then CursorX := SLeftX
                 else if TabAbsX > SRightX then CursorX := SRightX
                 else CursorX := TabAbsX;
                      if TabAbsY < STopY    then CursorY := STopY
                 else if TabAbsY > SBottomY then CursorY := SBottomY
                 else CursorY := TabAbsY;
                 if TabMode = ScrAbsTablet
                 then CursorY := STopY + (SBottomY - CursorY)
                end;
      RelTablet: begin CursorX := TabRelX; CursorY := TabRelY end;
      end;
    indepCursor: ;
    offCursor  : CursorY := MaxCurY + PointY + 1
    end;

  CursorUpdate;

  InLineByte({INTON} 106)        { enable further interrupts }

end { Vid_Interrupt (formerly TabIntr) };


function Vid_SetUpUDevTab: pointer;
{-----------------------------------------------------------------------------}
{
{ Abstract:
{       Set up the  screen control block for the microcode. Does not use
{       Screen package.
{ Returns:
{       Pointer to the screen buffer.
{-----------------------------------------------------------------------------}

var
  i : integer;

begin 
  
  new( IOSegNum, 4, ScrBuf );
  Vid_SetUpUDevTab := recast ( ScrBuf, pointer); 

  new( IOSegNum, 4, DisFile0 );
  new( IOSegNum, 4, DisFile1 );
  SetDDS(331);

  for i := 0 to 7 do 
    DisFile0^[i].int := Map+VisOnly; { set up initial display list }
  
  DisFile0^[8].int := DisCst0;
  DisFile0^[9].int := DisCst1;
  SetDDS(332);

  {$R-}
  with ScrBuf^ do begin      { and the master display blk }
    Cmd := DisFile0;

    if MemoryInBlocks = #1000 
    then begin
      with SAT^[ScreenSeg] do
        ScreenBase := Shift(BaseUpper,15) + Shift(BaseLower,7);
      with SAT^[CursorSeg] do
        CursorBase := Shift(BaseUpper,15) + Shift(BaseLower,7)
      end
    else begin
      with SAT^[ScreenSeg] do
        ScreenBase := Shift(BaseUpper,12) + Shift(BaseLower,4);
      with SAT^[CursorSeg] do
        CursorBase := Shift(BaseUpper,12) + Shift(BaseLower,4)
      end;

    CursX := 0
    end;
  {$R=}    
  SetDDS(333)

end.
