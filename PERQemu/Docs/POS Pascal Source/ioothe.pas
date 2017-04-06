{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IO_Others;
{-----------------------------------------------------------------------------
{
{ IO_Others - Miscellaneous IO routines.
{ Miles A. Barel  ca. 1 Jan 80.
{ Copyright (C) 1980, 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{       IO_Others exports routines for the Cursor, Table, Screen, Time, and
{       Keyboard.
{
{-----------------------------------------------------------------------------}

{ $Version 7.2 for POS}
{-----------------------------------------------------------------------------
{  Change Log:
{
  22 Feb 84  SSJ  V7.2  Set the LandKrizTablet boolean and use it to set
                        the shift factor when the tablet is in absolute
                        mode.
                        
  27 Oct 83  DLK  V7.1  Fix SetGPIBBitPad to use new IODevRead command.
  
  27 Apr 83  SSJ  V7.0  Tabmouse to be initialized to Ord('0') instead of 0
  
  18 Apr 83  ICL  V6.9  Dave Anderson...set GPIBTabletState (from IO_GPIB) to
                        zero when turning tablet off. Tidy GPIB commands
                        a little in SetGPIBBitPad and move circular buffer
                        clear loop to before turning tablet on.

  14 Apr 83  ICL  V6.8  Dave Anderson...added IOReset and interface clear to
                        SetGPIBBitPad to ensure BitPad always recovers.

  28 Feb 83  BAM  V6.7  Fixed bug in Kriz ShiftXAmt and changed GPIB ShiftXAmt.
                        
  14 Feb 83  BAM  V6.6  Turn off press when turn off tablet so not on when
                        tablet is turned on.
                        
   9 Feb 83  BAM  V6.5  Make work for Landscape monitor.  Fix some Document
                        problems.
                        
   3 Feb 83  SSJ  V6.4  KrizTabconnected was being set false when the
                        tablet was turned off; now check for connected only
                        when turning it on.
                        
  27 Jan 83  RSR  V6.3  Remove "The" from pointer types. Deleted exported
                        variable KrizTablet.  It duplicates TypePointDev.
               
  12 Jan 83  AGR  V6.2  Made several changes in SetGPIBBitPad to improve bit
                        pad operation.

  20 Dec 82  AGR  V6.1  Changed code dealing with the GPIB to reflect changes
                        made in IOGPIB.

  27 Oct 82       V6.0  August G. Reinig and Chuck Beckett
                        Changed code to interface with the new Z80 code
                        and micro code.  Added support of the Kriz tablet.

  28 Jan 82  WJH  V5.7  increment ErrorCnt in IOSetModeTablet

  22 Dec 81  WJH  V5.6  fix IOGetTime for calls while booting
  
  25-Jun-81  JPS  V5.5  Add new TabletMode: offTablet which disables the
                        tablet.  Make all other TabletModes enable it.
                        Get rid of EnTabUpdate and EnCurUpdate--go back to
                        disabling and enabling interrupts during tablet and
                        cursor code.  This means that loading and reading the
                        cursor pattern must use IncIOCount to ensure that the
                        client's segment is swapped in.

   4-Jun-81  JPS  V5.4  Export new IOKeyClear procedure.

   3-Jun-81  JPS  V5.3  Add Virgil headers and comments.

  23-May-81  JPS  V5.2  Replace enable/disable of control-c processing
                        (exported by System) with enable/disable of KeyBoard
                        interrupts (exported by IO_Others).
  
  31-Mar-81  GGR  V5.1  Added 4-button mouse support.

  11-May-81  JPS  V5.0  Split IO into several modules.
                        Removed IOSetTime procedure
  
   6-May-81  JPS  V4.7  1. Use new form of the SetCylinder StartIO.
                        2. Don't bother doing 10 trys in FindSize since only
                           the last result was believed regardless of success
                           or failure.
                        3. Hang if we cannot figure out the size of the disk.
  
  11-Apr-81  JPS  V4.6  Changes for virtual memory.
  
  19-Mar-81  BAM  V4.5  Changed name of included modules to IO_Init and IO_Proc
  
   3-Mar-81  JPS  V4.4  1) Fix LocateDskHeads and FindSize to agree with V4.3.
                        2) Teach the HardDisk timeout code about multi-sector
                           operations.

  28-Feb-81  JPS  V4.3  No longer do conversions on Disk Physical block
                        numbers (reinstating changes made in V4.0).

  25-Feb-81  GGR  V4.2  Added setting/reading of DskFill1 in UnitIO.
                        Moved new/dispose of CB from UnitIO to IO.Init.

  16-Feb-81  BAM  V4.1  Put back in conversions on Disk Physical block
                        numbers; fixed botCursF bug.  Del XXX procedures;
                        Changed to use new screen
  
   9-Feb-81  BAM  V4.0  No longer does conversions on Disk Physical block
                        numbers; fixed CursorUpdate to allow partial screen
                        display and added procedure IOScreenSize to set a 
                        new size.
  
  13-Jan-81  JPS  V3.3  Move creation of the IOSeg to memory manager init.
                        Move $R- to private part.

  20-Nov-80  JPS  V3.2  Initialize TabFifoInx in InitTablet.
  
  17-Nov-80  JPS  V3.1  Export the interrupt table.
                        Check SystemInitialized for control-C abort.
  
  16-Nov-80  BAM  V3.0  Radically changed Cursor and Tablet interface.  New
                        time procedures.  Split into another include file.
  
  10-Oct-80  JPS  V2.2  Added support for the diagnostic display (DDS).
 
  27-Sep-80  DAS  V2.1  Added timeout code to UnitIO for the
                        hard disk.
 

  19-Sep-80  DAS  V2.0  Added code for 24 MByte disks.

{-----------------------------------------------------------------------------}

{*******************************}   Exports   {*****************************}



Imports SystemDefs from SystemDefs; 
 



{ tablet/cursor procedures }

Type    
  CursFunction = (CTWhite,  CTCursorOnly, CTBlackHole, CTInvBlackHole,
                  CTNormal, CTInvert,     CTCursCompl, CTInvCursCompl);
  TabletMode = (relTablet, scrAbsTablet, tabAbsTablet, offTablet);
  CursMode = (OffCursor, TrackCursor, IndepCursor);
  CursorPattern = array[0..63,0..3] of integer;
  CurPatPtr = ^CursorPattern;
  TabletType = (NoPointDev, KrizTablet, GPIBBitPad );
        
        
Var     
  TabRelX, TabRelY : integer;  { tablet relative coordinates }
  TabAbsX, TabAbsY : integer;  { tablet absolute coordinates }  

  TabFinger  : boolean;       { finger on tablet }
  TabSwitch  : boolean;       { switch pushed down }
  TabWhite   : boolean;       { True if white or left button down }
  TabGreen   : boolean;       { True if green or right button down }
  TabBlue    : boolean;       { True if blue button down }
  TabYellow  : boolean;       { True if yellow or middle button down }
  TabMouse   : integer;       { Actual output from mouse }  
      
  DefaultCursor: CurPatPtr;   { default cursor pattern }  

  RealRelTablet : boolean;    { indicate if table in true relative mode }
  TypePointDev : TabletType;  { tells which tablet is the pointer }
  BitPadTimeOut : integer;    { how long before puck off bit pad }
  KrizTabConnected : boolean; { true if KrizTablet connected  }
  GPIBpadConnected : boolean; { ture iff the BitPad is connected  }

  TabLeft   : boolean;        { true if left or white button down }
  TabMiddle : boolean;        { true if middle or yellow button down }
  TabRight  : boolean;        { true if right or green button down }
  LandKrizTab: boolean;       { true if the kriz tablet is a landscape }

Procedure IOLoadCursor(Pat: CurPatPtr; pX, pY: integer); 
                                           { load user cursor pattern }         
Procedure IOReadTablet(var tabX, tabY: integer); { read tablet coordinates } 

Procedure IOSetFunction(f: CursFunction); 

Procedure IOSetModeTablet(m: TabletMode); { set the mode to tell what kind of
                                           tablet is currently in use }

Procedure IOCursorMode (m: CursMode); { if track is true, then Tablet
                                        coordinates are copied every 1/60th
                                        second into the cursor position. if
                                        indep, then coordinates are changed
                                        only by user.  If off, then no
                                        cursor displayed }

Procedure IOSetCursorPos(x,y: Integer); { if trackCursor is false, then sets
                                         cursor x and y pos.  If tracking, then
                                         sets both tablet and cursor. }

Procedure IOSetTabPos (x,y: Integer);  { if trackCursor is false, then sets
                                         tablet x and y pos.  If tracking, then
                                         sets both tablet and cursor }
                                         
Procedure IOReadCursPicture(pat: CurPatPtr; var px, py: integer);
                                        { copies current cursor picture into
                                          pat and sets px and py with the 
                                          offsets for the current cursor }



Procedure IOGetTime(var t: double);  { Get the double word 60 Hertz time }
        


{Procedure to change screen size}

Procedure IOScreenSize(newSize: integer; Complement: Boolean);
                                        { newSize is number of scan lines in
                                          new screen; must be a multiple of 
                                          128. Complement tells whether the 
                                          rest of the screen should be the
                                          opposite color from the displayed
                                          part }

{ disable/enable keyboard interrupts }

Procedure IOKeyDisable( var OldKeyEnable: Boolean );

Procedure IOKeyEnable( OldKeyEnable: Boolean );  { enable keyboard interrupts }

Procedure IOKeyClear;    { clear the IO type-ahead buffer }

Procedure IOSetRealRelTablet( state: boolean );  { if state is true, then the
                                                   tablet will be a true 
                                                   relative tablet }
                                                   
Procedure IOChooseTablet( model : TabletType );  { choose which tablet will
                                                   be operating }
                                                   
Procedure IOSetBitPadUpdateTimeOut( cnt: integer );  { set time-out constant
                                                       for BitPad updates }
                                                       
{*******************************}   Private   {*****************************}

const
  debug = false;

{$ifc debug
then}    Imports TestSystem from TestSystem;
{$elsec} Imports System from System;
{$endc} 

Imports IOErrors from IOErrors;
Imports IO_Private from IO_Private;
Imports IOKeyboard from IOKeyboard;
Imports IOGPIB from IOGPIB;
Imports IOPointDev from IOPointDev;
Imports IOVideo from IOVideo;
Imports IOPointDev from IOPointDev;
Imports Raster from Raster;
Imports Screen from Screen;
Imports Virtual from Virtual;


procedure IOCursorMode( M: CursMode );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Sets the mode for the cursor.  If the mode m is set to TrackCursor,
{      Tablet coordinates are copied every 1/60th second into the cursor
{      position.  If it's set to IndepCursor, coordinates are changed only
{      by the user.  If it is set to OffCursor, no cursor is displayed.
{
{ Parameters:
{
{      m - the new  mode  for  the  cursor.
{
{-----------------------------------------------------------------------------}

begin

 CCursMode := M

end { IOCursorMode };

procedure IOSetModeTablet( M: TabletMode );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Sets the mode to tell what kind of tablet is currently in use.
{
{ Parameters:
{
{      m  - the mode for the tablet.
{
{-----------------------------------------------------------------------------}

const
  On = 255;
  Off = 0;


Procedure SetKrizTablet( state : integer );

{----------------------------------------------------------------------------}
{
{ Abstract
{       Turns the Kriz Tablet on or off.
{
{ Parameters
{       state - On or Off
{
{----------------------------------------------------------------------------}

var
  OurBuf : IOBuffer;
  OurSts : IOStatus;
  Bufr   : IOBufPtr;
  Time   : long;
  StsPtr : IOStatPtr;
  i : integer;

begin
  {$ifc not debug then}
  OurBuf[0] := state;

  loadadr( Bufr );              { get a pointer to the buffer }
  inlinebyte( {LSSN}  99 );
  loadadr( OurBuf );
  inlinebyte( {STDW} 183 );
  
  loadadr( StsPtr );            { get a pointer to the status block }
  inlinebyte( {LSSN} 99 );
  loadadr( OurSts );
  inlinebyte( {STDW} 183 );

  UnitIO( PointDev, Bufr, IOConfigure, 1, recast(Time,double), nil, StsPtr );
  
  If State = On Then            { if turning on then check for connected }
    Begin
    TabSwitch := false;         { set a well defined tablet state }
    KrizInfo^.XPos := -1;       { set bogus value here so we can tell }
    KrizInfo^.YPos := 0;        { if tablet is active }
    KrizInfo^.Buttons := 0;
      
   
    Time := TimeBuf^;                 { get the current time }
    while (TimeBuf^ - Time <= 3)      { wait three jiffies }
      and (KrizInfo^.XPos = -1) do;   { or for data to come from the tablet }
   
    if KrizInfo^.XPos <> -1           { if data came from the tablet }
    then 
      begin
      KrizTabConnected := true;
      LandKrizTab := Land(KrizInfo^.Buttons, #100) <> 0;
      end
    else begin
      KrizTabConnected := false;
      KrizInfo^.XPos := 0
      end;
    End;
  {$endc}
end;

Procedure SetGPIBBitPad( state : integer );

{---------------------------------------------------------------------------}
{
{ Abstract
{      Turns the GPIB bit pad on or off
{
{---------------------------------------------------------------------------}

const
  GPIBBitPad = #10;   { GPIB address of the Bit Pad }
  GPIBNone   = #00;   { GPIB address of no device }
  AuxCmd     = #03;  { A GPIB Auxilary command }

type                             
  GPIBBuffer = packed array[0..7] of 0..255;

var
  pGPIBBuf : IOBufPtr;
  Buffer   : GPIBBuffer;
  StsPtr   : IOStatPtr;
  Status   : IOStatus;
  Indicator: integer;
  Results  : integer;
  i : integer;
  
procedure DoUnitIO( Command : IOCommands; ByteCnt : integer );

var
  TimeOut : long;

begin
  
  TimeOut := 60;         { one second }
  UnitIO( GPIB, pGPIBBuf, Command, ByteCnt
        , recast( TimeOut, double ), nil, StsPtr );

  if StsPtr^.SoftStatus <> IOEIOC then GPIBPadConnected := false;

end;

begin
  {$ifc not debug then}
  loadadr( pGPIBBuf );         { get a pointer to the buffer }
  inlinebyte( {LSSN} 99 );     { using this trick avoids a new } 
  loadadr( Buffer );
  inlinebyte( {STDW} 183 );
                          
  loadadr( StsPtr );           { get a pointer to the status block }
  inlinebyte( {LSSN} 99 ); 
  loadadr( Status );
  inlinebyte( {STDW} 183 );
  
  GPIBTabletState:=0; {tell ISR to ignore tablet data}
  GPIBPadConnected := true;    { assume the bit pad is there }
  DoUnitIO( IOReset, 0 );      { flush out the Z80 }
 
  Buffer[0] := AuxCmd;  Buffer[1] := #217;   { SIC, enable interface clear }
  DoUnitIO( IOWriteRegs, 2 );

  repeat                             { clear any garbage in the }
    loadexpr( GPIB );
    startio( EP_GetChar );           { GPIB's circular buffer }
    storexpr( Indicator );
    storexpr( Results )    
  until Indicator = 0;
  for i:=1 to 50 {100} do;         { wait 100usec since SIC (allowing for 
                                     time spent in UnitIO etc) }        
  
  Buffer[0] := AuxCmd;  Buffer[1] := #017;   { SIC, disable interface clear }
  DoUnitIO( IOWriteRegs, 2 );                                  
  
  { Issue the DevRead command to turn the BitPad on or off. }
  Buffer[0] := 0;     { Options flag byte }
  { Buffer[1],Buffer[2] only need be set if changing masks for 9914 interrupts}
  if state = on then Buffer[3] := GPIBBitPad
  else Buffer[3] := 255; {i.e., no address}
  Buffer[4] := 255;   {i.e., no Secondary Address}
  Buffer[5] := 0;     {i.e., we do not wait to get any data}
  DoUnitIO(IODevRead, 6);
  {$endc}
end;

var
  OldKeyEnable : boolean;

begin { IOSetModeTablet }

  if m = TabMode then exit( IOSetModeTablet ); { mode set?  we're done }
  
 { IOKeyDisable( OldKeyEnable );}
  
    {turn off all buttons}
    TabSwitch := false;
    TabWhite  := false;
    TabGreen  := false;
    TabBlue   := false;
    TabYellow := false;
    TabMouse  := Ord('0');
    TabLeft   := false;
    TabMiddle := false;
    TabRight  := false;

  if TabMode = offtablet 
  then begin                                { tablet is off, we want it on }
    TabFinger := false;
    TabCount := TabIgnore;

    case TypePointDev of
      KrizTablet : SetKrizTablet( on );
      GPIBBitPad : SetGPIBBitPad( on );
      otherwise     : { do nothing }
      end
    end
   
  else if m = offtablet then                { tablet is on, we want it off }
    begin
    
    case TypePointDev of
      KrizTablet : SetKrizTablet( off );
      GPIBBitPad : SetGPIBBitPad( off );
      otherwise     : { do nothing }
      end;
    end;
    
  TabMode := m;
 { IOKeyEnable( OldKeyEnable ) }
  
end { IOSetModeTablet };

procedure IOLoadCursor( Pat: CurPatPtr; pX, pY: integer );

{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Loads a user cursor pattern into the screen cursor.
{
{ Parameters:
{
{      Pat - a pointer to a cursor.  It should be quad-word aligned.
{
{      pX and pY - offsets in the cursor where the origin is thought
{                  to be.  For example, if the cursor is a bull's eye,
{                  31 bits diameter flushed to the upper left corner of
{                  the cursor box, using (pX, pY) = (15, 15) will have
{                  the cursor surround the things pointed at.
{
{      NOTE: This procedure supports a cursor which is 56 x 64; with a scan
{            line length of 4 }
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;
  CurPtr: IOPtrKludge;

begin { IOLoadCursor }

  { Load a new cursor pattern - take care to maintain X position orientation }
  { in case it is currently visible! }
  
  IOKeyDisable( OldKeyEnable );     { don't let user interfere }
  
  CurPtr.Buffer := recast( Pat, IOBufPtr );  { lock pattern into memory }
  IncIOCount( CurPtr.Segment );
  
  inlinebyte( {INTOFF} 105 );       { don't bother me }
  RasterOp( RRpl, 56, 64, OldCurX, 0, 4, Cursor, 0, 0, 4, Pat );
  PointX := pX;
  PointY := pY;
  inlinebyte( {INTON} 106 );        { finished }
  
  DecIOCount( CurPtr.Segment );     { unlock the pattern }          
  
  IOKeyEnable( OldKeyEnable )       { let user type }
  
end { IOLoadCursor };

procedure IOReadCursPicture( Pat: CurPatPtr; var pX, pY: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Copies the current cursor picture into Pat and sets pX and pY with
{      the offsets for the current cursor.
{
{ Parameters:
{
{      Pat, pX, and pY are filled with data on the current cursor.  Note
{      that Pat must be quad-word aligned.
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;
  CurPtr: IOPtrKludge;

begin
  
  IOKeyDisable( OldKeyEnable );
  CurPtr.Buffer := recast( Pat, IoBufPtr );
  IncIOCount( CurPtr.Segment );
  inlinebyte( {INTOFF} 105 );
  
  rasterop( RRpl, 56, 64, 0, 0, 4, Pat, OldCurX, 0, 4, Cursor );

  pX := PointX;
  pY := PointY;
  
  inlinebyte( {INTON} 106 );
  DecIOCount( CurPtr.Segment );
  IOKeyEnable( OldKeyEnable )


end { IOReadCursPicture };

Procedure IOSetFunction( f: CursFunction );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Sets the cursor function.
{
{ Parameters:
{
{      f - the function to set the cursor to.
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;

begin
  
  IOKeyDisable( OldKeyEnable );
  inlinebyte( {INTOFF} 105 );
  
  newFunct := true;
  CursF := shift( ord(f), 13 );
  
  if BotComplemented 
  then BotCursF := land( lxor( CursF, #20000 ), #20000 )
  else BotCursF := land( CursF, #20000 );
  
  inlinebyte( {INTON} 106 );
  IOKeyEnable( OldKeyEnable )

end;

procedure IOSetCursorPos( x, y: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      If the cursor's mode is not TrackCursor, this procedure sets the
{      cursor's x and y positions.  If tracking, it sets both tablet and
{      cursor.
{
{ Parameters:
{
{      x and y - the new cursor coordinates.
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;

begin


       if x < SLeftX  then x := SLeftX
  else if x > SRightX then x := SRightX;
  
       if Y < STopY    then y := STopY
  else if Y > SBottomY then y := SBottomY;  { or MaxCurY??? }
  
  IOKeyDisable( OldKeyEnable );
  inlinebyte( {INTOFF} 105 );
  
  if CCursMode = trackCursor then begin 
    TabRelX := x;
    TabRelY := y; 
    case TypePointDev of
      KrizTablet : begin
        KrizInfo^.XPos := Shift(x + KrizXfudge, -KrizShiftXAmt);
        KrizInfo^.YPos := KrizYfudge - y
        end;
      GPIBBitPad : begin
        GPIBTabBuf.X := Shift(x + GPIBxfudge, -GPIBShiftXAmt);
        GPIBTabBuf.Y := 2 * (GPIByFudge - y) 
        end 
      end
    end;
  
  CursorX := x;
  CursorY := y;
  
  inlinebyte( {INTON} 106 );
  iokeyenable( OldKeyEnable )
  
end { IOSetCursorPos };

procedure IOSetTabPos( x, y: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      If the cursor's mode is not set to TrackCursor, IOSetTabPos sets
{      the tablet's x and y positions. If the mode is TrackCursor, both
{      tablet and cursor are set.
{
{ Parameters:
{
{      x and y - the new tablet coordinates.
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;

begin

       if x < SLeftX  then x := SLeftX
  else if x > SRightX then x := SRightX;

       if y < STopY    then y := STopY
  else if y > SBottomY then y := SBottomY;

  IOKeyDisable( OldKeyEnable );
  inlinebyte( {INTOFF} 105 );
  
  TabRelX := x;
  TabRelY := y;
  
  if not RealRelTablet then 
    case TypePointDev of
      KrizTablet : begin
        KrizInfo^.XPos := Shift(x + KrizXfudge, -KrizShiftXAmt);
        KrizInfo^.YPos := KrizYfudge - y
        end;
      GPIBBitPad : begin
        GPIBTabBuf.X := Shift(x + GPIBxfudge, -GPIBShiftXAmt);
        GPIBTabBuf.Y := 2 * (GPIByFudge - y) 
        end
      end;

  if CCursMode = trackCursor then begin
    CursorX := x;
    CursorY := y
    end;
     
  inlinebyte( {INTON} 106 );
  IOKeyEnable( OldKeyEnable )        
      
end { IOSetTabPos };

procedure IOReadTablet( var tabX, tabY: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Reads tablet coordinates.
{
{ Parameters:
{
{      tabX and tabY - set to x and y values of the tablet.
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;

begin { IOReadTablet }

  IOKeyDisable( OldKeyEnable );
  inlinebyte( {INTOFF} 105 );
  
  case TabMode of
    TabAbsTablet : begin
                     TabX := TabAbsX;
                     TabY := TabAbsY
                   end;
    ScrAbsTablet : begin
                     TabX := STopY + SBottomY - TabAbsX;
                     TabY := TabAbsY
                   end;
    relTablet    : begin
                     TabX := TabRelX;
                     TabY := TabRelY
                   end
    end;
    
  inlinebyte( {INTON} 106 );
  IOKeyEnable( OldKeyEnable )
  
end { IOReadTablet };

Procedure IOScreenSize( newSize: Integer; Complement: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Changes the amount of screen visible to the user (the rest is
{      turned off, hence not displayed).  The  cursor is prevented from
{      going into the undisplayed part of the screen.
{
{ Parameters:
{
{      newSize - number of scan lines in new screen; it must be a multiple
{                of 128.
{
{      Complement  -  tells whether the rest of the screen should be the
{                     opposite color of the displayed part.
{
{-----------------------------------------------------------------------------}

var 
  OldKeyEnable: Boolean;

begin
  
  if (newSize < 0)              { check for valid screensize }
  or (newSize > SMaxBitHeight)
  or (newSize mod 128 <> 0)
  then exit( IOScreenSize );  
  
  BotComplemented :=  Complement;
  
  IOKeyDisable( OldKeyEnable );
  inlinebyte( {INTOFF} 105 );
  
  SBottomY := newSize - 1;
  if Complement
  then BotCursF := land( lxor( CursF, #20000), #20000)
  else BotCursF := land( CursF, #20000 ); 
  
  newFunct := true;             { so CursorUpdate will notice }
  
  inlinebyte( {INTON} 106 );
  IOKeyEnable( OldKeyEnable )
  
end;

Procedure IOGetTime( var t : double );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{      Reads the 60 Hertz clock.
{
{ Parameters:
{
{      t - set to the new time.
{
{-----------------------------------------------------------------------------}

begin

  if SystemInitialized
  then t := recast( TimeBuf^, double )
  else begin
    t[0] := 0;
    t[1] := 0
    end
    
end;

procedure IOKeyDisable( var OldKeyEnable: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       IOKeyDisable is used to disable keyboard interrupts.  This is used
{       to delay processing of control-c, control-shift-c and control-shift-d
{       at critical times.  The old value of the keyboard interrupt enable
{       is returned and must be passed back to  IOKeyEnable when re-enabling
{       keyboard interrupts.  Characters typed while  keyboard interrupts are
{       disabled are remembered.  When keyboard interrupts are re-enabled,
{       the characters are processed.
{
{ Parameters:
{       OldKeyEnable - set to the old value of the enable.
{
{-----------------------------------------------------------------------------}

begin { IOKeyDisable }

  Key_Disable( OldKeyEnable );  
  
end { IOKeyDisable };

procedure IOKeyEnable( OldKeyEnable: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       IOKeyEnable is used to enable keyboard interrupts.  The old value of
{       the keyboard interrupt enable (as returned from IOKeyDisable) must
{       be passed to IOKeyEnable when re-enabling keyboard interrupts.  If
{       characters were typed while keyboard interrupts were enabled,
{       IOKeyEnable calls KeyIntr to process those characters.  The master
{       interrupt control (INTON and INTOFF QCodes) must be on when this
{       procedure is called.
{
{ Parameters:
{       OldKeyEnable - the old value of the enable.
{
{-----------------------------------------------------------------------------}

begin { IOKeyEnable } 

  Key_Enable( OldKeyEnable ); 
  
end { IOKeyEnable };

procedure IOKeyClear; 

{-----------------------------------------------------------------------------
{
{ Abstract:
{       IOKeyClear clears the keyboard type-ahead buffer.
{
{-----------------------------------------------------------------------------}

begin { IOKeyClear } 

  Key_Clear; 
  
end { IOKeyClear };


Procedure IOSetRealRelTablet( state : boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{     Allows the Kriz Tablet or GPIBBItPad to be handled in a true relative
{     mode.
{
{ Parameters:
{     state - if true then a true relative mode is obtained whenever the
{             TabMode = relTablet; lifting the mouse/puck/pen from the tablet
{             surface and then returning it does not alter cursor position on
{             the screen - only the movement of the mouse/puck/pen on the 
{             tablet surface will cause corresponding delta-x and delta-y
{             changes in cursor position.
{           - if false, then x and y co-ords are simple linear transformation
{             of the actual values to provide a 1-1 mapping of the 768 by 1024
{             screeninto the tabler surface whenever TabMode = relTablet.
{
{-----------------------------------------------------------------------------}

begin
 
  if state or (not SIsLandscape) then begin
                                      KrizShiftXAmt := 0;
                                      GPIBShiftXAmt := -1;
                                      end
  else begin
       If Not LandKrizTab then KrizShiftXAmt := 1; 
                              { if kriz tablet and landscape and absolute then
                                need to multiply coordinates by two 
                                unless land scape tablet connected }
       GPIBShiftXAmt := 0; { if GPIB tablet and landscape and absolute then
                                need to NOT divide coordinates by two }
       end;
  RealRelTablet := state

end;


Procedure IOChooseTablet( model: TabletType );
{-----------------------------------------------------------------------------}
{
{ Abstract:
{     Allows selection of tablet that will provide co-ordinate positions and
{     switch values.
{
{ Parameters:
{     model - choices are KrizTablet, GPIBBitPad, or NoPointDev
{
{ Side Effects:
{     Depending upon the state of TabMode, we may have to call IOSetModeTablet
{     to turn one tablet off and the other one on.
{
{-----------------------------------------------------------------------------}

var
  SaveMode : TabletMode;

begin

  if TabMode = OffTablet
  then TypePointDev := model
  else begin
    SaveMode := TabMode;
    IOSetModeTablet( offTablet );
    TypePointDev := model;
    IOSetModeTablet( SaveMode )
    end;

end;


procedure IOSetBitPadUpdateTimeOut( Cnt: integer );
{-----------------------------------------------------------------------------}
{
{ Abstract:
{     Set the time-out used to determine if the puck/pen is off the BitPad.
{
{ Parameters:
{     cnt - Cnt*1/60sec is the actual time-out.
{
{-----------------------------------------------------------------------------}

begin

  GPIBTabBuf.Update := Cnt

end.
