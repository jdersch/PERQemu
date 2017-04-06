{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program KeyTest;

{---------------------------------------------------------------------------
{ 
{ Abstract: KeyTest - Perq Keyboard test program.
{ 
{   This program is used to test the PERQ Keyboard.  It
{   will put a picture of the keyboard on the screen and then
{   indicate the character that is typed on the keyboard.
{
{ NOTES:
{
{   The break key (on the krismas keyboard) overlaps the cursor control
{   keys.  Thus, it cannot be distinguished from them based on keyboard input.
{     
{    Don Scelza   8-Nov-80.
{     
{   Copyright (C)  1980, 1981, 1982, 1983, Three Rivers Computer Corporation.
{     
{------------------------------------------------------------------------}


{ 24 Feb-83   2.1 Brad Myers
{     Fixed for landscape monitor.  Changed name to PERQ2KeyTest.
{
{ 19-Jan-83   2.0 Tom Stambaugh
{     Rewrote for Kristmas keyboard.
{
{ 16-Nov-82   1.6 Bill Braucher
{     Fixed names for 14-character compiler.
{
{ 29-Jan-82   1.5 Brad Myers
{     Catch help exception.
{
{ 25-Jun-81   1.4 Brad Myers
{     Initialize with IOSetTabletMode for D.4.
{
{ 25-Jun-81   1.3 Diana Connan Forgy
{     Changed imports IO to imports IO_Others and IO_Unit
{     for POS D compatability.
{
{  3-Apr-81   1.2  Diana Connan Forgy
{     Converted KeyTest to run under POS C.
{
{ 19-Nov-80   1.1  Don Scelza
{     Added the code to handle the new access to the keyboard.
{     Added the code that makes that program start in the single
{          key test.
{ 
{ 8-Nov-80    1.0  Don Scelza
{     Created KeyTest.
{ }    



imports Screen from Screen;
imports Raster from Raster;
imports Memory from Memory;
imports IO_Others from IO_Others;
imports IO_Unit from IO_Unit;
imports IOErrors from IOErrors;
imports Sleep from Sleep;
imports system from System;
imports except from except;

Const

  TitleString = 'KeyTest    Version 2.1 FOR PERQ 2'; 

  NumKeys       = 92;
  keyCodeMax    = #377;

  TitleCmd      = 1;
  blinkCmd      = TitleCmd + 1;
  holdCmd       = blinkCmd + 1;
  QuitCmd       = holdCmd + 1; 
  NumCmds       = QuitCmd;

  Ctrl          = 51;           { The control key }
  CapsLock      = 52;           { The shift lock key }
  ShiftLeft     = 71;           { Left side shift key }
  ShiftRight    = 82;           { Right side shift key }

  XSquareLength = 40;    { Number of pixels for a square in the X dir }  
  YSquareLength = 40;    { Number of pixels for a square in the Y dir }
  InterKeySpace = 4;     { Number of pixels between keys }

  Margin        =    3;            { How many pixels to reverse around prompt }
  ChrWidth      =    9;            { width of font  }
  ChrHeight     =   13;            { Height of font }

  skipX         = XSquareLength + InterKeySpace;
  skipY         = YSquareLength + InterKeySpace;
      
  Row1X         = XSquareLength - interKeySpace;
  Row2X         = XSquareLength - interKeySpace;
  Row3X         = XSquareLength - interKeySpace;
  Row4X         = (skipX div 2) - (2 * interKeySpace);
  Row5X         = (skipX div 2) - (2 * interKeySpace);

  CtrlX         = row4X;
  CtrlY         = 3 * skipY;
  CapsLockX     = row4X + skipX;
  CapsLockY     = 3 * skipY;
  ShiftLeftX    = row5X + skipX;
  ShiftLeftY    = 4 * skipY;
  ShiftRightX   = row5X + (13 * skipX);
  ShiftRightY   = 4 * skipY;

  mainX         =  10;
  MainY         = 150;
  AuxX          = mainX + (7 * skipX);
  AuxY          = mainY + (7 * skipY);

  TitleX        = 250;
  TitleY        = AuxY + (7 * skipY);
  BlinkX        = 10;
  BlinkY        = TitleY + 2 * (chrHeight + margin);
  HoldX         = BlinkX;
  HoldY         = BlinkY + chrHeight + margin;
  QuitX         = HoldX;
  QuitY         = HoldY  + chrHeight + margin;

  XBanner       =  titleX;                             
  YBanner       =  QuitY + (2 * (chrHeight + margin)); 
  XPrompt       =  QuitX;
  YPrompt       =  YBanner + chrHeight + margin;

{ KeyInfo is a structure that keeps information about each key on }
{ the keyboard.}

type
  
  keyCode       = 0..keyCodeMax; {Eight bits, for the krismas keyboard}
  keyBoardType  = (main, aux);
  keyNum        = 0..numKeys;

  pKeyLegend    = ^KeyLegend;
  pKeyInfo      = ^KeyInfo;
  pKey          = ^key;

  CmdRecord = Packed Record
     Cmd: String;               { The command name. }
     X,Y: Integer;              { X & Y location of the command. }
     end;

  ShapeType = (Square, SpaceKey, TabKey, ShiftKey, LockKey, ReturnKey,
               EnterKey);
                  
  KeyLegend = packed record
    X,Y     : integer;   { X and Y of legend with respect to upper left of key}
    legend  : string;    { The string on the key }
    next    : pKeyLegend;{ pointer to the next one}
    end;

  KeyInfo = packed record
    Index   : keyNum;
    X,Y     : Integer;              { X,Y position of left corner of the key, 
                                    { with respect to its keybrd origin }
    Shape   : ShapeType;            { The shape of the key}
    KeyCap  : pKeyLegend;           { Stuff to go on the key }
    KeyBrd  : keyBoardType;
    NextKey : pKeyInfo;             { Next key in keyboard }
    end;
  
  Key     = packed record
    keyPtr  : pKeyInfo;             { The keyInfo record for this key }
    nextKey : pKey;                 { For multiple key sequences }
    end;

Var 

  I, J          : Integer;
  SaveKeyIntr   : Integer;
  X, Y          : Integer;
  SDum          : String;
  Ch            : keyCode;
  KeyStayOn     : Boolean;
  OldTabSwitch  : Boolean;
  Pressed       : Boolean;
  TabX, TabY    : Integer;
  Index         : Integer;
  MainKeyBoard  : pKeyInfo;
  AuxKeyBoard   : pKeyInfo;
  KeyCodes      : Array[0..KeyCodeMax] of pKey;       {Probe by keyCode}
  CmdArray      : Array[1..NumCmds] of CmdRecord;

  CtrlKey,
  CapsLockKey,
  ShiftLeftKey,
  ShiftRightKey,
  KbdHelpKey       : pKeyInfo;








function makeLegend: pKeyLegend;
  Var
    tmpLegend: pKeyLegend;
  begin {makeLegend}
    new(tmpLegend);
    with tmpLegend^ do
      begin
        X       := -1;
        Y       := -1;
        legend  := '';
        next    := nil;
      end;
    makeLegend := tmpLegend;
  end;  {makeLegend}







function makeKey: pkey;
{-----------------------------------------------------------------------
{
{ Abstract:
{
{   Spawn a new key record and initialize its fields.
{
{--------------------------------------------------------------------} 
  var
    tmpKey : pKey;
  begin
    new(tmpKey);
    with tmpKey^ do
      begin
        keyPtr  := nil;
        nextKey := nil;
      end;
    makeKey := tmpKey;
  end;







function makeKeyInfo: pKeyInfo;
{-----------------------------------------------------------------------
{
{ Abstract:
{
{   Spawn a new keyInfo record and initialize its fields.
{
{--------------------------------------------------------------------} 
  var 
    tmpKey  : pKeyInfo;
  begin {makeKeyInfo}
    new(tmpKey);
    with tmpKey^ do
      begin
        index   := 0;
        X       := -1;
        Y       := -1;
        Shape   := square;
        KeyCap  := nil;
        KeyBrd  := main;
        nextKey := nil;
      end;
    makeKeyInfo := tmpKey;
  end;  {makeKeyInfo}







procedure tellUser(message: string);
{-----------------------------------------------------------------------
{
{ Abstract:
{
{   Writes <message> in the prompt line, in reverse video.
{
{--------------------------------------------------------------------}
  Var
    CurX, CurY : integer;

  begin {tellUser}
    SReadCursor(CurX, CurY);
    SSetCursor(xPrompt, yPrompt);
    write(message);
    RasterOp(RNot,
      (Length(message) * ChrWidth) + (2 * Margin), chrHeight + 2 * Margin,
      xPrompt - margin, yPrompt - chrHeight - margin, SScreenW, SScreenP,
      xPrompt - margin, yPrompt - chrHeight - margin, SScreenW, SScreenP);
    SSetCursor(curX, curY);     {restore the cursor}   
  end;  {tellUser}









procedure ClearPrompt(howLong: integer);
{-----------------------------------------------------------------------
{
{ Abstract:
{
{   Clears <length> characters worth of prompt line.
{
{--------------------------------------------------------------------}
  begin {ClearPrompt}
    RasterOp(RXor,
      (howLong * ChrWidth) + (2 * Margin), chrHeight + 2 * Margin,
      xPrompt - margin, yPrompt - chrHeight - margin, SScreenW, SScreenP,
      xPrompt - margin, yPrompt - chrHeight - margin, SScreenW, SScreenP);   
  end;  {ClearPrompt}








function DefineKey( Index     : KeyNum;
                    InX,InY   : Integer;
                    KeyShape  : ShapeType;
                    aLegend   : pKeyLegend;
                    keyBoard  : keyBoardType): pKeyInfo;
{-----------------------------------------------------------------------
{
{ Abstract:
{   Searches the appropriate keyBoard for a keyInfo with whose index matches
{   <index>; returns it if found, otherwise makes new one and links it into
{   the keyBoard;
{
{ Parameters:
{    Index is the keyNumber of the key.
{    InX, InY are the location of the corner of the key, w.r.t.the keyboard
{       origin.
{    KeyShape is the shape of the key.
{    aLegend points to the legend on the keycap.
{    keyboard tells which one to use.
{
{----------------------------------------------------------------------------}
    Var
      aKey : pKeyInfo;
    begin {DefineKey}
      if Keyboard = main then
        aKey := mainKeyBoard
      else if keyBoard = Aux then
        aKey := auxKeyBoard
      else
        aKey := nil;
      while aKey <> nil do
        begin
          if aKey^.index = index then
            begin
              DefineKey := aKey;
              exit(defineKey);
            end
          else
            begin
              aKey := aKey^.nextKey;
            end;
        end;
      {no match, so spawn a new one and link it in}
      aKey := makeKeyInfo;
      with aKey^ do
        begin
          X         := InX;
          Y         := InY;
          Shape     := KeyShape;
          KeyBrd    := KeyBoard;
          keyCap    := aLegend;
        end;
      case keyBoard of
        Main:
          begin
            aKey^.nextKey   := mainKeyBoard;
            MainKeyBoard    := aKey;
          end; 
        Aux:
          begin
            aKey^.nextKey   := AuxKeyBoard;
            AuxKeyBoard     := aKey;
          end;
        end; {case}
      defineKey := aKey;
    end;  {DefineKey}                         







procedure installKey(
  aKey          : pKeyInfo;
  aKeyCode      : keyCode;
  withCtrl      : boolean;
  withShift     : boolean;
  withCapsLock  : boolean);
{--------------------------------------------------------------------
{
{ Abstract:
{
{   Links aKey into the keyCode array at aKeyCode.
{
{--------------------------------------------------------------------} 
  Var
    tmpKey : pKey;
  begin {installKey}
    new(tmpKey);
    with tmpKey^ do
      begin
        keyPtr := aKey;
        nextKey:= keyCodes[aKeyCode];
        keyCodes[aKeyCode] := tmpKey;
      end;
    if withCtrl then 
      begin
        new(tmpKey);
        tmpKey^.keyPtr := CtrlKey;
        tmpKey^.nextKey:= keyCodes[aKeyCode];
        keyCodes[aKeyCode] := tmpKey;
      end;
    if withShift then
      begin
        new(tmpKey);
        tmpKey^.keyPtr := shiftLeftKey;
        tmpKey^.nextKey:= keyCodes[aKeyCode];
        keyCodes[aKeyCode] := tmpKey;
        new(tmpKey);
        tmpKey^.keyPtr := shiftRightKey;
        tmpKey^.nextKey:= keyCodes[aKeyCode];
        keyCodes[aKeyCode] := tmpKey;
      end;
    if withCapsLock then
      begin
        new(tmpKey);
        tmpKey^.keyPtr := capsLockKey;
        tmpKey^.nextKey:= keyCodes[aKeyCode];
        keyCodes[aKeyCode] := tmpKey;
      end;
  end;  {installKey}








procedure Init;
{--------------------------------------------------------------------
{ Abstract:
{     This procedure is used to initialize the internal state of the
{     program.
{--------------------------------------------------------------------} 
  var
    I,
    keyX        : Integer;
    keyY        : Integer;
    tmpLegend   : pkeyLegend;
    Key         : pKeyInfo;

  begin {init}
    RasterOp(RXOr,SBitWidth,SBitHeight,0,0,SScreenW,SScreenP,
                                       0,0,SScreenW,SScreenP);
    mainKeyBoard := nil;
    AuxKeyBoard  := nil;
    for I := 0 to keyCodeMax do KeyCodes[I]     := nil;
  {Do the special keys}
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'CTRL';
        x := margin;
        y := chrHeight + margin;
      end;
    CtrlKey := defineKey(Ctrl, CtrlX, CtrlY, square, tmpLegend, main);
 
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'CAPS';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend := 'LOCK';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    CapsLockKey := defineKey(CapsLock,
                             CapsLockX,
                             CapsLockY,
                             tabKey,
                             tmpLegend,
                             main);

    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'SHIFT';
        x := margin;
        y := (2 * chrHeight) + margin;
      end;
    ShiftLeftKey := defineKey(ShiftLeft,
                              ShiftLeftX,
                              ShiftLeftY,
                              ShiftKey,
                              tmpLegend,
                              main);

    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'SHIFT';
        x := margin;
        y := (2 * chrHeight) + margin;
      end;
    ShiftRightKey := defineKey(ShiftRight,
                               ShiftRightX,
                               ShiftRightY,
                               ShiftKey,
                               tmpLegend,
                               main);
  {Main KeyBoard}

  begin {main, row 1}
    KeyX    := row1X;
    KeyY    := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'SET';
        x := margin + 15; {center the legend}
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend := 'UP';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end
      end;
    key := DefineKey(1, keyX, keyY, tabKey, tmpLegend, main);
    installKey(key,#13, false, false, false);
    
    KeyX    := keyX + skipX + (skipX div 2);
    KeyY    := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'HELP';
        x := margin; 
        y := (2 * chrHeight) + margin;
      end;
    key := DefineKey(2, keyX, keyY, square, tmpLegend, main);
    KbdHelpKey := key;
    installKey(key, #207, true, false, false);

    KeyX     := keyX + (9 * skipX) + (skipX div 2);
    KeyY     := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '^';
        x := margin; 
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend := '|';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight;
          end;
      end;
    key := DefineKey(3, keyX, keyY, square, tmpLegend, main);
    InstallKey(key, #200, false, false, false);
    InstallKey(key,  #34, true, false, false); 

    KeyX     := keyX + skipX;
    KeyY     := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '|';
        x := margin; 
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend := 'v';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight;
          end;
      end;
    key := DefineKey(4, keyX, keyY, square, tmpLegend, main);
    InstallKey(key, #201, false, false, false);
    InstallKey(key,  #35, true, false, false); 

    KeyX     := keyX + skipX;
    KeyY     := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '<--';
        x := margin; 
        y := (2 * chrHeight) + margin;
      end;
    key := DefineKey(5, keyX, keyY, square, tmpLegend, main);
    InstallKey(key, #202, false, false, false);
    InstallKey(key,  #36, true, false, false); 

    KeyX     := keyX + skipX;
    KeyY     := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '-->';
        x := margin; 
        y := (2 * chrHeight) + margin;
      end;
    key := DefineKey(6, keyX, keyY, square, tmpLegend, main);
    InstallKey(key, #203, false, false, false);
    InstallKey(key,  #37, true, false, false); 

    KeyX     := keyX + skipX;
    KeyY     := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'OOPS';
        x := margin; 
        y := chrHeight + margin;
      end;
    Key := DefineKey(7, keyX, keyY, square, tmpLegend, main);        
    installKey(key, #25, false, false, false);
    installKey(key, #225, true, false, false);

  end;  {main, row 1}

  begin {main, row 2}
    keyX     := row2X;
    KeyY     := skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'A E';
        x := margin;
        y := chrHeight; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := 'C S';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight;
            next := makeLegend;
            with next^ do
              begin
                legend := 'C C';
                x := tmpLegend^.next^.x;
                y := tmpLegend^.next^.y + chrHeight;
              end;
          end;
      end;
    key := defineKey(12, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #33, false, false, false); {noCtrl}
    installKey(key, #233, true, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '!';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '1';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(13, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #61, false, false, false);
    installKey(key,  #41, false, true, false);
    installKey(key, #261, true, false, false);
    installKey(key, #241, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '@';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '2';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key :=  defineKey(14, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #62, false, false, false);
    installKey(key, #100, false, true, false);
    installKey(key, #262, true,  false, false);
    installKey(key, #300, true,  true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '#';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '3';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(15, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #63, false, false, false);
    installKey(key,  #43, false, true, false);
    installKey(key, #263, true, false, false);
    installKey(key, #243, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '$';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '4';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(16, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #64, false, false, false);
    installKey(key,  #44, false, true, false);
    installKey(key, #264, true, false, false);
    installKey(key, #244, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '%';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '5';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(17, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #65, false, false, false);
    installKey(key,  #45, false, true, false);
    installKey(key, #265, true, false, false);
    installKey(key, #245, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '^';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '6';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(18, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #66, false, false, false);
    installKey(key, #136, false, true, false);
    installKey(key, #266, true, false, false);
    installKey(key, #336, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '&';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '7';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(19, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #67, false, false, false);
    installKey(key,  #46, false, true, false);
    installKey(key, #267, true, false, false);
    installKey(key, #246, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '*';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '8';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(20, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #70, false, false, false);
    installKey(key,  #52, false, true, false);
    installKey(key, #270, true, false, false);
    installKey(key, #252, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '(';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '9';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(21, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #71, false, false, false);
    installKey(key,  #50, false, true, false);
    installKey(key, #271, true, false, false);
    installKey(key, #250, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := ')';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '0';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(22, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #60, false, false, false);
    installKey(key,  #51, false, true, false);
    installKey(key, #260, true, false, false);
    installKey(key, #251, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '_';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '-';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight;
          end;
      end;

    key := defineKey(23, keyX, keyY, square, tmpLegend, main);
    installKey(key,  #55, false, false, false);
    installKey(key, #137, false, true, false);
    installKey(key, #255, true, false, false);
    installKey(key, #337, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '+';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '=';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(24, keyX, keyY, square, tmpLegend, main);

    installKey(key,  #75, false, false, false);
    installKey(key,  #53, false, true, false);
    installKey(key, #275, true, false, false);
    installKey(key, #253, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := '~';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := '`';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(25, keyX, keyY, square, tmpLegend, main);
    installKey(key, #140, false, false, false);
    installKey(key, #176, false, true, false);
    installKey(key, #340, true, false, false);
    installKey(key, #376, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'BS';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := ' ';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(26, keyX, keyY, square, tmpLegend, main);

    installKey(key,  #10, false, false, false);
    installKey(key, #210, true, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        Legend := 'BRK';
        x := margin;
        y := chrHeight + margin; {squeeze them vertically}
        next := makeLegend;
        with next^ do
          begin
            legend := ' ';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(27, keyX, keyY, square, tmpLegend, main);

    installKey(key, #200, false, false, false);
    installKey(key, #201, false, true, false);
    installKey(key, #202, true, false, false);
    installKey(key, #203, true, true, false);

  end; {main, row 2}  

  begin {main, row 3}

    keyX     := row3X;
    KeyY     := 2 * skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'TAB';
        x := margin + 15;    {center the legend}
        y := ((ySquareLength - chrHeight) div 2) + chrHeight;
      end;
    key := defineKey(32, keyX, keyY, tabKey, tmpLegend, main);
    installKey(key,  #11, false, false, false);
    installKey(key, #211, true, false, false);
    
    keyX     := keyX + skipX + (skipX div 2);
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'Q';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(33, keyX, keyY, square, tmpLegend, main);
    installKey(key, #161, false, false, false);
    installKey(key, #121, false, true, true);
    installKey(key, #361, true, false, false);
    installKey(key, #321, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'W';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(34, keyX, keyY, square, tmpLegend, main);
    installKey(key, #167, false, false, false);
    installKey(key, #127, false, true, true);
    installKey(key, #367, true, false, false);
    installKey(key, #327, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'E';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(35, keyX, keyY, square, tmpLegend, main);
    installKey(key, #145, false, false, false);
    installKey(key, #105, false, true, true);
    installKey(key, #345, true, false, false);
    installKey(key, #305, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'R';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(36, keyX, keyY, square, tmpLegend, main);
    installKey(key, #162, false, false, false);
    installKey(key, #122, false, true, true);
    installKey(key, #362, true, false, false);
    installKey(key, #322, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'T';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(37, keyX, keyY, square, tmpLegend, main);
    installKey(key, #164, false, false, false);
    installKey(key, #124, false, true, true);
    installKey(key, #364, true, false, false);
    installKey(key, #324, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'Y';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(38, keyX, keyY, square, tmpLegend, main);
    installKey(key, #171, false, false, false);
    installKey(key, #131, false, true, true);
    installKey(key, #371, true, false, false);
    installKey(key, #331, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'U';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(39, keyX, keyY, square, tmpLegend, main);
    installKey(key, #165, false, false, false);
    installKey(key, #125, false, true, true);
    installKey(key, #365, true, false, false);
    installKey(key, #325, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'I';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(40, keyX, keyY, square, tmpLegend, main);
    installKey(key, #151, false, false, false);
    installKey(key, #111, false, true, true);
    installKey(key, #351, true, false, false);
    installKey(key, #311, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'O';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(41, keyX, keyY, square, tmpLegend, main);
    installKey(key, #157, false, false, false);
    installKey(key, #117, false, true, true);
    installKey(key, #357, true, false, false);
    installKey(key, #317, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'P';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(42, keyX, keyY, square, tmpLegend, main);
    installKey(key, #160, false, false, false);
    installKey(key, #120, false, true, true);
    installKey(key, #360, true, false, false);
    installKey(key, #320, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '{';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := '[';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(43, keyX, keyY, square, tmpLegend, main);
    installKey(key, #133, false, false, false);
    installKey(key, #173, false, true, false);
    installKey(key, #333, true, false, false);
    installKey(key, #373, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '}';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := ']';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(44, keyX, keyY, square, tmpLegend, main);
    installKey(key, #135, false, false, false);
    installKey(key, #175, false, true, false);
    installKey(key, #335, true, false, false);
    installKey(key, #375, true, true, false);

    keyX     := keyX + (2 * skipX);
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'R D';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := 'E E';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight;
            next := makeLegend;
            with next^ do
              begin
                legend  := 'J L';
                x       := tmpLegend^.next^.x;
                y       := tmpLegend^.next^.y + chrHeight;
              end;
          end;
      end;
    key := defineKey(46, keyX, keyY, square, tmpLegend, main);
    installKey(key, #177, false, false, false);
    installKey(key, #377, true, false, false);

  end; {main, row 3}  

  begin {main, row 4}
    keyX     := row4X;
    KeyY     := 3 * skipY;

    KeyX := KeyX + skipX; {skip for the ctrl key}

    keyX     := keyX + skipX + (skipX div 2); {skip for the capsLock key}
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'A';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(53, keyX, keyY, square, tmpLegend, main);
    installKey(key, #141, false, false, false);
    installKey(key, #101, false, true, true);
    installKey(key, #341, true, false, false);
    installKey(key, #301, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'S';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(54, keyX, keyY, square, tmpLegend, main);
    installKey(key, #163, false, false, false);
    installKey(key, #123, false, true, true);
    installKey(key, #363, true, false, false);
    installKey(key, #323, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'D';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(55, keyX, keyY, square, tmpLegend, main);
    installKey(key, #144, false, false, false);
    installKey(key, #104, false, true, true);
    installKey(key, #344, true, false, false);
    installKey(key, #304, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'F';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(56, keyX, keyY, square, tmpLegend, main);
    installKey(key, #146, false, false, false);
    installKey(key, #106, false, true, true);
    installKey(key, #346, true, false, false);
    installKey(key, #306, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'G';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(57, keyX, keyY, square, tmpLegend, main);
    installKey(key, #147, false, false, false);
    installKey(key, #107, false, true, true);
    installKey(key, #347, true, false, false);
    installKey(key, #307, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'H';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(58, keyX, keyY, square, tmpLegend, main);
    installKey(key, #150, false, false, false);
    installKey(key, #110, false, true, true);
    installKey(key, #350, true, false, false);
    installKey(key, #310, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'J';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(59, keyX, keyY, square, tmpLegend, main);
    installKey(key, #152, false, false, false);
    installKey(key, #112, false, true, true);
    installKey(key, #352, true, false, false);
    installKey(key, #312, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'K';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(60, keyX, keyY, square, tmpLegend, main);
    installKey(key, #153, false, false, false);
    installKey(key, #113, false, true, true);
    installKey(key, #353, true, false, false);
    installKey(key, #313, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'L';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(61, keyX, keyY, square, tmpLegend, main);
    installKey(key, #154, false, false, false);
    installKey(key, #114, false, true, true);
    installKey(key, #354, true, false, false);
    installKey(key, #314, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := ':';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := ';';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(62, keyX, keyY, square, tmpLegend, main);
    installKey(key, #73, false, false, false);
    installKey(key, #72, false, true, false);
    installKey(key, #273, true, false, false);
    installKey(key, #272, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '"';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := '''';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(63, keyX, keyY, square, tmpLegend, main);
    installKey(key, #47, false, false, false);
    installKey(key, #42, false, true, false);
    installKey(key, #247, true, false, false);
    installKey(key, #242, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'RETRN';
        x := margin;
        y := (2 * chrHeight) + margin;
      end;
    key := defineKey(64, keyX, keyY, ReturnKey, tmpLegend, main);
    installKey(key, #15, false, false, false);
    installKey(key, #215, true, false, false);

    keyX     := keyX + skipX + (skipX div 2);
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '|';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := '\';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(65, keyX, keyY, square, tmpLegend, main);
    installKey(key, #134, false, false, false);
    installKey(key, #174, false, true, false);
    installKey(key, #334, true, false, false);
    installKey(key, #374, true, true, false);


  end; {main, row 4}  

  begin {main, row 5}
    keyX     := row5X;
    KeyY     := 4 * skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'NO';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend := 'SCRL';
            x := tmpLegend^.x;
            y := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(70, keyX, keyY, square, tmpLegend, main);
    installKey(key, #14, false, false, false);
    installKey(key, #16, true, false, false);

    keyX     := keyX + (3 * skipX);
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'Z';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(72, keyX, keyY, square, tmpLegend, main);
    installKey(key, #172, false, false, false);
    installKey(key, #132, false, true, true);
    installKey(key, #372, true, false, false);
    installKey(key, #332, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'X';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(73, keyX, keyY, square, tmpLegend, main);
    installKey(key, #170, false, false, false);
    installKey(key, #130, false, true, true);
    installKey(key, #370, true, false, false);
    installKey(key, #330, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'C';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(74, keyX, keyY, square, tmpLegend, main);
    installKey(key, #143, false, false, false);
    installKey(key, #103, false, true, true);
    installKey(key, #343, true, false, false);
    installKey(key, #303, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'V';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(75, keyX, keyY, square, tmpLegend, main);
    installKey(key, #166, false, false, false);
    installKey(key, #126, false, true, true);
    installKey(key, #366, true, false, false);
    installKey(key, #326, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'B';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(76, keyX, keyY, square, tmpLegend, main);
    installKey(key, #142, false, false, false);
    installKey(key, #102, false, true, true);
    installKey(key, #342, true, false, false);
    installKey(key, #302, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'N';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(77, keyX, keyY, square, tmpLegend, main);
    installKey(key, #156, false, false, false);
    installKey(key, #116, false, true, true);
    installKey(key, #356, true, false, false);
    installKey(key, #316, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'M';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(78, keyX, keyY, square, tmpLegend, main);
    installKey(key, #155, false, false, false);
    installKey(key, #115, false, true, true);
    installKey(key, #355, true, false, false);
    installKey(key, #315, true, true, true);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '<';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := ',';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(79, keyX, keyY, square, tmpLegend, main);
    installKey(key, #54, false, false, false);
    installKey(key, #74, false, true, false);
    installKey(key, #254, true, false, false);
    installKey(key, #274, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '>';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := '.';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(80, keyX, keyY, square, tmpLegend, main);
    installKey(key, #56, false, false, false);
    installKey(key, #76, false, true, false);
    installKey(key, #256, true, false, false);
    installKey(key, #276, true, true, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '?';
        x := margin;
        y := chrHeight + margin;
        next := makeLegend;
        with next^ do
          begin
            legend  := '/';
            x       := tmpLegend^.x;
            y       := tmpLegend^.y + chrHeight + margin;
          end;
      end;
    key := defineKey(81, keyX, keyY, square, tmpLegend, main);
    installKey(key, #57, false, false, false);
    installKey(key, #77, false, true, false);
    installKey(key, #257, true, false, false);
    installKey(key, #277, true, true, false);

    keyX     := keyX + 3 * skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'LF';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(83, keyX, keyY, square, tmpLegend, main);
    installKey(key, #12, false, false, false);
    installKey(key, #212, true, false, false);

    KeyX := row5X + (3 * skipX);
    keyY := 5 * skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(89, keyX, keyY, spaceKey, tmpLegend, main);
    installKey(key, #40, false, false, false);

  end; {main, row 5}  

  begin {Aux, row 1}
    keyX     := 0;
    keyY     := 0;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'PF1';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(8, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #204, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'PF2';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(9, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #205, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'PF3';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(10, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #206, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'PF4';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(11, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #213, false, false, false);

  end;  {Aux, row 1}

  begin {Aux, row 2}
    keyX     := 0;
    KeyY     := skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '7';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(28, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #236, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '8';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(29, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #237, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '9';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(30, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #240, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '-';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(31, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #223, false, false, false);

  end;  {Aux, row 2}

  begin {Aux, row 3}
    keyX     := 0;
    keyY     := 2 * skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '4';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(47, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #232, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '5';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(48, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #234, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '6';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(49, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #235, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := ',';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(50, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #222, false, false, false);

  end;  {Aux, row 3}

  begin {Aux, row 4}
    keyX    := 0;
    keyY    := 3 * skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '1';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(66, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #227, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '2';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(67, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #230, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '3';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(68, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #231, false, false, false);

    keyX     := keyX + skipX;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := 'ENT';
        x := margin;
        y := (3 * chrHeight) + margin;
      end;
    key := defineKey(69, keyX, keyY, EnterKey, tmpLegend, Aux);
    installKey(key, #214, false, false, false);

  end;  {Aux, row 4}

  begin {Aux, row 5}
    keyX    := 0;
    keyY    := 4 * skipY;
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '0';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(84, keyX, keyY, shiftKey, tmpLegend, Aux);
    installKey(key, #226, false, false, false);

    keyX     := keyX + (2 * skipX);
    tmpLegend:= makeLegend;
    with tmpLegend^ do
      begin
        tmpLegend^.Legend := '.';
        x := margin;
        y := chrHeight + margin;
      end;
    key := defineKey(85, keyX, keyY, square, tmpLegend, Aux);
    installKey(key, #224, false, false, false);

  end;  {Aux, row 5}

    IOCursorMode(TrackCursor);
    IOSetModeTablet(relTablet);
    
  end; {init}

   







procedure DrawKey(aKey: pKeyInfo;  Invert: Boolean);
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to draw a key on the screen.
{    It will reverse the "key shape" that is currently on the
{    screen.
{
{ Parameters:
{    Key is the key that is to be drawn.
{
{    Invert tells us if we are to invert the value of the key.  If
{    it is true then the key will be drawn in black.
{
{
{--------------------------------------------------------------------}
  var 
    XLen, YLen,
    TmpX, TmpY  : Integer;
    Func1, Func2: Integer;
    originX     : Integer;
    originY     : Integer;
    tmpLegend   : pKeyLegend;
      
  begin
    if aKey = nil then exit(drawKey);
    if not Invert then
      begin
        Func1 := RXNor;
        Func2 := RXOr;
      end
     else
      begin
        Func1 := RXOr;
        Func2 := RXNor;
      end;
    if aKey^.keyBrd = main then
      begin 
        originX := mainX;
        originY := mainY;
      end
    else if aKey^.keyBrd = aux then
      begin
        originX := AuxX;
        originY := AuxY;
      end
    else exit(drawKey);

    case aKey^.Shape of
      Square:
        begin {square}
          XLen := XSquareLength;
          YLen := YSquareLength;
        end {Square};
            
      SpaceKey:
        begin {SpaceKey}
          XLen := (skipX * 9) - interKeySpace ;
          YLen := ySquareLength;
        end {SpaceKey};
            
      TabKey:
        begin {TabKey}
          XLen := (SkipX + (skipX div 2)) - interKeySpace;
          YLen := YSquareLength;
        end {TabKey};
            
      ShiftKey:
        begin
          XLen := (SkipX * 2) - interKeySpace;
          YLen := YSquareLength;
        end {ShiftKey};
            
      LockKey :
        begin
          XLen := (skipX + (skipX div 2)) - interKeySpace;
          YLen := YSquareLength;
        end {LockKey};
        
      ReturnKey:
        begin
          XLen := XSquareLength;
          YLen := (2 * skipY) - interKeySpace;
          tmpX := originX + aKey^.X + (skipX div 2);
          tmpY := originY + aKey^.Y - skipY;

          RasterOp(Func1,
            XLen, YLen,
            tmpX, tmpY, SScreenW, SScreenP,
            tmpX, tmpY, SScreenW, SScreenP);

          tmpX := tmpX + (interKeySpace div 2);
          tmpY := tmpY + (interKeySpace div 2);
          RasterOp(Func2,
            XLen - interKeySpace, YLen - interKeySpace,
            tmpX, tmpY, SScreenW, SScreenP,
            tmpX, tmpY, SScreenW, SScreenP);

          XLen := (skipX div 2);
          YLen := ySquareLength;
        end {ReturnKey};
      
      EnterKey:
        begin
          XLen := xSquareLength;
          YLen := (skipY * 2) - interKeySpace;
        end;
      end {Case};
    
    tmpX := originX + aKey^.x;
    tmpY := originY + aKey^.y;
    RasterOp(Func1,
      XLen, YLen,
      tmpX, tmpY, SScreenW, SScreenP,
      tmpX, tmpY, SScreenW, SScreenP);

    tmpX := originX + aKey^.x + (interKeySpace div 2);
    tmpY := originY + aKey^.y + (interKeySpace div 2);  

    if aKey^.shape = returnKey then
      begin
        RasterOp(Func2,
          XLen, YLen - InterKeySpace,
          tmpX, tmpY, SScreenW, SScreenP,
          tmpX, tmpY, SScreenW, SScreenP);
      end
    else
      begin
        RasterOp(Func2,
          XLen - InterKeySpace, YLen - InterKeySpace,
          tmpX, tmpY, SScreenW, SScreenP,
          tmpX, tmpY, SScreenW, SScreenP);
        end;
        
    SChrFunc(RXor);
    tmpLegend := aKey^.keyCap;
    while tmpLegend <> nil do
      begin
        tmpX := originX + aKey^.x + tmpLegend^.x;
        tmpY := originY + aKey^.y + tmpLegend^.y;
        SSetCursor(tmpX, tmpY);
        write(tmpLegend^.legend);
        tmpLegend := tmpLegend^.next;
      end;
    SChrFunc(RRpl);
  end; {DrawKey}
    
   






procedure InitBoard;
{--------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to draw the initial keyboard onto the
{    screen.
{
{---------------------------------------------------------------------}
  Var
    ThisKey: pKeyInfo;
  begin
    ThisKey := mainKeyBoard;
    while thisKey <> nil do
      begin
        DrawKey(ThisKey,False);
        ThisKey := ThisKey^.nextKey;
      end;
    ThisKey := auxKeyBoard;
    while thisKey <> nil do
      begin
        DrawKey(ThisKey,False);
        ThisKey := ThisKey^.nextKey;
      end;
  end;
    







procedure InitCmd;
{-----------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to initialize the menu command table.
{    It will also draw the menu on the screen.
{
{---------------------------------------------------------------}
  Var
    I: Integer;
  begin
    CmdArray[titleCmd].Cmd := TitleString;
    CmdArray[titleCmd].X := titleX;
    CmdArray[titleCmd].Y := titleY;
    CmdArray[BlinkCmd].Cmd := 'Keyboard test.  Single key test.';
    CmdArray[BlinkCmd].X := blinkX;
    CmdArray[BlinkCmd].Y := blinkY;
    CmdArray[holdCmd].Cmd := 'Keyboard test.  All keys stay on after hit.';
    CmdArray[holdCmd].X := holdX;
    CmdArray[holdCmd].Y := holdY;
    CmdArray[QuitCmd].Cmd := 'Quit';
    CmdArray[QuitCmd].X := QuitX;
    CmdArray[QuitCmd].Y := QuitY;
    
    for I := 1 to NumCmds do
      begin
        with CmdArray[I] do
          begin
            SSetCursor(X,Y);
            Write(Cmd);
          end;
      end;
  end;
    







function CmdIndex(InX,InY: Integer): Integer;
{----------------------------------------------------------------------
{
{ Abstract:
{   This function will lookup a command in CmdArray given the X/Y 
{   values from the tablet.
{
{ Parameters:
{   InX and InY give the location of the pen.
{
{ Results:
{   Return the index in CmdArray.  Return -1 if we could not get a
{   match.
{
{-------------------------------------------------------------------}
  Var I: Integer;
  begin
    for I := 1 to NumCmds  do
      begin
        with CmdArray[I] do
          begin
            if ( InX > ( X - 4)) and ( InX < ((Length(Cmd) * 9) + 4)) and
               ( InY > ( Y - 13 - 7)) and ( InY < ( Y + 7)) then
              begin
                CmdIndex := I;
                exit(CmdIndex);
              end;
          end;
      end;
    CmdIndex := -1;
  end;
    







procedure GetInput(Var Ch: keyCode;  Var X,Y: Integer;  Var Pressed: Boolean);
{--------------------------------------------------------------------
{
{ Abstract:
{ This procedure is used to get either a Tablet push or a key stroke.
{
{ Parameters:
{    Ch is the character that is to be read.  This procedure will place
{    the character that is read into this variable.
{
{    X and Y are the X/Y read from the tablet.
{
{    Pressed tells if there has been a pen press.
{
{--------------------------------------------------------------------}
  label
    1;
  Var
    tmp : keyCode;
    Status: Integer;
    LocalTabSwitch: Boolean;


  Handler CtlC;
  {
  {   Throw out control c's.  Note that control/Shift/C still stops it.
  {}
    begin
      IOKeyClear;
      CtrlCPending := false;
      tmp := #343;
      status := IOEIOC;
      goto 1;
    end;

  Handler CtlCAbort;
  {
  {   Throw out control c's.  Note that control/Shift/C still stops it.
  {}
    begin
      IOKeyClear;
      CtrlCPending := false;
      tmp := #343;
      status := IOEIOC;
      goto 1;
    end;


  Handler Dump (null: string);
  {
  {   Throw out control/shift/D's.  
  {}
    begin
    end;


  begin {GetInput}
    repeat
        Status := IOCRead(KeyBoard,recast(tmp, char));
1:      CtrlSPending := false;          {ignore ^S's}
        LocalTabSwitch := TabSwitch;
        Pressed := LocalTabSwitch and ( not OldTabSwitch ); 
        OldTabSwitch := LocalTabSwitch;
      until ( Status = IOEIOC ) or  Pressed;
    IOReadTablet(X,Y);
    ch := tmp;
  end;  {GetInput}








procedure DoKey(aKeyCode: keyCode; HoldKey: boolean);
  const
    napTime = 10;
  var
    tmpKey      : pKey
  begin {DoKey}
    tmpKey  := keyCodes[aKeyCode];
    while tmpKey <> nil do
      begin
        DrawKey(tmpKey^.keyPtr,True);
        tmpKey := tmpKey^.nextKey;
      end;
    if not holdKey then
      begin
        Nap(napTime);
        tmpKey := keyCodes[aKeyCode];
        while tmpKey <> nil do
          begin
            DrawKey(tmpKey^.keyPtr,False);
            tmpKey := tmpKey^.nextKey;
          end;
      end;
  end;  {DoKey}  








function inKey(aKeyCode: keyCode; aKey: pKeyInfo): boolean;
{-----------------------------------------------------------------------
{
{ Abstract:
{
{   If aKey is in the chain hanging from aKey code, return true, else
{   return false.
{
{--------------------------------------------------------------------}
  var
    tmpKey : pKey;
  begin {inKey}
    inKey := false;
    tmpKey := keyCodes[aKeyCode];
    while tmpKey <> nil do
      begin
        if tmpKey^.keyPtr = aKey then
          begin
            inKey := true;
            exit(inKey);
          end;
        tmpKey := tmpKey^.nextKey;
      end;
  end;  {inKey}







procedure TestKeyBoard;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to test the keyboard.  It will read
{    characters from the keyboard until there is a pen press.
{
{--------------------------------------------------------------------}
  var
    Ch      : keyCode;
    Pressed,
    Ctrl    : Boolean;
    Complaint : string;

  begin {TestKeyBoard}   
    while true do
      begin
        GetInput(Ch,TabX,TabY,Pressed);
        if Pressed then exit(TestKeyBoard);
        SSetCursor(XBanner,YBanner);
        Write('                                                             ');
        SSetCursor(XBanner,YBanner);
        Write(chr(Lor(ord(Ch), #200)), '  ', ord(ch):2:16);
        if inKey(ch, ctrlKey) then write('     Control');
        if inKey(ch, shiftLeftKey) then write('     Shifted');
        DoKey(Ch, keyStayOn);
      end;
  end;  {TestKeyBoard}



Handler HelpKey(var s: Sys9s);
  const
    napTime = 10;
  begin {DoKey}
    DrawKey(KbdHelpKey, True);
    if not keyStayOn then
      begin
        Nap(napTime);
        DrawKey(KbdHelpKey, False);
      end;
    s := ' ';
    s[1] := chr(7);
  end;

begin {KeyTest}
  reset(Input);
  rewrite(Output);

  Init;
  InitBoard;
  InitCmd;
  OldTabSwitch := False;
  Pressed := True;
  TabX := CmdArray[2].X;
  TabY := CmdArray[2].Y;

  while True do
    begin
      Index := CmdIndex(TabX,TabY);
      if Index = -1 then
        begin
          sDum := 'Command not recognized.  Please try again.';
          TellUser(sDum);
          Pressed := False;
          while not Pressed do GetInput(Ch,TabX,TabY,Pressed);
          ClearPrompt(length(SDum));
        end
      else
        begin
          X := CmdArray[Index].X;
          Y := CmdArray[Index].Y;
          SDum := CmdArray[Index].Cmd;
          RasterOp(RNot,
            (Length(Sdum) * chrWidth) + (2 * margin), chrHeight + (2 * margin),
            X - margin, Y - chrHeight - margin, SScreenW, SScreenP,
            X - margin, Y - chrHeight - margin, SScreenW,SScreenP);
          case Index of
            TitleCmd:
              begin
              end;
            BlinkCmd:
              begin {blink keys}
                KeyStayOn := False;
                TestKeyBoard;
              end;  {blink keys}
            HoldCmd:
              begin {hold keys}
                KeyStayOn := True;
                TestKeyBoard;
              end;  {hold keys}
            QuitCmd:
              begin {reset window}
                screenReset;
                exit(KeyTest);
              end;  {reset window}
            end {Case};
          InitBoard;
          RasterOp(RNot,
            (Length(Sdum) * chrWidth) + (2 * margin), chrHeight + (2 * margin),
            X - margin, Y - chrHeight - margin, SScreenW, SScreenP,
            X - margin, Y - chrHeight - margin, SScreenW,SScreenP);
        end;
    end;
end. {KeyTest}

