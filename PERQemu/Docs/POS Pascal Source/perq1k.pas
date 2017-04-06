{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program KeyTest;

{---------------------------------------------------------------------------
{ 
{ Abstract: KeyTest - Perq Keyboard test program.
{ 
{   This program is used to test the PERQ Keyboard.  It
{   will put a picture of the keyboard on the screen and then
{   indicate the character that is typed on the keyboard.
{     
{    Don Scelza   8-Nov-80.
{     
{   Copyright (C)  1980, 1981, 1982, 1983 Three Rivers Computer Corporation.
{     
{------------------------------------------------------------------------}

{}
{ 16-Nov-82   1.7 Brad Myers
{     Fixed for landscape. changed name for PERQ 1.
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
{}


Const TitleString = 'KeyTest    Version 1.7  for PERQ 1';

 
imports Screen from Screen;
imports Raster from Raster;
imports Memory from Memory;
imports IO_Others from IO_Others;
imports IO_Unit from IO_Unit;
imports IOErrors from IOErrors;
imports Sleep from Sleep;
imports system from System;

{ KeyInfo is a structure that keeps information about each key on
  the keyboard.
  }
  
type ShapeType = (Square, SpaceKey, TabKey, ShiftKey, LockKey, ReturnKey,
                  BackKey);
                  
                    
type KeyInfo = packed record
    X,Y: Integer;                { X,Y position of left corner of the key }
    Shape: ShapeType;            { The shape of the key }
    Shift: Boolean;              { Is the shift key down }
    Defined: Boolean;            { Is it a valid key }
    TwoChar: Boolean;            { Are there 2 characters on the key }
    XFirst,YFirst: Integer;      { X,Y position of first character on the key}
    XSecond,YSecond: Integer;    { X,Y position of the second character }
    FirstChar,SecondChar: Char;  { If FirstChar is a space don't print }
                                 { any characters. }
    end;
    
    
    

Const StartX = 68;            { X position of left upper corner of board }
      StartY = 150;           { Y position of left upper corner of board }
      
      
Var KeyBrd: Array[0..127] of KeyInfo;


Const XSquareLength = 16;    { Number of pixels for a square in the X dir }  
      YSquareLength = 16;    { Number of pixels for a square in the Y dir }
      InterKeySpace = 8;     { Number of pixels between keys }
      


Var I, J, SaveKeyIntr, X, Y: Integer;
    SDum: String;
    Ch: Char;
    KeyStayOn: Boolean;
    


{ These define the locations in KeyBrd of the special keys. }

Const ShiftLLoc = 0;            { The shift lock key }
      CtrlLoc = 1;              { The control key }
      Shift1Loc = 2;            { Left side shift key }
      Shift2Loc = 3;            { Right side shift key }
      
      
Const XBanner = 300;            { X and Y location of where to print the }
      YBanner = 500;            { info about a pressed key }
      
Var OldTabSwitch, Pressed: Boolean;
    TabX, TabY, Index: Integer;
    

{ This structure is used to hold information about the menu
{ for command processing. } 
   
Type CmdRecord = Packed Record
     Cmd: String;               { The command name. }
     X,Y: Integer;              { X & Y location of the command. }
     end;
     
Const NumCmds = 4;

Var CmdArray: Array[1..NumCmds] of CmdRecord;

{}
procedure DefineKey(Index,InX,InY: Integer;  KeyShape: ShapeType;  
                    AreTwo: Boolean; Char1, Char2: Char;  Shifted: Boolean);
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to set up the Keyboard structure.
{
{ Parameters:
{    Index is the index of the new key in Keyboard.
{    X,Y are the location of the corner of the key.
{    Shape is the shape of the key.
{    AreTwo is true if there are two characters on the key.
{    Char1, Char2 are the two characters that can go onto the key.
{    Shifted tells if the shift key is down.
{
{ Design:
{    This procedure will calculate the positions of the characters on
{    the key.
{
{----------------------------------------------------------------------------}
    begin
    KeyBrd[Index].X := InX;
    KeyBrd[Index].Y := InY;
    KeyBrd[Index].Shape := KeyShape;
    KeyBrd[Index].TwoChar := AreTwo;
    if AreTwo then
        begin
        KeyBrd[Index].XFirst := InX + 11;
        KeyBrd[Index].YFirst := InY + 17;
        KeyBrd[Index].XSecond := InX + 11;
        KeyBrd[Index].YSecond := InY + 30;
        end
    else
        begin
        KeyBrd[Index].XFirst := InX + 11;
        KeyBrd[Index].YFirst := InY + 23;
        KeyBrd[Index].XSecond := InX + 11;
        KeyBrd[Index].YSecond := InY + 23;
        end;
    KeyBrd[Index].FirstChar := Char1;    
    KeyBrd[Index].SecondChar := Char2;
    KeyBrd[Index].Shift := Shifted;
    KeyBrd[Index].Defined := True;
    end;
    
    
        
{}
procedure Init;
{--------------------------------------------------------------------
{ Abstract:
{     This procedure is used to initialize the internal state of the
{     program.
{--------------------------------------------------------------------} 
  var I,X,Y: Integer;    
    begin
    
(*
    SaveKeyIntr := DevTab^[KeyBoard].IntrMask;  {Save the old interrupt value}
    DevTab^[KeyBoard].IntrMask := 0;            {Turn off keyboard interrupts}o
*)

    RasterOp(RXOr,SBitWidth, SBitHeight,0,0,SScreenW,SScreenP,0,0,SScreenW,SScreenP);
    
    for I := 0 to 127 do KeyBrd[I].Defined := False;
    
    X := StartX;
    Y := StartY;
    DefineKey(27,X,Y,Square,False,' ',' ',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(9,X,Y,TabKey,False,' ',' ',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(49,X,Y,Square,True,'1','!',False);
    DefineKey(33,X,Y,Square,True,'1','!',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(50,X,Y,Square,True,'2','@',False);
    DefineKey(64,X,Y,Square,True,'2','@',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(51,X,Y,Square,True,'3','#',False);
    DefineKey(35,X,Y,Square,True,'3','#',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(52,X,Y,Square,True,'4','$',False);
    DefineKey(36,X,Y,Square,True,'4','$',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(53,X,Y,Square,True,'5','%',False);
    DefineKey(37,X,Y,Square,True,'5','%',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(54,X,Y,Square,True,'6','^',False);
    DefineKey(94,X,Y,Square,True,'6','^',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(55,X,Y,Square,True,'7','&',False);
    DefineKey(38,X,Y,Square,True,'7','&',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(56,X,Y,Square,True,'8','*',False);
    DefineKey(42,X,Y,Square,True,'8','*',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(57,X,Y,Square,True,'9','(',False);
    DefineKey(40,X,Y,Square,True,'9','(',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(48,X,Y,Square,True,'0',')',False);
    DefineKey(41,X,Y,Square,True,'0',')',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(45,X,Y,Square,True,'-','_',False);
    DefineKey(95,X,Y,Square,True,'-','_',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(61,X,Y,Square,True,'=','+',False);
    DefineKey(43,X,Y,Square,True,'=','+',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(8,X,Y,BackKey,False,' ',' ',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(21,X,Y,Square,False,' ',' ',False);
    
    X := StartX;
    Y := StartY + ( 2 * YSquareLength ) + InterKeySpace;
    DefineKey(127,X,Y,Square,False,' ',' ',False);
    X := X + ( 5 * XSquareLength ) + ( 2 * InterKeySpace);
    DefineKey(81,X,Y,Square,False,'Q','q',True);
    DefineKey(113,X,Y,Square,False,'Q','q',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(87,X,Y,Square,False,'W','w',True);
    DefineKey(119,X,Y,Square,False,'W','w',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(69,X,Y,Square,False,'E','e',True);
    DefineKey(101,X,Y,Square,False,'E','e',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(82,X,Y,Square,False,'R','r',True);
    DefineKey(114,X,Y,Square,False,'R','r',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(84,X,Y,Square,False,'T','t',True);
    DefineKey(116,X,Y,Square,False,'T','t',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(89,X,Y,Square,False,'Y','y',True);
    DefineKey(121,X,Y,Square,False,'Y','y',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(85,X,Y,Square,False,'U','u',True);
    DefineKey(117,X,Y,Square,False,'U','u',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(73,X,Y,Square,False,'I','i',True);
    DefineKey(105,X,Y,Square,False,'I','i',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(79,X,Y,Square,False,'O','o',True);
    DefineKey(111,X,Y,Square,False,'O','o',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(80,X,Y,Square,False,'P','p',True);
    DefineKey(112,X,Y,Square,False,'P','p',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(125,X,Y,Square,True,'{','}',True);
    DefineKey(123,X,Y,Square,True,'{','}',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(91,X,Y,Square,True,'[',']',False);
    DefineKey(93,X,Y,Square,True,'[',']',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(13,X,Y,ReturnKey,False,' ',' ',False);
    X := X + ( 1 * XSquareLength ) + InterKeySpace;
    DefineKey(124,X,Y,Square,True,'\','|',True);
    DefineKey(92,X,Y,Square,True,'\','|',False);
    
    X := StartX;
    Y := StartY + ( 4 * YSquareLength ) + ( InterKeySpace * 2 );
    DefineKey(7,X,Y,Square,False,' ',' ',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(ShiftLLoc,X,Y,LockKey,False,' ',' ',False);
    X := X + ( 4 * XSquareLength ) + InterKeySpace;
    DefineKey(65,X,Y,Square,False,'A','a',True);
    DefineKey(97,X,Y,Square,False,'A','a',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(83,X,Y,Square,False,'S','s',True);
    DefineKey(115,X,Y,Square,False,'S','s',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(68,X,Y,Square,False,'D','d',True);
    DefineKey(100,X,Y,Square,False,'D','d',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(70,X,Y,Square,False,'F','f',True);
    DefineKey(102,X,Y,Square,False,'F','f',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(71,X,Y,Square,False,'G','g',True);
    DefineKey(103,X,Y,Square,False,'G','g',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(72,X,Y,Square,False,'H','h',True);
    DefineKey(104,X,Y,Square,False,'H','h',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(74,X,Y,Square,False,'J','j',True);
    DefineKey(106,X,Y,Square,False,'J','j',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(75,X,Y,Square,False,'K','k',True);
    DefineKey(107,X,Y,Square,False,'K','k',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(76,X,Y,Square,False,'L','l',True);
    DefineKey(108,X,Y,Square,False,'L','l',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(59,X,Y,Square,True,';',':',False);
    DefineKey(58,X,Y,Square,True,';',':',True);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(34,X,Y,Square,True,'''','"',True);
    DefineKey(39,X,Y,Square,True,'''','"',False);
    X := X + ( 5 * XSquareLength ) + InterKeySpace;
    DefineKey(126,X,Y,Square,True,'~','`',True);
    DefineKey(96,X,Y,Square,True,'~','`',False);
    
    X := StartX;
    Y := StartY + ( 6 * YSquareLength ) + ( InterKeySpace * 3 );
    DefineKey(CtrlLoc,X,Y,Square,False,' ',' ',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(Shift1Loc,X,Y,ShiftKey,False,' ',' ',False);
    X := X + ( 5 * XSquareLength ) + InterKeySpace;
    DefineKey(90,X,Y,Square,False,'Z','z',True);
    DefineKey(122,X,Y,Square,False,'Z','z',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(88,X,Y,Square,False,'X','x',True);
    DefineKey(120,X,Y,Square,False,'X','x',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(67,X,Y,Square,False,'C','c',True);
    DefineKey(99,X,Y,Square,False,'C','c',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(86,X,Y,Square,False,'V','v',True);
    DefineKey(118,X,Y,Square,False,'V','v',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(66,X,Y,Square,False,'B','b',True);
    DefineKey(98,X,Y,Square,False,'B','b',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(78,X,Y,Square,False,'N','n',True);
    DefineKey(110,X,Y,Square,False,'N','n',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(77,X,Y,Square,False,'M','m',True);
    DefineKey(109,X,Y,Square,False,'M','m',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(60,X,Y,Square,True,'<',',',True);
    DefineKey(44,X,Y,Square,True,'<',',',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(62,X,Y,Square,True,'>','.',True);
    DefineKey(46,X,Y,Square,True,'>','.',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(63,X,Y,Square,True,'?','/',True);
    DefineKey(47,X,Y,Square,True,'?','/',False);
    X := X + ( 2 * XSquareLength ) + InterKeySpace;
    DefineKey(Shift2Loc,X,Y,LockKey,True,' ',' ',False);
    X := X + ( 4 * XSquareLength ) + InterKeySpace;
    DefineKey(10,X,Y,Square,False,' ',' ',False);
    
    X := StartX + ( 8 * XSquareLength) + ( 2 * InterKeySpace );
    Y := StartY + ( 8 * YSquareLength ) + ( 4 * InterKeySpace );
    DefineKey(32,X,Y,SpaceKey,False,' ',' ',False);
    
    IOCursorMode(TrackCursor);
    IOSetModeTablet(relTablet);
    
    end;

   
{}

procedure DrawKey(Key: KeyInfo;  Invert: Boolean);
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
  var XLen, YLen, TmpX, TmpY: Integer;
      Func1, Func2: Integer;
    begin
    if not Key.Defined then exit(DrawKey);
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
    case Key.Shape of
        Square: begin
            XLen := XSquareLength * 2;
            YLen := YSquareLength * 2;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP, 
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {Square};
            
        SpaceKey: begin
            XLen := ( XSquareLength * 18) + ( 9 * InterKeySpace );
            YLen := YSquareLength * 2;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP,
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {SpaceKey};
            
        TabKey: begin
            XLen := XSquareLength * 3;
            YLen := YSquareLength * 2;
            TmpX := Key.X;
            TmpY := Key.Y + ( 2 * YSquareLength) + InterKeySpace;
            RasterOp(Func1, XLen, YLen, TmpX, TmpY, SScreenW, SScreenP,
                   TmpX, TmpY, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, TmpX + 2, TmpY + 2,
                   SScreenW, SScreenP, TmpX + 2, TmpY + 2, SScreenW, SScreenP);
            XLen := XSquareLength * 2;
            YLen := YSquareLength * 2 + InterKeySpace;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP,
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {TabKey};
            
        ShiftKey: begin
            XLen := XSquareLength * 5;
            YLen := YSquareLength * 2;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP,
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {ShiftKey};
            
        LockKey : begin
            XLen := XSquareLength * 4;
            YLen := YSquareLength * 2;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP,
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {LockKey};
        
        ReturnKey: begin
            XLen := XSquareLength * 2 + InterKeySpace;
            YLen := YSquareLength * 2;
            TmpX := Key.X - ( 2 * XSquareLength ) + InterKeySpace;
            TmpY := Key.Y + ( 2 * YSquareLength ) + InterKeySpace;
            RasterOp(Func1, XLen, YLen, TmpX, TmpY, SScreenW, SScreenP,
                   TmpX, TmpY, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, TmpX + 2, TmpY + 2,
                   SScreenW, SScreenP, TmpX + 2, TmpY + 2, SScreenW, SScreenP);
            XLen := XSquareLength * 1;
            YLen := YSquareLength * 2 + InterKeySpace;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP,
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {ReutrnKey};
            
        BackKey: begin
            XLen := XSquareLength * 2;
            YLen := YSquareLength * 2;
            RasterOp(Func1, XLen, YLen, Key.X, Key.Y, SScreenW, SScreenP,
                   Key.X, Key.Y, SScreenW, SScreenP);
            RasterOp(Func2, XLen - 4, YLen - 4, Key.X + 2, Key.Y + 2,
                   SScreenW, SScreenP, Key.X + 2, Key.Y + 2, SScreenW, SScreenP);
            end {BackKey};           
        
        end {Case};
        
    if Key.FirstChar <> ' ' then
        begin
        SChrFunc(RXOr);
        SSetCursor(Key.XFirst, Key.YFirst);
        Write(Key.FirstChar);
        if Key.TwoChar then
            begin
            SSetCursor(Key.XSecond,Key.YSecond);
            Write(Key.SecondChar);
            end;
        SChrFunc(RRpl);
        end;
    end;
    
   
{}
procedure InitBoard;
{--------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to draw the initial keyboard onto the
{    screen.
{
{---------------------------------------------------------------------}
Var ThisKey: Integer;
    begin
    for ThisKey := 0 to 127 do DrawKey(KeyBrd[ThisKey],False);
    end;
    
{}

procedure InitCmd;
{-----------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to initialize the menu command table.
{    It will also draw the menu on the screen.
{
{---------------------------------------------------------------}
Var I: Integer;
    begin
    CmdArray[1].Cmd := TitleString;
    CmdArray[1].X := 250;
    CmdArray[1].Y := 700;
    CmdArray[2].Cmd := 'Keyboard test.  Single key test.';
    CmdArray[2].X := 10;
    CmdArray[2].Y := 750;
    CmdArray[3].Cmd := 'Keyboard test.  All keys stay on after hit.';
    CmdArray[3].X := 10;
    CmdArray[3].Y := 800;
    CmdArray[4].Cmd := 'Quit';
    CmdArray[4].X := 10;
    CmdArray[4].Y := 850;
    
    for I := 1 to NumCmds do
        begin
        with CmdArray[I] do
            begin
            SSetCursor(X,Y);
            Write(Cmd);
            end;
        end;
    end;
    
{}

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
    
{}

procedure GetInput(Var Ch: Char;  Var X,Y: Integer;  Var Pressed: Boolean);
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
Var Status: Integer;
    LocalTabSwitch: Boolean;
    begin
    repeat
        Status := IOCRead(KeyBoard,Ch);
        LocalTabSwitch := TabSwitch;
        Pressed := LocalTabSwitch and ( not OldTabSwitch ); 
        OldTabSwitch := LocalTabSwitch;
    until ( Status = IOEIOC ) or  Pressed;
    IOReadTablet(X,Y);
    end;
    
{}
procedure TestKeyBoard;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to test the keyboard.  It will read
{    characters from the keyboard until there is a pen press.
{
{--------------------------------------------------------------------}
var Ch: Char;
    Pressed, Ctrl: Boolean;
    begin    
    while true do
        begin
        GetInput(Ch,TabX,TabY,Pressed);
        if Pressed then exit(TestKeyBoard);
        I := Ord(Ch);
        SSetCursor(XBanner,YBanner);
        Write('                                                             ');
        SSetCursor(XBanner,YBanner);
        Write(Chr(Lor(ord(Ch), #200)),'  ',I:3:8,'   ');
        Ctrl := Land(I,#200) <> 0;
        if Ctrl then
            begin
            Write('Control  ');
            DrawKey(KeyBrd[CtrlLoc],True);
            I := Land(I,#177);
            end;
        if KeyBrd[I].Shift then 
            begin
            Write('Shifted   ');
            DrawKey(KeyBrd[ShiftLLoc],True);
            DrawKey(KeyBrd[Shift1Loc],True);
            DrawKey(KeyBrd[Shift2Loc],True);
            end;
        DrawKey(KeyBrd[I],True);
        if not KeyStayOn then
            begin
            Nap(5);
            DrawKey(KeyBrd[I],False);
            if KeyBrd[I].Shift then
                begin
                DrawKey(KeyBrd[ShiftLLoc],False);
                DrawKey(KeyBrd[Shift1Loc],False);
                DrawKey(KeyBrd[Shift2Loc],False);
                end;
            if Ctrl then DrawKey(KeyBrd[CtrlLoc],False);
            end;
        end;
    end;
{}
    

    


Handler HelpKey(var s: Sys9s);
   begin
   s := ' ';
   s[1] := chr(7);
   end;

begin
reset(Input);
rewrite(Output);

Init;
InitBoard;
InitCmd;
OldTabSwitch := False;
Pressed := False;

(*
while not Pressed do GetInput(Ch,TabX,TabY,Pressed);
*)

Pressed := True;
TabX := CmdArray[2].X;
TabY := CmdArray[2].Y;

while True do
    begin
    Index := CmdIndex(TabX,TabY);
    if Index = -1 then
        begin
        X := 13;
        Y := 1000;
        SSetCursor(X,Y);
        SDum := 'Command not recognized.  Please try again.';
        Write(Sdum);
        RasterOp(RNot,(Length(Sdum) * 9) + 6, 13 + 6, X - 3, Y - 13 - 3,
                 SScreenW, SScreenP, X - 3, Y - 13 - 3,
                 SScreenW, SScreenP);
        Pressed := False;
        while not Pressed do GetInput(Ch,TabX,TabY,Pressed);
        RasterOp(RXor,(Length(Sdum) * 9) + 6, 13 + 6, X - 3, Y - 13 - 3,
                 SScreenW, SScreenP, X - 3, Y - 13 - 3, 
                 SScreenW, SScreenP);
        end
    else
        begin
        X := CmdArray[Index].X;
        Y := CmdArray[Index].Y;
        SDum := CmdArray[Index].Cmd;
        RasterOp(RNot,(Length(Sdum) * 9) + 6, 13 + 6, X - 3, Y - 13 - 3,
                 SScreenW, SScreenP, X - 3, Y - 13 - 3,
                 SScreenW, SScreenP);
        case Index of
            1: ;
            2: begin
               KeyStayOn := False;
               TestKeyBoard;
               end;
            3: begin
               KeyStayOn := True;
               TestKeyBoard;
               end;
            4: begin
               (*   
               DevTab^[KeyBoard].IntrMask := SaveKeyIntr;
               *)
               RasterOp(RXOr,SBitWidth, SBitHeight,0,0,SScreenW, SScreenP,0,0,SScreenW, SScreenP);
               SSetCursor(10,500);
               exit(KeyTest);
               end;
            end {Case};
        InitBoard;
        RasterOp(RNot,(Length(Sdum) * 9) + 6, 13 + 6, X - 3, Y - 13 - 3,
                 SScreenW, SScreenP, X - 3, Y - 13 - 3, 
                 SScreenW, SScreenP);
        end;
    end;
end.

