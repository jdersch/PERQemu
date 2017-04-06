{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module Screen;
{---------------------------------------------------------------}
{ Perq Screen Driver                                            }
{ Written By: Miles A. Barel    July 1, 1980                    }
{             Three Rivers Computer Corporation                 }
{             Pittsburgh, PA 15213                              }
{ Abstract: Provides the interface to the PERQ screen including }
{           rudimentary support for multiple windows            }
{---------------------------------------------------------------}

{$Version 4.4 for POS}
{----------------------------------------------------------------
   Change Log:

  Version  4.4  29-Mar-83  Brad A. Myers  Fixed bug with BS over LF
  Version  4.3   1-Mar-83  Brad A. Myers  Fixed title line so doesn't need to
                                            turn off interrupts.
                                          Fixed bug with backspacing over chars
                                            with high bit set.
  Version  4.2  22-Feb-83  Brad A. Myers  Changed import of Configuration to
                                            private part.
                                          Export KSetSLen.
                                          Fix bug in landscape InitScreen.
  Version  4.1  14-Feb-83  Brad A. Myers  Added global SCurBitHeight.
                                          Increase number of windows to 32.
                                          If landscape, release some memory if
                                            not enough on init.
  Version  4.0   9-Feb-83  Brad A. Myers  Made to work with a landscape screen.
                                          Increased the number of windows to 25
                                          Fixed bug about showing cursor when
                                            cursor character not defined.
  Version  3.13  4-Feb-83  Chuck Beckett  ICL's version of screen.pas to
                                          clear up a few problems.  This
                                          Module is identical to the one
                                          recieved from ICL via August
                                          Reinig.
  Version  3.12 20-Jan-82  Brad A. Myers  Fix title line to quote characters
                                          Fixed cursorOutside exception from
                                              ChangeTitle after SSetSize.
                                          Fixed SBackSpace comment.
  Version  3.11 13-Aug-81  Brad A. Myers  Raise an exception if SSetCursor
                                            outside a window
  Version  3.10 26-Jun-81  Brad A. Myers  Export the Window Table
  Version  3.9  26-Jun-81  Brad A. Myers  Export all internal procedures
                                             used by SPutChr for XSPutChr
                                          Add new procedure for window change
                                             to full size.
                                          Fixed bug when ask for current window
                                             size.
                                          Fixed bug in SSetSize.
  Version  3.8  23-May-81  John P. Strait Use IOKeyDisable in ChangeTitle.
  Version  3.7  13-May-81  Brad A. Myers  Added new exceptions
  Version  3.6  17-Mar-81  Brad A. Myers  Changed SClearChar and SBackSpace to
                                           take a char; made name compatible
  Version  3.5   3-Mar-81  Brad A. Myers  Removed BS special char and added
                                           SBackSpace and SClearChar procedures
                                           Removed SRasterOp
                                           and SLine and SIsCurOn; SCursorOn
                                           vble exported
  Version  3.4  24-feb-81  Brad A. Myers  Added return param to GetWindowParms
                                           to tell HasTitle;
  Version  3.3  24-feb-81  Brad A. Myers  Added param to SSetSize to leave
                                           screen displayed
  Version  3.2  20-feb-81  Brad A. Myers  Changed to have a fixed number of
                                           windows; added refresh command and
                                           SIsCurOn; fixed dot in lower-right
                                           corner
  Version  3.1  19-feb-81  Brad A. Myers  BS to previous line added; restrict
                                           number of chars printed on line to
                                           255;
  Version  3.0  18-feb-81  Brad A. Myers  Added Window relative RasterOp and
                                           Line routines; changed windows to
                                           be a list rather that record;
                                           changed module name; added extra
                                           scan line to title and margin below;
                                           changed to separate CR-LF
  Version  2.2   9-Feb-81  Brad A. Myers  fixed broken screen size; make screen
                                           export SScreenP and SScreenW
  Version  2.1  14-Jan-81  Brad A. Myers  fixed cursor bug
  Version  1.0   1-Jul-80  Miles A. Barel started
----------------------------------------------------------------}

Exports

Imports Raster from Raster;

Const   ScreenVersion = 'V4.4';
        VarWin = false;  {if true then can have an arbitrary number of windows
                          and storage for them has to be allocated off a heap.
                          If false then there are 17 windows max, and
                          storage is in screens global data. 
                          NOTE: There are still bugs in VarWin true}

Type
        FontPtr = ^Font;
        Font = Packed Record    { Contains character sets }
                Height:integer; { Height of the KSet }
                Base: integer;  { distance from top of characters to base-line }
                Index: Array [0..#177] of { Index into character patterns }
                    Packed Record case boolean of
                      true: (Offset: 0..767; { position of character in patterns }
                        Line: 0..63;    { Line of patterns containing char }
                        Width: integer); { Width of the character }
                       false:(Loc:integer; Widd: integer)
                    end;
                Filler: array[0..1] of integer;
                Pat: Array [0..0] of integer;  { patterns go here }
                                        { We turn off range checking to }
                                        { access patterns, hence allowing }
                                        { KSets of different sizes }
            end;
{$ifc VarWin then}
      WindowP = ^WindowType;
{$endc}
      WindowType = Packed Record
{$ifc VarWin then}
                       winNumber: Integer;  {this window number}
{$endc}
                       winBY, winTY, winLX, winRX,   { Limits of window area }
                       winHX, winHY, winMX, winMY,   { Limits of useable area }
                       winCurX, winCurY, winFunc: integer;
                       winKSet: FontPtr;
                       winCrsChr: char;
                       winHasTitle, winCursorOn, defined: boolean;
{$ifc VarWin then}
                       winNext: WindowP;
{$endc}
                       end;
{$ifc VarWin then}
Const   MaxWIndx = 32767;
{$elsec}
Const   MaxWIndx = 32;
{$endc}

Type    WinRange = 0..MaxWIndx;        
        LineStyle = (DrawLine,EraseLine,XorLine);

        LS = String[255];
        
Const PortraitWordWidth = 48;
      PortraitBitWidth = 768;
      PortraitBitHeight = 1024;

Const LandscapeWordWidth = 80;
      LandscapeBitWidth = 1280;
      LandscapeBitHeight = 1024;
      
      TitStrLength = 255; {big so can fit lots of characters across a landscape
                           screen.  The actual number allowed now is in the
                           variable SNumTitleChars}
                                               
      KSetSLen = 48;          { Scan Line Length used by Fonts }

Type  STitStrType = String[TitStrLength];

Var   SScreenW: Integer;   {word width of screen; use when want Screen
                            in RasterOp or Line}
      SScreenP: RasterPtr; {for use when want Screen in RasterOp or Line}
      SBitWidth: Integer;  {bit width of current screen}
      SBitHeight: Integer; {bit height of current screen}
      SMaxBitHeight: Integer;  { maximum possible bit height for this screen
                                type; <> SBitHeight if screen shrunk at start
                                up }
      SIsLandScape: boolean; {true if landscape, else false}
      SNumTitleChars: Integer;  {number of characters that will fit in a full
                                 width title line USING THE STANDARD FONT}
      SCursorOn: boolean;
      SFunc: integer;      { Raster-op function for SPutChr }

      SCurBitHeight: Integer;  { current BitHeight.  Will always be 
                                 <= SBitHeight;  will be < if current screen
                                 shrunk }

{$ifc VarWin then}
      FirstWindp,             { first window's pointer; better not be NIL }
      CurWindp: WindowP;      { current window's pointer }
{$elsec}
      CurWind: WinRange;
      WinTable: Array[WinRange] of WindowType;
{$endc}

Procedure ScreenInit;           { CALL THIS ONCE AT BOOT }
Procedure ScreenReset;          { This procedure de-allocates storage for
                                  all windows and sets up the default window. }
Procedure SPutChr(CH:char);     { put character CH out to current position }
                                { on the screen.  Chars FF, CR, and LF }
                                { have special meanings unless #200 bit set: }
                                {       FF - clear screen               
                                {       CR - move left to margine
                                {       LF - move vertically down one 
                                {       BS - erase previous character   }
Procedure SSetCursor(X,Y: integer);     { Set Cursor Position to X,Y }
Procedure SReadCursor(var X,Y: integer);{ Read Cursor Position }
Procedure SCurOn;                       { Enable display of Cursor }
Procedure SCurOff;                      { Disable display of Cursor }
Procedure SCurChr(C: char);             { Set cursor character }
Procedure SChrFunc(F: integer);         { Set raster-op function for SPutChr }
Procedure SSetSize(Lines: integer; complemented, screenOff: Boolean);
                                        { Set Screen Size; lines must be a
                                          multiple of 128; screenOff if true
                                          turns off display in part below lines
                                          in which case, complemented 
                                          describes off part of screen }
Procedure CreateWindow(WIndx: WinRange;
                       OrgX, OrgY, Width, Height: integer; Title: STitStrType);
Procedure ChangeWindow(WIndx: WinRange);
Procedure GetWindowParms(var WIndx: WinRange;
          var OrgX, OrgY, Width, Height: integer; var hasTitle: Boolean);
Procedure ChangeTitle(Title: STitStrType);
Procedure SetFont(NewFont: FontPtr);
Function GetFont: FontPtr;

Procedure SClearChar(c: Char; funct: Integer); {delete prev char}
                                       { c BETTER NOT be CR or LF}
Procedure Line(Style: LineStyle; X1, Y1, X2, Y2: integer; Origin: RasterPtr);
Procedure SVarLine(Style: LineStyle; X1, Y1, X2, Y2, Width: integer;
                   Origin: RasterPtr);
Procedure SBackSpace(c: Char); {move back over last char of curLine}
                                       { c BETTER NOT be CR or LF}
Procedure RefreshWindow(WIndx: WinRange); {redraws window outline and title
                                            area. DOES NOT REDRAW TITLE}
Procedure StartLine;
Procedure ToggleCursor;
Procedure NewLine;
Procedure SaveLineEnd(x: Integer);
Procedure SFullWindow;


Exception WBadSize; {parameter to SSetSize bad}
{-----------------------------------------------------------------------------
 Abstract: Raised if the lines parameter to SSetSize is not a multiple of
           128 or is <=0.  Also raised if a window is totally below area to
           release so will disappear then if window # 0 or is the current
           window, then Raises WBadSize.
-----------------------------------------------------------------------------}

Exception BadWNum;  {indx is invalid}
{-----------------------------------------------------------------------------
 Abstract: Raised if a window number parameter is illegal (not defined or out
           of range.
-----------------------------------------------------------------------------}

Exception WTooBig;
{-----------------------------------------------------------------------------
 Abstract: Raised if parameters for new window specify an area that would
            extend off screen.
-----------------------------------------------------------------------------}

Exception CursOutSide;
{-----------------------------------------------------------------------------
 Abstract: Raised if try to set the cursor outside of the current window.
 Resume:  Allowed.  If resume, then cursor is NOT moved (same effect as if
          signal is caught but not resumed).  
-----------------------------------------------------------------------------}


Private

{$R-}

Imports IO_Others from IO_Others;
Imports Except from Except;
Imports System from System;
Imports Memory from Memory;
Imports IO_Unit from IO_Unit;
Imports Configuration from Configuration;
                     
{$ifc VarWin then}
Imports Memory from Memory;
{$endc}

Const {$Include PERQ.Qcodes.dfs}

Const   MaxLineLength = 255;    { Max characters allowed on a line }
        FF = chr(#14);          { Character used to Clear the screen and }
                                { Home the cursor }
        CR = Chr(13);           { Character used to indicate new line }
        LF = Chr(10);
        BS = chr(#10);          { BackSpace character }
        Bell = chr(#7);
        Del = chr(#177);
        

const   Indent = 5;
        LineIndent = 3;
        XMin = 0;
        YMin = 0;
        MinAreaSize = 27 + 2*Indent; {at least 2 lines of text}
Var

        SBottomY,STopY,         { Limits of Window Area }
        SLeftX, SRightX,
        HomeX,HomeY,            { Limits of usable Area (where chars go) }
        MaxX,MaxY: integer;
        YMax: Integer;  { current CurBitHeight-1;  will be < BitHeight
                          if current screen shrunk }
        HasTitle: Boolean;      { Window Has a Title }
        CurX,                   { Current Cursor Position (cursor points at }
        CurY:integer;           { base line where next character is to be   }
                                { inserted ) }
        KSet: FontPtr;          { This is the current character set }
        CrsChr: char;           { Cursor character }
        LineEnds: Array[0..9] of Integer;  {the last 10 line X ends}
        LastLineEnd: integer;   {index into LineEnd array where next will go}


{$ifc VarWin then}
Procedure FindOrMakeWindow(WIndx: WinRange);
{-----------------------------------------------------------------------------
 Abstract: Searches window list for window corresponding to WIndx and creates
          it if not there.
 Parameters: Windx is new window's index
 SideEffects: Sets CurWindp to pointer for WIndx; may add new element to END of
              window table;
 Environment: Assumes FirstWindP not NIL;
-----------------------------------------------------------------------------}
  var curP, lastp: WindowP;
  begin
  curP := FirstWindP;
  repeat
     if curP^.WinNumber = Windx then begin
                                 CurWindP := curP;
                                 exit(FindOrMakeWindow);
                                 end;
     lastP := curP;
     curP := curP^.winNext;
  until curP = NIL;
 {here new window not found and lastP is last element of list}
  NEW(CurWindP);
  lastP^.winNext := CurWindP;
  CurWindP^.winNumber := WIndx;
  CurWindP^.winNext := NIL;
  end; {FindOrMakeWindow}
{$endc}


Procedure StartLine;
{-----------------------------------------------------------------------------
 Abstract: Resets Curline and variables describing the current line start.
-----------------------------------------------------------------------------}
  begin
  lastLineEnd := 0;
  end; {StartLine}

  
Procedure ToggleCursor;
{-----------------------------------------------------------------------------
 Abstract: Inverts Cursor picture.
 SideEffects: Changes the picture on the screen;
-----------------------------------------------------------------------------}
  var Trik: record case boolean of
              true: (F: FontPtr);
              false:(Seg,Ofst: integer)
          end;
begin
with KSet^.Index[ord(CrsChr)] do
    if ((CurX + Width) < MaxX) and (width > 0) then
        { Only show the cursor if it will fit on the current line and is a 
          valid character }
       with Trik do
         begin
         F:=KSet;
         RasterOp(RXor,Width,KSet^.Height,CurX-1,CurY-KSet^.Base,SScreenW,
                       SScreenP,Offset,Line*KSet^.Height,
                       KSetSLen,MakePtr(Seg,Ofst+#404,FontPtr));
         end;
end { ToggleCursor };


Procedure SSetCursor(x,y: integer);
{-----------------------------------------------------------------------------
 Abstract: Moves the cursor to the specified screen position.
 Parameters: x and y are Screen position where the next char will go.  Note
             that y specified the BOTTOM of the character.
 SideEffects: Changes the cur char positions AND sets line to be empty (so BS
              won't work);
 Errors: Raises CursOutside if try to set the cursor outside the current window
-----------------------------------------------------------------------------}
begin
if (X >= HomeX) and (X <= MaxX) and (Y >= HomeY) and (Y <= MaxY) then
    begin
    if SCursorOn then ToggleCursor;
    CurX:=X;
    CurY:=Y;
    StartLine;
    if SCursorOn then ToggleCursor
    end
else Raise CursOutside;
end { SSetCursor };


Procedure SReadCursor(var x,y:integer);
{-----------------------------------------------------------------------------
 Abstract: Returns the current screen coords for chars.
 Parameters: x and y are set to the Screen position where the next char will go
-----------------------------------------------------------------------------}
begin
x:=CurX;
y:=CurY
end { SReadCursor };


Procedure SCurOn;
{-----------------------------------------------------------------------------
 Abstract: Turns the char cursor on.
 SideEffects: Changes SCursorOn global vble
-----------------------------------------------------------------------------}
begin
if not SCursorOn then
  begin
  SCursorOn:=true;
  ToggleCursor
  end
end;


Procedure SCurOff;
{-----------------------------------------------------------------------------
 Abstract: Turns the char cursor off.
 SideEffects: Changes SCursorOn global vble
-----------------------------------------------------------------------------}
begin
if SCursorOn then
  begin
  ToggleCursor;
  SCursorOn:=false
  end
end;


Procedure SCurChr(C: char);
{-----------------------------------------------------------------------------
 Abstract: Set the character to be used as the cursor.
 SideEffects: Changes the cursor character
-----------------------------------------------------------------------------}
begin
if SCursorOn then ToggleCursor;
CrsChr:=C;
if SCursorOn then ToggleCursor;
end;


Procedure SChrFunc(F: integer);
{-----------------------------------------------------------------------------
 Abstract: Set the function to be used for drawing chars to the screen.
 SideEffects: Changes the char function
-----------------------------------------------------------------------------}
begin
SFunc:=F;
end;


Procedure SSetSize(Lines: integer; complemented, screenOff: Boolean);
{-----------------------------------------------------------------------------
 Abstract: Change the size of the screen so rest of memory can be used
           for other things (if smaller)
 Parameters: Lines is the number of lines in the displayed part of the screen.
             It must be a multiple of 128 and > 0. Complemented describes
             the off part of the screen and screenOff determines whether it
             is displayed (false) or not; if displayed then complemented
             determines whether it is erased white or black.
 Errors: if lines a bad value then Raises WBadSize.
         If a window is totally below area to release and will disappear
           then if window # 0 or is the current window, then Raises WBadSize.
 SideEffects: Changes the values describing windows.  If a window is totally
               below area to release and will disappear then if not 
               window # 0 or is the current window, then makes the window
               undefined.
-----------------------------------------------------------------------------}
  var Dist, i, f:integer;
begin

{$ifc VarWin then}
 {&&&&&&&} This procedure needs to be fixed for VarWin 
{$endc}

{SCurBitHeight holds the current screen size; yMax is SCurBitHeight - 1}

if (Lines mod 128 <> 0) or (Lines <= yMin) then Raise WBadSize;

if (Lines > SBitHeight) then Lines := SBitHeight;

if Lines < yMax then {making screen smaller}
  begin
  for i := 0 to MaxWindX do
   if WinTable[i].defined then
     if WinTable[i].winTY > lines-minAreaSize then {window will disappear}
         if (i = 0) or (i = curWind) then Raise WBadSize
         else WinTable[i].defined := false
     else if WinTable[i].winBY >= lines then {shorten window}
        begin
        WinTable[i].winBY := lines-1-LineIndent;
        WinTable[i].winMY := lines-1-LineIndent-indent;
        if WinTable[i].winCurY > lines then
            WinTable[i].winCurY := WinTable[i].winHY+WinTable[i].winKSet^.Base;
        end;
   with KSet^ do
        begin
        if CurY + Height - Base >= Lines-1-Indent then
            begin
            if SCursorOn then ToggleCursor;
            Dist:=Lines-HomeY-1-indent;
          {move screen with cursor above lines }
            RasterOp(RRpl, MaxX-HomeX+1,Dist,HomeX,
               HomeY,SScreenW,SScreenP,HomeX,CurY-dist,SScreenW,SScreenP);
            SSetCursor(CurX,Lines-1-indent);
            if SCursorOn then ToggleCursor;
            end;
        end;
   SBottomY:=Lines-1-LineIndent;
   MaxY := Lines-1-indent;
   IOSetCursorPos(0,-1);  {in case was at bottom}
   RefreshWindow(curWind);
   StartLine;
   ChangeSize(ScreenSeg, Shrink((stretch(Lines) * SScreenW) div 256));
  {erase part of screen between "lines" and "YMax"}
   if complemented then f := RXNor
   else f := RXor;
   RasterOp(f, SBitWidth, YMax-Lines+2, 0, Lines-1, SScreenW, SScreenP,
                                        0, Lines-1, SScreenW, SScreenP);
   YMax := Lines - 1;
   SCurBitHeight := Lines;
   if screenOff then IOScreenSize(lines,complemented);
   end {making smaller}
else if lines > yMax+1 then {making bigger}
   begin
   ChangeSize(ScreenSeg, Shrink((stretch(Lines) * SScreenW) div 256));
   SBottomY:=Lines-1-LineIndent;
   MaxY := Lines-1-indent;
 {erase whatever there before including lower line}
   RasterOp(RXor,SBitWidth, Lines-YMax-1+Indent, 0, YMax+1-Indent,
               SScreenW,SScreenP, 0, YMax+1-Indent,SScreenW,SScreenP);
   YMax := Lines - 1;
   SCurBitHeight := Lines;
   RefreshWindow(curWind);
   StartLine;
   if screenOff then IOScreenSize(lines,complemented)
   else IOScreenSize(SMaxBitHeight, false);
   end
else if screenOff then IOScreenSize(lines,complemented) {same size}
     else IOScreenSize(SMaxBitHeight, false);
end { SSetSize };


Procedure NewLine;
{-----------------------------------------------------------------------------
 Abstract: Moves the cursor to the next line scrolling if necessary; DOES NOT
           do a CR
 SideEffects: Changes the cursor position and may scroll
-----------------------------------------------------------------------------}
begin
with KSet^ do   { Now for the Line-Feed }
    if CurY + Height + (Height - Base) > MaxY then
        begin   { Yes, we must scroll }
        RasterOp(RRpl,SRightX-SLeftX+1,SBottomY-STopY-Height,SLeftX,STopY+1,SScreenW,
               SScreenP,SLeftX,STopY+Height+1,SScreenW,
               SScreenP);
        RasterOp(RXor,SRightX-SLeftX+1,SBottomY-CurY+Base+1,SLeftX,CurY-Base,
               SScreenW,SScreenP,SLeftX,CurY-Base,
               SScreenW,SScreenP);
        end
    else
        CurY:=CurY+Height;      { No need to scroll, just position the cursor }
end { NewLine };


Procedure SaveLineEnd(x: Integer);
{-----------------------------------------------------------------------------
 Abstract: Saves x as the end of a line
 Parameters: x is the xPos of the end of a line
 SideEffects: puts x at the end of LineEnds table; increments lastLineEnd;
              if table is full then scrolls table 
-----------------------------------------------------------------------------}
  var i: integer;
  begin
  if lastLineEnd = 10 then
    begin
    for i := 0 to 8 do
       LineEnds[i] := LineEnds[i+1];
    lastLineEnd := 9;
    end;
  LineEnds[lastLineEnd] := x;
  lastLineEnd := lastLineEnd+1;
  end; {SaveLineEnd}


Procedure ClearScreen;
{-----------------------------------------------------------------------------
 Abstract: Clears screen of this window and home's cursor
 SideEffects: Erase current window; home's cursor; 
-----------------------------------------------------------------------------}
begin
RasterOp(RXor,SRightX-SLeftX+1,SBottomY-STopY+1,SLeftX,STopY,SScreenW,
       SScreenP,SLeftX,STopY,SScreenW,SScreenP);
CurX:=HomeX;
CurY:=HomeY+KSet^.Base;
StartLine;
if SCursorOn then ToggleCursor
end { ClearScreen };


Procedure SBackSpace(c: Char);
{-----------------------------------------------------------------------------
 Abstract: Move the cursor back over c; for CR or LF, backs up over graphic
           (will not just jump to previous line).
 Parameters: c is the character to backspace over.
 SideEffects: Moves the cursor back the width of char c;
              (DOES NOT ERASE CHAR)
-----------------------------------------------------------------------------}
var i, tempY, tempX: Integer;
begin
c := chr(LAnd(ord(c),#177));
if SCursorOn then ToggleCursor;
{ Back up the cursor to the beginning of the character }
tempX := CurX - KSet^.Index[ord(c)].Width;
if (tempX < HomeX) then {have to go to previous line}
   if lastLineEnd > 0 then {have a line to go to}
        begin
        tempX := LineEnds[lastLineEnd-1] - KSet^.Index[ord(c)].Width;
        lastLineEnd := lastLineEnd-1;
        tempY := CurY-Kset^.Height;
        if tempY > homeY then begin
                              curY := tempY;
                              curX := tempX;
                              end;
        end
   else  {can't do anything}
else curX := tempX; {not beginning of line; just back up}
if SCursorOn then ToggleCursor
end { SBackSpace };


Procedure SClearChar(c: char; funct: Integer);
{-----------------------------------------------------------------------------
 Abstract: Deletes the c from screen;
           c BETTER NOT be CR or LF
 Parameters: c is char to be erased; funct is RasterOp function to use in
             deleting char.  It should be RXor if chars are black on white
             and RXNor if chars are white on black.
 SideEffects: erases the last char of line;
-----------------------------------------------------------------------------}
   var curFunct: Integer;
   begin
   if (c = LF) or (c = CR) then exit(SClearChar);
   SBackSpace(c);
   curFunct := SFunc;
   SFunc := funct;
   SPutChr(c);
   SFunc := curFunct;
   SBackSpace(c);
   end;


Procedure SPutChr(CH: Char);
{-----------------------------------------------------------------------------
 Abstract: Write a char into the current window
 Parameters: Ch is char to write.  If #200 bit is not set, checks to see if
             char is one of Bell, BS, FF, LF, CR and does something special.
 SideEffects: Writes char to screen, moves cursor; may do a NewLine (and
              scroll) if at end of Line
-----------------------------------------------------------------------------}
  var Trik: record case boolean of
              true: (F: FontPtr);
              false:(Seg,Ofst: integer)
          end;
begin
case CH of
    Bell: IOBeep;
    FF:   ClearScreen;
    LF:   begin
          if SCursorOn then ToggleCursor;
          NewLine;
          StartLine;
          if SCursorOn then ToggleCursor;
          end;
    CR:   SSetCursor(homeX, CurY); {does StartLine}
    otherwise:
        begin
        Ch:=chr(LAnd(ord(CH),#177));
        with KSet^.Index[ord(Ch)] do
            if Width > 0 then
                begin
                if SCursorOn then ToggleCursor;
                if (CurX + Width) > MaxX then begin { Auto-CrLf is Needed }
                                              SaveLineEnd(curX);
                                              CurX := HomeX; {CR}
                                              NewLine;       {LF}
                                              end;
                with Trik do
                    begin
                    F:=KSet;
                RasterOp(SFunc,Width,KSet^.Height,CurX,CurY-KSet^.Base,SScreenW,
                       SScreenP,Offset,Line*KSet^.Height,
                       KSetSLen,MakePtr(Seg,Ofst+#404,FontPtr))
                    end;
                CurX:=CurX+Width;
                if SCursorOn then ToggleCursor
                end
        end
    end { case }
end { SPutChr };


Procedure ShowChar(func: Integer; var x: Integer; y, c: Integer);
{-----------------------------------------------------------------------------
 Abstract: Displays the character c at the position x,y on the screen using
           function func.  Increments x by width of character.
-----------------------------------------------------------------------------}
  var Trik: record case boolean of
              true: (F: FontPtr);
              false:(Seg,Ofst: integer)
          end;
  begin
  with KSet^.Index[c] do
    if (width > 0) then
       with Trik do
         begin
         F:=KSet;
         RasterOp(func,Width,KSet^.Height,x,y-KSet^.Base,SScreenW,
                       SScreenP,Offset,Line*KSet^.Height,
                       KSetSLen,MakePtr(Seg,Ofst+#404,FontPtr));
         x := x + width;
         end;
  end { ShowChar };


Procedure ChangeTitle(Title: STitStrType);
{-----------------------------------------------------------------------------
 Abstract: Changes the title of the current window (and displays new one).
 Parameters: Title is new string.  Characters in it are quoted so special
             characters will be displayed.   
 SideEffects: Changes title on screen.
 Design: Doesn't use SPutChr so doesn't need to change window specifications
         so CAN be interupted.
-----------------------------------------------------------------------------}
  var oldKeyEnable: boolean;
      x,y,i,c: Integer;
  label 1;
  begin
  if HasTitle then
    begin
    IOKeyDisable(OldKeyEnable); {no control C's}
    for i := length(title)+1 to TitStrLength do
        title[i] := ' ';
    with KSet^ do
       begin
       y := HomeY-Height-3+Base;
       x := HomeX;
       end;
    for i:= 1 to TitStrLength do
        begin
        c := LAnd(ord(Title[i]),#177);
        if (x + KSet^.Index[c].Width <= MaxX) then
            ShowChar(RNot,x,y,c) {increments x}
        else goto 1;
        end;
   1: IOKeyEnable(OldKeyEnable);
    end;
end { ChangeTitle };


Procedure CreateWindow(WIndx: WinRange;
                         OrgX,OrgY,Width,Height: integer; Title:STitStrType);
{-----------------------------------------------------------------------------
 Abstract: Creates new window for Windx (or overwrites old values for that
           window) and makes it the current window.  Writes title (IN CURRENT
           FONT) if title <> '';
 Parameters: WIndx is index to use for the window created;  OrgX and OrgY are
             the upper left corner of the outside of the new window (chars
             will be at least 5 bits in from that).  Width and Height are
             total outside values for window (NOT the width and height of
             the character area).   Title is title for window. If not '' then
             hairlines and a black area are put around window.   
 SideEffects: Writes current values into current window; creates a new window
               and erases its area on screen               
 Errors: Raises BadWNum if WIndx invalid 
         Raises WTooBig if window would extend off the screen
-----------------------------------------------------------------------------}
  var i:integer; NoOverFlo: boolean;
begin
inlinebyte( INTOFF );
{ Save Old State }
if (WindX > MaxWIndx) or (WIndx < 0) then Raise BadWNum;
{$ifc VarWin then}
with curWindP^ do {may be worthless if creating current window}
{$elsec}
With WinTable[curWind] do
{$endc}
   begin
   winBY := SBottomY; winTY := STopY; winLX := SLeftX; winRX := SRightX;
   winHX := HomeX; winHY := HomeY; winMX := MaxX; winMY := MaxY;
   winCurX := CurX; winCurY := CurY; winFunc := SFunc;
   winKSet := KSet; winCrsChr := CrsChr;
   winHasTitle := HasTitle;
   winCursorOn := SCursorOn;
   defined := true;
   end;
{$ifc VarWin then}
FindOrMakeWindow(Windx);
{$elsec}
curWind := WIndx;
winTable[WIndx].defined := true;
{$endc}
SCurOff;
if (OrgX < XMin) or (OrgX+2*Indent+MinAreaSize > SBitWidth-1) or
   (OrgY < YMin) or
   (OrgY+2*Indent+MinAreaSize > YMax) or (Width+OrgX > SBitWidth) or
   (Height+OrgY > YMax+1) then Raise WTooBig;

if Height < MinAreaSize then Height:=MinAreaSize;
if Width < MinAreaSize then Width:=MinAreaSize;
STopY:=OrgY;
SLeftX:=OrgX;
SBottomY:=OrgY+Height-1;
SRightX:=OrgX+Width-1;
HomeX:=SLeftX+Indent;
HomeY:=STopY+Indent;
MaxX:=SRightX-Indent;
MaxY:=SBottomY-Indent;
ClearScreen; {sets curX and curY}
STopY:=STopY+LineIndent;
SBottomY:=SBottomY-LineIndent;
SLeftX:=SLeftX+LineIndent;
SRightX:=SRightX-LineIndent;
if (Width <> SBitWidth) or (Height <> SBitHeight) or (length(Title) <> 0) then
    begin
    Line(DrawLine,SLeftX-1,STopY-1,SRightX+1,STopY-1,SScreenP);
    Line(DrawLine,SLeftX-1,SBottomY+1,SRightX+2,SBottomY+1,SScreenP);
    Line(DrawLine,SLeftX-1,STopY-1,SLeftX-1,SBottomY+1,SScreenP);
    Line(DrawLine,SRightX+1,STopY-1,SRightX+1,SBottomY+1,SScreenP);
    if Length(Title) <> 0 then
        begin
        HasTitle:=true;
        with KSet^ do
            begin
            RasterOp(RXNor, SRightX-SLeftX + 1, KSet^.Height+2, SLeftX, STopY,
                     SScreenW, SScreenP, SLeftX, StopY, SScreenW, SScreenP);
            STopY:=STopY+Height+2;
            HomeY:=HomeY+Height+2;
            curY := HomeY+KSet^.Base;
            ChangeTitle(Title);
            SSetCursor(HomeX,HomeY+Base)
            end
        end
    else
        HasTitle:=false
    end
else
    HasTitle:=false;
StartLine;
{$ifc VarWin then}
with curWindP^ do {now store the new window's parameters}
{$elsec}
With WinTable[curWind] do
{$endc}
   begin
   winBY := SBottomY; winTY := STopY; winLX := SLeftX; winRX := SRightX;
   winHX := HomeX; winHY := HomeY; winMX := MaxX; winMY := MaxY;
   winCurX := CurX; winCurY := CurY; winFunc := SFunc;
   winKSet := KSet; winCrsChr := CrsChr;
   winHasTitle := HasTitle;
   winCursorOn := SCursorOn;
   defined := true;
   end;
inlinebyte( INTON );
end { CreateWindow };


Procedure SFullWindow;
{-----------------------------------------------------------------------------
 Abstract: Changes the parameters of the current window to be the full screen
 SideEffects: Changes the size of the current window.  Does NOT refresh or
              change the title line or erase anything or move the cursor
-----------------------------------------------------------------------------}
  var LLeftX, LTopY, LRightX, LBottomY, LHeight, LMaxY: Integer;
      LHasTitle : Boolean;
  begin
  STopY:=LineIndent;
  SLeftX:=LineIndent;
  SBottomY:=YMax-LineIndent;
  SRightX:=SBitWidth-1-LineIndent;
  HomeX:=Indent;
  HomeY:=Indent;
  MaxX:=SBitWidth-1-Indent;
  MaxY:=YMax-Indent;
  if hasTitle then begin
                   STopY:=STopY+KSet^.Height+2;
                   HomeY:=HomeY+KSet^.Height+2;
                   end;
  end;  {SFullWindow}


Procedure RefreshWindow(WIndx: WinRange);
{-----------------------------------------------------------------------------
 Abstract: Redraws window outline and title area (but not title text)
 Parameters: Window to refresh (better be already created)
 Errors: Raises BadWNum if WIndx undefined
-----------------------------------------------------------------------------}
  var LLeftX, LTopY, LRightX, LBottomY, LHeight, LMaxY: Integer;
      LHasTitle : Boolean;
  begin
{$ifc VarWin then}
     {&&&&&&&&&&}
 if ptr=curWindp then
{$elsec}
  if WIndx = curWind then 
{$endc}
    begin
    LHeight := KSet^.Height;
    LMaxY := MaxY;
    LBottomY := SBottomY;
    if HasTitle then LTopY := STopY-LHeight-2
    else LTopY := STopY;
    LLeftX := SLeftX;
    LRightX := SRightX;
    LHasTitle := HasTitle;
    end
{$ifc VarWin then}
  else with ptr^ do
{$elsec}
  else if (WIndx < 0) or (WIndx > MaxWIndx) or
       (not winTable[WIndx].defined) then Raise BadWNum
  else with winTable[WIndx] do
{$endc}
    begin
    LHeight := winKset^.height;
    LMaxY := winMY;
    LBottomY := winBY;
    if WinHasTitle then LTopY := winTY-LHeight-2
    else LTopY := winTY;
    LLeftX := winLX;
    LRightX := winRX;
    LHasTitle := WinHasTitle;
    end;
 {erase area between home and Max}
{top} if LHasTitle then RasterOp(RXor, LRightX-LLeftX+2*LineIndent-1,
             LHeight+2+LineIndent,LLeftX-LineIndent+1,LTopY-LineIndent+1,
             SScreenW,SScreenP,LLeftX-LineIndent+1,LTopY-LineIndent+1,SScreenW,
             SScreenP)
      else RasterOp(RXor, LRightX-LLeftX+2*LineIndent-1,LineIndent,
                    LLeftX-LineIndent+1,LTopY-LineIndent+1,SScreenW, SScreenP,
                    LLeftX-LineIndent+1,LTopY-LineIndent+1,SScreenW, SScreenP);
{bot} RasterOp(RXor,LRightX-LLeftX+2*LineIndent-1, LineIndent+(LBottomY-LMaxY),
               LLeftX-LineIndent+1,LMaxY,SScreenW, SScreenP,
               LLeftX-LineIndent+1,LMaxY,SScreenW, SScreenP);
{left} RasterOp(RXor, LineIndent,LBottomY-LTopY+2*LineIndent-1,
               LLeftX-LineIndent+1,LTopY-LineIndent+1,SScreenW, SScreenP,
               LLeftX-LineIndent+1,LTopY-LineIndent+1,SScreenW, SScreenP);
{right} RasterOp(RXor, LineIndent,LBottomY-LTopY+2*LineIndent-1,
               LRightX,LTopY-LineIndent+1,SScreenW, SScreenP,
               LRightX,LTopY-LineIndent+1,SScreenW, SScreenP);
 {draw lines}
{top} Line(DrawLine,LLeftX-1,LTopY-1,LRightX+1,LTopY-1,SScreenP);
{bot}  Line(DrawLine,LLeftX-1,LBottomY+1,LRightX+2,LBottomY+1,SScreenP);
{left}  Line(DrawLine,LLeftX-1,LTopY-1,LLeftX-1,LBottomY+1,SScreenP);
{right}  Line(DrawLine,LRightX+1,LTopY-1,LRightX+1,LBottomY+1,SScreenP);
 {black title line}
  if LHasTitle then RasterOp(RXNor, LRightX-LLeftX + 1, LHeight+2,
         LLeftX, LTopY, SScreenW, SScreenP, LLeftX, LtopY, SScreenW, SScreenP);
 end; {RefreshWindow}


Procedure GetWindowParms(var WIndx: WinRange;
             var OrgX, OrgY, Width, Height: integer; var hasTitle: Boolean);
{-----------------------------------------------------------------------------
 Abstract: Returns parameters for current window
 Parameters: All set to current window's values
-----------------------------------------------------------------------------}
Begin
{$ifc VarWin then}
 with curWindP^ do
{$elsec}
 with winTable[curWind] do
{$endc}
   begin
{$ifc VarWin then}
   WIndx := WinNumber;
{$elsec}
   WIndx := curWind;
{$endc}
   OrgX := winLX; OrgY := winTY; 
   Width := winRX - winLX;
   Height := winBY - winTY;
   hasTitle := winHasTitle;
   end
end; {GetWindowParms}


Procedure ChangeWindow(WIndx: WinRange);
{-----------------------------------------------------------------------------
 Abstract: Writes out current window's parameters and changes to new one
 Parameters: WindX is new window's number
 Errors: Raises BadWNum if WIndx undefined
-----------------------------------------------------------------------------}
begin
  { Save Old State }
{$ifc VarWin then}
 inlinebyte( INTOFF );
 with curWindP^ do
{$elsec}
 if (WIndx < 0) or (WIndx > MaxWIndx) or
       not winTable[WIndx].defined then Raise BadWNum;
 inlinebyte( INTOFF );
 with winTable[curWind] do
{$endc}
   begin
   winBY := SBottomY; winTY := STopY; winLX := SLeftX; winRX := SRightX;
   winHX := HomeX; winHY := HomeY; winMX := MaxX; winMY := MaxY;
   winCurX := CurX; winCurY := CurY; winFunc := SFunc;
   winKSet := KSet; winCrsChr := CrsChr;
   winHasTitle := HasTitle;
   winCursorOn := SCursorOn;
   end;
SCurOff;
{ find pointer for window specified or abort if illegal }
{$ifc VarWin then}
curWindP := firstWindP;
while curWindP^.winNumber <> WIndx do
  begin
  curWindP := curWindP^.winNext;
  if curWindP = NIL then Raise BadWNum;
  end;
with curWindP^ do
{$elsec}
 curWind := WIndx;
 with winTable[curWind] do
{$endc}
   begin
   SBottomY := winBY; STopY := winTY; SLeftX := winLX; SRightX := winRX;
   HomeX := winHX; HomeY := winHY; MaxX := winMX; MaxY := winMY;
   CurX := winCurX; CurY := winCurY; SFunc := winFunc;
   KSet := winKSet; CrsChr := winCrsChr;
   HasTitle := winHasTitle;
   if winCursorOn then SCurOn;
   end;
inlinebyte( INTON );
StartLine;
end; {ChangeWindow}


Procedure SetFont(NewFont: FontPtr);
{-----------------------------------------------------------------------------
 Abstract: Changes font to be NewFont
 Parameters: NewFont is font to use
 SideEffects: Changes font in current window so all further writes (including
              titles) will be in this font
-----------------------------------------------------------------------------}
begin
KSet := NewFont;
StartLine;  {widths have changed}
end; {SetFont}


Function GetFont: FontPtr;
{-----------------------------------------------------------------------------
 Abstract: Returns current font
 Returns: font currently in use
-----------------------------------------------------------------------------}
begin
GetFont:=KSet
end;


Procedure ScreenReset;
{-----------------------------------------------------------------------------
 Abstract: Erases screen; Removes all window; sets
           Window 0 to have full screen boundary and a blank title
 SideEffects: Erases or sets all parameters; font set to system font
-----------------------------------------------------------------------------}
 var i: integer;
begin
CrsChr := '_';
SFunc := RRpl;
SCursorOn := false;
KSet:=MakePtr(FontSeg,0,FontPtr);
SNumTitleChars := (SBitWidth - 2*Indent) div KSet^.index[ord('W')].width;

{$ifc VarWin then}
While FirstWindP <> NIL do
  begin
  CurWindP := FirstWindP^.winNext;
  DISPOSE(FirstWindP);
  FirstWindP := CurWindP;
  end;
NEW(FirstWindP);
FirstWindP^.winNext := NIL;
FirstWindP^.winNumber := 0;
CurWindP := FirstWindP;
{$elsec}
for i := 0 to MaxWIndx do
   WinTable[i].defined := false;
CurWind := 0;
{$endc}
CreateWindow(0,0,0,SBitWidth,SBitHeight,' ');
end;


Procedure ScreenInit;
{-----------------------------------------------------------------------------
 Abstract: Sets FirstWindP to NIL and sets up default window;
 NOTE: CALL THIS PROCEDURE ONCE AT SYSTEM INITIALIZE
 Calls: ScreenReset;
-----------------------------------------------------------------------------}
   var temp: Integer;
   begin
   SScreenP := MakePtr(ScreenSeg,0,RasterPtr);
   if CF_Monitor = Cf_Landscape then
     begin
     SScreenW   := LandscapeWordWidth;
     SBitWidth  := LandscapeBitWidth;
     SBitHeight := LandscapeBitHeight;
     SMaxBitHeight := LandscapeBitHeight;
     SIsLandScape := true;
     {check to see if enough memory allocated for entire Landscape bit map:
      if booted from floppy, for example, may only have enough for a portrait}
     {$R-}
     if SAT^[ScreenSeg].Size+1 <> BlocksForLandscapeScreen then
        begin
        temp := SAT^[ScreenSeg].Size+1;
        SBitHeight := Shrink((((Stretch(temp) * 256)
                                div LandscapeWordWidth) div 128) * 128); 
             {round down to nearest multiple of 128 scan lines}
        temp := shrink((Stretch(LandscapeWordWidth)*SBitHeight+255) div 256);
        ChangeSize(ScreenSeg, temp);   {release unused memory}
        end;
     {$R=}
     end
   else begin
        SScreenW   := PortraitWordWidth;
        SBitWidth  := PortraitBitWidth;
        SBitHeight := PortraitBitHeight;
        SMaxBitHeight := PortraitBitHeight;
        SIsLandScape := false;
        {if booted with too much memory for screen then deallocate some}
        {$R-}
        if SAT^[ScreenSeg].Size+1 <> BlocksForPortraitScreen then
           ChangeSize(ScreenSeg, BlocksForPortraitScreen); 
        {$R=}
        end;
   YMax := SBitHeight-1;
   SCurBitHeight := SBitHeight;

{$ifc VarWin then}
   FirstWindP := NIL;
{$endc}
   ScreenReset;
   end;


Procedure Line(Style: LineStyle; X1, Y1, X2, Y2: integer; Origin: RasterPtr);
{-----------------------------------------------------------------------------
 Abstract: Draws a line.
 Parameters: Style - function for the line;
             X1, X2, Y1, Y2 - end points of line.
             Origin - pointer to the memory to draw lines in.
                      Use SScreenP for Origin to draw lines on the screen.
-----------------------------------------------------------------------------}
begin
 LoadExpr(Ord(Style) + Shift(SScreenW, 3));
 LoadExpr(X1);
 LoadExpr(Y1);
 LoadExpr(X2);
 LoadExpr(Y2);
 LoadExpr(Origin);
 InLineByte( TLATE0 );
 InLineByte( QLINE  )
end; { Line }


Procedure SVarLine(Style: LineStyle; X1, Y1, X2, Y2, Width: integer;
                   Origin: RasterPtr);
{-----------------------------------------------------------------------------
 Abstract: Draws a line.  Same as Line except it takes the buffer width as
           a parameter.  This is only useful when drawing lines in off-screen
           buffers.
 Parameters: Style - function for the line;
             X1, X2, Y1, Y2 - end points of line.
             Width - the word width of the "origin" buffer.
             Origin - pointer to the memory to draw lines in.
                      Use SScreenP for Origin to draw lines on the screen.
-----------------------------------------------------------------------------}
begin
 LoadExpr(Ord(Style) + Shift(Width, 3));
 LoadExpr(X1);
 LoadExpr(Y1);
 LoadExpr(X2);
 LoadExpr(Y2);
 LoadExpr(Origin);
 InLineByte( TLATE0 );
 InLineByte( QLINE  )
end. { Line }

{********************************* NO LONGER SUPPORTED ***************}


Procedure SLine(Style: LineStyle; X1, Y1, X2, Y2: integer; Origin: RasterPtr);
{-----------------------------------------------------------------------------
 Abstract: Draws an line relative and clipped to the current window;
 Parameters: Style is function for the line; X1, X2, Y1, Y2 are end points of
             line relative to current window (i.e. 0,0 is upper left corner of
             home char).  Origin is pointer for Screen;
 SideEffects: Draws a line on the screen
-----------------------------------------------------------------------------}
begin
 x1 := x1+homeX;
 y1 := y1+homeY;
 if x1 <= homeX then x1 := homeX else if x1 >= maxX then x1 := maxX;
 if y1 <= homeY then y1 := homeY else if y1 >= maxY then y1 := maxY;
 x2 := x2+homeX;
 y2 := y2+homeY;
 if x2 <= homeX then x2 := homeX else if x2 >= maxX then x2 := maxX;
 if y2 <= homeY then y2 := homeY else if y2 >= maxY then y2 := maxY;
 Line(Style,x1,y1,x2,y2,origin);
 end { SLine };


Procedure SRasterOp(Funct, Width, Height, DestX, DestY, DestWidth: Integer;
                    DestP: RasterPtr; SourceX, SourceY, SourceWidth: Integer;
                    SourceP: RasterPtr);
{-----------------------------------------------------------------------------
 Abstract: Does a RasterOp relative and clipped to the current window;
 Parameters: Same as for RasterOp; does relative to current window for dest if
             destP = SScreenP and for source if sourceP = SScreenP
 SideEffects: Does a rasterOp on the screen
-----------------------------------------------------------------------------}
begin
 if DestP = SScreenP then
     begin
     DestX := DestX+homeX;
     DestY := DestY+homeY;
     if DestX <= homeX then DestX := homeX
          else if DestX >= maxX then DestX := maxX;
     if DestY <= homeY then DestY := homeY
          else if DestY >= maxY then DestY := maxY;
     end;
 if SourceP = SScreenP then
     begin
     SourceX := SourceX+homeX;
     SourceY := SourceY+homeY;
     if SourceX <= homeX then SourceX := homeX else
           if SourceX >= maxX then SourceX := maxX;
     if SourceY <= homeY then SourceY := homeY else
           if SourceY >= maxY then SourceY := maxY;
     end;
 RasterOp(Funct, Width, Height, DestX, DestY, DestWidth, DestP,
                                SourceX, SourceY, SourceWidth, SourceP);
 end.

Procedure ChangeTitle(Title: STitStrType);
{-----------------------------------------------------------------------------
 Abstract: Changes the title of the current window (and displays new one).
 Parameters: Title is new string.  Characters in it are quoted so special
             characters will be displayed.   
 SideEffects: Changes title on screen
-----------------------------------------------------------------------------}
  var NoOverFlo, oldCurOn: boolean;
      i, OldFunc, OldX, OldY, oldlastLineEnd: integer;
      OldKeyEnable: Boolean;

  Procedure DoCleanUp;
  {----------------------------------------------------------------------------
   Abstract: Resets parameters to original values
  ----------------------------------------------------------------------------}
      begin
      SFunc:=OldFunc;
      with KSet^ do
           begin
           STopY:=STopY+Height+3;
           HomeY:=HomeY+Height+3;
           end;
      SSetCursor(OldX,OldY);
      SCursorOn := oldCurOn;
      lastLineEnd := oldlastLineEnd;
      end;

  Procedure DoChangeTitle;
  {---------------------------------------------------------------------------
   Abstract: Actually changes the title.
  ---------------------------------------------------------------------------}
      begin
       with KSet^ do
           begin
           STopY:=STopY-Height-3;
           HomeY:=HomeY-Height-3;
           SSetCursor(HomeX,HomeY+Base);
           end;
       for i:= 1 to TitStrLength do
        if (CurX + KSet^.Index[LAnd(ord(Title[i]),#177)].Width <= MaxX) and
                 NoOverFlo then
            SPutChr(Chr(LOr(ord(Title[i]), #200)))
        else
            NoOverFlo:=false;
      end;

begin
if HasTitle then
    begin
    inlinebyte( INTOFF );
    IOKeyDisable(OldKeyEnable);
    oldCurOn := SCursorOn;
    oldlastLineEnd := lastLineEnd;
    NoOverFlo:=true;
    for i := length(title)+1 to TitStrLength do
        title[i] := ' ';
    OldFunc:=SFunc;      { Save Normal RasterOp Function and Cursor Positions }
    SReadCursor(OldX,OldY);
    SFunc:=RNot;
    SCursorOn := false;  {so cursor doesn't blink when change title}
    DoChangeTitle;
    DoCleanUp;
    IOKeyEnable(OldKeyEnable);
    inlinebyte( INTON );
    end;
end { ChangeTitle };

