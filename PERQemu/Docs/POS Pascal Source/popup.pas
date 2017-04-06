{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
(****)
Module PopUp;
(*****)
(*****
Program PopUp(input, output);
(******)

{----------------------------------------------------------------------------
 PopUp displays "popUp" menu windows 
 
 Written by  Brad A. Myers 16-Nov-80
  
 Abstract: This program produces pop up windows that replace the screen
            area at a specified cursor location. The cursor is then changed and
            PopUp waits for a press.  Whenever the cursor is inside the window,
            the command at that point is highlighted.  If a press is done
            inside the window, the highlighted command is selected.  The
            user can control whether one or more than one command should
            be selected before window is removed.  If a press outside
            outside, no command is executed.  In any case, the window is
            erased and the original contents of that area is returned

 Copyright (C) 1981, 1982, 1983 - The Three Rivers Computer Corporation
-----------------------------------------------------------------------------}


{$Version V3.1 for POS}
{----------------------------------------------------------------------------
 Change Log:
      29-Mar-83  Brad A. Myers V3.1  Fixed so saves and restores old cursor.
                                     Fixed bug in PopKeyHit with CtlC, etc.
       3-Mar-83  C. Beckett    V3.0  Fixed bug:unconditional abort on key.
      23-Feb-83  Brad A. Myers V2.9  Fixed bug in AllocateRes.
      23-Feb-83  Brad A. Myers V2.8  Fixed bug in char writing for landscape.
      14-Feb-83  Brad A. Myers V2.7  Fixed in case screen shrunk.
                                     Provide information in ResRes for which
                                       button pressed.
                                     Provide a new procedure to abort when
                                       keyboard input happens.
      10-Feb-83  Brad A. Myers V2.6  Fixed for landscape monitor.
      16 Nov-82  Bill Braucher V2.5  Fixed names for 14-character compiler.
      19 Jan 82  Brad A. Myers V2.4  Handle the exception HelpKey.
      17 Sep-81  Brad A. Myers V2.3  fixed comments.  Made so will work with
                                     any current font.
      28-Aug-81  Brad A. Myers V2.2  Fixed histerises for bottom command.
                                     Made it harder to select the bottom
                                       command when trying to hit the scroll
                                       bar.
      22-Jul-81  Brad A. Myers V2.1  Handlers for ^C and ^Shift-C.
      22-Jul-81  Brad A. Myers V2.0  removed interface to TheFront.
      20-Nov-80  Brad A. Myers V1.1  Changed interface to TheFront.
      16-Nov-80  Brad A. Myers V1.0  Started.
-----------------------------------------------------------------------------}


{-*-*-*-*-*-*-*-*-*-*-*-*-*-*-} EXPORTS {-*-*-*-*-*-*-*-*-*-*-*-*-*-*-}

EXCEPTION BadMenu;
{-----------------------------------------------------------------------------
 Abstract: Raised when parameters are illegal.
 Resume: NOT ALLOWED.
-----------------------------------------------------------------------------}

EXCEPTION Outside;
{-----------------------------------------------------------------------------
 Abstract: raised when press outside of menu.
 Resume: NOT ALLOWED.
-----------------------------------------------------------------------------}

EXCEPTION PopKeyHit;
{-----------------------------------------------------------------------------
 Abstract: Raised when a key is hit and aborting on keys is enabled by calling
           AbortOnKey(true).  The key is left in the input buffer.
 Resume: NOT ALLOWED.
-----------------------------------------------------------------------------}

Type s25 = String[25];
     NameAr = Array[1..1] of s25;
     pNameAr = ^NameAr;
     NameDesc = Record
                   header: s25;
                   numCommands: integer;
                   commands: NameAr;
                 End;
     pNameDesc = ^NameDesc;

     ResRes = ^ResArray;
     ResArray = Record
                 numIndices: integer;
                 pressVal: Integer;  {TabMouse value when pressed}
                 indices: Array[1..1] of integer;
                End;

PROCEDURE Menu(names: pNameDesc; isList: boolean;
               first, last, curX, curY, maxYsize: integer; VAR res: ResRes);

PROCEDURE InitPopUp;
PROCEDURE DestroyRes(var res: ResRes);
PROCEDURE AbortOnKey(abort: boolean);

PROCEDURE AllocNameDesc(numNames, seg: Integer; Var names: pNameDesc);
PROCEDURE DestroyNameDesc(Var names: pNameDesc);

{-*-*-*-*-*-*-*-*-*-*-*-*-*-*-} PRIVATE {-*-*-*-*-*-*-*-*-*-*-*-*-*-*-}

Imports Memory from Memory;
Imports Dynamic from Dynamic;
Imports Screen from Screen;
Imports PopUpCurs from PopUpCurs;
Imports IO_Others from IO_Others;
Imports IO_Unit from IO_Unit;  {using IOBeep, IOCPresent}
Imports System from System;  {using CtlC}

EXCEPTION Impossible;
EXCEPTION Imposs2;

CONST TRANS = FALSE;

{$IFC Trans then}
Imports Transcript from TransFast;
Imports System from System;
Imports CmdParse from CmdParse;
Imports MultiRead from MultiRead;
Imports FileSystem from FileSystem;

Const popuptranscript = 'PopUp.Transcript';

var buttons: PressRec;

{$elsec}

var buttons: integer;

{$endc}

Type SizeArray = ARRAY[0..0] OF Integer;
     IResArray = PACKED ARRAY[0..0] OF BOOLEAN;
     pResAr = ^IResArray;
     pSizeAr = ^SizeArray;
     WhereCode = (wSelect, wOutside, wScroll, wDoIt);
     
Var  AbortIfKeys : boolean;


Procedure AllocNameDesc(numNames, seg: Integer; Var names: pNameDesc);
{----------------------------------------------------------------
 Abstract: There are two ways to allocate the storage for a NameDesc.  One
           is to declare in your program a type with an array of the correct
           size and the other fields exactly the same way.  You then RECAST
           a pointer to that array into a pNameDesc.  The other way is to use
           this procedure.  It allocates the storage for numNames out of a
           segment.  Turn off range checking when assigning or accessing the
           array.

 NOTE: To deallocate the nameDesc returned, use DestroyNameDesc
 Parameters: numNames - the number of names in the array.

             seg - the segment to allocate the nameDesc out of.  If zero, then
                   uses the default segment.  This procedure uses NewP so
                   the segment can have other things in it also.

             names - set with the newly allocated pNameDesc.  Its numCommands
                     field is set with numNames.  Do not change this size or
                     the deallocation will not work.
-------------------------------------------------------------------------}
    var p: MMPointer;
        size: integer;
    begin
    size := (numNames+1)*WordSize(s25)+1;
    NewP(seg, 1, P, size);
    names := RECAST(p.p, pNameDesc);
    names^.numCommands := numNames;
    end; {AllocNameDesc}


Procedure DestroyNameDesc(Var names: pNameDesc);
{----------------------------------------------------------------
 Abstract: Delete names.  It should have been created by AllocateNameDesc.
           The numCommands field better be the same as set when allocated.
 Parameters: names - The storage for names is deallocated and names is set to
                     NIL.
-------------------------------------------------------------------------}
   var p: MMPointer;
       size: integer;
   begin
   size := (names^.numCommands+1)*WordSize(s25)+1;
   p.b := RECAST(names, pMMBlockArray);
   DISPOSEP(p, size);
   names := NIL;
   end; {DestroyNameDesc}


{$ifc not Trans then}
FUNCTION GetAction(var x,y: integer; wait: boolean; buttons: integer): boolean;
{-----------------------------------------------------------------------------
Abstract: This procedure provides a procedure with the same name as the
          transcript procedure and simply returns the state of the tablet
Parameters: x and y are set with the position of the tablet
            wait is ignored
            buttons is not set
Returns: tabswitch
-----------------------------------------------------------------------------}
  begin
  IOReadTablet(x,y);
  if AbortIfKeys then
      if IOCPresent( keyboard ) then Raise PopKeyHit;
  GetAction := tabswitch;
  end;


PROCEDURE SetCursMode(mode: CursMode);
{-----------------------------------------------------------------------------
Abstract: Sets the cursor mode
Parameters: mode is the new mode
-----------------------------------------------------------------------------}
  begin
  IOCursorMode(mode);
  end;
{$endc}


PROCEDURE WaitNoPress;
{-----------------------------------------------------------------------------
Abstract: Wait until not pressing on tablet
-----------------------------------------------------------------------------}
  var x,y: integer;
      press: boolean;
  begin
  repeat
    press := GetAction(x,y,true,buttons);
  until not press;
  end;


PROCEDURE InitPopUp;
{-----------------------------------------------------------------------------
Abstract: creates cursors needed to make PopUp windows work.  This should be
          called once before calling menu.
Environment: sets cursors.  Sets the global abort on keyboard typing to false.
-----------------------------------------------------------------------------}
  begin
  InitCurs;
  AbortIfKeys := false;
  IOSetModeTablet(relTablet);
  end;


PROCEDURE AbortOnKey(abort: boolean);
{-----------------------------------------------------------------------------
Abstract: Sets the global flag that determines whether to abort on keyboard
          typing.  If this is not called, then popup does not abort when
          there are keys.  If this is called with TRUE, then the exception
          PopKeyHit is raised when a key is typed.  The key is left in
          the input buffer.
-----------------------------------------------------------------------------}
  begin
  AbortIfKeys := abort;
  end;


PROCEDURE DestroyRes(var res: ResRes);
{------------------------------------------------------------------------
 Abstract: Deallocates storage for res and sets it to NIL
 Parameters: res is ResRes to destroy
------------------------------------------------------------------------}
  var p: MMPointer;
  begin
  p := RECAST(res, MMPointer);
  DisposeP(p, res^.numIndices+2); {1 word for count, 1 for tabmouse}
  res := NIL;
  end; {DestroyRes}

{$R-}


FUNCTION Min(x,y:integer):integer;
{-----------------------------------------------------------------------------
Abstract: Performs Mininum calculation.
Parameters: x and y are numbers to test.
Returns: Lesser of x, y.
-----------------------------------------------------------------------------}
  begin
  if x < y then Min := x 
  else Min := y;
  end; {Min}


FUNCTION Max(x,y:integer):integer;
{-----------------------------------------------------------------------------
Abstract: Performs Maximum calculation.
Parameters: x and y are numbers to test.
Returns: Greater of x, y.
-----------------------------------------------------------------------------}
  begin
  if x > y then Max := x 
  else Max := y;
  end; {Max}


PROCEDURE GetFontP(var kset, fontp: FontPtr; var fontHeight: integer);
{-----------------------------------------------------------------------------
Abstract: gets some values for the current font.
Parameters: kset is assigned the fontPtr for the current font, fontP is
            assigned the pointer to be used in rasterOp, fontHeight is
            assigned the height of the font.
-----------------------------------------------------------------------------}
   var hack: record case boolean of
                true: (f: FontPtr);
                false: (Seg, Ofst: integer);
                end;
   begin
   Kset := GetFont;  {ask the screen package for the current font}
   hack.f := KSet;
   fontp := MakePtr(hack.seg, hack.ofst+#404, fontPtr);
   fontHeight := KSet^.Height;
   end; {GetFontP}


PROCEDURE GetSpares(var spare1, spare2, spare3: RasterPtr;
                      spare1to3size: integer;
                      var spare4: RasterPtr; 
                      spare4size: integer; var segNum: integer);
{-----------------------------------------------------------------------------
Abstract: allocates space for all spare rasters.
Parameters: spare1, spare2 and spare3 are each set with storage allocated
            in the amount of spare1to3size.  Spare4 is set with storage
            allocated of spare4size.  Seg is set with the segment used for
            the storage.
-----------------------------------------------------------------------------}
   var numBlks: integer;
   begin
   numBlks := (3*spare1to3size+spare4size+255) Div 256;
   CreateSegment(segNum, numBlks, 1, numBlks);
   spare1 := MakePtr(segNum, 0, RasterPtr);
   spare2 := MakePtr(segNum, spare1to3size, RasterPtr);
   spare3 := MakePtr(segNum, spare1to3size*2, RasterPtr);
   spare4 := MakePtr(segNum, spare1to3size*3, RasterPtr);
   end; {GetSpares}


PROCEDURE Menu(names: pNameDesc; isList: boolean;
               first, last, curX, curY, maxYsize: integer; VAR res: ResRes);
{-----------------------------------------------------------------------------
Abstract: puts up a window with commands commands stacked vertically with the
          center at curX, curY.  Allocates off the heap enough storage for old
          picture at that place so can restore it.  Deallocates all storage
          when done.
Parameters: names - a pointer to an array of names to put in the menu.  In it

              header - is put at the top of the menu.  It may be empty in which
                       case there is no header.

              numcommands - the number of names in the array.

              commands - an array of names to display.  These can be generated
                         by having pNameDesc with an array of the correct (or
                         larger than the correct) size and recasting it into
                         a pNameDesc, or by creating a segment to hold all
                         the names that will be needed.

            isList - if true says that a number of commands can be selected.
                       if false, Menu returns as soon as the first command
                       is selected.

            first - the index of the first command in names^.commands to
                      display.  To display all items, use 1.

            last - the index of the last command in names^.commands to
                      display.  To display all items, use names^.numCommands.
                      Last must be greater than first.

            curX - the x position at which to display the menu.  If -1 then
                     uses current pen position.

            curY - the y position at which to display the menu.  If -1 then
                     uses current pen position.

            maxYsize - the maximum size in bits of the menu.  If -1, then
                         menu will be big enough to hold all items (up to the
                         size of the screen).  maxYsize must be
                         greater than 4*(fontHeight+4) which is 68 for the
                         default font.

            res - is set with an answer array.  This array is allocated off
                   the heap by Menu.  Use DestroyRes to deallocate it.  If
                   Menu is exited via ^C or a press outside, then res is not
                   allocated.  The fields of res are set as follows:

                      numIndices - the number of items selected.  If not isList
                                   then will be 1.  If isList, will not be zero

                      indices - a variable length array of the indices of the
                                names chosen.  They are in increasing order
                                irrespective of the order the names were
                                picked.
Errors: Catches CtlC and raises CtlCAbort after removing the menu.
        Catches CtlShiftC and erases menu then re-raises CtlShiftC.
        Catches HelpKey and erases menu and then re-raises.
        If any of these are resumed, Raises PopKeyHit if allowed, or Outside
          if PopKeyHit not allowed.
        Raises OutSide if press outside of the menu window.
        Raises BadMenu if parameters are illegal.
Environment: Requires enough memory be on the heap for picture.  Requires that
             InitPopup has been called.
-----------------------------------------------------------------------------}
var i: integer;
    xSize, ySize, xStart, yStart, yRSize, yRStart, fontHeight, segNum,
    j, firstVisCmd, lastVisCmd, topOffset, botOffset, dum, tempy: integer;
      scrSpare, topSpare, botSpare, greySpare: RasterPtr;
      maxStrW, spareW : integer;
      sizeAr : pSizeAr;
      resAr: pResAr;
      KSet, fontP: FontPtr;
      specString : s25;
      pList, pSize: MMPointer;
      numCmds: integer;
      notEnough, started,press: Boolean;
      commands: pNameAr;
      scrollP, spotp: pFootAr; 
      footW, footerSize, footerStart, headerSize, minWidth: integer;
      minYsize, numSelected: integer

  PROCEDURE DoCleanup;
   {------------------------------------------------------------------------
    Abstract: Restores original picture; deallocates storage
    ------------------------------------------------------------------------}
    begin
  {**reset original picture**}
    if not Started then exit(DoCleanup);
    RasterOp(RRpl, xSize, yRSize, xStart, yRstart, SScreenW, SScreenP,
           0,0,SpareW, scrSpare);

    started := false; {so don't try to cleanup again}

  {**wait for not press**}
    while tabSwitch do ;

  {**Clean up storage**}
    DisposeP(pSize, numCmds+1);
    if islist then DisposeP(pList, (numCmds +2+ 15) div 16);
    DecRefCount(segNum); {deallocates all spares}

    SetCurs(Default);
    end; {DoCleanup}

  HANDLER HelpKey(var s: Sys9s);
   {------------------------------------------------------------------------
    Abstract: Does cleanup and raises HelpKey.  If continued, raises OutSide.
    ------------------------------------------------------------------------}
     begin
     DoCleanup;
     RAISE HelpKey(s);
     if AbortIfKeys then Raise PopKeyHit
     else RAISE OutSide;
     end;
  HANDLER CtlC;
   {------------------------------------------------------------------------
    Abstract: Does cleanup and raises CtlCAbort
    ------------------------------------------------------------------------}
     begin
     DoCleanup;
     RAISE CtlCAbort;
     if AbortIfKeys then Raise PopKeyHit
     else RAISE OutSide;
     end;
  HANDLER CtlShftC;
   {------------------------------------------------------------------------
    Abstract: Does cleanup and re-raises signal
    ------------------------------------------------------------------------}
     begin
     DoCleanup;
     RAISE CtlShftC;
     if AbortIfKeys then Raise PopKeyHit
     else RAISE OutSide;
     end;
  HANDLER PopKeyHit;
   {------------------------------------------------------------------------
    Abstract: Does cleanup and raises PopKeyHit.
    ------------------------------------------------------------------------}
     begin
     DoCleanup;
     RAISE PopKeyHit;
     end;

  PROCEDURE WriteString(s: s25; destp: RasterPtr; destW, x, y: integer);
   {------------------------------------------------------------------------
    Abstract: Draws a string into a raster two down from y and at x.
    Parameters: s is the string, destp is destination Raster, destW is
                  width of destp, x and y are the offsets in destp
    ------------------------------------------------------------------------}
       var j : integer;
       begin
       for j := 1 to length(s) do
          with KSet^.Index[ord(s[j])] do
              begin
              RasterOp(RRpl, width, fontHeight-4, x, y+2,
                   destW, destp, Offset, Line*(fontHeight-4), KSetSLen, fontP);
              x := x+width;
              end;
       end; {WriteString}
   PROCEDURE DrawString(cmdNum: integer; destp: RasterPtr; destW, x: integer; 
                         var y: integer);
    {------------------------------------------------------------------------
    Abstract: Draws a string into a raster.  If cmdNum=0 or maxCmds+1 then uses
               specString else uses pNameAr^[cmdNum].
    Parameters: cmdNum is index into command array, destp is destination
                 Raster, destW is width of destp, x and y are the offsets in
                 destp
    Environment: Uses maxStrW, fontHeight, KSet 
    Calls: WriteString
    ------------------------------------------------------------------------}
       var xPos: integer;
           s: s25;
       begin
       if (cmdNum=0) or (cmdNum=numCmds+1) then begin
                                     s := specString;
                                     xPos := x+((maxStrW-sizeAr^[0]) div 2);
                                     end
       else begin
            s := commands^[cmdNum];
            xPos := x+((maxStrW-sizeAr^[cmdNum]) div 2);
            end;
       WriteString(s, destp, destW, xPos, y);
       y := y + fontHeight;
       end;  {DrawString}

   FUNCTION StringW(s: s25): Integer;
    {------------------------------------------------------------------------
    Abstract: finds the number of bits on the screen that the string will take.
    Parameters: s is string
    Returns: length in bits
    Environment: Uses Kset.
    ------------------------------------------------------------------------}
      var i, size : integer;
      begin
      size := 0;
      for i := 1 to length(s) do
          size := size+KSet^.Index[ord(s[i])].width;
      StringW := size;
      end;

  PROCEDURE Fill(cmd: integer; top: boolean);
    {------------------------------------------------------------------------
    Abstract: fills the a spare with string representing cmd reversed if
              selected.
    Parameters: cmd is index into command array. if top=true then fills
                 topSpare else botSpare.
    Calls: DrawString.
    Environment:  Modifies Spares.
    ------------------------------------------------------------------------}
    var temp: integer;
    begin
    temp := 0;
    if (cmd < 0) or (cmd > numCmds+1) then BEGIN
                                    Write(' IMPOSSIBLE in Fill; val=',cmd:1);
                                    exit(fill);
                                    end;
     if top then
       begin
     {first, erase old value}
       RasterOp(RXor,xSize,fontHeight,0,0,SpareW,topSpare,0,0,SpareW,topSpare);
       DrawString(cmd, topSpare, spareW, 0, temp);
       if isList then if resAr^[cmd] then RasterOp(RNot, xSize-8,
          fontHeight, 0,0,SpareW, topSpare, 0,0,SpareW, topSpare);
       end
     else begin
      {first, erase old value}
       RasterOp(RXor,xSize,fontHeight,0,0,SpareW,botSpare,0,0,SpareW,botSpare);
       DrawString(cmd, botSpare, spareW, 0, temp);
       if isList then if resAr^[cmd] then RasterOp(RNot, xSize-8,
          fontHeight, 0,0,SpareW, botSpare, 0,0,SpareW, botSpare);
       end;
    end;  {Fill}
  PROCEDURE ScrollOne(dir: integer);
    {------------------------------------------------------------------------
    Abstract: Scrolls text up or down one scan line.
    Parameters: dir tells whether text goes up or down: 1 = up; -1 = down.
    Environment:  Assumes topSpare and bottomSpare are filled.  If need to
                  get new line into them, ScrollOne will do that after Scroll.
                  Uses specs of menu.  Modifies screen and specs of menu.
    ------------------------------------------------------------------------}
      var textSize: integer;
      begin
      textSize := yRSize-headerSize-footerSize-3;
      if dir = 1 then {move text up}
         begin
         if (botOffset = fontHeight) then {need new row}
                               begin
                               if lastVisCmd = numCmds+1 then exit(ScrollOne);
                               lastVisCmd := lastVisCmd+1;
                               Fill(lastVisCmd, false);
                               botOffset := 1;
                               end
         else botOffset := botOffset+1;
         topOffset := topOffset+1;
         if (topOffset = fontHeight) then {need new row}
                               begin
                               firstVisCmd := firstVisCmd+1;
                               topOffset := 0;
                               end
         else if (topOffset=1) then Fill(firstVisCmd, true);
         RasterOp(RRpl, xSize-4, textSize-1, xStart+2, yRStart+headerSize,
                   SScreenW, SScreenP, xStart+2, yRStart+headerSize+1,
                   SScreenW, SScreenP);
         RasterOp(RRpl, xSize-6, 1, xStart+4, yRStart+headerSize+textSize-1,
                   SScreenW, SScreenP, 0, botOffset-1, SpareW, botSpare);
         end
      else if dir = -1 then {move text down}
         begin
         if (topOffset = 0) then {need new row}
                               begin
                               if firstVisCmd = 0 then exit(ScrollOne);
                               firstVisCmd := firstVisCmd-1;
                               Fill(firstVisCmd, true);
                               topOffset := fontHeight-1;
                               end
         else topOffset := topOffset-1;
         botOffset := botOffset-1;
         if (botOffset = 0) then begin
                                 lastVisCmd := lastVisCmd -1;
                                 botOffset := fontheight;
                                 end
         else if (botOffset = fontHeight-1) then {need new row}
                               begin
                               Fill(lastVisCmd, false);
                               end;
         RasterOp(RRpl, xSize-4, textSize-1, xStart+2, yRStart+headerSize+1,
                   SScreenW, SScreenP, xStart+2, yRStart+headerSize,
                   SScreenW, SScreenP);
         RasterOp(RRpl, xSize-6, 1, xStart+4, yRStart+headerSize,
                   SScreenW, SScreenP, 0, topOffset, SpareW, topSpare);
         end
      else RAISE IMPOSSIBLE;
      end; {ScrollOne}
  PROCEDURE HandleScroll;
    {------------------------------------------------------------------------
    Abstract: Changes cursor to bar and retains control until press off.
              Scrolls the names up or down depending on position of pen.
    Environment:  Modifies screen and specs of menu.
    ------------------------------------------------------------------------}
      var temp, sTabx, sTaby, x, y, centerX, centerY, dir, speed,
            oldtemp: integer;
          press: boolean;
      begin
      SetCurs(bar);
      SetCursMode(indepCursor);
      press := GetAction(stabx, staby, false, buttons);
      centerX := xStart+20;
      centerY := yRStart+yRSize-footerSize+2;
      IOSetCursorPos(centerX, centerY);
    {initialize TopSpare and bottomSpare}
      if botOffset = 0 then botOffset := fontHeight;
      if topOffset = fontHeight then topOffset := 0;
      Fill(firstVisCmd, true);
      Fill(lastVisCmd, false);
    {go}
      oldTemp := -1; {temp is always >= 0}
      while press Do 
          begin
          press := GetAction(x,y,false,buttons);
          if x < sTabx then dir := -1
          else dir := 1;
          temp := Shift(ABS(x-sTabx), -3); {divide by 8}
          if temp <> oldTemp then
            begin
            if temp < 1 then begin 
                             x := 1;
                             speed := 16;
                             end
            else if temp < 2 then begin
                                  x := 2;
                                  speed := 8;
                                  end
            else if temp < 4 then begin
                                  x := 4;
                                  speed := 4;
                                  end
            else if temp < 8 then begin
                                  x := 6;
                                  speed := 1;
                                  end
            else if temp < 16 then begin
                                   x := 8;
                                   speed := 0;
                                   end
            else begin
                 x := 10;
                 speed := -1;
                 end;
            IOSetCursorPos(centerX+x*dir, centerY);
            oldTemp := temp;
            end; {if oldTemp <> temp}
         {wait a little depending on speed}
          For i := 0 to speed do
             for j := 1 to 100 do;
        {Now do scroll}
          ScrollOne(dir);
          end; {loop}
      SetCursMode(trackCursor);
      IOSetTabPos(centerX, centerY);
      SetCurs(select);
      end; {HandleScroll}
   PROCEDURE SelXor(sel: Integer; grey: boolean);
    {------------------------------------------------------------------------
    Abstract: XOrs picture of menu item sel or draws grey box around.
    Parameters: sel is menu item to xor. grey tells whether to use grey or box
    Environment:  Uses specs of menu.
    ------------------------------------------------------------------------}
       var ys, yH, yts: Integer;
       begin
       if (sel < 1) or (sel > numCmds) then RAISE IMPOSSIBLE;
     {first check if scrolled off screen}
       if (sel < firstVisCmd) or (sel > lastVisCmd) then exit(SelXor);
       if sel = firstVisCmd then begin
                                 ys := yRStart+headerSize;
                                 yH := fontHeight-topOffset;
                                 yts := topOffset;
                                 end
       else begin
             yts := 0;
             ys := yRStart+headerSize+(fontHeight-topOffset)+
                          (sel-firstVisCmd-1)*fontHeight;
             if (sel <> lastVisCmd) or ((sel=lastVisCmd) and 
                         (botOffset=0)) then yH := fontHeight
             else yH := botOffset;
             end;
       if not grey then RasterOp(RNot, xSize-8, yH, xStart+4, ys, SScreenW,
                           SScreenP, xStart+4, ys, SScreenW, SScreenP)
       else RasterOp(RXor, xSize-4, yH, xStart+2, ys, SScreenW, SScreenP,
                         2, yts, SpareW, greySpare);
       end; {SelXor}
  FUNCTION CheckCurs(VAR where: integer): WhereCode;
    {------------------------------------------------------------------------
    Abstract: Watches the cursor and changes its shape depending on where it
              it w.r.t. the menu.  If over a menu item, reverse videos it.
              Has histeresis.  Waits for a press before returning.
    Parameters: where is a return value describing which menu item (index into
                commands) is selected or -1 if outside.
    Returns: WhereCode describes where the cursor is.
    Environment:  Modifies cursor.
    ------------------------------------------------------------------------}
      CONST hist = 3;
            SafetyFactor = 6;
      var lastSel, curSel, x, y, lastSelBot, lastSelTop, lastSelRight, 
          lastSelLeft: integer;
          press: boolean;
      label 1,2,3;
      begin
      lastSel := -1;
      curSel := -1;
      while true do
          begin
          press := GetAction(x,y,true,buttons);
          if (curSel <> -1) and  {test if press in the same place}
              (y < lastSelBot+hist) and (y > lastSelTop-hist) and 
               (x < lastSelRight+hist) and (x > lastSelLeft-hist) then goto 2;
  {oob y} if (y < (yRStart+headerSize)) or (y> yRStart+yRSize) then goto 1;
  {oob x} if (x < xStart) or (x > xStart+xSize) then goto 1;
{in foot} if y > yRStart+yRSize-footerSize then
              begin
              if footerSize = 3 then goto 1;
              lastSelBot := yRStart+yRSize;
              lastSelTop := lastSelBot-footerSize;
              if notEnough and (x < xStart+39) then {scroll bar}
                    begin
                    curSel := -2;
                    if lastSel <> curSel then begin
                                    if lastSel>0 then SelXor(lastSel, isList);
                                    SetCurs(scroll);
                                    lastSel := curSel;
                                    end;
                    lastSelLeft := xStart;
                    lastSelRight := lastSelLeft+39;
                    goto 2;
                    end
              else if islist and (x > xStart+xSize-11) then {doIt spot}
                    begin
                    curSel := -3;
                    if lastSel <> curSel then begin
                                    if lastSel>0 then SelXor(lastSel, isList);
                                    SetCurs(DoIt);
                                    lastSel := curSel;
                                    end;
                    lastSelRight := xStart+xSize;
                    lastSelLeft := lastSelRight-11;
                    goto 2;
                    end
              else goto 1;
              end
        {make sure aren't pressing on a small piece of a command when trying
        {to get the scroll bar}
          else if y > yRStart+yRSize-footerSize-SafetyFactor then goto 1;
        {here must be a good selection!!!}
          if lastSel < -1 then SetCurs(select);
          lastSelLeft := xStart;
          lastSelRight := xStart+xSize;
          if y < yRStart+headerSize+(fontHeight-topOffset) then
                 begin
                 curSel := firstVisCmd;
                 lastSelTop := yRStart+headerSize;
                 lastSelBot := lastSelTop+(fontHeight-topOffset);
                 end
            else begin
                 curSel := firstVisCmd+
                              (y-yRStart-headerSize-(fontheight-topOffset)+
                              fontHeight-1) div fontHeight;
                 (*TEMP*) If curSel > numCmds+1 then curSel := numCmds+1;
                 lastSelTop := yRStart+headerSize+(fontHeight-topOffset)+
                                  (curSel-firstVisCmd-1)*fontHeight;
                 if (curSel <> lastVisCmd) or ((curSel=lastVisCmd) and 
                         (botOffset=0)) then
                             lastSelBot := lastSelTop+fontHeight-1
                 else lastSelBot := lastSelTop+botOffset-1;
                 end;
          if (curSel = 0) or (curSel = numCmds+1) then
                  begin
                  if (lastSel > 0) then SelXor(lastSel, isList);
                  lastSel := -1;
                  end
          else if (curSel <> lastSel) then
               begin
               if lastSel > 0 then SelXor(lastSel, isList);
               SelXor(curSel, isList);
               lastSel := curSel;
               end;
          goto 2;
       1: curSel := -1;
          if lastSel <> curSel then begin
                                    if lastSel > 0 then SelXor(lastSel,isList);
                                    SetCurs(select);
                                    lastSel := curSel;
                                    end;
       2: if press then begin
                            where := curSel;
                            if (curSel=0) or (curSel=numCmds+1) then
                                begin
                                IOBeep;
                                WaitNoPress; {wait for off}
                                goto 3;
                                end
                            else if curSel >  0 then CheckCurs := wSelect
                            else if curSel = -2 then CheckCurs := wScroll
                            else if curSel = -3 then CheckCurs := wDoIt
                            else if curSel = -1 then 
                               begin
                               if (y< yRStart-hist) or (y > yRStart+ySize+hist)
                                  or (x < xStart-hist) or (x>xStart+xSize+hist)
                                     then CheckCurs := wOutSide
                               else begin
                                    IOBeep;
                                    WaitNoPress; {wait for off}
                                    goto 3;
                                    end;
                               end;
                            exit(CheckCurs);
                            end;
       3: end {loop}
      end; {CheckCurs}
  PROCEDURE AllocateRes(size: integer);
    {------------------------------------------------------------------------
    Abstract: Allocates the ResRes res (arg to Menu) to hold size values
              and then sets the numIndices field to zero.
    Parameters: size is the number of indices that will need to store
    ------------------------------------------------------------------------}
      var p: MMPointer;
      begin
      NewP(0, 1, p, size+2); {size+2 words (1 for count, 1 for tabmouse)}
      res := RECAST(p.p, ResRes);
      res^.numIndices := 0;
      end; {AllocateRes}
  PROCEDURE WatchCursor;
    {------------------------------------------------------------------------
    Abstract: Watches the cursor, sets it depending upon where is w.r.t. 
              menu.  Sets selection if press.  Returns when done after
              setting Res.
    Environment:  Modifies res, uses specs of menu.
    ------------------------------------------------------------------------}
      var where: integer;
          whCode: WhereCode;
      begin
      if isList then for i := 0 to numCmds+1 do
                        resAr^[i] := false;
      SetCurs(select);
         while true do
            begin
            WaitNoPress;
            whCode := CheckCurs(where);
            if whCode = wScroll then HandleScroll
            else if whCode = wDoIt then
                   begin
                   if not isList then RAISE IMPOSSIBLE
                   else if numSelected = 0 then IOBeep
                   else if numSelected < 0 then RAISE IMPOSS2
                   else begin
                        AllocateRes(numSelected);
                        res^.pressVal := TabMouse;
                        for i := 1 to numCmds do
                        if resAr^[i] then begin
                                          res^.numIndices := res^.numIndices+1;
                                          res^.indices[res^.numIndices] := i;
                                          end;
                        exit(WatchCursor);
                        end
                   end   
            else if whCode = wOutside then begin
                                           DoCleanUp;
                                           Raise OutSide;
                                           end
            else begin
                 if not isList then begin
                                    AllocateRes(1);
                                    res^.pressVal := TabMouse;
                                    res^.numIndices := 1;
                                    res^.indices[1] := where;
                                    exit(WatchCursor);
                                    end
                 else begin
                      SelXor(where, true);  {remove grey}
                      SelXor(where, false); {complement box}
                      resAr^[where] := NOT resAr^[where];
                      if resAr^[where] then numSelected := numSelected + 1
                      else numSelected := numSelected - 1;
                      end;
                 end;
            end; {loop}
      end;  
  PROCEDURE SetUpCommands;
    {------------------------------------------------------------------------
    Abstract: sets commands array and numCmds.
    SideEffects: Sets commands, numCommands
    Errors: Raises BadMenu
    ------------------------------------------------------------------------}
      var fp: RECORD case Integer of
                  1 : (ptr: pNameDesc);
                  2: (seg, ofst: integer);
                  3: (p: pNameAr);
                  end;
      begin
      if last-first+1 > names^.numCommands then RAISE BadMenu;
      fp.ptr := names;
      fp.ofst := fp.ofst+(first-1)*WordSize(s25)+WordSize(s25)+1;
      commands := fp.p;
      numCmds := last-first+1;
     end; {SetUpCommands}
   PROCEDURE GetSizes;
    {------------------------------------------------------------------------
    Abstract: sets X and Y sizes.
    SideEffects: Modifies ySize, yStart, notEnough, yRStart, yRSize, XSize,
                  xStart.
    ------------------------------------------------------------------------}
      begin
     {x} xSize := MAX(maxStrW+6, minWidth);
       xStart := curX - (xSize div 2);
       if xStart < 0 then xStart := 0
       else if xStart+xSize > SBitWidth then xStart := SBitWidth-xSize;
     {y} ySize := fontHeight*(numCmds+2) + footerSize+headerSize;
      yStart := curY - (ySize div 2);
      notEnough := false;
      if ySize > maxYsize then begin
                               ySize := maxYsize;
                               yRStart := curY-(ySize div 2);
                               notEnough := true;
                               end
      else yRStart := yStart;
      if yRStart < 0 then begin
                         notEnough := true;
                         yRSize := ySize+yRStart;
                         yRStart := 0;
                         end
      else if yRStart+ySize > SCurBitHeight then begin
                                              yRSize:= SCurBitHeight-yRStart;
                                              notEnough := true;
                                              end
      else yRSize := ySize;
      end;
{---------------------}
  begin {Menu}
  started := false;
  if maxYsize < 0 then maxYsize := SCurBitHeight;
  if (first < 1) or (first > last) then RAISE BadMenu;
  SetUpCommands;
  numSelected := 0;
  GetFontP(KSet, fontP, fontHeight);
  fontHeight := fontHeight+4;
  minYsize := 4*fontHeight;
  if maxYsize < minYsize then RAISE BadMenu;
{**Allocate an array to hold string widths**}
  NewP(0, 1, pSize, numCmds+1);
  sizeAr := RECAST(pSize.p, pSizeAr);
{**Allocate an array for results if list**}
  if isList then begin
                 NewP(0, 1, pList, (numCmds+2 + 15) div 16);
                 resAr := RECAST(pList.p, pResAr);
                 end;
{**Get maxWidth**}
  maxStrW := StringW(names^.header);
  for i := 1 to numCmds do
      begin
      sizeAr^[i] := StringW(commands^[i]);
      if sizeAr^[i] > maxStrW then maxStrW := sizeAr^[i];
      end;
{**Calculate parameters**}
  if names^.header = '' then headerSize := 3
  else headerSize := FontHeight+1;
  if islist then begin
                 minWidth := 13;
                 footerSize := 14;
                 end
  else begin
       minWidth := KSet^.Index[ord('~')].width;
       footerSize := 3;
       end;
  if (curX < 0) or (curY < 0) then press := GetAction(curX, curY,true,buttons);
  if curY < minYsize then curY := minYsize
  else if curY > SCurBitHeight-minYsize then curY := SCurBitHeight-minYsize;
  GetSizes;
  if notEnough then {have to recalculate with bigger footer}
      begin
      if islist then minWidth := 51 else minWidth := 37;
      maxStrW := MAX(minWidth, maxStrW);
      footerSize := 14;
      GetSizes; {try again}
      end;
{**Assign special string to contain separator chars**}
  specString[0] := chr(MIN(maxStrW div KSet^.Index[ord('~')].width, 25));
  for i := 1 to length(specString) do
     specString[i] := '~';
  sizeAr^[0] := StringW(specString);
{**Get Memory for spares**}
  spareW := ((xSize+63) div 64) * 4; {round up to nearest multiple of 4 words}
  GetSpares(topSpare, botSpare, greySpare, spareW*fontHeight, scrSpare,
              yRSize*spareW, segNum);
{**Copy picture on screen into scrSpare**}
  RasterOp(RRpl, xSize, yRSize, 0,0,SpareW, scrSpare, xStart, yRStart,
           SScreenW, SScreenP);

{**Allocate Footers if needed}
  if footerSize <> 3 then InitFooter(scrollP, spotP, footW);

{**Now have done enough to allow cleanup}
  started := true;

{**Make border on screen for menu**}
  RasterOp(RXNor, xSize, yRSize, xStart, yRStart, SScreenW, SScreenP, xStart,
            yRStart, SScreenW, SScreenP);
  RasterOp(RXor, xSize-4, yRSize-4,xStart+2, yRStart+2, SScreenW, SScreenP,
             xStart+2, yRStart+2, SScreenW, SScreenP);
{**Erase top and bot spares**}
  RasterOp(RXor,xSize,fontHeight, 0,0,SpareW,topSpare,0,0,SpareW,topSpare);
  RasterOp(RXor,xSize,fontHeight, 0,0,SpareW,botSpare,0,0,SpareW,botSpare);

{**Write grey into grey square if needed**}
  if isList then begin
                 for i := 0 to fontHeight-1 do
                    for j := 0 to spareW-1 do
                       if i mod 2 = 0 then greySpare^[i*SpareW+j] := #122222
                       else greySpare^[i*SpareW+j] := #055555;
                 {now remove parts that should be white}
                 RasterOp(RXor, 2, fontHeight, 0,0,SpareW, greySpare, 0,0,
                           SpareW, greySpare);
                 RasterOp(RXor, (SpareW*16+4-xSize), fontHeight, xSize-2, 0,
                           SpareW, greySpare, xSize-2, 0, SpareW, greySpare);
                 RasterOp(RXor, xSize-8, fontHeight-4, 4, 2, SpareW, greySpare,
                           4, 2, SpareW, greySpare);
                 end;
{**Write Header if required**}
  if names^.header <> '' then 
      begin
      WriteString(names^.header, SScreenP, SScreenW, xStart+4, yRStart);
      RasterOp(RNot, xSize-4, FontHeight-2, xStart+2, yRStart+2,SScreenW,
                   SScreenP, xStart+2, yRStart+2, SScreenW, SScreenP);
      end;

{**Draw footer if required**}
  if footerSize <> 3 then 
      begin
      footerStart := yRstart+yRSize-footerSize;
      RasterOp(RXNor, xSize-4, footerSize-2, xStart+2, footerStart,SScreenW,
                   SScreenP, xStart+2, footerStart, SScreenW, SScreenP);
      if islist then RasterOp(RRpl, 9, 9, xStart+xSize-12, footerStart+2,
                   SScreenW, SScreenP, 0,0, footW, spotP);
      if notEnough then RasterOp(RRpl, 37, 9, xStart+2, footerStart+2,
                   SScreenW, SScreenP, 0,0, footW, scrollP);
      Dispose(scrollp);  {allocated by InitFooters}
      Dispose(spotp);
      end;

{*Now fill menu with all commands centered in their places**}
  firstVisCmd := 0 {** (yRStart-yStart) Div fontHeight **} ;
  topOffset := 0 {** (yRStart-yStart) Mod fontHeight **};
  lastVisCmd := (( 0 {*(yRstart-yStart)*}
                   +(yRsize-(headerSize+footerSize+3))) Div fontHeight);
  botOffset :=  (( 0 {*(yRstart-yStart)*}
                   +(yRsize-(headerSize+footerSize+3))) mod fontHeight);
  
{*Do top one special in case split*}
   tempY := yRStart+headerSize;
   if topOffset <> 0 then begin
                         dum := 0;
                         DrawString(firstVisCmd, topSpare, spareW, 0, dum);
                        RasterOp(RRpl, xSize-6, fontHeight-topOffset, xStart+4,
                                 tempY, SScreenW, SScreenP, 0, topOffset,
                                 SpareW, topSpare);
                         tempY := tempY+(fontHeight-topOffset);
                         end
   else DrawString(firstVisCmd, SScreenP, SScreenW, xStart+4, tempY);
  
{*Now do rest until second to last*}
  for i := firstVisCmd+1 to lastVisCmd-1 do
      DrawString(i, SScreenP, SScreenW, xStart+4, tempY);

{*Do bot one special in case split*}
   if botOffset <> 0 then begin
                         dum := 0;
                         DrawString(lastVisCmd, topSpare, spareW, 0, dum);
                         RasterOp(RRpl, xSize-6, botOffset, xStart+4,
                                 tempY, SScreenW, SScreenP, 0, 0,
                                 SpareW, topSpare);
                         end
   else DrawString(lastVisCmd, SScreenP, SScreenW, xStart+4, tempY);
  
SaveCurCursor;
WatchCursor;
DoCleanup;

end {Menu}

{$ifc trans then}
;
{-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
  This part creates or replays a transcript of a popup session.  It is
  used by the demo sequence.  Change PopUp to a program if Trans is true.
 -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-}

Type bigAr =ARRAY[1..150] of s25;
     pbigAr = ^bigAr;
var i, j, seg, blks: Integer;
    cmds: pBigAr;
    res: ResRes;
    isList: boolean;
    names: pNameDesc;
    fn,s: String;
    broke: string[1];
    replay,press: boolean;
    fid: FileID;
    
label 2;

Handler OutSide;
  begin
  goto 2;
  end;

begin
RemDelimiters(UsrCmdLine,' /',broke);
GetSymbol(UsrCmdLine,s,' /',broke);  {'popup'}
RemDelimiters(UsrCmdLine,' /',broke);
GetSymbol(UsrCmdLine,fn,' /',broke);  {filename}
RemDelimiters(UsrCmdLine,' /',broke);
GetSymbol(UsrCmdline,s,' /',broke); {'replay'}

CnvUpper(s);
if s = 'REPLAY' then replay := true
else if s = '' then replay := false
else begin
     WriteLn('** Unknown parameter: ',s);
     exit(PopUp);
     end;

if fn = '' then begin
                WriteLn('** FileName missing');
                exit(PopUp);
                end;

NEW (cmds);

InitPopUp;
InitTranscript(popuptranscript, replay);

fid := FSLookUp(fn, blks, i);
CreateSegment(seg, blks, 1, blks);
MultiRead(fid, MakePtr(seg, 0, pDirBlk), 0, blks);
names := MakePtr(seg, 0, pNameDesc);

2: repeat
      press := GetAction(i,j,true,buttons);
   until press;
   Menu(names, true, 1, names^.numCommands, -1, -1, 300, res);

CloseTranscript;

end

{$endc}

.

><><>>><><><><><~~~~    TESTING CODE    ~~~~><><><><><><><
Handler CtlC;
  begin
  IOKeyClear;
  Writeln('^C');
  goto 1;
  end;
Handler OutSide;
  begin
  WriteLn('**OUTSIDE**');
  goto 2;
  end;
Handler BadMenu;
  begin
  WriteLn('**BAD MENU**');
  goto 1;
  end;
  
while true do 
   begin
   Write('MaxYsize: [300] ');
   if eoln then maxYsize := 300
   else read(maxYsize);
   readln;
   Write('List? [true] ');
   if eoln then isList := true
   else read(isList);
   readln;
   Write('Use auto (0), typein (1), or file (2): [2] ');
   if eoln then j := 2
   else read(j);
   readln;
   if j <> 2 then begin
                  Write('Type number of commands ( <=150 ): [100] ');
                  if eoln then num := 100
                  else Read(num);
                  ReadLn;
                  end;
   if j = 0 then
       begin
       write('Width ( <=25 ): [4] ');
       if eoln then numTimes := 4
       else read(numtimes);
       readln;
       for i := 1 to num do
          begin
          cmds^[i][0] := chr(numtimes);
          for j := 1 to numtimes do
              cmds^[i][j] := chr((i mod 10)+ord('0'));
          end;
       end
   else if j = 1 then for i := 1 to num do
         begin
         Write('Type command number ',i:1,': ');
         ReadLn(cmds^[i]);
         end
   else begin
        fn := '';
        while fn = '' do
          begin
          Write('File name: ');
          ReadLn(fn);
          end;
        Reset(f,fn);
        num := 0;
        while not eof(f) do
          begin
          num := num+1;
          ReadLn(f,cmds^[num]);
          end;
        WriteLn('found ',num:1,' names.');
        end;
   Write('first: [1] ');
   if eoln then first := 1
   else read(first);
   readln;
   Write('last: [',num:1,'] ');
   if eoln then last := num
   else read(last);
   readln;
   names.numCommands := num;
   names.commands := RECAST(cmds,pNameAr);
   Write('Type Header: ');
   readLn(names.header);
   write('Number of times: [3] ');
   if eoln then numTimes := 3
   else read(numTimes);
   readln;
   for j := 1 to numTimes do
       begin
       Writeln('*** Press tablet switch to put up menu');
       while not tabSwitch do;
       writeLn('~~~~Calling menu routine~~~~');
       Menu(names, isList, header, first, last, -1, -1, maxYsize, res);
       Write('Menu reports: ');
       for i := 1 to res^.numIndices do
         Write(res^.indices[i]:1,' ');
       DestroyRes(Res);
  2:   end; {loop}
1:   end;
end

{$endc}
.

