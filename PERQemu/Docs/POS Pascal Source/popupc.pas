{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
MODULE PopUpCurs;
{----------------------------------------------------------------------------
 Abstract:  This module sets up and sets the various cursors used by PopUp.
 Written by Brad A. Myers
 Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
  Change log:
    29-Mar-83  V2.2  Brad Myers  fixed to save current cursor when popup
    17-Aug-81  V2.1  Brad Myers  fixed comments and picture for Scroll Bar
    22-Jul-81  V2.0  Brad Myers  fixed for POS version D
    14-Nov-80  V0.1  Brad Myers  started.
----------------------------------------------------------------------------}

{-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-} EXPORTS {-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-}

Type CursType = (Default, Select, Scroll, DoIt, Bar);
     FootAr = ARRAY [0..8] of ARRAY [0..3] of Integer;
     pFootAr = ^FootAr;
     
PROCEDURE InitCurs;
PROCEDURE DestroyCurs;

PROCEDURE SetCurs(t: CursType);

PROCEDURE InitFooter(VAR scrollP: pFootAr;  VAR spotP: pFootAr;
                         VAR footW: integer);
                         
PROCEDURE SaveCurCursor;

{-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-} PRIVATE {-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-}

Imports IO_Others from IO_Others;

Var scrollcurs, doitcurs, selectcurs, barCurs, defCurs: CurPatPtr;
    defX, defY: integer;


PROCEDURE DestroyCurs;
{----------------------------------------------------------------------------
 Abstract: Deallocates storage used for cursors
 SideEffects: Deallocates storage for cursors
 Environment: Must not be called before InitCursor is called
----------------------------------------------------------------------------}
   begin
   DISPOSE(scrollCurs);
   DISPOSE(doitCurs);
   DISPOSE(selectCurs);
   DISPOSE(barCurs);
   DISPOSE(defCurs);
   end;


PROCEDURE SaveCurCursor;
{----------------------------------------------------------------------------
 Abstract: Saves the current cursor as the default.
----------------------------------------------------------------------------}
   begin
   IOReadCursPicture(defCurs, defX, defY);
   end;


PROCEDURE SetCurs(t: CursType);
{----------------------------------------------------------------------------
 Abstract: Sets the cursor to the picture specified
 parameters: t is the cursor picture to set to
----------------------------------------------------------------------------}
   begin
   Case t of 
     Default: IOLoadCursor(DefCurs, defX, defY);
     Select: IOLoadCursor(selectCurs, 5,5);
     Scroll: IOLoadCursor(scrollCurs,5,5);
     DoIt: IOLoadCursor(doItCurs,5,5);
     Bar: IOLoadCursor(barCurs,0,0);
     end;
   end; {SetCurs}


PROCEDURE InitCurs;
{----------------------------------------------------------------------------
 Abstract: allocates storage used for cursors, and sets the cursors with the
           data for the pictures
 SideEffects: allocates storage for cursors
----------------------------------------------------------------------------}
   var i,j: Integer;
   begin
   NEW(0,4,scrollCurs);
   NEW(0,4,doitCurs);
   NEW(0,4,selectCurs);
   NEW(0,4,barCurs);
   NEW(0,4,defCurs);
   for i := 0 to 63 do
       for j := 0 to 3 do
          begin
          scrollCurs^[i,j] := 0;
          doitCurs^[i,j] := 0;
          selectCurs^[i,j] := 0;
          barCurs^[i,j] := 0;
          end;

   IOReadCursPicture(defCurs, defX, defY);
   
   doitCurs^[ 0, 0] := #116200;
   doitCurs^[ 0, 2] := #7400;
   doitCurs^[ 1, 0] := #77400;
   doitCurs^[ 1, 2] := #17600;
   doitCurs^[ 2, 0] := #61400;
   doitCurs^[ 2, 2] := #37700;
   doitCurs^[ 3, 0] := #152600;
   doitCurs^[ 3, 2] := #37700;
   doitCurs^[ 4, 0] := #144600;
   doitCurs^[ 4, 2] := #77740;
   doitCurs^[ 5, 0] := #152600;
   doitCurs^[ 5, 2] := #77740;
   doitCurs^[ 6, 0] := #61400;
   doitCurs^[ 6, 2] := #177760;
   doitCurs^[ 7, 0] := #77600;
   doitCurs^[ 7, 2] := #177760;
   doitCurs^[ 8, 0] := #116700;
   doitCurs^[ 8, 2] := #177760;
   doitCurs^[ 9, 0] := #340;
   doitCurs^[ 9, 2] := #177760;
   doitCurs^[10, 0] := #160;
   doitCurs^[10, 1] := #1;
   doitCurs^[10, 2] := #177770;
   doitCurs^[11, 0] := #70;
   doitCurs^[11, 1] := #1;
   doitCurs^[11, 2] := #177770;
   doitCurs^[12, 0] := #34;
   doitCurs^[12, 1] := #1;
   doitCurs^[12, 2] := #177770;
   doitCurs^[13, 0] := #16;
   doitCurs^[13, 1] := #1;
   doitCurs^[13, 2] := #177770;
   doitCurs^[14, 0] := #7;
   doitCurs^[14, 1] := #3;
   doitCurs^[14, 2] := #177774;
   doitCurs^[15, 0] := #3;
   doitCurs^[15, 1] := #100003;
   doitCurs^[15, 2] := #177774;
   doitCurs^[16, 0] := #1;
   doitCurs^[16, 1] := #140003;
   doitCurs^[16, 2] := #177774;
   doitCurs^[17, 1] := #160003;
   doitCurs^[17, 2] := #177774;
   doitCurs^[18, 1] := #60003;
   doitCurs^[18, 2] := #177774;
   doitCurs^[19, 1] := #3;
   doitCurs^[19, 2] := #177774;
   doitCurs^[20, 1] := #3;
   doitCurs^[20, 2] := #177774;
   doitCurs^[21, 1] := #3;
   doitCurs^[21, 2] := #177774;
   doitCurs^[22, 1] := #3;
   doitCurs^[22, 2] := #177774;
   doitCurs^[23, 1] := #3;
   doitCurs^[23, 2] := #177774;
   doitCurs^[24, 1] := #3;
   doitCurs^[24, 2] := #177774;
   doitCurs^[25, 1] := #3;
   doitCurs^[25, 2] := #177774;
   doitCurs^[26, 1] := #1;
   doitCurs^[26, 2] := #177770;
   doitCurs^[27, 1] := #1;
   doitCurs^[27, 2] := #177770;
   doitCurs^[28, 1] := #1;
   doitCurs^[28, 2] := #177770;
   doitCurs^[29, 1] := #1;
   doitCurs^[29, 2] := #177770;
   doitCurs^[30, 2] := #177760;
   doitCurs^[31, 2] := #177760;
   doitCurs^[32, 2] := #177760;
   doitCurs^[33, 2] := #177760;
   doitCurs^[34, 2] := #77740;
   doitCurs^[35, 2] := #77740;
   doitCurs^[36, 2] := #37700;
   doitCurs^[37, 2] := #37700;
   doitCurs^[38, 2] := #17600;
   doitCurs^[39, 2] := #7400;
   doitCurs^[44, 2] := #17600;
   doitCurs^[45, 2] := #77740;
   doitCurs^[46, 2] := #177760;
   doitCurs^[47, 1] := #3;
   doitCurs^[47, 2] := #177774;
   doitCurs^[48, 1] := #3;
   doitCurs^[48, 2] := #177774;
   doitCurs^[49, 1] := #7;
   doitCurs^[49, 2] := #177776;
   doitCurs^[50, 1] := #7;
   doitCurs^[50, 2] := #177776;
   doitCurs^[51, 1] := #17;
   doitCurs^[51, 2] := #177777;
   doitCurs^[52, 1] := #17;
   doitCurs^[52, 2] := #177777;
   doitCurs^[53, 1] := #17;
   doitCurs^[53, 2] := #177777;
   doitCurs^[54, 1] := #17;
   doitCurs^[54, 2] := #177777;
   doitCurs^[55, 1] := #17;
   doitCurs^[55, 2] := #177777;
   doitCurs^[56, 1] := #17;
   doitCurs^[56, 2] := #177777;
   doitCurs^[57, 1] := #7;
   doitCurs^[57, 2] := #177776;
   doitCurs^[58, 1] := #7;
   doitCurs^[58, 2] := #177776;
   doitCurs^[59, 1] := #3;
   doitCurs^[59, 2] := #177774;
   doitCurs^[60, 1] := #3;
   doitCurs^[60, 2] := #177774;
   doitCurs^[61, 2] := #177760;
   doitCurs^[62, 2] := #77740;
   doitCurs^[63, 2] := #17600;
   
   scrollCurs^[ 0, 0] := #116200;
   scrollCurs^[ 1, 0] := #77400;
   scrollCurs^[ 2, 0] := #61400;
   scrollCurs^[ 3, 0] := #152600;
   scrollCurs^[ 4, 0] := #144600;
   scrollCurs^[ 5, 0] := #152600;
   scrollCurs^[ 6, 0] := #61400;
   scrollCurs^[ 7, 0] := #77600;
   scrollCurs^[ 8, 0] := #116700;
   scrollCurs^[ 9, 0] := #340;
   scrollCurs^[10, 0] := #160;
   scrollCurs^[11, 0] := #70;
   scrollCurs^[12, 0] := #34;
   scrollCurs^[13, 0] := #16;
   scrollCurs^[14, 0] := #7;
   scrollCurs^[15, 0] := #3;
   scrollCurs^[15, 1] := #100000;
   scrollCurs^[16, 0] := #1;
   scrollCurs^[16, 1] := #140000;
   scrollCurs^[17, 1] := #160000;
   scrollCurs^[18, 1] := #60000;
   scrollCurs^[22, 1] := #140;
   scrollCurs^[23, 1] := #140;
   scrollCurs^[24, 1] := #140;
   scrollCurs^[25, 1] := #770;
   scrollCurs^[26, 1] := #417;
   scrollCurs^[26, 2] := #140000;
   scrollCurs^[27, 1] := #410;
   scrollCurs^[27, 2] := #20014;
   scrollCurs^[28, 1] := #410;
   scrollCurs^[28, 2] := #10014;
   scrollCurs^[29, 1] := #410;
   scrollCurs^[29, 2] := #6014;
   scrollCurs^[30, 1] := #412;
   scrollCurs^[30, 2] := #101077;
   scrollCurs^[31, 1] := #410;
   scrollCurs^[31, 2] := #20741;
   scrollCurs^[32, 1] := #410;
   scrollCurs^[32, 2] := #4041;
   scrollCurs^[33, 1] := #412;
   scrollCurs^[33, 2] := #122041;
   scrollCurs^[34, 1] := #410;
   scrollCurs^[34, 2] := #10441;
   scrollCurs^[35, 1] := #410;
   scrollCurs^[35, 2] := #4041;
   scrollCurs^[36, 1] := #410;
   scrollCurs^[36, 2] := #2041;
   scrollCurs^[37, 1] := #412;
   scrollCurs^[37, 2] := #120441;
   scrollCurs^[38, 1] := #410;
   scrollCurs^[38, 2] := #10241;
   scrollCurs^[39, 1] := #417;
   scrollCurs^[39, 2] := #146041;
   scrollCurs^[40, 1] := #770;
   scrollCurs^[40, 2] := #21041;
   scrollCurs^[41, 1] := #140;
   scrollCurs^[41, 2] := #10241;
   scrollCurs^[42, 1] := #140;
   scrollCurs^[42, 2] := #6041;
   scrollCurs^[43, 1] := #140;
   scrollCurs^[43, 2] := #1041;
   scrollCurs^[44, 2] := #741;
   scrollCurs^[45, 2] := #77;
   scrollCurs^[46, 2] := #14;
   scrollCurs^[47, 2] := #14;
   scrollCurs^[48, 2] := #14;

   selectCurs^[ 0, 0] := #116200;
   selectCurs^[ 1, 0] := #77400;
   selectCurs^[ 2, 0] := #61400;
   selectCurs^[ 3, 0] := #152600;
   selectCurs^[ 4, 0] := #144600;
   selectCurs^[ 5, 0] := #152600;
   selectCurs^[ 6, 0] := #61400;
   selectCurs^[ 7, 0] := #77600;
   selectCurs^[ 8, 0] := #116700;
   selectCurs^[ 9, 0] := #340;
   selectCurs^[10, 0] := #160;
   selectCurs^[11, 0] := #70;
   selectCurs^[12, 0] := #34;
   selectCurs^[13, 0] := #16;
   selectCurs^[14, 0] := #7;
   selectCurs^[15, 0] := #3;
   selectCurs^[15, 1] := #100000;
   selectCurs^[16, 0] := #1;
   selectCurs^[16, 1] := #140000;
   selectCurs^[17, 1] := #160000;
   selectCurs^[18, 1] := #60000;

   for i := 0 to 8 do
      barCurs^[i, 0] := #140000;
   end; {InitCurs}


PROCEDURE InitFooter(VAR scrollP: pFootAr;  VAR spotP: pFootAr;
                         VAR footW: integer);
{-----------------------------------------------------------------------------
Abstract: Allocates storage for pointers and fills in with right pictures.
Parameters: all parameters are set by InitFooter.  scrollp is the pointer to
            rasterOp from for the scroll bar and spotp is the pointer to the
            raster for the spot.  footW is the width of the array.
-----------------------------------------------------------------------------}
    begin
    NEW(0, 4, scrollp);
    NEW(0, 4, spotp);
    
    scrollp^[ 0, 0] := #156000;
    scrollp^[ 0, 1] := #1;
    scrollp^[ 0, 2] := #154000;
    scrollp^[ 1, 0] := #156400;
    scrollp^[ 1, 1] := #5;
    scrollp^[ 1, 2] := #104000;
    scrollp^[ 2, 0] := #156400;
    scrollp^[ 2, 1] := #5;
    scrollp^[ 3, 0] := #156410;
    scrollp^[ 3, 1] := #20205;
    scrollp^[ 3, 2] := #50000;
    scrollp^[ 4, 0] := #156777;
    scrollp^[ 4, 1] := #177775;
    scrollp^[ 4, 2] := #154000;
    scrollp^[ 5, 0] := #52410;
    scrollp^[ 5, 1] := #20205;
    scrollp^[ 5, 2] := #154000;
    scrollp^[ 6, 0] := #2400;
    scrollp^[ 6, 1] := #5;
    scrollp^[ 6, 2] := #154000;
    scrollp^[ 7, 0] := #106400;
    scrollp^[ 7, 1] := #5;
    scrollp^[ 7, 2] := #154000;
    scrollp^[ 8, 0] := #156000;
    scrollp^[ 8, 1] := #1;
    scrollp^[ 8, 2] := #154000;

    spotp^[ 0, 0] := #100200;
    spotp^[ 1, 0] := #40400;
    spotp^[ 2, 0] := #21000;
    spotp^[ 3, 0] := #12000;
    spotp^[ 4, 0] := #4000;
    spotp^[ 5, 0] := #12000;
    spotp^[ 6, 0] := #21000;
    spotp^[ 7, 0] := #40400;
    spotp^[ 8, 0] := #100200;
    
    footW := 4;
    
    end. {InitFooter}
