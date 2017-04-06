{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
MODULE Raster;
{--------------------------------------------------------------
  Support definitions for RasterOp
  
  Copywrite (C) 1980, 1981, 1982, 1983 - The Three Rivers Computer Corporation
--------------------------------------------------------------}

{--------------------------------------------------------------
 Versions:
  16-Nov-82  0.2 Bill Braucher Fixed names for 14-character compiler.
   2-Jun-81  0.1 Brad Myers    Added comments
  ??-???-80  0.0 ??            Started
--------------------------------------------------------------}

EXPORTS

  Const RRpl    = 0;       { Raster Op function codes }
        RNot    = 1;
        RAnd    = 2;
        RAndNot = 3;
        ROr     = 4;
        ROrNot  = 5;
        RXor    = 6;
        RXNor   = 7;
        
  Type RasterPtr = ^RasterArray; {a pointer that can be used as RasterOp 
                                    or Line source and destination }
       RasterArray = Array[0..0] of integer;

{**************************} PRIVATE {****************************}

Procedure CompilerBug;
begin end.
