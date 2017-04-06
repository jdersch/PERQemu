{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Arith;
{----------------------------------------------------------------------------
 Implements interim Double precision arithmetic package
      Needed until Pascal compiler supports type long.
 Copyright (C) 1980 Carnegie-Mellon University
 Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation
----------------------------------------------------------------------------}
{----------------------------------------------------------------------------
 Versions:
   1-Jun-81  Brad Myers       V2.2 Added comments
  14-Apr-81  George Robertson V2.1 Fixed bug in IntDouble
  12-Apr-81  George Robertson V2.0 Converted to use microcode support
  19-Mar-81  Brad Myers       V1.3 Fixed to import FileDefs
   2-Feb-81  George Robertson at Carnegie-Mellon University
                              V1.0 Fixed DoubleInt to handle negative integers.
                                   Fixed DoubleMul to handle
                                   multiplicands > 2^15
                                   by using Knuth's algorithm on p.233 of Knuth
                                   vol.2.
----------------------------------------------------------------------------}
exports

imports FileDefs from FileDefs;  { to get FSBitnn }

type
  MyDouble = packed record
             case integer of
               1:
                 (
                   Lsw : integer;
                   Msw : integer
                 );
               2:
                 (
                   Ptr : FSBit32
                 );
               3:
                 (
                   Byte0 : FSBit8;
                   Byte1 : FSBit8;
                   Byte2 : FSBit8;
                   Byte3 : FSBit8
                 )
             end;
             
function DoubleAdd(a,b : FSBit32) : FSBit32;
function DoubleSub(a,b : FSBit32) : FSBit32;
function DoubleNeg(a   : FSBit32) : FSBit32;
function DoubleMul(a,b : FSBit32) : FSBit32;
function DoubleDiv(a,b : FSBit32) : FSBit32;
function DoubleInt(a   : integer) : FSBit32;
function IntDouble(a   : FSBit32) : integer;
function DoubleBetween(a,start,stop : FSBit32) : boolean;

function DoubleMod(a,b : FSBit32) : FSBit32;
function DoubleAbs(a   : FSBit32) : FSBit32;

function DblEql(a,b : FSBit32) : boolean;
function DblNeq(a,b : FSBit32) : boolean;
function DblLeq(a,b : FSBit32) : boolean;
function DblLes(a,b : FSBit32) : boolean;
function DblGeq(a,b : FSBit32) : boolean;
function DblGtr(a,b : FSBit32) : boolean;

private

const
    {$Include perq.qcodes.dfs}

type
    look1 = (longint, rlong);
    look2 = (iinteger, bboolean);

var
    Breech1 : record
                case look1 of
                  longint : (lowword  : integer;
                             highword : integer);
                  rlong   : (longword : FSBit32);
                end;
    
    Breech2 : record
                case look2 of
                  iinteger : (word    : integer);
                  bboolean : (veritas : boolean);
                end;

Function DoubleAdd( a,b : FSBit32) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Adds two doubles together
 Parameters: a and b are doubles to add
 Returns: a+b
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(ADL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleAdd := longword;
   end;
  end;


Function DoubleSub(a,b : FSBit32) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Subtracts b from a
 Parameters: a and b are doubles
 Returns: a-b
 Design: a+(-b)
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(SBL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleSub := longword;
   end;
  end;

Function DoubleNeg(a : FSBit32 ) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Does a two-s complement negation of argument
 Parameters: a is number to negate
 Returns: -a
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(NGL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleNeg := longword;
   end;
  end;

Function DoubleAbs(a : FSBit32 ) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Does an absolute value of argument
 Parameters: a is number to abs
 Returns: |a|
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(ABL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleAbs := longword;
   end;
  end;

Function DoubleMul(a,b : FSBit32) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Multiplies a and b
 Parameters: a and b are doubles
 Returns: a*b
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(MPL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleMul := longword;
   end;
  end;


Function DoubleDiv(a,b : FSBit32) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Divides a by b
 Parameters: a and b are doubles
 Returns: a/b
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(DVL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleDiv := longword;
   end;
  end;


Function DoubleMod(a,b : FSBit32) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: Mods a by b
 Parameters: a and b are doubles
 Returns: a mod b
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(MODL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleMod := longword;
   end;
  end;


Function DoubleInt(a : integer) : FSBit32;
{----------------------------------------------------------------------------
 Abstract: converts a into a double
 Parameters: a is integer
 Returns: double of a; if a is negative then does a sign extend
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    LoadExpr(a);
    InLineByte(LOPS);
    InLineByte(CVTIL);
    StorExpr(lowword);
    StorExpr(highword);
    DoubleInt := longword;
   end;
  end;


Function IntDouble(a : FSBit32 ) : integer;
{----------------------------------------------------------------------------
 Abstract: returns the low word of a
 Parameters: a is a double
 Returns: low word
 Errors: Micro-code raises OvflLI (in Except) if a will not fit in one word
----------------------------------------------------------------------------}
  begin
  with breech1 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(CVTLI);
    StorExpr(lowword);
    IntDouble := lowword;
   end;
  end;
  

Function DoubleBetween(a,start,stop : FSBit32) : boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a is between start and stop (inclusive)
 Parameters: a is a double to test; start is low double and stop is high
 Returns: true if a >= start and a <= stop else false
----------------------------------------------------------------------------}
  var 
    da : MyDouble;
  
  begin
    da.Ptr := DoubleSub(a,start);
    if da.Msw < 0 then 
      DoubleBetween := false
    else 
      begin
        da.Ptr := DoubleSub(stop,a);
        if da.Msw < 0 then
          DoubleBetween := false
        else
          DoubleBetween := true;
      end;
  end;

function DblEql(a,b : FSBit32): boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a = b
 Parameters: a and b are doubles
 Returns: true if a = b; else false
----------------------------------------------------------------------------}
  begin
  with breech1, breech2 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(EQULong);
    StorExpr(veritas);
    DblEql := veritas;
   end;
  end;

function DblNeq(a,b : FSBit32): boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a <> b
 Parameters: a and b are doubles
 Returns: true if a <> b; else false
----------------------------------------------------------------------------}
  begin
  with breech1, breech2 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(NEQLong);
    StorExpr(veritas);
    DblNeq := veritas;
   end;
  end;

function DblLeq(a,b : FSBit32) : boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a <= b
 Parameters: a and b are doubles
 Returns: true if a <= b; else false
----------------------------------------------------------------------------}
  begin
  with breech1, breech2 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(LEQLong);
    StorExpr(veritas);
    DblLeq := veritas;
   end;
  end;

function DblLes(a,b : FSBit32) : boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a < b
 Parameters: a and b are doubles
 Returns: true if a < b; else false
----------------------------------------------------------------------------}
  begin
  with breech1, breech2 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(LESLong);
    StorExpr(veritas);
    DblLes := veritas;
   end;
  end;

function DblGeq(a,b : FSBit32) : boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a >= b
 Parameters: a and b are doubles
 Returns: true if a >= b; else false
----------------------------------------------------------------------------}
  begin
  with breech1, breech2 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(GEQLong);
    StorExpr(veritas);
    DblGeq := veritas;
   end;
  end;

function DblGtr(a,b : FSBit32) : boolean;
{----------------------------------------------------------------------------
 Abstract: determines whether a > b
 Parameters: a and b are doubles
 Returns: true if a > b; else false
----------------------------------------------------------------------------}
  begin
  with breech1, breech2 do
   begin
    longword := a;
    LoadExpr(highword);
    LoadExpr(lowword);
    longword := b;
    LoadExpr(highword);
    LoadExpr(lowword);
    InLineByte(LOPS);
    InLineByte(GTRLong);
    StorExpr(veritas);
    DblGtr := veritas;
   end;
  end.
      
