{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module RealFunctions;
{-----------------------------------------------------------------------------
{
{ RealFunctions - Standard functions for reals.
{ J. Strait   27 Nov 81.
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       RealFunctions implements many of the standard functions whose
{       domain and/or range is the set of real numbers.  The
{       implementation of these functions was guided by the book
{
{       Software Manual for the Elementary functions,
{       William J. Cody, Jr.  and  William Waite,
{       (C) 1980 by Prentice-Hall, Inc.
{
{       The domain (inputs) and range (outputs) of the functions are
{       given in their abstract.  The following notation is used.
{       Parentheses () are used for open intervals (those that do not
{       include the endpoints), and brackets [] are used for closed
{       intervals (those that do include their endpoints).  The closed
{       interval [RealMLargest, RealPLargest] is used to mean all real
{       numbers, and the closed interval [-32768, 32767] is used to
{       mean all integer numbers.
{
{       DISCLAIMER:
{
{       Only the most cursory testing of these functions has been done.
{       No guarantees are made as to the accuracy or correctness of the
{       functions.  Validation of the functions must be done, but at
{       some later date.
{
{ Design:
{       AdX, IntXp, SetXp, and Reduce are implemented as Pascal
{       functions.  It is clear that replacing the calls with in-line
{       code (perhaps through a macro expansion) would improve the
{       efficiency.
{
{       Many temporary variables are used.  Elimination of unnecessary
{       temporaries would also improve the efficiency.
{
{       Many limit constants have been chosen conservatively, thus
{       trading a small loss in range for a guarantee of correctness.
{       The choice of these limits should be re-evaluated by someone
{       with a better understanding of the issues.
{
{       Some constants are expressed in decimal (thus losing the
{       guarantee of precision).  Others are expressed as Sign,
{       Exponent, and Significand and are formed at execution time.
{       Converting these two 32-bit constants which are Recast into
{       real numbers would improve the correctness and efficiency.
{
{       More thought needs to be given to the values which are returned
{       after resuming from an exception.  The values that are returned
{       now are the ones recommended by Cody and Waite.  It seems that
{       Indefinite values (NaNs in the IEEE terminology) might make
{       more sense in some cases.
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{
{ Change history:
{
{ 20 Aug 82  V1.5  Michael R. Kristofic
{ Applied log10 function fix as given by Colin McPhail
{
{ 11 May 82  V1.4  JBBrodie
{ Restated Constants PiInv, r1, r2, r3, and r4 in SinCos as exact bit patterns
{
{  5 May 82  V1.3  JBBrodie
{ Constrained Sin/Cos domain to Trunc(Pi*2^12) (e.g. YMax in SinCos)
{ Restated Constants Eps, C1, and C2 in SinCos as exact bit patterns
{ Repaired the setting of the sign bit for SinCos results < Eps
{
{  3 May 82  V1.2  S L Brown
{ Fix the exponent sign of the constant q3 in Power function.  (It was a
{ '+' but it should have been a '-'.)
{
{  1 Mar 82  V1.1  JBBrodie
{ Addition of the Hyperbolic functions
{
{ 27 Nov 81  V1.0  J. Strait
{ Start module.
{
{-----------------------------------------------------------------------------}

exports


const RealPInfinity =   Recast(#17740000000,Real);   {  1.0 / 0.0 }
      RealMInfinity =   Recast(#37740000000,Real);   { -1.0 / 0.0 }
      RealPIndefinite = Recast(#00000000001,Real);   {  0.0 / 0.0 }
      RealMIndefinite = Recast(#20000000001,Real);   { -0.0 / 0.0 }
      RealPLargest =    Recast(#17737777777,Real);   { largest positive }
      RealMLargest =    Recast(#37737777777,Real);   { largest negative }
      RealPSmallest =   Recast(#00040000000,Real);   { smallest positive }
      RealMSmallest =   Recast(#20040000000,Real);   { smallest negative }


function Sqrt( X: Real ): Real;
function Exp( X: Real ): Real;
function Ln( X: Real ): Real;
function Log10( X: Real ): Real;
function Power( X, Y: Real ): Real;
function PowerI( X: Real; Y: Integer ): Real;
function Sin( X: Real ): Real;
function Cos( X: Real ): Real;
function Tan( X: Real ): Real;
function CoTan( X: Real ): Real;
function ArcSin( X: Real ): Real;
function ArcCos( X: Real ): Real;
function ArcTan( X: Real ): Real;
function ArcTan2( Y, X: Real ): Real;
function SinH( x:real ) : real;
function CosH( x:real ) : real;
function TanH( x:real ) : real;




exception SqrtNeg( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SqrtNeg is raised when Sqrt is passed a negative argument.
{       You may resume from this exception, in which case Sqrt returns
{       Sqrt(Abs(X)).
{
{ Parameters:
{       X - Argument of Sqrt.
{
{-----------------------------------------------------------------------------}




exception ExpLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ExpLarge is raised when Exp is passed an argument which is too
{       large.  You may resume from this exception, in which case Exp
{       returns RealPInfinity.
{
{ Parameters:
{       X - Argument of Exp.
{
{-----------------------------------------------------------------------------}




exception ExpSmall( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ExpLarge is raised when Exp is passed an argument which is too
{       small.  You may resume from this exception, in which case Exp
{       returns 0.0.
{
{ Parameters:
{       X - Argument of Exp.
{
{-----------------------------------------------------------------------------}




exception LogSmall( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       LogSmall is raise when Ln or Log10 is passed an argument which
{       is too small.  You may resume from this exception in which case
{       Ln or Log10 returns RealMInfinity if X is zero or the log of
{       Abs(X) if X is non-zero.
{
{ Parameters:
{       X - Argument of Ln or Log10.
{
{-----------------------------------------------------------------------------}




exception PowerZero( X, Y: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       PowerZero is raised when Power or PowerI is called with X = 0.0
{       and Y = 0.0.  You may resume from this exception in which case
{       Power or PowerI returns RealPInfinity.
{
{ Parameters:
{       X - Argument of Power or PowerI.
{       Y - Argument of Power or PowerI.
{
{-----------------------------------------------------------------------------}




exception PowerNeg( X, Y: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       PowerNeg is raised when Power is called with X < 0.0 or with
{       X = 0.0 and Y < 0.0, or PowerI is called with X = 0.0 and
{       Y < 0.  You may resume from this exception in which case Power
{       or PowerI returns Power(Abs(X),Y) in the case of X < 0.0 or
{       returns RealPInfinity in the case of X = 0.0 and Y < 0.0.
{
{ Parameters:
{       X - Argument of Power or PowerI.
{       Y - Argument of Power or PowerI.
{
{-----------------------------------------------------------------------------}




exception PowerBig( X, Y: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       PowerBig is raised when Power or PowerI is called with X and Y
{       for which X raised to the Y power is too large to be represented.
{       You may resume from this exception in which case Power or PowerI
{       returns RealPInfinity.
{
{ Parameters:
{       X - Argument of Power or PowerI.
{       Y - Argument of Power or PowerI.
{
{-----------------------------------------------------------------------------}




exception PowerSmall( X, Y: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       PowerSmall is raised when Power or PowerI is called with X and Y
{       for which X raised to the Y is too close to zero to be represented.
{       You may resume from this exception in which case Power or PowerI
{       returns 0.0.
{
{ Parameters:
{       X - Argument of Power or PowerI.
{       Y - Argument of Power or PowerI.
{
{-----------------------------------------------------------------------------}




exception SinLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SinLarge is raised when Sin is called with an argument which is
{       too large.  You may resume from this exception in which case
{       Sin returns 0.0.
{
{ Parameters:
{       X - Argument of Sin.
{
{-----------------------------------------------------------------------------}




exception CosLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CosLarge is raised when Cos is called with an argument which is
{       too large.  You may resume from this exception in which case
{       Cos returns 0.0.
{
{ Parameters:
{       X - Argument of Cos.
{
{-----------------------------------------------------------------------------}




exception TanLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CosLarge is raised when Tan or CoTan is called with an argument
{       which is too large.  You may resume from this exception in which
{       case Tan or CoTan returns 0.0.
{
{ Parameters:
{       X - Argument of Tan or CoTan.
{
{-----------------------------------------------------------------------------}




exception ArcSinLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ArcSinLarge is raised when ArcSin is called with an argument
{       which is too large.  You may resume from this exception in which
{       case ArcSin returns RealPInfinity.
{
{ Parameters:
{       X - Argument of ArcSin.
{
{-----------------------------------------------------------------------------}




exception ArcCosLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ArcCosLarge is raised when ArcCos is called with an argument
{       which is too large.  You may resume from this exception in which
{       case ArcCos returns RealPInfinity.
{
{ Parameters:
{       X - Argument of ArcCos.
{
{-----------------------------------------------------------------------------}




exception ArcTan2Zero( Y, X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ArcTan2Zero is raised when ArcTan2 is called with both X and Y
{       equal to zero.  You may resume from this exception in which
{       case ArcTan2 returns RealPInfinity.
{
{ Parameters:
{       Y - Argument of ArcTan2.
{       X - Argument of ArcTan2.
{
{-----------------------------------------------------------------------------}




exception SinHLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SinHLarge is raised when the arqument to SinH would cause a
{       result whose magnitude is too large to be represented on the Perq.
{       Note that SinH is implemented (for now at least) is terms of the 
{       Exp function and that function is the bound on SinH domain.
{
{ Parameters:
{       X - Argument of SinH
{
{-----------------------------------------------------------------------------}





exception CosHLarge( X: Real );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CosHLarge is raised when the arqument to CosH would cause a
{       result whose magnitude is too large to be represented on the Perq.
{       Note that CosH is implemented (for now at least) is terms of the 
{       Exp function and that function is the bound on CosH domain.
{
{ Parameters:
{       X - Argument of CosH
{
{-----------------------------------------------------------------------------}





private

const SqrtP5Upper = #065;
      SqrtP5Lower = #002363;

type Sign = (Positive, Negative);
     RealRep = packed record case integer of
                 1: (R:      Real);
                 2: (L:      Long);
                 3: (FLower: Integer;
                     FUpper: 0..127;
                     E:      0..255;
                     S:      Sign);
                 4: (LUpper: Integer;
                     LLower: Integer)
                 end;

function AdX( X: Real; I: Integer ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Augment the exponent of a floating-point number.
{
{ Parameters:
{       X - Input value.
{       I - Value used to augment the exponent
{
{ Returns:
{       X * 2^I.
{
{-----------------------------------------------------------------------------}

var RRX: RealRep;
begin { AdX }
  if X = 0.0 then AdX := 0.0
  else
    begin
      RRX.R := X;
      RRX.E := RRX.E + I;
      AdX := RRX.R
    end
end { AdX };

function IntXp( X: Real ): Integer;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Read the exponent of a floating-point number.
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Integer exponent of X.
{
{-----------------------------------------------------------------------------}

var RRX: RealRep;
begin { IntXp }
  if X = 0 then IntXp := 0
  else
    begin
      RRX.R := X;
      IntXp := RRX.E - 126
    end
end { IntXp };

function SetXp( X: Real; I: Integer ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Set the exponent of a floating-point number.
{
{ Parameters:
{       X - Input value.
{       I - New exponent.
{
{ Returns:
{       A real number with the mantissa of X and I for an exponent.
{
{-----------------------------------------------------------------------------}

var RRX: RealRep;
begin { SetXp }
  if X = 0.0 then SetXp := 0.0
  else
    begin
      RRX.R := X;
      RRX.E := I + 126;
      SetXp := RRX.R
    end
end { SetXp };

function Sqrt( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the square-root of a number.
{       
{       Domain = [0.0, RealPLargest].
{       Range  = [0.0, Sqrt(RealPLargest)].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Square-root of X.
{
{-----------------------------------------------------------------------------}

var f, y0, z, y2, T: Real;
    SqrtP5: RealRep;
    N: Integer;
begin { Sqrt }
  if X = 0.0 then Sqrt := 0.0
  else
    begin
      if X < 0.0 then
        begin
          raise SqrtNeg(X);
          X := -X
        end;
      N := IntXp(X);
      f := SetXp(X,0);
      y0 := 0.41731 + 0.59016 * f;
      z := y0 + f / y0;
      y2 := AdX(z,-2) + f / z;
      if Odd(N) then
        begin
          { form SqrtP5 = Sqrt(0.5) }
          SqrtP5.S := Positive;
          SqrtP5.E := 126;
          SqrtP5.FUpper := SqrtP5Upper;
          SqrtP5.FLower := SqrtP5Lower;   { Sqrt(0.5) }
          y2 := y2 * SqrtP5.R;
          N := N + 1
        end;
      Sqrt := AdX(y2,N div 2)
    end
end { Sqrt };

function Ln( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the natural log of a number.
{       
{       Domain = [0.0, RealPLargest].
{       Range  = [RealMLargest, Ln(RealPLargest)].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Natural log of X.
{
{-----------------------------------------------------------------------------}

const a0 = -0.5527074855E+0;   { unfortunately not rounded }
      b0 = -0.6632718214E+1;   { unfortunately not rounded }
      b1 =  0.1000000000E+1;   { unfortunately not rounded }
      C1Upper = #061;
      C1Lower = #100000;
      C2 = -2.121944400E-4;    { unfortunately not rounded }
var C0, C1: RealRep;
    f, z, w, Aw, Bw, Rzz, Rz, XN, znum, zden: Real;
    N: Integer;
begin { Ln }
  if X <= 0.0 then
    begin
      raise LogSmall(X);
      if X = 0.0 then
        begin
          Ln := RealMInfinity;
          Exit(Ln)
        end
      else X := Abs(X)
    end;
  N := IntXp(X);
  f := SetXp(X,0);
  { form C0 = Sqrt(0.5) }
  C0.S := Positive;
  C0.E := 126;
  C0.FUpper := SqrtP5Upper;
  C0.FLower := SqrtP5Lower;   { Sqrt(0.5) }
  if f > C0.R then
    begin
      znum := (f - 0.5) - 0.5;
      zden := f * 0.5 + 0.5
    end
  else
    begin
      N := N - 1;
      znum := f - 0.5;
      zden := znum * 0.5 + 0.5
    end;
  z := znum / zden;
  w := z * z;
  Aw := a0;
  Bw := w + b0;
  Rzz := w * Aw / Bw;
  Rz := z + z * Rzz;
  XN := N;
  { form C1 }
  C1.S := Positive;
  C1.E := 126;
  C1.FUpper := C1Upper;
  C1.FLower := C1Lower;
  Ln := (XN * C2 + Rz) + XN * C1.R
end { Ln };

function Log10( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the log to the base 10 of a number.
{       
{       Domain = [0.0, RealPLargest].
{       Range  = [RealMLargest, Log10(RealPLargest)].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Log to the base 10 of X.
{
{ Calls:
{       Ln
{
{-----------------------------------------------------------------------------}

const C3Upper = #136;
      C3Lower = #055745;
var C3: RealRep;
begin { Log10 }
  { form C3 }
  C3.S := Positive;
  C3.E := 125;
  C3.FUpper := C3Upper;
  C3.FLower := C3Lower;
  Log10 := C3.R * Ln(X)
end { Log10 };

function Exp( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the exponential function.
{       
{       Domain = [-87.336, 88.722].
{       Range  = (0.0, RealPLargest].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       e raised to the X power.
{
{-----------------------------------------------------------------------------}

const BigX = 88.722;      { This is somewhat too small.  Fix it later.-JPS }
      SmallX = -87.336;   { This is somewhat too large.  Fix it later.-JPS }
{ Heed! If you change the Domain of the Exp function be sure to reflect that
        change into the Hyperbolic functions (CosH and SinH)   -JBB }
      Eps = Recast(#06300000000,Real); {2^-25}
      Ln2Inv = 1.442695040;   { 1/Ln(2) }
      C1 = Recast(#7714300000,Real); { Is the correct constant 0.543? -JPS }
      C2 = -2.121944400E-4;    { unfortunately not rounded }
      p0 = 0.24999999950E+0;   { unfortunately not rounded }
      p1 = 0.41602886268E-2;   { unfortunately not rounded }
      q0 = 0.50000000000E+0;   { unfortunately not rounded }
      q1 = 0.49987178778E-1;   { unfortunately not rounded }
var X1, X2, XN, g, z, gPz, Qz, Rg: Real;
    N: Integer;
begin { Exp }
  if X > BigX then
    begin
      raise ExpLarge(X);
      Exp := RealPInfinity
    end
  else
    if X < SmallX then
      begin
        raise ExpSmall(X);
        Exp := 0.0
      end
    else
      if Abs(X) < Eps then Exp := 1.0
      else
        begin
          N := Round(X * Ln2Inv);
          X1 := Trunc(X);
          X2 := X - X1;
          XN := N;
          g := ((X1 - XN*C1) + X2) - XN*C2;
          z := g * g;
          gPz := (p1 * z + p0) * g;
          Qz := q1 * z + q0;
          Rg := 0.5 + gPz / (Qz - gPz);
          Exp := AdX(Rg,N+1)
        end
end { Exp };

function Power( X, Y: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the result of an arbitrary number raised to an
{       arbitrary power.
{
{       DomainX = [0.0, RealPLargest].
{       DomainY = [RealMLargest,RealPLargest].
{       Range   = [0.0, RealPLargest].
{
{       With the restrictions that
{       1) if X is zero, Y must be greater than zero.
{       2) X raised to the Y is a representable real number.
{
{ Parameters:
{       X - Input value.
{       Y - Input value.
{
{ Returns:
{       X raised to the Y power.
{
{-----------------------------------------------------------------------------}

  exception PowerBad;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       PowerBad is raised when A1 or A2 is called with a bad argument.
{       This should never happen, but we provide the exception just in
{       case.
{
{-----------------------------------------------------------------------------}


  function A1( J: Integer ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute negative integer powers of 2^(-1/16).  We compute
{       2^((1-J)/16.0) for J = 1, 2, ..., 17.  Thus A1(J), J = 1...17
{       returns (2^(-1/16))^I, I = 0...-16.  The values are rounded
{       to machine precision.
{
{ Parameters:
{       J - Input value.
{
{ Returns:
{       2^((1-J)/16).
{
{-----------------------------------------------------------------------------}

  var Result: RealRep;
  begin { A1 }
    case J - 1 of
       0: Result.L := #07740000000 { #1.00000000 };
       1: Result.L := #07735222575 { #0.75222575 };
       2: Result.L := #07732540307 { #0.72540307 };
       3: Result.L := #07730146337 { #0.70146337 };
       4: Result.L := #07725642375 { #0.65642375 };
       5: Result.L := #07723422214 { #0.63422214 };
       6: Result.L := #07721263452 { #0.61263452 };
       7: Result.L := #07717204244 { #0.57204244 };
       8: Result.L := #07715202363 { #0.55202363 };
       9: Result.L := #07713254077 { #0.53254077 };
      10: Result.L := #07711377327 { #0.51377327 };
      11: Result.L := #07707572462 { #0.47572462 };
      12: Result.L := #07706033760 { #0.46033760 };
      13: Result.L := #07704341723 { #0.44341723 };
      14: Result.L := #07702717702 { #0.42717702 };
      15: Result.L := #07701325303 { #0.41325303 };
      16: Result.L := #07700000000 { #0.00000000 };
      otherwise: raise PowerBad
      end;
    A1 := Result.R
  end { A1 };

  function A2( J: Integer ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute error in negative odd integer powers of 2^(-1/16).
{       We compute 2^((1-J)/16.0) - A1(J) for J = 1, 2, ..., 8.  Thus
{       A2(J), J = 1, 3, ..., 15 returns
{       (2^(-1/16))^I - A1(I), I = -1, -3, ..., -15.  The values are
{       rounded to somewhat less than machine precision.
{       This provides a means of computing with negative odd integer
{       powers of 2^(-1/16) to approximately twice machine precision.
{
{ Parameters:
{       J - Input value.
{
{ Returns:
{       2^((1-J)/16) - A1(J).
{
{-----------------------------------------------------------------------------}

  var Result: RealRep;
  begin { A2 }
    case J - 1 of
       1: Result.L := #06152222060  {  #0.05222206*2^-24};
       3: Result.L := #26147525540  { -#0.04752554*2^-24};
       5: Result.L := #06152176040  {  #0.05217604*2^-24};
       7: Result.L := #26260247750  { -#0.30123774*2^-24};
       9: Result.L := #26153365370  { -#0.05336537*2^-24};
      11: Result.L := #06260221502  {  #0.30110641*2^-24};
      13: Result.L := #06271653042  {  #0.34725421*2^-24};
      15: Result.L := #06263714220  {  #0.31746110*2^-24};
      otherwise: raise PowerBad
      end;
    A2 := Result.R
  end { A2 };

  function Reduce( V: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute Trunc(16.0*V) / 16.0.
{
{ Parameters:
{       V - Input value.
{
{ Returns:
{       Trunc(16.0*V) / 16.0.
{
{-----------------------------------------------------------------------------}

  begin { Reduce }
    Reduce := AdX(Trunc(AdX(V,4)),-4)
  end { Reduce };

const p1 = 0.83357541E-1;
      K = 0.4426950408E+0;
      BigW1 = 16 * 127;        { is this chosen properly?  -JPS }
      SmallW1 = 16 * (-125);   { is this chosen properly?  -JPS }
      q1 = 0.69314675E+0;
      q2 = 0.24018510E+0;
      q3 = 0.54360383E-1;
var g, A1pP1, A2pP1, z, v, Rz, U2, U1, Y1, Y2, W, W1, W2, A1pprimeP1: Real;
    m, p, IW1, I, mprime, pprime: Integer;
    Big: Array[0..255] of Integer; {*****}
begin { Power }
  if X <= 0.0 then
    if X <> 0.0 then
      begin
        raise PowerNeg(X,Y);
        X := -X
      end
    else { X = 0.0 }
      if Y <= 0.0 then
        begin
          if Y = 0.0 then raise PowerZero(X,Y)
          else raise PowerNeg(X,Y);
          Power := RealPInfinity;
          Exit(Power)
        end
      else
        begin
          Power := 0.0;
          Exit(Power)
        end;
  m := IntXp(X);
  g := SetXp(X,0);
  p := 1;
  if g <= A1(9) then p := 9;
  if g <= A1(p+4) then p := p + 4;
  if g <= A1(p+2) then p := p + 2;
  A1pP1 := A1(p+1);
  A2pP1 := A2(p+1);
  z := AdX(((g - A1pP1) - A2pP1) / (g + A1pP1),1);
       { order implied by ()s is important }
  v := z * z;
  Rz := p1 * v * z;
  U2 := ((Rz + K*Rz) + z*K) + z;    { order implied by ()s is important }
  U1 := AdX(m*16.0 - p,-4);
  Y1 := Reduce(Y);
  Y2 := Y - Y1;
  W := U2*Y + U1*Y2;
  W1 := Reduce(W);
  W2 := W - W1;
  W := W1 + U1*Y1;
  W1 := Reduce(W);
  W2 := W2 + (W - W1);
  W := Reduce(W2);
  IW1 := Trunc(16.0*(W1+W));
  W2 := W2 - W;
  if IW1 > BigW1 then
    begin
      raise PowerBig(X,Y);
      Power := RealPInfinity;
      Exit(Power)
    end;
  if IW1 < SmallW1 then
    begin
      raise PowerSmall(X,Y);
      Power := 0.0;
      Exit(Power)
    end;
  if W2 > 0.0 then
    begin
      IW1 := IW1 + 1;
      W2 := W2 - AdX(1.0,-4)
    end;
  if IW1 < 0 then I := 0 else I := 1;
  mprime := IW1 div 16 + I;
  pprime := 16 * mprime - IW1;
  Z := ((q3 * W2 + q2) * W2 + q1) * W2;
  A1pprimeP1 := A1(pprime+1);
  Z := A1pprimeP1 + A1pprimeP1 * Z;
  Power := AdX(Z,mprime)
end { Power };

function PowerI( X: Real; Y: Integer ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the result of an arbitrary number raised to an
{       arbitrary integer power.  The difference between Power and
{       PowerI is that negative values of X may be passed to PowerI.
{
{       DomainX = [RealMLargest, RealPLargest].
{       DomainY = [-32768, 32767].
{       Range   = [RealMLargest, RealPLargest].
{
{       With the restrictions that
{       1) if X is zero, Y must be non-zero.
{       2) X raised to the Y is a representable real number.
{
{ Parameters:
{       X - Input value.
{       Y - Input value.
{
{ Returns:
{       X raised to the Y power.
{
{-----------------------------------------------------------------------------}

begin { PowerI }
  if X < 0.0 then
    if Odd(Y) then PowerI := -Power(-X,Y)
    else PowerI := Power(-X,Y)
  else PowerI := Power(X,Y)
end { PowerI };

function SinCos( X, Y: Real; S: Sign ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Common routine for Sin and Cos.
{
{ Parameters:
{       X - Input value.
{       Y - Abs(X) for Sin, Abs(X)+pi/2 for Cos.
{       S - Sign of result.
{
{ Returns:
{       Sin or Cosine.
{
{-----------------------------------------------------------------------------}

const YMax  =  12867.0;        { is this chosen correctly?  -JPS/JBB }
      Eps   =  Recast(#07140000000,Real); { 2^-12 }
      C1    =  Recast(#10022200000,Real); { 3.140625 exactly }
      C2    =  Recast(#07237325044,Real); { 9.6765358989793E-4}
      r1    =  Recast(#27612525244,Real); {-0.1666665668E+0}
      r2    =  Recast(#07402103475,Real); { 0.8333025139E-2}
      r3    =  Recast(#27123731042,Real); {-0.1980741872E-3}
      r4    =  Recast(#06613516133,Real); { 0.2601903036E-5}
      PiInv =  Recast(#07650574604,Real); { 0.3183098861 }
var XN, X1, X2, f, g, Rg: Real;
    Result: RealRep;
    N: Integer;
begin { SinCos }
  if Y > YMax then
    begin
      if Abs(X) <> Y then raise CosLarge(X)
      else raise SinLarge(X);
      SinCos := 0.0;
      Exit(SinCos)
    end;
  N := Round(Y * PiInv);
  XN := N;
  if Odd(N) then
    if S = Positive then S := Negative
    else S := Positive;
  if Abs(X) <> Y then XN := XN - 0.5;
  { do we have guard digit for addition? }
  X1 := Trunc(Abs(X));
  X2 := Abs(X) - X1;
  f := ((X1 - XN*C1) + X2) - XN*C2;
  if Abs(f) < Eps then Result.R := f
  else
    begin
      g := f * f;
      Rg := (((r4 * g + r3) * g + r2) * g + r1) * g;
      Result.R := f + f*Rg;
    end;
  if S = Result.S then Result.S := Positive
  else Result.S := Negative;
  SinCos := Result.R
end { SinCos };

function Sin( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the sin of a number.
{       
{       Domain = [-12867, 12867].
{       Range =  [-1.0, 1.0].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Sin of X.
{
{-----------------------------------------------------------------------------}

begin { Sin }
  if X < 0.0 then Sin := SinCos(X,-X,Negative)
  else Sin := SinCos(X,X,Positive)
end { Sin };

function Cos( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the cosin of a number.
{       
{       Domain = [-12867, 12867].
{       Range =  [-1.0, 1.0].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Cos of X.
{
{-----------------------------------------------------------------------------}

const PiD2 = 1.5707963267;   { Pi / 2 }
begin { Cos }
  Cos := SinCos(X,Abs(X)+PiD2,Positive)
end { Cos };

function TanCoTan( X, Y: Real; Tan: Boolean ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the tangent or cotangent of a number.
{
{ Parameters:
{       X - Input value.
{       Y - Absolute value of X.
{       Tan - True for Tan, false for CoTan.
{
{ Returns:
{       Tangent or cotangent of X.
{
{-----------------------------------------------------------------------------}

const YMax = 6433.0;
      TwoDPi = 0.6366197723; { 2 / Pi }
      PiD2 = 1.5707963267;   { Pi / 2 }
      C1Upper = #111;
      C1Lower = #000000;
      C2 = 4.83826794897E-4;
      Eps = 0.0 {2^-12};
      p0 =  1.000000000E+0;
      p1 = -0.958017723E-1;
      q0 =  1.000000000E+0;
      q1 = -0.429135777E+0;
      q2 =  0.971685835E-2;
var XN, X1, X2, f, XNum, XDen, g: Real;
    C1: RealRep;
    N: Integer;
begin { TanCoTan }
  if Y > YMax then
    begin
      raise TanLarge(X);
      TanCoTan := 0.0;
      Exit(TanCoTan)
    end;
  N := Round(X * TwoDPi);
  XN := N;
  { do we have guard digit for addition? }
  { form C1 }
  C1.S := Positive;
  C1.E := 127;
  C1.FUpper := C1Upper;
  C1.FLower := C1Lower;
  X1 := Trunc(X);
  X2 := X - X1;
  f := ((X1 - XN*C1.R) + X2) - XN*C2;
  if Abs(f) < {Eps} AdX(1.0,-12) then
    begin
      XNum := f;
      XDen := 1.0
    end
  else
    begin
      g := f * f;
      XNum := p1 * g * f + f;
      XDen := (q2 * g + q1) * g + q0
    end;
  if Odd(N) then
    begin
      XNum := -XNum;
      Tan := not Tan
    end;
  if Tan then TanCoTan := XNum / XDen
  else TanCoTan := XDen / XNum
end { TanCoTan };

function Tan( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the tangent of a number.
{       
{       Domain = [-6433.0, 6433.0].
{       Range  = [RealMInfinity, RealPInfinity].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Tangent of X.
{
{-----------------------------------------------------------------------------}

begin { Tan }
  Tan := TanCoTan(X,Abs(X),True)
end { Tan };

function CoTan( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the cotangent of a number.
{       
{       Domain = [-6433.0, 6433.0].
{       Range  = [RealMInfinity, RealPInfinity].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Cotangent of X.
{
{-----------------------------------------------------------------------------}

const Eps1 = 0.0 {2^-125};   { this is not small enough.  fix it later  -JPS }
begin { CoTan }
  CoTan := TanCoTan(X,Abs(X),False)
end { CoTan };

function ArcSinArcCos( X: Real; Flag: Integer ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the arcsin or arccosin of a number.
{
{ Parameters:
{       X - Input value.
{       Flag - 0 for arcsin, 1 for arccosin.
{
{ Returns:
{       Arcsin or arccosin of X.
{
{-----------------------------------------------------------------------------}

const Eps = 0.0 {2^-12};
      p1 =  0.933935835E+0;
      p2 = -0.504400557E+0;
      q0 =  0.560363004E+1;
      q1 = -0.554846723E+1;
      q2 =  0.100000000E+1;
      a0 =  0.0;
      a1Upper = #111;
      a1Lower = #007733;
      b0Upper = #111;
      b0Lower = #007733;
var Y, Result, g, gPg, Qg, Rg: Real;
    a1, b0, b1: RealRep;
    i: Integer;
    UsePolynomial: Boolean;
begin { ArcSinArcCos }
  Y := Abs(X);
  if Y > 0.5 then
    begin
      i := 1 - Flag;
      if Y > 1 then
        begin
          if Flag = 0 then raise ArcSinLarge(X)
          else raise ArcCosLarge(X);
          ArcSinArcCos := RealPInfinity;
          Exit(ArcSinArcCos)
        end;
      g := AdX((0.5-Y)+0.5,-1);    { order implied by ()s is important }
      Y := -AdX(Sqrt(g),1);
      UsePolynomial := True
    end
  else
    begin
      i := Flag;
      UsePolynomial := Y >= {Eps} AdX(1.0,-12);
      if UsePolynomial then g := Y * Y
    end;
  if UsePolynomial then
    begin
      gPg := (p2 * g + p1) * g;
      Qg := (g + q1) * g + q0;
      Rg := gPg / Qg;
      Result := Y + Y*Rg
    end
  else Result := Y;
  { form a1 }
  a1.S := Positive;
  a1.E := 126;
  a1.FUpper := a1Upper;
  a1.FLower := a1Lower;
  { form b0 }
  b0.S := Positive;
  b0.E := 127;
  b0.FUpper := b0Upper;
  b0.FLower := b0Lower;
  { form b1 }
  b1 := a1;
  if Flag = 0 then { arcsin }
    begin
      if i = 0 then { Result := (a0 + Result) + a0 }
      else
        Result := (a1.R + Result) + a1.R;
      if X < 0.0 then Result := -Result
    end
  else { arccos }
    if X < 0 then
      if i = 0 then Result := (b0.R + Result) + b0.R
      else Result := (b1.R + Result) + b1.R
    else
      if i = 0 then Result := -Result { Result := (a0 - Result) + a0 }
      else Result := (a1.R - Result) + a1.R;
  ArcSinArcCos := Result
end { ArcSinArcCos };

function ArcSin( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the arcsin of a number.
{       
{       Domain = [-1.0, 1.0).
{       Range  = [-Pi/2, Pi/2).
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Arcsin of X.
{
{ Design:
{       It seems that the Domain and Range ought to be closed intervals,
{       however this implementation apparently returns a number very close
{       to zero when X is 1.0, rather than returning Pi/2 as it should.
{
{-----------------------------------------------------------------------------}

begin { ArcSin }
  ArcSin := ArcSinArcCos(X,0)
end { ArcSin };

function ArcCos( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the arccosin of a number.
{       
{       Domain = (-1.0, 1.0].
{       Range  = (-Pi/2, Pi/2].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Arccosin of X.
{
{ Design:
{       It seems that the Domain and Range ought to be closed intervals,
{       however this implementation apparently returns a number very close
{       to zero when X is -1.0, rather than returning -Pi/2 as it should.
{
{-----------------------------------------------------------------------------}

begin { ArcCos }
  ArcCos := ArcSinArcCos(X,1)
end { ArcCos };

function ArcTanArcTan2( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Common routine for ArcTan and ArcTan2.
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Intermediate result in computation of ArcTan and ArcTan2.
{
{ Design:
{       Seems fine except for very large numbers.
{
{-----------------------------------------------------------------------------}

const TwoMUpper = #011;
      TwoMLower = #030243;
      Sqrt3M1Upper = #073;
      Sqrt3M1Lower = #063657;
      Sqrt3Upper = #135;
      Sqrt3Lower = #131727;
      Eps = 0.0 {2^-12};
      p0 = -0.4708325141E+0;
      p1 = -0.5090958253E-1;
      q0 =  0.1412500740E+1;
      q1 =  0.1000000000E+1;
      a0 = 0.0;
      a1 = 0.5235987756;
      a2 = 1.5707963268;
      a3 = 1.0471975512;
var f, g, gPg, Qg, Rg, Result: Real;
    TwoMSqrt3, Sqrt3M1, Sqrt3: RealRep;
    N: Integer;
begin { ArcTanArcTan2 }
  f := Abs(X);
  if f > 1.0 then
    begin
      f := 1.0 / f;
      N := 2
    end
  else N := 0;
  { form TwoMSqrt3 = 2 - Sqrt(3) }
  TwoMSqrt3.S := Positive;
  TwoMSqrt3.E := 125;
  TwoMSqrt3.FUpper := TwoMUpper;
  TwoMSqrt3.FLower := TwoMLower;
  if f > TwoMSqrt3.R then
    begin
      { form Sqrt3M1 = Sqrt(3)-1 }
      Sqrt3M1.S := Positive;
      Sqrt3M1.E := 126;
      Sqrt3M1.FUpper := Sqrt3M1Upper;
      Sqrt3M1.FLower := Sqrt3M1Lower;
      { form Sqrt3 = Sqrt(3) }
      Sqrt3.S := Positive;
      Sqrt3.E := 127;
      Sqrt3.FUpper := Sqrt3Upper;
      Sqrt3.FLower := Sqrt3Lower;
      f := (((Sqrt3M1.R * f - 0.5) - 0.5) + f) / (Sqrt3.R + f);
      N := N + 1
    end;
  if Abs(f) < {Eps} AdX(1.0,-12) then Result := f
  else
    begin
      g := f * f;
      gPg := (p1 * g + p0) * g;
      Qg := g + q0;
      Rg := gPg / Qg;
      Result := f + f * Rg
    end;
  if N > 1 then Result := -Result;
  case N of
    0: {Result := a0 + Result};
    1: Result := a1 + Result;
    2: Result := a2 + Result;
    3: Result := a3 + Result
    end;
  ArcTanArcTan2 := Result
end { ArcTanArcTan2 };

function ArcTan( X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the arctangent of a number.
{       
{       Domain = [RealMLargest, RealPLargest].
{       Range  = (-Pi/2, Pi/2).
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Arctangent of X.
{
{ Design:
{       Seems fine except for very large numbers.
{
{-----------------------------------------------------------------------------}

begin { ArcTan }
  if X < 0 then ArcTan := -ArcTanArcTan2(X)
  else ArcTan := ArcTanArcTan2(X)
end { ArcTan };

function ArcTan2( Y, X: Real ): Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the arctangent of the quotient of two numbers.  One
{       interpretation is that the parameters represent the cartesian
{       coordinate (X,Y) and ArcTan2(Y,X) is the angle formed by
{       (X,Y), (0,0), and (1,0).
{
{       DomainY = [RealMLargest, RealPLargest].
{       DomainX = [RealMLargest, RealPLargest].
{       Range =   [-Pi, Pi].
{
{ Parameters:
{       Y - Input value.
{       X - Input value.
{
{ Returns:
{       Arctangent of Y / X.
{
{ Design:
{       Seems fine except for very large Y/X.
{
{-----------------------------------------------------------------------------}

const Pi = 3.1415926536;
      PiD2 = 1.5707963267;   { Pi / 2 }
var Result: Real;
    ExpDif: Integer;
begin { ArcTan2 }
  if X = 0 then
    if Y = 0 then
      begin
        raise ArcTan2Zero(Y,X);
        ArcTan2 := RealPInfinity;
        Exit(ArcTan2)
      end
    else Result := PiD2
  else
    begin
      ExpDif := IntXp(Y) - IntXp(X);
      if ExpDif >= 126 { is this the right number? } then { Y/X overflow}
        Result := PiD2
      else
        if ExpDif < -123 { is this the right value? } then { Y/X underflow }
          Result := 0
        else Result := ArcTanArcTan2(Y/X)
    end;
  if X < 0 then Result := Pi - Result;
  if Y < 0 then Result := -Result;
  ArcTan2 := Result
end { ArcTan2 };

function SinH( x:real ) : real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the Hyperbolic Sine of a number.
{
{       Domain = [-87.33,87.33].
{       Range = [RealMLargest, RealPLargest].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Hyperbolic Sine of X.
{
{-----------------------------------------------------------------------------}

const
   p0 = -0.713793159e+1;
   p1 = -0.190333399e+0;
   q0 = -0.428277109e+2;
   ybar = 87.33; { is largest arg to exp -- make bigger when grow exp - jbb }
   wmax = 87.33; { this may not be right either -- jbb }
   lnv = Recast(#07714271400,real);
   vd2m1 = 0.13830277879601902638e-4;
   eps = recast(#07140000000,real); { is this rite? approx. b**(-t/2) -- jbb }
                                    { b is 2.0 t/2 is 12 -- 2**-12    -- jbb }

var
   y,w,z,result,f,rf,pf,qf : real;

begin { SinH }
   y:=Abs(x);
   if y > 1.0 then
      begin
         if y > ybar then
            begin
               w:=y-lnV;
               if w > WMax then
                  begin
                     raise SinHLarge(x);
                     if x < 0.0 then
                        SinH:=RealMLargest
                     else
                        SinH:=RealPLargest;
                     exit(SinH);
                  end
               else
                  begin
                     z:=exp(w);
                     result:=z+Vd2m1*z;
                  end
            end
         else
            begin
               z:=exp(y);
               result:=(z-1/z)/2;
            end;
         if x < 0.0 then
            SinH:=-result
         else
            SinH:=result;
      end
   else
      if y < eps then
         SinH:=x
      else
         begin
            f:=x*x;
            pf:=p1*f+p0;
            qf:=f+q0;
            rf:=f*(pf/qf);
            SinH:=x+x*rf;
         end;
end; { SinH }

function CosH( x:real ) : real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the Hyperbolic Cosine of a number.
{
{       Domain = [-87.33,87.33].
{       Range = [1.0, RealPLargest].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Hyperbolic Cosine of X.
{
{-----------------------------------------------------------------------------}

const
   ybar = 87.33; { is largest arg to exp -- make bigger when grow exp - jbb }
   wmax = 87.33; { this may not be right either -- jbb }
   lnv = Recast(#07714271400,real);
   vd2m1 = 0.13830277879601902638e-4;
   
var
   y,w,z,result : real;

begin { CosH }
   y:=Abs(x);
   if y > ybar then
      begin
         w:=y-lnV;
         if w > WMax then
            begin
               raise CosHLarge(x);
               CosH:=RealPLargest;
               exit(CosH);
            end
         else
            begin
               z:=exp(w);
               result:=z+Vd2m1*z;
            end
      end
   else
      begin
         z:=exp(y);
         result:=(z+1/z)/2;
      end;
   CosH:=result;
end; { CosH }

function TanH( x:real ) : real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the Hyperbolic Tangent of a number.
{
{       Domain = [-8.66433975625,8.66433975625].
{       Range = [-1.0, 1.0].
{
{ Parameters:
{       X - Input value.
{
{ Returns:
{       Hyperbolic Tangent of X.
{
{-----------------------------------------------------------------------------}

const
   xbig  =  8.66433975625; { (ln(2) + (23 + 1)ln(2)) / 2 - rite? - jbb }
   ln3d2 =  0.54930614433405484570;
   p0    = -0.8237728127e+0;
   p1    = -0.3831010665e-2;
   q0    =  0.2471319654e+1;
   eps = recast(#07140000000,real); { is this rite? approx. b**(-t/2) -- jbb }
                                    { b is 2.0 t/2 is 12 -- 2**-12    -- jbb }

var
   f,result,g,pg,qg,rg : real;
   
begin { TanH }
   f:=Abs(x);
   if f > xbig then
      result:=1.0
   else
      if f > ln3d2 then
         begin
            result:=0.5-1.0/(exp(f+f)+1.0);
            result:=result+result;
         end
      else
         if f < eps then
            result:=f
         else
            begin
               g:=f*f;
               pg:=p1*g+p0;
               qg:=g+q0;
               rg:=g*(pg/qg);
               result:=f+f*rg;
            end;
   if x < 0.0 then
      result:=-result;
   TanH:=result;
end. { TanH }

