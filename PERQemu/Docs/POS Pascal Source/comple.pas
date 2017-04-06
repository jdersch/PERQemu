{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module ComplexFunctions;
{-----------------------------------------------------
{ ComplexFunctions -- Standard functions for Complex numbers
{ J. B. Brodie     1 Mar 82
{ Copyright (C) Three Rivers Computer Corporation 1982, 1983.
{
{ Abstract:
{       ComplexFunctions implements many of the standard functions
{       whose domain and/or range is within the Complex number
{       system.  The implementation of these functions utilize
{       mathematical identies for the relationships between
{       the real number system and the complex number system.
{
{ DISCLAIMER:
{       Since Math identities are utilized to evaluate these
{       functions,  accuracy and execution speed may be very
{       poor.  Only the most cursory testing of these functions
{       has been performed.  No guarantees are made as to the
{       accuracy or correctness of the functions.  Validation
{       of the functions must be done, but at some later date.
{
{----------------------------------------------------}

{-----------------------------------------------------
{ Change History:
{
{ 5 May 82   V1.1   Michael R. Kristofic
{ Fix declaration of complex record so that real part comes first
{
{ 1 Mar 82   V1.0   JBBrodie
{ Original
{
{----------------------------------------------------}

exports

type
   Complex = record
      Re : Real;
      Im : Real;
   end;
     
function CMult   ( Z1,Z2:Complex ) :     Complex;
function CExp    ( Z:Complex ) :         Complex;
function CCos    ( Z:Complex ) :         Complex;
function CSin    ( Z:Complex ) :         Complex;
function CLn     ( Z:Complex ) :         Complex;
function CSqrt   ( Z:Complex ) :         Complex;
function CPowerC ( Z1,Z2:Complex ) :     Complex;
function CPowerR ( Z:Complex; X:Real ) : Complex;

exception CExpReLarge ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CExpReLarge is raised when CExp is called with a complex number
{    whose real number component (e.g. Z.Re) would cause a result
{    which is too large to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CExp
{
{-----------------------------------------------------------------------------}




exception CExpReSmall ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CExpReSmall is raised when CExp is called with a complex number
{    whose real number component (e.g. Z.Re) would cause a result
{    which is too small to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CExp
{
{-----------------------------------------------------------------------------}




exception CExpImLarge ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CExpImLarge is raised when CExp is called with a complex number
{    whose imaginary number component (e.g. Z.Im) would cause a result
{    which is too large to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CExp
{
{-----------------------------------------------------------------------------}




exception CExpImSmall ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CExpImSmall is raised when CExp is called with a complex number
{    whose imaginary number component (e.g. Z.Im) would cause a result
{    which is too small to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CExp
{
{-----------------------------------------------------------------------------}




exception CCosReLarge ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CCosReLarge is raised when CCos is called with a complex number
{    whose real number component (e.g. Z.Re) would cause a result
{    which is too large to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CCos
{
{-----------------------------------------------------------------------------}




exception CCosImLarge ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CCosImLarge is raised when CCos is called with a complex number
{    whose imaginary number component (e.g. Z.Im) would cause a result
{    which is too large to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CCos
{
{-----------------------------------------------------------------------------}




exception CSinReLarge ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CSinReLarge is raised when CSin is called with a complex number
{    whose real number component (e.g. Z.Re) would cause a result
{    which is too large to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CSin
{
{-----------------------------------------------------------------------------}




exception CSinImLarge ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CSinImLarge is raised when CSin is called with a complex number
{    whose imaginary number component (e.g. Z.Im) would cause a result
{    which is too large to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CSin
{
{-----------------------------------------------------------------------------}




exception CLnSmall    ( Z:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CLnSmall is raised when CLn is called with a complex number which
{    would cause a result which is too small to be represented on the Perq.
{
{ Parameters:
{    Z -- Argument of CLn
{
{-----------------------------------------------------------------------------}




exception CPowerZero  ( Z1,Z2:Complex );
{-----------------------------------------------------------------------------
{
{ Abstract:
{    CPowerZero is raised when either CPowerC or CPowerR is called
{    with a zero exponent.
{
{ Parameters:
{    Z1, Z2 -- Arguments to CPowerC or CPowerR
{
{-----------------------------------------------------------------------------}





private

imports RealFunctions from RealFunctions;

const
   Sqrt2 = 1.4142135;
   InvSqrt2 = 0.7071068;


function CMult ( Z1,Z2:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Evaluates the vector cross product of two complex numbers
{
{ Parameters:
{    Z1, Z2 -- Complex numbers to be multiplied
{
{ Returns:
{    Complex cross product
{
{-----------------------------------------------------------------------------}

var
   ztemp : Complex;

begin
   ztemp.re:=Z1.re*Z2.re-Z1.im*Z2.im;
   ztemp.im:=Z1.re*Z2.im+Z1.im*Z2.re;
   CMult:=ztemp;
end; { CMult }

function CExp ( Z:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Compute exponential function of a complex number
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=[-85.0,87.0]   Imaginary=[-1E5,1E5]
{    Range:  Real=Imaginary=[RealMLargest,RealPLargest]
{
{ Parameters:
{    Z -- Input argument
{
{ Returns:
{    Exponential of Z
{
{-----------------------------------------------------------------------------}


var
   result : Complex;
   xtemp  : Real;
   
handler ExpLarge ( X:Real );
begin
   raise CExpReLarge(Z);
   result.re:=RealPLargest;
   result.im:=RealPLargest;
   CExp:=result;
   exit(CExp);
end;

handler ExpSmall ( X:Real );
begin
   raise CExpReSmall(Z);
   result.re:=0.0;
   result.im:=0.0;
   CExp:=result;
   exit(CExp);
end;

handler CosLarge ( X:Real );
begin
   raise CExpImLarge(Z);
   result.re:=Exp(Z.re)*InvSqrt2;
   result.im:=result.re;
   CExp:=result;
   exit(CExp);
end;

handler SinLarge ( X:Real );
begin
   raise CExpImLarge(Z);
   result.re:=Exp(Z.re)*InvSqrt2;
   result.im:=result.re;
   CExp:=result;
   exit(CExp);
end;

begin { CExp }
   xtemp:=Exp(Z.re);
   result.re:=xtemp*Cos(Z.im);
   result.im:=xtemp*Sin(Z.im);
   CExp:=result;
end; { CExp }

function CCos ( Z:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Compute the complex Cosine function.
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=[-1E5,1E5]   Imaginary=[-85.0,87.0]
{    Range:  Real=Imaginary=[RealMLargest,RealPLargest]
{
{ Parameters:
{    Z -- Input argument
{
{ Returns:
{    Cosine of Z
{
{-----------------------------------------------------------------------------}


var
   result : Complex;
   
handler CosLarge ( X:Real );
begin
   raise CCosReLarge(Z);
   result.re:=CosH(Z.im)*Sqrt2;
   result.im:=-(SinH(Z.im)*Sqrt2);
   CCos:=result;
   exit(CCos);
end;
   
handler SinLarge ( X:Real );
begin
   raise CCosReLarge(Z);
   result.re:=CosH(Z.im)*Sqrt2;
   result.im:=-(SinH(Z.im)*Sqrt2);
   CCos:=result;
   exit(CCos);
end;

handler CosHLarge ( X:Real );
begin
   raise CCosImLarge(Z);
   result.re:=RealPLargest;
   if X < 0.0 then
      result.im:=RealMLargest
   else
      result.im:=RealPLargest;
   CCos:=result;
   exit(CCos);
end;

handler SinHLarge ( X:Real );
begin
   raise CCosImLarge(Z);
   result.re:=RealPLargest;
   if X < 0.0 then
      result.im:=RealMLargest
   else
      result.im:=RealPLargest;
   CCos:=result;
   exit(CCos);
end;

begin { CCos }
   result.re:=Cos(Z.re)*CosH(Z.im);
   result.im:=Sin(Z.re)*SinH(Z.im);
   CCos:=result;
end; { CCos }

function CSin ( Z:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Compute the complex Sine function.
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=[-1E5,1E5]   Imaginary=[-85.0,87.0]
{    Range:  Real=Imaginary=[RealMLargest,RealPLargest]
{
{ Parameters:
{    Z -- Input argument
{
{ Returns:
{    Sine of Z
{
{-----------------------------------------------------------------------------}


var
   result : Complex;
   
handler CosLarge ( X:Real );
begin
   raise CSinReLarge(Z);
   result.re:=CosH(Z.im)*InvSqrt2;
   result.im:=SinH(Z.im)*InvSqrt2;
   CSin:=result;
   exit(CSin);
end;
   
handler SinLarge ( X:Real );
begin
   raise CSinReLarge(Z);
   result.re:=CosH(Z.im)*InvSqrt2;
   result.im:=SinH(Z.im)*InvSqrt2;
   CSin:=result;
   exit(CSin);
end;

handler CosHLarge ( X:Real );
begin
   raise CSinImLarge(Z);
   result.re:=RealPLargest;
   if X < 0.0 then
      result.im:=RealMLargest
   else
      result.im:=RealPLargest;
   CSin:=result;
   exit(CSin);
end;

handler SinHLarge ( X:Real );
begin
   raise CSinImLarge(Z);
   result.re:=RealPLargest;
   if X < 0.0 then
      result.im:=RealMLargest
   else
      result.im:=RealPLargest;
   CSin:=result;
   exit(CSin);
end;

begin { CSin }
   result.re:=Sin(Z.re)*CosH(Z.im);
   result.im:=Cos(Z.re)*SinH(Z.im);
   CSin:=result;
end; { CSin }

function CMagn ( Z:Complex ) : Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       CMagn is a local function used to calculate the magnitude of a complex
{       number.   Currently it is just brute force.  Should be made more better
{       in the future.
{
{ Parameters:
{       Z - Complex number whose magnitude is desired.
{
{ Returns:
{       a real number representing the magnitude of Z (e.g. |Z|).
{
{-----------------------------------------------------------------------------}


begin { CMagn }
   CMagn:=Sqrt(Z.re*Z.re+Z.im*Z.im);
end; { CMagn }

function CLn ( Z:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Compute natural logarithm of a complex number
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=Imaginary=[-1E19,1E19]
{    Range:  Real=[RealMLargest,RealPLargest]   Imaginary=[-Pi,Pi]
{
{ Parameters:
{    Z -- Input argument
{
{ Returns:
{    Natural log of Z
{
{-----------------------------------------------------------------------------}


var
   result : Complex;

begin { CLn }
   if (Z.re = 0.0) and (Z.im = 0.0) then
      begin
         raise CLnSmall(Z);
         result.re:=RealMLargest;
         result.im:=0.0;
         CLn:=result;
         exit(CLn);
      end
   else
      begin
         result.re:=Ln(CMagn(Z));
         result.im:=ArcTan2(Z.im,Z.re);
         CLn:=result;
      end;
end; { CLn }

function CSqrt ( Z:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Compute square root of a complex number
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=Imaginary=[-1E19,1E19]
{    Range:  Real=Imaginary=[-1E19,1E19]
{
{ Parameters:
{    Z -- Input argument
{
{ Returns:
{    Square root of Z
{
{-----------------------------------------------------------------------------}

var
   xtemp : Real;
   result: Complex;

begin { CSqrt }
   xtemp:=Sqrt(0.5*(Abs(Z.re)+CMagn(Z)));
   if xtemp = 0.0 then
      begin
         result.re:=0.0;
         result.im:=0.0;
      end
   else
      if Z.re >= 0.0 then
         begin
            result.re:=xtemp;
            result.im:=Z.im/(2.0*xtemp);
         end
      else
         begin
            result.re:=Z.im/(2.0*xtemp);
            if Z.im < 0.0 then
               result.im:=-xtemp
            else
               result.im:=xtemp;
         end;
   CSqrt:=result;
end; { CSqrt }

function CPowerC ( Z1,Z2:Complex ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Raise an arbitrary complex number to an arbitrary complex power.
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=Imaginary=[-1E19,1E19]
{    Range:  Real=[RealMLargest,RealPLargest]   Imaginary=[-Pi,Pi]
{
{ Parameters:
{    Z1 -- Input complex base
{    Z2 -- Input complex exponent
{
{ Returns:
{    Z1 raised to the Z2 power
{
{-----------------------------------------------------------------------------}


begin { CPowerC }
   
   if (Z1.re=0.0) and (Z1.im=0.0) then
      if Z2.re <= 0.0 then
         begin
            raise CPowerZero(Z1,Z2);
            CPowerC.re:=0.0;
            CPowerC.im:=0.0;
            exit(CPowerC);
         end
      else
         begin
            if Z2.im <> 0.0 then raise CPowerZero(Z1,Z2);
            CPowerC.re:=0.0;
            CPowerC.im:=0.0;
            exit(CPowerC);
         end;
   
   CPowerC:=CExp(CMult(Z2,CLn(Z1)));
end; { CPowerC }

function CPowerR ( Z:Complex; X:Real ) : Complex;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    Raise an arbitrary complex number to an arbitrary real power.
{
{    Note: the use of standard type-real functions in order to evaluate
{          this function may artificially constrain this functions Domain.
{
{    Domain: Real=Imaginary=[-1E19,1E19]
{    Range:  Real=[RealMLargest,RealPLargest]   Imaginary=[-Pi,Pi]
{
{ Parameters:
{    Z -- Input complex base
{    X -- Input real exponent
{
{ Returns:
{    Z1 raised to the X power
{
{-----------------------------------------------------------------------------}


var
   ztemp : Complex;

begin { CPowerR }
   ztemp.re:=X;
   ztemp.im:=0.0;
   CPowerR:=CPowerC(Z,ztemp);
end. { CPowerR }

