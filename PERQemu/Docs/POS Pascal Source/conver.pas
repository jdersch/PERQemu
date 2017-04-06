{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Convert;
{-----------------------------------------------------------------------------
{
{ Convert - Conversion functions for reals and longs.
{ Michael R. Kristofic   25 Feb 82.
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       Functions for converting between floating point and double
{       precision integers are provided.
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{
{ Change history:
{
{ 25 Feb 82  V1.0  Michael R. Kristofic
{ Start module.
{
{-----------------------------------------------------------------------------}

exports


function FloatLong(Arg : Long) : Real;
function TruncLong(Arg : Real) : Long;
function RoundLong(Arg : Real) : Long;


exception R2LOvrFlow(Arg : Real);
{-----------------------------------------------------------------------------
{
{ Abstract:
{       R2LOvrFlow is raised when RoundLong or TruncLong is called with Arg
{       exceeding the range for Longs (-2147483648 .. +2147483647).
{       You may resume from this exception in which
{       case RoundLong or TruncLong returns -2147483648 or +2147483647.
{
{ Parameters:
{       Arg - Argument of RoundLong or TruncLong.
{
{-----------------------------------------------------------------------------}




private


type Sign = (Positive, Negative);
     RealRep = packed record case integer of
                 1: (R:      Real);
                 2: (L:      Long);
                 3: (FLower: Integer;
                     FUpper: 0..127;
                     E:      0..255;
                     S:      Sign);
                 4: (LLower: Integer;
                     LUpper: Integer)
                 end;


function FloatLong(Arg : Long) : Real;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Convert a double precision integer to floating point.
{
{       Domain = [-2147483648, +2147483647].
{       Range =   [-2147483648.0, +2147483648.0].
{
{ Parameters:
{       Arg - Input value.
{
{ Returns:
{       Floating point equivalent of Arg.
{
{ Design:
{       Zero and -2147483648 are special cased.  Other numbers are handled
{       by an algorithm that shifts Arg until it falls into the floating
{       point mantissa pattern then sets the exponent based on the number and
{       direction of shifts. Floating point negative numbers are not 2's
{       complement, so if Arg is negative its 2's complement is converted
{       and the result is negated. NOTE: Floating point representation can only
{       handle 24 of the possible 31 bits of mantissa information, i.e.
{       accuracy is lost for large numbers.  Rounding occurs in these cases.
{
{-----------------------------------------------------------------------------}

  const
      BiasPlusFractionLess1 = 149; { IEEE Floating point exponent bias (127)
                                     plus number of fraction bits (23) less 1 }
      MaxNegFP = #31700000000;     { Max negative long in floating point 
                                     notation }
      MaxNegLong = #20000000000;   { Max negative long, long notation }

      Twenty4thBitOn = #40000000; { Twentyfour bits in a real mantissa }      

      Twenty5thBitOn = #100000000; { One more bit than will fit in mantissa }      

  var 
      Convert : RealRep;  { Used to build the result, piece by piece }
      Neg : Boolean;      { True iff Arg passed negative }
      Shifts : Integer;   { # of places Arg is shifted (- = leftshift) }
      CarryBit : 0..1;    { Value of the lost bit (used for rounding) }
      SaveArg : Long;     { Used to calculate CarryBit (by comparison) }
      
      
  begin
  if Arg <> 0 then   { 0 is a special case }
    begin
      { ***** 1st handle negatives ***** }
      if Arg < 0 then begin
                        if Arg = MaxNegLong then   { Max negative long! }
                                        begin
                                          Convert.L := MaxNegFP;
                                          FloatLong := Convert.R;
                                          Exit(FloatLong);
                                        end;
                        Arg := -Arg;        { Convert only positive numbers }
                        Neg := true
                      end
                 else Neg := false;
                 
      { ***** Shift if necessary, creating mantissa ***** }
      Convert.L := 0;
      Shifts := 0;
      if Arg < Twenty4thBitOn then                { LEFTshift }
          while Arg < Twenty4thBitOn do begin
                                     Arg := Arg * 2;
                                     Shifts := Shifts - 1;
                                   end
      else     { RIGHTshift }
        begin
          CarryBit := 0;
          while Arg >= Twenty5thBitOn do 
                                     begin
                                       SaveArg := Arg;
                                       Arg := Arg div 2;
                                       CarryBit := SHRINK(SaveArg - (Arg*2));
                                       Shifts := Shifts + 1;
                                     end;
          Arg := Arg + CarryBit;
          if Arg = Twenty5thBitOn then  { Rounding overflowed the mantissa! }
                                    begin
                                      Arg := Twenty4thBitOn;
                                      Shifts := Shifts + 1;
                                    end;
        end;

      { ***** Lastly, construct remainder of the floating point ***** }
      Convert.E := BiasPlusFractionLess1 + Shifts;
      Convert.L := Convert.L + Arg;        { This addition causes the exponent
                                             to be bumped by 1 to its correct
                                             value }
      if Neg then FloatLong := -Convert.R
             else FloatLong := Convert.R;
    end { Arg <> 0 }
    
  else FloatLong := 0;
  end;   { FloatLong }

function TruncRound(Arg : Real; VAR RoundBit : integer) : Long;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the double precision integer equivalent of a floating point
{       number. The fraction part is truncated, and the value of the last
{       truncated bit is returned.
{
{       Domain =  [-2147483648.0, +2147483520.0].
{       Range =   [-2147483648, +2147483520].
{
{ Parameters:
{       Arg - Input value.
{       RoundBit - On return, value of the most significant bit truncated.
{
{ Returns:
{       Double precision integer equivalent of Arg, fraction truncated, and
{       the value of the last truncated bit.
{
{ Design:
{       Raises R2LOvrFlow for floating point numbers outside the range above.
{
{-----------------------------------------------------------------------------}
  const
      BiasPlusFractionBits = 150;  { IEEE Floating point exponent bias (127)
                                     plus number of fraction bits (23) }
                                     
      MaxNegLong = #20000000000;   { Max negative long }

      Twenty4thBitOn =  #40000000; { Twentyfour bits in a real mantissa } 

      MaxShift = 7;                { Max number of places a floating point
                                     mantissa (24 bits) can be shifted before
                                     it exceeds long (31 bits) range }

      MinShift = -24;              { Min number of places a floating point
                                     mantissa (24 bits) can be shifted before
                                     its gauranteed to be 0 }


  var 
      Convert : RealRep; { Used to access the argument, piece by piece, and to
                           build the result }
      Exp : Integer;     { # of places mantissa needs shifted (+ = leftshift) }
      Sighn : Sign;      { Sign of the argument }
      I : Integer;       { FOR loop index }
      
  begin
  
  Convert.R := Arg;
  Exp := Convert.E - BiasPlusFractionBits;
  Sighn := Convert.S;
  
  Convert.S := Positive; { Create ABS(mantissa), ignoring binary point }
  Convert.E := 1;        { and inserting the hidden bit }
  
  if Exp >= 0 then { Shift to left Exp places }
    begin
      RoundBit := 0;
      { If a shift of more than MaxShift is required and the number isn't
        the largest negative long, Arg is too big }
      if (Exp > MaxShift) and 
         ( (Exp <> (MaxShift + 1))  or 
           (Convert.L <> Twenty4thBitOn) or 
           (Sighn = Positive) ) then begin
                                       raise R2LOvrFlow(Arg);
                                       if Sighn = Positive 
                                           then Convert.L := MaxLongInt
                                           else Convert.L := MaxNegLong
                                     end
      else
          for I := 1 to Exp do Convert.L := Convert.L * 2;
      if Sighn = Positive then TruncRound := Convert.L 
                          else TruncRound := -Convert.L;
    end { leftShift }
    
  else
    
    begin { Shift to right -Exp places }
      if Exp < MinShift then begin        { Too small -- 0 }
                               RoundBit := 0;
                               TruncRound := 0;
                             end
      else
          begin
            for I := -1 downto Exp do
                begin
                  if odd(Convert.LLower) then RoundBit := 1
                                         else RoundBit := 0;
                  Convert.L := Convert.L div 2;
                end;
            if Sighn = Positive then TruncRound := Convert.L
                                else begin
                                       TruncRound := -Convert.L;
                                       RoundBit := -RoundBit;
                                     end
          end
    end; { rightShift }
  end; { TruncRound }
  

function TruncLong(Arg : Real) : Long;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the double precision integer equivalent of a floating point
{       number. The fraction part is truncated.
{
{       Domain =  [-2147483648.0, +2147483520.0].
{       Range =   [-2147483648, +2147483520].
{
{ Parameters:
{       Arg - Input value.
{
{ Returns:
{       Double precision integer equivalent of Arg, fraction truncated.
{
{-----------------------------------------------------------------------------}
  var
      RoundBit : integer;
  begin
    TruncLong := TruncRound(Arg,RoundBit);
  end; { TruncLong }
  

function RoundLong(Arg : Real) : Long;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Compute the double precision integer equivalent of a floating point
{       number. The fraction part is rounded.
{
{       Domain =  [-2147483648.0, +2147483520.0].
{       Range =   [-2147483648, +2147483520].
{
{ Parameters:
{       Arg - Input value.
{
{ Returns:
{       Double precision integer equivalent of Arg, fraction rounded.
{
{-----------------------------------------------------------------------------}
  var
      RoundBit : integer;
      Temp : long;
  begin
    Temp := TruncRound(Arg,RoundBit);
    RoundLong := Temp + RoundBit;     { Note : overflow cannot be introduced by
                                        adding a round bit cause floating
                                        point mantissas are only 24 bits }
  end. { RoundLong }
