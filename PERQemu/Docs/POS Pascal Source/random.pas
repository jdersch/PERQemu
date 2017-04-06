{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module RandomNumbers;
{------------------------------------------------------------------------
     RandomNumbers - Random number generator.
      J. P. Strait    15 Sep 80.
      Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1982, 1983.
      
      
      Module RandomNumbers contains two routines:
      
        InitRandom - initializes the random number generator.
        
        Random - a function which returns a new random number each time
                 it is referenced.
                 
      There is currently no way to seed the generator.
      
      Random is a feedback shift-register pseudo-random number generator.
      The algorithm used is one described in the article:
      
           'Generalized Feedback Shift Register Pseudorandom Number
                 Generator'
           T. G. Lewis and W. H. Payne
           JACM Vol. 20, No. 3, July 1973, pp. 456-468.
           
      Random produces multidimensional pseudo-random numbers equally
      distributed in the interval -32768..32767 and has a period of
      2^98.
------------------------------------------------------------------------}

{------------------------------------------------------------------------
  Change log:
      
       1 Jun 81  BAM  V1.2  Add comments.
      17 Sep 80  JPS  V1.1  Correct an error in the table.
------------------------------------------------------------------------}

{////////////////////////////} exports {\\\\\\\\\\\\\\\\\\\\\\\\\}

procedure InitRandom;
function Random: integer;
 
{////////////////////////////} private {\\\\\\\\\\\\\\\\\\\\\\\\\}

const RandomVersion = '1.2';
      P = 98;         { table size }
      Q = 27;         { circular pointer displacement }
      Pm1 = P - 1;
            
var Table: array[0..P-1] of integer;
    I, J: integer;



Procedure InitRandom;
{------------------------------------------------------------------------
  Abstract: Initialize the random number generator.  Every time this is
             called, the random numbers start over at the same place.
------------------------------------------------------------------------}
 begin { InitRandom }
  I := Pm1;
  J := (I + Q) mod P;
  Table[ 0] := #020651;
  Table[ 1] := #147643;
  Table[ 2] := #164707;
  Table[ 3] := #125262;
  Table[ 4] := #104256;
  Table[ 5] := #074760;
  Table[ 6] := #114470;
  Table[ 7] := #052607;
  Table[ 8] := #045551;
  Table[ 9] := #134031;
  Table[10] := #024107;
  Table[11] := #030766;
  Table[12] := #154073;
  Table[13] := #114777;
  Table[14] := #024540;
  Table[15] := #111012;
  Table[16] := #011042;
  Table[17] := #104067;
  Table[18] := #056332;
  Table[19] := #142244;
  Table[20] := #131107;
  Table[21] := #034074;
  Table[22] := #052641;
  Table[23] := #163046;
  Table[24] := #026303;
  Table[25] := #131352;
  Table[26] := #077724;
  Table[27] := #002462;
  Table[28] := #110775;
  Table[29] := #127346;
  Table[30] := #020100;
  Table[31] := #137011;
  Table[32] := #136163;
  Table[33] := #145552;
  Table[34] := #144223;
  Table[35] := #134111;
  Table[36] := #075001;
  Table[37] := #075221;
  Table[38] := #176705;
  Table[39] := #000210;
  Table[40] := #103625;
  Table[41] := #120246;
  Table[42] := #062614;
  Table[43] := #016147;
  Table[44] := #054723;
  Table[45] := #151200;
  Table[46] := #105223;
  Table[47] := #021001;
  Table[48] := #016224;
  Table[49] := #073377;
  Table[50] := #150716;
  Table[51] := #014557;
  Table[52] := #112613;
  Table[53] := #037466;
  Table[54] := #002677;
  Table[55] := #052542;
  Table[56] := #063572;
  Table[57] := #105462;
  Table[58] := #106436;
  Table[59] := #063302;
  Table[60] := #053171;
  Table[61] := #133243;
  Table[62] := #113130;
  Table[63] := #123222;
  Table[64] := #072371;
  Table[65] := #041043;
  Table[66] := #163614;
  Table[67] := #037432;
  Table[68] := #147330;
  Table[69] := #153403;
  Table[70] := #130306;
  Table[71] := #056455;
  Table[72] := #175640;
  Table[73] := #120567;
  Table[74] := #100601;
  Table[75] := #042371;
  Table[76] := #154635;
  Table[77] := #051133;
  Table[78] := #074252;
  Table[79] := #174525;
  Table[80] := #163223;
  Table[81] := #052022;
  Table[82] := #022564;
  Table[83] := #135512;
  Table[84] := #021760;
  Table[85] := #006743;
  Table[86] := #006451;
  Table[87] := #067445;
  Table[88] := #106210;
  Table[89] := #025417;
  Table[90] := #066566;
  Table[91] := #062723;
  Table[92] := #124224;
  Table[93] := #144643;
  Table[94] := #164502;
  Table[95] := #025342;
  Table[96] := #003521;
  Table[97] := #024050
 end { InitRandom };


Function Random;
{------------------------------------------------------------------------
  Abstract: Returns a random number.
  Returns: A random 16-bit number.
------------------------------------------------------------------------}
 begin { Random }
  if I = Pm1 then I := 0 else I := I + 1;
  if J = Pm1 then J := 0 else J := J + 1;
  Table[I] := LXor(Table[I],Table[J]);
  Random := Table[I]
 end { Random }.
