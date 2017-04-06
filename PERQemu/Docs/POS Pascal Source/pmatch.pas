{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module PMatch;

{--------------------------------------------------------------
{ Copyright 1981, 1982, 1983, Three Rivers Computer Corporation
{
{ Abstract: 
{    Does pattern matching on strings
{
{            Patterns accepted are as follows:
{
{
{              "*" matches 0 or more characters.
{
{              "&" matches 1 or more characters.
{
{              "#" matches exactly 1 character.
{
{              "'0" matches any digit.
{
{              "'A" matches any alphabetic (capitals only unless casefold).
{
{              "'a" matches any alphabetic(lower case only unless 'casefold').
{
{              "'@" matches any non-alphanumeric.
{
{              "'*" matches '*', other patterns chars can be quoted also.
{             
{ Written by: Gene Ball at CMU
{--------------------------------------------------------------}

{--------------------------------------------------------------
 Change log:
     16-Nov-82 Bill Braucher V2.6 Fixed names for 14-character compiler.
     28-Dec-81 Brad Myers  V2.5  Removed IsPattern removal of ' if not pattern.
      1-Dec-81 Brad Myers  V2.4  Changed length of files compared to 255
      1-Dec-81 Brad Myers  V2.3  Fixed so IsPattern removes ' if not pattern
     13-Aug-81 Brad Myers  V2.2  Fixed exit in IsPattern so didn't exit from
                                  PattMatch so no crash if string ends in "'"
     15-May-81 Brad Myers  V2.1  Added new exception and changed module name
                                 Use the PERQ_String function uppercase
                                 Added comments in 3RCC style
     ??-Apr-81 Gene Ball   V2.0  Fixed bugs in PattMatch and added new
                                  procedures PattMap and IsPattern
     13-Mar-81 Brad Myers  V1.1  Fixed a small bug and changed import from
                                    PERQ.String to PERQ_String
     ??-Feb-81 Gene Ball   V1.0  Started
--------------------------------------------------------------}

{/////////////////////////////} Exports {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

Type pms255 = String[255];

Function PattMatch(var str,pattern: pms255; fold: boolean): boolean;
Function PattMap(var str,inpatt,outpatt,outstr:pms255; fold:boolean): boolean;
Procedure PattDebug(v: boolean);
Function IsPattern(var str: pms255): boolean;

Exception BadPatterns;
{---------------------------------------------------------------
 Abstract: Raised if outPatt and inPatt do not have the same patterns
            in the same order for PattMap
---------------------------------------------------------------}
 
{/////////////////////////////} Private {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

imports PERQ_String from PERQ_String;

const
  QUOTE = '''';
  
var
  casefold, debug : boolean;
  init: integer;
  myout: pms255;
  


Procedure PattDebug(v: boolean);
{---------------------------------------------------------------
 Abstract: Sets the global debug flag
 Parameters: v is value to set debug to
 SideEffects: Changes debug value
---------------------------------------------------------------}
  begin
    debug := v;
    init := #5652;
  end;



Function NextCh(var s: pms255; var i: integer; var sp: boolean): char;
{---------------------------------------------------------------
 Abstract: returns the next character from s starting at i; decodes special
                  characters, quoted characters and case folds if necessary
 Parameters: s - string to look at; i is index of where to start looking;
             sp - set to true if got a pattern matching character, else
                 false
 Returns: the character found
---------------------------------------------------------------}
  var
    c: char;
  begin
    if i > length(s) then
    begin
      NextCh := ' ';
      sp := true;
      exit(NextCh);
    end;
    
    c := s[i];
    if c = QUOTE then
    begin
      i := i+1;
      if i > length(s) then
      begin
        NextCh := ' ';
        sp := false;
        exit(NextCh);  { string ends with ' }
      end;
      c := s[i];
      if casefold and (c = 'a') then c := 'A';
      sp := (c='0') or (c='a') or (c='A') or (c='@');
    end
    else
      sp := (c = '*') or (c = '&') or (c = '#');
    
    if (not sp) and casefold and
       (ord(c) >= ord('a')) and (ord(c) <= ord('z')) then 
      c := chr( ord(c) - ord(' '));
    NextCh := c;
  end;


Function UpCh(var s: pms255; i: integer): char;
{---------------------------------------------------------------
 Abstract: Returns uppercase of s[i]
 Parameters: s - string to look at; i is index of char wanted;
 Returns: if i > length(s) then space; else if caseFold then UpperCase of the
           character; else the character itself
---------------------------------------------------------------}
  var
    c: char;
  begin
    if i > length(s) then 
      c := ' '
    else
      c := s[i];
    if casefold then c := UpperCase(c);
    
    UpCh := c;
  end;


Function IsPattern(var str: pms255): boolean;
{---------------------------------------------------------------
 Abstract: Tests to see whether str contains any pattern matching characters
 Parameters: str - string to test.  If not pattern then removes all quotes.
 Returns: true if str contains any pattern matching characters; else false
---------------------------------------------------------------}
  var
    i: integer;
    sp: boolean;
    ch: char;
  begin
    i := 1;
    while i <= length(str) do
    begin
      ch := NextCh(str, i, sp);
      if sp then
      begin
        IsPattern := true;
        exit(IsPattern);
      end;
      i := i+1;
    end;
    IsPattern := false;
  end;
  

Function ChMatch(pch,sch: char; special:boolean): boolean;
{---------------------------------------------------------------
 Abstract: Test is sch matches the pattern character pch
 Parameters: pch - pattern char.
             sch - char to match against pch.
             special - determines whether need to do pattern match (if true)
                        or simple equality
 Returns: if special=TRUE then does pattern match and returns true if 
           success; if special=false then returns sch=pch
---------------------------------------------------------------}
  var
    m: boolean;
  begin
    if special then
    begin
      if debug then write(' ',pch,'!',sch);
      m := false;
      if (pch = '0') then
      begin
        if (sch >= '0') and (sch <= '9') then
          m := true
      end
      else if pch = 'A' then
      begin
        if (sch >= 'A') and (sch <= 'Z') then
          m := true
        else
          if casefold and (sch >= 'a') and (sch <= 'z') then m := true;
      end
      else if pch = 'a' then
      begin
        if (sch >= 'a') and (sch <= 'z') then m := true;
      end
      else if pch = '@' then
      begin
        if ((sch < '0') or (sch > '9')) and
           ((sch < 'A') or (sch > 'Z')) and
           ((sch < 'a') or (sch > 'z')) then  m := true;
      end;
    end
    else
    begin
      if debug then write(' ',pch,':',sch);
      m :=  pch = sch;
    end;
    if debug then  if m then write('#');
    ChMatch := m;
  end;


Function StrPatt(var str,patt, outstr,outpatt: pms255; 
                       istr,ipatt,iout: integer): boolean;
{---------------------------------------------------------------
 Abstract: ??
 Parameters: ??
 Returns: ??
---------------------------------------------------------------}
  label 1;
  var
    i,j,min,savelen : integer;
    pch,sch,och: char;
    ormore, special,ospecial, match, zerook: boolean;
  begin

    if debug then
    begin
      writeln;
      write('   "');
      for i := istr to length(str) do write(str[i]);
      write('" : "');
      for i := ipatt to length(patt) do write(patt[i]);
      write('" => "');
      for i := iout to length(outpatt) do write(outpatt[i]);
      write('"');
    end;

1:
    StrPatt := false;
    if ipatt > length(patt) then 
    begin
      if istr > length(str) then StrPatt := true;
      for i := iout to length(outpatt) do
        AppendChar(outstr, outpatt[i]); 
      exit(StrPatt);
    end;
    
    pch := NextCh(patt, ipatt, special);
    if special then
    begin
      och := NextCh(outpatt, iout, ospecial);
      while not ospecial do
      begin
        AppendChar(outstr, outpatt[iout]);
        iout := iout + 1;
        och := NextCh(outpatt, iout, ospecial);
      end;
    end;
      
    if special and ((pch = '*') or (pch = '&') or (pch = '#')) then
    begin
      ormore := false;
    
      while (pch = '*') or (pch = '&') or (pch = '#') do
      begin
        ipatt := ipatt + 1;
        iout := iout + 1;
        if pch <> '*' then
        begin
          if istr > length(str) then exit(StrPatt);
          AppendChar(outstr, str[istr]);
          istr := istr + 1;
        end;
        { skip the required minimum match }

        if pch <> '#' then ormore := true;
        
        if (ipatt > length(patt)) or (iout > length(outpatt)) then
          pch := ' '
        else
          pch := patt[ipatt];
      end;
      { ipatt now points beyond the last wildcard }
      { and istr points beyond forced matches }
    
      if istr > length(str) then  { ran out of source chars }
      begin
        { failure because there wasn't enough to match the pattern? }
        StrPatt := (istr = length(str)+1) and (ipatt > length(patt));
        for i := iout to length(outpatt) do
          AppendChar(outstr, outpatt[i]); 
        exit(StrPatt); 
      end;

      if ormore then
      begin
        if ipatt > length(patt) then
        begin
          StrPatt := true;
          for i := istr to length(str) do AppendChar(outstr, str[i]);
          for i := iout to length(outpatt) do AppendChar(outstr, outpatt[i]);
        end
        else
        begin
          i := ipatt;
          pch := NextCh(patt, ipatt, special);
          ipatt := i;
          for i := istr to length(str) do
          begin
            sch := UpCh(str, i);
            if ChMatch(pch, sch, special) then
            begin
              if StrPatt(str,patt,outstr,outpatt, i,ipatt,iout) then
              begin
                StrPatt := true;
                exit(StrPatt);
              end
              else
                begin
                AppendChar(outstr, str[i]);
                if debug then writeln('    ');
                end
            end
            else
              AppendChar(outstr, str[i]);
          end;
          exit(StrPatt);    { match failed }
        end
      end
      else
        StrPatt := StrPatt(str,patt,outstr,outpatt, istr,ipatt,iout);
    end
    else
    begin { not a wildcard }
    
      sch := UpCh(str, istr);
      if ChMatch(pch, sch, special) then
      begin
        if special then
        begin
          AppendChar(outstr, str[istr]);
          iout := iout + 1;
        end;
        istr := istr + 1;
        ipatt := ipatt + 1;
        goto 1;
      end;
    end;
  end;


Function PattCheck(var p1,p2: pms255): boolean;
{---------------------------------------------------------------
 Abstract: Checks to see that two strings have the same pattern match
             characters in the same order
 Parameters: p1 and p2 are the patterns to compare
 Returns: true if have the same patterns in the same places; false otherwise
---------------------------------------------------------------}
var
  i1,i2: integer;
  special: boolean;
  ch1,ch2: char;
begin
  PattCheck := false;
  if (length(p1) = 0) or (length(p2) = 0) then exit(PattCheck);

  i1 := 1;
  i2 := 1;
  while (i1 <= length(p1)) and (i2 <= length(p2)) do
  begin
    special := false;
    while not special do
    begin
      ch1 := NextCh(p1, i1, special); { returns special & ' ' if off the end}
      i1 := i1 + 1;
    end;
    special := false;
    while not special do 
    begin
      ch2 := NextCh(p2, i2, special);
      i2 := i2 + 1;
    end; 
    if debug then write('  [',ch1,':',ch2,']');
    if ch1 <> ch2 then exit(PattCheck);
  end;
  PattCheck := true;
end;


Function PattMatch(var str, pattern: pms255; fold: boolean): boolean;
{---------------------------------------------------------------
 Abstract: Compares str against pattern
 Parameters: str - full string to compare against pattern;
             pattern - pattern to compare against.  It can have special
               characters in it
            fold - determines whether upper and lower case are distinct.  If
                   true then not.
 Returns: true string matches pattern; false otherwise
---------------------------------------------------------------}
var
  m: boolean;
begin
  if init <> #5652 then debug := false;
  if debug then
  begin
    writeln;
    write('PattMatch: "', pattern, '"  Str: "', str, '"');
  end;
  
  casefold := fold;
  m := false;
  Adjust(myout, 0);
  if (length(pattern) <> 0) and (length(str) <> 0) then
    m := StrPatt(str, pattern, myout, pattern, 1, 1, 1);
  if debug then 
    if m then writeln(' ===> ', myout) else writeln(' FAIL');
  PattMatch := m;
end;


Function PattMap(var str,inpatt,outpatt,outstr:pms255;fold:boolean):boolean;
{---------------------------------------------------------------
 Abstract: Compares str against inPatt, putting the parts of str that match
             inpatt into the corresponding places in outpatt and returning the
             result
 EXAMPLES:

       PattMap('test9.pas', 'test'0.pas', 'xtest'0.pas') => TRUE, 'xtest9.pas'
 
      PattMap('test9.pas', '*.pas', '*.ada') => TRUE,  'test9.ada'

 Parameters: str - full string to compare against pattern;
             inpatt - pattern to compare against.  It can have special
               characters in it
             outpatt - pattern to put the parts of str into;  it must have
               the same special characters in the same order as in inpatt
             outStr - the resulting string if PattMap returns true;
             fold-determines whether upper and lower case are distinct.  It
              true then not.
 Returns: true string matches pattern; false otherwise
 Errors: Raises BadPatterns if outPatt and inPatt do not have the same patterns
          in the same order
---------------------------------------------------------------}
var
  m: boolean;
begin
  if init <> #5652 then debug := false;
  if debug then
    writeln('PattMap: "', inpatt, '"  Str: "', str, '"');
  
  if not PattCheck(inpatt, outpatt) then Raise BadPatterns;
  
  casefold := fold;
  m := false;
  Adjust(outstr, 0);
  if (length(inpatt) <> 0) and (length(str) <> 0) then
    m := StrPatt(str, inpatt, outstr, outpatt, 1,1,1);
  if debug then 
    if m then writeln(' ===> ', outstr) else writeln(' FAIL');
  PattMap := m;
end.

