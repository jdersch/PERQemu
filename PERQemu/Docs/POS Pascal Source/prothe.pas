{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module PrOthers;
{-----------------------------------------------------------------}
{ Abstract:                                                       }
{     Utility procedures for PRINT utility. Moved to separate     }
{     module since PRINT.PAS was getting a bit on the large side  }
{-----------------------------------------------------------------}


{---------------------------}
{    Change Log             }
{---------------------------}
{
{ 16 May 84 V3.3  Rob Briggs (Perq Business Centre)
{   Added a facility to give a choice of RS232 channel A or B.
{   (Note changes also to Print and PrintUtils)

{ 17 May 83 V3.2  Sandeep Johar (3RCC)
{   Changed the help procedures so as to remove the ICL references when 
{   compiled for 3 rivers. 

{ 20 Apr 83 V3.1 LH
{ No changes to prothers.
{}

{ 15 Apr 83 V3.0 LH
{ Added /NOPAGE switch.
{}

{ 28-31 Mar 83 V3.0  LH
{ Put in better baud rate switch handling in Number.
{ Changed HELP file.
{ Tidied this section!
{}

{ 14/12/82 RMcL
{ Created OtherUtils from the device independent parts of PRINTUTILS.PAS
{}

{***********************} Exports {****************************}

  imports PrintUtils    from Printutils;
  imports RS232Baud     from RS232Baud;
  imports System        from System;
  imports Clock         from Clock;
  imports Profile       from Profile;
  imports Stream        from Stream;  (* remove when CmdParse catches PastEOF *)


const UsingProfile = TRUE;     { True to compile in User Profile code }

      DFLTBAUD = 9600;         { default speed for RS232 port }

      ERRFLG   = '**';
      
      TOPMARGIN = 0;           { lines to skip before and after title }
      BOTMARGIN = 1;
      DEFSHORT = TRUE;
      DEFNARROW = TRUE;

{$IFC Debug then}
   {$Message Debugging is ON.} {$ENDC}

      STall        = 1;
      SShort       = 2;
      SWide        = 3;
      SNarrow      = 4;
      SShift       = 5;
      STabs        = 6;
      SHelp        = 7;
      SCopies      = 8;
      SStart       = 9;
      SStop        = 10;
      SBaud        = 11;
      SRS232B      = 12;
      SDiablo      = 13;
      SHP          = 14;
      SLinePrinter = 15;
      STitle       = 16;
      SNoTitle     = 17;
      SBreak       = 18;
      SNoBreak     = 19;
      SPlain       = 20;
      SV80         = 21;
      SMl82a       = 22;
      SML84        = 23;
      SRicohRS232  = 24;
      SRicohGPIB   = 25;
      SRicohAptec  = 26;
      SAddr        = 27;
      SSingle      = 28;
      SContinuous  = 29;
      SNoPage      = 30;
      NumCmds      = 30;
      SNotFound    = NumCmds + 1;
      SNotUnique   = NumCmds + 2;


function StrToInt (NumS, whofrom: PString): integer;
{-------------------------------------------
{ Abstract:
{    Converts the string NumS to an integer.
{    The number must be sufficiently small,
{    and only simple error checking is done.
{
{ Errors:
{    raises Error if the string NumS has non-
{    numeric characters.
 -------------------------------------------}

procedure PrintHelp (**);
{--------------------------------------------------------------------
{ Abstract:
{    Types a Help message on the console describing the program.
{--------------------------------------------------------------------}

procedure DoSwitches (swrec: pSwitchRec;
                      var title,break,short,narrow,single,paged,RS232B: boolean;
                      var copies, start, stop, baud, tab, shift: integer;
                      var addr: gpDeviceAddress);
{--------------------------------------------------------------------
{ Abstract:
{    Handles switches.  DoSwitches looks at swrec, the list of switches
{    and sets the values of the switch variables appropriately.
{
{ Side-Effects:
{    sets the global Printer according to the type of printer.
{--------------------------------------------------------------------}

{$ifc UsingProfile then}
procedure ReadUserProfile (var title,break,short,narrow,single,paged,RS232B: boolean;
                           var copies, start, stop, baud, tab, shift: integer;
                           var addr: gpDeviceAddress;
                           var inargs, outargs: pArgRec;
                           var errorstr: longstr; var switches: pSwitchRec );
{---------------------------------------------------------------------
{ Abstract:
{
{   Reads the user's profile if one exists.  Profile settings can be
{   overridden by switches on the command line.
{
{ Calls:
{    PFileInit, ParseStringArgs, DoSwitches and Error.
{
{ Side-Effects:
{    Modifies global errorstr, inargs, outargs and switches.
{    Uses CurPFile and PFileEntry from module Profile.
{
{ Errors:
{    Raises exception Error if profile has invalid switches.
{    ReadUserProfile is aborted if no profile exists.
{---------------------------------------------------------------------}
{$endc}

procedure SendLine (ln: longstr; margin: integer);
{--------------------------------------------------------------------
{ Abstract:
{    Sends a string to the printer followed by CRLF.
{    The line is printed beginning in the margin column.
{--------------------------------------------------------------------}

procedure skip (n: integer);
{--------------------------------------------------------------------
{ Abstract:
{    Skips N lines downward by sending linefeeds to the printer.
{--------------------------------------------------------------------}

procedure head (fn: PathName; pnum, leftmargin: integer);
{--------------------------------------------------------------------
{ Abstract:
{    Makes a title at the top of a page which includes the name
{    of the file, the date and time, and the page number.
{--------------------------------------------------------------------}

{*************************} Private {***************************}

function StrToInt (NumS, whofrom: PString): integer;
{-------------------------------------------
{ Abstract:
{    Converts the string NumS to an integer.
{    The number must be sufficiently small,
{    and only simple error checking is done.
{
{ Errors:
{    raises Error if the string NumS has non-
{    numeric characters.
 -------------------------------------------}
var I, N: integer;
    minus: boolean;
begin
   minus := FALSE;
   I := 1;
   N := 0;
   while I <= Length(NumS) do
     begin
       minus := minus or (NumS[I] = '-');
       if NumS[I] in ['0'..'9', '-']
       then 
        begin
           if NumS[I] <> '-'
           then N := N * 10 + Ord(NumS[I]) - Ord('0')
        end
       else Raise Error(ErSwParam, whofrom, fatal);
        (* Raise Error(ErAnyError, Concat(' Switch value "',
                       Concat(NumS, '" is not numeric.')), fatal); *)
       I := I + 1
     end;   { while }
   if minus then StrToInt := -N else StrToInt := N
end;   { StrToInt }

procedure PrintHelp (**);
{--------------------------------------------------------------------
{ Abstract:
{    Types a Help message on the console describing the program.
{--------------------------------------------------------------------}
begin { PrintHelp }
   writeln;
   writeln('       Print is used to send files to various printers. ');
   writeln;
   {$ifc TRCCPrint Then}
   Writeln('       The following printers are supported. The switches are ');
   Writeln('       used to select the desired printers.');
   writeln;
   Writeln('             Diablo 630 daisy printer                  /DIABLO');
   writeln('             Hewlett-Packard 7310A                     /HP');
   writeln('             TI 810 lineprinter                        /LINEPRINTER');
   writeln('             Microline 82a printer                     /ML82A');
   writeln('             Versatec V80, GPIB printer                /ELECTROSTATIC');
   writeln('             RICOH correspondence printer, RS232       /RS232CORRESPONDENCE');
   writeln('             RICOH correspondence printer, GPIB        /GPIBCORRESPONDENCE');
   writeln('             OKIDATA, Matrix printer                   /MATRIX');
   writeln('             Simple, no special initialization         /PLAIN');
   {$elsec}
   writeln('       The printers which are supported are: ');
   writeln('             ICL 3185    Matrix Printer, ');
   writeln('             ICL 6202/02 Correspondence Printer (GPIB), ');
   writeln('             ICL 6202/03 Correspondence Printer (RS232), ');
   writeln('             ICL 6203    Electrostatic Printer,');
   writeln;
   writeln('       Other printers that can be used by print, but are not supported, are :');
   writeln('               Diablo 630 daisy printer                /DIABLO      ');
   writeln('               Hewlett-Packard 7310A                   /HP          ');
   writeln('               TI 810 (or similar) lineprinter         /LINEPRINTER ');
   writeln('               Microline 82a printer                   /ML82A       ');
   writeln('               Ricoh 1600 correspondence printer       /RICOH       ');
   writeln('                       on (Aptec) RS232');
   {$Endc}
   
   writeln;
   writeln('       Default settings may be included in the user profile.');
   writeln('       These are of the form:');
   writeln('               #print /gpib /tall /wide       ');
   writeln;
   
   {$ifc not TRCCPrint Then}
   writeln('       Print can be used to set up the printer by using the appropriate ');
   writeln('       switch options and specifying a nonexistent file name.');
   writeln;
   writeln('       Valid switches are (D = default):');
   writeln('               /HELP                   type this file');
   writeln('       Printers');
   writeln('               /MATRIX                 initialise to use the ICL 3185 ');
   writeln('                                               Matrix Printer');
   writeln('               /GPIBCORRESPONDENCE     initialise to use the ICL 6202/02 ');
   writeln('                                               Correspondence Printer on GPIB');
   writeln('               /RS232CORRESPONDENCE    initialise to use the ICL 6202/03 ');
   writeln('                                               Correspondence Printer on RS232');
   writeln('               /ELECTROSTATIC          initialise to use the ICL 6203 ');
   writeln('                                               Electrostatic Printer.');
   writeln('               /PLAIN       D          print on RS232 printer with no');
   writeln('                                               initialisation');
   {$elseC}
   writeln('       The various other switches associated with the print');
   writeln('       utility are:');
   writeln;
   {$endc}
   
   writeln('       Character spacing');
   writeln('               NOTE:The electrostatic printer has only one print format.');
   writeln('               /TALL           6 lines per inch');
   writeln('               /SHORT       D  8 lines per inch');
   writeln('               /WIDE           10 characters per inch');
   writeln('               /NARROW      D  12 to 16.5 characters per inch');
   writeln('                                       ( depends on printer )');
   writeln('       Print format');
   writeln('               /TABS=<n>       tab stop every <n> characters');
   writeln('                                       D=8');
   writeln('               /SHIFT=<n>      shift the listing <n> spaces to the right ');
   writeln('                                       D=0');
   writeln('               /TITLE          print a title on each page');
   writeln('                                       (title line plus one blank line)');
   writeln('               /NOTITLE        omit the title');
   writeln('                                       D=NOTITLE for ".doc" files ');
   writeln('                                       D=TITLE   for anything else');
   writeln('       File format');
   writeln('               /BREAK          print a break page before each file');
   writeln('               /NOBREAK     D  omit the break page');
   writeln('               /START=<n>      print starting at page <n> ');
   writeln('                                       D=1');
   writeln('               /STOP=<n>       print ending at page <n>');
   writeln('                                       D=last page of file');
   writeln('               /SINGLE         print one page then pause');
   writeln('               /CONTINUOUS  D  print pages continously');
   writeln('               /NOPAGE         do not format the file');
   writeln('               /COPIES=<n>     print <n> copies of the file ');
   writeln('                                       D=1');
   writeln('       Interface options');
   writeln('               /BAUD=<n>       set the RS232 baud rate to <n> ');
   writeln('                                       D=9600');
   writeln('               /ADDR=<n>       set the GPIB device address to <n>');
   writeln('                                       D=4 for /gpib');
   writeln('                                       D=1 for anything else');
   writeln('               /RS232B         set the RS232 channel to RSB');
   writeln('                                       D=RSA');
   writeln('       Command form:');
   writeln('               print filename {,filename} {/switch}');
   writeln;
end   { PrintHelp };

procedure DoSwitches (swrec: pSwitchRec;
                      var title,break,short,narrow,single,paged,RS232B: boolean;
                      var copies, start, stop, baud, tab, shift: integer;
                      var addr: gpDeviceAddress);
{--------------------------------------------------------------------
{ Abstract:
{    Handles switches.  DoSwitches looks at swrec, the list of switches
{    and sets the values of the switch variables appropriately.
{
{ Side-Effects:
{    sets the global Printer according to the type of printer.
{--------------------------------------------------------------------}


    function Number (switch: pSwitchRec; Default, Min, Max: Integer;
                     whofrom: PString): Integer;
    {----------------------------------------------------------------
    { Abstract:
    {    Number scans the string containing numeric switch arguments.
    {
    { Parameters:
    {    Min and Max set the range of allowable values, default is
    {    used if no value was typed for this switch.  Switch points
    {    to the list of switches.
    {
    { Errors:
    {    Gives an error message for values which are out of range.
    {    Switch should not be NIL.
     ----------------------------------------------------------------}
    var NumS, SwitchName: longstr;
        N: Integer;

    begin { Number }
      NumS := '###!!! Default';
      SwitchName := '';
      N := Default;
      if switch <> NIL then begin
         NumS := switch^.arg;
         SwitchName := switch^.switch
       end;   { then }
      if NumS <> '' then N := StrToInt(NumS, whofrom);
      if Switchname <> 'BAUD'
      then
         begin
            if (N < Min) or (N > Max) 
            then begin
               Writeln(ERRFLG, SwitchName, ' ranges from ', Min:1, ' to ', Max:1, '.');
               Raise Error(ErAnyError,
                        Concat(ERRFLG, Concat(NumS, ' is out of range.')), fatal)
            end;
         end  
      else
         while NOT (   (N=110)  or (N=150)  or (N=300)  or (N=600)
                    or (N=1200) or (N=2400) or (N=4800) or (N=9600) )
         do begin
            writeln(ERRFLG,'Invalid BAUD rate.');
            writeln('  Valid BAUD rates are 9600, 4800, 2400, 1200, 600, 300, 150, 110.');
            write('  Enter required BAUD rate  :');
            readln(N);
            end;
      Number := N       { set the value }
    end { Number };

    var i: integer;
        SSwitch: CmdArray;
        SaveSwrec: pSwitchRec;

    begin       { DoSwitches }

     { initialize the list of switches }
       SSwitch[SAddr       ] := 'ADDR';
       SSwitch[SRicohGPIB  ] := 'GPIBCORRESPONDENCE';
       SSwitch[SRicohRS232 ] := 'RS232CORRESPONDENCE';
       SSwitch[SRicohAptec ] := 'RICOH';
       SSwitch[SML84       ] := 'MATRIX';
       SSwitch[SMl82a      ] := 'ML82A';
       SSwitch[SV80        ] := 'ELECTROSTATIC';
       SSwitch[SBaud       ] := 'BAUD';
       SSwitch[SBreak      ] := 'BREAK';
       SSwitch[SRS232B     ] := 'RS232B';
       SSwitch[SContinuous ] := 'CONTINUOUS';
       SSwitch[SCopies     ] := 'COPIES';
       SSwitch[SDiablo     ] := 'DIABLO';
       SSwitch[SHelp       ] := 'HELP';
       SSwitch[SHP         ] := 'HP';
       SSwitch[SLinePrinter] := 'LINEPRINTER';
       SSwitch[SPlain      ] := 'PLAIN';
       SSwitch[SNarrow     ] := 'NARROW';
       SSwitch[SNoBreak    ] := 'NOBREAK';
       SSwitch[SNoPage     ] := 'NOPAGE';
       SSwitch[SNoTitle    ] := 'NOTITLE';
       SSwitch[SShift      ] := 'SHIFT';
       SSwitch[SShort      ] := 'SHORT';
       SSwitch[SSingle     ] := 'SINGLE';
       SSwitch[SStart      ] := 'START';
       SSwitch[SStop       ] := 'STOP';
       SSwitch[STabs       ] := 'TABS';
       SSwitch[STall       ] := 'TALL';
       SSwitch[STitle      ] := 'TITLE';
       SSwitch[SWide       ] := 'WIDE';

       SaveSwrec := swrec;               { Remember beginning of command line }
       
                          { If there's an /elec or /gpib in command line then }
                          {    set Addr = ( default ICL address of printer ). }
       while swrec <> NIL do begin
          ConvUpper(swrec^.switch);             { only changes the local copy }
          case UniqueCmdIndex(swrec^.switch, SSwitch, NumCmds) of
             SV80      : Addr := 1;
             SRicohGPIB: Addr := 4;
           end;  { case }
          swrec := swrec^.next;                             { do the next one }
       end;
       
       swrec := SaveSwrec;                 { Point to beginning of line again }
       
       while swrec <> NIL do begin
          ConvUpper(swrec^.switch);             { only changes the local copy }
          case UniqueCmdIndex(swrec^.switch, SSwitch, NumCmds) of
             STall:      if swrec^.arg = '' then Short := False
                            else Raise Error(ErNoSwParam, 'Tall', recover);
             SShort:     if swrec^.arg = '' then Short := True
                            else Raise Error(ErNoSwParam, 'Short', recover);
             SWide:      if swrec^.arg = '' then Narrow := False
                            else Raise Error(ErNoSwParam, 'Wide', recover);
             SNarrow:    if swrec^.arg = '' then Narrow := True
                            else Raise Error(ErNoSwParam, 'Narrow', recover);
             SHelp:      PrintHelp(**);
             SBreak:     if swrec^.arg = '' then break := TRUE
                            else Raise Error(ErNoSwParam, 'Break', recover);
             SNoBreak:   if swrec^.arg = '' then break := FALSE
                            else Raise Error(ErNoSwParam, 'NoBreak', recover);
             STitle:     if swrec^.arg = '' then title := TRUE
                            else Raise Error(ErNoSwParam, 'Title', recover);
             SNoTitle:   if swrec^.arg = '' then title := FALSE
                            else Raise Error(ErNoSwParam, 'NoTitle', recover);
             SSingle:    if swrec^.arg = '' then single := TRUE
                            else Raise Error(ErNoSwParam, 'Single', recover);
             SContinuous:if swrec^.arg = '' then single := FALSE
                           else Raise Error(ErNoSwParam, 'Continuous',recover);
             SNoPage:    if swrec^.arg = '' then Paged := FALSE
                           else Raise Error(ErNoSwParam, 'NoPage',recover);
             SRS232B:    if swrec^.arg = '' then RS232B := TRUE
                           else Raise Error(ErNoSwParam, 'RS232B',recover);
   
             SShift:     Shift := Number(swrec, 0, 0, 100, swrec^.switch);
             STabs:      Tab := Number(swrec, 8, 1, 128, swrec^.switch);
             SCopies:    Copies := Number(swrec, 1, 1, 100, swrec^.switch);
             SStart:     Start := Number(swrec, 1, 1, MAXINT, swrec^.switch);
             SStop:      Stop := Number(swrec, MAXINT, 1, MAXINT, swrec^.switch);
             SBaud:      Baud := Number(swrec, DFLTBAUD, 110, 9600,
                                        swrec^.switch);
             SAddr:      Addr := Number(swrec, GPIBCODE, 0, 31, swrec^.switch);
             SDiablo:    Printer.ptype := diablo;
             SLinePrinter: Printer.ptype := lineprinter;
             SHP:        Printer.ptype := hp;
             SPlain:     Printer.ptype := plain;
             SV80:       Printer.ptype := V80;
             SMl82a:     Printer.ptype := Ml82a;
             SML84:      Printer.ptype := ML84;
             SRicohRS232: Printer.ptype := RicohRS232;
             SRicohGPIB: Printer.ptype := RicohGPIB;
             SRicohAptec: Printer.ptype := RicohAptec;
             SNotFound:  Raise Error(ErBadSwitch, swrec^.switch, fatal);
             SNotUnique: Raise Error(ErSwNotUnique, swrec^.switch, fatal);
          otherwise: Raise Error(ErBadSwitch, swrec^.switch, fatal);
           end;  { case }
       swrec := swrec^.next;    { do the next one }
    end;
end;    { DoSwitches }

{$ifc UsingProfile then}
procedure ReadUserProfile (var title,break,short,narrow,single,paged,RS232B: boolean;
                           var copies, start, stop, baud, tab, shift: integer;
                           var addr: gpDeviceAddress;
                           var inargs, outargs: pArgRec;
                           var errorstr: longstr; var switches: pSwitchRec );
{---------------------------------------------------------------------
{ Abstract:
{
{   Reads the user's profile if one exists.  Profile settings can be
{   overridden by switches on the command line.
{
{ Calls:
{    PFileInit, ParseStringArgs, DoSwitches and Error.
{
{ Side-Effects:
{    Uses CurPFile and PFileEntry from module Profile.
{
{ Errors:
{    Raises exception Error if profile has invalid switches.
{    ReadUserProfile is aborted if no profile exists.
{---------------------------------------------------------------------}

    handler PNotFound(FileName: string {should be PathName} );
    {--------------------------------------------------------------------
    { Abstract:
    {     Handle exception raised by PFileInit when unable to find
    {     user profile.  We don't do anything since not having a
    {     profile is not an error (simply an indication of bogosity).
    {
    {--------------------------------------------------------------------}

    begin
       exit(ReadUserProfile);
    end;


const PFMARKER = 'PRINT';
var PLine: CString;
    success: boolean;

begin                       
   PFileInit(CurPFile, PFMARKER);
   PLine := PFileEntry;
   while PLine <> '' do
      begin
        success := ParseStringArgs(PLine, inargs, outargs, switches, errorstr);
        if not success then begin
             Writeln(ERRFLG, 'Error encountered while reading Profile.');
             Raise Error(ErAnyError, errorstr, fatal)
           end else DoSwitches (switches, title, break, short, narrow, single,
                                paged,RS232B, copies, start, stop, baud, tab,
                                shift, addr);
        PLine := PFileEntry;
      end   { while }
end;   { ReadUserProfile }
                        
{$endc}
procedure SendLine (ln: longstr; margin: integer);
{--------------------------------------------------------------------
{ Abstract:
{    Sends a string to the printer followed by CRLF.
{    The line is printed beginning in the margin column.
{--------------------------------------------------------------------}
var i: integer;
begin
   if length(ln) > 0 then begin
      for i := 1 to margin do
         SendCh(' ');
      for i := 1 to length(ln) do
         SendCh(ln[i])
     end;   { then }
   SendCh(CR);
   SendCh(LF)
end;   { SendLine }



procedure skip (n: integer);
{--------------------------------------------------------------------
{ Abstract:
{    Skips N lines downward by sending linefeeds to the printer.
{--------------------------------------------------------------------}
begin
   SendCh(CR);
   while n > 0 do begin
      SendCh(LF);
      n := n - 1
    end   { while }
end;   { skip }



procedure head (fn: PathName; pnum, leftmargin: integer);
{--------------------------------------------------------------------
{ Abstract:
{    Makes a title at the top of a page which includes the name
{    of the file, the date and time, and the page number.
{--------------------------------------------------------------------}
var DateTime: TimeString;       { declared in Clock }
    width, i: integer;
    TitleLine, Spacestr: LongStr;

begin
   GetTString(DateTime);
   Spacestr:='                                                             ';
       {Large number of blank spaces, correct number selected by "Substr".}
   Spacestr:=Concat(Spacestr,Spacestr);
   Spacestr:=Concat(Spacestr,Spacestr);
   width:=Printer.ColWidth-30;  {allow for date/time/page no. }
   if Printer.ptype=Ml82a then width:=66-30; {title is in wide chars on ml82a}
   if length(fn) > width then begin      { filename is longer than width }
      TitleLine:='...';  { (3 chars long) }
      TitleLine:=Concat(TitleLine,Substr(fn,length(fn)-width+1+3,width-3));
    end else TitleLine := fn;
      TitleLine := Concat(TitleLine,Substr(Spacestr,1,width-length(TitleLine)+1));
      TitleLine := Concat(TitleLine, DateTime);
      TitleLine := Concat(TitleLine, Concat('   Page ', IntToStr(pnum)));
  
   skip(TOPMARGIN);
   if (Printer.ptype = Ml82a) and printing then
       begin
          Sendch(GS); {narrow chars}
          Sendch(US); {set 8.3 cpi}
          Sendline(TitleLine, leftmargin);
          if (Printer.ColWidth <=80)
                  then Sendch(RS) else Sendch(GS); {restore typeface}
       end
   else SendLine(TitleLine, leftmargin);
   skip(BOTMARGIN)
end.   { head }
