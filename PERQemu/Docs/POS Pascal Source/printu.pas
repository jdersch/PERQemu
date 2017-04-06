{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module PrintUtils;
{-----------------------------------------------------------------}
{ Abstract:                                                       }
{     Utility procedures for PRINT utility. Moved to separate     }
{     module since PRINT.PAS was getting a bit on the large side  }
{-----------------------------------------------------------------}


{---------------------------}
{    Change Log             }
{---------------------------}
{  5 Jun 84 V3.3 John Jervis (Perq Business Centre)
{  SendCh now allows FF to be sent to non-Ricoh printers when /tall and
{  no /NOPAGE set.

{ 16 May 84 V3.3 Rob Briggs (Perq Business Centre)
{  Added a facility to give a choice of RS232 channel A or B.
{  (Note also changes to Print and Prothers)

{ 15 May 83 V3.2 Sandeep Johar
{  Put in some code to make it specific to 3RCC requirements:
{    1> new help message.
{    2> Form length set to 11 inches instead of 11 3/4 inch of UK.

{ 20 Apr 83 V3.1 LH
{ Changed initialisation of RH margin and handling of A4 FF for RicohRS232 so
{ they now work!
{}

{ 15 Apr 83 V3.0 LH
{ Added CR to every FF so carriage returns to beginning of line.
{ Added /NOPAGE switch.
{ Note that there are now the maximum number of switches allowed by POS.
{}

{ 28-31 Mar 83 V3.0  LH
{ Incorporated Ricoh Ricoh RS232 initialisation.
{ Added delay at end of RicohRS232 listing to 
{ prevent data corruption.
{ Changed order of printers in Initprinter so supported printers at beginning.
{ Tidied this section!
{}

{ 14/12/82 RMcL
{ PrintUtils now contains only printer-dependent routines
{ Other routines now in OtherUtils.pas
{}

{25/11/82 LH
{ Added high volume print facility
{ for V80.
{}

{15/11/82 LH
{ Started making changes for Z80 rewrite.
{ Deleted procedure nap (unwanted reference to device table).
{}

{28/9/82 RMcL 
{ Changed Ricoh RS232 handling back to simple DTR protocol
{ - note Ricoh cable now wired (Ricoh) DTR <-> CTS (PERQ)
{}

{8/9/82 RMcL }
{ Added RS232 block protocol handling for RICOH 1600 (ETX/ACK) and (DC1/DC3) 
{ Uses new switches /PROTOCOL and /BLOCK for use for commissioning only         {}

{***********************} Exports {****************************}

  imports CmdParse      from CmdParse;
  imports PERQ_String   from PERQ_String;
  imports GPIB          from GPIB;
  imports IO            from IO;   
  imports IOErrors      from IOErrors;
  imports IOErrMessages from IOErrMessages;



const Debug = FALSE;            { FALSE for final, fully tested version }
      TRCCPrint  = TRUE;       { This switch turns on the 3RCC specific stuff:
                                 1> The Help message is different.
                                 2> The form length defaults to 11 inches. }

      GPIBCODE = 1;            { Assume printer is device address 1 on GPIB }
      WAIT = 450;              { delay for the slowest GPIB printer }
      
      PAGEMESS = 'Press RETURN when ready to print a page.';
      
      DEFDATASEG = 0;

{$IFC Debug then}
   {$Message Debugging is ON.} {$ENDC}

      { ASCII values }
{$INCLUDE Ascii.dfs}
      
      {V80 Command codes}
      FORMFEED  = Chr(#002);
      LINEFEED  = Chr(#010);
      PRINTMODE = Chr(#120);
      DATAMODE  = Chr(#200);
      BUFFCLEAR = Chr(#001);
      
      V80PrintMode = Chr(#320);  { = PRINTMODE + DATAMODE }

      STRIPPARITY = 127;

type
      longstr  = PString;    { longest possible string size (from PERQ_String) }
      printrtyp = record
                     ptype: (plain, hp, diablo, canon, lineprinter,
                             v80, Ml82a, ML84, RicohRS232, RicohGPIB,
                             RicohAptec);
                     name: longstr;
                     port: (ChannelA,ChannelB, igpib, hvgpib);
                     hastabs: boolean;
                     hasformfeed: boolean;
                     colwidth: integer
                   end;   { printrtyp }
      ErrDegree = (recover, fatal);
         
var
      Printer    : printrtyp;
      linenum    : integer;
      Tab        : integer;
      Shift      : integer;
      narrow     : boolean;
      paged      : boolean;
      printing   : boolean;
      short      : boolean;
      single     : boolean;
      addr       : gpDeviceAddress;
         
exception Error(errorcode: ErrorType; msg: longstr; degree: ErrDegree);

procedure RSendCh (Ch: Char);
{----------------------------------------------------------------
{ Abstract:
{    RSendCh takes the argument Ch and sends it to the RS232 port.
{    It doesn't return until the character has been printed (or
{    unless an error condition is encountered which is announced).
{
{ Errors:
{    Raises exception Error if the IO operation failed and gives reason.
 ----------------------------------------------------------------}

procedure RSendTab(T: integer);
{----------------------------------------------------------------}
{ Abstract:                                                      }
{     RSendTab takes an integer which is a tab setting (for an   }
{     OKI ML84) and sends it to the RS232 port. It is sent as    }
{     a string of 3 ascii digits (eg 10 = '0' '1' '0' ),         }
{     followed by an ascii comma.                                }
{----------------------------------------------------------------}

procedure GSendCh (Ch: char);
{--------------------------------------------------------
{ Abstract:
{    Sends Ch to a printer that is attached to the GPIB
{    interface port.  For HP printers, checks that the
{    buffer is not full on CR, otherwise delays.
{    For V80 printers, flushes the buffer on LF.
{
{ Parameters:
{    Ch is the character to be printed.
{
{ Calls:
{    GPIB routines gpPutByte, gpFlushBuffer,
{    gpITalkHeListens and gpHeTalksIListen.
{
{ Globals:
{    Checks the global variable Printer to determine
{    type of printer in use.
{-----------------------------------------------------------}

procedure SendCh (Ch: char);
{--------------------------------------------------------------------
{ Abstract:
{    SendCh is used to print each character.
{
{ Parameters:
{    Ch is the character to be printed.
{
{ Side-Effects:
{    Checks the global Printer variable to determine printer type.
{
{ Calls:
{    Either RSendCh, HVSendch or GSendCh depending on the printer port.
{--------------------------------------------------------------------}

procedure InitPrinter;
{--------------------------------------------------------------------
{ Abstract:
{     InitPrinter is used to do printer dependent initialisation
{     for text printing.
{ Side-Effects:
{     Uses the global Printer variable to determine printer type.
{--------------------------------------------------------------------}

procedure ClosePrinter;
{--------------------------------------------------------------------
{ Abstract:
{     ClosePrinter is used to do printer dependent close down 
{     of text printing. 
{-------------------------------------------------------------------}

{*************************} Private {***************************}




procedure RSendCh (Ch: Char);
{----------------------------------------------------------------
{ Abstract:
{    RSendCh takes the argument Ch and sends it to the RS232 port.
{    It doesn't return until the character has been printed (or
{    unless an error condition is encountered which is announced).
{
{ Errors:
{    Raises exception Error if the IO operation failed and gives reason.
 ----------------------------------------------------------------}
var S: longstr;
    Status: Integer;
    Channel: Integer;
    InCh: Char;

begin { RSendCh }

  if    printer.port=ChannelA
  then  Channel := RSA
  else  if    printer.port=ChannelB
        then  Channel := RSB;
        
  repeat Status := IOCWrite(Channel,Ch)      { wait until not busy and empty }
  until (Status <> IOEIOB) and (Status <> IOECBF);
  if Status <> IOEIOC then      { completed successfully? }
    begin
      S := IOErrString(Status);
      Raise Error(ErAnyError, Concat('Printer error: ', S), recover)
    end;  { then }
  if ch = LF
  then while IOCRead(Channel,InCh) <> IOEIOB do;
end { RSendCh };

procedure RSendTab(T: integer);
{----------------------------------------------------------------}
{ Abstract:                                                      }
{     RSendTab takes an integer which is a tab setting (for an   }
{     OKI ML84) and sends it to the RS232 port. It is sent as    }
{     a string of 3 ascii digits (eg 10 = '0' '1' '0' ),         }
{     followed by an ascii comma.                                }
{----------------------------------------------------------------}
var
   f: integer;
   i: integer;
   D: integer;
begin
   f := 100;
   for i := 1 to 3 do
   begin
      D := (T div f) + ord('0');
      T := T mod f;
      f := f div 10;
      RSendch(chr(D));
   end;
   RSendch(',');
end;

procedure GSendCh (Ch: char);
{--------------------------------------------------------
{ Abstract:
{    Sends Ch to a printer that is attached to the GPIB
{    interface port.  For HP printers, checks that the
{    buffer is not full on CR, otherwise delays.
{    For V80 printers, flushes the buffer on LF.
{
{ Parameters:
{    Ch is the character to be printed.
{
{ Calls:
{    GPIB routines gpPutByte, gpFlushBuffer,
{    gpITalkHeListens and gpHeTalksIListen.
{
{ Globals:
{    Checks the global variable Printer to determine
{    type of printer in use.
{-----------------------------------------------------------}
var 
   i: integer;
   bite: gpByte; 

label 1;

handler GPIBError (S:integer);
begin
    if S=IOETIM then begin  {timeout}
       writeln('Printer timed out putting a byte.');
       write  ('Press RETURN when ready to go on to next character', 
               CmdChar, ' ');
       readln;
       if (Printer.PType = hp) and (Ch = CR) then
           gpITalkHeListens(GPIBCODE);        { Make me the talker again }
       goto 1;
    end else 
       Raise Error(ErAnyError,
               Concat('Printer error: ', IOErrString(S)), fatal);
end;    { GPIBError }


begin
 case Printer.Ptype of
        
 v80:
      begin
        gpPutByte(ord(ch));
      end;
 ricohgpib: 
      begin
        gpPutByte(ord(ch));
      end;
 otherwise:
      begin
      if (Printer.PType = hp) and (Ch = CR)
      then begin
           gpPutByte(ord(ENQ));         { ask if printer has buffer space }
           gpFlushBuffer;               { make it happen }
           gpHeTalksIListen(GPIBCODE);  { Listen for response }
           bite := gpGetByte;           { should be ACK, go ahead }
           gpITalkHeListens(GPIBCODE);  { Make me the talker again }
           end; 
      gpPutByte(ord(Ch));               { print it }
      end;
 end; { of case }
1:
end;    { GSendCh }

      
procedure SendCh (Ch: char);
{--------------------------------------------------------------------
{ Abstract:
{    SendCh is used to print each character.
{
{ Parameters:
{    Ch is the character to be printed.
{
{ Side-Effects:
{    Checks the global Printer variable to determine printer type.
{
{ Calls:
{    Either RSendCh or GSendCh depending on the printer port.
{--------------------------------------------------------------------}
begin
(* if Debug then write(Ch); *)
   if     (ch = FF) 
      and (NOT short)
      and Paged
      and Printing
      and ((printer.ptype = RicohGPIB)
        or (printer.ptype = RicohAptec)
        or (printer.ptype = RicohRS232))

   then 
    begin
      if   ((printer.ptype = RicohGPIB)
         or (printer.ptype = RicohAptec))
      then
       begin
         {$ifc not TRCCPrint Then}
         Sendch(ESC);Sendch(VT);Sendch(chr(71));    { absolute tab to line 70 }
         Sendch(ESC);Sendch(RS);Sendch(chr(5));      { set VMI to 4, = 12 lpi }
         
         {$EndC}
         if printer.ptype = RicohGPIB
         then GSendch(FF)                               { have tabbed 11 2/3" }
         else RSendch(FF);                                   { FF extra 1/12" }
         
         {$ifc not TRCCPrint Then}
         Sendch(ESC);Sendch(RS);Sendch(chr(9));                 { restore VMI }
         {$endC}
       end;
      if (printer.ptype = RicohRS232)
      then
       begin
         RSendch(FF);                                         { FF to line 66 }
         {$ifc not TRCCPrint Then}
         Sendch(ESC);Sendch(RS);Sendch(chr(7));       { set VMI to 6, = 8 lpi }
         Sendch(ESC);RSendch(FF);Sendch(chr(6));                 { FF = 6 lpp }
         RSendch(FF);                                         { FF extra 3/4" }
         Sendch(ESC);Sendch(RS);Sendch(chr(9));                 { restore VMI }
         Sendch(ESC);RSendch(FF);Sendch(chr(66));           { set FF = 66 lpp }
         {$EndC}
       end;
      Sendch(CR);
      linenum := 0;
    end
   else 
    begin
      if ch = LF then linenum := linenum + 1;
      if ch = FF
      then
       begin
         linenum := 0;
         if Paged and Printing then Sendch(CR)              { Return carriage }
       end;
      if printing
      then 
       begin
         case printer.port of
            ChannelA,ChannelB    : RSendch(Ch);
            igpib                : GSendch(Ch)
         end  { of case }
       end;
    end;
end;    { SendCh }

procedure InitPrinter;
{--------------------------------------------------------------------
{ Abstract:
{    Initializes the printer as specified by the user's profile
{    or switches.  Sends the proper escape sequences to set tabs,
{    and other printer options if they exist.
{
{--------------------------------------------------------------------}
var I, J: integer;
    Reply: char;
   
   {----------------------------------------------------------------}
   
   Procedure InitAptec;
   {----------------------------------------------------------------
   {     Common initialisation for "RS232Correspondence" and "Ricoh"
   {     i.e. Aptec RS232 and GPIB interfaces.
   {----------------------------------------------------------------}
   begin
      printer.hastabs := TRUE;
      printer.hasformfeed := FALSE;
      if Narrow then printer.colwidth := 86 else printer.colwidth := 73;
      
      Sendch(ESC);Sendch('5');                         { set forward printing }
      Sendch(ESC);Sendch('B');                               { print in black }
      Sendch(ESC);Sendch('2');                              { clear tab stops }
      Sendch(ESC);Sendch('4');                                 { graphics off }
      Sendch(ESC);Sendch('X');                    { cancel bold and underline }
      Sendch(ESC);Sendch('C');                 { clear top and bottom margins }
      Sendch(ESC);Sendch('Q');              { switch off proportional spacing }
                
                                                          {set format factors }
      Sendch(ESC);Sendch(RS);                                       { set VMI }
      if Short  then Sendch(chr(7))  else Sendch(chr(9));
      Sendch(ESC);Sendch(US);                                       { set HMI }
      if Narrow then Sendch(chr(11)) else Sendch(chr(13));

      {Set right hand margin}
      Sendch(ESC);Sendch(HT);Sendch(chr(printer.colwidth));
         {Put carriage in correct position}
      Sendch(ESC);Sendch('0');
         {Set RH margin at that position}
      Sendch(CR);
   end; { procedure InitAptec }


{---------------------------------------------------------------}
{        main body of InitPrinter                               }
{---------------------------------------------------------------}
begin
   case printer.ptype of

       {------------------------------------------------------}
       ML84: begin
               printer.name := 'Matrix';
               printer.hastabs := TRUE;
               printer.hasformfeed := TRUE;
               if Narrow 
                  then printer.colwidth := 220 
                  else printer.colwidth := 132;

               RSendch(CAN); {clear printer buffer}
               RSendch(DC1); {on line the printer }
               RSendch(ESC); {select ordinary     }
               RSendch('0'); {character generator }
               if not single then RSendch(FF);  {Send FF, so at top of form.}
               RSendCh(ESC); {short or tall       }
               if Short then RSendCh('8') else RSendCh('6');
               if Narrow then RSendCh(FS) else RSendCh(RS);
                       { narrow or wide }

               RSendch(ESC); {Set horizontal tabs}
               RSendch(HT); 
               I := Tab + Shift + 1;
               J := 1;
               While (I <= printer.colwidth) and (J <= 16) do
               begin
                 RSendTab(I);
                 I := I + Tab;
                 J := J + 1
               end; { while }
               RSendch(CR);    
            {Set form length explicitly.}
               RSendch(ESC);
               RSendch('G');
               RSendch('0');
               RSendch('0');  {Form length is set by rotary switch.}
               RSendch(ESC);
               RSendch('5');  {Set top of form, else form length not set.}
               
               RSendch(CR);
                             
             end; {ML84}
       {----------------------------------------------}
       RicohGPIB: begin
               printer.name := 'Correspondence (GPIB)';
               printer.port := igpib;
               gpinit;                              { initialise GPIB package }
               gpITalkHeListens(Addr);       { PERQ = talker, printer listens }

               InitAptec;

               Sendch(ESC); Sendch('I');                   { set primary font }
               
               Sendch(ESC); GSendch(FF);                     { set lines/page }
               if Short then Sendch(chr(94)) else Sendch(chr(71));

               i := Tab + Shift + 1;               { set horizontal Tab stops }
               while (i <= printer.colwidth)
                     and
                     (i <= 127)                       { no tabstops after 127 }
               do
               begin
                  Sendch(ESC);Sendch(HT);           { absolute HT to Tab posn }
                  GSendch(chr(i));
                  Sendch(ESC);Sendch('1');                     { set Tab posn }
                  i := i + Tab;
               end; { while }
               Sendch(CR);                { Put carriage at beginning of line }
             end; { RicohGPIB }
       {----------------------------------------------}
       RicohRS232: begin
               printer.name := 'Correspondence (RS232)';
               printer.hastabs := FALSE;
               printer.hasformfeed := TRUE;         { What does this mean? LH }

               Sendch(ESC);Sendch(SUB);Sendch('I');           { Reset printer }
               for i:=1 to 500 do;                             { Delay needed }
                                              { Bug in Ricoh RS232 interface? }
                  { Printer now in state of :  form length set by switches,   }
                  {                            6lpi (Tall), 10cpi (Wide),     }
                  {                            "selected".                    }
                                            
               if Short 
               then begin
                  Sendch(ESC);Sendch(RS);Sendch(chr(7));    { Set VMI : 8lpi  }
                  end;
               if Narrow
               then begin
                  Sendch(ESC);Sendch(US);Sendch(chr(11));   { Set HMI : 12cpi }
                  printer.colwidth := 86;                             { 86cpl }
                  end
               else printer.colwidth := 73;                           { 73cpl }

               Sendch(ESC);Rsendch(FF);
               if Short 
               then Sendch(chr(94))                           { lpp = 11 3/4" }
               else Sendch(chr(66));  { lpp = 11", when FF sent do extra 3/4" }
               
               Sendch(ESC);Sendch('9');         { Set left margin at far left }

               if Narrow 
               then
                begin
                  if ((printer.colwidth + shift) <= 163)
                  then for i:=1 to (printer.colwidth + shift) do Sendch(' ')
                  else for i:=1 to 163 do Sendch(' ')
                end
               else
                  if ((printer.colwidth + shift) <= 136)
                  then for i:=1 to (printer.colwidth + shift) do Sendch(' ')
                  else for i:=1 to 136 do Sendch(' ');
               Sendch(ESC);Sendch('0');     { Set right margin at end of line }

               Sendch(CR);                { Put carriage at beginning of line }

             end; { RicohRS232 }
       {----------------------------------------------}
        v80: begin
               printer.name := 'Electrostatic';
               printer.hastabs := FALSE;
               printer.hasformfeed := TRUE;
               printer.colwidth := 132;
               printer.port := igpib;
               gpinit;            { initialise the gpib package }
               gpITalkHeListens(addr);  { PERQ talks, V80 listens }
               gpAuxCommand(gpfeoi,gpdontcare);
               GSendch(NUL);
               GSendch(FORMFEED);
               GSendch(PRINTMODE);
               GSendch(DATAMODE);
               gpFlushBuffer;
               { no RESET to save paper }
           end;   { V80 }
       {----------------------------------------------}
        plain: begin
                  printer.name := 'plain';
                  printer.hastabs := FALSE;
                  printer.hasformfeed := TRUE;
                  printer.colwidth := 80;
                { no initialization is done }
               end;   { plain }
       {------------------------------------------------------}
        diablo: begin
                   printer.name := 'diablo';
                   printer.hastabs := TRUE;
                   printer.hasformfeed := TRUE;
                   printer.colwidth := 80;

                   I := 100;
                   repeat I := I - 1;
                     RSendCh(ETX);
                     J := 100;
                     repeat J := J - 1;
                       if IOCRead(RS232In, Reply) <> IOEIOC
                          then Reply := NUL
                     until (J = 0) or (Reply = ACK)
                   until (I = 0) or (Reply = ACK);
                   if Reply <> ACK then
                       Raise Error(ErAnyError,
                               'Could not initialize the Diablo.', fatal);
                   while IOCRead(RS232In, Reply) = IOEIOC do ;

                   RSendCh(ESC);
                   RSendCh(CR);             { power-up reset }
                   RSendCh('P');            { also sets the top of form }

                   RSendCh(ESC);
                   RSendCh('2');            { clear all tabs }
                   for I := 1 to Shift do
                       RSendCh(' ');
                   for i := Shift to printer.colwidth do begin
                       if (i mod Tab) = 0 then begin
                           RSendCh(ESC);
                           RSendCh('1')     { set tabstop in this column }
                         end;   { then }
                       RSendCh(' ');
                   end;   { for i }
                   RSendCh(CR)                    
                end;   { diablo }
       {----------------------------------------------}
        hp: begin
               printer.name := 'hp';
               printer.hastabs := TRUE;
               printer.hasformfeed := TRUE;
               printer.colwidth := 79;        { columns }
               printer.port := igpib;

               gpInit;                  { initialize gpib package }
             { Set Perq to Talker, set printer to Listener }
               gpITalkHeListens(addr);
(*      removed since also causes a form-feed which wastes a page
               GSendCh(ESC);            { Send a command sequence }
               GSendCh('E');            { Power up reset }
               gpFlushBuffer;           { Make it happen }
*)
               GSendCh(ESC);
               GSendCh('3');            { clear all tabs }
               for I := 1 to Shift do
                   GSendCh(' ');
               for i := Shift to printer.colwidth do begin
                   if (i mod Tab) = 0 then begin
                       GSendCh(ESC);
                       GSendCh('1')     { set a tabstop in this column }
                     end;   { then }
                   GSendCh(' ');
               end;   { for i }
               GSendCh(CR)
            end;   { hp }
       {----------------------------------------------}
      lineprinter: begin
           printer.name := 'lineprinter';
           printer.hastabs := TRUE;
           printer.hasformfeed := TRUE;
           if Narrow then printer.colwidth := 132 else printer.colwidth := 80;

           RSendCh(Esc);      { short or tall }
           if Short then RSendCh('5') else RSendCh('4');
           RSendCh(Esc);      { narrow or wide }
           if Narrow then RSendCh('7') else RSendCh('6');

           RSendCh(Esc);
           RSendCh('3');      { set the tab stops }
           I := Tab + Shift;
           while I <= 127 do
             begin
               RSendCh(Chr(I));
               I := I + Tab
             end;   { while }
           RSendCh(NUL)
         end;   { lineprinter }
       {----------------------------------------------}
       Ml82a:begin
               printer.name := 'Ml82a';
               printer.hastabs := FALSE;
               printer.hasformfeed := TRUE;
               if Narrow 
                  then printer.colwidth := 132 
                  else printer.colwidth := 80;
               RSendch(CAN); {clear printer buffer}
               RSendch(DC1); {on line the printer}
               RSendCh(Esc);      { short or tall }
               if Short then RSendCh('8') else RSendCh('6');
               if Narrow then RSendCh(GS) else RSendCh(RS);
                       { narrow or wide }
               RSendch(ESC); RSendch('A'); {long line}
               RSendch(CR);    
             end; {Ml82a}
       {----------------------------------------------}
       RicohAptec: begin
               printer.name := 'Ricoh (Aptec RS232)';

               InitAptec;

               Sendch(ESC); Sendch('J');                  { set primary font, }
                                                       { bug in Aptec RS232 ? }

               Sendch(ESC); RSendch(FF);                     { set lines/page }
               if Short then Sendch(chr(94)) else Sendch(chr(71));
               
               i := Tab + Shift + 1;               { set horizontal Tab stops }
               while (i <= printer.colwidth)
                     and
                     (i <= 127)                       { no tabstops after 127 }
               do
               begin
                  Sendch(ESC);Sendch(HT);       { absolute HT to Tab position }
                  RSendch(chr(i));
                  Sendch(ESC);Sendch('1');                     { set Tab posn }
                  i := i + Tab;
               end; { while }
               Sendch(CR);                                { put carriage back }
             
             end; { RicohAptec }
       {------------------------------------------------------}
       otherwise: Raise Error(ErAnyError, 'Unsupported printer.', fatal);
   end;   { case }
end;   { InitPrinter }
 
procedure ClosePrinter;
{--------------------------------------------------------------------
{ Abstract:
{     ClosePrinter is used to close down GPIB printers after 
{     text printing. It does not do a form-feed.
{-------------------------------------------------------------------}
var i,j:integer;

begin
   case printer.port of
      igpib: begin
               if printer.ptype = v80    { get paper out beyond }
               then Sendch(FF);          { dreaded toner wells  }
               
               gpAuxCommand(gpFEOI,gpOn);
               Sendch(NUL);
               gpflushBuffer;
               gpTbltOn
             end;
      otherwise: { do nothing };
   end; { case }

    { Delay to stop reinitialisation of Ricoh before it empties its buffer. }
   if printer.ptype = RicohRS232 then for i:=1 to 10000 do for j:=1 to 80 do;

end.
