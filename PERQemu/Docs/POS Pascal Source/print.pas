{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}

program Print(infile, outfile);

{-----------------------------------------------------------------------------
{
{ Print utility prints files on various printers.
{ John P. Strait  7 Jul 81
{ modified to handle various printers by Ed Frankenberry.
{ Versatec V80 added by Dave MacLaren.
{ Microline 82a added by Dave Anderson
{ Microline 84a,Ricoh 1600 (RS232) 
{   and Ricoh 1600 (GPIB)added by Robbie McLaren.
{ Ricoh 1600 (Ricoh RS232) added by Lynda Hardman.
{   (Previous Ricohs were on Aptec interfaces.)
{  
{
{ Copyright (C) Three Rivers Computer Corporation.
{
{ Abstract:
{      Print is used to send files to various printers. 
{
{      The printers which are supported are: 
{      ICL 3185    Matrix Printer            ( = OKI Microline 84a),
{      ICL 6202/02 Correspondence Printer    ( = Ricoh 1600 (Aptec GPIB ) ),
{      ICL 6202/03 Correspondence Printer    ( = Ricoh 1600 (Ricoh RS232) ),
{      ICL 6203    Electrostatic Printer     ( = Versatec V80 printer/plotter),
{
{      Other printers that can be used by print, but are not supported, are :
{      Ricoh 1600 (Aptec RS232) ( = ICL 6202/01 Correspondence Printer ),
{      OKI Microline 82a,
{      Texas Instruments model 810,
{      Diablo 630 and
{      the Hewlett-Packard 7310A graphics printer (but not the Canon LBP-10).
{
{      Switches are available to set print format and file format.
{      A formfeed is supplied at the beginning of a listing on the ML84 
{      and Versatec but not on the two Ricohs.
{      A form feed character is always supplied at the end of the listing.  
{      Print can be used to set up the printer using the switch options if a
{      nonexistent file name is specified.
{
{      Default settings may be included in the user profile.
{      These are of the form:
{
{               #print /gpib /tall /wide       
{
{      Valid switches are (D = default):
{              /HELP                   type this file
{      Printers
{              /MATRIX                 initialise to use the ICL 3185 
{                                              Matrix Printer
{              /GPIBCORRESPONDENCE     initialise to use the ICL 6202/02 
{                                              Correspondence Printer on GPIB
{              /RS232CORRESPONDENCE    initialise to use the ICL 6202/03 
{                                              Correspondence Printer on RS232
{              /ELECTROSTATIC          initialise to use the ICL 6203 
{                                              Electrostatic Printer.
{              /PLAIN       D          print on RS232 printer with no
{                                              initialisation
{      Unsupported printers
{              /DIABLO                 Diablo 630 daisy printer 
{              /HP                     Hewlett-Packard 7310A 
{              /LINEPRINTER            TI 810 (or similar) lineprinter
{              /ML82A                  Microline 82a printer
{              /RICOH                  Ricoh 1600 correspondence printer
{                                       on (Aptec) RS232
{      Character spacing
{              NOTE:The electrostatic printer has only one print format.
{              /TALL           6 lines per inch
{              /SHORT       D  8 lines per inch
{              /WIDE           10 characters per inch
{              /NARROW      D  12 to 16.5 characters per inch
{                                      ( depends on printer )
{      Print format
{              /TABS=<n>       tab stop every <n> characters
{                                      D=8
{              /SHIFT=<n>      shift the listing <n> spaces to the right 
{                                      D=0
{              /TITLE          print a title on each page
{                                      (title line plus one blank line)
{              /NOTITLE        omit the title
{                                      D=NOTITLE for ".doc" files 
{                                      D=TITLE   for anything else
{      File format
{              /BREAK          print a break page before each file
{              /NOBREAK     D  omit the break page
{              /START=<n>      print starting at page <n> 
{                                      D=1
{              /STOP=<n>       print ending at page <n>
{                                      D=last page of file
{              /SINGLE         print a page then pause
{              /CONTINUOUS  D  print pages continously 
{              /NOPAGE         do not format the file
{              /COPIES=<n>     print <n> copies of the file 
{                                      D=1
{       Interface options
{              /BAUD=<n>       set the RS232 baud rate to <n> 
{                                      D=9600
{              /ADDR=<n>       set the GPIB device address to <n>
{                                      D=4 for /gpib
{                                      D=1 for anything else
{              /RS232B         set the RS232 channel t0 RSB
{                                      D=RSA
{      Command form:
{              print filename {,filename} {/switch}
{-----------------------------------------------------------------------------}

{
{ 16 May 84 V3.3 Rob Briggs
{ Added a facility to allow a choice of RS232 channel A or B.
{ (Note also changes to PrintUtils and Prothers)
{ 18 May 83 V3.2 Sandeep Johar
{ Made some parts of the code specific to 3 rivers requirements.
{ 1> Form length = 11" (the default)
{ 2> Changed the help message
{ 3> added the constant 3RCCPrint for these changes.
{
{ 20 Apr 83 V3.1 LH
{ Changed initialisation of RH margin and handling of A4 FF for RicohRS232 so
{ they now work!
{}

{ 15 Apr 83 V3.0 LH
{ Added CR to every FF so that carriage returns to beginning of line.
{ Added /NOPAGE switch.
{ Note that there are now 30 switches in the program, and this is the maximum
{ number allowed by POS.
{}

{ 14 Apr 83 V3.0 LH
{ Changed logic for expanding horizontal tabs.
{}

{ 28-31 Mar 83 V3.0  LH
{ Incorporated Ricoh Ricoh RS232 initialisation.
{ Added delay at end of RicohRS232 listing to prevent data corruption.
{ Took out extra formfeed before break page.
{ Put in better baud rate switch handling.
{ Changed HELP file and comments above.
{ Tidied this section!
{}

{ 15 Dec 82 ??.?? RMcL
{ Split print+printutils into print, newputils, otherutils.
{}

{ 25 Nov 82 ??.?? LH
{ Added high volume print facility for V80.
{}

{ 15 Nov 82 ??.?? LH
{ Started changes for Z80 rewrite.
{ Deleted procedure nap.
{ Changed "gpib" to "igpib".
{}

{ 27 Oct 82 V2.18 RMcL
{ Changed Ricoh to use A4 length paper 
{ }

{ 27 Oct 82 V2.18 LH
{ Added /SINGLE and /CONTINUOUS switches
{ }

{ 28 Sep 82 V2.18 RMcL
{ Corrected V80 and Ricoh GPIB handling
{ Added extra handlers for ctlc to switch tablet back on
{ }

{ 8 Sep 82 V2.18 RMcL
{ Changed RICOH RS232 handling to use DC1/DC3 protocol and ETX/ACK via new
{ switches /PROTOCOL and /BLOCK for commissioning purposes only
{ }

{ 26 Aug 82 V2.18 RMcL
{ Added ADDR switch for GPIB addresses
{ }

{ 25 Aug 82 V2.18 RMcL
{ Split PRINT into PRINT.PAS and PRINTUTILS.PAS cos it was big
{ and Added Ricoh GPIB handling for ICL
{ }

{ 20 Aug 82 V2.18 RMcL
{ Added Ricoh RS232 handling for ICL
{ }

{ 19 Aug 82 V2.17 RMcL
{ Added ML84a handling for ICL
{ }

{ 9 Jul 82  V2.16 ABD
{ Added delays for slow V80's
{ }

{ 22 Jun 82  V2.15  da
{ Add Ml82a handling for ICL
{ }

{ 18 May 82  V2.14  dam
{ Add V80 handling for ICL
{ }

{ 29 Jan 82  V2.13  ezf
{ GPIB exception now prompts for continuation.
{ }

{ 29 Jan 82  V2.12  ezf
{ Eliminated BaudRate and BaudStr to simply use an integer, Baud.
{ GPIB timeout exception is now recoverable.
{ }

{ 27 Jan 82  V2.11  ezf
{ Long file names are truncated if necessary.  Added handler for
{ GPIB exception if timeout occurs during IO operation.
{ }

{ 25 Jan 82  V2.10  ezf
{ Added /plain switch for vanilla printer.
{ }

{ 22 Jan 82  V2.9  ezf
{ Added the ETX/ACK check when initializing Diablo printer.
{ }

{ 15 Jan 82  V2.8  ezf
{ Added the /Break and /NoBreak switches.
{ }

{ 13 Jan 82  V2.7  ezf
{ Made the Profile reading routines work and other bug fixes.
{ }

{ 12 Jan 82  V2.6  ezf
{ Updated to use new CmdParse (eliminates the RemDelimiters and GetSymbol
{ hackery).  Allows multiple input files.
{ }

{  7 Jan 82  V2.5  ezf
{ Added HP routines and started User's profile processing routines.
{ }

{  6 Jan 82  V2.4  ezf
{ Added Diablo initialization sequence.
{ }

{  4 Jan 82  V2.3  ezf
{ Added /title and /notitle switches.  Uses Clock module.
{ }

{ 31 Dec 81  V2.2  ezf
{ New switches added for copies, baud rate, start and stop pages,
{ title or no title and which printer to use.
{ }

{ 30 Dec 81  V2.1  ezf
{ Added Error handler for exceptional conditions.
{ }

{ 23 Dec 81  V2.0  ezf
{ start of combined Diablo/TI/HP print program.
{ }

{ 21 Jul 81  V1.1  JPS.
{ Add /HELP switch.
{ Add version number.
{ Add UtilProgress.
{ }

{ 7 Jul 81  V1.0  JPS.
{ Start file.
{ }

  imports PrintUtils    from PrintUtils;
  imports PrOthers      from PrOthers;
  imports FileSystem    from FileSystem;
  imports FileUtils     from FileUtils;
  imports UtilProgress  from UtilProgress;
  imports Configuration from Configuration;
  

const PrintVersion = 'V3.2';  { compilation constants: }
      EXTS = ' .Pas .Micro .Cmd .Dfs .Doc .Mss .Prose ';  { file extensions }
var   ThisCh, LastCh : Char;
      BaudStr: longstr;         { baudstr is the baud rate value }
      inargs, outargs: pArgRec;
      switches: pSwitchRec;
      errorstr: longstr;
      anyfiles: boolean;
  

handler Error (errorcode: ErrorType; msg: longstr; degree: errdegree);
{-----------------------------------------------------------------
{ Abstract:
{       Error exception is raised when fatal errors are
{ encountered.  An explanatory message is printed and then
{ the program exits.
{
{ Parameters:
{       Msg is the string to be printed, ErrorCode is one of the
{ standard errors declared in CmdParse and degree is either
{ fatal or recover (causes Error to halt or continue).
{-----------------------------------------------------------------}
(* Const WARNFLG = '* '; *)
begin
   if DEBUG and (degree = fatal) then writeln('[Aborting]');
   if printer.port = igpib then begin
       gpAuxCommand(gpFEOI,gpOn);
       Sendch(NUL);
       Sendch(chr(#004));
       gpFlushBuffer;
       gpTbltOn                         { return to initial state }
    end;   { then }
   QuitProgress;
   StdError(errorcode, msg, (degree = fatal))
 { StdError exits the program if error was fatal }
end;   { Error }


handler GPIBError (softstatus: integer);
{-----------------------------------------------------------------
{
{ Abstract:
{
{    Raised by routines in module GPIB when an error condition
{   arises.  The value of softstatus indicates which error has
{   occurred.
{
{-----------------------------------------------------------------}
begin
   Raise Error(ErAnyError,
               Concat('Printer error: ', IOErrString(softstatus)), fatal)
end;   { GPIBTimeOut }

handler CtlC;
begin
   if printer.port = igpib then gpTbltOn;
   Exit(Print);
end;

handler CtlCAbort;
begin
   if printer.port = igpib then gpTbltOn;
   CtrlCPending := false;
   Exit(Print);
end;

handler CtlShftC;
begin
   if printer.port = igpib then gpTbltOn;
   CtrlCPending := false;
   Exit(Print);
end;


handler BadBaudRate;
{-----------------------------------------------------------------
{
{ Abstract:
{
{    Raised by SetBaud when given a bogus baud rate.  We set the baud rate to
{   DFLTBAUD (which had better be non-bogus) and then call SetBaud again
{   after informing the user of the error.
{
{ Side-Effects:
{   Changes global BaudStr to the default value when an invalid one
{   was supplied.  Then calls SetBaud() with this new value.
{-----------------------------------------------------------------}
begin
  writeln(ERRFLG, BaudStr, ' is not a valid baud rate.',
          '  Using ', DFLTBAUD, ' instead.');
  BaudStr := IntToStr(DFLTBAUD);
  SetBaud(BaudStr, True)        { allow replies from printer port }
end;    { BadBaudRate }

procedure DoPrint (var printer: printrtyp);
const SHORTBOT = 80;            { max. lines per page, narrow spacing }
      TALLBOT = 60;             { number of lines per page, wide spacing }

var
    FId: FileId;
    Blocks, Bits: Integer;      { the size of the current file }
    Buffer: pDirBlk;
    title, break,RS232B: boolean;
    Baud, Reqcopies, Start, Stop, bottom: Integer;
    JumpLines: Integer;



    procedure DoBreak (fn: PathName);
    {--------------------------------------------------------------------
    { Abstract:
    {    Prints out a break page preceding the listing.
    {--------------------------------------------------------------------}
    const FLAGCH = '$';
              
        procedure banner (**);
        {--------------------------------------------------------------------
        { Abstract:
        {    Makes rows of FLAGCH along the left and right edges of the paper.
        {    These LINES are set off with two blank lines preceeding and
        {    following the rows.
        {
        { Side-Effects:
        {    Uses the short variable of DoPrint to print on the first
        {    and last lines of the break page.
        {--------------------------------------------------------------------}
        const SHORTLINES = 18;  { empirical constants so break page fits }
              TALLLINES = 10;   { on the page for both tall and short chars }
              V80LINES = 17;
              COLS = 10;
        var junk: longstr;
            i, lines: integer;
        begin
           junk := '';
           if short then lines := SHORTLINES else lines := TALLLINES;
           if printer.ptype = v80      { Versatec page smaller }
           then lines := V80LINES;     { than normal paper     }
           Adjust(junk, Printer.colwidth);
           for i := 1 to length(junk) do
               junk[i] := ' ';         { blank line to start }
           for i := Printer.colwidth downto Printer.colwidth - COLS + 1 do
               junk[i] := FLAGCH;
           for i := 1 to COLS do
               junk[i] := FLAGCH;
         { now junk contains the pattern line flush on both sides }
           SendLine('', 0);
           SendLine('', 0);
           for i := 1 to lines do
               SendLine(junk, 0);
           SendLine('', 0);
           SendLine('', 0)
        end;   { banner }

        procedure ShowSwitches (filenm: PathName);
        {--------------------------------------------------------------------
        { Abstract:
        {    ShowSwitches lists the settings of all switches as part of the
        {    break page information.
        {--------------------------------------------------------------------}
        const MARGIN = 30;
        var dummy: longstr;
        begin
           dummy := Concat('        ', Concat('File: ', filenm));
         { CurUserName is declared in System }
           SendLine(Concat(Concat('User: ', CurUserName), dummy), MARGIN - 15);
           SendLine('', 0);
           SendLine('', 0);
           SendLine(Concat('Printer  : ', Printer.name), MARGIN);
           dummy := 'Width    : ';
           if narrow then dummy := Concat(dummy, 'Narrow')
               else dummy := Concat(dummy, 'Wide');
           SendLine(dummy, MARGIN);
           dummy := 'Height   : ';
           if short then dummy := Concat(dummy, 'Short')
               else dummy := Concat(dummy, 'Tall');
           SendLine(dummy, MARGIN);
           dummy := 'Title    : ';
           if title then dummy := Concat(dummy, 'Yes')
               else dummy := Concat(dummy, 'No');
           SendLine(dummy, MARGIN);
           dummy := 'Single   : ';
           if single then dummy := Concat(dummy, 'Yes')
               else dummy := Concat(dummy, 'No');
           SendLine(dummy, MARGIN);
           dummy := 'Paged    : ';
           if Paged then dummy := Concat(dummy, 'Yes')
               else dummy := Concat(dummy, 'No');
           SendLine(dummy, MARGIN);
           SendLine(Concat('Start    : ', IntToStr(start)), MARGIN);
           dummy := 'Stop     : ';
           if stop = MAXINT then dummy := Concat(dummy, 'end')
               else dummy := Concat(dummy, IntToStr(stop));
           SendLine(dummy, MARGIN);
           SendLine(Concat('Shift    : ', IntToStr(shift)), MARGIN);
           SendLine(Concat('Copies   : ', IntToStr(Reqcopies)), MARGIN);
           SendLine(Concat('Tabstops : ', IntToStr(tab)), MARGIN)
        end;   { ShowSwitches }


        procedure blot (**);
        {--------------------------------------------------------------------
        { Abstract:
        {    Prints a line of FLAGCHs across the full width of the paper.
        {
        { Side-Effects:
        {    Uses the Printer variable to print a complete line.
        {--------------------------------------------------------------------}
        var i: integer;
            line: longstr;
        begin
           line := '';
           Adjust(line, Printer.colwidth);
           for i := 1 to Printer.colwidth do
              line[i] := FLAGCH;
           SendLine(line, 0);
        end;   { blot }

    begin   { DoBreak }
       if printing
       then begin
            if Debug then writeln('[Printing break page...]');
            if single  
            then begin
                 write(PAGEMESS);
                 readln
                 end;
            Blot(**);
            Blot(**);
            Head(fn, 0, 0);                  { identify the file and date }
            banner(**);
            ShowSwitches(fn);
            banner(**);
            Head(fn, 0, 0);
            Blot(**);
            Blot(**);
            if ( single and (printer.port = igpib) ) then gpFlushBuffer;
            SendCh(FF);
            end;
    end;   { DoBreak }

    Function NextCh (var ch: char; var ThisBlock, ThisByte: integer): boolean;
    {---------------------------------------------
    { Abstract:
    {    Gives the next character in the buffer.
    {
    { Parameters:
    {    ThisBlock, ThisByte are the current position
    {    in the file.  Ch is the value of the next character.
    {
    { Returns:
    {    FALSE when the end of file is reached.
    {
    { Side-Effects:
    {    Refers to globals Blocks, Bits, and JumpLines.
    {    Reads new blocks into Buffer when
    {    it is full and increments the
    {    progress indication.
    {---------------------------------------------}
    var limit: integer;
    begin
       Ch := NUL;
       NextCh := TRUE;

       if ThisBlock = Blocks - 1 then limit := Bits div 8 - 1 else limit := 511;
       if ThisByte <= limit then begin
            Ch := Chr(Buffer^.ByteBuffer[ThisByte]);
            ThisByte := ThisByte + 1
          end else if ThisBlock < Blocks - 1 then begin
                       ThisBlock := ThisBlock + 1;
                       FSBlkRead(FId, ThisBlock, Buffer);
                       Ch := Chr(Buffer^.ByteBuffer[0]);
                       ThisByte := 1;
                       ShowProgress(JumpLines)
                     end else NextCh := FALSE          { at end of file }
    end;   { NextCh }


    function FoundFile (var name: PathName; var FID: FileID; var Blocks,
                        Bits, JumpLines: integer): boolean;
    {---------------------------------------------
    { Abstract:
    {    FoundFile checks whether a file exists
    {    sets the value of the complete file name.
    {
    { Parameters: Name is the string containing the file name,
    {    FId gets the file pointer for this file, Blocks and
    {    Bits get set to the size of the file and JumpLines
    {    gets the amount by which to move the cursor per block.
    {
    { Returns:
    {    True if the file exists, False otherwise.
    {
    { Errors:
    {    If the file cannot be found, or the file name
    {    is empty, an error message is given.
    {---------------------------------------------}
    begin
    FoundFile := FALSE;
    if name = '' then WriteLn(ERRFLG, 'Filename is empty.')
       else begin
          FId := FSExtSearch(FSSysSearchList, EXTS,
                       Name, Blocks, Bits);
          if FId = 0 then
              Raise Error(ErFileNotFound, Name, recover)
             else begin
                    FoundFile := TRUE;
                    FSRemoveDots(Name);
                    if Blocks = 0 then JumpLines := 1
                       else JumpLines := 1024 div Blocks;
                  end;   { else }
        end;   { else }
    end;   { FoundFile }

    procedure DoInputLine (var title,break,short,narrow,single,paged,RS232B: boolean;
                           var copies, start, stop, baud, tab, shift: integer);
    {---------------------------------------------
    { Abstract:
    {    Parses the command line to set switches
    {    and read the list of input files.
    {
    { Side-Effects:
    {    Modifies global inargs, outargs, errorstr,
    {    switches and initializes printer.
    {---------------------------------------------}
    label 99;
    var fn, str: longstr;
        dummy: integer;
        isSwitch, ok, quit: boolean;
        ch: char;
(* "fixed" in Brad's latest CmdParse... *)
        handler PastEOF (filename: PathName);
        {---------------------------------------------
        { Abstract:
        {    Handles the end of file exception
        {    when it is not expected (such as
        {    during ParseCmdLine) by printing
        {    a warning and continuing.
        {---------------------------------------------}
        begin
           writeln;
           writeln(ERRFLG, 'Unexpected end of file.');
           reset(input, 'console:');    { back to console for input }
           goto 99
        end;
(**)
    begin   { DoInputLine }
       str := '';
       errorstr := '';
       ch := NextId(str, isSwitch);        { skip the "Print" }
       if (ch <> ' ') and (ch <> CR)
          then Raise Error(ErIllCharAfter, 'Print', fatal);
       ok := ParseCmdArgs(inargs, outargs, switches, errorstr);
       repeat
        99:
          quit := TRUE;
          dummy := Length(inargs^.name);
          if dummy > 4 then
             title := SubStr(Upper(inargs^.name), dummy - 3, 4) <> '.DOC';

          if NOT ok then StdError(ErAnyError, errorstr, TRUE);
          DoSwitches (switches, title, break, short, narrow, single, paged,
                      RS232B,copies, start, stop, baud, tab, shift, addr);

          if (outargs^.name <> '') or (outargs^.next <> NIL) then
             StdError(ErNoOutFile, 'Print', TRUE);
          if (inargs^.name = '') and (inargs^.next = NIL) then begin
                         write('File(s) to print: ');
                         fn := LastFileName;    { declared in System }
                         FId := FSExtSearch(FSSysSearchList, EXTS,
                                            fn, Blocks, Bits);
                         if fn <> '' then begin
                             write('[', fn, '] ');
                             if eoln(input) then begin
                                 str := fn;
                                 readln(**)
                               end else readln(str);
                          end else readln(str);
                         ok := ParseStringArgs(str, inargs, outargs,
                                               switches, errorstr);
                         quit := FALSE
                       end;   { then }
       until quit
    end;   { DoInputLine }

    procedure PrintIt(**);
    const SPACE = ' ';
    var  I, J, K: Integer;
         FileName: PathName;                           { declared in FileDefs }
         pagenum, copies: integer;
         charnum : integer;                                 { character count }
         charposn : integer;                             { character position }
         Check, LastESC : boolean;

       procedure OutPutChar;
       begin { OutPutChar }
          SendCh(ThisCh);
          if (ThisCh >= SPACE) then charnum := charnum + 1;
                             { Don't count nonprintable chars. Backspaces are }
                             { ignored by POS so don't have to consider them. }
       end;  { OutPutChar }
       
       procedure ExpandTab;
       begin { ExpandTab }
          if Tab > 1
          then
           begin
                              { Save character position, calculate where next }
                               { character is to be, then fill in with spaces }
             charposn := charnum;
             charnum := ((((charnum-Shift-1) div Tab)+1)*Tab)
                        +Shift+1;
             for k:=1 to ( charnum - charposn ) do Sendch(' ');
           end;
        end;  { ExpandTab }

    begin { PrintIt }
       Check := TRUE;
       LastESC := FALSE;
       FileName := inargs^.name;
       Writeln('   ', FileName, ' ==> [', Printer.name, ']');
       copies:=Reqcopies;
         
       repeat
          pagenum := 0;
          linenum := 0;
          charnum := 1;
          I := 0;                                               { first block }
          J := 0;                                           { first character }
          FSBlkRead(FId, 0, Buffer);                    { get the first block }
          if break then DoBreak(FileName)
             else if NextCh(LastCh, I, J)
                     and 
                     (Printer.ptype <> ML82a)
                     and
                     (Printer.ptype <> RicohRS232)   { Have to set up printer }
                     and                           { at TOF at start of batch }
                     (Printer.ptype <> RicohGPIB)   
                     and
                     (Printer.ptype <> ml84)         { FF done in InitPrinter }
                     and
                     (Printer.ptype <> v80)          { FF done in InitPrinter }
                     and
                     (Printer.ptype <> hp)
                  then if (LastCh <> FF) and (Paged)
                       then SendCh(FF);                    { first char a ^L? }
          LoadCurs;
          J := 0;                              { reset to the first character }
          LastCh := CR;                        { Should this really be FF? LH }
          if Debug then write('[', pagenum:1);
      
          while NextCh(ThisCh, I, J) and (pagenum <= stop) do 
           begin
             if linenum = 0 
             then
              begin
                pagenum := pagenum + 1;
                if Debug then write(' ', pagenum:1);
                printing := (pagenum >= start) and (pagenum <= stop);
                if printing and single
                then begin
                     if Printer.port = igpib then gpFlushBuffer;
                     write(PAGEMESS);
                     readln
                     end;
                linenum := 1;
                if printer.ptype = v80 then skip(3);
                if (title) and (Paged)
                then head(FileName, pagenum, shift);
              end;   { then }
      
             if LastCh in [CR, LF, FF]
             then
                if NOT (ThisCh in [CR, LF, FF]) 
                then 
                 begin
                   for K := 1 to Shift do SendCh(' ');
                   charnum := Shift +1;
                 end;

             if (printer.ptype=Ml82a)
             then
              begin
                if (ThisCh>=RS) or (ThisCh in [CR, LF, FF])
                then Sendch(ThisCh); {avoid control chars}
              end
             else
              begin
             { Only want to expand tabs when not sending control information. }
                if (Printer.HasTabs) or (NOT Paged)
                then OutPutChar
                else
                   if (Printer.ptype <> RicohRS232)
                   then 
                    begin
                      if (ThisCh = HT)
                      then ExpandTab
                      else OutputChar;
                    end
                   else 
                      if Check
                      then
                       begin
                         if   LastESC  and   ((ThisCh = FF) or (ThisCh = HT)
                          or (ThisCh = VT) or (ThisCh = RS) or (ThisCh = US))
                         then 
                          begin
                            OutPutChar;
                            Check := FALSE;
                            LastESC := FALSE;
                          end
                         else
                            if (ThisCh = HT)
                            then ExpandTab
                            else 
                             begin
                               if (ThisCh = ESC)
                               then LastESC := TRUE
                               else LastESC := FALSE;
                               OutputChar;
                             end
                       end
                      else
                       begin
                         OutputChar;
                         Check := TRUE;
                       end
              end;
             
             {--------------------------------------------------}
             {    check for LF done in Sendch now !             }
             {--------------------------------------------------}
             {   if ThisCh = LF then linenum := linenum + 1;    }
             {--------------------------------------------------}
      
             if (linenum > bottom) and (ThisCh <> FF) and (Paged)
             then SendCh(FF);
             LastCh := ThisCh;
          end;   { while }
      
          ShowProgress(JumpLines);                         { final indication }
          if (LastCh <> FF) and (Paged)
          then SendCh(FF);
       
          if Debug then Writeln(']');
          if Debug then begin
             if start > 1 then pagenum := pagenum - start + 1;
             if break then pagenum := pagenum + 1;
             writeln('[', pagenum:1, ' * ', copies:1, ' pages printed.]')
           end;   { then }
          copies := copies - 1;                 { one less to do }
     until copies = 0;
     QuitProgress
   end;   { PrintIt }


begin   { DoPrint }

   { set the default values: }

    Linenum         := 0;
    Reqcopies       := 1;                            { no of copies requested }
    Shift           := 0;                                 { flush left margin }
    Start           := 1;                                  { print every page }
    Tab             := 8;
    Baud            := DFLTBAUD;
    Short           := DEFSHORT;
    Narrow          := DEFNARROW;
    Printer.ptype   := Plain;
    Printer.port    := ChannelA;
    Stop            := MAXINT;
    Addr            := GPIBCODE;

    Anyfiles        := FALSE;                          { no files printed yet }
    Break           := FALSE;
    Paged           := TRUE;
    Printing        := TRUE;
    Single          := FALSE;
    Title           := TRUE;
    RS232B          := FALSE;
    
    
    FSAddToTitleLine(Concat('Print ',
                 Concat(PrintVersion, '.  Type "Print /help" for help.')));

 {$IFC UsingProfile THEN }
    ReadUserProfile(title, break, short, narrow, single, paged,RS232B, reqcopies,
                    start, stop, baud, tab, shift, addr, inargs, outargs,
                    errorstr, switches);
 {$ENDC}
    DoInputLine(title, break, short, narrow, single, paged,RS232B, reqcopies, start,
                stop, baud, tab, shift);

    BaudStr := IntToStr(Baud);
    SetBaud(BaudStr, True);             { allows RS232 input from printer }
    if    RS232B
          then  if    Cf_RS232Ports > 1
                then
                      begin
                      writeln('RSB Selected.');
                      SetRS232Port(BaudStr,RSB);
                      printer.port := ChannelB ;
                      
                      end
                else  begin
                      writeln;
                      writeln;
                      writeln('      Only one RS232 Port is availalble on this machine');
                      writeln;
                      writeln;
                      exit(print);
                      end 
    else  SetRS232Port(BaudStr,RSA) ;     InitPrinter;
    If short
    then bottom := SHORTBOT
    else bottom := TALLBOT;
    if (Printer.Ptype = hp) or (Printer.Ptype = Diablo)
    then bottom := SHORTBOT;
    if Printer.Ptype = v80 then bottom := TALLBOT;

    New(0, 256, Buffer);                { allocate buffer space }
    while inargs <> NIL do begin
       if FoundFile(inargs^.name, FID, Blocks, Bits, JumpLines) then 
          begin
            PrintIt(**);
            anyfiles := TRUE;
          end;
       inargs := inargs^.next
     end;   { while }
if anyfiles=TRUE then 
     begin
       if Printer.ptype=Ml82a then
          begin
             Sendch(FF); {end of batch..do form feed now}
             Sendch(LF); Sendch(LF); Sendch(LF);
             Sendch(LF); Sendch(LF); Sendch(LF);
          end;
     end;
ClosePrinter;
end;   { DoPrint }

  
begin { Print }
   DoPrint(Printer);
   if DEBUG then writeln('[Exit]');
end { Print }.

