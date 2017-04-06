{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Stream;


{-----------------------------------------------------------------------------
{
{       Stream - Perq Pascal stream package.
{       John Strait              ca. Jan 80.
{       Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1982, 1983
{
{
{ Abstract:
{       This module implements the low-level Pascal I/O.  It is not
{       intended for use directly by user programs, but rather the
{       compiler generates calls to these routines when a Reset,
{       Rewrite, Get, or Put is encountered.  Higher-level character
{       I/O functions (Read and Write) are implemented by the two
{       modules Reader and Writer.
{
{       In this module, the term "file buffer variable" refers to F^ for
{       a file variable F.
{
{-----------------------------------------------------------------------------}

{$Version V1.26 for POS}
{-----------------------------------------------------------------------------
{     Change log:

      V1.26  SSJ  27 Apr 83 Stream was passing keypad character up as control
                            characters. Fixed the problem.
                            
      V1.25  Jps  01 Apr 83 In the tradition of other special characters
                            such as ^S, ^Q, ^D, etc., ignore Control-
                            Shift-P since it now makes hardcopy. P.S.
                            Sandeep forgot to say that he changed Stream
                            to use the untranslated keyboard again.

      V1.24  SSJ  29 Mar 83 Changed FullLine again!  This was done as the
                            stream package was generating characters greater
                            than #200 and this was determined to be bad. Also
                            some keys behaved different.

      V1.23  SSJ   1 Mar 83 Changed the way stream interfaces with IOCread.
                            We now use transkey in stream hence the keypad 
                            works. The special function keys are the only keys
                            which stream passes up to the next layer as having
                            ords greater than #200, they are echoed as some
                            arbitrary character depending on the character
                            set, also implemented use of Scroll and NoScroll
                            key(stream throws them away). On the old keyboard
                            no difference should be apparent.
                            
      V1.22  RSR  21 Feb 83 Changed RS232 setup for F2 to use UnitIO to
                            configure for 8 bit characters.

      V1.21  BAM  14 May 82  Add procedures and mechanism to allow transcript
                              of all characters written to the screen.

      V1.20  BAM  27 Jan 82  Turn RSX back on.

      V1.19a SLB  26 Jan 82  Add exception definition NotReal.

      V1.18 SLB  07 Jan 82  Add exception definition RealWriteError.

      V1.17 SLB  30 Dec 81  Add an exception definition LargeReal, and change
                            exception definition SmallNumber to SmallReal.
                                  
      V1.16 BAM  17 Dec 81  Turn off RSX switch.
                            Make regular OOPs work.
      
      V1.15 SLB  24 Nov 81  Add an exception definition SmallNumber.
      
      V1.14 JPS  25 Sep 81  Fix bug in control-BS for keyboard input.
      
      V1.13 JPS  27 Jul 81  1) Export the keyboard buffer variables.
                            2) Change line editing to be like the Editor.
                            3) Make Readln work for files with no Eol before
                               the Eof.

      V1.12 BAM   6 Jul 81  Check for IOECBF as well as IOEIOB in RS: output

      V1.11 BAM   1 Jul 81  Changed IO Screen output device name to ScreenOut

      V1.10 JPS  24 Jun 81  1) Remove reverse video control character code.
                            2) Fixed bug in FullLn.
                            3) Made EofCh (^Z) echo at Console.
      
      V1.9  JPS   4 Jun 81  Add Virgil headers and comments.
      
      V1.8  JPS  22 May 81  Make StreamKeyBoardReset accept a Text parameter.
      
      V1.7  BAM  19 May 81  StreamKeyBoardReset turn cursor off.
      
      V1.6  JPS  12 May 81  Use exceptions instead of StreamError.
                            Fix bug when EofCh is typed at console.
                            Use new control-C exceptions.
      
      V1.5  BAM  19 Mar 81  PERQ_String
      
      V1.4  BAM  17 Mar 81  1) Change to use FSIsFSDev on open
                            2) Change to use new Screen ClearChar routine
      
      V1.3  JPS  16 Feb 81  1) Implement FullLn predicate which tells you if
                               there is a full line in the keyboard input
                               buffer.
                            2) Fix bug in reporting "file opened to $ is not
                               a text file" error.
                            3) Import the new Loader module.
                            4) Move $R- to private part.
                            5) Shrink the error routine.
      
      V1.2  DCF  11 Feb 81  Conforms to new PString; compatable with
                            new System and Compiler.
                            
                                  
      V1.1  JPS   6 Nov 80  1) Add RSX: as an output device, RSX:FileName
                               generates
                                        ^QPip FileName=TI:
                               For each character:
                                 a) Flush RS232 input buffer.
                                 b) Send RS232 character.
                                 c) Wait for RS232 echo--in the case of
                                    sending a carriage return, wait for
                                    a line feed.
                               Send ^Z at end of file.
                            2) Add RSX: as an input device, RSX:FileName
                               generates
                                        ^QPip TI:=FileName
                                        ^S
                               For each line of RS232 input:
                                 a) Send a ^Q.
                                 b) Read from RS232 until end of line.
                                 c) Send a ^S.
                               End of file is indicated by a '>' after an
                               end of line, and the ^S is not sent after
                               the end of file is received.
                            3) In Reset and Rewrite allow filenames of the
                               form <device name>:.  This allows dumb
                               programs to unconditionally concatenate
                               extensions onto filenames typed by the
                               user (e.g. Console:.Map, RSX:.List).
                            4) Clean up control character processing some.
                            5) Make RSX: optional at compile time.  See
                               the CompileRSX constant.
-----------------------------------------------------------------------------}

exports


imports FileDefs from FileDefs;


const StreamVersion = '1.24';

      IdentLength = 8;          { significant characters in an identifier }

 
type pStreamBuffer = ^StreamBuffer;

     StreamBuffer = record case integer of  { element size: }
      0: (W: array[0..255] of integer);       { 1 or more words, or > 8 bits }
      1: (B1: packed array[0..0] of 0..1);    { 1 bit }
      2: (B2: packed array[0..0] of 0..3);    { 2 bits }
      3: (B3: packed array[0..0] of 0..7);    { 3 bits }
      4: (B4: packed array[0..0] of 0..15);   { 4 bits }
      5: (B5: packed array[0..0] of 0..31);   { 5 bits }
      6: (B6: packed array[0..0] of 0..63);   { 6 bits }
      7: (B7: packed array[0..0] of 0..127);  { 7 bits }
      8: (B8: packed array[0..0] of 0..255);  { 8 bits }
      9: (C: packed array[0..255] of char);   { for character structured }
      end;
     

     ControlChar = 0..#37;                { ordinal of an ASCII control
                                            character }

     FileKind = (BlockStructured, CharacterStructured);

     FileType = { file of Thing }
      packed record
       Flag: packed record case integer of
        0: (CharReady  : boolean;           { character is in file window }
            FEoln      : boolean;           { end of line flag }
            FEof       : boolean;           { end of file }
            FNotReset  : boolean;           { false if a Reset has been
                                              performed on this file }
            FNotOpen   : boolean;           { false if file is open }
            FNotRewrite: boolean;           { set false if a Rewrite has been
                                              performed on this file }
            FExternal  : boolean;           { not used - will be permanent/temp
                                              file flag }
            FBusy      : boolean;           { IO is in progress }
            FKind      : FileKind);
        1: (skip1     : 0..3;
            ReadError : 0..7);
        2: (skip2     : 0..15;
            WriteError: 0..3)
        end;
       EolCh, EofCh, EraseCh, NoiseCh: ControlChar; {self explanatory}
       OmitCh        : set of ControlChar;
       FileNum       : integer;      { POS file number }
       Index         : integer;      { current word in buffer for un-packed
                                       files, current element for packed
                                       files }
       Length        : integer;      { length of buffer in words for un-
                                       packed files, in elements for packed
                                       files }
       BlockNumber   : integer;      { next logical block number }
       Buffer        : pStreamBuffer;{ I/O buffer }
       LengthInBlocks: integer;      { file length in blocks }
       LastBlockLength:integer;      { last block length in bits }
       SizeInWords   : integer;      { element size in words, 0 means
                                       packed file }
       SizeInBits    : 0..16;        { element size in bits for packed
                                       files }
       ElsPerWord    : 0..16;         { elements per word for packed files }
       Element: { Thing } record case integer of  {The File window}
        1: (C: char);
        2: (W: array[0..0] of integer)
        end
       end;



     ChArray = packed array[1..1] of char; {For read/write character array}


     Identifier = string[IdentLength];
     IdentTable = array[0..1] of Identifier;
     
     
     
     
var StreamSegment: integer;     { Segment buffer for I/O buffers }
    KeyBuffer: packed array[0..255] of char;
    KeyNext, KeyLength: integer;


     
     
 procedure StreamInit( var F: FileType; WordSize, BitSize: integer;
                       CharFile: boolean );
 procedure StreamOpen( var F: FileType; var Name: PathName;
                       WordSize, BitSize: integer; CharFile: boolean;
                       OpenWrite: boolean );
 procedure StreamClose( var F: FileType );
 procedure GetB( var F: Filetype );
 procedure PutB( var F: Filetype );
 procedure GetC( var F: Filetype );
 procedure PutC( var F: FileType );
 procedure PReadln( var F: Filetype );
 procedure PWriteln( var F: Filetype );
 procedure InitStream;
 function StreamName( var F: FileType ): PathName;
 function FullLn( var F: Text ): Boolean;
 procedure StreamKeyBoardReset( var F: Text );


 exception ResetError( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when unable to reset a file--usually file not found but
{       also could be ill-formatted name or bad device name.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception RewriteError( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when unable to rewrite a file--usually file unknown device
{       or partition but also could be ill-formatted name or bad device name.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception NotTextFile( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to open a non-text file to a
{       character-structured device.
{
{ Parameters:
{       FileName - name of the device.
{
{-----------------------------------------------------------------------------}


 exception NotOpen;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to use a file which is not open.
{
{-----------------------------------------------------------------------------}


 exception NotReset( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to read a file which is open but
{       has not been reset.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception NotRewrite( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to write a file which is open but
{       has not been
{       rewritten.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception PastEof( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to read past the end of the file.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception UnitIOError( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when IOCRead or IOCWrite returns an error status.
{
{ Parameters:
{       FileName - name of the device.
{
{-----------------------------------------------------------------------------}


 exception TimeOutError( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a device times out.
{
{ Parameters:
{       FileName - name of the device.
{
{-----------------------------------------------------------------------------}


 exception UndfDevice;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to reference a file which is open
{       to a character-structured device, but the device number is bad.
{       In the current system (lacking automatic initialization of file
{       variables), this may be caused by referencing a file which has
{       never been opened.
{
{-----------------------------------------------------------------------------}


 exception NotIdentifier( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an identifier is expected on a file, but something
{       else is encountered.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception NotBoolean( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a boolean is expected on a file, but something
{       else is encountered.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception BadIdTable( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised by ReadIdentifier when the identifier table is bad.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception IdNotUnique( FileName: PathName; Id: Identifier );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when non-unique identifier is read.
{
{ Parameters:
{       FileName - name of the file or device.
{       Id - the identifier which was read.
{
{-----------------------------------------------------------------------------}


 exception IdNotDefined( FileName: PathName; Id: Identifier );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an undefined identifier is read.
{
{ Parameters:
{       FileName - name of the file or device.
{       Id - the identifier which was read.
{
{-----------------------------------------------------------------------------}


 exception NotNumber( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a number is expected on a file, but something
{       else is encountered.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception LargeNumber( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a number is read from a file, but it is too large.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception SmallReal( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a real number is read from a file, but it is too small.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception BadBase( FileName: PathName; Base: Integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to read a number with a numeric base
{       that is not in the range 2..36.
{
{ Parameters:
{       FileName - name of the file or device.
{       Base - numeric base (which is not in the range 2..36).
{
{-----------------------------------------------------------------------------}


 exception LargeReal( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a real number is read from a file, but it is too large.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception RealWriteError( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when an attempt is made to write a real number which is 
{       invalid.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


 exception NotReal( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when a real number is expected on a file, but something
{       else is encountered.
{
{ Parameters:
{       FileName - name of the file or device.
{
{-----------------------------------------------------------------------------}


Procedure StartTranscript(fileName: PathName; append: boolean);
Procedure StopTranscript;
Procedure TransChar(c: Char);

Exception TransError(kind: String);
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Raised when startTranscript called but a transcript is already open
{       or if StopTranscript is called and no transcript is active.  Also,
{       if StartTranscript and file cannot be created.
{
{ Parameters:
{       Kind - a string describing the error.
{
{-----------------------------------------------------------------------------}

private

{$R-  shut off range checks }
Imports IOKeyBoard From IOKeyBoard;



const CompileRSX = TRUE; 
         { To add RSX: as a device, use the following steps.
              1) Set CompileRSX = true.
              2) Re-compile Stream.
              3) Write a system boot file using MakeBoot.
         }
         
const 
      { untranslated keys: }

      Nul   = Chr(#000);
      Bel   = Chr(#007);
      Tab   = Chr(#011);
      LF    = Chr(#012);
      FF    = Chr(#014);
      CR    = Chr(#015);
      BS    = Chr(#010);         { (back-space) }
      BW    = Chr(#210);         { control-BS (back-word) }
      BL    = Chr(#225);         { control-OOPS (back-line) }
      OOPS  = Chr(#025);

      { translated keys: }

(*   The first three are in ioKeyboard

      CtrlC = Chr(#003);
      CtrlQ = Chr(#021);
      CtrlS = Chr(#023);          *)

      CtrlD = Chr(#004);
      CtrlH = Chr(#010);
      CtrlP = Chr(#020);
      CtrlU = Chr(#025);
      CtrlW = Chr(#027);
      CtrlZ = Chr(#032);
      
      NoScroll = #14;
      CtrlNoScroll = #16;

      RSTimeOut = -1;
      
      { Character structured device numbers }
      
      Console = 1;
      RS = 2;
      RSX = 3;
      



imports IO_Unit from IO_Unit;
imports IOErrors from IOErrors;
imports Screen from Screen;
imports Memory from Memory;
imports FileSystem from FileSystem;
imports Perq_String from Perq_String;
imports System from System;
imports Raster from Raster;
imports DiskIO from DiskIO;
imports AllocDisk from AllocDisk;
      
     
     
     
var OutputF: text;
    Status: integer;
    FileName: string;
    CRTyped: Boolean;
    
    TransInUse: Boolean;
    TransFile: FileID;
    TransBlock, TransByte: Integer;
    TransSegment: integer;
    TransPtr: pDirBlk;
    

procedure StreamInit( var F: FileType; WordSize, BitSize: integer;
                      CharFile: boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Initializes, but does not open, the file variable F.  Automatically
{       called upon entry to the block in which the file is declared.  (To
{       be written when the compiler generates calls to it.)
{
{ Parameters:
{       F - the file variable to be initialized.
{       WordSize and BitSize are the size of an  element of the file.
{       CharFile - determines whether or not the file is of characters.
{
{-----------------------------------------------------------------------------}

begin { StreamInit }
end { StreamInit };

procedure StreamClose( var F: FileType );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Closes the file variable F.
{
{ Parameters:
{       F - the file variable to be closed.
{
{-----------------------------------------------------------------------------}

begin { StreamClose }
 with F, Flag do
  { if not FNotOpen then }
   begin
    if (FKind = BlockStructured) and not FNotRewrite then
     begin
      if Index <> 0 then
       begin FSBlkWrite(FileNum,BlockNumber,Recast(Buffer,pDirBlk));
        BlockNumber := BlockNumber + 1
       end
      else Index := Length;
      if SizeInWords = 0 then
       FSClose(FileNum,BlockNumber,
               (Index div ElsPerWord) * 16 +
               (Index mod ElsPerWord) * SizeInBits)
      else FSClose(FileNum,BlockNumber,Index * 16)
     end
    else
     if not FNotRewrite then
      if EofCh <> Ord(Nul) then
       begin Element.C := Chr(EofCh); PutC(F) end;
    if Buffer <> nil then Dispose(Buffer);
    CharReady := false;
    FEof := true;
    FNotOpen := true;
    FNotReset := true;
    FNotRewrite := true;
    FExternal := true
   end
end { StreamClose };

procedure StreamOpen( var F: FileType; var Name: PathName;
                      WordSize, BitSize: integer; CharFile: boolean;
                      OpenWrite: boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Opens the file variable F.  This procedure corresponds to both
{       Reset and Rewrite.
{
{ Parameters:
{       F - the file variable to be opened.
{       Name - the file name.
{       WordSize - number of words in an element of the file (0 indicates
{                  a packed file).
{       BitSize - number of bits in an element of the file (for packed files).
{       CharFile - true if the file is a character file.
{       OpenWrite - true if the file is to be opened for writing (otherwise
{                   it is opened for reading).
{
{ Errors:
{       ResetError  if unable to reset the file.
{       RewriteError  if unable to rewrite the file.
{       NotATextFile  if an attempt is made to open a non-text file to a
{                     character structured device.
{
{-----------------------------------------------------------------------------}

var i: integer;
    UpperName, UpperDevice: String;

 procedure EnableRS( Unit : UnitRng);
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Called by OpenRs and OpenRSX to enable receiver on RS232
{
{-----------------------------------------------------------------------------}
  Var
    Args : pRS232Stat;
    Status : IOStatPtr;
    LogAdr : Double;
  begin
  New(Args);
  New(Status);
  Args^.Reg[1].ID := 3;                 { Write Register 3 }
  Args^.Reg[1].RSRcvEnable := True;     { Enable the receiver }
  Args^.Reg[1].AutoEnables := True;     { Use CTS and DCD to enable Tx and Rx }
  Args^.Reg[1].RSRcvBits := RS_8;       { Receive 8 Bit characters }
  Args^.Reg[2].ID := 4;                 { Write Register 4 }
  Args^.Reg[2].RSParity := RS_NoParity; { Set no parity }
  Args^.Reg[2].RSStopBits := RS_St1;    { Set 1 stop bit }
  Args^.Reg[2].ClockRate := R_X16;      { Set clock rate to x16 }
  Args^.Reg[3].ID := 5;                 { Write Register 5 }
  Args^.Reg[3].RTS := true;             { Enable request to send }
  Args^.Reg[3].TxEnable := True;        { Enable transmitter }
  Args^.Reg[3].SendBreak := False;      { Do not send break }
  Args^.Reg[3].RSXmitBIts := RS_Send8;  { Transmit 8 bit characters }
  Args^.Reg[3].DTR := True;             { Set data terminal ready }

  UnitIO(Unit, RECAST(Args, IOBufPtr), IOWriteRegs, 6, LogAdr, Nil, Status);
  Dispose(Status);
  Dispose(Args);
  end;

 procedure OpenBlockStructured;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Called by StreamOpen to open files on block-structured devices.
{
{-----------------------------------------------------------------------------}

  handler FSNotFnd( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Catches the file system's file-not-found error.  Raises ResetError.
{
{-----------------------------------------------------------------------------}

  begin { FSNotFnd }
   raise ResetError(Name)
  end { FSNotFnd };

  handler FSBadName( FileName: PathName );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Catches the file system's bad-file-name error.  Raises RewriteError.
{
{-----------------------------------------------------------------------------}

  begin { FSBadName }
   raise RewriteError(Name)
  end { FSBadName };

 begin { OpenBlockStructured }
  with F do
   begin
    EolCh := Ord(LF);
    EofCh := Ord(Nul);
    EraseCh := Ord(Nul);
    NoiseCh := Ord(CR);
    OmitCh := [NoiseCh];
    Flag.FKind := BlockStructured;
    if StreamSegment = 0 then CreateSegment(StreamSegment,1,1,256);
    New(StreamSegment,256,Buffer);
    BlockNumber := 0;
    Length := 256 * ElsPerWord;
    if OpenWrite then
     begin FileNum := FSEnter(Name);
      Index := 0
     end
    else
     begin FileNum := FSLookUp(Name,LengthInBlocks,LastBlockLength);
      Index := Length - 1;
      if not CharFile then GetB(F)
     end
   end
 end { OpenBlockStructured };

 procedure OpenConsole;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Called by StreamOpen to open a file to the console.
{
{-----------------------------------------------------------------------------}

 begin { OpenConsole }
  with F do
   begin
    EolCh := Ord(CR);
    if OpenWrite then EofCh := Ord(Nul) else EofCh := Ord(CtrlZ);
    EraseCh := Ord(Nul);
    NoiseCh := Ord(LF);
    OmitCh := [NoiseCh, Ord(CtrlD), Ord(CtrlS), Ord(CtrlP), Ord(CtrlQ)]; 
   end
 end { OpenConsole };

 procedure OpenRS;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Called by StreamOpen to open a file to the RS232 port.
{
{-----------------------------------------------------------------------------}

 begin { OpenRS }
  with F do
   if OpenWrite then
    begin
     EolCh := Ord(LF);
     EofCh := Ord(Nul);
     EraseCh := Ord(Nul);
     NoiseCh := Ord(CR);
     OmitCh := []
    end
   else
    begin
     EolCh := Ord(Nul);
     EofCh := Ord(Nul);
     EraseCh := Ord(Nul);
     NoiseCh := Ord(Nul);
     OmitCh := [];

     EnableRS(RS232In);
    end
 end { OpenRS };

 procedure OpenRSX;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Called by StreamOpen to open a file to the RSX11-M operating system
{       through the RS232 port.
{
{-----------------------------------------------------------------------------}

 var Command: String;

  procedure SendCommand;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Sends a command to RSX11-M through the RS232 port.
{
{-----------------------------------------------------------------------------}

  var I: integer;
  begin { SendCommand }
   if CompileRSX then
    begin F.Element.C := CtrlQ; PutC(F);
     for I := 1 to Length(Command) do
      begin F.Element.C := Command[I]; PutC(F) end;
     F.Element.C := CR; PutC(F)
    end
  end { SendCommand };

 begin { OpenRSX }
  if CompileRSX then
   with F do
    begin
     EnableRS(RS232In);
     EolCh := Ord(CR);
     EofCh := Ord(CtrlZ);
     EraseCh := Ord(Nul);
     if OpenWrite then
      begin
       NoiseCh := Ord(Nul);
       OmitCh := [];
        Command:=Concat('Pip ', UpperName);
       Command:=Concat(Command, '=TI:');
       SendCommand
      end
     else
      begin
       NoiseCh := Ord(LF);
       OmitCh := [NoiseCh];
       Command:=Concat('Pip TI:=', UpperName);
       Flag.FNotReset := true;     { temporarily make this an output file }
       Flag.FNotRewrite := false;
       SendCommand;
       Element.C := CtrlS; PutC(F);
       Flag.FNotReset := false;    { make it an input file again }
       Flag.FNotRewrite := true;
       New(Buffer);  { character buffer }
       Length := 1;
       Index := 0;
       Buffer^.C[0] := CR
      end
    end
 end { OpenRSX };

begin { StreamOpen }
 { StreamClose(F); }
 with F, Flag do
  begin
   FileName := Name;
   CharReady := OpenWrite;
   FEoln := true;
   FEof := OpenWrite;
   FNotReset := OpenWrite;
   FNotOpen := false;
   FNotRewrite := not OpenWrite;
   { FExternal := Name <> ''; }
   FBusy := false;
   Buffer := nil;
   SizeInWords := WordSize;
   if SizeInWords = 0 then
    begin SizeInBits := BitSize; ElsPerWord := 16 div SizeInBits end
   else
    begin SizeInBits := 16; ElsPerWord := 1 end;
   UpperName := Name;
   for i := 1 to Ord(UpperName[0]) do
    if UpperName[i] in ['a'..'z'] then
     UpperName[i] := Chr(Ord(UpperName[i]) - Ord('a') + Ord('A'));
   if UpperName = '' then UpperName := 'CONSOLE:';
   i := FSIsFSDev(UpperName, UpperDevice);  {i=0 if is FS else i=Pos(':')}
   if i=0 then OpenBlockStructured
   else
    begin
     Delete(UpperName,1,i);
     FKind := CharacterStructured;
     if (UpperDevice = 'CONSOLE') or (UpperDevice = 'CON') then
      FileNum := Console
     else
      if UpperDevice = 'RS' then FileNum := RS
      else
       if CompileRSX and (UpperDevice = 'RSX') then FileNum := RSX
       else
        if OpenWrite then raise RewriteError(Name)
        else raise ResetError(Name);
     if not CharFile then raise NotTextFile(Name);
     case FileNum of
      Console: OpenConsole;
      RS:      OpenRS;
      RSX:     OpenRSX
      end
    end
  end
end { StreamOpen };

procedure GetB( var F: Filetype );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Advances to the next element of a block-structured file and gets
{       it into the file buffer variable.
{
{ Parameters:
{       F - the file to be advanced.
{
{ Errors:
{       NotOpen  if F is not open.
{       NotReset  if F has not been reset.
{       PastEof  if an attempt is made to read F past Eof.
{
{-----------------------------------------------------------------------------}

var n: integer;

 procedure Advance;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Advances F.Index (the index into the IO buffer for the file F) and
{       reads a new block from the disk if necessary.
{
{-----------------------------------------------------------------------------}

 begin { Advance }
  with F, Flag do
   begin Index := Index + 1;
    if Index = Length then
     if BlockNumber = LengthInBlocks then FEof := true
     else
      begin FSBlkRead(FileNum,BlockNumber,Recast(Buffer,pDirBlk));
       BlockNumber := BlockNumber + 1;
       if BlockNumber = LengthInBlocks then { last block }
        if SizeInWords = 0 then
         Length := (LastBlockLength div 16) * ElsPerWord +
                   (LastBlockLength mod 16) div SizeInBits
        else Length := LastBlockLength div 16;
       Index := 0
      end
   end
 end { Advance };

begin { GetB }
 with F, Flag do
  begin
   if ReadError <> 0 then
    begin
     if FNotOpen then raise NotOpen;
     if FNotReset then raise NotReset(StreamName(F));
     if FEof then raise PastEof(StreamName(F))
    end;
   if SizeInWords > 0 then
    begin n := 0;
     repeat Advance;
      Element.W[n] := Buffer^.W[Index];
      n := n + 1
     until (n = SizeInWords) or FEof
    end
   else
    begin Advance;
     with Buffer^ do
      case SizeInBits of
       0: ;
       1: Element.W[0] := B1[Index];
       2: Element.W[0] := B2[Index];
       3: Element.W[0] := B3[Index];
       4: Element.W[0] := B4[Index];
       5: Element.W[0] := B5[Index];
       6: Element.W[0] := B6[Index];
       7: Element.W[0] := B7[Index];
       8: Element.W[0] := B8[Index]
      end
    end
  end
end { GetB };

function FullLine( var F: FileType ): Boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Determines if there is a full line in the keyboard input buffer.
{       This is the case if a carriage-return has been typed.  This function
{       implements the Stream package's line editing facility.
{
{ Parameters:
{       F - file to be checked.
{
{ Returns:
{       True if a full line has been typed.
{
{ Errors:
{       UnitIOError  if IOCRead doesn't return IOEIOC or IOEIOB.
{       CtlCAbort  if a Control-C is encountered.
{
{-----------------------------------------------------------------------------}

var RawCh, Ch, CtrlLetter: char;
    i: integer;
    Done: Boolean;

 procedure AddChar;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Add Ch to the keyboard input buffer.
{
{-----------------------------------------------------------------------------}

 begin { AddChar }
  if KeyLength = 255 then Write(OutputF,Bel)
  else
   begin KeyLength := KeyLength + 1;
    if Ch < ' ' then
     Write(OutputF, '^', Chr(Ord(Ch) + Ord('@')))
    else Write(OutputF,Ch)
   end
 end { AddChar };

 procedure EraseChar;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Erase the most-recently typed character from the keyboard input
{       buffer and from the display screen.
{
{-----------------------------------------------------------------------------}

 var Ch: Char;
 begin { EraseChar }
  KeyLength := KeyLength - 1;
  Ch := KeyBuffer[KeyLength];
  if LAnd(Ord(Ch),#200) <> 0 then Ch := Chr(LAnd(Ord(Ch),#37));
  if Ch < ' ' then { control character }
   begin 
    SClearChar(Chr(Ord(Ch) + Ord('@')), RXor);
    SClearChar('^', RXor);
    if transInUse then
       begin
       TransChar('\');
       TransChar('\');
       end;
   end
  else begin
       SClearChar(Ch, RXor);
       if transInUse then TransChar('\');
       end;
 end { EraseChar };

begin { FullLine }
 if CRTyped or (F.Flag.CharReady and F.Flag.FEoln) then FullLine := True
 else
  begin
   SCurOn;
   SChrFunc(RRpl);
   repeat
    Status := IOCRead(KeyBoard,RawCh);
    if Status <> IOEIOB then
     if Status <> IOEIOC then raise UnitIOError(StreamName(F))
     else
      begin
       Ch := Key_Tlate(RawCh); 
       
       If (LAnd(Ord(RawCh), #200) <> 0) And (Ch <> Chr(LAnd( Ord(RawCh), #37)))
         Then KeyBuffer[KeyLength] := Ch      { we have a keypad character }
         Else KeyBuffer[KeyLength] := RawCh;  { not a key character        }
          
       if (Ord(RawCh) >= #37) and (Ord(RawCh) <= #177) then
        { not control character }
        AddChar
       else
        begin
         If LAnd(Ord(RawCh), #200) <> 0 Then    { Holding down control key }
           Begin
             CtrlLetter := Chr(LAnd( Ord(RawCh), #177) );
             If ((CtrlLetter >= 'A') And (CtrlLetter <= 'Z') Or
                 (CtrlLetter >= 'a') and (CtrlLetter <= 'z')) Then
                 CtrlLetter := Ch Else CtrlLetter := ' ';
           End Else CtrlLetter := ' ';


{BS}     if (RawCh = BS) or (CtrlLetter = CtrlH) then
          begin
           if KeyLength > 0 then EraseChar
          end
         else
{BW}      if (RawCh = BW) or (CtrlLetter = CtrlW) then
           begin
            Done := False;
            repeat
             if KeyLength = 0 then Done := True
             else
              if KeyBuffer[KeyLength-1] = ' ' then EraseChar
              else Done := True
            until Done;
            if KeyLength <> 0 then
             begin
              Done := False;
              Ch := KeyBuffer[KeyLength-1];
              EraseChar;
              if ((Ch >= 'A') and (Ch <= 'Z') or
                  (Ch >= 'a') and (Ch <= 'z')) then
               repeat
                if KeyLength = 0 then Done := True
                else
                 begin
                  Ch := KeyBuffer[KeyLength-1];
                  if ((Ch >= 'A') and (Ch <= 'Z') or
                      (Ch >= 'a') and (Ch <= 'z')) then EraseChar
                  else Done := True
                 end
               until Done
             end
           end
          else
{BL}       if (RawCh = BL) or (CtrlLetter = CtrlU) or (RawCh = OOPS) then
            begin
             while KeyLength > 0 do EraseChar
            end
           else
{^C}        if CtrlLetter = CtrlC then Raise CtlCAbort
            else
{Eof}        if CtrlLetter = Chr(F.EofCh) then
              begin
               Write(OutputF, '^', Chr(Ord(CtrlLetter) + Ord('@')));
               KeyBuffer[KeyLength] := Chr(F.EofCh);
               CRTyped := True
              end
             else
{CR}          if (RawCh = CR) Or (RawCh = Chr(140)) then { handle this below } 
                    CRTyped := True
              else
               if (Ord(RawCh) = NoScroll) Or (Ord(RawCh) = CtrlNoScroll) Then
                 { ignore the noscroll and Ctrl no scroll chars }
               else
{Omit}          if (Ord(Ch) >= #200) Or (Ord(CtrlLetter) in F.OmitCh) then
                  { ignore omitted characters }
{else}          else AddChar
        end
      end
   until CRTyped or (Status <> IOEIOC);
   if CRTyped then
    begin SCurOff;
     if KeyLength <> 255 then KeyLength := KeyLength + 1;
     if (RawCh = CR) Or (RawCh = Chr(140))
       then KeyBuffer[KeyLength-1] := Chr(F.EolCh);
     Writeln(OutputF);
     KeyNext := 0
    end;
   FullLine := CRTyped
  end
end { FullLine };

procedure GetC( var F: Filetype );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Advances to the next element of a character-structured file and gets
{       it into the file buffer variable.
{
{ Parameters:
{       F - the file to be advanced.
{
{ Errors:
{       NotOpen  - if F is not open.
{       NotReset -  if F has not been reset.
{       PastEof  - if an attempt is made to read F past Eof.
{       TimeOutError -  if RS: or RSX: times out.
{       UnitIOError  - if IOCRead doesn't return IOEIOC or IOEIOB.
{       UndfDevice -  if F is open, but the device number is bad.
{
{-----------------------------------------------------------------------------}

var Done: boolean;
    Count: integer;
    Ch, LastCh: Char;
begin { GetC }
 with F, Flag do
  begin
   if ReadError <> 0 then
    begin
     if FNotOpen then raise NotOpen;
     if FNotReset then raise NotReset(StreamName(F));
     if FEof then
      if FEoln then { allow this Get without error }
       begin
        FEoln := False;
        Exit(GetC)
       end
      else raise PastEof(StreamName(F))
    end;
   repeat Done := true;
    if FKind = CharacterStructured then
     case FileNum of
      Console: begin
                while not FullLine(F) do ;
                Ch := KeyBuffer[KeyNext]; 
                If LAnd(Ord(Ch), #200) <> 0 
                    Then F.Element.C := Chr(LAnd(Ord(Ch), #37)) 
                    Else F.Element.C := Ch;
                KeyNext := KeyNext + 1;
                if KeyNext >= KeyLength then
                 begin KeyLength := 0;
                  CRTyped := False
                 end
               end;
      RS:      begin Count := RSTimeOut;
                repeat Status := IOCRead(RS232In,Element.C);
                 Count := Count - 1
                until (Status <> IOEIOB) {or (Count = 0) ***time-out disabled};
                if Status <> IOEIOC then
                 if Status = IOEIOB then raise TimeOutError(StreamName(F))
                 else raise UnitIOError(StreamName(F));
                Ch := Element.C
               end;
      RSX:     if CompileRSX then
                begin Index := Index + 1;
                 if Index = Length then
                  begin
                   LastCh := Buffer^.C[Index-1];
                   Length := 0;
                   repeat Status := IOCWrite(RS232Out,CtrlQ)
                   until Status <> IOEIOB;
                   if Status <> IOEIOC then raise UnitIOError(StreamName(F));
                   repeat
                    repeat Status := IOCRead(RS232In,Ch)
                    until Status <> IOEIOB;
                    if Status <> IOEIOC then raise UnitIOError(StreamName(F));
                    if not (Ord(Ch) in OmitCh) then
                     begin
                      if (Length = 0) and
                         (LastCh = Chr(EolCh)) and (Ch = '>') then
                       Ch := Chr(EofCh);
                      Buffer^.C[Length] := Ch;
                      Length := Length + 1
                     end
                   until (Ch = Chr(EolCh)) or (Ch = Chr(EofCh)) or
                         (Length = 32);
                   if Ch <> Chr(EofCh) then
                    begin
                     repeat Status := IOCWrite(RS232Out,CtrlS)
                     until Status <> IOEIOB;
                     if Status <> IOEIOC then raise UnitIOError(StreamName(F))
                    end;
                   Index := 0
                  end;
                 Element.C := Buffer^.C[Index];
                 Ch := Element.C
                end
               else raise UndfDevice;
      otherwise: raise UndfDevice
      end
    else
     begin
      GetB(F);
      Ch := Element.C
     end;
    FEoln := FEof;
    if FEof then Element.C := ' '
    else
     if Ch < Chr(32) then
      if Ch <> Nul then
       if Ch = Chr(EolCh) then
        begin Element.C := ' ';
         FEoln := true
        end
       else
        if Ch = Chr(EofCh) then
         begin Element.C := ' ';
          FEof := true;
          FEoln := true
         end
        else
         begin
          if Ord(Ch) in OmitCh then Done := false
         end
      else
       if Ord(Nul) in OmitCh then Done := false
   until Done;
   CharReady := true
  end
end { GetC };

procedure PutB( var F: Filetype );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Writes the value of the file buffer variable to the block-structured
{       file and advances the file.
{
{ Parameters:
{       F - the file to be advanced.
{
{ Errors:
{       NotOpen  - if F is not open.
{       NotRewrite-   if F has not been rewritten.
{
{-----------------------------------------------------------------------------}

var n: integer;

 procedure Advance;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Advances F.Index (the index into the IO buffer for the file F) and
{       writes the block to the disk if necessary.
{
{-----------------------------------------------------------------------------}

 begin { Advance }
  with F, Flag do
   begin Index := Index + 1;
    if Index = Length then
     begin FSBlkWrite(FileNum,BlockNumber,Recast(Buffer,pDirBlk));
      BlockNumber := BlockNumber + 1;
      Index := 0
     end
   end
 end { Advance };

begin { PutB }
 with F, Flag do
  begin
   if WriteError <> 0 then
    begin
     if FNotOpen then raise NotOpen;
     if FNotRewrite then raise NotRewrite(StreamName(F))
    end;
   if SizeInWords > 0 then
    for n := 0 to SizeInWords-1 do
     begin
      Buffer^.W[Index] := Element.W[n];
      Advance
     end
   else
    begin
     with Buffer^ do
      case SizeInBits of
       0: ;
       1: B1[Index] := Element.W[0];
       2: B2[Index] := Element.W[0];
       3: B3[Index] := Element.W[0];
       4: B4[Index] := Element.W[0];
       5: B5[Index] := Element.W[0];
       6: B6[Index] := Element.W[0];
       7: B7[Index] := Element.W[0];
       8: B8[Index] := Element.W[0]
       end;
     Advance
    end
  end
end { PutB };

procedure PutC( var F: FileType );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Writes the value of the file buffer variable to the character-
{       structured file and advances the file.
{
{ Parameters:
{       F - the file to be advanced.
{
{ Errors:
{       NotOpen -  if F is not open.
{       NotRewrite -  if F has not been rewritten.
{       UnitIOError-   if IOCWrite doesn't return IOEIOC or IOEIOB.
{       TimeOutError-   if RS: or RSX: times out.
{       UndfDevice -  if F is open, but the device number is bad.
{
{-----------------------------------------------------------------------------}

var Count: integer;
    Ch: Char;
begin { PutC }
 with F, Flag do
  begin
   if WriteError <> 0 then
    begin
     if FNotOpen then raise NotOpen;
     if FNotRewrite then raise NotRewrite(StreamName(F))
    end;
   if FKind = CharacterStructured then
    case FileNum of
     Console: begin
               while CtrlSPending do ;
               Status := IOCWrite(ScreenOut,Element.C);
               if Status <> IOEIOC then raise UnitIOError(StreamName(F))
               else if TransInUse then TransChar(Element.C);
              end;
     RS:      begin Count := RSTimeOut;
               repeat Status := IOCWrite(RS232Out,Element.C);
                Count := Count - 1
               until ((Status <> IOEIOB) and (Status <> IOECBF))
                     {or (Count = 0) ***time-out disabled};
               if Status <> IOEIOC then
                if (Status = IOEIOB) or (Status = IOECBF) then
                 raise TimeOutError(StreamName(F))
                else raise UnitIOError(StreamName(F))
              end;
     RSX:     if CompileRSX then
               begin
                { flush RS232 input buffer }
                repeat Status := IOCRead(RS232In,Ch);
                until Status <> IOEIOC;
                if Status <> IOEIOB then raise UnitIOError(StreamName(F));
                { send the character }
                repeat Status := IOCWrite(RS232Out,Element.C)
                until Status <> IOEIOB;
                if Status <> IOEIOC then raise UnitIOError(StreamName(F));
                if (LAnd(Ord(Element.C),#177) > #37) { not a control character }
                   or (Element.C in [Chr(EolCh),Chr(EofCh)]) then
                 begin { wait to receive the echo }
                  repeat Status := IOCRead(RS232In,Ch);
                  until (Status <> IOEIOB) and
                        (not (Element.C in [Chr(EolCh),Chr(EofCh)])
                         or (Ch = LF));
                  if Status <> IOEIOC then raise UnitIOError(StreamName(F))
                 end
               end
              else raise UndfDevice;
     otherwise: raise UndfDevice
     end
   else PutB(F)
  end
end { PutC };

procedure PReadln( var F: Filetype );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Advances to the first character following an end-of-line.
{
{ Parameters:
{       F - the file to be advanced.
{
{-----------------------------------------------------------------------------}

begin { PReadln }
 if F.EolCh <> Ord(Nul) then
  repeat
   if not F.Flag.CharReady then GetC(F);
   F.Flag.CharReady := False
  until F.Flag.FEoln
end { PReadln };

procedure PWriteln( var F: Filetype );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Writes an end-of-line.
{
{ Parameters:
{       F - the file to which an end-of-line is written.
{
{-----------------------------------------------------------------------------}

begin { PWriteln }
 if F.NoiseCh <> Ord(Nul) then
  begin F.Element.C := Chr(F.NoiseCh); PutC(F) end;
 if F.EolCh <> Ord(Nul) then
  begin F.Element.C := Chr(F.EolCh); PutC(F) end
end { PWriteln };

procedure StreamKeyBoardReset( var F: Text );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Clears the keyboard input buffer and the file variable F so that
{       all input typed up to this point will be ignored.
{
{ Parameters:
{       F - file to be cleared.
{
{-----------------------------------------------------------------------------}

var P: record case integer of
         1: (P: ^FileType);
         2: (Offset: Integer;
             Segment: Integer)
         end;
begin { StreamKeyBoardReset }
 LoadAdr(F);
 StorExpr(P.Offset);
 StorExpr(P.Segment);
 with P.P^, Flag do
  if not (FNotOpen or FNotReset) then { F is open and reset }
   if FKind = CharacterStructured then
    if FileNum = Console then         { F is reset to Console: }
     CharReady := False;              { clear F^ }
 KeyLength := 0;                      { clear line editing buffer }
 CRTyped := False;
 SCurOff
end { StreamKeyBoardReset };

procedure InitStream;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Initializes the stream package.  Called by System.
{
{-----------------------------------------------------------------------------}

begin { InitStream }
 StreamSegment := 0;
 SCurOff;
 Rewrite(OutputF,'Console:');
 StreamKeyBoardReset( OutputF  { for lack of something better to pass } );
 TransInUse := false;
 TransFile := 0;
 TransBlock := 0;
 TransByte := 0;
end { InitStream };

function FullLn( var F: Text ): Boolean;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Determines if there is a full line in the keyboard input buffer.
{       This is the case if a carriage-return has been typed.  This function
{       is provided in order that a program may continue to do other things
{       while waiting for keyboard input.  If the file is not open to
{       the console, FullLn is always true.
{
{ Parameters:
{       F - file to be checked.
{
{ Returns:
{       True if a full line has been typed.
{
{ Errors:
{       NotOpen  - if F is not open.
{       NotReset -  if F has not been reset.
{
{-----------------------------------------------------------------------------}

var P: record case integer of
         1: (P: ^FileType);
         2: (Offset: Integer;
             Segment: Integer)
         end;
begin { FullLn }
 LoadAdr(F);
 StorExpr(P.Offset);
 StorExpr(P.Segment);
 with P.P^, Flag do
  begin
   if ReadError <> 0 then
    begin
     if FNotOpen then raise NotOpen;
     if FNotReset then raise NotReset(StreamName(P.P^));
     if FEof then FullLn := True
    end
   else
    if FKind = CharacterStructured then
     if FileNum = Console then FullLn := FullLine(P.P^)
     else FullLn := True
    else FullLn := True
  end
end { FullLn };

function StreamName( var F: FileType ): PathName;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Returns the file name associated with the file variable F.  For
{       block-structured files, the full path name including device and
{       partition is returned.  For character-structured files, the device
{       name is returned.
{
{ Parameters:
{       F - file variable whose name is to be returned.
{
{-----------------------------------------------------------------------------}

var Disk, Part: Integer;
    FileName: PathName;
    FIBlkPtr: ptrDiskBuffer;
begin { StreamName }
 if F.Flag.FNotOpen then StreamName := 'non-open file'
 else
  if F.Flag.FKind = CharacterStructured then
   begin
    StreamName := 'unknown character structured device';
    case F.FileNum of
     Console: StreamName := 'Console:';
     RS:      StreamName := 'RS:';
     RSX:     if CompileRSX then StreamName := 'RSX:'
     end
   end
  else
   if F.FileNum = 0 then StreamName := 'FileId = 0'
   else
    begin
     Disk := WhichDisk(FileIdToSegId(F.FileNum));
     if not DiskTable[Disk].InUse then
      begin
       StreamName := 'device not mounted';
       Exit(StreamName)
      end;
     Part := WhichPartition(FileIdToSegId(F.FileNum));
     if Part <> 0 then
      if not PartTable[Part].PartInUse then Part := 0;
     if Part = 0 then
      begin
       StreamName := 'partition not mounted';
       Exit(StreamName)
      end;
     New(StreamSegment,256,FIBlkPtr);
     FSBlkRead(F.FileNum,FIBlk,Recast(FIBlkPtr, pDirBlk));
     FileName := DiskTable[Disk].RootPartition;
     AppendChar(FileName, ':');
     AppendString(FileName, PartTable[Part].PartName);
     AppendChar(FileName, '>');
     AppendString(FileName, FIBlkPtr^.FSData.FileName);
     Dispose(FIBlkPtr);
     StreamName := FileName
    end
end { StreamName };


Procedure StartTranscript(fileName: PathName; append: boolean);
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Starts a transcript to the specified file.  The transcript will
{       contain all characters written to the screen.  A user-typed backspace
{       character will be echoed to the file as a backslash "\".  All other
{       characters will be copied directly.  Do not leave the transcript on
{       while running screen-based programs like PATCH or the EDITOR since they
{       will input lots of garbage into the transcript.  The transcript cannot
{       be read until the StopTranscript routine is called.   Note that some
{       programs, notably TYPEFILE, will not output anything to the transcript
{       since they RasterOp to the screen directly.  To get a file into the
{       transcript, use  COPY file ~ CONSOLE:
{
{ ***WARNING****
{               It is VERY dangerous to have transcripting on while running
{               the Scavenger or any similar program.  No checking is done
{               to insure that this is not done.  Caveat Emptor.
{ ***WARNING****
{               Also, it is dangerous to run Typefile while transcripting is
{               on.  The data typed out will be wrong (since TypeFile uses
{               ReadDisk) and the the PERQ may crash.
{ ***WARNING****
{
{ Parameters:
{       fileName - the name of the file that the transcript is supposed to
{                  go in.  If the name is empty or the file cannot be opened
{                  then the TransError exception is raised.
{       append - if false, the file is started from scratch.  That is, it will
{                contain only the text from this session.  If append is true,
{                then the new text will be put at the end of the file if it
{                already exists.  If the file does not exist, append will be
{                identical to not append.
{
{ Errors:
{       TransError - raised if this procedure is called and a transcript is
{                    already in effect or if the filename passed is invalid.
{
{-----------------------------------------------------------------------------}
   begin
   if transInUse then Raise TransError('Transcript already in use.');
   CreateSegment(TransSegment, 1, 1, 1);
   IncRefCount(TransSegment);
   New(TransSegment, 1, transPtr);
   transFile := FSLookUp(fileName, transBlock, TransByte);
   if append and (transFile <> 0) then
          begin
          transBlock := transBlock - 1;  {block starts with 0;  not count}
          transByte := transByte div 8;  {bytes not bits}
          if transByte >= 512 then
             begin
             transByte := 0;
             transBlock := transBlock+1;
             end
          else FSBlkRead(transFile, transBlock, transPtr);
          end
   else begin
        if transFile = 0 then transFile := FSEnter(fileName);
        if transFile = 0 then Raise TransError('File cannot be opened.');
        transBlock := 0;
        transByte := 0;
        end;
   transInUse := true;     
   end;


Procedure TransChar(c: Char);
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Enters a character into the transcript.
{
{ Parameters:
{       c - the character to put into the transcript.
{
{ Errors:
{       TransError - raised if transcript is not open.
{
{-----------------------------------------------------------------------------}
   begin
   if not TransInUse then Raise TransError('TransChar but no Transcript.');
   TransPtr^.ByteBuffer[transByte] := ord(c);
   TransByte := TransByte + 1;
   if TransByte = 512 then
      begin
      FSBlkWrite(TransFile, TransBlock, TransPtr);
      TransBlock := TransBlock + 1;
      TransByte := 0;
      end;
   end;


Procedure StopTranscript;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Closes the transcript file.  You must call this before accessing the
{       transcript.  If not, the file will be incomplete and not closed.
{
{ Errors:
{       TransError - raised if no transcript is open.
{
{-----------------------------------------------------------------------------}
   begin
   if not transInUse then Raise TransError('Transcript not in use.');
   if TransByte = 0 then {at start of a block}
      if TransBlock <> 0 then {need to go to last block; it is full}
        begin
        TransBlock := TransBlock - 1;
        TransByte := 512;
        end
      else {file is empty}
   else FSBlkWrite(TransFile, TransBlock, TransPtr); {write out last block}
   FSClose(transFile, transBlock+1, transByte*8);
   dispose(transPtr);
   DecRefCount(TransSegment);
   DecRefCount(TransSegment); {get rid of segment by decrementing count twice}
   transInUse := false;
   TransFile := 0;
   TransBlock := 0;
   TransByte := 0;
   end.
