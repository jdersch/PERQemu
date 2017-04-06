{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program OdtPrq(input,output);


{ OdtPrq - Perq-to-Perq debugger.
  John P. Strait        1 Jan 81.   Rewritten and lots of neat stuff added.
  RT11 version  Bill Glass   ca. 1978.
  UCSD version  Miles Barel  ca. 1780.
  
{  copyright 1983  Three Rivers Computer Corporation
{
  
  Change history.

   2 Dec 82  V8.5  C Beckett    14 Char. compiler fixes.
  
  15 Nov 82  V8.4  J Strait     Use explicit entrypoints for breakpoints to
                                make upper bank breakpoints work.
  
  15 Jul 82  V8.3  J Strait     Change Boot command so that Boot <number> sends
                                sends <number> across the link but doesn't
                                send a boot image.
  
  22 Feb 82  V8.2  J Strait     Fix bug in sending controlstore addresses
                                to the Krnl.  Upper bank addresses were sent
                                incorrectly.
                                Fix bug in SendUser that prevented loading into
                                the upper bank of the controlstore.

  27 Jan 82  V8.1  J Strait     Fix bug in font processing.
                                Fix bug in QLoad for POS.
                                Check size of ScreenSeg when QLoading.

  13 Jan 82  V8.0  J Strait     Make one OdtPrq to work under both POS and
                                MPOS via conditional compilation.
                                
   6 Jan 82  V7.4  J. Strait    Print decimal and octal numbers in
                                different fonts.
   
   4 Jan 82  V7.3  J. Strait    Add stack traceback options.
   
   9 Dec 81  V7.2  J. Strait    Add Virtual memory display command.
  
   7 Dec 81  V7.1  J. Strait    1. Handle multiple processes.
                                2. Add Dump Process.
                                3. Add Dump PostOffice.
                                4. Add ListProcesses.
  
  22 Nov 81  V7.0  J. Strait    1. Do virtual address translation to find
                                   the bootblock.
                                2. Use RunRead for process system.
  
  20 Oct 81  V6.3  J. Strait    Fix bug in Watch'ed values.
  
   5 Oct 81  V6.2  J. Strait    Changes for 16K WCS and full memory complement.
  
   6 Jul 81  V6.1  J. Strait    Enable the tablet explicitly.
  
  30 Jun 81  V6.0  J. Strait    Allow running with shortened screen.
                                Move Link microcode to #7400.
  
   4 Jun 81  V5.9  J. Strait    Fixed Control-C processing for D.

   3 Jun 81  V5.8  Brad Myers   Changed IO import for D.
                                Expand screen to full size

  18 May 81  V5.7  J. Strait    1) Increase number of Q-Code segments to 50.
                                2) Update Dump MTables command to agree with
                                   virtual memory version of Memory manager.
                                3) Update QLoad to agree with new format of
                                   run files.

  18 May 81  V5.6  J. Strait    Add BLoad command to load a binary file into
                   P. Reddy     memory.
  
  17 Apr 81  V5.5  J. Strait    1) Fix backspace for version C.
                                2) Clear micro breakpoints in QLoad.
                                3) Use module name instead of file name in 
                                   QLoad.
  
  27 Mar 81  V5.4  J. Strait    Minor changes for new version of POS version C.
  
  13 Mar 81  V5.3  J. Strait    1. Use .Boot and .MBoot files in LoadRunFile.
                                2. Fix termination to not re-create window 0.
  
  22 Feb 81  V5.2  J. Strait    Fix name conflicts with the CMU file system.
  
  21 Feb 81  V5.1  J. Strait    1. Converted to system version C.3.
                                2. Fixed bug in Dump Stack.
                                3. Added ListSegments command.
                                4. Change 'Command' command to '@'.
}


exports

 imports ControlStore from ControlStore;
 imports CmdParse from CmdParse;
 imports Screen from Screen;

const
    MPOS = False;              { True to compile MPOS version of OdtPrq }

{$ifc MPOS then}
    {$message Compiling MPOS version of OdtPrq.}
    OdtPrqVersion = '8.4 MPOS';
{$elsec}
    {$message Compiling POS version of OdtPrq.}
    OdtPrqVersion = '8.4 POS';
{$endc}
    
    { top level commands }
    
    CHelp           = 1;
    CQuit           = 2;
    CBoot           = 3;
    CDump           = 4;
    CGo             = 5;
    CBLoad          = 6;
    CListFile       = 7;
    CMemory         = 8;
    CRegister       = 9;
    CUCode          = 10;
    CLoad           = 11;
    COverlay        = 12;
    CBreak          = 13;
    CKillBreak      = 14;
    CQBreak         = 15;
    CQKillBreak     = 16;
    CProceed        = 17;
    CListBreaks     = 18;
    CQLoad          = 19;
    CClear          = 20;
    CVariable       = 21;
    CGlobal         = 22;
    CWatch          = 23;
    CSaveState      = 24;
    CGetState       = 25;
    CListSegments   = 26;
    CListProcesses  = 27;
    CVirtual        = 28;
    CDebug          = 29;
    CNumCmds        = 29;
    CNotFound       = CNumCmds + 1;
    CNotUnique      = CNumCmds + 2;
    
    { Dump subsystem commands }
    
    DumpHelp        = 1;
    DumpQuit        = 2;
    DumpMemory      = 3;
    DumpRegisters   = 4;
    DumpStack       = 5;
    DumpMTables     = 6;
    DumpIOTables    = 7;
    DumpAll         = 8;
    DumpPrcess      = 9;
    DumpPstOffice   = 10;
    DumpTrace       = 11;
    DumpNumCmds     = 11;
    DumpNotFound    = DumpNumCmds + 1;
    DumpNotUnique   = DumpNumCmds + 2;
    
    
    Nul =   Chr(#000);
    Bel =   Chr(#007);
    BS =    Chr(#010);
    Tab =   Chr(#011);
    LF =    Chr(#012);
    FF =    Chr(#014);
    CR =    Chr(#015);
    CtrlC = Chr(#003);
    CtrlD = Chr(#004);
    CtrlQ = Chr(#021);
    CtrlS = Chr(#023);
    CtrlU = Chr(#025);
    CtrlZ = Chr(#032);
    Del   = Chr(#177);


    InterruptRegister = #360;
    
    
    { entrypoints in Link.Micro }
    
    LinkInit = #7400;
    LinkSnd  = #7410;
    LinkRcv  = #7420;
    

    { Link Protocol Codes }
        
    LuA  = #1630;  { load microstore address }
    WuWa = #1631;  { write microstore word a (low third) }
    WuWb = #1632;  { write microstore word b (mid third) }
    WuWc = #1633;  { write microstore word c (high third) }
    LrA  = #1634;  { load register address }
    Wr   = #1635;  { write register }
    Rr   = #1636;  { read register }
    LmA  = #1637;  { load memory address }
    WmW  = #1640;  { write memory word }
    RmW  = #1641;  { read memory word }
    SuP  = #1642;  { start microprogram }
    XXX1 = #1643;  { unused }
    Clr  = #1644;  { clear memory }
    WmB  = #1645;  { write memory block }
    WuB  = #1646;  { write microstore block }
    RuS  = #1647;  { reset microprocessor state }
    
    Hello = #12345;      { message from Krnl to confirm the boot }
    Hey   = #54321;      { message from Krnl to announce a breakpoint }
        
        
    MaxUAddr = #37777;
    UArraySize = #1000;
    BeginKrnl = #7400;
    EndKrnl = #7777;
    SizeControlStore = (3 * UArraySize + 255) div 256;
    
    NBkp = 20;
    PBkp1 = NBkp;
    PBkp2 = NBkp-1;
    
    BkpQCode = #376;

    
    

type

    
    Bit20 = record Upper, Lower: 0..#1777 end;
    
    BkpKind = (Unused, UBkp, QBkp);

    ValKind = (NoKind, MKind, RKind, UKind, VKind);
    ValSign = (Signed, UnSigned);
    ValBase = (Octal, Decimal, Character);
    ValSize = (Wrd, Byt);
    ValForm = packed record
                Sign: ValSign;
                Base: ValBase;
                Size: ValSize
                end;
    
    UArray = array[0..UArraySize-1] of MicroInstruction;
    pUArray = ^UArray;
    
    ControlStore = array[0..MaxUAddr div UArraySize] of pUArray;
    
    SavedState = file of Integer;



var
    NoDoneBit: Boolean;     { if last communication failed }
    PrintNoDone: Boolean;
    InInFile: Boolean;
    InFile, OutFile: Text;
    Fwa, Lwa: Bit20;
    Done: Boolean;
    TMI: TransMicro;
    Zero, One, Two16: Bit20;
    Commands, DumpCommands: CmdArray;
    CmdLine, CmdString, Ignore: String;
    BreakCh: Char;
    Cmd: Integer;
    TopLevel: Boolean;
    BreakSet: set of Char;
    BreakString, BreakDigits: String;
    
    Nill: Bit20;
    MAddress: Bit20;
    RAddress: Integer;
    UAddress: Integer;
    VProcess: Integer;      { < 0 means virtual address, for POS process <= 0 }
    VSegment: Integer;
    VRoutine: Integer;      { < 0 means global variable }
    VOffset: Integer;
    Kind: ValKind;
    Form: ValForm;
    OpenLocation, Open: Boolean;

    UCode: ControlStore;
    EmptyInstruction: MicroInstruction;
    
    Bytes: packed record case integer of
             1: (int:integer);
             2: (byt0:0..#377;
                 byt1:0..#377);
             3: (ch0: 0..#177; fill1: 0..1;
                 ch1: 0..#177; fill2: 0..1)
             end;
    Bkp: array[1..NBkp] of record
          case Kind: BkpKind of
           Unused: ();
           UBkp:   (UAddr: Integer);
           QBkp:   (Seg:   Integer;
                    Rtn:   Integer;
                    Instr: Integer;
                    QCode: Integer)
          end;
          
    BkpLong, BkpLeap: MicroInstruction;
    LastBkp, ProceedBkp: Integer;
    ProceedAddr: Integer;
    StartAddress: Integer;
    
    DEBUG: Boolean;
    
    Font8, Font10: FontPtr;
 
 
 
 
 procedure OdtCommand;
 procedure Quit;



private


label 99;

    
const {$Include RD.Dfs}


{$R-}


 imports Perq_String from Perq_String;
 imports Memory from Memory;
 imports Screen from Screen;
 imports FileSystem from FileSystem;
 imports System from System;
 imports OdtUtils from OdtUtils;
 imports OdtDump from OdtDump;
 
 
 handler CtlC;
 begin { CtlC }
   Writeln('^C');
   if Done then Exit(OdtPrq)
   else Goto 99
 end { CtlC };
 
 
 procedure GetSave( var StateFile: SavedState; Saving: Boolean );
 
 
  procedure Which( S: String );
  begin { Which }
   if Saving then Write('  Saving ') else Write('  Getting ');
   Writeln(S)
  end { Which };
  
  
  procedure Int( var I: Integer );
  begin { Int }
   if Saving then
    begin StateFile^ := I; Put(StateFile) end
   else
    begin I := StateFile^; Get(StateFile) end
  end { Int };
 
 
  procedure B20( var B: Bit20 );
  begin { B20 }
   if Saving then
    begin StateFile^ := B.Lower; Put(StateFile);
     StateFile^ := B.Upper; Put(StateFile)
    end
   else
    begin B.Lower := LAnd(StateFile^,#1777); Get(StateFile);
     B.Upper := LAnd(StateFile^,#1777); Get(StateFile)
    end
  end { B20 };
  
  
  procedure Str( var S: String );
  var I, L: Integer;
  begin { Str }
   if Saving then
    begin StateFile^ := Length(S); Put(StateFile);
     for I := 1 to Length(S) do
      begin StateFile^ := Ord(S[I]); Put(StateFile) end
    end
   else
    begin L := StateFile^; Get(StateFile);
     S := '';
     for I := 1 to L do
      begin AppendChar(S, Chr(LAnd(StateFile^,#377)));
       Get(StateFile)
      end
    end
  end { Str };
 
 
  procedure UC;
  var I, J, UAddr: Integer;
  begin { UC }
   Which('control store.');
   if Saving then
    begin
     for I := 0 to MaxUAddr div UArraySize do
      if UCode[I] <> nil then
       for J := 0 to UArraySize - 1 do
        with UCode[I]^[J] do
         if (Word1 <> -1) or (Word2 <> -1) or (Word3 <> -1) then
          begin
           UAddr := I * UArraySize + J;
           Int(UAddr);
           Int(Word1);
           Int(Word2);
           Int(Word3)
          end;
     StateFile^ := -1; Put(StateFile)
    end
   else
    begin
     for I := 0 to MaxUAddr div UArraySize do
      DestroyUCode(UCode[I]);
     while StateFile^ >= 0 do
      begin
       I := StateFile^ div UArraySize;
       J := StateFile^ mod UArraySize;
       Get(StateFile);
       if I < MaxUAddr div UArraySize then
        begin
         CreateUCode(UCode[I]);
         with UCode[I]^[J] do
          begin
           Int(Word1);
           Int(Word2);
           Int(Word3)
          end
        end
       else
        begin
         Get(StateFile);
         Get(StateFile);
         Get(StateFile)
        end
      end;
     Get(StateFile)
    end
  end { UC };
  
  
  procedure Bk;
  var I: Integer;
  begin { Bk }
   Which('breakpoints.');
   if Saving then
    begin
     for I := 0 to nBkp do
      with Bkp[I] do
       if Kind <> Unused then
        begin Int(I);
         StateFile^ := Recast(Kind,Integer); Put(StateFile);
         case Kind of
          UBkp:   Int(UAddr);
          QBkp:   begin Int(Seg);
                   Int(Rtn);
                   Int(Instr);
                   Int(QCode)
                  end
          end
        end;
     StateFile^ := -1; Put(StateFile)
    end
   else
    begin
     for I := 0 to nBkp do
      with Bkp[I] do
       if I = StateFile^ then
        begin Get(StateFile);
         Kind := Recast(StateFile^,BkpKind); Get(StateFile);
         case Kind of
          UBkp:   Int(UAddr);
          QBkp:   begin Int(Seg);
                   Int(Rtn);
                   Int(Instr);
                   Int(QCode)
                  end
          end
        end
       else Kind := Unused;
     while StateFile^ >= 0 do Get(StateFile);
     Get(StateFile)
    end
  end { Bk };
  
  
  procedure Sl;
  var X, Y, I: Integer;
  begin { Sl }
   Which('watched locations.');
   if Saving then
    begin
     for X := 0 to MaxX do
      for Y := 0 to MaxY do
       begin I := Y * MaxX + MaxX;
        with Slot[X,Y] do
         if slKind <> NoKind then
          begin Int(I);
           StateFile^ := Recast(slForm.Sign,Integer); Put(StateFile);
           StateFile^ := Recast(slForm.Base,Integer); Put(StateFile);
           StateFile^ := Recast(slForm.Size,Integer); Put(StateFile);
           StateFile^ := Recast(slNameDsp,Integer); Put(StateFile);
           Str(slName);
           StateFile^ := Recast(slKind,Integer); Put(StateFile);
           case slKind of
            MKind,
            RKind:  B20(slLoc);
            VKind:  begin B20(slProcess);
                     B20(slSegment);
                     B20(slRoutine);
                     B20(slOffset)
                    end
            end
          end
       end;
     StateFile^ := -1; Put(StateFile)
    end
   else
    begin
     for X := 0 to MaxX do
      for Y := 0 to MaxY do
       begin I := Y * MaxX + MaxX;
        with Slot[X,Y] do
         if I = StateFile^ then
          begin Get(StateFile);
           slVal := Nill;
           slForm.Sign := Recast(StateFile^,ValSign); Get(StateFile);
           slForm.Base := Recast(StateFile^,ValBase); Get(StateFile);
           slForm.Size := Recast(StateFile^,ValSize); Get(StateFile);
           slNameDsp := Recast(StateFile^,NameDsp); Get(StateFile);
           Str(slName);
           slKind := Recast(StateFile^,ValKind); Get(StateFile);
           case slKind of
            MKind,
            RKind:  B20(slLoc);
            VKind:  begin B20(slProcess);
                     B20(slSegment);
                     B20(slRoutine);
                     B20(slOffset)
                    end
            end
          end
         else slKind := NoKind
       end;
     while StateFile^ >= 0 do Get(StateFile);
     Get(StateFile)
    end
  end { Sl };
  
  
{$ifc MPOS then}
  procedure QP;
  var P, S: Integer;
  begin { QP }
   Which('Q-code process information.');
   if Saving then
    begin
     for P := 1 to MaxQProc do
      with QProc[P] do
       if PName <> '' then
        begin
         Int(P);
         Str(PName);
         Int(PStackSegment);
         for S := 1 to MaxQSeg do
          if GDB[S] <> 0 then
           begin
            Int(S);
            Int(GDB[S])
           end;
         StateFile^ := -1; Put(StateFile)
        end;
     StateFile^ := -1; Put(StateFile)
    end
   else
    begin
     for P := 1 to MaxQProc do
      with QProc[P] do
       if P = StateFile^ then
        begin
         Get(StateFile);
         Str(PName);
         Int(PStackSegment);
         for S := 1 to MaxQSeg do
          if S = StateFile^ then
           begin Get(StateFile);
            Int(GDB[S])
           end
          else GDB[S]:= 0;
          while StateFile^ >= 0 do Get(StateFile);
          Get(StateFile)
         end
        else
         begin
          PName := '';
          PStackSegment := 0
         end;
     while StateFile^ >= 0 do Get(StateFile);
     Get(StateFile)
    end
  end { QP };
{$endc}
  
  
  procedure QS;
  var S: Integer;
  begin { QS }
{$ifc MPOS then}
   Which('Q-code segment names.');
   if Saving then
    begin
     for S := 0 to MaxQSeg do
      if QSeg[S] <> '' then
       begin
        Int(S);
        Str(QSeg[S])
       end;
     StateFile^ := -1; Put(StateFile)
    end
   else
    begin
     for S := 0 to MaxQSeg do
      if S = StateFile^ then
       begin
        Get(StateFile);
        Str(QSeg[S]);
       end
      else QSeg[S] := '';
     while StateFile^ >= 0 do Get(StateFile);
     Get(StateFile)
    end
{$elsec}
   Which('Q-code segment names.');
   if Saving then
    begin
     for S := 1 to MaxQSeg do
      with QSeg[S] do
       if Name <> '' then
        begin Int(S);
         Str(Name);
         Int(GDB)
        end;
     StateFile^ := -1; Put(StateFile)
    end
   else
    begin
     for S := 1 to MaxQSeg do
      with QSeg[S] do
       if S = StateFile^ then
        begin Get(StateFile);
         Str(Name);
         Int(GDB)
        end
       else Name := '';
     while StateFile^ >= 0 do Get(StateFile);
     Get(StateFile)
    end
{$endc}
  end { QS };
 
 
 begin { GetSave }
  Writeln;
  UC;
  Bk;
  Sl;
{$ifc MPOS then}
  QP;
{$endc}
  QS;
  Which('miscellaneous information.');
  Int(LastBkp);
  Int(ProceedBkp);
  Int(ProceedAddr);
  Int(StartAddress);
{$ifc MPOS then}
  Int(SysNSNumber);
  Int(PrcSNumber);
  Int(MsgSNumber);
{$endc}
  Writeln
 end { GetSave };


 procedure OdtCommand;
  
  
  handler CtlC;
  begin { CtlC }
    Writeln('^C');
    Exit(OdtCommand)
  end { CtlC };
  
  
  procedure SendBoot;
  var I: Integer;
  begin { SendBoot }
   if CmdLine = '' then
    begin
     if UCode[EndKrnl div UArraySize] = nil then
      Error('Can''t find the Krnl.');
     WriteWord(Hello);
     for I := EndKrnl downto BeginKrnl do
      begin Translate(UCode[I div UArraySize]^[I mod UArraySize],TMI);
       WriteWord(LNot(TMI.Word1));
       WriteWord(LNot(TMI.Word2));
       WriteWord(LNot(TMI.Word3))
      end;
     I := ReadWord(TryRead);
     if not NoDoneBit and (I = Hello) then
      begin PushInt(0); PushInt(InterruptRegister); WriteReg;
       if not NoDoneBit then
        begin
         Writeln('  Bootstrap successful, interrupts enabled.');
         Update(False)
        end
       else
        Writeln('  Bootstrap unsuccessful, couldn''t enable interrupts.')
      end
     else
      begin
       Write('  Bootstrap unsuccessful, reply = ');
       PushInt(I);
       Oct(Output,1)
      end
    end
   else
    begin
     PushInt(Hey); ReadNumber('Value to send across the link');
     PopInt(I);
     WriteWord(I)
    end
  end { SendBoot };


  procedure LoadUser;
   
   
   procedure SendUser;
   { load user microcode }
     var I, J, UAddr:integer; TMicro: TransMicro;
   begin
   for I := 0 to MaxUAddr div UArraySize do
    if UCode[I] <> nil then
     for J := 0 to UArraySize - 1 do
      begin
       UAddr := I * UArraySize + J;
       if (UAddr < BeginKrnl) or (UAddr > EndKrnl) then
        with UCode[I]^[J] do
         if (Word1 <> -1) or (Word2 <> -1) or (Word3 <> -1) then
          begin PushInt(UAddr);
           WriteMicro(UCode[I]^[J])
          end
      end
   end { SendUser };
  
  
  begin { LoadUser }
   ReadString('Micro binary file name');
   DefaultExtension(CmdString,'.BIN');
   if Existant(CmdString) then
    begin
     ClearUBkps;
     ReadMicro(CmdString, Cmd = COverlay);
     SendUser;
     Writeln('  ', CmdString, ' loaded');
    end
   else
    Writeln('  File not found: ', CmdString)
  end { LoadUser };
 
 
  procedure Display( WriteAddress: Boolean );
  
 
   procedure DVariable;
   begin { DVariable }
    PushInt(VProcess); PushInt(VSegment); PushInt(VRoutine); PushInt(VOffset);
    DisplayVal(VKind,Form)
   end { DVariable };
   
   
   procedure DMemory;
   begin { DMemory }
    Push(MAddress); DisplayVal(MKind,Form)
   end { DMemory };
 
 
   procedure DRegister;
   var Val: Bit20;
   begin { DRegister }
    PushInt(RAddress); DisplayVal(RKind,Form)
   end { DRegister };
 
 
   procedure DUCode;
   var UInstruction: MicroInstruction;
       I, J: Integer;
   begin { DUCode }
    I := UAddress div UArraySize;
    J := UAddress mod UArraySize;
    if UCode[I] = nil then UInstruction := EmptyInstruction
    else UInstruction := UCode[I]^[J];
    with UInstruction do
     begin
      Write(OutFile, ' X='); PushInt(X); Oct(OutFile,3);
      Write(OutFile, ' Y='); PushInt(Y); Oct(OutFile,3);
      Write(OutFile, ' A='); PushInt(A); Oct(OutFile,1);
      Write(OutFile, ' B='); PushInt(B); Oct(OutFile,1);
      Write(OutFile, ' W='); PushInt(W); Oct(OutFile,1);
      Write(OutFile, ' H='); PushInt(H); Oct(OutFile,1);
      Write(OutFile, ' ALU='); PushInt(ALU); Oct(OutFile,2);
      Write(OutFile, ' F='); PushInt(F); Oct(OutFile,1);
      Write(OutFile, ' SF='); PushInt(SF); Oct(OutFile,2);
      Write(OutFile, ' Z='); PushInt(Z); Oct(OutFile,3);
      Write(OutFile, ' CND='); PushInt(CND); Oct(OutFile,2);
      Write(OutFile, ' JMP='); PushInt(JMP); Oct(OutFile,2)
     end
   end { DUCode };

 
  begin { Display }
   if (BreakCh = ' ') and (Kind in [MKind,RKind,VKind]) then NumericForm(Form);
   if WriteAddress then
    begin Writeln(OutFile);
     case Kind of
      MKind: begin Write(OutFile, 'Memory ');
              Push(MAddress); Oct(OutFile, 1)
             end;
      RKind: begin
              Write(OutFile, 'Register ');
              PushInt(RAddress);
              Oct(OutFile,1)
             end;
      UKind: begin
              Write(OutFile, 'UCode ');
              PushInt(UAddress);
              Oct(OutFile,1)
             end;
      VKind: begin
              if VProcess < 0 then
                begin
                  Write(OutFile, 'Virtual ');
                  QSegName(OutFile,0,VSegment);
                  Write(OutFile, ' ');
                  PushInt(VOffset);
                  Oct(OutFile,1)
                end
              else
                begin
                  if VRoutine < 0 then Write(OutFile, 'Global ')
                  else Write(OutFile, 'Variable ');
{$ifc MPOS then}
                  QPrcName(OutFile,VProcess);
                  Write(OutFile, ' ');
{$endc}
                  QSegName(OutFile,VProcess,VSegment);
                  if VRoutine >= 0 then
                   begin
                    Write(OutFile, ' ');
                    PushInt(VRoutine);
                    Oct(OutFile,1)
                   end;
                  Write(OutFile, ' ');
                  PushInt(VOffset);
                  Oct(OutFile,1)
                end
             end
      end;
     Write(OutFile, '/')
    end;
   Write(OutFile, ' ');
   case Kind of
    MKind: DMemory;
    RKind: DRegister;
    UKind: DUCode;
    VKind: DVariable
    end;
   Write(OutFile, ' ');
   Open := True
  end { Display };


  procedure Change;
   
   
   procedure CVariable;
   begin { CVariable }
    PushInt(0); ReadNumber('New variable value');
    PushInt(VProcess); PushInt(VSegment); PushInt(VRoutine); PushInt(VOffset);
    VarAddress;
    WriteMem
   end { CVariable };
   
   
   procedure CMemory;
   begin { CMemory }
    PushInt(0); ReadNumber('New memory value'); Push(MAddress); WriteMem
   end { CMemory };
   
   
   procedure CRegister;
   begin { CRegister }
    PushInt(0); ReadNumber( 'New register value'); PushInt(RAddress); WriteReg
   end { CRegister };
   
   
   procedure CUCode;
   var I, J: Integer;
   
   
    function ChangeField(fld:string; def,max:integer):integer;
    var TmpInInFile: Boolean;
        TmpLine: String;
        Val: Integer;
    begin { ChangeField }
     PushInt(Def); ReadNumber(Fld); PopInt(Val);
     TmpInInFile := InInFile;
     TmpLine := CmdLine;
     while (Val < 0) or (Val > Max) do
      begin Write('  Value for ', Fld, ' too large, re-enter');
       InInFile := False;
       CmdLine := '';
       PushInt(Def); ReadNumber(Fld); PopInt(Val)
      end;
     ChangeField := Val;
     InInFile := TmpInInFile;
     CmdLine := TmpLine
    end { ChangeField };
 
 
   begin { CUCode }
    Writeln(OutFile);
    I := UAddress div UArraySize;
    J := UAddress mod UArraySize;
    CreateUCode(UCode[I]);
    with UCode[I]^[J] do
     begin
      X   := ChangeField('X', X, #377);
      Y   := ChangeField('Y', Y, #377);
      A   := ChangeField('A', A, 7);
      B   := ChangeField('B', B, 1);
      W   := ChangeField('W', W, 1);
      H   := ChangeField('H', H, 1);
      ALU := ChangeField('ALU', ALU, #17);
      F   := ChangeField('F', F, 3);
      SF  := ChangeField('SF', SF, #17);
      Z   := ChangeField('Z', Z, #377);
      CND := ChangeField('CND', CND, #17);
      JMP := ChangeField('JMP', JMP, #17)
     end;
    PushInt(UAddress);
    WriteMicro(UCode[I]^[J])
   end { CUCode };
  
  
  begin { Change }
   if not OpenLocation then Error('No location is open.');
   case Kind of
    MKind: CMemory;
    RKind: CRegister;
    UKind: CUCode;
    VKind: CVariable
    end;
   Update(False)
  end { Change };
  
  
  procedure DspMemory;
  begin { DspMemory };
   Push(MAddress); ReadNumber('Memory address'); Pop(MAddress);
   Kind := MKind;
   Display(InInFile)
  end { DspMemory };
  
  
  procedure DspRegister;
  begin { DspRegister };
   PushInt(RAddress); ReadNumber('Register number'); PopInt(RAddress);
   RAddress := LAnd(RAddress,#377);
   Kind := RKind;
   Display(InInFile)
  end { DspRegister };
  
  
  procedure DspUCode;
  begin { DspUCode };
   PushInt(UAddress); ReadNumber('Microcode address'); PopInt(UAddress);
   UAddress := LAnd(UAddress,#37777);
   Kind := UKind;
   Display(InInFile)
  end { DspUCode };
  
  
  procedure DspVariable;
  begin { DspVariable }
{$ifc MPOS then}
   QProcess; PopInt(VProcess);
{$elsec}
   VProcess := 0;
{$endc}
   QSegment(VProcess); PopInt(VSegment);
   PushInt(0); ReadNumber('Routine number'); PopInt(VRoutine);
   PushInt(0); ReadNumber('Offset'); PopInt(VOffset);
   Kind := VKind;
   Display(InInFile)
  end { DspVariable };
  
  
  procedure DspGlobal;
  begin { DspGlobal }
{$ifc MPOS then}
   QProcess; PopInt(VProcess);
{$elsec}
   VProcess := 0;
{$endc}
   QSegment(VProcess); PopInt(VSegment);
   VRoutine := -1;
   PushInt(0); ReadNumber('Offset'); PopInt(VOffset);
   Kind := VKind;
   Display(InInFile)
  end { DspGlobal };
  
  
  procedure DspVirtual;
  begin { DspVirtual }
   VProcess := -1;
   PushInt(0); ReadNumber('Segment'); PopInt(VSegment);
   VRoutine := -1;
   PushInt(0); ReadNumber('Offset'); PopInt(VOffset);
   Kind := VKind;
   Display(InInFile)
  end { DspVirtual };
  
  
  procedure DspAgain;
  begin { DspAgain }
   if BreakCh = LF then
    case Kind of
     MKind: begin Push(MAddress); PushInt(1); Add; Pop(MAddress) end;
     RKind: RAddress := LAnd(RAddress + 1,#377);
     UKind: UAddress := LAnd(UAddress + 1,#37777);
     VKind: VOffset := VOffset + 1
     end
   else
    if BreakCh = '^' then
     case Kind of
      MKind: begin Push(MAddress); PushInt(1); Sub; Pop(MAddress) end;
      RKind: RAddress := LAnd(RAddress + #377,#377);
      UKind: UAddress := LAnd(UAddress + #37777,#37777);
      VKind: VOffset := VOffset - 1
      end;
   Display(True)
  end { DspAgain };
    
  
  procedure Break;
  var Address: Integer;
      B: Integer;
      Found: Boolean;
  begin { Break }
   PushInt(0); ReadNumber('Break address'); PopInt(Address);
   Address := LAnd(Address,#37777);
   Found := False;
   B := 0;
   repeat B := B + 1;
    Found := (Bkp[B].Kind = UBkp) and (Bkp[B].UAddr = Address)
   until Found or (B = NBkp);
   if Cmd = CBreak then
    if Found then Error('There''s already a Breakpoint there.')
    else
     begin B := UnusedBkp; SetUBkp(B, Address) end
   else { CKillBreak }
    if Found then KillUBkp(B)
    else Error('Breakpoint not found.')
  end { Break };


  procedure QBreak;
  var Process, Segment, Routine, Instruction: Integer;
      B: Integer;
      Found: Boolean;
  begin { QBreak }
   QSegment(0); PopInt(Segment);
   PushInt(0); ReadNumber('Routine'); PopInt(Routine);
   PushInt(0); ReadNumber('Instruction'); PopInt(Instruction);
   B := 0;
   repeat B := B + 1;
    with Bkp[B] do
     Found := (Kind = QBkp) and
              (Seg = Segment) and (Rtn = Routine) and (Instr = Instruction)
   until Found or (B = NBkp);
   if Cmd = CQBreak then
     if Found then Error('There''s already a QBreakpoint there.')
     else
      begin B := UnusedBkp; SetQBkp(B,Segment,Routine,Instruction) end
   else { CQKillBreak }
    if Found then KillQBkp(B)
    else Error('QBreakpoint not found.');
  end { QBreak };
  
  
  procedure ListBreaks;
  var B: Integer;
  begin { ListBreaks }
   Writeln;
   for B := 1 to NBkp do
    with Bkp[B] do
     case Kind of
      Unused: ;
      UBkp:   begin
               Write(OutFile, ' Break  ');
               PushInt(B);
               Oct(OutFile,2);
               Write(' at address ');
               PushInt(UAddr);
               Oct(OutFile,5);
               Writeln
              end;
      QBkp:   begin
               Write(OutFile, ' QBreak ');
               PushInt(B);
               Oct(OutFile,8);
               Write(OutFile, ' at segment ');
               QSegName(OutFile, 0, Seg);
               Write(OutFile,', routine ');
               PushInt(Rtn);
               Oct(OutFile,8);
               Write(OutFile, ', instruction ');
               PushInt(Instr);
               Oct(OutFile,8);
               Write(OutFile, ', Q');
               PushInt(QCode);
               Int(OutFile,1);
               Writeln
              end
      end;
   Writeln
  end { ListBreaks };
  
  
  procedure ListSegments;
  var S: Integer;
  begin { ListSegments }
   Writeln;
   for S := 1 to MaxQSeg do
{$ifc MPOS then}
    if QSeg[S] <> '' then
     begin
      Write(' ', QSeg[S], ' ':QNameLength-Length(QSeg[S])+1, ' is segment ');
      PushInt(S);
      Int(Output,2);
      Writeln
     end;
{$elsec}
    with QSeg[S] do
     if Name <> '' then
      begin
       Write(' ', Name, ' ':QNameLength-Length(Name)+1, ' is segment ');
       PushInt(S);
       Int(Output,2);
       Write(Output, ', GDB offset = ');
       PushInt(GDB);
       Oct(Output,1);
       Writeln
      end;
{$endc}
   Writeln
  end { ListSegments };
  
  
  procedure ListProcesses;
  var P, S: Integer;
  begin { ListProcesses }
{$ifc MPOS then}
   Writeln;
   for P := 1 to MaxQProc do
    with QProc[P] do
     if PName <> '' then
      begin
       Write(' ', PName, ' is process ');
       PushInt(P);
       Int(Output,1);
       Write(', Stack segment = ');
       PushInt(PStackSegment);
       Int(Output,1);
       Writeln;
       for S := 1 to MaxQSeg do
        if QProc[P].GDB[S] <> 0 then
         begin
          Write(' ', QSeg[S], ' ':QNameLength-Length(QSeg[S])+1,
                'is segment ');
          PushInt(S);
          Int(Output,2);
          Write(', GDB offset = ');
          PushInt(GDB[S]);
          Oct(Output,1);
          Writeln
         end
      end;
   Writeln
{$elsec}
   Writeln('** Multiple processes not implemented in POS.')
{$endc}
  end { ListProcesses };
   
   
  procedure Clear;
  var Ignore: Bit20;
  begin { Clear }
   ClearQBkps;
   WriteWord(Clr);
   repeat PrintNoDone := False;
    PushInt(0); ReadReg; Pop(Ignore);
    PrintNoDone := True
   until not NoDoneBit
  end { Clear };
  
  
  procedure QLoad;
  begin { QLoad }
   ClearQBkps;
   ClearUBkps;
   ReadString('Root file name');
   LoadQSystem(CmdString)
  end { QLoad };
  
  
  procedure BLoad;
  var FId: FileId;
      Buffer: pDirBlk;
      Blocks, Bits, I, Word: Integer;
      MemAddr: Bit20;
  begin { BLoad }
   ReadString('Binary file name');
   if Existant(CmdString) then
    begin
     FId := FSLookUp(CmdString,Blocks,Bits);
     PushInt(0);
     ReadNumber('Memory address');
     Pop(MemAddr);
     New(0,256,Buffer);
     for I := 0 to Blocks-1 do
      begin
       FSBlkRead(FId,I,Buffer);
       WriteWord(LmA);
       WriteWord(Shift(MemAddr.Upper,10) + MemAddr.Lower);
       WriteWord(Shift(MemAddr.Upper,-6));
       WriteWord(WmB);
       for Word := 0 to 255 do WriteWord(Buffer^.Buffer[Word]);
       Push(MemAddr);
       PushInt(256);
       Add;
       Pop(MemAddr)
      end
    end
   else Error(Concat(CmdString, ' not found.'))
  end { BLoad };
  
  
  procedure Start;
  var Address, A1, A2: Integer;
      B: Integer;
      Found: Boolean;
   
   
   procedure FindSuccessors;
   var UInstruction: MicroInstruction;
       I, J: Integer;
   begin { FindSuccessors }
    I := UAddress div UArraySize;
    J := UAddress mod UArraySize;
    if UCode[I] = nil then UInstruction := EmptyInstruction
    else UInstruction := UCode[I]^[J];
    with UInstruction do
     begin
      if F = 3 then A1 := (15-SF) * 256 + (255-Z)
      else
       if (F = 1) and (SF = 7) then A1 := Y * 256 + (255 - Z)
       else
        A1 := LAnd( Address, #37400 ) + (255-Z);
      A2 := Address + 1
     end
   end { FindSuccessors };


  begin { Start }
   Address := 0;
   if Cmd = CGo then
    begin PushInt(StartAddress);
     ReadNumber('Start address');
     PopInt(StartAddress);
     Address := StartAddress;
     WriteWord(RuS)
    end
   else { CProceed }
    begin
     PushInt(ProceedAddr); ReadNumber('Proceed address'); PopInt(Address)
    end;
   Address := LAnd(Address,#37777);
   B := 0;
   repeat B := B + 1;
    Found := (Bkp[B].Kind = UBkp) and (Bkp[B].UAddr = Address)
   until (B = NBkp) or Found;
   ProceedBkp := 0;
   ProceedAddr := 0;
   if Found then
    begin ProceedBkp := B;
     ProceedAddr := Bkp[B].UAddr;
     FindSuccessors;
     SetUBkp(PBkp1,A1);
     SetUBkp(PBkp2,A2);
     KillUBkp(B)
    end;
   LastBkp := 0;
   StartPerq(Address)
  end { Start };

 
  procedure SetInFile;
  begin { SetInFile };
   ReadString('Command file name');
   if Pos(CmdString,':') = 0 then
    if Existant(CmdString) then
     begin
      if InInFile then Close(InFile);
      Reset(InFile,CmdString);
      InInFile := True
     end
    else Writeln(OutFile, '  File not found: ', CmdString)
   else
    begin
     if InInFile then Close(InFile);
     CnvUpper(CmdString);
     if CmdString = '' then CmdString := 'CONSOLE:';
     if CmdString = 'CONSOLE:' then InInFile := False
     else
      begin Reset(InFile,CmdString);
       InInFile := True
      end
    end
  end { SetInFile };
  
  
  procedure SetOutFile;
  begin { SetOutFile }
   ReadString('List file name');
   Close(OutFile);
   Rewrite(OutFile,CmdString)
  end { SetOutFile };
  
  
  procedure Watch;
  var Name: String;
      X, Y: Integer;
  begin { Watch }
   if not OpenLocation then
    Error('Can''t Watch, no open location');
   if not (Kind in [MKind, RKind, VKind]) then
    Error('Can''t Watch this kind of value');
   ReadString('Name');
   Name := '';
   if CmdString <> '' then
    if CmdString[1] in ['+', '-', '0'..'9'] then
     begin AppendChar(CmdString,' ');
      CmdLine := Concat(CmdString,CmdLine)
     end
    else
     begin Name := CmdString;
      if Length(Name) > 8 then Name := Substr(Name,1,8)
     end;
   PushInt(MaxX);
   ReadNumber('X coordinate');
   PopInt(X);
   PushInt(MaxY);
   ReadNumber('Y coordinate');
   PopInt(Y);
   if (X < 0) or (X > MaxX) then Error('X coordinate is out of range');
   if (Y < 0) or (Y > MaxY) then Error('Y coordinate is out of range');
   with slot[x,y] do
    begin
     if Name = '' then slNameDsp := NumberDsp
     else slNameDsp := SymbolDsp;
     slName := Name;
     slKind := Kind;
     slForm := Form;
     case Kind of
      MKind: slLoc := MAddress;
      RKind: begin PushInt(RAddress); Pop(slLoc) end;
      VKind: begin
              PushInt(VProcess); Pop(slProcess);
              PushInt(VSegment); Pop(slSegment);
              PushInt(VRoutine); Pop(slRoutine);
              PushInt(VOffset); Pop(slOffset)
             end;
      end;
     Push(Nill);
     Pop(slVal);
     Update(False)
    end
  end { Watch };
  
  
  procedure State;
  var StateFile: SavedState;
      Saving: Boolean;
      Ignore: Integer;
  begin { State }
   Saving := Cmd = CSaveState;
   ReadString('State file name');
   if Saving then
    begin
     if FSEnter(CmdString) = 0 then
      Error('Can''t create state file');
     Rewrite(StateFile,CmdString)
    end
   else
    begin
     if FSLookUp(CmdString,Ignore,Ignore) = 0 then
      Error('State file not found');
     Reset(StateFile,CmdString)
    end;
   GetSave(StateFile,Saving);
   Close(StateFile);
   Update(True)
  end { State };
 
 
 begin { OdtCommand }
  SetFont(Font8);
  StackReset;
  TopLevel := True;
  ReadString('>');
  TopLevel := False;
  OpenLocation := Open;
  Open := False;
  if CmdString = '' then
   begin
    if BreakCh = '=' then
     begin Change;
      if BreakCh <> ' ' then DspAgain
     end
    else
     if BreakCh in ['/', LF, '^'] then DspAgain
   end
  else
   if CmdString[1] in ['-', '0'..'9'] then
    begin
     AppendChar(CmdString,BreakCh);
     CmdLine := Concat(CmdString,CmdLine);
     Change;
     if BreakCh <> ' ' then DspAgain
    end
   else
    if BreakCh = '/' then
     begin BreakCh := ' ';
      AppendChar(CmdString,'/');
      CmdLine := Concat(CmdString,CmdLine);
      DspAgain
     end
    else
     if CmdString[1] = '@' then
      begin
       Delete(CmdString,1,1);
       AppendChar(CmdString,BreakCh);
       CmdLine := Concat(CmdString,CmdLine);
       SetInFile
      end
     else
      begin CnvUpper(CmdString);
       Cmd := UniqueCmdIndex(CmdString,Commands,CNumCmds);
       case Cmd of
        CHelp           : Help;
        CQuit           : Quit;
        CBoot           : SendBoot;
        CDump           : Dump;
        CGo,
        CProceed        : Start;
        CListFile       : SetOutFile;
        CMemory         : DspMemory;
        CRegister       : DspRegister;
        CUCode          : DspUCode;
        CVariable       : DspVariable;
        CGlobal         : DspGlobal;
        CVirtual        : DspVirtual;
        CLoad,
        COverlay        : LoadUser;
        CBreak,
        CKillBreak      : Break;
        CQBreak,
        CQKillBreak     : QBreak;
        CListBreaks     : ListBreaks;
        CListSegments   : ListSegments;
        CListProcesses  : ListProcesses;
        CClear          : Clear;
        CQLoad          : QLoad;
        CWatch          : Watch;
        CSaveState,
        CGetState       : State;
        CDebug          : begin ReadString('Debug on or off');
                           ConvUpper(CmdString);
                           if CmdString = 'ON' then DEBUG := True
                           else
                            if CmdString = 'OFF' then DEBUG := False
                            else
                             if CmdString = '' then DEBUG := not DEBUG
                          end;
        CBLoad          : BLoad;
        CNotFound       : begin
                           Writeln(OutFile, 'Unknown command: ', CmdString,
                                            ', type Help if you need it.');
                           CmdLine := ''
                          end;
        CNotUnique      : begin
                           Writeln(OutFile, 'Non-unique command: ', CmdString,
                                            ', type Help if you need it.');
                           CmdLine := ''
                          end
       end
   end
 end { OdtCommand };

 
 procedure Initialize;
 var I, J: Integer;
     Ch: Char;
     OldState: String;
     StateFile: SavedState;

 
  procedure InitLink;
  { initialize vars to handle physical link IO }
  var LinkBin: MicroFile;
      Success: boolean;
  begin
   PrintNoDone := true;
   Reset(LinkBin,'Link.Bin');
   LoadControlStore(LinkBin);
   LoadExpr(LOr(Shift(LinkInit,8),Shift(LinkInit,-8)));
   InLineByte( #277 {JCS} );
   StorExpr(Success);
   if not Success then
    begin Writeln('****** unable to initialize the Link.');
     Exit(OdtPrq)
    end
  end { InitLink };
  
  
  procedure InitFonts;
  var FontId: FileId;
      Blocks, Bits, B: Integer;
      FontSeg: SegmentNumber;
  begin { InitFonts }
    Font8 := GetFont;
    Font10 := Font8;
    FontId := FSLookUp('Odt13.Kst',Blocks,Bits);
    if FontId <> 0 then
      begin
        CreateSegment(FontSeg,Blocks,1,Blocks);
        for B := 0 to Blocks-1 do
          FSBlkRead(FontId,B,MakePtr(FontSeg,Shift(B,8),pDirBlk));
        Font8 := MakePtr(FontSeg,0,FontPtr)
      end;
    ChangeWindow(SlotWindow);
    SetFont(Font8);
    ChangeWindow(CmdWindow);
    SetFont(Font8)
  end { InitFonts };


 begin { Initialize }
  Done := True;
  Rewrite(OutFile,'Console:');
  InitSlots;
  Writeln;
  Writeln('OdtPrq ', OdtPrqVersion, ' - Perq debug utility.');
  Writeln;
  InitFonts;
  DEBUG := False;
  with EmptyInstruction do
   begin
    Word1 := -1;
    Word2 := -1;
    Word3 := -1
   end;
  for I := 0 to MaxUAddr div UArraySize do UCode[I] := nil;
  InitLink;
  Zero.Upper := 0;
  Zero.Lower := 0;
  One.Upper := 0;
  One.Lower := 1;
  Two16.Upper := #100;
  Two16.Lower := 0;
  Nill.Upper := #1777;
  Nill.Lower := #1777;
  Commands[CHelp           ] := 'Help';
  Commands[CQuit           ] := 'Quit';
  Commands[CBoot           ] := 'Boot';
  Commands[CDump           ] := 'Dump';
  Commands[CGo             ] := 'Go';
  Commands[CBLoad          ] := 'BLoad';
  Commands[CListFile       ] := 'ListFile';
  Commands[CMemory         ] := 'Memory';
  Commands[CRegister       ] := 'Register';
  Commands[CUCode          ] := 'UCode';
  Commands[CLoad           ] := 'Load';
  Commands[COverlay        ] := 'Overlay';
  Commands[CBreak          ] := 'Break';
  Commands[CKillBreak      ] := 'KillBreak';
  Commands[CQBreak         ] := 'QBreak';
  Commands[CQKillBreak     ] := 'QKillBreak';
  Commands[CProceed        ] := 'Proceed';
  Commands[CListBreaks     ] := 'ListBreaks';
  Commands[CClear          ] := 'Clear';
  Commands[CQLoad          ] := 'QLoad';
  Commands[CVariable       ] := 'Variable';
  Commands[CGlobal         ] := 'Global';
  Commands[CVirtual        ] := 'Virtual';
  Commands[CWatch          ] := 'Watch';
  Commands[CSaveState      ] := 'SaveState';
  Commands[CGetState       ] := 'GetState';
  Commands[CListSegments   ] := 'ListSegments';
{$ifc MPOS then}
  Commands[CListProcesses  ] := 'ListProcesses';
{$elsec}
  Commands[CListProcesses  ] := ' **untypeable**';
{$endc}
  Commands[CDebug          ] := 'Debug';
  for I := 1 to CNumCmds do CnvUpper(Commands[I]);
  DumpCommands[DumpHelp       ] := 'Help';
  DumpCommands[DumpQuit       ] := 'Quit';
  DumpCommands[DumpMemory     ] := 'Memory';
  DumpCommands[DumpRegisters  ] := 'Registers';
  DumpCommands[DumpStack      ] := 'Stack';
  DumpCommands[DumpTrace      ] := 'Trace';
  DumpCommands[DumpMTables    ] := 'MTables';
  DumpCommands[DumpIOTables   ] := 'IOTables';
  DumpCommands[DumpAll        ] := 'All';
{$ifc MPOS then}
  DumpCommands[DumpPrcess     ] := 'Process';
  DumpCommands[DumpPstOffice  ] := 'PostOffice';
{$elsec}
  DumpCommands[DumpPrcess     ] := ' **untypeable**';
  DumpCommands[DumpPstOffice  ] := ' **untypeable**';
{$endc}
  for I := 1 to DumpNumCmds do CnvUpper(DumpCommands[I]);
  CmdLine := '';
  InInFile := False;
  BreakSet := ['/', '^', Chr(0)..Chr(31)];
  BreakString := ' =,;';
  I := Length(BreakString);
  for Ch := Chr(0) to Chr(127) do
   if Ch in BreakSet then
    begin I := I + 1; AppendChar(BreakString,Ch) end;
  BreakDigits := Concat(BreakString,'-0123456789');
  MAddress := Zero;
  RAddress := 0;
  UAddress := 0;
  VProcess := 0;
  VSegment := 0;
  VRoutine := -1;
  VOffset := 0;
  Kind := MKind;
  Form.Sign := UnSigned;
  Form.Base := Octal;
  Form.Size := Wrd;
  Open := False;
  for i := 1 to NBkp do Bkp[i].Kind := Unused;
  LastBkp := 0;
  ProceedBkp := 0;
  ProceedAddr := 0;
  StartAddress := #2400;
  with BkpLong do
   begin X := #370;  Y := 0;  A := 0;  B := 1;  W := 1;  H := 0;
    ALU := 1;  F := 3;  SF := 0;  Z := #376;  CND := 0;  JMP := 3
   end;
  with BkpLeap do
   begin X := 0;  Y := #17;  A := 0;  B := 0;  W := 0;  H := 0;
    ALU := 0;  F := 1;  SF := 7;  Z := #277;  CND := 0;  JMP := 3
   end;
  ClearQSegments;
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,Ignore,' ',Ignore);   { skip 'OdtPrq' }
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,OldState,' ',Ignore);
  if OldState <> '' then
   if FSLookUp(OldState,I,I) = 0 then
    begin Writeln('Old state file not found');
     Exit(OdtPrq)
    end
   else
    begin Reset(StateFile,OldState);
     GetSave(StateFile,False)
    end;
  if OldState <> '' then Writeln('  Old state restored from ', OldState)
  else ReadMicro('Krnl.Bin', True);
  Writeln
 end { Initialize };
 
 
 procedure Quit;
 begin { Quit }
  Done := true;
  ChangeWindow(0);
  Write(FF)
 end { Quit };

  
begin { OdtPrq }
 Initialize;
 Done := false;
 99:
 repeat OdtCommand
 until Done;
 Close(OutFile)
end { OdtPrq }.
