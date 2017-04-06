{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module OdtDump;


{
{  copyright 1983  Three Rivers Computer Corporation
{
{-------------------------------------------------------------------
Change Log:

  13 Dec 82 V0.1  C. Beckett Fix for "Heap" field of SIT.
                             
   2 Dec 82 V0.0  C. Beckett Added a change log to program
                             only comments added - no code.
                             
{-------------------------------------------------------------------}

exports


  imports OdtPrq from OdtPrq;
  imports OdtUtils from OdtUtils;


  procedure Dump;


private


  imports Memory from Memory;
  imports CmdParse from CmdParse;
  imports Perq_String from Perq_String;
  imports Code from Code;
  imports Screen from Screen;
{$ifc MPOS then}
  imports PrcMsgDump from PrcMsgDump;
  imports ProcessDefs from ProcessDefs;
  imports Message from Message;
{$endc}


type StackOption = (StackShort, StackLong, StackAsk);
const {$Include ACB.Dfs}
      {$Include RD.Dfs}



 procedure Dump;
 var Fwa, Lwa: Bit20;
     SingleCommand: Boolean;
     Cmd: Integer;
 
 
  procedure FwaLwa;
  begin { FwaLwa }
   PushInt(0); ReadNumber('First address'); Pop(Fwa);
   PushInt(0); ReadNumber('Last address'); Pop(Lwa)
  end { FwaLwa };

  
  procedure Dump( Kind: ValKind; Sign: ValSign; Base: ValBase; Size: ValSize );
  const WordsPerLine = #10;  { **must** be a power of 2 }
        CharsPerLine = #100; { **must** be a power of 2 }
  var Address, Val: Bit20;
      Digit, I, J: integer;
      Ch: packed array[0..WordsPerLine-1] of Integer;
      Form: ValForm;
      B, W: Integer;
      PrintCharacters: Boolean;
  
  
  procedure WriteCh;
  begin { WriteCh }
   if Bytes.Ch0 in [#40..#176] then Write(OutFile, Chr(Bytes.Ch0))
   else Write(OutFile, ' ');
   if Bytes.Ch1 in [#40..#176] then Write(OutFile, Chr(Bytes.Ch1))
   else Write(OutFile, ' ')
  end { WriteCh };
  
   
 begin { Dump }
   Write(OutFile, ' (');
   if Base = Character then
    begin Write(OutFile, 'characters)');
     Digit := Fwa.Lower mod WordsPerLine
    end
   else
    begin
     if Sign = UnSigned then Write(OutFile, 'un');
     Write(OutFile, 'signed ');
     if Base = Octal then
      begin Write(OutFile, 'octal '); B := 8 end
     else
      if Base = Decimal then
       begin Write(OutFile, 'decimal '); B := 10 end;
     if Size = Wrd then Write(OutFile, 'words)')
     else Write(OutFile, 'bytes)');
     Digit := Fwa.Lower mod WordsPerLine;
    end;
   PrintCharacters := (Kind = MKind) and (Size = Wrd) and (Base <> Character);
   if PrintCharacters then W := 7 else W := 8;
   Write(OutFile, ' from '); Push(Fwa); Oct(OutFile,1);
   Write(OutFile, ' to '); Push(Lwa); Oct(OutFile,1);
   Writeln(OutFile); Writeln(OutFile);
   Address := Fwa;
   Push(Address);
   Push(Lwa);
   while {Address} LE {Lwa} do
    begin Push(Address); Oct(OutFile,7); Write(OutFile,': ');  { Address }
     J := 0;
     repeat Push(Address);
      if Kind = RKind then ReadReg
      else ReadMem;
      Pop(Val); Push(Val); PopInt(Bytes.int);
      if Base = Character then WriteCh
      else
       begin Ch[J] := Bytes.int;
        J := J + 1;
        if Sign = UnSigned then
         if Size = Wrd then
          begin Push(Val); Number(OutFile, W, -B) end
         else { Byt }
          begin
           PushInt(Bytes.Byt0);
           Number(OutFile, 4, -B);
           PushInt(Bytes.Byt1);
           Number(OutFile, 4, -B)
          end
        else { Signed }
         if Size = Wrd then
          begin Push(Val); Number(OutFile, W, B) end
         else { Byt }
          begin
           PushInt(LOr( LAnd(Bytes.Byt0,#200)*#777, Bytes.Byt0 ));
           Number(OutFile, 4, B);
           PushInt(LOr( LAnd(Bytes.Byt1,#200)*#777, Bytes.Byt1 ));
           Number(OutFile, 4, B)
          end
       end;
      Push(Address); PushInt(1); Add; Pop(Address);
      Push(Address); Push(Lwa)
     until {Address} GT {Lwa} or (Address.Lower mod WordsPerLine = Digit);
     if PrintCharacters then
      begin
       while Address.Lower mod WordsPerLine <> Digit do
        begin Write(OutFile,' ':W);
         Push(Address); PushInt(1); Add; Pop(Address)
        end;
       Write(OutFile, '  ');
       for I := 0 to J-1 do
        begin Bytes.Int := Ch[I]; WriteCh end
      end;
     Writeln(OutFile);
     Push(Address);
     Push(Lwa)
    end
  end { Dump };
   
   
  procedure StackDump( Option: StackOption );
  var SS, CB, SB, RN, CS, AP, LP, DL, GP, OldCS, UPC, PC: Bit20;
      SSInt, CSInt, T: Integer;
      MyRN: Integer;
      MyRelAddr: Bit20;
      Main: boolean;
      MainSeg: Integer;
      Process: Integer;
      CallerAP: Integer;
      CallerCB: Bit20;
      CallerPC, CallerCode: Integer;
      Print: Boolean;
      
      
   procedure DumpLocals;
   var N: Integer;
   begin { DumpLocals }
    if not Main then
     begin
      Push(LP); Push(AP);
      if {LP} GE {AP} then N := 0
      else
       begin
        Push(LP); PushInt(128); Add; Push(AP);
        if {LP+128} LT {AP} then N := 128
        else
         begin Push(AP); Push(LP); Sub; PopInt(N) end
       end;
      if N > 0 then
       begin Writeln(OutFile);
        Write(OutFile,'dump of first ');
        PushInt(N);
        Int(OutFile,1);
        Write(OutFile,' locals');
        Fwa := LP;
        Push(LP); PushInt(N-1); Add; Pop(Lwa);
        Dump(MKind,Signed,Decimal,Wrd)
       end
     end
   end { DumpLocals };
   
   
   procedure DumpGlobals;
   begin { DumpGlobals }
    Push(OldCS); Push(CS);
    if {OldCS} NE {CS} then
     begin Writeln(OutFile);
      Write(OutFile,'dump of first ');
      PushInt(128);
      Int(OutFile,1);
      Write(OutFile,' globals');
      Fwa := GP;
      Push(Fwa); PushInt(127); Add; Pop(Lwa);
      Dump(MKind,Signed,Decimal,Wrd)
     end
   end { DumpGlobals };
   
   
   procedure DumpCode;
   begin { DumpCode }
    Writeln(OutFile);
    Write(OutFile,'dump of code around execution address');
    Push(UPC); PushInt(16); Sub; Pop(Fwa);
    Push(UPC); PushInt(15); Add; Pop(Lwa);
    Dump(MKind,UnSigned,Decimal,Byt)
   end { DumpCode };
  
 
  begin { StackDump }
{$ifc MPOS then}
   QProcess; PopInt(Process);
{$elsec}
   Process := 0;
{$endc}
   Writeln(OutFile);
   Writeln(OutFile);
   ReadState(Process, SS, SB, AP, GP, LP, CS, CB, RN, UPC, PC );
   Push(SB); Push(Nill);
   if {SB} EQ {Nill} then Error('Stack not resident');
   Write(OutFile, 'stack trace-back');
{$ifc MPOS then}
   Write(OutFile, ' of process ');
   QPrcName(OutFile,Process);
{$endc}
   Write(OutFile, ', stack segment ');
   Push(SS); PopInt(SSInt);
   QSegName(OutFile,Process,SSInt);
   Writeln(OutFile,'.');
   Push(AP); Push(SB); Add; Pop(AP);
   Push(GP); Push(SB); Add; Pop(GP);
   Push(LP); Push(SB); Add; Pop(LP);
   Push(RN); PushInt(#77777);
   if {RN} EQ {#77777} then { RN was trashed by pressing boot button }
    begin { try to fix RN }
     Push(AP); PushInt(ACBRS); Add; ReadMem; PushInt(0); TLate; Pop(CallerCB);
     Push(CallerCB); Push(Nill);
     if {CallerCB} NE {Nill} then
      begin
       Push(AP); PushInt(ACBRA); Add; ReadMem; PopInt(CallerPC);
       Push(CallerCB); PushInt(Shift(CallerPC-1,-1)); ReadMem;
       PopInt(CallerCode);    { get last byte of call instruction }
       if Odd(CallerPC) then MyRN := LAnd(CallerCode,#377)
       else MyRN := Shift(CallerCode,-8);
       Push(RelAddr(CB,PC,MyRN)); Push(Nill);
       if {RelAddr} NE {Nill} then
        begin PushInt(MyRN); Pop(RN) end
      end
    end;
   OldCS := Zero;
   repeat
    Push(AP); PushInt(ACBDL); Add; ReadMem; PopInt(CallerAP);
    Main := CallerAP = 0;
    Writeln(OutFile);
    Writeln(OutFile);
    Write(OutFile,'----------  routine '); Push(RN); UInt(OutFile,1);
    Write(OutFile,' in segment ');
    Push(CS); PopInt(CSInt);
    QSegName(OutFile, Process,CSInt);
    Push(CS); PushInt(0); TLate; Pop(CB);
    Write(OutFile,' at ');
    MyRelAddr := RelAddr(CB,PC,RN.Lower);
    Push(MyRelAddr); Push(Nill);
    if {MyRelAddr} EQ {Nill} then Write(OutFile, 'unknown address')
    else
     begin
      Write(OutFile, 'address ');
      Push(MyRelAddr);
      UInt(OutFile,1)
     end;
    {UPC := PC div 2 + CB}
    UPC.Lower := Shift((PC.Lower + Shift(LAnd(PC.Upper,1),10)),-1);
    UPC.Upper := Shift(PC.Upper,-1);
    Push(CB); Push(UPC); Add; Pop(UPC);
    Write(OutFile,' ('); Push(UPC); Oct(OutFile,1);
    Writeln(OutFile,')  ----------');
    case Option of
     StackLong:  Print := True;
     StackShort: Print := False;
     StackAsk:   begin
                  ReadString('Print info [N]:');
                  if CmdString = '' then Print := False
                  else
                   Print := (CmdString[1] = 'y') or (CmdString[1] = 'Y')
                 end
     end;
    if Print then
     begin
      DumpLocals;
      DumpGlobals;
      DumpCode;
      Writeln(OutFile);
      Writeln(OutFile,'    ACB       SL     LP     DL     GL     TL',
                      '     RS          RA            RR');
      Push(AP); Oct(OutFile,7); Write(OutFile, ': ');        { ACB }
      Push(AP);                      ReadMem;       Oct(OutFile,7);    { SL }
      Push(AP); PushInt(ACBLP); Add; ReadMem;       Oct(OutFile,7);    { LP }
      Push(AP); PushInt(ACBDL); Add; ReadMem;       Oct(OutFile,7);    { DL }
      Push(AP); PushInt(ACBGL); Add; ReadMem;       Oct(OutFile,7);    { GL }
      Push(AP); PushInt(ACBTL); Add; ReadMem;       Oct(OutFile,7);    { TL }
      Push(AP); PushInt(ACBRS); Add; ReadMem; Repl; Oct(OutFile,7);    { RS }
         Write(OutFile,' ('); Int(OutFile,2); Write(OutFile,')');
      Push(AP); PushInt(ACBRA); Add; ReadMem; Repl; Oct(OutFile,7);    { RA }
         Write(OutFile,' ('); Int(OutFile,4); Write(OutFile,')');
      Push(AP); PushInt(ACBRR); Add; ReadMem; Repl; Oct(OutFile,7);    { RR }
         Write(OutFile,' ('); Int(OutFile,2); Write(OutFile,')');
      Writeln(OutFile);
      Push(AP); PushInt(ACBStackSize); Add; ReadMem; PopInt(T); { EStack save }
      if (T < 0) or (T > 16) then
       begin
        Writeln(OutFile);
        Writeln(OutFile,'E-Stack save area is trash.')
       end
      else
       if T <> 0 then
        begin Writeln(OutFile);
         Write(OutFile,'dump of ');
         PushInt(T);
         Int(OutFile,1);
         Write(' words of saved E-Stack');
         Push(AP); PushInt(ACBStackSize); Add; PushInt(T); Add; Pop(Lwa);
         Push(AP); PushInt(ACBSaveStack); Add; Pop(Fwa);
         Dump(MKind,Signed,Decimal,Wrd)
        end
     end;
    OldCS := CS;
    Push(AP); PushInt(ACBDL); Add; ReadMem; Push(SB); Add; Pop(DL);
    Push(AP); PushInt(ACBGL); Add; ReadMem; Push(SB); Add; Pop(GP);
    Push(AP); PushInt(ACBRS); Add; ReadMem;                Pop(CS);
    Push(AP); PushInt(ACBRA); Add; ReadMem;                Pop(PC);
    Push(AP); PushInt(ACBRR); Add; ReadMem;                Pop(RN);
    AP := DL;
    Push(AP); PushInt(ACBLP); Add; ReadMem; Push(SB); Add; Pop(LP)
   until Main
  end { StackDump };
 
 
  procedure MemManDump;
  const MaxSeg = 511;
        MaxSATWord = (MaxSeg+1) * WordSize(SATEntry) - 1;
        MaxSITWord = (MaxSeg+1) * WordSize(SITEntry) - 1;
  type SAT = record case integer of
              1: (Mem: array[0..MaxSATWord] of Integer);
              2: (T: array[0..MaxSeg] of SATEntry)
              end;
      SIT = record case integer of
              1: (Mem: array[0..MaxSITWord] of Integer);
              2: (T: array[0..MaxSeg] of SITEntry)
              end;
  var SATPtr: ^SAT;
      SITPtr: ^SIT;
      TableSeg, S: SegmentNumber;
      Base: integer;
      Fail: boolean;
      Addr: Bit20;
      i: integer;
      SegName: String;
      L, Line: Integer;
      NextBase: Integer;
      MemoryInBlocks: Integer;
      GoodMemoryInBlocks: Boolean;
      M0, Mn: Integer;
   
   
   procedure ReadTables;
   var i: Integer;
       Base: Bit20;
   begin { ReadTables }
    for i := 0 to MaxSATWord do
     begin PushInt(i); ReadMem; PopInt(SATPtr^.Mem[i]) end;
    PushInt(SITSeg); PushInt(0); TLate; Pop(Base);
    for i := 0 to MaxSITWord do
     begin Push(Base); PushInt(i); Add; ReadMem; PopInt(SITPtr^.Mem[i]) end
   end { ReadTables };
   
   
    procedure PrintSegment;
    var SysName: Bit20;
        W: Integer;
        Ch: Char;
        Base: Integer;
        GotSysName: Boolean;
    begin { PrintSegment }
      with SATPtr^.T[S], SITPtr^.T[S] do
        begin
          if (Line mod 5 = 0) and (Line <> 0) then Writeln(OutFile);
          Line := Line + 1;
          if not (InUse and NotResident) then { describes memory }
            begin
              Base := Shift(BaseUpper,8) + BaseLower;
              if Base < NextBase then
                Writeln(OutFile, '  -- overlapping segments --')
              else
                if Base > NextBase then
                  Writeln(OutFile, '  -- missing segment --');
              NextBase := Base + Size + 1
            end;
          PushInt(S);
          Int(OutFile,4);
          Write(OutFile, ':');
          SegName := '';
          GotSysName := False;
          if InUse then
            if BootLoaded then
              begin
{$ifc MPOS then}
                PushInt(SysNSNumber);
{$elsec}
                PushInt(SysNameSeg);
{$endc}
                PushInt(S * ((SysSegLength+1) div 2));
                TLate; Pop(SysName);
                Push(SysName);
                Push(Nill);
                if {SysName} EQ {Nill} then SegName := ''
                else
                  begin
                    for L := 1 to SysSegLength do
                      begin
                        Push(SysName);
                        ReadMem;
                        PopInt(W);
                        if Odd(L) then Ch := Chr(LAnd(W,#377))
                        else
                          begin
                            Ch := Chr(Shift(W,-8));
                            Push(SysName);
                            PushInt(1);
                            Add;
                            Pop(SysName)
                          end;
                        if Ch <> ' ' then AppendChar(SegName,Ch)
                      end;
                    GotSysName := True
                  end;
                if SegName = '' then
                 if Kind = CodeSegment then SegName := '(SyCode)'
                 else SegName := '(SyData)'
              end
            else
              if Kind = CodeSegment then SegName := '(Code)'
              else SegName := '(Data)';
          if (S > 0) and (S <= MaxQSeg) then
            begin
{$ifc MPOS then}
              if GotSysName then
                if QSeg[S] = '' then
                  QSeg[S] := SegName;
              if (SegName = '(SyCode)') or (SegName = '(SyData)') then
                if QSeg[S] <> '' then
                  SegName := QSeg[S]
{$elsec}
              if GotSysName then
                if QSeg[S].Name = '' then
                  QSeg[S].Name := SegName;
              if (SegName = '(SyCode)') or (SegName = '(SyData)') then
                if QSeg[S].Name <> '' then
                  SegName := QSeg[S].Name
{$endc}
            end;
          if Length(SegName) > 8 then SegName := SubStr(SegName,1,8);
          Write(OutFile, SegName, ' ':10 - Length(SegName));
          if InUse and NotResident then { swapped out }
            begin
              Write(OutFile, ' ':5);
              PushInt(Size + 1);
              Int(OutFile,4);
              Write(OutFile, ' ':5)
            end
          else
            begin
              PushInt(Base);
              Int(OutFile,4);
              Write(OutFile, '+');
              PushInt(Size + 1);
              Int(OutFile,4);
              Write(OutFile, '=');
              PushInt(NextBase);
              Int(OutFile,4)
            end;
          PushInt(Increment + 1);
          Int(OutFile,5);
          PushInt(Maximum + 1);
          Int(OutFile,5);
          PushInt(RefCount);
          Int(OutFile,5);
          PushInt(IOCount);
          Int(OutFile,5);
          PushInt(Freelist);
          PushInt(Freelist);
          Add;
          Int(OutFile,10);
          Write(OutFile, ' ');
          if Kind = CodeSegment then Write(OutFile, ' C')
          else Write(OutFile, ' D');
          case Mobility of
            Swappable:     Write(OutFile, ' SW');
            LessSwappable: Write(OutFile, ' LS');
            UnSwappable:   Write(OutFile, ' US');
            UnMovable:     Write(OutFile, ' UM')
            end;
          if not NotResident then Write(OutFile, ' RS');
          if Moving then Write(OutFile, ' MV');
          if RecentlyUsed then Write(OutFile, ' RU');
          if Heap then Write(OutFile, ' HP');
          if Full then Write(OutFile, ' FL');
          if InUse then Write(OutFile, ' IU');
          Writeln(OutFile)
        end
    end { PrintSegment };
    
    
  begin { MemManDump }
    SetFont(Font10);
{$ifc MPOS then}
    { read MemoryInBlocks }
    PushInt(1);
    ReadMem;
    PopInt(MemoryInBlocks);
    { check to see if MemoryInBlocks is reasonable }
    GoodMemoryInBlocks :=  (MemoryInBlocks = 512) or
                           (MemoryInBlocks = 1024) or
                           (MemoryInBlocks = 2048) or
                           (MemoryInBlocks = 4096);
{$endc}
    CreateSegment(TableSeg,(WordSize(SAT)+WordSize(SIT)+255) div 256,1,100);
    New(TableSeg,1,SATPtr);
    New(TableSeg,1,SITPtr);
    ReadTables;
    Writeln(OutFile);
    Writeln(OutFile);
    Writeln(OutFile, ' Seg Name      Base Size Next  Inc  Max  Ref  I/O  FreeList  Flags');
    Writeln(OutFile, ' --- ----      ---- ---- ----  ---  ---  ---  ---  --------  -----');
{$ifc MPOS then}
    if not GoodMemoryInBlocks then
      begin
        PushInt(MemoryInBlocks);
        Int(OutFile,1);
        Writeln(' is not reasonable for memory in blocks.')
      end;
{$endc}
    for S := SATseg to MaxSeg do SATPtr^.T[S].Lost := true;
    S := SATseg;
    Fail := false;
    Line := 0;
    NextBase := 0;
    repeat
      if (S < 1) or (S > MaxSeg) then
        begin
          PushInt(S);
          Int(OutFile,4);
          Writeln(OutFile, ': segment number out of range');
          Fail := True
        end
      else
        with SATPtr^.T[S], SITPtr^.T[S] do
          begin
            if not Lost then
              begin
                PushInt(S);
                Int(OutFile,4);
                Writeln(OutFile, ': circular list failure');
                Fail := true
              end
            else
              begin
                PrintSegment;
                Lost := false;
                S := NextSeg
              end
          end
    until (S = SATseg) or Fail;
{$ifc MPOS then}
    if GoodMemoryInBlocks then
      if NextBase > MemoryInBlocks then
        Writeln(OutFile, '  -- overlapping segments --')
      else
        if NextBase < MemoryInBlocks then
          Writeln(OutFile, '  -- missing segment --');
{$endc}
    Writeln(OutFile);
    Write(OutFile, '  C  - code segment      ');
    Write(OutFile, '  D  - data segment      ');
    Writeln(OutFile);
    Write(OutFile, '  SW - swappable         ');
    Write(OutFile, '  LS - less swappable    ');
    Writeln(OutFile);
    Write(OutFile, '  US - unswappable       ');
    Write(OutFile, '  UM - unmovable         ');
    Writeln(OutFile);
    Write(OutFile, '  RS - resident          ');
    Write(OutFile, '  IU - in use            ');
    Writeln(OutFile);
    Write(OutFile, '  FL - full data seg     ');
    Write(OutFile, '  MV - moving            ');
    Writeln(OutFile);
    Write(OutFile, '  RU - recently used     ');
    Write(OutFile, '  HP - Heap segment      ');
    Writeln(OutFile);
    DecRefCount(TableSeg);
    SetFont(Font8)
  end { MemManDump };
  
  
  procedure IODump;
  const FirstUnit = 1;
        LastUnit = 18;
  var DevTab, IntTab, Addr, Seg, Off: Bit20;
      i, IntPri: integer;
      
      
   procedure ScreenDump;
   begin { ScreenDump }
    { screen is unit 17: }
    Push(DevTab); PushInt(8 * 17); Add; Pop(Addr);
    Push(Addr); ReadMem; Pop(Off);
    Push(Addr); PushInt(1); Add; ReadMem; Pop(Seg);
    Push(Seg); Push(Off); TLate; Pop(Fwa);
    Push(Fwa); PushInt(7); Add; Pop(Lwa);
    Writeln(OutFile);
    Write(OutFile,'dump of screen control block');
    Dump(MKind,UnSigned,Octal,Wrd);
    Push(Fwa); ReadMem; Pop(Off);
    Push(Fwa); PushInt(1); ReadMem; Pop(Seg);
    Push(Seg); Push(Off); TLate; Pop(Fwa);
    Push(Fwa); PushInt(11); Add; Pop(Lwa);
    Writeln(OutFile);
    Write(OutFile,'dump of video command list');
    Dump(MKind,UnSigned,Octal,Wrd)
   end { ScreenDump };
   
   
   procedure TabletDump;
   begin { TabletDump }
    { tablet is unit 7 }
    Push(DevTab); PushInt(8 * 7); Add; Pop(Addr);
    Push(Addr); ReadMem; Pop(Off);
    Push(Addr); PushInt(1); Add; ReadMem; Pop(Seg);
    Push(Seg); Push(Off); TLate; Pop(Fwa);
    Push(Fwa); PushInt(13); Add; Pop(Lwa);
    Writeln(OutFile);
    Write(OutFile,'dump of tablet buffer');
    Dump(MKind,UnSigned,Octal,Wrd)
   end { TabletDump };
   
   
   procedure CharDump( Name: string; UnitNumber: integer );
   var Length, RdPtr, WrPtr: integer;
       Bad: boolean;
       Ch: char;
       i: integer;
   begin { CharDump }
    Bad := false;
    Push(DevTab); PushInt(8 * UnitNumber); Add; Pop(Addr);
    Push(Addr); ReadMem; Pop(Off);
    Push(Addr); PushInt(1); Add; ReadMem; Pop(Seg);
    Push(Seg); Push(Off); TLate; Pop(Fwa);
    Push(Fwa); ReadMem; PopInt(Length);
       Bad := (Length < 0) or (Length > 200);
    Push(Fwa); PushInt(1); Add; ReadMem; PopInt(RdPtr);
       Bad := Bad or (RdPtr < 0) or (RdPtr >= Length-1);
    Push(Fwa); PushInt(2); Add; ReadMem; PopInt(WrPtr);
       Bad := Bad or (WrPtr < 0) or (WrPtr >= Length-1);
    Push(Fwa); PushInt(3); Add; Pop(Addr);
    Writeln(OutFile);
    if Bad then
     begin Write(OutFile,Name,' buffer is bad, dump');
      Push(Fwa); PushInt(31); Add; Pop(Lwa);
      Dump(MKind,UnSigned,Octal,Byt)
     end
    else
     if RdPtr = WrPtr then
      Writeln(OutFile,Name,' buffer is empty.')
     else
      begin Writeln(OutFile,' dump of ',Name,' buffer.');
       i := 0;
       while RdPtr <> WrPtr do
        begin
         if i mod 40 = 0 then
          begin Writeln(OutFile); Write(OutFile,'     ') end;
         i := i + 1;
         Push(Addr); PushInt(RdPtr); Add; ReadMem; PopInt(Bytes.Int);
                                                 { Pop(Bytes);  to hang }
         Ch := Chr(LOr(Bytes.Ch0,#200));
         if Bytes.Ch0 <= #177 then Write(OutFile,Ch)
         else
          begin
           SChrFunc(RNot);
           Write(OutFile,Ch);
           SChrFunc(RRpl)
          end;
         RdPtr := (RdPtr + 1) mod Length
        end;
       Writeln(OutFile)
      end
   end { CharDump };
   
   
   procedure DiskDump;
   begin { DiskDump }
    { disk is unit 1: }
    Push(DevTab); PushInt(8 * 1); Add; Pop(Addr);
    Push(Addr); ReadMem; Pop(Off);
    Push(Addr); PushInt(1); Add; ReadMem; Pop(Seg);
    Push(Seg); Push(Off); TLate; Pop(Fwa);
    Push(Fwa); PushInt(15); Add; Pop(Lwa);
    Writeln(OutFile);
    Write(OutFile,'dump of disk control block');
    Dump(MKind,UnSigned,Octal,Wrd)
   end { DiskDump };
   
 
  begin { IODump }
   Writeln(OutFile);
   Writeln(OutFile);
   Writeln(OutFile,'dump of IO tables.');
   Writeln(OutFile);
   Writeln(OutFile,'unit  name  CtlSeg CtlOff  CtlAddr  Blk    Mask Pri',
                '   SSN      GP  RN      SL');
   PushInt(#225); ReadReg; Pop(DevTab);
   PushInt(#21); ReadReg; Pop(IntTab);
   for i := FirstUnit to LastUnit do
    begin
     PushInt(i);
     Int(OutFile,4);
     Write(OutFile, ': ');
     Push(DevTab); PushInt(i * 8); Add; Pop(Addr);
     Push(Addr); PushInt(6); Add; ReadMem; PopInt(Bytes.Int);
     Write(OutFile,Chr(Bytes.Byt0));
     Write(OutFile,Chr(Bytes.Byt1));
     Push(Addr); PushInt(7); Add; ReadMem; PopInt(Bytes.Int);
     Write(OutFile,Chr(Bytes.Byt0));
     Write(OutFile,Chr(Bytes.Byt1));
     Push(Addr);                  ReadMem; Pop(Off);
     Push(Addr); PushInt(1); Add; ReadMem; Pop(Seg);
     Push(Seg); Int(OutFile,8); Push(Off); Oct(OutFile,7);  { CtlSeg, CtlOff }
     Push(Seg); Push(Off); TLate;
     Write(OutFile,' ('); Oct(OutFile,7); Write(OutFile,')'); { CtlAddr }
     Push(Addr); PushInt(2); Add; ReadMem; Int(OutFile,4);  { BlkSize }
     Push(Addr); PushInt(3); Add; ReadMem; Oct(OutFile,8);  { IntrMask }
     Push(Addr); PushInt(4); Add; ReadMem; Repl; PopInt(IntPri);
                                           Int(OutFile,4);  { IntrPriority }
     Push(IntTab); PushInt(IntPri * 4); Add; Pop(Addr);
     Push(Addr);                  ReadMem; Int(OutFile,6);  { SSN }
     Push(Addr); PushInt(1); Add; ReadMem; Oct(OutFile,8);  { GP }
     Push(Addr); PushInt(2); Add; ReadMem; Int(OutFile,4);  { RN }
     Push(Addr); PushInt(3); Add; ReadMem; Oct(OutFile,8);  { SL }
     Writeln(OutFile)
    end;
   ScreenDump;
   TabletDump;
   CharDump('raw keyboard',8);
   CharDump('translated keyboard',16);
   CharDump('RS-232 input',9);
   CharDump('RS-232 output',10);
   DiskDump
  end { IODump };
  
  
{$ifc MPOS then}
  procedure ProcessDump;
  const PrcLength = PrcMaxProcesses*WordSize(PCB);
  type PrcA = array[0..PrcLength-1] of Integer;
  var PrcP: ^PrcA;
      S: SegmentNumber;
   
   
   procedure ReadTable;
   var PB: Bit20;
       i: Integer;
   begin { ReadTable }
    PushInt(PrcSNumber); PushInt(0); TLate; Pop(PB);
    for i := 0 to PrcLength-1 do
     begin Push(PB); PushInt(i); Add; ReadMem; PopInt(PrcP^[i]) end;
   end { ReadTable };


  begin { ProcessDump }
    SetFont(Font10);
    CreateSegment(S,(PrcLength+255) div 256,1,100);
    New(S,1,PrcP);
    ReadTable;
    DumpProcess(PrcP,OutFile);
    DecRefCount(S);
    SetFont(Font8)
  end { ProcessDump };


{$elsec}
  procedure ProcessDump;
  begin { ProcessDump }
    Writeln(OutFile, '** Multiple processes not implemented in POS.')
  end { ProcessDump };
{$endc}
 
  
{$ifc MPOS then}
  procedure PostOfficeDump;
  const MsgLength = MsgMaxMailBox*WordSize(MsgMailBox);
  type MsgA = array[0..MsgLength-1] of Integer;
  var MsgP: ^MsgA;
      S: SegmentNumber;
   
   
   procedure ReadTable;
   var MB: Bit20;
       i: Integer;
   begin { ReadTable }
    PushInt(MsgSNumber); PushInt(0); TLate; Pop(MB);
    for i := 0 to MsgLength-1 do
     begin Push(MB); PushInt(i); Add; ReadMem; PopInt(MsgP^[i]) end;
   end { ReadTable };


  begin { PostOfficeDump }
    SetFont(Font10);
    CreateSegment(S,(MsgLength+255) div 256,1,100);
    New(S,1,MsgP);
    ReadTable;
    DumpPostOffice(MsgP,OutFile);
    DecRefCount(S);
    SetFont(Font8)
  end { PostOfficeDump };
     
  
{$elsec}
  procedure PostOfficeDump;
  begin { PostOfficeDump }
    Writeln(OutFile, '** Multiple processes not implemented in POS.')
  end { PostOfficeDump };
{$endc}


  procedure CrashDump;
  var UPC, TP, AP, GP, LP, CB, SB, RN, CS, SS, SL, UserIntr,
      ioSeg, ioOffset, ioDevTab, ioIntTab, Z80Chr, Z80State, Z80Status,
      Z80WantOutput, Z80OState: Bit20;
   
   
   procedure Registers;
  
   
    procedure Reg( Name: String; Num: integer; var Val: Bit20; Base: Bit20 );
    begin { Reg }
     PushInt(Num); ReadReg; Pop(Val);
     Write(OutFile,Name:15,': '); Push(Val); Oct(OutFile,7);
     Push(Base); PushInt(0);
     if {Base} NE {Zero} then
      begin Push(Val); Push(Base); Sub; Oct(OutFile,8) end
     else Write(OutFile,' ':8);
     Push(Val); Int(OutFile,9);
     Writeln(OutFile)
    end { Reg };
   
   
   begin { Registers }
    Writeln(OutFile);
    Writeln(OutFile,'interesting registers.');
    Writeln(OutFile);
    Writeln(OutFile,' ':15,'    octal  offset  decimal');
    Writeln(OutFile);
    PushInt(7); ReadReg; Pop(SB);
    PushInt(6); ReadReg; Pop(CB);
    Reg('UPC',#16,UPC,CB);
    Reg('TP',#17,TP,SB);
    Reg('AP',3,AP,SB);
    Reg('GP',4,GP,SB);
    Reg('LP',5,LP,SB);
    Reg('CB',6,CB,Zero);
    Reg('SB',7,SB,Zero);
    Reg('RN',#10,RN,Zero);
    Reg('CS',#11,CS,Zero);
    Reg('SS',#12,SS,Zero);
    Reg('SL',#13,SL,SB);
    Writeln(OutFile);
    Reg('UserIntr',#20,UserIntr,Zero);
    Reg('ioSeg',#211,ioSeg,Zero);
    Reg('ioOffset',#212,ioOffset,Zero);
    Reg('ioDevTab',#225,ioDevTab,Zero);
    Reg('ioIntTab',#21,ioIntTab,Zero);
    Reg('Z80Chr',#226,Z80Chr,Zero);
    Reg('Z80State',#227,Z80State,Zero);
    Reg('Z80Status',#233,Z80Status,Zero);
    Reg('Z80WantOutput',#234,Z80WantOutput,Zero);
    Reg('Z80OState',#241,Z80OState,Zero);
  end { Registers };
 
   
  begin { CrashDump }
   Writeln(OutFile);
   Writeln(OutFile);
   Writeln(OutFile,'                        Perq crash dump.');
   Registers;
   MemManDump;
   IODump;
{$ifc MPOS then}
   ProcessDump;
   PostOfficeDump;
{$endc}
   StackDump(StackLong);
  end { CrashDump };
  
  
  procedure MemoryDump;
  var Form: ValForm;
  begin { MemoryDump }
   FwaLwa;
   NumericForm(Form);
   Writeln(OutFile);
   Writeln(OutFile);
   Write(OutFile,'Memory dump');
   with Form do Dump(MKind,Sign,Base,Size)
  end { MemoryDump };
  
  
  procedure RegisterDump;
   var Form: ValForm;
  begin { RegisterDump }
    FwaLwa;
    NumericForm(Form);
    Writeln(OutFile);
    Writeln(OutFile);
    Write(OutFile,'Register dump');
    with Form do Dump(RKind,Sign,Base,Size)
  end { RegisterDump };
  
  
  procedure Help;
  begin { Help }
   Writeln(OutFile);
   Writeln(OutFile,' OdtPrq Dump subsystem');
   Writeln(OutFile);
   Writeln(OutFile, '  Help        print Dump subsystem help.');
   Writeln(OutFile, '  Quit        leave Dump subsystem.');
   Writeln(OutFile, '  All         print all dumps.');
   Writeln(OutFile, '  Memory      dump an area of memory.');
   Writeln(OutFile, '  Registers   dump an area of the XY register file.');
   Writeln(OutFile, '  Stack       do a memory stack trace back.');
   Writeln(OutFile, '  Trace       do a brief memory stack trace back.');
   Writeln(OutFile, '  MTables     dump the memory manager tables.');
   Writeln(OutFile, '  IOTables    dump the I/O tables.');
{$ifc MPOS then}
   Writeln(OutFile, '  PostOffice  dump the message tables.');
   Writeln(OutFile, '  Process     dump the process tables.');
{$endc}
   Writeln(OutFile)
  end { Help };
 
 
 begin { Dump }
  SingleCommand := InInFile or (CmdLine <> '');
  repeat
   SetFont(Font8);
   ReadString('Dump>');
   if Length(CmdString) <> 0 then
    begin
     ConvUpper(CmdString);
     Cmd := UniqueCmdIndex(CmdString, DumpCommands, DumpNumCmds);
     case Cmd of
      DumpHelp       : Help;
      DumpQuit       : ;
      DumpAll        : CrashDump;
      DumpMemory     : MemoryDump;
      DumpRegisters  : RegisterDump;
      DumpStack      : StackDump(StackAsk);
      DumpTrace      : StackDump(StackShort);
      DumpMTables    : MemManDump;
      DumpIOTables   : IODump;
      DumpPstOffice  : PostOfficeDump;
      DumpPrcess     : ProcessDump;
      DumpNotFound   : begin
                        Writeln(OutFile, 'Unknown Dump command: ', CmdString,
                                         ', type Help if you need it.');
                        CmdLine := ''
                       end;
      DumpNotUnique  : begin
                        Writeln(OutFile, 'Non-unique Dump command: ', CmdString,
                                         ', type Help if you need it.');
                        CmdLine := ''
                       end;
     end
    end
  until SingleCommand or (Cmd = DumpQuit)
 end { Dump }.
