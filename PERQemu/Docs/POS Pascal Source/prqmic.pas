{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{$R-}
program PrqMic;


imports System from System;
imports Perq_String from Perq_String;
imports CmdParse from CmdParse;
imports MicroOption from MicroOption;
imports UtilProgress from UtilProgress;
imports FileSystem from FileSystem;


{ PERQ Micro Assembler 
  Brian Rosen      26-Dec-79
  John P. Strait   26-Jan-80    largely re-written
  Three Rivers Computer Corporation
  Copyright (C) 1980, 1981, 1982, 1983 Three Rivers Computer Corp,
  Pittsburgh, PA
  
  Change log

  V3.1  JPS   3 May 83  Fix bugs in constant expression evaluation:
                        1) Unary minus was being applied to simple expressions
                            rather than terms, thus  -1+2  evaluated as  -3.
                        2) Factor was requiring parenthesized expressions to
                            be simple expressions rather than allowing full
                            expressions, thus  (a xor b) + 1  was not allowed.

  V3.0  JPS   1 Dec 82  Fix errors caused by 14-character uniqueness.

  V2.9  JPS  20 Oct 81  1) Fix bug in ":= Victim" and ":= MQ".
                        2) Look for source file before resetting it.
                        3) Get rid of old Alu code.
  
  V2.8  JPS   7 Oct 81  Fix bug in current bank processing.
  
  V2.7  JPS   6 Oct 81  Add base register support for Perq1A.
  
  V2.6  JPS   1 Oct 81  1) Allow negative shift count to Rotate to mean
                           left rotate.
                        2) Tighten up processing of NextInst, Vector,
                           and Dispatch.
                        3) Add Interrupt(Addr,Number) like Case except
                           used for Vector targets.
                        4) Extend OpCode to allow OpCode(Addr,Number)
                           in addition to OpCode(Number).
  
  V2.5  JPS   1 Oct 81  Add operator precedence for constant expressions.
  
  V2.4  JPS  30 Sep 81  1) Add Victim for Perq1A. 
                        2) Add push long constant for Perq1A.
  
  V2.3  JPS  29 Sep 81  1) Add constant expressions.
                        2) Write cross reference info to a file.
  
  V2.2  JPS  29 Sep 81  1) Add Goto(Shift) for Perq1A.
                        2) Add MQ, MultiplyStep, DivideStep for Perq1A.
  
  V2.1  JPS  28 Sep 81  1) Teach PrqMic about the 16K WCS for Perq1A.
                        2) Use UtilProgress.
  
  V2.0  JPS  28 Sep 81  Begin changes for Perq1A:
                        1) Add $Perq1 and $Perq1A assembler options.
                        2) Add $Base and $NoBase assembler options.
                        3) Disallow LatchMA in Perq1 mode.
                        4) Do away with the PDP-11 version of PrqMic.
  
  V1.8  JPS   4 Jun 81  Rename "*PrqMic.Error" to "PrqMic.Error".

  V1.7  JPS  20 Feb 81  Convert to system C.3.
  
  V1.6  JPS  30 Jan 81  Make AMux select default to register rather than
                        Shift.  This fixes a hardware problem in Raster-Op.
  
  V1.5  JPS   6 Jan 81  1) Add AMux and BMux as binary operators.
                        2) Fix the automatic "if BPC[3] Goto(Refill)" to
                           allow the programmer to override.
                        3) Check for multiple Goto's to different labels
                           in a single micro-instruction.
                        4) Add $NoList and $List.
  
  V1.4  JPS  20 Nov 80  Allow octal numbers of the form #nnnnn for
                        compatibility with Pascal.
  
  V1.3  JPS  20 Nov 80  1) Allow Pascal style comments.
                        2) Implement assembler options:
                            $Include: one level include files.
                            $Title:   set title line on listing.
                        3) Implement Pascal style constant definitions:
                            <name> = <value>  .
                        4) Accept tabs and form-feeds as space like chars.
  
  V1.2  JPS  22 Oct 80  Fix automatic addition of 'Hold' on ReviveVictim and
                        Dispatch jumps.  This mod was made earlier and lost.
  
  V1.1  JPS  18 Sep 80  Add version number.
}


const PrqMicVersion = '3.1';
      NameMax = 9; {Length-1 of Symbol Print Names}
      UpperLower = -32;  {ORD('A') - ORD('a')}
      EndOfLine = '~'; {Replaces CR in InLine}
      HashMax = 256;  {Length -1 of Hash Table for Symbols}
      HashSize = 257; {Length of Hash Table for Symbols}
      InLineMax = 255; {Length-1 of Input Buffer}
      RefillAddress = 255; {Location on each of Pcode pages where
                              NextOpRefill is placed}
      SymMax = 100;  {maximum symbols on a line}
      
      TabCh = Chr(#010);
      FFCh  = Chr(#014);

type XYRange = 0..255;  {X and Y are 8 bit fields of MicroInstruction (MI)}
          {Virtual Addresses are in declaration order 
           Physical addresses are in placed order
             A line of source text containing a MI has a virtual address 1
             greater than the virtual address of the preceeding line, but
             may have a completely different physical address.
             Virtual addresses are used by the debugger, and by the main
             part of the assembler.  The Placer decides where each MI should
             go, and assigns the physical address. }
     VirtualAddress = 0..4095; {There is a 12 bit address}
     PhysicalAddress = 0..16383;
     
     NameRange = 0..NameMax;  {Length of an identifier}
     Name = packed array[0..NameMax] of char; {Print name of an identifier}
     InLineRange = 0..InLineMax; {Range of input line buffer}
     
     Symbol = ( { for internal use only: } LabelType, XYType, ReservedWordType,
                                           ConstType,
                { PseudoOp symbols: }
              None, DefineName, ConstName, CaseName, InterruptName, EndName,
              DecimalName, OctalName, BinaryName, PlaceName, NopName,
                { other symbols: }
              IfName, IOBname, TOSname, WCSlowName,
              WCSmidName, WCShiName, BPCname, ShiftName, NextOpName,
              MdiName, MdxName, UstateName, HoldName,
              AMuxName, BMuxName, AndName, NandName,
              OrName, NorName, XorName, XnorName, NotName, OldCarryName,
              StkResetName, PushName, PopName, IODname, 
              TrueName, FalseName, GtrName, LssName, EqlName, NeqName,
              GeqName, LeqName, OvfName, BPC3Name, A15Name, A7Name,
              A0Name, CarryName, C19Name, IPname, JZname, CallName,
              NxtInstName, RevVicName, GoToName, PushLodName,
              CallSname, VectorName, DispatchName, GoToSName, RptLpName,
              RepeatName, ReturnName, JumpPopName, LeapPopName,
              LoadSname, LoopName,
              NextName, TWBname, FetchName, StoreName, 
              Fetch4Name, Store4Name, Fetch4RName, Store4RName, 
              Fetch2Name, Store2Name, LatchMAName,
              MaName, MdoName,FieldName,LShiftName,
              RShiftName,RotateName,ShiftOnRName, OpcodeName,LocName,
              LoadOpName, CntlROname, SrcROname, DstROname, WidROname,
              MQName, MulStepName, DivStepName,
              LSHName, RSHName, RotName,
              DivName, ModName,
              VictimName, RBaseName,
              identifier, number, semicolon, comma, colon, becomes, lparen,
              rparen, plus, minus, equal, notequal, lessthan, greaterthan,
              lessorequal, greaterorequal, star, percent);
     SetofSymbols = set of Symbol;
     SymbolType = LabelType..ConstType;
     PseudoOp = None..NopName;
     
     HashCode = 0..HashMax;
     SymPointer = ^SymEntry;
     SymEntry = record                           {Entry in the Symbol Table}
                  Next: SymPointer;
                  PrintName: Name;               {Identifier}
                  case SymType: Symboltype of    {Type}
                   LabelType:        (LabelAddress: VirtualAddress;
                                      LabelDefined: boolean);
                   XYType:           (XYAddress: XYrange);
                   ReservedWordType: (WordKind: Symbol;
                                      WordValue: integer);
                   ConstType:        (ConstValue: integer)
                  end;
      RefEntry = record
                  PrintName: Name;
                  case Integer of
                   1: (RefLine: Integer);
                   2: (Page: Integer;
                       Line: Integer)
                  end;
            
            {The following enumerations are for the various fields of a MI}
      Fields = (Xf,Yf,Af,Bf,Wf,Hf,ALUf,Ff,SFf,Zf,CNDf,JMPf);
      FieldSet = set of Fields;
      AMUXtype = (ShiftSrc,NextOp,IodSrc,MdiSrc,MdxSrc,UstateSrc,XSrc,EStkSrc);
      BMUXtype = (YRam, YConstant);
      ALUtype = (Aalu,Balu,NotAalu,NotBalu,AandBalu,AandNotBalu,AnandBalu,
                   AorBalu,AorNotBalu,AnorBalu,AxorBalu,AxnorBalu,
                   AplusBalu,AplusBPlusOCalu,AminusBalu,AminusBminusOCalu);
      SFusetype = (SF1,MemCntl,SF2,LJSF,NoSF);
      Zusetype = (LongConstant,SJZ,ShiftFunction,LJZ,NoZ);
      SFtype = (NopSF, ShftOnRSF, StackResetSF, 
                   TOSgetsSF, PushSF, PopSF,
                   CntlROgetsSF, SrcROgetsSF, DstROgetsSF, WidROgetsSF,
                   LoadOpSF, BPCgetsSF, 
                   WCSlowGetsSF, WCSmidGetsSF, WCShiGetsSF, IOBfunctionSF);
      Perq1ASFtype = (ReadVictimSF,MulDivSF,LoadMQSF,LoadBaseSF,ReadMQSF,
                      PushLongSF,GotoShiftSF,LeapSF);
      ShiftType = FieldName..RotateName;
      MemFuncType = (UnusedM0, UnusedM1, UnusedM2, UnusedM3, UnusedM4, UnusedM5,
                     UnusedM6, UnusedM7, Fetch4Reverse, Store4Reverse, 
                     Fetch4, Store4, Fetch2, Store2, Fetch, Store);
      CNDtype = (TrueCnd, FalseCnd, IntrPendCnd, Eql20Cnd, BPC3Cnd, C19Cnd,
                 OddCnd, ByteSignCnd,  NeqCnd, LeqCnd, LssCnd, OvrflwCnd,
                 CarryCnd, EqlCnd, GtrCnd, GeqCnd);
      JMPtype = (JumpZeroJmp,CallJmp,NextInstJmp,GotoJmp,PushLoadJmp,
                   CallSJmp, VectorJmp, GotoSJmp, RepeatLoopJmp,
                   RepeatJmp, ReturnJmp, JumpPopJmp, LoadSJmp, LoopJmp,
                   NextJmp, ThreeWayBranchJmp);
      
      MicroInstruction =   {The format of a microinstruction before placing}
                packed record {Note that this is not the field order of a real MI}
                 case Line: integer of         {Source line number}
                 -1: { machine type }
                   (MachineOption: MOption);
                  0: { placement }
                   (First, Last: integer);
                  1: { micro instruction }
                   (PosVal:  integer;          {Physical position restrictions}
                    PosMask: Integer;          {Shows significant bits in PosVal}
                    Target:  Name;             {Name of target of Jmps}
                    TargtVA: integer;          {Virtual address of target}
                    Used:    FieldSet;         {Which fields have been used}
                   {Fields of the microinstruction:}
                    X,Y:  XYRange;
                    A:    0..7   { AMUXtype };
                    B:    0..1   { BMUXtype };
                    W,H:  boolean;
                    ALU:  0..15  { ALUtype };
                    F:    integer {0..4}   { Ftype, yes, 4 is correct, not 3 };
                    Z:    0..255;
                    CND:  0..15  { CNDtype };
                    JMP:  0..15  { JMPtype };
                    SF:   0..15  { case Ftype of
                                    LongConstant:    (SF: SFtype);
                                    ShiftFunction:   (ShiftSF: SFtype);
                                    MemCntl:         (MemFuncSF: MemFuncType);
                                    LongJump:        (LJSF: 0..15);
                                  }
                   )
                end;
 
 
 var Machine: MOption;
     BaseRegister: Boolean;
     Preamble: Boolean;
     Sym: array[1..SymMax] of record
           Pos: InLineRange;
           Kind: Symbol;
           Val: integer;
           Ptr: SymPointer;
           Id: name
           end;
     S: integer;  { index into Sym }
     SymTable: Array [0..HashMax] OF SymPointer;  {Symbol table Hash Entry points}
     HashVal: 0..HashMax;  {Hash value of last symbol searched for}
     CurrentSym: SymPointer; {Pointer to current symbol}
     Radix: integer;  {Radix of numbers: 2, 8, or 10}
     InFile, IncludeFile: text;
     Include: boolean;
     CodFile: file of MicroInstruction;
     SymFile: file of SymEntry;
     RefFile: file of RefEntry;
     RootFileName, InFileName, Ignore: string;
     InLine: string[InLineMax];  {Input buffers}
     InNumber: array[1..InLineMax] of Integer;
     LineNumber: integer;
     InLineLen: InLineRange;  {length of InLine}
     Used: FieldSet;
     SFuse: SFusetype;
     Zuse: Zusetype;
     Placement: boolean;             {have a Place pseudo-op}
     GeneratedInstruction: boolean;  {this line generates code}
     Dot: VirtualAddress;  {Current Virtual Address, incremented at each MI}
     CurMI: MicroInstruction;  {The microinstruction being built}
     EmptyMI: MicroInstruction;  {Empty microinstruction}
     EndOfProgram: boolean;
     GenName: Name;  {for assembler generated names}
     MaxPhysAddr: Integer;
     CurrentBank: Integer;
     LineInFile, LineInMainFile: Integer;
     Result, WantResult: (NoResult, ALUResult, SpecialResult);
     StackWant: (StackNone, StackAssign, StackPush);
     StackSym: Integer;    { index into Sym }
     


 procedure Err(n,p: integer);  forward;
 procedure Error(n: integer);  forward;
 procedure EnterSym; forward;
 function SearchSym( var Target: Name; Remember: Boolean ): SymPointer;
                               forward;
 procedure GetLine; forward;


 procedure Initialize;  {Initializes the symbol table and other vars}
 var i: integer;
 begin { Initialize }
  Writeln('PrqMic ', PrqMicVersion);
  InitOption;
  Placement := false;
  EndOfProgram := false;
  Radix := 8;
  GenName := '!AAAAAAAAA';
  for i := 0 to HashMax do SymTable[i] := nil;
  with EmptyMI do
   begin
    Line := 0;
    Target := '          ';
    TargtVA := 0;
    PosVal := 0;
    PosMask := 0;
    Used := [];
    X := 0;
    Y := 0;
    A := Ord(XSrc);
    B := Ord(YRam);
    W := false;
    H := false;
    ALU := Ord(Aalu);
    F := Ord(LongConstant);
    SF := Ord(NopSF);
    Z := 0;
    CND := Ord(TrueCnd);
    JMP := Ord(JumpZeroJmp)
   end;
  Dot := 0; {Start at Virtual 0}
  Radix := 8;  {Assume octal numbers}
  LineNumber := 0;
  LineInFile := 0;
  Preamble := true;
  Machine := OPerq1;
  CurrentBank := -1;
  LoadCurs;
 end { Initialize };


 procedure InitReservedWords;
 
 
  procedure InitSpecialName( N: Name; S: Symbol; V: integer );
   {Initialize a reserved word by entering it in the symbol table}
  var i: integer;
  begin { InitSpecialName }
   for i := 0 to NameMax do
    if (N[i] >= 'a') and (N[i] <= 'z') then
     N[i] := Chr(Ord(N[i]) + UpperLower);
   New(CurrentSym,ReservedWordType);  
   CurrentSym^.PrintName := N;
   CurrentSym^.SymType := ReservedWordType;
   CurrentSym^.WordKind := S;
   CurrentSym^.WordValue := V;
   EnterSym;
  end { InitSpecialName };

  
 begin { InitReservedWords }
   InitSpecialName('Define    ',DefineName,0); 
   InitSpecialName('If        ',IfName,0);
   InitSpecialName('End       ',EndName,0);
   InitSpecialName('IOB       ',IOBname,Ord(IobFunctionSF));
   InitSpecialName('IOD       ',IODname,Ord(IodSrc));
   InitSpecialName('TOS       ',TOSname,Ord(EstkSrc));
   InitSpecialName('WCSlow    ',WCSlowName,Ord(WcsLowGetsSF));
   InitSpecialName('WCSmid    ',WCSmidName,Ord(WcsMidGetsSF));
   InitSpecialName('WCShi     ',WCShiName,Ord(WcsHiGetsSF));
   InitSpecialName('LoadOp    ',LoadOpName,Ord(LoadOpSF));
   InitSpecialName('BPC       ',BPCname,Ord(BpcGetsSF));
   InitSpecialName('Shift     ',ShiftName,Ord(ShiftSrc));
   InitSpecialName('NextOp    ',NextOpName,Ord(NextOp));
   InitSpecialName('MDI       ',MdiName,Ord(MdiSrc));
   InitSpecialName('MDX       ',MdxName,Ord(MdxSrc));
   InitSpecialName('Ustate    ',UstateName,Ord(UstateSrc));
   InitSpecialName('Hold      ',HoldName,0);
   InitSpecialName('AMux      ',AMuxName,Ord(Aalu));
   InitSpecialName('BMux      ',BMuxName,Ord(Balu));
   InitSpecialName('and       ',AndName,Ord(AandBalu));
   InitSpecialName('nand      ',NandName,Ord(AnandBalu));
   InitSpecialName('or        ',OrName,Ord(AorBalu));
   InitSpecialName('nor       ',NorName,Ord(AnorBalu));
   InitSpecialName('Xor       ',XorName,Ord(AxorBalu));
   InitSpecialName('Xnor      ',XnorName,Ord(AxnorBalu));
   InitSpecialName('not       ',NotName,Ord(NotAalu));
   InitSpecialName('OldCarry  ',OldCarryName,0);
   InitSpecialName('StackReset',StkResetName,Ord(StackResetSF));
   InitSpecialName('Push      ',PushName,Ord(PushSF));
   InitSpecialName('Pop       ',PopName,Ord(PopSF));
   InitSpecialName('True      ',TrueName,Ord(TrueCnd));
   InitSpecialName('False     ',FalseName,Ord(FalseCnd));
   InitSpecialName('Gtr       ',GtrName,Ord(GtrCnd));
   InitSpecialName('Lss       ',LssName,Ord(LssCnd));
   InitSpecialName('Eql       ',EqlName,Ord(EqlCnd));
   InitSpecialName('Neq       ',NeqName,Ord(NeqCnd));
   InitSpecialName('Geq       ',GeqName,Ord(GeqCnd));
   InitSpecialName('Leq       ',LeqName,Ord(LeqCnd));
   InitSpecialName('Overflow  ',OvfName,Ord(OvrFlwCnd));
   InitSpecialName('BPC[3]    ',BPC3Name,Ord(Bpc3Cnd));
   InitSpecialName('Eql20     ',A15Name,Ord(Eql20Cnd));
   InitSpecialName('ByteSign  ',A7Name,Ord(ByteSignCnd));
   InitSpecialName('Odd       ',A0Name,Ord(OddCnd));
   InitSpecialName('Carry     ',CarryName,Ord(CarryCnd));
   InitSpecialName('C19       ',C19Name,Ord(C19Cnd));
   InitSpecialName('IntrPend  ',IPname,Ord(IntrPendCnd));
   InitSpecialName('JumpZero  ',JZname,Ord(JumpZeroJmp));
   InitSpecialName('Call      ',CallName,Ord(CallJmp));
   InitSpecialName('NextInst  ',NxtInstName,Ord(NextInstJmp));
   InitSpecialName('ReviveVict',RevVicName,Ord(NextInstJmp));
   InitSpecialName('GoTo      ',GoToName,Ord(GotoJmp));
   InitSpecialName('PushLoad  ',PushLodName,Ord(PushLoadJmp));
   InitSpecialName('CallS     ',CallSname,Ord(CallSJmp));
   InitSpecialName('Vector    ',VectorName,Ord(VectorJmp));
   InitSpecialName('Dispatch  ',DispatchName,Ord(VectorJmp));
   InitSpecialName('GoToS     ',GoToSname,Ord(GotoSJmp));
   InitSpecialName('RepeatLoop',RptLpName,Ord(RepeatLoopJmp));
   InitSpecialName('Repeat    ',RepeatName,Ord(RepeatJmp));
   InitSpecialName('Return    ',ReturnName,Ord(ReturnJmp));
   InitSpecialName('JumpPop   ',JumpPopName,Ord(JumpPopJmp));
   InitSpecialName('LoadS     ',LoadSname,Ord(LoadSJmp));
   InitSpecialName('Loop      ',LoopName,Ord(LoopJmp));
   InitSpecialName('Next      ',NextName,Ord(NextJmp));
   InitSpecialName('ThreeWayBr',TWBname,Ord(ThreeWayBranchJmp));
   InitSpecialName('Constant  ',ConstName,0);
   InitSpecialName('Case      ',CaseName,0);
   InitSpecialName('Interrupt ',InterruptName,0);
   InitSpecialName('Fetch     ',FetchName,Ord(Fetch));
   InitSpecialName('Store     ',StoreName,Ord(Store));
   InitSpecialName('Fetch2    ',Fetch2Name,Ord(Fetch2));
   InitSpecialName('Store2    ',Store2Name,Ord(Store2));
   InitSpecialName('Fetch4    ',Fetch4Name,Ord(Fetch4));
   InitSpecialName('Store4    ',Store4Name,Ord(Store4));
   InitSpecialName('Fetch4R   ',Fetch4RName,Ord(Fetch4Reverse));
   InitSpecialName('Store4R   ',Store4RName,Ord(Store4Reverse));
   InitSpecialName('MA        ',MaName,0);
   InitSpecialName('MDO       ',MdoName,0);
   InitSpecialName('Field     ',FieldName,Ord(ShiftFunction));
   InitSpecialName('LeftShift ',LShiftName,Ord(ShiftFunction));
   InitSpecialName('RightShift',RShiftName,Ord(ShiftFunction));
   InitSpecialName('Rotate    ',RotateName,Ord(ShiftFunction));
   InitSpecialName('ShiftOnR  ',ShiftOnRName,Ord(ShftOnRSF));
   InitSpecialName('Opcode    ',OpcodeName,0);
   InitSpecialName('Loc       ',LocName,0);
   InitSpecialName('CntlRaster',CntlROname,Ord(CntlRoGetsSF));
   InitSpecialName('SrcRasterO',SrcROname,Ord(SrcRoGetsSF));
   InitSpecialName('DstRasterO',DstROname,Ord(DstRoGetsSF));
   InitSpecialName('WidRasterO',WidROname,Ord(WidRoGetsSF));
   InitSpecialName('Decimal   ',DecimalName,0);
   InitSpecialName('Octal     ',OctalName,0);
   InitSpecialName('Binary    ',BinaryName,0);
   InitSpecialName('Place     ',PlaceName,0);
   InitSpecialName('Nop       ',NopName,0);
   InitSpecialName('LSH       ',LSHName,0);
   InitSpecialName('RSH       ',RSHName,0);
   InitSpecialName('ROT       ',ROTName,0);
   InitSpecialName('Div       ',DivName,0);
   InitSpecialName('Mod       ',ModName,0);
   if Machine = OPerq1 then
     begin
       InitSpecialName('LatchMA   ',LatchMAName,Ord(UnusedM0));
     end;
   if Machine = OPerq1A then
     begin
      InitSpecialName('LeapPop   ',LeapPopName,Ord(JumpPopJmp));
      InitSpecialName('MQ        ',MQName,Ord(ReadMQSF));
      InitSpecialName('MultiplySt',MulStepName,Ord(MulDivSF));
      InitSpecialName('DivideStep',DivStepName,Ord(MulDivSF));
      InitSpecialName('Victim    ',VictimName,Ord(ReadVictimSF));
      InitSpecialName('RBase     ',RBaseName,Ord(LoadBaseSF));
     end
 end { InitReservedWords };
  
 
 procedure GetLine;
 var K: Integer;
  
  
  procedure Option( var F: Text );
  var Word: String;
      O: MOption;
   
   
   procedure Advance;
   begin { Advance }
    InLineLen := InLineLen + 1;
    InLine[0] := Chr(InLineLen);
    InLine[InLineLen] := F^;
    Get(F)
   end { Advance };
   
   
   procedure EndLine;
   begin { EndLine }
    while not Eoln(F) do Advance
   end { EndLine };
   
   
   procedure SkipBlanks;
   begin { SkipBlanks }
    while (F^ in [' ', TabCh, FFCh]) and not Eoln(F) do Advance
   end { SkipBlanks };
   
   
   procedure ReadWord;
   var Len: Integer;
   begin { ReadWord }
    Len := 0;
    Word[0] := Chr(80);
    while not (F^ in [' ', TabCh, FFCh]) do
     begin Len := Len + 1;
      Word[Len] := F^;
      Advance
     end;
    Word[0] := Chr(Len)
   end { ReadWord };
   
   
  begin { Option }
   Advance;  { skip '$' }
   SkipBlanks;
   ReadWord;
   O := MicroOption(Word);
   case O of
    OInclude: begin SkipBlanks;
               ReadWord;
               EndLine;
               if Include then Err(55,1)
               else
                begin Writeln;
                 Writeln('Reading ', Word);
                 Reset(IncludeFile,Word);
                 LineInMainFile := LineInFile;
                 LineInFile := 0
                end;
               Include := True
              end;
    OTitle,
    ONoList,
    OList:    { leave listing options alone for the Placer } EndLine;
    OPerq1,
    OPerq1A:  begin
               EndLine;
               if Preamble then
                begin
                 Machine := O;
                 Preamble := False
                end
               else Err(58,1)
              end;
    OBase,
    ONoBase:  begin
               EndLine;
               if Machine = OPerq1 then Err(54,1);
               if Machine = OPerq1A then BaseRegister := O = OBase
              end;
    OUnknown: begin
               EndLine;
               Err(54,1)
              end
    end;
   Readln(F);
   LineNumber := LineNumber + 1;
   InLineLen := 0   { to prevent this line from being assembled }
  end { Option };
  
  
  procedure ReadLine( var F: Text );
  var State: (Scan, InComment, LastScan, LastInComment);
      Comment: (None, NeedBrace, NeedAsterisk, NeedParen, NeedEoln);
      Ch: Char;
      Eol: Boolean;
  begin { ReadLine }
   State := Scan;
   Comment := None;
   Eol := True;
   InLine[0] := Chr(InLineMax);
   InLineLen := 0;
   InNumber[1] := LineInFile;
   repeat
    if Eol then LineInFile := LineInFile + 1;
    if not (Eof(F) or (Eol and (F^ = '$'))) then
     begin
      Eol := Eoln(F);
      if Eol then
       begin
        LineNumber := LineNumber + 1;
        StreamProgress(F)
       end;
      Ch := F^;
      Get(F);
      case State of
       Scan,
       LastScan:
        if Ch = '!' then
         begin State := Succ(State); Comment := NeedEoln end
        else
         if Ch = '{' then
          begin State := Succ(State); Comment := NeedBrace end
         else
          if (Ch = '(') and (F^ = '*') then
           begin Get(F); State := Succ(State); Comment := NeedAsterisk end
          else
           begin InLineLen := InLineLen + 1;
            InLine[InLineLen] := Ch;
            InNumber[InLineLen] := LineInFile;
            if Ch in [' ', TabCh, FFCh] then
             begin InLine[InLineLen] := ' ';
              InNumber[InLineLen] := LineInFile;
              if InLineLen = 1 then { ignore leading blanks }
               InLineLen := 0;
              while (F^ in [' ', TabCh, FFCh]) and not Eoln(F) do Get(F)
             end
            else
             if Ch = ';' then State := LastScan
           end;
       InComment,
       LastInComment:
        case Comment of
         NeedBrace:
          if Ch = '}' then State := Pred(State);
         NeedAsterisk:
          if Ch = '*' then Comment := NeedParen;
         NeedParen:
          if Ch = ')' then State := Pred(State)
          else
           if Ch <> '*' then Comment := NeedAsterisk;
         NeedEoln:
          if Eol then State := Pred(State)
         end
       end
     end
   until Eof(F) or (Eol and ((State = LastScan) or (F^ = '$')));
   if (State <> LastScan) and (InLineLen <> 0) then
    begin InLineLen := InLineLen + 1;
     InLine[InLineLen] := ';';
     InNumber[InLineLen] := LineInFile
    end;
   InLine[InLineLen+1] := '~';
   InNumber[InLineLen+1] := LineInFile;
   InLine[0] := Chr(InLineLen);
   if (State <> LastScan) and (InLineLen <> 0) then Err(57,1)
  end { ReadLine };
 
 
 begin { GetLine }
  InLineLen := 0;
  repeat
   if Include then
    if Eof(IncludeFile) then
     begin Close(IncludeFile);
      Include := False;
      Writeln;
      Writeln('Reading ', InFileName);
      LineInFile := LineInMainFile
     end
    else
     begin ReadLine(IncludeFile);
      if (InLineLen = 0) and (IncludeFile^ = '$') then Option(IncludeFile)
     end
   else
    if Eof(InFile) then
     begin InLine := 'End;~';
      InLineLen := 4;
      InLine[0] := Chr(InLineLen);
      for K := 1 to 5 do InNumber[K] := LineInFile;
      Err(36,1);
     end
    else
     begin ReadLine(InFile);
      if (InLineLen = 0) and (InFile^ = '$') then Option(InFile)
     end
  until InLineLen <> 0
 end { GetLine };
 
  
 procedure Instruction;  {The main work horse of the assembler}
 var SyKind: Symbol;
     
     
  procedure Skip(Fsys: SetofSymbols);
  begin { Skip }
   while not (Sym[S].Kind in Fsys) do S := S + 1
  end { Skip };
  
  
  procedure NeedX(X: integer);
  begin { NeedX }
   if Xf in Used then
    if CurMI.X <> X then Error(Ord(Xf));
   CurMI.X := X;
   Used := Used + [Xf]
  end { NeedX };
  
  
  procedure NeedY(Y: integer);
  begin { NeedY }
   if Yf in Used then
    if CurMI.Y <> Y then Error(Ord(Yf));
   CurMI.Y := Y;
   Used := Used + [Yf]
  end { NeedY };
  
  
  procedure NeedA(A: integer);
  begin { NeedA }
   if Af in Used then
    if CurMI.A <> A then Error(Ord(Af));
   CurMI.A := A;
   Used := Used + [Af]
  end { NeedA };
  
  
  procedure NeedB(B: integer);
  begin { NeedB }
   if Bf in Used then
    if CurMI.B <> B then Error(Ord(Bf));
   CurMI.B := B;
   Used := Used + [Bf]
  end { NeedB };
  
  
  procedure NeedW(W: boolean);
  begin { NeedW }
   if Wf in Used then
    if CurMI.W <> W then Error(Ord(Wf));
   CurMI.W := W;
   Used := Used + [Wf]
  end { NeedW };
  
  
  procedure NeedH(H: boolean);
  begin { NeedH }
   if Hf in Used then
    if CurMI.H <> H then Error(Ord(Hf));
   CurMI.H := H;
   Used := Used + [Hf]
  end { NeedH };
  
  
  procedure NeedALU(ALU: integer);
  begin { NeedALU }
   if ALUf in Used then
    if CurMI.ALU <> ALU then Error(Ord(ALUf));
   CurMI.ALU := ALU;
   Used := Used + [ALUf]
  end { NeedALU };
  
  
  procedure NeedZ(Z: integer; F: Zusetype);
  begin { NeedZ }
   if F <> NoZ then
    begin
     if Zuse <> NoZ then
      if Zuse <> F then Error(Ord(Ff));
     if SFuse <> NoSF then
      if (Ord(F) <> Ord(SFuse)) and
         ((F <> ShiftFunction) or (SFuse <> SF1)) then Error(Ord(Ff));
     CurMI.F := Ord(F);
     Used := Used + [Ff]
    end;
   Zuse := F;
   if Zf in Used then
    if CurMI.Z <> Z then Error(Ord(Zf));
   CurMI.Z := Z;
   Used := Used + [Zf]
  end { NeedZ };
  
  
  procedure NeedCND(CND: integer);
  begin { NeedCND }
   if CNDf in Used then
    if CurMI.CND <> CND then Error(Ord(CNDf));
   CurMI.CND := CND;
   Used := Used + [CNDf]
  end { NeedCND };
  
  
  procedure NeedJMP(JMP: integer);
  begin { NeedJMP }
   if JMPf in Used then Error(Ord(JMPf));
   CurMI.JMP := JMP;
   Used := Used + [JMPf]
  end { NeedJMP };
  
  
  procedure NeedSF(SF: integer; F: SFusetype);
  begin { NeedSF }
   if F <> NoSF then
    begin
     if SFuse <> NoSF then
      if SFuse <> F then Error(Ord(Ff));
     if Zuse <> NoZ then
      if Ord(F) <> Ord(Zuse) then
       if (F = SF1) and (Zuse = ShiftFunction) then F := SF2
       else Error(Ord(Ff));
     CurMI.F := Ord(F);
     Used := Used + [Ff]
    end;
   SFuse := F;
   if SFf in Used then
    if SF <> CurMI.SF then Error(Ord(SFf));
   CurMI.SF := SF;
   Used := Used + [SFf]
  end { NeedSF };
  
  
  function Bank( Addr: Integer ): Integer;
  begin { Bank }
    Bank := Shift(Addr,-12)
  end { Bank };
  
  
  function Offset( Addr: Integer ): Integer;
  begin { Offset }
    Offset := LAnd(Addr,#7777)
  end { Offset };
  
  
  procedure NewName(var N: Name);
  var i: integer;
  begin { NewName }
   N := GenName;
   i := NameMax;
   while GenName[i] = 'Z' do
    begin GenName[i] := 'A'; i := i - 1 end;
   GenName[i] := Succ(GenName[i])
  end { NewName };

     
  procedure PreScan;
  var SymIndex, LineIndex, N: integer;
      C: char;
      CanBeAnId: boolean;
      
      
   procedure GatherNumber(Radix: integer);
   var Upper, Lower, T: integer;
      Large: boolean;
   begin { GatherNumber }
    N := 0;
    if not (C in ['0'..'9']) then Err(46,LineIndex)
    else
     begin Upper := 0;
      Lower := 0;
      Large := false;
      repeat
       T := Ord(C) - Ord('0');
       if T >= Radix then
        if Radix = 8 then Err(12,LineIndex)
        else Error(44);
       Lower := Lower * Radix + T;
       T := Lower div 256;
       Lower := Lower mod 256;
       Upper := Upper * Radix + T;
       Large := Large or (Upper > 255);
       LineIndex := LineIndex + 1;
       C := InLine[LineIndex]
      until not (C in ['0'..'9']);
      if Large then
       begin Err(47,Sym[SymIndex].Pos); N := 0 end
      else N := Shift(Upper,8) + Lower
     end
   end { GatherNumber };
    
    
  begin { PreScan }
   LineIndex := 1;
   SymIndex := 1;
   while InLine[LineIndex] = ' ' do LineIndex := LineIndex + 1;
   C := InLine[LineIndex];
   while C <> ';' do with Sym[SymIndex] do
    begin Pos := LineIndex;
     if C in ['0'..'9', '#'] then
      begin Kind := number;
       if C = '#' then
        begin LineIndex := LineIndex + 1;
         C := InLine[LineIndex];
         GatherNumber(8)
        end
       else
        begin N := LineIndex;
         repeat N := N + 1 until not (InLine[N] in ['0'..'9']);
         if InLine[N] = '#' then
          begin GatherNumber(10);
           if not (N in [2,8,10]) then
            begin Err(45,LineIndex); N := 8 end;
           LineIndex := LineIndex + 1;
           C := InLine[LineIndex];
           GatherNumber(N)
          end
         else GatherNumber(Radix)
        end;
       Val := N;
       SymIndex := SymIndex + 1
      end
     else
      if C in ['A'..'Z', 'a'..'z'] then
       begin Kind := Identifier;
        N := 0;
        CanBeAnId := true;
        Id := '          ';
        repeat
         if C in ['a'..'z'] then C := Chr(Ord(C) + UpperLower)
         else
          if (C = '[') or (C = ']') then CanBeAnId := false;
         if N <= NameMax then Id[N] := C;
         N := N + 1;
         LineIndex := LineIndex + 1;
         C := InLine[LineIndex]
        until not (C in ['A'..'Z', 'a'..'z', '0'..'9', '[', ']']);
        Ptr := SearchSym(Id,False);
        if Ptr <> nil then
         begin
          if Ptr^.SymType = ReservedWordType then
           begin Kind := Ptr^.WordKind;
            Val := Ptr^.WordValue
           end;
          SymIndex := SymIndex + 1
         end
        else
         if not CanBeAnId then Err(13,Pos)
         else SymIndex := SymIndex + 1
       end
      else
       if C in [':', ',', '(', ')', '+', '-', '=', '*', '<', '>', '%'] then
        begin
         case C of
          ':': if InLine[LineIndex + 1] = '=' then
                begin Kind := becomes; LineIndex := LineIndex + 1 end
               else Kind := colon;
          ',': Kind := comma;
          '(': Kind := lparen;
          ')': Kind := rparen;
          '+': Kind := plus;
          '-': Kind := minus;
          '=': Kind := equal;
          '<': if InLine[LineIndex + 1] = '=' then
                begin Kind := lessorequal; LineIndex := LineIndex + 1 end
               else
                if InLine[LineIndex + 1] = '>' then
                 begin Kind := notequal; LineIndex := LineIndex + 1 end
                else Kind := lessthan;
          '>': if InLine[LineIndex + 1] = '=' then
                begin Kind := greaterorequal; LineIndex := LineIndex + 1 end
               else Kind := greaterthan;
          '*': Kind := star;
          '%': Kind := percent;
          end;
         LineIndex := LineIndex + 1;
         SymIndex := SymIndex + 1
        end
       else
        begin Err(13,LineIndex);
         repeat LineIndex := LineIndex + 1
         until InLine[LineIndex] in ['0'..'9', 'A'..'Z', 'a'..'z',
                                    ':', ',', '(', ')', '+', '-',
                                    '=', '*', '<', '>', '%']
        end;
     while InLine[LineIndex] = ' ' do LineIndex := LineIndex + 1;
     C := InLine[LineIndex]
    end;
   repeat LineIndex := LineIndex + 1
   until InLine[LineIndex] <> ' ';
   if InLine[LineIndex] <> '~' then Err(35,LineIndex);
   Sym[SymIndex].Kind := semicolon; Sym[SymIndex].Pos := LineIndex;
   SymIndex := SymIndex + 1;
   Sym[SymIndex].Kind := semicolon; Sym[SymIndex].Pos := LineIndex
  end { PreScan };
 
  
  
  procedure DefineLabel;
  begin { DefineLabel }
   New(CurrentSym);
   with CurrentSym^ do
    begin PrintName := Sym[S].Id;
     SymType := LabelType;
     LabelAddress := Dot;
     LabelDefined := true;
     EnterSym;
     {** writeln; **}
     write(PrintName);
     {**} Write('  ')
    end
  end { DefineLabel };
  
  
  function Constant(Min,Max,E: integer; Fsys: SetofSymbols): integer;
  var C: integer;
    
   
   function ConstExpr(Fsys: SetofSymbols): integer;
   var L, R: Integer;
       RelOp: SetofSymbols;
       Operator: Symbol;


    function ConstSimpleExpr(Fsys: SetofSymbols): integer;
    var L, R: Integer;
        AddOp: SetofSymbols;
        Operator: Symbol;
        UnaryMinus: Boolean;
    
    
     function ConstTerm(Fsys: SetofSymbols): integer;
     var L, R: Integer;
         MulOp: SetofSymbols;
         Operator: Symbol;
    
    
      function ConstFactor(Fsys: SetofSymbols): integer;
      var F: Integer;
          UnaryNot: Boolean;
          P: SymPointer;
      begin { ConstFactor }
       F := 0;
       UnaryNot := False;
       while Sym[S].Kind = NotName do
        begin UnaryNot := not UnaryNot; S := S + 1 end;
       with Sym[S] do
        if Kind = number then
         begin F := Val; S := S + 1 end
        else
         if Kind = Identifier then
          begin P := SearchSym(Id,True);
           if P = nil then Error(14)
           else
            if P^.SymType = ConstType then F := P^.ConstValue
            else Error(15);
           S := S + 1
          end
         else
          if Kind = lparen then
           begin
            S := S + 1;
            F := ConstExpr(Fsys + [rparen]);
            if Sym[S].Kind = rparen then S := S + 1
            else
             begin Error(18); Skip(Fsys) end
           end
          else
           begin Error(15); Skip(Fsys) end;
       if UnaryNot then ConstFactor := LNot(F)
       else ConstFactor := F
      end { ConstFactor };
   
   
     begin { ConstTerm };
      MulOp := [star, DivName, ModName, AndName, NAndName,
                LSHName, RSHName, RotName];
      L := ConstFactor(Fsys + MulOp);
      while Sym[S].Kind in MulOp do
       begin
        Operator := Sym[S].Kind;
        S := S + 1;
        R := ConstFactor(Fsys + MulOp);
        case Operator of
         star:           L := L * R;
         DivName:        if R = 0 then
                          begin Error(65); L := 0 end
                         else L := L div R;
         ModName:        if R = 0 then
                          begin Error(65); L := 0 end
                         else L := L mod R;
         AndName:        L := LAnd(L,R);
         NAndName:       L := LNot(LAnd(L,R));
         LSHName:        if Abs(R) > 15 then L := 0
                         else L := Shift(L,R);
         RSHName:        if Abs(R) > 15 then L := 0
                         else L := Shift(L,-R);
         RotName:        L := Rotate(L, LAnd(R,#17));
         end
       end;
      ConstTerm := L
     end { ConstTerm };


    begin { ConstSimpleExpr };
     AddOp := [plus, minus, OrName, NOrName];
     UnaryMinus := False;
     if Sym[S].Kind in [plus, minus] then
      begin UnaryMinus := Sym[S].Kind = minus; S := S + 1 end;
     L := ConstTerm(Fsys + AddOp);
     if UnaryMinus then L := -L;
     while Sym[S].Kind in AddOp do
      begin
       Operator := Sym[S].Kind;
       S := S + 1;
       R := ConstTerm(Fsys + AddOp);
       case Operator of
        plus:           L := L + R;
        minus:          L := L - R;
        OrName:         L := LOr(L,R);
        NOrName:        L := LNot(LOr(L,R));
        end
      end;
     ConstSimpleExpr := L
    end { ConstSimpleExpr };


   begin { ConstExpr };
    RelOp := [equal, notequal, lessthan, greaterthan, lessorequal,
              greaterorequal, XOrName, XNOrName];
    L := ConstSimpleExpr(Fsys + RelOp);
    if Sym[S].Kind in RelOp then
     begin
      Operator := Sym[S].Kind;
      S := S + 1;
      R := ConstSimpleExpr(Fsys + RelOp);
      case Operator of
       equal:          L := Ord(L = R);
       notequal:       L := Ord(L <> R);
       lessthan:       L := Ord(L < R);
       greaterthan:    L := Ord(L > R);
       lessorequal:    L := Ord(L <= R);
       greaterorequal: L := Ord(L >= R);
       XOrName:        L := LXOr(L,R);
       XNOrName:       L := LNot(LXOr(L,R));
       end
     end;
    ConstExpr := L
   end { ConstExpr };
   
   
  begin { Constant }
   C := ConstExpr(Fsys);
   if Max <> 0 then
    if (C > Max) or (C < Min) then
     begin Err(E,Sym[S-1].Pos); C := Min end;
   Constant := C
  end { Constant };
  
  
  procedure Define(Ty: Symbol);
  var P: SymPointer;
      PercentPresent: Boolean;
      S1: Integer;
  begin { Define }
   if Sym[S].Kind = Lparen then
    begin S := S + 1;
     S1 := Sym[S].Pos;
     if (Machine = OPerq1A) and BaseRegister and (Ty = XYtype) then
      begin
       PercentPresent := Sym[S].Kind = percent;
       if PercentPresent then S := S + 1
      end
     else PercentPresent := False;
     if Sym[S].Kind = Identifier then
      begin New(CurrentSym);
       with CurrentSym^ do
        begin PrintName := Sym[S].Id;
         SymType := Ty
        end;
       S := S + 1;
       if Sym[S].Kind = comma then S := S + 1
       else
        begin Error(25); Skip([number,identifier,comma,semicolon]) end;
       if Ty = XYtype then
        begin
         CurrentSym^.XYAddress := Constant(0,255,43,[rparen,comma,semicolon]);
         if (Machine = OPerq1A) and BaseRegister then
          if PercentPresent <> (CurrentSym^.XYAddress < #100) then
           if PercentPresent then Err(66,S1)
           else Err(67,S1)
        end
       else CurrentSym^.ConstValue := Constant(0,0,0,[rparen,comma,semicolon]);
       EnterSym;
       if Sym[S].Kind = rparen then S := S + 1
       else
        begin Error(18); Skip([comma,semicolon]) end
      end
     else
      begin Error(16); Skip([comma,semicolon]) end
    end
  end { Define };
  
  
  procedure Place;
  begin { Place }
   if Dot = 0 then
    if Sym[S].Kind = lparen then with CodFile^ do
     begin Line := 0;
      Placement := true;
      S := S + 1;
      First := Constant(0,MaxPhysAddr,40,[comma,rparen,semicolon]);
      if Sym[S].Kind = comma then
       begin S := S + 1;
        Last := Constant(0,MaxPhysAddr,40,[comma,rparen,semicolon])
       end
      else
       begin Error(25);
        Last := First;
        Skip([rparen,semicolon])
       end;
      if First > Last then
       begin Error(50); First := 0; Last := 4095 end;
      if CurrentBank = -1 then
       begin
        CurrentBank := Bank(First);
        if CurrentBank <> Bank(Last) then Error(59)
       end
      else
       if CurrentBank <> Bank(First) then Error(59);
      if Sym[S].Kind = rparen then S := S + 1
      else
       begin Error(18); Skip([semicolon]) end;
      Put(CodFile)
     end
    else
     begin Error(17); Skip([semicolon]) end
   else
    begin Error(49); Skip([semicolon]) end
  end { Place };
     
  
  procedure Kase(MaxVal,E: Integer);
  var Addr, CaseNumber: integer;
  begin { Kase }
   GeneratedInstruction := true;
   if Sym[S].Kind = lparen then
    begin S := S + 1;
     Addr := Constant(0,MaxPhysAddr,40,[comma,rparen,semicolon]);
     if LAnd(Addr,#74) <> 0 then Error(53);
     if Sym[S].Kind = comma then
      begin S := S + 1;
       CaseNumber := Constant(0,MaxVal,E,[comma,rparen,semicolon])
      end
     else
      begin Error(25); CaseNumber := 0; Skip([rparen,semicolon]) end;
     if Sym[S].Kind = rparen then S := S + 1
     else
      begin Error(18); Skip([semicolon]) end;
     CurMI.PosVal := Addr + (MaxVal - CaseNumber) * 4;
     CurMI.PosMask := -1
    end
   else
    begin Error(17); Skip([semicolon]) end
  end { Kase };
     
  
  procedure OpKode;
  var Addr, OpCode, SOpCode, SAddr: integer;
  begin { OpKode }
   GeneratedInstruction := true;
   if Sym[S].Kind = lparen then
    begin S := S + 1;
     Addr := 0;
     OpCode := Constant(0,0,0,[comma,rparen,semicolon]);
     SOpCode := S - 1;
     if Sym[S].Kind = comma then
      begin S := S + 1;
       SAddr := SOpCode;
       Addr := OpCode;
       OpCode := Constant(0,0,0,[comma,rparen,semicolon])
      end;
     if (Addr < 0) or (Addr > MaxPhysAddr) then
      begin
       Err(40,Sym[SAddr].Pos);
       Addr := 0
      end;
     if (OpCode < 0) or (OpCode > 255) then
      begin
       Err(39,Sym[SOpcode].Pos);
       OpCode := 0
      end;
     if LAnd(Addr,#1774) <> 0 then Err(63,Sym[SAddr].Pos);
     if Sym[S].Kind = rparen then S := S + 1
     else
      begin Error(18); Skip([semicolon]) end;
     CurMI.PosVal := Addr + (255 - OpCode) * 4;
     CurMI.PosMask := -1
    end
   else
    begin Error(17); Skip([semicolon]) end
  end { OpKode };
       
  
  function Funktion(Min,Max,E: integer): integer;
  var P: SymPointer;
  begin { Funktion }
   Funktion := 0;
   if Sym[S].Kind = Lparen then
    begin S := S + 1;
     Funktion := Constant(Min,Max,E,[rparen,comma,semicolon]);
     if Sym[S].Kind = Rparen then S := S + 1
     else
      begin Error(18); Skip([comma,semicolon]) end
    end
   else
    begin Error(17); Skip([comma,semicolon]) end
  end { Funktion };
          
       
  
  procedure Pseudo;
  var Start: integer;
  begin { Pseudo }
   if SyKind in [EndName,DefineName,ConstName,PlaceName,
                 BinaryName,OctalName,DecimalName] then
    if GeneratedInstruction then
     Error(48);
   Start := S;
   S := S + 1;
   case SyKind of
    NopName: GeneratedInstruction := true;
    PlaceName: Place;
    DefineName: Define(XYType);
    ConstName: Define(ConstType);
    OpcodeName: OpKode;
    LocName: begin GeneratedInstruction := true;
              CurMI.PosVal := Funktion(0,MaxPhysAddr,40);   { force position }
              CurMI.PosMask := -1
             end;
    CaseName: Kase(15,52);
    InterruptName: Kase(7,64);
    EndName: begin EndOfProgram := true; S := S + 1 end;
    DecimalName: Radix := 10;
    OctalName: Radix := 8;
    BinaryName: Radix := 2
    end;
   if SyKind in [PlaceName,DefineName,ConstName,EndName,DecimalName,
                 OctalName,BinaryName] then
    if Sym[S].Kind <> Semicolon then Err(48,Sym[Start].Pos)
  end { Pseudo };
  
  
  procedure NameEqualConstant;
  begin { NameEqualConstant }
   if Sym[S].Kind = Identifier then
    begin New(CurrentSym);
     with CurrentSym^ do
      begin PrintName := Sym[S].Id;
       SymType := ConstType
      end;
     S := S + 1;
     if Sym[S].Kind = equal then S := S + 1
     else
      begin Error(56); Skip([number,identifier,comma,semicolon]) end;
     CurrentSym^.ConstValue := Constant(0,0,0,[comma,semicolon]);
     EnterSym
    end
   else
    begin Error(16); Skip([comma,semicolon]) end
  end { NameEqualConstant };
  
  
  procedure Jump( Conditional: Boolean );
  var Jkind, Tkind: Symbol;
      Tval: integer;
  begin { Jump }
   GeneratedInstruction := true;
   Jkind:= Sym[S].Kind;
   if Jkind in [CallName, NxtInstName, RevVicName, GotoName,
                PushLodName, CallsName, VectorName, DispatchName,
                GotosName, RepeatName, JumpPopName, LeapPopName,
                LoadsName,
                TWBName, JzName, RptLpName, ReturnName, LoopName,
                NextName] then
    begin
     if CurMI.Target = 'REFILL    ' then
      begin
       Used := Used - [JMPf,Zf];
       CurMI.Target := '          '
      end;
     NeedJmp(Sym[S].Val);
     if (Jkind in [JzName, NxtInstName, RevVicName, RptLpName, RepeatName,
                   LoadsName, NextName]) and
        Conditional then Error(38);
     S := S + 1;
     if Sym[S].Kind = Lparen then
      begin
       if not (Jkind in [CallName, NxtInstName, GotoName, PushLodName,
                         CallsName, VectorName, DispatchName, GotosName,
                         RepeatName, JumpPopName, LeapPopName,
                         LoadsName, TwbName]) then
        Error(33);
       if (Jkind in [CallsName,GotosName]) and not Conditional then
        Error(37);
       S := S + 1;
       with Sym[S] do
        begin Tkind := ConstType; Tval := 0;
         if Kind = Identifier then
          begin CurrentSym := SearchSym(Id,True);
           if CurrentSym = nil then { forward reference to label }
            begin New(CurrentSym,LabelType);
             with CurrentSym^ do
              begin SymType := LabelType;
               PrintName := Id;
               LabelDefined := false;
               LabelAddress := 0
              end;
              EnterSym
            end;
           with CurrentSym^ do
            if SymType = ConstType then Tval := ConstValue
            else
             if SymType = LabelType then
              begin Tkind := LabelType; Tval := LabelAddress end
             else
              begin Error(21); Skip([rparen,comma,semicolon]); S := S - 1 end;
           S := S + 1
          end
         else
          if Kind = number then begin Tval := Val; S := S + 1 end
          else
           if (Machine = OPerq1A) and (Kind = ShiftName) then
            begin Tkind := ShiftName; S := S + 1 end
           else
            begin Error(21); Skip([rparen,comma,semicolon]) end
        end;
       if Sym[S].Kind = rparen then S := S + 1
       else
        begin Error(18); Skip([comma,semicolon]) end
      end
     else
      begin Tkind := None;
       if not (Jkind in [GotosName, CallsName, RptLpName, ReturnName,
                         LoopName, NextName, JzName, RevVicName]) then
        begin Error(34); Skip([comma,semicolon]) end;
       if Jkind in [CallsName, GotosName] then
        begin
         if Conditional then
          begin New(CurrentSym);
           with CurrentSym^ do
            begin SymType := LabelType;
             NewName(PrintName);
             LabelDefined := true;
             LabelAddress := Dot + 1
            end;
           EnterSym;
           Tkind := LabelType
          end
         else
          begin NeedCnd(Ord(FalseCnd)); Conditional := True end
        end
      end;
     if Jkind in [DispatchName,RevVicName,NxtInstName,VectorName] then
      if (Jkind = DispatchName) or (Jkind = RevVicName) then
       begin NeedH(true); Jkind := Pred(Jkind) end
      else NeedH(false);
     if JKind in [JumpPopName,LeapPopName] then
      if Machine = OPerq1A then
       if JKind = LeapPopName then
        begin NeedH(True); JKind := JumpPopName end
       else NeedH(False);
     if Tkind = LabelType then
      begin
       if CurMI.Target <> '          ' then Error(Ord(JMPf));
       CurMI.Target := CurrentSym^.PrintName;
       CurMI.TargtVA := CurrentSym^.LabelAddress;
       NeedZ(0,NoZ)
      end
     else
      if Tkind = ConstType then
       begin
        if CurrentBank = -1 then CurrentBank := Bank(0);
        if Tval > MaxPhysAddr then
         begin Err(40,Sym[S-2].Pos); Tval := Shift(CurrentBank,12) end;
        if Jkind in [DispatchName,RevVicName,NxtInstName,VectorName] then
         begin
          if Bank(Tval) <> CurrentBank then Err(60,Sym[S-2].Pos);
          if JKind = NxtInstName then
           begin
            if LAnd(Tval,#1774) <> 0 then Error(63)
           end
          else
           if Jkind in [DispatchName,VectorName] then
            begin
             if LAnd(Tval,#74) <> 0 then Error(53)
            end;
          Tval := Offset(Tval);
          NeedZ(255 - (Tval div 16 + Tval mod 4),NoZ);
          if (CurMI.B = Ord(Yconstant)) and
             (CurMI.F = Ord(SF1)) and (CurMI.SF = Ord(NopSF)) then
           { protect short constant } CurMI.SF := Ord(DstRoGetsSF)
         end
        else
         if JKind in [CallName,GotoName,PushLodName,CallSName,GotoSName,
                      JumpPopName,LeapPopName,LoadSName] then
          if Bank(Tval) = CurrentBank then
           begin { long jump }
            NeedSF(LXOr(Shift(Tval,-8),#17),LJSF);
            NeedZ(LXOr(LAnd(Tval,#377),#377),LJZ)
           end
          else { leap jump }
           begin
            NeedSF(Ord(LeapSF),MemCntl);
            NeedY(Shift(Tval,-8));
            NeedZ(LXOr(LAnd(Tval,#377),#377),SJZ)
           end
         else { leap jumps are not allowed for Perq1A }
          begin
           if Bank(Tval) <> CurrentBank then Err(60,Sym[S-2].Pos);
           Tval := Offset(Tval);
           NeedSF(LXOr(Shift(Tval,-8),#17),LJSF);
           NeedZ(LXOr(LAnd(Tval,#377),#377),LJZ)
          end
       end
      else
       if Tkind = ShiftName then
        begin
         if not (Jkind in [CallName,GotoName,PushLodName,CallSName,GotoSName,
                           RepeatName,JumpPopName,LeapPopName,LoadSName,
                           TWBName]) then
          Err(61,Sym[S-2].Pos);
         NeedSF(Ord(GotoShiftSF),MemCntl)
        end
    end
   else
    begin Error(22); Skip([comma,semicolon]) end;
   if not Conditional then NeedCnd(Ord(TrueCnd))
  end { Jump };
               
                
                  
  procedure Test;
  begin { Test }
   GeneratedInstruction := true;
   S := S + 1;
   if Sym[S].Kind in [TrueName, FalseName, Bpc3Name, C19Name, IpName, A0Name,
                      A7Name, A15Name, EqlName, NeqName, GtrName, GeqName,
                      LssName, LeqName, CarryName, OvfName] then
    begin
     NeedCnd(Sym[S].Val);
     S := S + 1;
     Jump(True)
    end
   else
    begin Error(19); Skip([comma,semicolon]) end
  end { Test };
  
  
  procedure SetShift( S: integer );
  begin { SetShift }
   if (CurMI.B = Ord(Yconstant)) and
      (CurMI.F = Ord(SF1)) and (CurMI.SF = Ord(NopSF)) then
    { protect short constant } CurMI.SF := Ord(DstRoGetsSF);
   NeedZ(255 - S,ShiftFunction)
  end { SetShift };
  
  
  
  procedure Nonary;
  begin { Nonary }
   GeneratedInstruction := true;
   case SyKind of
    MulStepName,
    DivStepName,
    FetchName,
    Fetch2Name,
    Fetch4Name,
    Fetch4rName,
    StoreName,
    Store2Name,
    Store4Name,
    Store4rName,
    LatchMaName: NeedSF(Sym[S].Val,MemCntl);
    HoldName:    NeedH(true);
    PushName:    begin
                  StackWant := StackPush;
                  StackSym := S
                 end;
    PopName,
    WcsLowName,
    WcsMidName,
    WcsHiName,
    LoadOpName,
    StkResetName,
    ShiftOnRName: NeedSF(Sym[S].Val,SF1)
    end;
   S := S + 1
  end { Nonary };
  
  
  procedure Unary;
  var V: integer;
  begin { Unary }
   GeneratedInstruction := true;
   S := S + 1;
   case SyKind of
    CntlRoName:  begin NeedSF(Ord(CntlRoGetsSF),SF1);
                  NeedZ(Funktion(0,255,41),NoZ)
                 end;
    IobName:     begin NeedSF(Ord(IobFunctionSF),SF1);
                  V := Funktion(0,255,42);
                  if V < 128 then NeedZ(127 - V,NoZ)
                  else NeedZ(383 - V,NoZ)
                 end;
    LShiftName:  begin V := Funktion(0,15,23);
                  SetShift( V*16 + 15 )
                 end;
    RShiftName:  begin V := Funktion(0,15,23);
                  SetShift( V*16 + 15 - V )
                 end;
    RotateName:  begin V := Funktion(-15,15,23);
                  if V < 0 then V := 16 + V;
                  if V < 8 then SetShift( (V + 8) * 16 + 13 )
                  else SetShift( V*16 + 14 )
                 end;
    end
  end { Unary };
  
  
  procedure Binary;
  var V1, V2: integer;
  begin { Binary }
   GeneratedInstruction := true;
   { must be FieldName }
   S := S + 1;
   if Sym[S].Kind = lparen then
    begin S := S + 1;
     V1 := Constant(0,15,24,[comma,semicolon]);
     if Sym[S].Kind = comma then
      begin S := S + 1; V2 := Constant(0,15,24,[comma,semicolon]) end
     else
      begin Error(25); V2 := 0; Skip([rparen,comma,semicolon]) end;
     SetShift( V1*16 + V2 - 1 );
     if Sym[S].Kind = rparen then S := S + 1
     else
      begin Error(18); Skip([comma,rparen]) end
    end
   else
    begin Error(17); Skip([comma,rparen]) end
  end { Binary };
     
     
  procedure Alu;
  var Op, Lval, Rval, SLeft, SOp, SRight: integer;
      Lkind, Rkind: Symbol;
      Lnot, Rnot: boolean;
      Left, Right: boolean;
      Byte: packed record case integer of
             1: (I: integer);
             2: (Lower: 0..255;
                 Upper: 0..255)
             end;
      Done: Boolean;
  
  
   procedure Operand( var Fkind: Symbol; var Fval: integer;
                      var Fnot: boolean; FSys: SetOfSymbols );
   var P: SymPointer;
   
   
    procedure RegOrOperand( var Fkind: Symbol; var Fval: integer;
                            FSys: SetOfSymbols );
    var PercentPresent: Boolean;
        S1, S2: Integer;
    begin { RegOrOperand }
     Fkind := XYtype;
     Fval := 0;
     S1 := Sym[S].Pos;
     if (Machine = OPerq1A) and BaseRegister then
      begin
       PercentPresent := Sym[S].Kind = Percent;
       if PercentPresent then S := S + 1
      end
     else PercentPresent := False;
     S2 := Sym[S].Pos;
     with Sym[S] do
      if Kind = identifier then
       begin P := SearchSym(Id,True);
        if P = nil then Error(14)
        else
         with P^ do
          if SymType = XYType then Fval := XYAddress
          else
           if SymType = ConstType then
            begin Fkind := ConstType; Fval := ConstValue end
           else Error(27);
        S := S + 1
       end
      else
       if Kind = number then
        begin Fkind := ConstType; Fval := Val; S := S + 1 end
       else
        if Kind in [ShiftName, IodName, MdiName, MdxName,
                    TosName, NextOpName,
                    MQName, VictimName, RBaseName, MaName, MdoName,
                    SrcRoName, DstRoName, WidRoName] then
         begin Fkind := Kind; Fval := Val; S := S + 1 end
        else
         if Kind = lparen then
          begin Fkind := ConstType; Fval := Constant(0,0,0,Fsys) end
         else
          begin
           Error(16);
           Skip(Fsys)
          end;
      if (Machine = OPerq1A) and BaseRegister then
       begin
        if PercentPresent then
         if FKind = ConstType then
          begin
           if (Fval < 0) or (Fval > 255) then
            begin
             Err(43,S2);
             Fval := 0
            end;
           Fkind := XYType
          end
         else
          if Fkind <> XYtype then Err(27,S2);
        if (Fkind = XYtype) and (PercentPresent <> (Fval < #100)) then
         if PercentPresent then Err(66,S1)
         else Err(67,S1)
       end
    end { RegOrOperand };
    
    
    procedure Ustate;
    var Lkind: Symbol;
        Lval: Integer;
        S1: Integer;
    begin { Ustate }
     FKind := UstateName;
     FVal := Ord(UstateSrc);
     S := S + 1;
     if Sym[S].Kind = lparen then
      begin S := S + 1;
       S1 := Sym[S].Pos;
       RegOrOperand(Lkind,Lval,FSys+[rparen]);
       if Lkind = XYtype then
        begin
         NeedB(Ord(YRam));
         NeedY(Lval)
        end
       else Err(27,S1);
       if Sym[S].Kind = rparen then S := S + 1
       else
        begin Error(18); Skip(FSys) end
      end
    end { Ustate };
    
    
   begin { Operand }
    if Sym[S].Kind = NotName then
     begin Fnot := true; S := S + 1 end
    else Fnot := false;
    with Sym[S] do
     if Kind = UstateName then Ustate
     else RegOrOperand(Fkind,Fval,FSys);
   end { Operand };


   procedure Assignment;
   begin { Assignment }
    case Lkind of
     MQName:     NeedSF(Ord(LoadMQSF),MemCntl);
     RBaseName:  NeedSF(Ord(LoadBaseSF),MemCntl);
     TosName:    if StackWant <> StackPush then
                  begin
                   StackWant := StackAssign;
                   StackSym := S
                  end;
     BpcName:    NeedSF(Ord(BpcGetsSF),SF1);
     MaName,
     MdoName:    ;
     CntlRoName,
     SrcRoName,
     DstRoName,
     WidRoName:  NeedSF(Lval,SF1);
     XYtype:     begin
                  NeedX(Lval);
                  NeedW(true)
                 end
     end
   end { Assignment };


  begin { Alu }
   GeneratedInstruction := true;
   Done := False;
   repeat
    SLeft := Sym[S].Pos;
    if Sym[S].Kind in [MQName, VictimName, RBaseName,
                       TosName, BpcName, MaName, MdoName,
                       SrcRoName, DstRoName, WidRoName] then
     begin
      Lkind := Sym[S].Kind;
      Lval := Sym[S].Val;
      Lnot := false;
      S := S + 1
     end
    else
     Operand(Lkind,Lval,Lnot,[andName, orName, xorName, nandName, norName,
                              xnorName, plus, minus, comma, semicolon]);
    if not Lnot and
       (Lkind in [XYtype, MQName, RBaseName, TosName, BpcName, MaName,
                  MdoName, SrcRoName, DstRoName, WidRoName]) and
       (Sym[S].Kind = becomes) then
     begin
      Assignment;
      S := S + 1
     end
    else Done := True
   until Done;
   Left := True; Right := True;
   Op := Ord(Aalu);
   if Lkind = ConstType then Op := Ord(Balu);
   if Lkind = XYtype then if Xf in Used then
    if CurMI.X <> Lval then Op := Ord(Balu);
   if Op = Ord(Balu) then
    begin Rkind := Lkind; Rval := Lval; Left := False end
    else Right := False;
   if Lnot then Op := Op + 2;
   if Sym[S].Kind in [AMuxName, BMuxName, andName, orName, xorName,
                      nandName, norName, xnorName, plus, minus] then
    if not Left then
     begin Err(28,SLeft); Skip([comma,semicolon]) end
    else
     begin Right := True;
      SOp := Sym[S].Pos;
      if Sym[S].Kind in [plus,minus] then
       if Sym[S].Kind = plus then Op := Ord(AplusBalu)
       else Op := Ord(AminusBalu)
      else Op := Sym[S].Val;
      if Lnot then
       if Op = Ord(Aalu) then Op := Op + 2
       else Err(30,SLeft);
      S := S + 1;
      SRight := Sym[S].Pos;
      Operand(Rkind,Rval,Rnot,[plus,minus,comma,semicolon]);
      if Rnot then
       if Op in [Ord(AandBalu), Ord(AorBalu)] then Op := Op + 1
       else
        if Op = Ord(Balu) then Op := Op + 2
        else Err(30,SRight);
      if not (Rkind in [XYtype,ConstType]) then
       begin Err(29,Sym[S-1].Pos); Rkind := XYtype; Rval := 0 end
     end;
   WantResult := ALUResult;
   if Left then
    if Lkind = XYtype then begin NeedX(Lval); NeedA(Ord(Xsrc)) end
    else
     if Lkind = NextOpName then
      begin NeedA(Ord(NextOp));
       NeedJmp(Ord(GotoJmp));
       NeedCnd(Ord(Bpc3Cnd));
       NeedZ(0,NoZ);
       CurMI.Target := 'REFILL    '
      end
     else
      case LKind of
       ShiftName,
       IodName,
       MdiName,
       MdxName,
       TosName,
       NextOpName,
       UStateName:    NeedA(Lval);
       MQName,
       VictimName:    case Machine of
                       OPerq1:    { shouldn't be possible } Err(68,SLeft);
                       OPerq1A:   begin
                                   WantResult := SpecialResult;
                                   NeedSF(Lval,MemCntl)
                                  end;
                       end;
       otherwise:     Err(68,SLeft)
       end;
   if not (Result in [NoResult, WantResult]) then Err(62,SLeft);
   Result := WantResult;
   if Right then
    if Rkind in [XYType, ConstType] then
     begin
      Byte.I := Rval;
      NeedY(Byte.Lower);
      if Rkind = XYtype then NeedB(Ord(YRam))
      else
       begin NeedB(Ord(YConstant));
        if Byte.Upper <> 0 then { long constant }
         begin
          NeedSF(Ord(NopSF),SF1);
          NeedZ(Byte.Upper,LongConstant)
         end
        else { short constant }
         if (Zf in Used) and
            (CurMI.F = Ord(SF1)) and (CurMI.SF = Ord(NopSF)) then
          { protect short constant } CurMI.SF := Ord(DstRoGetsSF)
       end
     end
    else { not XYTpe or ConstType }
     Err(68,SRight);
   if Sym[S].Kind in [plus,minus] then
    if (Sym[S].Kind = plus) and (Op = Ord(AplusBalu)) then
     Op := Ord(AplusBplusOCalu)
    else
     if (Sym[S].Kind = minus) and (Op = Ord(AminusBalu)) then
      Op := Ord(AminusBminusOCalu)
     else
      begin Error(31); Skip([comma,semicolon]) end;
   if Op in [Ord(AplusBplusOCalu), Ord(AminusBminusOCalu)] then
    begin S := S + 1;
     if Sym[S].Kind = OldCarryName then S := S + 1
     else
      begin Error(32); Skip([comma,semicolon]) end
    end;
   case Result of
     SpecialResult:  if Op <> Ord(Aalu) then Err(62,SOp);
     AluResult:      NeedALU(Op);
     end;
  end { Alu };
      
            
           
 begin { Instruction }
  Result := NoResult;
  PreScan;
  Used := [];
  S := 1;
  while (Sym[S].Kind = Identifier) and (Sym[S+1].Kind = Colon) do
   begin DefineLabel; S := S + 2 end;
  while Sym[S].Kind <> semicolon do
   begin SyKind := Sym[S].Kind;
    if Sym[S].Kind in [DefineName, ConstName, OpcodeName, LocName, CaseName,
       InterruptName,
       EndName, PlaceName, NopName, DecimalName, OctalName, BinaryName,
       IfName, CallName, NxtInstName, RevVicName, GotoName, PushLodName,
       CallsName, VectorName, DispatchName, GotosName, RepeatName,
       JumpPopName, LeapPopName,
       LoadsName, TwbName, JzName, RptLpName, ReturnName,
       LoopName, NextName,
       WcsLowName, WcsMidName, WcsHiName, LoadOpName, HoldName,
       StkResetName, PushName, PopName, FetchName, Fetch2Name,
       Fetch4Name, Fetch4rName, StoreName, Store2Name,
       Store4Name, Store4rName, LatchMaName, CntlRoName,
       SrcRoName, DstRoName, WidRoName, ShiftOnRName,      
       LShiftName, RShiftName, RotateName, IobName,
       FieldName,
       Identifier, TosName,
       MaName, MdoName, BpcName,
       MQName, MulStepName, DivStepName,
       becomes,comma] then
     case Sym[S].Kind of
      NopName,
      PlaceName,
      DefineName,
      ConstName,
      OpcodeName,
      LocName,
      CaseName,
      InterruptName,
      EndName,
      DecimalName,
      OctalName,
      BinaryName:                 Pseudo;
      IfName:                     Test;
      CallName,
      NxtInstName,
      RevVicName,
      GotoName,
      PushLodName,
      CallsName,
      VectorName,
      DispatchName,
      GotosName,
      RepeatName,
      JumpPopName,
      LeapPopName,
      LoadsName,
      TwbName,
      JzName,
      RptLpName,
      ReturnName,
      LoopName,
      NextName:                   Jump(False);
      MulStepName,
      DivStepName,
      WcsLowName,
      WcsMidName,
      WcsHiName,
      LoadOpName,
      HoldName,
      StkResetName,
      PushName,
      PopName,
      FetchName,
      Fetch2Name,
      Fetch4Name,
      Fetch4rName,
      StoreName,
      Store2Name,
      Store4Name,
      Store4rName,
      LatchMaName,
      ShiftOnRName:               Nonary;
      LShiftName,
      RShiftName,
      RotateName,
      IobName:                    Unary;
      FieldName:                  Binary;
      CntlRoName:                 Unary;
      Identifier:                 if Sym[S+1].Kind = equal then
                                   NameEqualConstant
                                  else Alu;
      MQName,
      TosName,
      SrcRoName,
      DstRoName,
      WidRoName,
      MaName,
      MdoName,
      BpcName,
      becomes:                    Alu
      end
    else Alu;
    if Sym[S].Kind = comma then S := S + 1
    else
     if Sym[S].Kind <> semicolon then
      begin Error(25); Skip([comma,semicolon]) end
   end;
  if StackWant <> StackNone then
   begin
    S := StackSym;   { so errors print in the right place }
    if StackWant = StackPush then
     if Zuse = LongConstant then
      if Machine = OPerq1A then
       begin
        CurMI.F := Ord(MemCntl);
        CurMI.SF := Ord(PushLongSF);
        SFuse := MemCntl;
        ZUse := SJZ
       end
      else { Machine <> OPerq1A }
       NeedSF(Ord(PushSF),SF1)
     else { Zuse <> LongConstant }
      NeedSF(Ord(PushSF),SF1)
    else { StackWant <> StackPush }
     if StackWant = StackAssign then
      NeedSF(Ord(TosGetsSF),SF1)
   end
 end { Instruction };
    
  

 procedure WriteSymbolTable;  {Write out a symbol table}
 var Sym: SymPointer; I: integer;
 begin { WriteSymbolTable }
  Writeln; Writeln('Writing Symbol Table');
  for I := 0 to HashMax do
   begin
    Sym := SymTable[I];
    while Sym <> nil do
     with Sym^ do
      begin
       if (SymType = LabelType) and (not LabelDefined) then
        Writeln('**** Undefined Label ',PrintName);
       if SymType <> ReservedWordType then
        begin
         SymFile^ := Sym^;
         Put(SymFile)
        end;
       Sym := Next
      end
   end
 end { WriteSymbolTable };
 
   
 function SearchSym( var Target: Name; Remember: Boolean ): SymPointer;
   {Search Symbol Table for target, return Symbol entry}
 var Sym: SymPointer; i: integer;
 begin { SearchSym }
  SearchSym := nil;  {Return nil if we don't find it}
  HashVal := 0;
  i := NameMax;
  while Target[i] = ' ' do i := i - 1;
  repeat HashVal := HashVal + ORD(Target[i]);
   i := i - 1
  until i < 0;
  if HashVal >= 0 then HashVal := HashVal mod HashSize
  else HashVal := -HashVal mod HashSize;
  Sym := SymTable[HashVal];  {Start at beginning of Hash Chain}
  while Sym <> nil do  {Search entire chain until found}
   if Target = Sym^.PrintName then  {Got it}
    begin
     if Remember then
      if (Sym^.SymType in [LabelType, XYType, ConstType]) and
         (Sym^.PrintName[0] <> '!') then
       begin
        RefFile^.PrintName := Target;
        RefFile^.RefLine := LineNumber;
        Put(RefFile)
       end;
     SearchSym := Sym;  {Return pointer to the symbol}
     Sym := nil;  {And force exit}
    end
   else Sym := Sym^.Next;  {Otherwise Chain on}
 end { SearchSym };
 
 
 procedure EnterSym;
 var Sym: SymPointer;
     {Enter Symbol in Symbol Table}
 begin { EnterSym }
  if (CurrentSym^.SymType in [LabelType, XYType, ConstType]) and
     (CurrentSym^.PrintName[0] <> '!') then
   begin
    RefFile^.PrintName := CurrentSym^.PrintName;
    RefFile^.RefLine := LineNumber;
    Put(RefFile)
   end;
  Sym := SearchSym(CurrentSym^.PrintName,False);
  if Sym <> nil then
   if (Sym^.SymType = LabelType) and (NOT Sym^.LabelDefined) then
    begin  {Forward Reference now being defined}
     Sym^.LabelDefined := true;
     Sym^.LabelAddress := CurrentSym^.LabelAddress
    end
   else Error(20)  {Multiply Defined Symbol}
  else  {Not previously encounterd}
   begin
    CurrentSym^.Next := SymTable[HashVal];
    SymTable[HashVal] := CurrentSym
   end
 end { EnterSym };
 
 
 procedure Err( n,p: integer );
 type ErrMsg = record
                ErrNum: integer;
                ErrTxt: string[63]
               end;
 var i: integer;
     C: char;
     ErrFile: file of ErrMsg;
 begin { Err }
  writeln; writeln;
  writeln(InNumber[p]:5, ': ', InLine);
  write('^':p+7, '  ');
  Reset(ErrFile,'PrqMic.Error');
  while (ErrFile^.ErrNum <> n) and not Eof(ErrFile) do Get(ErrFile);
  if Eof(ErrFile) then writeln(n,': undefined error number')
  else writeln(ErrFile^.ErrTxt);
  write('+++ type <return> to continue, <^C> to terminate +++');
  readln
 end { Err };
 
 
 procedure Error( n: integer );
 begin { Error }
  Err(n,Sym[S].Pos)
 end { Error };
 
 
 Procedure OpenFiles;
 var CodFileName, SymFileName, RefFileName: string;
     FId: Integer;
     Blocks, Bits: Integer;
 begin { OpenFiles }
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,Ignore,' ',Ignore);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  if UsrCmdLine = '' then
   begin
    Write('Root file name: ');
    Readln(UsrCmdLine);
   end;
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,RootFileName,' ',Ignore);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  if UsrCmdLine <> '' then
   begin Write('unknown parameter: ',UsrCmdLine,' type <return> to continue');
    Readln
   end;
  InFileName := Concat(RootFileName,'.Micro');
  CodFileName := Concat(RootFileName,'.Rel');
  SymFileName := Concat(RootFileName,'.RSym');
  RefFileName := Concat(RootFileName,'.RRef');
  Include := false;
  if FSLookUp(InFileName,Blocks,Bits) = 0 then
   begin
    Writeln('** ', InFileName, ' not found.');
    Exit(PrqMic)
   end;
  Reset(InFile,InFileName);
  Writeln('Reading ',InFileName);
  Rewrite(CodFile,CodFileName);
  Rewrite(SymFile,SymFileName);
  Rewrite(RefFile,RefFileName)
 end { OpenFiles };
 
 
begin { PrqMic }
 Initialize;
 OpenFiles;
 GetLine;
 InitReservedWords;
 Preamble := False;
 {** write('          '); **}
 with CodFile^ do
  begin Line := -1;
   MachineOption := Machine;
   Put(CodFile)
  end;
 MaxPhysAddr := 4095;
 if Machine = OPerq1A then MaxPhysAddr := 16383;
 repeat CurMI := EmptyMI;
  SFuse := NoSF;
  Zuse := NoZ;
  StackWant := StackNone;
  GeneratedInstruction := false;
  Instruction;
  if GeneratedInstruction then
   begin
    if (Dot = 0) and not Placement then { generate default placement }
     with CodFile^ do
      begin Line := 0;
       First := 0;
       Last := 4095;
       Put(CodFile)
      end;
    Dot := Dot + 1;
    CurMI.Line := LineNumber;
    CurMI.Used := Used;
    CodFile^ := CurMI;
    Put(CodFile)
   end;
  {** Write('.'); **}
  if not EndOfProgram then GetLine
 until EndOfProgram;
 WriteSymbolTable;
 Close(CodFile);
 Close(SymFile);
 Close(RefFile)
end { PrqMic }.
