{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{$R-}
program PrqPlace;


imports System from System;
imports Perq_String from Perq_String;
imports CmdParse from CmdParse;
imports MicroOption from MicroOption;
imports UtilProgress from UtilProgress;
imports Memory from Memory;
imports PrqPl_Sort from PrqPl_Sort;
imports FileSystem from FileSystem;
imports FileUtils from FileUtils;


{ PERQ Micro Code Placer 
  Brian Rosen           ??-???-79
  John P. Strait        26-Jan-80      re-written
  Copyright (C) 1980, 1981, 1982, 1983 Three Rivers Computer Corp,
  Pittsburgh, PA
  
  Change log
    
  V2.6  Bill Braucher  16 Nov 82  Fixed names for 14-character compiler.
  
  V2.5  JPS  20 Oct 81  Delete intermediate files at completion.
    
  V2.4  JPS  18 Oct 81  Fix formatting error in cross reference.
    
  V2.3  JPS  29 Sep 81  Add cross-reference.
    
  V2.2  JPS  28 Sep 81  1) Fix bug when first instruction doesn't have a Loc.
                        2) Teach PrqPlace about the 16K WCS.
                        3) Use UtilProgress.
    
  V2.1  JPS  28 Sep 81  Begin changes for PERQ1A:
                        1) Add $Perq1 and $Perq1A assembler options.
                        2) Add $Base and $NoBase assembler options.
                        3) Get machine type from PrqMic.
                        4) Do away with the PDP-11 version of PrqMic.
  
  V2.0  JPS  18 May 81  Increase VMemorySize to 3072.

  V1.9  JPS  21 Feb 81  Convert to system C.3.
  
  V1.8  JPS  13 Feb 81  1) Print PA to VA mapping at end of listing.
                        2) Fix minor formatting problem on the report.
  
  V1.7  JPS  11 Feb 81  1) Correct bug in FindHole which caused PrqPlace to
                           hang under certain circumstances.
                        2) Fix minor formatting problem on the listing.
  
  V1.6  JPS   6 Jan 81  1) Change adjancency computation to agree with 
                           Prqmic 1.5.
                        2) Add $NoList and $List.

  V1.5  JPS   3 Jan 81  1) Fix formatting problems in the listing.
                        2) Only close the source file if it was opened.

  V1.4  JPS   3 Dec 80  1) Upon encountering a blank line, check to be sure
                           there are LinesLeft lines left on the listing page.
                        2) Do not print blank lines at the top of a page.
  
  V1.3  JPS  22 Nov 80  1) Implement assembler options:
                            $Include - Read include file.
                            $Title   - Set title and subtitle lines.
                        2) Allow form-feeds in the source file.
  
  V1.2  JPS   6 Nov 80  Shorten listed lines per page by one so that microcode
                        listings made under RSX-11M will look good.
  
  V1.1  JPS  18 Sep 80  Add version number.
}


const PrqPlaVersion = '2.6';
      PrintableLines = 62;     { printable lines per page on listing }
      LinesInHeader = 3;       { lines in page header }
      LinesPerPage = PrintableLines - LinesInHeader;
      LinesLeft = 5;           { ensure this many after a blank line }
      VMemorySize = 3072;      { amount of virtual memory allowed }
      MaxVAddress = 3071;      { VMemorySize - 1 }
      NoVAddress = 3072;       { MaxVAddress + 1 }
      MemorySize = 16384;      { size of microcode memory }
      MaxPAddress = 16383;     { MemorySize - 1 }
      NoPAddress = 16384;      { MaxPAddress + 1 }
      PageSize = 256;          { size of a page of memory }
      MaxPageAddr = 255;       { PageSize - 1 }
      NoPageAddr = 256;        { MaxPageAddr + 1 }
      NumberOfPages = 64;      { MemorySize div PageSize }
      MaxPage = 63;            { NumberOfPages - 1 }
      NoPage = 64;             { MaxPage + 1 }
      
      HashMax = 256;
      HashSize = 257;
      
      
type VAddress = 0..VMemorySize;     { virtual address }
     PAddress = 0..MemorySize;      { physical address }
     PageAddress = 0..PageSize;     { address within a page }
     PageNumber = 0..NumberOfPages; { page number }
     
     SType = (LabelType, XYType, ReservedWordType, ConstType);
     XYRange = 0..255;  {X and Y are 8 bit fields of MicroInstruction (MI)}
          {Virtual Addresses are in declaration order 
           Physical addresses are in placed order
             A line of source text containing a MI has a virtual address 1
             greater than the virtual address of the preceeding line, but
             may have a completely different physical address.
             Virtual addresses are used by the debugger, and by the main
             part of the assembler.  The Placer decides where each MI should
             go, and assigns the physical address. }
     HashCode = 0..HashMax;
     SymPointer = ^SymEntry;
     SymEntry = record  {Entry in the Symbol Table}
                  Next: SymPointer;  {Chained in reverse declaration order}
                  PrintName: Name;   {With an identifier}
                case SymType: Stype of  {and a type}
                  LabelType: (LabelAddress: integer;
                              LabelDefined: boolean);
                  XYType: (XYAddress: XYrange);
                  ReservedWordType: (WordKind: integer;
                                     WordValue: integer);
                  ConstType: (ConstValue: integer)
                end;
                {The following enumerations are for the various fields of a MI}
      AMUXtype = (ShiftSrc,NextOp,IobSrc,MdiSrc,MdxSrc,UstateSrc,XSrc,EStkSrc);
      ALUtype = (Aalu,Balu,NotAalu,NotBalu,AandBalu,AandNotBalu,AnandBalu,
                   AorBalu,AorNotBalu,AnorBalu,AxorBalu,AxnorBalu,
                   AplusBalu,AplusBPlusOCalu,AminusBalu,AminusBminusOCalu);
      SFusetype = (SF1,MemCntl,SF2,LJSF,NoSF);
      Zusetype = (LongConstant,SJZ,ShiftFunction,LJZ,NoZ);
      Fields = (Xf,Yf,Af,Bf,Wf,Hf,ALUf,Ff,SFf,Zf,CNDf,JMPf);
      FieldSet = set of Fields;
      SFtype = (NopSF, ShftOnRSf, StackResetSf, 
                   TOSgetsSF, PushSF, PopSF,
                   CntlROsf, SrcROsf, DstROsf, WidROsf, OpGetsSf, BPC,
                   WCSlowGetsSF, WCSmidGetsSF, WCShiGetsSF, IOBfunctionSf);
      MemFuncType = (Fetch, Store, Fetch4, Store4, Fetch4Reverse,
                     Store4Reverse);
      CNDtype = (TrueCnd, FalseCnd, IntrPendCnd, Eql20Cnd, BPC3Cnd, C19Cnd,
                 OddCnd, ByteSignCnd, NeqCnd, LeqCnd, LssCnd, OvrflwCnd, CryCnd,
                 EqlCnd, GtrCnd, GeqCnd);
      JMPtype = (JumpZeroJmp,CallJmp,NextInstJmp,GotoJmp,PushLoadJmp,
                   CallSJmp, VectorJmp, GoToSJmp, RepeatLoopJmp,
                   RepeatJmp, ReturnJmp, JumpPopJmp, LoadSJmp, LoopJmp,
                   NextJmp, ThreeWayBranchJmp);
      
      MicroInstruction =   {The format of a microinstruction before placing}
              packed record {Note that this is not the field order of a real MI}
               case Line: integer of   { source line number }
               -1: { machine type }
                 (MachineOption: MOption);
                0: { placement restrictions }
                 (First,Last: integer);
                1: { micro instruction }
                 (PosVal: integer;     { Position restrictions }
                  PosMask: Integer;    { which bits of PosVal are significant }
                  Target: Name;    {The name of the label which is the next MI}
                  TargtVA: integer; { Virtual address }
                  Used: FieldSet;
                  X,Y:  XYRange;
                  A:    AMUXtype;
                  B:    (YRam,YConstant);
                  W,H:  BOOLEAN;
                  ALU:  ALUtype;
                  F:    packed record case integer of
                         1: (SFuse: SFusetype);
                         2: (Zuse: Zusetype)
                         end;
                  Z:    0..255;
                  CND:  CNDtype;
                  JMP:  JMPtype;
                  case SFusetype of
                        SF1, SF2: (SF: SFtype);
                        MemCntl:  (MF: MemFuncType);
                        LJSF:     (LJ: 0..15)
                 )
                end;
        MicroBinary = Packed Record
                  bADRS:  integer;
                  bJMP:   0..15;
                  bCND:   0..15;
                  bZ:     0..255;
                  bSF:    0..15;
                  bF:     0..3;
                  bALU:   0..15;
                  bH:     0..1;
                  bW:     0..1;
                  bB:     0..1;
                  bA:     0..7;
                  bY:     0..255;
                  bX:     0..255
                end;
                
     TargetKind = (NoTarget, SamePage, AnyPage);
                
     VLocation = packed record            { describes a virtual location }
                  Tkind:    TargetKind;   { Target kind }
                  TA:       VAddress;     { VA of target }
                  VError:   boolean;      { Error at this VA }
                  Next:     boolean;      { Next VA must be next PA also }
                  PA:       PAddress;     { PA of this VA }
                  Head:     VAddress;     { Head pointer }
                 case BasicHead: boolean of
                  false:   ();
                  true:    (NextGroup: VAddress;
                            Done:      boolean);
                  end;
                 
                 
     VMemory = array[VAddress] of VLocation;
     
     PMemory = packed array[PageAddress] of boolean;
                
       
     
                
 var Machine: MOption;
     MSeg: SegmentNumber;
     M: ^VMemory;
     P: array[PageNumber] of record
         Empty:      PMemory;
         PError:     PMemory;
         Usable:     PMemory;
         Free:       PageAddress;
         Search:     PageAddress;
         UsablePage: boolean
         end;
     TotalFree: integer;
     
     RelFile, IntFile: file of MicroInstruction;
     SymFile:          file of SymEntry;
     SrcFile, LstFile: text;
     BinFile:          file of MicroBinary;
     Root, SrcName, IntName, RefName, IntRefName, Ignore: string;
     RelName, SymName, LstName, BinName: string;
     ListToConsole: Boolean;
     
     Largest: Integer;       { largest basic group in current page group }
     Total: Integer;         { total size of current page group }
     Page: PageNumber;       { current page }
     Last: VAddress;         { last virtual address used }
     PrintErrors: Boolean;   { if FindHole should print errors }
     Errors: Integer;        { count of errors in this run }
     Listing: Boolean;       { if listing desired }
     DEBUG: Boolean;
     MicroCount: Integer;
     SymTable: Array [0..HashMax] OF SymPointer;
     HashVal: integer;  {Hash value of last symbol searched}
     CurSym: SymPointer;


 function  BasicSize(V: VAddress): Integer;                     forward;
 function  FindHole(Length: integer): PAddress;                 forward;
 procedure ReserveMemory(VA: VAddress; PA: PAddress);           forward;
 procedure O(N: integer);                                       forward;
 procedure Oops;                                                forward;
 procedure PrintPMem(Page: PageNumber);                         forward;
 procedure PrintVMem(First,Last: VAddress);                     forward;
 
 
     
 
 procedure FindFile( var Name: String );
 var FId: FileId;
     Blocks, Bits: Integer;
 begin { FindFile }
   FId := FSLookUp(Name,Blocks,Bits);
   if FId = 0 then
     begin
       Writeln;
       Writeln('** ', Name, ' not found.');
       Exit(PrqPlace)
     end
 end { FindFile };
 
 
 
 function SearchSym(var Target: Name ): SymPointer;
   {Search Symbol Table for target, return Symbol entry}
 var Sym: SymPointer; i: integer;
 begin { SearchSym }
  SearchSym := nil;  {Return nil if we don't find it}
  HashVal := 0;
  for i := 0 to NameMax do
    HashVal := HashVal + Ord(Target[i]);
  HashVal := HashVal mod HashSize;
  Sym := SymTable[HashVal];  {Start at beginning of Hash Chain}
  while Sym <> nil do  {Search entire chain until found}
   if Target = Sym^.PrintName then  {Got it}
    begin
     SearchSym := Sym;  {Return pointer to the symbol}
     Sym := nil         {and force exit}
    end
   else Sym := Sym^.Next  {Otherwise Chain on}
 end { SearchSym };

 procedure EnterSym;
 var Sym: SymPointer;
     {Enter Symbol in Symbol Table}
 begin { EnterSym }
  Sym := SearchSym(CurSym^.PrintName);
  if Sym = nil then
   begin
    CurSym^.Next := SymTable[HashVal];
    SymTable[HashVal] := CurSym
   end
  else
   begin Dispose(CurSym);
    CurSym := Sym
   end
 end { EnterSym };
 
 
 
 
 { ----------------------------------------- }
 {                                           }
 { Pass1:  Assemble the source and create    }
 {           the file x.MICRO.REL.           }
 {                                           }
 { ----------------------------------------- }
 
 
 
 
 { ----------------------------------------- }
 {                                           }
 { Pass2:  Resolve forward references.       }
 {         Coerce jumps to the longest form  }
 {           possible.                       }
 {                                           }
 { ----------------------------------------- }
 

 procedure Pass2;
 var i: integer;
     Sym: SymPointer;
     NullName: Name;
     Dot: VAddress;
 
 begin { Pass2 }
  Write('Reading symbol table');
  NullName := '          ';
  for i := 0 to HashMax do SymTable[i] := nil;
  LoadCurs;
  while not eof(SymFile) do
   begin
    StreamProgress(SymFile);
    if SymFile^.SymType in [LabelType, XYType, ConstType] then
     begin
      New(CurSym);
      CurSym^ := SymFile^;
      with CurSym^ do
       if SymType = LabelType then
        if not LabelDefined then
         begin Writeln; Write('Undefined label ',PrintName); Oops;
          LabelAddress := NoVAddress
         end;
      EnterSym;
      {** Write('.') **}
     end;
    Get(SymFile)
   end;
  Close(SymFile);
  Writeln;
  Write('Resolving forward references');
  Dot := 0;
  LoadCurs;
  while not Eof(RelFile) do with IntFile^ do
   begin
    StreamProgress(RelFile);
    IntFile^ := RelFile^;
    TargtVA := NoVAddress;
    if Target <> NullName then
     begin
      Sym := SearchSym(Target);
      if Sym = nil then
       begin Writeln; Write('Unresolved reference to ',Target); Oops end
      else TargtVA := Sym^.LabelAddress;
      {** Write('.') **}
     end;
    if not (JMPf in Used) then
     if Zf in Used then JMP := NextJmp
     else
      begin Used := Used + [JMPf,Zf]; TargtVA := Dot + 1; JMP := GotoJmp end
    else
     if JMP in [JumpZeroJmp,RepeatLoopJmp,ReturnJmp,LoopJmp,NextJmp] then
      Used := Used - [JMPf]
     else
      if (JMP = NextInstJmp) and (H = true) then { ReviveVictim }
       Used := Used - [JMPf]
      else
       if (JMP in [GotosJmp,CallsJmp]) and (CND = FalseCnd) then
        Used := Used - [JMPf];
    Dot := Dot + 1;
    Get(RelFile);
    Put(IntFile)
   end;
  Last := Dot - 1;
  if Dot > TotalFree then
   begin Writeln; O(Dot); Write(' total instructions.  Available memory of ');
    O(TotalFree); Write(' words is not enough.'); Oops
   end;
  Close(RelFile);
  Close(IntFile);
  {***
  for i := 0 to HashMax do
   while SymTable[i] <> nil do
    begin CurSym := SymTable[i];
     SymTable[i] := CurSym^.Next;
     Dispose(CurSym)
    end;
  ***}
  Writeln
 end { Pass2 };
 
 
 
 
 { ----------------------------------------- }
 {                                           }
 { Pass3:  Find basic groups: those groups   }
 {           of instructions which must be   }
 {           in contiguous memory.           }
 {                                           }
 { ----------------------------------------- }
  
  
 procedure Pass3;
 var PStart: integer;
     VStart, Dot, i: VAddress;
     Adjacent: boolean;
     
     
  procedure GatherBasicGroup;
  begin { GatherBasicGroup }
   VStart := Dot;
   PStart := NoPAddress;
   repeat
    StreamProgress(IntFile);
    with IntFile^, M^[Dot] do
     begin Head := VStart;
      BasicHead := false;
      if TargtVA = NoVAddress then TKind := NoTarget
      else
       if JMPf in Used then
        if JMP in [NextInstJmp,VectorJmp] then TKind := AnyPage
        else
         if (SFf in Used) or (SF <> NopSF) then TKind := SamePage
         else TKind := AnyPage
       else TKind := NoTarget;
      TA := TargtVA;
      if PosMask <> 0 then
       begin
        if PosMask <> -1 then
         begin Writeln; Write('At VA='); O(Dot); 
          Write(', Partial PosMask not implemented.'); Oops;
          VError := true
         end;
        if not P[PosVal div PageSize].Usable[PosVal mod PageSize] then
         begin Writeln; Write('At VA='); O(Dot);
          Write(', PosVal='); O(PosVal);
          Write(' is outside of usable memory.'); Oops;
          VError := true
         end
        else
         if PStart = NoPAddress then PStart := PosVal - (Dot - VStart)
         else
          if PStart + (Dot - VStart) <> PosVal then
           begin Writeln; Write('VA='); O(Dot);
            Write(' assigned to both PA='); O(PStart + (Dot - VStart));
            Write(' and PA='); O(PosVal);
            Write('by the source program.'); Oops;
            VError := true
           end
       end;
      Adjacent := JMP in [CallJmp,PushLoadJmp,CallsJmp,RepeatLoopJmp,
                          RepeatJmp,LoadsJmp,NextJmp];
      if not Adjacent then
       if CNDf in Used then
        Adjacent := ((CND <> TrueCnd) and
                     (JMP in [GotoJmp,VectorJmp,ReturnJmp,JumpPopJmp])) or
                    ((CND <> FalseCnd) and
                     (JMP in [LoopJmp,ThreeWayBranchJmp]))
       else
        Adjacent := Jmp in [LoopJmp,ThreeWayBranchJmp];
      Next := Adjacent
     end;
    Dot := Dot + 1;
    Get(IntFile)
   until not Adjacent or Eof(IntFile)
  end { GatherBasicGroup };
  
   
 begin { Pass3 }
  CreateSegment(MSeg,2,2,100);
  New(MSeg,1,M);
  Write('Finding basic groups');
  for Dot := 0 to MaxVAddress do
   with M^[Dot] do
    begin Tkind := NoTarget;
     TA := NoVAddress;
     PA := NoPAddress;
     VError := false;
     Next := false;
     Head := NoVAddress;
     BasicHead := false
    end;
  Reset(IntFile,IntName);
  Dot := 0;
  while not Eof(IntFile) do
   begin
    GatherBasicGroup;
    with M^[VStart] do
     begin Head := NoVAddress;
      BasicHead := true;
      NextGroup := VStart;
      Done := false;
      PA := PStart
     end;
    {** Write('.') **}
   end;
  M^[Dot].BasicHead := true;
  Last := Dot - 1;
  with M^[Last] do
   if Next or ((Tkind <> NoTarget) and (TA = Dot)) then
    begin Writeln; Write('Last instruction in program has no successor.');
     Oops;
     Next := false;
     Tkind := NoTarget;
     TA := NoVAddress;
     VError := true
    end;
  Close(IntFile);
  Writeln
 end { Pass3 };
 
 
 
 
 { ----------------------------------------- }
 {                                           }
 { Pass4:  Find page groups:  those groups   }
 {           of instructions that must be    }
 {           on the same page of memory.     }
 {                                           }
 { ----------------------------------------- }
 
 
 procedure Pass4;
 var I, T1, T2, N: VAddress;
 begin { Pass4 }
  Write('Finding page groups');
  LoadCurs;
  for I := 0 to Last do
   begin
    ComputeProgress(I,Last);
    if M^[I].TKind = SamePage then
     begin T1 := I;
      T2 := M^[I].TA;
      while M^[T1].Head <> NoVAddress do T1 := M^[T1].Head;
      while M^[T2].Head <> NoVAddress do T2 := M^[T2].Head;
      if T1 <> T2 then { combine two page groups into one }
       begin M^[T2].Head := T1;
        N := M^[T1].NextGroup;
        M^[T1].NextGroup := M^[T2].NextGroup;
        M^[T2].NextGroup := N
       end;
      {** Write('.') **}
     end
   end;
  Writeln
 end { Pass4 };
 
 
 
 
 { ----------------------------------------- }
 {                                           }
 { Pass5:  Assign basic groups which contain }
 {            wired-down instructions.       }
 {         Assign page groups which contain  }
 {            the basic groups which have    }
 {            been assigned.                 }
 {                                           }
 { ----------------------------------------- }
  
  
 procedure Pass5;
 var I, J: VAddress;
 begin { Pass5 }
  Write('Assigning page groups containing wired-down instructions');
  LoadCurs;   { cursor runs down during passes 5 and 6 }
  MicroCount := 1;
  for I := 0 to Last do
   if M^[I].BasicHead then
    if M^[I].PA <> NoPAddress then ReserveMemory(I,M^[I].PA);
  PrintErrors := true;
  for I := 0 to Last do with M^[I] do
   if BasicHead then
    if (PA <> NoPAddress) and not Done then
     begin Page := PA div PageSize;
      P[Page].Search := MaxPageAddr;
      J := I;
      repeat J := M^[J].NextGroup;
       if M^[J].PA = NoPAddress then
        ReserveMemory(J,FindHole(BasicSize(J)));
       M^[J].Done := true
      until J = I
     end;
  Writeln
 end { Pass5 };
 
 
 
 
 { ----------------------------------------- }
 {                                           }
 { Pass6:  Assign all other page groups.     }
 {                                           }
 { ----------------------------------------- }
 
 
 procedure Pass6;
 var I, J: VAddress;


  procedure MeasurePageGroup(V: VAddress);
  var Start: VAddress;
      Size: integer;
  begin { MeasurePageGroup }
   Start := V;
   Largest := 0;
   Total := 0;
   repeat Size := BasicSize(V);
    if Size > Largest then Largest := Size;
    Total := Total + Size;
    V := M^[V].NextGroup
   until V = Start
  end { MeasurePageGroup };
  
  
  procedure FindPage;
  var Found: boolean;
  begin { FindPage }
   Page := 0;
   Found := false;
   PrintErrors := false;
   repeat
    if P[Page].Free > 0 then
     if (P[Page].Free >= Total) and (Largest = 1) then Found := true
     else
      if P[Page].Free > Total + 15 then
       Found := FindHole(Largest) <> NoPAddress
      else
       if Largest = Total then
        Found := FindHole(Largest) <> NoPAddress;
    Page := Page + 1
   until Found or (Page = NoPage);
   if not Found then { last ditch effort }
    begin Page := 0;
     repeat
      if P[Page].Free >= Total then
       Found := FindHole(Largest) <> NoPAddress;
      Page := Page + 1
     until Found or (Page = NoPage)
    end;
   PrintErrors := true;
   if Found then Page := Page - 1
   else
    begin Writeln;
     Write('Cannot find a page for the page group including VA=');
     O(I);
     {** Write('.'); **}
     Oops
    end
  end { FindPage };
  
  
 begin { Pass6 }
  Write('Assigning the remaining page groups');
  for I := 0 to Last do
   if M^[I].BasicHead then
    if not M^[I].Done then
     begin MeasurePageGroup(I);
      FindPage;
      P[Page].Search := MaxPageAddr;
      J := I;
      repeat J := M^[J].NextGroup;
       if Page <> NoPage then ReserveMemory(J,FindHole(BasicSize(J)));
       M^[J].Done := true
      until J = I
     end;
  Writeln
 end { Pass6 };




 { ----------------------------------------- }
 {                                           }
 { Pass7:  Fix up instructions and write the }
 {           file x.MICRO.BIN.               }
 {         Print the listing.                }
 {                                           }
 { ----------------------------------------- }
 
 
 procedure Pass7;
 const TabCh = Chr(#010);    { Tab }
       FFCh  = Chr(#014);    { FormFeed }
 type Mapping = array[0..4095] of VAddress;
 var LineNumber, LinesRemaining, PageCnt, ThisPage, i: integer;
     VDot, VTarget: VAddress;
     PDot, PTarget: PAddress;
     OptLine: String;
     Title, Subtitle: String;
     Include: Boolean;
     IncludeName: String;
     IncludeFile: Text;
     ListOn, NewListOn: Boolean;
     MapSeg: Integer;
     Map: ^Mapping;
     MinPDot, MaxPDot: Integer;
     RefFile, IntRefFile: RefFileType;
     CRefLine: Integer;
 
  
   
  procedure FormFeed;
  begin { FormFeed }
   PageCnt := PageCnt + 1;
   if not ListToConsole then Write(LstFile,FFCh);
   Write(LstFile, '   ');
   if Length(Title) > 46 then Title := SubStr(Title,1,46);
   Writeln(LstFile,Title, ' ':49 - Length(Title),SubTitle);
   Write(LstFile,'   VA    PA   X   Y A B W H AL F SF   Z CN JP   VT  File: ');
   if Include then Write(LstFile,IncludeName)
   else Write(LstFile,SrcName);
   Writeln(LstFile,'   Perq Microcode  Page ',PageCnt);
   Writeln(LstFile);
   LinesRemaining := LinesPerPage;
  end { FormFeed };
  
  
  procedure EndPage;
  begin { EndPage }
   LinesRemaining := 0
  end { EndPage };
  
  
  procedure CountLine;
  begin { CountLine }
   if ListOn then
    begin
     if LinesRemaining = 0 then FormFeed;
     LinesRemaining := LinesRemaining - 1
    end
  end { CountLine };
  
  
  procedure WriteOct(Val,Width: Integer);
  var D: array[1..7] of char;
      K: integer;
  begin { WriteOct }
   K := 0;
   repeat K := K + 1;
    D[K] := Chr(Val mod 8 + Ord('0'));
    Val := Val div 8
   until Val = 0;
   while Width > K do
    begin LstFile^ := ' '; Put(LstFile); Width := Width - 1 end;
   repeat LstFile^ := D[K]; Put(LstFile); K := K - 1
   until K = 0
  end { WriteOct };
  
  
  procedure Option( var F: Text );
  var Word: String;
      OptLineLen: Integer;
      O: MOption;
   
   
   procedure Advance;
   begin { Advance }
    OptLineLen := OptLineLen + 1;
    OptLine[0] := Chr(OptLineLen);
    if F^ in [' ', TabCh, FFCh] then OptLine[OptLineLen] := ' '
    else OptLine[OptLineLen] := F^;
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
    while not (F^ in [' ', TabCh, FFCh]) and not Eoln(F) do
     begin Len := Len + 1;
      Word[Len] := F^;
      Advance
     end;
    Word[0] := Chr(Len)
   end { ReadWord };
   
   
   procedure ReadString( var S: String );
   var Len: Integer;
   begin { ReadString }
    Len := 0;
    S[0] := Chr(80);
    while not Eoln(F) do
     begin
      if Len < 80 then
       begin Len := Len + 1;
        if F^ in [' ', TabCh, FFCh] then S[Len] := ' '
        else S[Len] := F^
       end;
      Advance
     end;
    S[0] := Chr(Len)
   end { ReadString };
   
   
  begin { Option }
   OptLineLen := 0;
   Advance;  { skip '$' }
   SkipBlanks;
   ReadWord;
   O := MicroOption(Word);
   case O of
    OInclude: begin SkipBlanks;
               ReadWord;
               EndLine;
               if Include then
                begin Writeln('+++ ', OptLine, ' +++');
                 Write('"$Include" not allowed in an included file.');
                 Oops
                end
               else
                begin EndPage;
                 Writeln;
                 Write('Reading ', Word);
                 Reset(IncludeFile,Word);
                 IncludeName := Word
                end;
               Include := True
              end;
   OTitle:    begin SkipBlanks;
               if Title = '' then ReadString(Title)
               else ReadString(Subtitle)
              end;
   ONoList:   begin EndLine; NewListOn := False end;
   OList:     begin EndLine; ListOn := True; NewListOn := True; EndPage end;
   OPerq1,
   OPerq1A,
   OBase,
   ONoBase:   EndLine;
   OUnknown:  begin EndLine;
               Writeln('+++ ', OptLine, ' +++');
               Write('Unknown assembler option.');
               Oops
              end
    end;
   Readln(F)
  end { Option };
   
   
  procedure CopyLine( LeadingBlanks: Boolean );
  
  
   procedure Copy( var F: Text );
   var i: integer;
   begin { Copy }
     begin i := 0;
      if ListOn then
       begin
        while (F^ = ' ') and not Eoln(F) and (i < 80) do
         begin i := i + 1; Get(F) end;
        if (F^ = ' ') and LeadingBlanks then { blank line }
         begin
          LinesRemaining := LinesRemaining + 1;
          if LinesRemaining < LinesLeft then { eject } EndPage;
          if LinesRemaining <> 0 then
           { we're not about to print at the top of a page }
           begin { print a blank line }
            CountLine;
            Writeln(LstFile)
           end
         end
        else
         begin
          if LeadingBlanks then Write(LstFile, ' ':52 + i)
          else Write(LstFile, ' ':2 + i);
          while not Eoln(F) and (i < 80) do
           begin i := i + 1;
            if F^ = FFCh then
             begin Get(F);
              Writeln(LstFile);
              EndPage;
              if not Eoln(F) and (i < 80) then
               begin CountLine;
                Write(LstFile, ' ':52 + i)
               end
             end
            else
             begin LstFile^ := F^;
              Put(LstFile);
              Get(F)
             end
           end;
          Writeln(LstFile)
         end
       end;
      Readln(F);
     end;
    if Eof(F) and Include then
     begin Close(IncludeFile);
      Include := False;
      EndPage;
      Writeln;
      Write('Reading ', SrcName)
     end
   end { Copy };
  
  
  begin { CopyLine };
   if OptLine <> '' then
    begin
     if ListOn then
      begin Write(LstFile, ' ':52);
       if Length(OptLine) > 80 then Write(LstFile, OptLine:80)
       else Write(LstFile, OptLine);
       Writeln(LstFile)
      end;
     ListOn := NewListOn;
     OptLine := ''
    end
   else
    if Include then Copy(IncludeFile)
    else Copy(SrcFile);
   if Include then
    begin
     if IncludeFile^ = '$' then Option(IncludeFile)
    end
   else
    begin
     if SrcFile^ = '$' then Option(SrcFile)
    end
  end { CopyLine };
  
  
  procedure PrintLine;
  var i: integer;
  begin { PrintLine }
   LineNumber := LineNumber + 1;
   while LineNumber < IntFile^.Line do
    begin
     CountLine;
     repeat
      if Eof(RefFile) then CRefLine := LineNumber + 1
      else CRefLine := RefFile^.RefLine;
      if CRefLine <= LineNumber then
       begin
        if ListOn then
         begin
          IntRefFile^ := RefFile^;
          IntRefFile^.Page := PageCnt;
          IntRefFile^.Line := LinesPerPage - LinesRemaining;
          Put(IntRefFile)
         end;
        Get(RefFile)
       end
     until CRefLine > LineNumber;
     CopyLine(True { please print leading blanks });
     LineNumber := LineNumber + 1
    end;
   with BinFile^ do 
    begin CountLine;
     repeat
      if Eof(RefFile) then CRefLine := LineNumber + 1
      else CRefLine := RefFile^.RefLine;
      if CRefLine <= LineNumber then
       begin
        if ListOn then
         begin
          IntRefFile^ := RefFile^;
          IntRefFile^.Page := PageCnt;
          IntRefFile^.Line := LinesPerPage - LinesRemaining;
          Put(IntRefFile)
         end;
        Get(RefFile)
       end
     until CRefLine > LineNumber;
     if ListOn then
      begin
       if M^[VDot].VError or P[PDot div PageSize].PError[PDot mod PageSize] then
        LstFile^ := '>'
       else LstFile^ := ' ';
       Put(LstFile);
       WriteOct(VDot,4);
       if PDot = NoPAddress then Write(LstFile,'      ')
       else WriteOct(PDot,6);
       WriteOct(bX,4);
       WriteOct(bY,4);
       WriteOct(bA,2);
       WriteOct(bB,2);
       WriteOct(bW,2);
       WriteOct(bH,2);
       WriteOct(bALU,3);
       WriteOct(bF,2);
       WriteOct(bSF,3);
       WriteOct(bZ,4);
       WriteOct(bCND,3);
       WriteOct(bJMP,3);
       if (VTarget = NoVAddress) or (VTarget = VDot + 1) then
        Write(LstFile,'    -')
       else WriteOct(VTarget,5)
      end;
     CopyLine(False { please don't print leading blanks })
    end
  end { PrintLine };
  
  
  procedure PrintMapping;
  var Last: PAddress;
      I: Integer;
      Found, Printed: Boolean;
  begin { PrintMapping }
   if Listing and ListOn then
    begin
     Writeln;
     Writeln('Writing PA to VA mapping.');
     SubTitle := 'PA to VA mapping.';
     EndPage;
     Last := MaxPAddress;
     while (Map^[LAnd(Last,#7777)] = NoVAddress) and (Last > 0) do
      Last := Last - 1;
     Printed := True;
     PDot := LAnd(MinPDot,LNot(7));
     repeat
      I := PDot;
      Found := False;
      for I := PDot to PDot+7 do
       Found := Found or (Map^[LAnd(I,#7777)] <> NoVAddress);
      if Found then
       begin
        if not Printed and (LinesRemaining <> 0) then
         begin CountLine;
          Writeln(LstFile)
         end;
        if LinesRemaining = 0 then
         begin
          CountLine;
          Write(LstFile, '    PA:');
          for I := 0 to 7 do Write(LstFile, '      +', I:1);
          Writeln(LstFile);
          CountLine;
          Writeln(LstFile)
         end;
        CountLine;
        Write(LstFile, ' ');
        WriteOct(PDot,5);
        Write(LstFile, ':');
        for I := PDot to PDot+7 do
         if Map^[LAnd(I,#7777)] = NoVAddress then Write(LstFile, '        ')
         else WriteOct(Map^[LAnd(I,#7777)],8);
        Writeln(LstFile);
        Printed := True
       end
      else Printed := False;
      PDot := PDot + 8
     until PDot > MaxPDot;
     DecRefCount(MapSeg)
    end;
  end { PrintMapping };
  
  
  procedure Statistics;
  begin { Statistics }
   if Listing and ListOn then
    begin
     SubTitle := 'Placement statistics.';
     EndPage;
     CountLine;
     Writeln(LstFile,'Placement complete, ',errors:1,' errors.');
     for Page := 0 to MaxPage do
      if P[Page].UsablePage then
       begin CountLine;
        Write(LstFile,'Page '); WriteOct(Page,1);
        Write(LstFile,' has '); WriteOct(P[Page].Free,1);
        Writeln(LstFile,' unused locations.')
       end
    end;
   Writeln;
   Writeln('Placement complete, ',errors:1,' errors.');
   for Page := 0 to MaxPage do
    if P[Page].UsablePage then
     begin Write('Page '); O(Page);
      Write(' has '); O(P[Page].Free);
      Writeln(' unused locations.')
     end
  end { Statistics };
  
  
  procedure PrintCrossReference;
  var ThisName, NextName: Name;
      LastPage, LastLine: Integer;
      Occurrences: Integer;
  begin { PrintCrossReference }
   if Listing and ListOn then
    begin
     Write('Sorting the cross reference');
     Sort(IntRefName);
     Writeln;
     Write('Printing the cross reference');
     SubTitle := 'Cross reference.';
     EndPage;
     Reset(IntRefFile,IntRefName);
     LoadCurs;
     while not Eof(IntRefFile) do
      begin
       ThisName := IntRefFile^.PrintName;
       LastPage := -1;
       LastLine := -1;
       Occurrences := 0;
       repeat
        StreamProgress(IntRefFile);
        if (LastPage <> IntRefFile^.Page) or
           (LastLine <> IntRefFile^.Line) then
         begin
          if Occurrences mod 10 = 0 then
           begin
            if LinesRemaining = 0 then
             begin
              CountLine;
              Writeln(LstFile, 'Name         Value Type   References');
              CountLine;
              Writeln(LstFile)
             end;
            CountLine;
            if Occurrences = 0 then
             begin
              CurSym := SearchSym(ThisName);
              Write(LstFile,ThisName);
              if CurSym = nil then Write(LstFile,' ':10)
              else
               with CurSym^ do
                case SymType of
                 LabelType:        Write(LstFile,LabelAddress:8:-8, ' L');
                 XYType:           Write(LstFile,XYAddress:8:-8,' R');
                 ReservedWordType: Write(LstFile,'  Reserved');
                 ConstType:        Write(LstFile,ConstValue:8:-8,' C')
                 end
             end
            else Write(LstFile,' ':20)
           end;
          Write(LstFile, IntRefFile^.Page:7, '/', IntRefFile^.Line:1);
          if IntRefFile^.Line < 10 then Write(LstFile, ' ');
          Occurrences := Occurrences + 1;
          if Occurrences mod 10 = 0 then Writeln(LstFile);
          LastPage := IntRefFile^.Page;
          LastLine := IntRefFile^.Line
         end;
        Get(IntRefFile);
        if Eof(IntRefFile) then NextName := '          '
        else NextName := IntRefFile^.PrintName
       until NextName <> ThisName;
       if Occurrences mod 10 <> 0 then Writeln(LstFile)
      end;
     Close(IntRefFile);
     Writeln
    end
  end { PrintCrossReference };
  
  
  procedure InitPass7;
  begin { InitPass7 }
   Write('Writing the binary');
   if Listing then Write(' and the listing');
   Reset(IntFile,IntName);
   Reset(RefFile,RefName);
   IntRefName := 'PrqPlace.Scratch.0';
   Rewrite(IntRefFile,IntRefName);
   P[NoPage].PError[NoPageAddr] := true;
   LinesRemaining := 0;
   PageCnt := 0;
   VDot := 0;
   LineNumber := 0;
   Title := '';
   Subtitle := '';
   OptLine := '';
   Include := False;
   ListOn := True;
   NewListOn := True;
   MinPDot := #77777;
   MaxPDot := 0;
   if Listing then
    begin
     Writeln;
     Write('Reading ', SrcName);
     if SrcFile^ = '$' then Option(SrcFile);
     CreateSegment(MapSeg,2,2,100);
     New(MapSeg,1,Map);
     for PDot := 0 to 4095 do Map^[LAnd(PDot,#7777)] := NoVAddress
    end;
   LoadCurs
  end { InitPass7 };
  
    
 begin { Pass7 }
  InitPass7;
  while not eof(IntFile) do with IntFile^, BinFile^, M^[VDot] do
   begin
    StreamProgress(IntFile);
    PDot := PA;
    if PDot > MaxPDot then MaxPDot := PDot;
    if PDot < MinPDot then MinPDot := PDot;
    VTarget := TA;
    if Listing then Map^[LAnd(PDot,#7777)] := VDot;
    PTarget := M^[VTarget].PA;
    if Next then { Next VA must be next PA also }
     if M^[VDot+1].PA <> PDot+1 then
      begin Writeln; Write('VA='); O(VDot); Write(' and VA='); O(VDot+1);
       Write(' should be adjacent in physical memory, but they''re not!');
       Oops;
       M^[VDot+1].VError := true
      end;
    if TKind = SamePage then { PA of TAddress must be on same page as VDot }
     if M^[VDot].PA div PageSize <> PTarget div PageSize then
      begin Writeln; Write('VA='); O(VDot); Write(' and VA='); O(TA);
       Write(' should be on the same page, but they''re not!');
       Oops;
       VError := true
      end;
    bADRS := PDot;
    bJMP := Ord(JMP);
    bCND := Ord(CND);
    bZ := Ord(Z);
    bSF := Ord(SF);
    bF := Ord(F.SFuse);
    bALU := Ord(ALU);
    bH := Ord(H);
    bW := Ord(W);
    bB := Ord(B);
    bA := Ord(A);
    bY := Ord(Y);
    bX := Ord(X);
    case TKind of
     NoTarget: ;
     SamePage: bZ := 255 - PTarget mod 256;
     AnyPage: if JMP in [NextInstJmp, VectorJmp] then
               begin
                if ((JMP = NextInstJmp) and (LAnd(PTarget,#1774) <> 0)) or
                   ((JMP = VectorJmp) and (LAnd(PTarget,#74) <> 0)) then
                 begin
                  Writeln;
                  Write('At VA='); O(VDot);
                  Write(' target of ');
                  if JMP = NextInstJmp then Write('NextInst')
                  else Write('Vector or Dispatch');
                  Write(' (VA='); O(VTarget);
                  Write(', PA='); O(PTarget);
                  Write(') must have zero in bits ');
                  if JMP = NextInstJmp then Write('9')
                  else Write('5');
                  Write(':2');
                  Oops
                 end;
                bZ := 255 - (PTarget div 16 + PTarget mod 4)
               end
              else
               begin bF := Ord(LJSF);
                bZ := 255 - PTarget mod 256;
                bSF:= 15 - PTarget div 256
               end
     end;
    if Listing then PrintLine;
    VDot := VDot + 1;
    {** Write('.'); **}
    Put(BinFile);
    Get(IntFile)
   end;
  DecRefCount(MSeg);
  if Listing then
   while not Eof(SrcFile) do
    begin
     CountLine;
     repeat
      if Eof(RefFile) then CRefLine := LineNumber + 1
      else CRefLine := RefFile^.RefLine;
      if CRefLine <= LineNumber then
       begin
        if ListOn then
         begin
          IntRefFile^ := RefFile^;
          IntRefFile^.Page := PageCnt;
          IntRefFile^.Line := LinesPerPage - LinesRemaining;
          Put(IntRefFile)
         end;
        Get(RefFile)
       end
     until CRefLine > LineNumber;
     CopyLine(True { please print leading blanks })
    end;
  Close(RefFile);
  Close(IntRefFile);
  PrintMapping;
  PrintCrossReference;
  Statistics;
  if Listing then
   begin
    if not ListToConsole then
     begin LstFile^ := Chr(FFCh); Put(LstFile) end;
    Close(LstFile);
    Close(SrcFile)
   end;
  Close(IntFile);
  BinFile^.bADRS := -1;
  Put(BinFile);
  Close(BinFile);
  Writeln
 end { Pass7 };




function BasicSize( V: VAddress ): Integer;
var Length: integer;
begin { BasicSize }
 Length := 0;
 repeat V := V + 1;
  Length := Length + 1
 until M^[V].BasicHead;
 BasicSize := Length
end { BasicSize };


function FindHole( Length: integer ): PAddress;
var Scan, Limit, StartHole, EndHole, HoleLength: PageAddress;
begin { FindHole }
 {Writeln; Write('('); O(Page); Write(','); O(Length); write(':');}
 with P[Page] do
  begin Limit := P[Page].Search;
   Scan := Limit;
   repeat Scan := (Scan + 1) mod PageSize
   until Empty[Scan] or (Scan = Limit);
   Limit := (Scan + MaxPageAddr) mod PageSize;
   if not Empty[Scan] then
    begin Writeln;
     Write('FindHole('); O(Page); Write(','); O(Length);
     Writeln(') called with every location filled!');
     Write('FindHole returns NoAddress.'); Oops;
     FindHole := NoPAddress; exit(FindHole)
    end;
   if Scan <> 0 then
    if Empty[Limit] then
     begin Writeln;
      Write('FindHole('); O(Page); Write(','); O(Length);
      Write(') might tight loop!  FindHole returns NoAddress'); Oops;
      FindHole := NoPAddress; exit(FindHole)
     end;
   {write(' Limit='); O(Limit);}
   repeat StartHole := Scan; EndHole := StartHole;
    {writeln; write('StartHole='); O(StartHole);}
    while Empty[EndHole] do
     EndHole := EndHole + 1;
    HoleLength := EndHole - StartHole;
    {write(' EndHole='); O(EndHole); Write(' HoleLength='); O(HoleLength);}
    Scan := EndHole mod PageSize;
    if Scan <> Limit then
     repeat Scan := (Scan + 1) mod PageSize
     until Empty[Scan] or (Scan = Limit)
   until (HoleLength >= Length) or (Scan = Limit)
  end;
 if HoleLength >= Length then
  begin FindHole := StartHole + Page * PageSize;
   P[Page].Search := (StartHole + MaxPageAddr) mod PageSize
   end
 else
  begin FindHole := NoPAddress;
   {write('*');}
   if PrintErrors then
    begin Writeln; Write('Unable to find hole with size '); O(Length);
     Write(' on page '); O(Page);
     {** Write('.'); **}
     Oops
    end
  end;
 {write(')')}
end { FindHole };


procedure ReserveMemory( VA: VAddress; PA: PAddress );
var Page: PageNumber;
    Offset: PageAddress;
    Length: integer;
begin { ReserveMemory }
 Length := 0;
 Page := PA div PageSize;
 Offset := PA mod PageSize;
 if PA = NoPAddress then
  repeat Writeln; Write('Unable to place VA='); O(VA);
   MicroCount := MicroCount + 1;
   ComputeProgress(MicroCount,Last);
   {** Write('.'); **}
   Oops;
   VA := VA + 1
  until M^[VA].BasicHead
 else
  with P[Page] do
   begin
    repeat
     if Empty[Offset] then Length := Length + 1
     else
      if Usable[Offset] then
       begin Writeln; Write('Duplicate placement in PA='); O(PA);
        Write('  (this VA='); O(VA); Write(').'); Oops;
        PError[Offset] := true
       end
      else
       begin Writeln; Write('Attempt to place VA='); O(VA);
        Write(' in PA='); O(PA);
        Write(' which is unavailable for placement.'); Oops;
        M^[VA].VError := true
       end;
     MicroCount := MicroCount + 1;
     ComputeProgress(MicroCount,Last);
     {** Write('.'); **}
     Empty[Offset] := false;
     if M^[VA].PA <> NoPAddress then     (*** debugging ***)
      if M^[VA].PA <> PA then
       begin Writeln; Write('VA='); O(VA);
        Write(' is being assigned PA='); O(PA);
        Write(' but was already assigned PA='); O(M^[VA].PA);
        Writeln
       end;                                   (*** debugging ***)
     M^[VA].PA := PA;
     VA := VA + 1;
     PA := PA + 1;
     Offset := Offset + 1
    until M^[VA].BasicHead or (Offset = NoPageAddr);
    if not M^[VA].BasicHead then
     begin Writeln; Write('Basic group crosses a page boundary at VA=');
      O(VA); Oops;
      M^[VA].VError := true
     end;
    Free := Free - Length
   end
end { ReserveMemory };


 procedure O( N: Integer );
  var D: array[1..7] of char;
      K: integer;
 begin { O }
  K := 0;
  repeat K := K + 1;
   D[K] := Chr(N mod 8 + Ord('0'));
   N := N div 8
  until N = 0;
  repeat Write(D[K]); K := K - 1
  until K = 0
 end { O };
 
 
 procedure Oops;
 var c: char;
 begin { Oops }
  Errors := Errors + 1;
  Writeln;
  Write('+++ type <return> to continue, <^C> to terminate +++');
  Readln
 end { Oops };
 
 
 procedure PrintVMem( First,Last: VAddress );
 var i:integer;
  
  procedure B(V: boolean);
  begin {B} if V then write('T') else write('F') end {B};
  
 begin { PrintVMem }
  for i := First to Last do with M^[i] do
   begin O(i); write(': TKind=');
    if TKind = NoTarget then write('NoTarget')
    else
     if TKind = SamePage then write('SamePage')
     else write('AnyPage ');
    write('  TA='); O(TA);
    write('  PA='); O(PA);
    write('  Head='); O(Head);
    write('  BasicHead='); B(BasicHead);
    if BasicHead then
     begin write('  NextGroup='); O(NextGroup);
      write('  Done='); B(Done)
     end;
    writeln
   end
 end { PrintVMem };
 
 
 procedure PrintPMem( Page: PageNumber );
 var i,s: integer;
     e: boolean;
 begin { PrintPMem }
  with P[Page] do
   begin Write('Page '); O(Page);
    Write(':  Free='); O(Free); Write('  Search='); O(Search);
    Writeln('   empty locations:');
    s := 0; e := Empty[0];
    for i := 1 to MaxPageAddr do
     if e <> Empty[i] then
      if e then
       begin Write('   '); O(s); Write(' - '); O(i-1); Writeln;
        s := i;
        e := not e
       end;
    if e then
     begin Write('   '); O(s); Write(' - '); O(MaxPageAddr); Writeln end
   end
 end { PrintPMem };
   
    
 
 
 procedure Initialize;  {Initializes the symbol table and other vars}
 var PFirst, PLast, FirstPage, LastPage, FirstOffset, LastOffset, i: integer;
     Ch: char;
     
     
  procedure OpenFiles;
  var MI: MicroInstruction;
  begin { OpenFiles }
   Write('Opening files');
   FindFile(RelName);
   FindFile(SymName);
   FindFile(RefName);
   FindFile(SrcName);
   Reset(RelFile,RelName);
   Reset(SymFile,SymName);
   Rewrite(IntFile,IntName);
   if Listing then
    begin Rewrite(LstFile,LstName);
     Reset(SrcFile,SrcName)
    end;
   Rewrite(BinFile,BinName);
   Writeln
  end { OpenFiles };
     
  
  procedure InitPage(Page: PageNumber; First,Last: PageAddress);
  begin { InitPage }
   with P[Page] do
    begin UsablePage := true;
     for i := First to Last do
      if not Empty[i] then
       begin Empty[i] := true;
        PError[i] := false;
        Usable[i] := true;
        Free := Free + 1;
        TotalFree := TotalFree + 1
       end
    end;
  end { InitPage };
  
     
 begin { Initialize }
  Writeln('PrqPlace ', PrqPlaVersion);
  InitOption;
  Machine := OPerq1;
  DEBUG := false;
  Errors := 0;
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,Ignore,' ',Ignore);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  if UsrCmdLine = '' then
   begin
    Write('Root file name(s): '); Readln(Root);
    Write('List file name (<return> for none): '); Readln(LstName);
    Listing := LstName <> ''
   end
  else
   begin GetSymbol(UsrCmdLine,Root,' ',Ignore);
    RemDelimiters(UsrCmdLine,' ',Ignore);
    GetSymbol(UsrCmdLine,LstName,' ',Ignore);
    Listing := LstName <> '';
    RemDelimiters(UsrCmdLine,' ',Ignore);
    if UsrCmdLine <> '' then
     begin Write('unknown parameter: ',UsrCmdLine,' type <return> to continue');
      Readln
     end
   end;
  CnvUpper(LstName);
  ListToConsole := LstName = 'CONSOLE:';
  RelName := Concat(Root, '.Rel');
  SymName := Concat(Root, '.RSym');
  RefName := Concat(Root, '.RRef');
  IntName := Concat(Root, '.Int');
  SrcName := Concat(Root, '.Micro');
  BinName := Concat(Root, '.Bin');
  Writeln;
  OpenFiles;
  with P[0] do
   begin Free := 0;
    Search := MaxPageAddr;
    UsablePage := false;
    for i := 0 to NoPageAddr do
     begin Empty[i] := false; PError[i] := true; Usable[i] := false end
   end;
  for i := 1 to NoPage do P[i] := P[0];
  TotalFree := 0;
  while RelFile^.Line <= 0 do { process preamble }
   begin
    case RelFile^.Line of
     -1: Machine := RelFile^.MachineOption;
      0: begin
          PFirst := RelFile^.First;
          PLast := RelFile^.Last;
          FirstPage := PFirst div PageSize;
          FirstOffset := PFirst mod PageSize;
          LastPage := PLast div PageSize;
          LastOffset := PLast mod PageSize;
          if FirstPage = LastPage then
           InitPage(FirstPage,FirstOffset,LastOffset)
          else
           begin InitPage(FirstPage,FirstOffset,MaxPageAddr);
            if FirstPage+1 < LastPage then
             begin InitPage(FirstPage+1,0,MaxPageAddr);
              for Page := FirstPage+2 to LastPage-1 do
               begin TotalFree := TotalFree + PageSize - P[Page].Free;
                P[Page] := P[FirstPage+1]
               end
             end;
            InitPage(LastPage,0,LastOffset)
           end
         end
     end;
    Get(RelFile)
   end;
 end { Initialize };


 procedure CleanUp;


   procedure Delete( FileName: PathName; NotFoundOk: Boolean );
   
   
     handler DelError( FileName: PathName );
     begin { DelError }
       if NotFoundOk then Exit(Delete)
       else
         begin
           Writeln;
           Writeln('** Cannot delete ', FileName, '.');
           Exit(PrqPlace)
         end
     end { DelError };
   
   
   begin { Delete }
     FSDelete(FileName)
   end { Delete };
 
 
 begin { CleanUp }
   Write('Deleting intermediate files');
   Delete(RelName,False);
   Delete(SymName,False);
   Delete(RefName,False);
   Delete(IntName,False);
   Delete('PrqPlace.Scratch.0',True);
   Delete('PrqPlace.Scratch.1',True);
   Delete('PrqPlace.Scratch.2',True);
   Delete('PrqPlace.Scratch.3',True);
   Writeln
 end { CleanUp };

 
 

begin { PrqPlace }
 Initialize;
 {Pass1;}
 Pass2;
 Pass3;
 Pass4;
 Pass5;
 Pass6;
 Pass7;
 CleanUp
end { PrqPlace }.
