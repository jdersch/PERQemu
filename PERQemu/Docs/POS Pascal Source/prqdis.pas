{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{ R-}
program PrqDis;


imports System from System;
imports Perq_String from Perq_String;
imports CmdParse from CmdParse;


{ PERQ Micro Dis-assembler 
  John P. Strait   11 Oct 80
  Three Rivers Computer Corporation
  Copyright (C) 1980, 1982, 1983 Three Rivers Computer Corp, Pittsburgh, PA
  
  Change log
  
  V1.2  JPS   6 Oct 81  Changes for Perq1A.
  
  V1.1  JPS   4 Apr 81  Convert to POS version C.
  
  V1.0  JPS  11 Oct 80  Start program.
}


const PrqDisVersion = '1.2';
      ShiftTarget = -1;    { bad address used to flag Goto(Shift) }
      MaxPhysAddr = #37777;

type XYRange = 0..255;  {X and Y are 8 bit fields of MicroInstruction (MI)}
     PhysicalAddress = 0..MaxPhysAddr;

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
      MemFuncType = (UnusedM0, UnusedM1, UnusedM2, UnusedM3, UnusedM4,
                     UnusedM5, UnusedM6, UnusedM7, Fetch4Reverse,
                     Store4Reverse, Fetch4, Store4, Fetch2, Store2,
                     Fetch, Store);
      CNDtype = (TrueCnd, FalseCnd, IntrPendCnd, Eql20Cnd, BPC3Cnd, C19Cnd,
                 OddCnd, ByteSignCnd,  NeqCnd, LeqCnd, LssCnd, OvrflwCnd,
                 CarryCnd, EqlCnd, GtrCnd, GeqCnd);
      JMPtype = (JumpZeroJmp,CallJmp,NextInstJmp,GotoJmp,PushLoadJmp,
                   CallSJmp, VectorJmp, GotoSJmp, RepeatLoopJmp,
                   RepeatJmp, ReturnJmp, JumpPopJmp, LoadSJmp, LoopJmp,
                   NextJmp, ThreeWayBranchJmp);
      
      MicroBinary =
                packed record
                    Adrs: Integer;
                    JMP:  JMPtype;
                    CND:  CNDtype;
                    Z:    0..255;
                    SF:   0..15  { case Ftype of
                                    LongConstant:    (SF: SFtype);
                                    ShiftFunction:   (ShiftSF: SFtype);
                                    MemCntl:         (MemFuncSF: MemFuncType);
                                    LongJump:        (LJSF: 0..15);
                                  };
                    F:    0..3;
                    ALU:  ALUtype;
                    H:    boolean;
                    W:    boolean;
                    B:    BMUXtype;
                    A:    AMUXtype;
                    Y:    XYRange;
                    X:    XYRange;
                end;
                
var CodFileName, OutFileName, Ignore: String;
    CodFile: file of MicroBinary;
    OutFile: Text;
    Address: packed array[PhysicalAddress] of packed record
              Referenced, Assembled: boolean
              end;
    Register: packed array[XYRange] of boolean;
    First, Last: Integer;
     


 procedure Initialize;  {Initializes the symbol table and other vars}
 var i: integer;
 begin { Initialize }
  Reset(Input);
  Rewrite(Output);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,Ignore,' ',Ignore);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  if UsrCmdLine = '' then
   begin
    Write('Code root name: ');
    Readln(UsrCmdLine);
   end;
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,CodFileName,' ',Ignore);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  if UsrCmdLine = '' then
   begin
    Write('Output file name (<return> for same name): ');
    Readln(UsrCmdLine);
   end;
  RemDelimiters(UsrCmdLine,' ',Ignore);
  GetSymbol(UsrCmdLine,OutFileName,' ',Ignore);
  RemDelimiters(UsrCmdLine,' ',Ignore);
  if OutFileName = '' then OutFileName := Concat(CodFileName,'.Micro');
  CodFileName := Concat(CodFileName,'.Bin');
  if UsrCmdLine <> '' then
   begin Write('unknown parameter: ',UsrCmdLine,' <return> to continue');
    Readln
   end;
  for i := 0 to MaxPhysAddr do with Address[i] do
   begin Referenced := false; Assembled := false end;
  for i := 0 to 255 do Register[i] := false;
  First := MaxPhysAddr + 1;
  Last := -1
 end { Initialize };
 
 
 procedure Pass1;  { Determine label and register use }
 var CurMB: MicroBinary;
     SimpleTarget, FancyTarget, S, T: Integer;
     PrintJump, PrintTest: Boolean;
 begin { Pass1 }
  Reset(CodFile,CodFileName);
  while not Eof(CodFile) and (CodFile^.Adrs >= 0) do
   begin CurMB := CodFile^;
    Get(CodFile);
    with CurMB do
     begin
      if Adrs < First then First := Adrs;
      if Adrs > Last then Last := Adrs;
      Address[Adrs].Assembled := true;
      if ((A = XSrc) and not (Alu in [Balu, NotBalu]))
         or W then
       Register[X] := true;
      if (B = YRam) and not (Alu in [Aalu, NotAalu]) then Register[Y] := true;
      PrintTest := true;
      if Cnd = TrueCnd then PrintTest := Jmp in [GotoSJmp, CallSJmp]
      else
       if Cnd = FalseCnd then PrintTest := not (Jmp in [GotoSJmp, CallSJmp]);
      S := LXor(Z,#377);
      if F = 3 then SimpleTarget := Shift(LXor(SF,#17),8) + S
      else
       if (F = 1) and (Recast(SF,Perq1ASFtype) = LeapSF) then
        SimpleTarget := Shift(LAnd(Y,#77),8) + S
       else
        if (F = 1) and (Recast(SF,Perq1ASFtype) = GotoShiftSF) then
         SimpleTarget := ShiftTarget
        else SimpleTarget := LAnd(Adrs,#7400) + S;
      T := LAnd(S,#3);
      S := LAnd(S,#374);
      FancyTarget := Shift(S,4) + T;
      PrintJump := PrintTest;
      if not PrintJump then
       if Jmp = GotoJmp then PrintJump := SimpleTarget <> CodFile^.Adrs
       else
        if Jmp = NextJmp then PrintJump := Adrs + 1 <> CodFile^.Adrs
        else PrintJump := true;
       if PrintJump then
        if Jmp in [CallJmp, GotoJmp, PushLoadJmp, CallSJmp, GotoSJmp,
                   RepeatJmp, JumpPopJmp, ThreeWayBranchJmp] then
         begin
          if SimpleTarget <> ShiftTarget then
           Address[SimpleTarget].Referenced := true
         end
        else
         if ((Jmp = NextInstJmp) and not H) or
            (Jmp = VectorJmp) then
          Address[FancyTarget].Referenced := true
     end
   end
 end { Pass1 };
 
  
 procedure Pass2;  { Print dis-assembled listing }
 var CurMB: MicroBinary;
     OutL: Integer;
     OutS: packed array[1..150] of char;
 
 
  procedure Preamble;
  var i: integer;
  begin { Preamble }
   Writeln(OutFile, '!       ', CodFileName);
   Writeln(OutFile);
   for i := 0 to 255 do
    if Register[i] then
     Writeln(OutFile, '        Define(R', i:1:8, ',', i:1:8, ');');
   Writeln(OutFile);
   for i := 0 to MaxPhysAddr do with Address[i] do
    if Referenced and not Assembled then
     Writeln(OutFile, '        Constant(C', i:1:8, ',', i:1:8, ');');
   Writeln(OutFile);
   Writeln(OutFile, '        Place(', First:1:8, ',', Last:1:8, ');');
   Writeln(OutFile)
  end { Preamble };
  
  
  procedure Character( C: Char );
  begin { Character }
   OutL := OutL + 1;
   OutS[OutL] := C
  end { Character };
  
  
  procedure Number( X: integer );
  var D: array[1..8] of Char;
      N, K: Integer;
  begin { Number }
   N := 0;
   repeat N := N + 1;
    D[N] := Chr(LAnd(X,7) + Ord('0'));
    X := Shift(X,-3)
   until X = 0;
   for K := N downto 1 do Character(D[K])
  end { Number };
  
  
  procedure CharString( S: String );
  var K: integer;
  begin { CharString }
   for K := 1 to Length(S) do Character(S[K])
  end { CharString };
  
  
  procedure Target( T: integer );
  begin { Target }
   if T = ShiftTarget then CharString('Shift')
   else
    begin
     if Address[T].Assembled then Character('L') else Character('C');
     Number(T)
    end
  end { Target };
   
 
 
  procedure Instruction;
  var CodePrinted, PrintBlankLine: boolean;
      i: integer;
   
                   
   procedure Comma;
   begin { Comma }
    if CodePrinted then Character(',');
    CodePrinted := true
   end { Comma };
   
   
   procedure Test;
   var PrintTest, PrintJump: boolean;
       SimpleTarget, FancyTarget, S, T: integer;
 
 
    procedure Jump;
    begin { Jump }
     with CurMB do
      begin
       case Jmp of
        JumpZeroJmp:       CharString(' JumpZero');
        CallJmp:           begin CharString(' Call(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        NextInstJmp:       if H then CharString(' ReviveVictim')
                           else
                            begin CharString(' NextInst(');
                             Target(FancyTarget);
                             Character(')')
                            end;
        GotoJmp:           begin CharString(' Goto(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        PushLoadJmp:       begin CharString(' PushLoad(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        CallSJmp:          begin CharString(' CallS(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        VectorJmp:         begin
                            if H then CharString(' Dispatch(')
                            else CharString(' Vector(');
                            Target(FancyTarget);
                            Character(')')
                           end;
        GotoSJmp:          begin CharString(' GotoS(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        RepeatLoopJmp:     CharString(' RepeatLoop');
        RepeatJmp:         begin CharString(' Repeat(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        ReturnJmp:         CharString(' Return');
        JumpPopJmp:        begin
                            if H then CharString(' LeapPop(')
                            else CharString(' JumpPop(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        LoadSJmp:          begin CharString(' LoadS(');
                            Target(SimpleTarget);
                            Character(')')
                           end;
        LoopJmp:           CharString(' Loop');
        NextJmp:           CharString(' Next');
        ThreeWayBranchJmp: begin CharString(' ThreeWayBranch(');
                            Target(SimpleTarget);
                            Character(')')
                           end
        end;
       PrintBlankLine := not (PrintTest or
                              (Jmp in [CallJmp, CallSJmp, PushLoadJmp,
                                       RepeatLoopJmp, RepeatJmp, LoadSJmp,
                                       LoopJmp, ThreeWayBranchJmp]))
      end
    end { Jump };
 
                
   begin { Test }
    with CurMB do
     begin PrintTest := true;
      if Cnd = TrueCnd then PrintTest := Jmp in [GotoSJmp, CallSJmp]
      else
       if Cnd = FalseCnd then PrintTest := not (Jmp in [GotoSJmp, CallSJmp]);
      S := LXor(Z,#377);
      if F = 3 then SimpleTarget := Shift(LXor(SF,#17),8) + S
      else
       if (F = 1) and (Recast(SF,Perq1ASFtype) = LeapSF) then
        SimpleTarget := Shift(LAnd(Y,#77),8) + S
       else
        if (F = 1) and (Recast(SF,Perq1ASFType) = GotoShiftSF) then
         SimpleTarget := ShiftTarget
        else SimpleTarget := LAnd(Adrs,#7400) + S;
      T := LAnd(S,#3);
      S := LAnd(S,#374);
      FancyTarget := Shift(S,4) + T;
      PrintJump := PrintTest;
      if not PrintJump then
       if Jmp = GotoJmp then PrintJump := SimpleTarget <> CodFile^.Adrs
       else
        if Jmp = NextJmp then PrintJump := Adrs + 1 <> CodFile^.Adrs
        else PrintJump := true;
      if PrintTest or PrintJump then Comma;
      if PrintTest then
       begin CharString(' if');
        case Cnd of
         TrueCnd      : CharString(' True');
         FalseCnd     : CharString(' False');
         IntrPendCnd  : CharString(' IntrPend');
         Eql20Cnd     : CharString(' Eql20');
         Bpc3Cnd      : CharString(' Bpc[3]');
         C19Cnd       : CharString(' C19');
         OddCnd       : CharString(' Odd');
         ByteSignCnd  : CharString(' ByteSign');
         NeqCnd       : CharString(' Neq');
         LeqCnd       : CharString(' Leq');
         LssCnd       : CharString(' Lss');
         OvrFlwCnd    : CharString(' OverFlow');
         CarryCnd     : CharString(' Carry');
         EqlCnd       : CharString(' Eql');
         GtrCnd       : CharString(' Gtr');
         GeqCnd       : CharString(' Geq')
         end
       end;
      if PrintJump then Jump
     end
   end { Test };
   
   
   
   procedure SpecialFunction;
   var Hi, Lo: integer;
   begin { SpecialFunction }
    with CurMB do
     begin
      case Recast(F,SFUseType) of
       SF1,
       SF2:
        if Recast(SF,SFType) in [ShftOnRSF, StackResetSF, PushSF, PopSF,
                                 CntlRoGetsSF, LoadOpSF, WcsLowGetsSF,
                                 WcsMidGetsSF, WcsHiGetsSF,
                                 IobFunctionSF] then
         begin Comma;
          case Recast(SF,SFType) of
           ShftOnRSF:     CharString(' ShiftOnR');
           StackResetSF:  CharString(' StackReset');
           PushSF:        CharString(' Push');
           PopSF:         CharString(' Pop');
           CntlRoGetsSF:  begin CharString(' CntlRasterOp(');
                           Number(Z);
                           Character(')')
                          end;
           LoadOpSF:      CharString(' LoadOp');
           WcsLowGetsSF:  CharString(' WcsLow');
           WcsMidGetsSF:  CharString(' WcsMid');
           WcsHiGetsSF:   CharString(' WcsHi');
           IobFunctionSF: begin CharString(' Iob(');
                           Number(LXor(Z,#177));
                           Character(')')
                          end
           end
         end;
       MemCntl:
        if SF <= 7 then
         begin
          if Recast(SF,Perq1ASFtype) in [MulDivSF, PushLongSF] then
           case Recast(SF,Perq1ASFtype) of
            MulDivSF:     begin Comma; CharString(' MultiplyStep') end;
            PushLongSF:   begin Comma; CharString(' Push') end
            end
         end
        else
         begin Comma;
          case Recast(SF,MemFuncType) of
           Fetch:           CharString(' Fetch');
           Fetch2:          CharString(' Fetch2');
           Fetch4:          CharString(' Fetch4');
           Fetch4Reverse:   CharString(' Fetch4R');
           Store:           CharString(' Store');
           Store2:          CharString(' Store2');
           Store4:          CharString(' Store4');
           Store4Reverse:   CharString(' Store4R');
           otherwise:       CharString(' LatchMA')
           end
         end;
       LJSF:
       end;
      if Recast(F,ZUseType) = ShiftFunction then
       begin Comma;
        Hi := Shift( LXor(Z,#377), -4 );
        Lo := LAnd(  LXor(Z,#377), #17 );
        if Lo = #17 then
         begin CharString(' LeftShift(');
          Number(Hi)
         end
        else
         if Lo = #17 - Hi then
          begin CharString(' RightShift(');
           Number(Hi)
          end
         else
          if (Lo in [#15,#16]) and (Hi >= #10) then
           begin CharString(' Rotate(');
            if Lo = #15 then Number(Hi-#10)
            else Number(Hi)
           end
          else
           begin CharString(' Field(');
            Number(Hi);
            Character(',');
            Number(Lo+1)
           end;
        Character(')')
       end;
      if H then begin Comma; CharString(' Hold') end
     end
   end { SpecialFunction };
          
 
   procedure Assignment;
 
 
    procedure Alu;
    var NoAlu, PrintAmux, PrintBmux: Boolean;
  
  
     procedure Amux;
     begin { Amux }
      with CurMB do
       case A of
        ShiftSrc:   CharString(' Shift');
        NextOp:     CharString(' NextOp');
        IodSrc:     CharString(' Iod');
        MdiSrc:     CharString(' Mdi');
        MdxSrc:     CharString(' Mdx');
        UStateSrc:  begin CharString(' UState');
                     if B = YRam then
                      begin CharString('(R');
                       Number(Y);
                       Character(')')
                      end
                    end;
        XSrc:       begin CharString(' R');
                     Number(X)
                    end;
        EStkSrc:    CharString(' Tos')
        end
     end { Amux };
     
     
     procedure Bmux;
     begin { Bmux }
      with CurMB do
       if B = YRam then
        begin CharString(' R');
         Number(Y)
        end
       else
        begin Character(' ');
         if ((F in [0,2]) and (SF = 0)) or
            ((F = 1) and (Recast(SF,Perq1ASFtype) = PushLongSF)) then
          { long constant }
          Number(LOr( Shift(Z,8), Y ))
         else
          Number(Y)
        end
     end { Bmux };
           
        
    begin { Alu }
     with CurMB do
      begin
       NoAlu := (F = 1) and
                (Recast(SF,Perq1ASFtype) in [ReadVictimSF, ReadMQSF]);
       if NoAlu then
        case Recast(SF,Perq1ASFtype) of
         ReadVictimSF: CharString(' Victim');
         ReadMQSF:     CharString(' MQ')
         end;
       if not NoAlu or (A <> XSrc) or (Alu <> Aalu) or
          ((X <> 0) and not W) then
        begin
         if NoAlu then CharString(' {');
         if Alu = NotAalu then CharString(' not');
         PrintAmux := not (Alu in [Balu, NotBalu]) or (A <> XSrc) or
                      ((X <> 0) and not W);
         PrintBmux := not (Alu in [Aalu, NotAalu]) or (B <> YRam) or
                      ((Y <> 0) and
                       ((F <> 1) or (Recast(SF,Perq1ASFtype) <> LeapSF)));
         if PrintAmux then Amux;
         case Alu of
          Aalu,
          NotAalu:          if PrintBmux then CharString(' amux');
          Balu,
          NotBalu:          if PrintAmux then CharString(' bmux');
          AandBalu,
          AandNotBalu:      CharString(' and');
          AnandBalu:        CharString(' nand');
          AorBalu,
          AorNotBalu:       CharString(' or');
          AnorBalu:         CharString(' nor');
          AxorBalu:         CharString(' xor');
          AxnorBalu:        CharString(' xnor');
          AplusBalu,
          AplusBplusOCalu:  CharString(' +');
          AminusBalu,
          AminusBminusOCalu:CharString(' -')
          end;
         if Alu in [NotBalu, AandNotBalu, AorNotBalu] then CharString(' not');
         if PrintBmux then Bmux;
         if Alu = AplusBplusOCalu then CharString(' + OldCarry');
         if Alu = AminusBminusOCalu then CharString(' - OldCarry');
         if NoAlu then CharString(' }')
        end;
       CodePrinted := true
      end
    end { Alu };
   
   
   begin { Assignment }
    with CurMB do
     begin
      if W then
       begin CharString(' R');
        Number(X);
        CharString(' :=');
        CodePrinted := true
       end;
      if (F in [0,2]) and
         (Recast(SF,SFType) in [PushSF, TosGetsSF, BpcGetsSF, SrcRoGetsSF,
                                DstRoGetsSF, WidRoGetsSF]) then
       begin
        case Recast(SF,SFType) of
         PushSF,
         TosGetsSF:   CharString(' Tos :=');
         BpcGetsSF:   CharString(' Bpc :=');
         SrcRoGetsSF: CharString(' SrcRasterOp :=');
         DstRoGetsSF: CharString(' DstRasterOp :=');
         WidRoGetsSF: CharString(' WidRasterOp :=')
         end;
        CodePrinted := true
       end
      else
       if F = 1 then
        begin
         if Recast(SF,Perq1ASFtype) in [LoadMQSF, LoadBaseSF, PushLongSF] then
          begin
           case Recast(SF,Perq1ASFtype) of
            LoadMQSF:   CharString(' MQ :=');
            LoadBaseSF: CharString(' RBase :=');
            PushLongSF: CharString(' Tos :=')
            end;
           CodePrinted := True
          end
         else
          if Recast(SF,MemFuncType) in [Fetch, Fetch2, Fetch4,
                                        Fetch4Reverse, Store, Store2,
                                        Store4, Store4Reverse] then
           begin
            CharString(' MA :=');
            CodePrinted := True
           end
        end
     end;
    Alu
   end { Assignment };
       
             
            
  begin { Instruction }
   OutL := 0;
   if Address[CurMB.Adrs].Referenced then
    begin Character('L');
     Number(CurMB.Adrs);
     Character(':')
    end;
   for i := OutL+1 to 7 do Character(' ');
   CodePrinted := false;
   PrintBlankLine := false;
   Assignment;
   SpecialFunction;
   Test;
   if not CodePrinted then CharString('nop');
   Character(';');
   if OutL >= 40 then CharString('  ')
   else
    for i := OutL+1 to 40 do Character(' ');
   CharString('! ');
   Number(CurMB.Adrs);
   Writeln(OutFile, OutS:OutL);
   if PrintBlankLine then Writeln(OutFile)
  end { Instruction };
  
  
  Procedure OpenFiles;
  begin { OpenFiles }
  end { OpenFiles };
 
 
 begin { Pass2 }
  Reset(CodFile,CodFileName);
  Rewrite(OutFile,OutFileName);
  Preamble;
  while not Eof(CodFile) and (CodFile^.Adrs >= 0) do
   begin CurMB := CodFile^; Get(CodFile); Instruction end;
  Writeln(OutFile,'        end;');
  Close(OutFile)
 end { Pass2 };
 
 
begin { PrqDis }
  Writeln('PrqDis V', PrqDisVersion);
 Initialize;
 Pass1;
 Pass2
end { PrqDis }.
