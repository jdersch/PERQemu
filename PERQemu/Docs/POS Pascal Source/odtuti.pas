{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module OdtUtils;

{   copyright 1983  Three Rivers Computer Corporation
{
{------------------------------------------------------------------------
Change log:

   9 Dec 82  V0.2  C. Beckett  Fix for new comp: label problems.

   2 Dec 82  V0.1  C. Beckett  14 char compiler fix, add a change log.

{------------------------------------------------------------------------}

exports

 imports OdtPrq from OdtPrq;
 imports ControlStore from ControlStore;
{$ifc MPOS then}
 imports ProcessDefs from ProcessDefs;
{$endc}
 
 

 const ChrWidth = 9;
       NameLen = 8;
       MaxX = 3;
       MaxY = 20;
       slWinLen = MaxY*15;
       XWidth = 768 div (MaxX+1);
       CmdWidth = 54;
       NameWidth = NameLen*ChrWidth;
       SlotWindow = 1;
       CmdWindow = 2;
       slotlen = NameLen+1+10;
       TryRead = 10;
       

 type NameDsp = (NumberDsp,SymbolDsp);
      slotrec = record
                  slVal: Bit20;
                  slForm: ValForm;
                  slNameDsp: NameDsp;
                  slName: String [NameLen];
                  Case slKind: ValKind of
                   MKind, RKind: (slLoc: Bit20);
                   VKind: (slProcess,slSegment,slRoutine,slOffset: Bit20);
                 end;


 var slot: array [0..MaxX,0..MaxY] of slotrec;
     DisplayWindow: Integer;
     DisplayX, DisplayY: Integer;


 procedure Update( ForceAll: boolean );
 procedure InitSlots;


 procedure WriteWord( x:integer );
 function  ReadWord( TryMax: Integer ):integer;

 procedure Help;
 procedure Error( Message: String );
 procedure ReadString( Prompt: String );
 procedure ReadNumber( Prompt: String );
 procedure NumericForm( var Form: ValForm );

 procedure DefaultExtension(var FileName: String; Extension: String);
 function  Existant( FileName: String ): Boolean;

 procedure Translate(var UWord: MicroInstruction; var TLate: TransMicro);

 procedure ReadMicro(FileName: String; Overlay: Boolean);
 procedure LoadQSystem( QSystemName: String );

 procedure StackReset;
 
 procedure Push( V: Bit20 );
 procedure Pop( var V: Bit20 );
 procedure PushInt( V: Integer );
 procedure PopInt( var V: Integer );
 procedure Repl;
 
 procedure Add;
 procedure Sub;
 procedure TLate;
 
 function EQ: Boolean;
 function NE: Boolean;
 function LT: Boolean;
 function LE: Boolean;
 function GT: Boolean;
 function GE: Boolean;  {30}
 
 procedure ClearQSegments;
 procedure QSegment( Process: Integer );
 procedure QSegName( var F: Text; PrcNumber, SegNumber: Integer );
 procedure VarAddress;

 procedure UInt(var F: text; Width: Integer);
 procedure Int(var F: text; Width: Integer);
 procedure Oct(var F: text; Width: Integer);
 procedure SOct(var F: text; Width: Integer);
 procedure Number(var F: Text; Width, Base: Integer);
 procedure DisplayVal( Kind: ValKind; Form: ValForm ); {40}

 procedure WriteReg;
 procedure ReadReg;
 procedure ReadMem;
 procedure WriteMem;
 procedure WriteMicro(Val: MicroInstruction);

 procedure StartPerq(A:Integer);

 function  UnusedBkp: Integer;
 procedure SetUBkp(B: Integer; Addr: Integer);
 procedure KillUBkp(B: Integer);
 procedure ClearUBkps;  {50}
 procedure SetQBkp(B: Integer; Segment, Routine, Instruction: Integer);
 procedure KillQBkp(B: Integer);
 procedure ClearQBkps;
 
 procedure CreateUCode( var UA: pUArray );
 procedure DestroyUCode( var UA: pUArray );
 
 procedure QProcess;
 procedure QPrcName( var F: Text; PrcNumber: Integer ); 
 function CurrentProcess: Integer;
 procedure ReadState( Process: Integer;
                      var SS, SB, AP, GP, LP, CS, CB, RN, UPC, PC: Bit20 );
 function RelAddr( CB, PC: Bit20; RN: Integer ): Bit20;
 
 
 const MinStack = 0;
       MaxStack = 20;
{$ifc MPOS then}
       MaxQProc = PrcMaxProcesses;
{$endc}
       MaxQSeg = 100;
       QNameLength = 20;
 
 type QNameString = String[QNameLength];
 
 var Stack: array[MinStack..MaxStack] of Bit20;
     SP: Integer;
{$ifc MPOS then}
     QProc: array[1..MaxQProc] of record
              PName: QNameString;
              PStackSeg: Integer;
              GDB: array[0..MaxQSeg] of Integer
              end;
     QSeg: array[1..MaxQSeg] of QNameString;
{$elsec}
     QSeg: array[1..MaxQSeg] of record
            Name: QNameString;
            GDB: Integer
            end;
{$endc}
     SSize: Integer;
     SysNSNumber: Integer;  { SysNameSeg number }
     PrcSNumber: Integer;   { PrcSeg number }
     MsgSNumber: Integer;   { MsgSeg number }


private


 imports FileSystem from FileSystem;
 imports Perq_String from Perq_String;
 imports IOErrors from IOErrors;
 imports IO_Unit from IO_Unit;
 imports IO_Others from IO_Others;
 imports Screen from Screen;
 imports Raster from Raster;
 imports Memory from Memory;
{$ifc not MPOS then}
 imports Code from Code;
 imports RunRead from RunRead;
{$endc}

 
 {$R-}
 
 
 const leftmargin = 10;
       topmargin = 20;
       {$Include ACB.Dfs}
       {$Include RD.Dfs}

 var i,j,k,height,oldx,oldy,oldCmd: integer;
     OdtPrompt: String;
     ScreenP: RasterPtr;
     OnSlots, OnCmd: boolean;
 
 
 
 procedure WriteWord(x:integer);
 var Success: boolean;
 begin
  LoadExpr(X);
  LoadExpr(Lor(Shift(LinkSnd,8),Shift(LinkSnd,-8)));
  InLineByte( #277 {JCS} );
  StorExpr(Success);
  NoDoneBit := not Success;
  if NoDoneBit and PrintNoDone then Writeln('No done bit on transmit.')
 end { WriteWord };
 
 
 function ReadWord( TryMax: Integer ):integer;
 var Success: boolean;
     TryCount, X: integer;
 begin
  TryCount := TryMax;
  repeat TryCount := TryCount - 1;
   LoadExpr(LOr(Shift(LinkRcv,8),Shift(LinkRcv,-8)));
   InLineByte( #277 {JCS} );
   StorExpr(Success);
   if Success then begin StorExpr(X); ReadWord := X end
   else ReadWord := -1
  until (Success) or (TryCount <= 0);
  NoDoneBit := not Success;
  if NoDoneBit and PrintNoDone then Writeln('No done bit on receive.')
 end { ReadWord };


 procedure Help;
 begin { Help }
  writeln;
  writeln(' Help            print help listing.');
  writeln(' Quit            quit.');
  writeln(' Boot            boot Perq.');
  writeln;
  writeln(' Memory          display/change a memory word.');
  writeln(' Register        display/change a register.');
  writeln(' UCode           display/change a micro instruction.');
  writeln(' Variable        display/change a Pascal variable.');
  writeln(' Global          display/change a Pascal global variable.');
  writeln(' Virtual         display/change a virtual memory variable.');
  writeln(' Watch           display a value in the top window.');
  writeln(' Clear           clear memory.');
  writeln;
  writeln(' Load            initial load of a micro binary.');
  writeln(' Overlay         load a micro binary over previously loaded code.');
  writeln(' QLoad           load Q-code (Pascal) program.');
  writeln(' ListSegments    list Q-code segments currently loaded.');
{$ifc MPOS then}
  writeln(' ListProcesses   list Q-code processes currently loaded.');
{$endc}
  writeln;
  writeln(' Go              start Perq executing.');
  writeln(' Break           set microcode breakpoint.');
  writeln(' KillBreak       clear microcode breakpoint.');
  writeln(' QBreak          set Q-code breakpoint.');
  writeln(' QKillBreak      clear Q-code breakpoint.');
  writeln(' ListBreaks      list active breakpoints.');
  writeln(' Proceed         proceed from recent breakpoint.');
  writeln;
  writeln(' Dump            enter Dump subsystem.');
  writeln;
  writeln(' CommandFile     read alternate command file.');
  writeln(' ListFile        write alternate list file.');
  writeln;
  writeln(' SaveState       save OdtPrq state on a file.');
  writeln(' GetState        get OdtPrq state from a file.');
  writeln;
  writeln(' Debug           print all sorts of interesting stuff.');
  writeln;
  writeln(' /               display current location.');
  writeln(' (LF)            display current location.');
  writeln(' ^               display previous location.');
  writeln(' =               open current location for changes.');
  writeln
 end { Help };


 procedure Error( Message: String );
 begin { Error }
  if Message <> '' then Writeln('  ', Message);
  CmdLine := '';
  Open := false;
  ChangeWindow(CmdWindow);
  DisplayWindow := CmdWindow;
  Exit(OdtCommand)
 end { Error };
 
 
 procedure CurrentProcess;
 var SS: Integer;
     Prc: Integer;
 begin { CurrentProcess }
{$ifc MPOS then}
   PushInt(#12); ReadReg; PopInt(SS);
   for Prc := 1 to MaxQProc do
     if QProc[Prc].PStackSeg = SS then
       begin
         CurrentProcess := Prc;
         Exit(CurrentProcess)
       end;
{$endc}
   CurrentProcess := 0
 end { CurrentProcess };
  
  
 procedure ReadString( Prompt: String );
 var Word: Integer;
     Number: Boolean;
 
 
  procedure BreakPoint;
  
  
    procedure UBreak;
    var Address: Integer;
    begin { UBreak }
      if (LastBkp = PBkp1) or (LastBkp = PBkp2) then
          {coming back from a proceed}
          begin Address := Bkp[LastBkp].UAddr;
          KillUBkp(PBkp1);
          KillUBkp(PBkp2);
          SetUBkp(ProceedBkp,ProceedAddr);
          StartPerq(Address);
          ProceedAddr := 0;
          ProceedBkp := 0;
          LastBkp := 0
          end
      else
          if (LastBkp <= NBkp) and (LastBkp > 0) then
                  begin { UCode Breakpoint }
                  ProceedBkp := LastBkp;
                  ProceedAddr := Bkp[LastBkp].UAddr;
                  Write(OutFile, '  Breakpoint ');
                  PushInt(LastBkp);
                  Oct(OutFile,1);
                  Write(OutFile, ' at address ');
                  PushInt(ProceedAddr);
                  Oct(OutFile,1)
                  end
          else
              begin { special breakpoint }
              Write(OutFile, '  Breakpoint ');
              PushInt(LastBkp);
              Oct(OutFile,1);
              if LastBkp = #377 then
                  write(OutFile, ' from unknown location')
              else
                  case LastBkp of
                      20: write(OutFile, ' Seg Fault');
                      21: write(OutFile, ' Stk Ovl');
                      22: write(OutFile, ' Run Err');
                      23: write(OutFile, ' IO Seg Fault');
                      24: write(OutFile, ' Memory parity error');
                      30: write(OutFile, ' Krnl detected a bad command');
                      31: write(OutFile, ' Krnl detected a bad interrupt return');
                      32: write(OutFile, ' Krnl detected a bad interrupt')
                    end;
               LastBkp := 0;
               ProceedAddr := 0
               end
    end { UBreak };
    
    
    procedure QBreak;
    var B: Integer;
        Base, PC: Bit20;
        Process, Segment, Routine, Instruction, T: Integer;
        Found: Boolean;
    begin { QBreak }
     Process := CurrentProcess;
     PushInt(#11); ReadReg; PopInt(Segment);
     PushInt(#10); ReadReg; PopInt(Routine);
     PushInt(Segment); PushInt(0); TLate; Pop(Base);
     PushInt(#16); ReadReg; Push(Base); Sub;
         Repl; Add; Pop(PC);                    { (UPC - CB) * 2 }
     PushInt(#364); ReadReg; PopInt(T); T := LAnd(T,#17) - 1;
         PushInt(T); Push(PC); Add; Pop(PC);    { (UPC - CB) * 2 + BPC }
     PC := RelAddr(Base,PC,Routine);            { relative address }
     Push(PC); PopInt(Instruction);
     B := 0;
     repeat B := B + 1;
      with Bkp[B] do
       Found := (Kind = QBkp) and
                (Seg = Segment) and (Rtn = Routine) and (Instr = Instruction)
     until Found or (B = NBkp);
     Write(OutFile, '  QBreakpoint');
     if Found then
      begin LastBkp := B;
       Write(OutFile, ' ');
       PushInt(B);
       Oct(OutFile,1);
       ProceedAddr := Shift( LXor(Bkp[B].QCode,#377), 2)
      end
     else
      begin LastBkp := 0;
       ProceedAddr := 0
      end;
     Write(OutFile, ' from');
{$ifc MPOS then}
     Write(OutFile, ' process ');
     QPrcName(OutFile, Process);
     Write(OutFile, ',');
{$endc}
     Write(OutFile, ' segment ');
     QSegName(OutFile, Process, Segment);
     Write(OutFile, ', routine ');
     PushInt(Routine);
     Int(OutFile,1);
     Push(PC); Push(Nill);
     if {PC} EQ {Nill} then Write(OutFile, ', unknown address')
     else
      begin
       Write(OutFile, ', instruction ');
       PushInt(Instruction);
       Int(OutFile,1)
      end
    end { QBreak };


  begin { BreakPoint }
    if Word = Hey then
      begin
      PushInt(#370); ReadReg; Repl; PopInt(LastBkp);  {get breakpoint number}
      PushInt(#2000);
      if (LastBkp >= 0) and {LastBkp} LT {#2000} then UBreak else QBreak
      end
    else
      begin
        if Word = Hello then Write(OutFile, 'Krnl entered at 7400')
        else
         begin
          Write(OutFile, 'unexpected message from the Krnl: ');
          PushInt(Word);
          Oct(OutFile,1)
         end;
        ProceedAddr := 0
      end;
    Writeln;
    Update(False)
  end { BreakPoint };


  procedure GetLine;
  var Ch: char;
      Status: integer;
   
   
   procedure Erase;
   var Ch: Char;
   begin { Erase }
    Ch := CmdLine[Length(CmdLine)];
    Delete(CmdLine,Length(CmdLine),1);
    if Ch < ' ' then { control character }
     begin
      if not (Ch in BreakSet) then
       begin SClearChar(Chr(Ord(Ch) + Ord('@')), RXor);
        SClearChar('^', RXor)
       end
     end
    else SClearChar(Ch, RXor)
   end { Erase };
   
   
   procedure CheckTablet;
   var SeenNum: Boolean;
       xslot, yslot, CmdPos: Integer;


    procedure XorSlot(x,y: integer);
    begin { XorSlot }
     RasterOp(RNot,xwidth-8,height,
              x*xWidth+leftmargin-4,y*Height+topmargin,48,ScreenP,
              x*xWidth+leftmargin-4,y*Height+topmargin,48,ScreenP);
    end { XorSlot };
   
    procedure XorCmd(x: integer);
    begin { XorCmd }
     RasterOp(RNot,CmdWidth-8,height,
              x*CmdWidth+8-4,slWinLen+3,48,ScreenP,
              x*CmdWidth+8-4,slWinLen+3,48,ScreenP);
    end { XorCmd };
   

    procedure TrackCursor;
    var xt, yt: Integer;
    begin { TrackCursor }
     IOReadTablet(xt,yt);
     if (xt > LeftMargin) and (xt < (MaxX + 1) * xWidth + LeftMargin) and
        (yt > TopMargin) and
        (yt < (MaxY + 1) * Height + TopMargin) then {in the window}
      begin
       if OnCmd then
        begin
         XorCmd(oldCmd);
         OnCmd := false;
         OldCmd := -1
        end;
       xslot := (xt - leftmargin) div xWidth;
       yslot := (yt - topmargin)  div Height;
       if (xt > ((oldX  )*xWidth-5+leftmargin)) and
          (xt < ((oldX+1)*xWidth+5+leftmargin))
              then xslot := oldx;
       if (yt > ((oldY  )*Height-2+topmargin)) and
          (yt < ((oldY+1)*Height+2+topmargin))
              then yslot := oldy;
       if (oldX <> xslot) or (oldy <> yslot) then
        begin
         if OnSlots then XorSlot(oldX,oldY);
         OnSlots := true;
         XorSlot(xslot,yslot);
        end;
       oldX := xslot;
       oldY := yslot;
      end
     else
      if (xt > LeftMargin) and
         (xt < Leftmargin + Length(OdtPrompt) * ChrWidth) and
         (yt > slWinLen+3) and (yt < slWinLen + Height + 3) then
       begin
        if OnSlots then
         begin
          XorSlot(oldX,oldY);
          OnSlots := false;
          oldX := -1; oldY := -1
         end;
        CmdPos := (xt - 8) div CmdWidth;
        if (xt > ((oldCmd  )*CmdWidth-5+8)) and
           (xt < ((oldCmd+1)*CmdWidth+5+8))
              then CmdPos := oldCmd;
        if (oldCmd <> CmdPos) then
         begin
          if OnCmd then XorCmd(oldCmd);
          OnCmd := true;
          XorCmd(CmdPos);
         end;
        oldCmd := CmdPos
       end
      else
       if OnSlots then
         begin
          XorSlot(oldX,oldY);
          OnSlots := false;
          oldX := -1; oldY := -1
         end
       else
        if OnCmd then
         begin
          XorCmd(oldCmd);
          OnCmd := false;
          OldCmd := -1
         end
    end { TrackCursor };
    
    
    procedure Hit;
    var S: String;
        Process: Integer;

   
    procedure AppendOct;
    var v: Bit20;
        Upper, Lower: Integer;
    begin { AppendOct }
     Pop(v);
     Upper := LAnd(V.Upper,#1777);
     Lower := LAnd(V.Lower,#1777);
     AppendChar(S,' ');
     for i := 0 to 6 do
      begin
       AppendChar(S,chr(upper div #400 + ord('0')));
       Upper := Land(Upper * #10 + Lower div #200,#3777);
       lower := Land(Lower * #10,#1777);
      end
    end { AppendOct };


    begin { Hit }
     S := '';
     while tabswitch do ;
     if OnSlots then
      with slot[xslot,yslot] do
       begin
        XorSlot(oldX,oldY);
        OnSlots := False;
{*****}
        if slKind = NoKind then
         begin
          if CmdLine = '' then S := 'Watch';
          PushInt(xslot); AppendOct;
          PushInt(yslot); AppendOct;
          CmdLine := Concat(CmdLine,S)
         end
        else
         begin
          Push(slProcess);
          PopInt(Process);
          case slKind of
           RKind: S := ' Reg';
           MKind: S := ' Mem';
           VKind: if Process < 0 then S := ' Vir'
                  else S := ' Var';
           end;
          if slKind <> VKind then
           begin Push(slLoc); AppendOct end
          else
           if Process < 0 then { virtual address }
            begin
             Push(slSegment); AppendOct;
             Push(slOffset); AppendOct
            end
           else
            begin
{$ifc MPOS then}
             Push(slProcess); AppendOct;
{$endc}
             Push(slSegment); AppendOct;
             Push(slRoutine); AppendOct;
             Push(slOffset); AppendOct
            end;
          if SeenNum then AppendChar(S,'=')
          else AppendChar(S,'/');
          CmdLine := Concat(S,CmdLine)
         end
{*****
        if SeenNum then
         begin
          Case slKind of
           NoKind: S := '';
           RKind: S := ' Reg';
           MKind: S := ' Mem';
           VKind: S := ' Var';
          end;
          if slKind <> VKind then
           begin Push(slLoc); AppendOct end
          else
           begin
            Push(slProcess); AppendOct;
            Push(slSegment); AppendOct;
            Push(slRoutine); AppendOct;
            Push(slOffset); AppendOct
           end;
          AppendChar(S,'=');
          CmdLine := Concat(S,CmdLine)
         end;
        if (S = '') or not SeenNum then
         begin
          if CmdLine = '' then S := 'Watch';
          PushInt(xslot); AppendOct;
          PushInt(yslot); AppendOct;
          CmdLine := Concat(CmdLine,S)
         end
*****}
       end
     else
      if OnCmd then
       begin
        XorCmd(CmdPos);
        OnCmd := False;
        OldCmd := -1;
        S := SubStr(OdtPrompt,CmdPos*6+1,3);
        CmdLine := Concat(CmdLine,S)
       end
      else
       if TopLevel and (Length(CmdLine) = 0) then CmdLine := Commands[Cmd];
    end { Hit };


   begin { CheckTablet }
    TrackCursor;
    if TabSwitch then
     begin
      SeenNum := False;
      if Length(CmdLine) > 0 then
       SeenNum := CmdLine[1] in ['+', '-', '0'..'9'];
      if TabSwitch then Hit;
      Writeln('    "', CmdLine, '"');
      Status := IOEIOC;
      Ch := CR
     end
   end { CheckTablet };
   
   
  begin { GetLine }
   if Prompt <> '' then
    if Prompt[Length(Prompt)] = '>'  then Write(Prompt)
    else Write('  ', Prompt, ':');
   CmdLine := ' ';
   CmdLine := '';
   SChrFunc(RRpl);
   repeat
    Status := IOCRead(TransKey,Ch);
    CheckTablet;
    if Status <> IOEIOC then
     begin
      SCurOn;
      while Status <> IOEIOC do
       begin PrintNoDone := False;
        Word := ReadWord(1);
        PrintNoDone := True;
        if not NoDoneBit then
         begin BreakPoint;
          CmdLine := '';
          if Prompt = '>' then Write('>')
          else
           if Prompt <> '' then Write('  ', Prompt, ':')
         end;
        Status := IOCRead(TransKey,Ch);
        CheckTablet
       end;
      SCurOff
     end;
    if Ch in [BS, CtrlU, CtrlS, CtrlQ, CtrlD] then
     begin
      if Ch = BS then
       begin
        if Length(CmdLine) > 0 then Erase
       end
      else
       if Ch = CtrlU then
        while Length(CmdLine) > 0 do Erase;
      Ch := Del
     end
    else
     if Length(CmdLine) = 80 then Write(Bel)
     else
      begin
       if Ch = CR then AppendChar(CmdLine,' ') else AppendChar(CmdLine,Ch);
       if Ch < ' ' then
        if Ch = CR then Writeln
        else
         begin
          if not (Ch in BreakSet) then Write('^', Chr(Ord(Ch) + Ord('@')))
         end
       else Write(Ch)
      end
   until Ch in BreakSet
  end { GetLine };


 begin { ReadString }
  PrintNoDone := False;
  Word := ReadWord(1);
  PrintNoDone := True;
  if not NoDoneBit then BreakPoint;
  RemDelimiters(CmdLine,' ',Ignore);
  if CmdLine = '' then
   if InInFile then
    if Eof(InFile) then
     begin InInFile := False; GetLine end
    else
     begin Readln(InFile,CmdLine); Writeln(OutFile, '>', CmdLine) end
   else GetLine;
  Number := False;
  if CmdLine <> '' then Number := CmdLine[1] in ['-', '0'..'9'];
  if Number or not TopLevel then
   GetSymbol(CmdLine,CmdString,BreakString,Ignore)
  else GetSymbol(CmdLine,CmdString,BreakDigits,Ignore);
  if CmdLine = '' then BreakCh := ' '
  else
   if CmdLine[1] in ['-', '0'..'9'] then BreakCh := ' '
   else
    begin BreakCh := CmdLine[1]; Delete(CmdLine,1,1) end;
  RemDelimiters(CmdLine,' ',Ignore)
 end { ReadString };
  

 procedure ReadNumber( Prompt: String );
 var I: Integer;
     Ch: Char;
     TmpInInFile: Boolean;
     TmpLine: String;
     TmpBreakCh: Char;
     Error: Boolean;
     OldValWritten: Boolean;
     Default, Val: Bit20;
     Base: Integer;
     Chars: set of Char;
     Len: Integer;
     Negative: Boolean;
 begin { ReadNumber }
  Pop(Default);
  Val := Default;
  RemDelimiters(CmdLine,' ',Ignore);
  OldValWritten := CmdLine = '';
  if OldValWritten then
   begin Write('  ', Prompt, '[');
    Push(Val); Oct(OutFile, 1);
    Write(']:')
   end;
  ReadString('');
  TmpBreakCh := BreakCh;
  TmpInInFile := InInFile;
  TmpLine := CmdLine;
  repeat Error := False;
   if CmdString = '' then
    begin Negative := False;
     if not OldValWritten then
      begin Write('  [', Prompt, '=');
       Push(Val); Oct(OutFile,1);
       Writeln(']')
      end
    end
   else
    begin
     if CmdString[Length(CmdString)] = '.' then
      begin Base := 10;
       Chars := ['0'..'9'];
       Len := Length(CmdString) - 1
      end
     else
      begin Base := 8;
       Chars := ['0'..'7'];
       Len := Length(CmdString)
      end;
     Val.Lower := 0;
     Val.Upper := 0;
     I := 0;
     if CmdString[1] in ['+', '-'] then
      begin Negative := CmdString[1] = '-'; I := I + 1 end
     else Negative := False;
     while (I < Len) and not Error do
      begin
       I := I + 1;
       Ch := CmdString[I];
       if Ch in Chars then
        begin Val.Lower := Val.Lower * Base + Ord(Ch) - Ord('0');
         Val.Upper := LAnd(Val.Upper * Base + Shift(Val.Lower,-10),#1777);
         Val.Lower := LAnd(Val.Lower,#1777)
        end
       else
        begin
         Writeln;
         Write('Bad character in number: ',
               CmdString, ', re-enter ', Prompt, '[');
         Push(Default); Oct(OutFile, 1); Write(']:');
         Error := True;
         InInFile := False;
         CmdLine := '';
         Val := Default;
         ReadString('')
        end
      end
    end
  until not Error;
  InInFile := TmpInInFile;
  CmdLine := TmpLine;
  BreakCh := TmpBreakCh;
  if Negative then
   begin PushInt(0); Push(Val); Sub end
  else Push(Val)
 end { ReadNumber };
   
   
 procedure NumericForm( var Form: ValForm );
 var I: Integer;
     SeenSigned: Boolean;
 
 
  procedure NumericHelp;
  begin { NumericHelp }
   Writeln;
   Writeln('Numeric form is described by a string of characters:');
   Writeln;
   Writeln(' O - octal       D - decimal     C - char.');
   Writeln(' W - word        B - byte');
   Writeln(' U - unsigned    S - signed');
   Writeln;
   Writeln('Defaults: O');
   Writeln('          W');
   Writeln('          U for octal and S for decimal.');
   Writeln;
   Writeln('Examples: O   - unsigned octal word.');
   Writeln('          UDB - unsigned decimal byte.');
   Writeln('          C   - character.');
   Writeln('          SO  - signed octal.');
   Writeln
  end { NumericHelp };
 
 
 begin { NumericForm }
  with Form do
   begin
    repeat ReadString('Numeric form (type ? for help)');
     if CmdString = '?' then NumericHelp
    until CmdString <> '?';
    ConvUpper(CmdString);
    I := 1;
    Sign := UnSigned;
    Base := Octal;
    Size := Wrd;
    SeenSigned := False;
    while I <= Length(CmdString) do
     begin
      case CmdString[I] of
       'U': begin Sign := UnSigned; SeenSigned := True end;
       'S': begin Sign := Signed; SeenSigned := True end;
       'O': Base := Octal;
       'D': Base := Decimal;
       'C': Base := Character;
       'W': Size := Wrd;
       'B': Size := Byt;
       otherwise: Error('Bad character in numeric form.')
       end;
      I := I + 1
     end;
    if not SeenSigned then
     if Base = Decimal then Sign := Signed
     else Sign := UnSigned
   end
 end { NumericForm };
  
  
 procedure DefaultExtension(var FileName: String; Extension: String);
 var P: Integer;
 begin { DefaultExtension }
  ConvUpper(FileName); ConvUpper(Extension);
  P := Pos(FileName,Extension);
  if (P = 0) or (P <> Length(FileName) - Length(Extension) + 1) then
   FileName := Concat(FileName,Extension)
 end { DefaultExtension };
 
 
 function Existant( FileName: String ): Boolean;
 var Ignore: Integer;
 begin { Existant }
  Existant := FSLookUp(FileName,Ignore,Ignore) <> 0
 end { Existant };
 
 
 procedure Translate(var UWord: MicroInstruction; var TLate: TransMicro);
 { Translate UWord into TLate }
 begin
 with TLate do
     begin
     ALU23 := UWord.ALU23;
     ALU0 := UWord.ALU0;
     W := UWord.W;
     ALU1 := UWord.ALU1;
     A := UWord.A;
     Z := UWord.Z;
     SFF := UWord.SFF;
     H := UWord.H;
     B := UWord.B;
     JmpCnd := UWord.JmpCnd;
     Word3 := UWord.Word3
     end
 end { Translate };
 
 
 procedure unTranslate(var UWord: MicroInstruction; var TLate: TransMicro);
 { unTranslate TLate into UWord }
 begin
 with UWord do
     begin
     ALU23 := TLate.ALU23;
     ALU0 := TLate.ALU0;
     W := TLate.W;
     ALU1 := TLate.ALU1;
     A := TLate.A;
     Z := TLate.Z;
     SFF := TLate.SFF;
     H := TLate.H;
     B := TLate.B;
     JmpCnd := TLate.JmpCnd;
     Word3 := TLate.Word3
     end
 end { Translate };


 procedure ReadMicro(FileName: String; Overlay: Boolean);
 { loads user microcode files }
   var f: MicroFile; I, J, UAddr: Integer;
 begin { ReadMicro }
 if not Existant(FileName) then
  begin Writeln('  File not found: ', FileName);
   Exit(ReadMicro)
  end;
 reset(f,FileName);
 if not OverLay then
   for I := 0 to MaxUAddr div UArraySize do
     if UCode[I] <> nil then
       if I = EndKrnl div UArraySize then
         for J := 0 to UArraySize - 1 do
           begin
             UAddr := I * UArraySize + J;
             if (UAddr < BeginKrnl) or (UAddr > EndKrnl) then
               UCode[I]^[J] := EmptyInstruction
           end
       else DestroyUCode(UCode[I]);
 while f^.adrs >= 0 do
   begin
     UAddr := f^.adrs;
     I := UAddr div UArraySize;
     J := UAddr mod UArraySize;
     CreateUCode(UCode[I]);
     if OverLay then
       with UCode[I]^[J] do
         if (Word1 <> -1) or (Word2 <> -1) or (Word3 <> -1) then
           begin
             write(OutFile, '  Location ');
             PushInt(UAddr);
             Oct(OutFile,1);
             writeln(OutFile, ' already in use, new instruction overrides');
           end;
     UCode[I]^[J]:=f^.MI;
     Get(f)
   end;
 close(f);
 writeln('  ',FileName,' Read')
 end { ReadMicro };


 procedure LoadQSystem( QSystemName: String );
 var BootFileName, MBootFileName: String;
{$ifc MPOS then}
     InfoFileName: String;
{$elsec}
     RunFileName: String;
{$endc}
     BootLetter: Char;
     BootDisk: Integer;


{$ifc MPOS then}
  procedure ReadInfo;
  var Info: Text;
      QPNum, QSNum, QGDB: Integer;
      QPName, QSName: QNameString;
  
  
    procedure SkipBlanks;
    begin { SkipBlanks }
      while (Info^ = ' ') and not Eoln(Info) do Get(Info)
    end { SkipBlanks };
    
    
    procedure ReadName( var Name: QNameString );
    var N: String;
    begin { ReadName }
      {$R-}
      N[0] := Chr(0);
      {$R=}
      SkipBlanks;
      while Info^ <> ' ' do
        begin
          if Length(N) < 80 then
            begin
              {$R-}
              N[0] := Succ(N[0]);
              {$R=}
              N[Length(N)] := Info^
            end;
          Get(Info)
        end;
      Delete(N,1,RevPosC(N,'>'));
      if Length(N) > QNameLength then Name := SubStr(N,1,QNameLength)
      else Name := N
    end { ReadName };
    
          
  begin { ReadInfo }
    Reset(Info,InfoFileName);
    SysNSNumber := 0;
    PrcSNumber := 0;
    MsgSNumber := 0;
    while Info^ = 'X' do
      begin
        Get(Info);
        case Info^ of
          'N': begin Get(Info); Read(Info, SysNSNumber) end;
          'P': begin Get(Info); Read(Info, PrcSNumber) end;
          'M': begin Get(Info); Read(Info, MsgSNumber) end;
          otherwise:
          end;
        Readln(Info)
      end;
    if (SysNSNumber <= 0) or (PrcSNumber <= 0) or (MsgSNumber <= 0) then
      Error(Concat(InfoFileName, ' is ill-formed.'));
    while not Eof(Info) do
      begin
        if Info^ <> 'P' then Error(Concat(InfoFileName, ' is ill-formed.'));
        Get(Info);
        Read(Info, QPNum);
        ReadName(QPName);
        Readln(Info);
        Write(OutFile, QPName, ' is process ');
        PushInt(QPNum);
        Int(OutFile,1);
        Writeln;
        if (QPNum < 1) or (QPNum > MaxQProc) then
          Error('Process number out of range.');
        with QProc[QPNum] do
          begin
            PName := QPName;
            while Info^ = 'C' do
              begin
                Get(Info);
                Read(Info, QSNum);
                ReadName(QSName);
                SkipBlanks;
                if Info^ = 'G' then
                  begin
                    Get(Info);
                    Read(Info, QGDB)
                  end
                else Error(Concat(InfoFileName, ' is ill-formed.'));
                if (QSNum < 1) or (QSNum > MaxQSeg) then
                  Error('Segment number out of range.');
                if QSeg[QSNum] = '' then QSeg[QSNum] := QSName;
                GDB[QSNum] := QGDB;
                Readln(Info)
              end;
            if Info^ = 'S' then
              begin
                Get(Info);
                Readln(Info,QSNum)
              end
            else Error(Concat(InfoFileName, ' is ill-formed.'));
            if (QSNum < 1) or (QSNum > MaxQSeg) then
              Error('Segment number out of range.');
            PStackSeg := QSNum
          end
      end
  end { ReadInfo };


{$elsec}
  procedure ReadRun;
  var RunFile: RunFileType;
      FirstSeg, FirstUserSeg, LastSeg, S: pSegNode;
      K: integer;
      System: Boolean;
      RunSeg: SegmentNumber;
      CodeSeg: integer;
      I, J: Integer;
      RunHead: RunInfo;
  begin { ReadRun }
    CreateSegment(RunSeg,2,2,100);
    Reset(RunFile,RunFileName);
    ReadRunFile(RunFile,RunSeg,RunHead,FirstSeg,FirstUserSeg,LastSeg,False);
    if FirstSeg = nil then
      begin
        Close(RunFile);
        DecRefCount(RunSeg);
        if RunHead.RFileFormat <> RFileFormat then
          Error(Concat(RunFileName, ' has an incompatible run file format.'))
        else
          Error(Concat(RunFileName, ' is ill-formed.'));
        Exit(ReadRun)
      end;
    Close(RunFile);
    if not RunHead.System in [false,true] then
    else
      begin RunHead.System := true; RunHead.Version := -1 end;
    if not RunHead.System then
      begin
        Close(RunFile);
        DecRefCount(RunSeg);
        Error(Concat(RunFileName, ' was not linked as a System.'));
        Exit(ReadRun)
      end;
    PushInt(#405); ReadMem; PopInt(CodeSeg);
    S := FirstSeg;
    while S <> nil do
      begin
        if (CodeSeg < 1) or (CodeSeg > MaxQSeg) then CodeSeg := 1;
        with QSeg[CodeSeg] do
          begin
            I := SegLength;
            while S^.SegId[I] = ' ' do I := I - 1;
            Name := '';
            for J := 1 to I do AppendChar(Name,S^.SegId[J]);
            GDB := S^.GDBOff;
            Write(OutFile, '  ', Name, ' ':QNameLength-Length(Name)+1,
                           'is segment ');
            PushInt(CodeSeg);
            Int(OutFile,2);
            Write(OutFile, ', GDB offset = ');
            PushInt(GDB);
            Oct(OutFile,1);
            Writeln(OutFile);
            CodeSeg := CodeSeg + 1
          end;
        S := S^.Next
      end;
    Close(RunFile);
    DecRefCount(RunSeg)
  end { ReadRun };
{$endc}
 
  
  procedure LoadBootFile;
  var Boot: file of array[0..255] of Integer;
      Block, Word: Integer;
      ScreenSize: Integer;
  begin { LoadBootFile }
    Writeln('  Loading ', BootFileName);
    Block := 0;
    Reset(Boot,BootFileName);
    while not Eof(Boot) do
      begin
        WriteWord(LmA);
        WriteWord(Shift(Block,8));
        WriteWord(Shift(Block,-8));
        WriteWord(WmB);
        for Word := 0 to 255 do WriteWord(Boot^[Word]);
        Get(Boot);
        Block := Block + 1;
        if Block = 6 then { Skip screen }
          begin
            PushInt(ScreenSeg + ScreenSeg + 1);
            ReadMem;
            PopInt(ScreenSize);
            ScreenSize := Shift(ScreenSize,-4) + 1;
            Block := Block + ScreenSize
          end
      end;
    Close(Boot);
    Writeln('  ', BootFileName, ' loaded.')
  end { LoadBootFile };
  
  
  procedure LoadMBootFile;
  var MBoot: file of TransMicro;
      I, J, UAddr: Integer;
      TMicro: TransMicro;
  begin { LoadBootFile }
    Writeln('  Loading ', MBootFileName);
    for I := 0 to MaxUAddr div UArraySize do
      if UCode[I] <> nil then
        if I = EndKrnl div UArraySize then
          for J := 0 to UArraySize - 1 do
            begin
              UAddr := I * UArraySize + J;
              if (UAddr < BeginKrnl) or (UAddr > EndKrnl) then
                UCode[I]^[J] := EmptyInstruction
            end
        else DestroyUCode(UCode[I]);
    UAddr := 0;
    Reset(MBoot,MBootFileName);
    while not Eof(MBoot) do
      begin
      I := UAddr div UArraySize;
      J := UAddr mod UArraySize;
      CreateUCode(UCode[I]);
      with UCode[I]^[J] do
        begin
          MBoot^.Word1 := LNot(MBoot^.Word1);
          MBoot^.Word2 := LNot(MBoot^.Word2);
          MBoot^.Word3 := LNot(MBoot^.Word3);
          unTranslate(UCode[I]^[J],MBoot^);
          Get(MBoot);
          UAddr := UAddr + 1
        end
      end;
    Close(MBoot);
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
          end;
    Writeln('  ', MBootFileName, ' loaded.')
  end { LoadBootFile };

 
 begin { LoadQSystem }
  ReadString('Boot letter');
  if CmdString = '' then BootLetter := 'a'
  else BootLetter := CmdString[1];
  if BootLetter in ['A'..'Z'] then { floppy } BootDisk := 1
  else BootDisk := 0;
{$ifc MPOS then}
  AppendChar(QSystemName, '.');
  AppendChar(QSystemName, BootLetter);
  InfoFileName := QSystemName;
  AppendString(InfoFileName, '.Info');
  ReadString(Concat(Concat('Name of info file [', InfoFileName), ']'));
  if CmdString <> '' then InfoFileName := CmdString;
  if not Existant(InfoFileName) then
    Error(Concat('File not found: ', InfoFileName));
{$elsec}
  RunFileName := QSystemName;
  AppendString(RunFileName,'.Run');
  ReadString(Concat(Concat('Name of run file [', RunFileName), ']'));
  if CmdString <> '' then RunFileName := CmdString;
  if not Existant(RunFileName) then
    Error(Concat('File not found: ', RunFileName));
  AppendChar(QSystemName, '.');
  AppendChar(QSystemName, BootLetter);
{$endc}
  BootFileName := QSystemName;
  AppendString(BootFileName, '.Boot');
  ReadString(Concat(Concat('Name of Q-code boot file [', BootFileName), ']'));
  if CmdString <> '' then BootFileName := CmdString;
  if not Existant(BootFileName) then
    Error(Concat('File not found: ', BootFileName));
  MBootFileName := QSystemName;
  AppendString(MBootFileName, '.MBoot');
  ReadString(Concat(Concat('Name of Q-code boot file [', MBootFileName), ']'));
  if CmdString <> '' then MBootFileName := CmdString;
  if not Existant(MBootFileName) then
    Error(Concat('File not found: ', MBootFileName));
  ClearQSegments;
  Writeln;
  LoadBootFile;
  Writeln;
  LoadMBootFile;
  Writeln;
{$ifc MPOS then}
  ReadInfo;
{$elsec}
  ReadRun;
{$endc}
  Writeln;
  PushInt(BootDisk);
  PushInt(SITSeg); PushInt(6); TLate;
  WriteMem;
  PushInt(Ord(BootLetter));
  PushInt(SITSeg); PushInt(7); TLate;
  WriteMem
 end { LoadQSystem };
 
 
 procedure PrintStack( Where: String );
 var I: Integer;
 begin { PrintStack }
  if DEBUG then
   begin DEBUG := False;
    Write(OutFile, Where:15, ':');
    for I := 0 to SP-1 do
     begin Push(Stack[I]); Oct(OutFile,9) end;
    Writeln(OutFile);
    DEBUG := True
   end
 end { PrintStack };
 
 
 procedure StackReset;
 begin { StackReset }
  SP := MinStack;
  ;PrintStack('StackReset')
 end { StackReset };
 
 
 procedure Push( V: Bit20 );
 begin { Push }
  if SP > MaxStack then Error('Stack overflow');
  Stack[SP] := V;
  SP := SP + 1
  ;PrintStack('Push')
 end { Push };
 
 
 procedure Pop( var V: Bit20 );
 begin { Pop }
  if SP = MinStack then Error('Stack underflow');
  SP := SP - 1;
  V := Stack[SP]
  ;PrintStack('Pop')
 end { Pop };

 
 procedure PushInt( V: Integer );
 var InVal: packed record case integer of
              1: (Lower: 0..#1777;
                  Upper: 0..#77);
              2: (i: integer )
              end;
     SVal: Bit20;
 begin { PushInt }
  InVal.i := V;
  SVal.Upper := InVal.Upper;
  SVal.Lower := InVal.Lower;
  Push(SVal)
 end { PushInt };
 
 
 procedure PopInt( var V: Integer );
 var OutVal: packed record case integer of
               1: (Lower: 0..#1777;
                   Upper: 0..#77);
               2: (i: integer )
               end;
     SVal: Bit20;
 begin { PopInt }
  Pop(SVal);
  OutVal.Upper := LAnd(SVal.Upper,#77);
  OutVal.Lower := SVal.Lower;
  V := OutVal.i
 end { PopInt };
 
 
 procedure Repl;
 begin { Repl }
  if SP = MinStack then Error('Stack underflow');
  Push(Stack[SP-1])
  ;PrintStack('Repl')
 end { Repl };
 
 
 procedure Add;
 var Lower: integer;
     A, B, R: Bit20;
 begin { Add }
  Pop(B);
  Pop(A);
  Lower := A.Lower + B.Lower;
  R.Upper := LAnd(A.Upper + B.Upper + Shift(Lower,-10),#1777);
  R.Lower := LAnd(Lower,#1777);
  Push(R)
  ;PrintStack('Add')
 end { Add };
 
 
 procedure Sub;
 var Upper, Lower: integer;
     A, B, R: Bit20;
 begin { Sub }
  Pop(B);
  Pop(A);
  Lower := A.Lower - B.Lower;
  Upper := A.Upper - B.Upper;
  if Upper < 0 then Upper := Upper + #2000;
  if Lower < 0 then
   begin Upper := Upper - 1; Lower := Lower + #2000 end;
  if Upper < 0 then Upper := Upper + #2000;
  R.Upper := Upper;
  R.Lower := Lower;
  Push(R)
  ;PrintStack('Sub')
 end { Sub };
 
 
 procedure TLate;
 var Seg, Off, SATAddr, Word, Addr: Bit20;
     T: Integer;
 begin { TLate }
  Pop(Off);
  Repl; Add; Pop(SATAddr);     { Seg + Seg }
  Push(SATAddr); ReadMem; PopInt(T);
  if not Odd(T) then { segment is resident }
   begin T := LAnd(T,#177400);
    Push(SATAddr); PushInt(1); Add; ReadMem; Pop(Word);
    Addr.Upper := Shift(LAnd(Word.Lower,#17),6);
    Addr.Lower := 0;
    PushInt(T); Push(Addr); Add; Push(Off); Add
   end
  else Push(Nill)
  ;PrintStack('TLate')
 end { TLate };

 
 function EQ: Boolean;
 var A, B: Bit20;
 begin { EQ }
  Pop(B);
  Pop(A);
  EQ := (A.Upper = B.Upper) and (A.Lower = B.Lower)
  ;PrintStack('EQ')
 end { EQ };
 
 
 function NE: Boolean;
 var A, B: Bit20;
 begin { NE }
  Pop(B);
  Pop(A);
  NE := (A.Upper <> B.Upper) or (A.Lower <> B.Lower)
  ;PrintStack('NE')
 end { NE };
 
 
 function LT: Boolean;
 var A, B: Bit20;
 begin { LT }
  Pop(B);
  Pop(A);
  LT := (A.Upper < B.Upper) or
        ((A.Upper = B.Upper) and (A.Lower < B.Lower))
  ;PrintStack('LT')
 end { LT };
 
 
 function LE: Boolean;
 var A, B: Bit20;
 begin { LE }
  Pop(B);
  Pop(A);
  LE := (A.Upper < B.Upper) or
        ((A.Upper = B.Upper) and (A.Lower <= B.Lower))
  ;PrintStack('LE')
 end { LE };
 
 
 function GT: Boolean;
 var A, B: Bit20;
 begin { GT }
  Pop(B);
  Pop(A);
  GT := (A.Upper > B.Upper) or
        ((A.Upper = B.Upper) and (A.Lower > B.Lower))
  ;PrintStack('GT')
 end { GT };
 
 
 function GE: Boolean;
 var A, B: Bit20;
 begin { GE }
  Pop(B);
  Pop(A);
  GE := (A.Upper > B.Upper) or
        ((A.Upper = B.Upper) and (A.Lower >= B.Lower))
  ;PrintStack('GE')
 end { GE };
 
 
 procedure ClearQSegments;
 var P, S: Integer;
 begin { ClearQSegments }
{$ifc MPOS then}
  for P := 1 to MaxQProc do
    with QProc[P] do
      begin
        PName := '';
        PStackSegment := 0;
        for S := 1 to MaxQSeg do GDB[S] := 0
      end;
  for S := 1 to MaxQSeg do QSeg[S] := ''
{$elsec}
  for S := 1 to MaxQSeg do QSeg[S].Name := ''
{$endc}
 end { ClearQSegments };
 
 
 procedure Match( Source, Mask: String; var L: Integer );
 begin { Match }
   ConvUpper(Source);
   ConvUpper(Mask);
   if Source = Mask then L := 10000
   else
     if Pos(Source,Mask) = 1 then L := Length(Source)
     else L := 0
 end { Match };
 
 
 procedure QProcess;
 var P: Integer;
{$ifc MPOS then}
     NotFound, NotUnique: Boolean;
     BestP: Integer;
     ThisL, BestL: Integer;
     Default: String;
     ProcessCB: PCB;
{$endc}
 begin { QProcess }
   P := CurrentProcess;
{$ifc MPOS then}
   if (P <= 0) or (P > MaxQProc) then Default := ''
   else
     if QProc[P].PName = '' then Default := ''
     else Default := Concat(' [', Concat(QProc[P].PName, ']'));
   ReadString(Concat('Process name or number', Default));
   if CmdString = '' then
     begin
       if Default = '' then Error('Process not found')
     end
   else
     begin
       if CmdString[1] in ['-', '0'..'9'] then
         begin AppendChar(CmdString,BreakCh);
           CmdLine := Concat(CmdString,CmdLine);
           PushInt(0); ReadNumber('Process number'); PopInt(P);
           if P = 0 then P := CurrentProcess
         end
       else
         begin
           BestP := 0;
           BestL := 0;
           NotFound := True;
           NotUnique := False;
           for P := 1 to MaxQProc do
             begin
               Match(QProc[P].PName,CmdString,ThisL);
               if ThisL = BestL then NotUnique := True
               else
                 if ThisL > BestL then
                   begin
                     NotFound := False;
                     NotUnique := False;
                     BestP := P;
                     BestL := ThisL
                   end
             end;
           if NotUnique then Error('Process not unique');
           if NotFound then Error('Process not found');
           P := BestP
         end
     end;
   if (P < 1) or (P > MaxQProc) then Error('Process number out of range.');
{$endc}
   PushInt(P);
   ;PrintStack('QProcess')
 end { QProcess };
 
 
 procedure QSegment( Process: Integer );
   { process must be 0 or valid }
 var S: Integer;
     NotFound, NotUnique: Boolean;
     BestS: Integer;
     ThisL, BestL: Integer;
     Default, TryS: String;
 begin { QSegment }
   ReadString('Segment name or number');
   if CmdString = '' then Error('Segment not found')
   else
     if CmdString[1] in ['-', '0'..'9'] then
       begin AppendChar(CmdString,BreakCh);
         CmdLine := Concat(CmdString,CmdLine);
         PushInt(0); ReadNumber('Segment number'); PopInt(S)
       end
     else
       begin
         BestS := 0;
         BestL := 0;
         NotFound := True;
         NotUnique := False;
         for S := 1 to MaxQSeg do
           begin
{$ifc MPOS then}
             if Process = 0 then TryS := QSeg[S]
             else
               if QProc[Process].GDB[S] <> 0 then TryS := QSeg[S]
               else TryS := ' untypable ';
{$elsec}
             TryS := QSeg[S].Name;
             if TryS = '' then TryS := ' untypable ';
{$endc}
             Match(TryS,CmdString,ThisL);
             if ThisL <> 0 then
               if ThisL = BestL then NotUnique := True
               else
                 if ThisL > BestL then
                   begin
                     NotFound := False;
                     NotUnique := False;
                     BestS := S;
                     BestL := ThisL
                   end
             end;
         if NotUnique then Error('Segment not unique');
         if NotFound then Error('Segment not found');
         S := BestS
       end;
   PushInt(S);
   ;PrintStack('QSegment')
 end { QSegment };
 
 
 procedure QSegName( var F: Text; PrcNumber, SegNumber: Integer );
 var PrintName: Boolean;
 begin { QSegName }
{$ifc MPOS then}
   PrintName := False;
   if (SegNumber > 0) and (SegNumber <= MaxQSeg) then
     if PrcNumber = 0 then PrintName := QSeg[SegNumber] <> ''
     else
       if (PrcNumber > 0) and (PrcNumber <= MaxQProc) then
         if QProc[PrcNumber].GDB[SegNumber] <> 0
           then PrintName := QSeg[SegNumber] <> '';
   if PrintName then
     Write(F, QSeg[SegNumber])
   else
    begin
     PushInt(SegNumber);
     Oct(F,1)
    end
{$elsec}
  if (SegNumber > 0) and (SegNumber <= MaxQSeg) then
   if QSeg[SegNumber].Name <> '' then Write(F, QSeg[SegNumber].Name)
   else
    begin
     PushInt(SegNumber);
     Oct(F,1)
    end
  else
   begin
    PushInt(SegNumber);
    Oct(F,1)
   end
{$endc}
 end { QSegName };
 
 
 procedure QPrcName( var F: Text; PrcNumber: Integer );
 begin { QPrcName }
{$ifc MPOS then}
   if PrcNumber = 0 then PrcNumber := CurrentProcess;
   if (PrcNumber > 0) and (PrcNumber <= MaxQProc) then
     if QProc[PrcNumber].PName <> '' then
       Write(F, QProc[PrcNumber].PName)
     else
      begin
       PushInt(PrcNumber);
       Int(F,1)
      end
   else
     begin
      PushInt(PrcNumber);
      Int(F,1)
     end
{$endc}
 end { QPrcName };
   
 
 procedure ReadState( Process: Integer;
                      var SS, SB, AP, GP, LP, CS, CB, RN, UPC, PC: Bit20 );
{$ifc MPOS then}
 label 1;
 const PCBSize = WordSize(PCB);
{$endc}
 var PP: Bit20;
     T, Instruction: Integer;
{$ifc MPOS then}
     ProcessCB: record case Integer of
                  1: (W: array[0..PCBSize-1] of Integer);
                  2: (CB: PCB)
                  end;
{$endc}
     P, S: Integer;
 begin { ReadState }
{$ifc MPOS then}
   if QProc[Process].PStackSegment = 0 then
     begin
       for I := 0 to PCBSize-1 do
         begin
           PushInt(PrcSNumber);
           PushInt((Process-1)*PCBSize+I);
           TLate;
           ReadMem;
           PopInt(ProcessCB.W[I])
         end;
       QProc[Process].PStackSegment := ProcessCB.CB.SSN;
       for S := 1 to MaxQSeg do
         begin
           for P := 1 to MaxQProc do
             if QProc[P].GDB[S] <> 0 then
               begin
                 QProc[Process].GDB[S] := QProc[P].GDB[S];
                 goto 1
               end;
       1:end
     end;
{$endc}
{$ifc MPOS then}
   if Process = CurrentProcess then
     begin
{$endc}
       PushInt(#12); ReadReg; Pop(SS);
       PushInt(#7);  ReadReg; Pop(SB);
       PushInt(#3);  ReadReg; Push(SB); Sub; Pop(AP);
       PushInt(#4);  ReadReg; Push(SB); Sub; Pop(GP);
       PushInt(#5);  ReadReg; Push(SB); Sub; Pop(LP);
       PushInt(#11); ReadReg; Pop(CS);
       PushInt(#6);  ReadReg; Pop(CB);
       PushInt(#10); ReadReg; Pop(RN); Push(RN);
       PushInt(#16); ReadReg; Push(CB); Sub; Pop(UPC);
         Push(UPC); Repl; Add; Pop(PC);         { (UPC - CB) * 2 }
       PushInt(#364); ReadReg; PopInt(T); T := LAnd(T,#17);
         PushInt(T); Push(PC); Add;             { (UPC - CB) * 2 + BPC }
         Pop(PC)                                { byte PC offset from CB }
{$ifc MPOS then}
     end
   else
     begin
       PushInt(QProc[Process].PStackSegment); Pop(SS);
       Push(SS); PushInt(0); TLate; Pop(SB);
       PushInt(PrcSNumber); PushInt((Process-1)*WordSize(PCB)+4); TLate;
         { address of PP in processes PCB }
       ReadMem; Pop(PP);
       Push(PP); Push(SB); Add; Pop(PP);
       Push(PP); PushInt(ACBDL); Add; ReadMem; Pop(AP);
       Push(PP); PushInt(ACBGL); Add; ReadMem; Pop(GP);
       Push(PP); PushInt(ACBRS); Add; ReadMem; Pop(CS);
       Push(PP); PushInt(ACBRA); Add; ReadMem; Pop(PC);
       Push(PP); PushInt(ACBRR); Add; ReadMem; Pop(RN);
       Push(AP); Push(SB); Add; PushInt(ACBLP); Add; ReadMem; Pop(LP);
       Push(CS); PushInt(0); TLate; Pop(CB);
       Push(PC); PopInt(Instruction);
       Push(CB); PushInt(LAnd(Shift(Instruction,-1),LNot(3))); Add; Pop(UPC)
     end
{$endc}
 end { ReadState };

 
 function RelAddr( CB, PC: Bit20; RN: Integer ): Bit20;
 { CB - CodeBase,  PC - Byte PC (offset from CB),  RN - Routine number }
 var RDAddr, EntryPoint, ExitPoint, Offset: Bit20;
 begin { RelAddr }
   RelAddr := Nill;                          { assume bad }
   Push(CB); ReadMem;                        { routine dictionary address }
   PushInt(RN * 8); Add;                     { routine dictionary entry }
   Push(CB); Add; Pop(RDAddr);               { address of RD entry }
   Push(RDAddr); PushInt(RDEntry); Add; ReadMem; { entry point address }
   Pop(EntryPoint);                          { entry point }
   Push(RDAddr); PushInt(RDExit); Add; ReadMem;  { exit point address }
   Pop(ExitPoint);                           { exit point }
   Push(PC); Push(EntryPoint);
   if {PC} LT {EntryPoint} then Exit(RelAddr);   { return bad address }
   Push(PC); Push(ExitPoint);
   if {PC} GT {ExitPoint} then Exit(RelAddr);    { return bad address }
   Push(PC); Push(EntryPoint); Sub;          { offset from entry point }
   Pop(Offset);
   RelAddr := Offset                         { return relative address }
 end { RelAddr };


 procedure VarAddress;
 var Process, Segment, Routine, Offset, I, CS, RN: Integer;
     SS, SB, AP, GP, LP, CSb20, CB, RNb20, UPC, PC: Bit20;
 begin { VarAddress }
  PopInt(Offset);
  PopInt(Routine);
  PopInt(Segment);
  PopInt(Process);
  if Process = 0 then Process := CurrentProcess;
  if Process < 0 then
    begin
      PushInt(Segment);
      PushInt(Offset);
      TLate
    end
  else
    begin
{$ifc MPOS then}
      if (Process <= 0) or (Process > MaxQProc) then
        Error('Unable to find this process');
      with QProc[Process] do
{$endc}
        begin
          ReadState( Process, SS, SB, AP, GP, LP, CSb20, CB, RNb20, UPC, PC );
          Push(CSb20); PopInt(CS);
          Push(RNb20); PopInt(RN);
          Push(SB); Push(Nill);
          if {SB} EQ {Nill} then Push(Nill)
          else
            if Routine < 0 then { global }
              begin
                if (Segment < 0) or (Segment > MaxQSeg) then
                  Error('Unable to find globals for this segment');
{$ifc MPOS then}
                if GDB[Segment] = 0 then
                  Error('Unable to find globals for this segment');
{$elsec}
                if QSeg[Segment].Name = '' then
                  Error('Unable to find globals for this segment');
{$endc}
                Push(SB);
{$ifc MPOS then}
                PushInt(GDB[Segment]);
{$elsec}
                PushInt(QSeg[Segment].GDB);
{$endc}
                Add;
                PushInt(Offset);
                Add
              end
            else
              begin
                I := 0;
                Push(AP); PushInt(0);
                while (I < 100) and {AP} NE {0} and
                      ((CS <> Segment) or (RN <> Routine)) do
                  begin Push(AP); Push(SB); Add; Pop(AP);
                    Push(AP); PushInt(5); Add; ReadMem; PopInt(CS);
                    Push(AP); PushInt(7); Add; ReadMem; PopInt(RN);
                    Push(AP); PushInt(2); Add; ReadMem; Pop(AP);
                    I := I + 1;
                    Push(AP); PushInt(0)
                  end;
                if (CS = Segment) and (RN = Routine) then
                  begin Push(AP); Push(SB); Add; PushInt(1); Add; ReadMem;
                    PushInt(Offset); Add;
                    Push(SB); Add
                  end
                else Push(Nill)
              end
        end
    end
  ;PrintStack('VarAddress')
 end { VarAddress };


 procedure Int(var F: text; Width: Integer);
 begin { Int }
  Number(F,Width,10)
 end { Int };
 
 
 procedure UInt(var F: text; Width: Integer);
 begin { SInt }
  Number(F,Width,-10)
 end { SInt };
 
 
 procedure Oct(var F: text; Width: Integer);
 begin { Oct }
  Number(F,Width,-8)
 end { Oct };
 
 
 procedure SOct(var F: text; Width: Integer);
 begin { SOct }
  Number(F,Width,8)
 end { SOct };
 
 
 procedure Number(var F: Text; Width, Base: Integer);
  var Ch: Char;
      D: array[1..21] of char;
      Negative: Boolean;
      T, N, K: integer;
      Val: Bit20;
      LDEBUG: Boolean;
 begin { Number }
  LDEBUG := DEBUG; DEBUG := False;
  Pop(Val);
  Negative := False;
  Val.Upper := LAnd(Val.Upper,#1777);
  Val.Lower := LAnd(Val.Lower,#1777);
  if Base < 0 then { write unsigned } Base := -Base
  else
   { write signed number }
   if (LAnd(Val.Upper,#1000) <> 0) or (LAnd(Val.Upper,#1740) = #40) then
    begin Negative := True;
     Val.Upper := LOr(Val.Upper,#1740);
     PushInt(0); Push(Val); Sub; Pop(Val)
    end;
  N := 0;
  repeat N := N + 1;
   T := Val.Upper mod Base;
   Val.Upper := Val.Upper div Base;
   T := Val.Lower + Shift(T,10);
   Val.Lower := T div Base;
   K := T mod Base;
   if K >= 10 then D[N] := Chr(K - 10 + Ord('A'))
   else D[N] := Chr(K + Ord('0'))
  until Val.Upper + Val.Lower = 0;
  if Negative then
   begin N := N + 1; D[N] := '-' end;
  for K := N + 1 to Width do
   begin F^ := ' '; Put(F) end;
  if Base = 10 then SetFont(Font10);
  for K := N downto 1 do
   begin F^ := D[K]; Put(F) end;
  if Base = 10 then SetFont(Font8);
  ;DEBUG := LDEBUG;
  PrintStack('Number')
 end { Number };

  
 procedure DisplayVal( Kind: ValKind; Form: ValForm );
  { field widths:  Character  4
                   Wrd        8
                   Byt        9 for memory,
                             11 for register }
 var Val: Bit20;
     B: Integer;
     NoValue: Boolean;
 begin { DisplayVal }
  NoValue := False;
  case Kind of
   RKind: ReadReg;
   MKind: ReadMem;
   VKind: begin VarAddress; Repl; Push(Nill);
           if {VarAddress} EQ {Nill} then
            begin NoValue := True; Pop(Val) end
           else ReadMem
          end
   end;
  if DisplayWindow <> CmdWindow then
   begin ChangeWindow(DisplayWindow);
    SSetCursor(DisplayX,DisplayY)
   end;
  with Form do
   if Base = Character then
    if NoValue then Write(OutFile, 'No V')
    else
     begin PopInt(Bytes.Int);
      Write(OutFile, '"');
      if Bytes.Ch1 in [#40..#177] then Write(Chr(Bytes.Ch1)) else Write(' ');
      if Bytes.Ch0 in [#40..#177] then Write(Chr(Bytes.Ch0)) else Write(' ');
      Write(OutFile, '"')
     end
   else
    begin
     if Base = Octal then B := 8 else B := 10;
     if Size = Wrd then
      if NoValue then Write(OutFile, 'No Value')
      else
       if Sign = Signed then Number(OutFile,8,B) else Number(OutFile,8,-B)
     else { Size = Byt }
      if NoValue then Write(OutFile, ' No Value')
      else
       begin
        if Kind = RKind then
         begin
          Repl;
          Pop(Val);
          PushInt(Shift(Val.Upper,#177));
          Number(OutFile,2,B)
         end;
        PopInt(Bytes.Int);
        if Sign = Signed then
         begin
          PushInt(LOr( Bytes.Byt1, LAnd(Bytes.Byt1,#200)*#777 ));
          Number(OutFile,4,B);
          PushInt(LOr( Bytes.Byt0, LAnd(Bytes.Byt0,#200)*#777 ));
          Number(OutFile,4,B)
         end
        else { UnSigned }
         begin
          PushInt(Bytes.Byt1);
          Number(OutFile,4,B);
          PushInt(Bytes.Byt0);
          Number(OutFile,4,B)
         end
       end
    end;
  if DisplayWindow <> CmdWindow then ChangeWindow(CmdWindow);
 end { DisplayVal };
 

 procedure WriteReg;
 var Val: Integer;
 begin { WriteReg }
  WriteWord(LrA);
  PopInt(Bytes.Int);
  Bytes.byt1:=Bytes.byt0;
  WriteWord(Bytes.int);
  WriteWord(WR);
  PopInt(Val);
  WriteWord(Val)
  ;PrintStack('WriteReg')
 end { WriteReg };


 procedure ReadReg;
   var Bottom16: packed record case integer of
                  1: (i: integer);
                  2: (Lower: 0..#1777;
                      Upper: 0..#77)
                  end;
       Top4: packed record case integer of
              1: (i: integer);
              2: (fill: 0..#7777;
                  top4: 0..#17)
              end;
       Val: Bit20;
 begin { ReadReg }
  PopInt(Bytes.Int);
  WriteWord(LrA);
  Bytes.byt1:=Bytes.byt0;
  WriteWord(Bytes.int);
  WriteWord(RR);
  Bottom16.i := ReadWord(TryRead);
  WriteWord(0);
  Top4.i := ReadWord(TryRead);
  Val.Upper := (#17 - Top4.top4) * #100 + Bottom16.Upper;
  Val.Lower := Bottom16.Lower;
  Push(Val)
  ;PrintStack('ReadReg')
 end { ReadReg };
 
 
 procedure ReadMem;
 var Addr: Bit20;
 begin { ReadMem }
  Pop(Addr);
  WriteWord(LmA);
  WriteWord(Shift(Addr.Upper,10) + Addr.Lower);
  WriteWord(Shift(Addr.Upper,-6));
  WriteWord(RmW);
  PushInt(ReadWord(TryRead))
  ;PrintStack('ReadMem')
 end { ReadMem };
 
 
 procedure WriteMem;
 var Addr: Bit20;
     Val: Integer;
 begin { WriteMem }
  Pop(Addr);
  WriteWord(LmA);
  WriteWord(Shift(Addr.Upper,10) + Addr.Lower);
  WriteWord(Shift(Addr.Upper,-6));
  WriteWord(WmW);
  PopInt(Val);
  WriteWord(Val)
  ;PrintStack('WriteMem')
 end { WriteMem };


 procedure WriteMicro(Val: MicroInstruction);
 var A: Integer; byt:0..255;
 begin { WriteMicro }
  PopInt(A);
  with Bytes do
         begin
         int:=A;
         byt:=byt1;
         byt1:=byt0;
         byt0:=byt;
         WriteWord(LuA);
         WriteWord(int)
         end;
  Translate(Val,TMI);
  WriteWord(WUWa);
  WriteWord(LNot(TMI.Word1));
  WriteWord(WUWb);
  WriteWord(LNot(TMI.Word2));
  WriteWord(WUWc);
  WriteWord(LNot(TMI.Word3))
  ;PrintStack('WriteMicro')
 end { WriteMicro };


 procedure StartPerq(A:Integer);
 { Start PERQ going at uword address adr }
   var byt:0..255;
 begin
 PushInt(#377); PushInt(#370); WriteReg; { clear breakpoint number }
 WriteWord(LuA);
 with Bytes do
         begin
         int:=A;
         byt:=byt1;
         byt1:=byt0;
         byt0:=byt;
         WriteWord(int)
         end;
 WriteWord(SuP)                { here we go }
  ;PrintStack('StartPerq')
 end { StartPerq };
 
 
 function UnusedBkp: Integer;
 var B: Integer;
 begin { UnusedBkp }
  B := 0;
  repeat B := B + 1
  until (Bkp[B].Kind = Unused) or (B = NBkp - 2);
  if Bkp[B].Kind = Unused then UnusedBkp := B
  else Error('All breakpoints are in use.')
 end { UnusedBkp };
 
 
 procedure SetUBkp(B: Integer; Addr: Integer);
 var Tmp: MicroInstruction;
 begin { SetUBkp }
  if Addr <= #7777 then
   begin Tmp := BkpLong; Tmp.Y := Tmp.Y + B end
  else
   begin Tmp := BkpLeap; Tmp.Z := Tmp.Z - B end;
  PushInt(Addr); WriteMicro(Tmp);
  with Bkp[B] do
   begin Kind := UBkp; UAddr := Addr end
  ;PrintStack('SetUBkp')
 end { SetUBkp };
 
 
 procedure KillUBkp(B: Integer);
 var I, J: Integer;
 begin { KillUBkp }
  with Bkp[B] do
   begin
    if Kind = UBkp then
     begin
      I := UAddr div UArraySize;
      J := UAddr mod UArraySize;
      CreateUCode(UCode[I]);
      PushInt(UAddr);
      WriteMicro(UCode[I]^[J])
     end;
    Kind := Unused
   end
  ;PrintStack('KillUBkp')
 end { KillUBkp };
 
 
 procedure ClearUBkps;
 var B: Integer;
 begin { ClearUBkps }
  for B := 1 to NBkp do
   if Bkp[B].Kind = UBkp then
    KillUBkp(B)
 end { ClearUBkps };
 
 
 function ExchangeQCode( QCode: Integer;
                         Segment, Routine, Instruction: Integer ): Integer;
 var Base, RD, T, Entry, Word, WordAddress: Bit20;
     ByteAddress: Integer;
     UpperByte: Boolean;
 begin { ExchangeQCode }
  PrintNoDone := False;
  PushInt(Segment); Push(Zero); TLate; Pop(Base);
  PrintNoDone := True;
  if NoDoneBit then Exit(ExchangeQCode);
  Push(Base); Push(Nill);
  if {Base} EQ {Nill} then
   begin Write(OutFile, '  Segment ');
    PushInt(Segment);
    Oct(OutFile,1);
    Write(OutFile, ' is not resident.');
    Exit(ExchangeQCode)
   end;
  Push(Base); PushInt(0);
  if {Base} EQ {0} then Exit(ExchangeQCode);
  Push(Base); ReadMem; Push(Base); Add;
     PushInt(Routine * 8); Add;
     PushInt(RDEntry); Add;
     ReadMem;                    { entry point address }
     PushInt(Instruction); Add;
     PopInt(ByteAddress);
  UpperByte := Odd(ByteAddress);
  PushInt(Shift(ByteAddress,-1)); Push(Base); Add; Pop(WordAddress);
  Push(WordAddress); ReadMem; PopInt(Bytes.Int);
  if UpperByte then
   begin ExchangeQCode := Bytes.Byt1; Bytes.Byt1 := QCode end
  else
   begin ExchangeQCode := Bytes.Byt0; Bytes.Byt0 := QCode end;
  PushInt(Bytes.Int); Push(WordAddress); WriteMem
 end { ExchangeQCode };
 

 procedure SetQBkp( B: Integer; Segment, Routine, Instruction: Integer );
 begin { SetQBkp }
  with Bkp[B] do
   begin Kind := QBkp;
    Seg := Segment;
    Rtn := Routine;
    Instr := Instruction;
    QCode := ExchangeQCode(BkpQCode,Segment,Routine,Instruction)
   end
  ;PrintStack('SetQBkp')
 end { SetQBkp };
 
 
 procedure KillQBkp( B: Integer );
 var Ignore: Integer;
 begin { KillQBkp }
  with Bkp[B] do
   begin
    Ignore := ExchangeQCode(QCode,Seg,Rtn,Instr);
    Kind := Unused
   end
  ;PrintStack('KillQBkp')
 end { KillQBkp };
 
 
 procedure ClearQBkps;
 var B: Integer;
 begin { ClearQBkps }
  for B := 1 to NBkp do
   if Bkp[B].Kind = QBkp then
    KillQBkp(B)
 end { ClearUBkps };
 
 
 procedure Update(ForceAll: boolean);
 Label 1;
 var i,j,k: integer;
     VAddr, Valu: Bit20;
 begin { Update }
  DisplayWindow := SlotWindow;
  ChangeWindow(SlotWindow);
  if ForceAll then Write(FF);
  ChangeWindow(CmdWindow);
  PrintNoDone := False;
  for i := 0 to MaxX do
   for j := 0 to MaxY do
    with slot[i,j] do if slKind <> NoKind then
     begin
      Case slKind of
       MKind: begin Push(slLoc); ReadMem end;
       RKind: begin Push(slLoc); ReadReg end;
       VKind: begin Push(slProcess); Push(slSegment);
                 Push(slRoutine); Push(slOffset);
                 VarAddress; Pop(VAddr); Push(VAddr);
                 Push(Nill);
                 if {VAddr} NE {Nill} then
                  begin Push(VAddr); ReadMem end
                 else
                  begin Push(Nill); PushInt(1); Sub end
              end;
       end;
      Pop(Valu);
      Push(Valu); Push(slVal);
      if NoDoneBit then Goto 1;
      if {Valu} NE {slVal} or ForceAll then
       begin
        ChangeWindow(SlotWindow);
        SSetCursor(i*Xwidth+leftmargin,(j+1)*Height+topmargin);
        for k := 0 to slotlen do SPutChr(' ');
        SSetCursor(i*Xwidth+leftmargin,(j+1)*Height+topmargin);
        Case slNameDsp of
         NumberDsp: Case slKind of
                     MKind: begin 
                              write(OutFile,'M');
                              Push(slLoc);
                              Oct(OutFile,1)
                            end;
                     RKind: begin
                              write(OutFile,'R');
                              Push(slLoc);
                              Oct(OutFile,1)
                            end;
                     VKind: begin
                              write(OutFile,'V');
                              Push(VAddr);
                              Oct(OutFile,1)
                            end;
                 end;
         SymbolDsp: begin
                      write(slName:1,' ');
                    end;
         end;
        ChangeWindow(CmdWindow);
        DisplayX := i*XWidth+NameWidth+leftmargin;
        DisplayY := (j+1)*Height+topmargin;
        if slKind = VKind then
         begin
          Push(slProcess); Push(slSegment); Push(slRoutine); Push(slOffset)
         end
        else Push(slLoc);
        DisplayVal(slKind,slForm);
        slVal := Valu;
       end {if Val <>};
     end; {with slot[x,y]}
1:PrintNoDone := True;
  DisplayWindow := CmdWindow
 end { Update };
     
 
 procedure InitSlots;
  var KSet: FontPtr;
      Windx: WinRange;
      OrgX, OrgY, WinWidth, WinHeight: Integer;
      HasTitle: Boolean;
 begin { InitSlot }
   IOSetModeTablet(relTablet);
   KSet := GetFont;
   Height := KSet^.Height;
   ScreenP := MakePtr(ScreenSeg,0,RasterPtr);
   IOCursorMode(trackCursor);
   OdtPrompt :=  'Reg  ,Mem  ,Boot ,Break,Kill ,QBrea,QKill,Go   ,Proc ,Load ,Over ,QLoad,Watch';
   for i := 0 to MaxX do for j := 0 to MaxY do with slot[i,j] do
    begin
     slKind := NoKind;
     slVal := nill;
     slForm.Sign := UnSigned; slForm.Base := Octal; slForm.Size := Wrd;
     slNameDsp := NumberDsp;
     slName := '';
    end;
   OnSlots := false;
   OnCmd := false;
   DisplayWindow := SlotWindow;
   GetWindowParms(WindX,OrgX,OrgY,WinWidth,WinHeight,HasTitle);
   SSize := (OrgY + WinHeight + 127) div 128 * 128;
   CreateWindow(SlotWindow,0,0,768,slWinLen,
                Concat( Concat('OdtPrq ', OdtPrqVersion),
                        '     Type "help" if you need it.'));
   CreateWindow(CmdWindow,0,slWinLen,768,SSize-slWinLen,OdtPrompt);
   DisplayWindow := CmdWindow;
   IOSetModeTablet(relTablet)
 end { InitSlot };

 
 procedure CreateUCode( var UA: pUArray );
 var S: SegmentNumber;
     I: Integer;
 begin { CreateUCode }
  if UA = nil then
   begin
    CreateSegment(S,SizeControlStore,1,SizeControlStore);
    New(S,1,UA);
    for I := 0 to UArraySize - 1 do UA^[I] := EmptyInstruction
   end
 end { CreateUCode };
 
 
 procedure DestroyUCode( var UA: pUArray );
 var P: record case Integer of
         1: (P: pUArray);
         2: (Off: Integer;
             Seg: SegmentNumber)
         end;
 begin { DestroyUCode }
  if UA <> nil then
    begin
      P.P := UA;
      DecRefCount(P.Seg);
      UA := nil
    end
 end { DestroyUCode }.
