{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{-----------------------------------------
{ Abstract:
{
{
{  copyright 1983  Three Rivers Computer Corporation
{
{ Parameters:
{
{ Results:
{
{ Side Effects:
{
{ Errors:
{
{ Design:
{-----------------------------------------}
{$R-}
{Change history::
            2- Apr-81  DCF  V0.4   Converted to run under new system.
            30-Oct-80  MAB  V0.3   Communication protocal revamped.
            20-Oct-80  BAM  V0.2   Fixed lots of bugs; split in half.
            ??-???-80  MAB  V0.1A  Started
            
}

program PDM(input,output);

EXPORTS
imports PDCommon from PDCommon;
procedure ExitPDM;

PRIVATE

   imports PDM2 from PDM2;
   imports System from System;
   imports FileSystem from FileSYstem;
   imports Memory from Memory;
   
Procedure EvalStatus(var StatBlk: PDCStatBlock); forward;

procedure ExitPDM;
  begin
  exit(PDM);
  end;

Procedure MicCommand;
{-----------------------------------------
{
{ Abstract:
{    Complete parsing and execution of MICRO commands
{
{ Side Effects:
{    CurState and CurAdr are modified.
{-----------------------------------------}
  label 1;
  var BrkChr: string[1]; LoVal,HiVal: integer; Sym:string;
begin
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
if length(Sym) = 0 then
    1: writeln('MICRO <micro-address>')
else
    if GetVal(Sym,HiVal,LoVal,12, curRadix) then
        begin
        DisplayMicro(LoVal, curRadix);
        CurState:=UCode;
        CurAdr.Lo16:=LoVal
        end
    else
        goto 1
end { MicCommand };

Procedure PrintReg(Reg:integer);
{-----------------------------------------
{
{ Abstract:
{    Print the contents of a register
{
{ Parameters:
{    Reg: The number of the register to be printed
{-----------------------------------------}
  var HiVal,LoVal:integer;
begin
write('R');
OutNum(Reg,3, curRadix);
write('/  ');
LoVal:=ReadReg(Reg,HiVal);
OutNum(HiVal,2, curRadix);
write(',,');
OutNum(LoVal,6, curRadix);
writeln
end { PrintReg };

Procedure DoRadix;
{-----------------------------------------
{
{ Abstract:
{    Allows specifying default radix or prints current value if no arg
{
{ Side Effects:
{    CmdLine, CurRadix are modified.
{-----------------------------------------}
  var BrkChr: string[1]; Sym:string;
begin
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
If length(Sym) = 0 then
   WriteLn('Current radix is: ',curRadix:1,'.  Choices are: 2, 8, 10, 16')
else if sym = '2' then curRadix := 2
else if sym = '8' then curRadix := 8
else if sym = '10' then curRadix := 10
else if sym = '16' then curRadix := 16
else writeLn(' ?Bad radix "',sym,'". Choices are: 2, 8, 10, 16');
end;


Procedure RegCommand;
{-----------------------------------------
{
{ Abstract:
{    Complete parsing and execution of REGISTER commands
{
{ Side Effects:
{    CmdLine, CurState and CurAdr are modified.
{-----------------------------------------}
  label 1;
  var BrkChr: string[1];  StartVal,LoVal,HiVal,i: integer; Sym:string;
begin
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
RemDelimiters(CmdLine,Dels,BrkChr);
if length(Sym) = 0 then
   1: writeln('REGISTER <reg-num>[:<reg-num>] | REGISTER <reg-num>=<data>')
else
    if GetVal(Sym,HiVal,LoVal,8, curRadix) then
        begin
        StartVal:=LoVal;
        if length(BrkChr) = 0 then
            begin
            CurState:=Register;
            CurAdr.Lo16:=StartVal;
            PrintReg(StartVal)
            end
        else if BrkChr[1] = '=' then
            begin
            LOp(CmdLine);
            RemDelimiters(CmdLine,Dels,BrkChr);
            GetSymbol(CmdLine,Sym,Breaks,BrkChr);
            if GetVal(Sym,HiVal,LoVal,16, curRadix) then
                begin
                CurState:=Register;
                CurAdr.Lo16:=StartVal;
                WriteReg(StartVal,LoVal)
                end
            end
        else if BrkChr[1] = ':' then
            begin
            LOp(CmdLine);
            RemDelimiters(CmdLine,Dels,BrkChr);
            GetSymbol(CmdLine,Sym,Breaks,BrkChr);
            if GetVal(Sym,HiVal,LoVal,8, curRadix) then
                begin
                CurState:=Register;
                CurAdr.Lo16:=LoVal;
                for i:=StartVal to LoVal do
                    PrintReg(i)
                end
            else
                goto 1
            end
        else
            goto 1
        end
    else
        goto 1
end { RegCommand };

Function YesOrNo(oldVal: BOOLEAN): BOOLEAN;
{-----------------------------------------
{
{ Abstract:
{    Get a yes or no answer from the user.
{
{ Parameters:
{    The default value.
{
{ Results:
{    The new value.
{
{ Design:
{    The user can either type Yes or No, or type X or <return> to obtain
{    the default value.
{-----------------------------------------}
  var sym: STRING; brk: STRING[1];
begin
while true do
     begin
     NeedInput;
     RemDelimiters(CmdLine,Dels,Brk);
     GetSymbol(CmdLine,Sym,Break1,Brk);
     if length(sym) > 0 then
        if sym[1] in ['y','Y'] THEN BEGIN
                                    YesOrNo := True;
                                    exit(YesOrNo)
                                    end
        else if sym[1] in ['n','N'] THEN BEGIN
                                    YesOrNo := FALSE;
                                    exit(YesOrNo)
                                    end
        else if sym[1] in ['x','X'] THEN BEGIN
                                    YesOrNo := oldVal;
                                    exit(YesOrNo)
                                    end
        else Write('Yes, No, X, or <return>: ')
     else
         begin
         YesOrNo := oldVal;
         exit(YesOrNo)
         end
     end
end; {YesOrNo}
  
Procedure WriteYesOrNo(b: BOOLEAN);
{-----------------------------------------
{
{ Abstract:
{    Write Yes or No
{
{ Parameters:
{    boolean telling Yes (true) or No (false)
{-----------------------------------------}
begin
if b then
    write('Yes')
else
    write('No')
end { WriteYesOrNo };

Procedure ReadStatus(var StatBlk: PDCStatBlock);
{-----------------------------------------
{
{ Abstract:
{    Read the PDS Status block
{
{ Parameters:
{    StatBlk: where to put the result
{-----------------------------------------}
  var i,junk:integer; PA: PhysAdr;
begin
if Running then
    begin
    writeln('?FATAL INTERNAL ERROR');
    writeln('?Attempt to get PDS status while PDS was running');
    write('*');
    exit(ReadStatus)
    end;
if CurTest.TstType = MicroTest then
    for i:= 2 to 3 do
        StatBlk.Ary[i]:=ReadReg(StatRegBase+i,junk)
else
    begin
    TLate(SITSeg,4,PA);
    for i:=0 to 3 do
        begin
        with PA do
            case Hi4 of
                0: StatBlk.Ary[i]:=ReadMemory(Lo16);
                1: StatBlk.Ary[i]:=ReadDisplay(Lo16);
                Otherwise: 
                    begin
                    writeln('?Fatal Memory Address: ',Hi4:1:8,'''',
                             Lo16:6:-8);
                    write('*');
                    StatBlk.Ary[i]:=0
                    end
              end { case };
        AddAdrs(PA,OneAdr,PA)
        end
    end
end { ReadStatus };

Procedure SendStatus(var StatBlk: PDCStatBlock);
{-----------------------------------------
{
{ Abstract:
{    Send Status Block to PDS
{
{ Parameters:
{    StatBlk: the status block to be sent.
{
{ Side Effects:
{    State of PDS may be effected.  Most specifically, if PDS is running,
{    it will be stopped and not restarted.  StatBlk may be altered due to
{    the current state of PDS status.
{
{ Design:
{    PDS Must be stopped before the status may be sent
{-----------------------------------------}
  var i,j:integer; Success:boolean; OldStat:PDCStatBlock; PA:PhysAdr;
begin
if Running then
    { Must stop PDS first }
    begin
    WriteWord(Stop);            { Signal PDS to Stop }
    repeat
        i:=ReadWord(Success)    { Wait for PDS to Report back }
    until Success;
    while success do
        begin
        i:=ReadWord(Success);   { In case more junk is coming }
        if Success then writeln('Got Extra ',i:1:-8)
        end;
    Running:=false;
    ReadStatus(OldStat);
    EvalStatus(OldStat)
    end;
    
with StatBlk.Status do
    begin
    { Set Loop control, etc flags }
    HFreeBits:=0; { make sure the unused portion of the high byte is always 0 }
    LFreeBits:=0; { Low byte also, although this is just for shits and grins }
    SST:=Flags[SSTMode];
    Loop:=(HadError and Flags[LFonE]) or Flags[LonT];
    Term:=SendTerminate;
    SendTerminate:=false;
    RepOnce:=SendRepOnce;
    SendRepOnce:=false;
    AdvOnce:=SendAdvOnce;
    SendAdvOnce:=false;
    if HadError then
        begin
        RepOnce:=Flags[LonE] or RepOnce;
        HadError:=Flags[LFonE]
        end
    end;
if CurTest.TstType = MicroTest then
    for i:=2 to 3 do
        WriteReg(StatRegBase+i,StatBlk.Ary[i])
else
    begin
    TLate(SITSeg,6,PA);
    for i:=2 to 3 do
        begin
        with PA do
            case Hi4 of
                0: WriteMemory(Lo16,StatBlk.Ary[i]);
                1: WriteDisplay(Lo16,StatBlk.Ary[i]);
                Otherwise:
                    begin
                    writeln('?Fatal Memory Address: ',Hi4:1:8,'''',
                             Lo16:6:-8);
                    write('*')
                    end
              end { case };
        AddAdrs(PA,OneAdr,PA)
        end
    end
end { SendStatus };

Procedure ResCommand;
{-----------------------------------------
{ Abstract:
{    Send Resume to PDS
{
{ Side Effects:
{    Clears Error Flags if Repeat on Error conditions to have no effect
{-----------------------------------------}
begin
if (CurTest.TstType=MicroTest) and BetweenSubtests then
    with CurStatus do
        with CurTest.SubList[SubTestNum] do
            begin     { start the next subtest }
            SendStatus(CurStatus);
            if CurOutput = Verbose then
                Writeln('Starting ',STName);
            StartPerq(StartAdr);
            Running:=true;
            BetweenSubtests:=false
            end
else
    begin
    SendAdvOnce:=true;
    HadError:=false;
    SendStatus(CurStatus);
    StartPerq(UniversalAddress);
    Running:=true
    end
end { ResCommand };

Procedure RepCommand;
{-----------------------------------------
{ Abstract:
{    Cause PDS to perform the next interation with the same values as the
{    last time
{-----------------------------------------}
begin
if BetweenSubtests then
    writeln('?Cannot REPEAT bewteen Subtests')
else if Flags[SSTMode] then
    begin
    SendRepOnce:=true;
    SendStatus(CurStatus);
    StartPerq(UniversalAddress);
    Running:=true
    end
else
    writeln('?REPEAT is only valid in Single Step Mode')
end { RepCommand };

Procedure TrmCommand;
{-----------------------------------------
{ Abstract:
{    Send Terminate to PDS
{-----------------------------------------}
begin
SendTerminate:=true;
SendStatus(CurStatus);
StartPerq(UniversalAddress);
Running:=true
end { ResCommand };

Procedure AbortPDS;
{-----------------------------------------
{ Abstract:
{    Stop PDS
{
{ Side Effects:
{    State of PDS may be effected
{-----------------------------------------}
  var i:integer; success:boolean;
begin
if Running then
    begin
    WriteWord(Stop);        { Signal PDS to Stop }
    Running:=false;
    WaitAbort:=true
    end
end { AbortPDS };

Procedure DoFlags;
{-----------------------------------------
{
{ Abstract:
{    Execute command to set the flags.
{
{ Side Effects:
{    Modifies global variable Flags.
{
{ Design:
{    Will prompt for values if no values given; if ? is arg, then lists current
{    values
{-----------------------------------------}
   var sym: String; Brk: string[1]; NewFlags:PDMFlags; i:integer;
       OkToSet,WasRunning: boolean;
begin
OkToSet:=false;       { false until we get a full valid set of flags }
RemDelimiters(CmdLine,Dels,Brk);
GetSymbol(CmdLine,Sym,Break1,Brk);
if Length(Sym) = 0 then        { prompt for flag values individually }
    begin
    for i:=0 to LastFlag do
        begin
        write('  ',FlgNames[i],' [');
        WriteYesOrNo(Flags[i]);
        write(']:');
        NewFlags[i]:=YesOrNo(Flags[i])
        end;
    OkToSet:=true
    end
else if Sym = '?' then         { List the current flag values }
    for i:=0 to LastFlag do
        begin
        write('  ',FlgNames[i],': ');
        WriteYesOrNo(Flags[i]);
        writeln
        end
else                           { Get the flags off the command line }
    begin
    OkToSet:=true;             { we'll set it to false if we get any errors }
    for i:=0 to LastFlag do
        begin
        if Sym[1] in ['Y','y'] then
            NewFlags[i]:=true
        else if Sym[1] in ['N','n'] then
            NewFlags[i]:=false
        else if Sym[1] in ['X','x'] then
            NewFlags[i]:=Flags[i]
        else
            begin
            writeln('?Invalid Flag List');
            OkToSet:=false;
            i:=LastFlag
            end;
        RemDelimiters(CmdLine,Dels,Brk);
        GetSymbol(CmdLine,Sym,Break1,Brk);
        if (Length(Sym) = 0) and (i <> LastFlag) then
            begin
            writeln('?Not Enough Flags specified');
            OkToSet:=false;
            i:=LastFlag
            end
        end;
    if OkToSet and (Length(Sym) <> 0) then
        begin
        writeln('?Too Many Flags Specified');
        OkToSet:=false
        end
    end;
if OkToSet then                  { We've got good flags }
    begin
    Flags:=NewFlags;
    WasRunning:=Running;
    if WasRunning then
        begin
        SendStatus(CurStatus);
        StartPerq(UniversalAddress);
        Running:=true
        end
    end
end { DoFlags };

Procedure PchCommand;
{-----------------------------------------
{ Abstract:
{    Alter the contents of a location of the microstore
{
{ Side Effects:
{    May Alter CurAdr and CurState
{-----------------------------------------}
  var BrkChr: string[1]; Sym: string; HiVal,LoVal: integer;
begin
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
if GetVal(Sym,HiVal,LoVal,12, curRadix) then
    begin
    DisplayMicro(LoVal,curRadix);
    ChangeMicro(User[LoVal]);
    WriteMicro(LoVal,User[LoVal]);
    CurState:=UCode;
    CurAdr.Lo16:=LoVal
    end
else
    writeln('PATCH <micro-address>')
end;

Function FindNextTest(oldTest: integer): integer;
{-----------------------------------------
{
{ Abstract: finds the next subtest starting from the current sub Test.  Wraps
{            around if no new ones around and sets End-Of-Pass flag. 
{
{ Parameters: oldTest is the current subtest
{
{ Results: the next subtest to run
{
{ Errors: if no subtests set, prints message, returns 0 and set End-Of-Pass flg
{-----------------------------------------}
  var i,old: integer;
begin
if OldTest = #377 then old:=-1 else old:=oldtest;
for i := old+1 to curTest.subCnt-1 do
    if curTest.exeList[i] THEN 
        begin
        FindNextTest := i;
        exit(FindNextTest);
        end;
EOPFlag:=true;
for i := 0 to curTest.subCnt-1 do {wrap around}
    if curTest.exeList[i] THEN 
        begin
        FindNextTest := i;
        exit(FindNextTest);
        end;
WriteLn('?NO SubTests to Run!!!');
FindNextTest := 0;
EOPFlag:=true
end { FindNextTest };

Procedure SttCommand;
{-----------------------------------------
{ Abstract:
{    Start up a diagnostic.  For microcode diagnostics, this starts the first
{    Subtest.  For Pascal diagnostics this starts the initialization code.
{
{ Side Effects:
{    Affects the state of PDS and Running
{-----------------------------------------}
  var BrkChr: string[1]; Sym:string; HiVal,LoVal: integer;
begin
{ Start Command }
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
if Length(Sym) = 0 then
    with CurTest do
      if TstName = '' then
        writeln('?No Test to Start')
      else
        begin
        EOPFlag:=false;
        CurStatus.SubTestNum:=FindNextTest(-1);
        if EOPFlag then
            writeln('?No Subtests to run')
        else
            begin
            SendStatus(CurStatus);
            WriteWord(ResetU);
            if TstType = PascalTest then
                StartPerq(PerqStartAdr)
            else
                StartPerq(SubList[CurStatus.SubTestNum].StartAdr);
            Running:=true
            end
        end
else
    if GetVal(Sym,HiVal,LoVal,12, curRadix) then
        begin
        EOPFlag:=false;
        CurStatus.SubTestNum:=FindNextTest(-1);
        if EOPFlag then
            writeln('?Warning: No Subtests to run; Starting Perq anyway.');
        SendStatus(CurStatus);
        WriteWord(ResetU);
        StartPerq(LoVal);
        Running:=true
        end
    else
        writeln('START [<uadr>]')
end { SttCommand };

Procedure LdMCommand;
{-----------------------------------------
{ Abstract:
{    Load a Microcode file into a fresh Microstore
{-----------------------------------------}
  var BrkChr: string[1]; Sym:string;
begin
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
if Length(Sym) = 0 then
     writeln(
         'LOAD <file> | * (for std Perq microcode)')
else
    if Sym[1] = '*' then
        begin { Load Standard PERQ microcode }
        LoadUser(false,'PERQ');
        LoadUser(true,'IO');
        LoadUser(true,'RO');
        LoadUser(true,'LINE');
        LoadUser(true,'LINK')
        end
    else
        LoadUser(false,Sym)
end { LdMCommand };

Procedure OvrCommand;
{-----------------------------------------
{ Abstract:
{    Overlay Microcode files in the microstore
{-----------------------------------------}
  var BrkChr: string[1]; Sym: string;
begin
RemDelimiters(CmdLine,Dels,BrkChr);
GetSymbol(CmdLine,Sym,Breaks,BrkChr);
if Length(Sym) = 0 then
    writeln('OVERLAY <file>')
else
    LoadUser(true,Sym)
end { OvrCommand };

Procedure SelCommand;
{-----------------------------------------
{
{ Abstract:
{    Execute Select Command.
{
{ Side Effects:
{    Modifies CurTest.ExeList.
{
{ Design:
{    Will print current Select List if no list is given
{-----------------------------------------}
  label 1;
  var Sym:string; Brk: string[1]; i,j,Low,Hi: integer; BTmp: boolean;
begin
RemDelimiters(CmdLine,Dels,Brk);
GetSymbol(CmdLine,Sym,Break1,Brk);
if Length(Sym) = 0 then
    with CurTest do
        begin
        { print the current list }
        BTmp:=false;
        for i:=0 to SubCnt-1 do
            if ExeList[i] then
                begin
                if BTmp then write(', ') else BTmp:=true;
                OutNum(i,6,curRadix);
                end;
        writeln
        end
else
    with CurTest do
        begin
        { get the new select list }
        for i:=0 to SubCnt-1 do ExeList[i]:=false;
        BTmp:=false;
        RemDelimiters(CmdLine,Dels,Brk);
        repeat
            if GetVal(Sym,i,j,16, curRadix) then
                Low:=j
            else
                begin
                1: writeln('?Invalid Select List at char ', i:1);
                for i:=0 to SubCnt-1 do ExeList[i]:=true;
                exit(SelCommand)
                end;
            if (Length(Brk) <> 0) and (Brk[1] = ':') then
                begin
                { It's a range }
                LOp(CmdLine);    { remove the : }
                RemDelimiters(CmdLine,Dels,Brk);
                GetSymbol(CmdLine,Sym,Break1,Brk);
                RemDelimiters(CmdLine,Dels,Brk);
                if GetVal(Sym,i,j,16, curRadix) then
                    Hi:=j
                else
                    goto 1
                end
            else
                Hi:=Low;
            if (Hi < Low) or (Hi >= SubCnt) then
                goto 1
            else
                for i:=Low to Hi do
                    ExeList[i]:=true;
            if (Length(Brk) <> 0) and (Brk[1] = ',') then
                begin
                LOp(CmdLine);
                RemDelimiters(CmdLine,Dels,Brk);
                GetSymbol(CmdLine,Sym,Break1,Brk);
                if length(Sym) = 0 then
                    begin
                    WriteLn(' ?Syntax error');
                    goto 1;
                    end;
                end
            else
                BTmp:=true
        until BTmp
        end
end { SelCommand };

Procedure ErrorReport(ErrNum: integer);
{-----------------------------------------
{
{ Abstract:
{    Report Error from Diagnostic
{
{ Parameters:
{    ErrNum: The error number
{
{ Errors:
{    Errors may occur if
{         1. No Diagnostic is Running
{         2. There is no entry for said error in the current test
{         3. The Error Entries in the Master File are bad
{-----------------------------------------}
  label 1,2;
  var Line,Sym: string; brk:string[1]; Hi,Lo,i:integer; NotFound:boolean;
      Mode: char; MemBase: PhysAdr; RegBase: integer; Hack: string[2];
      TmpA: PhysAdr;
begin
if (CurTest.TstType = NoTest) then
    begin
  1:writeln('?Fatal Error while attempting to report Error ');
    OutNum(ErrNum,6,curRadix);
    write('*');
    exit(ErrorReport)
    end;
reset(ErrFil,ErrFilNam);
line:='';
Write('?ERROR--');
while (length(Line) = 0) and (not eof(ErrFil)) do
    begin
    readln(ErrFil,Line);
    RemDelimiters(Line,Dels,Brk)
    end;
NotFound:=true;
while NotFound and (not EOF(ErrFil)) do
    begin
    if Line[1] in ['0'..'7'] then
        begin
        GetSymbol(Line,Sym,Breaks,Brk);     { See if this is our error }
        if GetVal(Sym,Hi,Lo,16, 8) then
            if Lo = ErrNum then
                NotFound:=false
            else
        else
            begin
         2: writeln('?Error in Error Section of Master File');
            goto 1
            end
        end;
    if NotFound then
        begin
        Line:='';
        while (Length(Line) = 0) and (not EOF(ErrFil)) do
            begin
            readln(ErrFil,Line);
            RemDelimiters(Line,Dels,Brk)
            end
        end
    end;
if NotFound then
    begin
    writeln('?Undefined Error');
    goto 1
    end;
if CurTest.TstType = MicroTest then
    begin
    RemDelimiters(Line,Dels,Brk);     { get base address of memory buffer }
    GetSymbol(Line,Sym,Breaks,Brk);
    with MemBase do
        if not GetVal(Sym,Hi4,Lo16,20, 8) then goto 2;
    RemDelimiters(Line,Dels,Brk);     { get base register # of reg buffer }
    GetSymbol(Line,Sym,Breaks,Brk);
    if not GetVal(Sym,Hi,RegBase,8, 8) then goto 2
    end
else
    begin
    TLate(SITSeg,4,MemBase);      { get base address of memory buffer }
    with MemBase do
        begin
        Lo:=GetMemory(MemBase);
        AddAdrs(MemBase,OneAdr,MemBase); 
        Hi:=GetMemory(MemBase);
        TLate(Hi,Lo,MemBase)
        end
    end;
{ Now That we've got the basics, Report the error }
Hack:='<<';                 { This is a hack since the compiler can't handle }
Hack[0]:=chr(1);            { string constants of length = 1 }
Readln(ErrFil,Line);        { get the first line of the error text }
RemDelimiters(Line,Dels,Brk);
NotFound:=not ((length(Line) > 0) and (Line[1] in ['0'..'7']));
while NotFound do
    begin
    i:=Pos(Line,Hack);
    if (i <= 0) or (i >= length(Line)) then
        begin
        write(Line);
        Line:=''
        end
    else if Line[i+1] in ['M','m','R','r'] then
        begin
        Sym := SubStr(Line,1,i-1);
        write(Sym);
        Mode:=Line[i+1];
        Line := SubStr(Line,i+2,length(Line)-i-1);
        RemDelimiters(Line,Dels,Brk);
        GetSymbol(Line,Sym,Break1,Brk);
        if GetVal(Sym,Hi,Lo,16, 8) then
            begin
            case Mode of
                'R','r': if CurTest.TstType = PascalTest then
                             begin
                             writeln;
                             writeln('?Register Values not permitted in ',
                                     'Pascal Diagnostics');
                             goto 2
                             end
                         else
                             begin
                             i:=RegBase+Lo;
                             if i > #377 then
                                  begin
                                  writeln;
                                  write('?Register ');
                                  OutNum(i, 6, curRadix);
                                  WriteLn(' out of range');
                                  goto 1
                                  end;
                              Lo:=ReadReg(i,Hi);
                              OutNum(Hi,2, curRadix);
                              write(',,');
                              OutNum(Lo,6, curRadix)
                              end;
                 'M','m': begin
                          with TmpA do
                              begin
                              Hi4:=Hi;
                              Lo16:=Lo
                              end;
                          AddAdrs(TmpA,MemBase,TmpA);
                          i:=GetMemory(TmpA);
                          OutNum(i,6,curRadix)
                          end
              end { case };
            while (Length(Line) > 0) and (Line[1] <> '>') do Lop(Line);
            if length(Line) = 0 then
                begin
                writeln;
                writeln('?">" expected');
                goto 2
                end
            else
                Lop(Line)       { remove the > }
            end
        else
            goto 2
        end
    else
        begin
        { Not an error value, just a < }
        Sym := SubStr(Line,1,i);
        write(Sym);
        Line := SubStr(Line,i+1,Length(Line)-i)
        end;
    if (Length(Line) = 0) and eof(ErrFil) then
        begin
        writeln;
        NotFound:=false
        end;
    while (Length(Line) = 0) and (not eof(ErrFil)) and NotFound do
        begin
        readln(ErrFil,Line);
        writeln;
        RemDelimiters(Line,Dels,Brk);
        if length(Line) > 0 then
            begin
            if (Line[1] in ['0'..'7']) or (eof(ErrFil)) then
                NotFound:=false
            end
        else if eof(ErrFil) then
            NotFound:=false
        end
    end;
write('*');
close(ErrFil)
end { ErrorReport };

Procedure EvalStatus(* var StatBlk: PDCStatBlock *);
{-----------------------------------------
{
{ Abstract:
{    Handles data from PDS.  Evaluates status and acts upon whatever!
{
{ Parameters:
{    StatBlk - PDS Status Block
{    ReSend - True = OK to send new changed status
{
{ Side Effects:
{    may change state of PDS Perq; may report PDS errors;
{    may change StatBlk
{    may change CurTest; may change Running
{-----------------------------------------}
  var flg:boolean;
begin
with StatBlk do
  with Status do
    begin
    if EndOfST then
        begin      { End of the current subtest has been reached }
        EndOfST:=false;
        BetweenSubtests:=true;
        Flg:=false;
        if (CurOutput = Verbose) or Flags[HatEOST] then
          if SubTestNum <> #377 then
            begin
            flg:=true;
            writeln('Subtest ',
            CurTest.SubList[SubTestNum].STName,' complete')
            end;
        SubTestNum:=FindNextTest(SubTestNum);
        { On to the next subtest }
        if EOPFlag then
            begin
            if CurOutput in [Verbose,Brief] then
                { Tell User about the end of the pass }
                begin
                flg:=true;
                writeln('End of Pass ',PassNum:1,chr(13),
                        ErrCnt:1,' Errors Detected')
                end;
            PassNum:=PassNum+1
            end;
        if Flags[HatEOST] or (Flags[HatEOP] and EOPFlag) then
            begin
            flg:=true;
            Writeln('Type RESUME to continue')
            end
        else
            with CurTest.SubList[SubTestNum] do
                begin     { start the next subtest }
                SendStatus(StatBlk);
                if CurOutput = Verbose then
                    begin
                    flg:=true;
                    Writeln('Starting ',STName)
                    end;
                if CurTest.TstType = MicroTest then
                    StartPerq(StartAdr)
                else
                    StartPerq(UniversalAddress);
                Running:=true;
                BetweenSubtests:=false
                end;
        EOPFlag:=false;
        if flg then write('*')
        end;
    if SSTBPT then
        begin
        SSTBPT:=false;
        writeln('<BreakPoint>',chr(13),'*')
        end;
    if Error then
        begin
        Error:=false;
        HadError:=true;
        if Flags[SonE] then write(chr(7));
        if CurOutput <> Silent then
            ErrorReport(ErrorNum);
        ErrCnt:=ErrCnt+1;
        if Flags[HonE] then
            writeln('Type RESUME to Continue',chr(13),'*')
        else
            begin
            SendStatus(StatBlk);
            StartPerq(UniversalAddress);
            Running:=true
            end
        end;
    if BadData then
        begin
        BadData:=false;
        writeln('?PDS received bad status',chr(13),'*');
        end;
    if Aborted then
       begin
       Aborted:=false;
       if WaitAbort then
           begin
           WaitAbort:=false;
           writeln('PDS Ready for commands',chr(13),'*')
           end
       end
    end
end { EvalStatus };

Procedure HandleBPT;
{-----------------------------------------
{
{ Abstract:
{    Checks for data from PDS Perq and handles the data if exists
{
{ Parameters: none
{
{ Results: none
{
{ Side Effects:
{    may change state of PDS Perq; may report PDS errors;
{    may change CurTest; may change Running
{
{ Errors: 
{
{ Design:
{-----------------------------------------}
  var i:integer;
      success: boolean;
begin
i:=ReadWord(Success);          { see if there's a word from PDS Perq }
if Success then                { Yes Burtha, we've got one }
    begin
    while Success do
        begin
        i:=ReadWord(Success);   { In case more junk is coming }
        if Success then writeln('Got Extra ',i:1:-8)
        end;
    Running:=false;        { PDS can't be running if we got a BPT }
    if CurTest.TstType = NoTest then
        writeln('?PDS Perq Ready for Commands',chr(13),'*')
    else
        begin
        ReadStatus(CurStatus);
        EvalStatus(CurStatus)
        end
    end
end { HandleBPT };

Procedure Commander;
{-----------------------------------------
{ Abstract:
{    Main command processing loop
{
{ Side Effects:
{    May affect almost any globals somewhere along the line
{-----------------------------------------}
  var BrkChr: string[1]; Sym:string; HiVal,LoVal: integer; Ix: CmdType;
begin
haveCmdLine := false;
RemDelimiters(CmdLine,Dels,BrkChr);
if Length(CmdLine) <> 0 then
    if CmdLine[1] = chr(#12) then
        if running then WriteLn('  Illegal during execution')
        else
        { Got a lonely line-feed - perform an examine auto-incr }
        case CurState of
          Register:
            begin
            AddAdrs(CurAdr,OneAdr,CurAdr);
            if CurAdr.Lo16 = #400 then CurAdr.Lo16 := 0;
            PrintReg(CurAdr.Lo16)
            end;
          Memory:
            begin
            AddAdrs(CurAdr,OneAdr,CurAdr);
            if CurAdr.Lo16 = 0 then
                if CurAdr.Hi4 > 1 then CurAdr.Hi4 := 0;
            PrintMem(CurAdr)
            end;
          UCode:
            begin
            AddAdrs(CurAdr,OneAdr,CurAdr);;
            if CurAdr.Lo16 > #7777 then CurAdr.Lo16:=0;
            DisplayMicro(CurAdr.Lo16, curRadix)
            end
          end { case }
    else if CmdLine[1] = '@' then
        if running then WriteLn('  Illegal during execution')
        else
       begin
        { specifying a command file - get it }
        if HavePDMFile then
            writeln('?Nested Command Files are Illegal')
        else
            begin
            Lop(CmdLine);
            GetSymbol(CmdLine,Sym,Breaks,BrkChr);
            if Existant(Sym) then
                begin
                reset(PDMFile,Sym);
                HavePDMFile:=true
                end
            else
                writeln('?Command File ',Sym,' not found')
            end;
        end
    else if CmdLine[1] = '?' then {list legal commands}
        begin
        Writeln(chr(13),'Valid commands are:');
        for Ix := FirstCmd to LastCmd do
             begin
             if (ord(Ix)-1) mod 6 = 0 then writeln;
             Write(cmds[ord(Ix)],'':12-Length(cmds[ord(Ix)]))
             end;
        WriteLn;
        end
    else if CmdLine[1] = '=' then
        begin
     (*&&*)      WriteLn(' Not Yet Implemented');
        { Got an '=' - perform a Deposit Auto-Incr }
        end
    else if CmdLine[1] in ['0'..'9'] then
      begin
     (*&&*)    WriteLn(' Not Yet Implemented');
      { Line starts with a # - should be an examine or deposit same }
      end
    else if CmdLine[1] in ['A'..'Z'] then
        begin
        { it's a keyword driven command }
        GetSymbol(CmdLine,Sym,Breaks,BrkChr);    { get the Keyword }
        Ix:=recast(UniqueCmdIndex(Sym,Cmds,ord(LastCmd)),CmdType);
        IF running AND (ix IN [BootCmd,MemCmd,RegCmd,PatCmd,ZeroCmd,LoadCmd,
                              OverCmd,MasCmd,GetCmd,SttCmd,RepCmd,MicCmd]) then
           WriteLn('Command not allowed during test execution')
        else
        case Ix of
            NoCmd: begin
                   writeln('?No such command as ',Sym);
                   writeln('Type ? for list of commands')
                   end;
            NotUniq: begin
                     writeln('?',Sym,' is not unique');
                     writeln('Type HELP if you need help')
                     end;
            BootCmd: begin
                     { Boot Command }
                     IsOpen:=false;     { No location is currently open }
                     SendBoot           { Send the Boot to PERQ }
                     end;
            MemCmd: MemCommand;        { Memory Command }
            RegCmd: RegCommand;        { Register Command }
            PatCmd: PchCommand;
            ZeroCmd:WriteWord(MClear);
            LoadCmd:LdMCommand;
            OverCmd:OvrCommand;
            MasCmd:  MasCommand;            { Master Command }
            GetCmd:  GetCommand;
            SttCmd: SttCommand;
            AboCmd: AbortPDS;
            SelCmd: SelCommand;
            FlagCmd: DoFlags;
            ResCmd: ResCommand;
            RepCmd: RepCommand;
            RunCmd: CheckRunning;
            ListCmd: LstCommand;
            VerCmd: CurOutput := Verbose;
            BrfCmd: CurOutput := Brief;
            SilCmd: CurOutput := Silent;
            HelpCmd: HlpCommand;
            QuitCmd: exit(PDM);
            MicCmd: MicCommand;
            DebCmd: begin
                PDMDebug := not PDMDebug;
                remdelimiters(CmdLine,Dels,BrkChr);
                GetSymbol(CmdLine,Sym,Breaks,BrkChr);
                if not getval(sym,hival,loval,32, curRadix) then
                    {nothing}
                else
                    begin
                    Write('  ',hiVal:1,'|',loVal:1, '   radix: ');
                    readLn(hiVal);
                    Write('width: ');
                    readLn(i);
                    OutNum(loVal,i,hiVal);
                    WriteLn;
                    end;
                end;
            RadCmd: DoRadix;
            TermCmd: TrmCommand;
(*&&*)      Otherwise: writeln('Command Not Yet Implemented!')
            end { case }
        end {first char is letter}
    else
        writeln('?Bad Command');
NewCmdLine
end { Commander };

begin { PDM }
Reset(Input, 'CONSOLE:');
Rewrite(Output, 'CONSOLE:');

writeln('Perq Diagnostic Monitor  Version ',PDMVersion);

Initialize;

while true do           {here we've got the main loop}
    begin
    HandleBPT;
    if Running then
        CheckForInput   {only from keyboard; not command file}
    else
        NeedInput;      {should exit with HaveCmdLine true}
    if HaveCmdLine then {handle this command}
        Commander
    end {loop}           
end { main }.
