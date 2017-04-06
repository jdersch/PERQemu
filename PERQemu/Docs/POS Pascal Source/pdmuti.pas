{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{-----------------------------------------
{
{ Abstract:
{
{   copyright 1983  Three Rivers Computer Corporation
{
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
Module PDMUtils;

Exports

imports ControlStore from ControlStore;

const
    MaxUCode = #7777;       { maximum microcode address }
    MaxUser = #6777;        { max user microcode address (less Krnl and SysB) }
    TotBrks = 11;           { total break points (some extra for PDM) }
    BootCode =  #12345;     { Boot Start/Ack }
    BreakCode = #54321;     { breakpoint message }
    InterruptRegister = #360;

    { Link Protocal Codes }
    
    LdRegAdr =  #1634;          { Load Register Address }
    WReg =      #1635;          { Write Register Data }
    RReg =      #1636;          { Read Register Data }
    
    LdMemAdr =  #1637;          { Load Low Memory Address (bottom 64K words) }
    LdDisAdr =  #1643;          { Load High Memory Address (top 64K words) }
    WMem =      #1640;          { Write Memory Data }
    RMem =      #1641;          { Read Memory Data }
    WBlock =    #1645;          { Write Memory 256 word block }
    MClear =    #1644;          { Memory Clear }
    
    LdUAdr =    #1630;          { Load Micro Address }
    WU1 =       #1631;          { Write Micro Word 1 }
    WU2 =       #1632;          { Write Micro Word 2 }
    WU3 =       #1633;          { Write Micro Word 3 }
    LdUData =   #1646;          { Load 2K MicroCode }
    UStart =    #1642;          { Start MicroCode }
    ResetU =    #1647;          { Reset Micro State }

        
var
    NoDoneBit: boolean;     { if last communication failed }
    PDMDebug: boolean;     { if true, lots of info typed out }
    User:array[0..MaxUCode] of MicroInstruction;
    BreakAddress:array[0..TotBrks] of integer;
    Disabled: boolean;      { if interrupts should be disabled }

procedure OutNum(val,wid, radix:integer);
procedure WriteWord(x:integer);
function ReadWord(var success: BOOLEAN):integer;
procedure InitLink;
procedure DefaultExtension(var s:string; e:string);
procedure Translate(var UWord: MicroInstruction; var TLate: TransMicro);
procedure WriteReg(reg,data:integer);
function ReadReg(reg:integer; var Xtra4: integer):integer;
procedure WriteMemory(adr,data:integer);
function ReadMemory(Adr:integer):integer;
procedure WriteDisplay(adr,data:integer);
function ReadDisplay(adr:integer):integer;
procedure WriteMicro(Addr: integer; UWord:MicroInstruction);
procedure StartPERQ(adr:integer);
procedure SetBreak(adr,brk:integer);
procedure KillBreak(brk:integer);
function Existant(Name:string): Boolean;
procedure SendBoot;
procedure SendUser;
procedure LoadUser(OverLay: boolean; FilNam: string);
procedure DisplayMicro(adr, radix:integer);
procedure FindSuccessors(adr:integer; var s1,s2:integer);

Private

imports FileSystem from FileSystem;
imports Perq_String from Perq_String;
   
type
    Swab = packed record case boolean of
            true: (int:integer);
            false:(byt0:0..255;
                    byt1:0..255)
            end;

function MIN(i1, i2: integer): integer;
  begin
  if i1 < i2 then MIN := i1
  else MIN := i2;
  end;

procedure OutNum(* val,wid, radix:integer *);
{ print Val in radix;
  wid is the max number of octal digits allowed (if decimal, no extra digits
    printed }
   var i, j:integer;
   begin
   case radix of
      2: for i := MIN(wid*3-1, 15) downto 0 do
            Write(LAnd(Shift(val,-i), 1):1);
      8: for i := MIN(wid-1, 5) downto 0 do
            Write(LAnd(Shift(val,-3*i), #7):1);
      10: Write(val:1);
      16: for i := MIN((((wid+1)*3) div 4) -1, 3) downto 0 do
            Begin
            j := LAnd(Shift(val, -4*i), #17);
            if j <= 9 then write(j:1) else write(chr(j+(ord('A')-10)));
            end;
      otherwise: WriteLn('BAD Radix: ',radix:1,', val is: ',val:1);
      end;
  end; {OutNum}

procedure WriteWord(* x:integer *);
  var Success: boolean;
begin
 LoadExpr(X);
 LoadExpr(LOr(Shift(#5010,8),Shift(#5010,-8)));
 InLineByte( #277 {JCS} );
 StorExpr(Success);
 NoDoneBit := not Success;
 if NoDoneBit and PDMDebug then Writeln('No done bit on transmit.')
end { WriteWord };

function ReadWord(* var success: boolean):integer *);
  var f: boolean;
  X: integer;
begin
 LoadExpr(LOr(Shift(#5020,8),Shift(#5020,-8)));
 InLineByte( #277 {JCS} );
 StorExpr(f);
 success := f;
 if Success then
  begin StorExpr(X); ReadWord := X end
 else ReadWord := -1;
 NoDoneBit := not Success;
end { ReadWord };

procedure InitLink;
{ initialize vars to handle physical link IO }
label 1;
var LinkBin: MicroFile;
    Success: boolean;
begin
 Reset(LinkBin,'Link.Bin');
1: LoadControlStore(LinkBin);
 LoadExpr(LOr(Shift(#5000,8),Shift(#5000,-8)));
 InLineByte( #277 {JCS} );
 StorExpr(Success);
 if not Success then
  begin Writeln('?Unable to initialize the link');
   goto 1
  end
end { InitLink };

procedure DefaultExtension(* var s:string; e:string *);
{ append default extension e to filename s if there is no extension }
{ also converts filename to all upper case }
  var i:integer;
begin
for i:=1 to length(s) do
    if s[i] in ['a'..'z'] then
        s[i]:=chr(ord(s[i])-32);
if pos(s,e) = 0 then s := concat(s,e)
end { DefaultExtension };

procedure Translate(* var UWord: MicroInstruction; var TLate: TransMicro *);
{ Translate UWord into TLate }
begin
with TLate do
    begin
    ALU23:=UWord.ALU23;
    ALU0:=UWord.ALU0;
    W:=UWord.W;
    ALU1:=UWord.ALU1;
    A:=UWord.A;
    Z:=UWord.Z;
    SFF:=UWord.SFF;
    H:=UWord.H;
    B:=UWord.B;
    JmpCnd:=UWord.JmpCnd;
    Word3 := UWord.Word3
    end
end { Translate };

procedure WriteReg(* reg,data:integer *);
{ write data to XY register reg }
  var trik: swab;
begin
WriteWord(LdRegAdr);
with trik do
        begin
        byt0:=reg;
        byt1:=reg;
        WriteWord(int)
        end;
WriteWord(WReg);
WriteWord(data)
end { WriteReg };

function ReadReg(* reg:integer; var Xtra4: integer):integer *);
{ read contents of XY register REG }
  var success: BOOLEAN;
      trik:swab;
      trikky: packed record case boolean of
                true: (I: Integer);
                false: (grabage: 0..#7777;
                        top4: 0..15);
              end;
  
begin
WriteWord(LdRegAdr);
with trik do
        begin
        byt0:=reg;
        byt1:=reg;
        WriteWord(int)
        end;
WriteWord(RReg);
ReadReg:=ReadWord(success);
IF not success and PDMDebug then WriteLn('No done bit on receive');
WriteWord(0);
trikky.I:=ReadWord(success);
IF not success and PDMDebug then WriteLn('No done bit on receive');
Xtra4:=15 - trikky.top4
end { ReadReg };

procedure WriteMemory(* adr,data:integer *);
{ write data to memory location adr - addresses first 128K of memory only }
begin
WriteWord(LdMemAdr);
WriteWord(adr);
WriteWord(WMem);
WriteWord(data)
end { WriteMemory };

function ReadMemory(* Adr:integer):integer *);
{ read memory location adr - addresses first 128K of memory only }
var success: boolean;
begin
WriteWord(LdMemAdr);
WriteWord(adr);
WriteWord(RMem);
ReadMemory:=ReadWord(success);
IF not success and PDMDebug then WriteLn('No done bit on receive');
end { ReadMemory };

procedure WriteDisplay(* adr,data:integer *);
{ write data to memory location #200000 + adr (2nd 128K only) }
begin
WriteWord(LdDisAdr);
WriteWord(adr);
WriteWord(WMem);
WriteWord(data)
end { WriteDisplay };

function ReadDisplay(* adr:integer):integer *);
{ read memory location #200000 + adr (2nd 128K only) }
var success: boolean;
begin
WriteWord(LdDisAdr);
WriteWord(adr);
WriteWord(RMem);
ReadDisplay:=ReadWord(success);
IF not success and PDMDebug then WriteLn('No done bit on receive');
end { ReadDisplay };

procedure WriteMicro(* Addr: integer; UWord:MicroInstruction *);
{ write out the micro-word uword }
  var TMicro: TransMicro; trik:swab;
begin
WriteWord(LdUAdr);
with trik do
        begin
        int:=Addr;
        Addr:=byt0;
        byt0:=byt1;
        byt1:=Addr;
        WriteWord(int)
        end;
Translate(UWord,TMicro);
WriteWord(WU1);
WriteWord(LNot(TMicro.Word1));
WriteWord(WU2);
WriteWord(LNot(TMicro.Word2));
WriteWord(WU3);
WriteWord(LNot(TMicro.Word3));
end { WriteMicro };

procedure StartPERQ(* adr:integer *);
{ Start PERQ going at uword address adr }
  var trik:swab; byt:0..255;
begin
WriteReg(#370,#377); { clear breakpoint number }
WriteWord(LdUAdr);
adr:=LAnd(adr,#7777);
with trik do
        begin
        int:=adr;
        byt:=byt1;
        byt1:=byt0;
        byt0:=byt;
        WriteWord(int)
        end;
WriteWord(UStart)                { here we go }
end { StartPERQ };

procedure SetBreak(* adr,brk:integer *);
{ the break instruction is R370:=n; Goto(7401) ::= }
{ X=370, Y=n, B=1, W=1, ALU=1, F=3, SF=0, Z=376, CND=0, JMP=3 }
  var uword:MicroInstruction;
begin
with uword do
        begin
        X:=#370;
        Y:=brk;
        A:=0;
        B:=1;
        W:=1;
        ALU:=1;
        F:=3;
        SF:=0;
        Z:=#376;
        Cnd:=0;
        Jmp:=3
        end;
WriteMicro(adr,uword)
end { SetBreak };

procedure KillBreak(* brk:integer *);
{ remove breakpoint brk }
begin
if BreakAddress[Brk] <> -1 then
    WriteMicro(BreakAddress[Brk],User[BreakAddress[Brk]])
end { KillBreak };

function Existant(* Name:string *);
var Ignore: integer;
begin { Existant }
 Existant := FSLookUp(Name,Ignore,Ignore) <> 0
end { Existant };

procedure SendBoot;
{ send bootstrap microcode to PERQ }
  var success: boolean;
      i:integer;
      Boot: array[0..255] of MicroInstruction;
      trik: Swab;
      tmp: integer;

    procedure LoadBoot( FileName: string );
    { Load bootstrap into Boot array from microcode file }
      var f:MicroFile; i:integer; adr:integer; TMicro: TransMicro;
    begin
    if not Existant(FileName) then
        begin
        Writeln('File not found: ',FileName);
        Exit(SendBoot)
        end;
    reset(f, FileName);
    for i:=0 to 255 do
            begin
            Boot[i].Word1:=-1;
            Boot[i].Word2:=-1;
            Boot[i].Word3:=-1
            end;
    while f^.Adrs >= 0 do
            begin
            adr:=LAnd(f^.adrs,#377);
            Translate(f^.MI,TMicro);
            boot[adr].word1:=ord(LNot(TMicro.Word1));
            boot[adr].word2:=ord(LNot(TMicro.Word2));
            boot[adr].word3:=ord(LNot(TMicro.Word3));
            get(f)
            end;
    close(f)
    end { LoadBoot };

begin
LoadBoot('KRNL.BIN');
WriteWord(BootCode);
for i:=255 downto 0 do
        begin
        WriteWord(Boot[i].word1);
        WriteWord(Boot[i].word2);
        WriteWord(Boot[i].word3)
        end;
tmp := ReadWord(success);
IF not success and PDMDebug then WriteLn('No done bit on receive');
if not NoDoneBit and (tmp = BootCode) then
        begin
        write('Bootstrap successful, interrupts ');
        WriteReg(InterruptRegister,Ord(Disabled));
        if Disabled then
            write('dis')
        else
            begin
            write('en')
            end;
        writeln('abled');
        LoadBoot('SYSB.BIN');
        Writeln('Loading SYSB.BIN');
        for i := 0 to 255 do
            begin WriteWord(LdUAdr);
            with trik do
                begin
                int:=#7000+i;
                tmp:=byt0;
                byt0:=byt1;
                byt1:=tmp;
                WriteWord(int)
                end;
            WriteWord(WU1);
            WriteWord(Boot[i].Word1);
            WriteWord(WU2);
            WriteWord(Boot[i].Word2);
            WriteWord(WU3);
            WriteWord(Boot[i].Word3)
            end
        end
else
        begin
        write('Bootstrap unsuccessful');
        if not NoDoneBit then
            begin
            write(', reply = ');
            OutNum(tmp,1,8)
            end;
        writeln
        end;
end { SendBoot };

procedure SendUser;
{ load user microcode }
  var i:integer; TMicro: TransMicro;
begin
for i := MaxUser downto 2048 do
 with User[i] do
  if (Word1 <> -1) or (Word2 <> -1) or (Word3 <> -1) then
   WriteMicro(i,User[i]);
WriteWord(LdUData);
for i:=2047 downto 0 do
    begin
    Translate(User[i],TMicro);
    WriteWord(LNot(TMicro.Word1));
    WriteWord(LNot(TMicro.Word2));
    WriteWord(LNot(TMicro.Word3))
    end
end { SendUser };

procedure LoadUser(* OverLay: boolean; FilNam: string *);
{-----------------------------------------
{
{ Abstract:
{    Load User Microcode into the PERQ
{
{ Parameters:
{    OverLay : true if ucode is to be added to current ucode
{    FilNam : the name of the microcode binary
{
{ Side Effects:
{    The contents of the ucode binary are put into USER
{
{ Errors:
{    Will detect 'File Not Found'
{    Checks for multiply-used locations
{-----------------------------------------}
{ loads user microcode files }
  var f:file of microbinary; add:integer;
begin
DefaultExtension(FilNam,'.BIN');
if not Existant(FilNam) then
    begin
    Writeln('?File not found: ',FilNam);
    Exit(LoadUser)
    end;
reset(f,FilNam);
if not OverLay then
    for add:=0 to MaxUser do
        with User[add] do
            begin
            word1:=-1;
            word2:=-1;
            word3:=-1
            end;
while f^.adrs >= 0 do
        begin
        add := f^.adrs;
        if OverLay then
            with User[add] do
                if (Word1 <> -1) or (Word2 <> -1) or (Word3 <> -1) then
                    begin
                    write('?Overlay Error - Location ');
                    OutNum(add,6, 8);
                    writeln(' already in use');
                    exit(LoadUser)
                    end;
        User[add]:=f^.MI;
        Get(f)
        end;
close(f);
SendUser;
writeln(FilNam,' Loaded')
end { LoadUser };

procedure DisplayMicro(* adr, radix:integer *);
{ write out the contents of microcode address ADR }
begin
write('U');
OutNum(adr,4, radix);
write('/  ');
with User[adr] do
        begin
        write('X='); OutNum(x,3, radix);
        write(' Y='); OutNum(y,3, radix);
        write(' A='); OutNum(a,1, radix);
        write(' B='); OutNum(b,1, radix);
        write(' W='); OutNum(w,1, radix);
        write(' H='); OutNum(h,1, radix);
        write(' ALU='); OutNum(ALU,2, radix);
        write(' F='); OutNum(f,1, radix);
        write(' SF='); OutNum(sf,2, radix);
        write(' Z='); OutNum(z,3, radix);
        write(' CND='); OutNum(cnd,2, radix);
        write(' JMP='); OutNum(jmp,2, radix)
        end;
writeln
end { DisplayMicro };

procedure FindSuccessors(* adr:integer; var s1,s2:integer *);
begin
with User[adr] do
        if f = 3 then
                s1:=(15-sf)*256+(255-z)
        else
                s1:=LAnd(adr,#7400) + (255-z);
s2:=adr+1
end { FindSuccessors }.
