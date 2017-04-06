{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{-----------------------------------------
{
{ Abstract:
{
{  copyright 1980, 1981, 1982, 1983  Three Rivers Computer Corporation
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
{Change history:
            2  Apr-81  DCF  V0.4   Updated references to Perq_String & Screen
            12-Nov-80  MAB  V0.3   Bug Fixes for MicroTests
            30-Oct-80  MAB  V0.2   Revamped PDM <--> PDS protocols
            20-Oct-80  BAM  V0.1   Broken off from PDM.
}

module PDM2;


EXPORTS

   imports Perq_String from Perq_String;
   imports Screen from Screen;
   imports CmdParse from CmdParse;
   imports PDMUtils from PDMUtils;
   imports PDMLoad from PDMLoad;
   imports PDCommon from PDCommon;
   

const
    PDMVersion = 'V0.3';
            
    ErrFilNam = 'PDM.ERRS'; { name for error message template temp file }
    LinkAd = #167770;
    BootUCodeOffset = #400; { Where ucode image goes in memory for disk boot }
    LdBootCmd = '>';        { Load microcode into memory command }
    MaxSubTests = 30;       { Maximum number of subtests in a test }
    
    PerqStartAdr = #2400;   { Start Address for Std Perq User MicroCode }
    
    { Flag indices }
    LonT = 0;               { Loop On Test }
    LonE = 1;               { Loop On Error }
    LFonE = 2;              { Loop Forever on Error }
    HonE = 3;               { Halt On Error }
    SonE = 4;               { Speak on Error }
    HatEOP = 5;             { Halt at End of Pass }
    HatEOST = 6;            { Halt at End of Subtest }
    SSTMode = 7;            { Single Step Mode }
    LastFlag = SSTMode;     { the last flag defined }

    
type
  CmdType = 
    { Keyword Commands - Note: This list should be maintained alphabetically }
    (DummyCmd,               { This must be first for CmdParse }
    AboCmd,
    BootCmd,
    BrfCmd,
    FlagCmd,
    GetCmd,
    HelpCmd,
    ListCmd,
    LoadCmd,
    MasCmd,
    MemCmd,
    MicCmd,
    OverCmd,
    PatCmd,
    QuitCmd,
    RadCmd,
    RegCmd,
    RepCmd,
    ResCmd,
    RunCmd,
    SelCmd,
    SilCmd,
    SttCmd,
    TermCmd,
    VerCmd,
    ZeroCmd,
    DebCmd,
    NoCmd,                       { IMPORTANT: These last two entries must }
    NotUniq);                    { be here, and in this order }

Const
    FirstCmd = AboCmd;
    LastCmd = DebCmd;
    
Type
    StateType = (memory,register,ucode);
    TestType = (MicroTest,PascalTest,NoTest);
    OutputQuantity = (Silent, Brief, Verbose);
    
    SubTest = record
                StartAdr: integer;
                STName: string
              end;
    TestRec = record
                TstName: string;
                TstType: TestType;
                SubCnt: integer;     { number of subtests }
                SubList: array[0..MaxSubTests-1] of SubTest;
                                     { list of the subtest names and SA's }
                ExeList: packed array [0..MaxSubTests-1] of boolean
              end;
    
    PhysAdr = record                 { Perq Physical Address record }
                Hi4,Lo16: integer
              end;
    
    PDMFlags = packed array [0..LastFlag] of boolean;
                                     { Array to keep track of the user flags }

    Triple = 0..2;

    Swab = packed record case boolean of
            true: (int:integer);
            false:(byt0: Byte;
                    byt1: Byte)
            end;

                 
var
    CurOutput : OutputQuantity; { how much text to print }

    Flags: PDMFlags;     { User flags }
    FlgNames: array[0..LastFlag] of string[30];{ flag names: used for prompts }

    CurState:statetype;  { current state }
    CurAdr: PhysAdr;     { current address }

    i,j:integer;
    TTYChr: char;
    
    IsOpen,              { location is currently open }
    BetweenSubtests,     { Currently between subtest executions }
    SendAdvOnce,         { True when AdvOnce must be sent }
    SendRepOnce,         {  "     "  RepOnce   "   "   "  }
    SendTerminate,       {  "     "  Terminate "   "   "  }
    WaitAbort: boolean;  {  "     "  Abort has been sent, but not yet ack'd }
    RunFileName: string;
    Cmds: CmdArray;      { Command List }

                         { Vars for command processing }
    CmdLine,             { Current Command Line }
    OldCmdLine:string;   { Last Command Line }
    Breaks,              { break characters for the command parser }
    Break1,              { more break characters for the command parser }
    Dels: string[10];    { delimiters for the command parser }
    HaveCmdLine,         { True indicates the presense of a command line }
    HavePDMFile: boolean;{ True when processing a command file }

    CurStatus: PDCStatBlock;
                         { The Current Status of PDS }
    HadError,            { Set to true when an error has been detected,
                           and not cleared until we care to forget that
                           there has been an error
                         }
    EOPFlag,             { Set True When End-Of-Pass is detected }
    
    Running:boolean;     { True when the PERQ is running a test }
    PDMFile,             { The Command File }
    MastFile,            { The Master File }
    ErrFil: Text;        { Error Template File }
    MastName: string;    { The Master File filename }
    CurTest: TestRec;    { The Current Test Name }
    OneAdr: PhysAdr;     { Double Precision value of 1 }
    PassNum,             { Pass Counter }
    ErrCnt,              { Error Counter }
    CurRadix: integer;   {current radix to input and output numbers in}

procedure CheckRunning;
Procedure NewCmdLine;
Procedure AddAdrs(Adr1,Adr2: PhysAdr; var ResAdr: PhysAdr);
function GetMemory(Adr: PhysAdr): integer;
Procedure TLate(Seg,Ofst: integer; var PA: PhysAdr);
Procedure CheckForInput;
Procedure NeedInput;
procedure LOp(var S:string);
function GetVal(Str: String; var Hi,Lo: integer; Bits: integer; radix: integer):boolean;
function ChangeField(fld:string; def,max:integer):integer;
procedure ChangeMicro(var uword:MicroInstruction);
Procedure MasCommand;
Function FindTest(TName:string): TestType;
Procedure LstCommand;
Procedure GetCommand;
procedure Initialize;
Procedure HlpCommand;
Function ParseAdr(var S:string; Var Res: PhysAdr): boolean;
Procedure PrintMem(Adr: PhysAdr);
Procedure MemCommand;

{>>>>>>>>>>>>>>>>>>>>>>>>>} PRIVATE {<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<}

imports PDM from PDM; {USING ExitPDM}
   imports IO from IO;
   imports IOErrors from IOErrors;

procedure CheckRunning;
{-----------------------------------------
{ Abstract:
{    Report Current Test and whether it's running or not
{-----------------------------------------}
begin
if Running then
    WriteLn('Test ',CurTest.TstName,' is running Subtest ',
            CurTest.SubList[CurStatus.SubTestNum].STName)
else if CurTest.TstName <> '' then
    Writeln('Test ',CurTest.TstName,' is loaded but not running')
else
    WriteLn('No test running')
end;

Procedure NewCmdLine;
{-----------------------------------------
{
{ Abstract:
{    This procedure is called to empty the current command line
{
{ Side Effects:
{    CmdLine is set to the empty string
{    HaveCmdLine is set to false
{    Displays Prompt
{-----------------------------------------}
begin
CmdLine:='';
HaveCmdLine:=false;
Write('*');
end { NewCmdLine };

Procedure CmdInit;
{-----------------------------------------
{
{ Abstract:
{    This procedure is used to initialize command parser variables
{
{-----------------------------------------}
begin
Cmds[ord(BootCmd)]:='BOOT';
Cmds[ord(MemCmd)]:='MEMORY';
Cmds[ord(RegCmd)]:='REGISTER';
Cmds[ord(PatCmd)]:='PATCH';
Cmds[ord(ZeroCmd)]:='ZERO';
Cmds[ord(LoadCmd)]:='LOAD';
Cmds[ord(OverCmd)]:='OVERLAY';
Cmds[ord(MasCmd)]:='MASTER';
Cmds[ord(GetCmd)]:='GET';
Cmds[ord(SttCmd)]:='START';
Cmds[ord(AboCmd)]:='ABORT';
Cmds[ord(SelCmd)]:='SELECT';
Cmds[ord(FlagCmd)]:='FLAGS';
Cmds[ord(ResCmd)]:='RESUME';
Cmds[ord(RepCmd)]:='REPEAT';
Cmds[ord(RunCmd)]:='RUNNING';
Cmds[ord(ListCmd)]:='LIST';
Cmds[ord(VerCmd)]:='VERBOSE';
Cmds[ord(BrfCmd)]:='BRIEF';
Cmds[ord(SilCmd)]:='SILENT';
Cmds[ord(HelpCmd)]:='HELP';
Cmds[ord(QuitCmd)]:='QUIT';
Cmds[ord(MicCmd)]:='MICRO';
Cmds[ord(DebCmd)]:='DEBUG';
Cmds[ord(RadCmd)]:='RADIX';
Cmds[ord(TermCmd)]:='TERMINATE';
Dels := '  '; Dels[2]:=chr(9);  { a tab }
Breaks:=':=-';
Break1:=',:=->';
Breaks := Concat(Breaks,Dels);
Break1 := Concat(Break1,Dels);
NewCmdLine
end { CmdInit };

Procedure AddAdrs(* Adr1,Adr2: PhysAdr; var ResAdr: PhysAdr *);
{-----------------------------------------
{
{ Abstract:
{    Adds two physical addresses (double precision add)
{
{ Parameters:
{    Adr1, Adr2: the addresses to be added
{    ResAdr: where to put the result
{
{ Side Effects:
{    ResAdr is modified
{-----------------------------------------}
  var tmp: integer;
begin
tmp:=LAnd(Adr1.Lo16,#77777) + LAnd(Adr2.Lo16,#77777);
with ResAdr do
    begin
    Lo16:=Adr1.Lo16 + Adr2.Lo16;
    Hi4:=Adr1.Hi4 + Adr2.Hi4;
    if tmp < 0 then Hi4 := Hi4 + 1
    end
end { AddAdrs };

function GetMemory(* Adr: PhysAdr): integer  *);
{-----------------------------------------
{
{ Abstract:
{    Read from Perq's memory, given a 20 bit address
{
{ Parameters:
{    Adr: The address to be read
{
{ Results:
{    Returns the contents of the memory location
{
{ Errors:
{    Fatal error occurs if the address is invalid
{-----------------------------------------}
begin
with Adr do
    case Hi4 of
        0: GetMemory:=ReadMemory(Lo16);
        1: GetMemory:=ReadDisplay(Lo16);
        Otherwise:
            begin
            writeln('?Fatal Memory Address: ',Hi4:1:8,'''',Lo16:6:-8);
            GetMemory:=0
            end
      end { case }
end { GetMemory };

Procedure TLate(* Seg,Ofst: integer; var PA: PhysAdr *);
{-----------------------------------------
{
{ Abstract:
{    Translate a virtual address to a physical address
{
{ Parameters:
{    Seg, Ofst: the segment and offset of the virtual address
{    PA: The physical address to be returned
{
{ Side Effects:
{    PA is modified
{
{ Errors:
{
{-----------------------------------------}
  var TmpA: PhysAdr; i,j:integer;
begin
TmpA.Lo16:=Seg;
TmpA.Hi4:=0;
AddAdrs(TmpA,TmpA,TmpA);     { Find SAT entry as Seg # * 2 }
i:=GetMemory(TmpA);
if odd(i) then writeln('?WARNING: Segment ',Seg:1,' is not resident');
AddAdrs(TmpA,OneAdr,TmpA);
j:=GetMemory(TmpA);
PA.Lo16:=LAnd(i,#177400);  { First Get the base address of the segment }
PA.Hi4:=LAnd(j,#17);
TmpA.Lo16:=Ofst;
TmpA.Hi4:=0;
AddAdrs(PA,TmpA,PA)
end { TLate };
       
Procedure CheckForInput;
{-----------------------------------------
{
{ Abstract:
{    This procedure checks the keyboard for a new command line
{
{ Side Effects:
{    Affects the values of CmdLine and HaveCmdLine
{
{ Design:
{    This procedure checks the keyboard for characters, and as long as
{    characters are present it appends them to CmdLine.  When EOLN is
{    reached, HaveCmdLine is set true, and no more scanning is performed.
{    A Line-feed will also terminate input (and will be returned in the
{    resultant string if the string is null).
{-----------------------------------------}
  var Ch: char; HaveChar: boolean;
begin
HaveChar:= (IOCRead(TransKey,Ch) = IOEIOC);
if HaveChar then
    begin
    { we've got a character }
    case Ch of
      chr(#12):        { It's a line-feed }
          begin
          writeln;
          if Length(CmdLine) = 0 then
              begin
              CmdLine[0]:=chr(1);
              CmdLine[1]:=chr(#12)
              end;
          oldCmdLine := CmdLine;
          HaveCmdLine:=true
          end;
      chr(#15):        { It's a return }
          begin
          writeln;
          oldCmdLine := CmdLine;
          HaveCmdLine:=true
          end;
      chr(#3):
          begin        { ^C }
          writeln('^C');
          exitPDM;
          end;
      chr(#25):        { ^U }
          begin
          writeln('^U');
          Write('*');  {new prompt}
          CmdLine:=''
          end;
      chr(#10):        { BS }
          if Length(CmdLine) <> 0 then
              begin
              write(ch,' ',ch);
              CmdLine[0]:=chr(length(CmdLine)-1)
              end;
      '?' : begin
            write(Ch);
            if length(CmdLine)=0 then begin
                                      haveCmdLine := true;
                                      oldCmdLine := '?';
                                      end;
            CmdLine[0]:=chr(Length(CmdLine)+1);
            CmdLine[Length(CmdLine)]:=Ch;
            end;
      chr(#33) : begin {ESC}
            if length(CmdLine)=0 then 
                begin
                haveCmdLine := true;
                CmdLine := OldCmdLine;
                WriteLn(CmdLine);
                end
             else begin  {pretend it is a CR}
                  writeln;
                  oldCmdLine := CmdLine;
                  HaveCmdLine:=true
                  end;
             end;
      Otherwise:      { It's an ordinary character }
          begin
          write(Ch);
          CmdLine[0]:=chr(Length(CmdLine)+1);
          CmdLine[Length(CmdLine)]:=Ch
          end
      end { case };
    end; {have char}
if HaveCmdLine then CnvUpper(CmdLine)
end { CheckForInput };
            
Procedure NeedInput;
{-----------------------------------------
{
{ Abstract:
{    This procedure is called when an input line is needed.
{
{ Side Effects:
{    HaveCmdLine is set true and CmdLine contains the new command line
{
{ Design:
{    Command Files are only used for input when a command line is
{    required;  hence command files are read here as opposed to
{    CheckForInput
{-----------------------------------------}
begin
if HavePDMFile and not running then
    begin
    readln(PDMFile,CmdLine);
    CnvUpper(CmdLine);
    Writeln(CmdLine);
    HaveCmdLine:=true;
    if EOF(PDMFile) then
        begin
        HavePDMFile := false;
        close(PDMFile)
        end
    end
else
    begin
    HaveCmdLine:=false;
    CmdLine:='';
    repeat
        CheckForInput;
    until HaveCmdLine;
    end
end { NeedInput };

procedure LOp(* var S:string *);
{-----------------------------------------
{
{ Abstract:
{    Removes the first character from a string
{
{ Parameters:
{    The String to be altered
{
{ Side Effects:
{    The original string is directly altered
{-----------------------------------------}
begin
if Length(S) > 1 then
    S := SubStr(S,2,Length(S)-1)
else
    S:=''
end { LOp };

function HexConvert(c: Char; var val: integer): boolean;
    begin
    HexConvert := true;
    if c in ['0'..'9'] then val := ord(c) - ord('0')
    else if c in ['a'..'f'] then val := ord(c)-ord('a')+10
    else if c in ['A'..'F'] then val := ord(c)-ord('A')+10
    else Begin
         val := 0;
         HexConvert := false;
         end;
    end;

function GetVal(* Str: String; var Hi,Lo: integer; Bits: integer; radix: integer):boolean *);
{-----------------------------------------
{
{ Abstract:
{    Get a number out of Str in the specified radix.
{    GetVal can translate up to 32 bit numbers.
{
{ Parameters:
{    Str : the string containing the number to be converted
{    Hi : return value of the high 16 bits of the number
{    Lo : return value of the low 16 bits of the number
{    Bits : the maximum number of allowable bits (for range checking )
{
{ Results:
{    Returns true if a valid number has been parsed
{
{ Errors:
{    Errors will occur if either bad digits are encountered or if
{    too many bits of precision are given
{-----------------------------------------}
  var i,tmp:integer; Brk:string[1];
      c: Char;
begin
GetVal:=false;        { assume it's no good until we get done }
RemDelimiters(Str,Dels,Brk);
if Length(Str) = 0 then exit(GetVal);
lo := 0;
hi := 0;
tmp := 1;
case radix of
   2: for i := 1 to Length(Str) do
         begin
         c := str[Length(Str)-i+1]; 
         if not(c in ['0'..'1']) then
             begin
             WriteLn('?Bad binary digit "',c,'" in "',Str,'"');
             exit(GetVal);
             end;
         if i < 17 then lo := lo + Shift(ord(c)-ord('0'), i-1)
         else if i < 33 then hi := hi + shift(ord(c)-ord('0'), i-17) 
         else begin     
              writeln('?Too many digits');
              exit(GetVal)
              end;
         end;
   8: for i:=1 to Length(Str) do
         begin
         c := str[Length(Str)-i+1]; 
         if not(c in ['0'..'7']) then
             begin
             WriteLn('?Bad octal digit "',c,'" in "',Str,'"');
             exit(GetVal);
             end;
         if i < 6 then
             begin
             Lo:=Lo + Tmp*(ord(c) - ord('0'));
             Tmp:=Tmp*8
             end
         else if i=6 then
             begin
             if LAND(1,ord(c)) <> 0 then
                 Lo:=LOr(Lo,#100000);
             Hi:=(ord(c)-ord('0')) div 2;
             Tmp:=4
             end
         else if i < 11 then
             begin
             Hi:=Hi + (ord(c) - ord('0'))*Tmp;
             Tmp:=Tmp*8
             end
         else if i = 11 then
             if Str[Length(Str)-i+1] > '3' then
                 begin
                 writeln('?Number too large');
                 exit(GetVal)
                 end
             else
                 Hi:=LOr(Hi,(ord(c) - ord('0'))*Tmp)
         else
             begin
             writeln('?Too many digits');
             exit(GetVal)
             end
         end;
  10: for i:=1 to Length(Str) do
         begin
         c := str[Length(Str)-i+1]; 
         if not(c in ['0'..'9']) then
             begin
             WriteLn('?Bad decimal digit "',c,'" in "',Str,'"');
             exit(GetVal);
             end;
         if i < 5 then
             begin
             Lo:=Lo + (ord(c) - ord('0')) * Tmp;
             Tmp:=Tmp*10
             end
         else if i=5 then
             begin
             if (c > '3') or ((c = '3') and (lo > 2767)) then
                begin
                WriteLn('??Sorry, only 15 bits in decimal allowed');
                exit(GetVal);
                end;
             Lo:=Lo + (ord(c) - ord('0'))*Tmp;
             Tmp:=Tmp*10
             end
         else
             begin
             WriteLn('?Sorry, only 15 bits in decimal allowed');
             exit(GetVal)
             end
         end;
  16: for i := 1 to Length(Str) do
         begin
         c := str[Length(Str)-i+1]; 
         if not HexConvert(c, j) then
             begin
             WriteLn('?Bad hex digit "',c,'" in "',Str,'"');
             exit(GetVal);
             end;
         if i < 5 then lo := lo+shift(j, (i-1)*4)
         else if i < 9 then hi := Hi + shift(j,(i-5)*4)
         else begin     
              writeln('?Too many digits');
              exit(GetVal)
              end;
         end;
  otherwise : WriteLn('  Bad radix: ', radix:1);
  end;
{ Now that we've got the number,  make sure we don't have too many significant
  bits
}
if Hi <> 0 then
    begin
    tmp:=16;
    i:=Hi
    end
else
    begin
    tmp:=0;
    i:=Lo
    end;
while i <> 0 do
    begin
    i:=Shift(i,-1);
    tmp:=tmp+1
    end;
if tmp > Bits then
    begin
    writeln('?Value requires more than specified number of bits');
    exit(GetVal)
    end;
GetVal:=true
end { GetVal };
        
function ChangeField(* fld:string; def,max:integer):integer *);
{-----------------------------------------
{
{ Abstract:
{    Get new input for a value.  Prompts with value name and default (current)
{    value.  Also performs range checking for validity.
{
{ Parameters:
{    fld : name of the value to be altered
{    def : the default (current) value
{    max : the greatest possible valid value
{
{ Results:
{    Returns the new value
{
{ Errors:
{    Checks to insure that the values are valid octal numbers
{-----------------------------------------}
  var num,i, dum:integer; BrkChr: string[1]; success: boolean;
  label 1;
begin
1: write(fld,'['); OutNum(def,1, curRadix); write('] ');
NeedInput;
RemDelimiters(CmdLine,Dels,BrkChr);
if Length(CmdLine) = 0 then
    num:=def
else
    success := GetVal(CmdLine, dum, num, 16, curRadix);
if (not success) or (num > max) then goto 1;
ChangeField:=num
end { ChangeField };

procedure ChangeMicro(* var uword:MicroInstruction *);
{-----------------------------------------
{
{ Abstract:
{    This procedure is called to patch the contents of a micro location
{
{ Parameters:
{    uword : the micro-address to be modified
{
{ Side Effects:
{    The contents of USER are modified.
{-----------------------------------------}
{ prompt and allow individual field changes of microword }
begin
with uword do
        begin
        x:=ChangeField('X',x,#377);
        y:=ChangeField('Y',y,#377);
        a:=ChangeField('A',a,7);
        b:=ChangeField('B',b,1);
        w:=ChangeField('W',w,1);
        h:=ChangeField('H',h,1);
        alu:=ChangeField('ALU',alu,#17);
        f:=ChangeField('F',f,3);
        sf:=ChangeField('SF',sf,#17);
        z:=ChangeField('Z',z,#377);
        cnd:=ChangeField('CND',cnd,#17);
        jmp:=ChangeField('JMP',jmp,#17)
        end
end { ChangeMicro };

Procedure MasCommand;
{-----------------------------------------
{
{ Abstract:
{    Open a Master file
{
{ Side Effects:
{    Closes current master file if one exists.
{   
{ Errors:
{    Leaves no master file open if the specifies file doesn't exist
{
{ Design:
{    MastName = '' indicates no master file open
{-----------------------------------------}
  var Brk:string[1];
begin
if MastName <> '' then
    close(MastFile);          { close current master file }
RemDelimiters(CmdLine,Dels,Brk);
GetSymbol(CmdLine,MastName,Breaks,Brk);
if length(MastName) = 0 then MastName := 'PDM.MAS';
if Existant(MastName) then
    Reset(MastFile,MastName)
else
    begin
    DefaultExtension(MastName,'.MAS');
    if Existant(MastName) then
        Reset(MastFile,MastName)
    else
        begin
        writeln('?Master File ',MastName,' not found');
        MastName:=''
        end
    end;
CurTest.TstName:=''
end { MasCommand };

Function FindTest(* TName:string): TestType *);
{-----------------------------------------
{
{ Abstract:
{    Locate Test TName in the current Master File.
{    The next readln(MastFile) will return the line after the
{    .PTEST or .UTEST line.
{
{ Parameters:
{    TName: The name of the test to be found
{
{ Results:
{    The type of the test, microcode or pascal, or NoTest if the test
{    isn't found
{
{ Side Effects:
{    Repositions the read pointer into MastFile
{-----------------------------------------}
  var FoundTest: boolean; TmpS,Line:string; Brk: string[1];
begin
FoundTest:=false;
reset(MastFile,MastName);
readln(MastFile,Line);
CnvUpper(Line);
while (not EOF(MastFile)) and (Not FoundTest) and (Pos(Line,'.END') <> 1) do
    begin
    if (Pos(Line,'.UTEST') = 1) or (Pos(Line,'.PTEST') = 1) then
        begin
        if Pos(Line,'.UTEST') = 1 then
            FindTest:=MicroTest
        else
            FindTest:=PascalTest;
        GetSymbol(Line,TmpS,Dels,Brk);
        RemDelimiters(Line,Dels,Brk);
        GetSymbol(Line,TmpS,Breaks,Brk);
        if TmpS = TName then FoundTest:=true
        end;
    if not FoundTest then
        begin
        Readln(MastFile,Line);
        CnvUpper(Line)
        end
    end;
if not FoundTest then FindTest:=NoTest
end { FindTest };

Procedure LstCommand;
{-----------------------------------------
{
{ Abstract:
{    List either the tests contained in a master file or the subtests
{    contained in a test
{ Side Effects:
{    Affects the current element of MastFile
{-----------------------------------------}
  var Line,TmpS,Sym:string;  Brk: string[1]; TType: TestType;
begin
if length(MastName) = 0 then
    begin
    writeln('?No Master File');
    exit(LstCommand)
    end;
RemDelimiters(CmdLine,Dels,Brk);
GetSymbol(CmdLine,Sym,Breaks,Brk);
if Length(Sym) = 0 then       { list all tests in the msater file }
    begin
    reset(MastFile,MastName);
    readln(MastFile,Line);
    CnvUpper(Line);
    while (Not EOF(MastFile)) and (Pos(Line,'.END') <> 1) do
        begin
        if (Pos(Line,'.UTEST') = 1) or (Pos(Line,'.PTEST') = 1) then
            begin
            GetSymbol(Line,TmpS,Dels,Brk);
            RemDelimiters(Line,Dels,Brk);
            writeln('  ',Line)
            end;
        readln(MastFile,Line);
        CnvUpper(Line)
        end
    end
else
    begin
    { First find the desired test }
    TType:=FindTest(Sym);
    if TType = NoTest then
        writeln('?Test ',Sym,' is not contained in Master File ',
                MastName)
    else
        begin
        { we've got the test, now look for the subtest list }
        ReadLn(MastFile,Line);
        CnvUpper(Line);
        while (Pos(Line,'.SUBTEST') <> 1) and
              (Pos(Line,'.UTEST') <> 1) and
              (Pos(Line,'.PTEST') <> 1) and
              (Pos(Line,'.END') <> 1) do
                  begin
                  Readln(MastFile,Line);
                  CnvUpper(Line)
                  end;
        if Pos(Line,'.SUBTEST') = 1 then
            begin
            { Get the list and print! }
            GetSymbol(Line,TmpS,Breaks,Brk);  {remove "SUBTEST"}
            RemDelimiters(Line,Dels,Brk);
            GetSymbol(Line,TmpS,Breaks,Brk);  {get number of subtests}
            if GetVal(TmpS,i,j,16, 8) then
                begin
                write('Master File ',MastName,', Test ',Sym);
                writeln(' contains:');
                for i:=0 to j-1 do
                    begin
                    readln(MastFile,Line);
                    if TType = MicroTest then
                        begin
                        RemDelimiters(Line,Dels,Brk);
                        GetSymbol(Line,TmpS,Dels,Brk);
                        RemDelimiters(Line,Dels,Brk)
                        end;
                    write('  ');
                    OutNum(i,2,curRadix);
                    WriteLn('  ',Line)
                    end
                end
            else
                writeln('?Error in SUBTEST command in Master File '
                         ,MastName)
                        
            end
        else
            writeln('Test ',Sym,' contains no subtests')
        end
    end
end { LstCommand };     
            
Procedure GetCommand;
{-----------------------------------------
{
{ Abstract:
{    Set up a diagnostic test for execution.
{
{ Side Effects:
{    Creates a file with the error templates for this test
{
{ Errors:
{    Errors will occur if the test is not found, or the master file
{    is bad.  Also, the binarys required to run the test must be
{    present.
{
{ Design:
{-----------------------------------------}
  var Brk:string[1]; TStr,TStr1:string; NotFirstFile:boolean;
      STmp: SubTest; i,j,k:integer;
begin
if MastName = '' then
    begin
    writeln('?No Master File');
    exit(GetCommand)
    end;
with CurTest do
    begin
    rewrite(ErrFil,ErrFilNam);
    SubCnt:=0;
    RemDelimiters(CmdLine,Dels,Brk);
    GetSymbol(CmdLine,TStr,Breaks,Brk);
    TstType:=FindTest(TStr);
    if TstType = NoTest then
        begin
        TstName:='';
        writeln('?Test ',TStr,' is not contained in Master File ',MastName);
        exit(GetCommand)
        end
    else
        TstName:=TStr;
    BetweenSubtests:=true;
    Readln(MastFile,TStr);
    CnvUpper(TStr);
    while (Pos(TStr,'.UTEST') = 0) and (Pos(TStr,'.PTEST') = 0) and
          (Pos(TStr,'.END') = 0) do
        begin
        if Pos(TStr,'.FILES') = 1 then
            begin
            { get the binaries and load them }
            GetSymbol(TStr,TStr1,Dels,Brk);     { Remove the .FILES from TStr }
            RemDelimiters(TStr,Dels,Brk);
            if TstType = PascalTest then
                begin
                { Only one name, on the same line, and it's a run file }
                GetSymbol(TStr,TStr1,Breaks,Brk);
                LoadRunFile(TStr1);
                Readln(MastFile,TStr);
                end
            else {micro test}
                begin
                while Length(TStr) = 0 do
                    begin
                    Readln(MastFile,TStr);
                    RemDelimiters(TStr,Dels,Brk)
                    end;
                NotFirstFile:=false;
                while TStr[1] <> '.' do
                    begin
                    GetSymbol(TStr,TStr1,Breaks,Brk);
                    LoadUser(NotFirstFile,TStr1);
                    NotFirstFile:=true;
                    RemDelimiters(TStr,Dels,Brk);
                    while Length(TStr) = 0 do
                        begin
                        Readln(MastFile,TStr);
                        RemDelimiters(TStr,Dels,Brk)
                        end
                    end
                end
            end
        else if Pos(TStr,'.SUBTEST') = 1 then
            begin
            { get the subtest count and list }
            GetSymbol(TStr,TStr1,Dels,Brk); {remove .SUBTEST}
            RemDelimiters(TStr,Dels,Brk);
            GetSymbol(TStr,TStr1,Breaks,Brk); {get number of subtests}
            if GetVal(TSTr1,i,j,16, 8) then
                begin
                SubCnt:=j;
                for i:=1 to SubCnt do
                    begin
                    ExeList[i-1]:=true;
                    Readln(MastFile,TStr); {read the subtest name}
                    if TstType = MicroTest then
                        begin
                        { Get the Start Address for the subtest }
                        RemDelimiters(TStr,Dels,Brk);
                        GetSymbol(TStr,TStr1,Breaks,Brk);
                        if GetVal(TStr1,j,k,12, 8) then
                            SubList[i-1].StartAdr:=k
                        else
                            begin
                            writeln('?BAD format in MasterFile: Bad Start Address for micro test: ', TStr1);
                            TstName:='';
                            Exit(GetCommand)
                            end
                        end;
                    RemDelimiters(TStr,Dels,Brk);
                    SubList[i-1].STName:=TStr
                    end;
                Readln(MastFile,TStr);  {get line after last subtest}
                end
            else
                begin
                WriteLn('?BAD format in MasterFile: Bad number of subTests: ', TStr1);
                TstName:='';
                exit(GetCommand)
                end;
            while length(TStr) = 0 do Readln(MastFile,TStr); {skip blank lines}
            if TStr[1] <> '.' then 
               begin
               WriteLn('?BAD format in MasterFile: Line after subtests must begin with a dot: ',TStr);
               TstName:='';
               exit(GetCommand)
               end;
            end
        else if Pos(TStr,'.ERRORS') = 1 then
            begin
            readln(MastFile,TStr);
            while (length(TStr) <> 0) and (TStr[1] <> '.') do
                begin
                writeln(ErrFil,TStr);
                readln(MastFile,TStr)
                end;
            close(ErrFil)
            end
        else
            begin
            WriteLn('?BAD format in MasterFile: Illegal command: ',TStr); 
            TstName:='';
            exit(GetCommand)
            end;
        end
    end;
if CurTest.TstName <> '' then
    begin
    PassNum:=0;
    ErrCnt:=0;
    HadError:=false;
    SendRepOnce:=false;
    SendAdvOnce:=false;
    SendTerminate:=false;
    with CurStatus do
        for i:=0 to 3 do
            Ary[i]:=0
    end
end { GetCommand };

Procedure HlpCommand;
{-----------------------------------------
{
{ Abstract:
{    Provide User with HELP
{
{ Errors:
{    If PDM.HELP is not found, a fatal File System Error will occur
{
{ Design:
{    GiveHelp simply prints the file PDM.HELP
{-----------------------------------------}
  var F:Text;  Str:string;
begin
reset(F,'PDM.HELP');
SCurOff;  {so print is faster}
while not EOF(F) do
    begin
    readln(f,str);
    writeln(str)
    end;
close(f);
SCurOn;
end { HlpCommand };

Function ParseAdr(* var S:string; Var Res: PhysAdr): boolean *);
{-----------------------------------------
{
{ Abstract:
{    Parse Memory address from S.  If address is virtual, convert it to
{    physical
{
{ Parameters:
{    S: String containing address
{    Res: the resultant address
{
{ Results:
{    Returns true if address is OK
{
{ Side Effects:
{    Address is removed from S.  Contents of Res is modified.
{-----------------------------------------}
  var Brk:string[1]; Sym:string; seg,ofst,i,Hi,Lo:integer;
begin
ParseAdr:=false;       { inut presumed Bad until totally parsed }
RemDelimiters(S,Dels,Brk);     { pitch any leading spaces }
GetSymbol(S,Sym,Break1,Brk);
RemDelimiters(S,Dels,Brk);
if (Length(Brk) = 0) or (Brk[1] <> ',') then
    begin
    { address is physical }
    ParseAdr:=GetVal(Sym,Hi,Lo,20, curRadix);
    with Res do
        begin
        Lo16:=Lo;
        Hi4:=Hi
        end
    end
else
    { address is virtual }
    if GetVal(Sym,i,Seg,16, curRadix) then
        begin
        LOp(S);     { remove the ',' }
        RemDelimiters(S,Dels,Brk);
        GetSymbol(S,Sym,Breaks,Brk);
        if GetVal(Sym,i,Ofst,16, curRadix) then
            begin
            TLate(Seg,Ofst,Res);
            ParseAdr:=true
            end
        end
end { ParseAdr };

Procedure PrintMem(* Adr: PhysAdr *);
{-----------------------------------------
{
{ Abstract:
{    Read and Print the contents of the memory location specified by Adr.
{
{ Parameters:
{    Adr: The physical address of the memory location to be printed
{
{ Errors:
{    Fatal Error if address is invalid
{-----------------------------------------}
  var val:integer;
begin
write('M');
with Adr do
    begin
    Val:=GetMemory(Adr);
    if LAnd(#100000,Lo16) = 0 then
        begin
        OutNum(Hi4*2,1, curRadix);
        OutNum(Lo16,5, curRadix)
        end
    else
        begin
        OutNum(Hi4*2+1,1, curRadix);
        OutNum(LAnd(#77777,Lo16),5, curRadix)
        end
    end;
write('/  ');
OutNum(Val,6, curRadix);
writeln
end { PrintMem };

Procedure MemCommand;
{-----------------------------------------
{
{ Abstract:
{    Complete parsing and execution of MEMORY commands
{
{ Side Effects:
{    CmdLine, CurState, CurAdr are modified.
{-----------------------------------------}
  label 1;
  var Brk: string[1]; Sym: string; StartA,EndA: PhysAdr; HiVal,LoVal,i: integer;
begin
RemDelimiters(CmdLine,Dels,Brk);
GetSymbol(CmdLine,Sym,Breaks,Brk);
RemDelimiters(CmdLine,Dels,Brk);
if Length(Sym) = 0 then
    1: writeln('MEMORY <address>[:<address>] | MEMORY <address>=<data>')
else
    if ParseAdr(Sym,StartA) then
        if length(Brk) = 0 then
             begin
             CurState:=Memory;
             CurAdr:=StartA;
             PrintMem(StartA)
             end
         else if Brk[1] = ':' then
             begin
             Lop(CmdLine);
             RemDelimiters(CmdLine,Dels,Brk);
             GetSymbol(CmdLine,Sym,Breaks,Brk);
             if ParseAdr(Sym,EndA) then
                 if (EndA.Hi4 < StartA.Hi4) or ((EndA.Hi4 = StartA.Hi4) and
                   (EndA.Lo16 < StartA.Lo16)) then
                     goto 1
                 else
                     begin
                     CurAdr:=StartA;
                     CurState:=Memory;
                     PrintMem(CurAdr);
                     while (CurAdr.Lo16 <> EndA.Lo16) or
                       (CurAdr.Hi4 <> EndA.Hi4) do
                         begin
                         AddAdrs(CurAdr,OneAdr,CurAdr);
                         PrintMem(CurAdr)
                         end
                     end
             else
                 goto 1
             end
         else if Brk[1] = '=' then
             begin
             Lop(CmdLine);
             RemDelimiters(CmdLine,Dels,Brk);
             GetSymbol(CmdLine,Sym,Breaks,Brk);
             if GetVal(Sym,HiVal,LoVal,16, curRadix) then
                 begin
                 CurState:=Memory;
                 CurAdr:=StartA;
                 with StartA do
                     case Hi4 of
                         0: WriteMemory(Lo16,LoVal);
                         1: WriteDisplay(Lo16,LoVal);
                         Otherwise: writeln('?Fatal Memory Address: ',Hi4:1:8,
                                            '''',Lo16:6:-8)
                       end { case }
                 end
             else goto 1
             end
         else goto 1
end { MemCommand };

Procedure Initialize;
begin { Initialize }
PDMDebug := true;
SCurOn;
CurOutput := Verbose;
with OneAdr do
    begin
    Hi4:=0;
    Lo16:=1
    end;
CmdInit;
OldCmdLine := '';
InitLink;
MastName:='';             { Currently no master file }
CurTest.TstName:='';      { Currently no test setup }
HavePDMFile:=false;
HaveCmdLine:=false;
Running:=false;
WaitAbort:=false;
curRadix := 8;
CurState:=Register;
CurAdr.Lo16:=0;
Disabled := false;
for i:=0 to MaxUser do
    with User[i] do
        begin
        word1:=-1;
        word2:=-1;
        word3:=-1
        end;
{ Initialize the User Flags }
for i:=0 to LastFlag do
    Flags[i]:=false;
FlgNames[LonT]:='Loop On Test';
FlgNames[LonE]:='Loop On Error';
FlgNames[LFonE]:='Loop Forever On Error';
FlgNames[HonE]:='Halt On Error';
FlgNames[SonE]:='Speak On Error';
FlgNames[HatEOP]:='Halt At End Of Pass';
FlgNames[HatEOST]:='Halt At End Of Subtest';
FlgNames[SSTMode]:='Single Step Mode'

end { Initialize }.
