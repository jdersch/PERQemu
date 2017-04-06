{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module EditorKeySelection;

{---------------------------------------
{ written by: WJHansen
{ Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{     Key selection commands for editor
{---------------------------------------}

{---------------------------------------
{ Change Log:
     16 Feb 83  V2.8  WJHansen
         Adapt for landscape monitor.
{     9 Sep 82  V2.7  WJHansen
{        export the primitive word and line operations
{---------------------------------------}



exports 

  imports Editor from Editor;

    procedure PickUnit(CmdCh: char);
    procedure CharMove(CmdCh: char);
    procedure ScrollCmd(CmdCh: char);
    function  KeyThumb: Boolean;
    procedure GotoChar(Auto:boolean);
    
    function  IsLnStart(C: Cursor): Boolean;
    procedure BckLnStart(var C: Cursor; var P: Position);

    procedure LFofCRLF(var P: Position);
    procedure CRofCRLF(var P: Position);

    function IsWdStart (C: Cursor; var WordChars: CharTable): Boolean;
    function IsWdEnd (C: Cursor; var WordChars: CharTable): Boolean;
    function FwdWdStart (var C:Cursor; var P: Position; 
                   var WordChars: CharTable): Boolean;
    function BckWdEnd (var C:Cursor; var P: Position; 
                   var WordChars: CharTable): Boolean;
    procedure FwdWdEnd (var C:Cursor; var P: Position; 
                   var WordChars: CharTable);
    procedure BckWdStart (var C:Cursor; var P: Position; 
                   var WordChars: CharTable);
    function PrevWord(var C:Cursor;var P:Position;
                   var WordChars:CharTable):Boolean;
    
    function IsLnEnd(C:Cursor): Boolean;
    procedure FwdLnEnd (var C: Cursor; var P: Position);
    function FwdLnStart(var C: Cursor;  var P: Position):  Boolean;
    function BckLnEnd (var C: Cursor; var P: Position): Boolean;
    function PrevLine(var C: Cursor;  var P: Position):  Boolean;

var
    OldGChar: char;     {Character searched for by automatic G command}

private

{$ifc ReplayVersion and KeySelection then}
{$message Compiling transcript and keyselection version.}
{$elsec}
{$ifc ReplayVersion then}
{$message Compiling transcript/replay version.}
{$message ERROR    ERROR   Should be with KeySelection.   ERROR   ERROR}
{$elsec}
{$IFC KeySelection THEN}
{$message Compiling key selection version.}
{$ELSEC}
{$message Compiling standard version.}
{$message ERROR    ERROR   Should be with KeySelection.   ERROR   ERROR}
{$ENDC}
{$endc}
{$endc}


 imports EditorUtilities from EditorU;
 imports Perq_String from Perq_String;
 imports CmdParse from CmdParse;

type 
   SpaceLength = (Single, Tabbing);
   
var

   NewFirst, NewLast: Position;   {Temporaries so SelectFirst And SelectLast
                                   can be limits for Moreing}


{Note:  Be Sure that we don't go off ends of file.  Editor has inserted
dummy CR and LF before beginning and dummy Etx after end of file.  
Program uses Bot and Eot to detect being at one end or the other.
}


{Adopt this obscure convention for internal routines:  selections are to
the Cr of a CRLF.}
 



procedure LFofCRLF(var P: Position);
begin
   Attach (Cursor1, P, ReadCursor);
   if Cursor1.Ch = CR then begin
      Add1C(Cursor1);
      if Cursor1.Ch=LF then
         P := Cursor1.Pos;
   end;
   Detach(Cursor1);
end;


procedure CRofCRLF(var P: Position);
begin
   Attach (Cursor1, P, ReadCursor);
   if Cursor1.Ch = LF then begin
      Sub1C(Cursor1);
      if Cursor1.Ch=CR then
         P := Cursor1.Pos;
   end;
   Detach(Cursor1);
end;


procedure BackChar (var C: Cursor);   
      {Backs one char, but two if it's a CRLF}
begin
   Sub1C(C);
   if C.Ch = LF then begin
      Sub1C(C);
      if C.Ch <> CR then 
         Add1C(C)
   end
end;


procedure PickChar(MoveDirection: char;  HowFar: SpaceLength);
var
   JustSawCR: Boolean;
   MaxCount: integer;
   TPos: Position;
begin
   Count := 0;
   if RepeatCount <1 then RepeatCount := 1;
   if HowFar=Single then MaxCount := RepeatCount
   else if RepeatCount > MaxInt div TABLength then
      MaxCount := MaxInt
   else MaxCount := TABLength * RepeatCount;
   if MoveDirection = '>' then begin
      Attach (Cursor1, NewLast, ReadCursor);
      if MaxCount < 20 then begin
         repeat 
            JustSawCR := (Cursor1.Ch=CR);
            Add1C(Cursor1);
            if Eot(Cursor1.Pos) then
               MaxCount := 0
            else if JustSawCR and (Cursor1.Ch=LF) then
               Add1C(Cursor1);
            Count := Count + 1
         until Count >= MaxCount;
      end else begin
         AddC(Cursor1, MaxCount);
         if Cursor1.Ch = LF then
            Add1C (Cursor1);
      end;
   end else begin {MoveDirection='<'}
      Attach (Cursor1, NewFirst, ReadCursor);
      if MaxCount < 20 then begin
         repeat 
            Sub1C(Cursor1);
            if Cursor1.Ch = LF then begin
               Sub1C(Cursor1);
               if Cursor1.Ch = CR then begin
                  if Bot(Cursor1.Pos) then 
                     MaxCount := 0;
               end
               else if Count+1 < MaxCount then 
                  Count := Count+1
               else Add1C(Cursor1);
            end;
            Count := Count + 1
         until Count >= MaxCount;
      end else begin
         AddC(Cursor1, -MaxCount);
         if Cursor1.Ch = LF then
            Sub1C (Cursor1);
      end;
      Add (FilledFirst, 2, TPos);
      while LT(Cursor1.Pos, TPos) do 
         Add1C(Cursor1);
   end;
   if HowFar=Tabbing then 
      Count := Count div 5;
   NewFirst := Cursor1.Pos;
   NewLast := NewFirst;
   Detach(Cursor1);
end {PickChar};


{Words:  A word is a consecutive sequence of the characters in the
parameter  WordChars.}



function IsWdStart (C: Cursor; var WordChars: CharTable): Boolean;
var 
   TCh: char;
begin
   TCh := C.Ch;
   Sub1C(C);
   IsWdStart := (WordChars[TCh] and not WordChars[C.Ch]);
end;


function IsWdEnd (C: Cursor; var WordChars: CharTable): Boolean;
var 
   TCh: char;
begin
   TCh := C.Ch;
   Add1C(C);
   IsWdEnd := (WordChars[TCh] and not WordChars[C.Ch]);
end;




{Note that Bot is true for first character in text, but Eot is true
only for the dummy after the end of the text.}


function FwdWdStart (var C:Cursor; var P: Position; var WordChars: CharTable):
       Boolean;
   {Assumes the cursor points to a non-word character.}
begin
   repeat 
      Add1C(C);
   until  WordChars[C.Ch] or Eot(C.Pos);
   if Eot(C.Pos) then begin
      BackChar(C);
      FwdWdStart := False;
   end else
      FwdWdStart := True;
   P := C.Pos;
end;


function BckWdEnd (var C:Cursor; var P: Position; var WordChars: CharTable):
       Boolean;
   {Assumes the cursor points to a non-word character.}
begin
   while not WordChars[C.Ch] and not Bot(C.Pos) do
      Sub1C(C);
   BckWdEnd := (WordChars[C.Ch]);
   P := C.Pos;
end;


procedure FwdWdEnd (var C:Cursor; var P: Position; var WordChars: CharTable);
   {Assumes it is in a word;  i.e., on a WordChars character.}
begin
   repeat 
      Add1C(C)
   until
      not WordChars[C.Ch];
   Sub1C(C);
   P := C.Pos;
end;


procedure BckWdStart (var C:Cursor; var P: Position; var WordChars: CharTable);
   {Assumes it is in a word;  i.e., on a WordChars character.}
begin
   repeat 
      Sub1C(C)
   until
      not WordChars[C.Ch];
   Add1C(C);
   P := C.Pos;
end;


function PrevWord(var C:Cursor;var P:Position;var WordChars:CharTable):Boolean;
begin
   PrevWord := True;
   if IsWdStart(C,WordChars) then 
      if not Bot(C.Pos) then Sub1C(C);
   if WordChars[C.Ch] then 
      BckWdStart(C,P,WordChars)
   else 
      if BckWdEnd(C,P,WordChars) then 
         BckWdStart(C,P,WordChars)
      else PrevWord := False;
end;


function PrevLets(var C:Cursor; var P:Position): Boolean;
begin
   PrevLets := PrevWord(C, P, LettersAndDigits);
end;


function PrevPrint(var C:Cursor; var P:Position): Boolean;
begin
   PrevPrint := PrevWord(C, P, PrintChars);
end;


procedure PickWord (Direction: char;  WordChars: CharTable);
var
   Changed: Boolean;
begin
   Attach (Cursor1, NewFirst, ReadCursor);
   Changed := False;
   if Moreing then begin
      if Direction = '>' then 
         ReAttach (Cursor1, NewLast);
      if WordChars[Cursor1.Ch] then
         if (Direction = '>') and not IsWdEnd(Cursor1, WordChars) then begin
            FwdWdEnd(Cursor1, tmp, WordChars);
            NewLast := tmp;
            Changed := True;
         end
         else if (Direction='<')and not IsWdStart(Cursor1,WordChars) then begin
            BckWdStart(Cursor1, tmp, WordChars);
            NewFirst := tmp;
            Changed := True;
         end;
   end
   else if WordChars[Cursor1.Ch] then begin
      BckWdStart(Cursor1, tmp, WordChars);
      if NE(tmp, NewFirst) then 
         Changed := True;
      ReAttach (Cursor1, NewFirst);
      NewFirst := tmp;
      FwdWdEnd(Cursor1, tmp, WordChars);
      if NE(tmp, NewLast) then 
         Changed := True;
      NewLast := tmp;
   end;
   if RepeatCount <= 0 then RepeatCount := 1;
   if Changed then Count := 1
   else Count := 0;
   if Direction = '>' then begin
      if WordChars[Cursor1.Ch] then
         Add1C(Cursor1);  {Move to non-word char} 
      if Eot(Cursor1.Pos) then
         RepeatCount := 0;
      while Count < RepeatCount do begin
         {Assert:  Cursor1.Ch is not in WordChars}
         Count := Count+1;
         if not FwdWdStart(Cursor1, tmp, WordChars) then
            RepeatCount := 0
         else begin
            NewFirst := tmp;
            FwdWdEnd (Cursor1, NewLast, WordChars);
            Add1C(Cursor1);
            if Eot(Cursor1.Pos) then
               RepeatCount := 0;
         end;
      end;
   end
   else {Direction = '<'} begin
      ReAttach (Cursor1, NewFirst);
      if WordChars[Cursor1.Ch] then
         Sub1C(Cursor1);
      if Bot(Cursor1.Pos) then
         RepeatCount := 0;
      while Count < RepeatCount do begin
         {Assert:  Cursor1.Ch is not in WordChars}
         Count := Count+1;
         if not BckWdEnd(Cursor1, tmp, WordChars) then
            RepeatCount := 0
         else begin
            NewLast := tmp;
            BckWdStart (Cursor1, NewFirst, WordChars);
            Sub1C(Cursor1);
            if Bot(Cursor1.Pos) then
               RepeatCount := 0;
         end;
      end;
   end;
   Detach (Cursor1);
   ThisSelect := SelectWord;
end {PickWord};


{A line ends with CRLF or the end of file.
It starts with the first character after an end of a line.
}

function IsLnEnd(C:Cursor): Boolean;
var
   tch: char;
begin
   IsLnEnd := False;
   if Eot(C.Pos) then  IsLnEnd := True
   else if C.Ch=CR then begin
         Add1C(C);
         if C.Ch=LF then IsLnEnd := True
   end
end;
   

function IsLnStart(C:Cursor): Boolean;
var
   tch: char;
begin
   IsLnStart := False;
   Sub1C(C);
   if C.Ch=LF then begin
      Sub1C(C);
      if C.Ch=CR then IsLnStart := True
   end
end;
   

procedure FwdLnEnd (var C: Cursor; var P: Position);
var 
   done: Boolean;
begin
   repeat
      done := True;
      repeat
         Add1C(C)
      until not LineChars[C.Ch];
      if C.Ch=LF then begin
         Sub1C(C);
         if C.Ch<>CR then begin
            done := false;
            Add1C(C)
         end;
      end
      else if C.Ch = Etx then 
         if not Eot(C.Pos) then
            done := False;
   until done;
   P := C.Pos;
end;


function FwdLnStart(var C: Cursor;  var P: Position):  Boolean;
begin
   if not IsLnEnd(C) then
      FwdLnEnd (C, P);
   FwdLnStart := True;
   if C.Ch=CR then AddC (C, 2);
   if Eot (C.Pos) then 
      FwdLnStart := False;
   P := C.Pos;
end;


function BckLnEnd (var C: Cursor; var P: Position): Boolean;
var
   done: Boolean;
begin
   BckLnEnd := True;
   repeat
      done := True;
      repeat
         Sub1C(C)
      until not LineChars[C.Ch];
      if C.Ch=LF then begin
         Sub1C(C);
         if C.Ch<>CR then begin
            done := false;
            Add1C(C)
         end;
      end
      else if C.Ch=Etx then
         done := False;
   until done;
   if EQ(C.Pos, FilledFirst) then begin
      BckLnEnd := False;
      Add1C(C);
      Add1C(C);
   end;
   P := C.Pos;
end;


procedure BckLnStart(var C: Cursor;  var P: Position);
   {Assumes it is not at a line start.}
begin
   if BckLnEnd(C, P) then
      if FwdLnStart(C, P) then ;   {Neither 'else' can happen.}
   P := C.Pos;
end;


function PrevLine(var C: Cursor;  var P: Position):  Boolean;
begin
   if IsLnStart(C) then
      if BckLnEnd (C, P) then  ;
   if Bot(C.Pos) then PrevLine := False
   else begin
      PrevLine := True;
      BckLnStart(C,P);   end;
end;


procedure PickLine (Direction: char);
begin
   Count := 0;
   Attach (Cursor1, NewFirst, ReadCursor);
   if Moreing then begin
      if Direction = '>' then 
         ReAttach (Cursor1, NewLast);
      if (Direction = '>') and not IsLnEnd(Cursor1) then begin
         FwdLnEnd(Cursor1, NewLast);
         Count := 1;
      end
      else if (Direction='<')and not IsLnStart(Cursor1) then begin
         BckLnStart(Cursor1, NewFirst);
         Count := 1;
      end;
   end
   else begin
      if not IsLnStart(Cursor1) then begin
         BckLnStart(Cursor1, NewFirst);
         Count := 1;
      end;
      ReAttach (Cursor1, NewLast);
      if not IsLnEnd(Cursor1) then begin
         FwdLnEnd(Cursor1, NewLast);
         Count := 1;
      end;
   end;
   if RepeatCount <= 0 then RepeatCount := 1;
   if Direction = '>' then begin
      if Eot(Cursor1.Pos) then
         RepeatCount := 0;
      while Count < RepeatCount do begin
         Count := Count+1;
         if not FwdLnStart(Cursor1, tmp) then
            RepeatCount := 0
         else begin
            NewFirst := tmp;
            FwdLnEnd (Cursor1, NewLast);
            if Eot(Cursor1.Pos) then
               RepeatCount := 0;
         end;
      end;
   end
   else {Direction = '<'} begin
      ReAttach (Cursor1, NewFirst);
      if Bot(Cursor1.Pos) then
         RepeatCount := 0;
      while Count < RepeatCount do begin
         Count := Count+1;
         if not BckLnEnd(Cursor1, tmp) then
            RepeatCount := 0
         else begin
            NewLast := tmp;
            BckLnStart (Cursor1, NewFirst);
            if Bot(Cursor1.Pos) then
               RepeatCount := 0;
         end;
      end;
   end;
   Detach (Cursor1);
   ThisSelect := SelectLine;
end {PickLine};


 
procedure ReFocus(Direction: char);
   {Clean up after key selection:
      Advance NewLast to LF if CRLF;
      Set SelectFirst and SelectLast;
      Show the end of the selection in the direction of motion;
      Do the selection cleanup: S in thumbbar, Selection display, underline.
      Set Position so cursor will display.
   }
var
   tPos: Position;
begin
   LFofCRLF(NewLast);
   if Direction = '>' then begin
      if Moreing then begin
         if OnScreen(NewLast,0,LastLine) then 
            if LT(NewLast, SelectLast) then begin
               tPos := NewLast;
               Add1(tPos);
               UnderLine (tPos, SelectLast, White);
            end
            else begin
               CRofCRLF(SelectLast);
               Underline(SelectLast,NewLast,Black);
            end;
         SelectLast := NewLast;
         if LT(SelectLast, SelectFirst) then begin
            SelectFirst := SelectLast;
            CRofCRLF(SelectFirst);
            UnderLine(SelectFirst,SelectLast,Black);
         end;
      end
      else begin
         SelectFirst := NewFirst;
         SelectLast := NewLast;
         if OnScreen(SelectLast,0,LastLine) then 
            UnderLine(SelectFirst, SelectLast, Black);
      end;
      Show(SelectLast,0,LastLine);
      ScreenPos(SelectLast,ThisLine, ThisColumn);
   end 
   else begin
      if Moreing then begin
         if OnScreen(NewFirst,0,LastLine) then 
            if GT(NewFirst, SelectFirst) then begin
               tPos := NewFirst;
               Sub1(tPos);
               CRofCRLF(tpos);
               UnderLine(SelectFirst, tPos, White);
            end
            else begin
               LFofCRLF(SelectFirst);
               UnderLine(NewFirst, SelectFirst, Black);
            end;
         SelectFirst := NewFirst;
         if LT(SelectLast, SelectFirst) then begin
            SelectLast := SelectFirst;
            LFofCRLF(SelectLast);
            UnderLine(SelectFirst,SelectLast,Black);
         end;
      end
      else begin
         SelectFirst := NewFirst;
         SelectLast := NewLast;
         if OnScreen(SelectFirst,0,LastLine) then
            UnderLine(SelectFirst,SelectLast,Black);
      end;
      Show(SelectFirst,0,LastLine);
      ScreenPos(SelectFirst,ThisLine, ThisColumn);
   end;
   DrawLn(SelectL);
   DrawThumbBar;
   ThisColumn := LastColumn-1;
end {ReFocus};



{Adopt this obscure convention for internal routines:  selections are to
the Cr of a CRLF.  Never select a LF.}
 

procedure PickUnit(CmdCh:Char);
begin
   if RepeatCount <= 0 then RepeatCount := 1;
   NewFirst := SelectFirst;
   NewLast := SelectLast;
   if GT(SelectFirst, ScreenLast) or LT(SelectLast, ScreenFirst) then begin
      NewFirst:= ScreenFirst;
      NewLast:= ScreenFirst;
   end
   else if not Moreing then 
      UnderLine (SelectFirst,SelectLast, White);
   CRofCRLF(NewLast);
   case CmdCh of
      'c', 'C':   PickChar(Direction, Single);
      'w':        PickWord(Direction, LettersAndDigits);
      'W':        PickWord(Direction, PrintChars);
      'l', 'L':   PickLine(Direction);
   end;
   ReFocus (Direction);
end;


procedure CharMove(CmdCh:char);
var
   MoveDirection: char;
   
   procedure CountUnits(function Move(var C:Cursor; var P:Position): Boolean);
   begin
      Attach (Cursor1, NewFirst, ReadCursor);
      Count := 0;
      while Count < RepeatCount do begin
         Count := Count+1;
         if not Move(Cursor1, NewFirst) then
            RepeatCount := 0;
      end;
      NewLast := NewFirst;
      Detach(Cursor1);
   end;
      
begin {CharMove}
   if RepeatCount <= 0 then RepeatCount := 1;
   if CmdCh in [' ', TAB, CR, LF] then 
      MoveDirection := '>'
   else
      MoveDirection := '<';
   if (Moreing and (Direction = '>'))
         or  (not Moreing and (MoveDirection = '>')) then begin  
      NewFirst := SelectLast;
      CRofCRLF(NewFirst);
   end
   else NewFirst := SelectFirst;   
   NewLast := NewFirst;
   if GT(SelectFirst, ScreenLast) or LT(SelectLast, ScreenFirst) then begin
      NewFirst:= ScreenFirst;
      NewLast:= ScreenFirst;
   end
   else if not Moreing then 
      UnderLine (SelectFirst,SelectLast, White);
   case CmdCh of
      ' ', BS1, BS2, BS3:   PickChar(MoveDirection, Single);
      TAB, CtlTab:          PickChar(MoveDirection, Tabbing);
      CR:                   CountUnits (FwdLnStart);
      BL1, BL2, BL3, CtlCR: CountUnits (PrevLine);
      BW1, BW2:             CountUnits (PrevLets);
      BW3:                  CountUnits (PrevPrint);
   end; 
   ReFocus(Direction);      {Display the end where the action is}
   ThisSelect := SelectChar;
end {CharMove};


procedure ScrollCmd(CmdCh: Char);
var
   NowLine: LineIndex;
   NowColumn: ColumnIndex;
begin
   Count:=0;
   if RepeatCount<=0 then RepeatCount:=1;
   while Count<RepeatCount do begin
      Count := Count +1;
      Case CmdCh of
      LF:     ScrollUp(0, LastLine, 3*LastLine div 4);
      CtlLF:  ScrollDown(0, LastLine, 3*LastLine div 4);
      'T','t':begin
                 RepeatCount := 0;
                 ScreenPos(SelectFirst, NowLine, NowColumn);
                 if NowLine = 0 then 
                 else if (NowLine>0) and (NowLine<=LastLine) then 
                    ScrollUp(0, LastLine, NowLine)
                 else begin
                    OldFirst := ScreenFirst;
                    OldDot := ScreenFDot;
                    Show (SelectFirst, 0, LastLine);
                 end;
              end;
      'B','b':begin
                 RepeatCount := 0;
                 ScreenPos(SelectLast, NowLine, NowColumn);
                 if NowLine=LastLine then
                 else if (NowLine>=0) and (NowLine<LastLine) then 
                    ScrollDown(0, LastLine, LastLine-NowLine)
                 else begin
                    OldFirst := ScreenFirst;
                    OldDot := ScreenFDot;
                    Show (SelectLast, 0, LastLine);
                 end;
              end;
      end;
      if Bot(ScreenFirst) or Eot(ScreenLast) then
         RepeatCount := 0;
   end;
   ScreenPos(SelectFirst, ThisLine, ThisColumn);
   if (ThisLine<0) or (ThisLine>LastLine) then begin
      ThisLine := 0;
      ThisColumn := LastColumn-1;
   end;
end;


function KeyThumb: Boolean;
var
   TLine: LineIndex;
   TColumn: ColumnIndex;
   NewColumn: integer;
   LastChar: char;
   PrevOFirst: Position;
   PrevODot: integer;
   
   procedure GatherNumber;
   var
      NextNumber, Digit: integer;
   begin
      MovePencil (PromptL, RepeatC);
      Write ('R    ');
      Digit := 1;
      NextNumber := 0;
      while Ch in ['0'..'9'] do begin
         if Digit=3 then begin
            Warn('Repeat count too large');
            exit(GatherNumber);
         end;
         MovePencil(PromptL, RepeatC+Digit);
         Write(Ch);
         NextNumber:=NextNumber*10 + Ord(Ch) - Ord('0');
         Digit := Digit+1;
         NextChar;
      end;
      RepeatCount := NextNumber;
   end {GatherNumber};

begin  {KeyThumb}
   TColumn := ScreenFDot;
   LastChar := ' ';
   repeat
      MovePencil (ThumbL, TColumn);
      Write('X');
      NewColumn := TColumn;
      NextChar;
      RepeatCount := 1;
      while Ch in ['0'..'9'] do GatherNumber;
      case RawCh of 
      ' ':             NewColumn := TColumn+RepeatCount;
      BS1, BS2, BS3:   NewColumn := TColumn-RepeatCount;
      TAB:             NewColumn := TColumn+RepeatCount*TABLength;
      CtlTAB:          NewColumn := TColumn-RepeatCount*TABLength;
      CR:              NewColumn := EofDot;
      CtlCR:           NewColumn := 0;
      'S', 's':        NewColumn := SelectDot;
      'T', 't':        NewColumn := ScreenFDot;
      'B', 'b':        NewColumn := ScreenLDot;
      'O', 'o':        NewColumn := OldDot;
      'N', 'n':        NewColumn := NoteDot;
      end;
      if RawCh in [CR,'S','s','B','b','O','o','N','n'] then
         LastChar := RawCh
      else if RawCh in [' ',BS1,BS2,BS3,TAB,CtlTAB,CtlCR,'T','t'] then
         LastChar := ' ';
      if NewColumn<0 then NewColumn := 0
      else if NewColumn > EofDot then NewColumn := EofDot;
      MovePencil (ThumbL, TColumn);
      Write(' ');
      TColumn := NewColumn;      
      DrawThumbBar;
   until Accept or Reject;
   KeyThumb := False;
   if Accept and (TColumn<>ScreenFDot) then begin
      PrevOFirst := OldFirst;
      PrevODot :=   OldDot;
      if (TColumn<ScreenFDot) or (TColumn>ScreenLDot) then begin
         OldFirst := ScreenFirst;
         OldDot :=   ScreenFDot;
      end;
      case LastChar of 
      CR:                                  Show (FilledLast, 0, LastLine);
      'S', 's':                            Show (SelectFirst,0,LastLine div 2); 
      'O', 'o':                            Draw (PrevOFirst, FilledLast, 0, -1);
      'N', 'n':                            Draw (NoteFirst, FilledLast, 0, -1);
      'B', 'b':                            ScrollUp (0, LastLine, 37); 
      otherwise:
         if TColumn = EofDot then          Show (FilledLast, 0, LastLine)
         else if Tcolumn = 0 then          begin
                                              Add(FilledFirst,2,tmp);
                                              Draw(tmp, FilledLast,0,-1);
                                           end
         else if TColumn = SelectDot then  Show (SelectFirst,0,LastLine div 2)
         else if Tcolumn = ScreenLDot then ScrollUp (0, LastLine, 37)
         else if TColumn = PrevODot then   Draw (PrevOFirst, FilledLast, 0, -1)
         else if TColumn = NoteDot then    Draw (NoteFirst, FilledLast, 0, -1)
         else                              begin
                                              KeyThumb := True;
                                              ThisColumn := TColumn;
                                           end;
      end;
   end;
end {KeyThumb};



procedure GotoChar(Auto:boolean);
var
   WantCh: char; {WantCh is the character we are looking for.}
   PrevPos: Position;  {Position where last one was found.}
begin
   if Auto then
      WantCh := OldGChar
   else begin
      Prompt(concat('Go to.  Enter character', CmdChar));
      MovePencil (PromptL, 22);
      NextChar;
      if not Quoted and((RawCh=Reject1)or(RawCh=Reject2)or(RawCh=Reject3)) 
            then
         exit(GotoChar);
      WantCh := Ch;   
      OldGChar := WantCh;
   end;
   AllChars[WantCh] := False;
   
   {Note:  AllChars must be fixed if the loops are abnormally exited.}
   
   if Direction ='>' then begin
      if GT(SelectLast, ScreenLast) or LT(SelectLast, ScreenFirst) then
         Attach (Cursor1, ScreenFirst, ReadCursor)
      else 
         Attach (Cursor1, SelectLast, ReadCursor);
      Count := 0;
      repeat
         repeat
            Add1C(Cursor1)
         until not AllChars[Cursor1.Ch] or GE(Cursor1.Pos, ScreenLast);
         PrevPos := Cursor1.Pos;
         Count := Count+1;
      until (Count >= RepeatCount)  or  GE(Cursor1.Pos, ScreenLast);
   end
   else begin {Direction = '<'}      
      if GT(SelectFirst, ScreenLast) or LT(SelectFirst, ScreenFirst) then
         Attach (Cursor1, ScreenLast, ReadCursor)
      else 
         Attach (Cursor1, SelectFirst, ReadCursor);
      Count := 0;
      repeat
         repeat
            Sub1C(Cursor1)
         until not AllChars[Cursor1.Ch] or LE(Cursor1.Pos, ScreenFirst);
         PrevPos := Cursor1.Pos;
         Count := Count+1;
      until (Count >= RepeatCount)  or  LE(Cursor1.Pos, ScreenFirst);
   end;
   AllChars[WantCh] := True;
   if (WantCh=Cursor1.Ch) or (Count>1) then begin
      if not Moreing then
         UnderLine(SelectFirst, SelectLast, White); 
      NewFirst := PrevPos;
      NewLast := NewFirst;
      if Cursor1.Ch=WantCh then begin
         if Count=RepeatCount then Count := 0;
      end else
         Count := Count-1;
      Detach (Cursor1);
      CRofCRLF(NewFirst);
      ReFocus(Direction);
      ScreenPos(SelectFirst, ThisLine, ThisColumn);
      ThisSelect := SelectChar;
   end
   else begin 
      Detach (Cursor1);
      write (Bel);
      Count := Count-1;
   end;
end  {GotoChar}.
