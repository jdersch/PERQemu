Program Z80Script(input, output);

Imports IO_Unit from IO_Unit;
Imports ControlStore from ControlStore;
Imports Memory from Memory;

Const   SOM = 170;

    { entrypoints in Link.Micro }
    LinkInit = #7400;
    LinkSnd  = #7410;
    LinkRcv  = #7420;

    Hello = #12345;      { message from Krnl to confirm the boot }
    Hey   = #54321;      { message from Krnl to announce a breakpoint }


    MaxUAddr = #37777;
    UArraySize = #1000;
    BeginKrnl = #7400;
    EndKrnl = #7777;
    SizeControlStore = (3 * UArraySize + 255) div 256;

type


    UArray = array[0..UArraySize-1] of MicroInstruction;
    pUArray = ^UArray;

    ControlStore = array[0..MaxUAddr div UArraySize] of pUArray;

    tCirBuf = packed record
        Max : Integer;
        Read : Integer;
        Write : Integer;
        Data : Array [0..4095] of Integer;
        end;
    pCirBuf = ^tCirBuf;

Var     I   : integer; {always comes in handy }
        KeyNib: Integer; { Previous nibble from keyboard }
        KBPending : boolean; { Whether nibble pending }

        TMI: TransMicro;

        UCode: ControlStore;
        EmptyInstruction: MicroInstruction;
        FileName : String;
        Buffer : pCirBuf;
        OldWrite : Integer;
        Script : Text;
        Log : Text;
        Command : String;
        C : integer;
        success : integer;
        DefFileName : String;
        DefLogName : String;

Procedure PrintByte( Value : Integer);
{  This procedure prints the value in what might be }
{ a meaningful manner to the user: eg as a control }
{ char, as ascii etc...}

Procedure Phex(val : integer);

{ Prints the parameter as a hex BYTE }

Procedure Nibble (Nib : integer);
   begin
     if (0 <= Nib) and (Nib <= 9) then
         write(chr(Nib + Ord('0')))
     else
        write(chr(Nib + (Ord('A') - 10)))
   end;

begin
   Nibble( Land( Shift(val, -4), 15));
   Nibble( Land( val, 15))
end;

begin { PrintByte }
   if Value = SOM then
     begin
       WriteLn;
       Write('SOM')
     end
   else if (32 <= Value) and (Value <= ord('z')) then
     write('"', chr(value), '"')
   else Phex(value);
   write(' ')
end;


Procedure SendWord( B : integer);
{ Sends a Word down the link... }
Var Success : boolean;
begin
   LoadExpr(B);
   LoadExpr(Lor(Shift(LinkSnd,8), Shift(LinkSnd,-8)));
   InLineByte( #277 {JCS} );
   StorExpr(Success);
   if not success then writeln('Failed to send a Word?')
end;

Procedure ReadWord( var X : integer);
{ Reads a word from the link... waits for timeout }
Var  Timer, val : integer;
     Success    : boolean;

Begin
   Timer := 30000;
   While Timer > 0 do
   begin
      LoadExpr( Lor( Shift(LinkRcv, 8), Shift(LinkRcv, -8)));
      InlineByte(#277);
      StorExpr(Success);
      if Success then begin
         StorExpr(val);
         Timer := 0; { force a loop exit }
      end;
      Timer := Timer - 1
   end;
   if Success then X := Val
   else writeln(' Timed out reading a word ')
end;

Procedure TestLink;
{ This routine checks for link activity, and }
{ prints out anything it finds there }
Var     B : integer;
   Success: Boolean;

Begin
   LoadExpr( Lor( Shift(LinkRcv, 8), Shift(LinkRcv, -8)));
   InlineByte(#277 {JCS});
   StorExpr(Success);
   if Success then begin
       StorExpr(B);
       LoadAdr(Buffer^);
       LoadExpr(B);
       Startio(#16);            { put character in circular buffer }
       StorExpr(Success);
       if NOT success then WriteLn('Buffer put error');
   end
end;

Procedure SendCommand(Command : String);
Var
    Flag : boolean;
    l : integer;
    c : char;
    Val : integer;
begin
WriteLn(log);WriteLn;
Write(Log, 'Sending    ');Write('Sending    ');
val := 0;
flag := False;
l := 1;
While l <= length(Command)
do  begin
    c := Command[l];
    if ('0' <= C) and ('9' >= C)
    then begin val := val * 16 + ord(C) - ord('0'); flag := true end
    else if ('A' <= C) and (C <= 'F')
    then begin val := val * 16 + ord(C) - (ord('A') - 10); flag := true end
    else if ('a' <= C) and (C <= 'f')
    then begin val := val * 16 + ord(C) - (ord('a') - 10); flag := true end
    else begin
         if flag
         then begin 
              Write(Log, Land(Val,255):2:-16, ' ');
              Write(Land(Val,255):2:-16, ' ');
              end;

         if C = '!'
         then begin
              While l <= Length(Command)
              do  begin
                  Write(Log, Command[l]);
                  Write(Command[l]);
                  l := l + 1;
                  end;
              end;
         if flag then begin SendWord( Val); val := 0; flag := false end;
         end;
    l := l + 1;
    end;
if flag
then begin 
     Write(Log, Land(Val,255):2:-16, ' ');
     Write(Land(Val,255):2:-16, ' ');
     SendWord(Val);
     end;

end { SendCommand };

procedure InitLink;
  { initialize vars to handle physical link IO }
  var LinkBin: MicroFile;
      Success: boolean;
  begin
   Reset(LinkBin,'Link.Bin');
   LoadControlStore(LinkBin);
   LoadExpr(LOr(Shift(LinkInit,8),Shift(LinkInit,-8)));
   InLineByte( #277 {JCS} );
   StorExpr(Success);
   if not Success then
    begin Writeln('****** unable to initialize the Link.');
     Exit(Z80Script)
    end
  end { InitLink };



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

 
 procedure ReadMicro(FileName: String);
 { loads user microcode files }
   var f: MicroFile; I, J, UAddr: Integer;
 begin { ReadMicro }
 
 reset(f,FileName);

 while f^.adrs >= 0 do
   begin
     UAddr := f^.adrs;
     I := UAddr div UArraySize;
     J := UAddr mod UArraySize;
     CreateUCode(UCode[I]);
     UCode[I]^[J]:=f^.MI;
     Get(f)
   end;
 close(f);
 writeln('  ',FileName,' Read')
 end { ReadMicro };

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
 
 procedure SendBoot;

 var I: Integer;

 begin { SendBoot }
   if UCode[EndKrnl div UArraySize] = nil then
        WriteLn('Can''t find the Krnl.')
   else begin
     SendWord(Hello);
     for I := EndKrnl downto BeginKrnl do
      begin Translate(UCode[I div UArraySize]^[I mod UArraySize],TMI);
       SendWord(LNot(TMI.Word1));
       SendWord(LNot(TMI.Word2));
       SendWord(LNot(TMI.Word3))
      end;
     ReadWord(I);
     if I = Hello then
       Writeln(' Bootstrap successful.')
     else
      Writeln(' Bootstrap unsuccessful.')
   end

 end { SendBoot };

begin
New(Buffer);
Buffer^.Max := 4095;
Buffer^.Read := 0;
Buffer^.Write := 0;

for I := 0 to MaxUAddr div UArraySize do UCode[I] := nil;
InitLink;
Write('Enter slave microcode file: ');ReadLn(FileName);
if Length(FileName) <> 0
then
    begin
    ReadMicro(FileName);
    SendBoot;
    end;
DefFileName := '';
DefLogName := '';
   
KBPending := false;
While True
do  begin
    Write('Enter Script file [', DefFileName:1,'] : ');ReadLn(FileName);
    if FileName = '' then FileName := DefFileName;
    DefFileName := FileName;
    Reset(Script, FileName);

    Write('Enter Log file [', DefLogName:1, '] : ');ReadLn(FileName);
    if FileName = '' then FileName := DefLogName;
    DefLogName := FileName;
    Rewrite(Log, FileName);

    While NOT EOF(Script)
    do  begin
        ReadLn(Script, Command);
        SendCommand(Command);
        OldWrite := -1;
        While Buffer^.Write <> OldWrite
        do  begin
            OldWrite := Buffer^.Write;
            i := 0;
            Repeat
               i := i + 1;
               TestLink; 
            until (i > 3000) or (OldWrite <> Buffer^.Write);
            end;
        LoadAdr(Buffer^);
        StartIO(#17); { get byte from circular buffer }
        StorExpr(Success);
        if Success = -1
        then begin
             WriteLn; WriteLn(Log);
             Write(Log, 'Receiving '); Write('Receiving ');
             end;
        While Success = -1
        do  begin
            StorExpr(C);
            if C = SOM
            then begin
                 Write('SOM ');
                 Write(log, 'SOM ')
                 end
            else begin
                 Write(Log, Land(C,255):2:-16, ' ');
                 Write(Land(C,255):2:-16, ' ');
                 end;
            LoadAdr(Buffer^);
            StartIO(#17); { get byte from circular buffer }
            StorExpr(Success);
            end;
        end;
    WriteLn(Log);
    Close(Log);
    WriteLn;
    Close(Script);
    end;
end.
