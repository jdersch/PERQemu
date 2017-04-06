module Z80Link;


Exports

Procedure PNibble (Nib : integer);
Procedure PHexbyte(val : integer);
Procedure PHexWord(Val : integer);
Procedure PrintByte( Value : Integer);
Procedure SendWord( B : integer);
Procedure ReadWord( var X : integer);
Function  TestLink(var value : integer) : boolean;
Function  InitLink : boolean;
procedure ReadMicro(FileName: String);
procedure SendBoot;

private


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

Var     TMI: TransMicro;
        UCode: ControlStore;
        EmptyInstruction: MicroInstruction;
        FileName : String;

Procedure PNibble (Nib : integer);
   begin
     if (0 <= Nib) and (Nib <= 9) then
         write(chr(Nib + Ord('0')))
     else
        write(chr(Nib + (Ord('A') - 10)))
   end;

Procedure PHexbyte(val : integer);

{ Prints the parameter as a hex BYTE }

  begin
  PNibble( Land( Shift(val, -4), 15));
  PNibble( Land( val, 15))
  end;


Procedure PHexWord(Val : integer);

  begin
  PNibble (Land(Shift(Val, -12), 15));
  PNibble (Land(Shift(Val, -8), 15));
  PNibble (Land(Shift(Val, -4), 15));
  PNibble (Land(Val, 15));
  end;

Procedure PrintByte( Value : Integer);
{  This procedure prints the value in what might be }
{ a meaningful manner to the user: eg as a control }
{ char, as ascii etc...}

  begin { PrintByte }
   if Value = SOM then
     begin
       WriteLn;
       Write('SOM')
     end
   else if (32 <= Value) and (Value <= ord('z')) then
     write('"', chr(value), '"')
   else PHexByte(value);
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
   While Timer > 0
   do begin
      LoadExpr( Lor( Shift(LinkRcv, 8), Shift(LinkRcv, -8)));
      InlineByte(#277);
      StorExpr(Success);
      if Success
      then
         begin
         StorExpr(val);
         Timer := 0; { force a loop exit }
         end;
      Timer := Timer - 1;
      end;
   if Success
   then X := Val
   else writeln(' Timed out reading a word ');
end;

Function TestLink(var value : integer) : boolean;
{ This routine checks for link activity, and returns the value if any}

  Var Success: Boolean;
      B : integer;

  Begin
   LoadExpr( Lor( Shift(LinkRcv, 8), Shift(LinkRcv, -8)));
   InlineByte(#277 {JCS});
   StorExpr(Success);
   if Success
   then begin
        StorExpr(B);
        end;
   TestLink := success;
   value := B;
  end;


Function InitLink : boolean;
  { initialize vars to handle physical link IO }
  var LinkBin: MicroFile;
      Success: boolean;
  begin
   Reset(LinkBin,'Link.Bin');
   LoadControlStore(LinkBin);
   LoadExpr(LOr(Shift(LinkInit,8),Shift(LinkInit,-8)));
   InLineByte( #277 {JCS} );
   StorExpr(Success);
   Initlink := success;
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
   end;
 end { CreateUCode };

 
procedure ReadMicro(FileName: String);
 { loads user microcode files }
   var f: MicroFile; I, J, UAddr: Integer;
 begin { ReadMicro }

 for I := 0 to MaxUAddr div UArraySize do UCode[I] := nil;
 
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
 writeln('  ',FileName,' Read');
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
     end;
 end { Translate };
 
procedure SendBoot;

  var I: Integer;

  begin
   if UCode[EndKrnl div UArraySize] = nil
   then
     WriteLn('Can''t find the Krnl.')
   else
     begin
     SendWord(Hello);
     for I := EndKrnl downto BeginKrnl do
      begin
       Translate(UCode[I div UArraySize]^[I mod UArraySize],TMI);
       SendWord(LNot(TMI.Word1));
       SendWord(LNot(TMI.Word2));
       SendWord(LNot(TMI.Word3))
      end;
     ReadWord(I);
     if I = Hello
     then Writeln(' Bootstrap successful.')
     else Writeln(' Bootstrap unsuccessful.');
     end;
  end { SendBoot }.

