Program Talker(input, output);

Imports IO_Unit from IO_Unit;
Imports ControlStore from ControlStore;
Imports Memory from Memory;
Imports Screen from Screen;
Imports FileSystem from FileSystem;

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

Var     I   : integer; {always comes in handy }
        KeyNib: Integer; { Previous nibble from keyboard }
        KBPending : boolean; { Whether nibble pending }

        TMI: TransMicro;

        UCode: ControlStore;
        EmptyInstruction: MicroInstruction;
        FileName : String;

Procedure LoadFont(FontName: string);

{-------------------------------------------------------------------
{
{ Abstract:
{   Loads a font file into memory.
{
{ Parameters:
{   FontName - name of font file to load.
{
{ Side effects:
{   Creates a segment for the font.
{   Updates FontP to point to the font in memory.
{
{------------------------------------------------------------------}

var
    c : integer;
    FontId : Integer;
    FontBlocks : Integer;
    FontBits : Integer;
    FontS : integer;

begin

FontID := FSLookUp(FontName, FontBlocks, FontBits);
if FontID = 0
then begin
     WriteLn('Unable to load font ',FontName:1);
     Exit(LoadFont);
     end;

CreateSegment(FontS, FontBlocks, 1, FontBlocks);
for c := 0 to FontBlocks-1 do
    FSBlkRead(FontID, c, MakePtr(FontS, c*256, PDirBlk));
FSClose(FontID, FontBlocks, FontBits);

SetFont(MakePtr(FontS, 0, FontPtr));

end; (* LoadFont *)

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
   else begin
        Phex(value);
        if (32 <= Value) and (Value <= ord('z'))
        then write('="', chr(value), '"');
        end;
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
       PrintByte(B)
   end
end;

Procedure TestKeyboard;
{ This procedure deals with keyboard IO }
{ by sampling the buffer, and storing nibbles }
Var c : char;
  Val : integer;

   begin
      if IOCRead(Transkey, C) = 1 then
      begin
         if ('0' <= C) and ('9' >= C) then
            val := ord(C) - ord('0')
         else if ('A' <= C) and (C <= 'F') then
            val := ord(C) - (ord('A') - 10)
         else if ('a' <= C) and (C <= 'f') then
            val := ord(C) - (ord('a') - 10)
         else Val := -1;
         if Val < 0 then begin
             KBPending := false;
             IOBeep
          end
         else
          begin
            write(C);
            if KBpending then begin
               Val := Lor( Shift( KeyNib, 4), Val);
               writeln('Sending ',Val);
               SendWord( Val );
               KBpending := false
            end else begin
               KBpending := true;
               KeyNib := Val
            end
         end
      end
   end;


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
     Exit(Talker)
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

Var
    WIndX : WinRange;
    OrgX, OrgY, Width, Height : Integer;
    HasTitle : boolean;

 begin
   CreateWindow(2, 0, 0, 768, 128, '');
   Write(Chr(Ord(#14)));
   WriteLn('Floppy   = 03  ReqData  = 1  Boot    = 0A  RdDel    = 13');
   WriteLn('RSA      = 04  BlkData  = 2  Reset   = 0B  WrDel    = 14');
   WriteLn('RSB      = 05  Data     = 3  Sense   = 0C  Specify  = 15');
   WriteLn('Speech   = 06  Ack      = 4  Register= 0D  Format   = 16 ');
   WriteLn('GPIB     = 07  Nak      = 5  HVI     = 0E  Recal    = 17');
   WriteLn('Keyboard = 08  Attn     = 6  HVO     = 0F  SnsDrive = 18');
   WriteLn('Clock    = 0A  Status   = 7  Read    = 10  DEOI     = 19');
   WriteLn('Pointer  = 0B  Seek     = 8  Write   = 11  Flush    = 1A');
   Write  ('Z80      = 0F  Config   = 9  RdId    = 12');
   CreateWindow(1, 0, 128, 768, 1024-128, '');
   Write(Chr(Ord(#14)));

   for I := 0 to MaxUAddr div UArraySize do UCode[I] := nil;
   InitLink;
   Write('Enter slave microcode file: ');ReadLn(FileName);
   if Length(FileName) <> 0
   then
       begin
       ReadMicro(FileName);
       SendBoot;
       end;

   KBPending := false;
   While True do begin
     testkeyboard;
     TestLink
   end
end.
