{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOKeyboard;
{--------------------------------------------------------------------------
{
{ IOKeyboard - Keyboard IO routines.
{ 
{ Copyright (C) 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{       IOKeyboard exports procedures to perform IO on the keyboard.
{
{-------------------------------------------------------------------------}



{ $Version V0.5 for POS}
{--------------------------------------------------------------------------
{ Change Log:
{
{ 28 Oct 83  V0.5 Dirk Kalp
{            Make Key_ReadChar only change its parameter if it has a 
{            character to return.
{
{ 01 Apr 83  V0.4 J Strait.
{            Raise the new HardCopy exception in response to Control-
{            Shift-P.
{
{ 29 Mar 83  V0.3 Sandeep Johar
{            Added the Key_Tlate routine. Use it to translate the TransKey
{            codes.
{
{ 06 Jan 83  V0.2 August G. Reinig
{            Changed calls to SegDDS.
{
{ 15 Dec 82  V0.1 August G. Reinig
{            Moved circular buffer initialization into here.
{
{ 27 Oct 82  V0.0 C.Beckett 
{            Created module.
{
{-------------------------------------------------------------------------}

{*******************************}   Exports   {*****************************}

Imports IO_Private from IO_Private;
        
Const
        CtrlC = chr(#3);
        CtrlS = chr(#23);
        CtrlQ = chr(#21);
        BlamCh = Chr(#303);      { untranslated shift-control-C }
        DumpCh = Chr(#304);      { untranslated shift-control-D }
  
var
  KTBuf : CirBufPtr;  { Keyboard translated buffer }

procedure Key_Initialize;

Function Key_ReadChar( Unit : UnitRng; var Ch: char): integer;

{ disable/enable keyboard interrupts }
Procedure Key_Disable( var OldKeyEnable: Boolean );  
Procedure Key_Enable( OldKeyEnable: Boolean );

Procedure Key_Clear;        { clear the IO type-ahead buffer }

Procedure Key_Interrupt;

Function Key_TLate(Ch : Char): Char;

{***************************}   Private   {*****************************}

const debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} Imports System from System;
{$endc}

Imports IOErrors from IOErrors;
Imports Except from Except;

{ IO_Private defines a variable, KeyEnable, which when true, indicates that
  the interrupt routine is processing characters typed at the keyboard.
  If false, characters from the keyboard are ignored.  (They get queued until
  the Keyboard is enabled. }

var
  OutStanding : boolean;    { tells use if Ack Received, not used right now }
  KRBuf : CirBufPtr;  { Keyboard Raw Buffer, we keep a copy of this }
                      { pointer so we don't have to recompute }
                      { pUDevTab^[Keyboard].pCirBuf all the time }

procedure Key_Initialize;
{---------------------------------------------------------------------------
{ Abstract:
{
{      Keyboard initialization logic.
{---------------------------------------------------------------------------}

const on = 255;

var 
    KeyBdStatus : pKeyStat;
    InitMsg : Z_MsgPtrKludge;
  

begin       {KeyBoard:  ============================================}
  
  SetDDS(381);
  New( IOSegNum, 4, KeyBdStatus ); { create then build keyboard status }
  pUDevTab^[Keyboard].pStatus :=  recast (KeyBdStatus, pointer);
  KeyBdStatus^.OnOff    := 0;      { no one uses this yet, but we create it }
  KeyBdStatus^.Overflow := 0;
  
  SetDDS(382);
  new( IOSegNum, 4, KRBuf );       { create raw keyboard circular buffer }
  pUDevTab^[Keyboard].pCirBuf := KRBuf;
  KrBuf^.Length := CirBufSize;     { initialize it }
  KrBuf^.RdPtr := 0;
  KrBuf^.WrPtr := 0;

  new( IOSegNum, 4, KTBuf );       { create translated circular buffer }
  KTBuf^ := KRBuf^;                { initialize it }

  KeyEnabled := false;
  CtrlSPending := false;
  OutStanding := false;

  SetDDS(383);                        
  Loadexpr ( shift( 1, Dev_DataAvailable ) ); { only interrupts we accept }
  Loadexpr ( Keyboard );      { are data available, if you change this,  }
  StartIO (EP_SetEnableMask); { change KeyEnabled and KeyDisable }
  
  Key_Enable ( true );        { keyboard is now ready for action }

end { Key_Init };

Function Key_TLate(Ch : Char): Char;
{----------------------------------------------------------------
{
{ Abstract:
{      Translate a raw key board character.
{
{ Parameters:
{       Ch-  The character to be translated.
{
{ Returns:
{       A valid ascii (less than #200) character.
{
{----------------------------------------------------------------}
Var C : Char;
Begin
  C := Ch;
  If Ord(Ch) > #200 Then
    Case Ord(Ch) Of
      140 : c := Chr(#15);
      146 : c := ',';
      147 : c := '-';
      148 : c := '.';
      150 : c := '0';
      151 : c := '1';
      152 : c := '2';
      153 : c := '3';
      154 : c := '4';
      156 : c := '5';
      157 : c := '6';
      158 : c := '7';
      159 : c := '8';
      160 : c := '9';
      #204: c := Chr(#366);        { 84 => F6  Also PF1 }
      #205: c := Chr(#367);        { 85 => F7  Also PF2 }
      #206: c := Chr(#370);        { 86 => F8  Also PF3 }
      #213: c := Chr(#371);        { 87 => F9  Also PF4 }
      #200: c := Chr(#361);        { 80 => F1  Also Up Arrow }
      #201: c := Chr(#362);        { 81 => F2  Also Down Arrow }
      #202: c := Chr(#363);        { 82 => F3  Also Left Arrow }
      #203: c := Chr(#364);        { 83 => f4  Also Right Arrow }
      otherwise : c:=Chr( LAnd(ord(c),#37) );
    End;
  Key_TLate := C;
End;



Function Key_ReadChar( Unit : UnitRng; var Ch: char): integer;
{---------------------------------------------------------------------------
{ Abstract:
{
{      Reads a character from keyboard and returns a completion
{      or error code.
{
{ Parameters:
{
{      Ch - character to read.
{
{ Returns:
{
{      A condition code as defined in the module IOErrors.
{---------------------------------------------------------------------------}
var result, i :integer;
begin

  {$ifc debug then}
  LoadAdr(KTBuf);
  {$elsec}
  LoadAdr(KTBuf^);
  {$endc}
  StartIO (EP_GetCircBuffer);
  StorExpr(Result);
  StorExpr(i);

  if result = 0 
  then Key_ReadChar := IOEIOB   { IO Busy - i.e., no char avail }  
  else begin                    { got a character }
    Ch := chr (land(i,255));
    Key_ReadChar := IOEIOC;     { IO Complete } 
    If Unit = TransKey Then Ch := Key_TLate(Ch);
    end;

end { Key_ReadChar };

Procedure Key_Interrupt;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Key_Interrupt processes KeyBoard interrupts by copying characters from
{       the KeyBoard buffer into the (misnamed) translated keyboard buffer
{       (KTBuf).  Control-C, Control-Shift-C, Control-Shift-D, Control-S,
{       HELP and Control-Q are processed also.
{
{-----------------------------------------------------------------------------}

  CONST HELP = Chr(7); {Raw help key code}
        RawCtrlC = #343;
        RawCtrlS = #363;
        RawCtrlQ = #361;
        RawCtrlShS = #323;
        RawCtrlShQ = #321;
        RawCtrlShC  = #303;
        RawCtrlShD  = #304;
        RawCtrlShP = #320;
        NoScroll = #14;
        CtrlNOScroll = #16;
  var Ch, C: Char; 
      ReadChar, WriteChar, i, IntrExpr: Integer;
      OldKeyEnable: Boolean;
      HoldIntrCause, HoldAtnCause : Z_CmdRegister;
      helpStr: Sys9s;
label 1;
{ Keyboard Interrupt Service }
begin

  LoadExpr( Keyboard);  { Determine the cause of interrupt and/or attention. }
  StartIO ( EP_ReadCause );
  StorExpr( HoldAtnCause.number );
  StorExpr( HoldIntrCause.number );
   
  if HoldIntrCause.Bits[Dev_AckReceived] then OutStanding := false;    

  if HoldIntrCause.Bits[Dev_DataAvailable] 
  then repeat 
        Loadexpr(Keyboard);
        StartIO(EP_GetChar);
        StorExpr(ReadChar);
        StorExpr(IntrExpr);
        Ch := chr(Land(255, IntrExpr));

        if ReadChar <> 0  then begin { got a character }

            if Ch = HELP 
            then begin
              {$R-} helpStr[0] := chr(0); {$R=}
              InLineByte({INTON} 106);
              raise HelpKey(helpStr);
              InLineByte({INTOFF} 105);
              for i := 1 to length(helpStr) do begin
                {$ifc debug 
                then}    LoadAdr(KTBuf);
                {$elsec} LoadAdr(KTBuf^);
                {$endc} 
                LoadExpr(ord( helpStr[i]) );
                StartIO(EP_PutCircBuffer);
                StorExpr(WriteChar);
                if writeChar = 0 then goto 1;
                end;{do begin}
              end
            else begin {if Ch <> HELP}
              {$ifc debug 
              then}    LoadAdr(KTBuf);
              {$elsec} LoadAdr(KTBuf^);
              {$endc} 
              LoadExpr(ord(Ch));
              StartIO(EP_PutCircBuffer);
              StorExpr(WriteChar);
              end;

            if Ord(Ch) <> RawCtrlC Then CtrlCPending := False;

            Case Ord(Ch) Of
                  RawCtrlS, RawCtrlShS: CtrlSPending := True;
                  RawCtrlQ, RawCtrlShQ: CtrlSPending := False;
                  RawCtrlC:
                      if CtrlCPending Then 
                        begin
                          KTBuf^.RdPtr := KTBuf^.WrPtr;
                          InLineByte({INTON} 106);
                          raise CtlCAbort;
                          InLineByte({INTOFF} 105)
                        end
                      else
                        begin { not ctrlcpending }
                          InLineByte({INTON} 106);
                          raise CtlC;
                          InLineByte({INTOFF} 105)
                        end;

                  NoScroll,
                  CtrlNoScroll: CtrlSPending := Not CtrlSPending;

                  RawCtrlShC  : Begin
                              KtBuf^.RdPtr := KTBuf^.WrPtr;
                              InLineByte( {INTON} 106);
                              Raise CtlShftC;
                              InLineByte( {INTOFF} 105);
                            End;
                  RawCtrlShD  : Begin
                              InLineByte( {INTON} 106 );
                              Raise Dump('Control-Shift-D dump');
                              InLineByte( {INTOFF} 105 );
                            End;
                  RawCtrlShP: begin
                                InLineByte( {INTON} 106 );
                                raise HardCopy;
                                InLineByte( {INOFF} 105 )
                              end;
                  OtherWise: Begin  End;
            End;     {Case }

1:      If (WriteChar = 0) And Not IOInProgress Then 
          Begin
            Key_Disable(OldKeyEnable);
            InLineByte({INTON} 106);
            IOBeep;  
            InLineByte({INTOFF} 105);
            Key_Enable(OldKeyEnable)
            end
          end; { ReadChar <> 0 }

    until ReadChar = 0;

InLineByte({INTON} 106)         { enable further interrupts }

end; { Key_Interrupt }

procedure Key_Disable( var OldKeyEnable: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Key_Disable is used to disable keyboard interrupts.  This is used
{       to delay processing of control-c, control-shift-c and control-shift-d
{       at critical times.  The old value of the keyboard interrupt enable
{       is returned and must be passed back to  Key_Enable when re-enabling
{       keyboard interrupts.  Characters typed while  keyboard interrupts are
{       disabled are remembered.  When keyboard interrupts are re-enabled,
{       the characters are processed.
{
{ Parameters:
{       OldKeyEnable - set to the old value of the enable.
{
{-----------------------------------------------------------------------------}

begin { Key_Disable }

  OldKeyEnable := KeyEnabled;
                      
  if KeyEnabled then begin      { if keyboard is on, turn if off }
    loadexpr( 0 );             { don't accept data available interrupts }
    loadexpr( Keyboard );      { (right now that's the only one we ever }
    startio( EP_SetEnableMask ); { accept }
    KeyEnabled := false; 
    end;
     
end { Key_Disable };

procedure Key_Enable( OldKeyEnable: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Key_Enable is used to enable keyboard interrupts.  The old value of
{       the keyboard interrupt enable (as returned from Key_Disable) must
{       be passed to Key_Enable when re-enabling keyboard interrupts.  If
{       characters were typed while keyboard interrupts were enabled,
{       Key_Enable calls Key_Interrupt to process those characters.  The master
{       interrupt control (INTON and INTOFF QCodes) must be on when this
{       procedure is called.
{
{ Parameters:
{       OldKeyEnable - the old value of the enable.
{
{-----------------------------------------------------------------------------}

begin { Key_Enable }
  
  if OldKeyEnable then begin         { if we are to turn on the keyboard }
    Loadexpr ( shift( 1, Dev_DataAvailable ) );
    Loadexpr ( Keyboard );      { allow data available interrupts once again }
    StartIO (EP_SetEnableMask); { if we accepted others, they would be set as }
    KeyEnabled := true;          { well }
    end;
  
end { Key_Enable };

procedure Key_Clear;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Key_Clear clears the keyboard type-ahead buffer.
{
{-----------------------------------------------------------------------------}

begin { Key_Clear }

  KTBuf^.RdPtr := KTBuf^.WrPtr
{end  Key_Clear }
end { IOKeyboard }.
