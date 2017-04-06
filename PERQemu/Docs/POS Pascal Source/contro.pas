{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module ControlStore;
{-----------------------------------------------------------------------------
{
{ ControlStore - Load and call routines in the PERQ control-store.
{ J. P. Strait   ca. July 80.
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1983.
{
{ Abstract:
{        The ControlStore module exports types defining the format of PERQ
{ micro-instructions and procedures to load and call routines in the
{ control-store.
{
{-----------------------------------------------------------------------------}

{-------------------------------------
{ Change Log:
{
{ 28 Feb 83  V1.3 Sandeep Johar
{    Add the exception WCSSizeError and checking to ensure that the address
{    is legal on the hardware. Add 1983 to copyright.
{
{ 9 Dec 81  V1.2  WJHansen
{    Remove close comment bracket from inside comment
{
{ 3 Jun 81  V1.1  John Strait.
{ Add Virgil headers and comments.
{ }

{ ca. Jul 81  V1.0  John Strait.
{ Started file.
{ }
{-----------------------------------------------------------------------------}

exports


type MicroInstruction = { The format of a micro-instruction as produced by
                          the micro-assembler. }
       packed record case integer of
         0: (Word1: integer;
             Word2: integer;
             Word3: integer);
         1: (Jmp: 0..15;
             Cnd: 0..15;
             Z:   0..255;
             SF:  0..15;
             F:   0..3;
             ALU: 0..15;
             H:   0..1;
             W:   0..1;
             B:   0..1;
             A:   0..7;
             Y:   0..255;
             X:   0..255);
         2: (JmpCnd: 0..255;
             Fill1:  0..255;
             SFF:    0..63;
             ALU0:   0..1;
             ALU1:   0..1;
             ALU23:  0..3)
         end;
      
     MicroBinary = { The format of a micro-instruction and its address as
                     produced by the micro-assembler. }
       record
         Adrs: integer;
         MI:   MicroInstruction
         end;
      
     TransMicro = { The format of a micro-instruction as needed by the WCS
                    QCode. }
       packed record case integer of
         0: (Word1: integer;
             Word2: integer;
             Word3: integer);
         1: (ALU23: 0..3;
             ALU0:  0..1;
             W:     0..1;
             ALU1:  0..1;
             A:     0..7;
             Z:     0..255;
             SFF:   0..63;
             H:     0..1;
             B:     0..1;
             JmpCnd:0..255)
         end;
      
     MicroFile = file of MicroBinary;   { A file of micro-instructions. }
     
     
 procedure LoadControlStore( var F: MicroFile );
 procedure LoadMicroInstruction( Adrs: integer; MI: MicroInstruction );
 procedure JumpControlStore( Adrs: integer );
 
 exception WCSSizeError;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       A WCS operation with address greater than 4K was attempted on
{       a system running with only 4K writable control store.
{
{        *****WARNING ******
{
{        This exception is raised by PASCAL so users using InLineByte do not
{        enjoy this protection.
{
{ Resume:
{       Allowed. The address gets truncated to 12 bits.
{
{----------------------------------------------------------------------------}
 
private

Imports Configuration From Configuration;


 procedure LoadControlStore( var F: MicroFile );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Loads the contents of a MicroFile into the PERQ control-store.
{       The file whould be opened (with Reset) before calling LoadControlStore.
{       It is read to EOF but not closed; thus it should be closed after
{       calling LoadControlStore.
{
{ Parameters:
{       F - The MicroFile that contains the micro-instructions to be loaded.
{
{-----------------------------------------------------------------------------}

 var TLate: TransMicro;
 begin { LoadControlStore }
  while not Eof(F) and (F^.Adrs >= 0) do with F^, TLate do
   begin 
    If (Cf_WCSSize <> 1) And (F^.Adrs > #7777) Then Raise WCSSizeError;
    ALU23 := MI.ALU23;
    ALU0 := MI.ALU0;
    W := MI.W;
    ALU1 := MI.ALU1;
    A := MI.A;
    Z := MI.Z;
    SFF := MI.SFF;
    H := MI.H;
    B := MI.B;
    JmpCnd := MI.JmpCnd;
    LoadExpr(Word1);
    LoadExpr(Word2);
    LoadExpr(MI.Word3);
    LoadExpr(Lor(Shift(Adrs,8),Shift(Adrs,-8)));
    InLineByte( #276 {WCS} );
    Get(F)
   end
 end { LoadControlStore };
 
 
 procedure LoadMicroInstruction( Adrs: integer; MI: MicroInstruction );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Loads a single micro-instruction into the PERQ control-store.
{
{ Parameters:
{       Adrs - The control store address to be loaded.
{       MI   - The micro-instruction to be loaded.
{
{-----------------------------------------------------------------------------}

 var TLate: TransMicro;
 begin { LoadMicroInstruction }
  with TLate do
   begin     
    If (Cf_WCSSize <> 1) And (Adrs > #7777) Then Raise WCSSizeError;
    ALU23 := MI.ALU23;
    ALU0 := MI.ALU0;
    W := MI.W;
    ALU1 := MI.ALU1;
    A := MI.A;
    Z := MI.Z;
    SFF := MI.SFF;
    H := MI.H;
    B := MI.B;
    JmpCnd := MI.JmpCnd;
    LoadExpr(Word1);
    LoadExpr(Word2);
    LoadExpr(MI.Word3);
    LoadExpr(Lor(Shift(Adrs,8),Shift(Adrs,-8)));
    InLineByte( #276 {WCS} )
   end
 end { LoadMicroInstruction };
 
 
 procedure JumpControlStore( Adrs: integer );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Transfers control of the PERQ micro engine to a particular address
{       in the control-store.
{
{       Note 1:  Values may not be loaded onto the expression stack before
{       calling JumpControlStore.  If you wish to pass values through the
{       expression stack, the following code whould be used rather than
{       calling LoadControlStore.
{
{            LoadExpr( LOr( Shift(Adrs,8), Shift(Adrs,-8) ) );
{            InLineByte( #277 );       {the JCS QCode *)
{
{       Note 2: Microcode called by JumpControlStore should terminate with
{       a "NextInst(0)" microcode jump instruction.
{
{ Parameters:
{       Adrs - The address to jump to.
{
{-----------------------------------------------------------------------------}

 begin { JumpControlStore }
  If (Cf_WCSSize <> 1) And (Adrs > #7777) Then Raise WCSSizeError;
  LoadExpr(Lor(Shift(Adrs,8),Shift(Adrs,-8)));
  InLineByte( #277 {JCS} )
 end { JumpControlStore }.
 