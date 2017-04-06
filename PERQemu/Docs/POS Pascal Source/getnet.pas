{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module GetNetAddr;

{--------------------------------------------------------------------
{
{ Abstract:
{   This module is used to get the network address of a machine given
{   that machines name.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983
{
{-----------------------------------------------------------------------}


{----------------------------------------------------------------------
{
{ Change History:
{
{ 25 Nov 81     V1.0    Don Scelza
{   Created the module.
{
{-----------------------------------------------------------------------}



{********************} Exports {********************}

imports Ether10IO from Ether10IO;

procedure NetAddrInit;
function NetAddrLookUp(Name: String;  var Addr: EtherAddress): boolean;

{********************} Private {********************}

var Inited: boolean;
    F: text;
    InLine, SDum, Broke: string;
    NumNames: integer;

const MaxNames = 10;

type NameRecord = record
        Who: string;
        Where: EtherAddress;
        end;

var NameArray: array[1..MaxNames] of NameRecord;
    
imports Sail_String from Sail_String;
imports Stream from Stream;

const Debug = false;


procedure NetAddrInit;
{----------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to initialize the module.
{
{----------------------------------------------------------------------}
  handler ResetError(FName: PathName);
    begin
    writeln('** Could not reset ', FName);
    Inited := false;
    exit(NetAddrInit);
    end;

  var BrkOnSym, BrkOnDel: BreakTable;
      Broke, SDum: string;
      Index: integer;

    begin
    BrkOnSym := GetBreak;
    BrkOnDel := GetBreak;
    SetBreak(BrkOnDel, ' ', '', [Inclusive, Skip, FoldUp]);
    SetBreak(BrkOnSym, ' ', '', [Exclusive, Retain, FoldUp]);
    reset(f, '>Ethernet.Names');
    Index := 0;
    while not EOF(f) do
        begin
        Index := Index + 1;
        readln(F, InLine);
        {$ifc Debug then} writeln('Addr init: ', InLine); {$endc}
        SDum := Scan(InLine, BrkOnSym, Broke);
        SDum := Scan(InLine, BrkOnDel, Broke);
        NameArray[Index].Who := SDum;
        SDum := Scan(InLine, BrkOnSym, Broke);
        SDum := Scan(InLine, BrkOnDel, Broke);
        NameArray[Index].Where.High := Cvd(SDum);
        SDum := Scan(InLine, BrkOnSym, Broke);
        SDum := Scan(InLine, BrkOnDel, Broke);
        NameArray[Index].Where.Mid := Cvd(SDum);
        SDum := Scan(InLine, BrkOnSym, Broke);
        SDum := Scan(InLine, BrkOnDel, Broke);
        NameArray[Index].Where.Low := Cvd(SDum);
        end;
    NumNames := Index;
    Inited := true;
    end;

        
        
function NetAddrLookUp(Name: String;  var Addr: EtherAddress): boolean;
{--------------------------------------------------------------------
{
{ Abstract:
{   this prcoedure is used to find an address for a given name.
{
{ Parameters:
{   Name is the name that we are to find the address for.
{
{   Addr will be set to the address of Name.
{
{ Results:
{   Return true if Name was found.  False otherwise.
{
{---------------------------------------------------------------------}
  var I: integer;
  
    begin
    NetAddrLookUp := false;
    Name := CvUp(Name);
    for I := 1 to NumNames do
        begin
        {$ifc Debug then }
            writeln('Lookup ', I:1, NameArray[I].Who, '   ',
                    NameArray[I].Where.High:1, '   ',
                    NameArray[I].Where.Mid:1, '   ',
                    NameArray[I].Where.Low:1);
        {$endc}
        if NameArray[I].Who = Name then
            begin
            Addr := NameArray[I].WHere;
            NetAddrLookUp := true;
            exit(NetAddrLookUp);
            end;
        end;
    end.

  
        
        
        
        
    
