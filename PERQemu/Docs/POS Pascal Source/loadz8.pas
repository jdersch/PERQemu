{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module LoadZ80;

{---------------------------------------------------------------------}
{
{ Abstract:
{    Module to read Tektronix format load files and load them into
{    the Z80.
{
{    Copyright 1983 Three Rivers Computer Corporation
{
{---------------------------------------------------------------------}

{$Version V0.3 for POS}

{---------------------------------------------------------------------}
{
{ 15 Feb 83 V0.3  Roger Riggs
{           Major changes to speed this up.
{
{ 10 Jan 83 V0.2  August G. Reinig
{           Fixed an off by one bug in the code which sends data to the Z80.
{
{ 17 Dec 82 V0.1  August G. Reinig
{           Modified Roger Riggs' TekLoad module to call UnitIO to write
{           the binary to the Z80.
{
{ 01 Dec 82 V0.0  Roger Riggs
{           Created Module.
{
{---------------------------------------------------------------------}

{****************************}  Exports  {****************************}

Type
    TekResult = (TekOk, TekFNF, TekFMT, TekIO);

Function TekLoad(FileName : String; Var StartAddress : Integer) : TekResult;

{****************************}  Private  {****************************}

Imports FileSystem from FileSystem;
Imports IO_Unit from IO_Unit;
Imports IOErrors from IOErrors;

Type
    tByteArray = Packed Array [0..511] of 0..255;
    pByteArray = ^tByteArray;



Function TekLoad(FileName : String; Var StartAddress : Integer) : TekResult;

Var
    Addr : Integer;         { Address of current block }
    Binary : pByteArray;    { Binary data to be sent to Z80 }
    InBuffer : pByteArray;  { Input buffer for input ascii file }
    Data : tByteArray;      { data read from input file }
    i,j,k : integer;        { index }
    Blocks, Bits : Integer; { End of file Blocks, bits }
    CurrByte : Integer;     { Current offset in file }
    CurrBlock : Integer;    { Current block in file }
    LastBlock : Integer;    { Last block in file }
    LastByte : Integer;     { Last byte in file }
    FID : FileID;           { Input file }
    len : integer;          { Length of current Tek block }
    C : 0..255;             { Current character }
    CkSum : integer;        { Checksum of current TEK block }
    LogAdr : double;        { double word address for UnitIO }
    StsPtr : IOStatPtr;     { status for UnitIO call }

Handler FSnotFnd(Name : PathName);
begin
TekLoad := TekFNF;
Exit(TekLoad);
end;

Handler FSBadName(Name : PathName);
begin
TekLoad := TekFNF;
Exit(TekLoad);
end;


begin

Fid := FSLookup(FileName, Blocks, Bits);

New(0, 4, StsPtr);          { Allocate a status buffer for UnitIO }
New(0, 4, Binary);          { Allocate a 4 word aligned buffer }
New(0, 256, InBuffer);      { Allocate a 256 word aligned buffer }

LastByte := 512;            { Set to last byte in block }
LastBlock := Blocks - 1;    { Set to last block in file }
CurrByte := 512;            { Set initial byte position to force block read }
CurrBlock := -1;            { Set initial block position }

TekLoad := TekFMT;          { Assume format error }

repeat

    Len := 0;
    While Len = 0
    do  begin
        Repeat
            CurrByte := CurrByte + 1;
            if CurrByte >= LastByte
            then begin
                 CurrBlock := CurrBlock + 1;
                 If CurrBlock > LastBlock
                 Then Exit(TekLoad);{ Bad format, should not get to EOF }
    
                 If CurrBlock = LastBlock
                 Then LastByte := Bits Div 8
                 Else LastByte := 512;
    
                 FSBlkRead(FID, CurrBlock, RECAST(InBuffer, pDIRBlk));
                 CurrByte := 0;
                 end;
    
            Len := Len + 1;
            C :=  InBuffer^[CurrByte];
            Case Chr(C) of
                '0' .. '9' : Data[Len] := C - Ord('0');
                'A' .. 'Z' : Data[Len] := C - Ord('A') + 10;
                '$'        : Data[Len] := 36;
                '%'        : Data[Len] := 37;
                '.'        : Data[Len] := 38;
                '_'        : Data[Len] := 39;
                'a' .. 'z' : Data[Len] := C - Ord('a') + 40;
                Chr(10)    : Len := Len - 1;
                end;
        until C = 13;
    
        Len := Len - 1;
        Data[0] := Len;
        end;
    
    CkSum := Data[2] + Data[3] + Data[4];
    For i := 7 to Len do CkSum := CkSum + Data[i];
    
    if (Data[1] <> 37) OR
       (Data[0]-1 <> (Shift(Data[2],4) + Data[3])) OR
       (LAND(CkSum, 255) <> (Shift(Data[5],4) + Data[6]))
    then exit(TekLoad);             { Exit for bad checksum or format }

    case data[4] of
        6 : begin
            Addr := 0;
            J := 8;
            For i := 1 to data[7]
            do  begin Addr := Shift(Addr,4) + Data[J]; J := J + 1 end;
            I := (Data[0] - J) Div 2 + 1;
            K := 0;
            While J < data[0]
            do  begin
                Binary^[K] := Shift(data[J], 4) + data[J+1];
                J := J + 2;
                K := K + 1;
                end;
            LogAdr[0] := Addr;     { The Address to write to }
            UnitIO( Z80, recast( Binary, IOBufPtr )
                  , IOWriteHiVol , I , LogAdr, nil, StsPtr );
            if StsPtr^.SoftStatus <> IOEIOC 
            then begin TekLoad := TekIO ; Exit(TekLoad) end; 
            end;
        8 : begin
            Addr := 0;
            J := 8;
            For i := 1 to data[7]
            do  begin Addr := Shift(Addr,4) + Data[J]; J := J + 1 end;
            StartAddress := Addr;
            end;
        end
Until Data[4] = 8;

FSClose(FID, Blocks, Bits);

TekLoad := TekOk;
  
end.
