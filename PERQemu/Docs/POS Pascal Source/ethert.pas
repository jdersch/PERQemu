{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module EtherTime;

{----------------------------------------------------------------------------
{
{ Copyright (C), 1982, 1983  Three Rivers Computer Corporation.
{
{ Written by:   Mark G. Faust
{
{----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
{
{ Abstract:
{
{   This module exports procedures for getting the date and time from an
{   EtherNet time server.  The string return is compatible with standard
{   Perq time string formats (from Clock.Pas, for example):  
{
{               <dd>-<mmm>-<yy> <hh>:<mm>:<ss>
{
{   e.g.        22-Jun-82 13:05:21
{
{
{---------------------------------------------------------------------------}


{---------------------------------------------------------------------------
{
{ Change Log:
{ 
{   27 Jan 83   V1.2    Don Scelza      Added calls to allow E10RecvDone
{                                       exceptions.
{
{    1 Dec 82   V1.1    Chuck Beckett   14 Char. compiler fixes.
{
{   22 Jun 82   V1.0    Mark G. Faust   Module written.
{
{
{---------------------------------------------------------------------------}

            {****************} exports {****************}

function GetEtherTime(TimeOut :integer) :string;



            {****************} private {****************}

imports SystemDefs from SystemDefs;
imports Ether10IO from Ether10IO;
imports IO_Others from IO_Others;
imports AlignMemory from AlignMemory;
imports Memory from Memory;
imports Perq_String from Perq_String;
imports IO_Unit from IO_Unit;

const

    Version     = '1.2';
    Debug       = false;


    Request         = 1;            { Code for time server request }
    Reply           = 2;            { Code for time server reply   }
    TimeLength      = 23;           { Expect 23 bytes for time     }
    PacketLength    = MinDataBytes;

    {$INCLUDE E10Types.Pas }


type
                            
    pTimePacket = ^TimePacket;
    TimePacket  = packed record
                  ReplyOrRequest  :char;
                  Dummy           :char;
                  TimeString      :string;
                  end;


    JiffyRecord = record
                case integer of
                    1: (Dbl :Double);
                    2: (Now :long)
                end;
                

var

    OurAddress  :EtherAddress;
    OurAddrPtr  :pEtherAdRec;

    SndHeader   :pEtherHeader;
    SndBuffer   :pEtherBuffer;
    SndStatus   :pEtherStatus;

    RcvHeader   :pEtherHeader;
    RcvBuffer   :pEtherBuffer;
    RcvStatus   :pEtherStatus;

    PacketValid :boolean;       { True if we got a good packet back }
    PostAnother :boolean;       { True if we should post another receive }

    TimeCapsule :pTimePacket;
    EtherSeg    :integer;
    SegSize     :integer;
    Jiffies     :JiffyRecord;
    Deadline    :long;
    OldExcept   :boolean;


function SwapByte(Wrd: integer): integer;
{-------------------------------------------------------------
{
{ Abstract:
{   This function is used to swap the two bytes of a word.
{
{ Parameters:
{   Wrd is the word that we are to swap the bytes of.
{
{ Results:
{   Return Wrd with the bytes swapped.
{
{-----------------------------------------------------------}
begin 
SwapByte := lor(shift(Wrd, -8), shift(Wrd, 8));
end;

procedure InitializeEtherNet;
{----------------------------------------------------------------------------
{
{ Abstract:
{
{   This procedure must be called before anything else.  It initializes the
{   EtherNet and allocates all EtherNet buffers.  This code was stolen from
{   FTPUtils.
{
{
{---------------------------------------------------------------------------}
begin
NewBuffer(recast(RcvBuffer, AlignedPointer), 4, 4);
NewBuffer(recast(SndBuffer, AlignedPointer), 4, 4);
SegSize := (((WordSize(EtherHeader) + WordSize(EtherAdRec) + 
              WordSize(EtherStatus)) * 2) div 256) + 1;
CreateSegment(EtherSeg, SegSize, 1, SegSize);
new(EtherSeg, 8, SndHeader);
new(EtherSeg, 8, RcvHeader);
new(EtherSeg, 2, RcvStatus);
new(EtherSeg, 2, SndStatus);
new(EtherSeg, 1, OurAddrPtr);
SetMobility(EtherSeg, UnMovable);

SndHeader^.EType := TimeServerType;

SndHeader^.Dest.High := -1;     { Broadcast for send }
SndHeader^.Dest.Mid :=  -1;
SndHeader^.Dest.Low :=  -1;

OurAddress := E10GetAdr;
SndHeader^.Src.High :=  SwapByte(OurAddress.High);
SndHeader^.Src.Mid :=   SwapByte(OurAddress.Mid);
SndHeader^.Src.Low :=   SwapByte(OurAddress.Low);

OurAddrPtr^.LowAddress := SndHeader^.Src.Low;
OurAddrPtr^.MCB := MltCstNone;

E10Reset(OurAddrPtr);
end;

function GetEtherTime(TimeOut   :integer) :string;
{----------------------------------------------------------------------------
{
{ Abstract:
{
{   Get the current time from an EtherNet time server.  Return a Perq time
{   string in standard format.  Time out after specified number of jiffies.
{   If we time out then we return the null string.
{
{---------------------------------------------------------------------------}

    handler E10ReceiveDone(Stat :pEtherStatus);
    {------------------------------------------------------------------------
    {
    { Abstract:
    {
    {   This handler is invoked when the receive we posted completes.  We
    {   check the status and make sure we got a valid packet.  If the packet
    {   was addressed to us and contained the expected reply then we return
    {   after setting packet valid.  Otherwise we set a boolean, indicating
    {   that once we leave the handler, we should post another receive.
    {
    {-----------------------------------------------------------------------}
    begin
    TimeCapsule := recast(RcvBuffer,pTimePacket);
    if Stat^.CRCError then
        begin
        if Debug then
            writeln('GetEtherTime: Bogus reply packet (Bad CRC)');
        PostAnother := True;
        end 
    else
        if TimeCapsule^.ReplyOrRequest <> chr(Reply) then
            begin
            if Debug then
                begin
                writeln('GetEtherTime: Bogus reply packet (Bad Reply/Req)');
                writeln('Was: ',ord(TimeCapsule^.ReplyOrRequest));
                end;
            PostAnother := True;
            end
    else
        if RcvHeader^.EType <> TimeServerType then
            begin
            if Debug then
                writeln('GetEtherTime: Bogus reply packet (Bad type) ');
            PostAnother := True;
            end
    else
        if ((Stat^.BitsRecv div 8) - 18) <> PacketLength then
            begin
            if Debug then
                begin
                writeln('GetEtherTime: Bogus reply packet (Bad bit count)');
                writeln('Got: ',((Stat^.BitsRecv div 8) - 18),' bytes');
                end;
            PostAnother := True;
            end
    else
        PacketValid := True;
    end;


var
    i           :integer;
    TS          :string;

begin
PacketValid := False;
PostAnother := False;
GetEtherTime := '';

OldExcept := true;
IOSetException(Ether10, IODataInterrupt, OldExcept);

if Debug then
    writeln('GetEtherTime: calling InitializeEtherNet');
InitializeEtherNet;

if Debug then
    writeln('GetEtherTime: posting receive');
E10IO(EReceive,RcvHeader,RcvBuffer,RcvStatus,MaxDataBytes);

if Debug then
    writeln('GetEtherTime: Sending time request packet');

TimeCapsule := recast(SndBuffer,pTimePacket);
TimeCapsule^.ReplyOrRequest := chr(Request);
E10IO(ESend,SndHeader,SndBuffer,SndStatus,MinDataBytes);
if (SndStatus^.SendError) then
    begin
    if Debug then
        writeln('** GetEtherTime: E10IO Send Error!');
    IOSetException(Ether10, IODataInterrupt, OldExcept);
    exit(GetEtherTime);
    end;

if Debug then
    writeln('GetEtherTime: Successfuly sent time request packet');

IOGetTime(Jiffies.Dbl);
DeadLine := Jiffies.Now + TimeOut;
while Jiffies.Now <= Deadline do
    begin
    if PacketValid then
      with TimeCapsule^ do    { Remove '19' from year, and trailing .<n><n> }
        begin
        i := RevPosC(TimeString,'-');
        TS :=  SubStr(TimeString,1,i);
        TS := ConCat(TS,SubStr(TimeString,i+3,length(TimeString)));
        i := RevPosC(TS,'.');
        GetEtherTime := SubStr(TS,1,i-1);
        if Debug then
            writeln('GetEtherTime: Got valid reply');
        IOSetException(Ether10, IODataInterrupt, OldExcept);
        exit(GetEtherTime);
        end
    else
        if PostAnother then
            begin
            E10IO(EReceive,RcvHeader,RcvBuffer,RcvStatus,MaxDataBytes);
            PostAnother := False;
            end;
    IOGetTime(Jiffies.Dbl);
    end;

E10Reset(OurAddrPtr);
IOSetException(Ether10, IODataInterrupt, OldExcept);
end.
