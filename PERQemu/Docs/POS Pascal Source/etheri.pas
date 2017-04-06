{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module EtherInterrupt;

{----------------------------------------------------------------------------
{
{ Abstract:
{   This module provides the interrupt service for the 10 MBaud ethernet.
{
{ Written by: Don Scelza.
{
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1983
{
{---------------------------------------------------------------------------}

{--------------------------------------------------------------------------
{
{ Change History:
{
{  3 Mar 83     Version 1.5     Sandeep Johar
{       Decrease the window from the interrupt enable to the raise
{       of E10ReceiveDone exception.
{
{  1 Dec 82     Version 1.4     S Johar
{       Raise ReceiveDone exceptions only if the user has expressly
{       enabled them. This works only in systems f.2 and later
{       corresponding to the Z80 rewrite.
{
{ 05 Oct 82     Version 1.3     J Strait
{       Raise an exception for each receive recognized by the E10Srv routine.
{
{ 09 Jun 82     Version 1.2     Don Scelza
{       Changed the "if RListHead <>" to "while RListHead <>"
{       This will take care of two receives done before the
{       Pascal level interrupt is handled.
{
{ 17 Nov 81     Version 1.1     Don Scelza
{       Added a raise of E10ReceiveDone if a receive completed.
{       This was done to help with the implementation of IP.
{
{ 04 Nov 81     Version 1.0     Don Scelza
{       Created the module.  Took the Pop, Push and interrupt routines
{       form Ether10IO
{
{---------------------------------------------------------------------------}


{********************} Exports {********************}


imports Ether10IO from Ether10IO;

var
    StackPointer: Integer;
    DCBStack: array[1..NumDCBs] of pEtherDCB;
    RListHead, RListTail, SListHead: pEtherDCB;
    SendsPosted, RecvsPosted: Integer;

function PopDCB: pEtherDCB;
procedure PushDCB(Ptr: pEtherDCB);
procedure E10Srv;

{********************} Private {*******************}

Imports IO_Private From IO_Private;


function PopDCB: pEtherDCB;
{-------------------------------------------------------------------------
{
{ Abstract:
{   Get the next free DCB from the stack.
{
{ Results:
{   Return a pointer to the next free DCB.
{
{ Side Effects:
{   Move the stack pointer.
{
{-------------------------------------------------------------------------}
    begin
    PopDCB := DCBStack[StackPointer];
    StackPointer := StackPointer - 1;
    end;


procedure PushDCB(Ptr: pEtherDCB);
{-------------------------------------------------------------------------
{
{ Abstract:
{   Push a free DCB onto the DCB stack.
{
{ Parameters:
{   Ptr is a pointer to the DCB that is to be pushed onto the stack.
{
{ Side Effects:
{   Move the stack pointer.
{
{-------------------------------------------------------------------------}
    begin
    StackPointer := StackPointer + 1;
    DCBStack[StackPointer] := Ptr;
    end;
    

procedure E10Srv;
{------------------------------------------------------------------------
{
{ Abstract:
{   This is the 10 megabuad Ethernet interrupt routine.
{
{ Exceptions:
{   This procedure will raise E10ReceiveDone if a receive has completed.
{
{-------------------------------------------------------------------------}
  label 1;
  var DCBPtr: pEtherDCB;
      RecvStat: pEtherStatus;

    begin
    if SListHead <> nil then
        begin
        if not SListHead^.StatPtr^.CmdInProgress then
            begin
            PushDCB(SListHead);
            SListHead := nil;
            SendsPosted := SendsPosted - 1;
            end;
        end;
    while RListHead <> nil do
        begin
        if not RListHead^.StatPtr^.CmdInProgress then
          begin
            RecvStat := RListHead^.StatPtr;     
            DCBPtr := RListHead;
            RListHead := RListHead^.NextDCB;
            PushDCB(DCBPtr);
            RecvsPosted := RecvsPosted - 1;
            If RaiseException[Ether10].DataAvailable Then
              Begin
                inlinebyte({INTON} 106);        { Enable interrupts. }
                raise E10ReceiveDone(RecvStat);
                inlinebyte({INTOFF} 105);       { Disable interrupts. }
              End;
          end
        else
            goto 1;
        end;
1:  inlinebyte({INTON} 106);        { Enable interrupts. }
    end.
