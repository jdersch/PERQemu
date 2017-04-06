{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Ether10IO;

{------------------------------------------------------------------------
{
{ Abstract:
{   This module provides the client interface to the 10 Mbaud Ethernet
{   microcode.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1983
{
{-------------------------------------------------------------------------}


{$Version 2.6 }
{--------------------------------------------------------------------------
{
{ Change History:
{
{ 31 May 83     Version 2.6     Sandeep Johar
{     Set the DCB bit count on receives to the negative of the number
{     bits expected. If E10IO is called with bytes set to zero
{     use -1500 times 8 for backward compatibility. The microcode 
{     puts this negative number at the hardware and the receiver is shut
{     by the hardware when the counter crosses zero. This change goes
{     with version 5.5 of CIOEther10.Micro, version 6.5 of eioether10.micro
{     and Rev F of the ethernet OIO board.
{
{  3 mar 83     Version 2.5     Sandeep Johar
{     Fix the race condition in E10Reset.  Fix the segsize calculation.
{
{ 17 Feb 83     Version 2.4     S Johar
{     Move exports of IO_Private to private part.
{
{ 31 Jan 83     Version 2.3     R Riggs
{     Corrected GetAdr to recover address from low nibble for K1
{
{ 24 Jan 83     Version 2.2     R Riggs
{     Modified GetAdr to not Mirror if running on K1
{
{ 03 Dec 82     Version 2.1     S Johar
{     Fixed comment for the exception E10ReceiveDone to state
{     that exception raised only if exception on data available bit is set.
{
{ 22 Nov 82     Version 2.0     S Johar
{   Make changes for F2 rewrite.
{     (1)  DevTab  ==> pUDevTab
{     (2)  StartIO(Ether10) ==> StartIO(EP_Ethernet)
{     (3)  CtlPtr ==> pDataCtrl
{     (4)  Import IO_Private in the PRIVATE part.
{
{ 25 Oct 82     Version 1.15    D Scelza
{   Put the V1.13 exceptions back in.
{
{ 05 Oct 82     Version 1.14    J Strait
{ 1. Turn off Pascal interrupts while posting new commands.
{ 2. Execute E10Start to the microcode when the microcode finishes the
{    last receive just as a new one is being added the the RListTail.
{ 3. Comment out the V1.13 exceptions (for F0) with (****F0****) comments.
{
{ 18 Aug 82     Version 1.13    Don Scelza
{   Added exceptions for NoNetBoard and Bytecount.
{
{ 18 May 82     Version 1.12    David Golub
{   Changed to use StartIO(Ether10) instead of JumpControlStore.
{
{ 14 Apr 82     Version 1.11    Don Scelza
{   Started to add the code to get the address from the board.
{
{ 12 Apr 82     Version 1.10    Don Scelza
{   Added the structures needed for setting address and group addressing.
{
{ 07 Apr 82     Version 1.9     Don Scelza
{   Made changes to make the driver compatible with new Net proms.
{   This will fix a bug that caused the network hardware to send out
{   one word too many in the header.
{   Changes were made to:
{       Definition of header.
{       All calculations of: 
{           Bytes from Bits      E10DataBytes
{           and Bits from Bytes  E10IO.
{
{ 28 jan 82     Version 1.8     Sandeep Johar
{   Etherheaders aligned at 8 word boundaries.
{
{ 30 dec 81     Version 1.7     Don Scelza
{   Made changes to allow for Back-off on transmit.
{ 
{ 17 Nov 81     Version 1.6     Don Scelza
{   Added the exception E10ReceiveDone.  George R. needed this to implement
{   IP/TCP
{
{ 09 Nov 81     Version 1.5     Don Scelza
{   Made some changed in the IO routine.  Changed the way that
{   DCBs are linked.
{
{ 05 Nov 81     Version 1.4      Don Scelza
{   Changed to look at the constant UseSys.
{ 
{ 04 Nov 81     Version 1.3     Don Scelza
{   Took out some code to create the module EtherInterrupt.
{
{ 02 Nov 81     Version 1.2     Don Scelza
{   Implemented the changes from the design review.
{
{ 29 Oct 81     Version 1.1     Don Scelza
{   Started to add code to the module.
{
{ 02 Oct 81     Version 1.0     Don Scelza
{   Created the module.  Took the definitions from
{   the program TestEther.
{
{-------------------------------------------------------------------------}



  
{********************} Exports {********************} 


{-----------------------------------------------------------------------
{
{ This module provides the raw I/O interface to the Three Rivers Computer
{ Ethernet system.  The procedures in this module allow the client to
{ send and receive packets on the net.
{
{ For details of the  Physical and Data Link layers of the network 
{ see the document:
{
{   The Ethernet
{   A Local Area Network
{   Data Link Layer and Physical Layer Specifications
{
{   DEC - Intel - XEROX
{
{ For details on the Three Rivers hardware interface to the network see:
{
{   Ethernet Interface Programmers Guide
{
{   Pradeep Reddy
{
{ For details on the interface presented, to this module, by the
{ Ethernet microcode see the file:
{
{   Ether10.Micro
{
{   Donald A. Scelza
{
{ Following is some general information about the client interface
{ presented by the Ethernet microcode and this module:
{
{ It is possible to always have a receive pending.  If a send command
{ is executed while a receive is pending the internal state of the
{ interface is saved in a register save area in memory.  This is
{ done by saving the VA of the DCB for the receive. After the
{ send has completed the receive state is reloaded and the receive is
{ restarted.
{ 
{ In addition to the ability to do a Receive followed by a Send, it is
{ also possible to do multiple Receives.  The Receives are linked using
{ the NextDCB field of the Ethernet DCB.  When a Receive completes the
{ next Receive in the chain is started.
{
{ Command information for the Ethernet driver is provided in an
{ Ethernet Device Control Block, DCB.  All data areas referenced by
{ pointers in the DCB as well as the DCB itself must be LOCKED in
{ memory until the request has completed.  They can NOT be moved.
{ The best way to do this is to mark the segment that the buffers are
{ allocated from as UnMovable.  This will allow the memory manager to
{ place the buffers in a convient place in memory before they are
{ locked down.
{
{ The Ethernet driver needs to have a four (4) word area of memory in which
{ it can save registers.  A pointer to this area of memory is provided
{ by the BuffPtr when a Reset command is executed.  Once the Reset
{ command has been processed this register save area can NOT be moved.
{ To change the register save area another Reset command must be
{ executed.
{
{ The Ethernet DCB must be unmovable while the command is pending. 
{
{ To wait for the completion of a command it is possible to spin on the
{ Command-In-Progress bit in the status block.  This bit will be cleared
{ when the requested command has been completed.
{
{
{ After a receive the Bits field of the status block has the number of
{ bits that were received.  To translate this into the number of data
{ bytes you must perform a number of operations.  First divide it by 8.
{ This will give the number of bytes that were received.  If the number
{ is not evenly divisable by 8 then there was a transmition error.
{ After the division you must subtract off the number of bytes in the
{ header and the CRC.  There is a total of 18 bytes in these two portions
{ of the packet.
{
{ The Ethernet controler can receive packets that are addressed in the
{ following ways:
{   
{   a) Packets addressed to this machine.
{   b) Packets addressed to any machine.
{   c) Packets addressed to five of 256 groups.
{   d) Packets addressed to any group.
{
{ The Reset command is used to set up the addressing for a given machine.
{
{------------------------------------------------------------------------}


imports SystemDefs from SystemDefs;
imports System from System;

{
{ Define the types and variables used by the Network stuff.
{ }

type
    
    {
    { These are the valid commands for the Ethernet interface.
    { }

    EtherCommand = (EReset, EReceive, EPromiscuousReceive, ESend);

    {
    { An Ethernet address is 48 bits long.  It is made up of 6
    { octets or in our case 3 words.
    { }

    EtherAddress = packed record   { An address on the net is 48 bits }
        High: integer;
        Mid:  integer;
        Low:  integer;
        end;
    
    {
    { This record defines an Ethernet status block.  The first
    { 15 bits of the block are defined by the hardware interface.
    { The 16th bit of the first word and the second word are defined by the
    { Ethernet microcode.
    {
    { Alignment:    Double word.
    { Locked:       Yes.
    { }

    EtherStatus = packed record     { The status record. }
        CRCError:      boolean;     { There was a CRC error.}
        Collision:     boolean;     { There was a collision }
        RecvTrans:     boolean;     { 0 - receive has finished.  1 for trans. }
        Busy:          boolean;     { The interface is bust. }
        UnUsed4:       boolean;
        ClockOver:     boolean;     { The microsecond clock overflowed. }
        PIP:           boolean;     { There is a Packet In Progress. }
        Carrier:       boolean;     { There is traffic on the net. }
        RetryTime:     0..15;
        LargePKt:      boolean;     { Recd a large packet }
        UnUsed13:      boolean;
        SendError:     boolean;     { Could not send packet after 16 tries. }
        CmdInProgress: boolean;     { There is a command pending. } 
        BitsRecv:      integer;     { Number of bits that were received. }
        end;
    
    {
    { This record defines the header for an Ethernet transfer.
    {
    { Alignment:    8 word.
    { Locked:       Yes.
    { }

    EtherHeader = packed record
        UnUsed:     Integer;        { A filler word.  This must be here.}
        Dest:       EtherAddress;
        Src:        EtherAddress;
        EType:      Integer;        { Type field defined by XEROX } 
        end;
 
    {
    { This record provides the definition of an EtherBuffer.
    { 
    { Alignment:    1k word.
    { Locked:       Yes.
    { }

    EtherBuffer = array [0..749] of integer; 
    
    {
    { Define all of the pointers that we need.
    { }

    pEtherStatus = ^EtherStatus;
    pEtherBuffer = ^EtherBuffer;
    pEtherHeader = ^EtherHeader; 
    pEtherDCB = ^EtherDCB; 

    {
    { This is the definition of an Ethernet Device Control Block.
    {
    { Alignment:    Quad word.
    { Locked:       Yes.
    { } 


    EtherDCB = packed record
        HeadPtr:    pEtherHeader;
        BuffPtr:    pEtherBuffer;
        StatPtr:    pEtherStatus;
        Cmd:        EtherCommand;
        BitCnt:     Integer;            { Total bits in buffer and header }
        NextDCB:    pEtherDCB;
        end;

    {
    { This is the definition that is used to create the register save
    { area.  Must exist across transfers.  
    {
    { Alignment:    Double word.
    { Locked:       Yes.
    { }
    
    EtherRegSave = record
        RecvDCB:    pEtherDCB;
        SendDcb:    pEtherDCB;
        end;
    
    pEtherRegSave = ^EtherRegSave;

    
    {
    { This is the definition of the structure that is used to set
    { the physical address of this machine and the groups that we are 
    { to look for.
    {
    { Alignment:    1 word.
    { Locked:       Yes, during Reset.
    { }
    
    EtherAdRec = packed record
        LowAddress: Integer;        { Low word of Physical address }
        MCB:        0..255;         { Mulitcast command byte. }
        MultCst1:   0..255;         { Five group addresses. }
        MultCst2:   0..255;
        MultCst3:   0..255;
        MultCst4:   0..255;
        MultCst5:   0..255;
        end;
        
    pEtherAdRec = ^EtherAdRec;


    
    {
    { Following are the definitions that are used to deal with the
    { micro-second clock.
    { }

    {
    { The microsecond clock takes a two word combined control and
    { status block.  The first word of the block gives the number
    { of microseconds to be loaded into the clock.
    { The second word provides the status information from the
    { clock.  Once a clock command has been started it is
    { possible to spin on the CmdInProgress bit in the control
    { block.  When the bit is cleared the specified number of
    { micro-seconds has elapsed.
    {
    { Alignment:    Double word.
    { Locked:       Yes.
    { }

type
    
    uSClkDCB = packed record
        uSeconds: integer;
        UnUsed0:   boolean;
        UnUsed1:   boolean;
        UnUsed2:   boolean;
        UnUsed3:   boolean;
        UnUsed4:   boolean;
        UnUsed5:   boolean;
        UnUsed6:   boolean;
        UnUsed7:   boolean;
        UnUsed8:   boolean;
        UnUsed9:   boolean;
        UnUsed10:  boolean;
        UnUsed11:  boolean;
        UnUsed12:  boolean;
        UnUsed13:  boolean;
        UnUsed14:  boolean;
        CmdInProg: boolean;
        end;

    puSClkDCB = ^uSClkDCB;   



{
{ Define the constants for the address block supplied to Three Rivers by
{ Xerox.
{
{ High 16 bits (2 octets) are 02 1C (Hex).
{ Next 8 bits (1 octet) is 7C (Hex).
{
{ The low order byte of the second PERQ word as well as the third PERQ word
{ are Three Rivers defined.  Currently the low order byte of the second word
{ is used to define the type of interface.  The valid values are:
{
{       0 - Interface is on an IO option board.
{       1 - Interface is on the IO board
{ }
 

const

    TRCCAdrMid = 31744;         { 7C hex in the high order 8 bits. }
    TRCCAdrHigh = 540;          { 02 1C hex. }
    EBoardOption = 0;           { The interface is on an I/O Option board. }
    EBoardIO = 1;               { The interface is on the I/O board. }


{
{ These are some other useful constants.
{ }

const

    MinDataBytes = 46;          { Smallest number of data bytes in a packet. }
    MaxDataBytes = 1500;        { Largest number of data bytes in a packet. }
    NumDCBs = 16;               { The number of DCBs, commands, possible at }
                                { a single time }

{
{ Define the constants for the multicast command byte.
{ }

const
    MltCstAll = 0;              { Receive all multicasts. }
    MltCstNone = #377;          { Don't receive any multicast packets. }
    MltCstAddr = #376;          { Return the Physical addr of this device. }
    MltCstGrp = 1;              { Only receive specified groups. }


{
{ These are the procedures exported by this module.
{ }

procedure E10Init;
procedure E10IO(Cmd: EtherCommand;  Header: pEtherHeader;  Buff: pEtherBuffer;
                  Stat: pEtherStatus;  Bytes: Integer);
procedure E10Wait(Stat: pEtherStatus); 
procedure E10Reset(Ptr: pEtherAdRec);
function  E10DataBytes(RecvBits: Integer): Integer;
function  E10GetAdr: EtherAddress;
procedure E10State(var NumSend, NumReceive: Integer);
procedure E10WIO(Cmd: EtherCommand;  Header: pEtherHeader;  
                  Buff: pEtherBuffer;  Stat: pEtherStatus;  Bytes: Integer);





{
{ These are the exceptions that may be raised by this module.
{ }


exception E10NInited;
{---------------------------------------------------------------
{
{ Abstract:
{   This exception will be raised if any procedures in this package
{   are called before E10Init.
{
{---------------------------------------------------------------} 


exception E10NReset;
{----------------------------------------------------------------
{
{ Abstract:
{   This exception will be raised if any transfer commands are executed
{   before a E10Reset is done.
{
{---------------------------------------------------------------}


exception E10ByteCount;
{---------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if a byte count passed to this 
{   interface is not in the valid range.  The number of data bytes
{   in an Ethernet packet must be in the range 46 <-> 1500,
{   (MinDataBytes <-> MaxDataBytes).
{
{----------------------------------------------------------------}


exception E10DByteError;
{--------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if the number of Bits passed to
{   E10DataBytes does not form a valid packet.
{
{---------------------------------------------------------------}


exception E10BadCommand;
{---------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if a bad command is given to any
{   of the routines in this package.
{
{---------------------------------------------------------------} 

exception E10TooMany;
{----------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if more than NumDCBs commands are
{   executed at any time.
{
{-----------------------------------------------------------------} 

exception E10STooMany;
{----------------------------------------------------------------
{
{ Abstract:
{   This exception is raised if more the client tries to execute more
{   than one send.
{
{----------------------------------------------------------------}

exception E10ReceiveDone(Stat: pEtherStatus);
{------------------------------------------------------------------
{
{ Abstract:
{   This exception is raised when a receive command has finished.  It
{   is raised by the Pascal level interrupt routine for the net. The
{   exception is only raised when the ethernet exception for data
{   interrupts has been turned on. The following does the trick:
{
{          Import IO_Others From IO_Others;
{          Setting := True;
{          IOSetExceptions(Ether10, IODataInterrupt, Setting);
{
{   Note that process clean upresets the bit on exit of a program.
{
{ Parameters:
{   Stat will be set to the status pointer of the command that finished.
{
{-------------------------------------------------------------------} 

exception E10NoHardware;
{---------------------------------------------------------------------
{
{ Abstract:
{   This exception is raised by E10GetAdr if there is no ethernet
{   board in the machine. 
{
{----------------------------------------------------------------------}
       


{********************} Private {********************} 

imports Memory from Memory;  
imports IO_Unit from IO_Unit;
imports IO_Private From IO_Private;

type 
    pDouble = ^Double;

{
{ Define the types that are used to get the physical address
{ from the ethernet board.
{ }

type 
    PhyAddRec = packed record
       High:  integer;
       Mid:   integer;
       EIOMHN:  0..15;
       CIOMHN:  0..15;
       EIOHN:   0..15;
       CIOHN:   0..15;
       EIOLN:   0..15;
       CIOLN:   0..15;
       EIOMLN:  0..15;
       CIOMLN:  0..15;
       end;
    pPhyAddRec = ^PhyAddRec;


var
    ResetDCB : pEtherDCB;
    HeadPtr: pEtherHeader;
    StatPtr: pEtherStatus;
    ClkDCBPtr: puSClkDCB;
    SavePtr: pEtherRegSave;
    EtherSeg: Integer;
    Init: Integer;
    IsReset: Boolean;
    ThisMachine: EtherAddress;
    DoublePtr: pDouble;


 
const
      InitVal =  #125252;   { 1010101010101010.   Special initalized value. }

      UseStartIO = True;   { True ==> StartIO,  False ==> JCS }
      
{$ifc UseStartIO then} 
    {$Message StartIO version}
{$elsec}
    {$Message JCS version}
{$endc}
                
imports EtherInterrupt from EtherInterrupt; 


function GetAdd: EtherAddress;
{--------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to get the address of the machine from
{   from the Network board.
{
{ Results:
{   Return the address of this machine.
{
{-------------------------------------------------------------------}
  type Nib = 0..15;  
  function Mirror(NibIn: Nib): Nib;
  type HackRec = packed record
    case boolean of
        true: (Nibble: Nib);
        false:(Bit0: boolean;
               Bit1: boolean;
               Bit2: boolean;
               Bit3: boolean);
         end;
  var H1, H2: Hackrec;
    begin
    H1.Nibble := NibIn;
    H2.Bit3 := H1.Bit0;
    H2.Bit2 := H1.Bit1;
    H2.Bit1 := H1.Bit2;
    H2.Bit0 := H1.Bit3;
    Mirror := H2.Nibble;
    end;
          
                 
  var DCBPtr: pEtherDCB;
      Ptr: pEtherAdRec; 
      RetVal :EtherAddress;
      PhyPtr: pPhyAddRec;
      I: integer;
      
    begin
 
    DCBPtr := PopDCB;

{
{ Set up the reset to set the Multicast function
{ to return the address of the machine.
{}

    new(Ptr);
    Ptr^.LowAddress := 0;
    Ptr^.MCB := MltCstAddr;
    DCBPtr^.Headptr := recast(Ptr, pEtherHeader);
    DCBPtr^.BuffPtr := recast(SavePtr, pEtherBuffer);
    DCBPtr^.StatPtr := StatPtr;
    DCBPtr^.Cmd := EReset;
    DCBPtr^.BitCnt := 0;
    DCBPtr^.NextDCB := nil; 
    pUDevTab^[Ether10].pDataCtrl := recast(DCBPtr, Pointer);
{$ifc UseStartIO then}
    StartIO(EP_Ethernet);
{$elsec}
    LoadExpr(Lor(Shift(EtherLoc,8),Shift(EtherLoc,-8)));
    InLineByte( #277 {JCS} );
{$endc}
    dispose(Ptr);

{
{ Now do the receive.
{ }

    DCBPtr^.HeadPtr := HeadPtr;
    DCBPtr^.BuffPtr := recast(Saveptr, pEtherBuffer);
    DCBPtr^.StatPtr := StatPtr;
    StatPtr^.CmdInProgress := true;
    DCBPtr^.Cmd := EReceive;
    DCBPtr^.NextDCB := nil;

    DCBPtr^.BitCnt := 0;
    RecvsPosted := RecvsPosted + 1;
    pUDevTab^[Ether10].pDataCtrl := recast(DCBPtr, Pointer);
    RListHead := DCBPtr;
    RListTail := RListHead;
{$ifc UseStartIO then}
    StartIO(EP_Ethernet);
{$elsec}
    LoadExpr(Lor(Shift(EtherLoc,8),Shift(EtherLoc,-8)));
    InLineByte( #277 {JCS} );
{$endc} 
    I := 0;
    while StatPtr^.CmdInProgress do 
        begin
        I := I + 1;
        if I > 1000 then raise E10NoHardware;
        end;
    
{
{ Now get the address from the header.
{}

    PhyPtr := recast(HeadPtr, pPhyAddRec);
    RetVal.High := PhyPtr^.High;
    RetVal.Mid :=  PhyPtr^.Mid;

    If Land(RetVal.Mid,255) = 0
    then begin    
         I := shift(Mirror(PhyPtr^.CIOHN), 12);
         I := lor(I, shift(Mirror(PhyPtr^.CIOMHN), 8));
         I := lor(I, shift(Mirror(PhyPtr^.CIOMLN), 4));
         I := lor(I, Mirror(PhyPtr^.CIOLN));
         end
    else begin
         I := shift(PhyPtr^.EIOHN, 12);
         I := lor(I, shift(PhyPtr^.EIOMHN, 8));
         I := lor(I, shift(PhyPtr^.EIOMLN, 4));
         I := lor(I, PhyPtr^.EIOLN);
         end;
    
    RetVal.Low := I; 
    GetAdd := RetVal;
    end;




procedure E10Init;
{--------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to intialize the Ethernet module.
{   It must be called before any other procedure in this package are used.
{
{   This procedure is called ONCE at boot time by the O.S.  It MUST NOT
{   be called by user programs.
{
{ Side Effects:
{   This procedure will allocate any memory used by this module.
{
{--------------------------------------------------------------------------}
  handler E10ReceiveDone(Stat: pEtherStatus);
    begin
    end;

  var SegSize, I: Integer;

    begin
    { if Init = InitVal then exit(E10Init); }
    
    SetDDS(961);
    
    {$ifc UseStartIO then}
        { InitEtherNet called by DevTabSetup microcode }
    {$elsec} 
        LoadExpr(Lor(Shift(EInitLoc,8),Shift(EInitLoc,-8)));
        InLineByte( #277 {JCS} );
     {$endc}
    
    SetDDS(962); 
           
    SegSize := ((NumDCBs * WordSize(EtherDCB) + 2)      { 4 word aligned }
                + WordSize(EtherStatus)
                + WordSize(EtherHeader) + WordSize(EtherRegSave)
                + WordSize(uSClkDCB) + WordSize(EtherDCB) + 255) div 256;

    CreateSegment(EtherSeg, SegSize, 1, SegSize);    
    SetDDS(963);
    for I := 1 to NumDCBs do new(EtherSeg, 4, DCBStack[I]);
    new(EtherSeg, 8, HeadPtr);
    new(EtherSeg, 2, StatPtr);
    new(EtherSeg, 2, SavePtr);
    new(EtherSeg, 2, ClkDCBPtr);    
    New(EtherSeg, 4, ResetDCB);
    
    SetDDS(964);
    SetMobility(EtherSeg, UnMovable);    
    SetDDS(965);

    SendsPosted := 0;
    RecvsPosted := 0;
    StackPointer := NumDCBs;
    RListHead := nil;
    RListTail := nil;
    SListHead := nil;
    
    IsReset := false;
    ThisMachine.Low := -1;
    Init := InitVal;    
    SetDDS(966);
    
    end;

    

procedure E10IO(Cmd: EtherCommand;  Header: pEtherHeader;  Buff: pEtherBuffer;
                  Stat: pEtherStatus;  Bytes: Integer);
{-------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to start an Ethernet I/O operation and return.
{
{ Parameters:
{   Cmd is the command that is to be executed.
{
{   Header is a pointer to an Ethernet header block.  The client must fill
{   in all fields of this header.
{
{   Buff is a pointer to the buffer that is to be sent or filled. For
{   receives the buffer must be allocated with an extra quad word. The
{   extra space is used to ensure that there is space for the CRC in memory.
{
{   Stat is a pointer to a status block for use during this command.
{
{   Bytes is the number of data bytes that are to be transfered.
{   This value must be between 46 and 1500
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before EtherInit.
{ 
{   E10NReset: Raised if this procedure is called before EReset.
{
{   E10ByteCount: Raised if Bytes is not in the valid range.
{
{   E10BadCommand: This is raised if the command passed is not
{   Send, Receive or PromisciousReceive.
{
{   E10TooMany: is raised if too many commands are executed at a given
{   time.
{
{   E10STooMany: is raised if more than one send command is executed.
{
{------------------------------------------------------------------------} 
  var DCBPtr: pEtherDCB;
      OldTail: pEtherDCB;

    begin
    if Init <> InitVal then raise E10nInited;
    if not IsReset then raise E10NReset;

    if (Cmd <> ESend) and (Cmd <> EReceive) and
       (Cmd <> EPromiscuousReceive) then raise E10BadCommand;

    if (SendsPosted + RecvsPosted) = NumDCBs then raise E10TooMany;

    if (Cmd = ESend) and (SListHead <> nil) then raise E10STooMany;
                                                        
    if ((Bytes < MinDataBytes) or (Bytes > MaxDataBytes)) and                                   (Cmd = ESend) then raise E10ByteCount;

{
{ Set up the common parts of the DCBs.
{ After that is done go into the command specific code.
{ If we are doing a receive build the linked list of DCBs.
{ We only have to start the microcode if there are no receives
{ pending.
{ }
    
    DCBPtr := PopDCB;
    DCBPtr^.HeadPtr := Header;
    DCBPtr^.BuffPtr := Buff;
    DCBPtr^.StatPtr := Stat;
    Stat^.CmdInProgress := true;
    DCBPtr^.Cmd := Cmd;
    DCBPtr^.NextDCB := nil;

    InLineByte( 105 {INTOFF} );   { no interruptions during this stuff }

    if (Cmd = ESend) then
        begin
        DCBPtr^.BitCnt := (Bytes + 14) * 8;
        SendsPosted := SendsPosted + 1;
        Stat^.RetryTime := 0;
        SListHead := DCBPtr; 
        pUDevTab^[Ether10].pDataCtrl := recast(DCBPtr, Pointer);
        {$ifc UseStartIO then}
            StartIO(EP_Ethernet);
        {$elsec}
            LoadExpr(Lor(Shift(EtherLoc,8),Shift(EtherLoc,-8)));
            InLineByte( #277 {JCS} );
        {$endc}  
        end
    else
        begin
        If Bytes > 1500 Then Raise E10ByteCount;
        If (Bytes = 0) Or (Bytes > 1500) Then Bytes := 1500;
        DCBPtr^.BitCnt :=  - (Bytes + 18) * 8 - 1;  
        DCBPtr^.StatPtr^.LargePkt := False;
        RecvsPosted := RecvsPosted + 1;
        if RListHead = nil then
            begin
            pUDevTab^[Ether10].pDataCtrl := recast(DCBPtr, Pointer);
            RListHead := DCBPtr;
            RListTail := RListHead;
            {$ifc UseStartIO then}
                StartIO(EP_Ethernet);
            {$elsec}
                LoadExpr(Lor(Shift(EtherLoc,8),Shift(EtherLoc,-8)));
                InLineByte( #277 {JCS} );
            {$endc}
            end
        else
            begin
            OldTail := RListTail;
            RListTail^.NextDCB := DCBPtr;
            RListTail := DCBPtr;
            if not OldTail^.StatPtr^.CmdInProgress then
              if recast(OldTail, Pointer) = pUDevTab^[Ether10].pDataCtrl then
                begin
                  pUDevTab^[Ether10].pDataCtrl := recast(DCBPtr, Pointer);
                  {$ifc UseStartIO then}
                      StartIO(EP_Ethernet);
                  {$elsec}
                      LoadExpr(Lor(Shift(EtherLoc,8),Shift(EtherLoc,-8)));
                      InLineByte( #277 {JCS} );
                  {$endc}
                end
            end;
        end;

    InLineByte( 106 {INTON} );  { interrupts are ok now }

    end;


procedure E10Wait(Stat: pEtherStatus);
{-----------------------------------------------------------------------
{
{ Abstract:
{   This procedure will wait for the completion of some Ethernet request.
{
{ Parameters:
{   Stat is the pointer to the EtherStatus that was provided when the
{   command was initiated.
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before E10Init.
{ 
{   E10NReset: Raised if this procedure is called before EReset.
{
{-----------------------------------------------------------------------} 
    begin
    if Init <> InitVal then raise E10nInited;
    if not IsReset then raise E10NReset;
    while Stat^.CmdInProgress do ;
    end;  


procedure E10Reset(Ptr: pEtherAdRec);
{-------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to reset the Ethernet interface.
{
{ Parameters:
{   Ptr is a pointer to the address record that is to be used for
{   the reset.
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before EtherInit. 
{
{-----------------------------------------------------------------------}
  var DCBPtr: pEtherDCB;

    begin
    if Init <> InitVal then raise E10NInited;

{
{ Clear all current DCBs.
{ }
    

    InLineByte( 105 {INTOFF} );   { no interruptions during this stuff }

    while RListHead <> nil do
        begin
        PushDCB(RListHead);
        RListHead := RListHead^.NextDCB;
        end;

    RListTail := nil;

    while SListHead <> nil do
        begin
        PushDCB(SlistHead);
        SListHead := SListHead^.NextDCB;
        end;

    SendsPosted := 0;
    RecvsPosted := 0;    

    InLineByte( 106 {INTON} );  { interrupts are ok now }


{
{ Set up the DCB.
{ }

    DCBPtr := ResetDCB;
    
    DCBPtr^.Headptr := recast(Ptr, pEtherHeader);
    DCBPtr^.BuffPtr := recast(SavePtr, pEtherBuffer);
    DCBPtr^.StatPtr := StatPtr;
    DCBPtr^.Cmd := EReset;
    DCBPtr^.BitCnt := 0;
    DCBPtr^.NextDCB := nil; 
    pUDevTab^[Ether10].pDataCtrl := recast(DCBPtr, Pointer);

{
{ Do the reset.
{ }

    {$ifc UseStartIO then }
        StartIO(EP_Ethernet);
    {$elsec}
        LoadExpr(Lor(Shift(EtherLoc,8),Shift(EtherLoc,-8)));
        InLineByte( #277 {JCS} );
    {$endc}

    ThisMachine.Low := lor(shift(Ptr^.LowAddress, -8), 
                           shift(Ptr^.LowAddress, 8));
    IsReset := true;
    end;




function E10DataBytes(RecvBits: Integer): Integer;
{------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to obtain the number of data bytes that are
{   in a packet that was received over the network.
{
{ Parameters:
{   RecvBits is the number of bits that were in the packet.  This
{   value will come from the BitsRecv field of the status block.
{
{ Results:
{   This function will return the number of data bytes that were in
{   the packet.
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before EtherInit.
{ 
{   E10NReset: Raised if this procedure is called before EReset.
{
{   E10DByteError: Raised if the numebr of bits in the packet was
{   not a multiple of 8 or if the number of data bytes was less than
{   MinDataBytes.
{
{------------------------------------------------------------------------}
  Var Tmp1: Integer;
    begin
    if Init <> InitVal then raise E10nInited;
    if not IsReset then raise E10NReset;
 
    Tmp1 := RecvBits div 8;
    if (Tmp1 * 8) <> RecvBits then raise E10DByteError;

    Tmp1 := Tmp1 - 18;
    if (Tmp1 < MinDataBytes) or (Tmp1 > MaxDataBytes) then
        raise E10DByteError;

    E10DataBytes := Tmp1;
    end;


function  E10GetAdr: EtherAddress;
{-----------------------------------------------------------------------
{
{ Abstract:
{   This function will return the address of this machine.
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before E10Init.
{
{   E10NoHardware: Raised if there is no ethernet board in the machine.
{ 
{-----------------------------------------------------------------------}
  handler E10ReceiveDone(Stat: pEtherStatus);
    begin
    end;

    begin
    if Init <> InitVal then raise E10nInited;

    if ThisMachine.Low = -1 then
        ThisMachine := GetAdd;
    
    E10GetAdr := ThisMachine;
    end;


procedure E10State(var NumSend, NumReceive: integer);
{------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to return the internal state of the
{   Ethernet interface.
{
{ Parameters:
{   NumSend will be set to the number of Sends that are pending.
{
{   NumReceive will be set to the number of receives that are pending.
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before E10Init.
{ 
{   E10NReset: Raised if this procedure is called before EReset.
{
{-----------------------------------------------------------------------}
    begin
    if Init <> InitVal then raise E10nInited;
    if not IsReset then raise E10NReset;

    NumSend := SendsPosted;
    NumReceive := RecvsPosted;
    end;



procedure E10WIO(Cmd: EtherCommand;  Header: pEtherHeader;  
                  Buff: pEtherBuffer;  Stat: pEtherStatus;  Bytes: Integer);
{-------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to start an Ethernet I/O operation and wait for it
{   to complete.
{
{ Parameters:
{   Cmd is the command that is to be executed.
{
{   Header is a pointer to an Ethernet header block.  The client must fill
{   in all fields of this header.
{
{   Buff is a pointer to the buffer that is to be sent or filled.
{
{   Stat is a pointer to a status block for use during this command.
{
{   Bytes is the number of data bytes that are to be transfered.
{   This value must be between 46 and 1500
{
{ Exceptions:
{   E10NInited: Raised if this procedure is called before E10Init.
{ 
{   E10NReset: Raised if this procedure is called before EReset.
{
{   E10ByteCount: Raised if Bytes is not in the valid range.
{
{   E10BadCommand: This is raised if the command passed is not
{
{   Send, Receive or PromisciousReceive.
{
{   E10TooMany: is raised if too many commands are executed at a given
{   time.
{
{   E10STooMany: is raised if more than one send command is executed.
{
{------------------------------------------------------------------------}
    begin
    E10IO(Cmd, Header, Buff, Stat, Bytes);
    E10Wait(Stat);
    end.


