{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module IO_Private; 
{---------------------------------------------------------------------------
{
{ Abstract:
{
{   IO type, and data definitions common to the IO system
{   but not available to other Perq modules.   
{
{   Copyright (C) Three Rivers Computer Corporation, 1982, 1983
{---------------------------------------------------------------------------}

{$Version V6.3 for POS}
{---------------------------------------------------------------------------
{
{  Change History:
{
{   27 Oct 83       V6.3  Dirk Kalp
{                         Added Z_FlushIn, Z_DevRead, and Z_DevWrite
{                         Z80 commands.
{
{   23 Feb 83       V6.2  Roger Riggs
{                         Added Z_Flush Z80 command
{
{    9 Feb 83       V6.1  Brad Myers
{                         Make SRightX a variable.
{
{   27 Oct 82       V6.0  August G. Reinig, Chuck Becket
{                         Changed to work with new Z80 code and new micro code.
{                         A complete rewrite, very little of the interface
{                         is the same.
{
{    2 Dec-81  BAM  V5.9  Added Help switch.  
{
{   16-Nov-81  DAS  V5.8  (JPS) Put in John's changes for megabyte memory. 
{  
{    4-Nov-81  DAS  V5.8  Fixed for 10 MBaud Ethernet. 
{  
{   25-Jun-81  JPS  V5.7  Get rid of EnTabUpdate and EnCurUpdate since they 
{                         didn't work anyway. 
{  
{    4-Jun-81  JPS  V5.6  Fix Beep when type ahead too far (for the last time 
{                         I hope). 
{  
{    3-Jun-81  JPS  V5.5  Add Virgil headers and comments. 
{  
{   23-May-81  JPS  V5.4  Use new IOKeyDisable/IOKeyEnable in place of old 
{                         DisCtlC/EnaCtlC.  Reinstate Beep when type ahead too 
{                         far. 
{  
{   22-May-81  JPS  V5.3  Fix control-c processing.  Remove Beep when type 
{                         ahead too far. 
{    
{   16-Apr-81  GGR  V5.2  Add 3MHz Ethernet Drivers (JEB) 
{    
{   31-Mar-81  GGR  V5.1  Added 4-button mouse support. 
{    
{   11-May-81  JPS  V5.0  Split IO into several modules. 
{    
{    6-May-81  JPS  V4.7  1. Use new form of the SetCylinder StartIO. 
{                         2. Don't bother doing 10 trys in FindSize since only 
{                            the last result was believed regardless of 
{                            succes or failure. 
{                         3. Hang if we cannot figure out the size of the disk.
{
{   11-Apr-81  JPS  V4.6  Changes for virtual memory. 
{    
{   19-Mar-81  BAM  V4.5  Changed name of included modules to IO_Init and 
{                         IO_Proc 
{    
{    3-Mar-81  JPS  V4.4  1) Fix LocateDskHeads and FindSize to agree with 
{                            V4.3. 
{                         2) Teach the HardDisk timeout code about
{                            multi-sector operations.
{  
{   28-Feb-81  JPS  V4.3  No longer do conversions on Disk Physical block 
{                         numbers (reinstating changes made in V4.0). 
{  
{   25-Feb-81  GGR  V4.2  Added setting/reading of DskFill1 in UnitIO. 
{                         Moved new/dispose of CB from UnitIO to IO.Init. 
{  
{   16-Feb-81  BAM  V4.1  Put back in conversions on Disk Physical block 
{                         numbers; fixed botCursF bug.  Del XXX procedures; 
{                         Changed to use new screen 
{    
{    9-Feb-81  BAM  V4.0  No longer does conversions on Disk Physical block 
{                         numbers; fixed CursorUpdate to allow partial screen 
{                         display and added procedure IOScreenSize to set a  
{                         new size. 
{    
{   13-Jan-81  JPS  V3.3  Move creation of the IOSeg to memory manager init. 
{                         Move $R- to private part. 
{  
{   20-Nov-80  JPS  V3.2  Initialize TabFifoInx in InitTablet. 
{    
{   17-Nov-80  JPS  V3.1  Export the interrupt table. 
{                         Check SystemInitialized for control-C abort. 
{    
{   16-Nov-80  BAM  V3.0  Radically changed Cursor and Tablet interface.  New 
{                         time procedures.  Split into another include file. 
{    
{   10-Oct-80  JPS  V2.2  Added support for the diagnostic display (DDS). 
{   
{   27-Sep-80  DAS  V2.1  Added timeout code to UnitIO for the 
{                         hard disk. 
{   
{  
{   19-Sep-80  DAS  V2.0  Added code for 24 MByte disks. 
{ 
{---------------------------------------------------------------------------}

{>>>>>>>>>>>>>>>>>>>>}    exports    {<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<}

imports SystemDefs from SystemDefs;
imports IO_Unit from IO_Unit;
imports IO_Others from IO_Others;
imports Raster from Raster;

const

  { Micro-code I/O Entry Points }

  { These entry points are parameters to the Pascal supported Q-code    }
  { "STARTIO".  Additional parameters, where required, are documented   }
  { in the explainations.  Note that ETOS refers to the top of the      }
  { expression stack as seen by the Pascal programmer.  After a StartIO }
  { instruction is executed, the I/O Entry Point has been pushed to the }
  { ETOS, and thus, what looks like ETOS to Pascal, is ETOS-1 to the    }
  { micro-code.  Note also that all addresses are virtual addresses, and}
  { that }
   

    EP_IOStart = 0; { I/O Micro-Code Initialization Entry Point }
                    { Expects a pointer to the micro-code device}
                    { table at top of stack.  Expects entry 0 of}
                    { the device table to have its data control }
                    { pointer set to the address of the         }
                    { interrupt vector.  To be executed once    }
                    { at system boot time as part of startup.   }
                    { Also sets stack base and limit.           }
                    
    EP_HardDisk = 1; { Hard Disk I/O request }
                     { Expects the hard disk uCode Device Table }
                     { entry's data control pointer to point to }
                     { a Disk Control Block. }

    EP_Ethernet = 2; { 3/10MB Ethernet I/O request }
                     { Expects the ethernet uCode Device Table  }
                     { entry's data control pointer to point to }
                     { an Ethernet Control Block. }

    EP_SetEnableMask = 3; { Update device's interrupt enable mask  }
                     { Expects a Z80 device ID at ETOS and a mask  }
                     { to be applied to interrupts for that        }
                     { device at ETOS-1. }

    EP_ReadTimer = 4;{ Returns a count of 'jiffies' since last }
                     { 'ReadTimer' request.  Returns jiffies   }
                     { on ETOS, ETOS-1.  ETOS-2 = return code. }

    EP_ReadCause = 5;{ Determine the cause of an interrupt and }
                     { attention from a device.  Expects a device ID
                     { at ETOS.  Returns interrupt cause on ETOS, }
                     { attention cause on ETOS-1.  }

    EP_Z80Msg = 6;   { Enqueue a message for delivery to Z80 }
                     { Expects a pointer to a message at     }
                     { ETOS,ETOS-1 and a destination queue   }
                     { at ETOS-2. Returns a results indicator}
                     { (integer) at ETOS  }


    { StartIO entry points 7 and 8 no longer exists and can be
      redefined. }
      
    EP_UcodeMsg = 9; { Get a Z80 message from the Ucode sent message }
                     { queue.  The Ucode returns a pointer to a Z80  }
                     { message at ETOS, ETOS-1. }

    EP_GetChar = 10; { Return next char from the circular buffer of }
                     { the specified device, if any has been received, }
                     { with completion status.  Expects a device ID at }
                     { ETOS.  Returns a results indicator (integer) at  }
                     { ETOS and a byte at ETOS-1. }

    EP_PutCircBuffer = 11; { Place a byte onto a circular buffer }
                     { Expects a character on ETOS, a pointer to a     }
                     { circular buffer control block at ETOS-1,ETOS-2, }
                     { and returns a results indicator (integer) at ETOS. }
    

    EP_GetCircBuffer = 12; { Return next byte from Specified Circular }
                     { buffer(if not empty) with completion status.   }
                     { Expects a pointer to a circular buffer control }
                     { block at ETOS,ETOS-1. }
                     { Returns a results indicator (integer) at ETOS  }
                     { and a byte at ETOS-1. }

 { Device table definitions }
  
  { Device type: }

    Dev_Unused = 0;     { indicates that the device field is not used }
    
    { Hard Disk device types }
    Dev_Shugart = 1;    { Shugart disk drive }
    Dev_Micropolis = 2; { Micropolis disk drive }
    Dev_SMD = 3;        { Store Module Technology }
 
  { Intr cause - all numbers are indexex into the IntrCause field }
   
   { Interrupt causes common to all devices }

    Dev_Attention = 0;    { We received an attention msg. from Z80 }
    Dev_AckReceived = 1;  { Z80 'Acked' our request msg. }
    Dev_NakReceived = 2;  { Z80 'Naked' our request msg. }
    Dev_StatReceived = 3; { Z80 sent us a status msg. }

   { Intr cause - Device Specific }

    { the following is valid for RSB, RSA, GPIB, and the KeyBoard }
    
    Dev_DataAvailable = 5;    { Data has arrived in the circular buffer }
                        
    { the following is valid for screen out }
    
    Dev_ScreenUpdate = 6;     { time for a screen update }
        

const

  CirBufSize = #100-3;

type

{ The definition of a circular buffer }

  CirBufItem = packed record   { one element of a circular buffer }
   ch: char;                   { the character }
   status: 0..255             { and a byte of status information }
   end;
   
  CircularBuffer = packed record    { the circular buffer for characters }
   Length: integer;            { number of characters in the buffer }
   RdPtr:  integer;            { where to get characters from }
   WrPtr:  integer;            { where to put characters to }
   Buffer: packed array[0..CirBufSize-1] of CirBufItem
   end;                        { lastly, the buffer of items }
   
  CirBufPtr = ^CircularBuffer;      { points to a circular buffer }

  Z_CmdRegister = packed record
  case integer of
    1: ( Bits : packed array [0..15] of boolean );
    2: ( Number : integer );
  end;


type
  
  DevTblEntry = packed record { Each entry must be }
                                                     { quad-word aligned. }
    IntrCause    : Z_CmdRegister;
                 { This is a bit map of the cause(s) of the interrupt(s)  }
                 { which the Pascal has not as yet received.              }

    IntrPriority : Z_CmdRegister;
                 { This array determines the interrupt vector entry and }
                 { thus the priority to be used when an interrupt for   }
                 { this device occurs.  0=highest, 15=lowest.           }
                 
    EnableMask   : Z_CmdRegister;
                 { This is a mask determining which types of interrupts,  }
                 { as defined in the IntrCause array, will produce Pascal }
                 { level interrupts.  '1' enables intr. at correponding   }
                 { bit position in IntrCause, '0' masks the intr. until   }
                 { the mask bit is again set to '1'}
                 
    ATNCause     : Z_CmdRegister;
                 { This is a device-specific bit map of the reason(s) for }
                 { an attention message from the device.                  }

    pCirBuf      : CirBufPtr;                 { KEEP THIS QUAD WORD ALIGNED }
                 { Points to a Circular Buffer }
                                  
    pStatus      : pointer;
                 { Points to device-specific device status information. }
                 
    pDataCtrl    : pointer;                   { KEEP THIS QUAD WORD ALIGNED }
                 { Points to device-specific I/O control structure to be }
                 { used in all operations not using the circular buffer }

    DeviceType   : integer;
                 { Differentiates units as to their type.  This code will }
                 { vary among different hard disk drives where the uCode  }
                 { must process requests for the drives differently.      }

    Reserved     : packed array [0..0] of integer; { Filler to insure a }
                                                    { quad word alignment}
  end; 

  pUDeviceTable = ^UDeviceTable;
  UDeviceTable = array [0..MaxUnit] of DevTblEntry;


{ The definition of a high volume data control buffer }

  HiVolBlock = packed record  { MUST BE QUAD WORD ALIGNED }
   DataByteCnt: integer;     { number of bytes in the buffer }
   pDataBuffer: IOBufPtr     { pointer to buffer supplied by the user }
   end;
  pHiVolBlock = ^HiVolBlock;

  

Type
        
        IOPtrKludge = record case integer of
                        1: (Buffer: IOBufPtr);
                        2: (Offset: Integer;
                            Segment: Integer)
                        end;
                        



type

   Z_Commands = ( { The order of declaration of these commands }
                    { should not be changed as the uCode and Z80 }
                    { depend on their generated values remaining }
                    { constant. } 
          Z_Illegal,          Z_RequestData,
          Z_BlockData,        Z_SendData,
          Z_ACK,              Z_NAK,
          Z_ATN,              Z_Status,
          Z_Seek,             Z_Config,
          Z_Boot,             Z_Reset,
          Z_Sense,            Z_WriteRegisters,
          Z_InHiVolumeStart,  Z_OutHiVolumeStart,
          Z_ReadData,         Z_WriteData,
          Z_ReadID,           Z_ReadDeletedData,
          Z_WrtDeletedData,   Z_Specify,             
          Z_Format,           Z_Recal,             
          Z_SenseDriveStatus, Z_EOIdata,
          Z_Flush,            Z_FlushIn,
          Z_DevRead,          Z_DevWrite  );
                               
const

  Z_SOM = 170 ;  { Start of Z80 message character for Z80 messages }
                   { Binary 10101010 to make it unlikely to match a  }
                   { device, command, or byte count. }
 
{ Length constants for data portion of Z80 messages }  

  Z_NoData = 2;  

  Z_FirstData = Z_NoData + 1;  

  Z_MaxData = 14;
  
  Z_DataSize = Z_MaxData - Z_NoData;  

type

  Z_Queue = (Z_Q0, Z_Q1, Z_Q2, Z_Q3 );
  
  Z_Data = packed array[Z_FirstData..Z_MaxData] of 0..255;
  
  pZ_Msg = ^Z_Msg;
  Z_Msg = packed record   
    pNext        : pZ_Msg;
    UCodeArea    : long;   { micro code uses these two words, don't touch }
    SOMDelimiter : 0..255;
    ByteCount    : 0..255;
    Device       : 0..255;
    Command      : 0..255;
    Data         : Z_Data
    end;
  
                              
  Z_MsgPtrKludge = record case boolean of
       true: (pMsg: pZ_Msg);
       false:(Offset: integer;
              Segment: integer);
       end;


const
  SLeftX = 0;             { left most X coordinate }
  STopY = 0;              { top most coordinate }
 
Var 
  SRightX : Integer;      { calculated based on screen type }
  SBottomY : integer;     { current bottom most Y coordinate }
  
    {the next two are used to adjust the tablet coordinates to fit onto
     the landscape screen since its x coordinate goes up to 1280 }
  GPIBShiftXAmt : Integer; { 0 if landscape, else -1 }
  KrizShiftXAmt : Integer; { 1 if landscape and absolute, else 0 }
  
  pUDevTab : pUDeviceTable;   { points to the device table }

  Z_MsgNotAvailable : integer; { count of number of times Ucode did not have }
                               { an empty Z80 message when asked for one }

  IOSegNum : integer;  { For debugging, holds the actual segment number }
                       { from which IOSegment data structures are       }
                       { allocated.  Holds actual segment number of the }
                       { IOSeg when not testing. }

  Z_IntDisabled : boolean;

  KeyEnabled : boolean;
    
  TimeBuf : ^long;    { points to timer information, here because }
                      { there is no IOTimer }

  RaiseException : packed array[0..MaxUnit] of packed record
                     Attention : boolean;
                     DataAvailable : boolean
                     end;
  




Procedure Z_SendMsg( pMsgToSend : pZ_Msg; SendQueue : Z_Queue );

function Z_DqSysMsg : pZ_Msg;

procedure Z_QSysMsg (pMessage : pZ_Msg );

procedure Z_CriticalSection ( BeginIt : boolean;
                              Unit    : integer;
                          var Save  : integer);


{>>>>>>>>>>>>>>>>>>>>}    private    {<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<}

const
  debug = false;

{$ifc debug 
then}    Imports TestSystem from TestSystem;
{$elsec} imports System from System;
{$endc}

const
  IntOff = 105;
  IntON  = 106;
  


Procedure Z_SendMsg( pMsgToSend : pZ_Msg; SendQueue : Z_Queue );

{----------------------------------------------------------------------}
{ Abstract:
{
{    This procedure is called by I/O system logic to send a message to
{    the Z80 I/O controller.  The message will be queued by the Perq
{    micro-code to be sent when the Z80 port becomes available.  Return
{    to the caller is immediate; the completion of transmission will be
{    detected from Z80 return messages.
{
{ Parameters:
{
{  pMsgToSend:
{    The single parameter is a pointer to a record which contains
{    the message as well as the queue to place the message on.
{----------------------------------------------------------------------}

begin

  pMsgToSend^.SOMDelimiter := Z_SOM;   { Stuff in constant  }
  pMsgToSend^.uCodeArea    := 0;       { Init uCode area    }

  LoadExpr(ord(SendQueue));
  {$ifc debug 
  then}    LoadAdr( pMsgToSend );
  {$elsec} LoadAdr( pMsgToSend^);
  {$endc}
  StartIO(EP_Z80Msg);
 
end; { Z_SendMsg }

function Z_DqSysMsg : pZ_Msg;
{----------------------------------------------------------------------}
{ Abstract:
{
{    This routine removes a Z80 message from system owned queue
{    of messages which have been sent to the Z80.  Each call to this
{    routine will return with ether a pointer to a Z80 message.
{    
{    This procedure is meant to be called by routines needing an
{    empty Z80 message.
{
{ Returns:
{    The value returned is a pointer to a Z80 message.  If no messages
{    remain on the system owned queue this returned value is NIL.
{----------------------------------------------------------------------}

var 
  i : integer;
  FreeMsg : Z_MsgPtrKludge; { pointer to an empty Z80 message }

begin
  
  LoadExpr( 0 );
  StartIO(EP_UcodeMsg);      
  StorExpr(FreeMsg.Segment); 
  StorExpr(FreeMsg.Offset);      

  if FreeMsg.pMsg = nil then begin { if we failed the first time }
    Z_MsgNotAvailable := Z_MsgNotAvailable + 1; { count the failure }
    
    repeat                    { now continue trying until we succeed }
    LoadExpr( 0 );
    StartIO(EP_UcodeMsg);      
    StorExpr(FreeMsg.Segment); 
    StorExpr(FreeMsg.Offset);
    until FreeMsg.pMsg <> nil;
    end;
  
  Z_DqSysMsg := FreeMsg.pMsg;

end; { Z_DqSysMsg }

procedure Z_QSysMsg ( pMessage : pZ_Msg);
{----------------------------------------------------------------------}
{ Abstract:
{
{    This routine places a Z80 message onto the system owned queue
{    of messages which have been sent to the Z80.  Its purpose is to
{    provide IO routines with system owned messages and to
{    provide initialization logic with a way of first adding messages
{    to the queue.
{ Parameters:
{
{  Device:
{    This is the device of interest.  It is a Z80 device whose associated
{    queue of sent Z80 messages will be used.
{----------------------------------------------------------------------}
begin
          
  pMessage^.UCodeArea := 0;
  {$ifc debug 
  then}    LoadAdr( pMessage );
  {$elsec} LoadAdr( pMessage^);
  {$endc}                   
  loadexpr( -1 );
  startio( EP_UcodeMsg )

end; { Z_QSysMsg }

procedure Z_CriticalSection ( BeginIt : boolean;
                              Unit  : integer;
                          var Save  : integer);
{----------------------------------------------------------------------}
{ Abstract:
{
{    This routine implements a critical section by disableing all
{    interrupts for a device and restoring the interrupt state on
{    completion of the critical section.
{ Parameters:
{
{  BeginIt:
{    When true, a critical section should begin, else the critical
{    section is ended.
{
{  Unit:
{    Unit whose interrupts will be disabled.
{
{  Save:
{    When a critical section should begin, this value will be returned
{    to the caller containing the interrupt mask before all interrupt
{    types were disabled.
{    
{    When the critical section is being ended, this value is an input
{    parameter and holds(the previously saved) interrupt mask which
{    should be retored.
{----------------------------------------------------------------------}

begin
  
{$ifc debug then}
  if BeginIt
  then Write ( 'Enter Critical Section ')
  else Write ( 'Leave Critical Section ');
{$endc}

  if BeginIt 
  then begin
    save := pUDevTab^[ Unit ].EnableMask.Number;
    LoadExpr( 0 );
    LoadExpr( Unit )
    end
  else begin
    LoadExpr( save );
    LoadExpr( Unit )
    end;
  
  StartIO ( EP_SetEnableMask );

end. { Z_CriticalSection }
