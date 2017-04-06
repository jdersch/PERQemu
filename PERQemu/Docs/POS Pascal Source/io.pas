{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IO;
{-------------------------------------------}
{
{ Abstract:
{
{ PERQ Raw IO Drivers - Compatibility file.
{ Written by: Miles A. Barel
{ Copyright (C) 1980, 1981, 1982, 1983
{ Three Rivers Computer Corporation
{
{   The IO module has been split into four modules:  IO_Init, IO>Unit,
{   IO_Others, and IO_Private.  This module is provided for 
{   compatibility.  It imports the first three modules in its exports
{   section.  IO_Private is not exported because it provides definitions
{   which were private in the old IO module and is used only by the
{   new IO modules.
{
{ Design:
{       1) Interrupt routines must *never* cause segment faults.
{       2) UnitIO must increment and decrement the IOCount of the segments
{          which are involved in IO.
{       3) Segment faults must *never* happen while interrupts are off.
{
{-------------------------------------------}

{*******************************}   Exports   {*****************************}

Const IOVersion = '4.8';

{
  11-May-81  JPS  V4.8  Split IO into several modules.
  
   6-May-81  JPS  V4.7  1. Use new form of the SetCylinder StartIO.
                        2. Don't bother doing 10 trys in FindSize since only
                           the last result was believed regardless of success
                           or failure.
                        3. Hang if we cannot figure out the size of the disk.
  
  11-Apr-81  JPS  V4.6  Changes for virtual memory.
  
  19-Mar-81  BAM  V4.5  Changed name of included modules to IO_Init and IO_Proc
  
   3-Mar-81  JPS  V4.4  1) Fix LocateDskHeads and FindSize to agree with V4.3.
                        2) Teach the HardDisk timeout code about multi-sector
                           operations.

  28-Feb-81  JPS  V4.3  No longer do conversions on Disk Physical block
                        numbers (reinstating changes made in V4.0).

  25-Feb-81  GGR  V4.2  Added setting/reading of DskFill1 in UnitIO.
                        Moved new/dispose of CB from UnitIO to IO.Init.

  16-Feb-81  BAM  V4.1  Put back in conversions on Disk Physical block
                        numbers; fixed botCursF bug.  Del XXX procedures;
                        Changed to use new screen
  
   9-Feb-81  BAM  V4.0  No longer does conversions on Disk Physical block
                        numbers; fixed CursorUpdate to allow partial screen
                        display and added procedure IOScreenSize to set a 
                        new size.
  
  13-Jan-81  JPS  V3.3  Move creation of the IOSeg to memory manager init.
                        Move $R- to private part.

  20-Nov-80  JPS  V3.2  Initialize TabFifoInx in InitTablet.
  
  17-Nov-80  JPS  V3.1  Export the interrupt table.
                        Check SystemInitialized for control-C abort.
  
  16-Nov-80  BAM  V3.0  Radically changed Cursor and Tablet interface.  New
                        time procedures.  Split into another include file.
  
  10-Oct-80  JPS  V2.2  Added support for the diagnostic display (DDS).
 
  27-Sep-80  DAS  V2.1  Added timeout code to UnitIO for the
                        hard disk.
 

  19-Sep-80  DAS  V2.0  Added code for 24 MByte disks.
} 


Imports IO_Init from IO_Init;
Imports IO_Unit from IO_Unit;
Imports IO_Others from IO_Others;


{*******************************}   Private   {*****************************}


procedure Foo;
begin end.
