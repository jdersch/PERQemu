{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module VolumeSystem;
{---------------------------------------------------------------------------
{
{  VolumeSystem - TV. ( Tony Vezza )
{
{  CopyRight (C) 1983, Three Rivers Computer Corporation.
{
{  Abstract:
{    This module provides uniform abstractions of the disks available on a 
{    Perq.  Disks are named by unique Constants of an eneumerated Type
{    exported by the module. A set of operations is provided and each is
{    named by a Constant of an another enumerated Type exported by the module.
{    Each disk is made to appear as an array of pairs of data blocks and
{    logical headers.  A general address Type with two components, one
{    to specify a disk and another to specify an index into the array on
{    that disk, is defined and exported. Mounting and dismounting of disks
{    is supported by a pair of Procedures. (Mounting a disk means reading
{    a symbolic name from a known address on that disk and Recording
{    a mapping of that symolic name to an identifier for the disk.)
{    Operations are provided to determine the number of pages (pairs of data
{    blocks and logical headers) and the last valid address on a given disk.
{
{    Naming conventions (necessitated by short identifier limits):
{
{       "Int" - means "Internal" (hard disk without removable packs).
{
{       "Ext" - means "External" (hard disk with removable packs).
{
{       "Flp" - means "Floppy".
{
{       "Mic" - means "Micropolis".
{
{       "Phy" - means "Physical".
{
{       "Vol" - means "Volume".
{
{       "ID"  - means "identifier" and refers to an abstract name Type.
{
{--------------------------------------------------------------------------}

{$Version V4.9 for POS}

{---------------------------------------------------------------------------
{
{  Change history:
{    
{
{   13 Apr 83  CDH  V4.9  Modify to support micropolis disk on CIO:
            In volidtoPhyId and PhyIdToVolId, add DCIOMicropolis to main 
            case statement.
            Code efficiency improved in VolNumberPages. 
            In DoVolIo, DCIOMicropolis added to main case statement.
            In VolIO, Numbertries changed to variable and set to 10 for 
            cio micropolis (since theres no point in doing more than one 
            reset/restore).   
            VolDskReset changed to do reset followed by restore for 
            cio micropolis (NB 3RCC code seems never to do Restore on eio).
            NB I can't find the V4.6 change and haven't done it for cio microp.
{
{   22 Feb 83  SSJ  V4.8  Add an unsupported disk kind to intdiskKind.
{
{   15 Feb 83  CDB  V4.7  Fixed compiler error.
{
{   11 Feb 83  TV   V4.6  Fixed problem with error counts incrementing
{                         for disk recalibrates.
{
{    2 Feb 83  TV   V4.5  Fixed up Documentation.
{
{   30 Nov 82  TV   V4.4  Actually implemented the Procedures and Functions
{                         defined in the File System interface described 
{                         by THD and SSJ in V4.3 thru V4.0. Tony Vezza.
{
{    8 Nov 82  THD  V4.3  Convert from DiskIO module to VolumeSystem module.
{
{    3 Nov 82  SSJ  V4.2  Do not export the import of FileDefs and Arith.
{  
{   27 Oct 82  THD  V4.1  Fixes to interface:
{                           1) Add NoSuchVolume and LBNOutOfRange exceptions.
{                           2) Introduce LogBlockNumber Type and use it in 
{                              LogDiskAddress Type definition, and in the
{                              signatures of Function NumberPages and exception
{                              LBNOutOfRange.
{                           3) Restore definition of LogHeaderBuffer to
{                              explicitly specify the Record structure of a
{                              header as required by the microcode.
{                           4) Add an input parameter to the Function 
{                              OnDiskToLogAddr which specifies the logical disk
{                              id component of the resulting LogDiskAddress.
{                           5) Remove Procedure ZeroBuffer from interface.
{                           6) Remove PhyDiskAddr Type definition and related
{                              conversion routines, LogAddrToPhyAddr and 
{                              PhyAddrToLogAddr.
{                           7) Remove exception BadDevice which is superseded by{
{                              exception NoSuchDevice.
{                           8) Add definition of MaxTotalVolumes Constant.
{
{                      
{   18 Oct 82  THD  V4.0  Major redefinition of interface for POS Version F.2:
{                           1) Add PhysDiskID Type to support naming of all
{                              possible disks on a system.
{                           2) Add LogDiskID Type to desgnate a mounted file
{                              system disk.
{                           3) Add OnDiskAddress Type, e.g. for hints.
{                           4) Add two pairs of address conversion operations:
{                                 OnDiskAddress <-> LogDiskAddress
{                                 LogDiskAddress <-> PhyDiskAddress
{                           5) Add a pair of disk name conversion operations:
{                                 PhyDiskID <-> LogDiskID     
{                           
{                           6) Add MountDisk and DisMountDisk Procedures.
{                           7) Add LookUpDiskID Function to support symbolic 
{                              names for disks.
{                           8) Define new Type for logically addressing blocks 
{                              on mounted disks, LogDiskAddress.
{                           9) Remove application oriented definitions of 
{                              the DiskBuffer Type leaving them to the client
{                              modules to define for themselves.
{                          10) Remove WhichDisk, AddrToField, and FieldToAddr
{                              routines since they are redundant facilities
{                              given the Type LogDiskAddress.
{                          11) Change the signatures of LastDiskAddr, 
{                              NumberPages, and DiskReset.
{                          12) Rewrote the abstract.
{                          13) Remove the disk Type notion from the interface.
{
{  /// From Old DiskIO Module \\\
{
{  12 Jan 82  BAM  V3.13 Fix bug in FloppyIO failure so deallocates storage.
{  24 Jun 81  BAM  V3.12 Fix to prevent retry if floppy write and device not
{                          writable.
{  28 May 81  BAM  V3.11 Fix bug in floppy header die-On-error.
{                        Add Recalibrate light to floppy
{                        New light definitions
{  26 May 81  JPS  V3.10 Use new Lights module.
{  19 May 81  BAM  V3.9  Fixed position of Reset light
{  12 May 81  BAM  V3.8  Removed print out for DiskErrors; more accurate print
{                          out of errors when happen; blink "light" during
{                          IOReset.
{                        Added new Exceptions VolIOFailure and BadDevice;
{                        Removed Procedure DiskError;
{                        Use new IO
{   6 May 81  JPS  V3.7  Fix bug in DiskReset by using the new form of the
{                          SetCylinder StartIO.
{  20 Apr 81  JPS  V3.6  Use DiskSegment consistently.
{                        Set DiskSegment as UnSwappable for virtual memory.
{   9 Apr 81  BAM  V3.5  Fix Retry so no recal on last time; DiskError tell op.
{                         Fixed bug in DoDiskIO exit
{  30 Mar 81  BAM  V3.4  Added Retry count to TryDisk and Const NumTries.
{  27 Mar 81  BAM  V3.3  Added comments and WriteLn to DiskError.
{  19 Mar 81  BAM  V3.2  Combined JPS+GGR and BAM's changes.
{  17 Mar 81  GGR  V3.1  Removed WinchDebug and FloppyDebug.
{  17 Mar 81  JPS  V3.0  Changed MapAddr and UnMapAddr to handle 24 Mbyte
{                          drives.
{                        Changed harddisk interlace factor to one.
{                        Removed partition kind 'node'.
{                        Added TryDiskIO, and changed DiskError printout.
{                        Changed FirstDiskBlock for harddisks and
{                        LastDiskBlock for 24 Mbyte disks.
{                        Improved retry and retry messages from DiskIO and
{                          FloppyIO.
{  16 Mar 81  BAM  V2.2  Changed directory to have extra bits
{   5 Mar 81  BR   V2.1  Added comments
{   5 Mar 81  BAM  V2.0  Changed Definitions and import FileDefs.
{   3 Mar 81  JPS  V1.4  Fix DiskReset to agree with IO V4.4.
{   1 Mar 81  BR   V1.3  Change FileDate to TimeStamp.
{   1 Mar 81  JPS  V1.2  Export the DiskReset routine.
{  28 Feb 81  JPS  V1.1  Get rid of LogMapping parameter to MapAddr by
{                          teaching UnitIO not to fool with hard disk address.
{---------------------------------------------------------------------------}


{******************} exports {***************************}

Imports IOErrors From IOErrors;

Type
   
    { Values of Type DiskKinds denote distinct classes of disk devices
      which can be connected to a Perq. }

    DiskKinds = (FlpDisk, IntDisk, ExtDisk);
    

    { FlpUnitNumber, IntUnitNumber, and ExtUnitNumber are Types
      for numbers denoting physical units of each distinct class of disk. 
      Separate Types are defined to express the necessity of performing
      distinct run time checks on values of unit numbers for each class of
      disk.  Precise ranges cannot be specified at compile time (i.e. in
      this program text) because of the requirement that this program must
      run on machines of many possible configurations without recompilation. }
    
    Cardinal = 0 .. #77777;

    FlpUnitNumber = Cardinal;
    IntUnitNumber = Cardinal;
    ExtUnitNumber = Cardinal;
     

    { Values of Type PhyDiskID are used to uniquely identify disk units 
      for the volume mounting and dismounting operations. }
      
    PhyDiskID = Record
                   Case Kind : DiskKinds Of
                      FlpDisk : (FlpUnit : FlpUnitNumber);
                      IntDisk : (IntUnit : IntUnitNumber);
                      ExtDisk : (ExtUnit : ExtUnitNumber)
                End;

    
    { InternalDiskKinds is the Type whose values denote the various kinds of
      internal disks. }

    IntDiskKinds = (Shugart14, Mic8, Mic5, UnSupported);


    
    { OnVolAddress is used to represent the logical address of a block
      of a volume in data structures on THAT volume or on a nonremovable
      volume; these are typically hints which link multiple file structures
      together.  SOLAR requires that such hints be two word objects with
      the two high order bits set and the eight low order bits cleared.
      The remaining 22 bits are divided into two fields: a 3 bit logical
      volume specifier and a 19 bit volume relative logical block number.
      Certain volume specifiers refer to the nonremovable volumes which are
      mounted as the corresponding logical disk (see comments below for Type
      VolID.  The remaining possible values of a volume specifier field all
      denote the volume that the hint itself is written on. }
      
    OnVolAddress = Long;

    
    
    { VolName is a string used as part of full file names to name mounted 
      file system disks. }

    VolName = String[8];

    
    { MaxTotalVols is the upper limit on the number of file system volumes
      that can be mounted at one time.  MinVolID and MaxVolID delimit
      the range of non-nil (i.e. actually mounted) file system volumes.  
      NilVolID denotes a volume different from any possible mounted volume. }

Const
    MaxTotalVols = 8;
    MaxVolID  = MaxTotalVols - 1;
    MinVolID  = 0;
    NilVolID  = MinVolID  - 1;


    { VolID uniquely identifies a mounted file system volume or a nil
      volume; it is also used as a component of a VolAddress.  VolID values
      in the subrange 0 .. MaxInternalUnits - 1 always denote a nonremovable 
      file system volume, i.e. an internal class disk device.  (This is related
      to the requirement that internal physical disks be mounted at 
      corresponding fixed VolIDs by the VolMount Function.)  Values in
      MaxInternalUnits .. MaxVolID denote mounted  removeable
      volumes.  MaxInternalUnits is an implicit Constant whose value is 
      determined by the configuration of the machine.

      VolRangeType is intended for use as an index Type for arrays which 
      correspond to mounted actual disks only. }
      

Type
    VolID        = NilVolID .. MaxVolID;
    VolRangeType = MinVolID .. MaxVolID;
    
   
    { VolBlockNumber is a subrange of Long used to uniquely specify a block of
      a file system volume.  A better Type definition (were it expressible 
      in current Perq Pascal) would be: #0 .. #1777777, i.e. 19 bit non-
      negative Integers. }

    VolBlockNumber = Long; 



 
    { VolAddress uniquely specifies a block on any of the mounted file
      system disks. }
      
    VolAddress = Record
                    Volume      : VolID;
                    BlockNumber : VolBlockNumber;
                 End;
    
    
    
    { VolBuffer defines an uninterpreted structure to be used for input
      and output buffers for data blocks during volume io operations.
      These buffers must be aligned on 256 word boundaries. }
      
    VolBuffer    = Packed Array[0 .. 4095] Of Boolean;
      
    ptrVolBuffer = ^VolBuffer;
    
    
    
    { VolHeaderBuffer defines an structure to be used for translated input
      and output buffers for header blocks during volume io operations. }
      
    VolHeaderBuffer = Record
                         SerialNumber       : VolBlockNumber;
                         SegmentBlockNumber : Integer;
                         FreeListHint       : Integer; {formerly called filler}
                         PreviousBlock,
                         NextBlock          : VolBlockNumber;
                      End; { VolHeaderBuffer }
               
    PtrVolHeaderBuffer = ^VolHeaderBuffer;
                              
    
    { VolIOCommand enumerates the commands available in the volume io 
      operations, VolIO and TryVolIO. }
      
    VolIOCommand = (VolRd, VolRdCheck, VolWr, VolWrCheck, VolReset,
                    { Last two for error reporting only (floppy only) }
                    VolHdrRead, VolHdrWrite
                   ); 

  




Procedure InitVolumeSystem ;

Procedure VolDiskReset( VID : VolID);

Function GetIntDiskKind : IntDiskKinds;

Function VolMount( PID : PhyDiskID ) : VolID;

Procedure VolDisMount( PID : PhyDiskID );

Function VolIDLookUp( Name : VolName) : VolID;

Function VolNameLookUp( VID : VolID) : VolName;

Function VolToOnVolAddr( VA : VolAddress) : OnVolAddress;

Function OnVolToVolAddr( VID  : VolID;
                         OVA  : OnVolAddress ) : VolAddress;

Function VolIDToPhyID( VID : VolID) : PhyDiskID; 

Function PhyIDToVolID( PID : PhyDiskID) : VolID; 

Function LastVolAddress( VID : VolID ) : VolAddress;


Function VolNumberPages( VID : VolId) : VolBlockNumber;

Procedure VolIO( VA          : VolAddress;
                 Ptr         : PtrVolBuffer;
                 HPtr        : PtrVolHeaderBuffer;
                 VolCommand  : VolIOCommand ) ;

Function TryVolIO( VA          : VolAddress;
                   Ptr         : PtrVolBuffer;
                   HPtr        : PtrVolHeaderBuffer;
                   VolCommand  : VolIOCommand;
                   NumTries    : Integer ) : Boolean;



{ Raised by VolIdLookUp(n) if no mounted Volume has N as its name }
Exception NoSuchNameForVol(N : VolName);

{ Raised when a VolAddress, VA, passed to an operation is such that 
  VA.BlockNumber is greater than VolNumberPages(VA.Volume) }
Exception VBNOutOfRange(VID : VolID;
                        VBN : VolBlockNumber);
                        
{ Raised when a VolAddress, VA, or a VolID, VID, passed to an operation
  is such that VA.Volume or VID denote a Volume which is not Mounted }
Exception NoSuchVol(vid : VolID);

{ Raised when a PhyDiskID, D, passed to an operation denotes a Disk not in the
  configuration }
Exception NoSuchDevice(D : PhyDiskID);

{ Raised whenever an entry in VolErrorCnt is incremented;
  this is a temporary measure to allow the compatibility version of
  DiskIO to keep its variable ErrorCnt updated. }
Exception VolErrInc(Error_code : Integer {IOEFirstError .. IOELastError} );

Exception VolIOFailure(Msg      : String; 
                      Operation : VolIOCommand;
                      Addr      : VolAddress;
                      SoftStat  : Integer);

Exception VolDiskError(Msg : String);

Exception VMountErr( D : PhyDiskID);


Var VolErrorCnt : Array[IOEFirstError..IOELastError,
                        VolRangeType] Of Integer;


{*****************}  PRIVATE {*****************}

Imports IO_Unit From IO_Unit;
Imports DiskUtility From DiskUtility;
Imports Diskdef From Diskdef;
Imports Screen From Screen;
Imports System From System;
Imports Memory From Memory;
Imports Lights From Lights;

    {//////////////////////////////////////////////////////////////////////
    {
    {    From DiskDef Module
    {
    {    PtrDCA         : PtrDskCtrlArray;
    {    NumDCBUsed     : 0..MaxTotalDisks;
    {    IntDiskType    : IntDiskKinds;
    {    PtrVBuf        : PtrVolBuffer;
    {    PtrVHBuf       : PtrVolHeaderBuffer;
    {    StatPtr        : IOStatPtr;
    {    BufPtr         : IOBufPtr;
    {    HdrPtr         : IOHeadPtr;
    {    FHeadPtr       : FlopHeadPtr;
    {
    {
    {    EIOFlag indicating what Type of system this is.
    {         If True then EIO.
    {         If False then CIO.
    {
    {    EIOFlag        : Boolean;
    {
    {
    {//////////////////////////////////////////////////////////////////////}
    

Procedure InitVolumeSystem ;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Initialize Volume System
    {
    {  Results:
    {    Currently Mounts Volumes - Floppy and HardDisk.
    {
    {  Calls:
    {    - VInitialize
    {
    {----------------------------------------------------------------------}

    Begin { InitVolumeSystem }

        VInitialize    { In DiskUtility }
                        
    End; { InitVolumeSystem }



Function GetIntDiskKind : IntDiskKinds;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Tells What Type of disks are connected to the Internal Disk
    {    Controller one of the three values:
    {        - Shugart14
    {        - Mic8
    {        - Mic5
    {
    {  Parameters:
    {        None. Just returns the Value of the Variable, IntDiskType,
    {        which is set up at InitVolumeSystem.
    {
    {  Results:
    {        Returns the value Internal Disk Kind.
    {
    {----------------------------------------------------------------------}

    Begin { GetIntDiskKind }
        GetIntDiskKind := IntDiskType
    End;  { GetIntDiskKind }


Function VolIDLookUp( Name : VolName) : VolID;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Scan the Disk Control Array for a Volume with Name and return
    {    that Volume's ID.
    {
    {  Parameters:
    {        Name is a Volume Name (a String[8]).
    {
    {  Results:
    {        The Volume ID corresponding to the Volume with Name if
    {        such a Volume exists and is mounted (in the DCA). If no
    {        Volume is mounted then return the Nil Volume ID.
    {
    {  Errors:
    {        - NoSuchNameForVol.
    {
    {----------------------------------------------------------------------}

    Var
        VID : VolID;
        NameFound : Boolean;
        
    Begin { VolIDLookUp }

        NameFound := False;

        For VID := MinVolID To MaxVolID Do
            Begin
                If PtrDCA^[ VID].DCBStatus.Mounted Then
                    If Name = PtrDCA^[ VID].VolumeName Then
                        Begin
                            VolIDLookUp := VID;
                            NameFound := True
                        End
            End;

        { Should Raise Exception, NoSuchNameForVol, when Not NameFound. }
        
        If Not NameFound Then
            Begin
                VolIDLookUp := NilVolID;
                Raise NoSuchNameForVol( Name)
            End

    End;  { VolIDLookUp }
            

Function VolNameLookUp( VID : VolID) : VolName;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Function returns the Volume Name of a Volume ID.
    {
    {  Parameters:
    {        VID - VolID.
    {
    {  Results:
    {        VolName corresponding to the VID. The VolumeName for
    {        a mounted Volume is set up by the Volume Mount Function.
    {
    {  Errors:
    {        - NoSuchVol, Raised if this VID is not Mounted.
    {
    {----------------------------------------------------------------------}

    Begin { VolNameLookUp }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    VolNameLookUp := PtrDCA^[VID].VolumeName
                End
            Else
                Begin
                    VolNameLookUp := '';
                    Raise NoSuchVol( Vid)
                End
    End;  { VolNameLookUp }



Function VolToOnVolAddr( VA : VolAddress) : OnVolAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Result given, in general, by
    {
    {    -        OnVolAddress :=  2^31 + 2^30
    {    -                          + VA.Volume * 2^27
    {    -                          + VA.BlockNumber * 2^8
    {
    {    Note that if VA.Volume > 3, use 7.
    {
    {  Parameters:
    {        VA - VolAddress
    {
    {  Results:
    {        - OnVolAddress, given by formula above.
    {
    {  Calls:
    {        - VolToLogAddr
    {        - VolNumberPages
    {
    {  Errors:
    {        - NoSuchVol (in VolNumberPages)
    {        - VBNOutofRange
    {
    {----------------------------------------------------------------------}

    Var
        VA1 : VolAddress;
        OVA : OnVolAddress;
        LA : LogAddress;
                
    Begin { VolToOnVolAddr }
        If VA.Volume > 3 Then VA1.Volume := 7 Else VA1.Volume := VA.Volume;
        VA1.BlockNumber := VA.BlockNumber;
        LA := VolToLogAddr( VA1) - 1073741824;
        VolToOnVolAddr := Recast( LA, OnVolAddress);
        If VA.BlockNumber >= VolNumberPages( VA.Volume)
            Then Raise VBNOutofRange( VA.Volume, VA.BlockNumber)
    End;  { VolToOnVolAddr }


Function OnVolToVolAddr( VID  : VolID;
                         OVA  : OnVolAddress ) : VolAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Result given by
    {
    {    -        VolAddress.Volume := VID
    {    -        VolAddress.BlockNumber := OVA Bits <26:8>
    {
    {    Note that the other bits of OVA are Ignored.
    {
    {  Parameters:
    {        OVA - OnVolAddress.
    {
    {  Results:
    {        - VolAddress, given by formula above.
    {
    {  Calls:
    {        - LogToVolAddr
    {        - VolNumberPages
    {
    {  Errors:
    {        - NoSuchVol (in VolNumberPages)
    {        - VBNOutofRange
    {
    {----------------------------------------------------------------------}

    Var
        LA : LogAddress;
        VA : VolAddress;
        
    Begin { OnVolToVolAddr }
        LA := Recast( OVA, LogAddress) + 1073741824;
        VA  := LogToVolAddr( LA);
        OnVolToVolAddr := VA;
        OnVolToVolAddr.Volume := VID;
        If VA.BlockNumber >= VolNumberPages( VID)
            Then Raise VBNOutofRange( VID, VA.BlockNumber)
    End;  { OnVolToVolAddr }


Function VolIDToPhyID( VID : VolID) : PhyDiskID; 
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Takes a Volume ID and returns that Volumes Physical Disk ID.
    {
    {  Parameters:
    {        VID - VolID.
    {
    {  Results:
    {        - PhyDiskID selected by given Volume ID.
    {
    {  Errors:
    {        - NoSuchVol
    {
    {----------------------------------------------------------------------}

    Begin { VolIDToPhyID }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    Case PtrDCA^[ VID].DskType Of
                        D5Inch, D8Inch, D14Inch, DCIOShugart, DCIOMicropolis :
                          Begin
                            VolIDToPhyID.Kind := IntDisk;
                            VolIDToPhyID.IntUnit := PtrDCA^[VID].DskUnit
                          End;
                        DSMD :
                          Begin
                            VolIDToPhyID.Kind := ExtDisk;
                            VolIDToPhyID.ExtUnit := PtrDCA^[VID].DskUnit
                            End;
                        DFloppy :
                          Begin
                            VolIDToPhyID.Kind := FlpDisk;
                            VolIDToPhyID.FlpUnit := PtrDCA^[VID].DskUnit
                          End
                      End { Case }
                End
            Else
                Raise NoSuchVol( VID)
    End;  { VolIDToPhyID }


Function PhyIDToVolID( PID : PhyDiskID) : VolID; 
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Takes a Physical Disk ID and returns that Disk's Volume ID.
    {
    {  Parameters:
    {        PID - PhyDiskID.
    {
    {  Results:
    {        - VolID of given Physical Disk ID.
    {
    {  Errors:
    {        - NoSuchDevice
    {
    {----------------------------------------------------------------------}

    Var
        VID1 : VolID;
        VID2 : VolID;
        Flag : Boolean;

    Begin { PhyIDToVolID }
        Flag := False;
        For VID1 := MinVolID To MaxVolID Do
            Begin
                If PtrDCA^[ VID1].DCBStatus.Mounted Then
                    Case PtrDCA^[ VID1].DskType Of
                        D5Inch, D8Inch, D14Inch, DCIOShugart, DCIOMicropolis :
                          If PID.Kind = IntDisk
                            Then If PID.IntUnit = PtrDCA^[ VID1].DskUnit
                                Then
                                    Begin
                                        Flag := True;
                                        VID2 := VID1
                                    End;
                        DSMD :
                          If PID.Kind = ExtDisk
                            Then If PID.ExtUnit = PtrDCA^[ VID1].DskUnit
                                Then
                                    Begin
                                        Flag := True;
                                        VID2 := VID1
                                    End;
                        DFloppy :
                          If PID.Kind = FlpDisk
                            Then If PID.FlpUnit = PtrDCA^[ VID1].DskUnit
                                Then
                                    Begin
                                        Flag := True;
                                        VID2 := VID1
                                    End;
                        Otherwise : Begin End
                    End { Case }
            End;
        If Flag
            Then PhyIDToVolID := VID2
            Else
                Begin
                    PhyIDToVolID := NilVolID;
                    Raise NoSuchDevice( PID)
                End
    End;  { PhyIDToVolID }

        

Function LastVolAddress( VID : VolID ) : VolAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Returns the Volume Address of the last Sector on the selected
    {    disk. Given by:
    {
    {    -         LastVolAddress.Volume := VID
    {    -         LastVolAddress.BlockNumber := VolNumberPages - 1
    {
    {  Parameters:
    {        VID - VolID
    {
    {  Results:
    {        LastVolAddress - VolAddress
    {
    {  Calls:
    {        - VolNumberPages
    {
    {  Errors:
    {        - NoSuchVol
    {
    {----------------------------------------------------------------------}

    Begin { LastVolAddress }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    LastVolAddress.Volume := VID;
                    LastVolAddress.BlockNumber := VolNumberPages( VID) - 1
                End
            Else
                Raise NoSuchVol( VID)
    End;  { LastVolAddress }


Function VolNumberPages( VID : VolId) : VolBlockNumber;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Returns the total number of Blocks on the Volume. Given by:
    {
    {    - VolNumberPages := ( PtrDCA^[ VID].PhysParameters.Cylinder
    {    -                   * PtrDCA^[ VID].PhysParameters.Head
    {    -                   * PtrDCA^[ VID].PhysParameters.Sector )
    {    -                   - PtrDCA^[ VID].PhysParameters.BootSize
    {
    {  Parameters:
    {        VID - VolID
    {
    {        PhyParameters in DCB
    {                             - Cylinder ( Number of Cylinder on Disk.)
    {                             - Head ( Number of Tracks per Cylinder.)
    {                             - Sector ( Number of Sectors per Track.)
    {                             - BootSize ( Number of Sectors for Boot.)
    {
    {  Results:
    {        VolNumberPages - VolBlockNumber ( Long)
    {
    {  Errors:
    {        - NoSuchVol
    {
    {----------------------------------------------------------------------}

    Begin { VolNumberPages }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
              with ptrdca^[vid].physparameters do
                VolNumberPages := stretch (cylinder) * Head * Sector - BootSize
            Else
                Raise NoSuchVol( VID)
    End;  { VolNumberPages }


Function FloppyIO( Ptr         : IOBufPtr;
                   Cmd         : VolIOCommand;
                   Command     : IOCommands;
                   PA          : Double;
                   VA          : VolAddress;
                   NumTries    : Integer;
                   DieOnError  : Boolean ) : Boolean;
    {----------------------------------------------------------------------}
    {
    {  Abstract:
    {    Used to perform IO to the Floppy by DoVolIO. Note that a floppy
    {    sector is 64 words Long. This means that 4 floppy sectors must
    {    be allocated for each Perq Disk Block (compressed of 256 words).
    {    This Function is called to perform each Floppy Sector transfer.
    {
    {  Parameters:
    {         Ptr          - IOBufPtr. A Buffer for data Read off
    {                            the Disk or the Buffer of data to be
    {                            Written onto the Disk.
    {         Cmd          - VolIOCommand. Read, Write, etc.
    {         Command      - IOCommands. Read, Write, etc.
    {         PA           - Physical Floppy Address. Includes Sector and
    {                            Cylinder Address.
    {         VA           - Volume Address. Includes Block Number as
    {                            well as VolID.
    {         NumberTries  - Integer. This is the number of times
    {                            DoVolIO will retry in the event of an
    {                            Error.
    {         DieOnError   - Boolean. If True and Operations fails after
    {                            retries then Raise a VolIOFailure
    {                            Exception. If False and Operation
    {                            succeeds then return True. If False and
    {                            Operation Fails even after retries then
    {                            return False.
    {
    {  Results:
    {        FloppyIO - Boolean. Indicates success or failure of the
    {                            requested Disk Operation.
    {        - A Floppy Disk Sector will be Written or Read.
    {
    {  Calls:
    {        - UnitIO
    {  Errors:
    {        - VolIOFailure
    {        - ( Others in Functions and Procedures which are called.)
    {
    {----------------------------------------------------------------------}

    Var
        Zero : Double;
        I, J : Integer;
        OK : Boolean;
        
    Begin { FloppyIO}
    
        Zero[ 0] := 1;
        Zero[ 1] := 0;
        
        OK := False;
        I := 0;
        Repeat
            I := I + 1;
            J := 0;
            Repeat
                J := J + 1;
                UnitIO( Floppy, Ptr, Command, WpFS*2, PA, Nil, StatPtr);
                If StatPtr^.SoftStatus = IOEIOC
                    Then
                        Begin
                            FloppyIO := True;
                            Exit( FloppyIO)
                        End
                    Else
                        Begin
                            Raise VolErrInc( StatPtr^.SoftStatus);
                            If StatPtr^.SoftStatus = IOEDNW Then
                                Begin
                                    If Not DieOnError Then
                                        Begin
                                            FloppyIO := False;
                                            Exit( FloppyIO);
                                        End;
                                    Raise VolIOFailure( 'FloppyIO: Failure',
                                        Cmd, VA, StatPtr^.SoftStatus)
                                End
                        End;
            Until OK Or ( J=5 );
            If Not OK And ( I*5 < NumTries )
                Then
                    Begin
                        { Show Recalibrate. }
                        RasterOp( RNot, LightWidth, LightHeight,
                            LightRecalibrate, LightY, SScreenW, SScreenP,
                            LightRecalibrate, LightY, SScreenW, SScreenP);
                        Raise VolErrInc( IOEFRS);
                        UnitIO( Floppy, Ptr, IOReset, 0, Zero, Nil, StatPtr);
                        RasterOp( RNot, LightWidth, LightHeight,
                            LightRecalibrate, LightY, SScreenW, SScreenP,
                            LightRecalibrate, LightY, SScreenW, SScreenP)
                    End;
        Until OK Or ( I*5 >= NumTries );
        FloppyIO := OK;
        If DieOnError
            Then Raise VolIOFailure( 'FloppyIO: Failure',
                                          Cmd, VA, StatPtr^.SoftStatus)
        
    End;  { FloppyIO }        



Function FloppyHeader ( PA          : Double;
                        VA          : VolAddress;
                        HdPtr       : IOHeadPtr;
                        DoWrite     : Boolean;
                        NumTries    : Integer;
                        DieOnError  : Boolean ) : Boolean;
    {----------------------------------------------------------------------}
    {
    {
    {  Abstract:
    {    Used to read/write a Floppy Header. Note that Headers are stored in
    {    Sector 1 of each Cylinder. A Cylinder has 6 Disk Blocks so
    {    6 Headers must be stored here.
    {
    {  Parameters:
    {         PA           - Physical Floppy Address. Includes Sector and
    {                            Cylinder Address.
    {         VA           - Volume Address. Includes Block Number as
    {                            well as VolID.
    {         HdPtr        - IOHeadPtr. A Buffer for the Header Read
    {                            off the Disk or the Buffer of the
    {                            Header to be Written onto the Disk.
    {         DoWrite      - Boolean. False if Read. True if Write.
    {         NumberTries  - Integer. This is the number of times
    {                            DoVolIO will retry in the event of an
    {                            Error.
    {         DieOnError   - Boolean. If True and Operations fails after
    {                            retries then Raise a VolIOFailure
    {                            Exception. If False and Operation
    {                            succeeds then return True. If False and
    {                            Operation Fails even after retries then
    {                            return False.
    {
    {  Results:
    {        FloppyIO - Boolean. Indicates success or failure of the
    {                            requested Disk Operation.
    {        - A Floppy Disk Sector will be Written or Read.
    {
    {  Calls:
    {        - FloppyIO
    {  Errors:
    {        - VolIOFailure
    {        - ( Others in Functions and Procedures which are called.)
    {
    {
    {----------------------------------------------------------------------}

    Var
        Cyl, Sec : Integer;
        Hack : IOBufPtr;
        OK : Boolean;
        Blk : Double;
    
    Begin
        Sec :=  Shrink(  ( VA.BlockNumber Mod (FSpT Div FSpDB))   );
        Cyl := PA[ 1];
        Blk[ 0] := 1;
        Blk[ 1] := Cyl;
        Hack := Recast( FHeadPtr, IOBufPtr);
        OK := FloppyIO(Hack, VolHdrRead, IORead, Blk,VA, NumTries, DieOnError);
        If DoWrite And OK
            Then
                Begin
                    FHeadPtr^[Sec Mod FHpS] := HdPtr^;
                    OK := FloppyIO( Hack, VolHdrWrite, IOWrite, Blk,
                            VA, NumTries, DieOnError)
                End
            Else
                Begin
                    HdPtr^ := FHeadPtr^[ Sec Mod FHpS]
                End;
        FloppyHeader := OK
    End;




Function DoVolIO( VA           : VolAddress;
                  DBPtr        : PtrVolBuffer;
                  HPtr         : PtrVolHeaderBuffer;
                  VolCommand   : VolIOCommand;
                  SectorCount  : Integer;
                  NumberTries  : Integer;
                  DieOnError   : Boolean ) : Boolean;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Function is used to set up the DCB Record when a Disk
    {    operation is to be done. Note that Disk Operations are directed
    {    toward 3 different classes of devices:
    {
    {            - CIO Shugart
    {            - Floppy
    {            - EIO Shugart, Micropolis and SMD
    {
    {  Parameters:
    {         VA           - Volume Address. Includes Block Number as
    {                            well as VolID.
    {         DBPtr        - PtrVolBuffer. A Buffer for data Read off
    {                            the Disk or the Buffer of data to be
    {                            Written onto the Disk.
    {         HPtr         - PtrVolHeaderBuffer. Buffer which for Logical
    {                            Header Read off the Disk or Buffer for
    {                            Logical Header to be compared with that
    {                            on the Disk during Check Operations.
    {         VolCommand   - VolIOCommand. Read, Write, etc.
    {         SectorCount  - Integer. Number of Sectors to be Read or
    {                            Formated. Only relevant for the Read
    {                            and Format Commands.
    {         NumberTries  - Integer. This is the number of times
    {                            DoVolIO will retry in the event of an
    {                            Error. Note that a recalibration, a Seek
    {                            to Sector 0, will be performed every
    {                            5 Errors until the operation has been
    {                            attempted NumTries times, at which time
    {                            the operation will be aborted and the
    {                            value False will be returned.
    {         DieOnError   - Boolean. If True and Operations fails after
    {                            retries then Raise a Disk Failure
    {                            Exception. If False and Operation
    {                            succeeds then return True. If False and
    {                            Operation Fails even after retries then
    {                            return False.
    {
    {  Results:
    {        DoVolIO - Boolean. Indicates success or failure of the
    {                            requested Disk Operation.
    {        - Usually Disk Sector(s) will be Written or Read.
    {        - The DCB will contain Status indicating what happened during
    {                the requested Operation.
    {
    {  Calls:
    {        - ToLogHeader
    {        - FromLogHeader
    {        - MapAddress
    {        - IncError
    {        - VolReset
    {        - UnitIO
    {        - FloppyIO
    {        - FloppyHeader
    {
    {  Errors:
    {        - VolIOFailure
    {        - NoSuchVol
    {        - VBNOutofRange
    {        - ( Others in Functions and Procedures which are called.)
    {
    {----------------------------------------------------------------------}

    Var
        I, J, K : Integer;   { k not used }
        VID     : VolID;
        VBlockN : VolBlockNumber;
        BlkNum  : VolBlockNumber;  {not used }
        FIOBuf  : IOBufPtr;  { not used }
        DIOBuf  : IOBufPtr;
        IOCom : IOCommands;
        LA : LogAddress;
        PA : Double;
        OK : Boolean;
        
        X : Packed Record
                Case Integer Of
                    0 : ( PVB : PtrVolBuffer );
                    1 : ( IOBP : IOBufPtr );
                    2 : ( OffSet : Integer;
                          SegNum : Integer )
                  End;


    Begin { DoVolIO }
        VID := VA.Volume;
        VBlockN := VA.BlockNumber;
        DIOBuf := Recast( DBPtr, IOBufPtr);
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    Case PtrDCA^[ VID].DskType Of
                        
                            D14Inch, D8Inch, D5Inch, DSMD, DCIOShugart,
                            DCIOMicropolis :
                            Begin
                                Case VolCommand Of
                                    VolRd : IOCom := IODiagRead;
                                    VolRdCheck :
                                        Begin
                                            IOCom := IORead;
                                            ToLogHeader( VID, HPtr, HdrPtr)
                                        End;
                                    VolWrCheck :
                                        Begin
                                            IOCom := IOWrite;
                                            ToLogHeader( VID, HPtr, HdrPtr)
                                        End;
                                    VolWr :
                                        Begin
                                            IOCom := IOWriteFirst;
                                            ToLogHeader( VID, HPtr, HdrPtr)
                                        End;
                                    VolReset : IOCom := IOReset
                                  End; { Case }
                                OK := False;
                                I := 0; { For Recalibrating after Retries. }
                                
                                PA := VolToPhyAddr( VA);
                                PA[ 1] := LOr( PA[ 1], Shift( VID, 13));

                                Repeat
                                    I := I+1;
                                    J := 0; { For retry. }
                                    Repeat
                                        J := J+1;
                                        UnitIO( HardDisk, DIOBuf, IOCom,
                                            ( WpDB*2 ), PA, HdrPtr, StatPtr);
                                        OK := ( StatPtr^.SoftStatus = IOEIOC );
                                        If Not OK Then 
                                          Raise VolErrInc(StatPtr^.SoftStatus);
                                    Until OK Or ( J = 5 )
                                        Or ( StatPtr^.SoftStatus = IOEADR );
                                    If Not OK And ( I*5 < NumberTries )
                                        Then
                                            Begin
                                                raise VolErrInc( IOEDRS);
                                                VolDiskReset( VID)
                                            End;
                                Until OK Or ( I*5 >= NumberTries );
                                If OK
                                    Then
                                        Begin
                                            If VolCommand = VolRd Then
                                                FromLogHeader(VID,HdrPtr,HPtr);
                                            DoVolIO := True
                                        End
                                    Else If DieOnError
                                        Then
                                            Begin
                                                DoVolIO := False;
                                                Raise VolIOFailure(
                                                    'DiskIO: Failure',
                                                    VolCommand, VA,
                                                    StatPtr^.SoftStatus);
                                                Exit( DoVolIO)
                                            End
                                        Else
                                            Begin
                                                DoVolIO := False;
                                                Exit( DoVolIO)
                                            End;
                            End;
                        
                        DFloppy :
                            Begin
                                PA := VolToPhyAddr( VA);
                                Case VolCommand Of
                                    VolRd :
                                        Begin
                                            IOCom := IORead
                                        End;
                                    VolRdCheck :
                                        Begin
                                            IOCom := IORead;
                                            ToLogHeader( VID, HPtr, HdrPtr)
                                        End;
                                    VolWrCheck :
                                        Begin
                                            IOCom := IOWrite;
                                            ToLogHeader( VID, HPtr, HdrPtr)
                                        End;
                                    VolWr :
                                        Begin
                                            IOCom := IOWriteFirst;
                                            ToLogHeader( VID, HPtr, HdrPtr)
                                        End;
                                    VolReset : IOCom := IOReset
                                  End; { Case }
                                Case IOCom Of
                                    IORead :
                                        OK := FloppyHeader( PA, VA, HdrPtr,
                                                False, NumberTries, DieOnError);
                                    IOWrite :
                                        OK := FloppyHeader( PA, VA, HdrPtr,
                                                True, NumberTries, DieOnError);
                                    IOWriteFirst :
                                        Begin
                                            OK := FloppyHeader( PA, VA, HdrPtr,
                                                True, NumberTries, DieOnError);
                                            IOCom := IOWrite
                                        End
                                  End { Case Command };
                                If Not OK
                                    Then
                                        Begin
                                            DoVolIO := False;
                                            Exit( DoVolIO)
                                        End;
                                X.PVB := DBPtr;
                                For I := 1 To FSpDB Do
                                    Begin
                                        OK := FloppyIO( X.IOBP, VolCommand,
                                                IOCom, PA, VA, NumberTries,
                                                DieOnError);
                                        X.OffSet := X.OffSet + WpFS;
                                        If Not OK
                                            Then
                                                Begin
                                                    DoVolIO := False;
                                                    Exit( DoVolIO)
                                                End;
                                        PA[ 0] := (((PA[ 0] - FFS) + 5)
                                                Mod FSpT) + FFS;
                                    End;
                                If OK
                                    Then
                                        Begin
                                            If VolCommand = VolRd Then
                                                FromLogHeader(VID,HdrPtr,HPtr);
                                        End;
                                DoVolIO := OK
                            End;
                        
                        Otherwise : { Need SMD Code?? And Error here. }
                        
                  End { Case }
                End
            Else
                Begin
                    Raise NoSuchVol( VID)
                End
    End;  { DoVolIO }



Procedure VolIO( VA          : VolAddress;
                 Ptr         : PtrVolBuffer;
                 HPtr        : PtrVolHeaderBuffer;
                 VolCommand  : VolIOCommand ) ;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This routine is used by the File System to perform Disk IO.
    {    It calls DoVolIO with a retry count of 15.
    {
    {  Parameters:
    {        VA - VolAddress
    {        Ptr - PtrVolBuffer
    {        HPtr - PtrVolHeaderBuffer
    {        VolCommand - VolIOCommand
    {
    {  Results:
    {        The Buffers are either Written onto the Disk, or Disk Data
    {        is read into the Buffers.
    {
    {  Calls:
    {        - DoVolIO
    {
    {  Errors:
    {        - ( See DoVolIO.)
    {
    {----------------------------------------------------------------------}

    Const
        SectorCount = 1;
        DieOnError = True;
        
    Var
        OK : Boolean;
        NumberTries : integer;
        
    Begin { VolIO }
        if ciodisktype = ciomicropolis then
          numbertries := 10
        else
          numbertries := 15;
        OK := DoVolIO( VA, Ptr, HPtr, VolCommand, SectorCount,
                    NumberTries, DieOnError);
    End;  { VolIO }



Function TryVolIO( VA          : VolAddress;
                   Ptr         : PtrVolBuffer;
                   HPtr        : PtrVolHeaderBuffer;
                   VolCommand  : VolIOCommand;
                   NumTries    : Integer ) : Boolean;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Function is used by the File System to perform Disk IO.
    {    It calls DoVolIO with a retry count of NumTries.
    {
    {  Parameters:
    {        VA - VolAddress
    {        Ptr - PtrVolBuffer
    {        HPtr - PtrVolHeaderBuffer
    {        VolCommand - VolIOCommand
    {        NumTries - Integer
    {
    {  Results:
    {        TryVolIO - Boolean. Indicates whether or not transfer was
    {        completed successfully.
    {        - The Buffers are either Written onto the Disk, or Disk Data
    {        is read into the Buffers.
    {
    {  Calls:
    {        - DoVolIO
    {
    {  Errors:
    {        - ( See DoVolIO.)
    {
    {----------------------------------------------------------------------}

    Const
        SectorCount = 1;
        DontDieOnError = False;
        
    Begin { TryVolIO }
        TryVolIO := DoVolIO( VA, Ptr, HPtr, VolCommand, SectorCount,
                        NumTries, DontDieOnError);
    End;  { TryVolIO }


Function VolMount( PID : PhyDiskID ) : VolID;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    The Mount Procedure is used to create a DCB for a particular
    {    drive and enter that DCB in the Disk Control Array. Returns the
    {    VID of the DCA entry which was used to Mount the Disk.
    {
    {  Parameters:
    {        PID - PhyDiskID
    {
    {  Results:
    {        VolMount - VolID
    {
    {  Calls:
    {        - Mount
    {
    {  Errors:
    {        - ( See Mount.)
    {
    {----------------------------------------------------------------------}

    Const
        Labelled = True;
        
    Begin { VolMount }
        VolMount := Mount( PID, Labelled)
    End;  { VolMount }


Procedure VolDisMount( PID : PhyDiskID );
    {----------------------------------------------------------------------
    {
    {  Abstract:
    { Volume DisMount
    {
    {    The DisMount Procedure Disolves the DCB for a particular Drive
    {    that was previously mounted, and frees up the Disk Control
    {    Array entry which was allocated for that DCB.
    {
    {  Parameters:
    {        PID - PhyDiskID
    {
    {  Results:
    {        - DisMounts the Disk.
    {
    {  Calls:
    {        - Dismount
    {
    {  Errors:
    {        - ( See DisMount.)
    {
    {----------------------------------------------------------------------}

    Begin { VolDisMount }
        Dismount( PID )
    End;  { VolDisMount }
    


Procedure VolDiskReset( VID : VolID);
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Used to Reset and Initialize the Disk Controller, Disk Drive
    {    and Disk uCode. Drive is Recalibrated.
    {
    {  Parameters:
    {        VID -  VolID. Of Disk to be affected.
    {
    {  Results:
    {        Drive and Controller is reset and reclibrated.
    {
    {  Calls:
    {        - UnitIO
    {
    {  Errors:
    {        - NoSuchVol
    {        - VolIOFailure
    {
    {----------------------------------------------------------------------}

    Var
        DAddress : Double;
        VA : VolAddress;
        I : Integer;

    Begin { VolReset }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    VA.Volume := VID;
                    VA.BlockNumber := 0;
                    Case PtrDCA^[ VID].DskType Of
                        D5Inch, D8Inch, D14Inch :
                            Begin
                                { Show Recalibrate. }
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP);
                                DAddress[ 0] := 0;
                                DAddress[ 1] := Shift( VID, 13);

                                UnitIO( EIODisk, BufPtr,
                                    IOReset, 0, DAddress, HdrPtr,
                                    StatPtr);

                                If ( Land( StatPtr^.HardStatus, #100) <> 0 )
                                    Then
                                        Begin
                                            Raise VolErrInc( IOET0);
                                            Raise VolIOFailure(
                                             'DiskReset: Cannot Find Track 0',
                                                VolReset, VA, IOEIOC)
                                        End;
                 
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP)
                            End;                            
                        DCIOShugart :
                            Begin
                                { Show Recalibrate. }
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP);
                                I := -1;
                                DAddress[ 0] := 0;
                                DAddress[ 1] := Shift( VID, 13);
                                Repeat { March Foreward 1 Track at a Time
                                         until we Reach Track 0 }
                                    I := I + 1;
                                    DAddress[0] := Shift( I, 8);
                                    StatPtr^.HardStatus := 0;
                                    UnitIO( HardDisk, BufPtr, IOSeek,
                                        0, DAddress, HdrPtr, StatPtr);
                                Until ( Land( StatPtr^.HardStatus, #20)
                                    <> 0) Or ( I = 255);
                                If ( Land( StatPtr^.HardStatus, #20) = 0)
                                    Then
                                        Repeat
                                            I := I - 1;
                                            DAddress[0] := Shift( I, 8);
                                            StatPtr^.HardStatus := 0;
                                            UnitIO( HardDisk, BufPtr,
                                                IOSeek, 0, DAddress,
                                                HdrPtr, StatPtr);
                                        Until ( Land( StatPtr^.HardStatus, #20)
                                            <> 0) Or ( I = 0);
                                If ( Land( StatPtr^.HardStatus, #20) = 0)
                                    Then
                                        Begin
                                            Raise VolErrInc( IOET0);
                                            Raise VolIOFailure(
                                             'DiskReset: Cannot Find Track 0',
                                                VolReset, VA, IOEIOC)
                                        End;
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP)
                            End;
                        
                        DCIOMicropolis :
                            Begin
                                { Show Recalibrate. }
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP);
                                DAddress[ 0] := 0;
                                DAddress[ 1] := Shift( VID, 13);
                                UnitIO( HardDisk, BufPtr,
                                    IOReset, 0, DAddress, HdrPtr,
                                    StatPtr);
                                If ( Land( StatPtr^.HardStatus, #300) <> #300 )
                                    Then   {seek complete not set or not ready}
                                        Begin
                                            Raise VolErrInc( IOET0);
                                            Raise VolIOFailure(
                                             'DiskReset: Drive not ready or not seek complete after Reset',
                                                VolReset, VA, IOEIOC)
                                        End;
                                   {Now send Restore (Seek 0) to cio Microp}
                                StatPtr^.HardStatus := 0;                                                       DAddress[ 0] := 0;
                                DAddress[ 1] := Shift( VID, 13);
                                UnitIO( HardDisk, BufPtr,
                                                IOSeek, 0, DAddress,
                                                HdrPtr, StatPtr);
                                If ( Land( StatPtr^.HardStatus, #67) <> 0)
                                    Then  {error bits set}
                                        Begin
                                          Raise VolErrInc( IOET0);
                                          Raise VolIOFailure(
                                           'DiskReset: Restore failed',
                                           VolReset, VA, statptr^.softstatus)
                                        End;
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP)
                            End;
                        
                        DFloppy :
                            Begin
                                { Show Recalibrate. }
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP);
                                
                                
                                (******
                                Raise VolErrInc( IOEFRS);    ******)
                                DAddress[ 0] := 0;
                                DAddress[ 1] := 0;
                                UnitIO( Floppy,BufPtr,IOReset,0,
                                            DAddress,HdrPtr,StatPtr);
                                            
                                { Check Status Here ???? }            
                                            
                                            
                                RasterOp( RNot,LightWidth,LightHeight,
                                    LightRecalibrate,LightY,SScreenW,SScreenP,
                                    LightRecalibrate,LightY,SScreenW,SScreenP)
                            End;
                        Otherwise :
                            Begin
                            End
                  
                  End { Case }
                End
            Else
                Begin
                    Raise NoSuchVol( Vid)
                End
    End.  { VolReset }


