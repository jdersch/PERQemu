{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module DiskUtility;
{--------------------------------------------------------------------------
{
{ DiskUtility -  TV ( Tony Vezza ). 
{ 
{ Copyright (C) 1983, Three Rivers Computer Corporation
{
{ Abstract:
{       DiskUtility exports procedures to the Vol SubSystem. Contains
{       procedures and functions to perform many disk operations.
{
{-------------------------------------------------------------------------}

{$Version V1.0 for POS}
{--------------------------------------------------------------------------
{
{ Change Log:
{
{ 26 Sep 83  SSJ  V1.0   Read the phys parameters from the disk for mic5
{
{  9 Sep 83  SSJ  V0.91  Added the GetDiskParameters procedure.
{
{ 15 Aug 83  DBG  V0.9   Modify for 5.25 inch disk.
{           For now - fake parameters for one kind of disk.
{           Later - read parameters from header of block 0 (?)
{           (will have to change iodisk so can't change header of that
{            block)
{
{ 13 Apr 83  CDH  V0.8   Modify for cio micropolis support:
            VInitialise changed to set intdsktype to mic8 for cio micropolis.
            CheckVolume modified to return DCIOMicropolis when necessary.   
            VolSize changed to set up size for DCIOMicropolis (530 
            cylinders only!). Code efficiency improved. 
            VolAddrToPhysAddr and PhysAddrToVolAddr treat DCIOMicropolis 
            like eio for physical addresses.

{
{ 22 Feb 83  SSJ  V0.7   Set IntDiskType to unsupported if uCode returns 
{                        the unused code.
{
{ 21 Feb 83  BAM  V0.6   Fixed bug in setting IntDiskKind.
{ 18 Feb 83  SJ   V0.5   GetVolName only looked at 6 chars now looks at all 8.
{                        Also was not returning the name right.
{
{  2 Feb 83  TV   V0.4   Cleaned up Documentation.
{
{  1 Feb 83  TV   V0.3   Fixed an OverFlow Error in Floppy Map and UnMap
{                        which should have raised a VBN Out of Range Error
{                        instead of OverFlow. Fixed CheckVolume to not Care
{                        about the Ready Bit in the SM Status register.
{                        is to allow for booting off the Floppy when the
{                        HardDisk is not working (not ready, actually).
{
{ 13 Jan 83  AGR  V0.2   STRETCHed some integers involved in long arithmetic.
{ 
{  7 Dec 82  SJ   V0.1   Fixed mount, initialize the disk and floppy units.
{                        in VInitialize.
{
{ 30 NOV 82  TV   V0.0   Created Module. Tony Vezza.
{
{
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}

Imports VolumeSystem From VolumeSystem;
Imports DiskDef From DiskDef;
Imports System From System;

    
Procedure VInitialize ;

Function  Mount( PID          : PhyDiskID;
                 Labelled     : Boolean ) : VolID;

Procedure DisMount( PID       : PhyDiskID );

Procedure InitDCB( VID        : VolID) ;

Function  CheckVolume( VID    : VolID;
                       DKind  : DiskKinds ) : DiskType;

Function  GetVolName( VID     : VolID) : VolName;

Function  GetDiskSize( VID : VolID) : Long;

Function  FreeDCB( VID        : VolID) : Boolean;

Procedure VolSize( VID        : VolID);

Function  VolToPhyAddr( VA : VolAddress) : Double;

Function  PhyToVolAddr(   VID : VolID;
                          PA  : PhyVolAddress) : VolAddress;

Procedure ToLogHeader(VID     : VolID;
                      PVolHead: PtrVolHeaderBuffer;
                      PLogHead: IOHeadPtr ) ;

Procedure FromLogHeader( VID      : VolID;
                         PLogHead : IOHeadPtr;
                         PVolHead : PtrVolHeaderBuffer ) ;

Function  PhyToLogAddr( VID : VolID; PA : PhyVolAddress) : LogAddress;

Function  LogToPhyAddr( LA  : LogAddress) : Double;

Function  VolToLogAddr( VA  : VolAddress) : LogAddress;

Function  LogToVolAddr( LA  : LogAddress ) : VolAddress;

Function  FlpyMap(   VA   : VolAddress) : Double;

Function  FlpyUnMap( VID  : VolID; 
                     PA   : PhyVolAddress) : VolAddress;

Procedure GetDiskParameters(Var Heads : Integer;
                            Var SectorsPerTrack : Integer;
                            Var NumCylinders: Integer);


{*****************************}   Private   {*****************************}

Imports IO_Unit From IO_Unit;
Imports Memory From Memory;
Imports PERQ_String From PERQ_String;

Procedure VInitialize ;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This procedure sets up the Disk Control Array and does the
    {    initialization of the DCB's within it.
    {
    {  Results:
    {        PtrDCA is created using a call to New. This allocates
    {        the System DCA.
    {
    {  Calls:
    {        - DCBInit
    {        - VolMount
    {        - StartIO
    {
    {----------------------------------------------------------------------}

    Var
        VID : VolID;
        PID : PhyDiskID;
        BootFromDisk : Boolean;
        DAddr : Double;
        
    Begin { VInitialize }
        
        If Initialized = #12345
            Then Exit(VInitialize)
            Else
                Begin
                
                    CreateSegment( DiskSegment, 11, 1, 11);
                    SetMobility( DiskSegment, UnSwappable);
                    New( DiskSegment, 256, PtrVBuf);
                    New( DiskSegment, 256, BufPtr);
                    New( DiskSegment, 4, HdrPtr);
                    New( DiskSegment, 1, StatPtr);
                    New( DiskSegment, 4, FHeadPtr);
                    
                    New( DiskSegment, 4, PtrVHBuf);
                
       { ????       For VID := MinVolID To MaxVolID Do
                        For I := IOEFirstError To IOELastError Do
                            ErrorCnt[ VID, I] := 0;   ???? }
                    
       { ????       For I := IOEFirstError To IOELastError Do
                        ErrorCnt[ I] := 0;            ???? }
                        
                    For VID := MinVolID To MaxVolID Do
                        Begin
                            InitDCB( VID)
                        End;

                    NumDCBUsed := 0;
                        
                    { Setup the DCBs for the single hard disk and the single
                      floppy device supported in F2. This should be fixed 
                      later.    }
                      
                    Pid.Kind := IntDisk;
                    Pid.IntUnit := 0;
                    VID := Mount(PID, False);
                    
                    PID.Kind := FlpDisk;
                    Pid.FlpUnit := 0;
                    VID := Mount(PID, False);
                    
                    { Does the FS need to know from where the System
                      has been Booted?? }
        
                    { Must Set Up, IntDiskType, for Later use. }
                    
                    If EIOFlag
                        Then
                             { Must Do and Idle Command To Get IntDiskType. }
                            Begin
                                DAddr[0] := 0;
                                DAddr[1] := 0;
                                UnitIO( HardDisk, BufPtr,
                                     IOIdle, 0, DAddr, HdrPtr,
                                     StatPtr);
                                Case PtrDCA^[ 0].DskStatus.DkType Of
                                    Dk5Inch  : IntDiskType := Mic5;
                                    Dk14Inch : IntDiskType := Shugart14;
                                    Dk8Inch  : IntDiskType := Mic8;
                                    DkUnUsed  : IntDiskType := UnSupported;
                                  End { Case }
                            End
                        Else
                            case ciodisktype of
                             cioshugart : IntDiskType := Shugart14;
                             ciomicropolis : IntDiskType := Mic8;
                             otherwise : IntDiskType := Unsupported
                            end
                End;
                
    End;  { VInitialize }


Function Mount( PID      : PhyDiskID;
                Labelled : Boolean ) : VolID;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    The Mount Procedure is used to create a DCB for a particular
    {    drive and enter that DCB in the Disk Control Array.
    {
    {  Parameters:
    {        PID -  PhyDiskID. Unit Number and Disk Kind to be Mounted.
    {        Labelled -  Boolean. When true Get Volume Name will be called
    {                    to read the DIB of the specified disk and
    {                    extract that Volume's Label. If False then the
    {                    DCB for the Disk will be set up without a
    {                    VolName. Note that the File System will always
    {                    Mount with Labelled = True.
    {
    {  Results:
    {        Mount - VolID. Identifies the DCB used to Mount the Disk.
    {
    {  Calls:
    {        - FreeDCB
    {        - InitDCB
    {        - CheckVolume
    {        - VolReset
    {        - VolSize
    {        - GetVolName
    {
    {  Errors:
    {        - NoSuchDevice
    {        - VMountErr
    {
    {----------------------------------------------------------------------}

    Var
        VID : VolID;
        Flag : Boolean;
        VolUnit : Integer;
    
    Begin { Mount }
    
        Case PID.Kind Of
            FlpDisk : VolUnit := PID.FlpUnit;
            IntDisk : VolUnit := PID.IntUnit;
            ExtDisk : VolUnit := PID.ExtUnit
        End; { Case }
        If VolUnit > 15
            Then
                Begin
                    Mount := NilVolID;
                    Raise NoSuchDevice( PID);;
                    Exit( Mount)
                End;

        Case PID.Kind Of
            FlpDisk :
                Begin
                    VID := 3;
                    Flag := False;
                    Repeat
                        VID := VID + 1;
                        If FreeDCB( VID) Then Flag := True
                    Until Flag Or ( VID = MaxVolID );
                    If Not Flag Then
                        Begin
                            Mount := NilVolID;
                            Raise VMountErr( PID);
                            Exit( Mount)
                        End
                End;
            IntDisk :
                Begin
    
                    VID := PID.IntUnit;
                    If Not FreeDCB( VID) Then
                        Begin
                            Mount := NilVolID;
                            Raise VMountErr( PID);
                            Exit( Mount)
                        End
                End;
            ExtDisk :
                Begin
                    VID := MaxVolID;
                    Flag := False;
                    Repeat
                        VID := VID - 1;
                        If FreeDCB( VID) Then Flag := True
                    Until Flag Or ( VID = MinVolID );
                    If Not Flag Then
                        Begin
                            Mount := NilVolID;
                            Raise VMountErr( PID);
                            Exit( Mount)
                        End
                End
          End; { Case }                        
                        
        InitDCB( VID);
        PtrDCA^[ VID].DskUnit := VolUnit;
        PtrDCA^[ VID].DCBStatus.Mounted := True;
    
        PtrDCA^[ VID].DskType := CheckVolume( VID, PID.Kind);
        If PtrDCA^[ VID].DskType = DUnUsed
            Then
                Begin
                    InitDCB( VID);
                    Mount := NilVolID;
                    Raise NoSuchDevice( PID);;
                    Exit( Mount)
                End
            Else
                Begin
                    VolDiskReset( VID);
                    VolSize( VID);
                    PtrDCA^[ VID].DCBStatus.Mounted := True;
                    PtrDCA^[ VID].DCBStatus.Free := False;
                    If Labelled Then
                        PtrDCA^[ VID].VolumeName := GetVolName( VID);
                    Mount := VID
                End;
            
    End;  { Mount }
        
        

Procedure DisMount( PID : PhyDiskID );
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    The DisMount Procedure Disolves the DCB for a particular Drive
    {    that was previously mounted, and frees up the Disk Control
    {    Array entry which was allocated for that DCB.
    {
    {  Parameters:
    {        PID - PhyDiskID
    {
    {  Results:
    {        DisMounts the Disk.
    {
    {  Calls:
    {        - InitDCB
    {        - PhyIDToVolID
    {
    {  Errors:
    {        - NoSuchDevice (See PhyIDToVolID Function.)
    {
    {----------------------------------------------------------------------}

    Var
        VID : VolID;
        
    Begin { DisMount }
        VID := PhyIDToVolID( PID);
        InitDCB( VID)
    End;  { DisMount }



Procedure InitDCB( VID: VolID) ;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Routine initializes the DCB selected by the indicated VID.
    {    All Entries of the DCB Record are set to a known state. In
    {    particular the DCB is marked as Free and UnMounted.
    {
    {  Parameters:
    {        VID - VolID
    {
    {  Results:
    {        - None Returned. But the Selected DCB is Initialized.
    {
    {----------------------------------------------------------------------}

    Var
        LocalDCB : EIODskCtrlBlock;
        
    Begin { InitDCB }
        
        LocalDCB.LastHead := 0;
        LocalDCB.LastCylinder := 0;

        LocalDCB.Command := 0;
        LocalDCB.SectorCount := 0;

        LocalDCB.PhysicalAddress[0] := 0;
        LocalDCB.PhysicalAddress[1] := 0;
                
        LocalDCB.HeaderBufferPointer := Nil;
        LocalDCB.DataBufferPointer   := Nil;
                
        LocalDCB.DskStatus.SMSt := DIdle;
        LocalDCB.DskStatus.SMInt := False;
        LocalDCB.DskStatus.NotTrk0OrNotSker := False;
        LocalDCB.DskStatus.NotFault := False;
        LocalDCB.DskStatus.NotOnCyl := True;
        LocalDCB.DskStatus.NotUnitReady := True;
        LocalDCB.DskStatus.IndexMark := False;
        LocalDCB.DskStatus.DkType := DkUnUsed;
        LocalDCB.DskStatus.UnUsed := 0;
                
        LocalDCB.PhysParameters.Cylinder    := 0;
        LocalDCB.PhysParameters.Head        := 0;
        LocalDCB.PhysParameters.Sector      := 0;
        LocalDCB.PhysParameters.BootSize    := 0;
        LocalDCB.PhysParameters.DiskPages    := 0;
        
        LocalDCB.DummyDCBStatus := 0;
        LocalDCB.DummyByte := 0;
        
        LocalDCB.DCBStatus.Ready := False;
        LocalDCB.DCBStatus.Free := True;
        LocalDCB.DCBStatus.Mounted := False;
        LocalDCB.DCBStatus.BootDevice := False;
        LocalDCB.DCBStatus.InProgress := False;
        LocalDCB.DCBStatus.Rsvd1 := 0;
                                
        LocalDCB.DskType := DUnUsed;
        LocalDCB.DskUnit := 0;
                
        LocalDCB.VolumeName   := '';
        
        PtrDCA^[ VID] := LocalDCB
                
    End;  { InitDCB }
                
                
Function CheckVolume( VID      : VolID;
                      DKind    : DiskKinds ) : DiskType;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This procedure is used to determine the Type of Disk being
    {    mounted.
    {
    {  Results:
    {        - DskType
    {
    {  Errors:
    {        - NoSuchVol
    {
    {----------------------------------------------------------------------}

    Var
        UNumber : Integer;
        DAddr : Double;
        
    Begin { CheckVolume }
        UNumber := PtrDCA^[ VID].DskUnit;
        If PtrDCA^[ VID].DCBStatus.Mounted
          Then
              Begin
                  Case DKind Of
                      FlpDisk :
                          Begin
                              CheckVolume := DFloppy;
                              If UNumber <> 0
                                  Then Raise NoSuchVol( VID)
                          End;
                      ExtDisk : CheckVolume := DSMD;
                      IntDisk :
                              If Not EIOFlag
                                Then
                                   Begin
                                     case ciodisktype of
                                       cioshugart : CheckVolume := DCIOShugart;
                                       ciomicropolis : 
                                          checkvolume := DCIOMicropolis;
                                       ciounknown : 
                                          begin
                                           checkvolume := DUnused;
                                           raise nosuchvol (vid);
                                          end
                                     end;
                                     If UNumber <> 0
                                              Then Raise NoSuchVol( VID)
                                   End
                               Else
                                      Begin
                                        DAddr[0] := 0;
                                        DAddr[1] := 0;
                                        UnitIO( HardDisk, BufPtr,
                                              IOIdle, 0, DAddr, HdrPtr,
                                              StatPtr);

            { This Code has been commented out so as to allow }
            { the EIO System to boot off a floppy even if the }
            { HardDisk is out to lunch. }
              
                 { If PtrDCA^[VID].DskStatus.NotUnitReady }
                     { Then Raise NoSuchVol( VID); }


                                        Case PtrDCA^[VID].DskStatus.DkType Of
                                              DkUnUsed :
                                                Begin
                                                  Raise NoSuchVol( VID);
                                                  CheckVolume := DUnUsed
                                                End;
                                              Dk5Inch :
                                                  CheckVolume := D5Inch;
                                              Dk8Inch :
                                                  CheckVolume := D8Inch;
                                              Dk14Inch :
                                                  CheckVolume := D14Inch
                                          End { Case }
                                      End
                End { Case }
              End
          Else
              Begin
                  Raise NoSuchVol( VID)
              End;
    End; { CheckVolume }

Function GetVolName( VID: VolID) : VolName;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Read the DIB, Disk Information Block, of the specified volume.
    {    The DIB is BlockNumber 0 on that Volume. Return the Volume
    {    Name from the Volume's DIB.
    {
    {  Parameters:
    {        VID - VolID
    {
    {  Results:
    {        GetVolName - VolName from Volume's DIB.
    {
    {  Calls:
    {        - VolIO
    {
    {----------------------------------------------------------------------}

    Var
        VName : VolName;
        Ch : Char;
        I : Integer;
        VAD : VolAddress;
        DIBTmp : PDIBlock;
        
    Begin { GetVolName }
        VAD.Volume := VID;
        VAD.BlockNumber := DIBAddress;
        VolIO( VAD, PtrVBuf, PtrVHBuf, VolRd );
        DIBTmp := Recast( PtrVBuf, PDIBlock);
        VName := '      ';
        For I := 1 To 8 Do
            Begin
                Ch := DIBTmp^.VolumeName[ I];
                If Ch = ' '
                    Then
                        Begin
                            Adjust( VName, I-1);
                            GetVolName := VName;
                            Exit( GetVolName)
                        End;
                VName[ I] := Ch
            End;
        GetVolName := VName;
    End; { VolName }


Function FreeDCB( VID: VolID) : Boolean;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Routine will see if a particular DCB Record is currently
    {    being used to mount a disk.
    {
    {  Parameters:
    {        VID - VolID
    {
    {  Results:
    {        FreeDCB - Boolean
    {
    {----------------------------------------------------------------------}

    Begin { FreeDCB }
        FreeDCB := PtrDCA^[VID].DCBStatus.Free
    End;  { FreeDCB }
    
    

Function GetDiskSize( VID : VolID) : Long;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {     This Function reads the DIB on the Volume and returns the
    {     Value of the last Disk Address in the DIB Record.
    {
    {----------------------------------------------------------------------}

    Var
        VAD : VolAddress;
        DIBTmp : PDIBlock;
        XAdr : Packed Record
                    Case Integer Of
                        0 : ( Lng : Long);
                        1 : ( Dbl : Double)
                      End;
        
    Begin
        VAD.Volume := VID;
        VAD.BlockNumber := DIBAddress;
        VolIO( VAD, PtrVBuf, PtrVHBuf, VolRd );
        DIBTmp := Recast( PtrVBuf, PDIBlock);
        XAdr.Lng := DIBTmp^.VolumeEnd;
        XAdr.Lng := XAdr.Lng Div 256;
        XAdr.Dbl[ 1] := LAnd( XAdr.Dbl[ 1], 7);
        GetDiskSize := XAdr.Lng + 1
    End;


Procedure VolSize( VID : VolID);
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Routine uses the Disk Seek Mechanism to determine the
    {    Sector, Head and  Cylinder Address Physical Parameters of a
    {    Disk. The possiblilities are (repeated from above)
    {
    { -    PlatterSize  DriveType    Capacity  #Cyls #Hds #Sect
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |    SMD     |  CDC 9766  |  300MB  | 823 | 19 |  32 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |    SMD     |  CDC 9767  |  150MB  | 411 | 19 |  32 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |  8 Inch    |Micropolis21|   21MB  | 580 |  3 |  24 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |  8 Inch    |Micropolis35|   35MB  | 580 |  5 |  24 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |  8 Inch    |Micropolis70|   70MB  | 1160|  5 |  24 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |  14 Inch   |   SA4000   |   12MB  | 200 |  4 |  30 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |  14 Inch   |   SA4002   |   24MB  | 200 |  8 |  30 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    { -   |  5.25 Inch |   Ampex    |   20MB  | 320 |  8 |  16 |
    { -   +////////////+////////////+/////////+/////+////+/////+
    {
    {  Parameters:
    {        - EIOFlag
    {        - IO24MByte
    {
    {----------------------------------------------------------------------}

    Var
        DSize : Long;
        DAddress : Double;
        BufPtr : IOBufPtr;
        HBufPtr : IOHeadPtr;

    Begin { VolSize }
        If PtrDCA^[ VID].DCBStatus.Mounted
          Then
            with PtrDCA^[ VID].PhysParameters do
              Begin
                  Case PtrDCA^[ VID].DskType Of
                      DCIOShugart, D14Inch :
                          Begin
                              BootSize := 30;
                              Sector := 30;
                              Head := 4;
                              Cylinder := 202;
                              If IO24MByte Then
                                  Head := 8
(********
                              DSize := GetDiskSize( VID);
                              DiskPages := DSize;
                              If DSize > 25000 Then
                                  Head := 8
                                                     *****)
                          End;
                      DCIOMicropolis :
                          Begin
                              BootSize := 24;
                              Sector := 24;
                              Head := 5;
                              Cylinder := 530;
                                {remaining cyls just dont happen for us!!}
                          End;
                      D8Inch :
                          Begin
                              BootSize := 48;
                              Sector := 24;
                              Head := 5;
                              Cylinder := 580;
(*********
                              DSize := GetDiskSize( VID);
                              DiskPages := DSize;
                              If DSize > 50000 Then
                                  Head := 5;
                              If DSize > 100000 Then
                                  Cylinder := 1160
                                                      ********)
                          End;
                      D5Inch :
                          Begin
                              { should read disk for this! }
                              {*************}
                              DAddress[1] := shift(VID, 13);{ cylinder 0 }
                              DAddress[0] := Shift(2, 8);   { head 2, sector 0}
                              HBufPtr := RECAST(PtrVHBuf, IOHeadPtr);
                              HBufPtr^.SerialNum := DAddress;
                              HBufPtr^.LogBlock := 0;
                              HBufPtr^.Filler := 0;
                              HBufPtr^.PrevAdr[0] := 0;
                              HBufPtr^.PrevAdr[1] := 0;
                              HBufPtr^.NextAdr[0] := 0;
                              HBufPtr^.NextAdr[1] := 0;
                              UnitIO(EIODisk, 
                                     RECAST(PtrVBuf, IOBufPtr), 
                                     IODiagRead, 
                                     512,
                                     DAddress,
                                     HBufPtr, 
                                     StatPtr);
                              BufPtr := RECAST(PtrVBuf, IOBufPtr);
                              {$R-}
                              BootSize := BufPtr^[0];  { first word of DCB }
                              Sector := BufPtr^[1];
                              Head := BufPtr^[2];
                              Cylinder := BufPtr^[3];
                              PtrDCA^[VID].PrecompCyl := BufPtr^[4];
                              {$R=}
(********
                              DSize := GetDiskSize( VID);
                              DiskPages := DSize
                                                      ********)
                              {*************}
                          End;
                      DFloppy :
                          Begin
                              BootSize := 30;
                              Sector := 6;
                              Head := 1;
                              Cylinder := 154;
(******
                              DSize := GetDiskSize( VID);
                              DiskPages := DSize;
                              If DSize < 500 Then
                                  Cylinder := 77
                                                     ******)
                                                     
                          End
                    End { Case }
              End
          Else
              Begin
                  Raise NoSuchVol( VID)
              End
    End;  { VolSize }



Function VolToPhyAddr( VA : VolAddress) : Double;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Takes the specified VolAddress and converts it to the
    {    corresponding Cylinder, Head and Sector address. Uses
    {    the Physical Parameter information in the DCB for the
    {    specified VID. Calculated using the following relations:
    {
    {    -    Cyl = (VA.BlkN+BootSize) Div (#Heads*#Sectors)
    {
    {    -    Hd  = ((VA.BlkN+BootSize) Mod (#Heads*#Sectors))
    {    -                          Div #Sectors
    {
    {    -    Sct = (VA.BlkN+BootSize) Mod #Sectors
    {
    {    Because the Floppy is interleaved these equations are not
    {    used for the floppy conversions. The Routine FlpyMap is used.
    {
    {    This Information must be packed differently for different
    {    Disk Types. Thus,
    {
    {    -    For Floppy
    {    -            DskPhyAddr[ 0] = Sct
    {    -            DskPhyAddr[ 1] = Cyl
    {
    {    -    For CIOShugart
    {    -            DskPhyAddr[ 0] = Cyl*(2^8) + Hd*(2^5) + Sct
    {    -            DskPhyAddr[ 1] = 0
    {
    {    -    For Micropolis on cio or eio
    {    -            DskPhyAddr[ 0] = Hd*(2^8) +Sct
    {    -            DskPhyAddr[ 1] = Cyl
    {
    {  Parameters:
    {        VA - VolAddress
    {
    {  Results:
    {        PhyToVolAddr - Double
    {
    {  Calls:
    {        - VolNumberPages
    {        - FlpyMap
    {
    {  Errors:
    {        - NoSuchVol
    {        - VBNOutofRange
    {
    {----------------------------------------------------------------------}

    Var
        VID : VolID;
        Sect, SctCyl : Long;
        PAddr : Double;
        BlkN1, BlkN2 : Long;
        Sct, Hd, Cyl : Integer;

    Begin { VolToPhyAddr }
    
        VID := VA.Volume;
        If PtrDCA^[ VID].DCBStatus.Mounted
          Then

            Begin
              If ( VA.BlockNumber = -1 )
                Then
                    Begin
                        VolToPhyAddr[ 0] := 0;
                        VolToPhyAddr[ 1] := 0;
                        Exit( VolToPhyAddr)
                    End;
              If PtrDCA^[ VID].DskType = DFloppy
                Then
                    VolToPhyAddr := FlpyMap( VA)
                Else
                  Begin
                    BlkN1 := VA.BlockNumber;
                    BlkN2 := BlkN1 + PtrDCA^[ VID].PhysParameters.BootSize;
                    Sect := PtrDCA^[ VID].PhysParameters.Sector;
                    SctCyl := Sect * PtrDCA^[ VID].PhysParameters.Head; 
                    Cyl := Shrink(   ( BlkN2 Div SctCyl)   );
                    Hd := Shrink(   (( BlkN2 Mod SctCyl ) Div Sect)   );
                    Sct := Shrink(   ( BlkN2 Mod Sect )   );
                    If EIOFlag or (ciodisktype = ciomicropolis)
                        Then
                            Begin
                                VolToPhyAddr[ 0] := Shift( Hd, 8) + Sct;
                                VolToPhyAddr[ 1] := Cyl
                            End
                        Else
                            Begin
                                VolToPhyAddr[ 0] := Shift( Cyl, 8) +
                                                       Shift( Hd, 5) + Sct;
                                VolToPhyAddr[ 1] := 0
                            End;
                        
                    If BlkN1 > VolNumberPages( VID)
                        Then Raise VBNOutofRange( VID, BlkN1)
                  End
            End
              
          Else
              Begin
                  Raise NoSuchVol( VID)
              End
    End;  { VolToPhyAddr }




Function PhyToVolAddr( VID  : VolID;
                       PA   : Double) : VolAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Function will convert a VID and a Physical Address to
    {    a Volume Address. Calculated using the relation:
    {
    {    -    VA.Volume = VID
    {
    {    -    VA.BlockNumber = Cylinder * (#Heads*#Sector)
    {    -                       + Head * #Sectors
    {    -                       + Sector
    {    -                       - BootSize
    {
    {    Note For Floppy Disks Flpy UnMap is Used. The Alogorithm
    {    described here does not apply.
    {
    {  Parameters:
    {        PA - Double
    {
    {  Results:
    {        PhyToVolAddr - VolAddress
    {
    {  Calls:
    {        - VolNumberPages
    {        - FlpyUnMap
    {
    {  Errors:
    {        - NoSuchVol
    {        - VBNOutofRange
    {
    {----------------------------------------------------------------------}

    Var
        BlkN : Long;
        Sct, Hd, Cyl : Integer;

    Begin { PhyToVolAddress }
        If PtrDCA^[ VID].DCBStatus.Mounted
          Then

            Begin
              If ( PA[ 0] = 0 ) And ( PA[ 1] = 0 )
                Then
                    Begin
                        PhyToVolAddr.Volume := VID;
                        PhyToVolAddr.BlockNumber := -1;
                        Exit( PhyToVolAddr)
                    End;
              If PtrDCA^[ VID].DskType = DFloppy
                Then
                    PhyToVolAddr := FlpyUnMap( VID, PA)
                Else
                  Begin
                    If EIOFlag or (ciodisktype = ciomicropolis)
                        Then
                            Begin
                                Sct := LAnd( PA[ 0], 255);
                                Hd := LAnd( Shift( PA[ 0], -8), 255);
                                Cyl := PA[ 1]
                            End
                        Else
                            Begin
                                Sct := LAnd( PA[ 0], 31);
                                Hd := LAnd( Shift( PA[ 0], -5), 7);
                                Cyl := LAnd( Shift( PA[ 0], -8), 255)
                            End;
                    BlkN := Sct;
                    BlkN := BlkN - PtrDCA^[ VID].PhysParameters.BootSize;
                    BlkN := BlkN + ( stretch(Hd)
                                  * PtrDCA^[ VID].PhysParameters.Sector);
                    BlkN := BlkN + ( stretch(Cyl)
                                  * PtrDCA^[ VID].PhysParameters.Head
                                  * PtrDCA^[ VID].PhysParameters.Sector);
                    PhyToVolAddr.Volume := VID;
                    PhyToVolAddr.BlockNumber := BlkN;
                    If BlkN > VolNumberPages( VID)
                        Then Raise VBNOutofRange( VID, BlkN)
                End
            End

          Else
              Begin
                    Raise NoSuchVol( VID)
              End
    End;  { PhyToVolAddress }


Procedure ToLogHeader( VID      : VolID;
                       PVolHead : PtrVolHeaderBuffer;
                       PLogHead : IOHeadPtr ) ;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Procedure is called to convert a File System Logical
    {    Header to a Real On Disk Logical Header. The Conversion
    {    involves changing Volume Addresses to Physical Disk Addresses.
    {
    {  Parameters:
    {        VID - VolID
    {        PVolHead - PtrVolHeaderBuffer
    {        PLogHead - IOHeadPtr
    {
    {  Results:
    {        - The IOHeadPtr is set up.
    {
    {  Calls:
    {        - VolToPhyAddress
    {
    {  Errors:
    {        - NoSuchVol
    {        - VBNOutofRange (In VolToPhyAddress Routine.)
    {
    {----------------------------------------------------------------------}

    Var
        VA : VolAddress;
    
    Begin { ToLogHeader }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    VA.Volume := VID;
                    VA.BlockNumber := PVolHead^.SerialNumber;
                    PLogHead^.SerialNum := VolToPhyAddr( VA);
                    PLogHead^.LogBlock := PVolHead^.SegmentBlockNumber;
                    PLogHead^.Filler := PVolHead^.FreeListHint;
                    VA.BlockNumber := PVolHead^.PreviousBlock;
                    PLogHead^.PrevAdr := VolToPhyAddr( VA);
                    VA.BlockNumber := PVolHead^.NextBlock;
                    PLogHead^.NextAdr := VolToPhyAddr( VA);
                End
            Else
                Raise NoSuchVol( VID)
    End;  { ToLogHeader }


Procedure FromLogHeader( VID      : VolID;
                         PLogHead : IOHeadPtr;
                         PVolHead : PtrVolHeaderBuffer ) ;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    This Procedure is called to convert a  Logical Real On Disk
    {    Header to a File System Logical Header. The Conversion
    {    involves changing Physical Addresses to Volume Disk Addresses.
    {
    {  Parameters:
    {        VID - VolID
    {        PLogHead - IOHeadPtr
    {        PVolHead - PtrVolHeaderBuffer
    {
    {  Results:
    {        - The PtrVolHeaderBuffer is set up.
    {
    {  Calls:
    {        - PhyToVolAddress
    {
    {  Errors:
    {        - NoSuchVol
    {        - VBNOutofRange (In PhyToVolAddress Routine.)
    {
    {----------------------------------------------------------------------}
    Var
        PA : Double;       
        VA : VolAddress;
            
    Begin { FromLogHeader }
        If PtrDCA^[ VID].DCBStatus.Mounted
            Then
                Begin
                    PA := PLogHead^.SerialNum;
                    VA := PhyToVolAddr( VID, PA);
                    PVolHead^.SerialNumber := VA.BlockNumber;
                    PVolHead^.SegmentBlockNumber := PLogHead^.LogBlock;
                    PVolHead^.FreeListHint := PLogHead^.Filler;
                    PA := PLogHead^.PrevAdr;
                    VA := PhyToVolAddr( VID, PA);
                    PVolHead^.PreviousBlock := VA.BlockNumber;
                    PA := PLogHead^.NextAdr;
                    VA := PhyToVolAddr( VID, PA);
                    PVolHead^.NextBlock := VA.BlockNumber;
                End
            Else
                Raise NoSuchVol( VID)
    End;  { FromLogHeader }



Function PhyToLogAddr( VID : VolID; PA : Double) : LogAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Takes VID and PA and calls PhyToVolAddress to get a VolAddress.
    {    This VolAddress is then converted into a LogAddress by
    {    VolToLogAddress.
    {
    {  Parameters:
    {        VID - VolID
    {        PA  - Double
    {
    {  Results:
    {        PhyToLogAddr - LogAddress.
    {
    {  Calls:
    {        - PhyToVolAddr
    {        - VolToLogAddr
    {
    {  Errors:
    {        - NoSuchVol (In PhyToVolAddr)
    {        - VBNOutofRange (In PhyToVolAddr)
    {
    {----------------------------------------------------------------------}

    Var
        VA : VolAddress;
        
    Begin { PhyToLogAddr }
        VA := PhyToVolAddr( VID, PA);
        PhyToLogAddr := VolToLogAddr( VA)
    End; { PhyToLogAddr }
      


Function LogToPhyAddr( LA : LogAddress) : Double;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Uses LogToVolAddress to convert LA to a VolAddress. Then calls
    {    VolToPhyAddress to get the PhyVolAddress.
    {
    {  Parameters:
    {        LA - LogAddress
    {
    {  Results:
    {        LogToPhyAddr - Double
    {
    {  Calls:
    {        - LogToVolAddr
    {        - VolToPhyAddr
    {
    {  Errors:
    {        - NoSuchVol (In VolToPhyAddr)
    {        - VBNOutofRange (In VolToPhyAddr)
    {
    {----------------------------------------------------------------------}

    Var
        VA : VolAddress;
        
    Begin { LogToPhyAddr }
        VA := LogToVolAddr( LA);
        LogToPhyAddr := VolToPhyAddr( VA)
    End; { LogToPhyAddr }
      
  


Function VolToLogAddr( VA : VolAddress) : LogAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Repacks a Volume Address in Logical Address Format. The
    {    Logical Address is used as a parameter to the UnitIO and
    {    DskUnitIO Routines.
    {    Result given by:
    {
    {    -        LogAddress := VA.Volume * 2^27 + VA.BlockNumber * 2^8
    {
    {  Parameters:
    {        VA - VolAddress
    {
    {  Results:
    {        - LogAddress, given by formula above.
    {
    {----------------------------------------------------------------------}

    Var
        DA1, DA2 : Packed Record
                        Case Integer Of
                              0 : ( Lng : Long);
                              1 : ( Dbl : Double)
                          End;
        LA : LogAddress;
                
    Begin { VolToLogAddr }
        DA2.Lng := VA.BlockNumber;
        DA1.Dbl[ 1] :=  Shift( VA.Volume, 11)
                      + Shift( DA2.Dbl[ 1], 8)
                      + Shift( DA2.Dbl[ 0], -8);
        DA1.Dbl[ 0] := Shift( DA2.Dbl[ 0], 8);
        VolToLogAddr := DA1.Lng
    End;  { VolToLogAddr }


Function LogToVolAddr( LA : LogAddress ) : VolAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Repacks a Logical Address into a Volume Address.
    {    Result given by:
    {
    {    -        VolAddress.Volume := LA Bits <29:27>
    {    -        VolAddress.BlockNumber := LA Bits <26:8>
    {
    {  Parameters:
    {        LA - OnVolAddress.
    {
    {  Results:
    {        - VolAddress, given by formula above.
    {
    {----------------------------------------------------------------------}

    Var
        DA1, DA2 : Packed Record
                        Case Integer Of
                              0 : ( Lng : Long);
                              1 : ( Dbl : Double)
                          End;
        VID : VolID;
        
    Begin { LogToVolAddr }
        DA1.Lng := LA;
        DA2.Dbl[ 1] := LAnd( Shift( DA1.Dbl[ 1], -8), 7);
        DA2.Dbl[ 0] := Shift( DA1.Dbl[ 1], 8) + Shift( DA1.Dbl[ 0], -8);
        VID := ReCast( LAnd( Shift( DA1.Dbl[ 1], -11), 7), VolID);
        LogToVolAddr.BlockNumber := DA2.Lng;
        LogToVolAddr.Volume := VID
    End;  { LogToVolAddr }

Function FlpyMap( VA : VolAddress) : Double;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Convert a Volume Address to a Floppy Physical Address.
    {
    {  Parameters:
    {        VA - VolAddress
    {
    {  Results:
    {        FlpyMapAddr - Double
    {
    {  Calls:
    {        - LastVolAddress
    {
    {  Errors:
    {        - NoSuchVol
    {        - VBNOutofRange
    {
    {----------------------------------------------------------------------}

    Var
        Sec : Integer;
        Cyl : Integer;
        VID : VolID;
        
    Begin { FlpyMap }
        VID := VA.Volume;
        If PtrDCA^[VID].DCBStatus.Mounted And ( PtrDCA^[VID].DskType = DFloppy)
            Then
                Begin
                    If VA.BlockNumber > VolNumberPages( VID)
                        Then Raise VBNOutOfRange( VID, VA.BlockNumber);
                    Sec := Shrink(  (VA.BlockNumber * FSpDB)  );
                    Cyl := Sec Div FSpT + FirstFC;
                    Sec := ( Sec * 5) Mod FSpT;
                    Sec := Sec + FFS;
                    FlpyMap[ 0] := Sec;
                    FlpyMap[ 1] := Cyl
                End                    
            Else
                Raise NoSuchVol( VID)
    End; { FlpyMap }    


Function FlpyUnMap( VID  : VolID; 
                    PA   : Double) : VolAddress;
    {----------------------------------------------------------------------
    {
    {  Abstract:
    {    Convert a Floppy Physical Address to a Volume Address.
    {
    {  Parameters:
    {        VID  - VolID
    {        PA   - Double
    {
    {  Results:
    {        FlpyUnMapAddress - VolAddress
    {
    {  Calls:
    {        - LastVolAddress
    {
    {  Errors:
    {        - NoSuchVol
    {        - VBNOutofRange
    {
    {----------------------------------------------------------------------}

    Var
        Sec : Integer;
        Cyl : Integer;
        BlkNum : Long;
        
    Begin { FlpyUnMap }
        If PtrDCA^[VID].DCBStatus.Mounted And ( PtrDCA^[VID].DskType = DFloppy)
            Then
                Begin
                    Cyl := PA[ 1] - FirstFC;
                    Sec := PA[ 0] - FFS;
                    Sec := (( Sec Mod 5) * 5) + ( Sec Div 5);;
                    BlkNum := ( (( Cyl * FSpT) + Sec ) Div FSpDB );
                    FlpyUnMap.Volume := VID;
                    FlpyUnMap.BlockNumber := BlkNum;
                    If BlkNum > VolNumberPages( VID)
                        Then Raise VBNOutOfRange( VID, BlkNum)
                End                    
            Else
                Raise NoSuchVol( VID)
    End; { FlpyUnMap }    

Procedure GetDiskParameters(Var Heads : Integer;
                            Var SectorsPerTrack : Integer;
                            Var NumCylinders: Integer);
    Var vid : integer;
        DAddress : Double;
    Begin
    Heads := PtrDCA^[0].PhysParameters.Head;
    NumCylinders := PtrDCA^[0].PhysParameters.Cylinder;
    SectorsPerTrack := PtrDCA^[0].PhysParameters.Sector;
    End.


    vid := 0;       { we have to fake it here. }
    DAddress[1] :=  shift(VID, 13);{ cylinder 0 }
    DAddress[0] := Shift(2, 8);   { head 2, sector 0}
    UnitIO(EIODisk, 
           RECAST(PtrVBuf, IOBufPtr), 
           IORead, 
           512,
           DAddress,
           HdrPtr, 
           StatPtr);
    SectorsPerTrack := BufPtr^[1];
    Head := BufPtr^[2];
    NumCylinders := BufPtr^[3];
    end.


    Heads := 8;
    SectorsPerTrack := 16;
    NumCylinders := 154;
    end.



