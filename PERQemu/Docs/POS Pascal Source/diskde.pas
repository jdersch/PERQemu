{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module DiskDef;
{--------------------------------------------------------------------------
{
{ DiskDef -  TV. ( Tony Vezza )
{
{ Copyright (C) 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{       DiskDef exports variables, constants and Types to the
{       rest of the Pascal Disk SubSystem. Defines the control
{       structures for CIO and EIO disks. Contains a description
{       of the EIO disk uCode to Pascal interface.
{
{-------------------------------------------------------------------------}

{$Version V1.6 for POS}

{--------------------------------------------------------------------------
{
{ Change Log:
{
{ 26 Sep 83   SSJ   V1.6    
{           Added the disk parameters to the DIBlock structure. Only valid
{           for the 5.25 Inch disks.
{
{ 15 Aug 83   DBG   V.15   Modified for 5.25 inch disk
{           Add an extra word to DCB to hold cylinder to start precompensation
{           for writes to 5.25 inch disk.  Finally made DCB size a multiple
{           of 4 words.
{           Corrected the documentation for Disk Type to match the
{           type definition.
{
{ 12 Apr 83   CDH   V1.4   Modified for CIO Micropolis
            Add an extra word to DiskCtrlBlock format; this will simply be 
            ignored by shugart code, but will hold device/cyl for micropolis.
            New Disktype DCIOMicropolis added to exports replacing Rsvd08. 
            New variable CIODiskType added to exports.
{
{  2 Feb 83    TV   V1.3   More Documentation changes.
{
{ 24 Jan 83    TV   V1.2   Documentation format changed to allow
{                              the DOCUMENT Program to generate
{                              meaningful output.
{
{ 12 Jan 83
{              AGR  V1.1   Changed order of SMSt's in SMStatus.
{
{ 30 NOV 82
{              TV   V1.0   Module Created. Tony Vezza.
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}

Imports IO_Unit From IO_Unit;
Imports VolumeSystem From VolumeSystem;
Imports DiskIO From DiskIO;


Const
    DIBAddress = 0;                { Address Of Disk Information Block. }
    WpDB       = 256;              { Words Per Disk Block. }
    WpFS       = 64;               { Words Per Floppy Sector. }
    FSpDB      = WpDB Div WpFS;    { Floppy Sectors Per Disk Block. }
    FSpT       = 24;               { Floppy Sectors Per Track. }
    FFS        = 3;                { Floppy First Sector. }
    FIdS       = 1;
    FHpS       = WpFS Div 8;       { Word Size ( DiskHeader) }
                                   { Floppy Headers Per Sector. }
    DBpFT      = FSpT Div FSpDB;   { Disk Blocks Per Floppy Track. }

    FirstFC    = 5;                { First Floppy Cylinder }

    FirstDB    = 30;               { Shuggart - First Disk Block, Length
                                   {    of Boot Rounded to Track Boundary. }

Const
  DskBlockSize = 512;
  DskSPC = 30;         { Shuggart - Sectors per cylinder }
  DskHds = 8;          { Shuggart - Max number of disk heads }
  DskExHds = 0;        { Shuggart - Extra heads not in use }
  DskCyls = 202;       { Shuggart - Number of cylinders }

















Type

{////////////////////////////////////////////////////////////
 
      Disk Types and Disk Unit Number
 
         Four Bits are used to designate the Disk Drive type to the
         uCode and the DiskIO Pascal code. The bits are interpreted
         in the following manner. Note SMD is not yet defined. Also
         note the number of spare encodings.
 
             DiskType Code              Designated Drive Type
             \\\\\\\\\\\\\              \\\\\\\\\\\\\\\\\\\\\
                   0                      Reserved
                   1                      5.25 Inch Drive
                   2                     14 Inch Drive
                   3                      8 Inch Drive
                 4..15                    Reserved
 
         Legal Unit Numbers to the uCode are 4 bit quantities. This
         allows selection of sixteen drives ( Units 0 thru 15).
 
{////////////////////////////////////////////////////////////}

    { Define The HardDisk Types }
    DiskType = ( D5Inch, DUnused, D14Inch, D8Inch,
                 DSMD, DFloppy, DCIOShugart, DCIOMicropolis,
                 Rsvd07, Rsvd06, Rsvd05, Rsvd04,
                 Rsvd03, Rsvd02, Rsvd01, Rsvd00 );


    { Define Unit Number }
    UnitNumber = 0..15;



{////////////////////////////////////////////////////////////
 
  Disk Address Formats
 
     There are several Disk Address Formats which are used to access
     data on the Disks. These are:
 
             VolBlockNumber
             PhyVolAddress
             LogAddress
             OnVolAddress
 
     Each of these is a Long which can be used to Identify a unique
     block of data on the Disk. Mechanisms (routines) are provided
     for converting one format address to another format address.
 
     This following table lists relevant Physical Disk Data:
 
     PlatterSize  DriveType    Capacity  #Cylinders #Heads #Sectors
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |    SMD     |  CDC 9766  |  300MB  |    823   |  19  |   32   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |    SMD     |  CDC 9767  |  150MB  |    411   |  19  |   32   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |  8 Inch    |Micropolis21|   21MB  |    580   |   3  |   24   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |  8 Inch    |Micropolis35|   35MB  |    580   |   5  |   24   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |  8 Inch    |Micropolis70|   70MB  |   1160   |   5  |   24   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |  14 Inch   |   SA4000   |   12MB  |    202   |   4  |   30   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |  14 Inch   |   SA4002   |   24MB  |    202   |   8  |   30   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
    |  5.25 Inch |   Ampex    |   20MB  |    320   |   8  |   16   |
    +\\\\\\\\\\\\+\\\\\\\\\\\\+\\\\\\\\\+\\\\\\\\\\+\\\\\\+\\\\\\\\+
 
    Here:
         #Sectors     = Number of Sectors Per Head (or Track)
         #Heads       = Number of Heads Per Cylinder
         #Cylinders   = Number of Cylinders Per Drive
 
 
    Note that Head(s) is synonomous with Track(s).
 
    VolBlockNumber
    \\\\\\\\\\\\\\
      Description
        In this form a Disk appears to the File System as a
        linear one dimensional array of Blocks enumerated from
        0 to the maximim number of blocks on the Disk.
        The VolBlockNumber is actually a 19 bit quantity which
        can be calculated from the PhyVolAddress using
        the information in the table above:
 
        VolBlockNumber = Cylinder * ( #Heads * #Sectors )
                           + Head * #Sectors
                           + Sector
                           - BootSize
 
        Here Cylinder, Head and Sector are components
        of the PhysicalDiskAddress. #Heads and #Sectors are
        Disk parameters taken from the above table.
 
      Use
        All File System transactions to and from a disk use
        a VolBlockNumber to identify to/from which disk block
        the transaction will be made.
 
    PhyVolAddress
    \\\\\\\\\\\\\
      Description
        This is a Double with a Word Cylinder Address,
        a Byte Head (or Track) Address and a Byte Sector Address.
        A Physical Volume Address can be calculated from a
        Volume Block Number using the Data in the above table:
 
        Cyl = (VolBlockNumber+BootSize) Div (#Heads*#Sectors)
 
        Hd  = ((VolBlockNumber+BootSize) Mod (#Heads*#Sectors))
                            Div  #Sectors
 
        Sct = (VolBlockNumber+BootSize) Mod #Sectors
 
        Here again, Cylinder, Head and Sector are components
        of the PhysicalVolAddress. #Heads and #Sectors are
        Disk parameters taken from the above table.
        The operation ( A Div B ) means the Truncated Integer
        Quotient of A divided by B. The operation ( A Mod B )
        means the Remainder of A divided by B.
 
        For DFloppy:
                 PhyDskAdr[0] = Sct
                 PhyDskAdr[1] = Cyl
 
        For DCIOShugart:
                 PhyDskAdr[0] = Cyl*(2^8) + Hd*(2^5) + Sct
                 PhyDskAdr[1] = 0
 
        For D8Inch, D5Inch and D14Inch:
                 PhyDskAdr[0] = Hd*(2^8) + Sct
                 PhyDskAdr[1] = Cyl
 
      Use
        The Physical Volume Address is the format used to tell
        the uCode which disk address the read, write or seek
        transaction will involve.
 
    LogAddress
    \\\\\\\\\\
      Description
        The LogAddress is a repackaged form of the
        VolAddress. Packaged in the following way:
 
        LogAddress = ( VolID * 2^27 ) + ( VolBlockNumber * 2^8 )
 
    OnVolAddress
    \\\\\\\\\\\\
      Description
        The OnVolAddress is a repackage form of the
        VolAddress. Packaged in the following way:
 
        OnVolAddress = 2^31 + 2^30 + ( VolID * 2^27 )
                                           + ( BlockNumber * 2^8 )
 
      Use
        The OnVolAddress is used as Hints.
 
 
{////////////////////////////////////////////////////////////}

    LogAddress = Long;

    PhyVolAddress = Double;



{////////////////////////////////////////////////////////////
 
    State Machine Status Bits
 
        Definition of the Status Bits (taken from Hardware
        Specification):
 
        <2:0>   SMSt                Status:
          0        DIdle                State Machine Is Idle.
          1        DBusy                State Machine Is Busy.
          2        DataCRC              CRC Error in Data.
          3        PHMismatch           Physical Header Mismatch.
          4        LHMismatch           Logical Header Mismatch.
          5        HeadCRC              CRC Error in Logical or
                                            Physical Header.
                                            For Others - Not Used.
 
        <3>    SMInt                When Set Bits<2:0> Have Meaning.
 
        <4>    NotTrk0orNotSker     For Shugart or 5.25 inch,
                                        Used to Find
                                        Track 0 For Calibration
                                        Of Seeking Algorithm,
                                        For Others Indicates an
                                        Error While Trying to do
                                        a Seek Operation.
 
        <5>    NotFault             When Clear This Bit Indicates
                                        a Drive Problem. Causes are
                                        Specific to each Drive Type.
 
        <6>    NotOnCyl             Indicates That a Seek is still
                                        in Progress or That the
                                        Mechanism has not yet come
                                        To Rest.
 
        <7>    NotUnitReady         When 0 This Bit indicates that
                                        The Selected Drive is
                                        Present and Ready to be used.
 
        <8>    Index                This Bit Will Toggle (from 1 to 0
                                        or from 0 to 1) For Every
                                        Revolution of the Disk. Can
                                        Be Useful During Formatting.
 
        <10:9> DiskTypeCode          Drive Type Identification
            1                            Undefined
            0                            5.25 Inch Drive
            2                            14   Inch Drive
            3                            8    Inch Drive
 
        <15:11> Unused
 
 
{ ////////////////////////////////////////////////////////////}

    SMStatus = Packed Record
            SMSt               : ( DIdle,          { 000007 Bits<2:0> }
                                   DBusy,
                                   DataCRC,
                                   PHMismatch,
                                   LHMismatch,
                                   HeadCRC,
                                   AbnormalError,
                                   SMError );
            SMInt              : Boolean;         { 000010 Bit<3> }
            NotTrk0orNotSker   : Boolean;         { 000020 Bit<4> }
            NotFault           : Boolean;         { 000040 Bit<5> }
            NotOnCyl           : Boolean;         { 000100 Bit<6> }
            NotUnitReady       : Boolean;         { 000200 Bit<7> }
            IndexMark          : Boolean;         { 000400 Bit<8> }
            DkType             : ( Dk5Inch,      { 003000 Bits<10:9> }
                                   DkUnused,
                                   Dk14Inch,
                                   Dk8Inch );
            Unused             : 0..31            { 174000 Bits<15:11> }
        End;



{////////////////////////////////////////////////////////////
 
    Disk Control Block
 
      This Data Structure is the Primary Mechanism for communication
      between the DiskIO Pascal System and the Disk Perq uCode.
 
      The Structure is created for a particular Drive when that
      Drive is mounted.
 
      The Size of a DCB is 24 Bytes, or 12 Words, or 6 Long Words,
      or 3 Quad Words. A DCB will be Quad aligned in memory to
      facilitate access to it by uCode.
 
      Components of the DCB are:
 
      LastHead         <16> Bits
      \\\\\\\\
        Description
            Indicates the current Selection of the Heads.
        Written By
            UCode which Executes the Seek Command and the uCode
            which executes any implicit seeks needed by any of the
            other Commands.
        Read By
            UCode which Executes the Seek Command and the uCode
            which executes any implicit seeks needed by any of the
            other Commands.
 
      LastCylinder      <16> Bits
      \\\\\\\\\\\\
        Description
            Indicates the current position of the Heads.
        Written By
            UCode which Executes the Seek Command and the uCode
            which executes any implicit seeks needed by any of the
            other Commands.
        Read By
            UCode which Executes the Seek Command and the uCode
            which executes any implicit seeks needed by any of the
            other Commands.
 
      DummyDCBStatus      <8> Bits
      \\\\\\\\\\\\\\
        Description
            UnUsed Dummy Field.
 
      TypeDrive         <4> Bits
      \\\\\\\\\
        Description
            Indicates physical drive type.
        Written By
            DiskIO Pascal Code at Mount time.
        Read By
            UCode uses this data in selecting the correct drive
            and in formatting the disk address.
 
      UnitNumber        <4> Bits
      \\\\\\\\\\
        Description
            Selected Drive for this command. 0..15 are legal.
        Written By
            By microcode only to Mount Boot Disk.
            By DiskIO Pascal Code whenever a command is issued
            to the uCode.
        Read By
            uCode to select Drive.
 
      Command           <8> Bits, Upper <5> Bits Are 0
      \\\\\\\
        Description
            Command to the uCode. (See Command Descriptions above)
        Written By
            Pascal DiskIO Routine every time a transaction is
            performed with the Disk uCode.
        Read By
            UCode uses this field to determine what to do.
 
      SectorCount       <8> Bits
      \\\\\\\\\\\
        Description
            On WriteChecks this is the number of sectors to write.
            On Reads this is the number of sectors to read. For
            WriteFormat this is the number of Sectors to Format,
            should be the number of Sectors on a whole Track!!
        Written By
            DiskIO Pascal Code.
        Read By
            UCode which executes the Read and WriteFormat Commands.
 
      PhysicalAddress   <32> Bits
      \\\\\\\\\\\\\\\
        Description
            Disk address used by the uCode for any read write or
            seek operation.
        Written By
            DiskIO Pascal Code.
        Read By
            uCode to perform the Disk Seek and Access.
 
      HeaderBufferPointer     <32> Bits
      \\\\\\\\\\\\\\\\\\\
        Description
            Pointer to the memory buffer containing the Logical
            Header for all compare and write Logical Header
            operations. For reads this points to the buffer for
            storing the Logical Header of the Sector being
            read. Used to perform the DMA.
        Written By
            Pointer is set up by DiskIO Pascal Code.
        Read By
            uCode which performs the compare, write Logical
            Header or read Logical Header operation.
 
      DataBufferPointer <32> Bits
      \\\\\\\\\\\\\\\\\
        Description
            Pointer to the Memory Buffer for the Disk Data Transfer.
            Used to perform the DMA.
        Written By
            Pointer is set up by DiskIO Pascal Code.
        Read By
            uCode which initiates the Disk Data transfer.
 
      DskStatus         <8> Bits
      \\\\\\\\\
        Description
            Indicates the status of the DiskControllerStateMachine.
        Written By
            Disk uCode.
        Read By
            DiskIO Pascal code after all transactions with  uCode.
 
      PhysParameters    <48> Bits
      \\\\\\\\\\\\\\
        Description
            Indicates the maximum legal Cylinder, Head and Sector
            addresses (plus 1) for this drive.
            BootSize indicates what number of sectors to skip
            over at Cyl=0, Head=0 and Sector=0 before allocating
            Logical Blocks. This space is reserved for the Boot
            code.
        Written By
            Pascal Mount Procedure.
        Read By
            Pascal and uCode, to format and translate addresses.
 
      Volume Name       <72> Bits
      \\\\\\\\\\\
        Description
            Name of the Disk Volume as it appears in the DIB,
            of the Disk. Note the DIB is written by the
            Partition Program when the disk is initialized.
        Written By
            The Mount procedure which will read the DIB and enter
            the DIB.VolName into the DCB.
        Read By
            The procedures: VolIDLookUp and VolNameLookUp.
            They are used by the file system to access
            the Disks.
 
      Dummy         <8> Bits
      \\\\\
        Description
            UnUsed Dummy Field.
 
      DCBStatus         <8> Bits
      \\\\\\\\\
        Description
            Status of this entry in DiskControlArray.
        Written By
            DiskIO Initialization code, and Mount and Dismount
            Pascal Procedures.
        Read By
            Mount and Dismount Pascal Procedures.
 
      PrecompCyl        <16> Bits
        Description
            Cylinder inside of which writes must be precompensated
            for greater bit densities (5.25 inch disk).
        Written By
            Disk Mount code when determining disk type.
        Read By
            Disk IO Microcode
 
{////////////////////////////////////////////////////////////}

    DStatus = Packed Record
                    Ready        : Boolean;       { Currently UnUsed }
                    Free         : Boolean;
                    Mounted      : Boolean;
                    BootDevice   : Boolean;       { Currently UnUsed }
                    InProgress   : Boolean;       { Currently UnUsed }
                    Rsvd1        : 0..7           { To Fill Out Byte }
                End;


    EIODskCtrlBlock = Packed Record

            LastHead     : Integer;
            LastCylinder : Integer;             { Cylinder of last Seek, Rd, }
                                                {  RdCheck, WrFormat, Wr }
                                                {  or WrCheck operation. }

            DummyDCBStatus       : 0..255;
            
            DskType      : DiskType;            { 0..15 }
            DskUnit      : UnitNumber;          { 0..15 }

            Command      : 0..255;      { Legal DiskuCodeCommand = 0..7 }

            SectorCount  : 0..255;                { Only used during Reads. }

            PhysicalAddress     : PhyVolAddress;   { Double }

            HeaderBufferPointer : IOHeadPtr;        { Long }
            DataBufferPointer   : IOBufPtr;         { Long }

            DskStatus    : SMStatus;            { Word }

            PhysParameters      : Packed Record        { 5 Words }
                    Sector          : 0..255;
                    Head            : 0..255;
                    Cylinder        : Integer;
                    BootSize        : Integer;
                    DiskPages       : Long
                End;

            VolumeName   : VolName;           { String[8], or 9 Bytes }

            DummyByte    : 0..255;

            DCBStatus : DStatus;

            PrecompCyl : Integer;

        End;  { Case }

{////////////////////////////////////////////////////////////
 
    Disk Control Array
 
      The Disk Control Array is an array of DCB's. For every disk
      which has been Mounted there is an active DCB. When a Disk is
      Dismounted the active DCB for that Disk is made inactive.
 
      When a StartIO is issued to the uCode, it is given (on the
      E Stack) a VolumeID. This VolumeID is actually an Index to
      this array. The uCode then Procedes to determine what command
      to execute by examining the indexed (selected) DCB. All the
      information necessary for executing this command and returning
      results is contained in this DCB. In fact some information is
      also returned in the DCB by the uCode.
 
 
{////////////////////////////////////////////////////////////}

    DskCtrlArray = Packed Array [VolRangeType] Of EIODskCtrlBlock ;

    PtrDskCtrlArray = ^DskCtrlArray;

{////////////////////////////////////////////////////////////
 
  Commands to the EIO uCode
 
    The uCode will be capable of performing the following
    operations:
 
 
    Idle (ReadStatus) Command
    \\\\\\\\\\\\\\\\\\\\\\\\\
      Description
          Selects the given Drive, then returns the current
          State Machine Status and puts the State Machine in
          an Idle Loop.
      Use
          Can be used to get the Status of a Drive and
          Status of the State Machine.
      Inputs
          Items of the DiskControlBlock Which are used:
                Command = Idle
                DiskType          :  To Select Drive
                Unit Number       :  To Select Drive
      Outputs
          SMState Returned in DiskControlBlock after Unit
          is selected.
 
    WrCheck Command
    \\\\\\\\\\\\\\\
      Description
          This Command is used to check a Logical Header
          and write a Data Buffer to a Disk block. An
          implicit Seek is performed to the desired disk
          address. Single and Multi Sector WriteChecks are
          currently supported.
      Use
          This command can be used to write block(s)
          of an existing file without modifying the overall
          blocks utilized by the file. Used by the System
          System for Swapping.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = WriteCheck
              SectorCount       :  Number of Sectors to Write.
              DiskType          :  To Select Drive.
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
              LastCylinder      :  To Perform Seek.
              PhysicalAddress   :  Disk Address to WriteCheck.
              HeaderPointer     :  Address of Logical Header.
                                     For DMA and Compare.
              DataBufferPointer :  Address of Data Buffer. For
                                     DMA. Data written onto
                                     the Disk.
      Outputs
              Status            :  SMStatus Reg after
                                     WriteCheck is complete.
              LastCylinder      :  Current Cylinder designated
                                     by the PhysicalAddress.
    WrFirst Command
    \\\\\\\\\\\\\\\
      Description
          This Command is used to write a Logical Header
          and write a Data Buffer to a Disk block. An
          implicit Seek is performed to the desired disk
          address. No Checking of the old (on disk) Logical
          Header is performed. This function is used by
          the file system as FirstWrite. Only single Sector
          Writes are currently supported.
      Use
          This command is used to create new files and modify
          existing files. This command is used to perform
          File System Disk Writes.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = Write
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
              LastCylinder      :  To Perform Seek.
              PhysicalAddress   :  Disk Address to Write.
              HeaderPointer     :  Address of Logical Header.
                                     For DMA. Logical Header
                                     is Written onto the Disk
              DataBufferPointer :  Address of Data Buffer. For
                                     DMA. Data is written onto
                                     the Disk.
      Outputs
              Status            :  SMStatus Reg after
                                     Write is complete.
              LastCylinder      :  Current Cylinder designated
                                     by the PhysicalAddress.
 
    Format Command
    \\\\\\\\\\\\\\
      Description
          This command writes Physical Header, Logical
          Header and Data Block to the selected Disk Sectors.
          This command is used to format an entire track.
          The uCode must first seek the correct Cylinder.
          Select the appropriate track. Then wait for the
          Index pulse from the drive. When the Index is seen
          then the uCode must WriteFormat the number of Sectors
          given by SectorCount. Note that the Sector number
          of the PhysicalHeader must be initially cleared
          and incremented after each sector is transferred.
          The old data in the Disk Track is Lost.
          No provision is provided for formatting a single
          Sector in the middle of a track. It is hard to
          imagine how or why this would be done.
      Use
          The Command is used only in the Disk Formatting
          and Disk Initialization Procedures.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = FormatWrite
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
              LastCylinder      :  To Perform Seek.
              PhysicalAddress   :  Disk Address to Format.
                                    Initially the Physical
                                    Header to Write. Sector
                                    Number will be cleared
                                    then incremented by uCode.
              SectorCount       :  Number of Sectors to Format.
              HeaderPointer     :  Address of Logical Header.
                                    For DMA. Logical Header
                                    which is written. The
                                    same Logical Header is
                                    written to all Sectors
                                    to be Formatted by one
                                    command.
              DataBufferPointer :  Address of Data Buffer. For
                                    DMA. Data written onto
                                    the Disk. The same Data
                                    Block is written to all
                                    Sectors which are
                                    Formatted by one command.
      Outputs
              Status            :  SMStatus Reg after
                                    Write is complete.
              LastCylinder      :  Current Cylinder designated
                                    by the PhysicalAddress.
 
    RdCheck Command
    \\\\\\\\\\\\\\\
      Description
          This command compares Logical Header Data given
          as an argument with the Logical Header of the
          selected sector. The Logical Header and Data
          Block are read off the Disk. An implicit Seek
          is performed to the desired Cylinder. Multiple
          Sector Reads are supported by the uCode. When
          doing a Multiple Sector Read the first Sector is
          address by the PhysicalAddress. Subsequent Sectors
          are addressed by the LogicalHeaders of their
          Predecessor. The DataBuffer is assumed to be
          large enough to hold the Multiple Sector transfer.
          The HeaderBuffer is overwritten with the Logical
          Header of each Sector which is transferred. Thus
          it is only the size of one LogicalHeader. When
          the transfer is complete the HeaderBuffer will be
          the LogicalHeader of the last Sector Transferred.
      Use
          Not used to perform reads of files by the
          File System. Note that the contents of the Disk
          Sector's Logical Header must be known for this
          operation to succeed. Used by the System
          for Swapping.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = ReadCheck
              SectorCount       :  Number of Sectors to Read.
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
              LastCylinder      :  To Perform Seek.
              PhysicalAddress   :  Disk Address to ReadCheck.
              HeaderPointer     :  Address of Logical Header.
                                     For DMA and Compare.
                                     Logical Header of the
                                     Sector will go here.
              DataBufferPointer :  Address of Data Buffer. For
                                     DMA. Data will be Read
                                     from the disk into this
                                     buffer.
      Outputs
              HeaderPointer     :  HeaderBuffer will be loaded
                                     with the Logical Header
                                     of the Last Sector
                                     which is Read.
              DataBufferPointer :  DataBuffer will be loaded
                                     with the Data Block
                                     of the Sector(s) being Read.
              Status            :  SMStatus Reg after
                                     ReadCheck is complete.
              LastCylinder      :  Current Cylinder designated
                                     by the PhysicalAddress.
 
    DiagRead Command
    \\\\\\\\\\\\\\\\
      Description
          Read the Logical Header and Data Block of the
          specified Disk Sector. No checking of the Logical
          Header is performed. An Implicit Seek is performed
          to the selected Disk Cylinder. Note that Multiple
          Sector Reads are supported by the uCode. When
          doing a Multiple Sector Read the first Sector is
          address by the PhysicalAddress. Subsequent Sectors
          are addressed by the LogicalHeaders of their
          Predecessor. The DataBuffer is assumed to be
          large enough to hold the Multiple Sector transfer.
          The HeaderBuffer is overwritten with the Logical
          Header of each Sector which is transferred. Thus
          it is only the size of one LogicalHeader. When
          the transfer is complete the HeaderBuffer will be
          the LogicalHeader of the last Sector Transferred.
      Use
          Used when the Logical header of a Disk Sector is
          not known, but the Sector needs to be read, for
          example, in Scavenge.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = Read
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
              LastCylinder      :  To Perform Seek.
              PhysicalAddress   :  Disk Address to Read.
                                     When Reading more than
                                     one Sector this is only
                                     the address of the first.
              SectorCount       :  Number of Sectors to Read.
              HeaderPointer     :  Address of Logical Header.
                                     Logical Header of the
                                     Sector will go here.
              DataBufferPointer :  Address of Data Buffer. For
                                     DMA. Data will be Read
                                     from the disk into this
                                     buffer.
      Outputs
              HeaderPointer     :  HeaderBuffer will be loaded
                                     with the Logical Header
                                     of the last Sector Read.
              DataBufferPointer :  DataBuffer will be loaded
                                     with the Data Block
                                     of all the Sectors Read.
              Status            :  SMStatus Reg after
                                     Read is complete.
              LastCylinder      :  Cylinder of the last block
                                     which is read.
 
    Seek Command
    \\\\\\\\\\\\
      Description
          Move the Heads of the specified Drive to the
          specified Cylinder and Head Addresses.
      Use
          Used in Initialization, Mounting and Dismounting,
          and in Error Recovery. Used to determine size of
          some Disk types available in different sizes.
          Not used by the File System. The commands Write,
          WriteCheck, Read and ReadCheck all perform implicit
          seeks in the Disk uCode.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = Seek
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
              LastCylinder      :  To Perform Seek.
              PhysicalAddress   :  Disk Address to Seek. Only
                                     the Cylinder and Head
                                     Addresses are used.
                                     used.
      Outputs
              Status            :  SMStatus Reg after
                                     Read is complete.
              LastCylinder      :  Cylinder to which Seek
                                     has been done.
 
    Reset Command
    \\\\\\\\\\\\\
      Description
          Clears all error conditions in the controller and
          in the selected drive. Also performs an implied
          Seek to Cylinder 0. The uCode will Issue a Drive
          Restore Command.
      Use
          Used at initialization, Mounting, Dismounting and
          Error Recovery.
      Inputs
          Items of the DiskControlBlock Which are used:
              Command = Reset
              DiskType          :  To Select Drive.
              Unit Number       :  To Select Drive.
      Outputs
              Status            :  SMStatus Reg after
                                      Seek to 0 is complete.
              LastCylinder      :  Set to 0.
 
 
{////////////////////////////////////////////////////////////}

Type
  DskCmds = (DskIdle, DskRdCheck, DskDiagRead, DskWrCheck,
             DskWrFirst, DskFormat, DskSeek, DskClear);


  PDiskCtrlBlock = ^DiskCtrlBlock;
  DiskCtrlBlock = Packed Record     { This must be quad word aligned }
      Buffer       : IOBufPtr;      { Pointer to data buffer for transaction }
      DskCommand   : 0..255;
      DskNumSect   : 0..255;
      DskAddr      : Integer;       {for icl cio microp holds hd/sec}
      DskHeader    : IOHeader;
      Dsk          : DskResult;
      DskpNext     : PDiskCtrlBlock;
      DskDevCyl    : Integer;       {Added for icl cio microp, 
                                     dev (3 bits) and cyl (13) }
      
    End;


Type

    DIBlock = Packed Record
        
        BootSize : Integer;                 { blocks in boot }
        NumSector : Integer;
        NumHeads : Integer;
        NumCylinders : Integer;
        PreCompCylinder : Integer;
        DIB1Filler : array[1..109] of integer;  { Word }

        { String to use as name for this volume after mount. }
        VolumeName  : packed array[1..8] of char;

        { Hints of the addresses of the first and last logical }
        {  blocks of this volume. }
        VolumeStart: Long;
        VolumeEnd : Long;

        { Hints of the Partition Information Blocks for the }
        {  partitions of this volume. }
        SubParts  : array[0..63] of Long;

        DIB2Filler : array[1..2] of integer; { Word }

        case DeviceClass : DiskKinds of
                         FlpDisk : ( );
                         IntDisk : (IntDiskClass : IntDiskKinds);
                         ExtDisk : ( )

      End; { DIBlock }

    PDIBlock = ^DIBlock;


{////////////////////////////////////////////////////////////
 
    When Mapping this File System stuff onto the Floppy, FSpDB
    Floppy Sectors are used for each Disk Page. One Sector of each
    Track of the Floppy is used to hold all the Headers for the
    Pages that fit in the remaining Sectors of that Track. One
    Floppy Sectors per Track are not used.
 
{////////////////////////////////////////////////////////////}

Type
    FlopHdArray = Array [ 0..FHpS-1] of IOHeader; { Holds the Headers }
    FlopHeadPtr = ^FlopHdArray;


Var
    PtrDCA         : PtrDskCtrlArray;
    NumDCBUsed     : VolRangeType;
    IntDiskType    : IntDiskKinds;
    PtrVBuf        : PtrVolBuffer;
    PtrVHBuf       : PtrVolHeaderBuffer;
    StatPtr        : IOStatPtr;
    BufPtr         : IOBufPtr;     { note that a full disk buffer is not 
                                     allocated for this variable, 
                                     Use only with IOSeek, IOIdle, IOReset }
    HdrPtr         : IOHeadPtr;
    FHeadPtr       : FlopHeadPtr;


Var
    EIOFlag        : Boolean;
    PDskCtrl : PDiskCtrlBlock;
    Initialized : Integer;
    CIODiskType : (CIOShugart, CIOMicropolis, CIOUnknown);



Private

Procedure Dummy;
    Begin
    End.
