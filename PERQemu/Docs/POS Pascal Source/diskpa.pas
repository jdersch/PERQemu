Module DiskParams;
{----------------------------------------------------------------
{
{ Abstract:
{       This program maintains a data base of mappings from disks
{       to there parameters. The format of the data base is:
{
{       1. ! starts a comment terminated by the end of line character.
{       2. Each entry occupies 1 Line.
{    <Disk> <SecPerTrack> <Heads> <NumCylinder> <PreCompCyl> <BootSize>
{
{       Micropolis 16 8 256 128 32
{       this line says that the micropolis 5.25" drive has 16 sectors per 
{       track, 8 heads per cylinder, 256 cylinders, write pre-comp starts at 
{       cylinder 128 and the boot size is 32 sectors.
{
{ Copyright (C) 1984 - Perq Systems Corporation.
{----------------------------------------------------------------}


{----------------------------------------------------------------
{
{ Change Log:
{
{    18 Apr 84   V0.3  Sandeep Johar
{               For systems booted from hard disk, setupdisk now returns
{               the disk parameters.
{
{    30 Jan 84   V0.2   Sandeep Johar
{               1. Fix a bug that caused the setupdisk to loop forever
{                  if bad name was typed.
{               2. Change order of questions if the user is entering them
{                  to match order used by the help message.
{               3. If booted from hard disk then do not do anything.
{
{     1 Nov 83   V0.1   Sandeep Johar
{               1. Added the abstract etc for SetupDiskParams.
{               2. Fixed so the disk.params file can have comments.
{
{    26 Sep 83   V0.0   Sandeep Johar
{               Started.
{
{----------------------------------------------------------------}

EXPORTS

Function GetParms(DiskName: string; 
                  Var Head, Cyls, sectors, precompcyl, bootsize: Integer
                 ): Boolean;

procedure ParamHelp;

Procedure SetUpDiskParams(Automatic: Boolean;
                          DiskName: String;
                      Var NumHeads, 
                          NumCylinders, 
                          SecPerTrk,
                          BootSize, writecompcyl: Integer);

PRIVATE

imports sail_string from sail_string;
imports cmdparse from cmdparse;
imports stream from stream;
imports diskdef from diskdef;
imports system from system;

Const PARAMFILE = 'Disk.Params';

exception Error;

Procedure Parse(LineOfInfo : CString;
                Var DiskName : String;
                Var Heads, cyls,
                    sectors, precompcyl,
                    bootsize : Integer);
Var BrkChar : cstring;
    SDum : String;
    begin
    RemDelimiters(LineOfInfo, ' ', BrkChar);
    (*
    If BrkChar = '!' Then 
        Begin
        DiskName := '';
        Heads := 0;
        Cyls := 0;
        Sectors := 0;
        PreCompCyl := 0;
        Bootsize := 0;
        Exit(Parse);
        End;
        *)
    If BrkChar = CCR Then Raise Error;
    GetSymbol(LineOfInfo, DiskName, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    
    RemDelimiters(LineOfInfo, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    GetSymbol(LineOfInfo, SDum, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    Heads := CVD(SDum);

    RemDelimiters(LineOfInfo, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    GetSymbol(LineOfInfo, SDum, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    Cyls := CVD(SDum);

    RemDelimiters(LineOfInfo, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    GetSymbol(LineOfInfo, SDum, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    Sectors := CVD(SDum);

    RemDelimiters(LineOfInfo, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    GetSymbol(LineOfInfo, SDum, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    PreCompCyl := CVD(SDum);

    RemDelimiters(LineOfInfo, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    GetSymbol(LineOfInfo, SDum, ' ', BrkChar);
    If BrkChar = CCR Then Raise Error;
    Bootsize := CVD(SDum);

(*
    RemDelimiters(LineOfInfo, ' ', BrkChar);
    
    If (BrkChar <> CCR) And (BrkChar <> '!') Then Raise Error;*)
    end;

Function ParamHelp;
{----------------------------------------------------------------
{
{ Abstract: 
{       This routine is used to print out information about all known disks.
{
{----------------------------------------------------------------}
Var Fil : Text;
    diskname : string;
    head, sectors, cyls, precompcyl, bootsize : Integer;
    LineOfInfo : CString;

 Handler Error;
     Begin
     Writeln('** The Disk Data Base is in an incorrect format.');
     Exit(ParamHelp);
     end;
     
    begin
    reset(fil, PARAMFILE);
    Writeln;
    Writeln('  Disk      Heads   Cylinders   Sectors   WriteCompCyl  BootSize ');
    Writeln('-------------------------------------------------------------------');
    writeln;
    While not eof(fil) Do
        begin
        Readln(fil, LineOfInfo);
        Parse(LineOfInfo, DiskName, Head, cyls, sectors, precompcyl,bootsize);
        If DiskName <> '' Then
            Writeln(DiskName:8, '  ', Head:5 , '          ',  cyls:3, '   ',
                    sectors:5, '         ', precompcyl:5, '     ',
                    bootsize:5);
        end;
    close(fil);
    end;
                
    
Function GetParms(DiskName: string; 
                  Var Head, Cyls, sectors, precompcyl, bootsize: Integer
                 ): Boolean;
{----------------------------------------------------------------
{
{ Abstract:
{       This soutine is used to get the parameters of a particular disk.
{
{ Parameters:
{       DiskName - The name of the disk whose parameters are desired.
{       Head     - The number of heads in the disk.
{       Cyls     - The number of cylinders on the disk.
{       Sectors  - The number of sectors on the disk.
{       PreCompCyl-The cylinder at which precompensation should be started.
{       BootSize - The size of the boot area on the disk.
{
{ Returns:
{       True if could successfully find the parameters.
{
{----------------------------------------------------------------}
Var LineName : String;
    Fil : Text;
    LineOfInfo : CString;

    Handler ResetError(FileName: PathName);
        Begin
        Writeln('** File of Disk Parameters (', FileName:1, ') not found.');
        GetParms := False;
        Exit(GetParms);
        End;
        
    Handler Error;
        Begin
        Writeln('** Error in parsing the data base file.');
        GetParms := False;
        Exit(GetParms);
        End;

    begin
    reset(fil, PARAMFILE);
    CnvUpper(DiskName);
    While Not eof(Fil) Do
        begin
        readln(fil, LineOfInfo);
        Parse(LineOfInfo, LineName, Head, cyls, sectors, precompcyl,bootsize);
        CnvUpper(LineName);
        If LineName = DiskName Then
            Begin
            GetParms := True;
            Close(Fil);
            Exit(GetParms);
            End;
        End;
    GetParms := False;
    end;

{*********************** SetupParams **************************}
Procedure SetUpDiskParams(Automatic: Boolean;
                          DiskName: String;
                      Var NumHeads, 
                          NumCylinders, 
                          SecPerTrk,
                          BootSize, writecompcyl: Integer);
{----------------------------------------------------------------
{
{ Abstract:
{       This routine is used to initialize the Operating system with the
{       parameters of the actual 5.25" disk being used. The name of
{       the disk is passed as a parameter, and if not may be read from the 
{       console.
{
{ Parameters:
{       Automatic: If true use the DiskName (if not null) passed as the default
{       DiskName: Used if automatic is true. Should be null otherwise.
{
{ Returns:
{       NumHeads- the number of heads on the disk.
{       NumCylinders- the number of cylinders on the disk.
{       SecPerTrk- the number of sectors on each track.
{       Bootsize- the size in blocks of the bootarea.
{       WriteCompCyl- the cylinder at which write precompensation is turned on.
{
{ Side Effects-
{       Sets up the operating system structures with the appropriate 
{       information.
{
{----------------------------------------------------------------}
Label 1;
Var GotAnswer : Boolean;
    I : Integer;
    Ch :Char;

  Handler NotNumber(FileName: PathName);
      Begin
      Writeln('** Error in entering paramters.');
      Goto 1;
      End;
      
  Handler HelpKey(Var RetStr: Sys9s);
    Begin
    RetStr := '';
    ParamHelp;
    Writeln;
    Goto 1;
    end;
    
    Begin
    If sysdisk = 0 then 
      begin
      BootSize := PtrDCA^[0].PhysParameters.Bootsize;
      SecPerTrk := PtrDCA^[0].PhysParameters.Sector;
      NumHeads := PtrDCA^[0].PhysParameters.Head;
      NumCylinders := PtrDCA^[0].PhysParameters.Cylinder;
      WriteCompCyl := PtrDCA^[0].PreCompCyl;
      exit(SetupDiskParams);
      end;
    Writeln;
    Writeln('For the 5.25 Inch disk the disk parameters need to be supplied');
1:
    Write('Enter Name of disk, <HELP> for help[Unknown]: ');
    GotAnswer := False;
    If Automatic then
        If DiskName <> '' then
            begin
            GotAnswer := True;
            Writeln(DiskName);
            End;
    
    If Not GotAnswer then
        Begin
        If eoln then 
            Begin
            diskname := '';
            readln;
            end
        else
            readln(diskname);
        End;
    
    If DiskName = '' Then
        Begin
        Write('Would you like to enter the parameters yourself[Yes]: ');
        If not eoln then 
            Begin
            Readln(Ch);
            If (ch = 'n') Or (ch = 'N') Then goto 1;
            End
        Else Readln;
        Write('Number of heads: '); readln(NumHeads);
        Write('Number of cylinders: '); readln(NumCylinders);
        Write('Sectors Per Track: '); readln(SecPerTrk);
        Write('Write Precompensation Cylinder: '); readln(WriteCompCyl);
        Write('Bootsize: '); readln(Bootsize);

        PtrDCA^[0].PhysParameters.BootSize := BootSize;
        PtrDCA^[0].PhysParameters.Sector := SecPerTrk;
        PtrDCA^[0].PhysParameters.Head := NumHeads;
        PtrDCA^[0].PhysParameters.Cylinder := NumCylinders;
        PtrDCA^[0].PreCompCyl := WriteCompCyl;
        Exit(SetupDiskParams);
        End;
    
    If GetParms(DiskName, NumHeads, NumCylinders, 
                SecPerTrk, WriteCompCyl, Bootsize) Then 
        Begin
        PtrDCA^[0].PhysParameters.BootSize := BootSize;
        PtrDCA^[0].PhysParameters.Sector := SecPerTrk;
        PtrDCA^[0].PhysParameters.Head := NumHeads;
        PtrDCA^[0].PhysParameters.Cylinder := NumCylinders;
        PtrDCA^[0].PreCompCyl := WriteCompCyl;
        Exit(SetupDiskParams);
        End;
    Automatic := False;
    DiskName := '';
    GoTo 1;
    End.

Var head, cyl, sec, pre, boots : Integer;
begin
 If  GetParms('MICrop', Head, Cyl, sec, pre, boots) Then writeln('found');

end.
