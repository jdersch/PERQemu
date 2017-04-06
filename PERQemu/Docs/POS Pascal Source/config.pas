{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Configuration;
{---------------------------------------------------------------------
Copyright (C) 1982,, 1983 Three Rivers Computer Corporation

 ABSTRACT: Configuration exports a series of functions and variables which
           provide configuration information to POS system and application
           software.
  
 AUTHOR: C. Beckett 
----------------------------------------------------------------------}
{$Version V0.7 for POS}
{---------------------------------------------------------------------}
{
{ Change Log:
{
{ 12 Apr 83  V0.7  Chris Hughes
{            Modified for ICL CIO Micropolis.
             The microcode has been modified to return the following 
             SitCon.BoardRev values:

                0  CIO Shugart disk
                1  CIO Micropolis disk
               15  CIO No identifiable disk
               16  EIO

            Cf-Init picks up these values and sets up Cf_IOBoard (as before) 
            and also the variable CIODiskType (declared in DiskDef.pas).
{
{ 22 Feb 83  V0.6  Sandeep Johar
{            Added the landscape/portrait logic.
{
{ 18 Feb 83  V0.5  David Golub
{            Turned off debugging printout - won't work when
{            CF_Init is called early in system.
{
{  8 Feb 83  V0.4  C. Beckett
{            Fixed bugs.
{
{  3 Feb 83  V0.3  C. Beckett
{            Fixed bugs.
{
{  7 Jan 83  V0.1  C. Beckett
{            Added private logic.
{
{ 23 Nov 82  V0.0  C. Beckett
{            Created module.
{---------------------------------------------------------------------}

{>>>>>>>>>>>>>>>>>>>>} EXPORTS 


Type 

  Cf_MonitorType = (Cf_Landscape, Cf_Portrait);
  Cf_IOBoardType = (Cf_CIO, Cf_EIO); 
  
var

  Cf_KeyPad: boolean; 
          { If true, the keyboard attached to the Perq has an auxiliary }
          { keypad. }

  Cf_KeyboardStyle: integer; 
          { A number which indicates the style of keyboard attached to }
          { the Perq. 
          { This number is: 
          {     0: If the keyboard is the original one manufactured by 3RCC.
          {     1: If the keyboard is the VT100-compatable keyboard introduced
          {        with the Perq K1 ("Kristmas") model in Jan. 1983.}

  Cf_RS232Ports: integer; 
          { Holds the number of usable RS232 ports attached to the Perq. }

  Cf_RS232MaxSpeed: integer; 
          { Holds an integer which corresponds to the maximum baud rate
          { supported by the the Perq.  This number is coded to conform to
          { the format defined in the system module IO_Unit.             }

  Cf_Monitor: Cf_MonitorType; 
          { If Cf_Monitor, the Perq has a landscape monitor with 1024 X 1280 
          { resolution.  If Cf_Portrait, the Perq has a monitor with 
          { 1024 X 768 resolution. }

  Cf_WCSSize: integer;  
          { Code for the number of K words of the writeable control store.
          { 0 = 4Kwcs, 1 = 16Kwcs. }

  Cf_FloatingHardware: boolean; 
          { If true, the Perq has an Intel 8087 floating point chip. }

  Cf_BootUnit: integer; 
          { A number which indicates the logical unit number of the disk }
          { drive attached to the Perq from which the system was booted. }
          { 0 = Harddisk, 1 = floppy. }

  Cf_BootChar: char; 
          { A letter which indicates the identification of the particular }
          { system (WCS and main memory contents) loaded as part of boot. } 

  Cf_IOBoard: Cf_IOBoardType; 
          { Indicates the type of IO board attached to the Perq. }
          { This is: 
          {        
          { Cf_CIO: If the IO board is the 'Current' board for the original
          {         Perq.('CIO board')
          {        
          { Cf_EIO: If the IO board is the Ethernet IO board introduced
          {         with the Perq K1 ("Kristmas") model in Jan. 1983.
          {         ('EIO board' }

function Cf_Init: boolean;  
                          { Called by IO_Init. Performs initialization logic. }
                          { Should not be called by applications.  }
                          { Returns true unless fatal errors were  }
                          { detected.  }


{>>>>>>>>>>>>>>>>>>>>} PRIVATE 

imports Memory from Memory;
imports IO_Unit from IO_Unit;
imports diskdef from diskdef;

const

    BootInformation = 0;

type

    { Definition of the interface between SysB and Configuration module: }
    
    SitBootBlockLayout = packed record
      case integer of 
        
        1: (
           SitConfigWord1 : integer { Part of the SIT boot block }
           );                       { current label is 'XX'.     }

        2: (
           WCSSize : 0..15;   { This value is 0 for a 4K WCS and }
                              { 1 for a 16K WCS.  Other values   }
                              { are undefined.                   }
           Reserved : 0..3;  { Reserved for future use }
           IsPortrait : Boolean; { True => portrait, False => landscape }
           BoardRev : 0..31  { IO board revision } 
              {  0 CIO + Shugart
              {  1 CIO + Micropolis
              { 15 CIO + no known disk
              { 16 EIO }
           );

      end;
                 
var
    SitCon : SitBootBlockLayout;

const
    CF_Debug = false;
    

function Cf_Init: boolean;  
{---------------------------------------------------------------------}
{ABSTRACT:

 Called by system before IO_Init is called. Performs initialization logic.
 Should not be called by applications.  Returns true unless fatal errors
 were detected.  

{---------------------------------------------------------------------}
BEGIN
     
{ DECISION LOGIC FOR: IO board version }

SitCon.SitConfigWord1 := SIT^[BootInformation].Bootblock.XX;
{$ifc CF_Debug then}
writeln ('Sitcon.boardrev :', Sitcon.boardrev);
writeln ('Sitcon.wcssize :', Sitcon.wcssize);
{$endc}
Cf_IOBoard := Cf_CIO;  { Init IO board revision } 
Case SitCon.BoardRev of
    0:   CIODiskType := CIOShugart;
    1:   CIODiskType := CIOMicropolis;
    15:  CIODiskType := CIOUnknown;
    16:  begin
           Cf_IOBoard := Cf_EIO;  { IO board revision }
           CIODiskType := CIOUnknown
         end
end; { Case }


{ DECISION LOGIC FOR: IO board version }

Cf_WCSSize := SitCon.WCSSize ;  { One less than WCS size in Kwords }


{ DECISION LOGIC FOR: Monitor Information }

If SitCon.IsPortrait Then CF_Monitor := Cf_Portrait 
    Else Cf_Monitor := Cf_Landscape;
    
{ If Cf_Landscape, the Perq has a landscape monitor with }
{ 1024 X 1280 resolution.


{ DECISION LOGIC FOR: Boot Unit }

Cf_BootUnit := SIT^[BootInformation].Bootblock.DK;
          { A number which indicates the logical unit number of the disk}
          { drive attached to the Perq from which the system was booted.} 
          
          { Currently, this number may be 0 for floppy, or 1 for the    }
          { hard disk. F.2 allows a boot only from one hard disk.    } 


{ DECISION LOGIC FOR: Boot Character }

Cf_BootChar := chr(SIT^[BootInformation].Bootblock.CH);
          { A letter which indicates the identification of the particular }
          { system (WCS and main memory contents) loaded as part of boot. }
          { The letter will be a capitol if boot was from floppy. }


{ DECISION LOGIC FOR: Keyboard }

IF (Cf_IOBoard = Cf_CIO) THEN { Old 'CIO' board is attached to this machine }
    BEGIN
    Cf_KeyPad := false;
    Cf_KeyboardStyle := 0; {The keyboard is the original one mfg.ed by 3RCC}
    END
ELSE { Ethernet IO board is attached to this machine }
    BEGIN
    Cf_KeyPad := true; {The keyboard has an auxiliary keypad.}
    Cf_KeyboardStyle := 1;{Is the VT100-compatable keyboard introduced }
                          { with the Perq K1 ("Kristmas") model in Jan. 1983.}
    END;


{ DECISION LOGIC FOR: RS232 }

IF (Cf_IOBoard = Cf_CIO) THEN { Old 'CIO' board is attached to this machine }
    BEGIN
    Cf_RS232Ports := 1;
    Cf_RS232MaxSpeed := RS9600;
    END
ELSE { Ethernet IO board is attached to this machine }
    BEGIN
    Cf_RS232MaxSpeed := RS19200;
    Cf_RS232Ports := 2; 
    END;


{ DECISION LOGIC FOR: Floating point hardware. }

IF (Cf_IOBoard = Cf_CIO) THEN { Old 'CIO' board is attached to this machine }
    Cf_FloatingHardware := false
ELSE { Ethernet IO board is attached to this machine }
    Cf_FloatingHardware := true; { Has an Intel 8087 floating point chip. }
            { If true, the Perq has an Intel 8087 floating point chip. }

Cf_Init := true; { All went well: Indicate no errors }
  

END. { Cf_Init }

