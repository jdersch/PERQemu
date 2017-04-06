{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IOErrMessages;
{------------------------------------------------------------
{
{ Abstract:
{
{ This module exports a procedure to return an  error string for a disk
   error
 
 Written by : Brad A. Myers  May 12, 1981
 Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation
{------------------------------------------------------------}

{------------------------------------------------------------
 Versions:
   23 Feb 83  R. Riggs        V1.5  Added IOEEND, IOEFRA, IOEPAR
   11 Feb 82  TV              V1.4  Added EIO Disk Error, IOEPHM. Tony Vezza.
    1 Feb 83  Sandeep Johar   V1.5  Added IOEDNS.
    8 Dec 82  TV              V1.4  Added EIO Disk Error Messages. Tony Vezza.
    7 Oct 82  C. Beckett      V1.3  Added new IO sub-system errors 
   13 May 81  Brad A. Myers   V1.1  Added Ether3MBaud Ethernet message
   12 May 81  Brad A. Myers   V1.0  Created
------------------------------------------------------------}

{//////////////////////////} EXPORTS {\\\\\\\\\\\\\\\\\\\\\\\}

Function IOErrString(err: integer): String;

{//////////////////////////} PRIVATE {\\\\\\\\\\\\\\\\\\\\\\\}
  
Imports IOErrors from IOErrors;

Function IOErrString(err: integer): String;
{------------------------------------------------------------
 Abstract: Returns a string describing a the error number
 Parameters: err is the error number returned by UnitIO
 Returns: A string describing the error
------------------------------------------------------------}
  begin
    case err of
      IOEIOC : IOErrString := 'IO Complete OK';
      IOEIOB : IOErrString := 'IO Busy';
      IOEBUN : IOErrString := 'Bad Unit Number';
      IOENBD : IOErrString := 'Raw Block IO to this device is not implemented';
      IOEWRF : IOErrString := 'Write Failure';
      IOEBSE : IOErrString := 'BlockSize Error';
      IOEILC : IOErrString := 'Illegal Command for this device';
      IOENHP : IOErrString := 'Nil Header Pointer';
      IOEADR : IOErrString := 'Address Error';
      IOEPHC : IOErrString := 'Physical Header CRC Error';
      IOELHC : IOErrString := 'Logical Header CRC Error';
      IOEDAC : IOErrString := 'Data CRC Error';
      IOEDNI : IOErrString := 'Device Not Idle';
      IOEUDE : IOErrString := 'Undefined Error!';
      IOENCD : IOErrString := 'Device is not a character device';
      IOECBF : IOErrString := 'Circular Buffer Full';
      IOELHS : IOErrString := 'Logical Header SerialNum Mismatch';
      IOELHB : IOErrString := 'Logical Header Logical Block Number Mismatch';
      IOECOR : IOErrString := 'Cylinder Out of Range';
      IOEDNR : IOErrString := 'Device not ready';
      IOEMDA : IOErrString := 'Missing data address mark';
      IOEMHA : IOErrString := 'Missing header address mark';
      IOEDNW : IOErrString := 'Floppy write-protected';
      IOECMM : IOErrString := 'Cylinder mis-match';
      IOESNF : IOErrString := 'Sector not found';
      IOEOVR : IOErrString := 'Overrun';
      IOEUEF : IOErrString := 'Undetermined equipment fault';
      IOESOR : IOErrString := 'Sector out of range';
      IOETIM : IOErrString := 'Time out error';
      IOEFRS : IOErrString := 'Floppy recalibrate done';
      IOEDRS : IOErrString := 'Disk recalibrate done';
      IOET0  : IOErrString := 'Can''t find track zero';
      IOECDI : IOErrString := 'Data supplied to configure command is bad '; 
      IOERDI : IOErrString := 'Register data for WriteRegs command is bad '; 
      IOEBAE : IOErrString := 'Buffer alignment error '; 
      IOENOC : IOErrString := 'Not on Cylinder';
      IOEABN : IOErrString := 'Abnormal Condition';
      IOELHE : IOErrString := 'Logical Header Mismatch';
      IOESME : IOErrString := 'State Machine Error';
      IOESKE : IOErrString := 'Drive Seek Error';
      IOEFLT : IOErrString := 'Drive Fault';
      IOEDNS : IOErrString := 'Device not supported';
      IOEPHM : IOErrString := 'Physical Header Mismatch';
      IOEEND : IOErrString := 'End of Data';
      IOEFRA : IOErrString := 'Framing error';
      IOEPAR : IOErrString := 'Parity Error';

{$ifc Ether3MBaud then}
      IOEPTL : IOErrString := 'Ether 3 - Received packet too large';
{$endc}
      otherwise : IOErrString := '*Unknown Error!!*'; 
      end;
end.  
