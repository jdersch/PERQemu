{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FTPVersion;

{------------------------------------------------------------------------------
{
{  Copyright (C) 1982 Three Rivers Computer Corporation
{
{  Written by:  Dirk Kalp
{
{  Abstract: Provides the current version number for FTP and serves as a 
{            reference for Change Log entries in the other FTP modules.
{
{------------------------------------------------------------------------------}
{$Version 7.9 for POS}
{------------------------------------------------------------------------------
{ Change Log:
{
{ 15 Mar 83  V7.9  WJHansen
{  Ftp:
{    Don't call DisplayStatus in mainloop.
{    Call RestoreSwitches in mainloop.
{    Set OnCmdLine only if it is a valid command or FTP/HELP.
{    Call FTPError for parsing errors.
{    Fix so can read switches on command line. 
{  FTPUser:
{    Call RestoreSwitches in FTP after each command.
{    Make NeedNewStatusDisplay local to DoSwitches;
{    do DisplayStatus anywhere else that changes switches.
{    Catch BadBaudRate in StrSetDev and RestoreSwitches.
{    Remove global NonUnique and NonExistent.
{    Set HoldConfirm in SaveSwitches.
{    If there is a parsing error, ask user if he wants to continue.
{    Add NeedDisplayStatus to RestoreSwitches.
{    Put control-G's in error messages.
{    Add warning about local and remote prefixes on Poll command.
{    Implement FTPError.
{    Reorder routines.
{    Check for extra text on line in FirstSwitch.
{    In DoSwitches, do not set baud rate unless device is RS232.
{ 7 Mar 83  V7.8  WJHansen
{  FTP:
{    Import IO_Unit and IO_Others.
{  FTPUser:  
{    Remove check for zero block file in Put.  (Allows putting directory.)
{    Do not reset remote machine name for Device command.
{  FTPUtils:
{    Fix FTPDoEPacket to supply a Stat pointer in CurrRecv & set EType.
{    Implement sending directories in WriteFile.
{    Do not restart timer when receive alien packet.
{
{ 28 Feb 83   V7.7  WJHansen
{  module FTP:
{    Remove MPOS stuff.
{    Remove E10ReceiveDone handler.  (Use no-interrupt version of ethernet.)
{    Implement RemotePrefix and LocalPrefix switches.
{    Read switches from profile.
{    Install call on ShowMemUsage.
{    Synchronize version number with FTPVersion & do dollar-Version.
{    Don't exit if initial command line starts with switches.
{    Move some variables from global to FTPUser to local here.
{    Make CommandName a CString.
{    Move all switch initialization to SetInitialState.
{  Module FTPUser:
{    Remove MPOS.
{    Implement switches for RemotePrefix and LocalPrefix.
{    Implement RSA and RSB.
{    Install SaveSwitches for default switch setting.
{    Remove E10ReceiveDone handler.
{    Use version number from FTPVersion and put in valid $version line.
{    Remove definition of x-ConfirmNonWildDelete and x-AskWild.
{    Remove some unneeded global private variables.
{    Move all switch init from Initialize to SetInitialState.
{    Correct and improve various messages.
{    Do IOPutStatus before setting baudrate.
{    Poll mode show 'Done'.
{    If file has 0 blocks after FTPGetFile, Delete the file.
{    Use [None] as default prefix.
{  Module FTPUtils:
{    Implement ShowMemUsage and call from File operations.
{    Change RS232 to FTP_RSA and FTP_RSB.
{    Remove MPOS.
{    for IOPutStatus, change ByteCount from 1 to 3.
{    Implement FTPDoEPacket.
{    Removed IOPutStatus(RS232) from FTPInit.
{    Don't rewrite(x-file) if doing ethertransfer. Fixes FullSegm error (1036).
{    Don't create file if it doesn't exist on sender. (1134)
{    Provide FTPSetEtherAddr.
{    Create PostAllReceives; call in NetAllocate, FTPErr, FTPSetEtherAddr.
{
{   11 Feb 83   V7.6  Brad Myers
{       Adapt for Landscape.
{
{   07 Jan 83   Version 7.5   Sandeep Johar
{       Implement aliases and fix a bug which caused ftp to die if the
{       connect failed and a put was executed.
{
{   03 Dec 82   Version 7.4   Sandeep Johar
{       Fixed command line processing with gets and puts to handle the
{       connect switch.
{
{   15 Nov 82   version 7.3   Don Scelza
{       Set default device to be FastEther.
{ 
{   15 Nov 82   Version 7.2   Don Scelza
{       Fixed a bug in FTPUtils that caused request for a file
{       of length 0 to hang the machine.
{
{   28 Oct 82   Version 7.1   Dirk Kalp
{       1. Use new module FTPVersion. (See FTP and FTPUser)
{       2. Put status at top of screen and make DisplayStatus calls more
{          precise. (See FTP and FTPUser)
{       3. Added procedure FixupWindows to FTPUser and use it in FTP instead
{          of ScreenReset on program exit. (Fixes the FTP/HELP problem also.)
{       4. Make POLL cmd work with switches. (See FTPUSER)
{       5. Fix up deallocation of memory for cmd line args. (See FTP and 
{          FTPUser)
{       6. Add BadBaudRate Handler in FTPUser for the BAUD switch.
{       7. Corrected some minor bugs found in FTPUser.
{                                                             
{   27 Oct 82   Version 7.0   Dirk Kalp
{       Module written. Begin with Version number at 7.0.
{ }
{
{-----------------------------------------------------------------------------}


{***********************}  Exports  {***********************}


Function FtpVersion: string;


    
{***********************}  Private  {***********************}

Const
   Version = '7.9';


Function FtpVersion: string;
{------------------------------------------------------------------------------
{
{ Abstract:
{    Return the version number.
{
{-----------------------------------------------------------------------------}
begin
   FtpVersion := Version;
end.
