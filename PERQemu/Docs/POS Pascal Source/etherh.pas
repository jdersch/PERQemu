{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module EtherHelp;

{----------------------------------------------------------------------
{
{ Abstract:
{   This module is used to provide help information for the ethernet
{   test program.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983
{
{-------------------------------------------------------------------------}


{------------------------------------------------------------------------
{
{ Change History
{
{ 12 Apr 82 V1.3    Don Scelza
{   Added help for the group stuff.
{
{  1 dec 81 V1.2    Don Scelza
{   Added help for the Intel diagnostic.
{
{ 26 Oct 81 V1.1    Don Scelza
{   Added the help text for the Clock DCB
{
{ 14 Oct 81 V1.0    Don Scelza
{   Created the module.
{
{-------------------------------------------------------------------------}



{********************} Exports {********************}

procedure CmdHelp;
procedure VarHelp;
procedure DisplayHelp;

{********************} Private {********************} 

imports System from System;
imports IO_Others from IO_Others;


procedure CmdHelp;
{----------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to print help text about the first level
{   commands.
{
{----------------------------------------------------------------------}
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(CmdHelp);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(CmdHelp);
    end;

    begin
    writeln('This program provides a test facility for the ',
            'PERQ Ethernet.');
    writeln('In general a press outside of a menu or typing a ^C will');
    writeln('exit the current command processor,  returning you to the');
    writeln('previous command level.');
    writeln;
    writeln('The commands are:');
    writeln('   Help          Print this message.');
    writeln;
    writeln('   Send          Send the send buffer in a packet.');
    writeln('                 "His Address" is used as the destination.');
    writeln('                 "Byte Count" bytes will be sent.');
    writeln;
    writeln('   Multicast     Send a multicast packet to the group specified');
    writeln('                 "Send Group".');
    writeln;    
    writeln('   Receive       Look for packets on the net that have');
    writeln('                 "My Address" as the destination field.');
    writeln('                 The data portion of the packet will be placed');
    writeln('                 into the receive buffer.');
    writeln;
    writeln('   Promiscuous   Look for any packets on the net');
    writeln('                 The data portion of the packet will be placed');
    writeln('                 into the receive buffer.');
    writeln;
    writeln('   Echo          Echo any packets addressed to "My Address".');
    writeln;
    writeln('   Random        Send packets of random data to an echo server.');
    writeln('                 The packets will be sent to "His Address".');
    writeln('                 After the packet has been sent, look for its echo.');
    writeln('                 When the echo is received check for correct data.');
    writeln;
    writeln('   Intel         This mode is for use with the Intel Development');
    writeln('                 System.  It allows the user to send packets of');
    writeln('                 text from the Intel box to a Perq and back.');
    writeln;
    writeln('   Load          Fill the Send or Receive buffer.');
    writeln;
    writeln('   Display       Look at any of the internal buffers.');
    writeln;
    writeln('   Reset         Execute a reset command.');
    writeln;
    writeln('   Status        Give network status information.');
    writeln;
    writeln('   SetVars       Set some of the internal variables.');
    writeln;
    writeln('   Quit          Exit this program.');
    writeln;
    writeln('Type ^C to return to command mode.');
    while true do ;
    end;


procedure VarHelp;
{---------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to display help about the user settable 
{   variables.
{
{----------------------------------------------------------------------}
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(VarHelp);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(VarHelp);
    end;

    begin
    writeln('This command is used to set some of TestEthers''s',
            ' internal variables.');
    writeln('To exit this mode press outside of the menu or type ^C.');
    writeln;
    writeln('The variables that you can set are:');
    writeln;
    writeln('    My Adddress =    The Ethernet address of this machine.');
    writeln;
    writeln('    His Address =    The Ethernet address of the other machine.');
    writeln;
    writeln('    Byte Count  =    The number of DATA bytes to be transfered.');
    writeln;
    writeln('    Packet Type =    The value of the Type field used on sends.');
    writeln;
    writeln('    Echo Delay  =    Loop bound for Echo delay.');
    writeln('                     The code for "I := 1 to Echo_Delay do ; "');
    writeln('                     will be executed before a received packet');
    writeln('                     is echoed.');
    writeln;
    writeln('    Timeout     =    Number of seconds to wait before an opeation');
    writeln('                     is timed out.');
    writeln;
    writeln('    Group Cmd   =    The byte that is pushed at the Group command');
    writeln('                     register.  This determins how group ');
    writeln('                     adddressing works.');
    writeln;
    writeln('    Group Addr  =    This is an array of 5 bytes that determins');
    writeln('                     which groups this machine belongs to.');
    writeln;
    writeln('    Send Group  =    This variable determins what group a');
    writeln('                     multicast packet goes to.');
    writeln;
    writeln('Type ^C to return to command mode.');
    while true do ;
    end;     


procedure DisplayHelp;
{--------------------------------------------------------------------------
{
{ Abstract:
{   This procedure is used to give help information about the buffers 
{   that can be loaded or displayed.
{
{--------------------------------------------------------------------------}
  handler CtlC;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(DisplayHelp);
    end;
  handler CtlCAbort;
    begin
    CtrlCPending := false;
    IOKeyClear;
    writeln;
    exit(DisplayHelp);
    end;

    begin
    writeln('This command is used to load and display TestEthers''s',
            ' internal buffers.');
    writeln('To exit this mode press outside of the menu or type ^C.');
    writeln;
    writeln('The buffers that you can load are:');
    writeln('    Send Buffer    = The buffer to be sent on the network.');
    writeln('    Receive Buffer = The buffer that will be used to',
            ' receive packets.'); 
    writeln;
    writeln('The buffers that you can display are:');
    writeln('    Send Buffer    = The buffer to be sent on the network.');
    writeln('    Receive Buffer = The buffer received from the network.');
    writeln('    Send Status    = The Status block for sends.');
    writeln('    Receive Status = The Status block for receives.');
    writeln('    Send Header    = The Header block for sends.');
    writeln('    Receive Header = The Header block for receives.');
    writeln('    Send DCB       = The DCB used for send commands.');
    writeln('    Receive DCB    = The DCB used for receive commands.');
    writeln('    Clock DCB      = The DCB used for the microsecond clock.');
    writeln;
    writeln('Type ^C to return to command mode.'); 
    while true do ;
    end.   
