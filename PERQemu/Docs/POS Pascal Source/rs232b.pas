{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module RS232Baud;
{----------------------------------------------------------------------
{
{ Abstract:
{
{     RS232Baud - set RS232 baud rate with optional input enable.
                  J. P. Strait       21 Aug 80.
                  Copyright (c) Three Rivers Computer Corporation 1980,
                  1981, 1982, 1983.
{---------------------------------------------------------------------}


{$Version V1.2 for POS}
{----------------------------------------------------------------------
 Change Log:
  17 Mar 83  V1.2  WJHansen
    Add entrypoint SetRS232Port.
    Teach it about 19200 baud.
    
     13-May-81 Brad A. Myers  V1.1  Changed to use exceptions and new IO
     21-Aug-80 John Strait    V1.0  Created
{---------------------------------------------------------------------}

exports

procedure SetBaud(Baud: String; Enable: Boolean);

Exception BadBaudRate;
{----------------------------------------------------------------------
 Abstract: Raised if Baud is not a valid baud rate.
{---------------------------------------------------------------------}

procedure SetRS232Port(Baud: string; Device: Integer);

Exception BadRSDevice;
{----------------------------------------------------------------------
 Abstract: Raised if Device is not an RS232 port.
{---------------------------------------------------------------------}



private

imports IO_Unit from IO_Unit;
imports Configuration from Configuration;
 

procedure SetBaud(Baud: String; Enable: Boolean);
{----------------------------------------------------------------------
 Abstract: 
     Sets the baud rate to baud specified by string arg
 Arguments: 
     Baud - string of new baud rate (e.g. "2400")
     Enable - is ignored
 SideEffects: 
     Changes status of RS232
 Errors: 
     Raises BadBaudRate if string is illegal
{---------------------------------------------------------------------}
begin { SetBaud }
    SetRS232Port(Baud, RSA);
end; { SetBaud }
 

procedure SetRS232Port(Baud: string; Device: Integer);
{----------------------------------------------------------------------
 Abstract: 
     Sets the baud rate to baud specified by string arg
 Arguments: 
     Baud - string of new baud rate (e.g. "2400")
     Device - chooses RSA or RSB
 SideEffects: 
     Changes status of RS232
 Errors: 
     Raises BadBaudRate if string is illegal
{---------------------------------------------------------------------}
var Speed: integer;
    Status: DevStatusBlock;
begin { SetBaud }
  if (Baud = '19200') and (Cf_RS232MaxSpeed=RS19200) then 
               Speed := RS19200
  else if Baud = '9600'  then Speed := RS9600
  else if Baud = '4800'  then Speed := RS4800
  else if Baud = '2400'  then Speed := RS2400
  else if Baud = '1200'  then Speed := RS1200
  else if Baud = '600'   then Speed := RS600
  else if Baud = '300'   then Speed := RS300
  else if Baud = '150'   then Speed := RS150
  else if Baud = '110'   then Speed := RS110
  else 
      Raise BadBaudRate;
  
  if (Device<>RSA) and ((Device<>RSB) or (Cf_RS232Ports=1)) then
      Raise BadRSDevice;

  with Status do begin 
    ByteCnt := 3;
    RSRcvEnable := True;
    RSFill := 0;
    RSSpeed := Speed;
    RSParity := NoParity;
    RSStopBits := Stop1;
    RSXmitBits := Send8;
    RSRcvBits := Rcv8;
  end;
  IOPutStatus(Device,Status);
end { SetBaud }.
