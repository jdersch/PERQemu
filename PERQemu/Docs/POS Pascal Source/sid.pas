{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Sid;
{-----------------------------------------------------------------------------
{
{       Sid - Screen Image Dump.
{       J Strait    15 Mar 83.
{       Copyright (C) Three Rivers Computer Corporation, 1983.
{
{ Abstract:
{       Several hardcopy options available for the Three Rivers PERQ
{       computer are capable of printing an image of the display screen.
{       A module to print an image of the screen is included with each
{       of these hardcopy options.  The interfaces to these modules are
{       identical in order that programs may be written without knowing
{       which hardcopy option is available.  This version of Sid is
{       provided for systems with no hardcopy option.  
{       Programs which provide hardcopy ability may import Sid and
{       later be linked with the version of Sid which is specific to
{       a hardcopy device.
{
{       Every incarnation of Sid should implement all routines included
{       here.  This module may be used as a skeleton for other versions.
{
{       The comments in every incarnation should specify the name of
{       the device served and a description of the reasons for raising
{       failure:
{
{       Device name:    Null.
{       Errors raised:  SidNone     is always raised.
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{
{ Change log:
{
{ 04 Apr 83  V1.1  J Strait.
{ Change Sid to make it fit into the System better:
{   Remove SidInit and SidQuit.
{   Change SidDevice to be a function.
{   Remove SidDisposition and change the Disposition parameter into
{       Destination parameter.
{   Handle the new HardCopy exception to prevent recursive Sid.
{
{ 15 Mar 83  V1.0  J Strait.
{ Start module.
{
{-----------------------------------------------------------------------------}

exports


type SidWhy = (SidNone,         { the device is not connected }
               SidBroken,       { the device is physically broken }
               SidHelp,         { the device needs human help, e.g. paper out }
               SidBusy);        { the device is busy: try later }


procedure Sid( Destination: String );
function  SidExplain( Why: SidWhy ): String;
function  SidDevice: String;




exception SidFail( Why: SidWhy );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SidFail is raised when the screen image cannot be printed.
{       Resuming from this exception is allowed: resuming from
{       fatal errors (SidNone, SidBroken) exits from Sid and
{       resuming from non-fatal errors (SidHelp, SidBusy) retries.
{
{ Parameters:
{       Why - The reason that the screen image could not be printed.
{           This value may be converted to a character string with
{           the SidExplain function.
{
{-----------------------------------------------------------------------------}



private


  imports Profile from Profile;
  imports System from System;


const SidNull = True;

procedure Sid( Destination: String );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       The Sid procedure prints an image of the PERQ display screen to
{       a certain hardcopy device.
{
{ Parameters:
{     Destination - A string describing the destination of the hardcopy.
{           For the EtherNet version of Sid this is the string name of
{           the EtherNet Sid server. If non-null, Destination is used
{           as the name of the server machine, and if null, Sid looks
{           in the user profile for an entry of the form
{               #Sid <ServerName>
{           to determine the name of the server machine.  If the
{           parameter is null and there is no entry in the user
{           profile, any available server is used.  Destination
{           could be defined differently for other versions of Sid.
{           For example, it could be the name of a file to which a
{           screen image is written.
{
{ Errors:
{       SidFail is raised if the screen image cannot be printed.  This
{           incarnation of Sid always raises SidFail(SidNone).
{
{-----------------------------------------------------------------------------}

  handler HardCopy;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Handle the HardCopy exception to prevent recursive Sid.
{
{-----------------------------------------------------------------------------}

  begin { HardCopy }
  end { HardCopy };

begin { Sid }
{$ifc not SidNull then}
  if Destination = '' then
    begin
      PFileInit(PFileName, 'Sid');
      Destination := PFileEntry;
      if Destination <> '' then { read to end so that the profile gets closed }
        repeat until PFileEntry = ''
    end;
{$endc}
  raise SidFail(SidNone)
end { Sid };

function  SidExplain( Why: SidWhy ): String;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SidExplain converts a SidWhy value into a character string.
{
{ Parameters:
{       Why - The value to explain.
{
{ Returns:
{       The explanation of Why.
{
{-----------------------------------------------------------------------------}

begin { SidExplain }
{$ifc SidNull then}
  SidExplain := 'No hardcopy device is connected';
{$elsec}
  case Why of
    SidNone:    SidExplain := 'The hardcopy device is not connected';
    SidBroken:  SidExplain := 'The hardcopy device is physically broken';
    SidHelp:    SidExplain := 'The hardcopy device needs human help';
    SidBusy:    SidExplain := 'The hardcopy device is busy';
    end
{$endc}
end { SidExplain };

function  SidDevice: String;
{-----------------------------------------------------------------------------
{
{ Abstract:
{       SidDevice returns the string name of the hardcopy device
{       used by this version of Sid.
{
{ Returns:
{       The name of the hardcopy device.
{
{-----------------------------------------------------------------------------}

begin { SidDevice }
  SidDevice := 'Null'
end { SidDevice }.
