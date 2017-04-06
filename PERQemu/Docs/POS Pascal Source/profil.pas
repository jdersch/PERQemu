{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Profile;

{----------------------------------------------------------------------
{
{ Abstract:
{    This module is used to get information from the user profile
{    file.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corperation,  1981, 1982, 1983
{
{-----------------------------------------------------------------------}


{ 28 Dec-81  V1.1  Brad Myers
{ Fixed so PFileEntry only returns a blank line when no more inputs.
{ Changed length of strings returned to be 255!!
{ }

{ 30-Apr-81  V1.0  Don Scelza
{ Created the module.
{ }


{********************} Exports {********************}

{---------------------------------------------------------------------
{
{    This module provides facilities that will allow a program to get
{ information from the user profile.
{
{    The profile file is a text file that has the form:
{
{        #<Subsystem name> <Line of text for that sub system>
{        <More text for that subsystem>
{        -
{        -
{        -
{        #<Next subsystem>---
{
{ The base unit of the file is a text line.  The function that provides
{ values from the profile file will return a line of text each time that it
{ is called.  All text line between the #<Subsystem name> and the next
{ #<Subsystem name> are assumed to be assoicated with the first subsystem.
{ Successive calles to PFileEntry will return the next line of text for the
{ current subsystem.
{
{----------------------------------------------------------------------}

Exception PNotFound(FileName: String);
{------------------------------------------------------------------------
  Abstract: Raised when profile file cannot be found
  Parameters: fileName is profile not found
------------------------------------------------------------------------}

Exception PNotInited;
{------------------------------------------------------------------------
  Abstract: Raised when a profile procedure is used but PFileInit not called
              first
------------------------------------------------------------------------}

Type ProfStr = String[255];

procedure PFileInit(PFileName, SubSystem: ProfStr);

function PFileEntry: ProfStr;

{*******************} Private {*********************}


const
    FirstChar = '#';
    InitVal = #52525;
    Debug = false;
    
var
    PFile: Text;
    InLine: ProfStr;
    Inited: Integer;

imports FileSystem from FileSystem;
imports CmdParse from CmdParse;
imports Perq_String from Perq_String;


procedure PFileInit(PFileName, SubSystem: ProfStr);
{------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is called each time a subsystem wishes to start to
{    read information from the profile file.  It is only called once
{    per subsystem invocation.  It will lookup the profile file and
{    search for the required subsystem.
{
{ Parameters:
{    PFileName is the name of the profile file that is to be used.
{
{    SubSystem is the name of the subsystem that is to be searched for.
{
{ Side Effects:
{    This procedure will change Inited, InLine and PFile.
{
{ Errors:
{    If PFileName was not found then raise PNotFound.
{
{-----------------------------------------------------------------------}
var FID: FileID;
    Dum: Integer;
    SDum, Broke: String;
    
    begin
    FID := FSLookUp(PFileName, Dum, Dum);
    if FID = 0 then raise PNotFound(PFileName);
    
    reset(PFile, PFileName);
    Inited := InitVal;
    
    ConvUpper(SubSystem);
    SubSystem := ConCat(FirstChar, SubSystem);
    if Debug then writeln('Init> Looking for ', SubSystem);
    
    while (not eof(PFile)) do
        begin
        readln(PFile, InLine);
        if debug then writeln('Init> Line = ', InLine);
        RemDelimiters(InLine, ' ', Broke);
        if Broke = FirstChar then 
            begin
            if debug then writeln('Init> command found');
            GetSymbol(InLine, SDum, ' ', Broke);
            CnvUpper(SDum);
            if SDum = SubSystem then 
                begin
                RemDelimiters(InLine, ' ', Broke);
                if debug then writeln('Init> Command line = ', InLine);
                exit(PFileInit);
                end;
            end;
        end;
    
    InLine := '';
    end {PFileInit};
    

function PFileEntry: ProfStr;
{---------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to get the next profile entry for a
{    subsystem.
{
{ Results:
{    This procedure will return the next line from the profile
{    file for the current subsystem.  If there are no more lines
{    for the current subsystem return null.
{
{ Environment:
{    PFileInit must have been called before this procedure is used.
{    Uses the global InLine.  Sets InLine to be empty.
{
{ Errors:
{    If PFileInit was not called then raise PNotInited.
{
{---------------------------------------------------------------------}
    var Broke: String[1];
    begin
    if Inited <> InitVal then raise PNotInited;
    if InLine = '' then
        repeat
          if EOF(PFile) then Inited := 0 {InLine is '' already}
          else readln(PFile, InLine);
          RemDelimiters(InLine, ' ', Broke);
          if broke = '#' then begin
                              Inited := 0;
                              InLine := '';
                              end;
        until (Inited = 0) or (InLine <> '');
    PFileEntry := InLine;
    InLine := '';
    end.
    
