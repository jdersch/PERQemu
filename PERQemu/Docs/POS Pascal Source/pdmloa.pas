{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{$R-}
Module PDMLoad;


{
{ copyright 1981, 1982, 1983  Three Rivers Computer Corporation
{
{--------------------------------------------------------------------------
{ Modification History
{
{ 3-Apr-81 V0.1 Diana Forgy
{          Converted to run under new system - changed references to
{          Perq_String, IOErrors, and Loader.
{
{--------------------------------------------------------------------------}

Exports Procedure LoadRunFile(RunFileName: string);

Private

   imports System from System;
   imports Memory from Memory;
   imports IO from IO;
   imports IOErrors from IOErrors;
   imports FileSystem from FileSystem;
   imports Perq_String from Perq_String;
   imports PDMUtils from PDMUtils;
   imports Loader from Loader;
   
Procedure LoadRunFile( RunFileName: string );

{-----------------------------------------
{
{ Abstract:
{    Load a Run File into Perq and perform initial setup
{
{ Parameters:
{    RunFileName: Name of the file to be loaded.  The extension .RUN is
{    optional.
{-----------------------------------------}

      Procedure Load( RunFileName: string );     
      { hack because System declares Load }
      begin
          DefaultExtension(RunFileName,'.RUN');
          if Existant(RunFileName) then Load(RunFileName)
               else Writeln('?Cannot find ',RunFileName);
      end; {Load}
      
begin
     Load(RunFileName);
end.
           
