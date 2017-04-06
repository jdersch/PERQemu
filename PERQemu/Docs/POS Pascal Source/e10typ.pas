{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{----------------------------------------------------------------------------
{   Copyright 1982, 1983  Three Rivers Computer Corporation
{
{ Abstract:
{
{   This file contains ALL valid EtherNet type fields used by Perq software.
{   Any additional protocols and packet types should be added here.  This
{   file is meant to be used as an INCLUDE file by:
{
{       - Perq Pascal
{       - PrqMic (the Perq microassembler)
{       - VAX Pascal
{
{   It should contain character string names which are unique in 8 characters
{   and end in Type.  If you make changes to this file, be sure it will still
{   go through the compilers and assemblers mentioned above.
{
{
{
{
{---------------------------------------------------------------------------}

{---------------------------------------------------------------------------
{
{ Change Log:
{
{   27 AUg 82   Don Scelza          Added Service Request type.
{
{   22 Jun 82   Mark G. Faust       File created.
{
{
{---------------------------------------------------------------------------}


    FTPByteStreamType   = 0;    
    FTPEtherType        = 1;
    EchoServerType      = 6;
    TimeServerType      = 7;
    CSDXServerType      = 315;
    ServerRequest       = 8;

