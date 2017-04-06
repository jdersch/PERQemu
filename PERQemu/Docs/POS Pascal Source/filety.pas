{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FileTypes;
{-----------------------------------------------------------------------------
 This module exports the Types put in the FileType field of File FIBs.  The
 types are stored as integers.  Three Rivers reserves the first 512 types for
 their use.  Customers are encouraged to choose numbers > 512 if they invent
 new file types
 
 Written by Brad A. Myers  Feb. 2, 1981
 
 Copyright (C) 1980, 1981, 1982, 1983   Three Rivers Computer Corporation
-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
   Versions:
      V1.6 16-Nov-82  Bill Braucher   Fixed names for 14-character compiler.
      V1.5 24-Aug-82  Mike Kristofic  Added TempFile (per Ed Frankenberry)
      V1.4 23-Aug-82  Mike Kristofic  Added ExtFile and LibFile
      V1.3  2-Mar-82  Mike Kristofic  Added ForFile, DatFile and PsgFile
      V1.2  1-May-81  Brad Myers      Added SwapFile and BadFile
                                      Changed name of BootFile to SBootFile to
                                        remove name conflict
      V1.1 31-Mar-81  Brad Myers      Added BootFile and MBootFile
      V1.0  2-Feb-81  Brad Myers      Started
-----------------------------------------------------------------------------}

{\\\\\\\\\\\\\\\\\\\\\\\\\\} EXPORTS {/////////////////////////}

Const
      UnknownFile = 0;
      SegFile = 1;
      PasFile = 2;
      DirFile = 3;
      ExDirFile = 4;
      FontFile = 5;
      RunFile = 6;
      TextFile = 7;     {for non-Pas text files}
      CursorFile = 8;   {cursor bin files}
      BinaryFile = 9;
      BinFile = 10;     {microcode output}
      MicroFile = 11;
      ComFile = 12;
      RelFile = 13;
      IncludeFile = 14; {included in a pas file}
      SBootFile = 15; {system part of boot file}
      MBootFile = 16; {microcode part}
      SwapFile = 17; {a file used for swapping by compiler or editor; length
                        not set}
      BadFile = 18;  {created by the scavenger}
      ForFile = 19;  { Fortran source file }
      DatFile = 20;  { Fortran unformatted data file }
      PsgFile = 21;  { Fortran pre-seg file }
      ExtFile = 22;  { Fortran external definition file }
      LibFile = 23;  { Fortran library file }
      TempFile = 24;  { Created by Temper }
      

{\\\\\\\\\\\\\\\\\\\\\\\\\\} PRIVATE {/////////////////////////}
Procedure CompilerBug;
begin end.
