{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module ShellDefs;

{--------------------------------------------------------------------
{
{ Abstract:
{    Definition of the Shell control file used by Shell, InitShell and Login.
{
{ Copyright (C) Three Rivers Computer Corporation,  1981, 1982, 1983
{
{---------------------------------------------------------------------}



{----------------------------------------------------------------------
  Change log:
     21 Jan 82  V1.0  Brad Myers  Separated from Shell.Pas
---------------------------------------------------------------------}


{\\\\\\\\\\\\\\\\\\\\\\\\\\} EXPORTS {////////////////////////////}

Imports PopUp    from PopUp;
Imports CmdParse from CmdParse;
Imports GetTimeStamp from GetTimeStamp;

Const          
      CmdNames = {used in InitShell to find Indices for these commands}
               'OTHER     ?         HELP      PATH      RUN       RERUN     PAUSE     MOUNT     DISMOUNT  STATISTICSSWAP      SCREENSIZE';
      
      IdxOther = 0;      {All commands not on above list are implemented
                          solely by entries in the profile.}
        {The following entries must give indices in the same order as the
        names in ShlCmdNames.}
      IdxQuest = 1;      {'?'}
      IdxHelp = 2;
      IdxPath = 3;
      IdxRun = 4;
      IdxReRun = 5;
      IdxPause = 6;
      IdxMount = 7;
      IdxDismount = 8;
      IdxStatistics = 9;
      IdxSwap = 10;
      IdxScreenSize = 11;
      
      LastIdx = 11;

Type
     CmdRecord = packed record
         RunString: String;          {what string to put in UsrCmdLine and
                                      execute}
         AddDefault: Boolean;        {T if should add default file when none
                                      explicitly given}
         SetDefault: Boolean;        {T if should set default file from
                                      explicit filename}
         ScreenSize: 1..8;           {What screensize to use.  Due to compiler,
                                      this takes 4 bits}
         Index: 0..1023;             {Index into case statement below}
         end;
     
     CmdsArray = array [1..1] of CmdRecord;
     pCmdsArray  = ^CmdsArray;

     CtrlRec   = RECORD
                 NumCmds: Integer;     {Number of commands found in profile}
                 CmdDesc: pNameDesc;   {for PopCmdParse}
                 CmdTable: pCmdsArray; {info for Shell}
                 CmdFileList: pCmdList;{for GetCmdLine}
                 FirstPress: Boolean;  {for GetCmdLine}
                 END;
     pCtrlRec  = ^CtrlRec;

Const
    TimeFileName = '>Hold_Time.TimeStamp';
    TimeFBitSize = WordSize(TimeStamp)*16;


{\\\\\\\\\\\\\\\\\\\\\\\\\\} PRIVATE {////////////////////////////}

Const Version = 'V1.0';

Procedure CompilerBug;
  begin
  end.

