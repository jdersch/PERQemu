{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{     ExpandTabs - Expand tabs in a text file into spaces.
      J. P. Strait        12 Nov 80.
      Copyright (C) Three Rivers Computer Corporation, 1980, 1981, 1982, 1983.
      
      
      Modification history.
      
      V1.0  JPS  12 Nov 80  Start program.
}

program ExpandTabs;

 imports System from System;
 imports CmdParse from CmdParse;

const ExpTabVersion = '1.0';
      Tab = Chr(#011);

var InFile, OutFile: Text;
    InFileName, OutFileName, Ignore: String;
    Column: Integer;
begin { ExpandTabs }
 Reset(Input, 'Console:');
 Rewrite(Output, 'Console:');
 Writeln('ExpandTabs ', ExpTabVersion);
 RemDelimiters(UsrCmdLine, ' ', Ignore);
 GetSymbol(UsrCmdLine, Ignore, ' ', Ignore);
 RemDelimiters(UsrCmdLine, ' ', Ignore);
 while UsrCmdLine = '' do
  begin
   if UsrCmdLine = '' then
    begin Write('input file name: ');
     Readln(UsrCmdLine)
    end;
   RemDelimiters(UsrCmdLine, ' ', Ignore)
  end;
 GetSymbol(UsrCmdLine, InFileName, ' ', Ignore);
 RemDelimiters(UsrCmdLine, ' ', Ignore);
 while UsrCmdLine = '' do
  begin
   if UsrCmdLine = '' then
    begin Write('output file name: ');
     Readln(UsrCmdLine)
    end;
   RemDelimiters(UsrCmdLine, ' ', Ignore)
  end;
 GetSymbol(UsrCmdLine, OutFileName, ' ', Ignore);
 RemDelimiters(UsrCmdLine, ' ', Ignore);
 if UsrCmdLine <> '' then
  begin Write(' unknown parameter(s): ', UsrCmdLine, ', <return> to continue');
   Readln
  end;
 Reset(InFile, InFileName);
 Rewrite(OutFile, OutFileName);
 Column := 0;
 while not Eof(InFile) do
  begin
   if InFile^ = Tab then
    repeat Write(OutFile, ' ');
     Column := Column + 1
    until Column mod 8 = 0
   else
    if Eoln(InFile) then
     begin Writeln(OutFile);
      Column := 0
     end
    else
     begin Write(OutFile, InFile^);
      Column := Column + 1
     end;
   Get(InFile)
  end;
 Close(InFile);
 Close(OutFile)
end { ExpandTabs }.
