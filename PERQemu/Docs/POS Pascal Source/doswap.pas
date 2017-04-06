{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module DoSwap;

{--------------------------------------------------------------------
{
{ Abstract:
{    Turns swapping on or off for the shell.
{
{ Copyright (C) Three Rivers Computer Corporation,  1982, 1983
{
{---------------------------------------------------------------------}

{--------------------------------------------------------------------
{ Change log:

{ 16 Nov 82 V1.1 Bill Braucher Fixed names for 14-character compiler.
{ 26 Jan 82 V1.0 Brad Myers    Created by breaking off from Shell.
{--------------------------------------------------------------------



{///////////////////////////////} EXPORTS {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

Imports CmdParse    from CmdParse;

Procedure DoSwap(args: CString);

{///////////////////////////////} PRIVATE {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}


const
     Version = 'V1.1';
                  
Imports Shell       from Shell;
Imports FileDir     from FileDir;    {using DefaultPartitionName (in Swap)}
Imports AllocDisk   from AllocDisk;  {using PartTable, FindPartition}
Imports Memory      from Memory;     {using EnableSwapping, DisableSwapping}
Imports FileSystem  from FileSystem; {using SegIDToFileID}


Procedure DoSwap(args: CString);
{-------------------------------------------------------------------------
{ Abstract: Handles the Swap command
{-------------------------------------------------------------------------}
var
   InFiles, OutFiles: pArgRec;
   Switches: pSwitchRec;
   TArgs: CString;
   ok, enable: Boolean;
   err: String;
   PartNumber: integer;
   
   begin
   TArgs := Args;
   ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
   while (Infiles^.next = NIL) and (inFiles^.name='') and (outFiles^.next=NIL) 
         and (outFiles^.name = '') and (switches = NIL) and ok do
      begin
      Write('Enable swapping? ');
      Readln(TArgs);
      ok := ParseStringArgs(TArgs, InFiles, OutFiles, Switches, err);
      end;
   if ok then
      if (infiles^.next <> NIL) or (outFiles^.next <> NIL) 
          then ok := false;
   if ok then ok := CheckTwo(inFiles^.name, enable, 'YES', 'NO');
   if (not ok) or (switches <> NIL) then
      begin
      if not CheckHelp(switches) then err := '** '
      else err := '     ';
      WriteLn(err, 'Swap takes "Yes" or "No" as input and partition name to swap to as output.');
      WriteLn(err, '  Default for partition name is ',DefaultPartitionName);
      WriteLn(err, '  It turns swapping on or off.');
      end
   else if enable then
        begin
        If outFiles^.name = '' Then outFiles^.name := DefaultPartitionName;
        PartNumber := FindPartition(outFiles^.name);
        If PartNumber = 0 Then
             Writeln('** Unknown partition: ', outFiles^.name, '.')
        Else begin
             EnableSwapping(SegIdToFileId(PartTable[PartNumber].PartInfoBlk));
             WriteLn('Swapping enabled to ',PartTable[PartNumber].PartName);
             end;
        end
   else begin
        DisableSwapping;
        WriteLn('Swapping disabled');
        end;
   DstryArgRec(InFiles);
   DstryArgRec(OutFiles);
   DstrySwitchRec(Switches);
   end.
