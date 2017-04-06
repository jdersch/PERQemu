{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Append;
{-----------------------------------------------------------------------------
{
{       Append - Append one file to the end of another.
{       B. A. Myers and J. P. Strait   2 Mar 81.
{       Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       Append copies one file to the end of the other.  Program call:
{
{            Append Destination Source
{
{       Where Source is appended to the end of Destination.
{
{-----------------------------------------------------------------------------

{  15 Feb 83 V3.3  Jerry Conner
{  Fixed zero length file problem.
{ }

{  16 Nov 82 V3.2  Bill Braucher
{  Fixed names for 14-character compiler.
{ }

{  29 Dec 81 V3.1  Brad Myers
{ Indent help messages. 
{ Use standard error message print out.
{ Remove Quotes.
{ }

{  14 Dec 81 V3.0  Brad Myers
{ New cmdParse interface  
{ }

{  3 Jun 81 V2.1  Brad Myers
{ Catch exceptions  
{ }

{  3 Jun 81 V2.1  Brad Myers
{ Catch exceptions  
{ }

{ 28 Mar 81 V2.0  Brad Myers
{ Uses FSAddToTitleLine  
{ }

{ 19 Mar 81 V1.3  Brad Myers
{ Fixed for new FS. (PERQ_String)  
{ }

{ 12 Mar 81 V1.2  Don Scelza
{ Fixed copy into a null file.  
{ }

{ 12 Mar 81 V1.1  Don Scelza
{ Added code for progress.  
{ }

{ 2 Mar 81  V1.0  John Strait.
{ Start file.
{ }

imports System from System;
imports CmdParse from CmdParse;
imports Perq_String from Perq_String;
imports FileUtils from FileUtils;
imports UtilProgress from UtilProgress;
imports FileTypes from FileTypes;


 
Const Title = 'Append Version V3.3.  Type /Help for help.';

Type FakeB = record case boolean of
                      true  : (w: DirBlk);
                      false : (byteBuffer: packed array[0..511] of FSBit8);
                  end;
     FakeBlock = ^FakeB;

Var DstName: String;
    SrcID, DstID: FileID;
    SrcBlocks, DstBlocks, SrcBits, DstBits, maxLength: Integer;
    DstBuf, SrcBuf: FakeBlock;
    ptr: ptrFSDataEntry;
    switchAr: CmdArray;
    
  

Procedure Oops(FileName: PathName; S: String );
{-----------------------------------------------------------------------------
   Abstract: Report error and then exit.  Error is ** filename s
-----------------------------------------------------------------------------}
   begin { Oops }
   Writeln('** ',fileName, S);
   exit(Append)
   end { Oops };
  

Procedure DoHelp;
{-----------------------------------------------------------------------------
   Abstract: Print help and then exit.
-----------------------------------------------------------------------------}
  begin
  WriteLn;
  WriteLn('     Append is used to attach files together.   It takes a series of');
  WriteLn('     input files and puts them all at the end of the first input file.');
  WriteLn('     There is no output file for append.  The syntax for');
  WriteLn('     the command is:    Append f1, f2, f3, ...');
  WriteLn('     This will add f2 to the end of f1 and then add f3 to the end of');
  WriteLn('     the result, etc.  The only switch is HELP.');
  exit(Append);
  end;

Handler FSNotFnd(FileName: PathName);
  begin
  StdError(ErFileNotFound, FileName, true);
  end;


Procedure HandleLine(ins, outs: pArgRec; switches: pSwitchRec; err: string;
                    ok: boolean);
{-----------------------------------------------------------------------------
   Abstract: Handle the parsed input line.  Checks for errors and handles
             switches.
-----------------------------------------------------------------------------}
  var MyFID: FileID;
      i: integer;
  begin
  if not ok then StdError(ErAnyError,err, true);
  while switches <> NIL do
     begin
     ConvUpper(switches^.switch);
     i := UniqueCmdIndex(switches^.switch, switchAr, 1);
     case i of
          1 : DoHelp;
          otherwise: StdError(ErBadSwitch, switches^.switch, true);
          end;
     switches := switches^.next;
     end;
  if (outs^.name <> '') or (outs^.next <> NIL) then
      StdError(ErAnyError,'** There is no output file for Append.  Separate files with commas.', true);
  end;


Procedure DoAppend(var finalBlks, finalBits: integer);
{-----------------------------------------------------------------------------
   Abstract: Does the actual append of two files.
   NOTE: Dst is output file, one specified first.  Src is other.
   Parameters: finalBlks and finalBits are set at the end with the
               number of blks and bits in the final file.
   Environment: Uses the globals: SrcBlocks, SrcBits, SrcID, SrcBuf,
                                  DstBlocks, DstBits, DstID, DstBuf.
-----------------------------------------------------------------------------}
var numWords, shiftAmt, numBytes, DstCount, JumpLines, i, j: Integer;
begin
if DstBlocks = 0 then {easy case, destination empty}
   begin
   if SrcBlocks <> 0 then 
      begin
      JumpLines := 1024 div SrcBlocks;
      LoadCurs;
      for i := 0 to SrcBlocks-1 do
         begin
         ShowProgress(JumpLines);
         FSBlkRead(SrcID, i, Recast(SrcBuf,pDirBlk));
         FSBlkWrite(DstID, i, Recast(SrcBuf,pDirBlk));
         end;
      QuitProgress;
      end;
   finalBlks := srcBlocks;
   finalBits := srcBits;
   exit(DoAppend);
   end;

if DstBits <> 4096 then {have to combine blocks}
   begin
   FSBlkRead(DstID, DstBlocks-1, Recast(DstBuf,pDirBlk));
            {last destination block}
   numBytes := DstBits div 8;
   DstCount := DstBlocks-1;  {make sure write out this block}
   end
else begin
     DstCount := DstBlocks;
     numBytes := 0;  {start with the block after the last of destination}
     end;

if SrcBlocks <> 0 then JumpLines := 1024 div SrcBlocks
else JumpLines := 1024;
LoadCurs;
for i := 0 to SrcBlocks-1 do {now do the transfer}
   begin
   ShowProgress(JumpLines);
   FSBlkRead(SrcID, i, Recast(SrcBuf,pDirBlk));
   for j := numbytes to 511 {last byte of block} do
      DstBuf^.bytebuffer[j] := SrcBuf^.bytebuffer[j-numbytes];
   FSBlkWrite(DstID, i+DstCount, Recast(DstBuf,pDirBlk));
   for j := 0 to numBytes-1 do
      DstBuf^.byteBuffer[j] := SrcBuf^.byteBuffer[j+512-numBytes];
   end;

i := SrcBits+DstBits;
if i > 4096 then
   begin
   {have to dump out last block}

      FSBlkWrite(DstID, SrcBlocks+DstBlocks-1, Recast(DstBuf,pDirBlk));
   i := i-4096;
   j := SrcBlocks+DstBlocks;
   end
else j := SrcBlocks+DstBlocks-1;

QuitProgress;
finalBlks := j;
finalBits := i;
end; {DoAppend}


var ins, outs, temp: pArgRec;
    switches: pSwitchRec;
    err: String;
    ok: boolean;
    c: Char;
    s: CString;
    isSwitch: boolean;
      
begin
New(DstBuf);
New(SrcBuf);
New(ptr);

  FSAddToTitleLine(title);
  switchAr[1] := 'HELP';
  err := '';
  c := NextId(s, isSwitch);  {remove "append"}
  if (c<>' ') and (c<>CCR) then StdError(ErIllCharAfter, 'Append', true);
  ok := ParseCmdArgs(ins, outs, switches, err);
  repeat
     HandleLine(ins, outs, switches, err, ok);
     DstName := ins^.name;
     ins := ins^.next;
     if DstName = '' then 
        begin
        Write('File to be appended to: ');
        ReadLn(s);
        ok := ParseStringArgs(s, ins, outs, switches, err);
        end;
  until DstName <> '';
  
  if not RemoveQuotes(DstName) then StdError(ErBadQuote, '', true);
  if ins = NIL then 
    repeat
        begin
        Write('File(s) to append to ', DstName, ': ');
        ReadLn(s);
        ok := ParseStringArgs(s, ins, outs, switches, err);
        HandleLine(ins, outs, switches, err, ok);
        end;
    until ins^.name <> '';
  
  DstID := FSSearch(FSSysSearchList, DstName,DstBlocks,DstBits);
  FSRemoveDots(DstName);
  FSGetFSData(dstId, ptr);
  if ptr^.FileType = DirFile then Oops(dstName, ' is a directory.  Cannot append.');
  maxLength := length(DstName);
  if DstBits mod 8 <> 0 then
        Oops(DstName, ' does not end on a byte boundary.');
  temp := ins;
  repeat
     if not RemoveQuotes(DstName) then StdError(ErBadQuote, '', true);
     SrcID := FSSearch(FSSysSearchList, temp^.name, SrcBlocks, SrcBits);
     FSRemoveDots(temp^.name);
     FSGetFSData(srcID, ptr);
     if ptr^.FileType = DirFile then Oops(temp^.name, ' is a directory.  Cannot append.');

     if (SrcBits mod 8 <> 0) and (temp^.next <> NIL) then
        Oops(temp^.name, ' does not end on a byte boundary.');
     if length(temp^.name) > maxLength then maxLength := Length(temp^.name);
     temp := temp^.next;
  until temp = NIL;
   
{-- All OK, begin work --}

  WriteLn('  ',DstName,' ':maxLength-Length(DstName)+3,'+');

  repeat
     SrcID := FSInternalLookUp(ins^.name,SrcBlocks,SrcBits);
     Write('    ',ins^.name);
     DoAppend(DstBlocks, DstBits);
     if ins^.next = NIL then writeln('.')  {last file}
     else writeLn(' ':maxLength-Length(ins^.name)+1, '+');

     ins := ins^.next;
  until ins = NIL;

  WriteLn('Final length is ',dstBlocks:1,' blocks and ',dstBits:1,' bits.');
  FSClose(DstID, DstBlocks, DstBits);

End { Append }.


