{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program FindString;
{----------------------------------------------------------
  Fast program to find all files that contain at least one occurance of a
   string

  Written by: Brad A. Myers  2-Sept, 1981
  
  Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation
----------------------------------------------------------}

{----------------------------------------------------------
   Change log:
     16 Nov 82  Bill Braucher V2.3 Fixed names for 14-character compiler.
     14 May 82  Brad Myers  V2.2  Fixed bug when writing to a file.
      4 Jan 82  Brad Myers  V2.1  Fixed bug in NoCaseSensitive.
     30 Dec 81  Brad Myers  V2.0  New user interface from CmdParse.
                                  Doesn't bomb out with files of 0 length.
      2 Sep 81  Brad Myers  V1.0  Created
----------------------------------------------------------}

imports FileUtils from FileUtils;
imports PMatch from PMatch;
imports ReadDisk from ReadDisk;
imports PERQ_String from PERQ_String;
imports UtilProgress from UtilProgress;
imports CmdParse from CmdParse;

Const debug = false;
      CR = Chr(13);
      Title = 'FindString V2.3. Type /Help for Help.';

Const NoContextIndex = 1;
      ContextIndex = 2;
      CaseSensitiveIndex = 3;
      NoCaseSensitiveIndex = 4;
      HelpIndex = 5;
      NumSwitches = 5;
      
var scanptr: ptrScanRecord;
    fid : FileID;
    addr: DiskAddr;
    filePattern, outName: PathName;
    sid: SegID;
    ptr: PtrDiskBuffer;
    hdr: PtrHeader;
    c: char;
    numMatches, numFiles, numScanFiles, index, patIndex, i, numBlks, blkCount,
       start, stop, bits, endIndex, lineCount: integer;
    pattern: String;
    ans: String[1];
    fold, context, matchThisFile: Boolean;
    switchAr: CmdArray;
    lineWithMatch: String;
     
      

Procedure DoHelp;
  begin
  WriteLn;
  WriteLn('     FindString is used to search through a number of files for an occurance');
  WriteLn('     of a particular string.  It has two modes.  If /context switch is used');
  WriteLn('     then about 10 characters on each side of each occurance will be printed.');
  WriteLn('         NOTE: A carriage return is printed as: ',chr(#215), chr(#212));
  WriteLn('     If /NoContext is used, then only the first occurance per file is noted and');
  WriteLn('     no context is given.');
  WriteLn;
  WriteLn('     The first argument to FindString is the pattern to look for.');
  WriteLn('     The second argument is the file pattern to match files against.');
  WriteLn('     FindString looks in all files matching the file pattern for');
  WriteLn('     the first pattern.');
  WriteLn('         NOTE: To include a space, comma, or slash in the pattern,');
  WriteLn('               precede it by an apostrophe ('').');
  WriteLn('     An output may be specified which will be a file for the output to go to.');
  WriteLn('     If there is no output specified, then output goes to the screen.');
  WriteLn;
  WriteLn('     A sample invocation might be: ');
  WriteLn('         FindString screen, :boot>os>*.pas ~ screenUsers/noContext');
  WriteLn;
  WriteLn('     The valid switches are:');
  for i := 1 to NumSwitches do
      WriteLn('         ',switchAr[i]);
  WriteLn;
  WriteLn('     The defaults are /Context and /NoCaseSensitive');
  WriteLn;
  exit(FindString);
  end;


function Nextfile(var fid: FileID): boolean;
   var fileName: PathName;
   begin
   nextfile := true;

   if debug then WriteLn('~~Dir is :',scanPtr^.dirName);

   while FSScan(scanptr, fileName, fid) do
      begin
      if debug then WriteLn('~~file :',filename);
      if PattMatch(fileName, filePattern, true) then
         begin
         write(scanptr^.dirname,fileName,': ');
         if context then writeln;
         exit(NextFile);
         end;
      end;
   nextfile := false;
   end;

Procedure NumWrite(num: Integer; what, plural: String);
   begin
   Write(num:1,' ',what);
   if num <> 1 then Write(plural);
   end;

Procedure HandleLine;
  var i: integer;
      ins, outs: pArgRec;
      switches: pSwitchRec;
      err: String;
      ok, leave: boolean;
      c: Char;
      s: CString;
  begin
  s := '';
  err := '';
  c := NextId(s, ok);  {remove "FindString"}
  if (c<>' ') and (c<>CCR) then StdError(ErIllCharAfter, 'FindString', true);
  ok := ParseCmdArgs(ins, outs, switches, err);
  pattern := '';
  filePattern := '';
  outName := '';
  context := true;
  fold := true;
  repeat
    if not ok then StdError(ErAnyError,err, true);
    while switches <> NIL do
        begin
        ConvUpper(switches^.switch);
        i := UniqueCmdIndex(switches^.switch, switchAr, NumSwitches);
        if (i <= NumSwitches) and (switches^.arg <> '') then
           StdError(ErNoSwParam, switches^.switch,true);
        case i of
          NoContextIndex : context := false;
          ContextIndex: context := true;
          CaseSensitiveIndex: fold := false;
          NoCaseSensitiveIndex: fold := true;
          HelpIndex: DoHelp;
          NumSwitches+2: StdError(ErSwNotUnique, switches^.switch, true);
          otherwise: StdError(ErBadSwitch, switches^.switch, true);
          end;
        switches := switches^.next;
        end;
    if ins^.name = '' then 
       begin
       if pattern <> '' then Write('File Pattern to search in: ')
       else Write('Pattern to search for: ');
       Readln(s);
       end
    else if ins^.next = NIL then 
       if pattern = '' then
            begin
            pattern := ins^.name;
            Write('File Pattern to search in: ');
            ReadLn(s);
            end
       else filePattern := ins^.name
    else if (pattern <> '') or (filePattern <> '') then
               StdError(ErAnyError,'** There are only two inputs for FindString.', true)
         else begin
              pattern := ins^.name;
              filePattern := ins^.next^.name;
              end;
    if ins^.next <> NIL then
        if (ins^.next^.next <> NIL) then
          StdError(ErAnyError,'** There are only two inputs for FindString.', true);
    if (outs^.next <> NIL) then StdError(ErOneOutput, 'MakeDir',true);
    if outs^.name <> '' then outName := outs^.name;
  leave := (filePattern <> '') and (pattern <> '');
  if not leave then ok := ParseStringArgs(s, ins, outs, switches, err);
  until leave;
  if not RemoveQuotes(pattern) then StdError(ErBadQuote,'',true);
  if not RemoveQuotes(filePattern) then StdError(ErBadQuote,'',true);
  if not RemoveQuotes(outName) then StdError(ErBadQuote,'',true);
  if outName <> '' then
       begin
       WriteLn('Output being directed to file ',outName);
       ReWrite(Output, outName);
       end;
  end;



label 1;

{$R-}

begin

SwitchAr[NoContextIndex] :=     'NOCONTEXT       Show only the first match in each file with no context';
SwitchAr[ContextIndex] :=       'CONTEXT         Show about 10 characters on each side of each occurance';
SwitchAr[CaseSensitiveIndex] := 'CASESENSITIVE   Case is significant when searching';  
SwitchAr[NoCaseSensitiveIndex]:='NOCASESENSITIVE Case is not significant';
SwitchAr[HelpIndex] :=          'HELP            Print this message';

  FSAddToTitleLine(title);

  HandleLine;

if fold then ConvUpper(pattern);

WriteLn;
Write('Files matching "',filePattern,'" containing "',pattern,'"');
if fold then WriteLn(' (case ignored)')
else writeln;
WriteLn;

NEW(scanPtr);
i := RevPosC(filePattern,'>');
if i <> 0 then begin
               scanPtr^.dirName := SubStr(filePattern,1,i);
               filePattern := SubStr(filePattern, i+1, length(filePattern)-i);
               end
else scanPtr^.dirName := '';
scanPtr^.initialcall := true;

numMatches := 0;
numFiles := 0;
numScanFiles := 0;

while NextFile(fid) do
  begin
  LoadCurs;
  sid := FileIDToSegID(fid);
  hdr := ReadHeader(sid);
  ptr := ReadDisk(sid);
  numBlks := ptr^.FSData.FileBlocks;
  bits := ptr^.FSData.FileBits;
  if numBlks = 0 then blkCount := 1
  else blkCount := 1024 div numBlks;
  index := 512;
  endIndex := 512;
  patIndex := 1;
  lineCount := 1;
  matchThisFile := false;
  numScanFiles := numScanFiles+1;
  repeat
     if index = endIndex then begin
                              addr := hdr^.nextAdr;
                              if addr = DBLZERO then goto 1;
                              ShowProgress(blkCount);
                              ptr := ReadDisk(addr);
                              hdr := ReadHeader(addr);
                              if hdr^.nextAdr = DBLZERO then {last blk}
                                  endIndex := bits div 8;
                              index := 0;
                              end;
     if fold then c := UpperCase(chr(ptr^.byteData[index]))
     else c := chr(ptr^.byteData[index]);
     if c = pattern[patIndex] then
          begin
          if patIndex >= length(pattern) then
             begin
             if context then
                begin
                start := index-patIndex-9;
                if start < 0 then start := 0;
                stop := start+length(pattern)+20;
                if stop >= endIndex then stop := endIndex-1;
                {$R-}
                lineWithMatch[0] := chr(stop-start+1);
                {$R=}
                for i := start to stop do
                  lineWithMatch[i-start+1] := chr(Lor(ptr^.byteData[i],#200));

{after the first WriteLn, the disk buffer ptrs may be invalid since we
 might be writing to a file.  Therefore, don't use ptr or hdr after write
 until read them again.}

                Write('         ');
                end;
             Write(' *Match');
             numMatches := numMatches+1;
             if context then
                begin
                if not matchThisFile then
                                begin
                                matchThisFile := true;
                                numFiles := numFiles+1;
                                end;
                WriteLn(' on line ',lineCount:4,': ',lineWithMatch);
                patIndex := 1;
                end
             else begin
                  Write('*');
                  patIndex := 1;
                  goto 1;
                  end;
             ptr := ReadDisk(addr);    {may have done WriteLns to a file}
             hdr := ReadHeader(addr);  {may have done WriteLns to a file}
             end
          else patIndex := patIndex+1;
          end
     else if patIndex <> 1 then
              begin
              index := index-patIndex+1;
              if index < -1 then begin
                                 index := index+512;
                                 addr := hdr^.prevAdr;  {backup}
                                 ptr := ReadDisk(addr);
                                 hdr := ReadHeader(addr);
                                 endIndex := 512;
                                 end;
              patIndex := 1;
              end
     else if c = CR then lineCount := lineCount+1;
     index := index+1;
  until false;
1: if not context then WriteLn;
  end;

Writeln;
Write('Found ');
if context then begin
                NumWrite(numMatches,'match','es');
                Write(' in ');
                NumWrite(numFiles,'file','s');
                end
else begin
     Write('matches in ');
     NumWrite(numMatches,'file','s');
     end;
Write(' out of ');
NumWrite(numScanFiles,'file','s');
WriteLn(' scanned.');

if outName <> '' then Close(output);
end.
