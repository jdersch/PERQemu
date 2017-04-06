{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program Link;
{------------------------------------------------------------------------
  Abstract: Program to create Run files from a group of Seg files.  A run
            file is a program that can be executed.
  Copyright (C) 1980,1981,1982,1983 - The Three Rivers Computer Corporation
------------------------------------------------------------------------}

{------------------------------------------------------------------------
  Change Log:

       V4.9  26 Apr 83  Sandeep Johar  Fixed printing of GDB offsets to avoid
                                       negative numbers. Also check for 
                                       stack segment sizes greater than 64K
                                       by checking for integer overflow.
                                       
       V4.8  28 Feb 83  Brad Myers     Fixed String to be assigned is too long
                                         bug that has been around since V4.0.
       V4.7  11 Feb 83  Brad Myers     Undid changes of 4.4a since now have
                                         heaps implemented by memory system
                                         and since 4.4a changes do not work
                                         anymore.
       V4.6  16 Nov 82  Bill Braucher  Fixed names for 14-character compiler.
       V4.5  20 Aug 82  M. Kristofic   Change units for stack and heap switches
                                       to blocks.
       V4.4a  1 Jul 82  C. McPhail     Allow multiple heap segments.
                                       (Note: each New except the first now
                                        needs to be in a Repeat .. Until <> Nil
                                        loop in case exception FullSegment has
                                        been raised first time round.)
       V4.4   2 Mar 82  M. Kristofic   Handle .FOR (fortran) file extension.
       V4.3  26 Jan 82  Brad A. Myers  Not change Run File to upper case.
                                       Not put full path on LastFileName.
       V4.2  19 Jan 82  Brad A. Myers  Fix Link message about file linked.
                                       Warnings have one *.
       V4.1  30-Dec-81  Brad A. Myers  Small bug fixes.
                                       Set the default file to be run file.
                                       UtilProgress busy.
       V4.0  16-Dec-81  Brad A. Myers  New CmdParse interface.
       V3.11 21-Jul-81  John Strait    Add Hints.
       V3.10 2-Jun-81   Brad A. Myers  Fix Map print out so no NIL reference
                                         and so lines aren't so long
       V3.9 12-May-81   Brad A. Myers  Catch some exceptions.
                                       Remove extra writeln from Errors.
                                       Fix sys:boot>link bug.
                                       Prompt for /Continue switch.
                                       Removed requirement for , on first line
                                         after continue.
                                       Aborts if first seg file not found
       V3.8 10-Apr-81   John Strait    Use the RunRead and RunWrite modules.
       V3.7  8-Apr-81   John Strait    Indicate which module is the main
                                       program in the run file by using the
                                       new ProgramSN field in the RunHeader.
       V3.6  4-Apr-81   Brad A. Myers  Fix bug that caused errors when imported
                                        file not found
       V3.5 28-Mar-81   Brad A. Myers  1) /System doesn't set version number.
                                       2) allow ":" to be first char of
                                          filenames
                                       3) Use search lists if run file or main
                                          seg file have them
                                       4) List all seg files not found before
                                          exiting
                                       5) Switch to prevent full path name from
                                          going into run file
                                       6) Remove import of FileProcs
                                       7) Allow more seg names after switches
                                       8) Remove references to POS
       V3.4 25-Mar-81   Miles A. Barel Change file name length.
                                        Change reading of import list.
                                        Add Q-Code version checking.
       V3.3  4-Mar-81   Brad A. Myers  Changed Map file into a switch so can
                                        say "Link new.plx, new.plxutil". Syntax
                                        for map is "/MAP:mapfilename".  If use
                                        "/MAP" then uses runfilename.  If use
                                        "/MAP:" then uses console:. 
       V3.2  3-Mar-81   Brad A. Myers  Made default version take numeric end of
                                         run file. /System does not change
                                         version if version explicitly set.
       V3.1  23-Feb-81  Don Scelza     Changed the name of Header to RunHeader.
       V3.0  18-Feb-81  Brad A. Myers  Fixed switch processing
       V2.6  17-Dec-80  Brad A. Myers  Fixed bug with importing system modules
------------------------------------------------------------------------}

Imports System from System;
Imports FileUtils From FileUtils;
Imports Perq_String From Perq_String;
Imports Memory From Memory;
Imports RunRead from RunRead;
Imports RunWrite from RunWrite;
Imports CmdParse from CmdParse;
Imports UtilProgress from UtilProgress;
Imports Dynamic From Dynamic;

{$R-}
const  LinkerTitle = 'Link V4.9.  Type Link/Help for Help';
        SegExtension = '.SEG';
        ForExtension = '.FOR';
        PasExtension = '.PAS';
        MapExtension = '.MAP';
        RunExtension = '.RUN';
        DefExtLength = 4;
                        
        Debug = false;
        
type    ErrType=(Fatal,Warn,WaitForMore);

var
        HeapSeg: SegmentNumber;         { Current segment for NEW }
        
        TargetI: integer;
        TargetS: string;

        SegFile: FileId;                { Segment file to read }
        RunFile: RunFileType;           { Run file to write }
        
        FirstSeg,                       { List of segments in process }
        LastSeg,
        FirstNewSegment,
        ProgSeg: pSegNode;
        
        ThisImport: pImpNode;
        
        SegBuf: pSegBlock;
        
        MapFile: Text;
        
        SFileName,                      { name of Segment file }
        TFileName,                      { name of First Seg File }
        IFileName,                      { name of current imported file }
        RunFileName,                    { name of Run file }
        MFileName: FNString;            { name of Map file }
        
        SModName: SNArray;
        
        Mapping, DoMapping: boolean;    { indicates map is to be generated }
        Errors: integer;
        
        Header: RunInfo;                { image of the run file header }
        
        TotalSize: integer;             { Total of Segment Sizes (in blocks) }
        
        VerboseMode:Boolean;
        runOpen: boolean;
        ErrStr:CString;                  { Used to transfer info to the }
                                         { error routine. }
                                         
        Blocks,Bits: integer;            { length of current seg file }
        
        S: pSegNode;                     { temporary }
        I: pImpNode;                     { temporary }

        mySearchList: SearchList;        { for use in lookup }
        
Type SwitchTypes = (SNull, SHelp, SStackSize, SStackIncr, SHeapSize,
                     SHeapIncr, SMap, SVerbose, SVersion, SSystem);

Const   NumSwitches = Ord(SSystem);

Var     SwitchNames: CmdArray;
        CopyCmdLine: CString;
        
Procedure DoHelp;
  var i: integer;
  begin
  WriteLn;
  WriteLn('     The Link program is used to create programs that can be executed,');
  WriteLn('     called "Run Files".  A Run file contains pointers to a group of');
  WriteLn('     compiled files called "Seg Files".  The inputs to the linker are');
  WriteLn('     the set of Seg files to use.  The first file must be the main');
  WriteLn('     program.  The files imported by that program will be added to the');
  WriteLn('     Run file.  If other input files are specified, they replace the');
  WriteLn('     default imports.  The output file for the linker is the name of');
  WriteLn('     the Run file.  The syntax for the command line is:');
  WriteLn('         Link prog, imp1, imp2, ...  ~ RunfileName/switch1/switch2=arg');
  WriteLn('     Where the runFileName defaults to prog.Run.');
  WriteLn;
  WriteLn('     Ordinary usage is: ');
  WriteLn('         Link prog');
  WriteLn;
  WriteLn('     Switches are: ');
  for i := 1 to NumSwitches do
     WriteLn('        ',SwitchNames[i]);
  WriteLn;
  exit(Link);
  end;

procedure Error(EType: ErrType; ErrNum: integer );
    Var CNCrt,ErrWrit:String;
        fid: FileID;
        dum: integer;
    
    Procedure Err1;
        Begin
        case ErrNum of
            0: writeln('File ',SFileName,' not found');
            1: writeln('Error reading ',SFileName);
            2: writeln('Multiple program segments');
            3: writeln('No program segment encountered');
            4: writeln(ErrWrit,RunFileName);
            5: writeln(ErrWrit,MFileName);
            6: WriteLn('Number too big');
            7: writeln(CNCrt,RunFileName);
            8: WriteLn('Negative numbers are illegal');
            9: writeln('Bad switch: ',ErrStr);
  {*}      10: ; {* not used *}
           11: writeln('Maximum stack less than initial stack');
  {*}      12: ; {* Not used *}
  {*}      13: ; {* Not used *}
           14: writeln('Error in import section:  Import file name=',
                       IFileName,' in ',SegExtension,' file ',SFileName);
           15: writeln('Segment ',ThisImport^.SId,' from file ',
                       ThisImport^.FilN^,' not found.');
           16: writeln('File name "',ErrStr,'" too long.');
           17: writeln('File System.',TargetS,'.Run not found.');
           18: writeln('File System.',TargetS,'.Run is ill-formatted.');
           19: writeln('Multiple occurances of module ',SModName);
           20: WriteLn('Switch ',errStr,' is not unique');
           21: WriteLn('IMPOSSIBLE!! ',errstr);
           22: WriteLn('Missing switch after /');
           23: WriteLn('Bad decimal value ', errStr);
           24: WriteLn('Bad octal value ',errstr);
           25: begin
               writeln(SFileName,' - Incompatable QCode Version');
               writeln('**        Recompile ',SFileName)
               end;
           26: begin
               Write('Aborting; ',Errors:1,' error');
               if errors <> 1 then write('s');
               WriteLn(' found');
               end;
           27: WriteLn('No room on Search List for ',ErrStr,'. Not pushed.');
           28: WriteLn('Directory ',ErrStr,' not found.');
           29: WriteLn('System.', TargetS:1, '.Run was not linked as version ',
                       TargetI:1, '!');
           30: WriteLn('System.', TargetS,
                       '.Run has an incompatible run file format.');
 {*}       31: ; {* not used *}
           32: WriteLn(ErrStr);
           33: WriteLn('There is only one output file for LINK.');
           34: WriteLn('Illegal character after LINK.');
           35: Writeln('** Initial stack greater than maximum segment size.');
          end { case };
       End {Err1};
       
           
    begin
    
    CNCrt:='Can not create ';
    ErrWrit:='Error writing ';
    if ErrNum <> 32 then 
      if EType = Warn then Write('* ')
      else write('** ');
    
    if EType = WaitForMore then Errors := Errors+1
    else if EType = Warn then Write('WARNING - ');
    
    Err1;
    
    if EType = Fatal then
        begin
        if runOpen then begin
                        if debug then WriteLn('Delete run file: ',RunFileName);
                        Close(RunFile);
                        FSDelete(RunFileName);
                        end;
        if Mapping then begin
                        if debug then writeLn('Closing mapfile: ',MFileName);
                        Close(MapFile);
                        end;
        Exit(Link)
        end;
    ErrStr:='';
    end { Error };

procedure PrintSize(var f:Text; siz:integer);
{ print size as "# blocks (#K words)" on file F }
  var K,Quarters:integer;
  
    begin
    Quarters:=siz mod 4;
    K:=siz div 4;
    write(f,siz:3,' blocks (');
    if K <> 0 then
        begin write(f,K:1);
        if Quarters <> 0 then begin f^ := ' '; Put(F) end
        end;
    case Quarters of
        0: ;
        1: write(f,'1/4');
        2: write(f,'1/2');
        3: write(f,'3/4')
      end {case};
    write(f,'K words)')
    end { PrintSize };

procedure GetSegment( Seg: pSegNode ); forward;

function FindSeg(Fil: pFNString; Seg: SNArray): pSegNode;
{ find and return a pointer to the segment Seg in file Fil.  Returns NIL if }
{ the segment doesn't exist }
  var S,Tmp: pSegNode;  NotFound:Boolean;
begin
FindSeg:=nil;
NotFound:=true;
S:=FirstSeg;
If Debug Then WriteLn('  Debug: Enter FindSeg with "',Fil^,'"  "',Seg,'"');
while (S <> nil) and NotFound do
    with S^ do
        Begin
        If Debug Then 
            WriteLn('    Debug: checking against "',
                    RootNam^,'"  "',SegId,'"');
        if (Seg = SegId) then
            begin
            FindSeg:=S;
            NotFound:=false
            end
        Else If ((Seg = '        ') And (Fil^ = '')) And (S = ProgSeg) Then
            Begin
            FindSeg:=S;
            NotFound:=False;
            End
        else
            S:=Next
       End;
       
   If NotFound Then
       Begin
       New(HeapSeg,1,Tmp);
       LastSeg^.Next := Tmp;
       Tmp^.Next:=Nil;
       Tmp^.UsageCnt:=0;
       Tmp^.RootNam:=Fil;
       LastSeg:=Tmp;
       GetSegment(Tmp);
       FindSeg:=Tmp;
       End;
end { FindSeg };

Procedure AddReqExt(var fileName: PathName; reqExt: String);
  var i: Integer;
  begin
  if debug then write('Add ext "',fileName,'", "',reqExt,'"');
  if reqExt <> '' then
    begin
    i := Pos(fileName, reqExt);
    if debug then
         WriteLn(', Pos = ',i:1, ' len=',Length(fileName)-Length(reqExt)+1:1);
    if (i <> Length(fileName)-Length(reqExt)+1) or (i = 0)
         then AppendString(fileName,reqExt);
    end;
  if Length(fileName) > FileLength then begin
                                        ErrStr := fileName;
                                        Error(Fatal,16);
                                        end;
  end; {AddReqExt}

Procedure RemExt(var fileName: CString; Ext: String);
  var i: Integer;
  begin
  if Ext <> '' then
    begin
    i := Pos(fileName, ext);
    if (i <> 0) and (i = Length(fileName)-Length(Ext)+1) then
         Delete(fileName, Length(fileName)-Length(Ext)+1, Length(Ext));
    end;
  end;

Procedure CheckForDir(segFileName: FNString);
 {checks for a directory on segFileName and if there pushes it on MySearchList;   this procedure should only be called once}
   var i, dum: integer;
       fid: FileID;
   Handler SrchWarn(name: PathName);
   {---------------------------------------------------------
     Abstract: This is raised when filling the last slot of the search list;
                That is OK here, so just continue and allow the Push to happen
   ---------------------------------------------------------}
     begin
     end;
   Handler SrchErr(name: PathName);
   {---------------------------------------------------------
     Abstract: This is raised when the search list if full; warn of problem 
                 and exit CheckForDir
   ---------------------------------------------------------}
     begin
     Error(Warn, 27);
     exit(CheckForDir);
     end;
   
   begin
   for i := length(segFileName) downto 1 do
     begin
     if segFileName[i] = '>' then
        begin
        segFileName := SubStr(segFileName, 1, i);
        if debug then WriteLn('Dir pushed is ',segFileName);
        fid := FSSearch(mySearchList, segFileName, dum, dum);
        ErrStr := segFileName; {just in case}
        if fid = 0 then Error(Fatal, 28)
        else FSPushSearchItem(segFileName, mySearchList);
        if debug then for i := 1 to SEARCHSIZELIST do
           WriteLn('   ',i:1,' : "',mySearchList[i],'"');
        exit(CheckForDir);
        end;
     end;
   end; {CheckForDir} 
 
Procedure ProcessSeg(name: CString; first: boolean);
 {sets the global SFileName}
   var S: pSegNode;
       fid: FileId;
       dum: integer;
   begin
   SFileName := name;
   if first then
      begin
      RemExt(SFileName, ForExtension);
      RemExt(SFileName, PasExtension);
      RemExt(SFileName, RunExtension);
      CheckForDir(SFileName);
      end;
   AddReqExt(SFileName,SegExtension);
   New(heapSeg,1,S);
   New(heapSeg,1,S^.RootNam);
   if FirstNewSegment = nil then FirstNewSegment := S;
   if FirstSeg = nil then FirstSeg := S
   else LastSeg^.Next := S;
   TFileName := SFileName;
   with S^ do
        begin
        If debug then Write('**Seg Name = "',SFileName,'"');
        fid := FSSearch(mySearchList, SFileName, dum, dum);
        If debug then Write(' with dir = "',SFileName,'" fid = ',fid:1);
        Next:=nil;
        if fid = 0 then
           begin
           Error(WaitForMore, 0); {uses SFileName}
           ImpList := nil;
           SegId := '*NotFnd*';
           UsageCnt := 1;    {so not reported as unused}
           RootNam^ := '';   {so don't report missing later}
           end
        else begin
             RootNam^ := Substr(SFileName, 1,
                Length(SFileName)-Length(SegExtension));
             UsageCnt:=0;
             end
        end;
   LastSeg := S;
   end; {ProcessSeg}
   
Function S2I(s: String): Integer;
   var rad, val, neg, start, i, num, stop: integer;
   begin
   errStr := s;  {in case error}
   if s='' then Error(Fatal,23);
   if s[1] = '#' then begin
                      rad := 8;
                      start := 2;
                      end
   else begin
        rad := 10;
        start := 1;
        end;
   if s[start] = '-' then begin
                          Error(Fatal, 8);
                          end
   else neg := 1;
   num := 0;
   if s[length(s)] = '.' then stop := length(s)-1
   else stop := length(s);
   for i := start to stop do
      begin
      val := ord(s[i])-ord('0');
      if (val < 0) or (val >= rad) then if rad = 10 then Error(Fatal, 23)
                                        else Error(Fatal, 24)
      else if ((rad = 10) and (num > 3276) or ((num = 3276) and (val > 7))) or
              ((rad = 8) and (num >= 4096)) then Error(Fatal, 6)
           else num := num*rad+val;
      end;
   num := num*neg;
   S2I := num;
   if debug then WriteLn('Value is ',num:1);
   end; {S2I}
   
Procedure HandleSwitches(switches: pSwitchRec);
   var swval: integer;
   begin
   While switches <> NIL do
      begin
      ConvUpper(Switches^.switch);
      ErrStr := switches^.switch; {in case error}
      If switches^.switch = '' then Error(Fatal,22);
      swVal := UniqueCmdIndex(switches^.switch, SwitchNames, numSwitches);
      if debug then writeLn('Switch=',switches^.switch,' val=',swVal);
      case swVal of
          ord(SHelp): DoHelp;
          ord(SStackSize): begin
                           Header.StackSize :=S2I(switches^.arg);
                           if debug then WriteLn('SS=',Header.stacksize:1);
                           end;
          ord(SStackIncr): begin
                           Header.StackIncr :=S2I(switches^.arg);
                           if debug then WriteLn('SI=',Header.stackIncr:1);
                           end;
          ord(SMap): begin
                     MFileName := switches^.arg;
                     doMapping := true;
                     end;
          ord(SHeapSize): begin
                          Header.HeapSize := S2I(switches^.arg);
                          if debug then WriteLn('HS=',Header.HeapSize:1);
                          end;
          ord(SHeapIncr): begin
                          Header.HeapIncr := S2I(switches^.arg);
                          if debug then WriteLn('HI=',Header.HeapIncr:1);
                          end;
          ord(SVerbose): VerboseMode := true;
          ord(SSystem): Header.System := true;
          ord(SVersion): begin
                         TargetI := S2I(switches^.arg);
                         end;
          numSwitches+1: Error(Fatal,9);
          numSwitches+2: Error(Fatal,20);
          otherWise: Error(Fatal,21);
          end; {case}
      switches := switches^.next;
      end;
   end; {HandleSwitches}

Function HandleLine(ins, outs: pArgRec; switches: pSwitchRec;
                    ok: boolean): boolean;
  begin
  if not ok then Error(fatal, 32);
  HandleSwitches(switches);
  if (outs^.next <> NIL) then Error(Fatal, 33);
  if outs^.name = '' then outs^.name := ins^.name
  else if ins^.name = '' then ins^.name := outs^.name;
  if ins^.name = '' then HandleLine := false
  else begin
       HandleLine := true;
       RemExt(outs^.name, ForExtension);
       RemExt(outs^.name, PasExtension);
       RemExt(outs^.name, SegExtension);
       RunFileName := outs^.name;
       end;
  end;

Procedure ProcessCommands;
  Type NumState = (right, num, done); {used to see if .nn ends run file name}
  var curNumState : NumState;
      i: integer;
      ins, outs: pArgRec;
      switches: pSwitchRec;
      ok: boolean;
      c: Char;
      isSwitch: boolean;
  begin
  mySearchList := FSSysSearchList;
  RunFileName := '';
  CopyCmdLine := UsrCmdLine;
  c := NextString(UsrCmdLine, errStr, isSwitch);  {remove "link"}
  if (c='=') then Error(Fatal, 34);
  errStr := '';
  ok := ParseCmdArgs(ins, outs, switches, errStr);
  while not HandleLine(ins, outs, switches, ok) do {sets RunFileName}
     begin
     Write('File to Link: ');
     Readln(UsrCmdLine);
     CopyCmdLine := Concat('LINK ',UsrCmdLine);
     ok := ParseCmdArgs(ins, outs, switches, errStr);
     end;
  if RunFileName = '' then Error(Fatal,21); {Impossible}
  LastFileName := RunFileName;
  i := length(RunFileName);
  curNumState := right;
  repeat    {check to see if ".nn" on end of run file}
    if i <= 0 then curNumState := done
    else if RunFileName[i] = '.' then
        begin
        if curNumState = num then 
            begin
            TargetI := S2I(SubStr(RunFileName,i+1, length(RunFileName)-i));
            WriteLn('  Using default version of ',TargetI:1);
            end;
        curNumState := done;
        end
    else if (RunFileName[i] >= '0') and (RunFileName[i] <='9') then
                 curNumState := num
    else curNumState := done;
    i := i-1;
  until curNumState = done;
  AddReqExt(RunFileName,RunExtension);
  FixFileName(RunFileName, true);
  ProcessSeg(ins^.name, true);
  IFileName := TFileName;  {save for typeOut}
  ins := ins^.next;
  while ins <> NIL do
     begin
     ProcessSeg(ins^.name, false);
     ins := ins^.next;
     end;
  if debug then WriteLn('Version is ',TargetI:1);
  if doMapping then
     begin
     mapping := true;
     if MFileName = '' then
        MFileName := SubStr(RunFileName,1, Length(RunFileName)-
                               Length(RunExtension));
     AddReqExt(MFileName,MapExtension);
     if debug then writeLn('Map=',MFileName);
     Mapping := true;
     ReWrite(MapFile,MFileName);
     WriteLn(MapFile,LinkerTitle);
     WriteLn(MapFile);
     WriteLn(MapFile,CopyCmdLine);
     end;
  if errors = 1 then Exit(Link)
  else if errors > 1 then Error(Fatal, 26)
  else WriteLn('  ',IFileName, ' ==> ',RunFileName);
  end; {ProcessCommand}  

procedure GetSegment(Seg: pSegNode);
  var ImpCnt,ImpBlk:Integer; c:char; ImpItem: pImpNode;  Sdum:String;
      i,k: integer; S: pSegNode; ThisBlk,ThisOfst: integer;
      ThisEntry:CImpInfo;
      pData: ptrFSDataEntry;
      SrcExtension : string[DefExtLength];
    begin
    ShowProgress(1);
    if Seg^.RootNam^ = '' then exit(GetSegment); {already reported missing}
    SFileName := Concat(Seg^.RootNam^,SegExtension);
    SegFile := FSSearch(mySearchList, SFileName, i, i);
    Seg^.ISN := Header.SegCount;
    Header.SegCount := Header.SegCount + 1;
    if SegFile = 0 then
        begin
        Error(WaitForMore,0);
        Seg^.ImpList := nil;
        Seg^.SegId := '*NotFnd*';
        Exit(GetSegment);
        end;
    Seg^.RootNam^ := Substr(SFileName, 1,     { put full name in run file }
                Length(SFileName)-Length(SegExtension));
    Seg^.Hint.FId := SegFile;
    ShowProgress(1);
    New(heapSeg,1,pData);
    FSGetFSData(SegFile,pData);
    Seg^.Hint.Update := pData^.FileWriteDate;
    Dispose(pData);
    FSBlkRead(SegFile,0,recast(SegBuf,PDirBlk));
    with Seg^ do
        begin
        If VerboseMode Then WriteLn('Reading Seg file:',SFileName);
        if SegBuf^.QVersion <> QCodeVersion then
           begin
           Error(WaitForMore,25);
           Seg^.ImpList := nil;
           Seg^.SegId := '*NotFnd*';
           Exit(GetSegment);
           end;
        if SegBuf^.ProgramSegment then
            begin
            if ProgSeg = nil then
                ProgSeg := Seg
            else
                Error(Warn,2);
            end;
        SegId := SegBuf^.ModuleName;
        IFileName := SegBuf^.FileName;
        { Make Sure there are no other segments with the same Module Name }
        S := FirstSeg;
    {*BAM*} while S <> FirstNewSegment do
                begin
                if s^.SegID = SegID then s^.segID := '*UNUSED*';
                s := s^.next;
                end;
        S := FirstSeg;
        while S <> Seg do
            { NOTE: This uses the knowledge that the segment list is only
              defined at this time up to this point (i.e., the segment list
              is defined from FirstSeg in order to the end of the chain)
            }
            begin
            ShowProgress(1);
            if S^.SegId = {Seg^.}SegId then
                begin
                SModName:={Seg^.}SegId;
                Error(Fatal,19)
                end;
            S := S^.Next
            end;
        ImpList:=nil;
        ImpCnt:=SegBuf^.NumSeg;
        ImpBlk:=SegBuf^.ImportBlock;
        CodeSize:=(ImpBlk-1)*256;
        XSTSize:=2 * ImpCnt;

        { Check for integer overflow before doing the arithmetic }
        
        If (Header.CurOffset < 0) And (Header.CurOffset + XSTSize > 0) Then
            Error(Fatal, 35);
        GDBOff:=Header.CurOffset + XSTSize;
        GDBSize:=SegBuf^.GDBSize;
        if odd(GDBSize) then GDBSize:=GDBSize+1;
                         { GP must always point to a double word boundary }
        

        { Check for integer overflow before doing the arithmetic }
        { GDBOff = Header.CurOffset + XSTSize is true here }

        if (GDBOff < 0) And (GDBOff + GDBSize > 0)
            then error(Fatal, 35);          
            
        Header.CurOffset:=Header.CurOffset+XSTSize+GDBSize;
        ThisBlk:=ImpBlk;
        ThisOfst:=0;
        FSBlkRead(SegFile,ThisBlk,recast(SegBuf,PDirBlk));
        for i:=1 to ImpCnt do
            begin
            ShowProgress(1);
            for k:=0 to WordSize(CImpInfo)-1 do
                begin
                ThisEntry.Ary[k]:=SegBuf^.Block[ThisOfst];
                ThisOfst:=ThisOfst+1;
                if ThisOfst = 256 then
                    begin
                    ThisBlk:=ThisBlk+1;
                    ThisOfst:=0;
                    FSBlkRead(SegFile,ThisBlk,recast(SegBuf,pDirBlk))
                    end
                end;
            with ThisEntry do
                begin IFileName := FileName;
                New(heapSeg,1,ImpItem);
                New(heapSeg,1,ImpItem^.FilN);
                with ImpItem^ do
                     begin
                     if VerboseMode Then 
                         begin
                         if Length(IFileName) <> 0 then
                             WriteLn('    Imports ',IFileName)
                         else
                             WriteLn('    Imports MainProgram')
                         end;
                     if Length(IFileName) <> 0 then
                         begin
                         if Pos(IFileName,PasExtension) = 0
                             then SrcExtension := ForExtension
                             else SrcExtension := PasExtension;
                         if Pos(IFileName,SrcExtension) = Length(IFileName)-
                                    length(SrcExtension)+1 then
                              IFileName[0]:=chr(ord(IFileName[0]) -
                                length(SrcExtension));
                         end;
                     SId:=ModuleName;
                     FilN^:=IFileName;
                     Next:=ImpList;
                     Seg:=nil
                     end;
                 ImpList:=ImpItem
                 end
             end
        end;
    end { GetSegment };

    
    procedure DoMapStuff;
     var S: pSegNode; I: pImpNode; K: RunElement;
     begin
     S := FirstSeg;
     K := SysSegment;
     while S <> nil do
         with S^ do
             begin
             ShowProgress(1);
             if S = FirstNewSegment then K := UserSegment;
             TotalSize:=TotalSize+(CodeSize + 255) div 256;
             if K = UserSegment then
                 begin
                 Writeln(MapFile);
                 Writeln(MapFile,'  Segment:',SegId,
                         '  File:',RootNam^);
                 Write(MapFile,'    GDB Size:',GDBSize:5,
                       ' words     Code Size:');
                 PrintSize(MapFile,(CodeSize + 255) div 256);
                 writeln(MapFile,'    Segment = ',ISN:1);
                 I:=ImpList;
                 if Mapping and (ImpList <> nil) then
                         writeln(MapFile,'Imports:':12);
                 while I <> nil do with I^, Seg^ do
                     begin
                     Write(MapFile,SegId:16);
                     if RootNam <> NIL then
                        begin
                        Write(MapFile, ' from');
                        if Length(RootNam^) <= 15 then
                            Write(MapFile, RootNam^:16)
                        else begin
                             WriteLn(MapFile);
                             Write(MapFile, RootNam^:37);
                             end;
                        end
                     else write(MapFile, ' ':21);
                     WriteLn(MapFile, '    GDB offset = ',GDBOff:6:-10,
                           '    Segment = ',ISN:1);
                     I := I^.Next
                     end
                 end;
             S := Next
             end;
    If Errors>0 Then
        Begin
        WriteLn(MapFile);
        WriteLn(MapFile,'**** ',Errors,' errors detected ****');
        WriteLn(MapFile);
        End;
    Writeln(MapFile);
    Write(MapFile,'Specified Stack Size   ');
    PrintSize(MapFile,Header.StackSize);
    Writeln(MapFile);
    Write(MapFile,'Initial Stack Size     ');
    PrintSize(MapFile,(Header.CurOffset + 255) div 256 + Header.StackSize);
    Writeln(MapFile);
    Writeln(MapFile,'Globals              ',Header.CurOffset:6:-10,' words');
    Write(MapFile,'Stack Size Increment   ');
    PrintSize(MapFile,Header.StackIncr);
    Writeln(MapFile);
    Write(MapFile,'Initial Heap Size      ');
    PrintSize(MapFile,Header.HeapSize);
    Writeln(MapFile);
    Write(MapFile,'Heap Size Increment    ');
    PrintSize(MapFile,Header.HeapIncr);
    Writeln(MapFile);
    Write(MapFile,'Total virtual memory   ');
    PrintSize(MapFile,TotalSize);
    writeln(MapFile);
    Close(MapFile)
    End {DoMapStuff};
    
    
Procedure Initialize;
    begin
    FSAddToTitleLine(LinkerTitle);
    SwitchNames[Ord(SHelp)] :=      'HELP       Print this message.';
    SwitchNames[Ord(SStackSize)] := 'STACKSIZE  Set the preliminary stack size.  Arg is in blocks.';
    SwitchNames[Ord(SStackIncr)] := 'STACKINCR  Set the preliminary stack increment.  Arg is in blocks.';
    SwitchNames[Ord(SHeapSize)] :=  'HEAPSIZE   Set the preliminary heap size.  Arg is in blocks.';
    SwitchNames[Ord(SHeapIncr)] :=  'HEAPINCR   Set the preliminary heap increment.  Arg is in blocks.';
    SwitchNames[Ord(SMap)] :=       'MAP        Create a MAP file.  Optional arg is fileName for map.';
    SwitchNames[Ord(SVerbose)] :=   'VERBOSE    Print imports for each module on the screen.';
    SwitchNames[Ord(SVersion)] :=   'VERSION    Specify the system version number.  Arg is version number.';
    SwitchNames[Ord(SSystem)] :=    'SYSTEM     Specify that this is to be a system run file.';
    SFileName := '';
    runOpen:= false;
    ErrStr:='';
    Errors:=0;
    Header.Version := -1;
    Header.System := false;
    Header.InitialGP := -1;
    Header.CurOffset := StackLeader;
    Header.StackSize:=DefStackSize;
    Header.StackIncr:=DefIncStack;
    Header.HeapSize:=DefHeapSize;
    Header.HeapIncr:=DefIncHeap;
    Header.ProgramSN := -1;
    Header.SegCount := 0;
    VerboseMode := false;
    CreateHeap(HeapSeg, 20);
    New(HeapSeg,256,SegBuf);
    FirstSeg := nil;
    FirstNewSegment := nil;
    LastSeg := nil;
    ProgSeg := nil;
    TotalSize := 0;
    TargetI := SystemVersion;
    mapping := false;
    doMapping := false;
    end { Initialize };
    
    
    procedure OpenRunFile;
    begin { OpenRunFile }
     ShowProgress(1);
     if Header.System and (RunFileName = 'SYSTEM.RUN') then
      begin SysVers(TargetI,RunFileName);
       RunFileName := Concat('SYSTEM.',RunFileName);
       AppendString(RunFileName,RunExtension);
       if Mapping then
        begin Writeln(MapFile);
         Writeln(MapFile,'  Writing new system run file ',RunFileName);
         Writeln(MapFile)
        end
      end;
     Rewrite(RunFile,RunFileName);
     ShowProgress(1);
     runOpen := true;
    end { OpenRunFile };
    
    
    procedure ReadSystemRunFile;
    var S, First, FirstUser, Last: pSegNode;
        SysRun: RunFileType;
        SysFileName: string;
        SysHeader: RunInfo;
        fid: FileID;
        dum: integer;
    begin { ReadSystemSysRun }
    SysVers(TargetI,TargetS);
    SysFileName := Concat('SYSTEM.', TargetS);
    AppendString(SysFileName, RunExtension);
    ShowProgress(1);
    fid := FSLookUp(SysFileName,dum,dum);
    if fid = 0 then begin
                    SFileName := SysFileName;
                    Error(Fatal,0);
                    end;
    ShowProgress(1);
    Reset(SysRun,SysFileName);
    ShowProgress(1);
    ReadRunFile(SysRun,0,SysHeader,First,FirstUser,Last,False);
    ShowProgress(1);
    Close(SysRun);
    if First = nil then
      if SysHeader.RFileFormat <> RFileFormat then Error(Fatal,30)
      else Error(Fatal,18);
    if SysHeader.Version <> TargetI then Error(Fatal,29);
    Last^.Next := FirstSeg;
    FirstSeg := First;
    Header.SegCount := SysHeader.SegCount;
    Header.CurOffset := (SysHeader.CurOffset + FudgeStack + 255) div 256 * 256
                                                       { round to 256 }
    end { ReadSystemRunFile };
    
    
 begin { PerqLinker }
    Initialize;
    ProcessCommand;      { sets up SegId, RootNam, UsageCnt, Next }
    LoadBusy;
    OpenRunFile;
    if not Header.System then ReadSystemRunFile;
    S := FirstNewSegment;
    while S <> nil do
     begin GetSegment(S); S := S^.Next end;

    if debug then WriteLn('*Done all segs*');
    
    if ProgSeg = nil then
        Error(Fatal,3);
    
    S:=FirstNewSegment;        { make sure all referenced segments are here }
    while S <> nil do
        with S^ do
            begin
            ThisImport:=ImpList;
            while ThisImport <> nil do
                with ThisImport^ do
                    begin
                    Seg:=FindSeg(FilN,SId);
                    if Seg = nil then
                        Error(Fatal,15)
                    else
                        with Seg^ do
                            UsageCnt:=UsageCnt+1;
                    ThisImport:=Next
                    end;
            S:=Next
            end;
    
    S:=FirstNewSegment;
    while S <> nil do
        with S^ do
            begin
            if (UsageCnt = 0) and (S <> ProgSeg) then
                writeln('* WARNING - Segment ',SegId,' from ',RootNam^,
                        ' is not referenced');
            S:=Next
            end;

    if Errors = 0 then
      begin
        Header.Version := TargetI;
        Header.InitialGP := ProgSeg^.GDBOff;
        Header.ProgramSN := ProgSeg^.ISN;
        WriteRunFile(RunFile,Header,FirstSeg,FirstNewSegment);
        Close(RunFile)
      end;
        
    if Mapping then DoMapStuff;
    
    QuitProgress;

    if mapping then WriteLn(MFileName,' written.');
    
    if errors > 0 then Error(Fatal, 26);  {abort}

end { PERQLinker }.
