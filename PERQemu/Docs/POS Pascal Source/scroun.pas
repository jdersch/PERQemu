{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module Scrounge;

{-----------------------------------------------------------------------------
 Title: PERQ Pascal Preliminary debugger.
 Abstract: This module contains the procedure Scrounge which allows a small
            amount of debugging.  Since there are no symbol tables or micro--
            code support for breakpoints, you can look at the stack trace and
            examine vbles by offsets.  The types of the variables have to be
            specified by the user.
 Written by: Brad A. Myers  1-May-1981
 Copyright (C) 1981,1982, 1983  Three Rivers Computer Corporation.
 
-----------------------------------------------------------------------------}

{$Version V0.27 for POS}
{----------------------------------------------------------------------------
  Versions:

    04 Apr 83  V0.27  J Strait       Pass the HardCopy exception unchanged.
    
    10 Feb 83  V0.26  Brad Myers     Disabled Ether10 interrupts while in
                                     Scrounge.
    
    30 Nov 82  V0.25  Scott Brown    Undo fix from V0.24, which undid the fix
                                     from V0.21.
    
     3 May 82  V0.24  Ellen Colwell  changed to Imports qcodes from qcodes.dfs
     
    28 Apr 82  V0.23  Brad Myers     Made work for POS D.6.
                                     Fixed ptr dereference.
                                     Fixed spelling of exception.

    23 Mar 82  V0.22  Dave Golub     Fix the CtlShiftC problem for the shell.
                                     Assign false to ShellGlobals^.InCmdFile
                                     and call SClearChars;
                                     
    22 Mar 82  V0.21  SLBrown        Fix to compile under POS, using 
                                     Perq.Qcodes.DFS instead of Qcodes.Dfs.
                                     
    12 Mar 82  V0.20  EMColwell      fix dereference pointer code to print
                                     the number of entries specified instead
                                     of printing up to that number.
                                     
    10 Mar 82  V0.19  EMColwell      allow keyboard input for MPOS.
                                     allow debug to run for MPOS.
                                     
                                     
     2 Mar 82  V0.18  WJHansen       don't declare MPOSVersion
                                     import SystemDefs before use MPOSVersion
                                     import System rather than MultiSystem.
    26 Feb 82  V0.17  Brad Myers     Rewrite Output.
    17 Feb 82  V0.16  Brad Myers     No change Window for MPOS
    16 Feb 82  V0.15  E.M.Colwell    Changes for MPOS
    
    11 Dec 81  V0.14  WJHansen       Change to set InCmdFile False instead
                                         of closing CmdFile
                      Brad A. Myers  enable keyboard at start of Scrounge
                      
     3 Dec-81  V0.13  Brad Myers     ChangeWindow(0).
                                     Help exception.
                                     Command for showing exception parameters.
                                     Command to dereference pointers.
    26-Oct-81  V0.12  John P. Strait Change to agree with V2.11 Memory.
    30-Jun-81  V0.11  Brad A. Myers  Increase screen size for default window
                                     Set ^S Pending to false
                                     Fix ^Shift-D while ^Shift-D
                                     Remove n command (NIY anyway)
     5-Jun-81  V0.10  Brad A. Myers  Fixed ^C handling
                                     More information when Scrounge aborts
     4-Jun-81  V0.9   Brad A. Myers  Catch FSNotFnd for lookups
                                     Clear IO buffer for CleanUp.
     1-Jun-81  V0.8   Brad A. Myers  Fixed bugs in recursive invocation.
    28-May-81  V0.7   Brad A. Myers  Made safer for recursive invocation.
                                     Add all handlers
    23-May-81  V0.6   John P. Strait Enable keyboard interrupts before reading.
    22-May-81  V0.5   John P. Strait Change call to StreamKeyBoardReset.
    20-May-81  V0.4   Brad A. Myers  New stream reset procedure
                                      Compiler switch for run info at top
                                      Proceed command
    12-May-81  V0.3   Brad A. Myers  Use new IO modules; new exceptions
     6-May-81  V0.2   Brad A. Myers  No procedure names for system modules
                                       until debug since takes too long
     1-May-81  V0.1   Brad A. Myers  Started by reworking PMD
-----------------------------------------------------------------------------}


{////////////////////////} EXPORTS {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

Procedure Scrounge(ES, ER, PStart, PEnd, ExcSeg, RaiseAP: Integer);

{////////////////////////} PRIVATE {\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\}

{$R-}

Const DoSysNamesFirst = true; {if true then reads the system run file so can
                               print out system procedure names at the top } 
      LongIDs = true;         {if true then will handle 14 character ids}
      
Imports Screen      from Screen;
Imports Code        from Code;
Imports Memory      from Memory;
Imports Filesystem  from FileSystem;
Imports PERQ_String from PERQ_String;
Imports Except      from Except;
Imports System      from System;

Imports SystemDefs  from SystemDefs;

{$ifc not MPOSVersion then}
Imports Stream      from Stream;
Imports RunRead     from RunRead;
Imports Screen      from Screen;

const
{$INCLUDE Perq.Qcodes.Dfs}

{Imports QCodes      from QCodes.Dfs;}

Imports IO_Unit     from IO_Unit;
Imports IO_Others   from IO_Others;
Imports IOErrors    from IOErrors;
{$endc}

const {$Include ACB.Dfs}
      {$Include RD.Dfs}
      {$Include Except.Dfs}

      version = 'V0.26';
      
Type pCharAr = ^CharAr;
     CharAr = Packed Array [0..511] of Char;
     MyBuf = Record Case Integer of
                      1: (s: pSegBlock);
                      2: (p: pDirBlk);
                      4: (c: pCharAr);
                     End;
Type StackAr = Array[0..0] of integer;
     ByteAr = Packed Array[0..1] of Char;
     pStack = ^StackAr;
     pByteStack = ^ByteAr; {used for chars or strings in the stack}
     RtnDict = Array [0..0] of array [0..7] of integer;
     pRtnDict = ^RtnDict;

     
var glbl1 : String;  {these 3 vbles for debugging}
    glbl2 : integer;
    dumI: integer;
    input, output: Text;
    
    stack: pStack;
    bStack: pByteStack; {used for chars or strings in the stack}
    buf : MyBuf;
    debugSeg: integer;  {segment to use for debugger's data}
    mode: Char; {c, i, s, b}
    radix: integer;
    firstSeg: pSegNode;
    lastSeg: pSegNode;
    runGotten: Boolean;
    srcStackSeg: integer;    {stack segment}
                     

{$ifc not MPOSVersion then}
Function GetSysSeg(seg: integer): pSegNode;
{-------------------------------------------------------------------------
 Abstract: Gets the SegNode for the system segment number seg
 Parameters: seg is the number of the system segment to get node for
 Returns: a pointer to node or NIL if not found
 Environment: if runfile not found, then FirstSeg and LastSeg are NIL,
              otherwise, they are set up
------------------------------------------------------------------------}
   var p: pSegNode;
       i: integer;
   begin
   GetSysSeg := NIL;
   if seg < FirstSystemSegment then exit(GetSysSeg);
   p := FirstSeg;
   for i := 1 to seg-FirstSystemSegment do
     begin
     if p = LastSeg then exit(GetSysSeg);
     p := p^.next;
     end;
   GetSysSeg := p;
   end; {GetSysSeg}

{$endc}


Procedure PrintRoutineName(rtn: integer; f: FileID);
{-------------------------------------------------------------------------
 Abstract: Prints the routine name for rtn specified in file specified.
 Parameters: rtn is routine number and f is fileID
 Environment: Block zero of file better be read into buf.p.
------------------------------------------------------------------------}
   var i, blk, offset: integer;
       rtnName: SimpleName;
       NameLength: integer; {* length of identifers (8 or 14) *}

   begin
   blk := buf.s^.ImportBlock;

{$ifc not LongIDs then}
   NameLength := 8;
{$elsec}
   if buf.s^.LongIds then NameLength:= 14 else NameLength:= 8;
{$endc}
   offset := buf.s^.NumSeg*WordSize(CImpInfo) + rtn*(NameLength div 2);
   blk := blk+offset div 256;
   offset := (offset mod 256)*2;
   rtnName := '';
   FSBlkRead(f, blk, buf.p);
   for i := 0 to NameLength-1 do
       begin
       if offset = 512 then
            begin
            offset := 0;
            FSBlkRead(f, blk+1, buf.p);
            end;
       if buf.c^[offset] > ' ' then AppendChar(RtnName,buf.c^[offset]);
       offset := offset+1;
       end;
   if rtnName <> '' then Write(rtnName,' (',rtn:1,')')
   else Write(rtn:1);
   end; {PrintRoutineName}


Procedure WriteLocation(seg, rtn, addr: integer; suppressAddr,
              inDanger: Boolean );
{-------------------------------------------------------------------------
 Abstract: WriteLocation writes out a code location or an exception number.
            It writes the location (Seg, Rtn, Addr) using the segment table
            to determine the name of Seg (if possible) and the routine
            dictionary of Seg to determine the relative address within
            Rtn.  For an exception, only the segment name and routine number
            are printed.
 Parameters: seg is the number of the segment to be printed;
             rtn is the routine number of the exception or routine to be shown
             addr is the address in the procedure that currently at
             if suppressAddr is true then doesn't print address; otherwise
               does.  Make this false for Exceptions
             inDanger tells WriteLoc not to do any disk addresses since
               these are likely to fail
------------------------------------------------------------------------}
    type pInteger = ^Integer;
    var P: pInteger;
        SegName: SimpleName;
        L, dum: Integer;
{$ifc not MPOSVersion then }
        SystemNames: pSysNames;
{$elsec}
        sit: pSit;
{$endc}
        ok : boolean;
        s: pSegNode;
        fid: FileID;
    begin
{$ifc MPOSVersion then }
    sit := MakePtr(sitSeg, 0, pSit);
{$endc}

      if not suppressAddr then
        begin
          P := MakePtr(Seg, 0, pInteger);
          P := MakePtr(Seg, P^ + Rtn * 8 + RDEntry, pInteger);
          Write(Addr - P^:5);
          Write(' in routine ')
        end;
      SegName := '';
      with SIT^[Seg] do

{$ifc not MPOSVersion then }

        if BootLoaded then
            begin
            ok := false;
            if inDanger then s := NIL
            else s := GetSysSeg(seg);
            if s <> NIL then
              if s^.RootNam <> NIL then
                 begin
                 fid := FSInternalLookUp(Concat(s^.RootNam^,'.SEG'), dum, dum);
                 if fid <> 0 then begin
                                  FSBlkRead(fid, 0, buf.p);
                                  PrintRoutineName(rtn, fid);
                                  ok := true;
                                  end;
                 end;
            if not ok then Write(rtn:2);
            SystemNames := MakePtr(SysNameSeg,0,pSysNames);
            for L := 1 to SysSegLength do
              if SystemNames^[Seg][L] <> ' ' then
                AppendChar(SegName,SystemNames^[Seg][L]);
            end {bootLoaded}
        else

{$endc }

             begin
             if (SwapInfo.DiskId <> 0) and not inDanger then
                begin
                FSBlkRead(SwapInfo.DiskId,0,Buf.p);
                for L := 1 to SegLength do
                    if Buf.s^.ModuleName[L] <> ' ' then
                    AppendChar(SegName,Buf.s^.ModuleName[L]);
                PrintRoutineName(rtn, SwapInfo.DiskID);
                end {have a fileID}
             else Write(rtn:2);
             end; {not Bootloaded}
      if SegName = '' then Write(' in segment ', Seg:2)
      else Write(' in ', SegName);
    end { WriteLocation};


Procedure ShowAll(RaiseAP, curAP:integer; isDump : boolean);
{-------------------------------------------------------------------------
 Abstract: Shows all of stack from RaiseAP to system 0
 Parameters: RaiseAP is the offset for AP for Raise itself (caller is person
               who did the raise)
             curAp is the current AP and it is marked with a <**>
------------------------------------------------------------------------}
   var AP, seg, rtn, addr: integer;
       LocStr : String[21];
   begin
   if isDump then LocStr := 'Debug at    '
   else LocStr := 'Aborted at  ';
   AP := RaiseAP;
   repeat
     Seg := Stack^[AP + ACBRS];
     Rtn := Stack^[AP + ACBRR];
     Addr := Stack^[AP + ACBRA];
     Write(LocStr);
     WriteLocation(Seg, Rtn, Addr, False, False);
     AP := Stack^[AP + ACBDL];
     if curAP=AP then WriteLn('.    <**>')
     else Writeln('.');
     LocStr := 'Called from '
   until (Rtn = 0) and (Seg = FirstSystemSeg);
   end;
 

Procedure PrintVbles(top, first, last: integer);
{-------------------------------------------------------------------------
 Abstract: Displays memory locations specified in mode and radix specified
             by global variables
 Parameters: top is start of data for this field;
             first is offset from top for first item to print
             last is offset from top of last item to print
------------------------------------------------------------------------}
   Handler CtlCAbort;
     {-------------------------------------------------------------------------
      Abstract: Handles one CtlC; exits PrintVbles if hit
     ------------------------------------------------------------------------}
       var c: char;
       begin
       WriteLn(' ^c');
{$ifc not MPOSVersion then}
       IOKeyClear;
{$endc}
       exit(PrintVbles);
       end; {CtlC}

   var i, len: integer;
   begin
   if (mode = 'i') then
      for i := first to last do
        WriteLn(' [',i:2,']  (',top+i:5,'^) = ', stack^[top+i]:1:radix)
   else if mode = 'B' then
      for i := first to last do
        begin
        Write(' [',i:2,']  (',top+i:5,'^) = ');
        if stack^[top+i] = ord(TRUE) then WriteLn('TRUE')
        else if stack^[top+i] = ord (FALSE) then WriteLn('FALSE')
        else WriteLn('UNDEF (',stack^[top+i]:1:radix,')');
        end
   else if mode = 'c' then
        begin
        len := 0;
        for i := first*2 to last*2+1 do
            begin
            if len = 0 then 
              begin
              WriteLn;
              Write(' [',i div 2:2,']  (',top+i div 2:5,'^) = ');
              end;
            Write('''',chr(LOr(ord(bStack^[top*2+i]),#200)),''' ');
            len := (len+1) mod 16;
            end;
        WriteLn;
        end
   else if mode = 'b' then
        begin
        len := 0;
        for i := first*2 to last*2+1 do
            begin
            if len = 0 then 
              begin
              WriteLn;
              Write(' [',i div 2:2,']  (',top+i div 2:5,'^) = ');
              end;
            Write(ord(bStack^[top*2+i]):4:radix);
            len := (len+1) mod 16;
            end;
        WriteLn;
        end
   else if mode = 's' then
        begin
        Write(' [',first:2,']  (',top+first:5,'^) = ');
        len := ord(bStack^[(top+first)*2]);
        Write(' (',len:1,') ''');
        for i := 1 to len do
                Write(chr(LOr(ord(bStack^[(top+first)*2+i]),#200)));
        WriteLn('''');
        end
  else WriteLn('** Impossible mode ',mode);
  end; {PrintVble}

{$ifc not MPOSVersion then}
Procedure GetSysRun;
{-------------------------------------------------------------------------
 Abstract: Reads the system run file if not already read in
 SideEffects: Reads in run file (sets FirstSeg and LastSeg) and sets
                runGotten to true
------------------------------------------------------------------------}
  var fuSeg: pSegNode;
      r: RunFileType;
      RunFileName: PathName;
      dum: integer;
      header: RunInfo;
  Handler ResetError(fileName: PathName);
    begin
    SClearChar('.', RXor); {will be two dots before reset is done}
    SClearChar('.', RXor);
    exit(GetSysRun);
    end;
  begin
  if runGotten then exit(GetSysRun);
  Write('.');
  runGotten := true;
  SysVers(SystemVersion, RunFileName);
  RunFileName := Concat('SYSTEM.',RunFileName);
  AppendString(RunFileName, '.RUN');
  Write('.');
  Reset(r, RunFileName);
  Write('.');
  ReadRunFile(r, DebugSeg, header, FirstSeg, fuSeg, LastSeg, false);
  Write('.');
  ReadSegNames(r, DebugSeg, fuSeg);
  SClearChar('.',RXor);
  SClearChar('.',RXor);
  SClearChar('.',RXor);
  SClearChar('.',RXor);
  end; {GetSysRun}
{$endc}

  

Function DoDebug(ES, ER, pStart, pEnd, RaiseAP:integer;
                 isDump: Boolean): boolean;
{-------------------------------------------------------------------------
 Abstract: Allows debugging
 Parameters: RaiseAP is the AP of the Raise procedure
             ES and ER are for the exception not caught
             isDump tells whether got here through a ^SHIFT-D
 Returns: True if should continue or false if should abort
------------------------------------------------------------------------}
  var curAP, callerAP: integer;
      numParam, top, numResPparam, numLocals, globals, rtn, seg, numGlobals,
            callerRtn, callerSeg, callerGlobals, AP, first, last, offset,
            i : integer;
      s: pSegNode;
      modName: SNArray;
      isProgram: boolean;
      command: String[1];
      c: char;
      rtnD: pRtnDict;
      segP: pStack;
      ans: String[1];

{$ifc not MPOSVersion then}
      SystemNames: pSysNames;
{$elsec}
      SIT: pSit;
{$endc}

      f: FileID;
      
  label 1;

  const CR = chr(13);
        CtrlC = Chr(3);
        CtrlQ = Chr(17);
        CtrlS = Chr(19);
        
   Procedure SetAll(AP: Integer);
    {-------------------------------------------------------------------------
     Abstract: Sets all integer offsets, etc for a specified AP
     Parameters: AP is ap to set numbers for
    ------------------------------------------------------------------------}
      begin
      curAP := AP;
      callerAP := stack^[AP+ACBDL];
      rtn := callerRtn;
      seg := callerSeg;
      globals := callerGlobals;
      callerRtn := stack^[AP+ACBRR];
      callerSeg := stack^[AP+ACBRS];
      callerGlobals := stack^[AP+ACBGL];
      segP := MakePtr(seg, 0, pStack);
      top := stack^[AP+ACBLP];
      rtnD := MakePtr(seg, segP^[0], pRtnDict);
      numParam := rtnD^[rtn][RDPS];
      numResPparam := rtnD^[rtn][RDRPS];
      numLocals := rtnD^[rtn][RDLTS];
      end; {SetAll}
 
   Procedure ShowCurrent;
    {-------------------------------------------------------------------------
     Abstract: Types out message describing current place on stack
    ------------------------------------------------------------------------}
      begin
      Write('  Now at routine ');
      WriteLocation(Seg, Rtn, 0, True, false);
      WriteLn;
      WriteLn('  There are ',numlocals:1,' local words, ',numParam:1,
           ' argument words, and ',numResPparam-numParam:1,' result words.');
      end;

   Procedure InitCallers;
    {-------------------------------------------------------------------------
     Abstract: Sets all caller fields to the correct initial values
    ------------------------------------------------------------------------}
     begin
     callerAP  := stack^[RaiseAP+ACBDL];
     callerRtn := stack^[RaiseAP+ACBRR];
     callerSeg := stack^[RaiseAP+ACBRS];
     callerGlobals := stack^[RaiseAP+ACBGL];
     end;
 
   Procedure DoHelp;
    {-------------------------------------------------------------------------
     Abstract: Shows help
    ------------------------------------------------------------------------}
      begin
      WriteLn;
      WriteLn('x=radix, >=uplevel, <=down level, l=local, a=arguments,');
      WriteLn('r=returns, g=globals, m=mode, d=display stack,');
      WriteLn('c=current, t=Top of stack, b=Bot of stack,');
      WriteLn('s=stack, q=quit, p=proceed, e=exception Args, ^=de-Ref ptr');
      WriteLn;
      end;
   
   Handler HelpKey(var s: sys9s);
    {-------------------------------------------------------------------------
     Abstract: Shows help
    ------------------------------------------------------------------------}
     begin
     DoHelp;
     goto 1;
     end;

 begin  {DoDebug}
 WriteLn;
 Write('Scrounge, ',version,' ');
 
{$ifc not MPOSVersion then}
 GetSysRun;
{$elsec}
 SIT := MakePtr(SitSeg,0,pSit);
{$endc}

 WriteLn;
 WriteLn;
 InitCallers;
 SetAll(CallerAP); {procedure which called raise}
 radix := 10;
 mode := 'i';
 ShowCurrent;
 repeat
   Write('DEBUG>');
   ReadLn(input, command);
   if command = '' then command := ' ';
   Case command[1] of
      '>' : if curAP = stack^[RaiseAP+ACBDL] then 
               WriteLn('  ** Currently at top of stack')
            else begin
                 AP  := stack^[RaiseAP+ACBDL];
                 rtn := stack^[RaiseAP+ACBRR];
                 seg := stack^[RaiseAP+ACBRS];
                 globals := stack^[RaiseAP+ACBGL];
                 repeat
                   callerSeg := seg;
                   callerRtn := rtn;
                   callerGlobals := globals;
                   callerAP := AP;
                   seg := Stack^[AP + ACBRS];
                   rtn := Stack^[AP + ACBRR];
                   globals := Stack^[AP + ACBGL];
                   AP := Stack^[AP + ACBDL];
                 until AP = curAP;
                 SetAll(callerAP);
                 ShowCurrent;
                 end;
      '<' : if (Rtn = 0) and (Seg = FirstSystemSeg) then
              WriteLn('  ** Currently at base of stack.')
            else begin
                 SetAll(callerAP);
                 ShowCurrent;
                 end;
      '^' : begin
            WriteLn(' Type segment, offset of first and offset of last values to print for');
            Write  ('   ptr to dereference: [Exit] ');
            if not eoln(input) then
              begin
              readLn(input, i, first, last);
              stack := MakePtr(i, 0, pStack);
              bStack := MakePtr(i, 0, pByteStack);
              PrintVbles(0, first, last);
              stack := MakePtr(srcStackSeg, 0, pStack);
              bStack := MakePtr(srcStackSeg, 0, pByteStack);
              end
            else ReadLn(input);
            end;
      '?' : begin
            DoHelp;
            end;
      ' ' : ;
      'a' : begin
            Write('  There are ',numParam:1,' arg words. Offset (-1 for all, -2 for range) [Exit]: ');
            if not eoln(input) then
              begin
              readLn(input, offset);
              if offset = -1 then 

                PrintVbles(numResPparam-numParam+top,0,numParam-1)

              else if offset = -2 then 
                begin
                Write('first and last to print: ');
                Read(input, first);
                ReadLn(input, last);
                PrintVbles(numResPparam-numParam+top, first, last);
                end
              else 
                PrintVbles(numResPparam-numParam+top, offset, offset);
              end
            else readLn(input);
            end;
      'b' : begin
            while (rtn <> 0) or (seg <> FirstSystemSeg) do
              SetAll(callerAP);
            ShowCurrent;
            end;
      'c' : begin
            ShowCurrent;
            end;
      'd' : begin
            WriteLn;
            if not isDump then
                begin
                Write('Uncaught Exception: ');
                WriteLocation(ES, ER, 0, True, false);
                WriteLn;
                end;
            ShowAll(raiseAP, curAP, isDump);
            WriteLn;
            end;
      'e' : begin
            Write('  There are ',pEnd-pStart:1, ' exception words. Offset (-1 for all, -2 for range) [Exit]: ');
            if not eoln(input) then
              begin
              readLn(input, offset);
              if offset = -1 then 
                 PrintVbles(pStart, 0, pEnd-pStart-1)
              else if offset = -2 then 
                 begin
                 Write('first and last to print: ');
                 Read(input, first);
                 ReadLn(input, last);
                 PrintVbles(pStart, first, last);
                 end
              else PrintVbles(pStart, offset, offset);
              end
            else readLn(input);
            end;
      'g' : begin

{$ifc not MPOSVersion then }

            if SIT^[seg].BootLoaded then
               begin
               SystemNames := MakePtr(SysNameSeg,0,pSysNames);
               modName :=  RECAST(SystemNames^[Seg], SNArray);
               s := GetSysSeg(seg);
               if s <> NIL then begin
                                numGlobals := s^.GDBSize;
                                isProgram := seg=FirstSystemSeg; (***TEMP***)
                                end
               else begin
                    numGlobals := -1;
                    isProgram := false;
                    end;
               end
            else

{$endc }
                 begin
                 f := SIT^[seg].SwapInfo.DiskID;
                 FSBlkRead(f, 0, buf.p);
                 numGlobals := buf.s^.GDBSize;
                 isProgram := buf.s^.ProgramSegment;
                 modName := buf.s^.ModuleName;
                 end;
            If IsProgram then Write('  Program ')
            else Write('  Module ');
            WriteLn(modName);
            i := 0;
            if isProgram then begin
                              Write('Skip input and output? [Y]');
                              readLn(input, ans);
                              if ans <> 'n' then begin
                                                 i := 2*WordSize(Text);
                                                 numGlobals := numGlobals-i;
                                                 end;
                              end;
            Write('There are ');
            if numGlobals >= 0 then Write(numGlobals:1)
            else write(' an UNKNOWN number of ');
            Write(' words. Offset (-1 for all, -2 for range) [Exit]: ');
            if not eoln(input) then
              begin
              readLn(input, offset);
              if offset = -1 then begin
                                  first := 0;
                                  last := numGlobals-1;
                                  end
              else if offset = -2 then 
                 begin
                 Write('First and last to print: ');
                 Read(input, first);
                 ReadLn(input, last);
                 end
              else begin
                   first := offset;
                   last := offset;
                   end;
              PrintVbles(globals+i, first, last);
              end
            else readLn(input);
            end;
      'l' : begin
            Write('  There are ',numLocals:1, ' local words. Offset (-1 for all, -2 for range) [Exit]: ');
            if not eoln(input) then
              begin
              readLn(input, offset);
              if offset = -1 then 
                 PrintVbles(numResPparam+top, 0, numLocals-1)
              else if offset = -2 then 
                 begin
                 Write('first and last to print: ');
                 Read(input, first);
                 ReadLn(input, last);
                 PrintVbles(numResPparam+top, first, last);
                 end
              else PrintVbles(numResPparam+top, offset, offset);
              end
            else readLn(input);
            end;
      'm' : begin
            Write('  Mode = ',mode,' new mode [CR for exit] : ');
            readLn(input, ans);
            if ans = '' then ans := CR;
            if ans = '?' then WriteLn('   Mode choices are: i=integer, s=string, c=char, B=Boolean, b=byte')
            else if ans[1] IN ['i','s','c','b','B'] then mode := ans[1]
            else if ans = CR then {nothing}
            else WriteLn(' ** Bad mode ',ans,'. Type "m ?" for help');
            end;
      'p' : begin
            DoDebug := True;
            if not IsDump then
               begin
               Write('  Proceed; Not a dump!! Confirm [N] : ');
               readLn(input, ans);
               if ans = 'y' then 
                 begin
                 WriteLn('  ~~~ Returning to program ~~~');
                 exit(DoDebug);
                 end;
               end
            else begin
                 WriteLn('  ~~~ Returning to program ~~~');
                 exit(DoDebug);
                 end;
            end;
      'q' : begin
            Write('  quit  Confirm [Y] : ');
            readLn(input, ans);
            if (ans = 'y') or (ans = '') then
              begin
              DoDebug := false;
              exit(DoDebug);
              end;
            end;
      'r' : begin
            Write('  There are ',numResPparam-numParam:1,' result words. Offset (-1 for all, -2 for range) [Exit]: ');
            if not eoln(input) then
              begin
              readLn(input, offset);
              if offset = -1 then 
                 PrintVbles(top, 0, numResPparam-numParam-1)
              else if offset = -2 then 
                 begin
                 Write('first and last to print: ');
                 Read(input, first);
                 ReadLn(input, last);
                 PrintVbles(top, first, last);
                 end
              else 
                 PrintVbles(top, offset, offset);
              end
            else ReadLn(input);
            end;
      's' : begin
            Write('  Stack Offset (-2 for range) [Exit]: ');
            if not eoln(input) then
              begin
              readLn(input, offset);
              if offset = -1 then {$ifc not MPOSVersion then}
                                  IOBeep
                                  {$endc}
              else if offset = -2 then 
                 begin
                 Write('first and last to print: ');
                 Read(input, first);
                 ReadLn(input, last);
                 PrintVbles(0, first, last);
                 end
              else 
                 PrintVbles(0, offset, offset);
              end
            else ReadLn(input);
            end;
      't' : begin
            InitCallers;
            SetAll(callerAP); {procedure which called raise}
            ShowCurrent;
            end;
      'x' : begin
            write('  Old radix = ',radix:1,' new? ');
            ReadLn(input, radix);
            end;
      Otherwise: writeLn('** Illegal command.  Type ? for help');
      end;  {case}
1: until false;
 end;



Procedure Scrounge(ES, ER, PStart, PEnd, ExcSeg, RaiseAP: Integer);
{-------------------------------------------------------------------------
 Abstract: Scrounge is called when uncaught signals are noticed or when
            the user types ^SHIFT-D.  It allows looking around at local and
            global vbles and the stack trace.  If ^SHIFT-D then can continue
            with program, otherwise aborts when exit
 Parameters: ES - segment number of exception
             ER - routine number of exception
             PStart - offset of start of parameters to exception
             PEnd - offset of end of parameters to exception
             ExcSeg - the segment number of the exceptions module
               if (ES = ExcSeg) and (ER = ErrDump) then is ^SHIFT-D
               For now, can't tell ^SHIFT-C
             RaiseAP - the offset for AP for Raise itself (caller is person
               who did the raise)
------------------------------------------------------------------------}
  Type pString = ^String;
  var userDump, recursiveDebug, wantDebug, oldExcept: Boolean;
      i, seg, AP, rtn, addr, newES, newER: integer;
{$ifc not MPOSVersion then}
      curWin: WinRange;
{$endc}
      StringParam : pString;
      ans: String[1];

  label 1, 2;
  
  Procedure DoCleanUp(abortProg: Boolean);
     {-------------------------------------------------------------------------
      Abstract: Does final cleanup of shell and system state before scrounge
                 returns
      Parameters: if abortProg then raises ExitProgram to abort program after
                   cleaning up command file;  otherwise, continue execution
     ------------------------------------------------------------------------}
    Handler All(ES, ER, PS, PE: integer);
         {------------------------------------------------------------------
          Abstract: Handle all in cleanup and just abort; won't reset command
                     files and all that stuff
         ------------------------------------------------------------------}
       begin
         InPmd := false;
         if (ES <> FirstSystemSeg) or (ER <> ErrExitProgram) then
              WriteLn('Scrounge aborted during Cleanup; Exception ',ER:1,
                         ' in ',ES:1);
         Raise ExitProgram;
       end;
    begin
    if DebugSeg <> 0 then
              begin
              DecRefCount(DebugSeg);
              DebugSeg := 0;
              end;
    if abortProg then
       begin

{$ifc not MPOSVersion then }
       InCmdFile := False;
       SFullWindow;                  { make it full size}
{$elsec}
       ShellGlobals^.InCmdFile := false;
{$endc }
       end
{$ifc not MPOSVersion then }
    else ChangeWindow(curWin)
{$endc}
  ;
    InPmd := false;
    if abortProg then Raise ExitProgram
    else IOSetExceptions(Ether10, IODataInterrupt, oldExcept);
    end; {DoCleanUp}
  
  Handler All(aES, aER, aPS, aPE: integer);
     {-------------------------------------------------------------------------
      Abstract: Handles all exceptions in the debugger;  If is stackOverflow
                  or Segment fault then allow default handler to catch it.
                  Also, allow ExitProgram and HardCopy to pass through;
                  otherwise, print out abort message and as much data as
                  seems safe.
     ------------------------------------------------------------------------}
    begin
    if (aES = ExcSeg) and (aER = ErrDump) then {nothing}
    else if (aES = FirstSystemSeg) and (aER = ErrHelpKey) then {nothing}

    else if (aES = FirstSystemSeg) and (aER = ErrExitProgram) then
              Raise ExitProgram
    else if (aES = FirstSystemSeg) and (aER = ErrHardCopy) then
              Raise HardCopy
    else if RecursiveDebug then {double recursive debug}
            begin
            WriteLn;
            WriteLn('Scrounge aborted; Exception ',aER:1, ' in ',aES:1);
            DoCleanUp(true);
            end
         else begin
              newES := aES;
              newER := aER;
              PStart := aPS;
              PEnd := aPE;
              RecursiveDebug := true;
              UserDump := (aES = ExcSeg) and (aER = ErrDump);
              goto 1;
              end;
    end; {All}
  
  begin {Scrounge}
  oldExcept := false;
  IOSetExceptions(Ether10, IODataInterrupt, oldExcept);
{$ifc not MPOSVersion then}
  IOKeyClear;
  IOKeyEnable(True);    { in case it is disabled }
  curWin := 0;
{$elsec}
  SClearChars;
{$endc}
  FirstSeg := NIL;
  LastSeg := NIL;
  DebugSeg := 0;
{$ifc MPOSVersion then}
  srcStackSeg := StackSegment;
{$elsec}
  InLineByte(LSSN);
  StorExpr(srcStackSeg);
{$endc}
  stack := MakePtr(srcStackSeg, 0, pStack);
  bStack := MakePtr(srcStackSeg, 0, pByteStack);
  newES := 0;
  newER := 0;
  CtrlSPending := false;
  RecursiveDebug := InPmd;
  UserDump := (ES = ExcSeg) and (ER = ErrDump);
1: runGotten := recursiveDebug;
  if UserDump and RecursiveDebug then Exit(Scrounge);
  InPmd := True;                    { prevent recursive PMD }
{$ifc not MPOSVersion then}
  InLineByte(INTON);  {**** NOTE, It is unlikely to get into scrounge with 
                      {**** interrupts off.  Scrounge needs interrupts on to
                      {**** do work.  We hope that this will not mess anything
                      {**** up. }
{$endc}
  if RecursiveDebug then 
     if (newES = FirstSystemSegment) and
          ( {$ifc not MPOSVersion then} (newER = ErrCtlC) or {$endc}
          (newER = ErrCtlCAbort) or
          (newER = ErrCtlShftC)) then
         begin { ^C abort while in debugger}
{$ifc not MPOSVersion then}
         IOKeyClear;
{$elsec}
         SClearChars;
{$endc}
         WriteLn('^C');
         goto 2;
         end
     else begin
          WriteLn;
          Write('Scrounge aborted. Original exception was: ');
          WriteLocation(ES, ER, 0, True, True);
          WriteLn;
          Write('New error is: ');
          ES := newES;
          ER := newER;
          end
  else begin
{$ifc not MPOSVersion then}
       GetWindowParms(curWin, i,i,i,i,wantDebug);
       ChangeWindow(0);
{$endc}
       Rewrite(output, 'console:');
       Writeln;
       CreateSegment(DebugSeg,1,3,20);
       New(DebugSeg,256,Buf.p);
       end;
  wantDebug := true;
{$ifc not MPOSVersion then}
  if IOInProgress then begin
                       Write('Waiting for IO...');
                       while IOInProgress do ;  { wait IO complete }
                       WriteLn('done');
                       end;
{$endc}
  if (ES = ExcSeg) then
      case ER of
        ErrAbort, ErrDump: begin
                           StringParam := MakePtr(srcStackSeg,PStart,pString);
                           Write(StringParam^);
                           end;
        ErrSegmentFault: begin
                         Write('Segment fault, segments');
                         for I := 0 to 3 do Write(' ', Stack^[PStart+I]:1)
                         end;
        ErrDivZero: Write('Division by zero');
        ErrMulOvfl: Write('Overflow in multiplication');
        ErrStrIndx: Write('String index out of range');
        ErrStrLong: Write('String to be assigned is too long');
        ErrInxCase: Write('Expression out of range');
        ErrSTLATE:  Write('Parameter in STLATE instruction is too large');
        ErrUndfQcd: Write('Execution of an undefined Q-code');
        ErrUndfInt: Write('Undefined device interrupt detected');
        ErrIOSFlt:  Write('Segment fault detected during I/O');
        ErrMParity: Write('Memory parity error');
        ErrEStk:    Write('Expression stack not empty at INCDDS');
        ErrOvflLI:  Write('Overflow in conversion Long Integer ==> Integer');
        otherwise:  begin
                    Write('Uncaught Exception: ');
                  {$ifc DoSysNamesFirst and not MPOSVersion then}
                    GetSysRun;
                  {$endc}
                    WriteLocation(ES, ER, 0, True, recursiveDebug);
                    end;
        end
    else if (ES = FirstSystemSegment) and
            ( {$ifc not MPOSVersion then} (newER = ErrCtlC) or {$endc}
              (ER = ErrCtlCAbort) or (ER = ErrCtlShftC)) then
               begin
               Write('Control-C Abort');
               wantDebug := false;
               end
    else begin
         Write('Uncaught Exception: ');
      {$ifc DoSysNamesFirst and not MPOSVersion then}
         GetSysRun;
      {$endc}
         WriteLocation(ES, ER, 0, True, recursiveDebug);
         end;
    Writeln;
    if not RecursiveDebug then
      begin
    {now show stack}
      ShowAll(RaiseAP, -1, UserDump); {may not have system procedure names}
      Writeln;
      if wantDebug then
         begin
         Reset(input, 'CONSOLE:');

{$ifc not MPOSVersion then}
         StreamKeyBoardReset(input);
{$endc}

         Write('Debug? [No] ');
         ReadLn(input, ans);  {ok to ^C here or later}
         if (ans = 'y') or (ans = 'Y') then
              UserDump := DoDebug(ES, ER, pStart, pEnd, RaiseAP, UserDump);
         Close(input);
         end;
      end;

 2: DoCleanUp((not UserDump) or RecursiveDebug);
  end { Scrounge }

{-----------------------------------}
{$ifc false then}
;
Type shortStr = string[21];

Procedure Test2Scrounge(s: ShortStr); {this is an uncaught exception}

  procedure FakeRaise; {this pretends to be raise}
    var AP, rtn, seg, pStart, pEnd, param, resPparam, locals, stackSeg,
          excSeg, callerAP: integer;
      rtnD: pRtnDict;
      segP: pStack;
  begin
  InLineByte(LSSN);
  StorExpr(stackSeg);
  stack := MakePtr(stackSeg, 0, pStack);
  InLineByte(LDAP);
  StorExpr(AP);
  callerAP := stack^[AP+ACBDL];
  seg := stack^[AP+ACBRS];
  segP := MakePtr(seg, 0, pStack);
  rtn := stack^[AP+ACBRR];
  locals := stack^[callerAP+ACBLP];
  rtnD := MakePtr(seg, segP^[0], pRtnDict);
  param := rtnD^[rtn][RDPS];
  resPparam := rtnD^[rtn][RDRPS];

  pStart := resPparam-param+locals;
  pEnd := pStart+param-1;
  
  WriteLn('seg=',seg:1,' rtn=',rtn:1,' locals=',locals:1,' param=',param:1,' resPparam=',resPparam:1,' pStart=',pStart:1,' pEnd=',pEnd:1);

  Write('system exception # (num or CR for no): ');
  if not eoln then begin
                   ReadLn(rtn);
                   if rtn = ErrAbort then 
                     begin
                     Write('Message: ');
                     ReadLn(s);
                     end;
                   excSeg := seg;
                   end
  else begin
       excSeg := 0;
       readLn;
       end;
  Scrounge(seg, rtn, pStart, pEnd, excSeg, AP);
  WriteLn;
  WriteLn('CONTINUING AFTER SCROUNGE');
  end; {FakeRaise}

 begin {Test2Scrounge}
 FakeRaise;
 end;

Function GoingHome(s: ShortStr; c: Char): integer;
  var local1: integer;
      local2: boolean;
  begin
  glbl1 := 'Hello';
  glbl2 := 16;
  local1 := 17;
  local2 := true;
  GoingHome := 14;
  Test2Scrounge('Who woke me up???');
  end;

begin
dumI := GoingHome('I"m going home...','@');
end

{$endc}
.
