{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program SetSearch(input, output);
{----------------------------------------------------------------------------
  User level program to modify the search lists
  Written by: Brad A. Myers  27-Mar-81
  Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation
-----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
 Versions:
   v1.4  13 may 82 Ellen Colwell added /reset and fixed /noconfirm bug.
   V1.3  31-dec-81 Ellen Colwell Implemented standard user interface for D.6
   V1.2  12-May-81 Brad Myers  Checks to see if shell can be found before exit
   V1.1  30-Mar-81 Brad Myers  Fixed to take multiple args on cmd line
   V1.0  27-Mar-81 Brad Myers  Created
----------------------------------------------------------------------------}

imports FileUtils   from FileUtils;
imports PopCmdParse from PopCmdParse;
imports System      from System;
imports CmdParse    from CmdParse;
imports FileSystem  from FileSystem;
imports PERQ_String from PERQ_String;

var
  fid             : FileID;
  dum, i          : Integer;
  haveArgs        : boolean;
  ans             : integer;
  tmp             : PathName;
  ins,outs        : pArgRec;
  switches        : pSwitchRec;
  SwitchTable     : CmdArray;
  err             : CString;
  numSwitches     : integer;
  c               : char;
  s               : CString;
  isSwitch        : boolean;
  ok              : boolean;
  Line            : CString;
  errflag         : boolean;
  beginning       : boolean;
  some            : boolean;
  done            : boolean;
  confirm         : boolean;
  emptyargs       : boolean;
  single          : boolean;
    
const START=1;
      NOARGS=2;
      POPLAST=3;
      FILLFIRST=4;
      NOSHELL=5;
      HELP=1;
      CONF=2;
      NOCONFIRM=3;
      RESET=4;
      VERSION='1.4';
      
      

Function FindHoleForPush: integer;
 {------------------------------------------------------------------------
   Abstract: Finds a hole in the front of the search list and returns its index
   Returns: index of hole if room else 0
 -------------------------------------------------------------------------}
  var i: integer;
      leave: boolean;
  begin
  if (FSSysSearchList[1] <> '') then FindHoleForPush := 0
  else
    begin
      i := -1;
      leave := false;
      repeat
        i := i+1; {start at 0}
        if i >= SearchSizeList then leave := true
        else if FSSysSearchList[i+1] <> '' then leave := true;
      until leave;
      FindHoleForPush := i;
    end;
  end {FindHoleForPush};
  

Function FindItemToPop: Integer;
 {------------------------------------------------------------------------
   Abstract: Finds index of last item on search list
   Returns: index of last item or 0 if list empty;
 -------------------------------------------------------------------------}
  var i: integer;
      leave: boolean;
  begin
   i := 0;
   leave := false;
   repeat
     i := i+1; {start at 1}
     if i > SearchSizeList then leave := true
     else if FSSysSearchList[i] <> '' then leave := true;
   until leave;
   if i > SearchSizeList then FindItemToPop := 0
   else FindItemToPop := i;
  end {FindItemToPop};


Procedure ResetList;
{-------------------------------------------------------------------------
{
{  Abstract: empties search list except for last item.
{
{-------------------------------------------------------------------------}
  var i: integer;
  
  begin
  
  for i:=1 to (SearchSizeList-1) do begin
     FSSysSearchList[i]:= '';
     end;
     
  end; {* ResetList *}
  
  

Procedure PrintSearchList;
 {------------------------------------------------------------------------
   Abstract: Prints out the system search list
 -------------------------------------------------------------------------}
   var i: integer;
   begin
   WriteLn;
   For i := 1 to SEARCHSIZELIST do
       begin
       if i = 1 then                   Write('Head: ')
       else if i = SEARCHSIZELIST then Write('Tail: ')
       else                            Write('      ');
       WriteLn('[',i:1,'] = "',FSSysSearchList[i],'"');
       end;
   WriteLn;
   end;

procedure error(err:Cstring);
{--------------------------------------------------------------------------
{ Abstract: This procedure prints the input error string and then exits
{           the setsearch program.
{ Parameters: err- error msg to be printed.
{ SideEffects: SetSearch is exited.
{---------------------------------------------------------------------------}

   begin
   writeln('**ERROR: ',err);
   exit(SetSearch);
   end;  {* of procedure *}
   

procedure CheckForOuts(outs: pArgRec);
{---------------------------------------------------------------------------
{ Abstract: This procedure checks to see that the output list is empty.
{ Parameters: outs- output list.
{ SideEffects: if output list is not empty, then setsearch is exited.
{---------------------------------------------------------------------------}

   begin
   if outs^.name <> '' then
      begin
      writeln('**ERROR: Input Directories must be separated by commas.');
      writeln('    Type SetSearch/help for help');
      exit(SetSearch);
      end; {* of if outs^.name *}
   end;  {* of procedure *}
   

procedure printhelp(where:integer);
{-----------------------------------------------------------------------------
{ Abstract: Prints out the help message
{ Parameters: where-tells us which message to print.
{ SideEffects: message gets printed
{-----------------------------------------------------------------------------}

begin
   
case where of
   
 START: begin
   writeln;
   writeln('        Setsearch is used to modify the search paths of the current user');
   writeln;
   writeln('        Usage: SetSearch [{directory_name,}] [{-}] [switches] ');
   writeln;
   writeln('           directory_name: is a valid directory name which will be');
   writeln('                           added to the top of the search list.   ');
   writeln;
   writeln('           "-": specifies that the current top item of the search  ');
   writeln('                list is to be deleted.                             ');
   writeln;
   writeln('           If you opt not to enter a directory name or "-", you will');
   writeln('           be prompted for a [single] dirname or "-".  A CR in response');
   writeln('           to the prompt will exit the program');
   writeln;
   writeln('        Note: The CURRENT directory is always searched first.');
   writeln;
   writeln('        The valid Switches are: /HELP, /NoConfirm, /Confirm,');
   writeln('        and /RESET.  /Reset pops all but the first (usually boot)');
   writeln('        directory from the search list.');
   writeln;
   end; {* of START case *}
   
 
 NOARGS: begin
   writeln;
   writeln('        Entering a directory name followed by a CR will add a ');
   writeln('        directory to your current search list');
   writeln;
   writeln('        Entering a "-" will remove the top item (lowest numbered)');
   writeln('        from the current search list');
   writeln;
   writeln('        Entering a CR by itself will terminate the SetSearch command');   writeln;
   end;  {* of NOARGS case *}
   
 POPLAST: begin
   writeln;
   writeln('        If the last item is popped from the search list, some critical');
   writeln('        system utilities such as the shell will not be available to you.');
   writeln;
   writeln('        If you answer no to the following question, the last item on');
   writeln('        the search list will not be popped as requested, and setsearch');
   writeln('        will continue with any remaining input.');
   writeln;
   writeln('        If you answer yes to the following question, the last item');
   writeln('        will be popped from the search list. Critical functions will');
   writeln('        not run. A reboot will be necessary.');
   writeln;
   end;  {* of POPLAST case *}
   
 FILLFIRST: begin
   writeln;
   writeln('        Some of the system utilites (such as the linker) fill the ');
   writeln('        first item of the search list temporarily.  If you fill the');
   writeln('        first item, these utilities will not run.');
   writeln;
   writeln('        If you answer no to the following question, execution of');
   writeln('        Setsearch will continue, but ',ins^.name,' will not be');
   writeln('        added as the first item of the search list.');
   writeln;
   writeln('        If you answer yes to the following question, ',ins^.name);
   writeln('        will be added as the first item of the search list, and');
   writeln('        SetSearch will continue with any remaining input.');
   writeln;
   end;  {* of FILLFIRST case *}
   
 NOSHELL: begin
   writeln;
   writeln('        The current search list does not include the directory');
   writeln('        which contains the shell program.  Without the Shell,');
   writeln('        you will not be able to run any utilities or user programs.');
   writeln;
   writeln('        If you leave SetSearch with the current search list, you');
   writeln('        will probably have to reboot.  Adding sys:boot> or ');
   writeln('        sys:"your boot partition name"> to the list will probably');
   writeln('        enable you to access the shell. ');
   writeln;
   writeln('        If you answer no to the following question, you will ');
   writeln('        continue in SetSearch and have a chance to add directories');
   writeln('        to your search list. If the Shell still can not be found');
   writeln('        when you try to leave SetSearch after adding directories');
   writeln('        you will get the warning message again and get an infinite');
   writeln('        number of chances to modify the search list and insert the');
   writeln('        directory which contains the shell. (Just keep answering');
   writeln('        NO when the warning message comes up).');
   writeln;
   writeln('        If you answer yes to the following question, you will leave');
   writeln('        SetSearch and will not be able to run the shell (and will');
   writeln('        probably have to reboot).');
   writeln;
   end;  {* of NOSHELL case *}
   
 end;  {* of case stmt *}
 end;  {* of procedure *}

Function HandleSwitches(switches:pSwitchRec; where:integer):boolean;
{---------------------------------------------------------------------------
{ Abstract: This procedure looks at the switches specified and if anything 
{           is there besides /help, then prints error msg and returns false.
{           Also handles the /help switch.
{ Parameters: switches- list of switches specified, where- calling loc.
{ SideEffects: /help info is printed.  switches return false.
{---------------------------------------------------------------------------}
   var ThisSwitch: CString;
       DoReset: boolean;
       
       
   begin
   HandleSwitches:=true;
   DoReset:= false;
   while switches<>nil do
      begin
      HandleSwitches:=true;
      thisSwitch := switches^.switch;
      
      {* check to make sure no args were specified to switch *}
     
      if switches^.arg<>'' then
         begin
         writeln('**Error: No arguments may be specified to switches');
         Handleswitches:= false;
         exit(HandleSwitches);
         end;
         
      switches:= switches^.next;
      ConvUpper(ThisSwitch);
      case UniqueCmdIndex(ThisSwitch,SwitchTable, NumSwitches) of
         HELP: begin
            PrintHelp(where);
            HandleSwitches:=false;
            end;
             
         NOCONFIRM: begin
            confirm:= false;
            end;
            
         CONF: begin
            confirm:= true;
            end;
            
         RESET: begin
            DoReset:= true;
            end;
            
         otherwise:
            begin
            writeln('**ERROR: ',ThisSwitch,' is not a valid Switch.');
            HandleSwitches:=false;
            end;  {* of otherwise clause *}
            
      end;  {* of case *}
      
   end;  {* of while *}
   
   { now that we've processed other switches - like help do reset }
   
   if DoReset then begin
       ResetList;
       if ((beginning) and (ins^.name='')) then begin
          PrintSearchList;
          exit(setsearch);
          end;
       end;
          
   end; {* of procedure *}
   
   

Function HandleIns(var ins: pArgRec;var Err:boolean): boolean;
{-------------------------------------------------------------------------
{ Abstract: This function returns false if the input list is empty, but if
{           not empty, checks to see that each member of the list is either
{           a "-", or a legal directory.  If any are not legal directories,
{           then a message is printed and HandleIns returns an error flag
{           in the Err parameter.
{ Parameters: ins-list of input arguments, Err, flag set to false if error
{             occurs.
{ SideEffects: Err set on directory error, returns true if all ok.
{--------------------------------------------------------------------------}
   var fid: FileId;
       dum: integer;
       FirstIns: pArgRec;
       n: integer;
       dir: string;
           
   begin
   FirstIns:=ins; {* remember the first input *}
                  {* ins must be a var param since we have to change *}
                  {* the names part into full pathnames for dir checking*}
                  {* However, that means that when we leave, ins will be *}
                  {* pointing to nil.  To fix this, save ins first.      *}
                  {* Restore its value before we leave                   *}
                  
   Err:=true;
   if ins^.name='' then
      begin
      HandleIns:= false;
      end
   else
      begin
      while(ins<>nil) do
         begin
         if ins^.name<>'-' then
            begin
             {* find out if filename has .dr extension *}
            n:= length(ins^.name); {* get length *}
            dir:= substr(ins^.name,n-2,3); {* get last 3 chars*}
            convupper(dir);
            if dir='.DR' then
               delete(ins^.name,n-2,3); {* get rid of .dr *}
            if ins^.name[length(ins^.name)] <> '>' then AppendChar(ins^.name,'>');
            FSRemoveDots(ins^.name);
            fid:= FSInternalLookUp(ins^.name,dum,dum);
            if fid=0 then
               begin
               writeln('**ERROR: ',ins^.name,' not found');
               Err:= false;
               end; {* of if fid=0 *}
            end; {* of name<>"-" *}
         ins:= ins^.next;
         end;  {* of while loop *}
      HandleIns:=true;
      end; {* of else clause *}
      
   ins:= FirstIns;     {* Ins points to first value again *}
   end;  {* of procedure *}
   


  


begin
   
   
   
   {*  Initializations *}
   FSAddToTitleLine(Concat(Concat('SetSearch ',VERSION),' type /Help for help'));
   err:='';
   SwitchTable[HELP]:='HELP';
   SwitchTable[CONF]:='CONFIRM';
   SwitchTable[NOCONFIRM]:= 'NOCONFIRM';
   SwitchTable[RESET]:= 'RESET';
   
   confirm:= true;
   beginning:= true;
   numSwitches:= 3;
   emptyargs:= true;
   single:= false;
   
   c:= NextID(s,isSwitch);
   if (c<>' ') and (c<>CCR) then 
      begin
      writeln('**ERROR: Illegal character(s) after Setsearch');
      exit(SetSearch);
      end;
   ok:= ParseCmdArgs(ins,outs,switches, err);
   if not ok then
      Error(err);
   CheckForOuts(outs);
   ok:=HandleSwitches(switches,START);
   if not ok then
      exit(SetSearch);
   haveArgs:= HandleIns(ins,ok);
   if not ok then exit(SetSearch);
   beginning:= false;
    
   
  
   if not haveArgs then PrintSearchList;

 repeat
  if (not haveArgs) and (emptyargs)  then
      Repeat
      Write('Name to push, "-" to pop, CR to exit: ');
      Readln(Line);
      ok:=ParseStringArgs(Line,ins,outs,switches,err);
      if not ok then Error(err);
      ok:= HandleSwitches(switches,NOARGS);
      if ok then
         if (outs^.name<>'') then {* > we got some output files *}
            begin
            ok:= false;
            writeln('**Error: No output files may be specified.');
            end
         else if ins^.name<>'' then   {* some directories specified *}
            begin
            some:= HandleIns(ins,ok);
            emptyargs:= false;
            if ins^.next=nil then single:=true
            else single:=false;
            end
         else ok:= true;
       until ok;
       
  
   if ins^.name = '-' then 
     begin
     i := FindItemToPop;
     if i = 0 then WriteLn('**ERROR: Search list is empty, cannot pop')
     else if i = SearchSizeList then
         begin
         done:= false;
         
         while (not done) and (confirm) do
            begin
            if (haveArgs) or (not single) then PrintSearchList;
            WriteLn('*WARNING: It is dangerous to pop the last item from the search list');
            ans:= GetConfirm(NullIdleProc(**),true,'Are you sure this is what you want to do?',2,switches);
            done:= HandleSwitches(switches,POPLAST);
            end; {* of while loop *}
            
         if ans = 1 then
            begin
            FSSysSearchList[i] := '';
            end;
         end {* of if i=SearchSizeList *}
     else begin
          FSSysSearchList[i] := '';
          end;
     end
   else if ins^.name <> '' then
        begin
             i := FindHoleForPush;
             if i = 0 then WriteLn('**ERROR: Sorry, no room on search list. **')
             else if i = 1 then
                  begin
                  done:= false;
                  
                  while (not done) and (confirm) do
                     begin
                     if (haveArgs) or (not single) then PrintSearchList;
                     WriteLn('*WARNING: It is dangerous to fill the first item of the search list');
                     ans:= GetConfirm(NullIdleProc(**),true,'Are you sure this is what you want to do?',2,switches);
                     done:=HandleSwitches(switches,FILLFIRST);
                     end; {*  of while loop *}
                     
                  if ans = 1 then
                     begin
                     FSSysSearchList[i] := ins^.name;
                     end;
                  end
             else begin
                  FSSysSearchList[i] := ins^.name;
                  end;
        end;

  if haveArgs then
     begin
     if ins^.next=nil then
        ins^.name:=''
     else ins:= ins^.next;
     end;
     
 if ins^.name='' then
   begin
   Tmp:= Concat('Shell.',StrVersion);
   Tmp:= Concat(Tmp,'.Run');
   fid := FSLookUp(tmp, dum, dum);
   if fid = 0 then
       begin
       done:=false;
      
       while (not done) and (confirm) do
          begin
          WriteLn('* WARNING: ',tmp,' cannot be found with the');
          WriteLn('             current path and search lists!!');
          ans:=GetConfirm(NullIdleProc(**),true,'Are you sure this is what you want to do?',2,switches);
          done:=HandleSwitches(switches,NOSHELL);
          end; {* of while loop *}
          
       if ans=2 then
        begin
        PrintSearchList;
        ins^.name := 'DON''T EXIT';
        end;  {* of if ans=2 *}
       haveArgs := false;
       end;
   end;

 
 if not haveargs then
    begin
    if ins^.next=nil then
       emptyargs:=true
    else
       ins:=ins^.next;
    end;
    
 if (not haveargs) and ((emptyargs) and (ins^.name<>'')) then PrintSearchList;
 
 until ins^.name='';

 if haveArgs then PrintSearchList;

 end.
   
