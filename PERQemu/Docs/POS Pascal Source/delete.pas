{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
program FileDelete;

{----------------------------------------------------------------------------
{
{ Abstract:
{    This program is used to delete files from the file system.
{
{ Copyright (C) 1981, 1982, 1983 - Three Rivers Computer Corporation.
{
{--------------------------------------------------------------------------}



Const Version = 'V2.7';

{ 16-Nov-82  V2.7  Bill Braucher
{ Fixed names for 14-character compiler.
{}

{ 20-May-82  V2.6  Michael R. Kristofic
{ Fix help message and abort deletion of files when invalid command line
{}

{  1-May-82  V2.5  Michael R. Kristofic
{ Check for too many files for array in wild card case
{}

{  1-feb-82  V2.4  Ellen Colwell
{ When deleting a boot file, gets device name from path name of file, instead
{ of from device that was booted.  Also was writing boot area of disk even
{ when couldn't find corresponding boot in DIB boot table.  Doesn't do that
{ any more.
{}

{ 26-jan-82  V2.3  Ellen Colwell
{ Make delete keep prompting for info when no input given(used to be an error).
{ Got rid of "ERROR" and "WARNING" from msgs.
{ Delete works when directory entry points to nothing (bad file ).
{ Delete shows wait cursor while sorting file for display in menu.
{ Delete users RemoveQuotes to take care of double quote cases.
{}

{ 31-dec-81  V2.2  Ellen Colwell
{ Implemented standard user interface for D.6 release. Applied quicksort
{ function to menu displays.  Added routines to check for the deletion of
{ a non-empty directory or current directory (both illegal). Also added
{ routine to check for deletion of boot and mboot files. Asks for confirmation
{ for boot and mboot files and if affirmative then deletes both entries in
{ the Disk Information Block Boot Tables.
{ }


{ 25-Aug-81  V2.1  Dirk Kalp
{ New command syntax to provide a filelist specification and standard switch 
{ placements; fixes bug problems related to extra command line args and  
{ improper handling of other command line forms.
{
{ Changes to: global declarations
{             program body
{             procedure DoSwitch
{
{
{      The DELETE command is provided in the two basic forms.  In Form A, the 
{   user is prompted for the filelist whereas, in Form B, he enters it on the  
{   same line with the command name.  Switches may be supplied both before the 
{   filelist to set the global default mode and after each filelist element to 
{   selectively set the mode for that element.  
{      In Form A, PopUp windows may be seleced with a Pen Press.  In the event
{   windows are not chosen here or in Form B of the command, each wildcard spec
{   to which a switch has not been applied will result in a prompt to the user 
{   who must respond by confirming or by selecting a PopUp window. The response
{   may optionally be preceded by new switch selection for this element.  The
{   Pen Press always results in PopUp windows for wildcards unless the NoConfirm
{   switch has been applied to that wildcard filespec.  All wildcards (even 
{   those displayed with a menu) and non-wildcard filespecs always obey their 
{   most recently applied switch setting before they will be deleted.
{      Overall, the command syntax consists of a command name followed by the 
{   global switch setting and the filelist.  These three components may be 
{   separated by commas or blanks.  A filelist consists of a series of elements
{   where each is a filespec followed by a switch spec.  Each element is 
{   separated by commas or blanks.  A comma never appears within a filelist 
{   element or a global switch spec while blanks may be used freely.  A more 
{   precise definition of the syntax is provided below.
{
{
{}(*   :{ }Delete{ |,}[global sw][filelist]<CR>
{   
{      null filelist => Form A => prompt to user who can then enter:
{                                 { |,}[global sw][filelist]Action
{      where Action ::= <CR> | Pen Press
{   
{      filelist ::= {< |,>{ |,}filespec[filespec sw]}
{   
{      filespec sw ::= switch spec
{   
{      global sw ::= switch spec
{   
{      switch spec ::= {{ }{/{ }[switch]{ }}/{ }switch{ }}
{   
{      switch ::= Confirm | NoConfirm | Help
{   
{      filespec ::= as defined in PERQ User Manual and may contain wildcards
{*)
{ }

{ 17-Aug-81  V2.0  Brad Myers
{ PopUp window available for file names!! 
{ }

{ 21-May-81  V1.9  Brad Myers
{ Catch signals from new OS
{ Add ** to error messages
{ Prompt if file name not specified
{ }

{ 14-Apr-81  V1.8  Brad Myers
{ Shortened title line message
{ }

{ 31-Mar-81  V1.7  Brad Myers
{ Notes when illegal directory
{ }

{ 28-Mar-81  V1.6  Brad Myers
{ Use FSAddToTitleLine; use LocalLookup on name to delete
{ }

{ 24-Mar-81  V1.5  Brad Myers
{ Fixed bug in deleting files in other directories; full name of deleted file
{ }

{ 19-Mar-81  V1.4  Brad Myers
{ Changed for new FS; Perq_String; PathName. Changed title to note help; import
{  fileutils; added GetDirName procedure to allow deleting files in directories
{  Search => PMatch
{ }

{  9-Mar-81  V1.3  Don Scelza
{ Fixed a bug in verify mode.
{ }

{  3-Mar-81  V1.2  Don Scelza
{ Cleaned up user interface.  Added switch processing.
{ }

{ ??-??-81   V1.0  CMU Spice Group
{ Program created.
{ }



imports PopCmdParse from PopCmdParse;
imports System      from System;
imports CmdParse    from CmdParse;
imports FileUtils   from FileUtils;
imports FileDir     from FileDir;
imports AllocDisk   from AllocDisk;
imports PMatch      from PMatch;
imports Perq_String from Perq_String;
imports PopUp       from PopUp;
imports IO_Others   from IO_Others;
imports Stream      from Stream;
imports Raster      from Raster;
imports QuickSort   from QuickSort;
imports DiskIO      from DiskIO;

Const MaxFiles = 600;

Type DelNames = Record
             header: s25;
             numCommands: integer;
             names: Array[1..MaxFiles] of s25;
             End;
    pDelNames = ^DelNames;

  
  var Answer: integer;
  var StringPtr : pStrArray;

  
var
  scanptr : ptrScanRecord;
  pattern, tmpPattern : PathName;
  Ignore, ans  : string;
  id      : integer;
  I       : integer;
  Dum     : integer;
  
  alpha: string;
  Broke   : string;
  Confirm, GotConfirm, Press, InMiddle: boolean;
  GlobalVerify, GlobalgotVerify, GlobalPress : boolean;
  Wild, FirstWild, floppy : boolean;
  tmpUsrCmdLine : String[255];
  Switch  : String;
  Names   : pDelNames;
  res     : ResRes;
  wait    : CurPatPtr;
  Seg     : SegId;
  
Label  999;

var ins,outs , ignoreins, ignoreouts: pArgRec;
    FileData: ptrFSDataEntry;    {* global pointer for FSGetFSData *}
    NumSwitches, NumCmds : integer;
    Switches:   pSwitchRec;
    SwitchTable: CmdArray;
    Console, DelFile: pCmdList;
    ComandName: string;
    Const DelCursFile = 'Delete.Cursor';
    const START = 1;
          BOOTFILE = 2;
          CANTDEL = 3;
          CONFIRMDEL = 4;
          FORMAHELP = 5;
        Var s: cstring;
        c: char;
        err: string;
        IsSwitch, ok, FormA, NoneSpecified, Stuck: boolean;
    

Procedure PrintHelp(where:integer);
{----------------------------------------------------------------------------
{ Abstract: Prints help messages depending on "where" parameter.
{ Parameters: where- tells us which message to print
{ SideEffects: prints help message
{----------------------------------------------------------------------------}
begin
case where of
 START:begin
      WriteLn;
      writeln('        Delete is used to remove files from the file system.');
      writeln('        A file specification given may contain wild cards.');
      WriteLn('        All files that match the source will be deleted.');
      writeln;
      writeln('        For a description of the wild cards; see Directory/Help');
      WriteLn;
      writeln('           Command line is of the form:');
      writeln('             Delete {/Switch} <filelist>');
      writeln;
      writeln('           where filelist is:');
      writeln('             filename, filename, filename,  ...');
      writeln;
      writeln('           If no files are specified, ');
      writeln('             you will be prompted for a filelist ');
      writeln;
      writeln('            The valid switches are:');
      for i := 1 to NumSwitches do writeln('            ', SwitchTable[i]);
      exit(FileDelete);
      end;  {* of START case *}
      
 BOOTFILE: begin
      writeln;
      writeln('        You are about to delete a valid boot file.  The boot ');
      writeln('        that uses this file will not work if you delete this ');
      writeln('        file.  Additionally, when the boot file is deleted,  ');
      writeln('        the entry in the system boot file corresponding to   ');
      writeln('        this boot will be deleted.');
      writeln;
      writeln('        If you answer no to the following question, this file');
      writeln('        will not be deleted.');
      writeln;
      writeln('        If you answer yes to the following question, this file');
      writeln('        will be deleted');
      writeln;
      end;  {* of BOOTFILE case *}
      
 CANTDEL: begin
      writeln;
      writeln('        Could not delete the file even though it could be found');
      writeln('        on the file system.  If you can repeat this situation, ');
      writeln('        please fill out a software report form. ');
      writeln;
      end; {* of CANTDEL case *}
      
 CONFIRMDEL: begin
      writeln;
      writeln('        Answer yes if you want this file to be deleted.  Answer');
      writeln('        no or CR if you do not want this file delted.');
      writeln;
      end; {* of CONFIRMDEL case *}
      
 FORMAHELP: begin
      WriteLn;
      writeln('        Delete is used to remove files from the file system.');
      writeln('        A file specification given may contain wild cards.');
      WriteLn('        All files that match the source will be deleted.');
      writeln;
      writeln('        For a description of the wild cards; see Directory/Help');
      WriteLn;
      writeln('           Command line is of the form:');
      writeln('             Delete {/Switch} <filelist>');
      writeln;
      writeln('           where filelist is:');
      writeln('             filename, filename, filename,  ...');
      writeln;
      writeln('           If no files are specified, ');
      writeln('             you will be prompted for a filelist ');
      writeln;
      writeln('            The valid switches are:');
      for i := 1 to NumSwitches do writeln('            ', SwitchTable[i]);
      writeln;
      writeln('        If you press a mouse button, all of the files in the ');
      writeln('        current directory, or the current wild card selection');
      writeln('        will be displayed in a PopUp menu on the screen.  You');
      writeln('        can select the files you want to delete by pointing and');
      writeln('        pressing at the file names you wish to select for');
      writeln('        deletion.  Pressing a second time will "unselect"');
      writeln('        the file.');
      writeln;
      writeln('        If the file you wish to delete is not in the current ');
      writeln('        directory, or if you would rather type the names of');
      writeln('        the files (specifying wild card characters if you wish)');
      writeln('        you may list files names to be deleted. (Separate them');
      writeln('        with commas.)');
      writeln;
      end;  {* of FORMAHELP case *}
     
end; {* of case stmt *}
      
end;   {* of PrintHelp procedure ;*}
       




Procedure CursOn(on, show: boolean);
{---------------------------------------------------------------
{ Abstract:
{    Turn the cursor on or off
{-------------------------------------------------------------------}
  begin
  if on then begin  {* cursor will follow puck and be on *}
             IOLoadCursor(DefaultCursor,0,0);
             IOCursorMode(trackCursor);
             IOSetModeTablet(reltablet);
             end
  else begin  {* don't follow cursor *}
       IOSetModeTablet(offtablet);
       if show then IOLoadCursor(wait,0,0) {* if show then display wait curs *}
       else IOCursorMode(offCursor);
       end;           
  end;


procedure GetDirName(var FileName: PathName);
{---------------------------------------------------------------
{ Abstract:
{    This procedure is used to get the directory information from
{    the search pattern.
{ SideEffects: Initializes scanPtr; changes filename to remove dir
{-------------------------------------------------------------------}
var I, dum: Integer;
    fid: FileID;
  begin
    new(scanptr);
    scanptr^.InitialCall := true;
    scanptr^.DirName := '';
  
    FSRemoveDots(filename);
    i := RevPosC(fileName, '>');
    if i <> 0 then
            begin
            scanptr^.DirName := SubStr(fileName, 1, i);
            fileName := SubStr(fileName, i+1, length(fileName)-i);
            fid := FSLocalLookUp(scanPtr^.DirName, dum, dum);
            if fid = 0 then
               begin
               WriteLn('** Directory ',scanPtr^.dirName,' not found.');
               exit(FileDelete);
               end;
            end;
  end; {GetDirName}



procedure DoOneDelete(fileName: PathName);forward;


Procedure DoDirCheck(name: pathname);
{-------------------------------------------------------------------
{ Abstract: Checks to see if the current directory being deleted is
{           empty.  If it is not, then DoOneDelete is aborted.
{ Parameters: name is name of directory to delete
{ SideEffects: If the directory is not empty, this delete is aborted.
{-------------------------------------------------------------------}
   var AFileThere: boolean;
       FileFound:SimpleName;
       id: FileID;
       scanptr: ptrScanRecord;
       n: integer;
       dir: string;
       
       begin
       new(scanptr);
       scanptr^.InitialCall:= true;
       scanptr^.Dirname:= name;
       
       AFileThere:= FSScan(scanptr,FileFound,id);
       
       if AFileThere then      {* directory is not empty *}
          begin
          writeln('* ',name,' This directory is not empty - Can Not Delete');
          exit(DoOneDelete);
          end;
       
       n:= length(name);
       dir:= substr(name,n-2,3);
       convupper(dir);
       if dir='.DR' then
          begin
          delete(name,n-2,3);
          appendchar(name,'>');
          end;
           
       if name=FSDirPrefix then
          begin
          writeln('* Attempt to delete current directory.');
          exit(DoOneDelete);
          end;
          
       end;  {* of DoDirCheck procedure*}

procedure IdleProc;
begin
end;

function HandleSwitchOuts(var switch:pSwitchRec; outs:pArgRec; ok:boolean; err:string; where:integer):boolean;
{---------------------------------------------------------------
 Abstract:  Applies switches from UsrCmdLine.
 SideEffects: Sets Confirm switch; leaves broke as first non-space
               character after switch
-------------------------------------------------------------------}
      var i: integer;
      ThisSwitch: Cstring;
      begin
      HandleSwitchOuts:= false;
      if not ok then
         begin
         write('**  ');
         writeln(err);
         writeln('  type Delete /Help for help');
         exit(FileDelete);
         end;
      while switch <> nil do
         begin
         ThisSwitch:= switch^.switch;
         if switch^.arg<>'' then
            begin
            writeln('** There are no valid arguments to any of the valid switches.');
            HandleSwitchOuts:=true;
            end;
         switch:= switch^.next;
         ConvUpper(ThisSwitch);
         case UniqueCmdIndex(ThisSwitch,SwitchTable, NumSwitches) of

          1: begin
             Confirm := true;
             GotConfirm:= true;
             end;

          2: begin
             Confirm := false;
             GotConfirm:= true;
             end;

          3: begin
             PrintHelp(where);
             HandleSwitchOuts:= true;
             end;


  otherwise: begin
             writeln('** ',ThisSwitch, ' is not a valid switch');
             if where=START then exit(FileDelete);
             HandleSwitchOuts:= true;
             end;  {* of otherwise clause *}
             
             end;  {* of case *}
         end;   {* of while *}
      
      
      {* check to see if any output files specified *}
      if (outs^.name <> '') and (not InMiddle) then 
        StdError(ErNoOutFile,'Delete',true);
        {$ifc false then}
         begin
         writeln('** Arguments to delete must be separated by commas');
         HandleSwitchOuts:=true;  {* flag error *}
         end; {* of check for output files *}
        {$endc}

      end;  {* of procedure *}

Procedure BootDel(filetype:integer; filename:Pathname; filenumber:FileID);
{---------------------------------------------------------------------
{ Abstract: Deletes the entries in the System Boot Table for the boot
{           file which is about to be deleted.
{ Parameters: filetype- 15=BootFile, 16=MBootFile, and filename of the boot file
{             filenumber:FileID
{ SideEffects: zeroes out the entries in the BootTable which point to this
{              boot file.
{---------------------------------------------------------------------}
   var Answer:integer;
       Cheat: DiskCheatType;
       found: boolean;
       InfoBlock: FSBit32 ;
       InfoHeader: ptrHeader ;
       ptr: ptrDiskBuffer;
       SegmentID: SegID;
       stuck: boolean;
       tmpfilename: pathname;
       whatdisk:  integer;
       whatpart:  integer;
       i,j: integer;
       
       
   begin
   new(0,256, ptr);
   new(0,4, InfoHeader);
   
   tmpfilename:= filename;
   stuck:= GetDisk(tmpfilename,whatpart);{* get partition name from file name*}
   
   if not stuck then
      begin
      writeln('** Bad file name: ',filename);
      exit(DoOneDelete);
      end;
      
   whatdisk:= PartTable[whatpart].PartDevice;  {* get device of file *}
   
   
   Cheat.Dbl[0]:= 0;
   if whatdisk=1 then
         Cheat.Dbl[1]:= FLOPBITS
   else
      Cheat.Dbl[1]:= DISKBITS;
      
   InfoBlock:= Cheat.Addr;
   
   InfoHeader^.SerialNum:= InfoBlock;
   InfoHeader^.LogBlock:= 0;
   
   DiskIO(InfoBlock,Ptr,InfoHeader,DSKREAD);
   
   SegmentID:=LogAddrToPhysAddr(FileIDtoSegID(filenumber));
   i:=0;
   Found:= false;
   
   Case filetype of
   
      15: begin
          while (not found) and (i<26) do
             begin
             if ptr^.BootTable[i] = SegmentID then
                found:= true
             else i:= i+1;
             end
           end; {* of 15 case *}
          
      16: begin
          while (not found) and (i<26) do
             begin
             if ptr^.InterpTable[i] = SegmentID then
                found:= true
             else i:= i+1;
             end
           end; {* of 16 case *}
          
   end; {* of case *}
   
   if found then
      begin
      writeln('* ',filename,' is a valid Boot File corresponding');
      writeln('           to letter ',alpha[i+1],'.');
      repeat
        Answer:= GetConfirm(NullIdleProc(**),true,'Are you SURE you want to delete this? ',2,switches);
        stuck:= HandleswitchOuts(switches,outs,true,'',BOOTFILE);
      Until not stuck;
      if (answer=2) then
         begin
         writeln('* Aborting delete of ',filename);
         exit(DoOneDelete);
         end
      else if(answer=3) then
         writeln('* No switches now please');
         
      
      ptr^.BootTable[i]:= DBLZERO;
      ptr^.InterpTable[i]:= DBLZERO;
      DiskIO(InfoBlock,Ptr,InfoHeader,DSKWRITE);
      end;
   
      
   end;  {* of BootDel procedure *}
   


Procedure DoOneDelete;
{---------------------------------------------------------------
{ Abstract: Deletes one file.
{ Parameters: fileName is name of file to delete; it cannot have *'s in it
{ SideEffects: Deletes file
{-------------------------------------------------------------------}
   var answer: integer;
   var ans: String;
   var StopLooking: boolean;
   var dummy: integer;
   var id: FileID;
       stuck: boolean;
       
   Handler DelError(name: PathName);
     {---------------------------------------------------------------
     { Abstract: Prints message when get error; should never happen
     {-------------------------------------------------------------------}
      var s: string;
     begin
     s:= concat('** Unable to delete ',name);
     s:= concat(s,'.  Continue? ');
     repeat
       answer:= getconfirm(NullIdleProc(**),true,s,2,switches);
       stuck:= HandleSwitchOuts(switches,outs,true,'',CANTDEL);
     until not stuck;
     if answer=1 then exit(DoOneDelete)
     else exit(FileDelete);
     end;
   
    begin
    StopLooking:= true;
    
    
    id:= FSInternalLookup(filename,dummy,dummy);  {* get FileID of file *}
    
    if id<>0 then
      begin
      FSGetFSData(id,FileData);                     {* use FileID to get File Data *}
     
      if FileData^.FileType = 3 then DoDirCheck(filename);  {* check for empty dir *} 
      end; {* of id = 0 case *}
      
      {* In case you're wondering why we did this id=0 thing....
      {* Sometimes a file will exist in the directory but will
      {* be pointing to nothing. In this case when we try to read the -1 block
      {* of the file, we'll get a FileNotFound exception raised. To avoid this
      {* we don't try to read the -1 block of a file, but still delete the
      {* name from the directory. *}
 if Confirm then 
     begin
     repeat
       answer:= GetConfirm(NullIdleProc(**),true,concat(concat(' ',filename),' '),2,switches);
       stuck:= HandleSwitchOuts(switches,outs,true,'',CONFIRMDEL);
     until not stuck;
     if answer=2 then exit(DoOneDelete);
     end;
        
  if (id<>0) then
     if (FileData^.FileType=15) or (FileData^.FileType=16) then
        BootDel(FileData^.FileType,filename,id);    {* if boot file then delete corresponding boot entries in the DIB (disk information block) *}
     
    if (wild and not confirm) or (not confirm) then writeln(' ',filename);
    FSDelete(fileName);
    end;  {* of procedure *}




Function EmptyInput(ins:pArgRec):boolean;
{-------------------------------------------------------------------
{ Abstract:
{    This Routine does nothing more than check to see if any input
{    files were entered as part of a command string check by of of the
{    standard user interface parsing routines (B.Myers:1981).
{ Returns: TRUE if list of input files is empty, False otherwise.
{ SideEffects: none
{--------------------------------------------------------------------}

   begin
   If ins^.name = '' then
      EmptyInput:= true
   else
      EmptyInput:= false;
   end;  {*of EmptyInput function*}
   
                

Function ReadPressString(var s: PathName): boolean;
{---------------------------------------------------------------
 Abstract: Reads a string but stops if press and not floppy.
 Parameters: s is set with the string read up until a CR or a press
 Returns: true if press and not floppy else false
 SideEffects: characters of s are printed and a CR is typed afterwards
-------------------------------------------------------------------}
  var i : integer;
  begin
  repeat
  until FullLn(input) or (tabSwitch and not floppy);
  if tabSwitch and not floppy then
     begin
     WriteLn;
     ReadPressString := true;
     if keylength = 0 then s := '*'
     else begin
          Adjust(s, KeyLength);
          for i := 1 to KeyLength do
             s[i] := KeyBuffer[i-1];
          end;
     StreamKeyBoardReset(input);
     end
  else begin
       ReadLn(s);
       ReadPressString := false;
       end;
  end;
  

Procedure ReadCursor;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Abstract: reads in the wait cursor if found.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  var fid: fileID;
      blks,i: integer;
  begin
  New(0,4,wait);
  RasterOp(RXor, 64, 64, 0,0,4,wait, 0,0,4,wait);
  fid := FSLookUp(DelCursFile, blks, i);
  if (fid <> 0) and (blks = 1) then FSBlkRead(fid, 0, RECAST(wait, pDirBlk));
  end;

Handler Outside;
{- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Abstract: When press outside of menu, then abort delete.
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -}
  begin
  CursOn(false, false);
  WriteLn('* Aborting delete of ', ins^.name);
  goto 999;
  end;


begin {main body}

  floppy := SysBootChar < Ord('a');
  
  Confirm := false;   { the default }
  GotConfirm := false;
  press := false;
  firstwild := true;
  InMiddle:=false;
  s:='';
  err:='';
  new(FileData);    {* allocate space before call to FSGetFSData *}
      
  FSAddToTitleLine(Concat(Concat('Delete ', Version),'  Type /Help for help'));

  SwitchTable[1] := 'CONFIRM      ask before deleting a file.';
  SwitchTable[2] := 'NOCONFIRM    don''t ask before deleting files.';
  SwitchTable[3] := 'HELP        print this message.';
  NumSwitches := 3;
  alpha:='abcdefghijklmnopqrstuvwxyz';
  
  c:= Nextid(s, isSwitch);       {* remove Delete *}
  if (c<>' ') and (c <> CCR) then
   begin
   writeln('** Illegal character after Delete');
   exit(FileDelete);
   end;
  ok:= ParseCmdArgs(ins,outs,switches,err);
  stuck:= HandleSwitchOuts(switches,outs,ok,err,START);
  FormA := EmptyInput(ins);   {* see if any files were specified *}
  
  if not floppy then ReadCursor;
   
  { If we have no files specified, then Form A else Form B }
  
  if FormA then      {Form A}
     repeat
     write('Filelist to delete');
     if not floppy then 
        begin
        CursOn(true,true);
        write(' and/or press for Menu');
        end;
     write(': ');
     press := ReadPressString(s);
     if press then CursOn(false,true)
     else if not floppy then CursOn(false,false);
     ok:= ParseStringArgs(s,ins,outs,switches,err);
     stuck:= HandleSwitchOuts(switches,outs,ok,err,FORMAHELP); {* could have picked up more *}
     if ins^.name='' then stuck:= true;
     until not stuck;
   
   { Now Forms A and B are handled the same. }
   {* all of the files specified are in the "ins" list *}
   {* all switches input have been handled (they're all global) *}
   
   
        
   while ins <> nil do   { Process the filelist. }
      begin

      Wild := IsPattern(ins^.name);
      
      if Wild and (not Press) and (not GotConfirm) and (not floppy) then
         confirm:= true;
      
      if not GotConfirm then Confirm := Wild and not Press;
                                           

      if not Wild then   {* process one file on the list *}
         begin
         FSRemoveDots(ins^.name);
         if not RemoveQuotes(ins^.name) then
           StdError(ErAnyError,ins^.name,true);
         ID := FSInternalLookUp(ins^.name, Dum, Dum);
         if ID = 0 then
           begin
           seg:= DeleteFileId(ins^.name); {* might be a bad file *}
           writeln('**',ins^.name, ' not found.')
           end {* of id=0 *}
         else DoOneDelete(ins^.name);
         end
      else               {* we're processing a wild card file *}
         begin
        { if not floppy then CursOn(false, press);}
         if firstwild then     {* start a names list *}
            begin
            New(names);
            firstwild := false;
            end;
         tmpPattern := ins^.name;
         GetDirName(ins^.name);
         names^.numCommands := 1;       {* now look through the directory *}
         while FSScan(scanptr,names^.names[names^.numCommands],id) do
             if PattMatch(names^.names[names^.numCommands],ins^.name,true) then
                 if names^.numCommands < MaxFiles then
                     names^.numCommands := names^.numCommands+1
                 else StdError(ErAnyError,
                               '** Too many files match this pattern.',
                               true);

         if names^.numCommands = 1 then 
            WriteLn('* No files found matching ', tmpPattern)
         else 
            begin
            
            {* Well....  We have a pointer to the Names record       *}
            {* which contains the array we want... but not a pointer *}
            {* to the array itself.  Since StringSort requires a     *}
            {* pointer to the array, we'll have to build the pointer.*}
            {* Bye the way... You'll notice that we have to subtract *}
            {* one from the count of the files within the call to    *}
            {* StringSort.  This is because the Names record that's  *}
            {* being built for the call to Menu begins its array     *}
            {* starting at 1.  StringSort begins it's array starting *}
            {* at 0.  Therefore, when StringSort expects to see N,   *}
            {* and its array is numbered from 0..N, and our array is *}
            {* numbered from 1..N, we've got to hand it an N which is*}
            {* one less that what we've computed.                    *}
            
            loadadr(StringPtr); {* Make pointer to names array *}
            loadadr(Names^.Names);
            inlinebyte(183);    {* STDW  *}

            names^.numCommands := names^.numCommands-1;  {added once too often for call to Menu}
            StringSort(names^.numCommands-1,StringPtr,true); {* put in lex order *}
            if press then begin    {* go get files from menu *}
                          CursOn(true, true);
                          InitPopUp;
                          names^.header := 'Delete:';
                          Menu(RECAST(names, pNameDesc), true, 1,
                                names^.numCommands, -1, -1, 500, res);
                          CursOn(false, false);
                        {$R-}
                          for i := 1 to res^.numIndices do
                              DoOneDelete(Concat(scanPtr^.dirName,
                                          names^.names[res^.indices[i]]));
                          end
                        {$R=}
            else   {* there is no menu displayed *}
               begin
                  for i := 1 to names^.numCommands do  {* delete each file that matched *}
                   DoOneDelete(Concat(scanPtr^.dirName, names^.names[i]));
               end; {* of else clause *}
            end;

         end;

999:     { Exception OUTSIDE returns here to continue with the filelist. }
         {* you get an OUTSIDE exception when the cursor is moved to    *}
         {* outside of the menu and press.  This causes an abort of the *}
         {* current delete    *}
         

      ins := ins^.next;    {* get next file specified *}
         
      end{while};

      if not floppy then CursOn(false, false);

end{**Delete**}.
