{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module InitShell;
{--------------------------------------------------------------------
{
{ Abstract:
{    This module initializes the command and popup window tables used
{    by the shell.  It should only be used once per login since the
{    tables are kept around in memory.
{
{ Written by: Brad A. Myers and W.J.Hansen
{
{ Copyright (C) Three Rivers Computer Corporation,  1981, 1982, 1983
{
{---------------------------------------------------------------------}


{--------------------------------------------------------------------
{ Change Log:
{
{ 16 Nov 82  V2.3   Bill Braucher
{     Fixed names for 14-character compiler.

{ 21 Jan 82  V2.2   Brad Myers
{     Don't set UseCmd.
{     Don't reverse order of commands from Profile.
{     Import ShellDefs instead of Shell.
{     Set debug to false.
{ }

{  6 Jan 82  V2.1   Brad Myers
{     Use standard removeQuotes routine.
{     changed default names loaded if no profile.
{     allow accepting !
{     UtilProgress busy for when reading profile.
{ }

{  6 Dec 81  V2.0   WJH
{     take command list from profile (replaced almost entire module)
{
{  3 Dec 81  V1.1   WJH
{     Use name Scavenge in CmdTable (so popup shrinks screen)
{     Use PString instead of String
{     Use Ctrl instead of several variables in Shell
{     utilize CmdHelp for Help only;  use CmdDesc for searching
{ }

{ 13-Aug-81  V1.0   BAM
{  Created by breaking off from Shell
{ }
{--------------------------------------------------------------------}


{\\\\\\\\\\\\\\\\\\\\\\\\\\} EXPORTS {////////////////////////////}
 
const
    InitShlVersion = 'V2.3';
    
Procedure InitCmds;
Procedure DumpCmds;

    
{\\\\\\\\\\\\\\\\\\\\\\\\\\} PRIVATE {////////////////////////////}
                  
Imports CmdParse    from CmdParse;
Imports Profile     from Profile;
Imports ShellDefs   from ShellDefs;
Imports Dynamic     from Dynamic;
Imports Memory      from Memory;
Imports System      from System;
Imports Stream      from Stream;
Imports IO_Others   from IO_Others;
Imports PERQ_String from PERQ_String;
Imports UtilProgress from UtilProgress;


const  Debug = False;

   
type
    pCmdRec = ^CmdRec;
    
    CmdRec = record
        CmdName: S25;               {Command name}
        CmdString: string;          {string to execute}
        AddDefault: Boolean;        {True if add default file name}
        SetDefault: Boolean;        {True if should set default file }
        ScreenSize: integer;        {ScreenSize}
        CmdIndex: integer;          {Index for commands implemented by shell }
        NextCmd: pCmdRec;           {pt to next on list}
        end;

var 
    Ctrl:    pCtrlRec;              {ptr to Shell control block}
    CmdList: pCmdRec;               {pt to first command on list}
    TProFile: String;               {profile file in use}
    OtherIndex: integer;            {index of OTHER in Shell case statement}


Procedure DumpCmds;
{-----------------------------------------------------
{ Abstract:
{   Displays the contents of the Ctrl data structure,
{   especially the tables of commands.
{-----------------------------------------------------}
var
   i, j:integer;
   S: PString;
   Ctrl: pCtrlRec;
begin
{$R-}
Ctrl := recast(ShellCtrl, pCtrlRec);
with Ctrl^ do begin
    writeln('Number of Commands is ', NumCmds:1, '    (table entries are ',
                 wordsize(CmdRecord):1, ' words)');
    writeln;
    writeln('Command         Implementation       Add   Set   SSz Inx');
    writeln;
    for i := 1 to Ctrl^.NumCmds do begin
        S := Ctrl^.CmdDesc^.commands[i];
        j := PosC(S, ' ');
        if (j=0) or (j>length(S)) then j := length(S);
        if j<=15 then write(S:15, ' ')
        else write(S:j, ' ');
        with CmdTable^[i] do begin
           S := RunString;
           j := length(S);
           while S[j]=' ' do j := j-1;
           if j<=0 then j := length(S);
           if j<20 then write(S:20,' ')
           else write(S:j,' ');
           
           writeln(AddDefault:6, ' ', SetDefault:6, ' ',
                   ScreenSize:1, '   ', Index:1);
           end;
        end;
    end;
{$R=}
end; {DumpCmds}

   

procedure ProcessCommandEntry(Entry: PString);
{-----------------------------------------------------
{ Abstract: 
{   Processes a line of the profile and makes a command entry.
{   A line must have the form:
{
{   string to  adjust   setdefault   screensize   commandname   helpinfo
{   execute
{              Bool     Bool         integer      string[25]    string
{
{ Parameters:
{   Entry - a string read from profile file
{
{ Side Effects:
{   Appends an entry at the front of CmdList
{-----------------------------------------------------}
var
   TEntry: PString;
   T: pCmdRec;
   loc, i: integer;
   Cmd: String;
   IgnoreS: string;
   
   exception BadEntry; {raised when profile entry is bad}
   handler BadEntry; 
      var
         i: integer;
      begin 
      writeln;
      writeln ('** Bad entry in #ShellCommands section of ', TProFile);
      writeln ('** ''', Entry, '''');
      write ('** @');
      if loc > 1 then write(' ':loc-1);
      for i := loc to length(Entry)-length(TEntry) do
         write('X');
      writeln;
      exit (ProcessCommandEntry);
      end;
   

   function GetString(var S: PString): string;
   var   IsSw: Boolean;
         D:  PString;
         Term: char;
   begin
   loc := length(Entry) - Length(S) + 1;
   Term := NextIdString(S, D, IsSw);
   if not RemoveQuotes(D) then Raise BadEntry;
   if IsSw or (D='') then raise BadEntry;
   if length(D) > 80 then raise BadEntry;
   GetString := D;
   ShowProgress(1);
   end;
   
   function IntVal(S:string): integer; 
   var I, loc: integer;
   begin
   I := 0;
   loc := 1;
   while loc <= length(S) do 
      if s[loc] <> ' ' then begin
         if ('0'>S[loc]) or ('9'<S[loc]) then raise BadEntry;
         I := 10*I + ord(S[loc]) - ord('0');
         loc := loc+1;
         end;
   IntVal := I;
   end;
   
   Function GetBool(S: String): boolean;
     begin
     if (s[1] = 't') or (s[1] = 'T') then GetBool := true
     else if (s[1] = 'f') or (s[1]  = 'F') then GetBool := false
     else Raise BadEntry;
     end;
  
begin {ProcessCommandEntry}
new(T);
TEntry := Entry;
with T^ do
   begin
   ShowProgress(1);
   RemDelimiters(TEntry, ' ', IgnoreS);
   if TEntry='' then exit(ProcessCommandEntry);
   CmdString   := GetString(TEntry);
   i := Pos (CmdNames, CmdString) - 1;
   CmdIndex := OtherIndex;
   if i mod 10 = 0 then begin
      i := i div 10;
      if (i>=0) and (i<=LastIdx) then CmdIndex := i;
      end;
   
   AddDefault := GetBool(GetString(TEntry));
   SetDefault := GetBool(GetString(TEntry));

   ScreenSize := IntVal(GetString(TEntry));
   if ScreenSize > 8 then begin
       if ScreenSize mod 128 <> 0 then
           raise BadEntry;
       ScreenSize := ScreenSize div 128;
       end; 
   if (ScreenSize<1) or (ScreenSize>8) then 
       raise BadEntry;
   
   Cmd := GetString(TEntry);
   if (length(Cmd) > 25) then raise BadEntry;
   CmdName := Cmd;
   ConvUpper(CmdName);
   end;
        
Ctrl^.NumCmds := Ctrl^.NumCmds + 1;
T^.NextCmd := CmdList;
CmdList := T;
end;  {ProcessCommandEntry}

   

Procedure InitCmds;
{-----------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is called to initialize the command table.
{
{ Side Effects:
{    This procedure will change Ctrl and the records it points at.
{
{---------------------------------------------------------------------------}
label 2, 3;

Handler PNotFound(filename: String);
   {---------------------------------------------------------------------------
     Abstract:
           Raised if profile not found; try for '>Default.Profile'
           if that fails, use internal default.
    --------------------------------------------------------------------------}
    
        Handler PNotFound(FileName: string);
          var T: pCmdRec;
              i,j: integer;
          begin
          writeln ('** No #ShellCommands found at all, using defaults.');
          new(CmdList);
          T := CmdList;
          for i := IdxQuest to LastIdx do
             with T^ do
                begin
                ShowProgress(1);
                CmdName := SubStr(CmdNames, i*10+1, 10); {skip first one}
                j := PosC(CmdName, ' ');
                if j <> 0 then CmdName := SubStr(CmdName, 1, j-1);
                CmdString := ' ';
                AddDefault := false;
                SetDefault := false;
                ScreenSize := 8;
                CmdIndex :=  i;
                if i = LastIdx then NextCmd := NIL
                else begin
                     new(T^.NextCmd);
                     T := T^.NextCmd;
                     end;
                end;
          Ctrl^.NumCmds := LastIdx;  {SET THIS VALUE if change commands above}
          goto 2;
          end;  {inner PNotFound}
  
  begin  {outer PNotFound}
      if FileName = '>Default.Profile' then Raise PNotFound(FileName);
      writeln;
      writeln('** Your profile file ''', FileName, ''' cannot be found,');
      writeln('** or has no entry for ''#ShellCommands''.');
      writeln('** Now trying to use ''>Default.Profile''.');
      TProFile := '>Default.Profile';
      PFileInit(TProFile, 'ShellCommands');
      goto 3; {found a profile, now parse as if nothing happened}
      end;
  
var
  i: integer;
  TCmd: pCmdRec;
  CmdLine: PString;
  LocCmdTable: MMPointer;


begin {InitCmds}

if CmdSegment<>0 then 
    DecRefCount(CmdSegment);                {release old command segment}
if StreamSegment=0 then
    CreateSegment(StreamSegment,1,1,256);   {be sure there is a stream seg}
CmdSegment := StreamSegment;
IncRefCount(CmdSegment);                    {use current stream seg}
LoadBusy;

new (CmdSegment, 1, Ctrl);
ShellCtrl := RECAST(Ctrl, pointer);
Ctrl^.NumCmds := 0;

OtherIndex := (Pos(CmdNames, 'OTHER') - 1) div 10;

{ Look to see if there is a profile file.  If so, see if there are any
{ shell commands.}

    TProFile := CurPFile;
    PFileInit(TProFile, 'ShellCommands');
3:  CmdLine := PFileEntry;
    ShowProgress(1);
    while CmdLine <> '' do begin
        ShowProgress(1);
        ProcessCommandEntry(CmdLine);
        CmdLine := PFileEntry;
        end;
    
    if Ctrl^.NumCmds < 1 then Raise PNotFound (TProFile); {try again}
        
        
{  Initialize CmdDesc table used to find commands  }

2:  NewP (CmdSegment, 1, LocCmdTable,  
                          Ctrl^.NumCmds*wordsize(CmdRecord));
    Ctrl^.CmdTable := recast(LocCmdTable.P, pCmdsArray);
    AllocNameDesc(Ctrl^.NumCmds, CmdSegment, Ctrl^.CmdDesc);
    Ctrl^.CmdDesc^.Header := 'Pick a command:';
     
    TCmd := CmdList;     
    for i := Ctrl^.NumCmds downto 1 do begin
{$R-}
        Ctrl^.CmdDesc^.commands[i] := TCmd^.CmdName;
        with Ctrl^.CmdTable^[i] do begin
{$R=}
            RunString  := TCmd^.CmdString;
            AddDefault := TCmd^.AddDefault;
            SetDefault := TCmd^.SetDefault;
            ScreenSize := TCmd^.ScreenSize;
            Index      := TCmd^.CmdIndex;
            end;
        TCmd := TCmd^.NextCmd;
        end;
    if Debug then DumpCmds;
    
    InitCmdFile(Ctrl^.CmdFileList, CmdSegment);
    InCmdFile := False;
    Ctrl^.FirstPress := True;         
    
    IOSetFunction(recast(DefCursFunct, CursFunction));
    QuitProgress;

end {initcmds}.
