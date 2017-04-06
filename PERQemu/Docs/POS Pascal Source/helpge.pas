{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Program HelpGen;
{----------------------------------------------------
{    WJHansen
{    Copyright 1982, 1983  Three Rivers Computer Corp.
{
{ Abstract:
{    Processes a X.Paras file of help paragraphs to
{    produce one Z.Help file for each page.  
{    Also produces X.Index
{----------------------------------------------------}

{----------------------------------------------------
{ Change Log:
{   
{   16 Nov 82 V0.1 Bill Braucher Fixed names for 14-character compiler.
{   25 Jan 82 V0.0 WJHansen      Create program.
{----------------------------------------------------}

{ Documentation:
{ 
{ The command line to invoke HelpGen is
{    
{       HelpGen  input[,input]...  [output]  [/Extension=xxx]
{       
{ The input file[s] are processed to generate files in the pathname given
{ as the "output".  The input extension defaults to ".Paras".  If no
{ output is specified, the same path as that to the input is used.
{ 
{ The output files have the extension ".xxx", which defaults to .Help
{ If no output pathname is given, the same path as the input is used.
{ 
{ 
{ Everything in the input up to the first FormFeed (^L) is placed in the
{ index file with the same name as the input and the extension ".Index".
{ In addition, if the line following the title line of a section contains
{ "%" or "$", that line is placed at the end of the .Index file rather
{ than in the OutFile.
{ 
{ 
{ The input file is an initial index section followed by a sequence of
{ text sections.  Each text section begins with a FormFeed (^L) and the
{ same line continues with the name of the file that the text is to go in.
{ 
{ Example input:
{ 
{      |<-- column 1
{      This is the index portion 
{          %Text1 explains text.
{          $Text2 is a directive to go to another directory
{      Text1
{      This will be in the file Text1.Help
{      Text2>Text2.Index
{      This is another index
{         %Text25 is in the other index
{         and this is too
{      Text3
{      %Text3 is another item in index
{      This is contents of Text3.Help
{      Text2>Text25
{      This is the explanation of Text25
{   
{ Corresponding output is these files:
{  
{   Text1.Index:
{      This is the index portion
{          %Text1 explains text.
{          $Text2 is a directive to go to another directory
{      %Text3 is another item in index
{      
{   Text1.Help:
{      This will be in the file Text1.Help
{      
{   Text2>Text2.Index
{      This is another index
{         %Text25 is in the other index
{         and this is too
{         
{   Text3.Help
{      This is contents of Text3.Help
{   
{   Text2>Text25.Help
{      This is the explanation of Text25
{ }     

imports CmdParse    from CmdParse;
imports Perq_String from Perq_String;

const
   CtlL = chr(#014);            {FormFeed}
   
var
   IndexFile,                   {File for writing index}
   InFile,                      {current input file}
   OutFile: text;               {current output file}
   Prefix: String;              {directory for output files}
   Line: PString;               {text line from InFile}
   Extension: string;           {extension for output files}
   SW: pSwitchRec;              {list of switches from command line}
   Ins, Outs: pArgRec;          {lists of files from command line}
   

procedure GetArgs;
{----------------
{ Abstract:
{    Reads command line and parses.
{    Sets up Extension and ensures there is at least one input file.
{-----------------------}
var 
   Err: String;
   L: integer;
   IgnoreC: char;
   IgnoreS: CString;
   IgnoreB: Boolean;
   
begin
   IgnoreC := NextId(IgnoreS, IgnoreB);   {remove command name}
   if not ParseCmdArgs(Ins, Outs, SW, Err) then begin
      writeln(Err);
      exit(HelpGen);
   end;
   
   { process extension }
   
   Extension := '.Help';
   if SW <> NIL then 
      if SW^.Switch[1] in ['E', 'e'] then begin
         Extension := SW^.arg;
         if Extension[1]<>'.' then
            Extension := concat('.', Extension);
      end;
      
   { ensure having at least one input filename }

   if Ins^.Name = '' then begin
      write('Input file: ');
      readln(Ins^.Name);
   end;
   
   { process output directory Prefix }

   if Outs^.Name <> '' then
      Prefix := Outs^.Name
   else begin
      L := RevPosC(Ins^.Name, '>');
      Prefix := substr(Ins^.Name, 1, L);
   end;
   if length(Prefix) > 0 then
      if Prefix[length(Prefix)]<>'>' then
         AppendChar(Prefix, '>');
end;

procedure PutBody(var F: text);
{-----------------------------
{ Abstract: 
{    Reads lines and puts them in file F.  
{    Exits with EOF(InFile) or Line[1]=CtlL.
{ Parameter:
{    F - File to write lines to
{ Environment:
{    Expects the initial line to be in Line on entry.
{-------------------------------}
var
   Putting: Boolean;
begin
   Putting := True;
   while Putting do begin
      if length(Line)=0 then
         writeln(F)
      else if Line[1]=CtlL then 
         Putting := False
      else writeln(F, Line);
      if Putting then
         if eof(InFile) then
             Putting := False
         else
             readln(InFile, Line);
   end;
end;

procedure DoSection;
{-------------------
{ Abstract:
{   Writes a section to OutFile, after determining the name of the file.
{   Processes possible index entry on second line.
{ Environment:
{   The caller must be sure that Line contains a line beginning with
{   CtlL and continuing with a file name.
{-------------------}
var
   L: integer;    {character location in line}
begin
   AppendChar(Line, ' ');
   L := PosC(Line, ' ');
   Line := Substr(Line, 2, L-2);
   L := PosC(Line, '.');
   if L=0 then
      Line := concat(Line, Extension);
   Line := concat(Prefix, Line);
   rewrite(OutFile, Line);
   writeln(Line);
   readln(InFile, Line);
   L := PosC(Line, '%');
   if L=0 then 
      L := PosC(Line, '$');
   if L<>0 then begin
      writeln(IndexFile, Line);
      readln(InFile, Line);
   end;
   PutBody(OutFile);
   close(OutFile);
end;

{   m a i n   p r o g r a m   }

var
   IndexName: String;
   L: integer;

begin
   GetArgs;
   IndexName := Ins^.Name;
   L := PosC(IndexName, '.');
   if L<>0 then
      IndexName := substr(IndexName, 1, L-1);
   IndexName := concat(Prefix, concat(IndexName, '.Index'));
   rewrite(IndexFile, IndexName);
   writeln(IndexName);
   while Ins<>NIL do begin   {process each input file}
      if PosC(Ins^.Name,'.')=0 then 
         Ins^.Name := concat (Ins^.Name, '.Paras');
      reset(InFile, Ins^.Name);
      readln(InFile, Line);
      putbody(IndexFile);
      while not eof(InFile) do 
         DoSection;
      Ins := Ins^.Next;
   end;
   Close(IndexFile);
end.
