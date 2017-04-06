{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module Helper;
{-------------------------------------------------------------------      
{     Helper - Help processor.
{     WJHansen       Jan 82. 
{     Copyright (C) Three Rivers Computer Corporation, 1982, 1983.
{
{  Abstract:
{     Reads an index file and presents options for assistance to the user.     
{-------------------------------------------------------------------}      

{-------------------------------------------------------------------      
{  Change Log:
{
{      FUTURE:         add spelling correction
{
{  10-Feb-83 Brad Myers    V1.4 Fixed for landscape monitor.
{
{  16 Nov 82 Bill Braucher V1.3 Fixed names for 14-character compiler.
{
{  17 May 82 WJH V1.2  Add brief, visible explanation.
{
{  27 Jan 82 BAM V1.1  Use PopCmdParse.  Fix RasterOp so doesn't erase lines.
{                      fix HelpKey so it never rolls top of screen off
{
{  27 Jan 82 WJH V1.0w change prompt if there are no index entries
{
{  24 Jan 82 WJH V1.0x modify to take help from sub files (not 1 file).
{                      handle HelpKey
{
{  23 Jan 82 WJH V1.0y Handle '$' flag which allows sub-indices of Help
{
{  23 Jan 82 WJH V1.0z Adapt from the Help facility formerly in editor.
{                      (This version has some mentions of help files,
{                      but still uses the unitary help file of the editor)
{-------------------------------------------------------------------}      



exports 
 
    imports FileDefs from FileDefs;        {for PathName}
    
    procedure GiveHelp(FName:PathName);



private
 
    imports Perq_String from Perq_String;
    imports Screen from Screen;            
    imports Raster from Raster;            {for RXor}
    imports Stream from Stream;            {for ResetError}
    imports System from System;            {for HelpKey}
    imports PopCmdParse from PopCmdParse;
    imports IO_Others from IO_Others;      {for IOKeyClear}
    imports IO_Unit from IO_Unit;          {for CtrlSPending}

const
    
    Version = 'V1.4';
   
    CR       = Chr(#015);    { carriage return }
    FF       = Chr(#014);    { form feed }
    Bel      = Chr(#007);    { bell }

      { screen characteristics }
      
    LineHeight = 16;
      
    XMargin = 5;
    YMargin = 5;


{$R-}

  

procedure GiveHelp(FName:PathName);
{----------------------------------------------------------------------
{
{ Abstract:
{     Reads a help index and displays it.  Lets user ask for information
{     on topics in the index and displays the files containing those topics.
{ Parameters:
{     FName - Name of the file containing the index.  The path to this
{             file is used as the path to the individual help files.
{-----------------------------------------------------------------------}  
{    
    An index file may have flagged entries: words preceded by % or $.
    The word must be followed by space or end of line.
    A percent flagged entry is the name of an ordinary help file, which
    will be displayed in the bottom of the screen.
    A dollar flagged entry is the name of an index file; GiveHelp will
    be called recursively on this file.
    
    The constant MaxEntry defines the maximum number of help entries in the
    file.
    
    Only the handle (first few characters) of an entry must be typed.
    
    In the text of a help entry, preceding a character with an up-arrow
    indicates that the following character is to be interpreted as a
    control character.  Thus "^J" will cause a literal ^J to be typed with
    the upper bit set.  This allows printing of the beginning-of-text marker
    which would be interpreted as an end-of-line if typed as a naked ^J.
    The "^" may be inserted in the file by typing "^^".
    
  
  }
  
  const MaxEntry = 50;
        Margin = '  ';
        Error  = '** ';
        Prompt = 'HELPER';

    
  var 
      Help: Text;
      Prefix: PathName;
      Entry: pNameDesc;
      IsIndex: array[1..MaxEntry] of Boolean;
      CurrentString: String;
      FoundIndex: integer;
      nEntry, nMatch, StartX, StartY,x,y: Integer;
      firstPress, leave: Boolean;
      inline, cmd: CString;
      inF: pCmdList;
      

   procedure Header;
{----------------------------------------------------------
{ Abstract:
{   Process the index file
{----------------------------------------------------------}
   var
      Flags: set of char;
      Flag: char;
      Done: Boolean;
   
   begin { Header }
    Reset(Help, FName);
    AllocNameDesc(MaxEntry, 0, Entry);
    Entry^.Header := 'Help on:';
    Write(FF);
    Flags := ['$', '%', '^', FF];
    nEntry := 0;
    while (Help^ <> FF) and not eof(Help) do begin
       while not (Help^ in Flags) do begin
          if eoln(Help) then begin
             writeln(Help^);
             if eof(Help) then 
                 Help^:= FF
             else
                 readln(Help);
          end
          else begin
             write(Help^);
             get(Help);
          end;
       end;
       if Help^ = '^' then begin
          Get(Help);
          if Help^ = '^' then Write('^')
          else Write( Chr( LOr( LAnd( Ord(Help^), #37), #200)))
       end
       else if (Help^ = '%') or (Help^ = '$') then begin 
          Flag := Help^;
          write(' ');
          Get(Help);    { skip flag }
          CurrentString := '';
          Done := False;
          repeat 
             AppendChar(CurrentString, Help^);
             if eoln(Help) then begin
                writeln(Help^);
                Done := True;
                if eof(Help) then 
                    Help^:= FF
                else
                    readln(Help);
             end
             else begin
                write(Help^);
                get(Help);
             end;
          until (Help^=' ') or Done;
          if nEntry = MaxEntry then begin
              Writeln;
              Writeln(Error, 'Too many entries in index ', FName);
              Writeln(Error, 'Next entry would be: ', CurrentString);
          end
          else begin 
              nEntry := nEntry + 1;
              IsIndex[nEntry] := (Flag='$');
              RemDelimiters(CurrentString, ' ',inline);
              if Length(CurrentString)>0 then
                 Entry^.commands[nEntry] := CurrentString
              else nEntry := nEntry-1;
          end;
       end;
    end {while not FF};
    SReadCursor(StartX,StartY);
    Line(DrawLine, XMargin, StartY, SBitWidth-XMargin, StartY, SScreenP);
    SSetCursor(StartX, StartY+LineHeight);
    writeln 
('            Press HELP,  type a category name,  or press RETURN to exit.');
    Line(DrawLine, XMargin, StartY+LineHeight, 
                   SBitWidth-XMargin, StartY+LineHeight, SScreenP);
    StartY := StartY + 2*LineHeight;
    SSetCursor(StartX, StartY+LineHeight);
    Entry^.numCommands := nEntry+1;
    Entry^.commands[nEntry+1] := '<Exit>';
    close(Help);
   end { Header };
   
   

 Procedure DoFF;
{----------------------------------------------------------
{ Abstract:
{   Clear screen after waiting for ^Q
{----------------------------------------------------------}
             begin
             Writeln;
             Write('  ** ^Q for more **');
             SCurOn;
             CtrlSPending := true;
             while CtrlSPending do;
             SCurOff;
             IOKeyClear;
             RasterOp(RXor,SBitWidth-2*xMargin,SBitHeight-StartY-yMargin,
                      xmargin,StartY,SScreenW,SScreenP,
                      xmargin,StartY,SScreenW,SScreenP);
             SSetCursor(StartX,StartY+LineHeight);
             end;
             

   procedure Print;
{----------------------------------------------------------
{ Abstract:
{   Display the help user has requested
{----------------------------------------------------------}
 var Found: Boolean;
     SubFile: PathName;

 handler ResetError(F:PathName);
       begin
       Writeln(Error, 'Couldn''t find the entry for "',
                        Entry^.commands[foundIndex], '",');
       Writeln(Error, 'Missing file: ', SubFile);   
       exit(Print);
       end;
 handler HelpKey(var S:Sys9s);
            begin
            s := '';
            end;
   
 begin { Print }
    if IsIndex[FoundIndex] then begin
       SubFile := concat(Prefix, concat(Entry^.commands[FoundIndex], concat ('>',
                                 concat(Entry^.commands[FoundIndex], '.Index'))));
       GiveHelp(SubFile);
       Header;
    end
    else begin
       SubFile := concat(Prefix, concat(Entry^.commands[FoundIndex], '.Help'));
       Reset(Help, SubFile);
       RasterOp(RXor,SBitWidth-2*xMargin,SBitHeight-StartY-yMargin,
                      xmargin,StartY,SScreenW,SScreenP,
                      xmargin,StartY,SScreenW,SScreenP);
       SSetCursor(StartX,StartY+LineHeight);
       while not eof(Help) do
          begin 
          Write(Margin);
          while not Eoln(Help) do begin
             if Help^ = '^' then begin 
                Get(Help);
                if Help^ = '^' then Write('^')
                else Write( Chr( LOr( LAnd( Ord(Help^), #37), #200)))
             end
             else if Help^=FF then DoFF
             else Write(Help^);
             Get(Help)
          end;
          Get(Help);
          Writeln;
          SReadCursor(X,Y);
          if Y + 3 * LineHeight > SBitHeight - YMargin then DoFF; 
          end;
       close(Help);
    end;
 end { Print };
   
   

  handler ResetError(FileName:PathName);  begin
  {---------------------------
  { Abstract:
  {    If the help file is not found, this handler will be invoked
  {    to give the user a message and exit from HElper.
  {---------------------------}
    writeln(Bel, '** Help file not found under name: ', FName);
    writeln('** Press RETURN to continue.');
    readln;
    exit(GiveHelp);
  end;
  
  label 1;
  handler HelpKey(var S:Sys9s); begin
     writeln;
     writeln('      Type one of the key words indicated above and press RETURN.');
     WriteLn('      Press a button on the pen/puck for a PopUp menu of key words.');
     WriteLn('      Press in the PopUp menu for help on item selected.');
     WriteLn('      Press in scroll area at bottom and move left or right to scroll menu.');
     writeln('      Just type the RETURN key to exit the HELPER.');
     
     StreamKeyBoardReset(inF^.cmdFile);

     goto 1;
  end;
 


  begin { GiveHelp }
     Prefix := Substr(FName, 1, RevPosC(FName, '>'));
     Header;
     firstPress := true;
     InitCmdFile(inF,0);
  1:
     repeat
      leave := false;
      Writeln;
      SReadCursor(X,Y);
      if Y + 3 * LineHeight > SBitHeight - YMargin - 50 then DoFF; 
      foundIndex := GetCmdLine(NullIdleProc, prompt, inline, cmd, inF,
                               entry, firstPress, true);

      if foundIndex = nEntry+1 then leave := true
      else if foundIndex = nEntry+2 then WriteLn('** "',cmd,'" not found.')
      else if foundIndex = nEntry+3 then WriteLn('** "',cmd,'" is not unique.')
      else if foundIndex = nEntry+4 then leave := true
      else if foundIndex = nEntry+5 then WriteLn('** No switches valid here.')
      else if foundIndex = nEntry+6 then StdError(ErIllCharAfter,cmd,false)
      else Print;
     until leave;
  entry^.numCommands := maxEntry;
  DestroyNameDesc(entry);
  DstryCmdFiles(inF);
  end { GiveHelp }.
