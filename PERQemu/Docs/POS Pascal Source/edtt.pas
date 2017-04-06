{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module EditorTerminate;

{-------------------------------
{ Abstract:
{    Provides options for termination of an edit session.
{
{   copyright (C) 1983 Three Rivers Computer Corporation
{
{------------------------------}

{-------------------------------
{ Change Log:
     Comment out the Dispose because it gives run time error.

   15 Feb 83  V2.9  WJHansen
     Fix so does not bomb for long file names or pathnames.
     Add Change Log.
     Use ten blocks for output file to speed up writing file.
     Use MVBW in Send to speed it up.
     Handle ReNDir.
{------------------------------}

exports procedure Terminate;
 

private
 

 imports Editor          from Editor;
 imports EditorUtilities from EditorU;
 imports FileUtils       from FileUtils;
 imports FileSystem      from FileSystem;
 imports AllocDisk       from AllocDisk;
 imports Perq_String     from Perq_String;
 imports CmdParse        from CmdParse;
 imports Screen          from Screen;
 imports UtilProgress    from UtilProgress;
 
 const
   {$include Perq.QCodes.Dfs}


 
 
 {-- finish up --}
 
 
 procedure Terminate;
 var Chars: set of char;
     Message: String[200];
     WFileName: String;
     Success: Boolean;
 
 
  procedure WriteFile( FileName: String );
  var Buffer: pSwapBuffer;
      OutBuf: array [1..10] of pSwapBuffer;  {Buffer will be one of these}
      CurBuf: 1..10;                          {Buffer == OutBuf[CurBuf]}
      Block: Integer;
      Filled: Integer;
      FileNumber: Integer;
      NChars: Integer;
      Ch: Char;
      Ignore: Integer;
      BackupName: String;
      MaxBlock: integer;
   
   Handler PartFull (PName: string);
   begin
     writeln(Bel, ' * * * * ', Bel, '* * * * ', Bel, '* * * * ', Bel);
     Message := concat('** Not enough room in partition ', PName);
     FSClose(FileNumber,Block,512*8);
     exit(WriteFile);
   end;
   
   Handler ReNDir(FName: PathName);
   begin
     Message := concat('** Cannot rename directory: ', FName);
     exit(WriteFile);
   end;
   
   procedure ExtendName(var Name: string);
    {checks that entire name is less than 80 chars 
     and the actual file name is less than 25}
   begin
      if length(Name)>=80 then begin
           Message := 'Pathname for backup file would exceed 80 characters';
           exit(WriteFile);
      end;
      if length(Name)-RevPosC(Name,'>')>=25 then begin
           Message := 
             'Name of backup file (after last ">") would exceed 25 characters';
           exit(WriteFile);
      end;
      AppendChar(Name, '$');
   end;
   
   procedure Send( B: pSwapBuffer; Start, Length: Integer );
   const CxxVxV = 6*16+3;
   var I, N: Integer;
   begin { Send }
    if Filled + Length > 512 then
     begin N := 512 - Filled;
      (* for I := 0 to N-1 do  *)
       (* Buffer^[Filled+I] := B^[Start+I]; *)
      if N>0 then begin
           {To:}   LoadAdr(Buffer^);   LoadExpr(Filled);
           {From:} LoadAdr(B^);        LoadExpr(Start);
           LoadExpr(N);
           InLineByte(STLATE);  InLineByte(CxxVxV);  InLineByte(MVBW);
      end;
      if CurBuf<10 then 
          CurBuf := CurBuf+1
      else begin
          for CurBuf := 1 to 10 do begin
            FSBlkWrite(FileNumber,Block,Recast(OutBuf[CurBuf],pDirBlk));
            Block := Block + 1;
            ComputeProgress(Block, MaxBlock);
          end;
          CurBuf := 1;
      end;
      Buffer := OutBuf[CurBuf];
      
      Start := Start + N;
      Length := Length - N;
      Filled := 0
     end;
    (* for I := 0 to Length-1 do  *)
     (* Buffer^[Filled+I] := B^[Start+I];  *)
    if Length>0 then begin
           {To:}   LoadAdr(Buffer^);   LoadExpr(Filled);
           {From:} LoadAdr(B^);        LoadExpr(Start);
           LoadExpr(Length);
           InLineByte(STLATE);  InLineByte(CxxVxV);  InLineByte(MVBW);
    end;
    Filled := Filled + Length
   end { Send };

  var
    fid: FileId;
    IgnoreI: integer; 
    Max: integer;
    i: integer;
   
  begin { WriteFile }
   Writeln;
   Success := False;
   FileNumber := FSLocalLookUp(FileName,Ignore,Ignore);
   if FileNumber <> 0 then
    begin
     Writeln(' ', FileName, ' already exists');
     BackUpName := FileName;
     ExtendName(BackUpName);
     fid := FSLocalLookUp(BackUpName, IgnoreI, IgnoreI);
     if fid <> 0 then begin
        if fid = IdFile[EditFile] then begin
            ExtendName(BackUpName);
            fid := FSLocalLookUp(BackUpName, IgnoreI, IgnoreI);
        end;
        if fid <> 0 then FSDelete(BackUpName);
     end;
     FSRename(FileName, BackupName);
     Writeln(' ', FileName, ' renamed to ', BackupName)
    end;
   FileNumber := FSEnter(FileName);
   if FileNumber = 0 then
    begin Message := Concat('can''t write to ', FileName);
     Exit(WriteFile)
    end;
   Write(' writing ',FileName);
   Attach(Cursor1,FilledFirst,ReadCursor);
   Add1C(Cursor1);
   Add1C(Cursor1);
   Block := 0;
   Filled := 0;

   for CurBuf := 1 to 10 do
      new(0,256,OutBuf[CurBuf]);
   CurBuf := 1;
   Buffer := OutBuf[CurBuf];

   LoadCurs;
   Max := FilledLast.Chunk^.OrderP;
   MaxBlock := Max;
   while not EOT(Cursor1.Pos) do
    begin NChars := Cursor1.Pos.Chunk^.Length - Cursor1.Pos.Offset;
     Send( Txt[Cursor1.ChPage].Buffer,
           Cursor1.Pos.Offset + Cursor1.Pos.Chunk^.First,
           NChars );
     AddC(Cursor1,NChars);
    end;
   Message := Concat(FileName, ' written');
   QuitProgress;
   if CurBuf > 1 then 
       for i := 1 to CurBuf-1 do begin
         FSBlkWrite(FileNumber,Block,Recast(OutBuf[i],pDirBlk));
         Block := Block + 1
       end;
   if Filled <> 0 then
    begin
     FSBlkWrite(FileNumber,Block,Recast(OutBuf[CurBuf],pDirBlk));
     Block := Block + 1
    end
   else Filled := 512;
   FSClose(FileNumber,Block,Filled*8);
   for CurBuf := 1 to 10 do 
      dispose(OutBuf[CurBuf]);
   Writeln;
   Detach(Cursor1);
   Success := True
  end { WriteFile };

 
  
 begin { Terminate }
  OffPointer;
  Message := '';
  Editing := false;
  CreateWindow(0,0,0,16*SScreenW,1024, ' ' );
  FSAddToTitleLine (EdVersion);
  repeat Success := True;
   repeat Write(FF);
    Writeln; writeln; writeln;
    if Message <> '' then
     begin Writeln(' ', Message);
      Writeln
     end;
    if FileChanged then 
        writeln('     CHANGES have been made.')
    else 
        writeln('     NO changes have been made.');
    writeln;
    if FileName <> '' then
      if FileChanged then begin
          Chars := ['U', 'W', 'E', 'R'];
          Writeln('     U  to update ', FileName);
          Writeln('     W  to write to another file');
      end
      else begin
          Chars := ['W', 'E', 'R'];
          Writeln('     File name is ', FileName);
          Writeln;
          Writeln('     W  to write to another file');
      end
    else
     begin Chars := ['W', 'E', 'R'];
      Writeln('     W  to write to a file')
     end;
    Writeln('     E  to exit without updating');
    Writeln('     R  to return to the editor');
    Writeln;
    Write('     Editor', CmdChar, ' ');
{$IFC ReplayVersion THEN}
    if DemoSwitch then begin
       write ('E');
       CH := 'E';
    end
    else
{$ENDC}
    begin
      if Eoln then Ch := ' '
      else Read(Ch);
      Readln;
    end;
    if Ch in ['a'..'z'] then Ch := Chr(Ord(Ch) - Ord('a') + Ord('A'))
   until Ch in Chars;
   Message := '';
   if Ch = 'R' then
    begin RefreshScreen;
     OnPointer;
     Editing := true
    end
   else
    begin
     if Ch = 'U' then WriteFile(FileName)
     else
      if Ch = 'W' then
       begin Writeln;
        Write(' enter file name: ');
        Readln(WFileName);
        if not RemoveQuotes(WFileName) then {allow final quote};
        FSRemoveDots(WFileName);
        WriteFile(WFileName)
       end;
     Writeln
    end
  until Success;
  if Message <> '' then
   begin Writeln(' ', Message);
    Writeln
   end;
  {*** Close(SwapFile,Purge) ***}
 end { Terminate }.

