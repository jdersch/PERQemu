{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module RunRead;
{-----------------------------------------------------------------------------
{
{       RunRead - Module to read run files.
{       John P Strait   9 Apr 81.
{       CopyRight (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       RunRead exports procedures to read and write run files.
{
{ Design:
{       If and when the format of run files is changed, the constant
{       RFileFormat in module Code must be changed.  This is necessary
{       so that the procedures to read run files will not crap out.
{
{-----------------------------------------------------------------------------}


{ 16 Nov 82  V1.2  Bill Braucher
{ Fixed names for 14-character compiler.
{ }

{ 21 Jul 81  V1.1  John Strait
{ Add Hints.
{ }

{  9 Apr 81  V1.0  John Strait
{ Start file.
{ }

exports


const RunReadVersion = '1.2';


imports Code from Code;



procedure ReadRunFile( var RunFile: RunFileType; Seg: Integer;
                       var Header: RunInfo;
                       var FirstSeg, FirstUserSeg, LastSeg: pSegNode;
                       ImportsWanted: Boolean );

procedure ReadSegNames( var RunFile: RunFileType; Seg: Integer;
                        FirstUserSeg: pSegNode );


private


imports Perq_String from Perq_String;
imports Dynamic from Dynamic;

procedure ReadRunFile( var RunFile: RunFileType; Seg: Integer;
                       var Header: RunInfo;
                       var FirstSeg, FirstUserSeg, LastSeg: pSegNode;
                       ImportsWanted: Boolean );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReadRunFile reads a run file and builds a structure that represents
{       that run file.  The run file is read up to, but not including, the
{       names of the .Seg files.
{
{ Parameters:
{       RunFile  - A file variable which has been Reset to the desired file.
{                  ReadRunFile does *not* close the file.
{       Seg      - Segment number for dynamic allocation.
{       Header   - The RunInfo record.
{       FirstSeg - Set to point to the first segment in the run file.
{       FirstUserSeg - Set to point to the first user segment in the run file.
{       LastSeg  - Set to point to the last segment in the run file.
{       ImportsWanted - True iff Import entries are to be read from the run
{                  file.
{
{-----------------------------------------------------------------------------}

var pSeg: pSegNode;
    pImp, LastImp: pImpNode;
    K: RunElement;
    SN: SNArray;

  procedure ReadSNArray( var S: SNArray );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReadSNArray writes a string to the run file.
{
{ Parameters:
{       S - SNArray to be written.
{
{-----------------------------------------------------------------------------}

  var I: Integer;
  begin { ReadSNArray }
    for I := 1 to SegLength do
      if I mod 2 = 0 then
        begin
          S[I] := Chr(Shift(RunFile^,-8));
          Get(RunFile)
        end
      else S[I] := Chr(LAnd(RunFile^,#377));
    if SegLength mod 2 <> 0 then Get(RunFile)
  end { ReadSNArray };

begin { ReadRunFile }
  FirstSeg := nil;
  FirstUserSeg := nil;
  LastSeg := nil;
  Header.RFileFormat := RunFile^;  Get(RunFile);
  if Header.RFileFormat <> RFileFormat then Exit(ReadRunFile);
  with Header do
    begin
      Version := RunFile^;      Get(RunFile);
      System := RunFile^ <> 0;  Get(RunFile);
      InitialGP := RunFile^;    Get(RunFile);
      CurOffset := RunFile^;    Get(RunFile);
      StackSize := RunFile^;    Get(RunFile);
      StackIncr := RunFile^;    Get(RunFile);
      HeapSize := RunFile^;     Get(RunFile);
      HeapIncr := RunFile^;     Get(RunFile);
      ProgramSN := RunFile^;    Get(RunFile);
      SegCount := RunFile^;     Get(RunFile)
    end;
  while RunFile^ <> Ord(SegFileNames) do
    begin
      K := Recast(RunFile^,RunElement);  Get(RunFile);
      if (K = SysSegment) or (K = UserSegment) then
        begin
          New(Seg,1,pSeg);
          if FirstSeg = nil then FirstSeg := pSeg
          else LastSeg^.Next := pSeg;
          if (K = UserSegment) and (FirstUserSeg = nil) then
            FirstUserSeg := pSeg;
          LastSeg := pSeg;
          with pSeg^ do
            begin
              ReadSNArray(SegId);
              RootNam := nil;
              if K = UserSegment then
                with Hint do
                  begin
                    Word1 := RunFile^; Get(RunFile);
                    Word2 := RunFile^; Get(RunFile);
                    Word3 := RunFile^; Get(RunFile)
                  end;
              GDBSize := RunFile^;   Get(RunFile);
              XSTSize := RunFile^;   Get(RunFile);
              GDBOff := RunFile^;    Get(RunFile);
              ISN := RunFile^;       Get(RunFile);
              CodeSize := RunFile^;  Get(RunFile);
              ImpList := nil;
              Next := nil
            end;
          if K = UserSegment then
            while RunFile^ = Ord(Import) do
              begin
                Get(RunFile);
                if ImportsWanted then
                  begin
                    New(pImp);
                    if pSeg^.ImpList = nil then pSeg^.ImpList := pImp
                    else LastImp^.Next := pImp;
                    LastImp := pImp;
                    with pImp^ do
                      begin
                        ReadSNArray(SId);
                        FilN := nil;
                        XGP := RunFile^;  Get(RunFile);
                        XSN := RunFile^;  Get(RunFile);
                        Next := nil
                      end
                  end
                else
                  begin
                    ReadSNArray(SN);
                    Get(RunFile);
                    Get(RunFile)
                  end
              end
        end
    end
end { ReadRunFile };

procedure ReadSegNames( var RunFile: RunFileType; Seg: Integer;
                        FirstUserSeg: pSegNode );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReadSegNames reads .Seg file names from a run file and adds them to a
{       structure that represents that run file.
{
{ Parameters:
{       RunFile  - A file variable which has been Reset to the desired file
{                  and already read with ReadRunFile.  ReadSegNames does *not*
{                  close the file.
{       Seg      - Segment number for dynamic allocation.
{       FirstUserSeg - A pointer to the first user segment in the run file.
{
{-----------------------------------------------------------------------------}
var pSeg: pSegNode;

  procedure ReadString( var S: pFNString );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReadString reads a string from the run file.
{
{ Parameters:
{       S - String to be read.
{
{-----------------------------------------------------------------------------}

  var I, L: Integer;
      P: MMPointer;
  begin { ReadString }
    L := RunFile^;  Get(RunFile);
    NewP(Seg,1,P,(L+2) div 2);
    S := Recast(P.P,pFNString);
    S^ := '';
    for I := 1 to L do
      if I mod 2 = 0 then
        begin
          AppendChar(S^, Chr(Shift(RunFile^,-8)));
          Get(RunFile)
        end
      else AppendChar(S^, Chr(LAnd(RunFile^,#377)));
    if L mod 2 <> 0 then Get(RunFile)
  end { ReadString };

begin { ReadSegNames }
  Get(RunFile);
  pSeg := FirstUserSeg;
  while pSeg <> nil do
    begin
      ReadString(pSeg^.RootNam);
      pSeg := pSeg^.Next
    end
end { ReadSegNames }.
