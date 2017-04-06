{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module RunWrite;
{-----------------------------------------------------------------------------
{
{       RunWrite - Module to write run files.
{       John P Strait   9 Apr 81.
{       CopyRight (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       RunWrite exports procedures to write run files.
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


const RunWriteVersion = '1.2';


imports Code from Code;



procedure WriteRunFile( var RunFile: RunFileType; Header: RunInfo;
                        FirstSeg, FirstUserSeg: pSegNode );

private



procedure WriteRunFile( var RunFile: RunFileType; Header: RunInfo;
                        FirstSeg, FirstUserSeg: pSegNode );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       ReadRunFile writes a run file from a structure that represents that
{       run file.
{
{ Parameters:
{       RunFile  - A file variable which has been Rewritten to the desired
{                  file.  WriteRunFile does *not* close the file.
{       Header   - The RunInfo record.
{       FirstSeg - A pointer to the first segment in the run file.
{       FirstUserSeg - A pointer to the first user segment in the run file.
{
{-----------------------------------------------------------------------------}

var pSeg: pSegNode;
    pImp: pImpNode;
    K: RunElement;

  procedure WriteString( S: pFNString );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       WriteString writes a string to the run file.
{
{ Parameters:
{       S - String to be written.
{
{-----------------------------------------------------------------------------}

  var I: Integer;
  begin { WriteString }
    RunFile^ := Length(S^);  Put(RunFile);
    I := 1;
    repeat
      if I mod 2 = 0 then
        begin RunFile^ := LOr(RunFile^,Shift(Ord(S^[I]),8));
          Put(RunFile)
        end
      else RunFile^ := Ord(S^[I]);
      I := I + 1
    until I > Length(S^);
    if Length(S^) mod 2 <> 0 then Put(RunFile)
  end { WriteString };

  procedure WriteSNArray( S: SNArray );
{-----------------------------------------------------------------------------
{
{ Abstract:
{       WriteSNArray writes a string to the run file.
{
{ Parameters:
{       S - SNArray to be written.
{
{-----------------------------------------------------------------------------}

  var I: Integer;
  begin { WriteSNArray }
    I := 1;
    repeat
      if I mod 2 = 0 then
        begin RunFile^ := LOr(RunFile^,Shift(Ord(S[I]),8));
          Put(RunFile)
        end
      else RunFile^ := Ord(S[I]);
      I := I + 1
    until I > SegLength;
    if SegLength mod 2 <> 0 then Put(RunFile)
  end { WriteSNArray };

begin { WriteRunFile }
  RunFile^ := RFileFormat;  Put(RunFile);
  with Header do
    begin
      RunFile^ := Version;      Put(RunFile);
      RunFile^ := Ord(System);  Put(RunFile);
      RunFile^ := InitialGP;    Put(RunFile);
      RunFile^ := CurOffset;    Put(RunFile);
      RunFile^ := StackSize;    Put(RunFile);
      RunFile^ := StackIncr;    Put(RunFile);
      RunFile^ := HeapSize;     Put(RunFile);
      RunFile^ := HeapIncr;     Put(RunFile);
      RunFile^ := ProgramSN;    Put(RunFile);
      RunFile^ := SegCount;     Put(RunFile)
    end;
  pSeg := FirstSeg;
  K := SysSegment;
  while pSeg <> nil do with pSeg^ do
    begin
      if pSeg = FirstUserSeg then K := UserSegment;
      RunFile^ := Ord(K);    Put(RunFile);
      WriteSNArray(SegId);
      if K = UserSegment then
        with Hint do
          begin
            RunFile^ := Word1; Put(RunFile);
            RunFile^ := Word2; Put(RunFile);
            RunFile^ := Word3; Put(RunFile)
          end;
      RunFile^ := GDBSize;   Put(RunFile);
      RunFile^ := XSTSize;   Put(RunFile);
      RunFile^ := GDBOff;    Put(RunFile);
      RunFile^ := ISN;       Put(RunFile);
      RunFile^ := CodeSize;  Put(RunFile);
      if K = UserSegment then
        begin
          pImp := ImpList;
          while pImp <> nil do with pImp^, Seg^ do
            begin
              RunFile^ := Ord(Import);  Put(RunFile);
              WriteSNArray(SId);
              RunFile^ := GDBOff;       Put(RunFile);    { XGP }
              RunFile^ := ISN;          Put(RunFile);    { XSN }
              pImp := pImp^.Next
            end
        end;
      pSeg := pSeg^.Next
    end;
  RunFile^ := Ord(SegFileNames);  Put(RunFile);
  pSeg := FirstUserSeg;
  while pSeg <> nil do
    begin
      WriteString(pSeg^.RootNam);
      pSeg := pSeg^.Next
    end
end { WriteRunFile }.
