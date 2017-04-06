{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module MicroOption;
{-----------------------------------------------------------------------------
{
{ MicroOption - Classify microassembler options.
{ J. P. Strait   28 Sep 81.
{ Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{        MicroOption classifies PERQ microassembler options.  A function
{ is exported which translates the string name of the option into an
{ element of a scalar type.
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{
{ Change log:
{
{ 28 Sep 81  V1.0  J. Strait.
{ Start module.
{
{-----------------------------------------------------------------------------}

exports


type MOption = (OFirst,
                OInclude,
                OList, ONoList,
                OTitle,
                OPerq1, OPerq1A,
                OBase, ONoBase,
                OUnknown);


  procedure InitOption;
  function MicroOption( S: String ): MOption;


private


type OptionString = String[20];

var OptionName: array[OFirst..OUnknown] of OptionString;

procedure InitOption;
{-----------------------------------------------------------------------------
{
{ Abstract:
{        Initialize the microassembler option module.
{
{-----------------------------------------------------------------------------}

begin { InitOption }
  OptionName[OInclude ] := 'INCLUDE';
  OptionName[OList    ] := 'LIST';
  OptionName[ONoList  ] := 'NOLIST';
  OptionName[OTitle   ] := 'TITLE';
  OptionName[OPerq1   ] := 'PERQ1';
  OptionName[OPerq1A  ] := 'PERQ1A';
  OptionName[OBase    ] := 'BASE';
  OptionName[ONoBase  ] := 'NOBASE';
end { InitOption };

function MicroOption( S: String ): MOption;
{-----------------------------------------------------------------------------
{
{ Abstract:
{        Classify a microassembler option.
{
{ Parameters:
{        S - String name of an option.
{
{ Returns:
{        The scalar value of the option corresponding to S.  If S is not
{        a valid option name, OUnknown is returned.
{
{-----------------------------------------------------------------------------}

var I: Integer;
    M: MOption;
begin { MicroOption }
  for I := 1 to Ord(S[0]) do
    if S[I] in ['a'..'z'] then
      S[I] := Chr(Ord(S[I]) - Ord('a') + Ord('A'));
  OptionName[OUnknown] := S;
  M := OFirst;
  repeat M := Succ(M)
  until OptionName[M] = S;
  MicroOption := M
end { MicroOption }.
