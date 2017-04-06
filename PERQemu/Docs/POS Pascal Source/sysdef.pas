{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module SystemDefs;


{-----------------------------------------------------------------------------
{
{       SystemDefs - Common system definitions.
{       John P. Strait  13 May 81.
{       Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983.
{
{ Abstract:
{       SystemDefs exports common system Const and Type definitions.  The
{       intent is that SystemDefs should not export Procedures or Vars since
{       these require a Seg file.  It is also intended that SystemDefs be
{       reasonably short so that it doesn't take long to import.
{
{-----------------------------------------------------------------------------}
 
{----------------------------------------------------------------------------
{
{ Change log:
{
{  9 Nov 82     V1.4    Chuck Beckett
{ Add dummy procedure to avoid compiler errors.
{
{ 17 Nov 82     V1.3    Scott Brown
{ Add UnderAccent equal false, so the POS version of the compiler can find
{ it here.  Also, if/when we release a debugging compiler under POS, the
{ modules QMapDefs and SymDefs will need this.
{
{  1 Dec 81     V1.2    Brad Myers
{ Set Ether10MBaud to false.
{
{ 04 Nov 81     V1.1    Don Scelza
{ Set Ether10MBaud to true.
{
{-----------------------------------------------------------------------------}


exports


const Ether3MBaud = False;             { no support for 3 MBaud EtherNet }
      Ether10MBaud = True;            { no support for 10 MBaud EtherNet }
      MPOSVersion = False;
      UnderAccent = False;
      

type Double = array[0..1] of Integer;


private
procedure foobar;
begin end.
