{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module Lights;

{-----------------------------------------------------------------------------
{
{       Lights - Perq Lights.
{       J. P. Strait  26 May 81.
{       Copyright (C) Three Rivers Computer Corporation, 1981, 1982, 1983
{
{ Abstract:
{       This module defines the screen coordinates and size of the
{       Perq "lights".  These are portions of the screen that are
{       inverted during tedious operations such as recalibrating the
{       disk and scavenging files (in FileAccess).
{
{ Design:
{       The lights must *not* extend below the 128th line of the screen.
{       The Y + Size must be less than or equal to 256.
{       It is a good idea for the lights to be totally inside of the title
{          line.
{       The current lights start at the left leave lots of room for new
{          lights to the right of the current one.  There is room for
{          10 lights all together
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{ Versions:
{   01 Apr 83 J Strait       V1.4  Add new light for hardcopy.  Also
{                                   remove the compiler light since
{                                   it is used only for MPOS version.
{   16-Nov-82 Bill Braucher  V1.3  Fixed names for 14-character compiler.
{   20-Jan 82 Brad A. Myers  V1.2  New light for the compiler
{   28-may-81 Brad A. Myers  V1.1  Changed size and spacing of lights and
{                                   made them non-square
{   26-May-81 John Strait    V1.0  Started
{---------------------------------------------------------------------------}

exports

const

      LightUsed        = TRUE; {whether should use the lights at all}
      
      LightY           = 3;
      LightHeight      = 14;
      LightWidth       = 18;
      LightSpacing     = 3*LightWidth;
      
      LightRecalibrate = LightSpacing;
      LightScavenge    = LightRecalibrate + LightWidth + LightSpacing;
      LightSwap        = LightScavenge + LightWidth + LightSpacing;
      LightHardCopy    = LightSwap + LightWidth + LightSpacing;

{*************************} private {******************************}

Procedure CompilerBug;
begin end.

