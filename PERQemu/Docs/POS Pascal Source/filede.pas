{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FileDefs;
{-----------------------------------------------------------------------------
{
{Abstract:
{
{ Defines some constants and types needed by various people so FileSystem
{  doesn't need to import DiskIO in its export section
{
{Written by: Brad A. Myers  3-Mar-81
{
{Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation
----------------------------------------------------------------------------}

{----------------------------------------------------------------------------
 Versions: 
   V1.2 16-Mar-81  Brad A. Myers   Changed openType
   V1.1 11-Mar-81  Brad A. Myers   Added sparse and OpenHow bits to FSData
   V1.0  5-Mar-81  Brad A. Myers   Created by copying text from Arith and
                                    DiskIO
----------------------------------------------------------------------------}

EXPORTS

Imports GetTimeStamp from GetTimeStamp; {Using TimeStamp}

const
  DBLZERO            = nil;    {a two word 0}

type
  FSBit8             = 0..255;
  FSBit16            = integer;
  FSBit32            = ^integer; {will be a long when compiler knows about 'em}

Const DISKBUFSIZE  = 256;      {defined by hardware, 256 words per sec}

type SegID         = FSBit32;  {In SpiceSeg, the virtual address of the
                                 -1 block of a file}
     DiskAddr      = FSBit32;  {The virtual address of a DiskBlock}
                       
     SimpleName    = string[25];   {only the filename in the directory}
     PathName      = string[100];  {full name of file with partition and dev}
     PartialPathName = string[80]; {file name including all directories}
     FSOpenType    = (FSNotOpen, FSOpenRead, FSOpenWrite, FSOpenExecute);
     FSDataEntry   = packed record
                      FileBlocks       : integer;  {Size of file in blocks}
                      FileBits         : 0..4096;  {Number of bits in last blk}
                      FileSparse       : Boolean;  {true if can be sparse}
                      FileOpenHow      : FSOpenType;  {howOpen}
                      FileCreateDate   : TimeStamp;
                      FileWriteDate    : TimeStamp;
                      FileAccessDate   : TimeStamp;
                      FileType         : integer;  {see FileType.pas}
                      FileRights       : integer;  {protection code}
                      FileOwner        : FSBit8;   {UserId of file owner}
                      FileGroup        : FSBit8;   {GroupId}
                      Filename         : PartialPathName;
                 end;
    ptrFSDataEntry = ^FSDataEntry;

PRIVATE

procedure CompilerBug;
begin
end.
