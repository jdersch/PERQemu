{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
MODULE PDCommon;
{
{  copyright 1980, 1981, 1982, 1983  Three Rivers Computer Corporation  }

EXPORTS

{PERQ Diagnostic Common--Types and constants used by both PDS and PDM.  
 
    Version
       V0.3   Bill Braucher 16-Nov-82  Fixed names for 14-character compiler.
       V0.2   MAB+BAM       30-Oct-80   Communication Protocal revamped.
       V0.1   BAM           20-Oct-80   changed to module
       V0.0   BAM           16-Oct-80   begun

}
CONST ImHere = #54321;   {Msg to PDM from special microcode}
      Stop = -1;         {Msg to PDS to signal it to BPT }
      UniversalAddress = #20;
                         { Address to restart PDS at }
      StatRegBase = #20; { Register containing first word of Status Block }
      
TYPE Byte = 0..#377;

TYPE PDCStatus = PACKED RECORD
                  CASE integer OF
                    1 : (Int: INTEGER);
                    2 : (LowB: Byte;
                         HighB: Byte);
                    3 : (Aborted: boolean;  { Bit 0.  Aborted.  Abortion
                                              acknowledgement from PDS to PDM.
                                              One shot flag cleared by PDM }
                         EndOfST: boolean;  { Bit 1.  End of Subtest.  One
                                              shot flag set by PDS to tell PDM
                                              that the current subtest has
                                              completed.  PDM can then start
                                              the next subtest if desired;
                                              otherwise PDS remains "command
                                              ready." }
                         SSTBPT: boolean;   { Bit 2.  Single Step Breakpoint.
                                              PDS signal to PDM that PDS has
                                              stopped due to being in Single
                                              Step Mode.  One shot flag cleared
                                              by PDM. }
                         Error: boolean;    { Bit 3.  Set by PDS when an error
                                              has occured.  Cleared by PDM. }
                         BadData: boolean;  { Bit 4.  Set by PDS when bad data
                                              is received over the link.  One
                                              shot flag cleared by PDM. }
                         LFreeBits: 0..7;   { Unused }
                         { NOTE: The upper byte is tested for a complete zero
                           value before individual flags are tested
                         }
                         Term: boolean;     { Bit 8.  Terminate.  Causes
                                              current generator to terminate.
                                              One shot flag cleared by PDS }
                         SST: boolean;      { Bit9.  Single Step Mode }
                         AdvOnce: boolean;  { Bit 10.  Advance on Next time.
                                              Causes generator to advance on
                                              the next generation irreguardless
                                              of the Loop Flag. }
                         RepOnce: boolean;   { Bit 11.  Repeat on Next time.
                                               Causes the generator to repeat
                                               on the next generation
                                               irreguardless of the loop flag }
                         Loop: boolean;      { Bit 12.  Hold all pattern
                                               generators. }
                         HFreeBits: 0..7     { Bits 13 to 15 }
                                             { Unused-But Must Be 0!!!!!! }
                         )
                  end;

ErrBlock = ARRAY [0..0] of integer;
ErrBlkPtr = ^ErrBlock;

PDCStatBlock = packed record case boolean of
                 true: (errPtr : ErrBlkPtr;
                        subTestNum: Byte;
                        errorNum: Byte;
                        status: PDCStatus);
                 false:(ary: array[0..3] of integer)
                end;

PDCStatPtr = ^PDCStatBlock;  {this is used as "MakePtr(SITSeg,4,PDCStatPtr)" }


{**********************} PRIVATE {******************************}

Procedure CompilerBug;
begin end.

