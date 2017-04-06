{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module FloppyDefs;
{-----------------------------------------------------------------
{
{ Module FloppyDefs - Diana Connan Forgy
{ Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{
{     Global definitions for the Floppy program.
{
{-----------------------------------------------------------------}
{{$Version V2.0 for POS}
{-----------------------------------------------------------------
{
{ Change Log:
{
{  3 Feb 84 V2.0  Dirk Kalp
{ Add vars HidingDDensErrors, ShowTrackErrors, and ShowVerifyErrors to
{ be used to turn ON/OFF the display of certain floppy errors. See command
{ XSHOWERRORS and the Floppytransfers module..
{
{ 14 Oct 82 V1.1  Roger Riggs
{ Updated to support new I/O subsystem.  Added Interleave switch
{ to control the order of phyical sectors during Format commands.
{
{ 12 May 82 V1.0 Brad A. Myers
{ Fixed lots of bugs.  Add new switches for inverse of some that are here.
{ Removed import of IO
{
{ 19 Apr 82 V0.3 JLC
{ Removed constant FloppyFSFloppy
{
{ 16 Mar 82 V0.2 C Beckett
{ Interfaced program to MPOS floppy IO.  Will now run under either
{ MPOS or POS (hopefully!)
{
{ 28 Feb 82 V0.1 DCF
{ Added FloppyFSFloppy and a set of index constants to use
{ with new commands dealing with boot floppies.
{
{ 1 Dec 81 V0.0 DCF
{ Created FloppyDefs.
{
{-----------------------------------------------------------------}

Exports

Imports PopCmdParse from PopCmdParse;
Imports CmdParse from CmdParse;
Imports SystemDefs from SystemDefs;
{$ifc MPOSVersion then}{===========================================}
Imports IO_Others from IO_Others;
Imports FlpUIoInt from FlpUIoInt;
{$endc=============================================================}

Imports IOErrors from IOErrors;
Imports IO_Unit from IO_Unit;
Imports IOErrMessages from IOErrMessages;
Imports UtilProgress from UtilProgress;
Imports Perq_String from Perq_String;
Imports Screen from Screen;
Imports Configuration from Configuration;


Const BootFloppyVers = false;

const Bell = chr(7);

      { Command Table entries: }
      
      GetIndex         = 1;
      PutIndex         = GetIndex + 1;
      CmprIndex        = PutIndex + 1;
      CompIndex        = CmprIndex + 1;
      DelIndex         = CompIndex + 1;
      DirIndex         = DelIndex + 1;
      HelpIndex        = DirIndex + 1;
      TypeIndex        = HelpIndex + 1;
      ZeroIndex        = TypeIndex + 1;
      DupIndex         = ZeroIndex + 1;
      FlpGetIndex      = DupIndex + 1;
      FlpPutIndex      = FlpGetIndex + 1;
      FormatIndex      = FlpPutIndex + 1;
      FastIndex        = FormatIndex + 1;
      RenIndex         = FastIndex + 1;
      QuitIndex        = RenIndex + 1;
      DensIndex        = QuitIndex + 1;
      ConfIndex        = DensIndex + 1;
      NoConfIndex      = ConfIndex + 1;
      AskIndex         = NoConfIndex + 1;
      NoAskIndex       = AskIndex + 1;
      SafeIndex        = NoAskIndex + 1;
      VerIndex         = SafeIndex + 1;
      NoVerIndex       = VerIndex + 1;
      MountIndex       = NoVerIndex + 1;
      DismtIndex       = MountIndex + 1;
      PauseIndex       = DismtIndex + 1;
      FSFlopIndex      = PauseIndex + 1;
      PathIndex        = FSFlopIndex + 1;
      XShowErrIndex    = PathIndex + 1;
            
      NumCmds = XShowErrIndex;
      
      { Switch Table entries: }
      
      AskSwitch        = 1;
      NoAskSwitch      = AskSwitch + 1;
      VerSwitch        = NoAskSwitch + 1;
      NoVerSwitch      = VerSwitch + 1;
      HelpSwitch       = NoVerSwitch + 1;
      ConfSwitch       = HelpSwitch + 1;
      NoConfSwitch     = ConfSwitch + 1;
       { Special switches for Format and Zero commands }
      DblDensity       = NoConfSwitch + 1;
      SingleDensity    = DblDensity + 1;
      DblSided         = SingleDensity + 1;
      SingleSided      = DblSided + 1;
      Test             = SingleSided + 1;
      NoTest           = Test + 1;
      SWInterleave     = NoTest + 1;
       { Special switches for Directory command }
      ShortSwitch      = SWInterleave + 1;
      LongSwitch       = ShortSwitch + 1;
       { don't delete scratchfiles from CopyFloppy commands }
      DelSwitch        = LongSwitch + 1;
      NoDelSwitch      = DelSwitch + 1;
       { switch for disabling ^L waits for Type}
      WaitSwitch       = NoDelSwitch + 1;
      NoWaitSwitch     = WaitSwitch + 1;
       { switch for using FileSystem floppies }
      FSSwitch         = NoWaitSwitch + 1;
       {}
       { Switches used for XSHOWERRORS cmd.
       {}
      XAllSwitch       = FSSwitch + 1;
      XNoneSwitch      = XAllSwitch + 1;
      XDDensSwitch     = XNoneSwitch + 1;
      XTrackSwitch     = XDDensSwitch + 1;
      XVerifySwitch    = XTrackSwitch + 1;
      
      NumSwitches      = XVerifySwitch;
            
      Interlace        = 5; { Interlace factor for writes }
      
Type DensityTypes = (SDens, DDens); {single or double density for floppies}

var CmdTable, SwitchTable, InTable, OutTable: pNameDesc;
    CmdIndex, SwitchIndex, InIndex, OutIndex, I: Integer;
    FlpCmdFile, CmdFiles: pCmdList;
    Delimiter: Char;
    IsSwitch, FirstCmd: Boolean;
    CmdLine, CommandName, SwitchName: CString;
    Error: String;
    InFiles, OutFiles, DummyList: pArgRec;
    Switches: pSwitchRec;
    HelpString: CString;
    FirstPress: Boolean;
    WhichSwitch: Integer;
    
    { Values set by switches }
    
    Confirm, HoldConfirm, AskSingle, HoldAskSingle, AskWild, HoldAskWild, 
        ConfirmNonWildDelete, HHoldConfirmNonWildDelete, Verify, HoldVerify, 
            LongDir : Boolean;
    DupSuccess: boolean;  { set in CopyFloppy module}
    Answer, InAnswer, OutAnswer, InQuery, OutQuery: String;
    OutFile: Text;
    Dens: DensityTypes;
    Sides: Integer;
    FmtTest: Boolean;       {tells if we test during format}
    Interleave: Integer;    {Number of sectors to leave between sectors }
    Unformatted: Boolean;   {tells whether or not floppy is formatted.}
    DeleteScratch: Boolean; {Tells whether or not to delete scratch files
                            {after Duplicate, FloppyGet, or FloppyPut.}
    Wait: boolean;          { for ^L in Type}
    ExitRoutine: Boolean;
    Finished: Boolean; {Allows us to exit the program.}
    FSFlop: Boolean; { true when using a filesystem floppy }
    
    {}
    { These booleans are provided for us to perform reliability testing
    { on Perq2 double density floppy operations. Because of apparent
    { hardware problems on Perq2 double density, we are currently suppressing
    { some double density errors on retries (re: module FloppyTransfers).
    { These booleans can be altered using the XSHOWERRORS cmd.
    {}
    HidingDDensErrors,
    ShowTrackErrors,
    ShowVerifyErrors: boolean;

{$ifc MPOSVersion then}{===========================================}
procedure UnitIO ( Unit :UnitRng; 
                   Bufr :IOBufPtr; 
                   Command :IOCommands; 
                   ByteCnt :integer;
                   LogAdr :double;
                   HdPtr  :IOHeadPtr;
                   StsPtr :IOStatPtr  );
{$endc=============================================================}
      

Private


{$ifc MPOSVersion then}{===========================================}
procedure UnitIO ( Unit :UnitRng; 
                   Bufr :IOBufPtr; 
                   Command :IOCommands; 
                   ByteCnt :integer;
                   LogAdr :double;
                   HdPtr  :IOHeadPtr;
                   StsPtr :IOStatPtr  );
{---------------------------------------------------------------------------

 ABSTRACT:

   Simulate POS procedure 'UnitIO' for floppy related MPOS IO operations.
      
 PARAMETERS:

   Unit :UnitRng -  This must be 'Floppy', or an error will be returned.
   
   Bufr :IOBufPtr -  Buffer for data transfers, as per 'UnitIO'.
   
   Command IOCommands - Standard UnitIO commands, as per UnitIO.
                            
   ByteCnt :integer - As per UnitIO.  Passed to floppy process.  Must be
                      a multiple of 128.  Floppy process now accepts counts
                      greater than the physical sector size, and executes
                      multiple sector reads to satisfy them.
                      
   LogAdr :double - As per UnitIO.
   
   HdPtr  :IOHeadPtr - Ignored.
   
   StsPtr :IOStatPtr - As per UnitIO.


{---------------------------------------------------------------------------}

begin
  FlpUIO ( Unit, Bufr, Command, ByteCnt, LogAdr, HdPtr, StsPtr);
end { UnitIO and FloppyDefs module }.
{$elsec} {=========================================================}
procedure fun;
begin
end { FloppyDefs module }.
{$endc=============================================================}
