{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
Module IODisk;
{--------------------------------------------------------------------------
{
{ IODisk - Contains Disk IOUnit Function for EIO and CIO Disks. TV.
{          'Private' HardDisk type and variable declarations - available
{          to the IO subsystem.  Disk routines. AGR.
{ 
{ Copyright (C) 1982, 1983 Three Rivers Computer Corporation
{
{ Abstract:
{       IODisk exports variables, constants, and procedures the IO subsystem
{       uses to do disk IO.
{
{-------------------------------------------------------------------------}

{ $Version V0.6 for POS}
{--------------------------------------------------------------------------
{
{ Change Log:
{
{ 15 Aug 83  DBG  V0.6  Modified for 5.25 inch disk:
{    Don't check for SeekError on 5.25 inch disk.  There isn't any.
{
{ 12 apr 83  CDH  V0.5  Modified for cio micropolis:
     Change findSize to always set io24mbyte for micropolis; don't let
     it hang if no hard disk.  
     Change dsk_initialise to do recalibrate only for shugart, and to do reset
     for micropolis: new procedure resetMicropolis introduced.  
     Change DskCio: set up both words of physical address; 
     do long timeout for micropolis seeks (in case of seek 0 giving restore);
     reset command doesn't set bit 5 of command for micropolis;
     result processing for cio micropolis added using the new result format 
     declared in io_unit; 
     if a transfer terminates without the seekComplete bit set in status, 
     this is treated as error ioecmm (cylinder mismatch).
{
{ 16 Feb 83  SSJ  V0.4  Fixed bugs:
{                       a> Time out for EIO resets to 3 minutes.
{                       b> Set Buf.Buffer to nil when DecIOCOunt so that
{                          DecIOCount will not be re-executed.
{                       c> Error codes returned on certain EIO errors changed.
{                       d> use the configuration module.
{
{ 14 Jan 83  AGR  V0.3  Changed the timeout loops so that they use the timer.
{
{ 13 Jan 83  AGR  V0.2  Removed local PDskCtrl so that Dsk_Initialize uses
{                       the global exported by DiskDef
{
{ 30 NOV 82  TV   V0.1  Module rewritten for support of
{                       multiple disks, including all EIO Disks:
{                       Micropolis, Shuggart (and SMD partially).
{                       This code will run on a CIO and EIO system.
{                       A new Control structure, the DCA, was created
{                       for managing multiple disks. Tony Vezza.
{
{ 25 AUG 82  AGR  V0.0  Created module. August G. Reinig.
{
{-------------------------------------------------------------------------}

{*****************************}   Exports   {*****************************}

Imports IO_Unit From IO_Unit;

    

Procedure Dsk_Interrupt;
Procedure Dsk_Initialize;
Procedure Dsk_UnitIO( Unit    : UnitRng;
                      Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : Integer;
                      DskAdr  : Double;
                      HdPtr   : IOHeadPtr;
                      StsPtr  : IOStatPtr );

    {----------------------------------------------------------------------}
    {                                                                      }
    {                                                                      }
    {                                                                      }
    {                                                                      }
    {                                                                      }
    {----------------------------------------------------------------------}


{***************************}   Private   {*****************************}


    Imports DiskDef From DiskDef;
    Imports IO_Private From IO_Private;
    Imports Virtual From Virtual;
    Imports Configuration from configuration;

    Const
        Debug = False;

    {$IFC Debug 
    THEN}    Imports TestSystem From TestSystem;
    {$ELSEC} Imports System From System;  
    {$ENDC}

   

Procedure Dsk_Interrupt;
    {----------------------------------------------------------------------}
    {                                                                      }
    { Disk Interrupt                                                       }
    {                                                                      }
    {    This Procedure is called when an interrupt is received from       }
    {    either the HardDisk (under the CIO System) or the                 }
    {    EIODisk (under the EIO System).                                   }
    {                                                                      }
    {    Input                                                             }
    {        None.                                                         }
    {    Output                                                            }
    {        Clears IOInProgress Flag (set to False)                       }
    {        Clears pUDevTab^[HardDisk].IntrCause.Number                   }
    {        Clears pUDevTab^[EIODisk].IntrCause.Number                    }
    {    Procedures Called                                                 }
    {        None.                                                         }
    {    Exceptions Raised                                                 }
    {        None.                                                         }
    {                                                                      }
    {----------------------------------------------------------------------}

    Begin
        PUDevTab^[HardDisk].IntrCause.Number := 0; 
        PUDevTab^[EIODisk].IntrCause.Number := 0; 

        { This information kept in DskCtrlBlck so we ignore it here. }

        IOInProgress := false;
        InLineByte( {INTON} 106 )  { enable further interrupts }
   End;

Procedure Dsk_Initialize;
    {----------------------------------------------------------------------}
    {                                                                      }
    { Disk Initialize                                                      }
    {                                                                      }
    {    This Procedure Creates and Initializes the Disk Control           }
    {    Structures for both the CIO and EIO Systems. If running under     }
    {    CIO then Recalibrate and Size the Shugart Disk.                   }
    {    Set up the Flag EIOFlag which indicates whether running with      }
    {    a CIO or EIO System.                                              }
    {                                                                      }
    {    Input                                                             }
    {        None.                                                         }
    {    Output                                                            }
    {        DCA (Disk Control Array) Created                              }
    {        CIO DCB (Disk Control Block) Created                          }
    {        EIOFlag Set                                                   }
    {                                                                      }
    {                                                                      }
    {                                                                      }
    {    Procedures Called                                                 }
    {        None.                                                         }
    {    Exceptions Raised                                                 }
    {        None.                                                         }
    {                                                                      }
    {----------------------------------------------------------------------}

    Type
        Buffer = Array[0..256] Of Integer;
        PBuffer = ^Buffer;
  
    Var
        BPtr : PBuffer;
        StatPtr : IOStatPtr;
        LogAddress : Double;
        TempPtr : IOBufPtr;
        Header : IOHeadPtr;



    Procedure FindSize;
        {------------------------------------------------------------------}
        {                                                                  }
        { Find CIO Shugart Size                                            }
        {                                                                  }
        {    See if CIO Shugart is 12M Byte or 24M Byte.                   }
        {                                                                  }
        {    Input                                                         }
        {        None.                                                     }
        {    Output                                                        }
        {        IO24MByte Flag Set                                       }
        {    Procedures Called                                             }
        {        Dsk_UnitIO                                                }
        {    Exceptions Raised                                             }
        {        None.                                                     }
        {                                                                  }
        {------------------------------------------------------------------}

        Const
           First24Sector = #200;
     
        Begin { FindSize }
            SetDDS(397);
            statptr^.softstatus := ioeioc; {preset for micropolis on cio}
            If ciodisktype = cioshugart then
             begin
              StatPtr^.HardStatus := 0;
              LogAddress[0] := First24Sector;         { a 24 MByte address }
              TempPtr := Recast( Bptr, IOBufPtr );
              Dsk_UnitIO( HardDisk, TempPtr,
                     IODiagRead, 0, LogAddress, Header, StatPtr );
             end;
            SetDDS( 398 );
            Case StatPtr^.SoftStatus Of
                IOEIOC : IO24MByte := True;
                IOEADR : IO24MByte := False;
                IOETIM : IO24MByte := False;
                Otherwise : {While True Do { Hang ???? }
                  {Don't hang - there may be no hard disk}
              End; { Case }   
        End; { FindSize }

    Procedure LocateDskHeads;
        {------------------------------------------------------------------}
        {                                                                  }
        { Locate Disk Heads                                                }
        {                                                                  }
        {    Recalibrate CIO Shugart.                                      }
        {                                                                  }
        {    Input                                                         }
        {        None.                                                     }
        {    Output                                                        }
        {        CIO Shugart Calibated.                                    }
        {    Procedures Called                                             }
        {        Dsk_UnitIO                                                }
        {    Exceptions Raised                                             }
        {        None.                                                     }
        {                                                                  }
        {------------------------------------------------------------------}

        Var
          I : Integer;

        Begin { LocateDskHeads }
            SetDDS(394);
            TempPtr := Recast( Bptr, IOBufPtr ); 

            I := -1;
            Repeat       { march forward one track at a time, till we reach 0 }
                I := I+1;
                LogAddress[0] := Shift(I,8);
                StatPtr^.HardStatus := 0;
                Dsk_UnitIO( HardDisk, TempPtr, IOReset,
                                0, LogAddress, Header, StatPtr )
              Until ( Land( StatPtr^.HardStatus, #20) <> 0) Or (I=255);

            If Land( StatPtr^.HardStatus, #20 ) = 0   { Check Cylinder 0 bit }
                Then
                    Repeat  { now march backward }
                        I := I-1;
                        LogAddress[0] := Shift(i,8);
                        StatPtr^.HardStatus := 0;
                        Dsk_UnitIO( HardDisk, TempPtr, IOReset,
                                        0, LogAddress, Header, StatPtr )
                      Until (Land( StatPtr^.HardStatus, #20 ) <> 0) Or (I=0);

            If Land( StatPtr^.HardStatus, #20 ) = 0   { Check cylinder 0 bit }
                Then while true do { hang };              { Not set, hang ??? }

            SetDDS(395);
  
        End; { LocateDskHeads }

    Procedure resetMicropolis;
        {------------------------------------------------------------------}
        {                                                                  }
        { Reset Micropolis disk for icl cio                                 }
        {                                                                  }
        {                                                                  }
        {    Input                                                         }
        {        None.                                                     }
        {    Output                                                        }
        {        Icl cio micropolis reset.                                 }
        {    Procedures Called                                             }
        {        Dsk_UnitIO                                                }
        {    Exceptions Raised                                             }
        {        None.                                                     }
        {                                                                  }
        {------------------------------------------------------------------}


        Begin {  }
            SetDDS(394);
            TempPtr := Recast( Bptr, IOBufPtr ); 

            LogAddress[0] := 0;
            LogAddress[1] := 0;
            StatPtr^.HardStatus := 0;
            Dsk_UnitIO( HardDisk, TempPtr, IOReset,
                              0, LogAddress, Header, StatPtr);


            SetDDS(395);
  
        End; { }

    Var
        EnableMask : Z_CmdRegister;

    Begin  { Dsk_Initialize }
  
        SetDDS(391);


        EIOFlag := (CF_IOBoard = CF_EIO) ; 
        
        
        
        If Not EIOFlag Then
            Begin
                
                With pDskCtrl^ Do
                    Begin
                        Buffer := Nil;
                        DskCommand := 0;
                        DskNumSect := 0;
                        DskAddr    := 0;
                        DskHeader.SerialNum[0] := 0;
                        DskHeader.SerialNum[1] := 0;
                        DskHeader.LogBlock     := 0;
                        DskHeader.Filler       := 0;
                        DskHeader.NextAdr[0]   := 0;
                        DskHeader.NextAdr[1]   := 0;
                        DskHeader.PrevAdr[0]   := 0;
                        DskHeader.PrevAdr[1]   := 0;
                        Dsk.Result := 0;
                        DskPNext   := Nil
                    End;
    
                SetDDS(392);
                New( 0, 256, Bptr );
                New( 0,   1, StatPtr );
                New( 0,   8, Header );
  
                SetDDS(393);  
                  case ciodisktype of
                   cioshugart : LocateDskHeads;
                   ciomicropolis : resetMicropolis;
                   ciounknown : ;
                  end;
  
                SetDDS(396);
                FindSize;        
  
                SetDDS(399);
                Dispose( BPtr );
                Dispose( StatPtr );
                Dispose( Header )
            
            End
  
    End; { Dsk_Initialize }




Procedure DskCIO( Bufr    : IOBufPtr;
                  Command : IOCommands;
                  ByteCnt : Integer;
                  DskAdr  : Double;
                  HdPtr   : IOHeadPtr;
                  StsPtr  : IOStatPtr );
    {----------------------------------------------------------------------}
    {                                                                      }
    { CIO Disk IO                                                          }
    {                                                                      }
    {    This Procedure is called by Disk Unit IO to perform all CIO       }
    {    Disk Operations.                                                  }
    {                                                                      }
    {    Input                                                             }
    {        Bufr    : IOBufPtr                                            }
    {        Command : IOCommands                                          }
    {        ByteCnt : Integer                                             }
    {        DskAdr  : Double                                              }
    {        HdPtr   : IOHeadPtr                                           }
    {        StsPtr  : IOStatPtr                                           }
    {    Output                                                            }
    {        Bufr    : IOBufPtr                                            }
    {        HdPtr   : IOHeadPtr                                           }
    {        StsPtr  : IOStatPtr                                           }
    {        DCA (Disk Control Array) Modified                             }
    {        CIO DCB (Disk Control Block) Modified                         }
    {    Procedures Called                                                 }
    {        DskEIO                                                        }
    {        DskCIO                                                        }
    {    Exceptions Raised                                                 }
    {        None.                                                         }
    {                                                                      }
    {----------------------------------------------------------------------}


    Var
        Tmp : Integer;
        PA : PhyVolAddress;
        Buf : IOPtrKludge;
        StartTime : long;
        TimeOutLength : Integer;


        Procedure IOErr( Err: integer );
                { Return from Disk_IOEP with error Err }   
            Begin { IOErr }   
                StsPtr^.BytesTransferred := 0;
                StsPtr^.SoftStatus := Err;
                If Buf.Buffer <> Nil Then DecIOCount( Buf.Segment );
                Exit( Dsk_UnitIO )
            End; { IOErr }
    
    Begin { DskCIO }
    
        {the code assumes that ciodisktype is set to shugart or micropolis;}
        {if its set to Unknown the effect is undefined.}

        Buf.Buffer := Bufr;
        IncIOCount( Buf.Segment);
        
        PDskCtrl := ReCast( PUDevTab^[HardDisk].PDataCtrl, PDiskCtrlBlock );
        With pDskCtrl^ Do
            Begin    
                Buffer := Bufr;
                DskNumSect := ByteCnt Div DskBlockSize;
                DskAddr := DskAdr[ 0];  {for microp, hd/sec}
                DskDevCyl := DskAdr[1]; {for shugart, ignored}
                DskHeader := HdPtr^;
    
                Case Command of
                    IOReset      : if ciodisktype = cioshugart then
                                     DskCommand := Ord(DskClear) + #40
                       { Force IOReset to clear write fault } 
                                   else dskcommand := ord(dskclear);
                    IORead       : DskCommand := Ord(DskRdCheck);
                    IOWrite      : DskCommand := Ord(DskWrCheck);
                    IOSeek       : DskCommand := Ord(DskSeek);
                    IOFormat     : DskCommand := Ord(DskFormat);
                    IODiagRead   : DskCommand := Ord(DskDiagRead);
                    IOWriteFirst : DskCommand := Ord(DskWrFirst);
                    Otherwise    : IOErr( IOEILC )
                  End { Case }
                  
            End;
    
        IOInProgress := True;      { Mark IO as Busy }
        StartIO( EP_HardDisk );
        StoreXpr(Tmp);             { Result from startio - will be true }
  
        StartTime := TimeBuf^;  { time out in 60 jiffies, (one second) }
          {or 180 jiffies if micropolis & seek,in case of restore}
        if (ciodisktype=ciomicropolis) and (command=ioseek) then
          timeoutlength := 180
        else timeOutLength := 60;
        Tmp := pDskCtrl^.DskHeader.LogBlock;
        while (TimeBuf^ - StartTime < TimeOutLength) and IOInProgress do  
          if pDskCtrl^.DskHeader.LogBlock <> Tmp then begin
             StartTime := TimeBuf^;  
             Tmp := pDskCtrl^.DskHeader.LogBlock; 
             end; 
             
        StsPtr^.HardStatus := PDskCtrl^.Dsk.Result;
        
        If IOInProgress
            Then
                Begin    { Reset the Device }
                    if ciodisktype=cioshugart then
                      PDskCtrl^.DskCommand := Ord(DskClear) + #40
                    else
                      PDskCtrl^.DskCommand := Ord(DskClear);
                    PDskCtrl^.DskAddr := 0;
                    pdskctrl^.Dskdevcyl := 0;
                    IOInProgress := true;
                    StartIO( EP_HardDisk );
              
                    { time out the reset in 120 jiffies, (two seconds) }
                    StartTime := TimeBuf^;  
                    while (TimeBuf^ - StartTime < 120) and IOInProgress do;  

                    IOErr( IOETIM )
                End;
    
        DecIOCount( Buf.Segment );
        Buf.Buffer := Nil;         { so no more DecIOCounts happen. }

  
        With PDskCtrl^ Do        { now determine the error status }
            Begin
             if ciodisktype = cioshugart then
              begin
                If Dsk.WriteFault Then IOErr( IOEWRF );
                Case Dsk.CntlError Of
                    AddrsErr  : IOErr( IOEADR );
                    PHCRC     : IOErr( IOEPHC );
                    LHSer     : IOErr( IOELHS );
                    LHLB      : IOErr( IOELHB );
                    LHCRC     : IOErr( IOELHC );
                    DaCRC     : IOErr( IOEDAC );
                    Busy      : IOErr( IOEDNI );
                    Otherwise : begin { nada } end
                  End; { Case }
              end
             else {micropolis on cio}
              begin
                If Dsk.cioMFault Then IOErr( IOEWRF );
                If Dsk.cioMillegalAddr Then IOErr( IOEADR );
                Case Dsk.cioMCntlError Of
                    cioMAddrsErr  : IOErr( IOEADR );
                    cioMPHCRC     : IOErr( IOEPHC );
                    cioMLHSer     : IOErr( IOELHS );
                    cioMLHLB      : IOErr( IOELHB );
                    cioMLHCRC     : IOErr( IOELHC );
                    cioMDaCRC     : IOErr( IOEDAC );
                    cioMBusy      : IOErr( IOEDNI );
                    Otherwise : begin { nada } end
                  End; { Case }
                 if not dsk.cioMseekcomplete then IOErr(IOECMM) 
                     {cylinder mismatch}
              end;

              HdPtr^ := DskHeader;
            End    

    End; { DskCIO }



Procedure DskEIO( ID      : VolID;
                  Bufr    : IOBufPtr;
                  DCom     : IOCommands;
                  ByteCnt : Integer;
                  DskAdr  : Double;
                  HdPtr   : IOHeadPtr;
                  StsPtr  : IOStatPtr );
    {----------------------------------------------------------------------}
    {                                                                      }
    { EIO Disk IO                                                          }
    {                                                                      }
    {    This Procedure is called by Disk Unit IO to perform all EIO       }
    {    Disk Operations.                                                  }
    {                                                                      }
    {    Input                                                             }
    {        ID      : VolID                                               }
    {        Bufr    : IOBufPtr                                            }
    {        DCom    : IOCommands                                          }
    {        ByteCnt : Integer                                             }
    {        DskAdr  : Double                                              }
    {        HdPtr   : IOHeadPtr                                           }
    {        StsPtr  : IOStatPtr                                           }
    {    Output                                                            }
    {        Bufr    : IOBufPtr                                            }
    {        HdPtr   : IOHeadPtr                                           }
    {        StsPtr  : IOStatPtr                                           }
    {        DCA (Disk Control Array) Modified                             }
    {    Procedures Called                                                 }
    {        DskEIO                                                        }
    {        DskCIO                                                        }
    {    Exceptions Raised                                                 }
    {        None.                                                         }
    {                                                                      }
    {----------------------------------------------------------------------}

   
    Var
        Index, Tmp : Integer;
        Buf : IOPtrKludge;
        StartTime : long;
        TimeOutLength : Integer;
        

        Procedure IOErr( Err: integer );
                { Return from Disk_IOEP with error Err }   
            Begin { IOErr }   
                StsPtr^.BytesTransferred := 0;
                StsPtr^.SoftStatus := Err;
                If Buf.Buffer <> Nil Then DecIOCount( Buf.Segment );
                Exit( Dsk_UnitIO )
            End; { IOErr }
    
    Begin { DskEIO }
    
        Buf.Buffer := Bufr;
        IncIOCount( Buf.Segment);

        With PtrDCA^[ ID] Do
            Begin    
                DataBufferPointer := Bufr;
                SectorCount := ByteCnt Div DskBlockSize;
                PhysicalAddress := DskAdr;
                HeaderBufferPointer := HdPtr;
    
                Case DCom Of
                    IOIdle       : Command := 0;
                    IOReset      : Command := 16;
                    IORead       : Command := 7;
                    IOWrite      : Command := 3;
                    IOSeek       : Command := 8;
                    IOFormat     : Command := 1;
                    IODiagRead   : Command := 5;
                    IOWriteFirst : Command := 2;
                    Otherwise    : IOErr( IOEILC )
                  End { Case }
                  
            End;
    
        IOInProgress := True;      { Mark IO as Busy }
        
        Index := ID;
        LoadExpr( Index);
        
        StartIO( EP_HardDisk );
        StoreXpr(Tmp);             { Result from startio - will be true }
  
        TimeOutLength := 60;
        If DCom = IOReset Then TimeOutLength := 180;
        StartTime := TimeBuf^;  { time out in 60 jiffies, (one second) }
        with PtrDCA^[ID].HeaderBufferPointer^ do begin
          Tmp := LogBlock;
          while (TimeBuf^ - StartTime < TimeOutLength) and IOInProgress do  
            if LogBlock <> Tmp then begin
              StartTime := TimeBuf^;  
              Tmp := LogBlock; 
              end; 
          end;

        StsPtr^.HardStatus := Recast( PtrDCA^[ ID].DskStatus, Integer);
        
        If IOInProgress
            Then
                Begin    { Reset the Device }
                    PtrDCA^[ ID].Command := 16;
                    PtrDCA^[ ID].PhysicalAddress[ 0] := 0;
                    PtrDCA^[ ID].PhysicalAddress[ 1] := 0;
                    IOInProgress := true;
                    LoadExpr( Index);
                    StartIO( EP_HardDisk );

                    { time out the reset in 120 jiffies, (two seconds) }
                    StartTime := TimeBuf^;  
                    while (TimeBuf^ - StartTime < 180) and IOInProgress do;  

                    { Check for time out }
                    IOErr( IOETIM )
                End;
    
        DecIOCount( Buf.Segment );
        Buf.Buffer := Nil;                   { so no more DecIOCounts. }
        HdPtr^ := PtrDCA^[ ID].HeaderBufferPointer^;
  
        With PtrDCA^[ ID] Do
          With DskStatus Do     { now determine the error status }
            Begin
                If NotOnCyl Then IOErr( IOENOC );
                If DskType <> D5Inch then
                  If Not NotTrk0OrNotSker Then IOErr( IOESKE );
                If Not NotFault Then IOErr( IOEFLT );
                Case SMSt Of
                    AbnormalError : IOErr( IOEABN);
                    PHMisMatch    : IOErr( IOEADR);
                    LHMisMatch    : IOErr( IOELHE);
                    SMError       : IOErr( IOEABN);
                    HeadCRC       : IOErr( IOELHC);
                    DataCRC       : IOErr( IOEDAC);
                    DBusy         : IOErr( IOEDNI);
                    DIdle         : Begin End
                  End; { Case }

            End    

    End; { DskEIO }


Procedure Dsk_UnitIO( Unit    : UnitRng;
                      Bufr    : IOBufPtr;
                      Command : IOCommands;
                      ByteCnt : Integer;
                      DskAdr  : Double;
                      HdPtr   : IOHeadPtr;
                      StsPtr  : IOStatPtr );
    {----------------------------------------------------------------------}
    {                                                                      }
    { Disk Unit IO                                                         }
    {                                                                      }
    {    This Procedure is called by Unit IO to perform all CIO and EIO    }
    {    Disk Operations (not including Floppy).                           }
    {                                                                      }
    {    Note that the Log Adr Parameter is really a Physical address      }
    {    Parameter which may include the Vol ID of the Device. This        }
    {    Vol ID is only used here under an EIO System.                     }
    {                                                                      }
    {        LogAdr[0] =  Head*(2^8) + Sector                              }
    {        LogAdr[1] = VID*(2^13) + Cylinder                             }
    {                                                                      }
    {    This System of passing the VID to Unit IO as part of the Cyl      }
    {    address imposes a limit to the size of the Cylinder address       }
    {    to 13 bits. This is 8K Cylinders.                                 }
    {    This restriction can be avoided if 8 new Unit Rng devices are     }
    {    created and used to pass the VID to Unit IO. This Optimization    }
    {    has been left for future implementation.                          }
    {                                                                      }
    {    Input                                                             }
    {        Unit    : UnitRng                                             }
    {        Bufr    : IOBufPtr                                            }
    {        Command : IOCommands                                          }
    {        ByteCnt : integer                                             }
    {        DskAdr  : double                                              }
    {        HdPtr   : IOHeadPtr                                           }
    {        StsPtr  : IOStatPtr                                           }
    {    Output                                                            }
    {        Bufr    : IOBufPtr                                            }
    {        HdPtr   : IOHeadPtr                                           }
    {        StsPtr  : IOStatPtr                                           }
    {        DCA (Disk Control Array) Modified                             }
    {        CIO DCB (Disk Control Block) Modified                         }
    {    Procedures Called                                                 }
    {        DskEIO                                                        }
    {        DskCIO                                                        }
    {    Exceptions Raised                                                 }
    {        None.                                                         }
    {                                                                      }
    {----------------------------------------------------------------------}

    Var
        DAddr : Double;
        ID : Integer;
  
        Procedure IOErr( Err: integer );
                { Return from Disk_IOEP with error Err }   
            Begin { IOErr }   
                StsPtr^.BytesTransferred := 0;
                StsPtr^.SoftStatus := Err;
                Exit( Dsk_UnitIO )
            End; { IOErr }
    
    Begin { Dsk_UnitIO }

          { Check Type of Unit }
        If ( Unit <> HardDisk) And ( Unit <> EIODisk) Then IOErr( IOEBUN);
          { Check Blocksize }
        If (ByteCnt Mod DskBlockSize) <> 0 Then IOErr( IOEBSE ); 
          { Make sure we have a Header }
        If HdPtr = Nil Then IOErr( IOENHP);
          { Check Buffer alignment }
        DAddr := ReCast( Bufr, Double);
        If LAnd( DAddr[ 0], 255) <> 0 Then IOErr( IOEBAE);
        
        DAddr[ 0] := DskAdr[ 0];
        DAddr[ 1] := LAnd( #17777, DskAdr[ 1]);
        ID := Shift( LAnd( #160000, DskAdr[ 1]) , -13);
        
        If EIOFlag
            Then DskEIO( ID, Bufr, Command, ByteCnt, DAddr, HdPtr, StsPtr)
            Else DskCIO( Bufr, Command, ByteCnt, DAddr, HdPtr, StsPtr);

        StsPtr^.SoftStatus := IOEIOC;
        StsPtr^.BytesTransferred := ByteCnt
        
    End. { Dsk_UnitIO }
        


