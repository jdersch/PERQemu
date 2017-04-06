{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
{DirScavenge}
{-----------------------------------------------------------------------------
 Abstract: Included in Scavenger for directory rebuilding
{
{  copyright 1982, 1983  Three Rivers Computer Corporation
{
{-----------------------------------------------------------------------------}

Imports FileTypes from FileTypes;


Function PlausibleFilename(var name : PathName) : boolean;
{-----------------------------------------------------------------------------
 Abstract: Checks name to see if valid name for a file; returns false if
            length too big, too small, contains non-printing characters,
            spaces, colons, '<', or has .. or . directory, or ends in a '>'
 Parameters: name is filename to check
 Returns: True if name OK, else false
-----------------------------------------------------------------------------}
  var
    i,len : integer;
    
  begin
    PlausibleFilename := false;
    
    len := length(name);
    if (len < 1) or (len > 80) then exit(PlausibleFilename);
    
    for i := 1 to len do
      begin
        if (name[i] <= ' ') or (name[i] >= chr(#177)) 
          or (name[i] = ':') or (name[i] = '<')
          or (name[i] = '/') 
          then exit(PlausibleFilename);
      end;
      
    if (Pos(name, '>..>') <> 0) or (Pos(name, '>.>') <> 0) then
           exit(PlausibleFilename);
    if SubStr(name, 1, 3) = '..>' then exit(PlausibleFilename);
    if name[len] <> '>' then PlausibleFilename := true;
  end;


Procedure EnterName(addr: DiskAddr; var name: PathName;  Fix: boolean;
                    partID: integer);
{-----------------------------------------------------------------------------
 Abstract: Creates a directory entry for addr.  Will use name if it is OK, 
            otherwise asks user for new one.  If name is OK and contains 
            subDirectories, then creates directories along the way.
 Parameters: addr is the SegID for the file; name is the name in the header
              block, it may not be valid.  If fix, then always writes out the
              addr block to the file. If not fix, then only writes it if
              changed inside EnterName. PartID is partition id for partition
              in which to create directories
 SideEffects: Changes directory, may change FIBlk of file
 Design: Uses Full file name for entering and looking up names so will be sure
          to get them in the partition being scavenged (and not the current
          default partition
-----------------------------------------------------------------------------}
  var
    ptr     : ptrDiskBuffer;
    ok      : boolean;
    checkid : SegID;
    changed : boolean;
    dirName : PathName;
    indx    : Integer;
    hdr     : ptrHeader;
    fullName : PathName;
    
  label 1;
  
  begin
    changed := Fix;
  1:
    fullName := Concat(ScavPartitionName, name);

    if not PlausibleFilename(Name) then
      begin
        Writeln;
        Write('-->',Name,' not legal, type new name: ');
        ReadLn(Name);
        changed := true;
        goto 1;
      end;
   
    if length(name) > 3 then
       begin
       dirName := SubStr(name, length(name)-2, 3);
       ConvUpper(dirName);
       if dirName = '.DR' then
          begin
           Writeln;
           Write('-->',Name,' not a directory, type new name: ');
           ReadLn(Name);
           changed := true;
           goto 1;
         end;
       end;
    
 {find the second > since the first one is at end of partition name}
    indx := length(ScavPartitionName);
    repeat
      indx := indx+1;
    until (fullName[indx]='>') or (indx = Length(fullName));
       {works since fullName[indx] <> '>' }

  {loop through subdirs in Name, creating file DirName if it doesn't exist.
   DirName holds the complete path name of the dir}

 {if no directories on name, then indx = Length(fullName) }
   while fullName[indx] = '>' do {works since Name[length(Name)] <> '>'}
      begin
       dirName := SubStr(fullName, 1, indx-1);
       dirName := Concat(dirName, '.DR');
       checkID := GetFileID(dirName);  {if there, created it during this Scav}
       if checkID = Zero then
          begin
           WriteLn;
           Write('   ----Creating directory: ',dirName);
           checkID := CreateSpiceSegment(PartID, Permanent);
           ok := PutFileID(dirName, checkId); {modifies dirName for Fiblk}
           if not ok then
             begin
              DestroySpiceSegment(checkID);
              Writeln;
              Write('-->Not able to enter dir ', dirName, '. Type new name for ',Name,': ');
              ReadLn(name);
              goto 1;
             end;
           ptr := ChangeDisk(checkID);
           hdr := ReadHeader(checkID);
           ptr^.FSData.Filetype := DirFile;
           ptr^.FSData.FileBlocks := 0;
           ptr^.FSData.FileBits := 4096;
           ptr^.FSData.Filename := DirName;
           WriteDisk(checkID, ptr, hdr);
         end; {create new dir}
       indx := indx + 1;
       while (indx < length(fullName)) and (fullName[indx] <> '>') do
        indx := indx+1;
      end; {loop for > exists}

    checkid := GetFileID(fullName);
    if checkid <> Zero then
      begin
        if fix and (length(name) < 80) then {change name so won't exist}
          begin
          Writeln;
          Write('   ',Name,' exists; trying ');
          AppendChar(Name, '$');
          Write(Name);
          end
        else
          begin
            Writeln;
            Write('--> ',Name,' already exists, type new name: ');
            ReadLn(Name);
          end;
        changed := true;
        goto 1;
      end;
    
    ok := PutFileID(fullName, addr);
    if not ok then
      begin
        Writeln;
        Write('-->Not able to enter ',Name,', type new name: ');
        ReadLn(Name);
        changed := true;
        goto 1;
      end;
   
    if changed then
      begin
        ptr := ChangeDisk(addr);
        ptr^.FSData.Filename := Name;
        FlushDisk(addr);
      end;
  end; {EnterName}


Procedure RebuildDirectory;
{-----------------------------------------------------------------------------
 Abstract: Changes all existing directories to ExDirFiles and creates entirely
            new directories for all files.  Also recreates indices of all
            blocks if desired
 SideEffects: Changes all old directories to exDir and adds a '$' to the end
               of the fileName or deletes them if desired.  Creates a new
               directory structure.  Any empty directories will disappear.
               Recreates indices if user says to
 Environment: Assumes full file system is set up and well formed (except for
              directories) since uses SpiceDir, SpiceSeg, etc.
 NOTE: This procedure will fail if there aren't enough free blocks in partition
-----------------------------------------------------------------------------}
    var
        i, j : integer;
        Blk : FSBit32;
        Name : PathName;
        OldRootDirID : SegID;
        ok, fix : boolean;
        PartID : integer;
        RootID : SegID;
        BadID  : SegID;
        ptr    : ptrDiskBuffer;
        FixIndex: Boolean;
        FixLength: Boolean;
        FileLen : Integer;
        isNormal: Boolean;
        
    Procedure RebuildIndex( i: Integer);
     {---------------------------------------------------------------------
      Abstract: Rebuilds index for file index i
      SideEffects: Changes fiBlk of file and creates index blks
     ----------------------------------------------------------------------}
      var
        ID, addr,next, StartIndex, EndIndex : DiskAddr;
        hdr         : ptrHeader;
        ptr,indptr  : ptrDiskBuffer;
        k, indoff   : integer;
        LNumBlks    : Integer;
        LLastBlk    : Integer;
        LLastAddr   : DiskAddr;
        IndexBlks   : Integer;
        logblk      : Integer;
        newlogblk   : Integer;
        indblk      : Integer;
        
      begin { RebuildIndex }
        Id := IdxToAddr(Disk,i);
        
        addr := Id;
        LNumBlks := 0;
        while addr <> DBLZERO do
          begin
            hdr := ReadHeader(addr);
            LNumBlks := LNumBlks + 1;
            LLastBlk := hdr^.LogBlock;
            LLastAddr := addr;
            addr := hdr^.NextAdr;
          end;
    
        addr := Id;
        StartIndex := DBLZERO;
        EndIndex := DBLZERO;
        IndexBlks := 0;
        hdr := ReadHeader(addr);
        addr := hdr^.PrevAdr;
        while addr <> DBLZERO do
          begin
            hdr := ReadHeader(addr);
            StartIndex := addr;
            if EndIndex = DBLZERO then EndIndex := addr;
            addr := hdr^.PrevAdr;
          end;
   
        if DryRun then
          begin
            ptr := ReadDisk(id);
            hdr := ReadHeader(id)
          end
        else
          begin
            ptr := ChangeDisk(id);
            hdr := ChangeHeader(id);
            hdr^.PrevAdr := DBLZERO;
            ptr^.SegKind := Permanent;
            ptr^.NumBlksInUse := LNumBlks;
            ptr^.LastBlk := LLastBlk;
            ptr^.LastAddr := LLastAddr;
            ptr^.LastNegBlk := -1;
            ptr^.LastNegAddr := id;
            for k := 0 to DIRECTSIZE-1 do ptr^.Direct[k]   := DBLZERO;
            for k := 0 to INDSIZE-1    do ptr^.Indirect[k] := DBLZERO;
            for k := 0 to DBLINDSIZE-1 do ptr^.DblInd[k]   := DBLZERO;
            FlushDisk(id);
            if StartIndex <> DBLZERO then
              DeallocChain(StartIndex,EndIndex,IndexBlks);
          end;
    
    
        addr := Id;
        next := addr;
        while next <> DBLZERO do
          begin
            hdr := ReadHeader(addr);
            logblk := hdr^.LogBlock;
            next := hdr^.NextAdr;
            if next <> DBLZERO then
              begin
                hdr := ReadHeader(next);
                newlogblk := hdr^.LogBlock;
                
                new(indptr);
                Index(newlogblk,indblk,indoff);
                ReadSpiceSegment(id,indblk,1,indptr);
                if DryRun then
                  begin
                    if indptr^.Addr[indoff] <> next then
                      begin
                        Writeln;
                        Writeln('Random index disagrees for logical block ',
                                newlogblk:1);
                        if not Ask('Continue in this file','Yes') then
                          Exit(RebuildIndex)
                      end
                  end
                else
                  begin
                    indptr^.Addr[indoff] := next;
                    WriteSpiceSegment(id,indblk,1,indptr);
                  end;
                dispose(indptr);
    
                addr := next;
              end;
          end;
      end { RebuildIndex };

  label 1;
  
    begin { RebuildDirectory }
    FirstIdx := Interlace;
    InitBuffers;
    InitAlloc;
    DeviceMount(Disk);
    PartID := MountPartition(ScavPartitionName);
    if PartID = 0 then
      begin
      WriteLn('** Could not mount partition ',ScavPartitionName,'. Aborting!');
      DoExit;
      end;
    RootID := CreateSpiceSegment(PartID, Permanent);
    ptr := ChangeDisk(RootID);
    ptr^.FSData.Filetype := Dirfile;
    ptr^.FSData.Filename := 'root.DR';
    FlushDisk(RootID);
    DismountPartition(ScavPartitionName);

    DiskIO(InfoBlk, BufferPtr, LabelPtr, DskRead);
    BadID := BufferPtr^.BadSegID;
    OldRootDirID := BufferPtr^.RootDirID;
    BufferPtr^.RootDirID := RootID;
    if not DryRun then
      DiskIO(InfoBlk, BufferPtr, LabelPtr, DskFirstWrite);
    
    DeviceMount(Disk);
    PartID := MountPartition(ScavPartitionName);
    
    FixIndex := Ask('Rebuild random indexes','No');
    if FixIndex then
       FixIndex := Ask('Are you sure','Yes');

    FixLength := Ask('Check file lengths','No');
    if FixLength then
       FixLength := Ask('Are you sure','Yes');

    DelOldDirs := Ask('Delete old directories','Yes');

    for i := 1 to EndIdx do
        if SegHead^[i] and Confirmed^[i] then
            begin
            CurUpdate(WriteCursor, i);
            Blk := IdxToAddr(Disk,i);
            ptr := ReadDisk(Blk);
            if (ptr^.SegKind = Permanent) and (Blk <> RootID) then
                begin
                fix := false;
                isNormal := false;
                if (length(ptr^.FSData.Filename) < 0) or
                   (length(ptr^.FSData.Filename) > 80)
                  then Name := '** BAD NAME **'
                else Name := ptr^.FSData.Filename;
                Write('SegID ',AddrToField(Blk):6:-10,' ',Name);
                if ptr^.FSData.Filetype = ExDirFile then
                    if DelOldDirs then
                       begin
                       WriteLn('  ~~ (Ex-Directory) ~~ DELETED ~~');
                       DestroySpiceSegment(Blk);
                       goto 1;
                       end
                    else fix := true {so will add $'s to end if there}
                else if ptr^.FSData.Filetype = DirFile then
                   if DelOldDirs then
                        begin
                        WriteLn('  ~~ (Directory) ~~ DELETED ~~');
                        DestroySpiceSegment(Blk);
                        goto 1; {don't enter it}
                        end
                    else begin
                         if Length(name)=80 then name[80] := '$'
                         else AppendChar(name, '$');
                         ptr := ChangeDisk(blk);  {need to do this here}
                         ptr^.FSData.Filetype := ExDirFile;
                         fix := true;
                         end
                else if ptr^.FSData.FileType <> SwapFile then isNormal := true;
                if not DryRun then EnterName(Blk, Name, fix, partID);
                Write('  ... Entered.');
                if FixIndex then
                    begin
                    Write('  Index ');
                    RebuildIndex(i);
                    Write('rebuilt.');
                    end;
                if isNormal and FixLength then {check for length OK}
                  begin
                  ptr := ReadDisk(blk);  {in case ptr messed up}
                  j := ptr^.LastBlk+1;
                  if ptr^.FSData.FileBlocks <> j then
                    begin
                    WriteLn('   ****',chr(7));  {beep}
                    WriteLn('** Stored length (',ptr^.FSData.FileBlocks:1,
                     ') does not agree with actual number of blocks in file (',
                     j:1,') **');
                    If not dryRun then
                       If Ask(' Do you want it fixed? ','No') then
                           begin
                           Write('New length [',j:1,'] ');
                           if not auto then
                             begin
                             if not eoln then read(j);
                             readln;
                             end;
                           ptr := ChangeDisk(blk);
                           ptr^.FSData.FileBlocks := j;
                           FlushDisk(blk);
                           end;
                    end;
                  end; {isNormal}
                WriteLn;
        1:      end; {fix}
            end; {loop}
    
    if (BadID <> Zero) and not DryRun then
        begin
            WriteLn;
            Write('Type name for bad segment (',AddrToField(BadID):1:-10,
                      ') [Bad$] ');
            if auto then begin
                         name := 'Bad$';
                         writeLn(name);
                         end
            else begin
                 ReadLn(name);
                 if name = '' then name := 'Bad$';
                 end;
            EnterName(BadID, Name, true, partID);
            if not FixIndex then
              begin
              Writeln('Suggest building a random index for ', Name, '.');
              if Ask( Concat('Build the index of ', Name), 'Yes') then
                RebuildIndex(AddrToIdx(BadID));
              end
            else RebuildIndex(AddrToIdx(BadID));
        ptr := ChangeDisk(BadID);
        ptr^.FSData.FileType := BadFile;
        ptr^.SegKind := Permanent;
        FlushDisk(BadID);
        end;
    
    DismountPartition(ScavPartitionName);
    end { RebuildDirectory };

