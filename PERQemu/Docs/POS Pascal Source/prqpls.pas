{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module PrqPl_Sort;

{-----------------------------------------------------------------------------
{
{  Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation
{
{  Written by:  Mark G. Faust and John P. Strait
{
{  Abstract:                                        
{
{   This module implements a hybrid Mergesort/Quicksort for PrqPlace.  It
{   uses Hoare's Quicksort algorithm with some simple optimizations.  This
{   module provides types used by PrqPlace for dealing with its file of cross
{   reference entries and a procedure for sorting such a file.  
{
{   For a detailed description of the Quicksort algorithm and references
{   to papers on its analysis see [Robert Sedgewick, "Implementing
{   Quicksort Programs," in  CACM 21(10), 1978.]  
{
{
{-----------------------------------------------------------------------------}

{-----------------------------------------------------------------------------
{
{ Change Log:
{                                                             
{ 30 Sep 81  V2.0  John P. Strait
{ Adapt from Mark Faust's Quicksort module.
{
{ 10 Sep 81  V1.1  Mark G. Faust                            
{ Added Fold for folding to upper case when doing string comparison.
{
{  2 Sep 81  V1.0  Mark G. Faust
{ Module written.
{
{-----------------------------------------------------------------------------}

exports


const NameMax = 9;

type Name = packed array[0..NameMax] of Char;
     
     RefEntry = record
                  PrintName: Name;
                case Integer of
                    1: (RefLine: Integer);
                    2: (Page: Integer;
                        Line: Integer)
                end;

     RefFileType = file of RefEntry;


procedure Sort( var RefFileName: String );


    
private
  
  
  imports UtilProgress from UtilProgress;
  imports Memory from Memory;


const InMemorySortLength = 500;

type pRefFileType = ^RefFileType;


{$R-}

     



const

    Version = '2.0';


function max(x,y :integer):integer;
begin
if x > y then max := x
else max := y;
end;


function min(x,y :integer):integer;
begin
if x < y then min := x
else min := y;
end;

procedure Sort( var RefFileName: String );
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{       Sorts the cross reference entries in the file named by RefFileName.
{       A hybrid Mergesort/Quicksort is used.  Runs of length
{       InMemorySortLength are sorted by Quicksort and then those runs are
{       sorted by MergeSort.  Since the sorted result may be left in a file
{       whose name is not the same as the original file name, RefFileName
{       is changed to contain the name of the resulting file.
{
{
{ Parameters:
{
{       RefFileName: String   Name of the file containing the unsorted cross
{                             reference entries.  RefFileName is changed to
{                             contain the name of the resulting file.
{
{
{ Design:
{
{       See module header.
{
{-----------------------------------------------------------------------------}

var F1Name, F2Name, F3Name, F4Name, TName: String;
    RunLength, RefCount: Integer;

  function Greater( A, B: RefEntry ): Boolean; 
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Determine if A is greater than B.
{
{ Parameters:
{       A, B - Two RefEntrys to compare.
{
{ Result:
{       True iff A > B.
{
{-----------------------------------------------------------------------------}

  begin { Greater }
    if A.PrintName > B.PrintName then Greater := True
    else
      if A.PrintName = B.PrintName then
        if A.Page > B.Page then Greater := True
        else
          if A.Page = B.Page then Greater := A.Line > B.Line
          else Greater := False
      else Greater := False
  end { Greater };

  function GreaterOrEqual( A, B: RefEntry ): Boolean; 
{-----------------------------------------------------------------------------
{
{ Abstract:
{       Determine if A is greater than or equal to B.
{
{ Parameters:
{       A, B - Two RefEntrys to compare.
{
{ Result:
{       True iff A >= B.
{
{-----------------------------------------------------------------------------}

  begin { GreaterOrEqual }
    if A.PrintName > B.PrintName then GreaterOrEqual := True
    else
      if A.PrintName = B.PrintName then
        if A.Page > B.Page then GreaterOrEqual := True
        else
          if A.Page = B.Page then GreaterOrEqual := A.Line >= B.Line
          else GreaterOrEqual := False
      else GreaterOrEqual := False
  end { GreaterOrEqual };

  procedure Distribute;
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{       Sorts runs of length InMemorySortLength using Quicksort and
{       distributes them to files named by F1Name and F2Name.  If the
{       number of entries is less than or equal to InMemorySortLength,
{       all entries are distributed (in order) to the file named by
{       F1Name.
{
{ Side Effects:
{       RefCount is set to the number of cross reference entries.
{
{-----------------------------------------------------------------------------}

  type SortArray = array[0..InMemorySortLength] of RefEntry;
       pSortArray = ^SortArray;
  var pF1, pF2, T: pRefFileType;
      ASeg: SegmentNumber;
      A: pSortArray;
      N, I: Integer;
      RefFile: RefFileType;

    procedure Quicksort; 
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{       Given an integer N and a pointer A to an array [0..N+1] of cross
{       reference entries, sort [0..N] into ascending order using QuickSort.
{       Quicksort is case sensitive (e.g. A < a), but we know the names are
{       already in upper case.
{
{-----------------------------------------------------------------------------}

    const
        
        MaxStackDepth = 40;                 { max of O(2^MaxStackDepth/2) keys }
        CutOff = 9;                         { smallest size subfile }
    
    
    var
        left, right, i, j  :integer;
        v :RefEntry;
        tmp :RefEntry;
        done :boolean;
    
        StackPtr :0..MaxStackDepth;
        Stack :array[1..MaxStackDepth] of integer;

    begin { Quicksort } 
    
                
    with A^[N+1] do
        begin
        PrintName :=  '';
        Page := 32000;
        Line := 32000
        end;
    left := 0;                 
    right := N;
    StackPtr := 0;
    done := (N <= CutOff);
    
    while not done do
        begin                    
        tmp := A^[left+1];
        A^[left+1] := A^[(left+right) div 2];    
        A^[(left+right) div 2] := tmp;
    
        if Greater(A^[left+1],A^[right]) then
            begin
            tmp := A^[left+1];
            A^[left+1] := A^[right];
            A^[right] := tmp;
            end;
    
        if Greater(A^[left],A^[right]) then
            begin
            tmp := A^[left];
            A^[left] := A^[right];
            A^[right] := tmp;
            end;
    
        if Greater(A^[left+1],A^[left]) then
            begin
            tmp := A^[left+1];
            A^[left+1] := A^[left];
            A^[left] := tmp;
            end;
    
        i := left+1;
        j := right;
        v := A^[left];
    
        while (i < j) do
            begin
            repeat i := i+1 until GreaterOrEqual(A^[i],v);
            repeat j := j-1 until GreaterOrEqual(v,A^[j]);
            if (i < j) then
                begin
                tmp := A^[j];
                A^[j] := A^[i];
                A^[i] := tmp;
                end;
            end;
    
        tmp := A^[left];
        A^[left] := A^[j];
        A^[j] := tmp;
    
        if (max(j-left,right-i+1) <= CutOff) then
            begin                                
            if StackPtr = 0 then    { stack empty }
                done := true
            else
                begin       { pop stack }
                left := Stack[StackPtr];
                right := Stack[StackPtr-1];
                StackPtr := StackPtr-2;
                end;                     
            end
    
    
        else                       { push large subfile }
    
            begin
            if (j-left > right-i+1) then  { large subfile is (left,j-1) }         
                begin
                if (right-i+1 <= CutOff) then
                    right := j-1
                else
                    begin
                    StackPtr := StackPtr+1;
                    Stack[StackPtr] := j-1;
                    StackPtr := StackPtr+1;
                    Stack[StackPtr] := left;
                    left := i;          
                    end;
                end
    
            else                          { large subfile is (i,right) }
                begin
                if (j-left <= CutOff) then
                     left := i
                else
                    begin              
                    StackPtr := StackPtr+1;
                    Stack[StackPtr] := right;
                    StackPtr := StackPtr+1;
                    Stack[StackPtr] := i;
                    right := j-1;       
                    end;                
                end;
            end;
        end;
    
    
    { insertion sort for subfiles }
    
    for i := N-1 downto 0 do
        begin            
        if Greater(A^[i],A^[i+1]) then
            begin
            v := A^[i];
            j := i+1;
            repeat
                begin
                A^[j-1] := A^[j];
                j := j+1;
                end
            until GreaterOrEqual(A^[j],v);
            A^[j-1] := v;
            end;
        end;


    end { QuickSort };

  begin { Distribute }
    LoadCurs;
    Reset(RefFile,RefFileName);
    New(pF1); Rewrite(pF1^,F1Name);
    New(pF2); Rewrite(pF2^,F2Name);
    RefCount := 0;
    CreateSegment(ASeg,2,2,100);
    New(ASeg,1,A);
    while not Eof(RefFile) do
      begin
        N := 0;
        while (N < InMemorySortLength) and not Eof(RefFile) do
          begin
            StreamProgress(RefFile);
            A^[N] := RefFile^;
            N := N + 1;
            RefCount := RefCount + 1;
            Get(RefFile)
          end;
        N := N - 1;   { Quicksort wants this funny number }
        QuickSort;
        for I := 0 to N do
          begin
            pF1^^ := A^[I];
            Put(pF1^)
          end;
        T := pF1;
        pF1 := pF2;
        pF2 := T
      end;
    DecRefCount(ASeg);
    Close(pF1^); Dispose(pF1);
    Close(pF2^); Dispose(pF2);
    Close(RefFile)
  end { Distribute };

  procedure Merge;
{-----------------------------------------------------------------------------
{
{ Abstract:
{
{       Performs one merge pass of the Mergesort.  Files named by F1Name
{       and F2Name are merged and the result is left in files named by
{       F3Name and F4Name.  On the final merge pass (when RunLength +
{       RunLength >= RefCount), the sorted result is left in the file
{       named by F3Name.
{
{-----------------------------------------------------------------------------}

  var F1, F2: RefFileType;
      pF3, pF4, T: pRefFileType;
      IF1, IF2: Integer;
  begin { Merge }
    Reset(F1,F1Name);
    Reset(F2,F2Name);
    New(pF3); Rewrite(pF3^,F3Name);
    New(pF4); Rewrite(pF4^,F4Name);
    LoadCurs;
    while not (Eof(F1) and Eof(F2)) do
      begin
        IF1 := 0;
        IF2 := 0;
        while (IF1 < RunLength) and (IF2 < RunLength) and
              not Eof(F1) and not Eof(F2) do
          begin
            StreamProgress(F1);
            if Greater(F2^,F1^) then
              begin
                pF3^^ := F1^;
                Put(pF3^);
                Get(F1);
                IF1 := IF1 + 1
              end
            else
              begin
                pF3^^ := F2^;
                Put(pF3^);
                Get(F2);
                IF2 := IF2 + 1
              end
          end;
        while (IF1 < RunLength) and not Eof(F1) do
          begin
            StreamProgress(F1);
            pF3^^ := F1^;
            Put(pF3^);
            Get(F1);
            IF1 := IF1 + 1
          end;
        while (IF2 < RunLength) and not Eof(F2) do
          begin
            pF3^^ := F2^;
            Put(pF3^);
            Get(F2);
            IF2 := IF2 + 1
          end;
        T := pF3;
        pF3 := pF4;
        pF4 := T
      end;
    Close(F1);
    Close(F2);
    Close(pF3^); Dispose(pF3);
    Close(pF4^); Dispose(pF4)
  end { Merge };
   
   
begin { Sort }
  F1Name := 'PrqPlace.Scratch.1';
  F2Name := 'PrqPlace.Scratch.2';
  Distribute;
  RunLength := InMemorySortLength;
  F3Name := 'PrqPlace.Scratch.3';
  F4Name := RefFileName;
  while RunLength < RefCount do
    begin
      Merge;
      RunLength := RunLength + RunLength;
      TName := F1Name;
      F1Name := F3Name;
      F3Name := TName;
      TName := F2Name;
      F2Name := F4Name;
      F4Name := TName
    end;
  RefFileName := F1Name
end { Sort }.
