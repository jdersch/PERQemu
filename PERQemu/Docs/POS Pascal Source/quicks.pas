{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module QuickSort;

{------------------------------------------------------------------------------
{
{  Copyright (C) 1981, 1982, 1983 Three Rivers Computer Corporation
{
{  Written by:  Mark G. Faust
{
{  Abstract:                                        
{
{   Hoare's Quicksort algorithm with some simple optimizations.  This
{   module provides two procedures, IntegerSort and StringSort, which sort
{   arrays of integers and strings respectively.  
{
{   For a detailed description of the algorithm and references to papers on its
{   analysis see [Robert Sedgewick, "Implementing Quicksort Programs," in 
{   CACM 21(10), 1978.]  
{
{   Because of rigid type checking of arrays in Pascal, pointers to the arrays
{   to be sorted are passed along with an integer specifying the length of the
{   array.  The procedures require that the array be declared [0..N+1] where 
{   the 0th through Nth elements are to be sorted.  The additional array
{   element is used to speed up the sorting routine.  Before passing the array
{   pointer to the sort procedure it is RECAST as either a IntegerArrayPtr or a
{   StringArrayPtr.  An example for the integer sort is given below.  The 
{   string sort is analogous.
{
{                    
{   program ShowSort(input,output);
{
{   imports QuickSort from QuickSort;
{
{    const
{       Size = 99;
{
{    type
{       MyArray = array[0..Size+1] of integer;
{
{    var
{       MyArrayPtr :^MyArray;
{       i :integer;
{
{    begin
{    new(MyArrayPtr);
{
{    for i := 0 to Size do
{       readln(MyArrayPtr^[i]);
{
{    IntegerSort(Size,recast(MyArrayPtr,PIntArray));
{
{    for i := 0 to Size do
{       writeln(MyArrayPtr^[i]);
{    end.
{
{------------------------------------------------------------------------------}
{------------------------------------------------------------------------------
{
{ Change Log:
{                                                             
{      30 Nov 81        Version 1.2   Brad Myers                            
{       Change length of strings for StringArray to be 25 so can be used with
{       filenames by Delete, Rename, etc.
{ }
{      10 Sep 81        Version 1.1   Mark G. Faust                            
{       Added Fold for folding to upper case when doing string comparison
{ }
{
{       2 Sep 81        Version 1.0   Mark G. Faust
{       Module written
{ }
{
{-----------------------------------------------------------------------------}


exports

type

    ss25 = String[25];
    IntArray = array[0..0] of integer;
    StrArray = array[0..0] of ss25;

    PIntArray = ^IntArray;
    PStrArray = ^StrArray;

procedure IntegerSort(N :integer; A :PIntArray);
procedure StringSort(N :integer; A: PStrArray; Fold :boolean);


    
private

{$R-}

     




imports Perq_String from Perq_String;

const

    Version = '1.2';


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


function UpCase(S :string) :string;
{------------------------------------------------------------------------------
{
{ Abstract:
{    Return upper case of S.  Needed because Perq_String's ConvUpper procedure
{    munges the string itself.
{
{-----------------------------------------------------------------------------}

begin
ConvUpper(S);
UpCase := S;
end;



procedure IntegerSort(N :integer; A :PIntArray);
{------------------------------------------------------------------------------
{
{ Abstract:
{       Given an integer N and a pointer to an array [0..N+1] of integers, sort
{       [0..N] into ascending order using QuickSort.
{
{ Parameters:
{       N :integer      One less than the upper bound of the array.  It is
{                       the largest index of a valid key.
{
{       A :PIntArray    A pointer to an array [0..N+1] of integers.
{
{ Results:
{       The array from [0..N] is in ascending order
{
{ Side Effects:
{       The array is sorted and the N+1st element contains MaxInt
{
{-----------------------------------------------------------------------------}

const
    
    MaxStackDepth = 40;                 { max of O(2^MaxStackDepth/2) keys }
    CutOff = 9;                         { smallest size subfile }


var
    left, right, i, j  :integer;
    v :integer;
    tmp :integer;
    done :boolean;

    StackPtr :0..MaxStackDepth;
    Stack :array[1..MaxStackDepth] of integer;
    

begin                 


A^[N+1] := MaxInt;
left := 0;                 
right := N;
StackPtr := 0;
done := (N <= CutOff);

while not done do
    begin                    
    tmp := A^[left+1];
    A^[left+1] := A^[(left+right) div 2];    
    A^[(left+right) div 2] := tmp;

    if A^[left+1] > A^[right] then
        begin
        tmp := A^[left+1];
        A^[left+1] := A^[right];
        A^[right] := tmp;
        end;

    if A^[left] > A^[right] then
        begin
        tmp := A^[left];
        A^[left] := A^[right];
        A^[right] := tmp;
        end;

    if A^[left+1] > A^[left] then
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
        repeat i := i+1 until A^[i] >= v;
        repeat j := j-1 until A^[j] <= v;
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
    if (A^[i] > A^[i+1]) then
        begin
        v := A^[i];
        j := i+1;
        repeat
            begin
            A^[j-1] := A^[j];
            j := j+1;
            end
        until (A^[j] >= v);
        A^[j-1] := v;
        end;
    end;
  
end;
   

procedure StringSort(N :integer; A :PStrArray; Fold :boolean); 
{------------------------------------------------------------------------------
{
{ Abstract:
{       Given an integer N and a pointer to an array [0..N+1] of strings, sort
{       [0..N] into ascending lexigocraphic order using QuickSort.  StringSort
{       is case sensitive (e.g. A < a) unless Fold is True.
{
{
{ Parameters:
{       N :integer      One less than the upper bound of the array.  It is
{                       the largest index of a valid key.
{
{       A :PStrArray    A pointer to an array [0..N+1] of strings.
{
{       Fold :boolean   If True then we fold to UpCase for comparisons.
{
{ Results:
{
{       The array from [0..N] is in ascending order
{
{ Side Effects:
{
{       The array is sorted and the N+1st element contains the DEL character
{
{------------------------------------------------------------------------------}
    
{------------------------------------------------------------------------------
{
{ Although it takes more space we have two separate StringSort routines
{ and choose among them depending upon the value of Fold.  This way we don't
{ need to lose time checking the value of Fold in the inner loop.
{
{-----------------------------------------------------------------------------}

const
    
    MaxStackDepth = 40;                 { max of O(2^MaxStackDepth/2) keys }
    CutOff = 9;                         { smallest size subfile }


var
    left, right, i, j  :integer;
    v :string;
    tmp :string;
    done :boolean;

    StackPtr :0..MaxStackDepth;
    Stack :array[1..MaxStackDepth] of integer;
    

begin                 

            
A^[N+1] := '';                  
left := 0;                 
right := N;
StackPtr := 0;
done := (N <= CutOff);

if Fold then                { Here if Fold }
begin
while not done do
    begin                    
    tmp := A^[left+1];
    A^[left+1] := A^[(left+right) div 2];    
    A^[(left+right) div 2] := tmp;

    if UpCase(A^[left+1]) > UpCase(A^[right]) then
        begin
        tmp := A^[left+1];
        A^[left+1] := A^[right];
        A^[right] := tmp;
        end;

    if UpCase(A^[left]) > UpCase(A^[right]) then
        begin
        tmp := A^[left];
        A^[left] := A^[right];
        A^[right] := tmp;
        end;

    if UpCase(A^[left+1]) > UpCase(A^[left]) then
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
        repeat i := i+1 until UpCase(A^[i]) >= UpCase(v);
        repeat j := j-1 until UpCase(A^[j]) <= UpCase(v);
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
    if (UpCase(A^[i]) > UpCase(A^[i+1])) then
        begin
        v := A^[i];
        j := i+1;
        repeat
            begin
            A^[j-1] := A^[j];
            j := j+1;
            end
        until (UpCase(A^[j]) >= UpCase(v));
        A^[j-1] := v;
        end;
    end;
end



else                    { Here if not Fold }


begin
while not done do
    begin                    
    tmp := A^[left+1];
    A^[left+1] := A^[(left+right) div 2];    
    A^[(left+right) div 2] := tmp;

    if A^[left+1] > A^[right] then
        begin
        tmp := A^[left+1];
        A^[left+1] := A^[right];
        A^[right] := tmp;
        end;

    if A^[left] > A^[right] then
        begin
        tmp := A^[left];
        A^[left] := A^[right];
        A^[right] := tmp;
        end;

    if A^[left+1] > A^[left] then
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
        repeat i := i+1 until A^[i] >= v;
        repeat j := j-1 until A^[j] <= v;
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
    if (A^[i] > A^[i+1]) then
        begin
        v := A^[i];
        j := i+1;
        repeat
            begin
            A^[j-1] := A^[j];
            j := j+1;
            end
        until (A^[j] >= v);
        A^[j-1] := v;
        end;
    end;
end

  
end.

