{---------------------------------------------------------------------
{ Copyright (C) 1983 - Perq Systems Corporation.
{---------------------------------------------------------------------}
module UserPass;

{-------------------------------------------------------------------------
{
{ Abstract:
{    This module provides facilities for dealing with the password
{    and accounts file for PERQ.  The login and protection facilities
{    for Perq provide a very simple user validification.  This system
{    is NOT completly secure.
{
{ Written by: Don Scelza
{
{ Copyright (C) Three Rivers Computer Corporation,  1981, 1982, 1983.
{
{-------------------------------------------------------------------------}


{{$Version V1.4}
{----------------------------------------------------------------------
{ Change Log:
{ 
{ 19 Feb 82  V1.4  WJHansen
{ fixed so FindUser won't find user in records with InUse false
{
{  6-Jun-81  V1.3  Brad Myers
{ Fixed so System.Users always in root directory: >System.Users
{ }

{ 29-Apr-81  V1.2  Son Scelza
{ Added code and types for profile and remove user.
{ }

{ 9-Mar-81   V1.1  Don Scelza
{ Changed the max number of users.  This will allow the entire
{ file to fit into a single disk block.
{ }

{  5-Mar-81  V1.0  Don Scelza
{ Created UserPass
{ }
{-----------------------------------------------------------------------}


{********************} Exports {********************}

type IDType  = 0..255;

    PassType =  ^Integer;              { a two word value }
    
    UserRecord = packed record
            InUse: boolean;                { is this entry in use. }
            Name: String[31];              { Name of the user }
            UserID: IDType;                { The user ID of the user. }
            GroupID: IDType;               { The group ID of the user. }
            EncryptPass: PassType;         { The encrypted password. }
            Profile: String;               { Path name of the profile file. }
        end;


function FindUser(UserName: String;  var UserRec: UserRecord): Boolean;
function ValidUser( UserName, Password: String;  var UserRec: UserRecord): Boolean;
function AddUser(UserName, Password: String;  Group: IDType;  
                 ProPath: String): Boolean;
procedure NewUserFile;
procedure ListUsers;
function RemoveUser(UserName: String): boolean;

const PassFile = '>System.Users';

const MaxUsers = 10;

type Users = array[0..MaxUsers] of UserRecord;




{********************} Private {********************}




var UsrFile: file of Users;
    CurIndex: Integer;
    TmpUser: Users;

imports CmdParse from CmdParse;
imports Arith from Arith;


function Encrypt(Password: String): PassType;
{---------------------------------------------------------------------------
{
{ Abstract:
{    This function is used to encrypt a user password.
{
{ Parameters:
{    Password is the user password that we are to encrypt.
{
{ Results:
{    This function will return the encrypted form of the password.
{
{----------------------------------------------------------------------------}
  var HackRecord: record
          case Integer of
          1: ( StrType: String);
          2: ( RetType: PassType);
          3: ( First, Second: Integer);
          4: ( LongType: FSBit32)
          end;
      Tmp1, Tmp2: FSBit32;
      Int1, Int2: Integer;
      
    begin
    HackRecord.StrType := Password;
    Tmp1 := HackRecord.LongType;
    Int1 := HackRecord.First;
    Int2 := HackRecord.Second;
    HackRecord.First := Int2;
    HackRecord.Second := Int1;
    Tmp2 := HackRecord.LongType;
    HackRecord.LongType := DoubleAdd(Tmp1, Tmp2);
    Encrypt := HackRecord.RetType;
    end;


function FindUser(UserName: String;  var UserRec: UserRecord): Boolean;
{----------------------------------------------------------------------------
{
{ Abstract:
{    This function is used to see if a user exists in the user file.
{
{ Parameters:
{    UserName is the name of the user that we are looking for.
{
{    UserRec is a var parameter that is used to return the information
{    about the user UserName if he is in the file.
{
{ Results:
{    This procedure will return true if the user UserName was in the
{    user file.  It will return False otherwise.
{
{---------------------------------------------------------------------------}
  var Str1: String;
      Indx: Integer;
    begin
    reset(UsrFile, PassFile);
    CnvUpper(UserName);
    for Indx := 0 to MaxUsers do
        begin
        Str1 := UsrFile^[Indx].Name;
        CnvUpper(Str1);
        if (Str1 = UserName) and UsrFile^[Indx].InUse then
            begin
            UserRec := UsrFile^[Indx];
            CurIndex := Indx;
            FindUser := true;
            close(UsrFile);
            exit(FindUser);
            end;
        end;
    close(UsrFile);
    FindUser := false;
    end;


function ValidUser( UserName, Password: String;  var UserRec: UserRecord): Boolean;
{---------------------------------------------------------------------------
{
{ Abstract:
{    This function is used to see if a user name and password match.
{
{ Parameters:
{    Username is the name of the user that we want to check.
{
{    Password is the password for the user.
{
{    UserRec will be filled with the user information if the user name
{    and password match.
{
{ Results:
{    If the password is valid for the user then return true.  Otherwise
{    return false.
{
{ Side Effects:
{    This function will change the file PassFile.
{
{-------------------------------------------------------------------------}
  var User: UserRecord;
    begin
    ValidUser := false;
    if FindUser(UserName, User) then
        if User.EncryptPass = Encrypt(Password) then
            begin
            UserRec := User;
            ValidUser := true;
            end;
    end;




function AddUser(UserName, Password: String;  Group: IDType;  
                 ProPath: String): Boolean;
{---------------------------------------------------------------------------
{
{ Abstract:
{    This function is used to add a new user to the user file or change
{    the parameters of an already existing user.
{
{ Parameters:
{    Username is the name of the user that we want to add or change.
{
{    Password is the password for the user.
{
{    Group is the group number for the new user.
{
{    ProPath is the path name of the profile file for this user.
{
{ Results:
{    If the user could be added or changed then return true.  Otherwise
{    return false.
{
{ Side Effects:
{    This function will change the file PassFile.
{
{-------------------------------------------------------------------------}
  var Str: String;
      Current, Found: Boolean;
      Indx: Integer;
      NewUser: UserRecord;
    begin
    Current := true;
    if not FindUser(UserName, NewUser) then
        begin
        Current := false;
        Indx := 0;
        Found := false;
        repeat
            if not UsrFile^[Indx].InUse then 
                begin
                CurIndex := Indx;
                Found := true;
                end;
            Indx := Indx + 1;
            until found or (Indx = MaxUsers);
        end
    else
        Found := true;
        
    if not Found then
        begin
        AddUser := false;
        exit(AddUser);
        end;
        
    UsrFile^[CurIndex].Name := UserName;
    UsrFile^[CurIndex].UserID := CurIndex;
    UsrFile^[CurIndex].GroupID := Group;
    UsrFile^[CurIndex].EncryptPass := Encrypt(Password);
    UsrFile^[CurIndex].Profile := ProPath;
    UsrFile^[CurIndex].InUse := true;
    
    TmpUser := UsrFile^;
    close(UsrFile);
    rewrite(UsrFile, PassFile);
    UsrFile^ := TmpUser;
    put(UsrFile);
    close(UsrFile);
    
    AddUser := true;
    end;


procedure NewUserFile;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to create a new user file.
{
{ Side Effects:
{    This procedure will create a new file.  It will destroy any information
{    in the current file.
{
{------------------------------------------------------------------------}
  var Indx: Integer;
    begin
    For Indx := 0 to MaxUsers do
        begin
        TmpUser[Indx].InUse := false;
        TmpUser[Indx].UserID := Indx;
        TmpUser[Indx].Name := '';
        TmpUser[Indx].Profile := '';
        end;
    rewrite(UsrFile, PassFile);
    UsrFile^ := TmpUser;
    Put(UsrFile);
    close(UsrFile);
    end;


procedure ListUsers;
{---------------------------------------------------------------------------
{
{ Abstract:
{    This procedure is used to supply a list of the valid users.
{---------------------------------------------------------------------------}
  var I: Integer;
    begin
    reset(UsrFile, PassFile);
    for I := 0 to MaxUsers do
        begin
        if UsrFile^[I].InUse then
            begin
            writeln('    ', UsrFile^[I].Name, '   ', UsrFile^[I].Profile);
            end;
        end;
    end;


function RemoveUser(UserName: String): boolean;
{-----------------------------------------------------------------------
{
{ Abstract:
{    This procedure is usd to remove a user from the list of valid users.
{
{ Parameters:
{    UserName is the name of the user that is to be removed.
{
{ Results:
{    If the user could be removed the return true.  Otherwise return
{    false.
{
{----------------------------------------------------------------------}
  var RemUser: UserRecord;
    begin
    RemoveUser := false;
    if not FindUser(UserName, RemUser) then
        begin
        exit(RemoveUser);
        end;        
           
    UsrFile^[CurIndex].InUse := false;
    
    TmpUser := UsrFile^;
    close(UsrFile);
    rewrite(UsrFile, PassFile);
    UsrFile^ := TmpUser;
    put(UsrFile);
    close(UsrFile);
    
    RemoveUser := true;
    end.

