{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10129: mwStringHashList.pas 
{
{   Rev 1.0    2006-06-07 17:21:26  Supervisor
}
{
{   Rev 1.0    2002-09-23 18:47:16  Supervisor
}
{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License Version
1.1 (the "License"); you may not use this file except in compliance with the
License. You may obtain a copy of the License at
http://www.mozilla.org/NPL/NPL-1_1Final.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: mwStringHashList.pas, released December 18, 2000.

The Initial Developer of the Original Code is Martin Waldenburg
(Martin.Waldenburg@T-Online.de).
Portions created by Martin Waldenburg are Copyright (C) 2000 Martin Waldenburg.
All Rights Reserved.

Contributor(s): ___________________.

Last Modified: 1/2/2001
Current Version: 2.0

Notes: This is a very fast Hash list for strings.

Known Issues:
-----------------------------------------------------------------------------}
{$WARN UNIT_PLATFORM OFF}
{$WARN SYMBOL_PLATFORM OFF}
{.$WARN UNSAFE_CODE OFF}
{.$WARN UNSAFE_TYPE OFF}
{.$WARN UNSAFE_CAST OFF}
{$R-}
unit mwStringHashList;

interface

uses Classes, SysUtils, Windows;

var
  mwHashTable: array[#0..#255] of byte;
  mwInsensitiveHashTable: array[#0..#255] of Byte;

type
  TmwStringHash = function(const aString: string): Integer;
  TmwStringHashCompare = function(const Str1: string; const Str2: string): Boolean;

  TmwHashWord = class
    S: string;
    Id: Integer;
    ExID: Integer;
    constructor Create(aString: string; anId, anExId: Integer);
  end;

  PHashPointerList = ^THashPointerList;
  THashPointerList = array[1..1] of TObject;

  TmwBaseStringHashList = class(TObject)
    FList: PHashPointerList;
    fCapacity: Integer;
  protected
    fHash: TmwStringHash;
    function Get(Index: Integer): Pointer;
    procedure Put(Index: Integer; Item: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
  public
    destructor Destroy; override;
    procedure Clear;
    property Capacity: Integer read fCapacity;
    property Items[Index: Integer]: Pointer read Get write Put; default;
  end;

  TmwHashStrings = class(TmwBaseStringHashList)
  public
    procedure AddString(aString: string; anId, anExId: Integer);
  end;

  TmwHashItems = class(TmwBaseStringHashList)
  public
    constructor Create(aHash: TmwStringHash);
    procedure AddString(aString: string; anId, anExId: Integer);
  end;

  TmwStringHashList = class(TmwBaseStringHashList)
  private
    fSecondaryHash: TmwStringHash;
    fCompare: TmwStringHashCompare;
  public
    constructor Create(Primary, Secondary: TmwStringHash; aCompare: TmwStringHashCompare);
    procedure AddString(aString: string; anId, anExId: Integer);
    function Hash(const S: string; var anId: Integer; var anExId: Integer): Boolean;
    function HashEX(const S: string; var anId: Integer; var anExId: Integer; HashValue: Integer): Boolean;
  end;

function CrcHash(const aString: string): integer;
function ICrcHash(const aString: string): integer;
function SmallCrcHash(const aString: string): integer;
function ISmallCrcHash(const aString: string): integer;
function TinyHash(const aString: string): Integer;
function ITinyHash(const aString: string): Integer;
function HashCompare(const Str1: string; const Str2: string): Boolean;
function IHashCompare(const Str1: string; const Str2: string): Boolean;

function HashSecondaryOne(const aString: string): Integer;
function HashSecondaryTwo(const aString: string): Integer;

procedure InitTables;

implementation

procedure InitTables;
var
  I, K: Char;
  Temp: Byte;
begin
  for I := #0 to #255 do
  begin
    mwHashTable[I] := Ord(I);
  end;
  RandSeed := 255;
  for I := #1 to #255 do
  begin
    repeat
      K := Char(Random(255));
    until K <> #0;
    Temp := mwHashTable[I];
    mwHashTable[I] := mwHashTable[K];
    mwHashTable[K] := Temp;
  end;
  for I := #0 to #255 do
    mwInsensitiveHashTable[I] := mwHashTable[AnsiLowerCase(string(I))[1]];
end;

{ based on a Hasch function by Cyrille de Brebisson }

function CrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor mwHashTable[aString[I]]) and $F) * $1000);
    Result := (Result shr 4) xor (((Result xor (ord(mwHashTable[aString[I]]) shr 4)) and $F) * $1000);
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function ICrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor mwInsensitiveHashTable[aString[I]]) and $F) * $1000);
    Result := (Result shr 4) xor (((Result xor (ord(mwInsensitiveHashTable[aString[I]]) shr 4)) and $F) * $1000);
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function SmallCrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor mwHashTable[aString[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (ord(mwHashTable[aString[I]]) shr 4)) and $F) * $80);
    if I = 3 then break;
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function ISmallCrcHash(const aString: string): integer;
var
  I: Integer;
begin
  Result := 0;
  for i := 1 to length(aString) do
  begin
    Result := (Result shr 4) xor (((Result xor mwInsensitiveHashTable[aString[I]]) and $F) * $80);
    Result := (Result shr 4) xor (((Result xor (ord(mwInsensitiveHashTable[aString[I]]) shr 4)) and $F) * $80);
    if I = 3 then break;
  end;
  if Result = 0 then Result := Length(aString) mod 8 + 1;
end;

function TinyHash(const aString: string): Integer;
var
  I: Integer;
begin
  Result := Length(aString);
  for i := 1 to length(aString) do
  begin
    inc(Result, mwHashTable[aString[I]]);
    Result := Result mod 128 + 1;
    if I = 2 then break;
  end;
end;

function ITinyHash(const aString: string): Integer;
var
  I: Integer;
begin
  Result := Length(aString);
  for i := 1 to length(aString) do
  begin
    inc(Result, mwInsensitiveHashTable[aString[I]]);
    Result := Result mod 128 + 1;
    if I = 2 then break;
  end;
end;

function HashCompare(const Str1: string; const Str2: string): Boolean;
var
  I: Integer;
begin
  if Length(Str1) <> Length(Str2) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 1 to Length(Str1) do
    if Str1[I] <> Str2[I] then
    begin
      Result := False;
      Exit;
    end;
end;

function IHashCompare(const Str1: string; const Str2: string): Boolean;
var
  I: Integer;
begin
  if Length(Str1) <> Length(Str2) then
  begin
    Result := False;
    Exit;
  end;
  Result := True;
  for I := 1 to Length(Str1) do
    if mwInsensitiveHashTable[Str1[I]] <> mwInsensitiveHashTable[Str2[I]] then
    begin
      Result := False;
      Exit;
    end;
end;

function HashSecondaryOne(const aString: string): Integer;
begin
  Result := Length(aString);
  inc(Result, mwInsensitiveHashTable[aString[Length(aString)]]);
  Result := Result mod 16 + 1;
  inc(Result, mwInsensitiveHashTable[aString[1]]);
  Result := Result mod 16 + 1;
end;

function HashSecondaryTwo(const aString: string): Integer;
var
  I: Integer;
begin
  Result := Length(aString);
  for I := Length(aString) downto 1 do
  begin
    inc(Result, mwInsensitiveHashTable[aString[I]]);
    Result := Result mod 32 + 1;
  end;
end;

{ TmwHashString }

constructor TmwHashWord.Create(aString: string; anId, anExId: Integer);
begin
  inherited Create;
  S := aString;
  Id := anId;
  ExID := anExId;
end;

{ TmwBaseStringHashList }

procedure TmwBaseStringHashList.Clear;
var
  I: Integer;
begin
  for I := 1 to fCapacity do
    if fList[I] <> nil then
      fList[I].Free;
  ReallocMem(FList, 0);
  fCapacity := 0;
end;

destructor TmwBaseStringHashList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TmwBaseStringHashList.Get(Index: Integer): Pointer;
begin
  Result := nil;
  if (Index > 0) and (Index <= fCapacity) then
    Result := fList[Index];
end;

procedure TmwBaseStringHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index > 0) and (Index <= fCapacity) then
    fList[Index] := Item;
end;

procedure TmwBaseStringHashList.SetCapacity(NewCapacity: Integer);
var
  I, OldCapacity: Integer;
begin
  if NewCapacity > fCapacity then
  begin
    ReallocMem(FList, (NewCapacity) * SizeOf(Pointer));
    OldCapacity := fCapacity;
    FCapacity := NewCapacity;
    for I := OldCapacity + 1 to NewCapacity do Items[I] := nil;
  end;
end;

{ TmwHashStrings }

procedure TmwHashStrings.AddString(aString: string; anId, anExId: Integer);
begin
  SetCapacity(Capacity + 1);
  fList[Capacity] := TmwHashWord.Create(aString, anId, anExId);
end;

{ TmwHashItems }

procedure TmwHashItems.AddString(aString: string; anId, anExId: Integer);
var
  HashWord: TmwHashWord;
  HashStrings: TmwHashStrings;
  HashVal: Integer;
begin
  HashVal := fHash(aString);
  SetCapacity(HashVal);
  if Items[HashVal] = nil then
  begin
    Items[HashVal] := TmwHashWord.Create(aString, anId, anExId);
  end else
    if fList[HashVal] is TmwHashStrings then
    begin
      TmwHashStrings(Items[HashVal]).AddString(aString, anId, anExId);
    end else
    begin
      HashWord := Items[HashVal];
      HashStrings := TmwHashStrings.Create;
      Items[HashVal] := HashStrings;
      HashStrings.AddString(HashWord.S, HashWord.Id, HashWord.ExId);
      HashWord.Free;
      HashStrings.AddString(aString, anId, anExId)
    end;
end;

constructor TmwHashItems.Create(aHash: TmwStringHash);
begin
  inherited Create;
  fHash := aHash;
end;

{ TmwStringHashList }

constructor TmwStringHashList.Create(Primary, Secondary: TmwStringHash; aCompare: TmwStringHashCompare);
begin
  inherited Create;
  fHash := Primary;
  fSecondaryHash := Secondary;
  fCompare := aCompare;
end;

procedure TmwStringHashList.AddString(aString: string; anId, anExId: Integer);
var
  HashWord: TmwHashWord;
  HashValue: Integer;
  HashItems: TmwHashItems;
begin
  HashValue := fHash(aString);
  if HashValue >= fCapacity then SetCapacity(HashValue);
  if Items[HashValue] = nil then
  begin
    Items[HashValue] := TmwHashWord.Create(aString, anId, anExId);
  end else
    if fList[HashValue] is TmwHashItems then
    begin
      TmwHashItems(Items[HashValue]).AddString(aString, anId, anExId);
    end else
    begin
      HashWord := Items[HashValue];
      HashItems := TmwHashItems.Create(fSecondaryHash);
      Items[HashValue] := HashItems;
      HashItems.AddString(HashWord.S, HashWord.Id, HashWord.ExId);
      HashWord.Free;
      HashItems.AddString(aString, anId, anExId);
    end;
end;

function TmwStringHashList.Hash(const S: string; var anId: Integer; var anExId: Integer): Boolean;
begin
  Result := HashEX(S, anId, anExId, fHash(S));
end;

function TmwStringHashList.HashEX(const S: string; var anId: Integer; var anExId: Integer; HashValue: Integer): Boolean;
var
  Temp: TObject;
  Hashword: TmwHashWord;
  HashItems: TmwHashItems;
  I, ItemHash: Integer;
begin
  Result := False;
  anID := -1;
  anExId := -1;
  if HashValue < 1 then Exit;
  if HashValue > Capacity then Exit;
  if Items[HashValue] <> nil then
  begin
    if fList[HashValue] is TmwHashWord then
    begin
      Hashword := Items[HashValue];
      Result := fCompare(HashWord.S, S);
      if Result then
      begin
        anID := HashWord.Id;
        anExId := HashWord.ExID;
      end;
    end else
    begin
      HashItems := Items[HashValue];
      ItemHash := HashItems.fHash(S);
      if ItemHash > HashItems.Capacity then Exit;
      Temp := HashItems[ItemHash];
      if Temp <> nil then
        if Temp is TmwHashWord then
        begin
          Result := fCompare(TmwHashWord(Temp).S, S);
          if Result then
          begin
            anID := TmwHashWord(Temp).Id;
            anExId := TmwHashWord(Temp).ExID;
          end;
        end else
          for I := 1 to TmwHashStrings(Temp).Capacity do
          begin
            HashWord := TmwHashStrings(Temp)[I];
            Result := fCompare(HashWord.S, S);
            if Result then
            begin
              anID := HashWord.Id;
              anExId := HashWord.ExID;
              exit;
            end;
          end;
    end;
  end;
end;

initialization
  InitTables;
{$R+}
end.

