{Copyright (c) 2008 Ville Krumlinde

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

unit uSymTab;

interface

uses Classes,Contnrs,ZLog, Generics.Collections;

type
  TSymTabFunc = reference to procedure(const S : string; Item : TObject; Context : pointer);

  TSymbolTable = class
  strict private type
    TSymTabEntry = class
      Name : string;
      Value : TObject;
    end;
    TSymTabScope = TObjectDictionary<string,TSymTabEntry>;
  strict private
    Log : TLog;
    Scopes : TObjectList<TSymTabScope>;
    function CurrentScope : TSymTabScope;
    //procedure DumpToLog(const Header : string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name : string; Value : TObject);
    procedure AddPrevious(const Name : string; Value : TObject);
    function ScopeContains(const Name: string): boolean;
    function Contains(const Name : string) : boolean;
    function Lookup(const Name : string) : TObject;
    function MakeUnique(const Prefix : string) : string;
    procedure Remove(const Name : string);
    procedure ClearAll;
    procedure PushScope;
    procedure PopScope;
    procedure Iterate(F : TSymTabFunc; Context : pointer = nil);
  end;

implementation

uses SysUtils;

{ TSymbolTable }

constructor TSymbolTable.Create;
begin
  Log := GetLog(Self.ClassName);
  Scopes := TObjectList<TSymTabScope>.Create(True);
  ClearAll;
end;

function TSymbolTable.CurrentScope: TSymTabScope;
begin
  Result := Scopes[Scopes.Count-1];
end;

procedure TSymbolTable.PushScope;
var
  List : TSymTabScope;
begin
  List := TSymTabScope.Create([doOwnsValues]);
  Scopes.Add(List);
end;

procedure TSymbolTable.PopScope;
begin
  Scopes.Delete(Scopes.Count-1);
end;

procedure TSymbolTable.Add(const Name: string; Value: TObject);
var
  Entry : TSymTabEntry;
  Key : string;
begin
  Key := LowerCase(Name);
  Entry := TSymTabEntry.Create;
  Entry.Name := Name;
  Entry.Value := Value;
  CurrentScope.Add(Key,Entry);
end;

procedure TSymbolTable.AddPrevious(const Name: string; Value: TObject);
//Add to previous (global scope)
var
  Entry : TSymTabEntry;
  Key : string;
begin
  Key := LowerCase(Name);
  Entry := TSymTabEntry.Create;
  Entry.Name := Name;
  Entry.Value := Value;
  Scopes[Scopes.Count-2].Add(Key,Entry);
end;

procedure TSymbolTable.ClearAll;
begin
  Scopes.Clear;
  PushScope;
end;

function TSymbolTable.Contains(const Name: string): boolean;
var
  I : integer;
  List : TSymTabScope;
  Key : string;
begin
  Result := False;
  Key := LowerCase(Name);
  for I := Scopes.Count-1 downto 0 do
  begin
    List := Scopes[I];
    Result := List.ContainsKey(Key);
    if Result then
      Break;
  end;
end;

function TSymbolTable.ScopeContains(const Name: string): boolean;
var
  Key : string;
begin
  Key := LowerCase(Name);
  Result := CurrentScope.ContainsKey(Key);
end;

destructor TSymbolTable.Destroy;
begin
  Scopes.Free;
  inherited;
end;

function TSymbolTable.Lookup(const Name: string): TObject;
var
  I : integer;
  List : TSymTabScope;
  Key : string;
  Entry : TSymTabEntry;
begin
  Key := LowerCase(Name);
  Result := nil;
  for I := Scopes.Count-1 downto 0 do
  begin
    List := Scopes[I];
    if List.TryGetValue(Key,Entry) then
    begin
      Result := Entry.Value;
      Break;
    end;
  end;
end;

function TSymbolTable.MakeUnique(const Prefix: string): string;
var
  I : integer;
begin
  I := 0;
  repeat
    Inc(I);
    Result := Prefix + IntToStr(I);
  until not Contains(Result);
end;

procedure TSymbolTable.Remove(const Name: string);
var
  Key : string;
begin
  Key := LowerCase(Name);
  CurrentScope.Remove(Key);
end;

procedure TSymbolTable.Iterate(F: TSymTabFunc; Context: pointer);
var
  I : integer;
  List : TSymTabScope;
  Entry : TSymTabEntry;
begin
  for I := Scopes.Count-1 downto 0 do
  begin
    List := Scopes[I];
    for Entry in List.Values do
      F(Entry.Name,Entry.Value,Context);
  end;
end;

end.
