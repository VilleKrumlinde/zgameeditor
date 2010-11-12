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

uses Classes,Contnrs,ZLog;

type
  TSymTabFunc = reference to procedure(const S : string; Item : TObject; Context : pointer);

  TSymbolTable = class
  private
    Log : TLog;
    Scopes : TObjectList;
    function CurrentScope : TStringList;
    //procedure DumpToLog(const Header : string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const Name : string; Value : TObject);
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
  Scopes := TObjectList.Create(True);
  ClearAll;
end;

function TSymbolTable.CurrentScope: TStringList;
begin
  Result := TStringList(Scopes[Scopes.Count-1]);
end;

procedure TSymbolTable.PushScope;
var
  List : TStringList;
begin
//  DumpToLog('pushscope');
  List := TStringList.Create;
  List.Sorted := True;
  List.Duplicates := dupIgnore;
  List.CaseSensitive := False;
  Scopes.Add(List);
end;

procedure TSymbolTable.PopScope;
begin
//  DumpToLog('popscope');
  Assert(Scopes.Count>1);
  Scopes.Delete(Scopes.Count-1);
end;

procedure TSymbolTable.Add(const Name: string; Value: TObject);
begin
  Assert(not ScopeContains(Name),'Symboltable current scope already contains: ' + Name);
  CurrentScope.AddObject(Name,Value);
end;

procedure TSymbolTable.ClearAll;
begin
  Scopes.Clear;
  PushScope;
end;

function TSymbolTable.Contains(const Name: string): boolean;
var
  I : integer;
  List : TStringList;
begin
  Result := False;
  for I := Scopes.Count-1 downto 0 do
  begin
    List := TStringList(Scopes[I]);
    Result := List.IndexOf(Name)>-1;
    if Result then
      Break;
  end;
end;

function TSymbolTable.ScopeContains(const Name: string): boolean;
begin
  Result := CurrentScope.IndexOf(Name)>-1;
end;

destructor TSymbolTable.Destroy;
begin
  Scopes.Free;
  inherited;
end;

{procedure TSymbolTable.DumpToLog(const Header : string);
var
  I : integer;
  List : TStringList;
begin
  Log.Write(Header);
  for I := 0 to Scopes.Count-1 do
  begin
    List := TStringList(Scopes[I]);
    Log.Write( IntToStr(I) + ': ' + List.CommaText );
  end;
end;}

function TSymbolTable.Lookup(const Name: string): TObject;
var
  I,J : integer;
  List : TStringList;
begin
  Result := nil;
  for I := Scopes.Count-1 downto 0 do
  begin
    List := TStringList(Scopes[I]);
    J := List.IndexOf(Name);
    if J>-1 then
    begin
      Result := List.Objects[J];
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
begin
//  DumpToLog('before remove: ' + Name);
  Assert(ScopeContains(Name));
  CurrentScope.Delete(CurrentScope.IndexOf(Name));
//  DumpToLog('after remove: ' + Name);
end;

procedure TSymbolTable.Iterate(F: TSymTabFunc; Context: pointer);
var
  I,J : integer;
  List : TStringList;
begin
  for I := Scopes.Count-1 downto 0 do
  begin
    List := TStringList(Scopes[I]);
    for J := 0 to List.Count - 1 do
      F(List[J],List.Objects[J],Context);
  end;
end;

end.
