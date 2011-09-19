{ Copyright (C) 2009, Serge Voloshenyuk
  
  This file is Free Software and part of DCocoR
  
  It is licensed under the following three licenses as alternatives:
    1. GNU Lesser General Public License (LGPL) V2.1 or any newer version
    2. GNU General Public License (GPL) V2 or any newer version
    3. Apache License, V2.0 or any newer version
  
  You may not use this file except in compliance with at least one of
  the above three licenses.
  
  See LICENSE.txt at the top of this package for the
  complete terms and further detail along with the license texts for
  the licenses in COPYING-LIB.txt, COPYING.txt and LICENSE-2.0.txt respectively.
} 

unit CocoSets;

interface
uses SysUtils;

type
  TIterateProc = procedure(data: Pointer; Index: Integer);

  TCocoSet = class
  private
    FSize: Integer;
    FBits: Pointer;
    procedure Error;
    procedure SetSize(Value: Integer);
    procedure SetBit(Index: Integer; Value: Boolean);
    function  GetBit(Index: Integer): Boolean;
    procedure SyncSize(other: TCocoSet);
  public
    class function Intersect(a,b: TCocoSet): TCocoSet;
    constructor Create(aSize: Integer=0); overload;
    constructor Create(vals: array of Integer); overload;
    destructor Destroy; override;

    procedure Fill;
    procedure Assign(other: TCocoSet);
    function Clone: TCocoSet;
    procedure Clear;

    function Empty: Boolean;
    function Equals(other: TCocoSet): Boolean; reintroduce;
    function Intersects(other: TCocoSet): Boolean;
    function Includes(subset: TCocoSet): Boolean;

    procedure Unite(addset: TCocoSet);
    procedure Subtract(aSet: TCocoSet);

    function IsOne(var UniqIndex: Integer): Boolean;
    procedure IterateTrue(proc: TIterateProc; data: Pointer);

    function Count: Integer;
    property Bits[Index: Integer]: Boolean read GetBit write SetBit; default;
    property Size: Integer read FSize write SetSize;
  end;

  ESetError = class(Exception);

resourcestring
  SSetIndexError = 'Set index out of range';


implementation

const
  BitsPerInt = SizeOf(Integer) * 8;

type
  TBitEnum = 0..BitsPerInt - 1;
  TBitSet = set of TBitEnum;

  PBitArray = ^TBitArray;
  TBitArray = array[0..4096] of TBitSet;

{ TSet }

constructor TCocoSet.Create(aSize: Integer);
begin
  Size := aSize;
end;

procedure TCocoSet.Assign(other: TCocoSet);
var I,E: Integer;
begin
  if other=nil then
  begin Clear; Exit; end;

  SyncSize(other);
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
   PBitArray(FBits)^[I] := PBitArray(other.FBits)^[I];
end;

function TCocoSet.Clone: TCocoSet;
begin
  if Self=nil then
    Result := nil
  else begin
    Result := TCocoSet.Create(Size);
    Result.Assign(Self);
  end;
end;

constructor TCocoSet.Create(vals: array of Integer);
var I,mI: Integer;
begin
  if Length(vals)=0 then Exit;
  mI := vals[0];
  for I := 1 to High(vals) do
  if vals[I]>mI then mI := vals[I];
  Size := mI+1;
  for I := 0 to High(vals) do
    Bits[vals[I]] := True;
end;

destructor TCocoSet.Destroy;
begin
  setSize(0);
  inherited;
end;

procedure TCocoSet.Error;
begin
  raise ESetError.CreateRes(@SSetIndexError);
end;

procedure TCocoSet.SyncSize(other: TCocoSet);
begin
  if Size<other.Size then Size := other.Size
  else if Size>other.Size then other.Size := Size;
end;

procedure TCocoSet.Fill;
var I,E: Integer;
begin
  E := Size div BitsPerInt;
  for I := 0 to E-1 do
    PBitArray(FBits)^[I] := [0..BitsPerInt - 1];
  for I := E*BitsPerInt to Size-1 do
    Bits[I] := True;
end;

procedure TCocoSet.SetSize(Value: Integer);
var
  NewMem: Pointer;
  NewMemSize: Integer;
  OldMemSize: Integer;

  function Min(X, Y: Integer): Integer;
  begin
    Result := X;
    if X > Y then Result := Y;
  end;

begin
  if Value <> Size then
  begin
    if Value < 0 then Error;
    NewMemSize := ((Value + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    OldMemSize := ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer);
    if NewMemSize <> OldMemSize then
    begin
      NewMem := nil;
      if NewMemSize <> 0 then
      begin
        GetMem(NewMem, NewMemSize);
        FillChar(NewMem^, NewMemSize, 0);
      end;
      if OldMemSize <> 0 then
      begin
        if NewMem <> nil then
          Move(FBits^, NewMem^, Min(OldMemSize, NewMemSize));
        FreeMem(FBits, OldMemSize);
      end;
      FBits := NewMem;
    end;
    FSize := Value;
  end;
end;

function TCocoSet.GetBit(Index: Integer): Boolean; assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     @@1
        MOV     EAX,[EAX].FBits
        BT      [EAX],Index
        SBB     EAX,EAX
        AND     EAX,1
        RET
@@1:    MOV     EAX,0
end;

procedure TCocoSet.SetBit(Index: Integer; Value: Boolean);  assembler;
asm
        CMP     Index,[EAX].FSize
        JAE     @@Size

@@1:    MOV     EAX,[EAX].FBits
        OR      Value,Value
        JZ      @@2
        BTS     [EAX],Index
        RET

@@2:    BTR     [EAX],Index
        RET

@@Size: CMP     Index,0
        JL      TCocoSet.Error
        PUSH    Self
        PUSH    Index
        PUSH    ECX {Value}
        INC     Index
        CALL    TCocoSet.SetSize
        POP     ECX {Value}
        POP     Index
        POP     Self
        JMP     @@1
end;

procedure TCocoSet.Clear;
begin
  FillChar(FBits^, ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer), 0);
end;

function TCocoSet.Empty: Boolean;
var I,E: Integer;
begin
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if PBitArray(FBits)^[I]<>[] then
    begin
      Result := False; Exit;
    end;
  Result := True;
end;

function TCocoSet.Equals(other: TCocoSet): Boolean;
begin
  SyncSize(other);
  Result := CompareMem(FBits, other.FBits,
    ((Size + BitsPerInt - 1) div BitsPerInt) * SizeOf(Integer));
end;

function TCocoSet.Includes(subset: TCocoSet): Boolean;
var I,E: Integer;
begin
  SyncSize(subset);
  E := (subset.Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if (PBitArray(FBits)^[I] * PBitArray(subset.FBits)^[I])<>PBitArray(subset.FBits)^[I] then
    begin
      Result := False; Exit;
    end;
  Result := True;
end;

class function TCocoSet.Intersect(a, b: TCocoSet): TCocoSet;
var I,E: Integer;
begin
  a.SyncSize(b);
  Result := TCocoSet.Create(a.Size);

  E := (a.Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
   PBitArray(Result.FBits)^[I] := PBitArray(a.FBits)^[I] * PBitArray(b.FBits)^[I];
end;

function TCocoSet.Intersects(other: TCocoSet): Boolean;
var I,E: Integer;
begin
  SyncSize(other);
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
   if (PBitArray(FBits)^[I] * PBitArray(other.FBits)^[I])<>[] then
   begin
     Result := True; Exit;
   end;
  Result := False;
end;

procedure TCocoSet.IterateTrue(proc: TIterateProc; data: Pointer);
var I,J,E: Integer;
begin
  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    if PBitArray(FBits)^[I]<>[] then
    for J := I*BitsPerInt to I*BitsPerInt + BitsPerInt - 1 do
    begin
      if J>=Size then Break;
      if Bits[J] then
        proc(data,J);
    end;
end;

procedure _FindOne(data: Pointer; Index: Integer);
begin
  if PInteger(data)^=-2 then
    PInteger(data)^ := Index
  else PInteger(data)^ := -1;
end;

function TCocoSet.IsOne(var UniqIndex: Integer): Boolean;
var ind: Integer;
begin
  ind := -2;
  IterateTrue(_FindOne,@ind);
  Result := ind>=0;
  if Result then UniqIndex := ind;
end;

procedure TCocoSet.Unite(addset: TCocoSet);
var I,E: Integer;
begin
  if addset=nil then Exit;
  SyncSize(addset);

  E := (Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
    PBitArray(FBits)^[I] := PBitArray(FBits)^[I] + PBitArray(addset.FBits)^[I];
end;

procedure TCocoSet.Subtract(aSet: TCocoSet);
var I,E: Integer;
begin
  SyncSize(aSet);
  E := (aSet.Size + BitsPerInt - 1) div BitsPerInt - 1;
  for I := 0 to E do
   PBitArray(FBits)^[I] := PBitArray(FBits)^[I] - PBitArray(aSet.FBits)^[I];
end;

procedure _Count(data: Pointer; Index: Integer);
begin
  Inc(PInteger(data)^);
end;

function TCocoSet.Count: Integer;
begin
  Result := 0;
  IterateTrue(_Count,@Result);
end;

end.
