unit unitResourceRCData;

interface

uses Windows, Classes, SysUtils, Contnrs, unitResourceDetails;

type
TRCDataResourceDetails = class (TResourceDetails)
public
  class function GetBaseType : ansistring; override;
end;

TRCDataDescriptionResourceDetails = class (TRCDataResourceDetails)
private
  function GetDescription: ansistring;
  procedure SetDescription(const Value: ansistring);
protected
  class function SupportsRCData (const AName : ansistring; Size : Integer; data : Pointer) : Boolean; override;
public
  property Description : ansistring read GetDescription write SetDescription;
end;

TRCDataFormResourceDetails = class (TRCDataResourceDetails)
  private
  function GetText: ansistring;
  procedure SetText(const Value: ansistring);
protected
  class function SupportsRCData (const AName : ansistring; Size : Integer; data : Pointer) : Boolean; override;
public
  property Text : ansistring read GetText write SetText;
end;

TPackageEnvironment = (pePreV4, peUndefine, peBCB, peDelphi);
TModuleType = (mtEXE, mtPackageDLL, mtLibraryDLL, mtUndefine);

TRCDataPackagesResourceDetails = class (TRCDataResourceDetails)
private
  fRequiresList : TStrings;
  fContainsList : TStrings;
  fFlags : DWORD;

  function GetRequiresCount: Integer;
  function GetRequires(idx : Integer): ansistring;
  function GetContainsCount: Integer;
  function GetContains(idx: Integer): ansistring;
  function GetContainsFlag(idx: Integer): Byte;

  procedure DecodeData;
  function GetCheckForDuplicates: Boolean;
  function GetDesignTimeOnly: Boolean;
  function GetEnvironment: TPackageEnvironment;
  function GetModuleType: TModuleType;
  function GetNeverBuild: Boolean;
  function GetRunTimeOnly: Boolean;
protected
  class function SupportsRCData (const AName : ansistring; Size : Integer; data : Pointer) : Boolean; override;
public
  destructor Destroy; override;
  procedure ChangeData (newData : TMemoryStream); override;
  property RequiresCount : Integer read GetRequiresCount;
  property Requires [idx : Integer] : ansistring read GetRequires;
  property ContainsCount : Integer read GetContainsCount;
  property Contains [idx : Integer] : ansistring read GetContains;
  property ContainsFlag [idx : Integer] : Byte read GetContainsFlag;

  property NeverBuild : Boolean read GetNeverBuild;
  property DesignTimeOnly : Boolean read GetDesignTimeOnly;
  property RunTimeOnly : Boolean read GetRunTimeOnly;
  property CheckForDuplicates : Boolean read GetCheckForDuplicates;
  property Environment : TPackageEnvironment read GetEnvironment;
  property ModuleType : TModuleType read GetModuleType;


end;

implementation

uses AnsiStrings;

type
  TPkgName = packed record
    HashCode : Byte;
    Name : array [0..255] of Char;
  end;
  PPkgName = ^TPkgName;

  { PackageUnitFlags:
    bit      meaning
    -----------------------------------------------------------------------------------------
    0      | main unit
    1      | package unit (dpk source)
    2      | $WEAKPACKAGEUNIT unit
    3      | original containment of $WEAKPACKAGEUNIT (package into which it was compiled)
    4      | implicitly imported
    5..7   | reserved
  }

  PUnitName = ^TUnitName;
  TUnitName = packed record
    Flags : Byte;
    HashCode: Byte;
    Name: array[0..255] of Char;
  end;

{ TRCDataResourceDetails }

class function TRCDataResourceDetails.GetBaseType: ansistring;
begin
  result := AnsiString( IntToStr (Integer (RT_RCDATA)) );
end;

{ TRCDataDescriptionResourceDetails }

function TRCDataDescriptionResourceDetails.GetDescription: ansistring;
begin
  Result := AnsiString(PWideChar (data.Memory));
end;

procedure TRCDataDescriptionResourceDetails.SetDescription(
  const Value: ansistring);
var
  ws : WideString;
begin
  data.Size := (Length (Value) + 1) * SizeOf (WideChar);
  ws := String(Value);
  Move (ws [1], data.memory^, (Length (Value) + 1) * SizeOf (WideChar))
end;

class function TRCDataDescriptionResourceDetails.SupportsRCData(
  const AName: ansistring; Size: Integer; data: Pointer): Boolean;
begin
  Result := CompareText ( String(AName), 'DESCRIPTION') = 0;
end;

{ TRCDataPackagesResourceDetails }

procedure TRCDataPackagesResourceDetails.ChangeData(
  newData: TMemoryStream);
begin
  inherited;
  FreeAndNil (fRequiresList);
  FreeAndNil (fContainsList);
end;

procedure TRCDataPackagesResourceDetails.DecodeData;
var
  p : PAnsiChar;
  i, Count : Integer;
  pkg : PPkgName;
  unt : PUnitName;
begin
  if not Assigned (fRequiresList) then
  begin
    fRequiresList := TStringList.Create;
    fContainsList := TStringList.Create;

    p := Data.Memory;
    fFlags := PDWORD (p)^;
    Inc (p, SizeOf (DWORD)); //  Flags

    Count := PInteger (p)^;
    Inc (p, SizeOf (Integer));

    for i := 0 to Count - 1 do
    begin
      pkg := PPkgName (p);


      fRequiresList.Add (pkg^.Name);
      Inc (p, 2 + lstrlen (pkg^.Name));
    end;

    Count := PInteger (p)^;
    Inc (p, SizeOf (Integer));

    for i := 0 to Count - 1 do
    begin
      unt := PUnitName (p);
      fContainsList.AddObject (unt^.Name, TObject (Integer (unt.Flags)));
      Inc (p, 3 + lstrlen (unt^.Name));
    end
  end
end;

destructor TRCDataPackagesResourceDetails.Destroy;
begin
  fRequiresList.Free;
  fContainsList.Free;
  inherited;
end;

function TRCDataPackagesResourceDetails.GetCheckForDuplicates: Boolean;
begin
  DecodeData;
  Result := (fFlags and 8) = 0
end;

function TRCDataPackagesResourceDetails.GetContains(idx: Integer): ansistring;
begin
  DecodeData;
  Result := AnsiString(fContainsList [idx]);
end;

function TRCDataPackagesResourceDetails.GetContainsCount: Integer;
begin
  DecodeData;
  Result := fContainsList.Count
end;

function TRCDataPackagesResourceDetails.GetContainsFlag(
  idx: Integer): Byte;
begin
  DecodeData;
  Result := Integer (fContainsList.Objects [idx])
end;

function TRCDataPackagesResourceDetails.GetDesignTimeOnly: Boolean;
begin
  DecodeData;
  Result := (fFlags and 2) <> 0
end;

function TRCDataPackagesResourceDetails.GetEnvironment: TPackageEnvironment;
begin
  DecodeData;
  Result := TPackageEnvironment ((fFlags shr 26) and 3);
end;

function TRCDataPackagesResourceDetails.GetModuleType: TModuleType;
begin
  DecodeData;
  Result := TModuleType (fFlags shr 30);
end;

function TRCDataPackagesResourceDetails.GetNeverBuild: Boolean;
begin
  DecodeData;
  Result := (fFlags and 1) <> 0
end;

function TRCDataPackagesResourceDetails.GetRequires(idx : Integer): ansistring;
begin
  DecodeData;
  Result := AnsiString(fRequiresList [idx]);
end;

function TRCDataPackagesResourceDetails.GetRequiresCount: Integer;
begin
  DecodeData;
  Result := fRequiresList.Count
end;

function TRCDataPackagesResourceDetails.GetRunTimeOnly: Boolean;
begin
  DecodeData;
  Result := (fFlags and 4) <> 0
end;

class function TRCDataPackagesResourceDetails.SupportsRCData(
  const AName: ansistring; Size: Integer; data: Pointer): Boolean;
begin
  Result := CompareText (String(AName), 'PACKAGEINFO') = 0;
end;

{ TRCDataFormResourceDetails }

function TRCDataFormResourceDetails.GetText: ansistring;
var
  s : TStringStream;
begin
  s := TStringStream.Create ('');
  try
    data.Seek (0, soFromBeginning);
    ObjectBinaryToText (data, s);
    Result := AnsiString( s.DataString );
  finally
    s.Free
  end
end;

procedure TRCDataFormResourceDetails.SetText(const Value: ansistring);
var
  s : TStringStream;
  m : TMemoryStream;
begin
  s := TStringStream.Create (Value);
  try
    m := TMemoryStream.Create;
    try
      s.Seek (0, soFromBeginning);
      ObjectTextToBinary (s, m);
      ChangeData (m);
    finally
      m.Free;
    end
  finally
    s.Free
  end
end;

class function TRCDataFormResourceDetails.SupportsRCData(
  const AName: ansistring; Size: Integer; data: Pointer): Boolean;
begin
  Result := (Size > 0) and (strlcomp (PAnsiChar (data), 'TPF0', 4) = 0);
end;

initialization
  RegisterResourceDetails (TRCDataDescriptionResourceDetails);
  RegisterResourceDetails (TRCDataPackagesResourceDetails);
  RegisterResourceDetails (TRCDataFormResourceDetails);
finalization
  UnregisterResourceDetails (TRCDataDescriptionResourceDetails);
  UnregisterResourceDetails (TRCDataPackagesResourceDetails);
  UnregisterResourceDetails (TRCDataFormResourceDetails);
end.
