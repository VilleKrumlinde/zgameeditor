unit frmSpriteSheetEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmScriptedCompEditBase, Vcl.ExtCtrls, ZExpressions,
  Renderer;

type
  TSpriteSheetEditFrame = class(TScriptedCompEditFrameBase)
  private
    { Private declarations }
    SpritesArray : TDefineArray;
    MaterialTexture : TMaterialTexture;
    Sheet : TSpriteSheet;
    DataChanged : TDefineVariable;
    procedure CopyDataToComponent;
    procedure CopyDataFromComponent;
  protected
    procedure BindData; override;
    procedure UpdateData; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation

{$R *.dfm}

uses frmEditor;

{ TSpriteSheetEditFrame }

procedure TSpriteSheetEditFrame.BindData;
begin
  inherited;

  Self.Sheet := Self.Component as TSpriteSheet;

  if Glp.App.BindComponent<TMaterialTexture>('SheetMaterialTexture', Self.MaterialTexture) then
  begin
    MaterialTexture.Texture := (Self.Component as TSpriteSheet).Bitmap;
  end;

  Glp.App.BindComponent<TDefineArray>('Sprites',Self.SpritesArray);

  Glp.App.BindComponent<TDefineVariable>('DataChanged',Self.DataChanged);

  CopyDataFromComponent;
end;

constructor TSpriteSheetEditFrame.Create(AOwner: TComponent);
begin
  inherited;
  Self.ScriptName := 'spritesheet.zgeproj';
end;

procedure TSpriteSheetEditFrame.CopyDataToComponent;
var
  I : integer;
  Ps : PInteger;
  M : TMemoryStream;
  X,Y,W,H,Ox,Oy : smallint;
begin
  M := TMemoryStream.Create;

  Ps := PInteger(SpritesArray.GetData);
  for I := 0 to SpritesArray.SizeDim1-1 do
  begin
    X := Ps^; Inc(Ps);
    M.Write(X,SizeOf(SmallInt));

    Y := Ps^; Inc(Ps);
    M.Write(Y,SizeOf(SmallInt));

    W := Ps^; Inc(Ps);
    M.Write(W,SizeOf(SmallInt));

    H := Ps^; Inc(Ps);
    M.Write(H,SizeOf(SmallInt));

    Ox := Ps^; Inc(Ps);
    M.Write(Ox,SizeOf(SmallInt));

    Oy := Ps^; Inc(Ps);
    M.Write(Oy,SizeOf(SmallInt));
  end;

  M.Position := 0;
  Self.Sheet.SpriteData.Size := M.Size;
  ReallocMem(Self.Sheet.SpriteData.Data,Self.Sheet.SpriteData.Size);
  Move(M.Memory^ , Self.Sheet.SpriteData.Data^, Self.Sheet.SpriteData.Size);

  M.Free;
end;

procedure TSpriteSheetEditFrame.CopyDataFromComponent;
var
  I : integer;
  Ps : PInteger;
  M : TMemoryStream;
  X,Y,W,H,Ox,Oy : smallint;
begin
  M := TMemoryStream.Create;

  M.Size := Self.Sheet.SpriteData.Size;
  Move(Self.Sheet.SpriteData.Data^, M.Memory^, Self.Sheet.SpriteData.Size);

  SpritesArray.SizeDim1 := M.Size div (6*2);
  Ps := PInteger(SpritesArray.GetData);

  for I := 0 to SpritesArray.SizeDim1-1 do
  begin
    M.Read(X,SizeOf(SmallInt));
    Ps^ := X; Inc(Ps);

    M.Read(Y,SizeOf(SmallInt));
    Ps^ := Y; Inc(Ps);

    M.Read(W,SizeOf(SmallInt));
    Ps^ := W; Inc(Ps);

    M.Read(H,SizeOf(SmallInt));
    Ps^ := H; Inc(Ps);

    M.Read(Ox,SizeOf(SmallInt));
    Ps^ := Ox; Inc(Ps);

    M.Read(Oy,SizeOf(SmallInt));
    Ps^ := Oy; Inc(Ps);
  end;

  M.Free;
end;

procedure TSpriteSheetEditFrame.UpdateData;
begin
  inherited;

  if Assigned(MaterialTexture) then
    MaterialTexture.Texture := (Self.Component as TSpriteSheet).Bitmap;

  if Assigned(SpritesArray) then
  begin
    if (Self.DataChanged=nil) or (Self.DataChanged.IntValue<>0) then
    begin
      CopyDataToComponent;
      if Assigned(Self.DataChanged) then
        //Script assigns DataChanged to 1 when data is to be copied back.
        //Then it is reset to 0.
        Self.DataChanged.IntValue := 0;
    end;
  end;
end;

end.
