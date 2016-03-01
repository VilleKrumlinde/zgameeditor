unit frmTileSetEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmScriptedCompEditBase, Vcl.ExtCtrls, Renderer,
  Vcl.StdCtrls;

type
  TTileSetEditFrame = class(TScriptedCompEditFrameBase)
  private
    TileSet,ScriptTileSet : TTileSet;
    TileSetTexture : TMaterialTexture;
  protected
    procedure BindData; override;
    procedure UpdateData; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent) ; override;
  end;

implementation

{$R *.dfm}

{ TScriptedCompEditFrameBase1 }

procedure TTileSetEditFrame.BindData;
begin
  inherited;

  Self.TileSet := Self.Component as TTileSet;

  Glp.App.BindComponent<TTileSet>('TileSet',Self.ScriptTileSet);
  Glp.App.BindComponent<TMaterialTexture>('TileSetTexture',Self.TileSetTexture);
end;

constructor TTileSetEditFrame.Create(AOwner: TComponent);
begin
  inherited;
  Self.ScriptName := 'tileset.zgeproj';
end;

procedure TTileSetEditFrame.UpdateData;
begin
  inherited;

  if Assigned(ScriptTileSet) then
  begin
    ScriptTileSet.TileWidth := TileSet.TileWidth;
    ScriptTileSet.TileHeight := TileSet.TileHeight;
    ScriptTileSet.TileBorder := TileSet.TileBorder;
  end;

  if Assigned(TileSetTexture) then
    TileSetTexture.Texture := TileSet.Bitmap;
end;

end.
