unit frmSpriteSheetEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmScriptedCompEditBase, Vcl.ExtCtrls;

type
  TSpriteSheetEditFrame = class(TScriptedCompEditFrameBase)
  private
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent) ; override;
    procedure BindData; override;
  end;

implementation

{$R *.dfm}

uses frmEditor, Renderer;

{ TSpriteSheetEditFrame }

procedure TSpriteSheetEditFrame.BindData;
var
  O : TObject;
begin
  inherited;

  O := EditorApp.SymTab.Lookup('SheetMaterialTexture');
  if (O is TMaterialTexture) then
  begin
    TMaterialTexture(O).Texture := (Self.Component as TSpriteSheet).Bitmap;
  end;
end;

constructor TSpriteSheetEditFrame.Create(AOwner: TComponent);
begin
  inherited;
  Self.ScriptName := 'spritesheet.zgeproj';
end;

end.
