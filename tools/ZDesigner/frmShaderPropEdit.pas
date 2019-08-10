unit frmShaderPropEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  SynEdit, frmCustomPropEditBase;

type
  TShaderPropEditForm = class(TCustomPropEditBaseForm)
    ShaderPanel: TGroupBox;
    CompileShaderButton: TButton;
    CompileErrorLabel: TStaticText;
    procedure FormCreate(Sender: TObject);
  private
    procedure OnShaderExprChanged(Sender: TObject);
  public
    ShaderSynEdit : TSynEdit;
    procedure SaveChanges; override;
  end;

implementation

{$R *.dfm}

uses
  dmCommon, SynHighlighterGLSL, SynEditSearch;

procedure TShaderPropEditForm.FormCreate(Sender: TObject);
begin
  inherited;

  ShaderSynEdit := TSynEdit.Create(Self);
  ShaderSynEdit.Highlighter := TSynGLSLSyn.Create(Self);
  ShaderSynEdit.Align := alClient;
  ShaderSynEdit.Gutter.Visible := True;
  ShaderSynEdit.Gutter.ShowLineNumbers := True;
  ShaderSynEdit.Parent := ShaderPanel;
  ShaderSynEdit.OnChange := OnShaderExprChanged;
  ShaderSynEdit.WantTabs := True;
  ShaderSynEdit.TabWidth := 2;
  ShaderSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  ShaderSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ShaderSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;
end;


procedure TShaderPropEditForm.OnShaderExprChanged(Sender: TObject);
begin
  CompileShaderButton.Enabled := True;
end;

procedure TShaderPropEditForm.SaveChanges;
begin
  if CompileShaderButton.Enabled then
    CompileShaderButton.OnClick(CompileShaderButton);
end;

end.
