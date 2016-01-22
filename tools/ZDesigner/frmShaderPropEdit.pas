unit frmShaderPropEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomPropEditBase, Vcl.StdCtrls, SynEdit;

type
  TShaderPropEditForm = class(TCustomPropEditBaseForm)
    ShaderPanel: TGroupBox;
    CompileShaderButton: TButton;
    CompileErrorLabel: TStaticText;
    procedure FormCreate(Sender: TObject);
  private
    procedure OnShaderExprChanged(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    ShaderSynEdit : TSynEdit;
  end;

implementation

{$R *.dfm}

uses dmCommon, SynHighlighterCpp, SynEditSearch;

procedure TShaderPropEditForm.FormCreate(Sender: TObject);
begin
  inherited;

  ShaderSynEdit := TSynEdit.Create(Self);
  ShaderSynEdit.Highlighter := TSynCppSyn.Create(Self);
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

end.
