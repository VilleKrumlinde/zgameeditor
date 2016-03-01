unit frmExprPropEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomPropEditBase, Vcl.StdCtrls,
  SynEdit, SynCompletionProposal;

type
  TExprPropEditForm = class(TCustomPropEditBaseForm)
    ExprPanel: TGroupBox;
    ExprCompileButton: TButton;
    ExprHelpButton: TButton;
    CompileErrorLabel: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure ExprHelpButtonClick(Sender: TObject);
  private
    procedure OnExprChanged(Sender: TObject);
    { Private declarations }
  public
    { Public declarations }
    ExprSynEdit : TSynEdit;
    AutoComp,ParamComp : TSynCompletionProposal;
    procedure SaveChanges; override;
  end;

implementation

{$R *.dfm}

uses dmCommon, SynHighlighterZc, SynEditSearch;

procedure TExprPropEditForm.ExprHelpButtonClick(Sender: TObject);
begin
  HtmlHelp(0,Application.HelpFile + '::/ScriptingLanguage.html', HH_DISPLAY_TOPIC, 0);
end;

procedure TExprPropEditForm.FormCreate(Sender: TObject);
begin
  inherited;

  ExprSynEdit := TSynEdit.Create(Self);
  ExprSynEdit.Align := alClient;
  ExprSynEdit.Gutter.Visible := True;
  ExprSynEdit.Gutter.ShowLineNumbers := True;
  ExprSynEdit.Parent := ExprPanel;
  ExprSynEdit.OnChange := OnExprChanged;
  ExprSynEdit.Highlighter := TSynZcSyn.Create(Self);
  ExprSynEdit.WantTabs := True;
  ExprSynEdit.TabWidth := 2;
  ExprSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  ExprSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ExprSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;

  //SynEdit autocompletion
  AutoComp := TSynCompletionProposal.Create(Self);
  AutoComp.Editor := ExprSynEdit;
  AutoComp.EndOfTokenChr := '+-/*=()[]., @';
  AutoComp.TriggerChars := 'abcdefghijklmnopqrstuvxyz.@';
  AutoComp.ShortCut := 16416;
  AutoComp.Options := DefaultProposalOptions + [scoCaseSensitive,scoUseBuiltInTimer,scoUseInsertList,scoUsePrettyText];
  AutoComp.TimerInterval := 2000;

  //SynEdit autocompletion for parameters
  ParamComp := TSynCompletionProposal.Create(Self);
  ParamComp.DefaultType := ctParams;
  ParamComp.Options := [scoLimitToMatchedText, scoUseBuiltInTimer];
  ParamComp.TriggerChars := '(';
  ParamComp.EndOfTokenChr := '';
  ParamComp.ShortCut := 24608;
  ParamComp.Editor := ExprSynEdit;
  ParamComp.TimerInterval := 2000;
end;

procedure TExprPropEditForm.OnExprChanged(Sender: TObject);
begin
  ExprCompileButton.Enabled := True;
end;

procedure TExprPropEditForm.SaveChanges;
begin
  inherited;
  if ExprCompileButton.Enabled then
    ExprCompileButton.OnClick(ExprCompileButton);
end;

end.
