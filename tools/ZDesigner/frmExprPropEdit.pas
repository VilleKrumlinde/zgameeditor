unit frmExprPropEdit;

interface

uses
  {$ifndef ZgeLazarus}
  SynEdit, SynCompletionProposal,
  {$endif}
  Windows, Messages, SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomPropEditBase, StdCtrls,
  ExtCtrls;

type
  TExprPropEditForm = class(TCustomPropEditBaseForm)
    ExprPanel: TGroupBox;
    ExprCompileButton: TButton;
    ExprHelpButton: TButton;
    CompileErrorLabel: TStaticText;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure ExprHelpButtonClick(Sender: TObject);
  private
    FMousePos: TPoint;
    FOldCaretY: Integer;
    FUnderLine: Integer;
    procedure OnExprChanged(Sender: TObject);
    procedure EditorGutterPaint(Sender: TObject; aLine, X, Y: Integer);
    procedure EditorMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    {$ifndef ZgeLazarus}
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    {$endif}
  public
    {$ifndef ZgeLazarus}
    ExprSynEdit : TSynEdit;
    AutoComp,ParamComp : TSynCompletionProposal;
    {$endif}
    procedure SaveChanges; override;
    procedure ShowError(MsgText: String);
    procedure HideError;
  end;

implementation

{$R *.dfm}

uses
  {$ifndef ZgeLazarus}
  SynHighlighterZc, SynEditSearch,
  {$endif}
  dmCommon, Types;

procedure TExprPropEditForm.ExprHelpButtonClick(Sender: TObject);
begin
  {$ifndef ZgeLazarus}
  HtmlHelp(0,Application.HelpFile + '::/ScriptingLanguage.html', HH_DISPLAY_TOPIC, 0);
  {$endif}
end;

procedure TExprPropEditForm.FormCreate(Sender: TObject);
begin
  inherited;

  {$ifndef ZgeLazarus}
  ExprSynEdit := TSynEdit.Create(Self);
  ExprSynEdit.Align := alClient;
  ExprSynEdit.Gutter.Visible := True;
  ExprSynEdit.Gutter.ShowModification := True;
  ExprSynEdit.Gutter.ShowLineNumbers := False;
  ExprSynEdit.Parent := ExprPanel;
  ExprSynEdit.OnChange := OnExprChanged;
  ExprSynEdit.OnMouseMove := EditorMouseMove;
  ExprSynEdit.OnStatusChange := EditorStatusChange;
  ExprSynEdit.OnGutterPaint := EditorGutterPaint;
  ExprSynEdit.Highlighter := TSynZcSyn.Create(Self);
  ExprSynEdit.WantTabs := True;
  ExprSynEdit.TabWidth := 2;
  ExprSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoShowScrollHint, eoTabsToSpaces, eoHideShowScrollbars, eoScrollPastEol,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces, eoAutoSizeMaxScrollWidth];
  ExprSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ExprSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;
  ExprSynEdit.MaxScrollWidth := 2048;

  // SynEdit autocompletion
  AutoComp := TSynCompletionProposal.Create(Self);
  AutoComp.Editor := ExprSynEdit;
  AutoComp.EndOfTokenChr := '+-/*=()[]., @';
  AutoComp.TriggerChars := 'abcdefghijklmnopqrstuvxyz.@';
  AutoComp.ShortCut := 16416;
  AutoComp.Options := DefaultProposalOptions + [scoCaseSensitive,
    scoUseBuiltInTimer, scoUseInsertList, scoUsePrettyText];
  AutoComp.TimerInterval := 2000;

  // SynEdit autocompletion for parameters
  ParamComp := TSynCompletionProposal.Create(Self);
  ParamComp.DefaultType := ctParams;
  ParamComp.Options := [scoLimitToMatchedText, scoUseBuiltInTimer];
  ParamComp.TriggerChars := '(';
  ParamComp.EndOfTokenChr := '';
  ParamComp.ShortCut := 24608;
  ParamComp.Editor := ExprSynEdit;
  ParamComp.TimerInterval := 2000;
  {$endif}
end;

procedure TExprPropEditForm.EditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
{$ifdef ZgeLazarus}
begin
end;
{$else}
var
  MouseCoord: TDisplayCoord;
  UnderLine: Integer;
begin
  inherited;

  // only continue in case of cursor changes
  if (X = FMousePos.X) or (Y = FMousePos.Y) then
    Exit;

  // update last mouse position and restart mouse interval timer
  FMousePos := Point(X, Y);
(*
  FMouseInterval.Enabled := False;
  FMouseInterval.Enabled := True;
*)

  // eventualy invalid old highlighted line
  if FUnderLine > 0 then
    TSynEdit(Sender).InvalidateLine(FUnderLine);

  // check whether word in line should be shown as "link"
  if ssCtrl in Shift then
  begin
    // extract current line
    MouseCoord := TSynEdit(Sender).PixelsToRowColumn(X, Y);
    UnderLine := TSynEdit(Sender).DisplayToBufferPos(MouseCoord).Line;

    // check if line is different to previous line and eventually invalidate
    if UnderLine <> FUnderLine then
    begin
      FUnderLine := UnderLine;
      TSynEdit(Sender).InvalidateLine(FUnderLine);
    end;
  end
  else
    FUnderLine := -1;
end;
{$endif}

{$ifndef ZgeLazarus}
procedure TExprPropEditForm.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  NewCaretY: Integer;
begin
  if (scCaretY in Changes) and TSynEdit(Sender).Gutter.Visible  then
  begin
    NewCaretY := TSynEdit(Sender).CaretY;
    TSynEdit(Sender).InvalidateGutterLine(FOldCaretY);
    TSynEdit(Sender).InvalidateGutterLine(NewCaretY);
    FOldCaretY := NewCaretY;
  end;
end;
{$endif}

procedure TExprPropEditForm.OnExprChanged(Sender: TObject);
begin
  ExprCompileButton.Enabled := True;
end;

procedure TExprPropEditForm.SaveChanges;
begin
  inherited;
  if ExprCompileButton.Enabled then
    ExprCompileButton.OnClick(ExprCompileButton);
  {$ifndef ZgeLazarus}
  ExprSynEdit.MarkModifiedLinesAsSaved;
  {$endif}
end;

procedure TExprPropEditForm.EditorGutterPaint(Sender: TObject; aLine, X,  Y: Integer);
{$ifdef ZgeLazarus}
begin
end;
{$else}
var
  SynEdit: TSynEdit;
  num: string;
  numRct: TRect;
  GutterWidth : Integer;
  OldFont: TFont;
const
  CTickSizes : array [Boolean] of Integer = (2, 5);
begin
  Assert(Sender is TSynEdit);
  SynEdit := TSynEdit(Sender);
  SynEdit.Canvas.Brush.Style := bsClear;
  GutterWidth := SynEdit.Gutter.Width - 4;

  //line numbers
  if (aLine = 1) or (aLine = SynEdit.CaretY) or ((aLine mod 10) = 0) then
  begin
    num := IntToStr(aLine);
    numRct := Rect(x, y, GutterWidth, y + SynEdit.LineHeight);
    OldFont := TFont.Create;
    try
      OldFont.Assign(SynEdit.Canvas.Font);
      SynEdit.Canvas.Font := SynEdit.Gutter.Font;
      SynEdit.Canvas.TextRect(numRct, num, [tfVerticalCenter, tfSingleLine, tfRight]);
      SynEdit.Canvas.Font := OldFont;
    finally
      OldFont.Free;
    end;
  end
  //small tick
  else
  begin
    SynEdit.Canvas.Pen.Color := SynEdit.Gutter.Font.Color;
    numRct.Left := GutterWidth - CTickSizes[(aLine mod 5) = 0];
    numRct.Right := GutterWidth;
    numRct.Top := y + SynEdit.LineHeight div 2;
    SynEdit.Canvas.MoveTo(numRct.Left, numRct.Top);
    SynEdit.Canvas.LineTo(numRct.Right, numRct.Top);
  end;
end;
{$endif}

procedure TExprPropEditForm.ShowError(MsgText: String);
begin
  Splitter.Show;
  CompileErrorLabel.Caption := MsgText;
  CompileErrorLabel.Hint := MsgText;
  CompileErrorLabel.Show;
  Splitter.Top := ExprPanel.Height - Splitter.Height - CompileErrorLabel.Height;
end;

procedure TExprPropEditForm.HideError;
begin
  CompileErrorLabel.Caption := '';
  {$ifndef ZgeLazarus}
  CompileErrorLabel.BevelKind := bkNone;
  {$endif}
  CompileErrorLabel.Hide;
  Splitter.Hide;
end;

end.
