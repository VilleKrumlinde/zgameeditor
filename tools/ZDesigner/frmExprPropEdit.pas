unit frmExprPropEdit;

interface

uses
  SynEdit, 
  {$ifdef ZgeLazarus}
  SynCompletion, 
  {$else}
  Windows, Messages,
  SynCompletionProposal, 
  {$endif}
  SysUtils, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomPropEditBase, StdCtrls,
  ExtCtrls;

type
  {$ifdef ZgeLazarus}
  TSynCompletionProposal = TSynCompletion;
  {$endif}

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
    procedure EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
  public
    ExprSynEdit : TSynEdit;
    AutoComp,ParamComp : TSynCompletionProposal;
    procedure SaveChanges; override;
    procedure ShowError(MsgText: String);
    procedure HideError;
  end;

implementation

{$R *.dfm}

uses
  {$ifndef ZgeLazarus}
  SynHighlighterZc, 
  {$endif}
  SynEditSearch,
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

  ExprSynEdit := TSynEdit.Create(Self);
  ExprSynEdit.Align := alClient;
  ExprSynEdit.Gutter.Visible := True;
  ExprSynEdit.Parent := ExprPanel;
  ExprSynEdit.OnChange := OnExprChanged;
  ExprSynEdit.OnMouseMove := EditorMouseMove;
  ExprSynEdit.OnStatusChange := EditorStatusChange;
  {$ifndef ZgeLazarus}
  ExprSynEdit.Gutter.ShowModification := True;
  ExprSynEdit.Gutter.ShowLineNumbers := False;
  ExprSynEdit.OnGutterPaint := EditorGutterPaint;
  ExprSynEdit.Highlighter := TSynZcSyn.Create(Self);
  ExprSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoShowScrollHint, eoTabsToSpaces, eoHideShowScrollbars, eoScrollPastEol,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces, eoAutoSizeMaxScrollWidth];
  ExprSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ExprSynEdit.MaxScrollWidth := 2048;
  {$endif}
  ExprSynEdit.WantTabs := True;
  ExprSynEdit.TabWidth := 2;
  ExprSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;

  // SynEdit autocompletion
  AutoComp := TSynCompletionProposal.Create(Self);
  AutoComp.Editor := ExprSynEdit;
  AutoComp.EndOfTokenChr := '+-/*=()[]., @';
  AutoComp.ShortCut := 16416;
  {$ifndef ZgeLazarus}
  AutoComp.TriggerChars := 'abcdefghijklmnopqrstuvxyz.@';
  AutoComp.Options := DefaultProposalOptions + [scoCaseSensitive,
    scoUseBuiltInTimer, scoUseInsertList, scoUsePrettyText];
  AutoComp.TimerInterval := 2000;
  {$endif}


  // SynEdit autocompletion for parameters
  ParamComp := TSynCompletionProposal.Create(Self);
  {$ifndef ZgeLazarus}
  ParamComp.DefaultType := ctParams;
  ParamComp.Options := [scoLimitToMatchedText, scoUseBuiltInTimer];
  ParamComp.TriggerChars := '(';
  ParamComp.TimerInterval := 2000;
  {$endif}
  ParamComp.EndOfTokenChr := '';
  ParamComp.ShortCut := 24608;
  ParamComp.Editor := ExprSynEdit;
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

procedure TExprPropEditForm.EditorStatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  NewCaretY: Integer;
begin
  {$ifndef ZgeLazarus}
  if (scCaretY in Changes) and TSynEdit(Sender).Gutter.Visible  then
  begin
    NewCaretY := TSynEdit(Sender).CaretY;
    TSynEdit(Sender).InvalidateGutterLine(FOldCaretY);
    TSynEdit(Sender).InvalidateGutterLine(NewCaretY);
    FOldCaretY := NewCaretY;
  end;
  {$endif}
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
