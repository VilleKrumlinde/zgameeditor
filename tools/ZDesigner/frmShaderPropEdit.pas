unit frmShaderPropEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  SynEdit, frmCustomPropEditBase, Vcl.ExtCtrls;

type
  TShaderPropEditForm = class(TCustomPropEditBaseForm)
    ShaderPanel: TGroupBox;
    CompileShaderButton: TButton;
    CompileErrorLabel: TStaticText;
    Splitter: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure EditorGutterPaint(Sender: TObject; aLine, X, Y: Integer);
  private
    FMousePos: TPoint;
    FOldCaretY: Integer;
    FUnderLine: Integer;
    FErrorLines: array of Integer;
    procedure OnShaderExprChanged(Sender: TObject);
    procedure EditorMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: Integer);
    procedure EditorSpecialLineColors(Sender: TObject;
      Line: Integer; var Special: Boolean; var FG, BG: TColor);
    procedure EditorStatusChange(Sender: TObject;
      Changes: TSynStatusChanges);
  public
    ShaderSynEdit : TSynEdit;
    procedure SaveChanges; override;
    procedure ShowError(MsgText: String);
    procedure HideError;
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
  ShaderSynEdit.Gutter.ShowModification := True;
  ShaderSynEdit.Gutter.ShowLineNumbers := False;
  ShaderSynEdit.Parent := ShaderPanel;
  ShaderSynEdit.OnChange := OnShaderExprChanged;
  ShaderSynEdit.OnMouseMove := EditorMouseMove;
  ShaderSynEdit.OnStatusChange := EditorStatusChange;
  ShaderSynEdit.OnSpecialLineColors := EditorSpecialLineColors;
  ShaderSynEdit.OnGutterPaint := EditorGutterPaint;
  ShaderSynEdit.WantTabs := True;
  ShaderSynEdit.TabWidth := 2;
  ShaderSynEdit.Options := [eoAutoIndent, eoDragDropEditing, eoEnhanceEndKey,
    eoScrollPastEol, eoShowScrollHint, eoTabsToSpaces,
    eoGroupUndo, eoTabIndent, eoTrimTrailingSpaces];
  ShaderSynEdit.SearchEngine := TSynEditSearch.Create(Self);
  ShaderSynEdit.PopupMenu := dmCommon.CommonModule.SynEditPopupMenu;
end;

procedure TShaderPropEditForm.EditorMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
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

procedure TShaderPropEditForm.EditorSpecialLineColors(Sender: TObject;
  Line: Integer; var Special: Boolean; var FG, BG: TColor);
var
  Index: Integer;
begin
  for Index := Low(FErrorLines) to High(FErrorLines) do
    if FErrorLines[Index] = Line then
    begin
      BG := $AAAAFF;
      Special := True;
    end;
end;

procedure TShaderPropEditForm.EditorStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  NewCaretY: Integer;
begin
  if (scCaretY in Changes) and TSynEdit(Sender).Gutter.Visible  then
  begin
    SetLength(FErrorLines, 0);
    NewCaretY := TSynEdit(Sender).CaretY;
    TSynEdit(Sender).InvalidateGutterLine(FOldCaretY);
    TSynEdit(Sender).InvalidateGutterLine(NewCaretY);
    FOldCaretY := NewCaretY;
  end;
end;

procedure TShaderPropEditForm.OnShaderExprChanged(Sender: TObject);
begin
  CompileShaderButton.Enabled := True;
end;

procedure TShaderPropEditForm.SaveChanges;
begin
  if CompileShaderButton.Enabled then
    CompileShaderButton.OnClick(CompileShaderButton);
  ShaderSynEdit.MarkModifiedLinesAsSaved;
end;

procedure TShaderPropEditForm.ShowError(MsgText: String);
var
  ErrorLines: TStringList;
  Temp: String;
  ClosePos: Integer;
  Index: Integer;
begin
  Splitter.Show;
  SetLength(FErrorLines, 0);

  ErrorLines := TStringList.Create;
  try
    ErrorLines.Text := MsgText;
    for Index := 0 to ErrorLines.Count - 1 do
    begin
      Temp := ErrorLines.Strings[Index];

      // detection for NVIDIA error messages
      if (Length(Temp) >= 4) and (Temp[1] = '0') and (Temp[2] = '(') then
      begin
        ClosePos := Pos(')', Temp);
        if (ClosePos > 0) and (Pos('error', Temp) > 0) then
        begin
          Temp := Copy(Temp, 3, ClosePos - 3);
          SetLength(FErrorLines, Length(FErrorLines) + 1);
          FErrorLines[Length(FErrorLines) - 1] := StrToInt(Temp);
        end;
      end;
    end;
  finally
    ErrorLines.Free;
  end;

  CompileErrorLabel.Caption := MsgText;
  CompileErrorLabel.Hint := MsgText;
  CompileErrorLabel.Show;
  Splitter.Top := ShaderPanel.Height - Splitter.Height - CompileErrorLabel.Height;
end;

procedure TShaderPropEditForm.EditorGutterPaint(Sender: TObject; aLine, X,
  Y: Integer);
var
  SynEdit: TSynEdit;
  num: string;
  numRct: TRect;
  GutterWidth, ImgIndex: Integer;
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

procedure TShaderPropEditForm.HideError;
begin
  SetLength(FErrorLines, 0);
  CompileErrorLabel.Hide;
  Splitter.Hide;
end;

end.
