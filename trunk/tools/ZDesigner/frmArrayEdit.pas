unit frmArrayEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ZExpressions, Grids, StdCtrls, ZClasses, ComCtrls;

type
  TArrayEditForm = class(TForm)
    OkButton: TButton;
    Grid: TStringGrid;
    UpDown1: TUpDown;
    Dim3Edit: TEdit;
    CopyAllButton: TButton;
    PasteAllButton: TButton;
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure UpDown1ChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Smallint; Direction: TUpDownDirection);
    procedure CopyAllButtonClick(Sender: TObject);
    procedure PasteAllButtonClick(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { Private declarations }
    TheArray : TDefineArray;
    Index3 : integer;
    function ValueAsText(P : pointer) : string;
    procedure SetValueFromText(const S: String; P: pointer);
    procedure ReadFromArray;
  public
    { Public declarations }
    procedure SetArray(A : TDefineArray);
  end;

var
  ArrayEditForm: TArrayEditForm;

implementation

{$R *.dfm}

uses DesignerGUI,Clipbrd;

{ TArrayEditForm }

procedure TArrayEditForm.GridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  P : PByte;
  Index : integer;
begin
  P := PByte(TheArray.GetData);
  Index := 0;
  case TheArray.Dimensions of
    dadOne:
      begin
        Index := ACol-1;
      end;
    dadTwo:
      begin
        Index := ((ARow-1)*TheArray.SizeDim2) + (ACol-1);
      end;
    dadThree:
      begin
        Index := (Index3*TheArray.SizeDim2*TheArray.SizeDim3) + ((ARow-1)*TheArray.SizeDim3) + (ACol-1);
      end;
  end;
  Inc(P,Index * TheArray.GetElementSize);
  SetValueFromText(Value,P);
end;

procedure TArrayEditForm.SetArray(A: TDefineArray);
begin
  TheArray := A;

  Index3 := 0;
  Dim3Edit.Visible := A.Dimensions=dadThree;
  Dim3Edit.Text := '0';
  UpDown1.Visible := A.Dimensions=dadThree;
  if UpDown1.Visible then
    UpDown1.Max := A.SizeDim1-1;

  ReadFromArray;
end;

procedure TArrayEditForm.ReadFromArray;
var
  J: Integer;
  I: Integer;
  P: PByte;
  A: TDefineArray;
begin
  A := Self.TheArray;
  P := PByte(A.GetData);
  case A.Dimensions of
    dadOne:
      begin
        Grid.RowCount := 2;
        Grid.ColCount := A.SizeDim1 + 1;
        for I := 0 to A.SizeDim1 - 1 do
          Grid.Cells[I + 1, 0] := IntToStr(I);
        for I := 0 to A.SizeDim1 - 1 do
        begin
          Grid.Cells[I + 1, 1] := ValueAsText(P);
          Inc(P,A.GetElementSize);
        end;
      end;
    dadTwo:
      begin
        Grid.RowCount := A.SizeDim1 + 1;
        Grid.ColCount := A.SizeDim2 + 1;
        for I := 0 to A.SizeDim1 - 1 do
          Grid.Cells[0, I + 1] := IntToStr(I);
        for J := 0 to A.SizeDim2 - 1 do
          Grid.Cells[J + 1, 0] := IntToStr(J);
        for I := 0 to A.SizeDim1 - 1 do
        begin
          for J := 0 to A.SizeDim2 - 1 do
          begin
            Grid.Cells[J + 1, I + 1] := ValueAsText(P);
            Inc(P,A.GetElementSize);
          end;
        end;
      end;
    dadThree:
      begin
        Grid.RowCount := A.SizeDim2 + 1;
        Grid.ColCount := A.SizeDim3 + 1;
        for I := 0 to A.SizeDim2 - 1 do
          Grid.Cells[0, I + 1] := IntToStr(I);
        for J := 0 to A.SizeDim3 - 1 do
          Grid.Cells[J + 1, 0] := IntToStr(J);
        Inc(P,Self.Index3*A.SizeDim2*A.SizeDim3);
        for I := 0 to A.SizeDim2 - 1 do
        begin
          for J := 0 to A.SizeDim3 - 1 do
          begin
            Grid.Cells[J + 1, I + 1] := ValueAsText(P);
            Inc(P,A.GetElementSize);
          end;
        end;
      end;
  end;
end;

function TArrayEditForm.ValueAsText(P: pointer): string;
begin
  case TheArray._Type of
    zctFloat: Result := DesignerFormatFloat(PFloat(P)^);
    zctInt: Result := IntToStr(PInteger(P)^);
    zctByte: Result := IntToStr(PByte(P)^);
  end;
end;

procedure TArrayEditForm.SetValueFromText(const S : String; P: pointer);
begin
  case TheArray._Type of
    zctFloat: PFloat(P)^ := StrToFloatDef(S,0);
    zctInt: PInteger(P)^ := StrToIntDef(S,0);
    zctByte: PByte(P)^ := StrToIntDef(S,0);
  end;
end;

procedure TArrayEditForm.UpDown1ChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Smallint; Direction: TUpDownDirection);
begin
  if (NewValue>=0) and (NewValue<TheArray.SizeDim1) then
  begin
    Index3 := NewValue;
    ReadFromArray;
  end
  else
    AllowChange := False;
end;

procedure TArrayEditForm.OkButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TArrayEditForm.CopyAllButtonClick(Sender: TObject);
var
  I,J : integer;
  List : TStringList;
  S : string;
begin
  List := TStringList.Create;
  for I := 0 to Grid.RowCount-2 do
  begin
    S := '';
    for J := 0 to Grid.ColCount-2 do
    begin
      S := S + Grid.Cells[J+1,I+1] + ',';
    end;
    List.Add( Copy(S,1,Length(S)-1) );
  end;
  Clipboard.AsText := List.Text;
  List.Free;
end;


procedure TArrayEditForm.PasteAllButtonClick(Sender: TObject);
var
  I,J : integer;
  List1,List2 : TStringList;
begin
  List1 := TStringList.Create;
  List2 := TStringList.Create;
  List1.Text := Clipboard.AsText;

  for I := 0 to List1.Count - 1 do
  begin
    List2.CommaText := List1[I];
    for J := 0 to List2.Count - 1 do
    begin
      Grid.Cells[J+1,I+1] := List2[J];
      GridSetEditText(Grid, J+1, I+1,List2[J]);
    end;
  end;

  List1.Free;
  List2.Free;
end;


end.
