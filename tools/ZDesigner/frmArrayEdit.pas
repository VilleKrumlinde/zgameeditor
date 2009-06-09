unit frmArrayEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,ZExpressions, Grids, StdCtrls;

type
  TArrayEditForm = class(TForm)
    Button1: TButton;
    Grid: TStringGrid;
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    { Private declarations }
    TheArray : TDefineArray;
  public
    { Public declarations }
    procedure SetArray(A : TDefineArray);
  end;

var
  ArrayEditForm: TArrayEditForm;

implementation

{$R *.dfm}

uses ZClasses,DesignerGUI;

{ TArrayEditForm }

procedure TArrayEditForm.GridSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  FValue : single;
  P : PFloat;
  Index : integer;
begin
  FValue := StrToFloatDef(Value,0);
  P := TheArray.GetData;
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
  end;
  Inc(P,Index);
  P^ := FValue;
end;

procedure TArrayEditForm.SetArray(A: TDefineArray);
var
  P : PFloat;
  I,J : integer;
begin
  TheArray := A;
  case A.Dimensions of
    dadOne:
      begin
        Grid.RowCount := 2;
        Grid.ColCount := A.SizeDim1+1;
        for I := 0 to A.SizeDim1 - 1 do
          Grid.Cells[I+1,0] := IntToStr(I);
        P := A.GetData;
        for I := 0 to A.SizeDim1 - 1 do
        begin
          Grid.Cells[I+1,1] := DesignerFormatFloat(P^);
          Inc(P);
        end;
      end;
    dadTwo:
      begin
        Grid.RowCount := A.SizeDim1+1;
        Grid.ColCount := A.SizeDim2+1;
        P := A.GetData;
        for I := 0 to A.SizeDim1 - 1 do
          Grid.Cells[0,I+1] := IntToStr(I);
        for J := 0 to A.SizeDim2 - 1 do
          Grid.Cells[J+1,0] := IntToStr(J);
        for I := 0 to A.SizeDim1 - 1 do
        begin
          for J := 0 to A.SizeDim2 - 1 do
          begin
            Grid.Cells[J+1,I+1] := DesignerFormatFloat(P^);
            Inc(P);
          end;
        end;
      end;
    dadThree: ;
  end;
end;

end.
