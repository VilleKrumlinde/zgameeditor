unit frmFloatPropEdit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomPropEditBase, Vcl.ComCtrls,
  Vcl.StdCtrls;

type
  TFloatPropEditForm = class(TCustomPropEditBaseForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure TrackBar1Change(Sender: TObject);
  private
    { Private declarations }
  public
    MinFloat,MaxFloat : single;
    FloatEdit : TEdit;
    PropIndex : integer;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Math, ZClasses;

procedure TFloatPropEditForm.TrackBar1Change(Sender: TObject);
var
  NewValue,Scale : single;
  Value : TZPropertyValue;
begin
  if FloatEdit=nil then
    Exit;
  Scale := (MaxFloat - MinFloat) / TrackBar1.Max;
  NewValue := MinFloat + (TrackBar1.Position * Scale);
  if Self.Parent<>nil then
  begin //Attached, write directly to edit box
    FloatEdit.Text := FloatToStr( RoundTo(NewValue,-2) );
  end else
  begin
    Value := Self.Component.GetProperty(Self.Prop);
    case Prop.PropertyType of
      zptRectf : Value.RectfValue.Area[Self.PropIndex] := NewValue;
      zptVector3f : Value.Vector3fValue[Self.PropIndex] := NewValue;
      zptFloat,zptScalar : Value.FloatValue := NewValue;
      else
        Assert(False);
    end;
    Self.Component.SetProperty(Self.Prop,Value);
  end;
end;

end.
