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
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Math;

procedure TFloatPropEditForm.TrackBar1Change(Sender: TObject);
var
  NewValue,Scale : single;
begin
  if FloatEdit=nil then
    Exit;
  Scale := (MaxFloat - MinFloat) / TrackBar1.Max;
  NewValue := MinFloat + (TrackBar1.Position * Scale);
  FloatEdit.Text := FloatToStr( RoundTo(NewValue,-2) );
end;

end.
