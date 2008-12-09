unit frmLfoFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, AudioPlayer,
  ComCtrls, StdCtrls;

type
  TLfoFrame = class(TFrame)
    ActiveCheckBox: TCheckBox;
    StyleComboBox: TComboBox;
    SpeedTrackBar: TTrackBar;
    BipolarCheckBox: TCheckBox;
    Label1: TLabel;
    NrLabel: TLabel;
    procedure ActiveCheckBoxClick(Sender: TObject);
    procedure StyleComboBoxChange(Sender: TObject);
    procedure SpeedTrackBarChange(Sender: TObject);
    procedure BipolarCheckBoxClick(Sender: TObject);
  private
    { Private declarations }
    Lfo : PLfo;
  public
    { Public declarations }
    procedure SetLfo(L : PLfo);
  end;

implementation

{$R *.dfm}

uses DesignerGUI,TypInfo;


procedure TLfoFrame.ActiveCheckBoxClick(Sender: TObject);
begin
  Lfo.Active := (Sender as TCheckBox).Checked;
end;

procedure TLfoFrame.StyleComboBoxChange(Sender: TObject);
begin
  Lfo.Style := TLfoStyle( (Sender as TComboBox).ItemIndex );
end;

procedure TLfoFrame.SpeedTrackBarChange(Sender: TObject);
begin
  Lfo.Speed := (Sender as TTrackBar).Position * 1 / (Sender as TTrackBar).Max;
end;

procedure TLfoFrame.BipolarCheckBoxClick(Sender: TObject);
begin
  Lfo.IsBipolar := (Sender as TCheckBox).Checked;
end;

procedure TLfoFrame.SetLfo(L: PLfo);
var
  I : TLfoStyle;
begin
  if StyleComboBox.Items.Count=0 then
  begin
    StyleComboBox.Items.BeginUpdate;
    for I:=Low(TLfoStyle) to High(TLfoStyle) do
      StyleComboBox.Items.Add(
        Copy(GetEnumName(TypeInfo(TLfoStyle),Ord(I)) ,3,100) );
    StyleComboBox.Items.EndUpdate;
  end;

  Self.Lfo := L;
  ActiveCheckBox.Checked := L.Active;
  StyleComboBox.ItemIndex := Ord(L.Style);
  BipolarCheckBox.Checked := L.IsBipolar;
  SpeedTrackBar.Position := Round( L.Speed*100 );
end;

end.
