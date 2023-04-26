unit frmModulationFrame;

interface

uses 
  {$ifndef ZgeLazarus}
  Windows, Messages,
  {$endif}
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, AudioPlayer;

type
  TModulationFrame = class(TFrame)
    SourceComboBox: TComboBox;
    DestComboBox: TComboBox;
    AmountTrackBar: TTrackBar;
    ActiveCheckBox: TCheckBox;
    Label1: TLabel;
    NrLabel: TLabel;
    procedure ActiveCheckBoxClick(Sender: TObject);
    procedure AmountTrackBarChange(Sender: TObject);
    procedure SourceComboBoxChange(Sender: TObject);
    procedure DestComboBoxChange(Sender: TObject);
  private
    { Private declarations }
    Modulation : PModulation;
  public
    { Public declarations }
    procedure SetModulation(M : PModulation);
  end;

implementation

{$R *.dfm}

uses TypInfo;

procedure TModulationFrame.ActiveCheckBoxClick(Sender: TObject);
begin
  Modulation.Active := (Sender as TCheckBox).Checked;
end;

procedure TModulationFrame.AmountTrackBarChange(Sender: TObject);
begin
  Modulation.Amount := AmountTrackBar.Position * 1.0 / AmountTrackBar.Max;
end;

procedure TModulationFrame.SourceComboBoxChange(Sender: TObject);
begin
  Modulation.Source := TModulationSource((Sender as TComboBox).ItemIndex);
end;

procedure TModulationFrame.DestComboBoxChange(Sender: TObject);
begin
  Modulation.Destination := TModulationDestination((Sender as TComboBox).ItemIndex);
end;

procedure TModulationFrame.SetModulation(M: PModulation);
var
  SourceI : TModulationSource;
  DestI : TModulationDestination;
begin
  if SourceComboBox.Items.Count=0 then
  begin
    SourceComboBox.Items.BeginUpdate;
    for SourceI:=Low(TModulationSource) to High(TModulationSource) do
      SourceComboBox.Items.Add(
        Copy( GetEnumName(TypeInfo(TModulationSource),Ord(SourceI)) , 3, 100) );
    SourceComboBox.Items.EndUpdate;

    DestComboBox.Items.BeginUpdate;
    for DestI:=Low(TModulationDestination) to High(TModulationDestination) do
      DestComboBox.Items.Add(
        Copy( GetEnumName(TypeInfo(TModulationDestination),Ord(DestI)) , 3, 100) );
    DestComboBox.Items.EndUpdate;
  end;

  Self.Modulation := M;
  ActiveCheckBox.Checked := M.Active;
  SourceComboBox.ItemIndex := Ord(M.Source);
  DestComboBox.ItemIndex := Ord(M.Destination);
  AmountTrackBar.Position := Round(M.Amount*100);
end;

end.
