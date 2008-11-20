unit frmChannelFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, AudioPlayer, StdCtrls;

type
  TChannelFrame = class(TFrame)
    VolumeTrackBar: TTrackBar;
    NrLabel: TLabel;
    ActiveCheckBox: TCheckBox;
    DelayActiveCheckBox: TCheckBox;
    DelayLengthTrackBar: TTrackBar;
    procedure VolumeTrackBarChange(Sender: TObject);
    procedure ActiveCheckBoxClick(Sender: TObject);
    procedure DelayActiveCheckBoxClick(Sender: TObject);
    procedure DelayLengthTrackBarChange(Sender: TObject);
  private
    { Private declarations }
    Channel : PChannel;
  public
    { Public declarations }
    procedure SetChannel(C : PChannel);
  end;

implementation

{$R *.dfm}

{ TChannelFrame }

procedure TChannelFrame.SetChannel(C: PChannel);
begin
  Channel := C;
  VolumeTrackBar.Position := 100-Round(C.Volume*100);
  ActiveCheckBox.Checked := C.Active;
  DelayActiveCheckBox.Checked := C.UseDelay;
  DelayLengthTrackBar.Position := 100-Round(C.DelayLength*100);
end;

procedure TChannelFrame.VolumeTrackBarChange(Sender: TObject);
begin
  Channel.Volume := ((Sender as TTrackBar).Max-(Sender as TTrackBar).Position) * 1 / (Sender as TTrackBar).Max;
end;

procedure TChannelFrame.ActiveCheckBoxClick(Sender: TObject);
begin
  Channel.Active := (Sender as TCheckBox).Checked;
end;

procedure TChannelFrame.DelayActiveCheckBoxClick(Sender: TObject);
begin
  Channel.UseDelay := (Sender as TCheckBox).Checked;
end;

procedure TChannelFrame.DelayLengthTrackBarChange(Sender: TObject);
begin
  Channel.DelayLength := ((Sender as TTrackBar).Max-(Sender as TTrackBar).Position) * 1 / (Sender as TTrackBar).Max;
end;

end.
