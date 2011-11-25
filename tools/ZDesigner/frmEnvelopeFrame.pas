unit frmEnvelopeFrame;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, AudioPlayer, ExtCtrls;

type
  TEnvelopeFrame = class(TFrame)
    NrLabel: TLabel;
    ActiveCheckBox: TCheckBox;
    AttackTrackBar: TTrackBar;
    Label2: TLabel;
    Label4: TLabel;
    ReleaseTrackBar: TTrackBar;
    DecayTrackBar: TTrackBar;
    SustainTrackBar: TTrackBar;
    Label1: TLabel;
    Label3: TLabel;
    Image1: TImage;
    procedure AttackTrackBarChange(Sender: TObject);
    procedure ReleaseTrackBarChange(Sender: TObject);
    procedure ActiveCheckBoxClick(Sender: TObject);
    procedure DecayTrackBarChange(Sender: TObject);
    procedure SustainTrackBarChange(Sender: TObject);
    procedure TrackBarContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    { Private declarations }
    Envelope : PEnvelope;
    procedure Draw;
  public
    { Public declarations }
    procedure SetEnvelope(E : PEnvelope);
  end;

implementation

{$R *.dfm}

uses Math;

{ TEnvelopeFrame }

procedure TEnvelopeFrame.SetEnvelope(E: PEnvelope);
begin
  Envelope := E;

  ActiveCheckBox.Checked := E.Active;
  AttackTrackBar.Position := Round( (E.AttackTime/EnvTimeScale)*AttackTrackBar.Max );
  DecayTrackBar.Position := Round( (E.DecayTime/EnvTimeScale)*DecayTrackBar.Max );
  SustainTrackBar.Position := Round( E.SustainLevel*SustainTrackBar.Max );
  ReleaseTrackBar.Position := Round( (E.ReleaseTime/EnvTimeScale)*ReleaseTrackBar.Max );

  AttackTrackBar.Tag := integer(@E.AttackTime);
  DecayTrackBar.Tag := integer(@E.DecayTime);
  SustainTrackBar.Tag := integer(@E.SustainLevel);
  ReleaseTrackBar.Tag := integer(@E.ReleaseTime);

  Draw;
end;

procedure TEnvelopeFrame.SustainTrackBarChange(Sender: TObject);
var
  Tb : TTrackBar;
begin
  Tb := (Sender as TTrackBar);
  Envelope.SustainLevel := (Tb.Position * 1.0 / Tb.Max);
  Tb.Hint := 'Sustain level: ' + FloatToStr( RoundTo(Envelope.SustainLevel,-2) );
  Draw;
end;

procedure TEnvelopeFrame.AttackTrackBarChange(Sender: TObject);
var
  Tb : TTrackBar;
begin
  Tb := (Sender as TTrackBar);
  Envelope.AttackTime := (Tb.Position * 1.0 / Tb.Max) * EnvTimeScale;
  Tb.Hint := 'Attack time: ' + FloatToStr( RoundTo(Envelope.AttackTime,-2) ) + ' seconds';
  Draw;
end;

procedure TEnvelopeFrame.TrackBarContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var
  S : string;
  F : ^single;
begin
  F := pointer((Sender as TComponent).Tag);
  S := InputBox('Value editor','Enter new value:',FloatToStr( RoundTo(F^,-2) ) );
  F^ := StrToFloat(S);
  SetEnvelope(Envelope);
end;

procedure TEnvelopeFrame.DecayTrackBarChange(Sender: TObject);
var
  Tb : TTrackBar;
begin
  Tb := (Sender as TTrackBar);
  Envelope.DecayTime := (Tb.Position * 1.0 / Tb.Max) * EnvTimeScale;
  Tb.Hint := 'Decay time: ' + FloatToStr( RoundTo(Envelope.DecayTime,-2) ) + ' seconds';
  Draw;
end;

procedure TEnvelopeFrame.Draw;
var
  C : TCanvas;
  W,H : integer;
  TimeW,PSize : single;
begin
  C := Image1.Canvas;
  W := Image1.Width;
  H := Image1.Height;

  //Time length of graph. Adjust to envelope total time.
  TimeW := Math.Ceil( (Envelope.AttackTime+Envelope.DecayTime+Envelope.ReleaseTime)/2 ) * 2;

  C.Brush.Color := clBlack;
  C.FillRect( Rect(0,0,W, H) );

  //Time length of each pixel.
  if TimeW>0.00001 then
  begin
    PSize := W / TimeW;
    C.Pen.Color := clGreen;
    C.MoveTo(0,H);
    C.LineTo( Round(Envelope.AttackTime*PSize) ,0);
    C.LineTo( C.PenPos.X + Round(Envelope.DecayTime*PSize) ,H-(Round(Envelope.SustainLevel*H)));
    C.LineTo( C.PenPos.X + Round(Envelope.ReleaseTime*PSize) ,H);
  end;
end;

procedure TEnvelopeFrame.ReleaseTrackBarChange(Sender: TObject);
var
  Tb : TTrackBar;
begin
  Tb := (Sender as TTrackBar);
  Envelope.ReleaseTime := (Tb.Position * 1.0 / Tb.Max) * EnvTimeScale;
  Tb.Hint := 'Release time: ' + FloatToStr( RoundTo(Envelope.ReleaseTime,-2) ) + ' seconds';
  Draw;
end;

procedure TEnvelopeFrame.ActiveCheckBoxClick(Sender: TObject);
begin
  Envelope.Active := ActiveCheckBox.Checked;
end;

end.
