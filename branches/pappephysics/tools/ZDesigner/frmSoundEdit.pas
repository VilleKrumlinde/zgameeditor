unit frmSoundEdit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, frmCompEditBase,
  Dialogs, ComCtrls, ExtCtrls, StdCtrls, Spin, ZClasses, DesignerGUI, AudioComponents;

type
  TSoundEditFrame = class(TCompEditFrameBase)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    ModulationsParent: TScrollBox;
    OctaveEdit: TEdit;
    NoteLengthEdit: TEdit;
    LfosParent: TScrollBox;
    Label9: TLabel;
    Label8: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Label13: TLabel;
    Label12: TLabel;
    Label1: TLabel;
    GroupBox4: TGroupBox;
    Label5: TLabel;
    Label11: TLabel;
    NotesEdit: TComboBox;
    AutoPlayCheckBox: TCheckBox;
    TempoTrackBar: TTrackBar;
    GroupBox3: TGroupBox;
    Osc1WaveformCombo: TComboBox;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    FilterActiveCheckBox: TCheckBox;
    FilterCutoffTrackBar: TTrackBar;
    FilterQTrackBar: TTrackBar;
    GroupBox1: TGroupBox;
    Label2: TLabel;
    Osc2ActiveCheckBox: TCheckBox;
    Osc2FreqEdit: TEdit;
    Osc2WaveformCombo: TComboBox;
    HardSyncCheckBox: TCheckBox;
    EnvelopesParent: TScrollBox;
    ChannelEdit: TEdit;
    AutoPlayTimer: TTimer;
    Label10: TLabel;
    ChannelsParent: TScrollBox;
    Label14: TLabel;
    GlobalLfosParent: TScrollBox;
    Label15: TLabel;
    NoteNrLabel: TLabel;
    SoundGraphPaintBox: TPaintBox;
    SoundGraphTimer: TTimer;
    DumpButton: TButton;
    VolumeTrackBar: TTrackBar;
    Label16: TLabel;
    PanTrackBar: TTrackBar;
    Label17: TLabel;
    HelpButton: TButton;
    HelpMixerButton: TButton;
    UpDown1: TUpDown;
    Osc2VolumeTrackBar: TTrackBar;
    Label18: TLabel;
    Osc1PWTrackBar: TTrackBar;
    MasterVolumeTrackBar: TTrackBar;
    Label19: TLabel;
    Panel1: TPanel;
    procedure NotesEditKeyPress(Sender: TObject; var Key: Char);
    procedure AutoPlayCheckBoxClick(Sender: TObject);
    procedure AutoPlayTimerTimer(Sender: TObject);
    procedure TempoTrackBarChange(Sender: TObject);
    procedure HardSyncCheckBoxClick(Sender: TObject);
    procedure SoundGraphTimerTimer(Sender: TObject);
    procedure SoundGraphPaintBoxPaint(Sender: TObject);
    procedure DumpButtonClick(Sender: TObject);
    procedure VolumeTrackBarChange(Sender: TObject);
    procedure PanTrackBarChange(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure HelpMixerButtonClick(Sender: TObject);
    procedure MasterVolumeTrackBarChange(Sender: TObject);
  private
    Sound : TSound;
    procedure PlayNoteFromChar(Key: char);
    procedure SetupMixerGui;
    { Private declarations }
  public
    procedure SetComponent(C: TZComponent; TreeNode: TZComponentTreeNode);  override;
    procedure OnKeyPress(var Key: Char); override;
    constructor Create(AOwner: TComponent) ; override;
  end;

var
  SoundEditFrame: TSoundEditFrame;

implementation

{$R *.dfm}

uses MMSystem, AudioPlayer, frmModulationFrame,frmLfoFrame,frmChannelFrame,
  frmEnvelopeFrame,ZPlatform,Math, uHelp, System.Types;


var
  SoundGraphMutex : pointer;
  SoundGraphWriteIndex : integer;
  SoundGraphReadIndex : integer;
  SoundGraphMaxLength : integer;
  SoundGraphPixelPos : integer;
  SoundGraphBuffer : array[0..(5*AudioRate)*AudioPlayer.StereoChannels] of TSoundOutputUnit;
  SoundGraphBitmap : TBitmap;
  SoundGraphSavePenPos : array[0..StereoChannels-1] of TPoint;


procedure ReceiveAudioFunc(P : pointer; SampleCount : integer);
begin
  Platform_EnterMutex(SoundGraphMutex);
    if SoundGraphWriteIndex+(SampleCount * AudioPlayer.StereoChannels)>SoundGraphMaxLength then
      SampleCount := (SoundGraphMaxLength - SoundGraphWriteIndex) div AudioPlayer.StereoChannels;
    if SampleCount>0 then
    begin
      Move( P^, SoundGraphBuffer[SoundGraphWriteIndex], SampleCount * SizeOf(TSoundOutputUnit) * AudioPlayer.StereoChannels);
      Inc(SoundGraphWriteIndex,SampleCount * AudioPlayer.StereoChannels);
    end;
  Platform_LeaveMutex(SoundGraphMutex);
end;

procedure TSoundEditFrame.PlayNoteFromChar(Key : char);
const
  Notes : string = 'asdfghjklöä';
var
  I : integer;
  NoteLength,NoteNr : single;
begin
  for I := 1 to Length(Notes) do
  begin
    if Notes[I]=Key then
    begin

      Sound.Voice.Osc1.Waveform := TWaveform(Osc1WaveformCombo.ItemIndex);
      Sound.Voice.Osc1.PulseWidth := Round(Osc1PWTrackBar.Position) / 100;

      Sound.Voice.UseOsc2 := Osc2ActiveCheckBox.Checked;
      Sound.Voice.Osc2.NoteModifier := StrToIntDef(Osc2FreqEdit.Text,0) * 0.1;
      Sound.Voice.Osc2.Waveform := TWaveform(Osc2WaveformCombo.ItemIndex);
      Sound.Voice.Osc2Volume := Round(Osc2VolumeTrackBar.Position) / 100;

      Sound.Voice.UseFilter := FilterActiveCheckBox.Checked;
      Sound.Voice.FilterCutoff := Round(FilterCutoffTrackBar.Position) / 100.1;
      Sound.Voice.FilterQ := Round(FilterQTrackBar.Position) / 100.1;

      NoteLength := StrToFloatDef(NoteLengthEdit.Text,0.25);
      Sound.Voice.Length := NoteLength;

      NoteNr := (StrToIntDef(OctaveEdit.Text,0)*12) + I - 1;
      AudioPlayer.AddNoteToEmitList(@Sound.Voice, NoteNr, StrToIntDef(ChannelEdit.Text,0), NoteLength, 1.0);

      NoteNrLabel.Caption := 'NoteNr ' + IntToStr(Round(NoteNr));

      //Init sound graph
      Platform_EnterMutex(SoundGraphMutex);
      try
        SoundGraphWriteIndex := 0;
        SoundGraphReadIndex := 0;
        SoundGraphMaxLength := Round(Min(NoteLength * AudioRate * StereoChannels,High(SoundGraphBuffer)));
        SoundGraphPixelPos := 0;
        if SoundGraphBitmap=nil then
        begin
          SoundGraphBitmap := TBitmap.Create;
          SoundGraphBitmap.Height := SoundGraphPaintBox.Height;
          SoundGraphBitmap.Transparent := False;
        end;
        SoundGraphBitmap.Width := SoundGraphPaintBox.Width;
        FillChar(SoundGraphSavePenPos,SizeOf(SoundGraphSavePenPos),0);
        SoundGraphBitmap.Canvas.Brush.Color := clBlack;
        SoundGraphBitmap.Canvas.FillRect( Rect(0,0,SoundGraphBitmap.Width,SoundGraphBitmap.Height) );
//    FillChar(SoundGraphBuffer,SizeOf(SoundGraphBuffer),128);
      finally
        Platform_LeaveMutex(SoundGraphMutex);
      end;

      AudioPlayer.EmitSoundsInEmitList;

      Break;
    end;
  end;
end;

procedure TSoundEditFrame.NotesEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not AutoPlayCheckBox.Checked then
    PlayNoteFromChar(Key);
end;

procedure TSoundEditFrame.AutoPlayCheckBoxClick(Sender: TObject);
begin
  AutoPlayTimer.Enabled := AutoPlayCheckBox.Checked;
end;

var
  AutoPlayCharI : integer;

procedure TSoundEditFrame.AutoPlayTimerTimer(Sender: TObject);
begin
  if Length(NotesEdit.Text)=0 then
    Exit;
  Inc(AutoPlayCharI);
  if AutoPlayCharI>Length(NotesEdit.Text) then
    AutoPlayCharI:=1;
  PlayNoteFromChar(NotesEdit.Text[AutoPlayCharI]);
end;

constructor TSoundEditFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Panel1.DoubleBuffered := True;
end;

procedure TSoundEditFrame.TempoTrackBarChange(Sender: TObject);
begin
  AutoPlayTimer.Interval := 250 + ((5-TempoTrackBar.Position)*20);
end;

procedure TSoundEditFrame.HardSyncCheckBoxClick(Sender: TObject);
begin
  Sound.Voice.HardSync := (Sender as TCheckBox).Checked;
end;

procedure TSoundEditFrame.HelpButtonClick(Sender: TObject);
begin
  ShowHelp('ComponentRef.Sound');
end;

procedure TSoundEditFrame.HelpMixerButtonClick(Sender: TObject);
begin
  ShowHelp('ComponentRef.AudioMixer');
end;

procedure TSoundEditFrame.MasterVolumeTrackBarChange(Sender: TObject);
begin
  AudioPlayer.MasterVolume := (Sender as TTrackBar).Position * 1 / (Sender as TTrackBar).Max;
end;

procedure TSoundEditFrame.SetComponent(C: TZComponent; TreeNode: TZComponentTreeNode);
var
  I : integer;
  ModulationFrame : TModulationFrame;
  Modulation : PModulation;
  LfoFrame : TLfoFrame;
  Lfo : PLfo;
  ChannelFrame : TChannelFrame;
  Channel : PChannel;
  P : TPanel;
  Envelope : PEnvelope;
  EnvelopeFrame : TEnvelopeFrame;
  IsGuiCreated : boolean;
begin
  inherited;
  Self.Sound := C as TSound;

  Osc1WaveformCombo.ItemIndex := Ord(Sound.Voice.Osc1.Waveform);
  Osc1PWTrackBar.Position := Round(Sound.Voice.Osc1.PulseWidth*100);

  Osc2ActiveCheckBox.Checked := Sound.Voice.UseOsc2;
  Osc2FreqEdit.Text := IntToStr(Round(Sound.Voice.Osc2.NoteModifier * 10));
  Osc2WaveformCombo.ItemIndex := Ord(Sound.Voice.Osc2.Waveform);
  Osc2VolumeTrackBar.Position := Round(Sound.Voice.Osc2Volume*100);

  FilterActiveCheckBox.Checked := Sound.Voice.UseFilter;
  FilterCutoffTrackBar.Position := Round(Sound.Voice.FilterCutoff*100);
  FilterQTrackBar.Position := Round(Sound.Voice.FilterQ*100);

  NoteLengthEdit.Text := FormatFloat('0.0',Sound.Voice.Length);

  VolumeTrackBar.Position := Round(Sound.Voice.Volume*100);

  PanTrackBar.Position := Round(Sound.Voice.Pan*100);

//  Hide;

  IsGuiCreated := ModulationsParent.ControlCount>0;

  for I := 0 to High(Sound.Voice.Modulations) do
  begin
    Modulation := @Sound.Voice.Modulations[I];
    if not IsGuiCreated then
    begin
      ModulationFrame := TModulationFrame.Create(ModulationsParent);
      ModulationFrame.NrLabel.Caption := IntToStr(I+1);
      ModulationFrame.Name := '';
      ModulationFrame.Parent := ModulationsParent;
      ModulationFrame.Top := 1000;
      ModulationFrame.Align := alTop;
    end
    else
      ModulationFrame := ModulationsParent.Controls[I] as TModulationFrame;
    ModulationFrame.SetModulation(Modulation);
  end;

  for I := 0 to High(Sound.Voice.Lfos) do
  begin
    Lfo := @Sound.Voice.Lfos[I];
    if not IsGuiCreated then
    begin
      LfoFrame := TLfoFrame.Create(LfosParent);
      LfoFrame.NrLabel.Caption := IntToStr(I+1);
      LfoFrame.Name := '';
      LfoFrame.Parent := LfosParent;
      LfoFrame.Top := 1000;
      LfoFrame.Align := alTop;
    end
    else
      LfoFrame := LfosParent.Controls[I] as TLfoFrame;
    LfoFrame.SetLfo(Lfo);
  end;

  for I := 0 to MaxChannels-1 do
  begin
    Channel := AudioPlayer.GetChannel(I);
    if not IsGuiCreated then
    begin
      //Lägg i en panel för att få en ram
      P :=  TPanel.Create(ChannelsParent);
      P.BorderStyle := bsNone;
      P.BevelOuter := bvNone;
      P.BevelInner := bvNone;
      P.Width := 26;

      ChannelFrame := TChannelFrame.Create(ChannelsParent);
      ChannelFrame.NrLabel.Caption := IntToStr(I);
      ChannelFrame.Name := '';
      ChannelFrame.Parent := P;
      ChannelFrame.Align := alClient;

      P.Parent := ChannelsParent;
      P.Left := 1000;
      P.Align := alLeft;
    end
    else
      ChannelFrame := (ChannelsParent.Controls[I] as TPanel).Controls[0] as TChannelFrame;
    ChannelFrame.SetChannel(Channel);
  end;

  for I := 0 to High(Sound.Voice.Envelopes) do
  begin
    Envelope := @Sound.Voice.Envelopes[I];
    if not IsGuiCreated then
    begin
      EnvelopeFrame := TEnvelopeFrame.Create(EnvelopesParent);
      EnvelopeFrame.NrLabel.Caption := IntToStr(I+1);
      EnvelopeFrame.Name := '';
      EnvelopeFrame.Parent := EnvelopesParent;
      EnvelopeFrame.Top := 1000;
      EnvelopeFrame.Align := alTop;
    end
    else
      EnvelopeFrame := EnvelopesParent.Controls[I] as TEnvelopeFrame;
    EnvelopeFrame.SetEnvelope(Envelope);
  end;

  SetupMixerGui;

  Platform_EnterMutex( SoundGraphMutex );
    ZPlatform.Platform_DesignerSetAudioCallback(ReceiveAudioFunc);
  Platform_LeaveMutex( SoundGraphMutex );
end;

procedure TSoundEditFrame.OnKeyPress(var Key: Char);
var
  P : TPoint;
begin
  if CharInSet(Key,['a','s','d','f','g','h','j','k','l','ö','ä']) then
  begin
    P := Self.ClientToScreen( Point(0,0) );
    if PtInRect( Rect(P.X,P.Y,P.X + Width, P.Y + Height) , Mouse.CursorPos ) then
    begin
      NotesEditKeyPress(nil,Key);
      Key := #0;
    end;
  end;
end;

procedure TSoundEditFrame.SetupMixerGui;
var
  I : integer;
  LfoFrame : TLfoFrame;
  Lfo : PLfo;
  IsGuiCreated : boolean;
begin
  IsGuiCreated := GlobalLfosParent.ControlCount>0;

  for I := 0 to High(Sound.Voice.Lfos) do
  begin
    Lfo := @GlobalLfos[I];
    if not IsGuiCreated then
    begin
      LfoFrame := TLfoFrame.Create(GlobalLfosParent);
      LfoFrame.NrLabel.Caption := IntToStr(I+1);
      LfoFrame.Name := '';
      LfoFrame.Parent := GlobalLfosParent;
      LfoFrame.Top := 1000;
      LfoFrame.Align := alTop;
    end
    else
      LfoFrame := GlobalLfosParent.Controls[I] as TLfoFrame;
    LfoFrame.SetLfo(Lfo);
  end;

  MasterVolumeTrackBar.Position := Round(AudioPlayer.MasterVolume*100);
end;

procedure TSoundEditFrame.SoundGraphTimerTimer(Sender: TObject);
var
  D,Y,I,J,ISample,MaxDots,YSize : integer;
  FSample : single;
begin
  if (SoundGraphReadIndex>=SoundGraphMaxLength) or
      (SoundGraphPixelPos>=SoundGraphBitmap.Width) then
    Exit;

  Platform_EnterMutex(SoundGraphMutex);
  try

    //Draw bounds
    YSize := SoundGraphBitmap.Height div (2 * StereoChannels);
    SoundGraphBitmap.Canvas.Pen.Color := clGreen;
    for J := 0 to StereoChannels-1 do
    begin
      FSample := 1;
      Y := (YSize + J*(YSize*2)) - Round(FSample * YSize);
      SoundGraphBitmap.Canvas.PenPos := Point(0,Y);
      SoundGraphBitmap.Canvas.LineTo(SoundGraphBitmap.Width,Y);

      FSample := -1;
      Y := (YSize + J*(YSize*2)) - Round(FSample * YSize);
      SoundGraphBitmap.Canvas.PenPos := Point(0,Y);
      SoundGraphBitmap.Canvas.LineTo(SoundGraphBitmap.Width,Y);
    end;


    //Antal samples att stega i buffer för varje pixel
    D := Round( (SoundGraphMaxLength/StereoChannels)/SoundGraphBitmap.Width );
    //Max antal pixels att rita under en timeranrop
    MaxDots := Round(Min(SoundGraphBitmap.Width div 5,50));
    SoundGraphBitmap.Canvas.Pen.Color := clRed;
    I := 0;
    while (SoundGraphReadIndex<SoundGraphMaxLength) and
      (SoundGraphReadIndex<SoundGraphWriteIndex) and
      (SoundGraphPixelPos<SoundGraphBitmap.Width) and
      (I<MaxDots) do
    begin

      for J := 0 to StereoChannels-1 do
      begin
        ISample := SoundGraphBuffer[SoundGraphReadIndex+J];

        if ISample>=0 then
          FSample := ISample/High(TSoundOutputUnit)
        else
          FSample := ISample/Abs(Low(TSoundOutputUnit));
        Y := (YSize + J*(YSize*2)) -
          Round(FSample * YSize);

        SoundGraphBitmap.Canvas.PenPos := SoundGraphSavePenPos[J];
        if SoundGraphPixelPos=0 then
          SoundGraphBitmap.Canvas.PenPos := Point(0,Y)
        else
          SoundGraphBitmap.Canvas.LineTo(SoundGraphPixelPos,Y);
        SoundGraphSavePenPos[J] := SoundGraphBitmap.Canvas.PenPos;
      end;

      Inc(SoundGraphReadIndex,D * StereoChannels);
      Inc(SoundGraphPixelPos);
      Inc(I);
    end;

  finally
    Platform_LeaveMutex(SoundGraphMutex);
  end;

  SoundGraphPaintBox.Invalidate;
end;

procedure TSoundEditFrame.SoundGraphPaintBoxPaint(Sender: TObject);
begin
  SoundGraphPaintBox.Canvas.Draw(0,0,SoundGraphBitmap);
end;

procedure TSoundEditFrame.VolumeTrackBarChange(Sender: TObject);
begin
  Sound.Voice.Volume := (Sender as TTrackBar).Position * 1 / (Sender as TTrackBar).Max;
end;

procedure TSoundEditFrame.PanTrackBarChange(Sender: TObject);
begin
  Sound.Voice.Pan := (Sender as TTrackBar).Position * 1 / (Sender as TTrackBar).Max;
end;

var
  SaveDialog : TSaveDialog;

procedure TSoundEditFrame.DumpButtonClick(Sender: TObject);
var
  F : file;
begin
  if SaveDialog=nil then
  begin
    SaveDialog := TSaveDialog.Create(Self);
    SaveDialog.Filter := 'RAW audio file (*.raw)|*.raw';
    SaveDialog.DefaultExt := '*.raw';
  end;
  if not SaveDialog.Execute then
    Exit;
  AssignFile(F,SaveDialog.FileName);
  try
    Rewrite(F,1);
    BlockWrite(F,SoundGraphBuffer,SoundGraphMaxLength*SizeOf(TSoundOutputUnit) );
  finally
    CloseFile(F);
  end;
end;

initialization
  SoundGraphMutex :=  Platform_CreateMutex;
finalization
  Platform_FreeMutex(SoundGraphMutex);
  if SoundGraphBitmap<>nil then
    SoundGraphBitmap.Free;
end.
