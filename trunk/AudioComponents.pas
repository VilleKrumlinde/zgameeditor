{Copyright (c) 2008 Ville Krumlinde

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

unit AudioComponents;

interface

uses ZClasses,AudioPlayer;

type
  TSound = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Voice : TVoiceEntry;
    constructor Create(OwnerList: TZComponentList); override;
  end;

  TPlaySound = class(TCommand)
  private
    LastPlayed : single;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Sound : TSound;
    NoteNr : single;
    Channel : integer;
    ReplayDelay : single;
    procedure Execute; override;
    {$ifndef minimal}
    function GetDisplayName: AnsiString; override;
    {$endif}
  end;

  //todo: force single instance
  TAudioMixer = class(TZComponent)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  end;

  TMusic = class(TZComponent)
  private
    IsPlaying : boolean;
    Stream : TZInputStream;
    NextEventTime : single;
    LocalTime : single;
    CurrentInstruments : array[0..15] of integer;
    ChannelVolumes : array[0..15] of single;
    procedure GetNextEventTime;
    function ReadVarLength: Integer;
    procedure ProcessNextEvent;
  {$ifndef minimal}public{$else}private{$endif}
    procedure AdvanceMusic(const DeltaTime : single);
    procedure Start;
    procedure Stop;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Instruments : TZComponentList;
    MusicFile : TZBinaryPropValue;
    OnPlayNote : TZComponentList;
    Tempo : single;
    Volume : single;
    NoteParam,NoteChannelParam,NoteLengthParam : single;
    destructor Destroy; override;
    procedure Update; override;
  end;

  TMusicControl = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (mckStart,mckStop);
    Music : TMusic;
    procedure Execute; override;
  end;


implementation

uses ZApplication
  {$ifndef minimal}
  ,SysUtils
  {$endif}
  ;

{ TSound }

constructor TSound.Create(OwnerList: TZComponentList);
begin
  inherited;
{$ifndef minimal}
  //Default values in designer: Connect env1 with volume
  Voice.Modulations[0].Active := True;
  Voice.Modulations[0].Source := msEnv1;
  Voice.Modulations[0].Destination := mdVolume;
  Voice.Modulations[0].Amount := 1;
  Voice.Envelopes[0].Active := True;
  Voice.Envelopes[0].ReleaseTime := 0.2;
{$endif}
end;

procedure TSound.DefineProperties(List: TZPropertyList);
var
  I : integer;
  Modulation : PModulation;
  Lfo : PLfo;
//  Channel : PChannel;
  Envelope : PEnvelope;
  {$ifndef minimal}
  S : string;
  SaveCount : integer;
  {$endif}
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Length',{$ENDIF}integer(@Voice.Length) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'Volume',{$ENDIF}integer(@Voice.Volume) - integer(Self), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 0.25;
  List.AddProperty({$IFNDEF MINIMAL}'BaseNoteNr',{$ENDIF}integer(@Voice.BaseNoteNr) - integer(Self), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}'Osc1Waveform',{$ENDIF}integer(@Voice.Osc1.Waveform) - integer(Self), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Osc1NoteModifier',{$ENDIF}integer(@Voice.Osc1.NoteModifier) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Osc1PW',{$ENDIF}integer(@Voice.Osc1.PulseWidth) - integer(Self), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}'UseOsc2',{$ENDIF}integer(@Voice.UseOsc2) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Osc2Waveform',{$ENDIF}integer(@Voice.Osc2.Waveform) - integer(Self), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Osc2NoteModifier',{$ENDIF}integer(@Voice.Osc2.NoteModifier) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Osc2Volume',{$ENDIF}integer(@Voice.Osc2Volume) - integer(Self), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'HardSync',{$ENDIF}integer(@Voice.HardSync) - integer(Self), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'UseFilter',{$ENDIF}integer(@Voice.UseFilter) - integer(Self), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'FilterCutoff',{$ENDIF}integer(@Voice.FilterCutoff) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FilterQ',{$ENDIF}integer(@Voice.FilterQ) - integer(Self), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}'Pan',{$ENDIF}integer(@Voice.Pan) - integer(Self), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 0.5;

  {$ifndef minimal}
  SaveCount := List.Count;
  {$endif}

  for I := 0 to High(Voice.Modulations) do
  begin
    {$ifndef minimal}
    S := 'Mod' + Chr(Ord('0')+I);
    {$endif}
    Modulation := @Voice.Modulations[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}integer(@Modulation.Active) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Source',{$ENDIF}integer(@Modulation.Source) - integer(Self), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Destination',{$ENDIF}integer(@Modulation.Destination) - integer(Self), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Amount',{$ENDIF}integer(@Modulation.Amount) - integer(Self), zptFloat);
  end;

  for I := 0 to High(Voice.Envelopes) do
  begin
    {$ifndef minimal}
    S := 'Env' + Chr(Ord('0')+I);
    {$endif}
    Envelope := @Voice.Envelopes[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}integer(@Envelope.Active) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'AttackTime',{$ENDIF}integer(@Envelope.AttackTime) - integer(Self), zptFloat);
    List.AddProperty({$IFNDEF MINIMAL}S + 'DecayTime',{$ENDIF}integer(@Envelope.DecayTime) - integer(Self), zptFloat);
    List.AddProperty({$IFNDEF MINIMAL}S + 'SustainLevel',{$ENDIF}integer(@Envelope.SustainLevel) - integer(Self), zptFloat);
      List.GetLast.DefaultValue.FloatValue := 1.0;
    List.AddProperty({$IFNDEF MINIMAL}S + 'ReleaseTime',{$ENDIF}integer(@Envelope.ReleaseTime) - integer(Self), zptFloat);
  end;

  for I := 0 to High(Voice.Lfos) do
  begin
    {$ifndef minimal}
    S := 'Lfo' + Chr(Ord('0')+I);
    {$endif}
    Lfo := @Voice.Lfos[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}integer(@Lfo.Active) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'IsBipolar',{$ENDIF}integer(@Lfo.IsBipolar) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Style',{$ENDIF}integer(@Lfo.Style) - integer(Self), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Speed',{$ENDIF}integer(@Lfo.Speed) - integer(Self), zptFloat);
  end;

  {$ifndef minimal}
  for I := SaveCount to List.Count-1 do
    TZProperty(List[I]).HideInGui := True;
  {$endif}

  {$ifndef minimal}
  Assert(SizeOf(Voice.SampleData)=SizeOf(TZBinaryPropValue));
  {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SampleData',{$ENDIF}integer(@Voice.SampleData) - integer(Self), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}'SampleRepeatPosition',{$ENDIF}integer(@Voice.SampleRepeatPosition) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'SampleRate',{$ENDIF}integer(@Voice.SampleRate) - integer(Self), zptFloat);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SampleFormat',{$ENDIF}integer(@Voice.SampleFormat) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.SetOptions(['8 bit signed','16 bit signed']);{$endif}
end;

{ TPlaySound }

procedure TPlaySound.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Sound',{$ENDIF}integer(@Sound) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSound]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'NoteNr',{$ENDIF}integer(@NoteNr) - integer(Self), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Channel',{$ENDIF}integer(@Channel) - integer(Self), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'ReplayDelay',{$ENDIF}integer(@ReplayDelay) - integer(Self), zptFloat);
end;

procedure TPlaySound.Execute;
begin
  {$ifndef minimal}if Sound=nil then Exit;{$endif}

  if NoSound then
    Exit;

  //  Sound.Play;
  if (ReplayDelay>0) then
  begin
    if (LastPlayed>0) and (ZApp.Time<Self.LastPlayed+ReplayDelay) then
      Exit;
    Self.LastPlayed := ZApp.Time;
  end;
  AudioPlayer.AddNoteToEmitList(@Sound.Voice, NoteNr, Channel, 0, 1.0);
end;

{$ifndef minimal}
function TPlaySound.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Sound) then
    Result := Result + '  ' + Sound.Name;
end;
{$endif}

{ TMusic }

const
  MidiTimeScale = 0.01;  //Midi times to seconds

function TMusic.ReadVarLength : Integer;
var
  i: integer;
  b: byte;
begin
  b := $80;
  i := $0;
  while b > $7F do
  begin
    i := i shl 7;
    Stream.Read(B,1);
    i := i + b and $7F;
  end;
  Result := i;
end;

procedure TMusic.ProcessNextEvent;
var
  Cmd,NoteNr,Ch,B : byte;
  InstrumentNr : integer;
  NoteLength,Velocity : single;
  Sound : TSound;
begin
  Stream.Read(Cmd,1);
  Ch := Cmd and 15;
  case Cmd shr 4 of
    $9 : //Note on
      begin
        Stream.Read(NoteNr,1);
        NoteLength := ReadVarLength * MidiTimeScale;
        Stream.Read(B,1);   //Velocity 0..127
        Velocity := B * (1/127);  //Convert velocity to 0..1 range
        Velocity := Velocity * Self.ChannelVolumes[Ch] * Self.Volume;
        InstrumentNr := Self.CurrentInstruments[Ch];
        if (InstrumentNr<Instruments.Count) and AudioPlayer.GetChannel(Ch).Active then
        begin
          Sound := TSound(Self.Instruments[InstrumentNr]);
          NoteLength := NoteLength + Sound.Voice.Envelopes[0].ReleaseTime;
          if not NoSound then
            AudioPlayer.AddNoteToEmitList(@Sound.Voice, NoteNr, Ch, NoteLength, Velocity);
          if Self.OnPlayNote.Count>0 then
          begin
            Self.NoteParam := NoteNr;
            Self.NoteChannelParam := Ch;  //int to float
            Self.NoteLengthParam := NoteLength;
            Self.OnPlayNote.ExecuteCommands;
          end;
        end;
      end;
    $B : //Channel volume
      begin
        Stream.Read(B,1);
        Self.ChannelVolumes[Ch] := B * (1/127);  //Convert volume to 0..1 range
      end;
    $C : //Program change
      begin
        Stream.Read(B,1);
        Self.CurrentInstruments[Ch] := B;
      end;
  end;
end;

procedure TMusic.AdvanceMusic(const DeltaTime: single);
begin
  LocalTime := LocalTime + DeltaTime;// * Self.Tempo;
  while (Self.LocalTime>=Self.NextEventTime) and (Self.IsPlaying) do
  begin
    ProcessNextEvent;
    GetNextEventTime;
  end;
end;

procedure TMusic.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'MusicFile',{$ENDIF}integer(@MusicFile) - integer(Self), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}'Instruments',{$ENDIF}integer(@Instruments) - integer(Self), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSound]);{$endif}

//  List.AddProperty({$IFNDEF MINIMAL}'Tempo',{$ENDIF}integer(@Tempo) - integer(Self), zptFloat);
//    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'Volume',{$ENDIF}integer(@Volume) - integer(Self), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'OnPlayNote',{$ENDIF}integer(@OnPlayNote) - integer(Self), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'NoteParam',{$ENDIF}integer(@NoteParam) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'NoteChannelParam',{$ENDIF}integer(@NoteChannelParam) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'NoteLengthParam',{$ENDIF}integer(@NoteLengthParam) - integer(Self), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

destructor TMusic.Destroy;
begin
  Stop;
  inherited;
end;

procedure TMusic.GetNextEventTime;
var
  I : integer;
begin
  if Stream.Position>=Stream.Size then
    Stop
  else
  begin
    I := ReadVarLength;
    Self.NextEventTime := Self.NextEventTime + (I * MidiTimeScale);
  end;
end;

procedure TMusic.Start;
var
  I : integer;
begin
  Stop;
  Stream := TZInputStream.CreateFromMemory(MusicFile.Data,MusicFile.Size);
  Self.LocalTime := 0.0; //Tiden där "nålen" är och spelar musik från skivan
  Self.NextEventTime := 0.0;
  IsPlaying := True;
  FillChar(CurrentInstruments,SizeOf(CurrentInstruments),0);
  for I := 0 to High(ChannelVolumes) do
    ChannelVolumes[I] := 1.0;
  GetNextEventTime;
end;

procedure TMusic.Stop;
begin
  if Stream<>nil then
  begin
    Stream.Free;
    Stream := nil;
  end;
  IsPlaying := False;
end;

procedure TMusic.Update;
begin
  if IsPlaying then
    AdvanceMusic(ZApp.DeltaTime);
end;

{ TMusicControl }

procedure TMusicControl.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}integer(@Kind) - integer(Self), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Start','Stop']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Music',{$ENDIF}integer(@Music) - integer(Self), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMusic]);{$endif}
end;

procedure TMusicControl.Execute;
begin
  case Kind of
    mckStart :
      begin
        {$ifndef minimal}if Music=nil then Exit;{$endif}
        Music.Start;
        ZApp.CurrentMusic := Music;
      end;
    mckStop : ZApp.CurrentMusic := nil;
  end;
end;

{ TAudioMixer }

procedure TAudioMixer.DefineProperties(List: TZPropertyList);
var
  I : integer;
  {$ifndef minimal}
  S : string;
  {$endif}
  Channel : PChannel;
  Lfo : PLfo;
begin
  inherited;

  List.AddProperty({$IFNDEF MINIMAL}'MasterVolume',{$ENDIF}integer(@AudioPlayer.MasterVolume) - integer(Self), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 1.0;

  //Läser/skriver till global data
  //Fungerar så länge det enbart finns en instans
  for I := 0 to MaxChannels-1 do
  begin
    {$ifndef minimal}
    S := 'Ch' + IntToStr(I);
    {$endif}
    Channel := AudioPlayer.GetChannel(I);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}integer(@Channel.Active) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Volume',{$ENDIF}integer(@Channel.Volume) - integer(Self), zptScalar);
    List.AddProperty({$IFNDEF MINIMAL}S + 'UseDelay',{$ENDIF}integer(@Channel.UseDelay) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'DelayLength',{$ENDIF}integer(@Channel.DelayLength) - integer(Self), zptScalar);
  end;

  for I := 0 to High(GlobalLfos) do
  begin
    {$ifndef minimal}
    S := 'Lfo' + Chr(Ord('0')+I);
    {$endif}
    Lfo := @GlobalLfos[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}integer(@Lfo.Active) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'IsBipolar',{$ENDIF}integer(@Lfo.IsBipolar) - integer(Self), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Style',{$ENDIF}integer(@Lfo.Style) - integer(Self), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Speed',{$ENDIF}integer(@Lfo.Speed) - integer(Self), zptFloat);
  end;
end;

initialization

  ZClasses.Register(TSound,SoundClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=1;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TPlaySound,PlaySoundClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=18;{$endif}
  ZClasses.Register(TAudioMixer,AudioMixerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Mix audio. Only one in each project.';{$endif}
    {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
    ComponentManager.LastAdded.HasGlobalData := True;
    {$ifend}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=17;{$endif}

  ZClasses.Register(TMusic,MusicClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=16;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}

  ZClasses.Register(TMusicControl,MusicControlClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Start/stop playing of music';{$endif}

end.
