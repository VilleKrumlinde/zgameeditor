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
    {$ifndef minimal}
    PatternString : TPropString;
    {$endif}
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
    ByReference : boolean;
    procedure Execute; override;
    {$ifndef minimal}
    procedure DesignerReset; override;
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
    LoopPlayback : boolean;
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

  TSampleUnit = single;
  PSampleUnit = ^TSampleUnit;
  PSampleUnits = PFloatArray;
  TSample = class(TContent)
  private
    Memory : PSampleUnit;
    procedure CleanUp;
    procedure ReInit;
  protected
    procedure CopyAndDestroy(Source : TContent); override;
    procedure DefineProperties(List: TZPropertyList); override;
  public
    SampleCount : integer;
    Length : single;
    destructor Destroy; override;
    function GetMemory : PSampleUnit;
  end;

  TSampleExpression = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    Expression : TZExpressionPropValue;
    Time,Sample : single;
  end;

  TSampleImport = class(TContentProducer)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    SampleData : TZBinaryPropValue;
    SampleRate : single;
    SampleFormat : (sfoSigned8bit,sfoSigned16bit);
  end;

implementation

uses ZApplication,ZExpressions,ZMath
  {$ifndef minimal}
  ,SysUtils
  {$endif}
  ;

{ TSound }

constructor TSound.Create(OwnerList: TZComponentList);
begin
  inherited;
{$ifndef minimal}
//  //Default values in designer: Connect env1 with volume
//  Voice.Modulations[0].Active := True;
//  Voice.Modulations[0].Source := msEnv1;
//  Voice.Modulations[0].Destination := mdVolume;
//  Voice.Modulations[0].Amount := 1;
//  Voice.Envelopes[0].Active := True;
//  Voice.Envelopes[0].ReleaseTime := 0.2;
{$endif}
end;

procedure TSound.DefineProperties(List: TZPropertyList);
var
  I : integer;
  Modulation : PModulation;
  Lfo : PLfo;
  Envelope : PEnvelope;
  {$ifndef minimal}
  S : string;
  SaveCount : integer;
  {$endif}
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Length',{$ENDIF}(@Voice.Length), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'Volume',{$ENDIF}(@Voice.Volume), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 0.25;
  List.AddProperty({$IFNDEF MINIMAL}'BaseNoteNr',{$ENDIF}(@Voice.BaseNoteNr), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}'Osc1Waveform',{$ENDIF}(@Voice.Osc1.Waveform), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Osc1NoteModifier',{$ENDIF}(@Voice.Osc1.NoteModifier), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Osc1PW',{$ENDIF}(@Voice.Osc1.PulseWidth), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}'UseOsc2',{$ENDIF}(@Voice.UseOsc2), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'Osc2Waveform',{$ENDIF}(@Voice.Osc2.Waveform), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}'Osc2NoteModifier',{$ENDIF}(@Voice.Osc2.NoteModifier), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Osc2Volume',{$ENDIF}(@Voice.Osc2Volume), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'HardSync',{$ENDIF}(@Voice.HardSync), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'UseFilter',{$ENDIF}(@Voice.UseFilter), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}'FilterCutoff',{$ENDIF}(@Voice.FilterCutoff), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'FilterQ',{$ENDIF}(@Voice.FilterQ), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}'Pan',{$ENDIF}(@Voice.Pan), zptScalar);
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
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}(@Modulation.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Source',{$ENDIF}(@Modulation.Source), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Destination',{$ENDIF}(@Modulation.Destination), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Amount',{$ENDIF}(@Modulation.Amount), zptFloat);
  end;

  for I := 0 to High(Voice.Envelopes) do
  begin
    {$ifndef minimal}
    S := 'Env' + Chr(Ord('0')+I);
    {$endif}
    Envelope := @Voice.Envelopes[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}(@Envelope.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'AttackTime',{$ENDIF}(@Envelope.AttackTime), zptFloat);
    List.AddProperty({$IFNDEF MINIMAL}S + 'DecayTime',{$ENDIF}(@Envelope.DecayTime), zptFloat);
    List.AddProperty({$IFNDEF MINIMAL}S + 'SustainLevel',{$ENDIF}(@Envelope.SustainLevel), zptFloat);
      List.GetLast.DefaultValue.FloatValue := 1.0;
    List.AddProperty({$IFNDEF MINIMAL}S + 'ReleaseTime',{$ENDIF}(@Envelope.ReleaseTime), zptFloat);
  end;

  for I := 0 to High(Voice.Lfos) do
  begin
    {$ifndef minimal}
    S := 'Lfo' + Chr(Ord('0')+I);
    {$endif}
    Lfo := @Voice.Lfos[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}(@Lfo.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'IsBipolar',{$ENDIF}(@Lfo.IsBipolar), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Style',{$ENDIF}(@Lfo.Style), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Speed',{$ENDIF}(@Lfo.Speed), zptFloat);
  end;

  {$ifndef minimal}
  for I := SaveCount to List.Count-1 do
    TZProperty(List[I]).HideInGui := True;
  {$endif}

  List.AddProperty({$IFNDEF MINIMAL}'Sample',{$ENDIF}(@Voice.SampleRef), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSample]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SampleRepeatPosition',{$ENDIF}(@Voice.SampleRepeatPosition), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'UseSampleHz',{$ENDIF}(@Voice.UseSampleHz), zptBoolean);

  {$ifndef minimal}
  List.AddProperty({$IFNDEF MINIMAL}'PatternString',{$ENDIF}(@Self.PatternString), zptString);
    List.SetDesignerProperty;
    List.GetLast.DefaultValue.StringValue := 'jhgfgddfgdjdgddhjhgfgddfgdjdfssh';
  {$endif}
end;

{ TPlaySound }

procedure TPlaySound.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Sound',{$ENDIF}(@Sound), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSound]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'NoteNr',{$ENDIF}(@NoteNr), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'Channel',{$ENDIF}(@Channel), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}'ReplayDelay',{$ENDIF}(@ReplayDelay), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}'ByReference',{$ENDIF}(@ByReference), zptBoolean);
end;

procedure TPlaySound.Execute;
begin
  {$ifndef minimal}if Sound=nil then Exit;{$endif}

  if ZApp.NoSound then
    Exit;

  if (ReplayDelay>0) then
  begin
    if (LastPlayed>0) and (ZApp.Time<Self.LastPlayed+ReplayDelay) then
      Exit;
    Self.LastPlayed := ZApp.Time;
  end;
  AudioPlayer.AddNoteToEmitList(@Sound.Voice, NoteNr, Channel, 0, 1.0, Self.ByReference);
end;

{$ifndef minimal}
function TPlaySound.GetDisplayName: AnsiString;
begin
  Result := inherited GetDisplayName;
  if Assigned(Sound) then
  begin
    Result := Result + '  ' + Sound.Name;
    if ByReference then
      Result := Result + ' (Reference)';
  end;
end;

procedure TPlaySound.DesignerReset;
begin
  LastPlayed := 0;
end;
{$endif}

{ TMusic }

const
  MidiTimeScale : single = 0.01;  //Midi times to seconds

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
          if not ZApp.NoSound then
            AudioPlayer.AddNoteToEmitList(@Sound.Voice, NoteNr, Ch, NoteLength, Velocity, False);
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
  LocalTime := LocalTime + DeltaTime * Self.Tempo;
  while (Self.LocalTime>=Self.NextEventTime) and (Self.IsPlaying) do
  begin
    Self.LocalTime := Self.NextEventTime;
    ProcessNextEvent;
    GetNextEventTime;
  end;
end;

procedure TMusic.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'MusicFile',{$ENDIF}(@MusicFile), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}'Instruments',{$ENDIF}(@Instruments), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSound]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'LoopPlayback',{$ENDIF}(@LoopPlayback), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}'Tempo',{$ENDIF}(@Tempo), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'Volume',{$ENDIF}(@Volume), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}'OnPlayNote',{$ENDIF}(@OnPlayNote), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}'NoteParam',{$ENDIF}(@NoteParam), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'NoteChannelParam',{$ENDIF}(@NoteChannelParam), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'NoteLengthParam',{$ENDIF}(@NoteLengthParam), zptFloat);
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
  begin
    if LoopPlayback then
      Start
    else
      Stop;
  end
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
  List.AddProperty({$IFNDEF MINIMAL}'Kind',{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Start','Stop']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Music',{$ENDIF}(@Music), zptComponentRef);
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

  List.AddProperty({$IFNDEF MINIMAL}'MasterVolume',{$ENDIF}(@AudioPlayer.MasterVolume), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 1.0;

  //Läser/skriver till global data
  //Fungerar så länge det enbart finns en instans
  for I := 0 to MaxChannels-1 do
  begin
    {$ifndef minimal}
    S := 'Ch' + IntToStr(I);
    {$endif}
    Channel := AudioPlayer.GetChannel(I);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}(@Channel.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Volume',{$ENDIF}(@Channel.Volume), zptScalar);
    List.AddProperty({$IFNDEF MINIMAL}S + 'UseDelay',{$ENDIF}(@Channel.UseDelay), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'DelayLength',{$ENDIF}(@Channel.DelayLength), zptScalar);
  end;

  for I := 0 to High(GlobalLfos) do
  begin
    {$ifndef minimal}
    S := 'Lfo' + Chr(Ord('0')+I);
    {$endif}
    Lfo := @GlobalLfos[I];
    List.AddProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}(@Lfo.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'IsBipolar',{$ENDIF}(@Lfo.IsBipolar), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Style',{$ENDIF}(@Lfo.Style), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + 'Speed',{$ENDIF}(@Lfo.Speed), zptFloat);
  end;
end;

{ TSample }

procedure TSample.CleanUp;
begin
  FreeMem(Memory);
  Memory := nil;
end;

procedure TSample.CopyAndDestroy(Source: TContent);
var
  S : TSample;
begin
  CleanUp;
  S := TSample(Source);
  Self.Length := S.Length;
  Self.Memory := S.Memory;
  Self.SampleCount := S.SampleCount;
  S.Memory := nil;
  S.Free;
end;

procedure TSample.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Length',{$ENDIF}(@Length),zptFloat);
end;

destructor TSample.Destroy;
begin
  CleanUp;
  inherited;
end;

function TSample.GetMemory: PSampleUnit;
begin
  if (Memory=nil) or (Producers.IsChanged) or (IsChanged) then
  begin
    ReInit;
  end;
  Result := Memory;
end;

procedure TSample.ReInit;
begin
  CleanUp;
  if Producers.Count>0 then
    RefreshFromProducers
  else
  begin
    Self.SampleCount := Round(Self.Length * AudioRate);
    GetMem(Self.Memory, SampleCount * SizeOf(TSampleUnit) );
  end;
  IsChanged := False;
  Producers.IsChanged := False;
end;

{ TSampleExpression }

procedure TSampleExpression.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'Expression',{$ENDIF}(@Expression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Sample : current sample (-1..1)'#13#10 +
      '//Time : current time'#13#10 +
      '//Example: this.Sample=sin(Time*((PI*2)*777));'#13#10;
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}'Sample',{$ENDIF}(@Sample), zptFloat);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}'Time',{$ENDIF}(@Time), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

procedure TSampleExpression.ProduceOutput(Content: TContent;
  Stack: TZArrayList);
var
  S : TSample;
  TimeStep : single;
  I : integer;
  P : PSampleUnit;
begin
  if Stack.Count>0 then
    S := TSample( Stack.Pop )
  else
  begin
    S := TSample.Create(nil);
    S.Length := TSample(Content).Length;
  end;

  P := S.GetMemory;
  if S.SampleCount>0 then
  begin
    Self.Time := 0;
    TimeStep := S.Length/S.SampleCount;
    for I := 0 to S.SampleCount-1 do
    begin
      Self.Sample := P^;
      ZExpressions.RunCode(Expression.Code);
      P^ := Clamp(Self.Sample,-1,1);
      Inc(P);
      Self.Time := Self.Time + TimeStep;
    end;
  end;

  Stack.Push(S);
end;

{ TSampleImport }

procedure TSampleImport.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}'SampleData',{$ENDIF}(@Self.SampleData), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}'SampleRate',{$ENDIF}(@Self.SampleRate), zptFloat);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}'SampleFormat',{$ENDIF}(@Self.SampleFormat), zptByte);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.SetOptions(['8 bit signed','16 bit signed']);{$endif}
end;

procedure TSampleImport.ProduceOutput(Content: TContent; Stack: TZArrayList);
var
  S : TSample;
  SourceStep,SourcePosFloat{,SampleFraction} : single;
  SourceCount,SourcePos,OutputPos : integer;
  P : PSampleUnit;

  function GetSample(const I : integer) : single;
  begin
    case Self.SampleFormat of
      sfoSigned8bit :
        Result := ShortInt(PBytes(Self.SampleData.Data)^[ I ]) / High(ShortInt);
      else //sfoSigned16bit :
        Result := SmallInt(PWords(Self.SampleData.Data)^[ I ]) / High(SmallInt);
    end;
  end;

begin
  S := TSample.Create(nil);
  S.Length := TSample(Content).Length;

  SourceCount := Self.SampleData.Size shr (ord(Self.SampleFormat));
  if S.Length=0 then
    S.Length := SourceCount / AudioRate;

  P := S.GetMemory;

  SourcePosFloat := 0.0;
//  SampleFraction := 0.0;
  SourcePos := 0;
  OutputPos := 0;

  SourceStep := Self.SampleRate / AudioRate;

  //Upsample to target rate
  //(quality improves when not using fractions strangely enough)
  while (OutputPos<S.SampleCount) and (SourcePos<SourceCount-1) do
  begin
    P^ := GetSample(SourcePos);// * (1.0-SampleFraction) + GetSample(SourcePos+1) * SampleFraction;
    Inc(P);
    Inc(OutputPos);

    SourcePosFloat := SourcePosFloat + SourceStep;
//    SampleFraction := Frac(SourcePosFloat);
    SourcePos := Trunc(SourcePosFloat);
  end;

  Stack.Push(S);
end;

initialization

  ZClasses.Register(TSound,SoundClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=1;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TPlaySound,PlaySoundClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=19;{$endif}
  ZClasses.Register(TAudioMixer,AudioMixerClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Mix audio. Only one in each project.';{$endif}
    {$if (not defined(MINIMAL)) or defined(zzdc_activex)}
    ComponentManager.LastAdded.HasGlobalData := True;
    {$ifend}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=17;{$endif}

  ZClasses.Register(TSample,SampleClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=30;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}
  ZClasses.Register(TSampleExpression,SampleExpressionClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=38;{$endif}
  ZClasses.Register(TSampleImport,SampleImportClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=31;{$endif}

  ZClasses.Register(TMusic,MusicClassId);
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=16;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}

  ZClasses.Register(TMusicControl,MusicControlClassId);
    {$ifndef minimal}ComponentManager.LastAdded.HelpText := 'Start/stop playing of music';{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=42;{$endif}

end.
