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

{$include zzdc_globalopt.inc}

interface

uses ZClasses,AudioPlayer;

resourcestring
  strLength = 'Length';
  strVolume = 'Volume';
  strBaseNoteNr = 'BaseNoteNr';
  strOsc1Waveform = 'Osc1Waveform';
  strOsc1NoteModifier = 'Osc1NoteModifier';
  strOsc1PW = 'Osc1PW';
  strUseOsc2 = 'UseOsc2';
  strOsc2Waveform = 'Osc2Waveform';
  strOsc2NoteModifier = 'Osc2NoteModifier';
  strOsc2Volume = 'Osc2Volume';
  strHardSync = 'HardSync';
  strUseFilter = 'UseFilter';
  strFilterCutoff = 'FilterCutoff';
  strFilterQ = 'FilterQ';
  strPan = 'Pan';
  strActive = 'Active';
  strSource = 'Source';
  strDestination = 'Destination';
  strAmount = 'Amount';
  strAttackTime = 'AttackTime';
  strDecayTime = 'DecayTime';
  strSustainLevel = 'SustainLevel';
  strReleaseTime = 'ReleaseTime';
  strIsBipolar = 'IsBipolar';
  strStyle = 'Style';
  strSpeed = 'Speed';
  strSample = 'Sample';
  strSampleRepeatPosition = 'SampleRepeatPosition';
  strUseSampleHz = 'UseSampleHz';
  strPatternString = 'PatternString';
  strSound = 'Sound';
  strNoteNr = 'NoteNr';
  strChannel = 'Channel';
  strReplayDelay = 'ReplayDelay';
  strByReference = 'ByReference';
  strMusicFile = 'MusicFile';
  strInstruments = 'Instruments';
  strLoopPlayback = 'LoopPlayback';
  strTempo = 'Tempo';
  strOnPlayNote = 'OnPlayNote';
  strNoteParam = 'NoteParam';
  strNoteChannelParam = 'NoteChannelParam';
  strNoteLengthParam = 'NoteLengthParam';
  strKind = 'Kind';
  strMusic = 'Music';
  strExpression = 'Expression';
  strTime = 'Time';
  strSampleData = 'SampleData';
  strSampleRate = 'SampleRate';
  strSampleFormat = 'SampleFormat';
  strSampleFileFormat = 'SampleFileFormat';

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
  strict private
    IsPlaying : boolean;
    Stream : TZInputStream;
    NextEventTime : single;
    LocalTime : single;
    CurrentInstruments : array[0..15] of integer;
    ChannelVolumes : array[0..15] of single;
    PlayedNotes : TZArrayList;
    procedure GetNextEventTime;
    function ReadVarLength: Integer;
    procedure ProcessNextEvent;
  public
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
    constructor Create(OwnerList: TZComponentList); override;
  end;

  TMusicControl = class(TCommand)
  protected
    procedure DefineProperties(List: TZPropertyList); override;
  public
    Kind : (mckStart,mckStop);
    Music : TMusic;
    procedure Execute; override;
  end;

  TSampleUnit = TSoundMixUnit;
  PSampleUnit = ^TSampleUnit;
  TSampleUnits = array[0..10000] of TSampleUnit;
  PSampleUnits = ^TSampleUnits;
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
  strict private
    class function LoadOGG(S : TSample; DataP : pointer; DataSize : integer):boolean;
  protected
    procedure DefineProperties(List: TZPropertyList); override;
    procedure ProduceOutput(Content : TContent; Stack : TZArrayList); override;
  public
    SampleData : TZBinaryPropValue;
    SampleRate : single;
    SampleFormat : (sfoSigned8bit,sfoSigned16bit);
    SampleFileFormat : (sffRAW,sffOGG);
  end;

var
  CurrentMusic : TMusic;

implementation

uses ZApplication,ZExpressions,ZMath, ZPlatform, BeRoAudioOGGVorbisTremor
  {$ifndef minimal}
  ,SysUtils
  {$endif}
  ;

{ TSound }

constructor TSound.Create(OwnerList: TZComponentList);
begin
  inherited;
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
  List.AddProperty({$IFNDEF MINIMAL}strLength,{$ENDIF}(@Voice.Length), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}strVolume,{$ENDIF}(@Voice.Volume), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 0.25;
  List.AddProperty({$IFNDEF MINIMAL}strBaseNoteNr,{$ENDIF}(@Voice.BaseNoteNr), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}strOsc1Waveform,{$ENDIF}(@Voice.Osc1.Waveform), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strOsc1NoteModifier,{$ENDIF}(@Voice.Osc1.NoteModifier), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}strOsc1PW,{$ENDIF}(@Voice.Osc1.PulseWidth), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}strUseOsc2,{$ENDIF}(@Voice.UseOsc2), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strOsc2Waveform,{$ENDIF}(@Voice.Osc2.Waveform), zptByte);
  List.AddProperty({$IFNDEF MINIMAL}strOsc2NoteModifier,{$ENDIF}(@Voice.Osc2.NoteModifier), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}strOsc2Volume,{$ENDIF}(@Voice.Osc2Volume), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}strHardSync,{$ENDIF}(@Voice.HardSync), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}strUseFilter,{$ENDIF}(@Voice.UseFilter), zptBoolean);
  List.AddProperty({$IFNDEF MINIMAL}strFilterCutoff,{$ENDIF}(@Voice.FilterCutoff), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}strFilterQ,{$ENDIF}(@Voice.FilterQ), zptFloat);

  List.AddProperty({$IFNDEF MINIMAL}strPan,{$ENDIF}(@Voice.Pan), zptScalar);
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
    List.AddProperty({$IFNDEF MINIMAL}S + strActive,{$ENDIF}(@Modulation.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + strSource,{$ENDIF}(@Modulation.Source), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + strDestination,{$ENDIF}(@Modulation.Destination), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + strAmount,{$ENDIF}(@Modulation.Amount), zptFloat);
  end;

  for I := 0 to High(Voice.Envelopes[0]) do
  begin
    {$ifndef minimal}
    S := 'Env' + Chr(Ord('0')+I);
    {$endif}
    Envelope := @Voice.Envelopes[0,I];
    List.AddProperty({$IFNDEF MINIMAL}S + strActive,{$ENDIF}(@Envelope.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + strAttackTime,{$ENDIF}(@Envelope.AttackTime), zptFloat);
    List.AddProperty({$IFNDEF MINIMAL}S + strDecayTime,{$ENDIF}(@Envelope.DecayTime), zptFloat);
    List.AddProperty({$IFNDEF MINIMAL}S + strSustainLevel,{$ENDIF}(@Envelope.SustainLevel), zptFloat);
      List.GetLast.DefaultValue.FloatValue := 1.0;
    List.AddProperty({$IFNDEF MINIMAL}S + strReleaseTime,{$ENDIF}(@Envelope.ReleaseTime), zptFloat);
  end;

  for I := 0 to High(Voice.Lfos) do
  begin
    {$ifndef minimal}
    S := 'Lfo' + Chr(Ord('0')+I);
    {$endif}
    Lfo := @Voice.Lfos[I];
    List.AddProperty({$IFNDEF MINIMAL}S + strActive,{$ENDIF}(@Lfo.Active), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + strIsBipolar,{$ENDIF}(@Lfo.IsBipolar), zptBoolean);
    List.AddProperty({$IFNDEF MINIMAL}S + strStyle,{$ENDIF}(@Lfo.Style), zptByte);
    List.AddProperty({$IFNDEF MINIMAL}S + strSpeed,{$ENDIF}(@Lfo.Speed), zptFloat);
  end;

  {$ifndef minimal}
  for I := SaveCount to List.Count-1 do
    TZProperty(List[I]).HideInGui := True;
  {$endif}

  List.AddProperty({$IFNDEF MINIMAL}strSample,{$ENDIF}(@Voice.SampleRef), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSample]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strSampleRepeatPosition,{$ENDIF}(@Voice.SampleRepeatPosition), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strUseSampleHz,{$ENDIF}(@Voice.UseSampleHz), zptBoolean);

  {$ifndef minimal}
  List.AddProperty({$IFNDEF MINIMAL}strPatternString,{$ENDIF}(@Self.PatternString), zptString);
    List.SetDesignerProperty;
    List.GetLast.DefaultValue.StringValue := 'jhgfgddfgdjdgddhjhgfgddfgdjdfssh';
  {$endif}
end;

{ TPlaySound }

procedure TPlaySound.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strSound,{$ENDIF}(@Sound), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSound]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strNoteNr,{$ENDIF}(@NoteNr), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}strChannel,{$ENDIF}(@Channel), zptInteger);
  List.AddProperty({$IFNDEF MINIMAL}strReplayDelay,{$ENDIF}(@ReplayDelay), zptFloat);
  List.AddProperty({$IFNDEF MINIMAL}strByReference,{$ENDIF}(@ByReference), zptBoolean);
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
  AudioPlayer.AddNoteToEmitList( TNoteEmitEntry.Create(@Sound.Voice, NoteNr, Channel, 0, 1.0, Self.ByReference) );
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
          NoteLength := NoteLength + Sound.Voice.Envelopes[0,0].ReleaseTime;
          if not ZApp.NoSound then
          begin
            Self.PlayedNotes.Add(TNoteEmitEntry.Create(@Sound.Voice, NoteNr, Ch, NoteLength, Velocity, False));
            AudioPlayer.EmitNote( TNoteEmitEntry(PlayedNotes.Last) );
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

constructor TMusic.Create(OwnerList: TZComponentList);
begin
  inherited;
  PlayedNotes := TZArrayList.Create;
end;

procedure TMusic.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strMusicFile,{$ENDIF}(@MusicFile), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}strInstruments,{$ENDIF}(@Instruments), zptComponentList);
    {$ifndef minimal}List.GetLast.SetChildClasses([TSound]);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strLoopPlayback,{$ENDIF}(@LoopPlayback), zptBoolean);

  List.AddProperty({$IFNDEF MINIMAL}strTempo,{$ENDIF}(@Tempo), zptFloat);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}strVolume,{$ENDIF}(@Volume), zptScalar);
    List.GetLast.DefaultValue.FloatValue := 1.0;
  List.AddProperty({$IFNDEF MINIMAL}strOnPlayNote,{$ENDIF}(@OnPlayNote), zptComponentList);
  List.AddProperty({$IFNDEF MINIMAL}strNoteParam,{$ENDIF}(@NoteParam), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strNoteChannelParam,{$ENDIF}(@NoteChannelParam), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strNoteLengthParam,{$ENDIF}(@NoteLengthParam), zptFloat);
    List.GetLast.NeverPersist := True;
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
end;

destructor TMusic.Destroy;
begin
  Stop;
  PlayedNotes.Free;
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
  Self.LocalTime := 0.0; //Tiden d�r "n�len" �r och spelar musik fr�n skivan
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
var
  I : integer;
  Note : TNoteEmitEntry;
begin
  if PlayedNotes.Count>0 then
  begin
    Platform_EnterMutex(VoicesMutex);
      if Self.OnPlayNote.Count>0 then
      begin
        for I := 0 to Min(PlayedNotes.Count-1,8) do
        begin
          Note := TNoteEmitEntry(PlayedNotes[I]);
          Self.NoteParam := Note.NoteNr;
          Self.NoteChannelParam := Note.ChannelNr;  //int to float
          Self.NoteLengthParam := Note.Length;
          Self.OnPlayNote.ExecuteCommands;
        end;
      end;
      PlayedNotes.Clear;
    Platform_LeaveMutex(VoicesMutex);
  end;
end;

{ TMusicControl }

procedure TMusicControl.DefineProperties(List: TZPropertyList);
begin
  inherited;
  List.AddProperty({$IFNDEF MINIMAL}strKind,{$ENDIF}(@Kind), zptByte);
    {$ifndef minimal}List.GetLast.SetOptions(['Start','Stop']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strMusic,{$ENDIF}(@Music), zptComponentRef);
    {$ifndef minimal}List.GetLast.SetChildClasses([TMusic]);{$endif}
end;

procedure TMusicControl.Execute;
begin
  case Kind of
    mckStart :
      begin
        {$ifndef minimal}if Music=nil then Exit;{$endif}
        Music.Start;
        AudioComponents.CurrentMusic := Music;
      end;
    mckStop : AudioComponents.CurrentMusic := nil;
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

  //All the properties below are in global data section

  List.AddGlobalDataProperty({$IFNDEF MINIMAL}'MasterVolume',{$ENDIF}@AudioPlayer.MasterVolume, zptScalar);
    List.GetLast.DefaultValue.FloatValue := 1.0;

  for I := 0 to MaxChannels-1 do
  begin
    {$ifndef minimal}
    S := 'Ch' + IntToStr(I);
    {$endif}
    Channel := AudioPlayer.GetChannel(I);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}@Channel.Active, zptBoolean);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'Volume',{$ENDIF}@Channel.Volume, zptScalar);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'UseDelay',{$ENDIF}@Channel.UseDelay, zptBoolean);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'DelayLength',{$ENDIF}@Channel.DelayLength, zptScalar);
  end;

  for I := 0 to High(GlobalLfos) do
  begin
    {$ifndef minimal}
    S := 'Lfo' + Chr(Ord('0')+I);
    {$endif}
    Lfo := @GlobalLfos[I];
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'Active',{$ENDIF}@Lfo.Active, zptBoolean);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'IsBipolar',{$ENDIF}@Lfo.IsBipolar, zptBoolean);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'Style',{$ENDIF}@Lfo.Style, zptByte);
    List.AddGlobalDataProperty({$IFNDEF MINIMAL}S + 'Speed',{$ENDIF}@Lfo.Speed, zptFloat);
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
  List.AddProperty({$IFNDEF MINIMAL}strLength,{$ENDIF}(@Length),zptFloat);
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
  List.AddProperty({$IFNDEF MINIMAL}strExpression,{$ENDIF}(@Expression), zptExpression);
    {$ifndef minimal}
    List.GetLast.DefaultValue.ExpressionValue.Source :=
      '//Sample : current sample (-1..1)'#13#10 +
      '//Time : current time'#13#10 +
      '//Example: this.Sample=sin(Time*((PI*2)*777));'#13#10;
    {$endif}
  List.AddProperty({$IFNDEF MINIMAL}strSample,{$ENDIF}(@Sample), zptFloat);
    List.GetLast.NeverPersist := True;
  List.AddProperty({$IFNDEF MINIMAL}strTime,{$ENDIF}(@Time), zptFloat);
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
      Self.Sample := P^.Left;
      ZExpressions.RunCode(Expression.Code);
      P^.Left := Self.Sample;
      P^.Right := Self.Sample;
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
  List.AddProperty({$IFNDEF MINIMAL}strSampleData,{$ENDIF}(@Self.SampleData), zptBinary);
  List.AddProperty({$IFNDEF MINIMAL}strSampleRate,{$ENDIF}(@Self.SampleRate), zptFloat);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strSampleFormat,{$ENDIF}(@Self.SampleFormat), zptByte);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.SetOptions(['8 bit signed','16 bit signed']);{$endif}
  List.AddProperty({$IFNDEF MINIMAL}strSampleFileFormat,{$ENDIF}(@Self.SampleFileFormat), zptByte);
    {$ifndef minimal}List.GetLast.IsReadOnly := True;{$endif}
    {$ifndef minimal}List.GetLast.SetOptions(['RAW','OGG']);{$endif}
end;


function ogg_fread(ptr:pointer;size,nmemb:BeRoAudioOGGPtrUInt;datasource:pointer):BeRoAudioOGGPtrUInt;
//var res:integer;
var
  bytes : integer;
  S : TZInputStream;
begin
 bytes:=nmemb * size;
 S := TZInputStream(datasource);
 if S.Position+Bytes>S.Size then
   Bytes:=S.Size - S.Position;
 S.Read(ptr^, bytes);
 result:=bytes;
end;

function ogg_fseek(datasource:pointer;offset:int64;whence:longint):longint;
begin
 case whence of
  SEEK_SET:begin
    TZInputStream(datasource).Position := offset;
  end;
  SEEK_CUR:begin
   Inc(TZInputStream(datasource).Position, offset);
  end;
  SEEK_END:begin
   TZInputStream(datasource).Position := TZInputStream(datasource).Size + Offset;
  end;
 end;
 result:=TZInputStream(datasource).Position;
end;

function ogg_fclose(datasource:pointer):longint;
begin
 result:=0;
end;

function ogg_ftell(datasource:pointer):longint;
begin
 result:=TZInputStream(datasource).Position;
end;


class function TSampleImport.LoadOGG(S : TSample; DataP : pointer; DataSize : integer):boolean;
const _ov_open_callbacks:ov_callbacks=(read_func:ogg_fread;seek_func:ogg_fseek;close_func:ogg_fclose;tell_func:ogg_ftell);
var vf:POggVorbis_File;
     Data:PSmallints;
     DataEx:PSmallint;
     Channels,SampleRate,TotalSamples,Total,BytesIn,bitstream,Bytes:longint;
     info:Pvorbis_info;
     Stream : TZInputStream;

  SourceStep,SourcePosFloat{,SampleFraction} : single;
  SourcePos,OutputPos : integer;
  P : PSampleUnit;

  function GetSample(const I : integer) : TSampleUnit;
  begin
    case Channels of
     1:
       begin
         Result.Left :=Data^[i]/High(SmallInt);
         Result.Right := Result.Left;
       end;
     2:
       begin
         Result.Left := Data^[(i*2)]/High(SmallInt);
         Result.Right := Data^[(i*2)+1]/High(SmallInt);
       end;
     {$ifndef minimal}
     else
       Assert(False,'Non-supported channel count');
     {$endif}
     end;
  end;

begin
  Stream := TZInputStream.CreateFromMemory(DataP, DataSize);

  Result := False;
  New(vf);

  if ov_open_callbacks(Stream, vf, nil, 0, _ov_open_callbacks) = 0 then
  begin
    info := ov_info(vf, -1);
    Channels := info^.Channels;
    SampleRate := info^.rate;
    TotalSamples := ov_pcm_total(vf, -1);
    GetMem(Data, (TotalSamples + 4096) * Channels * SizeOf(smallint));
    bytes := TotalSamples * Channels * SizeOf(smallint);
    FillChar(Data^, bytes, 0);
    DataEx := @Data[0];
    Total := 0;
    bitstream := 0;
    while Total < bytes do
    begin
      BytesIn := ov_read(vf, DataEx, bytes - Total, @bitstream);
      if BytesIn = OV_HOLE then
      begin
        continue;
      end
      else if (BytesIn = OV_EBADLINK) or (BytesIn = OV_EINVAL) or (BytesIn = 0)
      then
      begin
        break;
      end;
      Inc(PAnsiChar(pointer(DataEx)), BytesIn);
      Inc(Total, BytesIn);
    end;

    if Total > 0 then
    begin
      S.Length := TotalSamples / SampleRate;

      P := S.GetMemory;

      SourcePosFloat := 0.0;
      // SampleFraction := 0.0;
      SourcePos := 0;
      OutputPos := 0;

      SourceStep := SampleRate / AudioRate;

      // Upsample to target rate
      // (quality improves when not using fractions strangely enough)
      while (OutputPos < S.SampleCount) and (SourcePos < TotalSamples - 1) do
      begin
        P^ := GetSample(SourcePos);

//        P^ := GetSample(SourcePos) * (1.0-SampleFraction) + GetSample(SourcePos+1) * SampleFraction;
        Inc(P);
        Inc(OutputPos);
        SourcePosFloat := SourcePosFloat + SourceStep;
        SourcePos := Trunc(SourcePosFloat);
      end;
    end;

    FreeMem(Data);
    ov_clear(vf);
  end;
  Dispose(vf);
  Stream.Free;
end;

procedure TSampleImport.ProduceOutput(Content: TContent; Stack: TZArrayList);
var
  S : TSample;
  SourceStep,SourcePosFloat{,SampleFraction} : single;
  SourceCount,SourcePos,OutputPos : integer;
  P : PSampleUnit;

  function GetSample(const I : integer) : TSampleUnit;
  begin
    case Self.SampleFormat of
      sfoSigned8bit : Result.Left := ShortInt(PBytes(Self.SampleData.Data)^[ I ]) / High(ShortInt)
      else //sfoSigned16bit :
        Result.Left := SmallInt(PWords(Self.SampleData.Data)^[ I ]) / High(SmallInt);
    end;
    Result.Right := Result.Left;
  end;

begin
  S := TSample.Create(nil);
  S.Length := TSample(Content).Length;

  case Self.SampleFileFormat of
    sffRaw :
      begin
        SourceCount := Self.SampleData.Size shr (ord(Self.SampleFormat));
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
      end;
    sffOgg :
      LoadOgg(S,Self.SampleData.Data, Self.SampleData.Size);
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
    {$endif}
    {$ifndef minimal}ComponentManager.LastAdded.ImageIndex:=17;{$endif}
    {$ifndef minimal}ComponentManager.LastAdded.AutoName := True;{$endif}

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
