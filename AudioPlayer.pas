{Copyright (c) 2008- Ville Krumlinde

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

{
  References:

    Kb of Farbrausch http://www.kebby.org/
    Quake 3 source code
    DirectX Audio reference
}
unit AudioPlayer;

{$include zzdc_globalopt.inc}

interface

const
  //Nr of modulators per sound
  MaxModulations = 4;
  MaxLfos = 2;
  MaxEnvelopes = 2;
  MaxGlobalLfos = 2;

type
  //Type used when mixing/generating sounds
  TSoundMixUnit =
    packed record
      Left,Right : single;
    end;
  PSoundMixUnit = ^TSoundMixUnit;
  TSoundMixUnits = array[0..100000] of TSoundMixUnit;
  PSoundMixUnits = ^TSoundMixUnits;

  //One unit of sound when output to platform
  TSoundOutputUnit = TSoundMixUnit;
  PSoundOutputUnit = ^TSoundOutputUnit;

  TWaveform = (wfSquare,wfSaw,wfNoise,wfSine);

  //All calculations of frequency is based on midi notenumbers
  //This way relative frequency is constant over all octaves
  TOscEntry = record
    Waveform : TWaveform;
    NoteModifier : single;
    Frequency : single; //calculated from notemodifier
    PulseWidth : single;
    CurValue : integer;
    WStep : integer;
    IPulseWidth : integer;
  end;

  PEnvelope = ^TEnvelope;
  TEnvelope = record
    //Properties
    Active : boolean;
    AttackTime,DecayTime,SustainLevel,ReleaseTime : single;
    //State-vars
    State : (esInit,esAttack,esDecay,esSustain,esRelease,esStopped);
    Rate,Value : single;
  end;
  TEnvelopes = array[0..MaxEnvelopes-1] of TEnvelope;

  TLfoStyle = (lsSine,lsRandom,lsZeroOne);
  PLfo = ^TLfo;
  TLfo = record
    Active : boolean;
    IsBipolar : boolean; //true=output range -1 .. 1
    Style : TLfoStyle;
    Speed : single;      //0..1
    //State
    Value : single;
    Counter : single;
  end;
  TLfos = array[0..MaxLfos-1] of TLfo;

  //Modulation is one row in the modulationsmatrix in the designer
  //A instance of a modulation (change over time) of a value in a sound
  TModulationSource = (msEnv1,msEnv2,msLfo1,msLfo2,
   msGlobalLfo1,msGlobalLfo2);
  TModulationDestination = (mdFilterCutoff,mdFilterQ,mdNoteNr,
    mdLfo1Speed,mdLfo2Speed,
    mdMod1Amount,mdMod2Amount,mdMod3Amount,mdMod4Amount,
    mdOsc1NoteMod,mdOsc2NoteMod,mdVolume,mdPan,mdOsc2Vol,mdOsc1PW);
  PModulation = ^TModulation;
  TModulation = record
    Active : boolean;
    Source : TModulationSource;
    Destination : TModulationDestination;
    Amount : single;
    //The original value for what is modulated (example: FilterCutoff 0.5)
    //Copied into a modulation when a new voice is allocated
    OriginalDestinationValue : single;
    DestinationPtr : PSingle;
    Interpolation : single;
  end;
  TModulations = array[0..MaxModulations-1] of TModulation;

  PVoiceEntry = ^TVoiceEntry;
  TVoiceEntry =
  record
    Active : boolean;
    NoteNr : single;  //Controls frequency
    Time : single;

    Volume : single;
    Length : single;
    BaseNoteNr : single;
    Osc1 : TOscEntry;
    Osc2 : TOscEntry;
    //Envelopes + one copy of every envelope with offset time (for interpolation)
    Envelopes : array[0..1] of TEnvelopes;
    UseOsc2 : boolean;
    HardSync : boolean;
    UseFilter : boolean;
    FilterCutoff : single;
    FilterQ : single;
    Modulations : TModulations;
    Lfos : TLfos;
    Pan : single;  //Stero panning 0.5=center
    Osc2Volume : single; //0..1

    //Extra state-vars
    PanL,PanR : single;
    //Statevars f�r filter
    Buf0,Buf1 : TSoundMixUnit;

    //Sampled waveform
    SampleRef : pointer;  //Pointer to TSample-component
    SampleData : pointer;
    SampleRepeatPosition : integer;
    SamplePosition,SampleStep : single;
    SampleCount: integer;  //Nr of samples in sample (size/2 if 16 bit)
    UseSampleHz : boolean;

    Next : PVoiceEntry;
  end;

  PChannel = ^TChannel;
  TChannel = record
    Active : boolean;
    Volume : single;
    Voices : PVoiceEntry;
    UseDelay : boolean;
    DelayLength : single;  //0..1
    DelayBuffer : PSoundMixUnits;
    DelayInPoint,DelayOutPoint : integer;
  end;

const
  AudioRate = 44100;  //44khz

  OutputBits = 32;

  MaxVoices = 32;
  MaxChannels = 16;

  //V�rde som 0..1-range f�r attack/release-time skalas upp med i gui
  EnvTimeScale = 5.0;  //Max attacktime/releasetime  seconds

  //1 for mono, 2 for stereo
  StereoChannels = 2;

  //DMA-buffer sizes
  //Det blir en f�rdr�jning av nya ljud som �r lika med dma-buffer size eftersom denna
  //buffer loopar och man fyller hela tiden p� med data precis f�re playingposition.
  //B�r d�rf�r ej vara l�ngre �n en tiondels sekund f�r ljudeffekter.
  SoundBufferFramesSize = Round(AudioRate/20);
  SoundBufferByteSize = SoundBufferFramesSize * SizeOf(TSoundOutputUnit);

  AudioModulateLength = 1.0 / 100;                      //Time between updates of modulations
  ModulateFrameCount = Round(AudioModulateLength * AudioRate);

var
  VoicesMutex : pointer;
  GlobalLfos : array[0..MaxGlobalLfos-1] of TLfo;
  MasterVolume : single;

type
  TNoteEmitEntry = class
    Sound : PVoiceEntry;
    NoteNr : single;
    Length : single;
    Velocity : single;
    ChannelNr : integer;
    ByReference : boolean;
  public
    constructor Create(const Sound : PVoiceEntry; const NoteNr : single; const ChannelNr : integer;
      const Length : single; const Velocity : single; const ByReference : boolean);
  end;

procedure EmitNote(Note: TNoteEmitEntry);

function GetChannel(I : integer) : PChannel;
procedure RenderToMixBuffer(Buf : PSoundMixUnit; FrameCount : integer);

procedure AddNoteToEmitList(Note: TNoteEmitEntry);
procedure EmitSoundsInEmitList;

{$ifndef minimal}
procedure DesignerResetMixer;
procedure DesignerStopAllAudio;
{$endif}

implementation

uses ZPlatform,ZMath,ZClasses,AudioComponents;


const
  DelayBufferSampleCount = 2 * AudioRate;  //2 seconds delay

  //Antal bits f�r hur m�nga steg channel volume g�r i
  ChannelVolBits = 6;

  VoiceFullRange = 2.0;

var
  Voices : array[0..MaxVoices-1] of TVoiceEntry;

  Channels : array[0..MaxChannels-1] of TChannel;
  //Buffer where the voices for each channel are rendered before mix with mixbuffer
  ChannelBuffer : array[0..ModulateFrameCount-1] of TSoundMixUnit;

function GetChannel(I : integer) : PChannel;
begin
  Result := @Channels[I];
end;

procedure UpdateEnvelope(E : PEnvelope; V : PVoiceEntry; const TimeStep: single);
begin
  while True do
  begin
    case E.State of
      esInit :
        begin
          if V.Length<=E.ReleaseTime then
          begin //G� direkt till release ifall length �r kort
            E.State := esRelease;
            E.Value := 1.0;
            E.Rate := 1.0/V.Length;
          end
          else
          begin
            E.State := esAttack;
            if E.AttackTime>0 then
            begin
              E.Rate := 1.0/E.AttackTime;
              E.Value := 0;
            end
            else
              E.Value := 1.0;
          end;
          Continue;  //Go to top of loop
        end;
      esAttack :
        begin
          E.Value := E.Value + E.Rate * TimeStep;
          if E.Value>=1 then
          begin
            E.Value := 1.0;
            E.State := esDecay;
            if E.DecayTime>0 then
              E.Rate := (E.SustainLevel - E.Value) / E.DecayTime
            else
              E.Value := E.SustainLevel;
            Continue; //top of loop
          end;
        end;
      esDecay :
        begin
          if E.Value<=E.SustainLevel then
          begin
            E.State := esSustain;
            E.Value := E.SustainLevel;
            Continue; //Top of loop
          end else
            E.Value := E.Value + E.Rate * TimeStep;
        end;
      esSustain :
        begin
          if V.Time>=V.Length-E.AttackTime-E.DecayTime-E.ReleaseTime then
          begin
            E.State := esRelease;
            if E.ReleaseTime>0 then
              //E.Rate := 1.0/(E.ReleaseTime
              //Anv�nd all �terst�ende tid f�r att tvinga envelop n� noll
              E.Rate := 1.0/(V.Length - V.Time)
            else
              E.Value := 0;
            Continue; //Top of loop
          end;
        end;
      esRelease :
        begin
          if E.Value<=0 then
          begin
            E.Value := 0;
            E.State := esStopped;
          end else
            E.Value := E.Value - E.Rate * TimeStep;
        end;
    end;
    Break;
  end;
end;

procedure UpdateLfo(Lfo : PLfo; const TimeStep : single);
const
  LfoMaxHz = 20.0; //Speed 1 = 20 waves per second
begin
  case Lfo.Style of
    lsSine :
      begin
        Lfo.Counter := Lfo.Counter + TimeStep;
        Lfo.Value := Sin( Lfo.Counter * Lfo.Speed*LfoMaxHz*2*PI );
        if not Lfo.IsBipolar then
          Lfo.Value := 0.5 + Lfo.Value * 0.5;
      end;
    lsRandom :
      begin
        Lfo.Counter := Lfo.Counter + Lfo.Speed*(LfoMaxHz*2) * TimeStep;
        if Lfo.Counter>1 then
        begin //New value each time counter>1
          Lfo.Counter := Frac(Lfo.Counter);
          Lfo.Value := System.Random;
          if Lfo.IsBipolar then
            Lfo.Value := 1 - Lfo.Value * 2;
        end;
      end;
    lsZeroOne :
      begin
        Lfo.Counter := Lfo.Counter + TimeStep;
        if Frac(Lfo.Counter * Lfo.Speed*LfoMaxHz*2)>0.5 then
          Lfo.Value := 1
        else
          Lfo.Value := 0;
        if Lfo.IsBipolar then
          Lfo.Value := 1 - Lfo.Value * 2;
      end;
  end;
end;

procedure UpdateModulators(Voice : PVoiceEntry; const TimeStep : single);
var
  I,J : integer;
  Modulation : PModulation;
  ModValue,Value : single;
  Lfo : PLfo;
  Envelope : PEnvelope;
begin
  for J := 0 to 1 do
    for I := 0 to High(Voice.Envelopes[0]) do
    begin
      Envelope := @Voice.Envelopes[J,I];
      if Envelope.Active then
        UpdateEnvelope(Envelope,Voice,TimeStep);
    end;

  for I := 0 to High(Voice.Lfos) do
  begin
    Lfo := @Voice.Lfos[I];
    if Lfo.Active then
      UpdateLfo(Lfo,TimeStep);
  end;

  for I := 0 to MaxModulations-1 do
  begin
    Modulation := @Voice.Modulations[I];
    if not Modulation.Active then
      Continue;
    for J := 0 to 1 do
    begin
      case Modulation.Source of
        msEnv1..msEnv2 : ModValue := Voice.Envelopes[J, Ord(Modulation.Source)-Ord(msEnv1) ].Value;
        msLfo1 : ModValue := Voice.Lfos[0].Value;
        msLfo2 : ModValue := Voice.Lfos[1].Value;
        msGlobalLfo1 : ModValue := GlobalLfos[0].Value;
        msGlobalLfo2 : ModValue := GlobalLfos[1].Value;
      else //todo: ifdef debug
        ModValue := 0;
      end;

      ModValue := ModValue * Modulation.Amount;

      //ModValue �r nu 0..1, eller -1 .. 1 beroende p� polarity

      case Modulation.Destination of
        mdFilterCutoff :
          Value := Clamp(Modulation.OriginalDestinationValue + ModValue,0.0,0.99);
        mdFilterQ :
          Value := Clamp(Modulation.OriginalDestinationValue + ModValue,0.0,0.99);
        mdNoteNr :
          //Let modvalue modulation two octaves (12*2 notes)
          Value := Modulation.OriginalDestinationValue + (ModValue * 24);
        mdLfo1Speed..mdLfo2Speed :
          Value := Clamp(Modulation.OriginalDestinationValue + ModValue,0.0,1.0);
        mdMod1Amount..mdMod4Amount :
          Value := Clamp(Modulation.OriginalDestinationValue + ModValue,0.0,1.0);
        mdOsc1NoteMod :
          //Osc1 detune. One note range.
          Value := Modulation.OriginalDestinationValue + ModValue;
        mdOsc2NoteMod :
          //Osc2 detune. One note range.
          Value := Modulation.OriginalDestinationValue + ModValue;
        mdVolume :
          Value := Clamp(Modulation.OriginalDestinationValue * ModValue,0.0,1.0);
        mdPan :
          Value := Clamp(Modulation.OriginalDestinationValue + ModValue,0.0,1.0);
        mdOsc2Vol :
          Value := Clamp(Modulation.OriginalDestinationValue * ModValue,0.0,1.0);
        mdOsc1PW :
          Value := Clamp(Modulation.OriginalDestinationValue + ModValue,-1.0,1.0);
      else
        Value := 0;
      end;
      if J=0 then
        Modulation.DestinationPtr^ := Value
      else
        Modulation.Interpolation := (Value-Modulation.DestinationPtr^) / ModulateFrameCount;
    end;

  end;
end;

procedure SetVoiceModulateConstants(V : PVoiceEntry);
//Uppdatera v�rden i Voice som g�ller tills n�sta g�ng update anropas
var
  NoteNr : single;
begin
  if StereoChannels=2 then
  begin
    //EQP panning. kebby.org.
    V.PanL := Sqrt(1.0 - V.Pan);
    V.PanR := Sqrt(V.Pan);
  end;

  NoteNr := (V.NoteNr-69.0) + V.BaseNoteNr;

  if V.UseSampleHz then
    //Set playback speed to 44100hz so sample will use original pitch
    V.Osc1.Frequency := AudioRate
  else
    //double MIDItoFreq( char keynum ) { return 440.0 * pow( 2.0, ((double)keynum - 69.0) / 12.0 ); }
    V.Osc1.Frequency := 440.0 * Power(2, ((NoteNr + V.Osc1.NoteModifier))/12);

  if V.SampleData<>nil then
  begin
    //440 / (22050 * (22050/8363))
//    V.SampleStep := Round(V.Osc1.Frequency / (AudioRate * (AudioRate/8363)) * (1 shl SamplePosPBits));
    //11025 /  (11025 * (AudioRate/11025) ))
    V.SampleStep := V.Osc1.Frequency / AudioRate;
  end;

  //M�ste ta freq*2 pga MixFullRange ej kan representeras som en integer-konstant
  V.Osc1.WStep := Round( (V.Osc1.Frequency / AudioRate) * 2 * High(integer) );
  if V.Osc1.Waveform=wfSquare then
    V.Osc1.IPulseWidth := Round(V.Osc1.PulseWidth * High(integer));

  if V.UseOsc2 then
  begin
    V.Osc2.Frequency := 440.0 * Power(2, ((NoteNr + V.Osc2.NoteModifier))/12);
    V.Osc2.WStep := Round( (V.Osc2.Frequency / AudioRate) * 2 * High(integer) );
  end;
end;

//Read a sample value. Only called from RenderVoice.
function GetSample(V : PVoiceEntry; SamplePos : integer) : TSampleUnit; inline;
begin
  Result := PSampleUnits(V.SampleData)^[SamplePos];
end;

procedure RenderVoice(V : PVoiceEntry; Count : integer);
const
  VoiceLowestValue = -1.0;
  VoiceHighestValue = 1.0;
var
  W1,W2,LastW1 : integer;
  Value1,Value2 : TSoundMixUnit;
  Buf,DestBuf : PSoundMixUnit;
  I,J : integer;
  HasSample : boolean;
  Sample1,Sample2 : TSampleUnit;
  SamplePos : integer;
  SampleFraction : single;
  Modulation : PModulation;
  FilterFb : single;
  //Buffer where voice is rendered before mix with channelbuffer
  //Voicebuffer is always mono
  VoiceBuffer : array[0..ModulateFrameCount-1] of TSoundMixUnit;
begin
  //Write to voice buffer

  //Voice ber�knas med VoicePBits precision
  //MixPBits �r f�r h�gt och ger integer overflow

  //M�ste render �ven om noll volym, annars blir det klick vid avslut
  //pga att filtret inte f�r jobba.

  {if V.IVol=0 then
  begin
    FillChar(VoiceBuffer,SizeOf(VoiceBuffer),0);
    Exit;  //No point rendering nothing
  end;      }

  Value1.Left := 0;  //Get rid of warning
  Value1.Right := 0;  //Get rid of warning

  HasSample := V.SampleData<>nil;

  Buf := @VoiceBuffer[0];
  W1 := V.Osc1.CurValue;
  W2 := V.Osc2.CurValue;
  LastW1 := W1;
  for I := 0 to Count-1 do
  begin

    if not HasSample then
    begin
      //Osc 1
      case V.Osc1.Waveform of
        wfSquare :
          if W1 >= V.Osc1.IPulseWidth then
            Value1.Left := VoiceHighestValue
          else
            Value1.Left := VoiceLowestValue;
        wfSaw :
          Value1.Left := VoiceLowestValue + Round(W1 div 65536)/65536;
        wfNoise :
          if W1>=0 then
          begin
            //Value1 := IntRandom div (1 shl (MixToOutputBits));
            Value1.Left := VoiceLowestValue + System.Random*VoiceFullRange;
            Dec(W1, High(integer) div 2 );
          end;
        wfSine :
          begin
            Value1.Left := Sin(W1 * (1.0/High(integer)* PI*2) );
          end;
      end;

      if V.HardSync then
      begin
        if (LastW1>0) and (W1<0) then
          //HardSync: Restart osc2 when osc1 restarts
          W2 := W1;
        LastW1 := W1;
      end;

      Inc(W1,V.Osc1.WStep);

      //Osc 2
      if V.UseOsc2 then
      begin
        case V.Osc2.Waveform of
          wfSquare :
            if W2<0 then
              Value2.Left := VoiceLowestValue
            else
              Value2.Left := VoiceHighestValue;
          wfSaw :
            Value2.Left := VoiceLowestValue + Round(W2 div 65536)/65536;
        else
          Value2.Left := 0;
        end;
        Inc(W2,V.Osc2.WStep);

        Value2.Left := Value2.Left * V.Osc2Volume;

        Value1.Left := Value1.Left + Value2.Left; //Mix osc1 + osc2
      end;

      Value1.Right := Value1.Left;
    end
    else
    begin
      //Sampled waveform
      SamplePos := Round(V.SamplePosition);
      if (SamplePos>=V.SampleCount) then
      begin
        //Sample pos is beyond end (repeat=-1)
        Value1.Left := 0;
        Value1.Right := 0;
      end
      else
      begin
        SampleFraction := Frac(V.SamplePosition);
        //Value=(Sample1 * (1-Fraction)) + (Sample2 * Fraction)

        Sample1 := GetSample(V,SamplePos);
        Sample1.Left := Sample1.Left * (1 - SampleFraction);
        Sample1.Right := Sample1.Right * (1 - SampleFraction);

        if SamplePos<V.SampleCount-1 then
          Inc(SamplePos)
        else
          SamplePos := 0;

        Sample2 := GetSample(V,SamplePos);
        Sample2.Left := (Sample2.Left * SampleFraction);
        Sample2.Right := (Sample2.Right * SampleFraction);

        Value1.Left := (Sample1.Left + Sample2.Left);
        Value1.Right := (Sample1.Right + Sample2.Right);

        V.SamplePosition := V.SamplePosition + V.SampleStep;
        if (Round(V.SamplePosition) >= V.SampleCount) and
          (V.SampleRepeatPosition>=0) then
          V.SamplePosition := V.SampleRepeatPosition;
      end;
    end;

    //Volume
    Value1.Left := Value1.Left * V.Volume;
    Value1.Right := Value1.Right * V.Volume;

    for J := 0 to MaxModulations-1 do
    begin
      Modulation := @V.Modulations[J];
      if Modulation.Active then
        Modulation.DestinationPtr^ := Modulation.DestinationPtr^ + Modulation.Interpolation;
    end;

    Buf^ := Value1;
    Inc(Buf);
  end;
  V.Osc1.CurValue := W1;
  V.Osc2.CurValue := W2;

  //Filter, obs m�ste ske *efter* volym
  //Blir brus ifall volym sker efter filter
  if V.UseFilter then
  begin  //Based on http://www.musicdsp.org/showone.php?id=29
    Buf := @VoiceBuffer[0];
    V.FilterCutoff := Clamp(V.FilterCutoff, 0.0, 0.99);
    //set feedback amount given f and q between 0 and 1
    FilterFb := (V.FilterQ + V.FilterQ/(1.0 - V.FilterCutoff));
    for I := 0 to Count-1 do
    begin
      V.Buf0.Left := V.Buf0.Left + V.FilterCutoff * (Buf^.Left - V.Buf0.Left + FilterFb * (V.Buf0.Left - V.Buf1.Left));
      V.Buf0.Right := V.Buf0.Right + V.FilterCutoff * (Buf^.Right - V.Buf0.Right + FilterFb * (V.Buf0.Right - V.Buf1.Right));

      V.Buf1.Left := V.Buf1.Left + V.FilterCutoff * (V.Buf0.Left - V.Buf1.Left);
      V.Buf1.Right := V.Buf1.Right + V.FilterCutoff * (V.Buf0.Right - V.Buf1.Right);

      Buf^ := V.Buf1;

      Inc(Buf);
    end;
  end;

  //Skala upp till mixbits och addera till channelbuffer
  Buf := @VoiceBuffer[0];
  DestBuf := @ChannelBuffer[0];
  for I := 0 to Count-1 do
  begin
    Value1 := Buf^;

    DestBuf^.Left := DestBuf^.Left + Value1.Left * V.PanL;
    DestBuf^.Right := DestBuf^.Right + Value1.Right * V.PanR;
    Inc(DestBuf);

    Inc(Buf);
  end;
end;


procedure AddToMixBuffer(Source : PSoundMixUnit; Dest : PSoundMixUnit; Count : integer);
var
  I : integer;
begin
  for I := 0 to Count-1 do
  begin
    Dest^.Left := Dest^.Left + Source^.Left;
    Dest^.Right := Dest^.Right + Source^.Right;
    Inc(Source);
    Inc(Dest);
  end;
end;

procedure ReleaseVoice(V : PVoiceEntry);
var
  I : integer;
  M : PModulation;
begin
  V.Active := False;
  for I := 0 to High(V.Modulations) do
  begin //Restore initial values (important when using ByReference)
    M := @V.Modulations[I];
    if M.Active then
      M.DestinationPtr^ := M.OriginalDestinationValue;
  end;
  for I := 0 to High(V.Lfos) do
    with V.Lfos[I] do
    begin
      Value := 0;
      Counter := 0;
    end;
end;

//Tick all voices and channels with Modulate-length
procedure UpdateModulate;
var
  I,J : integer;
  PrevVoice,Voice : PVoiceEntry;
  Channel : PChannel;
  Lfo : PLfo;
  DTime : single;
begin
  Channel := @Channels[0];
  for I := 0 to MaxChannels-1 do
  begin
    if Channel.Active and (Channel.Voices<>nil) then
    begin
      //Update channel data
      if Channel.UseDelay then
      begin
        Channel.DelayOutPoint := Channel.DelayInPoint -
          Trunc(Channel.DelayLength*DelayBufferSampleCount);
        while Channel.DelayOutPoint<0 do
          Inc(Channel.DelayOutPoint,DelayBufferSampleCount);
      end;

      //Update global lfos
      for J := 0 to High(GlobalLfos) do
      begin
        Lfo := @GlobalLfos[J];
        if Lfo.Active then
          UpdateLfo(Lfo,AudioModulateLength);
      end;

      //Update channel voices
      PrevVoice := nil;
      Voice := Channel.Voices;
      while Voice<>nil do
      begin

        if Voice.Time>=Voice.Length then //Voice.Env1.State=esStopped then
        begin //Release voice
          ReleaseVoice(Voice);
          //todo cleanup klantlig linked-list kodning
          if PrevVoice<>nil then
          begin
            PrevVoice.Next := Voice.Next;
            Voice.Next := nil;
            Voice := PrevVoice.Next;
          end
          else
          begin //f�rst i listan
            Channel.Voices := Voice.Next;
            Voice.Next := nil;
            Voice := Channel.Voices;
          end;
          Continue;
        end;

        //Don't allow voice.time>voice.length
        DTime := AudioModulateLength;
        if Voice.Time + DTime>Voice.Length then
          DTime:=Voice.Length - Voice.Time;
        Voice.Time := Voice.Time + DTime;

        UpdateModulators(Voice,DTime);
        SetVoiceModulateConstants(Voice);

        PrevVoice := Voice;
        Voice := Voice.Next;
      end;
    end;
    Inc(Channel);
  end;
end;


procedure ChannelApplyDelay(Channel : PChannel; Count : integer);
var
  Buf : PSoundMixUnit;
  I : integer;
  Value : TSoundMixUnit;
begin
  Buf := @ChannelBuffer;
  for I := 0 to Count-1 do
  begin
    {$ifndef minimal}
    Assert(Channel.DelayOutPoint<DelayBufferSampleCount);
    Assert(Channel.DelayInPoint<DelayBufferSampleCount);
    {$endif}

    //L�s fr�n delay
    Value := Channel.DelayBuffer[ Channel.DelayOutPoint ];

    //Mixa med input
    Value.Left := (Value.Left * 0.25) + Buf^.Left;
    Value.Right := (Value.Left * 0.25) + Buf^.Right;

    //Skriv det mixade v�rdet till delay-buffer f�r feedback
    Channel.DelayBuffer[ Channel.DelayInPoint ] := Value;

    Buf^ := Value;
    Inc(Buf);

    Inc(Channel.DelayOutPoint);
    if Channel.DelayOutPoint>=DelayBufferSampleCount then
      Channel.DelayOutPoint := 0;

    Inc(Channel.DelayInPoint);
    if Channel.DelayInPoint>=DelayBufferSampleCount then
      Channel.DelayInPoint := 0;
  end;
end;

procedure RenderChannel(Channel : PChannel; Count : integer);
var
  Voice : PVoiceEntry;
  Buf : PSoundMixUnit;
  I : integer;
begin
  FillChar(ChannelBuffer,Count * SizeOf(TSoundMixUnit),0);
  if Channel.Volume<0.00001 then
    Exit;

  Voice := Channel.Voices;
  while Voice<>nil do
  begin
    //Render voice and add to channel mix
    RenderVoice(Voice,Count);
    Voice := Voice.Next;
  end;

  //Delay
  if Channel.UseDelay then
    ChannelApplyDelay(Channel,Count);

  //Volume
  Buf := @ChannelBuffer;
  for I := 0 to Count-1 do
  begin
    Buf^.Left := Buf^.Left * Channel.Volume;
    Buf^.Right := Buf^.Right * Channel.Volume;
    Inc(Buf);
  end;
end;

var
  //Minne var n�gonstans i modulate man befinner sig mellan anrop till RenderToMixBuffer
  RenderCounter : integer;

//Main render routine, called from thread
procedure RenderToMixBuffer(Buf : PSoundMixUnit; FrameCount : integer);
var
  I : integer;
  ModulateCrossOvers,ModulateLeft,ModulateCount : integer;
  Finished : boolean;
  Channel : PChannel;
  VBuf : PSoundMixUnit;
begin
  ModulateCrossOvers := (RenderCounter+FrameCount) div ModulateFrameCount;
  if ModulateCrossOvers>0 then
    ModulateCount := ModulateFrameCount - RenderCounter
  else
    ModulateCount := FrameCount;

  ModulateLeft := ModulateCrossOvers;
  repeat

    Channel := @Channels[0];
    for I := 0 to MaxChannels-1 do
    begin
      if Channel.Active and ((Channel.Voices<>nil) or Channel.UseDelay) then
      begin
        RenderChannel(Channel,ModulateCount);
        //Add channel to main mix
        AddToMixBuffer(@ChannelBuffer,Buf,ModulateCount);
      end;
      Inc(Channel);
    end;


    if MasterVolume<1.0 then
    begin
      //Volume
      VBuf := Buf;
      for I := 0 to ModulateCount-1 do
      begin
        VBuf^.Left := VBuf^.Left * MasterVolume;
        VBuf^.Right := VBuf^.Right * MasterVolume;
        Inc(VBuf);
      end;
    end;

    if ModulateLeft>0 then
    begin
      //New, update modulations
      UpdateModulate;
      Dec(ModulateLeft);
      Inc(Buf,ModulateCount);
      Dec(FrameCount,ModulateCount);
      if FrameCount>ModulateFrameCount then
        ModulateCount := ModulateFrameCount
      else
        ModulateCount := FrameCount;
      Finished := ModulateCount<=0;

      //And update midi music
      if AudioComponents.CurrentMusic<>nil then
        AudioComponents.CurrentMusic.AdvanceMusic(ModulateFrameCount / AudioRate);
    end
    else
      Finished := True;

  until Finished;

  if ModulateCrossOvers>0 then
    RenderCounter := ModulateCount
  else
    Inc(RenderCounter,FrameCount);
end;


function GetFreeVoice : PVoiceEntry;
var
  I : integer;
begin
  Result := @Voices[0];
  for I := 0 to MaxVoices-1 do
  begin
    if not Result.Active then
      Exit;
    Inc(Result);
  end;
  Result := nil;
end;

procedure AddVoiceToChannel(Voice : PVoiceEntry; Channel : PChannel);
begin
  //Linked list prepend
  Voice.Next := Channel.Voices;
  Channel.Voices := Voice;
end;

var
  //List with notes to emit in next call to emitsounds
  EmitList : TZArrayList;

procedure AddNoteToEmitList(Note: TNoteEmitEntry);
begin
  EmitList.Add(Note);
end;

procedure EmitNote(Note: TNoteEmitEntry);
var
  P: PSingle;
  V: PVoiceEntry;
  Channel: PChannel;
  I,J: Integer;
  M: PModulation;
begin
  Channel := @Channels[Note.ChannelNr];
  if not Channel.Active then
    Exit;
  if Note.ByReference then
  begin
    V := Note.Sound;
    if V.Active or (V.Next<>nil) then
      Exit;
  end
  else
  begin
    V := GetFreeVoice;
    if V=nil then
      Exit;
    V^ := Note.Sound^;  //Memcopy voice data
  end;
  V.Time := 0;

  V.Active := True;
  V.NoteNr := Note.NoteNr;
  //V.Volume := 0.25;
  if Note.Length <> 0 then
    //Override sound length-value
    V.Length := Note.Length;

  //Modulate volume with velocity (0..1)
  V.Volume := V.Volume * Note.Velocity;

  V.Envelopes[1] := V.Envelopes[0]; //memcopy
  for J := 0 to 1 do
    for I := 0 to High(V.Envelopes) do
    begin
      V.Envelopes[J,I].State := esInit;
      if J=1 then //offset for interpolation
        UpdateEnvelope(@V.Envelopes[J,I],V,AudioModulateLength);
    end;

  //Determine the nr of samples in sampledata (size in bytes / sampleformat)
  if V.SampleRef <> nil then
  begin
    V.SampleData := TSample(V.SampleRef).GetMemory;
    V.SampleCount := TSample(V.SampleRef).SampleCount;
    V.SamplePosition := 0;
  end;
  //Initialize modulations
  for I := 0 to High(V.Modulations) do
  begin
    M := @V.Modulations[I];
    if M.Active then
    begin
      case M.Destination of
        mdFilterCutoff:  P := @V.FilterCutoff;
        mdFilterQ: P := @V.FilterQ;
        mdNoteNr:  P := @V.NoteNr;
        mdLfo1Speed..mdLfo2Speed: P := @V.Lfos[Ord(M.Destination) - Ord(mdLfo1Speed)].Speed;
        mdMod1Amount..mdMod4Amount:  P := @V.Modulations[Ord(M.Destination) - Ord(mdMod1Amount)].Amount;
        mdOsc1NoteMod: P := @V.Osc1.NoteModifier;
        mdOsc2NoteMod: P := @V.Osc2.NoteModifier;
        mdVolume: P := @V.Volume;
        mdPan: P := @V.Pan;
        mdOsc2Vol: P := @V.Osc2Volume;
        mdOsc1PW: P := @V.Osc1.PulseWidth;
      else //todo ifdef debug
        P := nil;
      end;
      M.DestinationPtr := P;
      M.OriginalDestinationValue := P^;
    end;
  end;
  UpdateModulators(V, 0);
  SetVoiceModulateConstants(V);
  AddVoiceToChannel(V, Channel);
end;

//Emit all sounds queued up in emitlist
//This minimizes synchronization problems with playerthread
procedure EmitSoundsInEmitList;
var
  I : integer;
  Note : TNoteEmitEntry;
begin
  if EmitList.Count=0 then
    Exit;

  Platform_EnterMutex(VoicesMutex);
    for I := 0 to EmitList.Count-1 do
    begin
      Note := TNoteEmitEntry(EmitList[I]);
      EmitNote(Note);
    end;
    EmitList.Clear;
  Platform_LeaveMutex(VoicesMutex);
end;

procedure InitChannels;
const
  DelayBufferByteSize = DelayBufferSampleCount * SizeOf(TSoundMixUnit);
var
  I : integer;
  Channel : PChannel;
begin
  Channel := @Channels;
  for I := 0 to MaxChannels-1 do
  begin
    if I<2 then
    begin
      Channel.Volume := 0.5;
      Channel.DelayLength := 0.1;
      Channel.Active := True;
    end;
    GetMem(Channel.DelayBuffer,DelayBufferByteSize);
    FillChar(Channel.DelayBuffer^,DelayBufferByteSize,0);
    Inc(Channel);
  end;
end;

procedure FreeChannels;
var
  I : integer;
  Channel : PChannel;
begin
  Channel := @Channels;
  for I := 0 to MaxChannels-1 do
  begin
    FreeMem(Channel.DelayBuffer);
    Inc(Channel);
  end;
end;

{$ifndef minimal}
procedure DesignerResetMixer;
begin
  Platform_EnterMutex(VoicesMutex);
    FreeChannels;
    FillChar(Channels,SizeOf(Channels),0);
    FillChar(Voices,SizeOf(Voices),0);
    FillChar(GlobalLfos,SizeOf(GlobalLfos),0);
    InitChannels;
    MasterVolume := 1.0;
  Platform_LeaveMutex(VoicesMutex);
end;

procedure DesignerStopAllAudio;
var
  I : integer;
  Channel : PChannel;
  Voice,Tmp : PVoiceEntry;
begin
  Platform_EnterMutex(VoicesMutex);
    for I := 0 to MaxChannels-1 do
    begin
      Channel := GetChannel(I);
      Voice := Channel.Voices;
      while Voice<>nil do
      begin //Need to walk through voices because some may have been created ByReference
        Voice.Active := False;
        Tmp := Voice.Next;
        Voice.Next := nil;
        Voice := Tmp;
      end;
      Channel.Voices := nil;
    end;
    FillChar(Voices,SizeOf(Voices),0);
    AudioComponents.CurrentMusic := nil;
  Platform_LeaveMutex(VoicesMutex);
end;
{$endif}

{ TNoteEmitEntry }

constructor TNoteEmitEntry.Create(const Sound: PVoiceEntry;
  const NoteNr: single; const ChannelNr: integer; const Length,
  Velocity: single; const ByReference: boolean);
begin
  Self.Sound := Sound;
  Self.NoteNr := NoteNr;
  Self.ChannelNr := ChannelNr;
  Self.Length := Length;
  Self.Velocity := Velocity;
  Self.ByReference := ByReference;
end;

initialization

  EmitList := TZArrayList.Create;
  VoicesMutex := Platform_CreateMutex;

  InitChannels;
  MasterVolume := 1.0;

finalization

  EmitList.Free;
  Platform_FreeMutex(VoicesMutex);

  FreeChannels;

end.
