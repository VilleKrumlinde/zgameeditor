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

{
  References:

    Kb of Farbrausch http://www.kebby.org/
    Quake 3 source code
    DirectX Audio reference
}
unit AudioPlayer;

interface

const
  //Nr of modulators per sound
  MaxModulations = 4;
  MaxLfos = 2;
  MaxEnvelopes = 2;
  MaxGlobalLfos = 2;

type
  //Type used when mixing/generating sounds
  TSoundMixUnit = integer;
  PSoundMixUnit = ^TSoundMixUnit;
  TSoundMixUnits = array[0..100000] of TSoundMixUnit;
  PSoundMixUnits = ^TSoundMixUnits;

  //One unit of sound when output to platform
  TSoundOutputUnit = smallint;
  PSoundOutputUnit = ^TSoundOutputUnit;

  TWaveform = (wfSquare,wfSaw,wfNoise,wfSine);

  //All calculations of frequency is based on midi notenumbers
  //This way relative frequency is constant over all octaves
  TOscEntry = record
    Waveform : TWaveform;
    NoteModifier : single;
    Frequency : single; //calculated from notemodifier
    PulseWidth : single;
    CurValue : TSoundMixUnit;
    WStep : TSoundMixUnit;
    IPulseWidth : TSoundMixUnit;
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
    Envelopes : TEnvelopes;
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
    IVol,IPanL,IPanR,IOsc2Vol : integer;
    //Statevars för integerfilter
    FilterFb,FilterIntCut : integer;
    Buf0,Buf1 : integer;

    //Sampled waveform
    SampleRef : pointer;  //Pointer to TSample-component
    SampleData : pointer;
    SampleRepeatPosition,SampleStep : integer;
    SamplePosition : NativeUInt;
    SampleCount: integer;  //Nr of samples in sample (size/2 if 16 bit)
    UseSampleHz : boolean;

    Next : PVoiceEntry;
  end;

  PChannel = ^TChannel;
  TChannel = record
    Active : boolean;
    Volume : single;
    Voices : PVoiceEntry;
    IVol : integer;
    UseDelay : boolean;
    DelayLength : single;  //0..1
    DelayBuffer : PSoundMixUnits;
    DelayInPoint,DelayOutPoint : integer;
  end;

const
  AudioRate = 44100;  //44khz

  OutputBits = SizeOf(TSoundOutputUnit)*8;

  MixPBits = 24;      //Fixed point precision bits
  //Detta betyder att allt över 1.0 (dvs 1<<24) kommer att clippas
  //Output till 16 bits blir en shr/div med 8 (så gör Q3)
  //Output = (data shr (MixPBits-OutputBits)) clamp (min/max outputrange)

  MixToOutputBits = MixPBits-OutputBits;

  MaxVoices = 32;
  MaxChannels = 16;

  //Värde som 0..1-range för attack/release-time skalas upp med i gui
  EnvTimeScale = 5.0;  //Max attacktime/releasetime  seconds

  //1 for mono, 2 for stereo
  StereoChannels = 2;

  //DMA-buffer sizes
  //Det blir en fördröjning av nya ljud som är lika med dma-buffer size eftersom denna
  //buffer loopar och man fyller hela tiden på med data precis före playingposition.
  //Bör därför ej vara längre än en tiondels sekund för ljudeffekter.
  SoundBufferSamplesSize = Round(AudioRate/20);
  SoundBufferByteSize = SoundBufferSamplesSize * SizeOf(TSoundOutputUnit) * StereoChannels;

  AudioFrameLength = 1.0 / 50;                      //Tid mellan varje uppdatering av modulations
  FrameSampleCount = Round(AudioFrameLength * AudioRate);  //Antal samples i varje frame

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
procedure RenderToMixBuffer(Buf : PSoundMixUnit; Count : integer);

procedure AddNoteToEmitList(Note: TNoteEmitEntry);
procedure EmitSoundsInEmitList;

{$ifndef minimal}
procedure DesignerResetMixer;
procedure DesignerStopAllAudio;
{$endif}

implementation

uses ZPlatform,ZMath,ZClasses,AudioComponents;


const
  FilterPBits = 8;  //Filter fixed point
  DelayBufferSampleCount = 2 * AudioRate;  //2 seconds delay

  //Voice fixed point
  VoicePBits = 16;
  VoiceFullRange = (1 shl VoicePBits);
  VoiceVolBits = 6;

  SamplePosPBits = 8;

  //Antal bits för hur många steg channel volume går i
  ChannelVolBits = 6;

var
  Voices : array[0..MaxVoices-1] of TVoiceEntry;

  Channels : array[0..MaxChannels-1] of TChannel;
  //Buffer where the voices for each channel are rendered before mix with mixbuffer
  ChannelBuffer : array[0..(FrameSampleCount*StereoChannels)-1] of TSoundMixUnit;
  IMasterVolume : integer;

function GetChannel(I : integer) : PChannel;
begin
  Result := @Channels[I];
end;

procedure UpdateEnvelope(E : PEnvelope; V : PVoiceEntry; const TimeStep: single);
begin
  case E.State of
    esInit :
      begin
        if V.Length<=E.ReleaseTime then
        begin //Gå direkt till release ifall length är kort
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
        end;
      end;
    esDecay :
      begin
        if E.Value<=E.SustainLevel then
        begin
          E.State := esSustain;
          E.Value := E.SustainLevel;
        end else
          E.Value := E.Value + E.Rate * TimeStep;
      end;
    esSustain :
      begin
        if V.Time>=V.Length-E.AttackTime-E.DecayTime-E.ReleaseTime then
        begin
          E.State := esRelease;
          if E.ReleaseTime>0 then
//            E.Rate := 1.0/(E.ReleaseTime
            //Använd all återstående tid för att tvinga envelop nå noll
            E.Rate := 1.0/(V.Length - V.Time)
          else
            E.Value := 0;
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
  I : integer;
  Modulation : PModulation;
  ModValue,Value : single;
  Lfo : PLfo;
  Envelope : PEnvelope;
begin
  for I := 0 to High(Voice.Envelopes) do
  begin
    Envelope := @Voice.Envelopes[I];
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
    if Modulation.Active then
    begin
      case Modulation.Source of
        msEnv1..msEnv2 : ModValue := Voice.Envelopes[ Ord(Modulation.Source)-Ord(msEnv1) ].Value;
        msLfo1 : ModValue := Voice.Lfos[0].Value;
        msLfo2 : ModValue := Voice.Lfos[1].Value;
        msGlobalLfo1 : ModValue := GlobalLfos[0].Value;
        msGlobalLfo2 : ModValue := GlobalLfos[1].Value;
      else //todo: ifdef debug
        ModValue := 0;
      end;

      ModValue := ModValue * Modulation.Amount;

      //ModValue är nu 0..1, eller -1 .. 1 beroende på polarity

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
      Modulation.DestinationPtr^ := Value;

    end;
  end;
end;

procedure SetVoiceFrameConstants(V : PVoiceEntry);
//Uppdatera värden i Voice som gäller tills nästa gång update anropas
var
  NoteNr : single;
begin
  //IVol är 0..1 i VoicePBits fixedpoint format
  V.IVol := Trunc( V.Volume * (1 shl VoiceVolBits) );

  if StereoChannels=2 then
  begin
    //EQP panning. kebby.org.
    V.IPanL := Round( Sqrt(1.0 - V.Pan) * (1 shl VoiceVolBits) );
    V.IPanR := Round( Sqrt(V.Pan) * (1 shl VoiceVolBits) );
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
    V.SampleStep := Round((V.Osc1.Frequency / AudioRate) * (1 shl SamplePosPBits));
  end;

  //Måste ta freq*2 pga MixFullRange ej kan representeras som en integer-konstant
  V.Osc1.WStep := Round( (V.Osc1.Frequency / AudioRate) * 2 * High(integer) );
  if V.Osc1.Waveform=wfSquare then
    V.Osc1.IPulseWidth := Round(V.Osc1.PulseWidth * High(TSoundMixUnit));

  if V.UseOsc2 then
  begin
    V.Osc2.Frequency := 440.0 * Power(2, ((NoteNr + V.Osc2.NoteModifier))/12);
    V.Osc2.WStep := Round( (V.Osc2.Frequency / AudioRate) * 2 * High(integer) );
    V.IOsc2Vol := Trunc( V.Osc2Volume * (1 shl VoiceVolBits) );
  end;

  if V.UseFilter then
  begin
    //set feedback amount given f and q between 0 and 1
    V.FilterFb := Round( (V.FilterQ + V.FilterQ/(1.0 - V.FilterCutoff)) * (1 shl FilterPBits) );
    V.FilterIntCut := Round(V.FilterCutoff * (1 shl FilterPBits) );
  end;
end;

//Read a sample value. Only called from RenderVoice.
function GetSample(V : PVoiceEntry; SamplePos : integer) : TSoundMixUnit; inline;
begin
  Result := PSampleUnits(V.SampleData)^[SamplePos];
end;

procedure RenderVoice(V : PVoiceEntry; Count : integer);
const
  VoiceLowestValue =  (-1 shl (VoicePBits-1));
  VoiceHighestValue =  (1 shl (VoicePBits-1));
  VoiceToMixBits = MixPBits-VoicePBits;
var
  W1,Value1,LastW1 : TSoundMixUnit;
  W2,Value2 : TSoundMixUnit;
  Buf,DestBuf : PSoundMixUnit;
  I,Temp1 : integer;
  //Buffer where voice is rendered before mix with channelbuffer
  //Voicebuffer is always mono
  VoiceBuffer : array[0..FrameSampleCount-1] of TSoundMixUnit;
  HasSample : boolean;
  Sample1,Sample2 : TSoundMixUnit;
  SamplePos,SampleFraction : integer;
begin
  //Write to voice buffer

  //Voice beräknas med VoicePBits precision
  //MixPBits är för högt och ger integer overflow

  //Måste render även om noll volym, annars blir det klick vid avslut
  //pga att filtret inte får jobba.

  {if V.IVol=0 then
  begin
    FillChar(VoiceBuffer,SizeOf(VoiceBuffer),0);
    Exit;  //No point rendering nothing
  end;      }

  Value1 := 0;  //Get rid of warning

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
            Value1 := VoiceHighestValue
          else
            Value1 := VoiceLowestValue;
        wfSaw :
          Value1 := W1 div VoiceFullRange;
        wfNoise :
          if W1>=0 then
          begin
            //Value1 := IntRandom div (1 shl (MixToOutputBits));
            Value1 := VoiceLowestValue + Round(System.Random*(VoiceFullRange-1));
            Dec(W1, High(TSoundMixUnit) div 2 );
          end;
        wfSine :
          begin
            Value1 := VoiceLowestValue + Round( (1.0 + Sin(W1 * (1/High(integer)* PI*2) )) * (VoiceFullRange div 2-1));
          end;
      end;
    end
    else
    begin
      //Sampled waveform
      SamplePos := V.SamplePosition shr SamplePosPBits;
      if (SamplePos>=V.SampleCount) then
      begin
        //Sample pos is beyond end (repeat=-1)
        Value1 := 0
      end
      else
      begin
        SampleFraction := V.SamplePosition and ((1 shl SamplePosPBits)-1);
        //Value=(Sample1 * (1-Fraction)) + (Sample2 * Fraction)

        Sample1 := GetSample(V,SamplePos);
        Sample1 := (Sample1 * ((1 shl SamplePosPBits) - SampleFraction)) div (1 shl SamplePosPBits);

        if SamplePos<V.SampleCount-1 then
          Inc(SamplePos)
        else
          SamplePos := 0;

        Sample2 := GetSample(V,SamplePos);
        Sample2 := (Sample2 * SampleFraction) div (1 shl SamplePosPBits);

        Value1 := Sample1 + Sample2;
        Inc(V.SamplePosition,V.SampleStep);
        if (V.SamplePosition shr SamplePosPBits>=V.SampleCount) and
          (V.SampleRepeatPosition>=0) then
          V.SamplePosition := (V.SampleRepeatPosition shl SamplePosPBits) or
            V.SamplePosition and ((1 shl SamplePosPBits)-1);
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
            Value2 := VoiceLowestValue
          else
            Value2 := VoiceHighestValue;
        wfSaw :
          Value2 := W2 div VoiceFullRange;
      else
        Value2 := 0;
      end;
      Inc(W2,V.Osc2.WStep);

      Value2 := (Value2 * V.IOsc2Vol) div (1 shl VoiceVolBits);

      Value1 := Value1 + Value2; //Mix osc1 + osc2
    end;

    //Volym
    //Multiplicera med volym som är 0..1 i fixed point VoiceVolBits
    //Dela sedan med (1 shl VoicePBits) för att justera fixed-multiplication
    Value1 := (Value1 * V.IVol) div (1 shl VoiceVolBits);

    Buf^ := Value1;

    Inc(Buf);
  end;
  V.Osc1.CurValue := W1;
  V.Osc2.CurValue := W2;

  //Filter, obs måste ske *efter* volym
  //Blir brus ifall volym sker efter filter
  if V.UseFilter then
  begin
    Buf := @VoiceBuffer[0];
    for I := 0 to Count-1 do
    begin
      Temp1 := (V.FilterFb * (V.Buf0 - V.Buf1)) div (1 shl FilterPBits);
      Temp1 := Buf^ - V.Buf0 + Temp1;

      Temp1 := (V.FilterIntCut * Temp1) div (1 shl FilterPBits);

      V.Buf0 := V.Buf0 + Temp1;

      Temp1 := (V.FilterIntCut * (V.Buf0 - V.Buf1)) div (1 shl FilterPBits);
      V.Buf1 := V.Buf1 + Temp1;

      Buf^ := V.Buf1;

      Inc(Buf);
    end;
  end;

  //Skala upp till mixbits och addera till channelbuffer
  Buf := @VoiceBuffer[0];
  DestBuf := @ChannelBuffer[0];
  for I := 0 to Count-1 do
  begin
    Value1 := Buf^ * (1 shl VoiceToMixBits);

    //Mono
    if StereoChannels=1 then
    begin
      Inc(DestBuf^,Value1);
      Inc(DestBuf);
    end;

    //Stereo
    if StereoChannels=2 then
    begin
      //Left
      Inc(DestBuf^,(Value1 * V.IPanL) div (1 shl VoiceVolBits) );
      Inc(DestBuf);
      //Right
      Inc(DestBuf^,(Value1 * V.IPanR) div (1 shl VoiceVolBits) );
      Inc(DestBuf);
    end;

    Inc(Buf);
  end;
end;


procedure AddToMixBuffer(Source : PSoundMixUnit; Dest : PSoundMixUnit; Count : integer);
var
  I : integer;
begin
  for I := 0 to (Count * StereoChannels)-1 do
  begin
    Inc(Dest^,Source^);
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
end;

//Tick all voices and channels with Frame-length
procedure UpdateFrame;
var
  I,J : integer;
  PrevVoice,Voice : PVoiceEntry;
  Channel : PChannel;
  Lfo : PLfo;
  DTime : single;
begin
  IMasterVolume := Trunc( ((1 shl ChannelVolBits)-1) * MasterVolume );
  Channel := @Channels[0];
  for I := 0 to MaxChannels-1 do
  begin
    if Channel.Active and (Channel.Voices<>nil) then
    begin
      //Update channel data
      Channel.IVol := Trunc( ((1 shl ChannelVolBits)-1) * Channel.Volume );

      if Channel.UseDelay then
      begin
        Channel.DelayOutPoint := Channel.DelayInPoint -
          Trunc(Channel.DelayLength*DelayBufferSampleCount)*StereoChannels;
        while Channel.DelayOutPoint<0 do
          Inc(Channel.DelayOutPoint,DelayBufferSampleCount*StereoChannels);
      end;

      //Update global lfos
      for J := 0 to High(GlobalLfos) do
      begin
        Lfo := @GlobalLfos[J];
        if Lfo.Active then
          UpdateLfo(Lfo,AudioFrameLength);
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
          begin //först i listan
            Channel.Voices := Voice.Next;
            Voice.Next := nil;
            Voice := Channel.Voices;
          end;
          Continue;
        end;

        //Don't allow voice.time>voice.length
        DTime := AudioFrameLength;
        if Voice.Time + DTime>Voice.Length then
          DTime:=Voice.Length - Voice.Time;
        Voice.Time := Voice.Time + DTime;

        UpdateModulators(Voice,DTime);
        SetVoiceFrameConstants(Voice);

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
  I,J : integer;
  Value : TSoundMixUnit;
begin
  Buf := @ChannelBuffer;
  for I := 0 to Count-1 do
  begin
    {$ifndef minimal}
    Assert(Channel.DelayOutPoint<DelayBufferSampleCount*StereoChannels);
    Assert(Channel.DelayInPoint<DelayBufferSampleCount*StereoChannels);
    {$endif}
    for J := 0 to StereoChannels-1 do
    begin
      //Läs från delay
      Value := Channel.DelayBuffer[ Channel.DelayOutPoint+J ];

      //Mixa med input
      Value := (Value div 4) + Buf^;

      //Skriv det mixade värdet till delay-buffer för feedback
      Channel.DelayBuffer[ Channel.DelayInPoint+J ]:=Value;

      Buf^ := Value;
      Inc(Buf);
    end;

    Inc(Channel.DelayOutPoint,StereoChannels);
    if Channel.DelayOutPoint>=DelayBufferSampleCount*StereoChannels then
      Channel.DelayOutPoint := 0;

    Inc(Channel.DelayInPoint,StereoChannels);
    if Channel.DelayInPoint>=DelayBufferSampleCount*StereoChannels then
      Channel.DelayInPoint := 0;
  end;
end;

procedure RenderChannel(Channel : PChannel; Count : integer);
var
  Voice : PVoiceEntry;
  Buf : PSoundMixUnit;
  I : integer;
begin
  FillChar(ChannelBuffer,SizeOf(ChannelBuffer),0);
  if Channel.IVol=0 then
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
  for I := 0 to (Count * AudioPlayer.StereoChannels)-1 do
  begin
    //IVol är i fixed point 8 bits
    Buf^:=(Buf^ * Channel.IVol) div (1 shl ChannelVolBits);
    Inc(Buf);
  end;
end;

var
  //Minne var någonstans i frame man befinner sig mellan anrop till RenderToMixBuffer
  RenderCounter : integer;

//Main render routine, called from thread
procedure RenderToMixBuffer(Buf : PSoundMixUnit; Count : integer);
var
  I : integer;
  FrameCrossOvers,FramesLeft,FrameCount : integer;
  Finished : boolean;
  Channel : PChannel;
  VBuf : PSoundMixUnit;
begin
  FrameCrossOvers := (RenderCounter+Count) div FrameSampleCount;
  if FrameCrossOvers>0 then
    FrameCount := FrameSampleCount - RenderCounter
  else
    FrameCount := Count;

  FramesLeft := FrameCrossOvers;
  repeat

    Channel := @Channels[0];
    for I := 0 to MaxChannels-1 do
    begin
      if Channel.Active and ((Channel.Voices<>nil) or Channel.UseDelay) then
      begin
        RenderChannel(Channel,FrameCount);
        //Add channel to main mix
        AddToMixBuffer(@ChannelBuffer,Buf,FrameCount);
      end;
      Inc(Channel);
    end;

    if IMasterVolume<((1 shl ChannelVolBits)-1) then
    begin
      //Volume
      VBuf := Buf;
      for I := 0 to (FrameCount * AudioPlayer.StereoChannels)-1 do
      begin
        VBuf^:=(VBuf^ * IMasterVolume) div (1 shl ChannelVolBits);
        Inc(VBuf);
      end;
    end;

    if FramesLeft>0 then
    begin
      //Ny frame, uppdatera modulators
      UpdateFrame;
      Dec(FramesLeft);
      Inc(Buf,FrameCount * StereoChannels);
      Dec(Count,FrameCount);
      if Count>FrameSampleCount then
        FrameCount := FrameSampleCount
      else
        FrameCount := Count;
      Finished := FrameCount<=0;

      //And update midi music
      if AudioComponents.CurrentMusic<>nil then
        AudioComponents.CurrentMusic.AdvanceMusic(FrameSampleCount / AudioRate);
    end
    else
      Finished := True;

  until Finished;

  if FrameCrossOvers>0 then
    RenderCounter := FrameCount
  else
    Inc(RenderCounter,Count);
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
  I: Integer;
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
  for I := 0 to High(V.Envelopes) do
    V.Envelopes[I].State := esInit;
  V.Active := True;
  V.NoteNr := Note.NoteNr;
  //V.Volume := 0.25;
  if Note.Length <> 0 then
    //Override sound length-value
    V.Length := Note.Length;

  //Modulate volume with velocity (0..1)
  V.Volume := V.Volume * Note.Velocity;

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
  SetVoiceFrameConstants(V);
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
  DelayBufferByteSize = DelayBufferSampleCount * SizeOf(TSoundMixUnit) * StereoChannels;
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
