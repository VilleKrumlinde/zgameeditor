# Sound {#Sound}

Define a sound using the real-time synthesizer. Sounds are designed in the
editor using the custom sound editor that is displayed when a Sound component is
selected.

The real-time synthesizer is a simple form of audio generator similar to the
analogue synthesizers from the early 1980s. It is useful for producing synthetic
sounding sound effects. It can also playback sampled audio.

![The custom audio editor](comp-sound-editor.png)

External links on Wikipedia:
[Modular synthesizer](https://en.wikipedia.org/wiki/Modular_synthesizer),
[ADSR envelope](https://en.wikipedia.org/wiki/Synthesizer#ADSR_envelope),
[LFO](https://en.wikipedia.org/wiki/Low-frequency_oscillation),
[Audio filter](https://en.wikipedia.org/wiki/Audio_filter)

See also: @ref PlaySound

## Properties

@dl

@dt Length @dd The length of the sound in seconds.

@dt Volume @dd Volume: 0 silent, 1 max. The final volume depends on the volume
of the channel that the sound is played in, see AudioMixer.

@dt BaseNoteNr @dd The note number of the sound. Middle C is 60. Add or remove
12 to change octave.

@dt Osc1Waveform @dd Waveform of the first oscillator. Square (0), Saw (1),
Noise (2) or Sine (3).

@dt Osc1NoteModifier @dd A value for detuning the oscillator from the
BaseNoteNr.

@dt Osc1PW @dd A value between -1 and 1 that controls relative square pulse
duration of the first oscillator. 0 (default) produces perfect square wave. This
parameter does not influence other waveforms.

@dt UseOsc2 @dd Set this property if the second oscillator should be used.

@dt Osc2Waveform @dd Waveform of the second oscillator. Square, Saw, Noise.

@dt Osc2NoteModifier @dd A value for detuning the oscillator from the
BaseNoteNr.

@dt Osc2Volume @dd A value between 0 and 1 that controls the volume of the
second oscillator.

@dt HardSync @dd If this property is set, then osc2 will be restarted when osc1
restarts.

@dt UseFilter @dd Set to use filter.

@dt FilterCutoff @dd Cut-off value of the filter, 0 to 1 range.

@dt FilterQ @dd Filter "Q", or resonance. 0 to 1 range.

@dt Pan @dd Sound stereo panning. 0 full left, 1 full right, 0.5 centered
(default).

@dt Sample @dd If set to a @ref Sample then the sound waveform is taken from
this sample instead of the first oscillator. However, the second oscillator and
the filter can be used together with the sample.

@dt SampleRepeatPosition @dd The sample position where the audio will be
repeated if the sound is played for longer than the sample length. The value is
expressed as position in sample data. Size of sample data can be computed as
Sample.Length _ SampleImport.SampleRate for imported sample, or Sample.Length _
44100 for computed sample. For instance, if sample data has size 40000, setting
the SampleRepeatPosition to 20000 repeats sound from middle. Setting the
SampleRepeatPosition to negative number disables sample looping.

@dt UseSampleHz @dd Check this box to playback sample at its original speed,
without having to find the correct note number. Uncheck this box when no sample
is used.

@dt @anchor SoundPatternString PatternString @dd Use is to set a new pattern.
Pattern notes are specified by their keys:

| Key   | Note  |
| ----- | ----- |
| a     | C     |
| w     | C#    |
| s     | D     |
| e     | D#    |
| d     | E     |
| f     | F     |
| t     | F#    |
| g     | G     |
| y     | G#    |
| h     | A     |
| u     | A#    |
| j     | B     |
| Space | pause |

@dlx

## Pattern

Sequence of notes to be played with the defined sound. It can be used to test
the edited sound. New pattern is set by means of the @ref SoundPatternString
"PatternString" property. The "Copy to Music" button allows to copy the current
pattern into a @ref Music component that can be pasted to the application's
component tree to play the specified pattern with this sound.

## Modulator properties

Each sound can have a number of modulations for varying the sound over time. For
every modulator the following values can be set:

- Active - Set if this modulator is active.
- Source - The source value of this modulation. An LFO, GlobalLFO or an
  envelope.
- Destination - The value to modulate (change).
- Amount - How much the destination value will be changed from the source value.

## LFO properties

Each sound can have a number of LFOs (low-frequency oscillator). An LFO is a
function which slowly changes its value over time. This is used for modulating
the sound. For every LFO the following values can be set:

- Active - Set if this LFO is active.
- Style - The function this LFO use. Sine, Random or ZeroOne.
- Bipolar - Set if this LFO has the range -1 to 1, instead of 0 to 1.
- Speed - Speed of this LFO.

## Envelope properties

Each sound can have a number of ADSR envelopes. For every envelope the following
values can be set:

- Active - Set if this envelope is active.
- Attack time (A) - The time taken for initial run-up of level from zero to
  peak, beginning when the sound is being played.
- Decay time (D) - The time taken for the subsequent run down from the attack
  level to the designated sustain level.
- Sustain level (S) - The level during the main sequence of the sound's
  duration.
- Release time (R) - The time taken for the level to decay from the sustain
  level to zero.
