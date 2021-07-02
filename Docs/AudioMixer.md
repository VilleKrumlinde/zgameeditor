# AudioMixer {#AudioMixer}

A mixer for the real-time audio synthesizer. When a 2ref Sound component is selected in the editor, the AudioMixer custom editor is displayed as a separate page next to the audio custom editor.

The mixer is used for changing settings to different audio channels.

@note To save the mixer settings you must add a audiomixer component to your project. You can only have a single AudioMixer in one project.

See also: @ref Sound

## Properties

@dl

@dt MasterVolume
@dd A value from 0 to 1 controlling the volume of all audio. Default is 1.

@dt Channel Properties
@dd Every channel (16 channels) have the following properties:

* __Active__ - Set if this channel should be active. Default: channels 0 and 1 are active.
* __Volume__ - A value from 0 to 1 controlling the volume of this channel.
* __UseDelay__ - Set if this channel should use a delay-filter (an echo effect).
* __DelayLength__ - Length of the delay effect.

@dt Global LFOs Properties
@dd An LFO is a function which slowly changes its value over time. This is used for modulating the sound. The global LFOs are different from the LFOs on the sound component because they do not reset when sound is initiated. When a sound is modulated using a global LFO it can make the sound a bit different every time.

For every global LFO the following values can be set:

* __Active__ - Set if this LFO is active.
* __IsBipolar__ - Set if this LFO has the range -1 to 1, instead of 0 to 1.
* __Style__ - The function this LFO use. Sine, Random or ZeroOne.
* __Speed__ - Speed of this LFO.

@dlx
