# PlaySound {#PlaySound}

Play a sound using the real-time synthesizer.

See also: @ref Sound, @ref AudioMixer

## Properties

@dl

@dt Sound
@dd The sound that will be played.

@dt NoteNr
@dd The note number of the sound. Middle C is 60. Add or remove 12 to change octave. When the sound is played this value is added to the sounds BaseNoteNr-property to determine the final pitch of the sound.

@dt Channel
@dd The number of the channel that the sound will be played in.

@dt ReplayDelay
@dd A delay in seconds before this PlaySound can be repeated.

@dt ByReference
@dd When this is checked then the sound isn't cloned when emitted so scripting can modify the properties and they will take effect in realtime.

@dlx
