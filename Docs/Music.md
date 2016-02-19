# Music {#Music}

Represents a imported MIDI music file (*.mid) that can be played in your application using the @ref MusicControl component.

Note: The midi-functionality in ZGE is very limited. A better approach is to play music using external libraries, such as, BASS, SunVox or MikMod.

Example usage: see the included project @ref ModPlay for an example.

## Properties

@dl

@dt MusicFile
@dd The content of the imported MIDI-file. Click the "Import..." button that is located beside the property in the editor to browse for a music (*.mid) file. When a file is selected it is imported to your application and is not read from disk.

@dt LoopPlayback
@dd If checked, the music playback is looped.

@dt Tempo
@dd Sets tempo ratio of the music playback.

@dt Volume
@dd The volume of the music playback. Valid values range from 0 (silent) to 1 (full volume).

@dt @anchor MusicNoteParam NoteParam
@dd Read only. Number of the currently played note. Middle C is 60. One octave is given by 12 tones.

@dt @anchor MusicNoteChannelParam NoteChannelParam
@dd Read only. Channel of the currently played note.

@dt @anchor MusicNoteLengthParam NoteLengthParam
@dd Read only. Length of the currently played note.

@dlx

# List Properties

@dl

@dt Instruments
@dd A list of @ref Sound components that are used as instruments for the music. They are created when MIDI file is imported, but you can modify them later.

@dt OnPlayNote
@dd A list of components executed when a MIDI note is played. These components usually process the currently played note accessed through the properties @ref MusicNoteParam "NoteParam", @ref MusicNoteChannelParam "NoteChannelParam" and @ref MusicNoteLengthParam "NoteLengthParam".

@dlx
