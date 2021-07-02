# Visualizer {#Visualizer}

**ZGameEditor Visualizer** (**ZgeViz**) is a visualization effect plugin for
[FL Studio](https://www.image-line.com/flstudio) digital audio workstation with
movie render capability. See the
[plugin help page](https://www.image-line.com/support/flstudio_online_betamanual/html/plugins/ZGameEditor%20Visualizer.htm)
for usage details.

When you have created a .zgeproj file with an effect, store it in the Effects
directory or any of its sub-directories of the "ZGameEditor Visualizer" plugin
directory. The visualizer will load all effects on startup. If you make a change
in the .zgeproj file while the effect is running in the visualizer, the effect
is automatically reloaded to apply change.

@note ZGE projects of all out-of-the-box effects shipped with FL Studio can be
edited from the Visualizer by pressing the "Edit effect in ZGameEditor" pop-up
menu from the the effect layer.

# Effect Information {#ZgevizEffectInfo}

Use the _Comment_ property of the @ref ZApplication component to write help text
about your effect that will be displayed in ZgeViz when right mouse is clicked
and the "Show help for this effect" is chosen. The comment should contain also
description of effect parameters which will be displayed in the Hint panel of FL
Studio when mouse is over the effect's parameter. Each parameter is described on
a separate line in the form: @<parameter name@> - @<description@>.

Put a @ref Constant component with name "AuthorInfo" and specify the effect's
author name to its StringValue property. In a script use: @tt{const string
AuthorInfo = "author";}. The author name is displayed in the effect panel.

# Custom Content {#ZgevizCustomContent}

Effects can use the media content specified by user in the "Add content" tab of
the visualizer.

## Images

Any image (picture file, video file, web camera, etc.) specified in the _Add
content_ / _Images_ tab of the visualizer dialog replaces the content of a @ref
Bitmap component from the effect ZGE project file. The IMAGE SRC property of the
effect specifies which image is rendered to the first bitmap of the project. If
the visualizer specifies several content images and the effect project uses
several bitmaps they are replaced by content images in the same order.

If a @ref Bitmap component specifies the "NoCustom" Comment, it is not replaced
by specified content image.

## Meshes

Analogously to images, also meshes specified in the visualizer replace content
of the @ref Mesh components in the effect's ZGE project. The MESH property
specified the first mesh data replacing the content of the first @ref Mesh
component. If the visualizer specifies several content meshes and the effect's
project uses several meshes, they are replaced in the same order as occur in the
project file.

## HTML

If a ZGE project file contains a @ref Bitmap component named "UserTextBitmap"
its content is given by the rendered HTML code specified in _Add content_ /
_HTML_ tab of the visualizer dialog.

## Text

Text specified in the _Add content_ / _Text_ tab of the visualizer dialog is
placed to a built-in array UserTextArray. See @ref UserTextArray "here" for
details.

# Built-in Components {#ZgevizBuiltinComponents}

The following components must be inserted to the ZGE project manually but their
content is set by visualizer automatically.

@note For all components also their scripting variant is specified here.
Scripting definitions of components should be put to a @ref ZLibrary component
in the @ref ZApplicationOnLoaded "OnLoaded section" of the ZGE project tree.

@dl @dt @anchor UserTextArray UserTextArray @dd @ref Array, Name:
"UserTextArray", SizeDim1: 0, Type: string

    string[0] UserTextArray;

Add this array if your effect needs to use the strings entered in the custom
content dialog.

TODO string[3] FLProjectInfoArray;

@dt SpecBandArray @dd @ref Array, Name="SpecBandArray", SizeDim1=32, Type=float

    float[32] SpecBandArray;

This array is filled with the amount of audio at different frequencies. The
values range from -1 to 1. Note that when the effect is loaded the actual length
will be set by visualizer. The initial size is 32 but it can be configured to
another size by the user in the settings dialog, so effects should be written
with this in mind.

@dt AudioArray @dd @ref Array, Name="AudioArray", SizeDim1=32, Type=float

    float[32] AudioArray;

This array is filled with the audio pcm data that is currently playing. The
values range from -1 to 1. The actual size of this array is set at runtime.

@dt SongPositionInBeats @dd @ref Variable, Name="SongPositionInBeats",
Type=float

    float SongPositionInBeats;

This variable holds the current position in beats. An example use of this
variable is to take the fraction part and modify camera Z position to make the
scene pulse in sync with the music.

@dt ParamHelpConst @dd @ref Constant, Name="ParamHelpConst", Type=string,
StringValue=@<parameter definition@>

    const string ParamHelpConst = "Param1 @<type@>\nParam2\n...";

This is a string that is used to hold the names and types of the parameters the
effect is using. So if you have an effect with two parameters that controls
shape and color, this property could hold the names "Shape" and "Color"
separated with a carriage return. The names of the parameters are shown as
labels in the visualizer user interface. Optionally, a parameter can specify the
widget type used in the effect editor to modify its value. The type is specified
after the parameter name separated by space. There are the following parameter
types:

- _none_ - parameter is displayed as a slider.
- \@checkbox - parameter is displayed as a checkbox.
- \@list: "first","second","last item" - parameter is displayed as drop-down
  list. Values of items are evenly distributed among 0 (for 1st item) and 1 (for
  last item). List items are separated by comma or by space. If an item label
  contains space characters, it should be enclosed in double quotation marks.
- \@list1000: - parameter is displayed as optionally hierarchical drop-down
  list. Its values are set dynamically by calling the @ref
  ParamsUpdateComboItems "ParamsUpdateComboItems() function". Index of the
  selected item is given by @tt{Parameters[x] \* 1000}.

@dt Parameters @dd @ref Array, Name="Parameters", SizeDim1=@<number of
parameters@>, Type=float, Persistent=true

    float[<number of parameters>] Parameters;

This is a one-dimensional array that hold values that range from 0 to 1 and are
controlled by widgets in the visualizer interface. They can be used freely (or
ignored) in the effect to modify visuals.

@dt FLPluginHandle @dd @ref Variable, Name="FLPluginHandle", Type=xptr

    xptr FLPluginHandle;

Variable that returns the window handle to the visualizer plugin. The value is
used in @ref ZgevizFunctions [functions described later].

@dt LayerNr @dd @ref Variable, Name="LayerNr", Type=int

    int LayerNr;

Variable that indicates what layer the current effect is loaded on. The value is
used in @ref ZgevizFunctions [functions described later].

@dt FLPluginPath @dd @ref Variable, Name="FLPluginPath", Type=string

    string FLPluginPath;

Variable set to the path of the visualizer plugin.

@dlx

# MIDI Input {#ZgevizMidi}

@note !!! maybe put this to a separate section Callbacks !!!

Effects can act on MIDI messages, for instance, to watch note on/off messages
that could be used for interactive effects. Put the following callback function
to a @ref ZLibrary component placed in @ref ZApplicationOnLoaded "OnLoaded" or
@ref ZApplicationContent "Content":

    void OnMidiMessage(int status, int data1, int data2) {
    	<your code to process parameters>
    }

[Table of MIDI messages.](http://www.midi.org/techspecs/midimessages.php)

# Built-in Functions {#ZgevizFunctions}

@note If you want to use any of the following functions in your effect you must
declare them explicitly in a @ref ZExternalLibrary component.

<!-- WORKING HERE

TBD add descriptions on web...

@dl

@dtn void @bf{ParamsNotifyChanged} (xptr Handle,int Layer) @dd Returns the
absolute value of _x_.

@dlx

TBD maybe some of them are really callbacks; if so, add a section Callbacks
after functions and describe what is a callback (function with predefined
signature called by the engine on event and the body represents handling of the
event by effect... also MIDI message processing move there!!!)

- @anchor ParamsUpdateComboItems ParamsUpdateComboItems
- ZgeVizGetFileNames
- void ParamsChangeName(xptr Handle,int Layer, int Parameters, string NewName) {
  }
- void ZgeVizGetFileNames(string path, string pattern, string[] list) { }
- void ParamsNotifyChanged(xptr Handle,int Layer) { }
- void ParamsWriteValueForLayer(xptr Handle, int Layer, int Param, float
  NewValue) {}
- int ReadPrivateData(xptr Handle, xptr Data, int Size) {}
- void WritePrivateData(xptr Handle, xptr Data, int Size) {}
- void ParamsUpdateComboItems(xptr Handle, int Layer,int Param, string[]
  NewItems) {}
- void ZgeVizGetFileNames(string path, string pattern, string[] list) {}
- xptr ZgeVizSvgOpen(string fileName) {}
- string ZgeVizSvgGetContent(xptr svg) {}
- void ZgeVizSvgClose(xptr svg) {}
- void ZgeVizSetEnablePostProcess(xptr Handle,int Layer, int Value) { }

TODO go through all effects and search for built-in functions, components, etc.

-->
