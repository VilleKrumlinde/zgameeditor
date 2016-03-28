# AnimatorSimple {#AnimatorSimple}

Animates a floating point property of another component.

See also: @ref AnimatorGroup

## Properties

@dl

@dt Duration
@dd Length of this animation.

@dt BeginTime
@dd Delay before this animation starts.

@dt AutoStart
@dd If set then this animator will start automatically. If not set then it can be started with a StartAnimator component.

@dt Target
@dd The target property which the animation controls. Value of this property is an @ref ScriptingLanguage "expression" specified in @ref CodeEditor "Code editor".

Example: "ShipModel.Scale.X".

@dt FromValue
@dd Start value of animation.

@dt FromKind
@dd How to get the start value:

* UseFromValue - Use the value of the FromValue property.
* UseTargetValue - Use the current value of the Target property. 

@dt ToValue
@dd End value of animation.

@dt Smooth
@dd If set, then a smooth transition from FromValue to ToValue will be used. If not set, a linear transition from FromValue to ToValue will be used.

@dt AutoReverse
@dd If set then the animation will be played in reverse when it has finished. The reverse of a animation will start at the ToValue and end at the FromValue.

@dt RepeatCount
@dd Number of times to repeat animation. Set to -1 for infinite repeat.

@dlx
