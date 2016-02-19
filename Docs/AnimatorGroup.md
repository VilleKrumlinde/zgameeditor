# AnimatorGroup {#AnimatorGroup}

A group of animators.

See also: @ref AnimatorSimple

## Properties

@dl

@dt Duration
@dd Length of this group, in seconds.

@dt BeginTime
@dd Delay before this animation starts, in seconds.

@dt AutoStart
@dd If set then this animator will start automatically, if not set then it can be started with a @ref StartAnimator component.

@dlx

## List Properties

@dl

@dt Animators
@dd List of @ref AnimatorSimple and/or AnimatorGroup components controlled by this AnimatorGroup.

@dt OnStart
@dd Called before the group and all its children start their animation.

@dt OnStop
@dd Called when the group and all its children have finished their animation.

@dlx
