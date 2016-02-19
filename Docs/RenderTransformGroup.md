# RenderTransformGroup {#RenderTransformGroup}

Move, scale or rotate the graphics that is being rendered. Only the render commands in the Children property will be affected by the transformation.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

See also: @ref RenderTransform

## Properties

@dl

@dt Scale
@dd Scale in x, y and z axis.

@dt Translate
@dd Translate (move) in x, y and z axis.

@dt Rotate
@dd Rotate in x, y and z axis. Unit of rotation is cycles. 1 cycle=360 degrees.

@dlx

## List Properties

@dl

@dt Children
@dd The render commands that will be affected by the transformation.

@dlx
