# RenderTransform {#RenderTransform}

Move, scale or rotate the graphics that is being rendered. All render-commands following a RenderTransform will be affected by the transformation.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

See also: @ref RenderTransformGroup

## Properties

@dl

@dt Scale
@dd Scale in x, y and z axis.

@dt Translate
@dd Translate (move) in x, y and z axis.

@dt Rotate
@dd Rotate in x, y and z axis. Unit of rotation is cycles. 1 cycle=360 degrees.

@dlx
