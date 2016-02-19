# BitmapExpression {#BitmapExpression}

A bitmap producer component that generates a bitmap from a mathematical expression.

This component can only be used in the @ref BitmapProducers "Bitmap.Producers" property.

See also: @ref Bitmap

Example usage: BallTexture in @ref ZPong sample project.

## Properties

@dl

@dt Expression
@dd An expression which defines the bitmap. The expression is called once for every pixel in the bitmap. A number of special variables are valid within the expression:
* "X" and "Y" holds the current x and y coordinate in 0 to 1 range.
* Assign "Pixel" color (vec3) to set the current pixel.
* "this" refers to the current Bitmap. It can be omitted if built-in variables do not collide with locally defined variables.

Example of an expression: 

    this.Pixel.R = abs(X-0.5)*2;

@dlx
