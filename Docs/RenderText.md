# RenderText {#RenderText}

Render text on screen using the current material.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

See also: @ref Font, @ref Material.

## Properties

@dl

@dt Text
@dd The text that will be rendered.

@dt TextFloatRef
@dd A reference to a floating point property that have its value rendered. The value will first be rounded to an integer, use FloatMultiply if decimals are needed. Value of this property is an @ref ScriptingLanguage "expression" specified in @ref CodeEditor "Code editor".

@dt TextArray
@dd Integer @ref Array of which cells represent ASCII codes of the displayed characters. Dimension(s) of array determine the length of the displayed text. If the array is multi-dimensional, the length of text is given by multiplication of dimension sizes and the characters are displayed in the order they are stored in memory.

@dt X, Y
@dd X and Y coordinates where text will be rendered. Range is -1 to 1, with 0 at center. See also UseModelSpace property.

@dt Scale
@dd Scale of text that will be rendered.

@dt Align
@dd Align text: Left or Center. If the text includes line breaks, then the text will be centered using the width of the first line only.

@dt RenderCharExpression
@dd An expression that will be executed before each character is rendered. This is useful for making animations. Special properties that can be modified in the expression are: CharX, CharY, CharRotate, CharScale and CharI.

@dt FloatMultiply
@dd If TextFloatRef is set, then the value will first be multiplied using FloatMultiply before render.

@dt UseModelSpace
@dd If set then the text will be rendered relative to the current position of the Model executing the RenderText command. If not set (default) then the text will be rendered in screen coordinates.

@dt StretchY
@dd Default value is 1. Modify this value to stretch or shrink the text in the Y axis.

@dt CharX, CharY
@dd X and Y coordinates of the current character. These properties can be accessed only from the RenderCharExpression.

@dt CharI
@dd Read only. Index of the current character. It can be accessed only from the RenderCharExpression.

@dt CharRotate
@dd Rotation of the current character in radians. It can be accessed only from the RenderCharExpression.

@dt CharScale
@dd Scale of the current character. It can be accessed only from the RenderCharExpression.

@dlx
