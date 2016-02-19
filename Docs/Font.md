# Font {#Font}

Defines a font that can be used to display text using the @ref RenderText component.

The fonts graphics must be defined in a bitmap and imported to the project using the @ref BitmapFromFile component. Select the font by setting the Font property of a @ref Material and then use the material with @ref UseMaterial. Here are a couple of examples of bitmaps that can be used as fonts:

![FirstChar=33, Height/Width=8, Borderpixels=1](comp-font-size9-256x32.png)

![FirstChar=33, Height/Width=31, Borderpixels=1](comp-font-size32-512x128.png)

## Properties

@dl

@dt Bitmap
@dd Set to a @ref Bitmap that holds the font graphics.

@dt CharPixelWidth, CharPixelHeight
@dd The pixel width and height of the characters in the font.

@dt FirstChar
@dd The ASCII value of the first character in the font. 32 is space. 65 is 'A'. 48 is '0'.

@dt BorderPixels
@dd If the font has extra pixels below and to the right then set this property to the number of extra pixels.

@dlx
