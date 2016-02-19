# Bitmap {#Bitmap}

A representation of a bitmap image that can be used as a texture.

See also: ref Material, ref BitmapExpression

## Properties

@dl

@dt Height
@dd Bitmap height.

@dt Width
@dd Bitmap width.

@dt Filter
@dd Controls how the bitmap is displayed. Can be set to one of the following values:

* Linear - Smooth linear filter
* Nearest - Nearest pixel
* Mipmap - Smooth linear filter using mipmaps

@dlx

## List Properties

@dl

@dt @anchor BitmapProducers Producers
@dd A list of components that provide the definition of the bitmap. The following components can be included here:

* @subpage BitmapBlur
* @subpage BitmapCells
* @subpage BitmapCombine
* @subpage BitmapConvolution
* @subpage BitmapDistort
* @subpage BitmapExpression
* @subpage BitmapFromFile
* @subpage BitmapLoad
* @subpage BitmapNoise
* @subpage BitmapPixels
* @subpage BitmapRect
* @subpage BitmapZoomRotate

@dlx
