# BitmapDistort {#BitmapDistort}

Distorts a bitmap using the content of another bitmap. Pixels of the 1st bitmap are shifted down by the G pixel value and left by the B pixel value of the 2nd bitmap at the same position.

This component can only be used in the @ref BitmapProducers "Bitmap.Producers" property.

See also: @ref Bitmap

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?p=2057#p2057)

## Properties

@dl

@dt Amount
@dd Amount of distortion applied in both directions. G and B pixel values are multiplied by this value.

@dlx
