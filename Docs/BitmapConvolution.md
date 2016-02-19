# BitmapConvolution {#BitmapConvolution}

Applies convolution to a bitmap.

This component can only be used in the @ref BitmapProducers "Bitmap.Producers" property.

See also: @ref Bitmap

External links: [Discussion of this component in the Forum](http://www.emix8.org/forum/viewtopic.php?p=2262#p2262), [Article on Wikipedia](https://en.wikipedia.org/wiki/Kernel_%28image_processing%29#Convolution)

## Properties

@dl

@dt SwapDimensions
@dd If checked, the dimensions of convolution array are swapped. 

@dt ConvArray
@dd 2D array representing the convolution matrix (mask). It is usually 3x3, 5x5, or 7x7 array.

@dt Divisor
@dd A number that divides the result of convolution.

@dt Bias
@dd A number that is added to the result of convolution.

@dlx
