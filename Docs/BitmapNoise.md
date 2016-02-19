# BitmapNoise {#BitmapNoise}

Generates a Perlin noise (i.e., a coherent pseudo random) bitmap.

This component can only be used in the @ref BitmapProducers "Bitmap.Producers" property.

See also: @ref Bitmap

## Properties

@dl

@dt StartingOctaves
@dd Determines the base granularity of noise.

@dt Octaves
@dd Determines the finer granularity of noise - the number of higher frequencies that will be combined.

@dt Offset
@dd Specifies darkness of the generated bitmap.

@dt Persistence
@dd Determines contrast of the generated noise.

@dt ZHeight
@dd Variation of noise pattern.

@dt Color
@dd Color of the generated bitmap. It can be one of the following:

* White
* Red
* Green
* Blue

@dt Tile
@dd If checked, the generated bitmap is a seamless tile usable for texture tiling.

@dlx
