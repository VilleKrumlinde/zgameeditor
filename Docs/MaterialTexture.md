# MaterialTexture {#MaterialTexture}

Represents a texture that is used by a material. 

This component can only be used in the @ref MaterialTextures "Material.Textures" property.

See also: @ref Material

## Properties

@dl

@dt Texture
@dd Set to a @ref Bitmap that the material will use for texturing.

@dt RenderTarget
@dd Set to a @ref RenderTarget that the material will use for texturing instead of bitmap. If used, the Texture property should be left empty.

@dt TextureScale
@dd This value controls scaling of the texture. 1.0 is the default value for normal scale. 0.5 is half scale, 2.0 is double scale. You can specify a separate scale for the X and Y axes; the Z axis is ignored.

@dt TextureX, TextureY
@dd This values controls the texture offset in X and Y axis. Animate the values to "move" the texture over the rendered object.

@dt TextureRotate
@dd This value controls rotation of the texture.

@dt TextureWrapMode
@dd This values controls how the texture will repeated over the mesh surface:

* Mirror - The texture is repeated with every other repetition mirrored (flipped).
* Tile - The texture is repeated normally.
* Clamp - No repetition, pixels outside the texture will hold the same color as the textures edges.

@dt TexCoords
@dd This value controls how texture coordinates are fetched.

* Generated - Material will use automatically generated texture coordinates.
* ModelDefined - Texture coordinates supplied with the model will be used. Set this value when using the @ref RenderSprite component.

@dt Origin
@dd Origin of texture rotation. Use the X and Y axes; the Z axis is ignored.

@dlx
