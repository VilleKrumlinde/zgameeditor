# Material {#Material}

A definition of material properties for rendered objects.

See also: @ref UseMaterial, @ref MaterialTexture

## Properties

@dl

@dt WireframeWidth
@dd The width of lines in Wireframe mode.

@dt Shading
@dd The style of shading that this material will use. Can have one of the following values:

* Smooth - smooth shading.
* Flat - flat shading.
* Wireframe - wireframe.

@dt Color
@dd The color of the material. See also @ref RenderSetColor for an alternative of setting individual colors to models using the same material.

@dt Light
@dd If set then the material will be lit.

@dt Blend
@dd Controls the blending mode used. Can have one of the following values:

* None
* Alpha/OneMinusSourceAlpha
* Alpha/One
* Color/OneMinusSourceColor
* AlphaSaturate/One

See external link: [Transparency, Translucency, and Blending (OpenGL.org)](http://www.opengl.org/resources/faq/technical/transparency.htm).

@dt ZBuffer
@dd If set, the Material will use Z-buffer when rendering.

@dt DrawBackFace
@dd If this value is set then surfaces turned away from the camera will be rendered. Set this value when rendering transparent surfaces.

@dt Font
@dd Set to a @ref Font and then use @ref RenderText to display text using the font.

@dt Shader
@dd If set to a @ref Shader then the material will use the shader for fragment (pixel) and vertex processing.

@dlx

## List Properties

@dl

@dt @anchor MaterialTextures Textures
@dd A list of @subpage MaterialTexture - components that each represents a texture that will be blended together for the finished material.

@dlx
