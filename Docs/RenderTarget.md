# RenderTarget {#RenderTarget}

Defines a render target for rendering to texture. To make the render target active for rendering, use the @ref SetRenderTarget component.

_Note: This feature needs a graphic card that supports FBOs (GL_EXT_framebuffer_object OpenGL extension). If your card does not support this then ZGE designer will display a message in the log on start up. Also note that each RenderTarget takes up a fair amount of video memory so don't use too many instances._

Example usage: "RenderTarget1" in @ref RenderTexture sample project.

See also: @ref SetRenderTarget, RenderTarget property of @ref MaterialTexture

## Properties

@dl

@dt Width
@dd Viewport width. It is set to one of the following values:

* Half viewport width
* Quarter viewport width
* 128
* 256
* 512

@dt Height
@dd Viewport height. It is set to one of the following values:

* Half viewport height
* Quarter viewport height
* 128
* 256
* 512

@dt CustomWidth
@dd Custom, user-defined width that can be used instead of the predefined values of Width property.

@dt CustomHeight
@dd Custom, user-defined height that can be used instead of the predefined values of Height property.

@dt ClearBeforeUse
@dd If set, the render target's content is cleared with value of ClearColor property before rendering to it.

@dt AutoPowerOfTwo
@dd Set this property to make the actual size of the render target to be the next larger power of two. This makes it compatible with older ATI-drivers that doesn't support render targets of other sizes.

@dt ClearColor
@dd Clear color of render target. It is applied if the ClearBeforeUse property is set.

@dt Filter
@dd Controls how the texture is displayed. Can be set to one of the following values:

* Linear - Smooth linear filter
* Nearest - Nearest pixel
* Mipmap - Smooth linear filter using mipmaps

@dlx
