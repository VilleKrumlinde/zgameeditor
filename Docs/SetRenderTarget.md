# SetRenderTarget {#SetRenderTarget}

Activates or deactivates the @ref RenderTarget for rendering.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

Example usage: @ref RenderTexture and @ref RenderPass sample projects.

See also: @ref RenderTarget

## Properties

@dl

@dt RenderTarget
@dd If a RenderTarget is specified, all consequent rendering components will produce output to this render target. Use a blank value to stop rendering to the render target and go back to rendering to screen.

@dlx
