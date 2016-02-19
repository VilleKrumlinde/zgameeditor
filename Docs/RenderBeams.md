# RenderBeams {#RenderBeams}

Render "beams" using the current material. This is a special effect that can be useful for rendering explosions.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components.

Example usage: "BossDeadModel.OnRender" in @ref TripleE sample project.

## Properties

@dl

@dt Count
@dd Number of beams to render.

@dt Length
@dd Lenght of beams.

@dt Width
@dd Width of beams.

@dt Speed
@dd Controls how fast beams rotate around origin.

@dlx
