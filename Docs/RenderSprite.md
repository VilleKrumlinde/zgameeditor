# RenderSprite {#RenderSprite}

Render a 2D sprite. To change size (e.g., to draw a rectangle) or rotation of the sprite use @ref Model's Scale or Rotate properties, @ref RenderTransform or @ref RenderTransformGroup specified before RenderSprite. The sprite can either be displayed in:

* a single color, if the used @ref Material does not use texture(s),
* a static bitmap, if the used Material specifies texture(s), or
* a (possibly) dynamic bitmap, if the bitmap is selected from a @ref SpriteSheet by SpriteIndex which may change in time.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components. If other rendering commands are used after RenderSprite in the same OnRender property, you must add another call to @ref UseMaterial to render them properly, even if they use the same material.

Example usage: "TitleScreenModel.OnRender" in @ref TripleE sample project.

See also: @ref Material, @ref SpriteSheet

External links: [Discussion about tiles and sprites on the Forum](http://www.emix8.org/forum/viewtopic.php?f=1&t=1159)

# Properties

@dl

@dt SpriteSheet
@dd Set to a @ref SpriteSheet that holds the bitmap.

@dt SpriteIndex
@dd Index of the sprite in the SpriteSheet. Change the index in runtime to achieve sprite animation.

@dt MirrorHorizontal
@dd If set, the sprite bitmap is flipped horizontally.

@dt MirrorVertical
@dd If set, the sprite bitmap is flipped vertically.

@dlx
