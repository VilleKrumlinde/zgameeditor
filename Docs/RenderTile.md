# RenderTile {#RenderTile}

Render a 2D tile on screen. This component is intended to be used for drawing 2D background maps composed of tiles. To change size (e.g., to draw a rectangle) or rotation of the sprite use @ref Model's Scale or Rotate properties, @ref RenderTransform or @ref RenderTransformGroup specified before RenderTile.

This component can only be used in the OnRender property of @ref Model, @ref ModelState, @ref ZApplication or @ref AppState components. If other rendering commands are used after RenderTile in the same OnRender property, you must add another call to @ref UseMaterial to render them properly, even if they use the same material.

See also: @ref TileSet

External links: [Discussion about tiles and sprites on the Forum](http://www.emix8.org/forum/viewtopic.php?f=1&t=1159)

# Properties

@dl

@dt TileSet
@dd Set to a @ref TileSet that specifies bitmaps of tiles.

@dt TileIndex
@dd Index of the tile in TileSet.

@dt OriginX, OriginY
@dd Horizontal and vertical relative position of the rendered tile. The position is expressed in pixels of the rendered bitmap, not pixels of screen. For instance, if OriginX and OriginY are set to 0 (the default value), the tile is drawn from upper-left corner. Setting OriginX to 4 and OriginY to 8 for 8x16 tile draws it centered. 

@dt MirrorHorizontal
@dd If set, the tile bitmap is flipped horizontally.

@dt MirrorVertical
@dd If set, the tile bitmap is flipped vertically.

@dlx
