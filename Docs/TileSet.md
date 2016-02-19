# TileSet {#TileSet}

Defines a tile set. Tile set is a collection of reusable tiles, i.e., smaller images representing basic building blocks for drawing 2D background maps. Drawing tile-based maps is performed by means of the @ref RenderTile component. All tiles in one set have the same size and form a grid.

See also: @ref RenderTile

External links: [Discussion about tiles and sprites on the Forum](http://www.emix8.org/forum/viewtopic.php?f=1&t=1159)

## Properties

@dl

@dt Bitmap
@dd Set to a @ref Bitmap that holds the tiles graphics.

@dt TileWidth, TileHeight
@dd The pixel width and height of one tile.

@dt TileBorder
@dd If tiles have extra pixels below and to the right then set this property to the number of extra pixels.

@dlx
