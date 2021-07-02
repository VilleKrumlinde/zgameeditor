# SpriteSheet {#SpriteSheet}

Defines a sprite sheet. Sprite sheet is a collection of smaller rectangular
images representing different phases of animated sprites. Sprite sheet is given
by underlying @ref Bitmap and a set of sprite data. Each sprite is defined by:

- index which is used to refer the sprite during its drawing in @ref
  RenderSprite component,
- X and Y position in pixels determining its placement in the sprite sheet
  bitmap,
- width and height of the sprite in pixels,
- origin X and Y position used for sprite horizontal and vertical flipping; see
  the @ref RenderSprite component.

Sprite data are edited visually in SpriteSheet editor.

![Sprite sheet editor](comp-sprite-editor.png)

The following functionality is available:

| Key / Button                          | Action                             |
| ------------------------------------- | ---------------------------------- |
| mouse wheel or MMB drag/drop          | Zoom in/out bitmap                 |
| RMB drag/drop                         | Move bitmap                        |
| LMB click                             | Select sprite                      |
| LMB drag/drop of selected point       | Resize sprite or move origin point |
| ![ ](comp-sprite-delete-button.png)   | Delete selected sprite             |
| ![ ](comp-sprite-center-button.png)   | Center sprite origin               |
| ![ ](comp-sprite-forward-button.png)  | Bring sprite forward               |
| ![ ](comp-sprite-backward-button.png) | Bring sprite backward              |
| ![ ](comp-sprite-alpha-button.png)    | Toggle alpha view                  |
| ![ ](comp-sprite-bright-button.png)   | Enable bright grid                 |
| ![ ](comp-sprite-dark-button.png)     | Enable dark grid                   |

@note If sprites are of the same size, with the same origin and placed in a
grid, it is more optimal to define them by @ref TileSet and render them by @ref
RenderTile.

See also: @ref RenderSprite

External links:
[Discussion about tiles and sprites on the Forum](http://www.emix8.org/forum/viewtopic.php?f=1&t=1159)

## Properties

@dl

@dt Bitmap @dd Set to a @ref Bitmap that holds the sprites graphics.

@dt SpriteData (WORK IN PROGRESS) @dd Definition of sprites. The "Clear" button
removes all sprite definitions. The "Import" button imports sprite data from
external source.

@dlx
