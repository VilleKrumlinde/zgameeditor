# SDL2-for-Pascal

Unit files for building
[Free Pascal](https://freepascal.org/) / [Delphi](https://www.embarcadero.com/products/delphi) applications
using the [SDL2 library](https://libsdl.org).

This repository is a community-maintained fork of the [Pascal-SDL-2-Headers](https://github.com/ev1313/Pascal-SDL-2-Headers) repo.

## Installation

Simply add the units to your include path. You can achieve this by:
 - (FPC) using the `{$UNITPATH XXX}` directive in your source code;
 - (FPC) using the `-FuXXX` command-line argument to the compiler;
 - just copying & pasting the units into the same directory as your main source code.

Use the `sdl2` unit for the main SDL2 library (should be always needed). Units for the other SDL2 libraries are also provided:
 - [`sdl2_gfx`](https://www.ferzkopp.net/wordpress/2016/01/02/sdl_gfx-sdl2_gfx/)
 - [`sdl2_image`](https://www.libsdl.org/projects/SDL_image/)
 - [`sdl2_mixer`](https://www.libsdl.org/projects/SDL_mixer/)
 - [`sdl2_net`](https://www.libsdl.org/projects/SDL_net/)
 - [`sdl2_ttf`](https://www.libsdl.org/projects/SDL_ttf/)

## Bugs / Contributions

If you have any contributions, feel free to drop a pull request or send in a patch.

Same goes for bugs, please use the GitHub issue tracker.

## License

You may license the Pascal SDL2 units either
with the [MPL license](blob/master/MPL-LICENSE) or
with the [zlib license](blob/master/zlib-LICENSE).
