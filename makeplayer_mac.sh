fpc -Xm -XXis -O2 -FU/tmp -dMINIMAL -B -Mdelphi -FE./tools/ZDesigner -gw -gl -godwarfsets -k"-framework OpenGL -framework Cocoa -framework QuartzCore" zzdc.dpr
cp ./tools/ZDesigner/zzdc ./tools/ZDesigner/ZGameEditor.app/Contents/MacOS/player_macos.bin
