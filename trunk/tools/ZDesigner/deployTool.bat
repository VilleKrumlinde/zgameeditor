@echo off
setlocal

set tool=ZGameEditor

copy .\exe\ZDesigner.exe ..\..\Deploy\%tool%\%tool%.exe
copy .\exe\player.bin ..\..\Deploy\%tool%\
copy .\exe\player_ss.bin ..\..\Deploy\%tool%\
copy .\exe\player_linux.bin ..\..\Deploy\%tool%\
copy .\exe\player_osx86.bin ..\..\Deploy\%tool%\
copy .\exe\MidiInstruments.xml ..\..\Deploy\%tool%\
copy .\exe\Library.xml ..\..\Deploy\%tool%\
copy .\exe\zzdc.map ..\..\Deploy\%tool%\
copy .\exe\zgameeditor.chm ..\..\Deploy\%tool%\

copy .\exe\projects\About.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\Implicit.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\Particles.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\Steering.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\TripleE.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\ZPong.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\FileDemo\FileDemo.zgeproj ..\..\Deploy\%tool%\projects\FileDemo\
copy .\exe\projects\FileDemo\TestFile.txt ..\..\Deploy\%tool%\projects\FileDemo\
copy .\exe\projects\CleanseCube.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\ShaderDemo.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\FpsDemo\FpsDemo.zgeproj ..\..\Deploy\%tool%\projects\FpsDemo\
copy .\exe\projects\FpsDemo\FpsLevelLayout.txt ..\..\Deploy\%tool%\projects\FpsDemo\
copy .\exe\projects\ZBlast.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\RenderTexture.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\RenderPass.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\YakYakReader.zgeproj ..\..\Deploy\%tool%\projects\
copy .\exe\projects\GLES2Demo.zgeproj ..\..\Deploy\%tool%\projects\

copy .\exe\projects\ModPlay\ModPlay.zgeproj ..\..\Deploy\%tool%\projects\ModPlay\
copy .\exe\projects\ModPlay\bass.dll ..\..\Deploy\%tool%\projects\ModPlay\
copy .\exe\projects\ModPlay\4-mat_-_rose.xm ..\..\Deploy\%tool%\projects\ModPlay\

copy .\exe\lib\* ..\..\Deploy\%tool%\lib\
copy .\exe\Templates\* ..\..\Deploy\%tool%\Templates\
copy .\exe\Styles\readme.txt ..\..\Deploy\%tool%\Styles\
copy .\exe\Editors\* ..\..\Deploy\%tool%\Editors\

copy .\exe\Android\* ..\..\Deploy\%tool%\Android\

xcopy exe\Android\Template\*.* ..\..\Deploy\%tool%\Android\Template /S /D /Y

rem Remove any extra files
rem del ..\..\Deploy\%tool%\%tool%.ini
rem del ..\..\Deploy\%tool%\projects\*.exe
rem del ..\..\Deploy\%tool%\projects\*.scr

:exit
rem Remove local variables
endlocal

