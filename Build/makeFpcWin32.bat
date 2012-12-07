@echo off
rem -B   Build all
rem -Sd  Delphi extensions
rem -Os  Generate smaller code
rem -dZZDC_SDL SDL eller ZZDC_WIN32
rem -al  Keep assembler-files
rem -gl  Show line-nr in stack trace (debuginfo)

rem FPC 2.0 krashar zblast i G-läge
cd ..
C:\lazarus\fpc\2.6.1\bin\i386-win32\fpc -al -XXis -O2 -dMINIMAL -FU.\build\obj\ -B -Mdelphi -FE.\build\ zzdc.dpr
cd build

