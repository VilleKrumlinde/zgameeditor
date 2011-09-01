@echo off
setlocal

rem no param=build normal, SS=build screensaver
rem -GD = detailed map file

rem Delphi XE2
set dcc="C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\dcc32.exe"
set commonparams=-B -N.\build\obj\ -E.\tools\zdesigner -U.\rtl\DXE2\lib ZzDC.dpr

rem Delphi 2010
rem set dcc="C:\Program Files (x86)\Embarcadero\RAD Studio\7.0\bin\dcc32.exe"
rem set commonparams=-B -N.\build\obj\ -E.\tools\zdesigner -U.\rtl\D2010\lib ZzDC.dpr

rem Delphi 2007
rem set dcc="C:\Program Files\CodeGear\RAD Studio\5.0\bin\dcc32.exe"
rem set commonparams=-B -N.\build\obj\ -E.\tools\zdesigner -U.\rtl\D2007\lib ZzDC.dpr

rem Delphi 6
rem set dcc="C:\Program Files\Borland\Delphi6\Bin\dcc32"
rem set commonparams=-B -N.\build\obj\ -E.\tools\zdesigner -U.\rtl\D6 ZzDC.dpr

del zzdc.exe
cd ..\..

rem -DZDEBUG to make debug player (large)

if %1.==. (
  rem Normal
  %dcc% -DMINIMAL %commonparams% -GD
  set playername=Player.bin
) else if %1.==SS. (
  rem Screensaver
  %dcc% -DMINIMAL -Dzzdc_screensaver %commonparams%
  set playername=Player_SS.bin
) else (
  echo Error
  goto :exit
)

cd tools\zdesigner
dir .\exe\%playername%
del .\exe\%playername%
copy zzdc.exe .\exe\%playername%

if %1.==. (
  copy zzdc.map .\exe\
)

dir .\exe\%playername%

:exit
rem Remove local variables
endlocal

