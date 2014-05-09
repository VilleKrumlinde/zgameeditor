@echo off
setlocal

rem no param=build normal, SS=build screensaver
rem -GD = detailed map file

set zgeproduct=NORMAL
FOR %%A IN (%*) DO (
  if "%%A"=="64" set BIT=64
  if "%%A"=="SS" set zgeproduct=SS
  if "%%A"=="OSX" set zgeproduct=OSX
)

rem Delphi XE5
if "%BIT%"=="64" (
  set dcc="C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\dcc64.exe"
  set commonparams=-B -N.\build\obj\ -E.\tools\zdesigner ZzDC.dpr
) else (
  set dccosx="C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\dccosx.exe"
  set dcc="C:\Program Files (x86)\Embarcadero\RAD Studio\12.0\bin\dcc32.exe"
  set commonparams=-$J+ -$I- --no-config -B -N.\build\obj\ -E.\tools\zdesigner -U.\rtl\DXE5\lib ZzDC.dpr
)

rem Delphi XE2
rem if "%BIT%"=="64" (
rem   set dcc="C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\dcc64.exe"
rem   set commonparams=-B -N.\build\obj\ -E.\tools\zdesigner ZzDC.dpr
rem ) else (
rem  set dcc="C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\dcc32.exe"
rem  set commonparams=-$J+ -$I- --no-config -B -N.\build\obj\ -E.\tools\zdesigner -U.\rtl\DXE2\lib ZzDC.dpr
rem )

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

if %zgeproduct%==NORMAL (
  rem Normal
  %dcc% -DMINIMAL %commonparams% -GD
  set playername=Player.bin
) else if %zgeproduct%==SS (
  rem Screensaver
  %dcc% -DMINIMAL -Dzzdc_screensaver %commonparams%
  set playername=Player_SS.bin
) else if %zgeproduct%==OSX (
  %dccosx% -DZZDC_SDL;DARWIN;UNIX;MINIMAL -$J+ -$I- -B -N.\build\obj\ -E.\tools\zdesigner ZzDC.dpr
  set playername=player_osx86.bin
) else (
  echo Error
  goto :exit
)

cd tools\zdesigner
dir .\exe\%playername%
del .\exe\%playername%

if %zgeproduct%==OSX (
  copy zzdc .\exe\%playername%
) else (
  copy zzdc.exe .\exe\%playername%
)

if %1.==. (
  copy zzdc.map .\exe\
)

dir .\exe\%playername%

:exit
rem Remove local variables
endlocal

