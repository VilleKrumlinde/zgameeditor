@echo off

pushd "%~dp0"

REM Define the FPC path and version
set FPC_PATH=C:\FPC\3.2.2\bin\i386-win32

REM Define the Android NDK path and API number
set NDK_PATH=C:\Android\android-ndk-r27b\toolchains

REM Enable or disable debug mode (set to true or false)
set DEBUG=false

REM Set DEBUG_FLAG based on the DEBUG variable
if "%DEBUG%"=="true" (
    set DEBUG_FLAG=-g
    ECHO Warning: debug mode is activated!
    ECHO.
) else (
    set DEBUG_FLAG=
)

cd android

REM Create libzgeandroid.so for Android 32 (armeabi-v7a)

if not exist "java\libs\armeabi-v7a\" mkdir java\libs\armeabi-v7a
if not exist "obj\armeabi-v7a\" mkdir obj\armeabi-v7a

ECHO Building libzgeandroid.so for Android 32 (armeabi-v7a)...
ECHO.
%FPC_PATH%\ppcrossarm -B -MDelphi -Sghi -O3 -Tandroid -Parm -XXis -vw -Filib\armeabi-v7a -Fl%NDK_PATH%\arch-arm\usr\lib -Fu. -Fu..\.. -FUobj\armeabi-v7a\ -FEjava\libs\armeabi-v7a\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV6 -CfVFPv2 %DEBUG_FLAG% zgeandroid.pas
ECHO.

REM Create libzgeandroid.so for Android 64 (arm64-v8a)

if not exist "java\libs\arm64-v8a\" mkdir java\libs\arm64-v8a
if not exist "obj\arm64-v8a\" mkdir obj\arm64-v8a

ECHO Building libzgeandroid.so for Android 64 (arm64-v8a)...
ECHO.
%FPC_PATH%\ppcrossa64 -B -MDelphi -Sghi -O3 -Tandroid -Paarch64 -XXis -vw -Filib\arm64-v8a -Fl%NDK_PATH%\arch-arm64\usr\lib -Fu. -Fu..\.. -FUobj\arm64-v8a\ -FEjava\libs\arm64-v8a\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV8 %DEBUG_FLAG% zgeandroid.pas
ECHO.

popd
pause
