@echo off
pushd "%~dp0"

REM Define the FPC path and version
set FPC_PATH=C:\FPC\3.2.2\bin\i386-win32

REM Define the Android NDK path and API number
set NDK_PATH=C:\Android\android-ndk-r21e\platforms\android-29

REM SDK and Ant used to package ZGEAndroid-debug.apk
set SDK_PATH=C:\Android\android-sdk-windows
set ANT_PATH=C:\Android\apache-ant-1.10.15

REM dx.bat passes -Djava.ext.dirs, which Java 9 and later reject
set "JAVA8_HOME=C:\Program Files\Java\jdk1.8.0_281"

REM Enable or disable debug mode (set to true or false)
set DEBUG=false

REM Set flags based on the DEBUG variable
if "%DEBUG%"=="true" (
    set DEBUG_FLAG=-g -O-
    ECHO Warning: debug mode is activated!
    ECHO.
) else (
    set DEBUG_FLAG=-O3
)

cd android

REM Create libzgeandroid.so for Android 32 (armeabi-v7a)

if not exist "java\libs\armeabi-v7a\" mkdir java\libs\armeabi-v7a
if not exist "obj\armeabi-v7a\" mkdir obj\armeabi-v7a

call :title "Building libzgeandroid.so for Android 32 (armeabi-v7a)..."
call %FPC_PATH%\ppcrossarm -B -MDelphi -Sghi -Tandroid -Parm -XXis -vw -Filib\armeabi-v7a -Fl%NDK_PATH%\arch-arm\usr\lib -Fu. -Fu..\.. -FUobj\armeabi-v7a\ -FEjava\libs\armeabi-v7a\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV7A -CfVFPv3 %DEBUG_FLAG% zgeandroid.pas
ECHO.

REM Check for errors after 32-bit build

if errorlevel 1 (
    ECHO "ERROR: Failed to build libzgeandroid.so for Android 32 (armeabi-v7a)"
    goto end
)

REM Create libzgeandroid.so for Android 64 (arm64-v8a)

if not exist "java\libs\arm64-v8a\" mkdir java\libs\arm64-v8a
if not exist "obj\arm64-v8a\" mkdir obj\arm64-v8a

call :title "Building libzgeandroid.so for Android 64 (arm64-v8a)..."
call %FPC_PATH%\ppcrossa64 -B -MDelphi -Sghi -Tandroid -Paarch64 -XXis -vw -Filib\arm64-v8a -Fl%NDK_PATH%\arch-arm64\usr\lib -Fu. -Fu..\.. -FUobj\arm64-v8a\ -FEjava\libs\arm64-v8a\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV8 %DEBUG_FLAG% zgeandroid.pas
ECHO.

REM Check for errors after 64-bit build

if errorlevel 1 (
    ECHO "ERROR: Failed to build libzgeandroid.so for Android 64 (arm64-v8a)"
    goto end
)

REM Install both libraries into the ZDesigner export template

set "TEMPLATE=..\..\tools\ZDesigner\exe\Android\Template\libs"
if not exist "%TEMPLATE%\armeabi-v7a" mkdir "%TEMPLATE%\armeabi-v7a"
if not exist "%TEMPLATE%\arm64-v8a" mkdir "%TEMPLATE%\arm64-v8a"
copy /Y "java\libs\armeabi-v7a\libzgeandroid.so" "%TEMPLATE%\armeabi-v7a\"
copy /Y "java\libs\arm64-v8a\libzgeandroid.so" "%TEMPLATE%\arm64-v8a\"

REM Package the generic player. It has no zzdc.dat and loads /sdcard/zzdc.dat.

set "SDK_URL=%SDK_PATH:\=/%"
> "java\local.properties" echo sdk.dir=%SDK_URL%

set "PLAYER=%~dp0..\tools\ZDesigner\exe\Android"
if not exist "%PLAYER%" mkdir "%PLAYER%"

if not exist "%JAVA8_HOME%\bin\java.exe" (
    echo ERROR: JDK 8 not found at %JAVA8_HOME%
    echo dx.bat cannot run on Java 9 or later.
    goto end
)

REM JDK 9+ rewrites debug.keystore as PKCS12. JDK 8 signs only JKS.
set "DEBUG_KEYSTORE=%USERPROFILE%\.android\debug.keystore"
if exist "%DEBUG_KEYSTORE%" (
    "%JAVA8_HOME%\bin\keytool.exe" -list -keystore "%DEBUG_KEYSTORE%" -storepass android >nul 2>&1
    if errorlevel 1 (
        echo Debug keystore is not readable by JDK 8. Converting it to JKS...
        keytool -importkeystore -noprompt -srckeystore "%DEBUG_KEYSTORE%" -srcstoretype PKCS12 -srcstorepass android -destkeystore "%DEBUG_KEYSTORE%.jks" -deststoretype JKS -deststorepass android
        if errorlevel 1 (
            echo Conversion failed. Deleting debug.keystore so the SDK can recreate it.
            del "%DEBUG_KEYSTORE%"
        ) else (
            move /Y "%DEBUG_KEYSTORE%.jks" "%DEBUG_KEYSTORE%" >nul
        )
    )
)

set "JAVA_HOME=%JAVA8_HOME%"
set "PATH=%JAVA_HOME%\bin;%PATH%"

pushd java
call :title "Building ZGEAndroid-debug.apk..."
call "%ANT_PATH%\bin\ant.bat" debug
if errorlevel 1 (
    echo ERROR: Failed to build ZGEAndroid-debug.apk
    popd
    goto end
)
copy /Y "bin\ZGEAndroid-debug.apk" "%PLAYER%\"
popd

:end
popd
ECHO.
pause
exit /b

:title
ECHO.
ECHO ======================================================================
ECHO  %~1
ECHO ======================================================================
ECHO.
exit /b
