pushd "%~dp0"

rem C:\lazarus\fpc\2.6.1\bin\i386-win32\ppcrossarm -B -MDelphi -Sghi -O3 -Tlinux -Parm -XXis -vw -Filib\arm-linux -FlC:\android\ndk\platforms\android-8\arch-arm\usr\lib -Fu. -Fu..\.. -FUlib\arm-linux\ -FEjava\libs\armeabi\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV6 -CfVFPv2 zgeandroid.pas

C:\fpc\bin\i386-win32\ppcrossarm -B -MDelphi -Sghi -O3 -Tandroid -Parm -XXis -vw -Filib\arm-linux -FlC:\android\ndk\platforms\android-8\arch-arm\usr\lib -Fu. -Fu..\.. -FUlib\arm-linux\ -FEjava\libs\armeabi\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV6 -CfVFPv2 zgeandroid.pas


IF %ERRORLEVEL% NEQ 0 GOTO error
cd java
call m.bat
cd ..
:error

popd