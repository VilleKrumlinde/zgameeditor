pushd "%~dp0"

if not exist "android\java\libs\armeabi-v7a\" mkdir android\java\libs\armeabi-v7a
if not exist "android\lib\arm-linux\" mkdir android\lib\arm-linux

PATH=C:\Android\android-ndk-r21e\toolchains\arm-linux-androideabi-4.9\prebuilt\windows-x86_64\bin;

CD android
C:\FPC\trunk\bin\i386-win32\ppcrossarm -B -MDelphi -Sghi -O3 -Tandroid -Parm -XXis -vw -Filib\arm-linux -FlC:\Android\android-ndk-r21e\platforms\android-29\arch-arm\usr\lib -Fu. -Fu..\.. -FUlib\arm-linux\ -FEjava\libs\armeabi-v7a\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV6 -CfVFPv2 -FuC:\FPC\trunk\units\arm-android\fcl-base -FuC:\FPC\trunk\units\arm-android\hash -FuC:\FPC\trunk\units\arm-android\rtl-objpas -FuC:\FPC\trunk\units\arm-android\paszlib zgeandroid.pas

popd
pause