pushd "%~dp0"

if not exist "android\java\libs\arm64-v8a\" mkdir android\java\libs\arm64-v8a
if not exist "android\lib\arm64-linux\" mkdir android\lib\arm64-linux

PATH=C:\Android\android-ndk-r21e\toolchains\aarch64-linux-android-4.9\prebuilt\windows-x86_64\bin;

CD android
C:\FPC\trunk\bin\i386-win32\ppcrossa64 -B -MDelphi -Sghi -O3 -Tandroid -Paarch64 -XXis -vw -Filib\arm64-linux -FlC:\Android\android-ndk-r21e\platforms\android-29\arch-arm64\usr\lib -Fu. -Fu..\.. -FUlib\arm64-linux\ -FEjava\libs\arm64-v8a\ -olibzgeandroid.so -dANDROID -dMINIMAL -Xd -CpARMV8 -FuC:\FPC\trunk\units\aarch64-android\fcl-base -FuC:\FPC\trunk\units\aarch64-android\hash -FuC:\FPC\trunk\units\aarch64-android\rtl-objpas -FuC:\FPC\trunk\units\aarch64-android\paszlib zgeandroid.pas

popd
pause