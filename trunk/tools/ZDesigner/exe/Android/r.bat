%ANDROID_SDK_PATH%\platform-tools\adb push "%ZZDC_PATH%" /sdcard/
%ANDROID_SDK_PATH%\platform-tools\adb install -r "%APK_PATH%"
%ANDROID_SDK_PATH%\platform-tools\adb shell am start -n org.zgameeditor/org.zgameeditor.ZgeActivity
%ANDROID_SDK_PATH%\platform-tools\adb logcat -s "ZgeAndroid"

