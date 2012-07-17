$sdkpath_normal$\platform-tools\adb install -r "$apkpath$"
$sdkpath_normal$\platform-tools\adb shell am start -n $package$/org.zgameeditor.ZgeActivity
$sdkpath_normal$\platform-tools\adb logcat -s "ZgeAndroid"
