"$sdkpath_normal$\platform-tools\adb" install -r "$apkpath$"
IF %ERRORLEVEL% NEQ 0 GOTO error
"$sdkpath_normal$\platform-tools\adb" shell am start -n $package$/org.zgameeditor.ZgeActivity
IF %ERRORLEVEL% NEQ 0 GOTO error
"$sdkpath_normal$\platform-tools\adb" logcat -s "ZgeAndroid"
IF %ERRORLEVEL% NEQ 0 GOTO error
goto end
:error
pause
:end