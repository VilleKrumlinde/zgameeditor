"%ANDROID_SDK_PATH%\platform-tools\adb" push "%ZZDC_PATH%" /sdcard/
IF %ERRORLEVEL% NEQ 0 GOTO error
"%ANDROID_SDK_PATH%\platform-tools\adb" install -r "%APK_PATH%"
IF %ERRORLEVEL% NEQ 0 GOTO error
"%ANDROID_SDK_PATH%\platform-tools\adb" shell am start -n org.zgameeditor/org.zgameeditor.ZgeActivity
IF %ERRORLEVEL% NEQ 0 GOTO error
"%ANDROID_SDK_PATH%\platform-tools\adb" logcat -s "ZgeAndroid"
IF %ERRORLEVEL% NEQ 0 GOTO error
goto end
:error
pause
:end
