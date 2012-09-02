set JAVA_HOME=C:\Program Files\Java\jdk1.6.0_31
call \lazarus\ant\bin\ant debug
copy bin\ZGEAndroid-debug.apk C:\Data\Delphi32\ZzDC_Public\tools\ZDesigner\exe\Android
copy libs\armeabi\libzgeandroid.so C:\Data\Delphi32\ZzDC_Public\tools\ZDesigner\exe\Android\Template\libs\armeabi