# Generate Application for Android {#ExportAndroid}

ZGameEditor allows to develop and deploy applications running on various Android devices, such as, mobile phones, tablets and game consoles (e.g., [OUYA (external link)](https://www.ouya.tv/)).

There are provided several methods how to compile the project, deploy it to the Android device and run it there. You can choose the one the best fits to your needs.

# Manual Generation and Deployment of zzdc.dat

If you want test your Android application which uses neither external libraries nor built-in resources and you do not want to install any additional tools on PC, use this method.

First, install the _ZGE Android_ application to your Android device. You can use any way of file transfer, e.g., Bluetooth file transfer, shared network disc, or Internet file sharing services as Google Drive or Dropbox, but the most straightforward is to use USB connection described in the following steps:

1. Connect your Android device to PC via USB cable. The type of USB connection should be ["Media device (MTP)" (external link)](http://www.mobile-phone-transfer.com/connect-android-to-pc-via-usb-mass-storage.html).

  ![](ht4-scr1.png)

2. Copy the `<ZGameEditor install dir>\Android\ZGEAndroid-debug.apk` file to the Android's internal storage or SD card mapped as a disk drive to your PC; for instance, as "\\Computer\<device name>\Internal storage" or "\\Computer\<device name>\SD Card".

3. Enable "Settings / Security / Unknown sources" on the Android device. 

  ![](ht4-scr2.png)

4. Install the `ZGEAndroid-debug.apk` by means of a file manager installed on your Android device. The application will appear as "ZGE Android".

  ![](ht4-scr3.png)


  The __ZGE Android__ is an Android application that interprets the compiled ZGameEditor project file `zzdc.dat` located in the root of the Android device's SD card. It cannot run without this file.
  
Once you have installed the _ZGE Android_ application, you do not need to reinstall it on each change of ZGameEditor project. Just follow the following steps how to compile, deploy and run the project on Android device:

1. Open your project in ZGameEditor.

2. Press "Project / Android: Build zzdc.dat file" menu item.

  ![](ht4-scr4.png)


  After compiling the project, ZGameEditor shows you a message with the path to the resulting `zzdc.dat` file.

3. Copy the `zzdc.dat` file to the SD card's root directory of your Android device. You can use USB connection again (or any other way, if you want).

4. Go to the Android device and run the _ZGE Android_ application.

See [this video tutorial (external link)](https://www.youtube.com/watch?v=96a5t-275%5Fk) how to create your first Android application with ZGameEditor from scratch. It demonstrates usage of Dropbox for transferring files between PC and Android device.

# One-Click Compiling, Deployment and Running

If you want to quickly test your Android application which uses neither external libraries nor built-in resources you can use the following method.

Prerequisite:

* To allow automatic deployment of files and running of Android applications from PC , you need to install the [Android SDK (external link)](http://developer.android.com/sdk/). You can install either the whole Android Studio IDE or stand-alone Android SDK Tools; Android SDK Tools are smaller and sufficient for development with ZGameEditor. Android SDK requires [Java SE Development Kit (external link)](http://www.oracle.com/technetwork/java/javase/downloads/index.html) to be installed on your machine.

First, prepare your Android device:

1. Connect the Android device to PC via USB cable. The type of USB connection should be ["Media device (MTP)" (external link)](http://www.mobile-phone-transfer.com/connect-android-to-pc-via-usb-mass-storage.html). 

2. Enable "USB debugging" in the [Developer options (external link)](http://www.androidcentral.com/all-about-your-phones-developer-options) on your Android device.

  ![](ht4-scr5.png)

Compile, deploy and run the application:

1. Open your project in ZGameEditor. 

2. Select the "Project / Android: Run project" menu item.

  ![](ht4-scr6.png)


  This command automatically installs the _ZGE Android_ application on the connected Android device, compiles the project to the `zzdc.dat` file, copies the file to the device's SD card root directory, and runs the _ZGE Android_ application on the compiled project file.

3. After the application is started on the Android device, you can manually close the opened console window and return to the editor.

4. Independently on running of the editor on PC, you can close or pause the running Android application.

If you run the _ZGE Android_ application again, it opens the last deployed ZGameEdior project, unless you manually removed the _zzdc.dat_ file from sdcard.

# Generating APK

To create distributable stand-alone Android application (.apk file) with embedded ZGameEditor engine, compiled project file, and possibly also local resource files, external libraries, application icons, and defines specific Android application access rights, use this method.

You can create either an unsigned __debug version__ used for testing and unofficial distribution, or signed __release version__ for official distribution on app markets, e.g., [Google Play (external link)](https://play.google.com) or [itch.io (external link)](https://itch.io/).

Prerequisites:

* To allow compiling ZGameEditor projects to Android application files (.apk), install [Android SDK (external link)](http://developer.android.com/sdk/). You can install either the whole Android Studio IDE or stand-alone Android SDK Tools; Android SDK Tools are smaller and sufficient for development with ZGameEditor. Android SDK requires [Java SE Development Kit (external link)](http://www.oracle.com/technetwork/java/javase/downloads/index.html) to be installed on your machine.

* Start _Android SDK Manager_ tool and install "Android 2.2 (API 8) / SDK Platform". To see this item in the Packages list, check the "Obsolete" check-box. If you want to use sensors (by means of the ZgeSensors external library) or controllers in your Android applications, you must install also "Android 4.1.2 (API 16) / SDK Platform".

  ![](ht4-scr7.png)

* You need to install and setup [Apache Ant (external link)](http://ant.apache.org/) for Windows. This is required for Android SDK building. See the [help page (external link)](http://ant.apache.org/manual/install.html) for details.

* Optionally, if you want to publish your applications, you need to sign them by your keystore key. The description how to generate and use the key can be found [here (external link)](http://developer.android.com/tools/publishing/app-signing.html).

* Specify the Android SDK installation path, Ant installation path and optionally also keystore file path and alias to the Android tab of the Settings dialog:

  ![Android settings](ht4-scr8.png)

  Without these settings you are not able to compile ZGameEditor projects to Android.

Build the application:

1. Open project in ZGameEditor.

2. Set the application properties. The @ref ZApplication component (the top-most component in project) defines several properties that determine properties of the generated Android application; namely:

  * @ref ZApplicationAndroidPackageName "AndroidPackageName"
  * @ref ZApplicationAndroidVersionName "AndroidVersionName"
  * @ref ZApplicationAndroidVersionNumber "AndroidVersionNumber"
  * @ref ZApplicationAndroidPortrait "AndroidPortrait"
  * @ref ZApplicationAndroidSdk "AndroidSdk"

  You can set these properties either before generating the Android application the first time, or during the next step.

3. Select "Project / Android: Build APK (debug)" or "Project / Android: Build APK (release)" menu item. In a consequent dialog you can verify and optionally also change the application's properties.

  ![](ht4-scr9.png)

  ZGameEditor generates the Android Application Project folder with name you specified in the @ref ZApplicationAndroidPackageName "ZApplication.AndroidPackageName" property. The folder structure is described [here (external link)](http://developer.android.com/tools/projects/index.html#ApplicationModules).
  
  Consequently, the building process is automatically started and the result application is placed to the `bin\<app-name>-debug.apk` file. The `<app-name>` is taken from the @ref ZApplicationCaption "ZApplication.Caption" property.

  When compiling the release version, the building script asks you for entering the keystore password. The final APK file will be `bin\<app-name>.apk`, without the `-debug` suffix.
  
4. You can optionally customize the generated application by:

  * Changing/adding application icon(s) for various screen density. The icons are placed in the  `res\drawable-<resolution>\icon.png` files. The following resolutions can be used (not necessarily all): 
    
    - `ldpi` - 36 x 36px icon for 120 dpi screen
    - `mdpi` - 48 x 48px icon for 160 dpi screen
    - `hdpi` - 72 x 72px icon for 240 dpi screen
    - `xhdpi` - 96 x 96px icon for 320 dpi screen
    - `xxhdpi` - 144 x 144px icon for 480 dpi screen
    - `xxxhdpi` - 192 x 192px icon for 640 dpi screen

  * Adding external shared libraries (.so files) into the `libs\<architecture>` directory. The architecture can be `armeabi`, `armeabi-v7a`, `armeabi-v7a-hard`, and/or `arm64-v8a` depending on the target CPU. Other than ARM architectures are not supported.

  * Adding asset files which are packed to the final APK and can be read from application code, see section @ref  AccessingFiles "Accessing Android Files" for details.
  
  * Modification of application properties in `AndroidManifest.xml`. You can modify, for instance, application permissions or supported screen resolutions. See the following snippet of the file:

        <uses-permission android:name="android.permission.INTERNET" />

        <supports-screens
        android:smallScreens="false"
        android:normalScreens="false"
        android:largeScreens="true"
        android:xlargeScreens="true" />

  For details about `AndroidManifest.xml` file see [here (external link)](http://developer.android.com/guide/topics/manifest/manifest-intro.html)

5. After changing the application properties, you need to recompile the application again (i.e., repeat step 2) to take effect in the final APK file.

There are several ways how to copy and install the generated APK file on your Android device, including, putting it to an application market and install it from there. We will describe usage of USB connection between PC and Android device:

1. Connect your Android device to PC via USB cable. The type of USB connection should be ["Media device (MTP)" (external link)](http://www.mobile-phone-transfer.com/connect-android-to-pc-via-usb-mass-storage.html). 

2. Copy the APK file to the Android's sdcard mapped as a disk drive to your PC; for instance, as "\\Computer\<device name>\Internal storage".

3. Enable "Settings / Security / Unknown sources" on the Android device. 

4. Install the APK by means of a file manager installed on your Android device.

5. After finished, you can run the installed Android application.

@anchor AccessingFiles
# Accessing Android Files

ZGameEditor application running on Android device can read files stored as resources in the APK file, and also files from SD card. To read files, in general, use the @ref File component. It's @ref FileFileName "FileFileName" property should be prefixed by:

* `/assets/` for asset files, or
* `/sdcard/` for file on SD card.

Example: Copy the default configuration file `config.txt` from assets to SD card, if it does not exist yet:

    // declare array
    byte[] buffer;

    // declare target file
    File target = @File(
                    FileName: "/sdcard/MyGame/config.txt",
                    TargetArray: buffer);

    // read the target file
    @FileAction(File: target);

    // does target file exist?
    if(target.Size == 0) {

      // read source
      @FileAction(File: @File(
                          FileName: "/assets/config.txt",
                          TargetArray: buffer));
      
      // write to target
      @FileAction(File: target, Action: 1);
      
    }

# Handling Android Device Inputs

## Touch Screen

If your application uses @ref KeyPress with the Key property set to "{" (left mouse button press) and detects position of pointer (touch) by @ref ZApplicationMousePosition "ZApplication.MousePosition.X/Y" properties, it will work also on Android. These properties identify the first screen touch.

However, you can detect multiple screen touches with the following functions: @ref touchGetCount, @ref touchGetID, @ref touchGetX, and @ref touchGetY.

Example: Obtaining positions of all touches in a script.

    vec2[8] touch;
    int TouchCount, j;

    ...

    TouchCount = touchGetCount();
    for(int i = 0; i < TouchCount; i++) {
      j = touchGetID(i);
      touch[i].X = touchGetX(j);
      touch[i].Y = touchGetY(j);
    }
    
## Application Focus

Android application can at any time loose focus, and if so, it should stop playing music, performing demanding calculations and rendering, intensive network communication, etc. After getting focus back, the application should resume all these activities. Identification and handling these events is, therefore, quite important.

In ZGameEditor you use @ref KeyPress component with specific CharCode property:

* 254 : Android application got focus
* 255 : Android application lost focus

## Back Button

If @ref ZApplicationEscapeToQuit "ZApplication.EscapeToQuit" is set to true, the Android "Back" button behaves like Escape key on Windows - terminates the application.

You can set this property to false and handle "Back" button by your code. To detect pressing of the "Back" button, use the @ref KeyPress component with CharCode set to 253.

## Controller

Since Android 4.1 (API level 16), it is possible to uniformly use gamepad controllers. ZGameEditor provides library the "Android Gamepad Library" which defines standard Android codes and OUYA-specific codes for identification of controller buttons and axes used in @ref joyGetButton and @ref joyGetAxis functions.

To allow this functionality, the @ref ZApplicationAndroidSdk "ZApplication.AndroidSdk" property must be set to "4.1 (API Level 16)".