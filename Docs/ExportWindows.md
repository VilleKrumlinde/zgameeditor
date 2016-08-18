# Generate Executable for Windows {#ExportWindows}

This "HOW TO" describes how to make Windows executable and/or screensaver file from ZGameEditor project.

## Generate Executable

1. Start ZGameEditor.

2. Open the project file (*.zgeproj).

3. If you want to create just uncompressed EXE file, press the "Run" button in the toolbar or press F9 key.

4. There are also other options, select one of the following menu items:

  ![&nbsp;](ht2-scr1.png)

  * "Project / Build and compress Windows exe-file" to generate a compressed EXE file. Use this option for producing releases of your applications.
  
  * "Project / Build Screensaver" to generate uncompressed version of Windows screen saver; the *.scr file.
  
  * "Project / Build and compress Screensaver" to generate compressed version of Windows screen saver; the *.scr file. Use this option for producing releases of your screensaver.

The generated executable (EXE or SCR) file is placed in the same directory as the project file.
  
_Note: It is recommended to check the "Project / Remove unused code" menu item which can reduce size of the generated file._

## Compression Settings

To reduce the size of the generated Windows executable file, you can apply UPX or kkrunchy compression to the generated EXE/SCR file. The final executable is automatically uncompressed and loaded to memory when executed.

_Note: To use kkrunchy, you should first manually copy the kkrunchy.exe file to the &lt;ZGameEditor installdir&gt;/Tools directory. You can download it from [kkrunchy homepage (external link)](http://www.farbrausch.de/~fg/kkrunchy/). UPX executable is included in the ZGameEditor's distribution._

To customize compression options, use the "Packer settings" section of the Settings dialog accessible from the "Tools / Settings..." menu item:

![Settings dialog](ht2-scr2.png)

You can select from several presets, or you can also specify your own command line options. See the manual pages for [UPX (external link)](https://github.com/korczis/upx/blob/master/doc/upx.pod) or [kkrunchy (external link)](http://www.farbrausch.de/~fg/kkrunchy/) for details.

## Setting Application Properties

The @ref ZApplication component (the top-most component in project) defines several properties that you can use to set appearance and behaviour of the generated executable; namely:

* @ref ZApplicationCaption "Caption"
* @ref ZApplicationFullScreen "FullScreen"
* @ref ZApplicationScreenMode "ScreenMode"
* @ref ZApplicationShowOptionsDialog "ShowOptionsDialog"
* @ref ZApplicationCustomScreenWidthHeight "CustomScreenWidth, CustonScreenHeight"
* @ref ZApplicationViewportRatio "ViewportRatio"
* @ref ZApplicationCustomViewportRatio "CustomViewportRatio"
* @ref ZApplicationMouseVisible "MouseVisible"
* @ref ZApplicationEscapeToQuit "EscapeToQuit"
* @ref ZApplicationIcon "Icon"

@anchor CommandLineSwitches
## Command Line Switches

Generated Windows executable file recognizes the following command line switches:

Option | Semantics
-------|----------
-f | force fullscreen mode
-w | force windowed mode
-s | disable sound

_Note: Do not use "-f", "-w", and "-s" as part of the project file name (and therefore also the generated executable file) or part of the directory path to the executable file, because ZGameEditor would interpret it as a command line switch. For instance, "u-f-o-s.exe" is not a good file name for a windowed application with sounds._
