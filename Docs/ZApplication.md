# ZApplication {#ZApplication}

The application top-most component. This component is automatically created when you start a new project.

## Properties

@dl

@dt @anchor ZApplicationCaption Caption
@dd A text string that is displayed in the window caption of your application.

@dt @anchor ZApplicationDeltaTime DeltaTime
@dd Read only. The time in seconds that has passed since last frame was rendered. Use this value when updating model positions in expressions to make your game frame-rate independent. Example of expression using the DeltaTime property:

    CurrentModel.Position.X+=1*App.DeltaTime;

@dt Time
@dd Read only. Time in seconds since application was started.

@dt @anchor ZApplicationGLBase GLBase
@dd Set this to the desired OpenGL API version used for rendering objects. It can be:

* Compatible - default rendering is used for visual components. This will work on most of OpenGL GPUs.
* ES2/GL3 - render model which conforms with GLES2 (OpenGL ES 2) and GL3 (OpenGL 3). Using this option, everything must be rendered using @ref Shader components. Use it, for instance, if you want to use custom shaders in Android application.

@dt FpsCounter
@dd Read only. The current number of frames per second. This value can be displayed on screen using the @ref  RenderText component and is useful for detecting performance issues.

@dt CurrentRenderPass
@dd Read only. Can be used to define different behaviour on multiple render passes.

@dt @anchor ZApplicationMousePosition MousePosition
@dd Read only. X and Y coordinates of the current mouse position, in -1 to 1 range.

@dt MouseWheelDelta
@dd Read only. Returns integer value 1 or -1 depending on wheel direction.

@dt ClearScreenMode
@dd Use it to control clearing of screen before each drawing cycle. It can be:

* ClearScreen - the screen is cleared before every frame update
* NotClearScreen - the screen is not cleared. Use this for feedback effects. See the @ref Particles sample project for how to use this property.

@dt @anchor ZApplicationRenderPasses RenderPasses
@dd Defines the number of passes. The default is 1

@dt WindowHandle
@dd Read only. This is the handle to the main window. Use this as a parameter when you need to call Win32-API using a external library.

@dt ClearColor
@dd The color that the screen will be cleared with. Default black.

@dt AmbientLightColor
@dd Global ambient light value. Default is (0.4, 0.4, 0.4).

@dt @anchor ZApplicationFullScreen FullScreen
@dd Set this property to make your application run in fullscreen mode. You can override this property with @ref CommandLineSwitches "command line switches" (-w or -f).

@dt FrameRateStyle
@dd Set to one of preset values:

* SyncedWithMonitor - (default) the frame rate will be limited to the monitors refresh rate. Defaults to 60 if the rate can't be determined.
* Free - the frame rate will be as high as possible, this will use maximum CPU.
* Fixed - the frame rate will attempt to match the value set in the FixedFrameRate property.

@dt FixedFrameRate
@dd Controls the desired framerate when FrameRateStyle is set to Fixed.

@dt @anchor ZApplicationScreenMode ScreenMode
@dd Select the screen resolution for your application.

@dt @anchor ZApplicationShowOptionsDialog ShowOptionsDialog
@dd Set this property to display a dialog box where the user can choose screen resolution and fullscreen option on startup of your game. This features currently only works on Windows and Linux.

@dt @anchor ZApplicationCustomScreenWidthHeight CustomScreenWidth, CustomScreenHeight
@dd If these properties contains other values than zero, this screensize is used instead of the ScreenMode property. Use this for setting custom windowed modes such as a vertical shooting game.

@dt CameraPosition
@dd The current position of the camera. Animate this property to make scrolling games. Example ZExpression "App.CameraPosition.X=App.Time * 0.5;" moves the screen slowly to the left.

@dt CameraRotation
@dd The current rotation of the camera in X,Y and Z axis.

@dt @anchor ZApplicationCamera Camera
@dd Instead of the default camera, you can select the @ref Camera component for rendering. If so, the CameraPosition, CameraRotation and FOV properties of ZAplication will be ignored.

@dt @anchor ZApplicationLightPosition LightPosition
@dd The position or direction of the OpenGL light. The default OpenGL light is directional down the Z-axis.

@dt @anchor ZApplicationViewportRatio ViewportRatio
@dd Set to one of preset values:

* Full Window - This is the default. The whole screen area is used for rendering. This means that depending on the ratio of the screen resolution the bounds of the visible area of your game world will be slightly different which may not always be desirable.
* Custom - Use the value of the CustomViewportRatio.
* 4:3 - Use standard (non-widescreen) viewport ratio.
* 16:9 - Use widescreen ratio.

@dt @anchor ZApplicationCustomViewportRatio CustomViewportRatio
@dd Set a value here to use a custom viewport-ratio.

@dt FOV
@dd Camera Field Of View. Default value is 45 degrees.

@dt ClipNear
@dd The near clipping distance of OpenGL graphics. Objects that are closer to the camera than this value will not be drawn.

@dt ClipFar
@dd The far clipping distance of OpenGL graphics. Objects that are further away from the camera than this value will not be drawn.

@dt @anchor ZApplicationMouseVisible MouseVisible
@dd Set this to keep the mouse cursor visible.

@dt @anchor ZApplicationEscapeToQuit EscapeToQuit
@dd Uncheck this to disable user pressing Escape key to quit your application. On Android, the Back menu button is used to quit the application.

@dt ViewportX, ViewportY
@dd Read only. X and Y position of viewport in application window.

@dt ViewportWidth, ViewportHeight
@dd Read only. Width and height of viewport.

@dt ScreenWidth, ScreenHeight
@dd Read only. Width and height of application window.

@dt @anchor ZApplicationIcon Icon
@dd Load a ICO-file to this property for customizing the application icon of the generated EXE file. Note: The icon is only inserted in compressed Windows EXE files, not for other platforms.

@dt PreviewClearColor
@dd Choose which color to use as a background in the preview-window of the designer. Available in designer only.

@dt @anchor ZApplicationAndroidPackageName AndroidPackageName
@dd A unique name of Android application placed to AndroidManifest.xml file as `manifest.package` attribute. See [Android developer's page](http://developer.android.com/guide/topics/manifest/manifest-element.html#package) for details.

@dt @anchor ZApplicationAndroidVersionName AndroidVersionName
@dd Version of Android application placed to AndroidManifest.xml file as `manifest.android:versionName` attribute. See [Android developer's page](http://developer.android.com/guide/topics/manifest/manifest-element.html#vname) for details.

@dt @anchor ZApplicationAndroidVersionNumber AndroidVersionNumber
@dd Internal unique version number of Android application placed to AndroidManifest.xml file as `manifest.android:versionCode` attribute. See [Android developer's page](http://developer.android.com/guide/topics/manifest/manifest-element.html#vcode)  for details.

@dt @anchor ZApplicationAndroidPortrait AndroidPortrait
@dd If checked, the application generated for Android will have "portrait" screen orientation. By default, the property is unchecked and the screen orientation is "landscape".

@dt @anchor ZApplicationAndroidSdk AndroidSdk
@dd The minimal version of Android SDK (OS) version the application is able to run on. It can be:

* Android 2.2 (API Level 8) for better compatibility also on older devices, or
* Android 4.1 (API Level 16) for application that uses game controllers (joysticks), sensors, and other features provided by libraries or external libraries for Android.

This property is reflected by AndroidManifest.xml file as `<uses-sdk android:minSdkVersion="8" />` tag. See [Android developer's page](http://developer.android.com/guide/topics/manifest/uses-sdk-element.html#min) for details.

@dlx

## List Properties

@dl

@dt @anchor ZApplicationOnLoaded OnLoaded
@dd This list is executed once at every program start. Put your application initialization components here.

@dt States
@dd A list of @ref AppState components that the application uses.

@dt OnUpdate
@dd Called once between every frame update.

@dt OnBeginRenderPass
@dd This is called before each render pass.

@dt OnRender
@dd A list of render commands.

@dt OnClose
@dd This is called when your application exits.

@dt @anchor ZApplicationLights Lights
@dd A list of Light components that the application uses.

@dt Content
@dd The application content list. Put your game components here.

@dlx
