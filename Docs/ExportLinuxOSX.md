# Generate Executable for Linux and OS X {#ExportLinuxOSX}

This "HOW TO" describes how to make your ZGameEditor projects run under Linux and Mac OS X (Intel x86).

1. Start ZGameEditor.

2. Open the project file (*.zgeproj).

3. Choose to generate a binary for the target platform:

  ![&nbsp;](ht3-scr1.png)

4. Copy the generated binary to the target platform.

Platform requirements:

  * OpenGL must be installed, and
  * [SDL (external link)](http://www.libsdl.org/) must be installed.

Also note that on Linux you must check 2 things:

  * The file must be executable. You can set the file executable from a shell terminal with the command "`chmod 755 your_file_name`", or from a file manager application. 

  * You might have some other libs like "libGL.so.1" or "`libGL.so.1.2`" installed in your "`usr/lib`" folder, because the engine looks for a lib called "`libGL.so`". If this is the case, you can create a system link using the command "`ln -s usr/lib/libGL.so.1 usr/lib/libGL.so`". Root privileges are needed.

Current limitations:

  * Built-in font is not supported, use bitmap fonts instead.

  * Binaries are quite large and cannot be compressed with [UPX (external link)](http://upx.sourceforge.net).

Example screenshots:

![WolfAndBart application running on Ubuntu Linux](ht3-scr2.png)
<br>
![ShaderDemo application running on Mac OS X](ht3-scr3.png)
