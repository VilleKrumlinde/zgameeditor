# Import a 3D Model from a 3DS File {#Import3DS}

This "HOW TO" describes how to import a 3D model stored in a 3DS file into ZGameEditor to use in your own projects.

1. First step is to select "File / Import 3D-Model..." menu item.

  ![](ht1-scr1.png)

2. Then you select which file to import using a file browser.

  ![](ht1-scr2.png)
  
3. Next is the 3DS Import options dialog which looks like this:

  ![](ht1-scr3.png)

  The default options are normally the best for most models. Here is a more detailed description of what the options mean:

  * Name prefix: This is the name that the generated components will have in your project after the model is imported.

  * Auto scale model: Click this box to make the imported model scaled to the length of one in the X axis. This is useful because many 3DS files have very large dimensions and need to be scaled down.

  * Mesh scale: Select a scale value in percent. This option is only enabled if Auto Scale unchecked.

  * Import vertex colors: Click this box for importing the colors of the model on a per-vertex basis. If this box is not checked then ZGameEditor tries to pick one color for each mesh in the model based on the dominating material.

  * Auto center model: If this box is checked the model will be centered around the origin (zero) coordinate.

4. When you select "OK" in the 3DS import dialog the model is imported into your currently opened ZGameEditor project.

   ![](ht1-scr4.png)

The 3DS format is quite common and many 3D modeling tools can export 3D models in this format. Here are some sites that contain free 3DS models:

  * http://www.3dtotal.com/
  * http://www.3drt.com/downloads.htm
  * http://www.klicker.de/human.html

Limitations of the import routine:

  * Textures are currently not imported.

  * Many different 3DS files were tested to ensure correctness, but if you find a 3DS file that does not import, please send it to the [forum](http://www.emix8.org/forum) (external link).

Example: Here is a sample project in which two models are imported into a ZGameEditor project. The zip-file contains both the .zgeproj file and an separate EXE file that can be executed.

![](ht1-scr5.png)

Download here: [WolfAndBart.zip (external link; 100kb)](http://www.zgameeditor.org/files/zzdc/WolfAndBart.zip).