# BitmapFromFile {#BitmapFromFile}

Import an external bitmap to your application.

This component can only be used in the @ref BitmapProducers "Bitmap.Producers" property.

See also: @ref Bitmap

## Properties

@dl

@dt BitmapFile
@dd The content of the imported bitmap. Click the "Import..." button that is located beside the property in the editor to browse for a bitmap file. When a bitmap is selected it is imported to your application and is no longer referenced on disk.

The size of the Bitmap is set to the size of imported bitmap after importing.

@note Importing bitmaps will increase your exe-file size very quickly, so be careful if you want to keep your program small. Upx compression works similar to what is used in the GIF file format, so if your image is small when saved as a GIF-file then it will also have good compression ratio in ZGE as well. Bitmaps from JPG files have very poor compression in Upx so try first to preprocess the image in an art-package like Photoshop for reducing the number of colors used.

@dt Transparency
@dd Controls bitmap is transparency.

* None - Bitmap do not have any transparent areas.
* BlackColor - All black (RGB 0) areas of the bitmap will be transparent.
* AlphaLayer - Use the bitmaps alpha layer for transparency. Alpha is imported from 32bit BMP or PNG files.

@dlx
