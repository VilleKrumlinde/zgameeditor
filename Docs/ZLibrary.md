# ZLibrary {#ZLibrary}

A library of global functions, constants and variables (including arrays).

## Properties

@dl

@dt Source
@dd Type your functions, constants and global variables here. They can be accessed/called from other expressions in your project. The functions/constants/variables must be defined before they are accessed/called, so insert the ZLibrary component somewhere at the top of your project, in @ref ZApplicationOnLoaded "ZApplication.OnLoaded" for instance.

Here is an example:

    const float PI2 = PI * 2.0;

    int IsRunning;

    float multiply(float x1,float x2) {
      return x1*x2;
    }

@dt UseThreadLock
@dd If set, the included functions are thread-safety; they can be called safely from different threads - expressions in @ref Thread, @ref BitmapExpression, and @ref MeshExpression components.

@dlx
