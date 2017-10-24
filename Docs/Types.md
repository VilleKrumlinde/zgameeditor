# Types {#Types}

ZGameEditor scripting language supports the following types:

# int {#intType}

Signed integer number.

Size: 32 bits

Range: from -2147483648 to 2147483647

In addition to decimal values, you can write also hexadecimal values in format:

@syn{0x<hexa code>}

Examples:

    int a = 255;    // 255
    a -= 100 + 100; // 55
    a = 0x0f;       // 15
    a = 0xff;       // 255
    a = 0x1f48;     // 8008
    a = 0x1F48;     // 8008

# float {#floatType}

Floating point number.

Size: 32 bits

Range: from 3.4e +/- 38 (23 bits for significand, i.e., up to 7 valid digits)

To ensure that integral values are treated as floats, you can add suffix ".0" or "f".

Floats support also scientific notation for large or small numbers in form _significand_ x 10<SUP>exponent</SUP>. Instead of 10<SUP>exponent</SUP>, "e" or "E" is used to delimit exponent from significand.

Examples:

    float a = 5.0;  // 5.0
    a = 22f         // 22.0
    a = 1.2e4;      // 12000
    a = 5e-2;       // 0.05

# byte {#byteType}

Small integer.

Size: 8 bits

Range: from -128 to 127

# string {#stringType}

Sequence of characters. String literals are enclosed in double quotes. In expression, a string literal must start and finish at the same line.

Special characters in string literals:

* `\"` : double quote character
* `\n` : newline character
* `\\` : backslash character

Examples:

    string s = "Hi, my name is John.";
    trace("aaa" + chr(42) + "bbb");     // "aaa*bbb"

    s = "aaa
    bbb";                               // syntax error

    s = "aaa\n" +
    "bbb";                              // correct

# Vectors {#Vectors}
@anchor vec2Type @anchor vec3Type @anchor vec4Type

The following vector types are supported in scripting language:

* vec2 - two-component float vector
* vec3 - three-component float vector
* vec4 - four-component float vector

Components of vectors are floats and can be accessed from vector variables or vector component properties by __index properties__:

* X, Y, Z, W (or x, y, z, w) - for position, or
* R, G, B, A (or r, g, b, a) - for colors.

Arithmetic operations must be performed on particular index properties, it is not allowed to add or multiply vectors directly.

Examples:

    vec2 v2 = vector2(1, 2);
    vec3 v3 = vector3(10, 10, 10);
    vec4 v4 = MyRenderSetColor.Color;

    v2.X = 14.2;
    v2.Y *= v2.X;
    App.LightPosition = v3;
    v4.A = 0.5;
    
# mat4 {#mat4Type}

Matrix of 4x4 floats. It is usually used to store a OpenGL matrix for geometric transformations, see [here](https://open.gl/transformations) for details.

Entries of a matrix can be accessed as array of 16 floats. So you can use 1D (matrix[i], i = 0..15) or 2D (matrix[column,row]) indexing notation.

Matrix multiplication is supported by * operator. Example:

    mat4 m1, m2;
    // ...
    mat4 m3 = m1 * m2;

Example of changing model-view matrix in OnRender property of @ref Model :

    // inputs
    float scale = 2.0;
    vec3 tran = vector3(1, 2, 3);

    mat4 modelViewMatrix;
    
    getMatrix(0, modelViewMatrix);
    
    // translate model
    modelViewMatrix[3,0] += tran.X;
    modelViewMatrix[3,1] += tran.Y;
    modelViewMatrix[3,2] += tran.Z;
    
    // scale model
    modelViewMatrix[0,0] *= scale;
    modelViewMatrix[1,1] *= scale;
    modelViewMatrix[2,2] *= scale;
    
    setMatrix(0, modelViewMatrix);

# xptr {#xptrType}

The type `xptr` is a pointer (a memory address) similar to pointer in C/C++ language. Its primary purpose is to transfer pointers to C/C++ objects, structures and arrays to/from @ref ExternalFunctions "external functions".

The keyword `null` is used for null pointer - an "empty" pointer or pointer to "nowhere". It is used to determine that the pointer is not set.

Example:

OpenGL external library contains function `glTexImage2D` used to define 2D or 1D texture array. Its last parameter is a pointer to an array with texture image data:

    void glTexImage2D(int target, int level, int internal format, int width, int height, int border, int format, int atype, xptr pixels) {}

This function can be called with specified name of @ref Array containing the data:

    byte[] imageData;
    ...
    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, imageWidth, imageHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, imageData);

`xptr` can also be used as an identifier of the C++ objects returned from calls of external functions that are used in consequent calls of external functions. ZGameeditor scripts do not change these pointers, just remember them in variables and pass them to function calls. This usage can be seen, for instance, in ZgeBullet 3D physic library in function `xptr zbtCreateWorld ()`, which creates a physics simulation world and returns a pointer to it. This pointer is then used in other functions as parameter:

    xptr World; // global variable
    ...

    // initialization of physical world
    World = zbtCreateWorld();
    zbtSetCurrentWorld(World);
    zbtSetWorldGravity(0, -10, 0);  

    // continue simulation
    ...
    
    // destroy physical world
    zbtDestroyWorld(World);

# Component-based Types {#ComponentTypes}
@anchor modelType @anchor MaterialType @anchor SoundType @anchor ShaderType @anchor BitmapType @anchor MeshType @anchor CameraType @anchor FontType @anchor SampleType

There are several types corresponding to (instances of) some components. You can create variables of that types, assign components to them and set their properties in scripts. Except of the `model` type, variables of component-based types must be defined either as local expression variables or global variables defined in @ref ZLibrary component.

Type | Corresponding component
-----|------------------------
model | An instance of the @ref Model component
Material | @ref Material
Sound | @ref Sound
Shader | @ref Shader
Bitmap | @ref Bitmap
Mesh | @ref Mesh
Camera | @ref Camera
Font | @ref Font
Sample | @ref Sample
Component | any component

Example: Multiple instantiation of models and setting their properties:

    // instantiation of asteroids
    model m;
    for(int i =0; i < 100; i++) {
      m = createModel(AsteroidModel);
      m.Position.X = random(0, 50);
      m.Position.Y = random(0, 50);
      m.Position.Z = random(0, 50);
      m.AsteroidWeight = random(30, 10); // setting of Model's local variable
    }

Additional various examples:

    // Play a sound
    Sound snd;
    snd = @Sound();  // create and define a new sound
    @PlaySound(Sound : snd, NoteNr : 65);  // play it

    // ... or with compact syntax
    @PlaySound(
      Sound : @Sound(Osc1WaveForm : 1),
      NoteNr : 60);

    // Write to file
    byte[256] buf;
    for(int i=0; i<buf.SizeDim1; i++)
      buf[i]=i;
    File f = @File(
      FileName : "test.txt",
      Encoding : 1,
      TargetArray : buf);
    @FileAction(File : f, Action : 1);

    // ... or with compact syntax
    @FileAction(
      File : @File(
        FileName : "test.txt",
        Encoding : 1,
        TargetArray : buf),
      Action : 1);

    // Read from file
    byte[256] buf;
    File f = @File(
      FileName : "test.txt",
      Encoding : 1,
      TargetArray : buf);
    @FileAction(File : f, Action : 0);
    for(int i=0; i<buf.SizeDim1; i++)
      if(i!=buf[i])
        trace("error when reading");
    
# Arrays {#Arrays}

In addition to the @ref Array component, 1D, 2D or 3D arrays can be defined also in scripts with the following syntax:

@syn{<type> [ <size1> ] <name>;}

@syn{<type> [ <size1>, <size2> ] <name>;}

@syn{<type> [ <size1>, <size2>, <size3> ] <name>;}

Size can be unspecified an therefore is 0 by default.

Examples:

    int[10] arr1;     // 1D array of integers
    const int SIZE = 100;
    string[10 * SIZE] arr2; // computed array size
    float[5,10] arr3; // 2D array of floats
    vec3[,] arr4;    // 2D array of vec3   
    model[,,] arr5;    // 3D array of models

Size of array (defined as scripting variable or @ref Array component) can be changed dynamically by setting its properties SizeDim1, SizeDim2 or SizeDim3 respectively, for each dimension of an array. The SizeDim* properties can also be used to check actual size of array.

Accessing of array items (defined as scripting variable or @ref Array component) is done with syntax:

@syn{<name> [ <index1> ]}

@syn{<name> [ <index1>, <index2> ]}

@syn{<name> [ <index1>, <index2>, <index3> ]}

Index must be in range from 0 to SizeDim1/2/3.

Example: Copy of an array.

    int[] source;
    int[] destination;

    // in meanwhile, source array is filled by data from a file

    // copying source array to destination array
    destination.SizeDim1 = source.SizeDim1;
    for(int i = source.SizeDim1 - 1; i >= 0; i--)
      destination[i] = source[i];
      
Example: Passing an array as parameter to function and dynamic changing the size of the array.

    const int BUFFER_DELTA = 100;

    // adding an item to buffer at position
    void addItem(int[] buffer, int item, ref int position) {
      if(buffer.SizeDim1 == position) buffer.SizeDim1 += BUFFER_DELTA;
      buffer[position] = item;
      position++;
    }

    // variables
    int[] MyBuffer;
    int index = 0;
    
    // calling the function
    for(; index < 1000;)
      addItem(MyBuffer, index, index);

Example: Declaring a function-local array and returning it as a result.

    int[] getArray() {
      int[10] x;
      return x;
    }
