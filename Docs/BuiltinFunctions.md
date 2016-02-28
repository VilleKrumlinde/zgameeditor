# Built-in Functions Reference {#BuiltinFunctions}

# Math {#Math}

@dl

@dtn float @bf{abs} (float x)
@dd Returns the absolute value of _x_.

@dtn float @bf{sqrt} (float x)
@dd Returns the square root of _x_.

@dtn float @bf{pow} (float base, float exponent)
@dd Returns base raised to the power of exponent.

@dtn float @bf{exp} (float x)
@dd Returns the base-e exponential function of _x_, which is, e raised to the power _x_.

@dtn float @bf{log2} (float x)
@dd Returns the binary (base-2) logarithm of _x_.

@dtn float @bf{sin} (float x)
@dd Returns the sine of the angle _x_ in radians.

@dtn float @bf{cos} (float x)
@dd Returns the cosine of the angle _x_ in radians.

@dtn float @bf{tan} (float x)
@dd Returns the tangent of the angle _x_ in radians.

@dtn float @bf{asin} (float x)
@dd Returns the arcsine of _x_, expressed in radians.

@dtn float @bf{acos} (float x)
@dd Returns the arccosine of _x_, expressed in radians.

@dtn float @bf{atan2} (float x, float y)
@dd Returns the arctangent of _x/y_, expressed in radians. It uses the signs of both parameters to determine the quadrant of the return value.

@dtn int @bf{round} (float x)
@dd Returns the integer value that is nearest to _x_, with halfway cases rounded towards zero.

@dtn float @bf{floor} (float x)
@dd Rounds x downward, returning the largest integer value that is not greater than _x_.

@dtn float @bf{ceil} (float x)
@dd Rounds x upward, returning the smallest integer value that is not less than _x_.

@dtn float @bf{frac} (float x)
@dd Returns the decimal (fractional) part of _x_.

@dtn float @bf{clamp} (float x, float min, float max)
@dd Computes the value _x_ clamped to the range from _min_ (including) to _max_ (including).

@dtn float @bf{rnd} ()
@dd Returns random number in the range 0 (including) to 1 (excluding).

@dtn float @bf{random} (float base, float variance)
@dd Returns random number in the range _base-variance_ to _base+variance_.

@dtn float @bf{setRandomSeed} (float x)
@dd Set the random seed (a number used to initialize a pseudo-random number generator) to _x_. The function returns the old seed. Usually, it is set at the application or state initialization like this:

    setRandomSeed(getSystemTime());

@dtn float @bf{noise2} (float x, float y) and float @bf{noise3} (float x, float y, float z)
@dd 2- and 3-dimensional Perlin Noise. Returns a value in an approximate range of -0.3 to 0.3 so you need to scale it to get a -1 to 1 range. More info: [Hugo Elias article about noise](http://freespace.virgin.net/hugo.elias/models/m%5Fperlin.htm).

@dlx

# Vectors and Matrices {#VectorsAndMatrices}

@dl

@dtn vec2 @bf{vector2} (float x, float y)
@dd Creates vec2 instance from two floats.

@dtn vec3 @bf{vector3} (float x, float y, float z)
@dd Creates vec3 instance from three floats.

@dtn vec4 @bf{vector4} (float x, float y, float z, float w)
@dd Creates vec4 instance from four floats.

@dtn void @bf{getMatrix} (int type, mat4 matrix)
@dd Updates the specified _matrix_ by the current OpenGL matrix used for rendering. The _type_ parameter determines type of obtained matrix: 0 - Model view, 1 - Projection, 2 - Texture. 

@dtn void @bf{setMatrix} (int type, mat4 matrix)
@dd Sets the currently used OpenGL matrix to the specified _matrix_.  The _type_ parameter determines type of obtained matrix: 0 - Model view, 1 - Projection, 2 - Texture.

@dtn vec3 @bf{transformPoint} (mat4 matrix, vec3 point)
@dd Returns position of _point_ transformed by _matrix_. More information about transformation matrices see [here](https://open.gl/transformations).

@dlx

# Strings {#Strings}

@dl

@dtn string @bf{intToStr} (int i)
@dd Converts integer _i_ to string. Example:

    intToStr(42); // "42"

@dtn int @bf{strToInt} (string s)
@dd Converts string _s_ to integer. String must be formated as valid integer number, else the result can be improper number. Example: 

    strToInt("42"); // 42

@dtn string @bf{chr} (int code)
@dd Returns character from ASCII _code_. Example:

    chr(65); // "A"

@dtn int @bf{ord} (string s)
@dd Returns ASCII code of the 1st character of string _s_. Example:

    ord("A"); // 65

@dtn int @bf{length} (string s)
@dd Returns length of string _s_. Example:

    length("1234"); // 4

@dtn int @bf{indexOf} (string substr, string str, int startpos)
@dd Returns position of substring in string from a start position. _Startpos_ must be less than length of string. Returns -1 if substring was not found. Example:

    indexOf("lo","hello",0); // 3

@dtn string @bf{subStr} (string source, int startpos, int length)
@dd Returns substring of _source_ string. _Startpos_ and _length_ must be within _source_ string bounds. Example:

    subStr("hello",0,2); // "he"

@dlx

# Threads {#Threads}

@dl

@dtn @anchor startThread void @bf{startThread} (Thread t, int parameter)
@dd Launches a new @ref Thread _t_ using _parameter_ as parameter to the thread expression. The parameter is meant to be used for giving each thread a context when several instances of the same Thread component are launched.

@dtn @anchor sleep void @bf{sleep} (int milliseconds)
@dd Pauses the current @ref Thread and allows other threads to execute. If you use threads and notice your CPU usage is too high, insert a sleep in your computation loop to lower CPU usage. 

@dlx

# Device Input {#DeviceInput}

@dl

@dtn @anchor joyGetAxis float @bf{joyGetAxis} (int joyId, int axisNr)
@dd Returns position of joystick axis.

@dtn @anchor joyGetButton int @bf{joyGetButton} (int joyId, int buttonNr)
@dd Returns state of joystick button. 1 - pressed, 0 - released.

@dtn float @bf{joyGetPOV} (int joyId)
@dd Returns position of joystick POV control.

@dtn @anchor touchGetCount int @bf{touchGetCount} ()
@dd Android only. Returns number of touches on touchscreen.

@dtn @anchor touchGetID int @bf{touchGetID} (int i)
@dd Android only. Returns ID of _i_-th touch.

@dtn @anchor touchGetX float @bf{touchGetX} (int id)
@dd Android only. Returns position X of touch identified by _id_.

_Remark: ZApplication.MousePosition.X is on Android identical with touchGetX(0)._

@dtn @anchor touchGetY float @bf{touchGetY} (int id)
@dd Android only. Returns position Y of touch identified by _id_.

_Remark: ZApplication.MousePosition.Y is on Android identical with touchGetY(0)._

@dtn void @bf{centerMouse} ()
@dd Centers the mouse cursor to middle of viewport.

@dlx

# Misc {#Misc}

@dl

@dtn @anchor createModel model @bf{createModel} (model m)
@dd Creates and returns an instance of model type _m_.

@dtn void @bf{getModels} (Array items, int category)
@dd Updates the _items_ array (an array of type model) to model instances of the specified _category_.

@dtn int @bf{getSystemTime} ()
@dd Returns returns seconds elapsed since midnight.

@dtn void @bf{getBinaryProp} (ref void property, Array a)
@dd Updates the content of byte array _a_ with the content of a binary _property_. Example:

    // buffers content of imported bitmap
    byte[] buffer;
    getBinaryProp(BitmapFromFile1.BitmapFile, buffer);

@dtn void @bf{setBinaryProp} (ref void property, Array a)
@dd Set the content of a binary _property_ with the content of a byte array _a_. Example:

    byte[1024] buffer; // data for 16x16 bitmap

    for(int i = 0; i < 1024; i++)
    buffer[i] = i / 4;
    
    setBinaryProp(BitmapFromFile1.BitmapFile, buffer).

@dtn void @bf{trace} (string message)
@dd Shows a _message_ in the @ref LogWindow. Useful for debugging.

    trace(chr(12));                       // clear the log
    trace("Time: " + intToStr(App.Time)); // log application running time

@dtn void @bf{quit} ()
@dd Quit the application.

@dlx
