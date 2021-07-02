# Array {#Array}

Defines an array with values that can be used in expressions. The Name property must be defined in order to refer to the array in expressions.

@note Range checking of arrays are only performed inside the designer tool, so be sure to test your game that use arrays in preview mode before generating a exe-file.

See also: @ref Arrays "Arrays in expressions"

## Properties

@dl

@dt Type
@dd The data type of the array elements. It is one of the following:

* @ref floatType
* @ref intType
* @ref stringType
* @ref modelType
* @ref byteType
* @ref mat4Type
* @ref vec2Type
* @ref vec3Type
* @ref vec4Type
* @ref xptrType

@dt Dimensions
@dd This property can be set to one of the following values:

* One - A one dimensional array. Example of usage in expression:

      MyArray[4] = 1;

* Two - A two dimensional array. Example of usage in expression:

      MyArray[4, 1] = 1;

* Three - A three dimensional array. Example of usage in expression:

      MyArray[4, 1, 2] = 1;

@dt SizeDim1
@dd Number of elements in the first dimension. For instance, a SizeDime1 value of 2 with dimensions set to One defines an array with two elements that can be accessed with index 0 and 1. This value can also be set at runtime.

@dt SizeDim2
@dd Number of elements in the second dimension. This value can also be set at runtime.

@dt SizeDim3
@dd Number of elements in the third dimension. This value can also be set at runtime.

@dt Persistent
@dd With a persistent array, values are stored in your project file. Only numeric persistent arrays are supported. Array values can be set/modified in editor, but can also be changed in runtime.

When editing a persistent array, the values can be copied to a text editor, edited, and pasted back into the value editor.

@dlx
