# Variable {#Variable}

Defines a global variable that can be used in expressions.

## Properties

@dl

@dt Name
@dd The name of the variable. A variable can be accessed from an expression in the following syntax:

    MyVar1 = 3.5 * MyVar2;

@dt Type
@dd The data type of the variable. It can be one of the following:

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

@dt Value
@dd Read only. The value of the variable when type is float. This value is not saved in project file and is 0.0 by default.

@dt IntValue
@dd Read only. The value of the variable when type is integer. This value is not saved in project file and is 0 by default.

@dt StringValue
@dd Read only. The value of the variable when type is string. This value is not saved in project file and is "" by default.

@dt ModelValue
@dd Read only. The value of the variable when type is model. This value is not saved in project file and is null (empty) by default.

@dt ByteValue
@dd Read only. The value of the variable when type is byte. This value is not saved in project file and is 0 by default.

@dlx
