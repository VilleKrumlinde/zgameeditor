# ShaderVariable {#ShaderVariable}

Represents a uniform variable passed to a GLSL shader.

This component can only be used in the @ref ShaderUniformVariables "Shader.UniformVariables" property.

See also: @ref Shader

##Properties

@dl

@dt VariableName
@dd The name of the uniform variable in GLSL. Case-sensitive.

@dt Value
@dd Set this property to set the current value of the uniform variable. This property is only valid if ValuePropRef is not set.

@dt ValuePropRef
@dd Use this @ref ScriptingLanguage "expression" to compute a value of uniform variable from other properties. Value of this property is specified in @ref CodeEditor "Code editor". For example, type "App.Time * 8" to connect the current application time to a GLSL variable used for animation.

@dt ValueArrayRef
@dd Set this reference to an @ref Array to pass it to GLSL. The GLSL type is determined by ArrayKind property. Use only float, vec2, vec3, vec4 and mat4 arrays. 

@dt ArrayKind
@dd Determines how an array passed through the ValueArrayRef property is represented in GLSL. If it is set to:

* Texture2D - the type of GLSL uniform variable is "sampler2D". Float values from array are placed to .r values of sampler pixels. Sampley y-axis is always 0. To access sampler values use, for instance, the following GLSL code:

      texelFetch(array, ivec2(index,0), 0).r

* Mat4 - the type of GLSL uniform variable is "mat4"

@dt VariableRef
@dd Use this to link a @ref Variable component to a shader variable. It supports float, vec2, vec3, vec4 and mat4 types.

@dlx
