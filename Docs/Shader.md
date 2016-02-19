# Shader {#Shader}

This component allows you to use OpenGL shaders.

Example usage: @ref ShaderDemo sample project.

See also: @ref Material

External links: [OpenGL Shading Language on Wikipedia](https://en.wikipedia.org/wiki/OpenGL_Shading_Language) 

## Properties

@dl

@dt VertexShaderSource
@dd GLSL source code for the vertex shader.

@dt FragmentShaderSource
@dd GLSL source code for the fragment (pixel) shader.

@dt UpdateVarsOnEachUse
@dd Setting this property on will update uniform variables before each usage of shader. If it is unset, shader variables are updated only on the beginning of rendering cycle.

@dt Handle
@dd Read only. The OpenGL handle of the shader. Use this when you want to make direct OpenGL calls. This value is available only in runtime. Example of usage Handle to obtain handle to attribute variable "position" in MyShader:

    positionAttrVar = glGetAttribLocation(MyShader.Handle, "position");

@dlx

## List Properties

@dl

@dt @anchor ShaderUniformVariables UniformVariables
@dd A list of uniform variables that may be referenced in the GLSL code. See @subpage ShaderVariable.

@dlx
